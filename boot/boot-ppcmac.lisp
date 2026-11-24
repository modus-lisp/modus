;;;; boot-ppcmac.lisp — bare PowerPC Macintosh entry for the CL image.
;;;;
;;;; Open Firmware (a New World Mac's, or QEMU mac99's OpenBIOS) loads the image
;;;; as an ELF from an HFS volume (scripts/ppcmac-disk.lisp) and enters it with
;;;; the MMU on under OF's own translations, r5 = the client interface, r1 = an
;;;; OF stack.  The CL runtime on 32 bits expects the HOSTED ppc32 picture
;;;; (boot-linux-ppc.lisp): the image low, the runtime's fixed block and the heap
;;;; at #x10000000.., the collector's kind map at #x1FC00000, NIL's page at
;;;; #xDEAD0000, and nothing the Lisp side addresses at or above 2^29.  This
;;;; stub builds that picture with four BATs, which every 32-bit PowerPC from
;;;; the 601 on has, and then does what the hosted stub does after its mmaps:
;;;;
;;;;   VA = PA #x00000000..#x07FFFFFF  IBAT0 + DBAT0   image (#x10000..), stack,
;;;;                                                   handler-frame extension
;;;;   VA = PA #x10000000..#x1FFFFFFF  DBAT1           fixed block, heap, kind map
;;;;   VA #xDEAC0000..#xDEADFFFF       DBAT2 -> PA #x07800000  NIL's page
;;;;   VA #x08000000..#x09FFFFFF       DBAT3 -> PA #x80000000.., uncached: mac-io
;;;;                                   (VA #x08000000: the ESCC, the VIA) and, on
;;;;                                   QEMU mac99, the framebuffer (VA #x09000000)
;;;;
;;;; THE DEVICES ARE LOW because Lisp reaches them with MEM-REF, whose address
;;;; travels as a 30-bit fixnum: nothing at or above 2^29 can be named.  (The
;;;; translator's own console traps could use any address; drivers in Lisp --
;;;; the CUDA/ADB keyboard and mouse, the screen -- cannot.)
;;;;
;;;; Memory (all below 512 MB, so -m 512 or more; QEMU mac99 runs with -m 1024):
;;;;   #x00010000..  the image (35 MB) -- Open Firmware loads the file at its
;;;;                 load-base (#x04000000 on OpenBIOS) and copies the segment,
;;;;                 so the file's buffer is dead once the stub runs
;;;;   #x04800000..#x06000000  the boot payload (scripts/ppcmac-disk.lisp), up to 24 MB
;;;;   #x06000000..#x07000000  the stack, down from +PPCMAC-STACK-TOP+
;;;;   #x07000000..#x07200000  the handler-frame extension (fixed, 64K frames)
;;;;   #x07800000..#x07820000  NIL's 128 KB BAT block
;;;;   #x07A00000..#x07A10000  the first stage (modus-boot.elf), dead once here
;;;;   #x07B00000..#x07C00000  Open Firmware's device tree, copied (+PPCMAC-DT+)
;;;;   #x07C00000..#x07D00000  USB's DMA: OHCI's HCCA, descriptors, buffers
;;;;   #x07D00000..#x07D10000  the mutex arena (%BARE-SYNC-ARENA)
;;;;   #x07E00000, #x07F00000  the CLI's scratch pages
;;;;   #x10000000..            the heap, +PPCMAC-HEAP-SIZE+, then its guard
;;;;   #x1FC00000..#x20000000  the kind map (heap/16)
;;;;
;;;; Interrupts stay off (MSR[EE] = 0) and Open Firmware's vectors stay in place:
;;;; a fault lands in OF.  Translated with PPC-SET-MAC-MODE (Mac traps, hosted
;;;; layout) -- see translate-ppc.lisp *PPC-MAC-MODE*.

(in-package :modus.mvm)

;; the registers the firmware query and the first stage use past translate-ppc's
(macrolet ((regs (&rest ns)
             `(progn ,@(loop for n in ns
                             for sym = (intern (format nil "+PPC-R~D+" n) :modus.mvm)
                             collect `(unless (boundp ',sym) (defconstant ,sym ,n))))))
  (regs 22 23 24 25 26 27 28 29 30))

(defconstant +ppcmac-load-addr+   #x10000    "Image byte 0 (the ELF header).")
(defconstant +ppcmac-stack-top+   #x07000000 "Grows down; the extension is above.")
(defconstant +ppcmac-heap-size+   #x4000000
  "64 MB: two 32 MB semispaces, and a kind map (size/16) that ends exactly at
   #x20000000, the top of DBAT1.")
(defconstant +ppcmac-nil-pa+      #x07800000 "PA behind VA #xDEAC0000 (128 KB).")
(defconstant +ppcmac-fb-va+       #x09000000
  "VA of a framebuffer that is not beside mac-io: DBAT4's 16 MB (a G4e).")
(defconstant +ppcmac-device-va+   #x08000000
  "VA of PA #x80000000 (mac-io, then QEMU's framebuffer at +#x1000000): 32 MB.")
(defconstant +ppcmac-hstack-ext-frames+ #x10000
  "Frames in the fixed extension: 64K x 32 bytes = 2 MB at #x07000000.")

(defun ppcmac-mtspr (buf spr rs)
  (ppc-emit-word buf (logior (ash 31 26) (ash rs 21) (ash (logand spr 31) 16)
                             (ash (ash spr -5) 11) (ash 467 1))))

(defconstant +ppcmac-boot-info+ #x07F80000
  "What Open Firmware said, written by the stub: \"PPCM\", then the framebuffer's
   VA (0: none), width, height, bytes per row, depth; mac-io's VA; the VIA's
   kind (1 CUDA, 2 PMU, 0 unknown); RAM bytes; the framebuffer's and mac-io's PAs;
   the PVR; 1 when Open Firmware's console was a serial port; the device window
   (VA +PPCMAC-DEVICE-VA+): its PA and size; the timebase's frequency.  A word
   each, from +4.")

(defun ppcmac-emit-mfspr (buf rt spr)
  (ppc-emit-word buf (logior (ash 31 26) (ash rt 21) (ash (logand spr 31) 16)
                             (ash (ash spr -5) 11) (ash 339 1))))

(defun ppcmac-emit-put-hex (buf r)
  "' ' and register R in hex on the ESCC.  r9..r12, r0 and CTR."
  (let ((top (mvm-make-label)) (dig (mvm-make-label)))
    (ppc-emit-li buf +ppc-r11+ 32) (ppc-emit-mac-putc buf +ppc-r11+)
    (ppc-emit-mr buf +ppc-r9+ r)
    (ppc-emit-li buf +ppc-r11+ 8) (ppc-emit-mtctr buf +ppc-r11+)
    (ppc-emit-label buf top)
    (ppc-emit-rlwinm buf +ppc-r9+ +ppc-r9+ 4 0 31)
    (ppc-emit-andi-dot buf +ppc-r11+ +ppc-r9+ 15)
    (ppc-emit-cmpwi buf +ppc-r11+ 10)
    (ppc-emit-blt buf dig)
    (ppc-emit-addi buf +ppc-r11+ +ppc-r11+ 7)
    (ppc-emit-label buf dig)
    (ppc-emit-addi buf +ppc-r11+ +ppc-r11+ 48)
    (ppc-emit-mac-putc buf +ppc-r11+)
    (ppc-emit-bc buf 16 0 top)))                                 ; bdnz

(defparameter *ppcmac-of-strings*
  '("finddevice" "getprop" "child" "peer" "open" "call-method" "screen" "address"
    "assigned-addresses" "width" "height" "linebytes" "depth" "mac-io" "name"
    "/memory" "reg" "color!" "/chosen" "stdout" "instance-to-package" "device_type"
    "nextprop" "getproplen" "claim" "/cpus" "timebase-frequency"))

(defconstant +ppcmac-dt+ #x07B00000
  "Open Firmware's device tree, copied by the stub for Lisp (PPCMAC-EMIT-DT-DUMP):
   \"DTRE\", then records of words -- 1 a node begins (its properties, then its
   children, follow), 2 a property: its NUL-terminated name in 32 bytes, its
   length, the length kept (at most 1 KB), the data padded to a word -- 3 a node
   ends, 9 the end.")
(defconstant +ppcmac-dt-limit+ #x07BE0000 "No property record starts past here.")
(defconstant +ppcmac-dt-stack+ #x07BF0000 "The walk's stack of parent phandles.")

(defun ppcmac-emit-dt-dump (buf cif li)
  "Copy Open Firmware's device tree to +PPCMAC-DT+ (its format there).  A
   depth-first walk: child, then peer, a stack of parents at +PPCMAC-DT-STACK+;
   each node's properties by nextprop / getproplen / getprop.  CIF and LI are
   PPCMAC-EMIT-OF-QUERY's helpers; r14 = the cursor, r15 = the node, r16 = the
   stack, r17 = the previous property's name, r18, r19, r28 scratch."
  (flet ((cif (&rest a) (apply cif a)) (li (r v) (funcall li r v)))
    (let ((node (mvm-make-label)) (prop (mvm-make-label)) (props-done (mvm-make-label))
          (leaf (mvm-make-label)) (up (mvm-make-label)) (pop (mvm-make-label))
          (finish (mvm-make-label)) (none (mvm-make-label)))
      (cif "claim" (list (list :imm +ppcmac-dt+) (list :imm #x100000) (list :imm 0)) 1)
      (li +ppc-r14+ +ppcmac-dt+)
      (li +ppc-r3+ #x44545245) (ppc-emit-stw buf +ppc-r3+ +ppc-r14+ 0)   ; "DTRE"
      (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 4)
      (li +ppc-r16+ +ppcmac-dt-stack+)
      (li +ppc-r3+ 0) (ppc-emit-stw buf +ppc-r3+ +ppc-r16+ -4)        ; "" for the first nextprop
      (cif "peer" (list (list :imm 0)) 1 +ppc-r15+)                    ; the root
      (ppc-emit-cmpwi buf +ppc-r15+ 0) (ppc-emit-beq buf finish)
      (ppc-emit-cmpwi buf +ppc-r15+ -1) (ppc-emit-beq buf finish)
      ;; a node: 1, its properties
      (ppc-emit-label buf node)
      (li +ppc-r3+ 1) (ppc-emit-stw buf +ppc-r3+ +ppc-r14+ 0)
      (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 4)
      (li +ppc-r19+ +ppcmac-dt-stack+) (ppc-emit-addi buf +ppc-r17+ +ppc-r19+ -4)
      (ppc-emit-label buf prop)
      (li +ppc-r3+ +ppcmac-dt-limit+)
      (ppc-emit-cmpl-word buf +ppc-r14+ +ppc-r3+) (ppc-emit-bge buf props-done)
      (li +ppc-r3+ 2) (ppc-emit-stw buf +ppc-r3+ +ppc-r14+ 0)
      (ppc-emit-addi buf +ppc-r28+ +ppc-r14+ 4)
      (cif "nextprop" (list +ppc-r15+ +ppc-r17+ +ppc-r28+) 1 +ppc-r18+)
      (ppc-emit-cmpwi buf +ppc-r18+ 1) (ppc-emit-bne buf props-done)
      (ppc-emit-addi buf +ppc-r17+ +ppc-r14+ 4)                       ; the next one's previous
      (cif "getproplen" (list +ppc-r15+ +ppc-r17+) 1 +ppc-r18+)
      (ppc-emit-stw buf +ppc-r18+ +ppc-r14+ 36)
      (ppc-emit-mr buf +ppc-r19+ +ppc-r18+)
      (let ((pos (mvm-make-label)) (small (mvm-make-label)) (get (mvm-make-label)) (skip (mvm-make-label)))
        (ppc-emit-cmpwi buf +ppc-r19+ 0) (ppc-emit-bge buf pos)
        (li +ppc-r19+ 0)
        (ppc-emit-label buf pos)
        (ppc-emit-cmpwi buf +ppc-r19+ 1024) (ppc-emit-ble buf small)
        (li +ppc-r19+ 1024)
        (ppc-emit-label buf small)
        (ppc-emit-stw buf +ppc-r19+ +ppc-r14+ 40)
        (ppc-emit-cmpwi buf +ppc-r19+ 0) (ppc-emit-beq buf skip)
        (ppc-emit-label buf get)
        (ppc-emit-addi buf +ppc-r28+ +ppc-r14+ 44)
        (cif "getprop" (list +ppc-r15+ +ppc-r17+ +ppc-r28+ +ppc-r19+) 1)
        (ppc-emit-label buf skip))
      (ppc-emit-addi buf +ppc-r19+ +ppc-r19+ 3)
      (ppc-emit-rlwinm buf +ppc-r19+ +ppc-r19+ 0 0 29)                ; to a word
      (ppc-emit-add buf +ppc-r14+ +ppc-r14+ +ppc-r19+)
      (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 44)
      (ppc-emit-b buf prop)
      ;; its children: down, the node on the stack
      (ppc-emit-label buf props-done)
      (cif "child" (list +ppc-r15+) 1 +ppc-r18+)
      (ppc-emit-cmpwi buf +ppc-r18+ 0) (ppc-emit-beq buf leaf)
      (ppc-emit-cmpwi buf +ppc-r18+ -1) (ppc-emit-beq buf leaf)
      (ppc-emit-stw buf +ppc-r15+ +ppc-r16+ 0)
      (ppc-emit-addi buf +ppc-r16+ +ppc-r16+ 4)
      (ppc-emit-mr buf +ppc-r15+ +ppc-r18+)
      (ppc-emit-b buf node)
      ;; no children: 3, then the peer, or up
      (ppc-emit-label buf leaf)
      (li +ppc-r3+ 3) (ppc-emit-stw buf +ppc-r3+ +ppc-r14+ 0)
      (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 4)
      (ppc-emit-label buf up)
      (cif "peer" (list +ppc-r15+) 1 +ppc-r18+)
      (ppc-emit-cmpwi buf +ppc-r18+ 0) (ppc-emit-beq buf pop)
      (ppc-emit-cmpwi buf +ppc-r18+ -1) (ppc-emit-beq buf pop)
      (ppc-emit-mr buf +ppc-r15+ +ppc-r18+)
      (ppc-emit-b buf node)
      (ppc-emit-label buf pop)
      (li +ppc-r3+ +ppcmac-dt-stack+)
      (ppc-emit-cmpl-word buf +ppc-r16+ +ppc-r3+) (ppc-emit-ble buf finish)
      (ppc-emit-addi buf +ppc-r16+ +ppc-r16+ -4)
      (ppc-emit-lwz buf +ppc-r15+ +ppc-r16+ 0)
      (li +ppc-r3+ 3) (ppc-emit-stw buf +ppc-r3+ +ppc-r14+ 0)
      (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 4)
      (ppc-emit-b buf up)
      (ppc-emit-label buf finish)
      (li +ppc-r3+ 9) (ppc-emit-stw buf +ppc-r3+ +ppc-r14+ 0)
      (ppc-emit-label buf none))))

(defun ppcmac-emit-of-query (buf)
  "Ask Open Firmware's client interface (r5 at entry) what this Mac is, before
   the stub takes the MMU: the framebuffer of the \"screen\" alias (its \"address\",
   else the first 32-bit memory range of its \"assigned-addresses\" -- QEMU's
   QEMU,VGA has no \"address\"), its width, height, linebytes and depth; mac-io's
   first assigned address; whether a child of mac-io is via-pmu or via-cuda;
   /memory's first size.  Into r20..r27 (see EMIT-PPCMAC-ENTRY); a value the
   firmware lacks is 0.  On an 8-bit screen the CLUT gets a 6x6x6 colour cube,
   index = 36r + 6g + b, through the display's COLOR! (Linux's prom_init sets its
   palette the same way, arguments index b g r).  Uses OF's stack, a 2 KB frame:
   the arguments at +64, a buffer at +512; r14..r19, r28..r30."
  (let* ((strs *ppcmac-of-strings*)
         (offs (let ((o 0) (h nil))
                 (dolist (s strs h)
                   (push (cons s o) h)
                   (incf o (* 4 (ceiling (1+ (length s)) 4))))))
         (past (mvm-make-label)))
    (labels ((off (s) (cdr (assoc s offs :test #'string=)))
             (li (r v) (ppc-emit-li buf r v))
             (arg (i spec)
               ;; SPEC: an integer register, (:imm v), (:str s) or (:buf k)
               (cond ((integerp spec) (ppc-emit-mr buf +ppc-r3+ spec))
                     ((eq (car spec) :imm) (li +ppc-r3+ (second spec)))
                     ((eq (car spec) :str) (ppc-emit-addi buf +ppc-r3+ +ppc-r29+ (off (second spec))))
                     ((eq (car spec) :buf) (ppc-emit-addi buf +ppc-r3+ +ppc-r1+ (+ 512 (second spec)))))
               (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ (+ 76 (* 4 i))))
             (cif (service args nret &optional into)
               ;; INTO: the register for the first result
               (ppc-emit-addi buf +ppc-r3+ +ppc-r29+ (off service))
               (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 64)
               (li +ppc-r3+ (length args)) (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 68)
               (li +ppc-r3+ nret) (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 72)
               (loop for a in args for i from 0 do (arg i a))
               (ppc-emit-addi buf +ppc-r3+ +ppc-r1+ 64)
               (ppc-emit-mtctr buf +ppc-r30+)
               (ppc-emit-bctrl buf)
               (when into
                 (ppc-emit-lwz buf into +ppc-r1+ (+ 76 (* 4 (length args))))))
             (prop (phandle name into &optional (len 4))
               ;; INTO = the property's first word, or 0 when it has no such
               (let ((no (mvm-make-label)) (done (mvm-make-label)))
                 (li +ppc-r3+ 0) (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 512)
                 (cif "getprop" (list phandle (list :str name) (list :buf 0) (list :imm len)) 1 +ppc-r15+)
                 (ppc-emit-cmpwi buf +ppc-r15+ 4)
                 (ppc-emit-blt buf no)
                 (ppc-emit-lwz buf into +ppc-r1+ 512)
                 (ppc-emit-b buf done)
                 (ppc-emit-label buf no)
                 (li into 0)
                 (ppc-emit-label buf done))))
      (ppc-emit-mr buf +ppc-r30+ +ppc-r5+)
      (ppc-emit-stwu buf +ppc-r1+ +ppc-r1+ (logand -2048 #xFFFF))
      ;; the strings, in the code: r29 = their address
      (ppc-emit-bl buf past)
      (dolist (s strs)
        (let ((bytes (append (map 'list #'char-code s)
                             (make-list (- (* 4 (ceiling (1+ (length s)) 4)) (length s)) :initial-element 0))))
          (loop for (a b c d) on bytes by #'cddddr
                do (ppc-emit-word buf (logior (ash a 24) (ash b 16) (ash c 8) d)))))
      (ppc-emit-label buf past)
      (ppc-emit-mflr buf +ppc-r29+)
      ;; ---- the whole tree, for Lisp's drivers (USB's controllers, ...)
      (ppcmac-emit-dt-dump buf #'cif #'li)
      (dolist (r (list +ppc-r20+ +ppc-r21+ +ppc-r22+ +ppc-r23+ +ppc-r24+ +ppc-r25+ +ppc-r26+ +ppc-r27+))
        (li r 0))
      ;; ---- the screen
      (let ((none (mvm-make-label)) (have (mvm-make-label)) (scan (mvm-make-label))
            (found (mvm-make-label)))
        (cif "finddevice" (list (list :str "screen")) 1 +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ -1)
        (ppc-emit-beq buf none)
        (prop +ppc-r28+ "address" +ppc-r20+)
        (ppc-emit-cmpwi buf +ppc-r20+ 0)
        (ppc-emit-bne buf have)
        ;; assigned-addresses: 5-word entries; the first 32-bit memory space
        (cif "getprop" (list +ppc-r28+ (list :str "assigned-addresses") (list :buf 0) (list :imm 200)) 1 +ppc-r15+)
        (ppc-emit-addi buf +ppc-r14+ +ppc-r1+ 512)
        (ppc-emit-label buf scan)
        (ppc-emit-cmpwi buf +ppc-r15+ 20)
        (ppc-emit-blt buf have)
        (ppc-emit-lwz buf +ppc-r16+ +ppc-r14+ 0)
        (ppc-emit-rlwinm buf +ppc-r16+ +ppc-r16+ 8 30 31)        ; space code
        (ppc-emit-cmpwi buf +ppc-r16+ 2)
        (ppc-emit-beq buf found)
        (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 20)
        (ppc-emit-addi buf +ppc-r15+ +ppc-r15+ -20)
        (ppc-emit-b buf scan)
        (ppc-emit-label buf found)
        (ppc-emit-lwz buf +ppc-r20+ +ppc-r14+ 8)
        (ppc-emit-label buf have)
        (prop +ppc-r28+ "width" +ppc-r21+)
        (prop +ppc-r28+ "height" +ppc-r22+)
        (prop +ppc-r28+ "linebytes" +ppc-r23+)
        (prop +ppc-r28+ "depth" +ppc-r24+)
        ;; an 8-bit screen: the colour cube in its CLUT
        (let ((not8 (mvm-make-label)) (loop-top (mvm-make-label)))
          (ppc-emit-cmpwi buf +ppc-r24+ 8)
          (ppc-emit-bne buf not8)
          (cif "open" (list (list :str "screen")) 1 +ppc-r19+)
          (ppc-emit-cmpwi buf +ppc-r19+ 0)
          (ppc-emit-beq buf not8)
          (li +ppc-r18+ 0)                                       ; index
          (ppc-emit-label buf loop-top)
          ;; r14 = r = i/36, r15 = g = i/6 - 6r, r16 = b = i - 6(i/6), each x51
          (li +ppc-r17+ 36) (ppc-emit-divw buf +ppc-r14+ +ppc-r18+ +ppc-r17+)
          (li +ppc-r17+ 6)  (ppc-emit-divw buf +ppc-r16+ +ppc-r18+ +ppc-r17+)   ; i/6
          (ppc-emit-mullw buf +ppc-r15+ +ppc-r14+ +ppc-r17+)
          (ppc-emit-subf buf +ppc-r15+ +ppc-r15+ +ppc-r16+)                    ; i/6 - 6r
          (ppc-emit-mullw buf +ppc-r16+ +ppc-r16+ +ppc-r17+)
          (ppc-emit-subf buf +ppc-r16+ +ppc-r16+ +ppc-r18+)                    ; i - 6(i/6)
          (li +ppc-r17+ 51)
          (ppc-emit-mullw buf +ppc-r14+ +ppc-r14+ +ppc-r17+)
          (ppc-emit-mullw buf +ppc-r15+ +ppc-r15+ +ppc-r17+)
          (ppc-emit-mullw buf +ppc-r16+ +ppc-r16+ +ppc-r17+)
          (cif "call-method" (list (list :str "color!") +ppc-r19+ +ppc-r18+ +ppc-r16+ +ppc-r15+ +ppc-r14+) 1)
          (ppc-emit-addi buf +ppc-r18+ +ppc-r18+ 1)
          (ppc-emit-cmpwi buf +ppc-r18+ 216)
          (ppc-emit-blt buf loop-top)
          (ppc-emit-label buf not8))
        (ppc-emit-label buf none))
      ;; ---- mac-io, and its VIA
      (let ((none (mvm-make-label)) (kid (mvm-make-label)) (next (mvm-make-label))
            (cuda (mvm-make-label)))
        (cif "finddevice" (list (list :str "mac-io")) 1 +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ -1)
        (ppc-emit-beq buf none)
        (cif "getprop" (list +ppc-r28+ (list :str "assigned-addresses") (list :buf 0) (list :imm 200)) 1 +ppc-r15+)
        (ppc-emit-cmpwi buf +ppc-r15+ 12)
        (ppc-emit-blt buf kid)
        (ppc-emit-lwz buf +ppc-r25+ +ppc-r1+ (+ 512 8))
        (ppc-emit-label buf kid)
        (cif "child" (list +ppc-r28+) 1 +ppc-r28+)
        (let ((top (mvm-make-label)))
          (ppc-emit-label buf top)
          (ppc-emit-cmpwi buf +ppc-r28+ 0)
          (ppc-emit-beq buf none)
          (ppc-emit-cmpwi buf +ppc-r28+ -1)
          (ppc-emit-beq buf none)
          (li +ppc-r3+ 0) (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 512) (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 516)
          (cif "getprop" (list +ppc-r28+ (list :str "name") (list :buf 0) (list :imm 16)) 1)
          (ppc-emit-lwz buf +ppc-r14+ +ppc-r1+ 512)
          (li +ppc-r15+ #x7669612D)                              ; "via-"
          (ppc-emit-cmpw buf +ppc-r14+ +ppc-r15+)
          (ppc-emit-bne buf next)
          (ppc-emit-lbz buf +ppc-r14+ +ppc-r1+ 516)
          (ppc-emit-cmpwi buf +ppc-r14+ (char-code #\c))
          (ppc-emit-beq buf cuda)
          (ppc-emit-cmpwi buf +ppc-r14+ (char-code #\p))
          (ppc-emit-bne buf next)
          (li +ppc-r26+ 2)
          (ppc-emit-b buf none)
          (ppc-emit-label buf cuda)
          (li +ppc-r26+ 1)
          (ppc-emit-b buf none)
          (ppc-emit-label buf next)
          (cif "peer" (list +ppc-r28+) 1 +ppc-r28+)
          (ppc-emit-b buf top))
        (ppc-emit-label buf none))
      ;; ---- is Open Firmware's console a serial port?  r19 = 1 if so: then the
      ;; image's console is too, and its REPL useful; else (a PowerBook's screen)
      ;; the desk keeps the machine
      (let ((no (mvm-make-label)))
        (li +ppc-r19+ 0)
        (cif "finddevice" (list (list :str "/chosen")) 1 +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ -1) (ppc-emit-beq buf no)
        (prop +ppc-r28+ "stdout" +ppc-r14+)
        (ppc-emit-cmpwi buf +ppc-r14+ 0) (ppc-emit-beq buf no)
        (cif "instance-to-package" (list +ppc-r14+) 1 +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ -1) (ppc-emit-beq buf no)
        (prop +ppc-r28+ "device_type" +ppc-r14+ 16)
        (li +ppc-r15+ #x73657269)                                ; "seri"
        (ppc-emit-cmpw buf +ppc-r14+ +ppc-r15+) (ppc-emit-bne buf no)
        (li +ppc-r19+ 1)
        (ppc-emit-label buf no))
      ;; ---- RAM: /memory's first (base, size)
      (let ((none (mvm-make-label)))
        (cif "finddevice" (list (list :str "/memory")) 1 +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ -1)
        (ppc-emit-beq buf none)
        (cif "getprop" (list +ppc-r28+ (list :str "reg") (list :buf 0) (list :imm 8)) 1)
        (ppc-emit-lwz buf +ppc-r27+ +ppc-r1+ (+ 512 4))
        (ppc-emit-label buf none))
      ;; ---- the timebase's rate: the first CPU's timebase-frequency, into r28
      ;; (QEMU's 100 MHz when the firmware does not say)
      (let ((none (mvm-make-label)) (have (mvm-make-label)))
        (cif "finddevice" (list (list :str "/cpus")) 1 +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ -1) (ppc-emit-beq buf none)
        (cif "child" (list +ppc-r28+) 1 +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ 0) (ppc-emit-beq buf none)
        (ppc-emit-cmpwi buf +ppc-r28+ -1) (ppc-emit-beq buf none)
        (prop +ppc-r28+ "timebase-frequency" +ppc-r28+)
        (ppc-emit-cmpwi buf +ppc-r28+ 0) (ppc-emit-bne buf have)
        (ppc-emit-label buf none)
        (li +ppc-r28+ 100000000)
        (ppc-emit-label buf have))
      (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 2048))))

(defun ppcmac-emit-device-bats (buf)
  "DBAT3 (and on a G4e DBAT4) for mac-io (PA r25, #x80000000 when the firmware
   gave none) and the framebuffer (PA r20).  r17 = mac-io's VA, r18 = the
   framebuffer's.  When the framebuffer is within 28 MB of mac-io's 32 MB-aligned
   base (QEMU mac99: #x81000000), ONE 32 MB BAT at +PPCMAC-DEVICE-VA+ maps both.
   Otherwise (a real Mac's graphics card) mac-io's 16 MB-aligned 16 MB go there
   (the PCI devices beside it too: USB's controllers) and, on a G4e
   (PVR in r16), DBAT4 maps the framebuffer's 16 MB-aligned 16 MB at
   +PPCMAC-FB-VA+ with HID0[HIGH_BAT_EN]; on a 7400/7410 there is no BAT left
   and the framebuffer is not mapped (r18 = 0).  All cache-inhibited, guarded."
  (let ((have-io (mvm-make-label)) (split (mvm-make-label)) (done (mvm-make-label))
        (no-fb (mvm-make-label)))
    (flet ((li (r v) (ppc-emit-li buf r v)))
      (ppc-emit-cmpwi buf +ppc-r25+ 0)
      (ppc-emit-bne buf have-io)
      (li +ppc-r25+ #x80000000)
      (ppc-emit-label buf have-io)
      (ppc-emit-rlwinm buf +ppc-r14+ +ppc-r25+ 0 0 6)             ; 32 MB-aligned base
      (ppc-emit-subf buf +ppc-r15+ +ppc-r14+ +ppc-r20+)          ; fb - base
      (li +ppc-r3+ #x01C00000)
      (ppc-emit-cmpl-word buf +ppc-r15+ +ppc-r3+)
      (ppc-emit-bge buf split)
      (ppc-emit-cmpwi buf +ppc-r20+ 0)
      (ppc-emit-beq buf split)
      ;; one BAT: both
      (li +ppc-r3+ +ppcmac-boot-info+)                             ; the window, for Lisp
      (ppc-emit-stw buf +ppc-r14+ +ppc-r3+ 52)
      (li +ppc-r4+ #x02000000) (ppc-emit-stw buf +ppc-r4+ +ppc-r3+ 56)
      (ppc-emit-ori buf +ppc-r3+ +ppc-r14+ #x2A) (ppcmac-mtspr buf 543 +ppc-r3+)
      (li +ppc-r3+ (logior +ppcmac-device-va+ #x3FF)) (ppcmac-mtspr buf 542 +ppc-r3+)
      (ppc-emit-subf buf +ppc-r17+ +ppc-r14+ +ppc-r25+)
      (ppc-emit-addis buf +ppc-r17+ +ppc-r17+ (ash +ppcmac-device-va+ -16))
      (ppc-emit-addis buf +ppc-r18+ +ppc-r15+ (ash +ppcmac-device-va+ -16))
      (ppc-emit-b buf done)
      ;; mac-io alone: its 16 MB (and the PCI devices beside it: USB's controllers)
      (ppc-emit-label buf split)
      (ppc-emit-rlwinm buf +ppc-r14+ +ppc-r25+ 0 0 7)             ; 16 MB-aligned
      (li +ppc-r3+ +ppcmac-boot-info+)                             ; the window, for Lisp
      (ppc-emit-stw buf +ppc-r14+ +ppc-r3+ 52)
      (li +ppc-r4+ #x01000000) (ppc-emit-stw buf +ppc-r4+ +ppc-r3+ 56)
      (ppc-emit-ori buf +ppc-r3+ +ppc-r14+ #x2A) (ppcmac-mtspr buf 543 +ppc-r3+)
      (li +ppc-r3+ (logior +ppcmac-device-va+ #x1FF)) (ppcmac-mtspr buf 542 +ppc-r3+)
      (ppc-emit-subf buf +ppc-r17+ +ppc-r14+ +ppc-r25+)
      (ppc-emit-addis buf +ppc-r17+ +ppc-r17+ (ash +ppcmac-device-va+ -16))
      (li +ppc-r18+ 0)
      ;; the framebuffer: DBAT4 on a G4e
      (ppc-emit-cmpwi buf +ppc-r20+ 0)
      (ppc-emit-beq buf done)
      (ppc-emit-rlwinm buf +ppc-r3+ +ppc-r16+ 16 16 31)
      (ppc-emit-addi buf +ppc-r3+ +ppc-r3+ (logand (- #x8000) #xFFFF))
      (li +ppc-r4+ 5)
      (ppc-emit-cmpl-word buf +ppc-r3+ +ppc-r4+)
      (ppc-emit-bge buf no-fb)
      (ppcmac-emit-mfspr buf +ppc-r3+ 1008)                       ; HID0
      (ppc-emit-oris buf +ppc-r3+ +ppc-r3+ #x0080)                ; HIGH_BAT_EN
      (ppcmac-mtspr buf 1008 +ppc-r3+)
      (ppc-emit-word buf #x4C00012C)                              ; isync
      (ppc-emit-rlwinm buf +ppc-r14+ +ppc-r20+ 0 0 7)             ; 16 MB-aligned
      (ppc-emit-ori buf +ppc-r3+ +ppc-r14+ #x2A) (ppcmac-mtspr buf 569 +ppc-r3+)
      (li +ppc-r3+ (logior +ppcmac-fb-va+ #x1FF)) (ppcmac-mtspr buf 568 +ppc-r3+)
      (ppc-emit-subf buf +ppc-r18+ +ppc-r14+ +ppc-r20+)
      (ppc-emit-addis buf +ppc-r18+ +ppc-r18+ (ash +ppcmac-fb-va+ -16))
      (ppc-emit-b buf done)
      (ppc-emit-label buf no-fb)
      (li +ppc-r18+ 0)
      (ppc-emit-label buf done))))

(defun ppcmac-emit-heap-bats (buf)
  "DBAT1 for VA #x10000000.., the fixed block, the heap and the kind map.  With
   512 MB or more (r27, /memory's size; 0 when the firmware said nothing) it is
   identity, 256 MB.  With less, on a G4e: DBAT1 maps the heap's 128 MB at PA
   #x08000000 (so 256 MB is the least) and DBAT5 the kind map's 4 MB at
   #x1FC00000 to PA #x04000000, below the stack and above the native tier's
   code area.  A PowerBook G4 ships with 256 or 512 MB.  On a 7400/7410 with
   less than 512 MB there are no BATs for it: the identity map stands."
  (let ((big (mvm-make-label)) (done (mvm-make-label)))
    (flet ((li (r v) (ppc-emit-li buf r v)))
      (ppc-emit-cmpwi buf +ppc-r27+ 0)
      (ppc-emit-beq buf big)
      (li +ppc-r3+ #x20000000)
      (ppc-emit-cmpl-word buf +ppc-r27+ +ppc-r3+)
      (ppc-emit-bge buf big)
      (ppc-emit-rlwinm buf +ppc-r3+ +ppc-r16+ 16 16 31)          ; a G4e?
      (ppc-emit-addi buf +ppc-r3+ +ppc-r3+ (logand (- #x8000) #xFFFF))
      (li +ppc-r4+ 5)
      (ppc-emit-cmpl-word buf +ppc-r3+ +ppc-r4+)
      (ppc-emit-bge buf big)
      (ppcmac-emit-mfspr buf +ppc-r3+ 1008)                       ; HID0: HIGH_BAT_EN
      (ppc-emit-oris buf +ppc-r3+ +ppc-r3+ #x0080)
      (ppcmac-mtspr buf 1008 +ppc-r3+)
      (ppc-emit-word buf #x4C00012C)
      (li +ppc-r3+ #x08000002) (ppcmac-mtspr buf 539 +ppc-r3+)    ; DBAT1L: PA 128 MB
      (li +ppc-r3+ #x10000FFF) (ppcmac-mtspr buf 538 +ppc-r3+)    ; DBAT1U: 128 MB
      (li +ppc-r3+ #x04000002) (ppcmac-mtspr buf 571 +ppc-r3+)    ; DBAT5L: PA 64 MB
      (li +ppc-r3+ #x1FC0007F) (ppcmac-mtspr buf 570 +ppc-r3+)    ; DBAT5U: 4 MB
      (ppc-emit-b buf done)
      (ppc-emit-label buf big)
      (li +ppc-r3+ #x10000002) (ppcmac-mtspr buf 539 +ppc-r3+)    ; identity, 256 MB
      (li +ppc-r3+ #x10001FFF) (ppcmac-mtspr buf 538 +ppc-r3+)
      (ppc-emit-label buf done))))

(defun emit-ppcmac-entry (mbuf)
  "Entry stub of the bare PowerPC Mac CL image.  See the file header."
  (let* ((*ppc-64-bit* nil)
         (buf (make-ppc-buffer))
         (base +linux-ppc-heap-addr+)
         (size +ppcmac-heap-size+)
         (mid (ash size -1))
         (alloc-start +linux-ppc-heap-alloc-start+)
         (space (- mid alloc-start))
         (margin (min +linux-ppc-gc-margin-max+ (logand (ash space -2) (lognot 15)))))
    (flet ((li (r v) (ppc-emit-li buf r v))
           (fill-range (start bytes value)
             ;; r3 = cursor, r4 = value, CTR = words
             (let ((top (mvm-make-label)))
               (ppc-emit-li buf +ppc-r3+ start)
               (ppc-emit-li buf +ppc-r4+ value)
               (ppc-emit-li buf +ppc-r5+ (floor bytes 4))
               (ppc-emit-mtctr buf +ppc-r5+)
               (ppc-emit-label buf top)
               (ppc-emit-stw buf +ppc-r4+ +ppc-r3+ 0)
               (ppc-emit-addi buf +ppc-r3+ +ppc-r3+ 4)
               (ppc-emit-bc buf 16 0 top))))                      ; bdnz
      ;; ---- 0. ask Open Firmware, while it is still there (PPCMAC-EMIT-OF-QUERY):
      ;; r20 fb PA, r21 width, r22 height, r23 linebytes, r24 depth, r25 mac-io PA,
      ;; r26 the VIA's kind, r27 RAM; on an 8-bit screen, a colour cube in its CLUT
      (ppcmac-emit-of-query buf)
      ;; ---- 1. translation off (and EE off), under OF's identity mapping ----
      (ppc-emit-word buf #x7C6000A6)                             ; mfmsr r3
      (ppc-emit-rlwinm buf +ppc-r3+ +ppc-r3+ 0 28 25)            ; IR, DR off
      (ppc-emit-rlwinm buf +ppc-r3+ +ppc-r3+ 0 17 15)            ; EE off
      (ppc-emit-word buf #x7C600124)                             ; mtmsr r3
      (ppc-emit-word buf #x4C00012C)                             ; isync
      ;; ---- 2. the BATs: all off, then ours (lower word first) --------------
      (ppcmac-emit-mfspr buf +ppc-r16+ 287)                      ; r16 = PVR
      (li +ppc-r0+ 0)
      (loop for spr from 528 to 543 do (ppcmac-mtspr buf spr +ppc-r0+))
      (let ((not-g4e (mvm-make-label)))
        ;; a G4e (745x / 7447 / 7448: PVR #x8000..#x8004) has BATs 4-7 too
        (ppc-emit-rlwinm buf +ppc-r3+ +ppc-r16+ 16 16 31)       ; PVR >> 16
        (ppc-emit-addi buf +ppc-r3+ +ppc-r3+ (logand (- #x8000) #xFFFF))
        (ppc-emit-li buf +ppc-r4+ 5)
        (ppc-emit-cmpl-word buf +ppc-r3+ +ppc-r4+)
        (ppc-emit-bge buf not-g4e)
        (ppc-emit-li buf +ppc-r0+ 0)
        (loop for spr from 560 to 575 do (ppcmac-mtspr buf spr +ppc-r0+))
        (ppc-emit-label buf not-g4e))
      (ppc-emit-word buf #x4C00012C)                             ; isync
      (dolist (bat `((528 529 #x00000FFF #x00000002)   ; IBAT0: 0..128 MB
                     (536 537 #x00000FFF #x00000002)   ; DBAT0: 0..128 MB
                     (540 541 #xDEAC0003 ,(logior +ppcmac-nil-pa+ 2))))  ; DBAT2: NIL
        (destructuring-bind (u l uval lval) bat
          (li +ppc-r3+ lval) (ppcmac-mtspr buf l +ppc-r3+)
          (li +ppc-r3+ uval) (ppcmac-mtspr buf u +ppc-r3+)))
      ;; the devices: r17 = mac-io's VA, r18 = the framebuffer's (0: none)
      (ppcmac-emit-device-bats buf)
      ;; the heap window: identity on 512 MB, moved down on less (PPCMAC-EMIT-HEAP-BATS)
      (ppcmac-emit-heap-bats buf)
      (ppc-emit-word buf #x4C00012C)                             ; isync
      ;; ---- 3. translation on: the BATs, and FP ------------------------------
      (ppc-emit-word buf #x7C6000A6)                             ; mfmsr r3
      (ppc-emit-ori buf +ppc-r3+ +ppc-r3+ #x2030)                ; FP | IR | DR
      (ppc-emit-word buf #x7C600124)                             ; mtmsr r3
      (ppc-emit-word buf #x4C00012C)                             ; isync
      ;; ---- 3b. what the firmware said, where Lisp reads it (+PPCMAC-BOOT-INFO+)
      (li +ppc-r11+ +ppcmac-boot-info+)
      (li +ppc-r12+ #x5050434D) (ppc-emit-stw buf +ppc-r12+ +ppc-r11+ 0)   ; "PPCM"
      (li +ppc-r12+ 0) (ppc-emit-stw buf +ppc-r12+ +ppc-r11+ #x114)          ; the serial port: alive
      ;; the timebase's rate for #x0311 (translate-ppc +PPC-MAC-TB-RATE+):
      ;; r28 >> 16, tagged = r28 >> 15 with bit 0 clear; and in the boot info
      (ppc-emit-rlwinm buf +ppc-r12+ +ppc-r28+ 17 14 30)
      (ppc-emit-stw buf +ppc-r12+ +ppc-r11+ #x118)
      (ppc-emit-stw buf +ppc-r28+ +ppc-r11+ 60)
      (loop for (r off) in (list (list +ppc-r18+ 4) (list +ppc-r21+ 8) (list +ppc-r22+ 12)
                                 (list +ppc-r23+ 16) (list +ppc-r24+ 20) (list +ppc-r17+ 24)
                                 (list +ppc-r26+ 28) (list +ppc-r27+ 32) (list +ppc-r20+ 36)
                                 (list +ppc-r25+ 40) (list +ppc-r16+ 44) (list +ppc-r19+ 48))
            do (ppc-emit-stw buf r +ppc-r11+ off))
      ;; and on the console: the PVR, the framebuffer PA and VA, its depth, mac-io
      ;; PA, the VIA kind (1 CUDA, 2 PMU), the RAM
      (let ((*ppc-mac-mode* t))
        (dolist (r (list +ppc-r16+ +ppc-r20+ +ppc-r18+ +ppc-r24+ +ppc-r25+ +ppc-r26+ +ppc-r27+ +ppc-r19+ +ppc-r28+))
          (ppcmac-emit-put-hex buf r)))
      ;; ---- 4. what the hosted stub does after its mmaps ----------------------
      (li +ppc-r1+ (- +ppcmac-stack-top+ 64))
      (li +ppc-r0+ 0)
      (ppc-emit-stw buf +ppc-r0+ +ppc-r1+ 0)                     ; back chain 0
      (fill-range base #x28000 0)                                ; the fixed block, argv
      (fill-range +ppc-gc-kind-map+ (ash size -4) 0)             ; the kind map
      (fill-range #xDEAC0000 #x20000 +nil-value+)                ; NIL's page (and block)
      (fill-range #x07D00000 #x10000 0)                          ; the mutex arena
      ;; the handler-frame extension: fixed (translate-ppc's Mac arm), capped
      (li +ppc-r3+ +ppc-hstack-ext-limit-addr+)
      (li +ppc-r4+ +ppcmac-hstack-ext-frames+)
      (ppc-emit-stw buf +ppc-r4+ +ppc-r3+ 0)
      ;; MVM registers: VA = r19, VL = r20, VN = r21
      (li +ppc-r19+ (+ base alloc-start))
      (li +ppc-r20+ (- (+ base mid) margin))
      (li +ppc-r21+ +nil-value+)
      ;; the Cheney block at #x10000040
      (li +ppc-r11+ (+ base #x40))
      (ppc-emit-stw buf +ppc-r19+ +ppc-r11+ #x00)                ; from_start
      (li +ppc-r12+ (+ base mid))
      (ppc-emit-stw buf +ppc-r12+ +ppc-r11+ #x08)                ; to_start
      (li +ppc-r12+ space)
      (ppc-emit-stw buf +ppc-r12+ +ppc-r11+ #x10)                ; space_size
      (ppc-emit-stw buf +ppc-r1+ +ppc-r11+ #x18)                 ; stack_base
      (li +ppc-r12+ 0)
      (ppc-emit-stw buf +ppc-r12+ +ppc-r11+ #x20)                ; gc_count
      ;; VFP needs a frame before the first prologue runs
      (ppc-emit-mr buf +ppc-r31+ +ppc-r1+)
      ;; a first byte on the console, before any Lisp: the stub ran
      (li +ppc-r11+ (char-code #\M))
      (let ((*ppc-mac-mode* t)) (ppc-emit-mac-putc buf +ppc-r11+))
      (ppc-fixup-labels buf)
      (loop for b across (ppc-buffer-to-bytes buf)
            do (mvm-emit-byte mbuf b)))))

(defun ppcmac-cl-boot-descriptor ()
  "Boot descriptor for the bare PowerPC Mac CL image: the hosted ppc32 ELF's
   shape (Open Firmware loads ELF), the Mac's entry stub."
  (list :arch :ppc32
        :entry-fn #'emit-ppcmac-entry
        :elf-machine 20                 ; EM_PPC
        :elf-class 32
        :load-addr +ppcmac-load-addr+
        :stack-top +ppcmac-stack-top+
        :heap-base +linux-ppc-heap-addr+
        :cons-base +linux-ppc-heap-addr+
        :endianness :big))

;;; ============================================================
;;; THE FIRST STAGE: modus-boot.elf
;;; ============================================================
;;;
;;; A 37 MB ELF cannot simply be Open Firmware's `boot' file on a real Mac.
;;; Apple's firmware reads the whole file at its load-base (#x800000) and then
;;; copies each segment into place -- and our first segment goes at #x10000,
;;; inside that buffer.  (OpenBIOS reads at #x04000000, which is why QEMU never
;;; minded; and OpenBIOS rejects `to load-base' from a boot script.)  So, as
;;; yaboot does for Linux, the firmware boots a small loader instead, linked
;;; at +PPCMAC-LOADER-BASE+ clear of everything the image uses; it opens the
;;; image named by /chosen's bootargs through the client interface, claims and
;;; reads each PT_LOAD straight into place (a dot a megabyte on the firmware's
;;; console), zeroes its BSS, flushes the caches over it, and jumps to e_entry
;;; with r5 = the client interface -- the handoff EMIT-PPCMAC-ENTRY expects.
;;; scripts/ppcmac-disk.lisp puts both files on the disk; bootinfo.txt boots
;;; the loader with the image's path as its argument.

(defconstant +ppcmac-loader-base+ #x07A00000
  "Where modus-boot.elf runs: above the handler-frame extension and NIL's block
   (#x07800000..), below the mutex arena (#x07D00000).  64 KB: its code, then a
   header buffer at +#x8000 and the image's path at +#x9000.")

(defun ppcmac-loader-code ()
  "modus-boot.elf's code, as bytes (it runs at +PPCMAC-LOADER-BASE+)."
  (let* ((*ppc-64-bit* nil)
         (buf (make-ppc-buffer))
         (strs '("finddevice" "getprop" "open" "read" "seek" "close" "claim" "write" "exit"
                 "/chosen" "bootargs" "stdout" "." "hd:,\\modus.elf"))
         (msgs `(("boot" . ,(format nil "~C~Cmodus: loading " #\Return #\Newline))
                 ("fail" . ,(format nil "~C~Cmodus: cannot read the image~C~C" #\Return #\Newline #\Return #\Newline))
                 ("go" . ,(format nil " ok~C~C" #\Return #\Newline))))
         (all (append strs (mapcar #'cdr msgs)))
         (offs (let ((o 0) (h nil))
                 (dolist (s all h)
                   (push (cons s o) h)
                   (incf o (* 4 (ceiling (1+ (length s)) 4))))))
         (hdr (+ +ppcmac-loader-base+ #x8000))
         (path (+ +ppcmac-loader-base+ #x9000))
         (past (mvm-make-label)) (fail (mvm-make-label)))
    (labels ((off (s) (cdr (assoc s offs :test #'string=)))
             (msg (k) (cdr (assoc k msgs :test #'string=)))
             (li (r v) (ppc-emit-li buf r v))
             (arg (i spec)
               (cond ((integerp spec) (ppc-emit-mr buf +ppc-r3+ spec))
                     ((eq (car spec) :imm) (li +ppc-r3+ (second spec)))
                     ((eq (car spec) :str) (ppc-emit-addi buf +ppc-r3+ +ppc-r29+ (off (second spec)))))
               (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ (+ 76 (* 4 i))))
             (cif (service args nret &optional into)
               (ppc-emit-addi buf +ppc-r3+ +ppc-r29+ (off service))
               (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 64)
               (li +ppc-r3+ (length args)) (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 68)
               (li +ppc-r3+ nret) (ppc-emit-stw buf +ppc-r3+ +ppc-r1+ 72)
               (loop for a in args for i from 0 do (arg i a))
               (ppc-emit-addi buf +ppc-r3+ +ppc-r1+ 64)
               (ppc-emit-mtctr buf +ppc-r30+)
               (ppc-emit-bctrl buf)
               (when into (ppc-emit-lwz buf into +ppc-r1+ (+ 76 (* 4 (length args))))))
             (say (s)
               ;; write(stdout, s, len) when there is a stdout
               (let ((skip (mvm-make-label)))
                 (ppc-emit-cmpwi buf +ppc-r27+ 0)
                 (ppc-emit-beq buf skip)
                 (cif "write" (list +ppc-r27+ (list :str s) (list :imm (length s))) 1)
                 (ppc-emit-label buf skip))))
      (ppc-emit-mr buf +ppc-r30+ +ppc-r5+)
      (ppc-emit-stwu buf +ppc-r1+ +ppc-r1+ (logand -1024 #xFFFF))
      (ppc-emit-bl buf past)
      (dolist (s all)
        (let ((bytes (append (map 'list #'char-code s)
                             (make-list (- (* 4 (ceiling (1+ (length s)) 4)) (length s)) :initial-element 0))))
          (loop for (a b c d) on bytes by #'cddddr
                do (ppc-emit-word buf (logior (ash a 24) (ash b 16) (ash c 8) d)))))
      (ppc-emit-label buf past)
      (ppc-emit-mflr buf +ppc-r29+)
      ;; /chosen: stdout (r27, for messages) and bootargs (the image's path)
      (cif "finddevice" (list (list :str "/chosen")) 1 +ppc-r28+)
      (li +ppc-r27+ 0)
      (li +ppc-r3+ 0) (li +ppc-r4+ path) (ppc-emit-stw buf +ppc-r3+ +ppc-r4+ 0)
      (cif "getprop" (list +ppc-r28+ (list :str "stdout") (list :imm (+ path #x100)) (list :imm 4)) 1 +ppc-r15+)
      (let ((no (mvm-make-label)))
        (ppc-emit-cmpwi buf +ppc-r15+ 4) (ppc-emit-bne buf no)
        (li +ppc-r4+ (+ path #x100)) (ppc-emit-lwz buf +ppc-r27+ +ppc-r4+ 0)
        (ppc-emit-label buf no))
      (cif "getprop" (list +ppc-r28+ (list :str "bootargs") (list :imm path) (list :imm 255)) 1 +ppc-r15+)
      (let ((have (mvm-make-label)) (copy (mvm-make-label)))
        (li +ppc-r4+ path) (ppc-emit-lbz buf +ppc-r3+ +ppc-r4+ 0)
        (ppc-emit-cmpwi buf +ppc-r15+ 2) (ppc-emit-blt buf copy)
        (ppc-emit-cmpwi buf +ppc-r3+ 0) (ppc-emit-bne buf have)
        ;; no bootargs: the default, hd:,\modus.elf
        (ppc-emit-label buf copy)
        (let ((top (mvm-make-label)) (s "hd:,\\modus.elf"))
          (ppc-emit-addi buf +ppc-r5+ +ppc-r29+ (off s))
          (li +ppc-r6+ (1+ (length s))) (ppc-emit-mtctr buf +ppc-r6+)
          (ppc-emit-label buf top)
          (ppc-emit-lbz buf +ppc-r3+ +ppc-r5+ 0) (ppc-emit-stb buf +ppc-r3+ +ppc-r4+ 0)
          (ppc-emit-addi buf +ppc-r5+ +ppc-r5+ 1) (ppc-emit-addi buf +ppc-r4+ +ppc-r4+ 1)
          (ppc-emit-bc buf 16 0 top))
        (ppc-emit-label buf have))
      (say (msg "boot"))
      ;; open the image; read its ELF header and program headers
      (cif "open" (list (list :imm path)) 1 +ppc-r28+)
      (ppc-emit-cmpwi buf +ppc-r28+ 0) (ppc-emit-beq buf fail)
      (cif "seek" (list +ppc-r28+ (list :imm 0) (list :imm 0)) 1)
      (cif "read" (list +ppc-r28+ (list :imm hdr) (list :imm 4096)) 1 +ppc-r15+)
      (ppc-emit-cmpwi buf +ppc-r15+ 52) (ppc-emit-blt buf fail)
      (li +ppc-r26+ hdr)
      (ppc-emit-lwz buf +ppc-r20+ +ppc-r26+ 24)                 ; e_entry
      (ppc-emit-lwz buf +ppc-r22+ +ppc-r26+ 28)                 ; e_phoff
      (ppc-emit-lhz buf +ppc-r23+ +ppc-r26+ 44)                 ; e_phnum
      (ppc-emit-add buf +ppc-r21+ +ppc-r26+ +ppc-r22+)          ; first phdr
      (let ((ph (mvm-make-label)) (next (mvm-make-label)) (done (mvm-make-label)))
        (ppc-emit-label buf ph)
        (ppc-emit-cmpwi buf +ppc-r23+ 0) (ppc-emit-beq buf done)
        (ppc-emit-lwz buf +ppc-r3+ +ppc-r21+ 0)
        (ppc-emit-cmpwi buf +ppc-r3+ 1) (ppc-emit-bne buf next)  ; PT_LOAD only
        (ppc-emit-lwz buf +ppc-r16+ +ppc-r21+ 4)                ; p_offset
        (ppc-emit-lwz buf +ppc-r17+ +ppc-r21+ 8)                ; p_vaddr
        (ppc-emit-lwz buf +ppc-r18+ +ppc-r21+ 16)               ; p_filesz
        (ppc-emit-lwz buf +ppc-r19+ +ppc-r21+ 20)               ; p_memsz
        (cif "claim" (list +ppc-r17+ +ppc-r19+ (list :imm 0)) 1)
        (cif "seek" (list +ppc-r28+ (list :imm 0) +ppc-r16+) 1)
        ;; read in megabytes: r14 = where, r15 = left
        (ppc-emit-mr buf +ppc-r14+ +ppc-r17+) (ppc-emit-mr buf +ppc-r15+ +ppc-r18+)
        (let ((chunk (mvm-make-label)) (small (mvm-make-label)) (rd (mvm-make-label)) (rdone (mvm-make-label)))
          (ppc-emit-label buf chunk)
          (ppc-emit-cmpwi buf +ppc-r15+ 0) (ppc-emit-ble buf rdone)
          (li +ppc-r24+ #x100000)
          (ppc-emit-cmpw buf +ppc-r15+ +ppc-r24+) (ppc-emit-bge buf rd)
          (ppc-emit-label buf small)
          (ppc-emit-mr buf +ppc-r24+ +ppc-r15+)
          (ppc-emit-label buf rd)
          (cif "read" (list +ppc-r28+ +ppc-r14+ +ppc-r24+) 1 +ppc-r25+)
          (ppc-emit-cmpwi buf +ppc-r25+ 0) (ppc-emit-ble buf fail)
          (ppc-emit-add buf +ppc-r14+ +ppc-r14+ +ppc-r25+)
          (ppc-emit-subf buf +ppc-r15+ +ppc-r25+ +ppc-r15+)
          (say ".")
          (ppc-emit-b buf chunk)
          (ppc-emit-label buf rdone))
        ;; the BSS: [vaddr+filesz, vaddr+memsz) zeroed
        (let ((z (mvm-make-label)) (zd (mvm-make-label)))
          (ppc-emit-add buf +ppc-r14+ +ppc-r17+ +ppc-r18+)
          (ppc-emit-add buf +ppc-r15+ +ppc-r17+ +ppc-r19+)
          (li +ppc-r3+ 0)
          (ppc-emit-label buf z)
          (ppc-emit-cmpl-word buf +ppc-r14+ +ppc-r15+) (ppc-emit-bge buf zd)
          (ppc-emit-stb buf +ppc-r3+ +ppc-r14+ 0)
          (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 1)
          (ppc-emit-b buf z)
          (ppc-emit-label buf zd))
        ;; the caches over what was read: dcbst, sync, icbi, isync
        (let ((d (mvm-make-label)) (i (mvm-make-label)))
          (ppc-emit-rlwinm buf +ppc-r14+ +ppc-r17+ 0 0 26)
          (ppc-emit-add buf +ppc-r15+ +ppc-r17+ +ppc-r18+)
          (ppc-emit-label buf d)
          (ppc-emit-word buf (logior #x7C00006C (ash +ppc-r14+ 11)))   ; dcbst 0,r14
          (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 32)
          (ppc-emit-cmpl-word buf +ppc-r14+ +ppc-r15+) (ppc-emit-blt buf d)
          (ppc-emit-sync buf)
          (ppc-emit-rlwinm buf +ppc-r14+ +ppc-r17+ 0 0 26)
          (ppc-emit-label buf i)
          (ppc-emit-word buf (logior #x7C0007AC (ash +ppc-r14+ 11)))   ; icbi 0,r14
          (ppc-emit-addi buf +ppc-r14+ +ppc-r14+ 32)
          (ppc-emit-cmpl-word buf +ppc-r14+ +ppc-r15+) (ppc-emit-blt buf i)
          (ppc-emit-sync buf)
          (ppc-emit-word buf #x4C00012C))                                ; isync
        (ppc-emit-label buf next)
        (ppc-emit-addi buf +ppc-r21+ +ppc-r21+ 32)
        (ppc-emit-addi buf +ppc-r23+ +ppc-r23+ -1)
        (ppc-emit-b buf ph)
        (ppc-emit-label buf done))
      (cif "close" (list +ppc-r28+) 0)
      (say (msg "go"))
      ;; into the image: r5 = the client interface, OF's stack, r3 = r4 = 0
      (ppc-emit-mr buf +ppc-r5+ +ppc-r30+)
      (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 1024)
      (li +ppc-r3+ 0) (li +ppc-r4+ 0)
      (ppc-emit-mtctr buf +ppc-r20+)
      (ppc-emit-bctr buf)
      ;; failure: say so and return to the firmware
      (ppc-emit-label buf fail)
      (say (msg "fail"))
      (cif "exit" nil 0)
      (ppc-emit-word buf #x48000000)                            ; b .
      (ppc-fixup-labels buf)
      (coerce (ppc-buffer-to-bytes buf) '(vector (unsigned-byte 8))))))

(defun ppcmac-loader-elf ()
  "modus-boot.elf: an ELF32 big-endian executable, one PT_LOAD at
   +PPCMAC-LOADER-BASE+ (file offset #x1000), 64 KB in memory."
  (let* ((code (ppcmac-loader-code))
         (out (make-array (+ #x1000 (length code)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((u16 (o v) (setf (aref out o) (ldb (byte 8 8) v) (aref out (1+ o)) (ldb (byte 8 0) v)))
           (u32 (o v) (dotimes (i 4) (setf (aref out (+ o i)) (ldb (byte 8 (- 24 (* 8 i))) v)))))
      (replace out #(#x7F #x45 #x4C #x46 1 2 1 0))
      (u16 16 2) (u16 18 20) (u32 20 1) (u32 24 +ppcmac-loader-base+) (u32 28 52)
      (u16 40 52) (u16 42 32) (u16 44 1)
      (u32 52 1) (u32 56 #x1000) (u32 60 +ppcmac-loader-base+) (u32 64 +ppcmac-loader-base+)
      (u32 68 (length code)) (u32 72 #x10000) (u32 76 7) (u32 80 #x1000)
      (replace out code :start1 #x1000)
      out)))
