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
;;;;   #x06000000..#x07000000  the stack, down from +PPCMAC-STACK-TOP+
;;;;   #x07000000..#x07200000  the handler-frame extension (fixed, 64K frames)
;;;;   #x07800000..#x07820000  NIL's 128 KB BAT block
;;;;   #x07D00000..#x07D10000  the mutex arena (%BARE-SYNC-ARENA)
;;;;   #x07E00000, #x07F00000  the CLI's scratch pages
;;;;   #x10000000..            the heap, +PPCMAC-HEAP-SIZE+, then its guard
;;;;   #x1FC00000..#x20000000  the kind map (heap/16)
;;;;
;;;; Interrupts stay off (MSR[EE] = 0) and Open Firmware's vectors stay in place:
;;;; a fault lands in OF.  Translated with PPC-SET-MAC-MODE (Mac traps, hosted
;;;; layout) -- see translate-ppc.lisp *PPC-MAC-MODE*.

(in-package :modus.mvm)

(defconstant +ppcmac-load-addr+   #x10000    "Image byte 0 (the ELF header).")
(defconstant +ppcmac-stack-top+   #x07000000 "Grows down; the extension is above.")
(defconstant +ppcmac-heap-size+   #x4000000
  "64 MB: two 32 MB semispaces, and a kind map (size/16) that ends exactly at
   #x20000000, the top of DBAT1.")
(defconstant +ppcmac-nil-pa+      #x07800000 "PA behind VA #xDEAC0000 (128 KB).")
(defconstant +ppcmac-device-va+   #x08000000
  "VA of PA #x80000000 (mac-io, then QEMU's framebuffer at +#x1000000): 32 MB.")
(defconstant +ppcmac-hstack-ext-frames+ #x10000
  "Frames in the fixed extension: 64K x 32 bytes = 2 MB at #x07000000.")

(defun ppcmac-mtspr (buf spr rs)
  (ppc-emit-word buf (logior (ash 31 26) (ash rs 21) (ash (logand spr 31) 16)
                             (ash (ash spr -5) 11) (ash 467 1))))

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
      ;; ---- 1. translation off (and EE off), under OF's identity mapping ----
      (ppc-emit-word buf #x7C6000A6)                             ; mfmsr r3
      (ppc-emit-rlwinm buf +ppc-r3+ +ppc-r3+ 0 28 25)            ; IR, DR off
      (ppc-emit-rlwinm buf +ppc-r3+ +ppc-r3+ 0 17 15)            ; EE off
      (ppc-emit-word buf #x7C600124)                             ; mtmsr r3
      (ppc-emit-word buf #x4C00012C)                             ; isync
      ;; ---- 2. the BATs: all off, then ours (lower word first) --------------
      (li +ppc-r0+ 0)
      (loop for spr from 528 to 543 do (ppcmac-mtspr buf spr +ppc-r0+))
      (ppc-emit-word buf #x4C00012C)                             ; isync
      (dolist (bat `((528 529 #x00000FFF #x00000002)   ; IBAT0: 0..128 MB
                     (536 537 #x00000FFF #x00000002)   ; DBAT0: 0..128 MB
                     (538 539 #x10001FFF #x10000002)   ; DBAT1: 256..512 MB
                     (540 541 #xDEAC0003 ,(logior +ppcmac-nil-pa+ 2))  ; DBAT2: NIL
                     (542 543 ,(logior +ppcmac-device-va+ #x3FF)       ; DBAT3: 32 MB
                              #x8000002A)))    ; -> PA #x80000000, I|G
        (destructuring-bind (u l uval lval) bat
          (li +ppc-r3+ lval) (ppcmac-mtspr buf l +ppc-r3+)
          (li +ppc-r3+ uval) (ppcmac-mtspr buf u +ppc-r3+)))
      (ppc-emit-word buf #x4C00012C)                             ; isync
      ;; ---- 3. translation on: the BATs, and FP ------------------------------
      (ppc-emit-word buf #x7C6000A6)                             ; mfmsr r3
      (ppc-emit-ori buf +ppc-r3+ +ppc-r3+ #x2030)                ; FP | IR | DR
      (ppc-emit-word buf #x7C600124)                             ; mtmsr r3
      (ppc-emit-word buf #x4C00012C)                             ; isync
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
