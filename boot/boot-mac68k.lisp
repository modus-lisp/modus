;;;; boot-mac68k.lisp — a bare 68k Macintosh: modus IS the boot disk.
;;;;
;;;; The Mac ROM reads the boot disk's first two blocks (the boot blocks: 'LK',
;;;; a branch, a header) and jumps into them; scripts/mac68k-disk.py writes boot
;;;; blocks that read this image off the disk to +MAC68K-LOAD-ADDR+ with the
;;;; ROM's own driver and jump to its first byte, the entry below.  No OS, no
;;;; System file, no MMU: the image runs on the Mac's physical RAM, with the
;;;; ROM still underneath it -- its low memory, system heap, interrupt handlers
;;;; and drivers are left alone, which is what keeps the serial port (the
;;;; console), Ticks (the clock) and the keyboard working.
;;;;
;;;; THE MEMORY MAP IS THE HOSTED ONE (boot-linux-68k.lisp), kept in real RAM,
;;;; as ARM32's bare CL image keeps its hosted map: the low block at #x10000000,
;;;; the heap above it, the native collector, the convention slots at
;;;; #x10000A00.  So the Mac needs RAM to #x18000000 (384 MB) -- what Basilisk
;;;; II is given.  Measured at boot-block time on Basilisk II with the LC 475
;;;; ROM and 384 MB: MemTop #x18000000, BufPtr #x17FFE000, the ROM's heap from
;;;; #x2000 and its boot stack near #x0C010000, the screen at #x18110000 and
;;;; the ROM at #x18000000.  So [#x0D000000, #x17F00000) is ours:
;;;;
;;;;   #x0D000000  this image (code, then its BSS)
;;;;   #x0FD00000  top of our stack (grows down)
;;;;   #x0FE00000  the CL image's I/O scratch pages (*CSTR-SCRATCH*, *IO-BUF-ADDR*)
;;;;   #x10000000  the low block, then the heap: two 48 MB semispaces + guard
;;;;   #x16800000  the collector's kind map (*68K-GC-KIND-MAP*)
;;;;   #x17F00000  NIL's page (NIL = #x17F00001), then T's
;;;;
;;;; Everything is below 2^29, the highest address a tagged fixnum can name.
;;;; A real LC 475 tops out at 36 MB; for it the same picture is built with the
;;;; 68LC040's MMU (it has one -- only the FPU is missing), arm32's way.  Not
;;;; yet.

(in-package :modus.mvm)

(defconstant +mac68k-load-addr+ #x0D000000)
(defconstant +mac68k-stack-top+ #x0FD00000)
(defconstant +mac68k-heap-size+ #x06000000
  "96 MB: two 48 MB semispaces; with the guard it ends at #x16400000, below the
   kind map.")
(defconstant +mac68k-kind-map+ #x16800000)
(defconstant +mac68k-nil-page+ #x17F00000)
(defconstant +mac68k-ser-out-pb+ #x10009000
  "The serial drivers' parameter blocks, in the low block's argv area (a Mac
   image has no argv); translate-68k's +68K-MAC-CONSOLE-CODE+ reads them.")
(defconstant +mac68k-ser-in-pb+ #x10009080)

(defun mac68k-configure (&optional (on t))
  "The translator and compiler settings a Mac image is built with: Mac mode,
   NIL's page in RAM, the kind map below it.  Call before building."
  (declare (ignore on))
  (m68k-set-mac-mode t)
  (setf *68k-nil-page* +mac68k-nil-page+
        *68k-gc-kind-map* +mac68k-kind-map+))

(defun emit-mac68k-zero (buf start bytes)
  "Zero BYTES (a multiple of 4) from START.  Clobbers D0/A0."
  (let ((top (mvm-make-label)))
    (m68k-emit-move-imm-an buf start +68k-a0+)
    (m68k-emit-move-imm-dn buf (floor bytes 4) +68k-d0+)
    (m68k-emit-label buf top)
    (m68k-emit-word buf #x4298)                         ; CLR.L (A0)+
    (m68k-emit-subq buf +68k-d0+ 1)
    (m68k-emit-bne buf top)))

(defun emit-mac68k-open (buf pb name-label)
  "PBOpen the driver named at NAME-LABEL (a Pascal string in this stub) into
   the zeroed parameter block PB: ioNamePtr at 18, ioPermssn 0."
  (m68k-emit-move-imm-an buf pb +68k-a0+)
  (m68k-emit-word buf #x43FA)                           ; LEA (d16,PC),A1
  (m68k-emit-word buf 0)
  (m68k-emit-fixup buf name-label :disp16)
  (m68k-emit-move-an-dn buf +68k-a1+ +68k-d0+)
  (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a0+ 18)
  (m68k-emit-word buf #xA000))                          ; _Open

(defun emit-mac68k-entry (mbuf)
  "Emit the bare-Mac entry into MBUF: the boot blocks jump here, in supervisor
   mode, on the ROM's boot stack."
  (let* ((buf (make-m68k-buffer))
         (over (mvm-make-label)) (ain (mvm-make-label)) (aout (mvm-make-label))
         (fill (mvm-make-label))
         (base #x10000000)
         (size +mac68k-heap-size+)
         (mid (floor size 2))
         (space (- mid +linux-68k-heap-alloc-start+))
         (margin (min +linux-68k-gc-margin-max+
                      (logand (floor space 4) (lognot 15)))))
    ;; supervisor, interrupts ON: the ROM's 60 Hz tick and serial drivers run
    (m68k-emit-word buf #x46FC) (m68k-emit-word buf #x2000)   ; MOVE.W #$2000,SR
    (m68k-emit-move-imm-an buf +mac68k-stack-top+ +68k-a7+)
    ;; the driver names, jumped over
    (m68k-emit-bra buf over)
    (m68k-emit-label buf ain)
    (m68k-emit-word buf #x042E) (m68k-emit-word buf #x4149) (m68k-emit-word buf #x6E00) ; 4 ".AIn"
    (m68k-emit-label buf aout)
    (m68k-emit-word buf #x052E) (m68k-emit-word buf #x414F) (m68k-emit-word buf #x7574) ; 5 ".AOut"
    (m68k-emit-label buf over)
    ;; the low block and the kind map start zeroed, as a fresh mmap does
    (emit-mac68k-zero buf base #x20000)
    (emit-mac68k-zero buf +mac68k-kind-map+
                      (ash (+ size +linux-68k-gc-guard+) (- (m68k-kind-shift))))
    ;; NIL's page and T's, filled with NIL: (car nil) and (cdr nil) are loads
    (m68k-emit-move-imm-an buf +mac68k-nil-page+ +68k-a0+)
    (m68k-emit-move-imm-dn buf +nil-value+ +68k-d1+)
    (m68k-emit-move-imm-dn buf 2048 +68k-d0+)
    (m68k-emit-label buf fill)
    (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-postinc+ +68k-a0+
                                               +ea-mode-dn+ +68k-d1+))  ; MOVE.L D1,(A0)+
    (m68k-emit-subq buf +68k-d0+ 1)
    (m68k-emit-bne buf fill)
    ;; the event queue: with no System, nothing has called InitEvents, and
    ;; GetOSEvent (the console's keyboard) faults without it -- measured
    (m68k-emit-moveq buf +68k-d0+ 20)
    (m68k-emit-word buf #xA06D)                          ; _InitEvents
    ;; the console: .AIn, then .AOut
    (emit-mac68k-open buf +mac68k-ser-in-pb+ ain)
    (emit-mac68k-open buf +mac68k-ser-out-pb+ aout)
    ;; MVM allocation registers: VA = a2, VL = a3, VN = a4
    (m68k-emit-move-imm-an buf (+ base +linux-68k-heap-alloc-start+) +68k-a2+)
    (m68k-emit-move-imm-an buf (- (+ base mid) margin) +68k-a3+)
    (m68k-emit-move-imm-an buf +nil-value+ +68k-a4+)
    (when *68k-a5-globals*
      (m68k-emit-move-imm-an buf *68k-hosted-globals-base* +68k-a5+))
    ;; the Cheney block at #x10000040, as the Linux stub writes it
    (m68k-emit-move-imm-an buf (+ base #x40) +68k-a0+)
    (m68k-emit-move-imm-dn buf (+ base +linux-68k-heap-alloc-start+) +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x00)   ; from_start
    (m68k-emit-move-imm-dn buf (+ base mid) +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x08)   ; to_start
    (m68k-emit-move-imm-dn buf space +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x10)   ; space_size
    (m68k-emit-move-an-dn buf +68k-a7+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x18)   ; stack_base
    (m68k-emit-moveq buf +68k-d1+ 0)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x20)   ; gc_count
    ;; VFP must point at a frame before the first prologue runs
    (m68k-emit-move-an-an buf +68k-a7+ +68k-a6+)
    (m68k-fixup-labels buf)
    (loop for b across (m68k-buffer-to-bytes buf)
          do (mvm-emit-byte mbuf b))))

(defun mac68k-boot-descriptor ()
  "Boot descriptor for the bare 68k Mac: a flat image (no ELF) the boot blocks
   load at +MAC68K-LOAD-ADDR+ and enter at its first byte."
  (list :arch :68k
        :entry-fn #'emit-mac68k-entry
        :load-addr +mac68k-load-addr+
        :heap-base #x10000000
        :cons-base #x10000000
        :endianness :big))
