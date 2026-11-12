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
;;;; THE MEMORY MAP IS THE HOSTED ONE, MOVED INTO A REAL MAC'S RAM.  The runtime
;;;; and the collector use the hosted 68k picture (boot-linux-68k.lisp): a low
;;;; block of convention slots and tables, the heap above it.  Its addresses
;;;; are the runtime-data region's, which the compiler and the translator
;;;; rebase by a link-time delta (compiler.lisp *CONV-RELATIVE* / *CONV-DELTA*,
;;;; CONV-REAL; docs/macos-hosting.md), so the region can sit anywhere.  On a
;;;; 32 MB Mac (an LC 475 or Quadra 605 tops out at 36): measured at boot-block
;;;; time on Basilisk II with the LC 475 ROM, the ROM's low memory and system
;;;; heap end at #x39FE0, its boot stack and our boot blocks sit near
;;;; #x01010000 (dead once the image runs), RAM ends at #x02000000, the ROM
;;;; follows it and the screen is at #x02110000.  So:
;;;;
;;;;   #x00100000  this image (code, then its BSS)
;;;;   #x00A80000  top of our stack (4 MB, down to the image's 6.5 MB mark)
;;;;   #x00A80000  the CL image's I/O scratch pages (*CSTR-SCRATCH*, *IO-BUF-ADDR*)
;;;;   #x00B00000  the low block (virtual #x10000000), then the heap: two 8 MB
;;;;               semispaces and a 1 MB guard
;;;;   #x01C00000  the collector's kind map (*68K-GC-KIND-MAP*)
;;;;   #x01F00000  NIL's page (NIL = #x01F00001), then T's
;;;;
;;;; Everything is below 2^29, the highest address a tagged fixnum can name.

(in-package :modus.mvm)

(defparameter *mac68k-machine*
  (let ((m (sb-ext:posix-getenv "MODUS_MAC68K_MACHINE")))
    (cond ((or (null m) (string= m "") (string-equal m "lc")) :lc)
          ((string-equal m "classic") :classic)
          (t (error "MODUS_MAC68K_MACHINE=~A: lc or classic" m))))
  "Which Mac's memory map (MODUS_MAC68K_MACHINE): :LC, the 32 MB LC 475 /
   Quadra 605 class this file was measured on, or :CLASSIC, a Mac Classic
   with 4 MB (its maximum) -- measured in MAME's macclasc when our boot
   blocks run: system heap from #x1600, application zone #x6DF4, the ROM's
   boot stack #x1FFBFC, the screen #x3FA700 up to MemTop #x400000, the ROM
   at #x400000.  A 68000, so build it with MODUS_68K_CPU=68000 too.")

(defparameter *mac68k-layouts*
  '((:lc      :load #x00100000 :stack-top #x00A80000 :scratch #x00A80000
              :region #x00B00000 :heap-size #x01000000 :gc-guard #x00100000
              :kind-map #x01C00000 :nil-page #x01F00000)
    ;; the Classic: image from 64 KB, I/O scratch (a CL image's) at 1.44 MB,
    ;; a 64 KB stack under 2 MB, the region at 2 MB with a 1 MB heap (two
    ;; 512 KB semispaces) and a 64 KB guard, the kind map, NIL's page under
    ;; the screen.  A CL REPL image (5.5 MB) does not fit 4 MB: this map is
    ;; for the ladder's images until it is made to.
    (:classic :load #x00010000 :stack-top #x00200000 :scratch #x00170000
              :region #x00200000 :heap-size #x00100000 :gc-guard #x00010000
              :kind-map #x00320000 :nil-page #x003E0000)))

(defun mac68k-layout (key)
  (or (getf (cdr (assoc *mac68k-machine* *mac68k-layouts*)) key)
      (error "mac68k: no ~S in the ~S layout" key *mac68k-machine*)))

(defparameter +mac68k-load-addr+ (mac68k-layout :load))
(defvar *mac68k-load-addr* +mac68k-load-addr+
  "Where this build is linked and the boot blocks put it: +MAC68K-LOAD-ADDR+
   for an image, elsewhere for the boot-disk loader that unpacks one there
   (mvm/mac68k-unlzma.lisp; build-hosted's MODUS_MAC68K_LOAD_ADDR).")
(defparameter +mac68k-stack-top+ (mac68k-layout :stack-top))
(defparameter +mac68k-scratch+ (mac68k-layout :scratch)
  "The I/O scratch pages: *CSTR-SCRATCH* here, *IO-BUF-ADDR* 256 KB on.")
(defparameter +mac68k-region+ (mac68k-layout :region)
  "Where the runtime-data region's base (virtual #x10000000) really is.")
(defparameter +mac68k-heap-size+ (mac68k-layout :heap-size)
  "LC: 16 MB, two 8 MB semispaces.  Classic: 1 MB.")
(defparameter +mac68k-gc-guard+ (mac68k-layout :gc-guard)
  "Past the second semispace (boot-linux-68k's guard is 4 MB).")
(defparameter +mac68k-kind-map+ (mac68k-layout :kind-map))
(defparameter +mac68k-nil-page+ (mac68k-layout :nil-page))
(defun mac68k-ser-out-pb ()
  "The serial drivers' parameter blocks, in the low block's argv area (a Mac
   image has no argv); translate-68k's +68K-MAC-CONSOLE-CODE+ reads them."
  (conv-real #x10009000))
(defun mac68k-ser-in-pb () (conv-real #x10009080))

(defun mac68k-configure (&optional (on t))
  "The translator and compiler settings a Mac image is built with: the
   runtime-data region moved to +MAC68K-REGION+ (it must be first: the mode
   setter places the convention slots with it), Mac mode, NIL's page and the
   kind map in RAM.  Call before building."
  (declare (ignore on))
  (setf *conv-relative* t
        *conv-delta* (- +mac68k-region+ +conv-region-base+))
  (m68k-set-mac-mode t)
  (setf *68k-nil-page* +mac68k-nil-page+
        *68k-gc-kind-map* +mac68k-kind-map+))

(defun mac68k-coinit-text ()
  "Source text the image runs first in KERNEL-MAIN: the same region delta for
   the compiler the image carries, which compiles every REPL form."
  (format nil "  (setq *conv-relative* t)~%  (setq *conv-delta* ~D)~%"
          (- +mac68k-region+ +conv-region-base+)))

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
         (base (conv-real #x10000000))
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
                      (ash (+ size +mac68k-gc-guard+) (- (m68k-kind-shift))))
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
    (emit-mac68k-open buf (mac68k-ser-in-pb) ain)
    (emit-mac68k-open buf (mac68k-ser-out-pb) aout)
    ;; MVM allocation registers: VA = a2, VL = a3, VN = a4
    (m68k-emit-move-imm-an buf (+ base +linux-68k-heap-alloc-start+) +68k-a2+)
    (m68k-emit-move-imm-an buf (- (+ base mid) margin) +68k-a3+)
    (m68k-emit-move-imm-an buf +nil-value+ +68k-a4+)
    (when *68k-a5-globals*
      (m68k-emit-move-imm-an buf (conv-real *68k-hosted-globals-base*) +68k-a5+))
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
        :load-addr *mac68k-load-addr*
        :heap-base +mac68k-region+
        :cons-base +mac68k-region+
        :endianness :big))
