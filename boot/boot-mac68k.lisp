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
              :kind-map #x01C00000 :nil-page #x01F00000
              ;; paged code (MODUS_68K_PAGED): the code cache and the loader's
              ;; staging buffer, between the resident image and the stack
              :cache-lo #x00400000 :cache-hi #x00660000
              :staging #x00660000 :staging-size #x00010000)
    ;; THE CLASSIC (4 MB, booting from a saved core): the resident image from
    ;; 64 KB, the static heap the core brings, then the runtime-data region
    ;; (low block, then the YOUNG pair right after it, so the kind map -- which
    ;; covers [region, young end) -- is small), the code cache and the
    ;; loader's staging buffer, NIL's page, the stack, the screen at #x3FA700.
    ;; The boot blocks read [image, low block end) in one go.  Without a core
    ;; (the build's core run, on a big Mac) the stub uses the :BOOT- values
    ;; instead: boot's 5 MB heap, a big code cache and stack, all above 4 MB.
    ;; Measured: the all-threaded paged image is 797 KB resident; boot's static
    ;; heap ~1.56 MB.  The core run's heap is at 16 MB so that ITS kind bytes
    ;; (kind map + (address - region) / 8) land above 4 MB, clear of this map.
    ;; Loaded at #x40000, not lower: on the 32 MB LC that runs the core build the
    ;; ROM's system heap reaches #x39FE0 when the boot blocks run (the Classic's
    ;; ends near #x8600), and the same binary must load on both.
    ;; Below the image, [#x10000, #x40000) is free on a Classic (its system
    ;; heap ends near #x8600) and is used only by a boot from the core: the
    ;; stack, the scratch page, the I/O buffer.  So the code cache has the rest
    ;; up to NIL's page: ~796 KB (measured: a DEFUN's active streams -- the
    ;; interpreter, the compiler's biggest pieces -- did not fit in 460 KB).
    (:classic :load #x00040000 :static-lo #x00110000 :static-hi #x002B0000
              :region #x002B0000 :heap-size #x00080000 :gc-guard #x00008000
              :kind-map #x00318000 :cache-lo #x00329000 :cache-hi #x003F0000
              :staging #x003D0000 :staging-size #x00010000
              :nil-page #x003F0000
              :stack-top #x00020000 :scratch #x00020000 :io-buf #x00021000
              :boot-heap #x01000000 :boot-heap-size #x00800000
              :boot-cache-lo #x01A00000 :boot-cache-hi #x01E00000
              :boot-stack-top #x01A00000)))

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
        *68k-gc-kind-map* +mac68k-kind-map+)
  ;; paged code: where the stream loader puts what it reads
  (setf *68k-paged-cache*
        (and *68k-paged*
             (list (mac68k-layout :cache-lo) (mac68k-layout :cache-hi)
                   (mac68k-layout :staging) (mac68k-layout :staging-size)
                   *mac68k-load-addr*))))

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

(defconstant +mac68k-save-core-word+ #x1000602C
  "Low-block word the stub sets from the core descriptor's SAVE flag: KERNEL-MAIN
   then sends a core over serial port A and stops (the build's core run).")

(defun emit-mac68k-heap (buf base lo half guard)
  "Zero the kind map over [BASE, LO + 2*HALF + GUARD), write the Cheney block for
   semispaces [LO, LO+HALF) and [LO+HALF, LO+2*HALF), and set VA / VL.
   GC-COUNT: zeroed unless KEEP-COUNT.  Clobbers D0/D1/A0."
  (let ((margin (min +linux-68k-gc-margin-max+ (logand (floor half 4) (lognot 15)))))
    (emit-mac68k-zero buf +mac68k-kind-map+
                      (logand (+ (ash (- (+ lo (* 2 half) guard) base) (- (m68k-kind-shift))) 3)
                              (lognot 3)))
    (m68k-emit-move-imm-an buf lo +68k-a2+)
    (m68k-emit-move-imm-an buf (- (+ lo half) margin) +68k-a3+)
    (m68k-emit-move-imm-an buf (+ base #x40) +68k-a0+)
    (m68k-emit-move-imm-dn buf lo +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x00)   ; from_start
    (m68k-emit-move-imm-dn buf (+ lo half) +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x08)   ; to_start
    (m68k-emit-move-imm-dn buf half +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x10)   ; space_size
    (m68k-emit-move-an-dn buf +68k-a7+ +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x18))) ; stack_base

(defun emit-mac68k-cache (buf lo hi)
  "Paged code: the code cache [LO, HI) into the low block (translate-68k
   +68K-CACHE-PTR+ / -HI+ / -LO+ / -MAP+): its OWNER MAP first, one 16-bit word
   per 64-byte granule of the whole cache, zeroed; the streams after it."
  (when *68k-paged*
    (let* ((map-bytes (ash (- hi lo) (- 1 +68k-cache-granule-shift+)))
           (start (logand (+ lo map-bytes 63) (lognot 63))))
      (m68k-emit-move-imm-an buf (conv-real +68k-cache-ptr+) +68k-a0+)
      (m68k-emit-move-imm-dn buf start +68k-d1+)
      (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 0)    ; the pointer
      (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 8)    ; +68K-CACHE-LO+: streams' start
      (m68k-emit-move-imm-dn buf hi +68k-d1+)
      (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 4)    ; +68K-CACHE-HI+
      (m68k-emit-move-imm-dn buf lo +68k-d1+)
      (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 12)   ; +68K-CACHE-MAP+
      (emit-mac68k-zero buf lo (logand (+ map-bytes 3) (lognot 3))))))

(defun emit-mac68k-entry (mbuf)
  "Emit the bare-Mac entry into MBUF: the boot blocks jump here, in supervisor
   mode, on the ROM's boot stack.

   THE CLASSIC (*MAC68K-MACHINE* :CLASSIC) has two boots.  With a SAVED CORE --
   the core descriptor's CORE flag set by scripts/mac68k-core.py, which put the
   low block and the static heap where the boot blocks read them -- the low
   block is NOT cleared, the young semispaces are the small ones after it, and
   KERNEL-MAIN resumes the REPL without boot init.  Without one (the build's
   core run, on a Mac with more than 4 MB) boot runs as ever but on the :BOOT-
   heap, cache and stack, above 4 MB; the descriptor's SAVE flag then has
   KERNEL-MAIN send the core over serial port A."
  (let* ((buf (make-m68k-buffer))
         (classic (eq *mac68k-machine* :classic))
         (over (mvm-make-label)) (ain (mvm-make-label)) (aout (mvm-make-label))
         (desc (mvm-make-label)) (core (mvm-make-label)) (common (mvm-make-label))
         (fill (mvm-make-label))
         (base (conv-real #x10000000))
         (size +mac68k-heap-size+)
         (mid (floor size 2))
         (alloc +linux-68k-heap-alloc-start+))
    ;; supervisor, interrupts ON: the ROM's 60 Hz tick and serial drivers run
    (m68k-emit-word buf #x46FC) (m68k-emit-word buf #x2000)   ; MOVE.W #$2000,SR
    ;; the driver names (and the Classic's core descriptor), jumped over
    (m68k-emit-bra buf over)
    (m68k-emit-label buf ain)
    (m68k-emit-word buf #x042E) (m68k-emit-word buf #x4149) (m68k-emit-word buf #x6E00) ; 4 ".AIn"
    (m68k-emit-label buf aout)
    (m68k-emit-word buf #x052E) (m68k-emit-word buf #x414F) (m68k-emit-word buf #x7574) ; 5 ".AOut"
    (when classic
      (m68k-emit-label buf desc)
      (m68k-emit-long buf #x6D6F6475) (m68k-emit-long buf #x734D4352)  ; "modusMCR"
      (m68k-emit-long buf 0)                                          ; +8 CORE
      (m68k-emit-long buf 0)                                          ; +12 SAVE
      (m68k-emit-long buf 0)                                          ; +16 } reserved
      (m68k-emit-long buf 0))                                         ; +20 }
    (m68k-emit-label buf over)
    (when classic
      (m68k-emit-word buf #x41FA) (m68k-emit-word buf 0)              ; LEA desc(PC),A0
      (m68k-emit-fixup buf desc :disp16)
      (m68k-emit-move-disp-dn buf +68k-a0+ 8 +68k-d0+)
      (m68k-emit-tst buf +68k-d0+)
      (m68k-emit-bne buf core))
    ;; ---- a boot from scratch ----
    (m68k-emit-move-imm-an buf (if classic (mac68k-layout :boot-stack-top) +mac68k-stack-top+)
                           +68k-a7+)
    (emit-mac68k-zero buf base #x20000)          ; the low block, as a fresh mmap
    (if classic
        (let ((half (floor (mac68k-layout :boot-heap-size) 2)))
          (emit-mac68k-heap buf base (mac68k-layout :boot-heap) half +mac68k-gc-guard+)
          (emit-mac68k-cache buf (mac68k-layout :boot-cache-lo) (mac68k-layout :boot-cache-hi))
          ;; the SAVE flag, for KERNEL-MAIN
          (m68k-emit-word buf #x41FA) (m68k-emit-word buf 0)
          (m68k-emit-fixup buf desc :disp16)
          (m68k-emit-move-disp-dn buf +68k-a0+ 12 +68k-d0+)
          (m68k-emit-move-imm-an buf (conv-real +mac68k-save-core-word+) +68k-a0+)
          (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a0+ 0))
        (progn
          ;; the kind map is cleared over (size + guard), as it always was
          (emit-mac68k-heap buf base (+ base alloc) (- mid alloc) (+ +mac68k-gc-guard+ alloc))
          (emit-mac68k-cache buf (if *68k-paged* (mac68k-layout :cache-lo) 0)
                             (if *68k-paged* (mac68k-layout :cache-hi) 0))))
    (m68k-emit-move-imm-an buf (+ base #x60) +68k-a0+)
    (m68k-emit-moveq buf +68k-d1+ 0)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 0)          ; gc_count
    (m68k-emit-bra buf common)
    ;; ---- the Classic from its core: the low block is the core's ----
    (m68k-emit-label buf core)
    (when classic
      (m68k-emit-move-imm-an buf +mac68k-stack-top+ +68k-a7+)
      (emit-mac68k-heap buf base (+ base alloc) (- mid alloc) +mac68k-gc-guard+)
      (emit-mac68k-cache buf (mac68k-layout :cache-lo) (mac68k-layout :cache-hi))
      ;; the serial drivers' parameter blocks: zeroed for the opens below
      (emit-mac68k-zero buf (mac68k-ser-out-pb) #x100)
      (m68k-emit-move-imm-an buf (conv-real +mac68k-save-core-word+) +68k-a0+)
      (m68k-emit-moveq buf +68k-d1+ 0)
      (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 0))
    ;; ---- both ----
    (m68k-emit-label buf common)
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
    ;; VN = NIL; A5 the globals base when it is one
    (m68k-emit-move-imm-an buf +nil-value+ +68k-a4+)
    (when *68k-a5-globals*
      (m68k-emit-move-imm-an buf (conv-real *68k-hosted-globals-base*) +68k-a5+))
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
