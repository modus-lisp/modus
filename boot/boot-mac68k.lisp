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
          ((string-equal m "classic2m") :classic-2m)
          ((string-equal m "plus1m") :plus-1m)
          ((string-equal m "plus1mjit") :plus-1m-jit)
          ((string-equal m "classicos") :classic-os)
          ((string-equal m "quadra") :quadra)
          (t (error "MODUS_MAC68K_MACHINE=~A: lc, classic, classicos, quadra, classic2m, plus1m or plus1mjit" m))))
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
              :boot-stack-top #x01A00000)
    ;; THE CLASSIC (4 MB) LAUNCHED FROM MAC OS (MODUS_MAC68K_MACHINE=classicos,
    ;; scripts/mac68k-chain.py): the :CLASSIC map with nothing under the image.
    ;; Mac OS keeps its low memory and system heap -- measured on MAME's Plus
    ;; and System 6.0.8 with no INITs, the system heap #x1400-#x24270, the
    ;; application zone from #x2427C -- and the image runs on top of them as it
    ;; runs on top of the ROM's.  So the stack (64 KB) and the I/O scratch pages
    ;; move from #x10000 to under NIL's page, out of the code cache's top; the
    ;; loader application checks the system heap ends below the image.
    ;; The runtime threaded JIT's code area (mvm/jit-68k.lisp; MODUS_68K_JIT=1)
    ;; is the top of the old static range, #x240000-#x2B0000 (448 KB): the
    ;; saved heap ends near #x120000.
    (:classic-os :load #x00040000 :static-lo #x00110000 :static-hi #x00240000
              :jit-area-lo #x00240000 :jit-area-hi #x002B0000 :jit-max-bytecode 2048
              :region #x002B0000 :heap-size #x00080000 :gc-guard #x00008000
              :kind-map #x00318000 :cache-lo #x00329000 :cache-hi #x003D0000
              :staging #x003D0000 :staging-size #x00000000
              :nil-page #x003F0000
              :stack-top #x003E0000 :scratch #x003E0000 :io-buf #x003E1000
              :boot-heap #x01000000 :boot-heap-size #x00800000
              :boot-cache-lo #x01A00000 :boot-cache-hi #x01E00000
              :boot-stack-top #x01A00000)
;; A BIG MAC LAUNCHED FROM MAC OS (MODUS_MAC68K_MACHINE=quadra): :CLASSIC-OS's
    ;; shape for a 32 MB Quadra 950 (MAME -ramsize 32M), every region big enough
    ;; that nothing pages or thrashes: the all-native image (~9 MB of 68000
    ;; code, MODUS_68K_THREADED=0 MODUS_68K_PAGED=0) from 6 MB -- a 32 MB
    ;; Quadra's ROM boots with its stack near #x400A00, where a load at 4 MB
    ;; read over it (Sad Mac 0000000F 00000001) -- a 4 MB static
    ;; heap, a 2 MB JIT area, two 2 MB semispaces; nothing above #x1B00000 (Mac
    ;; OS keeps the top 5 MB).  The place to meet MCL before memory is cut back.
    ;; THE CORE RUN must fit in 32 MB too (Basilisk II faults above that), so
    ;; its boot heap is the static heap's and the JIT area's 6 MB, which the run
    ;; does not use; its kind bytes (kind map + (address - region) / 8) then
    ;; fall in the young pair's range, unused in that run, and its stack under
    ;; NIL's page above the kind map.
    ;; THE YOUNG HEAP IS LAST, so a Mac OS application can grow it into its
    ;; partition: the code cache, the stack, the I/O pages and NIL's page sit
    ;; below the region, and the kind map after the heap.  A disk boot uses
    ;; :HEAP-SIZE and :KIND-MAP as they stand; the application's loader
    ;; (scripts/mac68k-chain.s) sizes the semispaces from the partition, puts
    ;; the kind map past them -- moving every kind-map bias by the difference
    ;; (the relocation table's x7/8 class) -- and leaves both in the low block
    ;; (+MAC68K-HEAP-SLOT+), where EMIT-MAC68K-ENTRY takes them.
    (:quadra  :load #x00600000 :static-lo #x00F00000 :static-hi #x01300000
              :jit-area-lo #x01300000 :jit-area-hi #x01500000 :jit-max-bytecode 16384
              :cache-lo #x01500000 :cache-hi #x01520000
              :staging #x01520000 :staging-size #x00000000
              :stack-top #x01550000 :scratch #x01550000 :io-buf #x01551000
              :nil-page #x01560000
              :region #x01570000 :heap-size #x00420000 :gc-guard #x00010000
              :kind-map #x01980000
              :boot-heap #x00F00000 :boot-heap-size #x00600000
              :boot-cache-lo #x01A10000 :boot-cache-hi #x01B00000
              :boot-stack-top #x019F0000)
    ;; THE CLASSIC WITH 2 MB (MODUS_MAC68K_MACHINE=classic2m): the screen at
    ;; #x1FA700, so everything packs below it.  The image from #x40000 as on
    ;; the 4 MB map; the static heap from the image's init-only text
    ;; (mac68k-core.py) up to #x178000; then the low block, a YOUNG pair of
    ;; 100 KB (56 KB of low block + 2 x 100 KB) and a 16 KB
    ;; guard, the kind map over them, a 196 KB code cache (the interpreter's
    ;; stream, ~176 KB, is the floor), NIL's page under the screen.  The boot
    ;; blocks relocate themselves to #x1EA700, inside the cache: they are done
    ;; before the stub zeroes it.  The same :BOOT- values: the core is still
    ;; made on a big Mac.
    (:classic-2m :load #x00040000 :static-lo #x000D5000 :static-hi #x00178000
              ;; the low block's 56 KB, then two 100 KB semispaces: the region
              ;; ends (base + heap-size - alloc-start + guard) at the kind map
              :region #x00178000 :heap-size #x0004E000 :gc-guard #x00004000
              :alloc-start #x0000E000
              :kind-map #x001BC000 :cache-lo #x001C5000 :cache-hi #x001F6000
              :staging #x001F6000 :staging-size #x00000000
              :nil-page #x001F6000
              :stack-top #x00020000 :scratch #x00020000 :io-buf #x00021000
              :boot-heap #x01000000 :boot-heap-size #x00800000
              :boot-cache-lo #x01A00000 :boot-cache-hi #x01E00000
              :boot-stack-top #x01A00000)
    ;; A MAC PLUS (or 512Ke / SE / Classic) WITH 1 MB (MODUS_MAC68K_MACHINE=plus1m):
    ;; the screen at #xFA700.  Below the image, [#x10000, #x40000), which no ROM
    ;; uses once our boot blocks run: the stack (32 KB), the I/O scratch pages,
    ;; the code cache (136 KB), and NIL's page and T's (#x3E000: in the core
    ;; run, on an LC, the ROM's system heap ends at #x39FE0).  From #x40000: the
    ;; image's resident part (~340 KB), the static heap the core brings, from the
    ;; image's init-only marker up to #xD1000 (measured: it ends at #xCFBB8);
    ;; then the low block (56 KB), a YOUNG pair of 40 KB and an 8 KB guard,
    ;; ending at the kind map (#xF5000, a byte per 8 bytes: 18 KB), which ends
    ;; under the screen.  Built with NO hot natives (an empty MODUS_68K_HOT):
    ;; their 25 KB of image is 12 KB more of each semispace.  Measured: a pair
    ;; of 20 KB exhausts before the REPL prompt, 32 KB at the first DEFUN, 40 KB
    ;; at the first DEFMACRO.  The init-only
    ;; text is LINKED at 4 MB (:INIT-BASE, cross.lisp *POOL-INIT-BASE*): in the
    ;; core run, on a big Mac, the region here would sit on it -- and that run's
    ;; kind bytes, for its heap at 16 MB, reach #x3D9000.  The boot blocks relocate themselves to
    ;; #xEA700, past the end of what they read (the low block ends #xE4000).
    (:plus-1m :load #x00040000 :static-lo #x0008E000 :static-hi #x000D1000
              :region #x000D1000 :heap-size #x00030000 :gc-guard #x00002000
              :alloc-start #x0000E000
              :kind-map #x000F5000 :cache-lo #x0001C000 :cache-hi #x0003E000
              :staging #x0003E000 :staging-size #x00000000
              :nil-page #x0003E000
              :stack-top #x00018000 :scratch #x00018000 :io-buf #x00019000
              :init-base #x00400000
              :boot-heap #x01000000 :boot-heap-size #x00800000
              :boot-cache-lo #x01A00000 :boot-cache-hi #x01E00000
              :boot-stack-top #x01A00000)
    ;; THE SAME 1 MB MAC WITH THE RUNTIME THREADED JIT (MODUS_MAC68K_MACHINE=plus1mjit,
    ;; MODUS_68K_JIT=1).  The JIT's table and handlers make the resident image
    ;; ~12 KB bigger than plus1m has room for, so it loads 16 KB lower, NIL's and
    ;; T's pages under it at #x3A000 -- no lower: in the core run, on an LC, the
    ;; ROM's system heap ends at #x39FE0.  The code area is the cache's top 16 KB,
    ;; free in a session (the core run caches at :BOOT-CACHE-LO), so the cache is
    ;; 104 KB, not 136: measured on plus1m with 92 KB, a DEFUN ~35% slower, calls
    ;; the same.  Modules capped at 1 KB of bytecode for the 40 KB semispaces.
    ;; A translation pages the compiler and the JIT through that cache (3-5
    ;; minutes a DEFUN, measured), so DEFUN does not translate: (compile 'f) does.
    (:plus-1m-jit :load #x0003C000 :static-lo #x0008E000 :static-hi #x000D1000
              :region #x000D1000 :heap-size #x00030000 :gc-guard #x00002000
              :alloc-start #x0000E000
              :kind-map #x000F5000 :cache-lo #x0001C000 :cache-hi #x00036000
              :jit-area-lo #x00036000 :jit-area-hi #x0003A000 :jit-max-bytecode 1024
              :jit-on-defun nil
              :staging #x0003A000 :staging-size #x00000000
              :nil-page #x0003A000
              :stack-top #x00018000 :scratch #x00018000 :io-buf #x00019000
              :init-base #x00400000
              :boot-heap #x01000000 :boot-heap-size #x00800000
              :boot-cache-lo #x01A00000 :boot-cache-hi #x01E00000
              :boot-stack-top #x01A00000)))

(defparameter *mac68k-shift*
  (let ((s (sb-ext:posix-getenv "MODUS_MAC68K_SHIFT")))
    (if (and s (plusp (length s))) (parse-integer s :radix 16) 0))
  "MODUS_MAC68K_SHIFT=HEX: every address of the layout moved by that much (a
   size is not an address).  Two builds a shift apart, diffed, are the image's
   relocations: what a Mac OS application needs to run wherever the Process
   Manager puts its partition.")

(defparameter +mac68k-layout-sizes+
  '(:heap-size :boot-heap-size :gc-guard :staging-size :alloc-start
    :jit-max-bytecode :jit-on-defun))

(defun mac68k-layout (key)
  (let ((v (or (getf (cdr (assoc *mac68k-machine* *mac68k-layouts*)) key)
               (error "mac68k: no ~S in the ~S layout" key *mac68k-machine*))))
    (when (and (integerp v) (not (member key +mac68k-layout-sizes+)))
      (setq v (+ v *mac68k-shift*)))
    ;; MODUS_MAC68K_CACHE_KB=N: a code cache of N KB, at the top of the
    ;; layout's -- the test of eviction under pressure (and the size a smaller
    ;; machine will have)
    (let ((kb (and (eq key :cache-lo) (sb-ext:posix-getenv "MODUS_MAC68K_CACHE_KB"))))
      (if (and kb (plusp (length kb)))
          (max v (logand (- (mac68k-layout :cache-hi) (* 1024 (parse-integer kb))) (lognot 4095)))
          v))))

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

(defun mac68k-alloc-start ()
  "The low block's size: where the young heap starts in the runtime-data region
   (the layout's :ALLOC-START; boot-linux-68k's #x20000 by default).  The static
   heap's crossing table is its last 8 KB."
  (or (getf (cdr (assoc *mac68k-machine* *mac68k-layouts*)) :alloc-start)
      +linux-68k-heap-alloc-start+))

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
        *68k-gc-kind-map* +mac68k-kind-map+
        *68k-gc-heap-start* (+ +conv-region-base+ (mac68k-alloc-start))
        *68k-card-crossing* (+ +conv-region-base+ (mac68k-alloc-start) -8192)
        ;; the init-only text's link address, where the layout gives one
        *pool-init-base* (getf (cdr (assoc *mac68k-machine* *mac68k-layouts*)) :init-base))
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
    ;; the kind bytes of [LO, LO + 2*HALF + GUARD) -- and of the low block when
    ;; LO is above it.  A heap below the region (the core run's boot heap)
    ;; has its bytes BELOW the kind map; measured as a count gone negative
    ;; once :QUADRA's region moved above that heap, zeroing past 32 MB
    (let* ((sh (- (m68k-kind-shift)))
           (from (ash (- (min lo base) base) sh))
           (to (ash (- (+ lo (* 2 half) guard) base) sh))
           (from4 (logand from (lognot 3))))
      (emit-mac68k-zero buf (+ +mac68k-kind-map+ from4)
                        (logand (+ (- to from4) 3) (lognot 3))))
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

;; #x10006058 / #x1000605C: written by an application's loader
;; (scripts/mac68k-chain.s), zero otherwise
(defconstant +mac68k-heap-slot+ #x10006058
  "Low-block words the Mac OS application's loader fills: +0 a semispace's
   size, chosen from the partition; +4 where the kind map then is.  Zero (a
   disk boot, the core run): the layout's :HEAP-SIZE and :KIND-MAP.")

(defun emit-mac68k-heap-from-slot (buf base lo guard)
  "When +MAC68K-HEAP-SLOT+ is set: zero the kind map it names over [BASE, LO +
   2*HALF + GUARD), write the Cheney block for two HALF-byte semispaces from LO
   and set VA / VL, then branch past the code that follows (the layout's own
   heap, EMIT-MAC68K-HEAP).  Clobbers D0-D2/A0."
  (let ((skip (mvm-make-label)) (done (mvm-make-label)) (zero (mvm-make-label))
        (n (+ (m68k-kind-shift) 2)))
    (m68k-emit-move-imm-an buf (conv-real +mac68k-heap-slot+) +68k-a0+)
    (m68k-emit-move-disp-dn buf +68k-a0+ 0 +68k-d1+)      ; D1 = HALF
    (m68k-emit-tst buf +68k-d1+)
    (m68k-emit-beq buf skip)
    ;; the kind map: (LO - BASE + 2*HALF + GUARD) >> shift bytes, in longs
    (m68k-emit-word buf #x2001)                           ; MOVE.L D1,D0
    (m68k-emit-word buf #xD080)                           ; ADD.L D0,D0
    (m68k-emit-word buf #x0680) (m68k-emit-long buf (+ (- lo base) guard)) ; ADDI.L
    (m68k-emit-word buf (logior #xE088 (ash (logand n 7) 9)))  ; LSR.L #n,D0
    (m68k-emit-word buf #x2068) (m68k-emit-word buf 4)    ; MOVEA.L 4(A0),A0
    (m68k-emit-label buf zero)
    (m68k-emit-word buf #x4298)                           ; CLR.L (A0)+
    (m68k-emit-subq buf +68k-d0+ 1)
    (m68k-emit-bne buf zero)
    ;; VL's margin: min(1 MB, HALF/4 rounded down to 16)
    (m68k-emit-word buf #x2001)                           ; MOVE.L D1,D0
    (m68k-emit-word buf #xE488)                           ; LSR.L #2,D0
    (m68k-emit-word buf #x0280) (m68k-emit-long buf #xFFFFFFF0) ; ANDI.L #-16,D0
    (m68k-emit-word buf #x0C80) (m68k-emit-long buf +linux-68k-gc-margin-max+) ; CMPI.L
    (m68k-emit-word buf #x6306)                           ; BLS.S +6
    (m68k-emit-word buf #x203C) (m68k-emit-long buf +linux-68k-gc-margin-max+) ; MOVE.L #,D0
    (m68k-emit-move-imm-an buf lo +68k-a2+)               ; VA
    (m68k-emit-word buf #x243C) (m68k-emit-long buf lo)   ; MOVE.L #LO,D2
    (m68k-emit-word buf #xD481)                           ; ADD.L D1,D2
    (m68k-emit-word buf #x9480)                           ; SUB.L D0,D2
    (m68k-emit-word buf #x2642)                           ; MOVEA.L D2,A3: VL
    (m68k-emit-move-imm-an buf (+ base #x40) +68k-a0+)
    (m68k-emit-word buf #x243C) (m68k-emit-long buf lo)   ; MOVE.L #LO,D2
    (m68k-emit-move-dn-disp buf +68k-d2+ +68k-a0+ #x00)   ; from_start
    (m68k-emit-word buf #xD481)                           ; ADD.L D1,D2
    (m68k-emit-move-dn-disp buf +68k-d2+ +68k-a0+ #x08)   ; to_start
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ #x10)   ; space_size
    (m68k-emit-word buf #x240F)                           ; MOVE.L A7,D2
    (m68k-emit-move-dn-disp buf +68k-d2+ +68k-a0+ #x18)   ; stack_base
    (m68k-emit-bra buf done)
    (m68k-emit-label buf skip)
    done))

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
         (classic (member *mac68k-machine* '(:classic :classic-os :quadra :classic-2m :plus-1m :plus-1m-jit)))
         (over (mvm-make-label)) (ain (mvm-make-label)) (aout (mvm-make-label))
         (desc (mvm-make-label)) (core (mvm-make-label)) (common (mvm-make-label))
         (fill (mvm-make-label)) (hosted (mvm-make-label))
         (base (conv-real #x10000000))
         (size +mac68k-heap-size+)
         (mid (floor size 2))
         (alloc (mac68k-alloc-start)))
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
    (emit-mac68k-zero buf base alloc)            ; the low block, as a fresh mmap
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
      (let ((done (emit-mac68k-heap-from-slot buf base (+ base alloc) +mac68k-gc-guard+)))
        (emit-mac68k-heap buf base (+ base alloc) (- mid alloc) +mac68k-gc-guard+)
        (m68k-emit-label buf done))
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
    ;; a Mac OS application (+68K-MAC-HOST+ set by scripts/mac68k-chain.s):
    ;; the events, the cursor and the console are the application's -- none
    ;; of the bare Mac's takeover below
    (let ((h (conv-real +68k-mac-host+)))
      (m68k-emit-word buf #x4AB9) (m68k-emit-word buf (ash h -16)) (m68k-emit-word buf (logand h #xFFFF))
      (m68k-emit-bne buf hosted))
    (m68k-emit-moveq buf +68k-d0+ 20)
    (m68k-emit-word buf #xA06D)                          ; _InitEvents
    ;; no arrow cursor over a text console: the ROM drew one at boot, and on a
    ;; Quadra its cursor machinery (DAFB's cursor-scanline interrupt) and our
    ;; console's writes to the screen together ended in an unclaimed slot
    ;; interrupt -- _SysError with no System, recursing until the stack was
    ;; gone (MAME's Quadra 950).  The Mac OS launcher hides it too.
    (m68k-emit-word buf #xA852)                          ; _HideCursor
    ;; the console: .AIn, then .AOut -- and the port is not given up yet
    ;; (translate-68k +68K-MAC-CONSOLE-CODE+; a saved core may carry it set)
    (m68k-emit-move-imm-an buf (conv-real #x1000910C) +68k-a0+)
    (m68k-emit-word buf #x4210)                          ; CLR.B (A0)
    (emit-mac68k-open buf (mac68k-ser-in-pb) ain)
    (emit-mac68k-open buf (mac68k-ser-out-pb) aout)
    (m68k-emit-label buf hosted)
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
