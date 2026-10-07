;;;; lib/save-image.lisp -- SAVE-AND-DIE: heap snapshot and restore (hosted)
;;;;
;;;; SBCL bakes Quicklisp into its core with save-lisp-and-die; a Modus image
;;;; has no heap snapshot, so every boot re-compiles the quicklisp client from
;;;; source (the whole hour on the Pi Zero 2 W is that reload, not the
;;;; hardware).  This is the snapshot.
;;;;
;;;; MODEL.  After a full Cheney collection every live object sits in ONE
;;;; contiguous range [from_start, alloc-ptr) of the first semispace, and the
;;;; only pointers into that range from outside it are the collector's fixed
;;;; roots in the metadata window (globals alist, symbol / keyword / package
;;;; tables, the JIT constvec root) plus the object-start and cons-kind bitmap
;;;; bits that validate conservative roots.  So a snapshot is: header, the 4 KB
;;;; metadata window, the live range, and the two bitmap slices covering it.
;;;; Restore is the same reads in the same order, into the same addresses --
;;;; the boot stub maps the heap at a FIXED address (boot-linux-aarch64.lisp,
;;;; +linux-aarch64-fixed-heap-base+) precisely so no pointer needs relocating,
;;;; and EQ hash tables keyed on addresses stay valid.  The bitmaps are mapped
;;;; per process at whatever address the kernel picks, but they are indexed
;;;; relative to the heap base, so their BYTES are position-independent.
;;;;
;;;; WHAT IS PER-PROCESS and deliberately NOT restored: stack_base / saved_rsp
;;;; (a new stack), the native handler-frame triple (0x180..), nargs, the argv
;;;; copies, the bitmap config words (new mappings), signal handlers (sigaction
;;;; is per process -- %core-post-restore reinstalls).
;;;;
;;;; JIT PAGES travel too: the stub maps one fixed RWX arena and the
;;;; %mmap-exec-page trap bump-allocates from it (bump word 0x10000F58), so
;;;; JIT'd code -- absolute addresses into the fixed image, the fixed heap and
;;;; the arena itself -- is valid verbatim in the next process.  The bitmap
;;;; slices are always written too: on hosted aarch64 they happen to sit inside
;;;; the arena (redundant, harmless); on the Pi they are at their own fixed
;;;; addresses.  A process WITHOUT an arena (kernel refused the mapping) can
;;;; still save with the JIT off, and refuses to save with it on.
;;;;
;;;; BARE METAL has no file: the core is a RAM range (Pi: 0x18000000) that the
;;;; boot loader places next to the kernel, and the seams below become word
;;;; copies over a cursor -- see build-cl-repl-common.lisp.
;;;;
;;;; USE:
;;;;   modus --eval '(ql:quickload :sha1)' --eval '(save-and-die "ql.core")'
;;;;   modus --core ql.core --eval '(sha1:sha1-hex "abc")' --quit
;;;;
;;;; --core must be argv[1]: kernel-main tests it BEFORE init-symbol-table (the
;;;; snapshot carries every table that boot would build) and reads the path
;;;; through the raw argv[2] pointer the stub stored at heap-base+32, so the
;;;; restore path touches no global, no string and no stream -- nothing exists
;;;; yet.  Every value it holds is a fixnum in its own frame; the heap read
;;;; overwrites whatever the restore code itself allocated, which is nothing
;;;; it still needs.
;;;;
;;;; mem-ref :u64 convention (see gc.lisp): a load yields raw>>1, a store
;;;; writes value<<1.  Header words are written and read through the same
;;;; pair, so they round-trip; the window and heap bytes are moved by read(2)
;;;; and write(2) directly and never pass through a Lisp word.

(defun %core-magic () 20260905)

(defun %core-jit-arena-lo ()
  "Base of the fixed JIT exec arena (boot-linux-aarch64.lisp
   +linux-aarch64-jit-arena-base+, overridable per build as the hosted
   layout's :JIT-ARENA-BASE).  Arch builds without one override to 0."
  (%layout :jit-arena-base #x3000000000))

(defun %core-jit-bump-slot ()
  "Address of the arena's RAW bump word (the trap reads it).  Hosted aarch64:
   0x10000F58 (0x1000FF38 with threads: translate-aarch64 A64-GC-STAT-ADDR);
   the Pi overrides to its own (%jit-exec-bump)."
  (%layout-if :a64-threads (%conv-addr #x1000FF38) (%conv-addr #x10000F58)))

(defun %core-jit-arena-bump ()
  "Current bump pointer of the arena, or 0 when this process has none.  The
   bump word is RAW, so the :u64 load is halved."
  (* 2 (mem-ref (%core-jit-bump-slot) :u64)))

(defun %core-jit-lossy-p ()
  "True when a snapshot would silently drop live JIT code pages: the JIT is on
   and produced pages the core cannot carry.  Hosted aarch64 carries them in
   the fixed arena, so this is only true when the arena is absent (kernel
   refused the fixed mapping).  Arches whose JIT emits no pages (bare aarch64
   translates nothing) override this to NIL."
  (and (%jit-enabled-p) (= (%core-jit-arena-bump) 0)))

;;; ---- the I/O seams: a core is a FILE on Linux, a RAM range on bare metal --
;;; `fd' is opaque to the shared code: a Linux descriptor here, the address of
;;; a cursor word in the bare-metal overrides (build-cl-repl-common.lisp).

(defun %core-open-out (path)
  "Create the core sink for PATH; negative on failure."
  (%sys-open-wronly path))

(defun %core-open-in ()
  "Open the core source for restore: argv[2], read through the raw pointer
   the stub stored at heap-base+32 (STR x21,[x22,#32]) -- no string exists yet.
   Read as two :u32 halves, which load exactly.  It was (* 2 <:u64 load>), and
   a :u64 load yields the word shifted right by one, so doubling it dropped bit
   0: whenever argv[2] began at an ODD address the open got the byte before it
   -- the NUL ending \"--core\" -- i.e. an empty path, and the restore died
   with \"cannot open the core file\".  The address's parity follows the
   lengths of the other arguments, so it looked flaky and path-dependent."
  (let* ((slot (+ (- (%gc-from-start) 512) 32))
         (lo (mem-ref slot :u32))
         (hi (mem-ref (+ slot 4) :u32)))
    ;; HI is 0 on a 32-bit image, where 2^32 would be a bignum -- and nothing
    ;; may allocate yet.
    (%core-open-path-at (if (= hi 0) lo (+ lo (* 4294967296 hi))))))

(defun %core-close (fd)
  (syscall3 3 fd 0 0))

(defun %core-open-path-at (addr)
  "Open the NUL-terminated path at raw address ADDR for reading.  x86-64
   syscall numbering; the aarch64 image overrides this with openat (its ABI
   has no open)."
  (syscall3 2 addr 0 0))

(defun %core-read-all (fd addr len)
  "read(2) LEN bytes into ADDR, looping over short reads.  Returns bytes read."
  (let ((got 0))
    (loop
      (when (>= got len) (return got))
      (let ((n (syscall3 0 fd (+ addr got) (- len got))))
        (when (<= n 0) (return got))
        (setq got (+ got n))))))

(defun %core-write-all (fd addr len)
  "write(2) LEN bytes from ADDR, looping over short writes.  Returns bytes written."
  (let ((done 0))
    (loop
      (when (>= done len) (return done))
      (let ((n (syscall3 1 fd (+ addr done) (- len done))))
        (when (<= n 0) (return done))
        (setq done (+ done n))))))

(defun %gc-force ()
  "Run the collector NOW: lower the allocation limit to the allocation pointer
   so the next allocation trips its gc-check, then allocate.  Loops until the
   active region's collection count moves.

   %GC-EPOCH, NOT A :u64 READ OF THE COUNT WORD: x64 stores the count RAW, and
   a :u64 load hands the raw word back as a tagged VALUE -- an ODD count is an
   immediate, not a fixnum, so (> after before) was never true and this looped
   forcing collections forever (17 a second, measured on the UEFI image after
   five natural collections; it had worked by luck whenever the count was
   even).  The same trap mvm/gc.lisp documents for %GC-COUNT."
  (let ((before (%gc-epoch)))
    (loop
      (set-alloc-limit (get-alloc-ptr))
      (let ((probe (cons 1 2)))
        (when (> (%gc-epoch) before)
          (return (car probe)))))))

(defun %gc-force-to-space-0 ()
  "Collect until the live data sits in the FIRST semispace (from_start equals
   the bitmap page_base), the space a fresh boot stub sets the registers up
   for -- so a restored process needs no relocation and no limit change.
   Returns from_start."
  (%gc-force)
  (when (/= (%gc-from-start) (%gc-bitmap-page-base))
    (%gc-force))
  (%gc-from-start))

(defun %save-image (path)
  "Write a heap snapshot of this process to PATH.  Full GC first; then header,
   the 4 KB metadata window, the live heap range and the two bitmap slices.
   Returns the number of live heap bytes saved.  Refuses while the JIT is on
   (see the file header)."
  (when (%core-jit-lossy-p)
    (error "save-image: the JIT produced code pages this process cannot snapshot (no fixed exec arena); (setq *use-jit* nil) before loading what you want baked"))
  (finish-output)
  (%jit-before-save)
  (let* ((from (%gc-force-to-space-0))
         (free (get-alloc-ptr))
         (boff (floor (- from (%gc-bitmap-page-base)) 128))
         (blen (+ 1 (floor (- free from) 128)))
         (alo (%core-jit-arena-lo))
         (abump (%core-jit-arena-bump))
         (hdr *io-buf-addr*)
         (fd (%core-open-out path)))
    (when (< fd 0)
      (error "save-image: cannot create ~A" path))
    (setf (mem-ref hdr :u64) (%core-magic))
    (setf (mem-ref (+ hdr 8) :u64) from)
    (setf (mem-ref (+ hdr 16) :u64) (%gc-to-start))
    (setf (mem-ref (+ hdr 24) :u64) (%gc-space-size))
    (setf (mem-ref (+ hdr 32) :u64) free)
    (setf (mem-ref (+ hdr 40) :u64) 4096)
    (setf (mem-ref (+ hdr 48) :u64) blen)
    (setf (mem-ref (+ hdr 56) :u64) boff)
    (setf (mem-ref (+ hdr 64) :u64) alo)
    (setf (mem-ref (+ hdr 72) :u64) abump)
    ;; WHERE THE LAYOUT WAS: a restore at another slide relocates by the
    ;; difference (see RELOCATION below).  0 in a core from before this.
    (setf (mem-ref (+ hdr 80) :u64) (%conv-addr #x10000000))
    (setf (mem-ref (+ hdr 88) :u64) (%core-layout-lo))
    (setf (mem-ref (+ hdr 96) :u64) (%core-layout-hi))
    ;; THE ARENA'S DATA PAGES (mvm-eval.lisp %JIT-DATA-PAGE): [dlo, top), or 0.
    (setf (mem-ref (+ hdr 104) :u64) (%core-jit-data-lo))
    (%core-write-all fd hdr 128)
    (%core-write-all fd (%conv-addr #x10000000) 4096)
    (%core-write-all fd from (- free from))
    (%core-write-all fd (+ (%gc-bitmap-base) boff) blen)
    (%core-write-all fd (+ (%gc-cons-bitmap-base) boff) blen)
    (when (> abump 0)
      (%core-write-all fd alo (- abump alo))
      (let ((dlo (%core-jit-data-lo)))
        (when (> dlo 0)
          (%core-write-all fd dlo (- (%core-layout-hi) dlo)))))
    (%core-close fd)
    (- free from)))

(defun save-and-die (path)
  "Snapshot this process to PATH and exit.  Restart it with: modus --core PATH."
  (%save-image path)
  (sys-exit 0))

;;; ---- restore: runs in kernel-main before any boot init ---------------------

(defun %core-requested-p ()
  "True when argv[1] is --core.  The boot stub copies argv[1] to 0x10000208
   and argc to 0x10000200."
  (and (>= (mem-ref #x10000200 :u32) 3)
       (= (mem-ref #x10000208 :u8) 45)      ; -
       (= (mem-ref #x10000209 :u8) 45)      ; -
       (= (mem-ref #x1000020A :u8) 99)      ; c
       (= (mem-ref #x1000020B :u8) 111)     ; o
       (= (mem-ref #x1000020C :u8) 114)     ; r
       (= (mem-ref #x1000020D :u8) 101)     ; e
       (= (mem-ref #x1000020E :u8) 0)))

(defun %core-die (msg)
  (write-string-serial msg)
  (write-char-serial 10)
  (sys-exit 1))

(defun %core-slice (fd dst len)
  "Read exactly LEN bytes of the snapshot into DST, or die."
  (when (< (%core-read-all fd dst len) len)
    (%core-die "core: short read")))

(defun %restore-image ()
  "Read the snapshot named by argv[2] into this process.  See the file header
   for what is and is not restored.  Returns the restored allocation pointer."
  (let* ((from (%gc-from-start))
         (base (- from 512))                          ; heap-alloc-start
         (fd (%core-open-in))
         (hdr (+ base 256))                           ; below from_start: never live
         (stage (%conv-addr #x0FF00000)))             ; the io-buf BSS page
    (when (< fd 0) (%core-die "core: cannot open the core file"))
    (%core-slice fd hdr 128)
    (when (/= (mem-ref hdr :u64) (%core-magic))
      (%core-die "core: not a Modus core file"))
    (let* ((was (mem-ref (+ hdr 80) :u64))
           ;; THE SLIDE between the saving process and this one: every address
           ;; in the layout moved by D (they are one mapping on Darwin, ASLR-
           ;; slid as a whole).  0 for a core saved without the field.
           (d (if (= was 0) 0 (- (%conv-addr #x10000000) was)))
           (sfrom (mem-ref (+ hdr 8) :u64)))
    (when (/= (+ sfrom d) from)
      (%core-die "core: heap base differs from this process (the core was saved by an image with a different layout, or the stub did not get its fixed mapping)"))
    (when (/= (mem-ref (+ hdr 24) :u64) (%gc-space-size))
      (%core-die "core: heap geometry differs from this image"))
    (let ((free (mem-ref (+ hdr 32) :u64))
          (blen (mem-ref (+ hdr 48) :u64))
          (boff (mem-ref (+ hdr 56) :u64))
          (alo (mem-ref (+ hdr 64) :u64))
          (abump (mem-ref (+ hdr 72) :u64))
          (l0 (mem-ref (+ hdr 88) :u64))
          (l1 (mem-ref (+ hdr 96) :u64))
          (dlo (mem-ref (+ hdr 104) :u64)))
      (when (and (> abump 0)
                 (or (= (%core-jit-arena-bump) 0) (/= (+ alo d) (%core-jit-arena-lo))))
        (%core-die "core: snapshot has JIT pages but this process has no matching exec arena"))
      ;; The metadata window, sequentially: the collector's fixed roots and
      ;; gc_count land in place; every other word is per-process and is read
      ;; to the staging page instead.  Offsets sum to 0x1000.
      (%core-slice fd stage #x60)
      (%core-slice fd (%conv-addr #x10000060) 8)       ; gc_count
      (%core-slice fd stage #x18)         ; 0x68..0x80: saved sp / regs
      (%core-slice fd (%conv-addr #x10000080) 16)      ; globals alist, symbol intern table
      (%core-slice fd stage #xB8)         ; 0x90..0x148: mv area, gc temps
      (%core-slice fd (%conv-addr #x10000148) 8)       ; keyword intern table
      (%core-slice fd stage #x20)         ; 0x150..0x170: nargs, handler frames
      (%core-slice fd (%conv-addr #x10000170) 8)       ; package-by-hash table
      (%core-slice fd stage #xE28)        ; 0x178..0xFA0: argv, bitmap cfg, stats,
                                          ; the per-CPU region cells
      ;; The global-cell cache vector + its init guard.  In place, not staged:
      ;; the cells it points at live in the heap slice that follows and are
      ;; restored at the same addresses, so a restored image that dropped this
      ;; word would read every special through the SAVING process's pairs.
      (%core-slice fd (%conv-addr #x10000FA0) 16)      ; global-cell cache root + guard
      ;; The static-literal vector (docs/static-literals.md phase 2), in place:
      ;; it points into the heap slice.  This process's own word is 0 here
      ;; (restore runs before any quoted symbol is loaded) or, if not, points
      ;; at a vector the heap slice has just overwritten.
      (%core-slice fd (%conv-addr #x10000FB0) 8)       ; static-literal vector root
      (%core-slice fd stage #x18)         ; 0xFB8..0xFD0
      (%core-slice fd (%conv-addr #x10000FD0) 8)       ; JIT constant-vector root (aarch64)
      (%core-slice fd stage #x28)         ; 0xFD8..0x1000
      (%core-slice fd from (- free sfrom))
      (%core-slice fd (+ (%gc-bitmap-base) boff) blen)
      (%core-slice fd (+ (%gc-cons-bitmap-base) boff) blen)
      (when (> abump 0)
        ;; The arena: bitmaps + JIT pages, then the bump word (RAW: store the
        ;; halved value so the machine word is the address) and an I-cache
        ;; invalidate over the code we just read in.  JIT code is PC-relative
        ;; for the layout, so it needs no relocation.
        (%core-load-code fd (+ alo d) (- abump alo) stage)
        (setf (mem-ref (%core-jit-bump-slot) :u64) (ash (+ abump d) -1))
        (%jit-icache-flush (+ alo d) (- abump alo))
        ;; Its data pages (the linkage cells): ordinary writable memory.
        (when (> dlo 0)
          (%core-slice fd (+ dlo d) (- l1 dlo))))
      ;; Publish the alloc pointer BEFORE anything that allocates -- %core-close
      ;; prints CORE-END via print-dec, which conses; until this runs, the
      ;; pointer is still the fresh boot's (heap base), so that string would
      ;; land ON TOP of the just-restored data at the low heap and corrupt it.
      (set-alloc-ptr (+ free d))
      (unless (= d 0)
        (%core-relocate from (+ free d) l0 l1 d))
      (%core-close fd)
      (+ free d)))))

;;; ---- RELOCATION: restoring at another slide ---------------------------------
;;;
;;; iOS slides an app's whole image on every launch (and arm64 apps must be
;;; position-independent), so a core saved on one launch is restored at a
;;; different address on the next.  The image's own code and a PC-relative
;;; image's JIT pages are position-independent; what holds absolute addresses
;;; is DATA, and it all moved by the same D:
;;;   - every pointer-bearing heap word, walked object by object exactly as the
;;;     collector scans to-space (%GC-SCAN-COPIED: conses by the cons-kind
;;;     bitmap, headed objects by count, leaf payloads skipped);
;;;   - the metadata window's restored roots;
;;;   - the JIT's linkage cells, which hold native entry addresses;
;;;   - *CONV-DELTA*, which is a difference, not an address.
;;; A word is relocated when it is a TAGGED POINTER into the old layout, or a
;;; FIXNUM whose value is an old layout address -- the same by-value rule the
;;; compiler applies to literals (compiler.lisp PCREL-LAYOUT-ADDR-P), sound
;;; for the same reason: a PC-relative layout lies wholly above 4 GB, where
;;; ordinary numbers do not reach.  Nothing here allocates: the words are read
;;; as 32-bit halves and only those below 2^40 are formed, as fixnums.
;;; Address-keyed hashing does not exist (%HT-HASH buckets only strings,
;;; fixnums, characters and symbols), so moved objects stay found.

(defun %core-jit-data-lo ()
  "Low end of the JIT arena's data pages (mvm-eval.lisp %JIT-DATA-PAGE), or 0."
  (let ((d *jit-data-next*)) (if (integerp d) d 0)))

(defun %core-load-code (fd dst len stage)
  "Read LEN bytes of JIT code from the core to DST, a chunk at a time through
   STAGE, writing a chunk only where it differs from what DST already holds.
   An iOS app carries a snapshot's code as SIGNED, READ-ONLY pages at exactly
   this address (host/macos/image-segments.sh), and they hold these bytes
   already, so nothing is written; anywhere else the arena is fresh and every
   chunk is copied in.  STAGE is a 4 KB page."
  (let ((off 0))
    (loop
      (when (>= off len) (return nil))
      (let ((n (if (> (- len off) 4096) 4096 (- len off))))
        (%core-slice fd stage n)
        (unless (%core-same-p stage (+ dst off) n)
          (%core-copy stage (+ dst off) n))
        (setq off (+ off n))))))

(defun %core-same-p (a b n)
  (let ((i 0))
    (loop
      (when (>= i n) (return t))
      (unless (and (= (mem-ref (+ a i) :u32) (mem-ref (+ b i) :u32))
                   (= (mem-ref (+ a (+ i 4)) :u32) (mem-ref (+ b (+ i 4)) :u32)))
        (return nil))
      (setq i (+ i 8)))))

(defun %core-copy (src dst n)
  (let ((i 0))
    (loop
      (when (>= i n) (return nil))
      (setf (mem-ref (+ dst i) :u32) (mem-ref (+ src i) :u32))
      (setq i (+ i 4)))))

(defun %core-layout-lo ()
  "Low end of this process's layout: the code base, less the Darwin syscall
   slot's page below it."
  (- (%layout :code-base 0) #x4000))

(defun %core-layout-hi ()
  "High end of this process's layout: the end of the JIT arena."
  (+ (%core-jit-arena-lo) #x20000000))

(defun %core-set-word (at w)
  (setf (mem-ref at :u32) (logand w #xFFFFFFFF))
  (setf (mem-ref (+ at 4) :u32) (ash w -32)))

(defun %core-reloc-word (at l0 l1 d)
  "Relocate the machine word at AT by D when it addresses the old layout."
  (let ((lo (mem-ref at :u32)) (hi (mem-ref (+ at 4) :u32)))
    (when (< hi 256)
      (let ((w (+ (* hi 4294967296) lo)))
        (if (= (logand lo 5) 1)
            (let ((a (- w (logand lo 15))))
              (when (and (>= a l0) (< a l1)) (%core-set-word at (+ w d))))
            (when (= (logand lo 1) 0)
              (let ((v (ash w -1)))
                (when (and (>= v l0) (< v l1)) (%core-set-word at (+ w (* 2 d))))))))))
  nil)

(defun %core-object-bytes (subtag count)
  "An object's size, as the native collector's walk computes it (translate-
   aarch64 EMIT-AARCH64-OBJECT-WALK): a byte vector counts bytes, a single-
   float vector 4-byte lanes, everything else words; header and padding word
   first, rounded to 16."
  (logand (+ 15 (+ 16 (cond ((= subtag #x11) count)
                            ((= subtag #x12) (* count 4))
                            (t (* count 8)))))
          (lognot 15)))

(defun %core-leaf-p (subtag)
  "A raw payload, as the native walk's leaf set: never relocated."
  (or (= subtag #x10) (= subtag #x11) (= subtag #x12) (= subtag #x14)
      (= subtag #x16) (= subtag #x30) (= subtag #x31) (= subtag #x60)
      (= subtag #x64) (= subtag #x65) (= subtag #x66)))

(defun %core-reloc-heap (from free l0 l1 d)
  "Relocate every pointer-bearing word of the heap [FROM, FREE), walked object
   by object exactly as the native collector walks to-space."
  (let ((scan from))
    (loop
      (when (>= scan free) (return nil))
      (if (%gc-is-cons-granule scan)
          (progn (%core-reloc-word scan l0 l1 d)
                 (%core-reloc-word (+ scan 8) l0 l1 d)
                 (setq scan (+ scan 16)))
          (let* ((hdr-lo (%gc-word-lo scan))
                 (count (%gc-header-count hdr-lo (%gc-word-hi scan)))
                 (subtag (logand hdr-lo #xFF)))
            (unless (%core-leaf-p subtag)
              (let ((i 0))
                (loop
                  (when (>= i count) (return nil))
                  (%core-reloc-word (+ scan (+ 16 (* i 8))) l0 l1 d)
                  (setq i (+ i 1)))))
            (setq scan (+ scan (%core-object-bytes subtag count))))))))

(defun %core-relocate (from free l0 l1 d)
  "Relocate a just-restored snapshot by D (see RELOCATION above)."
  (%core-reloc-heap from free l0 l1 d)
  (dolist (off (list #x80 #x88 #x148 #x170 #xFA0 #xFB0 #xFD0))
    (%core-reloc-word (+ (%conv-addr #x10000000) off) l0 l1 d))
  (setq *conv-delta* (+ *conv-delta* d))
  (setq *layout-slide* (+ (%layout-slide) d))
  (%jit-after-relocate l0 l1 d))

(defun %core-post-restore ()
  "Per-process state a snapshot cannot carry: signal handlers, and whether
   this process may compile to native code.  An iOS app cannot write code at
   all -- its snapshot's JIT pages are signed and read-only -- and says so
   with MODUS_NO_RUNTIME_JIT (host/macos/modus-shim.c); code compiled there
   after the restore is interpreted."
  (%init-signal-handling)
  (let ((v (%cli-getenv "MODUS_NO_RUNTIME_JIT")))
    (when (and v (> (length v) 0) (not (string= v "0")))
      (setq *use-jit* nil))))
