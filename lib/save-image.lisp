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
;;;; the arena itself -- is valid verbatim in the next process.  The GC bitmaps
;;;; come through the same trap, so they are inside the arena and need no
;;;; separate slices.  A process WITHOUT an arena (kernel refused the mapping)
;;;; can still save with the JIT off, and refuses to save with it on.
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
   +linux-aarch64-jit-arena-base+).  Arch builds without one override to 0."
  #x3000000000)

(defun %core-jit-arena-bump ()
  "Current bump pointer of the arena, or 0 when this process has none.  The
   word at 0x10000F58 is RAW (the trap reads it), so the :u64 load is halved."
  (* 2 (mem-ref #x10000F58 :u64)))

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
   collection counter at 0x10000060 moves."
  (let ((before (mem-ref #x10000060 :u64)))
    (loop
      (set-alloc-limit (get-alloc-ptr))
      (let ((probe (cons 1 2)))
        (when (> (mem-ref #x10000060 :u64) before)
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
  (when (and (%jit-enabled-p) (= (%core-jit-arena-bump) 0))
    (error "save-image: the JIT is on but this process has no fixed exec arena; (setq *use-jit* nil) before loading what you want baked"))
  (finish-output)
  (let* ((from (%gc-force-to-space-0))
         (free (get-alloc-ptr))
         (boff (floor (- from (%gc-bitmap-page-base)) 128))
         (blen (+ 1 (floor (- free from) 128)))
         (alo (%core-jit-arena-lo))
         (abump (%core-jit-arena-bump))
         (hdr *io-buf-addr*)
         (fd (%sys-open-wronly path)))
    (when (< fd 0)
      (error "save-image: cannot create ~A" path))
    ;; With an arena the bitmaps live inside it and travel with it.
    (when (> abump 0) (setq blen 0))
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
    (%core-write-all fd hdr 128)
    (%core-write-all fd #x10000000 4096)
    (%core-write-all fd from (- free from))
    (%core-write-all fd (+ (%gc-bitmap-base) boff) blen)
    (%core-write-all fd (+ (%gc-cons-bitmap-base) boff) blen)
    (when (> abump 0)
      (%core-write-all fd alo (- abump alo)))
    (%sys-close fd)
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
         (argv2 (* 2 (mem-ref (+ base 32) :u64)))     ; stub: STR x21,[x22,#32]
         (fd (%core-open-path-at argv2))
         (hdr (+ base 256))                           ; below from_start: never live
         (stage #x0FF00000))                          ; the io-buf BSS page
    (when (< fd 0) (%core-die "core: cannot open the core file"))
    (%core-slice fd hdr 128)
    (when (/= (mem-ref hdr :u64) (%core-magic))
      (%core-die "core: not a Modus core file"))
    (when (/= (mem-ref (+ hdr 8) :u64) from)
      (%core-die "core: heap base differs from this process (stub did not get its fixed mapping)"))
    (when (/= (mem-ref (+ hdr 24) :u64) (%gc-space-size))
      (%core-die "core: heap geometry differs from this image"))
    (let ((free (mem-ref (+ hdr 32) :u64))
          (blen (mem-ref (+ hdr 48) :u64))
          (boff (mem-ref (+ hdr 56) :u64))
          (alo (mem-ref (+ hdr 64) :u64))
          (abump (mem-ref (+ hdr 72) :u64)))
      (when (and (> abump 0)
                 (or (= (%core-jit-arena-bump) 0) (/= alo (%core-jit-arena-lo))))
        (%core-die "core: snapshot has JIT pages but this process has no matching exec arena"))
      ;; The metadata window, sequentially: the collector's fixed roots and
      ;; gc_count land in place; every other word is per-process and is read
      ;; to the staging page instead.  Offsets sum to 0x1000.
      (%core-slice fd stage #x60)
      (%core-slice fd #x10000060 8)       ; gc_count
      (%core-slice fd stage #x18)         ; 0x68..0x80: saved sp / regs
      (%core-slice fd #x10000080 16)      ; globals alist, symbol intern table
      (%core-slice fd stage #xB8)         ; 0x90..0x148: mv area, gc temps
      (%core-slice fd #x10000148 8)       ; keyword intern table
      (%core-slice fd stage #x20)         ; 0x150..0x170: nargs, handler frames
      (%core-slice fd #x10000170 8)       ; package-by-hash table
      (%core-slice fd stage #xD98)        ; 0x178..0xF10: argv, bitmap cfg, stats
      (%core-slice fd #x10000F10 8)       ; JIT constant-vector root
      (%core-slice fd stage #xE8)         ; 0xF18..0x1000
      (%core-slice fd from (- free from))
      (%core-slice fd (+ (%gc-bitmap-base) boff) blen)
      (%core-slice fd (+ (%gc-cons-bitmap-base) boff) blen)
      (when (> abump 0)
        ;; The arena: bitmaps + JIT pages, then the bump word (RAW: store the
        ;; halved value so the machine word is the address) and an I-cache
        ;; invalidate over the code we just read in.
        (%core-slice fd alo (- abump alo))
        (setf (mem-ref #x10000F58 :u64) (ash abump -1))
        (%jit-icache-flush alo (- abump alo)))
      (syscall3 3 fd 0 0)
      (set-alloc-ptr free)
      free)))

(defun %core-post-restore ()
  "Per-process state a snapshot cannot carry: signal handlers."
  (%init-signal-handling))
