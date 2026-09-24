;;;; build-riscv64-cli.lisp — the hosted Modus CLI for Linux/RISC-V 64.
;;;;
;;;; A :linux-riscv ELF you run as an ordinary process (under qemu-riscv64-static
;;;; here, and unchanged on a VisionFive / StarFive / SiFive board).  Same
;;;; SBCL-faithful toplevel as x64's ./modus: full argv, --eval/--load/--script/
;;;; --quit/--version, ~/.modusrc, and the REPL.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-riscv64-cli.lisp
;;;;   -> /tmp/modus-riscv64-cli   (override with MODUS_RISCV64_OUT / MODUS_CLI_OUT)
;;;;   qemu-riscv64-static /tmp/modus-riscv64-cli --eval '(print (+ 1 2))' --quit
;;;;
;;;; STRUCTURE.  This is a thin RISC-V tail over mvm/build-cli-common.lisp, the
;;;; same shared assembly build-generic-cli.lisp (x64), build-aarch64-cli.lisp and
;;;; build-i386-cli.lisp use.  Per that file's contract, what may live HERE is
;;;; RISC-V hardware/ABI FACT and nothing else; a CAPABILITY belongs in the shared
;;;; file, where every arch gets it at once.
;;;;
;;;; WHAT MADE THIS PORT POSSIBLE, since it is worth knowing which prerequisites
;;;; were real: an opcode census over the real image said it needs EIGHT opcodes
;;;; this back end lacked (:fn-addr at 5299 uses, :li-const at 44, and the six
;;;; doubles) and NOT the six single-float f32 ops, which i386 also lacks while
;;;; shipping.  A trap census then said it needs FIVE of the 23 #x05xx traps.  All
;;;; thirteen are in; #x0530 (mmap-exec-page, 393 SITES) is not, and needs not to
;;;; be — that is the JIT seam, and *JIT-ON* is forced off here exactly as on i386.

;;; ============================================================
;;; ARCH SLOTS — Linux/RISC-V 64
;;; ============================================================

(defvar *cli-arch* :riscv64)

;;; ABI FACT: RISC-V uses the asm-generic table, so these are AArch64's numbers
;;; and NOT x86's — 93 is `exit' (the calling THREAD only) and 94 is `exit_group'
;;; (the process).  Both x86-64 (60 vs 231) and AArch64 (93 vs 94) shipped the
;;; wrong one here for a while: indistinguishable in a single-threaded image, and
;;; a permanent HANG once a second thread exists, because the leader becomes an
;;; unreapable zombie with a parked sibling nobody wakes.  This image is
;;; single-threaded (no hosted-actors layer on this arch), so it cannot bite yet —
;;; which is exactly why it is written correctly now rather than later.
;;;
;;; The `(let ((c code)) ...)' rebind is kept identical in shape to the other three
;;; arches, so a codegen difference in the operand shuffle shows up as a difference
;;; in the TRANSLATOR rather than in the source.
(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 94 c 0 0)))
(defun halt ()
  (syscall3 94 1 0 0))
")

;;; Probe slot: argc/argv and the handler-frame introspection the shared toplevel
;;; and the x64/i386 probe programs read.  boot/boot-linux-riscv.lisp publishes
;;; argc at heap+0x200 and translate-riscv.lisp puts the jmpbuf at 0x10000180 and
;;; the handler-stack depth at 0x10000400 — the SAME addresses as the other ports,
;;; so this block is shared verbatim rather than re-derived.
;;;
;;; ARGV IS READ OFF THE LIVE STACK, not a staged copy.  i386 must stage, because
;;; its kernel stack sits above the 2^30 ceiling a tagged 30-bit-fixnum mem-ref
;;; address can express; a 62-bit fixnum has no such problem, so this port does
;;; what x64 and AArch64 do and walks the real stack through %gc-stack-base, which
;;; the boot stub published at 0x10000058.
(defvar *cli-arch-probe-source* "
(defun %argv-string-at (addr)
  (let ((len 0))
    (let ((i 0))
      (loop
        (let ((b (mem-ref (+ addr i) :u8)))
          (when (= b 0) (return nil))
          (setq i (+ i 1)))
        (setq len i)))
    (if (zerop len) nil
        (let ((s (%make-string-array len)) (i 0))
          (loop
            (when (>= i len) (return s))
            (aset s i (mem-ref (+ addr i) :u8))
            (setq i (+ i 1)))))))
(defun %argc  () (mem-ref #x10000200 :u32))
(defun %hc-depth () (mem-ref #x10000400 :u64))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u64) 0) nil t))
")

;;; No pre-init hardware setup, and one thing that must NOT be done.
;;;
;;; The other 64-bit ports zero a list of runtime-metadata BSS slots here, because
;;; a Linux kernel does not reliably zero a ~900 MB BSS tail and garbage in those
;;; words corrupts the global alist and the handler frames.  THIS PORT NEEDS NONE
;;; OF THAT: boot-linux-riscv.lisp gets its whole low block from a MAP_ANONYMOUS
;;; mmap, which the kernel guarantees zero-filled.
;;;
;;; %GC-BITMAP-INIT IS DELIBERATELY NOT CALLED.  There is no native RISC-V
;;; collector and therefore no object-start or cons-kind bitmap to reserve; the
;;; call would allocate through trap #x0531, which this back end does not
;;; implement.  aarch64 calls it because it HAS a native collector.
(defvar *cli-arch-kernel-prologue* "")

;;; MEMORY-MAP FACT, and the one place this port cannot copy x64.
;;;
;;; x64 and AArch64 leave this slot EMPTY, taking cl-fileio.lisp's defaults of
;;; #x1DF00000 / #x1DE00000 — addresses that work there because those images'
;;; single PT_LOAD carries a ~900 MB demand-zeroed BSS tail covering them.  This
;;; port asks for NO bss: every byte it owns comes from the mmap at #x10000000, so
;;; #x1DF00000 is simply unmapped and the first %string-to-cstr would fault.
;;;
;;; The window used instead is #x10001000..#x10002000, which exists because
;;; +LINUX-RISCV-HEAP-ALLOC-START+ starts the bump allocator at heap+0x2000 rather
;;; than heap+0x200: below it sit the fixed runtime slots (metadata #x40, globals
;;; #x80, MV #x90..#x138, argc #x200, handler frames #x400..#xC0F, the convention
;;; slots at #xA00) and then this 4 KB of scratch.  cstr gets the low half and the
;;; I/O page the high half — one 4 KiB page is exactly what cl-fileio reads and
;;; writes in.
;;;
;;; *SCRATCH-MMAPPED* is forced T because the region is ALREADY mapped, and for a
;;; second reason worth naming: %ensure-scratch-mmapped issues `(syscall3 9 ...)',
;;; which is mmap on x86-64 and IOCTL on the asm-generic table.  A bogus ioctl on
;;; two garbage pointers is harmless today and is not something to leave armed.
(defvar *cli-arch-io-scratch-source* "  (setq *cstr-scratch* #x10001000)
  (setq *io-buf-addr*  #x10001800)
  (setq *scratch-mmapped* t)
")

;;; Toplevel entry: the shared SBCL-faithful CLI, with no baked probe program.
;;; This is a shipping image, and per CLAUDE.md's build taxonomy a shipping image
;;; bakes no test corpus.
(defvar *cli-arch-kernel-epilogue*
"  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
")

;;; ARCH SLOT — late last-defun-wins overrides: the asm-generic syscall numbers.
;;;
;;; mvm/cl-fileio.lisp hardcodes the x86-64 table (open=2 close=3 read=0 write=1
;;; lseek=8 stat=4 fstat=5 unlink=87 rename=82 mkdir=83 getpid=39
;;; getdents64=217).  RISC-V shares almost NONE of them, and the asm-generic ABI
;;; additionally DROPPED open/stat/unlink/mkdir/rename in favour of *at forms —
;;; the same class of gap build-aarch64-cli.lisp fixed at be1aef1, and without
;;; these LOAD, OPEN and every path predicate are silently dead.
;;;
;;; THE *at CALLS GO THROUGH syscall6, NOT through five dedicated traps.  They
;;; take four arguments where syscall3 carries three; AArch64 added #x0506..#x050A
;;; for that, and one general six-argument trap (#x050B) covers the same ground
;;; with the ABI spelled out HERE, in Lisp, where it can be read and corrected.
;;; AT_FDCWD is -100.
(defvar *cli-arch-override-source* "
(defun %rv-openat (path-addr flags mode)
  (syscall6 56 -100 path-addr flags mode 0 0))
(defun %sys-open-rdonly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%rv-openat *cstr-scratch* 0 0))
(defun %core-open-path-at (addr) (%rv-openat addr 0 0))
(defun %sys-open-wronly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%rv-openat *cstr-scratch* 577 420))
(defun %sys-open-append (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%rv-openat *cstr-scratch* 1089 420))
(defun %sys-open-rdwr (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%rv-openat *cstr-scratch* 66 420))
(defun %sys-open-create-excl (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (%rv-openat *cstr-scratch* 193 420))
(defun %sys-close (fd) (syscall3 57 fd 0 0))
(defun %sys-getpid () (syscall3 172 0 0 0))
(defun %sys-read-raw (fd buf-addr count) (syscall3 63 fd buf-addr count))
(defun %sys-write-raw (fd buf-addr count) (syscall3 64 fd buf-addr count))
(defun %sys-lseek (fd offset whence) (syscall3 62 fd offset whence))
(defun %sys-unlink (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall6 35 -100 *cstr-scratch* 0 0 0 0))
(defun %sys-rename (old-str new-str)
  (%string-to-cstr old-str *cstr-scratch*)
  (let ((new-addr (+ *cstr-scratch* 2048)))
    (%string-to-cstr new-str new-addr)
    (syscall6 276 -100 *cstr-scratch* -100 new-addr 0 0)))
(defun %sys-mkdir (path-str mode)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall6 34 -100 *cstr-scratch* mode 0 0 0))
(defun %sys-stat-exists (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*)))
    (if (< (syscall6 48 -100 path-addr 0 0 0 0) 0) nil t)))
(defun %sys-stat-size (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall6 79 -100 path-addr buf-addr 0 0 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 48) :u32)))))
(defun %sys-stat-mtime (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall6 79 -100 path-addr buf-addr 0 0 0)))
      (if (< ret 0) 0 (mem-ref (+ buf-addr 88) :u32)))))
(defun %sys-fstat-size (fd)
  (let ((buf-addr *io-buf-addr*))
    (let ((ret (syscall3 80 fd buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 48) :u32)))))
(defun %sys-getdents64 (fd buf-addr buf-size) (syscall3 61 fd buf-addr buf-size))
")

(require :sb-posix)
(load (merge-pathnames "build-cli-common.lisp"
                       (directory-namestring (truename *load-truename*))))

;;; ============================================================
;;; BUILD
;;; ============================================================
;;;
;;; The translator-gap report is the point of the census i386 pioneered: a silent
;;; placeholder means the image builds clean and dies with no explanation, which in
;;; a 40 MB image is not something you find by reading.  Printed unconditionally.

;;; The hosted boot descriptors are NOT in lib/load-mvm.lisp's set — every
;;; build-*-linux script loads its own, and this follows that.  Two orderings
;;; matter here:
;;;
;;;   * boot-linux-riscv.lisp delegates its ELF emission to the generic ELF64-LE
;;;     writer that lives in boot-linux-aarch64.lisp (not AArch64-specific any
;;;     more — see its docstring), so that file must be present first.
;;;   * these calls are made BEFORE the (in-package :modus.mvm) below, because
;;;     MVM-LOAD is CL-USER's.  Inside :modus.mvm the same name resolves to a
;;;     DIFFERENT function, which fails with "invalid number of arguments: 1"
;;;     rather than with anything that names the real problem.
(mvm-load "boot/boot-linux-aarch64.lisp")
(mvm-load "boot/boot-linux-riscv.lisp")

(in-package :modus.mvm)

(setf *riscv-unimpl-ops* nil)
(install-riscv-translator)
(riscv-set-linux-mode t)

(let* ((out (or (sb-ext:posix-getenv "MODUS_RISCV64_OUT")
                (sb-ext:posix-getenv "MODUS_CLI_OUT")
                "/tmp/modus-riscv64-cli"))
       (image (build-image :target :linux-riscv
                           :source-text cl-user::*full-source*)))
  (let ((report (riscv-unimplemented-report)))
    (if (null report)
        (format t "~%  TRANSLATOR: no unimplemented opcodes or traps.~%")
        (progn
          (format t "~%  *** TRANSLATOR GAPS (~D distinct) ***~%" (length report))
          (dolist (e report)
            (let ((k (car e)) (n (cdr e)))
              (if (>= k #x10000)
                  (format t "    trap #x~4,'0X  x~D~%" (- k #x10000) n)
                  (format t "    opcode #x~2,'0X  x~D~%" k n)))))))
  (ensure-directories-exist out)
  (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
    (write-sequence (kernel-image-image-bytes image) o))
  (when (sb-ext:posix-getenv "MODUS_DUMP_NATIVE")
    (with-open-file (o (concatenate 'string out ".native")
                       :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
      (write-sequence (kernel-image-native-code image) o)))
  (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
  (format t "~%Wrote ~D bytes to ~A~%"
          (length (kernel-image-image-bytes image)) out)
  (format t "Run: qemu-riscv64-static ~A --eval EXPR --quit~%" out))
