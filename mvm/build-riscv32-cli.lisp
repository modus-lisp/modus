;;;; build-riscv32-cli.lisp -- the hosted Modus CLI for Linux/RISC-V 32.
;;;;
;;;; A :linux-riscv32 ELF run under qemu-riscv32-static.  Same SBCL-faithful
;;;; toplevel as ./modus (x64), build-riscv64-cli.lisp and build-i386-cli.lisp.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-riscv32-cli.lisp
;;;;   -> /tmp/modus-riscv32-cli   (override with MODUS_CLI_OUT)
;;;;
;;;; A thin RISC-V/32 tail over mvm/build-cli-common.lisp.  It is
;;;; build-riscv64-cli.lisp with the RV32 ABI facts swapped in; the two things
;;;; that are genuinely different are the SYSCALL TABLE (RV32 Linux is 64-bit-time
;;;; only: no newfstatat, no fstat, no lseek) and ARGV, which has to be read from
;;;; the copy the boot stub stages below 2^29 (see boot-linux-riscv32.lisp).

(defvar *cli-arch* :riscv32)

;;; exit_group is 94 on the asm-generic table, at both widths.
(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 94 c 0 0)))
(defun halt ()
  (syscall3 94 1 0 0))
")

;;; argc and the handler-frame introspection, as 32-bit words.
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
(defun %hc-depth () (mem-ref #x10000400 :u32))
(defun %hc-armed-p () (if (eql (mem-ref #x10000180 :u32) 0) nil t))
")

;;; No pre-init setup: the whole low block is a zero-filled MAP_ANONYMOUS mmap,
;;; and there is no native collector, so no %gc-bitmap-init (as on RV64).
(defvar *cli-arch-kernel-prologue* "")

;;; Scratch pages below the allocator (which starts at heap+#x20000 here):
;;; cstr at #x10001000 (rename uses +2048), the 4 KB I/O page at #x10002000.
(defvar *cli-arch-io-scratch-source* "  (setq *cstr-scratch* #x10001000)
  (setq *io-buf-addr*  #x10002000)
  (setq *scratch-mmapped* t)
")

(defvar *cli-arch-kernel-epilogue*
"  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
")

;;; The asm-generic table for a 64-BIT-TIME 32-bit port.  RV32 Linux has no
;;; newfstatat (79), no fstat (80) and no lseek (62 is _llseek there, with a
;;; different signature), so:
;;;   * stat / fstat go through STATX (291); struct statx has stx_size at 40 and
;;;     stx_mtime.tv_sec at 112.  fstat is statx on the fd with AT_EMPTY_PATH
;;;     (#x1000) and an empty path.
;;;   * lseek is _llseek(fd, off_hi, off_lo, &result, whence), result written to
;;;     memory -- a word in the cstr page, past rename's two paths.
;;; The *at calls, AT_FDCWD = -100, and syscall6 are as on RV64.
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
(defun %sys-lseek (fd offset whence)
  (let ((res (+ *cstr-scratch* 4088)))
    (let ((ret (syscall6 62 fd 0 offset res whence 0)))
      (if (< ret 0) ret (mem-ref res :u32)))))
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
    (let ((ret (syscall6 291 -100 path-addr 0 2047 buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 40) :u32)))))
(defun %sys-stat-mtime (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall6 291 -100 path-addr 0 2047 buf-addr 0)))
      (if (< ret 0) 0 (mem-ref (+ buf-addr 112) :u32)))))
(defun %sys-fstat-size (fd)
  (let ((path-addr (%string-to-cstr \"\" *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall6 291 fd path-addr 4096 2047 buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 40) :u32)))))
(defun %sys-getdents64 (fd buf-addr buf-size) (syscall3 61 fd buf-addr buf-size))

(defun %cli-argv-base () 268472320)   ; #x10009000 -- the staged pointer array

(defun %cli-collect-argv ()
  (let ((argc (%cli-argc)) (base (%cli-argv-base)) (acc nil) (i 0))
    (loop
      (when (>= i argc) (return (reverse acc)))
      (let ((ptr (mem-ref (+ base (* 4 i)) :u32)))
        (when (eql ptr 0) (return (reverse acc)))
        (setq acc (cons (%cli-cstr-at ptr) acc)))
      (setq i (+ i 1)))))

(defun %cli-getenv (name)
  (let ((argc (%cli-argc)) (base (%cli-argv-base)) (i 0))
    (let ((envp (+ base (* 4 (+ argc 1))))
          (prefix (concatenate (quote string) name (string (code-char 61)))))
      (let ((plen (length prefix)))
        (loop
          (let ((ptr (mem-ref (+ envp (* 4 i)) :u32)))
            (when (eql ptr 0) (return nil))
            (let ((entry (%cli-cstr-at ptr)))
              (when (and entry (>= (length entry) plen)
                         (string= (subseq entry 0 plen) prefix))
                (return (subseq entry plen)))))
          (setq i (+ i 1)))))))
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
;; The ELF32-LE writer (wrap-in-elf32-le-i386) lives in boot-linux-i386.lisp and
;; is generic apart from its name -- build-riscv32-linux.lisp loads it the same way.
(mvm-load "boot/boot-linux-i386.lisp")
(mvm-load "boot/boot-linux-riscv32.lisp")

(in-package :modus.mvm)

;; MODUS_SYMMAP=path writes a tab-separated virtual-address -> name map beside
;; the image.  It is what turns a `-d in_asm' trace's raw PC into the name of the
;; function that faulted, which in 31 MB is the difference between a bisect and a
;; guess.  Same knob build-generic-cli.lisp has.
(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf *write-symmap-path* sm)))

(setf *riscv-unimpl-ops* nil)
(install-riscv32-translator)
(riscv-set-linux-mode t)

(let* ((out (or (sb-ext:posix-getenv "MODUS_CLI_OUT")
                "/tmp/modus-riscv32-cli"))
       (image (build-image :target :linux-riscv32
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
  (format t "Run: qemu-riscv32-static ~A --eval EXPR --quit~%" out))
