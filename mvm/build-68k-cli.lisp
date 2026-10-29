;;;; build-68k-cli.lisp -- the hosted Modus CLI for Linux/m68k.
;;;;
;;;; A :linux-68k ELF (68020+ with a 6888x/68040 FPU) run under qemu-m68k-static.
;;;; Same SBCL-faithful toplevel as ./modus (x64) and the other CLI ports.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-68k-cli.lisp
;;;;   -> /tmp/modus-68k-cli   (override with MODUS_CLI_OUT)
;;;;
;;;; The FIRST BIG-ENDIAN CL image.  A thin tail over build-cli-common.lisp, cut
;;;; from build-arm32-cli.lisp: Linux/m68k uses the legacy (i386) syscall table
;;;; for most of what this slot touches, but not all -- every number here was
;;;; MEASURED with qemu-m68k -strace, because two guesses were wrong:
;;;;   exit_group 247 (i386 252)   clock_gettime 260 (i386 265; 263 is statfs64)
;;;;   getdents64 220   stat64 195   fstat64 197   rt_sigaction 174
;;;; struct stat64 (big-endian): st_size is 64 bits at +40, so its low word is
;;;; at +44; st_atime +60, st_mtime +68 (measured on a file with known values).
;;;; The heap is sized at startup (--dynamic-space-size), argv is staged at
;;;; #x10009000 and NIL's page is mapped at #xDEAD0000 by boot-linux-68k.lisp.
;;;;
;;;; STATUS: bring-up.  The build prints the translator's census of opcodes and
;;;; service traps it has no arm for.
(defvar *cli-arch* :68k)

;; MODUS_68K_SOFT_FLOAT=1: no FPU instructions at all, for the 68LC040 and the
;; 68EC0x0 -- floats go through mvm/softfloat.lisp (see build-cli-common).
(defvar *cli-soft-float*
  (let ((v (sb-ext:posix-getenv "MODUS_68K_SOFT_FLOAT")))
    (and v (plusp (length v)) (not (string= v "0")))))

(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 247 c 0 0)))
(defun halt ()
  (syscall3 247 1 0 0))
")

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

;;; The heap mapping is zero-filled and covers the low block; nothing to set up.
(defvar *cli-arch-kernel-prologue* "")

;;; Scratch pages below the allocator (heap+#x20000): cstr at #x10001000
;;; (rename uses +2048), the 4 KB I/O page at #x10002000 -- RV32's layout.
(defvar *cli-arch-io-scratch-source* "  (setq *cstr-scratch* #x10001000)
  ;; getdents64 scratch: cl-fileio's default is a fixed #x1DD00000, which on
  ;; this layout is INSIDE the heap (it starts at #x10000000 and reaches
  ;; #x1F800000), so listing a directory wrote dirents over live objects --
  ;; found when the 68k CLI died in install-tarball, in a directory with a
  ;; thousand entries.  4 KB is all it uses.
  (setq *%dirent-buf-addr* #x10003000)
  (setq *io-buf-addr*  #x10002000)
  (setq *scratch-mmapped* t)
")

(defvar *cli-arch-kernel-epilogue*
"  ;; --- entry: the SHARED SBCL-faithful CLI toplevel ------------------------
  (handler-case (cli-toplevel) (t (c) (sys-exit 1))))
")

(defvar *cli-arch-override-source* "
(defun %sys-open-rdonly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 0 0))
(defun %sys-open-wronly (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 577 420))
(defun %sys-open-append (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 1089 420))
(defun %sys-open-rdwr (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 66 420))
(defun %sys-open-wronly-existing (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 1 0))
(defun %sys-open-create-excl (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 5 *cstr-scratch* 193 420))
(defun %sys-close (fd) (syscall3 6 fd 0 0))
(defun %sys-getpid () (syscall3 20 0 0 0))
(defun %sys-read-raw (fd buf-addr count) (syscall3 3 fd buf-addr count))
(defun %sys-write-raw (fd buf-addr count) (syscall3 4 fd buf-addr count))
(defun %sys-lseek (fd offset whence) (syscall3 19 fd offset whence))
(defun %sys-unlink (path-str)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 10 *cstr-scratch* 0 0))
(defun %sys-rename (old-str new-str)
  (%string-to-cstr old-str *cstr-scratch*)
  (let ((new-addr (+ *cstr-scratch* 2048)))
    (%string-to-cstr new-str new-addr)
    (syscall3 38 *cstr-scratch* new-addr 0)))
(defun %sys-mkdir (path-str mode)
  (%string-to-cstr path-str *cstr-scratch*)
  (syscall3 39 *cstr-scratch* mode 0))
(defun %sys-readlink (path-str) (declare (ignore path-str)) nil)   ; links not followed here yet

(defun %sys-stat-exists (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*)))
    (if (< (syscall3 33 path-addr 0 0) 0) nil t)))
(defun %sys-stat-size (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 195 path-addr buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 44) :u32)))))
(defun %sys-stat-mtime (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 195 path-addr buf-addr 0)))
      (if (< ret 0) 0 (mem-ref (+ buf-addr 68) :u32)))))
(defun %sys-fstat-size (fd)
  (let ((buf-addr *io-buf-addr*))
    (let ((ret (syscall3 197 fd buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 44) :u32)))))
(defun %sys-getdents64 (fd buf-addr buf-size) (syscall3 220 fd buf-addr buf-size))
;; clock_gettime is 260 on m68k (measured with qemu -strace: 263 is statfs64 there,
;; 265 tgkill; ARM EABI 263, i386 265).  struct timespec is
;; two 32-bit longs, so tv_nsec is at +4, not +8.  Without this override
;; GET-INTERNAL-REAL-TIME fell back to its call counter and every timing on
;; i386 read 1 tick.  (32-bit time_t: good until 2038.)
(defun %clock-gettime-ns (clk)
  (let ((buf *io-buf-addr*))
    (if (eql (syscall3 260 clk buf 0) 0)
        (+ (* (mem-ref buf :u32) 1000000000) (mem-ref (+ buf 4) :u32))
        0)))

(defun %cli-argv-base () 268472320)   ; #x10009000 — the staged pointer array

(defun %cli-collect-argv ()
  (let ((argc (%cli-argc)) (base (%cli-argv-base)) (acc nil) (i 0))
    (loop
      (when (>= i argc) (return (reverse acc)))
      (let ((ptr (mem-ref (+ base (* 4 i)) :u32)))
        (when (eql ptr 0) (return (reverse acc)))
        (setq acc (cons (%cli-cstr-at ptr) acc)))
      (setq i (+ i 1)))))

(defun %cli-getenv (name)
  ;; envp begins one slot past argv's NULL terminator, exactly as on the real
  ;; stack.  NB the equals sign is built with code-char: this source is a LISP
  ;; STRING and a double quote in it would terminate the string early.
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

(mvm-load "boot/boot-linux-68k.lisp")

(in-package :modus.mvm)

(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf *write-symmap-path* sm)))

(install-68k-translator)
(m68k-set-linux-mode t)

(let ((v (sb-ext:posix-getenv "MODUS_68K_GC_POISON")))
  (when (and v (plusp (length v)) (not (string= v "0")))
    (setf modus.mvm::*68k-gc-poison* t)
    (format t "~&;; MODUS_68K_GC_POISON: collections poison the evacuated semispace~%")))
(let* ((out (or (sb-ext:posix-getenv "MODUS_CLI_OUT")
                "/tmp/modus-68k-cli"))
       (image (build-image :target :linux-68k
                           :source-text cl-user::*full-source*)))
  (if *68k-unimpl-opcodes*
      (format t "~%  *** TRANSLATOR GAPS: ~{#x~X~^ ~} (#x1xxxx = trap xxxx) ***~%"
              (sort (copy-list *68k-unimpl-opcodes*) #'<))
      (format t "~%  TRANSLATOR: no unimplemented opcodes or traps.~%"))
  (ensure-directories-exist out)
  (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
    (write-sequence (kernel-image-image-bytes image) o))
  (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
  (format t "~%Wrote ~D bytes to ~A~%"
          (length (kernel-image-image-bytes image)) out)
  (format t "Run: qemu-m68k-static ~A --eval EXPR --quit~%" out))
