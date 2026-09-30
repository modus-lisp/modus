;;;; build-arm32-cli.lisp -- the hosted Modus CLI for Linux/ARM32 (EABI).
;;;;
;;;; A :linux-arm32 ELF (ARMv7-A) run under qemu-arm-static.  Same SBCL-faithful
;;;; toplevel as ./modus (x64) and the other CLI ports.
;;;;
;;;;   sbcl --dynamic-space-size 12288 --script mvm/build-arm32-cli.lisp
;;;;   -> /tmp/modus-arm32-cli   (override with MODUS_CLI_OUT)
;;;;
;;;; STATUS: the full CL image with a native copying collector (kind-byte
;;;; allocation marks; translate-arm32's hosted-collector header), faults as
;;;; conditions, overflow promotion to bignums, single floats and f32 vectors,
;;;; the clock (ms since first read, 1000 Hz) and the JIT traps.  The build
;;;; prints the translator's census; it is empty.
;;;;
;;;; A thin tail over build-cli-common.lisp.  The ARM EABI syscall table is
;;;; i386's legacy table for almost everything this slot touches, so the
;;;; override block is i386's with ARM's differences swapped in:
;;;;   exit_group 248 (i386 252), getdents64 217 (220), clock_gettime 263 (265),
;;;;   and struct stat64: EABI aligns the 64-bit st_size to 8, so it is at 48
;;;;   (i386's packed layout has it at 44) and st_mtime at 80 (72).
;;;; ARGV is read from the copy boot-linux-arm32.lisp stages below 2^29, at the
;;;; same #x10009000 as i386/RV32.

(defvar *cli-arch* :arm32)

(defvar *cli-arch-syscall-source* "
(defun sys-exit (code)
  (let ((c code))
    (syscall3 248 c 0 0)))
(defun halt ()
  (syscall3 248 1 0 0))
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
(defun %sys-stat-exists (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*)))
    (if (< (syscall3 33 path-addr 0 0) 0) nil t)))
(defun %sys-stat-size (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 195 path-addr buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 48) :u32)))))
(defun %sys-stat-mtime (path-str)
  (let ((path-addr (%string-to-cstr path-str *cstr-scratch*))
        (buf-addr *io-buf-addr*))
    (let ((ret (syscall3 195 path-addr buf-addr 0)))
      (if (< ret 0) 0 (mem-ref (+ buf-addr 80) :u32)))))
(defun %sys-fstat-size (fd)
  (let ((buf-addr *io-buf-addr*))
    (let ((ret (syscall3 197 fd buf-addr 0)))
      (if (< ret 0) -1 (mem-ref (+ buf-addr 48) :u32)))))
(defun %sys-getdents64 (fd buf-addr buf-size) (syscall3 217 fd buf-addr buf-size))
;; clock_gettime is 263 on ARM EABI (265 on i386, 228 on x86-64), and i386's struct timespec is
;; two 32-bit longs, so tv_nsec is at +4, not +8.  Without this override
;; GET-INTERNAL-REAL-TIME fell back to its call counter and every timing on
;; i386 read 1 tick.  (32-bit time_t: good until 2038.)
(defun %clock-gettime-ns (clk)
  (let ((buf *io-buf-addr*))
    (if (eql (syscall3 263 clk buf 0) 0)
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

(mvm-load "boot/boot-linux-i386.lisp")     ; the generic ELF32-LE writer
(mvm-load "boot/boot-linux-arm32.lisp")

(in-package :modus.mvm)

(let ((sm (sb-ext:posix-getenv "MODUS_SYMMAP")))
  (when (and sm (> (length sm) 0))
    (setf *write-symmap-path* sm)))

(install-armv7-translator)
(arm32-set-linux-mode t)

(let* ((out (or (sb-ext:posix-getenv "MODUS_CLI_OUT")
                "/tmp/modus-arm32-cli"))
       (image (build-image :target :linux-arm32
                           :source-text cl-user::*full-source*)))
  (if *arm32-unimpl-opcodes*
      (format t "~%  *** TRANSLATOR GAPS: ~{#x~X~^ ~} (#x1xxxx = trap xxxx) ***~%"
              (sort (copy-list *arm32-unimpl-opcodes*) #'<))
      (format t "~%  TRANSLATOR: no unimplemented opcodes or traps.~%"))
  (ensure-directories-exist out)
  (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
    (write-sequence (kernel-image-image-bytes image) o))
  (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
  (format t "~%Wrote ~D bytes to ~A~%"
          (length (kernel-image-image-bytes image)) out)
  (format t "Run: qemu-arm-static ~A --eval EXPR --quit~%" out))
