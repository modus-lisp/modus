;;;; test/ddc-riscv32-host.lisp -- the SBCL half of the RV32 DDC (W32 M1).
;;;;   sbcl --dynamic-space-size 12288 --script test/ddc-riscv32-host.lisp SRC OUT
;;;; Compiles SRC (a source dump, e.g. run-uefi-ddc.sh's full-source.lisp) to a
;;;; Linux/RISC-V 32 ELF with exactly the settings `modus-sh --compile-riscv32`
;;;; applies in-image (build-modus-selfhost.lisp, %INIT-RISCV32-TRANSLATOR and
;;;; %SELFHOST-COMPILE-FILE-RV32), so the two outputs differ only if the two
;;;; compilers do.  The UEFI and AArch64 DDCs are both 64-bit little-endian;
;;;; this is the one whose TARGET width differs from the self-hosted
;;;; compiler's own, so it is the one that catches a compiler decision taken
;;;; by the running image's widths instead of the compilation's (docs/w32.md).
;;;; Not a production image: it is the comparison that matters.
(load (merge-pathnames "../lib/load-mvm.lisp" *load-truename*))
;; build-riscv32-cli.lisp's boot files, in its order (the ELF32-LE writer is in
;; boot-linux-i386.lisp).
(mvm-load "boot/boot-linux-aarch64.lisp")
(mvm-load "boot/boot-linux-riscv.lisp")
(mvm-load "boot/boot-linux-i386.lisp")
(mvm-load "boot/boot-linux-riscv32.lisp")
(in-package :modus.mvm)
(setf *riscv-unimpl-ops* nil)
(install-riscv32-translator)
(riscv-set-linux-mode t)
(setq *static-build-p* t
      *mvm-emit-halves* nil
      *mvm-eval-runtime-p* nil)
(let* ((args sb-ext:*posix-argv*)
       (src (with-open-file (s (second args) :external-format :utf-8)
              (let ((str (make-string (file-length s))))
                (subseq str 0 (read-sequence str s)))))
       (image (progn
                (setq *static-keywords-p* t *static-symbols-p* t)
                (when (search "(defun %console-write-char" src)
                  (setq *write-char-serial-hook* '%console-write-char))
                (build-image :target :linux-riscv32 :source-text src)))
       (bytes (kernel-image-image-bytes image)))
  (with-open-file (o (third args) :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
    (write-sequence bytes o))
  (format t "~&wrote ~D bytes to ~A~%" (length bytes) (third args)))
