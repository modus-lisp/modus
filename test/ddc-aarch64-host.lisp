;;;; test/ddc-aarch64-host.lisp -- the SBCL half of the fast AArch64 DDC.
;;;;   sbcl --dynamic-space-size 12288 --script test/ddc-aarch64-host.lisp SRC OUT
;;;; Compiles SRC (a source dump, e.g. run-uefi-ddc.sh's full-source.lisp) to a
;;;; Linux/AArch64 ELF with exactly the settings `modus-sh --compile-aarch64`
;;;; applies in-image (build-modus-selfhost.lisp, %INIT-AARCH64-TRANSLATOR and
;;;; %SELFHOST-COMPILE-FILE-AA64), so the two outputs differ only if the two
;;;; compilers do.  Not a production image: it is the comparison that matters.
(load (merge-pathnames "../lib/load-mvm.lisp" *load-truename*))
(mvm-load "boot/boot-linux-aarch64.lisp")
(in-package :modus.mvm)
(install-aarch64-translator)
(let ((v (make-array 23)))
  (loop for (i r) on '(0 0 1 1 2 2 3 3 4 19 5 20 6 21 7 22 8 23 9 6 10 7 11 4 12 5 13 8
                       14 nil 15 nil 16 0 17 24 18 25 19 26 20 31 21 29 22 nil) by #'cddr
        do (setf (aref v i) r))
  (setq *a64-vreg-to-phys* v))
(setq *a64-x18-base* t
      *aarch64-serial-width* 0
      *aarch64-linux-mode* t
      *aarch64-stack-align-16* t
      *aarch64-fn-align-offset* 120
      *linux-aarch64-r25-offset* #x38000000
      *linux-aarch64-gc-midpoint* #x1C000000
      *linux-aarch64-gc-metadata-shl* nil
      *aarch64-gc-bitmap-enabled* nil
      *aarch64-gc-native-mcgc* nil
      *aarch64-force-absolute-inmodule-calls* nil
      *aarch64-sched-lock-addr* nil
      *aarch64-setup-irq-enable* nil
      *aarch64-jit-mode* nil
      *aarch64-translate-into-buf* nil
      *static-build-p* t
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
                (build-image :target :linux-aarch64 :source-text src)))
       (bytes (kernel-image-image-bytes image)))
  (with-open-file (o (third args) :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
    (write-sequence bytes o))
  (format t "~&wrote ~D bytes to ~A~%" (length bytes) (third args)))
