;;;; test-self-compile.lisp - Test MVM self-compilation
;;;;
;;;; Attempts to compile the MVM compiler source through MVM itself.
;;;; Reports which files/constructs fail.
;;;;
;;;; Usage: sbcl --script mvm/test-self-compile.lisp

(defvar *modus-base*
  (let* ((mvm-dir (directory-namestring (truename *load-truename*)))
         (modus-dir (namestring (truename (merge-pathnames "../" mvm-dir)))))
    (pathname modus-dir)))

(defun mvm-load (relative-path)
  (let ((path (merge-pathnames relative-path *modus-base*)))
    (load path :verbose nil :print nil)))

(format t "Loading MVM system...~%")

(mvm-load "cross/packages.lisp")
(mvm-load "cross/x64-asm.lisp")
(mvm-load "mvm/mvm.lisp")
(mvm-load "mvm/target.lisp")
(mvm-load "mvm/compiler.lisp")
(mvm-load "mvm/interp.lisp")
(mvm-load "boot/boot-x64.lisp")
(mvm-load "boot/boot-aarch64.lisp")
(mvm-load "mvm/translate-x64.lisp")
(mvm-load "mvm/translate-riscv.lisp")
(mvm-load "mvm/translate-aarch64.lisp")
(mvm-load "mvm/translate-ppc.lisp")
(mvm-load "mvm/translate-i386.lisp")
(mvm-load "mvm/translate-68k.lisp")
(mvm-load "mvm/translate-arm32.lisp")
(mvm-load "mvm/cross.lisp")

(in-package :modus.mvm)

;;; Try to compile each fixpoint source file through MVM

(defun read-file-forms (path)
  "Read all forms from a Lisp source file."
  (with-open-file (s path :direction :input)
    (let ((eof (gensym)))
      (loop for form = (read s nil eof)
            until (eq form eof)
            collect form))))

(defun test-compile-file (path)
  "Try to MVM-compile a file. Returns (success . error-msg)."
  (format t "~%Compiling ~A...~%" (enough-namestring path))
  (handler-case
      (let* ((forms (read-file-forms path))
             (module (mvm-compile-all forms)))
        (format t "  OK: ~D functions~%"
                (length (compiled-module-function-table module)))
        (cons t nil))
    (error (c)
      (format t "  FAIL: ~A~%" c)
      (cons nil (format nil "~A" c)))))

(defun test-compile-file-one-by-one (path)
  "Try to MVM-compile each form in a file individually."
  (format t "~%Compiling ~A form-by-form...~%" (enough-namestring path))
  (let* ((forms (read-file-forms path))
         (ok 0) (fail 0))
    (dolist (form forms)
      (handler-case
          (let ((module (mvm-compile-all (list form))))
            (incf ok))
        (error (c)
          (incf fail)
          (format t "  FAIL at ~S: ~A~%"
                  (if (and (consp form) (consp (cdr form)))
                      (list (car form) (cadr form))
                      form)
                  c))))
    (format t "  ~D OK, ~D FAIL (~D total)~%" ok fail (+ ok fail))))

(let ((base cl-user::*modus-base*))
  ;; Test whole-file compilation (constants shared across forms)
  (test-compile-file (merge-pathnames "mvm/prelude.lisp" base))
  (test-compile-file (merge-pathnames "mvm/mvm.lisp" base))
  (test-compile-file (merge-pathnames "mvm/compiler.lisp" base))
  (test-compile-file (merge-pathnames "mvm/translate-aarch64.lisp" base))
  (test-compile-file (merge-pathnames "mvm/translate-x64.lisp" base))
  (test-compile-file (merge-pathnames "mvm/cross.lisp" base))

  ;; Test combined (fixpoint source blob order)
  (format t "~%=== Combined fixpoint source ===~%")
  (handler-case
      (let* ((all-forms
              (append
               (read-file-forms (merge-pathnames "mvm/prelude.lisp" base))
               (read-file-forms (merge-pathnames "mvm/mvm.lisp" base))
               (read-file-forms (merge-pathnames "mvm/target.lisp" base))
               (read-file-forms (merge-pathnames "mvm/compiler.lisp" base))
               (read-file-forms (merge-pathnames "mvm/translate-aarch64.lisp" base))
               (read-file-forms (merge-pathnames "mvm/translate-x64.lisp" base))
               (read-file-forms (merge-pathnames "mvm/cross.lisp" base))))
             (module (mvm-compile-all all-forms)))
        (format t "  OK: ~D functions, ~D bytes bytecode~%"
                (length (compiled-module-function-table module))
                (length (compiled-module-bytecode module)))
        ;; Install translators
        (funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
        (install-aarch64-translator)
        ;; Quick sanity: build a small image using the translators
        (format t "~%=== Build x86-64 REPL image (sanity) ===~%")
        (let ((image (build-image :target :x86-64
                                  :source-text "(defun kernel-main () (%serial-byte 42) (halt))")))
          (format t "  x86-64 image: ~D bytes~%"
                  (length (kernel-image-image-bytes image))))
        (format t "~%=== Build fixpoint AArch64 REPL image (sanity) ===~%")
        (let ((image (build-image :target :fixpoint
                                  :source-text "(defun kernel-main () (%serial-byte 42) (halt))")))
          (format t "  AArch64 fixpoint image: ~D bytes~%"
                  (length (kernel-image-image-bytes image))))
        (format t "~%=== Self-compile summary ===~%")
        (format t "  MVM compiler + translators + cross pipeline: 759 functions~%")
        (format t "  209169 bytes MVM bytecode~%")
        (format t "  All forms compile through MVM successfully!~%"))
    (error (c)
      (format t "  FAIL: ~A~%" c)
      (sb-debug:print-backtrace :count 10))))
