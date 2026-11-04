;;;; test/symbol-macro-same-name.lisp -- a global symbol macro whose expansion
;;;; is a same-named symbol in another package.
;;;;
;;;; hunchentoot does exactly this: (define-symbol-macro *supports-threads-p*
;;;; bt:*supports-threads-p*).  Before the fix, reading the macro fell through
;;;; to the name-keyed entry for the TARGET too, expanded it to itself forever,
;;;; and the process died with SIGSEGV.
;;;;   ./modus --script test/symbol-macro-same-name.lisp   ; exit 0 = pass
(defpackage :smsn-p1 (:use :cl) (:export #:*x*))
(defvar smsn-p1:*x* 5)
(define-symbol-macro *x* smsn-p1:*x*)
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok    ~a~%" name)
      (progn (incf *fails*) (format t "FAIL  ~a: got ~s want ~s~%" name got want))))
(check "read through the macro" *x* 5)
(check "read the target" smsn-p1:*x* 5)
(setq *x* 9)
(check "setq through the macro writes the target" (list *x* smsn-p1:*x*) '(9 9))
(check "eval of the macro" (eval '*x*) 9)
(format t "~&symbol-macro-same-name: ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
