;;;; test/shadowed-return-values.lisp — a RETURN macro that shadows CL:RETURN and
;;;; takes several values keeps them all.  The compiler's tail scan read it as
;;;; CL:RETURN, took its first argument for the whole value, and clamped the
;;;; count to 1.  Run: ./modus --load test/shadowed-return-values.lisp
;;;; Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defpackage :srv-lm (:use :cl) (:shadow #:return #:return-from) (:export #:return #:return-from))
(in-package :srv-lm)
(defmacro return (&rest values) `(cl:return-from nil (values ,@values)))
(defmacro return-from (name &rest values) `(cl:return-from ,name (values ,@values)))
(defpackage :srv-user (:use :cl) (:shadowing-import-from :srv-lm #:return #:return-from))
(in-package :cl-user)

(defparameter *cases*
  '(("block, return"            "(block nil (return t ba))"                                    (t :clean))
    ("block, let, return"       "(block nil (let ((r nil)) (declare (ignorable r)) (return t ba)) nil)" (t :clean))
    ("named block, return-from" "(block b (return-from b t ba))"                               (t :clean))
    ("block, when, return"      "(block nil (when ba (return t ba)) :none)"                    (t :clean))
    ("dolist, return"           "(dolist (x '(1)) (return x ba))"                              (1 :clean))
    ("single value stays one"   "(block nil (return t))"                                       (t))))

(dolist (c *cases*)
  (let* ((body (let ((*package* (find-package :srv-user))) (read-from-string (second c))))
         (lam (list 'lambda (list (intern "BA" :srv-user)) body))
         (want (third c)))
    (check (format nil "~a: compiled" (first c))
           (equal (multiple-value-list (funcall (compile nil lam) :clean)) want))
    (check (format nil "~a: EVAL'd" (first c))
           (equal (multiple-value-list (funcall (eval lam) :clean)) want))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
