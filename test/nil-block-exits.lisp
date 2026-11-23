;;;; test/nil-block-exits.lisp — RETURN-FROM NIL out of a lambda exits the enclosing
;;;; BLOCK NIL, with every value.  Run: ./modus --load test/nil-block-exits.lisp
;;;; Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun via-funcall () (block nil (funcall (lambda () (return-from nil (values :x :y)))) :fell))
(defun via-return () (block nil (funcall (lambda () (return (values :x :y)))) :fell))
(defun via-inline () (block nil ((lambda () (return-from nil (values :x :y)))) :fell))
(defun via-mapc () (block nil (mapc (lambda (e) (return-from nil (values e :y))) '(:x)) :fell))
(defun via-let-closure () (block nil (let ((f (lambda () (return-from nil (values :x :y))))) (funcall f)) :fell))
(defun named () (block b (funcall (lambda () (return-from b (values :x :y)))) :fell))
(defun inner-nil-shadows ()
  (block nil (funcall (lambda () (block nil (return-from nil :inner)) )) :outer))
(defun prog-shape (ba) (block abort (block nil (let ((r nil)) (declare (ignorable r)) (funcall (lambda () (return-from nil (values t ba)))) nil))))

(dolist (f '(via-funcall via-return via-inline via-mapc via-let-closure named))
  (check (format nil "~(~a~): (BLOCK NIL ...) exits with both values" f)
         (equal (multiple-value-list (funcall f)) '(:x :y)))
  (check (format nil "~(~a~), EVAL'd" f)
         (equal (multiple-value-list (eval (list f))) '(:x :y))))
(check "an inner BLOCK NIL inside the lambda takes its own RETURN-FROM NIL"
       (eq (inner-nil-shadows) :outer))
(check "a PROG-shaped exit keeps its second value"
       (equal (multiple-value-list (prog-shape :clean)) '(t :clean)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
