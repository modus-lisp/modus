;;;; test/lambda-form-values.lisp — a lambda form returns all its values
;;;; (CLHS 3.1.2.1.2.4: the same as FUNCALL of the function).
;;;; Run: ./modus --load test/lambda-form-values.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; TAIL-FORM-IS-VALUES-P answered NIL for any non-symbol head, so a function
;;;; whose tail was ((LAMBDA ...) ...) had its value count set to 1 by its own
;;;; epilogue.  TI Explorer's LOOP expands to that shape, so a (RETURN (VALUES
;;;; ...)) out of their LOOP kept only its first value.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun lf-plain (x) ((lambda (y) (values 1 y)) x))
(defun lf-block (x) ((lambda (y) (block nil (return (values 1 y)))) x))
(defun lf-loop-shape (l)
  ((lambda (ns g)
     (block nil
       (let nil
         (tagbody
          next (and (null g) (go end))
            (setq ns (car g)) (setq g (cdr g))
            (when ns (return (values ns :found)))
            (go next)
          end (return nil)))))
   nil l))
(defun lf-single (x) ((lambda (y) (+ y 1)) x))
(defun lf-then-single (x) (progn ((lambda (y) (values 1 y)) x) 9))

(check "((lambda (y) (values 1 y)) x) returns both values"
       (equal (multiple-value-list (lf-plain 7)) '(1 7)))
(check "and so does a RETURN out of its block"
       (equal (multiple-value-list (lf-block 7)) '(1 7)))
(check "TI's LOOP shape: (RETURN (VALUES ...)) out of the TAGBODY keeps both"
       (equal (multiple-value-list (lf-loop-shape '(nil 5 6))) '(5 :found)))
(check "and falls out of the end with one NIL"
       (equal (multiple-value-list (lf-loop-shape '(nil))) '(nil)))
(check "a single-valued lambda form is still one value"
       (equal (multiple-value-list (lf-single 7)) '(8)))
(check "and a lambda form NOT in tail position leaves no stray values"
       (equal (multiple-value-list (lf-then-single 7)) '(9)))
(check "under EVAL too"
       (equal (multiple-value-list (eval '((lambda (y) (values 1 y)) 7))) '(1 7)))
(check "as FUNCALL of the same lambda always did"
       (equal (multiple-value-list (funcall (lambda (y) (values 1 y)) 7)) '(1 7)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
