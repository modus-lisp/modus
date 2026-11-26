;;;; test/compiler-macro-whole.lisp — &WHOLE in a compiler macro is the whole
;;;; form, operator included (CLHS 3.4.4), when the expander is called through
;;;; COMPILER-MACRO-FUNCTION.
;;;; Run: ./modus --load test/compiler-macro-whole.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; It was the argument list: (funcall (compiler-macro-function 'f) '(f 1 2) nil)
;;;; bound &WHOLE to (1 2), so a compiler macro that declines -- returns its
;;;; &WHOLE form, as cortez's DEFSUBST does -- answered a call to 1.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun cmw (name form) (funcall (compiler-macro-function name) form nil))

(defun cw-1 (x y) (list x y))
(define-compiler-macro cw-1 (&whole form &rest args) (list :whole form :args args))
(defun cw-2 (x y) (list x y))
(define-compiler-macro cw-2 (&whole form a b) (list :whole form :a a :b b))
(defun cw-3 (x y) (list x y))
(define-compiler-macro cw-3 (a b) (list :a a :b b))
(defun cw-4 (x y) (list x y))
(define-compiler-macro cw-4 (&whole form &rest args &environment env)
  (declare (ignore env)) (list :whole form :args args))
(defun cw-6 (x) x)
(define-compiler-macro cw-6 (&whole form x) (if (eql x 0) form (list 'quote x)))

(check "&WHOLE with &REST: the whole form, and the arguments"
       (equal (cmw 'cw-1 '(cw-1 1 2)) '(:whole (cw-1 1 2) :args (1 2))))
(check "&WHOLE with required parameters"
       (equal (cmw 'cw-2 '(cw-2 1 2)) '(:whole (cw-2 1 2) :a 1 :b 2)))
(check "no &WHOLE: the parameters, as before"
       (equal (cmw 'cw-3 '(cw-3 1 2)) '(:a 1 :b 2)))
(check "&WHOLE and &ENVIRONMENT together"
       (equal (cmw 'cw-4 '(cw-4 1 2)) '(:whole (cw-4 1 2) :args (1 2))))
(check "a declining compiler macro returns the form it was given, EQ"
       (let ((f (list 'cw-6 0))) (eq (cmw 'cw-6 f) f)))
(check "and one that does not decline expands as before"
       (equal (cmw 'cw-6 '(cw-6 5)) ''5))
(check "defined under EVAL too"
       (progn (eval '(define-compiler-macro cw-1 (&whole form &rest args) (list :eval form args)))
              (equal (cmw 'cw-1 '(cw-1 1 2)) '(:eval (cw-1 1 2) (1 2)))))
(check "called with one argument it is still a PROGRAM-ERROR"
       (handler-case (progn (funcall (compiler-macro-function 'cw-3) '(cw-3 1 2)) nil)
         (program-error () t)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
