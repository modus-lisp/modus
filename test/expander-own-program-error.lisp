;;;; test/expander-own-program-error.lisp — a macro expander's own PROGRAM-ERROR
;;;; comes out as itself, not as a wrong argument count to the expander.
;;;; Run: ./modus --load test/expander-own-program-error.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; %RUN-EXPANDER calls an expander installed with (SETF MACRO-FUNCTION) as
;;;; (form), and took any PROGRAM-ERROR to mean that was the wrong shape and
;;;; retried with (form env).  When the expander takes one argument and the
;;;; PROGRAM-ERROR came from inside its body, the retry failed with "invalid
;;;; number of arguments ... : 2", which named neither the macro nor its error.
;;;; TI Explorer's expanders take one argument, and cortez's UCL files hit it.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun inner-takes-one (x) x)
(defun error-text (form)
  "The error FORM signals -- or, for a DEFUN, its call: modus compiles an
   expansion's error into the function, raised when it runs."
  (handler-case (progn (eval form)
                       (when (eq (car form) 'defun) (funcall (cadr form)))
                       nil)
    (error (e) (princ-to-string e))))

;; One-argument expanders whose bodies signal a PROGRAM-ERROR.
(setf (macro-function 'own-pe-1)
      (eval '(lambda (form) (declare (ignore form)) (apply 'inner-takes-one '(1 2 3)))))
(setf (macro-function 'own-pe-2)
      (eval '(lambda (form) (declare (ignore form))
               (error 'program-error))))
;; And the shapes that must keep working.
(setf (macro-function 'one-arg-ok)
      (eval '(lambda (form) (list 'quote (cdr form)))))
(setf (macro-function 'two-arg-ok)
      (eval '(lambda (form env) (declare (ignore env)) (list 'quote (cdr form)))))
(setf (macro-function 'rest-ok)
      (eval '(lambda (&rest args) (list 'quote (length args)))))

(let ((m (error-text '(defun use-own-pe-1 () (own-pe-1)))))
  (check "expander's inner wrong count is reported as the inner call's"
         (and m (search "INNER-TAKES-ONE" m) (not (search ": 2" m)))))
(let ((m (error-text '(own-pe-1))))
  (check "the same under EVAL of the bare form"
         (and m (not (search ": 2" m)))))
(check "an expander's own PROGRAM-ERROR is still a PROGRAM-ERROR"
       (handler-case (progn (eval '(defun use-own-pe-2 () (own-pe-2))) (funcall 'use-own-pe-2) nil)
         (program-error () t)
         (error () nil)))
(check "a one-argument expander still expands"
       (equal (eval '(one-arg-ok a b)) '(a b)))
(check "a two-argument expander still expands"
       (equal (eval '(two-arg-ok a b)) '(a b)))
(check "and so does one taking &REST"
       (integerp (eval '(rest-ok a b))))
;; cortez's forwarder: a cell holding a symbol becomes (lambda (&rest args) (apply
;; definition args)), and the definition takes (form env) -- as SBCL calls it.
(defun fwd-target (form env) (declare (ignore env)) (list 'quote (cdr form)))
(setf (macro-function 'fwd-ok)
      (eval '(lambda (&rest args) (apply 'fwd-target args))))
(check "a &REST forwarder to a (form env) expander is called with both"
       (equal (eval '(fwd-ok a b)) '(a b)))
(check "and so inside a DEFUN"
       (progn (eval '(defun use-fwd () (fwd-ok 3)))
              (equal (funcall 'use-fwd) '(3))))
(check "inside a DEFUN too"
       (progn (eval '(defun use-ok () (list (one-arg-ok 1) (two-arg-ok 2))))
              (equal (funcall 'use-ok) '((1) (2)))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
