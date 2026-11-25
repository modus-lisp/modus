;;;; test/setf-macro-function-copy.lisp — a macro function copied with
;;;; (SETF (MACRO-FUNCTION 'B) (MACRO-FUNCTION 'A)) is still callable with (FORM ENV).
;;;; It was wrapped twice and signalled a bare PROGRAM-ERROR.
;;;; Run: ./modus --load test/setf-macro-function-copy.lisp   Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))
(defmacro try (form) `(handler-case ,form (error (e) (list :error (type-of e)))))

(defmacro smf-a (&rest args) (cons 'list args))
(defmacro smf-one (x) `(list ,x ,x))

(check "MACRO-FUNCTION answers the same function each time" (eq (macro-function 'smf-a) (macro-function 'smf-a)))
(check "SETF returns the value it was given"
       (let ((f (macro-function 'smf-a))) (eq (setf (macro-function 'smf-b) f) f)))
(check "the copy is callable with (FORM ENV)"
       (equal (try (funcall (macro-function 'smf-b) '(smf-b 1 2) nil)) '(list 1 2)))
;; modus's macro functions refuse one argument (ANSI's X.ERROR.2); SBCL's take it.
(when (find-package "MODUS")
  (check "and still refuses one argument"
         (equal (try (funcall (macro-function 'smf-b) '(smf-b 1 2))) '(:error program-error))))
(check "MACROEXPAND-1 of the copy" (equal (macroexpand-1 '(smf-b 3)) '(list 3)))
(check "the copy works compiled and evaluated"
       (and (equal (eval '(smf-b 4 5)) '(4 5)) (equal (funcall (compile nil '(lambda () (smf-b 6)))) '(6))))
(check "through a *MACROEXPAND-HOOK* that funcalls what it is handed"
       (equal (let ((*macroexpand-hook* (lambda (f form env) (funcall f form env)))) (macroexpand-1 '(smf-b 7)))
              '(list 7)))
(setf (macro-function 'smf-c) (macro-function 'smf-b))
(check "a copy of a copy" (equal (try (funcall (macro-function 'smf-c) '(smf-c 8) nil)) '(list 8)))
(setf (macro-function 'smf-d) (macro-function 'smf-one))
(check "a copy of a macro with a fixed lambda list" (equal (try (funcall (macro-function 'smf-d) '(smf-d 9) nil)) '(list 9 9)))
(setf (macro-function 'smf-e) (lambda (form env) (declare (ignore env)) `(quote ,(cdr form))))
(check "a plain function installed as a macro, called with (FORM ENV)"
       (equal (try (funcall (macro-function 'smf-e) '(smf-e 1 2) nil)) ''(1 2)))
(check "and evaluated" (equal (eval '(smf-e a b)) '(a b)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
