;;;; test/macro-implicit-block.lisp — a macro's body is in an implicit BLOCK named after it
;;;; Run: ./modus --load test/macro-implicit-block.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; CLHS gives DEFMACRO's body, a MACROLET definition's, and DEFINE-SETF-EXPANDER's an
;;;; implicit BLOCK named by the macro (the accessor, for DEFINE-SETF-EXPANDER).  Modus's
;;;; runtime DEFMACRO and MACROLET had none, and (RETURN-FROM NAME ...) signalled nothing: it
;;;; left only the innermost loop around it and the expander ran on.  MIT LOOP's LOOP-BODY
;;;; returns that way from inside two nested DOs, and spun forever.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defmacro rf-dolist (x)
  (dolist (y x) (when (eql y 2) (return-from rf-dolist (list 'quote (list :found y)))))
  ''none)
(check "RETURN-FROM a DEFMACRO's name inside DOLIST"
       (equal (macroexpand-1 '(rf-dolist (1 2 3))) ''(:found 2)))

(defmacro rf-do-end (x)
  (do ((l x (cdr l))) ((null l) (return-from rf-do-end ''end)))
  ''after)
(check "from a DO's end-test clause"
       (equal (macroexpand-1 '(rf-do-end (1 2 3))) ''end))

(defmacro rf-nested (x)
  (do () (nil)
    (do ((l x (cdr l))) ((null l) (return-from rf-nested ''inner-end)))
    (return ''outer))
  ''after)
(check "from inside two nested DOs (MIT LOOP's LOOP-BODY shape)"
       (equal (macroexpand-1 '(rf-nested (1 2))) ''inner-end))

(defmacro rf-doc (x)
  "A docstring, then a declaration, then the body."
  (declare (ignorable x))
  (return-from rf-doc ''early)
  ''late)
(check "with a docstring and declarations ahead of the body"
       (equal (macroexpand-1 '(rf-doc 1)) ''early))

(defmacro rf-env (&whole w &environment e x)
  (declare (ignore e x))
  (return-from rf-env (list 'quote (car w))))
(check "with &WHOLE and &ENVIRONMENT"
       (equal (macroexpand-1 '(rf-env 1)) ''rf-env))

(check "a MACROLET definition's body"
       (eq (eval '(macrolet ((ml (x) (dolist (y x) (when (eql y 2) (return-from ml '(quote :found))))
                               ''none))
                   (ml (1 2 3))))
           :found))

(check "and used inside a DEFUN"
       (progn (eval '(defun rf-use () (macrolet ((ml () (return-from ml 7) 8)) (ml))))
              (eql (funcall 'rf-use) 7)))

(check "an ordinary macro is unchanged"
       (progn (defmacro rf-plain (x) `(list ,x ,x))
              (equal (eval '(rf-plain 3)) '(3 3))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
