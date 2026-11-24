;;;; test/eval-function-undefined.lisp — (FUNCTION name) of an undefined function
;;;; under EVAL signals UNDEFINED-FUNCTION, as SBCL does, instead of answering NIL.
;;;; Run: ./modus --load test/eval-function-undefined.lisp   Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun signals-undefined (form name)
  (handler-case (progn (eval form) nil)
    (undefined-function (c) (eq (cell-error-name c) name))))

(check "EVAL of #'undefined signals UNDEFINED-FUNCTION naming it"
       (signals-undefined '(function efu-none-1) 'efu-none-1))
(check "and inside a larger form"
       (signals-undefined '(list 1 (function efu-none-2)) 'efu-none-2))
(check "and as a SETQ's value, which is left alone"
       (progn (defvar *efu* :untouched)
              (and (signals-undefined '(setq *efu* (function efu-none-3)) 'efu-none-3)
                   (eq *efu* :untouched))))
(check "defined afterwards, the same form answers the function"
       (progn (defun efu-later () :later)
              (eq (funcall (eval '(function efu-later))) :later)))
(check "#'defined still answers the function"
       (eq (eval '(function car)) (symbol-function 'car)))
(check "a function DEFUNed earlier in the same EVAL'd form"
       (eq (funcall (eval '(progn (defun efu-same () :same) (function efu-same)))) :same))
(check "a FLET-bound name still answers the local function"
       (eq (eval '(flet ((efu-none-4 () :local)) (funcall (function efu-none-4)))) :local))
(check "and a LABELS-bound one"
       (eq (eval '(labels ((efu-none-5 () :labels)) (funcall (function efu-none-5)))) :labels))
(check "a resolver's retry: define on UNDEFINED-FUNCTION, then run again"
       (let ((form '(funcall (function efu-pulled))))
         (handler-case (eval form)
           (undefined-function (c)
             (setf (fdefinition (cell-error-name c)) (lambda () :pulled))
             (eq (eval form) :pulled)))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
