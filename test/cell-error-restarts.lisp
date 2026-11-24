;;;; test/cell-error-restarts.lisp -- UNBOUND-VARIABLE and UNDEFINED-FUNCTION
;;;; carry SBCL's restarts, and a handler that fixes the cell resumes in place.
;;;; Run: ./modus --load test/cell-error-restarts.lisp

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun names (c) (mapcar #'restart-name (compute-restarts c)))

(defun fixing (fix thunk)
  "THUNK with a handler that runs FIX and then CONTINUEs, as cortez's resolver does."
  (handler-case
      (handler-bind ((cell-error (lambda (c) (funcall fix (cell-error-name c))
                                   (invoke-restart (find-restart 'continue c)))))
        (funcall thunk))
    (error (e) (list :escaped (type-of e)))))

;; Compiled reads, as EVAL compiles a DEFUN's body.
(eval '(defun read-v1 () *cer-v1*))
(check "a compiled read: CONTINUE after the handler binds it reads the new value"
       (eq (fixing (lambda (s) (proclaim `(special ,s)) (set s :bound)) #'read-v1) :bound))
(eval '(defun read-v2 () *cer-v2*))
(check "its restarts include CONTINUE, USE-VALUE and STORE-VALUE"
       (let ((n (block got
                  (handler-case (handler-bind ((unbound-variable
                                                 (lambda (c) (return-from got (names c)))))
                                  (read-v2))
                    (error () nil)))))
         (subsetp '(continue use-value store-value) n)))
(eval '(defun read-v3 () *cer-v3*))
(check "USE-VALUE answers this once"
       (eq (handler-bind ((unbound-variable (lambda (c) (use-value :given c)))) (read-v3)) :given))
(eval '(defun read-v4 () *cer-v4*))
(check "STORE-VALUE sets it"
       (and (eq (handler-bind ((unbound-variable (lambda (c) (store-value :stored c)))) (read-v4)) :stored)
            (eq (symbol-value '*cer-v4*) :stored)))

;; SYMBOL-VALUE.
(check "SYMBOL-VALUE: CONTINUE after binding reads the new value"
       (eq (fixing (lambda (s) (set s :sv)) (lambda () (symbol-value '*cer-v5*))) :sv))

;; Undefined functions.
(check "a call to an undefined function: CONTINUE after defining it calls it"
       (eql (fixing (lambda (s) (setf (fdefinition s) (lambda (x) (* x 3))))
                    (lambda () (funcall 'cer-f1 14)))
            42))
(check "USE-VALUE calls the function given"
       (eql (handler-bind ((undefined-function (lambda (c) (use-value (lambda (x) (+ x 1)) c))))
              (funcall 'cer-f2 41))
            42))

;; Unhandled, they are still errors.
(check "with no handler fixing anything, an unbound read is still an UNBOUND-VARIABLE"
       (eq (handler-case (symbol-value '*cer-never*) (unbound-variable () :caught)) :caught))
(check "and an undefined call an UNDEFINED-FUNCTION"
       (eq (handler-case (funcall 'cer-never) (undefined-function () :caught)) :caught))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
