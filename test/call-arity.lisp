;;; test/call-arity.lisp -- calls with the wrong number of arguments signal
;;; PROGRAM-ERROR, and a GO from a lambda-list default reaches its tag.
;;; Expected values are SBCL's.
;;;
;;;   ./modus --script test/call-arity.lisp
;;;
;;; The arity half covers code compiled at runtime (this file): DEFUNs called
;;; interpreted and native, FUNCALL / APPLY / a #'NAME, &optional bounds, a
;;; LAMBDA, a COMPILEd lambda, a generic function, and local functions.

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (what got want)
  (setq *checks* (+ *checks* 1))
  (unless (equal got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a: got ~s want ~s~%" what got want)))

(defmacro arity (form)
  `(handler-case (list :ok ,form) (program-error () :program-error)))

(defun f1 (a) a)
(defun f2 (a b) (list a b))
(defun fo (a &optional (b 7) c) (list a b c))
(defgeneric g2 (a b))
(defmethod g2 (a b) (list a b))

(defun all-cases ()
  (list (arity (funcall #'f2 1))
        (arity (funcall #'f1 1 2))
        (arity (apply #'f1 '(1 2)))
        (arity (funcall #'fo))
        (arity (funcall #'fo 1 2 3 4))
        (arity (list (fo 1) (fo 1 2) (fo 1 2 3)))
        (arity (funcall (lambda (a) a)))
        (arity (funcall (compile nil '(lambda (a b) (list a b))) 1))
        (arity (g2 1))
        (arity (apply #'g2 '(1 2 3)))
        (arity (flet ((h (x) x)) (funcall #'h)))
        (arity (labels ((h (x &optional y) (list x y))) (funcall #'h 1 2 3)))
        (arity (mapcar #'f1 '(1 2 3)))
        (arity (sort (list 3 1 2) (lambda (a b) (< a b))))))

(defvar *want*
  '(:program-error :program-error :program-error :program-error :program-error
    (:ok ((1 7 nil) (1 2 nil) (1 2 3)))
    :program-error :program-error :program-error :program-error
    :program-error :program-error
    (:ok (1 2 3)) (:ok (1 2 3))))

(check "interpreted" (all-cases) *want*)
(dotimes (i 100) (all-cases))
(check "native" (all-cases) *want*)

;; Local functions with distinct names and arities in one form resolve to
;; themselves (they once shared one key when their names were hash-only).
(check "local functions"
       (funcall (compile nil '(lambda (a)
                                (flet ((f11 () 0))
                                  (flet ((f13 (x y z) (list x y z)))
                                    (funcall #'f13 0 a (f11))))))
                5)
       '(0 5 0))

;; A GO from a local function's &optional / &key default, or a lambda's, is
;; a non-local exit to the enclosing TAGBODY (it compiled to NIL).
(check "go from labels &optional default"
       (let ((r nil))
         (tagbody (labels ((f (x &optional (y (go out))) (list x y)))
                    (push (f 1 2) r) (push (f 1) r))
          out (push :out r))
         r)
       '(:out (1 2)))
(check "go from flet &key default"
       (let ((r nil))
         (tagbody (flet ((f (&key (k (go out))) k)) (push (f :k 3) r) (push (f) r))
          out (push :out r))
         r)
       '(:out 3))
(check "go from lambda &optional default"
       (let ((r nil))
         (tagbody (funcall (lambda (&optional (y (go out))) (push y r)))
          out (push :out r))
         r)
       '(:out))

(format t "~&call-arity: ~d checks, ~d failed~%" *checks* *fails*)
(when (> *fails* 0) (sys-exit 1))
