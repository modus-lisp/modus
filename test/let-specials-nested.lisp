;;;; test/let-specials-nested.lisp -- a worker's nested LETs of several specials keep the outer bindings.
;;;;   modus --script test/let-specials-nested.lisp   => LET-SPECIALS-NESTED PASS
;;;;
;;;; On a worker thread a binding lives on the thread's own binding stack.  The restores of a LET
;;;; of several specials ran in BINDING order and each searched down for its variable: the first
;;;; took the LET's own entries, and the rest found OUTER bindings of their variables and cut the
;;;; stack there -- losing every live binding above them.  shuttle's compiler binds (*strict*
;;;; *in-function*) and then *out* for each JS function it compiles, so compiling a function inside
;;;; a function read the outer *out* unbound, on a worker only (the main thread binds shallowly).
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defvar *strict* nil) (defvar *in-fn* nil) (defvar *out*)

(defun compile-like (depth)
  "shuttle's COMPILE-FN shape: two specials in one LET, then *OUT*, then a nested compile, then a
   push onto this level's *OUT*."
  (let ((*strict* (list :s depth)) (*in-fn* (list :f depth)))
    (let ((*out* '()))
      (push (list :start depth) *out*)
      (when (> depth 0) (push (compile-like (- depth 1)) *out*))
      (push (list :end depth *strict* *in-fn*) *out*)
      (reverse *out*))))

(defun let*-throw ()
  "An outer binding of *IN-FN*, then a LET* whose second init throws: the cleanup must not reach
   the outer binding."
  (let ((*in-fn* :outer) (*out* :outer-out))
    (handler-case (let* ((*strict* :inner) (*in-fn* (error "boom"))) (list *strict* *in-fn*))
      (error () nil))
    (list *in-fn* *out* *strict*)))

(defun progv-nested ()
  (let ((*in-fn* :outer))
    (let ((*out* :kept))
      (progv '(*strict* *in-fn*) '(:p1 :p2) (list *strict* *in-fn*))
      (list *in-fn* *out*))))

(defun all-cases ()
  (list (compile-like 2) (let*-throw) (progv-nested)
        (list (boundp '*out*) *strict* *in-fn*)))

(defparameter *want*
  (list '((:start 2) ((:start 1) ((:start 0) (:end 0 (:s 0) (:f 0))) (:end 1 (:s 1) (:f 1)))
          (:end 2 (:s 2) (:f 2)))
        '(:outer :outer-out nil)
        '(:outer :kept)
        '(nil nil nil)))

(check "main thread" (all-cases) *want*)
(check "worker" (sb-thread:join-thread (sb-thread:make-thread
                                        (lambda () (modus:share (handler-case (all-cases)
                                                                   (error (e) (princ-to-string e)))))))
       *want*)
(format t "~&LET-SPECIALS-NESTED ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
