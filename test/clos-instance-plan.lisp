;;;; test/clos-instance-plan.lisp -- MAKE-INSTANCE through the per-class
;;;; instantiation plan (cl-clos.lisp %CLOS-INSTANCE-PLAN) keeps CLHS 7.1.
;;;;
;;;;   ./modus --script test/clos-instance-plan.lisp      (SBCL runs it too)
;;;;
;;;; A plan caches, per class, which slots each initarg fills, the default
;;;; initargs, the initforms and the initarg key lists already validated.  So
;;;; the checks below are mostly about what must NOT be cached: values that
;;;; are re-evaluated per call, and every change that must make a plan stale
;;;; (a method added or removed, a class redefined).  Each shape is run twice,
;;;; so the second call is the cached one.

(defvar *fails* 0)
(defun chk (name got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "FAIL") name)
  (unless (equal got want)
    (format t "      got ~s want ~s~%" got want)
    (setq *fails* (+ *fails* 1))))
(defun slots (o names)
  (mapcar (lambda (n) (if (slot-boundp o n) (slot-value o n) :unbound)) names))
(defun twice (f) (funcall f) (funcall f))

(defvar *count* 0)
(defclass base () ((a :initarg :a :initarg :aa) (b :initarg :b :initform (incf *count*))))
(defclass derived (base)
  ((c :initarg :c :initform :c-default) (d))
  (:default-initargs :a (list :a-default)))
(defclass multi () ((x :initarg :k) (y :initarg :k)))

(chk "initargs fill their slots"
     (twice (lambda () (slots (make-instance 'base :a 1 :b 2) '(a b)))) '(1 2))
(chk "a second initarg for a slot"
     (twice (lambda () (slots (make-instance 'base :aa 7) '(a)))) '(7))
(chk "leftmost initarg wins"
     (twice (lambda () (slots (make-instance 'base :a 1 :aa 2 :a 3) '(a)))) '(1))
(chk "an initform runs per instance, only when the slot is unset"
     (progn (setq *count* 0)
            (list (slots (make-instance 'base) '(b)) (slots (make-instance 'base) '(b))
                  (slots (make-instance 'base :b :x) '(b)) *count*))
     '((1) (2) (:x) 2))
(chk "a slot with neither is unbound"
     (twice (lambda () (slots (make-instance 'derived) '(d)))) '(:unbound))
(chk "a default initarg fills its slot, inherited initforms run"
     (twice (lambda () (slots (make-instance 'derived) '(a c)))) '((:a-default) :c-default))
(chk "a default initarg is evaluated per instance"
     (let ((o1 (make-instance 'derived)) (o2 (make-instance 'derived)))
       (eq (slot-value o1 'a) (slot-value o2 'a)))
     nil)
(chk "a supplied initarg suppresses the default"
     (twice (lambda () (slots (make-instance 'derived :a 5 :c 6) '(a c)))) '(5 6))
(chk "so does the other initarg of the same slot"
     (twice (lambda () (slots (make-instance 'derived :aa 9) '(a)))) '(9))
(chk "one initarg fills two slots"
     (twice (lambda () (slots (make-instance 'multi :k 4) '(x y)))) '(4 4))
(chk "a class object designator"
     (twice (lambda () (slots (make-instance (find-class 'base) :a 8) '(a)))) '(8))

(defun err-of (thunk)                ; an unknown initarg: SBCL signals a
  (handler-case (progn (funcall thunk) :no-error) ; PROGRAM-ERROR, modus ERROR
    (error () :error)))
(defun perr-of (thunk)
  (handler-case (progn (funcall thunk) :no-error)
    (program-error () :program-error)
    (error () :error)))
(chk "an unknown initarg is an error, every time"
     (list (err-of (lambda () (make-instance 'base :zz 1)))
           (err-of (lambda () (make-instance 'base :zz 1))))
     '(:error :error))
(chk "even after the same keys were valid without it"
     (progn (make-instance 'base :a 1) (err-of (lambda () (make-instance 'base :a 1 :zz 2))))
     :error)
(chk "an odd initarg list is a PROGRAM-ERROR"
     (twice (lambda () (perr-of (lambda () (make-instance 'base :a))))) :program-error)
(chk ":ALLOW-OTHER-KEYS T admits it"
     (twice (lambda () (err-of (lambda () (make-instance 'base :zz 1 :allow-other-keys t)))))
     :no-error)

;;; Methods change what MAKE-INSTANCE does: the plan must go stale.
(defvar *log* nil)
(make-instance 'derived :c 1)                       ; plan in place
(defmethod initialize-instance :after ((o derived) &key extra)
  (push (list :after extra) *log*))
(chk "an :AFTER method added later runs"
     (progn (setq *log* nil) (make-instance 'derived :c 1) *log*) '((:after nil)))
(chk "and its &KEY is a valid initarg"
     (progn (setq *log* nil) (make-instance 'derived :extra 3) *log*) '((:after 3)))
(chk "a subclass sees an inherited method"
     (progn (defclass derived2 (derived) ())
            (setq *log* nil) (make-instance 'derived2) (make-instance 'derived2) (length *log*))
     2)
(let ((m (find-method (ensure-generic-function 'initialize-instance) '(:after) (list (find-class 'derived)))))
  (remove-method (ensure-generic-function 'initialize-instance) m))
(chk "removed again, it does not run"
     (progn (setq *log* nil) (make-instance 'derived :c 1) (make-instance 'derived :c 1) *log*)
     nil)
(chk "and its &KEY is invalid again"
     (err-of (lambda () (make-instance 'derived :extra 3))) :error)
(defmethod shared-initialize :before ((o base) slots &rest r)
  (declare (ignore slots r))
  (push :si *log*))
(chk "a SHARED-INITIALIZE method on a superclass runs"
     (progn (setq *log* nil) (make-instance 'derived) *log*) '(:si))
(let ((m (find-method (ensure-generic-function 'shared-initialize) '(:before) (list (find-class 'base) (find-class t)))))
  (remove-method (ensure-generic-function 'shared-initialize) m))

;;; Redefinition.
(make-instance 'base :a 1)
(defclass base () ((a :initarg :a) (b :initarg :b :initform :new-b) (e :initarg :e :initform :e)))
(chk "a redefined class has its new slots and initforms"
     (twice (lambda () (slots (make-instance 'base :a 1) '(a b e)))) '(1 :new-b :e))
(chk "a dropped initarg is invalid"
     (err-of (lambda () (make-instance 'base :aa 1))) :error)
(chk "a subclass picks the change up"
     (twice (lambda () (slots (make-instance 'derived :e 2) '(a e c)))) '((:a-default) 2 :c-default))

;;; Shapes the plan declines, still right.
(defclass shared () ((s :allocation :class :initarg :s :initform :shared) (i :initarg :i)))
(chk "a class-allocated slot"
     (twice (lambda () (let ((o (make-instance 'shared :i 1))) (slots o '(s i))))) '(:shared 1))
(defclass lenient () ((p :initarg :p)) (:default-initargs :allow-other-keys t))
(chk "an :ALLOW-OTHER-KEYS default initarg"
     (twice (lambda () (err-of (lambda () (make-instance 'lenient :anything 1))))) :no-error)

(format t "~&CLOS-INSTANCE-PLAN: ~a failed~%" *fails*)
#+sbcl (sb-ext:exit :code (if (zerop *fails*) 0 1))
#-sbcl (sys-exit (if (zerop *fails*) 0 1))
