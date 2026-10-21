;;;; test/clos-subclass-layout.lisp -- a subclass's slots follow its
;;;; superclasses' definitions (cl-clos.lisp %CLOS-PROPAGATE-TO-SUBCLASSES).
;;;;
;;;;   ./modus --script test/clos-subclass-layout.lisp      (SBCL runs it too)
;;;;
;;;; A class's layout was computed once, when it was defined, from its
;;;; superclasses as they were then.  A subclass defined before its superclass
;;;; -- an ordinary file order -- never got the superclass's slots, and one
;;;; whose superclass was redefined kept the old layout.  And an instance made
;;;; before its class was redefined kept its OLD cells while slot access used
;;;; the NEW indices (CLHS 4.3.6 updates it; see INSTANCES AND THEIR LAYOUT).

(defvar *fails* 0)
(defun chk (name got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "FAIL") name)
  (unless (equal got want)
    (format t "      got ~s want ~s~%" got want)
    (setq *fails* (+ *fails* 1))))
(defun try (f) (handler-case (funcall f) (error () :error)))
(defun slots (o names)
  (mapcar (lambda (n) (if (slot-boundp o n) (slot-value o n) :unbound)) names))

;;; A forward reference.
(defclass fb (fa) ((z :initarg :z :initform :z0)))
(defclass fc (fb) ((w :initform :w0)))
(defclass fa () ((x :initarg :x :initform :x0)))
(chk "a subclass defined first has the superclass's slots"
     (try (lambda () (slots (make-instance 'fb :x 1) '(x z)))) '(1 :z0))
(chk "and so does its own subclass"
     (try (lambda () (slots (make-instance 'fc) '(x z w)))) '(:x0 :z0 :w0))
(chk "with the superclass's initarg"
     (try (lambda () (slots (make-instance 'fc :x 7 :z 8) '(x z)))) '(7 8))
(chk "and TYPEP through it" (typep (make-instance 'fc) 'fa) t)

;;; Redefinition.
(defclass ra () ((x :initarg :x :initform :x0)))
(defclass rb (ra) ((z :initform :z0)))
(defclass rc (rb) ())
(defvar *old-rb* (make-instance 'rb :x 1))
(make-instance 'rc)                            ; a plan in place for RC
(defclass ra () ((w :initform :w0) (x :initarg :x :initform :x0)))
(chk "a slot added to the superclass reaches new subclass instances"
     (try (lambda () (slots (make-instance 'rb :x 2) '(w x z)))) '(:w0 2 :z0))
(chk "two levels down"
     (try (lambda () (slots (make-instance 'rc) '(w x z)))) '(:w0 :x0 :z0))
(defclass ra () ((x :initarg :x :initform :x1)))
(chk "a slot removed again is gone"
     (try (lambda () (slot-exists-p (make-instance 'rb) 'w))) nil)
(chk "and a changed initform is the new one"
     (try (lambda () (slots (make-instance 'rc) '(x)))) '(:x1))
(defclass rmix () ((m :initform :m0)))
(defclass ra (rmix) ((x :initarg :x :initform :x1)))
(chk "a superclass gaining a superclass"
     (try (lambda () (slots (make-instance 'rc) '(m x z)))) '(:m0 :x1 :z0))
(chk "and the CPL with it" (typep (make-instance 'rc) 'rmix) t)
(chk "redefining a class unchanged keeps its subclasses"
     (progn (defclass rb (ra) ((z :initform :z0)))
            (try (lambda () (slots (make-instance 'rc) '(m x z)))))
     '(:m0 :x1 :z0))

;;; CLHS 4.3.6: instances made BEFORE a redefinition are updated.
(chk "an instance made before keeps its values (CLHS 4.3.6)"
     (try (lambda () (slot-value *old-rb* 'x))) 1)
(chk "and gets the added slot, initialised"
     (try (lambda () (slot-value *old-rb* 'm))) :m0)

(defclass ua () ((p :initarg :p) (q :initarg :q) (r :initform :r0)))
(defvar *u1* (make-instance 'ua :p 1 :q 2))
(defvar *u1-same* *u1*)
(defvar *seen* nil)
(defmethod update-instance-for-redefined-class :after ((o ua) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (push (list added discarded plist) *seen*))
(defclass ua () ((s :initform :s0) (q :initarg :q) (p :initarg :p)))   ; R gone, S new, reordered
(chk "a reordered slot reads its own value" (try (lambda () (list (slot-value *u1* 'p) (slot-value *u1* 'q)))) '(1 2))
(chk "an added slot has its initform" (try (lambda () (slot-value *u1* 's))) :s0)
(chk "a removed slot is gone" (try (lambda () (slot-exists-p *u1* 'r))) nil)
(chk "UPDATE-INSTANCE-FOR-REDEFINED-CLASS saw added, discarded and their values"
     *seen* '(((s) (r) (r :r0))))
(chk "and ran once" (progn (slot-value *u1* 'p) (length *seen*)) 1)
(chk "the instance is still the same object" (eq *u1* *u1-same*) t)
(chk "SETF of a slot on an updated instance"
     (try (lambda () (setf (slot-value *u1* 'q) 5) (slot-value *u1* 'q))) 5)

(defclass ob () ((v :initarg :v)))
(defvar *ob1* (make-instance 'ob :v 9))
(defvar *ob-updates* 0)
(defmethod update-instance-for-redefined-class :before ((o ob) a d p &rest i)
  (declare (ignore a d p i))
  (incf *ob-updates*))
(make-instances-obsolete 'ob)
(chk "MAKE-INSTANCES-OBSOLETE: the next access updates"
     (list (try (lambda () (slot-value *ob1* 'v))) *ob-updates*) '(9 1))

(defclass small () ((a :initarg :a)))
(defclass big () ((a :initarg :a) (b :initform :b0) (c :initform :c0) (d :initform :d0)))
(defvar *sm* (make-instance 'small :a 1))
(change-class *sm* 'big)
(chk "CHANGE-CLASS to a class with more slots keeps all of them"
     (try (lambda () (slots *sm* '(a b c d)))) '(1 :b0 :c0 :d0))

(format t "~&CLOS-SUBCLASS-LAYOUT: ~a failed~%" *fails*)
#+sbcl (sb-ext:exit :code (if (zerop *fails*) 0 1))
#-sbcl (sys-exit (if (zerop *fails*) 0 1))
