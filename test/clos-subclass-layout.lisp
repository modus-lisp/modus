;;;; test/clos-subclass-layout.lisp -- a subclass's slots follow its
;;;; superclasses' definitions (cl-clos.lisp %CLOS-PROPAGATE-TO-SUBCLASSES).
;;;;
;;;;   ./modus --script test/clos-subclass-layout.lisp      (SBCL runs it too)
;;;;
;;;; A class's layout was computed once, when it was defined, from its
;;;; superclasses as they were then.  A subclass defined before its superclass
;;;; -- an ordinary file order -- never got the superclass's slots, and one
;;;; whose superclass was redefined kept the old layout.

(defvar *fails* 0)
(defvar *known* 0)
(defun chk (name got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "FAIL") name)
  (unless (equal got want)
    (format t "      got ~s want ~s~%" got want)
    (setq *fails* (+ *fails* 1))))
;;; Older and not addressed here: CLHS 4.3.6 updates instances made BEFORE a
;;; redefinition.  modus keeps their old layout.  Reported, not counted.
(defun known (name got want)
  (format t "~&  ~a ~a~%" (if (equal got want) "ok  " "KNOWN") name)
  (unless (equal got want) (setq *known* (+ *known* 1))))
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

(known "an instance made before keeps its values (CLHS 4.3.6)"
       (try (lambda () (slot-value *old-rb* 'x))) 1)
(known "and gets the added slot"
       (try (lambda () (slot-value *old-rb* 'm))) :m0)

(format t "~&CLOS-SUBCLASS-LAYOUT: ~a failed (~a known)~%" *fails* *known*)
#+sbcl (sb-ext:exit :code (if (zerop *fails*) 0 1))
#-sbcl (sys-exit (if (zerop *fails*) 0 1))
