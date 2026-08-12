;;; make-instance initialization-protocol differential battery (task #246)
(defvar *log* nil)
(defun chkf (name thunk want)
  (let ((got (handler-case (funcall thunk) (error (c) :ERROR-SIGNALLED))))
    (format t "~A ~A got=~S want=~S~%"
            (if (equal got want) "PASS" "FAIL") name got want)))

;; --- 1. initialize-instance :after on direct call
(defvar *r1* nil)
(defclass k1 () ())
(defmethod initialize-instance :after ((x k1) &key) (setq *r1* :YES))
(setq *r1* nil) (make-instance 'k1)
(chkf "ii-after-direct" (lambda () *r1*) :YES)
(setq *r1* nil) (apply #'make-instance 'k1 '())
(chkf "ii-after-apply" (lambda () *r1*) :YES)
(setq *r1* nil) (make-instance (find-class 'k1))
(chkf "ii-after-classobj" (lambda () *r1*) :YES)
(setq *r1* nil) (let ((n 'k1)) (make-instance n))
(chkf "ii-after-varname" (lambda () *r1*) :YES)

;; --- 2. :before and :around
(defvar *r2* nil)
(defclass k2 () ())
(defmethod initialize-instance :before ((x k2) &key) (push :before *r2*))
(defmethod initialize-instance :after ((x k2) &key) (push :after *r2*))
(defmethod initialize-instance :around ((x k2) &key)
  (push :around-in *r2*) (call-next-method) (push :around-out *r2*))
(setq *r2* nil) (make-instance 'k2)
(chkf "ii-order" (lambda () (reverse *r2*)) '(:around-in :before :after :around-out))

;; --- 3. shared-initialize :after
(defvar *r3* nil)
(defclass k3 () ())
(defmethod shared-initialize :after ((x k3) sn &key) (setq *r3* :SI))
(setq *r3* nil) (make-instance 'k3)
(chkf "si-after-direct" (lambda () *r3*) :SI)

;; --- 4. initforms still applied (no user methods)
(defclass k4 () ((a :initform 41) (b :initarg :b :initform 7)))
(let ((i (make-instance 'k4)))
  (chkf "initform-a" (lambda () (slot-value i 'a)) 41)
  (chkf "initform-b" (lambda () (slot-value i 'b)) 7))
(let ((i (make-instance 'k4 :b 99)))
  (chkf "initarg-b" (lambda () (slot-value i 'b)) 99)
  (chkf "initform-a2" (lambda () (slot-value i 'a)) 41))

;; --- 5. initforms applied WITH a user init method present
(defvar *r5* nil)
(defclass k5 () ((a :initform 5) (b :initarg :b :initform 6)))
(defmethod initialize-instance :after ((x k5) &key) (setq *r5* :K5))
(let ((i (make-instance 'k5 :b 60)))
  (chkf "k5-initform-a" (lambda () (slot-value i 'a)) 5)
  (chkf "k5-initarg-b" (lambda () (slot-value i 'b)) 60)
  (chkf "k5-ran" (lambda () *r5*) :K5))

;; --- 6. user method sees the initargs
(defvar *r6* nil)
(defclass k6 () ((s :initarg :s)))
(defmethod initialize-instance :after ((x k6) &key s) (setq *r6* s))
(setq *r6* nil) (make-instance 'k6 :s 'hello)
(chkf "ii-sees-initarg" (lambda () *r6*) 'hello)

;; --- 7. user method can set a slot
(defclass k7 () ((v :initarg :v) (d)))
(defmethod initialize-instance :after ((x k7) &key)
  (setf (slot-value x 'd) (* 2 (slot-value x 'v))))
(chkf "ii-derives-slot" (lambda () (slot-value (make-instance 'k7 :v 21) 'd)) 42)

;; --- 8. user PRIMARY method + call-next-method
(defvar *r8* nil)
(defclass k8 () ((a :initform 8)))
(defmethod initialize-instance ((x k8) &rest ia)
  (setq *r8* :primary) (call-next-method))
(let ((i (make-instance 'k8)))
  (chkf "ii-primary-ran" (lambda () *r8*) :primary)
  (chkf "ii-primary-cnm-initform" (lambda () (slot-value i 'a)) 8))

;; --- 9. default-initargs still work
(defclass k9 () ((a :initarg :a)) (:default-initargs :a 90))
(chkf "default-initargs" (lambda () (slot-value (make-instance 'k9) 'a)) 90)

;; --- 10. invalid initarg still rejected / valid accepted
(defclass ka () ((a :initarg :a)))
(chkf "valid-initarg-ok" (lambda () (handler-case (progn (make-instance 'ka :a 1) :ok) (error (c) :err))) :ok)
(chkf "invalid-initarg-rejected" (lambda () (handler-case (progn (make-instance 'ka :zz 1) :ok) (error (c) :err))) :err)
(chkf "odd-plist-rejected" (lambda () (handler-case (progn (make-instance 'ka :a) :ok) (error (c) :err))) :err)
;; :allow-other-keys
(chkf "aok-accepts" (lambda () (handler-case (progn (make-instance 'ka :zz 1 :allow-other-keys t) :ok)
       (error (c) :err))) :ok)
;; a method's &key makes that key a valid initarg (CLHS 7.1.2)
(defclass kb () ((a :initarg :a)))
(defmethod initialize-instance :after ((x kb) &key extra) extra)
(chkf "method-key-is-valid-initarg" (lambda () (handler-case (progn (make-instance 'kb :extra 3) :ok) (error (c) :err))) :ok)
(chkf "method-key-nonmatching-still-rejected" (lambda () (handler-case (progn (make-instance 'kb :nope 3) :ok) (error (c) :err))) :err)

;; --- 11. inheritance: method on parent runs for a subclass instance
(defvar *rc* nil)
(defclass kp () ())
(defclass kc (kp) ())
(defmethod initialize-instance :after ((x kp) &key) (setq *rc* :parent))
(setq *rc* nil) (make-instance 'kc)
(chkf "inherited-ii-after" (lambda () *rc*) :parent)

;; --- 12. accessors + reinitialize-instance unaffected
(defclass kd () ((a :initarg :a :accessor kd-a :initform 1)))
(let ((i (make-instance 'kd)))
  (chkf "accessor-read" (lambda () (kd-a i)) 1)
  (reinitialize-instance i :a 5)
  (chkf "reinit" (lambda () (kd-a i)) 5))

;; --- 13. no-method fast path still correct on a class with no methods
(defclass ke () ((a :initform (+ 1 2))))
(chkf "initform-thunk" (lambda () (slot-value (make-instance 'ke) 'a)) 3)

(format t "BATTERY-DONE~%")
