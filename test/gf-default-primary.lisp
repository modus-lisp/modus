;;; A user method on SHARED-INITIALIZE for ONE class must not take the
;;; system (standard-object) method away from every other class.
(defclass gdp-a () ((x :initarg :x)))
(defclass gdp-b () ((x :initarg :x)))
(defmethod shared-initialize :after ((o gdp-b) slot-names &rest args)
  (declare (ignore slot-names args)) nil)
(defparameter *ok*
  (and (let ((o (allocate-instance (find-class 'gdp-a))))
         (shared-initialize o nil :x 7)
         (eql (slot-value o 'x) 7))
       (eql (slot-value (make-instance 'gdp-a :x 3) 'x) 3)
       (eql (slot-value (make-instance 'gdp-b :x 4) 'x) 4)))
(format t "~&gf-default-primary: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
