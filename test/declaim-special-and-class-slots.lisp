;;; (declaim (special x)) / (proclaim '(special x)) make later LET bindings
;;; dynamic; DEFCLASS :allocation :class slots are shared by all instances.
(declaim (special *dsc-a*))
(proclaim '(special *dsc-b*))
(defun dsc-read-a () (symbol-value '*dsc-a*))
(defclass dsc-c () ((own :initform 1) (shared :allocation :class)))
(defparameter *ok*
  (and (eql (let ((*dsc-a* 1)) (dsc-read-a)) 1)
       (eql (let* ((*dsc-b* 2)) (funcall (lambda () *dsc-b*))) 2)
       (let ((x (make-instance 'dsc-c)) (y (make-instance 'dsc-c)))
         (setf (slot-value x 'shared) :s)
         (and (eq (slot-value y 'shared) :s)
              (progn (setf (slot-value x 'own) 9) (eql (slot-value y 'own) 1))))))
(format t "~&declaim-special-and-class-slots: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
