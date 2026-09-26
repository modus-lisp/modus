;;; DEFMETHOD on a name that was an ordinary built-in function must not take
;;; the built-in away from arguments no user method specialises.
(defclass dkb-a () ())
(defclass dkb-b () ())
(defmethod class-name ((x dkb-b)) 'custom)
(defun dkb-plain (x) (list :plain x))
(defmethod dkb-plain ((x integer)) (list :int x))
(defmethod documentation ((x dkb-b) (dt (eql 't))) "doc-b")
(defparameter *ok*
  (and (eq (class-name (find-class 'dkb-a)) 'dkb-a)
       (eq (class-name (make-instance 'dkb-b)) 'custom)
       (equal (dkb-plain 3) '(:int 3))
       (equal (dkb-plain "s") '(:plain "s"))
       (equal (documentation (make-instance 'dkb-b) t) "doc-b")
       (eq (handler-case (progn (documentation 'car 'function) :ok) (error () :err)) :ok)))
(format t "~&defmethod-keeps-builtin: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
