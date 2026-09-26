;;; BIT / SBIT at any rank (zero-rank included) and their SETFs; an integer
;;; division by zero signals DIVISION-BY-ZERO (it used to be a CPU trap);
;;; a restart frame is removed when its body exits non-locally.
(defparameter *ok*
  (and (equal (let ((a (make-array nil :element-type 'bit :initial-element 0)))
                (list (aref a) (bit a) (setf (bit a) 1) (aref a) (bit a) (sbit a)))
              '(0 0 1 1 1 1))
       (equal (let ((m (make-array '(2 2) :element-type 'bit :initial-element 0)))
                (setf (bit m 1 0) 1) (setf (sbit m 0 1) 1)
                (list (bit m 1 0) (sbit m 0 1) (bit m 0 0)))
              '(1 1 0))
       (eq (handler-case (/ 0) (division-by-zero () :dz)) :dz)
       (eq (handler-case (/ 5 0) (division-by-zero () :dz)) :dz)
       (eql (/ 6 3) 2)
       (progn (block out (handler-bind ((warning (lambda (c) (declare (ignore c)) (return-from out nil))))
                           (warn "x")))
              (null (find-restart 'muffle-warning)))))
(format t "~&bit-and-divide: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
