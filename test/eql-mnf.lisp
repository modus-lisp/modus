;;; most-negative-fixnum (-2^62) has two representations -- the tower keeps
;;; it as a bignum, native fixnum arithmetic can produce it as a fixnum --
;;; and they must be EQL.
(defparameter *a* (ash (- (expt 2 100)) -38))
(defparameter *b* (floor (* (- (expt 2 100)) (expt 2 -38))))
(defparameter *ok*
  (and (= *a* *b*) (eql *a* *b*) (eql *b* *a*)
       (eql (ash (- (expt 2 100)) -38) most-negative-fixnum)
       (not (eql *a* (1+ most-negative-fixnum)))))
(format t "~&eql-mnf: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
