;;; A closure returned as a SECONDARY value is callable; RANDOM survives a
;;; 3-value return (its seed used to live in the MV buffer); a recovered
;;; hardware fault reaches HANDLER-CASE as a TYPE-ERROR, not a stale condition.
(defun mvc2 () (values 1 (lambda (x) (- 1 x))))
(defun mvc3 () (values 1 2 (lambda (x) (* 2 x))))
(defun three-values () (values 1 "str" #\c))
(defparameter *ok*
  (and (eql (multiple-value-bind (a b) (mvc2) (declare (ignore a)) (funcall b 1)) 0)
       (eql (multiple-value-bind (a b c) (mvc3) (declare (ignore a b)) (funcall c 4)) 8)
       (equalp (multiple-value-bind (a b c) (mvc3) (declare (ignore a b)) (mapcar c '(1 2))) '(2 4))
       (progn (three-values) (integerp (random 20)))
       (progn (warn "leave a stale condition")
              (eq (handler-case (car (the t 5)) (type-error () :te) (condition () :other)) :te))))
(format t "~&mv-closures-and-random: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
