;;; A cons shaped like the old array wrappers -- (fixnum . vector),
;;; ((a . b) . vector) -- is an ordinary CONS in the CLI: not an array, and
;;; printing / measuring it treats it as a list.
(defparameter *ok*
  (and (not (arrayp (cons 5 (vector 1 2))))
       (not (vectorp (cons 5 (vector 1 2))))
       (not (stringp (cons 1 "ab")))
       (eq (handler-case (length (cons 5 (vector 1 2 3))) (type-error () :type-error)) :type-error)
       (equal (prin1-to-string (cons 5 (vector 1 2))) "(5 . #(1 2))")
       (equal (prin1-to-string (list (cons (cons 1 2) (vector 3)) (cons 7 "xy"))) "(((1 . 2) . #(3)) (7 . \"xy\"))")
       (= (length (make-array 5 :fill-pointer 2)) 2)
       (arrayp (make-array 3 :adjustable t))))
(format t "~&cons-not-array: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
