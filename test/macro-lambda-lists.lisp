;;; Destructuring patterns in &optional / &key of a macro lambda list, an
;;; explicit ((:keyword var)) key, and SETF of a MACROLET-defined place.
(defmacro mll1 (x &optional ((y z) '(2 3))) `(quote (,x ,y ,z)))
(defmacro mll2 (&key ((:a b))) `(quote ,b))
(defmacro mll3 (&key ((:a (b c)) '(3 4) a-p)) `(quote (,(not (null a-p)) ,c ,b)))
(defparameter *ok*
  (and (equal (list (mll1 a) (mll1 a (b c))) '((a 2 3) (a b c)))
       (equal (list (mll2) (mll2 :a x)) '(nil x))
       (equal (list (mll3 :a (1 2)) (mll3)) '((t 2 1) (nil 4 3)))
       (equal (destructuring-bind (&key ((:k (p q)))) '(:k (1 2)) (list p q)) '(1 2))
       (equal (let ((y (list 1 2))) (macrolet ((%m (x) `(car ,x))) (setf (%m y) 6)) y) '(6 2))))
(format t "~&macro-lambda-lists: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
