;;; Regression: nested backquote keeps its levels (7a9a87b flattened the
;;; compiler's expander: `(deftype ,s (&optional (x 14)) `(integer 0 ,x))
;;; unquoted X at the OUTER level — upstream DEFTYPE.10-13, MISC.323,
;;; STRUCT-TEST-*/5), and unary minus of a float zero is NEGATIVE zero
;;; (upstream CONJUGATE.3, READ-FLOAT.1).
(defvar *fails* 0)
(defmacro chk (name form)
  `(let ((v (handler-case ,form (error (c) (list :error c)))))
     (unless (eq v t) (setq *fails* (+ *fails* 1)) (format t "FAIL ~A => ~S~%" ,name v))))

;; nested backquote, evaluated at runtime
(chk "deftype-nested"
     (let* ((sym (gensym))
            (form `(deftype ,sym (&optional (x 14)) `(integer 0 ,x))))
       (and (eq (eval form) sym)
            (eq (typep 3 `(,sym)) t)
            (eq (typep 15 `(,sym)) nil)
            (eq (typep 15 `(,sym 20)) t))))
;; `,',x tunnelling
(chk "tunnel"
     (let* ((v 'abc)
            (form `(defmacro zz-tunnel () `',',v)))
       (eval form)
       (eq (eval '(zz-tunnel)) 'abc)))
;; ,,x — inner comma evaluated at inner level
(chk "double-comma"
     (let* ((v 5)
            (form `(let ((q 6)) `(list ,q ,,v))))
       (equal (eval (eval form)) '(6 5))))
;; splice inside a quoted subform of a backquote (MISC.323 shape)
(chk "splice-in-quote"
     (let* ((tail '(:from-end t))
            (form `(lambda () (eval '(reduce #'logior (vector (reduce #'logand (vector 0 0) ,@tail) 0) ,@tail)))))
       (eql (funcall (compile nil form)) 0)))
;; nested splice stays data at the outer level
(chk "nested-splice-is-data"
     (let* ((inits '(1 2))
            (form `(defmacro zz-mk (&rest r) `(list ,@r 9))))
       (eval form)
       (equal (eval `(zz-mk ,@inits)) '(1 2 9))))

;; dotted comma tail: `(a . ,b) reads as (A COMMA B) in the list representation
(chk "dotted-tail"
     (let ((tail '(:from-end t)))
       (and (equal (eval (read-from-string "(let ((tail '(3 4))) `(1 2 . ,tail))")) '(1 2 3 4))
            (eql (funcall (compile nil `(lambda () (eval '(reduce #'logior (vector (reduce #'logand (vector 0 0) . ,tail) 0) . ,tail))))) 0))))
(chk "dotted-tail-nested"
     (equal (eval (read-from-string "(let ((inits '(1 2))) (defmacro zz-mk4 (&rest r) `(list . ,r)) (eval `(zz-mk4 . ,inits)))")) '(1 2)))

;; imagpart of a negative float is -0.0 (CLHS: (* 0 x))
(chk "imagpart-negzero" (and (eql (imagpart -79916.61) -0.0) (eql (imagpart 2.5d0) 0.0d0) (eql (imagpart 3) 0)))

;; negative zero
(chk "neg-single" (and (eql (- 0.0) -0.0) (not (eql (- 0.0) 0.0)) (eql (- 1.5) -1.5)))
(chk "neg-double" (and (eql (- 0.0d0) -0.0d0) (eql (- 2.5d0) -2.5d0)))
(chk "neg-var" (let ((z 0.0) (d 0.0d0) (n 7) (r 1/2)) (and (eql (- z) -0.0) (eql (- d) -0.0d0) (eql (- n) -7) (eql (- r) -1/2))))
(chk "neg-apply" (and (eql (apply #'- '(0.0)) -0.0) (eql (funcall #'- 3) -3) (eql (- 0 0.0) 0.0)))
(chk "conjugate" (and (eql (conjugate #c(0.0 0.0)) #c(0.0 -0.0)) (eql (conjugate #c(1 2)) #c(1 -2))))
(chk "abs" (and (eql (abs -0.0) 0.0) (eql (abs -3.5) 3.5) (eql (abs -3.5d0) 3.5d0)))

(format t "~&nested-backquote-and-negzero: ~A~%" (if (zerop *fails*) "PASS" (format nil "~D FAIL" *fails*)))
(sys-exit (if (zerop *fails*) 0 1))
