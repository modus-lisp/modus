;;;; generic-arith-type-errors.lisp -- + - * / (and 1+, 1-, unary -) on a NON-NUMBER
;;;; signal TYPE-ERROR whose datum is the offending operand, whatever number sits on the
;;;; other side (fixnum, bignum, ratio, float, complex).  The generic slow paths fell back
;;;; to raw fixnum arithmetic on the tagged word for anything they did not recognise but
;;;; NIL and characters, so (+ x 1) on a symbol returned a garbage object.  Number
;;;; arithmetic through the same paths is checked too.  Expected results are SBCL's.
;;;; Prints "GENERIC-ARITH-TYPE-ERRORS PASS" when every check holds.

(defvar *fails* 0)

(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~a~%" name)
      (progn (incf *fails*)
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

;; What calling THUNK did: (:type-error DATUM) or (:value V).
(defun outcome (thunk)
  (handler-case (list :value (funcall thunk))
    (type-error (e) (list :type-error (type-error-datum e)))
    (error (e) (list :other-error (type-of e)))))

;; Not inlinable by constant folding: operands arrive as parameters.
(defun add (a b) (+ a b))
(defun sub (a b) (- a b))
(defun mul (a b) (* a b))
(defun div (a b) (/ a b))
(defun inc (x) (+ x 1))
(defun one+ (x) (1+ x))
(defun one- (x) (1- x))
(defun neg (x) (- x))
(defun recip (x) (/ x))
(defun add3 (a b c) (+ a b c))

(defvar *non-numbers* (list 'a "s" (list 1 2) nil #\a (vector 1 2) :key))
(defvar *numbers* (list 7 (expt 2 70) 1/3 2.5 2.5d0 #c(1 2)))

(dolist (nn *non-numbers*)
  (check (format nil "inc ~s" nn) (outcome (lambda () (inc nn))) (list :type-error nn))
  (check (format nil "1+ ~s" nn) (outcome (lambda () (one+ nn))) (list :type-error nn))
  (check (format nil "1- ~s" nn) (outcome (lambda () (one- nn))) (list :type-error nn))
  (check (format nil "unary - ~s" nn) (outcome (lambda () (neg nn))) (list :type-error nn))
  (check (format nil "unary / ~s" nn) (outcome (lambda () (recip nn))) (list :type-error nn))
  (dolist (n *numbers*)
    (dolist (op (list (cons "+" #'add) (cons "-" #'sub) (cons "*" #'mul) (cons "/" #'div)))
      (check (format nil "~a ~s ~s" (car op) n nn)
             (outcome (lambda () (funcall (cdr op) n nn))) (list :type-error nn))
      (check (format nil "~a ~s ~s" (car op) nn n)
             (outcome (lambda () (funcall (cdr op) nn n))) (list :type-error nn)))))

;; two non-numbers: the first is the datum
(check "+ a b" (outcome (lambda () (add 'a 'b))) '(:type-error a))
(check "* nil c" (outcome (lambda () (mul nil #\c))) '(:type-error nil))
;; a non-number in the middle of an n-ary chain
(check "+ 1 2 b" (outcome (lambda () (add3 1 2 'b))) '(:type-error b))
(check "+ 1 b 2" (outcome (lambda () (add3 1 'b 2))) '(:type-error b))
;; through #'+ and friends
(check "apply #'+" (outcome (lambda () (apply #'+ (list 1 'a)))) '(:type-error a))
(check "funcall #'*" (outcome (lambda () (funcall #'* 3 "x"))) '(:type-error "x"))
(check "reduce #'-" (outcome (lambda () (reduce #'- (list 10 1 'z)))) '(:type-error z))
;; a handler sees the condition as a TYPE-ERROR expecting a number
(check "expected-type number"
       (handler-case (progn (inc 'q) :none)
         (type-error (e) (and (subtypep 'integer (type-error-expected-type e)) :number)))
       :number)

;; numbers through the same slow paths are unchanged
(check "fixnum + fixnum" (add 2 3) 5)
(check "fixnum + bignum" (add 1 (expt 2 70)) (+ 1 (expt 2 70)))
(check "ratio + fixnum" (add 1/3 2) 7/3)
(check "ratio * ratio" (mul 2/3 3/4) 1/2)
(check "float + ratio" (add 0.5d0 1/2) 1.0d0)
(check "float * bignum" (mul 1.0d0 (expt 2 70)) (* 1.0d0 (expt 2 70)))
(check "complex - fixnum" (sub #c(3 4) 1) #c(2 4))
(check "complex * complex" (mul #c(1 2) #c(3 4)) #c(-5 10))
(check "fixnum / fixnum" (div 6 4) 3/2)
(check "fixnum / float" (div 1 2.0d0) 0.5d0)
(check "1+ bignum" (one+ (expt 2 70)) (+ 1 (expt 2 70)))
(check "unary - ratio" (neg 1/3) -1/3)
(check "unary / ratio" (recip -2/3) -3/2)
(check "unary / single" (recip 2.5) 0.4)
(check "unary / double" (recip 4d0) 0.25d0)
(check "unary / single type" (type-of (recip 2.0)) 'single-float)
(check "single / ratio" (div 1.5 1/2) 3.0)
(check "ratio / double" (div 1/2 0.25d0) 2.0d0)
(check "ratio / fixnum" (div 1/2 3) 1/6)
(check "single / bignum type" (type-of (div 1.0 (expt 2 70))) 'single-float)
(check "overflow to bignum" (add most-positive-fixnum 1) (+ most-positive-fixnum 1))

(format t "~&GENERIC-ARITH-TYPE-ERRORS ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
