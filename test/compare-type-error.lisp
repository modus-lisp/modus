;;;; test/compare-type-error.lisp -- the numeric comparison operators signal
;;;; TYPE-ERROR for a non-number (CLHS 12.2), compiled and functional.
;;;;
;;;;   ./modus --script test/compare-type-error.lisp
;;;;
;;;; They used to answer NIL: the compiler's slow path and the functional
;;;; < = ... called helpers whose fall-through arm is (t nil).  So a library
;;;; comparing against an uninitialised NIL constant looped forever instead of
;;;; failing (cl-nostr's GENERATE-KEYPAIR against secp256k1-fast's
;;;; *SECP256K1-N* before SECP-INIT).  Values that ARE numbers must compare
;;;; exactly as before; that half is the control.

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name got want)
  (if (equal got want)
      (incf *pass*)
      (progn (incf *fail*) (format t "~&FAIL ~A: got ~S want ~S~%" name got want))))
(defmacro outcome (form)
  `(handler-case (list :value ,form)
     (type-error (c) (list :type-error (type-error-expected-type c)))
     (error (c) (list :other-error (type-of c)))))

(defvar *nil* nil)
(defvar *big* (expt 2 255))
(defvar *str* "x")

;; compiled two-argument forms, every operator, bad operand on either side
(defun lt-a (a b) (< a b))
(defun gt-a (a b) (> a b))
(defun le-a (a b) (<= a b))
(defun ge-a (a b) (>= a b))
(defun eq-a (a b) (= a b))
(dolist (f (list #'lt-a #'gt-a #'le-a #'ge-a))
  (check "compiled real nil right" (car (outcome (funcall f *big* *nil*))) :type-error)
  (check "compiled real nil left"  (car (outcome (funcall f *nil* 3))) :type-error)
  (check "compiled real string"    (car (outcome (funcall f 3 *str*))) :type-error)
  (check "compiled real char"      (car (outcome (funcall f 1.5 #\a))) :type-error)
  (check "compiled real complex"   (car (outcome (funcall f 1 #c(1 2)))) :type-error))
(check "compiled = nil"     (car (outcome (eq-a *big* *nil*))) :type-error)
(check "compiled = string"  (car (outcome (eq-a 3 *str*))) :type-error)
(check "compiled = complex is fine" (outcome (eq-a #c(1 2) #c(1 2))) '(:value t))
(check "expected type real"   (outcome (lt-a 3 *nil*)) '(:type-error real))
(check "expected type number" (outcome (eq-a 3 *nil*)) '(:type-error number))

;; one argument: still evaluated and type-checked
(defun lt1 (a) (< a))
(defun eq1 (a) (= a))
(check "(< x) bad"  (car (outcome (lt1 *nil*))) :type-error)
(check "(= x) bad"  (car (outcome (eq1 *str*))) :type-error)
(check "(< x) good" (outcome (lt1 5)) '(:value t))

;; three arguments
(defun lt3 (a b c) (< a b c))
(check "(< 1 2 nil)" (car (outcome (lt3 1 2 *nil*))) :type-error)

;; functional entry points
(dolist (op (list #'< #'> #'<= #'>= #'= #'/=))
  (check "apply bad"   (car (outcome (apply op (list 1 *nil*)))) :type-error)
  (check "funcall bad" (car (outcome (funcall op *str* 2))) :type-error)
  (check "funcall one bad" (car (outcome (funcall op *nil*))) :type-error))

;; THE CONTROL: numbers compare exactly as before, on every path
(dolist (case (list (list 1 2) (list 2 1) (list 2 2) (list *big* (1+ *big*)) (list (1+ *big*) *big*)
                    (list 1/3 1/2) (list 1.5 2) (list 2 1.5) (list -1.5d0 -1.5d0) (list (- *big*) 0)))
  (destructuring-bind (a b) case
    (check "< control"  (lt-a a b) (cl:< a b))
    (check "> control"  (gt-a a b) (cl:> a b))
    (check "<= control" (le-a a b) (cl:<= a b))
    (check ">= control" (ge-a a b) (cl:>= a b))
    (check "= control"  (eq-a a b) (cl:= a b))
    (check "funcall < control" (funcall #'< a b) (lt-a a b))
    (check "apply = control" (apply #'= (list a b)) (eq-a a b))))
(check "apply < chain" (apply #'< (list 1 2 3 4)) t)
(check "apply < chain false" (apply #'< (list 1 3 2)) nil)
(check "/= distinct" (funcall #'/= 1 2 3) t)
(check "/= dup" (funcall #'/= 1 2 1) nil)

(format t "~&compare-type-error: ~D passed, ~D failed~%" *pass* *fail*)
(sys-exit (if (zerop *fail*) 0 1))
