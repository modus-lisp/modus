;;;; bitop-literal-args.lisp -- LOGAND / LOGIOR / LOGXOR (and MOD by a power of two,
;;;; which the compiler rewrites into LOGAND) whose operands are literal arithmetic
;;;; such as (+ 3 7) or (* 3 7).  The compiler folds such an operand through
;;;; %CONST-INT-VALUE, whose (APPLY #'+ VALS) took APPLY's pre-packed &rest path into
;;;; CL's + -- an (&OPTIONAL A B &REST MORE) function, which that path does not
;;;; support -- so it signalled a TYPE-ERROR and the whole compile failed: a toplevel
;;;; (print (logand (+ 3 7) 255)) aborted the load.  Expected values are SBCL's.
;;;; Prints "BITOP-LITERAL-ARGS PASS" when every check holds.

(defvar *fails* 0)

(defun check (name got want)
  (if (eql got want)
      (format t "ok   ~a~%" name)
      (progn (incf *fails*)
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

;;; toplevel forms (the load's own compile of each form)
(check "toplevel logand (+ 3 7) 255" (logand (+ 3 7) 255) 10)
(check "toplevel logand (* 3 7) 255" (logand (* 3 7) 255) 21)
(check "toplevel logior (* 3 7) 256" (logior (* 3 7) 256) 277)
(check "toplevel logxor (+ 3 7) 6" (logxor (+ 3 7) 6) 12)
(check "toplevel logand (* 3 7) (* 1 255)" (logand (* 3 7) (* 1 255)) 21)
(check "toplevel mod (* 3 7) 256" (mod (* 3 7) 256) 21)
(check "toplevel mod (* 300 7) 256" (mod (* 300 7) 256) 52)
(check "toplevel mod (+ -3 -7) 16" (mod (+ -3 -7) 16) 6)

;;; value position inside a function, mixed with a variable
(defun f-and (x) (logand (+ 3 7) x))
(defun f-and2 (x) (logand x (* 3 7) 255))
(defun f-ior (x) (logior (* 3 7) x 256))
(defun f-xor (x) (logxor x (+ 1 2) (* 2 8)))
(defun f-mod (x) (+ x (mod (* 3 7) 16)))
(check "defun logand (+ 3 7) x" (f-and 6) 2)
(check "defun logand x (* 3 7) 255" (f-and2 #xff0f) 5)
(check "defun logior (* 3 7) x 256" (f-ior 8) 285)
(check "defun logxor x (+ 1 2) (* 2 8)" (f-xor 1) 18)
(check "defun mod (* 3 7) 16" (f-mod 100) 105)

;;; unary and negative literal arithmetic
(check "logand (+ 3) 255" (logand (+ 3) 255) 3)
(check "logand (* -3 7) 255" (logand (* -3 7) 255) 235)
(check "logior (+ -3) 1" (logior (+ -3) 1) -3)
(check "logxor (* -1 5) (+ 0)" (logxor (* -1 5) (+ 0)) -5)

;;; test position
(defun t-and (x) (if (logand (+ 3 7) x) :yes :no))
(defun t-zerop (x) (if (zerop (logand (+ 3 7) x)) :zero :nonzero))
(defun t-logtest (x) (if (logtest (* 3 7) x) :set :clear))
(defun t-mod (x) (if (= (mod (* 3 7) 4) x) :eq :ne))
(check "test logand" (t-and 1) :yes)
(check "test zerop logand 5" (t-zerop 5) :zero)
(check "test zerop logand 2" (t-zerop 2) :nonzero)
(check "test logtest 4" (t-logtest 4) :set)
(check "test logtest 8" (t-logtest 8) :clear)
(check "test mod" (t-mod 1) :eq)
(check "toplevel when zerop" (when (zerop (logxor (+ 3 7) 10)) :hit) :hit)
(check "toplevel cond /=" (cond ((/= (logand (* 3 7) 255) 21) :bad) (t :good)) :good)

;;; nested
(check "nested logand of logior" (logand (logior (* 4 4) (+ 1 0)) (+ 1 (* 2 15))) 17)
(check "nested logxor of logand" (logxor (logand (* 3 7) 255) (logior (+ 1 1) 1)) 22)
(check "nested ash of +" (ash (+ 3 7) 2) 40)
(check "nested logand ash" (logand (ash (* 3 7) 1) (- (* 16 4) 1)) 42)
(check "nested mod of mod" (mod (+ (mod (* 3 7) 8) (* 2 8)) 16) 5)
(defun nested-fn (x) (logior (logand (+ x (* 3 7)) (* 15 17)) (logxor (+ 1 2) x)))
(check "nested in defun" (nested-fn 4) 31)

;;; COMPILE of a lambda
(check "compile logand" (funcall (compile nil '(lambda () (logand (+ 3 7) 255)))) 10)
(check "compile logior" (funcall (compile nil '(lambda (x) (logior (* 3 7) x))) 256) 277)
(check "compile mod" (funcall (compile nil '(lambda () (mod (* 3 7) 256)))) 21)

;;; the same APPLY path with a user function of the (&optional ... &rest ...) shape
(defun opt-rest (&optional (a 1) (b 2) &rest more) (list a b more))
(check "apply #'+" (apply #'+ (list 3 7)) 10)
(check "apply #'*" (apply #'* (list 3 7 2)) 42)
(check "apply opt-rest" (equal (apply #'opt-rest (list 5 6 7 8)) '(5 6 (7 8))) t)
(check "apply opt-rest short" (equal (apply #'opt-rest (list 5)) '(5 2 nil)) t)

(format t "~&BITOP-LITERAL-ARGS ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
