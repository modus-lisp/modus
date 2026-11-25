;;;; test/loop-global-hoist.lisp -- global arrays read once by a loop that calls nothing.
;;;;   modus --script test/loop-global-hoist.lisp   => LOOP-GLOBAL-HOIST PASS
;;;;
;;;; A loop that calls no function reads a global DECLAIMed an array type once, into a lexical
;;;; of that type (%GLOBAL-HOIST-REWRITE), and a rank-2 array declared with literal dimensions
;;;; checks its subscripts against them (%RANK2-ACCESS).  The cases that must still see the
;;;; global change -- a call, a SETQ, a LET of it in the loop -- and subscripts out of range.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defvar *v* (make-array 8 :initial-element 1))
(defvar *w* (make-array 8 :initial-element 10))
(defvar *m* (make-array '(3 4) :initial-element 0))
(declaim (type simple-vector *v* *w*))
(declaim (type (simple-array t (3 4)) *m*))

(defun sum-v () (let ((s 0)) (dotimes (i 8) (setq s (+ s (svref *v* i)))) s))
(defun swap () (rotatef *v* *w*) nil)
(defun sum-with-call ()
  (let ((s 0)) (dotimes (i 8) (when (= i 4) (swap)) (setq s (+ s (svref *v* i)))) (swap) s))
(defun sum-with-setq ()
  (let ((s 0) (old *v*))
    (dotimes (i 8) (when (= i 4) (setq *v* *w*)) (setq s (+ s (svref *v* i))))
    (setq *v* old) s))
(defun sum-with-let ()
  (let ((s 0)) (dotimes (i 8) (let ((*v* (if (< i 4) *v* *w*))) (setq s (+ s (svref *v* i))))) s))
(defun zero-trips (n) (let ((s 0)) (dotimes (i n) (setq s (+ s (svref *v* i)))) s))
(defun fill-m () (dotimes (i 3) (dotimes (j 4) (setf (aref *m* i j) (+ (* 10 i) j)))) *m*)
(defun sum-m () (let ((s 0)) (dotimes (i 3) (dotimes (j 4) (setq s (+ s (aref *m* i j))))) s))
(defun m-ref (i j) (aref *m* i j))
(defun m-set (i j v) (setf (aref *m* i j) v))

(check "sum" (sum-v) 8)
(check "call-sees-swap" (sum-with-call) 44)
(check "setq-sees-new" (sum-with-setq) 44)
(check "let-binding" (sum-with-let) 44)
(check "after-let" (sum-v) 8)
(check "zero-trips" (zero-trips 0) 0)
(fill-m)
(check "rank2-fill" (list (aref *m* 0 0) (aref *m* 1 2) (aref *m* 2 3)) '(0 12 23))
(check "rank2-sum" (sum-m) 138)
(check "rank2-ref" (m-ref 2 1) 21)
(m-set 1 1 :x)
(check "rank2-set" (aref *m* 1 1) :x)
(check "rank2-row-oob" (handler-case (m-ref 3 0) (error () :error)) :error)
(check "rank2-col-oob" (handler-case (m-ref 0 4) (error () :error)) :error)
(check "rank2-neg" (handler-case (m-ref -1 0) (error () :error)) :error)
(check "rank2-set-oob" (handler-case (m-set 0 4 1) (error () :error)) :error)
(check "rank2-intact" (aref *m* 2 3) 23)

(format t "~&LOOP-GLOBAL-HOIST ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
