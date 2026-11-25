;;;; test/fp-locals-across-calls.lisp -- float locals in FP registers through calls; LOG in registers.
;;;;   modus --script test/fp-locals-across-calls.lisp   => FP-LOCALS-ACROSS-CALLS PASS
;;;;
;;;; A scope's declared float locals stay in FP registers when it calls functions (on aarch64 at
;;;; run time: FP-SPILL / FP-RELOAD around each call), (LOG x) of a double tree is computed in
;;;; registers, and a double array element is a tree leaf.  Callees here use FP registers of their
;;;; own, so a missed spill shows up as a wrong sum.  Every answer is SBCL's.
(defvar *fails* 0)
;; SBCL traps a division by zero and overflow; modus computes the IEEE answer
(defmacro masked (&body body)
  #+sbcl `(sb-int:with-float-traps-masked (:divide-by-zero :invalid :overflow :inexact) ,@body)
  #-sbcl `(progn ,@body))
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defun clobber (x)                      ; uses FP registers itself
  (declare (double-float x))
  (let ((a (* x 3d0)) (b (+ x 0.25d0)) (c (/ x 7d0)))
    (declare (double-float a b c))
    (- (+ a b) c)))
(defun clobber-s (x) (declare (single-float x)) (let ((a (* x 3.0)) (b (+ x 0.25))) (declare (single-float a b)) (+ a b)))
(defun opaque (x) x)

(defun sum-with-calls (n)               ; N undeclared: the loop test has a slow-path call too
  (let ((s 0d0) (x 1d0) (y 2.5d0))
    (declare (double-float s x y))
    (dotimes (i n)
      (setq x (+ x 0.5d0))
      (setq s (+ s (clobber x) y))
      (setq y (* y 0.5d0)))
    (list s x y)))
(defun sum-single (n)
  (let ((s 0.0) (x 1.0))
    (declare (single-float s x))
    (dotimes (i n) (setq x (+ x 0.5)) (setq s (+ s (clobber-s x) (opaque 1.0))))
    (list s x)))
(defun nested (n)
  (let ((a 1d0))
    (declare (double-float a))
    (dotimes (i n)
      (let* ((b (+ a 1d0)) (c (clobber b)))
        (declare (double-float b c))
        (setq a (+ a (* 0.001d0 c)))))
    a))
(defun recur (k x)
  (declare (double-float x))
  (let ((y (* x 1.5d0)))
    (declare (double-float y))
    (if (= k 0) y (+ y (recur (- k 1) (+ y 1d0)) y))))
(defun log-sum (v)
  (declare (type (simple-array double-float (*)) v))
  (let ((s 0d0))
    (declare (double-float s))
    (dotimes (i (length v)) (setq s (+ s (log (aref v i)))))
    s))
(defun log-each (v)
  (declare (type (simple-array double-float (*)) v))
  (let ((out (make-array (length v))))
    (dotimes (i (length v)) (let ((x 0d0)) (declare (double-float x)) (setq x (aref v i)) (setf (aref out i) (+ 0d0 (log x)))))
    out))
(defun log-into (x)
  (declare (double-float x))
  (let ((s 1d0)) (declare (double-float s)) (setq s (+ s (log x))) s))

(check "calls" (sum-with-calls 50) (let ((s 0d0) (x 1d0) (y 2.5d0)) (dotimes (i 50) (setq x (+ x 0.5d0)) (setq s (+ s (opaque (clobber x)) y)) (setq y (* y 0.5d0))) (list s x y)))
(check "calls-single" (sum-single 40) (let ((s 0.0) (x 1.0)) (dotimes (i 40) (setq x (+ x 0.5)) (setq s (+ s (opaque (clobber-s x)) 1.0))) (list s x)))
(check "nested" (nested 100) (let ((a 1d0)) (dotimes (i 100) (let* ((b (+ a 1d0)) (c (opaque (clobber b)))) (setq a (+ a (* 0.001d0 c))))) a))
(check "recursion" (recur 6 1d0) (labels ((r (k x) (let ((y (* x 1.5d0))) (if (= k 0) y (+ y (r (- k 1) (+ y 1d0)) y))))) (r 6 1d0)))
(let ((v (make-array 6 :element-type 'double-float :initial-contents
                     '(1d0 2d0 0.5d0 1.4142135623730951d0 1d300 1d-300))))
  (check "log-sum" (log-sum v) (let ((s 0d0)) (dotimes (i 6) (setq s (+ s (log (opaque (aref v i)))))) s))
  (check "log-exact" (coerce (log-each v) 'list) (map 'list (lambda (x) (log (opaque x))) v)))
(check "log-zero" (masked (log-into 0d0)) (masked (+ 1d0 (log (opaque 0d0)))))
(check "log-subnormal" (masked (log-into least-positive-double-float))
       (masked (+ 1d0 (log (opaque least-positive-double-float)))))
(check "log-negative" (handler-case (log-into -2d0) (type-error () :type-error)) :type-error)
(check "log-plain-negative" (realpart (log (opaque -1d0))) 0d0)   ; a plain LOG stays complex

(format t "~&FP-LOCALS-ACROSS-CALLS ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
