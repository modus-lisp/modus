;;;; Denormal multiply/divide battery: `sbcl --script test/float-denormal-gen.lisp`
;;;; writes e2e.lisp + expect.txt here.  2000 * and / whose results are double
;;;; DENORMALS; 17 of them come out wrong if the result is rounded to 53 bits and
;;;; then again to the denormal -- the x87 failure MODUS_I386_X87_NOSCALE=1 shows.
;;;; (Do not compute expectations with SBCL COERCE of a rational: it misrounds
;;;; ~half of these.  The expectations here are SBCL hardware results.)
(setf *random-state* (sb-ext:seed-random-state 1029))
(defun mk (e) (* (if (zerop (random 2)) 1 -1) (+ (ash 1 52) (random (ash 1 52))) (expt 2 (- e 52))))
(defun round-to (x bits)      ; round rational X to BITS significant bits, RNE
  (if (zerop x) 0
      (let* ((e (- (integer-length (floor (abs x))) 0))
             (k (- bits (if (>= (abs x) 1) (integer-length (floor (abs x))) (- (ceiling (- (log (abs x) 2)))) ))))
        (declare (ignore e))
        (/ (round (* x (expt 2 k))) (expt 2 k)))))
(defun dbl (x) (sb-int:with-float-traps-masked (:underflow :inexact) (coerce x 'double-float)))
(defun dec (x) (multiple-value-list (integer-decode-float x)))
(let ((n 0) (sensitive 0))
  (with-open-file (o "e2e.lisp" :direction :output :if-exists :supersede)
   (with-open-file (x "expect.txt" :direction :output :if-exists :supersede)
    (format o "(defun d (k x) (format t \"~~D ~~S~~%\" k (multiple-value-list (integer-decode-float x))))~%")
    (loop while (< n 2000) do
      (let* ((mul (zerop (random 2)))
             (er (- (random 66) 1080))
             (ea (if mul (- (random 400) 700) (- (random 120) 1000)))
             (eb (if mul (- er ea) (- ea er)))
             (a (dbl (mk ea))) (b (dbl (mk eb)))
             (exact (if mul (* (rational a) (rational b)) (/ (rational a) (rational b))))
             (r (sb-int:with-float-traps-masked (:underflow :inexact) (if mul (* a b) (/ a b))))
             (twice (dbl (let* ((k (- 53 (integer-length (floor (* (abs exact) (expt 2 1200)))) -1200)))
                           (/ (round (* exact (expt 2 k))) (expt 2 k))))))
        (when (and (/= r 0) (< (abs r) least-positive-normalized-double-float))
          (unless (= twice r) (incf sensitive))
          (format o "(d ~D (~A ~S ~S))~%" n (if mul "*" "/") a b)
          (format x "~D ~S~%" n (dec r))
          (incf n))))))
  (format t "~D denormal cases, ~D where rounding twice gives a different double~%" n sensitive))
