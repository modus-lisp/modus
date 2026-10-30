;;;; Random float battery: `sbcl --script test/float-e2e-gen.lisp` writes e2e.lisp
;;;; and expect.txt in the current directory; run e2e.lisp on an image and diff
;;;; its stdout against expect.txt (results as INTEGER-DECODE-FLOAT, exact).
;; Generate e2e.lisp (ops on literals, printing integer-decode-float of each
;; result) and expect.txt (SBCL's answers).  Finite results only.
(setf *random-state* (sb-ext:seed-random-state 20261029))
(defun rnd (type)
  (let* ((e (case (random 4) (0 (- (random 60) 30)) (1 (- (random 600) 300))
              (2 (if (eq type 'double-float) (- -1074 (- (random 40)) ) (- -149 (- (random 20)))))
              (t (- (random 200) 100))))
         (m (+ 1 (random (ash 1 (if (eq type 'double-float) 53 24)))))
         (v (handler-case (coerce (* (if (zerop (random 2)) 1 -1) m (expt 2 e)) type) (error () (coerce 1 type)))))
    v))
(defun dec (x) (multiple-value-list (integer-decode-float x)))
(with-open-file (o "e2e.lisp" :direction :output :if-exists :supersede)
 (with-open-file (x "expect.txt" :direction :output :if-exists :supersede)
  (let ((*read-default-float-format* 'single-float) (n 0))
   (format o "(defun d (k x) (format t \"~~D ~~S~~%\" k (multiple-value-list (integer-decode-float x))))~%")
   (loop while (< n 3000) do
     (let* ((type (if (zerop (random 3)) 'single-float 'double-float))
            (a (rnd type)) (b (rnd type))
            (op (nth (random 6) '(+ - * / sqrt conv)))
            (r (sb-int:with-float-traps-masked (:overflow :underflow :inexact :invalid :divide-by-zero)
                 (case op (sqrt (sqrt (abs a))) (conv (coerce a (if (eq type 'double-float) 'single-float 'double-float)))
                       (t (funcall op a b))))))
       (when (and (floatp r) (not (sb-ext:float-infinity-p r)) (not (sb-ext:float-nan-p r)))
         (format o "(d ~D ~A)~%" n
                 (case op (sqrt (format nil "(sqrt ~S)" (abs a)))
                       (conv (format nil "(coerce ~S '~A)" a (if (eq type 'double-float) "single-float" "double-float")))
                       (t (format nil "(~A ~S ~S)" op a b))))
         (format x "~D ~S~%" n (dec r))
         (incf n)))))))
