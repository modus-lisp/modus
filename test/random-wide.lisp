;;;; test/random-wide.lisp -- RANDOM of an integer limit above 2^30.  modus's generator makes
;;;; 30 bits a draw and RANDOM reduced ONE draw modulo the limit, so (random (expt 2 64)) never
;;;; exceeded 2^30.  Each line must read the same under SBCL and modus (test/run-random-wide.sh).
(defun widest (limit n)
  (let ((mx 0)) (dotimes (i n mx) (setf mx (max mx (random limit))))))
(defun all-below (limit n) (dotimes (i n t) (unless (< -1 (random limit) limit) (return nil))))
(dolist (bits '(31 40 64 128 300))
  (let ((limit (expt 2 bits)))
    (format t "2^~d: uses the top bits ~s, stays below ~s~%" bits
            (>= (integer-length (widest limit 400)) (- bits 4)) (all-below limit 400))))
(let ((odd (+ (expt 10 30) 7)))
  (format t "10^30+7: stays below ~s, reaches above 10^29 ~s~%"
          (all-below odd 400) (> (widest odd 400) (expt 10 29))))
(let* ((a (make-random-state t)) (b (make-random-state a)))
  (format t "a copied random-state replays wide draws ~s~%"
          (equal (loop repeat 20 collect (random (expt 2 100) a))
                 (loop repeat 20 collect (random (expt 2 100) b)))))
(format t "small limits unchanged in range: ~s~%"
        (and (all-below 1 100) (all-below 7 400) (all-below (expt 2 30) 400)))
