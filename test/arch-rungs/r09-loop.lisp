;;;; r09-loop.lisp -- expect 42
;;;;
;;; A loop with an early RETURN, accumulating across iterations.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun sum-to (n)
  (let ((acc 0) (i 0))
    (loop
      (when (> i n) (return acc))
      (setq acc (+ acc i))
      (setq i (+ i 1)))))
(defun probe () (- (sum-to 9) 3))
