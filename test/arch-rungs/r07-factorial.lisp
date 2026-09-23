;;;; r07-factorial.lisp -- expect 3628800
;;;;
;;; Recursion plus multiply, carrying a value back up through every frame.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun factorial (n) (if (< n 2) 1 (* n (factorial (1- n)))))
(defun probe () (factorial 10))
