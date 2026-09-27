;;;; r04-branch.lisp -- expect 42
;;;;
;;; A conditional branch with a constant comparison.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe () (if (< 3 5) 42 99))
