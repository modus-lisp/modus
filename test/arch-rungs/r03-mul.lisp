;;;; r03-mul.lisp -- expect 42
;;;;
;;; Tagged multiply (one operand is untagged first).
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe () (* 6 7))
