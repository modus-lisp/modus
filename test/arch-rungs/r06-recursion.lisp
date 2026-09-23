;;;; r06-recursion.lisp -- expect 42
;;;;
;;; Self-recursion: the link register and frame must survive a nested call.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun down (n) (if (< n 1) 42 (down (1- n))))
(defun probe () (down 5))
