;;;; r05-argument.lisp -- expect 42
;;;;
;;; Passing an ARGUMENT and using it twice.  `(+ x x)` goes through
;;; :add-checked, which ARM32 silently NOP'd -- this rung returned 21.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun dbl (x) (+ x x))
(defun probe () (dbl 21))
