;;;; r14-string.lisp -- expect 42
;;;;
;;; A runtime-sized string, which is what reaches :alloc-string (a constant
;;; size takes the :alloc-obj path instead), read back through :array-len.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun mk (n) (%make-string-array n))
(defun probe () (+ 40 (%prim-array-length (mk 2))))
