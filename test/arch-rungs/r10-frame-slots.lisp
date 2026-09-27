;;;; r10-frame-slots.lisp -- expect 42
;;;;
;;; A parameter plus two LET bindings -- enough to push a local into a frame
;;; slot.  ppc32's frame-slot base was PPC64's, on a frame that size, so every
;;; such local landed on the caller's save area.  Passed on all seven other
;;; targets, including ppc64, which shares that translator.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun f (n) (let ((acc 5) (i 2)) (+ n (+ acc i))))
(defun probe () (f 35))
