;;;; r12-byte-vector.lisp -- expect 42
;;;;
;;; Byte vectors: :alloc-u8, :u8-ref, :u8-set.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (let ((v (%alloc-u8 4)))
    (%u8-set v 0 40)
    (%u8-set v 1 2)
    (+ (%u8-ref v 0) (%u8-ref v 1))))
