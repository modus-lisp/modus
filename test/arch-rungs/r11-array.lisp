;;;; r11-array.lisp -- expect 42
;;;;
;;; Arrays via the PRIMITIVES.  Plain `aref`/`aset` are library calls that
;;; need the runtime and fail on all eight targets including x64 -- an array
;;; rung written the obvious way measures the library, not the back end.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (let ((v (make-array 3)))
    (%prim-aset v 0 40)
    (%prim-aset v 1 2)
    (+ (%prim-aref v 0) (%prim-aref v 1))))
