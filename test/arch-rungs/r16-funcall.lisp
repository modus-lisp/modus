;;;; r16-funcall.lisp -- expect 42
;;;;
;;; FUNCALL of a `#'name' — :fn-addr plus :call-ind, a PAIR the first fifteen
;;; rungs never touched (r01-call is a DIRECT call, which is :call).
;;;
;;; This rung exists because of a measurement: an opcode census over the real CL
;;; image (3,006,921 MVM instructions, 92 distinct opcodes) found :FN-ADDR used
;;; **5299 times** and NOT IMPLEMENTED by four of the nine back ends.  It is the
;;; single thing most in the way of running the real image on those targets --
;;; far more so than the seven float opcodes, which the census showed are used a
;;; few dozen times each, and six of which i386 lacks while shipping a working
;;; CL image.
;;;
;;; :fn-addr and :call-ind must be implemented TOGETHER: fn-addr tags the address
;;; with +tag-function+ (3), and call-ind has to strip it before jumping.  A back
;;; end with one and not the other jumps three bytes into a function.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun add2 (a b) (+ a b))
(defun probe () (funcall #'add2 40 2))
