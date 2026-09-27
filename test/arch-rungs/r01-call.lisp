;;;; r01-call.lisp -- expect 42
;;;;
;;; A function CALL and nothing else.  Every call emits :set-nargs first, so
;;; this is the rung that catches a back end with no calling-convention
;;; opcodes: RISC-V, PPC and 68k all trapped here before c38b504.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe () 42)
