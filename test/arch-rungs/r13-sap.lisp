;;;; r13-sap.lisp -- expect 42
;;;;
;;; System area pointers.  AArch64 carried 138 opcodes and did not implement
;;; these two at all -- opcode COUNT is a poor proxy for capability.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe () (sap-address (make-sap 42)))
