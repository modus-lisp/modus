;;;; r27-five-params.lisp -- expect 42
;;;;
;;; A FIFTH AND SIXTH PARAMETER.  Only V0-V3 travel in registers; the caller
;;; PUSHes the rest, and the callee's frame-enter (TRAP <nparams>) must copy
;;; them into frame slots 4.. where the body reads them as `obj-ref VFP i'.
;;; RISC-V, PowerPC and 68k emitted the prologue and skipped the copy, so every
;;; parameter past the fourth was an UNINITIALISED frame slot.
;;;
;;; Twenty-six rungs never passed more than four arguments.  The real CL image
;;; passes five on the first INTERN of boot -- COPY-SEQ's (%bulk-copy result 0
;;; array 0 len) -- and LEN arrived as stack garbage, so (>= i n) went down the
;;; generic numeric path on a non-number and the image spun in
;;; NUMERIC-VALUE-LESS-P forever, printing nothing.
;;;
;;; E and F carry DIFFERENT weights so that a swapped pair (a copy that gets the
;;; stack order backwards) answers 38, not 42, and so does a pair that is merely
;;; zero.

(defun f6 (a b c d e f)
  (+ a b c d (* 2 e) (* 3 f)))

(defun probe ()
  (f6 1 2 3 4 4 8))                     ; 10 + 8 + 24 = 42
