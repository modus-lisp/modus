;;;; r20-consp.lisp -- expect 42
;;;;
;;; CONSP and ATOM as PREDICATES — that is, values a caller can compare against
;;; NIL.  This rung exists because RISC-V's :consp returned a RAW 0 or #x16 while
;;; every caller tests the result against VN, so (CONSP anything) was ALWAYS TRUE,
;;; and no rung noticed: r08-cons calls car/cdr directly and nothing in the first
;;; nineteen ever asked a predicate a question.
;;;
;;; The measured consequence in the real CL image was a cdr-chain walk that ran
;;; off the end of a list and faulted taking the cdr of 0 — three steps removed
;;; from the actual defect.
;;;
;;; Same gap shape as r15-cons-mutate's: A RUNG PER OPCODE, NOT PER DATA TYPE.
;;; "cons" looked covered because allocation, reading and (later) mutation were.
;;;
;;; Both arms are checked in both directions, because a predicate that answers T
;;; for everything and one that answers NIL for everything each pass a one-sided
;;; test: 40 needs consp true AND atom false, 2 needs the reverse.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (let ((c (cons 1 2)))
    (+ (if (consp c) (if (atom c) 0 40) 0)
       (if (atom 7) (if (consp 7) 0 2) 0))))
