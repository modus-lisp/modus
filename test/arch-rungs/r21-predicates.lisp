;;;; r21-predicates.lisp -- expect 42
;;;;
;;; NULL and EQ as PREDICATES, each in BOTH directions.  Written while hunting a
;;; non-terminating alist probe in the real CL image: the loop exits on
;;; `(when (null cur) (return))', so a NULL that always answers NIL hangs exactly
;;; the way that image hangs, and a NULL that always answers true would return
;;; immediately instead.  Only a two-directional test can tell those apart from
;;; correct behaviour.
;;;
;;; Companion to r20-consp, and the same lesson: for the first nineteen rungs
;;; NOTHING ASKED A PREDICATE A QUESTION, which is how :consp sat broken (always
;;; true) on RISC-V through 19 green cells.  A RUNG PER OPCODE, NOT PER DATA TYPE.
;;;
;;; 20 + 10 + 8 + 4 = 42, and every term needs a different answer to be right.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (+ (if (null nil) 20 0)
     (if (null 7) 0 10)
     (if (eq 3 3) 8 0)
     (if (eq 3 4) 0 4)))
