;;;; r23-and-value.lisp -- expect 42
;;;;
;;; AND AS A VALUE: (and t 40) must be 40, not T.  CLHS 5.3 — AND returns the
;;; value of its LAST form, not of the test that made it true.
;;;
;;; THIS RUNG PASSED THE MOMENT IT WAS WRITTEN, ON EVERY TARGET.  It exists as
;;; a recorded NEGATIVE result: it was written to test a hypothesis about a
;;; %GV-CELL failure -- that `(and (consp c) (car (cdr ...)))' was yielding the
;;; CONSP instead of the CAR -- and it disproved it, which is what sent the hunt
;;; to the real defects (a three-bit tag mask, a missing NIL exclusion, and an
;;; eight-slot frame; see r24 and r25).  Keeping it costs one cell per arch and
;;; means no later hunt has to re-ask the same question.
;;;
;;; Written from a gdb session on the real CL image.  %GV-CELL does
;;;     (let* ((holder (and (consp c) (car (cdr (cdr (cdr (cdr (cdr c)))))))) 
;;;            (vec    (and holder (car holder))))
;;; and the image was calling %SIGNAL-TYPE-ERROR with a0 = #xDEAD1009 = T from
;;; inside that function — i.e. (car T).  An AND that yields its FIRST value hands
;;; `holder' the T from (consp c), and then (car holder) is a type error.
;;;
;;; The consequence was not an error message.  %SIGNAL-TYPE-ERROR stores a global,
;;; which goes through %GV-SET -> %GV-CELL, which signals again: INFINITE MUTUAL
;;; RECURSION, %GV-CELL -> %SIGNAL-TYPE-ERROR -> %GV-SET -> %GV-CELL.  %GV-CELL's
;;; own docstring warns about exactly this shape ("any callee of this probe that
;;; reads a special re-enters %GV-REF -> %GV-CELL").  Nothing could register a
;;; global, so the globals alist stayed at one entry where the build reports 683.
;;;
;;; Both arms use EQ against the expected VALUE, so an AND that returns T passes
;;; neither.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (+ (if (eq (and t 40) 40) 30 0)
     (if (eq (and t t 12) 12) 12 0)))
