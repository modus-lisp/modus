;;;; r22-compare.lisp -- expect 42
;;;;
;;; NUMERIC COMPARISON AS A VALUE: (< a b) and (> a b) used as predicates, each in
;;; BOTH directions.  The branch forms (:blt/:bge and friends) were covered from
;;; rung 4 onward; asking a comparison for a VALUE is a different opcode path and
;;; had never been tested.
;;;
;;; Written while hunting a hang in the real CL image on RISC-V.  A diagnostic
;;; inside the looping function printed its entry marker and then nothing — not the
;;; per-iteration marker guarded by `(< n 25)', and not the bail-out guarded by
;;; `(> n 200)'.  Comparisons answering NIL for everything explains all three
;;; silences at once, and is the same defect class as :consp returning a raw 0/1
;;; (r20): a predicate whose result no caller can test correctly.
;;;
;;; 16 + 8 + 12 + 6 = 42, and every term needs a DIFFERENT answer, so a comparison
;;; stuck on true and one stuck on NIL both fail.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (+ (if (< 1 5) 16 0)
     (if (< 5 1) 0 8)
     (if (> 9 2) 12 0)
     (if (> 2 9) 0 6)))
