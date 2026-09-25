;;;; r31-float-negative.lisp -- expect 42
;;;;
;;; NEGATIVE INTEGERS THROUGH THE DOUBLE CONVERSIONS, AND TRUNCATION TOWARD ZERO.
;;;
;;; r18 and r30 only ever convert POSITIVE integers, so neither can see a sign
;;; bug.  That is where conversions go wrong: ppc32 has no FCFID and builds a
;;; double from an integer with the 2^52 + 2^31 magic-number trick, whose
;;; XOR #x80000000 is exactly the step a sign error would hide in; and :ftoi is
;;; specified to TRUNCATE, so -3.5 must become -3, where a floor (or a default
;;; round-to-nearest-even FPU mode) gives -4.
;;;
;;;   (ftoi (itof -40))        = -40
;;;   (ftoi (/ (itof -7) 2.0)) = -3   (truncate; floor would be -4)
;;;   -40 + -3 + 85            =  42

(defun probe ()
  (+ (%float-to-int (%float-from-int -40))
     (%float-to-int (%float-div (%float-from-int -7) (%float-from-int 2)))
     85))
