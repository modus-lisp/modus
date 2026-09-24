;;;; r18-float.lisp -- expect 42
;;;;
;;; ALL SIX double-float opcodes in one expression: :itof, :fsub, :fmul, :fdiv,
;;; :fadd, :ftoi.  ((48-8)*2)/4 + 22 = 42, computed entirely in IEEE doubles and
;;; converted back at the end, so a wrong answer localises to the arithmetic
;;; rather than to the boxing.
;;;
;;; These are six of the ten opcodes the four 92-of-144 back ends lack, and the
;;; census over the real CL image puts them at 160 uses total — rare, but i386
;;; implements all six, so the real image does reach them.  (The SIX it does NOT
;;; implement are the single-float f32 ops, used 8 times, on paths a working i386
;;; CL image never executes.  That is the difference between "rare" and "dead".)
;;;
;;; A double is a FOUR-SLOT object, subtag #x60, holding the 64 IEEE bits as four
;;; TAGGED 16-BIT CHUNKS — because a raw 64-bit value does not fit in a slot that
;;; carries a 62-bit fixnum.  A back end that gets the chunking wrong produces a
;;; plausible-looking float that is off by a power of two.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (%float-to-int
    (%float-add
      (%float-div
        (%float-mul (%float-sub (%float-from-int 48) (%float-from-int 8))
                    (%float-from-int 2))
        (%float-from-int 4))
      (%float-from-int 22))))
