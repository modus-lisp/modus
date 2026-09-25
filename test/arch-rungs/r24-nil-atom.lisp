;;;; r24-nil-atom.lisp -- expect 42
;;;;
;;; CONSP and ATOM ON THE TWO VALUES THAT ARE NOT DATA: NIL and T.
;;;
;;; r20 already asks consp/atom in both directions, and passed on RISC-V while
;;; BOTH were wrong for both of these — because r20 only ever asks about a cons
;;; and a fixnum, and NIL and T are the two values whose tags lie.
;;;
;;;   NIL is #xDEAD0001 and its low nibble IS +tag-cons+.  That is deliberate
;;;   (it lets car/cdr of NIL be a plain load into a NIL-filled page), so a bare
;;;   tag test answers (consp NIL) => T.  x64 compares against R15, and i386
;;;   against *vn-addr*, BEFORE testing the tag; RISC-V did not.
;;;
;;;   T is #xDEAD1009 and its low nibble is 1001 = +tag-object+.  Under the
;;;   FOUR-bit +tag-mask+ that is not a cons.  RISC-V masked THREE bits, so
;;;   1001 read as 001 and (consp T) answered T as well.
;;;
;;; Both defects make the same idiom non-terminating, and translate-i386's own
;;; comment records the cost the last time a back end shipped the NIL half:
;;;     (loop while (consp cur) do ... (setq cur (cdr cur)))
;;; reaches the terminating NIL, consp says T, (car NIL) hands back NIL, and the
;;; walk recurses on NIL forever.  In the real CL image on RISC-V it also WROTE:
;;; a walker that believed NIL was a cons stored through it, and (car nil) came
;;; back #xDEAD0003 instead of NIL — a corrupted NIL page, measured in gdb.
;;;
;;; The general rule this rung encodes: A PREDICATE MUST BE ASKED ABOUT THE
;;; VALUES WHOSE REPRESENTATION IS A SPECIAL CASE, not only about ordinary data.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (+ (if (consp nil) 0 20)              ; NIL is not a cons
     (if (atom nil) 10 0)               ; NIL is an atom
     (if (consp t) 0 8)                 ; T is not a cons
     (if (atom t) 4 0)))                ; T is an atom
