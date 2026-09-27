;;;; r25-many-locals.lisp -- expect 42
;;;;
;;; MORE LOCALS THAN THE BACK END RESERVED FRAME SLOTS FOR, HELD ACROSS A CALL.
;;;
;;; The MVM compiler addresses let-bound locals as `obj-ref VFP <idx>' with an
;;; index it picks per function, and it tells the back end no bound.  x64
;;; reserves 128 slots for exactly that reason.  RISC-V reserved EIGHT, so a
;;; ninth local was addressed BELOW its own 208-byte frame — in the memory the
;;; next call's frame occupies — and read back whatever that callee left.
;;;
;;; TWO CONDITIONS ARE BOTH REQUIRED, which is why nineteen earlier rungs missed
;;; it.  Many locals alone is not enough: nothing overwrites a slot below the
;;; frame until something else PUSHES a frame there.  A call alone is not
;;; enough: r01/r06/r16 all call, with few locals.  It takes a local that is
;;; LIVE ACROSS a call and that lives past slot 8.
;;;
;;; What it cost in the real CL image: %GV-CELL binds eight locals and calls
;;; two functions, so `%gv-holder' read RAW 0 out of its slot.  0 is neither
;;; NIL (#xDEAD0001) nor cons-tagged, so `(car %gv-holder)' reached
;;; %SIGNAL-TYPE-ERROR — which reads a special, re-entering %GV-CELL, and the
;;; mutual recursion presented as a HANG in init with nothing in the log.
;;;
;;; The `+' chain at the end is deliberate: it forces every one of the sixteen
;;; to be live simultaneously AFTER the call returns, so a slot the callee
;;; overwrote cannot be masked by a value that happens to be recomputed.

(defun bump (x) (+ x 1))

(defun probe ()
  (let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8)
        (i 9) (j 10) (k 11) (l 12) (m 13) (n 14) (o 15) (p 16))
    ;; A CALL IN THE MIDDLE, with all sixteen live across it.
    (let ((q (bump (+ a b))))
      ;; 1+2+...+16 = 136; q = 4; 136 - 4 - 90 = 42
      (- (+ a b c d e f g h i j k l m n o p) q 90))))
