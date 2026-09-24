;;;; r15-cons-mutate.lisp -- expect 42
;;;;
;;; MUTATING a cons: :setcar and :setcdr.  This rung exists because for the
;;; first fourteen rungs NOTHING on ANY architecture mutated a cons -- r08-cons
;;; is `(let ((l (cons 40 2))) (+ (car l) (cdr l)))', which only reads -- and
;;; both PowerPC store paths turned out to write to ABSOLUTE ADDRESS ZERO
;;; (rA=0 in a D-form store means the literal zero, not the contents of r0).
;;;
;;; That went unseen on bare ppc32 for a second reason worth stating: it loads at
;;; address 0 with RAM from 0, so the stray store landed on the image's own first
;;; words and nothing ever read them again.  The hosted port, with nothing mapped
;;; at zero, made it a SIGSEGV.
;;;
;;; Two separate lessons, both cheap to act on: a rung per OPCODE PAIR, not per
;;; data type, and an unmapped zero page is an oracle.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun probe ()
  (let ((l (cons 1 2)))
    (set-car l 40)
    (set-cdr l 2)
    (+ (car l) (cdr l))))
