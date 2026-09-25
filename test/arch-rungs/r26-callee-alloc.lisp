;;;; r26-callee-alloc.lisp -- expect 42
;;;;
;;; AN OBJECT ALLOCATED BY A CALLEE MUST SURVIVE THE CALLEE'S RETURN.
;;;
;;; The allocation pointer is GLOBAL STATE held in a register (VA).  RISC-V's
;;; prologue saved s1-s11 and its epilogue restored all eleven — and s8 IS VA,
;;; so every return rolled the heap pointer back over whatever the callee had
;;; allocated, and the caller's next CONS was handed memory that was still
;;; live.  translate-x64 saves RBX alone and says why: "R12 (alloc ptr), R14
;;; (alloc limit), R15 (nil) are global state that must NOT be saved/restored."
;;;
;;; WHY TWENTY-FIVE RUNGS MISSED IT.  r08-cons and r15-cons-mutate allocate,
;;; but in the PROBE ITSELF — there is no return in between to undo.  r01/r06/
;;; r16 call, but their callees allocate nothing.  It takes a callee that
;;; allocates, a return, and then a SECOND allocation whose address the first
;;; object's fate depends on.  MK and the (cons 100 200) below are exactly that.
;;;
;;; In the real CL image this destroyed the first nine conses ever allocated:
;;; (make-hash-table)'s bucket-holder was overwritten by the alist that
;;; SET-SYMBOL-VALUE built immediately after, so the globals table was
;;; malformed from the first global on, and 682 of 683 never registered.
;;;
;;; Under the bug (cons 100 200) lands on MK's cell, so (car a) reads 100 and
;;; the answer is 995 rather than 42 — wrong by enough that no tolerance hides it.

(defun mk () (cons 5 7))

(defun probe ()
  (let ((a (mk)))
    (let ((b (cons 100 200)))
      (+ (* (car a) 8)                    ; 40  — 5, not 100
         (cdr a)                          ;  7  — 7, not 200
         (if (eq (car b) 100) 0 1000)     ;  0  — b itself must be intact
         -5))))
