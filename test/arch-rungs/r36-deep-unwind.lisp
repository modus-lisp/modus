;;;; r36-deep-unwind.lisp -- expect 42
;;;;
;;; A NON-LOCAL EXIT RUNS EVERY CLEANUP, OR THE PUSH THAT COULD NOT STORE ITS
;;; FRAME SAYS SO.  300 nested UNWIND-PROTECTs, one per call, then a LONGJMP
;;; (TRAP #x0511) from the bottom: every level's cleanup must run, innermost
;;; first (each one's error path re-issues the longjmp to the next).
;;;
;;; The handler-frame stack used to be a fixed array whose overflow was a
;;; TRANSPARENT NO-OP (BALANCED-CAP): a push past the cap stored nothing and the
;;; setjmp site skipped arming, so the longjmp from the bottom went straight to
;;; the deepest STORED frame and every cleanup past the cap was silently
;;; skipped -- this rung then counted (cap - frames below PROBE) cleanups.
;;; Measured before the fix: i386 / arm32 / ppc / riscv / 68k all short.
;;;
;;; Two answers are right, because a bare-metal image keeps a FIXED frame area:
;;;   * every level entered and all 301 cleanups ran (a hosted image, whose
;;;     frames past the in-memory area go to a reserved extension, or a bare
;;;     image whose area holds 301 frames);
;;;   * the LOUD overflow: the push at level K could not be stored, so level K
;;;     never ran its body and the push longjmped to level K-1's frame: levels
;;;     0..K entered, exactly K cleanups ran.  A silent cap can never look like
;;;     this -- it enters all 301 levels and runs fewer than 301 cleanups.
;;;
;;; BOX = (cleanups . deepest level entered).  Only RAW longjmps and lexical
;;; exits: a ladder image has no condition system and no THROW (r19's header).
;;; The outermost UNWIND-PROTECT's cleanup ends the unwind with a lexical
;;; RETURN-FROM (its own frame was already popped by the longjmp that reached it).

(defun nest (n lim box)
  (setf (cdr box) n)
  (unwind-protect (if (< n lim) (nest (+ n 1) lim box) (%hc-longjmp))
    (setf (car box) (+ (car box) 1))))

(defun probe ()
  (let ((box (cons 0 0)))
    (block out
      (unwind-protect (nest 0 300 box)
        (return-from out nil)))
    (cond ((and (= (cdr box) 300) (= (car box) 301)) 42)
          ((and (< (cdr box) 300) (> (cdr box) 0) (= (car box) (cdr box))) 42)
          (t (+ 1000 (car box))))))
