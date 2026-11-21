;;;; h02-deep-unwind.lisp -- expect 42
;;;;
;;; HOSTED ONLY: a hosted image has no silent ceiling on handler frames.  The
;;; arch-ladder rung r36 accepts a LOUD overflow, because a bare image keeps a
;;; fixed frame area; a hosted image reserves an extension past its in-memory
;;; frames, so here 3000 nested UNWIND-PROTECTs must ALL run their cleanups
;;; after a LONGJMP from the bottom -- no overflow is a right answer.
;;;
;;; AND THE FRAMES SURVIVE A COLLECTION.  On these ports a saved frame holds
;;; the callee-saved V-registers of its setjmp (heap pointers), and a LONGJMP
;;; restores them, so every frame -- the extension's too -- is a GC root.  Each
;;; level keeps a fresh list (N N) live across the nested call, the bottom
;;; churns 320 MB (several collections) BEFORE the longjmp, and each cleanup
;;; counts only if its own list still reads (N N).  A frame the collector
;;; missed hands back a stale from-space pointer: a wrong count or a crash.
;;;
;;; Run with:  scripts/hosted-ladder.py riscv64 test/hosted-rungs/h02-deep-unwind.lisp
;;;
;;; BOX = (cleanups . deepest level entered); 1000 + cleanups on failure.

(defun churn (n)
  (let ((i 0) (junk nil))
    (loop (when (>= i n) (return junk))
      (setq junk (cons i junk))
      (when (= 0 (logand i 1023)) (setq junk nil))
      (setq i (+ i 1)))))

(defun nest (n lim box)
  (setf (cdr box) n)
  (let ((mine (list n n)))
    (unwind-protect
         (if (< n lim)
             (nest (+ n 1) lim box)
             (progn (churn 20000000) (%hc-longjmp)))
      (when (and (consp mine) (eql (car mine) n) (eql (car (cdr mine)) n))
        (setf (car box) (+ (car box) 1))))))

(defun probe ()
  (let ((box (cons 0 0)))
    (block out
      (unwind-protect (nest 0 3000 box)
        (return-from out nil)))
    (if (and (= (cdr box) 3000) (= (car box) 3001)) 42 (+ 1000 (car box)))))
