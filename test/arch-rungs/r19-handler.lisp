;;;; r19-handler.lisp -- expect 42
;;;;
;;; HANDLER-CASE's ARM AND DISARM: :setjmp (#x0510) and :clear-handler (#x0512),
;;; plus the NESTING those two must survive.  The real CL image emits 708 setjmps
;;; and 709 clear-handlers (measured), and they are part of the last codegen
;;; blocker for running it on a new back end.
;;;
;;; WHAT THIS RUNG DOES *NOT* COVER, stated because a green cell here must not be
;;; read as "handler-case works": the LONGJMP arm (#x0511, 123 uses).  Reaching it
;;; needs a signal, and matching even a `t' clause afterwards goes through the
;;; CONDITION SYSTEM, which a minimal ladder image does not contain -- measured:
;;; `(handler-case (progn (%hc-longjmp) 0) (t (c) 2))' fails on x64 too, so such a
;;; rung would grade the library rather than the back end.  Exactly the trap
;;; r11-array's header names for plain AREF.  A back end can pass this rung with a
;;; LONGJMP that jumps to garbage; that arm is exercised by the real image.
;;;
;;; NESTING IS THE PART THAT IS NOT DEFERRABLE.  A single-level implementation
;;; passes every SEQUENTIAL handler-case -- which is what init-all-globals does,
;;; one per init thunk -- and then fails on the first nested one.  So `probe'
;;; wraps a handler-case around a call to a function that has its own: the outer
;;; jmpbuf must be stacked on the inner arm and restored on the inner disarm.
;;;
;;; 2 from the inner + 40 = 42.
;;;
;;; The gate wraps this file: `probe' is called, its answer is stored at a
;;; fixed physical address, and the image spins.  scripts/arch-ladder.py
;;; reads that address back over QMP and compares it to the expected value.

(defun inner () (handler-case 2 (t (c) 0)))
(defun probe () (handler-case (+ 40 (inner)) (t (c) 0)))
