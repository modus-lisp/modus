;;;; hosted-bringup-bare.lisp — THE SHAPE IN WHICH %SB-THREADS-UP LATCHES
;;;; SUCCESS WITHOUT ARMING ANYTHING.  Companion to test/hosted-bringup-gate.lisp
;;;; (read its header); this is probe F, kept as its own file because the finding
;;;; IS the script shape and putting it in the other file would change it.
;;;;
;;;;   ./modus --script test/hosted-bringup-bare.lisp
;;;;
;;;; MEASURED, 4 of 4, with and without a large DEFVAR prepended:
;;;;   PRE  gate=0 mode=0 flag=NIL
;;;;   POST ret=T gate=0 mode=0 flag=T
;;;;
;;;; ret=T and the flag LATCHED, with the threads-live gate (#x10000DB8) and the
;;;; per-CPU mode word (#x10000FF8) both still zero — so the runtime-table lock
;;;; is inert, B-lite's arena is never carved, and every later %SB-THREADS-UP
;;;; short-circuits on the flag and can never fix it.  The same call in
;;;; test/hosted-bringup-gate.lisp's shape ARMS (gate=1 mode=1).
;;;;
;;;; No threads, no sockets, no glass.  Two FORMATs and a LET, deliberately.
(format t "~&PRE  gate=~s mode=~s flag=~s~%"
        (mem-ref #x10000DB8 :u32) (mem-ref #x10000FF8 :u32) *sb-threads-up*)
(let ((r (%sb-threads-up)))
  (format t "~&POST ret=~s gate=~s mode=~s flag=~s~%"
          r (mem-ref #x10000DB8 :u32) (mem-ref #x10000FF8 :u32) *sb-threads-up*))
(finish-output)
(sys-exit 0)
