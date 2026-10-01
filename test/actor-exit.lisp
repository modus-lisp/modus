;;;; actor-exit.lisp -- a program that EXITS with an actor parked must end.
;;;;
;;;;   test/run-actor-exit.sh   (runs this under a clock; see there)
;;;;
;;;; SB-EXT:EXIT -> SYS-EXIT -> trap #x0500.  Its exit_group was gated on a flag
;;;; the 2026-09-30 merge made constant NIL, so this ended only the main thread:
;;;; a zombie leader, the scheduler threads parked forever, no EOF on the pipe.
(defun idle () (actors-receive))
(actors-start 2 2 #x400000)
(actors-spawn 'idle)
(format t "~&ACTOR-EXIT: exiting with an actor parked~%")
(finish-output)
(sb-ext:exit :code 7)
