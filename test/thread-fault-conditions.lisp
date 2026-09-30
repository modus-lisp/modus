;;;; thread-fault-conditions.lisp -- a caught fault's condition on a WORKER.
;;;;
;;;;   ./modus --script test/thread-fault-conditions.lisp     (hosted x86-64)
;;;;
;;;; Two mechanisms meet in a handler-case dispatch on hosted x86-64:
;;;; %HC-FAULT-FIXUP (the fault COUNT, and THE SHARED-STORE GUARD's own error)
;;;; and %TAKE-PENDING-FAULT (the recorded signal number -> a typed condition).
;;;; The guard traps with UD2, a SIGILL, which the typed path alone would call
;;;; an ILLEGAL-INSTRUCTION-ERROR.  Checked here, on a worker: the guard's
;;;; message survives; an ordinary fault is still a TYPE-ERROR; and a later
;;;; fault on main is typed afresh, not left as the guard's error.
;;;;
;;;; WHAT THIS DOES NOT ISOLATE: %HC-FAULT-FIXUP's clearing of the pending
;;;; signal.  Measured: an image without that line passes too -- the refused
;;;; store is caught first by a runtime handler-case compiled without the
;;;; fixup, whose %TAKE-PENDING-FAULT consumes the signal, and the outer
;;;; dispatch's fixup then overrides it with the guard's error.  The line is
;;;; for a guarded store with no such handler between it and the user's.

(defvar *tf-fail* 0)
(defun tf (name got want)
  (if (equal got want)
      (format t "  ok   ~A~%" name)
      (progn (setq *tf-fail* (+ *tf-fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))

(%sb-threads-up)
(defvar *shared* (list 0))

(defun worker-body ()
  (list
   ;; A worker's own cons stored into a main-thread object: refused.
   (handler-case (progn (setf (car *shared*) (list 1 2 3)) :stored)
     (simple-error (c) (if (search "shared memory" (simple-condition-format-control c))
                           :guard :other-simple-error))
     (error (c) (symbol-name (type-of c))))
   ;; A plain fault on the worker: a TYPE-ERROR, not the guard's error.
   (handler-case (mem-ref 48 :u32)
     (simple-error () :stale-guard) (type-error () :fault))
   ;; Non-pointers pass the guard.
   (handler-case (progn (setf (car *shared*) 42) :stored)
     (error (c) (symbol-name (type-of c))))))

(let ((r (sb-thread:join-thread (sb-thread:make-thread (function worker-body)))))
  (format t "  worker: ~S~%" r)
  (tf "guard refusal keeps its own error" (first r) :guard)
  (tf "worker fault after the guard is a TYPE-ERROR" (second r) :fault)
  (tf "fixnum store passes the guard" (third r) :stored))
(tf "main fault is typed afresh"
    (handler-case (mem-ref 64 :u32) (error (c) (symbol-name (type-of c))))
    "MEMORY-FAULT-ERROR")
(tf "shared cell holds the fixnum" (car *shared*) 42)

(format t "~&thread-fault-conditions: ~D failure(s)~%" *tf-fail*)
(when (> *tf-fail* 0) (error "thread-fault-conditions: ~D failure(s)" *tf-fail*))
