;;;; hosted-dynbind.lisp — A SPECIAL BOUND ON A WORKER IS THAT WORKER'S.
;;;;
;;;;   ./modus --script test/hosted-dynbind.lisp
;;;;
;;;; test/run-glass-tx-cell.sh proves the DEFECT is gone (its `txfr' arm counts
;;;; region-0 -> worker pointers and goes 1 -> 0 against a between-arm
;;;; control).  It does not prove the REPLACEMENT is Common Lisp.  A binding
;;;; mechanism that simply dropped every worker-side binding on the floor would
;;;; score zero foreign refs too, and would pass that test perfectly.
;;;;
;;;; So this file asks the language questions, on a WORKER, and asks the same
;;;; ones on MAIN so that a difference between the two shows up as a failure
;;;; rather than as a story:
;;;;
;;;;   EQ IS EXACT.  A thread reading its own binding gets the IDENTICAL object
;;;;   it bound — not a copy.  That is why "copy the value on publish", the
;;;;   route CL:INTERN takes for its name strings, is not available here: a
;;;;   special can be bound to any object and EQ is observable.
;;;;
;;;;   IT SURVIVES THE COLLECTOR.  The bound object is reachable ONLY through
;;;;   the binding for the duration, and the loop inside it allocates hard
;;;;   enough to collect this thread's own region many times.  A raw per-thread
;;;;   word is not a GC root; if EMIT-DYNBIND-ROOT-SCAN were missing this check
;;;;   is the one that fails, with a moved-or-reclaimed object.
;;;;
;;;;   SHADOWING, RESTORE, NON-LOCAL EXIT, SETQ, LET*, PROGV.  An inner binding
;;;;   hides an outer one and gives it back; a THROW out of the middle of a
;;;;   binding still restores; a SETQ inside a binding writes THAT binding and
;;;;   does not leak past it; LET*'s later inits see the earlier bindings.
;;;;
;;;;   AND A SPECIAL THIS THREAD NEVER BOUND STILL READS THE PROCESS-WIDE
;;;;   VALUE.  This is the check that fails if the per-thread path ever stops
;;;;   falling through, and it is the one that keeps "give the worker its own
;;;;   storage" from meaning "give the worker its own universe".
;;;;
;;;; THE SCORE IS A BITMASK RETURNED THROUGH JOIN-THREAD, i.e. a FIXNUM, on
;;;; purpose: a thread return value that is a heap object crosses regions BY
;;;; POINTER (the documented `joinshare' residual), so a richer report would
;;;; have measured that instead of this.

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

(defvar *da* :global-a)
(defvar *db* :global-b)
(defvar *dc* :global-c)

;;; Every bit is one question.  Kept as separate bits rather than a count so a
;;; partial failure names WHICH rule broke instead of how many did.
(defvar *bit-eq*        1)      ; the binding hands back the identical object
(defvar *bit-restore*   2)      ; leaving the binding gives the old value back
(defvar *bit-shadow*    4)      ; an inner binding hides an outer one
(defvar *bit-unshadow*  8)      ; and gives it back on the way out
(defvar *bit-setq*     16)      ; SETQ inside a binding writes THAT binding
(defvar *bit-setq-out* 32)      ; and does not leak past it
(defvar *bit-throw*    64)      ; a non-local exit still restores
(defvar *bit-letstar* 128)      ; LET*'s later init sees the earlier binding
(defvar *bit-letstar-out* 256)  ; and both are restored
(defvar *bit-unbound* 512)      ; a special this thread never bound reads global
(defvar *bit-survive* 1024)     ; the bound object survives this region's GC
(defvar *bit-progv*   2048)     ; PROGV binds, and restores
(defvar *bit-progv-out* 4096)
(defvar *all-bits* 8191)

(defun dynbind-battery ()
  "Every question above, in one function, answering a bitmask.  Runs unchanged
   on the main thread and on a worker — that sameness is the point."
  (let ((score 0)
        (obj (list 7)))
    (let ((*da* obj))
      (if (eq *da* obj) (setq score (+ score *bit-eq*)) 0))
    (if (eq *da* :global-a) (setq score (+ score *bit-restore*)) 0)

    (let ((*da* 1))
      (let ((*da* 2))
        (if (eql *da* 2) (setq score (+ score *bit-shadow*)) 0))
      (if (eql *da* 1) (setq score (+ score *bit-unshadow*)) 0))

    (let ((*da* 1))
      (setq *da* 9)
      (if (eql *da* 9) (setq score (+ score *bit-setq*)) 0))
    (if (eq *da* :global-a) (setq score (+ score *bit-setq-out*)) 0)

    (catch 'dynb-out (let ((*da* 77)) (throw 'dynb-out 0)))
    (if (eq *da* :global-a) (setq score (+ score *bit-throw*)) 0)

    (let* ((*da* 5) (*db* (+ *da* 1)))
      (if (and (eql *da* 5) (eql *db* 6))
          (setq score (+ score *bit-letstar*)) 0))
    (if (and (eq *da* :global-a) (eq *db* :global-b))
        (setq score (+ score *bit-letstar-out*)) 0)

    ;; *DC* is bound by NOBODY here.  A per-thread store that stopped falling
    ;; through to the shared table would fail exactly this.
    (if (eq *dc* :global-c) (setq score (+ score *bit-unbound*)) 0)

    ;; THE COLLECTOR QUESTION.  OBJ is now reachable only through the binding
    ;; — the local was rebound above — and the loop allocates hard enough that
    ;; this thread's own region collects repeatedly while it is bound.
    (setq obj (list 12345))
    (let ((*da* obj))
      (let ((i 0) (junk nil))
        (loop
          (when (>= i 200000) (return 0))
          (setq junk (cons i (cons i nil)))
          (setq i (+ i 1)))
        (if junk 0 0))
      (if (and (consp *da*) (eql (car *da*) 12345) (eq *da* obj))
          (setq score (+ score *bit-survive*)) 0))

    (progv (list '*dc*) (list 111)
      (if (eql *dc* 111) (setq score (+ score *bit-progv*)) 0))
    (if (eq *dc* :global-c) (setq score (+ score *bit-progv-out*)) 0)
    score))

(format t "~%=== A SPECIAL BOUND ON A WORKER IS THAT WORKER'S ==========~%")

(format t "  all bits = ~D~%" *all-bits*)
(chk "the battery on the MAIN thread" (dynbind-battery) *all-bits*)

;;; THE WORKER.  sb-thread:make-thread is what glass writes and what arms the
;;; runtime seam; a raw %MAKE-NATIVE-THREAD would leave the gate unarmed and
;;; measure the shallow path twice.
(let ((th (sb-thread:make-thread (lambda () (dynbind-battery)))))
  (let ((r (sb-thread:join-thread th)))
    (format t "  worker returned ~D~%" r)
    (chk "the battery on a WORKER thread" r *all-bits*)))

;;; AND MAIN IS STILL ITSELF AFTERWARDS — a worker's bindings must not have
;;; disturbed the process-wide cells the worker read through.
(chk "*DA* on main after the worker ran" *da* :global-a)
(chk "*DB* on main after the worker ran" *db* :global-b)
(chk "*DC* on main after the worker ran" *dc* :global-c)
(chk "the battery on MAIN again" (dynbind-battery) *all-bits*)

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(if (zerop *fail*)
    (format t "PER-THREAD DYNAMIC BINDINGS: PASS~%")
    (format t "PER-THREAD DYNAMIC BINDINGS: FAIL~%"))
