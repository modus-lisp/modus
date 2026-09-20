;;;; hosted-mv-handler.lisp — MULTIPLE VALUES AND HANDLER-CASE ON TWO THREADS.
;;;;
;;;;   ./modus --script test/hosted-mv-handler.lisp
;;;;
;;;; The multiple-value return buffer (0x10000090) and the handler-frame stack
;;;; (0x10000400, with the armed frame at 0x10000180) were ONE COPY FOR THE
;;;; WHOLE IMAGE.  A producer writes its extras in its epilogue and the
;;;; consumer reads them at the call site; a handler-case arms one global frame
;;;; and pops one global stack.  With two OS threads under a preemptive kernel
;;;; both are races, and the handler one is not a wrong ANSWER but a WRONG
;;;; JUMP: thread A's unwind restores a frame thread B armed and lands on B's
;;;; stack.
;;;;
;;;; They are per-thread now — same addresses, a per-thread SEGMENT BASE (see
;;;; THE PER-THREAD WINDOW in mvm/compiler.lisp).  This is the test.
;;;;
;;;; WHAT WOULD MAKE THIS A LIE, AND WHAT STOPS IT.
;;;;
;;;;   "It completed" is not a result.  Every failure counter here is a value a
;;;;   thread computed and compared ITSELF — a truncate whose quotient and
;;;;   remainder must reconstruct the dividend, three values whose second and
;;;;   third must be the first plus one and two, a handler-case whose handler
;;;;   must be the one that runs.  Both threads' counters must be zero AND both
;;;;   iteration counts must reach N.
;;;;
;;;;   "Both threads ran" is a barrier with a spin budget.  A sequential run
;;;;   spins its budget out alone and reports a timeout; both timeouts must be
;;;;   0, so this cannot pass unless the two threads were in the workload at
;;;;   the same instant.
;;;;
;;;;   "It was per-thread" is not taken on trust either: each thread records
;;;;   the segment base it is actually running with, and the driver requires
;;;;   them DIFFERENT and thread 2's non-zero.
;;;;
;;;;   THE COLLECTIONS ARE REAL and counted: each thread forces collections of
;;;;   its OWN region while the other is mid-workload, and a live cons chain
;;;;   must still checksum afterwards.
;;;;
;;;;   AND THE NEGATIVE CONTROL IS ONE WORD, in the same binary, on the same
;;;;   workload: test/hosted-mv-handler-unsync.lisp passes MODE 1, which makes
;;;;   thread 2 skip installing its window and keep the base it was cloned with
;;;;   — this fix removed, exactly and only.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *n* 400)
(defvar *gcevery* 25)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(defun w (base off) (%gc-read64 (+ base off)))

(format t "~%=== MULTIPLE VALUES AND HANDLER-CASE ON TWO THREADS =======~%")
(format t "  ~D iterations per thread, a forced collection every ~D.~%"
        *n* *gcevery*)
(format t "  Per iteration each thread does: a TRUNCATE it reconstructs, a~%")
(format t "  three-value return it checks in full, an unwind through~%")
(format t "  handler-case, a NESTED unwind whose inner handler itself~%")
(format t "  unwinds, and a three-value return ACROSS an armed handler-case.~%")

(let ((ctl (%mvhc-selftest 0 *n* *gcevery*)))
  (if (zerop ctl)
      (progn (format t "~%  FAIL: the selftest could not start (carve/mmap).~%")
             (setq *fail* (+ *fail* 1)))
      (let ((s0 (+ ctl #x100))
            (s1 (+ ctl #x140)))

        (format t "~%=== BOTH THREADS RAN, AND AT THE SAME TIME ===============~%")
        (chk-true "thread 2 was cloned (non-zero TID)" (> (w ctl #x58) 0))
        (chk "thread 2 reached the workload" (w ctl #x28) 1)
        (chk "thread 2 finished it" (w ctl #x30) 1)
        (chk "the kernel cleared thread 2's TID word (it really exited)"
             (w ctl #x50) 0)
        (format t "  A barrier timeout is what a SEQUENTIAL run scores: the~%")
        (format t "  first arrival spins its whole budget alone.  Both must be 0.~%")
        (chk "thread 1's barrier timed out" (w ctl #x40) 0)
        (chk "thread 2's barrier timed out" (w ctl #x48) 0)

        (format t "~%=== THE WINDOW REALLY IS PER-THREAD ======================~%")
        (chk "thread 2's %TLS-INSTALL result (0 = installed)" (w ctl #x38) 0)
        (format t "  Each thread recorded the segment base it was RUNNING with.~%")
        (format t "  thread 1: ~D   thread 2: ~D~%" (w s0 #x28) (w s1 #x28))
        (chk "thread 1's segment base (the main thread's is 0)" (w s0 #x28) 0)
        (chk-true "thread 2's segment base is not zero" (> (w s1 #x28) 0))
        (chk-true "the two threads' windows are different"
                  (not (= (w s0 #x28) (w s1 #x28))))

        (format t "~%=== THE WORKLOAD COMPLETED ON BOTH SIDES =================~%")
        (chk "thread 1's iterations" (w s0 #x00) *n*)
        (chk "thread 2's iterations" (w s1 #x00) *n*)
        (format t "  Forced collections of each thread's OWN region:~%")
        (chk-true "thread 1 collected several times" (> (w s0 #x20) 1))
        (chk-true "thread 2 collected several times" (> (w s1 #x20) 1))
        (chk-true "thread 1's region really collected" (> (w ctl #x60) 0))
        (chk-true "thread 2's region really collected" (> (w ctl #x68) 0))

        (format t "~%=== NOTHING WAS CLOBBERED ================================~%")
        (chk "thread 1: multiple-value failures" (w s0 #x08) 0)
        (chk "thread 2: multiple-value failures" (w s1 #x08) 0)
        (chk "thread 1: handler-case failures" (w s0 #x10) 0)
        (chk "thread 2: handler-case failures" (w s1 #x10) 0)
        (chk "thread 1: nested-handler failures" (w s0 #x18) 0)
        (chk "thread 2: nested-handler failures" (w s1 #x18) 0)
        (format t "  Every arm must have been matched by its own pop: a thread~%")
        (format t "  that ends deeper (or shallower) than it started has been~%")
        (format t "  pushing onto, or popping from, somebody else's stack.~%")
        (chk "thread 1's net handler-stack depth change" (w s0 #x30) 0)
        (chk "thread 2's net handler-stack depth change" (w s1 #x30) 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "MV AND HANDLER-CASE ARE PER-THREAD: PASS (~D checks)~%"
                    *checks*)
            (format t "MV AND HANDLER-CASE ARE PER-THREAD: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
