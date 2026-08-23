;;;; hosted-blocking-receive.lisp — AN IDLE ACTOR THAT COSTS NOTHING.
;;;;
;;;;   ./modus --script test/hosted-blocking-receive.lisp
;;;;
;;;; RECEIVE has a blocking path — mark the actor BLOCKED, save its context,
;;;; hand the CPU to the scheduler — and in a hosted image that path was
;;;; unreachable.  The bare-metal idle loop it ends at uses a per-CPU idle-stack
;;;; trap and CLI / STI+HLT, all three privileged, so the hosted override
;;;; COUNTED THE EVENT AND RETURNED, and every threaded selftest in this tree
;;;; avoids RECEIVE: the workers poll TRY-RECEIVE + YIELD, which burns a whole
;;;; core per idle actor.
;;;;
;;;; It works now, and the claim worth making about it is a COST claim.
;;;;
;;;; WHAT WOULD MAKE THIS TEST A LIE, and what stops it.
;;;;
;;;;   "The messages arrived" proves nothing about blocking — they arrive under
;;;;   busy-polling too.  So the cost is measured over an IDLE WINDOW: a fixed
;;;;   stretch of wall time with NO traffic at all, in which the actor has
;;;;   nothing to do and the only question is what that costs.  The window is
;;;;   run twice in the same binary, one flag apart: mode 0 blocks in RECEIVE,
;;;;   mode 1 polls with TRY-RECEIVE + YIELD.  A RECEIVE that quietly fell back
;;;;   to spinning would pass every delivery check and FAIL here.
;;;;
;;;;   "CPU time was low" proves nothing on its own — it depends on the box.
;;;;   The polling arm is the yardstick, measured in the same process seconds
;;;;   later over the same window length.
;;;;
;;;;   NOTHING IS SENT DURING THE WINDOW, deliberately.  A busy-poller takes the
;;;;   scheduler lock twice per iteration with no work in between, and a sender
;;;;   on another thread then has to win an XCHG against a thread whose cache
;;;;   line is already Exclusive.  Measured here: with traffic, the polling
;;;;   arm's ten sends took 2 s of extra wall time typically and 260 s in the
;;;;   worst run seen.  That is a real property of busy-polling — it does not
;;;;   only burn a core, it starves its neighbours — but it is not the cost
;;;;   being measured, so the window is kept traffic-free and the two arms are
;;;;   compared there.
;;;;
;;;;   "It blocked" is also checked structurally, not only by cost: the
;;;;   scheduler counts how many times it went idle and how many times it
;;;;   actually issued FUTEX_WAIT.  In the blocking arm both must be non-zero;
;;;;   the legacy AP-SCHEDULER return counter — the pre-blocking behaviour every
;;;;   other selftest asserts is zero — must STILL be zero, because the blocked
;;;;   actor went to a real scheduler instead of falling out of one.
;;;;
;;;; TOPOLOGY is net/hosted-actors-post.lisp's step 4, unchanged: thread 1 is
;;;; the driver (actor 1, process stack, never yields); thread 2 has its own
;;;; stack, GS base and GC region, and runs %SCHED-RUN; actor 2 lives on a band
;;;; stack and is dispatched by it.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *n* 8)
(defvar *pn* 2)
(defvar *gap* 100)
(defvar *idle* 1000)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A~%" name))))

(defun ms (ns) (floor ns 1000000))

(defun report-arm (label res)
  (format t "  ~A  phase A (traffic): wall ~Dms  CPU ~Dms   messages ~D/~D  acks ~D~%"
          label (ms (w res #x08)) (ms (w res #x10))
          (w res #x38) (w res #x28) (w res #x48))
  (format t "  ~A  phase B (idle):    wall ~Dms  CPU ~Dms~%"
          label (ms (w res #xA8)) (ms (w res #xB0)))
  (format t "  ~A  scheduler: ~D idle entries, ~D FUTEX_WAITs, ~D dispatches, ~D wakes~%"
          label (w res #x80) (w res #x88) (w res #x90) (w res #x98)))

(defun check-arm (label res)
  (chk-true (concatenate 'string label ": clone(2) returned a TID") (> (w res #x00) 0))
  (chk (concatenate 'string label ": actor 2 started") (w res #x58) 1)
  (chk (concatenate 'string label ": thread 2 entered its scheduler") (w res #x68) 1)
  (chk (concatenate 'string label ": thread 2's cpu id") (w res #x78) 1)
  (chk (concatenate 'string label ": messages received") (w res #x38) (w res #x28))
  (chk (concatenate 'string label ": malformed messages") (w res #x40) 0)
  (chk (concatenate 'string label ": acknowledgements the driver got")
       (w res #x48) (w res #x28))
  (chk (concatenate 'string label ": bad acknowledgements") (w res #x50) 0)
  (chk (concatenate 'string label ": actor 2 saw the whole message run") (w res #x60) 1)
  (chk (concatenate 'string label ": thread 2 left its scheduler") (w res #x70) 1)
  (chk (concatenate 'string label ": the join saw the kernel clear the TID word")
       (w res #x18) 0))

(let ((bres 0) (bwall 0) (bcpu 0) (pres 0) (pwall 0) (pcpu 0))

  (format t "~%=== ARM 1: RECEIVE BLOCKS ================================~%")
  (format t "  ~D messages, one every ~Dms, then a ~Dms idle window.~%"
          *n* *gap* *idle*)
  (setq bres (%br-selftest 0 *n* *gap* *idle*))
  (if (= bres 0)
      (format t "~%SKIP: no actor band, no thread page, or the thread would not spawn.~%")
      (progn
        (report-arm "blocking" bres)
        (setq bwall (ms (w bres #xA8)))
        (setq bcpu (ms (w bres #xB0)))
        (check-arm "blocking" bres)

        (format t "~%=== IT REALLY WENT TO SLEEP ==============================~%")
        (chk-true "the scheduler went idle at least once" (> (w bres #x80) 0))
        (chk-true "and it issued a real FUTEX_WAIT" (> (w bres #x88) 0))
        (chk-true "and it was woken and dispatched again" (> (w bres #x90) 1))
        (format t "  The legacy hosted AP-SCHEDULER — count the event and~%")
        (format t "  return, which is what every other selftest asserts stays~%")
        (format t "  at zero — must STILL be zero: the blocked actor went to a~%")
        (format t "  REAL scheduler instead of falling out of one.~%")
        (chk "legacy AP-SCHEDULER returns" (w bres #xA0) 0)

        (format t "~%=== ARM 2: THE NEGATIVE CONTROL — TRY-RECEIVE + YIELD ====~%")
        (format t "  The same idle window, the same binary, one flag apart.~%")
        (format t "  This is what every threaded selftest in this tree does.~%")
        (setq pres (%br-selftest 1 *pn* *gap* *idle*))
        (if (= pres 0)
            (progn (setq *fail* (+ *fail* 1))
                   (format t "  FAIL the polling arm did not run~%"))
            (progn
              (report-arm "polling " pres)
              (setq pwall (ms (w pres #xA8)))
              (setq pcpu (ms (w pres #xB0)))
              (check-arm "polling" pres)

              (format t "~%=== THE IDLE WINDOW, SIDE BY SIDE ========================~%")
              (format t "  blocking   wall ~Dms   CPU ~Dms~%" bwall bcpu)
              (format t "  polling    wall ~Dms   CPU ~Dms~%" pwall pcpu)
              (chk-true "the polling arm burned a core for the wait (CPU >= half its wall time)"
                        (>= pcpu (floor pwall 2)))
              (chk-true "the blocking arm did NOT (CPU under a tenth of its wall time)"
                        (< bcpu (floor bwall 10)))
              (chk-true "and blocking cost strictly less CPU than polling"
                        (< bcpu pcpu))))

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "HOSTED x64 BLOCKING RECEIVE: PASS (~D checks)~%" *checks*)
            (format t "HOSTED x64 BLOCKING RECEIVE: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
