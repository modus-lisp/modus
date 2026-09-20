;;;; hosted-thread-actors.lisp — STEP 4: ACTORS ON TWO NATIVE THREADS.
;;;;
;;;;   ./modus --script test/hosted-thread-actors.lisp
;;;;
;;;; ONE actor system — one run queue, one mailbox pool, one staging area, one
;;;; scheduler lock — with TWO kernel threads pulling work out of it, passing
;;;; messages across the boundary.
;;;;
;;;; WHAT MAKES IT SAFE IS THE STEP-1 PROTOCOL AND NOTHING ELSE.  Every
;;;; RESTORE-CONTEXT is issued while HOLDING the scheduler lock, and the switch
;;;; releases it after the stack switch.  That is not a style rule: an actor
;;;; becomes claimable by the other thread the instant the lock drops, so a
;;;; release before the stack has moved would put two CPUs on one stack.  It
;;;; also means a RESTORE-CONTEXT issued WITHOUT the lock would zero a lock the
;;;; other thread is holding — which is why thread 2's hand-back takes the lock
;;;; first even though it has no queue to protect.
;;;;
;;;; THE TOPOLOGY is deliberately not "let everything migrate":
;;;;   thread 1 runs actor 1, the primordial actor, on the PROCESS stack.  It
;;;;            never YIELDs, so it is never enqueued and the other thread can
;;;;            never claim it; it drives the run and reports.
;;;;   thread 2 runs actors 2 and 3 on their own 64 KB band stacks, alternating
;;;;            through the SHARED run queue with ordinary YIELD.
;;;; So a round trip crosses a thread boundary twice: actor 1 (thread 1) ->
;;;; actor 2 (thread 2) -> actor 3 (thread 2) -> actor 1.
;;;;
;;;; NOBODY BLOCKS, and that is a constraint rather than laziness.  RECEIVE's
;;;; blocking path ends at AP-SCHEDULER when the run queue is empty, and the
;;;; hosted AP-SCHEDULER cannot be made correct under threads without first
;;;; switching off the blocking actor's stack: the actor is already marked
;;;; BLOCKED with its context saved, so the other thread may resume it onto the
;;;; very stack we would still be standing on.  The workers use TRY-RECEIVE +
;;;; YIELD instead — exactly what net/isolated-net.lisp's net-domain does — and
;;;; the test asserts the AP-SCHEDULER counter never moved.
;;;;
;;;; SCOPE, MEASURED RATHER THAN ASSUMED.  The runtime's shared mutable tables
;;;; (the globals alist, the symbol / keyword / package intern tables, the macro
;;;; tables) are UNSYNCHRONISED, so an actor that interns a symbol or triggers
;;;; dynamic compilation races.  These actors do arithmetic, raw memory
;;;; reads/writes, and message passing, and nothing else: no FORMAT, no INTERN,
;;;; no EVAL, no symbol or keyword literal.  What they DO allocate is conses —
;;;; TERM-DECODE-STEP builds the received message in the receiver's heap — which
;;;; is why each thread needs its own region, and why the collection counts
;;;; below are reported rather than ignored.
;;;;
;;;; WHY "BOTH THREADS RAN ACTORS" IS NOT JUST A CLAIM.  Every actor, every
;;;; iteration, reads the :CPU-ID slot through ITS OWN GS base and bumps one of
;;;; two counters.  Actor 1's cpu-1 count must be 0 and its cpu-0 count large;
;;;; actors 2 and 3 must be the other way round.  And the parallelism is
;;;; measured, not inferred: the driver counts how many of its own poll
;;;; iterations saw actor 2's progress counter move underneath it, which a
;;;; sequential run leaves at exactly zero.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nmsg* 64)
(defvar *budget* 200000000)

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

(let ((res (%ha-mt-selftest *nmsg* *budget* 0)))
  (if (= res 0)
      (format t "~%SKIP: no actor band, or the thread stack could not be mapped.~%")
      (let* ((join    (w res #x00))
             (tid     (w res #x08))
             (ida     (w res #x10))
             (idb     (w res #x18))
             (got     (w res #x20))
             (nmsg    (w res #x28))
             (fwd     (w res #x30))
             (logged  (w res #x38))
             (badA    (w res #x40))
             (badB    (w res #x48))
             (badack  (w res #x50))
             (idle    (w res #x58))
             (a2c0    (w res #x60))
             (a2c1    (w res #x68))
             (a3c0    (w res #x70))
             (a3c1    (w res #x78))
             (a1c0    (w res #x80))
             (a1c1    (w res #x88))
             (disp    (w res #x90))
             (idlesp  (w res #x98))
             (handed  (w res #xA0))
             (spins   (w res #xA8))
             (seen    (w res #xB0))
             (t2to    (w res #xB8))
             (t1to    (w res #xC0))
             (reached (w res #xC8))
             (t2tid   (w res #xD0))
             (t1tid   (w res #xD8))
             (t2cpu   (w res #xE0))
             (t2reg   (w res #xE8))
             (rcb3    (w res #xF0))
             (t1reg   (w res #xF8))
             (logbase (w res #x100))
             (g3      (w res #x108))
             (g0b     (w res #x110))
             (g0a     (w res #x118))
             (t2alloc (w res #x120))
             (r2from  (w res #x128))
             (clean   (w res #x130))
             (fr32    (w res #x188))
             (fr23    (w res #x190))
             (ctl3    (w res #x198))
             (ctl2    (w res #x1A0)))

        (format t "~%=== TWO THREADS, ONE ACTOR SYSTEM ========================~%")
        (format t "  thread 1 gettid ~D ; thread 2 gettid ~D~%" t1tid t2tid)
        (chk-true "clone returned a TID" (> tid 0))
        (chk "and thread 2 reported the same one" t2tid tid)
        (chk "thread 2 stamped its own CPU id" t2cpu 1)
        (chk "thread 2 reached its dispatch loop" reached 1)
        (chk "actor A's id" ida 2)
        (chk "actor B's id" idb 3)

        (format t "~%=== BOTH THREADS RAN ACTORS ==============================~%")
        (format t "  Each actor reads the :CPU-ID slot through ITS OWN GS base~%")
        (format t "  once per iteration.  These are the counts it recorded.~%")
        (format t "    actor 1 : cpu0 ~D   cpu1 ~D~%" a1c0 a1c1)
        (format t "    actor 2 : cpu0 ~D   cpu1 ~D~%" a2c0 a2c1)
        (format t "    actor 3 : cpu0 ~D   cpu1 ~D~%" a3c0 a3c1)
        (chk-true "actor 1 ran on thread 1" (> a1c0 0))
        (chk "and never on thread 2" a1c1 0)
        (chk-true "actor 2 ran on thread 2" (> a2c1 0))
        (chk "and never on thread 1" a2c0 0)
        (chk-true "actor 3 ran on thread 2" (> a3c1 0))
        (chk "and never on thread 1" a3c0 0)
        (format t "  thread 2 dispatched ~D actor(s) off the shared run queue~%" disp)
        (chk-true "thread 2 took work off the SHARED run queue" (> disp 0))
        (chk "the hosted AP-SCHEDULER was never reached" idle 0)

        (format t "~%=== THE PARALLELISM IS MEASURED ==========================~%")
        (format t "  The driver polls for acks without ever blocking, and counts~%")
        (format t "  how many of its own iterations saw actor 2's progress~%")
        (format t "  counter move underneath it.  A sequential run scores 0.~%")
        (format t "  driver poll iterations that found no ack : ~D~%" spins)
        (format t "  times it saw actor 2 advance              : ~D~%" seen)
        (chk-true "the driver watched actor 2 make progress" (> seen 0))
        (format t "  Before any of that, the driver and thread 2 met at a~%")
        (format t "  BARRIER neither can pass alone.~%")
        (chk "thread 1 spun out its budget alone (1 = yes)" t1to 0)
        (chk "thread 2 spun out its budget alone (1 = yes)" t2to 0)

        (format t "~%=== EVERY MESSAGE CROSSED THE BOUNDARY INTACT ============~%")
        (format t "  actor 1 (thread 1) -> actor 2 (thread 2) -> actor 3~%")
        (format t "  (thread 2) -> actor 1.  Every message is serialised into~%")
        (format t "  the target's staging buffer and decoded into the target's~%")
        (format t "  heap, twice per round trip, under the shared lock.~%")
        (chk "messages sent" nmsg *nmsg*)
        (chk "actor 2 forwarded" fwd *nmsg*)
        (chk "actor 3 logged" logged *nmsg*)
        (chk "acknowledgements the driver collected" got *nmsg*)
        (chk "acks with the wrong value" badack 0)
        (chk "actor 2 structural errors" badA 0)
        (chk "actor 3 structural errors" badB 0)
        (let ((bad 0) (i 1))
          (loop
            (when (> i *nmsg*) (return 0))
            (let ((e (+ logbase (* (- i 1) 32))))
              (if (= (%gc-read64 (+ e 24)) 1) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 e) i) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 (+ e 8)) (* i 7)) 0 (setq bad (+ bad 1)))
              (if (= (%gc-read64 (+ e 16)) (+ i 1000)) 0 (setq bad (+ bad 1))))
            (setq i (+ i 1)))
          (format t "  actor 3's log, read straight out of the band: message i~%")
          (format t "  must carry (i . (7i . i+1000)) after TWO round trips.~%")
          (chk "log entries wrong in any field" bad 0))

        (format t "~%=== EACH THREAD ALLOCATED IN ITS OWN REGION ==============~%")
        (format t "  thread 1's active region ~X (region 0)~%" t1reg)
        (format t "  thread 2's active region ~X~%" t2reg)
        (chk "thread 2's region is the one it was given" t2reg rcb3)
        (chk-true "which is NOT thread 1's" (not (= t2reg t1reg)))
        (chk "thread 2's allocation pointer started in its own from-space"
             t2alloc r2from)
        (format t "  collections: thread 2's region ~D, region 0 ~D -> ~D~%"
                g3 g0b g0a)
        (chk "thread 2's region collected" g3 0)
        (chk "region 0 collected" (- g0a g0b) 0)
        (format t "  (step 4 does not collect on purpose — forced, interleaved~%")
        (format t "   collections in BOTH threads are step 5.)~%")

        (format t "~%=== AND NEITHER REGION POINTS INTO THE OTHER =============~%")
        (chk "pointers from thread 2's live heap into thread 1's region" fr32 0)
        (chk "pointers from thread 1's region into thread 2's" fr23 0)
        (format t "  POSITIVE CONTROL: a zeroed window holding exactly ONE~%")
        (format t "  cons-tagged pointer into thread 2's from-space.~%")
        (chk "the oracle counts it" ctl3 1)
        (chk "and does not count it against the other region" ctl2 0)

        (format t "~%=== A CLEAN SHUTDOWN =====================================~%")
        (chk "an actor handed thread 2 back to its scheduler" handed 1)
        (chk "thread 2's scheduler ran to the end" clean 1)
        (chk "the join saw the kernel clear the TID word (0 = joined)" join 0)
        (format t "  thread 2 idle spins while the queue was empty: ~D~%" idlesp)
        (chk "and the mode word is back where it was" (%ha-percpu-mode) 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "ACTORS ON TWO THREADS: PASS (~D checks)~%" *checks*)
            (format t "ACTORS ON TWO THREADS: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
