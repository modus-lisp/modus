;;;; hosted-intern-hop.lisp — IS THE `low' INSTABILITY THE REGION HOP?  NO.
;;;;
;;;;   ARM=hop|percall|perloop  K=<count>  ./modus --script test/hosted-intern-hop.lisp
;;;;
;;;; ============================================================
;;;; WHAT THIS BISECTS
;;;; ============================================================
;;;;
;;;; test/hosted-intern-layers.lisp's `low' arm — a worker looping on fresh
;;;; %INTERN-SYMBOL-PKG — dies about one run in two with
;;;; `MVM LONGJMP (TRAP #x0511) with no active handler-case' WHILE ITS AUDIT
;;;; READS ZERO, so it is not the cross-region defect.  It is not the JIT either
;;;; (3 of 6 with MODUS_NO_JIT=1, the same rate).  And it stands against
;;;; test/hosted-thread-lisp.lisp, which interns fresh symbols through the SAME
;;;; function, two threads, 300 iterations each, at 100 of 100.
;;;;
;;;; Same function, opposite outcomes, so the variable is something else.  Two
;;;; candidates were named: the REGION HOP that %RT-ENTER performs, taken once
;;;; per call here and once per ITERATION there; and AOT versus runtime-compiled
;;;; code.  Three arms, one worker, one loop shape, one process per run:
;;;;
;;;;   hop      K x (%rt-enter %rt-leave), no intern at all
;;;;   percall  K x %intern-symbol-pkg — each call hops in and out (today's low)
;;;;   perloop  ONE %rt-enter, K x %intern-symbol-pkg, ONE %rt-leave.  The inner
;;;;            enters are recursive depth bumps, so the region is hopped ONCE.
;;;;
;;;; ============================================================
;;;; MEASURED, K=20, ONE PROCESS PER RUN
;;;; ============================================================
;;;;
;;;;   hop       6 of 6 clean
;;;;   percall   1 of 4 clean   — 2 died, and 1 HUNG
;;;;   perloop   0 of 4 clean   — 4 died
;;;;
;;;; TWO OF THE THREE CANDIDATE EXPLANATIONS ARE DEAD.
;;;;
;;;;   THE HOP IS NOT THE KILLER.  20 enter/leave pairs on a worker, moving the
;;;;   active region to region 0 and back each time, is 6 of 6 clean.  Whatever
;;;;   kills `low' needs the INTERN, not the hop.
;;;;
;;;;   HOLDING THE LOCK ACROSS THE LOOP IS NOT THE FIX — IT IS STRICTLY WORSE.
;;;;   `perloop' is the shape hosted-thread-lisp uses (lock held across the
;;;;   work) and it is 0 of 4 against `percall''s 1 of 4.  So "lock-per-call
;;;;   versus lock-per-iteration" is not the variable that separates this file
;;;;   from that green test, and the surviving candidate is the other one: AOT
;;;;   versus runtime-compiled.  THAT IS NOT MEASURED — it needs a build that
;;;;   puts this loop inside the image, and no such build has been made.
;;;;
;;;;   AND A SURVIVING RUN IS NOT A CLEAN RUN.  ONE OBSERVATION, recorded as one
;;;;   observation and not as a rate: a `percall' run that completed and
;;;;   reported got=20 also reported the shared symbol table's COUNT, read on
;;;;   the MAIN thread afterwards, as having grown by **-1949** — from ~1978 to
;;;;   ~29.  The table was gutted.  `hop' on the same line reports +2 (the two
;;;;   the harness itself interns).  If that reproduces it is the best lead in
;;;;   this cluster, because it explains both the rate and the signature: a
;;;;   worker's PUTHASH destroys the shared intern table roughly when it forces
;;;;   the table's growth/rehash, and every later symbol lookup on the main
;;;;   thread then misses and signals — which is exactly
;;;;   `MVM LONGJMP (TRAP #x0511) with no active handler-case', arriving some
;;;;   time AFTER the operation that caused it.  It also says the `low' arm of
;;;;   hosted-intern-layers measures the wrong side: it samples the count on the
;;;;   WORKER, which sees +20, while main afterwards sees the wreck.  NOT YET
;;;;   REPRODUCED — survivors are about 1 run in 4, and the six runs attempted
;;;;   after that one were four deaths and two hangs.
;;;;
;;;;   A THIRD OUTCOME EXISTS AND WAS NOT PREVIOUSLY RECORDED: `percall' HANGS,
;;;;   1 run in 4, rather than dying.  A hang is not a variant of a fault; it is
;;;;   a lock left held or a lost wakeup, which is the same %MUTEX-LOCK 20 ms
;;;;   recovery park the mutex-handoff work is about.  Anything timing these
;;;;   arms must bound each run and classify hang separately from death, or the
;;;;   rate it reports is wrong.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE ARMS SHARE ONE THREAD, ONE LOOP AND ONE COUNT.  They differ in their
;;;;   body and in nothing else.
;;;;
;;;;   THE INTERN ARMS MUST REALLY HAVE INTERNED.  The shared table's COUNT is
;;;;   sampled on the main thread before and after and printed, so an arm that
;;;;   completed having interned nothing is visible rather than counted clean.
;;;;   (`hop' reports 1 — the one symbol the harness itself interns — and that
;;;;   is the point: it is the arm that does NOT intern.)
;;;;
;;;;   ONE RUN IS NOT A RESULT.  Read rates, and bound every run: see the hang.

(%ha-actors-bringup 4 0)

(defvar *arm* (let ((s (%cli-getenv "ARM"))) (if (and s (> (length s) 0)) s "percall")))
(defvar *k* (let ((s (%cli-getenv "K"))) (if (and s (> (length s) 0)) (parse-integer s) 20)))

(defun ih-count () (hash-table-count (mem-ref #x10000088 :u64)))

(defun ih-hop (k)
  (let ((i 0))
    (loop (when (>= i k) (return 0)) (%rt-enter) (%rt-leave) (setq i (+ i 1)))
    k))

(defun ih-percall (k base)
  (let ((i 0) (last nil))
    (loop (when (>= i k) (return 0))
      (setq last (%intern-symbol-pkg (+ base i) 0))
      (setq i (+ i 1)))
    (if last k 0)))

(defun ih-perloop (k base)
  ;; ONE hop for the whole loop.  The %RT-ENTER inside each %INTERN-SYMBOL-PKG
  ;; sees this thread as the owner already and only bumps the depth, so no
  ;; region is entered or left between iterations.
  (%rt-enter)
  (let ((i 0) (last nil))
    (loop (when (>= i k) (return 0))
      (setq last (%intern-symbol-pkg (+ base i) 0))
      (setq i (+ i 1)))
    (%rt-leave)
    (if last k 0)))

(defvar *c0* 0) (setq *c0* (ih-count))
(defvar *res* nil)
(setq *res*
      (let ((arm *arm*) (k *k*))
        (sb-thread:join-thread
         (sb-thread:make-thread
          (cond ((string= arm "hop")     (lambda () (ih-hop k)))
                ((string= arm "perloop") (lambda () (ih-perloop k 900000000)))
                (t                       (lambda () (ih-percall k 900000000))))
          :name "ih"))))

(format t "~&ARM=~a K=~d got=~s   shared table grew by ~d~%"
        *arm* *k* *res* (- (ih-count) *c0*))
(finish-output)
(sys-exit (if *res* 0 1))
