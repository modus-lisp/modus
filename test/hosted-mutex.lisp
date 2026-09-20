;;;; hosted-mutex.lisp — A MUTEX AND A CONDITION VARIABLE THAT BLOCK.
;;;;
;;;;   ./modus --script test/hosted-mutex.lisp
;;;;
;;;; Everything the tree calls a lock is a SPIN.  net/actors.lisp's SPIN-LOCK
;;;; is TTAS on +OP-ATOMIC-XCHG+; the threaded selftests poll TRY-RECEIVE and
;;;; YIELD; the barriers spin on a counter.  That burns a core for the whole
;;;; wait, which is the wrong answer for anything a compositor will hold.
;;;;
;;;; net/hosted-sync.lisp adds a futex-backed mutex and condition variable, and
;;;; %SYNC-SELFTEST drives them from TWO clone(2) threads at once.
;;;;
;;;; WHAT WOULD MAKE THIS TEST A LIE, and what stops it.
;;;;
;;;;   "The counter reached 2N" proves nothing unless the SAME LOOP WITHOUT THE
;;;;   MUTEX reaches less.  So phase A runs exactly that: both threads
;;;;   read-modify-write one shared word N times with no mutex at all, and the
;;;;   test requires the result to be STRICTLY LESS than 2N.  If phase A ever
;;;;   scored 2N, phase B would be measuring nothing.
;;;;
;;;;   "Both threads ran" is checked the way every threaded test here checks
;;;;   it: a BARRIER with a spin budget.  A sequential run spins its budget out
;;;;   alone and reports a timeout, so the timeout count must be zero.
;;;;
;;;;   "%COND-WAIT returned" proves nothing either — a spin loop returns too.
;;;;   The waiting thread therefore measures its OWN consumed CPU time
;;;;   (CLOCK_THREAD_CPUTIME_ID) alongside the wall time across the wait, while
;;;;   the other thread deliberately sleeps a third of a second before
;;;;   signalling.  A spin shows the two roughly equal; a kernel park shows
;;;;   wall time passing and CPU time not.
;;;;
;;;;   "The thread allocated" would invalidate the whole run: the sibling has
;;;;   no region of its own here, so its R12 is a COPY of this thread's and any
;;;;   allocation hands out addresses both threads believe they own.  Entry and
;;;;   exit allocation pointers must be equal.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *n* 300000)

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

(let ((res (%sync-selftest *n*)))
  (if (= res 0)
      (format t "~%SKIP: no actor band, no thread page, or the thread would not spawn.~%")
      (let* ((tid    (w res #x00))
             (join   (w res #x08))
             (started (w res #x10))
             (done   (w res #x18))
             (n      (w res #x20))
             (unsync (w res #x28))
             (u2     (w res #x30))
             (u1     (w res #x38))
             (sync   (w res #x40))
             (s2     (w res #x48))
             (s1     (w res #x50))
             (cwall  (w res #x58))
             (ccpu   (w res #x60))
             (cwakes (w res #x68))
             (csaw   (w res #x70))
             (t2to   (w res #x78))
             (t1to   (w res #x80))
             (mfinal (w res #x88))
             (a0     (w res #x90))
             (a1     (w res #x98)))

        (format t "~%=== TWO THREADS, AND THEY REALLY OVERLAPPED ==============~%")
        (chk-true "clone(2) returned a TID" (> tid 0))
        (chk "the sibling thread entered its body" started 1)
        (chk "and reached the end of it" done 1)
        (chk "the join saw the kernel clear the TID word" join 0)
        (chk "barrier timeouts on this thread (a sequential run scores 3)" t1to 0)
        (chk "barrier timeouts on the sibling" t2to 0)

        (format t "~%=== THE NEGATIVE CONTROL: THE SAME LOOP, NO MUTEX ========~%")
        (format t "  Both threads did ~D unprotected read-modify-writes on ONE~%" n)
        (format t "  word.  A correct-by-accident run would score ~D.~%" (* 2 n))
        (format t "  thread 1 completed ~D iterations~%" u1)
        (format t "  thread 2 completed ~D iterations~%" u2)
        (format t "  the shared word ended at ~D~%" unsync)
        (format t "  LOST UPDATES: ~D~%" (- (* 2 n) unsync))
        (chk "each thread ran its full count (thread 1)" u1 n)
        (chk "each thread ran its full count (thread 2)" u2 n)
        (chk-true "and updates WERE lost — the race is real, so phase B has something to prove"
                  (< unsync (* 2 n)))

        (format t "~%=== THE SAME LOOP UNDER THE MUTEX ========================~%")
        (format t "  thread 1 completed ~D iterations~%" s1)
        (format t "  thread 2 completed ~D iterations~%" s2)
        (chk "each thread ran its full count (thread 1)" s1 n)
        (chk "each thread ran its full count (thread 2)" s2 n)
        (chk "and the protected counter is EXACT" sync (* 2 n))
        (chk "the mutex word is free again afterwards" mfinal 0)

        (format t "~%=== THE CONDITION VARIABLE BLOCKS, IT DOES NOT SPIN ======~%")
        (format t "  The sibling waited on the condvar while THIS thread slept~%")
        (format t "  300ms before flipping the predicate and signalling.~%")
        (format t "  sibling wall time in the wait: ~D ms~%" (floor cwall 1000000))
        (format t "  sibling CPU  time in the wait: ~D ms~%" (floor ccpu 1000000))
        (format t "  %COND-WAIT returns: ~D~%" cwakes)
        (chk "the sibling saw the predicate set" csaw 1)
        (chk-true "it really waited (>= 250ms of wall time)"
                  (>= cwall 250000000))
        (chk-true "and it was PARKED, not spinning (CPU time under 50ms)"
                  (< ccpu 50000000))
        (chk-true "and it woke at least once" (>= cwakes 1))

        (format t "~%=== THE SIBLING ALLOCATED NOTHING ========================~%")
        (format t "  It has no region of its own here, so its R12 is a COPY of~%")
        (format t "  this thread's; an allocation would double-hand-out.~%")
        (format t "  alloc ptr at entry ~X~%" a0)
        (format t "  alloc ptr at exit  ~X~%" a1)
        (chk "the sibling's alloc pointer moved by" (- a1 a0) 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "HOSTED x64 MUTEX + CONDVAR: PASS (~D checks)~%" *checks*)
            (format t "HOSTED x64 MUTEX + CONDVAR: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
