;;;; hosted-thread-lisp.lisp — TWO THREADS RUNNING REAL LISP AT ONCE.
;;;;
;;;;   ./modus --script test/hosted-thread-lisp.lisp
;;;;
;;;; Every threaded selftest before this one was restricted to "arithmetic, raw
;;;; memory access and message passing only — no FORMAT, INTERN, EVAL, or
;;;; symbol/keyword literal".  That restriction was the CEILING, not an
;;;; accident: the globals alist, the symbol / keyword / package intern tables
;;;; and the macro expander table are shared mutable state with no
;;;; synchronisation, so a second thread could run computation but not Lisp.
;;;; Any real workload interns symbols.
;;;;
;;;; This is that workload, on both threads at once, with forced collections
;;;; running on both sides.
;;;;
;;;; WHAT EACH THREAD DOES, EVERY ITERATION:
;;;;   1. interns a fresh symbol of its own TWICE and requires the two to be EQ;
;;;;   2. interns a SHARED name — one both threads intern — and records the raw
;;;;      machine word it got back;
;;;;   3. calls FORMAT (a string in its own region; a keyword interned in the
;;;;      shared table for every `:foo' the printer evaluates);
;;;;   4. writes and reads back a global through the globals hash table;
;;;;   5. defines a function under a FRESHLY CONSTRUCTED name in the shared
;;;;      symbol-function table and calls it back by name;
;;;;   6. conses onto a live chain, and every few iterations FORCES A COLLECTION
;;;;      OF ITS OWN REGION and re-checks that the chain still checksums and
;;;;      that re-interning the shared name still returns the SAME object.
;;;;
;;;; WHAT WOULD MAKE THIS TEST A LIE, and what stops it.
;;;;
;;;;   "It completed" is not a result, so nothing here is asserted from a flag a
;;;;   worker set about itself.  The shared-symbol identities are RAW MACHINE
;;;;   WORDS recorded independently by the two threads and compared afterwards —
;;;;   and the DRIVER re-interns every one of them itself and requires the same
;;;;   answer, so this is not two workers agreeing with each other about a table
;;;;   neither can see.
;;;;
;;;;   "Both threads ran" is a barrier with a spin budget.  A sequential run
;;;;   spins its budget out alone and reports a timeout; both timeouts must be 0.
;;;;
;;;;   "The counts rose" is per thread and independent: each must reach N.
;;;;
;;;;   THE PRECONDITION IS MEASURED.  Region 0 is the shared runtime heap while
;;;;   the runtime lock is held, and it must not collect — its root window ends
;;;;   at the PROCESS stack base, which is not where thread 2's roots are.  The
;;;;   driver requires region 0's collection count UNCHANGED and both threads'
;;;;   own regions to have collected several times.  If the workload ever grew
;;;;   big enough to collect region 0 this test would FAIL rather than corrupt.
;;;;
;;;;   AND THE NEGATIVE CONTROL IS ONE WORD, in the same binary, on the same
;;;;   workload: test/hosted-thread-lisp-unsync.lisp leaves the threads-live
;;;;   gate OFF, so %RT-ENTER and %RT-LEAVE become the no-ops they are in a
;;;;   single-threaded image.  It is a separate script because a corrupted
;;;;   intern table is not obliged to fail politely.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *n* 300)
(defvar *gcevery* 25)

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

(let ((res (%tl-selftest 0 *n* *gcevery*)))
  (if (= res 0)
      (format t "~%SKIP: no actor band, no thread page, or the arrays would not map.~%")
      (progn
        (format t "~%=== TWO THREADS, AND THEY OVERLAPPED =====================~%")
        (chk-true "clone(2) returned a TID" (> (w res #x60) 0))
        (chk "thread 2 entered its body" (w res #x68) 1)
        (chk "thread 2 reached the end of it" (w res #x70) 1)
        (chk "the join saw the kernel clear the TID word" (w res #x08) 0)
        (format t "  A SEQUENTIAL run scores 1 on each of the next two: the~%")
        (format t "  first arrival would spin out its whole budget alone.~%")
        (chk "thread 1 spun out the barrier alone" (w res #x80) 0)
        (chk "thread 2 spun out the barrier alone" (w res #x78) 0)
        (chk "thread 1's cpu id" (w res #xF0) 0)
        (chk "thread 2's cpu id" (w res #xF8) 1)

        (format t "~%=== COUNTS ROSE INDEPENDENTLY ============================~%")
        (chk "thread 1 iterations" (w res #xA0) (w res #x50))
        (chk "thread 2 iterations" (w res #xA8) (w res #x50))

        (format t "~%=== NO LOST INTERNS ======================================~%")
        (format t "  ~D names, interned by BOTH threads.  Each thread recorded~%"
                (w res #x50))
        (format t "  the machine word it got; the driver then re-interned every~%")
        (format t "  one itself.  All three answers must agree, name by name.~%")
        (chk "shared names where the two threads got DIFFERENT objects"
             (w res #x30) 0)
        (chk "shared names where the DRIVER got a different object again"
             (w res #x38) 0)
        (chk "recorded words that were zero (never interned at all)"
             (w res #x48) 0)
        (format t "  And the symbols are DISTINCT — a table that handed out one~%")
        (format t "  object for every name would pass the identity check above.~%")
        (chk "consecutive names that resolved to the SAME object" (w res #x40) 0)
        (chk "own-name re-intern failures (a thread's own symbol, twice)"
             (w res #xB0) 0)

        (format t "~%=== AND IT WAS REAL LISP =================================~%")
        (chk "FORMAT failures" (w res #xB8) 0)
        (chk "global write/read-back failures" (w res #xC8) 0)
        (chk "shared function-table define-and-call failures" (w res #xD0) 0)

        (format t "~%=== WITH FORCED COLLECTIONS ON BOTH SIDES ================~%")
        (format t "  thread 1 forced ~D collections of its own region~%" (w res #xE0))
        (format t "  thread 2 forced ~D collections of its own region~%" (w res #xE8))
        (chk-true "thread 1's region really collected" (> (w res #x10) 0))
        (chk-true "thread 2's region really collected" (> (w res #x18) 0))
        (chk "live chains that did not survive a collection" (w res #xC0) 0)
        (chk "shared symbols whose identity changed across a collection"
             (w res #xD8) 0)

        (format t "~%=== THE PRECONDITION, MEASURED ===========================~%")
        (format t "  Region 0 is the shared runtime heap while the lock is held.~%")
        (format t "  Its root window ends at the PROCESS stack base, which is~%")
        (format t "  not where thread 2's roots are, so it must not collect.~%")
        (chk "region 0's collection count before" (w res #x20) (w res #x28))

        (format t "~%=== THE LOCK WAS ACTUALLY USED ===========================~%")
        (format t "  acquisitions ~D, of which ~D had to wait for the other thread~%"
                (w res #x88) (w res #x90))
        (chk-true "the runtime lock was acquired" (> (w res #x88) 0))
        (chk-true "and it was CONTENDED — the two threads really met in it"
                  (> (w res #x90) 0))
        (chk "the hosted AP-SCHEDULER was never reached" (w res #x98) 0)
        (format t "  Both threads reached the end of the workload (99 = done):~%")
        (chk "thread 1's last phase marker" (w res #x118) 99)
        (chk "thread 2's last phase marker" (w res #x120) 99)
        (format t "  %MUTEX-LOCK parks with a 20ms timeout so a lost wake would~%")
        (format t "  be a re-check rather than a hang.  The net must never have~%")
        (format t "  caught anything: a non-zero count is a lost wake.~%")
        (chk "parked waits that timed out instead of being woken" (w res #x110) 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "TWO THREADS RUNNING REAL LISP: PASS (~D checks)~%" *checks*)
            (format t "TWO THREADS RUNNING REAL LISP: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
