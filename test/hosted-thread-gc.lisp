;;;; hosted-thread-gc.lisp — STEP 5: COLLECTION UNDER TWO NATIVE THREADS.
;;;;
;;;;   ./modus --script test/hosted-thread-gc.lisp
;;;;
;;;; The same two-thread actor system as test/hosted-thread-actors.lisp, with
;;;; the one thing that step deliberately did not do: BOTH THREADS FORCE
;;;; COLLECTIONS OF THEIR OWN REGIONS, interleaved, while holding live data.
;;;;
;;;; THE COLLECTOR IS NOT REENTRANT, AND THIS DOES NOT PRETEND OTHERWISE.
;;;; Per-collection working state lives at three FIXED, SHARED addresses with no
;;;; per-region or per-thread copy: 0x10000100 (the scratch word
;;;; %GC-FORWARD-SLOT / %GC-STORE-TAGGED / %GC-MOVE-WORD use once per forwarded
;;;; slot), 0x10000108 (tmp-free, the copying allocator's next free pointer) and
;;;; 0x10000110 (to-end, the bound %GC-COPY-OBJECT's overrun guard reads).
;;;; Separately, %GC-SCAN-GLOBALS forwards a SHARED ROOT SET — the globals
;;;; alist, the symbol/keyword/package intern tables, the multiple-value extras
;;;; — that EVERY region scans, and forwarding REWRITES those slots.  Two
;;;; collections at once would each corrupt the other.
;;;;
;;;; SO EVERY COLLECTION IS SERIALIZED behind a GLOBAL COLLECTION LOCK
;;;; (%HA-LOCKED-COLLECT-HERE), taken around the whole of the collection.  That
;;;; is an EXPLICIT PLACEHOLDER: making the collector reentrant is a dedicated
;;;; pass that comes after threads, and this lock is what that pass removes.  It
;;;; buys correctness, not parallelism — the threads block each other for the
;;;; duration of each collection.  It is NOT the scheduler lock, deliberately:
;;;; +OP-RESTORE-CTX+ zeroes the scheduler lock on every context switch, so a
;;;; collection lock sharing that word would be released by any switch.
;;;;
;;;; WHO COLLECTS WHAT, and why actor 3 does not.  A region's root window is
;;;; [live SP, its stack_base).  Thread 2's region's stack_base is the TOP of
;;;; the actor-stack slice its two actors live in, so a collection triggered
;;;; from ACTOR 2 — whose stack is the LOWER of the two — covers actor 2's live
;;;; frames AND the whole of actor 3's stack, i.e. both actors' roots.
;;;; Triggered from actor 3 it would start ABOVE actor 2's frames and miss
;;;; actor 2's chain entirely.  So actor 2 forces, actor 3 holds its chain live
;;;; across actor 2's collections and checks it every message.  Thread 1's
;;;; region has the PROCESS stack base, and its actor (the primordial one) runs
;;;; on the process stack, so its window is right by construction.
;;;;
;;;; WHAT IS ASSERTED:
;;;;   - each thread's live chain still walks after every collection of its own
;;;;     region, and the message in flight survived the move (it is re-checked
;;;;     AFTER the collection of the region it was decoded into);
;;;;   - the counts rise INDEPENDENTLY — one per message on thread 2, one per
;;;;     send on thread 1 — and neither thread's count is the other's;
;;;;   - the other thread's region is UNTOUCHED, heap AND control block, by a
;;;;     collection of this one.  That is measured AFTER THE JOIN, because while
;;;;     thread 2 is still collecting its own region those checksums are
;;;;     supposed to move; with thread 2 gone, this thread collects its own
;;;;     region twice and thread 2's heap and control block must be bit-for-bit
;;;;     identical across it;
;;;;   - %GC-COUNT-FOREIGN-REFS is 0 in BOTH directions, WITH A POSITIVE CONTROL
;;;;     that returns non-zero — an oracle that can only answer zero is
;;;;     worthless.
;;;;
;;;; SCOPE.  The actors do arithmetic, raw memory access and message passing.
;;;; No FORMAT, no INTERN, no EVAL, no symbol or keyword literal — the runtime's
;;;; shared mutable tables are unsynchronised and are NOT exercised here.  What
;;;; they do allocate is conses (TERM-DECODE-STEP builds each message in the
;;;; receiver's heap, and %GC-CHAIN-BUILD builds the live chains), which is
;;;; exactly what the per-thread regions are for.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nmsg* 48)
(defvar *nlinks* 300)
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

(let ((res (%ha-mt-selftest *nmsg* *budget* *nlinks*)))
  (if (= res 0)
      (format t "~%SKIP: no actor band, or the thread stack could not be mapped.~%")
      (let* ((join    (w res #x00))
             (got     (w res #x20))
             (nmsg    (w res #x28))
             (fwd     (w res #x30))
             (logged  (w res #x38))
             (badA    (w res #x40))
             (badB    (w res #x48))
             (badack  (w res #x50))
             (idle    (w res #x58))
             (a2c1    (w res #x68))
             (a3c1    (w res #x78))
             (a1c0    (w res #x80))
             (a1c1    (w res #x88))
             (seen    (w res #xB0))
             (t2to    (w res #xB8))
             (t1to    (w res #xC0))
             (t2reg   (w res #xE8))
             (rcb3    (w res #xF0))
             (logbase (w res #x100))
             (clean   (w res #x130))
             (nlinks  (w res #x138))
             (chA     (w res #x140))
             (chB     (w res #x148))
             (ch1     (w res #x150))
             (colA    (w res #x158))
             (col1    (w res #x160))
             (g3      (w res #x168))
             (g2      (w res #x170))
             (tref    (w res #x178))
             (fr32    (w res #x188))
             (fr23    (w res #x190))
             (ctl3    (w res #x198))
             (ctl2    (w res #x1A0))
             (a0      (w res #x1B8))
             (a1      (w res #x1C0))
             (endreg  (w res #x1C8))
             (sum3b   (w res #x1D0))
             (sum3a   (w res #x1D8))
             (ctlb    (w res #x1E0))
             (ctla    (w res #x1E8))
             (g3b     (w res #x1F0))
             (g3a     (w res #x1F8))
             (g2b     (w res #x200))
             (g2a     (w res #x208)))

        (format t "~%=== THE RUN ==============================================~%")
        (chk "messages" nmsg *nmsg*)
        (chk "chain length each side held live" nlinks *nlinks*)
        (chk "actor 2 forwarded" fwd *nmsg*)
        (chk "actor 3 logged" logged *nmsg*)
        (chk "acknowledgements the driver collected" got *nmsg*)
        (chk "acks with the wrong value" badack 0)
        (chk "the hosted AP-SCHEDULER was never reached" idle 0)
        (chk-true "actor 1 ran on thread 1" (> a1c0 0))
        (chk "and never on thread 2" a1c1 0)
        (chk-true "actor 2 ran on thread 2" (> a2c1 0))
        (chk-true "actor 3 ran on thread 2" (> a3c1 0))
        (chk "thread 1 spun out the barrier alone (1 = yes)" t1to 0)
        (chk "thread 2 spun out the barrier alone (1 = yes)" t2to 0)
        (chk-true "the driver watched actor 2 advance while it polled"
                  (> seen 0))

        (format t "~%=== BOTH THREADS COLLECTED, INTERLEAVED ==================~%")
        (format t "  thread 1 forced ~D collections of its own region ~X~%" col1 tref)
        (format t "  thread 2 forced ~D collections of its own region ~X~%" colA rcb3)
        (chk "thread 1's forced collections" col1 *nmsg*)
        (chk "thread 2's forced collections" colA *nmsg*)
        (chk-true "thread 1's region really collected that many times"
                  (>= g2 *nmsg*))
        (chk-true "thread 2's region really collected that many times"
                  (>= g3 *nmsg*))
        (format t "  region counts at the join: thread 1 ~D, thread 2 ~D~%" g2 g3)
        (chk "thread 2's active region is the one it was given" t2reg rcb3)

        (format t "~%=== EACH THREAD'S DATA SURVIVED ==========================~%")
        (format t "  Each side holds a ~D-cons chain live in its own frame~%" nlinks)
        (format t "  across every collection of its own region, and re-checks~%")
        (format t "  the message it is holding AFTER the collection of the~%")
        (format t "  region that message was decoded into.~%")
        (chk "actor 2 chain-survival failures" chA 0)
        (chk "actor 3 chain-survival failures" chB 0)
        (chk "thread 1 chain-survival failures" ch1 0)
        (chk "actor 2 structural errors (message shape, before AND after GC)"
             badA 0)
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
          (chk "log entries wrong in any field, after all that collecting" bad 0))

        (format t "~%=== THE OTHER THREAD'S REGION IS UNTOUCHED ===============~%")
        (format t "  Measured AFTER the join: while thread 2 is still collecting~%")
        (format t "  its own region those checksums are SUPPOSED to move.  With~%")
        (format t "  thread 2 gone, this thread collects ITS region twice.~%")
        (format t "  thread 2 heap checksum  ~D -> ~D~%" sum3b sum3a)
        (format t "  thread 2 control block  ~D -> ~D~%" ctlb ctla)
        (chk-true "thread 2's live heap is a REAL span, not an empty one"
                  (> sum3b 0))
        (chk "thread 2's live heap across two collections of thread 1's"
             sum3a sum3b)
        (chk "thread 2's control block across them" ctla ctlb)
        (chk "and thread 2's collection count" g3a g3b)
        (format t "  meanwhile thread 1's own count ~D -> ~D~%" g2b g2a)
        (chk "thread 1's count rose by exactly the two it forced"
             (- g2a g2b) 2)

        (format t "~%=== NEITHER REGION POINTS INTO THE OTHER =================~%")
        (chk "pointers from thread 2's live heap into thread 1's region" fr32 0)
        (chk "pointers from thread 1's live heap into thread 2's region" fr23 0)
        (format t "  POSITIVE CONTROL: a zeroed window holding exactly ONE~%")
        (format t "  cons-tagged pointer into thread 2's from-space.  Without~%")
        (format t "  this the two zeros above would prove nothing.~%")
        (chk "the oracle counts it" ctl3 1)
        (chk "and does not count it against thread 1's region" ctl2 0)

        (format t "~%=== A CLEAN SHUTDOWN =====================================~%")
        (chk "thread 2's scheduler ran to the end" clean 1)
        (chk "the join saw the kernel clear the TID word (0 = joined)" join 0)
        (format t "  driver alloc ptr before ~X, after ~X~%" a0 a1)
        (chk "%GC-REGION-ENTER put the driver's region-0 frontier back" (- a1 a0) 0)
        (chk "and the driver is back in region 0" endreg (%gc-region-0))
        (chk "the per-CPU mode word is back where it was" (%ha-percpu-mode) 0)

        (format t "~%=== VERDICT ==============================================~%")
        (if (= *fail* 0)
            (format t "COLLECTION UNDER TWO THREADS: PASS (~D checks)~%" *checks*)
            (format t "COLLECTION UNDER TWO THREADS: FAIL (~D of ~D checks)~%"
                    *fail* *checks*)))))
