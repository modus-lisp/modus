;;;; hosted-thread-gc-concurrent.lisp — STEP 6: THE COLLECTION LOCK IS GONE,
;;;; AND THE COLLECTIONS REALLY DO OVERLAP IN TIME.
;;;;
;;;;   ./modus --script test/hosted-thread-gc-concurrent.lisp
;;;;
;;;; test/hosted-thread-gc.lisp already runs both threads with forced
;;;; collections of their own regions and audits isolation.  Every one of its
;;;; checks passes WITH THE LOCK STILL IN PLACE — which is the problem.  A test
;;;; that cannot tell a concurrent collector from a serialized one is not a test
;;;; of removing a lock.
;;;;
;;;; SO THIS TEST'S SUBJECT IS THE OVERLAP ITSELF, and it is measured where a
;;;; collection actually begins and ends: inside translate-x64's GC trampoline,
;;;; which is what a forced collection runs on hosted x86-64 (%GC-COLLECT-HERE
;;;; only pulls the allocation limit down so the next :gc-check calls it).  The
;;;; trampoline maintains four words with LOCKed instructions:
;;;;
;;;;   CUR      collectors currently inside.  Incremented after the register
;;;;            pushes, decremented at the restore label.  MUST end at 0.
;;;;   WITNESS  incremented when a collector enters and finds CUR >= 2 — i.e.
;;;;            another collector was ALREADY INSIDE.
;;;;   BARRIER  a spin budget.  A collector that is alone at entry re-reads CUR
;;;;            until a second one arrives or the budget is spent.
;;;;   MET      incremented when that wait SUCCEEDS.
;;;;
;;;; HOW OVERLAP IS ESTABLISHED, stated plainly: WITNESS > 0 means one thread's
;;;; collector was inside the trampoline at an instant when another thread's
;;;; already was.  MET > 0 means a collector waited at a barrier that only a
;;;; second concurrent collector could release.  Neither number can be produced
;;;; by running the two collections one after the other.
;;;;
;;;; AND THE NEGATIVE CONTROL IS THE OLD LOCK, IN THE SAME BINARY.  The run is
;;;; done TWICE on the same workload, one flag apart: concurrent, then with
;;;; %HA-SET-COLLECT-SERIALIZED putting the global collection lock back.  Under
;;;; the lock the second thread CANNOT enter the trampoline while the first is
;;;; inside, so WITNESS and MET are structurally 0 and every barrier times out.
;;;; The test asserts exactly that.  A serialized implementation therefore FAILS
;;;; the concurrent arm; that is what makes the concurrent arm's numbers
;;;; evidence.  (The barrier budget is BOUNDED so the serialized arm fails an
;;;; assertion rather than hanging.)
;;;;
;;;; WHAT ELSE IT ASSERTS, all of it now with no lock held anywhere:
;;;;   - the live chain on each thread still walks after every collection of its
;;;;     own region, and the message in flight survived the move;
;;;;   - both regions' collection counts rose INDEPENDENTLY to NMSG;
;;;;   - after the join, thread 2's live heap and control block are BIT-FOR-BIT
;;;;     unchanged across two more collections of thread 1's region — and the
;;;;     checksum is asserted NON-ZERO first, because a checksum over an empty
;;;;     range is a check that can only answer 0;
;;;;   - %GC-COUNT-FOREIGN-REFS is 0 in both directions with a positive control
;;;;     that returns non-zero;
;;;;   - THE BITMAP ALIGNMENT INVARIANT: every region initialised in this run is
;;;;     1024-byte aligned relative to the bitmap page_base, and so is region 0.
;;;;     That is not bookkeeping — it is the one hazard the old lock was
;;;;     silently covering.  translate-x64 sets object-start bits with an
;;;;     unLOCKed `BTS [base], idx' whose read-modify-write unit is EIGHT bitmap
;;;;     bytes = 1024 heap bytes, so two regions sharing such a unit can lose
;;;;     each other's bits, and a lost start bit means an object is silently not
;;;;     forwarded.  Before this pass the two carved regions' TO-spaces were
;;;;     512-aligned and their boundary sat inside one of those units.
;;;;
;;;; SCOPE, unchanged from step 5: the actors do arithmetic, raw memory access,
;;;; message passing and CONSING.  No FORMAT, no INTERN, no EVAL, no symbol or
;;;; keyword literal — the runtime's shared mutable tables are unsynchronised
;;;; under SMP and are NOT exercised here.  See mvm/gc.lisp, THE GLOBALS ROOT
;;;; SET UNDER CONCURRENT COLLECTION, for why that rule is the boundary between
;;;; what this pass fixed and what it cannot.

(defvar *fail* 0)
(defvar *checks* 0)
(defvar *nmsg* 48)
(defvar *nlinks* 300)
(defvar *budget* 200000000)
;; The in-collection barrier's SPIN BUDGET.  Big enough that the other thread
;; has time to finish a message and reach its own collection; bounded so the
;; serialized arm, where the other thread can never arrive, spends it and moves
;; on instead of hanging.
(defvar *barrier* 20000000)

(defun w (res off) (%gc-read64 (+ res off)))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (= got want)
      (format t "  ok   ~A = ~D~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected ~D)~%" name got want))))

(defun chk-pos (name got)
  (setq *checks* (+ *checks* 1))
  (if (> got 0)
      (format t "  ok   ~A = ~D (> 0)~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A = ~D (expected > 0)~%" name got))))

;; ===== THE ALIGNMENT ORACLE, WITH A POSITIVE CONTROL, FIRST =============
;; Everything below asserts the violation ledger is ZERO.  An oracle that can
;; only ANSWER zero is worth nothing — the same trap the "other region is
;; untouched" checksum fell into once already.  So prove it can answer
;; non-zero, and with the right bit, before believing its zeros.
(format t "~%THE ALIGNMENT ORACLE CAN ANSWER NON-ZERO~%")
(let ((c (%ha-align-control)))
  (if (zerop c)
      (format t "SKIP: the actor band could not be carved.~%")
      (progn
        (chk "aligned region -> no mask"      (w c #x00) 0)
        (chk "from-space +512 -> mask 1"      (w c #x08) 1)
        (chk "to-space   +512 -> mask 2"      (w c #x10) 2)
        (chk "size       +512 -> mask 4"      (w c #x18) 4)
        (chk "all three  +512 -> mask 7"      (w c #x20) 7)
        (chk "%GC-REGION-INIT counted 4 of 5" (w c #x28) 4)
        (chk "and recorded the last mask"     (w c #x30) 7))))

(format t "~%CONCURRENT COLLECTION UNDER TWO NATIVE THREADS~%")
(format t "  ~D messages, ~D-cons chain live on each side,~%" *nmsg* *nlinks*)
(format t "  a forced collection per message on each thread, NO LOCK.~%~%")

;; ================= ARM 1: CONCURRENT (the shipping path) ==================
(let ((out (%ha-mt-conc-selftest *nmsg* *budget* *nlinks* 0 *barrier*)))
  (if (zerop out)
      (format t "SKIP: the actor band could not be carved.~%")
      (let ((res (w out #x28)))
        (if (zerop res)
            (progn (setq *fail* (+ *fail* 1))
                   (format t "FAIL: the two-thread selftest returned 0~%"))
            (progn
              (format t "-- the run happened at all --~%")
              (chk "thread 2 joined"        (w res #x00) 0)
              (chk "acks collected"         (w res #x20) *nmsg*)
              (chk "actor 2 forwarded"      (w res #x30) *nmsg*)
              (chk "actor 3 logged"         (w res #x38) *nmsg*)
              (chk "actor 2 struct errors"  (w res #x40) 0)
              (chk "actor 3 struct errors"  (w res #x48) 0)
              (chk "driver bad acks"        (w res #x50) 0)
              (chk "ap-scheduler entries"   (w res #x58) 0)
              (chk "the actors ran on cpu 1" (w res #x60) 0)
              (chk-pos "actor 2 ticks on cpu 1" (w res #x68))
              (chk "barrier: driver met t2" (w res #xC0) 0)
              (chk "barrier: t2 met driver" (w res #xB8) 0)

              (format t "~%-- BOTH THREADS COLLECTED, INDEPENDENTLY --~%")
              (chk "thread 1 forced collections" (w out #x40) *nmsg*)
              (chk "thread 2 forced collections" (w out #x48) *nmsg*)
              (chk "thread 2 region count"       (w res #x168) *nmsg*)
              (chk "thread 1 region count"       (w res #x170) *nmsg*)
              (chk "region 0 collected"          (w res #x118) (w res #x110))

              (format t "~%-- THE COLLECTIONS OVERLAPPED IN TIME --~%")
              (format t "  (WITNESS and MET are taken inside the trampoline;~%")
              (format t "   neither can be non-zero if the two ran in sequence)~%")
              (chk-pos "overlap witnesses (entered while another was inside)"
                       (w out #x00))
              (chk-pos "barrier meetings (waited, a second collector arrived)"
                       (w out #x08))
              (chk "collectors still inside at the end" (w out #x10) 0)

              (format t "~%-- NOTHING WAS CORRUPTED BY RUNNING THEM AT ONCE --~%")
              (chk "actor 1 chain-survival failures" (w res #x150) 0)
              (chk "actor 2 chain-survival failures" (w res #x140) 0)
              (chk "actor 3 chain-survival failures" (w res #x148) 0)
              (chk-pos "thread 2 live-heap checksum is real" (w res #x1D0))
              (chk "thread 2 live heap unchanged"
                   (w res #x1D8) (w res #x1D0))
              (chk-pos "thread 2 control-block checksum is real" (w res #x1E0))
              (chk "thread 2 control block unchanged"
                   (w res #x1E8) (w res #x1E0))
              (chk "thread 2 count unchanged"  (w res #x1F8) (w res #x1F0))
              (chk "thread 1 count +2"         (w res #x208) (+ (w res #x200) 2))
              (chk "region 0 frontier restored" (w res #x1C0) (w res #x1B8))

              (format t "~%-- ISOLATION, WITH A POSITIVE CONTROL --~%")
              (chk "refs from thread 2's heap into thread 1's" (w res #x188) 0)
              (chk "refs from thread 1's heap into thread 2's" (w res #x190) 0)
              (chk "positive control (one planted pointer)"    (w res #x198) 1)
              (chk "control aimed at the other region"         (w res #x1A0) 0)

              (format t "~%-- THE BITMAP ALIGNMENT INVARIANT --~%")
              (format t "  (1024 bytes relative to page_base: translate-x64's~%")
              (format t "   unLOCKed BTS read-modify-writes 8 bitmap bytes)~%")
              (chk "regions initialised with a violation" (w out #x18) 0)
              (chk "last violation mask"                  (w out #x20) 0)
              (chk "region 0's own alignment mask"        (w out #x60) 0)

              ;; MEASURED AND REPORTED, DELIBERATELY NOT ASSERTED.  Heap-window
              ;; uniformity is a precondition of the LISP collector's torn-read
              ;; argument (mvm/gc.lisp, THE GLOBALS ROOT SET UNDER CONCURRENT
              ;; COLLECTION, THE READ SIDE) and of NOTHING on this target: the
              ;; native trampoline rewrites a slot with one naturally-aligned
              ;; 8-byte store, which cannot tear.  And it is NOT A PROPERTY THIS
              ;; TEST CONTROLS — the heap is a ~1.8 GB mmap and ASLR places it
              ;; where it likes, so it straddles a 4 GiB boundary in about a
              ;; third of runs (measured: uniform in 26 of 40).  Asserting it
              ;; would make this test flaky for a reason that has nothing to do
              ;; with the collector.
              (format t "~%-- THE PER-COLLECTION SCRATCH BLOCK IS PER CPU --~%")
              ;; THE ADDRESSING, EXERCISED ON THE TARGET THAT HAS THREADS.
              ;; The three per-collection words this pass moved belong to the
              ;; LISP collector, and hosted x86-64 does not run it — it collects
              ;; in the native trampoline, whose per-collection state is in
              ;; REGISTERS.  So the COLLECTOR side is verified elsewhere (the
              ;; aarch64 shim path, under QEMU).  What can be verified HERE, and
              ;; is, is the part that could actually be wrong under two threads:
              ;; that %GC-SCRATCH-CELL resolves to a DIFFERENT block on each,
              ;; and that both lie in the installed array.
              (chk-pos "per-CPU scratch array installed" (w out #x78))
              (chk "thread 1's scratch cell is entry 0"
                   (w out #x68) (w out #x78))
              (chk "thread 2's scratch cell is entry 1"
                   (w out #x70) (+ (w out #x78) 32))
              (format t "~%-- NOTES --~%")
              (format t "  note thread 1's region in one 4 GiB window = ~D~%"
                      (w out #x50))
              (format t "  note thread 2's region in one 4 GiB window = ~D~%"
                      (w out #x58))
              (format t "       (ASLR-dependent; bounds the LISP collector's~%")
              (format t "        torn-read argument, not this target's)~%"))))))

;; ============== ARM 2: THE NEGATIVE CONTROL — PUT THE LOCK BACK ============
(format t "~%~%NEGATIVE CONTROL: THE SAME RUN WITH THE OLD LOCK BACK ON~%")
(format t "  If the collections were serialized, the overlap evidence must~%")
(format t "  VANISH.  If it does not, the evidence proves nothing.~%~%")
(let ((out (%ha-mt-conc-selftest *nmsg* *budget* *nlinks* 1 *barrier*)))
  (if (zerop out)
      (format t "SKIP: the actor band could not be carved.~%")
      (let ((res (w out #x28)))
        (if (zerop res)
            (progn (setq *fail* (+ *fail* 1))
                   (format t "FAIL: the serialized run returned 0~%"))
            (progn
              (chk "serialized: the run still completed" (w res #x00) 0)
              (chk "serialized: acks collected"   (w res #x20) *nmsg*)
              (chk "serialized: thread 1 collections" (w out #x40) *nmsg*)
              (chk "serialized: thread 2 collections" (w out #x48) *nmsg*)
              (chk "serialized: chain-survival failures"
                   (+ (w res #x140) (+ (w res #x148) (w res #x150))) 0)
              (format t "~%  and the overlap evidence is GONE:~%")
              (chk "serialized: overlap witnesses" (w out #x00) 0)
              (chk "serialized: barrier meetings"  (w out #x08) 0)
              (chk "serialized: collectors inside at end" (w out #x10) 0))))))

(format t "~%")
(if (zerop *fail*)
    (format t "CONCURRENT COLLECTION, NO LOCK: PASS (~D checks)~%" *checks*)
    (format t "CONCURRENT COLLECTION, NO LOCK: FAIL (~D of ~D checks)~%"
            *fail* *checks*))
