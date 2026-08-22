;;;; hosted-actors-post.lisp — the LAST-DEFUN-WINS half of the hosted x86-64
;;;; actor adapter.  Baked immediately AFTER net/actors.lisp, so what is here
;;;; overrides what is there; net/hosted-actors.lisp (the address hooks) is
;;;; baked immediately BEFORE it, because a forward reference to an address
;;;; hook would resolve to the NIL sentinel.
;;;;
;;;; One override (AP-SCHEDULER), one DELETED override (SPIN-LOCK — see below),
;;;; and then the actor-level selftests for steps C and D.

;;; ============================================================
;;; SPIN-LOCK / SPIN-UNLOCK — REAL, as of native threads
;;; ============================================================
;;;
;;; THERE IS NO OVERRIDE HERE ANY MORE, and its removal is the point.  This
;;; file used to define `(defun spin-lock (addr) 0)' / `(defun spin-unlock
;;; (addr) 0)', so net/actors.lisp's own TTAS lock on +OP-ATOMIC-XCHG+ never
;;; ran in a hosted image.  The reason was NOT "one core needs no lock" — it
;;; was that keeping the lock DEADLOCKED: net/actors.lisp hands the RELEASE to
;;; RESTORE-CONTEXT (YIELD's resume arm is commented "lock already released by
;;; restore-context"), aarch64's +OP-RESTORE-CTX+ honours that by storing zero
;;; to *AARCH64-SCHED-LOCK-ADDR*, and translate-x64's did not — so the lock
;;; stayed held across every switch and the next acquire spun forever.
;;;
;;; THAT IS NOW FIXED AT THE SOURCE, not worked around.  translate-x64 has
;;; *X64-SCHED-LOCK-ADDR*, the exact twin of the aarch64 knob, and its
;;; +OP-RESTORE-CTX+ stores zero to it AFTER `mov rsp,[base]' and BEFORE the
;;; jump — the same instant aarch64 releases, and the earliest instant that is
;;; safe.  Earlier is NOT safe: by the time YIELD reaches RESTORE-CONTEXT the
;;; OUTGOING actor is already back on the run queue, so releasing before the
;;; stack switch would let a second thread dequeue it and restore onto a stack
;;; this CPU has not left yet.  Later is not available: after the jump we are
;;; running arbitrary actor code, including a FRESHLY SPAWNED actor entered at
;;; its raw entry point, which has no idea a lock is outstanding.
;;;
;;; THE PAYOFF, which is why "the arriving code releases" was rejected as a
;;; Lisp-level protocol: releasing inside the switch means net/actors.lisp needs
;;; NO EDIT AT ALL.  Its resume arms are already written for this contract —
;;; YIELD's does nothing, RECEIVE's re-acquires — and a fresh actor's entry
;;; function is covered without a trampoline or an extra struct slot.
;;;
;;; The lock's address is +HOSTED-SCHED-LOCK-ADDR+, a fixed BSS word, because
;;; the release is baked into the instruction stream at translate time; see
;;; SCHED-LOCK-ADDR in net/hosted-actors.lisp.

;;; ============================================================
;;; AP-SCHEDULER — the bare-metal idle loop has no hosted meaning
;;; ============================================================
;;;
;;; net/actors.lisp's version switches to a per-CPU idle stack (MVM trap
;;; #x0400, which translate-x64 does not decode and falls through to a real
;;; `INT 0x30'), then loops on CLI / STI+HLT.  All three are privileged
;;; instructions; in a Linux process they fault.
;;;
;;; It is reached when an actor blocks and the run queue is EMPTY.  On bare
;;; metal that is an idle CPU waiting for an interrupt.  In a hosted process
;;; with no interrupt source it is a DEADLOCK — nothing can ever make an actor
;;; runnable again — so the honest hosted behaviour is to count the event and
;;; return, letting the caller's test report a wrong answer instead of hanging
;;; on an illegal instruction.  Every selftest below asserts the count is 0.
(defun ap-scheduler ()
  (let ((c (+ (%ha-base) #x190)))
    (%gc-write64 c (+ (%gc-read64 c) 1)))
  (set-current-actor 0)
  0)

;;; ============================================================
;;; STEP 1 ACCEPTANCE — the lock, and who releases it
;;; ============================================================
;;;
;;; The COROUTINE half.  Entered by RESTORE-CONTEXT with RSP on the id-0 actor
;;; stack slot (see %HA-CO-STACK-TOP).  Its ONE job is to look at the scheduler
;;; lock word from the ARRIVING side of a context switch — the driver held it
;;; when it switched, so a zero here is the +OP-RESTORE-CTX+ release and nothing
;;; else — and then switch straight back.
(defun %ha-lock-co-body ()
  (let ((a *ha-band*)
        (b (+ *ha-band* #x40))
        (obs (+ *ha-band* #x560)))
    (loop
      (%gc-write64 obs (%gc-read64 (sched-lock-addr)))
      (if (zerop (save-context b))
          (restore-context a)
          0))))

(defun %ha-lock-selftest (n)
  "STEP-1 ACCEPTANCE.  net/actors.lisp's REAL SPIN-LOCK/SPIN-UNLOCK — the TTAS
   pair built on +OP-ATOMIC-XCHG+ (a genuine `XCHG [mem], reg', which x86
   locks implicitly) — running in a hosted Linux process for the first time,
   plus the thing that used to make that impossible: a context switch RELEASING
   the lock.

   Returns the result block's raw byte address, or 0 if the band could not be
   carved.  Every number is read back out of memory; this function asserts
   nothing itself.

     res+0x00  the lock word before anything touched it   (must be 0: BSS)
     res+0x08  the lock word while HELD                   (must be 1)
     res+0x10  what an XCHG sees while held               (must be 1 — the
               oracle has to be able to say HELD, or it proves nothing)
     res+0x18  the lock word after SPIN-UNLOCK            (must be 0)
     res+0x20  what an XCHG sees while free               (must be 0)
     res+0x28  N, echoed
     res+0x30  round trips where the word was wrong       (must be 0)
     res+0x38  the lock word immediately BEFORE a switch  (must be 1)
     res+0x40  the lock word as the ARRIVING side sees it (must be 0)
     res+0x48  the lock word once control comes back      (must be 0)
     res+0x50  the lock's address, so the test can check it is the BSS word
               translate-x64 was told about and not a band offset"
  (if (zerop (%ha-carve))
      0
      (let* ((band *ha-band*)
             (lk (sched-lock-addr))
             (res (+ band #x500))
             (a band)
             (b (+ band #x40))
             (sa (+ band #x80))
             (obs (+ band #x560))
             (stk (%ha-co-stack-top))
             (i 0)
             (bad 0))
        (%ha-zero res (+ res #x80))
        (%gc-write64 lk 0)
        (%gc-write64 res (%gc-read64 lk))
        ;; ---- acquire, and look at the word underneath ----
        (spin-lock lk)
        (%gc-write64 (+ res #x08) (%gc-read64 lk))
        (%gc-write64 (+ res #x10) (xchg-mem lk 1))
        ;; ---- release, and look again ----
        (spin-unlock lk)
        (%gc-write64 (+ res #x18) (%gc-read64 lk))
        (%gc-write64 (+ res #x20) (xchg-mem lk 1))
        (spin-unlock lk)
        ;; ---- N uncontended round trips ----
        (loop
          (when (>= i n) (return 0))
          (spin-lock lk)
          (if (= (%gc-read64 lk) 1) 0 (setq bad (+ bad 1)))
          (spin-unlock lk)
          (if (zerop (%gc-read64 lk)) 0 (setq bad (+ bad 1)))
          (setq i (+ i 1)))
        (%gc-write64 (+ res #x28) n)
        (%gc-write64 (+ res #x30) bad)
        ;; ---- THE SWITCH RELEASES IT ----
        ;; Launch state for the coroutine, exactly as %HA-CTX-SELFTEST builds
        ;; it: RSP = the id-0 stack slot, RBX/RBP zero, continuation =
        ;; %HA-LOCK-CO-BODY's native entry with FN-ADDR's OR-3 tag taken off.
        (%ha-zero a (+ b #x40))
        (%gc-write64 obs 12345)
        (%gc-write64 b stk)
        (%gc-write64 (+ b #x18) 0)
        (%gc-write64 (+ b #x28)
                     (- (%gc-word-of (fn-addr %ha-lock-co-body) sa) 3))
        (%gc-write64 (+ b #x38) 0)
        (spin-lock lk)
        (%gc-write64 (+ res #x38) (%gc-read64 lk))
        (if (zerop (save-context a))
            (restore-context b)
            0)
        (%gc-write64 (+ res #x40) (%gc-read64 obs))
        (%gc-write64 (+ res #x48) (%gc-read64 lk))
        (%gc-write64 (+ res #x50) lk)
        ;; Leave it as the BSS had it.
        (%gc-write64 lk 0)
        res)))

;;; ============================================================
;;; STEP 2 — NATIVE OS THREADS
;;; ============================================================
;;;
;;; TWO MORE SLICES OF THE BAND, both in the 44 KB that sat unused between the
;;; per-CPU block (+0x1000, 16 KB) and the actor table (+0x10000):
;;;
;;;   +0x5000  THREAD BLOCK, 4 KB — everything two threads say to each other.
;;;   +0x6000  CPU 1's PER-CPU BLOCK, 16 KB — the second thread's GS base
;;;            (step 3).  CPU 0's is the existing one at +0x1000.
;;;
;;; %HA-CARVE zeroes only [band, band+0x5000), so both are zeroed here by
;;; their own initialisers rather than by the carve.
;;;
;;; THREAD-BLOCK LAYOUT (offsets from %HA-THREAD-BLOCK):
;;;   +0x000  barrier LOCK word (its own lock, not the scheduler's)
;;;   +0x008  barrier ARRIVAL counter
;;;   +0x010  thread 2 "I am running" flag
;;;   +0x018  thread 2's gettid            +0x020  thread 1's gettid
;;;   +0x028  thread 2 barrier TIMED OUT   +0x030  thread 1 barrier TIMED OUT
;;;   +0x038  thread 2's progress counter  +0x040  thread 1's progress counter
;;;   +0x048  thread 2 saw t1 advance      +0x050  thread 1 saw t2 advance
;;;   +0x058  thread 2 returned cleanly
;;;   +0x068  spin BUDGET                  +0x080  WORK iterations
;;;   +0x070  thread 2's getpid            +0x078  thread 1's getpid
;;;   +0x088  the CLONE TID WORD (4 bytes) — see %HA-SPAWN-T2
;;;   +0x098  thread 2's alloc pointer at ENTRY
;;;   +0x0A0  thread 2's alloc pointer at EXIT
;;;   +0x0A8  thread 2's cpu-id as IT reads it through GS (step 3)
;;;   +0x0B0  thread 2's active-region cell address (step 3)
;;;   +0x0B8  thread 2's active region  +0x0C0 .. +0x0F8 step 3/4/5 scratch
;;;   +0x100  RESULT BLOCK

(defun %ha-thread-block ()      (+ (%ha-base) #x5000))
(defun %ha-cpu1-percpu-base ()  (+ (%ha-base) #x6000))

;;; ---- A BARRIER, because "two TIDs exist" is not simultaneity -------------
;;;
;;; Each thread bumps the arrival counter under the block's OWN lock (not the
;;; scheduler's — a barrier must not be entangled with the actor system), then
;;; spins until the counter reaches 2.  If the two threads had run one after
;;; the other, the first one to arrive would spin out its whole budget and
;;; report a TIMEOUT: this cannot report success unless both threads were
;;; inside the barrier at the same instant.  The budget is what keeps a
;;; failure a FAILURE rather than a hang.

;;; OFF is the ARRIVAL COUNTER's offset, so a test can have more than one
;;; barrier; the LOCK is always the block's word 0.
(defun %ha-barrier-at (tb off)
  (spin-lock tb)
  (%gc-write64 (+ tb off) (+ (%gc-read64 (+ tb off)) 1))
  (spin-unlock tb)
  0)

(defun %ha-barrier-wait-at (tb off budget)
  "0 = the other thread arrived too; 1 = spun out BUDGET iterations alone."
  (let ((i 0)
        (r 1))
    (loop
      (if (>= (%gc-read64 (+ tb off)) 2)
          (progn (setq r 0) (return 0))
          0)
      (if (>= i budget)
          (return 0)
          (setq i (+ i 1))))
    r))

(defun %ha-barrier-arrive (tb) (%ha-barrier-at tb 8))
(defun %ha-barrier-wait (tb budget) (%ha-barrier-wait-at tb 8 budget))

;;; ---- INTERLEAVED PROGRESS ------------------------------------------------
;;;
;;; Bump MY counter N times and count how many of those iterations saw THEIR
;;; counter change underneath me.  Sequential execution scores ZERO here (the
;;; other thread's counter is frozen for the whole loop); genuine concurrency
;;; scores many.  Both threads run this at once, after the barrier.
(defun %ha-thread-work (tb mine theirs seen n)
  (let ((i 0)
        (s 0)
        (last (%gc-read64 (+ tb theirs))))
    (loop
      (when (>= i n) (return 0))
      (%gc-write64 (+ tb mine) (+ i 1))
      (let ((v (%gc-read64 (+ tb theirs))))
        (if (= v last)
            0
            (progn (setq s (+ s 1)) (setq last v))))
      (setq i (+ i 1)))
    (%gc-write64 (+ tb seen) s)
    s))

;;; ---- THE SECOND THREAD ---------------------------------------------------
;;;
;;; ZERO ARGUMENTS, and it must stay that way: the clone stub enters it with a
;;; bare `call rbx' on a fresh stack, so there is no argument marshalling and
;;; no caller frame to read from.
;;;
;;; IT MUST NOT ALLOCATE, and this is measured rather than asserted.  R12 (the
;;; bump-allocation pointer) and R14 (its limit) are ORDINARY REGISTERS, so the
;;; child gets a COPY of the parent's at clone time — two threads allocating
;;; from two copies of one pointer hand out the same addresses.  Fixing that is
;;; step 3 (a region per thread, whose %GC-REGION-ENTER loads this thread's own
;;; R12/R14).  Until then the thread records its alloc pointer at entry and at
;;; exit and the test requires them EQUAL.  Everything it does is fixnum
;;; arithmetic, raw memory reads/writes and syscalls, none of which allocate:
;;; %GC-READ64 only allocates when the word it reads is >= 2^62 (a bignum), and
;;; every word here is a small counter.
(defun %ha-thread2-body ()
  (let* ((tb (%ha-thread-block))
         (budget (%gc-read64 (+ tb #x68)))
         (work (%gc-read64 (+ tb #x80))))
    (%gc-write64 (+ tb #x98) (get-alloc-ptr))
    (%gc-write64 (+ tb #x18) (syscall3 186 0 0 0))   ; gettid
    (%gc-write64 (+ tb #x70) (syscall3 39 0 0 0))    ; getpid
    (%gc-write64 (+ tb #x10) 1)
    (%ha-barrier-arrive tb)
    (%gc-write64 (+ tb #x28) (%ha-barrier-wait tb budget))
    (%ha-thread-work tb #x38 #x40 #x48 work)
    (%gc-write64 (+ tb #xA0) (get-alloc-ptr))
    (%gc-write64 (+ tb #x58) 1)
    0))

(defun %ha-thread2-entry ()
  (- (%gc-word-of (fn-addr %ha-thread2-body) (+ (%ha-base) #x80)) 3))

;;; ---- THE STACK AND THE SPAWN --------------------------------------------

(defvar *ha-t2-stack* 0)             ; raw base of thread 2's stack, 0 = none
(defvar *ha-t2-stack-size* 262144)   ; 256 KB

(defun %ha-thread-stack ()
  "Thread 2's stack, mmap'd ONCE.  Returns its raw base, or 0 if mmap failed.

   NOT the carved band and NOT the GC heap.  A thread stack must not be inside
   any region's semispaces: the collector would either scan it as somebody
   else's roots or copy over it.  %MMAP-SHARED-PAGE is PROT_READ|WRITE
   MAP_SHARED|MAP_ANONYMOUS — plain writable memory the collector knows nothing
   about, and NOT the JIT's PROT_RWX page, because a stack has no business
   being executable."
  (if (> *ha-t2-stack* 0)
      *ha-t2-stack*
      (let ((p (%mmap-shared-page *ha-t2-stack-size*)))
        ;; A failed mmap comes back as -errno, tagged, i.e. a small negative.
        (if (< p 4096)
            0
            (progn (setq *ha-t2-stack* p) p)))))

(defun %ha-spawn-t2 (entry)
  "clone(2) thread 2 running ENTRY (a raw native entry address).  Returns the
   child TID, or 0 if the stack could not be mapped.

   THE TID WORD at thread-block +0x88 is the JOIN.  CLONE_PARENT_SETTID makes
   the kernel write the new TID there before this call returns, and
   CLONE_CHILD_CLEARTID makes it ZERO the same word when the thread has
   actually exited — so polling it for zero is an OS-level `has this thread
   really gone away', not a flag the thread set about itself."
  (let ((stk (%ha-thread-stack))
        (tb (%ha-thread-block)))
    (if (zerop stk)
        0
        (%spawn-thread entry (+ stk *ha-t2-stack-size*) (+ tb #x88)))))

(defun %ha-join-t2 (budget)
  "0 once the kernel has cleared the TID word (the thread is gone); 1 if the
   budget ran out first."
  (let ((tb (%ha-thread-block))
        (i 0)
        (r 1))
    (loop
      (if (zerop (mem-ref (+ tb #x88) :u32))
          (progn (setq r 0) (return 0))
          0)
      (if (>= i budget)
          (return 0)
          (setq i (+ i 1))))
    r))

(defun %ha-threads-selftest (budget work)
  "STEP-2 ACCEPTANCE.  A second NATIVE OS THREAD — clone(2), its own mmap'd
   stack, its own TID — with no actors involved at all.

   Returns the result block's raw byte address, or 0 if the band could not be
   carved or the stack could not be mapped."
  (if (zerop (%ha-carve))
      0
      (let* ((tb (%ha-thread-block))
             (res (+ tb #x100))
             (a0 0) (tid 0))
        (%ha-zero tb (+ tb #x300))
        (%gc-write64 (+ tb #x68) budget)
        (%gc-write64 (+ tb #x80) work)
        (if (zerop (%ha-thread-stack))
            0
            (progn
              (setq a0 (get-alloc-ptr))
              (%gc-write64 (+ tb #x20) (syscall3 186 0 0 0))   ; our gettid
              (%gc-write64 (+ tb #x78) (syscall3 39 0 0 0))    ; our getpid
              (setq tid (%ha-spawn-t2 (%ha-thread2-entry)))
              ;; The kernel wrote the TID into the word before returning here.
              (%gc-write64 (+ res #x08) (mem-ref (+ tb #x88) :u32))
              ;; ---- both threads must be inside the barrier at once ----
              (%ha-barrier-arrive tb)
              (%gc-write64 (+ tb #x30) (%ha-barrier-wait tb budget))
              ;; ---- and then make progress at the same time ----
              (%ha-thread-work tb #x40 #x38 #x50 work)
              ;; ---- and the thread must really exit ----
              (%gc-write64 (+ res #x78) (%ha-join-t2 budget))
              (%gc-write64 (+ res #x80) (mem-ref (+ tb #x88) :u32))
              (%gc-write64 res tid)
              (%gc-write64 (+ res #x10) (%gc-read64 (+ tb #x18)))
              (%gc-write64 (+ res #x18) (%gc-read64 (+ tb #x20)))
              (%gc-write64 (+ res #x20) (%gc-read64 (+ tb #x70)))
              (%gc-write64 (+ res #x28) (%gc-read64 (+ tb #x78)))
              (%gc-write64 (+ res #x30) (%gc-read64 (+ tb #x10)))
              (%gc-write64 (+ res #x38) (%gc-read64 (+ tb #x28)))
              (%gc-write64 (+ res #x40) (%gc-read64 (+ tb #x30)))
              (%gc-write64 (+ res #x48) (%gc-read64 (+ tb 8)))
              (%gc-write64 (+ res #x50) (%gc-read64 (+ tb #x38)))
              (%gc-write64 (+ res #x58) (%gc-read64 (+ tb #x40)))
              (%gc-write64 (+ res #x60) (%gc-read64 (+ tb #x48)))
              (%gc-write64 (+ res #x68) (%gc-read64 (+ tb #x50)))
              (%gc-write64 (+ res #x70) (%gc-read64 (+ tb #x58)))
              (%gc-write64 (+ res #x88) work)
              (%gc-write64 (+ res #x90) budget)
              (%gc-write64 (+ res #x98) *ha-t2-stack*)
              (%gc-write64 (+ res #xA0) (+ *ha-t2-stack* *ha-t2-stack-size*))
              ;; The second thread must not have allocated: it shares a COPY of
              ;; this thread's R12, so an allocation there is a double-handout.
              (%gc-write64 (+ res #xA8) (%gc-read64 (+ tb #x98)))
              (%gc-write64 (+ res #xB0) (%gc-read64 (+ tb #xA0)))
              (%gc-write64 (+ res #xB8) a0)
              res)))))

;;; ============================================================
;;; STEP 3 — A PER-THREAD ACTIVE REGION
;;; ============================================================
;;;
;;; Stage 3 of per-region GC built the mechanism and left it OFF: the
;;; active-region word became CPU 0's entry of a 16-entry array based at the
;;; same address (+GC-REGION-ADDR+ + 8*cpu_id), gated on both sides — the Lisp
;;; side on the BSS mode word +GC-REGION-PERCPU-ADDR+, the native side on
;;; translate-x64's *X64-GC-REGION-PERCPU*.  Nothing had ever written the mode
;;; word, so that code had never executed.  This turns it on.
;;;
;;; THE NATIVE SIDE IS :RUNTIME, NOT T, and that is the load-bearing decision.
;;; ./modus is one binary serving two populations: ordinary single-threaded runs
;;; where the GS base is 0 and an unguarded `GS:[16]' SIGSEGVs on the first
;;; collection, and threaded runs where every thread has a per-CPU block.  So
;;; EMIT-LOAD-GC-REGION now emits BOTH forms and branches on the SAME mode word
;;; %GC-REGION-CELL branches on.  One word flips the mutator and the collector
;;; together; they cannot end up reading different cells.
;;;
;;; ORDERING, which is the whole safety argument for turning it on at runtime:
;;;   1. this thread points GS at a real per-CPU block and stamps CPU 0 into it;
;;;   2. ONLY THEN is the mode word set;
;;;   3. a new thread inherits the parent's GS base (clone is issued WITHOUT
;;;      CLONE_SETTLS), so between the clone and its own arch_prctl it reads
;;;      CPU 0's cell — the wrong cell, but never an unmapped one — and it does
;;;      nothing that reads a region until it has stamped its own CPU id.

(defun %ha-percpu-mode () (mem-ref #x10000FF8 :u32))

(defun %ha-set-percpu-mode (v)
  "Write the per-CPU active-region MODE WORD (+GC-REGION-PERCPU-ADDR+).
   Non-zero = this CPU's cell is +GC-REGION-ADDR+ + 8*cpu_id, on BOTH the Lisp
   side (%GC-REGION-CELL) and the native side (EMIT-LOAD-GC-REGION, compiled
   :RUNTIME).  DO NOT set this before a real per-CPU block is installed."
  (setf (mem-ref #x10000FF8 :u32) v)
  v)

(defun %ha-percpu-init-cpu (base cpu)
  "Point THIS THREAD's GS base at BASE (arch_prctl ARCH_SET_GS, syscall 158)
   and stamp CPU into the :CPU-ID slot at +GC-PERCPU-CPU-ID-OFF+ = 16.  The
   order matters: PERCPU-SET is a GS-relative store, so it can only be issued
   after the base is set.  Returns the kernel's return value (0 = success)."
  (let ((r (syscall3 158 #x1001 base 0)))
    (if (zerop r) (percpu-set 16 cpu) 0)
    r))

(defun %ha-thread-adopt-region (rcb k)
  "Make RCB this THREAD's active region and load its allocation pointer/limit,
   WITHOUT parking whatever the cell named before.

   NOT %GC-REGION-ENTER, and the difference is a correctness one.  ENTER parks
   the LEAVING region's allocation pointer and limit — the right thing when one
   actor hands the CPU to another on the same thread.  A brand-new OS thread's
   R12/R14 are a COPY of the spawning thread's, made at clone time and stale the
   moment that thread allocates again; parking them into the region the cell
   happens to name (region 0, whose block is SHARED) would roll another thread's
   allocation frontier BACKWARDS and hand out addresses twice.  There is nothing
   to park, so this does not park.

   K IS AN ARGUMENT AND MUST NOT BE (%GC-META-SCALE) COMPUTED HERE.  That
   function derives raw-vs-SHL'd metadata storage by asking whether the LIVE
   ALLOCATION POINTER falls inside the ACTIVE REGION's from-space — a sound
   trick on one thread, where those two always belong together, and WRONG in a
   thread that has just been cloned: its R12 is a copy of the spawning thread's,
   which by then is pointing into the SPAWNER's region while this thread's cell
   still names region 0.  The derivation then answers 2 on x86-64, every field
   read comes back HALVED, and this thread starts bump-allocating at half its
   region's address.  Measured, not theorised — it is what the first run of
   test/hosted-thread-regions.lisp reported.  The caller computes K while its
   own cell and allocation pointer still agree, and hands it over."
  (%gc-set-region rcb)
  (set-alloc-ptr (%gc-meta-read (+ rcb #x30) k))
  (set-alloc-limit (%gc-meta-read (+ rcb #x38) k))
  0)

;;; ---- the experiment -----------------------------------------------------
;;;
;;; ONE thread alternates the value in ITS OWN cell while the OTHER samples its
;;; own cell and requires it never to change.  Two things make that a proof
;;; rather than a tautology:
;;;
;;;   - the watcher ALSO samples the switcher's RAW cell address and counts how
;;;     many DISTINCT values it saw there.  "I never saw my own cell change" is
;;;     worthless if the other thread never switched during the window; this
;;;     number says it did, and it is required to be >= 2.
;;;   - the cell addresses are read back and checked to be the two DIFFERENT
;;;     table entries (CPU 0 at +GC-REGION-ADDR+, CPU 1 eight bytes above it).
;;;
;;; THE SWITCHER USES %GC-SET-REGION, NOT %GC-REGION-ENTER, on purpose: ENTER
;;; also parks and reloads R12/R14, and doing that thousands of times on the
;;; driver's own thread would move its live allocation frontier around for
;;; reasons that have nothing to do with what is being measured.  The full ENTER
;;; path IS exercised, once per thread, by %HA-THREAD-ADOPT-REGION — and the
;;; test checks the two threads end up with DIFFERENT allocation pointers,
;;; which is the R12/R14-are-per-thread half of the claim.

(defun %ha-region-switch-loop (tb rcb n countoff)
  "Alternate this thread's cell between region 0 and RCB, N times.  Leaves it
   on RCB.  Allocates nothing, so the alternation cannot trip a collection."
  (let ((r0 (%gc-region-0))
        (i 0))
    (loop
      (when (>= i n) (return 0))
      (%gc-set-region r0)
      (%gc-set-region rcb)
      (%gc-write64 (+ tb countoff) (+ i 1))
      (setq i (+ i 1)))
    (%gc-set-region rcb)
    0))

(defun %ha-region-watch-loop (tb rcb other-cell n badoff seenoff)
  "Sample MY cell N times — it must always read RCB — while counting the
   DISTINCT values appearing at OTHER-CELL, the raw address of the other
   thread's entry in the same table."
  (let ((i 0)
        (bad 0)
        (seen 1)
        (last (%gc-read64 other-cell)))
    (loop
      (when (>= i n) (return 0))
      (if (= (%gc-region) rcb) 0 (setq bad (+ bad 1)))
      (let ((v (%gc-read64 other-cell)))
        (if (= v last)
            0
            (progn (setq seen (+ seen 1)) (setq last v))))
      (setq i (+ i 1)))
    (%gc-write64 (+ tb badoff) bad)
    (%gc-write64 (+ tb seenoff) seen)
    0))

;;; Extra thread-block words step 3 uses (all inside the 0x300 the selftest
;;; zeroes):
;;;   +0x0C0  thread 2's region control block   +0x0C8  CPU 0's raw cell address
;;;   +0x0D0  thread 2's arch_prctl return      +0x0D8  thread 2's region BEFORE
;;;                                                     it adopted one
;;;   +0x0E0  thread 2's alloc ptr after adopt  +0x0E8  its alloc limit
;;;   +0x0F0  phase-A mismatches (watcher = t2) +0x0F8  phase-A distinct values
;;;   +0x300  phase-B barrier counter           +0x308  phase-B mismatches (t1)
;;;   +0x310  phase-B distinct values           +0x318  t1 phase-B timeout
;;;   +0x320  t2 phase-B timeout                +0x328  t2 switch count
;;;   +0x330  t1 switch count                   +0x338  CPU 1's raw cell address
;;;   +0x340  the metadata SCALE, computed by the driver (see
;;;           %HA-THREAD-ADOPT-REGION for why a thread must not derive it)

(defun %ha-thread2-region-body ()
  (let* ((tb (%ha-thread-block))
         (budget (%gc-read64 (+ tb #x68)))
         (work (%gc-read64 (+ tb #x80)))
         (rcb (%gc-read64 (+ tb #xC0)))
         (cell0 (%gc-read64 (+ tb #xC8)))
         (k (%gc-read64 (+ tb #x340))))
    (%gc-write64 (+ tb #x98) (get-alloc-ptr))
    (%gc-write64 (+ tb #x18) (syscall3 186 0 0 0))
    ;; ---- MY GS base, MY cpu id ----
    (%gc-write64 (+ tb #xD0) (%ha-percpu-init-cpu (%ha-cpu1-percpu-base) 1))
    (%gc-write64 (+ tb #xA8) (percpu-ref 16))
    (%gc-write64 (+ tb #xB0) (%gc-region-cell))
    ;; ---- WHAT MY CELL SAYS BEFORE I TOUCH IT ----
    ;; This is the sharpest single number in the test.  Thread 1 has already put
    ;; its OWN region in its OWN cell; my cell is still the BSS zero, so it must
    ;; answer REGION 0.  If the per-CPU indexing were not working I would be
    ;; reading thread 1's cell and would see thread 1's region here.
    (%gc-write64 (+ tb #xD8) (%gc-region))
    (%ha-thread-adopt-region rcb k)
    (%gc-write64 (+ tb #xB8) (%gc-region))
    (%gc-write64 (+ tb #xE0) (get-alloc-ptr))
    (%gc-write64 (+ tb #xE8) (get-alloc-limit))
    (%gc-write64 (+ tb #x10) 1)
    ;; ---- PHASE A: thread 1 switches, I watch ----
    (%ha-barrier-arrive tb)
    (%gc-write64 (+ tb #x28) (%ha-barrier-wait tb budget))
    (%ha-region-watch-loop tb rcb cell0 work #xF0 #xF8)
    ;; ---- PHASE B: I switch, thread 1 watches ----
    (%ha-barrier-at tb #x300)
    (%gc-write64 (+ tb #x320) (%ha-barrier-wait-at tb #x300 budget))
    (%ha-region-switch-loop tb rcb work #x328)
    (%gc-write64 (+ tb #xA0) (get-alloc-ptr))
    (%gc-write64 (+ tb #x58) 1)
    0))

(defun %ha-thread2-region-entry ()
  (- (%gc-word-of (fn-addr %ha-thread2-region-body) (+ (%ha-base) #x80)) 3))

(defun %ha-regions-percpu-selftest (budget work)
  "STEP-3 ACCEPTANCE.  Two OS threads, two GS bases, two active-region cells.

   Returns the result block's raw byte address, or 0 if the band could not be
   carved or the thread stack could not be mapped."
  (if (zerop (%ha-carve))
      0
      (let* ((tb (%ha-thread-block))
             (res (+ tb #x100))
             (rcb2 (+ (%ha-base) #x200))
             (rcb3 (+ (%ha-base) #x240))
             (r0 (%gc-region-0))
             (mode0 0) (a1 0) (tid 0))
        (%ha-zero tb (+ tb #x400))
        (%ha-zero (%ha-cpu1-percpu-base) (+ (%ha-cpu1-percpu-base) #x4000))
        (%gc-write64 (+ tb #x68) budget)
        (%gc-write64 (+ tb #x80) work)
        (if (zerop (%ha-thread-stack))
            0
            (progn
              (setq mode0 (%ha-percpu-mode))
              ;; ---- 1. THIS thread: a real per-CPU block, stamped CPU 0 ----
              (%gc-write64 (+ res #x00) (%ha-percpu-init-cpu (%ha-percpu-base) 0))
              ;; ---- 2. ONLY NOW is the per-CPU cell turned on ----
              (%ha-set-percpu-mode 1)
              (%gc-write64 (+ res #x08) mode0)
              (%gc-write64 (+ res #x10) (%ha-percpu-mode))
              (%gc-write64 (+ res #x18) (percpu-ref 16))
              (%gc-write64 (+ res #x20) (%gc-region-cell))
              ;; THE TWO REGIONS, and their STACK_BASEs are not decoration.
              ;; A region's root window runs UP to its stack_base, so it must be
              ;; the top of the stack of the thread that owns it: thread 2's
              ;; region gets thread 2's mmap'd stack top, and this thread's gets
              ;; region 0's stack_base — the process stack — because this thread
              ;; is the one running on it.
              (%gc-region-init rcb2 *ha-r1-from* *ha-r1-to* *ha-rsize*
                               (%gc-meta-read (+ r0 #x18) (%gc-meta-scale))
                               (%gc-meta-scale))
              (%gc-region-init rcb3 *ha-r2-from* *ha-r2-to* *ha-rsize*
                               (+ *ha-t2-stack* *ha-t2-stack-size*)
                               (%gc-meta-scale))
              (%gc-write64 (+ tb #xC0) rcb3)
              (%gc-write64 (+ tb #xC8) (%gc-region-cell))
              ;; THE METADATA SCALE, computed HERE and handed to thread 2.
              ;; %GC-META-SCALE derives it by asking whether the live allocation
              ;; pointer brackets the active region's from-space; that question
              ;; is only meaningful on a thread whose cell and R12 belong
              ;; together, which a freshly cloned thread's do not.  See
              ;; %HA-THREAD-ADOPT-REGION.
              (%gc-write64 (+ tb #x340) (%gc-meta-scale))
              ;; ---- 3. THIS thread adopts its own region, REVERSIBLY ----
              ;; %GC-REGION-ENTER and not a bare cell write, because here the
              ;; park half is CORRECT: this thread is region 0's legitimate
              ;; owner, so its live R12/R14 are exactly what region 0's block
              ;; should record while it is away.  The matching (%gc-region-enter
              ;; r0) at the end reloads them, so the driver comes back to the
              ;; toplevel with the allocation frontier it left with — checked.
              (setq a1 (get-alloc-ptr))
              (%gc-region-enter rcb2)
              (%gc-write64 (+ res #x28) (%gc-region))
              (%gc-write64 (+ res #x120) (get-alloc-ptr))
              ;; ---- 4. and only now spawn the second thread ----
              (setq tid (%ha-spawn-t2 (%ha-thread2-region-entry)))
              (%gc-write64 (+ res #x30) tid)
              ;; ---- PHASE A: we switch, thread 2 watches ----
              (%ha-barrier-arrive tb)
              (%gc-write64 (+ tb #x30) (%ha-barrier-wait tb budget))
              (%ha-region-switch-loop tb rcb2 work #x330)
              ;; ---- PHASE B: thread 2 switches, we watch ----
              (%gc-write64 (+ tb #x338) (+ (%gc-region-cell) 8))
              (%ha-barrier-at tb #x300)
              (%gc-write64 (+ tb #x318) (%ha-barrier-wait-at tb #x300 budget))
              (%ha-region-watch-loop tb rcb2 (+ (%gc-region-cell) 8) work
                                     #x308 #x310)
              (%gc-write64 (+ res #x38) (%ha-join-t2 budget))
              ;; ---- NOBODY COLLECTED, and that is measured, not assumed ----
              ;; The switch loop leaves the cell naming region 0 for part of
              ;; every iteration while R12/R14 still belong to this thread's own
              ;; region, so a collection landing inside that window would
              ;; evacuate the wrong heap.  It cannot happen — neither loop
              ;; allocates, by construction, and %GC-REGION-SWITCH-LOOP must
              ;; stay that way — and the three collection counts below say so.
              (%gc-write64 (+ res #x130) (%gc-meta-read (+ r0 #x20) (%gc-meta-scale)))
              (%gc-write64 (+ res #x138) (%gc-meta-read (+ rcb2 #x20) (%gc-meta-scale)))
              (%gc-write64 (+ res #x140) (%gc-meta-read (+ rcb3 #x20) (%gc-meta-scale)))
              ;; ---- put this thread back where it started, both halves ----
              (%gc-region-enter r0)
              (%ha-set-percpu-mode mode0)
              ;; ---- evidence ----
              (%gc-write64 (+ res #x40) (%gc-read64 (+ tb #xD0)))
              (%gc-write64 (+ res #x48) (%gc-read64 (+ tb #xA8)))
              (%gc-write64 (+ res #x50) (%gc-read64 (+ tb #xB0)))
              (%gc-write64 (+ res #x58) (%gc-read64 (+ tb #xD8)))
              (%gc-write64 (+ res #x60) (%gc-read64 (+ tb #xB8)))
              (%gc-write64 (+ res #x68) rcb2)
              (%gc-write64 (+ res #x70) rcb3)
              (%gc-write64 (+ res #x78) r0)
              (%gc-write64 (+ res #x80) (%gc-read64 (+ tb #x10)))
              (%gc-write64 (+ res #x88) (%gc-read64 (+ tb #x28)))
              (%gc-write64 (+ res #x90) (%gc-read64 (+ tb #x30)))
              (%gc-write64 (+ res #x98) (%gc-read64 (+ tb #x320)))
              (%gc-write64 (+ res #xA0) (%gc-read64 (+ tb #x318)))
              (%gc-write64 (+ res #xA8) (%gc-read64 (+ tb #xF0)))
              (%gc-write64 (+ res #xB0) (%gc-read64 (+ tb #xF8)))
              (%gc-write64 (+ res #xB8) (%gc-read64 (+ tb #x308)))
              (%gc-write64 (+ res #xC0) (%gc-read64 (+ tb #x310)))
              (%gc-write64 (+ res #xC8) (%gc-read64 (+ tb #x330)))
              (%gc-write64 (+ res #xD0) (%gc-read64 (+ tb #x328)))
              (%gc-write64 (+ res #xD8) work)
              (%gc-write64 (+ res #xE0) (%gc-read64 (+ tb #xE0)))
              (%gc-write64 (+ res #xE8) (%gc-read64 (+ tb #xE8)))
              (%gc-write64 (+ res #xF0) a1)
              (%gc-write64 (+ res #xF8) (get-alloc-ptr))
              (%gc-write64 (+ res #x100) (%gc-read64 (+ tb #x58)))
              (%gc-write64 (+ res #x108) (%gc-read64 (+ tb #x338)))
              (%gc-write64 (+ res #x110) *ha-r2-from*)
              (%gc-write64 (+ res #x118) (%gc-region))
              (%gc-write64 (+ res #x128) *ha-r1-from*)
              res)))))

;;; ============================================================
;;; Band words the workers and the driver share
;;; ============================================================
;;;   +0x190  AP-SCHEDULER entry count (above)
;;;   +0x198  NMSG — how many messages this run passes
;;;   +0x1A0  worker A's forwarded count
;;;   +0x1A8  worker B's logged count
;;;   +0x1B0  worker A's structural-error count
;;;   +0x1B8  worker B's structural-error count
;;;   +0x1C0  worker A's chain-survival errors (step D)
;;;   +0x1C8  worker B's chain-survival errors (step D)
;;;   +0x1D0  worker A's OWN region's collection count when it parked (step D)
;;;   +0x1D8  worker B's OWN region's collection count when it parked (step D)
;;;   +0x1E0  NLINKS — chain length each worker holds live across every forced
;;;           collection of its own region.  ZERO MEANS STEP C: no chain, no
;;;           forced collection, so the same worker code measures the plain
;;;           actor system and the per-region one.
;;;   +0x300  8-word scratch window for the foreign-ref oracle's exact control
;;;   +0x800  message log, 32 bytes per message

(defun %ha-nmsg ()   (%gc-read64 (+ (%ha-base) #x198)))
(defun %ha-nlinks () (%gc-read64 (+ (%ha-base) #x1E0)))
(defun %ha-bump (off)
  (let ((a (+ (%ha-base) off)))
    (%gc-write64 a (+ (%gc-read64 a) 1))))
(defun %ha-log-entry (i) (+ (+ (%ha-base) #x800) (* i 32)))

;; The collection count of whatever region is active RIGHT NOW.  A worker calls
;; this while running, so it reads its OWN region's count.
(defun %ha-my-gc-count ()
  (%gc-meta-read (+ (%gc-region) #x20) (%gc-meta-scale)))

;;; ============================================================
;;; THE TWO WORKERS
;;; ============================================================
;;;
;;; Actor 2 (A) forwards; actor 3 (B) records and acknowledges.  Both are
;;; entered by RESTORE-CONTEXT jumping to their native entry point with RSP set
;;; to their own 64 KB stack in the band, so they take no arguments and read
;;; everything they need out of the band.
;;;
;;; MESSAGE SHAPE, and why it is a DOTTED tree and not a list.  net/actors.lisp
;;; is written for the bare-metal world where NIL IS ZERO, so TERM-SIZE's first
;;; test — `(zerop val)' — catches the end of a list.  In a hosted CL image NIL
;;; is the immediate #xDEAD0001, `(zerop nil)' is false, `(consp nil)' is false,
;;; `(numberp nil)' is false, and TERM-SIZE falls through to SOFT-SUBTAG, which
;;; dereferences it.  So a NIL-terminated list cannot be sent by this
;;; serializer in a hosted image; a tree of dotted pairs of non-zero fixnums
;;; can, and that is what exercises the cons path here.  Reported, not papered
;;; over: fixing it means teaching TERM-SIZE / TERM-ENCODE / TERM-DECODE-STEP
;;; about a NIL that is not zero, which changes net/actors.lisp itself.
;;;
;;; WHAT THE FORCED COLLECTION IS DOING IN HERE (step D).  When NLINKS > 0 each
;;; worker holds a chain of NLINKS conses live in a FRAME SLOT on its own stack
;;; and forces a collection OF ITS OWN REGION after every message.  Two things
;;; are then true only if per-actor regions work: the chain still walks (its
;;; roots were found in [live RBP, this actor's stack top), the RUNNING window
;;; that only exists because the region's stack_base is this actor's stack),
;;; and the message the worker is holding SURVIVED THE MOVE — it is re-checked
;;; and re-sent after the collection, which is the term-serialisation soundness
;;; claim being run for the first time.

(defun %ha-chain-want (nl) (+ 1 (ash (* nl (- nl 1)) -1)))

(defun %ha-msg-ok (m i)
  "1 if M is the message the driver sent as number I: (i . (7i . i+1000))."
  (if (consp m)
      (if (= (car m) i)
          (if (consp (cdr m))
              (if (= (car (cdr m)) (* i 7))
                  (if (= (cdr (cdr m)) (+ i 1000)) 1 0)
                  0)
              0)
          0)
      0))

(defun %ha-worker-a ()
  ;; Receive NMSG messages, check each one's shape, force a collection of THIS
  ;; ACTOR'S OWN REGION, check the message survived it, and forward it
  ;; UNCHANGED to actor 3.  Forwarding re-serialises: the message was decoded
  ;; into A's region on the way in and is encoded again on the way out, so the
  ;; round trip happens on both sides of a collection of the region it lives in.
  (let ((n (%ha-nmsg))
        (nl (%ha-nlinks))
        (i 1)
        (chain nil)
        (want 0))
    (if (> nl 0)
        (progn (setq chain (%gc-chain-build nl))
               (setq want (%ha-chain-want nl)))
        0)
    (loop
      (when (> i n) (return 0))
      (let ((m (receive)))
        (if (= (%ha-msg-ok m i) 1) 0 (%ha-bump #x1B0))
        (if (> nl 0)
            (progn
              (%gc-collect-here)
              (if (= (%gc-chain-check chain nl) want) 0 (%ha-bump #x1C0))
              ;; THE MESSAGE MUST HAVE MOVED WITH THE REST OF THE REGION.
              (if (= (%ha-msg-ok m i) 1) 0 (%ha-bump #x1B0)))
            0)
        (send 3 m))
      (%ha-bump #x1A0)
      (setq i (+ i 1)))
    (%gc-write64 (+ (%ha-base) #x1D0) (%ha-my-gc-count))
    ;; PARK, with CHAIN still live in this frame.  In this schedule the run
    ;; queue holds actor 3 at this point, so this RECEIVE blocks once and hands
    ;; the CPU on; control never comes back.  The chain staying live is what
    ;; gives the driver's parked-window collection something to find.
    (loop (receive))))

(defun %ha-worker-b ()
  ;; Receive NMSG forwarded messages, force a collection of THIS ACTOR'S OWN
  ;; REGION, then write the message's three fixnums into the log and
  ;; acknowledge to the primordial actor (id 1).  Logging AFTER the collection
  ;; is deliberate: the log is the driver's evidence that the message survived
  ;; a collection of the region it had just been decoded into.  The ack is a
  ;; plain fixnum, so it takes SEND's FAST path (no staging) while the
  ;; forwarded message took the serialised one — both are exercised.
  (let ((n (%ha-nmsg))
        (nl (%ha-nlinks))
        (i 1)
        (chain nil)
        (want 0))
    (if (> nl 0)
        (progn (setq chain (%gc-chain-build nl))
               (setq want (%ha-chain-want nl)))
        0)
    (loop
      (when (> i n) (return 0))
      (let ((m (receive)))
        (if (> nl 0)
            (progn
              (%gc-collect-here)
              (if (= (%gc-chain-check chain nl) want) 0 (%ha-bump #x1C8)))
            0)
        (if (consp m)
            (if (consp (cdr m))
                (let ((e (%ha-log-entry (- i 1))))
                  (%gc-write64 e (car m))
                  (%gc-write64 (+ e 8) (car (cdr m)))
                  (%gc-write64 (+ e 16) (cdr (cdr m)))
                  (%gc-write64 (+ e 24) 1))
                (%ha-bump #x1B8))
            (%ha-bump #x1B8)))
      (%ha-bump #x1A8)
      (send 1 (+ 900000 i))
      (setq i (+ i 1)))
    (%gc-write64 (+ (%ha-base) #x1D8) (%ha-my-gc-count))
    ;; PARK, with CHAIN still live.  The primordial actor is runnable here (B's
    ;; own acks woke it), so this RECEIVE blocks once and hands the CPU back.
    (loop (receive))))

;;; A worker's ENTRY ADDRESS as ACTOR-SPAWN wants it.  ACTOR-SPAWN stores
;;; `(untag fn)' into the struct at +0x30 and RESTORE-CTX jumps to that word
;;; verbatim, so FN must be the Lisp integer equal to the native address.
;;; (FN-ADDR …) hands back that address OR-3 tagged (translate-x64.lisp's
;;; mvm-fn-addr), and %GC-WORD-OF is the only way to look at a tagged value as
;;; a number, so the tag comes off with a subtraction.
(defun %ha-entry-a ()
  (- (%gc-word-of (fn-addr %ha-worker-a) (+ (%ha-base) #x80)) 3))
(defun %ha-entry-b ()
  (- (%gc-word-of (fn-addr %ha-worker-b) (+ (%ha-base) #x80)) 3))

;;; ============================================================
;;; Bring-up, shared by steps C and D
;;; ============================================================

(defun %ha-actors-bringup (nmsg nlinks)
  "Bring net/actors.lisp up on this process and clear the shared band words.
   NLINKS is the per-worker live-chain length; ZERO means step C (no chain, no
   forced collections).  Returns the arch_prctl return value (0 = GS base set)."
  (let ((rc (%ha-percpu-init))
        (band (%ha-base)))
    (%ha-zero (+ band #x600) (+ band #x800))
    (%ha-zero (+ band #x800) (+ band #x1000))
    (%gc-write64 (+ band #x190) 0)
    (%gc-write64 (+ band #x198) nmsg)
    (%ha-zero (+ band #x1A0) (+ band #x1E0))
    (%gc-write64 (+ band #x1E0) nlinks)
    ;; smp-init: per-CPU block, scheduler locks, CPU count.
    ;; actor-init: actor table, scheduler state, the primordial actor (id 1),
    ;;             and the mailbox pool.
    ;; staging-init: the 64 per-actor 16 KB serialisation buffers.
    (smp-init)
    (actor-init)
    (staging-init)
    rc))

;;; ============================================================
;;; STEP C — the actor system, running, with NO per-actor regions
;;; ============================================================

(defun %ha-actors-selftest (nmsg)
  "STEP-C ACCEPTANCE.  net/actors.lisp, unmodified apart from its three console
   markers, running in a hosted Linux process: spawn two actors, pass NMSG
   messages primordial -> A -> B -> back, and record what happened.  EVERY ACTOR
   STILL ALLOCATES IN REGION 0 — that is the point of doing C before D.

   Returns the result block's raw byte address, or 0 if the band could not be
   carved.  Result layout is read back by test/hosted-actors.lisp."
  (if (zerop (%ha-carve))
      0
      (let* ((band *ha-band*)
             (res (+ band #x600))
             (k (%gc-meta-scale))
             (rc (%ha-actors-bringup nmsg 0))
             (g0-before 0)
             (ida 0) (idb 0) (i 0) (bad 0))
        (setq g0-before (%gc-meta-read (+ (%gc-region) #x20) k))
        (setq ida (actor-spawn (%ha-entry-a)))
        (setq idb (actor-spawn (%ha-entry-b)))
        ;; HAND OFF ONCE, so both workers reach their first blocking RECEIVE.
        ;; This is the call that exercises YIELD's save path: the primordial
        ;; actor SAVE-CONTEXTs, re-enqueues itself, and RESTORE-CONTEXTs into A;
        ;; A blocks and hands to B; B blocks and hands back here.
        (yield)
        ;; NMSG messages to A.  Each is a dotted tree of non-zero fixnums, so
        ;; SEND takes its SERIALISING path into A's staging buffer.
        (setq i 1)
        (loop
          (when (> i nmsg) (return 0))
          (send ida (cons i (cons (* i 7) (+ i 1000))))
          (setq i (+ i 1)))
        ;; Collect NMSG acks.  The first RECEIVE blocks (mailbox empty) and
        ;; hands the CPU to A, which starts the whole pipeline.
        (setq i 1)
        (loop
          (when (> i nmsg) (return 0))
          (let ((a (receive)))
            (if (= a (+ 900000 i)) 0 (setq bad (+ bad 1))))
          (setq i (+ i 1)))
        ;; ---- evidence ----
        (%gc-write64 res nmsg)
        (%gc-write64 (+ res #x08) ida)
        (%gc-write64 (+ res #x10) idb)
        (%gc-write64 (+ res #x18) (%gc-read64 (+ band #x1A0)))
        (%gc-write64 (+ res #x20) (%gc-read64 (+ band #x1A8)))
        (%gc-write64 (+ res #x28) (%gc-read64 (+ band #x1B0)))
        (%gc-write64 (+ res #x30) (%gc-read64 (+ band #x1B8)))
        (%gc-write64 (+ res #x38) bad)
        (%gc-write64 (+ res #x40) (%gc-read64 (+ band #x190)))
        (%gc-write64 (+ res #x48) g0-before)
        (%gc-write64 (+ res #x50) (%gc-meta-read (+ (%gc-region) #x20) k))
        (%gc-write64 (+ res #x58) (if (zerop rc) 0 1))
        (%gc-write64 (+ res #x60) (actor-count))
        (%gc-write64 (+ res #x68) (%ha-log-entry 0))
        (%gc-write64 (+ res #x70) (%gc-read64 (+ (actor-struct-addr ida) #x08)))
        (%gc-write64 (+ res #x78) (%gc-read64 (+ (actor-struct-addr idb) #x08)))
        (%gc-write64 (+ res #x80) (actor-stack-top ida))
        (%gc-write64 (+ res #x88) (actor-stack-top idb))
        (%gc-write64 (+ res #x90) (get-current-actor))
        (%gc-write64 (+ res #x98) (actor-get ida #x00))
        (%gc-write64 (+ res #xA0) (actor-get idb #x00))
        (%gc-write64 (+ res #xA8) (%gc-read64 (+ (actor-struct-addr 1) #x08)))
        (%gc-write64 (+ res #xB0) (%gc-region))
        (%gc-write64 (+ res #xB8) (%gc-region-0))
        res)))

;;; ============================================================
;;; STEP D — each actor OWNS its region
;;; ============================================================
;;;
;;; The slot has been there since stage 3: actor struct +0x68 holds the RAW
;;; BYTE ADDRESS of that actor's 64-byte region control block, and ZERO MEANS
;;; REGION 0.  ACTOR-REGION-HOP (YIELD, RECEIVE) and ACTOR-REGION-RESUME (the
;;; idle scheduler, ACTOR-EXIT) already read it.  Nothing had ever written a
;;; non-zero value into it in a running system.  This does.

(defun %ha-give-region (id rcb from to)
  "Make RCB the region of actor ID: semispaces [FROM,+*HA-RSIZE*) and
   [TO,+*HA-RSIZE*), and the pointer written into the actor struct at +0x68 in
   net/actors.lisp's convention — the STORED MACHINE WORD is the address, hence
   the (untag …), which ACTOR-REGION-RAW inverts by doubling.

   STACK_BASE IS THIS ACTOR'S OWN STACK TOP, and that is the soundness gain of
   the whole step.  In step C every actor allocated in region 0, whose
   stack_base is the PROCESS stack base — so a collection while an actor ran
   would have scanned from a band stack up to the process stack, terabytes of
   unmapped address space.  With a region per actor the RUNNING window is
   [live RBP, this actor's stack top) and the PARKED window is [the SP the
   switch recorded, that same top): both lie inside one 64 KB stack."
  (%gc-region-init rcb from to *ha-rsize* (actor-stack-top id) (%gc-meta-scale))
  (actor-set id #x68 (untag rcb))
  rcb)

(defun %ha-regions-selftest (nmsg nlinks)
  "STEP-D ACCEPTANCE.  Two actors, each owning its own GC region, passing NMSG
   messages while each forces a collection of ITS OWN region after every one,
   holding an NLINKS-cons chain live across all of them — and then, with both
   actors parked, the driver collects each region from the OTHER side of the
   switch and audits the isolation.

   COLLECTIONS ARE FORCED, NEVER WAITED FOR.  A 16 MB region and a few thousand
   conses would not trip a :gc-check in this lifetime; %GC-COLLECT-HERE pulls
   the limit down to the pointer so the next allocation's own check fires and
   the target's ORDINARY collector runs.

   Returns the result block's raw byte address, or 0 if the band could not be
   carved."
  (if (zerop (%ha-carve))
      0
      (let* ((band *ha-band*)
             (res (+ band #x600))
             (rcb2 (+ band #x200))
             (rcb3 (+ band #x240))
             (r0 (%gc-region-0))
             (ctl-lo (+ band #x300))
             (ctl-hi (+ band #x340))
             (rc (%ha-actors-bringup nmsg nlinks))
             (k 0) (ida 0) (idb 0) (i 0) (bad 0)
             (f2 0) (t2 0) (a2 0) (f3 0) (t3 0) (a3 0)
             (sum3b 0) (rcb3sumb 0) (sum2b 0) (rcb2sumb 0))
        (setq k (%gc-meta-scale))
        (setq ida (actor-spawn (%ha-entry-a)))
        (setq idb (actor-spawn (%ha-entry-b)))
        ;; ---- THE WRITE THAT MAKES AN ACTOR OWN A REGION ----
        (%ha-give-region ida rcb2 *ha-r1-from* *ha-r1-to*)
        (%ha-give-region idb rcb3 *ha-r2-from* *ha-r2-to*)
        (%gc-write64 res nmsg)
        (%gc-write64 (+ res #x08) nlinks)
        (%gc-write64 (+ res #x10) ida)
        (%gc-write64 (+ res #x18) idb)
        (%gc-write64 (+ res #x20) rcb2)
        (%gc-write64 (+ res #x28) rcb3)
        (%gc-write64 (+ res #x30) r0)
        (%gc-write64 (+ res #x88) (%gc-meta-read (+ r0 #x20) k))
        ;; ---- the message phase, exactly as step C ----
        (yield)
        (setq i 1)
        (loop
          (when (> i nmsg) (return 0))
          (send ida (cons i (cons (* i 7) (+ i 1000))))
          (setq i (+ i 1)))
        (setq i 1)
        (loop
          (when (> i nmsg) (return 0))
          (let ((a (receive)))
            (if (= a (+ 900000 i)) 0 (setq bad (+ bad 1))))
          (setq i (+ i 1)))
        (%gc-write64 (+ res #x38) (%gc-read64 (+ band #x1A0)))
        (%gc-write64 (+ res #x40) (%gc-read64 (+ band #x1A8)))
        (%gc-write64 (+ res #x48) (%gc-read64 (+ band #x1B0)))
        (%gc-write64 (+ res #x50) (%gc-read64 (+ band #x1B8)))
        (%gc-write64 (+ res #x58) (%gc-read64 (+ band #x1C0)))
        (%gc-write64 (+ res #x60) (%gc-read64 (+ band #x1C8)))
        (%gc-write64 (+ res #x68) bad)
        (%gc-write64 (+ res #x70) (%gc-read64 (+ band #x190)))
        (%gc-write64 (+ res #x78) (%gc-read64 (+ band #x1D0)))
        (%gc-write64 (+ res #x80) (%gc-read64 (+ band #x1D8)))
        (%gc-write64 (+ res #x90) (%ha-log-entry 0))
        ;; ================= PHASE 2: THE ISOLATION AUDIT =================
        ;; Both actors are now parked inside RECEIVE with their regions parked
        ;; at the SP the scheduler recorded and their chains still live in
        ;; their own frames.  The driver is back in region 0 on the process
        ;; stack.  Collecting one region from here is the PARKED-window path.
        (setq k (%gc-meta-scale))
        (setq f3 (%gc-meta-read rcb3 k))
        (setq a3 (%gc-meta-read (+ rcb3 #x30) k))
        (setq sum3b (%gc-sum-range f3 a3))
        (setq rcb3sumb (%gc-sum-range rcb3 (+ rcb3 #x40)))
        (%gc-write64 (+ res #x98) (%gc-meta-read (+ rcb2 #x20) k))
        (%gc-write64 (+ res #xA8) (%gc-meta-read (+ rcb3 #x20) k))
        (%gc-write64 (+ res #xB8) (%gc-meta-read (+ r0 #x20) k))
        (%gc-write64 (+ res #x110) (%gc-meta-read rcb2 k))
        ;; --- collect REGION 2 TWICE, parked on actor 2's own window ---
        ;; Nothing between the two %GC-REGION-ENTERs may allocate except
        ;; %GC-COLLECT-HERE's own junk cons: while region 2 is active the
        ;; collector would scan actor 2's parked frames, not the driver's.
        (%gc-region-enter rcb2)
        (%gc-collect-here)
        (%gc-collect-here)
        (%gc-region-enter r0)
        (setq f2 (%gc-meta-read rcb2 k))
        (setq t2 (%gc-meta-read (+ rcb2 #x08) k))
        (setq a2 (%gc-meta-read (+ rcb2 #x30) k))
        (%gc-write64 (+ res #xA0) (%gc-meta-read (+ rcb2 #x20) k))
        (%gc-write64 (+ res #x108) (- a2 f2))
        (%gc-write64 (+ res #x118) f2)
        ;; region 3 and region 0 must be untouched by that
        (%gc-write64 (+ res #xC8) sum3b)
        (%gc-write64 (+ res #xD0) (%gc-sum-range f3 a3))
        (%gc-write64 (+ res #xD8) rcb3sumb)
        (%gc-write64 (+ res #xE0) (%gc-sum-range rcb3 (+ rcb3 #x40)))
        (%gc-write64 (+ res #xB0) (%gc-meta-read (+ rcb3 #x20) k))
        (%gc-write64 (+ res #xC0) (%gc-meta-read (+ r0 #x20) k))
        ;; --- now the other direction: collect REGION 3 ONCE ---
        (setq sum2b (%gc-sum-range f2 a2))
        (setq rcb2sumb (%gc-sum-range rcb2 (+ rcb2 #x40)))
        (%gc-region-enter rcb3)
        (%gc-collect-here)
        (%gc-region-enter r0)
        (setq f3 (%gc-meta-read rcb3 k))
        (setq t3 (%gc-meta-read (+ rcb3 #x08) k))
        (setq a3 (%gc-meta-read (+ rcb3 #x30) k))
        (%gc-write64 (+ res #x120) (- a3 f3))
        ;; region 3's count AFTER its own forced collection, and region 2's at
        ;; the very end.  Both are read HERE and not reused from an earlier
        ;; slot: a check that compares a value with itself cannot fail.
        (%gc-write64 (+ res #x1E0) (%gc-meta-read (+ rcb3 #x20) k))
        (%gc-write64 (+ res #x1E8) (%gc-meta-read (+ rcb2 #x20) k))
        (%gc-write64 (+ res #xE8) sum2b)
        (%gc-write64 (+ res #xF0) (%gc-sum-range f2 a2))
        (%gc-write64 (+ res #xF8) rcb2sumb)
        (%gc-write64 (+ res #x100) (%gc-sum-range rcb2 (+ rcb2 #x40)))
        ;; ================= THE FOREIGN-REF AUDIT =================
        ;; Zero in both directions across BOTH semispaces of the other region,
        ;; and zero out of each into region 0's live space.
        (%gc-write64 (+ res #x128)
                     (+ (%gc-count-foreign-refs f2 a2 f3 *ha-rsize*)
                        (%gc-count-foreign-refs f2 a2 t3 *ha-rsize*)))
        (%gc-write64 (+ res #x130)
                     (+ (%gc-count-foreign-refs f3 a3 f2 *ha-rsize*)
                        (%gc-count-foreign-refs f3 a3 t2 *ha-rsize*)))
        (%gc-write64 (+ res #x138)
                     (%gc-count-foreign-refs f2 a2
                                             (%gc-meta-read r0 k)
                                             (%gc-meta-read (+ r0 #x10) k)))
        ;; POSITIVE CONTROL 1, EXACT.  A zeroed 8-word window holding exactly
        ;; ONE cons-tagged pointer into region 2's from-space must count 1 — an
        ;; oracle that can only ever answer zero is worth nothing.
        (%ha-zero ctl-lo ctl-hi)
        (%gc-write64 (+ ctl-lo 24) (+ f2 1))
        (%gc-write64 (+ res #x140)
                     (%gc-count-foreign-refs ctl-lo ctl-hi f2 *ha-rsize*))
        (%gc-write64 (+ res #x148)
                     (%gc-count-foreign-refs ctl-lo ctl-hi f3 *ha-rsize*))
        ;; POSITIVE CONTROL 2, REAL.  Actor 2's PARKED WINDOW — the span the
        ;; collector actually scanned — must hold at least one live pointer into
        ;; region 2's current from-space, since that is where its chain is.
        (%gc-write64 (+ res #x150)
                     (%gc-count-foreign-refs
                      (%gc-read64 (+ (actor-struct-addr ida) #x08))
                      (actor-stack-top ida) f2 *ha-rsize*))
        (%gc-write64 (+ res #x158)
                     (%gc-count-foreign-refs
                      (%gc-read64 (+ (actor-struct-addr idb) #x08))
                      (actor-stack-top idb) f3 *ha-rsize*))
        ;; ================= STATE, READ BACK =================
        (%gc-write64 (+ res #x160) (%gc-read64 (+ (actor-struct-addr ida) #x08)))
        (%gc-write64 (+ res #x168) (actor-stack-top ida))
        (%gc-write64 (+ res #x170) (%gc-read64 (+ (actor-struct-addr idb) #x08)))
        (%gc-write64 (+ res #x178) (actor-stack-top idb))
        (%gc-write64 (+ res #x180) (if (%gc-region-parked-p rcb2) 1 0))
        (%gc-write64 (+ res #x188) (if (%gc-region-parked-p rcb3) 1 0))
        (%gc-write64 (+ res #x190) (if (%gc-region-parked-p r0) 1 0))
        (%gc-write64 (+ res #x198) (%gc-region))
        (%gc-write64 (+ res #x1A0) (actor-region-raw ida))
        (%gc-write64 (+ res #x1A8) (actor-region-raw idb))
        (%gc-write64 (+ res #x1B0) (%gc-meta-read (+ rcb2 #x28) k))
        (%gc-write64 (+ res #x1B8) (%gc-meta-read (+ rcb3 #x28) k))
        (%gc-write64 (+ res #x1C0) (%gc-meta-read (+ rcb2 #x18) k))
        (%gc-write64 (+ res #x1C8) (%gc-meta-read (+ rcb3 #x18) k))
        (%gc-write64 (+ res #x1D0) (if (zerop rc) 0 1))
        (%gc-write64 (+ res #x1D8) (%gc-meta-read (+ r0 #x20) k))
        res)))
