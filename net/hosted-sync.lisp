;;;; hosted-sync.lisp — REAL TIME AND REAL BLOCKING for hosted x86-64.
;;;;
;;;; Baked immediately AFTER net/hosted-actors-post.lisp (hosted x64 only), so
;;;; everything here is last-defun-wins over the CL bridge — which is how
;;;; `SLEEP' stops being `(defun sleep (n) nil)'.
;;;;
;;;; WHAT WAS MISSING, and why all four things are one file.
;;;;
;;;;   1. SLEEP WAS A NO-OP.  mvm/ansi-bridge.lisp defines `(defun sleep (n)
;;;;      nil)'.  Nothing in the tree could pace anything: no frame clock, no
;;;;      backoff, no timeout that is a duration rather than a spin budget.
;;;;   2. EVERY WAIT IN THE TREE BURNS A CORE.  net/actors.lisp's SPIN-LOCK is
;;;;      a TTAS spin; the threaded workers poll TRY-RECEIVE + YIELD; the
;;;;      barriers spin on a counter.  That is correct on bare metal with one
;;;;      actor per CPU and nothing else to run.  In a hosted process — and for
;;;;      a compositor holding a frame lock — it is not acceptable.
;;;;   3. A WAIT THAT BLOCKS NEEDS THE KERNEL, and on Linux the kernel
;;;;      primitive is FUTEX(2).  One syscall gives both halves: FUTEX_WAIT
;;;;      parks the calling thread on a word if that word still holds an
;;;;      expected value, FUTEX_WAKE unparks N waiters on it.
;;;;   4. AND ALL OF THEM NEED A FEW BYTES OF WRITABLE MEMORY AT A KNOWN
;;;;      ADDRESS — a timespec for nanosleep, a per-CPU idle word for the
;;;;      scheduler — which is what THE THREAD PAGE below is.
;;;;
;;;; XCHG IS ENOUGH; NO `LOCK CMPXCHG' IS ADDED.  +OP-ATOMIC-XCHG+ is an
;;;; UNCONDITIONAL exchange, not a compare-and-swap, and the classic futex
;;;; mutex is usually written with CAS.  It does not have to be: the
;;;; three-state (0 free / 1 held / 2 held-with-waiters) protocol below uses
;;;; only XCHG, and the reason it is still correct is spelled out at
;;;; %MUTEX-LOCK.  The condition variable likewise avoids an atomic increment
;;;; by requiring — as pthreads permits and most code already does — that the
;;;; ASSOCIATED MUTEX IS HELD across %COND-SIGNAL / %COND-BROADCAST, which
;;;; makes the sequence counter's read-modify-write ordinary protected code.
;;;; If a lock-free structure ever needs a true CAS, `LOCK CMPXCHG' has to be
;;;; added to mvm/mvm.lisp and every translator; nothing here needs it.

;;; ============================================================
;;; THE THREAD PAGE
;;; ============================================================
;;;
;;; ONE 4 KB anonymous mapping, whose address lives in ONE BSS word.  Not the
;;; carved actor band, because SLEEP has to work in a `./modus' that never
;;; starts an actor system; and not a Lisp global, because a global lives in
;;; the shared globals hash table, which is precisely the structure a second
;;; thread must not be mutating (see the runtime-table lock).  A raw BSS word
;;; plus a raw mmap touches neither.
;;;
;;; WHY A PAGE AND NOT BSS WORDS.  The BSS block at 0x10000000 is nearly full:
;;; the documented free gap (0x10000E40..0x10000EFF) is down to a handful of
;;; words, and on BARE-METAL x64 the heap itself starts at 0x10001000, so
;;; addresses above the block are not free to claim in shared source.  Two BSS
;;; words — a base and its init lock — buy 4 KB that is per-CPU-partitionable.
;;;
;;; LAYOUT (offsets from the page base):
;;;   +0x000  per-CPU TIMESPEC scratch, 64 bytes per CPU, 16 CPUs.
;;;           +0x00 req.tv_sec  +0x08 req.tv_nsec
;;;           +0x10 rem.tv_sec  +0x18 rem.tv_nsec   (nanosleep's remainder)
;;;           +0x20 clock_gettime scratch (tv_sec, tv_nsec)
;;;   +0x400  per-CPU SCHEDULER IDLE block, 64 bytes per CPU.
;;;           +0x00 the futex word this CPU's idle loop parks on
;;;           +0x08 idle entries  +0x10 futex waits  +0x18 futex wakes received
;;;   +0x800  free scratch for tests (mutex words, condvars, counters)
;;;
;;; The two BSS words are 0x10000DA8 and 0x10000DB0 — the first two free words
;;; above the safepoint-boundary slot at 0x10000DA0 and below the MCGC config
;;; block at 0x10000E00.  A grep of every `#x10000xxx' literal in the tree
;;; finds nothing between 0x10000DA0 and 0x10000E00.

(defun %thr-page-slot () #x10000DA8)
(defun %thr-page-lock () #x10000DB0)

(defun %thr-page ()
  "Raw byte address of the thread page, mapping it on first use.  0 if the
   mmap failed.  Double-checked under a spinlock so two threads racing on the
   first call cannot map two pages and disagree about which is the real one."
  (let ((p (%gc-read64 (%thr-page-slot))))
    (if (> p 0)
        p
        (progn
          (spin-lock (%thr-page-lock))
          (let ((q (%gc-read64 (%thr-page-slot))))
            (if (> q 0)
                (progn (spin-unlock (%thr-page-lock)) q)
                (let ((m (%mmap-shared-page 4096)))
                  ;; A failed mmap comes back as a small negative (-errno).
                  (if (< m 4096)
                      (progn (spin-unlock (%thr-page-lock)) 0)
                      (progn
                        (%ha-zero m (+ m 4096))
                        (%gc-write64 (%thr-page-slot) m)
                        (spin-unlock (%thr-page-lock))
                        m)))))))))

(defun %thr-cpu ()
  "This thread's CPU id.  0 unless per-CPU storage has been turned on — the
   same gate %GC-REGION-CELL reads, for the same reason: PERCPU-REF is a
   GS-relative load and the GS base is 0 in a process that never installed a
   per-CPU block, so an unguarded read faults at absolute address 16."
  (if (= (mem-ref #x10000FF8 :u32) 0)
      0
      (percpu-ref 16)))

(defun %thr-ts ()
  "This CPU's 64-byte timespec scratch, or 0 if the page could not be mapped."
  (let ((p (%thr-page)))
    (if (zerop p) 0 (+ p (* 64 (%thr-cpu))))))

(defun %thr-idle-block (cpu)
  "CPU's 64-byte scheduler idle block, or 0."
  (let ((p (%thr-page)))
    (if (zerop p) 0 (+ p (+ #x400 (* 64 cpu))))))

(defun %thr-scratch ()
  "The free 2 KB at the top of the thread page — test mutexes, condvars and
   counters live here.  0 if the page could not be mapped."
  (let ((p (%thr-page)))
    (if (zerop p) 0 (+ p #x800))))

;;; ============================================================
;;; CLOCKS
;;; ============================================================

(defun %clock-ns-at (buf clockid)
  "clock_gettime(CLOCKID) in NANOSECONDS, using the caller-supplied 16-byte
   timespec at BUF.  The explicit-buffer form exists so a thread that has NOT
   installed a per-CPU block — and therefore cannot be told apart from CPU 0 by
   %THR-CPU — can still take a clock reading without sharing one timespec with
   another running thread."
  (if (zerop buf)
      0
      (progn
        (%gc-write64 buf 0)
        (%gc-write64 (+ buf 8) 0)
        ;; 228 = SYS_clock_gettime on x86-64.
        (syscall3 228 clockid buf 0)
        (+ (* (%gc-read64 buf) 1000000000) (%gc-read64 (+ buf 8))))))

(defun %clock-ns (clockid)
  "clock_gettime(CLOCKID) in NANOSECONDS, or 0 if unavailable.

   CLOCK_MONOTONIC (1) is what every measurement here uses.  CLOCK_REALTIME
   nanoseconds since the epoch are ~1.7e18, uncomfortably close to the 2^62
   fixnum ceiling; monotonic nanoseconds are seconds-since-boot scaled, which
   is not."
  (let ((ts (%thr-ts)))
    (if (zerop ts) 0 (%clock-ns-at (+ ts #x20) clockid))))

(defun %monotonic-ns () (%clock-ns 1))

(defun %cpu-ns ()
  "This PROCESS's consumed CPU time in nanoseconds (CLOCK_PROCESS_CPUTIME_ID
   = 2), summed over every thread.  This is the number that tells a spinning
   idle loop from a blocked one: wall time passes either way, CPU time only
   passes when a thread is on a core."
  (%clock-ns 2))

(defun %thread-cpu-ns ()
  "THIS THREAD's consumed CPU time in nanoseconds (CLOCK_THREAD_CPUTIME_ID
   = 3)."
  (%clock-ns 3))

;;; ============================================================
;;; SLEEP
;;; ============================================================

(defun %nanosleep-at (ts sec nsec)
  "nanosleep(2) for SEC seconds + NSEC nanoseconds, restarting on EINTR with
   the kernel's own remainder, using the caller-supplied 32-byte scratch at TS
   (req at +0, rem at +16).  Returns 0 on success, or the last errno.

   RESTARTING IS NOT OPTIONAL.  nanosleep returns -EINTR with the unslept
   remainder in *rem the moment ANY signal is delivered, and this image
   installs signal handlers (the #PF/#GP rescue path).  A SLEEP that returned
   early on a signal would be a SLEEP that sometimes does nothing at all — the
   same defect as the no-op it replaces, only harder to see."
  (if (zerop ts)
        0
        (let ((req ts)
              (rem (+ ts 16))
              (r 0)
              (guard 0))
          (%gc-write64 req sec)
          (%gc-write64 (+ req 8) nsec)
          (loop
            (%gc-write64 rem 0)
            (%gc-write64 (+ rem 8) 0)
            ;; 35 = SYS_nanosleep on x86-64.
            (setq r (syscall3 35 req rem 0))
            (when (>= r 0) (return 0))
            ;; -4 = -EINTR.  Anything else is a real error; do not spin on it.
            (when (not (= r -4)) (return 0))
            (setq guard (+ guard 1))
            (when (> guard 1000000) (return 0))
            (%gc-write64 req (%gc-read64 rem))
            (%gc-write64 (+ req 8) (%gc-read64 (+ rem 8))))
          (if (>= r 0) 0 (- 0 r)))))

(defun %nanosleep (sec nsec)
  "%NANOSLEEP-AT on this CPU's own timespec scratch."
  (%nanosleep-at (%thr-ts) sec nsec))

(defun %sleep-ms (ms)
  "Block for MS milliseconds.  Integer-only, so it is callable from code that
   must not touch the float tower (the scheduler, a thread body)."
  (if (<= ms 0)
      0
      (%nanosleep (floor ms 1000) (* (- ms (* 1000 (floor ms 1000))) 1000000))))

(defun sleep (n)
  "CLHS SLEEP.  Was `(defun sleep (n) nil)'.

   Seconds may be an integer, a ratio or a float.  The nanosecond part is
   computed from the FRACTIONAL remainder rather than by scaling N and
   dividing, so an integer argument never goes near the float tower and a
   float argument never has to survive a 1e9 multiply of the whole value."
  (if (not (realp n))
      nil
      (if (<= n 0)
          nil
          (let* ((s (floor n))
                 (ns (floor (* (- n s) 1000000000))))
            (%nanosleep s ns)
            nil))))

;;; ============================================================
;;; FUTEX
;;; ============================================================
;;;
;;; futex(uaddr, op, val, timeout, uaddr2, val3) — syscall 202 on x86-64.
;;;
;;; FUTEX_PRIVATE_FLAG (128) is set on both operations.  Every futex word here
;;; is in this process's own memory and is never shared through a file mapping
;;; with another process, so the kernel can key the wait queue on the mm rather
;;; than on the page's inode+offset — which is both faster and, more to the
;;; point, correct only if the flag agrees on BOTH sides.  Wait and wake must
;;; use the same flag or they hash to different queues and the wake is lost.
;;;
;;; THE COMPARE IS 32-BIT.  FUTEX_WAIT reads FOUR bytes at uaddr and sleeps
;;; only if they equal VAL.  Every futex word in this file is a full machine
;;; word holding a small non-negative number, so on a little-endian target its
;;; low four bytes ARE the value; and %GC-WRITE64 always writes both halves, so
;;; the upper four bytes stay zero.

(defun %futex-wait (addr val)
  "Park this thread on ADDR while the word there still reads VAL.  Returns 0
   if it slept and was woken, -11 (-EAGAIN) if the value had already changed —
   which is not an error but the whole point of the compare-and-park."
  (syscall6 202 addr 128 val 0 0 0))

(defun %futex-wake (addr n)
  "Wake at most N threads parked on ADDR.  Returns the number woken."
  (syscall6 202 addr 129 n 0 0 0))

(defun %futex-wake-all (addr) (%futex-wake addr 1000000))

;;; ============================================================
;;; MUTEX
;;; ============================================================
;;;
;;; A mutex is ONE machine word with three states:
;;;   0  free
;;;   1  held, and no thread is known to be parked on it
;;;   2  held, and a thread may be parked on it
;;; The state is what makes the unlock cheap: a wake syscall is issued only
;;; when the word says a waiter might exist, so an uncontended lock/unlock
;;; pair is two XCHGs and no syscall at all.
;;;
;;; WHY THIS IS CORRECT WITH XCHG ALONE.  The textbook version uses CAS to
;;; move 1 -> 2 without disturbing 0.  XCHG cannot test before writing, so
;;; %MUTEX-LOCK's slow path writes 2 UNCONDITIONALLY and reads what was there:
;;;
;;;   * If it reads 0, the lock was free and is now ours — and the word says 2
;;;     even though we may be the only thread.  That is the safe direction: the
;;;     eventual unlock issues a wake nobody needs, costing one syscall.
;;;   * If it reads 1 or 2, the lock was held; we park on the value 2 we just
;;;     wrote, and the compare inside FUTEX_WAIT closes the race with an unlock
;;;     that lands in between — the word would then read 0, the compare fails,
;;;     and we loop instead of sleeping forever.
;;;
;;; The one thing to check is that a waiter can never be stranded: could a
;;; holder unlock, see 1, skip the wake, and leave somebody parked?  To be
;;; parked, a thread must have written 2 and observed a non-zero old value.
;;; From that instant the word reads 2 and only an unlock (which writes 0) or
;;; another slow-path acquirer (which writes 2 again) can change it.  Whoever
;;; finally acquires from the slow path acquired BY WRITING 2, so that holder's
;;; unlock reads 2 and wakes.  The fast path — XCHG 1 on a word reading 0 —
;;; can only run when the word is 0, i.e. after an unlock, and an unlock that
;;; had a parked waiter already woke it.  A woken waiter re-writes 2 before
;;; parking again.  So no waiter sleeps against a word that will not be woken.

(defun %mutex-init (addr) (%gc-write64 addr 0) 0)

(defun %mutex-trylock (addr)
  "1 if the lock was free and is now ours, 0 if it was already held."
  (if (zerop (xchg-mem addr 1)) 1 0))

(defun %mutex-lock (addr)
  "Acquire, blocking in the KERNEL rather than on a core when contended."
  (if (zerop (xchg-mem addr 1))
      0
      (loop
        (if (zerop (xchg-mem addr 2))
            (return 0)
            (%futex-wait addr 2)))))

(defun %mutex-unlock (addr)
  "Release, and wake one parked thread if the word says one may exist."
  (if (= (xchg-mem addr 0) 2)
      (progn (%futex-wake addr 1) 0)
      0))

(defun %mutex-held-p (addr)
  (if (zerop (%gc-read64 addr)) nil t))

;;; ============================================================
;;; CONDITION VARIABLE
;;; ============================================================
;;;
;;; ONE machine word, a SEQUENCE COUNTER, and the futex compare does the rest:
;;; a waiter reads the sequence, drops the mutex, and parks on the condvar word
;;; "while it still reads that sequence".  A signal that lands between the read
;;; and the park has already bumped the counter, so the compare fails and the
;;; waiter does not sleep — which is the classic lost-wakeup, closed by the
;;; kernel rather than by a lock.
;;;
;;; THE MUTEX MUST BE HELD ACROSS SIGNAL AND BROADCAST.  That is a real
;;; requirement here, not a convention: the increment is a plain
;;; read-modify-write, and this ISA has an unconditional exchange but no atomic
;;; add.  Holding the mutex makes it ordinary protected code.  (pthreads allows
;;; signalling without the mutex; this does not.  The alternative is `LOCK
;;; XADD' in the translator — a real instruction to add, for a requirement most
;;; callers already meet.)
;;;
;;; SPURIOUS WAKEUPS ARE PERMITTED, as with every condition variable: FUTEX_WAIT
;;; returns on EAGAIN and on signals, and the sequence is masked to 32 bits so
;;; it can alias after 2^32 signals.  CALL %COND-WAIT IN A LOOP AROUND THE
;;; PREDICATE.  Every use in the tree does.

(defun %cond-init (addr) (%gc-write64 addr 0) 0)

(defun %cond-wait (cv mtx)
  "Atomically drop MTX and park on CV; re-acquire MTX before returning.
   MTX must be held on entry."
  (let ((seq (%gc-read64 cv)))
    (%mutex-unlock mtx)
    (%futex-wait cv seq)
    (%mutex-lock mtx)
    0))

(defun %cond-bump (cv)
  (%gc-write64 cv (logand (+ (%gc-read64 cv) 1) #xFFFFFFFF))
  0)

(defun %cond-signal (cv)
  "Wake one waiter.  MTX must be held (see above)."
  (%cond-bump cv)
  (%futex-wake cv 1))

(defun %cond-broadcast (cv)
  "Wake every waiter.  MTX must be held (see above)."
  (%cond-bump cv)
  (%futex-wake-all cv))

;;; ============================================================
;;; ACCEPTANCE — THE MUTEX AND THE CONDITION VARIABLE, UNDER TWO
;;; NATIVE THREADS, WITH THEIR OWN NEGATIVE CONTROL
;;; ============================================================
;;;
;;; Three phases, run by THIS thread and a clone(2) sibling at the same time.
;;;
;;;   A. THE NEGATIVE CONTROL.  Both threads increment ONE shared word N times
;;;      with NO mutex.  A read-modify-write through %GC-READ64/%GC-WRITE64 is
;;;      two 32-bit loads and two 32-bit stores, so the interleaving window is
;;;      wide and updates are LOST.  The test asserts the final value is
;;;      STRICTLY LESS than 2N and prints how many were lost.  This is the
;;;      control for phase B: if phase B passed with the mutex removed, the
;;;      mutex would be proving nothing.
;;;   B. THE SAME LOOP UNDER THE MUTEX.  Exactly 2N, and each thread's private
;;;      tally is exactly N.
;;;   C. THE CONDITION VARIABLE, and it must BLOCK rather than spin.  Thread 2
;;;      takes the mutex and waits on the condvar until a predicate flips;
;;;      thread 1 SLEEPS for a third of a second first, then flips it under the
;;;      mutex and signals.  Thread 2 measures its OWN consumed CPU time
;;;      (CLOCK_THREAD_CPUTIME_ID) across the wait alongside the wall time.  A
;;;      spin-wait would show the two roughly equal; a kernel park shows wall
;;;      time passing and CPU time not.
;;;
;;; THE THREAD BODY ALLOCATES NOTHING, and that is measured, not assumed: this
;;; thread has no region of its own here (that is what the actor/region tests
;;; build), so its R12 is a copy of thread 1's and any allocation would hand out
;;; addresses thread 1 also holds.  Entry and exit alloc pointers must match.
;;;
;;; SCRATCH LAYOUT (offsets from %THR-SCRATCH):
;;;   +0x00 mutex          +0x08 protected counter   +0x10 condvar
;;;   +0x18 predicate      +0x20 t2 started          +0x28 t2 finished
;;;   +0x30 N              +0x38 t2 tally            +0x40 t1 tally
;;;   +0x48 UNSYNCHRONISED counter                   +0x50 t2 unsync tally
;;;   +0x58 t1 unsync tally                          +0x60 t2 barrier timeouts
;;;   +0x68 t2 alloc ptr at entry                    +0x70 t2 alloc ptr at exit
;;;   +0x80 t2's private 32-byte timespec scratch
;;;   +0xA0 t2 cond-wait cpu ns   +0xA8 t2 cond-wait wall ns
;;;   +0xB0 t2 cond-wait wakeups  +0xB8 t2 saw the predicate set
;;;   +0xC0 barrier A    +0xC8 barrier LOCK   +0xD0 barrier B   +0xD8 barrier C
;;;   +0x100 RESULT BLOCK (0x100)

(defun %sync-barrier (s off n budget)
  "Arrive at the barrier counter at S+OFF and spin until N have arrived.
   0 = everybody arrived; 1 = spun out BUDGET alone, which is what a
   SEQUENTIAL run scores and is therefore a failure, not a hang."
  (%mutex-lock (+ s #xC8))
  (%gc-write64 (+ s off) (+ (%gc-read64 (+ s off)) 1))
  (%mutex-unlock (+ s #xC8))
  (let ((i 0)
        (r 1))
    (loop
      (when (>= (%gc-read64 (+ s off)) n) (progn (setq r 0) (return 0)))
      (when (>= i budget) (return 0))
      (setq i (+ i 1)))
    r))

(defun %sync-bump-unlocked (s n tally)
  "Increment the UNSYNCHRONISED counter N times, with no mutex at all."
  (let ((i 0))
    (loop
      (when (>= i n) (return 0))
      (%gc-write64 (+ s #x48) (+ (%gc-read64 (+ s #x48)) 1))
      (setq i (+ i 1)))
    (%gc-write64 (+ s tally) i)
    i))

(defun %sync-bump-locked (s n tally)
  "Increment the PROTECTED counter N times, each under the mutex."
  (let ((i 0))
    (loop
      (when (>= i n) (return 0))
      (%mutex-lock s)
      (%gc-write64 (+ s #x08) (+ (%gc-read64 (+ s #x08)) 1))
      (%mutex-unlock s)
      (setq i (+ i 1)))
    (%gc-write64 (+ s tally) i)
    i))

(defun %sync-t2-body ()
  "The sibling thread.  ZERO ARGUMENTS — the clone stub enters it with a bare
   `call rbx' on a fresh stack, so there is nothing to marshal."
  (let ((s (%thr-scratch)))
    (%gc-write64 (+ s #x68) (get-alloc-ptr))
    (%gc-write64 (+ s #x20) 1)
    (let ((n (%gc-read64 (+ s #x30)))
          (budget 400000000))
      ;; ---- A: no mutex ----
      (when (= 1 (%sync-barrier s #xC0 2 budget))
        (%gc-write64 (+ s #x60) (+ (%gc-read64 (+ s #x60)) 1)))
      (%sync-bump-unlocked s n #x50)
      ;; ---- B: the mutex ----
      (when (= 1 (%sync-barrier s #xD0 2 budget))
        (%gc-write64 (+ s #x60) (+ (%gc-read64 (+ s #x60)) 1)))
      (%sync-bump-locked s n #x38)
      ;; ---- C: the condition variable ----
      (when (= 1 (%sync-barrier s #xD8 2 budget))
        (%gc-write64 (+ s #x60) (+ (%gc-read64 (+ s #x60)) 1)))
      (let ((w0 (%clock-ns-at (+ s #x80) 1))
            (c0 (%clock-ns-at (+ s #x80) 3))
            (k 0))
        (%mutex-lock s)
        (loop
          (when (= 1 (%gc-read64 (+ s #x18))) (return 0))
          (%cond-wait (+ s #x10) s)
          (setq k (+ k 1)))
        (%gc-write64 (+ s #xB8) 1)
        (%mutex-unlock s)
        (%gc-write64 (+ s #xA8) (- (%clock-ns-at (+ s #x80) 1) w0))
        (%gc-write64 (+ s #xA0) (- (%clock-ns-at (+ s #x80) 3) c0))
        (%gc-write64 (+ s #xB0) k)))
    (%gc-write64 (+ s #x70) (get-alloc-ptr))
    (%gc-write64 (+ s #x28) 1)
    0))

(defun %sync-t2-entry ()
  (- (%gc-word-of (fn-addr %sync-t2-body) (+ (%ha-base) #x80)) 3))

(defun %sync-selftest (n)
  "ACCEPTANCE for the mutex and the condition variable.  Returns the result
   block's raw byte address, or 0 if the band could not be carved (the thread
   spawn borrows net/hosted-actors-post.lisp's stack and TID word) or the
   thread page could not be mapped."
  (if (zerop (%ha-carve))
      0
      (let ((s (%thr-scratch))
            (tb (%ha-thread-block)))
        (if (zerop s)
            0
            (let ((res (+ s #x100))
                  (budget 400000000)
                  (a0 0)
                  (tid 0)
                  (t1to 0))
              (%ha-zero s (+ s #x200))
              (%ha-zero tb (+ tb #x300))
              (%mutex-init s)
              (%mutex-init (+ s #xC8))
              (%cond-init (+ s #x10))
              (%gc-write64 (+ s #x30) n)
              (setq a0 (get-alloc-ptr))
              (setq tid (%ha-spawn-t2 (%sync-t2-entry)))
              (if (< tid 1)
                  0
                  (progn
                    ;; ---- A: the negative control ----
                    (setq t1to (+ t1to (%sync-barrier s #xC0 2 budget)))
                    (%sync-bump-unlocked s n #x58)
                    ;; ---- B: the mutex ----
                    (setq t1to (+ t1to (%sync-barrier s #xD0 2 budget)))
                    (%sync-bump-locked s n #x40)
                    ;; ---- C: the condition variable ----
                    (setq t1to (+ t1to (%sync-barrier s #xD8 2 budget)))
                    (%sleep-ms 300)
                    (%mutex-lock s)
                    (%gc-write64 (+ s #x18) 1)
                    (%cond-signal (+ s #x10))
                    (%mutex-unlock s)
                    (%gc-write64 (+ res #x00) tid)
                    (%gc-write64 (+ res #x08) (%ha-join-t2 budget))
                    (%gc-write64 (+ res #x10) (%gc-read64 (+ s #x20)))
                    (%gc-write64 (+ res #x18) (%gc-read64 (+ s #x28)))
                    (%gc-write64 (+ res #x20) n)
                    (%gc-write64 (+ res #x28) (%gc-read64 (+ s #x48)))
                    (%gc-write64 (+ res #x30) (%gc-read64 (+ s #x50)))
                    (%gc-write64 (+ res #x38) (%gc-read64 (+ s #x58)))
                    (%gc-write64 (+ res #x40) (%gc-read64 (+ s #x08)))
                    (%gc-write64 (+ res #x48) (%gc-read64 (+ s #x38)))
                    (%gc-write64 (+ res #x50) (%gc-read64 (+ s #x40)))
                    (%gc-write64 (+ res #x58) (%gc-read64 (+ s #xA8)))
                    (%gc-write64 (+ res #x60) (%gc-read64 (+ s #xA0)))
                    (%gc-write64 (+ res #x68) (%gc-read64 (+ s #xB0)))
                    (%gc-write64 (+ res #x70) (%gc-read64 (+ s #xB8)))
                    (%gc-write64 (+ res #x78) (%gc-read64 (+ s #x60)))
                    (%gc-write64 (+ res #x80) t1to)
                    (%gc-write64 (+ res #x88) (%gc-read64 s))
                    (%gc-write64 (+ res #x90) (%gc-read64 (+ s #x68)))
                    (%gc-write64 (+ res #x98) (%gc-read64 (+ s #x70)))
                    (%gc-write64 (+ res #xA0) a0)
                    (%gc-write64 (+ res #xA8) (get-alloc-ptr))
                    res)))))))
