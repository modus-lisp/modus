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
;;; ONE anonymous mapping (8 KB when this was written, 76 KB once the
;;; per-thread windows moved in), whose address lives in ONE BSS word.  Not the
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
;;; words — a base and its init lock — buy 8 KB that is per-CPU-partitionable.
;;;
;;; LAYOUT (offsets from the page base):
;;;   +0x000  per-CPU TIMESPEC scratch, 64 bytes per CPU, 16 CPUs.
;;;           +0x00 req.tv_sec  +0x08 req.tv_nsec
;;;           +0x10 rem.tv_sec  +0x18 rem.tv_nsec   (nanosleep's remainder)
;;;           +0x20 clock_gettime scratch (tv_sec, tv_nsec)
;;;   +0x400  per-CPU SCHEDULER block, 128 bytes per CPU, 16 CPUs.
;;;           +0x00 .. +0x3F  this CPU's SCHEDULER CONTEXT save area (0x40) —
;;;                           where a blocking actor hands the thread back
;;;           +0x40 idle entries       +0x48 futex waits
;;;           +0x50 dispatches         +0x58 hand-backs from actors
;;;           +0x60 1 = this CPU has a live scheduler save area
;;;   +0xC00  SCHEDULER GLOBALS
;;;           +0x00 the WAKE WORD every idle thread parks on (0 = sleeping
;;;                 threads may exist and no wake is pending; 1 = wake pending)
;;;           +0x08 total wakes issued   +0x10 STOP flag for every scheduler
;;;           +0x18 legacy AP-SCHEDULER returns (the pre-blocking behaviour)
;;;   +0x1000 free scratch for tests (mutex words, condvars, counters)
;;;   +0x2000 PER-THREAD WINDOW BLOCKS, 4 KB per CPU, 16 CPUs (+0x2000 +
;;;           0x1000*cpu, ending at +0x12000).  See THE PER-THREAD WINDOW
;;;           below.
;;;   +0x12000 the MV/handler-case two-thread selftest's control block, 4 KB.
;;;   +0x13000 THE THREAD TABLE, 4 KB — one record per thread, plus the
;;;            spawn handshake words.  See MANY THREADS, FROM CLOSURES.
;;;   +0x14000 PER-CPU BLOCKS, 16 KB per CPU, 16 CPUs (ending at +0x54000).
;;;            The actor band only ever had room for two.
;;;
;;; The mapping is 336 KB rather than 8 KB for those additions; NOTHING at a
;;; lower offset moved, so every earlier user reads the same bytes.
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
                (let ((m (%mmap-shared-page 344064)))
                  ;; A failed mmap comes back as a small negative (-errno).
                  (if (< m 4096)
                      (progn (spin-unlock (%thr-page-lock)) 0)
                      (progn
                        ;; Only the first 8 KB is explicitly zeroed: the
                        ;; per-thread window blocks above +0x2000 are
                        ;; MAP_ANONYMOUS pages, which the kernel already
                        ;; guarantees zero — and zero is exactly the state a
                        ;; fresh thread's window must start in (handler depth
                        ;; 0, nothing armed).
                        (%ha-zero m (+ m 8192))
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

(defun %thr-sched-block (cpu)
  "CPU's 128-byte scheduler block, or 0."
  (let ((p (%thr-page)))
    (if (zerop p) 0 (+ p (+ #x400 (* 128 cpu))))))

(defun %thr-sched-globals ()
  "The scheduler's shared words, or 0."
  (let ((p (%thr-page)))
    (if (zerop p) 0 (+ p #xC00))))

(defun %thr-scratch ()
  "The free 4 KB at the top of the thread page — test mutexes, condvars and
   counters live here.  0 if the page could not be mapped."
  (let ((p (%thr-page)))
    (if (zerop p) 0 (+ p #x1000))))

(defun %thr-tls-block (cpu)
  "CPU's 4 KB PER-THREAD WINDOW block, or 0 if the page could not be mapped."
  (let ((p (%thr-page)))
    (if (zerop p) 0 (+ p (+ #x2000 (* cpu #x1000))))))

;;; ============================================================
;;; THE PER-THREAD WINDOW, INSTALLED
;;; ============================================================
;;;
;;; The multiple-value return buffer, the dynamic-nargs slot and the
;;; handler-frame machinery were ONE COPY FOR THE WHOLE IMAGE — see THE
;;; PER-THREAD WINDOW in mvm/compiler.lisp for what is in the window and what
;;; is deliberately left out of it.  The compiler now marks every access to
;;; one of those slots, and translate-x64 turns the mark into an FS segment
;;; override.  This is the other half: giving a thread a segment base.
;;;
;;; THE MAIN THREAD NEEDS NOTHING, AND THAT IS THE POINT.  A fresh Linux
;;; process has FS base 0, so `FS:[0x10000180]' is `[0x10000180]' — the same
;;; word at the same address, no initialisation, no mode word, nothing that
;;; has to have happened before the first MULTIPLE-VALUE-BIND (which happens
;;; during boot, well before any of this code could run).
;;;
;;; A WORKER THREAD INSTALLS ITS OWN AND MUST DO IT FIRST.  It is born with
;;; the parent's FS base (clone is issued without CLONE_SETTLS), i.e. sharing
;;; the main thread's window, so every instruction it executes before
;;; %TLS-INSTALL is still racing the main thread's values and handler frames.
;;; That window is a handful of instructions and it is stated rather than
;;; hidden: put %TLS-INSTALL first in a thread body, ahead of the per-CPU and
;;; region setup, because those allocate nothing but they do return values.

(defun %tls-self-base ()
  "This thread's segment base as the EXACT machine word, read from the self
   slot INSIDE the window (so it answers about this thread).  0 on the main
   thread, where nothing ever wrote it."
  (let ((lo (mem-ref #x10000C30 :u32))
        (hi (mem-ref #x10000C34 :u32)))
    (if (= hi 0) lo (+ (* (* hi 65536) 65536) lo))))

(defun %tls-set-self-base (base)
  "Store BASE as the EXACT machine word in the self slot.  Two :u32 halves,
   not one :u64: a :u64 store deposits val*2 (mvm/gc.lisp %GC-WRITE64 says
   why), and the collector reads this slot as a raw machine word.

   THE ADDRESS MUST BE A LITERAL HERE.  %GC-WRITE64 would take it as an
   argument, and an address that arrives as an argument cannot carry the
   segment override — it would write the MAIN thread's slot."
  (let* ((hi (ash (ash base -16) -16))
         (lo (- base (* (* hi 65536) 65536))))
    (setf (mem-ref #x10000C30 :u32) lo)
    (setf (mem-ref #x10000C34 :u32) hi)
    0))

(defun %tls-install (cpu)
  "Give THIS THREAD its own per-thread window: point FS at CPU's block.
   Returns 0 on success, the arch_prctl error otherwise (and 1 if the thread
   page could not be mapped).

   ARCH_SET_FS is 0x1002 (0x1001 is ARCH_SET_GS, which %HA-PERCPU-INIT-CPU
   uses for the per-CPU block — the two segments are deliberately separate,
   see *X64-TLS-WINDOW*).  The base is BLOCK - 0x10000000, because the
   ADDRESSES in the emitted code are unchanged: the segment moves, not the
   literal."
  (let ((b (%thr-tls-block cpu)))
    (if (or (zerop b) (< b #x10000000))
        ;; A block BELOW the window base would make the segment base negative,
        ;; i.e. non-canonical, and arch_prctl would refuse it.  Refuse first so
        ;; the caller sees a decision rather than an errno.
        1
        (let* ((delta (- b #x10000000))
               (r (syscall3 158 #x1002 delta 0)))
          (if (zerop r)
              (progn
                ;; EMPTY THIS THREAD'S DYNAMIC-BINDING STACK BEFORE ARMING IT.
                ;; The block is reused when a thread slot is reused, and a
                ;; thread that died with bindings live would otherwise hand
                ;; its successor a non-zero depth pointing at entries full of
                ;; the previous occupant's collected objects — which the
                ;; collector would then scan as roots.  Written through the
                ;; ABSOLUTE block address (B), not a window offset, and
                ;; BEFORE the self slot is set, so no path can observe the
                ;; thread as armed with a stale stack.  See PER-THREAD
                ;; DYNAMIC BINDINGS in mvm/prelude.lisp for the layout.
                (setf (mem-ref (+ b #xC50) :u64) 0)   ; next-free entry
                (setf (mem-ref (+ b #xC58) :u64) 0)   ; depth
                (%tls-set-self-base delta)
                0)
              r)))))

(defun %tls-installed-p ()
  "1 when this thread has its own window, 0 when it is still using the
   process-wide one.  Reads the self slot THROUGH the window, so a thread that
   has installed one sees its own non-zero base and the main thread sees 0."
  (if (zerop (%tls-self-base)) 0 1))

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

;;; ------------------------------------------------------------
;;; …AND THE CL INTERNAL CLOCKS RIDE ON THEM
;;; ------------------------------------------------------------
;;;
;;; LAST-DEFUN-WINS overrides of the seam mvm/ansi-bridge.lisp defines at 0.
;;; They live HERE and not there for the same reason %CLOCK-NS-AT does: a
;;; clock_gettime(2) needs a timespec that no other running thread is
;;; writing, and the per-CPU scratch that provides one is this file's.
;;;
;;; GET-INTERNAL-REAL-TIME is what glass paces frames with, so "how long
;;; since I last sent" has to be a duration and not a call count.  It is
;;; CLOCK_MONOTONIC, so it does not step when the wall clock is set.
(defun %irt-ns () (%monotonic-ns))

;;; CLOCK_PROCESS_CPUTIME_ID: the number that tells a spinning loop from a
;;; blocked one, which is exactly what GET-INTERNAL-RUN-TIME is for.
(defun %irun-ns () (%cpu-ns))

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

(defun %futex-wait-to (addr val ts)
  "FUTEX_WAIT with a RELATIVE TIMEOUT at the 16-byte timespec TS.  Returns 0 if
   it slept and was woken, -110 (-ETIMEDOUT) if the timeout expired, -11
   (-EAGAIN) if the value had already changed.  TS = 0 means no timeout."
  (syscall6 202 addr 128 val ts 0 0))

(defun %futex-timeout-ts ()
  "This CPU's futex-timeout timespec, armed at 20 ms.  0 if the thread page
   could not be mapped, which %FUTEX-WAIT-TO reads as `no timeout'."
  (let ((ts (%thr-ts)))
    (if (zerop ts)
        0
        (let ((b (+ ts #x30)))
          (%gc-write64 b 0)
          (%gc-write64 (+ b 8) 20000000)
          b))))

(defun %futex-timeouts ()
  "How many parked waits in this process expired on their own rather than being
   woken.  ZERO IS THE EXPECTED READING and the tests assert it: the timeout is
   a safety net under a protocol that is supposed to be wake-complete, and a
   non-zero count means a wake was lost — a performance bug that would have been
   a HANG without the net, and which must be chased rather than absorbed."
  (let ((g (%thr-sched-globals)))
    (if (zerop g) 0 (%gc-read64 (+ g #x20)))))

(defun %futex-timeout-bump ()
  (let ((g (%thr-sched-globals)))
    (if (zerop g) 0 (progn (%gc-write64 (+ g #x20)
                                        (+ (%gc-read64 (+ g #x20)) 1))
                           0))))

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
  "Acquire, blocking in the KERNEL rather than on a core when contended.

   THE PARK IS BOUNDED, and the bound is a SAFETY NET, not part of the
   protocol.  The three-state handshake above is wake-complete by the argument
   given there; a lost wake would nevertheless be a HANG, which is the one
   failure mode that gives you no information at all.  With a 20 ms timeout it
   is instead a re-check — the loop's next XCHG either acquires or parks again —
   and %FUTEX-TIMEOUTS COUNTS IT, so `no wake was ever lost' becomes something
   the tests assert rather than something the comment claims."
  (if (zerop (xchg-mem addr 1))
      0
      (let ((ts (%futex-timeout-ts)))
        (loop
          (if (zerop (xchg-mem addr 2))
              (return 0)
              (if (= (%futex-wait-to addr 2 ts) -110)
                  (%futex-timeout-bump)
                  0))))))

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
;;; A SCHEDULER THAT BLOCKS — RECEIVE STOPS BURNING A CORE
;;; ============================================================
;;;
;;; WHAT WAS THERE.  net/actors.lisp's RECEIVE, finding its mailbox empty and
;;; the run queue empty, marks itself BLOCKED and calls the idle scheduler.  The
;;; bare-metal AP-SCHEDULER switches to a per-CPU idle stack (MVM trap #x0400)
;;; and loops on CLI / STI+HLT — three PRIVILEGED instructions that fault in a
;;; process — so the hosted override simply COUNTED THE EVENT AND RETURNED, and
;;; every threaded selftest in this tree avoids RECEIVE entirely: the workers
;;; poll TRY-RECEIVE + YIELD, which burns a whole core per idle actor.
;;;
;;; WHY IT COULD NOT JUST BE WIRED UP.  Two problems, and only the second is
;;; about Linux:
;;;
;;;   1. THE STACK.  A blocked actor is claimable by another CPU the instant the
;;;      scheduler lock drops — MAILBOX-ENQUEUE-AND-WAKE flips it back to READY
;;;      and enqueues it — and the CPU that blocked it is still standing on its
;;;      stack.  So the switch off that stack must happen BEFORE the release,
;;;      which means the release cannot be RECEIVE's to make.  Hence
;;;      AP-SCHEDULER-BLOCKED in net/actors.lisp: entered WITH THE LOCK HELD,
;;;      bare-metal-identical there, overridden here to RESTORE-CONTEXT into
;;;      this thread's own scheduler context.  +OP-RESTORE-CTX+ zeroes the lock
;;;      AFTER `mov rsp,[base]' and before the jump, so the release happens at
;;;      the first instant it is safe and not one instruction earlier.
;;;   2. THE WAIT.  `HLT until an interrupt' has no hosted equivalent, and a
;;;      spin is the thing being removed.  The hosted idle loop parks on a futex
;;;      word instead, and WAKE-IDLE-AP — which net/actors.lisp already calls
;;;      from ACTOR-SPAWN and MAILBOX-ENQUEUE-AND-WAKE, and which was `(defun
;;;      wake-idle-ap () 0)' — becomes the wake.
;;;
;;; THE WAKE WORD IS A STATE, NOT A SEQUENCE, and that is what closes the
;;; check-then-park race without a compare-and-swap:
;;;
;;;     idler:   XCHG(wake, 0)          "I am about to look, then sleep"
;;;              lock; id = dequeue; unlock
;;;              if id -> dispatch
;;;              else  -> FUTEX_WAIT(wake, 0)
;;;     waker:   (enqueue under the lock, as net/actors.lisp already does)
;;;              XCHG(wake, 1)
;;;              FUTEX_WAKE(wake)
;;;
;;;   * If the waker's ENQUEUE precedes the idler's DEQUEUE, the idler finds the
;;;     work and never parks.
;;;   * Otherwise the enqueue follows the dequeue, which follows the idler's
;;;     XCHG(wake,0); so the waker's XCHG(wake,1) also follows it, and the idler
;;;     either finds 1 at FUTEX_WAIT (which then declines to sleep) or is
;;;     already parked and receives the wake.
;;;
;;;   A SEQUENCE COUNTER WOULD NOT DO, because incrementing it is a
;;;   read-modify-write and this ISA has an unconditional exchange but no atomic
;;;   add: two wakers could write back the same value, and a stale writer could
;;;   restore exactly the value a not-yet-parked idler is about to wait on.  A
;;;   state written with XCHG has no such window.

(defun %sched-globals () (%thr-sched-globals))

(defun %sched-block ()
  "THIS thread's scheduler block, by CPU id."
  (%thr-sched-block (%thr-cpu)))

(defun %sched-save-area ()
  "This thread's scheduler CONTEXT save area, or 0 if this thread has not
   entered %SCHED-RUN and therefore has nowhere to hand a thread back to.
   Zero is the pre-blocking behaviour, which is what every existing selftest
   asserts (they never block, so the legacy counter stays 0)."
  (let ((b (%sched-block)))
    (if (zerop b)
        0
        (if (zerop (%gc-read64 (+ b #x60))) 0 b))))

(defun %sched-stop-p ()
  (let ((g (%sched-globals)))
    (if (zerop g) nil (if (zerop (%gc-read64 (+ g #x10))) nil t))))

(defun %sched-stop ()
  "Tell every scheduler to leave its loop, and wake them so they can."
  (let ((g (%sched-globals)))
    (if (zerop g)
        0
        (progn (%gc-write64 (+ g #x10) 1)
               (wake-idle-ap)
               0))))

(defun %sched-reset ()
  "Clear the scheduler globals and every per-CPU block.  One run per process."
  (let ((g (%sched-globals))
        (p (%thr-page)))
    (if (zerop g)
        0
        (progn (%ha-zero (+ p #x400) (+ p #xC40)) 0))))

;;; ---- THE WAKE ------------------------------------------------------------
;;;
;;; net/actors.lisp calls WAKE-IDLE-AP from ACTOR-SPAWN and from
;;; MAILBOX-ENQUEUE-AND-WAKE, in both cases immediately AFTER releasing the
;;; scheduler lock, which is exactly the ordering the protocol above needs.
;;; It was `0'.

(defun wake-idle-ap ()
  (let ((g (%sched-globals)))
    (if (zerop g)
        0
        (progn
          (xchg-mem g 1)
          (%gc-write64 (+ g #x08) (+ (%gc-read64 (+ g #x08)) 1))
          (%futex-wake-all g)
          0))))

;;; ---- THE HAND-BACK -------------------------------------------------------

(defun ap-scheduler-blocked ()
  "Entered from RECEIVE with the SCHEDULER LOCK HELD and the calling actor
   already marked BLOCKED with its context saved.

   THE ONLY CORRECT ORDER IS: leave the actor's stack, THEN release.  This
   RESTORE-CONTEXTs into the thread's own scheduler context, and
   +OP-RESTORE-CTX+ zeroes the lock word after the stack switch — so between
   marking the actor blocked and letting another CPU have it, this CPU is never
   on its stack without the lock.

   A thread with no scheduler of its own (the primordial thread of an ordinary
   ./modus, which never entered %SCHED-RUN) has nowhere to go, so it falls back
   to the legacy behaviour: release, count, return.  That is what the sixteen
   existing selftests assert stays at zero, and they never block."
  (let ((sa (%sched-save-area)))
    (if (zerop sa)
        (progn (spin-unlock (sched-lock-addr))
               (ap-scheduler))
        (progn
          (set-current-actor 0)
          (set-idle-flag 1)
          (restore-context sa)))))

(defun %sched-park ()
  "Hand THIS THREAD back to its own scheduler from inside an actor that has
   finished.  Marks the actor dead so nothing re-queues it.

   THE LOCK IS TAKEN FOR THE SWITCH, NOT FOR THE QUEUE: +OP-RESTORE-CTX+
   releases the scheduler lock unconditionally, so issuing one without holding
   it would release a lock another CPU owns."
  (let ((sa (%sched-save-area)))
    (if (zerop sa)
        0
        (progn
          (spin-lock (sched-lock-addr))
          (actor-set (get-current-actor) #x00 3)
          (set-current-actor 0)
          (restore-context sa)))))

;;; ---- THE LOOP ------------------------------------------------------------

(defun %sched-idle-wait (b g)
  "Park until somebody calls WAKE-IDLE-AP.  The caller has already run
   XCHG(wake,0) and found the run queue empty under the lock."
  (%gc-write64 (+ b #x40) (+ (%gc-read64 (+ b #x40)) 1))
  (if (zerop (%gc-read64 g))
      (progn
        (%gc-write64 (+ b #x48) (+ (%gc-read64 (+ b #x48)) 1))
        (%futex-wait g 0)
        0)
      0))

(defun %sched-run ()
  "THIS THREAD'S SCHEDULER.  Dispatches actors off the shared run queue and
   BLOCKS IN THE KERNEL when there are none, until %SCHED-STOP is called.

   THE SAVE AREA IS REWRITTEN EVERY ITERATION, before every dispatch, because
   it is the way back: an actor that blocks or parks RESTORE-CONTEXTs into it
   and resumes here, just after the SAVE-CONTEXT, with a non-zero return."
  (let ((b (%sched-block))
        (g (%sched-globals)))
    (if (or (zerop b) (zerop g))
        0
        (progn
          (set-current-actor 0)
          ;; From this store on, a blocking actor on this thread has somewhere
          ;; to go.  It must not be set before the first SAVE-CONTEXT below has
          ;; something to hand back to — but nothing on this thread can block
          ;; between the two, because this IS the thread's scheduler.
          (%gc-write64 (+ b #x60) 1)
          (loop
            (when (%sched-stop-p) (return 0))
            (save-context b)
            (when (%sched-stop-p) (return 0))
            ;; DECLARE THE INTENT TO SLEEP BEFORE LOOKING AT THE QUEUE.  The
            ;; whole no-lost-wakeup argument is this one line's position.
            (xchg-mem g 0)
            (spin-lock (sched-lock-addr))
            (let ((id (actor-dequeue)))
              (if (zerop id)
                  (progn
                    (spin-unlock (sched-lock-addr))
                    (set-idle-flag 1)
                    (%sched-idle-wait b g)
                    (set-idle-flag 0))
                  (progn
                    (%gc-write64 (+ b #x50) (+ (%gc-read64 (+ b #x50)) 1))
                    (set-idle-flag 0)
                    (set-current-actor id)
                    (actor-set id #x00 1)
                    (percpu-set 40 (actor-get id #x70))
                    (percpu-set 48 (actor-get id #x78))
                    (actor-region-resume id)
                    (restore-context (+ (actor-struct-addr id) #x08))))))
          (%gc-write64 (+ b #x60) 0)
          0))))

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

;;; ============================================================
;;; ACCEPTANCE — AN IDLE ACTOR THAT COSTS NOTHING
;;; ============================================================
;;;
;;; THE TOPOLOGY is net/hosted-actors-post.lisp's step-4 topology, unchanged:
;;;   thread 1  the driver, actor 1, on the process stack.  It never YIELDs, so
;;;             it is never enqueued and the other thread can never claim it.
;;;   thread 2  its own mmap'd stack, its own GS base, its own GC region, and
;;;             %SCHED-RUN as its scheduler.  Actor 2 lives on a band stack and
;;;             is dispatched by it.
;;;
;;; THE WORKLOAD is deliberately slack: the driver sends ONE message every GAP
;;; milliseconds and sleeps in between.  A round trip costs microseconds, so
;;; actor 2 spends essentially the whole run with nothing to do.  THE QUESTION
;;; IS WHAT `NOTHING TO DO' COSTS.
;;;
;;; MODE 0 — BLOCKING.  Actor 2 calls RECEIVE.  Empty mailbox, empty run queue,
;;;          so it hands thread 2 back to %SCHED-RUN, which parks on the futex.
;;; MODE 1 — THE NEGATIVE CONTROL, IN THE SAME BINARY, ON THE SAME WORKLOAD.
;;;          Actor 2 polls TRY-RECEIVE + YIELD, which is what every threaded
;;;          selftest in this tree does today.  Nothing else is different.
;;;
;;; WHAT IS MEASURED is CLOCK_PROCESS_CPUTIME_ID across the run, beside the wall
;;; clock.  Both modes must deliver every message; only one of them may charge a
;;; core for the wait.  A test that only checked the messages arrived would pass
;;; identically in both modes and would prove nothing about blocking.
;;;
;;; SCRATCH (offsets from %THR-SCRATCH; the mutex selftest owns +0x00..+0x1FF):
;;;   +0x200 mode      +0x208 N        +0x210 gap ms
;;;   +0x218 messages actor 2 received +0x220 malformed messages
;;;   +0x228 actor 2 started           +0x230 actor 2 finished
;;;   +0x238 thread 2 entered its scheduler
;;;   +0x240 thread 2's cpu id as IT reads it
;;;   +0x248 thread 2 finished          +0x250 acks the driver got
;;;   +0x258 driver's bad acks          +0x260 actor 2's alloc ptr at start
;;;   +0x300 RESULT BLOCK

(defun %br-scratch () (+ (%thr-scratch) #x200))

(defun %br-worker ()
  "ACTOR 2.  Consume messages and acknowledge each one to actor 1, either by
   BLOCKING in RECEIVE (mode 0) or by polling TRY-RECEIVE + YIELD (mode 1).

   IT RUNS UNTIL TOLD TO STOP rather than until the last message, because the
   cost measurement is a fixed IDLE WINDOW with no traffic in it: the whole
   question is what this actor costs while it has nothing to do, and an actor
   that has already returned costs nothing in either mode.

   In BLOCKING mode it never sees the stop flag — it is parked inside RECEIVE —
   and that is correct: %SCHED-RUN leaves its loop, thread 2 returns, and the
   blocked actor is simply abandoned.  In POLLING mode the flag is what gets it
   out, and it hands the thread back through %SCHED-PARK."
  (let ((s (%br-scratch)))
    (%gc-write64 (+ s #x228) 1)
    (%gc-write64 (+ s #x260) (get-alloc-ptr))
    (let ((n (%gc-read64 (+ s #x208)))
          (mode (%gc-read64 s))
          (i 0))
      (loop
        (when (%sched-stop-p) (return 0))
        (let ((m (if (zerop mode) (receive) (try-receive))))
          (if (zerop m)
              (yield)
              (progn
                (if (consp m) 0 (%gc-write64 (+ s #x220)
                                             (+ (%gc-read64 (+ s #x220)) 1)))
                (setq i (+ i 1))
                (%gc-write64 (+ s #x218) i)
                (if (>= i n) (%gc-write64 (+ s #x230) 1) 0)
                (send 1 (+ 900000 i)))))))
    (%sched-park)
    ;; Unreachable when a scheduler took the hand-back; the loop is the honest
    ;; behaviour if one did not, because an actor entry function must not return.
    (loop (yield))))

(defun %br-worker-entry ()
  (- (%gc-word-of (fn-addr %br-worker) (+ (%ha-base) #x80)) 3))

(defun %br-sched-body ()
  "THREAD 2.  Install a GS base and a CPU id, adopt this thread's own GC region,
   and become a scheduler."
  (let ((tb (%ha-thread-block))
        (s (%br-scratch)))
    (%ha-percpu-init-cpu (%ha-cpu1-percpu-base) 1)
    (set-current-actor 0)
    (set-idle-flag 0)
    (%ha-thread-adopt-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ s #x240) (percpu-ref 16))
    (%gc-write64 (+ s #x238) 1)
    (%sched-run)
    (%ha-thread-park-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ s #x248) 1)
    0))

(defun %br-sched-entry ()
  (- (%gc-word-of (fn-addr %br-sched-body) (+ (%ha-base) #x80)) 3))

(defun %br-traffic (ida n gap s)
  "PHASE A.  N messages, GAP milliseconds apart, each acknowledged.  Returns the
   number of acknowledgements collected."
  (let ((i 1)
        (acks 0)
        (bad 0))
    (loop
      (when (> i n) (return 0))
      (%sleep-ms gap)
      (send ida (cons i (+ i 1)))
      ;; One poll per message.  The gap is orders of magnitude longer than a
      ;; round trip, so this does not spin; a missing ack shows up as a short
      ;; count rather than a loop that never ends.
      (%sleep-ms 4)
      (let ((a (try-receive)))
        (if (zerop a)
            0
            (progn
              (if (= a (+ 900000 acks 1)) 0 (setq bad (+ bad 1)))
              (setq acks (+ acks 1)))))
      (setq i (+ i 1)))
    (%gc-write64 (+ s #x258) bad)
    acks))

(defun %br-selftest (mode n gap idlems)
  "ACCEPTANCE for blocking RECEIVE.  MODE 0 blocks, MODE 1 polls (the negative
   control).  Returns the result block's raw byte address, or 0.

   TWO MEASURED WINDOWS, and only the second is the cost claim:
     PHASE A  traffic — N messages GAP ms apart, every one acknowledged.
     PHASE B  an IDLE WINDOW of IDLEMS with NO traffic at all, which is where
              `what does an idle actor cost' is actually answered.  Nothing is
              sent during it, so neither thread contends for the scheduler lock
              and the comparison between the two modes is clean."
  (if (zerop (%ha-carve))
      0
      (let ((s (%br-scratch))
            (tb (%ha-thread-block))
            (band (%ha-base)))
        (if (zerop s)
            0
            (let ((res (+ s #x300))
                  (rcb3 (+ band #x240))
                  (budget 400000000)
                  (k 0) (mode0 0) (ida 0) (tid 0)
                  (w0 0) (c0 0) (acks 0))
              (%ha-zero s (+ s #x400))
              (%ha-zero tb (+ tb #x600))
              (%ha-zero (%ha-cpu1-percpu-base) (+ (%ha-cpu1-percpu-base) #x4000))
              (%sched-reset)
              (%gc-write64 s mode)
              (%gc-write64 (+ s #x208) n)
              (%gc-write64 (+ s #x210) gap)
              (if (zerop (%ha-thread-stack))
                  0
                  (progn
                    (setq mode0 (%ha-percpu-mode))
                    (%ha-actors-bringup n 0)
                    (%ha-percpu-init-cpu (%ha-percpu-base) 0)
                    (setq k (%gc-meta-scale))
                    (%gc-write64 (+ tb #x340) k)
                    ;; Thread 2's region.  STACK_BASE is the top of the actor
                    ;; stack slice its actors live on, for the reason
                    ;; %HA-MT-SELFTEST gives: that is where its live roots are.
                    (%gc-region-init rcb3 *ha-r2-from* *ha-r2-to* *ha-rsize*
                                     (+ (actor-stack-base) #x40000) k)
                    (%gc-write64 (+ tb #x4D0) rcb3)
                    (%ha-set-percpu-mode 1)
                    (setq ida (actor-spawn (%br-worker-entry)))
                    (setq tid (%ha-spawn-t2 (%br-sched-entry)))
                    ;; ---- PHASE A: TRAFFIC ----
                    (setq w0 (%monotonic-ns))
                    (setq c0 (%cpu-ns))
                    (setq acks (%br-traffic ida n gap s))
                    (%gc-write64 (+ res #x08) (- (%monotonic-ns) w0))
                    (%gc-write64 (+ res #x10) (- (%cpu-ns) c0))
                    ;; ---- PHASE B: THE IDLE WINDOW ----
                    (setq w0 (%monotonic-ns))
                    (setq c0 (%cpu-ns))
                    (%sleep-ms idlems)
                    (%gc-write64 (+ res #xA8) (- (%monotonic-ns) w0))
                    (%gc-write64 (+ res #xB0) (- (%cpu-ns) c0))
                    ;; ---- shut the scheduler down and collect the thread ----
                    (%sched-stop)
                    (%gc-write64 (+ res #x18) (%ha-join-t2 budget))
                    (%ha-set-percpu-mode mode0)
                    (%gc-write64 (+ res #x00) tid)
                    (%gc-write64 (+ res #x20) mode)
                    (%gc-write64 (+ res #x28) n)
                    (%gc-write64 (+ res #x30) gap)
                    (%gc-write64 (+ res #x38) (%gc-read64 (+ s #x218)))
                    (%gc-write64 (+ res #x40) (%gc-read64 (+ s #x220)))
                    (%gc-write64 (+ res #x48) acks)
                    (%gc-write64 (+ res #x50) (%gc-read64 (+ s #x258)))
                    (%gc-write64 (+ res #x58) (%gc-read64 (+ s #x228)))
                    (%gc-write64 (+ res #x60) (%gc-read64 (+ s #x230)))
                    (%gc-write64 (+ res #x68) (%gc-read64 (+ s #x238)))
                    (%gc-write64 (+ res #x70) (%gc-read64 (+ s #x248)))
                    (%gc-write64 (+ res #x78) (%gc-read64 (+ s #x240)))
                    (%gc-write64 (+ res #x80)
                                 (%gc-read64 (+ (%thr-sched-block 1) #x40)))
                    (%gc-write64 (+ res #x88)
                                 (%gc-read64 (+ (%thr-sched-block 1) #x48)))
                    (%gc-write64 (+ res #x90)
                                 (%gc-read64 (+ (%thr-sched-block 1) #x50)))
                    (%gc-write64 (+ res #x98)
                                 (%gc-read64 (+ (%thr-sched-globals) #x08)))
                    (%gc-write64 (+ res #xA0) (%gc-read64 (+ band #x190)))
                    res)))))))

;;; ============================================================
;;; THE RUNTIME-TABLE LOCK, FOR REAL
;;; ============================================================
;;;
;;; mvm/prelude.lisp defines %RT-ENTER / %RT-LEAVE as a gate on one BSS word and
;;; two no-op bodies.  These are the bodies, and they do TWO things, because the
;;; shared tables have two independent hazards and a mutex only answers one:
;;;
;;;   1. MUTUAL EXCLUSION over the tables themselves.  Two threads that both
;;;      miss the same GETHASH both allocate a symbol and both PUTHASH it, and
;;;      then `(eq 'foo 'foo)' does not hold across threads.  Worse, PUTHASH
;;;      grows and rehashes, so a concurrent reader can walk a structure that is
;;;      momentarily neither the old one nor the new one.
;;;
;;;   2. WHICH HEAP THE SYMBOL LANDS IN.  This is the half a lock does not fix
;;;      and the reason mvm/gc.lisp calls the mutator/collector meeting out of
;;;      scope.  Per-region GC gives each thread its own heap and each collector
;;;      only copies objects in ITS region's from-space, reachable from the
;;;      globals root set and ITS OWN stack.  A Cheney collector scans only what
;;;      it COPIES, so a table living in region 0 is never walked into by thread
;;;      B's collector — and a symbol thread B allocated in B's own region, whose
;;;      only reference is that table, is not copied and not forwarded.  It is
;;;      garbage the instant B collects, and the table holds a pointer into dead
;;;      from-space.
;;;
;;;      SO EVERYTHING THE LOCKED SECTION ALLOCATES GOES IN REGION 0.  %RT-ENTER
;;;      makes region 0 the active heap (%GC-REGION-ENTER parks the thread's own
;;;      allocation pointer and limit and loads region 0's) and %RT-LEAVE puts
;;;      the thread back.  Under the lock exactly one thread is in region 0 at a
;;;      time, so its single parked allocation frontier is not a shared mutable
;;;      pointer being read by two CPUs — which is what would otherwise hand out
;;;      the same address twice.
;;;
;;; AND THE PRECONDITION THAT MAKES THAT SOUND, STATED PLAINLY: REGION 0 MUST
;;; NOT COLLECT WHILE THIS IS IN USE.  Its root window is [root_sp, stack_base)
;;; and its stack_base is the PROCESS stack — thread 1's.  A collection triggered
;;; from thread 2, which is on an mmap'd stack somewhere else entirely, would
;;; scan a window that is not its stack.  Region 0 is ~840 MB after the carve and
;;; the interned universe is small, so this holds by a wide margin; the
;;; acceptance test MEASURES it (region 0's collection count must not move) rather
;;; than assuming it.  Making region 0 collectable under threads needs a
;;; stop-the-world handshake with per-thread root windows, which is a larger
;;; piece of work than this one and is NOT done here.
;;;
;;; THE LOCK IS RECURSIVE, and that is not a nicety.  %INTERN-SYMBOL-PKG under
;;; the lock allocates, PUTHASHes and compares; any of those can reach a global,
;;; and SYMBOL-VALUE takes the same lock.  A non-recursive mutex would deadlock
;;; on the first such call.  Ownership is the CPU id (one GS-relative load)
;;; rather than gettid (a syscall on every uncontended acquire), which is why
;;; %RT-THREADS-ON refuses unless per-CPU storage is actually on: with the mode
;;; word off, every thread reads CPU 0 and two threads would each believe they
;;; already owned the lock.
;;;
;;; WORDS (BSS, all zero-filled, all in the free gap above 0x10000DA0):
;;;   0x10000DB8 threads-live gate (u32)   0x10000DC0 the futex mutex word
;;;   0x10000DC8 owner (cpu id + 1)        0x10000DD0 recursion depth
;;;   0x10000DD8 the owner's own region    0x10000DE0 acquisitions
;;;   0x10000DE8 acquisitions that had to wait

(defun %rt-gate-addr ()  #x10000DB8)
(defun %rt-mutex-addr () #x10000DC0)
(defun %rt-owner-addr () #x10000DC8)
(defun %rt-depth-addr () #x10000DD0)
(defun %rt-saved-addr () #x10000DD8)

;;; ============================================================
;;; B-LITE: PER-CPU ALLOCATION SLICES FOR LOCKED SECTIONS
;;; ============================================================
;;;
;;; THE DEFECT THIS REMOVES, measured before it was designed
;;; (test/run-region0-frontier.sh): region 0 had TWO mutators and ONE frontier
;;; stored in TWO places.  Main allocates region 0 from its REGISTERS, outside
;;; the lock, because region 0 is its heap; a locked worker allocated region 0
;;; from the PARKED field (+0x30), which does not track main's registers
;;; between hops; and main's next hop parked its register value back OVER the
;;; worker's advance.  A worker's region-0 cons came back overwritten in 5 of
;;; 17 runs while the same worker's own-region cons survived 17 of 17.
;;;
;;; THE FIX: a locked section no longer touches region 0's frontier AT ALL.
;;; %RT-THREADS-ON carves an ARENA off the top of region 0's semispace —
;;; address space main can never reach, because the carve shrinks region 0's
;;; SIZE and main's allocation limit in the same breath (%GC-REGION-SHRINK,
;;; the same gesture %HA-CARVE has always made) — and each CPU gets a SLICE of
;;; it: a 64-byte block whose +0x30/+0x38 the existing %GC-REGION-ENTER parks
;;; and loads exactly as it would a region's.  Disjoint ranges, no
;;; coordination: main's registers and every worker's slice advance without
;;; ever reading each other.  Region 0's parked field is once again written
;;; only by its owner's own enter/leave pairs.
;;;
;;; WHY THE OBJECTS ARE STILL "IN REGION 0" WHERE IT MATTERS: the arena is
;;; region-0 ADDRESS SPACE, so a region-0 table entry pointing at a slice
;;; object is not a cross-region reference and dangles under nobody's
;;; collection — which is the property the whole lock discipline exists to
;;; buy.  (Shape A died at its gate for lacking exactly this; see CLAUDE.md.)
;;;
;;; WHAT HAPPENS WHEN REGION 0 COLLECTS — the question that killed the
;;; alternative designs, answered by READ MECHANICS rather than hope: the
;;; trampoline loads from_start/to_start/space_size FROM THE REGION BLOCK AT
;;; COLLECTION TIME (translate-x64.lisp, emit-gc-trampoline entry), so after
;;; the shrink the arena is above the from-space membership test and outside
;;; the copy destination of EVERY future collection, whichever semispace holds
;;; it.  Arena objects are never evacuated and never overwritten: the arena is
;;; IMMORTAL.  No re-carve hook, no epoch, no stale slice after a flip —
;;; there is nothing to go stale.  The cost of immortality is that arena
;;; garbage is never reclaimed; at the measured 128 bytes per fresh locked
;;; intern (percall, K=20, the only shape that survives measuring on the
;;; pre-fix tree) the 32 MB arena affords ~260 000 fresh interns per process,
;;; two orders of magnitude above a full glass load.
;;;
;;; DEFAULT-OFF, BYTE FOR BYTE: the slice path exists only behind the
;;; threads-live gate, which only %RT-THREADS-ON sets, and only when the carve
;;; produced an arena (big heaps; small ones keep today's path).  An image or
;;; a run that never brings up actors never executes one instruction of this.
;;;
;;; RESIDUAL, STATED PLAINLY (pre-existing, narrowed but NOT closed): main can
;;; still collect region 0 naturally while a worker is inside the lock.  The
;;; worker's frontier and its fresh objects now survive that (they are in the
;;; immortal arena), which is strictly better than before — but a worker READING
;;; a region-0 structure mid-evacuation still races the collector, exactly as
;;; it always has.
;;;
;;; SLICE EXHAUSTION: an outermost %RT-ENTER refills this CPU's slice from the
;;; arena whenever headroom is below 64 KB, so a single locked section has at
;;; least that; a section allocating MORE than 64 KB in one hold would run the
;;; slice into its limit and the gc-check would collect THE SLICE BLOCK, which
;;; is meaningless — that is this design's one landmine, sized 64 KB against
;;; locked sections that measure in hundreds of bytes.  An EXHAUSTED ARENA
;;; falls back to today's region-0 path and COUNTS the fall (the word after
;;; the arena triple), so the collision can come back only visibly.
;;;
;;; WORDS (band, after the 16 thread-region-report slots at +0xB000):
;;;   +0xB800  16 x 64-byte per-CPU slice blocks (RCB layout; +0x30/+0x38 live)
;;;   +0xBC00  arena base   +0xBC08 arena frontier   +0xBC10 arena end
;;;   +0xBC18  fallback count (arena exhausted -> old path, counted)

(defun %rt-slice-base ()   (+ (%ha-base) #xB800))
(defun %rt-slice-block (cpu) (+ (%rt-slice-base) (* cpu #x40)))
(defun %rt-arena-words ()  (+ (%ha-base) #xBC00))
(defun %rt-arena-base ()
  (if (zerop (%ha-base)) 0 (%gc-read64 (%rt-arena-words))))
(defun %rt-arena-alloc ()
  (if (zerop (%ha-base)) 0 (%gc-read64 (+ (%rt-arena-words) #x08))))
(defun %rt-arena-end ()
  (if (zerop (%ha-base)) 0 (%gc-read64 (+ (%rt-arena-words) #x10))))
(defun %rt-arena-fallbacks ()
  (if (zerop (%ha-base)) 0 (%gc-read64 (+ (%rt-arena-words) #x18))))

(defun %rt-slice-ensure ()
  "This CPU's slice block with at least 64 KB of headroom, refilled from the
   arena if not — the caller holds the runtime mutex, which is what makes the
   arena frontier single-writer.  0 when there is no arena or it is exhausted
   (counted), in which case the caller uses the pre-B-lite path."
  (let ((ae (%rt-arena-end)))
    (if (zerop ae)
        0
        (let* ((k (%gc-meta-scale))
               (blk (%rt-slice-block (%thr-cpu)))
               (alloc (%gc-meta-read (+ blk #x30) k))
               (limit (%gc-meta-read (+ blk #x38) k)))
          (if (>= (- limit alloc) #x10000)
              blk
              (let ((af (%rt-arena-alloc)))
                (if (> (+ af #x100000) ae)
                    (progn
                      (%gc-write64 (+ (%rt-arena-words) #x18)
                                   (+ (%rt-arena-fallbacks) 1))
                      0)
                    (progn
                      ;; A slice IS a region block as far as %GC-REGION-ENTER
                      ;; is concerned: init writes +0x30 = AF, +0x38 = AF+1MB.
                      ;; It is never collected, so from/to/stack are inert.
                      (%gc-region-init blk af af #x100000 0 k)
                      (%gc-write64 (+ (%rt-arena-words) #x08) (+ af #x100000))
                      blk))))))))

(defun %rt-arena-carve ()
  "Carve the immortal lock arena off the top of region 0's CURRENT from-space,
   once, shrinking region 0's size — and, in the same breath, the live
   allocation limit if region 0 is active and the parked one if it is not —
   so main can never reach it.  1 = arena ready, 0 = heap too small or
   frontier already past the carve point (both keep today's path)."
  (if (> (%rt-arena-end) 0)
      1
      (let* ((k (%gc-meta-scale))
             (r0 (%gc-region-0))
             (from (%gc-meta-read r0 k))
             (size (%gc-meta-read (+ r0 #x10) k)))
        (if (< size #x6000000)
            0
            (let ((newsize (- size #x2000000)))
              (if (or (> (%gc-meta-read (+ r0 #x30) k) (+ from newsize))
                      (and (= r0 (%gc-region))
                           (> (get-alloc-ptr) (+ from newsize))))
                  0
                  (let ((base (%ha-align-up-to-page-base (+ from newsize))))
                    (%gc-region-shrink r0 newsize k)
                    ;; If main is NOT in region 0 right now (%TL-SELFTEST's
                    ;; shape), the shrink moved no live register; clamp the
                    ;; PARKED limit so the next enter of region 0 honors the
                    ;; carve too.
                    (if (> (%gc-meta-read (+ r0 #x38) k) (+ from newsize))
                        (%gc-meta-write (+ r0 #x38) (+ from newsize) k)
                        0)
                    (%ha-zero (%rt-slice-base) (+ (%rt-slice-base) #x400))
                    (%gc-write64 (%rt-arena-words) base)
                    (%gc-write64 (+ (%rt-arena-words) #x08) base)
                    (%gc-write64 (+ (%rt-arena-words) #x10) (+ from size))
                    (%gc-write64 (+ (%rt-arena-words) #x18) 0)
                    1)))))))

(defun %rt-enter-locked ()
  (let ((me (+ (%thr-cpu) 1)))
    (if (= (%gc-read64 (%rt-owner-addr)) me)
        (progn
          (%gc-write64 (%rt-depth-addr) (+ (%gc-read64 (%rt-depth-addr)) 1))
          0)
        (progn
          ;; The contention counter is a READ, not a TRYLOCK.  A trylock here
          ;; would be a second unconditional XCHG on the way in, and an XCHG
          ;; that lands on a mutex in state 2 writes 1 over it — erasing the
          ;; waiters flag for the length of one instruction, for a diagnostic.
          ;; A plain load cannot disturb anything.
          (if (zerop (%gc-read64 (%rt-mutex-addr)))
              0
              (%gc-write64 #x10000DE8 (+ (%gc-read64 #x10000DE8) 1)))
          (%mutex-lock (%rt-mutex-addr))
          (%gc-write64 #x10000DE0 (+ (%gc-read64 #x10000DE0) 1))
          (%gc-write64 (%rt-owner-addr) me)
          (%gc-write64 (%rt-depth-addr) 1)
          ;; PARK MY REGION, TAKE MY SLICE — or, with no arena (small heap,
          ;; exhausted arena, or an image from before the carve), REGION 0
          ;; exactly as before.  The slice is region-0 ADDRESS SPACE that main
          ;; can never reach, so the locked section's allocations are
          ;; co-located with the tables WITHOUT touching the frontier main
          ;; also uses — the two-mutators-one-frontier collision this block
          ;; exists to document.  %GC-REGION-ENTER returns the region it left,
          ;; which is exactly what %RT-LEAVE needs to undo it; on the way out
          ;; it parks the SLICE block, which is what persists the slice
          ;; frontier for this CPU's next acquisition.
          (let ((blk (%rt-slice-ensure)))
            (%gc-write64 (%rt-saved-addr)
                         (%gc-region-enter (if (zerop blk) (%gc-region-0) blk))))
          0))))

(defun %rt-leave-locked ()
  "Release one level.  At the outermost level: put this thread back in its own
   region, then drop ownership, then the mutex.

   THE DEPTH IS NOT DECREMENTED TO ZERO UNTIL AFTER THE REGION IS RESTORED, and
   that ordering is load-bearing rather than tidy.  %GC-REGION-ENTER is ordinary
   compiled Lisp; anything it touches that the compiler resolved as an implicit
   global becomes a SYMBOL-VALUE call, which takes this same lock.  With the
   depth already at 0 that nested acquire would see itself as the OUTERMOST
   holder, restore the region a second time and UNLOCK THE MUTEX — and then this
   frame would unlock it again, releasing a lock the OTHER thread by then owns.
   Two threads in region 0 with one parked allocation frontier between them is
   the exact corruption this lock exists to prevent.  Holding the depth at 1
   across the restore makes any such nested pair a no-op."
  (let ((me (+ (%thr-cpu) 1)))
    (if (= (%gc-read64 (%rt-owner-addr)) me)
        (let ((d (- (%gc-read64 (%rt-depth-addr)) 1)))
          (if (> d 0)
              (progn (%gc-write64 (%rt-depth-addr) d) 0)
              (let ((back (%gc-read64 (%rt-saved-addr))))
                (%gc-region-enter back)
                (%gc-write64 (%rt-depth-addr) 0)
                (%gc-write64 (%rt-owner-addr) 0)
                (%mutex-unlock (%rt-mutex-addr))
                0)))
        0)))

(defun %rt-threads-on ()
  "Declare that more than one thread is about to run Lisp through the shared
   runtime tables.  Returns 1 on success, 0 if the precondition is not met.

   IT IS AN EXPLICIT ACT, not something %SPAWN-THREAD does.  The existing
   threaded selftests deliberately run outside the runtime tables and manage
   their own regions; switching every intern to region 0 underneath them would
   change what they measure.  A program that wants a second thread to run REAL
   Lisp says so."
  (if (= (mem-ref #x10000FF8 :u32) 0)
      0
      (progn
        (%gc-write64 (%rt-owner-addr) 0)
        (%gc-write64 (%rt-depth-addr) 0)
        (%mutex-init (%rt-mutex-addr))
        (%gc-write64 #x10000DE0 0)
        (%gc-write64 #x10000DE8 0)
        ;; B-LITE (see the block above %RT-SLICE-BASE): carve the lock arena
        ;; BEFORE the gate opens, so no locked section ever runs against a
        ;; half-carved arena.  A 0 here (small heap, frontier in the way) is
        ;; not a failure — the slice path just never engages and every locked
        ;; section behaves exactly as before this change.
        (%rt-arena-carve)
        (setf (mem-ref (%rt-gate-addr) :u32) 1)
        1)))

(defun %rt-threads-off ()
  (setf (mem-ref (%rt-gate-addr) :u32) 0)
  0)

(defun %rt-acquisitions () (%gc-read64 #x10000DE0))
(defun %rt-contended ()    (%gc-read64 #x10000DE8))

;;; ============================================================
;;; ACCEPTANCE — TWO THREADS RUNNING REAL LISP AT THE SAME TIME
;;; ============================================================
;;;
;;; NOT ARITHMETIC.  Every threaded selftest before this one was restricted to
;;; "arithmetic, raw memory access and message passing only — no FORMAT, INTERN,
;;; EVAL, or symbol/keyword literal", and that restriction was the ceiling, not
;;; an accident: the shared tables had no synchronisation, so a second thread
;;; could run computation but not Lisp.  This is the workload the restriction
;;; forbade, run on both threads at once.
;;;
;;; EACH THREAD, EVERY ITERATION:
;;;   1. INTERNS A FRESH SYMBOL of its own twice and requires the two to be EQ.
;;;   2. INTERNS A SHARED SYMBOL — a name-hash BOTH threads intern — and records
;;;      the machine word it got.  The driver compares the two threads' records
;;;      afterwards: a symbol interned on either thread must be EQ-identical
;;;      when looked up from either.  This is the check the whole exercise is
;;;      about, and nothing else in the tree makes it.
;;;   3. CALLS FORMAT, which allocates a string in this thread's OWN region and
;;;      interns a keyword for every `:foo' literal it evaluates on the way
;;;      through the printer — i.e. it hits the shared keyword table hard.
;;;   4. WRITES AND READS BACK A GLOBAL through SET-SYMBOL-VALUE / SYMBOL-VALUE,
;;;      i.e. the globals hash table.
;;;   5. DEFINES A FUNCTION under a freshly CONSTRUCTED name in the shared
;;;      symbol-function table and CALLS IT BACK BY NAME.  (The name string is
;;;      built by FORMAT under the lock, so it lands in region 0 and is immortal
;;;      — a hash KEY that its own thread's collector could move is a dangling
;;;      key.)
;;;   6. CONSES onto a live chain, and every GCEVERY iterations FORCES A
;;;      COLLECTION OF ITS OWN REGION and then re-checks: the chain still
;;;      checksums, and re-interning the shared name still returns the SAME
;;;      object.
;;;
;;; WHAT WOULD MAKE THIS TEST A LIE, and what stops it.
;;;
;;;   "It completed" is not enough, so nothing here is asserted from a flag the
;;;   worker set about itself: the shared-symbol identities are RAW MACHINE
;;;   WORDS recorded by each thread and compared by the driver afterwards, and
;;;   the driver ALSO re-interns each one itself and requires the same answer.
;;;
;;;   "Both threads ran" is a barrier with a spin budget, as everywhere else
;;;   here: a sequential run spins its budget out alone and reports a timeout.
;;;
;;;   "The counts rose" is per thread and independent — each thread's iteration
;;;   counter must reach N on its own.
;;;
;;;   THE PRECONDITION IS MEASURED, NOT ASSUMED.  Region 0 is the shared runtime
;;;   heap and must not collect (see the block above); the driver records its
;;;   collection count before and after and requires it unchanged, while
;;;   requiring BOTH threads' own regions to have collected several times.
;;;
;;;   AND THE NEGATIVE CONTROL IS ONE WORD.  MODE 1 leaves the threads-live gate
;;;   OFF, so %RT-ENTER and %RT-LEAVE are the no-ops they are in a
;;;   single-threaded image: no mutex, no region switch, the exact code path
;;;   this work added, removed.  Same binary, same workload.  It lives in its
;;;   own script (test/hosted-thread-lisp-unsync.lisp) because a corrupted
;;;   intern table is not obliged to fail politely — losing the race can take
;;;   the process down, and a process that dies is evidence too.
;;;
;;; CONTROL BLOCK (offsets from %TL-CTL = thread-page scratch + 0x800):
;;;   +0x00 arrays base (a 64 KB mmap)   +0x08 N        +0x10 mode
;;;   +0x18 gc-every                     +0x20 barrier  +0x28 barrier mutex
;;;   +0x30 t2 started                   +0x38 t2 finished
;;;   +0x40 t2 barrier timeouts          +0x48 t1 barrier timeouts
;;;   +0x100 + 0x80*slot  PER-THREAD BLOCK:
;;;     +0x00 iterations   +0x08 own-symbol EQ failures   +0x10 FORMAT failures
;;;     +0x18 chain-survival failures     +0x20 forced collections
;;;     +0x28 global read-back failures   +0x30 function-table call failures
;;;     +0x38 shared-symbol identity changed across a collection
;;;     +0x40 my region's collection count  +0x48 my cpu id
;;;     +0x50 alloc ptr at start  +0x58 alloc ptr at end  +0x70 word scratch
;;;   +0x400 RESULT BLOCK

(defun %tl-ctl () (+ (%thr-scratch) #x800))
(defun %tl-res () (+ (%thr-scratch) #xC00))
(defun %tl-slot (s) (+ (%tl-ctl) (+ #x100 (* s #x80))))
(defun %tl-bump (me off) (%gc-write64 (+ me off) (+ (%gc-read64 (+ me off)) 1)) 0)

;; Name-hash bases, chosen far away from anything the image's dual-FNV-1a
;; hashing produces for a real name, so a collision cannot quietly turn a
;; "fresh" symbol into an existing one.
(defun %tl-shared-hash (i)      (+ 1099511627776 i))
(defun %tl-own-hash (slot i)    (+ 2199023255552 (+ (* slot 1048576) i)))
(defun %tl-global-hash (slot i) (+ 3298534883328 (+ (* slot 1048576) i)))

(defun %tl-barrier (ctl budget)
  "0 = both threads arrived; 1 = spun out BUDGET alone, which is what a
   SEQUENTIAL run scores."
  (%mutex-lock (+ ctl #x28))
  (%gc-write64 (+ ctl #x20) (+ (%gc-read64 (+ ctl #x20)) 1))
  (%mutex-unlock (+ ctl #x28))
  (let ((i 0) (r 1))
    (loop
      (when (>= (%gc-read64 (+ ctl #x20)) 2) (progn (setq r 0) (return 0)))
      (when (>= i budget) (return 0))
      (setq i (+ i 1)))
    r))

(defun %tl-chain (n)
  (let ((c nil) (i 0))
    (loop
      (when (>= i n) (return 0))
      (setq c (cons (+ i 1) c))
      (setq i (+ i 1)))
    c))

(defun %tl-chain-sum (c)
  (let ((s 0) (p c))
    (loop
      (when (null p) (return 0))
      (setq s (+ s (car p)))
      (setq p (cdr p)))
    s))

(defun %tl-callee (x) (+ x 7))

(defun %tl-fname (slot i)
  "A FRESHLY CONSTRUCTED function name, built under the runtime lock so the
   string lands in region 0.  A hash KEY that its own thread's collector can
   move is a dangling key the next time anything walks that bucket."
  (%rt-enter)
  (let ((s (format nil "%TLF~D-~D" slot i)))
    (%rt-leave)
    s))

(defun %tl-defun (name fn)
  (%rt-enter)
  (puthash name *symbol-function-table* fn)
  (%rt-leave)
  0)

(defun %tl-fn (name)
  (%rt-enter)
  (let ((f (gethash name *symbol-function-table*)))
    (%rt-leave)
    f))

(defun %tl-run (slot)
  "THE WORKLOAD.  Both threads run this, with different SLOTs.

   THE SHARED-RUNTIME WORK OF ONE ITERATION IS ONE CRITICAL SECTION, and that is
   a deliberate first cut rather than an oversight.  Locking each table
   operation on its own is enough for the TABLES, and the wrappers in
   mvm/prelude.lisp do exactly that — but FORMAT and the printer also touch two
   pieces of shared BSS that are NOT tables and are NOT per-thread: the
   multiple-value return buffer at 0x10000090 (one buffer for every CPU; two
   threads returning multiple values clobber each other's extras) and the
   handler-frame stack at 0x10000400 (one depth counter and one frame array).
   Making those per-thread means moving addresses the COMPILER bakes into every
   emitted MULTIPLE-VALUE-BIND and every function epilogue on four back-ends;
   that is its own campaign.  Until then, running the Lisp-level work of one
   iteration under the runtime lock keeps two threads from being inside the
   printer at the same instant, and it is honest to say so: the CONSING and the
   FORCED COLLECTIONS below are genuinely concurrent, the shared-runtime work
   interleaves.

   The lock is RECURSIVE, so every INTERN, SYMBOL-VALUE and macro-table call
   inside this section takes it again and returns to the right depth."
  (let ((ctl (%tl-ctl)))
    (let ((arr (%gc-read64 ctl))
          (n (%gc-read64 (+ ctl #x08)))
          (gcevery (%gc-read64 (+ ctl #x18)))
          (me (%tl-slot slot))
          (i 1)
          (since 0)
          (sh nil)
          (chain nil)
          (want 0))
      (setq chain (%tl-chain 64))
      (setq want (%tl-chain-sum chain))
      (%gc-write64 (+ me #x48) (%thr-cpu))
      (%gc-write64 (+ me #x50) (get-alloc-ptr))
      (loop
        (when (> i n) (return 0))
        (%gc-write64 (+ me #x78) 1)
        ;; ---- THE SHARED RUNTIME ----------------------------------------
        (%rt-enter)
        ;; 1. a symbol only THIS thread interns, twice — must be EQ
        (let ((a (%intern-symbol-pkg (%tl-own-hash slot i) 0))
              (b (%intern-symbol-pkg (%tl-own-hash slot i) 0)))
          (if (eq a b) 0 (%tl-bump me #x08)))
        (%gc-write64 (+ me #x78) 2)
        ;; 2. a symbol BOTH threads intern; record the machine word
        (setq sh (%intern-symbol-pkg (%tl-shared-hash i) 0))
        (%gc-write64 (+ (+ arr (* slot #x8000)) (* i 8))
                     (%gc-word-of sh (+ me #x70)))
        (%gc-write64 (+ me #x78) 3)
        ;; 3. FORMAT — a string, and a keyword interned in the SHARED table for
        ;;    every `:foo' the printer evaluates on the way through
        (let ((str (format nil "~D-~D" slot i)))
          (if (> (length str) 2) 0 (%tl-bump me #x10)))
        (%gc-write64 (+ me #x78) 4)
        ;; 4. a global of my own, written and read back
        (set-symbol-value (%tl-global-hash slot i) (* i 3))
        (if (= (symbol-value (%tl-global-hash slot i)) (* i 3))
            0 (%tl-bump me #x28))
        (%gc-write64 (+ me #x78) 5)
        ;; 5. a function under a FRESHLY CONSTRUCTED name in the shared
        ;;    symbol-function table, called back by name
        (let ((nm (%tl-fname slot i)))
          (%tl-defun nm (function %tl-callee))
          (let ((f (%tl-fn nm)))
            (if (null f)
                (%tl-bump me #x30)
                (if (= (funcall f i) (+ i 7)) 0 (%tl-bump me #x30)))))
        (%rt-leave)
        ;; ---- MY OWN REGION, concurrently with the other thread ----------
        (%gc-write64 (+ me #x78) 6)
        (setq chain (cons i chain))
        (setq want (+ want i))
        (setq since (+ since 1))
        (if (>= since gcevery)
            (progn
              (setq since 0)
              (%gc-write64 (+ me #x78) 7)
              (%ha-collect-here)
              (%tl-bump me #x20)
              (if (= (%tl-chain-sum chain) want) 0 (%tl-bump me #x18))
              ;; SH lives in region 0, which nothing collects; re-interning the
              ;; same name after MY region moved must hand back the SAME object.
              (if (eq (%intern-symbol-pkg (%tl-shared-hash i) 0) sh)
                  0 (%tl-bump me #x38)))
            0)
        (%gc-write64 me i)
        (setq i (+ i 1)))
      (%gc-write64 (+ me #x78) 99)
      (%gc-write64 (+ me #x58) (get-alloc-ptr))
      (%gc-write64 (+ me #x40) (%ha-my-gc-count))
      0)))

(defun %tl-t2-body ()
  "THREAD 2.  Its own GS base, its own CPU id, its own GC region — and only
   THEN anything that touches the runtime tables, because %RT-ENTER saves and
   restores THIS THREAD'S region and a freshly cloned thread does not have one
   yet."
  (let ((tb (%ha-thread-block))
        (ctl (%tl-ctl)))
    ;; ITS OWN PER-THREAD WINDOW, FIRST.  Until this returns, this thread's
    ;; multiple values and handler-case frames are the MAIN THREAD'S — it was
    ;; cloned without CLONE_SETTLS, so it inherited FS base 0.  Everything
    ;; below returns values and arms handler-cases; none of it may do so into
    ;; somebody else's buffer.
    (%gc-write64 (+ ctl #x50) (%tls-install 1))
    (%ha-percpu-init-cpu (%ha-cpu1-percpu-base) 1)
    (set-current-actor 0)
    (set-idle-flag 0)
    (%ha-thread-adopt-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ ctl #x30) 1)
    (%gc-write64 (+ ctl #x40) (%tl-barrier ctl 400000000))
    (%tl-run 1)
    (%ha-thread-park-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ ctl #x38) 1)
    0))

(defun %tl-t2-entry ()
  (- (%gc-word-of (fn-addr %tl-t2-body) (+ (%ha-base) #x80)) 3))

(defvar *tl-arrays* 0)

(defun %tl-arrays ()
  "64 KB for the two threads' recorded shared-symbol pointers, mapped once.
   NOT the heap: these are raw machine words, and a word in the heap that
   happens to look like a pointer is a conservative root."
  (if (> *tl-arrays* 0)
      *tl-arrays*
      (let ((m (%mmap-shared-page 65536)))
        (if (< m 4096) 0 (progn (setq *tl-arrays* m) m)))))

(defun %tl-selftest (mode n gcevery)
  "TWO THREADS RUNNING REAL LISP.  MODE 0 synchronises (the shipping path);
   MODE 1 leaves the threads-live gate OFF, which is this work REMOVED.
   Returns the result block's raw byte address, or 0."
  (if (zerop (%ha-carve))
      0
      (let ((ctl (%tl-ctl))
            (band (%ha-base)))
        (if (zerop ctl)
            0
            (let ((res (%tl-res))
                  (rcb2 (+ band #x200))
                  (rcb3 (+ band #x240))
                  (r0 (%gc-region-0))
                  (arr (%tl-arrays))
                  (budget 400000000)
                  (k 0) (mode0 0) (tid 0) (g0 0) (a0 0)
                  (bad 0) (drv 0) (dup 0) (zero 0) (i 1))
              (if (zerop arr)
                  0
                  (progn
                    (%ha-zero ctl (+ ctl #x800))
                    (%ha-zero arr (+ arr 65536))
                    (%ha-zero (%ha-cpu1-percpu-base)
                              (+ (%ha-cpu1-percpu-base) #x4000))
                    (%gc-write64 ctl arr)
                    (%gc-write64 (+ ctl #x08) n)
                    (%gc-write64 (+ ctl #x10) mode)
                    (%gc-write64 (+ ctl #x18) gcevery)
                    (%mutex-init (+ ctl #x28))
                    (if (zerop (%ha-thread-stack))
                        0
                        (progn
                          (setq mode0 (%ha-percpu-mode))
                          (setq a0 (get-alloc-ptr))
                          (%ha-percpu-init-cpu (%ha-percpu-base) 0)
                          (setq k (%gc-meta-scale))
                          (%gc-write64 (+ (%ha-thread-block) #x340) k)
                          (%gc-region-init rcb2 *ha-r1-from* *ha-r1-to*
                                           *ha-rsize*
                                           (%gc-meta-read (+ r0 #x18) k) k)
                          (%gc-region-init rcb3 *ha-r2-from* *ha-r2-to*
                                           *ha-rsize*
                                           (+ *ha-t2-stack* *ha-t2-stack-size*)
                                           k)
                          (%gc-write64 (+ (%ha-thread-block) #x4D0) rcb3)
                          (%ha-set-percpu-mode 1)
                          ;; THIS THREAD TAKES ITS OWN REGION FIRST, and that is
                          ;; load-bearing: %GC-REGION-ENTER parks the region it
                          ;; LEAVES, so this is what writes region 0's saved
                          ;; allocation pointer and limit.  Without it the first
                          ;; %RT-ENTER would load whatever %GC-REGION-INIT left
                          ;; in region 0's block.
                          (%gc-region-enter rcb2)
                          (setq g0 (%gc-meta-read (+ r0 #x20) k))
                          (if (zerop mode) (%rt-threads-on) 0)
                          (setq tid (%ha-spawn-t2 (%tl-t2-entry)))
                          (%gc-write64 (+ ctl #x48) (%tl-barrier ctl budget))
                          (%tl-run 0)
                          (%gc-write64 (+ res #x08) (%ha-join-t2 budget))
                          ;; ---- back to a single thread; audit ----
                          (%rt-threads-off)
                          (%gc-write64 (+ res #x10)
                                       (%gc-meta-read (+ rcb2 #x20) k))
                          (%gc-write64 (+ res #x18)
                                       (%gc-meta-read (+ rcb3 #x20) k))
                          (%gc-write64 (+ res #x20) g0)
                          (%gc-write64 (+ res #x28)
                                       (%gc-meta-read (+ r0 #x20) k))
                          ;; THE IDENTITY CHECK.  The two threads' recorded
                          ;; machine words for each shared name must be equal —
                          ;; and the DRIVER re-interns each one itself and
                          ;; requires the same answer, so this is not two
                          ;; workers agreeing with each other about a table
                          ;; neither of them can see.
                          (loop
                            (when (> i n) (return 0))
                            (let ((w0 (%gc-read64 (+ arr (* i 8))))
                                  (w1 (%gc-read64 (+ (+ arr #x8000) (* i 8)))))
                              (if (zerop w0) (setq zero (+ zero 1)) 0)
                              (if (zerop w1) (setq zero (+ zero 1)) 0)
                              (if (= w0 w1) 0 (setq bad (+ bad 1)))
                              (if (= w0 (%gc-word-of
                                         (%intern-symbol-pkg
                                          (%tl-shared-hash i) 0)
                                         (+ ctl #x60)))
                                  0 (setq drv (+ drv 1)))
                              (if (> i 1)
                                  (if (= w0 (%gc-read64
                                             (+ arr (* (- i 1) 8))))
                                      (setq dup (+ dup 1))
                                      0)
                                  0))
                            (setq i (+ i 1)))
                          (%gc-write64 (+ res #x30) bad)
                          (%gc-write64 (+ res #x38) drv)
                          (%gc-write64 (+ res #x40) dup)
                          (%gc-write64 (+ res #x48) zero)
                          (%gc-write64 (+ res #x50) n)
                          (%gc-write64 (+ res #x58) mode)
                          (%gc-write64 (+ res #x60) tid)
                          (%gc-write64 (+ res #x68) (%gc-read64 (+ ctl #x30)))
                          (%gc-write64 (+ res #x70) (%gc-read64 (+ ctl #x38)))
                          (%gc-write64 (+ res #x78) (%gc-read64 (+ ctl #x40)))
                          (%gc-write64 (+ res #x80) (%gc-read64 (+ ctl #x48)))
                          (%gc-write64 (+ res #x88) (%rt-acquisitions))
                          (%gc-write64 (+ res #x90) (%rt-contended))
                          (%gc-write64 (+ res #x98) (%gc-read64 (+ band #x190)))
                          ;; per-thread blocks, verbatim
                          (%gc-write64 (+ res #xA0) (%gc-read64 (%tl-slot 0)))
                          (%gc-write64 (+ res #xA8) (%gc-read64 (%tl-slot 1)))
                          (%gc-write64 (+ res #xB0)
                                       (+ (%gc-read64 (+ (%tl-slot 0) #x08))
                                          (%gc-read64 (+ (%tl-slot 1) #x08))))
                          (%gc-write64 (+ res #xB8)
                                       (+ (%gc-read64 (+ (%tl-slot 0) #x10))
                                          (%gc-read64 (+ (%tl-slot 1) #x10))))
                          (%gc-write64 (+ res #xC0)
                                       (+ (%gc-read64 (+ (%tl-slot 0) #x18))
                                          (%gc-read64 (+ (%tl-slot 1) #x18))))
                          (%gc-write64 (+ res #xC8)
                                       (+ (%gc-read64 (+ (%tl-slot 0) #x28))
                                          (%gc-read64 (+ (%tl-slot 1) #x28))))
                          (%gc-write64 (+ res #xD0)
                                       (+ (%gc-read64 (+ (%tl-slot 0) #x30))
                                          (%gc-read64 (+ (%tl-slot 1) #x30))))
                          (%gc-write64 (+ res #xD8)
                                       (+ (%gc-read64 (+ (%tl-slot 0) #x38))
                                          (%gc-read64 (+ (%tl-slot 1) #x38))))
                          (%gc-write64 (+ res #xE0)
                                       (%gc-read64 (+ (%tl-slot 0) #x20)))
                          (%gc-write64 (+ res #xE8)
                                       (%gc-read64 (+ (%tl-slot 1) #x20)))
                          (%gc-write64 (+ res #xF0)
                                       (%gc-read64 (+ (%tl-slot 0) #x48)))
                          (%gc-write64 (+ res #xF8)
                                       (%gc-read64 (+ (%tl-slot 1) #x48)))
                          ;; put this thread back where it started
                          (%gc-region-enter r0)
                          (%ha-set-percpu-mode mode0)
                          (%gc-write64 (+ res #x100) a0)
                          (%gc-write64 (+ res #x108) (get-alloc-ptr))
                          (%gc-write64 (+ res #x110) (%futex-timeouts))
                          (%gc-write64 (+ res #x118)
                                       (%gc-read64 (+ (%tl-slot 0) #x78)))
                          (%gc-write64 (+ res #x120)
                                       (%gc-read64 (+ (%tl-slot 1) #x78)))
                          res)))))))))

;;; ============================================================
;;; MULTIPLE VALUES AND HANDLER-CASE, ON TWO THREADS AT ONCE
;;; ============================================================
;;;
;;; WHAT THIS PINS.  The multiple-value return buffer and the handler-frame
;;; stack used to be ONE COPY FOR THE WHOLE IMAGE (mvm/compiler.lisp, THE
;;; PER-THREAD WINDOW).  A producer stores its extras and the consumer reads
;;; them at the call site; a handler-case arms one global frame and its
;;; matching CLEAR-HANDLER pops one global stack.  With two OS threads and a
;;; preemptive kernel scheduler, both are races — and the handler one is not a
;;; wrong ANSWER but a wrong JUMP: thread A's unwind restores a frame thread B
;;; armed and lands on B's stack, which is what the campaign's captured faults
;;; ("control transfer to 0 or into a live stack") looked like.
;;;
;;; WHAT MAKES IT A TEST RATHER THAN AN EXERCISE.  Every check is a VALUE the
;;; thread computed and compared itself, tallied per thread, and the driver
;;; requires BOTH tallies to be zero and BOTH iteration counts to reach N.
;;; "It finished" is not accepted as a result: a sequential run spins its
;;; barrier budget out alone and reports a timeout, and both timeouts must be
;;; 0, so this cannot pass unless the two threads were inside the workload at
;;; the same instant.  The forced collections are real collections of each
;;; thread's own region, counted.
;;;
;;; AND THE NEGATIVE CONTROL IS ONE WORD, IN THE SAME BINARY, ON THE SAME
;;; WORKLOAD.  MODE 1 makes thread 2 SKIP %TLS-INSTALL, so it keeps the FS
;;; base it was cloned with — base 0, the main thread's window.  That is this
;;; fix removed, exactly and only: the same code, the same collections, the
;;; same iteration count, one segment base not installed.  It lives in its own
;;; script (test/hosted-mv-handler-unsync.lisp) because a longjmp onto another
;;; thread's stack is not obliged to fail politely.
;;;
;;; NOTHING HERE TOUCHES THE SHARED RUNTIME TABLES, and that is deliberate:
;;; this must fail because of the WINDOW, not because of the intern tables the
;;; runtime lock already covers.  TRUNCATE, VALUES, HANDLER-CASE and consing in
;;; the thread's own region touch no table, so the workload runs with no lock
;;; held at all and the two threads genuinely overlap.
;;;
;;; CONTROL BLOCK (offsets from %MVHC-CTL):
;;;   +0x00 N            +0x08 gc-every     +0x10 mode (1 = control)
;;;   +0x18 barrier      +0x20 barrier mutex
;;;   +0x28 t2 started   +0x30 t2 finished  +0x38 t2's %TLS-INSTALL result
;;;   +0x40 t1 barrier timeout              +0x48 t2 barrier timeout
;;;   +0x100 + 0x40*slot  PER-THREAD BLOCK:
;;;     +0x00 iterations              +0x08 multiple-value failures
;;;     +0x10 handler-case failures   +0x18 nested-handler failures
;;;     +0x20 forced collections      +0x28 this thread's segment base
;;;     +0x30 handler-stack depth seen at the end (must be back to entry)

(defun %mvhc-ctl () (let ((p (%thr-page))) (if (zerop p) 0 (+ p #x12000))))
(defun %mvhc-slot (s) (+ (%mvhc-ctl) (+ #x100 (* s #x40))))

(defun %mvhc-three (i)
  "Three values whose extras land in the MV buffer.  The caller checks all
   three, so a clobbered extra is a FAILED COMPARISON and not a crash."
  (values i (+ i 1) (+ i 2)))

(defun %mvhc-raise ()
  "Unwind to the nearest armed handler-case, without allocating and without
   touching a shared table.  %HC-LONGJMP is the same trap the condition system
   uses once it has decided to transfer control (mvm/cl-conditions.lisp); going
   straight to it keeps this test about the FRAME MACHINERY."
  (%hc-longjmp)
  0)

(defun %mvhc-barrier (ctl budget)
  "0 = both threads arrived; 1 = spun out BUDGET alone (a sequential run)."
  (%mutex-lock (+ ctl #x20))
  (%gc-write64 (+ ctl #x18) (+ (%gc-read64 (+ ctl #x18)) 1))
  (%mutex-unlock (+ ctl #x20))
  (let ((i 0) (r 1))
    (loop
      (when (>= (%gc-read64 (+ ctl #x18)) 2) (progn (setq r 0) (return 0)))
      (when (>= i budget) (return 0))
      (setq i (+ i 1)))
    r))

(defun %mvhc-run (slot)
  "THE WORKLOAD.  Both threads run this, with different SLOTs and no lock."
  (let* ((ctl (%mvhc-ctl))
         (me (%mvhc-slot slot))
         (n (%gc-read64 ctl))
         (gcevery (%gc-read64 (+ ctl #x08)))
         (chain (%tl-chain 200))
         (want (%tl-chain-sum chain))
         (depth0 (%hc-depth))
         (i 0)
         (since 0))
    (%gc-write64 (+ me #x28) (%tls-self-base))
    (loop
      (when (>= i n) (return 0))
      ;; ---- multiple values, plain ----
      (multiple-value-bind (q r) (truncate (+ (* i 7) 3) 5)
        (if (= (+ (* q 5) r) (+ (* i 7) 3)) 0 (%tl-bump me #x08)))
      (multiple-value-bind (a b c) (%mvhc-three i)
        (if (= a i)
            (if (= b (+ i 1))
                (if (= c (+ i 2)) 0 (%tl-bump me #x08))
                (%tl-bump me #x08))
            (%tl-bump me #x08)))
      ;; ---- an unwind through handler-case ----
      (if (= (handler-case (%mvhc-raise) (t (c) 8)) 8) 0 (%tl-bump me #x10))
      ;; ---- a NESTED unwind: the inner handler itself unwinds, so the frame
      ;; stack has to be popped twice, in order, on THIS thread ----
      (if (= (handler-case
                 (handler-case (%mvhc-raise) (t (c) (%mvhc-raise)))
               (t (c) 5))
             5)
          0 (%tl-bump me #x18))
      ;; ---- multiple values ACROSS an armed handler-case: the extras have to
      ;; survive the arm/disarm, on this thread, while the other thread is
      ;; doing the same thing ----
      (multiple-value-bind (a b c)
          (handler-case (%mvhc-three i) (t (c) (values 0 0 0)))
        (if (= a i)
            (if (= b (+ i 1))
                (if (= c (+ i 2)) 0 (%tl-bump me #x08))
                (%tl-bump me #x08))
            (%tl-bump me #x08)))
      ;; ---- forced collection of THIS thread's own region ----
      (setq since (+ since 1))
      (if (>= since gcevery)
          (progn
            (setq since 0)
            (%ha-collect-here)
            (%tl-bump me #x20)
            (if (= (%tl-chain-sum chain) want) 0 (%tl-bump me #x08)))
          0)
      (%gc-write64 me (+ i 1))
      (setq i (+ i 1)))
    ;; The DIFFERENCE, not the absolute depth: thread 1 is the main thread and
    ;; is running inside the script loader's own armed handler-cases, so its
    ;; depth at entry is not 0.  What must hold is that every arm this workload
    ;; made was matched by ITS OWN pop.
    (%gc-write64 (+ me #x30) (- (%hc-depth) depth0))
    0))

(defun %mvhc-t2-body ()
  "THREAD 2.  Its own per-thread window FIRST — unless MODE says not to, which
   is the negative control."
  (let ((tb (%ha-thread-block))
        (ctl (%mvhc-ctl)))
    (%gc-write64 (+ ctl #x38)
                 (if (= (%gc-read64 (+ ctl #x10)) 1) 99 (%tls-install 1)))
    (%ha-percpu-init-cpu (%ha-cpu1-percpu-base) 1)
    (set-current-actor 0)
    (set-idle-flag 0)
    (%ha-thread-adopt-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ ctl #x28) 1)
    (%gc-write64 (+ ctl #x48) (%mvhc-barrier ctl 400000000))
    (%mvhc-run 1)
    (%ha-thread-park-region (%gc-read64 (+ tb #x4D0)) (%gc-read64 (+ tb #x340)))
    (%gc-write64 (+ ctl #x30) 1)
    0))

(defun %mvhc-t2-entry ()
  (- (%gc-word-of (fn-addr %mvhc-t2-body) (+ (%ha-base) #x80)) 3))

(defun %mvhc-selftest (mode n gcevery)
  "MODE 0 gives thread 2 its own per-thread window (the shipping path);
   MODE 1 does not, which is this work REMOVED.  Returns the control block's
   raw byte address, or 0."
  (if (zerop (%ha-carve))
      0
      (let ((ctl (%mvhc-ctl))
            (band (%ha-base)))
        (if (zerop ctl)
            0
            (let ((rcb2 (+ band #x200))
                  (rcb3 (+ band #x240))
                  (r0 (%gc-region-0))
                  (budget 400000000)
                  (k 0) (mode0 0) (tid 0))
              (progn
                (%ha-zero ctl (+ ctl #x1000))
                (%ha-zero (%ha-cpu1-percpu-base)
                          (+ (%ha-cpu1-percpu-base) #x4000))
                (%gc-write64 ctl n)
                (%gc-write64 (+ ctl #x08) gcevery)
                (%gc-write64 (+ ctl #x10) mode)
                (%mutex-init (+ ctl #x20))
                (if (zerop (%ha-thread-stack))
                    0
                    (progn
                      (setq mode0 (%ha-percpu-mode))
                      (%ha-percpu-init-cpu (%ha-percpu-base) 0)
                      (setq k (%gc-meta-scale))
                      (%gc-write64 (+ (%ha-thread-block) #x340) k)
                      (%gc-region-init rcb2 *ha-r1-from* *ha-r1-to* *ha-rsize*
                                       (%gc-meta-read (+ r0 #x18) k) k)
                      (%gc-region-init rcb3 *ha-r2-from* *ha-r2-to* *ha-rsize*
                                       (+ *ha-t2-stack* *ha-t2-stack-size*) k)
                      (%gc-write64 (+ (%ha-thread-block) #x4D0) rcb3)
                      (%ha-set-percpu-mode 1)
                      (%gc-region-enter rcb2)
                      (setq tid (%ha-spawn-t2 (%mvhc-t2-entry)))
                      (%gc-write64 (+ ctl #x40) (%mvhc-barrier ctl budget))
                      (%mvhc-run 0)
                      (%gc-write64 (+ ctl #x50) (%ha-join-t2 budget))
                      (%gc-write64 (+ ctl #x58) tid)
                      (%gc-write64 (+ ctl #x60)
                                   (%gc-meta-read (+ rcb2 #x20) k))
                      (%gc-write64 (+ ctl #x68)
                                   (%gc-meta-read (+ rcb3 #x20) k))
                      (%gc-region-enter r0)
                      (%ha-set-percpu-mode mode0)
                      ctl))))))))

;;; ============================================================
;;; MANY THREADS, FROM CLOSURES
;;; ============================================================
;;;
;;; WHAT WAS THERE BEFORE.  ONE spare thread, and %SPAWN-THREAD took a RAW
;;; NATIVE ENTRY ADDRESS: net/hosted-actors-post.lisp had one stack, one thread
;;; block and one TID word, so the image ran exactly two OS threads, and the
;;; thing a thread ran had to be a zero-argument top-level DEFUN whose address
;;; the caller had computed.  A closure could not be a thread body, so a thread
;;; could not carry any state of its own — which is the first thing any real
;;; MAKE-THREAD is asked for.
;;;
;;; WHAT IS HERE NOW.  A table of N threads, each with its own stack, its own
;;; per-thread window (FS), its own per-CPU block (GS), its own TID word, and a
;;; LISP CLOSURE as its body.  %MAKE-NATIVE-THREAD takes a function and returns
;;; a handle; %JOIN-NATIVE-THREAD waits on the kernel's own answer.
;;;
;;; HOW A CLOSURE REACHES A THREAD THAT CANNOT BE PASSED AN ARGUMENT.  The
;;; clone stub enters the child with a bare `call rbx' on a fresh stack: no
;;; argument marshalling, no caller frame, nothing to read from.  So the child
;;; is always the SAME zero-argument trampoline, and what varies is a SLOT
;;; NUMBER it picks up from the table.  Spawning is serialised by a lock and a
;;; handshake — the parent publishes the slot, spawns, and waits for the child
;;; to acknowledge before releasing the lock — so "which slot am I?" has
;;; exactly one answer while any child is reading it.  That is also why the
;;; child ACKs before running the body rather than after: the ack means "I have
;;; read my slot", not "I have finished".
;;;
;;; WHAT A THREAD GETS, AND IN WHICH ORDER, because the order is load-bearing:
;;;   1. its PER-THREAD WINDOW (FS).  First, always — until this returns the
;;;      thread's multiple values and handler frames are the spawner's.
;;;   2. its PER-CPU BLOCK (GS) and CPU id, which is what the collector's
;;;      active-region cell and %THR-CPU read.
;;;   3. its slot in the table marked running, then the closure.
;;;
;;;   4. its GC REGION — but only if the process is in a state where it can
;;;      have one.  See A THREAD THAT CAN CONS below.
;;;
;;; A THREAD THAT CAN CONS.  Until the carve went to N regions this layer gave
;;; 16 threads everything a thread needs EXCEPT a heap, and a body that consed
;;; had nowhere to put it.  Region i of the carve now belongs to thread slot i,
;;; and %MAKE-NATIVE-THREAD initialises it — semispaces, root window, collection
;;; count — before the clone, because the SPAWNER is the one that knows the
;;; stack it just mapped and is the only one that may compute the metadata scale
;;; (see %HA-THREAD-ADOPT-REGION for why a cloned thread must not).
;;;
;;; IT IS CONDITIONAL, AND THE CONDITION IS NOT A PREFERENCE.  A thread can own
;;; an active region only if the active-region cell is PER CPU: with the mode
;;; word at +GC-REGION-PERCPU-ADDR+ off, every thread's %GC-SET-REGION writes
;;; the ONE shared word, so a thread adopting its own region would move the MAIN
;;; thread's heap out from under it mid-allocation.  So the rule is exactly:
;;;
;;;     a thread gets its own region when per-CPU active-region storage is on,
;;;     and gets none when it is off.
;;;
;;; With the mode word off — the state of every fresh ./modus, and the state
;;; test/hosted-many-threads.lisp runs in — this layer behaves precisely as it
;;; did before regions existed.  Turning the mode on is %HA-SET-PERCPU-MODE, an
;;; explicit act, and %THR-THREADS-CAN-CONS-P is the question this file asks.

(defun %thr-max-threads () (%ha-max-regions))

(defun %thr-table () (let ((p (%thr-page))) (if (zerop p) 0 (+ p #x13000))))
(defun %thr-percpu-base (cpu)
  (let ((p (%thr-page))) (if (zerop p) 0 (+ p (+ #x14000 (* cpu #x4000))))))

;; Handshake words (offsets in the thread table):
;;   +0x00 the slot the child about to start should take
;;   +0x08 that child's acknowledgement
;;   +0x10 the spawn lock            +0x18 %GC-WORD-OF scratch
;;   +0x20 threads started ever      +0x28 a shared arrival counter for tests
;;   +0x30 that counter's lock
;;   +0x100 + 0x80*i  PER-THREAD RECORD — 128 BYTES, NOT 64.  It was 64 until a
;;     thread needed a heap, and the four words that describe one did not fit;
;;     writing them anyway put a live region control block on top of the NEXT
;;     record's STATE word, so %THR-FREE-SLOT saw every other slot as taken and
;;     eight threads came back on slots 1,3,5,…,15.  Measured, on the first run
;;     of test/hosted-many-regions.lisp.  Everything reaches a record through
;;     %THR-REC, so widening it is one line — but the reports had to move up.
;;     +0x00 state (0 free, 1 live, 2 done)   +0x08 the CLONE TID WORD (u32)
;;     +0x10 stack base   +0x18 stack size    +0x20 cpu id
;;     +0x28 reached the body  +0x30 returned from the body
;;     +0x38 this thread's segment base
;;     +0x40 the REGION CONTROL BLOCK this thread must adopt, 0 = none
;;     +0x48 the METADATA SCALE the spawner computed for it
;;     +0x50 what its active region answered BEFORE it adopted one
;;     +0x58 its allocation pointer just after adopting
;;   +0xA00 + 0x40*i  PER-THREAD REPORT BLOCK, for tests.  Above the records
;;     (which now end at +0x900) and below the end of the 4 KB table.
(defun %thr-pending-slot () (+ (%thr-table) #x00))
(defun %thr-ack ()          (+ (%thr-table) #x08))
(defun %thr-spawn-lock ()   (+ (%thr-table) #x10))
(defun %thr-scratch-word () (+ (%thr-table) #x18))
(defun %thr-started ()      (+ (%thr-table) #x20))
(defun %thr-arrivals ()     (+ (%thr-table) #x28))
(defun %thr-barrier-lock () (+ (%thr-table) #x30))
(defun %thr-rec (i)         (+ (%thr-table) (+ #x100 (* i #x80))))

(defvar *thr-funs* nil
  "The thread bodies, by slot.  A vector and not a table: a fresh thread reads
   its own entry and nothing writes the vector while a child is starting (the
   spawn lock and the handshake see to that), so this needs no locking of its
   own — which matters, because a starting thread has no GC region and must not
   go anywhere near the shared runtime tables.")

(defvar *thr-stack-bytes* 262144)

(defun %thr-funs ()
  (if (null *thr-funs*) (setq *thr-funs* (make-array 16)) 0)
  *thr-funs*)

(defun %thr-threads-can-cons-p ()
  "1 when a spawned thread may be given a GC region of its own, 0 when it may
   not.  The condition is that the active-region cell is PER CPU and that the
   carve produced regions at all; see A THREAD THAT CAN CONS above."
  (if (= (mem-ref #x10000FF8 :u32) 0)
      0
      (if (zerop (%ha-nregions)) 0 1)))

(defun %thr-prepare-region (slot stack-top k)
  "Initialise thread slot SLOT's region: its own semispace pair out of the
   carve, a root window bounded above by STACK-TOP — the top of the stack this
   thread is about to be cloned onto, so the window the collector scans is that
   thread's frames and nothing else — and a collection count of its own,
   starting at zero.  Returns the control block, or 0 if SLOT has no region.

   K IS THE SPAWNER'S metadata scale, passed in for the reason given at
   %HA-THREAD-ADOPT-REGION: %GC-META-SCALE asks whether the live allocation
   pointer falls inside the active region's from-space, which is only a
   meaningful question on a thread whose cell and R12 belong together."
  (if (>= slot (%ha-nregions))
      0
      (let ((rcb (%ha-rcb slot)))
        (%gc-region-init rcb (%ha-region-from slot) (%ha-region-to slot)
                         *ha-rsize* stack-top k)
        rcb)))

(defun %thr-trampoline ()
  "EVERY thread starts here.  Zero arguments, because the clone stub enters it
   with a bare `call rbx' — see the handshake above for how it learns which
   thread it is."
  (let* ((tt (%thr-table))
         (slot (%gc-read64 tt))
         (rec (%thr-rec slot)))
    ;; 1. ITS OWN WINDOW, BEFORE ANYTHING THAT RETURNS A VALUE OR ARMS A
    ;;    HANDLER.  It was cloned without CLONE_SETTLS, so right now its
    ;;    multiple values and handler frames are still the spawner's.
    (%tls-install slot)
    (%gc-write64 (+ rec #x38) (%tls-self-base))
    ;; 2. its own per-CPU block and CPU id.
    (%ha-percpu-init-cpu (%thr-percpu-base slot) slot)
    (set-current-actor 0)
    (set-idle-flag 0)
    (%gc-write64 (+ rec #x20) slot)
    ;; 3. ITS OWN HEAP, if the spawner prepared one.  AFTER the per-CPU block
    ;;    and not before: adopting a region is a write to THIS CPU's
    ;;    active-region cell, and which cell that is comes from the CPU id
    ;;    stamped one line above.  Adopting before the stamp would write CPU 0's
    ;;    cell — the main thread's.
    ;;
    ;;    The BEFORE reading is recorded rather than discarded: with per-CPU
    ;;    cells working, a thread that has not adopted anything must answer
    ;;    REGION 0, and a thread reading its spawner's region here would mean
    ;;    the indexing is not working at all.
    (let ((rcb (%gc-read64 (+ rec #x40))))
      (if (zerop rcb)
          0
          (progn
            (%gc-write64 (+ rec #x50) (%gc-region))
            (%ha-thread-adopt-region rcb (%gc-read64 (+ rec #x48)))
            (%gc-write64 (+ rec #x58) (get-alloc-ptr)))))
    ;; 4. tell the spawner the slot has been read; it may now start the next.
    (%gc-write64 (+ rec #x28) 1)
    (%gc-write64 (%thr-ack) 1)
    (funcall (aref (%thr-funs) slot))
    ;; PARK THE ALLOCATION FRONTIER ON THE WAY OUT.  A region's +0x30 is where
    ;; its live heap ends as far as anything outside this thread is concerned;
    ;; until it is parked it still holds the from-space START, and every
    ;; checksum and every foreign-reference sweep over "this region's live heap"
    ;; would run over an empty range and answer 0.
    (let ((rcb (%gc-read64 (+ rec #x40))))
      (if (zerop rcb) 0 (%ha-thread-park-region rcb (%gc-read64 (+ rec #x48)))))
    (%gc-write64 (+ rec #x30) 1)
    (%gc-write64 (+ rec #x00) 2)
    0))

(defun %thr-trampoline-entry ()
  (- (%gc-word-of (fn-addr %thr-trampoline) (%thr-scratch-word)) 3))

(defun %thr-free-slot ()
  "The lowest slot that has never been used or whose thread has exited.  Slot
   0 is the MAIN thread's — it has a window and a per-CPU block of its own
   identity and is not startable — so slots run from 1."
  (let ((i 1) (r -1))
    (loop
      (when (>= i (%thr-max-threads)) (return 0))
      (when (zerop (%gc-read64 (+ (%thr-rec i) #x00)))
        (progn (setq r i) (return 0)))
      (setq i (+ i 1)))
    r))

(defun %make-native-thread (fn)
  "Start an OS thread running the closure FN.  Returns its slot — the handle
   %JOIN-NATIVE-THREAD and %NATIVE-THREAD-ALIVE-P take — or a negative number:
   -1 no free slot, -2 the thread page could not be mapped, -3 no stack,
   -4 clone failed, -5 the child never acknowledged its slot.

   FN TAKES NO ARGUMENTS, which is not a restriction on generality: a closure
   carries whatever it captured, and that is how a thread gets its state."
  (let ((tt (%thr-table)))
    (if (zerop tt)
        -2
        (progn
          (%thr-funs)
          (spin-lock (%thr-spawn-lock))
          (let ((slot (%thr-free-slot)))
            (if (< slot 0)
                (progn (spin-unlock (%thr-spawn-lock)) -1)
                (let* ((rec (%thr-rec slot))
                       (stk (%mmap-shared-page *thr-stack-bytes*)))
                  (if (< stk 4096)
                      (progn (spin-unlock (%thr-spawn-lock)) -3)
                      (progn
                        (aset (%thr-funs) slot fn)
                        (%gc-write64 (+ rec #x00) 1)
                        (%gc-write64 (+ rec #x08) 0)
                        (%gc-write64 (+ rec #x10) stk)
                        (%gc-write64 (+ rec #x18) *thr-stack-bytes*)
                        (%gc-write64 (+ rec #x28) 0)
                        (%gc-write64 (+ rec #x30) 0)
                        (%gc-write64 (+ rec #x50) 0)
                        (%gc-write64 (+ rec #x58) 0)
                        ;; ITS HEAP, PREPARED BY THE SPAWNER, BEFORE THE CLONE.
                        ;; The root window's top is the top of the stack just
                        ;; mapped, which only this side knows; and the metadata
                        ;; scale is computed HERE, where the cell and the
                        ;; allocation pointer still belong together.
                        (if (zerop (%thr-threads-can-cons-p))
                            (progn (%gc-write64 (+ rec #x40) 0)
                                   (%gc-write64 (+ rec #x48) 0))
                            (let ((k (%gc-meta-scale)))
                              (%gc-write64 (+ rec #x48) k)
                              (%gc-write64 (+ rec #x40)
                                           (%thr-prepare-region
                                            slot (+ stk *thr-stack-bytes*) k))))
                        (%gc-write64 (%thr-pending-slot) slot)
                        (%gc-write64 (%thr-ack) 0)
                        (let ((tid (%spawn-thread (%thr-trampoline-entry)
                                                  (+ stk *thr-stack-bytes*)
                                                  (+ rec #x08))))
                          (if (< tid 1)
                              (progn (%gc-write64 (+ rec #x00) 0)
                                     (spin-unlock (%thr-spawn-lock))
                                     -4)
                              ;; WAIT FOR THE ACK BEFORE RELEASING THE LOCK.
                              ;; The pending-slot word has one reader and it
                              ;; has not necessarily run yet.
                              (let ((i 0) (ok 0))
                                (loop
                                  (when (= (%gc-read64 (%thr-ack)) 1)
                                    (progn (setq ok 1) (return 0)))
                                  (when (>= i 2000000000) (return 0))
                                  (setq i (+ i 1)))
                                (%gc-write64 (%thr-started)
                                             (+ (%gc-read64 (%thr-started)) 1))
                                (spin-unlock (%thr-spawn-lock))
                                (if (= ok 1) slot -5))))))))))))) 

(defun %native-thread-alive-p (slot)
  "1 while the KERNEL still has this thread, 0 once it is gone.  The TID word
   is cleared by the kernel itself (CLONE_CHILD_CLEARTID), so this is not a
   flag the thread set about itself."
  (if (zerop (mem-ref (+ (%thr-rec slot) #x08) :u32)) 0 1))

(defun %join-native-thread (slot budget)
  "0 once the thread is gone, 1 if BUDGET ran out first.  Frees the slot on
   success so a later %MAKE-NATIVE-THREAD can reuse it."
  (let ((i 0) (r 1))
    (loop
      (when (zerop (mem-ref (+ (%thr-rec slot) #x08) :u32))
        (progn (setq r 0) (return 0)))
      (when (>= i budget) (return 0))
      (setq i (+ i 1)))
    (if (zerop r) (%gc-write64 (+ (%thr-rec slot) #x00) 0) 0)
    r))

(defun %thr-arrive-and-wait (want budget)
  "A barrier with a SPIN BUDGET, for proving simultaneity: 0 = every one of
   WANT threads was inside it at once, 1 = this thread spun out its budget
   alone, which is what a sequential run scores.

   THE ARRIVAL IS UNDER A LOCK.  A read-modify-write of one counter by eight
   threads loses increments, and a lost increment here is not a wrong number
   but a barrier that never opens — i.e. a hang dressed as a timeout."
  (spin-lock (%thr-barrier-lock))
  (%gc-write64 (%thr-arrivals) (+ (%gc-read64 (%thr-arrivals)) 1))
  (spin-unlock (%thr-barrier-lock))
  (let ((i 0) (r 1))
    (loop
      (when (>= (%gc-read64 (%thr-arrivals)) want) (progn (setq r 0) (return 0)))
      (when (>= i budget) (return 0))
      (setq i (+ i 1)))
    r))

(defun %thr-reset-table ()
  "Zero the whole table — the handshake words, every record and the arrival
   counter.  Called by a driver before it starts a batch."
  (let ((tt (%thr-table)))
    (if (zerop tt) 0 (progn (%ha-zero tt (+ tt #x1000)) 1))))

(defun %thr-my-cpu-raw ()
  "This thread's CPU id straight out of its per-CPU block.  UNGUARDED — only a
   thread that has installed a per-CPU block may call it, because PERCPU-REF is
   a GS-relative load and GS base 0 makes it read absolute address 16.  %THR-CPU
   is the guarded one; this exists so a worker can report its OWN id without
   the process-wide mode word (which the collector also reads) being touched."
  (percpu-ref 16))

(defun %thr-make-counter (k out want budget)
  "A THREAD BODY AS A CLOSURE, capturing K (this worker's own number) and OUT
   (where it reports).  Returned by a function so the capture is real: eight
   calls make eight closures over eight different values, and each thread's
   arithmetic answer is derived from ITS OWN K, so a thread running somebody
   else's body is a wrong number rather than an indistinguishable success.

   IT DOES NOT ALLOCATE, and that is a boundary rather than a preference: a
   thread has no GC region of its own yet (see MANY THREADS, FROM CLOSURES).
   Fixnum arithmetic, raw memory and syscalls only.

   REPORT BLOCK at OUT:
     +0x00 barrier result (0 = all WANT threads were inside it at once)
     +0x08 the K it captured   +0x10 K*ITERS, summed the long way
     +0x18 its gettid          +0x20 its CPU id, read through its own GS
     +0x28 its segment base, read through its own FS
     +0x30 99 when the body ran to the end"
  (lambda ()
    (%gc-write64 (+ out #x00) (%thr-arrive-and-wait want budget))
    (%gc-write64 (+ out #x08) k)
    (let ((sum 0) (i 0))
      (loop
        (when (>= i 200000) (return 0))
        (setq sum (+ sum k))
        (setq i (+ i 1)))
      (%gc-write64 (+ out #x10) sum))
    (%gc-write64 (+ out #x18) (syscall3 186 0 0 0))
    (%gc-write64 (+ out #x20) (%thr-my-cpu-raw))
    (%gc-write64 (+ out #x28) (%tls-self-base))
    (%gc-write64 (+ out #x30) 99)
    0))

(defun %thr-report (i) (+ (%thr-table) (+ #xA00 (* i #x40))))

;;; ============================================================
;;; N THREADS, N HEAPS — the acceptance worker
;;; ============================================================
;;;
;;; The report blocks are in the BAND and not in the thread page because they
;;; must outlive nothing and collide with nothing: the thread page's scratch is
;;; already spoken for by the two-thread selftests, and the band has 23 KB free
;;; above the region control blocks.  128 bytes per slot at +0xB000.

(defun %thr-region-report (i) (+ (%ha-base) (+ #xB000 (* i #x80))))

(defun %thr-rec-region (i)  (%gc-read64 (+ (%thr-rec i) #x40)))
(defun %thr-rec-before (i)  (%gc-read64 (+ (%thr-rec i) #x50)))
(defun %thr-rec-alloc (i)   (%gc-read64 (+ (%thr-rec i) #x58)))

(defun %thr-make-heap-worker (out nlinks ngc want budget k)
  "A THREAD BODY THAT ALLOCATES AND COLLECTS ITS OWN HEAP.  Holds a live chain
   of NLINKS conses across NGC forced collections of its own region, building a
   fresh chain's worth of garbage between each so every collection has
   something to reclaim and something to move.

   IT IS A CLOSURE, so N of these are N independent workers over N different
   report blocks — the same construction %THR-MAKE-COUNTER uses, and for the
   same reason: a thread running somebody else's body must produce a WRONG
   NUMBER rather than an indistinguishable success.

   K IS THE SPAWNER'S metadata scale.  A worker reads its own collection count
   with %GC-META-READ and not %GC-COUNT, because %GC-COUNT goes through
   (mem-ref … :u64), which HALVES — right on the targets that store the eight
   control-block fields SHL'd by one, wrong on hosted x86-64, which stores them
   raw.  The count that comes back here is therefore the count the collector
   actually wrote.

   NO FORMAT, NO INTERN, NO SYMBOL LITERAL.  Same restriction every threaded
   selftest before this one carries: the runtime's shared tables are
   unsynchronised unless %RT-THREADS-ON has been called, and this test is about
   heaps, not about that lock.  CONS is not on that list — conses go in this
   thread's own region, which is the whole point.

   REPORT BLOCK at OUT:
     +0x00 barrier result (0 = all WANT threads were inside it at once)
     +0x08 its active region      +0x10 its active-region CELL address
     +0x18 its CPU id             +0x20 allocation pointer on entry
     +0x28 its own region's collection count BEFORE
     +0x30 ... and AFTER          +0x38 chain walks that failed mid-run
     +0x40 the final chain check (0 = broken, else 1 + the sum of its cars)
     +0x48 allocation pointer at the end
     +0x50 its gettid             +0x58 its segment base (FS)
     +0x60 99 when the body ran to the end"
  (lambda ()
    (%gc-write64 (+ out #x00) (%thr-arrive-and-wait want budget))
    (%gc-write64 (+ out #x08) (%gc-region))
    (%gc-write64 (+ out #x10) (%gc-region-cell))
    (%gc-write64 (+ out #x18) (%thr-my-cpu-raw))
    (%gc-write64 (+ out #x20) (get-alloc-ptr))
    (%gc-write64 (+ out #x28) (%gc-meta-read (+ (%gc-region) #x20) k))
    (let ((chain (%gc-chain-build nlinks))
          (bad 0)
          (i 0))
      (loop
        (when (>= i ngc) (return 0))
        (%ha-collect-here)
        (if (zerop (%gc-chain-check chain nlinks)) (setq bad (+ bad 1)) 0)
        (%gc-chain-build nlinks)
        (setq i (+ i 1)))
      (%gc-write64 (+ out #x38) bad)
      (%gc-write64 (+ out #x40) (%gc-chain-check chain nlinks)))
    (%gc-write64 (+ out #x30) (%gc-meta-read (+ (%gc-region) #x20) k))
    (%gc-write64 (+ out #x48) (get-alloc-ptr))
    (%gc-write64 (+ out #x50) (syscall3 186 0 0 0))
    (%gc-write64 (+ out #x58) (%tls-self-base))
    (%gc-write64 (+ out #x60) 99)
    0))

;;; ------------------------------------------------------------
;;; THE ACCEPTANCE DRIVER — N THREADS, N REGIONS, ONE AUDIT
;;; ------------------------------------------------------------
;;;
;;; WHAT WOULD MAKE THIS A LIE, AND WHAT STOPS IT.
;;;
;;;   "Each thread collected" is a COUNT ON ITS OWN CONTROL BLOCK, read with
;;;   %GC-META-READ in the target's own metadata scale, and it has to have risen
;;;   by at least the number of collections the worker was told to force.  A
;;;   shared counter would rise N times as fast on one block and not at all on
;;;   the others; N blocks each rising by NGC cannot be produced that way.
;;;
;;;   "Its data survived" is a WALK, not a flag: the chain's links must still
;;;   carry NLINKS-1 … 0 in order after every collection, and the walk is
;;;   repeated between every pair of them, not only at the end.
;;;
;;;   "Another thread's region is untouched" is measured with the workers GONE.
;;;   While they are running their checksums are supposed to move.  After the
;;;   join the DRIVER collects ITS OWN region twice, and every worker's heap and
;;;   every worker's control block must be bit-for-bit identical across those
;;;   two collections.  THE CHECKSUM IS ASSERTED NON-ZERO FIRST — a checksum
;;;   over an empty range answers 0 for any two runs, so "unchanged" would
;;;   otherwise be free.  (What makes the range non-empty is the trampoline
;;;   parking the thread's allocation frontier into +0x30 on the way out; before
;;;   that existed this check could only ever have answered 0 -> 0.)
;;;
;;;   "The regions are isolated" is %GC-COUNT-FOREIGN-REFS over EVERY ORDERED
;;;   PAIR — each worker's live heap swept for pointers into every other
;;;   worker's from-space AND to-space — WITH A POSITIVE CONTROL that plants one
;;;   cons-tagged pointer in a zeroed window and requires the answer 1, and asks
;;;   the same window about a different region and requires 0.
;;;
;;; RESULT BLOCK at band +0xC000 (band +0xC000..+0x10000 is otherwise unused):
;;;   +0x00 threads asked for      +0x08 regions the carve produced
;;;   +0x10 spawn failures         +0x18 join timeouts
;;;   +0x20 region 0's collection count before … +0x28 … and after
;;;   +0x30 region-alignment violations   +0x38 last violation mask
;;;   +0x40 the driver's own region      +0x48 its count before its two forced
;;;   +0x50 … and after                  +0x58 foreign refs, all ordered pairs
;;;   +0x60 positive control (must be 1) +0x68 same window, other region (0)
;;;   +0x70 workers whose HEAP checksum moved across the driver's collections
;;;   +0x78 workers whose CONTROL BLOCK checksum moved
;;;   +0x80 workers whose heap checksum was ZERO (an unassertable check)
;;;   +0x88 workers whose active region was not their own slot's block
;;;   +0x90 workers whose active-region CELL was not their own slot's entry
;;;   +0x98 the driver's chain check after its own two collections
;;;   +0xA0 the metadata scale     +0xA8 collectors that entered concurrently
;;;   +0xB0 collectors still inside at the end (must be 0)
;;;   +0xB8 the driver's allocation pointer, restored
;;;   +0xC0 the per-CPU mode word as this call found it
;;;   +0xC8 distinct worker regions seen   +0xD0 distinct worker gettids
;;;   +0xD8 distinct final chain answers   +0xE0 distinct heap checksums
;;;   +0x100 + 0x20*i  worker i: heap sum before / after, block sum before /after
;;;   +0x400 + 8*i     worker i's spawn handle

(defun %thr-nr-slot (res i) (+ res (+ #x100 (* i #x20))))
(defun %thr-nr-handle (res i) (+ res (+ #x400 (* i 8))))

(defun %thr-nr-distinct (res n off)
  "How many DISTINCT values the N worker report blocks carry at offset OFF.
   Pairwise, because N is 8 and a sort would allocate."
  (let ((i 0) (d 0))
    (loop
      (when (>= i n) (return 0))
      (let ((j 0) (new 1))
        (loop
          (when (>= j i) (return 0))
          (when (= (%gc-read64 (+ (%thr-region-report i) off))
                   (%gc-read64 (+ (%thr-region-report j) off)))
            (setq new 0))
          (setq j (+ j 1)))
        (setq d (+ d new)))
      (setq i (+ i 1)))
    d))

(defun %thr-nr-distinct-slots (res n)
  "How many DISTINCT heap checksums the N per-worker evidence slots carry."
  (let ((i 0) (d 0))
    (loop
      (when (>= i n) (return 0))
      (let ((j 0) (new 1))
        (loop
          (when (>= j i) (return 0))
          (when (= (%gc-read64 (%thr-nr-slot res i))
                   (%gc-read64 (%thr-nr-slot res j)))
            (setq new 0))
          (setq j (+ j 1)))
        (setq d (+ d new)))
      (setq i (+ i 1)))
    d))

(defun %thr-nr-pairs (n k)
  "Foreign references summed over EVERY ORDERED PAIR of worker regions: worker
   i's live heap [from, parked-alloc) swept for pointers into worker j's
   from-space and into worker j's to-space, for every j other than i.  Worker i
   occupies slot i+1, because slot 0 is the main thread's."
  (let ((i 0) (tot 0))
    (loop
      (when (>= i n) (return 0))
      (let* ((a (%ha-rcb (+ i 1)))
             (alo (%gc-meta-read a k))
             (ahi (%gc-meta-read (+ a #x30) k))
             (j 0))
        (loop
          (when (>= j n) (return 0))
          (if (= i j)
              0
              (let ((b (%ha-rcb (+ j 1))))
                (setq tot (+ tot (%gc-count-foreign-refs
                                  alo ahi (%gc-meta-read b k) *ha-rsize*)))
                (setq tot (+ tot (%gc-count-foreign-refs
                                  alo ahi (%gc-meta-read (+ b #x08) k)
                                  *ha-rsize*)))))
          (setq j (+ j 1))))
      (setq i (+ i 1)))
    tot))

(defun %thr-nr-snapshot (res n k off)
  "Checksum every worker's live heap and its control block into the per-worker
   evidence slots at OFF (0 = the before pair, 8 = the after pair)."
  (let ((i 0))
    (loop
      (when (>= i n) (return 0))
      (let ((a (%ha-rcb (+ i 1)))
            (s (%thr-nr-slot res i)))
        (%gc-write64 (+ s off)
                     (%gc-sum-range (%gc-meta-read a k)
                                    (%gc-meta-read (+ a #x30) k)))
        (%gc-write64 (+ s (+ #x10 off)) (%gc-sum-range a (+ a #x40))))
      (setq i (+ i 1)))
    0))

(defun %thr-nregion-selftest (n nlinks ngc budget)
  "N THREADS, N HEAPS.  Start N worker threads — slots 1..N, regions 1..N —
   each holding a live chain of NLINKS conses across NGC forced collections OF
   ITS OWN REGION, then audit isolation with the workers gone.  Returns the
   result block's raw byte address, or 0 if the band could not be carved.

   PARTIAL, AND SAID HERE RATHER THAN DISCOVERED: this leaves the per-CPU
   active-region mode word ON if it was off when it started only for the length
   of the call — it is restored before returning — and it does NOT exercise the
   runtime-table lock (%RT-THREADS-ON).  The workers do arithmetic, raw memory
   and CONS.  No FORMAT, no INTERN, no symbol literal."
  (if (zerop (%ha-carve))
      0
      (if (zerop (%thr-table))
          0
          (let* ((res (+ (%ha-base) #xC000))
                 (r0 (%gc-region-0))
                 (mode0 (%ha-percpu-mode))
                 (fails 0)
                 (touts 0)
                 (i 0))
            (%ha-zero res (+ res #x800))
            (%thr-reset-table)
            (let ((z 0))
              (loop
                (when (>= z (%ha-max-regions)) (return 0))
                (%ha-zero (%thr-region-report z) (+ (%thr-region-report z) #x80))
                (setq z (+ z 1))))
            (%gc-region-align-reset)
            (%ha-gc-conc-reset)
            (%gc-write64 (+ res #x00) n)
            (%gc-write64 (+ res #x08) (%ha-nregions))
            (%gc-write64 (+ res #xC0) mode0)
            (%gc-write64 (+ res #x20) (%gc-meta-read (+ r0 #x20) (%gc-meta-scale)))
            ;; ---- THIS thread: a real per-CPU block, stamped CPU 0, and only
            ;;      then the mode word.  The order is the one %HA-REGIONS-PERCPU-
            ;;      SELFTEST establishes: a per-CPU cell read with no per-CPU
            ;;      block is a GS-relative load at absolute address 16.
            (%ha-percpu-init-cpu (%ha-percpu-base) 0)
            (%ha-set-percpu-mode 1)
            (let ((k (%gc-meta-scale)))
              (%gc-write64 (+ res #xA0) k)
              ;; ---- spawn ----
              ;; The driver stays in REGION 0 for the whole of the spawn and the
              ;; join.  It must: %MAKE-NATIVE-THREAD stores the closure in a
              ;; vector that lives in region 0, and a closure allocated in the
              ;; driver's OWN carved region would be reachable only through that
              ;; vector — which no collector of the driver's region scans, since
              ;; the vector is not in that region's from-space.  It would be
              ;; garbage the first time the driver collected.
              (setq i 0)
              (loop
                (when (>= i n) (return 0))
                ;; WORKER I GETS A CHAIN OF ITS OWN LENGTH, NLINKS + 7i.  Not
                ;; decoration: with every worker holding the SAME chain, every
                ;; worker's heap checksum comes out the same number, because
                ;; %GC-SUM-RANGE folds to 24 bits and the regions are 16 MB —
                ;; exactly 2^24 — apart, so the only thing that differs between
                ;; two regions' live heaps is invisible to it.  "Region B is
                ;; unchanged" would then also hold if region B held region C's
                ;; data.  Distinct lengths make the eight checksums distinct,
                ;; and the test asserts that they are.
                (let ((h (%make-native-thread
                          (%thr-make-heap-worker (%thr-region-report (+ i 1))
                                                 (+ nlinks (* i 7))
                                                 ngc n budget k))))
                  (%gc-write64 (%thr-nr-handle res i) h)
                  (if (< h 0) (setq fails (+ fails 1)) 0))
                (setq i (+ i 1)))
              (%gc-write64 (+ res #x10) fails)
              ;; ---- join ----
              (setq i 0)
              (loop
                (when (>= i n) (return 0))
                (let ((h (%gc-read64 (%thr-nr-handle res i))))
                  (if (< h 0)
                      0
                      (if (zerop (%join-native-thread h budget))
                          0
                          (setq touts (+ touts 1)))))
                (setq i (+ i 1)))
              (%gc-write64 (+ res #x18) touts)
              ;; ---- did each worker land in ITS OWN region, on ITS OWN cell? --
              (setq i 0)
              (let ((badr 0) (badc 0) (zeroes 0))
                (loop
                  (when (>= i n) (return 0))
                  (let ((rp (%thr-region-report (+ i 1))))
                    (if (= (%gc-read64 (+ rp #x08)) (%ha-rcb (+ i 1)))
                        0 (setq badr (+ badr 1)))
                    (if (= (%gc-read64 (+ rp #x10)) (+ #x10000F08 (* (+ i 1) 8)))
                        0 (setq badc (+ badc 1))))
                  (setq i (+ i 1)))
                (%gc-write64 (+ res #x88) badr)
                (%gc-write64 (+ res #x90) badc)
                (%gc-write64 (+ res #xC8) (%thr-nr-distinct res n #x08))
                (%gc-write64 (+ res #xD0) (%thr-nr-distinct res n #x50))
                (%gc-write64 (+ res #xD8) (%thr-nr-distinct res n #x40))
                ;; ---- isolation, every ordered pair, before anything else
                ;;      moves: the workers are gone and their heaps are parked.
                (%gc-write64 (+ res #x58) (%thr-nr-pairs n k))
                ;; POSITIVE CONTROL, EXACT.  A zeroed 8-word window holding
                ;; exactly one cons-tagged pointer into worker 1's from-space
                ;; must count 1 — and 0 when asked about worker 2's.
                (let ((ctl (+ (%ha-base) #x300)))
                  (%ha-zero ctl (+ ctl #x40))
                  (%gc-write64 (+ ctl 24) (+ (%gc-meta-read (%ha-rcb 1) k) 1))
                  (%gc-write64 (+ res #x60)
                               (%gc-count-foreign-refs ctl (+ ctl #x40)
                                                       (%gc-meta-read (%ha-rcb 1) k)
                                                       *ha-rsize*))
                  (%gc-write64 (+ res #x68)
                               (%gc-count-foreign-refs ctl (+ ctl #x40)
                                                       (%gc-meta-read (%ha-rcb 2) k)
                                                       *ha-rsize*)))
                ;; ---- snapshot every worker's heap and block ----
                (%thr-nr-snapshot res n k 0)
                ;; ---- THE DRIVER COLLECTS ITS OWN REGION, TWICE ----
                ;; Slot 0's region, whose root window is bounded by the PROCESS
                ;; stack — this thread's — because this thread is the one
                ;; running on it.  %GC-REGION-ENTER and not a bare cell write:
                ;; the park half is correct here, this thread is region 0's
                ;; legitimate owner, and the matching enter at the end puts its
                ;; allocation frontier back.
                (%gc-region-init (%ha-rcb 0) (%ha-region-from 0) (%ha-region-to 0)
                                 *ha-rsize* (%gc-meta-read (+ r0 #x18) k) k)
                (%gc-write64 (+ res #x40) (%ha-rcb 0))
                (%gc-region-enter (%ha-rcb 0))
                (%gc-write64 (+ res #x48) (%gc-meta-read (+ (%ha-rcb 0) #x20) k))
                (let ((chain (%gc-chain-build nlinks)))
                  (%ha-collect-here)
                  (%gc-chain-build nlinks)
                  (%ha-collect-here)
                  (%gc-write64 (+ res #x98) (%gc-chain-check chain nlinks)))
                (%gc-write64 (+ res #x50) (%gc-meta-read (+ (%ha-rcb 0) #x20) k))
                (%thr-nr-snapshot res n k 8)
                (%gc-region-enter r0)
                ;; ---- and now compare, back in region 0 ----
                (setq i 0)
                (let ((movedh 0) (movedb 0))
                  (loop
                    (when (>= i n) (return 0))
                    (let ((s (%thr-nr-slot res i)))
                      (if (= (%gc-read64 s) (%gc-read64 (+ s 8)))
                          0 (setq movedh (+ movedh 1)))
                      (if (= (%gc-read64 (+ s #x10)) (%gc-read64 (+ s #x18)))
                          0 (setq movedb (+ movedb 1)))
                      (if (zerop (%gc-read64 s)) (setq zeroes (+ zeroes 1)) 0))
                    (setq i (+ i 1)))
                  (%gc-write64 (+ res #x70) movedh)
                  (%gc-write64 (+ res #x78) movedb)
                  (%gc-write64 (+ res #x80) zeroes)
                  (%gc-write64 (+ res #xE0) (%thr-nr-distinct-slots res n))))
              (%gc-write64 (+ res #x28) (%gc-meta-read (+ r0 #x20) k))
              (%gc-write64 (+ res #x30) (%gc-region-align-violations))
              (%gc-write64 (+ res #x38) (%gc-region-align-last))
              (%gc-write64 (+ res #xA8) (%ha-gc-conc-witness))
              (%gc-write64 (+ res #xB0) (%ha-gc-conc-cur))
              (%gc-write64 (+ res #xB8) (get-alloc-ptr)))
            (%ha-set-percpu-mode mode0)
            res))))

;;; ============================================================
;;; WHAT A PORTABLE THREAD API NEEDS THAT THE PRIMITIVES ABOVE DO NOT HAVE
;;; ============================================================
;;;
;;; Two things, and both exist because the shim layer (net/sb-thread-shim.lisp)
;;; is EVALUATED AT RUNTIME while these must be AOT: a runtime-compiled function
;;; is fine on any thread now that the JIT marks per-thread-window accesses, but
;;; a CELL ALLOCATOR handing out addresses that a FUTEX will be told to watch
;;; must not itself be able to move, and a raw spin lock protecting a bump
;;; pointer is exactly the kind of code that belongs in the image.
;;;
;;;   1. A TIMED CONDITION WAIT.  %COND-WAIT parks forever; sb-thread's
;;;      CONDITION-WAIT takes :TIMEOUT and glass's sender/reader handoff passes
;;;      1/60 of a second (src/rfb.lisp:744-753).  Without a timeout that handoff
;;;      is a hang rather than a frame.
;;;   2. A CELL ARENA.  A mutex is ONE MACHINE WORD AT A FIXED ADDRESS.  It
;;;      cannot live inside a Lisp object: this collector COPIES, so the address
;;;      a parked thread handed the kernel would be stale the moment the object
;;;      moved, and FUTEX_WAKE would be delivered to an address nobody is
;;;      watching.  So mutex/condvar/semaphore words come out of a raw arena and
;;;      the Lisp object holds the ADDRESS.
;;;
;;;      THE ARENA IS NEVER RECLAIMED, AND THAT IS A PARTIAL IMPLEMENTATION.  A
;;;      cell is handed out and never returned, because knowing a cell is dead
;;;      means knowing no thread is parked on it, and nothing here knows that.
;;;      With 4 MB of cells that is 65536 mutexes for the life of the process.
;;;      A program that makes a mutex per connection forever will exhaust it, and
;;;      %SYNC-CELL then answers 0 rather than handing out an address twice —
;;;      %SYNC-CELLS-EXHAUSTED counts the refusals so the failure is visible.

(defun %cond-ts (ms)
  "This CPU's 16-byte condition-wait timespec, armed at MS milliseconds.  0 if
   the thread page could not be mapped, which %FUTEX-WAIT-TO reads as `no
   timeout'.  It is the SCHEDULER block's free tail at +0x68 and not %THR-TS,
   whose 64 bytes are entirely spoken for (req, rem, clock scratch, and the
   mutex's own 20 ms safety-net timeout at +0x30)."
  (let ((b (%thr-sched-block (%thr-cpu))))
    (if (zerop b)
        0
        (let ((p (+ b #x68))
              (sec (truncate ms 1000)))
          (%gc-write64 p sec)
          (%gc-write64 (+ p 8) (* (- ms (* sec 1000)) 1000000))
          p))))

(defun %cond-wait-ms (cv mtx ms)
  "%COND-WAIT with a deadline.  Returns 0 if it was woken (or woke spuriously)
   and 1 if the wait expired.  MS of 0 or less means no timeout at all, i.e.
   exactly %COND-WAIT.  MTX must be held on entry and is held on return.

   SPURIOUS WAKEUPS ARE STILL PERMITTED, so 0 does not mean the predicate is
   true; call this in a loop around the predicate, the way every caller of
   %COND-WAIT already does."
  (let ((seq (%gc-read64 cv))
        (ts (if (> ms 0) (%cond-ts ms) 0)))
    (%mutex-unlock mtx)
    (let ((r (%futex-wait-to cv seq ts)))
      (%mutex-lock mtx)
      (if (= r -110) 1 0))))

;;; THE ARENA.  Control words in the band at +0xC800 (band +0xC000..+0xC800 is
;;; the N-regions selftest's result block; +0xC800..+0xD000 is otherwise unused):
;;;   +0x00 the bump lock   +0x08 arena base   +0x10 next free
;;;   +0x18 arena limit     +0x20 cells handed out   +0x28 refusals
(defun %sync-cell-ctl () (+ (%ha-base) #xC800))
(defun %sync-cell-size () 64)
(defun %sync-arena-bytes () 4194304)

(defun %sync-cells-handed-out () (%gc-read64 (+ (%sync-cell-ctl) #x20)))
(defun %sync-cells-exhausted () (%gc-read64 (+ (%sync-cell-ctl) #x28)))

(defun %sync-cell ()
  "A 64-byte ZEROED cell at a raw address that will never move, or 0 if the
   arena could not be mapped or is exhausted.  Cell layout, by convention of
   the callers rather than of this function:
     +0x00 futex mutex word   +0x08 condvar sequence word
     +0x10 owner (cpu id + 1)  +0x18 recursion depth
     +0x20 semaphore count     +0x28 .. +0x3F spare"
  (let ((ctl (%sync-cell-ctl)))
    (spin-lock ctl)
    (let ((base (%gc-read64 (+ ctl #x08))))
      (if (zerop base)
          (let ((m (%mmap-shared-page (%sync-arena-bytes))))
            (if (< m 4096)
                (progn (spin-unlock ctl) 0)
                (progn (%gc-write64 (+ ctl #x08) m)
                       (%gc-write64 (+ ctl #x10) m)
                       (%gc-write64 (+ ctl #x18) (+ m (%sync-arena-bytes)))
                       0)))
          0))
    (let ((next (%gc-read64 (+ ctl #x10)))
          (lim  (%gc-read64 (+ ctl #x18))))
      (if (or (zerop next) (> (+ next (%sync-cell-size)) lim))
          (progn (%gc-write64 (+ ctl #x28) (+ (%gc-read64 (+ ctl #x28)) 1))
                 (spin-unlock ctl)
                 0)
          (progn
            (%gc-write64 (+ ctl #x10) (+ next (%sync-cell-size)))
            (%gc-write64 (+ ctl #x20) (+ (%gc-read64 (+ ctl #x20)) 1))
            (spin-unlock ctl)
            ;; Zeroed OUTSIDE the lock: the cell is nobody else's now.  A fresh
            ;; MAP_ANONYMOUS page is already zero, but the arena is one mapping
            ;; for the life of the process and this is cheap certainty.
            (%ha-zero next (+ next (%sync-cell-size)))
            next)))))

;;; THE ALIEN SCRATCH.  sb-alien's WITH-ALIEN needs a few bytes of raw memory
;;; per binding, per thread, that a syscall can be handed a pointer to.  256
;;; bytes at the top of this CPU's per-CPU block, which is 16 KB of which the
;;; actor system uses the first 64.  PER CPU and not per call: WITH-ALIEN nests
;;; at most a few deep in glass and the shim bumps a cursor inside it.
(defun %alien-scratch ()
  (let ((b (%thr-percpu-base (%thr-cpu))))
    (if (zerop b) 0 (+ b #x3F00))))

;;; THE SOCKADDR SCRATCH, SEPARATE FROM MODUS'S OWN.  net/hosted-sockets-post's
;;; %SOCK-ADDR-BUF is per-CPU and SIXTY-FOUR BYTES; a sockaddr_un is 110, so a
;;; UNIX-socket bind built there would run into the NEXT CPU's buffer.  256
;;; bytes at +0x3D00, below the alien scratch, and used by nothing else — so a
;;; shim building an address cannot disturb an in-flight socket-send either.
(defun %sockaddr-scratch ()
  (let ((b (%thr-percpu-base (%thr-cpu))))
    (if (zerop b) 0 (+ b #x3D00))))

;;; A 256-BYTE PER-CPU BUFFER FOR STRUCTS A SYSCALL FILLS IN — struct stat is
;;; 144 bytes.  Separate from the alien scratch because that one is a BUMP
;;; ARENA whose cursor is only unwound by WITH-ALIEN; a function that just needs
;;; a buffer would leak it.
(defun %struct-scratch ()
  (let ((b (%thr-percpu-base (%thr-cpu))))
    (if (zerop b) 0 (+ b #x3C00))))

;;; ============================================================
;;; THE FILE-STREAM STAGING PAGE, PER CPU — AND IT IS A CORRECTNESS FIX
;;; ============================================================
;;;
;;; mvm/cl-fileio.lisp stages EVERY raw read and EVERY raw write through a page
;;; a syscall can be handed the address of.  There was one such page for the
;;; process (*IO-BUF-ADDR*), which was right while there was one thread and is
;;; silent data corruption now that there are sixteen: %FS-WRITE-BYTE stores the
;;; byte at the page and then issues write(2) from it, so two threads writing to
;;; two DIFFERENT descriptors send each other's bytes.
;;;
;;; MEASURED BEFORE THIS EXISTED, on this image: 327 680 bytes — the size of one
;;; RFB raw rectangle at 1280 wide with glass's default 64-row banding — written
;;; by one thread and read by another across a loopback socket pair came back
;;; with 73 933 bytes WRONG, while the identical transfer on one thread was
;;; byte-perfect.  Nothing signalled.  It is on glass's critical path twice over:
;;; glass is thread-per-client, and each client is a reader thread and a sender
;;; thread on the SAME socket.
;;;
;;; 4096 BYTES AT +0x2000, which is the first thing to use the middle of the
;;; per-CPU block: the actor system has the first 64 bytes and the four 256-byte
;;; scratches above have 0x3C00-0x3FFF, so 0x2000-0x2FFF is clear of both with
;;; room on either side.  The size is not a choice — cl-fileio reads 4096 into
;;; it (%FS-READ-CHAR, %FS-READ-BYTE) and a smaller page would overrun into
;;; whatever came next.
;;;
;;; IT DEFERS TO THE OLD PAGE UNLESS PER-CPU MODE IS ACTUALLY ON, and that guard
;;; is deliberate rather than defensive.  Reading the mode word is a load;
;;; asking %THR-PERCPU-BASE is a call that MMAPS the 336 KB thread page on first
;;; use.  Every single-threaded ./modus — which is every test in the tree that
;;; is not about threads — would otherwise pay a new mapping at its first
;;; character of output, for a page it has no second thread to share.  With the
;;; mode off there IS only one thread running Lisp, so the old page is not
;;; merely acceptable, it is correct.  The mode is turned on by %SB-THREADS-UP
;;; before MAKE-THREAD ever returns, so anything that can race is covered.
(defun %fs-io-page ()
  "THIS THREAD's 4096-byte raw I/O staging page.  Overrides the definition in
   mvm/cl-fileio.lisp by last-defun-wins, which is the same seam
   net/hosted-sockets-post.lisp uses for %SOCK-IO-BUF and for the same reason:
   the file that needs the page is loaded long before per-CPU blocks exist."
  (if (= (mem-ref #x10000FF8 :u32) 0)
      *io-buf-addr*
      (let ((b (%thr-percpu-base (%thr-cpu))))
        (if (zerop b) *io-buf-addr* (+ b #x2000)))))

;;; A C STRING IN PER-CPU SCRATCH.  The process-wide *CSTR-SCRATCH* is one
;;; buffer for every thread, which is a path two threads calling stat(2) at once
;;; both write.  256 bytes at +0x3E00; longer strings are REFUSED (0) rather
;;; than truncated into a different filename.
(defun %cstr-percpu (str)
  (let ((b (%thr-percpu-base (%thr-cpu))))
    (if (or (zerop b) (> (length str) 250))
        0
        (let ((a (+ b #x3E00)))
          (%string-to-cstr str a)
          a))))
