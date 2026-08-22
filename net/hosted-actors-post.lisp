;;;; hosted-actors-post.lisp — the LAST-DEFUN-WINS half of the hosted x86-64
;;;; actor adapter.  Baked immediately AFTER net/actors.lisp, so what is here
;;;; overrides what is there; net/hosted-actors.lisp (the address hooks) is
;;;; baked immediately BEFORE it, because a forward reference across the blob
;;;; does not resolve.
;;;;
;;;; Two overrides, both of them arch facts rather than conveniences, and then
;;;; the actor-level selftests.

;;; ============================================================
;;; SPIN-LOCK / SPIN-UNLOCK — no-ops on one cooperative core
;;; ============================================================
;;;
;;; THE REASON IS NOT "it is single-core so the lock is pointless" (true but
;;; not sufficient); it is that KEEPING the lock would DEADLOCK on x64.
;;;
;;; net/actors.lisp takes the scheduler lock, does the switch, and hands the
;;; RELEASE to RESTORE-CONTEXT — YIELD's resume arm is commented "lock already
;;; released by restore-context".  That is true on aarch64, whose
;;; +op-restore-ctx+ stores zero to *AARCH64-SCHED-LOCK-ADDR* after the SP
;;; switch (translate-aarch64.lisp).  translate-x64.lisp's +op-restore-ctx+ has
;;; NO such step: it reloads RBX/RBP/RSP and jumps.  So on x64 the lock would
;;; stay held across every switch and the next SPIN-LOCK would spin forever.
;;;
;;; On ONE cooperative core the lock protects nothing — there is no second CPU
;;; and no preemption, so no other agent can observe the critical section — and
;;; a no-op is exactly what every single-CPU adapter in the tree already ships
;;; (net/arch-x86.lisp, net/arch-aarch64.lisp, net/32bit-overrides.lisp).
;;;
;;; THE REAL FIX, deferred and named: teach translate-x64's RESTORE-CTX to
;;; release the lock the way aarch64's does.  It needs the lock's address at
;;; TRANSLATE time (aarch64 has *AARCH64-SCHED-LOCK-ADDR*, a build constant),
;;; and this adapter's lock address is derived at RUNTIME from the carve — so
;;; doing it properly means giving the hosted lock a fixed BSS address first.
;;; That is a translator change plus a BSS-layout change; it buys nothing until
;;; a second CPU exists, and it is not what steps C and D are about.
(defun spin-lock (addr) 0)
(defun spin-unlock (addr) 0)

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
;;; Band words the workers and the driver share
;;; ============================================================
;;;   +0x190  AP-SCHEDULER entry count (above)
;;;   +0x198  NMSG — how many messages this run passes
;;;   +0x1A0  worker A's forwarded count
;;;   +0x1A8  worker B's logged count
;;;   +0x1B0  worker A's structural-error count
;;;   +0x1B8  worker B's structural-error count
;;;   +0x1C0  worker A's live-data survival errors (step D)
;;;   +0x1C8  worker B's live-data survival errors (step D)
;;;   +0x1D0  worker A's own region collection count at exit (step D)
;;;   +0x1D8  worker B's own region collection count at exit (step D)
;;;   +0x800  message log, 32 bytes per message

(defun %ha-nmsg ()   (%gc-read64 (+ (%ha-base) #x198)))
(defun %ha-bump (off)
  (let ((a (+ (%ha-base) off)))
    (%gc-write64 a (+ (%gc-read64 a) 1))))
(defun %ha-log-entry (i) (+ (+ (%ha-base) #x800) (* i 32)))

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

(defun %ha-worker-a ()
  ;; Receive NMSG messages, check each one's shape, forward it UNCHANGED to
  ;; actor 3.  Forwarding re-serialises: the message is decoded into A's heap
  ;; on the way in and encoded again on the way out, so the round trip is
  ;; exercised twice per message and, in step D, once in each actor's region.
  (let ((n (%ha-nmsg))
        (i 1))
    (loop
      (when (> i n) (return 0))
      (let ((m (receive)))
        (if (consp m)
            (if (= (car m) i)
                (if (consp (cdr m))
                    (if (= (car (cdr m)) (* i 7))
                        (if (= (cdr (cdr m)) (+ i 1000))
                            0
                            (%ha-bump #x1B0))
                        (%ha-bump #x1B0))
                    (%ha-bump #x1B0))
                (%ha-bump #x1B0))
            (%ha-bump #x1B0))
        (send 3 m))
      (%ha-bump #x1A0)
      (setq i (+ i 1)))
    ;; PARK.  In this schedule the run queue holds actor 3 at this point, so
    ;; this RECEIVE blocks once and hands the CPU on; control never returns.
    (loop (receive))))

(defun %ha-worker-b ()
  ;; Receive NMSG forwarded messages, write each one's three fixnums into the
  ;; log, and acknowledge to the primordial actor (id 1).  The ack is a plain
  ;; fixnum, so it takes SEND's FAST path (no staging), while the forwarded
  ;; message took the serialised one — both are exercised.
  (let ((n (%ha-nmsg))
        (i 1))
    (loop
      (when (> i n) (return 0))
      (let ((m (receive)))
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
    ;; PARK.  The primordial actor is runnable here (worker B's own acks woke
    ;; it), so this RECEIVE blocks once and hands the CPU back to it.
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
;;; STEP C — the actor system, running, with NO per-actor regions
;;; ============================================================

(defun %ha-actors-bringup (nmsg)
  "Bring net/actors.lisp up on this process and clear the shared band words.
   Returns the arch_prctl return value (0 = the GS base is set)."
  (let ((rc (%ha-percpu-init))
        (band (%ha-base)))
    (%ha-zero (+ band #x600) (+ band #x800))
    (%ha-zero (+ band #x800) (+ band #x1000))
    (%gc-write64 (+ band #x190) 0)
    (%gc-write64 (+ band #x198) nmsg)
    (%ha-zero (+ band #x1A0) (+ band #x1E0))
    ;; smp-init: per-CPU block, scheduler locks, CPU count.
    ;; actor-init: actor table, scheduler state, the primordial actor (id 1),
    ;;             and the mailbox pool.
    ;; staging-init: the 64 per-actor 16 KB serialisation buffers.
    (smp-init)
    (actor-init)
    (staging-init)
    rc))

(defun %ha-actors-selftest (nmsg)
  "STEP-C ACCEPTANCE.  net/actors.lisp, unmodified, running in a hosted Linux
   process: spawn two actors, pass NMSG messages primordial -> A -> B -> back,
   and record what happened.  EVERY ACTOR STILL ALLOCATES IN REGION 0 — that is
   the point of doing C before D.

   Returns the result block's raw byte address, or 0 if the band could not be
   carved.  Result layout is read back by test/hosted-actors.lisp."
  (if (zerop (%ha-carve))
      0
      (let* ((band *ha-band*)
             (res (+ band #x600))
             (k (%gc-meta-scale))
             (rc (%ha-actors-bringup nmsg))
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
