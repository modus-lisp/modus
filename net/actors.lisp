;;;; actors.lisp - Shared actor system (architecture-independent)
;;;;
;;;; Erlang-style actors with mailboxes, cooperative scheduling, and
;;;; term serialization. Uses MVM intrinsics only.
;;;;
;;;; Requires architecture hooks (loaded before this file):
;;;;   actor-table-base, sched-state-base, sched-lock-addr,
;;;;   pool-state-base, staging-base-addr, actor-stack-base,
;;;;   actor-heap-base, mailbox-pool-base, mailbox-pool-limit,
;;;;   scratch-addr, decode-ptr-addr,
;;;;   get-current-actor, set-current-actor,
;;;;   get-idle-flag, set-idle-flag,
;;;;   wake-idle-ap
;;;;
;;;; THE THREE PROGRESS MARKERS ("ACT", "Bye", "!") GO TO WRITE-CHAR-SERIAL,
;;;; not to WRITE-BYTE, and that is not cosmetic.  WRITE-BYTE is a name with
;;;; two incompatible meanings: on a board it is the arch adapter's ONE-argument
;;;; console byte (net/arch-x86.lisp, net/arch-aarch64.lisp, …), and in a CL
;;;; image it is ANSI's TWO-argument (write-byte byte stream).  This file is
;;;; architecture-independent, so it cannot depend on which one it gets, and a
;;;; one-argument call to the CL function is an arity error.  WRITE-CHAR-SERIAL
;;;; is an MVM intrinsic (trap #x0300) that exists on every target — COM1/UART
;;;; on bare metal, SYS_write(1) on hosted Linux — and is exactly what every
;;;; board's WRITE-BYTE does with a boot marker anyway.  This is what lets the
;;;; file be linked into a hosted image at all.
;;;;
;;;; Actor struct layout (128 bytes, same as x86):
;;;;   +0x00  status        (0=free, 1=running, 2=ready, 3=dead, 4=blocked)
;;;;   +0x08  save area: SP
;;;;   +0x10  save area: alloc ptr (x24/R12)
;;;;   +0x18  save area: alloc limit (x25/R14)
;;;;   +0x20  save area: V4 (x19/RBX)
;;;;   +0x28  (reserved)
;;;;   +0x30  save area: continuation (entry fn / resume addr)
;;;;   +0x38  actor-id
;;;;   +0x40  name
;;;;   +0x48  next-in-queue
;;;;   +0x50  mailbox-head
;;;;   +0x58  mailbox-tail
;;;;   +0x60  linked-actor
;;;;   +0x68  GC REGION (raw byte address of this actor's 64-byte region
;;;;          control block; ZERO MEANS REGION 0 — see below)
;;;;   +0x70  obj-alloc (per-actor object space)
;;;;   +0x78  obj-limit (per-actor object space)
;;;;
;;;; PER-REGION GC, STAGE 3: A REGION IS A PROPERTY OF AN ACTOR.
;;;; Stage 1 made the heap a property of a region and stage 2 made the root set
;;;; one; +0x68 is where an actor says WHICH region is its own.  It holds the
;;;; raw byte address of that actor's control block in the same convention as
;;;; the SP at +0x08 and the alloc pointer/limit at +0x10/+0x18 — the STORED
;;;; MACHINE WORD is the address, hence (untag …) going in and *2 coming back.
;;;;
;;;; ZERO MEANS REGION 0, and that is the whole compatibility story: nothing
;;;; writes the slot, SPAWN does not initialise it, and an actor whose slot is
;;;; zero bump-allocates in region 0 exactly as every actor in every image
;;;; built so far does.  ACTOR-REGION-HOP below short-circuits when NEITHER
;;;; side of a switch owns a region, so a scheduler in which no actor owns one
;;;; performs not a single extra memory write.
;;;;
;;;; +0x68 rather than the other reserved slot at +0x28 because +0x28 is inside
;;;; the save area :SAVE-CTX writes through: SAVE-CONTEXT is handed cur+0x08 and
;;;; stores SP/alloc/limit/V4 at pa+0x00…0x18, the continuation at pa+0x28
;;;; (= struct +0x30) and, on aarch64, obj-alloc/obj-limit at pa+0x68/0x70
;;;; (= struct +0x70/+0x78).  Struct +0x68 is pa+0x60, which nothing writes.
;;;;
;;;; WHAT THIS IMAGE DOES NOT HAVE.  A net/-only image does not link
;;;; mvm/gc.lisp, so %GC-REGION-SWITCH and friends resolve through
;;;; bare-runtime-stubs.lisp's %UNRESOLVED-FN and return NIL here.  That is
;;;; harmless precisely because of the zero guard — the calls are unreachable
;;;; while no actor owns a region — but it means GIVING AN ACTOR A REGION ON
;;;; BARE METAL REQUIRES LINKING mvm/gc.lisp INTO THAT IMAGE FIRST.  The build
;;;; log names every unresolved callee, so the gap is visible, not silent.

;;; ============================================================
;;; Spinlocks
;;; ============================================================

;; Acquire spinlock at addr using atomic exchange (TTAS pattern)
;;
;; THE INNER LOOP READS :U8, AND IT USED TO READ :U64, WHICH MADE IT A NO-OP.
;; The whole point of test-and-TEST-and-set is that a thread which fails to
;; acquire spins on a plain LOAD — the line stays Shared in every waiter's cache
;; and only the release invalidates it — instead of hammering the line with
;; XCHG, which takes it Exclusive on every attempt and starves the holder's
;; neighbours.
;;
;; SPIN-UNLOCK writes the word as a RAW machine word (0 or 1), and per
;; mvm/gc.lisp's word-access block `(mem-ref addr :u64)' loads the machine word
;; and hands it back AS A TAGGED LISP VALUE — so a held lock, raw 1, read back
;; as :u64 is the Lisp value 0 and `zerop' said UNLOCKED every time.  The inner
;; loop therefore fell straight through on its first iteration and the acquire
;; degenerated into an unbounded XCHG hammer.
;;
;; :U8 IS THE FIX AND IT IS DELIBERATELY NOT :U64.  A :u8 load is tagged on the
;; way out (mvm-eval's mem-ref semantics), so the Lisp value IS the raw byte: 0
;; or 1.  It reads byte 0 of the word, which on a little-endian target is the
;; one SPIN-UNLOCK zeroes — the same assumption NEEDS-STAGING and STAGING-TAG in
;; this file already make.  On a big-endian target byte 0 is the MSB, always 0
;; for a 0/1 lock, so the loop falls through exactly as it does today: the fix
;; is an improvement where it applies and a no-op where it does not.
;;
;; MEASURED on hosted x86-64 with an actor busy-polling TRY-RECEIVE + YIELD on
;; one thread (it takes and releases this lock twice per iteration with no work
;; in between) while another thread SENDs to it: ten sends took 2.0 s of extra
;; wall time typically and 122 s in the worst run observed, because the
;; releasing thread re-acquired from its own L1 before the remote XCHG could
;; land.  With the read-only wait it is 0.1 s.
(defun spin-lock (addr)
  (loop
    (if (zerop (xchg-mem addr 1))
        (return 0)
        (loop
          (if (zerop (mem-ref addr :u8))
              (return 0)
              (pause))))))

;; Release spinlock
(defun spin-unlock (addr)
  (mfence)
  (setf (mem-ref addr :u64) 0))

;;; ============================================================
;;; Actor struct accessors
;;; ============================================================

;; Address of actor struct for actor ID
(defun actor-struct-addr (id)
  (+ (actor-table-base) (* id 128)))

;; Read field from actor struct
(defun actor-get (id offset)
  (mem-ref (+ (actor-struct-addr id) offset) :u64))

;; Write field to actor struct
(defun actor-set (id offset val)
  (setf (mem-ref (+ (actor-struct-addr id) offset) :u64) val))

;; Stack top for actor ID (each gets 64KB, grows down)
(defun actor-stack-top (id)
  (+ (actor-stack-base) (* (+ id 1) #x10000)))

;;; ============================================================
;;; Per-region GC (stage 3): which region is this actor's?
;;; ============================================================

;; Raw byte address of ID's region control block, 0 if it owns none.
;; ACTOR-GET reads the slot's machine word into a register, where a fixnum is
;; value<<1 — so the Lisp value it hands back is HALF the address and the
;; doubling here is the exact inverse of the (untag …) that stores it.
(defun actor-region-raw (id)
  (let ((w (actor-get id #x68)))
    (if (zerop w) 0 (* w 2))))

;; ONE SCHEDULER HOP, from the GC's point of view.  CUR-ID is being switched
;; out and NEXT-ID switched in.  %GC-REGION-SWITCH parks the region being left
;; — its allocation pointer and limit AND its root window, because an actor's
;; heap and an actor's roots go off-CPU together — and makes the arriving
;; actor's region the running one.
;;
;; THE SP IT PARKS AT is read straight back out of CUR-ID's struct at +0x08,
;; where the SAVE-CONTEXT immediately above every call site has just written
;; it.  That is the only place the number exists: no MVM primitive yields the
;; stack pointer as a VALUE on every target, which is why %GC-REGION-PARK takes
;; it as an argument at all.  ACTOR-GET returns the slot's machine word read as
;; a Lisp integer (a fixnum is value<<1), so the doubling is the exact inverse
;; of the (untag …) that SPAWN and :SAVE-CTX store through.
;;
;; THE GUARD IS THE COMPATIBILITY STORY, not an optimisation.  When NEITHER
;; actor names a region, every actor is allocating in region 0 — the state of
;; every image built so far — and this performs no write and makes no call.
(defun actor-region-hop (cur-id next-id)
  (let ((cr (actor-region-raw cur-id))
        (nr (actor-region-raw next-id)))
    (if (and (zerop cr) (zerop nr))
        0
        (%gc-region-switch (if (zerop nr) (%gc-region-0) nr)
                           (* (actor-get cur-id #x08) 2)))))

;; The same thing where there is NO outgoing actor to park: the idle scheduler
;; picking work up, and an actor that has already marked itself dead.  Nothing
;; is parked because nothing is leaving a live stack behind.
(defun actor-region-resume (next-id)
  (let ((nr (actor-region-raw next-id)))
    (if (zerop nr)
        0
        (progn (%gc-region-enter nr) (%gc-region-unpark nr)))))

;;; ============================================================
;;; Run queue (linked list via actor struct +0x48)
;;; ============================================================

;; Add actor to run queue tail
(defun actor-enqueue (id)
  (actor-set id #x48 0)
  (let ((head (mem-ref (+ (sched-state-base) #x10) :u64)))
    (if (zerop head)
        (setf (mem-ref (+ (sched-state-base) #x10) :u64) id)
        (let ((cur head))
          (loop
            (let ((next (actor-get cur #x48)))
              (when (zerop next)
                (actor-set cur #x48 id)
                (return 0))
              (setq cur next)))))))

;; Remove and return first actor from run queue (0 if empty)
(defun actor-dequeue ()
  (let ((head (mem-ref (+ (sched-state-base) #x10) :u64)))
    (if (zerop head)
        0
        (let ((next (actor-get head #x48)))
          (setf (mem-ref (+ (sched-state-base) #x10) :u64) next)
          (actor-set head #x48 0)
          head))))

;;; ============================================================
;;; Mailbox shared pool
;;; ============================================================
;;;
;;; Pool state at pool-state-base:
;;;   +0x00  pool-next   (next free cell raw address)
;;;   +0x08  pool-limit  (end of pool)
;;;   +0x10  free-head   (free list, cons-tagged or 0)

(defun pool-init ()
  (let ((ps (pool-state-base)))
    (setf (mem-ref ps :u64) (mailbox-pool-base))
    (let ((ps8 (+ ps 8)))
      (setf (mem-ref ps8 :u64) (mailbox-pool-limit)))
    (let ((ps16 (+ ps 16)))
      (setf (mem-ref ps16 :u64) 0))))

;; Allocate 16-byte cell. Returns cons-tagged pointer or 0.
(defun pool-alloc ()
  (let ((ps (pool-state-base)))
    (let ((free (mem-ref (+ ps 16) :u64)))
      (if (not (zerop free))
          (progn
            (setf (mem-ref (+ ps 16) :u64) (cdr free))
            free)
          (let ((ptr (mem-ref ps :u64)))
            (if (>= ptr (mem-ref (+ ps 8) :u64))
                0
                (progn
                  (setf (mem-ref ps :u64) (+ ptr 16))
                  (logior (untag ptr) (untag 1)))))))))

;; Return cell to free list
(defun pool-free (cell)
  (let ((ps (pool-state-base)))
    (set-cdr cell (mem-ref (+ ps 16) :u64))
    (setf (mem-ref (+ ps 16) :u64) cell)
    0))

;;; ============================================================
;;; Per-CPU accessors (standardized layout across all architectures)
;;; ============================================================
;;;
;;; AArch64 per-CPU layout (via TPIDR_EL1):
;;;   +0x00 self-ptr       +0x08 reduction
;;;   +0x10 cpu-id         +0x18 current-actor
;;;   +0x20 idle-flag      +0x28 obj-alloc
;;;   +0x30 obj-limit      +0x38 idle-stack-top

(defun get-current-actor () (percpu-ref 24))
(defun set-current-actor (val) (percpu-set 24 val))
(defun get-idle-flag () (percpu-ref 32))
(defun set-idle-flag (val) (percpu-set 32 val))

;;; ============================================================
;;; SMP initialization (single CPU, parameterized addresses)
;;; ============================================================

(defun smp-init ()
  ;; TPIDR_EL1 already set by boot code
  ;; Initialize per-CPU data for BSP (CPU 0)
  (let ((base (percpu-data-base)))
    (setf (mem-ref base :u64) base)
    (setf (mem-ref (+ base 8) :u64) 0)
    (setf (mem-ref (+ base 16) :u64) 0)
    (setf (mem-ref (+ base 24) :u64) 0)
    (setf (mem-ref (+ base 32) :u64) 0)
    ;; idle stack top for CPU 0: percpu-data-base + 0x2000
    (setf (mem-ref (+ base 56) :u64) (+ (percpu-data-base) #x2000)))
  ;; Zero lock variables
  (setf (mem-ref (sched-lock-addr) :u64) 0)
  (let ((lk2 (+ (sched-lock-addr) 8)))
    (setf (mem-ref lk2 :u64) 0))
  (let ((lk3 (+ (sched-lock-addr) 16)))
    (setf (mem-ref lk3 :u64) 0))
  ;; CPU count = 1 (single CPU)
  (let ((cc (+ (sched-state-base) #x20)))
    (setf (mem-ref cc :u64) 1))
  1)

;;; ============================================================
;;; IPI / Wake (stubs for single CPU, override for SMP)
;;; ============================================================

(defun send-ipi-to-idle (target-cpu) 0)
(defun wake-idle-ap () 0)

;;; ============================================================
;;; Shutdown
;;; ============================================================

(defun shutdown ()
  ;; Print "Bye\n" then halt
  (write-char-serial 66) (write-char-serial 121)
  (write-char-serial 101) (write-char-serial 10)
  (loop (halt)))

;;; ============================================================
;;; Actor init
;;; ============================================================

(defun actor-init ()
  ;; Zero actor table (64 * 128 = 8192 bytes)
  (let ((base (actor-table-base))
        (i 0))
    (loop
      (when (>= i 8192) (return 0))
      (setf (mem-ref (+ base i) :u64) 0)
      (setq i (+ i 8))))
  ;; Zero scheduler state
  (let ((ss (sched-state-base)))
    (setf (mem-ref (+ ss 8) :u64) 2)    ; actor-count = 2 (next available)
    (setf (mem-ref (+ ss #x10) :u64) 0) ; run-head = none
    (setf (mem-ref (+ ss #x18) :u64) 0)) ; initialized = 0 (set after per-CPU)
  ;; Per-CPU: current-actor = 1 (primordial), reduction counter
  (set-current-actor 1)
  (percpu-set 8 4000)         ; reduction counter: tagged 2000
  ;; Set initialized flag
  (setf (mem-ref (+ (sched-state-base) #x18) :u64) 1)
  ;; Initialize mailbox pool
  (pool-init)
  ;; Set up actor 1 (primordial)
  (actor-set 1 #x00 1)    ; status = running
  (actor-set 1 #x38 1)    ; actor-id = 1
  ;; Primordial actor keeps boot-time alloc region (large enough for SSH crypto).
  ;; Just record current alloc state for context-switch save/restore.
  (let ((cur-alloc (get-alloc-ptr))
        (cur-limit (get-alloc-limit)))
    (actor-set 1 #x70 cur-alloc)
    (actor-set 1 #x78 cur-limit)
    (percpu-set 40 cur-alloc)
    (percpu-set 48 cur-limit))
  ;; Print "ACT"
  (write-char-serial 65) (write-char-serial 67)
  (write-char-serial 84) (write-char-serial 10))

;;; ============================================================
;;; Actor spawn
;;; ============================================================
;;;
;;; fn is a tagged native function address.
;;; Entry function MUST NOT return (no actor-exit on AArch64 yet).
;;; Loop forever or explicitly handle lifecycle.

(defun actor-spawn (fn)
  (spin-lock (sched-lock-addr))
  (let ((count (mem-ref (+ (sched-state-base) 8) :u64)))
    (if (>= count 64)
        (progn
          (spin-unlock (sched-lock-addr))
          (write-char-serial 33)   ; '!' too many actors
          0)
        (let ((id count))
          ;; Bump actor count
          (setf (mem-ref (+ (sched-state-base) 8) :u64) (+ count 1))
          ;; Initialize actor struct
          (actor-set id #x00 2)    ; status = ready
          (actor-set id #x38 id)   ; actor-id
          ;; Set up save area for restore-context
          (let ((stack-top (actor-stack-top id)))
            ;; SP = stack top (no actor-exit pushed — fn must not return)
            (actor-set id #x08 (untag stack-top))
            ;; Per-actor heap: base = actor-heap-base + (id-1) * 0x400000 (4MB each)
            (let ((id1 (- id 1)))
              (let ((heap-off (ash id1 22)))
                (let ((heap-base (+ (actor-heap-base) heap-off)))
                  ;; Alloc ptr (x24/R12): bump allocator start
                  (actor-set id #x10 (untag heap-base))
                  ;; Alloc limit (x25/R14): bump allocator end (4MB per actor)
                  ;; Both cons and objects allocate from x24→x25 on AArch64
                  (let ((limit (+ heap-base #x400000)))
                    (actor-set id #x18 (untag limit)))
                  ;; obj-alloc and obj-limit (TAGGED — percpu-ref returns tagged values)
                  ;; 3.5MB obj space: 0x80000 to 0x400000 (enough for SSH crypto)
                  (actor-set id #x70 (+ heap-base #x80000))
                  (actor-set id #x78 (+ heap-base #x400000)))))
            ;; V4 (x19/RBX) = 0
            (actor-set id #x20 0)
            ;; Continuation = entry function
            (actor-set id #x30 (untag fn)))
          ;; Add to run queue
          (actor-enqueue id)
          (spin-unlock (sched-lock-addr))
          ;; Wake idle AP
          (wake-idle-ap)
          id))))

;;; ============================================================
;;; Yield (cooperative context switch)
;;; ============================================================

(defun yield ()
  (if (zerop (mem-ref (+ (sched-state-base) #x18) :u64))
      ;; Actor system not initialized, no-op
      0
      (progn
        (spin-lock (sched-lock-addr))
        (let ((next-id (actor-dequeue)))
          (if (zerop next-id)
              ;; No other actors ready
              (progn (spin-unlock (sched-lock-addr)) 0)
              (let ((cur-id (get-current-actor)))
                  (if (zerop cur-id)
                      ;; No current actor (idle scheduler) — run dequeued directly
                      (progn
                        (set-idle-flag 0)
                        (set-current-actor next-id)
                        (actor-set next-id #x00 1)
                        (percpu-set 40 (actor-get next-id #x70))
                        (percpu-set 48 (actor-get next-id #x78))
                        ;; No outgoing actor, so nothing to park.
                        (actor-region-resume next-id)
                        (let ((next-addr (actor-struct-addr next-id)))
                          (restore-context (+ next-addr #x08))))
                      (let ((cur-addr (actor-struct-addr cur-id)))
                        ;; save-context returns 0 on save, nonzero on resume
                        (if (zerop (save-context (+ cur-addr #x08)))
                            ;; Save path: do the switch
                            (progn
                              (actor-set cur-id #x00 2)
                              ;; Save outgoing actor's object space
                              (actor-set cur-id #x70 (percpu-ref 40))
                              (actor-set cur-id #x78 (percpu-ref 48))
                              (actor-enqueue cur-id)
                              ;; Switch to next actor
                              (set-current-actor next-id)
                              (actor-set next-id #x00 1)
                              (percpu-set 40 (actor-get next-id #x70))
                              (percpu-set 48 (actor-get next-id #x78))
                              ;; PER-REGION GC, STAGE 3.  Park the outgoing
                              ;; actor's region at the SP save-context wrote to
                              ;; its struct at +0x08 and make the arriving
                              ;; actor's region the running one.  It goes HERE,
                              ;; immediately before restore-context, on purpose:
                              ;; %gc-region-enter loads the arriving region's
                              ;; parked allocation pointer/limit, and
                              ;; restore-context then reinstates the same pair
                              ;; from the arriving actor's own save area — so
                              ;; there is no window in which the mutator is
                              ;; running on one actor's stack with another
                              ;; actor's allocation registers.
                              (actor-region-hop cur-id next-id)
                              (let ((next-addr (actor-struct-addr next-id)))
                                (restore-context (+ next-addr #x08))))
                            ;; Resume path: lock already released by restore-context
                            0)))))))))

;;; ============================================================
;;; Actor lifecycle
;;; ============================================================

(defun actor-self ()
  (get-current-actor))

(defun actor-count ()
  (mem-ref (+ (sched-state-base) 8) :u64))

;; Called when a spawned actor's function returns.
;; Entry functions SHOULD call this explicitly on AArch64.
(defun actor-exit ()
  (let ((cur (get-current-actor)))
    ;; Notify linked actor if any
    (let ((linked (actor-get cur #x60)))
      (if (not (zerop linked))
          (send linked cur)
          0))
    ;; Mark dead
    (actor-set cur #x00 3))
  ;; Switch to next ready actor
  (spin-lock (sched-lock-addr))
  (let ((next-id (actor-dequeue)))
    (if (zerop next-id)
        (progn (spin-unlock (sched-lock-addr)) (ap-scheduler))
        (progn
          (set-current-actor next-id)
          (actor-set next-id #x00 1)
          (percpu-set 40 (actor-get next-id #x70))
          (percpu-set 48 (actor-get next-id #x78))
          ;; The outgoing actor is DEAD (status 3) — its region has no roots
          ;; worth parking, so this only enters the arriving one's.
          (actor-region-resume next-id)
          (let ((next-addr (actor-struct-addr next-id)))
            (restore-context (+ next-addr #x08)))))))

;; Link current actor to another
(defun link (other-id)
  (let ((self-id (actor-self)))
    (actor-set self-id #x60 other-id)
    (actor-set other-id #x60 self-id)
    0))

;; Spawn + link
(defun spawn-link (fn)
  (let ((wid (actor-spawn fn)))
    (link wid)
    wid))

;;; ============================================================
;;; Scheduler (idle loop)
;;; ============================================================

;; THE HAND-OFF POINT FOR AN ACTOR THAT HAS JUST BLOCKED, and it is a separate
;; function for exactly one reason: WHO HOLDS THE SCHEDULER LOCK.
;;
;; RECEIVE reaches here having marked the calling actor BLOCKED (status 4) and
;; saved its context, WITH THE LOCK STILL HELD.  From the instant the lock drops
;; that actor is claimable by any other CPU — a sender flips it back to READY and
;; enqueues it (MAILBOX-ENQUEUE-AND-WAKE), and another CPU's scheduler may then
;; RESTORE-CONTEXT onto its stack.  This CPU is STILL STANDING ON THAT STACK
;; until it switches away, so the order "release, then leave the stack" is a
;; two-CPUs-one-stack race, and the order "leave the stack, then release" is not.
;;
;; ON BARE METAL these two lines ARE what RECEIVE used to do inline, unchanged:
;; release and fall into the idle loop, which switches to the per-CPU idle stack
;; as its first act.  The window is real there too, but bare metal has never run
;; a second CPU, so nothing can take the actor before the switch.
;;
;; A HOSTED IMAGE OVERRIDES THIS (net/hosted-sync.lisp) to do it in the safe
;; order: RESTORE-CONTEXT into the thread's own scheduler context — which moves
;; RSP off the actor's stack and only THEN releases the lock, because
;; +OP-RESTORE-CTX+ zeroes the lock word after the stack switch and before the
;; jump.  That is why RECEIVE now calls this instead of unlocking itself: an
;; override cannot re-acquire a lock it was not handed.
(defun ap-scheduler-blocked ()
  (spin-unlock (sched-lock-addr))
  (ap-scheduler))

(defun ap-scheduler ()
  ;; Switch to per-CPU idle stack
  (switch-idle-stack)
  ;; No actor running
  (set-current-actor 0)
  (loop
    (cli)
    (set-idle-flag 1)
    (spin-lock (sched-lock-addr))
    (let ((next-id (actor-dequeue)))
      (if (zerop next-id)
          ;; Nothing to do: unlock, then STI+HLT (WFI on AArch64)
          (progn
            (spin-unlock (sched-lock-addr))
            (sti-hlt))
          ;; Got work: switch to actor
          (progn
            (set-idle-flag 0)
            (set-current-actor next-id)
            (actor-set next-id #x00 1)
            (percpu-set 40 (actor-get next-id #x70))
            (percpu-set 48 (actor-get next-id #x78))
            ;; The idle loop owns no region, so there is nothing to park.
            (actor-region-resume next-id)
            (let ((next-addr (actor-struct-addr next-id)))
              (restore-context (+ next-addr #x08))))))))

;;; ============================================================
;;; Term serialization (staging buffers)
;;; ============================================================
;;;
;;; Per-actor staging buffers at staging-base-addr (16KB each, max 64).
;;; Layout: [0:8] write-offset, [8:16] read-offset, [16:16384] data.

(defun staging-init ()
  (let ((base (staging-base-addr))
        (i 0))
    (loop
      (when (>= i 131072) (return 0))
      (setf (mem-ref (+ base (ash i 3)) :u64) 0)
      (setq i (+ i 1)))))

;; Check if val needs serialization (machine bit 0 = 1 means cons/object)
(defun needs-staging (val)
  (let ((sa (scratch-addr)))
    (setf (mem-ref sa :u64) val)
    (logand (mem-ref sa :u8) 1)))

;; Tag a buffer address for staging (set machine bit 0)
(defun staging-tag (buf-addr)
  (let ((sa (scratch-addr)))
    (setf (mem-ref sa :u64) buf-addr)
    (let ((b0 (mem-ref sa :u8)))
      (setf (mem-ref sa :u8) (logior b0 1)))
    (mem-ref sa :u64)))

;; Check if pool cell car is a staging pointer
(defun staging-ptr-p (raw-msg)
  (let ((sa (scratch-addr)))
    (setf (mem-ref sa :u64) raw-msg)
    (logand (mem-ref sa :u8) 1)))

;; Remove staging tag (clear machine bit 0)
(defun staging-untag (raw-msg)
  (let ((sa (scratch-addr)))
    (setf (mem-ref sa :u64) raw-msg)
    (let ((b0 (mem-ref sa :u8)))
      (setf (mem-ref sa :u8) (logand b0 (- 0 2))))
    (mem-ref sa :u64)))

;; Read subtag byte from object header.
;; Compatible with try-alloc-obj pointer format (tag in low 2 bits).
;; Uses the same raw-address derivation as aref/array-length.
(defun soft-subtag (obj)
  (let ((raw (ash (logand obj (- 0 4)) 1)))
    (mem-ref raw :u8)))

;; Compute serialized byte count for a term
(defun term-size (val)
  (if (zerop val) 1
      (if (consp val)
          (let ((s1 (term-size (car val))))
            (let ((s2 (term-size (cdr val))))
              (+ 1 (+ s1 s2))))
          (if (numberp val) 9
              (let ((st (soft-subtag val)))
                (if (= st #x32) (+ 5 (array-length val))
                    (if (= st #x30)
                        (let ((addr (ash (logand val (- 0 4)) 1)))
                          (+ 5 (ash (ash (mem-ref addr :u64) -15) 3)))
                        9)))))))

;; Serialize term to buffer at raw address buf. Returns bytes written.
;; Uses soft-subtag for object type dispatch (compatible with try-alloc-obj).
(defun term-encode (val buf)
  (if (zerop val)
      (progn (setf (mem-ref buf :u8) 0) 1)
      (if (consp val)
          (progn
            (setf (mem-ref buf :u8) 2)
            (let ((n1 (term-encode (car val) (+ buf 1))))
              (let ((buf2 (+ (+ buf 1) n1)))
                (let ((n2 (term-encode (cdr val) buf2)))
                  (+ (+ 1 n1) n2)))))
          (if (numberp val)
              ;; Fixnum (non-zero)
              (progn
                (setf (mem-ref buf :u8) 1)
                (setf (mem-ref (+ buf 1) :u64) val)
                9)
          ;; ---- STRINGS AND SYMBOLS GO BY VALUE, NOT BY POINTER ----------
          ;;
          ;; Both used to fall off the end of this dispatch into the "unknown
          ;; object type — encode as fixnum" arm, which writes THE OBJECT'S OWN
          ;; TAGGED POINTER and labels it a fixnum.  Measured, not inferred: a
          ;; symbol encoded to tag 1 with a payload equal, bit for bit, to its
          ;; own machine word.  So every actor message carrying a string or a
          ;; symbol handed the receiver a pointer into the SENDER'S REGION —
          ;; the exact violation of "no region holds a pointer into another"
          ;; that the whole per-region collector rests on, sitting in the
          ;; message path.  Serialising is the ONLY thing that makes separate
          ;; regions per actor sound, and for the two commonest payload types
          ;; it was not happening.
          ;;
          ;; TAG 3 IS NOT NEW.  TERM-DECODE-STEP has always had the arm that
          ;; reads a length, reads the bytes and MAKE-STRINGs a copy IN THE
          ;; RECEIVER'S REGION; it was dead code because nothing ever emitted
          ;; it.  The asymmetry was the bug.  This emits it.
          (if (stringp val)
              (let ((slen (length val)))
                (setf (mem-ref buf :u8) 3)
                (setf (mem-ref (+ buf 1) :u32) slen)
                (let ((i 0))
                  (loop
                    (when (>= i slen) (return 0))
                    (setf (mem-ref (+ (+ buf 5) i) :u8) (char-code (aref val i)))
                    (setq i (+ i 1))))
                (+ 5 slen))
          ;; TAG 6 — A SYMBOL, BY NAME AND PACKAGE NAME.
          ;;
          ;; The receiver INTERNS it, which is the whole point: the intern
          ;; happens on the RECEIVING thread, so the symbol and the table that
          ;; points at it are allocated in the SAME region by construction.  No
          ;; barrier, no promotion, no delegation — the pointer that would have
          ;; had to be fixed up is never created.
          ;;
          ;; A symbol with no home package (an uninterned gensym) encodes a
          ;; ZERO-length package and decodes through MAKE-SYMBOL: two actors
          ;; cannot share an uninterned symbol by name and must not pretend to.
              (if (symbolp val)
                  (let* ((nm (symbol-name val))
                         (pk (symbol-package val))
                         (pn (if pk (package-name pk) ""))
                         (nlen (length nm))
                         (plen (length pn)))
                    (setf (mem-ref buf :u8) 6)
                    (setf (mem-ref (+ buf 1) :u32) nlen)
                    (let ((i 0))
                      (loop
                        (when (>= i nlen) (return 0))
                        (setf (mem-ref (+ (+ buf 5) i) :u8) (char-code (aref nm i)))
                        (setq i (+ i 1))))
                    (setf (mem-ref (+ (+ buf 5) nlen) :u32) plen)
                    (let ((i 0) (pbase (+ (+ buf 9) nlen)))
                      (loop
                        (when (>= i plen) (return 0))
                        (setf (mem-ref (+ pbase i) :u8) (char-code (aref pn i)))
                        (setq i (+ i 1))))
                    (+ (+ 9 nlen) plen))
              ;; Object: dispatch on subtag
              (let ((st (soft-subtag val)))
                (if (= st #x32)
                    ;; Array
                    (let ((alen (array-length val)))
                      (setf (mem-ref buf :u8) 4)
                      (setf (mem-ref (+ buf 1) :u32) alen)
                      (let ((i 0))
                        (loop
                          (when (>= i alen) (return 0))
                          (setf (mem-ref (+ (+ buf 5) i) :u8) (aref val i))
                          (setq i (+ i 1))))
                      (+ 5 alen))
                    (if (= st #x30)
                        ;; Bignum
                        (let ((addr (ash (logand val (- 0 4)) 1)))
                          (let ((nlimbs (ash (mem-ref addr :u64) -15)))
                            (setf (mem-ref buf :u8) 5)
                            (setf (mem-ref (+ buf 1) :u32) nlimbs)
                            (let ((i 0))
                              (loop
                                (when (>= i nlimbs) (return 0))
                                (setf (mem-ref (+ (+ buf 5) (ash i 3)) :u64)
                                      (mem-ref (+ (+ addr 8) (ash i 3)) :u64))
                                (setq i (+ i 1))))
                            (+ 5 (ash nlimbs 3))))
                        ;; STILL UNKNOWN — and still a pointer, deliberately
                        ;; left as it was.  Everything that has been MEASURED
                        ;; to come through here (symbols, strings) is now
                        ;; handled above; narrowing this arm further without a
                        ;; demonstration of what lands in it would be guessing.
                        ;; It remains a soundness hole for any type nobody has
                        ;; looked at yet.
                        (progn
                          (setf (mem-ref buf :u8) 1)
                          (setf (mem-ref (+ buf 1) :u64) val)
                          9))))))))))

;; Deserialize one term from staging buffer.
;; Uses global read pointer at decode-ptr-addr to track position.
(defun term-decode-step ()
  (let ((da (decode-ptr-addr)))
    (let ((buf (mem-ref da :u64)))
      (let ((tag (mem-ref buf :u8)))
        (if (zerop tag)
            (progn (setf (mem-ref da :u64) (+ buf 1)) 0)
            (if (= tag 1)
                (let ((v (mem-ref (+ buf 1) :u64)))
                  (setf (mem-ref da :u64) (+ buf 9))
                  v)
                (if (= tag 2)
                    (progn
                      (setf (mem-ref da :u64) (+ buf 1))
                      (let ((car-val (term-decode-step)))
                        (let ((cdr-val (term-decode-step)))
                          (cons car-val cdr-val))))
                    (if (= tag 6)
                        ;; A SYMBOL, BY NAME — AND THE INTERN HAPPENS HERE.
                        ;;
                        ;; That is the entire point of the tag.  Interning on
                        ;; the RECEIVING thread allocates the symbol in the
                        ;; RECEIVER'S region, which is the same region as the
                        ;; table that will point at it — so the cross-region
                        ;; pointer that used to be shipped in the message is
                        ;; never created rather than being created and then
                        ;; repaired.
                        (let ((nlen (mem-ref (+ buf 1) :u32)))
                          (let ((nm (make-string nlen)))
                            (let ((i 0))
                              (loop
                                (when (>= i nlen) (return 0))
                                (setf (aref nm i) (code-char (mem-ref (+ (+ buf 5) i) :u8)))
                                (setq i (+ i 1))))
                            (let ((plen (mem-ref (+ (+ buf 5) nlen) :u32)))
                              (let ((pn (make-string plen))
                                    (pbase (+ (+ buf 9) nlen)))
                                (let ((i 0))
                                  (loop
                                    (when (>= i plen) (return 0))
                                    (setf (aref pn i) (code-char (mem-ref (+ pbase i) :u8)))
                                    (setq i (+ i 1))))
                                (setf (mem-ref da :u64) (+ pbase plen))
                                ;; No home package — an uninterned symbol.  Two
                                ;; actors cannot share one by name, so give the
                                ;; receiver its own rather than pretend.
                                (if (= plen 0)
                                    (make-symbol nm)
                                    (intern nm pn))))))
                    (if (= tag 3)
                        ;; STRING-SET USED TO FILL THIS, AND STRING-SET DOES NOT
                        ;; EXIST.  Not "was removed" — `defun string-set' appears
                        ;; nowhere in the tree, (FBOUNDP 'STRING-SET) is NIL, and
                        ;; this was its ONLY call site.  So this arm was dead in
                        ;; two senses at once: nothing ever emitted tag 3, and it
                        ;; could not have worked if anything had.  MAKE-STRING
                        ;; fills with spaces, so decoding through it gave a
                        ;; string of the right LENGTH and no content — which is
                        ;; exactly what came back the first time the encoder
                        ;; emitted tag 3.
                        (let ((slen (mem-ref (+ buf 1) :u32)))
                          (let ((s (make-string slen)))
                            (let ((i 0))
                              (loop
                                (when (>= i slen) (return 0))
                                (setf (aref s i) (code-char (mem-ref (+ (+ buf 5) i) :u8)))
                                (setq i (+ i 1))))
                            (setf (mem-ref da :u64) (+ (+ buf 5) slen))
                            s))
                        (if (= tag 4)
                            (let ((alen (mem-ref (+ buf 1) :u32)))
                              (let ((a (make-array alen)))
                                (let ((i 0))
                                  (loop
                                    (when (>= i alen) (return 0))
                                    (aset a i (mem-ref (+ (+ buf 5) i) :u8))
                                    (setq i (+ i 1))))
                                (setf (mem-ref da :u64) (+ (+ buf 5) alen))
                                a))
                            ;; Bignum (tag 5)
                            (let ((nlimbs (mem-ref (+ buf 1) :u32)))
                              (let ((b (make-bignum-n nlimbs)))
                                (let ((baddr (ash (logand b (- 0 4)) 1)))
                                  (let ((i 0))
                                    (loop
                                      (when (>= i nlimbs) (return 0))
                                      (setf (mem-ref (+ (+ baddr 8) (ash i 3)) :u64)
                                            (mem-ref (+ (+ buf 5) (ash i 3)) :u64))
                                      (setq i (+ i 1))))
                                (setf (mem-ref da :u64)
                                      (+ (+ buf 5) (ash nlimbs 3)))
                                b)))))))))))))

;; Allocate bignum with nlimbs limbs
(defun make-bignum-n (nlimbs)
  (let ((byte-size (ash nlimbs 3)))
    (let ((result (try-alloc-obj byte-size #x30)))
      (if (zerop result) 0
          (let ((raw (ash (logand result (- 0 4)) 1)))
            (setf (mem-ref raw :u64) (logior (ash nlimbs 15) (untag #x30)))
            result)))))

;; Compact staging buffer
(defun staging-compact (staging-base)
  (let ((roff (mem-ref (+ staging-base 8) :u64)))
    (if (zerop roff)
        0
        (let ((woff (mem-ref staging-base :u64)))
          (let ((remaining (- woff roff)))
            (let ((i 0))
              (loop
                (when (>= i remaining) (return 0))
                (let ((dst (+ (+ staging-base 16) i)))
                  (let ((src (+ (+ (+ staging-base 16) roff) i)))
                    (setf (mem-ref dst :u8) (mem-ref src :u8))))
                (setq i (+ i 1))))
            (setf (mem-ref staging-base :u64) remaining)
            (setf (mem-ref (+ staging-base 8) :u64) 0)
            remaining)))))

;;; ============================================================
;;; Mailbox operations
;;; ============================================================

;; Dequeue message from actor's mailbox. Returns 0 if empty.
(defun mailbox-dequeue (id)
  (let ((head (actor-get id #x50)))
    (if (zerop head)
        0
        (let ((raw-msg (car head)))
          (let ((next (cdr head)))
            (actor-set id #x50 next)
            (if (zerop next) (actor-set id #x58 0) ())
            (pool-free head)
            (if (not (zerop (staging-ptr-p raw-msg)))
                ;; Staging pointer — decode into receiver's heap
                (let ((buf-addr (staging-untag raw-msg)))
                  (setf (mem-ref (decode-ptr-addr) :u64) buf-addr)
                  (let ((result (term-decode-step)))
                    (let ((staging-base (+ (staging-base-addr) (ash id 14))))
                      (let ((new-roff (- (mem-ref (decode-ptr-addr) :u64)
                                         (+ staging-base 16))))
                        (setf (mem-ref (+ staging-base 8) :u64) new-roff)))
                    result))
                ;; Fixnum/nil — return as-is
                raw-msg))))))

;; Enqueue cell into target's mailbox and wake if blocked
(defun mailbox-enqueue-and-wake (target-id cell)
  (let ((tail (actor-get target-id #x58)))
    (if (zerop tail)
        (progn
          (actor-set target-id #x50 cell)
          (actor-set target-id #x58 cell))
        (progn
          (set-cdr tail cell)
          (actor-set target-id #x58 cell))))
  ;; If target was blocked (status=4), wake it
  (if (= (actor-get target-id #x00) 4)
      (progn
        (actor-set target-id #x00 2)
        (actor-enqueue target-id)
        (spin-unlock (sched-lock-addr))
        (wake-idle-ap)
        0)
      (progn
        (spin-unlock (sched-lock-addr))
        0)))

;;; ============================================================
;;; Send / Receive
;;; ============================================================

(defun send (target-id message)
  (spin-lock (sched-lock-addr))
  (let ((cell (pool-alloc)))
    (if (zerop cell)
        (progn (spin-unlock (sched-lock-addr)) 0)
        (if (zerop (needs-staging message))
            ;; Fast path: fixnum/nil
            (progn
              (set-car cell message)
              (set-cdr cell 0)
              (mailbox-enqueue-and-wake target-id cell))
            ;; Slow path: serialize to target's staging buffer
            (let ((size (term-size message)))
              (let ((staging-base (+ (staging-base-addr) (ash target-id 14))))
                (let ((woff (mem-ref staging-base :u64)))
                  (if (> (+ woff size) 16368)
                      ;; Try compacting
                      (progn
                        (staging-compact staging-base)
                        (let ((woff2 (mem-ref staging-base :u64)))
                          (if (> (+ woff2 size) 16368)
                              (progn (pool-free cell)
                                     (spin-unlock (sched-lock-addr)) 0)
                              (let ((buf-addr (+ (+ staging-base 16) woff2)))
                                (let ((written (term-encode message buf-addr)))
                                  (setf (mem-ref staging-base :u64) (+ woff2 written))
                                  (let ((stag (staging-tag buf-addr)))
                                    (set-car cell stag))
                                  (set-cdr cell 0)
                                  (mailbox-enqueue-and-wake target-id cell))))))
                      ;; Enough space
                      (let ((buf-addr (+ (+ staging-base 16) woff)))
                        (let ((written (term-encode message buf-addr)))
                          (setf (mem-ref staging-base :u64) (+ woff written))
                          (let ((stag (staging-tag buf-addr)))
                            (set-car cell stag))
                          (set-cdr cell 0)
                          (mailbox-enqueue-and-wake target-id cell)))))))))))

;; Receive a message, blocking if mailbox is empty.
(defun receive ()
  (spin-lock (sched-lock-addr))
  (let ((cur-id (get-current-actor)))
    (let ((msg (mailbox-dequeue cur-id)))
      (if (not (zerop msg))
          (progn (spin-unlock (sched-lock-addr)) msg)
          ;; Block until message arrives
          (let ((cur-addr (actor-struct-addr cur-id)))
            (if (zerop (save-context (+ cur-addr #x08)))
                ;; Save path: mark blocked, switch
                (progn
                  (actor-set cur-id #x00 4)
                  (actor-set cur-id #x70 (percpu-ref 40))
                  (actor-set cur-id #x78 (percpu-ref 48))
                  (let ((next-id (actor-dequeue)))
                    (if (zerop next-id)
                        ;; NOTHING ELSE TO RUN.  Hand the CPU over WITH THE LOCK
                        ;; STILL HELD — see AP-SCHEDULER-BLOCKED for why the
                        ;; release cannot happen on this side of the call.
                        (ap-scheduler-blocked)
                        (progn
                          (set-current-actor next-id)
                          (actor-set next-id #x00 1)
                          (percpu-set 40 (actor-get next-id #x70))
                          (percpu-set 48 (actor-get next-id #x78))
                          ;; Same hop as YIELD's: this actor blocks (status 4)
                          ;; on its own stack, so its region parks at the SP
                          ;; the save-context above just recorded, and stays
                          ;; collectable from that window while it waits.
                          (actor-region-hop cur-id next-id)
                          (let ((next-addr (actor-struct-addr next-id)))
                            (restore-context (+ next-addr #x08)))))))
                ;; Resumed: dequeue the message that woke us
                (progn
                  (spin-lock (sched-lock-addr))
                  (let ((m (mailbox-dequeue (get-current-actor))))
                    (spin-unlock (sched-lock-addr))
                    m))))))))

;; Non-blocking receive: check mailbox, return message or 0.
;; Used by net-domain to interleave E1000 polling with outbound dispatch.
(defun try-receive ()
  (spin-lock (sched-lock-addr))
  (let ((msg (mailbox-dequeue (get-current-actor))))
    (spin-unlock (sched-lock-addr))
    msg))

;; Receive with timeout (busy-poll via yield)
(defun receive-timeout (max-yields)
  (let ((cur-id (get-current-actor)))
    (spin-lock (sched-lock-addr))
    (let ((msg (mailbox-dequeue cur-id)))
      (spin-unlock (sched-lock-addr))
      (if (not (zerop msg))
          msg
          (let ((count 0))
            (loop
              (yield)
              (spin-lock (sched-lock-addr))
              (let ((m (mailbox-dequeue (get-current-actor))))
                (spin-unlock (sched-lock-addr))
                (if (not (zerop m))
                    (return m)
                    (progn
                      (setq count (+ count 1))
                      (when (= count max-yields)
                        (return 0)))))))))))
