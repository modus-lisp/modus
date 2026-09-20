;;;; hosted-actors.lisp — net/actors.lisp's ARCH ADAPTER for HOSTED x86-64.
;;;;
;;;; net/actors.lisp is architecture-independent but has never run anywhere
;;;; except bare metal, because the twelve address hooks it needs
;;;; (actor-table-base, sched-state-base, actor-stack-base, …) were only ever
;;;; supplied by a board file — net/arch-x86.lisp, net/arch-aarch64.lisp — that
;;;; hands out FIXED PHYSICAL ADDRESSES of RAM nobody else owns.  A hosted
;;;; Linux process owns no such RAM: everything it has came from mmap, and the
;;;; one region it can name is its own GC heap.
;;;;
;;;; So this file gets its memory the way mvm/gc.lisp's stage-1/2/3 selftests
;;;; already get theirs: it SHRINKS REGION 0 and uses the top of the two
;;;; semispaces that shrink just freed.  That memory is inside the heap mapping
;;;; (so it is real, mapped, and needs no new mmap), it is above the collector's
;;;; limit (so nothing scans it, nothing copies into it, and a collection cannot
;;;; move anything in it), and its address is DERIVED from the running image
;;;; rather than invented — the one thing a hosted process must not do is
;;;; hard-code 0x06000000 and hope.
;;;;
;;;; WHAT IS HERE, in the order it was built and proven:
;;;;   A. %HA-CTX-SELFTEST — two coroutines driving SAVE-CONTEXT/RESTORE-CONTEXT
;;;;      directly.  translate-x64.lisp implements +op-save-ctx+/+op-restore-ctx+
;;;;      (a real setjmp/longjmp over RSP/RBX/RBP + a continuation RIP), but
;;;;      nothing outside net/actors.lisp has ever called them and net/actors.lisp
;;;;      is in no hosted image, so until this selftest ran, that code had never
;;;;      executed on x86-64 at all.
;;;;   B. %HA-PERCPU-INIT / %HA-PERCPU-SELFTEST — PER-CPU STORAGE.
;;;;      net/actors.lisp reads and writes its current-actor, idle-flag and
;;;;      object-space pointers through PERCPU-REF / PERCPU-SET, which
;;;;      translate-x64.lisp emits as `GS:[disp32]`.  On hosted Linux the GS
;;;;      base is 0, so every one of those touches an absolute low address and
;;;;      SIGSEGVs.  %HA-PERCPU-INIT points the GS base at the band's per-CPU
;;;;      block with arch_prctl(ARCH_SET_GS) — syscall 158, code 0x1001.
;;;;
;;;;      WHY arch_prctl AND NOT A PLAIN-MEMORY OVERRIDE.  Because PERCPU-REF /
;;;;      PERCPU-SET are COMPILER INTRINSICS (mvm/compiler.lisp dispatches on
;;;;      the op-name hash before it ever looks a function up), a `(defun
;;;;      percpu-ref …)' cannot shadow them: overriding would mean editing every
;;;;      percpu call site in net/actors.lisp, i.e. changing the file whose
;;;;      unmodified behaviour is the thing being tested.  Setting the segment
;;;;      base leaves net/actors.lisp untouched and costs one syscall.  It is
;;;;      also the honest emulation: GS: on x64 and TPIDR_EL1 on aarch64 are the
;;;;      same mechanism, so the hosted image exercises the same instructions
;;;;      the bare-metal one does rather than a different code path.
;;;;
;;;;      IT DOES NOT TURN ON *X64-GC-REGION-PERCPU*.  That flag makes the GC's
;;;;      ACTIVE-REGION CELL a GS:-relative read from the collector AND the
;;;;      mutator, and both sides must flip together or they read different
;;;;      cells.  Setting a GS base for the actor system is a necessary
;;;;      condition for that flag, not the same decision; the flag stays off.
;;;;
;;;; WHAT x64's SAVE-CTX DOES NOT SAVE, because it matters to everything above.
;;;; It saves RSP, RBX (V4) and RBP plus the continuation.  It does NOT save
;;;; R12 (alloc ptr), R14 (alloc limit) or R15 (nil) — deliberately: those are
;;;; global mutator state and rolling R12 back would hand the allocator a
;;;; pointer it has already allocated past.  It also does not save V0/V1/V2/V3/
;;;; V5/V6/V7/V8 (RSI/RDI/R8/R9/RCX/RDX/R10/R11), which aarch64's save-ctx
;;;; equivalent does push.  That is SOUND HERE ONLY BECAUSE the compiler keeps
;;;; let-bound locals in FRAME SLOTS (translate-x64.lisp +frame-slot-base+), not
;;;; in vregs, so everything a function is holding across a switch is on the
;;;; stack that RSP restores.  The selftest checks exactly that claim: it holds
;;;; a sentinel fixnum AND a heap cons in locals across every switch.

;;; ============================================================
;;; The carved band
;;; ============================================================
;;;
;;; ONE CARVE, recorded in globals, idempotent.  Region 0's two semispaces are
;;; shrunk: the freed top 16 MB of the FROM-space is the infrastructure band,
;;; and the N 16 MB slices above it are the semispace PAIRS (from0+off,
;;; to0+off) a per-actor — and now a per-THREAD — region uses.
;;;
;;; N REGIONS, NOT TWO.  The carve used to produce exactly two, which was
;;; enough for two actors on two threads and was the reason %MAKE-NATIVE-THREAD
;;; could hand back a thread that could not cons: sixteen thread slots and two
;;; heaps.  It now produces one per thread slot (%HA-MAX-REGIONS), each with its
;;; own semispace pair, its own control block, its own root window and its own
;;; collection count.  Region i's from-space is *HA-RBASE-FROM* + i*rsize and
;;; its control block is band +0xA000 + i*0x40.
;;;
;;; IT IS ADAPTIVE IN COUNT AND NOT IN SIZE.  A region is 16 MB or it is not
;;; carved; what varies with the heap is HOW MANY.  %HA-FIT-REGIONS never
;;; returns fewer than two, so an image whose semispaces only ever afforded the
;;; historic pair carves exactly the historic pair at exactly the historic
;;; addresses — *HA-R1-FROM* and *HA-R2-FROM* are still regions 0 and 1 — and
;;; every test written against those two is unchanged.  The shipping hosted
;;; x86-64 CLI has 896 MB semispaces and gets all sixteen.
;;;
;;; BAND LAYOUT — every address net/actors.lisp asks for, as an offset:
;;;   +0x000000  control head (4 KB)
;;;     +0x0000  context save area A (the driver's)          — step A
;;;     +0x0040  context save area B (the coroutine's)       — step A
;;;     +0x0080  machine-word scratch (%GC-WORD-OF)
;;;     +0x0088  coroutine progress counter                  — step A
;;;     +0x0090  driver progress counter                     — step A
;;;     +0x00A0  step-A result block (0x60)
;;;     +0x0100  SCHED-LOCK-ADDR      (3 words)
;;;     +0x0140  SCHED-STATE-BASE     (0x28)
;;;     +0x0180  SCRATCH-ADDR         (1 word)
;;;     +0x0188  DECODE-PTR-ADDR      (1 word)
;;;     +0x0190  hosted AP-SCHEDULER entry counter
;;;     +0x0200  region control block for actor 2            — step D
;;;     +0x0240  region control block for actor 3            — step D
;;;     +0x0280  POOL-STATE-BASE      (0x18)
;;;     +0x0400  step-B / step-C / step-D result block (0x400)
;;;     +0x0800  message log (0x800)
;;;   +0x001000  PERCPU-DATA-BASE (16 KB; GS base points here)
;;;   +0x005000  the two-thread selftest's thread block (net/hosted-actors-post)
;;;   +0x006000  cpu 1's per-CPU block for that selftest (16 KB)
;;;   +0x00A000  PER-THREAD REGION CONTROL BLOCKS — 16 x 64 = 0x400 bytes, one
;;;              per thread slot, %HA-RCB.  In the band and not in the thread
;;;              page because a region control block is memory the COLLECTOR
;;;              reads on every allocation check, and the band is the one
;;;              mapping this file already guarantees is live and never moves.
;;;   +0x00B000  per-thread REPORT BLOCKS for the N-regions acceptance test
;;;   +0x00C000  that test's result block (0x800), then %SYNC-CELL-CTL at
;;;              +0xC800 — the mutex/condvar arena's bump lock and pointers.
;;;              ZEROED BY THE CARVE, and it must be: its first word is a spin
;;;              lock and this memory was region 0's from-space a moment ago.
;;;   +0x010000  ACTOR-TABLE-BASE (64 x 128 = 0x2000)
;;;   +0x012000  GC SCRATCH ARRAY (16 x 32 = 0x200) — mvm/gc.lisp's
;;;              per-collection working state, one 32-byte block PER CPU.
;;;              Installed by %HA-CARVE via %GC-SCRATCH-INIT; until something
;;;              does that, the collector uses the historic single shared block
;;;              at 0x10000100, which is one collecting thread by construction.
;;;   +0x020000  MAILBOX-POOL-BASE .. +0x040000 MAILBOX-POOL-LIMIT
;;;   +0x080000  STAGING-BASE-ADDR (64 x 16 KB = 0x100000)
;;;   +0x200000  ACTOR-STACK-BASE (actor N's stack top = +0x200000 + (N+1)*64 KB)
;;;              The id-0 slot [+0x200000,+0x210000) is never an actor's (ids
;;;              start at 1), so step A's coroutine borrows it.
;;;   +0x400000  ACTOR-HEAP-BASE — see the note on ACTOR-HEAP-BASE below.

(defvar *ha-band* 0)      ; raw byte address of the infrastructure band, 0 = uncarved
(defvar *ha-bandsize* 0)  ; its length in bytes
(defvar *ha-rsize* 0)     ; per-region semispace size
(defvar *ha-nregions* 0)  ; how many per-thread regions the carve produced
(defvar *ha-rbase-from* 0) ; region i's from-space = *ha-rbase-from* + i * *ha-rsize*
(defvar *ha-rbase-to* 0)   ; region i's to-space   = *ha-rbase-to*   + i * *ha-rsize*
;; REGIONS 0 AND 1 UNDER THEIR OLD NAMES.  Every test and every selftest written
;; before the carve went to N spells the first two regions this way, and they are
;; still literally the first two slices — so these are aliases, not a parallel
;; allocation.  New code should use %HA-REGION-FROM / %HA-REGION-TO.
(defvar *ha-r1-from* 0)
(defvar *ha-r1-to* 0)
(defvar *ha-r2-from* 0)
(defvar *ha-r2-to* 0)
;; THE CARVE'S OWN LEDGER.  Both are counts, not flags, so "it happened once and
;; nobody noticed" is distinguishable from "it never happened" — the same reason
;; %HA-ALIGN-CONTROL exists.  See %HA-CARVE-ROOM.
(defvar *ha-carve-collections* 0)  ; forced collections the carve needed to fit
(defvar *ha-carve-refusals* 0)     ; carves declined because live data was in the way

(defun %ha-zero (start end)
  "Zero [START,END) a machine word at a time.  Raw byte addresses."
  (let ((a start))
    (loop
      (when (>= a end) (return 0))
      (setf (mem-ref a :u64) 0)
      (setq a (+ a 8)))))

(defun %ha-align-up-to-page-base (a)
  "A rounded UP to the next address congruent to the bitmap page_base modulo
   mvm/gc.lisp's region alignment (1024).  That congruence — not plain
   1024-alignment — is what the bitmap needs, because the granule index every
   BTS uses is (addr - page_base) >> 4.  Identity when the bitmaps are off."
  (if (= (%gc-bitmap-base) 0)
      a
      (let ((d (logand (- (logand (%gc-bitmap-page-base-exact) 1023)
                          (logand a 1023))
                       1023)))
        (+ a d))))

(defun %ha-max-regions ()
  "How many per-thread GC regions the carve will produce when the heap can
   afford them.  ONE PER THREAD SLOT — net/hosted-sync.lisp's %THR-MAX-THREADS
   is defined as this number, so a thread slot without a region cannot exist by
   construction rather than by two constants agreeing."
  16)

(defun %ha-fit-regions (size0 g s maxn)
  "How many S-byte region pairs a semispace of SIZE0 can afford after the
   G-byte band and 4096 bytes of alignment headroom, while leaving region 0 at
   least HALF of what it had.  Never more than MAXN.

   NEVER FEWER THAN TWO, and that floor is the compatibility guarantee rather
   than a rounding convenience: every image before this carve went to N shrank
   region 0 by exactly (G + 2S + 4096) and put its two regions at exactly those
   two addresses.  An image whose heap only affords two must keep doing exactly
   that, so the arithmetic below must produce 2 — not 1, and not 0 — wherever
   the old code produced its pair.  The 96 MB floor in %HA-CARVE is what stops
   a heap that affords NEITHER from getting a band at all.

   Repeated addition rather than a division: N is at most 16, and this runs
   during the carve, where the arithmetic must not be able to allocate."
  (let ((room (- size0 (ash size0 -1)))
        (used (+ g 4096))
        (n 0))
    (loop
      (when (>= n maxn) (return 0))
      (when (> (+ used s) room) (return 0))
      (setq used (+ used s))
      (setq n (+ n 1)))
    (if (< n 2) 2 n)))

(defun %ha-carve-new0 (size0)
  "The byte offset into a semispace of SIZE0 at which the carve begins — the
   band base, and the new size of region 0.  Split out of %HA-CARVE because
   %HA-CARVE-ROOM has to know the answer BEFORE the carve commits to it."
  (let* ((s #x1000000)
         (g #x1000000)
         (n (%ha-fit-regions size0 g s (%ha-max-regions))))
    (logand (- size0 (+ g (+ (* n s) 4096))) (- 0 4096))))

(defun %ha-carve-room (from0 size0)
  "MAKE ROOM FOR THE CARVE, OR SAY THERE IS NONE.  Returns 1 if region 0's live
   allocation frontier is at or below the carve point (so the carve takes only
   memory nothing is using), and 0 if it is not.

   THE CARVE IS NOT FREE MEMORY UNLESS NOTHING HAS REACHED IT YET, and nothing
   used to check.  %HA-CARVE takes the top ~272 MB of an 896 MB semispace and
   then ZEROES four ranges inside it (the control head, the region control
   blocks, the upper scratch, the actor table).  Called at boot — which is what
   every selftest in this file does, via %HA-PERCPU-INIT on its first line —
   the allocation frontier is nowhere near that and the memory really is spare.
   Called LATE it is not spare at all: it is live heap, and the zeroing lands on
   whatever objects are there.

   THAT IS NOT HYPOTHETICAL.  The carve is reached from %HA-BASE, which is
   reached from %SYNC-CELL-CTL, which is reached from the FIRST
   `sb-thread:make-mutex' a program executes — and a program asks for its first
   lock whenever it happens to want one, not at boot.  Loading cram's five files
   and glass's first three under `./modus' and then asking for one mutex put the
   frontier past the carve point, and the zeroing destroyed live objects: the
   keyword :NAME came back with a wrecked header, so SYMBOLP said NIL, so the
   compiler's terminal arm reported `WARN: cannot compile #<?>, using nil' on
   the NEXT form read, and the TYPE-ERROR that followed had a condition object
   whose own type-name slot was garbage.  An earlier round saw one face of this
   — a non-zero word at +0xC800 read as an already-held spin lock, hanging the
   first MAKE-MUTEX at a full core — and answered it by zeroing that range too,
   which cures the hang by widening the damage.  The zeroing was never the bug.

   SO: COLLECT FIRST, THEN RE-ASK.  A Cheney collection compacts the live set to
   the bottom of the (new) from-space, which is exactly the shape that makes the
   top spare again, and it is the region's ordinary collector rather than a side
   door.  If the frontier is still past the carve point afterwards — a program
   with more than ~620 MB genuinely live — the answer is 0 and %HA-CARVE returns
   an honest 0, the same answer a heap too small to carve from already gets.
   A refusal is a mutex the caller cannot have; a silent carve is a heap the
   caller cannot trust.

   CALLERS RE-READ from0/to0 AFTER THIS.  The collection FLIPS the semispaces,
   so every field read before it is stale on the way out.

   Degrades to the historic behaviour when the frontier cannot be read: a
   VA of 0, or one outside [from0, from0+size0), means this is not a live
   Cheney region (bare metal, an uninitialised image) and the check abstains."
  (let ((va (get-alloc-ptr)))
    (if (or (= va 0) (< va from0) (> va (+ from0 size0)))
        1
        (if (<= va (+ from0 (%ha-carve-new0 size0)))
            1
            (progn
              (setq *ha-carve-collections* (+ *ha-carve-collections* 1))
              (%gc-collect-here)
              ;; The flip moved everything: re-read the region's own fields.
              (let* ((k2 (%gc-meta-scale))
                     (r2 (%gc-region))
                     (f2 (%gc-meta-read r2 k2))
                     (s2 (%gc-meta-read (+ r2 #x10) k2))
                     (v2 (get-alloc-ptr)))
                (if (<= v2 (+ f2 (%ha-carve-new0 s2)))
                    1
                    (progn (setq *ha-carve-refusals* (+ *ha-carve-refusals* 1))
                           0))))))))

(defun %ha-carve ()
  "Carve the hosted actor band and the per-thread GC regions out of region 0,
   ONCE.  Returns the band's raw byte address, or 0 if the active region is too
   small to carve from, or if its LIVE DATA is in the way (see %HA-CARVE-ROOM).

   The band is NOT size-adaptive: the layout above needs at least 4.2 MB, and
   the only image that bakes this file is the hosted x86-64 CLI, whose
   semispaces are 896 MB.  A heap too small for the full carve gets an honest 0
   (the test prints SKIP) rather than a band whose sub-blocks silently overlap.
   The REGION COUNT is adaptive; see %HA-FIT-REGIONS."
  (if (> *ha-band* 0)
      *ha-band*
      (let ((k (%gc-meta-scale))
            (r0 (%gc-region)))
        ;; ROOM FIRST — it may COLLECT, which flips the semispaces, so every
        ;; field below must be read after it and not before.
        (let ((room (%ha-carve-room (%gc-meta-read r0 k)
                                    (%gc-meta-read (+ r0 #x10) k))))
        (let ((from0 (%gc-meta-read (%gc-region) k))
              (to0   (%gc-meta-read (+ (%gc-region) #x08) k))
              (size0 (%gc-meta-read (+ (%gc-region) #x10) k)))
          (if (or (= room 0) (< size0 #x6000000))
              0
              ;; NEW0 is page-aligned DOWN so the band base inherits the
              ;; semispace base's alignment.  net/actors.lisp's mailbox pool
              ;; hands out 16-byte cells and TAGS them as conses
              ;; (`(logior (untag ptr) (untag 1))'), so a band base whose low
              ;; nibble was not 0 would produce pointers whose tag bits are
              ;; part of the address.
              ;; The extra 4096 is HEADROOM, and it is not optional: the two
              ;; carved region bases are rounded UP to the bitmap alignment
              ;; (%HA-ALIGN-UP-TO-PAGE-BASE, up to 1023 bytes each), and
              ;; without it NEW0's own 4096-rounding can leave a slack of
              ;; exactly zero and the top region would run off the semispace.
              ;; NEW0 comes from %HA-CARVE-NEW0 and NOT from a second copy of
              ;; this arithmetic: %HA-CARVE-ROOM checked the frontier against
              ;; that function's answer, and two spellings that could drift
              ;; would make the check guard an address the carve does not use.
              (let* ((s #x1000000)
                     (g #x1000000)
                     (n (%ha-fit-regions size0 g s (%ha-max-regions)))
                     (new0 (%ha-carve-new0 size0)))
                ;; R0 was read before %HA-CARVE-ROOM; a collection there does
                ;; not move the CONTROL BLOCK (only the semispaces flip), but
                ;; shrink the block the fields above actually came from.
                (%gc-region-shrink (%gc-region) new0 k)
                (setq *ha-rsize* s)
                (setq *ha-bandsize* g)
                (setq *ha-nregions* n)
                ;; THE CARVED REGIONS MUST BE 1024-BYTE ALIGNED RELATIVE TO THE
                ;; BITMAP page_base, or two threads collecting at once share a
                ;; bitmap read-modify-write word: translate-x64's
                ;; `BTS [base], idx' at 64-bit operand size touches the
                ;; EIGHT-BYTE unit at base + 8*(idx >> 6) — 1024 heap bytes —
                ;; with no LOCK prefix.  See mvm/gc.lisp %GC-REGION-ALIGN-CHECK.
                ;;
                ;; MEASURED: the TO side was 512 off.  NEW0 and G are
                ;; 4096-multiples, so the from side inherited FROM0's
                ;; congruence; the to side adds SIZE0 as well, and region 0's
                ;; space_size is 939523584 = 512 mod 1024 on this layout — so
                ;; both carved to-spaces came out 512 off and their shared
                ;; boundary sat in the middle of a BTS unit.
                ;;
                ;; ALIGNMENT IS AGAINST page_base, NOT AGAINST FROM0.  FROM0 is
                ;; whichever semispace is CURRENTLY the from-space, and it swaps
                ;; on every collection, so after an odd number of collections
                ;; the from side is the one that is 512 off.  page_base is fixed
                ;; for the life of the image, which is exactly what the bitmap
                ;; granule index is computed from.  Both sides are rounded UP
                ;; into the 4 KB of headroom NEW0 reserves below.
                ;;
                ;; ROUNDING THE TWO BASES IS ENOUGH FOR ALL N.  The slices are
                ;; S = 16 MB apart and S is a multiple of 1024, so every later
                ;; region inherits the base's congruence exactly.  That is why
                ;; the headroom stays 4096 and does not grow with N.
                (setq *ha-rbase-from* (%ha-align-up-to-page-base (+ from0 (+ new0 g))))
                (setq *ha-rbase-to*   (%ha-align-up-to-page-base (+ to0   (+ new0 g))))
                (setq *ha-r1-from* *ha-rbase-from*)
                (setq *ha-r1-to*   *ha-rbase-to*)
                (setq *ha-r2-from* (+ *ha-rbase-from* s))
                (setq *ha-r2-to*   (+ *ha-rbase-to* s))
                ;; Zero the control head, the per-CPU block and the actor
                ;; table.  Everything else in the band is stacks and pools that
                ;; their own initialisers fill; zeroing 16 MB would cost more
                ;; than it proves.  The head MUST be zeroed: this memory was
                ;; region 0's from-space a moment ago and can still hold words
                ;; that look like heap pointers.
                (%ha-zero (+ from0 new0) (+ from0 (+ new0 #x5000)))
                ;; THE PER-THREAD REGION CONTROL BLOCKS.  %GC-REGION-INIT writes
                ;; every one of a block's eight fields, so zeroing is not what
                ;; makes an INITIALISED block correct — it is what makes an
                ;; UNINITIALISED one readable: %THR-TRAMPOLINE tests its slot's
                ;; block-address word for zero to decide whether it was given a
                ;; region, and this memory was region 0's from-space a moment
                ;; ago and can still hold anything.
                (%ha-zero (+ from0 (+ new0 #xA000))
                          (+ from0 (+ new0 #xA400)))
                ;; AND THE BAND'S UPPER SCRATCH, +0xC000..+0xD000.  It holds
                ;; the N-region selftest's result block and — the reason this
                ;; line exists — %SYNC-CELL-CTL at +0xC800, whose FIRST WORD IS
                ;; A SPIN LOCK.  This memory was region 0's from-space a moment
                ;; ago, so a non-zero word there is an ALREADY-HELD lock that
                ;; nothing will ever release: the first MAKE-MUTEX spins at a
                ;; full core forever.
                ;;
                ;; MEASURED, AND IT IS WHY THIS IS NOT A TIDY-UP.  Loading
                ;; glass's clipboard.lisp under modus hung at 100% of one core
                ;; on `(defvar *session-clipboard-lock* (%clip-make-lock))',
                ;; the first form in the whole campaign to ask for a mutex
                ;; AFTER enough allocation had happened to leave garbage at that
                ;; address.  Every earlier test asked for one from a nearly
                ;; fresh heap and got a zero by luck.
                (%ha-zero (+ from0 (+ new0 #xC000))
                          (+ from0 (+ new0 #xD000)))
                (%ha-zero (+ from0 (+ new0 #x10000))
                          (+ from0 (+ new0 #x12200)))
                (setq *ha-band* (+ from0 new0))
                ;; THE COLLECTOR'S PER-COLLECTION STATE BECOMES PER CPU.  Until
                ;; this runs, mvm/gc.lisp's three working words are the historic
                ;; FIXED SHARED addresses 0x10000100/0x10000108/0x10000110 — one
                ;; collecting thread by construction, and two threads collecting
                ;; at once clobber each other on every forwarded slot.  The
                ;; per-CPU form is additionally gated on the active-region
                ;; per-CPU word (%HA-SET-PERCPU-MODE), because indexing by CPU
                ;; means a GS-relative read and a hosted process starts with a
                ;; GS base of 0.
                (%gc-scratch-init (+ *ha-band* #x12000))
                *ha-band*)))))))

;;; ============================================================
;;; STEP B — per-CPU storage: the GS base
;;; ============================================================

(defvar *ha-gs-base* 0)   ; the address arch_prctl was last given; 0 = never set

;; CARVE-ON-DEMAND.  Every address hook below goes through this rather than
;; reading *HA-BAND* directly, so that a call into net/actors.lisp before
;; anything set the system up computes a REAL address instead of dereferencing
;; a small offset from zero.  Nothing in the shipping hosted image calls YIELD,
;; SEND, RECEIVE, LINK or SHUTDOWN — those names are defined nowhere else in
;; the hosted blob and called nowhere in it — so in practice the carve happens
;; when a selftest asks for it.  But the failure mode of being WRONG about that
;; should be a wasted 48 MB of semispace, not a SIGSEGV inside the scheduler.
(defun %ha-base ()
  (if (zerop *ha-band*) (%ha-carve) *ha-band*))

(defun %ha-percpu-base () (+ (%ha-base) #x1000))

(defun %ha-percpu-init ()
  "Point this thread's GS base at the band's per-CPU block, so that
   PERCPU-REF / PERCPU-SET — which translate-x64.lisp emits as GS:[disp32] —
   address real memory instead of absolute low addresses.

   arch_prctl(ARCH_SET_GS = 0x1001, base) is syscall 158 on x86-64.  Returns
   the raw kernel return value: 0 on success, a negative errno otherwise.
   Returns -1 without syscalling if the band could not be carved.

   IDEMPOTENT AND CHEAP.  Re-setting the same base is a no-op to the kernel;
   this is called at the top of every selftest below rather than being a boot
   hook, because a shipping ./modus that never touches an actor has no reason
   to carry a non-zero GS base."
  (if (zerop (%ha-carve))
      -1
      (let ((r (syscall3 158 #x1001 (%ha-percpu-base) 0)))
        (if (zerop r) (setq *ha-gs-base* (%ha-percpu-base)) 0)
        r)))

(defun %ha-percpu-selftest ()
  "STEP-B ACCEPTANCE.  Set the GS base, then write and read back EVERY per-CPU
   slot net/actors.lisp uses — 0 self-ptr, 8 reduction, 16 cpu-id, 24
   current-actor, 32 idle-flag, 40 obj-alloc, 48 obj-limit, 56 idle-stack-top —
   through PERCPU-SET / PERCPU-REF, and cross-check the underlying memory.

   THE MEMORY CROSS-CHECK IS THE POINT.  A read-back that only round-trips
   proves nothing: it would also pass if GS: happened to address some other
   mapped page.  PERCPU-SET stores the value STILL TAGGED (compile-percpu-set
   does not untag), so the machine word at the block + offset must be exactly
   2*value.  Reading that word with %GC-READ64 — which does not go through GS
   at all — is what ties the segment base to the block this file owns.

   Returns the result block's raw address, or 0 if the band could not be
   carved.  Slot j's evidence is at res + 0x20 + j*16 (read-back) and
   res + 0x28 + j*16 (the machine word), with j the slot INDEX 0..7 and the
   byte offset 8*j.  Values written are 700000 + j."
  (let ((rc (%ha-percpu-init))
        ;; THE :CPU-ID SLOT IS ABOUT TO HOLD 700002, and if the per-CPU
        ;; active-region mode were ON that would make %GC-REGION-CELL index
        ;; 700002 entries off a 16-entry table.  Nothing between here and the
        ;; block being zeroed again allocates, so no collection can read it —
        ;; but "nothing allocates" is a property of this function's body, not of
        ;; the mechanism, so the mode word is turned off for the duration and
        ;; put back.  It is zero in any image that has not started a thread.
        (savemode (mem-ref #x10000FF8 :u32)))
    (setf (mem-ref #x10000FF8 :u32) 0)
    (if (zerop (%ha-carve))
        0
        (let ((res (+ *ha-band* #x400))
              (pb (%ha-percpu-base)))
          (%ha-zero res (+ res #x100))
          (%gc-write64 res (if (zerop rc) 0 1))
          (%gc-write64 (+ res #x08) pb)
          (%gc-write64 (+ res #x10) *ha-gs-base*)
          ;; ---- write every slot ----
          ;; PERCPU-SET needs a CONSTANT offset (it becomes a disp32 in the
          ;; instruction), so the eight slots are unrolled, not looped.
          (percpu-set 0  700000)
          (percpu-set 8  700001)
          (percpu-set 16 700002)
          (percpu-set 24 700003)
          (percpu-set 32 700004)
          (percpu-set 40 700005)
          (percpu-set 48 700006)
          (percpu-set 56 700007)
          ;; ---- read every slot back, and look at the memory underneath ----
          (%gc-write64 (+ res #x20) (percpu-ref 0))
          (%gc-write64 (+ res #x28) (%gc-read64 (+ pb 0)))
          (%gc-write64 (+ res #x30) (percpu-ref 8))
          (%gc-write64 (+ res #x38) (%gc-read64 (+ pb 8)))
          (%gc-write64 (+ res #x40) (percpu-ref 16))
          (%gc-write64 (+ res #x48) (%gc-read64 (+ pb 16)))
          (%gc-write64 (+ res #x50) (percpu-ref 24))
          (%gc-write64 (+ res #x58) (%gc-read64 (+ pb 24)))
          (%gc-write64 (+ res #x60) (percpu-ref 32))
          (%gc-write64 (+ res #x68) (%gc-read64 (+ pb 32)))
          (%gc-write64 (+ res #x70) (percpu-ref 40))
          (%gc-write64 (+ res #x78) (%gc-read64 (+ pb 40)))
          (%gc-write64 (+ res #x80) (percpu-ref 48))
          (%gc-write64 (+ res #x88) (%gc-read64 (+ pb 48)))
          (%gc-write64 (+ res #x90) (percpu-ref 56))
          (%gc-write64 (+ res #x98) (%gc-read64 (+ pb 56)))
          ;; ---- a value BIG enough to be a real pointer, not a small int ----
          ;; obj-alloc/obj-limit carry heap addresses in net/actors.lisp, and a
          ;; 47-bit hosted address is where a sign-extension or a lost high
          ;; half would show up.  Use the live allocation pointer.
          (percpu-set 40 (get-alloc-ptr))
          (%gc-write64 (+ res #xA0) (percpu-ref 40))
          (%gc-write64 (+ res #xA8) (get-alloc-ptr))
          ;; ---- leave the block as net/actors.lisp expects to find it ----
          (%ha-zero pb (+ pb 64))
          (setf (mem-ref #x10000FF8 :u32) savemode)
          res))))

;;; ============================================================
;;; STEP A — the context switch itself
;;; ============================================================
;;;
;;; SAVE-AREA LAYOUT AS translate-x64.lisp WRITES IT (offsets from the address
;;; handed to SAVE-CONTEXT / RESTORE-CONTEXT):
;;;   +0x00 RSP   +0x18 RBX   +0x28 continuation RIP   +0x38 RBP
;;; net/actors.lisp passes (actor-struct-addr id) + 0x08, so those land on the
;;; actor struct at +0x08 / +0x20 / +0x30 / +0x40 respectively.  The two save
;;; areas here are standalone 64-byte blocks in the band's control head.
;;;
;;; Control head layout (offsets from *ha-band*):
;;;   +0x000  save area A — the DRIVER's (whoever called %ha-ctx-selftest)
;;;   +0x040  save area B — the COROUTINE's
;;;   +0x080  scratch word (machine-word inspection via %gc-word-of)
;;;   +0x088  coroutine progress counter
;;;   +0x090  driver progress counter
;;;   +0x0A0  result block
;;; The coroutine's stack TOP is the actor-stack slot for id 0 — an id no actor
;;; ever gets (net/actors.lisp's ACTOR-COUNT starts at 2 and the primordial
;;; actor is 1), so borrowing it cannot collide with a real actor's stack.

(defun %ha-co-stack-top () (+ *ha-band* #x210000))

;; THE COROUTINE.  Never returns.  It is entered the first time by
;; RESTORE-CONTEXT jumping to its native entry point with RSP = its own stack
;; top; every later entry resumes it at the SAVE-CONTEXT below.  Its LOCALS are
;; bound before the first switch and read after every later one, which is the
;; coroutine half of "values living across a switch are intact".
(defun %ha-co-body ()
  (let ((a *ha-band*)
        (b (+ *ha-band* #x40))
        (cc (+ *ha-band* #x88))
        (sentinel 305419896))
    (loop
      ;; Progress, and a check that our own frame survived the last switch.
      (if (= sentinel 305419896)
          (%gc-write64 cc (+ (%gc-read64 cc) 1))
          (%gc-write64 cc 0))
      (if (zerop (save-context b))
          (restore-context a)
          0))))

(defun %ha-ctx-selftest (n)
  "STEP-A ACCEPTANCE.  Switch N times between this function and %HA-CO-BODY
   using nothing but SAVE-CONTEXT / RESTORE-CONTEXT, and write the evidence
   into the carved band.  Returns the result block's raw byte address, or 0 if
   the region could not be carved.

   Every number is read back out of memory afterwards; this function asserts
   nothing itself.  What it records:
     res+0x00  N, echoed
     res+0x08  driver progress count   (must be N)
     res+0x10  coroutine progress count (must be N)
     res+0x18  resume count — times SAVE-CONTEXT returned NON-zero (must be N)
     res+0x20  sentinel mismatches across switches (must be 0)
     res+0x28  heap-cons mismatches across switches (must be 0)
     res+0x30  the RSP save-ctx recorded for the driver (must be a real stack)
     res+0x38  the coroutine's recorded RSP (must be inside its own stack)
     res+0x40  the continuation RIP save-ctx recorded for the driver
     res+0x48  the coroutine stack top it was launched on
     res+0x50  region-0 collection count before the switching
     res+0x58  region-0 collection count after  (must be equal — a collection
               here would scan from a band stack up to the process stack base)"
  (if (zerop (%ha-carve))
      0
      (let* ((band *ha-band*)
             (a band)
             (b (+ band #x40))
             (sa (+ band #x80))
             (cc (+ band #x88))
             (mc (+ band #x90))
             (res (+ band #xA0))
             (stk (%ha-co-stack-top))
             (k (%gc-meta-scale))
             (g0 (%gc-meta-read (+ (%gc-region) #x20) k))
             (i 0)
             (resumes 0)
             (badsent 0)
             (badcons 0)
             (sentinel 305419896)
             (witness (cons 12345 67890)))
        ;; ---- both save areas start clean ----
        (%ha-zero a (+ b #x40))
        (%gc-write64 cc 0)
        (%gc-write64 mc 0)
        ;; ---- launch state for the coroutine ----
        ;; RSP = its own stack top; RBX and RBP zero; the continuation is
        ;; %HA-CO-BODY's native entry.  (FN-ADDR …) yields the entry OR-3
        ;; tagged (translate-x64.lisp's mvm-fn-addr), and RESTORE-CTX jumps to
        ;; the word verbatim, so the tag has to come back off.
        (%gc-write64 b stk)
        (%gc-write64 (+ b #x18) 0)
        (%gc-write64 (+ b #x28) (- (%gc-word-of (fn-addr %ha-co-body) sa) 3))
        (%gc-write64 (+ b #x38) 0)
        ;; ---- N round trips ----
        (loop
          (when (>= i n) (return 0))
          (%gc-write64 mc (+ (%gc-read64 mc) 1))
          ;; THE SWITCH.  Zero on the save, non-zero when the coroutine
          ;; RESTORE-CONTEXTs back into us.
          (let ((r (save-context a)))
            (if (zerop r)
                (restore-context b)
                (setq resumes (+ resumes 1))))
          ;; Back on our own stack.  Everything below reads locals bound
          ;; BEFORE the switch: a fixnum, a loop counter and a heap cons.
          (if (= sentinel 305419896) 0 (setq badsent (+ badsent 1)))
          (if (consp witness)
              (if (= (car witness) 12345)
                  (if (= (cdr witness) 67890) 0 (setq badcons (+ badcons 1)))
                  (setq badcons (+ badcons 1)))
              (setq badcons (+ badcons 1)))
          (setq i (+ i 1)))
        ;; ---- evidence ----
        (%gc-write64 res n)
        (%gc-write64 (+ res #x08) (%gc-read64 mc))
        (%gc-write64 (+ res #x10) (%gc-read64 cc))
        (%gc-write64 (+ res #x18) resumes)
        (%gc-write64 (+ res #x20) badsent)
        (%gc-write64 (+ res #x28) badcons)
        (%gc-write64 (+ res #x30) (%gc-read64 a))
        (%gc-write64 (+ res #x38) (%gc-read64 b))
        (%gc-write64 (+ res #x40) (%gc-read64 (+ a #x28)))
        (%gc-write64 (+ res #x48) stk)
        (%gc-write64 (+ res #x50) g0)
        (%gc-write64 (+ res #x58) (%gc-meta-read (+ (%gc-region) #x20) k))
        res)))

;;; ============================================================
;;; STEP C — net/actors.lisp's twelve address hooks
;;; ============================================================
;;;
;;; This is the whole of what net/arch-x86.lisp's "Actor system address hooks"
;;; block does for bare metal, except that not one of these numbers is a
;;; constant: they are offsets into a band whose base the image DERIVED at
;;; runtime by shrinking its own heap.  A hosted process may not invent
;;; 0x06000000 — that address belongs to whatever the kernel put there.
;;;
;;; PERCPU-DATA-BASE is also the GS base (step B), so PERCPU-REF/-SET reach the
;;; same block SMP-INIT initialises through MEM-REF.  That equality is checked
;;; by test/hosted-percpu.lisp, not assumed here.
;;;
;;; ACTOR-HEAP-BASE, honestly.  ACTOR-SPAWN writes a per-actor bump-allocator
;;; range into the struct at +0x10/+0x18 and an object-space range at
;;; +0x70/+0x78, 4 MB per actor.  On x86-64 NOTHING DEREFERENCES THOSE: x64's
;;; SAVE-CTX/RESTORE-CTX deliberately do not touch R12/R14 (see the header),
;;; and the hosted image's allocator is region 0's, not percpu 40/48.  Only
;;; aarch64's RESTORE-CTX reloads x24/x25 from +0x10/+0x18.  So this hook exists
;;; to make ACTOR-SPAWN's arithmetic land on mapped memory rather than to hand
;;; out heaps; the band is 16 MB, so ids past 4 name addresses above it, which
;;; is harmless precisely because they are never dereferenced — and would NOT be
;;; harmless on aarch64.  Where an actor's heap really comes from on x64 is its
;;; GC REGION, which is step D.

;;; ============================================================
;;; THE PER-THREAD REGIONS, BY INDEX
;;; ============================================================
;;;
;;; Three functions and one rule: REGION I BELONGS TO THREAD SLOT I.  The
;;; mapping is the identity on purpose — a thread's slot is what it reads out of
;;; the spawn handshake before it has a window, a per-CPU block or a heap, so it
;;; is the one number a starting thread is certain of, and deriving its region
;;; from anything else would mean a lookup it cannot yet do.
;;;
;;; SLOT 0 IS THE MAIN THREAD'S AND ITS REGION IS SPARE.  The main thread runs
;;; on the process stack and allocates in region 0 — that is what every image
;;; has always done and what %RT-ENTER depends on — so region 0 of the carve is
;;; never adopted by %THR-TRAMPOLINE.  It is carved anyway, because indexing by
;;; slot with a hole at 0 is a subtraction waiting to be got wrong, and because
;;; it is what the two-actor selftests have always used as *HA-R1-FROM*.

(defun %ha-nregions ()
  "How many per-thread regions this image carved.  0 before the carve."
  *ha-nregions*)

(defun %ha-region-from (i) (+ *ha-rbase-from* (* i *ha-rsize*)))
(defun %ha-region-to (i)   (+ *ha-rbase-to*   (* i *ha-rsize*)))

(defun %ha-rcb (i)
  "Raw byte address of thread slot I's 64-byte region control block."
  (+ (%ha-base) (+ #xA000 (* i #x40))))

(defun percpu-data-base ()   (+ (%ha-base) #x1000))

;;; THE ONE HOOK THAT IS NOT AN OFFSET INTO THE BAND, and it is not an
;;; inconsistency.  net/actors.lisp hands the scheduler lock's RELEASE to
;;; RESTORE-CONTEXT (YIELD's resume arm is commented "lock already released by
;;; restore-context"), and a context switch releases it from INSIDE the
;;; instruction stream — translate-x64's +OP-RESTORE-CTX+ stores zero to
;;; *X64-SCHED-LOCK-ADDR*, exactly as translate-aarch64 has always stored zero
;;; to *AARCH64-SCHED-LOCK-ADDR*.  That address is baked in at TRANSLATE time,
;;; so it cannot be a number this image works out at RUNTIME by carving its own
;;; heap.  It is therefore a fixed BSS word, +HOSTED-SCHED-LOCK-ADDR+ in
;;; mvm/compiler.lisp, in the same documented gap the per-region table lives in;
;;; BSS zero-fill means it starts UNLOCKED with nothing having to initialise it.
;;; mvm/build-generic-cli.lisp ratchets this literal against that constant, so
;;; the two cannot drift apart silently — a drift would deadlock on the first
;;; context switch.
(defun sched-lock-addr ()    #x10000FC0)
(defun sched-state-base ()   (+ (%ha-base) #x140))
(defun scratch-addr ()       (+ (%ha-base) #x180))
(defun decode-ptr-addr ()    (+ (%ha-base) #x188))
(defun pool-state-base ()    (+ (%ha-base) #x280))
(defun actor-table-base ()   (+ (%ha-base) #x10000))
(defun mailbox-pool-base ()  (+ (%ha-base) #x20000))
(defun mailbox-pool-limit () (+ (%ha-base) #x40000))
(defun staging-base-addr ()  (+ (%ha-base) #x80000))
(defun actor-stack-base ()   (+ (%ha-base) #x200000))
(defun actor-heap-base ()    (+ (%ha-base) #x400000))
