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
;;; shrunk by 48 MB: the freed top 16 MB of the FROM-space is the
;;; infrastructure band, and the two 16 MB slices above it are the semispace
;;; PAIRS (from0+off, to0+off) a per-actor region uses in step D.
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
(defvar *ha-rsize* 0)     ; per-actor region semispace size
(defvar *ha-r1-from* 0)
(defvar *ha-r1-to* 0)
(defvar *ha-r2-from* 0)
(defvar *ha-r2-to* 0)

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

(defun %ha-carve ()
  "Carve the hosted actor band out of region 0, ONCE.  Returns the band's raw
   byte address, or 0 if the active region is too small to carve from.

   Unlike gc.lisp's selftests this is NOT size-adaptive: the layout above needs
   a band of at least 4.2 MB, and the only image that bakes this file is the
   hosted x86-64 CLI, whose semispaces are 896 MB.  A heap too small for the
   full carve gets an honest 0 (the test prints SKIP) rather than a band whose
   sub-blocks silently overlap."
  (if (> *ha-band* 0)
      *ha-band*
      (let ((k (%gc-meta-scale))
            (r0 (%gc-region)))
        (let ((from0 (%gc-meta-read r0 k))
              (to0   (%gc-meta-read (+ r0 #x08) k))
              (size0 (%gc-meta-read (+ r0 #x10) k)))
          (if (< size0 #x6000000)
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
              (let* ((s #x1000000)
                     (g #x1000000)
                     (new0 (logand (- size0 (+ g (+ (* 2 s) 4096)))
                                   (- 0 4096))))
                (%gc-region-shrink r0 new0 k)
                (setq *ha-rsize* s)
                (setq *ha-bandsize* g)
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
                (setq *ha-r1-from* (%ha-align-up-to-page-base (+ from0 (+ new0 g))))
                (setq *ha-r1-to*   (%ha-align-up-to-page-base (+ to0   (+ new0 g))))
                (setq *ha-r2-from* (+ *ha-r1-from* s))
                (setq *ha-r2-to*   (+ *ha-r1-to* s))
                ;; Zero the control head, the per-CPU block and the actor
                ;; table.  Everything else in the band is stacks and pools that
                ;; their own initialisers fill; zeroing 16 MB would cost more
                ;; than it proves.  The head MUST be zeroed: this memory was
                ;; region 0's from-space a moment ago and can still hold words
                ;; that look like heap pointers.
                (%ha-zero (+ from0 new0) (+ from0 (+ new0 #x5000)))
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
                *ha-band*))))))

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
