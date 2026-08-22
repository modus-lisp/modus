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
;;; shrunk by (G + 2*S) bytes; the freed top of the FROM-space is the
;;; infrastructure band, and the two S-byte slices above it are the semispace
;;; pairs a per-actor region will eventually use (stage D).  Sizes are adaptive
;;; for the same reason gc.lisp's selftests are: the hosted CLI has 896 MB
;;; semispaces, the bare-metal RPi CL image has 56 MB, and a fixed 16 MB would
;;; simply skip on the small one.

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

(defun %ha-carve ()
  "Carve the hosted actor band out of region 0, ONCE.  Returns the band's raw
   byte address, or 0 if the active region is too small to carve from."
  (if (> *ha-band* 0)
      *ha-band*
      (let ((k (%gc-meta-scale))
            (r0 (%gc-region)))
        (let ((from0 (%gc-meta-read r0 k))
              (to0   (%gc-meta-read (+ r0 #x08) k))
              (size0 (%gc-meta-read (+ r0 #x10) k)))
          (if (< size0 #x1800000)
              0
              (let* ((s (if (< size0 #x8000000) #x200000 #x1000000))
                     (g s)
                     (new0 (- size0 (+ g (* 2 s)))))
                (%gc-region-shrink r0 new0 k)
                (setq *ha-rsize* s)
                (setq *ha-bandsize* g)
                (setq *ha-r1-from* (+ from0 (+ new0 g)))
                (setq *ha-r1-to*   (+ to0   (+ new0 g)))
                (setq *ha-r2-from* (+ from0 (+ new0 (+ g s))))
                (setq *ha-r2-to*   (+ to0   (+ new0 (+ g s))))
                ;; Zero the control head only.  The rest of the band is actor
                ;; stacks and pools that their own initialisers fill; zeroing
                ;; 16 MB here would cost more than it proves.
                (%ha-zero (+ from0 new0) (+ from0 (+ new0 #x1000)))
                (setq *ha-band* (+ from0 new0))
                *ha-band*))))))

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
;;;   +0x100000  coroutine stack TOP (grows down; 960 KB of headroom below it)

(defun %ha-co-stack-top () (+ *ha-band* #x100000))

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
