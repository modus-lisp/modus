;;;; glass-tx-cell.lisp — THE WALL IS A HEAP OVERWRITE, AND THIS PRINTS IT.
;;;;
;;;;   test/run-glass-tx-cell.sh [MODUS-BINARY] [MODE] [GLASS-DIR] [CRAM-DIR]
;;;;
;;;; ============================================================
;;;; WHAT THIS SAYS THAT test/glass-send-worker.lisp DOES NOT
;;;; ============================================================
;;;;
;;;; run-glass-send-worker.sh reports "the `both' arm stops at 32785 of 49180
;;;; with MVM LONGJMP (TRAP #x0511) with no active handler-case".  Every word
;;;; of that is true and none of it names a defect: a longjmp complaint is what
;;;; the MVM interpreter prints (mvm/interp.lisp:898) when a TRAP #x0511 finds
;;;; an empty handler list, which is a SECOND-ORDER artefact of unwinding, not
;;;; the thing that went wrong.
;;;;
;;;; Arm the worker with a HANDLER-BIND and the real condition surfaces: a
;;;; TYPE-ERROR.  Grade GLASS::*TX* — the transfer counter that GLASS:W-U8
;;;; increments once per byte — between the two rectangles, and the TYPE-ERROR
;;;; has a name:
;;;;
;;;;     a=CONS/0   b=CONS/4   c=CONS/NOTINT:CHARACTER   c1=CONS/NOTINT:FUNCTION
;;;;
;;;; The cons is still a cons.  Its CAR was 0, then 4, and then it was A
;;;; CHARACTER, and then A FUNCTION.  Nothing in glass ever stores either into
;;;; it.  THE CELL IS BEING HANDED OUT AGAIN AS FRESH ALLOCATION MEMORY while
;;;; it is still live and still referenced.  `(incf (the fixnum (car *tx*)))'
;;;; in GLASS::TX+ then adds 1 to a character and signals the TYPE-ERROR, and
;;;; the unwind out of it is what prints the longjmp line.
;;;;
;;;; ============================================================
;;;; WHAT IT IS NOT — measured, each one killed by an arm below
;;;; ============================================================
;;;;
;;;; NOT THE UNWIND-PROTECT NESTING, which is what "the two wrappers, NESTED"
;;;; had pointed at for three rounds.  `with-fb-locked' is SB-THREAD:WITH-
;;;; RECURSIVE-LOCK (an unwind-protect) and a LET of a special compiles to
;;;; another (compile-let-with-specials), so `both' is two nested ones.  But
;;;; TX — ONE unwind-protect, no lock at all — corrupts the counter just as
;;;; `both' does, and BOTHNIL — the identical nesting to `both', differing only
;;;; in that *TX* is NIL so no counter cons is made — is CLEAN.  The nesting is
;;;; a passenger.
;;;;
;;;; NOT THE SPECIAL VARIABLE, and not a dynamic-binding stack: modus has no
;;;; binding stack to make per-thread.  COMPILE-LET-WITH-SPECIALS shallow-binds
;;;; — save the global cell into a LEXICAL slot, SET-SYMBOL-VALUE the new
;;;; value, restore from the lexical in an unwind-protect cleanup.  The save
;;;; already lives on the thread's own stack.  OTHERSP (the lock plus a LET of
;;;; *PRINT-BASE*, a special rebound over the same body) is CLEAN.
;;;;
;;;; NOT THE LOCK, in any sense: LOCK and BOTHNIL hold it and are clean, TX
;;;; does not hold it and is corrupted, and MYMTX — a mutex made in this file
;;;; and touched by nobody else — behaves as `both' does.
;;;;
;;;; NOT A RACE WITH MAIN.  GLASS_TX_MAINMODE=join parks main in JOIN-THREAD
;;;; instead of READ-BYTE, so main runs no reader and touches no stream:
;;;; `both' then fails IDENTICALLY — 32785 bytes, `c=CONS/NOTINT:CHARACTER'.
;;;; The worker is overwriting its OWN earlier allocation, and (%GC-REGION) on
;;;; the worker reads its own region block, not region 0's.
;;;;
;;;; NOT RECLAMATION OF AN UNROOTED OBJECT.  LOCKALLOC-KEEP holds the very same
;;;; cons in a LEXICAL on the worker's own stack for the whole extent of the
;;;; send — a live stack root, and it prints `keep=CONS' at the end to show the
;;;; root is still good — and the counter is overwritten anyway.
;;;;
;;;; ============================================================
;;;; WHAT IT IS, AND WHY EVERY EARLIER TABLE OF THIS WALL IS MISLEADING
;;;; ============================================================
;;;;
;;;; ***THE BYTE COUNT IS NOT THE ORACLE.  THE CAR IS.***  Grade the counter
;;;; instead of the transfer and the arms sort perfectly, ONE RUN EACH (the
;;;; first eight rows from a single `all' pass; lockset, mymtx and
;;;; lockalloc-keep run individually, same binary, same shape):
;;;;
;;;;   arm              counter installed?   CAR grade          delivered?
;;;;   plain            no  (*TX* NIL)       OFF, clean         yes
;;;;   lock             no                   OFF, clean         yes
;;;;   bothnil          no                   OFF, clean         yes
;;;;   othersp          no                   OFF, clean         yes
;;;;   tx               YES                  OVERWRITTEN        yes
;;;;   bothpre          YES                  OVERWRITTEN        yes
;;;;   lockset          YES                  OVERWRITTEN        yes
;;;;   lockalloc        YES                  OVERWRITTEN        yes
;;;;   lockalloc-keep   YES                  OVERWRITTEN        yes
;;;;   both             YES                  OVERWRITTEN        usually NOT
;;;;
;;;; EVERY arm that installs a live counter cons has that cons overwritten.
;;;; EVERY arm that installs none is clean.  Nothing else correlates — not the
;;;; lock, not the nesting, not which special, not where the cons was consed.
;;;;
;;;; SO WHAT `both' ACTUALLY IS is not a distinct failure: it is the ordinary
;;;; failure landing BEFORE the last TX+ that reads the counter instead of
;;;; after it.  That is why `both' stops at 32785 and the others do not, why
;;;; `both' is not even reliably 0-of-N in this shape, and why the fine
;;;; discriminants chased in a scratchpad reduction (LOCKALLOC 0 of 5 there,
;;;; 5 of 5 here) moved when the body was opened up: they were never measuring
;;;; whether the overwrite happened, only where it landed.
;;;;
;;;; ***AND test/run-glass-send-worker.sh's `tx' ARM IS NOT CLEAN EITHER.***  It
;;;; reports 49180 of 49180 and has for three rounds.  Its counter is corrupted
;;;; in the same run — here it read `c=CONS/26218438885942664', a garbage
;;;; FIXNUM rather than a character, so not even a TYPE-ERROR announces it.
;;;; That is the whole reason this file grades memory: a wire that carries every
;;;; byte proves nothing about a heap that has already been written over.
;;;;
;;;; The size dependence, measured in the reduction and NOT monotone — the tell
;;;; that this is placement, not a size limit.  GLASS_TX_FBW sets the width, so
;;;; rect 1's raw buffer is FBW*64*4 bytes:
;;;;
;;;;   FBW   8  buf  2048  DELIVERS      FBW  48  buf 12288  FAILS
;;;;   FBW  32  buf  8192  DELIVERS      FBW  96  buf 24576  FAILS
;;;;   FBW  64  buf 16384  DELIVERS      FBW 128  buf 32768  FAILS
;;;;
;;;; 4 pages deliver and 3 pages do not, so there is no threshold to find —
;;;; and by the table above those "DELIVERS" rows should be re-graded before
;;;; anyone leans on them.
;;;;
;;;; WHAT IS ESTABLISHED, in one sentence: a cons a worker allocates and keeps
;;;; writing to is handed out again as fresh allocation memory by that same
;;;; worker, deterministically, whenever glass's transfer counter is live.
;;;; WHAT IS NOT: which allocation writes it, or why.
;;;;
;;;; ============================================================
;;;; DEAD HYPOTHESIS: "THE SEAM IS INERT — THE GATE READS ZERO".  THE GATE
;;;; WAS ARMED THE WHOLE TIME AND THE ZERO WAS THIS FILE'S OWN INSTRUMENT.
;;;; ============================================================
;;;;
;;;; A previous version of CHK printed the threads-live gate and the per-CPU
;;;; mode word at every grade point, as inline (MEM-REF <literal> :U32), and
;;;; they read ZERO on the worker in every run — which was written up as
;;;; "%RT-ENTER is a load and a branch, the runtime-table lock never engages,
;;;; B-lite's arena is never carved, the seam cannot be what corrupts the
;;;; counter because it is not even executed."
;;;;
;;;; ***ALL OF THAT WAS A READ ARTIFACT.***  An inline MEM-REF of a literal
;;;; address, in a TOPLEVEL FORM that also contains a call to a function
;;;; defined by net/sb-thread-shim.lisp, reads the value the address held
;;;; BEFORE the form began — and the worker's whole lambda lives inside such a
;;;; form, because SB-THREAD:MAKE-THREAD is in it.  Measured deterministically,
;;;; JIT on and off; full isolation in test/hosted-bringup-bare.lisp and probe
;;;; F of test/hosted-bringup-gate.lisp.
;;;;
;;;; Read through the compiled accessors instead and the same run says
;;;; gate=1 mode=1 on the worker at every grade point, with the lock's own
;;;; acquisition counter (#x10000DE0) climbing past 700,000 during the send.
;;;; The seam is FULLY ARMED in glass's process and always was.  So:
;;;;
;;;;   * "the lock is inert here" is FALSE — do not re-derive it;
;;;;   * "B-lite's arena was never carved in the glass runs, which is why the
;;;;     wall measured byte-identical before and after B-lite" is FALSE too,
;;;;     and that candidate explanation is withdrawn;
;;;;   * arming the seam does NOT stop the overwrite — the overwrite happens
;;;;     with the seam armed, which is the state it was always measured in.
;;;;     The writer is still unfound, and the seam is still not implicated,
;;;;     but now for the honest reason (it runs and the counter dies anyway)
;;;;     rather than the false one (it never runs).
;;;;
;;;; THE GATE COLUMNS ARE GONE FROM CHK, and deliberately not replaced with
;;;; honest ones: see the layout warning below.  The seam's state on a worker
;;;; is established where it can be measured without disturbing this fixture —
;;;; probe A of test/hosted-bringup-gate.lisp has the worker itself report
;;;; gate=1 mode=1, and test/hosted-bringup-bare.lisp asserts the lock
;;;; ENGAGING via the acquisition counter rather than via any word read.
;;;;
;;;; ============================================================
;;;; ***THE INSTRUMENT MOVES THE BUG.  THIS IS THE FOURTH DEMONSTRATION.***
;;;; ============================================================
;;;;
;;;; Changing CHK from two inline MEM-REFs to two compiled accessor calls plus
;;;; one %RT-ACQUISITIONS read — a change that reads MORE honestly and does
;;;; strictly less lying — made the `both' arm deliver all 49180 bytes and
;;;; grade CLEAN.  It looked exactly like a fix.  It is not:
;;;;
;;;;   instrument                          binary      result
;;;;   inline MEM-REF gate columns         pre-guard   32785, NOTINT:CHARACTER
;;;;   inline MEM-REF gate columns         post-guard  32785, NOTINT:CHARACTER
;;;;   accessor calls + acquisitions       pre-guard   49180, graded "clean"
;;;;   accessor calls + acquisitions       post-guard  49180, graded "clean"
;;;;
;;;; The BINARY does not matter.  The INSTRUMENT decides.  Same lesson as the
;;;; CANARY (a three-cons allocation made a reliably-corrupted arm clean), as
;;;; opening BODY up, and as the non-monotone FBW table.  ***A CLEAN RUN UNDER
;;;; A NEW PROBE IS NOT EVIDENCE OF A FIX.  Change the probe and re-run the
;;;; OLD probe before believing anything.***
;;;;
;;;; ============================================================
;;;; ***AND `integerp' IS NOT ENOUGH EITHER — THE COUNTER GOES BACKWARDS.***
;;;; ============================================================
;;;;
;;;; The grade below reports NOTINT: when the CAR stops being an integer, and
;;;; the runner greps for that.  A garbage FIXNUM passes it silently.  Under
;;;; the accessor instrument above, the "clean" run reads:
;;;;
;;;;     a=CONS/0  b=CONS/4  c=CONS/32852  d=CONS/16459  e=CONS/16459
;;;;
;;;; d is LESS THAN c.  *TX* counts one per byte written, so it cannot
;;;; decrease; and the C value moves with the instrument too (32852 with the
;;;; accessors, 32837 without the gate columns, against a 4 + 12 + 32768 =
;;;; 32784 that neither matches).  So the counter is corrupted in the
;;;; "clean" arm as well — the overwrite merely landed on a value that
;;;; happens to be an integer.
;;;;
;;;; ***THE GRADE THAT WOULD NOT HAVE BEEN FOOLED IS MONOTONICITY PLUS A
;;;; FINAL VALUE***, and it is the next thing to add here: a=0, b=4, and
;;;; a=<b<=c<=d=e=49180 exactly.  It is not added in this round because
;;;; changing CHK is precisely what moves the bug, and the currently shipped
;;;; shape is the one that still REPRODUCES.  Whoever adds it must re-run the
;;;; old shape alongside and report both.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE GRADE IS OF MEMORY, NOT OF THE WIRE.  TYPE-OF on the CAR of a cons
;;;;   glass owns is not a self-referential check: no arm of this file, and no
;;;;   line of glass, ever stores a CHARACTER or a FUNCTION there.
;;;;
;;;;   THE PEER IS PYTHON AND IT COUNTS, so "it returned" is never the
;;;;   evidence; the byte count is taken on the far side of the kernel.
;;;;
;;;;   THE ARMS SHARE ONE BODY.  Every arm calls the same BODY on the same
;;;;   framebuffer over the same socket and differs only in the wrapper named
;;;;   in DO-SEND, so a difference between arms is that wrapper.
;;;;
;;;;   THE SCREEN IS TALLER THAN ONE BAND: 96 rows against *MAX-BAND-ROWS* = 64
;;;;   is two rectangles, and the corruption is visible between them.  A 64-row
;;;;   framebuffer passes every arm and proves nothing.
;;;;
;;;;   NOTHING IS LEFT LISTENING: loopback, a kernel-chosen port, 5900-5920
;;;;   refused outright, and `ss' asked afterwards by the runner.
;;;;
;;;; glass and cram are READ-ONLY: their sources are loaded where they sit.

;;; The manifest (test/glass-manifest.lisp) is prepended by the runner.
(unless (boundp '*glass-files*)
  (format t "~&test/glass-tx-cell.lisp needs a manifest; run its runner~%")
  (finish-output)
  (sys-exit 2))

(format t "~&main: PRE-LOAD gate=~s mode=~s flag=~s~%"
        (%rt-threads-live-p) (%ha-percpu-mode) *sb-threads-up*)
(finish-output)
(dolist (entry *glass-files*) (load (first entry)))
(format t "~&main: POST-LOAD gate=~s mode=~s flag=~s~%"
        (%rt-threads-live-p) (%ha-percpu-mode) *sb-threads-up*)
(format t "~&=== loaded ===~%")
(force-output)

(defun getenv-or (name default)
  (let ((s (%cli-getenv name)))
    (if (or (null s) (= (length s) 0)) default s)))

(defvar *mode* (getenv-or "GLASS_TX_MODE" "both"))
(defvar *mainmode* (getenv-or "GLASS_TX_MAINMODE" "read"))
(defvar *fbw* (let ((s (getenv-or "GLASS_TX_FBW" nil)))
                (if s (parse-integer s) 128)))
(defvar *fbh* 96)                       ; > 64 on purpose: glass bands at 64 rows
(defvar *mtx* (sb-thread:make-mutex :name "glass-tx-cell"))

(defun gsw-pixel (x y)
  (logior (ash (logand (* x 2) 255) 16)
          (ash (logand (* y 3) 255) 8)
          16))

;;; The bounds are LOCALS, not the globals: a special read compiles to
;;; SYMBOL-VALUE, which is a mechanism under test elsewhere and has no business
;;; running twelve thousand times inside the fixture that paints the reference.
(defun gsw-make-fb ()
  (let ((w *fbw*) (h *fbh*))
    (let ((fb (funcall (find-symbol "MAKE-FRAMEBUFFER" "GLASS") w h))
          (put (find-symbol "FB-PUT" "GLASS"))
          (y 0))
      (loop
        (when (>= y h) (return nil))
        (let ((x 0))
          (loop
            (when (>= x w) (return nil))
            (funcall put fb x y (gsw-pixel x y))
            (setq x (+ x 1))))
        (setq y (+ y 1)))
      fb)))

;;; ---- THE GRADE ---------------------------------------------------------------
;;;
;;; TAG=CONS/<n>            the counter is intact and holds <n>
;;; TAG=CONS/NOTINT:<type>  THE CELL WAS OVERWRITTEN — the CAR is a <type>
;;; TAG=BROKEN              *TX* itself is no longer a cons
;;;
;;; WRITE-STRING-SERIAL, not FORMAT: this runs on the worker at the exact
;;; moment under test, and the point is to disturb it as little as a print can.
(defun chk (tag)
  (write-string-serial tag)
  (if (null glass::*tx*)
      ;; The arms that never install a counter (plain / lock / bothnil /
      ;; othersp) legitimately read NIL.  That is OFF, not BROKEN — the runner
      ;; greps for BROKEN, so the two must not share a word.
      (write-string-serial "=OFF")
  (if (consp glass::*tx*)
      (progn
        (write-string-serial "=CONS/")
        (if (integerp (car glass::*tx*))
            (write-string-serial (princ-to-string (car glass::*tx*)))
            (progn (write-string-serial "NOTINT:")
                   (write-string-serial (princ-to-string (type-of (car glass::*tx*)))))))
      (write-string-serial "=BROKEN")))
  (write-string-serial " "))

;;; SEND-RECTS' own body, opened up so the counter can be graded BETWEEN the
;;; two rectangles.  Identical calls, identical order — GLASS:SEND-RECTS is
;;; (w-u8 0)(w-u8 0)(w-u16 nrects) then EMIT-RECT per banded rect, and with
;;; ENC 0 and no TRLE/ZRLE threshold met EMIT-RECT is WRITE-RECT-RAW.
(defun body (s fb)
  (let ((w-u8 (find-symbol "W-U8" "GLASS"))
        (w-u16 (find-symbol "W-U16" "GLASS"))
        (wrr (find-symbol "WRITE-RECT-RAW" "GLASS")))
    (chk "a")
    (funcall w-u8 s 0) (funcall w-u8 s 0) (funcall w-u16 s 2)
    (chk "b")
    (funcall wrr s fb 0 0 *fbw* 64 nil)
    (chk "c")
    (funcall wrr s fb 0 64 *fbw* 32 nil)
    (chk "d")
    (force-output s)
    (chk "e")))

;;; The arms.  ONE body; they differ only in the wrapper.
;;; ============================================================
;;; THE SECOND GRADER: MONOTONICITY AND AN EXACT FINAL VALUE
;;; ============================================================
;;;
;;; ***`integerp' IS NOT ENOUGH.***  The grade above reports NOTINT: when the
;;; CAR stops being an integer, and a garbage FIXNUM passes it silently — the
;;; run that produced a=0 b=4 c=32852 d=16459 e=16459 was graded CLEAN, and
;;; d IS LESS THAN c on a counter that only ever increments.
;;;
;;; *TX* counts one per byte written, so across the five grade points it must
;;; be NON-DECREASING and must END at exactly 49180 (4 header bytes + 12+32768
;;; for rect 1 + 12+16384 for rect 2).  That is the grade that would not have
;;; been fooled.
;;;
;;; ***IT IS A SEPARATE ARM, NOT AN EDIT TO CHK.***  Changing CHK is precisely
;;; what moves this bug — measured: accessor reads in CHK made `both' deliver
;;; 49180 and grade clean on BOTH binaries while the shipped shape stops at
;;; 32785 on both.  So the shipped reproduction stays byte-comparable and this
;;; runs alongside it under its own mode.  A disagreement between the two arms
;;; is a DATUM about layout sensitivity, not a contradiction.
(defvar *mono-seen* nil)            ; the five readings, newest first
(defvar *mono-expect* 49180)

(defun chk-mono (tag)
  (write-string-serial tag)
  (if (consp glass::*tx*)
      (let ((v (car glass::*tx*)))
        (setq *mono-seen* (cons v *mono-seen*))
        (write-string-serial "=CONS/")
        ;; SAME MARKER THE SHIPPED GRADE USES, so the runner's NOTINT: grep
        ;; still sees this arm.  A character printed bare reads as "S".
        (if (integerp v)
            (write-string-serial (princ-to-string v))
            (progn (write-string-serial "NOTINT:")
                   (write-string-serial (princ-to-string (type-of v))))))
      (progn (setq *mono-seen* (cons :broken *mono-seen*))
             (write-string-serial "=BROKEN")))
  (write-string-serial " "))

(defun body-mono (s fb)
  (let ((w-u8 (find-symbol "W-U8" "GLASS"))
        (w-u16 (find-symbol "W-U16" "GLASS"))
        (wrr (find-symbol "WRITE-RECT-RAW" "GLASS")))
    (chk-mono "a")
    (funcall w-u8 s 0) (funcall w-u8 s 0) (funcall w-u16 s 2)
    (chk-mono "b")
    (funcall wrr s fb 0 0 *fbw* 64 nil)
    (chk-mono "c")
    (funcall wrr s fb 0 64 *fbw* 32 nil)
    (chk-mono "d")
    (force-output s)
    (chk-mono "e")))

;;; Reports (verdict readings), and the verdict names WHICH law broke.
;;; ***THE AGGREGATE VERDICT ONLY PRINTS IF THE RUN COMPLETES.***  Today this
;;; arm dies mid-send (32785, CAR already a CHARACTER at grade point c), so the
;;; line to read is the per-point `a=CONS/… b=CONS/…' trace CHK-MONO emits
;;; inline.  That is deliberate: the readings are recorded as they are taken,
;;; so a run that dies still shows where the counter stopped being sane.
;;;
;;; AND NOTE WHAT THIS ARM ALREADY DEMONSTRATES: it is the shipped `both'
;;; nesting with a grader that merely RECORDS each reading, and it stops at
;;; 32785 with NOTINT:CHARACTER — while the shipped CHK, which records
;;; nothing, delivers all 49180 and only shows NOTINT at d/e.  One SETQ per
;;; grade point moves the failure.  Fifth demonstration.
(defun grade-mono ()
  (let ((v (reverse *mono-seen*)))
    (if (null v)
        (list :no-readings v)
        (let ((bad nil) (prev nil) (rest v))
          (loop
            (when (null rest) (return nil))
            (let ((x (car rest)))
              (if (not (integerp x))
                  (setq bad (or bad :not-an-integer))
                  (if (and prev (< x prev))
                      (setq bad (or bad :went-backwards))
                      nil))
              (if (integerp x) (setq prev x) nil))
            (setq rest (cdr rest)))
          (let ((last (car (last v))))
            (list (or bad
                      (if (equal last *mono-expect*) :ok :wrong-final))
                  v))))))

;;; ============================================================
;;; REGION 0 -> WORKER POINTERS, FROM SPECIAL-VARIABLE BINDING
;;; ============================================================
;;;
;;; THE ARM PARTITION NAMES A MECHANISM.  Clean: plain, lock, bothnil, othersp.
;;; Corrupted: tx, both, lockalloc.  `bothnil' is the tell — IDENTICAL nesting,
;;; same recursive lock, same unwind-protects, *TX* bound to NIL — and it is
;;; clean.  So it is not the nesting, not the lock, not the unwind.  What the
;;; corrupted arms do and the clean ones do not is ALLOCATE A FRESH CONS ON THE
;;; WORKER AND STORE IT INTO A GLOBAL:
;;;
;;;   (let ((glass::*tx* (list 0))) …)
;;;     1. (LIST 0) conses in the WORKER'S region;
;;;     2. modus shallow-binds — SET-SYMBOL-VALUE writes that cons into the
;;;        globals table, WHICH LIVES IN REGION 0.
;;;
;;; That is a REGION 0 -> WORKER pointer, the forbidden direction this campaign
;;; already proved fatal for CL:INTERN (the audit read +44 and one forced
;;; collection SIGSEGV'd 3 of 3).  Interning was fixed by copying into the lock
;;; arena; SPECIAL-VARIABLE BINDING WAS NEVER FIXED, and COMPILE-LET-WITH-
;;; SPECIALS does not go near the arena.  If that is the mechanism, the cons is
;;; unreachable from any root the WORKER'S collector walks, so its space is
;;; reclaimed and re-issued while *TX* still names it — which is exactly "a live
;;; cons handed out again as fresh allocation", and exactly why the CAR comes
;;; back a CHARACTER or a FUNCTION rather than a corrupted integer.
;;;
;;; ***MEASURED WITH %GC-COUNT-FOREIGN-REFS, AFTER THE SEND, IN ITS OWN ARMS.***
;;; Nothing is added to the hot path and no shipped arm is touched, so this
;;; cannot move the bug.  The count is taken INSIDE the binding but AFTER BODY
;;; has returned, because modus shallow-binds: once the LET unwinds, the global
;;; is restored and the pointer under test no longer exists.
;;;
;;; VACUITY GUARD, the same one that has already caught this campaign twice: a
;;; zero is only meaningful if the spans being scanned are non-zero.  FR-REPORT
;;; prints both spans next to the count, and a zero span means NOT MEASURED.
;;;
;;; ============================================================
;;; MEASURED, 3 of 3, DETERMINISTIC (arm `txfr'):
;;;
;;;   FR[pre] =0  r0span=107661168 wspan=16777216 wgc=0
;;;   a=CONS/0 b=CONS/4 c=CONS/32851 d=CONS/16429      <- d < c, corrupted
;;;   FR[post]=1  r0span=107669856 wspan=16777216 wgc=17
;;;
;;; ***EXACTLY ONE REGION 0 -> WORKER POINTER EXISTS AT THE END OF A CORRUPTED
;;; SEND, WHERE THERE WERE NONE BEFORE THE BINDING***, across 17 collections of
;;; the worker's own region.  One cons, one pointer, the forbidden direction.
;;; That is the shape the hypothesis predicts.
;;;
;;; ***THE FIRST VERSION OF THIS PROBE SCANNED ONLY FROM-SPACE AND READ 0.***
;;; The worker's live data had flipped to to-space.  A false zero would have
;;; killed a live hypothesis for the second time in this campaign; scanning
;;; BOTH semispaces is not defensive, it is the difference between the two
;;; answers.  Do not remove it.
;;;
;;; ***WHAT IS NOT ESTABLISHED: THE BETWEEN-ARM CONTROL.***  `bothnilfr' is the
;;; same shape with *TX* bound to NIL and it should read 0.  It does not run —
;;; "the server never announced a port", 3 of 3, deterministic, cause unknown
;;; and NOT diagnosed.  So the +1 is attributed to the binding only by the
;;; WITHIN-RUN control (pre=0 -> post=1, same process, same spans); anything
;;; else the send stores globally would also be counted.  ***THE MECHANISM IS
;;; CONSISTENT WITH THE MEASUREMENT, NOT YET PROVEN BY IT.***  Fixing
;;; `bothnilfr' is the next step and it is a cheap one.
(defun fr-fwd ()
  "Region 0's live span AND the lock arena -> THIS worker's region.
   ***BOTH SEMISPACES ARE SCANNED*** (+0x00 from_start, +0x08 to_start): after a
   flip the worker's live data is in the other one, and scanning only from-space
   would return a FALSE ZERO — which is the failure mode this whole campaign is
   about.  Returns (count region0-span worker-span worker-gc-count)."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wto (%gc-meta-read (+ rw #x08) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (wgc (%gc-meta-read (+ rw #x20) k))
         (r0 (%gc-region-0))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0alloc (%gc-meta-read (+ r0 #x30) k))
         (ab (%rt-arena-base))
         (aa (%rt-arena-alloc)))
    (list (+ (%gc-count-foreign-refs r0from r0alloc wfrom wsize)
             (%gc-count-foreign-refs r0from r0alloc wto wsize)
             (if (> aa ab) (%gc-count-foreign-refs ab aa wfrom wsize) 0)
             (if (> aa ab) (%gc-count-foreign-refs ab aa wto wsize) 0))
          (- r0alloc r0from)
          wsize
          wgc)))

(defun fr-report (tag)
  (let ((r (fr-fwd)))
    (write-string-serial " FR[")
    (write-string-serial tag)
    (write-string-serial "]=")
    (write-string-serial (princ-to-string (first r)))
    (write-string-serial " r0span=")
    (write-string-serial (princ-to-string (second r)))
    (write-string-serial " wspan=")
    (write-string-serial (princ-to-string (third r)))
    (write-string-serial " wgc=")
    (write-string-serial (princ-to-string (fourth r)))
    (write-string-serial " ")))

(defun do-send (s fb mode)
  (cond
    ;; ---- the four arms run-glass-send-worker.sh names ----
    ((string= mode "plain") (body s fb))
    ((string= mode "tx")    (let ((glass::*tx* (list 0))) (body s fb)))
    ((string= mode "lock")  (glass::with-fb-locked (fb) (body s fb)))
    ((string= mode "both")  (glass::with-fb-locked (fb)
                              (let ((glass::*tx* (list 0))) (body s fb))))
    ;; ---- REGION0->WORKER PROBE ARMS.  `txfr' mirrors `tx' (corrupted),
    ;; `bothnilfr' mirrors `bothnil' (clean).  Same shape, one binds a fresh
    ;; cons and one binds NIL: that is the whole experiment.
    ((string= mode "txfr")
     (fr-report "pre")
     (let ((glass::*tx* (list 0))) (body s fb) (fr-report "post")))
    ((string= mode "bothnilfr")
     (fr-report "pre")
     (glass::with-fb-locked (fb)
       (let ((glass::*tx* nil)) (body s fb) (fr-report "post"))))
    ;; ---- THE SECOND GRADER'S ARM: identical to `both', graded harder ----
    ((string= mode "bothmono") (glass::with-fb-locked (fb)
                                 (let ((glass::*tx* (list 0))) (body-mono s fb))))
    ;; ---- the nesting is a passenger ----
    ;; identical nesting; *TX* NIL so TX+ reads the special and does no INCF
    ((string= mode "bothnil") (glass::with-fb-locked (fb)
                                (let ((glass::*tx* nil)) (body s fb))))
    ;; a NON-glass special under the same lock
    ((string= mode "othersp") (glass::with-fb-locked (fb)
                                (let ((*print-base* 10)) (body s fb))))
    ;; ---- where the cons is made is the whole difference ----
    ;; ONE unwind-protect, no special LET; cons made INSIDE the lock
    ((string= mode "lockalloc") (glass::with-fb-locked (fb)
                                  (setq glass::*tx* (list 0)) (body s fb)))
    ;; the same cons, made BEFORE grab-mutex
    ((string= mode "bothpre") (let ((c (list 0)))
                                (glass::with-fb-locked (fb)
                                  (let ((glass::*tx* c)) (body s fb)))))
    ;; cons before the lock, plain SETQ, counter fully live
    ((string= mode "lockset") (setq glass::*tx* (list 0))
                              (glass::with-fb-locked (fb) (body s fb)))
    ;; ---- it is not the framebuffer's lock ----
    ((string= mode "mymtx") (sb-thread:with-recursive-lock (*mtx*)
                              (let ((glass::*tx* (list 0))) (body s fb))))
    ;; ---- it is not lost rooting: KEEP is a live stack root throughout ----
    ((string= mode "lockalloc-keep")
     (glass::with-fb-locked (fb)
       (let ((keep (list 0)))
         (setq glass::*tx* keep)
         (body s fb)
         (write-string-serial (if (consp keep) "keep=CONS " "keep=BROKEN "))
         keep)))
    ;; ---- is it the counter, or is it ANY object allocated there? ----
    ;; CANARY conses a 3-element list next to the counter, hands it to nobody,
    ;; reads it from nobody until the end, and reports whether it still holds
    ;; (0 1 2).  If the canary rots too, the overwrite is indiscriminate and the
    ;; subject is the allocator, not *TX*.
    ((string= mode "canary")
     (glass::with-fb-locked (fb)
       (let ((canary (list 0 1 2)))
         (setq glass::*tx* (list 0))
         (body s fb)
         (write-string-serial
          (if (and (consp canary) (equal canary '(0 1 2)))
              "canary=INTACT "
              (concatenate 'string "canary=ROTTED:" (princ-to-string canary) " ")))
         canary)))
    (t (error "glass-tx-cell: unknown mode ~a" mode))))

(let* ((fb (gsw-make-fb))
       (listener (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
  (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
  (sb-bsd-sockets:socket-listen listener 4)
  (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name listener)
    (declare (ignore addr))
    (if (and (>= port 5900) (<= port 5920))
        (progn
          (format t "~&REFUSING: the kernel handed back ~d, inside 5900-5920.~%" port)
          (sb-bsd-sockets:socket-close listener)
          (finish-output)
          (sys-exit 1))
        nil)
    (format t "~&MODE ~a FBW ~d EXPECT ~d~%" *mode* *fbw* (+ 28 (* *fbw* 384)))
    (format t "~&PORT ~d~%" port)
    (finish-output))
  (let* ((conn (sb-bsd-sockets:socket-accept listener))
         (s (sb-bsd-sockets:socket-make-stream conn :input t :output t
                                                    :element-type '(unsigned-byte 8))))
    ;; THE HANDLER-BIND IS THE INSTRUMENT.  Without an armed handler above it
    ;; the TYPE-ERROR unwinds into the interpreter's empty handler list and is
    ;; reported as the longjmp line, losing its identity; with one, the arm
    ;; reports what actually signalled.
    (format t "~&main: PRE-THREAD gate=~s mode=~s flag=~s~%"
            (%rt-threads-live-p) (%ha-percpu-mode) *sb-threads-up*)
    (finish-output)
    (let ((th (sb-thread:make-thread
               (lambda ()
                 (write-string-serial "worker: sending ")
                 (handler-case (progn (do-send s fb *mode*)
                                      (write-string-serial "worker: sent ")
                                      :ok)
                   (error (e)
                     (write-string-serial "worker: CAUGHT ")
                     (write-string-serial (princ-to-string e))
                     (write-string-serial " ")
                     :err)))
               :name "glass-tx-cell")))
      ;; MAIN PARKS READING, as glass's reader thread does — or, with
      ;; GLASS_TX_MAINMODE=join, does not run a reader at all, which is the arm
      ;; that shows main is not the other mutator.
      (if (string= *mainmode* "read")
          (progn (format t "~&reader got ~s~%" (read-byte s nil :eof)) (finish-output))
          (format t "~&main: not reading~%"))
      (finish-output)
      (format t "~&worker joined: ~s~%" (sb-thread:join-thread th))
      (finish-output))
    (sb-bsd-sockets:socket-close conn))
  (sb-bsd-sockets:socket-close listener))

;;; THE SECOND GRADER REPORTS FROM THE DRIVER, after the join — reading
;;; *MONO-SEEN* on the worker would be one more allocation in the hot path,
;;; which is the thing that moves this bug.
(if (string= *mode* "bothmono")
    (let ((g (grade-mono)))
      (format t "~&MONO readings=~s~%" (second g))
      (format t "~&MONO verdict=~s (expected final ~s)~%" (first g) *mono-expect*)
      (format t "~&MONO ~a~%"
              (if (eq (first g) :ok)
                  "PASS — non-decreasing and exactly the expected final byte count"
                  "FAIL — the counter is corrupted; `integerp' alone would have called this clean")))
    nil)
(finish-output)

(format t "~&SERVER DONE~%")
(finish-output)
(sys-exit 0)
