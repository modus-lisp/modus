;;;; hosted-mainhop-probe.lisp — SHAPE A'S GATE: WHAT BREAKS WHEN MAIN LEAVES
;;;; REGION 0?
;;;;
;;;;   test/run-mainhop-probe.sh [MODUS-BINARY] [RUNS]
;;;;   ./modus --script test/hosted-mainhop-probe.lisp
;;;;
;;;; ============================================================
;;;; WHY THIS RUNS BEFORE ANY FIX IS BUILT
;;;; ============================================================
;;;;
;;;; The measured defect is that region 0 has TWO mutators and ONE frontier
;;;; stored in two places: main allocates it from registers outside the lock,
;;;; a locked worker allocates it from the parked field, and main's next hop
;;;; parks its register value over the worker's advance
;;;; (test/run-region0-frontier.sh: worker's region-0 cons corrupted 5 of 17,
;;;; own-region control 0 of 17).
;;;;
;;;; The proposed fix — shape A — is one region, one mutator: at bringup MAIN
;;;; ENTERS A REGION OF ITS OWN, exactly as the 100-of-100 %TL-SELFTEST already
;;;; does, and region 0 becomes what the locked sections already treat it as: a
;;;; shared heap touched only under the runtime lock.  The carve has reserved
;;;; slot 0's region for main since it went to N ("SLOT 0 IS THE MAIN THREAD'S
;;;; AND ITS REGION IS SPARE", net/hosted-actors.lisp), so the fix would spend
;;;; memory that is already spent.
;;;;
;;;; But main is not the green test's main.  %TL-SELFTEST's main runs a
;;;; COMPILED workload engineered to publish nothing from its own region:
;;;; fresh-name strings are built UNDER the lock so they land in region 0, and
;;;; the function it stores is an image function.  A script's main runs
;;;; MVM-EVAL, which was never engineered that way.  So before the fix is
;;;; built, this file hops main and measures what actually happens, phase by
;;;; phase, each phase printed and flushed before the next begins so a later
;;;; death cannot eat an earlier number.
;;;;
;;;; ============================================================
;;;; THE PHASES, AND WHAT EACH ONE'S NUMBER MEANS
;;;; ============================================================
;;;;
;;;; P1  WHERE ALLOCATION LANDS.  Pre-hop a cons must land in region 0 (which
;;;;     also proves the where-instrument can answer "region 0"); post-hop a
;;;;     cons must land in main's region, and a fresh %INTERN-SYMBOL-PKG symbol
;;;;     must STILL land in region 0 — the lock hop routing shared allocations
;;;;     is the part of today's design that shape A keeps.
;;;;
;;;; P2  NO REGION-0 ALLOCATION OUTSIDE THE LOCK.  With main out of region 0,
;;;;     region 0's parked frontier may move only while somebody is inside the
;;;;     lock.  Compiled non-locking work on main must leave it exactly still.
;;;;
;;;; P3  THE PUBLICATION AUDIT, ONE SITE AT A TIME.  region 0 -> main's region
;;;;     references, re-counted after each kind of toplevel form: a DEFUN, a
;;;;     DEFVAR of a heap value, a SETQ of a heap value, a FORMAT, a CL:INTERN.
;;;;     The coordinator's criterion is verbatim "zero, or a small enumerable
;;;;     set of sites that can be brought under the lock or moved to main's
;;;;     region" — so the audit is BY SITE, not one lump.  CL:INTERN is also
;;;;     the instrument's positive control: it is known to add ~2 references
;;;;     per symbol, so a zero from the other sites is a zero from an
;;;;     instrument that can answer non-zero.
;;;;
;;;; P4  FORCED COLLECTION OF MAIN'S REGION.  The campaign's record says a
;;;;     forced collection is survivable where a natural one is not; and the
;;;;     precise root list (translate-x64's EMIT-GC-TRAMPOLINE) scans the
;;;;     region-0-resident tables' ROOT SLOTS but Cheney-walks only the
;;;;     collecting region's to-space — so a region-0 table's INTERNAL entry
;;;;     pointing into main's region is expected to DANGLE across a collection
;;;;     of main's region.  P4 measures exactly that: an argument-passed chain
;;;;     (conservatively stack-rooted) must survive; whether a DEFVAR'd list,
;;;;     a post-hop DEFUN and a CL:INTERN'd symbol survive is the finding.
;;;;
;;;; P5  NATURAL COLLECTION UNDER EVALUATED CODE — the recorded killer
;;;;     (CLAUDE.md: region 0 collected naturally under evaluated code and the
;;;;     running toplevel form was DESTROYED, re-executed and then swallowed).
;;;;     Shape A moves that exposure from a ~600 MB region to a 16 MB one, so
;;;;     it must be measured, not argued: one evaluated toplevel form
;;;;     allocates ~3x the region's semispace and reports its own iteration
;;;;     count and a re-entry counter.  LAST, because it is the likeliest
;;;;     killer and everything before it must already be on stdout.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   EVERY INSTRUMENT SHOWS IT CAN ANSWER BOTH WAYS IN THE SAME RUN: the
;;;;   where-probe answers "region 0" pre-hop and "main's region" post-hop;
;;;;   the publication audit's CL:INTERN row must be non-zero; P2's stillness
;;;;   check sits beside P3's lock-section growth.  P4's chain is the survival
;;;;   control for its three dangle probes.
;;;;
;;;;   THE PROBES ARE COMPILED BEFORE THE HOP and take every address as an
;;;;   ARGUMENT — an implicit global inside a probe is a SYMBOL-VALUE, which
;;;;   takes the lock, which hops regions and republishes the very fields
;;;;   being read (the lesson test/hosted-region0-frontier.lisp paid for).
;;;;
;;;;   ONE RUN IS NOT A RESULT.  test/run-mainhop-probe.sh runs this N times
;;;;   and classifies survived / died / hung per phase reached.

(%ha-actors-bringup 4 0)

;;; THE ATTRIBUTION CONTROL.  MAINHOP_CONTROL=1 runs the IDENTICAL script with
;;; the hop skipped and the forced collection skipped: main stays in region 0,
;;; whose headroom dwarfs this workload, so NOTHING collects.  Every workload
;;; form still runs.  If the control survives where the hopped run dies, the
;;; deaths are chargeable to "main's region collected", not to the workload.
(defvar *ctl* 0)
(setq *ctl* (let ((s (%cli-getenv "MAINHOP_CONTROL")))
              (if (and s (string= s "1")) 1 0)))

(defvar *r0* 0)  (setq *r0* (%gc-region-0))
(defvar *scr* 0) (setq *scr* (%thr-scratch))
(defvar *ms* 0)  (setq *ms* (%gc-meta-scale))

;;; ---- INSTRUMENTS, all compiled pre-hop, all state passed as arguments ----

(defun mh-where (x scr r0 ms)
  "0 = region 0's live semispace, 1 = either semispace of slot 0's region
   (main's, post-hop), 2 = anywhere else."
  (let ((w (%gc-word-of x (+ scr 512)))
        (r0from (%gc-meta-read (+ r0 #x00) ms))
        (r0size (%gc-meta-read (+ r0 #x10) ms))
        (mfrom (%ha-region-from 0))
        (mto (%ha-region-to 0))
        (msz *ha-rsize*))
    (cond ((and (>= w r0from) (< w (+ r0from r0size))) 0)
          ((and (>= w mfrom) (< w (+ mfrom msz))) 1)
          ((and (>= w mto) (< w (+ mto msz))) 1)
          (t 2))))

(defun mh-fwd (r0 ms)
  "region 0's live span -> EITHER semispace of slot 0's region.  The direction
   that dangles when main's region collects."
  (let ((r0from (%gc-meta-read (+ r0 #x00) ms))
        (r0alloc (%gc-meta-read (+ r0 #x30) ms)))
    (+ (%gc-count-foreign-refs r0from r0alloc (%ha-region-from 0) *ha-rsize*)
       (%gc-count-foreign-refs r0from r0alloc (%ha-region-to 0) *ha-rsize*))))

(defun mh-parked (r0 ms) (%gc-meta-read (+ r0 #x30) ms))

(defun mh-hop (r0 ms)
  "THE FIX'S GESTURE, MADE BY HAND: main enters slot 0's carved region — the
   region the carve has always reserved for it — with the process stack base
   as its root window, exactly the stack top %TL-SELFTEST hands main's region."
  (let ((rcb (%ha-rcb 0)))
    (%gc-region-init rcb (%ha-region-from 0) (%ha-region-to 0) *ha-rsize*
                     (%gc-meta-read (+ r0 #x18) ms) ms)
    (%gc-region-enter rcb)
    rcb))

(defun mh-consing (k)
  "Compiled, allocating, and LOCK-FREE: no global reads, no interns, no
   format.  P2's subject."
  (let ((i 0) (c nil))
    (loop (when (>= i k) (return 0))
      (setq c (cons i c))
      (setq i (+ i 1)))
    (if c k 0)))

(defun mh-p2 (r0 ms k)
  "Region 0's parked-frontier delta across MH-CONSING, computed INSIDE one
   compiled function.  The first version of this probe bracketed the two
   readings around EVALUATED toplevel forms, whose SETQs legitimately take the
   lock and allocate table entries in region 0 — and measured that instead."
  (let ((a (mh-parked r0 ms)))
    (mh-consing k)
    (- (mh-parked r0 ms) a)))

(defun mh-natural-compiled (r0 ms)
  "P5a: allocate ~3x the semispace INSIDE COMPILED CODE — the same 768 x 64 KB
   strings the evaluated twin allocates — holding a live 8-cons chain, and
   report (list completed-count gc-delta chain-sum).  Every worker test in the
   tree survives natural collections inside compiled code; if main-hopped
   compiled code survives too and the EVALUATED twin does not, the hazard is
   the evaluator's transient state, not the region or the collector.
   (A first version consed 3 000 000 times instead — bytecode at ~microseconds
   per iteration made that a false HANG at the runner's timeout.)"
  r0 ms
  (let ((i 0) (s nil) (keep (mh-chain 8)) (g0 (mh-gc-count)))
    (loop
      (when (>= i 768) (return nil))
      (setq s (make-string 65536))
      (setq i (+ i 1)))
    (if s
        (list i (- (mh-gc-count) g0) (mh-chain-sum keep))
        (list i (- (mh-gc-count) g0) -1))))

(defun mh-chain (n)
  (let ((c nil) (i 0))
    (loop (when (>= i n) (return 0))
      (setq c (cons i c))
      (setq i (+ i 1)))
    c))

(defun mh-chain-sum (c)
  (let ((s 0) (p c))
    (loop (when (null p) (return 0))
      (setq s (+ s (car p)))
      (setq p (cdr p)))
    s))

(defun mh-collect-with (chain)
  "Force one collection of the ACTIVE region — main's, post-hop — from
   compiled code, holding CHAIN in an argument so the conservative stack scan
   roots it.  Returns the chain's post-collection checksum."
  (%ha-collect-here)
  (mh-chain-sum chain))

(defun mh-gc-count () (%ha-my-gc-count))

;;; ---- verdict plumbing (pre-hop, region 0) ------------------------------

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want)))
  (finish-output))
(defun mh-note (fmt a b)
  (format t fmt a b) (finish-output))

;;; ---- P0: multithreaded configuration, and the pre-hop baseline ----------

(defvar *warm* 0)
(setq *warm* (sb-thread:join-thread
              (sb-thread:make-thread (lambda () 1) :name "warm")))

(mh-note "~&region 0: size ~d bytes, live ~d bytes at probe start~%"
         (%gc-meta-read (+ *r0* #x10) *ms*)
         (- (mh-parked *r0* *ms*) (%gc-meta-read (+ *r0* #x00) *ms*)))
(format t "~&=== P1: WHERE ALLOCATION LANDS ===========================~%")
(chk "P1 pre-hop: a cons lands in REGION 0"
     (mh-where (cons 1 2) *scr* *r0* *ms*) 0)
(chk "P1 pre-hop: no region 0 -> main-region references yet"
     (mh-fwd *r0* *ms*) 0)

;;; ---- THE HOP (skipped under MAINHOP_CONTROL=1) --------------------------

(defvar *rcb* 0) (setq *rcb* (if (= *ctl* 1) 0 (mh-hop *r0* *ms*)))

(chk "P1 post-hop: a cons lands in MAIN'S region (control: region 0)"
     (mh-where (cons 3 4) *scr* *r0* *ms*) (if (= *ctl* 1) 0 1))
(chk "P1 post-hop: a fresh %INTERN-SYMBOL-PKG symbol STILL lands in region 0"
     (mh-where (%intern-symbol-pkg 1800000001 0) *scr* *r0* *ms*) 0)
(chk "P1 post-hop: FORMAT still answers"
     (length (format nil "mh-~d" 42)) 5)

(format t "~&=== P2: NO REGION-0 ALLOCATION OUTSIDE THE LOCK ==========~%")
(chk "P2 region 0's frontier is exactly still under compiled lock-free work"
     (mh-p2 *r0* *ms* 5000) 0)

(format t "~&=== P3: THE PUBLICATION AUDIT, ONE SITE AT A TIME ========~%")
(format t "  region 0 -> main's region, re-counted after each kind of~%")
(format t "  toplevel form.  The number IS the answer to the gate.~%")
(defvar *f0* 0) (setq *f0* (mh-fwd *r0* *ms*))

(defun mh-wl-add1 (x) (+ x 1))          ; a DEFUN, evaluated post-hop
(defvar *f-defun* 0) (setq *f-defun* (mh-fwd *r0* *ms*))
(mh-note "  after a DEFUN                +~d   (total ~d)~%"
         (- *f-defun* *f0*) *f-defun*)

(defvar *mh-wl-list* nil)
(setq *mh-wl-list* (list 10 20 30 40))  ; a heap value through SET-SYMBOL-VALUE
(defvar *f-defvar* 0) (setq *f-defvar* (mh-fwd *r0* *ms*))
(mh-note "  after a heap-valued DEFVAR/SETQ  +~d   (total ~d)~%"
         (- *f-defvar* *f-defun*) *f-defvar*)

(defvar *mh-wl-str* nil)
(setq *mh-wl-str* (concatenate 'string "mh-" (write-to-string 77)))
(defvar *f-setqs* 0) (setq *f-setqs* (mh-fwd *r0* *ms*))
(mh-note "  after a heap-STRING global       +~d   (total ~d)~%"
         (- *f-setqs* *f-defvar*) *f-setqs*)

(defvar *mh-fmt* nil) (setq *mh-fmt* (format nil "wl-~d-~d" 1 2))
(defvar *f-format* 0) (setq *f-format* (mh-fwd *r0* *ms*))
(mh-note "  after a FORMAT                   +~d   (total ~d)~%"
         (- *f-format* *f-setqs*) *f-format*)

(defvar *mh-sym* nil) (setq *mh-sym* (intern "MH-CL-FRESH-1" "COMMON-LISP-USER"))
(defvar *f-intern* 0) (setq *f-intern* (mh-fwd *r0* *ms*))
(mh-note "  after ONE CL:INTERN              +~d   (total ~d)~%"
         (- *f-intern* *f-format*) *f-intern*)

(chk "P3 the audit CAN answer non-zero (CL:INTERN row > 0; control: stays 0)"
     (if (> (- *f-intern* *f-format*) 0) 1 0) (if (= *ctl* 1) 0 1))
(chk "P3 the post-hop DEFUN is callable and correct" (mh-wl-add1 41) 42)
(chk "P3 the DEFVAR'd list reads back" (mh-chain-sum *mh-wl-list*) 100)

(format t "~&=== P4: FORCED COLLECTION OF MAIN'S REGION ===============~%")
(defvar *gc0* 0) (setq *gc0* (mh-gc-count))
(defvar *cap* nil) (setq *cap* (mh-chain 200))     ; sum 0..199 = 19900
;; CONTROL: the forced collection is SKIPPED — forcing region 0 to collect
;; with main inside it is a different, already-recorded hazard, not this one.
(defvar *capsum* 0)
(setq *capsum* (if (= *ctl* 1) (mh-chain-sum *cap*) (mh-collect-with *cap*)))
(defvar *gc1* 0) (setq *gc1* (mh-gc-count))
(chk "P4 main's region really collected (control: did not)"
     (- *gc1* *gc0*) (if (= *ctl* 1) 0 1))
(chk "P4 the argument-passed chain survived the collection" *capsum* 19900)
(format t "  Now the three region-0-table references into main's region:~%")
(chk "P4 the DEFVAR'd list still reads back after the collection"
     (mh-chain-sum *mh-wl-list*) 100)
(chk "P4 the post-hop DEFUN still calls after the collection"
     (mh-wl-add1 41) 42)
(chk "P4 the CL:INTERN'd symbol still resolves EQ after the collection"
     (if (eq (intern "MH-CL-FRESH-1" "COMMON-LISP-USER") *mh-sym*) 1 0) 1)

(format t "~&=== P5a: NATURAL COLLECTION UNDER COMPILED CODE ==========~%")
(format t "  ~~3x the semispace allocated INSIDE one compiled function —~%")
(format t "  the shape every worker test already survives.~%")
(defvar *p5a* nil) (setq *p5a* (mh-natural-compiled *r0* *ms*))
(chk "P5a the compiled loop ran to its count" (nth 0 *p5a*) 768)
(if (= *ctl* 1)
    (mh-note "  CONTROL: region 0's own natural collections under it: ~d~a~%"
             (nth 1 *p5a*) "   (informational -- headroom decides this)")
    (chk "P5a main's region collected naturally under it"
         (if (> (nth 1 *p5a*) 0) 1 0) 1))
(chk "P5a the live chain survived the natural collections" (nth 2 *p5a*) 28)
(mh-note "  natural collections inside compiled code: ~d   (chain-sum ~d)~%"
         (nth 1 *p5a*) (nth 2 *p5a*))
(chk "P5a state defined before it is still intact"
     (+ (mh-chain-sum *mh-wl-list*) (mh-wl-add1 41)) 142)

(format t "~&=== P5b: NATURAL COLLECTION UNDER EVALUATED CODE =========~%")
(format t "  ONE evaluated toplevel form allocating ~~3x the semispace.~%")
(format t "  The recorded hazard is the form being DESTROYED and re-run;~%")
(format t "  *mh-p5-entries* counts entries, the loop reports its count.~%")
(defvar *mh-p5-entries* 0)
(defvar *mh-p5-n* 0)
(defvar *gc2* 0) (setq *gc2* (mh-gc-count))
(progn
  (setq *mh-p5-entries* (+ *mh-p5-entries* 1))
  (let ((i 0) (s nil))
    (loop
      (when (>= i 768) (return nil))
      (setq s (make-string 65536))
      (setq i (+ i 1)))
    (setq *mh-p5-n* i)
    (if s 0 0)))
(defvar *gc3* 0) (setq *gc3* (mh-gc-count))
(mh-note "  natural collections of main's region during the form: ~d   (entries ~d)~%"
         (- *gc3* *gc2*) *mh-p5-entries*)
(chk "P5b the form ran to its count" *mh-p5-n* 768)
(chk "P5b the form ran exactly once" *mh-p5-entries* 1)
(if (= *ctl* 1)
    (mh-note "  CONTROL: region 0's own natural collections under it: ~d~a~%"
             (- *gc3* *gc2*) "   (informational)")
    (chk "P5b main's region really collected under natural pressure"
         (if (> (- *gc3* *gc2*) 0) 1 0) 1))
(chk "P5b the pre-P5 state is still intact (chain, defun, symbol)"
     (+ (mh-chain-sum *mh-wl-list*) (mh-wl-add1 41)
        (if (eq (intern "MH-CL-FRESH-1" "COMMON-LISP-USER") *mh-sym*) 1 0))
     143)

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "MAINHOP PROBE: PASS~%")
    (format t "MAINHOP PROBE: FAIL~%"))
(finish-output)

;;; TOPLEVEL — a SYS-EXIT nested inside a LET*/IF in a --script does not take
;;; effect; see test/hosted-term-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
