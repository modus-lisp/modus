;;;; hosted-region0-frontier.lisp — DO THE MAIN THREAD AND A WORKER SHARE ONE
;;;; ALLOCATION FRONTIER IN REGION 0?  MEASURED, IN ADDRESSES.
;;;;
;;;;   ./modus --script test/hosted-region0-frontier.lisp
;;;;
;;;; ============================================================
;;;; THE STATEMENT
;;;; ============================================================
;;;;
;;;; A worker that takes the runtime lock allocates in REGION 0, and it takes
;;;; region 0's allocation frontier out of region 0's CONTROL BLOCK (+0x30) —
;;;; the value `%GC-REGION-ENTER' parked there the last time somebody LEFT
;;;; region 0.  The MAIN thread's frontier for region 0 is in REGISTERS, and
;;;; main allocates in region 0 continuously and OUTSIDE the lock, because
;;;; region 0 is its heap.  Nothing keeps the two in step between hops.
;;;;
;;;; `%RT-LEAVE-LOCKED''s own docstring says two threads in region 0 with one
;;;; parked frontier between them is the corruption the lock exists to prevent.
;;;; The lock does not cover main's ordinary allocation.  This file asks, in
;;;; addresses rather than by argument, whether that gap is real and whether a
;;;; worker's fresh symbol lands in memory main also used.
;;;;
;;;; ============================================================
;;;; WHY THIS IS NOT THE LEAD THAT WAS ALREADY REFUTED
;;;; ============================================================
;;;;
;;;; CLAUDE.md refutes "the runtime-lock region hop" three ways and all three
;;;; are about the HOP, which allocates nothing in region 0: a worker taking
;;;; the lock 200 000 times is clean; 20 hops with no intern is 6 of 6 clean;
;;;; and MAIN_ROUNDS=0 still fails — but "main's loop does not cons" is not
;;;; "main does not allocate", because main is still EVALUATING THE SCRIPT and
;;;; mvm-eval allocates in region 0 on every form.
;;;;
;;;; The statement here is narrower: a worker that ALLOCATES at region 0's
;;;; parked frontier while main's live frontier is somewhere else.  That is
;;;; what separates the arms in test/run-intern-shape.sh — `str' (a worker
;;;; allocating in its OWN region) 10 of 10, `main' (the same intern loop on
;;;; the main thread) 10 of 10, `bare' (a worker interning, i.e. allocating in
;;;; region 0 under the lock) 3 of 10 — and it is what separates the GREEN test
;;;; from the red ones: `%TL-SELFTEST' puts the MAIN thread in a carved region
;;;; of its own (`(%gc-region-enter rcb2)' before it spawns), so region 0 there
;;;; is nobody's mutator heap and its parked frontier is only ever moved by
;;;; lock holders.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE TWO READINGS MUST BE ATOMIC WITH RESPECT TO THE EVALUATOR.  A first
;;;;   version of this file read the marker's address in one toplevel form and
;;;;   the parked field in the next, and measured a megabyte of gap that was
;;;;   simply mvm-eval's own allocation in between — a number that says nothing.
;;;;   Every reading below is taken inside ONE COMPILED FUNCTION, with the
;;;;   region block, the metadata scale and the scratch address passed IN as
;;;;   arguments, so nothing in the probe resolves an implicit global (which
;;;;   would be a SYMBOL-VALUE, which takes the runtime lock, which
;;;;   REPUBLISHES the very field being measured and would manufacture a
;;;;   false "in step").
;;;;
;;;;   A CONS'S ADDRESS IS A PROXY FOR THE LIVE FRONTIER, and a sound one:
;;;;   GET-ALLOC-PTR reads 0 from evaluated code (CLAUDE.md), and a cons
;;;;   allocated on the spot sits at the frontier less its own 16 bytes.
;;;;
;;;;   THE INSTRUMENT MUST ANSWER BOTH WAYS.  Main takes the runtime lock
;;;;   itself and re-probes: `%GC-REGION-ENTER' parks the region it LEAVES, so
;;;;   that reading must come back IN STEP.  A probe that could not see the
;;;;   field at all would report the same gap twice.
;;;;
;;;;   THE GATE MUST BE ON.  With no thread ever started `%RT-ENTER' is a
;;;;   no-op.  A throwaway thread is started and joined first (that is what
;;;;   runs net/sb-thread-shim.lisp's %SB-THREADS-UP), and the acquisition
;;;;   counter is required to have moved.
;;;;
;;;;   ONE READING IS NOT A FINDING.  The gap is a function of how much main
;;;;   has allocated since its last hop, so it MOVES between runs.  What must
;;;;   not move is its sign.

(%ha-actors-bringup 4 0)

(defvar *r0* 0)  (setq *r0* (%gc-region-0))
(defvar *scr* 0) (setq *scr* (%thr-scratch))
(defvar *ms* 0)  (setq *ms* (%gc-meta-scale))

;;; ---- THE PROBES.  Everything is an ARGUMENT: an implicit global here would
;;; ---- be a SYMBOL-VALUE, which takes the lock, which republishes the field.

(defun gap-probe (r0 ms scr)
  "(live-frontier-proxy parked-frontier), read with nothing in between."
  (let* ((c (cons 7 8))
         (a (%gc-word-of c (+ scr 512)))
         (p (%gc-meta-read (+ r0 #x30) ms)))
    (list a p)))

(defun live-probe (scr)
  "Main's live frontier, as the address of a cons allocated on the spot."
  (let ((c (cons 1 2)))
    (%gc-word-of c (+ scr 512))))

(defun w-probe (r0 ms scr)
  "ON THE WORKER.  Region 0's parked frontier as the worker sees it on the way
   in, and where the fresh symbol actually landed.  ONE intern: this is a
   measurement, not a stress, and the shorter the exposure the likelier it is
   to come back and report at all.

   AND TWO MARKED CONSES, WHICH ARE THE OVERWRITE TEST AND ITS CONTROL.  Both
   are allocated by the SAME worker in the SAME function microseconds apart and
   differ in exactly one thing — which region they land in.  `hot' is allocated
   under the runtime lock, so it is in REGION 0, where the main thread is also
   bump-allocating from its own registers.  `own' is allocated outside the
   lock, in the worker's own region, which nothing else touches.  The driver
   reads both back AFTER the join.  If `hot' comes back changed and `own' does
   not, the difference is the region and not the worker, not the thread and not
   the timing."
  (let ((p (%gc-meta-read (+ r0 #x30) ms)))
    (let ((s nil) (hot nil) (own nil))
      (%rt-enter)
      (setq s (%intern-symbol-pkg 1700000001 0))
      (setq hot (cons 123456789 987654321))
      (%rt-leave)
      (setq own (cons 123456789 987654321))
      (list p (%gc-word-of s (+ scr 512)) hot own
            (%gc-word-of hot (+ scr 512))
            (%gc-word-of own (+ scr 512))))))

;;; ---- 0. SWITCH THE PROCESS TO THE MULTITHREADED CONFIGURATION ----------

(defvar *warm* 0)
(setq *warm* (sb-thread:join-thread
              (sb-thread:make-thread (lambda () 1) :name "warm")))

(defvar *acq0* 0) (setq *acq0* (%rt-acquisitions))

;;; ---- 1. MAIN'S LIVE FRONTIER versus REGION 0'S PARKED ONE --------------

(defvar *g1* nil) (setq *g1* (gap-probe *r0* *ms* *scr*))

;;; ---- 2. THE CONTROL: MAIN TAKES THE LOCK, THEN PROBES AGAIN ------------
;;; %RT-LEAVE's %GC-REGION-ENTER parks region 0 from the live registers, so
;;; this reading must come back in step.

(defvar *g2* nil)
(setq *g2* (progn (%rt-enter) (%rt-leave) (gap-probe *r0* *ms* *scr*)))

(defvar *acq1* 0) (setq *acq1* (%rt-acquisitions))

;;; ---- 3. WHERE A WORKER'S FRESH SYMBOL LANDS ---------------------------

(defvar *l0* 0) (setq *l0* (live-probe *scr*))

(defvar *wres* nil)
(setq *wres*
      (let ((r0 *r0*) (ms *ms*) (scr *scr*))
        (sb-thread:join-thread
         (sb-thread:make-thread (lambda () (w-probe r0 ms scr)) :name "r0f"))))

(defvar *l1* 0) (setq *l1* (live-probe *scr*))

(defvar *wp* 0) (setq *wp* (nth 0 *wres*))
(defvar *ws* 0) (setq *ws* (nth 1 *wres*))
(defvar *hot* nil) (setq *hot* (nth 2 *wres*))
(defvar *own* nil) (setq *own* (nth 3 *wres*))
(defvar *hota* 0) (setq *hota* (nth 4 *wres*))
(defvar *owna* 0) (setq *owna* (nth 5 *wres*))

;;; ---- REPORT ------------------------------------------------------------

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "ok   ~a~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "FAIL ~a~%" name))))

(format t "~&=== ONE REGION, TWO FRONTIERS ============================~%")
(format t "  main's live frontier   ~d~%" (nth 0 *g1*))
(format t "  region 0 parked        ~d   (live is ~d bytes AHEAD)~%"
        (nth 1 *g1*) (- (nth 0 *g1*) (nth 1 *g1*)))
(format t "  THE CONTROL — the same probe right after main took the lock:~%")
(format t "  main's live frontier   ~d~%" (nth 0 *g2*))
(format t "  region 0 parked        ~d   (live is ~d bytes AHEAD)~%"
        (nth 1 *g2*) (- (nth 0 *g2*) (nth 1 *g2*)))
(format t "  runtime-lock acquisitions  ~d -> ~d~%" *acq0* *acq1*)

(format t "~&=== WHERE THE WORKER'S FRESH SYMBOL LANDED ===============~%")
(format t "  main's live frontier before the spawn  ~d~%" *l0*)
(format t "  region 0 parked, as the worker saw it   ~d~%" *wp*)
(format t "  the worker's symbol                     ~d~%" *ws*)
(format t "  main's live frontier after the join     ~d~%" *l1*)
(format t "  the symbol is ~a the span main allocated across the worker's life~%"
        (if (and (>= *ws* *l0*) (< *ws* *l1*)) "INSIDE" "outside"))

(format t "~&=== TWO MARKED CONSES FROM THE SAME WORKER ===============~%")
(format t "  Same worker, same function, microseconds apart.  `hot' was~%")
(format t "  allocated under the runtime lock, so it is in REGION 0, where~%")
(format t "  the main thread is also bump-allocating from its registers;~%")
(format t "  `own' was allocated outside the lock, in the worker's own~%")
(format t "  region, which nothing else touches.  Both held 123456789 .~%")
(format t "  987654321 when the worker made them; read back after the join:~%")
(format t "  hot @ ~d   car ~s   cdr ~s~%" *hota* (car *hot*) (cdr *hot*))
(format t "  own @ ~d   car ~s   cdr ~s~%" *owna* (car *own*) (cdr *own*))

(format t "~&=== THE INSTRUMENT ANSWERS BOTH WAYS =====================~%")
(chk-true "the runtime lock was really taken on main" (> *acq1* *acq0*))
(chk-true "main really allocated while the worker ran" (> *l1* *l0*))
(chk-true "taking the lock brings the two frontiers back in step"
          (< (- (nth 0 *g2*) (nth 1 *g2*)) 64))

(format t "~&=== THE ASSERTION ========================================~%")
(format t "  Region 0 has ONE allocation frontier.  Main's copy of it and~%")
(format t "  the copy a worker is handed must not be different numbers, and~%")
(format t "  the worker's symbol must not be inside memory main also used.~%")
(chk-true "main's live frontier and region 0's parked one are in step"
          (< (- (nth 0 *g1*) (nth 1 *g1*)) 64))
(chk-true "the worker's symbol is NOT inside main's own allocation span"
          (not (and (>= *ws* *l0*) (< *ws* *l1*))))
;; THE CONTROL FIRST, so a failure of it is read as "the worker is broken"
;; rather than "region 0 is".
(chk-true "the CONTROL: the worker's own-region cons survived intact"
          (and (eql (car *own*) 123456789) (eql (cdr *own*) 987654321)))
(chk-true "the worker's REGION 0 cons survived intact"
          (and (eql (car *hot*) 123456789) (eql (cdr *hot*) 987654321)))

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "REGION 0 FRONTIER: PASS~%")
    (format t "REGION 0 FRONTIER: FAIL~%"))
(finish-output)

;;; TOPLEVEL — a SYS-EXIT nested inside a LET*/IF in a --script does not take
;;; effect; see test/hosted-term-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
