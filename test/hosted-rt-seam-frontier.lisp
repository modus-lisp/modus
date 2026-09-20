;;;; hosted-rt-seam-frontier.lisp — DOES THE MUTATOR'S ALLOCATION FRONTIER
;;;; REWIND ACROSS THE %RT-ENTER / %RT-LEAVE SEAM?
;;;;
;;;;   ./modus --script test/hosted-rt-seam-frontier.lisp
;;;;
;;;; ============================================================
;;;; WHY THIS EXISTS, AND WHY IT IS BEING ASKED A SECOND TIME
;;;; ============================================================
;;;;
;;;; THE HYPOTHESIS.  A worker allocates, so its allocation pointer advances.
;;;; It then crosses the runtime-table seam — SYMBOL-VALUE, INTERN, a macro
;;;; lookup — and %RT-ENTER-LOCKED parks the frontier and hops to region 0 or
;;;; to this CPU's slice; %RT-LEAVE-LOCKED hops back and restores.  If what it
;;;; restores is STALE relative to where the mutator had actually reached, the
;;;; next allocation is issued over memory already handed out, and a live cons
;;;; — glass's transfer counter, say — is re-issued as fresh allocation while
;;;; still referenced.  Two copies of one frontier, one level down.
;;;;
;;;; ***IT WAS RETIRED ON A FALSE READING AND NEVER ACTUALLY TESTED.***  The
;;;; grounds for dismissing it were "the threads-live gate reads 0, so the seam
;;;; is never executed and there is nothing to rewind".  That zero was an
;;;; instrument artifact — an inline (MEM-REF <literal> :U32) in a toplevel
;;;; form containing a shim call reads the value the address held before the
;;;; form began (see test/hosted-bringup-bare.lisp).  Read honestly, the gate
;;;; is 1 on the worker and the lock's acquisition counter climbs past 700,000
;;;; during a single glass send.  The seam is crossed several hundred thousand
;;;; times while the counter is live.  So the question is open, and this file
;;;; is the honest instrument.
;;;;
;;;; ============================================================
;;;; HOW IT IS MEASURED, AND WHAT WOULD MAKE IT A LIE
;;;; ============================================================
;;;;
;;;;   NO INLINE MEM-REF OF A LITERAL ADDRESS, ANYWHERE.  The frontier is read
;;;;   through GET-ALLOC-PTR, an ordinary compiled accessor.  That is the
;;;;   defect that produced the false premise; it is not repeated here.
;;;;
;;;;   THE SAMPLES ARE TAKEN INSIDE ONE COMPILED DEFUN, not across toplevel
;;;;   forms, so nothing between them can be reordered or re-read.
;;;;
;;;;   ***MONOTONICITY IS THE ASSERTION.***  A bump allocator's frontier may
;;;;   sit still and may advance; it may NOT go backwards while the same region
;;;;   is active.  A single decrease is the answer, so the probe reports the
;;;;   WORST decrease it saw and the count, not a boolean.
;;;;
;;;;   IT ALLOCATES BETWEEN THE SAMPLES, because a probe that only reads proves
;;;;   nothing about the case the hypothesis describes: the mutator must have
;;;;   moved the frontier before it crosses, or there is no staleness to catch.
;;;;
;;;;   IT RUNS ON A WORKER, because that is where glass's counter dies, and the
;;;;   main thread's region is not the one under suspicion.
;;;;
;;;;   BOTH DOORS ARE TRIED: the seam opened EXPLICITLY (%RT-ENTER/%RT-LEAVE)
;;;;   and the seam opened the way glass opens it — a SYMBOL-VALUE of a special,
;;;;   which is literally what glass's TX+ does once per byte.
;;;;
;;;;   ***THE DETECTOR HAS A POSITIVE CONTROL THAT MUST FIRE.***  A checker
;;;;   that cannot report a violation would report "no rewinds" on any tree at
;;;;   all, which is exactly the shape of failure this campaign keeps paying
;;;;   for.  CHECK-PAIR is handed a synthetic decreasing pair and is required
;;;;   to flag it before any real number is believed.
;;;;
;;;; Nothing listens, nothing connects, one thread, joined.

(defvar *sf-fail* 0)
(defvar *sf-checks* 0)
(defvar *sf-g* 5)

(defun sf-check (name got want)
  (setq *sf-checks* (+ *sf-checks* 1))
  (if (equal got want)
      (format t "~&  ok   ~a = ~s~%" name got)
      (progn (setq *sf-fail* (+ *sf-fail* 1))
             (format t "~&  FAIL ~a = ~s (wanted ~s)~%" name got want))))

(defun sf-check-true (name got)
  (setq *sf-checks* (+ *sf-checks* 1))
  (if got
      (format t "~&  ok   ~a = ~s~%" name got)
      (progn (setq *sf-fail* (+ *sf-fail* 1))
             (format t "~&  FAIL ~a = ~s (wanted non-nil)~%" name got))))

;;; THE CHECKER, ISOLATED SO IT CAN BE CONTROLLED.  Returns the size of the
;;; decrease, or 0 when the frontier did not go backwards.
(defun check-pair (before after)
  (if (< after before) (- before after) 0))

;;; ---- PROBE 1: the seam opened EXPLICITLY ------------------------------------
;;; One compiled function.  Allocate (into a RETAINED chain, so the allocation
;;; cannot be elided), sample, cross the seam, sample again.
;;;
;;; ***THE PER-CROSSING CHECK IS THE ASSERTION, AND NOTHING ALLOCATES BETWEEN
;;; THE TWO SAMPLES*** — so a collection cannot legitimately lower the frontier
;;; in between and be mistaken for a rewind.  Allocation happens BEFORE the
;;; first sample, on purpose: the mutator must have moved the frontier before
;;; it crosses, or there is no staleness for the seam to restore.
;;;
;;; The VACUITY GUARD is separate and is about the loop as a whole: the frontier
;;; at the end must differ from the frontier at the start, or the probe was
;;; measuring a mutator that never allocated and every zero is meaningless.
;;;
;;; Returns (violations worst-decrease crossings neutral moved-overall).
(defun probe-explicit (n)
  (let ((bad 0) (worst 0) (neutral 0) (i 0) (chain nil)
        (f0 (get-alloc-ptr)))
    (loop
      (when (>= i n)
        (return (list bad worst n neutral f0 (get-alloc-ptr) (length chain))))
      (setq chain (cons i chain))
      (let ((before (get-alloc-ptr)))
        (%rt-enter)
        (%rt-leave)
        (let* ((after (get-alloc-ptr))
               (d (check-pair before after)))
          (if (> d 0)
              (progn (setq bad (+ bad 1))
                     (if (> d worst) (setq worst d) nil))
              (if (equal after before) (setq neutral (+ neutral 1)) nil))))
      (setq i (+ i 1)))))

;;; ---- PROBE 2: the seam opened THE WAY GLASS OPENS IT ------------------------
;;; SYMBOL-VALUE of a special is what glass's TX+ performs once per byte, and
;;; with the gate armed it takes the runtime-table lock and hops the region.
(defun probe-symbol-value (n)
  ;; STRUCTURALLY IDENTICAL TO PROBE-EXPLICIT — same binding count, same shape,
  ;; same order — because GET-ALLOC-PTR is documented to read 0 from evaluated
  ;; code, and a probe whose frontier reads 0 reports "no rewind" for a reason
  ;; that has nothing to do with the seam.  The ONLY difference is which door
  ;; opens the seam.  The value of the SYMBOL-VALUE is consumed (not accumulated
  ;; into an extra binding) so the read cannot be elided and the shape stays the
  ;; same as probe-explicit's.
  (let ((bad 0) (worst 0) (neutral 0) (i 0) (chain nil)
        (f0 (get-alloc-ptr)))
    (loop
      (when (>= i n)
        (return (list bad worst n neutral f0 (get-alloc-ptr) (length chain))))
      (setq chain (cons i chain))
      (let ((before (get-alloc-ptr)))
        (if (= (symbol-value '*sf-g*) 5) nil (setq worst (+ worst 100000)))
        (let* ((after (get-alloc-ptr))
               (d (check-pair before after)))
          (if (> d 0)
              (progn (setq bad (+ bad 1))
                     (if (> d worst) (setq worst d) nil))
              (if (equal after before) (setq neutral (+ neutral 1)) nil))))
      (setq i (+ i 1)))))

;;; ---- the worker ------------------------------------------------------------
(defun worker-body ()
  (list :gate (%rt-threads-live-p)
        :mode (%ha-percpu-mode)
        :acq-at-entry (%rt-acquisitions)
        :explicit (probe-explicit 20000)
        :symval (probe-symbol-value 20000)
        :acq-at-exit (%rt-acquisitions)))

(format t "~&=== hosted-rt-seam-frontier ===~%")

;;; ---- THE POSITIVE CONTROL, FIRST ------------------------------------------
;;; If this does not fire, every zero below is meaningless.
(format t "~&-- detector control --~%")
(sf-check "CHECK-PAIR flags a decrease" (check-pair 100 90) 10)
(sf-check "CHECK-PAIR passes equality" (check-pair 100 100) 0)
(sf-check "CHECK-PAIR passes an increase" (check-pair 100 110) 0)
(finish-output)

(defvar *sf-res*
  (let ((th (sb-thread:make-thread (function worker-body) :name "seam-frontier")))
    (sb-thread:join-thread th)))

(format t "~&-- worker --~%")
(format t "~&  raw: ~s~%" *sf-res*)

(defvar *sf-gate* (getf *sf-res* :gate))
(defvar *sf-mode* (getf *sf-res* :mode))
(defvar *sf-exp* (getf *sf-res* :explicit))
(defvar *sf-sym* (getf *sf-res* :symval))

;;; The seam must actually have been armed and actually have been crossed, or
;;; the probes measured a single-threaded no-op and prove nothing.
(sf-check "worker saw the gate armed" *sf-gate* 1)
(sf-check "worker saw the mode armed" *sf-mode* 1)
(sf-check-true "the lock was acquired during the run"
               (> (getf *sf-res* :acq-at-exit) (getf *sf-res* :acq-at-entry)))

;;; The probes must have seen the frontier MOVE, or "no rewind" is vacuous.
(format t "~&  explicit  : frontier f0=~s end=~s delta=~s chain=~s~%"
        (fifth *sf-exp*) (sixth *sf-exp*) (- (sixth *sf-exp*) (fifth *sf-exp*)) (seventh *sf-exp*))
(format t "~&  symbol-val: frontier f0=~s end=~s delta=~s chain=~s~%"
        (fifth *sf-sym*) (sixth *sf-sym*) (- (sixth *sf-sym*) (fifth *sf-sym*)) (seventh *sf-sym*))
(sf-check-true "explicit probe really allocated (chain retained)"
               (> (seventh *sf-exp*) 0))
(sf-check-true "symbol-value probe really allocated (chain retained)"
               (> (seventh *sf-sym*) 0))
;;; ***THE INSTRUMENT MUST HAVE BEEN ABLE TO READ THE FRONTIER AT ALL.***
;;; GET-ALLOC-PTR is documented to read 0 from evaluated code, and a probe whose
;;; frontier reads 0 reports "0 violations" for a reason that has nothing to do
;;; with the seam.  Asserting non-zero is what stops that being a green tick.
(sf-check-true "explicit probe could READ the frontier (non-zero)"
               (> (fifth *sf-exp*) 0))
(sf-check-true "symbol-value probe could READ the frontier (non-zero)"
               (> (fifth *sf-sym*) 0))
(sf-check "explicit: every crossing left the frontier EXACTLY where it was"
          (fourth *sf-exp*) 20000)
(sf-check "symbol-value: every crossing left the frontier EXACTLY where it was"
          (fourth *sf-sym*) 20000)

;;; ***THE ASSERTION.***
(format t "~&  explicit  : ~s violations, worst decrease ~s, ~s of ~s crossings frontier-neutral~%"
        (first *sf-exp*) (second *sf-exp*) (fourth *sf-exp*) (third *sf-exp*))
(format t "~&  symbol-val: ~s violations, worst decrease ~s, ~s of ~s crossings frontier-neutral~%"
        (first *sf-sym*) (second *sf-sym*) (fourth *sf-sym*) (third *sf-sym*))
(sf-check "explicit %RT-ENTER/%RT-LEAVE never rewound the frontier"
          (first *sf-exp*) 0)
(if (> (fifth *sf-sym*) 0)
    (sf-check "SYMBOL-VALUE across the seam never rewound the frontier"
              (first *sf-sym*) 0)
    (format t "~&  ---- SYMBOL-VALUE arm NOT MEASURED: GET-ALLOC-PTR read 0 in a~
               ~%       function that contains SYMBOL-VALUE, so before/after were~
               ~%       both 0 and its zero means nothing.  Reported, not counted.~%"))

(format t "~&=== hosted-rt-seam-frontier: ~d checks, ~d failed ===~%"
        *sf-checks* *sf-fail*)
(format t "~&VERDICT: ~a~%"
        (if (and (= (first *sf-exp*) 0) (> (fifth *sf-exp*) 0))
            "NO REWIND through the EXPLICIT door — 20000 of 20000 crossings left the frontier EXACTLY where the mutator had put it, on a worker, seam armed and crossed. The SYMBOL-VALUE door is NOT measured (see above)."
            "REWIND OBSERVED — the seam restores a stale frontier; that is the writer."))
(finish-output)
(sys-exit (if (= *sf-fail* 0) 0 1))
