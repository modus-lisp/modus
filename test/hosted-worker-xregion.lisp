;;;; hosted-worker-xregion.lisp — A WORKER'S FRESH INTERN PUTS THE SYMBOL IN THE
;;;; WRONG REGION, AND THE SHARED TABLE POINTS AT IT.
;;;;
;;;;   test/run-worker-xregion.sh [MODUS-BINARY]
;;;;   XREGION_ARM=intern|strings [XREGION_K=1500] ./modus --script <this>
;;;;
;;;; ============================================================
;;;; THE INVARIANT THIS MEASURES
;;;; ============================================================
;;;;
;;;; Per-region collection rests on ONE rule: no region may hold a pointer into
;;;; another region.  Nothing enforces it; %GC-COUNT-FOREIGN-REFS is the audit.
;;;;
;;;; %RT-ENTER exists so that the shared runtime tables and everything reachable
;;;; from them live in REGION 0 — it takes the runtime lock and hops allocation
;;;; there for the duration of the locked section, precisely so that a symbol
;;;; interned by a worker does not die when that worker's region collects.
;;;;
;;;; MEASURED: it does not.  A symbol interned by a worker is allocated in the
;;;; WORKER'S OWN REGION — the symbol object itself, not merely the name string
;;;; that was computed before the call — and REGION 0's tables then hold
;;;; pointers into it.  On this tree, after 1500 fresh interns:
;;;;
;;;;     last interned SYMBOL   -> the worker's region
;;;;     its NAME STRING        -> the worker's region
;;;;     foreign refs, region 0's live span -> the worker's region:  ~510
;;;;                                          (508 and 516 on two runs; the
;;;;                                          count is a conservative scan, so
;;;;                                          read it as "many", not as a total)
;;;;
;;;; and with the SAME loop allocating strings the worker merely KEEPS, instead
;;;; of interning: 0.  That is the whole difference.
;;;;
;;;; WHY IT IS FATAL RATHER THAN UNTIDY.  Those pointers are not on the worker's
;;;; stack, so the worker's own collector does not update them.  Force one
;;;; collection of the worker's region after the interning and the process takes
;;;; SIGSEGV — 3 of 3 — where the identical shape allocating strings survives
;;;; 3 of 3 and every object correctly MOVES.  That destructive demonstration is
;;;; deliberately NOT in this file: a test that segfaults cannot report.  It is
;;;; the `intern-fresh' arm of test/run-worker-intern.sh, and the crash it shows
;;;; is the same `MVM LONGJMP (TRAP #x0511) with no active handler-case' that
;;;; stops glass's RFB sender.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   IT CARRIES ITS OWN NEGATIVE CONTROL.  XREGION_ARM=strings runs the same
;;;;   loop, the same length, allocating and retaining the same strings, and
;;;;   differs only in not interning them.  It must audit ZERO.  If BOTH arms
;;;;   report zero the audit is not looking where it thinks it is, and this test
;;;;   says so rather than passing.
;;;;
;;;;   THE AUDIT IS CONSERVATIVE IN THE SAFE DIRECTION.  %GC-COUNT-FOREIGN-REFS
;;;;   counts any word that LOOKS like a pointer into the other span, so a false
;;;;   POSITIVE is possible and a false negative is not.  A non-zero count is
;;;;   therefore a reason to look, and the symbol/name addresses printed beside
;;;;   it are the exact, non-statistical evidence.
;;;;
;;;;   NOTHING IS COLLECTED HERE.  The audit runs with the worker's region at
;;;;   collection count 0, so what it finds is the state the program is in
;;;;   BEFORE anything has had a chance to dangle.

(defvar *k*
  (let ((s (%cli-getenv "XREGION_K")))
    (if (and s (> (length s) 0)) (parse-integer s) 1500)))

(defvar *arm*
  (let ((s (%cli-getenv "XREGION_ARM")))
    (if (and s (> (length s) 0)) s "intern")))

(defvar *scratch* 0)
(defvar *fail* 0)
(defvar *checks* 0)

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "ok   ~a~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "FAIL ~a~%" name))))

(defun in-span (a from size) (and (>= a from) (< a (+ from size))))

;;; Runs ON THE WORKER.  Everything it needs is passed in, because reading a
;;; global here would take the runtime lock and hop regions — which is the
;;; mechanism under test and has no business being in the instrument.
(defun xr-probe (scratch k r0 intern-p)
  (let* ((kk (%gc-meta-scale))
         (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) kk))
         (wto   (%gc-meta-read (+ rw #x08) kk))
         (wsize (%gc-meta-read (+ rw #x10) kk))
         (r0from (%gc-meta-read (+ r0 #x00) kk))
         (r0size (%gc-meta-read (+ r0 #x10) kk))
         (held nil)
         (i 0))
    (loop
      (when (>= i k) (return nil))
      (let ((name (format nil "XREG-~D" i)))
        (setq held (cons (if intern-p (intern name "COMMON-LISP-USER") name) held)))
      (setq i (+ i 1)))
    (let* ((obj (car held))
           (aobj (%gc-word-of obj scratch))
           (aname (%gc-word-of (if intern-p (symbol-name obj) obj) scratch))
           (r0alloc (%gc-meta-read (+ r0 #x30) kk))
           (foreign (%gc-count-foreign-refs r0from r0alloc wfrom wsize)))
      (list rw wfrom wto wsize r0from r0size aobj aname foreign
            (%gc-meta-read (+ rw #x20) kk)))))

(sb-thread:join-thread (sb-thread:make-thread (lambda () 1) :name "xr-gate"))
(setq *scratch* (%mmap-shared-page 4096))

(let* ((scratch *scratch*)
       (k *k*)
       (arm *arm*)
       (intern-p (string= arm "intern"))
       (r0 (%gc-region-0))
       (res (sb-thread:join-thread
             (sb-thread:make-thread (lambda () (xr-probe scratch k r0 intern-p))
                                    :name "xr-worker"))))
  (if (not (consp res))
      (progn (format t "~&the probe did not return: ~s~%" res)
             (setq *fail* (+ *fail* 1))
             (setq *checks* (+ *checks* 1))
             (finish-output))
      (let* ((wfrom (nth 1 res)) (wto (nth 2 res)) (wsize (nth 3 res))
             (r0from (nth 4 res)) (r0size (nth 5 res))
             (aobj (nth 6 res)) (aname (nth 7 res))
             (foreign (nth 8 res)) (wgc (nth 9 res))
             (obj-in-worker (or (in-span aobj wfrom wsize) (in-span aobj wto wsize)))
             (name-in-worker (or (in-span aname wfrom wsize) (in-span aname wto wsize))))
        (format t "~&=== ARM ~a, ~d iterations ===~%" arm k)
        (format t "~&worker region collections during the run: ~d~%" wgc)
        (format t "~&last object   at ~x -> ~a~%" aobj
                (if obj-in-worker "THE WORKER'S REGION" "region 0 / elsewhere"))
        (format t "~&its name      at ~x -> ~a~%" aname
                (if name-in-worker "THE WORKER'S REGION" "region 0 / elsewhere"))
        (format t "~&FOREIGN REFS out of region 0's live span into the worker's region: ~d~%"
                foreign)
        (format t "~&~%")
        (chk-true "the audit ran with the worker's region uncollected" (= wgc 0))
        (chk-true "region 0's live span is plausible"
                  (and (> r0from 0) (> r0size 0)))
        (if intern-p
            (progn
              (chk-true "NO pointer from region 0 into the worker's region" (= foreign 0))
              (chk-true "the interned SYMBOL is not in the worker's region"
                        (not obj-in-worker))
              (chk-true "the symbol's NAME is not in the worker's region"
                        (not name-in-worker)))
            (chk-true "CONTROL: strings the worker merely keeps are not referenced from region 0"
                      (= foreign 0)))
        (format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
        (if (= *fail* 0)
            (format t "WORKER CROSS-REGION AUDIT (~a): PASS~%" arm)
            (format t "WORKER CROSS-REGION AUDIT (~a): FAIL~%" arm))
        (finish-output))))

;;; THE EXIT IS AT TOPLEVEL, and that is not style.  SYS-EXIT called from
;;; inside a nested LET*/IF in a --script did NOT take effect here: the process
;;; ran to the end of the file and exited 0 while the verdict line above it
;;; said FAIL.  A test whose exit code disagrees with its own output is worse
;;; than no test, because every runner reads the code and not the prose.
(sys-exit (if (= *fail* 0) 0 1))
