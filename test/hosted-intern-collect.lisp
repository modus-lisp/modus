;;;; hosted-intern-collect.lisp — THE SIGSEGV SUBJECT, NOW A TEST THAT CAN
;;;; REPORT: a worker INTERNS FRESH, then COLLECTS ITS OWN REGION, and lives.
;;;;
;;;;   test/run-intern-collect.sh [MODUS-BINARY] [RUNS] [K]
;;;;   INTC_K=<count> ./modus --script test/hosted-intern-collect.lisp
;;;;
;;;; ============================================================
;;;; WHY THIS FILE COULD NOT EXIST BEFORE THE FIX
;;;; ============================================================
;;;;
;;;; The campaign's root-cause section records the destructive demonstration:
;;;; a worker interning fresh names and then forcing ONE collection of its own
;;;; region took SIGSEGV, 3 of 3, because the symbols lived in the WORKER'S
;;;; region while region 0's tables pointed at them — pointers not on the
;;;; worker's stack, so its collector never updated them.  The demonstration
;;;; was deliberately kept OUT of the test suite: a test that segfaults cannot
;;;; report, so it could only ever be a rate of dead processes.
;;;;
;;;; TWO FIXES LATER IT CAN BE A TEST.  B-LITE (net/hosted-sync.lisp) gives
;;;; locked sections per-CPU slices of an IMMORTAL ARENA in region-0 address
;;;; space, and CL:INTERN (mvm/cl-packages.lisp) now runs under the lock with
;;;; the name COPIED there — so a worker's fresh symbol, its name and its
;;;; table entries all live in the arena, which NO region's collection ever
;;;; moves.  A collection of the worker's region then has nothing of the
;;;; intern's to strand.  That is the claim; this file measures it.
;;;;
;;;; WHAT ONE RUN DOES, all on the worker, all in compiled functions with
;;;; every address passed in:
;;;;   1. interns K fresh CL symbols, holding them in a list;
;;;;   2. records region-0/arena -> worker refs (must be 0 BEFORE collecting,
;;;;      or the collection tests nothing);
;;;;   3. forces ONE collection of ITS OWN region with a live cons chain;
;;;;   4. re-checks: the chain survived, every held symbol still resolves EQ
;;;;      through a fresh (intern name) — same object, post-collection — and
;;;;      the audit still reads 0;
;;;;   5. collects AGAIN (the campaign learned a copying collector's
;;;;      corruption often appears at the SECOND collection) and re-checks.
;;;;
;;;; WHAT WOULD MAKE THIS A LIE
;;;;
;;;;   THE COLLECTION MUST REALLY HAPPEN — the region's own count is asserted
;;;;   to rise by exactly 2 — and THE CHAIN IS THE COLLECTION'S OWN CONTROL:
;;;;   it lives in the collected region and must come back checksum-intact,
;;;;   so "nothing moved" and "everything moved correctly" are
;;;;   distinguishable.
;;;;
;;;;   THE MV HAND-OFF WORD IS CLEARED FIRST, like test/hosted-intern-layers:
;;;;   *MVM-LAST-MV* holds the last 2-valued call's cons (INTERN is 2-valued)
;;;;   — a known, separately-documented residual that is not this subject.
;;;;
;;;;   ONE RUN IS NOT A RESULT.  The pre-fix failure was 3 of 3 but the whole
;;;;   class is layout-sensitive; the runner reports a rate.

(%ha-actors-bringup 4 0)

(defvar *k*
  (let ((s (%cli-getenv "INTC_K")))
    (if (and s (> (length s) 0)) (parse-integer s) 200)))

(defun ic-fwd (r0)
  "region 0's live span AND the lock arena -> this worker's region."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0alloc (%gc-meta-read (+ r0 #x30) k))
         (ab (%rt-arena-base))
         (aa (%rt-arena-alloc)))
    (+ (%gc-count-foreign-refs r0from r0alloc wfrom wsize)
       (if (> aa ab) (%gc-count-foreign-refs ab aa wfrom wsize) 0))))

(defun ic-chain (n)
  (let ((c nil) (i 0))
    (loop (when (>= i n) (return 0))
      (setq c (cons i c))
      (setq i (+ i 1)))
    c))

(defun ic-chain-sum (c)
  (let ((s 0) (p c))
    (loop (when (null p) (return 0))
      (setq s (+ s (car p)))
      (setq p (cdr p)))
    s))

(defun ic-reintern-all-eq (held)
  "Re-intern every held symbol's NAME and count the ones that come back EQ.
   After a collection of this region, a symbol that had been stranded would
   come back as a DIFFERENT object (or the walk would die)."
  (let ((n 0) (p held))
    (loop (when (null p) (return 0))
      (when (eq (intern (symbol-name (car p)) "COMMON-LISP-USER") (car p))
        (setq n (+ n 1)))
      (setq p (cdr p)))
    n))

(defun ic-worker (r0 k)
  (let ((held nil)
        (i 0))
    (loop (when (>= i k) (return 0))
      (setq held (cons (intern (concatenate 'string "INTC-" (write-to-string i))
                               "COMMON-LISP-USER")
                       held))
      (setq i (+ i 1)))
    ;; The known, separately-documented interpreter MV hand-off residual —
    ;; not this subject; see test/hosted-intern-layers.lisp.
    (setq *mvm-last-mv* nil)
    (let ((f-pre (ic-fwd r0))
          (gc0 (%ha-my-gc-count))
          (chain (ic-chain 500)))          ; sum 0..499 = 124750
      (%ha-collect-here)
      (let ((sum1 (ic-chain-sum chain))
            (eq1 (ic-reintern-all-eq held))
            (f1 (ic-fwd r0)))
        (%ha-collect-here)
        (list f-pre
              (- (%ha-my-gc-count) gc0)
              sum1 eq1 f1
              (ic-chain-sum chain)
              (ic-reintern-all-eq held)
              (ic-fwd r0))))))

(defvar *res* nil)
(setq *res*
      (let ((r0 (%gc-region-0)) (k *k*))
        (sb-thread:join-thread
         (sb-thread:make-thread (lambda () (ic-worker r0 k)) :name "intc"))))

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

(format t "~&=== ~d FRESH INTERNS, THEN THE WORKER'S REGION COLLECTS TWICE ===~%" *k*)
(chk "audit 0 BEFORE collecting (or the collection tests nothing)"
     (nth 0 *res*) 0)
(chk "the worker's region really collected twice" (nth 1 *res*) 2)
(chk "the live chain survived collection 1" (nth 2 *res*) 124750)
(chk "every symbol still resolves EQ after collection 1" (nth 3 *res*) *k*)
(chk "audit still 0 after collection 1" (nth 4 *res*) 0)
(chk "the live chain survived collection 2" (nth 5 *res*) 124750)
(chk "every symbol still resolves EQ after collection 2" (nth 6 *res*) *k*)
(chk "audit still 0 after collection 2" (nth 7 *res*) 0)

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "INTERN THEN COLLECT: PASS~%")
    (format t "INTERN THEN COLLECT: FAIL~%"))
(finish-output)

;;; TOPLEVEL — a SYS-EXIT nested inside a LET*/IF in a --script does not take
;;; effect; see test/hosted-term-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
