;;;; hosted-intern-layers.lisp — THERE ARE TWO INTERNS, AND ONLY ONE OF THEM IS
;;;; THE DEFECT.
;;;;
;;;;   test/run-intern-layers.sh [MODUS-BINARY] [RUNS] [K]
;;;;   ARM=str|low|cl  K=<count>  ./modus --script test/hosted-intern-layers.lisp
;;;;
;;;; ============================================================
;;;; WHAT THIS CORRECTS
;;;; ============================================================
;;;;
;;;; CLAUDE.md said, under THE ROOT CAUSE, MEASURED: "a worker's INTERN puts the
;;;; symbol in the WRONG REGION", and named %INTERN-SYMBOL-PKG — the runtime's
;;;; hot low-level intern — as the operation at fault.  The whole per-actor
;;;; intern-store design was drawn against that.
;;;;
;;;; IT IS NOT THAT FUNCTION.  %INTERN-SYMBOL-PKG is already wrapped in
;;;; %RT-ENTER / %RT-LEAVE (mvm/prelude.lisp), and %RT-ENTER's whole job is to
;;;; make REGION 0 the active heap for the duration — so a fresh symbol interned
;;;; through it on a worker is allocated in region 0, co-located with the table
;;;; at 0x10000088 that registers it, and leaves NO region-0 -> worker pointer.
;;;; That is what the `low' arm below measures.
;;;;
;;;; THE OPERATION AT FAULT IS `CL:INTERN' (mvm/cl-packages.lisp), WHICH IS NOT
;;;; UNDER THE LOCK AT ALL.  Its create-new-symbol branch allocates the CL
;;;; symbol (%MAKE-CL-SYMBOL), its name string and the package symtab entry in
;;;; the CALLING thread's region, and then links all three into structures that
;;;; live in region 0 — the shared intern table at 0x10000088 AND the package
;;;; object's own internal symtab.  That is what the `cl' arm measures, and it
;;;; is the arm that is red.
;;;;
;;;; It also explains the tests: test/hosted-worker-intern.lisp's fatal
;;;; `intern-fresh' arm calls CL:INTERN, not %INTERN-SYMBOL-PKG, and
;;;; test/hosted-term-xregion.lisp's symbol case reaches CL:INTERN through
;;;; TERM-DECODE-STEP's tag-6 arm.
;;;;
;;;; ============================================================
;;;; THERE ARE TWO DEFECTS HERE AND THEY ARE NOT THE SAME DEFECT
;;;; ============================================================
;;;;
;;;; Measured on this tree, K=20, one process per run:
;;;;
;;;;   str    10 of 10 clean          the allocation control
;;;;   low     5 of 10 clean          audit ZERO whenever it completes
;;;;   cl      0 of 8  clean          audit +44, EVERY run, never a crash
;;;;
;;;; D1 — CROSS-REGION POINTERS, AND IT IS DETERMINISTIC.  `cl' fails its audit
;;;; 8 times out of 8 and its process never dies: it reports +44 region-0 ->
;;;; worker pointers for 20 fresh symbols and exits on the check.  Linear in K
;;;; (+36 at K=20 and +296 at K=200 in the probe this file grew out of).  This
;;;; is the number test/hosted-term-xregion.lisp reports as 29.
;;;;
;;;; D2 — THE LOW-LEVEL INTERN IS LETHAL AT ABOUT ONE RUN IN TWO, AND THE
;;;; CROSS-REGION MECHANISM DOES NOT EXPLAIN IT.  `low' dies 5 times in 10 with
;;;; `MVM LONGJMP (TRAP #x0511) with no active handler-case' — the campaign's
;;;; headline signature — WHILE ITS AUDIT READS ZERO on every run that survives.
;;;; So a fresh %INTERN-SYMBOL-PKG on a worker leaves no forbidden pointer and
;;;; still takes the process down half the time.  `str' at 10 of 10 rules out
;;;; "any loop on a worker dies".  What `low' does that `str' does not is take
;;;; the runtime lock and hop the active region to region 0 (%RT-ENTER); that is
;;;; the suspect, and it is NOT established here.
;;;;
;;;; D2 MATTERS FOR THE FIX: it means "put the allocation in region 0 under the
;;;; lock" — the remedy this tree already applies to %INTERN-SYMBOL-PKG and the
;;;; obvious one to extend to CL:INTERN — is a remedy whose own arm is red.
;;;; Bracketing a CL:INTERN call site with %RT-ENTER/%RT-LEAVE by hand, with no
;;;; rebuild, reproduces the same signature at K=1.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   THE AUDIT MUST BE ABLE TO ANSWER NON-ZERO.  The `low' arm and the `cl'
;;;;   arm are the SAME audit (%GC-COUNT-FOREIGN-REFS), over the SAME two spans,
;;;;   in the SAME direction, on the SAME worker, differing only in which intern
;;;;   is called.  They disagree.  An instrument that distinguishes them is not
;;;;   reporting 0 because it is looking at nothing.
;;;;
;;;;   BOTH ARMS MUST REALLY HAVE CREATED SYMBOLS.  A `low' arm that quietly
;;;;   found K existing symbols would report 0 for the wrong reason.  The shared
;;;;   table's COUNT is sampled before and after and is required to have grown by
;;;;   exactly K — the fresh-intern odometer CLAUDE.md already established.
;;;;
;;;;   THE `str' ARM IS THE ALLOCATION CONTROL.  A worker that merely allocates
;;;;   K strings must leave the audit at 0, or the number the `cl' arm reports is
;;;;   "a worker allocated", not "a worker interned".
;;;;
;;;;   ONE RUN IS NOT A RESULT.  Everything in this campaign is layout-sensitive.
;;;;   The runner does RUNS passes per arm and reports the rate; read that.

(%ha-actors-bringup 4 0)

(defvar *arm*
  (let ((s (%cli-getenv "ARM"))) (if (and s (> (length s) 0)) s "cl")))
(defvar *k*
  (let ((s (%cli-getenv "K"))) (if (and s (> (length s) 0)) (parse-integer s) 20)))

(defvar *r0* 0) (setq *r0* (%gc-region-0))
(defvar *scr* 0) (setq *scr* (%thr-scratch))

(defun il-count () (hash-table-count (mem-ref #x10000088 :u64)))

(defun il-fwd (r0)
  "region 0's LIVE span -> this worker's region.  The forbidden direction."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0alloc (%gc-meta-read (+ r0 #x30) k)))
    (%gc-count-foreign-refs r0from r0alloc wfrom wsize)))

(defun il-in-worker (w)
  "1 when machine word W points inside THIS worker's region, 0 otherwise."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (from (%gc-meta-read (+ rw #x00) k))
         (size (%gc-meta-read (+ rw #x10) k)))
    (if (and (>= w from) (< w (+ from size))) 1 0)))

;;; The three arms.  ONE loop shape; only the body differs.

(defun il-body-str (i) (concatenate 'string "ILS-" (write-to-string i)))
(defun il-body-low (i) (%intern-symbol-pkg (+ 900000000 i) 0))
(defun il-body-cl  (i)
  (intern (concatenate 'string "ILC-" (write-to-string i)) "COMMON-LISP-USER"))

(defun il-worker (arm r0 scr k)
  (let ((c0 (il-count))
        (f0 (il-fwd r0))
        (i 0)
        (last nil))
    (loop
      (when (>= i k) (return 0))
      (setq last (cond ((string= arm "str") (il-body-str i))
                       ((string= arm "low") (il-body-low i))
                       (t                   (il-body-cl i))))
      (setq i (+ i 1)))
    (list c0 (il-count) f0 (il-fwd r0)
          (il-in-worker (%gc-word-of last (+ scr 512)))
          (if last 1 0))))

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

(defvar *res* nil)
(setq *res*
      (let ((arm *arm*) (r0 *r0*) (scr *scr*) (k *k*))
        (sb-thread:join-thread
         (sb-thread:make-thread (lambda () (il-worker arm r0 scr k)) :name "il"))))

(defvar *c0* 0) (setq *c0* (nth 0 *res*))
(defvar *c1* 0) (setq *c1* (nth 1 *res*))
(defvar *f0* 0) (setq *f0* (nth 2 *res*))
(defvar *f1* 0) (setq *f1* (nth 3 *res*))
(defvar *inw* 0) (setq *inw* (nth 4 *res*))

(format t "~&=== ARM ~a, K=~d ===~%" *arm* *k*)
(format t "~&shared symbol table count  ~d -> ~d   (grew by ~d)~%" *c0* *c1* (- *c1* *c0*))
(format t "~&region 0 -> worker         ~d -> ~d   (grew by ~d)~%" *f0* *f1* (- *f1* *f0*))
(format t "~&the LAST object made is in the worker's own region: ~d~%" *inw*)

(chk "the loop really ran" (nth 5 *res*) 1)

;; The odometer.  `str' interns nothing; the two intern arms must each have
;; created exactly K fresh entries, or a zero below means "nothing happened".
(if (string= *arm* "str")
    (chk "the string arm interns nothing" (- *c1* *c0*) 0)
    (chk "K FRESH symbols were really created" (- *c1* *c0*) *k*))

;; THE ASSERTION.  Zero in the forbidden direction, in every arm.
(chk "NO new pointer from region 0 into the worker's region" (- *f1* *f0*) 0)

;; And WHERE the object landed, which is the mechanism behind the number above.
(if (string= *arm* "low")
    (chk "a %INTERN-SYMBOL-PKG symbol is NOT in the worker's region" *inw* 0)
    0)
(if (string= *arm* "cl")
    ;; RED TODAY.  CL:INTERN allocates in the caller's region and registers in
    ;; region 0's tables; this is the pointer that dangles when the worker's
    ;; region collects.
    (chk "a CL:INTERN symbol is NOT in the worker's region" *inw* 0)
    0)

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "INTERN LAYERS ~a: PASS~%" *arm*)
    (format t "INTERN LAYERS ~a: FAIL~%" *arm*))
(finish-output)

;;; TOPLEVEL — a SYS-EXIT nested inside a LET*/IF in a --script does not take
;;; effect; see test/hosted-term-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
