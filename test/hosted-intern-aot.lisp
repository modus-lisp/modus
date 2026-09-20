;;;; hosted-intern-aot.lisp — IS IT THE COMPILATION MODE?
;;;;
;;;;   test/run-intern-aot.sh [MODUS-BINARY] [RUNS] [K]
;;;;   MODE=aot|rt  ARM=str|low|cl  K=<count>  ./modus --script <this>
;;;;
;;;; ============================================================
;;;; THE QUESTION, AND WHY IT NEEDED A BUILD
;;;; ============================================================
;;;;
;;;; test/hosted-intern-layers.lisp's `low' arm — a worker thread calling
;;;; %INTERN-SYMBOL-PKG on fresh name-hashes in a loop — dies about half the
;;;; time with `MVM LONGJMP (TRAP #x0511) with no active handler-case', while
;;;; its cross-region audit reads ZERO on every run that completes.  So it is
;;;; not the region violation CL:INTERN commits; it is a SECOND defect.
;;;;
;;;; Against that stands test/hosted-thread-lisp.lisp, which interns fresh
;;;; symbols through the SAME %INTERN-SYMBOL-PKG, on two threads, 300
;;;; iterations each, and scores 100 of 100.  Two differences were left
;;;; standing between them.  One — that the green one holds the runtime lock
;;;; across a whole iteration — is already measured and is NOT the variable:
;;;; one lock held across K interns is WORSE than lock-per-call.  The other is
;;;; that the green one's loop is compiled INTO THE IMAGE while the red one's
;;;; loop is runtime-compiled `--script' code, and that one could not be
;;;; measured from a binary that did not contain the loop.
;;;;
;;;; net/hosted-intern-probe.lisp puts it there.  This script runs EITHER the
;;;; in-image copy OR the script-side copy below, chosen by MODE.
;;;;
;;;; WHY IT MATTERS BEYOND THE TEST.  glass is LOADed at runtime, so its RFB
;;;; sender loop is runtime-compiled.  If runtime-compiled is the variable,
;;;; this is the same defect that stops the frame, not a cousin of it.
;;;;
;;;; ============================================================
;;;; WHAT MAKES THE COMPARISON FAIR, AND WHAT WOULD MAKE IT A LIE
;;;; ============================================================
;;;;
;;;;   ONE SCRIPT, ONE PROCESS SHAPE, ONE BINARY.  Both modes take the same
;;;;   %HA-ACTORS-BRINGUP, the same SB-THREAD:MAKE-THREAD, the same lambda,
;;;;   the same arguments, the same audit and the same driver.  Everything the
;;;;   loop is CALLED FROM is runtime-compiled in both.  The only thing that
;;;;   moves between the modes is where the loop itself was compiled, so a
;;;;   difference between them is the compilation mode and nothing else.
;;;;
;;;;   THE TWO COPIES ARE LINE FOR LINE THE SAME SOURCE.  The `il-' functions
;;;;   below are a transcription of the `%ip-' functions in
;;;;   net/hosted-intern-probe.lisp, which are themselves a transcription of
;;;;   test/hosted-intern-layers.lisp.  If they drift, this measures the drift.
;;;;
;;;;   MODE=aot MUST REALLY REACH THE IMAGE.  A missing %IP-WORKER would be
;;;;   an UNDEFINED-FUNCTION, which is a death, which would read as "AOT dies
;;;;   too" — the exact wrong conclusion.  So the script asserts FBOUNDP
;;;;   before it spawns anything and says SKIP, loudly, if the binary does not
;;;;   contain the probe.
;;;;
;;;;   THE AUDIT MUST BE ABLE TO ANSWER NON-ZERO.  Same instrument as
;;;;   test/hosted-intern-layers.lisp: the `cl' arm answers +44 where `low'
;;;;   answers 0, on the same worker over the same spans in the same
;;;;   direction.  An instrument that distinguishes them is not reporting 0
;;;;   because it is looking at nothing.
;;;;
;;;;   BOTH ARMS MUST REALLY HAVE CREATED SYMBOLS.  The shared table's COUNT
;;;;   is sampled before and after and must have grown by exactly K, or a zero
;;;;   audit means "nothing happened".
;;;;
;;;;   ONE RUN IS NOT A RESULT.  Everything in this campaign is
;;;;   layout-sensitive.  The runner does RUNS passes per cell and classifies
;;;;   survived / died / HUNG separately — hangs are a distinct class here (a
;;;;   lost wakeup or a held lock), not a flavour of fault.

(%ha-actors-bringup 4 0)

(defvar *mode*
  (let ((s (%cli-getenv "MODE"))) (if (and s (> (length s) 0)) s "rt")))
(defvar *arm*
  (let ((s (%cli-getenv "ARM"))) (if (and s (> (length s) 0)) s "low")))
(defvar *k*
  (let ((s (%cli-getenv "K"))) (if (and s (> (length s) 0)) (parse-integer s) 20)))

(defvar *r0* 0) (setq *r0* (%gc-region-0))
(defvar *scr* 0) (setq *scr* (%thr-scratch))

;;; ---- THE SCRIPT-SIDE COPY (runtime-compiled) --------------------------
;;; Line for line the same source as net/hosted-intern-probe.lisp's %ip-*.

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

;;; ---- THE GATE ---------------------------------------------------------
;;; A binary without the probe must SKIP, not die: an UNDEFINED-FUNCTION here
;;; would be indistinguishable from the fault this test exists to look for.

(defvar *have-aot* 0)
(setq *have-aot* (if (fboundp '%ip-worker) 1 0))

(format t "~&=== MODE ~a  ARM ~a  K=~d ===~%" *mode* *arm* *k*)
(if (and (string= *mode* "aot") (= *have-aot* 0))
    (format t "~&SKIP: this binary has no %IP-WORKER — rebuild with net/hosted-intern-probe.lisp.~%")
    0)

(defvar *skip* 0)
(setq *skip* (if (and (string= *mode* "aot") (= *have-aot* 0)) 1 0))

;;; ---- THE RUN ----------------------------------------------------------

(defvar *res* nil)
(setq *res*
      (if (= *skip* 1)
          nil
          (let ((mode *mode*) (arm *arm*) (r0 *r0*) (scr *scr*) (k *k*))
            (sb-thread:join-thread
             (sb-thread:make-thread
              (lambda ()
                (if (string= mode "aot")
                    (%ip-worker arm r0 scr k)
                    (il-worker arm r0 scr k)))
              :name "ia")))))

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

(defvar *c0* 0) (setq *c0* (if *res* (nth 0 *res*) 0))
(defvar *c1* 0) (setq *c1* (if *res* (nth 1 *res*) 0))
(defvar *f0* 0) (setq *f0* (if *res* (nth 2 *res*) 0))
(defvar *f1* 0) (setq *f1* (if *res* (nth 3 *res*) 0))
(defvar *inw* 0) (setq *inw* (if *res* (nth 4 *res*) 0))

(if (= *skip* 1)
    0
    (progn
      (format t "~&shared symbol table count  ~d -> ~d   (grew by ~d)~%"
              *c0* *c1* (- *c1* *c0*))
      (format t "~&region 0 -> worker         ~d -> ~d   (grew by ~d)~%"
              *f0* *f1* (- *f1* *f0*))
      (format t "~&the LAST object made is in the worker's own region: ~d~%" *inw*)
      (chk "the loop really ran" (nth 5 *res*) 1)
      (if (string= *arm* "str")
          (chk "the string arm interns nothing" (- *c1* *c0*) 0)
          (chk "K FRESH symbols were really created" (- *c1* *c0*) *k*))
      (chk "NO new pointer from region 0 into the worker's region" (- *f1* *f0*) 0)
      (if (string= *arm* "low")
          (chk "a %INTERN-SYMBOL-PKG symbol is NOT in the worker's region" *inw* 0)
          0)
      (if (string= *arm* "cl")
          (chk "a CL:INTERN symbol is NOT in the worker's region" *inw* 0)
          0)))

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *skip* 1)
    (format t "INTERN AOT ~a/~a: SKIP~%" *mode* *arm*)
    (if (= *fail* 0)
        (format t "INTERN AOT ~a/~a: PASS~%" *mode* *arm*)
        (format t "INTERN AOT ~a/~a: FAIL~%" *mode* *arm*)))
(finish-output)

;;; TOPLEVEL — a SYS-EXIT nested inside a LET*/IF in a --script does not take
;;; effect; see test/hosted-term-xregion.lisp.  2 = SKIP, distinguishable from
;;; both a pass and a failure.
(sys-exit (if (= *skip* 1) 2 (if (= *fail* 0) 0 1)))
