;;;; hosted-intern-shape.lisp — IF IT IS NOT THE COMPILATION MODE, WHAT IS IT?
;;;;
;;;;   test/run-intern-shape.sh [MODUS-BINARY] [RUNS] [K]
;;;;   ARM=<arm> K=<count> ./modus --script test/hosted-intern-shape.lisp
;;;;
;;;; ============================================================
;;;; WHERE THIS PICKS UP
;;;; ============================================================
;;;;
;;;; test/run-intern-aot.sh settled the last candidate difference between the
;;;; red `low' arm and the green test/hosted-thread-lisp.lisp: whether the loop
;;;; is AOT-compiled inside the image.  IT IS NOT THAT.  Both modes die, on the
;;;; same binary, on the same loop, at rates that are the same within this
;;;; campaign's noise.
;;;;
;;;; So the green test's 100 of 100 has some OTHER explanation, and the honest
;;;; way to find it is to stop comparing two whole programs and start removing
;;;; one difference at a time from the RED one.  Every arm below is the same
;;;; worker, the same K, the same %INTERN-SYMBOL-PKG call, differing in exactly
;;;; one named thing — so a rate that moves between two arms names the thing.
;;;;
;;;;   bare     the loop and NOTHING else.  No audit, no table count, no list
;;;;            built in the worker's region — the worker returns a fixnum.
;;;;            This is the floor: if `bare' dies, nothing wrapped around the
;;;;            loop is implicated and the intern itself is the whole story.
;;;;   count    bare + HASH-TABLE-COUNT of the shared table, before and after.
;;;;   audit    count + %GC-COUNT-FOREIGN-REFS, which is an O(heap) sweep of
;;;;            region 0's whole live span RUN FROM THE WORKER, twice.  This is
;;;;            today's `low' arm, and the sweep is a real suspect rather than
;;;;            neutral instrumentation.
;;;;   list     bare, but the worker returns a LIST it consed in its own region
;;;;            and the driver reads it after the join.
;;;;   outer    bare, with the whole loop inside ONE %RT-ENTER/%RT-LEAVE — the
;;;;            shape %TL-RUN uses, where the region hop happens once instead
;;;;            of K times and the interns run at lock depth 2.
;;;;   main     the loop on the MAIN THREAD, no worker at all.  The control
;;;;            that says whether "on a worker" is part of the statement.
;;;;   str      K strings on the worker, no intern.  The allocation control,
;;;;            carried over so this file has its own floor.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   AN ARM THAT INTERNS NOTHING SCORES CLEAN FOR THE WRONG REASON.  Every
;;;;   interning arm uses a DISJOINT hash range, so no arm can quietly find
;;;;   symbols another arm made, and the arms that can measure it check that
;;;;   the shared table grew by exactly K.  `bare' and `outer' deliberately
;;;;   cannot measure it — that is the point of them — so they are read
;;;;   TOGETHER with `count', which is `bare' plus exactly that measurement.
;;;;
;;;;   ONE RUN IS NOT A RESULT, and a hang is not a death.  The runner does
;;;;   RUNS passes per arm and classifies survived / died / hung separately.
;;;;
;;;;   THE ARMS DIFFER BY ONE THING EACH, IN A CHAIN.  bare -> count -> audit
;;;;   adds one measurement at a time; bare -> list adds the returned list;
;;;;   bare -> outer moves the lock; bare -> main removes the worker.  A rate
;;;;   that moves between two adjacent arms is that difference and nothing
;;;;   else.

(%ha-actors-bringup 4 0)

(defvar *arm*
  (let ((s (%cli-getenv "ARM"))) (if (and s (> (length s) 0)) s "bare")))
(defvar *k*
  (let ((s (%cli-getenv "K"))) (if (and s (> (length s) 0)) (parse-integer s) 20)))

(defvar *r0* 0) (setq *r0* (%gc-region-0))

;;; DISJOINT HASH RANGES, one per arm, so no arm can find another's symbols.
(defun ish-base (arm)
  (cond ((string= arm "bare")  1000000000)
        ((string= arm "count") 1100000000)
        ((string= arm "audit") 1200000000)
        ((string= arm "list")  1300000000)
        ((string= arm "outer") 1400000000)
        ((string= arm "main")  1500000000)
        (t                     1600000000)))

(defun ish-count () (hash-table-count (mem-ref #x10000088 :u64)))

(defun ish-fwd (r0)
  "region 0's LIVE span -> this worker's region.  The forbidden direction."
  (let* ((k (%gc-meta-scale)) (rw (%gc-region))
         (wfrom (%gc-meta-read (+ rw #x00) k))
         (wsize (%gc-meta-read (+ rw #x10) k))
         (r0from (%gc-meta-read (+ r0 #x00) k))
         (r0alloc (%gc-meta-read (+ r0 #x30) k)))
    (%gc-count-foreign-refs r0from r0alloc wfrom wsize)))

;;; ---- THE ARMS.  One loop shape; one named difference each. -------------

(defun ish-bare (base k)
  (let ((i 0) (last nil))
    (loop
      (when (>= i k) (return 0))
      (setq last (%intern-symbol-pkg (+ base i) 0))
      (setq i (+ i 1)))
    (if last k 0)))

(defun ish-str (base k)
  (let ((i 0) (last nil))
    (loop
      (when (>= i k) (return 0))
      (setq last (concatenate 'string "ISS-" (write-to-string (+ base i))))
      (setq i (+ i 1)))
    (if last k 0)))

(defun ish-count-arm (base k)
  (let ((c0 (ish-count)))
    (ish-bare base k)
    (- (ish-count) c0)))

(defun ish-audit-arm (base k r0)
  (let ((c0 (ish-count))
        (f0 (ish-fwd r0)))
    (ish-bare base k)
    (if (= (- (ish-count) c0) k)
        (if (= (- (ish-fwd r0) f0) 0) k 0)
        0)))

(defun ish-list-arm (base k)
  (let ((i 0) (acc nil) (last nil))
    (loop
      (when (>= i k) (return 0))
      (setq last (%intern-symbol-pkg (+ base i) 0))
      (setq acc (cons i acc))
      (setq i (+ i 1)))
    (if last acc nil)))

(defun ish-outer-arm (base k)
  "The %TL-RUN shape: ONE region hop for the whole loop, interns at depth 2."
  (%rt-enter)
  (let ((r (ish-bare base k)))
    (%rt-leave)
    r))

;;; ---- THE RUN ----------------------------------------------------------

(defvar *arm-base* 0) (setq *arm-base* (ish-base *arm*))

(defvar *res* nil)
(setq *res*
      (let ((arm *arm*) (base *arm-base*) (k *k*) (r0 *r0*))
        (if (string= arm "main")
            ;; NO WORKER AT ALL.  The control for "on a worker".
            (ish-bare base k)
            (sb-thread:join-thread
             (sb-thread:make-thread
              (lambda ()
                (cond ((string= arm "bare")  (ish-bare base k))
                      ((string= arm "str")   (ish-str base k))
                      ((string= arm "count") (ish-count-arm base k))
                      ((string= arm "audit") (ish-audit-arm base k r0))
                      ((string= arm "list")  (ish-list-arm base k))
                      (t                     (ish-outer-arm base k))))
              :name "ish")))))

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

(format t "~&=== SHAPE ARM ~a  K=~d  base=~d ===~%" *arm* *k* *arm-base*)

;; The `list' arm's answer is a K-long list; every other arm's is K (or, for
;; `count', the number of FRESH symbols it made, which must also be K).
(if (string= *arm* "list")
    (chk "the worker returned K conses" (length *res*) *k*)
    (chk "the arm ran and answered K" *res* *k*))

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "INTERN SHAPE ~a: PASS~%" *arm*)
    (format t "INTERN SHAPE ~a: FAIL~%" *arm*))
(finish-output)

;;; TOPLEVEL — a SYS-EXIT nested inside a LET*/IF in a --script does not take
;;; effect; see test/hosted-term-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
