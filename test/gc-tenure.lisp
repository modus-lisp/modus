;;;; test/gc-tenure.lisp -- generational GC stage 1 (docs/generational-gc.md).
;;;;
;;;;   modus --load test/gc-tenure.lisp                        ; cards (stage 2)
;;;;   modus --eval '(defparameter *gen-cards* nil)' --load ... ; full walk (stage 1)
;;;;   modus --eval '(defparameter *gen-control* :walk)' --load ...    ; must FAIL
;;;;   modus --eval '(defparameter *gen-control* :barrier)' --load ... ; must FAIL
;;;;
;;;; Builds a live set, TENURES it, then churns through many collections while
;;;; storing YOUNG objects into OLD ones (a vector slot, a list's tail, a hash
;;;; table that grows, a global).  Every structure must walk back intact, and
;;;; after every collection no POINTER SLOT of the old space may point into the
;;;; semispace that collection vacated (%GC-OLD-AUDIT walks it typed, as the
;;;; collector does; a conservative word count also sees raw leaf payload).
;;;;
;;;; THE NEGATIVE CONTROLS are the same binary and workload with one piece
;;;; switched off right after tenure: :WALK stops the old space being looked at
;;;; at all (%GC-OLD-SPACE-FORGET), :BARRIER keeps the dirty-card scan but stops
;;;; stores marking cards (%GC-CARD-BARRIER-OFF).  Each must FAIL -- a wrong
;;;; value, a dangling count, or a crash.  A control that passes means the test
;;;; is not exercising that piece.
;;;;
;;;; The AArch64 and hosted x86-64 native collectors (the ones that tenure).

(defvar *gen-control* nil)
(defvar *gen-cards* t)
(defvar *gen-fails* 0)
(defvar *gen-checks* 0)

(defun gen-check (name ok &optional detail)
  (setq *gen-checks* (+ *gen-checks* 1))
  (unless ok (setq *gen-fails* (+ *gen-fails* 1)))
  (format t "~:[FAIL~;ok  ~] ~a~@[ ~a~]~%" ok name detail)
  ok)

;;; ---- the live set, built BEFORE tenure: all of it becomes old ----
(defvar *old-chain* nil)
(defvar *old-vec* nil)
(defvar *old-ht* nil)
(defvar *old-tail* nil)

(defun gen-build-old ()
  (let ((c nil))
    (dotimes (i 20000) (setq c (cons i c)))
    (setq *old-chain* c))
  (setq *old-vec* (make-array 5000 :initial-element nil))
  (dotimes (i 5000) (setf (svref *old-vec* i) (format nil "old-~d" i)))
  (setq *old-ht* (make-hash-table :test 'equal))
  (dotimes (i 500) (setf (gethash (format nil "k~d" i) *old-ht*) i))
  (setq *old-tail* (list :head)))

(defun gen-collect ()
  "Force ONE collection of the active region (region 0) and return its epoch."
  (let ((before (%gc-epoch)))
    (loop
      (set-alloc-limit (get-alloc-ptr))
      (let ((probe (cons 1 2)))
        (when (/= (%gc-epoch) before) (return (car probe)))))))

(defun gen-churn (n)
  "Garbage: N short lists that nothing keeps."
  (let ((junk nil))
    (dotimes (i n) (setq junk (list i i i i)))
    junk))

;;; ---- old -> young stores, made AFTER tenure ----
(defun gen-store-young (round)
  ;; a young list in every 10th slot of the old vector
  (loop for i from (mod round 10) below 5000 by 10
        do (setf (svref *old-vec* i) (list round i (format nil "y~d" i))))
  ;; a young cons onto the old list's tail
  (let ((cell (last *old-tail*)))
    (setf (cdr cell) (list round)))
  ;; the old table grows (its new storage is young) and gains young keys
  (dotimes (j 40)
    (setf (gethash (format nil "r~d-~d" round j) *old-ht*) (* round j))))

(defun gen-verify (rounds)
  (let ((ok t))
    ;; the chain: 20000 links, car = index, newest first
    (let ((n 0) (c *old-chain*) (bad 0))
      (loop while (and (consp c) (<= n 20000)) do (unless (eql (car c) (- 19999 n)) (incf bad)) (incf n) (setq c (cdr c)))
      (unless (and (= n 20000) (= bad 0)) (setq ok nil)
        (format t "  chain n=~d bad=~d~%" n bad)))
    ;; the vector: last round's young lists where it stored, old strings elsewhere
    (let ((bad 0) (last-round (- rounds 1)))
      (dotimes (i 5000)
        (let ((v (svref *old-vec* i)))
          (cond ((stringp v)
                 (unless (string= v (format nil "old-~d" i)) (incf bad)))
                ((consp v)
                 (unless (and (= (second v) i)
                              (string= (third v) (format nil "y~d" i))
                              (<= (first v) last-round)
                              (= (mod i 10) (mod (first v) 10)))
                   (incf bad)))
                (t (incf bad)))))
      (unless (= bad 0) (setq ok nil) (format t "  vector bad=~d~%" bad)))
    ;; the tail: :head then 0 .. rounds-1
    (let ((l (cdr *old-tail*)) (bad 0))
      (dotimes (r rounds) (unless (and (consp l) (eql (car l) r)) (incf bad)) (setq l (if (consp l) (cdr l) nil)))
      (unless (and (= bad 0) (null l) (eq (car *old-tail*) :head)) (setq ok nil)
        (format t "  tail bad=~d~%" bad)))
    ;; the table: 500 old keys + 40 per round
    (let ((bad 0))
      (dotimes (i 500) (unless (eql (gethash (format nil "k~d" i) *old-ht*) i) (incf bad)))
      (dotimes (r rounds)
        (dotimes (j 40)
          (unless (eql (gethash (format nil "r~d-~d" r j) *old-ht*) (* r j)) (incf bad))))
      (unless (and (= bad 0) (= (hash-table-count *old-ht*) (+ 500 (* 40 rounds))))
        (setq ok nil)
        (format t "  table bad=~d count=~d~%" bad (hash-table-count *old-ht*))))
    ok))

(defun gen-audit-control ()
  "POSITIVE CONTROL for %GC-CARD-AUDIT: after a collection (cards cleaned),
   put a young cons into an old cons's car with a RAW store the barrier cannot
   see; the audit must count it.  Then store it properly and the audit must
   be satisfied again."
  (gen-collect) (gen-collect)
  (let* ((cell (nthcdr 777 *old-chain*))
         (young (list :young))
         (scratch (%conv-addr #x10000C90))
         (addr (- (%gc-word-of cell scratch) 1)))
    (%gc-write64 scratch 0)
    (setf (mem-ref addr :u64) young)
    (let ((caught (second (%gc-card-audit))))
      (setf (car cell) young)
      (let ((after (second (%gc-card-audit))))
        (setf (car cell) (- 19999 777))
        (gen-check "the card audit catches a store the barrier did not see"
                   (and (>= caught 1) (= after 0))
                   (format nil "raw store: ~d, barriered store: ~d" caught after))))))

(defvar *gen-rounds* 60)

(defun gen-churn-rounds (from to)
  (let ((dmax 0) (cbad 0))
    (loop for round from from below to
          do (gen-store-young round)
             (gen-churn 20000)
             (gen-collect)
             (setq dmax (max dmax (%gc-old-dangling))
                   cbad (max cbad (second (%gc-card-audit)))))
    (setq *gen-rounds* to)
    (list dmax cbad)))

(defun gen-stage3 ()
  "Promotion and the major collection."
  (let ((rep (gc-gen-report)))
    (gen-check "survivors were PROMOTED into the old space"
               (> (getf rep :promoted) 0.0) (format nil "~s" rep)))
  (let* ((old0 (- (%gc-old-hi) (%gc-old-lo)))
         (t0 (get-internal-real-time))
         (old1 (gc-major))
         (ms (/ (* 1000.0 (- (get-internal-real-time) t0)) internal-time-units-per-second))
         (au (%gc-old-audit)))
    (gen-check "a major collection reclaimed old garbage"
               (and (= 1 (getf (gc-gen-report) :majors)) (< old1 old0))
               (format nil "old ~,1f -> ~,1f MB in ~,1f ms" (/ old0 1048576.0) (/ old1 1048576.0) ms))
    (gen-check "after the major: the old space parses, nothing dangles, cards hold"
               (and (= 0 (third au)) (= 0 (second au)) (= 0 (second (%gc-card-audit))))
               (format nil "~d objects" (first au)))
    (gen-check "after the major: every structure intact" (gen-verify *gen-rounds*)))
  (let ((r (gen-churn-rounds *gen-rounds* (+ *gen-rounds* 60))))
    (gen-check "60 more rounds after the major: intact, nothing dangling, cards hold"
               (and (gen-verify *gen-rounds*) (= 0 (first r)) (= 0 (second r)))
               (format nil "~s ~s" r (gc-gen-report))))
  (let ((r (gen-churn-rounds *gen-rounds* (+ *gen-rounds* 3))))
    (gc-major)
    (gen-check "a second major, then everything again"
               (and (gen-verify *gen-rounds*) (= 0 (first r)) (= 0 (second (%gc-card-audit)))
                    (= 2 (getf (gc-gen-report) :majors)))
               (format nil "~s" (gc-gen-report))))
  ;; AUTOMATIC majors: a reserve just above the room left, so the next
  ;; collection that promotes must ask for a major and do it, without GC-MAJOR.
  (let ((majors (getf (gc-gen-report) :majors)))
    (gc-gen-reserve (+ (%gc-old-room) 1))
    (let ((r (gen-churn-rounds *gen-rounds* (+ *gen-rounds* 10))))
      (gc-gen-reserve 0)
      (gen-check "running short of old room triggers a major by itself"
                 (and (> (getf (gc-gen-report) :majors) majors)
                      (gen-verify *gen-rounds*) (= 0 (first r)) (= 0 (second r)))
                 (format nil "~s" (gc-gen-report))))))

(defun gen-ceiling ()
  "The meta's CEILING (+72): promotion never goes above it.  On the board it is
   the I/O staging page at the top of the upper semispace; here one is planted
   64 KB above old_hi, then removed for the control."
  (gc-gen-reserve 0)
  (let* ((ceil (+ (%gc-old-hi) 65536)) (p0 (%gc-gen-meta-ref 6)) (f0 (%gc-gen-meta-ref 4)))
    (%gc-write64 (+ (%gc-gen-meta) 72) ceil)
    (let* ((r (gen-churn-rounds *gen-rounds* (+ *gen-rounds* 10)))
           (hi (%gc-old-hi)) (dp (- (%gc-gen-meta-ref 6) p0)) (df (- (%gc-gen-meta-ref 4) f0)))
      (%gc-write64 (+ (%gc-gen-meta) 72) 0)
      (gen-check "promotion stops at the ceiling (the board's I/O staging pages)"
                 (and (<= hi ceil) (<= dp 65536) (> df 0)
                      (gen-verify *gen-rounds*) (= 0 (first r)) (= 0 (second r)))
                 (format nil "old_hi ~x, ceiling ~x, ~d bytes promoted, ~d failures" hi ceil dp df)))
    (let* ((p1 (%gc-gen-meta-ref 6))
           (r (gen-churn-rounds *gen-rounds* (+ *gen-rounds* 10)))
           (dp (- (%gc-gen-meta-ref 6) p1)))
      (gen-check "CONTROL: without it the same churn promotes past that much"
                 (and (> dp 65536) (gen-verify *gen-rounds*) (= 0 (first r)) (= 0 (second r)))
                 (format nil "~d bytes promoted ~s" dp (gc-gen-report))))))

(defun gen-run ()
  (gen-build-old)
  (gen-check "no old space before tenure" (not (%gc-old-space-p)))
  (let* ((t0 (get-internal-real-time))
         (r (gc-tenure :cards *gen-cards*))
         (ms (- (get-internal-real-time) t0)))
    (gen-check "gc-tenure made an old space" (%gc-old-space-p)
               (format nil "old [~x, ~x) = ~,1f MB, young 2 x ~,1f MB, ~,1f ms"
                       (first r) (second r) (/ (- (second r) (first r)) 1048576.0)
                       (/ (third r) 1048576.0)
                       (/ (* 1000.0 ms) internal-time-units-per-second)))
    (let ((au (%gc-old-audit)))
      (gen-check "the old space parses object by object, end to end"
                 (= 0 (third au)) (format nil "~d objects, bad at ~x" (first au) (third au))))
    (gen-check "a second gc-tenure is refused"
               (handler-case (progn (gc-tenure) nil) (error () t))))
  (gen-check "the card table is on exactly when asked for"
             (eq (and *gen-cards* t) (%gc-cards-on-p)))
  (when *gen-cards* (gen-audit-control))
  (case *gen-control*
    ((t :walk)
     (format t "NEGATIVE CONTROL: the old-space walk is OFF from here~%")
     (%gc-old-space-forget))
    (:barrier
     (format t "NEGATIVE CONTROL: the card barrier is OFF from here~%")
     (%gc-card-barrier-off)))
  (%gc-stats-reset)
  (let ((rounds 60) (dangling-max 0) (card-bad 0) (collections 0))
    (dotimes (round rounds)
      (gen-store-young round)
      (gen-churn 20000)
      (gen-collect) (incf collections)
      (unless *gen-control*
        (setq dangling-max (max dangling-max (%gc-old-dangling)))
        (when *gen-cards*
          (setq card-bad (max card-bad (second (%gc-card-audit))))))
      (when (= 0 (mod round 3))
        (gen-churn 5000) (gen-collect) (incf collections)))
    (gen-check "every structure intact after the churn" (gen-verify rounds)
               (format nil "~d rounds, ~d forced collections" rounds collections))
    (unless *gen-control*
      (gen-check "old space never pointed into a vacated semispace"
                 (= dangling-max 0) (format nil "max ~d" dangling-max))
      (when *gen-cards*
        (gen-check "every young pointer in the old space sits on a dirty card"
                   (= card-bad 0) (format nil "max ~d on clean cards" card-bad))))
    (let ((t0 (get-internal-real-time)))
      (dotimes (i 20) (gen-collect))
      (format t "     a collection with ~a: ~,2f ms (mean of 20)~%"
              (if *gen-cards* "dirty cards" "the whole old space walked")
              (/ (* 1000.0 (- (get-internal-real-time) t0))
                 (* 20 internal-time-units-per-second)))))
  ;; the old space really is not copied: a collection's survivor count is
  ;; the young live set, not the old one
  ;; (measured as the young semispace's fill right after a collection, not by
  ;; the collector's byte counters, which x64 does not keep at those words)
  (unless *gen-control*
    (let ((e (%gc-epoch)))
      (gen-collect)
      (let* ((k (%gc-meta-scale))
             (young (- (get-alloc-ptr) (%gc-meta-read (%gc-region-0) k)))
             (old (- (%gc-old-hi) (%gc-old-lo))))
        (gen-check "a collection copies the young survivors, not the old space"
                   (and (/= e (%gc-epoch)) (< young (floor old 4)))
                   (format nil "~,2f MB young after it vs ~,1f MB old"
                           (/ young 1048576.0) (/ old 1048576.0))))))
  (when (and *gen-cards* (not *gen-control*))
    (gen-stage3)
    (gen-ceiling))
  (gen-check "save-image first puts the old space back into one semispace"
             (progn (handler-case (%save-image "/nonexistent/x.core") (error () nil))
                    (and (not (%gc-old-space-p)) (not (%gc-cards-on-p))
                         (gen-verify *gen-rounds*)))
             "(the write itself fails: the path does not exist)")
  (gen-collect) (gen-collect)
  (gen-check "after un-tenure, ordinary collections keep everything"
             (gen-verify *gen-rounds*))
  (format t "~a: ~d checks, ~d failed~%"
          (if *gen-control* "CONTROL" "GC-TENURE") *gen-checks* *gen-fails*))

(gen-run)
