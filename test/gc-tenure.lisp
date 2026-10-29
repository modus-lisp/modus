;;;; test/gc-tenure.lisp -- generational GC stage 1 (docs/generational-gc.md).
;;;;
;;;;   modus --load test/gc-tenure.lisp                       ; the real arm
;;;;   modus --eval '(defparameter *gen-control* t)' --load test/gc-tenure.lisp
;;;;
;;;; Builds a live set, TENURES it, then churns through many collections while
;;;; storing YOUNG objects into OLD ones (a vector slot, a list's tail, a hash
;;;; table that grows, a global).  Every structure must walk back intact, and
;;;; after every collection no POINTER SLOT of the old space may point into the
;;;; semispace that collection vacated (%GC-OLD-AUDIT walks it typed, as the
;;;; collector does; a conservative word count also sees raw leaf payload).
;;;;
;;;; THE NEGATIVE CONTROL (*GEN-CONTROL* T) is the same binary and workload with
;;;; the old-space walk switched off right after tenure (%GC-OLD-SPACE-FORGET).
;;;; It must FAIL -- a wrong value, a dangling count, or a crash.  A control that
;;;; passes means the test is not exercising the walk.
;;;;
;;;; AArch64 native collector only (the only one that tenures).

(defvar *gen-control* nil)
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

(defun gen-run ()
  (gen-build-old)
  (gen-check "no old space before tenure" (not (%gc-old-space-p)))
  (let* ((t0 (get-internal-real-time))
         (r (gc-tenure))
         (ms (- (get-internal-real-time) t0)))
    (gen-check "gc-tenure made an old space" (%gc-old-space-p)
               (format nil "old [~x, ~x) = ~,1f MB, young 2 x ~,1f MB, ~d ms"
                       (first r) (second r) (/ (- (second r) (first r)) 1048576.0)
                       (/ (third r) 1048576.0) ms))
    (let ((au (%gc-old-audit)))
      (gen-check "the old space parses object by object, end to end"
                 (= 0 (third au)) (format nil "~d objects, bad at ~x" (first au) (third au))))
    (gen-check "a second gc-tenure is refused"
               (handler-case (progn (gc-tenure) nil) (error () t))))
  (when *gen-control*
    (format t "NEGATIVE CONTROL: the old-space walk is OFF from here~%")
    (%gc-old-space-forget))
  (let ((rounds 60) (dangling-max 0) (collections 0))
    (dotimes (round rounds)
      (gen-store-young round)
      (gen-churn 20000)
      (gen-collect) (incf collections)
      (unless *gen-control*
        (setq dangling-max (max dangling-max (%gc-old-dangling))))
      (when (= 0 (mod round 3))
        (gen-churn 5000) (gen-collect) (incf collections)))
    (gen-check "every structure intact after the churn" (gen-verify rounds)
               (format nil "~d rounds, ~d forced collections" rounds collections))
    (unless *gen-control*
      (gen-check "old space never pointed into a vacated semispace"
                 (= dangling-max 0) (format nil "max ~d" dangling-max))))
  ;; the old space really is not copied: a collection's survivor count is
  ;; the young live set, not the old one
  (unless *gen-control*
    (let ((n (%gc-stat-count)) (b0 (%gc-stat-total-bytes)))
      (gen-collect)
      (let ((young (- (%gc-stat-total-bytes) b0))
            (old (- (%gc-old-hi) (%gc-old-lo))))
        (gen-check "a collection copies the young survivors, not the old space"
                   (and (> (%gc-stat-count) n) (< young (floor old 4)))
                   (format nil "~,2f MB copied vs ~,1f MB old"
                           (/ young 1048576.0) (/ old 1048576.0))))))
  (gen-check "save-image refuses while there is an old space"
             (handler-case (progn (%save-image "/nonexistent/x.core") nil)
               (error (e) (search "OLD SPACE" (princ-to-string e)))))
  (format t "~a: ~d checks, ~d failed~%"
          (if *gen-control* "CONTROL" "GC-TENURE") *gen-checks* *gen-fails*))

(gen-run)
