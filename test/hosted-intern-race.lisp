;;;; hosted-intern-race.lisp — INTERNING WITHOUT THE LOCK, UNDER A RACE.
;;;;
;;;;   ./modus --script test/hosted-intern-race.lisp
;;;;
;;;; A keyword or symbol that already exists is found WITHOUT the runtime lock
;;;; (mvm/prelude.lisp %HT-GET-RO); only a miss takes it, and the locked body
;;;; looks again before inserting.  Four threads here intern the SAME 20 000
;;;; fresh keyword hashes and 20 000 fresh symbol hashes, each in its own
;;;; order, all at once — so misses race misses, hits race the inserts that
;;;; make them, and both tables rebuild their bucket index (it doubles past two
;;;; entries a bucket) several times under readers that hold no lock.
;;;;
;;;; WHAT MUST HOLD: every thread got the SAME object for a hash (one keyword,
;;;; not two), and a lookup afterwards gets that object too.
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS: the hashes already existing (each is
;;;; checked absent first, through the same lock-free probe), or the threads
;;;; not overlapping (each reports when it started and finished, and the
;;;; windows must intersect).

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

(%sb-threads-up)

(defvar *n* 20000)
(defvar *nthreads* 4)
;; Synthetic name hashes far from any real one (real ones are name-hash
;; fixnums of symbol names; these share a high tag no name hash will have).
(defvar *kbase* 3000000000000000000)
(defvar *sbase* 3100000000000000000)
(defvar *spkg* 51798093)   ; COMMON-LISP-USER's package hash

;; Result vectors, made by MAIN so they live in region 0.  Workers store
;; keyword and symbol objects, which the locked insert allocated in region 0
;; (or its lock arena), so region 0 never points into a worker's region.
(defvar *kres* (let ((v (make-array *nthreads*))) (dotimes (i *nthreads*) (setf (aref v i) (make-array *n*))) v))
(defvar *sres* (let ((v (make-array *nthreads*))) (dotimes (i *nthreads*) (setf (aref v i) (make-array *n*))) v))
(defvar *times* (make-array (* 2 *nthreads*) :initial-element 0))

(defun key-of (i) (+ *kbase* i))
(defun sym-key-of (i) (+ *sbase* i))

(defun present-before ()
  "How many of the hashes the lock-free probe already finds.  Must be 0."
  (let ((n 0))
    (dotimes (i *n*)
      (when (%ht-get-ro (key-of i) (mem-ref #x10000148 :u64)) (setq n (+ n 1)))
      (when (%ht-get-ro (%symbol-pkg-key (sym-key-of i) *spkg*) (mem-ref #x10000088 :u64))
        (setq n (+ n 1))))
    n))

(defun racer (id)
  "Intern every hash, visiting them with a per-thread stride so the threads
   meet each other at different points."
  (let ((kv (aref *kres* id)) (sv (aref *sres* id))
        (stride (aref #(1 7919 104729 1299709) id)) (j (* id 5003)))
    (setf (aref *times* (* 2 id)) (%monotonic-ns))
    (dotimes (k *n*)
      (setq j (mod (+ j stride) *n*))
      (setf (aref kv j) (%intern-keyword (key-of j)))
      (setf (aref sv j) (%intern-symbol-pkg (sym-key-of j) *spkg*)))
    (setf (aref *times* (+ 1 (* 2 id))) (%monotonic-ns))
    0))

(format t "~%=== ~D THREADS INTERN THE SAME ~D KEYWORDS AND SYMBOLS ===~%" *nthreads* *n*)
(chk "none of the hashes existed before" (present-before) 0)
(let ((ths nil))
  (dotimes (w *nthreads*)
    (let ((id w))
      (setq ths (cons (sb-thread:make-thread
                       (lambda () (handler-case (racer id) (error (e) (list :err (%escape-describe e))))))
                      ths))))
  (let ((rs (mapcar (function sb-thread:join-thread) (reverse ths))))
    (chk "every thread finished" rs (make-list *nthreads* :initial-element 0))))

(let ((latest-start 0) (earliest-end nil))
  (dotimes (w *nthreads*)
    (setq latest-start (max latest-start (aref *times* (* 2 w))))
    (let ((e (aref *times* (+ 1 (* 2 w)))))
      (setq earliest-end (if earliest-end (min earliest-end e) e))))
  (chk "the threads ran at the same time (their windows overlap)" (< latest-start earliest-end) t))

(let ((kdiff 0) (sdiff 0) (knotkw 0) (kafter 0) (safter 0))
  (dotimes (i *n*)
    (let ((k0 (aref (aref *kres* 0) i)) (s0 (aref (aref *sres* 0) i)))
      (unless (keywordp k0) (setq knotkw (+ knotkw 1)))
      (dotimes (w *nthreads*)
        (unless (eq (aref (aref *kres* w) i) k0) (setq kdiff (+ kdiff 1)))
        (unless (eq (aref (aref *sres* w) i) s0) (setq sdiff (+ sdiff 1))))
      (unless (eq (%intern-keyword (key-of i)) k0) (setq kafter (+ kafter 1)))
      (unless (eq (%intern-symbol-pkg (sym-key-of i) *spkg*) s0) (setq safter (+ safter 1)))))
  (chk "keywords that differ between threads" kdiff 0)
  (chk "symbols that differ between threads" sdiff 0)
  (chk "results that are not keywords" knotkw 0)
  (chk "keywords a later intern does not return" kafter 0)
  (chk "symbols a later intern does not return" safter 0))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "LOCK-FREE INTERN RACE: PASS" "LOCK-FREE INTERN RACE: FAIL"))
