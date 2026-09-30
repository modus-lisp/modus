;;;; hosted-stw.lisp — REGION 0 COLLECTS WHILE WORKERS RUN LISP ON ITS OBJECTS.
;;;;
;;;;   ./modus --script test/hosted-stw.lisp
;;;;
;;;; Region 0 is main's heap and every thread's shared world: the code, the
;;;; symbols, and whatever main hands a worker.  Its collector MOVES objects.
;;;; Until stop-the-world (translate-aarch64, STOP-THE-WORLD FOR REGION 0) a
;;;; region-0 collection with a worker running forwarded those objects and left
;;;; the worker reading from-space: the documented "region 0 must not collect
;;;; under threads".  This test does exactly that, on purpose, many times.
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS, AND WHAT STOPS IT.
;;;;   - No region-0 collection happened while the workers ran: then nothing was
;;;;     tested.  The POSITIVE CONTROL requires region 0's own collection count
;;;;     to rise during the run, by at least *MIN-GCS*.
;;;;   - The workers finished before main collected: each worker checks the data
;;;;     until main says stop, and main says stop only after its collections.
;;;;   - A check that cannot fail: every traversal recomputes a SUM of the
;;;;     shared list and of the shared vector's string lengths, and calls the
;;;;     shared closure, against values fixed before any thread started.

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))
(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A~%" name))))

(defvar *nworkers* 4)
(defvar *min-gcs* 3)
(defvar *stop* nil)

;;; The shared world — built by MAIN, so it lives in REGION 0.
(defvar *list* nil)
(defvar *vec* nil)
(defvar *fn* nil)
(let ((i 0) (acc nil))
  (loop (when (>= i 2000) (return nil)) (setq acc (cons (* i 3) acc)) (setq i (+ i 1)))
  (setq *list* acc))
(setq *vec* (make-array 200))
(dotimes (i 200) (setf (aref *vec* i) (make-string (+ 1 (mod i 17)) :initial-element #\x)))
;; Main's closure, run by the MVM interpreter on every worker: the case that
;; found stop-the-world must not park at loop back-edges.
(defun make-adder (k) (lambda (x) (+ x k)))
(setq *fn* (make-adder 7))

(defun list-sum (l) (let ((s 0)) (dolist (e l) (setq s (+ s e))) s))
(defun vec-sum (v) (let ((s 0)) (dotimes (i (length v)) (setq s (+ s (length (aref v i))))) s))
(defvar *want-list* (list-sum *list*))
(defvar *want-vec* (vec-sum *vec*))

(defun worker (id)
  "Traverse the shared world until told to stop; count bad traversals.  Also
   allocate in this worker's own region, so both kinds of collection run."
  (let ((bad 0) (n 0) (junk nil))
    (loop
      (when *stop* (return nil))
      (unless (= (list-sum *list*) *want-list*) (setq bad (+ bad 1)))
      (unless (= (vec-sum *vec*) *want-vec*) (setq bad (+ bad 1)))
      (unless (= (funcall *fn* id) (+ id 7)) (setq bad (+ bad 1)))
      (setq junk (cons n (make-list 50)))
      (setq n (+ n 1)))
    (list bad n (length junk))))

(defun region0-gcs () (%gc-meta-read (+ (%gc-region-0) #x20) (%gc-meta-scale)))

(format t "~%=== REGION 0 COLLECTS UNDER RUNNING WORKERS ==============~%")
(let ((ths nil) (g0 0) (g1 0) (i 0))
  (dotimes (w *nworkers*)
    (let ((id w))
      (setq ths (cons (sb-thread:make-thread (lambda () (worker id))) ths))))
  (%sleep-ms 50)
  (setq g0 (region0-gcs))
  ;; Churn region 0 until it has collected *MIN-GCS* times (bounded).  In
  ;; ARRAYS, about 8 MB a round: region 0 is ~600 MB once the lock arena is
  ;; carved, and a round of 20 000 conses (320 KB) never filled it -- the
  ;; collections this counted used to come from the lock arena's refills,
  ;; which lock-free interning (mvm/prelude.lisp %HT-GET-RO) made rare.
  (loop
    (when (or (>= (- (region0-gcs) g0) *min-gcs*) (> i 2000)) (return nil))
    (let ((junk nil) (j 0))
      (loop (when (>= j 50) (return nil)) (setq junk (make-array 20000)) (setq j (+ j 1))))
    (setq i (+ i 1)))
  (setq g1 (region0-gcs))
  (setq *stop* t)
  (let ((results (mapcar (function sb-thread:join-thread) ths)))
    (format t "  region 0 collections during the run: ~D~%" (- g1 g0))
    (format t "  worker results (bad traversals, count, junk): ~S~%" results)
    (chk-true "POSITIVE CONTROL: region 0 collected while the workers ran"
              (>= (- g1 g0) *min-gcs*))
    (chk "bad traversals, all workers" (apply (function +) (mapcar (function car) results)) 0)
    (chk-true "every worker traversed more than once"
              (every (lambda (r) (> (cadr r) 1)) results))
    (chk "main's list still sums right" (list-sum *list*) *want-list*)
    (chk "main's vector still sums right" (vec-sum *vec*) *want-vec*)
    (chk "main's closure still answers" (funcall *fn* 1) 8)))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "REGION 0 STOP-THE-WORLD: PASS" "REGION 0 STOP-THE-WORLD: FAIL"))
