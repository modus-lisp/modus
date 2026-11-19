;;;; stw-spinning-worker.lisp — A LOOP THAT NEVER ALLOCATES MUST NOT HOLD UP
;;;; A STOP-THE-WORLD COLLECTION.
;;;;
;;;;   timeout 120 ./modus --script test/stw-spinning-worker.lisp
;;;;
;;;; A region-0 collection stops every other thread, and a thread used to stop
;;;; only at an ALLOCATION (a back-edge merely clamped its allocation limit).
;;;; A worker spinning in a loop with no allocation therefore never stopped:
;;;; main sat in the collector waiting for it (sched_yield, high system CPU),
;;;; and when main was the one that would end the loop -- by setting the flag
;;;; the worker polls -- the process hung for good.  A back-edge in a function
;;;; that holds no raw word now parks (compiler.lisp *PARK-SAFEPOINTS*,
;;;; translate-x64 EMIT-X64-STW-SAFEPOINT).
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS, AND WHAT STOPS IT.
;;;;   - No region-0 collection while a worker spun: every arm requires region
;;;;     0's own collection count to rise by *MIN-GCS* while its worker runs.
;;;;   - The worker finished first, so nothing waited on it: arms (a), (c), (d)
;;;;     loop until main sets *STOP*, which main does only after its
;;;;     collections; arm (b) has no flag at all and must still be RUNNING when
;;;;     main's collections are done (before the fix main finished after it).
;;;;   - Values: every worker's result is checked against what it must be;
;;;;     arm (c) re-sums main's vector, which those collections MOVE, on every
;;;;     pass; main's lists keep their lengths.
;;;; A hang is the failure this guards against: run it under `timeout'.

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

(defvar *min-gcs* 3)
(defvar *stop* nil)
(defvar *n* 0)
(defvar *mine* 0)
(defvar *vec* (make-array 1000))
(dotimes (i 1000) (setf (aref *vec* i) (list i (* 2 i))))
(defun vec-sum (v) (let ((s 0)) (dotimes (i (length v)) (setq s (+ s (cadr (aref v i))))) s))
(defvar *want-vec* (vec-sum *vec*))

(defun region0-gcs ()
  (modus-int::%gc-meta-read (+ (modus-int::%gc-region-0) #x20) (modus-int::%gc-meta-scale)))
(defun now-ms () (floor (get-internal-real-time) 1000))

(defvar *kept* nil)
(defun churn ()
  "Allocate in region 0 until it has collected *MIN-GCS* more times; keep a
   few of the lists so their lengths can be checked.  Returns the collections
   and the milliseconds taken."
  (let ((g0 (region0-gcs)) (t0 (now-ms)) (i 0))
    (setq *kept* nil)
    (loop
      (when (or (>= (- (region0-gcs) g0) *min-gcs*) (> i 4000)) (return nil))
      (let ((l (make-list 100000)))
        (when (= 0 (mod i 50)) (setq *kept* (cons l *kept*))))
      (setq i (+ i 1)))
    (list (- (region0-gcs) g0) (- (now-ms) t0))))

(defun kept-ok () (every (lambda (l) (= (length l) 100000)) *kept*))

;;; ---- the workers ------------------------------------------------------
;;; (a) special reads and writes, nothing else
(defun spin-special ()
  (let ((i 0))
    (loop (when *stop* (return i))
          (setq i (logand (+ i 1) #xffff))
          (setq *n* i))))
;;; (b) pure fixnum arithmetic, no flag at all: a fixed count
(defvar *fix-iters* 1500000000)
(defun spin-fixnum (n)
  (let ((s 0))
    (dotimes (k n) (setq s (logand (+ s k) #xffffff)))
    s))
(defun fixnum-want (n)
  ;; same arithmetic, but the masked sum has period 2^24 in k: count whole
  ;; periods only when N is a multiple of it, so just recompute it here --
  ;; main runs it AFTER the worker, unthreaded, as the reference.
  (spin-fixnum n))
;;; (c) reading main's shared vector (region 0, moved by every collection)
(defun spin-array ()
  (let ((passes 0) (bad 0))
    (loop (when *stop* (return (list passes bad)))
          (unless (= (vec-sum *vec*) *want-vec*) (setq bad (+ bad 1)))
          (setq passes (+ passes 1)))))
;;; (d) a special the worker bound itself
(defun spin-own-binding ()
  (let ((*mine* 0))
    (loop (when *stop* (return *mine*))
          (setq *mine* (+ *mine* 1)))))

;;; (c2) a typed-array pixel loop over main's (unsigned-byte 32) array,
;;; written into the worker's own one; the source moves at every collection
(defvar *src32* (make-array 4096 :element-type '(unsigned-byte 32)))
(dotimes (i 4096) (setf (aref *src32* i) (logand (* i 2654435761) #xffffffff)))
(defun swizzle (src dst)
  (declare (type (simple-array (unsigned-byte 32) (*)) src dst))
  (dotimes (i (length src))
    (let ((p (aref src i)))
      (setf (aref dst i) (logior (logand p #xff00ff00) (ash (logand p #xff) 16)
                                 (logand (ash p -16) #xff)))))
  dst)
(defun u32-sum (a) (let ((s 0)) (dotimes (i (length a)) (setq s (+ s (aref a i)))) s))
(defvar *want-swizzle* (u32-sum (swizzle *src32* (make-array 4096 :element-type '(unsigned-byte 32)))))
(defun spin-pixels ()
  (let ((dst (make-array 4096 :element-type '(unsigned-byte 32))) (passes 0) (bad 0))
    (loop (when *stop* (return (list passes bad)))
          (swizzle *src32* dst)
          (unless (= (u32-sum dst) *want-swizzle*) (setq bad (+ bad 1)))
          (setq passes (+ passes 1)))))

(defun run-arm (name fn check)
  (format t "~%=== ~A ===~%" name)
  (setq *stop* nil)
  (let* ((th (sb-thread:make-thread fn)))
    (modus-int::%sleep-ms 100)
    (let ((c (churn)))
      (format t "  region 0 collections: ~D in ~D ms~%" (car c) (cadr c))
      (chk-true "POSITIVE CONTROL: region 0 collected while the worker spun"
                (>= (car c) *min-gcs*))
      (setq *stop* t)
      (let ((r (sb-thread:join-thread th)))
        (format t "  worker result: ~S~%" r)
        (funcall check r)
        (chk-true "main's kept lists keep their lengths" (kept-ok))))))

(run-arm "(a) SPECIAL READS" #'spin-special
         (lambda (r) (chk-true "worker returned a fixnum it computed" (and (integerp r) (<= 0 r #xffff)))
                     (chk "*n* is the worker's last write" *n* r)))

(format t "~%=== (b) PURE FIXNUM LOOP, NO FLAG ===~%")
(let* ((done nil) (res nil)
       (th (sb-thread:make-thread (lambda () (setq res (spin-fixnum *fix-iters*)) (setq done t) res))))
  (modus-int::%sleep-ms 100)
  (let ((c (churn)))
    (format t "  region 0 collections: ~D in ~D ms~%" (car c) (cadr c))
    (chk-true "POSITIVE CONTROL: region 0 collected while the worker spun"
              (>= (car c) *min-gcs*))
    (chk-true "main's collections did not wait for the loop to end" (not done))
    (chk-true "main's kept lists keep their lengths" (kept-ok))
    (let ((r (sb-thread:join-thread th)))
      (format t "  worker result: ~S~%" r)
      (chk "worker's sum is the unthreaded sum" r (fixnum-want *fix-iters*)))))

(run-arm "(c) READING MAIN'S VECTOR" #'spin-array
         (lambda (r) (chk "bad sums" (cadr r) 0)
                     (chk-true "more than one pass" (> (car r) 1))
                     (chk "main's vector still sums right" (vec-sum *vec*) *want-vec*)))

(run-arm "(c2) A TYPED-ARRAY PIXEL LOOP OVER MAIN'S ARRAY" #'spin-pixels
         (lambda (r) (chk "bad pixel sums" (cadr r) 0)
                     (chk-true "more than one pass" (> (car r) 1))))

(run-arm "(d) A SPECIAL IT BOUND ITSELF" #'spin-own-binding
         (lambda (r) (chk-true "worker counted" (and (integerp r) (> r 0)))
                     (chk "the global *mine* is untouched" *mine* 0)))

;;; ---- the reverse: MAIN spins, a worker collects -------------------------
;;; A worker collecting its OWN region needs nobody; one collecting REGION 0
;;; (%GC-COLLECT-REGION-0, as the lock-arena refill does) must stop main, and
;;; main is in a loop that never allocates.
(format t "~%=== (e) MAIN SPINS WHILE WORKERS COLLECT ===~%")
(defun main-spin ()
  (let ((i 0))
    (loop (when *stop* (return i))
          (setq i (logand (+ i 1) #xffffff)))))
(setq *stop* nil)
(let* ((g0 (region0-gcs))
       (own (sb-thread:make-thread
             (lambda () (let ((n 0) (l nil))
                          (dotimes (k 3000) (setq l (make-list 10000)) (setq n (+ n (length l))))
                          n))))
       (r0 (sb-thread:make-thread
            (lambda () (dotimes (k 3) (modus-int::%gc-collect-region-0)) (setq *stop* t) :done)))
       (spun (main-spin)))
  (chk "the worker that collected region 0 finished" (sb-thread:join-thread r0) :done)
  (chk "the worker that collected its own region counted every list"
       (sb-thread:join-thread own) (* 3000 10000))
  (chk-true "main's spin ended (it was stopped and resumed)" (integerp spun))
  (chk-true "POSITIVE CONTROL: region 0 collected while main spun" (>= (- (region0-gcs) g0) 3))
  (chk "main's vector still sums right" (vec-sum *vec*) *want-vec*))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "STW SPINNING WORKER: PASS" "STW SPINNING WORKER: FAIL"))
