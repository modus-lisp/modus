;;;; stw-futex-wait.lisp -- STOP-THE-WORLD WAITS BLOCK IN THE KERNEL.
;;;;
;;;;   timeout 600 ./modus --script test/stw-futex-wait.lisp
;;;;   ./modus --eval '(defvar *ncollect* 200)' --eval '(defvar *nworkers* 8)' --load ...
;;;;
;;;; A region-0 collection stops every other thread.  The collector used to wait
;;;; for each running thread to park in a sched_yield loop, and every parked
;;;; thread waited for the collection to end in another one: a box running N
;;;; threads spent N cores in system time for the length of every collection,
;;;; and on a loaded box those spinners took the CPU from the collector and from
;;;; the threads it was waiting for.  Both waits are futex waits now
;;;; (translate-x64 / translate-aarch64, STOP-THE-WORLD FOR REGION 0, "THE
;;;; WAITS SLEEP"): the collector sleeps on the thread table's +0xC0 word and is
;;;; woken by a thread that stops; waiters sleep on the stop flag itself and are
;;;; woken by the collector's release.
;;;;
;;;; PHASE A, CORRECTNESS UNDER STRESS.  Main runs *NCOLLECT* region-0
;;;; collections back to back while *NWORKERS* threads run four kinds of loop:
;;;;   (0) allocating lists in their own region and checking every one,
;;;;   (1) a fixnum loop that never allocates (it parks at its back-edge),
;;;;   (2) re-summing main's vector, which every collection moves,
;;;;   (3) blocking in CHANNEL-RECEIVE for fixnums main sends between
;;;;       collections (a safe region entered and left around each one),
;;;; and each checks its own values on every pass.  A lost wake is a HANG
;;;; (run it under `timeout'; the waits' own timeouts make it a slow pass
;;;; instead, so %STW-WAIT-STATS counts sleeps that timed out with their
;;;; condition already true, and this test bounds them).
;;;; PHASE B, IDLE.  Every worker blocks in CHANNEL-RECEIVE while main
;;;; collects; system time per collection is reported.
;;;; Per-collection pauses (main's wall time around each collection: the stop,
;;;; the copy and the release) are reported as median / p90 / max.

(defvar *nworkers* 6)
(defvar *ncollect* 2000)
(defvar *nidle* 300)
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

(defun region0-gcs ()
  (modus-int::%gc-meta-read (+ (modus-int::%gc-region-0) #x20) (modus-int::%gc-meta-scale)))
(defun now-us () (get-internal-real-time))

(defun proc-ticks ()
  "(utime stime) of this process, in clock ticks, from /proc/self/stat."
  (with-open-file (s "/proc/self/stat")
    (let* ((line (read-line s))
           (p (position #\) line :from-end t))
           (rest (subseq line (+ p 2)))
           (fields nil) (start 0))
      ;; fields after the comm: state is field 3, utime 14, stime 15
      (loop for i from 0 below (length rest)
            do (when (char= (char rest i) #\Space)
                 (push (subseq rest start i) fields)
                 (setq start (+ i 1))))
      (setq fields (nreverse fields))
      (list (parse-integer (nth 11 fields)) (parse-integer (nth 12 fields))))))

(defvar *stop* nil)
(defvar *vec* (make-array 2000))
(dotimes (i 2000) (setf (aref *vec* i) (list i (* 3 i))))
(defun vec-sum (v) (let ((s 0)) (dotimes (i (length v)) (setq s (+ s (cadr (aref v i))))) s))
(defvar *want-vec* (vec-sum *vec*))

(defun w-alloc ()
  (let ((passes 0) (bad 0))
    (loop (when *stop* (return (list passes bad)))
          (let ((l (make-list 200 :initial-element 7)) (s 0))
            (dolist (x l) (setq s (+ s x)))
            (unless (= s 1400) (setq bad (+ bad 1))))
          (setq passes (+ passes 1)))))
(defun w-fixnum ()
  (let ((s 0) (k 0))
    (loop (when *stop* (return (list k s)))
          (setq s (logand (+ s k) #xffffff))
          (setq k (+ k 1)))))
(defun w-vector ()
  (let ((passes 0) (bad 0))
    (loop (when *stop* (return (list passes bad)))
          (unless (= (vec-sum *vec*) *want-vec*) (setq bad (+ bad 1)))
          (setq passes (+ passes 1)))))
(defun w-channel (ch)
  "Receive fixnums until -1; each must be the previous plus one."
  (let ((expect 0) (bad 0) (n 0))
    (loop (let ((v (modus::channel-receive ch)))
            (when (eql v -1) (return (list n bad)))
            (unless (eql v expect) (setq bad (+ bad 1)))
            (setq expect (+ v 1) n (+ n 1))))))

(defun stats ()
  (if (fboundp 'modus-int::%stw-wait-stats) (modus-int::%stw-wait-stats) :none))

(defun report-pauses (name v)
  (let* ((s (sort (copy-seq v) #'<)) (n (length s)))
    (format t "  ~A pauses over ~D collections: median ~D us  p90 ~D us  max ~D us  total ~D ms~%"
            name n (aref s (floor n 2)) (aref s (floor (* 9 n) 10)) (aref s (- n 1))
            (floor (reduce #'+ s) 1000))))

;;; ---- PHASE A -----------------------------------------------------------
(format t "~%=== A: ~D collections, ~D workers ===~%" *ncollect* *nworkers*)
(defvar *chans* nil)
(defvar *ths* nil)
(setq *stop* nil)
(dotimes (i *nworkers*)
  (let ((kind (mod i 4)))
    (push (cons kind
                (case kind
                  (0 (sb-thread:make-thread #'w-alloc))
                  (1 (sb-thread:make-thread #'w-fixnum))
                  (2 (sb-thread:make-thread #'w-vector))
                  (3 (let ((ch (modus::make-channel)))
                       (push ch *chans*)
                       (sb-thread:make-thread (lambda () (w-channel ch)))))))
          *ths*)))
(setq *ths* (nreverse *ths*))
(modus-int::%sleep-ms 200)
(defvar *pauses* (make-array *ncollect*))
(defvar *sent* 0)
(let* ((g0 (region0-gcs)) (t0 (now-us)) (k0 (proc-ticks)) (st0 (stats)))
  (dotimes (i *ncollect*)
    (let ((a (now-us)))
      (modus-int::%gc-collect-region-0)
      (setf (aref *pauses* i) (- (now-us) a)))
    (when (= 0 (mod i 4))
      (dolist (ch *chans*) (modus::channel-send ch *sent*))
      (setq *sent* (+ *sent* 1))))
  (let ((wall (- (now-us) t0)) (k1 (proc-ticks)) (g (- (region0-gcs) g0)))
    (format t "  region 0 collections: ~D in ~D ms~%" g (floor wall 1000))
    (format t "  process CPU during them: user ~D ticks, system ~D ticks (wall ~D ticks)~%"
            (- (car k1) (car k0)) (- (cadr k1) (cadr k0)) (floor wall 10000))
    (format t "  stw wait stats before ~S after ~S~%" st0 (stats))
    (report-pauses "A" *pauses*)
    (chk-true "POSITIVE CONTROL: every requested collection happened" (>= g *ncollect*))
    ;; LENIENT: the sched_yield waits spent 2-3 cores of system time per
    ;; core of wall time here; sleeping, it is ~0.005.
    (chk-true "system time during the collections under a quarter of wall time"
              (< (* 4 (- (cadr k1) (cadr k0))) (floor wall 10000)))
    (let ((st (stats)))
      (when (consp st)
        (chk-true "POSITIVE CONTROL: the collector slept on a running thread" (> (first st) 0))
        (chk-true "POSITIVE CONTROL: stopped threads slept until the release" (> (third st) 0))
        ;; A "lost" wake is a sleep that timed out with its condition already
        ;; true.  On a box with idle cores it reads 0; on one with a load of
        ;; twice its cores a waiter can be preempted between the kernel's
        ;; timeout and its own re-check, so allow 1%.
        (chk-true "collector: no lost wakes beyond 1% of its sleeps"
                  (<= (second st) (max 2 (floor (first st) 100))))
        (chk-true "waiters: no lost wakes beyond 1% of their sleeps"
                  (<= (fourth st) (max 2 (floor (third st) 100))))))))
(setq *stop* t)
(dolist (ch *chans*) (modus::channel-send ch -1))
(dolist (e *ths*)
  (let ((r (sb-thread:join-thread (cdr e))))
    (case (car e)
      (0 (chk "alloc worker: bad lists" (cadr r) 0) (chk-true "alloc worker made passes" (> (car r) 0)))
      (1 (chk-true "fixnum worker counted" (and (integerp (car r)) (> (car r) 0))))
      (2 (chk "vector worker: bad sums" (cadr r) 0) (chk-true "vector worker made passes" (> (car r) 0)))
      (3 (chk "channel worker: received everything, in order" r (list *sent* 0))))))
(chk "main's vector still sums right" (vec-sum *vec*) *want-vec*)

;;; ---- PHASE B -----------------------------------------------------------
(format t "~%=== B: ~D collections, ~D workers blocked in CHANNEL-RECEIVE ===~%" *nidle* *nworkers*)
(setq *chans* nil *ths* nil)
(dotimes (i *nworkers*)
  (let ((ch (modus::make-channel)))
    (push ch *chans*)
    (push (sb-thread:make-thread (lambda () (w-channel ch))) *ths*)))
(modus-int::%sleep-ms 200)
(defvar *pauses-b* (make-array *nidle*))
(let* ((t0 (now-us)) (k0 (proc-ticks)))
  (dotimes (i *nidle*)
    (let ((a (now-us)))
      (modus-int::%gc-collect-region-0)
      (setf (aref *pauses-b* i) (- (now-us) a))))
  (let ((wall (- (now-us) t0)) (k1 (proc-ticks)))
    (format t "  process CPU: user ~D ticks, system ~D ticks (wall ~D ticks)~%"
            (- (car k1) (car k0)) (- (cadr k1) (cadr k0)) (floor wall 10000))
    (report-pauses "B" *pauses-b*)))
(dolist (ch *chans*) (modus::channel-send ch 0) (modus::channel-send ch -1))
(dolist (th *ths*) (chk "idle worker: one message, in order" (sb-thread:join-thread th) (list 1 0)))
(format t "  stw wait stats ~S~%" (stats))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "STW FUTEX WAIT: PASS" "STW FUTEX WAIT: FAIL"))
