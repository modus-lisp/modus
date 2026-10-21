;;;; test/pmu-calibrate.lisp -- does the sampling profiler tell the truth?
;;;;
;;;;   docker run --rm --cap-add SYS_ADMIN --cap-add IPC_LOCK \
;;;;     -v /home/claude:/home/claude -w $PWD modus-perf ./modus --script test/pmu-calibrate.lisp
;;;;
;;;; A profile is only evidence once it has been shown to agree with something
;;;; MEASURED.  Two workloads whose true split is taken from the clock -- the
;;;; same loop with and without the expensive function -- then PMU-PROFILE must
;;;; put each one's share on the right function:
;;;;   DIVIDE  HEAVY (a chain of integer FLOORs) against LIGHT, called alike;
;;;;   CHASE   a cache-missing load in CHASE (a random cycle over 4M slots, 32
;;;;           MB) returning to a caller that does almost nothing after it --
;;;;           the shape skid would charge to the caller.
;;;; :CYCLES must put each share within TOLERANCE points of the clock's; :IBS-OP
;;;; must put most sampled data-cache misses on CHASE.  It also states, by
;;;; measurement, why IBS is not a TIME profile here: its per-op samples and
;;;; tag-to-retire cycles both undercount the chase badly (see PMU-PROFILE).
;;;; Needs perf_event_open: kernel.perf_event_paranoid <= 2 or CAP_SYS_ADMIN.
;;;; With neither it says SKIP and exits 0; IBS checks SKIP off AMD.

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok &optional detail)
  (setq *checks* (+ *checks* 1))
  (unless ok (setq *fails* (+ *fails* 1)))
  (format t "~a ~a~@[  ~a~]~%" (if ok "ok  " "FAIL") name detail))
(defvar *tolerance* 6.0)

(defun heavy (x) (let ((a (+ x 1000000007))) (dotimes (k 12) (setq a (+ 1000003 (floor a 3)))) a))
(defun light (x) (+ x 1))
(defun divide (n) (let ((s 0)) (dotimes (i n) (setq s (logand (+ (heavy i) (light s)) #xffffff))) s))
(defun divide-light (n) (let ((s 0)) (dotimes (i n) (setq s (logand (+ i (light s)) #xffffff))) s))

(defvar *perm* (let* ((n (* 4 1024 1024)) (v (make-array n)) (idx (make-array n)))
                 (dotimes (i n) (setf (svref idx i) i))
                 (loop for i from (1- n) downto 1 do (rotatef (svref idx i) (svref idx (random (1+ i)))))
                 (dotimes (i n) (setf (svref v (svref idx i)) (svref idx (mod (1+ i) n))))
                 v))
(defun chase (i) (svref *perm* i))
(defun after (i) (logxor i 1))
(defun walk (n) (let ((i 0)) (dotimes (k n) (setq i (after (chase i)))) i))
(defun walk-light (n) (let ((i 0)) (dotimes (k n) (setq i (after (logand (+ i 7) 4194303)))) i))

(dotimes (i 70) (divide 10) (divide-light 10) (walk 10) (walk-light 10))   ; hot JIT

(defun clock (f n) (let ((t0 (get-internal-real-time))) (funcall f n) (- (get-internal-real-time) t0)))
(defun true-share (f f-light n)
  (let ((a (clock f n)) (b (clock f-light n))) (* 100.0 (/ (- a b) a))))
(defun share (profile key name)
  (let ((e (assoc name (getf profile key) :test #'string=))
        (tot (let ((s 0)) (dolist (x (getf profile key) s) (setq s (+ s (cdr x)))))))
    (if (and e (> tot 0)) (* 100.0 (/ (cdr e) tot)) 0.0)))
(defun profile (thunk &rest keys)
  (handler-case (nth-value 1 (apply #'pmu-profile thunk :stream (make-broadcast-stream) keys))
    (error (c) (format t "~a~%" c) nil)))

(let ((truth-div (true-share #'divide #'divide-light 2000000))
      (truth-chase (true-share #'walk #'walk-light 2000000))
      (cyc-div (profile (lambda () (divide 2000000)))))
  (format t "clock: HEAVY ~,1f% of DIVIDE, CHASE ~,1f% of WALK~%" truth-div truth-chase)
  (if (null cyc-div)
      (format t "SKIP: perf_event_open refused (see above)~%")
      (let ((cyc-chase (profile (lambda () (walk 2000000)))))
        (let ((p (share cyc-div :self "HEAVY")) (q (share cyc-chase :self "CHASE")))
          (check "cycles: HEAVY's share matches the clock" (<= (abs (- p truth-div)) *tolerance*)
                 (format nil "~,1f% vs ~,1f%" p truth-div))
          (check "cycles: CHASE's share matches the clock -- no skid onto the caller"
                 (<= (abs (- q truth-chase)) *tolerance*) (format nil "~,1f% vs ~,1f%" q truth-chase)))
        (let ((ibs (profile (lambda () (walk 2000000)) :event :ibs-op :period 20000)))
          (if (null ibs)
              (format t "SKIP: IBS (AMD only)~%")
              (let ((m (share ibs :dc-miss "CHASE")) (o (share ibs :self "CHASE"))
                    (l (share ibs :latency "CHASE")))
                (check "ibs-op: data-cache misses land on CHASE" (>= m 80.0) (format nil "~,1f% of misses" m))
                (check "ibs-op: op samples are NOT a time profile (well under the clock's share)"
                       (< o (- truth-chase 15.0)) (format nil "ops ~,1f%, tag-to-retire ~,1f%, clock ~,1f%" o l truth-chase))))))))
(format t "~&pmu-calibrate: ~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
