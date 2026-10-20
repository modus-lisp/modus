;;;; test/hosted-pmu.lisp -- net/hosted-pmu.lisp counts and samples what it says.
;;;;
;;;;   docker run --rm --cap-add SYS_ADMIN --cap-add IPC_LOCK -v $PWD:$PWD -w $PWD \
;;;;       modus-perf ./modus --script test/hosted-pmu.lisp
;;;;
;;;; Needs perf_event_open (kernel.perf_event_paranoid <= 2, or CAP_PERFMON /
;;;; CAP_SYS_ADMIN).  Where it is refused the test checks that the refusal is an
;;;; ERROR saying why -- never silent zeros -- and reports SKIP.

(defvar *fails* 0)
(defun chk (name ok)
  (format t "~&  ~a ~a~%" (if ok "ok  " "FAIL") name)
  (unless ok (setq *fails* (+ *fails* 1))))

(defun spin (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s))
(defun leaf (x) (let ((s 0)) (dotimes (i 50) (setq s (+ s (* x i)))) s))
(defun mid (n) (let ((s 0)) (dotimes (i n) (setq s (+ s (leaf i)))) s))
(jit-eager)

(let ((refusal (handler-case (progn (pmu-start :instructions) (pmu-stop) nil)
                 (error (e) (princ-to-string e)))))
  (if refusal
      (progn
        (chk "a refusal names the cause" (search "perf_event_open" refusal))
        (format t "~&HOSTED-PMU: SKIP (~a)~%" refusal)
        (sys-exit 0))
      (progn
        ;; COUNTING: the work is linear in N, so instructions must scale with it
        (multiple-value-bind (r1 c1) (pmu-measure (lambda () (spin 1000000)) :instructions :branches)
          (multiple-value-bind (r2 c2) (pmu-measure (lambda () (spin 4000000)) :instructions :branches)
            (chk "the thunk's value comes back" (and (= r1 499999500000) (= r2 7999998000000)))
            (let ((i1 (getf c1 :instructions)) (i2 (getf c2 :instructions)))
              (chk "instructions counted" (and (> i1 1000000) (> i2 i1)))
              (chk "4x the work is ~4x the instructions" (< 3.5 (/ i2 (float i1)) 4.5))
              (chk "a loop retires at least one branch per iteration"
                   (>= (getf c2 :branches) 4000000))
              (chk "cycles counted" (> (getf c2 :cycles) 0)))))
        (chk "a group too big for the PMU says so, not zeros"
             (let ((c (handler-case
                          (progn (pmu-start :instructions :branches :branch-misses :l1d-read-miss
                                            :l1i-read-miss :itlb-read-miss :stalled-cycles-frontend)
                                 (spin 100000)
                                 (prog1 (pmu-read) (pmu-stop)))
                        (error (e) (pmu-stop) (list :error e)))))
               (or (getf c :not-counted) (getf c :running-fraction) (getf c :error)
                   (> (getf c :instructions 0) 0))))
        (chk "an unknown event is an error"
             (handler-case (progn (pmu-start :no-such-event) nil) (error () t)))
        ;; SAMPLING: LEAF does nearly all the work, MID calls it
        (multiple-value-bind (r p) (pmu-profile (lambda () (mid 300000)) :period 100000 :top 5)
          (chk "profile returns the value" (= r (mid 300000)))
          (let* ((self (getf p :self)) (incl (getf p :inclusive))
                 (leaf-self (cdr (find "LEAF" self :key #'car :test #'search)))
                 (mid-incl (cdr (find "MID" incl :key #'car :test #'search)))
                 (n (getf p :samples)))
            (chk "samples taken" (> n 100))
            (chk "the leaf has most self samples" (and leaf-self (> (/ leaf-self (float n)) 0.6)))
            (chk "its caller has them inclusively" (and mid-incl (>= mid-incl leaf-self)))
            (chk "annotation finds the function and writes its code"
                 (let ((pre (format nil "/tmp/hosted-pmu-test-~d" (random 1000000 (make-random-state t)))))
                   (and (integerp (pmu-annotate p 'leaf pre))
                        (probe-file (concatenate 'string pre ".bin"))
                        (probe-file (concatenate 'string pre ".hits"))))))))))
(format t "~&HOSTED-PMU: ~a failed~%" *fails*)
(sys-exit (if (zerop *fails*) 0 1))
