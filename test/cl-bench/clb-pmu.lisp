;;;; test/cl-bench/clb-pmu.lisp -- one cl-bench benchmark, timed or under the PMU.
;;;;
;;;; cl-bench (https://gitlab.common-lisp.net/ansi-test/cl-bench) needs
;;;; trivial-garbage.  CL_BENCH_DIR / TRIVIAL_GARBAGE_DIR point at them (with a
;;;; trailing slash).  Copy the cl-bench tree per Lisp: both write into output/.
;;;;
;;;;   CLB=TAK CLB_MODE=time ./modus --script test/cl-bench/clb-pmu.lisp
;;;;       -> "CLB-TIME TAK <run seconds>": one benchmark per PROCESS, which is how
;;;;          to compare -- inside one image the string-hash benchmarks leave
;;;;          ~0.5 GB key sets live (strings are 8 bytes a character here) and
;;;;          the next one's setup thrashes the collector.
;;;;   docker run --rm --cap-add SYS_ADMIN --cap-add IPC_LOCK -v ... modus-perf \
;;;;     env CLB=STAK CLB_MODE=both [CLB_ANNOTATE=NAME] ./modus --script ...
;;;;       -> PMU-MEASURE tables (:summary :frontend :memory) and a PMU-PROFILE;
;;;;          CLB_ANNOTATE also writes ann-<bench>.bin/.hits for
;;;;          scripts/pmu-annotate.py.
;;;;   test/cl-bench/clb-compare.py SBCL-OUTPUT MODUS-OUTPUT   (cl-bench report files,
;;;;       or lines ";; NAME real user ..."): ratios, sorted, and the geometric mean.
;;;;
;;;; Benchmarks that depend on earlier ones (CLOS/methodcalls needs CLOS/defclass
;;;; and CLOS/defmethod) fail run alone; run that group in order.

(require :asdf)
(push (pathname (or (sb-ext:posix-getenv "CL_BENCH_DIR") "cl-bench/")) asdf:*central-registry*)
(push (pathname (or (sb-ext:posix-getenv "TRIVIAL_GARBAGE_DIR") "trivial-garbage/")) asdf:*central-registry*)
(asdf:load-system "cl-bench")
(jit-eager)
(defun getenv (n) (let ((v (sb-ext:posix-getenv n))) (and v (> (length v) 0) v)))
(defun find-bench (name)
  (find name (symbol-value (find-symbol "*BENCHMARKS*" "CL-BENCH"))
        :key (lambda (b) (string (slot-value b (find-symbol "SHORT" "CL-BENCH")))) :test #'string-equal))
(defun run-bench (b)
  (let ((setup (slot-value b (find-symbol "SETUP" "CL-BENCH")))
        (fn (slot-value b (find-symbol "FUNCTION" "CL-BENCH")))
        (runs (slot-value b (find-symbol "RUNS" "CL-BENCH"))))
    (when setup (if (listp setup) (eval setup) (funcall setup)))
    (lambda () (dotimes (i runs) (funcall fn)))))
(let* ((name (or (getenv "CLB") "TAK"))
       (mode (or (getenv "CLB_MODE") "both"))
       (b (or (find-bench name) (error "no benchmark ~a" name)))
       (thunk (run-bench b)))
  (funcall thunk)                                   ; warm: hot JIT, tables
  (format t "~&===== ~a~%" name)
  (when (string= mode "time")
    (let ((t0 (get-internal-run-time)))
      (funcall thunk)
      (format t "~&CLB-TIME ~a ~,3f~%" name (/ (- (get-internal-run-time) t0) (float internal-time-units-per-second)))))
  (when (member mode '("measure" "both") :test #'string=)
    (let ((t0 (get-internal-real-time)))
      (pmu-measure thunk)
      (format t "  wall ~,3f s~%" (/ (- (get-internal-real-time) t0) (float internal-time-units-per-second))))
    (pmu-measure thunk :frontend)
    (pmu-measure thunk :memory))
  (when (member mode '("profile" "both") :test #'string=)
    (multiple-value-bind (r p) (pmu-profile thunk :period 100000 :pages 8192 :top 25)
      (declare (ignore r))
      (let ((an (getenv "CLB_ANNOTATE")))
        (when an
          (pmu-annotate p an (format nil "ann-~a" (substitute (code-char 95) (code-char 47) (string-downcase name)))))))))
