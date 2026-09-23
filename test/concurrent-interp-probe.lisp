;;;; concurrent-interp-probe.lisp — TWO THREADS RUNNING INTERPRETED CODE.
;;;;
;;;;   ./modus --script test/concurrent-interp-probe.lisp
;;;;   NT=<threads> K=<iterations> EAGER=1 RAW=1 ./modus --script …
;;;;
;;;; This is a REPRODUCER, not a pass/fail test, and it is deliberately five
;;;; seconds rather than the twenty test/hosted-sb-thread.lisp takes to reach
;;;; the same fault.  It prints SURVIVED or dies; read the RATE over several
;;;; runs, never one result — the failure is a rate and bisecting it by shape
;;;; on single runs measures noise (that mistake is why this file exists).
;;;;
;;;; WHAT IS ESTABLISHED, all by rates of 6-10 runs on one binary:
;;;;
;;;;   two threads, nested OUT-OF-MODULE calls, enough iterations   FAULTS
;;;;   one thread, the same nesting, 50x the iterations (K=100000)  clean 5/5
;;;;   two threads, the same work via LABELS (calls stay IN module) clean 8/8
;;;;   two threads, one call level, K=20000                         clean 3/3
;;;;   two threads, pure arithmetic or pure consing, K=20000        clean 3/3
;;;;   (JIT-EAGER) first, so nothing is interpreted                 clean 3/3
;;;;
;;;; So it is CONCURRENCY, not depth and not volume, and it needs the
;;;; out-of-module call bridge — the path that funcalls a callee resolved
;;;; through *SYMBOL-FUNCTION-TABLE* and re-enters MVM-INTERPRET.
;;;;
;;;; RULED OUT, each with a verified control rather than a reading:
;;;;   * worker stack size — 8 MB changes nothing, and the record really does
;;;;     read back 8388608, so the knob was not a no-op;
;;;;   * the JIT — MODUS_NO_JIT=1 gives the same rate, and *JIT-PAGE-CACHE*
;;;;     is empty in this workload, so %JIT-ENTRY-FOR's unlocked cache and
;;;;     its shared-page const re-patch are not what fires;
;;;;   * *MVM-LAST-MV* and *NLX-STATE-SERIAL*, the interpreter's two shared
;;;;     globals — bound per-thread by hand, 2 of 10 either way;
;;;;   * sharing the callees — two threads running DISJOINT function sets
;;;;     fault just the same.
;;;;
;;;; AND ONE THING THAT DOES MOVE IT, which is where to look next: spawning
;;;; through SB-THREAD:MAKE-THREAD faults where %MAKE-NATIVE-THREAD with the
;;;; identical body does not (0 of 6 against 6 of 6), and shim-spawn with a
;;;; raw join still faults, so it is the shim's SPAWN and not its join.  The
;;;; pieces of that spawn tested individually on raw threads — a main-region
;;;; box written by the worker, a CLOS instance dynamically bound to a
;;;; special, the registry push, MAKE-INSTANCE on main while a worker runs —
;;;; are each clean.  Something in the combination is not.
(defun envi (name dflt)
  (handler-case (let ((s (sb-ext:posix-getenv name))) (if s (parse-integer s) dflt))
    (t (c) dflt)))
(defun envf (name)
  (handler-case (let ((s (sb-ext:posix-getenv name))) (and s (string= s "1"))) (t (c) nil)))

(defvar *nt* (envi "NT" 2))
(defvar *k*  (envi "K" 2000))

(format t "~&threads-up = ~s  NT=~D K=~D EAGER=~s RAW=~s~%"
        (%sb-threads-up) *nt* *k* (envf "EAGER") (envf "RAW"))

;; Defined AFTER the surface is armed, so they stay BYTECODE — that is the
;; point.  Three separate DEFUNs, so WORK -> MID -> LEAF crosses the module
;; boundary twice; LABELS inside one DEFUN would keep both calls in-module and
;; is the control that passes.
(defun ci-leaf (n) (+ n 1))
(defun ci-mid  (n) (ci-leaf (ci-leaf n)))
(defun ci-work (k)
  (let ((i 0) (s 0))
    (loop (when (>= i k) (return s))
      (setq s (ci-mid s))
      (setq i (+ i 1)))))

(when (envf "EAGER") (format t "  jit-eager = ~s~%" (jit-eager)))

(if (envf "RAW")
    ;; %MAKE-NATIVE-THREAD: the arm that does NOT fault.
    (let ((hs nil) (i 0))
      (loop (when (>= i *nt*) (return 0))
        (setq hs (cons (%make-native-thread (lambda () (ci-work *k*) 0)) hs))
        (setq i (+ i 1)))
      (dolist (h hs) (%join-native-thread h 400000000)))
    ;; SB-THREAD:MAKE-THREAD: the arm that does.
    (let ((ths nil) (i 0))
      (loop (when (>= i *nt*) (return 0))
        (setq ths (cons (sb-thread:make-thread (lambda () (ci-work *k*) 0)) ths))
        (setq i (+ i 1)))
      (dolist (th ths) (sb-thread:join-thread th))))

(format t "SURVIVED~%")
