;;;; test/hot-jit-worker.lisp -- a runtime DEFUN that only a WORKER thread calls
;;;; still goes native.
;;;;
;;;;   ./modus --script test/hot-jit-worker.lisp
;;;;
;;;; Only the main thread may build (a build records into region-0 tables), so
;;;; %JIT-HOT-PROMOTE answers :RETRY on a worker -- and a DEFUN called only by
;;;; workers stayed interpreted forever (a Pi Zero's RFB sender: 6 s at a time
;;;; in the interpreter).  Now the worker marks the trampoline WANTED and the
;;;; main thread builds it between two toplevel forms (and, on bare metal,
;;;; whenever it idles).  The CONTROL arm, in the same binary, disables the flag
;;;; the worker raises: the same calls must then leave the DEFUN interpreted.

(setq *jit-script-defs* nil)          ; the DEFUNs here must start interpreted

(defvar *checks* 0)
(defvar *fails* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (unless (equal got want)
    (setq *fails* (+ *fails* 1))
    (format t "~&FAIL ~a~%   got  ~s~%   want ~s~%" name got want)))

(defun native-p (nm)
  (if (or (%jit-fn-native-p nm)
          (%jit-fn-native-p (concatenate 'string "COMMON-LISP-USER::" nm)))
      t nil))

(defun on-worker (fn)
  (sb-thread:join-thread (sb-thread:make-thread fn)))

;;; CONTROL: the flag is not a cons, so a worker's want is dropped.
(setq *jit-want-flag* :off)
(defun ctl-fn (x) (values (+ x 1) (* x 2)))
(chk "control: worker sees both values"
     (on-worker (lambda () (let ((r nil)) (dotimes (i 200) (setq r (multiple-value-list (ctl-fn i)))) r)))
     (list 200 398))
(chk "control: between forms" t t)
(defvar *workers-build* (native-p "CTL-FN"))
(when *workers-build*
  ;; Hosted x64 without per-CPU storage: every thread answers "main" to
  ;; %MVM-ON-MAIN-THREAD-P, so a worker promotes for itself and there is
  ;; nothing to fix.  The test still runs, but cannot fail the mechanism.
  (format t "~&hot-jit-worker: workers build for themselves on this target -- the drain is not exercised~%"))
(unless *workers-build*
  (chk "control: worker-only DEFUN stays interpreted" (native-p "CTL-FN") nil))

;;; THE FIX: the flag back, a fresh DEFUN, the same calls.
(setq *jit-want-flag* (list nil))
(defvar *built-before* (or *jit-want-built* 0))
(defun wk-fn (x) (values (+ x 1) (* x 2)))
(chk "worker sees both values"
     (on-worker (lambda () (let ((r nil)) (dotimes (i 200) (setq r (multiple-value-list (wk-fn i)))) r)))
     (list 200 398))
(chk "the worker raised the flag, main drained it at this form" t t)
(chk "worker-only DEFUN is native" (native-p "WK-FN") t)
(unless *workers-build*
  (chk "the drain counted it" (> (or *jit-want-built* 0) *built-before*) t))
(chk "native answers on the worker, both values"
     (on-worker (lambda () (multiple-value-list (wk-fn 41)))) (list 42 82))
(chk "and on main" (multiple-value-list (wk-fn 9)) (list 10 18))

;;; a DEFUN main never calls, reached through a callee chain on the worker
(defun wk-leaf (x) (* x 3))
(defun wk-mid (x) (+ (wk-leaf x) 1))
(chk "chain on worker" (on-worker (lambda () (let ((s 0)) (dotimes (i 100) (setq s (+ s (wk-mid i)))) s)))
     (+ 100 (* 3 (/ (* 99 100) 2))))
(chk "chain: between forms" t t)
(chk "chain: caller native" (native-p "WK-MID") t)
(chk "chain: callee native" (native-p "WK-LEAF") t)

(format t "~&hot-jit-worker: ~d checks, ~d failed~%" *checks* *fails*)
(sys-exit (if (zerop *fails*) 0 1))
