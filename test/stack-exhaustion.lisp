;;;; stack-exhaustion.lisp — UNBOUNDED RECURSION SIGNALS, IT DOES NOT CRASH.
;;;;
;;;;   timeout 300 ./modus --script test/stack-exhaustion.lisp
;;;;
;;;; (defun f (n) (1+ (f n))) runs the native stack out.  On hosted AArch64
;;;; that ended the process with SIGSEGV (exit 139) on the main thread and on a
;;;; worker alike: the fault stub ran on the overflowed stack itself (no
;;;; alternate signal stack, no SA_ONSTACK), so the kernel had nowhere to push
;;;; the signal frame.  On x86-64 the main thread's handler did run, but deep
;;;; recursion came out as MEMORY-FAULT-ERROR about three runs in four: the
;;;; stub read [RSP] for its diagnostics, and after a prologue's SUB RSP that
;;;; is the faulting address, so the stub faulted again and the re-entry
;;;; compared the first fault's address with the ALTERNATE stack's RSP.  An
;;;; x86-64 worker had no alternate stack at all and died like AArch64's.
;;;; Measured on origin/main 0d8ff428: AArch64 main 139; x86-64 main
;;;; MEMORY-FAULT-ERROR (3 of 4 runs), x86-64 worker 139.
;;;;
;;;; Now (translate-aarch64 / translate-x64 trap #x0520, net/hosted-sync
;;;; %THR-SLOT-STACK / %THR-SIGALTSTACK): every thread has an alternate signal
;;;; stack, a worker's stack ends in a PROT_NONE guard, and a fault within
;;;; 64 KB of the stack pointer is recorded as #x0B0B, which %TAKE-PENDING-FAULT
;;;; makes a STACK-EXHAUSTED (a STORAGE-CONDITION) for the nearest armed frame.
;;;;
;;;; Checked on MAIN and on a WORKER:
;;;;   CAUGHT     unbounded recursion inside HANDLER-CASE receives
;;;;              STACK-EXHAUSTED, which is a STORAGE-CONDITION, and the
;;;;              recursion really went deep (the depth reached is recorded).
;;;;   CLEANUPS   UNWIND-PROTECTs between the handler and the overflow run,
;;;;              innermost first.
;;;;   SPECIALS   specials bound between the handler and the overflow are
;;;;              restored to their outer values.
;;;;   AGAIN      a second and a third overflow are caught the same way (the
;;;;              guard re-arms), and the depth each reaches is the same.
;;;;   FINITE     recursion 80% as deep as the overflow depth still returns
;;;;              its value; on a worker, 50000 frames too.
;;;;   NEIGHBOUR  a worker overflowing three times inside HANDLER-CASE leaves a
;;;;              concurrently running worker's computation, special and
;;;;              HANDLER-CASE alone.
;;;; and the process continues: the last line is printed by the main thread.
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS: the condition coming from somewhere
;;;; other than the native stack (a binding-stack or handler-frame overflow is
;;;; also a STORAGE-CONDITION) -- so the check is for STACK-EXHAUSTED by name,
;;;; the recursion holds no handler frame and no binding, and the depth reached
;;;; must exceed 5000 frames.

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))
(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A~%" name))))

(defvar *depth* 0)
(defvar *sp* :outer)
(defvar *cleanups* nil)

(defun unbounded (n)
  "No handler frame, no binding, no allocation: only native frames."
  (setq *depth* n)
  (1+ (unbounded (1+ n))))

(defun finite (n) (if (= n 0) 0 (1+ (finite (1- n)))))

(defun wrapped (k)
  "K UNWIND-PROTECTs, each binding *SP* to its level, then unbounded recursion."
  (if (= k 0)
      (unbounded 0)
      (let ((*sp* k))
        (unwind-protect (wrapped (- k 1))
          (setq *cleanups* (cons (list k *sp*) *cleanups*))))))

(defun overflow-once ()
  "One overflow under a HANDLER-CASE: (condition-name storage-p depth cleanups sp)."
  ;; *CLEANUPS* is BOUND here, not assigned: a worker may not store a list of
  ;; its own into the global value cell (the shared-store guard refuses it).
  (setq *depth* 0)
  (let ((*cleanups* nil))
    (let ((r (handler-case (progn (wrapped 5) :returned)
               (storage-condition (c)
                 (list (symbol-name (type-of c)) (typep c 'storage-condition)))
               (error (c) (list :error (symbol-name (type-of c)) (princ-to-string c))))))
      (list r *depth* (reverse *cleanups*) *sp*))))

(defun check-thread (where)
  "Every check for one thread; returns (passed . failed) for that thread."
  (let ((f0 *fail*) (c0 *checks*) (depths nil))
    (dotimes (round 3)
      (let* ((o (overflow-once))
             (r (first o)) (d (second o)) (cl (third o)) (sp (fourth o)))
        (setq depths (cons d depths))
        (chk (format nil "~A overflow ~D: caught as" where (+ round 1)) r '("STACK-EXHAUSTED" t))
        (chk-true (format nil "~A overflow ~D: went deep (~D frames)" where (+ round 1) d) (> d 5000))
        ;; Cleanup k runs while *SP* is still bound to k: innermost first.
        (chk (format nil "~A overflow ~D: cleanups, innermost first" where (+ round 1))
             cl '((1 1) (2 2) (3 3) (4 4) (5 5)))
        (chk (format nil "~A overflow ~D: special restored" where (+ round 1)) sp :outer)))
    (let ((d (car depths)))
      (chk-true (format nil "~A: all three overflows reach the same depth ~S" where (reverse depths))
                (every (lambda (x) (< (abs (- x d)) 64)) depths))
      (let ((n (floor (* d 8) 10)))
        (chk (format nil "~A: finite recursion ~D deep" where n) (finite n) n)))
    (cons (- *checks* c0 (- *fail* f0)) (- *fail* f0))))

(format t "~%=== MAIN ===~%")
(check-thread "main")

(format t "~%=== WORKER ===~%")
(let ((r (sb-thread:join-thread
          (sb-thread:make-thread
           (lambda ()
             (check-thread "worker")
             (list :finite-50000 (finite 50000) :sp *sp*))))))
  (chk "worker: 50000 frames, and its *SP*" r '(:finite-50000 50000 :sp :outer)))

(format t "~%=== A NEIGHBOUR WORKER IS LEFT ALONE ===~%")
(defvar *stop* nil)
(defvar *mine* 0)
(defun neighbour ()
  "Sums 0..9999 over and over under its own binding and HANDLER-CASE until
   *STOP*; returns (passes bad-sums handler-result *mine*)."
  (let ((*mine* 0) (passes 0) (bad 0))
    (let ((hc (handler-case
                  (loop (when *stop* (return :stopped))
                        (let ((s 0)) (dotimes (i 10000) (setq s (+ s i)))
                          (unless (= s 49995000) (setq bad (+ bad 1))))
                        (setq *mine* (+ *mine* 1))
                        (setq passes (+ passes 1)))
                (error (c) (list :error (symbol-name (type-of c)))))))
      (list passes bad hc (= *mine* passes)))))
(setq *stop* nil)
(let* ((nb (sb-thread:make-thread #'neighbour))
       (ov (sb-thread:make-thread
            (lambda () (let ((rs nil))
                         (dotimes (i 3) (setq rs (cons (car (car (overflow-once))) rs)))
                         rs)))))
  (chk "the overflowing worker caught three" (sb-thread:join-thread ov)
       '("STACK-EXHAUSTED" "STACK-EXHAUSTED" "STACK-EXHAUSTED"))
  (setq *stop* t)
  (let ((r (sb-thread:join-thread nb)))
    (format t "  neighbour: ~S~%" r)
    (chk-true "the neighbour ran" (> (first r) 0))
    (chk "the neighbour's sums" (second r) 0)
    (chk "the neighbour's HANDLER-CASE body ended normally" (third r) :stopped)
    (chk "the neighbour's special counted every pass" (fourth r) t))
  (chk "main's *MINE* is untouched" *mine* 0))

(format t "~%=== THE PROCESS CONTINUES ===~%")
(chk "main: one more overflow after all that" (car (car (overflow-once))) "STACK-EXHAUSTED")
(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "STACK EXHAUSTION: PASS" "STACK EXHAUSTION: FAIL"))
