;;;; h03-fault-recover.lisp -- expect 42
;;;;
;;; HOSTED ONLY: a hardware fault inside a HANDLER-CASE is recovered into it.
;;; The image installs its signal handlers (TRAP #x0520, what a CL image's boot
;;; does), reads an unmapped address inside a HANDLER-CASE, and the clause
;;; runs; then it does it again, so the stub must leave the signal unblocked
;;; (qemu-user kept it blocked despite SA_NODEFER on riscv64), and a third time
;;; after a collection-sized churn.  40 + one per recovery past the first;
;;; 1000 + recoveries if a read returns.
;;;
;;; not-on: 68k -- the 68k hosted image installs no signal handlers yet (TRAP #x0520)
;;;
;;; Run with:  scripts/hosted-ladder.py ppc64 test/hosted-rungs/h03-fault-recover.lisp
(defun churn (n)
  (let ((i 0) (junk nil))
    (loop (when (>= i n) (return junk))
      (setq junk (cons i junk))
      (when (= 0 (logand i 1023)) (setq junk nil))
      (setq i (+ i 1)))))
(defun bad () (handler-case (progn (mem-ref 64 :u32) 1000) (t () 1)))
(defun probe ()
  (%install-signal-handlers)
  (let ((a (bad)) (b (bad)))
    (churn 200000)
    (+ 39 a b (bad))))
