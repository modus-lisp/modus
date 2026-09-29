;;;; fault-and-clock.lisp -- hardware faults become conditions; the clock runs.
;;;;
;;;; Run: ./modus --script test/fault-and-clock.lisp
;;;;
;;;; FAULTS.  A read of an unmapped address is a SIGSEGV; the handler TRAP
;;;; #x0520 installs longjmps into the innermost handler-case.  i386 had it as
;;;; a named NOP, so there the process died.  Several faults in a row: the
;;;; handler never returns, so without SA_NODEFER the signal stays blocked and
;;;; the SECOND fault kills.
;;;;
;;;; CLOCK.  (rdtsc)/(cntfrq) under the interpreter -- which is how --script
;;;; runs these forms -- used to be a fake 0/0.  Now the port's own trap: the
;;;; counter must be an integer that ADVANCES across real work, and the rate a
;;;; non-negative integer (0 = the platform cannot say, x64's TSC).

(defvar *fc-fail* 0)
(defun fc (name got want)
  (unless (equal got want)
    (setq *fc-fail* (+ *fc-fail* 1))
    (format t "~&FAIL ~A: got ~S want ~S~%" name got want)))

(dotimes (i 3)
  (fc (list 'fault i) (handler-case (mem-ref (+ 16 (* 16 i)) :u32) (error () :caught)) :caught))
(fc 'still-alive (list 1 2 (+ 3 4)) '(1 2 7))

(let ((t0 (rdtsc)) (x 0))
  (dotimes (i 200000) (setq x (+ x i)))
  (fc 'counter-integer (integerp t0) t)
  (fc 'counter-advances (> (rdtsc) t0) t))
(fc 'rate-integer (and (integerp (cntfrq)) (>= (cntfrq) 0)) t)

(format t "~&fault-and-clock: ~D failure(s) (rate ~S)~%" *fc-fail* (cntfrq))
(when (> *fc-fail* 0) (error "fault-and-clock: ~D failure(s)" *fc-fail*))
