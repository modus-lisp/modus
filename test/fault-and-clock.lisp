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
;;;; POSITIVE integer on every hosted port -- x64 used to answer 0 ("unknown")
;;;; for its TSC and now calibrates it.

(defvar *fc-fail* 0)
(defun fc (name got want)
  (unless (equal got want)
    (setq *fc-fail* (+ *fc-fail* 1))
    (format t "~&FAIL ~A: got ~S want ~S~%" name got want)))

(dotimes (i 3)
  (fc (list 'fault i) (handler-case (mem-ref (+ 16 (* 16 i)) :u32) (error () :caught)) :caught))
(fc 'still-alive (list 1 2 (+ 3 4)) '(1 2 7))

;; The caught fault IS a condition of its own: the stub cannot allocate one,
;; so the handler-case dispatch builds it (%TAKE-PENDING-FAULT).  Before, the
;; clauses matched whatever was signalled LAST -- here the SIMPLE-ERROR.
;; By NAME: the runtime's type symbol is homed in its build-host package,
;; so it is not EQ to the MEMORY-FAULT-ERROR this file reads into CL-USER
;; (handler-case clauses naming it match regardless).
(fc 'fault-type (handler-case (mem-ref 48 :u32) (error (c) (symbol-name (type-of c))))
    "MEMORY-FAULT-ERROR")
(fc 'not-stale (progn (ignore-errors (error "earlier"))
                      (handler-case (mem-ref 64 :u32)
                        (simple-error () :stale) (type-error () :fault)))
    :fault)
(fc 'real-error-after (handler-case (error "real") (simple-error (c) (simple-condition-format-control c)))
    "real")

(let ((t0 (rdtsc)) (x 0))
  (dotimes (i 200000) (setq x (+ x i)))
  (fc 'counter-integer (integerp t0) t)
  (fc 'counter-advances (> (rdtsc) t0) t))
(fc 'rate-known (and (integerp (cntfrq)) (> (cntfrq) 0)) t)

(format t "~&fault-and-clock: ~D failure(s) (rate ~S)~%" *fc-fail* (cntfrq))
(when (> *fc-fail* 0) (error "fault-and-clock: ~D failure(s)" *fc-fail*))
