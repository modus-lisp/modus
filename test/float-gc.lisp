;;;; float-gc.lisp -- boxed doubles survive real collections.
;;;;
;;;; Run: ./modus --script test/float-gc.lisp
;;;;
;;;; aarch64's FP ops (FADD/FSUB/FMUL/FDIV, ITOF) boxed their result without
;;;; setting the object-start bit, so the collector refused every pointer to
;;;; one as "not an object start" and left it aimed at from-space.  EVERY
;;;; double reached the heap that way -- the reader's 1.5d0 included -- and
;;;; after a collection read back as whatever was copied over it: a FUNCTION,
;;;; a string.  Singles, bignums and ratios were fine (their allocators mark).
;;;; A double held by a global, inside a list, and as a runtime defun's
;;;; literal, each checked after forced collections.  %GC-EPOCH is the
;;;; positive control: a run that never collected proves nothing.

(defvar *fg-fail* 0)
(defun fg (name got want)
  (unless (eql got want)
    (setq *fg-fail* (+ *fg-fail* 1))
    (format t "~&FAIL ~A: got ~S want ~S~%" name got want)))

(defparameter *fg-d* (read-from-string "1.5d0"))
(defparameter *fg-sum* (+ *fg-d* 2.25d0))
(defparameter *fg-list* (list (* 3 *fg-d*) (float 7 1d0) (/ 1d0 4)))
(defun fg-literal () 6.125d0)

(defun fg-check (round)
  (fg (list round 'global) *fg-d* 1.5d0)
  (fg (list round 'sum) *fg-sum* 3.75d0)
  (fg (list round 'list) (car *fg-list*) 4.5d0)
  (fg (list round 'itof) (cadr *fg-list*) 7d0)
  (fg (list round 'div) (caddr *fg-list*) 0.25d0)
  (fg (list round 'literal) (fg-literal) 6.125d0)
  (fg (list round 'type) (type-of *fg-d*) 'double-float))

(defun fg-churn ()
  (let ((keep nil))
    (dotimes (i 400000) (push (make-list 4) keep) (when (> (length keep) 1000) (setq keep nil)))
    keep))

(fg-check 0)
(let ((e0 (%gc-epoch)))
  (dotimes (r 3) (fg-churn) (fg-check (+ r 1)))
  (format t "~&float-gc: collections ~D~%" (- (%gc-epoch) e0))
  (fg 'collected (> (%gc-epoch) e0) t))
(format t "~&float-gc: ~D failure(s)~%" *fg-fail*)
(when (> *fg-fail* 0) (error "float-gc: ~D failure(s)" *fg-fail*))
