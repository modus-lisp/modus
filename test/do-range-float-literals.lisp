;;; DO counters typed from their end test, and single-float literals in double expressions (compiler.lisp
;;; %DO-COUNTER-RANGE, %FPD-WIDENS-P).  A DO counter stepped by (1+ I) from an integer literal, with (> I K) its
;;; end test or a top-level OR disjunct of it, is an (INTEGER LO K+1); a single-float literal beside a double is
;;; that double (CL contagion).  Either missing made cl-bench MANDELBROT/DFLOAT box its doubles every iteration
;;; (14.7 ns an iteration, SBCL 2).  The counter's type is trusted, so the cases it must NOT apply to are here
;;; too.  Values are checked on SBCL as well.  Run: ./modus --script THIS
(defpackage :do-range-test (:use :cl))
(in-package :do-range-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error () :error))))
     (incf *checks*)
     (unless (equalp got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))
(defvar *forms* (make-hash-table))
(defmacro defk (name args &body body)
  `(progn (setf (gethash ',name *forms*) '(lambda ,args ,@body)) (defun ,name ,args ,@body)))
(defun count-of (name what)
  #+modus (let ((s (with-output-to-string (*standard-output*) (disassemble (gethash name *forms*)))) (n 0) (p 0))
            (loop (let ((i (search what s :start2 p))) (unless i (return n)) (incf n) (setq p (1+ i)))))
  #-modus (progn name what 0))

;; the benchmark's kernel: counter from its OR test, (> double 4.0)
(defk mset (c1 c2)
  (declare (type double-float c1 c2))
  (let ((z1 0.0d0) (z2 0.0d0) (aux 0.0d0))
    (declare (double-float z1 z2 aux))
    (do ((iter 0 (1+ iter)))
        ((or (> (abs (+ (* z1 z1) (* z2 z2))) 4.0) (> iter 300)) iter)
      (setq aux z1 z1 (+ (* z1 z1) (- (* z2 z2)) c1) z2 (+ (* 2.0d0 z2 aux) c2)))))
(check "mset values" (list (mset 0d0 0d0) (mset 1d0 1d0) (mset -0.75d0 0.1d0) (mset 0.0001d0 0.36d0))
       (list 301 2 33 301))
(check "mset doubles stay unboxed" (count-of 'mset "FPD-BOX") 0)

;; the counter's final value is the test's bound plus one, or the init when that is past it
(defk upto (k) (do ((i 0 (1+ i))) ((> i 10) (list i k))))
(check "final value" (upto 1) (list 11 1))
(defk past () (do ((i 50 (1+ i))) ((> i 10) i)))
(check "init past the bound" (past) 50)
(defk ge () (let ((n 0)) (do ((i 3 (+ i 1))) ((>= i 8) (list i n)) (incf n))))
(check ">= bound" (ge) (list 8 5))
(defk flipped () (do ((i 0 (+ 1 i))) ((< 6 i) i)))
(check "(< K I)" (flipped) 7)

;; NOT a bounded counter: each of these must keep counting past any inferred range
(defk assigned () (do ((i 0 (1+ i))) ((> i 10) i) (when (= i 5) (setq i (expt 2 70)))))
(check "assigned in the body" (assigned) (1+ (expt 2 70)))
(defk incfd () (do ((i 0 (1+ i))) ((> i 10) i) (when (= i 2) (incf i (expt 2 70)))))
(check "INCF in the body" (incfd) (+ 3 (expt 2 70)))
(defk closure () (let ((f nil)) (do ((i 0 (1+ i))) ((> i 10) i) (unless f (setq f (lambda () (setq i (expt 2 70))))) (when (= i 3) (funcall f)))))
(check "assigned by a closure" (closure) (1+ (expt 2 70)))
(defk other-step () (do* ((i 0 (1+ i)) (j 0 (setq i (+ i (expt 2 70))))) ((> i 10) (> i 100))))
(check "assigned by another variable's step (DO*)" (other-step) t)
(defk and-test () (let ((n 0)) (do ((i 0 (1+ i))) ((and (> i 3) (> n 100)) i) (incf n))))
(check "AND test bounds nothing" (and-test) 101)
(defvar *sp* 0)
(defun bump () (setq *sp* (expt 2 70)))
(defk special-counter () (do ((*sp* 0 (1+ *sp*))) ((> *sp* 10) *sp*) (when (= *sp* 4) (bump))))
(check "special counter" (special-counter) (1+ (expt 2 70)))

;; single-float literals beside doubles are those doubles; without a double they stay single
(defk mixed (d) (declare (double-float d)) (list (> d 4.0) (< d 0.1) (* 2.0 d) (+ d 0.1) (- 1.5 d) (/ d 4.0)))
(check "mixed 4.5" (mixed 4.5d0) (list t nil 9.0d0 (+ 4.5d0 (float 0.1 1d0)) -3.0d0 1.125d0))
(check "mixed 0.1" (mixed 0.1d0) (list nil t 0.2d0 (+ 0.1d0 (float 0.1 1d0)) 1.4d0 0.025d0))
(defk singles () (list (+ 1.0 2.0) (* 2.0 3.0)))
(check "singles stay single" (mapcar #'type-of (singles)) (list 'single-float 'single-float))

(format t "do-range-float-literals: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
