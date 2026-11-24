;;; Float and vector scopes in run-time compiled code, and LOG / f32 REPLACE, FILL and COPY-SEQ
;;; (compiler.lisp %FPD-INT-CONVERSION-VAR, %MOD-POW2-REWRITE, %LET-FP-KINDS' SEQUENTIAL, %FP-SCOPE-CALL-FREE-P;
;;; cl-types.lisp %LOG-KERNEL-D; cl-eval.lisp %REPLACE-POS).  Each was a CALL inside a scope whose floats should
;;; have stayed in registers -- found on mill (stave's Zipformer) against SBCL.  The values are checked on SBCL
;;; too; on modus each kernel's disassembly is also checked to have no CALL.  Run: ./modus --script THIS
(defpackage :float-scope-test (:use :cl))
(in-package :float-scope-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (equal got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))

(defvar *forms* (make-hash-table))
(defmacro defk (name args &body body)
  "DEFUN, keeping the lambda: modus's DISASSEMBLE has bytecode for a lambda form, not a DEFUN."
  `(progn (setf (gethash ',name *forms*) '(lambda ,args ,@body)) (defun ,name ,args ,@body)))
(defun %calls (form)
  #+modus (let ((s (with-output-to-string (*standard-output*) (disassemble form))) (n 0) (p 0))
            (unless (search "SET-NARGS" s) (return-from %calls :no-bytecode))
            (loop (let ((i (search "CALL 0" s :start2 p))) (unless i (return n)) (incf n) (setq p (1+ i)))))
  #-modus (progn form 0))
(defun calls-in (name)
  "How many CALLs NAME's body has (modus; 0 elsewhere): its code's, less those of an empty function with the
   same arguments -- the prologue's wrong-argument-count error is a call."
  (let* ((form (gethash name *forms*)) (n (%calls form)) (base (%calls `(lambda ,(cadr form) nil))))
    (if (and (integerp n) (integerp base)) (- n base) (list n base))))

(defconstant +lanes+ 4)

;; (FLOAT K 1D0) of a fixnum in a double tree: widened in register, not a call to FLOAT
(defk scale (k x)
  (declare (type fixnum k) (type double-float x) (optimize (speed 3) (safety 0)))
  (let ((a 0d0)) (declare (type double-float a))
    (setq a (* (float k 1d0) x))
    (+ a (* (float k 1d0) 0.5d0))))
(check "fixnum->double" (scale 3 2d0) 7.5d0)
(check "fixnum->double 2^53+1" (scale (+ (expt 2 53) 1) 1d0) (* 1.5d0 (float (+ (expt 2 53) 1) 1d0)))
(check "fixnum->double most-negative" (scale most-negative-fixnum 1d0) (* 1.5d0 (float most-negative-fixnum 1d0)))
(check "fixnum->double no call" (calls-in 'scale) 0)

;; (MOD X 2^k): LOGAND, for a literal and a constant divisor, negatives included
(defk m4 (x) (declare (type fixnum x) (optimize (speed 3) (safety 0))) (mod x 4))
(defk m4c (x) (declare (type fixnum x) (optimize (speed 3) (safety 0))) (mod x +lanes+))
(check "mod pow2"
       (loop for x in (list 0 1 3 4 5 -1 -3 -4 -5 -7 1000003 -1000003 most-positive-fixnum most-negative-fixnum)
             unless (and (= (m4 x) (mod x 4)) (= (m4c x) (mod x 4))) collect x)
       nil)
(check "mod pow2 no call" (+ (calls-in 'm4) (calls-in 'm4c)) 0)

;; LET*: an init sees the bindings before it with their types (X a single, then V from X)
(defk sub-broadcast (a b r)
  (declare (type (simple-array single-float (*)) a b r) (optimize (speed 3) (safety 0)))
  #+modus (let* ((x (aref b 0)) (v (modus-int::%f32v-broadcast x)))
            (setf (modus-int::%f32v-ref r 0) (modus-int::%f32v- (modus-int::%f32v-ref a 0) v)))
  #-modus (let ((x (aref b 0))) (dotimes (i 4) (setf (aref r i) (- (aref a i) x))))
  nil)
(let ((a (make-array 4 :element-type 'single-float :initial-contents '(1.0 2.0 3.0 4.0)))
      (b (make-array 1 :element-type 'single-float :initial-element 0.5))
      (r (make-array 4 :element-type 'single-float)))
  (sub-broadcast a b r)
  (check "let* broadcast" (coerce r 'list) '(0.5 1.5 2.5 3.5))
  (check "let* broadcast no call" (calls-in 'sub-broadcast) 0))

;; A closure sees its captured variables' declared types (%COMPILE-LAMBDA-1's CAPTURED-TYPES): a FLET over the
;; caller's declared arrays and floats was 35x SBCL.  The values, and the cases the types must not break: a
;; variable the closure assigns (no inferred width may be stamped on it) and one captured from a float register.
(defun flet-dot (a b n)
  (declare (type (simple-array single-float (*)) a b) (type fixnum n) (optimize (speed 3) (safety 0)))
  (let ((acc 0f0) (scale 2f0))
    (declare (type single-float acc scale))
    (flet ((tap (j) (declare (type fixnum j)) (* scale (aref a j) (aref b j))))
      (dotimes (j n) (setq acc (+ acc (tap j)))))
    acc))
(let ((a (make-array 5 :element-type 'single-float :initial-contents '(1.0 2.0 3.0 4.0 5.0)))
      (b (make-array 5 :element-type 'single-float :initial-element 0.5)))
  (check "flet over declared arrays" (flet-dot a b 5) 15.0))
(defun closure-assigns (k)
  (let ((x 1))                                        ; a width-provable init, assigned in the closure
    (flet ((grow () (setq x (* x k k k k k k k k k k k k))))
      (grow) (grow))
    x))
(check "closure assigns its capture" (closure-assigns (expt 2 10)) (expt 2 240))
(defun closure-float (x)
  (declare (type double-float x))
  (let ((y (* x 2d0))) (declare (type double-float y))
    (mapcar (lambda (k) (+ y (float k 1d0))) '(1 2 3))))
(check "closure over a double" (closure-float 1.5d0) '(4d0 5d0 6d0))

;; LOG of a single: SBCL's bits
(defun single-bits (f)
  #+sbcl (sb-kernel:single-float-bits f)
  #+modus (let ((a (make-array 1 :element-type 'single-float :initial-element f)))
            (let ((b (modus-int::%f32-bits-ref a 0))) (if (>= b #x80000000) (- b #x100000000) b))))
(check "log bits"
       (loop for (x want) in '((0.5 -1087278568) (1.5 1053792543) (3.0 1066180436) (7.25 1073582453)
                               (1.0e-30 -1031133259) (1.0e30 1116350389) (123456.79 1094423566)
                               (0.9999999 -1275068415))
             unless (= (single-bits (log x)) want) collect x)
       nil)
(check "log 1" (log 1.0) 0.0)
(check "log 2d0" (log 2d0) 0.6931471805599453d0)
(check "log -1 is complex" (complexp (log -1.0)) t)

;; REPLACE / FILL / COPY-SEQ of single-float vectors, overlapping too
(let* ((n 37) (v (make-array n :element-type 'single-float)))
  (dotimes (i n) (setf (aref v i) (float i 1f0)))
  (let ((c (copy-seq v)))
    (check "copy-seq f32" (and (typep c '(simple-array single-float (*))) (equalp c v)) t))
  (replace v v :start1 3 :start2 0 :end2 20)
  (check "replace f32 overlap" (loop for i from 3 below 23 always (= (aref v i) (float (- i 3) 1f0))) t)
  (fill v -0.0 :start 5 :end 30)
  (check "fill f32" (list (aref v 4) (aref v 5) (aref v 29) (aref v 30)) (list 1.0 -0.0 -0.0 30.0)))

(format t "float-scope-fixes: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
