;;;; md5-substrate.lisp -- the three modus bugs that kept the md5 library wrong.
;;;;
;;;; Run: ./modus --script test/md5-substrate.lisp   (x64, i386)
;;;;
;;;; md5 (quicklisp) failed with UNDEFINED-FUNCTION, then UNBOUND-VARIABLE,
;;;; then -- on i386 only -- silently returned wrong digests.  Each layer is a
;;;; general bug; EXPECTED values are SBCL's.
;;;;  1. SETF of a MACRO place (CLHS 5.1.2.7) fell to an undefined SET-<macro>
;;;;     (and must NOT expand the compiler's open-coded CL functions such as
;;;;     CADDR; evaluated 3-4 letter c*r places now decompose to set-car/cdr).
;;;;  2. The inliner renamed parameters by blind tree substitution, so an
;;;;     inline function with a parameter named FROM lost LOOP's FROM keyword.
;;;;  3. Float <-> bignum: %fl took a bignum's pointer (i386 pi = 0.9999999),
;;;;     %float-to-int wrapped past the fixnum range ((truncate 3.5d9) => 0),
;;;;     and (- float bignum) went to bignum-sub (killed the i386 image).

(defvar *ms-fail* 0)
(defvar *ms-n* 0)
(defun ms (name got want)
  (setq *ms-n* (+ *ms-n* 1))
  (unless (equalp got want)
    (setq *ms-fail* (+ *ms-fail* 1))
    (format t "~&FAIL ~A: got ~S want ~S~%" name got want)))

;;; 1. macro places
(defmacro ms-first (x) `(car ,x))
(defmacro ms-second (x) `(ms-first (cdr ,x)))
(defmacro ms-vref (v i) `(aref ,v ,i))
(defun ms-setf-1 () (let ((c (list 1 2))) (setf (ms-first c) 9) c))
(ms "setf macro place" (ms-setf-1) '(9 2))
(ms "setf nested macro place" (let ((c (list 1 2))) (setf (ms-second c) 8) c) '(1 8))
(ms "incf macro place" (let ((v (vector 1 2 3))) (incf (ms-vref v 1) 10) v) #(1 12 3))

(ms "setf cdddr (evaluated)" (eval '(let ((x (list 1 2 3 4)) (i 0)) (setf (cdddr (progn (incf i) x)) 9) (list x i))) '((1 2 3 . 9) 1))
(ms "setf caddr (evaluated)" (eval '(let ((x (list 1 2 3))) (setf (caddr x) 9) x)) '(1 2 9))

;;; 2. inline expansion leaves the body alone
(declaim (inline ms-inl))
(defun ms-inl (from a) (list from a 'from 'a (loop for i from 1 below 3 collect i)))
(defun ms-inl-caller (a from) (ms-inl (list a from) (let ((a 10)) a)))
(ms "inline keeps LOOP FROM and quoted data" (ms-inl-caller 1 2) '((1 2) 10 from a (1 2)))

;;; 3. float <-> integer beyond the fixnum range
(ms "pi" (* 4 (atan 1d0)) 3.141592653589793d0)
(ms "sin 1" (sin 1d0) 0.8414709848078965d0)
(ms "md5 T[1]" (truncate (* 4294967296 (abs (sin (float 1 0d0))))) 3614090360)
(ms "md5 T[64]" (truncate (* 4294967296 (abs (sin (float 64 0d0))))) 3951481745)
(ms "truncate 3.5d9" (multiple-value-list (truncate 3.5d9)) '(3500000000 0d0))
(ms "truncate 1.5d9" (truncate 1.5d9) 1500000000)
(ms "truncate -1d20" (multiple-value-list (truncate -1d20)) '(-100000000000000000000 0d0))
(ms "floor -3.5d9" (floor -3.5d9) -3500000000)
(ms "ceiling 3.5d9" (ceiling 3.5d9) 3500000000)
(ms "round -4.5d9" (round -4.5d9) -4500000000)
(ms "round 2.5d0" (multiple-value-list (round 2.5d0)) '(2 0.5d0))
(ms "floor -1.5d0" (multiple-value-list (floor -1.5d0)) '(-2 0.5d0))
(ms "float - bignum" (- 3.5d9 3500000000) 0d0)
(ms "bignum + ratio" (+ (expt 10 20) 1/3) 300000000000000000001/3)
(ms "ratio - bignum" (- 1/3 (expt 10 20)) -299999999999999999999/3)

(format t "~&md5-substrate: ~D of ~D passed~%" (- *ms-n* *ms-fail*) *ms-n*)
(when (> *ms-fail* 0) (error "md5-substrate: ~D failure(s)" *ms-fail*))
