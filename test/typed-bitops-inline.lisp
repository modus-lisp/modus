;;;; test/typed-bitops-inline.lisp -- typed LOGAND/LOGIOR/LOGXOR, and integer-literal
;;;; arguments substituted into inline calls.
;;;;
;;;;   ./modus --script test/typed-bitops-inline.lisp      (and sbcl --script, the reference)
;;;;
;;;; Both are runtime-compiler changes in mvm/compiler.lisp:
;;;;   - %BITOP-TRUST: a LOGAND / LOGIOR / LOGXOR whose operands all have a known width is
;;;;     the plain op, with no tag test and no generic slow path;
;;;;   - %INLINE-SUBSTITUTE-CONSTANTS: an inline call's integer-literal arguments are
;;;;     substituted into a body made only of pure CL calls, and arithmetic left with
;;;;     literal operands is folded; any other body keeps its LET.
;;;;   - %INLINE-CALL-MASK-WIDTH: a call to an inline function ending in (LOGAND MASK ...)
;;;;     has the mask's width, so a sum of such calls is typed.
;;;; Every check runs the same computation through a NOTINLINE generic path too, so a
;;;; wrong typed answer cannot pass by agreeing with itself, and the whole file passes
;;;; under SBCL first.

(defvar *checks* 0)
(defvar *fails* 0)
(defun check (name got want)
  (incf *checks*)
  (unless (equal got want)
    (incf *fails*)
    (format t "  FAIL ~a: got ~s want ~s~%" name got want)))

;;; ---- typed bit operations ----------------------------------------------------------------
(defun u32-xor (a b) (declare (type (unsigned-byte 32) a b) (optimize (speed 3) (safety 0)))
  (logxor a b))
(defun u32-mix (b c d) (declare (type (unsigned-byte 32) b c d) (optimize (speed 3) (safety 0)))
  (logxor d (logand b (logxor c d))))
(defun u32-maj (b c d) (declare (type (unsigned-byte 32) b c d) (optimize (speed 3) (safety 0)))
  (logior (logand b c) (logand b d) (logand c d)))
(defun fix-and (a b) (declare (type fixnum a b)) (logand a b))
(defun fix-ior (a b) (declare (type fixnum a b)) (logior a b))
(defun untyped-and (a b) (logand a b))       ; bignums must still take the generic path

(declaim (notinline gen-and gen-ior gen-xor))
(defun gen-and (&rest xs) (apply #'logand xs))
(defun gen-ior (&rest xs) (apply #'logior xs))
(defun gen-xor (&rest xs) (apply #'logxor xs))

(let ((vals '(0 1 #x7fffffff #x80000000 #xffffffff #xdeadbeef #x12345678)))
  (dolist (a vals)
    (dolist (b vals)
      (check (list 'xor a b) (u32-xor a b) (gen-xor a b))
      (dolist (c '(0 #xffffffff #x5a5a5a5a))
        (check (list 'mix a b c) (u32-mix a b c) (gen-xor c (gen-and a (gen-xor b c))))
        (check (list 'maj a b c) (u32-maj a b c)
               (gen-ior (gen-and a b) (gen-and a c) (gen-and b c)))))))
(dolist (a (list most-positive-fixnum most-negative-fixnum -1 -12345 0 98765))
  (dolist (b (list most-positive-fixnum most-negative-fixnum -1 -7 0 255))
    (check (list 'fand a b) (fix-and a b) (gen-and a b))
    (check (list 'fior a b) (fix-ior a b) (gen-ior a b))))
(let ((big (ash 1 100)) (big2 (- (ash 3 90) 1)))
  (check 'bignum-and (untyped-and big2 (ash big2 -3)) (gen-and big2 (ash big2 -3)))
  (check 'bignum-and-fix (untyped-and (+ big 12345) #xffff) 12345))

;; the fixnum range's own bounds are fixnums, and the next integer past each is not --
;; modus's MOST-NEGATIVE-FIXNUM was -2^62, a bignum here, so a declared FIXNUM could hold
;; a pointer and the trust above had to stop one bit short
(check 'mnf-is-fixnum (typep most-negative-fixnum 'fixnum) t)
(check 'mpf-is-fixnum (typep most-positive-fixnum 'fixnum) t)
(check 'below-mnf (typep (1- most-negative-fixnum) 'fixnum) nil)
(check 'above-mpf (typep (1+ most-positive-fixnum) 'fixnum) nil)
(check 'fixnum-subtype (and (subtypep 'fixnum `(integer ,most-negative-fixnum ,most-positive-fixnum))
                            (subtypep `(integer ,most-negative-fixnum ,most-positive-fixnum) 'fixnum)) t)

;;; ---- inline calls with integer-literal arguments -----------------------------------------
(declaim (inline rol32 add3 shifty uses-let loopy assigns))
(defun rol32 (x n)
  (declare (type (unsigned-byte 32) x) (type (integer 0 31) n) (optimize (speed 3) (safety 0)))
  (logand #xffffffff (logior (ash x n) (ash x (- n 32)))))
(defun add3 (a b c) (+ a b c))
(defun shifty (x k) (if (plusp k) (ash x k) (ash x (- k))))
(defun uses-let (x n) (let ((m (* n 2))) (+ x m)))             ; a LET: not substituted
(defun loopy (from n) (loop for i from 0 below n sum (+ i from))) ; FROM is also a LOOP keyword
(defun assigns (x n) (setq n (+ n 1)) (* x n))                  ; N is assigned: not substituted

(declaim (notinline rol32-ref))
(defun rol32-ref (x n) (logand #xffffffff (logior (ash x n) (ash x (- n 32)))))

(defun use-rol (a) (declare (type (unsigned-byte 32) a)) (list (rol32 a 1) (rol32 a 5) (rol32 a 30)))
(defun use-sum (a b c d e w)
  ;; the SHA-1 round's shape: a typed sum of an inline call and typed operands
  (declare (type (unsigned-byte 32) a b c d e) (type (simple-array (unsigned-byte 32) (4)) w))
  (logand #xffffffff (+ (rol32 a 5) (logxor b c d) e #x5a827999 (aref w 2))))

(dolist (a '(0 1 #x80000000 #xffffffff #xdeadbeef #x12345678))
  (check (list 'rol a) (use-rol a) (list (rol32-ref a 1) (rol32-ref a 5) (rol32-ref a 30)))
  (let ((w (make-array 4 :element-type '(unsigned-byte 32) :initial-contents '(1 2 #xfedcba98 4))))
    (check (list 'sum a) (use-sum a #x11111111 #x22222222 #x44444444 #x88888888 w)
           (logand #xffffffff (+ (rol32-ref a 5) (logxor #x11111111 #x22222222 #x44444444)
                                 #x88888888 #x5a827999 #xfedcba98)))))
(check 'add3-consts (add3 1 2 3) 6)
(check 'add3-mixed (let ((v 10)) (add3 v 2 3)) 15)
(check 'shifty-left (shifty 3 4) 48)
(check 'shifty-right (shifty 48 -4) 768)        ; (ash 48 4): k=-4 so (- k)=4
(check 'uses-let (uses-let 5 3) 11)
(check 'loopy (loopy 10 4) 46)                 ; 10+11+12+13
(check 'assigns (assigns 7 2) 21)
;; evaluation order of the non-literal arguments is kept
(let ((log nil))
  (check 'order (add3 (progn (push 1 log) 1) 2 (progn (push 3 log) 3)) 6)
  (check 'order-log (reverse log) '(1 3)))

;;; ---- a package that shadows + must not be folded as CL's + -------------------------------
(defpackage :tbi-shadow (:use :cl) (:shadow #:+))
(in-package :tbi-shadow)
(defun + (a b) (cl:* a b))                      ; "+" that multiplies
(declaim (inline weird))
(defun weird (x n) (+ x n))
(defun use-weird () (weird 6 7))
(in-package :cl-user)
(check 'shadowed-plus (tbi-shadow::use-weird) 42)

(format t "~&typed-bitops-inline: ~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
