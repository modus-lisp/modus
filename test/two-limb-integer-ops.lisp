;;;; test/two-limb-integer-ops.lisp -- LOGAND LOGIOR LOGXOR + - ASH LDB on
;;;; integers around the fixnum / two-limb / big-bignum boundaries agree with
;;;; SBCL, interpreted and compiled.
;;;;
;;;;   ./modus --script test/two-limb-integer-ops.lisp
;;;;
;;;; The bitwise ops, ASH and the small-bignum add have a two-limb fast path
;;;; (mvm/cl-eval.lisp %GENERIC-BITWISE, %TWO-LIMB-SHL/-SAR, BIGNUM-ADD): a
;;;; u64 logxor went from ~500 bytes and ~1 us through limb lists to one
;;;; result object.  The fast path has to know where two limbs end, so the
;;;; operands here straddle every edge: +-2^62, +-2^63, 2^64, +-2^123, +-2^124
;;;; (a high limb of -2^62 is NOT representable: (logxor 2^123 -2^123) and
;;;; (+ -2^123 -2^123) need a third limb -- the latter printed garbage before),
;;;; plus 150 pseudo-random integers of 3..100 bits.  Each line is a checksum
;;;; over every pair (or every shift count); the expected values are SBCL's
;;;; output for this same file.

(defvar *seed* 12345)
(defun nxt () (setq *seed* (mod (+ (* *seed* 6364136223846793005) 1442695040888963407) 18446744073709551616)))
(defun rint ()
  (let* ((w (mod (nxt) 7))
         (bits (nth w '(3 30 61 62 63 64 100)))
         (v (mod (+ (* (nxt) 18446744073709551616) (nxt)) (expt 2 bits)))
         (v (if (oddp (nxt)) (- v) v)))
    (+ v (nth (mod (nxt) 5) '(0 1 -1 2 -2)))))
(defvar *vals* (append (list 0 1 -1 (- (expt 2 62) 1) (- (expt 2 62)) (- 1 (expt 2 62)) (- -1 (expt 2 62))
                             (expt 2 62) (- (expt 2 62)) (1- (expt 2 62)) (- 1 (expt 2 62)) (expt 2 63) (- (expt 2 63)) (1- (expt 2 64)) (expt 2 64)
                             (expt 2 123) (- (expt 2 123)) (1- (expt 2 123)) (expt 2 124) (- (expt 2 124)))
                       (loop repeat 150 collect (rint))))
(defun sxhash-free (x) (if (minusp x) (+ (* 2 (- x)) 1) (* 2 x)))
(defun h (x) (mod (sxhash-free x) 1000000007))
(defvar *got* nil)
(defun note (name acc) (push (format nil "~a ~d" name acc) *got*))
(dolist (op (list (cons "logand" #'logand) (cons "logior" #'logior) (cons "logxor" #'logxor) (cons "+" #'+) (cons "-" #'-)))
  (let ((acc 0))
    (dolist (a *vals*) (dolist (b *vals*) (setq acc (mod (+ (* acc 31) (h (funcall (cdr op) a b))) 1000000007))))
    (note (car op) acc)))
(dolist (k '(1 2 13 31 32 51 60 61 62 63 64 70 100 123 124 130))
  (let ((acc 0))
    (dolist (a *vals*) (setq acc (mod (+ (* acc 31) (h (ash a k)) (h (ash a (- k)))) 1000000007)))
    (note (format nil "ash ~d" k) acc)))
(let ((acc 0)) (dolist (a *vals*) (setq acc (mod (+ (* acc 31) (h (ldb (byte 64 0) a)) (h (ldb (byte 62 0) a)) (h (ldb (byte 32 13) a))) 1000000007)))
  (note "ldb" acc))
(let ((f (compile nil '(lambda (a b) (list (logxor a b) (logand a b) (logior a b) (+ a b) (ash a 13) (ash a -13) (ldb (byte 64 0) (+ a b))))))
      (acc 0))
  (dolist (a *vals*) (dolist (b *vals*) (dolist (r (funcall f a b)) (setq acc (mod (+ (* acc 31) (h r)) 1000000007)))))
  (note "compiled" acc))

(defparameter *want*   ; SBCL 2.x, this file
  '("logand 616925822" "logior 750211877" "logxor 680165373" "+ 319365807" "- 267224044"
    "ash 1 593912313" "ash 2 470005040" "ash 13 692088244" "ash 31 798490771" "ash 32 167942484"
    "ash 51 55919832" "ash 60 402333870" "ash 61 273715566" "ash 62 669670031" "ash 63 845772817"
    "ash 64 70235791" "ash 70 521924246" "ash 100 366240007" "ash 123 203597886" "ash 124 460487287"
    "ash 130 442768866" "ldb 485079168" "compiled 576204288"))
(let ((fail 0))
  (loop for g in (reverse *got*) for w in *want*
        unless (string= g w) do (incf fail) (format t "~&FAIL got ~a want ~a~%" g w))
  (format t "~&two-limb-integer-ops: ~d passed, ~d failed~%" (- (length *want*) fail) fail)
  (format t "~&~:[FAIL~;PASS~]~%" (zerop fail)))
