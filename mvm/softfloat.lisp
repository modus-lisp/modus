;;;; softfloat.lisp -- IEEE-754 binary64 arithmetic in FIXNUM-ONLY Lisp
;;;;
;;;; For a CPU with no FPU (the 68LC040, the 68EC0x0): the compiler lowers
;;;; %FLOAT-ADD / -SUB / -MUL / -DIV, %FSQRT, %FROUND32, %FLOAT-FROM-INT,
;;;; %FLOAT-TO-INT, %F32-LOAD and %F32-STORE to the functions here instead of
;;;; to FPU opcodes when +TARGET-SOFT-FLOAT+ is 1 (see compiler.lisp).
;;;;
;;;; FIXNUMS ONLY, AND SMALL ONES.  The 30-bit tower's largest fixnum is 2^29-1,
;;;; and on 68k the checked integer ops WRAP rather than promote, so a bignum
;;;; intermediate would not even be slow -- it would be wrong.  So a significand
;;;; is a vector of TEN 13-BIT LIMBS (130 bits, index 0 least significant): a
;;;; limb product is 26 bits, and every sum below stays under 2^29.  Shifts by a
;;;; variable count go through a table of powers of two (a multiply or a FLOOR),
;;;; never through a variable ASH, which would reach BIGNUM-ASH.
;;;;
;;;; Results are correctly rounded (round-to-nearest-even), with subnormals,
;;;; signed zeros and infinities.  A NaN result is the default quiet NaN.
;;;;
;;;; A boxed float is four slots of 16-bit chunks, slot 0 = bits 63..48 of the
;;;; IEEE double (prelude.lisp, %FLOAT-HI32).

(defun %sf-pow2 (r)
  "2^R for 0 <= R <= 13."
  (cond ((= r 0) 1) ((= r 1) 2) ((= r 2) 4) ((= r 3) 8) ((= r 4) 16)
        ((= r 5) 32) ((= r 6) 64) ((= r 7) 128) ((= r 8) 256) ((= r 9) 512)
        ((= r 10) 1024) ((= r 11) 2048) ((= r 12) 4096) (t 8192)))

(defun %sfv () (make-array 10 :initial-element 0))

(defun %sfv-copy (a)
  (let ((b (%sfv)) (i 0))
    (loop (when (>= i 10) (return b))
      (setf (svref b i) (svref a i))
      (setq i (+ i 1)))))

(defun %sfv-from (x)
  "A vector holding the non-negative fixnum X (< 2^29)."
  (let ((v (%sfv)))
    (setf (svref v 0) (logand x 8191))
    (setf (svref v 1) (logand (ash x -13) 8191))
    (setf (svref v 2) (ash x -26))
    v))

(defun %sfv-zerop (a)
  (let ((i 0))
    (loop (when (>= i 10) (return t))
      (unless (= (svref a i) 0) (return nil))
      (setq i (+ i 1)))))

(defun %sfv-ilen (a)
  "INTEGER-LENGTH of A: 0 for zero."
  (let ((i 9))
    (loop (when (< i 0) (return 0))
      (let ((x (svref a i)))
        (unless (= x 0)
          (let ((n 0))
            (loop (when (= x 0) (return nil))
              (setq x (ash x -1)) (setq n (+ n 1)))
            (return (+ (* 13 i) n)))))
      (setq i (- i 1)))))

(defun %sfv-bit (a n)
  "Bit N of A, 0 or 1."
  (let* ((w (floor n 13)) (r (- n (* 13 w))))
    (if (>= w 10) 0
        (logand (floor (svref a w) (%sf-pow2 r)) 1))))

(defun %sfv-setbit (a n)
  (let* ((w (floor n 13)) (r (- n (* 13 w))))
    (setf (svref a w) (logior (svref a w) (%sf-pow2 r)))
    a))

(defun %sfv-shl (a n)
  "A <<= N, in place (bits past 130 are lost).  Returns A."
  (let* ((w (floor n 13)) (r (- n (* 13 w))) (p (%sf-pow2 r)))
    (when (> w 0)
      (let ((i 9))
        (loop (when (< i 0) (return nil))
          (setf (svref a i) (if (>= (- i w) 0) (svref a (- i w)) 0))
          (setq i (- i 1)))))
    (when (> r 0)
      (let ((i 0) (carry 0))
        (loop (when (>= i 10) (return nil))
          (let ((t1 (+ (* (svref a i) p) carry)))
            (setf (svref a i) (logand t1 8191))
            (setq carry (ash t1 -13)))
          (setq i (+ i 1)))))
    a))

(defun %sfv-shr (a n)
  "A >>= N, in place.  Returns 1 if a 1 bit was shifted out, else 0."
  (let ((sticky 0))
    (if (>= n 130)
        (progn (unless (%sfv-zerop a) (setq sticky 1))
               (let ((i 0)) (loop (when (>= i 10) (return nil))
                              (setf (svref a i) 0) (setq i (+ i 1)))))
        (let* ((w (floor n 13)) (r (- n (* 13 w))))
          (when (> w 0)
            (let ((i 0))
              (loop (when (>= i w) (return nil))
                (unless (= (svref a i) 0) (setq sticky 1))
                (setq i (+ i 1))))
            (let ((i 0))
              (loop (when (>= i 10) (return nil))
                (setf (svref a i) (if (< (+ i w) 10) (svref a (+ i w)) 0))
                (setq i (+ i 1)))))
          (when (> r 0)
            (let ((p (%sf-pow2 r)) (up (%sf-pow2 (- 13 r))) (i 0))
              (let* ((x0 (svref a 0)) (q0 (floor x0 p)))
                (unless (= x0 (* q0 p)) (setq sticky 1)))
              (loop (when (>= i 10) (return nil))
                (let* ((x (svref a i)) (q (floor x p))
                       (hi (if (< (+ i 1) 10)
                               (let* ((y (svref a (+ i 1))) (qy (floor y p)))
                                 (- y (* qy p)))
                               0)))
                  (setf (svref a i) (+ q (* hi up))))
                (setq i (+ i 1)))))))
    sticky))

(defun %sfv-add (a b)
  "A += B, in place."
  (let ((i 0) (carry 0))
    (loop (when (>= i 10) (return a))
      (let ((t1 (+ (svref a i) (svref b i) carry)))
        (setf (svref a i) (logand t1 8191))
        (setq carry (ash t1 -13)))
      (setq i (+ i 1)))))

(defun %sfv-sub (a b)
  "A -= B, in place; A >= B."
  (let ((i 0) (borrow 0))
    (loop (when (>= i 10) (return a))
      (let ((t1 (- (svref a i) (svref b i) borrow)))
        (if (< t1 0)
            (progn (setf (svref a i) (+ t1 8192)) (setq borrow 1))
            (progn (setf (svref a i) t1) (setq borrow 0))))
      (setq i (+ i 1)))))

(defun %sfv-cmp (a b)
  (let ((i 9))
    (loop (when (< i 0) (return 0))
      (let ((x (svref a i)) (y (svref b i)))
        (cond ((> x y) (return 1)) ((< x y) (return -1))))
      (setq i (- i 1)))))

(defun %sfv-mul (a b)
  "A fresh vector A * B (the product must fit 130 bits)."
  (let ((c (%sfv)) (i 0))
    (loop (when (>= i 10) (return c))
      (let ((ai (svref a i)))
        (unless (= ai 0)
          (let ((j 0) (carry 0))
            (loop (when (>= (+ i j) 10) (return nil))
              (let ((t1 (+ (svref c (+ i j)) (* ai (svref b j)) carry)))
                (setf (svref c (+ i j)) (logand t1 8191))
                (setq carry (ash t1 -13)))
              (setq j (+ j 1))))))
      (setq i (+ i 1)))))

(defun %sfv-add1 (a)
  "A += 1, in place."
  (let ((i 0))
    (loop (when (>= i 10) (return a))
      (let ((t1 (+ (svref a i) 1)))
        (if (< t1 8192)
            (progn (setf (svref a i) t1) (return a))
            (setf (svref a i) 0)))
      (setq i (+ i 1)))))

(defun %sfv-field (a pos width)
  "WIDTH (<= 16) bits of A starting at bit POS, as a fixnum."
  (let ((b (%sfv-copy a)))
    (%sfv-shr b pos)
    (let ((x (+ (svref b 0) (* (logand (svref b 1) 7) 8192))))
      (logand x (- (* (%sf-pow2 (min width 13)) (if (> width 13) (%sf-pow2 (- width 13)) 1)) 1)))))

(defun %sfv-small (a)
  "A as a fixnum, when A < 2^29."
  (+ (svref a 0) (* (svref a 1) 8192) (* (svref a 2) 67108864)))

;;; ------------------------------------------------------------------
;;; Unpacking and packing
;;; ------------------------------------------------------------------

(defun %sf-chunks-vector (c0 c1 c2 c3)
  "The 64-bit value C0:C1:C2:C3 (16-bit chunks) as a vector."
  (let ((v (%sfv-from c0)))
    (%sfv-shl v 16) (%sfv-add v (%sfv-from c1))
    (%sfv-shl v 16) (%sfv-add v (%sfv-from c2))
    (%sfv-shl v 16) (%sfv-add v (%sfv-from c3))
    v))

(defun %sf-unpack (f)
  "F's (values sign biased-exponent significand E frac-zero-p), the value being
   significand * 2^E.  For exponent 2047 (inf/NaN) significand is the fraction."
  (let* ((c0 (%prim-aref f 0))
         (sign (ash c0 -15))
         (bexp (logand (ash c0 -4) 2047))
         (frac (%sf-chunks-vector (logand c0 15) (%prim-aref f 1)
                                  (%prim-aref f 2) (%prim-aref f 3)))
         (fz (%sfv-zerop frac)))
    (cond ((= bexp 2047) (values sign bexp frac 0 fz))
          ((= bexp 0) (values sign 0 frac -1074 fz))
          (t (values sign bexp (%sfv-setbit frac 52) (- bexp 1075) fz)))))

(defun %sf-box-chunks (c0 c1 c2 c3 single)
  (let ((f (if single (%make-single2) (%make-float2))))
    (%prim-aset f 0 c0) (%prim-aset f 1 c1)
    (%prim-aset f 2 c2) (%prim-aset f 3 c3)
    f))

(defun %sf-nan (single) (%sf-box-chunks #x7FF8 0 0 0 single))
(defun %sf-inf (sign single) (%sf-box-chunks (+ #x7FF0 (* sign #x8000)) 0 0 0 single))
(defun %sf-zero (sign single) (%sf-box-chunks (* sign #x8000) 0 0 0 single))

(defun %sf-pack (sign sig e single)
  "Box SIG * 2^E, which must be exactly representable as a double (nonzero)."
  (let* ((len (%sfv-ilen sig)) (e2 (+ e len -1)))
    (if (>= e2 -1022)
        ;; normal: leading bit to position 52
        (let ((sig2 (%sfv-copy sig)) (e3 e))
          (when (< len 53) (%sfv-shl sig2 (- 53 len)) (setq e3 (- e (- 53 len))))
          (when (> len 53) (%sfv-shr sig2 (- len 53)) (setq e3 (+ e (- len 53))))
          (let ((bexp (+ e3 1075)))
            (%sf-box-chunks (+ (* sign #x8000) (* bexp 16) (%sfv-field sig2 48 4))
                            (%sfv-field sig2 32 16) (%sfv-field sig2 16 16)
                            (%sfv-field sig2 0 16) single)))
        ;; subnormal: the LSB weighs 2^-1074
        (let ((sig2 (%sfv-copy sig)))
          (%sfv-shl sig2 (+ e 1074))
          (%sf-box-chunks (+ (* sign #x8000) (%sfv-field sig2 48 4))
                          (%sfv-field sig2 32 16) (%sfv-field sig2 16 16)
                          (%sfv-field sig2 0 16) single)))))

(defun %sf-round (sig e prec lsbmin emax)
  "Round SIG * 2^E (SIG nonzero, a STICKY bit already OR'd into its LSB when
   inexact) to PREC significant bits, the LSB no finer than 2^LSBMIN, nearest-
   even.  (values sig' e') -- or (values nil nil) on overflow past 2^EMAX."
  (let* ((len (%sfv-ilen sig)) (e2 (+ e len -1))
         (lsb (max (- e2 (- prec 1)) lsbmin))
         (shift (- lsb e)))
    (let ((s (%sfv-copy sig)) (e3 e))
      (when (> shift 0)
        (let* ((sticky (%sfv-shr s (- shift 1)))
               (guard (%sfv-bit s 0)))
          (%sfv-shr s 1)
          (setq e3 lsb)
          (when (and (= guard 1) (or (= sticky 1) (= (%sfv-bit s 0) 1)))
            (%sfv-add1 s))
          (when (> (%sfv-ilen s) prec)
            (%sfv-shr s 1) (setq e3 (+ e3 1)))))
      (if (or (%sfv-zerop s) (<= (+ e3 (%sfv-ilen s) -1) emax))
          (values s e3)
          (values nil nil)))))

(defun %sf-finish (sign sig e single)
  "Round to double and box (SINGLE: box it as a single-float as well)."
  (if (%sfv-zerop sig)
      (%sf-zero sign single)
      (multiple-value-bind (s e3) (%sf-round sig e 53 -1074 1023)
        (cond ((null s) (%sf-inf sign single))
              ((%sfv-zerop s) (%sf-zero sign single))
              (t (%sf-pack sign s e3 single))))))

(defun %sf-normalize (sig e)
  "(values sig' e') with SIG's leading bit moved to position 52 (SIG nonzero)."
  (let* ((s (%sfv-copy sig)) (len (%sfv-ilen s)))
    (if (< len 53)
        (progn (%sfv-shl s (- 53 len)) (values s (- e (- 53 len))))
        (values s e))))

;;; ------------------------------------------------------------------
;;; The operations
;;; ------------------------------------------------------------------

(defun %sf-addsub (a b negate-b)
  (multiple-value-bind (sa ea ma xa za) (%sf-unpack a)
    (multiple-value-bind (sb eb mb xb zb) (%sf-unpack b)
      (when negate-b (setq sb (- 1 sb)))
      (cond
        ((or (and (= ea 2047) (not za)) (and (= eb 2047) (not zb))) (%sf-nan nil))
        ((= ea 2047) (if (and (= eb 2047) (not (= sa sb))) (%sf-nan nil) (%sf-inf sa nil)))
        ((= eb 2047) (%sf-inf sb nil))
        ((%sfv-zerop ma)
         (if (%sfv-zerop mb)
             (%sf-zero (if (and (= sa 1) (= sb 1)) 1 0) nil)
             (%sf-pack sb mb xb nil)))
        ((%sfv-zerop mb) (%sf-pack sa ma xa nil))
        (t
         ;; X the operand with the larger exponent; scale X up by 8 (G,R,S)
         (let ((swap (< xa xb)))
           (let ((sx (if swap sb sa)) (mx (%sfv-copy (if swap mb ma))) (x (if swap xb xa))
                 (sy (if swap sa sb)) (my (%sfv-copy (if swap ma mb))) (y (if swap xa xb)))
             (%sfv-shl mx 3)
             (let ((d (- x y)))
               (if (<= d 3)
                   (%sfv-shl my (- 3 d))
                   (when (= (%sfv-shr my (- d 3)) 1) (%sfv-setbit my 0))))
             (let ((e (- x 3)))
               (if (= sx sy)
                   (progn (%sfv-add mx my) (%sf-finish sx mx e nil))
                   (let ((c (%sfv-cmp mx my)))
                     (cond ((= c 0) (%sf-zero 0 nil))
                           ((> c 0) (%sfv-sub mx my) (%sf-finish sx mx e nil))
                           (t (%sfv-sub my mx) (%sf-finish sy my e nil)))))))))))))

(defun %sf-add (a b) (%sf-addsub a b nil))
(defun %sf-sub (a b) (%sf-addsub a b t))

(defun %sf-mul (a b)
  (multiple-value-bind (sa ea ma xa za) (%sf-unpack a)
    (multiple-value-bind (sb eb mb xb zb) (%sf-unpack b)
      (let ((s (if (= sa sb) 0 1)))
        (cond
          ((or (and (= ea 2047) (not za)) (and (= eb 2047) (not zb))) (%sf-nan nil))
          ((or (= ea 2047) (= eb 2047))
           ;; inf * 0 is NaN; an infinity's own (zero) fraction is not a zero
           (if (or (and (/= ea 2047) (%sfv-zerop ma)) (and (/= eb 2047) (%sfv-zerop mb)))
               (%sf-nan nil) (%sf-inf s nil)))
          ((or (%sfv-zerop ma) (%sfv-zerop mb)) (%sf-zero s nil))
          (t (%sf-finish s (%sfv-mul ma mb) (+ xa xb) nil)))))))

(defun %sf-div (a b)
  (multiple-value-bind (sa ea ma xa za) (%sf-unpack a)
    (multiple-value-bind (sb eb mb xb zb) (%sf-unpack b)
      (let ((s (if (= sa sb) 0 1)))
        (cond
          ((or (and (= ea 2047) (not za)) (and (= eb 2047) (not zb))) (%sf-nan nil))
          ((= ea 2047) (if (= eb 2047) (%sf-nan nil) (%sf-inf s nil)))
          ((= eb 2047) (%sf-zero s nil))
          ((%sfv-zerop mb) (if (%sfv-zerop ma) (%sf-nan nil) (%sf-inf s nil)))
          ((%sfv-zerop ma) (%sf-zero s nil))
          (t
           (multiple-value-bind (na nxa) (%sf-normalize ma xa)
             (multiple-value-bind (nb nxb) (%sf-normalize mb xb)
               ;; q = floor(na * 2^58 / nb), 59 quotient bits, restoring division
               (let ((rem na) (q (%sfv)) (i 0))
                 (loop (when (> i 58) (return nil))
                   (%sfv-shl q 1)
                   (when (>= (%sfv-cmp rem nb) 0)
                     (%sfv-sub rem nb) (%sfv-setbit q 0))
                   (%sfv-shl rem 1)
                   (setq i (+ i 1)))
                 (unless (%sfv-zerop rem) (%sfv-setbit q 0))
                 (%sf-finish s q (- nxa nxb 58) nil))))))))))

(defun %sf-sqrt (a)
  (multiple-value-bind (sa ea ma xa za) (%sf-unpack a)
    (cond
      ((and (= ea 2047) (not za)) (%sf-nan nil))
      ((%sfv-zerop ma) (if (= ea 2047)
                           (if (= sa 1) (%sf-nan nil) (%sf-inf 0 nil))
                           (%sf-zero sa nil)))
      ((= sa 1) (%sf-nan nil))
      (t
       (multiple-value-bind (n e) (%sf-normalize ma xa)
         (when (oddp e) (%sfv-shl n 1) (setq e (- e 1)))
         ;; isqrt of n * 2^60: bit pairs from the top (n*2^60 < 2^114)
         (%sfv-shl n 60)
         (let ((res (%sfv)) (rem (%sfv)) (k 57))
           (loop (when (< k 0) (return nil))
             ;; rem = rem*4 + next two bits (bits 2k+1, 2k of n)
             (%sfv-shl rem 2)
             (when (= (%sfv-bit n (+ (* 2 k) 1)) 1) (%sfv-setbit rem 1))
             (when (= (%sfv-bit n (* 2 k)) 1) (%sfv-setbit rem 0))
             (let ((trial (%sfv-copy res)))
               (%sfv-shl trial 2) (%sfv-setbit trial 0)
               (%sfv-shl res 1)
               (when (>= (%sfv-cmp rem trial) 0)
                 (%sfv-sub rem trial) (%sfv-setbit res 0)))
             (setq k (- k 1)))
           (unless (%sfv-zerop rem) (%sfv-setbit res 0))
           (%sf-finish 0 res (floor (- e 60) 2) nil)))))))

(defun %sf-itof (n)
  "Fixnum N -> boxed double."
  (if (= n 0)
      (%sf-zero 0 nil)
      ;; |N| as (-1 - N) + 1: -N itself leaves fixnum range for the most
      ;; negative fixnum, and 68k's checked subtract wraps instead of promoting.
      (%sf-finish (if (< n 0) 1 0)
                  (if (< n 0) (%sfv-add1 (%sfv-from (- -1 n))) (%sfv-from n))
                  0 nil)))

(defun %sf-ftoi (a)
  "Boxed float -> fixnum, truncating toward zero.  Out of fixnum range (or
   inf/NaN), the low bits, as the FPU's register move would give garbage too."
  (multiple-value-bind (sa ea ma xa za) (%sf-unpack a)
    (declare (ignore za))
    (if (= ea 2047)
        0
        (let ((m (%sfv-copy ma)))
          (if (>= xa 0)
              (%sfv-shl m (min xa 130))
              (%sfv-shr m (- 0 xa)))
          (let ((v (+ (svref m 0) (* (svref m 1) 8192)
                      (* (logand (svref m 2) 7) 67108864))))
            (if (= sa 1) (- 0 v) v))))))

(defun %sf-round32 (a)
  "A's payload rounded to single precision, boxed as a single-float."
  (multiple-value-bind (sa ea ma xa za) (%sf-unpack a)
    (cond
      ((= ea 2047) (if za (%sf-inf sa t) (%sf-nan t)))
      ((%sfv-zerop ma) (%sf-zero sa t))
      (t (multiple-value-bind (s e3) (%sf-round ma xa 24 -149 127)
           (cond ((null s) (%sf-inf sa t))
                 ((%sfv-zerop s) (%sf-zero sa t))
                 (t (%sf-pack sa s e3 t))))))))

;;; Packed single-float lanes: raw IEEE32 bits in the CPU's byte order from
;;; byte 4i of the payload -- the layout %F32-BITS-REF-NARROW reads.
(defun %sf-lane-check (arr i) (%f32-lane-check arr i))

(defun %sf-f32-load (arr i)
  (%sf-lane-check arr i)
  (let* ((b0 (%u8-ref-unchecked arr (%f32-lane-byte i 0)))
         (b1 (%u8-ref-unchecked arr (%f32-lane-byte i 1)))
         (b2 (%u8-ref-unchecked arr (%f32-lane-byte i 2)))
         (b3 (%u8-ref-unchecked arr (%f32-lane-byte i 3)))
         (sign (ash b3 -7))
         (e8 (+ (* (logand b3 127) 2) (ash b2 -7)))
         (m23 (+ (* (logand b2 127) 65536) (* b1 256) b0)))
    (cond
      ((= e8 255) (if (= m23 0) (%sf-inf sign t) (%sf-nan t)))
      ((= e8 0) (if (= m23 0) (%sf-zero sign t) (%sf-pack sign (%sfv-from m23) -149 t)))
      (t (%sf-pack sign (%sfv-from (+ m23 8388608)) (- e8 150) t)))))

(defun %sf-f32-store (arr i x)
  "Round X's payload to single and store the lane.  Returns X."
  (%sf-lane-check arr i)
  (let ((sign 0) (e8 0) (m23 0))
    (multiple-value-bind (sa ea ma xa za) (%sf-unpack x)
      (setq sign sa)
      (cond
        ((= ea 2047) (setq e8 255 m23 (if za 0 4194304)))
        ((%sfv-zerop ma) nil)
        (t (multiple-value-bind (s e3) (%sf-round ma xa 24 -149 127)
             (cond ((null s) (setq e8 255))
                   ((%sfv-zerop s) nil)
                   (t (let ((q (%sfv-small s)))
                        (if (< q 8388608)
                            (setq m23 q)                         ; subnormal
                            (setq e8 (+ e3 23 127) m23 (- q 8388608))))))))))
    (%u8-set-unchecked arr (%f32-lane-byte i 0) (logand m23 255))
    (%u8-set-unchecked arr (%f32-lane-byte i 1) (logand (ash m23 -8) 255))
    (%u8-set-unchecked arr (%f32-lane-byte i 2) (+ (logand (ash m23 -16) 127) (* (logand e8 1) 128)))
    (%u8-set-unchecked arr (%f32-lane-byte i 3) (+ (* sign 128) (ash e8 -1)))
    x))
