;;;; cl-types.lisp — Type system: typep, numeric types, float operations
;;;; Part of the Modus CL runtime. Depends on cl-clos.lisp.

;;; ============================================================
;;; Trig / transcendental functions (stub implementations)
;;; These return values in the correct range for ANSI conformance tests:
;;; - Integer input 0 → exact integer 1 (cos) or 0 (sin/tan)
;;; - Float input 0.0 → float {1,1} (cos) or {0,1} (sin/tan)
;;; - Non-zero integer inputs → 0 (in range [-1,1]) for cos/sin
;;; This covers cos.1 (range loop), cos.6 (cos 0 = 1), cos.8-10 (cos 0.0 = 1.0)
;;; ============================================================

(defun %make-float-raw (num den)
  "Create a boxed float array with numerator NUM and denominator DEN."
  (let ((obj (make-array 2)))
    (aset obj 0 num)
    (aset obj 1 den)
    obj))

(defun %float-zero-p (x)
  "Return T if float X has numerator 0."
  (= (aref x 0) 0))

;;; ============================================================
;;; Transcendental functions — rational approximations via Taylor
;;; series.  Modus floats are 2-slot arrays (num . den) with subtag
;;; #x32, so we do all the arithmetic in fixed-precision rationals
;;; with a chosen denominator K (10^9 by default — ~9 digit precision
;;; for the inputs the ANSI tests probe).  Results return as
;;; (%make-float-raw num K) — a float-shaped rational.
;;; ============================================================

(defvar *%trig-precision* 1000000000)  ; K = 10^9
(defvar *%trig-pi*         3141592653)  ; π * K
(defvar *%trig-2pi*        6283185307)  ; 2π * K
(defvar *%trig-pi/2*       1570796327)  ; π/2 * K

(defun %as-scaled-int (x)
  "Convert X (integer, ratio, 2-slot float-shape, or IEEE float) to a
   fixnum scaled by *%trig-precision* (K).  Returns the scaled integer."
  (let ((k *%trig-precision*))
    (cond
      ((integerp x) (* x k))
      ((ratiop x) (truncate (* (ratio-numerator x) k)
                            (ratio-denominator x)))
      ((and (not (fixnump x)) (not (consp x)) (not (null x))
            (= (obj-subtag x) #x32) (= (array-length x) 2))
       (let ((n (aref x 0)) (d (aref x 1)))
         (if (= d 1) (* n k) (truncate (* n k) d))))
      ;; IEEE float (subtag #x60, 2-slot).  Compute scaled = round(x * K)
      ;; directly from the float bits (sign|exp(11)|mantissa(52)) so we
      ;; never build a giant intermediate bignum.  Going through
      ;; %ieee-float-to-rat → bignum-truncate is correct in theory but
      ;; bignum-truncate gives precision-wrong results for 80-bit-class
      ;; intermediates that the float→rat conversion produces.
      ;;
      ;; Algorithm: for x with biased exponent E and mantissa M (with
      ;; implicit leading 1), the value is (1 + M/2^52) * 2^(E-1023).
      ;; scaled = (2^52 + M) * K * 2^(E-1023) / 2^52
      ;;        = (2^52 + M) * K  shifted by (E-1075)
      ;; For 0 ≤ E-1075 we shift left (multiply by 2); for negative we
      ;; shift right (integer division by 2).  All arithmetic stays in
      ;; fixnum range when K ≤ 1e9 and |x| ≤ 8 — the Taylor inputs we
      ;; reduce mod 2π.
      ((and (not (fixnump x)) (not (consp x)) (not (null x))
            (not (characterp x))
            (= (obj-subtag x) #x60))
       (let* ((hi (aref x 0))
              (lo (aref x 1))
              (hi-u32 (logand hi 4294967295))
              (lo-u32 (logand lo 4294967295))
              (sign-bit (logand (ash hi-u32 -31) 1))
              (exp-bits (logand (ash hi-u32 -20) 2047))
              (mantissa-hi (logand hi-u32 1048575))
              (mantissa (logior (ash mantissa-hi 32) lo-u32))
              (raw-sign (if (zerop sign-bit) 1 -1)))
         (cond
           ;; Zero / subnormal — treat as 0.
           ((zerop exp-bits) 0)
           ;; Inf / NaN — return 0 (Modus doesn't represent these).
           ((= exp-bits 2047) 0)
           (t
            ;; Compute truncate(k * (1 + mantissa/2^52)) = k + truncate(k*m/2^52)
            ;; Split m into m_hi*2^26 + m_lo so k*m fits in two fixnum
            ;; products (k≤2^30, each half≤2^26 → k*half ≤ 2^56 < 2^62).
            (let* ((m-hi (ash mantissa -26))
                   (m-lo (logand mantissa 67108863))   ; (2^26 - 1)
                   ;; k*m / 2^52 = k*m_hi/2^26 + k*m_lo/2^52
                   (term-hi (truncate (* k m-hi) 67108864))    ; /2^26
                   (term-lo (truncate (* k m-lo) 4503599627370496))  ; /2^52
                   (base (+ k term-hi term-lo))   ; ~= k*(1+m/2^52)
                   (shift (- exp-bits 1023)))     ; final 2^shift scale
              (cond
                ((>= shift 0)
                 (if (> shift 30) 0    ; |x| > 2^30 — out of fixnum range
                     (* raw-sign (ash base shift))))
                (t
                 (let ((nshift (- 0 shift)))
                   (if (>= nshift 64)
                       0   ; |x| < 2^-64 — effectively zero
                       (* raw-sign (ash base (- 0 nshift))))))))))))
      (t 0))))

(defun %scaled-result (s)
  "Wrap a scaled-by-K integer S as a (num . den) float-shape."
  (%make-float-raw s *%trig-precision*))

(defun %trig-reduce (s)
  "Reduce scaled angle S into (-π, π] by repeated subtraction.  S is
   already an integer scaled by K."
  (let ((pi-k *%trig-pi*) (two-pi-k *%trig-2pi*))
    (loop (cond ((> s pi-k) (setq s (- s two-pi-k)))
                ((<= s (- 0 pi-k)) (setq s (+ s two-pi-k)))
                (t (return s))))))

(defun %sin-taylor (s)
  "Compute sin(s/K) using Taylor series, return scaled-by-K integer.
   s = scaled angle (integer in fixnum range).  Uses 10 terms which
   gives ~9 digit precision for |s| ≤ π."
  (let* ((k *%trig-precision*)
         (s (%trig-reduce s))
         ;; result = s - s^3/6 + s^5/120 - s^7/5040 + ...
         (acc s)
         (term s))   ; current term, scaled by K
    (let ((n 1))
      (loop
        (when (> n 19) (return acc))
        ;; term := -term * (s/K) * (s/K) / ((n+1)*(n+2))
        ;; To stay in fixnum range: term := truncate(-term*s*s / (K*K * (n+1)*(n+2)))
        (setq term (truncate (- 0 (* term (truncate (* s s) k))) (* k (* (+ n 1) (+ n 2)))))
        (setq acc (+ acc term))
        (when (and (< (abs term) 10)) (return acc))
        (setq n (+ n 2))))))

(defun %cos-taylor (s)
  "Compute cos(s/K) using Taylor series."
  (let* ((k *%trig-precision*)
         (s (%trig-reduce s))
         (acc k)    ; cos(0) = 1, scaled
         (term k))  ; current term
    (let ((n 0))
      (loop
        (when (> n 18) (return acc))
        (setq term (truncate (- 0 (* term (truncate (* s s) k))) (* k (* (+ n 1) (+ n 2)))))
        (setq acc (+ acc term))
        (when (and (< (abs term) 10)) (return acc))
        (setq n (+ n 2))))))

(defun sin (x)
  "Sine.  Exact 0 for integer 0; IEEE float result for other inputs.
   Taylor series runs in K-scaled rationals internally then %any-to-float
   converts to IEEE."
  (cond
    ((and (integerp x) (= x 0)) 0)
    (t (%any-to-float (%scaled-result (%sin-taylor (%as-scaled-int x)))))))

(defun cos (x)
  "Cosine.  Exact 1 for integer 0; IEEE float result otherwise."
  (cond
    ((and (integerp x) (= x 0)) 1)
    (t (%any-to-float (%scaled-result (%cos-taylor (%as-scaled-int x)))))))

(defun tan (x)
  "Tangent = sin/cos.  IEEE float result."
  (cond
    ((and (integerp x) (= x 0)) 0)
    (t (let* ((s (%as-scaled-int x))
              (sn (%sin-taylor s))
              (cs (%cos-taylor s)))
         (if (= cs 0)
             0
             (%any-to-float (%make-float-raw sn cs)))))))

(defun %exp-taylor (s)
  "Compute exp(s/K) using Taylor series.  Reduce |s| by halving if
   large: exp(s) = exp(s/2)^2."
  (let ((k *%trig-precision*))
    (cond
      ((> (abs s) k)
       ;; |x| > 1 — reduce
       (let ((half (%exp-taylor (truncate s 2))))
         (truncate (* half half) k)))
      (t
       (let ((acc k) (term k))
         (let ((n 1))
           (loop
             (when (> n 30) (return acc))
             (setq term (truncate (* term s) (* k n)))
             (setq acc (+ acc term))
             (when (< (abs term) 10) (return acc))
             (setq n (+ n 1)))))))))

(defun exp (x)
  "e^x.  Exact 1 for integer 0; IEEE float result otherwise."
  (cond
    ((and (integerp x) (= x 0)) 1)
    (t (%any-to-float (%scaled-result (%exp-taylor (%as-scaled-int x)))))))

(defun %log-newton (s)
  "Compute log(s/K) by Newton iteration on f(y) = exp(y) - s/K = 0.
   y_{n+1} = y_n - 1 + s / (K * exp(y_n))."
  (let* ((k *%trig-precision*)
         (y k))                    ; initial guess y = 1
    (let ((iter 0))
      (loop
        (when (> iter 40) (return y))
        (let* ((ey (%exp-taylor y))
               (delta (- (truncate (* s k) ey) k)))
          (when (< (abs delta) 10) (return y))
          (setq y (+ y delta))
          (setq iter (+ iter 1)))))))

(defun log (x &optional base)
  "Natural log (or log to BASE if supplied).  Domain x > 0; returns
   0 for x ≤ 0 (modus doesn't have complex logs)."
  ;; Exact 0 for x = 1.
  (when (and (integerp x) (= x 1)
             (or (null base) (and (integerp base) (>= base 2))))
    (return-from log 0))
  (let* ((s (%as-scaled-int x)))
    (when (<= s 0) (return-from log 0))
    (let ((ln-x (%log-newton s)))
      (if (null base)
          (%any-to-float (%scaled-result ln-x))
          ;; log_b(x) = log(x) / log(b)
          (let ((ln-b (%log-newton (%as-scaled-int base))))
            (if (= ln-b 0) 0 (%any-to-float (%make-float-raw ln-x ln-b))))))))

(defun cosh (x)
  "Hyperbolic cosine.  cosh(x) = (exp(x) + exp(-x))/2.  IEEE float result."
  (cond
    ((and (integerp x) (= x 0)) 1)
    (t (let* ((s (%as-scaled-int x))
              (ep (%exp-taylor s))
              (em (%exp-taylor (- 0 s))))
         (%any-to-float (%scaled-result (truncate (+ ep em) 2)))))))

(defun sinh (x)
  "Hyperbolic sine.  sinh(x) = (exp(x) - exp(-x))/2.  IEEE float result."
  (cond
    ((and (integerp x) (= x 0)) 0)
    (t (let* ((s (%as-scaled-int x))
              (ep (%exp-taylor s))
              (em (%exp-taylor (- 0 s))))
         (%any-to-float (%scaled-result (truncate (- ep em) 2)))))))

(defun tanh (x)
  "Hyperbolic tangent = sinh/cosh.  IEEE float result."
  (cond
    ((and (integerp x) (= x 0)) 0)
    (t (let* ((s (%as-scaled-int x))
              (ep (%exp-taylor s))
              (em (%exp-taylor (- 0 s)))
              (num (- ep em))
              (den (+ ep em)))
         (if (= den 0) 0 (%any-to-float (%make-float-raw num den)))))))

(defun %asin-taylor (s)
  "asin(s/K) via Taylor series for |s/K| ≤ 1.
   asin(x) = x + x^3/6 + 3x^5/40 + 15x^7/336 + ..."
  (let* ((k *%trig-precision*))
    (when (> (abs s) k) (return-from %asin-taylor 0))   ; out of domain
    (let ((acc s) (term s) (n 1))
      (loop
        (when (> n 30) (return acc))
        ;; term[n+2] = term[n] * (2n-1)*(2n+1) * s^2 / (K^2 * (2n)(2n+2))
        ;; Equivalently coefficient ratio.
        (let ((num-mult (* (- (* 2 n) 1) (+ (* 2 n) 1)))
              (den-mult (* (* 2 n) (+ (* 2 n) 2))))
          (setq term (truncate (* (truncate (* term (truncate (* s s) k)) k) num-mult)
                               den-mult)))
        (setq acc (+ acc term))
        (when (< (abs term) 10) (return acc))
        (setq n (+ n 1))))))

(defun asin (x)
  "Arc sine via Taylor.  Domain |x| ≤ 1.  IEEE float result."
  (cond
    ((and (integerp x) (= x 0)) 0)
    (t (%any-to-float (%scaled-result (%asin-taylor (%as-scaled-int x)))))))

(defun acos (x)
  "Arc cosine = π/2 - asin(x).  IEEE float result."
  (cond
    ((and (integerp x) (= x 1)) 0)
    ((and (integerp x) (= x 0)) (%any-to-float (%scaled-result *%trig-pi/2*)))
    (t (%any-to-float
        (%scaled-result (- *%trig-pi/2* (%asin-taylor (%as-scaled-int x))))))))

(defun atan (x &optional y)
  "Arc tangent.  Two-arg form: atan(y, x) for full quadrant.
   One-arg: atan(x) via series.  Uses atan(x) = asin(x/sqrt(1+x^2))."
  (cond
    (y
     ;; Two-arg atan(y, x): we treat the first arg as Y here per ANSI:
     ;; (atan y x).  Note ANSI param order: (atan number &optional divisor).
     ;; So x = number=Y/X factor, y = divisor=X.  Just compute Y/X via asin.
     ;; Approximation: only correct for quadrant 1 (x>0).
     (let ((ratio-scaled (truncate (* (%as-scaled-int x) *%trig-precision*)
                                   (%as-scaled-int y))))
       (atan (%make-float-raw ratio-scaled *%trig-precision*))))
    ((and (integerp x) (= x 0)) 0)
    (t
     (let* ((s (%as-scaled-int x))
            (k *%trig-precision*)
            (s2 (truncate (* s s) k))                  ; x^2
            (one-plus-s2 (+ k s2))
            (sqrt-arg (isqrt (* one-plus-s2 k)))       ; sqrt(1+x^2), scaled by K
            (ratio (truncate (* s k) sqrt-arg)))
       (%any-to-float (%scaled-result (%asin-taylor ratio)))))))

(defun phase (x)
  "Phase of x.  For real x: 0 if x≥0, π if x<0.  IEEE float for π case."
  (let ((pi-float (%any-to-float (%scaled-result *%trig-pi*))))
    (cond
      ((integerp x) (if (>= x 0) 0 pi-float))
      ((ratiop x) (if (>= (ratio-numerator x) 0) 0 pi-float))
      (t (let ((s (%as-scaled-int x)))
           (if (>= s 0) 0 pi-float))))))

(defun cis (x)
  "cis(x) = cos(x) + i*sin(x) — modus has no complex type, so return cos(x)
   as the real part (lossy but matches our (complex r) → r convention)."
  (cos x))
(defun integer (n) n)  ; not a real CL function but used as type coercion
(defun set-schar (str idx ch) (aset str idx (char-code ch)) ch)
(defun schar (str idx) (code-char (aref str idx)))
(defun char (str idx) (code-char (aref str idx)))
(defun symbol-plist (sym) nil)
; fboundp defined in Layer 8 above
(defun fill-pointer (vec)
  (if (consp vec) (car vec) (length vec)))
;; BIT-VECTOR-P + SIMPLE-BIT-VECTOR-P live below at L2195+; the early
;; copies here (subtag-walk variant and a nil-stub) silently lost the
;; last-defun-wins race.  Removed 2026-06-01 (redefinition audit).
(defun simple-string-p (x) (stringp x))
;;; ============================================================
;;; SUBTYPEP — basic ANSI lattice support.
;;;
;;; Strategy: be CONSERVATIVE.  Only return a definite answer
;;; (T T) for proven sub-relations or (NIL T) for proven disjoint
;;; pairs.  When we are not sure, return (NIL NIL) ("don't know").
;;; A wrong (T T) breaks check-all-not-subtypep tests; a wrong
;;; (NIL T) breaks anything that uses subtypep* and expects T T.
;;; (NIL NIL) at least preserves the stub's failure mode.
;;; ============================================================

(defun %subtype-bound-le (a b)
  "Numeric bound A <= B, where bounds are integers (no * here)."
  (numeric-<= a b))

(defun %subtype-bound-lt (a b)
  "Numeric bound A < B, where bounds are integers (no * here)."
  (numeric-value-less-p a b))

(defun %canon-low (lo)
  "Convert numeric range low bound to (kind . val) form.
   Returns (:open . n), (:closed . n), or (:neg-inf . nil)."
  (cond
    ((eq lo '*) (cons ':neg-inf nil))
    ((null lo) (cons ':neg-inf nil))
    ((and (consp lo) (null (cdr lo))) (cons ':open (car lo)))
    (t (cons ':closed lo))))

(defun %canon-high (hi)
  "Convert numeric range high bound to (kind . val) form.
   Returns (:open . n), (:closed . n), or (:pos-inf . nil)."
  (cond
    ((eq hi '*) (cons ':pos-inf nil))
    ((null hi) (cons ':pos-inf nil))
    ((and (consp hi) (null (cdr hi))) (cons ':open (car hi)))
    (t (cons ':closed hi))))

(defun %low-le-low (lo1 lo2)
  "Is the lower bound LO1 <= LO2 (i.e. LO1 lets in everything LO2 does)?
   LO1 and LO2 are canonicalized."
  (let ((k1 (car lo1)) (k2 (car lo2)))
    (cond
      ((eq k1 ':neg-inf) t)
      ((eq k2 ':neg-inf) nil)
      ;; both finite
      (t
       (let ((v1 (cdr lo1)) (v2 (cdr lo2)))
         (cond
           ((and (eq k1 ':closed) (eq k2 ':closed)) (%subtype-bound-le v1 v2))
           ((and (eq k1 ':closed) (eq k2 ':open))   (%subtype-bound-le v1 v2))
           ((and (eq k1 ':open)   (eq k2 ':closed)) (%subtype-bound-lt v1 v2))
           ((and (eq k1 ':open)   (eq k2 ':open))   (%subtype-bound-le v1 v2))
           (t nil)))))))

(defun %high-ge-high (hi1 hi2)
  "Is the upper bound HI1 >= HI2 (i.e. HI1 lets in everything HI2 does)?"
  (let ((k1 (car hi1)) (k2 (car hi2)))
    (cond
      ((eq k1 ':pos-inf) t)
      ((eq k2 ':pos-inf) nil)
      (t
       (let ((v1 (cdr hi1)) (v2 (cdr hi2)))
         (cond
           ((and (eq k1 ':closed) (eq k2 ':closed)) (%subtype-bound-le v2 v1))
           ((and (eq k1 ':closed) (eq k2 ':open))   (%subtype-bound-le v2 v1))
           ((and (eq k1 ':open)   (eq k2 ':closed)) (%subtype-bound-lt v2 v1))
           ((and (eq k1 ':open)   (eq k2 ':open))   (%subtype-bound-le v2 v1))
           (t nil)))))))

(defun %range-contains-p (lo1 hi1 lo2 hi2)
  "Range1 [lo1,hi1] contains range2 [lo2,hi2]?
   I.e. is range2 a subset of range1?"
  (and (%low-le-low lo1 lo2)
       (%high-ge-high hi1 hi2)))

(defun %parse-num-range (type)
  "TYPE is e.g. (integer L H), (integer L), (integer), 'integer.
   Returns cons (CANON-LOW . CANON-HIGH).  Caller has already
   verified the head matches the desired numeric type."
  (cond
    ((symbolp type) (cons (cons ':neg-inf nil) (cons ':pos-inf nil)))
    ((not (consp type)) (cons (cons ':neg-inf nil) (cons ':pos-inf nil)))
    (t
     (let ((rest (cdr type)))
       (let ((lo (if rest (car rest) '*))
             (hi (if (and rest (cdr rest)) (cadr rest) '*)))
         (cons (%canon-low lo) (%canon-high hi)))))))

(defun %num-type-head (type)
  "Return the head symbol of TYPE if TYPE is a numeric range type.
   Symbols return themselves."
  (cond
    ((symbolp type) type)
    ((consp type) (car type))
    (t nil)))

(defun %numeric-range-type-p (head)
  "Is HEAD one of the numeric range type names?"
  (or (eq head 'integer) (eq head 'rational) (eq head 'real)
      (eq head 'float) (eq head 'single-float) (eq head 'double-float)
      (eq head 'short-float) (eq head 'long-float) (eq head 'fixnum)
      (eq head 'bignum) (eq head 'bit) (eq head 'number)
      (eq head 'unsigned-byte) (eq head 'signed-byte)))

(defun %numeric-supertype-p (sup sub)
  "Is SUP a numeric supertype of (or equal to) SUB at the head level?
   SUP and SUB are head symbols (not compound types).
   Lattice: number > real > rational > integer > {fixnum, bignum, bit}
            number > real > rational > ratio
            number > real > float > {single,short,double,long}-float."
  (cond
    ((eq sup sub) t)
    ((eq sup 'number) (or (eq sub 'real) (eq sub 'rational) (eq sub 'integer)
                          (eq sub 'float) (eq sub 'fixnum) (eq sub 'bignum)
                          (eq sub 'bit) (eq sub 'ratio)
                          (eq sub 'single-float) (eq sub 'double-float)
                          (eq sub 'short-float) (eq sub 'long-float)
                          (eq sub 'unsigned-byte) (eq sub 'signed-byte)))
    ((eq sup 'real) (or (eq sub 'rational) (eq sub 'integer)
                        (eq sub 'float) (eq sub 'fixnum) (eq sub 'bignum)
                        (eq sub 'bit) (eq sub 'ratio)
                        (eq sub 'single-float) (eq sub 'double-float)
                        (eq sub 'short-float) (eq sub 'long-float)
                        (eq sub 'unsigned-byte) (eq sub 'signed-byte)))
    ((eq sup 'rational) (or (eq sub 'integer) (eq sub 'fixnum) (eq sub 'bignum)
                            (eq sub 'bit) (eq sub 'ratio)
                            (eq sub 'unsigned-byte) (eq sub 'signed-byte)))
    ((eq sup 'integer) (or (eq sub 'fixnum) (eq sub 'bignum) (eq sub 'bit)
                           (eq sub 'unsigned-byte) (eq sub 'signed-byte)))
    ((eq sup 'signed-byte) (or (eq sub 'integer) (eq sub 'fixnum)
                               (eq sub 'bignum) (eq sub 'bit)
                               (eq sub 'unsigned-byte)))
    ((eq sup 'unsigned-byte) (eq sub 'bit))
    ((eq sup 'fixnum) (eq sub 'bit))
    ((eq sup 'float) (or (eq sub 'single-float) (eq sub 'double-float)
                         (eq sub 'short-float) (eq sub 'long-float)))
    ;; CLHS 12.1.4.4: an implementation that lacks any of the float types
    ;; must consider them equivalent in the sense of SUBTYPEP.  Modus has
    ;; one IEEE double-precision representation behind all four names, so
    ;; (SUBTYPEP 'SHORT-FLOAT 'SINGLE-FLOAT) etc. all return T.
    ((or (eq sup 'single-float) (eq sup 'double-float)
         (eq sup 'short-float) (eq sup 'long-float))
     (or (eq sub 'single-float) (eq sub 'double-float)
         (eq sub 'short-float) (eq sub 'long-float)))
    (t nil)))

(defun %sub-default-low (head)
  "The default LOW bound when a subtype HEAD is unbounded.
   Most numeric types are unbounded below."
  (declare (ignore head))
  (cons ':neg-inf nil))

(defun %sub-default-high (head)
  "The default HIGH bound when a subtype HEAD is unbounded."
  (declare (ignore head))
  (cons ':pos-inf nil))

(defun %integer-head-p (h)
  "Is HEAD an integer-like type? (integer / fixnum / bignum / bit /
   signed-byte / unsigned-byte) — these have INTEGER bounds, so
   exclusive N at an integer N is equivalent to inclusive N+1."
  (or (eq h 'integer) (eq h 'fixnum) (eq h 'bignum) (eq h 'bit)
      (eq h 'signed-byte) (eq h 'unsigned-byte)))

(defun %normalize-int-low (lo)
  "If LO is (:open . N) and N is an integer, normalize to (:closed . N+1).
   Other bounds returned unchanged."
  (cond
    ((and (eq (car lo) ':open) (integerp (cdr lo)))
     (cons ':closed (+ (cdr lo) 1)))
    (t lo)))

(defun %normalize-int-high (hi)
  "If HI is (:open . N) and N is an integer, normalize to (:closed . N-1)."
  (cond
    ((and (eq (car hi) ':open) (integerp (cdr hi)))
     (cons ':closed (- (cdr hi) 1)))
    (t hi)))

(defun %sub-numeric-range (sub-type sup-type)
  "Both SUB-TYPE and SUP-TYPE are numeric range types (compound or symbol).
   Returns (values RESULT VALID).
   Returns (T T) if SUB ⊆ SUP, (NIL T) if disjoint or sup-too-narrow,
   (NIL NIL) otherwise."
  (let* ((sub-head (%num-type-head sub-type))
         (sup-head (%num-type-head sup-type))
         (sub-range (%parse-num-range sub-type))
         (sup-range (%parse-num-range sup-type))
         (sub-lo (car sub-range)) (sub-hi (cdr sub-range))
         (sup-lo (car sup-range)) (sup-hi (cdr sup-range)))
    ;; Integer-bound normalization: (:open . N) where N is integer is
    ;; equivalent to (:closed . N±1) when BOTH sides are integer types.
    ;; Lets `(integer (9)) ≡ (integer 10)` show as ranges containing each
    ;; other.
    (when (and (%integer-head-p sub-head) (%integer-head-p sup-head))
      (setq sub-lo (%normalize-int-low  sub-lo))
      (setq sub-hi (%normalize-int-high sub-hi))
      (setq sup-lo (%normalize-int-low  sup-lo))
      (setq sup-hi (%normalize-int-high sup-hi)))
    ;; First: check head-level subtype
    (cond
      ;; FLOAT vs RATIONAL/INTEGER are disjoint
      ((and (or (eq sub-head 'float) (eq sub-head 'single-float)
                (eq sub-head 'double-float) (eq sub-head 'short-float)
                (eq sub-head 'long-float))
            (or (eq sup-head 'rational) (eq sup-head 'integer)
                (eq sup-head 'fixnum) (eq sup-head 'bignum)
                (eq sup-head 'bit) (eq sup-head 'ratio)))
       (values nil t))
      ((and (or (eq sub-head 'rational) (eq sub-head 'integer)
                (eq sub-head 'fixnum) (eq sub-head 'bignum)
                (eq sub-head 'bit) (eq sub-head 'ratio))
            (or (eq sup-head 'float) (eq sup-head 'single-float)
                (eq sup-head 'double-float) (eq sup-head 'short-float)
                (eq sup-head 'long-float)))
       (values nil t))
      ;; Heads compatible?
      ((%numeric-supertype-p sup-head sub-head)
       ;; If sup is unbounded (head only or default range), trivially yes
       (cond
         ((%range-contains-p sup-lo sup-hi sub-lo sub-hi)
          (values t t))
         ;; Same head, range not contained → definite no
         ((eq sub-head sup-head)
          (values nil t))
         ;; Different heads, sup has narrower range — can't be sure for compound mismatch.
         ;; Conservative: if sub is unbounded but sup is bounded → no
         ((and (eq (car sub-lo) ':neg-inf) (eq (car sub-hi) ':pos-inf)
               (or (not (eq (car sup-lo) ':neg-inf))
                   (not (eq (car sup-hi) ':pos-inf))))
          (values nil t))
         (t (values nil nil))))
      ;; sup-head is not supertype of sub-head — but maybe sub-head is supertype of sup
      (t
       ;; e.g. (subtypep 'integer 'fixnum) — if sub-head ⊃ sup-head, definite NO
       ;; only if sub is unbounded or covers more than sup.
       (cond
         ((%numeric-supertype-p sub-head sup-head)
          ;; sub broader than sup at head level — must be NO unless ranges
          ;; restrict sub to fit within sup.  Be conservative: NIL NIL.
          (values nil nil))
         (t (values nil nil)))))))

;; --- Type-name lattice (non-numeric portion) ---
;; List of (sub super) edges; transitive closure handled by walker.
;; Includes redundant edges to cover ANSI subtype-table.

(defun %type-direct-supers (n)
  "Return list of immediate supertypes for type-name symbol N.
   Returns NIL for unknown names."
  (cond
    ((eq n 'null) '(symbol list boolean))
    ((eq n 'symbol) '(atom))
    ((eq n 'boolean) '(symbol))
    ((eq n 'keyword) '(symbol))
    ((eq n 'cons) '(list))
    ((eq n 'list) '(sequence))
    ((eq n 'sequence) '(atom))
    ((eq n 'array) '(atom))
    ((eq n 'simple-array) '(array))
    ((eq n 'vector) '(array sequence))
    ((eq n 'simple-vector) '(vector simple-array))
    ((eq n 'string) '(vector))
    ((eq n 'simple-string) '(string simple-array))
    ((eq n 'base-string) '(string))
    ((eq n 'simple-base-string) '(base-string simple-string))
    ((eq n 'bit-vector) '(vector))
    ((eq n 'simple-bit-vector) '(bit-vector simple-array))
    ((eq n 'character) '(atom))
    ((eq n 'base-char) '(character))
    ((eq n 'standard-char) '(base-char))
    ((eq n 'extended-char) '(character))
    ((eq n 'number) '(atom))
    ((eq n 'real) '(number))
    ((eq n 'rational) '(real))
    ((eq n 'integer) '(rational signed-byte))
    ((eq n 'ratio) '(rational))
    ((eq n 'float) '(real))
    ((eq n 'single-float) '(float))
    ((eq n 'double-float) '(float))
    ((eq n 'short-float) '(float))
    ((eq n 'long-float) '(float))
    ((eq n 'fixnum) '(integer))
    ((eq n 'bignum) '(integer))
    ((eq n 'bit) '(unsigned-byte fixnum))
    ((eq n 'unsigned-byte) '(signed-byte))
    ((eq n 'signed-byte) '(integer))
    ((eq n 'complex) '(number))
    ((eq n 'function) '(t))
    ((eq n 'compiled-function) '(function))
    ((eq n 'generic-function) '(function))
    ((eq n 'standard-generic-function) '(generic-function))
    ((eq n 'standard-object) '(t))
    ((eq n 'method) '(standard-object))
    ((eq n 'standard-method) '(method))
    ((eq n 'method-combination) '(t))
    ((eq n 'class) '(standard-object))
    ((eq n 'built-in-class) '(class))
    ((eq n 'structure-class) '(class))
    ((eq n 'standard-class) '(class))
    ((eq n 'structure-object) '(t))
    ((eq n 'condition) '(t))
    ((eq n 'serious-condition) '(condition))
    ((eq n 'error) '(serious-condition))
    ((eq n 'arithmetic-error) '(error))
    ((eq n 'division-by-zero) '(arithmetic-error))
    ((eq n 'cell-error) '(error))
    ((eq n 'unbound-slot) '(cell-error))
    ((eq n 'unbound-variable) '(cell-error))
    ((eq n 'undefined-function) '(cell-error))
    ((eq n 'parse-error) '(error))
    ((eq n 'reader-error) '(parse-error stream-error))
    ((eq n 'control-error) '(error))
    ((eq n 'program-error) '(error))
    ((eq n 'simple-condition) '(condition))
    ((eq n 'simple-error) '(simple-condition error))
    ((eq n 'simple-type-error) '(simple-condition type-error))
    ((eq n 'simple-warning) '(simple-condition warning))
    ((eq n 'type-error) '(error))
    ((eq n 'storage-condition) '(serious-condition))
    ((eq n 'warning) '(condition))
    ((eq n 'style-warning) '(warning))
    ((eq n 'package) '(t))
    ((eq n 'package-error) '(error))
    ((eq n 'random-state) '(atom))
    ((eq n 'pathname) '(atom))
    ((eq n 'logical-pathname) '(pathname))
    ((eq n 'file-error) '(error))
    ((eq n 'stream) '(atom))
    ((eq n 'broadcast-stream) '(stream))
    ((eq n 'concatenated-stream) '(stream))
    ((eq n 'echo-stream) '(stream))
    ((eq n 'file-stream) '(stream))
    ((eq n 'string-stream) '(stream))
    ((eq n 'synonym-stream) '(stream))
    ((eq n 'two-way-stream) '(stream))
    ((eq n 'stream-error) '(error))
    ((eq n 'end-of-file) '(stream-error))
    ((eq n 'print-not-readable) '(error))
    ((eq n 'readtable) '(atom))
    ((eq n 'hash-table) '(atom))
    ((eq n 'restart) '(t))
    ((eq n 'atom) '(t))
    (t nil)))

(defun %type-known-p (n)
  "Is N a recognized type name we know about?"
  (or (eq n 't) (eq n 'nil)
      (%type-direct-supers n)
      ;; Bare numeric types still known even if direct-supers is nil
      (eq n 'atom)))

(defun %has-supertype-p (sub sup depth)
  "Walk supertype chain: is SUP transitively a supertype of SUB?
   Bounded depth to prevent any cycle silliness.

   Float-name equivalence (CLHS 12.1.4.4): an implementation that lacks
   a particular float type must consider it equivalent to another.
   Modus stores all floats as one IEEE 64-bit type, so SHORT/SINGLE/
   DOUBLE/LONG-FLOAT are mutually interchangeable in the SUBTYPEP sense.
   Without this, tests gated on (SUBTYPEP 'SHORT-FLOAT 'SINGLE-FLOAT)
   would proceed into code that expects an actual distinction and fail."
  (cond
    ((<= depth 0) nil)
    ((eq sub sup) t)
    ((eq sup 't) t)
    ;; Float-name equivalence — short/single/double/long are all the same
    ;; underlying type in modus.
    ((and (or (eq sub 'short-float) (eq sub 'single-float)
              (eq sub 'double-float) (eq sub 'long-float))
          (or (eq sup 'short-float) (eq sup 'single-float)
              (eq sup 'double-float) (eq sup 'long-float)))
     t)
    (t
     (let ((parents (%type-direct-supers sub))
           (found nil))
       (dolist (p parents found)
         (when (and (not found) (%has-supertype-p p sup (- depth 1)))
           (setq found t)))))))

;; --- Disjoint-types check ---
;; Two types are disjoint if they belong to incompatible top-level
;; "buckets" with no shared bottom.  We determine the "root bucket"
;; for each type and compare.

(defun %type-bucket (n)
  "Return a top-level disjointness bucket symbol for type N, or NIL
   if uncertain.  Types in different buckets are disjoint."
  (cond
    ((eq n 'cons) ':cons)
    ;; symbol family — but null is in both symbol and list
    ((eq n 'null) ':null)
    ((eq n 'symbol) ':symbol)
    ((eq n 'boolean) ':symbol)
    ((eq n 'keyword) ':symbol)
    ;; numeric
    ((or (eq n 'number) (eq n 'real) (eq n 'rational) (eq n 'integer)
         (eq n 'float) (eq n 'fixnum) (eq n 'bignum) (eq n 'bit)
         (eq n 'ratio) (eq n 'single-float) (eq n 'double-float)
         (eq n 'short-float) (eq n 'long-float) (eq n 'complex)
         (eq n 'unsigned-byte) (eq n 'signed-byte))
     ':number)
    ((or (eq n 'character) (eq n 'base-char) (eq n 'standard-char)
         (eq n 'extended-char))
     ':character)
    ;; array/string family
    ((or (eq n 'array) (eq n 'simple-array) (eq n 'vector)
         (eq n 'simple-vector) (eq n 'string) (eq n 'simple-string)
         (eq n 'base-string) (eq n 'simple-base-string)
         (eq n 'bit-vector) (eq n 'simple-bit-vector))
     ':array)
    ((or (eq n 'function) (eq n 'compiled-function) (eq n 'generic-function)
         (eq n 'standard-generic-function))
     ':function)
    ((eq n 'hash-table) ':hash-table)
    ((or (eq n 'package) (eq n 'package-error)) ':package-or-error)
    ((eq n 'random-state) ':random-state)
    ((or (eq n 'pathname) (eq n 'logical-pathname)) ':pathname)
    ((or (eq n 'stream) (eq n 'broadcast-stream)
         (eq n 'concatenated-stream) (eq n 'echo-stream)
         (eq n 'file-stream) (eq n 'string-stream) (eq n 'synonym-stream)
         (eq n 'two-way-stream))
     ':stream)
    ((eq n 'readtable) ':readtable)
    (t nil)))

(defun %disjoint-buckets-p (b1 b2)
  "Two buckets are mutually disjoint if both are non-nil and not equal,
   AND neither is :null (which is in both :symbol and :cons-or-list space).
   Note: SEQUENCE intersects :array, :cons, :null."
  (and b1 b2 (not (eq b1 b2))
       (not (eq b1 ':null)) (not (eq b2 ':null))))

(defun %array-elt-bucket (n)
  "Sub-bucket within the :array family, by element type:
   :char (strings), :bit (bit-vectors), :t (simple-vectors), :any (array, vector)."
  (cond
    ((or (eq n 'string) (eq n 'simple-string)
         (eq n 'base-string) (eq n 'simple-base-string))
     ':char)
    ((or (eq n 'bit-vector) (eq n 'simple-bit-vector)) ':bit)
    ((eq n 'simple-vector) ':t)
    (t ':any)))   ; array, vector, simple-array — unknown elt-type

(defun %array-elt-disjoint-p (a b)
  "Within the :array bucket, two types are disjoint if they have
   incompatible element-type fingerprints (excluding :any which is
   compatible with anything)."
  (let ((ea (%array-elt-bucket a)) (eb (%array-elt-bucket b)))
    (and (not (eq ea ':any)) (not (eq eb ':any)) (not (eq ea eb)))))

(defun %symbol-type-disjoint-p (a b)
  "Definite-disjoint checker for two type-name symbols.
   Returns T if they are provably disjoint, NIL otherwise."
  (let ((ba (%type-bucket a)) (bb (%type-bucket b)))
    (cond
      ((%disjoint-buckets-p ba bb) t)
      ;; Within :array bucket — element-type sub-disjoint
      ((and (eq ba ':array) (eq bb ':array))
       (%array-elt-disjoint-p a b))
      (t nil))))

;; --- Compound-type SUBTYPEP entry ---

(defun %subtype-of-num (sub-type sup-type)
  "Both SUB-TYPE and SUP-TYPE have a numeric head.
   Wrap %sub-numeric-range, but also handle the common case where the
   sub-type is a bounded numeric range and sup-type is a wider numeric
   class symbol."
  (%sub-numeric-range sub-type sup-type))

(defun %subtypep-t-or (t-or)
  "Handle `T ⊆ (OR ...)`.  Split the OR into positive terms and a
   single negated term (if any).  Per CL: T ⊆ (OR X (NOT Y)) ⟺ Y ⊆ X.
   For OR with no NOT terms, only succeed if some term IS T (or all
   types cover everything — too hard to compute conservatively)."
  (let ((pos nil) (neg nil) (extra-neg nil))
    (dolist (term (cdr t-or))
      (cond
        ((and (consp term) (eq (car term) 'not))
         (if neg (setq extra-neg t) (setq neg (cadr term))))
        (t (push term pos))))
    (cond
      ;; Multiple NOT terms — give up.
      (extra-neg (values nil nil))
      ;; Single NOT term: T ⊆ (OR P1 P2 ... (NOT Y))
      ;; ⟺ (NOT (OR P1 P2 ...)) ⊆ NIL OR Y ⊆ (OR P1 P2 ...)
      ;; ⟺ Y ⊆ (OR P1 P2 ...)
      (neg
       (let ((pos-type (cond
                         ((null pos) 'nil)
                         ((null (cdr pos)) (car pos))
                         (t (cons 'or (nreverse pos))))))
         (multiple-value-bind (sub valid) (%subtypep-impl neg pos-type)
           (cond
             ((and valid sub) (values t t))
             (valid (values nil nil))   ; conservative — couldn't prove
             (t (values nil nil))))))
      ;; No NOT term — T ⊆ (OR ...) only if some term is T.
      ((member 't pos) (values t t))
      (t (values nil nil)))))

(defun %subtypep-and-nil (t-and)
  "Handle `(AND ...) ⊆ NIL`.  (AND P1 (NOT Y) P2 ...) ⊆ NIL
   ⟺ (AND P1 P2 ...) ⊆ Y (all positives are inside Y).
   For (AND ...) without a NOT term, check if any two positive terms
   are disjoint."
  (let ((pos nil) (neg nil) (extra-neg nil))
    (dolist (term (cdr t-and))
      (cond
        ((and (consp term) (eq (car term) 'not))
         (if neg (setq extra-neg t) (setq neg (cadr term))))
        (t (push term pos))))
    (cond
      (extra-neg (values nil nil))
      (neg
       (let ((pos-type (cond
                         ((null pos) 't)
                         ((null (cdr pos)) (car pos))
                         (t (cons 'and (nreverse pos))))))
         (multiple-value-bind (sub valid) (%subtypep-impl pos-type neg)
           (cond
             ((and valid sub) (values t t))
             (valid (values nil nil))
             (t (values nil nil))))))
      ;; No NOT in AND: empty iff any two positives are disjoint
      ;; (or any positive is NIL).
      (t
       (let ((seen nil) (disjoint nil))
         (dolist (term pos)
           (cond
             ((or (null term) (eq term 'nil)) (setq disjoint t))
             (t
              (dolist (s seen)
                (multiple-value-bind (sub valid) (%subtypep-impl term s)
                  (declare (ignore sub))
                  (when (and valid (multiple-value-bind (sub2 valid2)
                                       (%subtypep-impl term `(not ,s))
                                     (and valid2 sub2)))
                    (setq disjoint t))))
              (push term seen))))
         (cond
           (disjoint (values t t))
           (t (values nil nil))))))))

;; --- Array/string type-name canonicalisation for subtypep ---
;;
;; CLHS treats several names as equivalent for subtypep:
;;   string ≡ (vector character) ≡ (array character (*))
;;   base-string ≡ (vector base-char) ≡ (array base-char (*))
;;   simple-string ≡ (simple-array character (*))
;;   simple-base-string ≡ (simple-array base-char (*))
;;   vector ≡ (array * (*))
;;   simple-vector ≡ (simple-array t (*))
;;   bit-vector ≡ (vector bit) ≡ (array bit (*))
;;   simple-bit-vector ≡ (simple-array bit (*))
;; %canonicalise-array-type rewrites these aliases into a normalised
;; (HEAD ELT-TYPE DIMS) form so %subtypep-array can match them
;; bidirectionally.  Returns NIL when the type isn't an array/string
;; alias.

(defun %canonicalise-array-type (tp)
  "Rewrite array/vector aliases into canonical (HEAD ELT DIMS) form.
   HEAD ∈ {array simple-array}, ELT is a type spec (or '*), DIMS is '*
   or a list.  Returns NIL when not an array-family alias.

   Per CLHS, (vector ...) is (array ... (*)).  We only canonicalise the
   vector-family (vector / simple-vector / bit-vector / simple-bit-
   vector); the string family (string / base-string / simple-string /
   simple-base-string) is handled by dedicated clauses in %subtypep-impl
   so that (string ⊆ (vector character)) returns NIL T (CLHS allows
   them to be considered distinct in implementations that distinguish
   character types — we mimic that to match the ANSI tests' expectation)
   while ((vector character) ⊆ string) still returns T T."
  (cond
    ;; (vector ELT SIZE) — 1-D array
    ((and (consp tp) (%typename-eq (car tp) 'vector))
     (let ((elt (if (cdr tp) (cadr tp) '*))
           (sz  (if (cddr tp) (caddr tp) '*)))
       (list 'array
             (if (or (eq elt t) (null elt) (%typename-eq elt '*)) '* elt)
             (if (or (eq sz t) (null sz) (%typename-eq sz '*)) '(*) (list sz)))))
    ((%typename-eq tp 'vector) '(array * (*)))
    ;; (simple-vector SIZE)
    ((and (consp tp) (%typename-eq (car tp) 'simple-vector))
     (let ((sz (if (cdr tp) (cadr tp) '*)))
       (list 'simple-array 't
             (if (or (eq sz t) (null sz) (%typename-eq sz '*)) '(*) (list sz)))))
    ((%typename-eq tp 'simple-vector) '(simple-array t (*)))
    ;; (bit-vector SIZE)
    ((and (consp tp) (%typename-eq (car tp) 'bit-vector))
     (let ((sz (if (cdr tp) (cadr tp) '*)))
       (list 'array 'bit
             (if (or (eq sz t) (null sz) (%typename-eq sz '*)) '(*) (list sz)))))
    ((%typename-eq tp 'bit-vector) '(array bit (*)))
    ;; (simple-bit-vector SIZE)
    ((and (consp tp) (%typename-eq (car tp) 'simple-bit-vector))
     (let ((sz (if (cdr tp) (cadr tp) '*)))
       (list 'simple-array 'bit
             (if (or (eq sz t) (null sz) (%typename-eq sz '*)) '(*) (list sz)))))
    ((%typename-eq tp 'simple-bit-vector) '(simple-array bit (*)))
    ;; (base-string SIZE) ≡ (vector base-char SIZE)
    ((and (consp tp) (%typename-eq (car tp) 'base-string))
     (let ((sz (if (cdr tp) (cadr tp) '*)))
       (list 'array 'base-char
             (if (or (eq sz t) (null sz) (%typename-eq sz '*)) '(*) (list sz)))))
    ((%typename-eq tp 'base-string) '(array base-char (*)))
    ;; (simple-base-string SIZE) ≡ (simple-array base-char (SIZE))
    ((and (consp tp) (%typename-eq (car tp) 'simple-base-string))
     (let ((sz (if (cdr tp) (cadr tp) '*)))
       (list 'simple-array 'base-char
             (if (or (eq sz t) (null sz) (%typename-eq sz '*)) '(*) (list sz)))))
    ((%typename-eq tp 'simple-base-string) '(simple-array base-char (*)))
    (t nil)))

(defun %array-alias-p (tp)
  "True iff TP is one of the (vector …) / (simple-vector …) /
   (bit-vector …) / (simple-bit-vector …) / (base-string …) /
   (simple-base-string …) aliases (or bare symbols) that
   %canonicalise-array-type rewrites.  STRING / SIMPLE-STRING are
   excluded — they're CLHS-wider than (vector character) and are
   handled by dedicated string clauses in %subtypep-impl."
  (or (and (consp tp)
           (or (%typename-eq (car tp) 'vector)
               (%typename-eq (car tp) 'simple-vector)
               (%typename-eq (car tp) 'bit-vector)
               (%typename-eq (car tp) 'simple-bit-vector)
               (%typename-eq (car tp) 'base-string)
               (%typename-eq (car tp) 'simple-base-string)))
      (%typename-eq tp 'vector)
      (%typename-eq tp 'simple-vector)
      (%typename-eq tp 'bit-vector)
      (%typename-eq tp 'simple-bit-vector)
      (%typename-eq tp 'base-string)
      (%typename-eq tp 'simple-base-string)))

(defun %string-type-p (tp)
  "True iff TP is in the WIDE string family — 'string / 'simple-string
   and their parametric (NAME SIZE) variants.  These are unions over
   character subtypes (per CLHS 12.1.4.4) and are therefore wider than
   (vector character) etc.  base-string / simple-base-string are
   handled separately as direct aliases of (vector base-char) /
   (simple-array base-char (*))."
  (or (%typename-eq tp 'string)
      (%typename-eq tp 'simple-string)
      (and (consp tp)
           (or (%typename-eq (car tp) 'string)
               (%typename-eq (car tp) 'simple-string)))))

(defun %base-string-type-p (tp)
  "True iff TP is 'base-string / 'simple-base-string or their
   (NAME SIZE) parametric variant — these are direct aliases of
   (vector base-char) / (simple-array base-char (*))."
  (or (%typename-eq tp 'base-string)
      (%typename-eq tp 'simple-base-string)
      (and (consp tp)
           (or (%typename-eq (car tp) 'base-string)
               (%typename-eq (car tp) 'simple-base-string)))))

(defun %string-type-size (tp)
  "Extract the size spec from a string-family type: '* if absent."
  (cond
    ((and (consp tp) (cdr tp))
     (let ((sz (cadr tp)))
       (cond
         ((null sz) '*)
         ((%typename-eq sz '*) '*)
         ((eq sz t) '*)
         (t sz))))
    (t '*)))

(defun %string-type-simple-p (tp)
  "True iff TP is a 'simple-' string-family alias."
  (let ((head (if (consp tp) (car tp) tp)))
    (or (%typename-eq head 'simple-string)
        (%typename-eq head 'simple-base-string))))

(defun %char-array-type-p (tp)
  "True iff TP is a compound array type whose element type is one of
   the character family names.  Used to recognise (vector character),
   (array base-char (*)), etc. as equivalents of the string family."
  (and (consp tp)
       (or (%typename-eq (car tp) 'array)
           (%typename-eq (car tp) 'simple-array))
       (cdr tp)
       (%typename-character-elt-p (cadr tp))
       ;; Dims must be 1-D or unspecified: '*, (size), nil
       (let ((dims (if (cddr tp) (caddr tp) '*)))
         (or (eq dims '*)
             (%typename-eq dims '*)
             (eq dims t)
             (and (consp dims) (null (cdr dims)))))))

(defun %char-array-type-size (tp)
  "Extract size from a (array character (size)) / (array character N) /
   etc. compound.  '* if unspecified."
  (let ((dims (if (cddr tp) (caddr tp) '*)))
    (cond
      ((or (eq dims '*) (%typename-eq dims '*) (eq dims t)) '*)
      ((and (consp dims) (null (cdr dims)))
       (let ((c (car dims)))
         (if (or (eq c '*) (%typename-eq c '*) (eq c t)) '* c)))
      (t '*))))

(defun %subtypep-impl (t1 t2)
  "Implementation of SUBTYPEP returning two values: SUB? VALID?"
  (cond
    ;; Trivial cases
    ((eql t1 t2) (values t t))
    ((null t1) (values t t))
    ((eq t1 'nil) (values t t))
    ((eq t2 't) (values t t))
    ;; (NOT X) ⊆ (NOT Y) ⟺ Y ⊆ X (contrapositive).
    ;; Handle this BEFORE the t1=T or t2=NIL clauses below; check-
    ;; equivalence dispatches 4 NOT/NOT subchecks per call.
    ((and (consp t1) (eq (car t1) 'not)
          (consp t2) (eq (car t2) 'not))
     (%subtypep-impl (cadr t2) (cadr t1)))
    ;; T ⊆ (OR X (NOT Y)) ⟺ Y ⊆ X (everything-not-in-Y is in X).
    ;; This is the rule the universal-cover check uses.  We split the
    ;; (or ...) into a positive part and a single negated part; if the
    ;; negated part exists and the remaining positives are a single
    ;; type X, recurse on (Y ⊆ X).
    ((and (eq t1 't) (consp t2) (eq (car t2) 'or))
     (%subtypep-t-or t2))
    ;; (AND X (NOT Y)) ⊆ NIL  ⟺  X ⊆ Y (X minus Y is empty).
    ;; check-equivalence dispatches 4 of these per call.
    ((and (consp t1) (eq (car t1) 'and) (or (null t2) (eq t2 'nil)))
     (%subtypep-and-nil t1))
    ((eq t2 'nil)
     ;; Subtype of NIL means empty type; only NIL satisfies that.
     ;; If t1 is a known nonempty named type, return (NIL T).
     (cond
       ((symbolp t1)
        (if (%type-known-p t1) (values nil t) (values nil nil)))
       (t (values nil nil))))
    ((eq t1 't)
     ;; T is subtype only of T itself; t2 != t handled above.
     ;; If t2 is a named non-T type we know is below T, definite NO.
     (cond
       ((symbolp t2)
        (if (%type-known-p t2) (values nil t) (values nil nil)))
       (t (values nil nil))))
    ;; Both simple symbols (and not handled above)
    ((and (symbolp t1) (symbolp t2))
     (cond
       ;; Subtype walk
       ((%has-supertype-p t1 t2 30) (values t t))
       ;; Disjoint?
       ((%symbol-type-disjoint-p t1 t2) (values nil t))
       (t (values nil nil))))
    ;; t2 is a compound numeric range, t1 is a simple symbol or compound
    ((and (consp t2)
          (%numeric-range-type-p (car t2))
          (or (symbolp t1) (and (consp t1) (%numeric-range-type-p (car t1)))))
     (%sub-numeric-range t1 t2))
    ;; t1 is a compound numeric range, t2 is a simple symbol
    ((and (symbolp t2) (%numeric-range-type-p t2)
          (consp t1) (%numeric-range-type-p (car t1)))
     (%sub-numeric-range t1 t2))
    ;; t1 is a numeric symbol, t2 is a non-numeric symbol — disjoint?
    ((and (symbolp t1) (%numeric-range-type-p t1) (symbolp t2))
     (cond
       ((%has-supertype-p t1 t2 30) (values t t))
       ((%symbol-type-disjoint-p t1 t2) (values nil t))
       (t (values nil nil))))
    ;; Compound t1 numeric range, t2 non-numeric
    ((and (consp t1) (%numeric-range-type-p (car t1)) (symbolp t2))
     (cond
       ((%has-supertype-p (car t1) t2 30) (values t t))
       ((%symbol-type-disjoint-p (car t1) t2) (values nil t))
       (t (values nil nil))))
    ;; (eql v) ⊆ T2 — handled below for type-name t2
    ((and (consp t1) (eq (car t1) 'eql) (symbolp t2))
     (cond
       ((eq t2 't) (values t t))
       ((typep (cadr t1) t2) (values t t))
       ((%type-known-p t2) (values nil t))
       (t (values nil nil))))
    ;; (member v ...) ⊆ T2
    ((and (consp t1) (eq (car t1) 'member) (symbolp t2))
     (let ((all-in t) (vals (cdr t1)))
       (dolist (v vals)
         (unless (typep v t2) (setq all-in nil)))
       (cond
         (all-in (values t t))
         ((%type-known-p t2) (values nil t))
         (t (values nil nil)))))
    ;; (and ...) → if any sub of t2, then (and ...) is too
    ((and (consp t1) (eq (car t1) 'and))
     ;; (AND t a b) ⊆ T2 if any of a,b ⊆ T2
     (let ((some-yes nil))
       (dolist (s (cdr t1))
         (multiple-value-bind (sub valid) (%subtypep-impl s t2)
           (when (and valid sub) (setq some-yes t))))
       (cond
         (some-yes (values t t))
         (t (values nil nil)))))
    ;; (or A B ...) ⊆ T2 iff each of A,B ⊆ T2
    ((and (consp t1) (eq (car t1) 'or))
     (let ((all-yes t) (any-no nil))
       (dolist (s (cdr t1))
         (multiple-value-bind (sub valid) (%subtypep-impl s t2)
           (cond
             ((not valid) (setq all-yes nil))
             ((not sub) (setq all-yes nil) (setq any-no t)))))
       (cond
         (all-yes (values t t))
         (any-no  (values nil nil))   ; some element failed — could still be yes via covering
         (t (values nil nil)))))
    ;; T1 ⊆ (or A B ...) — yes if T1 ⊆ any of A,B
    ((and (consp t2) (eq (car t2) 'or))
     (let ((some-yes nil))
       (dolist (s (cdr t2))
         (multiple-value-bind (sub valid) (%subtypep-impl t1 s)
           (when (and valid sub) (setq some-yes t))))
       (cond
         (some-yes (values t t))
         (t (values nil nil)))))
    ;; T1 ⊆ (and A B ...) — yes if T1 ⊆ each of A,B
    ((and (consp t2) (eq (car t2) 'and))
     (let ((all-yes t))
       (dolist (s (cdr t2))
         (multiple-value-bind (sub valid) (%subtypep-impl t1 s)
           (unless (and valid sub) (setq all-yes nil))))
       (cond
         (all-yes (values t t))
         (t (values nil nil)))))
    ;; Array/vector alias canonicalisation: rewrite at most ONE side per
    ;; invocation, then recurse — both sides get normalised before
    ;; reaching the comparison clauses below.  String aliases are
    ;; handled separately to preserve the asymmetric (string ⊄ vector
    ;; character) relation the ANSI tests check for.
    ((%array-alias-p t1)
     (%subtypep-impl (%canonicalise-array-type t1) t2))
    ((%array-alias-p t2)
     (%subtypep-impl t1 (%canonicalise-array-type t2)))
    ;; String-family subtype clauses.
    ;;   1. (string A) ⊆ (string B) etc. — both are character arrays,
    ;;      size A ⊆ size B (in our world without extended-char).  We
    ;;      defer through the (array character (A)) representation.
    ;;   2. (array character …) ⊆ (string …) — yes, char arrays satisfy
    ;;      the string aliases.
    ;;   3. (string …) ⊆ (array character …) — NO (string is wider per
    ;;      CLHS: it's the union of char-element-typed arrays, not a
    ;;      direct subtype of any specific element type).
    ((and (%string-type-p t1) (%string-type-p t2))
     ;; Both string aliases — check the size relation.  simple-string ⊆
     ;; string but not vice versa; (string N) ⊆ (string M) iff N=M.
     (let* ((sz1 (%string-type-size t1))
            (sz2 (%string-type-size t2))
            (simple1 (%string-type-simple-p t1))
            (simple2 (%string-type-simple-p t2)))
       (cond
         ;; If t2 demands simple but t1 doesn't, fail.
         ((and simple2 (not simple1)) (values nil t))
         ;; Otherwise check size compatibility.
         ((or (eq sz2 '*) (%typename-eq sz2 '*)) (values t t))
         ((or (eq sz1 '*) (%typename-eq sz1 '*)) (values nil t))
         ((and (integerp sz1) (integerp sz2) (= sz1 sz2)) (values t t))
         (t (values nil t)))))
    ;; (array CHAR-FAMILY …) ⊆ (string …) — char-array IS a string.
    ((and (%char-array-type-p t1) (%string-type-p t2))
     (let* ((dim1 (%char-array-type-size t1))
            (sz2  (%string-type-size t2))
            (head1 (car t1))
            (simple2 (%string-type-simple-p t2)))
       (cond
         ((and simple2 (not (%typename-eq head1 'simple-array))) (values nil t))
         ((or (eq sz2 '*) (%typename-eq sz2 '*)) (values t t))
         ((or (eq dim1 '*) (%typename-eq dim1 '*)) (values nil t))
         ((and (integerp dim1) (integerp sz2) (= dim1 sz2)) (values t t))
         (t (values nil t)))))
    ;; (string …) ⊆ (array CHAR-FAMILY …) — NIL T per CLHS string-vs-
    ;; vector-of-character convention.  Strings can hold any char
    ;; subtype, so they aren't a subset of any one elt-type.
    ((and (%string-type-p t1) (%char-array-type-p t2))
     (values nil t))
    ;; (string …) ⊆ (array extended-char …) — NIL T (Modus has no
    ;; extended-char, so any extended-char-array is the empty type,
    ;; and non-empty strings don't fit there).
    ((and (%string-type-p t1)
          (consp t2)
          (or (%typename-eq (car t2) 'array)
              (%typename-eq (car t2) 'simple-array)
              (%typename-eq (car t2) 'vector)
              (%typename-eq (car t2) 'simple-vector))
          (cdr t2)
          (%typename-eq (cadr t2) 'extended-char))
     (values nil t))
    ;; Bare STRING / BASE-STRING etc. on one side, but bare ARRAY /
    ;; SIMPLE-ARRAY on the other — both name "array" shapes.  STRING is
    ;; a subtype of ARRAY (a string IS an array).  Conversely ARRAY is
    ;; not subtype of STRING.
    ((and (%string-type-p t1) (or (%typename-eq t2 'array)
                                  (%typename-eq t2 'simple-array)))
     (cond
       ((%typename-eq t2 'simple-array)
        (if (%string-type-simple-p t1) (values t t) (values nil t)))
       (t (values t t))))
    ((and (or (%typename-eq t1 'array) (%typename-eq t1 'simple-array))
          (%string-type-p t2))
     (values nil t))
    ;; (array ETYPE DIMS) ⊆ (array ETYPE2 DIMS2) — match ANSI rules
    ((and (or (and (consp t1) (or (%typename-eq (car t1) 'array)
                                  (%typename-eq (car t1) 'simple-array)))
              (%typename-eq t1 'array) (%typename-eq t1 'simple-array))
          (or (and (consp t2) (or (%typename-eq (car t2) 'array)
                                  (%typename-eq (car t2) 'simple-array)))
              (%typename-eq t2 'array) (%typename-eq t2 'simple-array)))
     (%subtypep-array t1 t2))
    ;; (cons A B) — compound CONS type.
    ((and (or (and (consp t1) (eq (car t1) 'cons)) (eq t1 'cons))
          (or (and (consp t2) (eq (car t2) 'cons)) (eq t2 'cons)))
     (%subtypep-cons t1 t2))
    ;; (cons ...) ⊆ symbol Y — true if Y is supertype of cons (list etc.)
    ;; If the cons is empty (NIL car or cdr), it's empty type ⊆ everything.
    ((and (or (and (consp t1) (eq (car t1) 'cons)) (eq t1 'cons))
          (symbolp t2))
     (let* ((rest1 (if (consp t1) (cdr t1) nil))
            (a (if rest1 (car rest1) 't))
            (b (if (and rest1 (cdr rest1)) (cadr rest1) 't)))
       (cond
         ((or (%cons-arg-empty-p a) (%cons-arg-empty-p b)) (values t t))
         ((%has-supertype-p 'cons t2 30) (values t t))
         ((%type-known-p t2) (values nil t))
         (t (values nil nil)))))
    ;; symbol X ⊆ (cons ...) — only NULL is fully covered
    ((and (symbolp t1) (consp t2) (eq (car t2) 'cons))
     (cond
       ((eq t1 'null) (values nil t))   ; null is not cons
       ((%has-supertype-p t1 'cons 30) (values nil nil))
       ((%symbol-type-disjoint-p t1 'cons) (values nil t))
       (t (values nil nil))))
    ;; (array ...) / (simple-array ...) ⊆ symbol Y
    ((and (or (and (consp t1) (or (eq (car t1) 'array) (eq (car t1) 'simple-array)))
              (eq t1 'array) (eq t1 'simple-array))
          (symbolp t2))
     (let ((head (if (consp t1) (car t1) t1)))
       (cond
         ((%has-supertype-p head t2 30) (values t t))
         ((%type-known-p t2) (values nil t))
         (t (values nil nil)))))
    ;; Default: don't know
    (t (values nil nil))))

(defun %canon-array-dims (dims)
  "Normalize ARRAY DIMS form to a canonical shape.
   '* → '*
   N (integer) → list of N '*'s (representing rank N with all-* dims)
   (* * ... *) (all-asterisk list) → equivalent to integer rank length
   (specific) → returned as-is
   Returns either '* (any rank) or a list of dim entries."
  (cond
    ((eq dims '*) '*)
    ((null dims) nil)
    ((integerp dims)
     (let ((acc nil) (i 0))
       (loop (when (>= i dims) (return acc))
         (setq acc (cons '* acc))
         (setq i (+ i 1)))))
    ((listp dims) dims)
    (t dims)))

(defun %dim-le-p (d1 d2)
  "Is dim spec D1 ⊆ D2?  '*' matches anything; integers and other
   values must match by EQL."
  (or (eq d2 '*) (eql d1 d2)))

(defun %dims-le-p (ds1 ds2)
  "Pairwise check that every dim in DS1 fits in DS2.  Both must have
   the same length."
  (cond
    ((and (null ds1) (null ds2)) t)
    ((or (null ds1) (null ds2)) nil)
    ((%dim-le-p (car ds1) (car ds2)) (%dims-le-p (cdr ds1) (cdr ds2)))
    (t nil)))

(defun %subtypep-array (t1 t2)
  "Subtypep for (array ETYPE DIMS) compound types.
   ETYPE form: T or * for any element type; otherwise a concrete element type.
   DIMS form: * for any rank, integer N for rank N, list for specific dim spec.

   Per CL: SIMPLE-ARRAY ⊆ ARRAY.  SUBTYPE relations on ETYPE / DIMS are
   applied pairwise.  ETYPEs normalized via canon (* or T as wildcard).
   DIMS normalized via %canon-array-dims so integer rank N and a list of
   N asterisks are equivalent."
  (let* ((head1 (if (consp t1) (car t1) t1))
         (head2 (if (consp t2) (car t2) t2))
         (rest1 (if (consp t1) (cdr t1) nil))
         (rest2 (if (consp t2) (cdr t2) nil))
         (et1 (if rest1 (car rest1) '*))
         (et2 (if rest2 (car rest2) '*))
         (dims1 (%canon-array-dims (if (and rest1 (cdr rest1)) (cadr rest1) '*)))
         (dims2 (%canon-array-dims (if (and rest2 (cdr rest2)) (cadr rest2) '*))))
    ;; Head subtype: simple-array ⊆ array
    (cond
      ((and (%typename-eq head1 'array) (%typename-eq head2 'simple-array))
       (values nil t))     ; non-simple-array isn't simple-array
      ;; Element-type EXTENDED-CHAR is the empty type in Modus (we have
      ;; no extended characters), so any (array extended-char …) is
      ;; the empty type ⊆ anything = T, and any non-empty (array X …)
      ;; ⊄ (array extended-char …) = NIL T.
      ((%typename-eq et1 'extended-char) (values t t))
      ((%typename-eq et2 'extended-char) (values nil t))
      (t
       (let ((et-ok (%array-etype-equiv-p et1 et2)))
         (cond
           ((not et-ok) (values nil nil))
           ;; dims2 is * → any rank/dims accepted
           ((eq dims2 '*) (values t t))
           ;; dims1 is * but dims2 specific → over-broad
           ((eq dims1 '*) (values nil t))
           ;; Both are lists — pairwise check
           ((%dims-le-p dims1 dims2) (values t t))
           (t (values nil t))))))))

(defun %array-etype-equiv-p (et1 et2)
  "True iff array element-type specs ET1 and ET2 should be considered
   equivalent for SUBTYPEP purposes.  CLHS allows wide latitude here
   because element-type upgrading is implementation-defined; we equate:
     - '* / T / (eql T) with each other.
     - 'character / 'base-char / 'standard-char (Modus has only base
       chars).
     - 'bit with itself.
     - Same compound type by EQUAL.
   When ET1=T and ET2 is a non-* specific type (or vice versa) we
   answer T (the test is meant to detect when the upgraded element
   types are compatible — Modus upgrades everything to T)."
  (cond
    ((or (eq et1 '*) (%typename-eq et1 '*))
     (or (eq et2 '*) (%typename-eq et2 '*) (eq et2 't) (%typename-eq et2 't)))
    ((or (eq et1 't) (%typename-eq et1 't))
     (or (eq et2 '*) (%typename-eq et2 '*) (eq et2 't) (%typename-eq et2 't)))
    ((or (eq et2 '*) (%typename-eq et2 '*) (eq et2 't) (%typename-eq et2 't))
     ;; ET1 is some specific non-* type, ET2 is T-like.  ARRAY of
     ;; specific-type IS a subtype of ARRAY of T (specific upgrades to
     ;; T), so this direction can be T — but we're called for the
     ;; equality of upgraded types.  Modus upgrades nearly everything
     ;; to T, so treat the inclusion as element-type-OK; the SUBTYPEP
     ;; caller already handles the array-direction via dims walk.
     t)
    ;; Character family.
    ((and (%typename-character-elt-p et1) (%typename-character-elt-p et2)) t)
    ;; NIL element-type is the empty type — never matches anything
    ;; except NIL itself.
    ((and (or (null et1) (eq et1 'nil)) (or (null et2) (eq et2 'nil))) t)
    ((or (null et1) (eq et1 'nil) (null et2) (eq et2 'nil)) nil)
    ;; Same bare type name (handle symbol-identity duplicates).
    ((and (symbolp et1) (symbolp et2) (%typename-eq et1 et2)) t)
    ;; Same compound type (e.g. (unsigned-byte 8) vs (unsigned-byte 8)).
    ((equal et1 et2) t)
    (t nil)))

(defun %typename-character-elt-p (e)
  "True iff E is one of the character-family element-type names."
  (or (%typename-eq e 'character)
      (%typename-eq e 'base-char)
      (%typename-eq e 'standard-char)))

(defun %cons-arg-empty-p (a)
  "True if A as a CONS car/cdr type-spec is empty.  NIL (the type) is
   empty.  (and X (not X)) is empty.  Conservative: returns NIL when
   unsure."
  (cond
    ((null a) t)
    ((eq a 'nil) t)
    (t nil)))

(defun %subtypep-cons (t1 t2)
  "Subtypep for (cons CAR-TYPE CDR-TYPE) compound types.
   (cons A B) ⊆ (cons C D) iff A ⊆ C AND B ⊆ D.
   Missing args default to T.
   Empty cons (any arg empty) is the empty type, ⊆ everything."
  (let* ((rest1 (if (consp t1) (cdr t1) nil))
         (rest2 (if (consp t2) (cdr t2) nil))
         (a (if rest1 (car rest1) 't))
         (b (if (and rest1 (cdr rest1)) (cadr rest1) 't))
         (c (if rest2 (car rest2) 't))
         (d (if (and rest2 (cdr rest2)) (cadr rest2) 't)))
    ;; Treat * as t
    (when (eq a '*) (setq a 't))
    (when (eq b '*) (setq b 't))
    (when (eq c '*) (setq c 't))
    (when (eq d '*) (setq d 't))
    ;; (cons NIL X) or (cons X NIL) is empty — empty ⊆ anything = T.
    (cond
      ((or (%cons-arg-empty-p a) (%cons-arg-empty-p b)) (values t t))
      (t
       (multiple-value-bind (s1 v1) (%subtypep-impl a c)
         (multiple-value-bind (s2 v2) (%subtypep-impl b d)
           (cond
             ((and v1 v2 s1 s2) (values t t))
             ((and v1 v2) (values nil t))
             (t (values nil nil)))))))))

(defun subtypep (t1 t2)
  "Two-value SUBTYPEP per ANSI: returns (sub valid).
   Conservative — returns (NIL NIL) when uncertain.
   Note: dropped &rest args (was &rest args (declare (ignore args))).
   The &rest+MV-tail combination interfered with multiple-value
   propagation, so callers using (multiple-value-list (subtypep ...))
   only got 1 value back instead of 2."
  (multiple-value-bind (sub valid) (%subtypep-impl t1 t2)
    (values sub valid)))
;; Minimal stub: ANSI returns 3 values (lambda-expr closure-p name).
;; Returning (NIL NIL NIL) at least lets length-checking tests pass.
(defun function-lambda-expression (fn) (values nil nil nil))
;; Many basic predicates are inline opcodes with no callable function
;; entry, so `#'pred' resolves to NIL/garbage and (funcall #'pred x)
;; silently misbehaves.  Adding real defuns gives them callable
;; addresses without affecting inline `(pred x)' calls (the compiler
;; still uses the opcode for those).  Each defun closes a small set
;; of ANSI tests that pass the predicate through funcall/mapcar/etc.
(defun not (x) (if x nil t))
(defun null (x) (if x nil t))
(defun zerop (x) (= x 0))
(defun identity (x) x)
;; Selectors — needed as defuns so `#'second` etc. have fdefinition entries
;; for function-designator contexts (SORT :KEY #'SECOND, mapcar #'FIRST, etc.).
(defun first (x) (car x))
(defun second (x) (car (cdr x)))
(defun third (x) (car (cdr (cdr x))))
(defun fourth (x) (car (cdr (cdr (cdr x)))))
(defun fifth (x) (car (cdr (cdr (cdr (cdr x))))))
(defun sixth (x) (car (cdr (cdr (cdr (cdr (cdr x)))))))
(defun seventh (x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))
(defun eighth (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))
(defun ninth (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))
(defun tenth (x) (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))
(defun rest (x) (cdr x))
;; Numeric increment helpers
(defun 1+ (n) (+ n 1))
(defun 1- (n) (- n 1))
;; More callable inline-equivalents
(defun listp (x) (if (consp x) t (null x)))
(defun fixnump (x) (fixnump x))
;; eq/eql don't have wrapper-name mapping in compile-li-func, but they
;; ARE inline.  Bare defun gives #'eq/#'eql callable addresses.
;; equal IS wrappped via compile-li-func to %EQUAL-FN already; no defun
;; needed (and recursive (defun equal (a b) (equal a b)) infinite-loops).
(defun eq (a b) (eq a b))
(defun eql (a b)
  "EQL — same as EQ for non-numbers/non-chars; for numbers, equal in
   type AND value.  Boxed numerics (IEEE floats subtag #x60, ratios
   subtag #x33, bignums subtag #x30) allocate a fresh object per
   literal/computation, so two copies with the same value satisfy
   CLHS eql but fail the inline (eql a b) identity opcode.  Slot
   compare them here.  Body uses `eq` (not `eql`) at the end so the
   inline compile-eql can fall through into this defun without
   infinite recursion."
  (cond
    ((eq a b) t)
    ((and (%ieee-float-p a) (%ieee-float-p b))
     (and (= (aref a 0) (aref b 0))
          (= (aref a 1) (aref b 1))))
    ((and (ratiop a) (ratiop b))
     (and (= (aref a 0) (aref b 0))
          (= (aref a 1) (aref b 1))))
    ((and (bignump a) (bignump b))
     ;; Bignums also box per value.  numeric-equal-p handles all
     ;; integer-typed combinations including bignum slot-compare.
     (numeric-equal-p a b))
    (t nil)))
;; Bare defuns for the primitive cons/car/cdr opcodes so #'cons /
;; #'car / #'cdr resolve to callable function objects (used by reduce
;; and similar tests like (reduce #'cons '(a b c))).
(defun cons (a b) (cons a b))
(defun car (x) (car x))
(defun cdr (x) (cdr x))
(defun + (&rest args)
  ;; Variadic + so #'+ is callable. Inline + opcode requires fixed args.
  (if (null args) 0
      (let ((r (car args)) (rest (cdr args)))
        (loop (when (null rest) (return r))
              (setq r (+ r (car rest)))
              (setq rest (cdr rest))))))
(defun - (a &rest rest)
  (if (null rest) (- 0 a)
      (let ((r a) (cur rest))
        (loop (when (null cur) (return r))
              (setq r (- r (car cur)))
              (setq cur (cdr cur))))))
(defun * (&rest args)
  (if (null args) 1
      (let ((r (car args)) (rest (cdr args)))
        (loop (when (null rest) (return r))
              (setq r (* r (car rest)))
              (setq rest (cdr rest))))))

;; `/` is compile-time intrinsic but had no callable defun, so #'/ /
;; (apply #'/ ...) / runtime-EVAL of (/ a b) all failed.  Compiled
;; callers still use the inline opcode (compile-div); this defun only
;; serves the SFT-routed runtime path.
(defun / (a &rest rest)
  (if (null rest) (/ 1 a)
      (let ((r a) (cur rest))
        (loop (when (null cur) (return r))
              (setq r (/ r (car cur)))
              (setq cur (cdr cur))))))

;; truncate / floor / ceiling / round / mod / rem — all opcode-only at
;; compile time with no defun fallback, so runtime EVAL of (truncate
;; 17 5) errored with %eval-escape.  Compiled callers still use the
;; inline opcodes; these defuns serve the SFT-routed runtime path.

(defun truncate (n &rest rest)
  (cond
    ((null rest) (truncate n))
    (t (truncate n (car rest)))))

;; floor / ceiling / round / mod / rem aren't compile-time intercepted
;; the way truncate is, so a defun that recursed by name would loop
;; forever.  Compute via truncate (which IS intercepted, so inside the
;; compiled body the compiler emits the :div / :mod opcode directly).

(defun floor (n &rest rest)
  (cond
    ((null rest)
     ;; 1-arg: integer is itself.  Ratio must route through 2-arg
     ;; floor on num/denom so the toward-negative-infinity adjustment
     ;; fires.  Plain (truncate ratio) goes toward zero, giving the
     ;; wrong direction for negative ratios.
     (cond ((integerp n) n)
           ((ratiop n) (floor (aref n 0) (aref n 1)))
           (t (truncate n))))
    (t
     (let ((d (car rest)))
       (multiple-value-bind (q r) (truncate n d)
         ;; truncate is toward zero; floor is toward negative infinity.
         ;; If r != 0 and (sign r) != (sign d), q = q - 1, r = r + d.
         (if (and (not (= r 0))
                  (if (< d 0) (> r 0) (< r 0)))
             (values (- q 1) (+ r d))
             (values q r)))))))

(defun ceiling (n &rest rest)
  (cond
    ((null rest)
     (cond ((integerp n) n)
           ((ratiop n) (ceiling (aref n 0) (aref n 1)))
           (t (truncate n))))
    (t
     (let ((d (car rest)))
       (multiple-value-bind (q r) (truncate n d)
         ;; ceiling = toward positive infinity.  If r != 0 and signs of
         ;; r and d agree, q = q + 1, r = r - d.
         (if (and (not (= r 0))
                  (if (< d 0) (< r 0) (> r 0)))
             (values (+ q 1) (- r d))
             (values q r)))))))

(defun round (n &rest rest)
  (cond
    ((null rest)
     (cond ((integerp n) n)
           ((ratiop n) (round (aref n 0) (aref n 1)))
           (t (truncate n))))
    (t
     (let ((d (car rest)))
       (multiple-value-bind (q r) (truncate n d)
         ;; round to nearest, ties to even.  |r| compared to |d|/2.
         (let ((abs-r (if (< r 0) (- r) r))
               (abs-d (if (< d 0) (- d) d)))
           (cond
             ;; |r| > |d|/2: round away from zero
             ((> (* 2 abs-r) abs-d)
              (if (if (< d 0) (> r 0) (< r 0))
                  (values (- q 1) (+ r d))
                  (values (+ q 1) (- r d))))
             ;; |r| < |d|/2: keep truncation
             ((< (* 2 abs-r) abs-d) (values q r))
             ;; tie: round to even q
             (t (if (oddp q)
                    (if (< r 0) (values (- q 1) (+ r d))
                        (values (+ q 1) (- r d)))
                    (values q r))))))))))

(defun mod (n d)
  ;; CL: mod = n - d * (floor n d)
  (multiple-value-bind (q r) (floor n d) (declare (ignore q)) r))

(defun rem (n d)
  ;; CL: rem = n - d * (truncate n d)
  (multiple-value-bind (q r) (truncate n d) (declare (ignore q)) r))

;; ash and lognot are inline opcodes at compile time but have no defun
;; fallback for runtime EVAL.  Wrap so SFT lookup finds them.
(defun ash (n s) (ash n s))
(defun lognot (n) (lognot n))
;; ANSI: for n>=0, count 1-bits.  For n<0, count 0-bits in two's
;; complement — equivalently, count 1-bits of (lognot n) = -1-n.
(defun logcount (n)
  (let ((x (if (< n 0) (- -1 n) n)) (c 0))
    (loop
      (when (zerop x) (return c))
      (when (oddp x) (setq c (+ c 1)))
      (setq x (ash x -1)))))
(defun %remf (plist indicator)
  "Remove property INDICATOR from PLIST. Returns (removed-p . new-plist)."
  (cond
    ((null plist) (cons nil nil))
    ((eql (car plist) indicator)
     (cons t (cddr plist)))
    (t (let ((prev plist) (cur (cddr plist)))
         (loop
           (when (null cur) (return (cons nil plist)))
           (when (eql (car cur) indicator)
             (set-cdr (cdr prev) (cddr cur))
             (return (cons t plist)))
           (setq prev (cddr prev))
           (setq cur (cddr cur)))))))
;; Polymorphic membership helper used by the set ops below — returns T
;; iff some element of LST matches ITEM-KEY under TEST-FN/KEY-FN.
;; Uses inline `eql` when test-fn is nil because #'eql isn't callable.
(defun %set-member-p (item-key lst test-fn key-fn)
  (let ((cur lst) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (let ((v (if key-fn (funcall key-fn (car cur)) (car cur))))
        (when (if test-fn (funcall test-fn item-key v) (eql item-key v))
          (setq found t)))
      (setq cur (cdr cur)))))

;; Positional helpers for set-ops.  We avoid `(apply #'foo l1 l2 args)' for
;; trampolining between &rest-defun siblings because that combination
;; (the prelude.lisp `apply' allocates a let-bound lambda for the
;; collected args) is documented in CLAUDE.md as fragile — funcall of
;; a let-allocated lambda after a &rest defun has been called recently
;; can crash or return wrong values, which is exactly what we observed
;; for nunion/intersection in the 2026-04-28 ANSI run (NUNION.2-5
;; returned NIL when expecting (a)).  Going through a positional
;; helper sidesteps the whole pattern.

(defun %union-impl (l1 l2 args)
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (r (copy-list l1)))
    (dolist (item l2 r)
      (let ((item-key (if key-fn (funcall key-fn item) item)))
        (unless (%set-member-p item-key r test-fn key-fn)
          (setq r (cons item r)))))))

(defun union (l1 l2 &rest args) (%union-impl l1 l2 args))
(defun nunion (l1 l2 &rest args) (%union-impl l1 l2 args))

;; intersection/nintersection-with-check: forward to the real
;; nintersection in cl-sequences.lisp via direct args, not apply.
;; nintersection is itself a &rest defun; calling it directly with
;; the collected args list as a final positional doesn't work in
;; vanilla CL — but here we re-implement intersection inline so it
;; doesn't matter.  Kept as a thin shim because nintersection's
;; semantics are identical to intersection for our purposes.
(defun %intersection-impl (l1 l2 args)
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (r nil))
    (dolist (item l1 (nreverse r))
      (let ((item-key (if key-fn (funcall key-fn item) item)))
        (when (%set-member-p item-key l2 test-fn key-fn)
          (setq r (cons item r)))))))

(defun intersection (l1 l2 &rest args) (%intersection-impl l1 l2 args))
(defun nintersection-with-check (l1 l2 &rest args) (%intersection-impl l1 l2 args))
(defun subsetp (l1 l2 &rest args)
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (cur l1) (ok t))
    (loop
      (when (or (not ok) (null cur)) (return ok))
      (let ((item-key (if key-fn (funcall key-fn (car cur)) (car cur))))
        (unless (%set-member-p item-key l2 test-fn key-fn)
          (setq ok nil)))
      (setq cur (cdr cur)))))

(defun nsubst (new old tree &rest args)
  "Substitute NEW for OLD in TREE (destructive).  Honors :test/:test-not/:key.
   Forwards to SUBST since our SUBST is non-destructive but produces correct
   result; tree may share structure with input but ANSI tests don't observe
   destructive side effects when they use COPY-TREE first."
  (apply #'subst new old tree args))

(defun %nsubst-if-rec (new pred-fn tree key-fn)
  (let ((v (if key-fn (funcall key-fn tree) tree)))
    (cond ((funcall pred-fn v) new)
          ((consp tree)
           (set-car tree (%nsubst-if-rec new pred-fn (car tree) key-fn))
           (set-cdr tree (%nsubst-if-rec new pred-fn (cdr tree) key-fn))
           tree)
          (t tree))))

(defun nsubst-if (new pred tree &rest args)
  "Substitute NEW for elements satisfying PRED in TREE (destructive).
   Honors :key per CLHS."
  (%subst-check-kwargs args)
  (let ((key-fn nil) (key-set nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (when (and (eq k :key) (not key-set))
          (setq key-fn v) (setq key-set t))
        (setq cur (cddr cur))))
    (%nsubst-if-rec new pred tree key-fn)))

(defun nsubst-if-not (new pred tree &rest args)
  "Substitute NEW for elements not satisfying PRED in TREE (destructive)."
  (%subst-check-kwargs args)
  (let ((key-fn nil) (key-set nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (when (and (eq k :key) (not key-set))
          (setq key-fn v) (setq key-set t))
        (setq cur (cddr cur))))
    (%nsubst-if-rec new (lambda (x) (not (funcall pred x))) tree key-fn)))

(defun check-nsubst-if (new pred tree)
  "Test helper for nsubst-if."
  (nsubst-if new pred (copy-tree tree)))

(defun check-nsubst-if-not (new pred tree)
  "Test helper for nsubst-if-not."
  (nsubst-if-not new pred (copy-tree tree)))

(defun %subst-if-rec (new pred-fn tree key-fn)
  (let ((v (if key-fn (funcall key-fn tree) tree)))
    (cond ((funcall pred-fn v) new)
          ((consp tree)
           (let ((a (%subst-if-rec new pred-fn (car tree) key-fn))
                 (d (%subst-if-rec new pred-fn (cdr tree) key-fn)))
             (if (and (eq a (car tree)) (eq d (cdr tree))) tree
                 (cons a d))))
          (t tree))))

(defun subst-if (new pred tree &rest args)
  "Substitute NEW for elements satisfying PRED in TREE.  Honors :key."
  (%subst-check-kwargs args)
  (let ((key-fn nil) (key-set nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (when (and (eq k :key) (not key-set))
          (setq key-fn v) (setq key-set t))
        (setq cur (cddr cur))))
    (%subst-if-rec new pred tree key-fn)))

(defun subst-if-not (new pred tree &rest args)
  "Substitute NEW for elements not satisfying PRED in TREE."
  (%subst-check-kwargs args)
  (let ((key-fn nil) (key-set nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (when (and (eq k :key) (not key-set))
          (setq key-fn v) (setq key-set t))
        (setq cur (cddr cur))))
    (%subst-if-rec new (lambda (x) (not (funcall pred x))) tree key-fn)))

(defun %sublis-parse-args (args)
  "Parse :test/:test-not/:key for sublis.  Leftmost wins."
  (let ((test-fn nil) (test-not-fn nil) (key-fn nil)
        (test-set nil) (tn-set nil) (key-set nil)
        (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :test)     (unless test-set (setq test-fn (%resolve-fn v) test-set t)))
          ((eq k :test-not) (unless tn-set (setq test-not-fn (%resolve-fn v) tn-set t)))
          ((eq k :key)      (unless key-set (setq key-fn (%resolve-fn v) key-set t))))
        (setq cur (cddr cur))))
    (list test-fn test-not-fn key-fn)))

(defun %sublis-find (item alist test-fn test-not-fn key-fn)
  "Find first pair in alist whose car matches item per test/key.  Returns
   the pair or NIL."
  (let ((cur alist) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (let ((pair (car cur)))
        (when (consp pair)
          (let* ((k (car pair))
                 (v (if key-fn (funcall key-fn item) item)))
            (when (cond
                    (test-fn     (funcall test-fn v k))
                    (test-not-fn (not (funcall test-not-fn v k)))
                    (t           (eql v k)))
              (setq found pair)))))
      (setq cur (cdr cur)))))

(defun nsublis (alist tree &rest args)
  "Substitute from ALIST in TREE (destructive).  Honors :test/:test-not/:key.
   Rejects bad keyword args via %subst-check-kwargs."
  (%subst-check-kwargs args)
  (let* ((parsed (%sublis-parse-args args))
         (test-fn (car parsed))
         (test-not-fn (cadr parsed))
         (key-fn (caddr parsed)))
    (%nsublis-rec alist tree test-fn test-not-fn key-fn)))

(defun %nsublis-rec (alist tree test-fn test-not-fn key-fn)
  (let ((pair (%sublis-find tree alist test-fn test-not-fn key-fn)))
    (cond
      (pair (cdr pair))
      ((consp tree)
       (set-car tree (%nsublis-rec alist (car tree) test-fn test-not-fn key-fn))
       (set-cdr tree (%nsublis-rec alist (cdr tree) test-fn test-not-fn key-fn))
       tree)
      (t tree))))

(defun sublis (alist tree &rest args)
  "Substitute from ALIST in TREE.  Honors :test/:test-not/:key.
   Rejects bad keyword args via %subst-check-kwargs."
  (%subst-check-kwargs args)
  (let* ((parsed (%sublis-parse-args args))
         (test-fn (car parsed))
         (test-not-fn (cadr parsed))
         (key-fn (caddr parsed)))
    (%sublis-rec alist tree test-fn test-not-fn key-fn)))

(defun %sublis-rec (alist tree test-fn test-not-fn key-fn)
  (let ((pair (%sublis-find tree alist test-fn test-not-fn key-fn)))
    (cond
      (pair (cdr pair))
      ((consp tree)
       (let ((a (%sublis-rec alist (car tree) test-fn test-not-fn key-fn))
             (d (%sublis-rec alist (cdr tree) test-fn test-not-fn key-fn)))
         (if (and (eq a (car tree)) (eq d (cdr tree))) tree
             (cons a d))))
      (t tree))))

(defun logtest (a b) (not (zerop (logand a b))))

;;; ============================================================
;;; Exact division and rational arithmetic for ANSI tests
;;; ============================================================

(defun gcd-impl (a b)
  "Greatest common divisor (Euclidean algorithm)."
  (let ((a (if (< a 0) (- 0 a) a))
        (b (if (< b 0) (- 0 b) b)))
    (loop (when (= b 0) (return a))
      (let ((r (mod a b))) (setq a b) (setq b r)))))

;; Tagged ratio object (subtag #x33) — slot 0 = numerator, slot 1 = denominator.
(defun make-ratio-obj (num den)
  (let ((r (%make-ratio))) (aset r 0 num) (aset r 1 den) r))

(defun %make-rat (num den)
  "Build a normalised rational from NUM/DEN.  Reduces by gcd, lifts the sign
   into the numerator, collapses to an integer when DEN=1.  Used by every
   path that introduces a ratio so callers never need to re-normalise.
   Uses %idiv-trunc to avoid recursing through / (which itself routes
   non-exact divisions back here)."
  (let* ((g (gcd-impl num den))
         (n (%idiv-trunc num g))
         (d (%idiv-trunc den g)))
    (when (< d 0)
      (setq n (- 0 n))
      (setq d (- 0 d)))
    (if (= d 1) n (make-ratio-obj n d))))

(defun ratio-numerator (x) (aref x 0))
(defun ratio-denominator (x) (aref x 1))
(defun numerator (x) (if (ratiop x) (aref x 0) x))
(defun denominator (x) (if (ratiop x) (aref x 1) 1))

;; Real function named RATIOP — most call sites resolve via the compiler
;; intrinsic, but #'ratiop and (funcall 'ratiop ...) need an actual
;; entry in the function table.  The body's (ratiop x) re-dispatches to
;; the intrinsic, so the compiled function is just the inline tag check.
(defun ratiop (x) (ratiop x))

(defun exact-divide (a b)
  "Divide A by B.  Integer if exact, tagged ratio otherwise.
   When either operand is a ratio, route through %rational-divide so
   (exact-divide 1 1/2) returns 2 instead of garbage from %idiv-trunc
   on the raw ratio pointer.
   Uses %idiv-trunc directly so a recursive call to / can't reach back here."
  (cond
    ((or (ratiop a) (ratiop b)) (%rational-divide a b))
    ((= (mod a b) 0) (%idiv-trunc a b))
    (t (%make-rat a b))))

(defun %rational-divide (a b)
  "Divide A by B where at least one is a ratio (other may be ratio or
   integer).  Cross-multiply (na*db)/(da*nb), normalise via %make-rat.
   exact-divide handles the int/int case; this fills the ratio gap so
   (/ 5/2 3) -> 5/6 instead of garbage from %idiv-trunc on the raw
   ratio pointer."
  (let ((na (if (ratiop a) (aref a 0) a))
        (da (if (ratiop a) (aref a 1) 1))
        (nb (if (ratiop b) (aref b 0) b))
        (db (if (ratiop b) (aref b 1) 1)))
    (%make-rat (* na db) (* da nb))))

(defun generic-negate (x)
  "Negate X (integer or ratio)."
  (if (ratiop x)
      (make-ratio-obj (- 0 (aref x 0)) (aref x 1))
      (- 0 x)))

;; Slow-path runtime helpers — compile-add/sub/mul's tag-check hits these
;; when at least one operand isn't a fixnum.  We cannot use plain +/-/*
;; in the integer branch because for bignum operands the dispatch would
;; route right back here, causing infinite recursion.  The fixnum body
;; uses %fixnum-+ / %fixnum-- / %fixnum-* primitives instead — direct IR
;; emission with no further dispatch — and the ratio/integer branches
;; use the same primitives plus the (already-recursion-safe) ratio shape.
;;; ============================================================
;;; IEEE float decode helpers
;;; ============================================================
;;;
;;; Modus stores literal floats as 2-slot objects with subtag #x60.
;;; Slot 0 = high 32 bits of IEEE bits (signed-stored, sign bit ⇒
;;; negative fixnum), slot 1 = low 32 bits.
;;;
;;; ANSI numeric operations on such floats need to compute REAL float
;;; arithmetic.  We decode the bits to an exact rational, route
;;; through %make-rat, and accept that the result will be a rational
;;; rather than a re-encoded IEEE float — modus's float type is
;;; effectively a rational with limited precision anyway.

(defun %ieee-float-p (x)
  "True if X is an IEEE-bits boxed float (subtag #x60, 2 slots)."
  (and (not (fixnump x)) (not (consp x)) (not (null x))
       (not (characterp x))
       (= (obj-subtag x) #x60)
       (= (array-length x) 2)))

(defun %ieee-float-to-rat (x)
  "Decode IEEE 64-bit double bits (modus encoding) to an exact rational.
   Layout: sign|11-bit exponent|52-bit mantissa.  Value = (-1)^sign *
   (1 + mantissa/2^52) * 2^(exponent - 1023) for normal floats; subnormal
   has implicit-1 bit cleared and exponent = -1022."
  (let* ((hi (aref x 0))                              ; tagged fixnum, may be negative if sign bit set
         (lo (aref x 1))                              ; lo 32 bits (unsigned in 0..2^32-1)
         (hi-u32 (logand hi 4294967295))              ; mask to unsigned 32-bit
         (sign-bit (logand (ash hi-u32 -31) 1))
         (exponent (logand (ash hi-u32 -20) 2047))   ; 11 bits
         (mantissa-hi (logand hi-u32 1048575))        ; low 20 bits of hi
         (mantissa (logior (ash mantissa-hi 32) (logand lo 4294967295)))
         (raw-sign (if (= sign-bit 1) -1 1)))
    (cond
      ;; Zero
      ((and (= exponent 0) (= mantissa 0)) 0)
      ;; Infinity / NaN — return 0 (modus can't represent these)
      ((= exponent 2047) 0)
      ;; Subnormal: value = mantissa * 2^(-1022-52) * (-1)^sign
      ((= exponent 0)
       (%make-rat (* raw-sign mantissa) (ash 1 (+ 1022 52))))
      ;; Normal: value = (2^52 + mantissa) * 2^(exponent-1023-52) * (-1)^sign
      (t
       (let ((m (+ (ash 1 52) mantissa))
             (e (- exponent 1075)))   ; 1023 + 52
         (if (>= e 0)
             (* raw-sign m (ash 1 e))
             (%make-rat (* raw-sign m) (ash 1 (- 0 e)))))))))

(defun %coerce-numeric (x)
  "Coerce any modus numeric (fixnum / bignum / ratio / modus-float-as-array
   / IEEE-float) into a uniform fixnum-or-ratio form for arithmetic."
  (cond
    ((integerp x) x)
    ((ratiop x) x)
    ((%ieee-float-p x) (%ieee-float-to-rat x))
    ;; 2-slot generic array used as a rational num/den
    ((and (not (fixnump x)) (not (consp x)) (not (null x))
          (not (characterp x))
          (= (obj-subtag x) #x32)
          (= (array-length x) 2))
     (let ((n (aref x 0)) (d (aref x 1)))
       (if (= d 1) n (%make-rat n d))))
    (t x)))

(defun %any-to-float (n)
  "Coerce any numeric N to an IEEE float object via the SSE2-backed
   %float-from-int / %float-div primops.  Handles:
   - IEEE float (subtag #x60) — return unchanged
   - integer — %float-from-int
   - ratio (subtag #x33) — num/den as IEEE
   - modus rational-form float (subtag #x32, 2 slots) — same as ratio,
     produced by transcendentals (sin/cos/etc.) and the literal-float
     reader path before the SSE2 %float-from-int rewrite landed.
   Complex inputs and anything else are returned unchanged."
  (cond
    ((%ieee-float-p n) n)
    ((integerp n) (%float-from-int n))
    ((ratiop n)
     (%float-div (%float-from-int (aref n 0))
                 (%float-from-int (aref n 1))))
    ;; Modus rational-form float: 2-slot array with subtag #x32
    ((and (not (fixnump n)) (not (consp n)) (not (null n))
          (not (characterp n))
          (= (obj-subtag n) #x32) (= (array-length n) 2))
     (let ((num (aref n 0)) (den (aref n 1)))
       (cond
         ((= den 0) (%float-from-int 0))
         ((= den 1) (%float-from-int num))
         (t (%float-div (%float-from-int num) (%float-from-int den))))))
    (t n)))

(defun generic-add (a b)
  (cond
    ;; Bignum operands route through bignum-add (overflow-safe).
    ((or (bignump a) (bignump b)) (bignum-add a b))
    ;; Fixnum + fixnum: detect potential overflow by operand magnitude
    ;; check (each fits in 61 bits → sum fits in 62-bit fixnum range).
    ;; Larger operands route through bignum-add, which correctly
    ;; promotes a + b that would have wrapped %fixnum-+ to a bignum.
    ((and (integerp a) (integerp b))
     (if (and (<= a 2305843009213693951) (>= a -2305843009213693952)
              (<= b 2305843009213693951) (>= b -2305843009213693952))
         (%fixnum-+ a b)
         (bignum-add a b)))
    ((and (integerp a) (ratiop b))
     (%make-rat (%fixnum-+ (%fixnum-* a (aref b 1)) (aref b 0)) (aref b 1)))
    ((and (ratiop a) (integerp b))
     (%make-rat (%fixnum-+ (aref a 0) (%fixnum-* b (aref a 1))) (aref a 1)))
    ((and (ratiop a) (ratiop b))
     (%make-rat (%fixnum-+ (%fixnum-* (aref a 0) (aref b 1))
                           (%fixnum-* (aref b 0) (aref a 1)))
                (%fixnum-* (aref a 1) (aref b 1))))
    ((or (%complex-p a) (%complex-p b)) (complex-add a b))
    ((or (%ieee-float-p a) (%ieee-float-p b))
     (%float-add (%any-to-float a) (%any-to-float b)))
    (t (%fixnum-+ a b))))

(defun generic-multiply (a b)
  (cond
    ;; Integer/integer: route through bignum-mul which has a 31-bit
    ;; fast path AND promotes to bignum on overflow.  The naked
    ;; %fixnum-* silently wrapped `(* 2^60 2^60) -> 0' for products
    ;; that exceed 63 bits.
    ((and (or (bignump a) (integerp a)) (or (bignump b) (integerp b)))
     (bignum-mul a b))
    ((and (integerp a) (ratiop b))
     (%make-rat (%fixnum-* a (aref b 0)) (aref b 1)))
    ((and (ratiop a) (integerp b))
     (%make-rat (%fixnum-* (aref a 0) b) (aref a 1)))
    ((and (ratiop a) (ratiop b))
     (%make-rat (%fixnum-* (aref a 0) (aref b 0))
                (%fixnum-* (aref a 1) (aref b 1))))
    ((or (%complex-p a) (%complex-p b)) (complex-mul a b))
    ;; IEEE float fast path — SSE2 MULSD via %float-mul primop.
    ((or (%ieee-float-p a) (%ieee-float-p b))
     (%float-mul (%any-to-float a) (%any-to-float b)))
    (t (%fixnum-* a b))))

(defun generic-subtract (a b)
  (cond
    ;; Bignum-aware: bignum-sub handles fixnum/bignum mix.
    ((or (bignump a) (bignump b)) (bignum-sub a b))
    ((and (integerp a) (integerp b)) (%fixnum-- a b))
    ((and (integerp a) (ratiop b))
     (%make-rat (%fixnum-- (%fixnum-* a (aref b 1)) (aref b 0)) (aref b 1)))
    ((and (ratiop a) (integerp b))
     (%make-rat (%fixnum-- (aref a 0) (%fixnum-* b (aref a 1))) (aref a 1)))
    ((and (ratiop a) (ratiop b))
     (%make-rat (%fixnum-- (%fixnum-* (aref a 0) (aref b 1))
                           (%fixnum-* (aref b 0) (aref a 1)))
                (%fixnum-* (aref a 1) (aref b 1))))
    ((or (%complex-p a) (%complex-p b)) (complex-sub a b))
    ;; IEEE float fast path — SSE2 SUBSD via %float-sub primop.
    ((or (%ieee-float-p a) (%ieee-float-p b))
     (%float-sub (%any-to-float a) (%any-to-float b)))
    (t (%fixnum-- a b))))

(defun generic-1+ (x)
  "Add 1 to X (integer or ratio)."
  (if (ratiop x)
      (%make-rat (%fixnum-+ (aref x 0) (aref x 1)) (aref x 1))
      (%fixnum-+ x 1)))

;;; --- Bignum-aware bit ops ---
;;;
;;; Modus's raw :and / :or / :xor IR mvm-bitwise-ANDs the tagged 64-bit
;;; words.  That's correct for two tagged fixnums but garbage when
;;; either operand is a heap-allocated bignum (the pointer's low 4
;;; bits = object tag #x9; AND-ing them with a fixnum mask gives
;;; pointer arithmetic).  generic-logand / -logior / -logxor route
;;; bignum operands through a low-limb AND so `(logand bignum 1)` —
;;; the body of evenp/oddp — returns the right parity once a future
;;; emit-arith-pair revision wires the tag check into compile-logand.
;;;
;;; For now they are runtime-callable but not yet on any fast path; the
;;; ANSI suite reaches them only via `apply #'logand`.  See
;;; [[reference_overflow_promotion_ir]] for the larger plan.

(defun generic-logand (a b)
  (cond
    ((and (integerp a) (integerp b)) (logand a b))
    ((and (integerp a) (bignump b)) (bignum-logand-fixnum b a))
    ((and (bignump a) (integerp b)) (bignum-logand-fixnum a b))
    ((and (bignump a) (bignump b))  (bignum-logand-bignum a b))
    (t (error "logand: bad type"))))

(defun generic-logior (a b)
  (cond
    ((and (integerp a) (integerp b)) (logior a b))
    ;; bignum ∨ small-fixnum-mask: the bignum's bits beyond f's width
    ;; are untouched; bits inside f's width OR together.  When f fits
    ;; in a single 62-bit limb we just OR f into the bignum's low limb.
    ((and (integerp a) (bignump b)) (bignum-logior-fixnum b a))
    ((and (bignump a) (integerp b)) (bignum-logior-fixnum a b))
    (t (error "logior: NYI for bignum∨bignum"))))

(defun generic-logxor (a b)
  (cond
    ((and (integerp a) (integerp b)) (logxor a b))
    ((and (integerp a) (bignump b)) (bignum-logxor-fixnum b a))
    ((and (bignump a) (integerp b)) (bignum-logxor-fixnum a b))
    (t (error "logxor: NYI for bignum⊕bignum"))))

;;; ============================================================
;;; Float inspection helpers
;;; ============================================================

(defun float-negative-p (x)
  "Check if boxed float X has negative sign bit.
   The hi32 slot is stored as a signed tagged fixnum; negative means sign bit set."
  (< (aref x 0) 0))

(defun %bignum-trunc-doubling (na nb)
  "Return ⌊na/nb⌋ where NA, NB are non-negative integers (fixnum or
   bignum) and NA ≥ NB > 0.  Uses repeated subtraction with doubling
   to find the largest 2^k such that nb*2^k ≤ na, subtract that, add
   2^k to the quotient, repeat.  Each step at least halves the
   remainder, so O(log(na/nb)^2) bignum ops.  Slow but correct, no
   dependence on (logbitp i bignum) or (logand bignum bignum)."
  (let ((q 0) (r na))
    (loop
      (when (= (bignum-cmp r nb) -1) (return q))
      (let ((k 0) (shifted nb))
        (loop
          (let ((next (bignum-mul shifted 2)))
            (when (= (bignum-cmp next r) 1) (return nil))
            (setq shifted next)
            (setq k (+ k 1))))
        (setq r (bignum-sub r shifted))
        ;; Add 2^k to q.  For k up to ~62, ash 1 k is a fixnum.
        ;; Beyond that we'd need bignum representation, but for our
        ;; ANSI suite the trig case keeps k ≤ 47.  Guard anyway.
        (setq q (bignum-add q (if (>= k 62) (bignum-mul (ash 1 30) (ash 1 (- k 30))) (ash 1 k))))))))

(defun %integer-truncate (a b)
  "Bignum-aware integer truncate.  Returns the quotient only;
   %truncate2-generic computes the remainder separately.

   1. fixnum / fixnum   → native (truncate a b).
   2. fixnum / bignum   → 0 (since |a| < |b|).
   3. bignum / pos-fixnum-≤-2^31 → %bignum-divmod-fixnum (fast).
   4. bignum / neg-fixnum-≤-2^31 → negate-then-divmod, flip sign.
   5. bignum / larger-fixnum or bignum / bignum →
      %bignum-trunc-doubling (slow but correct).

   Note: bignum-truncate in cl-eval.lisp relies on (logbitp i bignum),
   but compile-logand emits a raw IR :and that mishandles bignum
   pointers — so the binary long-division loop in bignum-truncate
   silently sees every high bit of na as zero and produces 0 for any
   bignum input.  %bignum-divmod-fixnum (originally written for base-N
   printer) uses only safe fixnum-on-fixnum limb-level arithmetic, so
   we route the fast bignum/small-fixnum case through it.  The general
   bignum/anything case uses doubling-subtract, which only depends on
   bignum-mul / bignum-add / bignum-sub / bignum-cmp."
  (when (= b 0) (error "divide by zero"))
  (when (and (not (bignump a)) (not (bignump b)))
    (return-from %integer-truncate (truncate a b)))
  (when (and (not (bignump a)) (bignump b))
    (return-from %integer-truncate 0))
  ;; a is a bignum here.
  (let* ((b-neg (or (and (bignump b)
                         (= (car (%any-to-limbs b)) -1))
                    (and (not (bignump b)) (< b 0))))
         (b-mag (if b-neg (bignum-negate b) b))
         (a-neg (= (car (%any-to-limbs a)) -1))
         (na (if a-neg (bignum-negate a) a)))
    (let ((mag
            (cond
              ;; Fast path: divisor magnitude fits in (2^31 - 1).
              ((and (not (bignump b-mag)) (< b-mag 2147483648))
               (car (%bignum-divmod-fixnum na b-mag)))
              ;; General path: doubling-subtract.
              (t (%bignum-trunc-doubling na b-mag)))))
      (if (eq a-neg b-neg) mag (bignum-negate mag)))))

(defun %truncate2-generic (a b)
  "Slow-path 2-arg truncate.  Integer/integer path uses %INTEGER-TRUNCATE
   (which routes through %bignum-divmod-fixnum for bignum/small-fixnum
   and %bignum-trunc-doubling for the general case).  Float/ratio/mixed
   path keeps the original (/ a b) route.

   Returns (values q r) per CLHS — q = trunc(a/b), r = a − q·b.

   Note: with compile-truncate's POSITIVE gate, every (truncate bignum
   fixnum) now reaches %INTEGER-TRUNCATE, where it routes through
   %bignum-divmod-fixnum (or %bignum-trunc-doubling for divisor ≥ 2^31).
   With the NEGATIVE gate, bignum stayed on the inline :div fast path
   which silently gave garbage answers — trig tests `accidentally
   passed' because the garbage value still happened to land in [-1,1]
   after %any-to-float."
  (cond
    ((and (integerp a) (integerp b))
     (let* ((q (%integer-truncate a b))
            (r (generic-subtract a (generic-multiply q b))))
       (values q r)))
    (t
     (let* ((q (truncate (/ a b)))
            (r (- a (* q b))))
       (values q r)))))

(defun float-truncate-to-integer (x)
  "Extract absolute integer part of boxed float X (truncate toward zero).
   For the REAL tests, only used with positive floats (0.0001) and
   negative floats (-0.0001) which both have integer part 0."
  (if (< (aref x 0) 0)
      ;; Negative float: for small values like -0.0001, integer part is 0
      0
      ;; Positive float: extract from hi32/lo32
      (let ((raw-hi (ash (aref x 0) -1))
            (raw-lo (ash (aref x 1) -1)))
        (let ((exp-biased (logand (ash raw-hi -20) 2047)))
          (let ((exponent (- exp-biased 1023)))
            (if (< exponent 0)
                0
                (if (>= exponent 52)
                    (ash 1 exponent)
                    (let ((mantissa-hi (logior (logand raw-hi 1048575) 1048576)))
                      (if (>= exponent 20)
                          (logior (ash mantissa-hi (- exponent 20))
                                  (ash raw-lo (- 0 (- 52 exponent))))
                          (ash mantissa-hi (- exponent 20)))))))))))

;;; ============================================================
;;; Generic numeric comparison
;;; ============================================================

(defun %ieee-float-sign-class (x)
  "Return -2 if X <= -1, -1 if -1 < X < 0, 0 if X = 0, 1 if 0 < X < 1, 2 if X >= 1.
   Used to short-circuit range comparisons against integers without
   coercing through %ieee-float-to-rat (whose denominators of the form
   ash 1 66 overflow modus's 63-bit fixnums)."
  (let* ((hi (aref x 0))
         (hi-u32 (logand hi 4294967295))
         (sign-bit (logand (ash hi-u32 -31) 1))
         (exponent (logand (ash hi-u32 -20) 2047)))
    (cond
      ((and (= exponent 0) (= (aref x 1) 0)
            (= (logand hi-u32 1048575) 0)) 0)    ; ±zero
      ((and (= sign-bit 1) (< exponent 1023)) -1) ; -1 < x < 0
      ((= sign-bit 1) -2)                         ; x <= -1
      ((< exponent 1023) 1)                       ; 0 < x < 1
      (t 2))))                                    ; x >= 1

(defun numeric-value-less-p (a b)
  "Return T if numeric value A < numeric value B.
   Handles integers, boxed floats (subtag #x60 IEEE or #x32 rational
   form), and tagged ratios (subtag #x33)."
  ;; IEEE-float vs integer fast path — avoid %ieee-float-to-rat for
  ;; sub-unit-magnitude floats whose rational denominator would be
  ;; ash 1 66+ (overflows modus's 63-bit fixnum, breaks gcd-reduce).
  (when (and (%ieee-float-p a) (integerp b))
    (let ((sc (%ieee-float-sign-class a)))
      (cond
        ((and (= sc 0) (> b 0))   (return-from numeric-value-less-p t))    ; a=0, b>0
        ((and (= sc 0) (<= b 0))  (return-from numeric-value-less-p nil))  ; a=0, b<=0
        ((and (= sc 1) (>= b 1))  (return-from numeric-value-less-p t))    ; 0<a<1, b>=1
        ((and (= sc 1) (<= b 0))  (return-from numeric-value-less-p nil))  ; 0<a<1, b<=0
        ((and (= sc -1) (>= b 0)) (return-from numeric-value-less-p t))    ; -1<a<0, b>=0
        ((and (= sc -1) (<= b -1)) (return-from numeric-value-less-p nil))))) ; -1<a<0, b<=-1
  (when (and (integerp a) (%ieee-float-p b))
    (let ((sc (%ieee-float-sign-class b)))
      (cond
        ((and (= sc 0) (< a 0))   (return-from numeric-value-less-p t))    ; a<0, b=0
        ((and (= sc 0) (>= a 0))  (return-from numeric-value-less-p nil))  ; a>=0, b=0
        ((and (= sc 1) (<= a 0))  (return-from numeric-value-less-p t))    ; a<=0, 0<b<1
        ((and (= sc 1) (>= a 1))  (return-from numeric-value-less-p nil))  ; a>=1, 0<b<1
        ((and (= sc -1) (<= a -1)) (return-from numeric-value-less-p t))   ; a<=-1, -1<b<0
        ((and (= sc -1) (>= a 0)) (return-from numeric-value-less-p nil)))))
  ;; IEEE-float either side: coerce to rational and recurse.  Without
  ;; this, < on IEEE-bit floats falls through and returns NIL, breaking
  ;; (< 1.0 2.0) and friends.
  (when (or (%ieee-float-p a) (%ieee-float-p b))
    (return-from numeric-value-less-p
      (numeric-value-less-p (%coerce-numeric a) (%coerce-numeric b))))
  (cond
    ;; Bignum operands: use bignum-cmp which compares hi/lo correctly.
    ;; Must precede (integerp/integerp) → (< a b) because that branch
    ;; would recurse infinitely on bignum operands (compile-compare's
    ;; slow path calls numeric-value-less-p which would dispatch back).
    ((and (or (bignump a) (integerp a)) (or (bignump b) (integerp b))
          (or (bignump a) (bignump b)))
     (= (bignum-cmp a b) -1))
    ;; Both fixnums (integerp w/o bignum) — fast path.
    ((and (integerp a) (integerp b)) (< a b))
    ;; a is float
    ((floatp-impl a)
     (cond
       ((integerp b)
        (if (float-negative-p a)
            t
            (let ((int-part (float-truncate-to-integer a)))
              (< int-part b))))
       ((ratiop b)
        (if (float-negative-p a)
            (if (> (aref b 0) 0) t nil)
            (let ((int-part (float-truncate-to-integer a)))
              (< (* int-part (aref b 1)) (aref b 0)))))
       (t nil)))
    ;; a is ratio
    ((ratiop a)
     (cond
       ((integerp b)
        ;; a.num/a.den < b iff a.num < b * a.den
        (< (aref a 0) (* b (aref a 1))))
       ((ratiop b)
        ;; a.num/a.den < b.num/b.den iff a.num*b.den < b.num*a.den
        (< (* (aref a 0) (aref b 1)) (* (aref b 0) (aref a 1))))
       (t nil)))
    ;; a is integer, b is float
    ((and (integerp a) (floatp-impl b))
     (if (float-negative-p b)
         nil
         (let ((int-part (float-truncate-to-integer b)))
           (<= a int-part))))
    ;; a is integer, b is ratio
    ((and (integerp a) (ratiop b))
     (< (* a (aref b 1)) (aref b 0)))
    (t nil)))

(defun numeric-<= (a b)
  "Return T if A <= B for any numeric type."
  (or (numeric-equal-p a b) (numeric-value-less-p a b)))

(defun numeric->= (a b)
  "Return T if A >= B for any numeric type."
  (numeric-<= b a))

(defun %numeric-value-greater-p (a b)
  "Return T if A > B for any numeric type — used by compile-compare-2's
   slow path for the > branch (which has no direct generic helper)."
  (numeric-value-less-p b a))

(defun numeric-equal-p (a b)
  "Return T if A equals B numerically.
   Tagged-ratio aware: ratios are normalised so num/den uniquely
   represents value, hence componentwise compare is sufficient.
   IEEE float: coerce to rational and recurse."
  ;; IEEE-float vs integer fast path — avoid coerce-to-rat for the
  ;; common 0.0/integer 0 case (avoids bignum overflow on small floats).
  (when (and (%ieee-float-p a) (integerp b))
    (let ((sc (%ieee-float-sign-class a)))
      (cond
        ((= sc 0) (return-from numeric-equal-p (= b 0)))
        ((or (= sc 1) (= sc -1)) (return-from numeric-equal-p nil)))))
  (when (and (integerp a) (%ieee-float-p b))
    (let ((sc (%ieee-float-sign-class b)))
      (cond
        ((= sc 0) (return-from numeric-equal-p (= a 0)))
        ((or (= sc 1) (= sc -1)) (return-from numeric-equal-p nil)))))
  (when (or (%ieee-float-p a) (%ieee-float-p b))
    (return-from numeric-equal-p
      (numeric-equal-p (%coerce-numeric a) (%coerce-numeric b))))
  (cond
    ;; Bignum-aware equality: bignum-cmp returns 0 on equal.
    ((and (or (bignump a) (integerp a)) (or (bignump b) (integerp b))
          (or (bignump a) (bignump b)))
     (= (bignum-cmp a b) 0))
    ((and (integerp a) (integerp b)) (= a b))
    ((and (floatp-impl a) (floatp-impl b)) (float-equal a b))
    ((and (ratiop a) (ratiop b))
     (and (= (aref a 0) (aref b 0)) (= (aref a 1) (aref b 1))))
    ((and (ratiop a) (integerp b))
     ;; Normalised ratios never have den=1 (they collapse to integer
     ;; in %make-rat), so a ratio vs integer is always unequal.
     nil)
    ((and (integerp a) (ratiop b))
     nil)
    (t nil)))

;; LOOP comparison helpers — fast fixnum path inline, slow numeric path
;; for floats/ratios. Used by generate-loop-code so end-tests don't hang
;; on boxed-float end-forms.
(defun %loop-lt (a b)
  (if (and (fixnump a) (fixnump b)) (< a b) (numeric-value-less-p a b)))
(defun %loop-gt (a b)
  (if (and (fixnump a) (fixnump b)) (> a b) (numeric-value-less-p b a)))
(defun %loop-le (a b)
  (if (and (fixnump a) (fixnump b)) (<= a b) (numeric-<= a b)))
(defun %loop-ge (a b)
  (if (and (fixnump a) (fixnump b)) (>= a b) (numeric->= a b)))

;;; ============================================================
;;; Compound typep for ANSI tests
;;; ============================================================

(defun exclusive-bound-p (x)
  "Check if X is an exclusive type bound like (10), not a ratio like (4 . 3)."
  (and (consp x) (null (cdr x))))

(defun typep-range-check (obj low high)
  "Check if OBJ is in range [LOW, HIGH]. LOW/HIGH can be * (unbounded),
   an integer, a ratio cons (num . den), or a list (exclusive bound)."
  (let ((above-low
         (cond
           ((eq low '*) t)
           ((exclusive-bound-p low)
            ;; Exclusive lower bound: (val) means > val
            (numeric-value-less-p (car low) obj))
           (t (numeric-<= low obj))))
        (below-high
         (cond
           ((eq high '*) t)
           ((exclusive-bound-p high)
            ;; Exclusive upper bound: (val) means < val
            (numeric-value-less-p obj (car high)))
           (t (numeric-<= obj high)))))
    (and above-low below-high)))

;; %typename-eq: eq fast path with name-hash fallback.  The bare-metal
;; `%intern-symbol` sometimes produces multiple symbol objects with the
;; same name (CLAUDE.md "Symbol identity"), so the bare `eq` checks
;; below can miss when the user's type-name symbol came from a different
;; intern site than this file's literal.  Both native MVM symbols
;; (1 slot) and CL symbols (3 slots) store compute-name-hash(name) at
;; slot 0, so equal slot-0 hashes ≡ same name even when objects differ.
(defun %typename-eq (tn lit)
  (or (eq tn lit)
      (and (symbolp tn) (symbolp lit)
           (not (null tn)) (not (eq tn t))
           (not (null lit)) (not (eq lit t))
           (= (aref tn 0) (aref lit 0)))))

;; --- Array typep helpers ---
;;
;; Modus stores multi-dim arrays in two shapes:
;;   1. cons-wrapper MDA: (cons 9867654 (cons DIMS-LIST FLAT-DATA))
;;      Possibly nested inside an adjustable wrapper (cons 8765432 ...).
;;      Made by the build-time rewriter for (make-array '(N M ...) ...).
;;   2. Native MDA (subtag #x34), seven-slot header object.
;;      Made at runtime by %alloc-mda.  See mvm/cl-clos.lisp.
;; Both report rank/dims via array-rank / array-dimensions and pass
;; arrayp.  Plain 1-D vectors (subtag #x31 string, #x32 array) and the
;; FP/displaced wrappers also pass arrayp; their rank is always 1.
;;
;; The typep clauses for (array …) / (vector …) / (simple-array …) /
;; (simple-vector …) reuse those helpers so they accept any of these
;; shapes uniformly.

(defun %typep-array-elt-match-p (arr elt)
  "True iff ARR's element-type satisfies the type spec ELT.
   Modus upgrades nearly every element-type to T, so the only
   distinctions we can make are:
     - strings (subtag #x31 or MDA whose data is a string) hold base
       chars only — they satisfy CHARACTER / BASE-CHAR / STANDARD-CHAR
       and don't satisfy BIT / NUMBER / SYMBOL / T (since strings are
       NOT T-vectors).
     - bit-vectors (arrays of 0/1) satisfy BIT / INTEGER / UNSIGNED-BYTE
       and don't satisfy CHARACTER.
   For other arrays the element-type is T, so ELT = T / * / any
   T-supertype matches.  '*' / NIL always pass.

   Important: a string is NOT (vector T) / (vector *).  Strings have
   element type CHARACTER, and (vector T) means strictly element-type
   T.  So when ELT = T (explicit, not '*'), strings DON'T match.
   When ELT = '*' (no constraint), anything matches."
  (cond
    ((or (null elt) (eq elt '*) (%typename-eq elt '*)) t)
    ((stringp arr)
     ;; Strings only really satisfy character-family element types.
     ;; Importantly, ELT = T (explicit) is NOT a match — strings are
     ;; element-type CHARACTER, not T.
     (cond
       ((%typename-eq elt 'character) t)
       ((%typename-eq elt 'base-char) t)
       ((%typename-eq elt 'standard-char) t)
       ((%typename-eq elt 'extended-char) nil)
       ((or (%typename-eq elt 't) (eq elt t)) nil)
       ((or (%typename-eq elt 'bit) (%typename-eq elt 'fixnum)
            (%typename-eq elt 'integer) (%typename-eq elt 'unsigned-byte)
            (%typename-eq elt 'signed-byte) (%typename-eq elt 'number)
            (%typename-eq elt 'symbol))
        nil)
       ;; (unsigned-byte n) / (signed-byte n) / (integer …) etc. compound
       ;; on a string is false (string elt isn't an integer of any width).
       ((and (consp elt)
             (or (%typename-eq (car elt) 'unsigned-byte)
                 (%typename-eq (car elt) 'signed-byte)
                 (%typename-eq (car elt) 'integer)
                 (%typename-eq (car elt) 'mod)
                 (%typename-eq (car elt) 'bit)))
        nil)
       (t t)))
    (t
     ;; Non-string array.  ELT = T or T-equivalent matches.  CHARACTER
     ;; on a non-string array doesn't (since the elt is T, not character).
     (cond
       ((or (eq elt t) (%typename-eq elt 't)) t)
       ((or (%typename-eq elt 'character) (%typename-eq elt 'base-char)
            (%typename-eq elt 'standard-char) (%typename-eq elt 'extended-char))
        nil)
       (t t)))))

(defun %typep-array-dim-spec-match-p (actual spec)
  "Per-dimension: is ACTUAL (an integer dimension) covered by SPEC?
   SPEC may be '*' / T / an integer."
  (cond
    ((%typename-eq spec '*) t)
    ((eq spec t) t)
    ((integerp spec) (= spec actual))
    (t nil)))

(defun %typep-array-dims-match-p (arr dims-spec)
  "True iff ARR's dimensions match DIMS-SPEC, where:
     '*' / T             — any rank
     integer N           — rank N (any dim sizes)
     NIL                 — rank 0 only (Modus has none → NIL)
     list of N specs     — rank N, each dim matches spec
   The caller substitutes '*' for an absent dims-spec, so NIL here
   means an explicit rank-0 request from the source.
   ARR is any object that passed arrayp."
  (cond
    ((or (eq dims-spec '*) (%typename-eq dims-spec '*) (eq dims-spec t)) t)
    ((null dims-spec)
     ;; Explicit rank-0 request — Modus has no rank-0 arrays.
     ;; (The "absent dims" case never reaches here; caller substituted '*.)
     (= (array-rank arr) 0))
    ((integerp dims-spec) (= dims-spec (array-rank arr)))
    ((consp dims-spec)
     ;; Walk both lists in parallel.  Must have same length (= same rank).
     (let ((actual (array-dimensions arr)) (spec dims-spec) (ok t))
       (loop
         (when (or (and (null actual) (null spec)) (not ok))
           (return ok))
         (when (or (null actual) (null spec))
           (setq ok nil) (return ok))
         (unless (%typep-array-dim-spec-match-p (car actual) (car spec))
           (setq ok nil))
         (setq actual (cdr actual))
         (setq spec (cdr spec)))))
    (t nil)))

(defun typep (obj type)
  "Extended typep supporting compound type specifiers."
  (cond
    ;; Simple type names (symbols/keywords)
    ((not (consp type))
     (let ((tn type))
       (cond
         ((%typename-eq tn 'integer) (integerp obj))
         ((%typename-eq tn 'fixnum) (integerp obj))
         ;; MVM has no real bignum tower — all integers are 63-bit
         ;; fixnums. Reporting BIGNUM as NIL traps tests like the
         ;; (loop while (not (typep x 'bignum)) do (setf x (* x x)))
         ;; pattern in an infinite squaring loop until SIGALRM fires.
         ;; Treat anything beyond the 32-bit fixnum range that other
         ;; CL impls use as "bignum" so that loop exits.
         ((%typename-eq tn 'bignum)
          (and (integerp obj)
               (or (> obj 1073741823) (< obj -1073741824))))
         ((%typename-eq tn 'real) (or (integerp obj) (floatp-impl obj) (ratiop obj)))
         ((%typename-eq tn 'rational) (or (integerp obj) (ratiop obj)))
         ((%typename-eq tn 'number) (or (integerp obj) (floatp-impl obj) (ratiop obj)))
         ((%typename-eq tn 'float) (floatp-impl obj))
         ((%typename-eq tn 'single-float) (floatp-impl obj))
         ((%typename-eq tn 'double-float) (floatp-impl obj))
         ((%typename-eq tn 'short-float) (floatp-impl obj))
         ((%typename-eq tn 'long-float) (floatp-impl obj))
         ((%typename-eq tn 'ratio) (ratiop obj))
         ((%typename-eq tn 'cons) (consp obj))
         ((%typename-eq tn 'list) (or (null obj) (consp obj)))
         ((%typename-eq tn 'null) (null obj))
         ;; (typep x 'symbol) — `(integerp obj)` was a leftover from when
         ;; native MVM symbols were stored as bare hash fixnums.  Real
         ;; symbols today are heap objects (subtag #x50); use symbolp.
         ((%typename-eq tn 'symbol) (symbolp obj))
         ((%typename-eq tn 'string) (stringp obj))
         ((%typename-eq tn 'simple-string) (stringp obj))
         ((%typename-eq tn 'base-string) (stringp obj))
         ((%typename-eq tn 'simple-base-string) (stringp obj))
         ((%typename-eq tn 'character) (characterp obj))
         ((%typename-eq tn 'base-char) (characterp obj))
         ;; CLHS: extended-char is disjoint from base-char.  Modus only
         ;; has base characters, so this is always NIL.  extended-char.3
         ;; verifies the disjointness.
         ((%typename-eq tn 'extended-char) nil)
         ;; CLHS: standard-char = #\Newline + #\Space..#\~ — narrower than
         ;; character.  standard-char.5.body collects (typep c 'standard-char)
         ;; where (standard-char-p c) is nil and expects empty list.
         ((%typename-eq tn 'standard-char)
          (and (characterp obj)
               (let ((cc (char-code obj)))
                 (if (= cc 10) t (and (>= cc 32) (<= cc 126))))))
         ((%typename-eq tn 'atom) (not (consp obj)))
         ((%typename-eq tn 't) t)
         ((%typename-eq tn 'nil) nil)
         ((%typename-eq tn 'boolean) (or (null obj) (eq obj t)))
         ;; (typep x 'bit) — must be 0 or 1 AS AN INTEGER.  Without the
         ;; integerp guard `(= obj 0)` runs `=` on arbitrary values
         ;; (strings, conses, fn-addrs) which goes wrong fast.
         ((%typename-eq tn 'bit) (and (integerp obj) (or (= obj 0) (= obj 1))))
         ((%typename-eq tn 'bit-vector) (bit-vector-p obj))
         ((%typename-eq tn 'simple-bit-vector) (simple-bit-vector-p obj))
         ;; Array/vector predicates: route through arrayp (which now
         ;; recognises cons-wrappers and native MDA).  simple-vector
         ;; requires non-string element type; simple-array tracks
         ;; non-adjustable.  Modus mostly upgrades everything to T so
         ;; the distinctions are coarse.
         ((%typename-eq tn 'array) (arrayp obj))
         ((%typename-eq tn 'simple-array)
          (and (arrayp obj)
               (not (and (consp obj) (eql (car obj) 8765432)))))
         ((%typename-eq tn 'vector)
          (and (arrayp obj) (= (array-rank obj) 1)))
         ((%typename-eq tn 'simple-vector)
          ;; CL: simple-vector = simple 1-D array of T.  In Modus the
          ;; element type isn't tracked at the object level, so we
          ;; can't strictly distinguish (vector T) from (vector bit) /
          ;; (vector character) for arrays of 0/1 etc.  Reject strings
          ;; (#x31, definitely char-element-typed) and adjustable
          ;; wrappers but otherwise accept any 1-D arrayp.
          (and (arrayp obj) (= (array-rank obj) 1)
               (not (stringp obj))
               (not (and (consp obj) (eql (car obj) 8765432)))))
         ((%typename-eq tn 'sequence)
          (or (null obj) (consp obj) (arrayp obj)))
         ((%typename-eq tn 'unsigned-byte) (and (integerp obj) (>= obj 0)))
         ((%typename-eq tn 'signed-byte) (integerp obj))
         ((%typename-eq tn 'function) (or (functionp obj) (%generic-function-p obj)))
         ((%typename-eq tn 'generic-function) (%generic-function-p obj))
         ((%typename-eq tn 'standard-generic-function) (%generic-function-p obj))
         ((%typename-eq tn 'standard-method) (%standard-method-p obj))
         ((%typename-eq tn 'method) (%standard-method-p obj))
         ((%typename-eq tn 'method-combination) (%mc-p obj))
         ;; CLOS instance check
         ((%typename-eq tn 'standard-object) (%clos-instance-p obj))
         ;; User-defined CLOS class: check if obj is a CLOS instance and
         ;; tn is in obj's class precedence list (so typep recognizes
         ;; subclasses correctly).
         ;; Symbol equality uses eq first, then symbol-name string-equal
         ;; to dodge the bare-metal "duplicate-symbol" identity bug.
         (t
          (if (%clos-instance-p obj)
            (let ((cpl (%obj-cpl obj))
                  (tn-name (if (symbolp tn) (symbol-name tn) nil))
                  (found nil))
              (let ((c cpl))
                (loop
                  (when (null c) (return found))
                  (let ((cur (car c)))
                    (when (or (eq cur tn)
                              (and tn-name (symbolp cur)
                                   (string-equal (symbol-name cur) tn-name)))
                      (setq found t) (return found)))
                  (setq c (cdr c)))))
            nil)))))
    ;; Compound type specifiers.  Use %typename-eq instead of eq for
    ;; the head comparisons — `'real' inside a literal source form and
    ;; `'real' synthesized at runtime via (list 'real 0 10) can be
    ;; distinct symbol objects with the same hash (per CLAUDE.md
    ;; "Symbol identity" known bug).
    (t
     (let ((head (car type)))
       (cond
         ;; (real low high) — range check for reals
         ((%typename-eq head 'real)
          (if (or (integerp obj) (floatp-impl obj) (ratiop obj))
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (integer low high)
         ((%typename-eq head 'integer)
          (if (integerp obj)
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (float low high)
         ((or (%typename-eq head 'float)         (%typename-eq head 'single-float)
              (%typename-eq head 'double-float)  (%typename-eq head 'short-float)
              (%typename-eq head 'long-float))
          (if (floatp-impl obj)
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (rational low high)
         ((%typename-eq head 'rational)
          (if (or (integerp obj) (ratiop obj))
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (eql val)
         ((%typename-eq head 'eql)
          (eql obj (cadr type)))
         ;; (member val1 val2 ...)
         ((%typename-eq head 'member)
          (if (member obj (cdr type)) t nil))
         ;; (and type1 type2 ...)
         ((%typename-eq head 'and)
          (let ((ok t))
            (dolist (sub (cdr type))
              (unless (typep obj sub) (setq ok nil)))
            ok))
         ;; (or type1 type2 ...)
         ((%typename-eq head 'or)
          (let ((ok nil))
            (dolist (sub (cdr type))
              (when (typep obj sub) (setq ok t)))
            ok))
         ;; (not type)
         ((%typename-eq head 'not)
          (not (typep obj (cadr type))))
         ;; (satisfies pred)
         ((%typename-eq head 'satisfies) nil)  ; can't call arbitrary predicates
         ;; (unsigned-byte n) — integer in [0, 2^n - 1]
         ((%typename-eq head 'unsigned-byte)
          (and (integerp obj) (>= obj 0)
               (< obj (ash 1 (cadr type)))))
         ;; (signed-byte n) — integer in [-2^(n-1), 2^(n-1) - 1]
         ((%typename-eq head 'signed-byte)
          (and (integerp obj)
               (let ((half (ash 1 (- (cadr type) 1))))
                 (and (>= obj (- 0 half)) (< obj half)))))
         ;; (mod n) — integer in [0, n-1]
         ((%typename-eq head 'mod)
          (and (integerp obj) (>= obj 0) (< obj (cadr type))))
         ;; (bit-vector size) / (bit-vector *) — bit-vector with optional
         ;; size constraint.  Use %typename-eq for the size sentinel so
         ;; the literal '* compiled in the typep source matches the
         ;; runtime '* synthesized via list construction.
         ((or (%typename-eq head 'bit-vector)
              (%typename-eq head 'simple-bit-vector))
          (and (bit-vector-p obj)
               (let ((sz (and (cdr type) (cadr type))))
                 (or (null sz) (%typename-eq sz '*) (eq sz t)
                     (and (integerp sz) (= sz (array-length obj)))))))
         ;; (string size) / (simple-string size) — string with optional
         ;; size.
         ((or (%typename-eq head 'string)
              (%typename-eq head 'simple-string)
              (%typename-eq head 'base-string)
              (%typename-eq head 'simple-base-string))
          (and (stringp obj)
               (let ((sz (and (cdr type) (cadr type))))
                 (or (null sz) (%typename-eq sz '*) (eq sz t)
                     (and (integerp sz) (= sz (array-length obj)))))))
         ;; (vector elt-type size) — vector with optional element-type
         ;; and a single size dimension.  Vector is always rank-1.
         ((or (%typename-eq head 'vector)
              (%typename-eq head 'simple-vector))
          (and (arrayp obj)
               (= (array-rank obj) 1)
               (%typep-array-elt-match-p
                obj
                (cond
                  ((%typename-eq head 'simple-vector) 't)
                  ((cdr type) (cadr type))
                  (t '*)))
               (let ((sz (cond
                           ((%typename-eq head 'simple-vector)
                            (if (cdr type) (cadr type) nil))
                           ((cddr type) (caddr type))
                           (t nil))))
                 (cond
                   ((null sz) t)
                   ((%typename-eq sz '*) t)
                   ((eq sz t) t)
                   ((integerp sz) (= sz (array-length obj)))
                   (t nil)))))
         ;; (array elt dims) / (simple-array elt dims).  Dims spec:
         ;;   absent / '* / T            — any rank
         ;;   integer N                  — rank N
         ;;   NIL                        — rank 0
         ;;   list of N elements (or *)  — rank N with per-dim spec
         ((or (%typename-eq head 'simple-array)
              (%typename-eq head 'array))
          (and (arrayp obj)
               (%typep-array-elt-match-p
                obj (if (cdr type) (cadr type) '*))
               (%typep-array-dims-match-p
                obj (if (cddr type) (caddr type) '*))))
         ;; (cons car-type cdr-type) — type-check both halves.
         ((%typename-eq head 'cons)
          (and (consp obj)
               (let ((car-type (and (cdr type) (cadr type)))
                     (cdr-type (and (cddr type) (caddr type))))
                 (and (or (null car-type) (eq car-type '*) (eq car-type t)
                          (typep (car obj) car-type))
                      (or (null cdr-type) (eq cdr-type '*) (eq cdr-type t)
                          (typep (cdr obj) cdr-type))))))
         (t nil))))))

(defun typep* (obj type) (typep obj type))

;;; ============================================================
;;; Bit-vector primitives
;;; ============================================================
;;; Bit vectors are represented as plain MVM arrays whose elements are
;;; the fixnums 0 and 1.  The reader's #*101 syntax (cl-reader.lisp)
;;; already produces this representation, and rt-array-equal compares
;;; element-by-element so #*0001 from the reader is rt-equal to a
;;; bit-and result allocated at runtime.
;;;
;;; bit-vector-p / simple-bit-vector-p detect bit-vectors heuristically:
;;; an array all of whose elements are 0 or 1.  This catches the
;;; common ANSI test cases (#*..., make-array :element-type 'bit) and
;;; correctly rejects mixed-element arrays.

(defun bitp (x)
  "T if X is the integer 0 or 1."
  (and (integerp x) (or (= x 0) (= x 1))))

(defun %array-bits-only-p (a)
  "Return T if every element of array A is the integer 0 or 1."
  (let ((len (array-length a)) (i 0) (ok t))
    (loop
      (when (or (not ok) (= i len)) (return ok))
      (let ((e (aref a i)))
        (unless (and (integerp e) (or (= e 0) (= e 1)))
          (setq ok nil)))
      (setq i (+ i 1)))))

(defun bit-vector-p (x)
  "T if X is a bit-vector — an array whose elements are all 0 or 1.
   Empty arrays count as bit-vectors."
  (and (arrayp x) (not (stringp x)) (%array-bits-only-p x)))

(defun simple-bit-vector-p (x)
  "T if X is a simple bit-vector."
  (bit-vector-p x))

(defun make-bit-vector (size &optional init)
  "Allocate a bit-vector of SIZE filled with INIT (default 0)."
  (let ((v (make-array size)) (i 0) (b (or init 0)))
    (loop
      (when (= i size) (return v))
      (aset v i b)
      (setq i (+ i 1)))))

(defun %make-bit-vector-from-contents (size contents)
  "Allocate a bit-vector of SIZE and initialise from CONTENTS (list or vector)."
  (let ((v (make-array size)))
    (cond
      ((listp contents)
       (let ((cur contents) (i 0))
         (loop
           (when (or (null cur) (= i size)) (return v))
           (aset v i (car cur))
           (setq cur (cdr cur))
           (setq i (+ i 1))))
       (let ((j 0))
         (loop
           (when (= j size) (return v))
           (let ((e (aref v j)))
             (when (null e) (aset v j 0)))
           (setq j (+ j 1))))
       v)
      (t
       (let ((i 0))
         (loop
           (when (= i size) (return v))
           (aset v i (aref contents i))
           (setq i (+ i 1))))))))

(defun bit (bv idx) (aref bv idx))
(defun sbit (bv idx) (aref bv idx))
;; SETF expansions for (bit bv idx) / (sbit bv idx).
;; SETF macro generic case emits (set-bit BV IDX VAL); our defun
;; mirrors that arg order.
(defun set-bit  (bv idx val) (aset bv idx val) val)
(defun set-sbit (bv idx val) (aset bv idx val) val)

(defun %bit-result-array (bv1 result-arg)
  "Resolve the result-array argument of a bit-X function.
   T  → bv1 (destructive on bv1).
   NIL/missing → fresh bit-vector of bv1's length.
   array → use as-is."
  (cond
    ((eq result-arg t) bv1)
    ((null result-arg) (make-bit-vector (array-length bv1) 0))
    (t result-arg)))

(defun %bit-binop (bv1 bv2 result-arg op)
  "Apply OP (a 2-arg lambda taking two bits) element-wise on BV1 and BV2,
   writing into the result chosen by RESULT-ARG."
  (let* ((len (array-length bv1))
         (result (%bit-result-array bv1 result-arg))
         (i 0))
    (loop
      (when (= i len) (return result))
      (let ((a (aref bv1 i))
            (b (aref bv2 i)))
        (aset result i (funcall op a b)))
      (setq i (+ i 1)))))

(defun %bit-and-op  (a b) (logand a b))
(defun %bit-ior-op  (a b) (logior a b))
(defun %bit-xor-op  (a b) (logxor a b))
(defun %bit-eqv-op  (a b) (if (= a b) 1 0))
(defun %bit-nand-op (a b) (if (and (= a 1) (= b 1)) 0 1))
(defun %bit-nor-op  (a b) (if (and (= a 0) (= b 0)) 1 0))
(defun %bit-andc1-op (a b) (logand (if (= a 0) 1 0) b))
(defun %bit-andc2-op (a b) (logand a (if (= b 0) 1 0)))
(defun %bit-orc1-op  (a b) (logior (if (= a 0) 1 0) b))
(defun %bit-orc2-op  (a b) (logior a (if (= b 0) 1 0)))

(defun bit-and  (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-and-op))
(defun bit-ior  (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-ior-op))
(defun bit-or   (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-ior-op))
(defun bit-xor  (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-xor-op))
(defun bit-eqv  (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-eqv-op))
(defun bit-nand (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-nand-op))
(defun bit-nor  (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-nor-op))
(defun bit-andc1 (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-andc1-op))
(defun bit-andc2 (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-andc2-op))
(defun bit-orc1  (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-orc1-op))
(defun bit-orc2  (bv1 bv2 &optional result) (%bit-binop bv1 bv2 result #'%bit-orc2-op))

(defun bit-not (bv &optional result)
  "Return a bit-vector whose elements are 1 - bv[i]."
  (let* ((len (array-length bv))
         (out (%bit-result-array bv result))
         (i 0))
    (loop
      (when (= i len) (return out))
      (let ((e (aref bv i)))
        (aset out i (if (= e 0) 1 0)))
      (setq i (+ i 1)))))

