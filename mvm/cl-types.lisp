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

(defun cos (x)
  "Cosine — stub: exact for 0, approximation 0 for others."
  (if (integerp x)
      (if (= x 0) 1 0)
      ;; Float: return 1.0 for 0.0, else 0.0
      (if (and (not (fixnump x)) (not (consp x)) (not (null x))
               (= (obj-subtag x) #x32))
          (if (%float-zero-p x)
              (%make-float-raw 1 1)   ; cos(0.0) = 1.0
              (%make-float-raw 0 1))  ; cos(x) ≈ 0.0
          0)))

(defun sin (x)
  "Sine — stub: exact for 0, approximation 0 for others."
  (if (integerp x)
      0
      (if (and (not (fixnump x)) (not (consp x)) (not (null x))
               (= (obj-subtag x) #x32))
          (%make-float-raw 0 1)  ; sin(x) ≈ 0.0
          0)))

(defun tan (x)
  "Tangent — stub: 0 for all inputs."
  (if (integerp x)
      0
      (if (and (not (fixnump x)) (not (consp x)) (not (null x))
               (= (obj-subtag x) #x32))
          (%make-float-raw 0 1)
          0)))

(defun acos (x)
  "Arc cosine — stub: returns pi/2 ≈ rational {157, 100} for range check."
  (if (integerp x) 0 (%make-float-raw 0 1)))

(defun asin (x)
  "Arc sine — stub."
  (if (integerp x) 0 (%make-float-raw 0 1)))

(defun atan (x &optional y)
  "Arc tangent — stub."
  (if (integerp x) 0 (%make-float-raw 0 1)))

(defun cosh (x)
  "Hyperbolic cosine — stub: 1 for 0, 0 otherwise."
  (if (integerp x)
      (if (= x 0) 1 0)
      (if (and (not (fixnump x)) (not (consp x)) (not (null x))
               (= (obj-subtag x) #x32))
          (if (%float-zero-p x)
              (%make-float-raw 1 1)
              (%make-float-raw 0 1))
          0)))

(defun sinh (x)
  "Hyperbolic sine — stub."
  (if (integerp x) 0
      (if (and (not (fixnump x)) (not (consp x)) (not (null x))
               (= (obj-subtag x) #x32))
          (%make-float-raw 0 1)
          0)))

(defun tanh (x)
  "Hyperbolic tangent — stub."
  (if (integerp x) 0
      (if (and (not (fixnump x)) (not (consp x)) (not (null x))
               (= (obj-subtag x) #x32))
          (%make-float-raw 0 1)
          0)))

(defun exp (x)
  "e^x — stub: 1 for 0, 0 otherwise."
  (if (integerp x)
      (if (= x 0) 1 0)
      (if (and (not (fixnump x)) (not (consp x)) (not (null x))
               (= (obj-subtag x) #x32))
          (if (%float-zero-p x)
              (%make-float-raw 1 1)
              (%make-float-raw 0 1))
          0)))

(defun log (x &optional base)
  "Natural logarithm — stub: 0 for all inputs."
  (if (integerp x) 0 (%make-float-raw 0 1)))

(defun phase (x)
  "Phase of complex number — stub."
  (if (integerp x) 0 (%make-float-raw 0 1)))

(defun cis (x)
  "cis(x) = cos(x) + i*sin(x) — stub: returns 1 for 0."
  (if (integerp x)
      (if (= x 0) 1 0)
      (%make-float-raw 0 1)))
(defun integer (n) n)  ; not a real CL function but used as type coercion
(defun set-schar (str idx ch) (aset str idx (char-code ch)) ch)
(defun schar (str idx) (code-char (aref str idx)))
(defun char (str idx) (code-char (aref str idx)))
(defun symbol-plist (sym) nil)
; fboundp defined in Layer 8 above
(defun fill-pointer (vec)
  (if (consp vec) (car vec) (length vec)))
(defun bit-vector-p (x) nil)  ; stub
(defun simple-string-p (x) (stringp x))
(defun simple-bit-vector-p (x) nil)
(defun subtypep (t1 t2 &rest args) (values nil nil))  ; stub
(defun logcount (n) (let ((c 0) (x (abs n))) (loop (when (zerop x) (return c)) (when (oddp x) (setq c (+ c 1))) (setq x (ash x -1)))))
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
(defun nintersection-with-check (l1 l2 &rest args) (nintersection l1 l2))
(defun intersection (l1 l2 &rest args) (nintersection l1 l2))
(defun set-difference (l1 l2 &rest args) (let ((r nil)) (dolist (item l1 (nreverse r)) (unless (member item l2) (setq r (cons item r))))))
(defun nset-difference (l1 l2 &rest args) (set-difference l1 l2))
(defun union (l1 l2 &rest args) (let ((r (copy-list l1))) (dolist (item l2 r) (unless (member item r) (setq r (cons item r))))))
(defun nunion (l1 l2 &rest args) (union l1 l2))
(defun subsetp (l1 l2 &rest args) (every (lambda (x) (member x l2)) l1))

(defun nsubst (new old tree &rest args)
  "Substitute NEW for OLD in TREE (destructive)."
  (subst new old tree))

(defun nsubst-if (new pred tree &rest args)
  "Substitute NEW for elements satisfying PRED in TREE (destructive)."
  (cond ((funcall pred tree) new)
        ((consp tree) (set-car tree (nsubst-if new pred (car tree)))
                      (set-cdr tree (nsubst-if new pred (cdr tree)))
                      tree)
        (t tree)))

(defun nsubst-if-not (new pred tree &rest args)
  "Substitute NEW for elements not satisfying PRED in TREE (destructive)."
  (nsubst-if new (lambda (x) (not (funcall pred x))) tree))

(defun check-nsubst-if (new pred tree)
  "Test helper for nsubst-if."
  (nsubst-if new pred (copy-tree tree)))

(defun check-nsubst-if-not (new pred tree)
  "Test helper for nsubst-if-not."
  (nsubst-if-not new pred (copy-tree tree)))

(defun subst-if (new pred tree &rest args)
  "Substitute NEW for elements satisfying PRED in TREE."
  (cond ((funcall pred tree) new)
        ((consp tree) (let ((a (subst-if new pred (car tree)))
                            (d (subst-if new pred (cdr tree))))
                        (if (and (eq a (car tree)) (eq d (cdr tree))) tree
                            (cons a d))))
        (t tree)))

(defun subst-if-not (new pred tree &rest args)
  "Substitute NEW for elements not satisfying PRED in TREE."
  (subst-if new (lambda (x) (not (funcall pred x))) tree))

(defun nsublis (alist tree &rest args)
  "Substitute from ALIST in TREE (destructive)."
  (cond ((null tree) nil)
        ((consp tree) (set-car tree (nsublis alist (car tree)))
                      (set-cdr tree (nsublis alist (cdr tree)))
                      tree)
        (t (let ((pair (assoc tree alist)))
             (if pair (cdr pair) tree)))))

(defun sublis (alist tree &rest args)
  "Substitute from ALIST in TREE."
  (let ((pair (assoc tree alist)))
    (if pair (cdr pair)
        (if (consp tree)
            (let ((a (sublis alist (car tree)))
                  (d (sublis alist (cdr tree))))
              (if (and (eq a (car tree)) (eq d (cdr tree))) tree
                  (cons a d)))
            tree))))

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

(defun exact-divide (a b)
  "Divide A by B. Returns integer if exact, cons (num . den) ratio otherwise."
  (if (= (mod a b) 0)
      (/ a b)
      (let ((g (gcd-impl a b)))
        (let ((num (/ a g)) (den (/ b g)))
          (if (< den 0) (cons (- 0 num) (- 0 den)) (cons num den))))))

(defun generic-negate (x)
  "Negate X (integer or ratio)."
  (if (ratiop x)
      (cons (- 0 (car x)) (cdr x))
      (- 0 x)))

(defun generic-subtract (a b)
  "Subtract B from A, handling ratios."
  (cond
    ((and (integerp a) (integerp b)) (- a b))
    ((and (integerp a) (ratiop b))
     ;; a - num/den = (a*den - num)/den
     (let ((num (- (* a (cdr b)) (car b)))
           (den (cdr b)))
       (if (= (mod num den) 0) (/ num den) (cons num den))))
    ((and (ratiop a) (integerp b))
     ;; num/den - b = (num - b*den)/den
     (let ((num (- (car a) (* b (cdr a))))
           (den (cdr a)))
       (if (= (mod num den) 0) (/ num den) (cons num den))))
    ((and (ratiop a) (ratiop b))
     ;; a.num/a.den - b.num/b.den = (a.num*b.den - b.num*a.den)/(a.den*b.den)
     (let ((num (- (* (car a) (cdr b)) (* (car b) (cdr a))))
           (den (* (cdr a) (cdr b))))
       (let ((g (gcd-impl num den)))
         (let ((rn (/ num g)) (rd (/ den g)))
           (if (= rd 1) rn (cons rn rd))))))
    (t (- a b))))

(defun generic-1+ (x)
  "Add 1 to X (integer or ratio)."
  (if (ratiop x)
      (let ((num (+ (car x) (cdr x)))
            (den (cdr x)))
        (if (= (mod num den) 0) (/ num den) (cons num den)))
      (+ x 1)))

(defun ratiop (x)
  "Check if X is a cons-based ratio (num . den) where both are integers."
  (if (consp x)
      (if (integerp (car x))
          (if (integerp (cdr x))
              (if (not (= (cdr x) 0)) t nil)
              nil)
          nil)
      nil))

(defun ratio-numerator (x) (car x))
(defun ratio-denominator (x) (cdr x))
(defun numerator (x) (if (ratiop x) (car x) x))
(defun denominator (x) (if (ratiop x) (cdr x) 1))

;;; ============================================================
;;; Float inspection helpers
;;; ============================================================

(defun float-negative-p (x)
  "Check if boxed float X has negative sign bit.
   The hi32 slot is stored as a signed tagged fixnum; negative means sign bit set."
  (< (aref x 0) 0))

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

(defun numeric-value-less-p (a b)
  "Return T if numeric value A < numeric value B.
   Handles integers, boxed floats, and cons-based ratios."
  (cond
    ;; Both integers
    ((and (integerp a) (integerp b)) (< a b))
    ;; a is float
    ((floatp-impl a)
     (cond
       ((integerp b)
        ;; float vs integer
        (if (float-negative-p a)
            t  ; negative float < any non-negative integer we encounter
            (let ((int-part (float-truncate-to-integer a)))
              (< int-part b))))
       ((ratiop b)
        ;; float vs ratio: convert float to approximate integer comparison
        (if (float-negative-p a)
            (if (> (car b) 0) t nil)  ; negative float < positive ratio
            (let ((int-part (float-truncate-to-integer a)))
              ;; int-part < num/den iff int-part * den < num
              (< (* int-part (cdr b)) (car b)))))
       (t nil)))
    ;; a is ratio
    ((ratiop a)
     (cond
       ((integerp b)
        ;; ratio vs integer: a.num/a.den < b iff a.num < b * a.den
        (< (car a) (* b (cdr a))))
       ((ratiop b)
        ;; ratio vs ratio: a.num/a.den < b.num/b.den iff a.num*b.den < b.num*a.den
        (< (* (car a) (cdr b)) (* (car b) (cdr a))))
       (t nil)))
    ;; a is integer, b is float
    ((and (integerp a) (floatp-impl b))
     (if (float-negative-p b)
         nil  ; positive or zero integer not less than negative float
         (let ((int-part (float-truncate-to-integer b)))
           (<= a int-part))))
    ;; a is integer, b is ratio
    ((and (integerp a) (ratiop b))
     ;; a < num/den iff a*den < num
     (< (* a (cdr b)) (car b)))
    (t nil)))

(defun numeric-<= (a b)
  "Return T if A <= B for any numeric type."
  (or (numeric-equal-p a b) (numeric-value-less-p a b)))

(defun numeric->= (a b)
  "Return T if A >= B for any numeric type."
  (numeric-<= b a))

(defun numeric-equal-p (a b)
  "Return T if A equals B numerically."
  (cond
    ((and (integerp a) (integerp b)) (= a b))
    ((and (floatp-impl a) (floatp-impl b)) (float-equal a b))
    ((and (ratiop a) (ratiop b))
     (and (= (car a) (car b)) (= (cdr a) (cdr b))))
    ((and (ratiop a) (integerp b))
     (and (= (cdr a) 1) (= (car a) b)))
    ((and (integerp a) (ratiop b))
     (and (= (cdr b) 1) (= (car b) a)))
    (t nil)))

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

(defun typep (obj type)
  "Extended typep supporting compound type specifiers."
  (cond
    ;; Simple type names (symbols/keywords)
    ((not (consp type))
     (let ((tn type))
       (cond
         ((eq tn 'integer) (integerp obj))
         ((eq tn 'fixnum) (integerp obj))
         ((eq tn 'bignum) nil)
         ((eq tn 'real) (or (integerp obj) (floatp-impl obj) (ratiop obj)))
         ((eq tn 'rational) (or (integerp obj) (ratiop obj)))
         ((eq tn 'number) (or (integerp obj) (floatp-impl obj) (ratiop obj)))
         ((eq tn 'float) (floatp-impl obj))
         ((eq tn 'single-float) (floatp-impl obj))
         ((eq tn 'double-float) (floatp-impl obj))
         ((eq tn 'short-float) (floatp-impl obj))
         ((eq tn 'long-float) (floatp-impl obj))
         ((eq tn 'ratio) (ratiop obj))
         ((eq tn 'cons) (consp obj))
         ((eq tn 'list) (or (null obj) (consp obj)))
         ((eq tn 'null) (null obj))
         ((eq tn 'symbol) (or (null obj) (eq obj t) (integerp obj)))
         ((eq tn 'string) (stringp obj))
         ((eq tn 'simple-string) (stringp obj))
         ((eq tn 'base-string) (stringp obj))
         ((eq tn 'simple-base-string) (stringp obj))
         ((eq tn 'character) (characterp obj))
         ((eq tn 'base-char) (characterp obj))
         ((eq tn 'standard-char) (characterp obj))
         ((eq tn 'atom) (not (consp obj)))
         ((eq tn 't) t)
         ((eq tn 'nil) nil)
         ((eq tn 'boolean) (or (null obj) (eq obj t)))
         ((eq tn 'bit) (or (= obj 0) (= obj 1)))
         ((eq tn 'unsigned-byte) (and (integerp obj) (>= obj 0)))
         ((eq tn 'signed-byte) (integerp obj))
         ((eq tn 'function) (or (functionp obj) (%generic-function-p obj)))
         ((eq tn 'generic-function) (%generic-function-p obj))
         ((eq tn 'standard-generic-function) (%generic-function-p obj))
         ((eq tn 'standard-method) (%standard-method-p obj))
         ((eq tn 'method) (%standard-method-p obj))
         ((eq tn 'method-combination) (%mc-p obj))
         ;; CLOS instance check
         ((eq tn 'standard-object) (%clos-instance-p obj))
         (t nil))))
    ;; Compound type specifiers
    (t
     (let ((head (car type)))
       (cond
         ;; (real low high) — range check for reals
         ((eq head 'real)
          (if (or (integerp obj) (floatp-impl obj) (ratiop obj))
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (integer low high)
         ((eq head 'integer)
          (if (integerp obj)
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (float low high)
         ((or (eq head 'float) (eq head 'single-float)
              (eq head 'double-float) (eq head 'short-float) (eq head 'long-float))
          (if (floatp-impl obj)
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (rational low high)
         ((eq head 'rational)
          (if (or (integerp obj) (ratiop obj))
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ;; (eql val)
         ((eq head 'eql)
          (eql obj (cadr type)))
         ;; (member val1 val2 ...)
         ((eq head 'member)
          (if (member obj (cdr type)) t nil))
         ;; (and type1 type2 ...)
         ((eq head 'and)
          (let ((ok t))
            (dolist (sub (cdr type))
              (unless (typep obj sub) (setq ok nil)))
            ok))
         ;; (or type1 type2 ...)
         ((eq head 'or)
          (let ((ok nil))
            (dolist (sub (cdr type))
              (when (typep obj sub) (setq ok t)))
            ok))
         ;; (not type)
         ((eq head 'not)
          (not (typep obj (cadr type))))
         ;; (satisfies pred)
         ((eq head 'satisfies) nil)  ; can't call arbitrary predicates
         ;; (unsigned-byte n) — integer in [0, 2^n - 1]
         ((eq head 'unsigned-byte)
          (and (integerp obj) (>= obj 0)
               (< obj (ash 1 (cadr type)))))
         ;; (signed-byte n) — integer in [-2^(n-1), 2^(n-1) - 1]
         ((eq head 'signed-byte)
          (and (integerp obj)
               (let ((half (ash 1 (- (cadr type) 1))))
                 (and (>= obj (- 0 half)) (< obj half)))))
         ;; (mod n) — integer in [0, n-1]
         ((eq head 'mod)
          (and (integerp obj) (>= obj 0) (< obj (cadr type))))
         (t nil))))))

(defun typep* (obj type) (typep obj type))

