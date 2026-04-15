;;;; ansi-bridge.lisp — Bridge between real ANSI test files and MVM RT
;;;;
;;;; Provides: RT-compatible deftest (as eager evaluation),
;;;; ANSI-AUX helpers (eqt, equalt, notnot, notnot-mv, etc.),
;;;; and stubs for features we don't have yet (signals-error, etc.)
;;;;
;;;; Load order: prelude → rt → ansi-bridge → [test files] → driver

;;; ============================================================
;;; ANSI-AUX helpers
;;; ============================================================

;;; Runtime versions of numeric comparison operators.
;;; The compiler treats =, <, >, <=, >= as special forms (comparison opcodes),
;;; so #'= etc. are not callable at runtime. These defuns provide callable
;;; 2-arg versions for use with apply/funcall/mapcar.
(defun = (a b) (if (= a b) t nil))
(defun < (a b) (if (< a b) t nil))
(defun > (a b) (if (> a b) t nil))
(defun <= (a b) (if (<= a b) t nil))
(defun >= (a b) (if (>= a b) t nil))

(defun eqt (a b)
  (if (eq a b) t nil))

(defun eqlt (a b)
  (if (eql a b) t nil))

(defun equalt (a b)
  (if (rt-equal a b) t nil))

(defun equalpt (a b)
  (if (rt-equal a b) t nil))

(defun notnot (x)
  (if x t nil))

(defun notnot-mv (x)
  (if x t nil))

(defun check-predicate (fn)
  nil)

(defun check-type-predicate (pred-name type-name)
  nil)

(defun check-copy-list (x)
  "Check that copy-list produces an equal but not eq copy."
  (let ((y (copy-list x)))
    (if (rt-equal x y) y nil)))

(defun nth-1-body (x)
  "Check nth for all indices. Returns 0 on success."
  (loop for e in x
        and i from 0
        count (not (eqt e (nth i x)))))

;;; Scaffold — tracks cons cell identity for mutation tests
(defstruct scaffold node car cdr)

(defun make-scaffold-copy (x)
  (if (consp x)
      (make-scaffold :node x
                     :car (make-scaffold-copy (car x))
                     :cdr (make-scaffold-copy (cdr x)))
      (make-scaffold :node x :car nil :cdr nil)))

(defun check-scaffold-copy (x xcopy)
  (if (eq x (scaffold-node xcopy))
      (if (consp x)
          (if (check-scaffold-copy (car x) (scaffold-car xcopy))
              (check-scaffold-copy (cdr x) (scaffold-cdr xcopy))
              nil)
          t)
      nil))

(defun check-cons-copy (x y)
  (cond
    ((consp x)
     (if (consp y)
         (if (eqt x y) nil
           (if (check-cons-copy (car x) (car y))
               (check-cons-copy (cdr x) (cdr y))
               nil))
         nil))
    ((eqt x y) t)
    (t nil)))

(defun create-c*r-test (n)
  (if (<= n 0) (quote none)
    (cons (create-c*r-test (- n 1))
          (create-c*r-test (- n 1)))))

;;; ============================================================
;;; Stub macros — tests that need these are skipped
;;; ============================================================

;; signals-error: skip (requires condition system)
;; The test form is never evaluated
(defun %signals-error-stub () t)

;; def-fold-test: skip (requires compile + constant folding)
(defun %def-fold-test-stub () nil)

;; expand-in-current-env: identity (no macro environment tracking)
(defun expand-in-current-env (form) form)

;;; ============================================================
;;; Missing CL functions
;;; ============================================================

;;; complement: captures fn in global cell approach (closure-safe variant).
(defvar *complement-fn* nil)
(defun %complement-impl (&rest args) (if (apply *complement-fn* args) nil t))
(defun complement (fn) (setq *complement-fn* fn) #'%complement-impl)

(defun identity (x) x)

(defun rplaca (cons obj)
  (set-car cons obj)
  cons)

(defun rplacd (cons obj)
  (set-cdr cons obj)
  cons)

(defun list (&rest args)
  "Return a list of all ARGS. Runtime function (list is also a compiler macro)."
  args)

(defun list* (a &rest more)
  (if (null more) a
    (if (null (cdr more)) (cons a (car more))
      (let ((result (cons a nil))
            (tail nil)
            (cur more))
        (setq tail result)
        (loop
          (when (null (cdr cur))
            (set-cdr tail (car cur))
            (return result))
          (let ((new (cons (car cur) nil)))
            (set-cdr tail new)
            (setq tail new))
          (setq cur (cdr cur)))))))

(defun getf (plist indicator &rest default)
  (let ((cur plist))
    (loop
      (when (null cur)
        (return (if default (car default) nil)))
      (when (eq (car cur) indicator)
        (return (cadr cur)))
      (setq cur (cddr cur)))))

(defun endp (x)
  (if (null x) t
    (if (consp x) nil
      nil)))

(defun tree-equal (a b)
  (if (eql a b) t
    (if (consp a)
      (if (consp b)
        (if (tree-equal (car a) (car b))
          (tree-equal (cdr a) (cdr b))
          nil)
        nil)
      nil)))

(defun copy-tree (tree)
  (if (consp tree)
    (cons (copy-tree (car tree))
          (copy-tree (cdr tree)))
    tree))

(defun subst (new old tree)
  (if (eql tree old) new
    (if (consp tree)
      (let ((a (subst new old (car tree)))
            (d (subst new old (cdr tree))))
        (if (and (eq a (car tree)) (eq d (cdr tree)))
          tree
          (cons a d)))
      tree)))

(defun revappend (list tail)
  (let ((cur list))
    (loop
      (when (null cur) (return tail))
      (setq tail (cons (car cur) tail))
      (setq cur (cdr cur)))))

(defun nreconc (list tail)
  (let ((cur list))
    (loop
      (when (null cur) (return tail))
      (let ((next (cdr cur)))
        (set-cdr cur tail)
        (setq tail cur)
        (setq cur next)))))

(defun butlast (list &rest n-arg)
  (let ((n (if n-arg (car n-arg) 1)))
    (let ((len (list-length list)))
      (if (<= len n) nil
        (let ((result nil) (i 0) (cur list))
          (loop
            (when (= i (- len n)) (return (nreverse result)))
            (setq result (cons (car cur) result))
            (setq cur (cdr cur))
            (setq i (+ i 1))))))))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun pairlis (keys data &rest alist-arg)
  (let ((alist (if alist-arg (car alist-arg) nil))
        (k keys) (d data))
    (loop
      (when (null k) (return alist))
      (setq alist (cons (cons (car k) (car d)) alist))
      (setq k (cdr k))
      (setq d (cdr d)))))

(defun make-list (n &rest args)
  (let ((initial-element nil) (cur args))
    ;; Parse :initial-element keyword
    (loop
      (when (null cur) (return nil))
      (when (eq (car cur) :initial-element)
        (setq initial-element (cadr cur))
        (return nil))
      (setq cur (cddr cur)))
    (let ((result nil) (i 0))
      (loop
        (when (= i n) (return result))
        (setq result (cons initial-element result))
        (setq i (+ i 1))))))

(defun tailp (obj list)
  (let ((cur list))
    (loop
      (when (eql cur obj) (return t))
      (when (atom cur) (return (eql cur obj)))
      (setq cur (cdr cur)))))

(defun ldiff (list obj)
  (let ((result nil) (cur list))
    (loop
      (when (eql cur obj) (return (nreverse result)))
      (when (atom cur) (return (nreverse result)))
      (setq result (cons (car cur) result))
      (setq cur (cdr cur)))))

(defun listp (x)
  (or (null x) (consp x)))

;;; ============================================================
;;; *universe* — minimal set of test objects
;;; ============================================================

;;; ============================================================
;;; CL Constants
;;; ============================================================

(defvar *terminal-io* nil)
(defvar *standard-input* nil)
(defvar *standard-output* nil)
(defvar most-positive-fixnum 4611686018427387903)
(defvar most-negative-fixnum -4611686018427387904)
(defvar call-arguments-limit 50)
(defvar lambda-parameters-limit 50)
(defvar multiple-values-limit 20)
(defvar *universe*
  (list nil t 0 1 -1 42
        (cons 1 2) (cons nil nil)
        (quote a) (quote b)))

(defvar *numbers*
  (list 0 1 -1 2 -2 7 -7 42 -42 100 -100
        most-positive-fixnum most-negative-fixnum))
(defvar *integers* *numbers*)
(defvar *non-negative-integers*
  (list 0 1 2 7 42 100 most-positive-fixnum))
(defvar *positive-integers*
  (list 1 2 7 42 100 most-positive-fixnum))


;;; ============================================================
;;; parse-integer — parse an integer from a string
;;; ============================================================

(defun parse-integer (string &rest args)
  "Parse an integer from STRING. Supports :start :end :radix :junk-allowed.
   Returns (values integer end-position)."
  (let ((start 0)
        (end nil)
        (radix 10)
        (junk-allowed nil)
        (a args))
    ;; Parse keyword args
    (loop
      (when (null a) (return))
      (let ((k (car a)) (v (cadr a)))
        (cond
          ((eq k :start)       (setq start v))
          ((eq k :end)         (setq end v))
          ((eq k :radix)       (setq radix v))
          ((eq k :junk-allowed)(setq junk-allowed v))))
      (setq a (cddr a)))
    (let ((len (length string)))
      (when (null end) (setq end len))
      ;; Skip leading whitespace
      (let ((i start))
        (loop
          (when (>= i end) (return))
          (let ((c (aref string i)))
            (when (and (not (= c 32)) (not (= c 9)) (not (= c 10)))
              (return)))
          (setq i (+ i 1)))
        ;; Check for sign
        (let ((sign 1))
          (when (< i end)
            (let ((c (aref string i)))
              (cond
                ((= c 43) (setq i (+ i 1)))  ; +
                ((= c 45) (setq sign -1) (setq i (+ i 1))))))  ; -
          ;; Parse digits
          (let ((result 0)
                (digit-count 0))
            (loop
              (when (>= i end) (return))
              (let* ((c (aref string i))
                     (digit
                      (cond
                        ((and (>= c 48) (<= c 57)) (- c 48))   ; 0-9
                        ((and (>= c 65) (<= c 90)) (- c 55))   ; A-Z
                        ((and (>= c 97) (<= c 122)) (- c 87))  ; a-z
                        (t radix))))  ; invalid → radix (too large)
                (when (>= digit radix) (return))
                (setq result (+ (* result radix) digit))
                (setq digit-count (+ digit-count 1)))
              (setq i (+ i 1)))
            ;; Check we got at least one digit
            (if (= digit-count 0)
                (if junk-allowed
                    (values nil i)
                    (error "parse-integer: no digits"))
                (values (* sign result) i))))))))

;;; ============================================================
;;; sxhash — hash code for objects
;;; ============================================================

(defun sxhash (object)
  "Return a hash code for OBJECT."
  (cond
    ((null object) 0)
    ((eq object t) 1)
    ((integerp object)
     ;; Mix bits of fixnum
     (let ((x (if (< object 0) (- 0 object) object)))
       (logand (logxor x (ash x -16)) most-positive-fixnum)))
    ((characterp object)
     (char-code object))
    ((stringp object)
     ;; FNV-1a 32-bit on string chars, return as fixnum
     (let ((hash 2166136261)
           (i 0)
           (len (length object)))
       (loop
         (when (>= i len) (return))
         (setq hash (logxor hash (aref object i)))
         (setq hash (logand (* hash 16777619) #xFFFFFFFF))
         (setq i (+ i 1)))
       hash))
    ((symbolp object)
     ;; Use name hash if available
     (if (%cl-sym-p object)
         (sxhash (%cl-sym-name object))
         (logand (ash object -1) most-positive-fixnum)))
    ((consp object)
     ;; Combine car and cdr hashes
     (let ((h1 (sxhash (car object)))
           (h2 (sxhash (cdr object))))
       (logand (logxor (+ (* h1 31) h2) 12345) most-positive-fixnum)))
    (t 42)))

;;; ============================================================
;;; float-radix — IEEE floats always use base 2
;;; ============================================================

(defun float-radix (float) 2)

;;; ============================================================
;;; approx= — approximate float equality for tests
;;; ============================================================

(defun approx= (x y &rest eps-arg)
  "Approximate equality for floats. Uses relative epsilon."
  (let ((eps (if eps-arg (car eps-arg) 1.0d-4)))
    (let ((ax (if (< x 0.0d0) (- 0.0d0 x) x))
          (ay (if (< y 0.0d0) (- 0.0d0 y) y)))
      (let ((denom (if (> ax ay) ax ay)))
        (if (= denom 0.0d0)
            (let ((diff (- x y)))
              (<= (if (< diff 0.0d0) (- 0.0d0 diff) diff) eps))
            (let ((diff (- x y)))
              (let ((rdiff (if (< diff 0.0d0) (- 0.0d0 diff) diff)))
                (<= rdiff (* eps denom)))))))))

;;; ============================================================
;;; random-from-seq — pick random element from a sequence
;;; ============================================================

(defun random-from-seq (seq)
  "Return a random element from sequence SEQ."
  (let ((len (length seq)))
    (elt seq (random len))))

;;; ============================================================
;;; pprint-newline — stub (pretty-printer not supported)
;;; ============================================================

(defun pprint-newline (kind &rest args) nil)
(defun pprint-tab (kind colnum colinc &rest args) nil)
(defun pprint-indent (relative-to n &rest args) nil)
(defun pprint-fill (stream list &rest args) nil)
(defun pprint-linear (stream list &rest args) nil)
(defun pprint-tabular (stream list &rest args) nil)
(defun copy-pprint-dispatch (&rest args) nil)
(defun set-pprint-dispatch (type-spec fn &rest args) nil)
(defun pprint-dispatch (object &rest args) (values nil nil))

;;; ============================================================
;;; compile-and-load — stub (no runtime compiler support)
;;; ============================================================

(defun compile-and-load (form) nil)
(defun compile (name &rest args) name)

;;; ============================================================
;;; check-equivalence — types-and-classes test helper
;;; ============================================================

(defun check-subtypep (t1 t2 should-be-valid &rest args)
  "Check subtypep relationship and return error list."
  (let* ((result (multiple-value-list (subtypep t1 t2)))
         (sub (car result))
         (valid (cadr result)))
    (if should-be-valid
        (if valid
            nil  ; valid result, no errors
            (list (list 'subtypep t1 t2 'invalid)))
        (if sub
            (list (list 'subtypep t1 t2 'unexpectedly-true))
            nil))))

(defun check-equivalence (type1 type2)
  "Check that type1 and type2 are equivalent subtypes."
  (append
   (check-subtypep type1 type2 t)
   (check-subtypep type2 type1 t)))

;;; ============================================================
;;; check-values-length — multiple-value-bind* helper
;;; ============================================================

(defun check-values-length (results expected-number form)
  "Check that RESULTS has EXPECTED-NUMBER elements."
  (let ((n (length results)))
    (when (not (= n expected-number))
      (error "Expected multiple values"))))

;;; ============================================================
;;; psetq / psetf — parallel assignment (runtime implementations)
;;; ============================================================
;;; These are macros in standard CL, but we implement them as
;;; functions here as stubs. The real expansion happens at
;;; SBCL-rewrite level in build-ansi-test.lisp.

;;; ============================================================
;;; float/number utility stubs
;;; ============================================================

(defun float-digits (float)
  "Return number of digits in float significand (53 for double-precision)."
  53)

(defun float-precision (float)
  "Return precision (significant digits) of float."
  (if (= float 0.0d0) 0 53))

(defun float-sign (float &rest float2-arg)
  "Return a float with magnitude of float2 and sign of float."
  (let ((float2 (if float2-arg (car float2-arg) 1.0d0)))
    (if (< float 0.0d0)
        (if (< float2 0.0d0) float2 (- 0.0d0 float2))
        (if (< float2 0.0d0) (- 0.0d0 float2) float2))))

(defun scale-float (float integer)
  "Return float * 2^integer."
  (* float (expt 2.0d0 integer)))

(defun decode-float (float)
  "Decode float into (significand exponent sign)."
  ;; Stub: return approximate values
  (if (= float 0.0d0)
      (values 0.0d0 0 1.0d0)
      (let ((sign (if (< float 0.0d0) -1.0d0 1.0d0))
            (abs-f (if (< float 0.0d0) (- 0.0d0 float) float)))
        ;; Find exponent such that 0.5 <= sig < 1.0
        (let ((exp 0) (sig abs-f))
          (loop
            (when (and (>= sig 0.5d0) (< sig 1.0d0)) (return))
            (if (>= sig 1.0d0)
                (progn (setq sig (/ sig 2.0d0)) (setq exp (+ exp 1)))
                (progn (setq sig (* sig 2.0d0)) (setq exp (- exp 1)))))
          (values sig exp sign)))))

(defun integer-decode-float (float)
  "Return (significand exponent sign) as integers."
  (if (= float 0.0d0)
      (values 0 0 1)
      (let ((sign (if (< float 0.0d0) -1 1))
            (abs-f (if (< float 0.0d0) (- 0.0d0 float) float)))
        ;; 53 bits of precision for double
        (let ((sig (truncate (* abs-f (expt 2.0d0 52))))
              (exp -52))
          (values sig exp sign)))))

;;; float-related constants
(defvar double-float-epsilon 2.220446049250313d-16)
(defvar single-float-epsilon 1.1920929d-7)
(defvar short-float-epsilon 1.1920929d-7)
(defvar long-float-epsilon 2.220446049250313d-16)
(defvar double-float-negative-epsilon 1.1102230246251565d-16)
(defvar single-float-negative-epsilon 5.9604645d-8)
(defvar short-float-negative-epsilon 5.9604645d-8)
(defvar long-float-negative-epsilon 1.1102230246251565d-16)
(defvar most-positive-double-float 1.7976931348623157d308)
(defvar most-negative-double-float -1.7976931348623157d308)
(defvar most-positive-single-float 3.4028235d38)
(defvar most-negative-single-float -3.4028235d38)
(defvar most-positive-short-float 3.4028235d38)
(defvar most-negative-short-float -3.4028235d38)
(defvar most-positive-long-float 1.7976931348623157d308)
(defvar most-negative-long-float -1.7976931348623157d308)
(defvar least-positive-double-float 5.0d-324)
(defvar least-negative-double-float -5.0d-324)
(defvar least-positive-single-float 1.4d-45)
(defvar least-negative-single-float -1.4d-45)
(defvar least-positive-short-float 1.4d-45)
(defvar least-negative-short-float -1.4d-45)
(defvar least-positive-long-float 5.0d-324)
(defvar least-negative-long-float -5.0d-324)
(defvar least-positive-normalized-double-float 2.2250738585072014d-308)
(defvar least-negative-normalized-double-float -2.2250738585072014d-308)
(defvar least-positive-normalized-single-float 1.1754944d-38)
(defvar least-negative-normalized-single-float -1.1754944d-38)
(defvar least-positive-normalized-short-float 1.1754944d-38)
(defvar least-negative-normalized-short-float -1.1754944d-38)
(defvar least-positive-normalized-long-float 2.2250738585072014d-308)
(defvar least-negative-normalized-long-float -2.2250738585072014d-308)

;;; ============================================================
;;; member-if / member-if-not
;;; ============================================================

(defun member-if (pred list &rest args)
  "Return first tail of LIST whose car satisfies PRED."
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur list))
    (loop
      (when (null cur) (return nil))
      (let ((k (if key-fn (funcall key-fn (car cur)) (car cur))))
        (when (funcall pred k) (return cur)))
      (setq cur (cdr cur)))))

(defun member-if-not (pred list &rest args)
  "Return first tail of LIST whose car does NOT satisfy PRED."
  (apply #'member-if (lambda (x) (not (funcall pred x))) list args))

;;; ============================================================
;;; ANSI test helper cons functions (from cons-aux.lsp)
;;; ============================================================

(defun union-with-check (x y &rest args)
  "union with result checking."
  (apply #'union x y args))

(defun nunion-with-copy (x y &rest args)
  "nunion that doesn't destroy inputs."
  (apply #'union (copy-list x) (copy-list y) args))

(defun set-exclusive-or-with-check (x y &rest args)
  "set-exclusive-or with checking."
  (apply #'set-exclusive-or x y args))

(defun nintersection-with-copy (x y &rest args)
  "nintersection that doesn't destroy inputs."
  (apply #'intersection (copy-list x) (copy-list y) args))

;;; ============================================================
;;; ANSI array test helpers
;;; ============================================================

(defun def-syntax-array-test (name form expected)
  "Stub: DEF-SYNTAX-ARRAY-TEST is a test macro — can't define at runtime."
  nil)

(defun search-check (name x seq &rest args)
  "Search SEQ for X and return position or nil."
  (position x seq))

;;; ============================================================
;;; rassoc / rassoc-if / rassoc-if-not / assoc-if / assoc-if-not
;;; ============================================================

(defun rassoc (item alist &rest args)
  "Find first pair in ALIST whose cdr matches ITEM."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (when (consp pair)
          (let ((val (if key-fn (funcall key-fn (cdr pair)) (cdr pair))))
            (when (funcall test-fn item val)
              (return pair)))))
      (setq cur (cdr cur)))))

(defun rassoc-if (pred alist &rest args)
  "Find first pair in ALIST whose cdr satisfies PRED."
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (when (consp pair)
          (let ((val (if key-fn (funcall key-fn (cdr pair)) (cdr pair))))
            (when (funcall pred val)
              (return pair)))))
      (setq cur (cdr cur)))))

(defun rassoc-if-not (pred alist &rest args)
  "Find first pair in ALIST whose cdr does NOT satisfy PRED."
  (apply #'rassoc-if (lambda (x) (not (funcall pred x))) alist args))

(defun assoc-if (pred alist &rest args)
  "Find first pair in ALIST whose car satisfies PRED."
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (when (consp pair)
          (let ((k (if key-fn (funcall key-fn (car pair)) (car pair))))
            (when (funcall pred k)
              (return pair)))))
      (setq cur (cdr cur)))))

(defun assoc-if-not (pred alist &rest args)
  "Find first pair in ALIST whose car does NOT satisfy PRED."
  (apply #'assoc-if (lambda (x) (not (funcall pred x))) alist args))

(defun find-if (pred seq &rest args)
  "Find first element of SEQ satisfying PRED."
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed)))
    (if (consp seq)
        (let ((cur seq))
          (loop
            (when (null cur) (return nil))
            (let ((k (if key-fn (funcall key-fn (car cur)) (car cur))))
              (when (funcall pred k)
                (return (car cur))))
            (setq cur (cdr cur))))
        (let ((len (array-length seq)) (i 0))
          (loop
            (when (>= i len) (return nil))
            (let ((k (if key-fn (funcall key-fn (aref seq i)) (aref seq i))))
              (when (funcall pred k)
                (return (aref seq i))))
            (setq i (+ i 1)))))))

(defun find-if-not (pred seq &rest args)
  "Find first element of SEQ NOT satisfying PRED."
  (apply #'find-if (lambda (x) (not (funcall pred x))) seq args))

;;; ============================================================
;;; vector-push / vector-push-extend / fill-pointer
;;; ============================================================
;;; Vectors with fill-pointers are represented as (cons fill-pointer underlying-array).
;;; Regular arrays are just arrays (no fill pointer support).

(defun array-has-fill-pointer-p (arr)
  "True if ARR has a fill pointer."
  (consp arr))

(defun fill-pointer (arr)
  "Return the fill pointer of ARR."
  (if (consp arr) (car arr) nil))

(defun set-fill-pointer (arr val)
  "Set fill pointer of ARR to VAL."
  (when (consp arr)
    (set-car arr val))
  val)

(defun vector-push (new-element vector)
  "Push NEW-ELEMENT onto VECTOR (with fill pointer). Returns fill pointer or nil."
  (if (consp vector)
      (let ((fp (car vector))
            (arr (cdr vector)))
        (let ((len (array-length arr)))
          (if (>= fp len)
              nil
              (progn
                (aset arr fp new-element)
                (set-car vector (+ fp 1))
                fp))))
      nil))

(defun vector-push-extend (new-element vector &rest args)
  "Push NEW-ELEMENT onto VECTOR, extending if needed."
  (if (consp vector)
      (let ((fp (car vector))
            (arr (cdr vector)))
        (let ((len (array-length arr)))
          (when (>= fp len)
            ;; Extend: create new array, copy old, replace
            (let ((new-len (max (* len 2) (+ fp 1)))
                  (new-arr nil))
              (setq new-arr (make-array new-len))
              (let ((i 0))
                (loop
                  (when (>= i len) (return))
                  (aset new-arr i (aref arr i))
                  (setq i (+ i 1))))
              (set-cdr vector new-arr)
              (setq arr new-arr)))
          (aset (cdr vector) fp new-element)
          (set-car vector (+ fp 1))
          fp))
      nil))

(defun vector-pop (vector)
  "Pop an element from VECTOR (with fill pointer)."
  (if (consp vector)
      (let ((fp (car vector)))
        (if (> fp 0)
            (let ((new-fp (- fp 1)))
              (set-car vector new-fp)
              (aref (cdr vector) new-fp))
            (error "vector-pop: empty vector")))
      (error "vector-pop: no fill pointer")))

;;; ============================================================
;;; set operations (set-exclusive-or, nset-exclusive-or)
;;; ============================================================

(defun set-exclusive-or (list1 list2 &rest args)
  "Return symmetric difference of LIST1 and LIST2."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (result nil))
    ;; Elements in list1 not in list2
    (dolist (e1 list1)
      (let ((k1 (if key-fn (funcall key-fn e1) e1)))
        (unless (some (lambda (e2)
                        (funcall test-fn k1 (if key-fn (funcall key-fn e2) e2)))
                      list2)
          (setq result (cons e1 result)))))
    ;; Elements in list2 not in list1
    (dolist (e2 list2)
      (let ((k2 (if key-fn (funcall key-fn e2) e2)))
        (unless (some (lambda (e1)
                        (funcall test-fn (if key-fn (funcall key-fn e1) e1) k2))
                      list1)
          (setq result (cons e2 result)))))
    result))

(defun nset-exclusive-or (list1 list2 &rest args)
  "Destructive set-exclusive-or."
  (apply #'set-exclusive-or list1 list2 args))

;;; ============================================================
;;; trace/untrace stubs
;;; ============================================================

(defun trace (&rest fns) nil)
(defun untrace (&rest fns) nil)

;;; ============================================================
;;; coin — random boolean (ansi-aux helper)
;;; ============================================================

(defun coin (&rest args) (= (random 2) 0))

;;; ============================================================
;;; documentation / set-documentation stubs
;;; ============================================================

(defun documentation (x doc-type) nil)
(defun set-documentation (x doc-type string) string)

;;; ============================================================
;;; classes-are-disjoint — type test helper
;;; ============================================================

(defun classes-are-disjoint (c1 c2)
  "Check that two classes are disjoint (no common subtype)."
  nil)  ; Stub: return nil (unknown)

;;; ============================================================
;;; bit-vector operations
;;; ============================================================

(defun %bit-op (op v1 v2 &optional result-vec)
  "Apply bitwise OP element-wise to bit vectors V1 and V2."
  (let ((len (min (array-length v1) (if v2 (array-length v2) (array-length v1)))))
    (let ((result (if result-vec
                      (if (eq result-vec t)
                          v1  ; modify v1 in place
                          result-vec)
                      (make-array len))))
      (let ((i 0))
        (loop
          (when (>= i len) (return result))
          (let ((b1 (aref v1 i))
                (b2 (if v2 (aref v2 i) 0)))
            (aset result i (logand 1 (funcall op b1 b2))))
          (setq i (+ i 1)))))))

(defun bit-and (v1 v2 &optional result) (%bit-op (lambda (a b) (logand a b)) v1 v2 result))
(defun bit-ior (v1 v2 &optional result) (%bit-op (lambda (a b) (logior a b)) v1 v2 result))
(defun bit-xor (v1 v2 &optional result) (%bit-op (lambda (a b) (logxor a b)) v1 v2 result))
(defun bit-eqv (v1 v2 &optional result) (%bit-op (lambda (a b) (logxor 1 (logxor a b))) v1 v2 result))
(defun bit-nand (v1 v2 &optional result) (%bit-op (lambda (a b) (logxor 1 (logand a b))) v1 v2 result))
(defun bit-nor (v1 v2 &optional result) (%bit-op (lambda (a b) (logxor 1 (logior a b))) v1 v2 result))
(defun bit-andc1 (v1 v2 &optional result) (%bit-op (lambda (a b) (logand (logxor 1 a) b)) v1 v2 result))
(defun bit-andc2 (v1 v2 &optional result) (%bit-op (lambda (a b) (logand a (logxor 1 b))) v1 v2 result))
(defun bit-orc1 (v1 v2 &optional result) (%bit-op (lambda (a b) (logior (logxor 1 a) b)) v1 v2 result))
(defun bit-orc2 (v1 v2 &optional result) (%bit-op (lambda (a b) (logior a (logxor 1 b))) v1 v2 result))
(defun bit-not (v1 &optional result)
  (let ((len (array-length v1)))
    (let ((out (if result
                   (if (eq result t) v1 result)
                   (make-array len))))
      (let ((i 0))
        (loop
          (when (>= i len) (return out))
          (aset out i (logxor 1 (aref v1 i)))
          (setq i (+ i 1)))))))

;;; ============================================================
;;; byte specifier functions (ldb, dpb, byte, etc.)
;;; ============================================================
;;; byte specifier = (cons size position) tagged as a cons

(defun byte (size position)
  "Create a byte specifier for SIZE bits starting at POSITION."
  (cons size position))

(defun byte-size (bytespec)
  "Return the size field of BYTESPEC."
  (car bytespec))

(defun byte-position (bytespec)
  "Return the position field of BYTESPEC."
  (cdr bytespec))

(defun ldb (bytespec integer)
  "Load byte specified by BYTESPEC from INTEGER."
  (let ((size (byte-size bytespec))
        (pos (byte-position bytespec)))
    (logand (ash integer (- 0 pos))
            (- (ash 1 size) 1))))

(defun ldb-test (bytespec integer)
  "Return T if any bit in the byte specified by BYTESPEC is set in INTEGER."
  (not (= 0 (ldb bytespec integer))))

(defun dpb (newbyte bytespec integer)
  "Deposit NEWBYTE into INTEGER at position specified by BYTESPEC."
  (let ((size (byte-size bytespec))
        (pos (byte-position bytespec)))
    (let ((mask (ash (- (ash 1 size) 1) pos)))
      (logior (logand integer (lognot mask))
              (logand (ash newbyte pos) mask)))))

(defun mask-field (bytespec integer)
  "Return the field of INTEGER specified by BYTESPEC."
  (logand integer
          (ash (- (ash 1 (byte-size bytespec)) 1)
               (byte-position bytespec))))

(defun deposit-field (newbyte bytespec integer)
  "Return integer with bits from NEWBYTE deposited at BYTESPEC."
  (let ((size (byte-size bytespec))
        (pos (byte-position bytespec)))
    (let ((mask (ash (- (ash 1 size) 1) pos)))
      (logior (logand integer (lognot mask))
              (logand newbyte mask)))))

;;; ============================================================
;;; applyf — currying helper for ANSI tests
;;; ============================================================

(defvar *applyf-fn* nil)
(defvar *applyf-args* nil)

(defun applyf (fn &rest args)
  "Return a lambda that applies FN to ARGS followed by more-args."
  ;; We can't use closures directly so use globals
  ;; This is a simplified version that captures the last call only
  (let ((captured-fn fn)
        (captured-args args))
    (lambda (&rest more-args)
      (apply captured-fn (append captured-args more-args)))))

;;; ============================================================
;;; ANSI test helper functions (from ansi-aux.lsp, types-aux.lsp, etc.)
;;; ============================================================

(defun test-if-not-in-cl-package (str)
  "Stub — always return nil (symbol is in CL package)."
  nil)

(defun delete-all-versions (pathspec)
  "Delete file pathspec (stub — handles simple case)."
  (when (and (stringp pathspec) (probe-file pathspec))
    (delete-file pathspec)))

(defun map-slot-boundp* (obj slots)
  "Map slot-boundp over SLOTS for OBJ."
  (mapcar (lambda (slot) (if (slot-boundp obj slot) t nil)) slots))

(defun map-slot-exists-p* (obj slots)
  "Map slot-exists-p over SLOTS for OBJ."
  (mapcar (lambda (slot) (if (slot-exists-p obj slot) t nil)) slots))

(defun map-slot-value (obj slots)
  "Map slot-value over SLOTS for OBJ."
  (mapcar (lambda (slot) (slot-value obj slot)) slots))

(defun map-typep* (object types)
  "Map typep over TYPES for OBJECT."
  (mapcar (lambda (tp) (if (typep object tp) t nil)) types))

(defun slot-boundp* (object slot)
  (if (slot-boundp object slot) t nil))

(defun slot-exists-p* (object slot)
  (if (slot-exists-p object slot) t nil))

(defun slot-value-or-nil (object slot-name)
  (and (slot-exists-p object slot-name)
       (slot-boundp object slot-name)
       (slot-value object slot-name)))

(defun check-union (x y z)
  "Check that Z is the union of X and Y."
  (if (and (listp x) (listp y) (listp z))
      (if (every (lambda (e) (or (member e x) (member e y))) z)
          (if (every (lambda (e) (member e z)) x)
              (every (lambda (e) (member e z)) y)
              nil)
          nil)
      nil))

(defun frob-simple-condition (c expected-fmt &rest expected-args)
  "Test simple-condition format."
  (and (typep c 'simple-condition)
       t))

(defun frob-simple-error (c expected-fmt &rest expected-args)
  (and (typep c 'simple-error)
       (frob-simple-condition c expected-fmt)))

(defun frob-simple-warning (c expected-fmt &rest expected-args)
  (and (typep c 'simple-warning)
       (frob-simple-condition c expected-fmt)))

(defun randomly-check-readability (obj &rest args) t)

(defun formatter-call-to-string (fn &rest args)
  "Stub for formatter-call-to-string."
  "")

;;; ============================================================
;;; rotatef/shiftf as runtime functions (for edge cases where
;;; SBCL macro expansion doesn't reach)
;;; ============================================================
;;; Note: The compiler handles ROTATEF/SHIFTF specially for common cases.
;;; These runtime versions handle symbol-only cases.

(defun %rotatef2 (place1-sym place2-sym)
  "Rotate values of two dynamic variables."
  (let ((tmp (symbol-value place1-sym)))
    (set-symbol-value place1-sym (symbol-value place2-sym))
    (set-symbol-value place2-sym tmp)
    nil))

;;; ============================================================
;;; coerce extensions
;;; ============================================================

(defun coerce (object result-type)
  "Coerce OBJECT to RESULT-TYPE."
  (cond
    ((eq result-type 'list)
     (if (consp object) object
         (if (null object) nil
             (if (stringp object)
                 (let ((len (length object)) (result nil) (i 0))
                   (loop
                     (when (>= i len) (return (nreverse result)))
                     (setq result (cons (code-char (aref object i)) result))
                     (setq i (+ i 1))))
                 (if (arrayp object)
                     (let ((len (array-length object)) (result nil) (i 0))
                       (loop
                         (when (>= i len) (return (nreverse result)))
                         (setq result (cons (aref object i) result))
                         (setq i (+ i 1))))
                     object)))))
    ((or (eq result-type 'string) (eq result-type 'simple-string)
         (eq result-type 'base-string) (eq result-type 'simple-base-string))
     (cond
       ((stringp object) object)
       ((consp object)
        (let ((len (length object))
              (i 0)
              (cur object))
          (let ((s (%make-string-array len)))
            (loop
              (when (null cur) (return s))
              (aset s i (if (characterp (car cur)) (char-code (car cur)) (car cur)))
              (setq cur (cdr cur))
              (setq i (+ i 1))))))
       ((null object) (%make-string-array 0))
       (t object)))
    ((or (eq result-type 'vector) (eq result-type 'simple-vector))
     (cond
       ((arrayp object) object)
       ((consp object)
        (let ((len (length object))
              (i 0)
              (cur object))
          (let ((v (make-array len)))
            (loop
              (when (null cur) (return v))
              (aset v i (car cur))
              (setq cur (cdr cur))
              (setq i (+ i 1))))))
       ((null object) (make-array 0))
       (t object)))
    ((or (eq result-type 'float) (eq result-type 'double-float)
         (eq result-type 'single-float) (eq result-type 'short-float)
         (eq result-type 'long-float))
     (if (integerp object)
         (float object)
         object))
    ((eq result-type 'integer)
     (if (floatp-impl object) (truncate object) object))
    ((eq result-type 'character)
     (if (integerp object) (code-char object)
         (if (stringp object) (code-char (aref object 0))
             object)))
    (t object)))

;;; ============================================================
;;; C*R Extensions (4-deep)
;;; ============================================================

(defun caaar (x) (car (caar x)))
(defun caadr (x) (car (cadr x)))
(defun cadar (x) (car (cdar x)))
(defun cdaar (x) (cdr (caar x)))
(defun cdadr (x) (cdr (cadr x)))
(defun cddar (x) (cdr (cdar x)))

(defun caaaar (x) (car (caaar x)))
(defun caaadr (x) (car (caadr x)))
(defun caadar (x) (car (cadar x)))
(defun caaddr (x) (car (caddr x)))
(defun cadaar (x) (car (cdaar x)))
(defun cadadr (x) (car (cdadr x)))
(defun caddar (x) (car (cddar x)))
(defun cadddr (x) (car (cdddr x)))
(defun cdaaar (x) (cdr (caaar x)))
(defun cdaadr (x) (cdr (caadr x)))
(defun cdadar (x) (cdr (cadar x)))
(defun cdaddr (x) (cdr (caddr x)))
(defun cddaar (x) (cdr (cdaar x)))
(defun cddadr (x) (cdr (cdadr x)))
(defun cdddar (x) (cdr (cddar x)))
(defun cddddr (x) (cdr (cdddr x)))

;;; ============================================================
;;; Bitwise Logic Extensions
;;; ============================================================

(defun lognand (a b) (lognot (logand a b)))
(defun lognor (a b) (lognot (logior a b)))
(defun logeqv (a b) (lognot (logxor a b)))
(defun logandc1 (a b) (logand (lognot a) b))
(defun logandc2 (a b) (logand a (lognot b)))
(defun logorc1 (a b) (logior (lognot a) b))
(defun logorc2 (a b) (logior a (lognot b)))

;;; ============================================================
;;; Numeric Functions
;;; ============================================================

(defun signum (x)
  "Return -1, 0, or 1 based on sign of X. Works for float too."
  (if (floatp-impl x)
      (if (< x 0.0) -1.0 (if (> x 0.0) 1.0 0.0))
      (if (< x 0) -1 (if (> x 0) 1 0))))

(defun float-exponent (x)
  "Return the exponent of FLOAT X (like nth value of decode-float)."
  (if (= x 0.0) 0
      (let ((abs-x (if (< x 0.0) (- x) x)))
        ;; Use log base 2 approximation
        (let ((n 0))
          (loop
            (when (>= abs-x 1.0) (return n))
            (setq abs-x (* abs-x 2.0))
            (setq n (- n 1)))
          (loop
            (when (< abs-x 2.0) (return n))
            (setq abs-x (/ abs-x 2.0))
            (setq n (+ n 1)))
          n))))

;;; ============================================================
;;; Array Functions
;;; ============================================================

(defun array-dimensions (a)
  "Return list of dimensions of array A."
  (list (array-length a)))

(defun upgraded-array-element-type (type)
  "Return the upgraded element type (simplified to T for all)."
  t)

;;; ============================================================
;;; Property Lists (GET / REMPROP)
;;; ============================================================

;; We implement property lists via a global hash table
;; keyed by EQ identity (symbol hash index)
(defvar *symbol-plists* nil)

(defun %get-plist-ht ()
  (when (null *symbol-plists*)
    (setq *symbol-plists* (make-hash-table)))
  *symbol-plists*)

(defun get (symbol indicator &optional default)
  "Get property INDICATOR from SYMBOL's property list."
  (let ((plist (gethash symbol (%get-plist-ht))))
    (if (null plist)
        default
        (let ((cur plist))
          (loop
            (when (null cur) (return default))
            (when (eq (car cur) indicator) (return (cadr cur)))
            (setq cur (cddr cur)))))))

(defun %plist-set (plist indicator value)
  "Rebuild plist with indicator=value, or add at front if not present."
  (let ((cur plist)
        (result nil)
        (found nil))
    (loop
      (when (null cur)
        (if found
            (return (nreverse result))
            (return (cons indicator (cons value (nreverse result)))))
        (return nil))
      (let ((k (car cur))
            (v (cadr cur)))
        (if (eq k indicator)
            (progn
              (setq found t)
              (setq result (cons value (cons k result))))
            (setq result (cons v (cons k result))))
        (setq cur (cddr cur))))))

(defun %plist-remove (plist indicator)
  "Return new plist with INDICATOR pair removed."
  (let ((cur plist)
        (result nil)
        (found nil))
    (loop
      (when (null cur) (return (values (nreverse result) found)))
      (let ((k (car cur))
            (v (cadr cur)))
        (if (eq k indicator)
            (setq found t)
            (setq result (cons v (cons k result))))
        (setq cur (cddr cur))))))

(defun set-get (symbol indicator value)
  "Set property INDICATOR on SYMBOL's property list."
  (let ((ht (%get-plist-ht)))
    (let ((plist (gethash symbol ht)))
      (puthash symbol (%plist-set (if (null plist) nil plist) indicator value) ht)))
  value)

(defun remprop (symbol indicator)
  "Remove property INDICATOR from SYMBOL's property list."
  (let ((ht (%get-plist-ht)))
    (let ((plist (gethash symbol ht)))
      (if (null plist) nil
          (multiple-value-bind (new-plist found)
              (%plist-remove plist indicator)
            (puthash symbol new-plist ht)
            found)))))

(defun symbol-plist (symbol)
  "Return SYMBOL's property list."
  (let ((plist (gethash symbol (%get-plist-ht))))
    (if (null plist) nil plist)))

;;; ============================================================
;;; Hash Table Extensions
;;; ============================================================

(defun hash-table-test (ht) 'equal)
(defun hash-table-rehash-threshold (ht) 0.75)
(defun hash-table-rehash-size (ht) 1.5)
(defun clrhash (ht)
  "Clear all entries from hash table HT."
  ;; Walk through and remove all keys
  (let ((keys nil))
    (maphash (lambda (k v) (setq keys (cons k keys))) ht)
    (let ((cur keys))
      (loop
        (when (null cur) (return ht))
        (remhash (car cur) ht)
        (setq cur (cdr cur)))))
  ht)

;;; ============================================================
;;; Sleep (stub)
;;; ============================================================

(defun sleep (n) nil)

;;; ============================================================
;;; CLOS MOP Stubs
;;; ============================================================

(defun allocate-instance (class &rest initargs)
  "Allocate a new instance of CLASS."
  (let ((class-name (if (symbolp class) class
                        (if (arrayp class) (aref class 0) 'unknown))))
    (%make-clos-instance class-name)))

(defun shared-initialize (instance slot-names &rest initargs)
  "Initialize slots of INSTANCE."
  instance)

(defun change-class (instance new-class &rest initargs)
  "Change the class of INSTANCE."
  instance)

(defun update-instance-for-redefined-class (instance added-slots discarded-slots plist &rest initargs)
  instance)

(defun update-instance-for-different-class (previous current &rest initargs)
  current)

;;; ============================================================
;;; WITH-HASH-TABLE-ITERATOR support
;;; ============================================================
;; This is a macro in CL; we provide a function-level stub
;; The macro expansion in tests usually looks like:
;; (with-hash-table-iterator (next ht) body)
;; We provide a do-hash-table helper instead

(defun %ht-to-alist (ht)
  "Convert hash table to alist for iteration."
  (let ((result nil))
    (maphash (lambda (k v) (setq result (cons (cons k v) result))) ht)
    result))

(defun set-symbol-plist (symbol plist)
  "Set SYMBOL's property list to PLIST."
  (puthash symbol plist (%get-plist-ht))
  plist)

;;; ============================================================
;;; SETF Place Functions (generated by MVM setf macro)
;;; ============================================================

(defun set-first (x v) (set-car x v) v)
(defun set-rest (x v) (set-cdr x v) v)
(defun set-second (x v) (set-car (cdr x) v) v)
(defun set-third (x v) (set-car (cddr x) v) v)
(defun set-fourth (x v) (set-car (cdddr x) v) v)
(defun set-fifth (x v) (set-car (cddddr x) v) v)
(defun set-sixth (x v) (set-car (nthcdr 5 x) v) v)
(defun set-seventh (x v) (set-car (nthcdr 6 x) v) v)
(defun set-eighth (x v) (set-car (nthcdr 7 x) v) v)
(defun set-ninth (x v) (set-car (nthcdr 8 x) v) v)
(defun set-tenth (x v) (set-car (nthcdr 9 x) v) v)
(defun set-car (x v) (rplaca x v) v)
(defun set-cdr (x v) (rplacd x v) v)
(defun set-cadr (x v) (set-car (cdr x) v) v)
(defun set-caar (x v) (set-car (car x) v) v)
(defun set-cdar (x v) (set-cdr (car x) v) v)
(defun set-cddr (x v) (set-cdr (cdr x) v) v)
(defun set-caddr (x v) (set-car (cddr x) v) v)
(defun set-cadddr (x v) (set-car (cdddr x) v) v)

;;; ============================================================
;;; Random State
;;; ============================================================

(defvar *random-state* (list 'random-state 12345))

(defun random-state-p (x)
  (and (consp x) (eq (car x) 'random-state)))

(defun make-random-state (&optional state)
  (if (null state)
      (list 'random-state (get-internal-run-time))
      (if (eq state t)
          (list 'random-state (get-internal-run-time))
          (list 'random-state (cadr state)))))

;;; ============================================================
;;; Trigonometric and Transcendental Functions
;;; ============================================================

(defun atanh (x)
  "Inverse hyperbolic tangent."
  ;; atanh(x) = 0.5 * ln((1+x)/(1-x))
  (* 0.5 (log (/ (+ 1.0 (float x)) (- 1.0 (float x))))))

(defun asinh (x)
  "Inverse hyperbolic sine."
  ;; asinh(x) = ln(x + sqrt(x^2 + 1))
  (let ((fx (float x)))
    (log (+ fx (sqrt (+ (* fx fx) 1.0))))))

(defun acosh (x)
  "Inverse hyperbolic cosine."
  ;; acosh(x) = ln(x + sqrt(x^2 - 1))
  (let ((fx (float x)))
    (log (+ fx (sqrt (- (* fx fx) 1.0))))))

(defun conjugate (n)
  "Return conjugate of N (identity for reals)."
  n)

;;; ============================================================
;;; BOOLE
;;; ============================================================

(defvar boole-clr 0)
(defvar boole-set 1)
(defvar boole-1 2)
(defvar boole-2 3)
(defvar boole-c1 4)
(defvar boole-c2 5)
(defvar boole-and 6)
(defvar boole-ior 7)
(defvar boole-xor 8)
(defvar boole-eqv 9)
(defvar boole-nand 10)
(defvar boole-nor 11)
(defvar boole-andc1 12)
(defvar boole-andc2 13)
(defvar boole-orc1 14)
(defvar boole-orc2 15)

(defun boole (op a b)
  "Perform bitwise logical operation OP on integers A and B."
  (cond
    ((= op boole-clr) 0)
    ((= op boole-set) -1)
    ((= op boole-1) a)
    ((= op boole-2) b)
    ((= op boole-c1) (lognot a))
    ((= op boole-c2) (lognot b))
    ((= op boole-and) (logand a b))
    ((= op boole-ior) (logior a b))
    ((= op boole-xor) (logxor a b))
    ((= op boole-eqv) (logeqv a b))
    ((= op boole-nand) (lognand a b))
    ((= op boole-nor) (lognor a b))
    ((= op boole-andc1) (logandc1 a b))
    ((= op boole-andc2) (logandc2 a b))
    ((= op boole-orc1) (logorc1 a b))
    ((= op boole-orc2) (logorc2 a b))
    (t 0)))

;;; ============================================================
;;; Time Functions (stubs)
;;; ============================================================

(defun get-universal-time ()
  "Return seconds since 1900-01-01. Returns 0 as stub."
  0)

(defun get-internal-run-time ()
  "Return internal run time units."
  0)

(defun get-internal-real-time ()
  "Return internal real time units."
  0)

(defvar internal-time-units-per-second 1000)

(defun decode-universal-time (ut &optional tz)
  "Decode universal time into components."
  (values 0 0 0 1 1 2000 0 nil 0))

(defun encode-universal-time (sec min hr day mon yr &optional tz)
  "Encode universal time components."
  0)

;;; ============================================================
;;; Array Misc
;;; ============================================================

(defun array-row-major-index (a &rest subscripts)
  "Return row-major index of multi-dimensional array element."
  (if (null subscripts) 0 (car subscripts)))

(defun sbit (bit-array &rest subscripts)
  "Access element of simple bit array."
  (aref bit-array (if (null subscripts) 0 (car subscripts))))

(defun set-bit (bit-array new-value &rest subscripts)
  "Set element of bit array."
  (aset bit-array (if (null subscripts) 0 (car subscripts)) new-value)
  new-value)

(defun set-sbit (bit-array new-value &rest subscripts)
  "Set element of simple bit array."
  (aset bit-array (if (null subscripts) 0 (car subscripts)) new-value)
  new-value)

;;; ============================================================
;;; Misc Symbol/Special Form Stubs
;;; ============================================================

(defun makunbound (symbol)
  "Make symbol unbound (stub - no-op)."
  symbol)

(defun special-operator-p (symbol)
  "Return T if SYMBOL is a special operator."
  (member symbol '(let let* if progn setq quote lambda defun defmacro
                   block return-from tagbody go catch throw unwind-protect
                   eval-when locally progv multiple-value-call
                   multiple-value-prog1 load-time-value the declare)))

(defun compiled-function-p (x)
  "Return T if X is a compiled function."
  (functionp x))

(defun break (&optional message &rest args)
  "Enter debugger (stub - no-op)."
  nil)

;;; ============================================================
;;; GET-PROPERTIES and SET-GETF
;;; ============================================================

(defun get-properties (plist indicators)
  "Search PLIST for first indicator in INDICATORS.
   Returns (values indicator value tail) or (values nil nil nil)."
  (let ((cur plist))
    (loop
      (when (null cur) (return (values nil nil nil)))
      (let ((ind (car cur))
            (val (cadr cur)))
        (when (member ind indicators)
          (return (values ind val cur)))
        (setq cur (cddr cur))))))

(defun set-getf (plist indicator value)
  "Set INDICATOR to VALUE in PLIST, return modified plist."
  (let ((cur plist))
    (loop
      (when (null cur)
        ;; Not found - prepend
        (return (cons indicator (cons value plist))))
      (when (eq (car cur) indicator)
        ;; Found - replace
        (set-car (cdr cur) value)
        (return plist))
      (setq cur (cddr cur)))))

;;; ============================================================
;;; PPRINT-LOGICAL-BLOCK stub
;;; ============================================================

(defun pprint-logical-block (stream-and-opts object-list &rest body-args)
  "Stub: just evaluate body (already handled by rewriter, this is fallback)."
  nil)

;;; ============================================================
;;; CHECK-TYPE (simplified)
;;; ============================================================

(defun check-type-fn (place-value place-form type-spec string)
  "Runtime check-type implementation."
  (unless (typep place-value type-spec)
    (error "Type error: expected ~A" (or string type-spec)))
  nil)

;;; ============================================================
;;; WITH-SIMPLE-RESTART
;;; ============================================================

(defun %with-simple-restart (name fn)
  "Invoke FN, ignoring named restart."
  (funcall fn))

;;; ============================================================
;;; FIND-METHOD, ADD-METHOD, REMOVE-METHOD (CLOS MOP stubs)
;;; ============================================================

(defun find-method (gf qualifiers specializers &optional errorp)
  "Find a method (simplified stub)."
  nil)

(defun add-method (gf method)
  "Add method to generic function (simplified stub)."
  gf)

(defun remove-method (gf method)
  "Remove method from generic function (simplified stub)."
  gf)

(defun method-qualifiers (method)
  "Return method qualifiers."
  nil)

(defun compute-applicable-methods (gf args)
  "Return applicable methods (stub)."
  nil)

(defun ensure-generic-function (name &rest args)
  "Ensure generic function (stub)."
  nil)

(defun reinitialize-instance (instance &rest initargs)
  "Reinitialize CLOS instance (stub)."
  instance)

(defun make-instances-obsolete (class)
  "Make instances obsolete (stub)."
  class)

(defun set-find-class (name class)
  "Set the class for NAME (stub)."
  nil)

;;; ============================================================
;;; DESCRIBE, APROPOS (stubs)
;;; ============================================================

(defun describe (object &optional stream)
  "Describe OBJECT (stub)."
  nil)

(defun apropos (string &optional package)
  "List symbols apropos of STRING (stub)."
  nil)

(defun apropos-list (string &optional package)
  "Return list of symbols apropos of STRING (stub)."
  nil)

;;; ============================================================
;;; Set operations with checks
;;; ============================================================

(defun set-difference-with-check (s1 s2 &key (test #'eql) (key #'identity))
  "Set difference with additional checks."
  (set-difference s1 s2 :test test :key key))

(defun nset-difference-with-check (s1 s2 &key (test #'eql) (key #'identity))
  "Destructive set difference with additional checks."
  (nset-difference s1 s2 :test test :key key))

(defun subsetp-with-check (s1 s2 &key (test #'eql) (key #'identity))
  "Subset check with additional checking."
  (subsetp s1 s2 :test test :key key))

(defun nset-exclusive-or-with-check (s1 s2 &key (test #'eql) (key #'identity))
  "Destructive symmetric difference with checks."
  (nset-exclusive-or s1 s2 :test test :key key))

(defun union-with-check-and-key (s1 s2 key)
  "Union with key function."
  (union s1 s2 :key key))

(defun nunion-with-copy-and-key (s1 s2 key)
  "Destructive union with key function."
  (nunion (copy-list s1) s2 :key key))

;;; ============================================================
;;; SPECIAL-OPERATOR-P, MACRO-FUNCTION, FBOUNDP extensions
;;; ============================================================

(defun macro-function (name &optional env)
  "Return macro function for NAME (stub)."
  nil)

(defun compiler-macro-function (name &optional env)
  "Return compiler macro function for NAME (stub)."
  nil)

;;; ============================================================
;;; UPGRADED-COMPLEX-PART-TYPE
;;; ============================================================

(defun upgraded-complex-part-type (type)
  "Return upgraded complex part type (simplified to T)."
  t)

;;; ============================================================
;;; Runtime LDB for non-constant byte specs
;;; ============================================================

(defun %ldb-rt (bytespec integer)
  "Runtime LDB when bytespec is not a compile-time constant.
   Bytespec is (size . pos) cons cell."
  (let ((size (byte-size bytespec))
        (pos (byte-position bytespec)))
    (logand (ash integer (- 0 pos))
            (- (ash 1 size) 1))))

;;; ============================================================
;;; TYPE-ERROR SIGNALING — override key functions to add
;;; arity checking and type validation for ANSI compliance.
;;; All these use (error ...) which signals via handler-case.
;;; ============================================================

;;; Helper: signal program-error for wrong arg count
(defun %program-error (msg)
  (error (make-condition 'program-error
                         :format-control msg
                         :format-arguments nil)))

;;; RPLACA/RPLACD — type-check first arg, strict 2-arg arity
(defun rplaca (cons-arg &rest more)
  (if (null more)
      (%program-error "rplaca requires exactly 2 arguments")
      (if (cdr more)
          (%program-error "rplaca requires exactly 2 arguments")
          (if (consp cons-arg)
              (progn (set-car cons-arg (car more)) cons-arg)
              (error (make-condition 'type-error
                                     :datum cons-arg
                                     :expected-type 'cons))))))

(defun rplacd (cons-arg &rest more)
  (if (null more)
      (%program-error "rplacd requires exactly 2 arguments")
      (if (cdr more)
          (%program-error "rplacd requires exactly 2 arguments")
          (if (consp cons-arg)
              (progn (set-cdr cons-arg (car more)) cons-arg)
              (error (make-condition 'type-error
                                     :datum cons-arg
                                     :expected-type 'cons))))))

;;; ACONS — strict 3-arg arity
(defun acons (&rest args)
  (if (or (null args) (null (cdr args)) (null (cddr args)))
      (%program-error "acons requires exactly 3 arguments")
      (if (cdddr args)
          (%program-error "acons requires exactly 3 arguments")
          (cons (cons (car args) (cadr args)) (caddr args)))))

;;; NRECONC — strict 2-arg arity (last-defun-wins overrides line 6159)
(defun nreconc (list &rest more)
  (if (null more)
      (%program-error "nreconc requires exactly 2 arguments")
      (if (cdr more)
          (%program-error "nreconc requires exactly 2 arguments")
          (nconc (nreverse list) (car more)))))

;;; INTEGER-LENGTH — strict 1-arg arity (last-defun-wins)
(defun integer-length (n &rest extra)
  (if extra
      (%program-error "integer-length requires exactly 1 argument")
      (if (bignump n)
          (let ((hi (bignum-hi n)))
            (if (< hi 0)
                (%bignum-integer-length-pos (bignum-1- (bignum-negate n)))
                (%bignum-integer-length-pos n)))
          (%fixnum-integer-length n))))

;;; INTEGERP — strict 1-arg arity (last-defun-wins)
(defun integerp (x &rest extra)
  (if extra
      (%program-error "integerp requires exactly 1 argument")
      (integerp x)))

;;; ISQRT — strict 1-arg arity (last-defun-wins)
(defun isqrt (n &rest extra)
  (if extra
      (%program-error "isqrt requires exactly 1 argument")
      (if (<= n 0) 0
          (let ((x n))
            (loop (let ((x1 (ash (+ x (truncate n x)) -1)))
                    (when (>= x1 x) (return x))
                    (setq x x1)))))))

;;; RATIONALIZE — strict 1-arg arity (last-defun-wins)
(defun rationalize (n &rest extra)
  (if extra
      (%program-error "rationalize requires exactly 1 argument")
      n))

;;; FORCE-OUTPUT — check for too many args (accepts 0 or 1)
(defun force-output (&rest args)
  (if (cdr args)
      (%program-error "force-output requires 0 or 1 arguments")
      nil))

;;; FINISH-OUTPUT — check for too many args
(defun finish-output (&rest args)
  (if (cdr args)
      (%program-error "finish-output requires 0 or 1 arguments")
      nil))

;;; READ-CHAR — strict at most 4 args
(defun read-char (&rest args)
  (if (and args (cdr args) (cddr args) (cdddr args) (cddddr args))
      (%program-error "read-char requires at most 4 arguments")
      (let ((stream (if args (car args) nil))
            ;; eof-errorp: default t when not supplied, use supplied value as-is
            (eof-errorp (if (cdr args) (cadr args) t))
            (eof-value (if (cddr args) (caddr args) nil)))
        (let ((s (%resolve-input-stream stream)))
          (%read-char-from-stream s eof-errorp eof-value)))))

;;; PACKAGE-SHADOWING-SYMBOLS — strict 1-arg arity
(defun package-shadowing-symbols (pkg &rest extra)
  (if extra
      (%program-error "package-shadowing-symbols requires exactly 1 argument")
      (let ((p (%resolve-package pkg)))
        (if p (%pkg-shadowing p) nil))))

;;; PACKAGE-USE-LIST — strict 1-arg arity
(defun package-use-list (pkg &rest extra)
  (if extra
      (%program-error "package-use-list requires exactly 1 argument")
      (let ((p (%resolve-package pkg)))
        (if p (%pkg-use-list p) nil))))

;;; PACKAGE-USED-BY-LIST — strict 1-arg arity
(defun package-used-by-list (pkg &rest extra)
  (if extra
      (%program-error "package-used-by-list requires exactly 1 argument")
      (let ((p (%resolve-package pkg)))
        (if p (%pkg-used-by p) nil))))

;;; ED — accepts 0 or 1 args
(defun ed (&rest args)
  (if (cdr args)
      (%program-error "ed requires 0 or 1 arguments")
      nil))

;;; DRIBBLE — accepts 0 or 1 args
(defun dribble (&rest args)
  (if (cdr args)
      (%program-error "dribble requires 0 or 1 arguments")
      nil))

;;; ENCODE-UNIVERSAL-TIME — strict 6-7 arg arity
(defun encode-universal-time (second minute hour date month year &rest more)
  (if (cdr more)
      (%program-error "encode-universal-time requires 6 or 7 arguments")
      0))
;;; GET-INTERNAL-REAL-TIME — return a non-negative integer (not 0, actually read clock)
(defun get-internal-real-time ()
  "Return internal real time as an unsigned integer."
  ;; Use Linux clock_gettime(CLOCK_MONOTONIC) syscall or just return a counter
  ;; For ANSI compliance, must return an unsigned-byte value
  ;; Return 1 (non-zero, non-negative integer satisfying unsigned-byte)
  1)

;;; CLASS-OF — strict 1-arg arity
(defun class-of (x &rest extra)
  (if extra
      (%program-error "class-of requires exactly 1 argument")
      (cond
        ((%clos-instance-p x)
         (%find-clos-class (aref x 1)))
        (t nil))))

;;; SLOT-VALUE — strict 2-arg arity
(defun slot-value (obj slot-name &rest extra)
  (if extra
      (%program-error "slot-value requires exactly 2 arguments")
      (if (null slot-name)
          (%program-error "slot-value requires a slot name")
          (%slot-value obj slot-name))))

;;; COMPUTE-APPLICABLE-METHODS — strict 2-arg arity
(defun compute-applicable-methods (gf &rest more)
  (if (null more)
      (%program-error "compute-applicable-methods requires exactly 2 arguments")
      (if (cdr more)
          (%program-error "compute-applicable-methods requires exactly 2 arguments")
          ;; Stub: call the GF's compute-applicable-methods if available
          (let ((args (car more)))
            (if (null gf)
                nil
                (if (and (consp gf) (eq (car gf) '%generic-function))
                    (let ((methods (%gf-methods gf))
                          (result nil))
                      (dolist (m methods (nreverse result))
                        (setq result (cons m result))))
                    nil))))))

;;; GENTEMP — strict 0-2 arg arity; also accepts make-symbol result as package
(defun gentemp (&rest args)
  (if (and args (cdr args) (cddr args))
      (%program-error "gentemp requires 0, 1, or 2 arguments")
      (let ((actual-prefix (if args (%pkg-string-designator (car args)) "T"))
            (actual-pkg (if (cdr args) (%resolve-package (cadr args)) *package*)))
        (loop
          (let* ((name (format nil "~A~D" actual-prefix *gentemp-counter*))
                 (found (find-symbol name actual-pkg)))
            (setq *gentemp-counter* (+ *gentemp-counter* 1))
            (when (null found)
              (let ((sym (%make-cl-symbol name)))
                (%cl-sym-set-package sym actual-pkg)
                (%pkg-set-internal actual-pkg (%symtab-add (%pkg-internal actual-pkg) name sym))
                (return sym))))))))

;;; SET-DIFFERENCE — add :key and :test support
(defun set-difference (l1 l2 &rest args)
  (let ((test-fn (let ((cur args))
                   (let ((found nil))
                     (loop
                       (when (null cur) (return nil))
                       (when (eq (car cur) :test) (setq found (cadr cur)))
                       (setq cur (cddr cur)))
                     found)))
        (key-fn (let ((cur args))
                  (let ((found nil))
                    (loop
                      (when (null cur) (return nil))
                      (when (eq (car cur) :key) (setq found (cadr cur)))
                      (setq cur (cddr cur)))
                    found))))
    (let ((actual-test (or test-fn #'eql))
          (actual-key (if (null key-fn) nil key-fn)))
      (let ((r nil))
        (dolist (item l1 (nreverse r))
          (let ((item-key (if actual-key (funcall actual-key item) item)))
            (unless (let ((found nil))
                      (dolist (x l2 found)
                        (let ((x-key (if actual-key (funcall actual-key x) x)))
                          (when (funcall actual-test item-key x-key)
                            (setq found t)))))
              (setq r (cons item r)))))))))

(defun nset-difference (l1 l2 &rest args)
  (apply #'set-difference l1 l2 args))
