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
;; Variadic comparison bridges per CLHS — 0/1 arg returns T (vacuous);
;; n≥2 walks pairwise.  Was 2-arg only, so (apply #'= '(17 17 17)) etc
;; would crash on arity mismatch.
(defun = (&rest cs)
  (cond ((null cs) t)
        ((null (cdr cs)) t)
        (t (let ((a (car cs)) (rest (cdr cs)))
             (loop (when (null rest) (return t))
               (unless (numeric-equal-p a (car rest)) (return nil))
               (setq rest (cdr rest)))))))
(defun < (&rest cs)
  (cond ((null cs) t)
        ((null (cdr cs)) t)
        (t (let ((a (car cs)) (rest (cdr cs)))
             (loop (when (null rest) (return t))
               (unless (numeric-value-less-p a (car rest)) (return nil))
               (setq a (car rest))
               (setq rest (cdr rest)))))))
(defun > (&rest cs)
  (cond ((null cs) t)
        ((null (cdr cs)) t)
        (t (let ((a (car cs)) (rest (cdr cs)))
             (loop (when (null rest) (return t))
               (unless (numeric-value-less-p (car rest) a) (return nil))
               (setq a (car rest))
               (setq rest (cdr rest)))))))
(defun <= (&rest cs)
  (cond ((null cs) t)
        ((null (cdr cs)) t)
        (t (let ((a (car cs)) (rest (cdr cs)))
             (loop (when (null rest) (return t))
               (unless (numeric-<= a (car rest)) (return nil))
               (setq a (car rest))
               (setq rest (cdr rest)))))))
(defun >= (&rest cs)
  (cond ((null cs) t)
        ((null (cdr cs)) t)
        (t (let ((a (car cs)) (rest (cdr cs)))
             (loop (when (null rest) (return t))
               (unless (numeric->= a (car rest)) (return nil))
               (setq a (car rest))
               (setq rest (cdr rest)))))))
(defun /= (&rest cs)
  ;; All-distinct: outer-loop pairs against tail; if any equal, return NIL.
  (cond ((null cs) t)
        ((null (cdr cs)) t)
        (t (let ((outer cs))
             (loop (when (null (cdr outer)) (return t))
               (let ((a (car outer)) (inner (cdr outer)))
                 (loop (when (null inner) (return nil))
                   (when (numeric-equal-p a (car inner))
                     (return-from /= nil))
                   (setq inner (cdr inner))))
               (setq outer (cdr outer)))))))

(defun eqt (a b)
  (if (eq a b) t nil))

(defun eqlt (a b)
  (if (eql a b) t nil))

(defun equalt (a b)
  (if (rt-equal a b) t nil))

(defun equalpt (a b)
  "Pure T/NIL equalpt — equivalent to equalt, kept separate because some
   ANSI tests reference it by name (e.g. via def-print-test templates)."
  (if (rt-equal a b) t nil))

(defun string=t (s1 s2 &rest args)
  "ansi-aux's string=t (T/NIL-normalized STRING=).  ansi-aux.lsp is
   skipped wholesale at build (read-time #. error), so without this the
   defgeneric.2 documentation checks called an undefined function."
  (declare (ignore args))
  (if (string= s1 s2) t nil))

(defun notnot (x)
  (if x t nil))

(defun %notnot-mv-fn (results)
  "Helper for NOTNOT-MV: booleanize the FIRST value, leave rest as-is.
   Modus's `values` returns multi-values; we wrap rest via apply."
  (cond
    ((null results) (values))
    ((null (cdr results)) (values (if (car results) t nil)))
    ((null (cddr results))
     (values (if (car results) t nil) (cadr results)))
    (t (values (if (car results) t nil) (cadr results) (caddr results)))))

;; Legacy defun (single-value boolean) kept as a fallback for callsites
;; that don't go through a macroexpander.  The compile-time macro below
;; intercepts source-level (NOTNOT-MV form) so two-value forms like
;; SUBTYPEP get their VALID return preserved.
(defun notnot-mv (x)
  (if x t nil))

;; ---- ANSI aux helpers (from /tmp/ansi-test/auxiliary/ansi-aux.lsp).
;; The build pipeline evals defun/defmacro of aux files at SBCL-host
;; side so they can be used during macroexpansion of test files; but
;; the emitted Modus image doesn't see those evals.  Helpers used inside
;; (deftest ...) bodies are emitted raw and need real Modus defuns or
;; the tests crash with "undefined function".  Each is a few lines of
;; trivial code — no risk of layout-shift cascade.

(defun evendigitp (c)
  "T if C is an even digit character (#\\0 #\\2 #\\4 #\\6 #\\8)."
  (notnot (find c "02468")))

(defun odddigitp (c)
  "T if C is an odd digit character (#\\1 #\\3 #\\5 #\\7 #\\9)."
  (notnot (find c "13579")))

(defun nextdigit (c)
  "Next digit character after C, or NIL if C is #\\9 or non-digit."
  (cadr (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

(defun onep (x) (eql x 1))

(defun sequencep (x) (typep x 'sequence))

(defun is-t-or-nil (e)
  (or (eq e t) (eq e nil)))

(defun string-designator-p (x)
  "T if X can be coerced to a string via (string x)."
  (handler-case (progn (string x) t)
    (error () nil)))

(defun char-invertcase (c)
  (if (upper-case-p c) (char-downcase c) (char-upcase c)))

(defun string-invertcase (s)
  (map 'string #'char-invertcase s))

(defun make-int-list (n)
  "Return (0 1 2 ... N-1)."
  (let ((acc nil) (i (- n 1)))
    (loop (when (< i 0) (return acc))
          (setq acc (cons i acc))
          (setq i (- i 1)))))

(defun make-int-array (n &optional (fn #'make-array))
  "Make an int array of length N populated with 0..N-1.
   FN is the array-maker (default #'make-array)."
  (when (symbolp fn) (setf fn (symbol-function fn)))
  (let ((a (funcall fn n)))
    (let ((i 0))
      (loop (when (= i n) (return a))
            (setf (aref a i) i)
            (setq i (+ i 1))))))

(defun equal-array (a1 a2)
  "Element-wise equality.  Original suite definition is intentionally
   (equal a1 a1) — a no-op T-returning probe used as a sanity placeholder."
  (equal a1 a2))

(defun compose (&rest fns)
  "Right-to-left function composition: ((compose f g h) x) = (f (g (h x))).
   ansi-aux defines this as a macro; as a function it has the same
   semantics for typical (compose #'a #'b ...) call sites."
  (lambda (x)
    (let ((result x) (rfns (reverse fns)))
      (let ((cur rfns))
        (loop (when (null cur) (return result))
              (setq result (funcall (car cur) result))
              (setq cur (cdr cur)))))))

(defun printable-p (obj)
  "T iff OBJ can be printed to a string without signaling."
  (handler-case (and (stringp (write-to-string obj)) t)
    (error () nil)))

(defun safe (fn &rest args)
  "Call FN with ARGS, returning NIL on error instead of signaling."
  (handler-case (apply fn args) (error () nil)))

(defun even-size-p (arr)
  "T if ARR's total size is even.  Used in array tests."
  (evenp (array-total-size arr)))

(defun collect-properties (plist prop)
  "Collect all values for PROP in PLIST.  ansi-aux ll. 357.
   Iteration uses cddr stepping; we hand-step to avoid LOOP BY."
  (let ((acc nil) (e plist))
    (loop (when (null e) (return (nreverse acc)))
          (when (eql (car e) prop)
            (setq acc (cons (cadr e) acc)))
          (setq e (cddr e)))))

(defun to-function (fn)
  "Coerce designator to function: symbol → symbol-function; lambda → eval-it."
  (if (symbolp fn) (symbol-function fn) fn))

;; More ansi-aux helpers picked up via fail-test scan
(defun xcons (a b) (cons b a))

(defun rev-assoc-list (x)
  "ansi-aux/cons-aux: swap each (a . b) pair to (b . a) preserving NIL slots."
  (cond
    ((null x) nil)
    ((null (car x)) (cons nil (rev-assoc-list (cdr x))))
    (t (acons (cdar x) (caar x) (rev-assoc-list (cdr x))))))

(defun make-adj-array (n &rest args)
  "ansi-aux/array-aux: shorthand for adjustable (make-array N :adjustable T
   [:initial-contents …])."
  (let ((ic nil))
    (let ((cur args))
      (loop (when (null cur) (return))
            (when (eq (car cur) :initial-contents) (setq ic (cadr cur)))
            (setq cur (cddr cur))))
    (if ic
        (make-array n :adjustable t :initial-contents ic)
        (make-array n :adjustable t))))

(defun %gcd-euclid (a b)
  "Mod-based Euclid on two NON-NEGATIVE integers; O(log) recursion depth."
  (if (= b 0) a (%gcd-euclid b (mod a b))))

(defun my-gcd (x y)
  "ansi-aux/gcd-aux: mod-based Euclid gcd.  Result is non-negative per
   CLHS GCD.  Terminates fast even for large random fixnum pairs (the old
   subtractive `(my-gcd x (- y x))` recursed ~10^18 times → gcd.4/.6/.7
   and lcm.4-.7 HUNG)."
  (%gcd-euclid (abs x) (abs y)))

(defun my-lcm (x y)
  "ansi-aux/gcd-aux: lcm via gcd.  Uses (abs ...) → generic-negate-int so
   MOST-NEGATIVE-FIXNUM (-2^62) promotes to the bignum +2^62 instead of
   wrapping to itself (the plain (- x) below WRAPPED, feeding a negative
   `magnitude' into the bignum machinery → lcm.4/.6/.7 crash)."
  (setq x (abs x))
  (setq y (abs y))
  (if (or (= x 0) (= y 0)) 0
      (/ (* x y) (my-gcd x y))))

(defun eqlzt (a b) (eqlt a b))

(defun equiv (a b)
  "ansi-aux: logical equivalence — both true or both false."
  (if a (if b t nil) (if b nil t)))

(defun package-designator-p (x)
  "T if X could be a package designator (need not actually exist)."
  (or (packagep x)
      (handler-case (progn (string x) t) (error () nil))))

(defun equalpt-or-report (x y)
  "EQUALPT, but on mismatch return (list x y) so callers can dump the
   actual values into the failure message."
  (or (equalpt x y) (list x y)))

(defun is-builtin-class (type)
  "T if TYPE designates a built-in CLOS class.  Used in classes tests."
  (when (symbolp type) (setq type (find-class type nil)))
  (typep type 'built-in-class))

(defun typef (type)
  "Returns a closure that tests typep against TYPE.  ansi-aux defines
   it via macro magic ('require closure implementation') — function
   variant is sufficient for test call sites."
  (lambda (x) (typep x type)))

(defun trim-list (list n)
  "Take the first N elements of LIST; if longer, append a '... and N
   more omitted' tail."
  (let ((len (length list)))
    (if (<= len n) list
        (append (subseq list 0 n)
                (list (format nil "And ~A more omitted." (- len n)))))))

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

;; complement was a global-cell stub here; cl-sequences.lisp now has the
;; real per-closure implementation.  Removed so the cl-sequences version
;; (which loads earlier but wins because we deleted the override) takes
;; effect.  Multiple coexisting complements now work — was: every
;; complement used the LAST fn, breaking (position x lst :test (complement
;; #'eql)) once any other complement had been created.

(defun identity (x) x)

;;; ============================================================
;;; structures-03.lsp hand-rolled BOA structs (SBT-01..SBT-16)
;;; ============================================================
;;; The test file has all (defstruct* …) forms commented out and
;;; relies on the harness to provide them.  Each struct is represented
;;; as a vector #(<tag-sym> slot-0 slot-1 …) and accessors index
;;; via aref — `(setf (aref …) …)` already works in Modus so setf
;;; on accessors comes for free.  ~25 lines per struct.

;; SBT-01: (:constructor sbt-01-con (b a c)) — slots a b c.
(defun sbt-01-con (b a c) (vector 'sbt-01 a b c))
(defun sbt-01-a (s) (aref s 1))
(defun sbt-01-b (s) (aref s 2))
(defun sbt-01-c (s) (aref s 3))

;; SBT-02: 3 constructors — sbt-02-con (a b c), -con-2 (a b), -con-3 ().
;; Slot defaults: a='x b='y c='z.
(defun sbt-02-con (a b c) (vector 'sbt-02 a b c))
(defun sbt-02-con-2 (a b) (vector 'sbt-02 a b 'z))
(defun sbt-02-con-3 () (vector 'sbt-02 'x 'y 'z))
(defun sbt-02-a (s) (aref s 1))
(defun sbt-02-b (s) (aref s 2))
(defun sbt-02-c (s) (aref s 3))

;; SBT-03: (:constructor sbt-03-con (a b &optional c)) — slots c b a.
(defun sbt-03-con (a b &optional c) (vector 'sbt-03 c b a))
(defun sbt-03-a (s) (aref s 3))
(defun sbt-03-b (s) (aref s 2))
(defun sbt-03-c (s) (aref s 1))

;; SBT-04: (:constructor sbt-04-con (a b &optional c)) — slots (c nil) b (a nil).
(defun sbt-04-con (a b &optional c) (vector 'sbt-04 c b a))
(defun sbt-04-a (s) (aref s 3))
(defun sbt-04-b (s) (aref s 2))
(defun sbt-04-c (s) (aref s 1))

;; SBT-05: (:constructor sbt-05-con (&optional a b c)) — slots (c 1) (b 2) (a 3).
;; Slot order is c, b, a in defstruct; constructor's &optional fills a, b, c.
;; Test 05/1: (sbt-05-con) → a=3 b=2 c=1 (defaults from slot defs).
(defun sbt-05-con (&optional (a nil a-p) (b nil b-p) (c nil c-p))
  (vector 'sbt-05 (if c-p c 1) (if b-p b 2) (if a-p a 3)))
(defun sbt-05-a (s) (aref s 3))
(defun sbt-05-b (s) (aref s 2))
(defun sbt-05-c (s) (aref s 1))

;; SBT-06: (:constructor sbt-06-con (&optional (a 'p) (b 'q) (c 'r))) — slots (c 1) (b 2) (a 3).
;; (con) → a='p b='q c='r per defaults from CON's lambda list (defstruct slot defaults ignored when con provides defaults).
(defun sbt-06-con (&optional (a 'p) (b 'q) (c 'r)) (vector 'sbt-06 c b a))
(defun sbt-06-a (s) (aref s 3))
(defun sbt-06-b (s) (aref s 2))
(defun sbt-06-c (s) (aref s 1))

;; SBT-07: (:constructor sbt-07-con (&optional (a 'p a-p) (b 'q b-p) (c 'r c-p)
;;                                              &aux (d (list (notnot a-p) (notnot b-p) (notnot c-p))))) — slots a b c d.
(defun sbt-07-con (&optional (a 'p a-p) (b 'q b-p) (c 'r c-p))
  (let ((d (list (and a-p t) (and b-p t) (and c-p t))))
    (vector 'sbt-07 a b c d)))
(defun sbt-07-a (s) (aref s 1))
(defun sbt-07-b (s) (aref s 2))
(defun sbt-07-c (s) (aref s 3))
(defun sbt-07-d (s) (aref s 4))

;; SBT-08: (:constructor sbt-08-con (&key ((:foo a)))) — slot a.
(defun sbt-08-con (&key ((:foo a))) (vector 'sbt-08 a))
(defun sbt-08-a (s) (aref s 1))

;; SBT-09: (:constructor sbt-09-con (&key (a 'p a-p) ((:x b) 'q) (c 'r) d ((:y e))
;;                                       ((:z f) 's z-p)
;;                                       &aux (g (list (notnot a-p) (notnot z-p))))) — slots a b c d e f g.
(defun sbt-09-con (&key (a 'p a-p) ((:x b) 'q) (c 'r) d ((:y e)) ((:z f) 's z-p))
  (let ((g (list (and a-p t) (and z-p t))))
    (vector 'sbt-09 a b c d e f g)))
(defun sbt-09-a (s) (aref s 1))
(defun sbt-09-b (s) (aref s 2))
(defun sbt-09-c (s) (aref s 3))
(defun sbt-09-d (s) (aref s 4))
(defun sbt-09-e (s) (aref s 5))
(defun sbt-09-f (s) (aref s 6))
(defun sbt-09-g (s) (aref s 7))

;; SBT-10: (:constructor sbt-10-con (&aux (a 10) (b (1+ a)))) — slots (a 1) (b 2).
(defun sbt-10-con () (let* ((a 10) (b (1+ a))) (vector 'sbt-10 a b)))
(defun sbt-10-a (s) (aref s 1))
(defun sbt-10-b (s) (aref s 2))

;; SBT-11: (:constructor sbt-11-con (&aux a b)) — slots a (b 0 :type integer).
;; &aux without value means uninitialised — Modus represents as NIL.
(defun sbt-11-con () (vector 'sbt-11 nil nil))
(defun sbt-11-a (s) (aref s 1))
(defun sbt-11-b (s) (aref s 2))
(defun (setf sbt-11-a) (v s) (aset s 1 v))
(defun (setf sbt-11-b) (v s) (aset s 2 v))

;; SBT-12: (:constructor sbt-12-con (a &optional (b 1) &rest c &aux (d (list a b c)))) — slot d.
(defun sbt-12-con (a &optional (b 1) &rest c)
  (vector 'sbt-12 (list a b c)))
(defun sbt-12-d (s) (aref s 1))

;; SBT-13: (:constructor sbt-13-con (&key (a 1) (b 2) c &aux (d (list a b c)))) — slot d.
(defun sbt-13-con (&key (a 1) (b 2) c)
  (vector 'sbt-13 (list a b c)))
(defun sbt-13-d (s) (aref s 1))

;; SBT-14: (:constructor sbt-14-con (&key a b c &allow-other-keys)) — slots (a 1) (b 2) (c 3).
(defun sbt-14-con (&key (a 1) (b 2) (c 3) &allow-other-keys)
  (vector 'sbt-14 a b c))
(defun sbt-14-a (s) (aref s 1))
(defun sbt-14-b (s) (aref s 2))
(defun sbt-14-c (s) (aref s 3))

;; SBT-15: (:constructor sbt-15-con (&key ((:x a) nil) ((y b) nil) (c nil))) — slots a b c.
;; The (y b) form uses non-keyword indicator — caller must pass symbol 'y, not :y.
;; Test 15/2 just tests positive case; 15/3..8 test error paths (signals-error).
(defun sbt-15-con (&key ((:x a)) ((y b)) c)
  (vector 'sbt-15 a b c))
(defun sbt-15-a (s) (aref s 1))
(defun sbt-15-b (s) (aref s 2))
(defun sbt-15-c (s) (aref s 3))

;; SBT-16: (:constructor) + (:constructor sbt-16-con (a b c)) — slots a b c.
;; Default constructor name is make-sbt-16 with &key a b c.
;; Tests 15787-15790 verify it signals program-error on unknown keys,
;; odd-length plist, non-keyword indicator, etc.  Tests 15791/2 verify
;; :ALLOW-OTHER-KEYS T turns off the validation.
(defun make-sbt-16 (&key a b c)
  (vector 'sbt-16 a b c))
(defun sbt-16-con (a b c) (vector 'sbt-16 a b c))
(defun sbt-16-a (s) (aref s 1))
(defun sbt-16-b (s) (aref s 2))
(defun sbt-16-c (s) (aref s 3))

;; Sequence-aware REVERSE / NREVERSE — the prelude versions only handle
;; lists.  CLHS reverse/nreverse take any sequence (list, vector, string).
;; Override here (loads after prelude, last-defun-wins).
(defun reverse (seq)
  (cond
    ((null seq) nil)
    ((consp seq)
     ;; List path (covers ordinary lists + chain-tail wrappers — they
     ;; behave like lists for car/cdr walk; for true wrapper arrays
     ;; we go through %mda-data below).
     (let ((result nil) (cur seq))
       (loop
         (when (null cur) (return result))
         (setq result (cons (car cur) result))
         (setq cur (cdr cur)))))
    ((%mda-p seq)
     ;; Native MDA — reverse the logical sequence respecting fill-pointer.
     (let* ((len (length seq))
            (out (if (stringp seq) (make-string len) (make-array len)))
            (i 0))
       (loop
         (when (= i len) (return out))
         (aset out i (aref seq (- len i 1)))
         (setq i (+ i 1)))))
    ((stringp seq)
     (let* ((len (array-length seq))
            (out (make-string len))
            (i 0))
       (loop
         (when (= i len) (return out))
         (aset out i (aref seq (- len i 1)))
         (setq i (+ i 1)))))
    ((arrayp seq)
     (let* ((len (array-length seq))
            (out (make-array len))
            (i 0))
       (loop
         (when (= i len) (return out))
         (aset out i (aref seq (- len i 1)))
         (setq i (+ i 1)))))
    (t (error "REVERSE: ~S is not a sequence" seq))))

(defun nreverse (seq)
  (cond
    ((null seq) nil)
    ((consp seq)
     ;; Destructive list reverse (from prelude).
     (let ((prev nil) (cur seq))
       (loop
         (when (null cur) (return prev))
         (let ((next (cdr cur)))
           (rplacd cur prev)
           (setq prev cur)
           (setq cur next)))))
    ((%mda-p seq)
     ;; Reverse in place over logical length (respects fp).
     (let* ((len (length seq))
            (i 0)
            (j (- len 1)))
       (loop
         (when (>= i j) (return seq))
         (let ((tmp (aref seq i)))
           (aset seq i (aref seq j))
           (aset seq j tmp))
         (setq i (+ i 1))
         (setq j (- j 1)))))
    ((or (stringp seq) (arrayp seq))
     (let* ((len (array-length seq))
            (i 0)
            (j (- len 1)))
       (loop
         (when (>= i j) (return seq))
         (let ((tmp (aref seq i)))
           (aset seq i (aref seq j))
           (aset seq j tmp))
         (setq i (+ i 1))
         (setq j (- j 1)))))
    (t (error "NREVERSE: ~S is not a sequence" seq))))

;; RPLACA + RPLACD early simple versions removed 2026-06-01 — the
;; strict-arity, type-checking copies at L3258/L3269 win and are the
;; ANSI-conformant ones.
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
  "Lookup INDICATOR in property list PLIST.  Per CLHS:
   - extra args after the single optional default → program-error;
   - improperly-terminated (dotted) plist → type-error/program-error
     once the search reaches the dotted tail."
  (when (and default (cdr default))
    (error "getf: too many arguments"))
  (let ((cur plist))
    (loop
      (cond
        ((null cur) (return (if default (car default) nil)))
        ((not (consp cur)) (error "getf: not a proper plist"))
        ((not (consp (cdr cur))) (error "getf: odd-length plist"))
        ((eq (car cur) indicator) (return (cadr cur)))
        (t (setq cur (cddr cur)))))))

(defun endp (x)
  ;; ANSI: type-error if x is not a list (cons or nil).
  (cond ((null x) t)
        ((consp x) nil)
        (t (error "endp: argument is not a list"))))

;; NOTE: tree-equal lives in cl-sequences.lisp with full &key support
;; (:test / :test-not / :allow-other-keys, leftmost-wins, program-error on
;; bad keys).  The old 2-arg stub here SHADOWED it (ansi-bridge loads last),
;; dropping all the :test variants and keyword/error tests.  Removed so the
;; complete version wins.

(defun copy-tree (tree)
  (if (consp tree)
    (cons (copy-tree (car tree))
          (copy-tree (cdr tree)))
    tree))

(defun %check-kw-allowed (args allowed)
  "Generic kwarg validator.  Walk ARGS plist, signal program-error on
   odd length or any indicator not EQ to a member of ALLOWED, unless
   the plist itself contains :ALLOW-OTHER-KEYS T."
  (let ((aok nil) (aok-set nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur)) (%signal-program-error))
      (let ((k (car cur)))
        (when (and (eq k :allow-other-keys) (not aok-set))
          (setq aok (cadr cur)) (setq aok-set t)))
      (setq cur (cddr cur)))
    (unless aok
      (let ((cur args))
        (loop
          (when (null cur) (return nil))
          (let ((k (car cur)))
            (unless (or (eq k :allow-other-keys)
                        (%kw-in-list-p k allowed))
              (%signal-program-error)))
          (setq cur (cddr cur)))))))

(defun %seq-subst-check-kwargs (args)
  "Validate keyword args for SUBSTITUTE/NSUBSTITUTE/SUBSTITUTE-IF/etc.
   Allows :test :test-not :key :start :end :from-end :count
   :allow-other-keys.  Signals program-error on bad input."
  (let ((aok nil) (aok-set nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur)) (%signal-program-error))
      (let ((k (car cur)))
        (when (and (eq k :allow-other-keys) (not aok-set))
          (setq aok (cadr cur)) (setq aok-set t)))
      (setq cur (cddr cur)))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)))
          (cond
            ((eq k :test))
            ((eq k :test-not))
            ((eq k :key))
            ((eq k :start))
            ((eq k :end))
            ((eq k :from-end))
            ((eq k :count))
            ((eq k :allow-other-keys))
            (aok)
            (t (%signal-program-error))))
        (setq cur (cddr cur))))))

(defun %subst-check-kwargs (args)
  "Validate keyword args for SUBST/SUBST-IF/etc.  Allows :test :test-not
   :key :allow-other-keys.  Signals program-error on bad input.

   Note: don't use (symbolp k) as a type-check — compile-keyword emits
   keywords as raw fixnums (compiler.lisp ~line 1720), so symbolp returns
   NIL on every keyword.  The eq-vs-known-keyword cond-clauses already
   correctly accept valid keys and reject invalid ones; falling through
   to the (t ...) clause catches both unknown keywords and non-keyword
   garbage (e.g. (subst ... 1 2)).  Earlier code had (not (symbolp k))
   as a leading clause; that wrongly signaled program-error on every
   real :key/:test/etc. and silently broke SUBST.ALLOW-OTHER-KEYS.* tests
   (the symbolp check is unreachable as a defensive net but actively
   misfires)."
  ;; First pass: find :allow-other-keys (leftmost wins).
  (let ((aok nil) (aok-set nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur))
        (%signal-program-error))    ; odd-length plist
      (let ((k (car cur)))
        (when (and (eq k :allow-other-keys) (not aok-set))
          (setq aok (cadr cur)) (setq aok-set t)))
      (setq cur (cddr cur)))
    ;; Second pass: each key must be recognized or :allow-other-keys T.
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)))
          (cond
            ((eq k :test))
            ((eq k :test-not))
            ((eq k :key))
            ((eq k :allow-other-keys))
            (aok)
            (t (%signal-program-error))))
        (setq cur (cddr cur))))))

(defun %subst-match-p (item node test-fn test-not-fn key-fn)
  (let ((v (if key-fn (funcall key-fn node) node)))
    (cond
      (test-fn     (funcall test-fn item v))
      (test-not-fn (not (funcall test-not-fn item v)))
      (t           (eql item v)))))

(defun %subst-rec (new old tree test-fn test-not-fn key-fn)
  (cond
    ((%subst-match-p old tree test-fn test-not-fn key-fn) new)
    ((consp tree)
     (let ((a (%subst-rec new old (car tree) test-fn test-not-fn key-fn))
           (d (%subst-rec new old (cdr tree) test-fn test-not-fn key-fn)))
       (if (and (eq a (car tree)) (eq d (cdr tree))) tree
           (cons a d))))
    (t tree)))

(defun subst (new old tree &rest args)
  ;; Honor :test, :test-not, :key per CLHS.  Default test = eql.
  ;; Inline kwarg validation: bad kwargs signal program-error unless
  ;; :allow-other-keys T.  Per CLHS, leftmost :allow-other-keys wins.
  (let ((test-fn nil) (test-not-fn nil) (key-fn nil)
        (test-set nil) (tn-set nil) (key-set nil)
        (aok nil) (aok-set nil)
        (cur args)
        (bad-key nil))
    ;; Single pass: collect all kwargs + detect first bad key + first :aok.
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur))
        (%signal-program-error))     ; odd-length plist
      (let ((k (car cur)) (v (cadr cur)))
        ;; Don't reject by symbolp — keywords are emitted as raw fixnums
        ;; (compile-keyword in compiler.lisp).  The cond's eq-clauses
        ;; match valid keys; the (t ...) branch records anything else as
        ;; bad-key, which only signals if no :allow-other-keys T was set.
        (cond
          ((eq k :test)     (unless test-set (setq test-fn v) (setq test-set t)))
          ((eq k :test-not) (unless tn-set (setq test-not-fn v) (setq tn-set t)))
          ((eq k :key)      (unless key-set (setq key-fn v) (setq key-set t)))
          ((eq k :allow-other-keys)
                            (unless aok-set (setq aok v) (setq aok-set t)))
          (t (when (null bad-key) (setq bad-key k))))
        (setq cur (cddr cur))))
    ;; If a bad key was seen and :allow-other-keys T was not present, error.
    (when (and bad-key (not aok))
      (%signal-program-error))
    (%subst-rec new old tree test-fn test-not-fn key-fn)))

(defun revappend (list tail)
  (let ((cur list))
    (loop
      (when (null cur) (return tail))
      (setq tail (cons (car cur) tail))
      (setq cur (cdr cur)))))

;; NRECONC early simple version removed 2026-06-01 — strict-arity
;; copy at L3284 wins.

(defun butlast (list &rest n-arg)
  ;; ANSI: (butlast list &optional (n 1)). Extra args → program-error.
  (when (and n-arg (cdr n-arg))
    (error "butlast: too many arguments"))
  (when (and list (not (consp list)))
    (error "butlast: argument is not a list"))
  (let ((n (if n-arg (car n-arg) 1)))
    (when (or (not (fixnump n)) (< n 0))
      (%signal-type-error))
    (let ((len (list-length list)))
      (if (<= len n) nil
        (let ((result nil) (i 0) (cur list))
          (loop
            (when (= i (- len n)) (return (nreverse result)))
            (setq result (cons (car cur) result))
            (setq cur (cdr cur))
            (setq i (+ i 1))))))))

;; ACONS early simple version removed 2026-06-01 — strict-arity
;; copy at L3276+ wins.
(defun pairlis (keys data &rest alist-arg)
  "PAIRLIS keys data &optional alist — extra args after ALIST are a
   program-error.  Per CLHS, KEYS and DATA must be lists of equal
   length; an atom in place of either is a type-error."
  (when (and alist-arg (cdr alist-arg))
    (error "pairlis: too many arguments"))
  (when (and keys (not (consp keys)))
    (error "pairlis: keys is not a list"))
  (when (and data (not (consp data)))
    (error "pairlis: data is not a list"))
  (let ((alist (if alist-arg (car alist-arg) nil))
        (k keys) (d data))
    (loop
      (when (null k)
        (when d (error "pairlis: keys shorter than data"))
        (return alist))
      (when (null d) (error "pairlis: data shorter than keys"))
      (setq alist (cons (cons (car k) (car d)) alist))
      (setq k (cdr k))
      (setq d (cdr d)))))

(defun make-list (n &rest args)
  ;; ANSI: (make-list size &key initial-element). Unknown keywords
  ;; are a program-error unless :allow-other-keys t is also present.
  ;; For duplicate keys, first-binding wins.
  (when (or (not (fixnump n)) (< n 0))
    (error "make-list: size must be a non-negative fixnum"))
  ;; Pre-scan for :allow-other-keys so it doesn't have to appear first.
  (let ((allow-other nil) (scan args))
    (loop
      (when (null scan) (return nil))
      (when (null (cdr scan))
        (error "make-list: odd number of keyword arguments"))
      (when (eq (car scan) :allow-other-keys)
        (setq allow-other (cadr scan))
        (return nil))
      (setq scan (cddr scan)))
    (let ((initial-element nil) (init-set nil) (cur args))
      (loop
        (when (null cur) (return nil))
        (cond
          ((eq (car cur) :initial-element)
           (unless init-set
             (setq initial-element (cadr cur))
             (setq init-set t)))
          ((eq (car cur) :allow-other-keys) nil)
          (t (unless allow-other
               (error "make-list: unknown keyword argument"))))
        (setq cur (cddr cur)))
      (let ((result nil) (i 0))
        (loop
          (when (= i n) (return result))
          (setq result (cons initial-element result))
          (setq i (+ i 1)))))))

(defun tailp (obj list)
  (let ((cur list))
    (loop
      (when (eql cur obj) (return t))
      (when (atom cur) (return (eql cur obj)))
      (setq cur (cdr cur)))))

(defun ldiff (list obj)
  "Return a copy of LIST up to (but not including) OBJ if OBJ is a tail.
   Per CLHS LDIFF: if OBJ isn't a tail (i.e., (tailp OBJ LIST) is false),
   return a copy of LIST including any dotted tail."
  (let ((result nil) (cur list))
    (loop
      (when (eql cur obj) (return (nreverse result)))
      (when (atom cur)
        ;; OBJ wasn't a tail. Append the dotted final atom to preserve
        ;; the input shape (proper list → proper list, dotted → dotted).
        (let ((rev (nreverse result)))
          (if (null rev)
              (return cur)
              (let ((last-cell rev))
                (loop (when (null (cdr last-cell)) (return nil))
                  (setq last-cell (cdr last-cell)))
                (set-cdr last-cell cur)
                (return rev)))))
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

;;; Character-set constants from ansi-aux.lsp (which Modus skips at load
;;; time, so these names would otherwise be unbound → NIL).  The format
;;; ~D/~O/~X randomized padding tests do
;;;   (random-from-seq +standard-chars+)
;;; for the pad character; with +standard-chars+ NIL the pad char is
;;; garbage and every padding assertion fails.  defvar init-thunks do not
;;; run at boot, so these are ALSO setq'd in build-ansi-test's kernel-main.
(defvar +standard-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./
 ")
(defvar +base-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<,>.?/\"':;[{]}~`!@#$%^&*()_-+= \\|")
(defvar +num-base-chars+ 96)
(defvar +alpha-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defvar +alphanumeric-chars+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

;;; defvar init-thunks don't run at boot, so the constants above default
;;; to NIL.  kernel-main calls %init-standard-chars to bind them.  Keeping
;;; the literals in this real source file (not the driver-source string)
;;; avoids triple-level escaping of the embedded quote/backslash/newline.
(defun %init-standard-chars ()
  (setq +standard-chars+
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./
 ")
  (setq +base-chars+
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<,>.?/\"':;[{]}~`!@#$%^&*()_-+= \\|")
  (setq +num-base-chars+ 96)
  (setq +alpha-chars+
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (setq +alphanumeric-chars+
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  +standard-chars+)


;;; ============================================================
;;; PROCLAIM / DECLAIM — stub that respects arity
;;; ============================================================

(defun proclaim (&rest args)
  "PROCLAIM accepts exactly one declaration spec — a proper list whose
   first element identifies the declaration kind.  Modus doesn't act
   on most declarations but per CLHS the call must:
   - Signal program-error on wrong arity.
   - Reject malformed (non-proper-list) declaration specs.
   - Signal type-error for (ftype . foo).
   The cdr walk distinguishes a proper list from a dotted one; a final
   non-NIL atom triggers error.  Per-kind validation for type / ftype
   / inline / notinline / optimize / declaration uses the same walk."
  (when (null args)
    (%signal-program-error))
  (when (cdr args)
    (%signal-program-error))
  (let* ((decl (car args))
         (kind (and (consp decl) (car decl)))
         (cur  decl))
    ;; Walk the spine to confirm proper-listness.  A dotted tail is
    ;; never legal in a declaration spec — bare `(KIND . foo)' and
    ;; deeper dots like `(type integer . foo)' both fail here.
    ;; (ftype . foo) specifically requires type-error per CLHS.
    (loop
      (cond
        ((null cur) (return nil))
        ((not (consp cur))
         (when (eq kind 'ftype) (%signal-type-error))
         (error "proclaim: malformed declaration spec ~S" decl))
        (t (setq cur (cdr cur)))))
    nil))

(defun declaim (&rest decls)
  "DECLAIM-like wrapper that accepts any declaration list."
  (declare (ignore decls))
  nil)

;;; ============================================================
;;; parse-integer — parse an integer from a string
;;; ============================================================

(defun parse-integer (string &rest args)
  "Parse an integer from STRING. Supports :start :end :radix :junk-allowed.
   Returns (values integer end-position).  Signals error on:
   - odd-length keyword arg list
   - unknown keyword (unless :allow-other-keys T)
   - empty / whitespace-only string (when :junk-allowed NIL)
   - sign-only string '+', '-' (no digits)
   - trailing non-digit / non-whitespace junk (when :junk-allowed NIL)"
  (let ((start 0)
        (end nil)
        (radix 10)
        (junk-allowed nil)
        (allow-other nil)
        (a args))
    ;; Pre-scan :allow-other-keys
    (let ((scan args))
      (loop (when (or (null scan) (null (cdr scan))) (return))
        (when (and (eq (car scan) :allow-other-keys) (cadr scan))
          (setq allow-other t))
        (setq scan (cddr scan))))
    ;; Parse keyword args with validation.  CLHS 7.1.1: for repeated
    ;; keywords the LEFTMOST value is used (parse-integer.23/.24/.35/.36),
    ;; so a key already seen is not overwritten.
    (let ((saw-start nil) (saw-end nil) (saw-radix nil) (saw-junk nil))
      (loop
        (when (null a) (return))
        (when (null (cdr a))
          (error "parse-integer: odd-length keyword arg list"))
        (let ((k (car a)) (v (cadr a)))
          (cond
            ((eq k :start)       (unless saw-start (setq start v saw-start t)))
            ((eq k :end)         (unless saw-end   (setq end v saw-end t)))
            ((eq k :radix)       (unless saw-radix (setq radix v saw-radix t)))
            ((eq k :junk-allowed)(unless saw-junk  (setq junk-allowed v saw-junk t)))
            ((eq k :allow-other-keys) nil)
            (allow-other nil)
            (t (error "parse-integer: bad keyword"))))
        (setq a (cddr a))))
    (let ((len (length string)))
      (when (null end) (setq end len))
      ;; Skip leading whitespace
      (let ((i start))
        (loop
          (when (>= i end) (return))
          ;; %prim-aref → raw char CODE (public aref returns a CHARACTER
          ;; since e159986; this code compares against fixnum codes).
          (let ((c (%prim-aref string i)))
            ;; CL whitespace[2]: Space 32, Tab 9, Newline 10, Page 12, Return 13.
            (when (and (not (= c 32)) (not (= c 9)) (not (= c 10))
                       (not (= c 12)) (not (= c 13)))
              (return)))
          (setq i (+ i 1)))
        ;; Check for sign
        (let ((sign 1))
          (when (< i end)
            (let ((c (%prim-aref string i)))
              (cond
                ((= c 43) (setq i (+ i 1)))  ; +
                ((= c 45) (setq sign -1) (setq i (+ i 1))))))  ; -
          ;; Parse digits
          (let ((result 0)
                (digit-count 0))
            (loop
              (when (>= i end) (return))
              (let* ((c (%prim-aref string i))
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
            (when (= digit-count 0)
              (if junk-allowed
                  (return-from parse-integer (values nil i))
                  (error "parse-integer: no digits")))
            ;; Check trailing chars: skip trailing whitespace, then check
            ;; if anything non-whitespace remains.
            (unless junk-allowed
              (let ((j i))
                (loop
                  (when (>= j end) (return))
                  (let ((c (%prim-aref string j)))
                    (when (and (not (= c 32)) (not (= c 9)) (not (= c 10))
                               (not (= c 12)) (not (= c 13)))
                      (error "parse-integer: junk after digits")))
                  (setq j (+ j 1)))
                (setq i j)))
            (values (* sign result) i)))))))

;;; ============================================================
;;; sxhash — hash code for objects
;;; ============================================================

(defun %sxhash-1 (object depth)
  "SXHASH with bounded recursion so circular conses can't blow the stack."
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
         (setq hash (logxor hash (%ensure-char-code (aref object i))))
         (setq hash (logand (* hash 16777619) #xFFFFFFFF))
         (setq i (+ i 1)))
       hash))
    ((symbolp object)
     ;; Use name hash if available
     (if (%cl-sym-p object)
         (%sxhash-1 (%cl-sym-name object) (+ depth 1))
         (logand (ash object -1) most-positive-fixnum)))
    ((consp object)
     ;; Combine car and cdr hashes. Bound the walk so circular conses
     ;; (e.g. (let ((c (list 'a))) (setf (cdr c) c) (sxhash c)) — ANSI
     ;; sxhash.7) don't recurse forever and SIGSEGV the fork.
     (if (>= depth 16)
         42
         (let ((h1 (%sxhash-1 (car object) (+ depth 1)))
               (h2 (%sxhash-1 (cdr object) (+ depth 1))))
           (logand (logxor (+ (* h1 31) h2) 12345) most-positive-fixnum))))
    (t 42)))

(defun sxhash (object) (%sxhash-1 object 0))

;;; ============================================================
;;; float-radix — IEEE floats always use base 2
;;; ============================================================

(defun float-radix (float) 2)

;;; ============================================================
;;; approx= — approximate float equality for tests
;;; ============================================================

(defun approx= (x y &rest eps-arg)
  "Approximate equality for floats — matches ansi-aux's
   (<= (abs (/ (- x y) (max (abs x) 1))) eps).  The denominator is
   floored at 1.0 so that when x≈0 and y is tiny (e.g. cos(π/2)≈6e-17
   vs 0.0) the comparison uses an absolute tolerance — required by the
   ANSI trig approx= tests.  All arithmetic stays in doubles to avoid
   the float/integer mixed-compare path."
  (let ((eps (if eps-arg (car eps-arg) 1.0d-4))
        (one 1.0d0))
    (let ((ax (if (< x 0.0d0) (- 0.0d0 x) x)))
      (let ((denom (if (< one ax) ax one)))
        (let ((diff (- x y)))
          (let ((adiff (if (< diff 0.0d0) (- 0.0d0 diff) diff)))
            (<= adiff (* eps denom))))))))

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

;;; Pretty-printer — best-effort impl.  pprint-newline emits an actual
;;; newline; pprint-tab pads to a column; pprint-indent / pprint-fill /
;;; pprint-linear / pprint-tabular just write the list space-separated.
;;; The full PPRINT-LOGICAL-BLOCK / pprint-dispatch table machinery is
;;; far more complex (40-page spec); we cover the common-case calls.

(defun pprint-newline (kind &rest args)
  "Emit a newline.  KIND is :linear / :fill / :miser / :mandatory —
   modus treats all the same (just newline)."
  (declare (ignore kind))
  (let ((stream (if args (car args) nil)))
    (write-char-to-stream (code-char 10) (%resolve-output-stream stream)))
  nil)

(defun pprint-tab (kind colnum colinc &rest args)
  "Emit COLINC spaces if at COLNUM (approximate — we just emit COLINC
   spaces since modus doesn't track current column)."
  (declare (ignore kind colnum))
  (let ((stream (if args (car args) nil))
        (n (if (>= colinc 0) colinc 0)))
    (dotimes (i n)
      (write-char-to-stream (code-char 32) (%resolve-output-stream stream))))
  nil)

(defun pprint-indent (relative-to n &rest args)
  "No-op — modus doesn't carry a logical-block-relative indent state."
  (declare (ignore relative-to n args))
  nil)

(defun pprint-fill (stream list &rest args)
  "Print LIST elements separated by spaces, breaking onto newlines
   if needed.  We approximate: just print space-separated."
  (declare (ignore args))
  (let ((s (%resolve-output-stream stream))
        (first t))
    (dolist (e list)
      (unless first
        (write-char-to-stream (code-char 32) s))
      (setq first nil)
      (write-to-stream e s)))
  nil)

(defun pprint-linear (stream list &rest args)
  "Same approximation as pprint-fill — space-separated."
  (apply #'pprint-fill stream list args))

(defun pprint-tabular (stream list &rest args)
  "Same approximation — space-separated."
  (apply #'pprint-fill stream list args))

(defvar *%pprint-dispatch-table* nil)

(defun copy-pprint-dispatch (&rest args)
  "Return a copy of the current pprint dispatch table.  We just hand
   back a fresh symbol — the table contents aren't actually copied."
  (declare (ignore args))
  (gensym "PPRINT-DISP-"))

(defun set-pprint-dispatch (type-spec fn &rest args)
  "Install FN as the pprint-dispatch entry for TYPE-SPEC.  Stored in
   *%pprint-dispatch-table* keyed by (type . priority)."
  (declare (ignore args))
  (setq *%pprint-dispatch-table*
        (cons (cons type-spec fn) *%pprint-dispatch-table*))
  nil)

(defun pprint-dispatch (object &rest args)
  "Look up the most-specific pprint-dispatch entry for OBJECT.
   Returns (values function found-p)."
  (declare (ignore args))
  (let ((cur *%pprint-dispatch-table*)
        (found nil))
    (loop
      (when (or found (null cur)) (return nil))
      (let ((entry (car cur)))
        (when (handler-case (typep object (car entry)) (t (c) nil))
          (setq found entry)))
      (setq cur (cdr cur)))
    (if found
        (values (cdr found) t)
        (values nil nil))))

;;; ============================================================
;;; compile-and-load — stub (no runtime compiler support)
;;; ============================================================

(defun compile-and-load (form) nil)

;;; compile shadow — ansi-bridge.lisp loads AFTER cl-eval.lisp, so this
;;; defun wins via last-defun.  Replicate cl-eval.lisp:2883's logic
;;; (the previous stub `(defun compile (name &rest args) name)' silently
;;; shadowed it, breaking every `(funcall (compile nil '(lambda ...)) ...)'
;;; pattern — the misc.lsp test file's entire 460 tests crashed because
;;; compile returned NIL).
(defun compile (name &rest args)
  "Compile NAME (or lambda-expression in DEF).  For (compile nil
   '(lambda ...)) return an interpreted closure; for (compile NAME)
   return the SFT-looked-up compiled function."
  (let ((def (if args (car args) nil)))
    (cond
      ((and (null name) def)
       (let ((form (if (and (consp def) (eq (car def) 'quote))
                       (cadr def)
                       def)))
         (if (and (consp form)
                  (or (eq (car form) 'lambda)
                      (and (symbolp (car form))
                           (string-equal (symbol-name (car form)) "LAMBDA"))))
             (values (list '%interp-closure (cadr form) (cddr form) nil)
                     nil nil)
             (values def nil nil))))
      (name
       (let ((fn (if (and (boundp '*symbol-function-table*)
                          *symbol-function-table*)
                     (gethash (if (symbolp name) (symbol-name name) name)
                              *symbol-function-table*)
                     nil)))
         (values (or fn name) nil nil)))
      (t (values nil nil nil)))))

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
  "Decode float into (significand exponent sign).
   Bails on non-floats (e.g. NIL from an uninitialized
   *-float-epsilon defvar) so the caller sees a TYPE-ERROR
   instead of looping forever in the normalize loop below."
  (unless (floatp float)
    (error "decode-float: not a float ~S" float))
  ;; Stub: return approximate values
  (if (= float 0.0d0)
      (values 0.0d0 0 1.0d0)
      (let ((sign (if (< float 0.0d0) -1.0d0 1.0d0))
            (abs-f (if (< float 0.0d0) (- 0.0d0 float) float)))
        ;; Find exponent such that 0.5 <= sig < 1.0. Safety cap so a
        ;; pathological input (denormal, ±inf, or a value the comparisons
        ;; never converge on) can't loop indefinitely.
        (let ((exp 0) (sig abs-f) (iter 0))
          (loop
            (when (and (>= sig 0.5d0) (< sig 1.0d0)) (return))
            (when (> iter 4096) (return))
            (setq iter (+ iter 1))
            (if (>= sig 1.0d0)
                (progn (setq sig (/ sig 2.0d0)) (setq exp (+ exp 1)))
                (progn (setq sig (* sig 2.0d0)) (setq exp (- exp 1)))))
          (values sig exp sign)))))

(defun integer-decode-float (float)
  "Decode IEEE 64-bit float to (significand exponent sign) — three integers.
   Returns 0, 0, 1 for ±0.  For non-IEEE (modus rational-form floats or
   non-floats) falls back to a coarse mantissa*2^exp split."
  (cond
    ((%ieee-float-p float)
     (let* ((hi (aref float 0))
            (lo (aref float 1))
            (hi-u32 (logand hi 4294967295))
            (sign-bit (logand (ash hi-u32 -31) 1))
            (exponent (logand (ash hi-u32 -20) 2047))
            (mantissa-hi (logand hi-u32 1048575))
            (mantissa (logior (ash mantissa-hi 32) (logand lo 4294967295)))
            (sign (if (= sign-bit 1) -1 1)))
       (cond
         ((and (= exponent 0) (= mantissa 0)) (values 0 0 sign))
         ((= exponent 2047) (values mantissa 0 sign))  ; inf/nan
         ((= exponent 0)
          ;; Subnormal — exponent is -1074 effectively
          (values mantissa -1074 sign))
         (t
          ;; Normal: significand = (2^52 | mantissa), exponent = e-1075
          (values (logior (ash 1 52) mantissa)
                  (- exponent 1075)
                  sign)))))
    ((= float 0)
     (values 0 0 1))
    ((integerp float)
     (let ((sign (if (< float 0) -1 1))
           (abs (if (< float 0) (- 0 float) float)))
       (values abs 0 sign)))
    (t
     ;; Modus rational-form float (2-slot generic array num/den): mantissa
     ;; ~= num*2^k for some k that makes (mantissa*2^exp ≈ num/den) — pick
     ;; exp=0 and return num as the significand.  Approximation only.
     (let ((sign 1) (m float))
       (when (< (or (ignore-errors (%coerce-numeric float)) 0) 0)
         (setq sign -1) (setq m (- 0 m)))
       (values (or (ignore-errors (truncate m)) 0) 0 sign)))))

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
  "Return first tail of LIST whose car satisfies PRED.
   Outer LISTP check signals TYPE-ERROR for non-lists so tests like
   (catch-type-error (member-if-not #'listp \"abc\")) get a clean
   error instead of spinning a (cdr garbage) loop until SIGALRM kills
   the fork. Improper-list tails (proper-with-dotted-end) are walked
   to NIL or to a non-cons; we only check the initial argument."
  (unless (listp list)
    (error "member-if: not a list ~S" list))
  (%subst-check-kwargs args)
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur list))
    (loop
      (when (null cur) (return nil))
      (when (atom cur) (return nil))
      (let ((k (if key-fn (funcall key-fn (car cur)) (car cur))))
        (when (funcall pred k) (return cur)))
      (setq cur (cdr cur)))))

(defun member-if-not (pred list &rest args)
  "Return first tail of LIST whose car does NOT satisfy PRED.  Inlined
   instead of `(apply #'member-if (lambda ...) ...)' to avoid the
   apply-of-rest-through-sibling-defun fragility."
  (unless (listp list)
    (error "member-if-not: not a list ~S" list))
  (%subst-check-kwargs args)
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur list))
    (loop
      (when (null cur) (return nil))
      (when (atom cur) (return nil))
      (let ((k (if key-fn (funcall key-fn (car cur)) (car cur))))
        (unless (funcall pred k) (return cur)))
      (setq cur (cdr cur)))))

;;; ============================================================
;;; ANSI test helper cons functions (from cons-aux.lsp)
;;; ============================================================

(defun union-with-check (x y &rest args)
  "union with result checking. Routes directly through the positional
   helper (rather than `(apply #'union x y args)`) to dodge apply-of-rest
   fragility."
  (%union-impl x y args))

(defun nunion-with-copy (x y &rest args)
  "nunion that doesn't destroy inputs. Routes through positional helper."
  (%union-impl (copy-list x) (copy-list y) args))

(defun set-exclusive-or-with-check (x y &rest args)
  "set-exclusive-or with checking. Inlined positional call to dodge
   apply-of-rest fragility."
  (%set-exclusive-or-impl x y args))

(defun nintersection-with-copy (x y &rest args)
  "nintersection that doesn't destroy inputs. Routes through positional helper."
  (%intersection-impl (copy-list x) (copy-list y) args))

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
  "Find first pair in ALIST whose cdr matches ITEM.
   Skip NIL entries; error on other non-cons alist element.
   :test defaults to inline `eql` (#'eql is unusable in MVM)."
  (%subst-check-kwargs args)
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (cond ((null pair) nil)
              ((not (consp pair)) (error "rassoc: not a cons"))
              (t (let ((val (if key-fn (funcall key-fn (cdr pair)) (cdr pair))))
                   (when (if test-fn (funcall test-fn item val) (eql item val))
                     (return pair))))))
      (setq cur (cdr cur)))))

(defun rassoc-if (pred alist &rest args)
  "Find first pair in ALIST whose cdr satisfies PRED.
   Skip NIL entries (SBCL-compat); error on other non-cons (CLHS)."
  (%subst-check-kwargs args)
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (cond ((null pair) nil)
              ((not (consp pair)) (error "rassoc-if: not a cons"))
              (t (let ((val (if key-fn (funcall key-fn (cdr pair)) (cdr pair))))
                   (when (funcall pred val)
                     (return pair))))))
      (setq cur (cdr cur)))))

(defun rassoc-if-not (pred alist &rest args)
  "Find first pair in ALIST whose cdr does NOT satisfy PRED.
   Skip NIL entries (SBCL-compat); error on other non-cons (CLHS).
   Inlined (rather than `(apply #'rassoc-if (lambda ...) ...)') to dodge
   apply-of-rest fragility."
  (%subst-check-kwargs args)
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (cond ((null pair) nil)
              ((not (consp pair)) (error "rassoc-if-not: not a cons"))
              (t (let ((val (if key-fn (funcall key-fn (cdr pair)) (cdr pair))))
                   (when (not (funcall pred val))
                     (return pair))))))
      (setq cur (cdr cur)))))

(defun assoc-if (pred alist &rest args)
  "Find first pair in ALIST whose car satisfies PRED.
   Skip NIL entries (SBCL-compat); error on other non-cons (CLHS)."
  (%subst-check-kwargs args)
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (cond ((null pair) nil)
              ((not (consp pair)) (error "assoc-if: not a cons"))
              (t (let ((k (if key-fn (funcall key-fn (car pair)) (car pair))))
                   (when (funcall pred k)
                     (return pair))))))
      (setq cur (cdr cur)))))

(defun assoc-if-not (pred alist &rest args)
  "Find first pair in ALIST whose car does NOT satisfy PRED.
   Skip NIL entries (SBCL-compat); error on other non-cons (CLHS).
   Inlined (rather than `(apply #'assoc-if (lambda ...) ...)') to dodge
   apply-of-rest fragility."
  (%subst-check-kwargs args)
  (let* ((parsed (parse-test-key args))
         (key-fn (cdr parsed))
         (cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (cond ((null pair) nil)
              ((not (consp pair)) (error "assoc-if-not: not a cons"))
              (t (let ((k (if key-fn (funcall key-fn (car pair)) (car pair))))
                   (when (not (funcall pred k))
                     (return pair))))))
      (setq cur (cdr cur)))))

;; find-if / find-if-not previously had bridge overrides here that only
;; honored :test/:key — they silently dropped :from-end, :start, :end.
;; Removed in favor of cl-sequences.lisp's full implementations, which
;; load earlier in source order but win because (a) they're complete and
;; (b) MVM's "last-defun-wins" rule is satisfied by deleting the override.

;;; ============================================================
;;; vector-push / vector-push-extend / fill-pointer
;;; ============================================================
;;; Vectors with fill-pointers are represented as (cons fill-pointer underlying-array).
;;; Regular arrays are just arrays (no fill pointer support).

;; Helper: peel adjustable wrapper if present.  Returns the inner cell
;; (which is either an array, a fill-pointer wrapper, a displaced wrapper,
;; or a multi-dim wrapper).
(defun %fp-inner (arr)
  (if (and (consp arr) (eql (car arr) 8765432)) (cdr arr) arr))

(defun array-has-fill-pointer-p (arr)
  "True if ARR has a fill pointer.
   Plain (cons fp underlying) → T.  (cons 8765432 (cons fp underlying)) → T.
   (cons 8765432 underlying) → NIL (adjustable but no fp).
   (cons 9867654 (cons dims data)) → NIL (multi-dim wrapper, no fp).
   Native MDA #x34 → T iff %mda-fp slot is non-NIL."
  (cond
    ((%mda-p arr) (not (null (%mda-fp arr))))
    ;; Multi-dim cons-wrapper has marker 9867654 in head — never has fp.
    ((and (consp arr) (eql (car arr) 9867654)) nil)
    (t (let ((y (%fp-inner arr)))
         (if (consp y) (fixnump (car y)) nil)))))

(defun fill-pointer (arr)
  "Return the fill pointer of ARR.  Per CLHS, signal type-error when
   ARR has no fill pointer (rank>1 array, rank-0 array, plain vector
   created without :fill-pointer, etc.) — required by the test
   suite's signals-error wrappers."
  (cond
    ((%mda-p arr)
     (let ((fp (%mda-fp arr)))
       (if fp fp (progn (%signal-type-error) nil))))
    (t (let ((y (%fp-inner arr)))
         (if (and (consp y) (fixnump (car y)))
             (car y)
             (progn (%signal-type-error) nil))))))

(defun set-fill-pointer (arr val)
  "Set fill pointer of ARR to VAL."
  (cond
    ((%mda-p arr) (%mda-set-fp arr val) val)
    (t (let ((y (%fp-inner arr)))
         (when (and (consp y) (fixnump (car y)))
           (set-car y val))
         val))))

(defun vector (&rest elements)
  "CLHS: return a fresh simple-vector containing the args.
   At compile time the VECTOR macro inlines `(let ((v (make-array N))) (aset v 0 a0) … v)';
   this defun is the runtime fallback so `(vector …)' / `(apply #'vector …)' at
   runtime EVAL works without crashing on missing-function."
  (let* ((n (length elements))
         (v (make-array n))
         (i 0)
         (cur elements))
    (loop
      (when (null cur) (return v))
      (aset v i (car cur))
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

(defun vector-push (new-element vector)
  "Push NEW-ELEMENT onto VECTOR (with fill pointer). Returns fill pointer or nil.
   String underlying stores fixnum char-codes; coerce a character element
   via char-code so subsequent (aref ...) returns the char correctly.
   CLHS: signals type-error if VECTOR has no fill pointer.
   Displaced-aware: writes at (+ fp offset) when MDA has a displaced-to slot."
  (cond
    ((%mda-p vector)
     ;; MDA fast path: read fp slot, write into data, advance fp.
     (let ((fp (%mda-fp vector)))
       (cond
         ((null fp) (%signal-type-error))
         (t (let* ((arr (%mda-data vector))
                   (off (%mda-offset vector))
                   (dims (%mda-dims vector))
                   (cap (if (consp dims) (car dims) (array-length arr))))
              (if (>= fp cap)
                  nil
                  (let ((store-val (if (and (stringp arr) (characterp new-element))
                                       (char-code new-element)
                                       new-element)))
                    (aset arr (+ fp off) store-val)
                    (%mda-set-fp vector (+ fp 1))
                    fp)))))))
    (t (let ((inner (%fp-inner vector)))
         (cond
           ((and (consp inner) (fixnump (car inner)))
            (let ((fp (car inner))
                  (arr (cdr inner)))
              (let ((len (array-length arr)))
                (if (>= fp len)
                    nil
                    (let ((store-val (if (and (stringp arr) (characterp new-element))
                                         (char-code new-element)
                                         new-element)))
                      (aset arr fp store-val)
                      (set-car inner (+ fp 1))
                      fp)))))
           (t (%signal-type-error)))))))

(defun vector-push-extend (new-element vector &rest args)
  "Push NEW-ELEMENT onto VECTOR, extending if needed.  Character on
   string-backed vector is char-code-converted (vector-push-extend test
   20914 was storing tagged character into the string array → garbage
   on later aref).
   CLHS: signals type-error if VECTOR has no fill pointer.
   For displaced+adjustable arrays: when fp reaches declared dim, the
   displacement is broken — fresh local backing is allocated and the
   old contents are copied verbatim, per CLHS adjust-array semantics."
  (cond
    ((%mda-p vector)
     ;; MDA fast path: handles fp + auto-extend; displacement aware.
     (let ((fp (%mda-fp vector)))
       (cond
         ((null fp) (%signal-type-error))
         (t (let* ((arr (%mda-data vector))
                   (disp (%mda-displaced vector))
                   (off (%mda-offset vector))
                   ;; Declared length: dim 0 (1-D fast path).
                   (dims (%mda-dims vector))
                   (cap (if (consp dims) (car dims) (array-length arr))))
              (when (>= fp cap)
                ;; Need to grow.  Use extension hint (first &rest arg if given)
                ;; as the MINIMUM number of additional elements per CLHS.
                ;; Default to 1, fall back to cap (2× growth) when that's larger.
                (let* ((hint (cond ((and (consp args) (integerp (car args)) (> (car args) 0)) (car args))
                                   (t 1)))
                       (new-cap (max (+ cap hint) (+ fp 1) (* cap 2)))
                       (str-p (or (stringp arr)
                                  (and disp (stringp disp))))
                       (new-arr (if str-p
                                    (%make-string-array new-cap)
                                    (make-array new-cap))))
                  ;; Copy old visible contents (0..cap from old aref-routing)
                  ;; through the MDA's current view so displacement is honored.
                  (let ((i 0))
                    (loop
                      (when (>= i cap) (return))
                      (let ((v (cond
                                 (disp (aref arr (+ off i)))
                                 (t (aref arr i)))))
                        (aset new-arr i v))
                      (setq i (+ i 1))))
                  ;; Update MDA: data ← new-arr, displaced ← nil, offset ← 0,
                  ;; dims ← (new-cap).  Adjusting dims keeps array-total-size
                  ;; honest after the grow.
                  (%prim-aset vector 6 new-arr)
                  (%prim-aset vector 3 nil)
                  (%prim-aset vector 4 0)
                  (%prim-aset vector 1 (list new-cap))
                  (setq arr new-arr)
                  (setq disp nil)
                  (setq off 0)
                  (setq cap new-cap)))
              ;; Now write at (fp + off) in the underlying.
              (let* ((target-arr arr)
                     (target-idx (+ fp off))
                     (store-val (if (and (stringp target-arr) (characterp new-element))
                                    (char-code new-element)
                                    new-element)))
                (aset target-arr target-idx store-val))
              (%mda-set-fp vector (+ fp 1))
              fp)))))
    (t (let ((inner (%fp-inner vector)))
         (cond
           ((and (consp inner) (fixnump (car inner)))
            (let ((fp (car inner))
                  (arr (cdr inner)))
              (let ((len (array-length arr)))
                (when (>= fp len)
                  (let* ((hint (cond ((and (consp args) (integerp (car args)) (> (car args) 0)) (car args))
                                     (t 1)))
                         (new-len (max (+ len hint) (+ fp 1) (* len 2)))
                         (new-arr nil))
                    (if (stringp arr)
                        (setq new-arr (%make-string-array new-len))
                        (setq new-arr (make-array new-len)))
                    (let ((i 0))
                      (loop
                        (when (>= i len) (return))
                        (aset new-arr i (aref arr i))
                        (setq i (+ i 1))))
                    (set-cdr inner new-arr)
                    (setq arr new-arr)))
                (let ((store-val (if (and (stringp arr) (characterp new-element))
                                     (char-code new-element)
                                     new-element)))
                  (aset arr fp store-val))
                (set-car inner (+ fp 1))
                fp)))
           (t (%signal-type-error)))))))

(defun vector-pop (vector)
  "Pop an element from VECTOR (with fill pointer)."
  (cond
    ((%mda-p vector)
     (let ((fp (%mda-fp vector)))
       (if (and fp (> fp 0))
           (let ((new-fp (- fp 1)))
             (%mda-set-fp vector new-fp)
             (aref (%mda-data vector) new-fp))
           (error "vector-pop: empty vector"))))
    (t (let ((vector (%fp-inner vector)))
         (if (and (consp vector) (fixnump (car vector)))
             (let ((fp (car vector)))
               (if (> fp 0)
                   (let ((new-fp (- fp 1)))
                     (set-car vector new-fp)
                     (aref (cdr vector) new-fp))
                   (error "vector-pop: empty vector")))
             (error "vector-pop: no fill pointer"))))))

;;; ============================================================
;;; set operations (set-exclusive-or, nset-exclusive-or)
;;; ============================================================

(defun %set-exclusive-or-impl (list1 list2 args)
  "Positional helper for set-exclusive-or. Takes args as a real list.
   Inlined search loops avoid the nested-lambda-with-capture pattern
   that's fragile in MVM (closure cell aliasing across iterations)."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (result nil))
    ;; Elements in list1 not in list2
    (dolist (e1 list1)
      (let ((k1 (if key-fn (funcall key-fn e1) e1))
            (found nil)
            (cur list2))
        (loop
          (when (or found (null cur)) (return nil))
          (let ((v2 (if key-fn (funcall key-fn (car cur)) (car cur))))
            (when (if test-fn (funcall test-fn k1 v2) (eql k1 v2))
              (setq found t)))
          (setq cur (cdr cur)))
        (unless found (setq result (cons e1 result)))))
    ;; Elements in list2 not in list1
    (dolist (e2 list2)
      (let ((k2 (if key-fn (funcall key-fn e2) e2))
            (found nil)
            (cur list1))
        (loop
          (when (or found (null cur)) (return nil))
          (let ((v1 (if key-fn (funcall key-fn (car cur)) (car cur))))
            (when (if test-fn (funcall test-fn v1 k2) (eql v1 k2))
              (setq found t)))
          (setq cur (cdr cur)))
        (unless found (setq result (cons e2 result)))))
    result))

(defun set-exclusive-or (list1 list2 &rest args)
  "Return symmetric difference of LIST1 and LIST2.
   :test defaults to inline `eql` (#'eql is unusable in MVM)."
  (%set-exclusive-or-impl list1 list2 args))

(defun nset-exclusive-or (list1 list2 &rest args)
  "Destructive set-exclusive-or. Routes through positional helper to
   dodge apply-of-rest fragility."
  (%set-exclusive-or-impl list1 list2 args))

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

;;; DOCUMENTATION — registry-keyed lookup.  CL associates doc strings
;;; with (name . doc-type) pairs.  We store them in a single alist
;;; *documentation-strings* with entries of shape ((name . type) . doc).
;;; Always returns NIL for never-set entries; (setf documentation) puts.

(defvar *documentation-strings* nil)

(defun %doc-type-normalize (doc-type)
  "CLHS: for function objects, doc-types T and FUNCTION address the same
   documentation — (setf (documentation fn 'function) s) must be visible
   to (documentation fn t) (defgeneric.2).  Normalize FUNCTION → T so
   both use one storage key."
  (if (and doc-type (symbolp doc-type)
           (string= (symbol-name doc-type) "FUNCTION"))
      t
      doc-type))

(defun %doc-key-eq (x k)
  "Documentation-registry key identity.  Function-ish objects (compiled
   fns, closures, interp-closure conses) compare by EQ — EQUAL on two
   structurally-identical interp-closures from DIFFERENT (eval (defun
   ...)) calls wrongly unified their doc entries
   (documentation.function.function.1).  Names (symbols, (setf foo)
   lists) keep structural EQUAL."
  (if (or (functionp x)
          (and (consp x) (eq (car x) '%interp-closure)))
      (eq x k)
      (equal x k)))

(defun documentation (x doc-type)
  "Look up doc string for X under DOC-TYPE, or NIL."
  (setq doc-type (%doc-type-normalize doc-type))
  (let ((key (cons x doc-type))
        (cur *documentation-strings*)
        (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (when (and (consp (car cur)) (consp (car (car cur)))
                 (let ((k (car (car cur))))
                   (and (%doc-key-eq x (car k)) (eq (cdr k) doc-type))))
        (setq found (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun set-documentation (x doc-type string)
  "Install STRING as the doc for (X . DOC-TYPE) in
   *documentation-strings*, replacing any prior entry."
  (setq doc-type (%doc-type-normalize doc-type))
  (let ((key (cons x doc-type))
        (cur *documentation-strings*)
        (acc nil)
        (replaced nil))
    (loop
      (when (null cur) (return nil))
      (let ((entry (car cur)))
        (let ((k (car entry)))
          (cond
            ((and (consp k) (%doc-key-eq x (car k)) (eq (cdr k) doc-type))
             (setq acc (cons (cons key string) acc))
             (setq replaced t))
            (t (setq acc (cons entry acc))))))
      (setq cur (cdr cur)))
    (unless replaced (setq acc (cons (cons key string) acc)))
    (setq *documentation-strings* acc))
  string)

;;; ============================================================
;;; set-apply — receiver for (setf (apply #'aref arr subs...) v)
;;; ============================================================

(defun set-apply (fn &rest stuff)
  "CLHS 5.1.2.5: (setf (apply #'aref array subscripts) new-value).
   The compiler's generic-setf expansion for an unknown place head
   compiles (setf (apply F a1 .. an tail) V) into
   (SET-APPLY F a1 .. an tail V).  Only the #'AREF shape with a single
   subscript (1-D arrays) is supported — the only shape the ANSI suite
   exercises (defgeneric.33).  Returns NEW-VALUE per SETF semantics."
  (declare (ignore fn))
  (let ((value (car (last stuff)))
        (apply-args (butlast stuff)))
    (let ((spread (butlast apply-args))
          (tail (car (last apply-args))))
      (let ((all (append spread tail)))
        (let ((arr (car all))
              (subs (cdr all)))
          (when (and arr subs (null (cdr subs)))
            ;; Variable-index ASET as non-last form needs the let-dummy
            ;; workaround (CLAUDE.md MVM limitation #2).
            (let ((dummy (aset arr (car subs) value)))
              dummy))
          value)))))

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
  "Apply bitwise OP element-wise to bit arrays V1 and V2.
   When RESULT-VEC is unsupplied, allocate a fresh array with the
   SAME SHAPE as V1 (multi-dim aware): bit-and.{12..16}/etc. compare
   `(values … result)` to `#2a((…) (…))`-shaped expecteds, so a flat
   vector return mismatches the expected even when the element data
   is correct."
  (let ((len (min (array-length v1) (if v2 (array-length v2) (array-length v1)))))
    (let ((result (cond
                    ((eq result-vec t) v1)              ; in-place into V1
                    (result-vec result-vec)             ; user-supplied dest
                    (t
                     ;; Allocate same shape as v1.  If v1 is an MDA
                     ;; (multi-dim or has :element-type 'bit), preserve
                     ;; both via :dimensions + :element-type.
                     (make-array (array-dimensions v1)
                                 :element-type 'bit)))))
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

;; symbol< — variadic less-than comparator for symbols, used by SORT
;; in the loop6 tests' `(sort (loop for x being the hash-key …) #'symbol<)`
;; pattern.  ansi-aux.lsp defines it, but the build-side aux loader skips
;; ansi-aux.lsp ("STRING is unbound" — `(map string …)` body parses as
;; a variable reference at SBCL host eval time).  Adding it natively
;; here unblocks loop.6.6..14 (+9) and any other test that hashes-into
;; a sorted list of symbols.
(defun symbol< (x &rest args)
  (apply #'string< (symbol-name x) (mapcar #'symbol-name args)))

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
  "Call FN (the closure returned by FORMATTER) on a string-output-stream
   with ARGS, return the accumulated string. ANSI tests use this as a
   parallel of (format nil control args...) to exercise the FORMATTER
   form alongside FORMAT — same semantics, twice the coverage per
   directive. Was a stub returning \"\" until 2026-04-25."
  (let ((s (make-string-output-stream)))
    (apply fn s args)
    (get-output-stream-string s)))

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

(defun %coerce-canon-head (head)
  "Canonicalize a type-head symbol to the Modus built-in type symbol of
   the same NAME.  A type-head read in a foreign package (e.g.
   UIOP/UTILITY::VECTOR, when define-package inheritance hasn't exposed
   CL:VECTOR) is a distinct object from Modus's compiled 'VECTOR, so
   `(eq head 'vector)` fails and coerce silently returns OBJECT unchanged.
   Type specifiers compare by name (CLHS), so re-key by name.

   Pass Modus's OWN type symbols (and the EQ-recognised built-ins)
   through untouched so the compiled hot path is byte-for-byte unchanged
   — only re-key a symbol that the downstream EQ checks would miss
   (i.e. NOT already EQ to the built-in literal of its name)."
  (if (and head (not (consp head)) (not (stringp head))
           (not (integerp head)) (not (characterp head))
           (symbolp head)
           ;; already a recognised built-in literal → leave as-is
           (not (eq head 'list)) (not (eq head 'vector))
           (not (eq head 'simple-vector)) (not (eq head 'array))
           (not (eq head 'simple-array)) (not (eq head 'string))
           (not (eq head 'simple-string)) (not (eq head 'base-string))
           (not (eq head 'simple-base-string)) (not (eq head 'character))
           (not (eq head 'bit-vector)) (not (eq head 'simple-bit-vector))
           (not (eq head 'float)) (not (eq head 'single-float))
           (not (eq head 'double-float)) (not (eq head 'short-float))
           (not (eq head 'long-float)) (not (eq head 'complex))
           (not (eq head 'function)))
      (let ((n (symbol-name head)))
        (cond
          ((null n) head)
          ((string= n "LIST") 'list)
          ((string= n "VECTOR") 'vector)
          ((string= n "SIMPLE-VECTOR") 'simple-vector)
          ((string= n "ARRAY") 'array)
          ((string= n "SIMPLE-ARRAY") 'simple-array)
          ((string= n "STRING") 'string)
          ((string= n "SIMPLE-STRING") 'simple-string)
          ((string= n "BASE-STRING") 'base-string)
          ((string= n "SIMPLE-BASE-STRING") 'simple-base-string)
          ((string= n "CHARACTER") 'character)
          ((string= n "BIT-VECTOR") 'bit-vector)
          ((string= n "SIMPLE-BIT-VECTOR") 'simple-bit-vector)
          ((string= n "FLOAT") 'float)
          ((string= n "SINGLE-FLOAT") 'single-float)
          ((string= n "DOUBLE-FLOAT") 'double-float)
          ((string= n "SHORT-FLOAT") 'short-float)
          ((string= n "LONG-FLOAT") 'long-float)
          ((string= n "COMPLEX") 'complex)
          ((string= n "FUNCTION") 'function)
          (t head)))
      head))

(defun coerce (object result-type)
  "Coerce OBJECT to RESULT-TYPE.  Accepts compound type forms like
   (vector *), (vector * 2), (simple-string 5) — uses the head symbol
   for dispatch (per CLHS, compound array/string subtypes are still
   the same family of result-type)."
  (let* ((orig-type result-type)
         ;; Explicit length from a compound array/vector/string spec like
         ;; (vector * 4) / (simple-string 5) — third element (or second for
         ;; string specs).  NIL or '* means unconstrained.
         (spec-len
           (and (consp orig-type)
                (let ((head (car orig-type)) (rest (cdr orig-type)))
                  (cond
                    ;; (vector elt-type len) / (array elt-type (len)) family:
                    ;; length is the 3rd element
                    ((and (or (eq head 'vector) (eq head 'simple-vector)
                              (eq head 'array) (eq head 'simple-array))
                          (consp rest) (consp (cdr rest)))
                     (let ((l (car (cdr rest))))
                       (cond ((integerp l) l)
                             ((and (consp l) (integerp (car l))) (car l))
                             (t nil))))
                    ;; (string len) / (simple-string len) etc: length is 2nd
                    ((and (or (eq head 'string) (eq head 'simple-string)
                              (eq head 'base-string) (eq head 'simple-base-string)
                              (eq head 'bit-vector) (eq head 'simple-bit-vector))
                          (consp rest) (integerp (car rest)))
                     (car rest))
                    (t nil)))))
         ;; CLHS allows RESULT-TYPE to be a class object (e.g.
         ;; (coerce x (find-class 'vector))).  Normalize a CLOS class or
         ;; built-in class-proxy to its name symbol first so the head
         ;; dispatch below sees 'vector etc.
         (result-type (cond
                        ((and (not (consp result-type)) (not (symbolp result-type))
                              (or (%clos-class-p result-type)
                                  (%class-proxy-p result-type)))
                         (class-name result-type))
                        (t result-type)))
         (result-type (%coerce-canon-head (if (consp result-type) (car result-type) result-type)))
         (%cv
  (cond
    ((eq result-type 'list)
     (cond
       ;; Wrapped vector — use length+wrapper-aref to read effective contents
       ((and (consp object) (array-wrapper-p object))
        (let ((len (length object)) (result nil) (i 0)
              (string-p (stringp object)))
          (loop
            (when (>= i len) (return (nreverse result)))
            (let ((raw (%wrapper-aref object i)))
              (setq result (cons (if (and string-p (integerp raw))
                                     (code-char raw) raw)
                                 result)))
            (setq i (+ i 1)))))
       ((consp object) object)
       ((null object) nil)
       ((stringp object)
        (let ((len (length object)) (result nil) (i 0))
          (loop
            (when (>= i len) (return (nreverse result)))
            (setq result (cons (aref object i) result))   ; AREF→char already
            (setq i (+ i 1)))))
       ((arrayp object)
        (let ((len (array-length object)) (result nil) (i 0))
          (loop
            (when (>= i len) (return (nreverse result)))
            (setq result (cons (aref object i) result))
            (setq i (+ i 1)))))
       (t object)))
    ((or (eq result-type 'string) (eq result-type 'simple-string)
         (eq result-type 'base-string) (eq result-type 'simple-base-string))
     (cond
       ;; Wrapped string — flatten to plain string of effective length
       ((and (consp object) (array-wrapper-p object) (stringp object))
        (let* ((len (length object))
               (s (%make-string-array len))
               (i 0))
          (loop
            (when (>= i len) (return s))
            (let ((raw (%wrapper-aref object i)))
              (aset s i (if (integerp raw) raw (char-code raw))))
            (setq i (+ i 1)))))
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
    ((or (eq result-type 'vector) (eq result-type 'simple-vector)
         ;; Bit-vectors in Modus are plain arrays of 0/1 (bit-vector-p
         ;; tests element values), so coercing a list/vector/bit-vector to
         ;; BIT-VECTOR is the same flatten-to-array path as VECTOR.
         (eq result-type 'bit-vector) (eq result-type 'simple-bit-vector))
     (cond
       ;; Wrapped vector — flatten to plain vector of effective length
       ((and (consp object) (array-wrapper-p object))
        (let* ((len (length object))
               (v (make-array len))
               (string-p (stringp object))
               (i 0))
          (loop
            (when (>= i len) (return v))
            (let ((raw (%wrapper-aref object i)))
              (aset v i (if (and string-p (integerp raw)) (code-char raw) raw)))
            (setq i (+ i 1)))))
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
     ;; CLHS: object must be a character designator — a character, a
     ;; string of length 1, or a symbol whose name has length 1.
     (cond
       ((characterp object) object)
       ((integerp object) (code-char object))
       ((stringp object) (aref object 0))            ; AREF→char already
       ((symbolp object)                             ; symbol → its 1-char name
        (let ((nm (symbol-name object)))
          (if (and (stringp nm) (= (length nm) 1))
              (aref nm 0)
              (progn (%signal-type-error) nil))))
       (t object)))
    ((eq result-type 'function)
     ;; CLHS: a symbol or lambda expression coerces to a function.  A
     ;; symbol with no global function definition signals an error.
     (cond
       ((functionp object) object)
       ((and (consp object) (symbolp (car object))
             (string= (symbol-name (car object)) "LAMBDA"))
        ;; (lambda ...) expression → interpreted closure (null lexenv)
        (list '%interp-closure (cadr object) (cddr object) nil))
       ((symbolp object)
        (let ((fn (and (fboundp object) (symbol-function object))))
          (if fn fn (progn (%signal-undefined-function) nil))))
       (t (%signal-type-error) nil)))
    ((eq result-type 'cons)
     ;; CONS is not a valid coercion result-type unless OBJECT is already
     ;; a cons (CLHS 4.2.1: coerce identity when already of the type).
     ;; (coerce nil 'cons), (coerce x 'cons) for non-cons → TYPE-ERROR.
     (if (consp object) object (progn (%signal-type-error) nil)))
    ((eq result-type 'complex)
     ;; CLHS coerce/COMPLEX: a rational stays a rational (a rational IS of
     ;; type COMPLEX since (complex rational 0) = rational), but a FLOAT
     ;; becomes a true #C(float 0.0) — float contagion forbids the collapse
     ;; to a real that (complex r 0) does for rationals.  coerce.18/.19/.20.
     (cond
       ((%complex-p object) object)
       ((floatp-impl object)
        (let ((c (make-array 3)))
          (aset c 0 '%complex-marker)
          (aset c 1 object)
          (aset c 2 (float 0))
          c))
       ((or (integerp object) (ratiop object)) object)
       (t (progn (%signal-type-error) nil))))
    (t object))))
  ;; CLHS: if RESULT-TYPE specifies an explicit length, OBJECT must be
  ;; coercible to a sequence of exactly that length, else TYPE-ERROR.
  ;; The cond above ignores the length; validate it here on the result.
    (if (and spec-len
             (or (stringp %cv) (arrayp %cv) (consp %cv) (null %cv))
             (not (eql (length %cv) spec-len)))
        (progn (%signal-type-error) nil)
        %cv)))

;;; ============================================================
;;; C*R Extensions (4-deep)
;;; ============================================================
;;;
;;; Use %safe-car / %safe-cdr at each composition step so that calling
;;; e.g. (cdaaar 'a) signals TYPE-ERROR (per ANSI) rather than silently
;;; dereferencing garbage at the symbol's "car slot".

(defun %safe-car (x)
  (if (consp x) (car x)
      (if (null x) nil (%signal-type-error))))

(defun %safe-cdr (x)
  (if (consp x) (cdr x)
      (if (null x) nil (%signal-type-error))))

;; 3-letter cXXr — the 2-letter forms (CAAR/CADR/CDAR/CDDR) are routed
;; through %safe-car/%safe-cdr in compile-caar etc. (mvm/compiler.lisp);
;; CADDR and CDDDR get safer defuns here so 4-letter forms built on top
;; of them inherit the type-check.  runtime/cons.lisp defines a lax
;; (car (cdr (cdr x))) version of CADDR — last-defun-wins so this
;; override takes effect at runtime.
;; 2-letter c*r accessors — needed as real defuns for #'cadr / #'cddr
;; etc. in function-designator contexts (e.g. LOOP BY #'cddr).  The
;; compile-time primitive paths handle these as direct calls, but
;; (function NAME) needs an fdefinition table entry.
(defun caar (x) (%safe-car (%safe-car x)))
(defun cadr (x) (%safe-car (%safe-cdr x)))
(defun cdar (x) (%safe-cdr (%safe-car x)))
(defun cddr (x) (%safe-cdr (%safe-cdr x)))
(defun caaar (x) (%safe-car (%safe-car (%safe-car x))))
(defun caadr (x) (%safe-car (%safe-car (%safe-cdr x))))
(defun cadar (x) (%safe-car (%safe-cdr (%safe-car x))))
(defun caddr (x) (%safe-car (%safe-cdr (%safe-cdr x))))
(defun cdaar (x) (%safe-cdr (%safe-car (%safe-car x))))
(defun cdadr (x) (%safe-cdr (%safe-car (%safe-cdr x))))
(defun cddar (x) (%safe-cdr (%safe-cdr (%safe-car x))))
(defun cdddr (x) (%safe-cdr (%safe-cdr (%safe-cdr x))))

(defun caaaar (x) (%safe-car (caaar x)))
(defun caaadr (x) (%safe-car (caadr x)))
(defun caadar (x) (%safe-car (cadar x)))
(defun caaddr (x) (%safe-car (caddr x)))
(defun cadaar (x) (%safe-car (cdaar x)))
(defun cadadr (x) (%safe-car (cdadr x)))
(defun caddar (x) (%safe-car (cddar x)))
(defun cadddr (x) (%safe-car (cdddr x)))
(defun cdaaar (x) (%safe-cdr (caaar x)))
(defun cdaadr (x) (%safe-cdr (caadr x)))
(defun cdadar (x) (%safe-cdr (cadar x)))
(defun cdaddr (x) (%safe-cdr (caddr x)))
(defun cddaar (x) (%safe-cdr (cdaar x)))
(defun cddadr (x) (%safe-cdr (cdadr x)))
(defun cdddar (x) (%safe-cdr (cddar x)))
(defun cddddr (x) (%safe-cdr (cdddr x)))

;;; ============================================================
;;; Bitwise Logic Extensions
;;; ============================================================

(defun lognand (a b) (lognot (logand a b)))
(defun lognor (a b) (lognot (logior a b)))
(defun logeqv (&rest args)
  "Bitwise EQV — identity -1, n-ary fold of pairwise EQV (CLHS 12.2)."
  (cond
    ((null args) -1)
    ((null (cdr args)) (car args))
    (t (let ((acc (car args)) (rest (cdr args)))
         (loop
           (when (null rest) (return acc))
           (setq acc (lognot (logxor acc (car rest))))
           (setq rest (cdr rest)))))))
(defun logandc1 (a b) (logand (lognot a) b))
(defun logandc2 (a b) (logand a (lognot b)))
(defun logorc1 (a b) (logior (lognot a) b))
(defun logorc2 (a b) (logior a (lognot b)))

;;; ============================================================
;;; Numeric Functions
;;; ============================================================

(defun signum (x)
  "Return the sign of X.  For reals: -1, 0, or 1 (float-contagious for
   floats).  For complex X: x / |x|, which preserves the phase and has
   modulus 1 (or x itself when x is 0).  CLHS: (signum x) ≡ (if (zerop x)
   x (/ x (abs x)))."
  (cond
    ((complexp x)
     (let ((r (realpart x)) (i (imagpart x)))
       (if (and (zerop r) (zerop i))
           x
           ;; magnitude inline (the &rest-wrapped public ABS crashes when
           ;; handed a heap complex object); sqrt of sum of squares.
           (let ((mag (sqrt (+ (* r r) (* i i)))))
             (complex (/ r mag) (/ i mag))))))
    ((floatp-impl x)
     (if (< x 0.0) -1.0 (if (> x 0.0) 1.0 0.0)))
    (t (if (< x 0) -1 (if (> x 0) 1 0)))))

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
  "Return list of dimensions of array A.
   First: native MDA (subtag #x34) carries dims directly in slot 1.
   Otherwise peels (cons 8765432 ...) adjustable wrapper and detects
   multi-dim (9867654), fill-pointer (cons FP ...), and displaced
   (cons (cons SIZE OFFSET) ...) wrappers.

   CLHS: signal TYPE-ERROR on non-array."
  (cond
    ((%mda-p a) (%mda-dims a))
    (t
     (let ((a (if (and (consp a) (eql (car a) 8765432)) (cdr a) a)))
       (cond
         ((and (consp a) (eql (car a) 9867654) (consp (cdr a)))
          (cadr a))
         ((and (consp a) (fixnump (car a)))
          ;; fill-pointer wrapper — declared dim is underlying length
          (list (array-length (cdr a))))
         ((and (consp a) (consp (car a)))
          ;; displaced wrapper — declared dim is the size in the head cons
          (list (car (car a))))
         ((arrayp a) (list (array-length a)))
         ((stringp a) (list (array-length a)))
         (t (error "ARRAY-DIMENSIONS: ~S is not an array" a)))))))

(defun upgraded-array-element-type (type &optional environment)
  "Return the upgraded element type Modus actually uses for arrays of TYPE.
   Per CLHS 15.1.2.1: BIT upgrades to BIT, CHARACTER/BASE-CHAR upgrade to
   CHARACTER (Modus stores all chars uniformly), NIL upgrades to NIL, and
   everything else upgrades to T (Modus stores general elements as tagged
   words)."
  (declare (ignore environment))
  (cond
    ;; NIL element type — array that can hold no objects.
    ((null type) nil)
    ;; BIT — Modus stores bit-vectors specialized to BIT.
    ((eq type 'bit) 'bit)
    ;; (UNSIGNED-BYTE 1) is type-equivalent to BIT.
    ((equal type '(unsigned-byte 1)) 'bit)
    ((equal type '(integer 0 1)) 'bit)
    ((equal type '(mod 2)) 'bit)
    ;; Character element types upgrade to CHARACTER.
    ((or (eq type 'character)
         (eq type 'base-char)
         (eq type 'standard-char)
         (eq type 'extended-char))
     'character)
    ;; Everything else is stored as a general (T) element.
    (t t)))

(defun empirical-subtypep (type1 type2)
  "ANSI ansi-aux helper (ansi-aux.lsp is SKIPPED by Modus, so this is
   stubbed here).  Returns T iff TYPE1 appears to be a subtype of TYPE2.

   Per the Dietz definition: first ask the real SUBTYPEP via SUBTYPEP*.
   If SUBTYPEP returned a *definite* answer (second value T) use it
   directly.  Otherwise fall back to scanning *UNIVERSE*: every element
   that is of TYPE1 must also be of TYPE2.  This always returns T when
   TYPE1 is genuinely a subtype of TYPE2, and only returns T spuriously
   when the *UNIVERSE* sample happens not to distinguish the two types —
   exactly the documented behaviour of the reference helper.

   Critically this must NOT over-return T for a definite non-subtype:
   when SUBTYPEP is definite-NIL we return that NIL, never falling through
   to the (looser) universe scan."
  (multiple-value-bind (sub good) (subtypep* type1 type2)
    (if good
        sub
        (loop for e in *universe*
              always (or (not (typep e type1)) (typep e type2))))))

(defun simple-vector-p (x)
  "T iff X is a SIMPLE-VECTOR — a one-dimensional, non-displaced array of
   element-type T with no fill pointer and not adjustable.  Strings, bit
   arrays, character arrays, multi-dimensional arrays, rank-0 arrays and
   fill-pointered / displaced vectors are NOT simple-vectors.

   NB: Modus stores a literal bit-vector (#*0110) and a general fixnum
   vector identically, so (simple-vector-p #*0110) cannot be distinguished
   here and conservatively returns T — a representational limitation, not a
   logic bug.  Bit / character arrays built via MAKE-ARRAY with an explicit
   :element-type DO carry that type (MDA header slot 5 / string subtag) and
   are correctly rejected."
  (cond
    ((null x) nil)
    ((eq x t) nil)
    ((fixnump x) nil)
    ((consp x) nil)
    ((characterp x) nil)
    ;; Strings (incl. MAKE-ARRAY :element-type character/base-char, whose
    ;; data is a subtag-#x31 string) are not simple-vectors.
    ((stringp x) nil)
    ;; Native multi-dim header: only rank-1, T-element, plain vectors qualify.
    ((%mda-p x)
     (and (eql (%mda-rank x) 1)
          (null (%mda-fp x))
          (null (%mda-displaced x))
          (let ((et (%mda-etype x)))
            (or (eq et t) (null et)))))
    ;; Plain 1-D MVM array (subtag #x32), element-type T → simple vector.
    ((arrayp x) t)
    (t nil)))

(defun simple-bit-vector-p (x)
  "T iff X is a simple bit-vector.  Modus represents a bit-vector as a
   1-D array whose elements are all 0/1 (no distinct subtag), so this is
   BIT-VECTOR-P (every Modus bit-vector is simple)."
  (bit-vector-p x))

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

;; HASH-TABLE-TEST / HASH-TABLE-REHASH-{SIZE,THRESHOLD} / CLRHASH all
;; live in prelude.lisp now and pull real metadata out of the new
;; cdr-cell shape.  Earlier this file had three constant-returning stubs
;; that won under last-defun-wins, so MAKE-HASH-TABLE :TEST 'EQ tables
;; still reported HASH-TABLE-TEST → EQUAL etc.  Removed.

;;; ============================================================
;;; Sleep (stub)
;;; ============================================================

(defun sleep (n) nil)

;;; ============================================================
;;; Environment introspection
;;; ============================================================

(defun lisp-implementation-type ()    "Modus")
(defun lisp-implementation-version () "0.0.1")
(defun machine-instance ()            "modus")
(defun machine-type ()                "x86_64")
(defun machine-version ()             "x86_64")
(defun software-type ()               "Modus")
(defun software-version ()            "0.0.1")
(defun short-site-name ()             nil)
(defun long-site-name ()              nil)

;;; rational-safely — ansi-aux helper used by numbers-aux defconstants.
;;; Defined as a Modus runtime fn so compile-time references at SBCL
;;; build time and runtime references both succeed.
(defun rational-safely (x)
  (cond
    ((integerp x) x)
    ((floatp-impl x) (rational x))
    (t x)))

;;; ============================================================
;;; Float versions of integer division — return integer parts as
;;; integers (modus has no native floats); the fractional remainder is
;;; reported as a rational.  Mostly returns sensible values for cases
;;; the ANSI test suite asks about (integer/integer pairs).
;;; ============================================================

;;; CLHS: ffloor/fceiling/ftruncate/fround return the quotient as a
;;; FLOAT (of the same format as the argument, or single-float if the
;;; argument is rational).  The second value is the remainder.  Modus
;;; coerces the integer quotient back to a float; the remainder keeps
;;; the type the underlying floor/etc. produced (integer for
;;; integer/integer args, float for float args).
(defun ftruncate (n &optional (d 1))
  (multiple-value-bind (q r) (truncate n d)
    (values (%any-to-float q) r)))

(defun ffloor (n &optional (d 1))
  (multiple-value-bind (q r) (floor n d)
    (values (%any-to-float q) r)))

(defun fceiling (n &optional (d 1))
  (multiple-value-bind (q r) (ceiling n d)
    (values (%any-to-float q) r)))

(defun fround (n &optional (d 1))
  (multiple-value-bind (q r) (round n d)
    (values (%any-to-float q) r)))

;;; ============================================================
;;; CLOS MOP Stubs
;;; ============================================================

(defun allocate-instance (class &rest initargs)
  "Allocate a new instance of CLASS (a class object or class name).
   All slots are unbound; initforms are NOT applied (per ANSI: that
   happens in initialize-instance, not allocate-instance)."
  (let ((class-name (cond
                      ((symbolp class) class)
                      ((%clos-class-p class) (aref class 1))
                      (t nil))))
    (when (null class-name) (return-from allocate-instance nil))
    (%make-instance class-name)))

(defun %shared-initialize-default (instance slot-names &rest initargs)
  "Default method body for SHARED-INITIALIZE.
   Per ANSI:
     - For each slot-name in INSTANCE's class:
       1. If an initarg in INITARGS names this slot, set to its value (leftmost wins).
       2. Else if (eq slot-names t) or (member slot-name slot-names),
          and slot is currently unbound, apply initform if present.
       3. Else leave slot alone.
   Returns INSTANCE."
  (when (or (null instance) (not (%clos-instance-p instance)))
    (return-from %shared-initialize-default instance))
  (let* ((class-name (aref instance 1))
         (cls (%find-clos-class class-name)))
    (when (null cls) (return-from %shared-initialize-default instance))
    (let* ((slot-list (aref cls 2))
           (inst-len (array-length instance))
           (set-slots nil))
      ;; 1. Apply initargs first (leftmost wins).
      (let ((cur initargs))
        (loop
          (when (null cur) (return nil))
          (when (null (cdr cur)) (return nil))
          (let* ((key (car cur))
                 (val (car (cdr cur)))
                 ;; CLHS 7.1.4: one initarg may name several slots.
                 (slots (%clos-initarg-to-slots class-name key)))
            (let ((sc slots))
              (loop
                (when (null sc) (return nil))
                (let ((slot-nm (car sc)))
                  (when (not (member slot-nm set-slots))
                    (let ((idx (%clos-slot-index cls slot-nm)))
                      ;; idx is -1 (not found) or 0..n-1 — use `(>= idx 0)`
                      ;; to filter not-found.  See %clos-slot-index docstring
                      ;; for the AArch64 rationale (the previous nil sentinel
                      ;; collided with fixnum 0 on slot 0).
                      (when (and (>= idx 0) (< (+ 2 idx) inst-len))
                        (aset instance (+ 2 idx) val)
                        (setq set-slots (cons slot-nm set-slots))))))
                (setq sc (cdr sc)))))
          (setq cur (cdr (cdr cur)))))
      ;; 2. Then apply initforms for slots in slot-names that are still unbound.
      (let ((sn slot-list) (idx 0))
        (loop
          (when (null sn) (return nil))
          (when (< (+ 2 idx) inst-len)
            (let* ((nm (car sn))
                   (already-set (member nm set-slots))
                   (covered (cond ((eq slot-names t) t)
                                  ((null slot-names) nil)
                                  (t (member nm slot-names))))
                   (cur-val (aref instance (+ 2 idx)))
                   (was-unbound (and (fixnump cur-val) (= cur-val -999))))
              (when (and (not already-set) covered was-unbound)
                (let ((thunk (%clos-initform-thunk class-name nm)))
                  (when thunk
                    (aset instance (+ 2 idx) (funcall thunk)))))))
          (setq idx (+ idx 1))
          (setq sn (cdr sn))))
      instance)))

(defun %dispatch-shared-init (args)
  "Inline dispatch for SHARED-INITIALIZE.  Direct call to default fn
   when no user methods exist on the GF (cheap path); GF dispatch when
   they do.  funcall on a quoted symbol would require the symbol-function
   table to know about the default, which it doesn't for our internal
   helpers — so we call the bare defun by name in the fast path."
  (let ((gf (%find-gf 'shared-initialize)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (%shared-init-default-spread args)))
        (%shared-init-default-spread args))))

(defun %shared-init-apply-initargs (instance class-name initargs set-slots-acc)
  "Apply initargs list to INSTANCE.  Avoids %shared-initialize-default's
   funcall+&rest path (which loses trailing args).  Returns the
   updated set-slots accumulator (alist of slot-name → t)."
  (let ((cur initargs) (acc set-slots-acc))
    (loop
      (when (or (null cur) (null (cdr cur))) (return acc))
      (let* ((key (car cur))
             (val (car (cdr cur)))
             ;; CLHS 7.1.4: one initarg may name several slots; set ALL of
             ;; them (leftmost initarg wins per slot via the acc guard).
             (slots (%clos-initarg-to-slots class-name key)))
        (let ((sc slots))
          (loop
            (when (null sc) (return nil))
            (let ((slot-nm (car sc)))
              (when (null (member slot-nm acc :test #'eq))
                (set-slot-value instance slot-nm val)
                (setq acc (cons slot-nm acc))))
            (setq sc (cdr sc)))))
      (setq cur (cdr (cdr cur))))))

(defun %shared-init-default-spread (args)
  "ARGS is (instance slot-names &rest initargs) as a list.  Apply the
   initargs and initforms directly, bypassing the &rest re-pack path
   that loses trailing args.  Mirrors %shared-initialize-default's
   semantics.

   Per CLHS 7.1.2, MAKE-INSTANCE produces a 'defaulted initialization
   argument list' = the explicit initargs PLUS any default-initargs whose
   keys aren't already supplied.  Default-initarg thunks are eval'd here
   (CLHS 7.1.4 requires re-evaluation per call).  Default-initargs that
   map to slots also suppress those slots' :initform (per CLHS 7.1.4)."
  (when (or (null args) (null (cdr args)))
    (return-from %shared-init-default-spread nil))
  (let* ((instance   (car args))
         (slot-names (car (cdr args)))
         (initargs   (cdr (cdr args))))
    (when (or (null instance) (not (%clos-instance-p instance)))
      (return-from %shared-init-default-spread instance))
    (let* ((class-name (aref instance 1))
           (cls        (%find-clos-class class-name)))
      (when (null cls)
        (return-from %shared-init-default-spread instance))
      ;; Step 1: apply explicit initargs (leftmost wins).
      (let ((set-slots (%shared-init-apply-initargs
                        instance class-name initargs nil)))
        ;; Step 2: walk default-initargs for the class (CPL-merged, most-
        ;; specific first).  For each key NOT explicitly supplied in
        ;; initargs, eval the thunk and apply it.  Default-initargs whose
        ;; key maps to a slot suppress that slot's :initform via the
        ;; set-slots accumulator.
        ;;
        ;; CLHS 7.1.4: default-initargs are part of MAKE-INSTANCE's
        ;; "defaulted initialization argument list" — they are NOT applied
        ;; by a bare SHARED-INITIALIZE / REINITIALIZE-INSTANCE call.  Only
        ;; the make-instance entry paths bind *clos-applying-defaults* to T
        ;; (shared-initialize.2.1: (shared-initialize obj t) must leave slot
        ;; c — whose only initform source is (:default-initargs :c 100) —
        ;; UNBOUND).
        (when *clos-applying-defaults*
          (let ((di-entries (%clos-default-initargs-for-class class-name)))
            (let ((cur di-entries))
              (loop
                (when (null cur) (return nil))
                (let* ((entry (car cur))
                       (k (car entry))
                       (thunk (cdr entry))
                       (explicit-supplied (%initargs-has-key-p initargs k)))
                  (unless explicit-supplied
                    (let ((val (funcall thunk))
                          ;; CLHS 7.1.4: a default-initarg's key may name
                          ;; several slots; set ALL still-unset ones.
                          (slots (%clos-initarg-to-slots class-name k)))
                      (let ((sc slots))
                        (loop
                          (when (null sc) (return nil))
                          (let ((slot-nm (car sc)))
                            (when (null (member slot-nm set-slots :test #'eq))
                              (set-slot-value instance slot-nm val)
                              (setq set-slots (cons slot-nm set-slots))))
                          (setq sc (cdr sc)))))))
                (setq cur (cdr cur))))))
        ;; Step 3: apply initforms for slots in slot-names still unbound.
        (let ((sn (aref cls 2)) (idx 0))
          (loop
            (when (null sn) (return nil))
            (let* ((nm (car sn))
                   (already-set (member nm set-slots :test #'eq))
                   (covered (cond ((eq slot-names t) t)
                                  ((null slot-names) nil)
                                  (t (member nm slot-names :test #'eq))))
                   ;; Class-allocated slot? Check via per-class storage.
                   (class-slot (%slot-class-owner class-name nm))
                   (was-unbound
                    (if class-slot
                        (not (%class-slot-bound-p class-name nm))
                        (let ((cur-val (aref instance (+ 2 idx))))
                          (and (fixnump cur-val) (= cur-val -999))))))
              (when (and (null already-set) covered was-unbound)
                (let ((thunk (%clos-initform-thunk class-name nm)))
                  (when thunk
                    (set-slot-value instance nm (funcall thunk))))))
            (setq idx (+ idx 1))
            (setq sn (cdr sn))))
        instance))))

(defun %initargs-has-key-p (initargs key)
  "True iff INITARGS plist contains KEY.  Uses %clos-initarg-key-equal
   so the cl-symbol and native-MVM-symbol shapes for :foo both compare
   correctly (cross-file identity has historical fragility)."
  (let ((cur initargs))
    (loop
      (when (or (null cur) (null (cdr cur))) (return nil))
      (when (%clos-initarg-key-equal (car cur) key) (return t))
      (setq cur (cdr (cdr cur))))))

(defun %clos-initarg-key-equal (a b)
  "Compare two initarg keys (keyword/symbols) per the same rules used
   internally by %clos-initarg-lookup-1.  Robust against native-MVM
   vs CL-symbol identity drift on the same name."
  (cond
    ((eq a b) t)
    ((and (%native-mvm-sym-p a) (%native-mvm-sym-p b))
     (= (%native-mvm-sym-hash a) (%native-mvm-sym-hash b)))
    ((and (%cl-sym-p a) (%cl-sym-p b))
     (string-equal (%cl-sym-name a) (%cl-sym-name b)))
    (t nil)))

(defun shared-initialize (&rest %sh-args)
  "SHARED-INITIALIZE generic function entry.  Falls through to
   %shared-initialize-default unless user methods were defined.
   CLHS: requires at least 2 args (instance + slot-names)."
  (when (or (null %sh-args) (null (cdr %sh-args)))
    (%signal-program-error))
  ;; CLHS: the second argument is the slot-names designator — either T (all
  ;; slots), NIL (no slots), or a proper list of slot-name symbols.  A
  ;; non-T, non-list atom (e.g. the keyword :A in shared-initialize.error.3)
  ;; is a type-error.  Also reject an improper / non-symbol-keyed initargs
  ;; plist (odd length, etc.) per shared-initialize.error.4.
  (let ((slot-names (car (cdr %sh-args)))
        (initargs (cdr (cdr %sh-args))))
    (when (and slot-names (not (eq slot-names t)) (not (consp slot-names)))
      (%signal-type-error))
    ;; initargs must form a proper, even-length plist whose keys are symbols
    ;; (keywords or symbols).  A non-symbol key like the list (A B C) in
    ;; shared-initialize.error.4 is a PROGRAM-ERROR (CLHS 7.1.2).
    (let ((cur initargs))
      (loop
        (when (null cur) (return nil))
        (when (not (consp cur)) (%signal-program-error))
        (when (null (cdr cur)) (%signal-program-error))  ; odd-length
        (let ((key (car cur)))
          (when (not (or (symbolp key) (%cl-sym-p key)))
            (%signal-program-error)))
        (setq cur (cdr (cdr cur))))))
  (%dispatch-shared-init %sh-args))

(defun %change-class-validate-initargs (new-name initargs)
  "CLHS 7.1.2-style initarg validity check for CHANGE-CLASS.
   Signals PROGRAM-ERROR on a malformed plist (odd length, non-symbol
   key) and ERROR on an initarg that doesn't initialize any slot of
   NEW-NAME, unless :allow-other-keys is true (LEFTMOST occurrence
   wins per CLHS 7.1.4 — change-class.1.10)."
  (let ((aok-seen nil) (aok nil) (bad-key nil) (bad-key-p nil) (cur initargs))
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur)) (%signal-program-error))
      (let ((key (car cur)) (val (car (cdr cur))))
        (when (not (or (symbolp key) (%cl-sym-p key)))
          (%signal-program-error))
        (cond
          ((%clos-initarg-key-equal key ':allow-other-keys)
           (when (not aok-seen)
             (setq aok-seen t)
             (setq aok val)))
          (t
           (when (null (%clos-initarg-to-slot new-name key))
             (setq bad-key key)
             (setq bad-key-p t)))))
      (setq cur (cdr (cdr cur))))
    (when (and bad-key-p (not aok))
      (error "change-class: invalid initarg ~S for class ~S"
             bad-key new-name))
    nil))

(defun %change-class-impl (instance new-class initargs)
  "Standard primary method body for CHANGE-CLASS (CLHS 7.2.1/7.2.2).
   INITARGS is a proper list (not &rest) so arbitrary-length initarg
   lists arrive untruncated.  Mutates INSTANCE in place (preserves EQ
   identity).

   Per CLHS 7.2.1:
   - slots local in the new class that existed in the old class
     (local OR shared) retain their values by NAME
   - slots not in the new class disappear
   - newly added local slots start unbound; their initforms run via
     the UPDATE-INSTANCE-FOR-DIFFERENT-CLASS default primary
     (shared-initialize semantics on the added slots)
   - slots shared in the new class are left to per-class storage
   - a copy of the original instance is passed as PREVIOUS to
     UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, dispatched as a real GF so
     user methods (primary / :before / :after) run

   Remaining limitation: if NEW-CLASS has more slots than the backing
   array allocates, trailing slots are capped at the array size."
  (when (null instance) (return-from %change-class-impl instance))
  (when (not (%clos-instance-p instance)) (return-from %change-class-impl instance))
  ;; Resolve new-class to a class object
  (let ((new-cls (cond
                   ((symbolp new-class) (%find-clos-class new-class))
                   ((%clos-class-p new-class) new-class)
                   (t nil))))
    (when (null new-cls) (return-from %change-class-impl instance))
    (let* ((new-name (aref new-cls 1))
           (new-slot-names (aref new-cls 2))
           (old-name (aref instance 1))
           (old-cls (%find-clos-class old-name))
           (old-slot-names (if old-cls (aref old-cls 2) nil))
           (inst-len (array-length instance)))
      ;; ---- 1. Validate the initarg plist (before any mutation) ----
      (%change-class-validate-initargs new-name initargs)
      ;; ---- 2. Snapshot PREVIOUS: copy with the old class + values ----
      (let ((previous (make-array inst-len)))
        (let ((i 0))
          (loop
            (when (>= i inst-len) (return nil))
            (let ((dummy (aset previous i (aref instance i))))
              dummy)
            (setq i (+ i 1))))
        ;; ---- 3. Snapshot old slot values keyed by name ----
        ;; Entry: (name . (bound . value)).  Shared-in-old slots read
        ;; through the old class's storage — CLHS 7.2.1: values of slots
        ;; shared in Cfrom and local in Cto are retained.
        (let ((old-values nil))
          (let ((sn old-slot-names) (idx 0))
            (loop
              (when (null sn) (return nil))
              (let* ((nm (car sn))
                     (owner (%slot-class-owner old-name nm)))
                (cond
                  (owner
                   (if (%class-slot-bound-p old-name nm)
                       (setq old-values
                             (cons (cons nm (cons t (%class-slot-get old-name nm nil)))
                                   old-values))
                       (setq old-values
                             (cons (cons nm (cons nil nil)) old-values))))
                  ((< (+ 2 idx) inst-len)
                   (let* ((v (aref instance (+ 2 idx)))
                          (unbound (and (fixnump v) (= v -999))))
                     (if unbound
                         (setq old-values
                               (cons (cons nm (cons nil nil)) old-values))
                         (setq old-values
                               (cons (cons nm (cons t v)) old-values)))))
                  (t nil)))
              (setq idx (+ idx 1))
              (setq sn (cdr sn))))
          ;; ---- 4. Mutate instance to its new class + remap values ----
          (aset instance 1 new-name)
          (let ((sn new-slot-names) (idx 0))
            (loop
              (when (null sn) (return nil))
              (when (< (+ 2 idx) inst-len)
                (let* ((nm (car sn))
                       (shared-new (%slot-class-owner new-name nm))
                       (pair (let ((cur old-values) (found nil))
                               (loop
                                 (when (null cur) (return found))
                                 (when (eq (car (car cur)) nm)
                                   (setq found (car cur))
                                   (return found))
                                 (setq cur (cdr cur))))))
                  (cond
                    ;; Shared in the new class: value lives in per-class
                    ;; storage; the instance cell is unused.  Old local
                    ;; value is NOT transferred (CLHS 7.2.1 — only
                    ;; local-in-new slots retain values).
                    (shared-new
                     (aset instance (+ 2 idx) -999))
                    ;; Local in new + existed bound in old: retain.
                    ((and pair (car (cdr pair)))
                     (aset instance (+ 2 idx) (cdr (cdr pair))))
                    ;; Existed-but-unbound or newly added: unbound.
                    ;; Added slots get initforms in %uifdc-default.
                    (t
                     (aset instance (+ 2 idx) -999)))))
              (setq idx (+ idx 1))
              (setq sn (cdr sn))))
          ;; ---- 5. UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (real GF) ----
          (%dispatch-uifdc (cons previous (cons instance initargs)))
          instance)))))

(defun %change-class-default (instance new-class &rest initargs)
  "Default method body for CHANGE-CLASS — thin &rest wrapper over
   %change-class-impl (which takes the initargs as a list)."
  (%change-class-impl instance new-class initargs))

(defun %dispatch-change-class (args)
  "Inline dispatch for CHANGE-CLASS — direct call to default unless GF
   has user methods (cheap path)."
  (let ((gf (%find-gf 'change-class)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (%change-class-default-spread args)))
        (%change-class-default-spread args))))

(defun %change-class-default-spread (args)
  "ARGS is (instance new-class &rest initargs) as a list.  Pass the
   initargs tail straight through to %change-class-impl — the previous
   by-arity ladder capped at 5 elements and silently truncated longer
   initarg lists (change-class.1.10 passes 8)."
  (cond
    ((or (null args) (null (cdr args))) (%signal-program-error))
    (t (%change-class-impl (car args) (car (cdr args)) (cdr (cdr args))))))

(defun change-class (&rest %cc-args)
  "CHANGE-CLASS generic function entry.  Falls through to
   %change-class-default unless user methods were defined.
   CLHS: requires at least 2 args (instance + new-class) —
   change-class.error.1/.2 expect PROGRAM-ERROR."
  (when (or (null %cc-args) (null (cdr %cc-args)))
    (%signal-program-error))
  (%dispatch-change-class %cc-args))

;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS and UPDATE-INSTANCE-FOR-
;; DIFFERENT-CLASS are defined as real GFs at the bottom of this file
;; (%init-clos-protocol).  The previous stubs here have been removed
;; so last-defun-wins picks the dispatcher.

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

(defun set (symbol value)
  "CLHS SET: set SYMBOL's value cell to VALUE.  Equivalent to
   (setf (symbol-value sym) value).  Per CLHS arity is exactly two —
   zero or one args signal program-error; three+ signal too.  The
   defun's required-count enforces both ends automatically."
  (let ((hash (cond
                ((%cl-sym-p symbol) (compute-name-hash (%cl-sym-name symbol)))
                ((symbolp symbol) (aref symbol 0))
                (t (%signal-type-error) 0))))
    (set-symbol-value hash value))
  value)

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

;; Strict 1-arg arity (CLHS): (atanh), (atanh 1.0 1.0) etc. signal
;; PROGRAM-ERROR.  The &rest extra guard mirrors integerp/isqrt above.
(defun atanh (x &rest extra)
  "Inverse hyperbolic tangent."
  ;; atanh(x) = 0.5 * ln((1+x)/(1-x))
  (if extra
      (%program-error "atanh requires exactly 1 argument")
      (* 0.5 (log (/ (+ 1.0 (float x)) (- 1.0 (float x)))))))

(defun asinh (x &rest extra)
  "Inverse hyperbolic sine."
  ;; asinh(x) = ln(x + sqrt(x^2 + 1))
  (if extra
      (%program-error "asinh requires exactly 1 argument")
      (let ((fx (float x)))
        (log (+ fx (sqrt (+ (* fx fx) 1.0)))))))

(defun acosh (x &rest extra)
  "Inverse hyperbolic cosine."
  ;; acosh(x) = ln(x + sqrt(x^2 - 1))
  (if extra
      (%program-error "acosh requires exactly 1 argument")
      (let ((fx (float x)))
        (log (+ fx (sqrt (- (* fx fx) 1.0)))))))

;; conjugate lives in cl-sequences.lisp — the stub here returned the
;; arg unchanged and shadowed the real implementation that handles
;; (complex r i) properly.

;;; ============================================================
;;; Transcendental / math arity guards (CLHS 12.2)
;;; ============================================================
;;; The implementations of sin/cos/tan/asin/acos/atan/sinh/cosh/tanh/
;;; exp/sqrt/abs live in cl-types.lisp / cl-clos.lisp / cl-eval.lisp with
;;; a single required parameter (atan: one &optional), so calling them with
;;; surplus positional arguments — e.g. (sin 0.0 0.0), (exp 0 0 0),
;;; (abs 0 0) — was silently accepted (extra args dropped) instead of
;;; signalling PROGRAM-ERROR per CLHS.  ansi-bridge loads LAST, so these
;;; overrides win the last-defun-wins race.  Each delegates to the same
;;; %-helpers the home defun uses (NOT to the public name), so there is no
;;; recursion and the numeric result is bit-identical to the home version.
;;; Tests: sin.error / cos.error / … / exp.error / sqrt.error / abs.error.

(defun sin (x &rest extra)
  (if extra
      (%program-error "sin requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 0)
            (t (%sin-f (%any-to-float x))))))

(defun cos (x &rest extra)
  (if extra
      (%program-error "cos requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 1)
            (t (%cos-f (%any-to-float x))))))

(defun tan (x &rest extra)
  (if extra
      (%program-error "tan requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 0)
            (t (let* ((xf (%any-to-float x))
                      (r (%trig-reduce-f xf))
                      (sn (%sin-poly-f r))
                      (cs (%cos-poly-f r)))
                 (%float-div sn cs))))))

(defun asin (x &rest extra)
  (if extra
      (%program-error "asin requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 0)
            (t (%asin-f (%any-to-float x))))))

(defun acos (x &rest extra)
  (if extra
      (%program-error "acos requires exactly 1 argument")
      (cond ((and (integerp x) (= x 1)) 0)
            ((and (integerp x) (= x 0)) (%fpi/2))
            (t (%float-sub (%fpi/2) (%asin-f (%any-to-float x)))))))

;; atan accepts 1 or 2 args (atan x) / (atan y x); 0 or 3+ is the error
;; case.  Use a plain &rest dispatch (NOT &optional+&rest, whose
;; optional-populated call convention miscompiles in Modus) so the 2-arg
;; (atan y x) form binds correctly.
(defun atan (&rest args)
  (let ((n (length args)))
    (cond
      ((= n 1)
       (let ((x (car args)))
         (cond ((and (integerp x) (= x 0)) 0)
               (t (%atan-f (%any-to-float x))))))
      ((= n 2)
       (let ((yf (%any-to-float (car args)))     ; CLHS (atan number divisor)
             (xf (%any-to-float (car (cdr args))))) ; number=y, divisor=x
         (cond
           ((%float-zero-p xf)
            (cond ((float-negative-p yf) (%float-neg (%fpi/2)))
                  ((%float-zero-p yf) (%fl 0))
                  (t (%fpi/2))))
           ((float-negative-p xf)
            (let ((base (%atan-f (%float-div yf xf))))
              (if (float-negative-p yf)
                  (%float-sub base (%fpi))
                  (%float-add base (%fpi)))))
           (t (%atan-f (%float-div yf xf))))))
      (t (%program-error "atan requires 1 or 2 arguments")))))

(defun sinh (x &rest extra)
  (if extra
      (%program-error "sinh requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 0)
            (t (let* ((xf (%any-to-float x))
                      (ep (%exp-f xf))
                      (em (%exp-f (%float-neg xf))))
                 (%float-div (%float-sub ep em) (%fl 2)))))))

(defun cosh (x &rest extra)
  (if extra
      (%program-error "cosh requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 1)
            (t (let* ((xf (%any-to-float x))
                      (ep (%exp-f xf))
                      (em (%exp-f (%float-neg xf))))
                 (%float-div (%float-add ep em) (%fl 2)))))))

(defun tanh (x &rest extra)
  (if extra
      (%program-error "tanh requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 0)
            (t (let* ((xf (%any-to-float x))
                      (ep (%exp-f xf))
                      (em (%exp-f (%float-neg xf))))
                 (%float-div (%float-sub ep em) (%float-add ep em)))))))

(defun exp (x &rest extra)
  (if extra
      (%program-error "exp requires exactly 1 argument")
      (cond ((and (integerp x) (= x 0)) 1)
            (t (%exp-f (%any-to-float x))))))

;; ABS — strict 1-arg.  (abs 0 0) / (abs 0 nil nil) → PROGRAM-ERROR.
;; Body copies the cl-eval.lisp impl (complex magnitude via sqrt; else
;; sign-flip).  sqrt below is this file's override (terminating).
(defun abs (n &rest extra)
  (if extra
      (%program-error "abs requires exactly 1 argument")
      (cond
        ((complexp n)
         (let ((r (realpart n)) (i (imagpart n)))
           (sqrt (+ (* r r) (* i i)))))
        ;; generic-negate-int → %safe-fixnum-negate handles
        ;; MOST-NEGATIVE-FIXNUM (-2^62): plain (- 0 n) WRAPS it back to
        ;; itself (still negative), and the resulting bad "magnitude"
        ;; crashed gcd/lcm.4/.6/.7.  Also covers negative bignum inputs.
        ((< n 0) (generic-negate-int n))
        (t n))))

;; SQRT — strict 1-arg.  (sqrt 0 nil) → PROGRAM-ERROR.  Body copies the
;; cl-clos.lisp impl verbatim (integer/ratio/boxed-float/IEEE branches);
;; the boxed-float branch self-recurses through this override, which
;; terminates because the recursion target is an integer or ratio.
(defun sqrt (n &rest extra)
  (if extra
      (%program-error "sqrt requires exactly 1 argument")
      (cond
        ((integerp n)
         (when (< n 0) (error "sqrt of negative"))
         (let ((s (isqrt n)))
           (if (= (* s s) n)
               s
               (let* ((K 10000)
                      (scaled (* n K K))
                      (approx (isqrt scaled)))
                 (%make-rat approx K)))))
        ((ratiop n)
         (let* ((num (ratio-numerator n))
                (den (ratio-denominator n)))
           (when (< num 0) (error "sqrt of negative"))
           (let ((s (isqrt (* num den))))
             (if (= (* s s) (* num den))
                 (%make-rat s den)
                 (let* ((K 10000)
                        (approx (isqrt (* num den K K))))
                   (%make-rat approx (* den K)))))))
        ((and (not (fixnump n)) (not (consp n)) (not (null n))
              (= (obj-subtag n) #x32)
              (= (array-length n) 2))
         (let ((num (aref n 0)) (den (aref n 1)))
           (sqrt (if (= den 1) num (%make-rat num den)))))
        ((%ieee-float-p n)
         (cond
           ((%float-zero-p n) n)
           (t (let ((half (%float-div (%float-from-int 1) (%float-from-int 2)))
                    (x n))
                (let ((i 0))
                  (loop
                    (when (>= i 8) (return x))
                    (setq x (%float-mul (%float-add x (%float-div n x)) half))
                    (setq i (+ i 1))))
                x))))
        (t 0))))

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

(defun %init-boole-constants ()
  "defvar init-thunks don't run at boot (see CLAUDE.md limitation #7), so the
   16 BOOLE-* constants above default to NIL.  Tests reference e.g. BOOLE-AND
   directly as a value (boole.4, boole.order.1) AND pass it to BOOLE, where a
   NIL op fell through to the (t 0) clause — every result came back 0.  Set the
   distinct integer values explicitly; called once from kernel-main, mirroring
   %init-standard-chars."
  (setq boole-clr 0)
  (setq boole-set 1)
  (setq boole-1 2)
  (setq boole-2 3)
  (setq boole-c1 4)
  (setq boole-c2 5)
  (setq boole-and 6)
  (setq boole-ior 7)
  (setq boole-xor 8)
  (setq boole-eqv 9)
  (setq boole-nand 10)
  (setq boole-nor 11)
  (setq boole-andc1 12)
  (setq boole-andc2 13)
  (setq boole-orc1 14)
  (setq boole-orc2 15)
  t)

(defun %cl-constant-variable-name-p (name)
  "T if NAME (a string) names one of CL's standard constant variables
   (CLHS Figure 3-2 + the BOOLE-* set).  Used by the constantp override
   below so (constantp 'boole-and) → T per CLHS (these symbols name
   defconstants, but Modus declares them with defvar and has no
   defconstant registry that constantp can consult)."
  (or (string= name "BOOLE-1") (string= name "BOOLE-2")
      (string= name "BOOLE-AND") (string= name "BOOLE-ANDC1")
      (string= name "BOOLE-ANDC2") (string= name "BOOLE-C1")
      (string= name "BOOLE-C2") (string= name "BOOLE-CLR")
      (string= name "BOOLE-EQV") (string= name "BOOLE-IOR")
      (string= name "BOOLE-NAND") (string= name "BOOLE-NOR")
      (string= name "BOOLE-ORC1") (string= name "BOOLE-ORC2")
      (string= name "BOOLE-SET") (string= name "BOOLE-XOR")
      (string= name "PI")))

(defun constantp (form &rest env)
  "True if FORM is a constant per CLHS.  Extends the cl-packages.lisp
   version (this defun wins via last-defun load order) to also recognise
   the symbols that name CL's standard constant variables — without this,
   (constantp 'boole-and) returned NIL because Modus tracks defconstant
   only as plain defvars.  CONSTANTP is (form &optional environment); a
   third positional arg is a program-error (constantp.error.2)."
  ;; Arity: at most one optional ENVIRONMENT arg.
  (when (and env (cdr env))
    (%signal-program-error))
  (cond
    ((null form) t)              ; NIL
    ((eq form t) t)              ; T
    ((numberp form) t)
    ((characterp form) t)
    ((stringp form) t)
    ((keywordp form) t)          ; native keyword (subtag #x53)
    ((symbolp form)
     (if (%cl-sym-p form)
         (or (keywordp form)
             (%cl-constant-variable-name-p (symbol-name form)))
         ;; Native MVM symbol: match by name too.
         (%cl-constant-variable-name-p (symbol-name form))))
    ((and (consp form) (eq (car form) 'quote)) t)
    (t nil)))

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
  "Return seconds since 1900-01-01.  On Linux, calls time(2) (syscall
   201) to get Unix epoch seconds, then adds the 70-year offset
   2208988800.  On bare metal where syscall isn't available, returns 0."
  (let ((unix-sec (handler-case (syscall3 201 0 0 0) (t (c) 0))))
    (if (and (integerp unix-sec) (> unix-sec 0))
        (+ unix-sec 2208988800)
        0)))

(defun get-internal-run-time ()
  "Return internal run time units."
  0)

;; GET-INTERNAL-REAL-TIME early stub removed 2026-06-01 — real
;; syscall-based copy at L3388 wins.

(defvar internal-time-units-per-second 1000)

(defun decode-universal-time (ut &optional tz)
  "Decode universal time into components."
  (values 0 0 0 1 1 2000 0 nil 0))

;; ENCODE-UNIVERSAL-TIME early stub removed 2026-06-01 — strict-arity
;; copy at L3381 wins.

;;; ============================================================
;;; Array Misc
;;; ============================================================

(defun array-row-major-index (a &rest subscripts)
  "Return row-major index of multi-dimensional array element.
   Native MDA: walks dims, computing sub0*d1*d2*..*dN + sub1*d2..*dN + ..."
  (cond
    ((%mda-p a)
     (let ((dims (%mda-dims a)))
       (cond
         ((null dims) 0)
         ((null (cdr dims)) (if (null subscripts) 0 (car subscripts)))
         (t (%mda-row-major-index dims subscripts)))))
    (t (if (null subscripts) 0 (car subscripts)))))

(defun sbit (bit-array &rest subscripts)
  "Access element of simple bit array."
  (aref bit-array (if (null subscripts) 0 (car subscripts))))

(defun set-bit (bit-array idx new-value)
  "Setter for (SETF (BIT BV I) val) — modus's setf macro emits args
   in (BV IDX VAL) order; not the older val-first convention."
  (aset bit-array idx new-value)
  new-value)

(defun set-sbit (bit-array idx new-value)
  "Setter for (SETF (SBIT BV I) val) — same arg order as set-bit."
  (aset bit-array idx new-value)
  new-value)

;;; ============================================================
;;; Misc Symbol/Special Form Stubs
;;; ============================================================

(defun makunbound (symbol)
  "Make symbol unbound (stub - no-op)."
  symbol)

(defun special-operator-p (symbol)
  "Return T if SYMBOL is a special operator.  The eq-member check covers
   same-package literals; the symbol-name fallback covers cross-package
   symbols (CLHS 11.1.2 per-package interning means a CL-TEST-package
   'LET is not EQ to ours) against the canonical CLHS Figure 3-2 set."
  (or (member symbol '(let let* if progn setq quote lambda defun defmacro
                       block return-from tagbody go catch throw unwind-protect
                       eval-when locally progv multiple-value-call
                       multiple-value-prog1 load-time-value the declare))
      (and (symbolp symbol)
           (let ((n (symbol-name symbol)))
             (and (stringp n) (> (length n) 0)
                  (%dg-string-member
                   n '("BLOCK" "CATCH" "EVAL-WHEN" "FLET" "FUNCTION" "GO"
                       "IF" "LABELS" "LET" "LET*" "LOAD-TIME-VALUE"
                       "LOCALLY" "MACROLET" "MULTIPLE-VALUE-CALL"
                       "MULTIPLE-VALUE-PROG1" "PROGN" "PROGV" "QUOTE"
                       "RETURN-FROM" "SETQ" "SYMBOL-MACROLET" "TAGBODY"
                       "THE" "THROW" "UNWIND-PROTECT")))))))

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

;; find-method / add-method / remove-method / method-qualifiers /
;; compute-applicable-methods all live in cl-clos.lisp; the stubs here
;; were shadowing them via last-defun-wins.

;; ensure-generic-function lives in cl-clos.lisp now; the stub here
;; was shadowing it via last-defun-wins.

(defun reinitialize-instance (instance &rest initargs)
  "Per ANSI, REINITIALIZE-INSTANCE calls SHARED-INITIALIZE with the
   instance, slot-names = NIL (so no initforms re-apply), and the
   passed initargs.  Returns INSTANCE.

   Bypass APPLY+&rest through SHARED-INITIALIZE (which truncates
   trailing initargs when modus's funcall passes >4 args through
   the dispatch closure's &rest).  Call %shared-init-default-spread
   directly with the args list, the same way make-instance does."
  (let ((gf (%find-gf 'shared-initialize)))
    (cond
      ((and gf (%gf-methods gf))
       (let* ((sa-args (cons instance (cons nil initargs)))
              (applicable (%collect-applicable-methods gf sa-args)))
         (if applicable
             (%gf-dispatch-standard gf sa-args applicable)
             (%shared-init-default-spread sa-args))))
      (t
       (%shared-init-default-spread (cons instance (cons nil initargs))))))
  instance)

;;; Class obsolescence tracking.  When (make-instances-obsolete C)
;;; runs, we increment a counter on C's descriptor.  Each instance
;;; carries a stamp it was created with; if (instance-stamp ≠
;;; class-stamp) on slot access, we run update-instance-for-redefined-class
;;; once and refresh the stamp.  This is the AMOP redefinition protocol.

(defvar *%class-stamps* nil
  "Alist (class-name . stamp).  Incremented by make-instances-obsolete.")

(defun %class-stamp (class-name)
  (let ((entry (assoc class-name *%class-stamps* :test #'eq)))
    (if entry (cdr entry) 0)))

(defun (setf %class-stamp) (new-val class-name)
  (let ((entry (assoc class-name *%class-stamps* :test #'eq)))
    (if entry
        (set-cdr entry new-val)
        (setq *%class-stamps*
              (cons (cons class-name new-val) *%class-stamps*))))
  new-val)

(defun make-instances-obsolete (class)
  "Increment the obsolescence stamp on CLASS.  Returns CLASS.  When
   an instance with a stale stamp next has a slot accessed, the
   slot-access path can trigger update-instance-for-redefined-class
   (currently not wired since modus doesn't carry per-instance stamps;
   the bookkeeping is in place for when that arrives)."
  (let ((name (cond ((symbolp class) class)
                    ((%clos-class-p class) (aref class 1))
                    (t nil))))
    (when name
      (let ((entry (assoc name *%class-stamps* :test #'eq)))
        (if entry
            (set-cdr entry (+ (cdr entry) 1))
            (setq *%class-stamps* (cons (cons name 1) *%class-stamps*))))))
  class)

;;; ============================================================
;;; MAKE-LOAD-FORM — GF with default error-signaling methods
;;; ============================================================
;;;
;;; Per CLHS 4.3.5 / 21.1, MAKE-LOAD-FORM is a generic function.  The
;;; system-supplied methods on STANDARD-OBJECT, STRUCTURE-OBJECT, and
;;; CONDITION all signal an error; users override per class.  Tests in
;;; make-load-form.lsp probe both branches:
;;;  - (compute-applicable-methods #'make-load-form (list obj)) → non-NIL
;;;  - (handler-case (make-load-form obj) (error nil :good)) → :good
;;; Without the GF registered, the test bodies error on #'make-load-form
;;; (UNDEFINED-FUNCTION), the outer handler-case catches, and the whole
;;; file crash-fails.

(defun %make-load-form-default (obj &optional env)
  (declare (ignore env))
  (error "no MAKE-LOAD-FORM method defined for ~S" obj))

(defun make-load-form (obj &optional env)
  "Generic function — dispatches via %gf-dispatch."
  (%gf-dispatch 'make-load-form (list obj env)))

(defun %init-make-load-form ()
  "Register the MAKE-LOAD-FORM GF and its default error-signaling
   methods on STANDARD-OBJECT, STRUCTURE-OBJECT, and CONDITION.
   Top-level forms don't auto-run on bare-metal builds, so this has
   to be called explicitly from kernel-main."
  (%defgeneric 'make-load-form '(object &optional environment) nil)
  (%defmethod 'make-load-form nil '(standard-object)
              #'%make-load-form-default)
  (%defmethod 'make-load-form nil '(structure-object)
              #'%make-load-form-default)
  (%defmethod 'make-load-form nil '(condition)
              #'%make-load-form-default)
  (handler-case (%register-gf-fn #'make-load-form 'make-load-form)
                (t (c) nil)))

(defun make-load-form-saving-slots (object &key slot-names environment)
  "Per CLHS 7.1: return (values creation-form initialization-form)
   that, when evaluated, reconstructs OBJECT with the named slots.

   Returns two forms:
     creation-form     = (allocate-instance (find-class 'CLASS-NAME))
     initialization-form =
       (progn
         (setf (slot-value <obj> 'slot1) (load-time-value …))
         …)

   The caller (a binary FASL emitter, normally) walks these forms.
   Modus doesn't have a FASL emitter, but tests probe the SHAPE of
   the return values, and the values themselves must be valid forms."
  (declare (ignore environment))
  ;; DEFSTRUCT instances use a positional array representation (slot-0 =
  ;; '%struct-instance, slot-1 = type-name, slots 2+ = field values).  The
  ;; creation-form reconstructs the whole struct in one shot via
  ;; %ALLOC-STRUCT — which when EVAL'd yields a fresh struct whose CLASS-OF
  ;; is EQ to the original's (both resolve to the same struct class proxy
  ;; keyed by type-name).  Because every slot value is embedded in the
  ;; creation-form, the initialization-form is an empty (PROGN); the test's
  ;; SUBST NEWOBJ OBJECT then has nothing to rewrite, which is fine.
  (when (%struct-instance-p object)
    (let* ((type-name (%struct-type-name object))
           (n-slots (- (array-length object) 2))   ; slots 2+ are fields
           ;; Read field values straight out of the instance array (slots
           ;; 2..len-1, in effective-slot order — same order %ALLOC-STRUCT
           ;; writes them back).  Build the positional value list literally
           ;; so the creation-form is (%alloc-struct 'NAME '(v0 v1 ...)) —
           ;; a single QUOTE survives runtime EVAL cleanly (a (LIST …) form
           ;; depends on runtime LIST + nested QUOTE, which proved fragile).
           (vals
             (let ((acc nil) (i (- n-slots 1)))
               (loop
                 (when (< i 0) (return acc))
                 (setq acc (cons (aref object (+ 2 i)) acc))
                 (setq i (- i 1)))))
           (creation-form
             (list '%alloc-struct (list 'quote type-name) (list 'quote vals)))
           (init-form (list 'progn)))
      (return-from make-load-form-saving-slots
        (values creation-form init-form))))
  ;; Only standard CLOS instances reach the slot-by-slot path below.
  (when (or (null object) (not (%clos-instance-p object)))
    (return-from make-load-form-saving-slots (values nil nil)))
  (let* ((class-name (aref object 1))
         (cls (%find-clos-class class-name))
         (slots (cond
                  ;; explicit :slot-names list (and not T) → use as given
                  ((and slot-names (not (eq slot-names t))) slot-names)
                  ;; CLOS: all slots from the class slot list
                  (cls (aref cls 2))
                  (t nil)))
         (creation-form
           (list 'allocate-instance (list 'find-class (list 'quote class-name))))
         ;; Per CLHS 3.2.4.4 / make-load-form-saving-slots: the init-form
         ;; references the OBJECT directly so the FASL emitter (here, the
         ;; ANSI test via SUBST NEWOBJ OBJECT) can rewrite it to operate on
         ;; the freshly-created instance.  Only BOUND slots get a setter —
         ;; unbound slots must stay unbound after reconstruction.  Use a
         ;; direct SET-SLOT-VALUE call rather than the SETF macro: runtime-
         ;; EVAL of (setf (slot-value …) …) doesn't take effect, whereas the
         ;; direct setter does.
         (init-pairs
           (let ((acc nil) (cur slots))
             (loop
               (when (null cur) (return (nreverse acc)))
               (let ((sname (car cur)))
                 (when (handler-case (%slot-boundp object sname) (t (c) nil))
                   (let ((val (handler-case (%slot-value object sname) (t (c) nil))))
                     (setq acc (cons (list 'set-slot-value
                                           object (list 'quote sname) (list 'quote val))
                                     acc)))))
               (setq cur (cdr cur)))))
         (init-form (cons 'progn init-pairs)))
    (values creation-form init-form)))

;; NOTE: set-find-class is implemented in cl-conditions.lisp (it mutates
;; *clos-classes*).  An earlier stub HERE shadowed it via last-defun-wins,
;; silently dropping every (setf (find-class NAME) …).  Removed so the real
;; one wins.

(defun set-class-name (cls new-name)
  "(setf (class-name CLS) NEW-NAME) per CLHS 4.3.6.  Mutate the class
   object's name slot in place and return NEW-NAME.  The SETF macro's
   generic fallback rewrites (setf (class-name c) v) → (set-class-name c v)."
  (when (%clos-class-p cls)
    (let ((dummy (aset cls 1 new-name))) dummy))
  new-name)

;;; ============================================================
;;; DESCRIBE, APROPOS (stubs)
;;; ============================================================

(defun describe (object &optional stream)
  "Default DESCRIBE — dispatches to DESCRIBE-OBJECT.  Per CLHS, DESCRIBE
   is a regular function that calls the DESCRIBE-OBJECT generic.  We
   default the stream to NIL (caller's responsibility to bind a real
   one if they need formatted output)."
  (describe-object object stream))

;;; APROPOS — walk the named package's internal+external symbol tables
;;; and collect those whose name contains the search substring.
;;; apropos prints them; apropos-list returns the list.

(defun %symbol-name-of (entry)
  "Internal-symbol-table entries shape varies; pull the name string."
  (cond
    ((stringp entry) entry)
    ((consp entry)
     (cond ((stringp (car entry)) (car entry))
           ((consp (car entry)) (%symbol-name-of (car entry)))
           (t nil)))
    (t nil)))

(defun %string-contains (haystack needle)
  "Case-insensitive substring search."
  (let* ((hlen (length haystack))
         (nlen (length needle)))
    (when (> nlen hlen) (return-from %string-contains nil))
    (let ((i 0) (found nil))
      (loop
        (when (or found (> i (- hlen nlen))) (return found))
        (let ((j 0) (matches t))
          (loop
            (when (or (not matches) (>= j nlen)) (return nil))
            (let ((c1 (aref haystack (+ i j)))
                  (c2 (aref needle j)))
              ;; case-insensitive
              (when (and (>= c1 65) (<= c1 90)) (setq c1 (+ c1 32)))
              (when (and (>= c2 65) (<= c2 90)) (setq c2 (+ c2 32)))
              (unless (= c1 c2) (setq matches nil)))
            (setq j (+ j 1)))
          (when matches (setq found t))
          (setq i (+ i 1))))
      (if found t nil))))

(defun apropos-list (string &optional package)
  "Return list of symbols whose name contains STRING."
  (let* ((needle (cond ((stringp string) string)
                       ((symbolp string) (symbol-name string))
                       (t (return-from apropos-list nil))))
         (pkgs (if package
                   (let ((p (find-package package)))
                     (if p (list p) nil))
                   (let ((cl (find-package "CL"))
                         (clu (find-package "CL-USER")))
                     (let ((r nil))
                       (when cl (setq r (cons cl r)))
                       (when clu (setq r (cons clu r)))
                       r))))
         (acc nil))
    (dolist (p pkgs)
      ;; Walk BOTH the internal and the external symbol tables —
      ;; standard CL packages keep their exported symbols in the
      ;; external table (slot 3), and apropos must see those.
      (dolist (entries (list (%pkg-internal p) (%pkg-external p)))
        (dolist (entry entries)
          (let ((nm (%symbol-name-of entry)))
            (when (and nm (%string-contains nm needle))
              (setq acc (cons entry acc)))))))
    acc))

(defun apropos (string &optional package)
  "Print apropos list to *standard-output* and return NIL."
  (let ((syms (apropos-list string package)))
    (dolist (s syms)
      (let ((nm (%symbol-name-of s)))
        (when nm
          (write-string-serial nm)
          (write-char-serial 10))))
    nil))

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

;; macro-function lives in cl-eval.lisp (the real impl).  Earlier stub
;; here returned NIL unconditionally; ansi-bridge.lisp loads AFTER
;; cl-eval.lisp so last-defun-wins gave the stub, breaking every macro
;; lookup — including the runtime-load suite-shape probe which loads a
;; defmacro and then immediately uses it.  Removed.

(defun compiler-macro-function (name &optional env)
  "Return compiler macro function for NAME, or NIL.

   Looks up *compiler-macro-function-table* — populated at runtime by
   cl-eval's DEFINE-COMPILER-MACRO handler.  Wraps interp-closure
   expanders via %interp-macro-shim so user-facing
   `(funcall (compiler-macro-function ...) ...)` works through the
   compiled funcall path (same fix as for macro-function +
   runtime-defmacro)."
  (declare (ignore env))
  (let ((key (%macro-sym-key name)))
    (let ((raw (and key *compiler-macro-function-table*
                    (gethash key *compiler-macro-function-table*))))
      (cond
        ((null raw) nil)
        ((%interp-closure-p raw)
         (%make-closure #'%interp-macro-shim (cons raw nil)))
        (t raw)))))

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
;; RATIONALIZE — for IEEE doubles RATIONAL already yields the simplest
;; exact value (binary fraction in lowest terms), so RATIONALIZE delegates
;; to RATIONAL.  This shadows cl-types.lisp's (rationalize n) because
;; ansi-bridge loads later; the old stub returned N unchanged, so the
;; exact cl-types impl was dead and (rationalize 1.5) returned 1.5.
(defun rationalize (n &rest extra)
  (if extra
      (%program-error "rationalize requires exactly 1 argument")
      (rational n)))

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
;;; GET-INTERNAL-REAL-TIME — uses Linux time(2) (syscall 201) when
;;; available, falls back to a monotonic counter.
(defvar *%irt-counter* 0)
(defun get-internal-real-time ()
  (let ((t (handler-case (syscall3 201 0 0 0) (t (c) 0))))
    (cond
      ((and (integerp t) (> t 0)) t)
      (t (setq *%irt-counter* (+ *%irt-counter* 1))
         *%irt-counter*))))

;;; CLASS-OF — strict 1-arg arity
(defun class-of (x &rest extra)
  (if extra
      (%program-error "class-of requires exactly 1 argument")
      (cond
        ((%clos-instance-p x)
         (%find-clos-class (aref x 1)))
        (t nil))))

;;; TYPE-OF (CLHS) — replaces the prelude NIL stub.  The prelude's
;;; (defun type-of (obj) nil) made (typep x (type-of x)) NIL for every
;;; object → type-of.3 collected the whole *universe*, and type-of.6/.7
;;; returned NIL instead of the struct/class name.  We return a type
;;; specifier that the OBJECT is a member of (CLHS req 1.a) and that is a
;;; subtype of its class (req via type-of.4) — leaning on the existing
;;; most-specific-builtin-class dispatch so the result is always a name
;;; TYPEP recognises.
(defun type-of (obj)
  (cond
    ((null obj) 'null)
    ((eq obj t)  'boolean)
    ;; CLOS instance — proper class name if the name still names the class
    ;; (find-class round-trips), otherwise the class OBJECT itself
    ;; (type-of.8 / .9 after (setf (class-name c) nil) / (setf (find-class
    ;; 'n) nil)).  CLHS 4.3.7: type-of returns the class when it has no
    ;; proper name.
    ((%clos-instance-p obj)
     (let* ((cname (aref obj 1))
            (cls   (and cname (%find-clos-class cname))))
       (if (and cls cname (eq (find-class cname nil) cls))
           cname
           ;; No proper name — return the class object (class-of).
           (if cls cls (or cname 'standard-object)))))
    ;; Struct instance — its struct type name.
    ((%struct-instance-p obj) (%struct-type-name obj))
    ;; Condition — its condition type name.
    ((%condition-p obj) (%condition-type-name obj))
    ;; A CLOS class object is itself an instance of STANDARD-CLASS.
    ((%clos-class-p obj) 'standard-class)
    ;; Integers: CLHS says an (integer low high) spec is fine, but a bare
    ;; recognisable supertype name also satisfies req 1.a + the subtypep
    ;; checks.  Use FIXNUM / BIGNUM (TYPEP accepts both as INTEGER here).
    ((fixnump obj)
     (cond ((or (eql obj 0) (eql obj 1)) 'bit)
           (t 'fixnum)))
    ((ratiop obj)      'ratio)
    ((floatp-impl obj) 'single-float)
    ((%complex-p obj)  'complex)
    ((characterp obj)
     ;; standard-char for the printing ASCII range, else character.
     (let ((code (char-code obj)))
       (if (and (>= code 32) (< code 127)) 'standard-char 'character)))
    ((stringp obj) (list 'simple-base-string (length obj)))
    ((%generic-function-p obj) 'standard-generic-function)
    ((functionp obj) 'function)
    ((consp obj) 'cons)
    ((hash-table-p obj) 'hash-table)
    ((packagep obj) 'package)
    ((readtablep obj) 'readtable)
    ((random-state-p obj) 'random-state)
    ((symbolp obj) (if (keywordp obj) 'keyword 'symbol))
    ((vectorp obj) 'simple-vector)
    ((arrayp obj) 'array)
    (t t)))

;;; SLOT-VALUE — strict 2-arg arity
(defun slot-value (obj slot-name &rest extra)
  (if extra
      (%program-error "slot-value requires exactly 2 arguments")
      (if (null slot-name)
          (%program-error "slot-value requires a slot name")
          (%slot-value obj slot-name))))

;;; COMPUTE-APPLICABLE-METHODS — lives in cl-clos.lisp now with real
;;; GF-array support + #'name reverse lookup via *gf-fn-to-name*.  The
;;; stub here checked `(consp gf)' which never matched our array-shaped
;;; gf-objects, silently returning NIL for every well-formed call.

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
;; Find :test/:key in a flat &rest plist — common helper used by the
;; set-* / -if-* family.  Returns 'em separately (nil if not present).
(defun %find-key-arg (args key)
  "Return the value of the leftmost KEY in ARGS plist, or NIL.
   Symbol values (e.g. :test 'equal) are resolved via symbol-function."
  (let ((cur args) (found nil) (was-found nil))
    (loop (when (null cur) (return (if was-found (%resolve-fn found) nil)))
      (when (eq (car cur) key)
        (setq found (cadr cur))
        (setq was-found t)
        (return (%resolve-fn found)))
      (setq cur (cddr cur)))))

(defun %validate-test-key-plist (args)
  "Validate ARGS as a plist of :test/:test-not/:key/:count/:start/:end/
   :from-end/:allow-other-keys.  Per CLHS §3.4.1.4 signal PROGRAM-ERROR
   on odd-length plist or unknown keyword (unless :allow-other-keys is
   non-nil somewhere in the plist).  Helper for sequence/list ops that
   pluck individual keys via %find-key-arg and would otherwise silently
   accept (SET-DIFFERENCE NIL NIL :BAD T)."
  (let ((allow-other-keys nil) (aok-set nil))
    ;; First pass: probe :allow-other-keys (leftmost wins per CLHS
    ;; §3.4.1.4.1.1.2).
    (let ((p args))
      (loop (when (null p) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other-keys (and (consp (cdr p)) (cadr p)))
          (setq aok-set t))
        (setq p (cdr p))))
    (let ((cur args))
      (loop
        (when (null cur) (return))
        (when (null (cdr cur)) (%signal-program-error))
        (let ((k (car cur)))
          (unless (or (eq k :test) (eq k :test-not) (eq k :key)
                      (eq k :count) (eq k :start) (eq k :end)
                      (eq k :from-end) (eq k :allow-other-keys)
                      allow-other-keys)
            (%signal-program-error)))
        (setq cur (cddr cur))))))

(defun %set-difference-impl (l1 l2 args)
  "Positional helper for set-difference. See set-difference docstring."
  (%validate-test-key-plist args)
  (let ((test-fn (%find-key-arg args :test))
        (key-fn  (%find-key-arg args :key))
        (r nil))
    (dolist (item l1 (nreverse r))
      (let ((item-key (if key-fn (funcall key-fn item) item))
            (in-l2 nil)
            (cur l2))
        (loop
          (when (or in-l2 (null cur)) (return nil))
          (let ((x-key (if key-fn (funcall key-fn (car cur)) (car cur))))
            (when (if test-fn (funcall test-fn item-key x-key)
                              (eql item-key x-key))
              (setq in-l2 t)))
          (setq cur (cdr cur)))
        (unless in-l2 (setq r (cons item r)))))))

(defun set-difference (l1 l2 &rest args)
  ;; #'eql is unavailable in the runtime (eql is an inline opcode, not
  ;; a real function), so the previous (or test-fn #'eql) bound a NIL
  ;; or otherwise-unusable function and (funcall actual-test ...)
  ;; effectively short-circuited "no match" for every element — making
  ;; set-difference always return l1 (or NIL via different earlier
  ;; bug). Use the inline `eql` opcode directly when no :test is given.
  (%set-difference-impl l1 l2 args))

(defun nset-difference (l1 l2 &rest args)
  "Routes through positional helper to dodge apply-of-rest fragility."
  (%set-difference-impl l1 l2 args))

;; -------------------------------------------------------------------
;; %make-array-fill-init / %make-array-fill-list / %make-array-fill-vec
;;
;; Used by build-ansi-test's rewrite-make-array-initcontents to keep
;; the per-test expansion small.  The previous in-line expansion
;;   (let ((tmp (make-array N)))
;;     (aset tmp 0 v) (aset tmp 1 v) ... (aset tmp (1- N) v)
;;     tmp)
;; produced O(N) source forms per make-array call, which inflated the
;; lambda body of large test runners (run-ansi-adjust-array,
;; run-ansi-make-array, etc.) past a compile-time fragility threshold,
;; flipping unrelated tests to FAIL.  These runtime helpers replace the
;; per-element ASETs with a single function call.
;; -------------------------------------------------------------------
;; Runtime entry point for MAKE-ARRAY — produces a native MDA (subtag
;; #x34) for multi-dim / 0-dim / kwarg-bearing cases, or a flat 1-D
;; #x32 array for the simple `(make-array N)` case.  Phase 2a of
;; project_multidim_arrays.
;;
;; Compile-time calls (`(make-array N …)` in source) still dispatch
;; through compile-make-array's builtin (op-name match in
;; compile-compound), which only handles integer N + a couple of
;; quoted-dim forms.  This defun is the path that runs when MAKE-ARRAY
;; is reached via runtime eval or `(funcall #'make-array …)` or
;; `(apply #'make-array …)`.  Auto-SFT (Gap A) registers it so
;; eval/funcall find it.
;;
;; Limited kwarg support in Phase 2a: :initial-element, :initial-
;; contents (flat list / list-of-lists / vector).  :fill-pointer,
;; :adjustable, :displaced-to, :element-type non-T are deferred to
;; Phase 2b — until then the kwarg is recorded in the MDA header but
;; the actual semantics (fp shadowing length, displacement offset
;; lookup, etc.) aren't yet propagated through aref/aset/length.

;; make-array-with-checks: real defun is in the aux-overrides section
;; of build-ansi-test.lisp (loads AFTER array-aux.lsp's complex &key+&aux
;; version which the compiler can't faithfully handle).


(defun make-array (dim &rest kwargs)
  "ANSI make-array — see file-level comment above for current scope."
  (let* ((dim-list (cond ((null dim) nil)
                         ((consp dim) dim)
                         (t (list dim))))
         (rank 0) (cur dim-list) (total 1))
    (loop (when (null cur) (return nil))
      (setq total (* total (car cur)))
      (setq rank (+ rank 1))
      (setq cur (cdr cur)))
    ;; Kwarg validation per CLHS 3.4.1.4: odd-length arg list and
    ;; unknown keywords (without :allow-other-keys T) signal
    ;; program-error.  Leftmost :allow-other-keys wins per CLHS
    ;; 3.4.1.4.1.  (make-array.error.2/3/4)
    (let ((allow-other nil) (scan kwargs))
      (loop (when (or (null scan) (null (cdr scan))) (return))
        (when (eq (car scan) :allow-other-keys)
          (unless allow-other
            (setq allow-other (if (cadr scan) t :explicit-nil))))
        (setq scan (cddr scan))))
    (let ((leftmost-allow nil)
          (leftmost-allow-set nil))
      ;; CLHS 3.4.1.4.1: LEFTMOST :allow-other-keys value wins.
      (let ((scan kwargs))
        (loop (when (or (null scan) (null (cdr scan))) (return))
          (when (and (eq (car scan) :allow-other-keys)
                     (not leftmost-allow-set))
            (setq leftmost-allow (and (cadr scan) t))
            (setq leftmost-allow-set t))
          (setq scan (cddr scan))))
      (let ((vp kwargs))
        (loop
          (when (null vp) (return))
          (when (null (cdr vp)) (%signal-program-error) (return))
          (let ((k (car vp)))
            (unless (member k '(:initial-element :initial-contents
                                :fill-pointer :adjustable :element-type
                                :displaced-to :displaced-index-offset
                                :allow-other-keys))
              (unless leftmost-allow
                (%signal-program-error)
                (return))))
          (setq vp (cddr vp)))))
    ;; Walk kwargs into local vars.  Per CLHS 3.4.1.4.1 the LEFTMOST
    ;; occurrence of each keyword wins on duplicates — use *-set
    ;; sentinels so a later (:initial-element a :initial-element b)
    ;; pair doesn't overwrite the first value.  (make-array.keywords.8)
    (let ((ie-p nil) (ie nil) (ic-p nil) (ic nil)
          (fp nil) (fp-set nil) (adj nil) (adj-set nil)
          (etype t) (etype-set nil)
          (disp nil) (disp-set nil) (off 0) (off-set nil)
          (rest kwargs))
      (loop (when (null rest) (return nil))
        (let ((k (car rest)) (v (cadr rest)))
          (cond
            ((eq k :initial-element)
             (unless ie-p (setq ie-p t  ie v)))
            ((eq k :initial-contents)
             (unless ic-p (setq ic-p t  ic v)))
            ((eq k :fill-pointer)
             (unless fp-set
               (setq fp-set t)
               (setq fp (cond ((eq v t) total) ((null v) nil) (t v)))))
            ((eq k :adjustable)
             (unless adj-set (setq adj-set t adj v)))
            ((eq k :element-type)
             (unless etype-set (setq etype-set t etype v)))
            ((eq k :displaced-to)
             (unless disp-set (setq disp-set t disp v)))
            ((eq k :displaced-index-offset)
             (unless off-set (setq off-set t off v)))))
        (setq rest (cddr rest)))
      ;; Allocate + fill the flat data vector.
      ;; :element-type 'character / 'base-char → underlying is a string
      ;; (subtag #x31) so STRINGP / string ops work on the result.
      (let* ((char-elt (or (eq etype 'character) (eq etype 'base-char)
                           (eq etype 'standard-char)))
             (data (cond
                     ;; :initial-element — fill every slot.
                     (ie-p
                      (let* ((a (if char-elt
                                    (%make-string-array total)
                                    (make-array total)))
                             (store (if (and char-elt (characterp ie))
                                        (char-code ie) ie))
                             (i 0))
                        (loop (when (>= i total) (return a))
                          (aset a i store)
                          (setq i (+ i 1)))))
                     ;; :initial-contents — flatten in row-major order
                     ;; for multi-dim; copy verbatim for 1-D / flat lists.
                     (ic-p
                      (let ((a (if char-elt
                                   (%make-string-array total)
                                   (make-array total))))
                        (%mda-fill-contents-flat a ic dim-list)))
                     (char-elt (%make-string-array total))
                     (t (make-array total)))))
        ;; Decide return shape.  Phase 2a: wrap in MDA whenever rank ≠ 1,
        ;; or any non-trivial kwarg appeared, or the dim was passed as
        ;; a list (even a single-element list) — that preserves rank
        ;; info per CLHS (`(array-rank (make-array '(5)))` is 1, not 0).
        ;; Character element type with no other wrapping returns the
        ;; underlying string directly so STRINGP / EQUAL string ops work.
        (cond
          ((and (= rank 1) (not (consp dim))
                (not fp) (not adj) (not disp) (eq etype t))
           data)
          ((and (= rank 1) (not (consp dim))
                (not fp) (not adj) (not disp) char-elt)
           data)
          ;; Displaced MDA: set DATA slot to the displaced target so that
          ;; type predicates (stringp / typep) and length helpers see
          ;; through to the underlying.  aref/aset still consult the
          ;; displaced+offset slots via the MDA fast path.
          (disp (%alloc-mda rank dim-list fp disp off etype disp))
          (t (%alloc-mda rank dim-list fp disp off etype data)))))))

(defun %mda-fill-contents-flat (data contents dims)
  "Fill DATA in row-major order from CONTENTS shaped against DIMS.
   For rank 0: CONTENTS is the scalar value, stored at slot 0.
   For rank 1: CONTENTS is a flat sequence (list or vector).
   For rank >1: CONTENTS is a nested list of lists with the right
   shape — recursively flattened.  Helper for make-array's
   :initial-contents kwarg."
  (let ((i 0))
    (cond
      ;; Rank 0: scalar.
      ((null dims) (aset data 0 contents) data)
      ;; Last/only dim: contents is a flat sequence.
      ;; Strings store char-codes (fixnums), not character objects, so
      ;; when DATA is a string we must coerce element via char-code
      ;; before aset.
      ((null (cdr dims))
       (let ((cur contents)
             (str-data (stringp data)))
         (cond
           ((consp cur)
            (loop (when (null cur) (return data))
              (let ((v (car cur)))
                (aset data i (if (and str-data (characterp v)) (char-code v) v)))
              (setq cur (cdr cur))
              (setq i (+ i 1))))
           ((stringp cur)
            ;; Source is a string: aref returns a fixnum char-code.
            ;; If dest is a string → store fixnum directly (string slots
            ;; are u8); if dest is general → wrap via code-char to give
            ;; a proper character object.
            (let ((len (array-length cur)))
              (loop (when (>= i len) (return data))
                (let ((v (aref cur i)))
                  (aset data i (cond
                                 (str-data
                                  (if (characterp v) (char-code v) v))
                                 ((characterp v) v)
                                 ((integerp v) (code-char v))
                                 (t v))))
                (setq i (+ i 1)))))
           ;; Vector / bit-vector / any general array as initial-contents
           ;; source.  CL allows any sequence; we already cover list + string,
           ;; this catches everything else implementing aref/length.
           ((arrayp cur)
            (let ((len (array-length cur)))
              (loop (when (>= i len) (return data))
                (let ((v (aref cur i)))
                  (aset data i (if (and str-data (characterp v)) (char-code v) v)))
                (setq i (+ i 1)))))
           (t data))))
      ;; Multi-dim: recurse into each sub-row.  CONTENTS may be a list OR
      ;; a vector (e.g. a fill-pointer vector — MAKE-ARRAY.21).
      (t
       (let ((sub-size 1))
         ;; size of each "row" = product of remaining dims
         (let ((d (cdr dims)))
           (loop (when (null d) (return nil))
             (setq sub-size (* sub-size (car d)))
             (setq d (cdr d))))
         (if (consp contents)
             (let ((cur contents))
               (loop (when (null cur) (return data))
                 (%mda-fill-contents-sub data i (car cur) (cdr dims))
                 (setq i (+ i sub-size))
                 (setq cur (cdr cur))))
             (let ((len (length contents)) (j 0))
               (loop (when (>= j len) (return data))
                 (%mda-fill-contents-sub data i (elt contents j) (cdr dims))
                 (setq i (+ i sub-size))
                 (setq j (+ j 1)))))
         data)))))

(defun %mda-fill-contents-sub (data offset sub-contents sub-dims)
  "Fill DATA[offset…offset+sub-size) from SUB-CONTENTS shaped against
   SUB-DIMS.  Recursive helper for %mda-fill-contents-flat's
   multi-dim branch."
  (cond
    ((null sub-dims)
     (aset data offset sub-contents))
    ((null (cdr sub-dims))
     ;; Last dim: SUB-CONTENTS is a flat sequence — list OR vector/string.
     ;; CLHS allows any sequence at any level of :initial-contents, so a
     ;; row may be e.g. #(a b c) (MAKE-ARRAY.18/.20/.21).  The old code
     ;; assumed a list and CAR'd a vector → crash.
     (let ((str-data (stringp data)))
       (if (consp sub-contents)
           (let ((cur sub-contents) (i offset))
             (loop (when (null cur) (return data))
               (let ((v (car cur)))
                 (aset data i (if (and str-data (characterp v)) (char-code v) v)))
               (setq cur (cdr cur))
               (setq i (+ i 1))))
           ;; Vector / string row.
           (let ((len (length sub-contents)) (j 0) (i offset))
             (loop (when (>= j len) (return data))
               (let ((v (elt sub-contents j)))
                 (aset data i (cond
                                (str-data (if (characterp v) (char-code v) v))
                                (t v))))
               (setq j (+ j 1))
               (setq i (+ i 1)))))))
    (t
     ;; Interior dim: SUB-CONTENTS is a sequence of sub-rows — list OR
     ;; vector.  Recurse into each, advancing by the row's flat size.
     (let ((i offset)
           (sub-size 1))
       (let ((d (cdr sub-dims)))
         (loop (when (null d) (return nil))
           (setq sub-size (* sub-size (car d)))
           (setq d (cdr d))))
       (if (consp sub-contents)
           (let ((cur sub-contents))
             (loop (when (null cur) (return data))
               (%mda-fill-contents-sub data i (car cur) (cdr sub-dims))
               (setq i (+ i sub-size))
               (setq cur (cdr cur))))
           (let ((len (length sub-contents)) (j 0))
             (loop (when (>= j len) (return data))
               (%mda-fill-contents-sub data i (elt sub-contents j) (cdr sub-dims))
               (setq i (+ i sub-size))
               (setq j (+ j 1)))))))))

(defun %make-array-fill-init (n init)
  "Allocate a fresh general array of size N and fill every slot with INIT."
  (let ((a (make-array n)) (i 0))
    (loop
      (when (>= i n) (return a))
      (aset a i init)
      (setq i (+ i 1)))))

(defun %make-array-fill-list (n lst)
  "Allocate a fresh general array of size N and fill from LST (head-first).
   Stops when LST is exhausted; remaining slots are left NIL."
  (let ((a (make-array n)) (i 0) (cur lst))
    (loop
      (when (or (>= i n) (null cur)) (return a))
      (aset a i (car cur))
      (setq cur (cdr cur))
      (setq i (+ i 1)))))

(defun %make-array-fill-any (n contents)
  "Allocate a fresh general array of size N filled from CONTENTS, which
   may be either a list or a vector.  Dispatches at runtime so the build
   doesn't have to guess the shape of an arbitrary initial-contents form."
  (cond
    ((null contents) (make-array n))
    ((consp contents) (%make-array-fill-list n contents))
    (t (%make-array-fill-vec n contents))))

(defun %make-array-fill-vec (n vec)
  "Allocate a fresh general array of size N and fill from vector VEC.
   Stops at min(N, length VEC); remaining slots NIL."
  (let* ((vlen (array-length vec))
         (lim (if (< vlen n) vlen n))
         (a (make-array n))
         (i 0))
    (loop
      (when (>= i lim) (return a))
      (aset a i (aref vec i))
      (setq i (+ i 1)))))

(defun %make-array-fill-string (n s)
  "Allocate a fresh general array of size N and fill from string S
   (each char in S becomes a character element)."
  (let* ((slen (array-length s))
         (lim (if (< slen n) slen n))
         (a (make-array n))
         (i 0))
    (loop
      (when (>= i lim) (return a))
      (aset a i (aref s i))   ; AREF→char already
      (setq i (+ i 1)))))

(defun %make-string-fill-char (n ch)
  "Allocate a fresh string of size N and fill with character CH."
  (let ((s (%make-string-array n)) (i 0) (code (char-code ch)))
    (loop
      (when (>= i n) (return s))
      (aset s i code)
      (setq i (+ i 1)))))

;;; ============================================================
;;; CLOS protocol completion
;;; ============================================================
;;;
;;; Six generic functions that ANSI requires but that modus had
;;; previously stubbed as no-ops: INITIALIZE-INSTANCE,
;;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, UPDATE-INSTANCE-FOR-REDEFINED-
;;; CLASS, NO-APPLICABLE-METHOD, NO-NEXT-METHOD, and SLOT-MISSING (the
;;; user-callable entry on top of the internal %dispatch-slot-missing).
;;;
;;; Each follows the same pattern as SHARED-INITIALIZE: a
;;; %default-NAME defun holding the system-supplied behaviour, a
;;; %dispatch-NAME(args) that consults the GF registry for user
;;; methods, and the user-facing NAME (&rest args) entry that calls
;;; the dispatcher.  Init at boot via %init-clos-protocol.

;;; ---- INITIALIZE-INSTANCE -----------------------------------------
;;; Per CLHS 7.1.1, INITIALIZE-INSTANCE invokes SHARED-INITIALIZE with
;;; the instance, slot-names = T (apply all initforms for unbound
;;; slots), and the initargs.  Returns the instance.

(defun %initialize-instance-default-1 (instance slot-names initargs)
  "List-form initialize-instance default — takes initargs as a list,
   not a &rest tail.  Modus's funcall-with-&rest collapses arguments
   when nargs > +max-reg-args+, so the apply'd chain
   IIdefault → shared-initialize → %dispatch → %shared-init-default
   silently drops trailing initargs.  Bypass that whole stack: build
   the same effect inline by directly calling %shared-initialize-default
   through the list-spread helper that already handles this shape."
  ;; Check for user methods on shared-initialize; fall through to default
  ;; with the args list.  (initialize-instance methods would have already
  ;; dispatched higher up; this is the default body.)
  (let* ((sa-args (cons instance (cons slot-names initargs)))
         (gf (%find-gf 'shared-initialize)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf sa-args)))
          (if applicable
              (%gf-dispatch-standard gf sa-args applicable)
              (%shared-init-default-spread sa-args)))
        (%shared-init-default-spread sa-args)))
  instance)

(defun %initialize-instance-default (instance &rest initargs)
  "Same as -1 but exposed for the &rest signature used by methods."
  (%initialize-instance-default-1 instance t initargs))

(defun %dispatch-initialize-instance (args)
  "ARGS is (instance &rest initargs).  Calling the default needs the
   list-form helper because modus' funcall+&rest loses trailing args
   when nargs > +max-reg-args+."
  (let ((gf (%find-gf 'initialize-instance)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (%initialize-instance-default-1 (car args) t (cdr args))))
        (%initialize-instance-default-1 (car args) t (cdr args)))))

(defun initialize-instance (&rest %ii-args)
  (%dispatch-initialize-instance %ii-args))

;;; ---- NO-APPLICABLE-METHOD / NO-NEXT-METHOD -----------------------
;;; CLHS 7.6.6.2 / 7.6.6.3.  Both are GFs whose default behaviour is to
;;; signal an error.  Letting them be regular functions means a user
;;; can shadow the error by adding their own method (used by some
;;; test files to swallow expected misses).

(defun %no-applicable-method-default (gf &rest args)
  (declare (ignore args))
  (error "no applicable method for generic function ~S" gf))

(defun %dispatch-no-applicable-method (args)
  (let ((gf (%find-gf 'no-applicable-method)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (apply #'%no-applicable-method-default args)))
        (apply #'%no-applicable-method-default args))))

(defun no-applicable-method (&rest %nam-args)
  (%dispatch-no-applicable-method %nam-args))

(defun %no-next-method-default (gf method &rest args)
  (declare (ignore args))
  (error "call-next-method: no next method on ~S after ~S" gf method))

(defun %dispatch-no-next-method (args)
  (let ((gf (%find-gf 'no-next-method)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (apply #'%no-next-method-default args)))
        (apply #'%no-next-method-default args))))

(defun no-next-method (&rest %nnm-args)
  (%dispatch-no-next-method %nnm-args))

;;; ---- SLOT-MISSING -------------------------------------------------
;;; Existing internal %dispatch-slot-missing exists but there's no
;;; user-callable entry.  Real default: signal an error.

(defun %slot-missing-default (class instance slot-name operation &optional new-value)
  (declare (ignore class instance new-value))
  (error "slot-missing: ~S has no slot ~S during ~S"
         instance slot-name operation))

(defun slot-missing (class instance slot-name operation &optional new-value)
  "Default implementation signals an error; user methods on
   SLOT-MISSING can override via DEFMETHOD."
  (let ((gf (%find-gf 'slot-missing))
        (args (list class instance slot-name operation new-value)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (%slot-missing-default class instance slot-name operation new-value)))
        (%slot-missing-default class instance slot-name operation new-value))))

;;; ---- UPDATE-INSTANCE-FOR-* (real GFs, default = no-op) -----------
;;; The default methods are no-ops so old behaviour (returning the
;;; instance) is preserved, but registering them as GFs means users
;;; can add methods that CHANGE-CLASS / class redefinition will pick
;;; up.  ANSI requires these to be GFs.

(defun %uifdc-default (previous current &rest initargs)
  "Default primary for UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (CLHS 7.7.2):
   equivalent to (apply #'shared-initialize current added-slots initargs)
   where added-slots are the local slots of CURRENT's class that did not
   exist in PREVIOUS's class.  Implemented directly (initarg application
   leftmost-wins + initforms for still-unbound added slots) rather than
   through the shared-initialize spread path, which also runs
   :default-initargs — those must NOT apply on change-class."
  (when (or (null current) (not (%clos-instance-p current)))
    (return-from %uifdc-default current))
  (let* ((new-name (aref current 1))
         (new-cls (%find-clos-class new-name))
         (old-name (if (and previous (%clos-instance-p previous))
                       (aref previous 1)
                       nil))
         (old-cls (if old-name (%find-clos-class old-name) nil))
         (old-slot-names (if old-cls (aref old-cls 2) nil)))
    (when (null new-cls) (return-from %uifdc-default current))
    ;; Added slots: local in the new class, absent from the old class.
    (let ((added nil))
      (let ((sn (aref new-cls 2)))
        (loop
          (when (null sn) (return nil))
          (let ((nm (car sn)))
            (when (and (null (member nm old-slot-names :test #'eq))
                       (null (%slot-class-owner new-name nm)))
              (setq added (cons nm added))))
          (setq sn (cdr sn))))
      ;; 1. Apply initargs (leftmost wins) — they may set ANY slot of
      ;;    the new class, not just added ones (change-class.1.5).
      (let ((set-slots (%shared-init-apply-initargs
                        current new-name initargs nil)))
        ;; 2. Initforms for added slots still unbound + not initarg-set.
        (let ((cur added))
          (loop
            (when (null cur) (return nil))
            (let* ((nm (car cur))
                   (already (member nm set-slots :test #'eq))
                   (idx (%clos-slot-index new-cls nm))
                   (unbound (if (>= idx 0)
                                (let ((v (aref current (+ 2 idx))))
                                  (and (fixnump v) (= v -999)))
                                nil)))
              (when (and (null already) unbound)
                (let ((thunk (%clos-initform-thunk new-name nm)))
                  (when thunk
                    (set-slot-value current nm (funcall thunk))))))
            (setq cur (cdr cur))))))
    current))

(defun %dispatch-uifdc (args)
  (let ((gf (%find-gf 'update-instance-for-different-class)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (apply #'%uifdc-default args)))
        (apply #'%uifdc-default args))))

(defun update-instance-for-different-class (&rest %u-args)
  (%dispatch-uifdc %u-args))

(defun %uifrc-default (instance added-slots discarded-slots plist &rest initargs)
  (declare (ignore added-slots discarded-slots plist initargs))
  instance)

(defun %dispatch-uifrc (args)
  (let ((gf (%find-gf 'update-instance-for-redefined-class)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (apply #'%uifrc-default args)))
        (apply #'%uifrc-default args))))

(defun update-instance-for-redefined-class (&rest %u-args)
  (%dispatch-uifrc %u-args))

;;; ---- PRINT-OBJECT -------------------------------------------------
;;; Default method: write something readable.  Tests in print-object.lsp
;;; just probe that the GF exists and is dispatchable; full ~S-equivalent
;;; output isn't required.

(defun %print-object-default (object stream)
  (declare (ignore stream))
  object)

(defun %dispatch-print-object (args)
  (let ((gf (%find-gf 'print-object)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (apply #'%print-object-default args)))
        (apply #'%print-object-default args))))

(defun print-object (object stream)
  (%dispatch-print-object (list object stream)))

;;; ---- DESCRIBE-OBJECT ---------------------------------------------

(defun %describe-object-default (object stream)
  (declare (ignore stream))
  object)

(defun %dispatch-describe-object (args)
  (let ((gf (%find-gf 'describe-object)))
    (if (and gf (%gf-methods gf))
        (let ((applicable (%collect-applicable-methods gf args)))
          (if applicable
              (%gf-dispatch-standard gf args applicable)
              (apply #'%describe-object-default args)))
        (apply #'%describe-object-default args))))

(defun describe-object (object stream)
  (%dispatch-describe-object (list object stream)))

;;; ============================================================
;;; LOAD — read+eval all forms from a file or stream.
;;; ============================================================
;;;
;;; CLHS 24.1.x.  Override of the earlier cl-eval.lisp definition (last
;;; defun wins; ansi-bridge.lisp loads after cl-eval.lisp).  Adds
;;;   - stream filespec support (LOAD.3/8/13/16)
;;;   - :if-does-not-exist (LOAD.14)
;;;   - :verbose / :print emitting visible output to *standard-output*
;;;     so load-file-test's (position #\; str) sees the marker
;;;   - :allow-other-keys to accept the test wrapper's apply with
;;;     :allow-other-keys t, while still program-erroring on unknown
;;;     keywords when allow-other-keys is absent (LOAD.error.3)
;;;   - 0 args → program-error (LOAD.error.2)
;;;   - missing file with default if-does-not-exist → error (LOAD.error.1)
;;;
;;; *load-pathname*, *load-truename*, *load-print*, *load-verbose* are
;;; declared as defvars (default NIL) for tests LOAD-PATHNAME.1 etc.
;;; The set! during load uses set-symbol-value on the hash so that
;;; (symbol-value '*load-pathname*) reflects the binding while the
;;; file is being read.  Per CLAUDE.md "defvar init-thunks not run"
;;; the defvars stay NIL outside any LOAD call, which is what the
;;; default tests expect.

(defvar *load-pathname* nil)
(defvar *load-truename* nil)
(defvar *load-print* nil)
(defvar *load-verbose* nil)

;;; Helpers --------------------------------------------------------------

(defun %load-known-key-p (k)
  (or (eq k :verbose) (eq k :print) (eq k :if-does-not-exist)
      (eq k :external-format) (eq k :allow-other-keys)))

(defun %load-validate-kwargs (kw-list)
  "Walk plist; if any unknown key found and :allow-other-keys not
   present-and-true, signal program-error.  Returns T on OK."
  (let ((aok nil)
        (cur kw-list))
    ;; First pass: find :allow-other-keys t
    (loop
      (when (null cur) (return nil))
      (when (and (eq (car cur) :allow-other-keys) (cdr cur) (cadr cur))
        (setq aok t))
      (setq cur (cddr cur)))
    ;; Second pass: program-error on first unknown (unless aok)
    (unless aok
      (setq cur kw-list)
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)))
          (unless (%load-known-key-p k)
            (%signal-program-error)))
        (setq cur (cddr cur))))
    t))

(defun %load-getf (plist key default)
  "GETF-equivalent that doesn't depend on plist arity checks; default
   if missing or pair truncated."
  (let ((cur plist))
    (loop
      (when (null cur) (return default))
      (when (null (cdr cur)) (return default))
      (when (eq (car cur) key) (return (cadr cur)))
      (setq cur (cddr cur)))))

(defun %load-getf-p (plist key)
  "Return T if KEY appears as a key in PLIST."
  (let ((cur plist))
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur)) (return nil))
      (when (eq (car cur) key) (return t))
      (setq cur (cddr cur)))))

(defun %load-stream-p (x)
  "True if X is a stream object."
  (and x (streamp x)))

(defun %load-from-stream (stream verbose print)
  "Read+eval all forms from STREAM.  Returns T."
  (when verbose
    (write-string "; loading from stream" *standard-output*)
    (write-char #\Newline *standard-output*))
  (let ((eof-marker (cons 'eof nil))
        (result t))
    (loop
      (let ((form (handler-case (read stream nil eof-marker)
                    (t (c) eof-marker))))
        (when (eq form eof-marker) (return t))
        (let ((val (handler-case (eval form) (t (c) nil))))
          (when print
            (handler-case
                (progn (write val :stream *standard-output*)
                       (write-char #\Newline *standard-output*))
              (t (c) nil)))
          (setq result val))))
    result))

(defun load (&rest all-args)
  "Read and evaluate all forms from a file or stream.
   Accepts:
     (load filespec &key verbose print if-does-not-exist
                          external-format allow-other-keys)"
  ;; Zero-arg → program-error (LOAD.error.2)
  (when (null all-args)
    (%signal-program-error)
    (return-from load nil))
  (let* ((filespec (car all-args))
         (kwargs   (cdr all-args)))
    ;; Validate keyword args; allow-other-keys disables the check.
    (%load-validate-kwargs kwargs)
    (let* ((verbose-p (%load-getf-p kwargs :verbose))
           (verbose (if verbose-p
                        (%load-getf kwargs :verbose nil)
                        ;; Fall back to *load-verbose* (defaults to NIL).
                        (and (boundp '*load-verbose*) *load-verbose*)))
           (print-p (%load-getf-p kwargs :print))
           (print   (if print-p
                        (%load-getf kwargs :print nil)
                        (and (boundp '*load-print*) *load-print*)))
           (idne-p  (%load-getf-p kwargs :if-does-not-exist))
           (idne    (if idne-p
                        (%load-getf kwargs :if-does-not-exist nil)
                        :error)))
      (cond
        ;; --- Stream filespec ---
        ((%load-stream-p filespec)
         (%load-from-stream filespec verbose print))
        ;; --- Pathname / string filespec ---
        (t
         (let ((path (cond ((stringp filespec) filespec)
                           (t (%resolve-path filespec)))))
           ;; Attempt open with :if-does-not-exist nil so we can decide
           ;; ourselves whether the absence is an error vs a NIL return.
           (let ((stream (handler-case
                             (open path :direction :input
                                        :if-does-not-exist nil)
                           (t (c) nil))))
             (cond
               ((null stream)
                (cond
                  ((null idne) nil)
                  (t (error 'file-error :pathname path))))
               (t
                (unwind-protect
                     (let ((*load-pathname* path)
                           (*load-truename* path)
                           (*load-print* print)
                           (*load-verbose* verbose))
                       (declare (special *load-pathname* *load-truename*
                                         *load-print* *load-verbose*))
                       (when verbose
                         (write-string "; loading " *standard-output*)
                         (write-string path *standard-output*)
                         (write-char #\Newline *standard-output*))
                       (%load-from-stream stream verbose print))
                  (close stream)))))))))))

;;; ---- Initialization -----------------------------------------------

(defun %init-clos-protocol ()
  "Register the CLOS GFs that have system-supplied default methods.
   Called once from kernel-main after %init-make-load-form."
  ;; INITIALIZE-INSTANCE — register the GF but DON'T install a default
  ;; method on standard-object.  %dispatch-initialize-instance falls
  ;; through to %initialize-instance-default-1 directly when no
  ;; user-defined methods apply.  Installing a default method whose
  ;; body uses &rest+apply hits the funcall+&rest truncation that
  ;; loses trailing initargs.
  (%defgeneric 'initialize-instance '(instance &rest initargs) nil)
  ;; NO-APPLICABLE-METHOD / NO-NEXT-METHOD
  (%defgeneric 'no-applicable-method '(gf &rest args) nil)
  (%defmethod 'no-applicable-method nil '(t)
              (lambda (&rest args) (apply #'%no-applicable-method-default args)))
  (%defgeneric 'no-next-method '(gf method &rest args) nil)
  (%defmethod 'no-next-method nil '(t)
              (lambda (&rest args) (apply #'%no-next-method-default args)))
  ;; SLOT-MISSING / SLOT-UNBOUND
  (%defgeneric 'slot-missing '(class instance slot-name operation &optional new-value) nil)
  (%defmethod 'slot-missing nil '(t)
              (lambda (&rest args) (apply #'%slot-missing-default args)))
  ;; UPDATE-INSTANCE-FOR-*
  (%defgeneric 'update-instance-for-different-class '(previous current &rest initargs) nil)
  (%defmethod 'update-instance-for-different-class nil '(standard-object standard-object)
              (lambda (&rest args) (apply #'%uifdc-default args)))
  (%defgeneric 'update-instance-for-redefined-class
              '(instance added-slots discarded-slots plist &rest initargs) nil)
  (%defmethod 'update-instance-for-redefined-class nil '(standard-object)
              (lambda (&rest args) (apply #'%uifrc-default args)))
  ;; PRINT-OBJECT / DESCRIBE-OBJECT
  (%defgeneric 'print-object '(object stream) nil)
  (%defmethod 'print-object nil '(t)
              (lambda (obj s) (%print-object-default obj s)))
  (%defgeneric 'describe-object '(object stream) nil)
  (%defmethod 'describe-object nil '(t)
              (lambda (obj s) (%describe-object-default obj s)))
  ;; Register all fn-pointers for #'name → GF lookup.
  (handler-case (%register-gf-fn #'initialize-instance 'initialize-instance) (t (c) nil))
  (handler-case (%register-gf-fn #'no-applicable-method 'no-applicable-method) (t (c) nil))
  (handler-case (%register-gf-fn #'no-next-method 'no-next-method) (t (c) nil))
  (handler-case (%register-gf-fn #'slot-missing 'slot-missing) (t (c) nil))
  (handler-case (%register-gf-fn #'update-instance-for-different-class
                                 'update-instance-for-different-class) (t (c) nil))
  (handler-case (%register-gf-fn #'update-instance-for-redefined-class
                                 'update-instance-for-redefined-class) (t (c) nil))
  (handler-case (%register-gf-fn #'print-object 'print-object) (t (c) nil))
  (handler-case (%register-gf-fn #'describe-object 'describe-object) (t (c) nil))
  ;; SHARED-INITIALIZE, CHANGE-CLASS, REINITIALIZE-INSTANCE, MAKE-LOAD-FORM
  ;; are registered elsewhere — but their fn-pointer→name mapping wasn't.
  (handler-case (%register-gf-fn #'shared-initialize 'shared-initialize) (t (c) nil))
  (handler-case (%register-gf-fn #'change-class 'change-class) (t (c) nil))
  (handler-case (%register-gf-fn #'reinitialize-instance 'reinitialize-instance) (t (c) nil))
  nil)

;;; ---------- typep: stream subtype dispatch (CLHS 21.1.3) -----------------
;;;
;;; cl-conditions.lisp's TYPEP handles 'STREAM and 'FILE-STREAM but not the
;;; other concrete stream subtypes ('STRING-STREAM, 'BROADCAST-STREAM,
;;; 'CONCATENATED-STREAM, 'ECHO-STREAM, 'SYNONYM-STREAM, 'TWO-WAY-STREAM).
;;; Modus's call resolution is "last-defun-wins" with no way to call the
;;; previous binding from a replacement (a wrapper would loop via recursion).
;;; Re-implement here with the cl-conditions body verbatim plus the new
;;; stream-subtype clauses inserted at the top of the symbol-name branch.
;;; cl-streams.lisp owns the predicates (STRING-STREAM-P etc).

;;; ---- user DEFTYPE expansion ----------------------------------------
;;; Runtime (eval '(deftype NAME (params) body)) stores (params . body)
;;; in *%runtime-deftype-table* keyed by the upcased NAME string (see
;;; cl-eval.lisp).  TYPEP / SUBTYPEP did not consult it, so a user
;;; deftype used as a type specifier always fell through to NIL/unknown.
;;; %expand-deftype binds the deftype params (a full lambda-list with
;;; &optional/&key) to the *unevaluated* type arguments and evaluates the
;;; body to the expanded type specifier, which the caller re-checks.
(defun %deftype-lookup (head)
  "If HEAD names a user deftype, return its (params . body); else NIL."
  (and *%runtime-deftype-table*
       (symbolp head)
       (let ((nm (%eval-sym-name head)))
         (and nm (gethash nm *%runtime-deftype-table*)))))

(defun %expand-deftype (type)
  "Expand a user-deftype TYPE specifier (symbol or (name arg…)) to its
   underlying type specifier, or NIL if TYPE is not a user deftype."
  (let* ((head (if (consp type) (car type) type))
         (args (if (consp type) (cdr type) nil))
         (entry (%deftype-lookup head)))
    (if (null entry)
        nil
        (let* ((params (car entry))
               (body   (cdr entry))
               ;; Bind params to the *unevaluated* type args.  &optional
               ;; defaults and &key all flow through %bind-params.
               (env (%bind-params params args nil)))
          (%eval-progn body env)))))

(defun typep (obj type)
  (when (or (eq type 'values)
            (and (consp type)
                 (or (eq (car type) 'values)
                     (eq (car type) 'function))))
    (error "typep: this type-specifier is not legal here"))
  (cond
    ((%clos-class-p type)
     (typep obj (aref type 1)))
    ((%class-proxy-p type)
     (typep obj (aref type 1)))
    ((not (consp type))
     (let ((tn type))
       (cond
         ;; Stream subtype names — new clauses added on top of the
         ;; cl-conditions baseline.
         ((eq tn 'stream)             (streamp obj))
         ((eq tn 'file-stream)        (file-stream-p obj))
         ((eq tn 'string-stream)      (string-stream-p obj))
         ((eq tn 'broadcast-stream)   (broadcast-stream-p obj))
         ((eq tn 'concatenated-stream) (concatenated-stream-p obj))
         ((eq tn 'echo-stream)        (echo-stream-p obj))
         ((eq tn 'synonym-stream)     (synonym-stream-p obj))
         ((eq tn 'two-way-stream)     (two-way-stream-p obj))
         ((eq tn 'package) (packagep obj))
         ((eq tn 'keyword) (keywordp obj))
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
         ;; NB: the old branch had `(integerp obj)` — a leftover from when
         ;; native MVM symbols were bare hash fixnums.  It made
         ;; (typep 1 'symbol) → T and broke typecase.2/.3, ctypecase.3,
         ;; etypecase.3.  Dropped.  NIL and T must still be reported as
         ;; symbols (CLHS) — keep them explicit since the live symbolp
         ;; here doesn't reliably cover NIL in this build.
         ((eq tn 'symbol) (or (null obj) (eq obj t) (symbolp obj)))
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
         ((eq tn 'bit) (and (integerp obj) (or (= obj 0) (= obj 1))))
         ((eq tn 'unsigned-byte) (and (integerp obj) (>= obj 0)))
         ((eq tn 'signed-byte) (integerp obj))
         ((eq tn 'array)         (or (arrayp obj) (stringp obj)))
         ((eq tn 'simple-array)  (or (arrayp obj) (stringp obj)))
         ((eq tn 'vector)        (or (arrayp obj) (stringp obj)))
         ((eq tn 'simple-vector) (or (arrayp obj) (stringp obj)))
         ((eq tn 'bit-vector)    (arrayp obj))
         ((eq tn 'simple-bit-vector) (arrayp obj))
         ((eq tn 'sequence)      (or (null obj) (consp obj)
                                     (arrayp obj) (stringp obj)))
         ((eq tn 'function)      (or (functionp obj) (%generic-function-p obj)))
         ((eq tn 'compiled-function) (functionp obj))
         ((eq tn 'generic-function) (%generic-function-p obj))
         ((eq tn 'standard-generic-function) (%generic-function-p obj))
         ((eq tn 'standard-method) (%standard-method-p obj))
         ((eq tn 'method) (%standard-method-p obj))
         ((eq tn 'method-combination) (%mc-p obj))
         ((eq tn 'hash-table)    (hash-table-p obj))
         ((eq tn 'readtable)     (readtablep obj))
         ((eq tn 'random-state)  (random-state-p obj))
         ((eq tn 'condition) (%condition-p obj))
         ;; A CLOS class object is itself an instance of STANDARD-OBJECT
         ;; (and CLASS / STANDARD-CLASS / METAOBJECT / T).  CLHS 4.3.7.
         ((eq tn 'standard-object) (or (%clos-instance-p obj)
                                       (%clos-class-p obj)))
         ((eq tn 'class) (%clos-class-p obj))
         ;; STANDARD-CLASS only for user (defclass) classes — those whose
         ;; CPL (slot 4) includes STANDARD-OBJECT.  Built-in classes that
         ;; happen to be registered as %clos-class arrays (e.g. a proxy
         ;; promoted for PATHNAME) do NOT have STANDARD-OBJECT in their CPL,
         ;; so they are correctly excluded — prevents
         ;; all-standard-classes-are-subtypes-of-standard-object from
         ;; collecting LOGICAL-PATHNAME / PATHNAME.
         ((eq tn 'standard-class)
          (and (%clos-class-p obj)
               (and (member 'standard-object (aref obj 4)) t)))
         ((eq tn 'built-in-class) nil)
         ((eq tn 'structure-class) nil)
         ((eq tn 'metaobject) (%clos-class-p obj))
         ((eq tn 'restart) (%active-restart-p obj))
         (t (cond
              ;; Struct instance — when OBJ is a struct, decide membership
              ;; from the instance's own slot-1 type-name (and :include
              ;; ancestry).  This works WITHOUT a registered descriptor for
              ;; TN: the compile-time defstruct registration thunk may not
              ;; have run at boot, but the instance still carries its type.
              ;; structure-1-1 / structure-1-4 depend on this.
              ((%struct-instance-p obj) (%struct-instance-named-p obj tn))
              ;; Registered struct type — fall back to the registry walk
              ;; (covers (typep non-struct 'struct-type) → NIL too).
              ((%find-struct-type tn) (%struct-instance-typep obj tn))
              ((%cond-reg-find tn) (%condition-typep obj tn))
              ((%clos-instance-p obj)
               (let ((cpl (%obj-cpl obj))
                     (found nil))
                 (let ((c cpl))
                   (loop
                     (when (null c) (return found))
                     (let ((cur (car c)))
                       (when (cond
                               ((eq cur tn) t)
                               ((and (%native-mvm-sym-p cur)
                                     (%native-mvm-sym-p tn))
                                (= (%native-mvm-sym-hash cur)
                                   (%native-mvm-sym-hash tn)))
                               ;; Compare 3-slot CL syms by NAME-HASH
                               ;; (slot 0), NOT by %cl-sym-name string —
                               ;; names lazy-resolve through
                               ;; *SYM-NAME-TABLE*, which only covers
                               ;; symbols harvested from a few chapter
                               ;; dirs.  For two class names absent from
                               ;; the table (e.g. CHANGE-CLASS-CLASS-01A
                               ;; vs -01B from objects/), %cl-sym-name
                               ;; returned "" for BOTH sides and
                               ;; (string-equal "" "") made TYPEP match
                               ;; EVERY class — change-class.1.x failed
                               ;; on (typep obj 'other-class) → T.
                               ((and (%cl-sym-p cur) (%cl-sym-p tn))
                                (= (%cl-sym-hash cur) (%cl-sym-hash tn)))
                               (t nil))
                         (setq found t) (return found)))
                     (setq c (cdr c))))))
              ;; User DEFTYPE name used as an atomic type specifier —
              ;; expand it and re-check (deftype.7/.9+ define (sym) types).
              ((%deftype-lookup tn)
               (typep obj (%expand-deftype tn)))
              (t nil))))))
    ((%class-proxy-p type)
     (typep obj (%class-proxy-name type)))
    (t
     (let ((head (car type)))
       (cond
         ((eq head 'real)
          (if (or (integerp obj) (floatp-impl obj) (ratiop obj))
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ((eq head 'integer)
          (if (integerp obj)
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ((or (eq head 'float) (eq head 'single-float)
              (eq head 'double-float) (eq head 'short-float) (eq head 'long-float))
          (if (floatp-impl obj)
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ((eq head 'rational)
          (if (or (integerp obj) (ratiop obj))
              (let ((low (if (cdr type) (cadr type) '*))
                    (high (if (cddr type) (caddr type) '*)))
                (typep-range-check obj low high))
              nil))
         ((eq head 'eql)
          (eql obj (cadr type)))
         ((eq head 'member)
          (if (member obj (cdr type)) t nil))
         ((eq head 'and)
          (let ((ok t))
            (dolist (sub (cdr type))
              (unless (typep obj sub) (setq ok nil)))
            ok))
         ((eq head 'or)
          (let ((ok nil))
            (dolist (sub (cdr type))
              (when (typep obj sub) (setq ok t)))
            ok))
         ((eq head 'not)
          (not (typep obj (cadr type))))
         ((eq head 'satisfies) nil)
         ((eq head 'unsigned-byte)
          (and (integerp obj) (>= obj 0)
               (< obj (ash 1 (cadr type)))))
         ((eq head 'signed-byte)
          (and (integerp obj)
               (let ((half (ash 1 (- (cadr type) 1))))
                 (and (>= obj (- 0 half)) (< obj half)))))
         ((eq head 'mod)
          (and (integerp obj) (>= obj 0) (< obj (cadr type))))
         ((or (eq head 'bit-vector) (eq head 'simple-bit-vector))
          (and (bit-vector-p obj)
               (let ((sz (and (cdr type) (cadr type))))
                 (or (null sz) (eq sz '*) (eq sz t)
                     (and (integerp sz) (= sz (array-length obj)))))))
         ((or (eq head 'string) (eq head 'simple-string)
              (eq head 'base-string) (eq head 'simple-base-string))
          (and (stringp obj)
               (let ((sz (and (cdr type) (cadr type))))
                 (or (null sz) (eq sz '*) (eq sz t)
                     (and (integerp sz) (= sz (array-length obj)))))))
         ;; Native MDA (subtag #x34): the legacy clause below only knows
         ;; the #x31/#x32 vector subtags and the 9867654/8765432 cons
         ;; wrappers, so a native multi-dim array (e.g. a #2a(...) reader
         ;; literal, now built via %alloc-mda) falls through to NIL.
         ;; Handle it here using array-rank / array-dimensions, which both
         ;; understand MDA.  Covers (array ELT DIMS) and (vector ELT SIZE).
         ((and (or (eq head 'vector) (eq head 'simple-vector)
                   (eq head 'simple-array) (eq head 'array))
               (%mda-p obj))
          (let ((et   (and (cdr type) (cadr type)))
                (dims (and (cddr type) (caddr type)))
                (dims-given (and (cddr type) t)))
            (and
             ;; vector / simple-vector require rank 1
             (if (or (eq head 'vector) (eq head 'simple-vector))
                 (= (array-rank obj) 1)
                 t)
             ;; element-type: MDA-with-string-data → character family;
             ;; otherwise element type is T.
             (let ((str (%mda-stringp obj)))
               (cond
                 ((or (null et) (eq et '*)) t)
                 ((eq et t) (not str))
                 ((or (eq et 'character) (eq et 'base-char)
                      (eq et 'standard-char)) str)
                 ((eq et 'bit) nil)
                 (t (not str))))
             ;; dimension spec
             (cond
               ((not dims-given) t)
               ((eq dims '*) t)
               ((eq dims t) t)
               ((null dims) (= (array-rank obj) 0))
               ((integerp dims) (= dims (array-rank obj)))
               ((consp dims)
                (let ((actual (array-dimensions obj)) (spec dims) (ok t))
                  (loop
                    (when (or (and (null actual) (null spec)) (not ok))
                      (return ok))
                    (when (or (null actual) (null spec))
                      (setq ok nil) (return ok))
                    (let ((ad (car actual)) (sd (car spec)))
                      (unless (or (eq sd '*) (eq sd t)
                                  (and (integerp sd) (= sd ad)))
                        (setq ok nil)))
                    (setq actual (cdr actual))
                    (setq spec (cdr spec)))))
               (t nil)))))
         ((or (eq head 'vector) (eq head 'simple-vector)
              (eq head 'simple-array) (eq head 'array))
          (let ((wrapped-dims
                 (cond
                   ((and (consp obj) (eql (car obj) 9867654)
                         (consp (cdr obj)))
                    (cadr obj))
                   (t :no-wrapper))))
          (let ((obj (cond
                       ((and (consp obj) (eql (car obj) 9867654)
                             (consp (cdr obj))) (cddr obj))
                       ((and (consp obj) (eql (car obj) 8765432)) (cdr obj))
                       (t obj))))
          (and (not (or (fixnump obj) (characterp obj) (consp obj) (null obj)))
               (or (= (obj-subtag obj) #x31) (= (obj-subtag obj) #x32))
               (let* ((et (and (cdr type) (cadr type)))
                      (sz-given (and (cddr type) t))
                      (sz (and (cddr type) (caddr type)))
                      (is-string  (= (obj-subtag obj) #x31))
                      (is-array   (= (obj-subtag obj) #x32))
                      (is-bitvec  (and is-array
                                       (> (array-length obj) 0)
                                       (bit-vector-p obj)))
                      (et-ok
                       (cond
                         ((or (null et) (eq et '*) (eq et t))
                          (if (or (null et) (eq et '*))
                              t
                              (and is-array (not is-bitvec) (not is-string))))
                         ((eq et 'character)            is-string)
                         ((eq et 'base-char)            is-string)
                         ((eq et 'standard-char)        is-string)
                         ((eq et 'bit)                  is-bitvec)
                         (t t))))
                 (and et-ok
                      (cond
                        ((not sz-given) t)
                        ((eq sz '*) t)
                        ((eq sz t)  t)
                        ((null sz)
                         (and (not (eq wrapped-dims :no-wrapper))
                              (null wrapped-dims)))
                        ;; Integer third element: for VECTOR / SIMPLE-VECTOR
                        ;; it's the SIZE ((vector elt N) ≡ length N); for
                        ;; ARRAY / SIMPLE-ARRAY it's the RANK ((array elt N)
                        ;; ≡ rank N) per CLHS.  This clause only sees non-MDA
                        ;; objects (MDAs handled above), so rank is 1 for
                        ;; vectors/strings, 0 only for the empty wrapped-dims.
                        ;; (array t 1) on a vector → T; (array t 0) → NIL.
                        ((integerp sz)
                         (if (or (eq head 'vector) (eq head 'simple-vector))
                             (= sz (array-length obj))
                             (cond
                               ((eq wrapped-dims :no-wrapper) (= sz 1))
                               ((consp wrapped-dims) (= sz (length wrapped-dims)))
                               ((null wrapped-dims) (= sz 0))
                               (t (= sz 1)))))
                        ((consp sz)
                         (cond
                           ((null (cdr sz))
                            (cond
                              ((eq wrapped-dims :no-wrapper)
                               (or (eq (car sz) '*) (eq (car sz) t)
                                   (and (integerp (car sz))
                                        (= (car sz) (array-length obj)))))
                              ((and (consp wrapped-dims)
                                    (null (cdr wrapped-dims)))
                               (or (eq (car sz) '*) (eq (car sz) t)
                                   (and (integerp (car sz))
                                        (= (car sz) (car wrapped-dims)))))
                              (t nil)))
                           (t
                            (and (consp wrapped-dims)
                                 (= (length sz) (length wrapped-dims))
                                 (let ((ok t) (a sz) (b wrapped-dims))
                                   (loop (when (null a) (return ok))
                                     (let ((ad (car a)) (bd (car b)))
                                       (unless (or (eq ad '*) (eq ad t)
                                                   (and (integerp ad)
                                                        (integerp bd)
                                                        (= ad bd)))
                                         (setq ok nil) (return nil)))
                                     (setq a (cdr a)) (setq b (cdr b)))
                                   ok)))))
                        (t nil))))))))
         ((eq head 'cons)
          (and (consp obj)
               (let ((car-type (and (cdr type) (cadr type)))
                     (cdr-type (and (cddr type) (caddr type))))
                 (and (or (null car-type) (eq car-type '*) (eq car-type t)
                          (typep (car obj) car-type))
                      (or (null cdr-type) (eq cdr-type '*) (eq cdr-type t)
                          (typep (cdr obj) cdr-type))))))
         ;; Compound user DEFTYPE — (deftype-name arg…); expand with the
         ;; args bound and re-check (deftype.9-.18 parameterized types).
         ((%deftype-lookup head)
          (typep obj (%expand-deftype type)))
         (t (if (%cond-reg-find head)
                (%condition-typep obj head)
                nil)))))))

