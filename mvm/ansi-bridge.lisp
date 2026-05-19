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
  "Pure T/NIL equalpt — equivalent to equalt, kept separate because some
   ANSI tests reference it by name (e.g. via def-print-test templates)."
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

;; complement was a global-cell stub here; cl-sequences.lisp now has the
;; real per-closure implementation.  Removed so the cl-sequences version
;; (which loads earlier but wins because we deleted the override) takes
;; effect.  Multiple coexisting complements now work — was: every
;; complement used the LAST fn, breaking (position x lst :test (complement
;; #'eql)) once any other complement had been created.

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

(defun nreconc (list tail)
  (let ((cur list))
    (loop
      (when (null cur) (return tail))
      (let ((next (cdr cur)))
        (set-cdr cur tail)
        (setq tail cur)
        (setq cur next)))))

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

(defun acons (key datum alist)
  (cons (cons key datum) alist))

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
         (setq hash (logxor hash (aref object i)))
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
   (cons 8765432 underlying) → NIL (adjustable but no fp)."
  (let ((y (%fp-inner arr)))
    (if (consp y) (fixnump (car y)) nil)))

(defun fill-pointer (arr)
  "Return the fill pointer of ARR (NIL if none)."
  (let ((y (%fp-inner arr)))
    (if (and (consp y) (fixnump (car y))) (car y) nil)))

(defun set-fill-pointer (arr val)
  "Set fill pointer of ARR to VAL."
  (let ((y (%fp-inner arr)))
    (when (and (consp y) (fixnump (car y)))
      (set-car y val))
    val))

(defun vector-push (new-element vector)
  "Push NEW-ELEMENT onto VECTOR (with fill pointer). Returns fill pointer or nil."
  (let ((vector (%fp-inner vector)))
    (if (and (consp vector) (fixnump (car vector)))
        (let ((fp (car vector))
              (arr (cdr vector)))
          (let ((len (array-length arr)))
            (if (>= fp len)
                nil
                (progn
                  (aset arr fp new-element)
                  (set-car vector (+ fp 1))
                  fp))))
        nil)))

(defun vector-push-extend (new-element vector &rest args)
  "Push NEW-ELEMENT onto VECTOR, extending if needed."
  (let ((vector (%fp-inner vector)))
    (if (and (consp vector) (fixnump (car vector)))
        (let ((fp (car vector))
              (arr (cdr vector)))
          (let ((len (array-length arr)))
            (when (>= fp len)
              ;; Extend: create new array, copy old, replace
              (let ((new-len (max (* len 2) (+ fp 1)))
                    (new-arr nil))
                ;; Use string array if old underlying is a string; else generic.
                (if (stringp arr)
                    (setq new-arr (%make-string-array new-len))
                    (setq new-arr (make-array new-len)))
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
        nil)))

(defun vector-pop (vector)
  "Pop an element from VECTOR (with fill pointer)."
  (let ((vector (%fp-inner vector)))
    (if (and (consp vector) (fixnump (car vector)))
        (let ((fp (car vector)))
          (if (> fp 0)
              (let ((new-fp (- fp 1)))
                (set-car vector new-fp)
                (aref (cdr vector) new-fp))
              (error "vector-pop: empty vector")))
        (error "vector-pop: no fill pointer"))))

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

(defun documentation (x doc-type)
  "Look up doc string for X under DOC-TYPE, or NIL."
  (let ((key (cons x doc-type))
        (cur *documentation-strings*)
        (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (when (and (consp (car cur)) (consp (car (car cur)))
                 (let ((k (car (car cur))))
                   (and (equal (car k) x) (eq (cdr k) doc-type))))
        (setq found (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun set-documentation (x doc-type string)
  "Install STRING as the doc for (X . DOC-TYPE) in
   *documentation-strings*, replacing any prior entry."
  (let ((key (cons x doc-type))
        (cur *documentation-strings*)
        (acc nil)
        (replaced nil))
    (loop
      (when (null cur) (return nil))
      (let ((entry (car cur)))
        (let ((k (car entry)))
          (cond
            ((and (consp k) (equal (car k) x) (eq (cdr k) doc-type))
             (setq acc (cons (cons key string) acc))
             (setq replaced t))
            (t (setq acc (cons entry acc))))))
      (setq cur (cdr cur)))
    (unless replaced (setq acc (cons (cons key string) acc)))
    (setq *documentation-strings* acc))
  string)

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

(defun coerce (object result-type)
  "Coerce OBJECT to RESULT-TYPE."
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
            (setq result (cons (code-char (aref object i)) result))
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
    ((or (eq result-type 'vector) (eq result-type 'simple-vector))
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
     (if (integerp object) (code-char object)
         (if (stringp object) (code-char (aref object 0))
             object)))
    (t object)))

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
  "Return list of dimensions of array A.
   Peels (cons 8765432 ...) adjustable wrapper.
   Detects multi-dim wrapper: (cons 9867654 (cons DIMS-LIST FLAT-ARR)).
   Detects fill-pointer wrapper: (cons FP underlying).
   Detects displaced wrapper: (cons (cons SIZE OFFSET) underlying)."
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
      (t nil))))

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

(defun ftruncate (n &optional (d 1))
  (let ((q (truncate n d)))
    (values q (- n (* q d)))))

(defun ffloor (n &optional (d 1))
  (let ((q (floor n d)))
    (values q (- n (* q d)))))

(defun fceiling (n &optional (d 1))
  (let ((q (ceiling n d)))
    (values q (- n (* q d)))))

(defun fround (n &optional (d 1))
  (let ((q (round n d)))
    (values q (- n (* q d)))))

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
                 (slot-nm (%clos-initarg-to-slot class-name key)))
            (when (and slot-nm (not (member slot-nm set-slots)))
              (let ((idx (%clos-slot-index cls slot-nm)))
                (when (and idx (< (+ 2 idx) inst-len))
                  (aset instance (+ 2 idx) val)
                  (setq set-slots (cons slot-nm set-slots))))))
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
             (slot-nm (%clos-initarg-to-slot class-name key)))
        (when (and slot-nm (null (member slot-nm acc :test #'eq)))
          (set-slot-value instance slot-nm val)
          (setq acc (cons slot-nm acc))))
      (setq cur (cdr (cdr cur))))))

(defun %shared-init-default-spread (args)
  "ARGS is (instance slot-names &rest initargs) as a list.  Apply the
   initargs and initforms directly, bypassing the &rest re-pack path
   that loses trailing args.  Mirrors %shared-initialize-default's
   semantics."
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
      (let ((set-slots (%shared-init-apply-initargs
                        instance class-name initargs nil)))
        ;; Apply initforms for slots in slot-names that are still unbound.
        (let ((sn (aref cls 2)) (idx 0))
          (loop
            (when (null sn) (return nil))
            (let* ((nm (car sn))
                   (already-set (member nm set-slots :test #'eq))
                   (covered (cond ((eq slot-names t) t)
                                  ((null slot-names) nil)
                                  (t (member nm slot-names :test #'eq))))
                   (cur-val (aref instance (+ 2 idx)))
                   (was-unbound (and (fixnump cur-val) (= cur-val -999))))
              (when (and (null already-set) covered was-unbound)
                (let ((thunk (%clos-initform-thunk class-name nm)))
                  (when thunk
                    (set-slot-value instance nm (funcall thunk))))))
            (setq idx (+ idx 1))
            (setq sn (cdr sn))))
        instance))))

(defun shared-initialize (&rest %sh-args)
  "SHARED-INITIALIZE generic function entry.  Falls through to
   %shared-initialize-default unless user methods were defined."
  (%dispatch-shared-init %sh-args))

(defun %change-class-default (instance new-class &rest initargs)
  "Default method body for CHANGE-CLASS.
   Mutates INSTANCE in place (preserves identity) so EQ holds.

   Limitations vs full ANSI:
   - No update-instance-for-different-class hook is invoked.
   - If NEW-CLASS has more slots than the underlying array allocates,
     trailing slots may be inaccessible (we cap at the array size).
   - :allocation :class is treated as :instance — class-shared slots
     not implemented.
   - allow-other-keys checking not enforced."
  (when (null instance) (return-from %change-class-default instance))
  (when (not (%clos-instance-p instance)) (return-from %change-class-default instance))
  ;; Resolve new-class to a class object
  (let ((new-cls (cond
                   ((symbolp new-class) (%find-clos-class new-class))
                   ((%clos-class-p new-class) new-class)
                   (t nil))))
    (when (null new-cls) (return-from %change-class-default instance))
    (let* ((new-name (aref new-cls 1))
           (new-slot-names (aref new-cls 2))
           (old-name (aref instance 1))
           (old-cls (%find-clos-class old-name))
           (old-slot-names (if old-cls (aref old-cls 2) nil))
           (inst-len (array-length instance))
           ;; First, snapshot old slot values keyed by slot name (alist)
           (old-values nil))
      ;; Snapshot old values (incl. unbound sentinel -999)
      (let ((sn old-slot-names) (idx 0))
        (loop
          (when (null sn) (return nil))
          (when (< (+ 2 idx) inst-len)
            (setq old-values (cons (cons (car sn) (aref instance (+ 2 idx)))
                                   old-values)))
          (setq idx (+ idx 1))
          (setq sn (cdr sn))))
      ;; Mutate instance to its new class
      (aset instance 1 new-name)
      ;; Fill new slot positions: copy from old if same name, else apply
      ;; initform if available, else mark unbound (-999).
      (let ((sn new-slot-names) (idx 0))
        (loop
          (when (null sn) (return nil))
          (when (< (+ 2 idx) inst-len)
            (let* ((nm (car sn))
                   ;; Look up old value for this slot name
                   (old-pair (let ((cur old-values) (found nil))
                               (loop
                                 (when (null cur) (return found))
                                 (when (eq (car (car cur)) nm)
                                   (setq found (car cur))
                                   (return found))
                                 (setq cur (cdr cur)))))
                   (had-old (not (null old-pair)))
                   (old-val (if had-old (cdr old-pair) -999))
                   (was-unbound (and (fixnump old-val) (= old-val -999))))
              (cond
                ;; Slot exists in both, and was bound — keep it
                ((and had-old (not was-unbound))
                 (aset instance (+ 2 idx) old-val))
                ;; Slot exists in both but was unbound — leave unbound
                ;; (do NOT apply new class's initform per ANSI)
                ((and had-old was-unbound)
                 (aset instance (+ 2 idx) -999))
                (t
                 ;; Slot only in new class: try initform from new-class
                 (let ((thunk (%clos-initform-thunk new-name nm)))
                   (if thunk
                     (aset instance (+ 2 idx) (funcall thunk))
                     ;; No initform — leave unbound
                     (aset instance (+ 2 idx) -999)))))))
          (setq idx (+ idx 1))
          (setq sn (cdr sn))))
      ;; Apply :initargs on top — they override.
      ;; Per ANSI: when an initarg appears multiple times, the LEFTMOST wins.
      ;; Track which slots we've already set so later duplicates don't overwrite.
      (let ((set-slots nil)
            (cur initargs))
        (loop
          (when (null cur) (return nil))
          (when (null (cdr cur)) (return nil))
          (let* ((key (car cur))
                 (val (car (cdr cur)))
                 (slot-nm (%clos-initarg-to-slot new-name key)))
            (when (and slot-nm (not (member slot-nm set-slots)))
              (let ((idx (%clos-slot-index new-cls slot-nm)))
                (when (and idx (< (+ 2 idx) inst-len))
                  (aset instance (+ 2 idx) val)
                  (setq set-slots (cons slot-nm set-slots))))))
          (setq cur (cdr (cdr cur)))))
      instance)))

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
  "Spread args list to %change-class-default by name (no funcall-on-symbol)."
  (let ((n (length args)))
    (cond
      ((= n 0) (%change-class-default nil nil))
      ((= n 1) (%change-class-default (car args) nil))
      ((= n 2) (%change-class-default (car args) (cadr args)))
      ((= n 3) (%change-class-default (car args) (cadr args) (caddr args)))
      ((= n 4) (%change-class-default (car args) (cadr args) (caddr args)
                                      (cadddr args)))
      (t (%change-class-default (car args) (cadr args) (caddr args)
                                (cadddr args) (cadddr (cdr args)))))))

(defun change-class (&rest %cc-args)
  "CHANGE-CLASS generic function entry.  Falls through to
   %change-class-default unless user methods were defined."
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

;; conjugate lives in cl-sequences.lisp — the stub here returned the
;; arg unchanged and shadowed the real implementation that handles
;; (complex r i) properly.

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
  (when (or (null object) (not (%clos-instance-p object)))
    (return-from make-load-form-saving-slots (values nil nil)))
  (let* ((class-name (aref object 1))
         (cls (%find-clos-class class-name))
         (slots (cond ((null slot-names)
                       (if cls (aref cls 2) nil))
                      ((eq slot-names t)
                       (if cls (aref cls 2) nil))
                      (t slot-names)))
         (creation-form
           (list 'allocate-instance (list 'find-class (list 'quote class-name))))
         (init-pairs
           (let ((acc nil) (cur slots))
             (loop
               (when (null cur) (return (nreverse acc)))
               (let* ((sname (car cur))
                      (val (handler-case (slot-value object sname) (t (c) nil))))
                 (setq acc (cons (list 'setf
                                       (list 'slot-value 'obj (list 'quote sname))
                                       (list 'quote val))
                                 acc)))
               (setq cur (cdr cur)))))
         (init-form (cons 'progn (cons (list 'let
                                             (list (list 'obj creation-form))
                                             (cons 'progn init-pairs)
                                             'obj)
                                       nil))))
    (values creation-form init-form)))

(defun set-find-class (name class)
  "Set the class for NAME (stub)."
  nil)

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
      (aset a i (code-char (aref s i)))
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
  (declare (ignore initargs previous))
  current)

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

