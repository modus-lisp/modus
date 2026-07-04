;;;; prelude.lisp - Library functions for MVM self-compilation
;;;;
;;;; These are MVM-compilable implementations of Common Lisp library
;;;; functions used by the MVM compiler, translators, and cross-compilation
;;;; pipeline. They are loaded as source and compiled by MVM, providing
;;;; runtime definitions for self-hosting.
;;;;
;;;; All functions here must use only features MVM can compile:
;;;; - defun with positional args only (no &key, &optional, &rest)
;;;; - let, let*, if, cond, loop, setq, progn, when, unless
;;;; - car, cdr, cons, consp, null, not, eq, atom
;;;; - Arithmetic, comparisons, logand, logior, logxor, ash
;;;; - write-byte, mem-ref

(in-package :modus.mvm)

;;; ============================================================
;;; List Utilities
;;; ============================================================

(defun not (x)
  "Logical negation — same as null."
  (null x))

;;; ============================================================
;;; Function-cell wrappers for primitive ops
;;; ============================================================
;;; eql, eq, =, etc. are inline opcodes — they have no callable function
;;; entry, so #'eql is a NIL/garbage pointer. Tests that pass `:test #'eql`
;;; or `:test-not 'eql` get silently wrong answers. compile-function-ref
;;; rewrites #'eql etc. to these wrappers.
(defun %eql-fn (a b) (eql a b))
(defun %eq-fn (a b) (eq a b))
(defun %equal-fn (a b) (equal a b))
(defun %lt-fn (a b) (< a b))
(defun %gt-fn (a b) (> a b))
(defun %le-fn (a b) (<= a b))
(defun %ge-fn (a b) (>= a b))
(defun %eq-num-fn (a b) (= a b))
(defun %ne-num-fn (a b) (/= a b))

(defun nth (n list)
  "Return the Nth element of LIST (0-indexed). Signals TYPE-ERROR if N
   is not a non-negative fixnum (matches the ANSI requirement that N
   be of type unsigned-byte)."
  (when (or (not (fixnump n)) (< n 0))
    (%signal-type-error))
  (let ((i 0)
        (cur list))
    (loop
      (when (null cur) (return nil))
      (when (= i n) (return (car cur)))
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

(defun nthcdr (n list)
  "Return the Nth cdr of LIST. Signals TYPE-ERROR on negative or
   non-fixnum N."
  (when (or (not (fixnump n)) (< n 0))
    (%signal-type-error))
  (let ((i 0)
        (cur list))
    (loop
      (when (= i n) (return cur))
      (when (null cur) (return nil))
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

(defun last (list &rest n-arg)
  "Return the last N cons cells of LIST. N defaults to 1.
   Signals TYPE-ERROR if N is not a non-negative fixnum,
   or if LIST is neither a list nor NIL,
   or if more than one optional argument is supplied."
  ;; ANSI: (last list &optional (n 1)). Extra args → program-error.
  (when (and n-arg (cdr n-arg))
    (error "last: too many arguments"))
  (when (and list (not (consp list)))
    (error "last: first argument is not a list"))
  (let ((n (if n-arg (car n-arg) 1)))
    (when (or (not (fixnump n)) (< n 0))
      (%signal-type-error))
    (if (null list)
        nil
        (let ((len 0) (cur list))
          ;; Count length
          (loop
            (when (atom cur) (return nil))
            (setq len (+ len 1))
            (setq cur (cdr cur)))
          ;; Skip to (len - n)th element
          (if (<= len n)
              list
              (let ((skip (- len n))
                    (cur list))
                (loop
                  (when (= skip 0) (return cur))
                  (setq skip (- skip 1))
                  (setq cur (cdr cur)))))))))

(defun nreverse (list)
  "Destructively reverse LIST in place."
  (let ((prev nil)
        (cur list))
    (loop
      (when (null cur) (return prev))
      (let ((next (cdr cur)))
        (set-cdr cur prev)
        (setq prev cur)
        (setq cur next)))))

(defun reverse (list)
  "Return a new list that is the reverse of LIST."
  (let ((result nil)
        (cur list))
    (loop
      (when (null cur) (return result))
      (setq result (cons (car cur) result))
      (setq cur (cdr cur)))))

(defun append2 (list1 list2)
  "Append two lists non-destructively."
  (if (null list1)
      list2
      (cons (car list1) (append2 (cdr list1) list2))))

(defun append (&rest lists)
  "Append any number of lists."
  (if (null lists) nil
    (if (null (cdr lists)) (car lists)
      (let ((result (append2 (car lists) (cadr lists)))
            (rest (cddr lists)))
        (loop
          (when (null rest) (return result))
          (setq result (append2 result (car rest)))
          (setq rest (cdr rest)))))))

(defun nconc2 (list1 list2)
  "Destructively append LIST2 to the end of LIST1."
  (if (null list1)
      list2
      (let ((tail (last list1)))
        (set-cdr tail list2)
        list1)))

(defun nconc (&rest lists)
  "Destructively concatenate any number of lists."
  (if (null lists) nil
    (let ((result (car lists))
          (rest (cdr lists)))
      (loop
        (when (null rest) (return result))
        (setq result (nconc2 result (car rest)))
        (setq rest (cdr rest))))))

(defun copy-list (list)
  "Return a shallow copy of LIST."
  (if (null list)
      nil
      (let ((result (cons (car list) nil))
            (tail nil)
            (cur (cdr list)))
        (setq tail result)
        (loop
          (when (null cur) (return result))
          (let ((new-cell (cons (car cur) nil)))
            (set-cdr tail new-cell)
            (setq tail new-cell)
            (setq cur (cdr cur)))))))

;;; ============================================================
;;; Search and Membership
;;; ============================================================

(defun member (item list &rest options)
  "Return the tail of LIST starting from the first element EQL to ITEM.
   Signals TYPE-ERROR if LIST is not a list.
   Honors ANSI &key TEST/TEST-NOT/KEY.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates."
  (when (and (not (null list)) (not (consp list)))
    (%signal-type-error))
  (%subst-check-kwargs options)
  (let ((test nil) (test-not nil) (key nil)
        (test-set nil) (tn-set nil) (key-set nil)
        (a options))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test)
             (unless test-set (setq test (cadr a)) (setq test-set t))
             (setq a (cddr a)))
            ((eq (car a) :test-not)
             (unless tn-set (setq test-not (cadr a)) (setq tn-set t))
             (setq a (cddr a)))
            ((eq (car a) :key)
             (unless key-set (setq key (cadr a)) (setq key-set t))
             (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (let ((cur list))
      (loop
        (when (null cur) (return nil))
        (let* ((v (if key (funcall key (car cur)) (car cur)))
               (matched
                (cond
                  (test (funcall test item v))
                  (test-not (not (funcall test-not item v)))
                  (t (eql item v)))))
          (when matched (return cur)))
        (setq cur (cdr cur))))))

(defun member-string (item list)
  "Return the tail of LIST starting from the first element STRING-EQUAL to ITEM."
  (let ((cur list))
    (loop
      (when (null cur) (return nil))
      (when (string-equal (car cur) item) (return cur))
      (setq cur (cdr cur)))))

(defun assoc (key alist &rest options)
  "Find the first pair in ALIST whose car matches KEY. Honors :TEST,
   :TEST-NOT, :KEY. Default test is inline `eql`.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates."
  (%subst-check-kwargs options)
  (let ((test nil) (test-not nil) (key-fn nil)
        (test-set nil) (tn-set nil) (key-set nil)
        (a options))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test)
             (unless test-set (setq test (cadr a)) (setq test-set t))
             (setq a (cddr a)))
            ((eq (car a) :key)
             (unless key-set (setq key-fn (cadr a)) (setq key-set t))
             (setq a (cddr a)))
            ((eq (car a) :test-not)
             (unless tn-set (setq test-not (cadr a)) (setq tn-set t))
             (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (when (and test-not (not test))
      (let ((f test-not))
        (setq test (lambda (x y) (not (funcall f x y))))))
    (let ((cur alist))
      (loop
        (when (null cur) (return nil))
        (let ((pair (car cur)))
          (cond ((null pair) nil)        ; skip NIL alist entries (SBCL-compat)
                ((not (consp pair)) (error "assoc: not a cons"))
                (t (let ((c (if key-fn (funcall key-fn (car pair)) (car pair))))
                     (when (if test (funcall test key c) (eql key c))
                       (return pair))))))
        (setq cur (cdr cur))))))

(defun assoc-string (key alist)
  "Find the first pair in ALIST whose car is STRING-EQUAL to KEY."
  (let ((cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (when (consp pair)
          (when (string-equal (car pair) key) (return pair)))
        (setq cur (cdr cur))))))

(defun find-in-list (item list)
  "Find ITEM in LIST using EQL."
  (let ((cur list))
    (loop
      (when (null cur) (return nil))
      (when (eql (car cur) item) (return (car cur)))
      (setq cur (cdr cur)))))

(defun position-in-list (item list)
  "Return the index of ITEM in LIST (EQL test), or nil."
  (let ((cur list)
        (idx 0))
    (loop
      (when (null cur) (return nil))
      (when (eql (car cur) item) (return idx))
      (setq idx (+ idx 1))
      (setq cur (cdr cur)))))

(defun position (item seq &rest options)
  "Return the index of ITEM in SEQ (list or array). Honors :TEST, :KEY,
   :START, :END, :FROM-END. Default test is inline `eql`."
  (let ((test nil) (key nil) (start 0) (end nil) (from-end nil)
        (a options))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test) (setq test (cadr a)) (setq a (cddr a)))
            ((eq (car a) :key)  (setq key  (cadr a)) (setq a (cddr a)))
            ((eq (car a) :start) (setq start (cadr a)) (setq a (cddr a)))
            ((eq (car a) :end)  (setq end  (cadr a)) (setq a (cddr a)))
            ((eq (car a) :from-end) (setq from-end (cadr a)) (setq a (cddr a)))
            ((eq (car a) :test-not)
             (let ((f (cadr a)))
               (setq test (lambda (x y) (not (funcall f x y)))))
             (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (cond
      ((null seq) nil)
      ((consp seq)
       ;; Walk to start, then iterate to end (or list end)
       (let ((cur seq) (i 0) (result nil))
         (loop (when (or (null cur) (= i start)) (return nil))
           (setq cur (cdr cur)) (setq i (+ i 1)))
         (loop
           (when (null cur) (return result))
           (when (and end (= i end)) (return result))
           (let ((v (if key (funcall key (car cur)) (car cur))))
             (when (if test (funcall test item v) (eql item v))
               (if from-end (setq result i) (return i))))
           (setq cur (cdr cur)) (setq i (+ i 1)))))
      (t  ;; vector
       (let ((len (array-length seq))
             (result nil))
         (when (null end) (setq end len))
         (let ((i start))
           (loop (when (= i end) (return result))
             (let ((v (if key (funcall key (aref seq i)) (aref seq i))))
               (when (if test (funcall test item v) (eql item v))
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1)))))))))

(defun remove-if (pred seq)
  "Return a new sequence with elements for which PRED returns non-nil removed.
   Polymorphic over list / string / plain array — returns the same shape.
   Note: this short form ignores keyword args; the keyword-aware version
   in cl-sequences.lisp shadows this for callers via apply."
  (cond
    ((or (null seq) (consp seq))
     (let ((result nil)
           (cur seq))
       (loop
         (when (null cur) (return (nreverse result)))
         (unless (funcall pred (car cur))
           (setq result (cons (car cur) result)))
         (setq cur (cdr cur)))))
    ((stringp seq)
     ;; Count kept, allocate sized string, copy slot-wise (slots are fixnums).
     (let ((len (array-length seq))
           (kept 0))
       (let ((i 0))
         (loop (when (= i len) (return nil))
           (unless (funcall pred (aref seq i))
             (setq kept (+ kept 1)))
           (setq i (+ i 1))))
       (let ((out (%make-string-array kept)) (i 0) (j 0))
         (loop (when (= i len) (return out))
           (unless (funcall pred (aref seq i))
             (aset out j (aref seq i))
             (setq j (+ j 1)))
           (setq i (+ i 1))))))
    (t
     (let ((len (array-length seq))
           (kept 0))
       (let ((i 0))
         (loop (when (= i len) (return nil))
           (unless (funcall pred (aref seq i))
             (setq kept (+ kept 1)))
           (setq i (+ i 1))))
       (let ((out (make-array kept)) (i 0) (j 0))
         (loop (when (= i len) (return out))
           (unless (funcall pred (aref seq i))
             (aset out j (aref seq i))
             (setq j (+ j 1)))
           (setq i (+ i 1))))))))

;;; ============================================================
;;; Higher-Order Functions
;;; ============================================================

(defun mapcar1 (fn list)
  "Apply FN to each element of LIST, collecting results."
  (let ((result nil)
        (cur list))
    (loop
      (when (null cur) (return (nreverse result)))
      (setq result (cons (funcall fn (car cur)) result))
      (setq cur (cdr cur)))))

(defun mapcar (fn list &rest more-lists)
  "Apply FN to each element of LIST (and MORE-LISTS if provided).
   Fast paths for 1/2/3 lists avoid apply-of-rest fragility."
  (cond
    ((null more-lists) (mapcar1 fn list))
    ((null (cdr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2)) (return (nreverse result)))
         (setq result (cons (funcall fn (car c1) (car c2)) result))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)))))
    ((null (cddr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (c3 (cadr more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return (nreverse result)))
         (setq result (cons (funcall fn (car c1) (car c2) (car c3)) result))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    (t (let ((result nil)
             (lists (cons list more-lists)))
         (loop
           (when (some #'null lists) (return (nreverse result)))
           (setq result (cons (apply fn (mapcar1 #'car lists)) result))
           (setq lists (mapcar1 #'cdr lists)))))))

(defun some (fn list)
  "Return the first non-nil result of applying FN to elements of LIST."
  (let ((cur list))
    (loop
      (when (null cur) (return nil))
      (let ((result (funcall fn (car cur))))
        (when result (return result)))
      (setq cur (cdr cur)))))

(defun every (fn list &rest more-lists)
  "Return T if FN returns non-nil for all elements."
  (if (null more-lists)
      ;; Single list
      (let ((cur list))
        (loop
          (when (null cur) (return t))
          (when (null (funcall fn (car cur))) (return nil))
          (setq cur (cdr cur))))
      ;; Two lists
      (let ((a list) (b (car more-lists)))
        (loop
          (when (or (null a) (null b)) (return t))
          (when (null (funcall fn (car a) (car b))) (return nil))
          (setq a (cdr a))
          (setq b (cdr b))))))

(defun reduce (fn seq &rest args)
  "Fold FN over SEQ (list or vector). Honors :initial-value, :from-end,
   :start, :end, :key.  Without :initial-value:
     - empty seq: return (funcall fn) with no args
     - single elt: return that element unchanged
     - else: starts with the first element"
  (%check-kw-allowed args '(:initial-value :from-end :start :end :key))
  (let ((init :no-init) (init-given nil)
        (from-end nil) (start 0) (end nil) (key nil)
        (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :initial-value)
             (setq init (cadr a)) (setq init-given t) (setq a (cddr a)))
            ((eq (car a) :from-end) (setq from-end (cadr a)) (setq a (cddr a)))
            ((eq (car a) :start) (setq start (cadr a)) (setq a (cddr a)))
            ((eq (car a) :end)   (setq end   (cadr a)) (setq a (cddr a)))
            ((eq (car a) :key)   (setq key   (cadr a)) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    ;; Materialise as a list slice [start..end). Cheap conversion: walk
    ;; the original sequence (works for lists, vectors, fill-pointer
    ;; arrays via aref/length), collect the elements we'll fold.
    (let* ((elts (let ((lst nil) (i 0) (n (if seq (length seq) 0))
                       (eff-end (if end end (if seq (length seq) 0))))
                   (cond
                     ;; Wrapper cons — index via elt (wrapper-aware)
                     ((and (consp seq) (array-wrapper-p seq))
                      (loop (when (>= i eff-end) (return nil))
                        (when (>= i start)
                          (let ((v (if key (funcall key (elt seq i)) (elt seq i))))
                            (setq lst (cons v lst))))
                        (setq i (+ i 1))))
                     ((or (null seq) (consp seq))
                      ;; Walk list once
                      (let ((cur seq))
                        (loop (when (null cur) (return nil))
                          (when (and (>= i start) (< i eff-end))
                            (let ((v (if key (funcall key (car cur)) (car cur))))
                              (setq lst (cons v lst))))
                          (setq cur (cdr cur))
                          (setq i (+ i 1)))))
                     (t
                      ;; Vector/string indexing
                      (loop (when (>= i eff-end) (return nil))
                        (when (>= i start)
                          (let ((v (if key (funcall key (elt seq i)) (elt seq i))))
                            (setq lst (cons v lst))))
                        (setq i (+ i 1)))))
                   (nreverse lst))))
      (cond
        ((and (null elts) init-given) init)
        ((null elts) (funcall fn))
        ((and (null (cdr elts)) (not init-given)) (car elts))
        (from-end
         ;; Right-fold: (fn e1 (fn e2 (fn e3 init)))
         (let* ((rev (reverse elts))
                (acc (if init-given init (car rev)))
                (cur (if init-given rev (cdr rev))))
           (loop (when (null cur) (return acc))
             (setq acc (funcall fn (car cur) acc))
             (setq cur (cdr cur)))))
        (t
         ;; Left-fold: (fn (fn (fn init e1) e2) e3)
         (let* ((acc (if init-given init (car elts)))
                (cur (if init-given elts (cdr elts))))
           (loop (when (null cur) (return acc))
             (setq acc (funcall fn acc (car cur)))
             (setq cur (cdr cur)))))))))

(defun mapc (fn list)
  "Apply FN to each element of LIST for side effects. Return LIST."
  (let ((cur list))
    (loop
      (when (null cur) (return list))
      (funcall fn (car cur))
      (setq cur (cdr cur)))))

;;; ============================================================
;;; Length and Counting
;;; ============================================================

(defun list-length (list)
  "Return the length of LIST, or NIL for circular lists.
   Uses tortoise-and-hare cycle detection.
   Signals a type-error if LIST is not a proper list (i.e., dotted or
   non-list).  Per ANSI CLHS list-length:
     - proper list → length
     - circular     → NIL
     - improper     → type-error"
  (when (and list (not (consp list)))
    (%signal-type-error))
  (let ((n 0)
        (fast list)
        (slow list))
    (loop
      ;; Fast pointer moves 2 steps; bail with type-error on dotted tail.
      (when (null fast) (return n))
      (when (not (consp fast)) (%signal-type-error))
      (setq fast (cdr fast))
      (setq n (+ n 1))
      (when (null fast) (return n))
      (when (not (consp fast)) (%signal-type-error))
      (setq fast (cdr fast))
      (setq n (+ n 1))
      ;; Slow pointer moves 1 step
      (setq slow (cdr slow))
      ;; If they meet, it's circular
      (when (eq fast slow) (return nil)))))

(defun cddddr (x) (cdr (cdddr x)))
(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))

(defun copy-tree (tree)
  "Return a fresh copy of TREE (all conses copied recursively)."
  (if (consp tree)
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
      tree))

(defun %length-cdr-is-array-tail-p (x)
  "True if X is a real array, a string, or a one-cons-deep wrapper around
   one.  Used by LENGTH to disambiguate a fp / displaced wrapper from an
   ordinary list whose car happens to be a fixnum or cons.

   Bounded at one cons hop (so this is O(1), not O(list-length)) — long
   ANSI lists like `(make-list 200000)` would otherwise blow the stack.
   The adjustable wrapper layer (cons 8765432 ...) is peeled by LENGTH
   before invoking this helper, so we only need to recognise fp / displaced
   shapes here."
  (cond
    ((null x) nil)
    ((arrayp x) t)
    ((stringp x) t)
    ((not (consp x)) nil)
    (t (let ((cd (cdr x)))
         (cond
           ((null cd) nil)
           ((arrayp cd) t)
           ((stringp cd) t)
           (t nil))))))

(defun length (seq)
  "Return the length of SEQ (list or array).
   For array wrappers (commit 7c9a463: adj/fp/displaced/multi-dim wrappers
   over plain arrays), returns the ANSI length:
     fp wrapper       → fill pointer
     adj-only wrapper → length of underlying
     displaced        → declared size
     multi-dim        → length of flat backing array
     native MDA #x34  → fp if present, else array-length of data
   Plain conses route through list-length as before.
   A wrapper is disambiguated from an ordinary list by its CDR (chain)
   eventually pointing at an array/string rather than NIL/list."
  (cond
    ((null seq) 0)
    ((consp seq)
     (cond
       ;; adjustable wrapper marker — always a wrapper, peel and recurse
       ((eql (car seq) 8765432)
        (length (cdr seq)))
       ;; multi-dim wrapper marker
       ((and (eql (car seq) 9867654) (consp (cdr seq)))
        (array-length (cddr seq)))
       ;; fp / displaced wrapper — disambiguate from list by walking cdr chain
       ((%length-cdr-is-array-tail-p (cdr seq))
        (cond
          ((fixnump (car seq)) (car seq))                  ; fp → fill pointer
          ((consp (car seq))   (car (car seq)))            ; displaced → size
          (t (list-length seq))))                          ; defensive
       ;; ordinary list
       (t (list-length seq))))
    ;; Native MDA (subtag #x34) — fp first, else walk dims.  Cannot use
    ;; (array-length (%mda-data seq)) — for displaced MDAs, data slot
    ;; holds the displaced-to target which may be larger.
    ((%mda-p seq)
     (let ((fp (%mda-fp seq)))
       (cond
         (fp fp)
         (t (let ((dims (%mda-dims seq)) (total 1))
              (loop (when (null dims) (return total))
                (setq total (* total (car dims)))
                (setq dims (cdr dims))))))))
    (t (array-length seq))))

;;; ============================================================
;;; Array Utilities
;;; ============================================================

(defun copy-seq (array)
  "Copy an array, returning a new array with the same elements."
  (let ((len (array-length array))
        (result (make-array (array-length array))))
    (let ((i 0))
      (loop
        (when (= i len) (return result))
        (aset result i (aref array i))
        (setq i (+ i 1))))))

;;; ============================================================
;;; String Utilities
;;; ============================================================

(defun char-upcase (ch)
  "Return uppercase version of character CH."
  (let ((code (char-code ch)))
    (if (if (>= code 97) (<= code 122) nil)
        (code-char (- code 32))
        ch)))

(defun char-downcase (ch)
  "Return lowercase version of character CH."
  (let ((code (char-code ch)))
    (if (if (>= code 65) (<= code 90) nil)
        (code-char (+ code 32))
        ch)))

(defun string-upcase (str)
  "Return a new string with all characters uppercased.
   Uses %make-string-array so the result is actually a string (subtag),
   and converts char-upcase's returned character back to a fixnum char-code
   to match the convention that string slots hold fixnum char-codes."
  (let ((len (array-length str))
        (result (%make-string-array (array-length str))))
    (let ((i 0))
      (loop
        (when (= i len) (return result))
        (let ((ch (aref str i)))
          (let ((up (char-upcase ch)))
            (aset result i (if (characterp up) (char-code up) up))))
        (setq i (+ i 1))))))

(defun string-downcase (str)
  "Return a new string with all characters lowercased."
  (let ((len (array-length str))
        (result (%make-string-array (array-length str))))
    (let ((i 0))
      (loop
        (when (= i len) (return result))
        (let ((ch (aref str i)))
          (let ((dn (char-downcase ch)))
            (aset result i (if (characterp dn) (char-code dn) dn))))
        (setq i (+ i 1))))))

;;; ============================================================
;;; Equality
;;; ============================================================

(defun string-equal (a b &rest args)
  "Case-insensitive string compare (ANSI semantics: chars match if
   they would compare CHAR-EQUAL, i.e. case-folded for ASCII letters).
   STRING= is the case-sensitive variant.  Most callers in
   cl-packages.lisp / make-condition initarg matching want this case-
   insensitive form so cross-file keyword identity works.

   Honors :START1/:END1/:START2/:END2 — internal 2-arg callers still
   work (args is NIL, bounds default to full strings)."
  (let* ((s1 0) (e1 nil) (s2 0) (e2 nil) (o args) (allow-other nil))
    ;; Pre-scan for :allow-other-keys — LEFTMOST-wins (CLHS 3.4.1.4): the
    ;; value of the FIRST occurrence governs, so stop at the first one.
    ;; string-equal.error.6 passes ":allow-other-keys nil :allow-other-keys
    ;; t :foo bar" and expects a program-error (leading nil => :foo illegal).
    (let ((scan args))
      (loop (when (or (null scan) (null (cdr scan))) (return))
        (when (eq (car scan) :allow-other-keys)
          (when (cadr scan) (setq allow-other t))
          (return))
        (setq scan (cddr scan))))
    (loop (when (null o) (return))
      (when (null (cdr o))
        (error "string-equal: odd-length keyword arg list"))
      (let ((k (car o)))
        (cond ((eq k :start1) (setq s1 (cadr o)))
              ((eq k :end1)   (setq e1 (cadr o)))
              ((eq k :start2) (setq s2 (cadr o)))
              ((eq k :end2)   (setq e2 (cadr o)))
              ((eq k :allow-other-keys) nil)
              (allow-other nil)
              (t (error "string-equal: bad keyword"))))
      (setq o (cddr o)))
    (let* ((len-a (array-length a))
           (len-b (array-length b))
           (ee1 (or e1 len-a))
           (ee2 (or e2 len-b))
           (slen1 (- ee1 s1))
           (slen2 (- ee2 s2)))
      (if (= slen1 slen2)
          (let ((i 0))
            (loop
              (when (= i slen1) (return t))
              ;; AREF on a string now yields a CHARACTER; normalize to a
              ;; raw code for the case-fold + numeric compare.
              (let ((ca (%ensure-char-code (aref a (+ s1 i))))
                    (cb (%ensure-char-code (aref b (+ s2 i)))))
                (when (and (>= ca 65) (<= ca 90)) (setq ca (+ ca 32)))
                (when (and (>= cb 65) (<= cb 90)) (setq cb (+ cb 32)))
                (unless (= ca cb) (return nil)))
              (setq i (+ i 1))))
          nil))))

(defun equal (a b)
  "Structural equality: EQL for atoms, recursive for conses,
   case-SENSITIVE element-wise for strings.  ANSI: EQUAL on strings
   is case-sensitive; STRING-EQUAL is the case-insensitive variant."
  (if (eql a b)
      t
      (if (consp a)
          (if (consp b)
              (if (equal (car a) (car b))
                  (equal (cdr a) (cdr b))
                  nil)
              nil)
          (if (stringp a)
              (if (stringp b)
                  (string= a b)
                  nil)
              nil))))

;;; ============================================================
;;; Sort (insertion sort — simple, O(n²), stable)
;;; ============================================================

(defun sort-list (list pred)
  "Sort LIST using PRED as comparison function. Destructive, stable."
  (if (null list)
      nil
      (if (null (cdr list))
          list
          (let ((sorted nil)
                (cur list))
            (loop
              (when (null cur) (return sorted))
              (let ((item (car cur))
                    (next (cdr cur)))
                ;; Insert item into sorted list
                (if (null sorted)
                    (progn
                      (setq sorted (cons item nil)))
                    (if (funcall pred item (car sorted))
                        ;; Insert at front
                        (setq sorted (cons item sorted))
                        ;; Find insertion point
                        (let ((prev sorted)
                              (scan (cdr sorted))
                              (inserted nil))
                          (loop
                            (when inserted (return nil))
                            (when (null scan)
                              (set-cdr prev (cons item nil))
                              (setq inserted t))
                            (unless inserted
                              (when (funcall pred item (car scan))
                                (set-cdr prev (cons item scan))
                                (setq inserted t))
                              (unless inserted
                                (setq prev scan)
                                (setq scan (cdr scan))))))))
                (setq cur next)))))))

;;; ============================================================
;;; Apply (limited: call with list of args, up to 4 args)
;;; ============================================================

;; APPLY lives in cl-printer.lisp.  It used to be defined here too —
;; the bare-metal compiler's "last defun wins" rule meant the printer
;; version silently overrode this one and extending this body did
;; nothing.  Removed 2026-06-01; see [[reference_apply_cl_printer_override]].
;; If you want to extend the ladder or add a `%spread-call` primitive,
;; edit the cl-printer copy.

;;; ============================================================
;;; Format stub (for self-compilation — writes string to serial)
;;; ============================================================

(defun write-string-serial (str)
  "Write a string to serial output, character by character."
  (let ((len (array-length str))
        (i 0))
    (loop
      (when (= i len) (return nil))
      ;; write-char-serial wants a raw char-CODE; read the byte directly via
      ;; %prim-aref (public AREF now lifts string elements to characters).
      (write-char-serial (%prim-aref str i))
      (setq i (+ i 1)))))

(defun print-dec (n)
  "Print an integer in decimal to serial output."
  (if (< n 0)
      (progn
        (write-char-serial 45)  ;; #\-
        (print-dec (- 0 n)))
      (if (= n 0)
          (write-char-serial 48)  ;; #\0
          (let ((digits nil))
            (let ((tmp n))
              (loop
                (when (= tmp 0) (return nil))
                (setq digits (cons (+ 48 (mod tmp 10)) digits))
                (setq tmp (truncate tmp 10))))
            (let ((cur digits))
              (loop
                (when (null cur) (return nil))
                (write-char-serial (car cur))
                (setq cur (cdr cur))))))))

(defun print-hex (n)
  "Print an integer in hexadecimal to serial output."
  (if (= n 0)
      (write-char-serial 48)  ;; #\0
      (let ((digits nil)
            (tmp n))
        (loop
          (when (= tmp 0) (return nil))
          (let ((digit (logand tmp 15)))
            (if (< digit 10)
                (setq digits (cons (+ 48 digit) digits))
                (setq digits (cons (+ 55 digit) digits))))
          (setq tmp (ash tmp -4)))
        (let ((cur digits))
          (loop
            (when (null cur) (return nil))
            (write-char-serial (car cur))
            (setq cur (cdr cur)))))))

;;; ============================================================
;;; Multiple Values Support
;;; ============================================================
;;;
;;; MV-COUNT at 0x600010: number of values (tagged fixnum)
;;; MV-VALUES at 0x600020: up to 20 extra values (8 bytes each)
;;; Primary value is returned normally; extra values stored here.
;;; Any non-values form implicitly sets MV-COUNT to 1.

(defun %mv-to-list (primary)
  "Collect multiple values into a list. PRIMARY is the first value."
  (let ((count (mem-ref #x10000090 :u64)))
    (if (zerop count)
        nil
        (if (= count 1)
            (cons primary nil)
            (let ((result nil)
                  (i (- count 2)))
              (loop
                (when (< i 0) (return (cons primary result)))
                (setq result (cons (mem-ref (+ #x10000098 (* i 8)) :u64) result))
                (setq i (- i 1))))))))

;;; ============================================================
;;; Object Printer
;;; ============================================================

;; Caller sets this before write-object to bound output on potentially
;; cyclic / huge structures. Each element of a cons/array consumes one.
;; At 0 we print "..." once and stop. Set to big positive to not bound.
(defvar *write-object-budget* 0)

(defun write-object (obj)
  "Print a Lisp object to serial output (prin1-style).
   Bounded by *write-object-budget* if positive.
   Atomic types (fixnum, NIL, T) are always printed — they can't be cyclic
   and consumers like rt-run-test rely on them printing unconditionally."
  (cond
    ;; Atomic, bounded-size types: print unconditionally without touching budget.
    ((fixnump obj) (print-dec obj))
    ((null obj)    (write-char-serial 78) (write-char-serial 73) (write-char-serial 76))
    ((eq obj t)    (write-char-serial 84))
    ;; Budget-exhausted sentinel.
    ((= *write-object-budget* 0)
     ;; First time budget hits zero: emit "..." sentinel and flip to -1
     ;; so subsequent calls produce no output.
     (setq *write-object-budget* -1)
     (write-char-serial 46) (write-char-serial 46) (write-char-serial 46))
    ((< *write-object-budget* 0)
     nil)
    (t
     (setq *write-object-budget* (- *write-object-budget* 1))
     (cond
       ((null obj)
        (write-char-serial 78) (write-char-serial 73) (write-char-serial 76))
       ((eq obj t)
        (write-char-serial 84))
       ((fixnump obj)
        (print-dec obj))
       ((consp obj)
        (write-char-serial 40)
        (write-object (car obj))
        (let ((tail (cdr obj)))
          (loop
            (cond
              ((null tail) (return nil))
              ((<= *write-object-budget* 0)
               ;; Budget exhausted mid-list: emit ellipsis once and stop.
               (write-char-serial 32) (write-char-serial 46)
               (write-char-serial 46) (write-char-serial 46)
               (return nil))
              ((consp tail)
               ;; Each list element consumes one unit of budget so cons-of-fixnum
               ;; lists (which print fixnums unconditionally without decrementing)
               ;; are still bounded.
               (setq *write-object-budget* (- *write-object-budget* 1))
               (write-char-serial 32)
               (write-object (car tail))
               (setq tail (cdr tail)))
              (t
               (write-char-serial 32)
               (write-char-serial 46)
               (write-char-serial 32)
               (write-object tail)
               (return nil)))))
        (write-char-serial 41))
       ((stringp obj)
        (write-char-serial 34)
        (write-string-serial obj)
        (write-char-serial 34))
       ((symbolp obj)
        ;; Prefer the symbol's actual name when recoverable.  3-slot CL
        ;; symbols carry the name string at slot 2; 1-slot native syms
        ;; only carry a hash, fall back to the #<S<hash>> shape.
        (let ((nm (handler-case (symbol-name obj) (t (c) nil))))
          (cond
            ((and (stringp nm) (> (array-length nm) 0))
             (write-string-serial nm))
            (t
             (write-char-serial 35) (write-char-serial 60) (write-char-serial 83)
             (print-dec (aref obj 0))
             (write-char-serial 62)))))
       ((and (not (fixnump obj)) (not (consp obj)) (not (null obj))
             (= (obj-subtag obj) #x32))
        (write-char-serial 35) (write-char-serial 40)
        (let ((len (array-length obj)) (i 0))
          (loop
            (when (= i len) (return nil))
            (when (<= *write-object-budget* 0)
              (write-char-serial 32) (write-char-serial 46)
              (write-char-serial 46) (write-char-serial 46)
              (return nil))
            (when (> i 0)
              ;; Same per-element charge as the cons case — fixnums don't
              ;; consume budget on their own, so the array itself must.
              (setq *write-object-budget* (- *write-object-budget* 1))
              (write-char-serial 32))
            (write-object (aref obj i))
            (setq i (+ i 1))))
        (write-char-serial 41))
       ((characterp obj)
        ;; #\X (print the literal character after a #\ prefix)
        (write-char-serial 35) (write-char-serial 92)
        (write-char-serial (char-code obj)))
       ((and (not (fixnump obj)) (not (consp obj)) (not (null obj))
             (let ((st (obj-subtag obj))) (or (= st #x60) (and (>= st #x64) (<= st #x66)))))
        ;; Boxed IEEE float (any of #x60..#x63: double/single/short/long;
        ;; numeric tower N1) — print integer-part.fractional-part by
        ;; decoding mantissa/exponent.  Approximation: split via
        ;; %ieee-float-to-rat, scale to ~1e9, emit integer.fractional.
        (let* ((rat (%ieee-float-to-rat obj))
               (num (if (ratiop rat) (aref rat 0) rat))
               (den (if (ratiop rat) (aref rat 1) 1))
               (sign (if (< num 0) -1 1))
               (abs-num (if (< num 0) (- 0 num) num))
               (whole (truncate abs-num den))
               (frac-num (- abs-num (* whole den)))
               (frac (truncate (* frac-num 1000000) den)))
          (when (= sign -1) (write-char-serial 45))     ; -
          (print-dec whole)
          (write-char-serial 46)                         ; .
          ;; pad frac with leading zeros to 6 digits
          (let ((pad 100000))
            (loop
              (when (or (= pad 1) (>= frac pad)) (return nil))
              (write-char-serial 48)   ; 0
              (setq pad (truncate pad 10))))
          (print-dec frac)))
       (t
        ;; Emit a type-hinting sentinel that includes the subtag as a
        ;; decimal. "#<?123>" is still a distinctive "unknown type"
        ;; token but lets us tell different unprintable types apart
        ;; in test output.
        (write-char-serial 35) (write-char-serial 60) (write-char-serial 63)
        (when (and (not (fixnump obj)) (not (consp obj)) (not (null obj)))
          (print-dec (obj-subtag obj)))
        (write-char-serial 62))))))

(defun princ-object (obj)
  "Print a Lisp object to serial output (princ-style, no escapes)."
  (cond
    ((null obj)
     (write-char-serial 78)    ; N
     (write-char-serial 73)    ; I
     (write-char-serial 76))   ; L
    ((eq obj t)
     (write-char-serial 84))   ; T
    ((fixnump obj)
     (print-dec obj))
    ((consp obj)
     (write-object obj))       ; same as write-object for cons
    ((stringp obj)
     (write-string-serial obj)) ; no quotes for princ
    (t
     (write-object obj))))

;;; ============================================================
;;; Format
;;; ============================================================

(defun format (stream control &rest args)
  "Basic format: supports ~A ~S ~D ~% ~X ~B directives.
   STREAM: t = serial output, nil = not yet supported.
   Returns nil."
  (let ((len (array-length control))
        (i 0)
        (arg-rest args))
    (loop
      (when (>= i len) (return nil))
      (let ((ch (aref control i)))
        (if (= ch 126)  ; ~
            (progn
              (setq i (+ i 1))
              (when (>= i len) (return nil))
              (let ((directive (aref control i)))
                (cond
                  ;; ~A — aesthetic (princ)
                  ((or (= directive 65) (= directive 97))  ; A or a
                   (princ-object (car arg-rest))
                   (setq arg-rest (cdr arg-rest)))
                  ;; ~S — standard (prin1)
                  ((or (= directive 83) (= directive 115))  ; S or s
                   (write-object (car arg-rest))
                   (setq arg-rest (cdr arg-rest)))
                  ;; ~D — decimal
                  ((or (= directive 68) (= directive 100))  ; D or d
                   (print-dec (car arg-rest))
                   (setq arg-rest (cdr arg-rest)))
                  ;; ~X — hexadecimal
                  ((or (= directive 88) (= directive 120))  ; X or x
                   (print-hex (car arg-rest))
                   (setq arg-rest (cdr arg-rest)))
                  ;; ~% — newline
                  ((= directive 37)  ; %
                   (write-char-serial 10))
                  ;; ~~ — literal tilde
                  ((= directive 126)  ; ~
                   (write-char-serial 126))
                  ;; Unknown directive — print as-is
                  (t
                   (write-char-serial 126)
                   (write-char-serial directive)))))
            (write-char-serial ch)))
      (setq i (+ i 1))))
  nil)

;;; ============================================================
;;; Hash Tables (cons-cell alist, no arrays needed)
;;; ============================================================
;;;
;;; Structure: wrapper cons cell whose car is an alist of
;;; (key . value) pairs and whose cdr is metadata.
;;;
;;;   ht   = (cons alist meta)
;;;   meta = nil                      (legacy: equal test, no metadata)
;;;        | (cons %ht-tag (cons test (cons rsize (cons rthresh size))))
;;;
;;; %HT-TAG = 442386510 (= #x1A571A8E) is a fixnum sentinel that lets
;;; HASH-TABLE-P distinguish a real hash-table from an arbitrary cons.
;;; TEST is one of the symbols EQ EQL EQUAL EQUALP.
;;; O(n) lookup — sufficient for the small tables ANSI exercises.

(defun make-hash-table (&rest options)
  "Create a hash table.  Honoring OPTIONS (`&key TEST SIZE
   REHASH-SIZE REHASH-THRESHOLD`) is delegated to make-hash-table-args.
   The no-arg path builds a %HT-TAG-tagged table (alist still in the CAR,
   so internal callers that car/set-car the alist are unaffected) so
   HASH-TABLE-P can distinguish a real hash-table from an arbitrary cons.
   The TEST slot is NIL (not the symbol EQL) — INIT-SYMBOL-TABLE calls this
   at the very first instruction of boot, before the symbol intern table
   exists, so quoting 'EQL here would intern into an uninitialised table
   and fault.  HASH-TABLE-TEST maps a NIL test slot to EQL.

   The no-arg path now carries the SAME proper 5-list metadata tail as
   make-hash-table-args — (test rsize rthresh size bucket-holder) with
   test = NIL — so legacy tables ALSO graduate to the O(1) 256-bucket
   index once they cross %HT-BUCKET-THRESHOLD entries.  The holder's
   STRCMP? is T because the NIL-test comparator is %EQUAL-FN (strings
   compare by content), exactly like an explicit EQUAL table.  This is
   what fixes the asdf-gauntlet `wedge': the symbol intern table,
   *SYM-NAME-TABLE* (7.5k entries), *SYMBOL-FUNCTION-TABLE* (2.7k) and
   friends are all 0-arg tables, and their O(n) linear gethash walks
   made eval2 compiles of uiop forms take minutes each (quadratic
   blowup that presented as a deterministic rc=124 hang).  No symbol is
   quoted here — conses/fixnums/NIL/T only — so the boot-order
   constraint above still holds."
  (if (null options)
      (cons nil
            (cons (%ht-tag)
                  (cons nil                       ; test (NIL → %equal-fn)
                        (cons 2                   ; rehash-size
                              (cons 1             ; rehash-threshold
                                    (cons 16      ; size
                                          (cons (cons nil (cons 0 t)) ; bucket-holder (vec count . strcmp?)
                                                nil)))))))
      (make-hash-table-args options)))

(defun %ht-keytest (ht)
  "Return the key-comparator FUNCTION for HT's stored :TEST.

   Reads the raw test slot of the metadata directly (NOT via
   HASH-TABLE-TEST) and dispatches.  This is on the GETHASH/PUTHASH hot
   path and runs DURING %INTERN-SYMBOL-PKG at the very first instruction
   of boot — so the NIL-test fast path (every 0-arg / legacy table, incl.
   the symbol intern table itself) must return EQL WITHOUT quoting any
   symbol.  Quoting 'EQL here would call %INTERN-SYMBOL-PKG → GETHASH →
   %HT-KEYTEST → ... infinite recursion → stack overflow at boot.
   Symbol comparison (which interns 'EQ/'EQUAL/'EQUALP) is reached only
   for explicit-:TEST tables, all created after the symbol table exists.

   The NIL fast path returns %EQUAL-FN — NOT %EQL-FN — to exactly match
   the historical hardcoded-EQUAL behavior of every 0-arg / legacy table.
   Several internal 0-arg tables key on STRINGS (e.g. *MACRO-FUNCTION-
   TABLE* via %MACRO-SYM-KEY, the SFT name strings) and would MISS under
   EQL.  This is strictly no worse than the pre-fix behavior, and only
   explicit-:TEST tables get the newly-correct EQ/EQL/EQUALP dispatch."
  (let ((m (%ht-meta ht)))
    (let ((tn (if m (car m) nil)))
      (cond ((null tn)        (function %equal-fn)) ; 0-arg / legacy: EQUAL
            ((eq tn 'eq)      (function %eq-fn))
            ((eq tn 'eql)     (function %eql-fn))
            ((eq tn 'equalp)  (function equalp))
            (t                (function %equal-fn))))))

(defun gethash (key ht &optional default)
  "Look up KEY in hash table HT.  Returns (values value present-p);
   if not present, value is DEFAULT (nil if not supplied) and
   present-p is NIL.  We compute the (value present-p) pair as a cons
   inside the search loop and call `values' once at the tail — calling
   `values' inside `return' was losing the second value (loop+return
   appears to clobber MV-count on its way out)."
  (let ((found-pair nil)
        (use-linear t)
        (holder (%ht-bucket-holder ht)))
    (when holder
      (let ((vec (%ht-active-vec-h ht holder)))
        (when vec
          ;; O(1) bucket path.  :NOHASH key (e.g. a string in an EQ/EQL table)
          ;; falls through to the linear path.
          (let ((r (%ht-bucket-find vec key (%ht-h-strcmp holder))))
            (unless (eq r (%ht-nohash))
              (setq use-linear nil)
              (setq found-pair r))))))
    (when use-linear
      ;; Linear alist path (legacy / small / nohash table / :nohash key).
      (let ((cmp (%ht-keytest ht)) (cur (car ht)))
        (loop
          (when (null cur) (return nil))
          (let ((pair (car cur)))
            (when (funcall cmp (car pair) key)
              (setq found-pair pair)
              (return nil)))
          (setq cur (cdr cur)))))
    (if found-pair
        (values (cdr found-pair) t)
        (values default nil))))

(defun puthash (key ht value)
  "Set KEY to VALUE in hash table HT. Returns VALUE.
   Maintains both the authoritative CAR alist and (for large explicit-:TEST
   tables) the O(1) bucket index + the holder's entry count."
  (let* ((holder (%ht-bucket-holder ht))
         (vec (and holder (%ht-active-vec-h ht holder)))
         (strcmp? (and vec (%ht-h-strcmp holder)))
         (existing (and vec (%ht-bucket-find vec key strcmp?))))
    (cond
      ;; ---- Bucket path: key IS bucketable and already present → update. ----
      ((and vec existing (not (eq existing (%ht-nohash))))
       (set-cdr existing value)               ; update shared pair in place
       value)
      ;; ---- Bucket path: key IS bucketable and absent → add + index. ----
      ((and vec (null existing))
       (let ((new-pair (cons key value)))
         (set-car ht (cons new-pair (car ht)))
         (%ht-h-set-count holder (+ (%ht-h-count holder) 1))
         (%ht-bucket-put vec key new-pair strcmp?)
         value))
      ;; ---- Linear alist path (legacy / small / nohash table, OR a :NOHASH
      ;;      key under an active bucket).  Also bumps the holder count so a
      ;;      small explicit-:TEST table can later flip to the bucket index. ----
      (t
       (let ((cmp (%ht-keytest ht)) (cur (car ht)))
         (loop
           (when (null cur)
             (let ((new-pair (cons key value)))
               (set-car ht (cons new-pair (car ht)))
               (when holder (%ht-h-set-count holder (+ (%ht-h-count holder) 1))))
             (return value))
           (let ((pair (car cur)))
             (when (funcall cmp (car pair) key)
               (set-cdr pair value)
               (return value)))
           (setq cur (cdr cur))))))))

(defun remhash (key ht)
  "Remove KEY from hash table HT. Returns T if removed, NIL otherwise.
   Keeps the bucket index (if any) and the holder count in sync."
  (let* ((entries (car ht))
         (cmp (%ht-keytest ht))
         (holder (%ht-bucket-holder ht))
         (vec (and holder (%ht-active-vec-h ht holder)))
         (removed nil))
    (cond
      ((null entries) (setq removed nil))
      ((funcall cmp (car (car entries)) key)
       (set-car ht (cdr entries))
       (setq removed t))
      (t
       (let ((prev entries) (cur (cdr entries)))
         (loop
           (when (null cur) (return nil))
           (when (funcall cmp (car (car cur)) key)
             (set-cdr prev (cdr cur))
             (setq removed t)
             (return t))
           (setq prev cur)
           (setq cur (cdr cur))))))
    (when removed
      (when vec (%ht-bucket-rem vec key (%ht-h-strcmp holder)))
      (when holder (%ht-h-set-count holder (- (%ht-h-count holder) 1))))
    removed))

(defun maphash (fn ht)
  "Call FN with each key-value pair in HT."
  (let ((cur (car ht)))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (funcall fn (car pair) (cdr pair)))
      (setq cur (cdr cur)))))

;;; -------- ANSI hash-table accessors (added 2026-04-27) --------
;;;
;;; The legacy MAKE-HASH-TABLE above creates (cons nil nil) — equal-keyed,
;;; no recorded test/size/threshold.  These accessors return ANSI-required
;;; defaults for legacy tables; for tables created via the metadata-aware
;;; path (cl-eval.lisp's make-hash-table-args, exposed at runtime as the
;;; CL `MAKE-HASH-TABLE`), they pull the real values from the cdr.

(defun %ht-tag () 442386510)   ; #x1A571A8E

(defun %ht-meta (ht)
  "Return the (test rsize rthresh size) tail-cons, or NIL for legacy."
  (let ((c (cdr ht)))
    (if (and (consp c) (eql (car c) (%ht-tag)))
        (cdr c)
        nil)))

(defun hash-table-p (ht)
  "True iff HT is a hash-table created by MAKE-HASH-TABLE.
   Requires the (cons alist (cons %ht-tag meta)) shape — every table
   MAKE-HASH-TABLE now produces (incl. the 0-arg path) is tagged, so an
   arbitrary cons such as (cons X nil) or a 1-element list no longer
   false-positives."
  (and (consp ht)
       (let ((c (cdr ht)))
         (and (consp c) (eql (car c) (%ht-tag))))))

(defun hash-table-test (ht)
  "Return the test designator for HT — one of EQ EQL EQUAL EQUALP.
   0-arg tables carry a NIL test slot (see MAKE-HASH-TABLE) and report
   EQL (the ANSI default)."
  (let ((m (%ht-meta ht)))
    (if (and m (car m)) (car m) 'eql)))

(defun hash-table-rehash-size (ht)
  "Return the rehash-size of HT (default 2)."
  (let ((m (%ht-meta ht)))
    (if (and m (consp (cdr m))) (car (cdr m)) 2)))

(defun hash-table-rehash-threshold (ht)
  "Return the rehash-threshold of HT (default 1)."
  (let ((m (%ht-meta ht)))
    (if (and m (consp (cdr m)) (consp (cddr m))) (car (cddr m)) 1)))

(defun hash-table-size (ht)
  "Return the declared size of HT (default 16).
   Tolerates both the legacy improper meta tail (test rsize rthresh . SIZE)
   and the new proper 5-list (test rsize rthresh SIZE bucket-holder)."
  (let ((m (%ht-meta ht)))
    (if (and m (consp (cdr m)) (consp (cddr m)))
        (let ((sz-cell (cddr (cdr m))))   ; (cdddr m)
          (if (consp sz-cell)
              (car sz-cell)               ; proper 5-list: 4th element
              sz-cell))                   ; legacy improper: the tail itself
        16)))

;;; -------- O(1) bucket index over the alist (large equal/eq/eql tables) ----
;;; Modus's hash table keeps its authoritative alist in the CAR so that
;;; maphash / hash-table-count / the several internal callers that car/set-car
;;; the alist directly keep working unchanged.  For TABLES CREATED WITH AN
;;; EXPLICIT :TEST (never direct-car'd internally) we additionally maintain a
;;; 256-bucket name->pair index in the metadata's bucket-holder, built lazily
;;; once the table crosses %HT-BUCKET-THRESHOLD entries.  This turns the
;;; O(n) gethash/puthash alist scan into ~O(1) — the ASDF `ensure-package`
;;; define-package wall was ~10s per 1000-key equal table (O(n^2) probing).
;;; Small tables (the overwhelming majority, incl. every boot-time table)
;;; stay on the pure-alist path: the bucket-holder is only present on
;;; explicit-:TEST tables, and only activated past the threshold.

(defun %ht-nohash ()
  "Fixnum sentinel for `key/table is not structurally hashable'.
   Was the keyword :NOHASH — but a keyword literal compiles to a runtime
   %INTERN-KEYWORD call per evaluation, which (a) is a gethash on the
   keyword table on the gethash hot path itself, and (b) faults in
   builds that never call INIT-KEYWORD-TABLE (build-mvm) once legacy
   0-arg tables carry a bucket-holder.  A fixnum compares by EQ in one
   instruction and needs no table."
  -424242001)

(defun %ht-bucket-threshold () 32)

;;; Bucket-holder layout: (vec-or-flag count . strcmp?).
;;;   car  = vector | nil (small/unbuilt) | :nohash (bucketing disabled)
;;;   cadr = entry count (fixnum)
;;;   cddr = STRCMP? — T iff string keys compare by CONTENT (EQUAL tables);
;;;          NIL otherwise (EQ/EQL → string keys are identity, so NOT bucketed).
;;; Storing the precomputed STRCMP? boolean (not the test symbol) lets the hot
;;; path avoid a per-op (eq test 'equal) symbol compare and a HASH-TABLE-TEST
;;; meta walk — that overhead is what erased the bucket win.
(defun %ht-h-vec    (h) (car h))
(defun %ht-h-count  (h) (car (cdr h)))
(defun %ht-h-strcmp (h) (cdr (cdr h)))
(defun %ht-h-set-vec   (h v) (set-car h v))
(defun %ht-h-set-count (h n) (set-car (cdr h) n))

(defun %ht-bucket-holder (ht)
  "Return the (vec-or-flag count . strcmp?) bucket-holder cons for HT, or NIL if
   HT is a legacy table without one.  The proper 5-list meta has the holder as
   its 5th element; the legacy improper meta has no 5th element."
  (let ((m (%ht-meta ht)))
    (if (and (consp m) (consp (cdr m)) (consp (cddr m))
             (consp (cdr (cddr m))) (consp (cddr (cddr m))))
        (car (cddr (cddr m)))   ; 5th element = bucket-holder cons
        nil)))

(defun %ht-hash (key strcmp?)
  "Cheap bucket index for KEY, or :NOHASH for a key we can't index by structure.
   STRINGS are only bucketed when STRCMP? (EQUAL tables, content compare); under
   EQ/EQL distinct string objects with equal contents are DISTINCT keys, so
   string keys are :NOHASH there (correct linear fallback).  Fixnums / chars /
   symbols / nil / t index identically under all tests (immediates / interned).
   String hash is CASE-SENSITIVE."
  (cond
    ((stringp key)
     (if strcmp?
         (let ((h 2166136261) (len (array-length key)) (i 0))
           (loop
             (when (>= i len) (return nil))
             (setq h (logand (* (logxor h (%prim-aref key i)) 16777619) #xFFFFFFFF))
             (setq i (+ i 1)))
           (logand h 255))
         (%ht-nohash)))
    ((fixnump key)  (logand key 255))
    ((characterp key) (logand (char-code key) 255))
    ((null key) 17)
    ((eq key t) 19)
    ((%cl-sym-p key) (logand (%cl-sym-hash key) 255))
    ((%native-mvm-sym-p key) (logand (%native-mvm-sym-hash key) 255))
    (t (%ht-nohash))))

(defun %ht-vec-set (vec i val)
  "Var-index ASET forced to dest=frame-slot (CLAUDE.md var-index ASET bug)."
  (let ((dummy (aset vec i val))) dummy))

(defun %ht-new-bucket-vec ()
  "256 NIL-filled buckets (alloc-array zero-inits to fixnum 0, NOT NIL)."
  (let ((vec (make-array 256)) (i 0))
    (loop
      (when (>= i 256) (return vec))
      (%ht-vec-set vec i nil)
      (setq i (+ i 1)))))

(defun %ht-bucket-find (vec key strcmp?)
  "Find the (key . pair) entry in VEC's bucket for KEY; returns the alist PAIR,
   NIL (absent), or :NOHASH (key not bucketable → caller must use the linear
   alist path).  Bucket entries store the SAME pair cons that lives in the
   table's CAR alist.  Comparison is inlined: STRCMP? strings by content, all
   else by EQL."
  (let ((h (%ht-hash key strcmp?)))
    (if (eq h (%ht-nohash))
        (%ht-nohash)
        (let ((cur (aref vec h)))
          (if (and strcmp? (stringp key))
              ;; string-content path
              (loop
                (when (null cur) (return nil))
                (let ((e (car cur)))
                  (when (let ((ek (car e))) (and (stringp ek) (string= ek key)))
                    (return (cdr e))))
                (setq cur (cdr cur)))
              ;; identity path
              (loop
                (when (null cur) (return nil))
                (let ((e (car cur)))
                  (when (eql (car e) key) (return (cdr e))))
                (setq cur (cdr cur))))))))

(defun %ht-bucket-put (vec key pair strcmp?)
  "Index PAIR (the alist cons) under KEY in VEC.  Caller guarantees KEY isn't
   already present (gethash/puthash check first).  No-op for :NOHASH keys."
  (let ((h (%ht-hash key strcmp?)))
    (unless (eq h (%ht-nohash))
      (let ((old (aref vec h)))
        (%ht-vec-set vec h (cons (cons key pair) old)))))
  pair)

(defun %ht-bucket-rem (vec key strcmp?)
  "Remove KEY's entry from VEC's bucket."
  (let ((h (%ht-hash key strcmp?)))
    (unless (eq h (%ht-nohash))
      (let ((result nil) (cur (aref vec h)))
        (loop
          (when (null cur) (return nil))
          (let ((ek (car (car cur))))
            (unless (if (and strcmp? (stringp key))
                        (and (stringp ek) (string= ek key))
                        (eql ek key))
              (setq result (cons (car cur) result))))
          (setq cur (cdr cur)))
        (%ht-vec-set vec h (nreverse result))))))

(defun %ht-rebuild-index (ht holder strcmp?)
  "Build the 256-bucket index from HT's current alist and store it in HOLDER's
   car.  If any key isn't structurally hashable, mark :NOHASH and bail (the
   table permanently falls back to the linear alist scan)."
  (let ((vec (%ht-new-bucket-vec)) (cur (car ht)) (ok t))
    (loop
      (when (null cur) (return nil))
      (let* ((pair (car cur)) (k (car pair)))
        (when (eq (%ht-hash k strcmp?) (%ht-nohash)) (setq ok nil) (return nil))
        (%ht-bucket-put vec k pair strcmp?))
      (setq cur (cdr cur)))
    (if ok
        (progn (%ht-h-set-vec holder vec) vec)
        (progn (%ht-h-set-vec holder (%ht-nohash)) (%ht-nohash)))))

(defun %ht-active-vec-h (ht holder)
  "Return the active bucket vector for HT (HOLDER already fetched), building it
   lazily once the table crosses the threshold.  Returns NIL when HT should use
   the alist path (still-small table, or a :NOHASH-disabled table)."
  (let ((v (%ht-h-vec holder)))
    (cond
      ((eq v (%ht-nohash)) nil)                 ; permanently disabled
      (v v)                                ; already built
      ((>= (%ht-h-count holder) (%ht-bucket-threshold))
       (let ((r (%ht-rebuild-index ht holder (%ht-h-strcmp holder))))
         (if (eq r (%ht-nohash)) nil r)))
      (t nil))))

(defun hash-table-count (ht)
  "Return the number of entries in HT."
  (let ((cur (car ht)) (n 0))
    (loop
      (when (null cur) (return n))
      (setq n (+ n 1))
      (setq cur (cdr cur)))))

(defun clrhash (ht)
  "Remove all entries from HT.  Returns HT.  Also resets the bucket index
   holder (vec -> nil, count -> 0) so a re-filled table re-indexes lazily."
  (set-car ht nil)
  (let ((holder (%ht-bucket-holder ht)))
    (when holder
      ;; Zero the count and reset the vec so a re-filled table re-indexes
      ;; lazily — but KEEP a :NOHASH-disabled table disabled (EQUALP tables,
      ;; and any table that earlier saw a non-bucketable key).  STRCMP? in
      ;; (cddr holder) is preserved.
      (unless (eq (%ht-h-vec holder) (%ht-nohash))
        (%ht-h-set-vec holder nil))
      (%ht-h-set-count holder 0)))
  ht)

;;; --- Metadata-aware constructor.  Kept as a separate name so the
;;; --- 0-arg make-hash-table above (called from many internal init
;;; --- sites) keeps its fixed arity — adding &rest there can interact
;;; --- badly with funcall-of-let-allocated-lambda (CLAUDE.md known bug).

(defun %ht-canonicalize-test (v)
  "Map :TEST argument V to one of EQ / EQL / EQUAL / EQUALP. Default EQL.

   Symbol path: native MVM symbols (subtag #x50, single hash slot)
   carry only their FNV-1a name-hash; SYMBOL-NAME returns \"\" for them
   so we have to compare the hash directly.  CL-symbols (cons-tagged)
   go through SYMBOL-NAME → STRING= as a fallback.

   Function path: when the source said #'EQ etc, compile-function-ref
   already rewrote that to the address of the corresponding %FOO-FN
   wrapper, and #'EQUALP loads the address of EQUALP itself.  EQL the
   FN-ADDR fixnums for an exact match."
  (cond
    ((null v) 'eql)
    ((%native-mvm-sym-p v)
     (let ((h (%native-mvm-sym-hash v)))
       (cond ((eql h 644866047583222547) 'eq)
             ((eql h 743927193407775751) 'eql)
             ((eql h 777630921077348411) 'equal)
             ((eql h 349037300549106995) 'equalp)
             (t 'eql))))
    ((symbolp v)
     (let ((n (symbol-name v)))
       (cond ((string= n "EQ") 'eq)
             ((string= n "EQL") 'eql)
             ((string= n "EQUAL") 'equal)
             ((string= n "EQUALP") 'equalp)
             (t 'eql))))
    ((eql v (function %eq-fn))    'eq)
    ((eql v (function %eql-fn))   'eql)
    ((eql v (function %equal-fn)) 'equal)
    ((eql v (function equalp))    'equalp)
    (t 'eql)))

(defun make-hash-table-args (options)
  "Internal worker: parse OPTIONS plist (`(:test eq :size 100 ...)`) and
   build a metadata-bearing hash-table.  Defaults: test=eql, rsize=2,
   rthresh=1, size=16."
  (let ((test 'eql) (rsize 2) (rthresh 1) (size 16) (cur options))
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur)) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :test)             (setq test (%ht-canonicalize-test v)))
          ((eq k :size)             (setq size v))
          ((eq k :rehash-size)      (setq rsize v))
          ((eq k :rehash-threshold) (setq rthresh v))))
      (setq cur (cddr cur)))
    ;; Metadata is a PROPER 5-list: (test rsize rthresh size bucket-holder).
    ;; bucket-holder = (vec-or-flag count . strcmp?) — a mutable cell that
    ;; gethash/puthash/remhash use to maintain an O(1) bucket index once the
    ;; table grows past %HT-BUCKET-THRESHOLD entries.  car nil = no index yet
    ;; (still small); car :NOHASH = bucketing disabled (EQUALP, or a key we
    ;; can't structure-hash was inserted); car <vector> = active index.  cddr
    ;; STRCMP? (precomputed: T for EQUAL tables) lets the hot path decide
    ;; string-by-content vs identity WITHOUT a per-op HASH-TABLE-TEST meta walk
    ;; or symbol compare — that overhead is what erased the bucket win.
    ;; The 0-arg / legacy MAKE-HASH-TABLE path (NIL test) does NOT get this
    ;; tail — it stays the pure-alist shape so boot-critical tables (the symbol
    ;; intern table, macro table, ...) are byte-identical and never bucketed.
    ;; EQUALP gets the index pre-disabled (car=:NOHASH): EQUALP string keys are
    ;; case-INSENSITIVE and EQUALP conflates e.g. 1/1.0, but the index is
    ;; EQUAL-grade — so EQUALP tables stay on the correct linear path.
    (let ((holder (cons (if (eq test 'equalp) (%ht-nohash) nil)
                        (cons 0 (eq test 'equal)))))
      (cons nil
            (cons (%ht-tag)
                  (list test rsize rthresh size holder))))))

;;; ============================================================
;;; Gensym (for macro expansion)
;;; ============================================================

(defvar *gensym-counter* 0)

(defun gensym (&optional prefix)
  "Generate a unique integer ID. PREFIX is ignored on bare metal.
   ANSI gensym takes &optional prefix; making this an &optional matches
   that and lets compile-call's arity check distinguish (gensym) (valid)
   from (gensym 1 2 3) (invalid)."
  (let ((n *gensym-counter*))
    (setq *gensym-counter* (+ *gensym-counter* 1))
    n))

;;; ============================================================
;;; String construction (for format replacement)
;;; ============================================================

(defun princ-to-string (obj)
  "Identity — on bare metal, values are used as-is."
  obj)

(defun string (x)
  "Identity — on bare metal, symbols are already name-hashes or strings."
  x)

;;; ============================================================
;;; Multiple Values (bare-metal stub)
;;; ============================================================
;;;
;;; values is a compiler special form (compile-values).
;;; This function handles the funcall/apply case: (apply #'values list).
;;; It manually sets the MV buffer and returns the primary value.
;;; The function epilogue does NOT emit set-mv-count 1 (special-cased in compiler).
(defun values (&rest args)
  "Return multiple values. Sets MV buffer for count and extra values."
  (let ((n (length args)))
    (setf (mem-ref #x10000090 :u64) n)
    (let ((cur (if (null args) nil (cdr args)))
          (idx 0))
      (loop
        (when (null cur) (return nil))
        (setf (mem-ref (+ #x10000098 (* idx 8)) :u64) (car cur))
        (setq idx (+ idx 1))
        (setq cur (cdr cur))))
    (if (null args) nil (car args))))

;;; ============================================================
;;; Global Variable Store (bare-metal)
;;; ============================================================
;;;
;;; The MVM compiler emits calls to SYMBOL-VALUE and SET-SYMBOL-VALUE
;;; for defvar/defparameter globals. On bare metal, we implement these
;;; using an alist stored at fixed address 0x600000.
;;; (Was 0x400000 but image+fn-table can extend past 0x400000 for large
;;; builds like fixpoint-ssh. 0x600000 is above page tables at 0x500000.)
;;; Keys are tagged name hashes (fixnums), values are arbitrary.
;;; mem-ref :u64 returns raw bits, which for tagged Lisp values is
;;; the value itself (cons pointers, fixnums, etc.).

;; The global special-variable store at #x10000080 is a HASH TABLE keyed by
;; name-hash (a 60-bit FIXNUM per compute-name-hash — so %ht-hash buckets it
;; O(1)).  It was a hand-rolled LINEAR ALIST until this commit: every global
;; read/write walked the whole chain (symbol-value/set-symbol-value/boundp),
;; and each non-matching node paid an EQL→%IEEE-FLOAT-P comparator call.  UIOP
;; defines hundreds of specials, so the walk cost grew with the corpus and the
;; asdf-gauntlet's big WITH-UPGRADABILITY blocks (which read/write many globals)
;; hit an O(#globals × #accesses) quadratic — minutes/form at forms ~99–109.
;; This is the SAME family as 194bbfb (unindexed alist) but a DIFFERENT site.
;; A 0-arg make-hash-table graduates to the O(1) 256-bucket index past its
;; threshold; the head slot is forwarded by GC exactly like the keyword table
;; (#x10000148) and pkg-by-hash table (#x10000170), which are already
;; hash-table objects at BSS slots — so NO GC change is needed.  (The store
;; head is a hash-table = a cons whose car is the alist and whose cdr carries
;; the %HT-TAG metadata; a value of raw 0 means "not yet created".)
(defun %globals-table ()
  "The globals hash-table object at #x10000080, or NIL if not yet created."
  (let ((head (mem-ref #x10000080 :u64)))
    (if (or (eql head 0) (not (consp head))) nil head)))

(defun symbol-value (name-or-hash)
  "Look up a global variable by name hash or symbol object.
   O(1) via the globals hash table at #x10000080."
  ;; CLHS: (symbol-value nil) ≡ nil, (symbol-value 't) ≡ t — constants
  ;; with no table entry needed.  Without this, (aref nil 0) would fault.
  (when (null name-or-hash) (return-from symbol-value nil))
  (when (eq name-or-hash t) (return-from symbol-value t))
  (let ((key (if (integerp name-or-hash) name-or-hash
                 (aref name-or-hash 0)))
        (tbl (%globals-table)))
    (if tbl (gethash key tbl) nil)))

(defun set-symbol-value (name-hash value)
  "Set a global variable by its tagged name hash.  O(1) via the globals
   hash table at #x10000080, created lazily on first use."
  (let ((tbl (%globals-table)))
    (unless tbl
      (setq tbl (make-hash-table))
      (setf (mem-ref #x10000080 :u64) tbl))
    (puthash name-hash tbl value)
    value))

;;; ============================================================
;;; Interned Symbols
;;; ============================================================
;;;
;;; Symbols are 1-slot objects with subtag #x50.
;;;   Slot 0: name-hash (tagged fixnum, dual-FNV-1a)
;;;
;;; The intern table at 0x620000 maps name-hash → symbol object.
;;; %intern-symbol is called by compile-quote for every quoted symbol
;;; and by the reader's mksym for every parsed symbol.
;;; Since intern returns the same object for the same name, eq works.

(defun init-symbol-table ()
  "Initialize the intern table at 0x620000."
  (setf (mem-ref #x10000088 :u64) (make-hash-table)))

(defun init-keyword-table ()
  "Initialize the keyword intern table at #x10000148.
   Distinct from the symbol table so KEYWORDP can identify keywords without
   a per-symbol package slot.  See compile-keyword in mvm/compiler.lisp."
  (setf (mem-ref #x10000148 :u64) (make-hash-table)))

(defun %intern-symbol (name-hash)
  "Legacy 1-arg intern.  Delegates to %INTERN-SYMBOL-PKG with pkg-hash=0
   (i.e. no known home package).  Kept for back-compat with anything
   that interns a symbol without knowing its package — gensyms,
   constructed names, the old read path before the reader started
   passing *package*.  All `'foo' literals in source compile through
   %INTERN-SYMBOL-PKG instead and arrive package-tagged."
  (%intern-symbol-pkg name-hash 0))


(defun %intern-symbol-pkg (name-hash pkg-hash)
  "Intern a symbol identified by (NAME-HASH, PKG-HASH).  Per CLHS
   11.1.2 — see SYMBOLS_PLAN.md — symbol identity is per-package: the
   same NAME-HASH in two different packages produces two distinct
   symbol objects.  This is a reversal of the prior unified model
   where `(eq cl-test::x ds4::x)' was T.

   Returns the canonical symbol for the (NAME-HASH, PKG-HASH) pair;
   multiple calls with the same pair return `eq' results.

   The symbol is always 3-slot [hash, package, name].  PKG-HASH = 0
   means `no home package' (uninterned gensyms): those still share by
   NAME-HASH alone via the single-key path so that
   `(eq (gensym) (gensym))' false but `(eq '#:G '#:G)' from the same
   literal holds.

   Storage: the symbol intern table at #x10000088 is keyed by a
   composite of NAME-HASH and PKG-HASH.  pkg-hash=0 uses the bare
   name-hash so the legacy uninterned path stays compatible.

   GC-safety: %ALLOC-SYM3 can trigger GC, which moves the hash table
   to to-space and updates the root slot at #x10000088.  Re-read the
   root AFTER the allocation."
  (setf (mem-ref #x10000C80 :u64) (+ (mem-ref #x10000C80 :u64) 1))
  ;; Composite key: name-hash for no-package syms (legacy / uninterned),
  ;; otherwise combine name-hash + pkg-hash so that the same name in two
  ;; packages keys to two distinct slots.  Multiplier is a large prime;
  ;; mod 2^62 keeps the result in fixnum range.  MUST use %fixnum-+ / %fixnum-*
  ;; (raw, wrapping :add / :mul): plain + and * now promote on overflow to a
  ;; BIGNUM, and in-image (logand <bignum> mask) is lossy — that produced an
  ;; inconsistent intern key and symbols failed to resolve ("implicit
  ;; global <param>").  The mask makes the wraparound irrelevant.
  (let ((key (if (= pkg-hash 0)
                 name-hash
                 (logand (%fixnum-+ name-hash (%fixnum-* pkg-hash 2305843009213693951))
                         #x3FFFFFFFFFFFFFFF))))
    (let ((table (mem-ref #x10000088 :u64)))
      (let ((existing (gethash key table)))
        (cond
          (existing
           ;; Same (name, pkg) pair — return cached.
           (setf (mem-ref #x10000C80 :u64) (- (mem-ref #x10000C80 :u64) 1))
           existing)
          (t
           (let ((sym (%alloc-sym3)))
             (aset sym 0 name-hash)
             ;; Look up the home package.  pkg-hash=0 means none.
             ;;
             ;; A NON-zero pkg-hash means compile-quote knew this literal's
             ;; home package at build time — it is a genuine interned
             ;; symbol, never a gensym (those pass pkg-hash=0).  But the
             ;; build-host package may not have a runtime counterpart in
             ;; the pkg-by-hash table: ANSI-test deftest bodies and our own
             ;; source are READ into host packages like MODUS.MVM /
             ;; COMMON-LISP-USER, only some of which are registered.  When
             ;; the lookup misses, leaving slot 1 NIL is WRONG: the printer
             ;; then treats the symbol as uninterned and emits `#:NAME`
             ;; (format-s, print-cons, print-array all expect bare `NAME`).
             ;; So a known-but-unresolved home package falls back to the
             ;; default user package (CL-USER) — the symbol IS interned,
             ;; just in a package whose hash we don't carry.  Genuine
             ;; uninterned symbols keep slot 1 = NIL via the pkg-hash=0
             ;; branch; keywords never reach here (they intern through
             ;; %INTERN-KEYWORD).
             (let ((pkg nil))
               (when (> pkg-hash 0)
                 (let ((pkg-tab (mem-ref #x10000170 :u64)))
                   (when pkg-tab
                     (setq pkg (gethash pkg-hash pkg-tab))
                     (when (null pkg)
                       ;; Unresolved known package — default to CL-USER,
                       ;; looked up from the same table by its canonical
                       ;; name hash (self-heals if registration changes).
                       (setq pkg (gethash 26532410810097741 pkg-tab))))))
               (aset sym 1 pkg))
             (aset sym 2 "")    ; name — SYMBOL-NAME reverse-fills via *SYM-NAME-TABLE*
             ;; Re-read the root — %ALLOC-SYM3 may have triggered GC.
             (let ((live-table (mem-ref #x10000088 :u64)))
               (puthash key live-table sym))
             (setf (mem-ref #x10000C80 :u64) (- (mem-ref #x10000C80 :u64) 1))
             sym)))))))

;;; Pkg-by-hash root.  We DON'T use a Lisp special variable because
;;; referencing `*pkg-by-hash*' from %INTERN-SYMBOL-PKG would recurse
;;; to intern that variable's name and blow the stack at boot.  The
;;; slot at #x10000170 holds a hash-table object; readers go through
;;; (mem-ref #x10000170 :u64) and trust the GC to walk the slot as a
;;; root (same convention as the symbol intern table at #x10000088).
(defun %init-pkg-by-hash ()
  (setf (mem-ref #x10000170 :u64) (make-hash-table)))

(defun %intern-keyword (name-hash)
  "Intern a keyword by name hash.  Same shape as %INTERN-SYMBOL but uses
   the keyword table at #x10000148 and allocates a #x53-subtag object so
   KEYWORDP can identify it.  Compile-keyword in compiler.lisp emits
   `(li v0 hash) (call %INTERN-KEYWORD)' for every `:foo' literal in
   compiled code; the runtime reader's `intern KEYWORD' path lands here
   too via cl-packages.lisp.  Eq across both routes is preserved by the
   shared name-hash → object table."
  (let ((table (mem-ref #x10000148 :u64)))
    (let ((existing (gethash name-hash table)))
      (if existing
          existing
          (let ((kw (%make-keyword-obj)))
            (aset kw 0 name-hash)
            ;; Re-read the root (see %intern-symbol comment) — GC during
            ;; %make-keyword-obj may have relocated the table.
            (let ((live-table (mem-ref #x10000148 :u64)))
              (puthash name-hash live-table kw))
            kw)))))

;;; ============================================================
;;; Error handling (bare-metal stubs)
;;; ============================================================
;;;
;;; On bare metal, errors halt the system. MVM-compiled code calls
;;; these with variable arity — extra args are silently ignored.

(defun error (msg &rest args)
  "Signal an error. If handler-case is active, longjmp to it.
   Otherwise print error indicator and halt.
   ANSI's error is (datum &rest args); we mirror that signature here
   in prelude so all later callers see the &rest version even if
   compiled before cl-conditions.lisp redefines this function."
  (declare (ignore args))
  (if (%error-handler-active-p)
      (%hc-longjmp)
      (progn
        (write-string-serial "ERR:")
        (write-char-serial 10)
        (halt))))

(defun warn (msg &rest args)
  "Print warning indicator. Extra args ignored on bare metal."
  (declare (ignore args))
  (write-string-serial "WARN:")
  (write-char-serial 10))

;; FORMAT lives in cl-printer.lisp.  A nil-stub at this location used
;; to mask the real prelude FORMAT above (L1010); both are then masked
;; in turn by cl-printer's.  Removed 2026-06-01 as part of the
;; redefinition audit.

;;; ============================================================
;;; Type checking stubs
;;; ============================================================

(defun typep (obj type)
  "Type checking stub — returns nil on bare metal."
  nil)

(defun type-of (obj)
  "Type-of stub — returns nil on bare metal."
  nil)

;;; ============================================================
;;; Hash / symbol stubs
;;; ============================================================

;; Dual-FNV-1a 60-bit hash. Must match the build-time definition in
;; cross.lisp so that a symbol's stored hash (set by the compiler at
;; compile-quote time) matches what (compute-name-hash "NAME") yields
;; at runtime — used by the package hash→name map to round-trip
;; native MVM symbols back to their name string.
(defun compute-name-hash (name-string)
  "Dual-FNV-1a hash for a name string. 60-bit collision-resistant."
  (let ((h1 2166136261) (h2 3735928559)
        (len (array-length name-string))
        (i 0))
    (loop
      (when (>= i len) (return nil))
      (let ((c (%prim-aref name-string i)))   ; raw char-CODE for arithmetic hash
        ;; Uppercase if lowercase ASCII letter (matches string-upcase
        ;; path in build-time version).
        (when (and (>= c 97) (<= c 122))
          (setq c (- c 32)))
        (setq h1 (logand (* (logxor h1 c) 16777619) #xFFFFFFFF))
        (setq h2 (logand (* (logxor h2 c) 805306457) #xFFFFFFFF)))
      (setq i (+ i 1)))
    (let ((combined (logior (ash (logand h1 #x3FFFFFFF) 30)
                            (logand h2 #x3FFFFFFF))))
      (if (zerop combined) 1 combined))))

(defun name-eq (sym name-string)
  "Runtime version of MODUS.HASH:NAME-EQ — true iff SYM's name matches
   NAME-STRING (case-insensitively, via the dual-FNV-1a hash).

   This MUST exist at runtime: the compiler's SETF macro expander
   (compiler.lisp ~1707) — which is extracted and compiled into the
   image so runtime EVAL can macroexpand SETF — dispatches every place
   type with `(name-eq (car place) \"CAR\"/\"GETHASH\"/...)`.  Without a
   runtime NAME-EQ the call resolved to %UNRESOLVED-FN (→ NIL), so NO
   place branch matched and `(setf (gethash k h) v)` fell through to the
   broken generic `SET-<accessor>` fallback (which errored), making
   runtime-EVAL `(setf (gethash k h) v)` signal instead of storing.

   Mirrors %EVAL-SYM-EQ: native MVM symbols (subtag #x50, single hash
   slot) carry only a hash — SYMBOL-NAME may return \"\" for them — so we
   compare the stored hash slot DIRECTLY to (compute-name-hash NAME-STRING).
   CL symbols (3-slot) and the gensym/string cases go through SYMBOL-NAME."
  (cond
    ((null sym) nil)
    ((eq sym t) nil)
    ((consp sym) nil)
    ((characterp sym) nil)
    ((%cl-sym-p sym)
     (let ((n (symbol-name sym)))
       (and n (eql (compute-name-hash n) (compute-name-hash name-string)))))
    ((%native-mvm-sym-p sym)
     (eql (aref sym 0) (compute-name-hash name-string)))
    ((integerp sym) nil)
    (t nil)))

(defun intern (name &rest pkg-arg)
  "Intern stub — returns the name hash on bare metal.
   ANSI's intern is (string &optional package). Use &rest here so the
   prelude version's signature matches cl-packages.lisp's later
   redefinition (and so callers compiled before that redefinition still
   pass arity check)."
  (declare (ignore pkg-arg))
  (if (integerp name)
      name
      (compute-name-hash name)))

(defun find-package (name)
  "Find-package stub — returns nil."
  nil)

(defun find-symbol (name)
  "Find-symbol stub — returns nil."
  nil)

;;; ============================================================
;;; Sequence utilities
;;; ============================================================

(defun subseq (seq start &rest end-arg)
  "Return a subsequence from START to END (END defaults to length)."
  (let ((end (if end-arg (car end-arg) (length seq))))
  ;; NIL is a (empty) list: (subseq nil 0 0) must return NIL, not #().
  ;; The consp-only test sent NIL down the array branch, returning an
  ;; empty VECTOR — which broke the in-image compiler's static-rest
  ;; pre-pack ((append (subseq args 0 0) ...) → TYPE-ERROR) for any
  ;; 0-arg call to a req=0 &rest/&key callee in the same eval2 module.
  (if (or (null seq) (consp seq))
      ;; List: build new list
      (let ((result nil)
            (cur (nthcdr start seq))
            (i start))
        (loop
          (when (or (null cur) (>= i end)) (return (nreverse result)))
          (setq result (cons (car cur) result))
          (setq cur (cdr cur))
          (setq i (+ i 1))))
      ;; Array: copy elements.  Preserve string-ness so (subseq "abc" 1)
      ;; returns a STRING, not a general char-vector (aref/aset now move
      ;; characters; %make-string-array keeps the result string-typed).
      (let* ((len (- end start))
             (result (if (stringp seq)
                         (%make-string-array len)
                         (make-array len))))
        (let ((i 0))
          (loop
            (when (= i len) (return result))
            (aset result i (aref seq (+ start i)))
            (setq i (+ i 1))))))))

(defun concatenate-strings (s1 s2)
  "Concatenate two strings (arrays of chars)."
  (let ((l1 (array-length s1))
        (l2 (array-length s2)))
    (let ((result (make-array (+ l1 l2)))
          (i 0))
      (loop
        (when (= i l1) (return nil))
        (aset result i (aref s1 i))
        (setq i (+ i 1)))
      (setq i 0)
      (loop
        (when (= i l2) (return nil))
        (aset result (+ l1 i) (aref s2 i))
        (setq i (+ i 1)))
      result)))

(defun make-list (n initial-element)
  "Create a list of N elements, each set to INITIAL-ELEMENT."
  (let ((result nil)
        (i 0))
    (loop
      (when (= i n) (return result))
      (setq result (cons initial-element result))
      (setq i (+ i 1)))))

;;; UIOP/OS environment-variable helpers.
;;;
;;; UIOP (vendor/asdf/asdf.lisp) defines GETENV and GETENVP inside a single
;;; (with-upgradability () (defun getenv …) (defsetf getenv …) (defun getenvp …))
;;; form.  Under runtime EVAL that whole form ABORTS on the (defsetf getenv …)
;;; — leaving GETENVP undefined — so every later UIOP form that calls GETENVP
;;; (detect-os, default-temporary-directory, argv0, configuration probing, …)
;;; signals UNDEFINED-FUNCTION (asdf gauntlet FAILFORM 87 / 109).  Pre-defining
;;; them here in the image makes them available regardless of whether that form
;;; completes, and matches UIOP's own #+modus behaviour: Modus has no
;;; environment access yet, so GETENV returns NIL and GETENVP therefore returns
;;; NIL.  If runtime EVAL of the UIOP form ever succeeds it re-installs
;;; identical-behaving definitions (last-defun-wins).
(defun getenv (x)
  "Query the environment, as in C getenv.  Modus has no environment access
   yet, so this always returns NIL (the variable is treated as unset)."
  (declare (ignorable x))
  nil)

(defun getenvp (x)
  "Predicate: returns the named environment variable's value when it is
   present and non-empty, else NIL.  See GETENV — on Modus the environment
   is empty, so this is always NIL."
  (let ((g (getenv x)))
    (and g (stringp g) (> (length g) 0) g)))
