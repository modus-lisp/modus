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
          (when (consp pair)
            (let ((c (if key-fn (funcall key-fn (car pair)) (car pair))))
              (when (if test (funcall test key c) (eql key c))
                (return pair)))))
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

(defun string-equal (a b)
  "Compare two strings for equality, element by element."
  (let ((len-a (array-length a)))
    (if (= len-a (array-length b))
        (let ((i 0))
          (loop
            (when (= i len-a) (return t))
            (unless (= (aref a i) (aref b i))
              (return nil))
            (setq i (+ i 1))))
        nil)))

(defun equal (a b)
  "Structural equality: EQL for atoms, recursive for conses,
   element-wise for strings."
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
                  (string-equal a b)
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

(defun apply (fn &rest spread)
  "ANSI apply: (apply fn a1 a2 ... aN list) — call FN with the
   spread args followed by the elements of the final LIST.
   Special case: (apply fn list) is just (funcall fn list-elements)."
  ;; Build the full arg list: (a1 a2 ... aN) ++ final-list
  (let ((all-args
         (if (null spread)
             nil
             (if (null (cdr spread))
                 ;; (apply fn list) — spread = (list)
                 (car spread)
                 ;; (apply fn a1 a2 ... list) — append individual args + list
                 (let ((individual nil) (cur spread))
                   (loop
                     (when (null (cdr cur))
                       ;; last cur is the spread list; append it
                       (return (append (nreverse individual) (car cur))))
                     (setq individual (cons (car cur) individual))
                     (setq cur (cdr cur))))))))
    ;; Now dispatch on length of all-args (supports 0-8 args).
    (let ((n (length all-args)))
      (cond
        ((= n 0) (funcall fn))
        ((= n 1) (funcall fn (car all-args)))
        ((= n 2) (funcall fn (car all-args) (cadr all-args)))
        ((= n 3) (funcall fn (car all-args) (cadr all-args) (caddr all-args)))
        ((= n 4) (funcall fn (car all-args) (cadr all-args) (caddr all-args) (cadddr all-args)))
        ((= n 5) (funcall fn (nth 0 all-args) (nth 1 all-args) (nth 2 all-args)
                          (nth 3 all-args) (nth 4 all-args)))
        ((= n 6) (funcall fn (nth 0 all-args) (nth 1 all-args) (nth 2 all-args)
                          (nth 3 all-args) (nth 4 all-args) (nth 5 all-args)))
        ((= n 7) (funcall fn (nth 0 all-args) (nth 1 all-args) (nth 2 all-args)
                          (nth 3 all-args) (nth 4 all-args) (nth 5 all-args)
                          (nth 6 all-args)))
        (t (funcall fn (nth 0 all-args) (nth 1 all-args) (nth 2 all-args)
                       (nth 3 all-args) (nth 4 all-args) (nth 5 all-args)
                       (nth 6 all-args) (nth 7 all-args)))))))

;;; ============================================================
;;; Format stub (for self-compilation — writes string to serial)
;;; ============================================================

(defun write-string-serial (str)
  "Write a string to serial output, character by character."
  (let ((len (array-length str))
        (i 0))
    (loop
      (when (= i len) (return nil))
      (write-char-serial (aref str i))
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
        (write-char-serial 35) (write-char-serial 60) (write-char-serial 83)
        (print-dec (aref obj 0))
        (write-char-serial 62))
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
             (= (obj-subtag obj) #x60))
        ;; Boxed float (subtag #x60) — emit a placeholder so tests can
        ;; see the type name instead of #<?>. The underlying IEEE bits
        ;; live in slots 0 and 1 but we don't decimal-print them.
        (write-char-serial 35) (write-char-serial 60) (write-char-serial 70)
        (write-char-serial 62))   ; #<F>
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
   REHASH-SIZE REHASH-THRESHOLD`) is delegated to make-hash-table-args
   when any options were supplied; the no-arg path returns the cheap
   legacy (cons nil nil) shape that all internal callers rely on."
  (if (null options)
      (cons nil nil)
      (make-hash-table-args options)))

(defun gethash (key ht &optional default)
  "Look up KEY in hash table HT.  Returns (values value present-p);
   if not present, value is DEFAULT (nil if not supplied) and
   present-p is NIL.  We compute the (value present-p) pair as a cons
   inside the search loop and call `values' once at the tail — calling
   `values' inside `return' was losing the second value (loop+return
   appears to clobber MV-count on its way out)."
  (let ((found-pair nil))
    (let ((cur (car ht)))
      (loop
        (when (null cur) (return nil))
        (let ((pair (car cur)))
          (when (equal (car pair) key)
            (setq found-pair pair)
            (return nil)))
        (setq cur (cdr cur))))
    (if found-pair
        (values (cdr found-pair) t)
        (values default nil))))

(defun puthash (key ht value)
  "Set KEY to VALUE in hash table HT. Returns VALUE."
  (let ((cur (car ht)))
    (loop
      (when (null cur)
        (let ((new-pair (cons key value)))
          (set-car ht (cons new-pair (car ht))))
        (return value))
      (let ((pair (car cur)))
        (when (equal (car pair) key)
          (set-cdr pair value)
          (return value)))
      (setq cur (cdr cur)))))

(defun remhash (key ht)
  "Remove KEY from hash table HT. Returns T if removed, NIL otherwise."
  (let ((entries (car ht)))
    (if (null entries)
        nil
        (if (equal (car (car entries)) key)
            (progn
              (set-car ht (cdr entries))
              t)
            (let ((prev entries)
                  (cur (cdr entries)))
              (loop
                (when (null cur) (return nil))
                (when (equal (car (car cur)) key)
                  (set-cdr prev (cdr cur))
                  (return t))
                (setq prev cur)
                (setq cur (cdr cur))))))))

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
   Recognizes both the legacy (cons alist nil) and the modern
   (cons alist (cons %ht-tag meta)) shapes."
  (and (consp ht)
       (let ((c (cdr ht)))
         (or (null c)                              ; legacy
             (and (consp c) (eql (car c) (%ht-tag)))))))

(defun hash-table-test (ht)
  "Return the test designator for HT — one of EQ EQL EQUAL EQUALP.
   Legacy 0-arg tables report EQL (the ANSI default)."
  (let ((m (%ht-meta ht))) (if m (car m) 'eql)))

(defun hash-table-rehash-size (ht)
  "Return the rehash-size of HT (default 2)."
  (let ((m (%ht-meta ht)))
    (if (and m (consp (cdr m))) (car (cdr m)) 2)))

(defun hash-table-rehash-threshold (ht)
  "Return the rehash-threshold of HT (default 1)."
  (let ((m (%ht-meta ht)))
    (if (and m (consp (cdr m)) (consp (cddr m))) (car (cddr m)) 1)))

(defun hash-table-size (ht)
  "Return the declared size of HT (default 16)."
  (let ((m (%ht-meta ht)))
    (if (and m (consp (cdr m)) (consp (cddr m))) (cdr (cddr m)) 16)))

(defun hash-table-count (ht)
  "Return the number of entries in HT."
  (let ((cur (car ht)) (n 0))
    (loop
      (when (null cur) (return n))
      (setq n (+ n 1))
      (setq cur (cdr cur)))))

(defun clrhash (ht)
  "Remove all entries from HT.  Returns HT."
  (set-car ht nil)
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
    (cons nil
          (cons (%ht-tag)
                (cons test (cons rsize (cons rthresh size)))))))

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

(defun symbol-value (name-or-hash)
  "Look up a global variable by name hash or symbol object."
  (let ((key (if (integerp name-or-hash) name-or-hash
                 (aref name-or-hash 0)))  ; extract hash from symbol object
        (head (mem-ref #x10000080 :u64)))
    (if (zerop head)
        nil
        (let ((cur head))
          (loop
            (when (null cur) (return nil))
            (let ((pair (car cur)))
              (when (eql (car pair) key)
                (return (cdr pair))))
            (setq cur (cdr cur)))))))

(defun set-symbol-value (name-hash value)
  "Set a global variable by its tagged name hash."
  (let ((head (mem-ref #x10000080 :u64)))
    (if (zerop head)
        (progn
          (setf (mem-ref #x10000080 :u64)
                (cons (cons name-hash value) nil))
          value)
        (let ((cur head))
          (loop
            (when (null cur)
              (setf (mem-ref #x10000080 :u64)
                    (cons (cons name-hash value) head))
              (return value))
            (let ((pair (car cur)))
              (when (eql (car pair) name-hash)
                (set-cdr pair value)
                (return value)))
            (setq cur (cdr cur)))))))

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

(defun %intern-symbol (name-hash)
  "Intern a symbol by name hash. Returns existing symbol if already interned.
   Uses %make-symbol compiler builtin (ALLOC-OBJ subtag #x50) to allocate.

   NOTE on a known GC hazard kept-as-is for now: %make-symbol can trigger
   GC, which moves the hash table to to-space and updates the root slot
   at #x10000088.  Our local `table` binding is just a register/frame
   value (not GC-tracked) so it ends up pointing at the from-space copy,
   and the (puthash ... table sym) below writes the new entry into a
   table that gets reclaimed at the next collection.  The symptom is
   non-deterministic: future `'foo` references re-allocate a fresh
   symbol, eq between two `'foo` literals returns NIL, and CLOS marker
   checks ((eq (aref instance 0) '%clos-instance)) misclassify.

   The straightforward fix is to re-read the table from #x10000088 after
   %make-symbol.  When tried it shifted layout enough to net-regress
   the ANSI run by 159 tests (chunk-crash cascade in AREF.* / ARRAY.*),
   so it's not deployed here yet.  Revisit after the layout-stability
   work makes a one-instruction predicate-body change safe."
  (let ((table (mem-ref #x10000088 :u64)))
    (let ((existing (gethash name-hash table)))
      (if existing
          existing
          (let ((sym (%make-symbol)))
            (aset sym 0 name-hash)
            (puthash name-hash table sym)
            sym)))))

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

;;; ============================================================
;;; Format stub (bare-metal)
;;; ============================================================
;;;
;;; Full CL format is not available on bare metal.
;;; format with nil destination returns nil (no string construction).
;;; format with t destination writes nothing (stub).

(defun format (dest control-string)
  "Format stub — on bare metal, does nothing. Returns nil."
  nil)

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
      (let ((c (aref name-string i)))
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
  (if (consp seq)
      ;; List: build new list
      (let ((result nil)
            (cur (nthcdr start seq))
            (i start))
        (loop
          (when (or (null cur) (>= i end)) (return (nreverse result)))
          (setq result (cons (car cur) result))
          (setq cur (cdr cur))
          (setq i (+ i 1))))
      ;; Array: copy elements
      (let ((len (- end start))
            (result (make-array (- end start))))
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
