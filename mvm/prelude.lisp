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
   Signals TYPE-ERROR if N is not a non-negative fixnum."
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
   Accepts ANSI &key TEST/TEST-NOT/KEY via &rest (currently ignored)."
  (declare (ignore options))
  (when (and (not (null list)) (not (consp list)))
    (%signal-type-error))
  (let ((cur list))
    (loop
      (when (null cur) (return nil))
      (when (eql (car cur) item) (return cur))
      (setq cur (cdr cur)))))

(defun member-string (item list)
  "Return the tail of LIST starting from the first element STRING-EQUAL to ITEM."
  (let ((cur list))
    (loop
      (when (null cur) (return nil))
      (when (string-equal (car cur) item) (return cur))
      (setq cur (cdr cur)))))

(defun assoc (key alist &rest options)
  "Find the first pair in ALIST whose car is EQL to KEY.
   Accepts (and ignores) ANSI &key TEST/TEST-NOT/KEY options as &rest
   so callers using the keyword form don't trigger a too-many arity error."
  (declare (ignore options))
  (let ((cur alist))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (when (consp pair)
          (when (eql (car pair) key) (return pair)))
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
  "Return the index of ITEM in SEQ (list or array, EQL test), or nil.
   Accepts ANSI &key TEST/TEST-NOT/KEY/START/END via &rest (ignored)."
  (declare (ignore options))
  (if (consp seq)
      (position-in-list item seq)
      (if (null seq)
          nil
          ;; Array
          (let ((len (array-length seq))
                (i 0))
            (loop
              (when (= i len) (return nil))
              (when (eql (aref seq i) item) (return i))
              (setq i (+ i 1)))))))

(defun remove-if (pred list)
  "Return a new list with elements for which PRED returns non-nil removed.
   PRED must be a function (use with funcall)."
  (let ((result nil)
        (cur list))
    (loop
      (when (null cur) (return (nreverse result)))
      (unless (funcall pred (car cur))
        (setq result (cons (car cur) result)))
      (setq cur (cdr cur)))))

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
  "Apply FN to each element of LIST (and MORE-LISTS if provided)."
  (if (null more-lists)
      (mapcar1 fn list)
      (let ((result nil)
            (lists (cons list more-lists)))
        (loop
          (when (some #'null lists) (return (nreverse result)))
          (setq result (cons (apply fn (mapcar1 #'car lists)) result))
          (setq lists (mapcar1 #'cdr lists))))))

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

(defun reduce (fn list &rest args)
  "Fold FN over LIST. Optional :initial-value (taken positionally as
   args[1] when args[0] is the keyword stand-in). Matches the existing
   call sites that pass either (reduce fn list) or
   (reduce fn list :initial-value v)."
  (let* ((init (cond
                 ((null args) nil)
                 ((cdr args) (cadr args))   ; (... :initial-value V)
                 (t nil)))
         (acc init)
         (cur list))
    (loop
      (when (null cur) (return acc))
      (setq acc (funcall fn acc (car cur)))
      (setq cur (cdr cur)))))

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
   Uses tortoise-and-hare cycle detection."
  (let ((n 0)
        (fast list)
        (slow list))
    (loop
      ;; Fast pointer moves 2 steps
      (when (null fast) (return n))
      (when (atom fast) (return n))
      (setq fast (cdr fast))
      (setq n (+ n 1))
      (when (null fast) (return n))
      (when (atom fast) (return n))
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

(defun length (seq)
  "Return the length of SEQ (list or array)."
  (if (consp seq)
      (list-length seq)
      (if (null seq)
          0
          ;; Array: read element-count from header
          (array-length seq))))

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
  "Return a new string with all characters uppercased."
  (let ((len (array-length str))
        (result (make-array (array-length str))))
    (let ((i 0))
      (loop
        (when (= i len) (return result))
        (let ((ch (aref str i)))
          (aset result i (char-upcase ch)))
        (setq i (+ i 1))))))

(defun string-downcase (str)
  "Return a new string with all characters lowercased."
  (let ((len (array-length str))
        (result (make-array (array-length str))))
    (let ((i 0))
      (loop
        (when (= i len) (return result))
        (let ((ch (aref str i)))
          (aset result i (char-downcase ch)))
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
    ;; Now dispatch on length of all-args (supports 0-4 args).
    (if (null all-args)
        (funcall fn)
        (if (null (cdr all-args))
            (funcall fn (car all-args))
            (if (null (cddr all-args))
                (funcall fn (car all-args) (cadr all-args))
                (if (null (cdddr all-args))
                    (funcall fn (car all-args) (cadr all-args) (caddr all-args))
                    (funcall fn (car all-args) (cadr all-args) (caddr all-args) (cadddr all-args))))))))

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
       (t
        (write-char-serial 35) (write-char-serial 60) (write-char-serial 63) (write-char-serial 62))))))

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
;;; (key . value) pairs. Keys compared using equal.
;;; O(n) lookup — sufficient for fixpoint proof.

(defun make-hash-table ()
  "Create an empty hash table (wrapper cons cell)."
  (cons nil nil))

(defun gethash (key ht)
  "Look up KEY in hash table HT. Returns value or nil."
  (let ((cur (car ht)))
    (loop
      (when (null cur) (return nil))
      (let ((pair (car cur)))
        (when (equal (car pair) key)
          (return (cdr pair))))
      (setq cur (cdr cur)))))

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
   Uses %make-symbol compiler builtin (ALLOC-OBJ subtag #x50) to allocate."
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
;;; Intern / symbol stubs
;;; ============================================================

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
