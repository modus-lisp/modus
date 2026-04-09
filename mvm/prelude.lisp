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
  "Return the Nth element of LIST (0-indexed)."
  (let ((i 0)
        (cur list))
    (loop
      (when (null cur) (return nil))
      (when (= i n) (return (car cur)))
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

(defun nthcdr (n list)
  "Return the Nth cdr of LIST."
  (let ((i 0)
        (cur list))
    (loop
      (when (= i n) (return cur))
      (when (null cur) (return nil))
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

(defun last (list)
  "Return the last cons cell of LIST."
  (if (null list)
      nil
      (let ((cur list))
        (loop
          (when (null (cdr cur)) (return cur))
          (setq cur (cdr cur))))))

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

(defun append (list1 list2)
  "Append two lists."
  (append2 list1 list2))

(defun nconc (list1 list2)
  "Destructively append LIST2 to the end of LIST1."
  (if (null list1)
      list2
      (let ((tail (last list1)))
        (set-cdr tail list2)
        list1)))

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

(defun member (item list)
  "Return the tail of LIST starting from the first element EQL to ITEM."
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

(defun assoc (key alist)
  "Find the first pair in ALIST whose car is EQL to KEY."
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

(defun position (item seq)
  "Return the index of ITEM in SEQ (list or array, EQL test), or nil."
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

(defun mapcar (fn list)
  "Apply FN to each element of LIST."
  (mapcar1 fn list))

(defun some (fn list)
  "Return the first non-nil result of applying FN to elements of LIST."
  (let ((cur list))
    (loop
      (when (null cur) (return nil))
      (let ((result (funcall fn (car cur))))
        (when result (return result)))
      (setq cur (cdr cur)))))

(defun every (fn list)
  "Return T if FN returns non-nil for all elements of LIST."
  (let ((cur list))
    (loop
      (when (null cur) (return t))
      (when (null (funcall fn (car cur))) (return nil))
      (setq cur (cdr cur)))))

(defun reduce (fn list keyword initial-value)
  "Fold FN over LIST with INITIAL-VALUE. KEYWORD is ignored (bare-metal
   positional stand-in for CL :initial-value keyword)."
  (let ((acc initial-value)
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
  "Return the length of a proper list."
  (let ((count 0)
        (cur list))
    (loop
      (when (null cur) (return count))
      (setq count (+ count 1))
      (setq cur (cdr cur)))))

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

(defun apply (fn args)
  "Call FN with elements of ARGS as arguments. Supports 0-4 args."
  (if (null args)
      (funcall fn)
      (if (null (cdr args))
          (funcall fn (car args))
          (if (null (cddr args))
              (funcall fn (car args) (cadr args))
              (if (null (cdddr args))
                  (funcall fn (car args) (cadr args) (caddr args))
                  (funcall fn (car args) (cadr args) (caddr args) (cadddr args)))))))

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

(defun gensym (prefix)
  "Generate a unique integer ID. PREFIX is ignored on bare metal."
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
;;; MVM's multiple-value-bind expansion destructures via car/cdr,
;;; so values must return a cons cell. Callers not using m-v-b
;;; should destructure with car to get the primary value.

(defun values (a b)
  "Return multiple values as a cons cell (for m-v-b destructuring)."
  (cons a b))

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

(defun symbol-value (name-hash)
  "Look up a global variable by its tagged name hash."
  (let ((head (mem-ref #x600000 :u64)))
    (if (zerop head)
        nil
        (let ((cur head))
          (loop
            (when (null cur) (return nil))
            (let ((pair (car cur)))
              (when (eql (car pair) name-hash)
                (return (cdr pair))))
            (setq cur (cdr cur)))))))

(defun set-symbol-value (name-hash value)
  "Set a global variable by its tagged name hash."
  (let ((head (mem-ref #x600000 :u64)))
    (if (zerop head)
        (progn
          (setf (mem-ref #x600000 :u64)
                (cons (cons name-hash value) nil))
          value)
        (let ((cur head))
          (loop
            (when (null cur)
              (setf (mem-ref #x600000 :u64)
                    (cons (cons name-hash value) head))
              (return value))
            (let ((pair (car cur)))
              (when (eql (car pair) name-hash)
                (set-cdr pair value)
                (return value)))
            (setq cur (cdr cur)))))))

;;; ============================================================
;;; Error handling (bare-metal stubs)
;;; ============================================================
;;;
;;; On bare metal, errors halt the system. MVM-compiled code calls
;;; these with variable arity — extra args are silently ignored.

(defun error (msg)
  "Print error indicator and halt. Extra args ignored on bare metal."
  (write-string-serial "ERR:")
  (write-byte 10)
  (halt))

(defun warn (msg)
  "Print warning indicator. Extra args ignored on bare metal."
  (write-string-serial "WARN:")
  (write-byte 10))

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

(defun intern (name)
  "Intern stub — returns the name hash on bare metal."
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

(defun subseq (seq start end)
  "Return a subsequence from START to END."
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
            (setq i (+ i 1)))))))

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
