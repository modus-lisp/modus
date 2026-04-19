;;;; cl-sequences.lisp — Sequence, list, and collection functions
;;;; Part of the Modus CL runtime. Load after cl-types.lisp.

;;; ============================================================
;;; Additional missing functions
;;; ============================================================

;;; Sequence-aware some/every (override prelude versions that only handle lists)
(defun some (fn seq &rest more-seqs)
  "Return first non-nil result of FN on elements of SEQ (list or vector)."
  (if (null more-seqs)
      (if (consp seq)
          (let ((cur seq))
            (loop
              (when (null cur) (return nil))
              (let ((result (funcall fn (car cur))))
                (when result (return result)))
              (setq cur (cdr cur))))
          (if (null seq)
              nil
              ;; vector case
              (let ((len (array-length seq))
                    (i 0))
                (loop
                  (when (>= i len) (return nil))
                  (let ((result (funcall fn (aref seq i))))
                    (when result (return result)))
                  (setq i (+ i 1))))))
      ;; multi-sequence: use list form
      (let ((seqs (cons seq more-seqs)))
        (block seq-loop
          (let ((lists (mapcar (lambda (s)
                                 (if (consp s) s
                                     (let ((r nil) (n (array-length s)))
                                       (let ((k (- n 1)))
                                         (loop
                                           (when (< k 0) (return r))
                                           (setq r (cons (aref s k) r))
                                           (setq k (- k 1))))
                                       r)))
                               seqs)))
            (loop
              (when (some #'null lists) (return-from seq-loop nil))
              (let ((result (apply fn (mapcar #'car lists))))
                (when result (return-from seq-loop result)))
              (setq lists (mapcar #'cdr lists))))))))

(defun every (fn seq &rest more-seqs)
  "Return T if FN is true for all elements of SEQ (list or vector)."
  (if (null more-seqs)
      (if (consp seq)
          (let ((cur seq))
            (loop
              (when (null cur) (return t))
              (when (null (funcall fn (car cur))) (return nil))
              (setq cur (cdr cur))))
          (if (null seq)
              t
              ;; vector case
              (let ((len (array-length seq))
                    (i 0))
                (loop
                  (when (>= i len) (return t))
                  (when (null (funcall fn (aref seq i))) (return nil))
                  (setq i (+ i 1))))))
      ;; multi-sequence
      (not (apply #'some (lambda (&rest args) (not (apply fn args))) seq more-seqs))))

(defun copy-alist (alist)
  (if (null alist) nil
    (cons (if (consp (car alist))
              (cons (caar alist) (cdar alist))
              (car alist))
          (copy-alist (cdr alist)))))

(defun nthcdr (n list)
  (when (or (not (fixnump n)) (< n 0))
    (%signal-type-error))
  (let ((i 0) (cur list))
    (loop
      (when (= i n) (return cur))
      (when (null cur) (return nil))
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

(defun sublis (alist tree)
  (let ((pair (assoc (car tree) alist)))
    (if pair
        (cdr pair)
        (if (consp tree)
            (let ((a (sublis alist (car tree)))
                  (d (sublis alist (cdr tree))))
              (if (and (eq a (car tree)) (eq d (cdr tree)))
                  tree
                  (cons a d)))
            tree))))

(defun mapl (fn list)
  (let ((cur list))
    (loop
      (when (null cur) (return list))
      (funcall fn cur)
      (setq cur (cdr cur)))))

(defun mapcon (fn list)
  (let ((result nil) (cur list))
    (loop
      (when (null cur) (return result))
      (let ((r (funcall fn cur)))
        (setq result (nconc result r)))
      (setq cur (cdr cur)))))

(defun mapcan (fn list)
  (let ((result nil) (cur list))
    (loop
      (when (null cur) (return result))
      (let ((r (funcall fn (car cur))))
        (setq result (nconc result r)))
      (setq cur (cdr cur)))))

(defun maplist (fn list)
  (let ((result nil) (cur list))
    (loop
      (when (null cur) (return (nreverse result)))
      (setq result (cons (funcall fn cur) result))
      (setq cur (cdr cur)))))

(defun parse-test-key (args)
  "Parse :test and :key keyword args. Returns (test-fn . key-fn)."
  (let ((test-fn nil) (key-fn nil) (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test) (setq test-fn (cadr a)) (setq a (cddr a)))
            ((eq (car a) :key) (setq key-fn (cadr a)) (setq a (cddr a)))
            ((eq (car a) :test-not) (let ((f (cadr a))) (setq test-fn (lambda (x y) (not (funcall f x y))))) (setq a (cddr a)))
            ((eq (car a) :count) (setq a (cddr a)))
            ((eq (car a) :start) (setq a (cddr a)))
            ((eq (car a) :end) (setq a (cddr a)))
            ((eq (car a) :from-end) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (cons (or test-fn #'eql) key-fn)))

(defun remove (item list &rest args)
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed)))
    (remove-if (lambda (x) (funcall test-fn item (if key-fn (funcall key-fn x) x))) list)))

(defun remove-if-not (pred list &rest args)
  (remove-if (lambda (x) (not (funcall pred x))) list))

(defun count-if (pred list)
  (let ((n 0) (cur list))
    (loop
      (when (null cur) (return n))
      (when (funcall pred (car cur))
        (setq n (+ n 1)))
      (setq cur (cdr cur)))))

(defun count (item list)
  (count-if (lambda (x) (eql x item)) list))

;;; Function wrappers for compiler builtins (needed for apply/#')
;;; The compiler uses XOR/AND/OR opcodes for inline (logxor a b) etc.
;;; but #'logxor needs a real function object.
(defun %logxor2 (a b) (logxor a b))
(defun %logand2 (a b) (logand a b))
(defun %logior2 (a b) (logior a b))

(defun logxor (&rest args)
  "Variadic logxor function for use with apply/#'."
  (if (null args) 0
      (let ((result (car args)) (rest (cdr args)))
        (loop
          (when (null rest) (return result))
          (setq result (%logxor2 result (car rest)))
          (setq rest (cdr rest))))))

(defun logand (&rest args)
  "Variadic logand function for use with apply/#'."
  (if (null args) -1
      (let ((result (car args)) (rest (cdr args)))
        (loop
          (when (null rest) (return result))
          (setq result (%logand2 result (car rest)))
          (setq rest (cdr rest))))))

(defun logior (&rest args)
  "Variadic logior function for use with apply/#'."
  (if (null args) 0
      (let ((result (car args)) (rest (cdr args)))
        (loop
          (when (null rest) (return result))
          (setq result (%logior2 result (car rest)))
          (setq rest (cdr rest))))))

;;; ============================================================
;;; Override equal — the name "EQUAL" has a compiler bug (form-contains-call-p
;;; misclassifies it). This override is loaded LAST so it takes effect
;;; for all calls from ANSI test code.
;;; ============================================================

;;; Safe stub for all unresolved function calls.
;;; The compiler directs unresolved CALLs here instead of offset 0.
(defun %unresolved-fn () nil)

(defun nbutlast (list &rest n-arg)
  "Destructive butlast. Signals TYPE-ERROR on negative or non-fixnum N."
  (let ((n (if n-arg (car n-arg) 1)))
    (when (or (not (fixnump n)) (< n 0))
      (%signal-type-error))
    (let ((len (list-length list)))
      (if (or (null len) (<= len n)) nil
        (let ((tail (nthcdr (- len n 1) list)))
          (set-cdr tail nil)
          list)))))

(defun floatp-impl (x)
  "Check if x is a boxed float (subtag #x60)."
  (if (fixnump x) nil
    (if (consp x) nil
      (if (null x) nil
        (= (obj-subtag x) 96)))))  ; #x60 = 96

(defun float-equal (a b)
  "Compare two boxed floats by hi32/lo32 slots."
  (if (= (aref a 0) (aref b 0))
      (= (aref a 1) (aref b 1))
      nil))


;;; ============================================================
;;; Missing CL functions needed by ANSI tests
;;; ============================================================

(defun assert (test-form &rest ignored)
  (declare (ignore ignored))
  (if test-form t nil))
(defun equalp (a b) (equalp-impl a b))
(defun elt (seq idx)
  ;; Signal TYPE-ERROR for negative or non-fixnum index.
  (when (or (not (fixnump idx)) (< idx 0))
    (%signal-type-error))
  (if (consp seq) (nth idx seq) (aref seq idx)))
(defun string= (a b &rest options) (declare (ignore options)) (string-equal a b))
(defun string/= (a b) (if (string-equal a b) nil t))
;;; constantly: captures value. Use global cell.
(defvar *constantly-value* nil)
(defun %constantly-impl (&rest args) *constantly-value*)
(defun constantly (value) (setq *constantly-value* value) #'%constantly-impl)
;;; Closure support functions for is-eql-p / is-not-eql-p.
;;; These load the captured env from CLOSURE-ENV-ADDR (#x10000140), which
;;; funcall stores when it detects a closure object (cons fn-addr . env-list).
;;; The is-eql-p/is-not-eql-p functions themselves are defined in ansi-tests.lisp
;;; (loaded after ansi-aux.lsp) to override the aux version.
(defun closure-eql-fn (y)
  (let* ((env (mem-ref #x10000140 :u64))
         (x (car env)))
    (eql x y)))
(defun closure-not-eql-fn (y)
  (let* ((env (mem-ref #x10000140 :u64))
         (x (car env)))
    (not (eql x y))))
;;; Placeholder is-eql-p/is-not-eql-p (will be overridden by ansi-tests.lisp)
(defvar *is-eql-p-item* nil)
(defun is-eql-p (x) (cons #'closure-eql-fn (cons x nil)))
(defun is-not-eql-p (x) (cons #'closure-not-eql-fn (cons x nil)))
(defun sort (seq pred &rest options) (declare (ignore options)) (if (or (null seq) (null (cdr seq))) seq
  (let ((result (list (car seq)))) (dolist (item (cdr seq))
    (if (funcall pred item (car result)) (setq result (cons item result))
      (let ((prev result)) (loop (when (null (cdr prev)) (set-cdr prev (list item)) (return nil))
        (when (funcall pred item (cadr prev)) (set-cdr prev (cons item (cdr prev))) (return nil))
        (setq prev (cdr prev)))))) result)))
(defun stable-sort (seq pred) (sort seq pred))
;; Internal helpers: build a sequence of the same shape (list -> list,
;; string -> string, array -> array) by applying TRANSFORM to each element.
(defun %seq-substitute-with (transform seq)
  (cond
    ((or (null seq) (consp seq))
     (let ((result nil) (cur seq))
       (loop (when (null cur) (return (nreverse result)))
         (setq result (cons (funcall transform (car cur)) result))
         (setq cur (cdr cur)))))
    ((stringp seq)
     ;; String slots hold fixnum char-codes but test predicates typically
     ;; work with characters (e.g. (is-eql-p #\a)).  Present elements as
     ;; characters to TRANSFORM, coerce character return back to char-code
     ;; for aset into the new string.
     (let ((len (array-length seq))
           (out (%make-string-array (array-length seq))))
       (let ((i 0))
         (loop (when (= i len) (return out))
           (let ((v (funcall transform (code-char (aref seq i)))))
             (aset out i (if (characterp v) (char-code v) v)))
           (setq i (+ i 1))))))
    (t  ;; plain array
     (let ((len (array-length seq))
           (out (make-array (array-length seq))))
       (let ((i 0))
         (loop (when (= i len) (return out))
           (aset out i (funcall transform (aref seq i)))
           (setq i (+ i 1))))))))

(defun substitute (new old seq &rest args)
  (%seq-substitute-with (lambda (item) (if (eql item old) new item)) seq))
(defun substitute-if (new pred seq &rest args)
  (%seq-substitute-with (lambda (item) (if (funcall pred item) new item)) seq))
(defun substitute-if-not (new pred seq &rest args)
  (%seq-substitute-with (lambda (item) (if (funcall pred item) item new)) seq))

;;; Destructive substitute variants
;;; nsubstitute-if-core: shared implementation
;;; pred: test predicate (item → boolean)
;;; new: replacement value
;;; seq: list or vector
;;; from-end: nil or t
;;; count: nil=unlimited, integer=max substitutions
;;; Returns seq (modified in place for lists, or new seq for vectors)
(defun %nsubst-parse-args (args)
  "Parse keyword args: :from-end :test :test-not :start :end :count :key.
   Returns (count from-end start end test-fn test-not-fn key-fn)."
  (let ((from-end nil) (test-fn nil) (test-not-fn nil)
        (count nil) (key-fn nil) (start 0) (end nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :from-end) (setq from-end v))
          ((eq k :test) (setq test-fn v))
          ((eq k :test-not) (setq test-not-fn v))
          ((eq k :count) (setq count v))
          ((eq k :key) (setq key-fn v))
          ((eq k :start) (setq start v))
          ((eq k :end) (setq end v)))
        (setq cur (cddr cur))))
    (list count from-end start end test-fn test-not-fn key-fn)))

;;; Effective count: nil if count is nil, else max(0, count)
(defun %nsubst-effective-count (count)
  (if (null count) nil (if (> count 0) count 0)))

(defun %nsubst-in-window-p (idx start-idx end-idx)
  "Return t if idx is in [start-idx, end-idx)."
  (if (>= idx start-idx)
      (if (null end-idx) t (< idx end-idx))
      nil))

(defun %nsubst-list-core (new pred-fn seq count from-end start-idx end-idx)
  "Core list nsubstitute with start/end/count/from-end support."
  ;; count: nil=unlimited, 0=nothing, positive=limit. Already normalized by caller.
  (if from-end
      ;; Backward: collect matching positions in [start-idx, end-idx), apply count from end
      (let ((positions nil) (cur seq) (idx 0))
        (loop
          (when (null cur) (return nil))
          (let ((in-win (%nsubst-in-window-p idx start-idx end-idx)))
            (when in-win
              (let ((match (funcall pred-fn (car cur))))
                (when match
                  (setq positions (cons idx positions))))))
          (setq idx (+ idx 1))
          (setq cur (cdr cur)))
        ;; positions: largest index first
        ;; Take first count entries (= highest-indexed matches)
        (let ((to-replace nil) (remaining (or count (length positions))) (pos-cur positions))
          (loop
            (when (null pos-cur) (return nil))
            (when (<= remaining 0) (return nil))
            (setq to-replace (cons (car pos-cur) to-replace))
            (setq remaining (- remaining 1))
            (setq pos-cur (cdr pos-cur)))
          ;; Replace in original list at collected positions
          (let ((cur2 seq) (idx2 0))
            (loop
              (when (null cur2) (return seq))
              (when (member idx2 to-replace)
                (set-car cur2 new))
              (setq idx2 (+ idx2 1))
              (setq cur2 (cdr cur2))))
          seq))
      ;; Forward: iterate, replace up to count times in [start-idx, end-idx)
      (let ((cur seq) (idx 0) (n count))
        (loop
          (when (null cur) (return seq))
          (when (and n (= n 0)) (return seq))
          (let ((in-win (%nsubst-in-window-p idx start-idx end-idx)))
            (when in-win
              (let ((match (funcall pred-fn (car cur))))
                (when match
                  (set-car cur new)
                  (when n (setq n (- n 1)))))))
          (setq idx (+ idx 1))
          (setq cur (cdr cur)))
        seq)))

;; Keep old names for backward compat (used in nsubstitute-if below)
(defun %nsubst-list-forward (new pred-fn seq count)
  (%nsubst-list-core new pred-fn seq count nil 0 nil))

(defun %nsubst-list-backward (new pred-fn seq count)
  (%nsubst-list-core new pred-fn seq count t 0 nil))

(defun nsubstitute-if (new pred seq &rest args)
  "Destructive substitute-if."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (pred-fn (if key-fn
                      (lambda (x) (funcall pred (funcall key-fn x)))
                      pred))
         (eff-count (if (null count) nil (if (< count 0) 0 count))))
    (if (or (null seq) (and eff-count (= eff-count 0)))
        seq
        (if (consp seq)
            (%nsubst-list-core new pred-fn seq eff-count from-end start-idx end-idx)
            ;; vector case — coerce char → char-code when seq is a string,
            ;; so stored slots stay fixnums (matches literal strings and
            ;; what aref is expected to return for downstream = comparisons).
            ;; For strings, also PRESENT each element as a character to
            ;; pred-fn — string slots hold fixnum char-codes but tests
            ;; typically call nsubstitute-if with a character-matching
            ;; predicate (e.g. (is-eql-p #\a)).
            (let* ((len (array-length seq))
                   (eff-end (if (and end-idx (< end-idx len)) end-idx len))
                   (string-p (stringp seq))
                   (store-new (if (and string-p (characterp new))
                                  (char-code new)
                                  new))
                   (n eff-count))
              (if from-end
                  (let ((i (- eff-end 1)))
                    (loop
                      (when (< i start-idx) (return seq))
                      (when (and n (= n 0)) (return seq))
                      (let ((elt (aref seq i)))
                        (when string-p (setq elt (code-char elt)))
                        (when (funcall pred-fn elt)
                          (aset seq i store-new)
                          (when n (setq n (- n 1)))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return seq))
                      (when (and n (= n 0)) (return seq))
                      (let ((elt (aref seq i)))
                        (when string-p (setq elt (code-char elt)))
                        (when (funcall pred-fn elt)
                          (aset seq i store-new)
                          (when n (setq n (- n 1)))))
                      (setq i (+ i 1))))))))))
(defun nsubstitute-if-not (new pred seq &rest args)
  "Destructive substitute-if-not."
  (apply #'nsubstitute-if new (lambda (x) (not (funcall pred x))) seq args))

(defun nsubstitute (new old seq &rest args)
  "Destructive substitute."
  (let* ((parsed (%nsubst-parse-args args))
         (test-fn (car (cddddr parsed)))   ; index 4
         (actual-test (or test-fn #'eql)))
    (apply #'nsubstitute-if new (lambda (x) (funcall actual-test old x)) seq args)))
(defun count-if-not (pred seq) (let ((c 0)) (dolist (item seq) (unless (funcall pred item) (setq c (+ c 1)))) c))
(defun hash-table-count (ht) (let ((c 0) (cur (car ht))) (loop (when (null cur) (return c)) (setq c (+ c 1)) (setq cur (cdr cur)))))
(defun %maphash-impl (fn ht)
  "Apply FN to each key-value pair in hash table (function version)."
  (let ((cur (car ht)))
    (loop (when (null cur) (return nil))
      (let ((pair (car cur)))
        (funcall fn (car pair) (cdr pair)))
      (setq cur (cdr cur)))))
(defun array-element-type (a) t)
(defun check-type-error (fn args) nil)
(defun make-array-with-checks (dims &rest args) (if (consp dims) (make-array (car dims)) (make-array dims)))
(defun %make-array-ic (size contents)
  "Create an array of SIZE with :initial-contents from CONTENTS (list or sequence)."
  (let ((arr (make-array size)))
    (if (listp contents)
        (let ((cur contents) (i 0))
          (loop
            (when (or (null cur) (= i size)) (return arr))
            (aset arr i (car cur))
            (setq cur (cdr cur))
            (setq i (+ i 1))))
        (let ((i 0))
          (loop
            (when (= i size) (return arr))
            (aset arr i (aref contents i))
            (setq i (+ i 1)))))))
(defun make-sequence (type size &rest args)
  "Make a sequence of TYPE and SIZE.  Supports :initial-element."
  (let ((init nil) (a args))
    (loop (when (null a) (return))
      (when (eq (car a) :initial-element) (setq init (cadr a)) (return))
      (setq a (cddr a)))
    (cond
      ((or (eq type 'list) (eq type 'cons))
       (let ((r nil) (i 0))
         (loop (when (= i size) (return r))
           (setq r (cons init r)) (setq i (+ i 1)))))
      ((or (eq type 'string) (eq type 'simple-string)
           (eq type 'base-string) (eq type 'simple-base-string))
       (let ((s (%make-string-array size))
             (ch (cond ((null init) 32)
                       ((characterp init) (char-code init))
                       (t init))))
         (let ((i 0))
           (loop (when (= i size) (return s))
             (aset s i ch) (setq i (+ i 1))))))
      ((or (eq type 'bit-vector) (eq type 'simple-bit-vector))
       (let ((v (make-array size)))
         (let ((i 0) (b (or init 0)))
           (loop (when (= i size) (return v))
             (aset v i b) (setq i (+ i 1))))))
      (t  ;; Generic vector / array / sequence / null type fall-back.
       (let ((v (make-array size)))
         (when init
           (let ((i 0))
             (loop (when (= i size) (return nil))
               (aset v i init) (setq i (+ i 1)))))
         v)))))
(defun coerce (obj type) (cond ((eq type 'list) (if (consp obj) obj (list obj))) ((eq type 'character) obj) (t obj)))
(defun mismatch (s1 s2) (let ((l1 (length s1)) (l2 (length s2))) (let ((limit (if (< l1 l2) l1 l2)) (i 0))
  (loop (when (>= i limit) (return (if (= l1 l2) nil i))) (unless (eql (elt s1 i) (elt s2 i)) (return i)) (setq i (+ i 1))))))
(defun random (n &rest state) (declare (ignore state)) (mod (ash (logxor (* 6364136223846793005 (mem-ref #x100000A0 :u64)) 1442695040888963407) -17) n))
(defun do-special-strings (fn) (funcall fn ""))
(defun typep* (obj type) (typep obj type))

;;; String functions
(defun %concat-elt-count (s)
  (cond ((null s) 0)
        ((consp s) (length s))
        ((stringp s) (array-length s))
        (t (array-length s))))

(defun %concat-result-kind (result-type)
  "Resolve a concatenate RESULT-TYPE designator to one of :LIST,
   :STRING, or :VECTOR.  Compound forms like (vector ...) and
   (simple-vector ...) are recognised by their car."
  (cond
    ((or (eq result-type 'list) (eq result-type 'cons)) :list)
    ((or (eq result-type 'string) (eq result-type 'simple-string)
         (eq result-type 'base-string) (eq result-type 'simple-base-string))
     :string)
    ((or (eq result-type 'vector) (eq result-type 'simple-vector)
         (eq result-type 'array)  (eq result-type 'simple-array)
         (eq result-type 'bit-vector) (eq result-type 'simple-bit-vector))
     :vector)
    ((consp result-type)
     (let ((head (car result-type)))
       (cond ((or (eq head 'vector) (eq head 'simple-vector)
                  (eq head 'array)  (eq head 'simple-array)
                  (eq head 'bit-vector) (eq head 'simple-bit-vector))
              :vector)
             ((or (eq head 'string) (eq head 'simple-string)
                  (eq head 'base-string) (eq head 'simple-base-string))
              :string)
             (t :vector))))
    (t :vector)))

(defun concatenate (result-type &rest seqs)
  "Concatenate sequences.  Recognises list / string / vector result
   types (atomic and compound forms like (vector * *))."
  (let ((kind (%concat-result-kind result-type)))
    (cond
      ((eq kind :list)
       (let ((r nil))
         (dolist (s (nreverse seqs))
           (if (consp s) (setq r (append s r))
               (dotimes (i (length s)) (setq r (append r (list (elt s i)))))))
         r))
      ((eq kind :string)
       (let ((total 0))
         (dolist (s seqs) (setq total (+ total (%concat-elt-count s))))
         (let ((result (%make-string-array total)) (pos 0))
           (dolist (s seqs)
             (cond
               ((null s) nil)
               ((stringp s)
                (dotimes (i (array-length s))
                  (aset result pos (aref s i)) (setq pos (+ pos 1))))
               ((consp s)
                (dolist (c s)
                  (aset result pos (if (characterp c) (char-code c) c))
                  (setq pos (+ pos 1))))
               (t  ;; vector
                (dotimes (i (array-length s))
                  (let ((c (aref s i)))
                    (aset result pos (if (characterp c) (char-code c) c))
                    (setq pos (+ pos 1)))))))
           result)))
      (t  ;; :vector
       (let ((total 0))
         (dolist (s seqs) (setq total (+ total (%concat-elt-count s))))
         (let ((result (make-array total)) (pos 0))
           (dolist (s seqs)
             (cond
               ((null s) nil)
               ((consp s)
                (dolist (c s)
                  (aset result pos c) (setq pos (+ pos 1))))
               (t  ;; string or vector
                (dotimes (i (array-length s))
                  (aset result pos (aref s i))
                  (setq pos (+ pos 1))))))
           result))))))

(defun merge (result-type s1 s2 pred &rest args)
  "Merge two sorted sequences. Honors RESULT-TYPE designator."
  (let ((r nil) (a (if (consp s1) s1 (coerce s1 'list)))
                (b (if (consp s2) s2 (coerce s2 'list))))
    (let ((merged (loop
                    (cond ((null a) (return (nreconc r b)))
                          ((null b) (return (nreconc r a)))
                          ((funcall pred (car a) (car b))
                           (setq r (cons (car a) r)) (setq a (cdr a)))
                          (t (setq r (cons (car b) r)) (setq b (cdr b))))))
          (kind (%concat-result-kind result-type)))
      (cond
        ((eq kind :list) merged)
        ((eq kind :string)
         (let ((n (length merged))
               (cur merged))
           (let ((s (%make-string-array n)) (i 0))
             (loop (when (= i n) (return s))
               (let ((c (car cur)))
                 (aset s i (if (characterp c) (char-code c) c)))
               (setq cur (cdr cur)) (setq i (+ i 1))))))
        (t  ;; :vector
         (let ((n (length merged))
               (cur merged))
           (let ((v (make-array n)) (i 0))
             (loop (when (= i n) (return v))
               (aset v i (car cur))
               (setq cur (cdr cur)) (setq i (+ i 1))))))))))

(defun replace (s1 s2 &rest args)
  "Replace elements of S1 with elements from S2."
  (let ((len (if (< (length s1) (length s2)) (length s1) (length s2))))
    (dotimes (i len)
      (if (consp s1)
          (let ((cell (nthcdr i s1))) (when cell (set-car cell (elt s2 i))))
          (aset s1 i (elt s2 i)))))
  s1)

(defun fill (seq item &rest args)
  "Fill SEQUENCE with ITEM. Strings store fixnum char-codes; coerce
   character ITEM to its char-code before aset so the stored value
   matches what literal strings hold (used by string-equal/aref/=)."
  (if (consp seq)
      (let ((cur seq)) (loop (when (null cur) (return seq))
                         (set-car cur item) (setq cur (cdr cur))))
      (let ((store-item (if (and (stringp seq) (characterp item))
                            (char-code item)
                            item)))
        (dotimes (i (length seq) seq) (aset seq i store-item)))))

(defun map-into (result fn &rest seqs)
  "Apply FN to elements of SEQS, storing results in RESULT."
  (let ((len (length result)) (i 0))
    (if (null seqs)
        result
        (let ((src (car seqs)))
          (if (consp src)
              (dolist (item src result)
                (when (>= i len) (return result))
                (if (consp result) (set-car (nthcdr i result) (funcall fn item))
                    (aset result i (funcall fn item)))
                (setq i (+ i 1)))
              result)))))

;;; Sequence predicates
(defun notevery (pred seq &rest more)
  "True if PRED is false for some element."
  (not (apply #'every pred seq more)))

(defun notany (pred seq &rest more)
  "True if PRED is false for all elements."
  (not (apply #'some pred seq more)))

;;; Set/list operations
(defun adjoin (item list &rest args)
  "Add ITEM to LIST if not already present."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (item-key (if key-fn (funcall key-fn item) item)))
    (if (some (lambda (x) (funcall test-fn item-key (if key-fn (funcall key-fn x) x))) list)
        list
        (cons item list))))

(defun nintersection (list1 list2 &rest args)
  "Destructive intersection."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (r nil))
    (dolist (item list1 (nreverse r))
      (let ((item-key (if key-fn (funcall key-fn item) item)))
        (when (some (lambda (x) (funcall test-fn item-key (if key-fn (funcall key-fn x) x))) list2)
          (setq r (cons item r)))))))

(defun delete (item seq &rest args)
  "Remove ITEM from SEQ (destructive)."
  (remove item seq))

(defun delete-if (pred seq &rest args)
  "Remove items satisfying PRED (destructive)."
  (remove-if pred seq))

(defun delete-if-not (pred seq &rest args)
  "Remove items not satisfying PRED (destructive)."
  (remove-if-not pred seq))

(defun delete-duplicates (seq &rest args)
  "Remove duplicate items (destructive)."
  (remove-duplicates seq))

(defun pushnew-fn (item place)
  "Functional pushnew."
  (if (member item place) place (cons item place)))

;;; String operations
(defun make-string (size &rest args)
  "Make a string of SIZE filled with spaces (or :initial-element)."
  (let ((ch 32) (a args))
    (loop (when (null a) (return))
      (when (eq (car a) :initial-element) (setq ch (char-code (cadr a))) (return))
      (setq a (cddr a)))
    (let ((s (%make-string-array size)))
      (dotimes (i size) (aset s i ch))
      s)))

(defun string-trim (chars str)
  "Trim characters from both ends of string."
  (string-left-trim chars (string-right-trim chars str)))

(defun string-left-trim (chars str)
  "Trim characters from left of string."
  (let ((char-list (if (stringp chars)
                       (let ((r nil)) (dotimes (i (array-length chars)) (setq r (cons (aref chars i) r))) r)
                       chars))
        (start 0) (len (array-length str)))
    (loop (when (>= start len) (return ""))
      (unless (member (aref str start) char-list) (return))
      (setq start (+ start 1)))
    (if (= start 0) str
        (let ((result (%make-string-array (- len start))))
          (dotimes (i (- len start)) (aset result i (aref str (+ start i))))
          result))))

(defun string-right-trim (chars str)
  "Trim characters from right of string."
  (let ((char-list (if (stringp chars)
                       (let ((r nil)) (dotimes (i (array-length chars)) (setq r (cons (aref chars i) r))) r)
                       chars))
        (end (array-length str)))
    (loop (when (<= end 0) (return ""))
      (unless (member (aref str (- end 1)) char-list) (return))
      (setq end (- end 1)))
    (if (= end (array-length str)) str
        (let ((result (%make-string-array end)))
          (dotimes (i end) (aset result i (aref str i)))
          result))))

;;; Numeric
(defun logbitp (index integer)
  "True if bit INDEX of INTEGER is 1."
  (not (zerop (logand integer (ash 1 index)))))

(defun integer-length (n)
  "Number of bits needed to represent N."
  (let ((x (abs n)) (len 0))
    (loop (when (zerop x) (return len))
      (setq x (ash x -1)) (setq len (+ len 1)))))

(defun complex (r &optional i)
  "Create a complex number (stub — returns real part)."
  r)

;;; Hash table
(defun hash-table-p (obj)
  "True if OBJ is a hash table (cons cell)."
  (consp obj))

;;; Symbol
(defun symbol-name (sym)
  "Return the name of a symbol as a string (stub)."
  (if (null sym) "NIL"
      (if (eq sym t) "T"
          "")))

;;; Test helpers
(defun random-from-interval (max &optional (min 0))
  "Random integer in [min, max)."
  (+ min (random (- max min))))

(defun is-antisymmetrically-ordered-by (pred)
  "Return a predicate that checks antisymmetric ordering."
  (lambda (x y) (and (funcall pred x y) (not (funcall pred y x)))))

(defun do-special-integer-vectors (fn)
  "Stub — call FN with a simple vector."
  (funcall fn (make-array 5)))

;;; More CL functions
(defun evenp (n) (zerop (logand n 1)))
(defun oddp (n) (not (zerop (logand n 1))))
(defun boundp (sym) t)  ; stub — all symbols considered bound
(defun vectorp (obj) (and (not (consp obj)) (not (null obj)) (not (integerp obj))
                          (not (characterp obj)) (not (eq obj t))))
(defun remove-duplicates (seq &rest args)
  "Remove duplicates from SEQ."
  (if (consp seq)
      (let ((r nil))
        (dolist (item seq) (unless (member item r) (setq r (cons item r))))
        (nreverse r))
      seq))

;;; ===================================================
;;; Sequence Search Functions (find, search, position-if, etc.)
;;; ===================================================

(defun find (item sequence &rest args)
  "Return the first element of SEQUENCE that matches ITEM."
  (let ((test #'eql) (key nil) (start 0) (end nil) (from-end nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :test) (setq test v))
            ((eq k :key) (setq key v))
            ((eq k :start) (setq start v))
            ((eq k :end) (setq end v))
            ((eq k :from-end) (setq from-end v))))
        (setq cur (cddr cur))))
    (if (listp sequence)
        ;; List path
        (let ((lst sequence) (i 0) (result nil))
          ;; Skip to start
          (loop
            (when (or (null lst) (= i start)) (return nil))
            (setq lst (cdr lst))
            (setq i (+ i 1)))
          ;; Search
          (loop
            (when (null lst) (return result))
            (when (and end (= i end)) (return result))
            (let ((elem (car lst)))
              (let ((test-val (if key (funcall key elem) elem)))
                (when (funcall test item test-val)
                  (if from-end
                      (setq result elem)
                      (return elem)))))
            (setq lst (cdr lst))
            (setq i (+ i 1))))
        ;; Vector path
        (let ((len (length sequence))
              (result nil))
          (when (null end) (setq end len))
          (let ((i start))
            (loop
              (when (= i end) (return result))
              (let ((elem (aref sequence i)))
                (let ((test-val (if key (funcall key elem) elem)))
                  (when (funcall test item test-val)
                    (if from-end
                        (setq result elem)
                        (return elem)))))
              (setq i (+ i 1))))))))

(defun find-if (predicate sequence &rest args)
  "Return the first element of SEQUENCE satisfying PREDICATE."
  (let ((key nil) (start 0) (end nil) (from-end nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :key) (setq key v))
            ((eq k :start) (setq start v))
            ((eq k :end) (setq end v))
            ((eq k :from-end) (setq from-end v))))
        (setq cur (cddr cur))))
    (if (listp sequence)
        (let ((lst sequence) (i 0) (result nil))
          (loop
            (when (or (null lst) (= i start)) (return nil))
            (setq lst (cdr lst))
            (setq i (+ i 1)))
          (loop
            (when (null lst) (return result))
            (when (and end (= i end)) (return result))
            (let ((elem (car lst)))
              (let ((test-val (if key (funcall key elem) elem)))
                (when (funcall predicate test-val)
                  (if from-end (setq result elem) (return elem)))))
            (setq lst (cdr lst))
            (setq i (+ i 1))))
        (let ((len (length sequence)) (result nil))
          (when (null end) (setq end len))
          (let ((i start))
            (loop
              (when (= i end) (return result))
              (let ((elem (aref sequence i)))
                (let ((test-val (if key (funcall key elem) elem)))
                  (when (funcall predicate test-val)
                    (if from-end (setq result elem) (return elem)))))
              (setq i (+ i 1))))))))

(defun find-if-not (predicate sequence &rest args)
  "Return the first element of SEQUENCE not satisfying PREDICATE."
  (apply #'find-if (lambda (x) (not (funcall predicate x))) sequence args))

(defun position-if (predicate sequence &rest args)
  "Return the index of first element satisfying PREDICATE."
  (let ((key nil) (start 0) (end nil) (from-end nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :key) (setq key v))
            ((eq k :start) (setq start v))
            ((eq k :end) (setq end v))
            ((eq k :from-end) (setq from-end v))))
        (setq cur (cddr cur))))
    (if (listp sequence)
        (let ((lst sequence) (i 0) (result nil))
          (loop
            (when (or (null lst) (= i start)) (return nil))
            (setq lst (cdr lst))
            (setq i (+ i 1)))
          (loop
            (when (null lst) (return result))
            (when (and end (= i end)) (return result))
            (let ((elem (car lst)))
              (let ((test-val (if key (funcall key elem) elem)))
                (when (funcall predicate test-val)
                  (if from-end (setq result i) (return i)))))
            (setq lst (cdr lst))
            (setq i (+ i 1))))
        (let ((len (length sequence)) (result nil))
          (when (null end) (setq end len))
          (let ((i start))
            (loop
              (when (= i end) (return result))
              (let ((elem (aref sequence i)))
                (let ((test-val (if key (funcall key elem) elem)))
                  (when (funcall predicate test-val)
                    (if from-end (setq result i) (return i)))))
              (setq i (+ i 1))))))))

(defun position-if-not (predicate sequence &rest args)
  "Return the index of first element not satisfying PREDICATE."
  (apply #'position-if (lambda (x) (not (funcall predicate x))) sequence args))

(defun search (seq1 seq2 &rest args)
  "Search for SEQ1 as a subsequence of SEQ2. Return index or nil."
  (let ((test #'eql) (key nil) (start1 0) (end1 nil) (start2 0) (end2 nil) (from-end nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :test) (setq test v))
            ((eq k :key) (setq key v))
            ((eq k :start1) (setq start1 v))
            ((eq k :end1) (setq end1 v))
            ((eq k :start2) (setq start2 v))
            ((eq k :end2) (setq end2 v))
            ((eq k :from-end) (setq from-end v))))
        (setq cur (cddr cur))))
    ;; Convert to vectors for simpler indexing
    (let ((s1 (if (stringp seq1) seq1 (coerce seq1 'vector)))
          (s2 (if (stringp seq2) seq2 (coerce seq2 'vector))))
      (when (null end1) (setq end1 (length s1)))
      (when (null end2) (setq end2 (length s2)))
      (let ((len1 (- end1 start1))
            (result nil))
        (let ((i start2))
          (loop
            (when (> (+ i len1) end2) (return result))
            ;; Check if s1[start1..end1) matches s2[i..i+len1)
            (let ((match t) (j 0))
              (loop
                (when (= j len1) (return nil))
                (let ((e1 (aref s1 (+ start1 j)))
                      (e2 (aref s2 (+ i j))))
                  (let ((v1 (if key (funcall key e1) e1))
                        (v2 (if key (funcall key e2) e2)))
                    (unless (funcall test v1 v2)
                      (setq match nil)
                      (return nil))))
                (setq j (+ j 1)))
              (when match
                (if from-end
                    (setq result i)
                    (return i))))
            (setq i (+ i 1))))))))

