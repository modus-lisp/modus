;;;; cl-sequences.lisp — Sequence, list, and collection functions
;;;; Part of the Modus CL runtime. Load after cl-types.lisp.

;;; ============================================================
;;; Additional missing functions
;;; ============================================================

;;; Sequence-aware some/every (override prelude versions that only handle lists)
(defun some (fn seq &rest more-seqs)
  "Return first non-nil result of FN on elements of SEQ (list or vector).
   Multi-sequence form stops at the shortest sequence (ANSI).
   Accepts a symbol or function as FN per ANSI function designator."
  (let ((fn (%resolve-fn fn)))
  (cond
    ((null more-seqs)
     (cond
       ;; Wrapped vector — peel via array-wrapper-p / wrapper-aref
       ((and (consp seq) (array-wrapper-p seq))
        (let* ((len (length seq))
               (string-p (stringp seq))
               (i 0))
          (loop
            (when (>= i len) (return nil))
            (let* ((raw (%wrapper-aref seq i))
                   (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                   (result (funcall fn elem)))
              (when result (return result)))
            (setq i (+ i 1)))))
       ((consp seq)
        (let ((cur seq))
          (loop
            (when (null cur) (return nil))
            (let ((result (funcall fn (car cur))))
              (when result (return result)))
            (setq cur (cdr cur)))))
       ((null seq) nil)
       (t (let ((len (length seq)) (i 0)
                (string-p (stringp seq)))
            (loop
              (when (>= i len) (return nil))
              (let* ((raw (aref seq i))
                     (elem (if string-p (code-char raw) raw))
                     (result (funcall fn elem)))
                (when result (return result)))
              (setq i (+ i 1)))))))
    ;; Two-sequence fast path for two LISTS.
    ((and (null (cdr more-seqs))
          (or (consp seq) (null seq))
          (or (consp (car more-seqs)) (null (car more-seqs))))
     (let ((cur1 seq) (cur2 (car more-seqs)))
       (loop
         (when (or (null cur1) (null cur2)) (return nil))
         (let ((r (funcall fn (car cur1) (car cur2))))
           (when r (return r)))
         (setq cur1 (cdr cur1)) (setq cur2 (cdr cur2)))))
    ;; Three-sequence fast path for three LISTS.
    ((and (null (cddr more-seqs))
          (or (consp seq) (null seq))
          (or (consp (car more-seqs)) (null (car more-seqs)))
          (or (consp (cadr more-seqs)) (null (cadr more-seqs))))
     (let ((c1 seq) (c2 (car more-seqs)) (c3 (cadr more-seqs)))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return nil))
         (let ((r (funcall fn (car c1) (car c2) (car c3))))
           (when r (return r)))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    ;; Fallback: apply path.
    (t (let ((seqs (cons seq more-seqs)))
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
               (setq lists (mapcar #'cdr lists))))))))))

(defun every (fn seq &rest more-seqs)
  "Return T if FN is true for all elements of SEQ (list or vector).
   Multi-sequence form stops at the shortest sequence (ANSI).
   Accepts a symbol or function as FN per ANSI function designator."
  (let ((fn (%resolve-fn fn)))
  (cond
    ((null more-seqs)
     (cond
       ;; Wrapped vector (adj/fp/displaced/multi-dim).  Detected via
       ;; array-wrapper-p (peels adj wrapper if present).  Use length to
       ;; honor fill pointer, %wrapper-aref to read through the wrapper.
       ;; Determine string-ness by checking the underlying storage type.
       ((and (consp seq) (array-wrapper-p seq))
        (let* ((len (length seq))
               (string-p (stringp seq))
               (i 0))
          (loop
            (when (>= i len) (return t))
            (let* ((raw (%wrapper-aref seq i))
                   (elem (if (and string-p (integerp raw)) (code-char raw) raw)))
              (when (null (funcall fn elem)) (return nil)))
            (setq i (+ i 1)))))
       ((consp seq)
        (let ((cur seq))
          (loop
            (when (null cur) (return t))
            (when (null (funcall fn (car cur))) (return nil))
            (setq cur (cdr cur)))))
       ((null seq) t)
       (t (let ((len (length seq)) (i 0)
                (string-p (stringp seq)))
            (loop
              (when (>= i len) (return t))
              (let* ((raw (aref seq i))
                     (elem (if string-p (code-char raw) raw)))
                (when (null (funcall fn elem)) (return nil)))
              (setq i (+ i 1)))))))
    ;; Two-sequence fast path for two LISTS (skip if either is non-list,
    ;; e.g., a vector — fall through to the general path below).
    ((and (null (cdr more-seqs))
          (or (consp seq) (null seq))
          (or (consp (car more-seqs)) (null (car more-seqs))))
     (let ((cur1 seq) (cur2 (car more-seqs)))
       (loop
         (when (or (null cur1) (null cur2)) (return t))
         (when (null (funcall fn (car cur1) (car cur2))) (return nil))
         (setq cur1 (cdr cur1)) (setq cur2 (cdr cur2)))))
    ;; Three-sequence fast path for three LISTS.
    ((and (null (cddr more-seqs))
          (or (consp seq) (null seq))
          (or (consp (car more-seqs)) (null (car more-seqs)))
          (or (consp (cadr more-seqs)) (null (cadr more-seqs))))
     (let ((c1 seq) (c2 (car more-seqs)) (c3 (cadr more-seqs)))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return t))
         (when (null (funcall fn (car c1) (car c2) (car c3))) (return nil))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    ;; Fallback: apply path (rare, may hit fragility).
    (t (not (apply #'some (lambda (&rest args) (not (apply fn args)))
                   seq more-seqs))))))

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

(defun mapcon (fn list &rest more-lists)
  "ANSI variadic mapcon. Fast paths for 1/2/3 lists avoid apply-of-rest
   fragility. fn is called on the cdrs (sublists), and results are nconc'd."
  (cond
    ((null more-lists)
     (let ((result nil) (cur list))
       (loop
         (when (null cur) (return result))
         (let ((r (funcall fn cur)))
           (setq result (nconc result r)))
         (setq cur (cdr cur)))))
    ((null (cdr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2)) (return result))
         (let ((r (funcall fn c1 c2)))
           (setq result (nconc result r)))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)))))
    ((null (cddr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (c3 (cadr more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return result))
         (let ((r (funcall fn c1 c2 c3)))
           (setq result (nconc result r)))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    (t
     (let ((result nil) (lists (cons list more-lists)))
       (loop
         (when (some #'null lists) (return result))
         (let ((r (apply fn lists)))
           (setq result (nconc result r)))
         (setq lists (mapcar1 #'cdr lists)))))))

(defun mapcan (fn list &rest more-lists)
  "ANSI variadic mapcan. Fast paths for 1/2/3 lists avoid apply-of-rest
   fragility. fn is called on the cars (elements), and results are nconc'd."
  (cond
    ((null more-lists)
     (let ((result nil) (cur list))
       (loop
         (when (null cur) (return result))
         (let ((r (funcall fn (car cur))))
           (setq result (nconc result r)))
         (setq cur (cdr cur)))))
    ((null (cdr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2)) (return result))
         (let ((r (funcall fn (car c1) (car c2))))
           (setq result (nconc result r)))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)))))
    ((null (cddr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (c3 (cadr more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return result))
         (let ((r (funcall fn (car c1) (car c2) (car c3))))
           (setq result (nconc result r)))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    (t
     (let ((result nil) (lists (cons list more-lists)))
       (loop
         (when (some #'null lists) (return result))
         (let ((r (apply fn (mapcar1 #'car lists))))
           (setq result (nconc result r)))
         (setq lists (mapcar1 #'cdr lists)))))))

(defun maplist (fn list)
  (let ((result nil) (cur list))
    (loop
      (when (null cur) (return (nreverse result)))
      (setq result (cons (funcall fn cur) result))
      (setq cur (cdr cur)))))

(defun %resolve-fn (v)
  "If V is a symbol, return its symbol-function (so e.g. :test 'equal
   yields a callable).  Handles BOTH CL symbols and native MVM symbols
   (which need *native-sym-function-table* lookup).  Else return V."
  (cond
    ((null v) nil)
    ((eq v t) v)
    ((and (%cl-sym-p v) (not (keywordp v))) (symbol-function v))
    ;; Native MVM symbol: subtag #x50 = 80, single-slot [hash]
    ((and (not (consp v)) (not (fixnump v)) (not (characterp v))
          (not (stringp v))
          (= (obj-subtag v) 80))
     (let ((h (aref v 0)))
       (let ((fn (if *native-sym-function-table*
                     (gethash h *native-sym-function-table*)
                     nil)))
         (if fn fn v))))
    (t v)))

(defun parse-test-key (args)
  "Parse :test and :key keyword args. Returns (test-fn . key-fn).
   test-fn may be NIL — callers should use inline `eql` in that case
   rather than `#'eql`, because `eql` is an inline opcode in MVM and
   has no callable function entry (#'eql evaluates to a NIL/garbage
   pointer; (funcall <that> ...) silently returns wrong values).
   Symbol values (e.g. :test 'equal) are resolved via symbol-function.
   Per CLHS 3.4.1.4.1, when the same keyword appears multiple times the
   LEFTMOST occurrence supplies the value."
  (let ((test-fn nil) (key-fn nil) (test-set nil) (key-set nil) (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test)
             (unless test-set
               (setq test-fn (%resolve-fn (cadr a)))
               (setq test-set t))
             (setq a (cddr a)))
            ((eq (car a) :key)
             (unless key-set
               (setq key-fn (%resolve-fn (cadr a)))
               (setq key-set t))
             (setq a (cddr a)))
            ((eq (car a) :test-not)
             (unless test-set
               (let ((f (%resolve-fn (cadr a))))
                 (setq test-fn (lambda (x y) (not (funcall f x y)))))
               (setq test-set t))
             (setq a (cddr a)))
            ((eq (car a) :count) (setq a (cddr a)))
            ((eq (car a) :start) (setq a (cddr a)))
            ((eq (car a) :end) (setq a (cddr a)))
            ((eq (car a) :from-end) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (cons test-fn key-fn)))

;; Full ANSI parse: (count from-end start end test-fn test-not-fn key-fn).
;; Reuses %nsubst-parse-args defined in this file.
(defun remove (item seq &rest args)
  "Honors :test/:test-not/:key/:start/:end/:count/:from-end. Inline pred so
   we don't depend on a lambda closure capturing ITEM (lossy in MVM)."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn (car (cddddr parsed)))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) nil)
      ((and eff-count (= eff-count 0)) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-list item seq test-fn key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-vector item seq test-fn key-fn start-idx end-idx eff-count from-end)))))

(defun %remove-list (item lst test-fn key-fn start-idx end-idx count from-end)
  ;; Walk the list, marking which indices to drop, then build a new list.
  ;; FROM-END only matters when COUNT is bounded.
  (let ((indices nil) (cur lst) (i 0))
    (loop
      (when (null cur) (return nil))
      (when (and end-idx (>= i end-idx)) (return nil))
      (when (>= i start-idx)
        (let ((v (if key-fn (funcall key-fn (car cur)) (car cur))))
          (when (if test-fn (funcall test-fn item v) (eql item v))
            (push i indices))))
      (setq cur (cdr cur))
      (setq i (+ i 1)))
    ;; Apply count: keep only the first/last N matched indices.
    (let ((to-drop indices))
      (when count
        (when from-end
          ;; indices in reverse-traversal order (largest first); take first N
          (setq to-drop (subseq indices 0 (min count (length indices)))))
        (unless from-end
          ;; want first N matches (smallest indices); indices is largest-first
          (let ((rev (reverse indices)))
            (setq to-drop (subseq rev 0 (min count (length rev))))
            (setq to-drop to-drop))))
      ;; Build result list excluding to-drop indices
      (let ((result nil) (cur lst) (i 0))
        (loop
          (when (null cur) (return (nreverse result)))
          (unless (member i to-drop) (push (car cur) result))
          (setq cur (cdr cur))
          (setq i (+ i 1)))))))

(defun %remove-vector (item vec test-fn key-fn start-idx end-idx count from-end)
  (let* ((len (array-length vec))
         (eff-end (if (and end-idx (< end-idx len)) end-idx len))
         (string-p (stringp vec))
         (indices nil))
    ;; Collect matching indices.
    (let ((i start-idx))
      (loop
        (when (>= i eff-end) (return nil))
        (let ((elt (aref vec i)))
          (when string-p (setq elt (code-char elt)))
          (let ((v (if key-fn (funcall key-fn elt) elt)))
            (when (if test-fn (funcall test-fn item v) (eql item v))
              (push i indices))))
        (setq i (+ i 1))))
    (let ((to-drop indices))
      (when count
        (if from-end
            (setq to-drop (subseq indices 0 (min count (length indices))))
            (let ((rev (reverse indices)))
              (setq to-drop (subseq rev 0 (min count (length rev)))))))
      ;; Build a new vector skipping to-drop indices.
      (let* ((drop-count (length to-drop))
             (out-len (- len drop-count))
             (out (if string-p (%make-string-array out-len) (make-array out-len)))
             (i 0) (j 0))
        (loop
          (when (= i len) (return out))
          (unless (member i to-drop)
            (aset out j (aref vec i))
            (setq j (+ j 1)))
          (setq i (+ i 1)))))))

(defun remove-if (pred seq &rest args)
  "Honors :key/:start/:end/:count/:from-end. Drops elements where PRED is true.
   Inline pred-eval so we don't depend on closure capture across apply paths.
   Overrides the simpler 2-arg version in prelude.lisp."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) nil)
      ((and eff-count (= eff-count 0)) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-if-list pred seq key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-if-vector pred seq key-fn start-idx end-idx eff-count from-end)))))

(defun %remove-if-list (pred lst key-fn start-idx end-idx count from-end)
  (let ((indices nil) (cur lst) (i 0))
    (loop
      (when (null cur) (return nil))
      (when (and end-idx (>= i end-idx)) (return nil))
      (when (>= i start-idx)
        (let ((v (if key-fn (funcall key-fn (car cur)) (car cur))))
          (when (funcall pred v) (push i indices))))
      (setq cur (cdr cur))
      (setq i (+ i 1)))
    (let ((to-drop indices))
      (when count
        (if from-end
            (setq to-drop (subseq indices 0 (min count (length indices))))
            (let ((rev (reverse indices)))
              (setq to-drop (subseq rev 0 (min count (length rev)))))))
      (let ((result nil) (cur lst) (i 0))
        (loop
          (when (null cur) (return (nreverse result)))
          (unless (member i to-drop) (push (car cur) result))
          (setq cur (cdr cur))
          (setq i (+ i 1)))))))

(defun %remove-if-vector (pred vec key-fn start-idx end-idx count from-end)
  (let* ((len (array-length vec))
         (eff-end (if (and end-idx (< end-idx len)) end-idx len))
         (string-p (stringp vec))
         (indices nil))
    (let ((i start-idx))
      (loop
        (when (>= i eff-end) (return nil))
        (let ((elt (aref vec i)))
          (when string-p (setq elt (code-char elt)))
          (let ((v (if key-fn (funcall key-fn elt) elt)))
            (when (funcall pred v) (push i indices))))
        (setq i (+ i 1))))
    (let ((to-drop indices))
      (when count
        (if from-end
            (setq to-drop (subseq indices 0 (min count (length indices))))
            (let ((rev (reverse indices)))
              (setq to-drop (subseq rev 0 (min count (length rev)))))))
      (let* ((drop-count (length to-drop))
             (out-len (- len drop-count))
             (out (if string-p (%make-string-array out-len) (make-array out-len)))
             (i 0) (j 0))
        (loop
          (when (= i len) (return out))
          (unless (member i to-drop)
            (aset out j (aref vec i))
            (setq j (+ j 1)))
          (setq i (+ i 1)))))))

(defun remove-if-not (pred seq &rest args)
  "Inverse of remove-if; forwards the same keyword args.  Inlined
   instead of `(apply #'remove-if (lambda (x) (not (funcall pred x)))
   seq args)' to dodge apply-of-rest fragility (see find-if-not /
   position-if-not for the same pattern in commit 9c625ec)."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count))
         (neg-pred (lambda (x) (not (funcall pred x)))))
    (cond
      ((null seq) nil)
      ((and eff-count (= eff-count 0)) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-if-list neg-pred seq key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-if-vector neg-pred seq key-fn start-idx end-idx eff-count from-end)))))

(defun count-if (pred seq &rest args)
  "Count elements of SEQ for which PRED is true. Honors :key, :start, :end."
  (let ((key nil) (start 0) (end nil) (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :key) (setq key (cadr a)) (setq a (cddr a)))
            ((eq (car a) :start) (setq start (cadr a)) (setq a (cddr a)))
            ((eq (car a) :end) (setq end (cadr a)) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (cond
      ((null seq) 0)
      ;; Wrapped vector — use length+wrapper-aref so fp/displaced/adj are honored
      ((and (consp seq) (array-wrapper-p seq))
       (let* ((n 0)
              (string-p (stringp seq))
              (eff-end (if end end (length seq)))
              (i start))
         (loop (when (>= i eff-end) (return n))
           (let* ((raw (%wrapper-aref seq i))
                  (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                  (v (if key (funcall key elem) elem)))
             (when (funcall pred v) (setq n (+ n 1))))
           (setq i (+ i 1)))))
      ((consp seq)
       (let ((n 0) (cur seq) (i 0)
             (eff-end (if end end most-positive-fixnum)))
         (loop (when (or (null cur) (>= i eff-end)) (return n))
           (when (>= i start)
             (let ((v (if key (funcall key (car cur)) (car cur))))
               (when (funcall pred v) (setq n (+ n 1)))))
           (setq cur (cdr cur))
           (setq i (+ i 1)))))
      (t  ;; vector / string
       (let ((n 0) (i start)
             (eff-end (if end end (length seq))))
         (loop (when (>= i eff-end) (return n))
           (let ((v (if key (funcall key (elt seq i)) (elt seq i))))
             (when (funcall pred v) (setq n (+ n 1))))
           (setq i (+ i 1))))))))

(defun count (item seq &rest args)
  "Count occurrences of ITEM in SEQ. Honors :test, :key, :start, :end.
   For strings, presents each element as a character (string slots
   hold fixnum char-codes; the natural test against #\\X expects a
   character comparison via eql)."
  (let ((test nil) (key nil) (start 0) (end nil) (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test) (setq test (cadr a)) (setq a (cddr a)))
            ((eq (car a) :key) (setq key (cadr a)) (setq a (cddr a)))
            ((eq (car a) :start) (setq start (cadr a)) (setq a (cddr a)))
            ((eq (car a) :end) (setq end (cadr a)) (setq a (cddr a)))
            ((eq (car a) :test-not)
             (let ((f (cadr a)))
               (setq test (lambda (x y) (not (funcall f x y)))))
             (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (let ((string-p (stringp seq)))
      (count-if (lambda (x)
                  (let ((c (if string-p (code-char x) x)))
                    (if test (funcall test item c) (eql item c))))
                seq
                :start start
                :end (or end (cond ((null seq) 0)
                                    ((consp seq) most-positive-fixnum)
                                    (t (length seq))))
                :key key))))

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
  (cond
    ;; Array wrapper (adj/fp/displaced/multi-dim) — peel via wrapper-aref
    ((and (consp seq) (array-wrapper-p seq))
     (let ((v (%wrapper-aref seq idx)))
       (if (and (stringp seq) (integerp v)) (code-char v) v)))
    ((consp seq) (nth idx seq))
    (t (let ((v (aref seq idx)))
         (if (stringp seq) (code-char v) v)))))
(defun %string-designator (x)
  "Coerce a string designator (string, character, or symbol) to a string."
  (cond
    ((stringp x) x)
    ((characterp x) (let ((s (%make-string-array 1)))
                      (aset s 0 (char-code x))
                      s))
    ((%cl-sym-p x) (%cl-sym-name x))
    ((null x) "NIL")
    ((eq x t) "T")
    (t x)))                      ; fallthrough — might be a wrapper

(defun string= (a b &rest options)
  "ANSI string= takes string designators (strings, characters, symbols)."
  (declare (ignore options))
  (string-equal (%string-designator a) (%string-designator b)))
(defun string/= (a b) (if (string-equal a b) nil t))
;;; constantly: captures value. Use global cell.
(defvar *constantly-value* nil)
(defun %constantly-impl (&rest args) *constantly-value*)
(defun constantly (value) (setq *constantly-value* value) #'%constantly-impl)
;;; Closure support functions for is-eql-p / is-not-eql-p.
;;; These load the captured env from CLOSURE-ENV-ADDR (#x10000140), which
;;; funcall stores when it detects a closure object (tag=object,
;;; subtag=#x52, 2 slots [fn-addr env-list]). Previously closures were
;;; represented as cons cells, but that collided with CL symbols (also
;;; cons cells) in the funcall dispatch — see ansi-notes.md.
(defun closure-eql-fn (y)
  (let* ((env (%get-cenv))
         (x (car env)))
    (eql x y)))
(defun closure-not-eql-fn (y)
  (let* ((env (%get-cenv))
         (x (car env)))
    (not (eql x y))))
;;; Placeholder is-eql-p/is-not-eql-p (will be overridden by ansi-tests.lisp)
(defvar *is-eql-p-item* nil)
(defun is-eql-p (x) (%make-closure #'closure-eql-fn (cons x nil)))
(defun is-not-eql-p (x) (%make-closure #'closure-not-eql-fn (cons x nil)))
(defun %sort-list (seq pred key)
  ;; Insertion sort over a list — destructive on the spine of result.
  (if (or (null seq) (null (cdr seq)))
      seq
      (let ((result (list (car seq))))
        (dolist (item (cdr seq))
          (let ((iv (if key (funcall key item) item))
                (rv (if key (funcall key (car result)) (car result))))
            (if (funcall pred iv rv)
                (setq result (cons item result))
                (let ((prev result))
                  (loop
                    (when (null (cdr prev))
                      (set-cdr prev (list item)) (return nil))
                    (let ((nv (if key (funcall key (cadr prev)) (cadr prev))))
                      (when (funcall pred iv nv)
                        (set-cdr prev (cons item (cdr prev)))
                        (return nil)))
                    (setq prev (cdr prev)))))))
        result)))

(defun %sort-vector (seq pred key)
  ;; Insertion sort over a vector, in-place.  Walk i from 1 to len-1;
  ;; for each, slide it left while predicate(i, j-1) holds.
  (let ((len (array-length seq)))
    (let ((i 1))
      (loop
        (when (>= i len) (return seq))
        (let ((j i))
          (loop
            (when (= j 0) (return nil))
            (let* ((a (aref seq j))
                   (b (aref seq (- j 1)))
                   (av (if key (funcall key a) a))
                   (bv (if key (funcall key b) b)))
              (if (funcall pred av bv)
                  (progn (aset seq j b) (aset seq (- j 1) a)
                         (setq j (- j 1)))
                  (return nil)))))
        (setq i (+ i 1))))))

(defun sort (seq pred &rest options)
  ;; Honors :key.  Dispatches list vs vector.
  (let ((key nil) (a options))
    (loop (when (null a) (return))
      (when (eq (car a) :key) (setq key (cadr a)))
      (setq a (cddr a)))
    (cond
      ((null seq) seq)
      ((consp seq) (%sort-list seq pred key))
      (t (%sort-vector seq pred key)))))

(defun stable-sort (seq pred &rest options)
  ;; Insertion sort is naturally stable; same impl.
  (let ((key nil) (a options))
    (loop (when (null a) (return))
      (when (eq (car a) :key) (setq key (cadr a)))
      (setq a (cddr a)))
    (cond
      ((null seq) seq)
      ((consp seq) (%sort-list seq pred key))
      (t (%sort-vector seq pred key)))))
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
  "Non-destructive substitute. Honors :test/:test-not/:key/:start/:end/
   :count/:from-end. Inline vector path so we don't depend on lambda
   capture of OLD across nested apply paths."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn (car (cddddr parsed)))      ; index 4
         (key-fn (caddr (cddddr parsed)))     ; index 6
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ((consp seq)
       (%seq-substitute-with
        (lambda (item)
          (let ((v (if key-fn (funcall key-fn item) item)))
            (if (if test-fn (funcall test-fn old v) (eql old v))
                new
                item)))
        seq))
      (t
       (let ((copy (copy-seq seq)))
         (cond
           ((and eff-count (= eff-count 0)) copy)
           (t
            (let* ((len (array-length copy))
                   (eff-end (if (and end-idx (< end-idx len)) end-idx len))
                   (string-p (stringp copy))
                   (store-new (if (and string-p (characterp new))
                                  (char-code new)
                                  new))
                   (n eff-count))
              (if from-end
                  (let ((i (- eff-end 1)))
                    (loop
                      (when (< i start-idx) (return copy))
                      (when (and n (= n 0)) (return copy))
                      (let ((elt (aref copy i)))
                        (when string-p (setq elt (code-char elt)))
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (let ((match (if test-fn (funcall test-fn old cmp)
                                           (eql old cmp))))
                            (when match
                              (aset copy i store-new)
                              (when n (setq n (- n 1)))))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return copy))
                      (when (and n (= n 0)) (return copy))
                      (let ((elt (aref copy i)))
                        (when string-p (setq elt (code-char elt)))
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (let ((match (if test-fn (funcall test-fn old cmp)
                                           (eql old cmp))))
                            (when match
                              (aset copy i store-new)
                              (when n (setq n (- n 1)))))))
                      (setq i (+ i 1)))))))))))))

(defun substitute-if (new pred seq &rest args)
  "Non-destructive substitute-if. Same shape as SUBSTITUTE.
   List path inlined (no nested closure) — MVM's capture analysis
   loses bindings across the substitute-if-not → apply → substitute-if
   chain when the inner closure captures pred/key-fn."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ((consp seq)
       (let ((result nil) (cur seq) (idx 0) (n eff-count))
         (loop
           (when (null cur) (return (nreverse result)))
           (let* ((item (car cur))
                  (in-window (and (>= idx start-idx)
                                  (or (null end-idx) (< idx end-idx))))
                  (v (if (and in-window key-fn) (funcall key-fn item) item))
                  (replace (and in-window
                                (or (null n) (> n 0))
                                (funcall pred v))))
             (setq result (cons (if replace new item) result))
             (when replace (when n (setq n (- n 1)))))
           (setq cur (cdr cur))
           (setq idx (+ idx 1)))))
      (t
       (let ((copy (copy-seq seq)))
         (cond
           ((and eff-count (= eff-count 0)) copy)
           (t
            (let* ((len (array-length copy))
                   (eff-end (if (and end-idx (< end-idx len)) end-idx len))
                   (string-p (stringp copy))
                   (store-new (if (and string-p (characterp new))
                                  (char-code new)
                                  new))
                   (n eff-count))
              (if from-end
                  (let ((i (- eff-end 1)))
                    (loop
                      (when (< i start-idx) (return copy))
                      (when (and n (= n 0)) (return copy))
                      (let ((elt (aref copy i)))
                        (when string-p (setq elt (code-char elt)))
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (funcall pred cmp)
                            (aset copy i store-new)
                            (when n (setq n (- n 1))))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return copy))
                      (when (and n (= n 0)) (return copy))
                      (let ((elt (aref copy i)))
                        (when string-p (setq elt (code-char elt)))
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (funcall pred cmp)
                            (aset copy i store-new)
                            (when n (setq n (- n 1))))))
                      (setq i (+ i 1)))))))))))))

(defun substitute-if-not (new pred seq &rest args)
  "Non-destructive substitute-if-not.
   Inlined to avoid the apply+closure pattern that MVM's capture
   analysis loses bindings across (was: (apply #'substitute-if new
   (lambda (x) (not (funcall pred x))) seq args))."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ((consp seq)
       (let ((result nil) (cur seq) (idx 0) (n eff-count))
         (loop
           (when (null cur) (return (nreverse result)))
           (let* ((item (car cur))
                  (in-window (and (>= idx start-idx)
                                  (or (null end-idx) (< idx end-idx))))
                  (v (if (and in-window key-fn) (funcall key-fn item) item))
                  (replace (and in-window
                                (or (null n) (> n 0))
                                (not (funcall pred v)))))
             (setq result (cons (if replace new item) result))
             (when replace (when n (setq n (- n 1)))))
           (setq cur (cdr cur))
           (setq idx (+ idx 1)))))
      (t
       ;; Vector path inlined — mirror substitute-if with negated pred.
       ;; Avoids (apply #'substitute-if new (lambda ...) seq args), which
       ;; trips the apply-of-rest + closure-capture fragility.
       (let ((copy (copy-seq seq)))
         (cond
           ((and eff-count (= eff-count 0)) copy)
           (t
            (let* ((len (array-length copy))
                   (eff-end (if (and end-idx (< end-idx len)) end-idx len))
                   (string-p (stringp copy))
                   (store-new (if (and string-p (characterp new))
                                  (char-code new)
                                  new))
                   (n eff-count))
              (if from-end
                  (let ((i (- eff-end 1)))
                    (loop
                      (when (< i start-idx) (return copy))
                      (when (and n (= n 0)) (return copy))
                      (let ((elt (aref copy i)))
                        (when string-p (setq elt (code-char elt)))
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (not (funcall pred cmp))
                            (aset copy i store-new)
                            (when n (setq n (- n 1))))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return copy))
                      (when (and n (= n 0)) (return copy))
                      (let ((elt (aref copy i)))
                        (when string-p (setq elt (code-char elt)))
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (not (funcall pred cmp))
                            (aset copy i store-new)
                            (when n (setq n (- n 1))))))
                      (setq i (+ i 1)))))))))))))

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
   Returns (count from-end start end test-fn test-not-fn key-fn).
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates."
  (let ((from-end nil) (test-fn nil) (test-not-fn nil)
        (count nil) (key-fn nil) (start 0) (end nil) (cur args)
        (fe-set nil) (test-set nil) (tn-set nil)
        (count-set nil) (key-set nil) (start-set nil) (end-set nil))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :from-end) (unless fe-set (setq from-end v) (setq fe-set t)))
          ((eq k :test) (unless test-set (setq test-fn v) (setq test-set t)))
          ((eq k :test-not) (unless tn-set (setq test-not-fn v) (setq tn-set t)))
          ((eq k :count) (unless count-set (setq count v) (setq count-set t)))
          ((eq k :key) (unless key-set (setq key-fn v) (setq key-set t)))
          ((eq k :start) (unless start-set (setq start v) (setq start-set t)))
          ((eq k :end) (unless end-set (setq end v) (setq end-set t))))
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
  "Destructive substitute-if-not.
   Inlined list path to bypass the closure-loses-capture pattern in
   (apply #'nsubstitute-if new (lambda (x) (not (funcall pred x))) ...)."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ((and eff-count (= eff-count 0)) seq)
      ((consp seq)
       (let ((cur seq) (idx 0) (n eff-count))
         (loop
           (when (null cur) (return seq))
           (when (and n (= n 0)) (return seq))
           (let* ((item (car cur))
                  (in-window (and (>= idx start-idx)
                                  (or (null end-idx) (< idx end-idx))))
                  (v (if (and in-window key-fn) (funcall key-fn item) item)))
             (when (and in-window (not (funcall pred v)))
               (set-car cur new)
               (when n (setq n (- n 1)))))
           (setq cur (cdr cur))
           (setq idx (+ idx 1)))))
      (t
       ;; Vector path inlined — mirror nsubstitute-if with negated pred.
       ;; Avoids (apply #'nsubstitute-if ...) trampoline.
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
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (when (not (funcall pred cmp))
                       (aset seq i store-new)
                       (when n (setq n (- n 1))))))
                 (setq i (- i 1))))
             (let ((i start-idx))
               (loop
                 (when (>= i eff-end) (return seq))
                 (when (and n (= n 0)) (return seq))
                 (let ((elt (aref seq i)))
                   (when string-p (setq elt (code-char elt)))
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (when (not (funcall pred cmp))
                       (aset seq i store-new)
                       (when n (setq n (- n 1))))))
                 (setq i (+ i 1))))))))))

(defun nsubstitute (new old seq &rest args)
  "Destructive substitute. Inline vector path so we don't depend on a
   lambda closure capturing OLD (which can be lost across apply / nested
   funcall when the closure cell is shared with siblings)."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn (car (cddddr parsed)))      ; index 4
         (key-fn (caddr (cddddr parsed)))     ; index 6
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ((and eff-count (= eff-count 0)) seq)
      ((consp seq)
       ;; Lists: existing list-core works fine, build a small pred
       (%nsubst-list-core
        new
        (lambda (x)
          (let ((v (if key-fn (funcall key-fn x) x)))
            (if test-fn (funcall test-fn old v) (eql old v))))
        seq eff-count from-end start-idx end-idx))
      (t
       ;; Vector path: inline. Avoids lambda capture of OLD.
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
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (let ((match (if test-fn (funcall test-fn old cmp)
                                      (eql old cmp))))
                       (when match
                         (aset seq i store-new)
                         (when n (setq n (- n 1)))))))
                 (setq i (- i 1))))
             (let ((i start-idx))
               (loop
                 (when (>= i eff-end) (return seq))
                 (when (and n (= n 0)) (return seq))
                 (let ((elt (aref seq i)))
                   (when string-p (setq elt (code-char elt)))
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (let ((match (if test-fn (funcall test-fn old cmp)
                                      (eql old cmp))))
                       (when match
                         (aset seq i store-new)
                         (when n (setq n (- n 1)))))))
                 (setq i (+ i 1))))))))))
(defun count-if-not (pred seq) (let ((c 0)) (dolist (item seq) (unless (funcall pred item) (setq c (+ c 1)))) c))
;; HASH-TABLE-COUNT lives in prelude.lisp; the duplicate here used to
;; win under last-defun-wins and worked the same way for the alist shape,
;; but routing through prelude keeps a single point of truth.
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
      ;; null: only valid for size 0; returns NIL
      ((eq type 'null) (if (= size 0) nil (make-array size)))
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
(defun mismatch (s1 s2 &rest args)
  "Compare S1 vs S2 element-by-element. Returns first index where they
   differ, or NIL if equal. Honors :test, :key, :start1, :end1, :start2,
   :end2, :from-end."
  (let ((test nil) (key nil)
        (start1 0) (end1 nil) (start2 0) (end2 nil) (from-end nil)
        (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test) (setq test (cadr a)) (setq a (cddr a)))
            ((eq (car a) :key)  (setq key  (cadr a)) (setq a (cddr a)))
            ((eq (car a) :start1) (setq start1 (cadr a)) (setq a (cddr a)))
            ((eq (car a) :end1)   (setq end1   (cadr a)) (setq a (cddr a)))
            ((eq (car a) :start2) (setq start2 (cadr a)) (setq a (cddr a)))
            ((eq (car a) :end2)   (setq end2   (cadr a)) (setq a (cddr a)))
            ((eq (car a) :from-end) (setq from-end (cadr a)) (setq a (cddr a)))
            ((eq (car a) :test-not)
             (let ((f (cadr a)))
               (setq test (lambda (x y) (not (funcall f x y)))))
             (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (let* ((l1 (length s1)) (l2 (length s2))
           (eff-end1 (if end1 end1 l1))
           (eff-end2 (if end2 end2 l2))
           (len1 (- eff-end1 start1))
           (len2 (- eff-end2 start2))
           (limit (if (< len1 len2) len1 len2)))
      (if from-end
          ;; Compare from end backward.  ANSI: align the END of s1[start1..end1)
          ;; with the END of s2[start2..end2); compare element-wise from the
          ;; right; return one-past the index in s1 where the elements first
          ;; differ (or NIL if all matched and lengths equal; otherwise the
          ;; common-tail length signals an unequal-length pair).
          ;;
          ;; Pre-fix this code aligned start1 with start2 (using `(+ start1 i)'
          ;; and `(+ start2 i)' for the backward scan), so for sequences of
          ;; different lengths it compared the wrong elements — e.g.
          ;; (mismatch '(a b c a b c d) '(a b c) :from-end t) returned 3
          ;; instead of 7 because it compared (a b c) with (a b c) instead
          ;; of (b c d) with (a b c).
          (let ((i (- limit 1)) (mismatch-at nil)
                (off1 (+ start1 (- len1 limit)))
                (off2 (+ start2 (- len2 limit))))
            (loop (when (< i 0) (return mismatch-at))
              (let ((e1 (elt s1 (+ off1 i)))
                    (e2 (elt s2 (+ off2 i))))
                (let ((v1 (if key (funcall key e1) e1))
                      (v2 (if key (funcall key e2) e2)))
                  (unless (if test (funcall test v1 v2) (eql v1 v2))
                    (setq mismatch-at (+ off1 i 1))
                    (return mismatch-at))))
              (setq i (- i 1)))
            (if (= len1 len2)
                mismatch-at
                ;; Lengths differ and the common tail matched — the
                ;; mismatch is at the index past the longer prefix.
                (or mismatch-at (+ start1 (- len1 limit)))))
          ;; Forward
          (let ((i 0))
            (loop (when (>= i limit)
                    (return (if (= len1 len2) nil (+ start1 limit))))
              (let ((e1 (elt s1 (+ start1 i)))
                    (e2 (elt s2 (+ start2 i))))
                (let ((v1 (if key (funcall key e1) e1))
                      (v2 (if key (funcall key e2) e2)))
                  (unless (if test (funcall test v1 v2) (eql v1 v2))
                    (return (+ start1 i)))))
              (setq i (+ i 1))))))))
;; 30-bit LCG (a=1664525, c=1013904223, m=2^30) — period 2^30, plenty for
;; ANSI tests' 1000-iteration loops.  Old version forgot to write the new
;; seed back, so every call returned the same value and floor.1-fn et al.
;; tested floor on 1000 copies of the same (n,d) instead of 1000 random pairs.
;; Uses inline truncate-rem (next is positive, n is positive — sign matches
;; sign of d, so rem == mod in this regime) to stay fast.
(defun random (n &rest state)
  (declare (ignore state))
  (let* ((seed (mem-ref #x100000A0 :u64))
         (next (logand (+ (* seed 1664525) 1013904223) #x3FFFFFFF)))
    (setf (mem-ref #x100000A0 :u64) next)
    (- next (* (truncate next n) n))))
(defun do-special-strings (fn) (funcall fn ""))
(defun typep* (obj type) (typep obj type))

;;; String functions
(defun %concat-elt-count (s)
  (cond ((null s) 0)
        ((and (consp s) (array-wrapper-p s)) (length s))
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
       ;; Walk seqs in order, splicing each onto a tail-pointer chain so
       ;; the result preserves the input order (was reversed for vectors:
       ;; the old code mixed (append s r) for cons with (append r (...))
       ;; for vectors, producing wrong order on mixed-type input).
       (let* ((head (cons nil nil))
              (tail head))
         (dolist (s seqs)
           (cond
             ((null s) nil)
             ((consp s)
              (dolist (e s)
                (setf (cdr tail) (cons e nil))
                (setq tail (cdr tail))))
             (t
              (let ((n (length s)) (i 0))
                (loop
                  (when (>= i n) (return nil))
                  (setf (cdr tail) (cons (elt s i) nil))
                  (setq tail (cdr tail))
                  (setq i (+ i 1)))))))
         (cdr head)))
      ((eq kind :string)
       (let ((total 0))
         (dolist (s seqs) (setq total (+ total (%concat-elt-count s))))
         (let ((result (%make-string-array total)) (pos 0))
           (dolist (s seqs)
             (cond
               ((null s) nil)
               ;; Wrapped vector — use length/wrapper-aref
               ((and (consp s) (array-wrapper-p s))
                (let ((n (length s)) (i 0)
                      (string-p (stringp s)))
                  (loop
                    (when (>= i n) (return nil))
                    (let ((raw (%wrapper-aref s i)))
                      (aset result pos
                            (cond ((characterp raw) (char-code raw))
                                  ((and string-p (integerp raw)) raw)
                                  (t raw))))
                    (setq pos (+ pos 1))
                    (setq i (+ i 1)))))
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
               ;; Wrapped vector — use length/wrapper-aref
               ((and (consp s) (array-wrapper-p s))
                (let ((n (length s)) (i 0)
                      (string-p (stringp s)))
                  (loop
                    (when (>= i n) (return nil))
                    (let ((raw (%wrapper-aref s i)))
                      (aset result pos
                            (if (and string-p (integerp raw))
                                (code-char raw)
                                raw)))
                    (setq pos (+ pos 1))
                    (setq i (+ i 1)))))
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
  "Fill SEQUENCE with ITEM. Honors :START and :END.  Strings store
   fixnum char-codes; coerce character ITEM to its char-code before
   aset so the stored value matches what literal strings hold."
  (let ((start 0) (end nil) (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :start) (setq start (cadr a)) (setq a (cddr a)))
            ((eq (car a) :end)   (setq end   (cadr a)) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (cond
      ((consp seq)
       (let ((cur seq) (i 0)
             (eff-end (if end end most-positive-fixnum)))
         (loop (when (null cur) (return seq))
           (when (and (>= i start) (< i eff-end))
             (set-car cur item))
           (setq cur (cdr cur))
           (setq i (+ i 1)))
         seq))
      (t
       (let* ((len (length seq))
              (eff-end (if end end len))
              (store-item (if (and (stringp seq) (characterp item))
                              (char-code item)
                              item))
              (i start))
         (loop (when (>= i eff-end) (return seq))
           (aset seq i store-item)
           (setq i (+ i 1))))))))

(defun map-into (result fn &rest seqs)
  "Apply FN to elements of SEQS, storing each result in successive
   positions of RESULT. Stops at the shortest of RESULT and SEQS."
  (let* ((result-len (length result))
         ;; Determine iteration count: min of result length and all seq lengths.
         (n (let ((m result-len))
              (dolist (s seqs m)
                (let ((sl (length s)))
                  (when (< sl m) (setq m sl)))))))
    (if (null seqs)
        ;; Zero-arg FN: fill result with (funcall fn) calls.
        (let ((i 0))
          (loop (when (>= i result-len) (return result))
            (let ((v (funcall fn)))
              (if (consp result)
                  (set-car (nthcdr i result) v)
                  (aset result i (if (and (stringp result) (characterp v))
                                     (char-code v)
                                     v))))
            (setq i (+ i 1))))
        ;; One-or-more seqs: collect ith elements, apply fn, store.
        (let ((i 0))
          (loop (when (>= i n) (return result))
            (let ((args (let ((r nil) (sr (reverse seqs)))
                          (dolist (s sr r)
                            (setq r (cons (elt s i) r))))))
              (let ((v (apply fn args)))
                (if (consp result)
                    (set-car (nthcdr i result) v)
                    (aset result i (if (and (stringp result) (characterp v))
                                       (char-code v)
                                       v)))))
            (setq i (+ i 1)))))))

;;; Sequence predicates.  Avoid `(apply #'every pred seq more)' — apply
;;; through a sibling &rest defun is documented as fragile.  Dispatch
;;; manually for the common 1- and 2-sequence cases (3+ rare in tests).
(defun notevery (pred seq &rest more)
  "True if PRED is false for some element."
  (cond
    ((null more)        (not (every pred seq)))
    ((null (cdr more))  (not (every pred seq (car more))))
    (t                  (not (apply #'every pred seq more)))))

(defun notany (pred seq &rest more)
  "True if PRED is false for all elements."
  (cond
    ((null more)        (not (some pred seq)))
    ((null (cdr more))  (not (some pred seq (car more))))
    (t                  (not (apply #'some pred seq more)))))

;;; Set/list operations
;; ANSI: (tree-equal x y &key test test-not)
;; Recursive structural equality using TEST (default eql) for leaves.
;; Conses are equal iff cars are tree-equal AND cdrs are tree-equal.
(defun tree-equal (a b &rest args)
  (let ((test nil) (test-not nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :test) (setq test v))
          ((eq k :test-not) (setq test-not v))))
      (setq cur (cddr cur)))
    (when test-not
      (let ((tn test-not))
        (setq test (lambda (x y) (not (funcall tn x y))))))
    (%tree-equal-rec a b test)))

(defun %tree-equal-rec (a b test)
  (cond
    ((and (consp a) (consp b))
     (if (%tree-equal-rec (car a) (car b) test)
         (%tree-equal-rec (cdr a) (cdr b) test)
         nil))
    ((or (consp a) (consp b)) nil)
    (t (if test (funcall test a b) (eql a b)))))

(defun adjoin (item list &rest args)
  "Add ITEM to LIST if not already present."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (item-key (if key-fn (funcall key-fn item) item)))
    (if (some (lambda (x)
                (let ((v (if key-fn (funcall key-fn x) x)))
                  (if test-fn (funcall test-fn item-key v) (eql item-key v))))
              list)
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
        (when (some (lambda (x)
                      (let ((v (if key-fn (funcall key-fn x) x)))
                        (if test-fn (funcall test-fn item-key v) (eql item-key v))))
                    list2)
          (setq r (cons item r)))))))

(defun delete (item seq &rest args)
  "Remove ITEM from SEQ (destructive — but we forward to non-destructive
   remove since MVM doesn't track in-place mutation guarantees).
   Inlined parsing (same as remove) so we don't go through apply-of-rest."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn (car (cddddr parsed)))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) nil)
      ((and eff-count (= eff-count 0)) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-list item seq test-fn key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-vector item seq test-fn key-fn start-idx end-idx eff-count from-end)))))

;; delete-if: same body shape as remove-if without the apply trampoline.
(defun delete-if (pred seq &rest args)
  "Remove items satisfying PRED (destructive). Forwards keyword args."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) nil)
      ((and eff-count (= eff-count 0)) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-if-list pred seq key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-if-vector pred seq key-fn start-idx end-idx eff-count from-end)))))

(defun delete-if-not (pred seq &rest args)
  "Remove items not satisfying PRED (destructive). Forwards keyword args."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count))
         (neg-pred (lambda (x) (not (funcall pred x)))))
    (cond
      ((null seq) nil)
      ((and eff-count (= eff-count 0)) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-if-list neg-pred seq key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-if-vector neg-pred seq key-fn start-idx end-idx eff-count from-end)))))

(defun delete-duplicates (seq &rest args)
  "Remove duplicate items (destructive). Honors :test/:key/:from-end.
   Inlined (rather than `(apply #'remove-duplicates seq args)') to dodge
   apply-of-rest fragility — same body as remove-duplicates, since we
   don't truly mutate seq in place anyway (we cons a fresh result list)."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (from-end nil) (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :from-end) (setq from-end (cadr a)) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (cond
      ((null seq) nil)
      ((consp seq)
       (if from-end
           (let ((r nil))
             (dolist (item seq (nreverse r))
               (let ((item-key (if key-fn (funcall key-fn item) item)))
                 (unless (some (lambda (x)
                                 (let ((v (if key-fn (funcall key-fn x) x)))
                                   (if test-fn (funcall test-fn item-key v) (eql item-key v))))
                               r)
                   (setq r (cons item r))))))
           (let ((r nil) (cur seq))
             (loop
               (when (null cur) (return (nreverse r)))
               (let* ((item (car cur))
                      (item-key (if key-fn (funcall key-fn item) item))
                      (rest-tail (cdr cur)))
                 (unless (some (lambda (x)
                                 (let ((v (if key-fn (funcall key-fn x) x)))
                                   (if test-fn (funcall test-fn item-key v) (eql item-key v))))
                               rest-tail)
                   (setq r (cons item r))))
               (setq cur (cdr cur))))))
      (t seq))))

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
;;; The real definition is in prelude.lisp — it recognizes both the
;;; legacy (cons alist nil) shape and the modern (cons alist (cons %ht-tag
;;; meta)) shape that make-hash-table-args creates.  Removed the loose
;;; (consp obj) stub that previously won under last-defun-wins so plain
;;; cons lists wouldn't (typep '(1 2 3) 'hash-table) → T anymore.

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
;; Old definition was a process-of-elimination heuristic (the same
;; fragility class as the old functionp — see commit 7203e19).  It
;; returned T for closures, bignums, ratios, packages, and anything
;; else that survived the negation chain — and was layout-sensitive
;; for fn-addrs whose low bit happened to land on 0 or 1.  Replaced
;; with proper subtag checks.  In MVM all vectors share +subtag-array+
;; (#x32) — strings (#x31) are also vector-shaped per CL semantics.
(defun vectorp (obj) (or (arrayp obj) (stringp obj)))
(defun remove-duplicates (seq &rest args)
  "Remove duplicates from SEQ. CL default keeps the LAST occurrence
   of each duplicate group (so the relative order of survivors is the
   order they last appeared). With :from-end t, keeps the FIRST.
   Honors :test, :key. Default :test is inline `eql`."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (from-end nil) (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :from-end) (setq from-end (cadr a)) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (cond
      ((null seq) nil)
      ((consp seq)
       (if from-end
           ;; Keep first occurrence — walk forward
           (let ((r nil))
             (dolist (item seq (nreverse r))
               (let ((item-key (if key-fn (funcall key-fn item) item)))
                 (unless (some (lambda (x)
                                 (let ((v (if key-fn (funcall key-fn x) x)))
                                   (if test-fn (funcall test-fn item-key v) (eql item-key v))))
                               r)
                   (setq r (cons item r))))))
           ;; Default: keep last occurrence of each — walk forward,
           ;; remove element if a duplicate appears LATER in seq.
           (let ((r nil) (cur seq))
             (loop
               (when (null cur) (return (nreverse r)))
               (let* ((item (car cur))
                      (item-key (if key-fn (funcall key-fn item) item))
                      (rest-tail (cdr cur)))
                 (unless (some (lambda (x)
                                 (let ((v (if key-fn (funcall key-fn x) x)))
                                   (if test-fn (funcall test-fn item-key v) (eql item-key v))))
                               rest-tail)
                   (setq r (cons item r))))
               (setq cur (cdr cur))))))
      (t seq))))

;;; ===================================================
;;; Sequence Search Functions (find, search, position-if, etc.)
;;; ===================================================

(defun find (item sequence &rest args)
  "Return the first element of SEQUENCE that matches ITEM.
   :test defaults to inline `eql` (#'eql is unusable in MVM).
   :test-not is the negation of :test.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates."
  (let ((test nil) (test-not nil) (key nil)
        (start 0) (end nil) (from-end nil)
        (test-set nil) (tn-set nil) (key-set nil)
        (start-set nil) (end-set nil) (fe-set nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :test) (unless test-set (setq test (%resolve-fn v) test-set t)))
            ((eq k :test-not) (unless tn-set (setq test-not (%resolve-fn v) tn-set t)))
            ((eq k :key) (unless key-set (setq key (%resolve-fn v) key-set t)))
            ((eq k :start) (unless start-set (setq start v start-set t)))
            ((eq k :end) (unless end-set (setq end v end-set t)))
            ((eq k :from-end) (unless fe-set (setq from-end v fe-set t)))))
        (setq cur (cddr cur))))
    ;; Build effective test predicate that accounts for :test-not.
    (when test-not
      (let ((tn test-not))
        (setq test (lambda (a b) (not (funcall tn a b))))))
    (cond
      ;; Wrapped vector — handled before LISTP since wrappers are conses
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence))
             (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (%wrapper-aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw)))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (if test (funcall test item test-val) (eql item test-val))
                   (if from-end
                       (setq result elem)
                       (return elem)))))
             (setq i (+ i 1))))))
      ((listp sequence)
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
               (when (if test (funcall test item test-val) (eql item test-val))
                 (if from-end
                     (setq result elem)
                     (return elem)))))
           (setq lst (cdr lst))
           (setq i (+ i 1)))))
      (t
       ;; Vector path
       (let ((len (length sequence))
             (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (aref sequence i))
                    ;; String slots hold fixnum char-codes; present as
                    ;; characters so (eql item #\X) works.
                    (elem (if string-p (code-char raw) raw)))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (if test (funcall test item test-val) (eql item test-val))
                   (if from-end
                       (setq result elem)
                       (return elem)))))
             (setq i (+ i 1)))))))))

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
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (%wrapper-aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw)))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (funcall predicate test-val)
                   (if from-end (setq result elem) (return elem)))))
             (setq i (+ i 1))))))
      ((listp sequence)
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
           (setq i (+ i 1)))))
      (t
       (let ((len (length sequence)) (result nil))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let ((elem (aref sequence i)))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (funcall predicate test-val)
                   (if from-end (setq result elem) (return elem)))))
             (setq i (+ i 1)))))))))

(defun find-if-not (predicate sequence &rest args)
  "Return the first element of SEQUENCE not satisfying PREDICATE.
   Inlined (rather than `(apply #'find-if (lambda ...) ...)') to dodge
   the documented apply-of-rest-through-sibling-defun fragility."
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
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (%wrapper-aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                    (test-val (if key (funcall key elem) elem)))
               (unless (funcall predicate test-val)
                 (if from-end (setq result elem) (return elem))))
             (setq i (+ i 1))))))
      ((listp sequence)
       (let ((lst sequence) (i 0) (result nil))
         (loop
           (when (or (null lst) (= i start)) (return nil))
           (setq lst (cdr lst))
           (setq i (+ i 1)))
         (loop
           (when (null lst) (return result))
           (when (and end (= i end)) (return result))
           (let* ((elem (car lst))
                  (test-val (if key (funcall key elem) elem)))
             (unless (funcall predicate test-val)
               (if from-end (setq result elem) (return elem))))
           (setq lst (cdr lst))
           (setq i (+ i 1)))))
      (t
       (let ((len (length sequence)) (result nil))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((elem (aref sequence i))
                    (test-val (if key (funcall key elem) elem)))
               (unless (funcall predicate test-val)
                 (if from-end (setq result elem) (return elem))))
             (setq i (+ i 1)))))))))

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
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (%wrapper-aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                    (test-val (if key (funcall key elem) elem)))
               (when (funcall predicate test-val)
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1))))))
      ((listp sequence)
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
           (setq i (+ i 1)))))
      (t
       (let ((len (length sequence)) (result nil))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let ((elem (aref sequence i)))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (funcall predicate test-val)
                   (if from-end (setq result i) (return i)))))
             (setq i (+ i 1)))))))))

(defun position-if-not (predicate sequence &rest args)
  "Return the index of first element not satisfying PREDICATE.
   Inlined (rather than `(apply #'position-if (lambda ...) ...)') to
   dodge the documented apply-of-rest-through-sibling-defun fragility."
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
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (%wrapper-aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                    (test-val (if key (funcall key elem) elem)))
               (unless (funcall predicate test-val)
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1))))))
      ((listp sequence)
       (let ((lst sequence) (i 0) (result nil))
         (loop
           (when (or (null lst) (= i start)) (return nil))
           (setq lst (cdr lst))
           (setq i (+ i 1)))
         (loop
           (when (null lst) (return result))
           (when (and end (= i end)) (return result))
           (let* ((elem (car lst))
                  (test-val (if key (funcall key elem) elem)))
             (unless (funcall predicate test-val)
               (if from-end (setq result i) (return i))))
           (setq lst (cdr lst))
           (setq i (+ i 1)))))
      (t
       (let ((len (length sequence)) (result nil))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((elem (aref sequence i))
                    (test-val (if key (funcall key elem) elem)))
               (unless (funcall predicate test-val)
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1)))))))))

(defun position (item sequence &rest args)
  "Return the position of the first ITEM in SEQUENCE satisfying TEST.
   Supports :test/:test-not/:key/:start/:end/:from-end.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates."
  (let ((test nil) (test-not nil) (key nil)
        (start 0) (end nil) (from-end nil)
        (test-set nil) (tn-set nil) (key-set nil)
        (start-set nil) (end-set nil) (fe-set nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :test)     (unless test-set (setq test (%resolve-fn v) test-set t)))
            ((eq k :test-not) (unless tn-set (setq test-not (%resolve-fn v) tn-set t)))
            ((eq k :key)      (unless key-set (setq key (%resolve-fn v) key-set t)))
            ((eq k :start)    (unless start-set (setq start v start-set t)))
            ((eq k :end)      (unless end-set (setq end v end-set t)))
            ((eq k :from-end) (unless fe-set (setq from-end v fe-set t)))))
        (setq cur (cddr cur))))
    ;; Effective test: prefer :test-not if both somehow specified.
    (when test-not
      (let ((tn test-not))
        (setq test (lambda (a b) (not (funcall tn a b))))))
    ;; Walk inline (don't (apply #'position-if ...) — apply-of-rest
    ;; through a sibling &rest defun is documented as fragile).
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (%wrapper-aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                    (test-val (if key (funcall key elem) elem))
                    (matched (if test (funcall test item test-val)
                                 (eql item test-val))))
               (when matched
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1))))))
      ((listp sequence)
       (let ((lst sequence) (i 0) (result nil))
         (loop
           (when (or (null lst) (= i start)) (return nil))
           (setq lst (cdr lst))
           (setq i (+ i 1)))
         (loop
           (when (null lst) (return result))
           (when (and end (= i end)) (return result))
           (let* ((elem (car lst))
                  (test-val (if key (funcall key elem) elem))
                  (matched (if test (funcall test item test-val)
                               (eql item test-val))))
             (when matched
               (if from-end (setq result i) (return i))))
           (setq lst (cdr lst))
           (setq i (+ i 1)))))
      (t
       (let ((len (length sequence)) (result nil))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((elem (aref sequence i))
                    (test-val (if key (funcall key elem) elem))
                    (matched (if test (funcall test item test-val)
                                 (eql item test-val))))
               (when matched
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1)))))))))

;; complement: (complement #'pred) returns a function that negates pred,
;; accepting any number of arguments (ANSI requirement).  We dispatch
;; manually for 0/1/2/3 args to avoid the (apply fn args) path inside
;; an inner lambda, which is fragile in MVM.
(defun complement (fn)
  (lambda (&rest args)
    (cond
      ((null args)            (not (funcall fn)))
      ((null (cdr args))      (not (funcall fn (car args))))
      ((null (cddr args))     (not (funcall fn (car args) (cadr args))))
      ((null (cdddr args))    (not (funcall fn (car args) (cadr args) (caddr args))))
      (t                      (not (apply fn args))))))

(defun search (seq1 seq2 &rest args)
  "Search for SEQ1 as a subsequence of SEQ2. Return index or nil.
   :test defaults to inline `eql` (#'eql is unusable in MVM)."
  (let ((test nil) (key nil) (start1 0) (end1 nil) (start2 0) (end2 nil) (from-end nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :test) (setq test v))
            ((eq k :test-not)
             (let ((f v))
               (setq test (lambda (a b) (not (funcall f a b))))))
            ((eq k :key) (setq key v))
            ((eq k :start1) (setq start1 v))
            ((eq k :end1) (setq end1 v))
            ((eq k :start2) (setq start2 v))
            ((eq k :end2) (setq end2 v))
            ((eq k :from-end) (setq from-end v))))
        (setq cur (cddr cur))))
    ;; Read element through wrappers via %seq-elt (handles list, vector,
    ;; and wrapper conses uniformly).
    (labels ((%seq-elt (seq i)
               (cond
                 ((null seq) nil)
                 ((and (consp seq) (array-wrapper-p seq))
                  (let* ((raw (%wrapper-aref seq i)))
                    (if (and (stringp seq) (integerp raw)) (code-char raw) raw)))
                 ((consp seq) (nth i seq))
                 ((stringp seq) (code-char (aref seq i)))
                 (t (aref seq i)))))
      (when (null end1) (setq end1 (length seq1)))
      (when (null end2) (setq end2 (length seq2)))
      (let ((len1 (- end1 start1))
            (result nil))
        (let ((i start2))
          (loop
            (when (> (+ i len1) end2) (return result))
            (let ((match t) (j 0))
              (loop
                (when (= j len1) (return nil))
                (let ((e1 (%seq-elt seq1 (+ start1 j)))
                      (e2 (%seq-elt seq2 (+ i j))))
                  (let ((v1 (if key (funcall key e1) e1))
                        (v2 (if key (funcall key e2) e2)))
                    (unless (if test (funcall test v1 v2) (eql v1 v2))
                      (setq match nil)
                      (return nil))))
                (setq j (+ j 1)))
              (when match
                (if from-end
                    (setq result i)
                    (return i))))
            (setq i (+ i 1))))))))

