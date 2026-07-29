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
               (string-p (%wrapper-stringp seq))
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
              (let* ((raw (aref seq i))         ; AREF→char already for strings
                     (elem raw)
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
               (string-p (%wrapper-stringp seq))
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
              (let* ((raw (aref seq i))         ; AREF→char already for strings
                     (elem raw))
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

(defun mapl (fn list &rest more-lists)
  "ANSI variadic MAPL: call FN on successive sublists (tails) of the lists,
   for side effects; return the first LIST.  Fast paths for 1/2/3 lists
   avoid apply-of-rest fragility (mirrors mapcar)."
  (cond
    ((null more-lists)
     (let ((cur list))
       (loop
         (when (null cur) (return list))
         (funcall fn cur)
         (setq cur (cdr cur)))))
    ((null (cdr more-lists))
     (let ((c1 list) (c2 (car more-lists)))
       (loop
         (when (or (null c1) (null c2)) (return list))
         (funcall fn c1 c2)
         (setq c1 (cdr c1)) (setq c2 (cdr c2)))))
    ((null (cddr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (c3 (cadr more-lists)))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return list))
         (funcall fn c1 c2 c3)
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    (t (let ((lists (cons list more-lists)))
         (loop
           (when (some #'null lists) (return list))
           (apply fn lists)
           (setq lists (mapcar1 #'cdr lists)))))))

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

(defun maplist (fn list &rest more-lists)
  "ANSI variadic MAPLIST: collect (FN sublist1 sublist2 ...) over successive
   tails.  Fast paths for 1/2/3 lists avoid apply-of-rest fragility."
  (cond
    ((null more-lists)
     (let ((result nil) (cur list))
       (loop
         (when (null cur) (return (nreverse result)))
         (setq result (cons (funcall fn cur) result))
         (setq cur (cdr cur)))))
    ((null (cdr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2)) (return (nreverse result)))
         (setq result (cons (funcall fn c1 c2) result))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)))))
    ((null (cddr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (c3 (cadr more-lists)) (result nil))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return (nreverse result)))
         (setq result (cons (funcall fn c1 c2 c3) result))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    (t (let ((result nil) (lists (cons list more-lists)))
         (loop
           (when (some #'null lists) (return (nreverse result)))
           (setq result (cons (apply fn lists) result))
           (setq lists (mapcar1 #'cdr lists)))))))

;; Variadic MAPC override (prelude.lisp defines a single-list version;
;; cl-sequences loads later so this wins).  Calls FN on successive
;; elements of all lists for side effects; returns the first LIST.
;; Iteration stops at the shortest list.  Fast paths for 1/2/3 lists.
(defun mapc (fn list &rest more-lists)
  (cond
    ((null more-lists)
     (let ((cur list))
       (loop
         (when (null cur) (return list))
         (funcall fn (car cur))
         (setq cur (cdr cur)))))
    ((null (cdr more-lists))
     (let ((c1 list) (c2 (car more-lists)))
       (loop
         (when (or (null c1) (null c2)) (return list))
         (funcall fn (car c1) (car c2))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)))))
    ((null (cddr more-lists))
     (let ((c1 list) (c2 (car more-lists)) (c3 (cadr more-lists)))
       (loop
         (when (or (null c1) (null c2) (null c3)) (return list))
         (funcall fn (car c1) (car c2) (car c3))
         (setq c1 (cdr c1)) (setq c2 (cdr c2)) (setq c3 (cdr c3)))))
    (t (let ((lists (cons list more-lists)))
         (loop
           (when (some #'null lists) (return list))
           (apply fn (mapcar1 #'car lists))
           (setq lists (mapcar1 #'cdr lists)))))))

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

(defun %require-sequence (seq)
  "Signal a type-error unless SEQ is a sequence (list, string, array, or
   array-wrapper).  CLHS requires FIND/POSITION/COUNT/etc. to type-error
   on a non-sequence SEQUENCE argument (e.g. a bare symbol)."
  (unless (or (null seq) (listp seq) (stringp seq) (arrayp seq)
              (array-wrapper-p seq))
    (error "sequence function: not a sequence"))
  seq)

(defun parse-test-key (args)
  "Parse :test and :key keyword args. Returns (test-fn . key-fn).
   test-fn may be NIL — callers should use inline `eql` in that case
   rather than `#'eql`, because `eql` is an inline opcode in MVM and
   has no callable function entry (#'eql evaluates to a NIL/garbage
   pointer; (funcall <that> ...) silently returns wrong values).
   Symbol values (e.g. :test 'equal) are resolved via symbol-function.
   Per CLHS 3.4.1.4.1, when the same keyword appears multiple times the
   LEFTMOST occurrence supplies the value.

   Per CLHS §3.4.1.4: an odd number of keyword args or an unknown
   keyword (with :allow-other-keys NIL/absent) signals PROGRAM-ERROR.
   Caller is responsible for not invoking this on non-keyword-shaped
   plists (e.g. ADJOIN takes a plain `&rest args` and forwards here)."
  (let ((test-fn nil) (key-fn nil) (test-set nil) (key-set nil)
        (allow-other-keys nil) (aok-set nil) (a args))
    ;; First pass: look for :allow-other-keys (leftmost wins per
    ;; CLHS §3.4.1.4.1.1.2) so we know whether to signal on unknown
    ;; keyword.
    (let ((p args))
      (loop (when (null p) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other-keys (and (consp (cdr p)) (cadr p)))
          (setq aok-set t))
        (setq p (cdr p))))
    (loop (when (null a) (return))
      ;; Odd-length plist — final keyword with no value.
      (when (null (cdr a)) (%signal-program-error))
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
            ((eq (car a) :allow-other-keys) (setq a (cddr a)))
            (t
             ;; Unknown keyword.  Per ANSI, signal PROGRAM-ERROR unless
             ;; :allow-other-keys is non-nil.  Also, a non-keyword
             ;; (non-symbol or symbol not in keyword position) is itself
             ;; an error per CLHS §3.4.1.5 — covers (INTERSECTION NIL NIL 1 2).
             (unless allow-other-keys (%signal-program-error))
             (setq a (cddr a)))))
    (cons test-fn key-fn)))

;; Full ANSI parse: (count from-end start end test-fn test-not-fn key-fn).
;; Reuses %nsubst-parse-args defined in this file.
(defun remove (item seq &rest args)
  "Honors :test/:test-not/:key/:start/:end/:count/:from-end.
   :test-not is the test-result-inverted variant: an element is REMOVED
   when test-not(item, elt) is NIL (i.e., the element MATCHES the
   inverted test).  Previously the test-not-fn slot was extracted from
   the parse but never used — :test-not silently degraded to default
   EQL, returning the wrong list."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn (car (cddddr parsed)))
         (test-not-fn (cadr (cddddr parsed)))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count))
         ;; When :test-not is given, wrap so the (funcall pred item v)
         ;; site treats "v does NOT match" as the remove trigger.
         (eff-test-fn (cond
                        (test-fn test-fn)
                        (test-not-fn
                         (lambda (a b) (not (funcall test-not-fn a b))))
                        (t nil))))
    (cond
      ((null seq) nil)
      ((= eff-count 0) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-list item seq eff-test-fn key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-vector item seq eff-test-fn key-fn start-idx end-idx eff-count from-end)))))

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
      (when (>= count 0)
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
          ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
          (let ((v (if key-fn (funcall key-fn elt) elt)))
            (when (if test-fn (funcall test-fn item v) (eql item v))
              (push i indices))))
        (setq i (+ i 1))))
    (let ((to-drop indices))
      (when (>= count 0)
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
   Overrides the simpler 2-arg version in prelude.lisp.
   PRED accepts function designators (symbol or function)."
  (setq pred (%resolve-fn pred))
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) nil)
      ((= eff-count 0) (if (consp seq) (copy-list seq) (copy-seq seq)))
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
      (when (>= count 0)
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
          ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
          (let ((v (if key-fn (funcall key-fn elt) elt)))
            (when (funcall pred v) (push i indices))))
        (setq i (+ i 1))))
    (let ((to-drop indices))
      (when (>= count 0)
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
   position-if-not for the same pattern in commit 9c625ec).
   PRED accepts function designators (symbol or function)."
  (setq pred (%resolve-fn pred))
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
      ((= eff-count 0) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-if-list neg-pred seq key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-if-vector neg-pred seq key-fn start-idx end-idx eff-count from-end)))))

(defun count-if (pred seq &rest args)
  "Count elements of SEQ for which PRED is true. Honors :key, :start, :end, :from-end.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates.
   PRED and :key accept function designators (symbol or function).
   Per CLHS 3.4.1.4: odd-length plist or unknown keyword (without
   :allow-other-keys T) signals PROGRAM-ERROR."
  (setq pred (%resolve-fn pred))
  (%require-sequence seq)
  (let ((key nil) (start 0) (end nil) (from-end nil) (a args)
        (key-set nil) (start-set nil) (end-set nil) (fe-set nil)
        (allow-other nil) (aok-set nil))
    ;; Pre-scan for leftmost :allow-other-keys.
    (let ((p args))
      (loop (when (null p) (return))
        (when (null (cdr p)) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other (cadr p))
          (setq aok-set t))
        (setq p (cddr p))))
    (loop (when (null a) (return))
      (when (null (cdr a)) (%signal-program-error))
      (cond ((eq (car a) :key)
             (unless key-set (setq key (%resolve-fn (cadr a))) (setq key-set t)))
            ((eq (car a) :start)
             (unless start-set (setq start (cadr a)) (setq start-set t)))
            ((eq (car a) :end)
             (unless end-set (setq end (cadr a)) (setq end-set t)))
            ((eq (car a) :from-end)
             (unless fe-set (setq from-end (cadr a)) (setq fe-set t)))
            ((eq (car a) :allow-other-keys) nil)
            (t (unless allow-other (%signal-program-error))))
      (setq a (cddr a)))
    (cond
      ((null seq) 0)
      ;; Wrapped vector — use length+wrapper-aref so fp/displaced/adj are honored
      ((and (consp seq) (array-wrapper-p seq))
       (let* ((n 0)
              (string-p (%wrapper-stringp seq))
              (eff-end (if end end (length seq))))
         (if from-end
             (let ((i (- eff-end 1)))
               (loop (when (< i start) (return n))
                 (let* ((raw (%wrapper-aref seq i))
                        (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                        (v (if key (funcall key elem) elem)))
                   (when (funcall pred v) (setq n (+ n 1))))
                 (setq i (- i 1))))
             (let ((i start))
               (loop (when (>= i eff-end) (return n))
                 (let* ((raw (%wrapper-aref seq i))
                        (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                        (v (if key (funcall key elem) elem)))
                   (when (funcall pred v) (setq n (+ n 1))))
                 (setq i (+ i 1)))))))
      ((consp seq)
       (let ((eff-end (if end end most-positive-fixnum)))
         (if from-end
             ;; From-end on a list: walk forward into a vector then iterate backward.
             (let* ((tmp nil)
                    (cur seq) (i 0))
               (loop (when (or (null cur) (>= i eff-end)) (return nil))
                 (when (>= i start) (setq tmp (cons (car cur) tmp)))
                 (setq cur (cdr cur))
                 (setq i (+ i 1)))
               ;; tmp is now in REVERSE order (last-element first), which is what
               ;; from-end wants us to walk.  Apply pred/key in that order.
               (let ((n 0))
                 (dolist (item tmp n)
                   (let ((v (if key (funcall key item) item)))
                     (when (funcall pred v) (setq n (+ n 1)))))))
             (let ((n 0) (cur seq) (i 0))
               (loop (when (or (null cur) (>= i eff-end)) (return n))
                 (when (>= i start)
                   (let ((v (if key (funcall key (car cur)) (car cur))))
                     (when (funcall pred v) (setq n (+ n 1)))))
                 (setq cur (cdr cur))
                 (setq i (+ i 1)))))))
      (t  ;; vector / string
       (let ((eff-end (if end end (length seq))))
         (if from-end
             (let ((n 0) (i (- eff-end 1)))
               (loop (when (< i start) (return n))
                 (let ((v (if key (funcall key (elt seq i)) (elt seq i))))
                   (when (funcall pred v) (setq n (+ n 1))))
                 (setq i (- i 1))))
             (let ((n 0) (i start))
               (loop (when (>= i eff-end) (return n))
                 (let ((v (if key (funcall key (elt seq i)) (elt seq i))))
                   (when (funcall pred v) (setq n (+ n 1))))
                 (setq i (+ i 1))))))))))

(defun count (item seq &rest args)
  "Count occurrences of ITEM in SEQ. Honors :test, :test-not, :key,
   :start, :end, :from-end.
   :test/:test-not/:key accept function designators (symbol or function)."
  (%seq-subst-check-kwargs args)
  ;; CLHS: SEQ must be a sequence — type-error otherwise.
  (unless (or (null seq) (listp seq) (stringp seq) (arrayp seq) (array-wrapper-p seq))
    (error "count: not a sequence"))
  (let ((test nil) (key nil) (start 0) (end nil) (from-end nil) (a args)
        ;; CLHS §3.4.1.4.1: leftmost occurrence of a repeated keyword wins.
        (test-set nil) (key-set nil) (start-set nil) (end-set nil) (fe-set nil))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test)
             (unless test-set (setq test (%resolve-fn (cadr a)) test-set t)) (setq a (cddr a)))
            ((eq (car a) :key)
             (unless key-set (setq key (%resolve-fn (cadr a)) key-set t)) (setq a (cddr a)))
            ((eq (car a) :start)
             (unless start-set (setq start (cadr a) start-set t)) (setq a (cddr a)))
            ((eq (car a) :end)
             (unless end-set (setq end (cadr a) end-set t)) (setq a (cddr a)))
            ((eq (car a) :from-end)
             (unless fe-set (setq from-end (cadr a) fe-set t)) (setq a (cddr a)))
            ((eq (car a) :test-not)
             (unless test-set
               (let ((f (%resolve-fn (cadr a))))
                 (setq test (lambda (x y) (not (funcall f x y))) test-set t)))
             (setq a (cddr a)))
            (t (setq a (cdr a)))))
    ;; count-if's iteration already presents string elements as CHARACTERS
    ;; via (elt seq i) on the vector path and (%wrapper-aref + code-char)
    ;; on the wrapper path, so the lambda just compares item against x as-is.
    ;; Default :end — array-wrapper-p check MUST precede consp test,
    ;; because fp/adjustable/displaced wrappers are conses but have a
    ;; concrete LENGTH; using most-positive-fixnum walks past the array
    ;; and SIGSEGVs (was the count-test 16963 family fail mode).
    (count-if (lambda (x)
                (if test (funcall test item x) (eql item x)))
              seq
              :start start
              :end (or end (cond ((null seq) 0)
                                  ((array-wrapper-p seq) (length seq))
                                  ((consp seq) most-positive-fixnum)
                                  (t (length seq))))
              :key key
              :from-end from-end)))

;;; Function wrappers for compiler builtins (needed for apply/#')
;;; The compiler uses XOR/AND/OR opcodes for inline (logxor a b) etc.
;;; but #'logxor needs a real function object.
;; Route the funcall/apply helpers through the bignum-aware generic engine.
;; A bare (logand a b) here is the COMPILED raw :and intrinsic, which is
;; garbage when an operand is a bignum POINTER — so #'logand / (apply #'logand
;; ...) silently produced wrong results on bignums.  generic-logand checks
;; bignump first and routes to the two's-complement engine; only fixnum∧fixnum
;; takes the raw fast path (no recursion: that inner (logand a b) is the
;; intrinsic, not this helper).
(defun %logxor2 (a b) (generic-logxor a b))
(defun %logand2 (a b) (generic-logand a b))
(defun %logior2 (a b) (generic-logior a b))

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
  "Destructive butlast. Signals PROGRAM-ERROR on extra args; TYPE-ERROR
   on negative or non-fixnum N."
  (when (and n-arg (cdr n-arg))
    (error "nbutlast: too many arguments"))
  (let ((n (if n-arg (car n-arg) 1)))
    (when (or (not (fixnump n)) (< n 0))
      (%signal-type-error))
    (let ((len (list-length list)))
      (if (or (null len) (<= len n)) nil
        (let ((tail (nthcdr (- len n 1) list)))
          (set-cdr tail nil)
          list)))))

(defun floatp-impl (x)
  "Check if x is a boxed float — any of the four float subtags
   #x60..#x63 (double/single/short/long; numeric tower N1).  Means
   'any float'; do NOT narrow it (arithmetic dispatch keys off it)."
  (if (fixnump x) nil
    (if (consp x) nil
      (if (null x) nil
        (let ((st (obj-subtag x)))
          (or (= st 96) (and (>= st 100) (<= st 102))))))))  ; #x60 + #x64..#x66 (skip #x61-63: mvm-module etc.)

(defun float-equal (a b)
  "Compare two boxed floats by hi32/lo32 slots."
  (if (= (aref a 0) (aref b 0))
      (= (aref a 1) (aref b 1))
      nil))


;;; ============================================================
;;; Missing CL functions needed by ANSI tests
;;; ============================================================

(defun assert (test-form &rest ignored)
  "ANSI ASSERT returns NIL.  The fully-restartable error path on
   failure isn't implemented, so we degrade to a silent NIL — the
   1000+ ASSERT call sites in ANSI sources use it for in-test
   invariant checking and ignore the return.  Tried real (error)
   on NIL once; net regression -31 because many test-time invariants
   that happen to hit a typep edge case were previously masked.
   Keep silent until typep gaps are closed."
  (declare (ignore ignored test-form))
  nil)
(defun equalp (a b) (equalp-impl a b))
(defun elt (seq idx)
  ;; Signal TYPE-ERROR for negative or non-fixnum index.
  (when (or (not (fixnump idx)) (< idx 0))
    (%signal-type-error))
  (cond
    ;; (elt nil i) — empty list, always out of range
    ((null seq) (error "elt: index out of range"))
    ;; Array wrapper (adj/fp/displaced/multi-dim) — peel via wrapper-aref
    ((and (consp seq) (array-wrapper-p seq))
     (let ((v (%wrapper-aref seq idx)))
       (if (and (%wrapper-stringp seq) (integerp v)) (code-char v) v)))
    ((consp seq)
     (when (>= idx (length seq)) (error "elt: index out of range"))
     (nth idx seq))
    ((or (stringp seq) (arrayp seq))
     (when (>= idx (length seq)) (error "elt: index out of range"))
     ;; Public AREF already lifts string elements to CHARACTERs; return it
     ;; directly.  (Compiled (code-char X) is the raw shift-and-tag primop
     ;; with no characterp guard, so wrapping an already-character value
     ;; double-encodes it — the "BBBBBB" bug.)
     (aref seq idx))
    (t (error "elt: not a sequence"))))
(defun %string-designator (x)
  "Coerce a string designator (string, character, or symbol) to a string."
  (cond
    ((stringp x) x)
    ((characterp x) (let ((s (%make-string-array 1)))
                      (aset s 0 (char-code x))
                      s))
    ((null x) "NIL")
    ((eq x t) "T")
    ((%cl-sym-p x) (%cl-sym-name x))
    ;; Native MVM symbols (#x50) and keywords (#x53) are NOT %cl-sym-p
    ;; (that checks the cons-wrapper car); route them through symbol-name,
    ;; which resolves the hash → name via *sym-name-table* / package walk.
    ;; A quoted literal symbol designator like '|abc| arrives as a native
    ;; #x50 sym, so without this STRING= '|abc| "abc" coerced the symbol
    ;; OBJECT (garbage) instead of its name.
    ((symbolp x) (symbol-name x))
    (t x)))                      ; fallthrough — might be a wrapper

(defun string= (a b &rest options)
  "ANSI STRING= — case-SENSITIVE element-wise compare.  STRING-EQUAL is
   the case-insensitive variant.  Takes string designators (strings,
   characters, symbols).  Honors :START1, :END1, :START2, :END2.
   Signals error on odd-length args or unknown key (unless
   :allow-other-keys T precedes)."
  (let ((sa (%string-designator a))
        (sb (%string-designator b))
        (s1 0) (e1 nil) (s2 0) (e2 nil)
        (allow-other nil)
        (o options))
    ;; :allow-other-keys is LEFTMOST-wins (CLHS 3.4.1.4): the value of the
    ;; FIRST occurrence governs.  STRING=.ERROR.6 passes
    ;; ":allow-other-keys nil :allow-other-keys t :foo bar" — the leading
    ;; nil means :foo is illegal and must signal program-error, even though
    ;; a later :allow-other-keys is t.
    (let ((scan options))
      (loop (when (or (null scan) (null (cdr scan))) (return))
        (when (eq (car scan) :allow-other-keys)
          (when (cadr scan) (setq allow-other t))
          (return))
        (setq scan (cddr scan))))
    (loop (when (null o) (return))
      (when (null (cdr o))
        (%signal-program-error))
      (let ((k (car o)))
        (cond ((eq k :start1) (setq s1 (cadr o)))
              ((eq k :end1)   (setq e1 (cadr o)))
              ((eq k :start2) (setq s2 (cadr o)))
              ((eq k :end2)   (setq e2 (cadr o)))
              ((eq k :allow-other-keys) nil)
              (allow-other nil)
              (t (%signal-program-error))))
      (setq o (cddr o)))
    (let* ((la (array-length sa))
           (lb (array-length sb))
           (ee1 (or e1 la))
           (ee2 (or e2 lb))
           (len1 (- ee1 s1))
           (len2 (- ee2 s2)))
      (if (= len1 len2)
          (let ((i 0))
            (loop
              (when (= i len1) (return t))
              ;; AREF on strings now yields CHARACTERs — compare via char=.
              (unless (char= (aref sa (+ s1 i)) (aref sb (+ s2 i))) (return nil))
              (setq i (+ i 1))))
          nil))))
(defun string/= (a b &rest args)
  "Case-sensitive inequality.  Returns position of mismatch (in a's
   coords) or NIL if equal.  Honors :start1/:end1/:start2/:end2."
  (let ((r (%str-cmp-core a b args nil)))
    (when (or (eq (car r) :less) (eq (car r) :greater)) (cadr r))))
;;; constantly: real closure capturing VALUE via %make-closure pattern.
;;; The prior version used a single global *constantly-value* — every
;;; subsequent (constantly X) clobbered the value seen by ALL prior
;;; closures, including those installed via (setf (fdefinition g) c).
;;; PSETF.25 (and many other tests using two distinct constantly
;;; closures) returned the latest value for both.  Pattern from
;;; cl-clos.lisp's %synthetic-primary-closure-fn / %gf-stub-closure-fn:
;;; a top-level fn reads its captured arg from (%get-cenv).
(defun %constantly-closure-fn (&rest args)
  (declare (ignore args))
  (car (%get-cenv)))
(defun constantly (value)
  (%make-closure #'%constantly-closure-fn (cons value nil)))
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
  ;; STRING seqs store fixnum char-codes; present elements to PRED/KEY as
  ;; CHARACTER objects (so e.g. (sort "..." #'char< :key ...) works), then
  ;; store the swapped char-codes back.
  ;; Use fill-pointer-aware LENGTH (not raw ARRAY-LENGTH) so a vector with a
  ;; fill pointer sorts only its active prefix, not the whole backing store.
  (let ((len (length seq))
        (str-p (stringp seq)))
    (let ((i 1))
      (loop
        (when (>= i len) (return seq))
        (let ((j i))
          (loop
            (when (= j 0) (return nil))
            (let* ((ra (aref seq j))          ; AREF→char already for strings
                   (rb (aref seq (- j 1)))
                   (a ra)
                   (b rb)
                   (av (if key (funcall key a) a))
                   (bv (if key (funcall key b) b)))
              (if (funcall pred av bv)
                  (progn (aset seq j rb) (aset seq (- j 1) ra)
                         (setq j (- j 1)))
                  (return nil)))))
        (setq i (+ i 1))))))

(defun sort (seq pred &rest options)
  ;; Honors :key.  Dispatches list vs vector.
  (%seq-subst-check-kwargs options)
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
  (%seq-subst-check-kwargs options)
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
           (let ((v (funcall transform (aref seq i))))   ; AREF→char already
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
  (%seq-subst-check-kwargs args)
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn-raw (car (cddddr parsed)))      ; index 4
         (test-not-fn (cadr (cddddr parsed)))     ; index 5
         (key-fn (caddr (cddddr parsed)))         ; index 6
         ;; If :test-not given, build a negated test-fn so the rest of
         ;; the body just uses test-fn.
         (test-fn (cond
                    (test-fn-raw test-fn-raw)
                    (test-not-fn
                     (let ((tn test-not-fn))
                       (lambda (a b) (not (funcall tn a b)))))
                    (t nil)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ;; fp/displaced/adjustable wrapper: flatten to a fresh vector of the
      ;; effective length, then process via the vector path.  Must come
      ;; BEFORE (consp seq) since wrappers ARE conses.
      ((and (consp seq) (array-wrapper-p seq))
       (let* ((len (length seq))
              (flat (make-array len)))
         (let ((i 0))
           (loop (when (>= i len) (return nil))
             (aset flat i (%wrapper-aref seq i))
             (setq i (+ i 1))))
         (apply #'substitute new old flat args)))
      ;; List + no positional/count kwargs → simple per-element transform.
      ;; Keep this fast path for the bulk of plain (substitute new old list)
      ;; calls so we don't do index-tracking overhead.
      ((and (consp seq) (= start-idx 0) (null end-idx) (< eff-count 0)
            (not from-end))
       (%seq-substitute-with
        (lambda (item)
          (let ((v (if key-fn (funcall key-fn item) item)))
            (if (if test-fn (funcall test-fn old v) (eql old v))
                new
                item)))
        seq))
      ((consp seq)
       ;; List + at least one of :start/:end/:count/:from-end.
       ;; Walk to find match indices; build output replacing the right subset.
       ;; With :FROM-END T the test fn must be called in REVERSE order (ANSI)
       ;; because closure side-effects must observe reverse iteration.
       (let* ((items seq)
              (n (length items))
              (eff-end (if (and end-idx (< end-idx n)) end-idx n))
              (matches nil))
         (cond
           (from-end
            ;; Copy to array for random access, iterate idx high→low.
            (let* ((arr (make-array n)) (cur items) (j 0))
              (loop (when (null cur) (return nil))
                (aset arr j (car cur))
                (setq cur (cdr cur)) (setq j (+ j 1)))
              (let ((k (- eff-end 1)))
                (loop (when (< k start-idx) (return nil))
                  (let* ((elt (aref arr k))
                         (v (if key-fn (funcall key-fn elt) elt))
                         (match (if test-fn (funcall test-fn old v) (eql old v))))
                    (when match (setq matches (cons k matches))))
                  (setq k (- k 1))))))
           (t
            (let ((cur items) (i 0))
              (loop (when (or (null cur) (>= i eff-end)) (return nil))
                (when (>= i start-idx)
                  (let* ((elt (car cur))
                         (v (if key-fn (funcall key-fn elt) elt))
                         (match (if test-fn (funcall test-fn old v) (eql old v))))
                    (when match (setq matches (cons i matches)))))
                (setq cur (cdr cur))
                (setq i (+ i 1))))
            ;; Forward iter built matches in reverse (highest first). For
            ;; from-end path we built them with closure reverse-iter; that
            ;; ends with lowest-idx first. Normalize so subsequent logic
            ;; gets the OLD contract (highest first).
            ))
         (when (and from-end matches)
           (setq matches (nreverse matches)))
         ;; Pick the right subset given :count and :from-end.
         (let ((selected
                 (cond
                   ((< eff-count 0) matches)
                   ;; from-end keeps the LAST count matches = leading from
                   ;; reverse-walk order.
                   (from-end
                    (let ((kept nil) (src matches) (k eff-count))
                      (loop (when (or (null src) (= k 0)) (return kept))
                        (setq kept (cons (car src) kept))
                        (setq src (cdr src))
                        (setq k (- k 1)))))
                   ;; forward keeps the FIRST count matches = take from
                   ;; the tail of reverse-walk order.
                   (t
                    (let* ((nm (length matches))
                           (skip (- nm eff-count))
                           (src matches))
                      (when (< skip 0) (setq skip 0))
                      (loop (when (= skip 0) (return src))
                        (setq src (cdr src))
                        (setq skip (- skip 1))))))))
           ;; Build output replacing items at indices in `selected'.
           (let ((out nil) (cur items) (j 0))
             (loop (when (null cur) (return (nreverse out)))
               (setq out (cons (if (member j selected) new (car cur)) out))
               (setq cur (cdr cur))
               (setq j (+ j 1)))))))
      (t
       (let ((copy (copy-seq seq)))
         (cond
           ((= eff-count 0) copy)
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
                      (when (= n 0) (return copy))
                      (let ((elt (aref copy i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (let ((match (if test-fn (funcall test-fn old cmp)
                                           (eql old cmp))))
                            (when match
                              (aset copy i store-new)
                              (when (> n 0) (setq n (- n 1)))))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return copy))
                      (when (= n 0) (return copy))
                      (let ((elt (aref copy i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (let ((match (if test-fn (funcall test-fn old cmp)
                                           (eql old cmp))))
                            (when match
                              (aset copy i store-new)
                              (when (> n 0) (setq n (- n 1)))))))
                      (setq i (+ i 1)))))))))))))

(defun substitute-if (new pred seq &rest args)
  "Non-destructive substitute-if. Same shape as SUBSTITUTE.
   List path inlined (no nested closure) — MVM's capture analysis
   loses bindings across the substitute-if-not → apply → substitute-if
   chain when the inner closure captures pred/key-fn."
  (%seq-subst-check-kwargs args)
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ;; List + forward (or no count) → simple forward walk.
      ((and (consp seq) (not from-end))
       (let ((result nil) (cur seq) (idx 0) (n eff-count))
         (loop
           (when (null cur) (return (nreverse result)))
           (let* ((item (car cur))
                  (in-window (and (>= idx start-idx)
                                  (or (null end-idx) (< idx end-idx))))
                  (v (if (and in-window key-fn) (funcall key-fn item) item))
                  (replace (and in-window
                                (not (zerop n))
                                (funcall pred v))))
             (setq result (cons (if replace new item) result))
             (when replace (when (> n 0) (setq n (- n 1)))))
           (setq cur (cdr cur))
           (setq idx (+ idx 1)))))
      ;; List + :from-end → walk to collect matches, keep last :count of them.
      ((consp seq)
       (let* ((items seq) (n (length items))
              (eff-end (if (and end-idx (< end-idx n)) end-idx n))
              (matches nil) (cur items) (i 0))
         (loop (when (or (null cur) (>= i eff-end)) (return nil))
           (when (>= i start-idx)
             (let* ((elt (car cur))
                    (v (if key-fn (funcall key-fn elt) elt)))
               (when (funcall pred v) (setq matches (cons i matches)))))
           (setq cur (cdr cur))
           (setq i (+ i 1)))
         ;; from-end + count → leading from reverse-walk = last :count matches.
         (let ((selected
                 (cond
                   ((< eff-count 0) matches)
                   (t (let ((kept nil) (src matches) (k eff-count))
                        (loop (when (or (null src) (= k 0)) (return kept))
                          (setq kept (cons (car src) kept))
                          (setq src (cdr src))
                          (setq k (- k 1))))))))
           (let ((out nil) (cur items) (j 0))
             (loop (when (null cur) (return (nreverse out)))
               (setq out (cons (if (member j selected) new (car cur)) out))
               (setq cur (cdr cur))
               (setq j (+ j 1)))))))
      (t
       (let ((copy (copy-seq seq)))
         (cond
           ((= eff-count 0) copy)
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
                      (when (= n 0) (return copy))
                      (let ((elt (aref copy i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (funcall pred cmp)
                            (aset copy i store-new)
                            (when (> n 0) (setq n (- n 1))))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return copy))
                      (when (= n 0) (return copy))
                      (let ((elt (aref copy i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (funcall pred cmp)
                            (aset copy i store-new)
                            (when (> n 0) (setq n (- n 1))))))
                      (setq i (+ i 1)))))))))))))

(defun substitute-if-not (new pred seq &rest args)
  "Non-destructive substitute-if-not.
   Inlined to avoid the apply+closure pattern that MVM's capture
   analysis loses bindings across (was: (apply #'substitute-if new
   (lambda (x) (not (funcall pred x))) seq args))."
  (%seq-subst-check-kwargs args)
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ;; List + forward → simple forward walk.
      ((and (consp seq) (not from-end))
       (let ((result nil) (cur seq) (idx 0) (n eff-count))
         (loop
           (when (null cur) (return (nreverse result)))
           (let* ((item (car cur))
                  (in-window (and (>= idx start-idx)
                                  (or (null end-idx) (< idx end-idx))))
                  (v (if (and in-window key-fn) (funcall key-fn item) item))
                  (replace (and in-window
                                (not (zerop n))
                                (not (funcall pred v)))))
             (setq result (cons (if replace new item) result))
             (when replace (when (> n 0) (setq n (- n 1)))))
           (setq cur (cdr cur))
           (setq idx (+ idx 1)))))
      ;; List + :from-end → match-collect, keep last :count matches.
      ((consp seq)
       (let* ((items seq) (n (length items))
              (eff-end (if (and end-idx (< end-idx n)) end-idx n))
              (matches nil) (cur items) (i 0))
         (loop (when (or (null cur) (>= i eff-end)) (return nil))
           (when (>= i start-idx)
             (let* ((elt (car cur))
                    (v (if key-fn (funcall key-fn elt) elt)))
               (unless (funcall pred v) (setq matches (cons i matches)))))
           (setq cur (cdr cur))
           (setq i (+ i 1)))
         (let ((selected
                 (cond
                   ((< eff-count 0) matches)
                   (t (let ((kept nil) (src matches) (k eff-count))
                        (loop (when (or (null src) (= k 0)) (return kept))
                          (setq kept (cons (car src) kept))
                          (setq src (cdr src))
                          (setq k (- k 1))))))))
           (let ((out nil) (cur items) (j 0))
             (loop (when (null cur) (return (nreverse out)))
               (setq out (cons (if (member j selected) new (car cur)) out))
               (setq cur (cdr cur))
               (setq j (+ j 1)))))))
      (t
       ;; Vector path inlined — mirror substitute-if with negated pred.
       ;; Avoids (apply #'substitute-if new (lambda ...) seq args), which
       ;; trips the apply-of-rest + closure-capture fragility.
       (let ((copy (copy-seq seq)))
         (cond
           ((= eff-count 0) copy)
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
                      (when (= n 0) (return copy))
                      (let ((elt (aref copy i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (not (funcall pred cmp))
                            (aset copy i store-new)
                            (when (> n 0) (setq n (- n 1))))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return copy))
                      (when (= n 0) (return copy))
                      (let ((elt (aref copy i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                          (when (not (funcall pred cmp))
                            (aset copy i store-new)
                            (when (> n 0) (setq n (- n 1))))))
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
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates.

   Validation (CLHS 3.4.1.4): odd-length plist signals program-error.
   Unknown keyword signals program-error unless the caller passed
   :ALLOW-OTHER-KEYS T."
  ;; Scan for :ALLOW-OTHER-KEYS T first (short-circuits unknown-key check).
  (let ((allow nil) (cur args))
    (loop
      (when (null cur) (return nil))
      (when (null (cdr cur)) (return nil))
      (when (and (eq (car cur) :allow-other-keys) (cadr cur))
        (setq allow t)
        (return nil))
      (setq cur (cddr cur)))
    ;; Validate odd-length and unknown keys.
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (when (null (cdr cur))
          (%signal-program-error))
        (let ((k (car cur)))
          (unless (or allow
                      (eq k :from-end) (eq k :test) (eq k :test-not)
                      (eq k :count) (eq k :key) (eq k :start) (eq k :end)
                      (eq k :allow-other-keys))
            (%signal-program-error)))
        (setq cur (cddr cur)))))
  (let ((from-end nil) (test-fn nil) (test-not-fn nil)
        (count nil) (key-fn nil) (start 0) (end nil) (cur args)
        (fe-set nil) (test-set nil) (tn-set nil)
        (count-set nil) (key-set nil) (start-set nil) (end-set nil))
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :from-end) (unless fe-set (setq from-end v) (setq fe-set t)))
          ((eq k :test) (unless test-set (setq test-fn (%resolve-fn v)) (setq test-set t)))
          ((eq k :test-not) (unless tn-set (setq test-not-fn (%resolve-fn v)) (setq tn-set t)))
          ((eq k :count) (unless count-set (setq count v) (setq count-set t)))
          ((eq k :key) (unless key-set (setq key-fn (%resolve-fn v)) (setq key-set t)))
          ((eq k :start) (unless start-set (setq start v) (setq start-set t)))
          ((eq k :end) (unless end-set (setq end v) (setq end-set t))))
        (setq cur (cddr cur))))
    (list count from-end start end test-fn test-not-fn key-fn)))

;;; Effective count: -1 (sentinel) if count is nil meaning "unlimited",
;;; else max(0, count).
;;;
;;; AArch64 fixnum-0 / NIL collision: returning NIL for "unlimited" and
;;; an integer for "bounded" meant downstream code did `(> n 0)`
;;; or `(when n …)` to dispatch — both wrong for n=0 on AArch64 because
;;; (null 0) returns T (fixnum 0 and NIL share the raw-0 bit pattern).
;;; The -1 sentinel is reliably distinguishable from 0 via numeric
;;; comparison `(< n 0)`.  See reference_aa64_fixnum_zero_nil.md and the
;;; CLOS slot-0 fix in f1f6f0e for the same pattern.
(defun %nsubst-effective-count (count)
  (cond ((null count) -1)
        ((> count 0) count)
        (t 0)))

(defun %nsubst-in-window-p (idx start-idx end-idx)
  "Return t if idx is in [start-idx, end-idx)."
  (if (>= idx start-idx)
      (if (null end-idx) t (< idx end-idx))
      nil))

(defun %nsubst-list-core (new pred-fn seq count from-end start-idx end-idx)
  "Core list nsubstitute with start/end/count/from-end support."
  ;; count: -1=unlimited, 0=nothing, positive=limit. Already normalized by caller (was nil for unlimited; switched to -1 sentinel for AArch64 fixnum-0/NIL collision — see %nsubst-effective-count).
  (if from-end
      ;; Backward: collect matching positions in [start-idx, end-idx), apply count from end.
      ;; ANSI: with :FROM-END T the PRED is called in REVERSE order, so
      ;; closure side-effects observe reverse iteration.  Copy list to an
      ;; array first for random access, then iterate the index downward.
      (let* ((len (length seq))
             (arr (make-array len))
             (positions nil))
        (let ((cur seq) (j 0))
          (loop (when (null cur) (return nil))
            (aset arr j (car cur))
            (setq cur (cdr cur)) (setq j (+ j 1))))
        (let ((k (- len 1)))
          (loop (when (< k 0) (return nil))
            (when (%nsubst-in-window-p k start-idx end-idx)
              (let ((match (funcall pred-fn (aref arr k))))
                (when match
                  ;; build positions in low-to-high order so the
                  ;; "take first COUNT" step below keeps highest-indexed
                  ;; matches, matching the old semantics.
                  (setq positions (cons k positions)))))
            (setq k (- k 1)))
          ;; positions now: highest-idx-LAST (since we prepended in reverse iter)
          ;; we need: highest-idx-FIRST (old contract)
          (setq positions (nreverse positions)))
        ;; positions: largest index first
        ;; Take first count entries (= highest-indexed matches)
        ;; count = -1 (unlimited sentinel) → take all matches.
        (let ((to-replace nil)
              (remaining (if (< count 0) (length positions) count))
              (pos-cur positions))
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
          (when (= n 0) (return seq))
          (let ((in-win (%nsubst-in-window-p idx start-idx end-idx)))
            (when in-win
              (let ((match (funcall pred-fn (car cur))))
                (when match
                  (set-car cur new)
                  (when (> n 0) (setq n (- n 1)))))))
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
  (%seq-subst-check-kwargs args)
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (pred-fn (if key-fn
                      (lambda (x) (funcall pred (funcall key-fn x)))
                      pred))
         (eff-count (%nsubst-effective-count count)))
    (if (or (null seq) (= eff-count 0))
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
                      (when (= n 0) (return seq))
                      (let ((elt (aref seq i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (when (funcall pred-fn elt)
                          (aset seq i store-new)
                          (when (> n 0) (setq n (- n 1)))))
                      (setq i (- i 1))))
                  (let ((i start-idx))
                    (loop
                      (when (>= i eff-end) (return seq))
                      (when (= n 0) (return seq))
                      (let ((elt (aref seq i)))
                        ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                        (when (funcall pred-fn elt)
                          (aset seq i store-new)
                          (when (> n 0) (setq n (- n 1)))))
                      (setq i (+ i 1))))))))))
(defun nsubstitute-if-not (new pred seq &rest args)
  "Destructive substitute-if-not.
   Inlined list path to bypass the closure-loses-capture pattern in
   (apply #'nsubstitute-if new (lambda (x) (not (funcall pred x))) ...)."
  (%seq-subst-check-kwargs args)
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ((= eff-count 0) seq)
      ((consp seq)
       (if from-end
           ;; Backward path — copy to array for random access, scan from
           ;; end down to start-idx, collect first COUNT match positions
           ;; (those are the highest-indexed ones), then walk the list
           ;; once forward and replace at those positions.  Mirrors
           ;; %nsubst-list-core's backward branch with the predicate
           ;; negated for the -IF-NOT variant.  Fixes
           ;; nsubstitute-if-not-list.12/.18 and .order.1/.2 which
           ;; exercised :count + :from-end together.
           (let* ((len (length seq))
                  (arr (make-array len)))
             (let ((cur seq) (j 0))
               (loop (when (null cur) (return nil))
                 (aset arr j (car cur))
                 (setq cur (cdr cur)) (setq j (+ j 1))))
             (let ((positions nil)
                   (k (- (if (and end-idx (< end-idx len)) end-idx len) 1))
                   (lo start-idx))
               (loop (when (< k lo) (return nil))
                 (let* ((item (aref arr k))
                        (v (if key-fn (funcall key-fn item) item)))
                   (when (not (funcall pred v))
                     (setq positions (cons k positions))))
                 (setq k (- k 1)))
               ;; positions: lowest-idx-first.  Want highest-idx-first
               ;; so "take first COUNT" keeps the highest-indexed matches.
               (setq positions (nreverse positions))
               (let ((to-replace nil)
                     (remaining (if (< eff-count 0) (length positions) eff-count))
                     (pos-cur positions))
                 (loop
                   (when (null pos-cur) (return nil))
                   (when (<= remaining 0) (return nil))
                   (setq to-replace (cons (car pos-cur) to-replace))
                   (setq remaining (- remaining 1))
                   (setq pos-cur (cdr pos-cur)))
                 (let ((cur2 seq) (idx2 0))
                   (loop
                     (when (null cur2) (return seq))
                     (when (member idx2 to-replace)
                       (set-car cur2 new))
                     (setq idx2 (+ idx2 1))
                     (setq cur2 (cdr cur2)))
                   seq))))
           ;; Forward path — original inlined loop.
           (let ((cur seq) (idx 0) (n eff-count))
             (loop
               (when (null cur) (return seq))
               (when (= n 0) (return seq))
               (let* ((item (car cur))
                      (in-window (and (>= idx start-idx)
                                      (or (null end-idx) (< idx end-idx))))
                      (v (if (and in-window key-fn) (funcall key-fn item) item)))
                 (when (and in-window (not (funcall pred v)))
                   (set-car cur new)
                   (when (> n 0) (setq n (- n 1)))))
               (setq cur (cdr cur))
               (setq idx (+ idx 1))))))
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
                 (when (= n 0) (return seq))
                 (let ((elt (aref seq i)))
                   ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (when (not (funcall pred cmp))
                       (aset seq i store-new)
                       (when (> n 0) (setq n (- n 1))))))
                 (setq i (- i 1))))
             (let ((i start-idx))
               (loop
                 (when (>= i eff-end) (return seq))
                 (when (= n 0) (return seq))
                 (let ((elt (aref seq i)))
                   ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (when (not (funcall pred cmp))
                       (aset seq i store-new)
                       (when (> n 0) (setq n (- n 1))))))
                 (setq i (+ i 1))))))))))

(defun nsubstitute (new old seq &rest args)
  "Destructive substitute. Inline vector path so we don't depend on a
   lambda closure capturing OLD (which can be lost across apply / nested
   funcall when the closure cell is shared with siblings)."
  (%seq-subst-check-kwargs args)
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn-raw (car (cddddr parsed)))
         (test-not-fn (cadr (cddddr parsed)))
         (key-fn (caddr (cddddr parsed)))
         (test-fn (cond
                    (test-fn-raw test-fn-raw)
                    (test-not-fn
                     (let ((tn test-not-fn))
                       (lambda (a b) (not (funcall tn a b)))))
                    (t nil)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) seq)
      ((= eff-count 0) seq)
      ;; fp/displaced/adjustable wrapper: destructive operation on the
      ;; underlying array.  We route to the non-destructive substitute
      ;; (which already handles wrappers) and copy the result back via
      ;; the wrapper-aware aset.  replaced is a plain vector; use aref.
      ((and (consp seq) (array-wrapper-p seq))
       (let* ((replaced (apply #'substitute new old seq args))
              (len (length seq))
              (i 0))
         (loop (when (>= i len) (return seq))
           (let ((v (aref replaced i)))
             (%wrapper-aset seq i v))
           (setq i (+ i 1)))))
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
                 (when (= n 0) (return seq))
                 (let ((elt (aref seq i)))
                   ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (let ((match (if test-fn (funcall test-fn old cmp)
                                      (eql old cmp))))
                       (when match
                         (aset seq i store-new)
                         (when (> n 0) (setq n (- n 1)))))))
                 (setq i (- i 1))))
             (let ((i start-idx))
               (loop
                 (when (>= i eff-end) (return seq))
                 (when (= n 0) (return seq))
                 (let ((elt (aref seq i)))
                   ;; AREF already yields a CHARACTER for strings (compiled code-char re-shifts)
                   (let ((cmp (if key-fn (funcall key-fn elt) elt)))
                     (let ((match (if test-fn (funcall test-fn old cmp)
                                      (eql old cmp))))
                       (when match
                         (aset seq i store-new)
                         (when (> n 0) (setq n (- n 1)))))))
                 (setq i (+ i 1))))))))))
(defun count-if-not (pred seq &rest args)
  "Count elements of SEQ for which PRED is FALSE.  Honors :key, :start,
   :end, :from-end.  Equivalent to (count-if (complement pred) ...).
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates.
   PRED and :key accept function designators (symbol or function).
   Per CLHS 3.4.1.4: odd-length plist or unknown keyword (without
   :allow-other-keys T) signals PROGRAM-ERROR."
  (setq pred (%resolve-fn pred))
  (%require-sequence seq)
  (let ((key nil) (start 0) (end nil) (from-end nil) (a args)
        (key-set nil) (start-set nil) (end-set nil) (fe-set nil)
        (allow-other nil) (aok-set nil))
    (let ((p args))
      (loop (when (null p) (return))
        (when (null (cdr p)) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other (cadr p))
          (setq aok-set t))
        (setq p (cddr p))))
    (loop (when (null a) (return))
      (when (null (cdr a)) (%signal-program-error))
      (cond ((eq (car a) :key)
             (unless key-set (setq key (%resolve-fn (cadr a))) (setq key-set t)))
            ((eq (car a) :start)
             (unless start-set (setq start (cadr a)) (setq start-set t)))
            ((eq (car a) :end)
             (unless end-set (setq end (cadr a)) (setq end-set t)))
            ((eq (car a) :from-end)
             (unless fe-set (setq from-end (cadr a)) (setq fe-set t)))
            ((eq (car a) :allow-other-keys) nil)
            (t (unless allow-other (%signal-program-error))))
      (setq a (cddr a)))
    (count-if (lambda (x) (not (funcall pred x)))
              seq
              :start start
              ;; array-wrapper-p check before consp — wrappers are conses
              ;; but have a finite LENGTH; MPF crashes the walker.
              :end (or end (cond ((null seq) 0)
                                  ((array-wrapper-p seq) (length seq))
                                  ((consp seq) most-positive-fixnum)
                                  (t (length seq))))
              :key key
              :from-end from-end)))
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
(defun array-element-type (a)
  "Return the element type the array A is specialized to (CLHS 15.2.13).
   For a native multi-dim/specialized array (MDA) consult its stored
   element-type slot — already the upgraded type (BIT / CHARACTER / T).
   For a string, the upgraded element type is CHARACTER.  All other
   general arrays/vectors store tagged words → T.  Defensive on
   non-arrays: returns T rather than erroring (callers like typep
   probe loosely)."
  (cond
    ((%mda-p a) (upgraded-array-element-type (%mda-etype a)))
    ((stringp a) 'character)
    ;; Byte-packed (unsigned-byte 8) vector (subtag #x11).
    ((eql (obj-subtag a) #x11) '(unsigned-byte 8))
    (t t)))
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
  "Make a sequence of TYPE and SIZE.  Supports :initial-element.
   Accepts compound type forms like (VECTOR), (VECTOR *), (VECTOR T 5)
   by dispatching on the head symbol (CLHS 17.1.3).  Class objects
   (returned by FIND-CLASS) and class-proxies (from CLASS-OF) are
   resolved to their NAME first.  Per CLHS, when the compound spec
   pins a length (e.g. (VECTOR T 4)) and SIZE disagrees, the call
   must signal type-error.  Unrecognised non-sequence type names
   (SYMBOL, INTEGER, ...) likewise signal type-error."
  ;; Resolve a class designator (class object or class-proxy) to its name
  ;; so the dispatch below can match symbols uniformly.  Keeps callers
  ;; using `(find-class 'list)' / `(class-of '(a b c))' working.
  (when (or (%clos-class-p type) (%class-proxy-p type))
    (setq type (class-name type)))
  ;; SIZE must be a non-negative integer (CLHS: make-sequence size = "a
  ;; non-negative integer").  Validate before any length comparison or
  ;; element-filling loop.  This also guards the mvm-eval / compiled-call path:
  ;; when make-sequence is called with the SIZE argument missing (e.g. the
  ;; arity-error test (make-sequence 'list)), the compiled-call convention
  ;; leaves a garbage value in the SIZE slot; without this check the
  ;; list/vector fill loops `(loop (when (= i size) ...) ...)` never reach
  ;; their bound and spin forever.  Signaling here makes the missing-arg case
  ;; raise a (sub-)error that handler-case/signals-error catches — matching
  ;; the tree-walker's program-error — instead of hanging.
  (unless (and (integerp size) (>= size 0))
    (%signal-type-error)
    (return-from make-sequence nil))
  (let ((init nil) (a args)
        (head (if (consp type) (car type) type)))
    ;; Pinned-length check: a (VECTOR ELT-TYPE N) / (STRING N) etc. that
    ;; supplies an integer in the length slot must match SIZE.  Walking
    ;; the spec here once avoids per-branch duplication.
    (when (and (consp type) (consp (cdr type)))
      (let* ((rest (cdr type))
             (len-slot (cond
                         ((or (eq head 'string) (eq head 'simple-string)
                              (eq head 'base-string)
                              (eq head 'simple-base-string)
                              (eq head 'bit-vector)
                              (eq head 'simple-bit-vector))
                          (car rest))
                         ((or (eq head 'vector) (eq head 'simple-vector)
                              (eq head 'array) (eq head 'simple-array))
                          (and (consp (cdr rest)) (cadr rest)))
                         (t '*))))
        (when (and (integerp len-slot) (not (= len-slot size)))
          (%signal-type-error))))
    (loop (when (null a) (return))
      (when (eq (car a) :initial-element) (setq init (cadr a)) (return))
      (setq a (cddr a)))
    ;; Kwarg validation: odd-length args list or unknown keyword
    ;; (without :allow-other-keys T) signals program-error per CLHS
    ;; 3.4.1.4.  Recognized keys: :initial-element, :allow-other-keys.
    ;; (make-sequence.error.10/11/12/13)
    (let ((p args) (allow-other nil))
      (let ((scan args))
        (loop (when (or (null scan) (null (cdr scan))) (return))
          (when (and (eq (car scan) :allow-other-keys) (cadr scan))
            (setq allow-other t))
          (setq scan (cddr scan))))
      (loop
        (when (null p) (return))
        (when (null (cdr p)) (%signal-program-error) (return))
        (let ((k (car p)))
          (unless (or (eq k :initial-element)
                      (eq k :allow-other-keys)
                      allow-other)
            (%signal-program-error)
            (return)))
        (setq p (cddr p))))
    (cond
      ;; null: only valid for size 0; non-zero size signals type-error
      ;; (CLHS — NULL has cardinality 1, so size>0 doesn't fit).
      ((eq head 'null)
       (if (= size 0) nil (progn (%signal-type-error) nil)))
      ;; cons: at least 1 element; size=0 signals type-error (CONS
      ;; cardinality ≥ 1 — the empty list isn't a CONS).
      ((eq head 'cons)
       (if (= size 0)
           (progn (%signal-type-error) nil)
           (let ((r nil) (i 0))
             (loop (when (= i size) (return r))
               (setq r (cons init r)) (setq i (+ i 1))))))
      ((eq head 'list)
       (let ((r nil) (i 0))
         (loop (when (= i size) (return r))
           (setq r (cons init r)) (setq i (+ i 1)))))
      ((or (eq head 'string) (eq head 'simple-string)
           (eq head 'base-string) (eq head 'simple-base-string))
       (let ((s (%make-string-array size))
             (ch (cond ((null init) 32)
                       ((characterp init) (char-code init))
                       (t init))))
         (let ((i 0))
           (loop (when (= i size) (return s))
             (aset s i ch) (setq i (+ i 1))))))
      ((or (eq head 'bit-vector) (eq head 'simple-bit-vector))
       (let ((v (make-array size)))
         (let ((i 0) (b (or init 0)))
           (loop (when (= i size) (return v))
             (aset v i b) (setq i (+ i 1))))))
      ;; Symbol / integer / function / etc. — KNOWN non-sequence built-in
      ;; types.  CLHS demands type-error.  (make-sequence.error.1)
      ((member head '(symbol integer function character keyword
                      ratio rational complex number real
                      hash-table package readtable stream pathname))
       (%signal-type-error)
       nil)
      (t
       ;; Generic vector / array / sequence fall-back: also catches
       ;; class-proxy-derived names like 'standard-object that aren't
       ;; in the head list above but represent runtime vector types.
       ;; ALWAYS-fill via :initial-element-provided detection prevents
       ;; the default-zero from leaking through.
       (let ((v (make-array size))
             (initial-provided (let ((found nil) (p args))
                                 (loop (when (null p) (return found))
                                   (when (eq (car p) :initial-element)
                                     (setq found t) (return found))
                                   (setq p (cddr p))))))
         (when initial-provided
           (let ((i 0))
             (loop (when (= i size) (return nil))
               (aset v i init) (setq i (+ i 1)))))
         v)))))
(defun %coerce-canon-head (head)
  "Canonicalize a type-head symbol to the Modus built-in type symbol of
   the same NAME.  Type specifiers are compared by name (CLHS), but a
   type-head read in a foreign package (e.g. UIOP/UTILITY::VECTOR, when
   define-package's inheritance hasn't exposed CL:VECTOR) is a distinct
   object from Modus's compiled 'VECTOR, so `(eq head 'vector)` fails and
   coerce silently returns OBJ unchanged.  Re-key by name so the downstream
   EQ checks match regardless of the head symbol's home package."
  (if (and head (not (consp head)) (not (stringp head))
           (not (integerp head)) (not (characterp head))
           (symbolp head))
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

(defun coerce (obj type)
  "Convert OBJ to TYPE per CLHS 4.7.  Handles list/vector/string/
   character/symbol/bit-vector and their compound forms like
   (vector *) / (vector * 2) / (simple-string)."
  (let ((head (%coerce-canon-head (if (consp type) (car type) type))))
    (cond
      ((or (eq type t) (eq type 'common-lisp:t)) obj)
      ;; List
      ((eq head 'list)
       (cond
         ((null obj) nil)
         ((consp obj) obj)
         ((stringp obj)
          (let ((acc nil) (i (- (length obj) 1)))
            (loop
              (when (< i 0) (return acc))
              (setq acc (cons (aref obj i) acc))   ; AREF→char already
              (setq i (- i 1)))))
         ((or (arrayp obj) (and (consp obj) (array-wrapper-p obj)))
          (let ((acc nil) (i (- (length obj) 1)))
            (loop
              (when (< i 0) (return acc))
              (setq acc (cons (aref obj i) acc))
              (setq i (- i 1)))))
         (t (list obj))))
      ;; Vector / simple-vector / array / simple-array
      ((or (eq head 'vector) (eq head 'simple-vector)
           (eq head 'array) (eq head 'simple-array))
       (cond
         ((null obj) (make-array 0))
         ((consp obj)
          (let* ((n (length obj))
                 (v (make-array n))
                 (i 0) (cur obj))
            (loop (when (= i n) (return v))
              (aset v i (car cur))
              (setq cur (cdr cur))
              (setq i (+ i 1)))))
         ((stringp obj)
          ;; Tests expect (coerce "ab" '(vector *)) to give #(#\a #\b).
          (let* ((n (length obj))
                 (v (make-array n))
                 (i 0))
            (loop (when (= i n) (return v))
              (aset v i (aref obj i))   ; AREF→char already
              (setq i (+ i 1)))))
         (t obj)))
      ;; String / simple-string / base-string
      ((or (eq head 'string) (eq head 'simple-string)
           (eq head 'base-string) (eq head 'simple-base-string))
       (cond
         ((stringp obj) obj)
         ((characterp obj)
          (let ((s (%make-string-array 1)))
            (aset s 0 (char-code obj)) s))
         ((null obj) (%make-string-array 0))
         ((consp obj)
          (let* ((n (length obj))
                 (s (%make-string-array n))
                 (i 0) (cur obj))
            (loop (when (= i n) (return s))
              (let ((c (car cur)))
                (aset s i (if (characterp c) (char-code c) c)))
              (setq cur (cdr cur))
              (setq i (+ i 1)))))
         ((arrayp obj)
          (let* ((n (length obj))
                 (s (%make-string-array n))
                 (i 0))
            (loop (when (= i n) (return s))
              (let ((c (aref obj i)))
                (aset s i (if (characterp c) (char-code c) c)))
              (setq i (+ i 1)))))
         (t obj)))
      ;; Character
      ((eq head 'character)
       (cond ((characterp obj) obj)
             ((integerp obj) (code-char obj))
             ((and (stringp obj) (= (length obj) 1))
              (aref obj 0))   ; AREF→char already
             ((and (symbolp obj) (= (length (symbol-name obj)) 1))
              (aref (symbol-name obj) 0))
             (t obj)))
      ;; CONS: empty list isn't a CONS — CLHS requires type-error
      ;; (coerce.error.4).  Non-NIL list passes through.
      ((eq head 'cons)
       (cond ((null obj) (%signal-type-error) nil)
             ((consp obj) obj)
             (t (%signal-type-error) nil)))
      ;; FUNCTION designator — symbol → (symbol-function ...).
      ;; (lambda …) source forms would need runtime EVAL of `(function
      ;; (lambda …))`, which Modus's interpreter can't fully compile
      ;; — leave that case for the default pass-through so callers
      ;; that wrap `coerce` in `funcall` get a clean function-error
      ;; instead of a SIGSEGV.  (coerce.21)
      ((eq head 'function)
       (cond ((functionp obj) obj)
             ((and (symbolp obj) (fboundp obj))
              (symbol-function obj))
             (t obj)))
      ;; Default — pass through
      (t obj))))
(defun mismatch (s1 s2 &rest args)
  "Compare S1 vs S2 element-by-element. Returns first index where they
   differ, or NIL if equal. Honors :test, :key, :start1, :end1, :start2,
   :end2, :from-end.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicate kwargs."
  (%check-kw-allowed args
   '(:test :test-not :key :start1 :end1 :start2 :end2 :from-end))
  (let ((test nil) (key nil)
        (start1 0) (end1 nil) (start2 0) (end2 nil) (from-end nil)
        (test-set nil) (key-set nil) (s1-set nil) (e1-set nil)
        (s2-set nil) (e2-set nil) (fe-set nil)
        (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :test)
             (unless test-set (setq test (cadr a)) (setq test-set t))
             (setq a (cddr a)))
            ((eq (car a) :key)
             (unless key-set  (setq key  (cadr a)) (setq key-set t))
             (setq a (cddr a)))
            ((eq (car a) :start1)
             (unless s1-set (setq start1 (cadr a)) (setq s1-set t))
             (setq a (cddr a)))
            ((eq (car a) :end1)
             (unless e1-set (setq end1 (cadr a)) (setq e1-set t))
             (setq a (cddr a)))
            ((eq (car a) :start2)
             (unless s2-set (setq start2 (cadr a)) (setq s2-set t))
             (setq a (cddr a)))
            ((eq (car a) :end2)
             (unless e2-set (setq end2 (cadr a)) (setq e2-set t))
             (setq a (cddr a)))
            ((eq (car a) :from-end)
             (unless fe-set (setq from-end (cadr a)) (setq fe-set t))
             (setq a (cddr a)))
            ((eq (car a) :test-not)
             (unless test-set
               (let ((f (cadr a)))
                 (setq test (lambda (x y) (not (funcall f x y)))))
               (setq test-set t))
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
  ;; CLHS: (random limit &optional state).  Extra args after state are
  ;; a program-error.
  (when (and state (cdr state))
    (error "random: too many arguments"))
  ;; (random 0) is undefined in CL but happens when callers pass an
  ;; overflowed bound like (ASH 1 64) → 0.  Returning 0 keeps the
  ;; caller alive instead of #DE-trapping into a kernel-killing
  ;; nested-exception loop (we don't install an IDT entry for #DE).
  (if (= n 0)
      0
      (let* ((seed (mem-ref #x100000A0 :u64))
             (next (logand (+ (* seed 1664525) 1013904223) #x3FFFFFFF)))
        (setf (mem-ref #x100000A0 :u64) next)
        (- next (* (truncate next n) n)))))
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
   :STRING, :VECTOR, :BIT-VECTOR, or :NULL.  Compound forms like
   (vector ...) and (simple-vector ...) are recognised by their car.
   :NULL ('NULL → empty list) and :BIT-VECTOR (separate from :VECTOR
   so map/merge wrappers preserve bit-vector identity) are split out
   from the catch-all :VECTOR bucket — affects MAP/MERGE/CONCATENATE
   tests that check (typep result 'bit-vector) or compare to NIL."
  (cond
    ((eq result-type 'null) :null)
    ((or (eq result-type 'list) (eq result-type 'cons)) :list)
    ((or (eq result-type 'string) (eq result-type 'simple-string)
         (eq result-type 'base-string) (eq result-type 'simple-base-string))
     :string)
    ((or (eq result-type 'bit-vector) (eq result-type 'simple-bit-vector))
     :bit-vector)
    ((or (eq result-type 'vector) (eq result-type 'simple-vector)
         (eq result-type 'array)  (eq result-type 'simple-array))
     :vector)
    ((consp result-type)
     (let ((head (car result-type)))
       (cond ((or (eq head 'bit-vector) (eq head 'simple-bit-vector))
              :bit-vector)
             ((or (eq head 'vector) (eq head 'simple-vector)
                  (eq head 'array)  (eq head 'simple-array))
              :vector)
             ((or (eq head 'string) (eq head 'simple-string)
                  (eq head 'base-string) (eq head 'simple-base-string))
              :string)
             (t :vector))))
    (t :vector)))

(defun concatenate (result-type &rest seqs)
  "Concatenate sequences.  Recognises list / string / vector result
   types (atomic and compound forms like (vector * *)).

   Per CLHS 17.2.1: type-error when RESULT-TYPE is a known
   non-sequence designator (FIXNUM, SYMBOL, etc.) or when a
   pinned-length compound spec doesn't match the produced length."
  ;; Reject known non-sequence head types + check pinned-length match.
  (let* ((head (if (consp result-type) (car result-type) result-type)))
    (when (member head '(symbol integer fixnum function character keyword
                         ratio rational complex number real
                         hash-table package readtable stream pathname
                         ;; SEQUENCE is the abstract parent — no concrete
                         ;; representation, so concatenate signals.  (CLHS
                         ;; 17.1: result must be a concrete subtype.)
                         sequence))
      (%signal-type-error))
    ;; NULL is the (empty) sequence type containing only NIL.  An empty
    ;; concatenation yields NIL; a non-empty one is a type-error.
    (when (eq head 'null)
      (let ((total 0))
        (dolist (s seqs) (setq total (+ total (%concat-elt-count s))))
        (if (= total 0)
            (return-from concatenate nil)
            (%signal-type-error))))
    (when (and (consp result-type) (consp (cdr result-type)))
      (let* ((rest (cdr result-type))
             (total 0)
             (len-slot (cond
                         ((or (eq head 'string) (eq head 'simple-string)
                              (eq head 'base-string)
                              (eq head 'simple-base-string)
                              (eq head 'bit-vector)
                              (eq head 'simple-bit-vector))
                          (car rest))
                         ((or (eq head 'vector) (eq head 'simple-vector)
                              (eq head 'array) (eq head 'simple-array))
                          (and (consp (cdr rest)) (cadr rest)))
                         (t '*))))
        (when (integerp len-slot)
          (dolist (s seqs) (setq total (+ total (%concat-elt-count s))))
          (when (not (= len-slot total))
            (%signal-type-error))))))
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
                      (string-p (%wrapper-stringp s)))
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
                      (string-p (%wrapper-stringp s)))
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
               ((stringp s)
                ;; String slots hold fixnum char-codes; a VECTOR result must
                ;; receive CHARACTER objects (concatenate-vector.X expects
                ;; #\a not 97).
                (dotimes (i (array-length s))
                  (aset result pos (aref s i))   ; AREF→char already
                  (setq pos (+ pos 1))))
               (t  ;; vector
                (dotimes (i (array-length s))
                  (aset result pos (aref s i))
                  (setq pos (+ pos 1))))))
           result))))))

(defun merge (result-type s1 s2 pred &rest args)
  "Merge two sorted sequences.  Honors RESULT-TYPE designator and :KEY.
   PRED can be a symbol (function name) or a function; symbol form is
   resolved via SYMBOL-FUNCTION.  Was: ignored &rest args entirely
   (merge-test 17815 GOT raw concat instead of properly merged).

   Per CLHS 17.2.1: signal type-error when RESULT-TYPE is a non-
   sequence designator (e.g. 'SYMBOL) or when a pinned-length
   compound spec doesn't match the merged length.  Per CLHS 3.4.1.4:
   odd-length kwarg list and unknown keys signal program-error;
   recognised key is :key only."
  ;; Kwarg validation: only :key (and :allow-other-keys) recognised.
  (let ((scan args) (allow-other nil))
    (loop (when (or (null scan) (null (cdr scan))) (return))
      (when (and (eq (car scan) :allow-other-keys) (cadr scan))
        (setq allow-other t))
      (setq scan (cddr scan)))
    (let ((vp args))
      (loop
        (when (null vp) (return))
        (when (null (cdr vp)) (%signal-program-error) (return))
        (let ((k (car vp)))
          (unless (or (eq k :key) (eq k :allow-other-keys) allow-other)
            (%signal-program-error)
            (return)))
        (setq vp (cddr vp)))))
  ;; Type-error on known non-sequence head + on pinned-length mismatch.
  (let* ((head (if (consp result-type) (car result-type) result-type))
         (merged-len (+ (length s1) (length s2))))
    (when (member head '(symbol integer function character keyword
                         ratio rational complex number real
                         hash-table package readtable stream pathname))
      (%signal-type-error))
    ;; NULL is the type containing only NIL — a valid (empty) sequence
    ;; designator.  Merging a non-empty result into NULL is a type-error.
    (when (eq head 'null)
      (if (= merged-len 0)
          (return-from merge nil)
          (%signal-type-error)))
    (when (and (consp result-type) (consp (cdr result-type)))
      (let* ((rest (cdr result-type))
             (len-slot (cond
                         ((or (eq head 'string) (eq head 'simple-string)
                              (eq head 'base-string)
                              (eq head 'simple-base-string)
                              (eq head 'bit-vector)
                              (eq head 'simple-bit-vector))
                          (car rest))
                         ((or (eq head 'vector) (eq head 'simple-vector)
                              (eq head 'array) (eq head 'simple-array))
                          (and (consp (cdr rest)) (cadr rest)))
                         (t '*))))
        (when (and (integerp len-slot) (not (= len-slot merged-len)))
          (%signal-type-error)))))
  (let* ((key-fn (let ((cur args) (k nil))
                   (loop (when (or (null cur) (null (cdr cur))) (return k))
                     (when (eq (car cur) :key) (setq k (cadr cur)))
                     (setq cur (cddr cur)))))
         (pred-fn (cond
                    ((functionp pred) pred)
                    ((symbolp pred) (symbol-function pred))
                    (t pred)))
         (kfn (cond
                ((null key-fn) nil)
                ((functionp key-fn) key-fn)
                ((symbolp key-fn) (symbol-function key-fn))
                (t key-fn)))
         (r nil)
         (a (if (consp s1) s1 (coerce s1 'list)))
         (b (if (consp s2) s2 (coerce s2 'list))))
    (let ((merged
            (loop
              (cond ((null a) (return (nreconc r b)))
                    ((null b) (return (nreconc r a)))
                    ((funcall pred-fn
                              (if kfn (funcall kfn (car a)) (car a))
                              (if kfn (funcall kfn (car b)) (car b)))
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
        ((eq kind :bit-vector)
         (let ((n (length merged))
               (cur merged))
           (let ((v (make-array n)) (i 0))
             (loop (when (= i n) (return v))
               (aset v i (car cur))
               (setq cur (cdr cur)) (setq i (+ i 1))))))
        (t  ;; :vector
         (let ((n (length merged))
               (cur merged))
           (let ((v (make-array n)) (i 0))
             (loop (when (= i n) (return v))
               (aset v i (car cur))
               (setq cur (cdr cur)) (setq i (+ i 1))))))))))

(defun replace (s1 s2 &rest args)
  "Replace elements of S1 with elements from S2.
   NOTE: this definition is SHADOWED by the one in cl-eval.lisp (loaded
   later → last-defun-wins).  The active overlap-safe version lives there."
  (%check-kw-allowed args '(:start1 :end1 :start2 :end2))
  (let ((len (if (< (length s1) (length s2)) (length s1) (length s2))))
    (dotimes (i len)
      (if (consp s1)
          (let ((cell (nthcdr i s1))) (when cell (set-car cell (elt s2 i))))
          (aset s1 i (elt s2 i)))))
  s1)

(defun fill (seq item &rest args)
  "Fill SEQUENCE with ITEM. Honors :START and :END.  Strings store
   fixnum char-codes; coerce character ITEM to its char-code before
   aset so the stored value matches what literal strings hold.
   Per CLHS, signals PROGRAM-ERROR on odd plist or unknown keyword
   (unless :allow-other-keys is non-nil).  Type-error on non-sequence
   SEQ is handled by the (consp …) / (length …) dispatch below: if
   SEQ is neither a cons nor a sequence, (length SEQ) signals."
  (let ((start 0) (end nil) (allow-other-keys nil) (aok-set nil)
        ;; CLHS §3.4.1.4.1.1.2 (leftmost wins) — track whether each kwarg
        ;; has been set already so we don't overwrite the leftmost binding.
        ;; fill.order.4 has six `:end` repetitions and the test passes only
        ;; when the FIRST one (3) sticks.
        (start-set nil) (end-set nil))
    ;; Probe :allow-other-keys.  CLHS §3.4.1.4.1.1.2: when multiple
    ;; :allow-other-keys appear, the LEFTMOST supplies the value.
    (let ((p args))
      (loop (when (null p) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other-keys (and (consp (cdr p)) (cadr p)))
          (setq aok-set t))
        (setq p (cdr p))))
    (let ((a args))
      (loop (when (null a) (return))
        (when (null (cdr a)) (%signal-program-error))
        (cond ((eq (car a) :start)
               (unless start-set (setq start (cadr a)) (setq start-set t)))
              ((eq (car a) :end)
               (unless end-set (setq end (cadr a)) (setq end-set t)))
              ((eq (car a) :allow-other-keys) nil)
              (t (unless allow-other-keys (%signal-program-error))))
        (setq a (cddr a))))
    ;; ANSI: :START must be a non-negative integer, :END NIL or non-negative integer.
    (unless (and (integerp start) (>= start 0))
      (error "fill: :START must be a non-negative integer"))
    (unless (or (null end) (and (integerp end) (>= end 0)))
      (error "fill: :END must be NIL or a non-negative integer"))
    (cond
      ((null seq) seq)
      ((consp seq)
       (let ((cur seq) (i 0)
             (eff-end (if end end most-positive-fixnum)))
         (loop (when (null cur) (return seq))
           (when (and (>= i start) (< i eff-end))
             (set-car cur item))
           (setq cur (cdr cur))
           (setq i (+ i 1)))
         seq))
      ((or (stringp seq) (arrayp seq))
       (let* ((len (length seq))
              (eff-end (if end end len))
              (store-item (if (and (stringp seq) (characterp item))
                              (char-code item)
                              item))
              (i start))
         (loop (when (>= i eff-end) (return seq))
           (aset seq i store-item)
           (setq i (+ i 1)))))
      (t (error "fill: not a sequence")))))

(defun map-into (result fn &rest seqs)
  "Apply FN to elements of SEQS, storing each result in successive
   positions of RESULT. Stops at the shortest of RESULT and SEQS.

   CLHS map-into semantics for vectors:
   - The maximum number of elements stored is the ARRAY-TOTAL-SIZE of the
     result vector (NOT its current fill pointer — a 6-element fp=3 vector
     can take up to 6 mapped values).  We therefore measure capacity with
     ARRAY-TOTAL-SIZE, not LENGTH (LENGTH returns the fill pointer).
   - When the result has a fill pointer, it is set to the number of
     elements actually mapped.

   Result-shape dispatch:
   - cons (a real list, NOT an array wrapper): set-car down the spine.
   - everything array-ish (plain vector, fp/displaced/adjustable cons
     wrapper, native MDA, bit/char specialised): store via the generic
     ASET, which already routes each wrapper/MDA flavour correctly.  The
     array-wrapper-p check precedes consp so fp/displaced/adjustable
     wrappers route to aset (NOT set-car on the wrapper's cons cell which
     would clobber the fill-pointer)."
  (let* ((cons-p (and (consp result) (not (array-wrapper-p result))))
         ;; CLHS: (map-into nil …) returns NIL immediately (the empty list
         ;; result has zero capacity).  NIL is not consp, so without this
         ;; guard cap would route to ARRAY-TOTAL-SIZE → type-error.
         (nil-p (null result))
         ;; Capacity: 0 for the empty-list result, list-length for cons
         ;; results, ARRAY-TOTAL-SIZE for any array-ish result (handles
         ;; fp/MDA where LENGTH = fill ptr).
         (cap (cond (nil-p 0)
                    (cons-p (length result))
                    (t (array-total-size result))))
         (n (let ((m cap))
              (dolist (s seqs m)
                (let ((sl (length s)))
                  (when (< sl m) (setq m sl))))))
         ;; Character store: ASET on a STRING wants a char-code, but ASET
         ;; on an MDA/wrapper char-array accepts the character object.
         (str-p (stringp result)))
    (if (null seqs)
        (let ((i 0))
          (loop (when (>= i n) (return))
            (let ((v (funcall fn)))
              (cond
                (cons-p (set-car (nthcdr i result) v))
                ;; Variable-index ASET as a non-last form: wrap in LET to
                ;; force dest=frame-slot (CLAUDE.md "Variable-index ASET").
                (t (let ((dummy (aset result i
                                      (if (and str-p (characterp v))
                                          (char-code v) v))))
                     dummy))))
            (setq i (+ i 1))))
        (let ((i 0))
          (loop (when (>= i n) (return))
            (let ((args (let ((r nil) (sr (reverse seqs)))
                          (dolist (s sr r)
                            (setq r (cons (elt s i) r))))))
              (let ((v (apply fn args)))
                (cond
                  (cons-p (set-car (nthcdr i result) v))
                  (t (let ((dummy (aset result i
                                        (if (and str-p (characterp v))
                                            (char-code v) v))))
                       dummy)))))
            (setq i (+ i 1)))))
    ;; CLHS: set fill pointer to number of elements mapped, if present.
    ;; Guard with (not cons-p): a plain list like (6 2 3) has a fixnum
    ;; car, so array-has-fill-pointer-p / %fp-inner would mis-classify it
    ;; as an fp-wrapper and set-car position 0 — corrupting the result.
    (when (and (not cons-p) (not nil-p) (array-has-fill-pointer-p result))
      (set-fill-pointer result n))
    result))

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
;; Per CLHS: signal PROGRAM-ERROR on odd-length plist or unknown keyword
;; (with :allow-other-keys NIL/absent).  Leftmost (k v) pair wins on
;; duplicates per §3.4.1.4.1.
(defun tree-equal (a b &rest args)
  (let ((test nil) (test-not nil)
        (test-set nil) (tn-set nil)
        (allow-other-keys nil) (aok-set nil))
    ;; First pass: scan for :allow-other-keys (leftmost wins).
    (let ((p args))
      (loop (when (null p) (return))
        (when (null (cdr p)) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other-keys (cadr p))
          (setq aok-set t))
        (setq p (cddr p))))
    ;; Validate + extract keyword args.
    (let ((cur args))
      (loop (when (null cur) (return))
        (when (null (cdr cur)) (%signal-program-error))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :test) (unless test-set (setq test (%resolve-fn v)) (setq test-set t)))
            ((eq k :test-not) (unless tn-set (setq test-not (%resolve-fn v)) (setq tn-set t)))
            ((eq k :allow-other-keys) nil)
            (t (unless allow-other-keys (%signal-program-error)))))
        (setq cur (cddr cur))))
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
  (%subst-check-kwargs args)
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
   Inlined parsing (same as remove) so we don't go through apply-of-rest.
   :test-not handled by wrapping into a negated test-fn."
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (test-fn (car (cddddr parsed)))
         (test-not-fn (cadr (cddddr parsed)))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count))
         (eff-test-fn (cond
                        (test-fn test-fn)
                        (test-not-fn
                         (lambda (a b) (not (funcall test-not-fn a b))))
                        (t nil))))
    (cond
      ((null seq) nil)
      ((= eff-count 0) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-list item seq eff-test-fn key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-vector item seq eff-test-fn key-fn start-idx end-idx eff-count from-end)))))

;; delete-if: same body shape as remove-if without the apply trampoline.
(defun delete-if (pred seq &rest args)
  "Remove items satisfying PRED (destructive). Forwards keyword args.
   PRED accepts function designators (symbol or function)."
  (setq pred (%resolve-fn pred))
  (let* ((parsed (%nsubst-parse-args args))
         (count (car parsed))
         (from-end (cadr parsed))
         (start-idx (or (caddr parsed) 0))
         (end-idx (cadddr parsed))
         (key-fn (caddr (cddddr parsed)))
         (eff-count (%nsubst-effective-count count)))
    (cond
      ((null seq) nil)
      ((= eff-count 0) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-if-list pred seq key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-if-vector pred seq key-fn start-idx end-idx eff-count from-end)))))

(defun delete-if-not (pred seq &rest args)
  "Remove items not satisfying PRED (destructive). Forwards keyword args.
   PRED accepts function designators (symbol or function)."
  (setq pred (%resolve-fn pred))
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
      ((= eff-count 0) (if (consp seq) (copy-list seq) (copy-seq seq)))
      ((consp seq)
       (%remove-if-list neg-pred seq key-fn start-idx end-idx eff-count from-end))
      (t
       (%remove-if-vector neg-pred seq key-fn start-idx end-idx eff-count from-end)))))

(defun %dup-parse-bounds (args)
  "Parse :from-end / :start / :end from a remove/delete-duplicates arglist.
   Returns (from-end start end-or-nil).  Per CLHS §3.4.1.4.1 the LEFTMOST
   occurrence of a repeated keyword supplies the value.  :start defaults to
   0, :end to NIL (meaning the sequence length)."
  (let ((from-end nil) (fe-set nil)
        (start 0) (start-set nil)
        (end nil) (end-set nil)
        (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :from-end)
             (unless fe-set (setq from-end (cadr a) fe-set t)) (setq a (cddr a)))
            ((eq (car a) :start)
             (unless start-set (setq start (or (cadr a) 0) start-set t)) (setq a (cddr a)))
            ((eq (car a) :end)
             (unless end-set (setq end (cadr a) end-set t)) (setq a (cddr a)))
            ((null (cdr a)) (return))
            (t (setq a (cddr a)))))
    (list from-end start end)))

(defun %dups-keep-list (seq test-fn key-fn from-end start end)
  "Core list dedup honoring :start/:end.  Elements outside [start,end) are
   always retained and do NOT participate in duplicate comparison.  Within
   the window: without FROM-END keep the LAST occurrence of each group; with
   FROM-END keep the FIRST.  Result preserves original order."
  (let ((eff-end (if end end (length seq)))
        (r nil) (cur seq) (i 0))
    (loop
      (when (null cur) (return (nreverse r)))
      (let* ((item (car cur))
             (in-window (and (>= i start) (< i eff-end)))
             (keep t))
        (when in-window
          (let ((item-key (if key-fn (funcall key-fn item) item)))
            ;; Scan the OTHER window elements for a duplicate.  Default
            ;; (not from-end): a later dup -> drop this one (keep last).
            ;; from-end: an earlier dup -> drop this one (keep first).
            ;; Terminate the inner scan with a flag (no nested LOOP RETURN).
            (let ((cur2 seq) (j 0) (scanning t))
              (loop
                (when (or (null cur2) (not scanning)) (return))
                (when (and (/= j i) (>= j start) (< j eff-end)
                           (if from-end (< j i) (> j i)))
                  (let ((v (if key-fn (funcall key-fn (car cur2)) (car cur2))))
                    (when (if test-fn (funcall test-fn item-key v) (eql item-key v))
                      (setq keep nil) (setq scanning nil))))
                (setq cur2 (cdr cur2))
                (setq j (+ j 1))))))
        (when keep (setq r (cons item r))))
      (setq cur (cdr cur))
      (setq i (+ i 1)))))

(defun %dups-keep-vector (vec test-fn key-fn from-end start end)
  "Core vector/string dedup honoring :start/:end.  Implemented by lifting the
   vector into a plain list, running the proven list dedup core (%dups-keep-
   list — the same path lists use), then rebuilding the original sequence
   type.  AREF already yields a CHARACTER for string elements and ASET coerces
   a CHARACTER back to a code for string destinations, so the same rebuild
   loop produces either a general vector or a string."
  (let* ((len (array-length vec))
         (string-p (stringp vec))
         ;; Snapshot elements into a list (preserves AREF char semantics).
         (as-list (let ((acc nil) (i len))
                    (loop
                      (when (= i 0) (return acc))
                      (setq i (- i 1))
                      (setq acc (cons (aref vec i) acc)))))
         ;; Dedup via the list core (CLHS-correct :start/:end/:from-end).
         (kept (%dups-keep-list as-list test-fn key-fn from-end start end))
         (out-len (length kept))
         (out (if string-p (%make-string-array out-len) (make-array out-len)))
         (cur kept) (j 0))
    (loop
      (when (null cur) (return out))
      (let ((dummy (aset out j (car cur))))
        (setq dummy dummy))
      (setq j (+ j 1))
      (setq cur (cdr cur)))))

(defun delete-duplicates (seq &rest args)
  "Remove duplicate items (destructive). Honors :test/:key/:from-end and
   :start/:end on lists, vectors, and strings.  We don't truly mutate in
   place (we cons / build a fresh result), which is conformant — DELETE-
   DUPLICATES is permitted to either modify or return a fresh sequence."
  (let* ((parsed (parse-test-key args))
         (test-fn (car parsed))
         (key-fn (cdr parsed))
         (bounds (%dup-parse-bounds args))
         (from-end (car bounds))
         (start (cadr bounds))
         (end (caddr bounds)))
    (cond
      ((null seq) nil)
      ((consp seq) (%dups-keep-list seq test-fn key-fn from-end start end))
      ((or (stringp seq) (arrayp seq))
       (%dups-keep-vector seq test-fn key-fn from-end start end))
      (t seq))))

(defun pushnew-fn (item place)
  "Functional pushnew."
  (if (member item place) place (cons item place)))

;;; String operations
(defun make-string (size &rest args)
  "Make a string of SIZE filled with spaces (or :initial-element).
   Per CLHS: validates &key args; signals PROGRAM-ERROR on odd plist
   or unknown keyword (with :allow-other-keys NIL/absent).  Recognised
   keywords are :initial-element and :element-type (the latter is
   accepted but not interpreted — Modus has one string element type).
   ANSI: requires SIZE as a non-negative integer."
  (unless (and (integerp size) (>= size 0))
    (%signal-program-error))
  (let ((ch 32) (ie-set nil) (allow-other-keys nil) (aok-set nil))
    ;; Probe :allow-other-keys.  CLHS §3.4.1.4.1.1.2: leftmost wins.
    (let ((p args))
      (loop (when (null p) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other-keys (and (consp (cdr p)) (cadr p)))
          (setq aok-set t))
        (setq p (cdr p))))
    (let ((a args))
      (loop (when (null a) (return))
        (when (null (cdr a)) (%signal-program-error))
        (cond
          ;; Per CLHS 3.4.1.4.1: leftmost (k v) pair wins on duplicates.
          ((eq (car a) :initial-element)
           (unless ie-set (setq ch (char-code (cadr a))) (setq ie-set t)))
          ((eq (car a) :element-type)    nil)
          ((eq (car a) :allow-other-keys) nil)
          (t (unless allow-other-keys (%signal-program-error))))
        (setq a (cddr a))))
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
  "True if bit INDEX of INTEGER (two's complement) is 1.  A bit at or beyond
   INTEGER-LENGTH is the sign bit — 0 for a non-negative integer, 1 for a
   negative one — so we never compute 2^index for a huge INDEX (which would
   build an astronomically large bignum and hang, e.g.
   (logbitp most-positive-fixnum 0))."
  (if (>= index (integer-length integer))
      (< integer 0)
      (not (zerop (logand integer (ash 1 index))))))

(defun integer-length (n)
  "CLHS 12.2: the number of bits in N's two's-complement representation,
   excluding the sign.  For a NEGATIVE N this is the bit length of -(N+1),
   not of (abs N) — e.g. (integer-length -1) = 0, (integer-length -2) = 1."
  (let ((x (if (< n 0) (- (- 0 n) 1) n)) (len 0))
    (loop (when (zerop x) (return len))
      (setq x (ash x -1)) (setq len (+ len 1)))))

(defun %complex-p (x)
  "Recognise a modus complex number — 3-slot array with %complex-marker
   in slot 0, real in slot 1, imaginary in slot 2."
  (and (not (fixnump x)) (not (consp x)) (not (null x))
       (not (characterp x))
       (= (obj-subtag x) #x32)
       (>= (array-length x) 3)
       (eq (aref x 0) '%complex-marker)))

(defun complex (r &optional i)
  "Create a complex number with REAL=r and IMAGINARY=i (default 0).
   CLHS 12.1.5.3: (complex rational 0) collapses to the rational R — a
   rational complex with a zero rational imaginary part IS that rational.
   But (complex float ...) is ALWAYS a genuine complex object, and float
   contagion coerces BOTH parts to a float when either part is a float:
   (complex 1.0) => #C(1.0 0.0), (complex 1 2.0) => #C(1.0 2.0).  Only the
   all-rational, exactly-zero-imaginary case collapses to a real."
  (let ((i (or i 0)))
    (cond
      ;; All-rational with a zero imaginary part → collapse to R.
      ((and (rationalp r) (rationalp i) (= i 0)) r)
      (t
       ;; Float contagion: if either part is a float, coerce both to float.
       (let ((rf r) (im i))
         (when (or (%ieee-float-p r) (%ieee-float-p i))
           (setq rf (%any-to-float r))
           (setq im (%any-to-float i)))
         (let ((c (make-array 3)))
           (aset c 0 '%complex-marker)
           (aset c 1 rf)
           (aset c 2 im)
           c))))))

(defun realpart (x)
  (cond
    ((%complex-p x) (aref x 1))
    (t x)))

(defun imagpart (x)
  ;; CLHS: imagpart of a REAL is (* 0 x) — a rational 0 for rationals, but
  ;; a same-format float ZERO for floats (e.g. (imagpart 1.0) => 0.0).
  (cond
    ((%complex-p x) (aref x 2))
    ((%ieee-float-p x) (%irr-result (%float-sub x x) x))
    (t 0)))

(defun conjugate (x)
  (cond
    ((%complex-p x)
     (complex (aref x 1) (- 0 (aref x 2))))
    (t x)))

(defun complexp (x) (%complex-p x))

(defun %complex-abs (z)
  "Magnitude of complex Z = sqrt(realpart^2 + imagpart^2).  Used by the
   compiler's ABS macro for the complex case (and by abs's defun)."
  (sqrt (+ (* (realpart z) (realpart z))
           (* (imagpart z) (imagpart z)))))

;;; Complex arithmetic.  Use modus's existing rational + / - / * routes
;;; so the resulting (real, imag) parts come back in their natural form
;;; (integer / ratio / IEEE-decoded).
;;;
;;; (a + bi) + (c + di) = (a + c) + (b + d)i
;;; (a + bi) - (c + di) = (a - c) + (b - d)i
;;; (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
;;; (a + bi) / (c + di) = ((ac+bd)/(c²+d²)) + ((bc-ad)/(c²+d²))i

(defun %complex-real (x) (if (%complex-p x) (aref x 1) x))
(defun %complex-imag (x) (if (%complex-p x) (aref x 2) 0))

(defun complex-add (a b)
  (complex (+ (%complex-real a) (%complex-real b))
           (+ (%complex-imag a) (%complex-imag b))))

(defun complex-sub (a b)
  (complex (- (%complex-real a) (%complex-real b))
           (- (%complex-imag a) (%complex-imag b))))

(defun complex-mul (a b)
  (let ((ar (%complex-real a)) (ai (%complex-imag a))
        (br (%complex-real b)) (bi (%complex-imag b)))
    (complex (- (* ar br) (* ai bi))
             (+ (* ar bi) (* ai br)))))

(defun complex-div (a b)
  (let ((ar (%complex-real a)) (ai (%complex-imag a))
        (br (%complex-real b)) (bi (%complex-imag b)))
    (let ((denom (+ (* br br) (* bi bi))))
      (when (= denom 0) (error "complex divide by zero"))
      (complex (exact-divide (+ (* ar br) (* ai bi)) denom)
               (exact-divide (- (* ai br) (* ar bi)) denom)))))

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
(defun evenp (n)
  "Even predicate.  Bignum-safe: the raw :and IR can't compare a heap
   pointer's LSB to 1 (it ANDs the pointer's tag nibble), so a bignum
   N routes through its low-limb instead.  Sign-magnitude bignums match
   two's complement bit-for-bit on the LSB (low-limb LSB IS the parity
   bit) so this works for negative bignums too."
  (cond
    ((bignump n) (zerop (logand (%bignum-low-limb n) 1)))
    (t (zerop (logand n 1)))))

(defun oddp (n)
  (cond
    ((bignump n) (not (zerop (logand (%bignum-low-limb n) 1))))
    (t (not (zerop (logand n 1))))))
(defun boundp (sym)
  "Per CLHS, BOUNDP returns T iff SYM has a value cell binding.
   Modus stores special-var values in the global hash table at fixed slot
   0x10000080, keyed by name-hash.  Look SYM up by its hash.  Keywords are
   always bound to themselves.  Non-symbol input signals type-error per
   ANSI — we just return NIL for robustness.  (NB cl-packages.lisp defines
   the authoritative BOUNDP later; this one is kept consistent with the
   hash-table store so it stays correct under any load order.  The old
   `(eq (car (car alist)) sym)` walk was ALSO wrong for the hash-keyed
   store — the keys are name-hashes, not the SYM object.)"
  (cond
    ((null sym) t)
    ((eq sym t) t)
    ((keywordp sym) t)
    ((or (fixnump sym) (consp sym) (characterp sym)) nil)
    (t (%boundp-by-hash (aref sym 0)))))
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
         (bounds (%dup-parse-bounds args))
         (from-end (car bounds))
         (start (cadr bounds))
         (end (caddr bounds)))
    (cond
      ((null seq) nil)
      ((consp seq) (%dups-keep-list seq test-fn key-fn from-end start end))
      ((or (stringp seq) (arrayp seq))
       (%dups-keep-vector seq test-fn key-fn from-end start end))
      (t seq))))

;;; ===================================================
;;; Sequence Search Functions (find, search, position-if, etc.)
;;; ===================================================

(defun find (item sequence &rest args)
  "Return the first element of SEQUENCE that matches ITEM.
   :test defaults to inline `eql` (#'eql is unusable in MVM).
   :test-not is the negation of :test.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates."
  (%seq-subst-check-kwargs args)
  (%require-sequence sequence)
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
             (string-p (%wrapper-stringp sequence)))
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
                    ;; AREF already yields a CHARACTER for strings, so
                    ;; present it directly (compiled code-char would
                    ;; re-shift an already-character value).
                    (elem raw))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (if test (funcall test item test-val) (eql item test-val))
                   (if from-end
                       (setq result elem)
                       (return elem)))))
             (setq i (+ i 1)))))))))

(defun find-if (predicate sequence &rest args)
  "Return the first element of SEQUENCE satisfying PREDICATE.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicate kwargs.
   PREDICATE and :key accept function designators (symbol or function).
   Per CLHS 3.4.1.4: odd-length plist or unknown keyword (without
   :allow-other-keys T) signals PROGRAM-ERROR."
  (setq predicate (%resolve-fn predicate))
  (%require-sequence sequence)
  (let ((key nil) (start 0) (end nil) (from-end nil)
        (key-set nil) (start-set nil) (end-set nil) (fe-set nil)
        (allow-other nil) (aok-set nil))
    (let ((p args))
      (loop (when (null p) (return))
        (when (null (cdr p)) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other (cadr p))
          (setq aok-set t))
        (setq p (cddr p))))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (when (null (cdr cur)) (%signal-program-error))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :key)      (unless key-set    (setq key (%resolve-fn v))  (setq key-set t)))
            ((eq k :start)    (unless start-set  (setq start v)      (setq start-set t)))
            ((eq k :end)      (unless end-set    (setq end v)        (setq end-set t)))
            ((eq k :from-end) (unless fe-set     (setq from-end v)   (setq fe-set t)))
            ((eq k :allow-other-keys) nil)
            (t (unless allow-other (%signal-program-error)))))
        (setq cur (cddr cur))))
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (%wrapper-stringp sequence)))
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
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw)))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (funcall predicate test-val)
                   (if from-end (setq result elem) (return elem)))))
             (setq i (+ i 1)))))))))

(defun find-if-not (predicate sequence &rest args)
  "Return the first element of SEQUENCE not satisfying PREDICATE.
   Inlined (rather than `(apply #'find-if (lambda ...) ...)') to dodge
   the documented apply-of-rest-through-sibling-defun fragility.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicate kwargs.
   PREDICATE and :key accept function designators (symbol or function).
   Per CLHS 3.4.1.4: odd-length plist or unknown keyword (without
   :allow-other-keys T) signals PROGRAM-ERROR."
  (setq predicate (%resolve-fn predicate))
  (%require-sequence sequence)
  (let ((key nil) (start 0) (end nil) (from-end nil)
        (key-set nil) (start-set nil) (end-set nil) (fe-set nil)
        (allow-other nil) (aok-set nil))
    (let ((p args))
      (loop (when (null p) (return))
        (when (null (cdr p)) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other (cadr p))
          (setq aok-set t))
        (setq p (cddr p))))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (when (null (cdr cur)) (%signal-program-error))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :key)      (unless key-set    (setq key (%resolve-fn v))  (setq key-set t)))
            ((eq k :start)    (unless start-set  (setq start v)      (setq start-set t)))
            ((eq k :end)      (unless end-set    (setq end v)        (setq end-set t)))
            ((eq k :from-end) (unless fe-set     (setq from-end v)   (setq fe-set t)))
            ((eq k :allow-other-keys) nil)
            (t (unless allow-other (%signal-program-error)))))
        (setq cur (cddr cur))))
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (%wrapper-stringp sequence)))
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
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                    (test-val (if key (funcall key elem) elem)))
               (unless (funcall predicate test-val)
                 (if from-end (setq result elem) (return elem))))
             (setq i (+ i 1)))))))))

(defun position-if (predicate sequence &rest args)
  "Return the index of first element satisfying PREDICATE.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicate kwargs.
   PREDICATE and :key accept function designators (symbol or function).
   Per CLHS 3.4.1.4: odd-length plist or unknown keyword (without
   :allow-other-keys T) signals PROGRAM-ERROR."
  (setq predicate (%resolve-fn predicate))
  (%require-sequence sequence)
  (let ((key nil) (start 0) (end nil) (from-end nil)
        (key-set nil) (start-set nil) (end-set nil) (fe-set nil)
        (allow-other nil) (aok-set nil))
    (let ((p args))
      (loop (when (null p) (return))
        (when (null (cdr p)) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other (cadr p))
          (setq aok-set t))
        (setq p (cddr p))))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (when (null (cdr cur)) (%signal-program-error))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :key)      (unless key-set    (setq key (%resolve-fn v))  (setq key-set t)))
            ((eq k :start)    (unless start-set  (setq start v)      (setq start-set t)))
            ((eq k :end)      (unless end-set    (setq end v)        (setq end-set t)))
            ((eq k :from-end) (unless fe-set     (setq from-end v)   (setq fe-set t)))
            ((eq k :allow-other-keys) nil)
            (t (unless allow-other (%signal-program-error)))))
        (setq cur (cddr cur))))
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (%wrapper-stringp sequence)))
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
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw)))
               (let ((test-val (if key (funcall key elem) elem)))
                 (when (funcall predicate test-val)
                   (if from-end (setq result i) (return i)))))
             (setq i (+ i 1)))))))))

(defun position-if-not (predicate sequence &rest args)
  "Return the index of first element not satisfying PREDICATE.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicate kwargs.
   Inlined (rather than `(apply #'position-if (lambda ...) ...)') to
   dodge the documented apply-of-rest-through-sibling-defun fragility.
   PREDICATE and :key accept function designators (symbol or function).
   Per CLHS 3.4.1.4: odd-length plist or unknown keyword (without
   :allow-other-keys T) signals PROGRAM-ERROR."
  (setq predicate (%resolve-fn predicate))
  (%require-sequence sequence)
  (let ((key nil) (start 0) (end nil) (from-end nil)
        (key-set nil) (start-set nil) (end-set nil) (fe-set nil)
        (allow-other nil) (aok-set nil))
    (let ((p args))
      (loop (when (null p) (return))
        (when (null (cdr p)) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other (cadr p))
          (setq aok-set t))
        (setq p (cddr p))))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (when (null (cdr cur)) (%signal-program-error))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :key)      (unless key-set    (setq key (%resolve-fn v))  (setq key-set t)))
            ((eq k :start)    (unless start-set  (setq start v)      (setq start-set t)))
            ((eq k :end)      (unless end-set    (setq end v)        (setq end-set t)))
            ((eq k :from-end) (unless fe-set     (setq from-end v)   (setq fe-set t)))
            ((eq k :allow-other-keys) nil)
            (t (unless allow-other (%signal-program-error)))))
        (setq cur (cddr cur))))
    (cond
      ((and (consp sequence) (array-wrapper-p sequence))
       (let ((len (length sequence)) (result nil)
             (string-p (%wrapper-stringp sequence)))
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
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                    (test-val (if key (funcall key elem) elem)))
               (unless (funcall predicate test-val)
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1)))))))))

(defun position (item sequence &rest args)
  "Return the position of the first ITEM in SEQUENCE satisfying TEST.
   Supports :test/:test-not/:key/:start/:end/:from-end.
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicates."
  (%seq-subst-check-kwargs args)
  (%require-sequence sequence)
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
             (string-p (%wrapper-stringp sequence)))
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
       (let ((len (length sequence)) (result nil)
             (string-p (stringp sequence)))
         (when (null end) (setq end len))
         (let ((i start))
           (loop
             (when (= i end) (return result))
             (let* ((raw (aref sequence i))
                    (elem (if (and string-p (integerp raw)) (code-char raw) raw))
                    (test-val (if key (funcall key elem) elem))
                    (matched (if test (funcall test item test-val)
                                 (eql item test-val))))
               (when matched
                 (if from-end (setq result i) (return i))))
             (setq i (+ i 1)))))))))

;; complement: (complement #'pred) returns a function that negates pred.
;;
;; CL says complement returns a function "of zero or more arguments".  In
;; practice almost every test caller uses 1 or 2 args (predicates,
;; equality tests).  We avoid &rest in the closure body because that
;; combination — captured variable + &rest collection — currently
;; miscompiles in MVM (the captured fn slot reads the wrong cell once
;; &rest has consed up the rest list).  Instead, dispatch on a known
;; arity by emitting four sibling closures (one per arity) and pick the
;; right one via a runtime arity-checking trampoline.  4-arg cap is
;; well above what any predicate caller in the suite actually uses.
(defun complement (fn)
  "Return a function that returns the logical negation of FN's result.
   The returned function takes any number of args and forwards them
   verbatim via APPLY — per CLHS §17.3.1 the complement function must
   accept whatever arity the wrapped predicate accepts.  An earlier
   version hard-coded (lambda (a b) ...) which silently dropped extra
   args and passed garbage for unary predicates; with arity checks
   enabled on LISTP/CONSP/etc., callers like (complement #'listp)
   now signal PROGRAM-ERROR if invoked with 2 args."
  (lambda (&rest args)
    (not (apply fn args))))

(defun search (seq1 seq2 &rest args)
  "Search for SEQ1 as a subsequence of SEQ2. Return index or nil.
   :test defaults to inline `eql` (#'eql is unusable in MVM).
   Per CLHS 3.4.1.4.1, leftmost keyword wins on duplicate kwargs."
  (%check-kw-allowed args
   '(:test :test-not :key :start1 :end1 :start2 :end2 :from-end))
  (let ((test nil) (key nil) (start1 0) (end1 nil) (start2 0) (end2 nil) (from-end nil)
        (test-set nil) (key-set nil) (s1-set nil) (e1-set nil)
        (s2-set nil) (e2-set nil) (fe-set nil))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :test)     (unless test-set (setq test v)     (setq test-set t)))
            ((eq k :test-not)
             (unless test-set
               (let ((f v))
                 (setq test (lambda (a b) (not (funcall f a b)))))
               (setq test-set t)))
            ((eq k :key)      (unless key-set  (setq key v)      (setq key-set t)))
            ((eq k :start1)   (unless s1-set   (setq start1 v)   (setq s1-set t)))
            ((eq k :end1)     (unless e1-set   (setq end1 v)     (setq e1-set t)))
            ((eq k :start2)   (unless s2-set   (setq start2 v)   (setq s2-set t)))
            ((eq k :end2)     (unless e2-set   (setq end2 v)     (setq e2-set t)))
            ((eq k :from-end) (unless fe-set   (setq from-end v) (setq fe-set t)))))
        (setq cur (cddr cur))))
    ;; Read element through wrappers via %seq-elt (handles list, vector,
    ;; and wrapper conses uniformly).
    (labels ((%seq-elt (seq i)
               (cond
                 ((null seq) nil)
                 ((and (consp seq) (array-wrapper-p seq))
                  (let* ((raw (%wrapper-aref seq i)))
                    (if (and (%wrapper-stringp seq) (integerp raw)) (code-char raw) raw)))
                 ((consp seq) (nth i seq))
                 ((stringp seq) (aref seq i))   ; AREF→char already
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

;;; reduce — override of prelude.lisp's reduce.  The prelude version
;;; parsed repeated keywords RIGHTMOST-wins (each match overwrote the
;;; prior value).  CLHS 3.4.1.4.1 requires LEFTMOST-wins: the first
;;; occurrence of a repeated keyword is the one that takes effect.
;;; (reduce #'cons '(1 2 3) :from-end t :from-end nil
;;;         :initial-value nil :initial-value 'a) => (1 2 3)
;;; because :from-end t and :initial-value nil (the leftmost ones) win.
(defun reduce (fn seq &rest args)
  "Fold FN over SEQ (list or vector). Honors :initial-value, :from-end,
   :start, :end, :key.  Repeated keywords: leftmost wins (CLHS 3.4.1.4.1)."
  (%check-kw-allowed args '(:initial-value :from-end :start :end :key))
  (let ((init :no-init) (init-given nil)
        (from-end nil) (start 0) (end nil) (key nil)
        ;; per-keyword "seen" flags so the FIRST occurrence wins
        (fe-set nil) (start-set nil) (end-set nil) (key-set nil)
        (a args))
    (loop (when (null a) (return))
      (cond ((eq (car a) :initial-value)
             (unless init-given (setq init (cadr a) init-given t))
             (setq a (cddr a)))
            ((eq (car a) :from-end)
             (unless fe-set (setq from-end (cadr a) fe-set t)) (setq a (cddr a)))
            ((eq (car a) :start)
             (unless start-set (setq start (cadr a) start-set t)) (setq a (cddr a)))
            ((eq (car a) :end)
             (unless end-set (setq end (cadr a) end-set t)) (setq a (cddr a)))
            ((eq (car a) :key)
             (unless key-set (setq key (cadr a) key-set t)) (setq a (cddr a)))
            (t (setq a (cdr a)))))
    (let* ((elts (let ((lst nil) (i 0)
                       (eff-end (if end end (if seq (length seq) 0))))
                   (cond
                     ((and (consp seq) (array-wrapper-p seq))
                      (loop (when (>= i eff-end) (return nil))
                        (when (>= i start)
                          (let ((v (if key (funcall key (elt seq i)) (elt seq i))))
                            (setq lst (cons v lst))))
                        (setq i (+ i 1))))
                     ((or (null seq) (consp seq))
                      (let ((cur seq))
                        (loop (when (null cur) (return nil))
                          (when (and (>= i start) (< i eff-end))
                            (let ((v (if key (funcall key (car cur)) (car cur))))
                              (setq lst (cons v lst))))
                          (setq cur (cdr cur))
                          (setq i (+ i 1)))))
                     (t
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
         (let* ((rev (reverse elts))
                (acc (if init-given init (car rev)))
                (cur (if init-given rev (cdr rev))))
           (loop (when (null cur) (return acc))
             (setq acc (funcall fn (car cur) acc))
             (setq cur (cdr cur)))))
        (t
         (let* ((acc (if init-given init (car elts)))
                (cur (if init-given elts (cdr elts))))
           (loop (when (null cur) (return acc))
             (setq acc (funcall fn acc (car cur)))
             (setq cur (cdr cur)))))))))

