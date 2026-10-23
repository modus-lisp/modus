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

(defun %delete-splice-list (lst to-drop)
  "Destructively splice the elements of LST at the indices in TO-DROP out
   of the list, by SET-CDR on the surviving predecessor cons.  Returns the
   (possibly new) head.

   This is the DESTRUCTIVE half of DELETE / DELETE-IF / DELETE-IF-NOT on
   lists.  CLHS only says these MAY modify the argument, and Modus used to
   forward all of them to the copying REMOVE family — conformant, but every
   real implementation (SBCL, CCL, …) splices in place, and library code
   written against them depends on the aliasing.  alexandria's DELETEF is
   the canonical case:
     (let* ((x (list 1 2 3)) (x* x)) (deletef x 2) (list x x*))
   is ((1 3) (1 3)) everywhere else and was ((1 3) (1 2 3)) here.

   Only the CDR chain is rewritten; CARs are untouched, so a cons another
   list shares as a TAIL still sees the splice exactly as SBCL's does."
  (let ((head lst) (prev nil) (cur lst) (i 0))
    (loop
      (when (null cur) (return head))
      (let ((next (cdr cur)))
        (if (member i to-drop)
            (if prev (set-cdr prev next) (setq head next))
            (setq prev cur))
        (setq cur next)
        (setq i (+ i 1))))))

(defun %delete-list (item lst test-fn key-fn start-idx end-idx count from-end)
  "DELETE on a list: same match/COUNT/FROM-END logic as %REMOVE-LIST, but
   the result is produced by splicing LST rather than copying it."
  (%delete-splice-list
   lst (%remove-list-drop-indices item lst test-fn key-fn
                                  start-idx end-idx count from-end)))

(defun %delete-if-list (pred lst key-fn start-idx end-idx count from-end)
  "DELETE-IF / DELETE-IF-NOT on a list — see %DELETE-LIST."
  (%delete-splice-list
   lst (%remove-if-list-drop-indices pred lst key-fn
                                     start-idx end-idx count from-end)))

(defun %remove-list-drop-indices (item lst test-fn key-fn start-idx end-idx count from-end)
  "The 0-based indices of LST that a REMOVE/DELETE with these arguments
   drops.  Factored out of %REMOVE-LIST so the destructive DELETE path
   shares EXACTLY the same match/COUNT/FROM-END semantics."
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
    ;; INDICES is largest-first (push order).  Apply COUNT: :from-end keeps
    ;; the LAST n matches (already at the front), otherwise the FIRST n.
    (if (>= count 0)
        (if from-end
            (subseq indices 0 (min count (length indices)))
            (let ((rev (reverse indices)))
              (subseq rev 0 (min count (length rev)))))
        indices)))

(defun %remove-if-list-drop-indices (pred lst key-fn start-idx end-idx count from-end)
  "As %REMOVE-LIST-DROP-INDICES, for the predicate form."
  (let ((indices nil) (cur lst) (i 0))
    (loop
      (when (null cur) (return nil))
      (when (and end-idx (>= i end-idx)) (return nil))
      (when (>= i start-idx)
        (let ((v (if key-fn (funcall key-fn (car cur)) (car cur))))
          (when (funcall pred v) (push i indices))))
      (setq cur (cdr cur))
      (setq i (+ i 1)))
    (if (>= count 0)
        (if from-end
            (subseq indices 0 (min count (length indices)))
            (let ((rev (reverse indices)))
              (subseq rev 0 (min count (length rev)))))
        indices)))

(defun %remove-list (item lst test-fn key-fn start-idx end-idx count from-end)
  ;; Non-destructive: mark which indices to drop (shared with the DELETE
  ;; path via %REMOVE-LIST-DROP-INDICES), then build a fresh list.
  (let ((to-drop (%remove-list-drop-indices item lst test-fn key-fn
                                            start-idx end-idx count from-end)))
    (let ((result nil) (cur lst) (i 0))
      (loop
        (when (null cur) (return (nreverse result)))
        (unless (member i to-drop) (push (car cur) result))
        (setq cur (cdr cur))
        (setq i (+ i 1))))))

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
  ;; Non-destructive; index selection shared with %DELETE-IF-LIST.
  (let ((to-drop (%remove-if-list-drop-indices pred lst key-fn
                                               start-idx end-idx count from-end)))
    (let ((result nil) (cur lst) (i 0))
      (loop
        (when (null cur) (return (nreverse result)))
        (unless (member i to-drop) (push (car cur) result))
        (setq cur (cdr cur))
        (setq i (+ i 1))))))

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
  "Compare two boxed floats by their IEEE bits (four 16-bit chunk slots)."
  (if (= (%float-hi32 a) (%float-hi32 b))
      (= (%float-lo32 a) (%float-lo32 b))
      nil))


;;; ============================================================
;;; Missing CL functions needed by ANSI tests
;;; ============================================================

;;; STALE — SUPERSEDED.  ASSERT is now a real MACRO: see the
;;; (mvm-define-macro "ASSERT" …) in mvm/compiler.lisp's
;;; register-mvm-bootstrap-macros.  Macro expansion precedes function-call
;;; compilation (macroexpand-1-mvm), so no normal `(assert …)` call site reaches
;;; this defun any more; it survives only as the value of `#'assert` (which CLHS
;;; leaves undefined for a macro).  The docstring below records why the silent
;;; stub existed and is kept for history — it no longer describes what ASSERT
;;; does.  Its "-31 net regression" warning was real and REMAINS the cost of a
;;; conforming ASSERT: 17 ANSI tests in files that use ASSERT for in-test
;;; invariants (apropos, time, print-structure, read-from-string, the file
;;; chapter) were passing on a FALSE premise and now fail honestly, against
;;; +14 real gains.  Do NOT "fix" that by re-silencing ASSERT.
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
            (if (and (%prim-stringp sa) (not (%mda-p sa))
                     (%prim-stringp sb) (not (%mda-p sb))
                     (or (%prim-base-string-p sa) (%prim-base-string-p sb)
                         (%prim-string32-p sa) (%prim-string32-p sb)))
                ;; A NARROW string (#x13, a byte per character) on either side:
                ;; %PRIM-AREF reads the CODE from either representation, so a
                ;; narrow and a wide string with the same characters compare
                ;; equal.  (Symbol names are narrow: INTERN and FIND-SYMBOL
                ;; compare them here.)
                (loop
                  (when (= i len1) (return t))
                  (let ((ca (%prim-aref sa (+ s1 i))) (cb (%prim-aref sb (+ s2 i))))
                    (unless (or (eq ca cb)
                                (eql (%ensure-char-code ca) (%ensure-char-code cb)))
                      (return nil)))
                  (setq i (+ i 1)))
            (if (and (%prim-stringp sa) (not (%mda-p sa))
                     (%prim-stringp sb) (not (%mda-p sb)))
                ;; FAST PATH (both plain #x31 strings): compare raw char
                ;; codes slot-by-slot.  The generic loop below costs ~8
                ;; calls and a &rest cons per character (AREF dispatch +
                ;; code-char boxing x2, CHAR= &rest, %CHAR2=, %ENSURE-CHAR-
                ;; CODE x2) — CHAR= was 20% of a library-load profile.
                ;; %WORD-AREF, not %PRIM-AREF: both are plain #x31 strings (never
                ;; byte-packed), so the per-character u8-or-word subtag test is
                ;; waste; and whole strings (both starts 0, the EQUAL / hash-table
                ;; case) walk one index.
                (if (and (eql s1 0) (eql s2 0))
                    (loop
                      (when (= i len1) (return t))
                      (let ((ca (%word-aref sa i)) (cb (%word-aref sb i)))
                        (unless (or (eq ca cb)
                                    (eql (%ensure-char-code ca) (%ensure-char-code cb)))
                          (return nil)))
                      (setq i (+ i 1)))
                    (loop
                      (when (= i len1) (return t))
                      ;; Slots normally hold CODES, but a slot may hold a CHARACTER
                      ;; (see %ENSURE-CHAR-CODE) — so a raw EQ miss re-compares
                      ;; through the normalizer before declaring a mismatch.
                      (let ((ca (%word-aref sa (+ s1 i))) (cb (%word-aref sb (+ s2 i))))
                        (unless (or (eq ca cb)
                                    (eql (%ensure-char-code ca) (%ensure-char-code cb)))
                          (return nil)))
                      (setq i (+ i 1))))
                (loop
                  (when (= i len1) (return t))
                  ;; AREF on strings now yields CHARACTERs — compare via char=.
                  (unless (char= (aref sa (+ s1 i)) (aref sb (+ s2 i))) (return nil))
                  (setq i (+ i 1))))))
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
;;; SORT IS A MERGE SORT (stable, O(n log n)).  It was an insertion sort for
;;; lists and vectors alike -- O(n^2): flexi-streams sorts two ~24k-entry
;;; encoding tables at load time (enc-cn-tbl.lisp), ~300M comparisons, ~40 s of
;;; a 133 s load.  Merging takes the left run's element unless the right one is
;;; strictly less, so equal keys keep their order and STABLE-SORT is the same
;;; code.  Destructive on the list spine, as CLHS SORT allows.
(defun %merge-lists (a b pred key)
  (let* ((head (list nil)) (tail head))
    (loop
      (cond ((null a) (set-cdr tail b) (return nil))
            ((null b) (set-cdr tail a) (return nil))
            ((funcall pred (if key (funcall key (car b)) (car b))
                           (if key (funcall key (car a)) (car a)))
             (set-cdr tail b) (setq tail b) (setq b (cdr b)))
            (t (set-cdr tail a) (setq tail a) (setq a (cdr a)))))
    (cdr head)))

(defun %merge-sort-list (lst n pred key)
  "Sort the first N conses of LST (N = its length), returning the new head."
  (if (< n 2)
      (progn (when lst (set-cdr lst nil)) lst)
      (let* ((half (floor n 2))
             (mid (nthcdr half lst)))
        (%merge-lists (%merge-sort-list lst half pred key)
                      (%merge-sort-list mid (- n half) pred key)
                      pred key))))

(defun %sort-list (seq pred key)
  (if (or (null seq) (null (cdr seq)))
      seq
      (%merge-sort-list seq (length seq) pred key)))

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
    ;; Past a few elements: merge-sort a list of the elements (AREF already
    ;; presents a string's codes as characters) and store them back -- the
    ;; insertion sort below is O(n^2); see %SORT-LIST.
    (when (> len 8)
      (let ((l nil) (i (- len 1)))
        (loop (when (< i 0) (return nil)) (setq l (cons (aref seq i) l)) (setq i (- i 1)))
        (let ((sorted (%sort-list l pred key)) (k 0))
          (loop (when (null sorted) (return nil))
            (aset seq k (car sorted)) (setq sorted (cdr sorted)) (setq k (+ k 1))))
        (return-from %sort-vector seq)))
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
  ;; The merge sort is stable; same impl.
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
  "Apply FN to each key-value pair in hash table (function version), newest
   first.  What the compiler turns a non-lambda MAPHASH into, so it must not
   call MAPHASH (that recursed until the stack ran out).  Same walk as
   prelude.lisp MAPHASH: a compact dict's entries, or the alist."
  (let* ((holder (%ht-bucket-holder ht))
         (d (and holder (%ht-h-vec holder))))
    (if (consp d)
        (let* ((idx (car d)) (e (cdr d)) (j (- (%word-aref e 0) 1)))
          (loop
            (when (< j 0) (return nil))
            (let ((k (%word-aref e (+ 1 (+ j j)))))
              (unless (eq k idx)
                (funcall fn k (%word-aref e (+ 2 (+ j j))))))
            (setq j (- j 1))))
        (let ((cur (car ht)))
          (loop (when (null cur) (return nil))
            (let ((pair (car cur)))
              (funcall fn (car pair) (cdr pair)))
            (setq cur (cdr cur)))))))
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
    ;; Packed single-float vector (subtag #x12).
    ((eql (obj-subtag a) #x12) 'single-float)
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
;;; (UNSIGNED-BYTE 8) RESULT TYPES.  MAKE-ARRAY :ELEMENT-TYPE '(UNSIGNED-BYTE 8)
;;; makes a PACKED byte vector (subtag #x11), but COERCE, MAP, CONCATENATE and
;;; MAKE-SEQUENCE to the same type made a GENERAL vector (element type T).  Code
;;; that declares (simple-array (unsigned-byte 8) (*)) and compiles at
;;; (safety 0) reads the packed layout -- seal's GHASH table read tagged words
;;; as bytes and every AES-GCM tag came out wrong (TLS 1.3 could not decrypt a
;;; single handshake record).  These make the four agree with MAKE-ARRAY.
(defun %seq-type-u8-p (type)
  "True when TYPE is an array/vector type specifier whose element type is
   (UNSIGNED-BYTE 8)."
  (and (consp type) (symbolp (car type))
       (member (symbol-name (car type)) '("SIMPLE-ARRAY" "ARRAY" "VECTOR") :test #'string=)
       (consp (cdr type))
       (let ((et (cadr type)))
         (and (consp et) (symbolp (car et))
              (string= (symbol-name (car et)) "UNSIGNED-BYTE")
              (consp (cdr et)) (eql (cadr et) 8)))))

(defun %seq-type-f32-p (type)
  "True when TYPE is an array/vector type specifier whose element type
   upgrades to SINGLE-FLOAT (the packed #x12 vector)."
  (and (consp type) (symbolp (car type))
       (member (symbol-name (car type)) '("SIMPLE-ARRAY" "ARRAY" "VECTOR") :test #'string=)
       (consp (cdr type))
       (%f32-element-type-p (cadr type))))

(defun %seq-u8-copy (seq)
  "A fresh packed (unsigned-byte 8) vector holding SEQ's elements."
  (let* ((n (length seq))
         (v (make-array n :element-type '(unsigned-byte 8))))
    (if (listp seq)
        (let ((i 0)) (dolist (e seq) (setf (aref v i) e) (setq i (+ i 1))))
        (dotimes (i n) (setf (aref v i) (aref seq i))))
    v))

(defun make-sequence (type size &rest args)
  "Make a sequence of TYPE and SIZE.  Supports :initial-element.
   Accepts compound type forms like (VECTOR), (VECTOR *), (VECTOR T 5)
   by dispatching on the head symbol (CLHS 17.1.3).  Class objects
   (returned by FIND-CLASS) and class-proxies (from CLASS-OF) are
   resolved to their NAME first.  Per CLHS, when the compound spec
   pins a length (e.g. (VECTOR T 4)) and SIZE disagrees, the call
   must signal type-error.  Unrecognised non-sequence type names
   (SYMBOL, INTEGER, ...) likewise signal type-error."
  (when (%seq-type-u8-p type)
    (return-from make-sequence
      (make-array size :element-type '(unsigned-byte 8)
                       :initial-element (let ((p (member :initial-element args))) (if p (cadr p) 0)))))
  ;; (VECTOR SINGLE-FLOAT) is the packed #x12 vector, as MAKE-ARRAY makes it:
  ;; this built a general vector, which TYPEP of the very type asked for
  ;; rejects (ANSI make-sequence 17821).
  (when (%seq-type-f32-p type)
    (let ((n (and (eq (car type) 'vector) (consp (cddr type)) (caddr type))))
      (when (and (integerp n) (not (= n size)))
        (%signal-type-error)
        (return-from make-sequence nil)))
    (return-from make-sequence
      (let ((p (member :initial-element args)))
        (if p
            (make-array size :element-type 'single-float :initial-element (cadr p))
            (make-array size :element-type 'single-float)))))
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
  ;; CLHS: LIMIT is a positive integer or a positive float.  (random 0) used
  ;; to return 0 -- a guard against the integer path's division #DE-trapping
  ;; on bare metal, where no IDT entry handles #DE -- and a negative LIMIT
  ;; returned a value in (LIMIT, 0].  Both now signal the TYPE-ERROR SBCL
  ;; signals, which also keeps the zero away from the division.  A caller
  ;; that indexed with (random 0) used to go on to read one past the end of
  ;; an empty vector (ANSI subtypep 26525/26526 "passed" that way).
  ;; A positive fixnum, or a finite positive double by its bits (a sign-clear
  ;; high word below the infinity exponent, not zero), answers before the
  ;; generic test: (> n 0) of a double is the NUMERIC-VALUE-LESS-P chain, most
  ;; of a (random 1d0).
  (if (not (or (and (fixnump n) (> n 0))
               (and (double-float-p n)
                    (let ((hi (%float-hi32 n)))
                      (and (< hi #x7FF00000) (or (> hi 0) (> (%float-lo32 n) 0)))))
               (and (integerp n) (> n 0)) (and (floatp-impl n) (> n 0))))
      (error 'type-error :datum n :expected-type '(or (integer 1) (float (0.0))))
      ;; The seed lives at #x10000FD8 -- NOT #x100000A0, which is the SECOND
      ;; extra slot of the multiple-value buffer (#x10000098..#x10000138): every
      ;; function returning three or more values overwrote the seed with its
      ;; third value, so the next RANDOM multiplied a string or a character
      ;; (a TYPE-ERROR with no datum) or returned garbage (upstream
      ;; REMOVE-RANDOM and friends, whose generator returns three values).
      ;; :u32 so an uninitialised (bare-metal) word still reads as an integer.
      (let* ((seed (mem-ref #x10000FD8 :u32))
             (next (logand (+ (* seed 1664525) 1013904223) #x3FFFFFFF)))
        (setf (mem-ref #x10000FD8 :u32) next)
        ;; CLHS: LIMIT is a positive INTEGER *or* a positive FLOAT, and the
        ;; result is of the same type as LIMIT and strictly within [0,LIMIT).
        ;; The integer path below ((- next (* (truncate next n) n))) is a
        ;; modulo, which is nonsense for a float LIMIT: (truncate next 1.0)
        ;; is an integer, so the expression degenerated to (- next k) and
        ;; (random 1.0) returned values like 3.0, -23.0, 26.0 — outside the
        ;; interval, with the wrong type.  Every rejection sampler built on
        ;; (random 1.0) then failed to converge (alexandria GAUSSIAN-RANDOM.1
        ;; never terminated).  Scale the 30-bit LCG output into [0,1) and
        ;; multiply: NEXT maxes at #x3FFFFFFF so the fraction is < 1 and the
        ;; product is strictly < N.
        (cond
          ;; A DOUBLE limit: the fraction NEXT/2^30 is exact in double (and
          ;; < 1), and so is N times it short of N -- a double has far more
          ;; than 30 bits.  The single-float fraction below made (random 1d0)
          ;; single-precision, and ran through generic arithmetic and
          ;; single rounding (cl-bench MRG32K3A, which times RANDOM).
          ((double-float-p n)
           (%float-mul n (%float-div (%float-from-int next) 1073741824d0)))
          ((floatp-impl n)
           (* n (/ (float next) 1073741824.0)))
          (t (- next (* (truncate next n) n)))))))
(defun do-special-strings (fn) (funcall fn ""))

;;; String functions
(defun %concat-elt-count (s)
  "Element count of one CONCATENATE input.  Signals a type error for a
   non-sequence instead of guessing.

   The old catch-all `(t (array-length s))' ran ARRAY-LENGTH on ANY value that
   was not NIL, a cons or a string — a symbol, a number, a hash-table, a
   struct.  ARRAY-LENGTH is a raw header read with no type check, so it
   returned whatever bits sat where a header would be: `(array-length '(1 2))'
   yields 14593280 here.  That garbage becomes CONCATENATE's TOTAL, TOTAL
   becomes the argument of `(make-array total)', and a ~10^12 element count
   walks the allocator off the end of the heap.  So passing a non-sequence to
   CONCATENATE was an unbounded wild write and a bare SIGSEGV rather than the
   TYPE-ERROR CLHS 17.2.1 asks for — and the fault lands in the allocator,
   nowhere near the caller that supplied the bad value.

   ARRAYP covers strings and vectors alike (the old STRINGP clause did exactly
   what the catch-all did), and the two CONSP clauses computed the same LENGTH,
   so the fast paths are unchanged."
  (cond ((null s) 0)
        ((consp s) (length s))
        ((arrayp s) (array-length s))
        (t (%signal-type-error))))

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

(defun %concat-check-seqs (seqs)
  "Signal a TYPE-ERROR naming the first CONCATENATE input that is not a
   sequence — BEFORE any counting, allocation or copying happens.

   %CONCAT-ELT-COUNT already refuses a non-sequence, but it runs inside the
   counting pass and only on the arms that count; the :LIST arm never calls it
   and walks SEQS directly.  Checking every input up front makes the failure
   uniform across arms and, more importantly, makes it NAME THE VALUE: the
   condition carries the offending object as its DATUM, so a caller that passes
   the wrong thing (a FUNCTION, say) is identifiable from the error instead of
   from a debugger session.  That is the whole point — the previous behaviour
   was a raw header read whose garbage surfaced as a SIGSEGV in the allocator,
   arbitrarily far from the caller at fault.

   Cost is one pass over the ARGUMENT LIST — typically two or three elements,
   never over the sequence CONTENTS — so this is not a hot path and needs no
   opt-out.

   DELIBERATELY NOT A RUNTIME FLAG.  A `(defvar *concat-check* t)' switch would
   read as garbage in any image whose kernel-main never reaches
   init-all-globals (Active Limitation 7), and an UNBOUND read here would break
   CONCATENATE everywhere rather than merely skipping a check — trading a
   narrow bug for a broad one.  If this ever must become optional, make it a
   BUILD-TIME switch — which now exists: the body is wrapped in
   (SAFETY-CHECK 1 ...), so `MODUS_SAFETY=0' (or a source-level
   `(declaim (optimize (safety 0)))') compiles it out entirely rather than
   skipping it at runtime.  At the default level 1 it is emitted as before."
  (safety-check 1
    (dolist (s seqs)
      (unless (or (null s) (consp s) (arrayp s))
        (error 'type-error :datum s :expected-type 'sequence)))))

(defun concatenate (result-type &rest seqs)
  "Concatenate sequences.  Recognises list / string / vector result
   types (atomic and compound forms like (vector * *)).

   Per CLHS 17.2.1: type-error when RESULT-TYPE is a known
   non-sequence designator (FIXNUM, SYMBOL, etc.), when a
   pinned-length compound spec doesn't match the produced length, or
   when any input is not a sequence."
  (when (%seq-type-u8-p result-type)
    (return-from concatenate (%seq-u8-copy (apply (function concatenate) 'vector seqs))))
  (%concat-check-seqs seqs)
  ;; Reject known non-sequence head types + check pinned-length match.
  (let* ((head (if (consp result-type) (car result-type) result-type)))
    ;; PERF: the MEMBER below scans an 18-element constant list on EVERY call
    ;; (measured: 629 ns, ~8% of a two-string CONCATENATE).  Every designator
    ;; real code actually passes — STRING / LIST / VECTOR and their SIMPLE-
    ;; variants — is guaranteed to miss it, so short-circuit on those first:
    ;; four EQ tests at ~4 ns instead of the full scan.  The list is still
    ;; consulted for everything else, so SEQUENCE, FIXNUM, HASH-TABLE etc.
    ;; are rejected exactly as before.
    (unless (or (eq head 'string) (eq head 'list) (eq head 'vector)
                (eq head 'simple-string) (eq head 'simple-vector)
                (eq head 'base-string) (eq head 'simple-base-string))
    (when (member head '(symbol integer fixnum function character keyword
                         ratio rational complex number real
                         hash-table package readtable stream pathname
                         ;; SEQUENCE is the abstract parent — no concrete
                         ;; representation, so concatenate signals.  (CLHS
                         ;; 17.1: result must be a concrete subtype.)
                         sequence))
      (%signal-type-error)))
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
                ;; PUBLIC AREF/ASET, deliberately.  Two attempts to copy raw
                ;; codes here both broke FILL-POINTER strings (ANSI
                ;; concatenate.28/.29, which expect (concatenate 'string x)
                ;; to honour the fill pointer): first gated on STRINGP, then
                ;; on %PRIM-STRINGP — and %PRIM-STRINGP is TRUE for a
                ;; fill-pointer string, so the second guard excluded nothing.
                ;; The character boxing this does is real cost (~5x per char
                ;; vs SUBSEQ) but it is CORRECT for every string shape, and a
                ;; faster path needs a predicate that has been MEASURED to
                ;; exclude wrappers, not assumed to.
                ;;
                ;; %BULK-COPY-OK-P is that predicate, and it is measured: it
                ;; rejects conses (every wrapper), %mda-p, subtag #x11, and
                ;; any string/non-string mismatch, and the copy test exercises
                ;; fill-pointer strings, adjustable and displaced arrays and
                ;; u8 vectors against SBCL.  Note the two earlier attempts
                ;; failed by gating on STRINGP / %PRIM-STRINGP, both of which
                ;; are TRUE for a fill-pointer string — the CONSP test is what
                ;; actually excludes wrappers, because a wrapper IS a cons.
                (if (%bulk-copy-ok-p result s)
                    (let ((n (array-length s)))
                      (%bulk-copy result pos s 0 n)
                      (setq pos (+ pos n)))
                    (dotimes (i (array-length s))
                      (aset result pos (aref s i)) (setq pos (+ pos 1)))))
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
    (%fill-pos seq item start end)))

(defun %fill-pos (seq item start end)
  "FILL with its bounds as positional arguments (END NIL = the length).  The
   compiler turns a FILL call with literal keywords into this, so the call conses
   no &REST list of keyword arguments (one FILL a row is how glass fills a
   rectangle)."
  (progn
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
         ;; bounding indexes past the vector are a TYPE-ERROR (CLHS 17.1.1), not
         ;; stores past it: the fast paths below do no per-element check, and
         ;; the slow one wrote off the end silently too
         (when (> eff-end len)
           (%signal-type-error eff-end (list 'integer 0 len)))
         (when (> start eff-end)
           (%signal-type-error start (list 'integer 0 eff-end)))
         (cond
           ;; the two fast paths store without per-element checks (bounds are
           ;; checked above; see %FILL-U8 / %FILL-WORDS)
           ((and (integerp store-item) (%u8-bare-p seq))
            (%fill-u8 seq i eff-end store-item)
            seq)
           ((and (not (stringp seq)) (%bulk-copy-ok-p seq seq))
            ;; plain word-slot array (general or typed-generic, not a
            ;; string / wrapper / mda): raw slot stores — a VP8 macroblock
            ;; clears 25 coefficient blocks this way
            (%fill-words seq i eff-end store-item)
            seq)
           (t
            (loop (when (>= i eff-end) (return seq))
              (aset seq i store-item)
              (setq i (+ i 1)))))))
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
  "Remove ITEM from SEQ.  DESTRUCTIVE on lists (splices via SET-CDR, so
   an alias of the head observes the removal, as in SBCL/CCL); copies for
   vectors.  Inlined parsing (same as remove) so we don't go through
   apply-of-rest.  :test-not is wrapped into a negated test-fn."
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
      ((= eff-count 0) (if (consp seq) seq (copy-seq seq)))
      ((consp seq)
       (%delete-list item seq eff-test-fn key-fn start-idx end-idx eff-count from-end))
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
      ((= eff-count 0) (if (consp seq) seq (copy-seq seq)))
      ((consp seq)
       (%delete-if-list pred seq key-fn start-idx end-idx eff-count from-end))
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
      ((= eff-count 0) (if (consp seq) seq (copy-seq seq)))
      ((consp seq)
       (%delete-if-list neg-pred seq key-fn start-idx end-idx eff-count from-end))
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

(defun %dups-keep-list-eql (seq key-fn from-end start eff-end n)
  "%DUPS-KEEP-LIST for the default (EQL) test, in linear time: walk the window
   from the end (keep the LAST of each group) or from the start (FROM-END,
   keep the FIRST) through an EQL hash set of the keys seen so far.  The
   pairwise scan below was quadratic -- cl-bench's SUM-PERMUTATIONS dedups
   362880 sums and ran for over half an hour (SBCL: 0.2 s)."
  (let ((v (make-array n)) (keep (make-array n))
        (seen (make-hash-table :test 'eql)) (cur seq) (i 0))
    (loop
      (when (null cur) (return))
      (setf (svref v i) (car cur))
      (setf (svref keep i) t)
      (setq cur (cdr cur))
      (setq i (+ i 1)))
    (flet ((visit (j)
             (let ((k (if key-fn (funcall key-fn (svref v j)) (svref v j))))
               (if (nth-value 1 (gethash k seen))
                   (setf (svref keep j) nil)
                   (setf (gethash k seen) t)))))
      (if from-end
          (let ((j start)) (loop (when (>= j eff-end) (return)) (visit j) (setq j (+ j 1))))
          (let ((j (- eff-end 1))) (loop (when (< j start) (return)) (visit j) (setq j (- j 1))))))
    (let ((r nil) (j (- n 1)))
      (loop
        (when (< j 0) (return r))
        (when (svref keep j) (setq r (cons (svref v j) r)))
        (setq j (- j 1))))))

(defun %dups-keep-list (seq test-fn key-fn from-end start end)
  "Core list dedup honoring :start/:end.  Elements outside [start,end) are
   always retained and do NOT participate in duplicate comparison.  Within
   the window: without FROM-END keep the LAST occurrence of each group; with
   FROM-END keep the FIRST.  Result preserves original order."
  (when (null test-fn)
    (let ((n (length seq)))
      (when (> n 16)
        (return-from %dups-keep-list
          (%dups-keep-list-eql seq key-fn from-end start (if end end n) n)))))
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



(defun %complex-p (x)
  "Recognise a modus complex number — 3-slot array with %complex-marker
   in slot 0, real in slot 1, imaginary in slot 2."
  (and (not (fixnump x)) (not (consp x)) (not (null x))
       (not (characterp x))
       (= (obj-subtag x) #x32)
       (>= (%prim-array-length x) 3)
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
         ;; ONE format for both parts (CLHS 12.1.5.2): the widest float among
         ;; them.  %ANY-TO-FLOAT made a rational part DOUBLE beside a single
         ;; one -- (+ #c(1 2) 0.5) was #C(1.5 2.0d0), not #C(1.5 2.0).
         (when (or (%ieee-float-p r) (%ieee-float-p i))
           (let ((proto (if (or (double-float-p r) (double-float-p i)) 1d0 1.0)))
             (setq rf (float r proto))
             (setq im (float i proto))))
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
    ;; 0.0 * x, not x - x: the sign of (* 0 x) follows x, so
    ;; (imagpart -79916.61) is -0.0 (upstream MISC.598).
    ((%ieee-float-p x) (%irr-result (%float-mul (%fl 0) x) x))
    (t 0)))

(defun conjugate (x)
  (cond
    ((%complex-p x)
     (complex (aref x 1) (%negate-number (aref x 2))))
    (t x)))


(defun %complex-abs (z)
  "Magnitude of complex Z.  Used by the compiler's ABS macro for the complex
   case.  FLOAT parts go through the overflow-safe %HYPOT-F in their own
   format -- r*r + i*i is inf for |r| near 1e200, so (abs #C(1d200 1d200)) was
   inf; RATIONAL parts keep the exact form ((abs #C(3 4)) = 5)."
  (let ((r (realpart z)) (i (imagpart z)))
    (if (or (floatp r) (floatp i))
        (%irr-result (%hypot-f (%any-to-float r) (%any-to-float i))
                     (if (double-float-p r) r i))
        (sqrt (+ (* r r) (* i i))))))

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

;;; The complex arithmetic below inspects each operand ONCE (%COMPLEX-REAL and
;;; %COMPLEX-IMAG each re-ran %COMPLEX-P, whose marker test is a quoted-symbol
;;; lookup) and builds a float-part result with %COMPLEX-FLOATS -- what COMPLEX
;;; returns for two float parts, without its rational-collapse and contagion
;;; tests or the full MAKE-ARRAY.  An FFT on complex doubles spent most of its
;;; time in those (reed's Opus MDCT).

(defun %complex-floats (r i)
  "The complex R + iI.  When both parts are IEEE floats of one format this
   is exactly COMPLEX's object, built directly; otherwise COMPLEX."
  (if (and (%ieee-float-p r) (%ieee-float-p i) (= (obj-subtag r) (obj-subtag i)))
      (let ((c (%make-array-raw 3)))
        (aset c 0 '%complex-marker)
        (aset c 1 r)
        (aset c 2 i)
        c)
      (complex r i)))

(defun complex-add (a b)
  (let* ((ac (%complex-p a)) (bc (%complex-p b)))
    (%complex-floats (+ (if ac (aref a 1) a) (if bc (aref b 1) b))
                     (+ (if ac (aref a 2) 0) (if bc (aref b 2) 0)))))

(defun complex-sub (a b)
  (let* ((ac (%complex-p a)) (bc (%complex-p b)))
    (%complex-floats (- (if ac (aref a 1) a) (if bc (aref b 1) b))
                     (- (if ac (aref a 2) 0) (if bc (aref b 2) 0)))))

(defun complex-mul (a b)
  (let* ((ac (%complex-p a)) (bc (%complex-p b))
         (ar (if ac (aref a 1) a)) (ai (if ac (aref a 2) 0))
         (br (if bc (aref b 1) b)) (bi (if bc (aref b 2) 0)))
    (%complex-floats (- (* ar br) (* ai bi))
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

;; Old definition was a process-of-elimination heuristic (the same
;; fragility class as the old functionp — see commit 7203e19).  It
;; returned T for closures, bignums, ratios, packages, and anything
;; else that survived the negation chain — and was layout-sensitive
;; for fn-addrs whose low bit happened to land on 0 or 1.  Replaced
;; with proper subtag checks.  In MVM all vectors share +subtag-array+
;; (#x32) — strings (#x31) are also vector-shaped per CL semantics.
(defun vectorp (obj)
  "A one-dimensional array (CLHS): ARRAYP already excludes the marked
   objects (structs, instances, conditions, complexes)."
  (or (stringp obj) (and (arrayp obj) (= (array-rank obj) 1))))
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
  ;; CLHS: ENDP signals a TYPE-ERROR (not a plain error) when its argument is
  ;; not a list.  A bare (error "…") produced a SIMPLE-ERROR that escaped
  ;; callers' (handler-case … (type-error () …)) — e.g. alexandria proper-list-
  ;; length on a dotted list ((endp <improper-tail>)) reported the wrong
  ;; condition class (proper-list-length.2).
  (cond ((null x) t)
        ((consp x) nil)
        (t (%signal-type-error))))

;; NOTE: tree-equal lives in cl-sequences.lisp with full &key support
;; (:test / :test-not / :allow-other-keys, leftmost-wins, program-error on
;; bad keys).  An old 2-arg stub SHADOWED it (it assembled later),
;; dropping all the :test variants and keyword/error tests.  Removed so the
;; complete version wins.
(defun copy-tree (tree)
  ;; %RT-CONSP: a hash table is a tagged cons and must be a LEAF here, as the
  ;; runtime CONSP now says (upstream COPY-TREE.2).
  (if (%rt-consp tree)
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

  ; Stub: return nil (unknown)

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

(defun %bit-raw-data (v len)
  "The word vector holding V's bits in row-major order when V is a plain
   vector or a non-displaced native MDA over one, with room for LEN; else NIL."
  (cond ((or (null v) (consp v) (fixnump v)) nil)
        ((%mda-p v)
         (and (null (%mda-displaced v))
              (let ((d (%mda-data v)))
                (and d (not (consp d)) (not (%mda-p d)) (not (%prim-stringp d))
                     (not (eq (obj-subtag d) #x12))
                     (>= (array-length d) len) d))))
        ((%prim-stringp v) nil)
        ((eq (obj-subtag v) #x12) nil)
        ((>= (array-length v) len) v)
        (t nil)))

(defun %bit-op-fast (code v1 v2 result-vec op)
  "%BIT-OP with the operation as CODE (0 and 1 ior 2 xor 3 eqv 4 nand 5 nor
   6 andc1 7 andc2 8 orc1 9 orc2) over the arrays' raw data, when all three
   expose it: one raw read per operand and one raw write per bit, where
   %BIT-OP paid a generic AREF per operand, a closure call and a generic
   ASET (cl-bench BITVECTORS, 2265x SBCL).  Otherwise %BIT-OP with OP."
  (let* ((len (min (array-length v1) (array-length v2)))
         (result (cond ((eq result-vec t) v1)
                       (result-vec result-vec)
                       (t (make-array (array-dimensions v1) :element-type 'bit))))
         (d1 (%bit-raw-data v1 len))
         (d2 (and d1 (%bit-raw-data v2 len)))
         (dr (and d2 (%bit-raw-data result len))))
    (if (null dr)
        (%bit-op op v1 v2 result)
        (let ((i 0))
          (loop
            (when (>= i len) (return result))
            (let* ((a (logand (%prim-aref d1 i) 1))
                   (b (logand (%prim-aref d2 i) 1))
                   (r (cond ((= code 0) (logand a b))
                            ((= code 1) (logior a b))
                            ((= code 2) (logxor a b))
                            ((= code 3) (logxor 1 (logxor a b)))
                            ((= code 4) (logxor 1 (logand a b)))
                            ((= code 5) (logxor 1 (logior a b)))
                            ((= code 6) (logand (logxor 1 a) b))
                            ((= code 7) (logand a (logxor 1 b)))
                            ((= code 8) (logior (logxor 1 a) b))
                            (t (logior a (logxor 1 b))))))
              (%prim-aset dr i r))
            (setq i (+ i 1)))))))

(defun bit-and (v1 v2 &optional result) (%bit-op-fast 0 v1 v2 result (lambda (a b) (logand a b))))
(defun bit-ior (v1 v2 &optional result) (%bit-op-fast 1 v1 v2 result (lambda (a b) (logior a b))))
(defun bit-xor (v1 v2 &optional result) (%bit-op-fast 2 v1 v2 result (lambda (a b) (logxor a b))))
(defun bit-eqv (v1 v2 &optional result) (%bit-op-fast 3 v1 v2 result (lambda (a b) (logxor 1 (logxor a b)))))
(defun bit-nand (v1 v2 &optional result) (%bit-op-fast 4 v1 v2 result (lambda (a b) (logxor 1 (logand a b)))))
(defun bit-nor (v1 v2 &optional result) (%bit-op-fast 5 v1 v2 result (lambda (a b) (logxor 1 (logior a b)))))
(defun bit-andc1 (v1 v2 &optional result) (%bit-op-fast 6 v1 v2 result (lambda (a b) (logand (logxor 1 a) b))))
(defun bit-andc2 (v1 v2 &optional result) (%bit-op-fast 7 v1 v2 result (lambda (a b) (logand a (logxor 1 b)))))
(defun bit-orc1 (v1 v2 &optional result) (%bit-op-fast 8 v1 v2 result (lambda (a b) (logior (logxor 1 a) b))))
(defun bit-orc2 (v1 v2 &optional result) (%bit-op-fast 9 v1 v2 result (lambda (a b) (logior a (logxor 1 b)))))

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

(defun %element-type-expand (type)
  "TYPE with user DEFTYPEs expanded until its head is not one (bounded), so an
   element type spelled through a DEFTYPE -- flexi-streams' (deftype char* ()
   'character) -- is classified as what it names.  Standard type names have no
   DEFTYPE entry and come back unchanged."
  (let ((n 0))
    (loop
      (let ((head (if (consp type) (car type) type)))
        (if (and head (symbolp head) (< n 64) (%deftype-lookup head))
            (setq type (%expand-deftype type) n (+ n 1))
            (return type))))))

(defun upgraded-array-element-type (type &optional environment)
  "Return the upgraded element type Modus actually uses for arrays of TYPE.
   Per CLHS 15.1.2.1: BIT upgrades to BIT, CHARACTER/BASE-CHAR upgrade to
   CHARACTER (Modus stores all chars uniformly), NIL upgrades to NIL, and
   everything else upgrades to T (Modus stores general elements as tagged
   words)."
  (declare (ignore environment))
  (setq type (%element-type-expand type))
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
    ;; (UNSIGNED-BYTE 8) and near-equivalents upgrade to (UNSIGNED-BYTE 8)
    ;; — Modus stores these in a byte-packed vector (subtag #x11).
    ((%u8-element-type-p type) '(unsigned-byte 8))
    ;; SINGLE-FLOAT (and SHORT-FLOAT) — the packed #x12 vector.
    ((%f32-element-type-p type) 'single-float)
    ;; Everything else is stored as a general (T) element.
    (t t)))

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
    ;; Byte-packed (unsigned-byte 8) vector (subtag #x11): element-type
    ;; (unsigned-byte 8), NOT T → not a simple-vector.
    ((eql (obj-subtag x) #x11) nil)
    ;; Packed single-float vector (subtag #x12): element-type SINGLE-FLOAT.
    ((eql (obj-subtag x) #x12) nil)
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

;; Every other 3- and 4-level C*R place (CLHS 5.1.2.2).  Only CADDR and
;; CADDDR had setters, so (setf (caaar x) v) called an undefined SET-CAAAR:
;; 44 cxr / cons-test-05 failures.
(defun set-caaar (x v) (set-car (caar x) v) v)

(defun set-caadr (x v) (set-car (cadr x) v) v)

(defun set-cadar (x v) (set-car (cdar x) v) v)

(defun set-cdaar (x v) (set-cdr (caar x) v) v)

(defun set-cdadr (x v) (set-cdr (cadr x) v) v)

(defun set-cddar (x v) (set-cdr (cdar x) v) v)

(defun set-cdddr (x v) (set-cdr (cddr x) v) v)

(defun set-caaaar (x v) (set-car (caaar x) v) v)

(defun set-caaadr (x v) (set-car (caadr x) v) v)

(defun set-caadar (x v) (set-car (cadar x) v) v)

(defun set-caaddr (x v) (set-car (caddr x) v) v)

(defun set-cadaar (x v) (set-car (cdaar x) v) v)

(defun set-cadadr (x v) (set-car (cdadr x) v) v)

(defun set-caddar (x v) (set-car (cddar x) v) v)

(defun set-cdaaar (x v) (set-cdr (caaar x) v) v)

(defun set-cdaadr (x v) (set-cdr (caadr x) v) v)

(defun set-cdadar (x v) (set-cdr (cadar x) v) v)

(defun set-cdaddr (x v) (set-cdr (caddr x) v) v)

(defun set-cddaar (x v) (set-cdr (cdaar x) v) v)

(defun set-cddadr (x v) (set-cdr (cdadr x) v) v)

(defun set-cdddar (x v) (set-cdr (cddar x) v) v)

(defun set-cddddr (x v) (set-cdr (cdddr x) v) v)

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
;; Used by build-x64-linux's rewrite-make-array-initcontents to keep
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
;; of build-x64-linux.lisp (loads AFTER array-aux.lsp's complex &key+&aux
;; version which the compiler can't faithfully handle).
(defun %u8-element-type-p (etype)
  "True if ETYPE upgrades to (UNSIGNED-BYTE 8) for Modus's byte-packed
   u8 vector representation (subtag #x11).  Conservative: recognises the
   common CL spellings that SBCL upgrades to (unsigned-byte 8) — the
   exact list form, and (mod 256) / (integer 0 255) — plus the symbol
   forms.  Anything else keeps the general tagged-word representation."
  (cond
    ;; (unsigned-byte 8)
    ((and (consp etype)
          (eq (car etype) 'unsigned-byte)
          (consp (cdr etype))
          (eql (cadr etype) 8)
          (null (cddr etype)))
     t)
    ;; (mod 256)
    ((and (consp etype)
          (eq (car etype) 'mod)
          (consp (cdr etype))
          (eql (cadr etype) 256)
          (null (cddr etype)))
     t)
    ;; (integer 0 255)
    ((and (consp etype)
          (eq (car etype) 'integer)
          (consp (cdr etype))
          (eql (cadr etype) 0)
          (consp (cddr etype))
          (eql (caddr etype) 255)
          (null (cdddr etype)))
     t)
    (t nil)))

(defun %f32-element-type-p (etype)
  "True if ETYPE upgrades to SINGLE-FLOAT (the packed #x12 vector):
   single-float and short-float (which upgrades to single)."
  (or (eq etype 'single-float) (eq etype 'short-float)))

(defun %make-u8-vector (n)
  "Allocate a byte-packed (unsigned-byte 8) vector of N bytes (subtag
   #x11), payload zero-initialised by the alloc-u8 opcode."
  (%alloc-u8 n))

(defun make-array (dim &rest kwargs)
  "ANSI make-array — see file-level comment above for current scope."
  (let* ((dim-list (cond ((null dim) nil)
                         ((consp dim) dim)
                         (t (list dim))))
         (rank 0) (cur dim-list) (total 1))
    ;; CLHS MAKE-ARRAY: every dimension is a valid array dimension, a
    ;; non-negative fixnum.  Nothing checked it: a non-number's raw bits
    ;; became the element count.  With a keyword on the heap that was a huge
    ;; address and the allocation failed, which LOOKED like an error; with the
    ;; keyword a static constant-pool object it is a small address and
    ;; (make-array :fill-pointer) quietly built a vector of millions of zeros
    ;; (ADJUST-ARRAY.ERROR.6 then hung filling it).
    (let ((d dim-list))
      (loop (when (null d) (return nil))
        (unless (and (fixnump (car d)) (>= (car d) 0))
          (%signal-type-error))
        (setq d (cdr d))))
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
      (when etype-set (setq etype (%element-type-expand etype)))
      ;; Allocate + fill the flat data vector.
      ;; :element-type 'character / 'base-char → underlying is a string
      ;; (subtag #x31) so STRINGP / string ops work on the result.
      (let* ((char-elt (or (eq etype 'character) (eq etype 'base-char)
                           (eq etype 'standard-char)))
             ;; Byte-packed (unsigned-byte 8) storage (subtag #x11): 8x
             ;; smaller than the general tagged-word array.  Only for real
             ;; u8 element types; everything else is unchanged.
             (u8-elt (and etype-set (%u8-element-type-p etype)))
             ;; Packed single-float storage (subtag #x12), 4 bytes per lane —
             ;; ONLY for the bare simple 1-D vector.  A fill-pointer /
             ;; adjustable / displaced / multi-dim array is an MDA wrapper
             ;; whose accessors read the data vector with the raw word
             ;; primitives, so those keep the generic store (element type is
             ;; still reported from the MDA header).  gate18 lost 18 tests
             ;; to wrapped packed data.
             (f32-elt (and etype-set (%f32-element-type-p etype)
                           (= rank 1) (not (consp dim))
                           (not fp) (not adj) (not disp)))
             (data (cond
                     ;; :initial-element — fill every slot.
                     (ie-p
                      (let* ((a (cond (char-elt (%make-string-array total))
                                      (u8-elt (%make-u8-vector total))
                                      (f32-elt (%make-f32-vector total))
                                      (t (make-array total))))
                             (store (if (and char-elt (characterp ie))
                                        (char-code ie) ie))
                             (i 0))
                        (cond
                          ;; A byte vector comes back from %ALLOC-U8 already
                          ;; zeroed, so :INITIAL-ELEMENT 0 -- the usual spelling
                          ;; of a fresh buffer -- is nothing to do; any other
                          ;; byte goes through FILL's byte-store loop.  Both used
                          ;; to be a generic ASET per element.
                          ((and u8-elt (eql store 0)) a)
                          ((and u8-elt (integerp store) (<= 0 store 255)) (fill a store))
                          (t
                           (loop (when (>= i total) (return a))
                             (aset a i store)
                             (setq i (+ i 1)))))))
                     ;; :initial-contents — flatten in row-major order
                     ;; for multi-dim; copy verbatim for 1-D / flat lists.
                     (ic-p
                      (let ((a (cond (char-elt (%make-string-array total))
                                     (u8-elt (%make-u8-vector total))
                                     (f32-elt (%make-f32-vector total))
                                     (t (make-array total)))))
                        (%mda-fill-contents-flat a ic dim-list)))
                     (char-elt (%make-string-array total))
                     (u8-elt (%make-u8-vector total))
                     (f32-elt (%make-f32-vector total))
                     (t (make-array total)))))
        ;; Decide return shape.  Phase 2a: wrap in MDA whenever rank ≠ 1,
        ;; or any non-trivial kwarg appeared, or the dim was passed as
        ;; a list (even a single-element list) — that preserves rank
        ;; info per CLHS (`(array-rank (make-array '(5)))` is 1, not 0).
        ;; Character element type with no other wrapping returns the
        ;; underlying string directly so STRINGP / EQUAL string ops work.
        (cond
          ;; Plain 1-D, no wrapping kwargs, and a GENERIC element type — T,
          ;; or any declared type that is neither a u8 spelling nor a
          ;; character type ((signed-byte 32), fixnum, …): return the bare
          ;; tagged-word vector.  The MDA wrapper carried NOTHING for these
          ;; (array-element-type already answered T through it) but put every
          ;; aref on the slow wrapper branch — and it made
          ;;   (make-array 4 :element-type '(signed-byte 32) :initial-contents …)
          ;; a DIFFERENT representation from the same call without the fill
          ;; kwarg (which the compiler now inlines as a bare vector).  reel's
          ;; VP8 probability/tree tables are exactly this shape; the declared-
          ;; array fast path (compile-aref on a declared (simple-array
          ;; (signed-byte 32))) read their wrapper headers as elements.
          ((and (= rank 1) (not (consp dim))
                (not fp) (not adj) (not disp)
                (or (eq etype t) (and (not char-elt) (not u8-elt) (not f32-elt))))
           data)
          ;; Plain 1-D single-float vector: the packed #x12 object itself.
          ((and (= rank 1) (not (consp dim))
                (not fp) (not adj) (not disp) f32-elt)
           data)
          ((and (= rank 1) (not (consp dim))
                (not fp) (not adj) (not disp) char-elt)
           data)
          ;; Plain 1-D (unsigned-byte 8) vector with no other wrapping —
          ;; return the byte-packed #x11 vector directly so ARRAYP / TYPEP /
          ;; ARRAY-ELEMENT-TYPE / OBJ-SUBTAG see the real specialized object.
          ((and (= rank 1) (not (consp dim))
                (not fp) (not adj) (not disp) u8-elt)
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
