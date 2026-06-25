;;;; cl-clos.lisp — Minimal CLOS implementation
;;;; Part of the Modus CL runtime. Depends on cl-eval.lisp.

;;; ===================================================
;;; Minimal CLOS implementation for ANSI test suite
;;; ===================================================

;; Class registry: alist of (class-name . cls-array)
;; cls-array: #(%clos-class name slot-names-list supers cpl)
;;   slot [0] = '%clos-class
;;   slot [1] = name (symbol)
;;   slot [2] = slot-names (list)
;;   slot [3] = direct supers (list of class-names)
;;   slot [4] = CPL (list of class-names, most-specific first)
(defvar *clos-classes* nil)

;; Per-class slot info registry: alist of
;;   (class-name . (initarg-map . initform-map))
;; initarg-map: list of (initarg-string-name . slot-name-symbol)
;; initform-map: list of (slot-name-symbol . thunk-fn)
;; Populated by %register-clos-slot-info from the SBCL-side defclass rewriter.
(defvar *clos-slot-info* nil)

;; Per-class default-initargs registry: alist
;;   (class-name . initarg-thunks)
;; initarg-thunks: list of (initarg-key . thunk-fn) pairs.  Per CLHS 7.1.4,
;; the value form is re-evaluated each time make-instance is called, hence
;; the 0-arity thunks.
(defvar *clos-default-initargs* nil)

;; T only while a MAKE-INSTANCE entry path runs SHARED-INITIALIZE, so the
;; default-initargs get applied.  Bare SHARED-INITIALIZE / REINITIALIZE-
;; INSTANCE leave it NIL (defvar default — see CLAUDE.md item 7) so they do
;; NOT apply default-initargs (CLHS 7.1.4).
(defvar *clos-applying-defaults* nil)

;; slot-unbound methods: list of (class-name slot-spec fn)
;; slot-spec: nil = any slot; symbol = that specific slot name
(defvar *slot-unbound-methods* nil)

;; Unbound slot sentinel: fixnum -999.
;; Using a fixnum literal avoids SYMBOL-VALUE call clobbering arr-reg
;; in variable-index aset during %make-instance initialization loop.
(defvar *%unbound-slot* -999)

;; Generic function registry: alist of (name . gf-object)
;; gf-object is a vector: #(%generic-function name lambda-list methods-alist combination)
;;   methods-alist: list of (qualifier specializer-list . fn)
(defvar *generic-functions* nil)

;; Method combination registry: alist of (name . mc-object)
;; mc-object: list (name operator identity-with-one-argument)
(defvar *method-combinations* nil)

;; Default-primary registry: alist of (gf-name . symbol-naming-default-fn).
;; Looked up by %gf-dispatch-standard when an applicable method list contains
;; only auxiliary methods (:before / :after / :around).  Per CLHS, the
;; standard system-supplied primary still runs in that case — fixing
;; shared-initialize.7.x / 8.x, change-class.5.x and similar tests that
;; would otherwise hit "no applicable primary method".
(defvar *gf-default-primaries* nil)

;; Dynamic variable for call-next-method chain
;; Holds: list of (qualifier specializer-list . fn) remaining
(defvar *%next-methods* nil)
;; Dynamic variable for current method's args (for call-next-method with no args)
(defvar *%current-gf-args* nil)
;; Dynamic variable for the GF object currently being dispatched.  Set at the
;; top of each %gf-dispatch-* entry so CALL-NEXT-METHOD with replacement args
;; can recompute the applicable-method set and enforce CLHS 7.6.6.2 (the new
;; argument set must yield the same ordered applicable methods).
(defvar *%current-gf* nil)

;;; ============================================================
;;; Built-in class precedence hierarchy
;;; Used for dispatch on non-CLOS objects
;;; ============================================================

;; Returns the CPL (list of class names, most-specific first)
;; for a built-in type determined from an object
(defun %builtin-cpl (type-name)
  "Return CPL for a built-in type name."
  (cond
    ((eq type-name 'integer)   '(integer rational real number t))
    ((eq type-name 'ratio)     '(ratio rational real number t))
    ((eq type-name 'rational)  '(rational real number t))
    ((eq type-name 'float)     '(float real number t))
    ((eq type-name 'real)      '(real number t))
    ((eq type-name 'complex)   '(complex number t))
    ((eq type-name 'number)    '(number t))
    ((eq type-name 'string)    '(string vector array sequence t))
    ;; VECTOR/ARRAY CPLs append STRUCTURE-OBJECT + STANDARD-OBJECT:
    ;; Modus struct instances are markerless plain arrays (defstruct
    ;; emits bare make-array), indistinguishable from vectors.  Before
    ;; %type-of-for-dispatch knew about vectors at all, BOTH dispatched
    ;; as STANDARD-OBJECT — so make-load-form's STRUCTURE-OBJECT /
    ;; STANDARD-OBJECT methods matched struct objects.  Keeping those
    ;; in the CPL (after the array classes) preserves that matching
    ;; while letting (y array)/(y vector) specializers work
    ;; (defgeneric.33).
    ((eq type-name 'vector)
     '(vector array sequence structure-object standard-object t))
    ((eq type-name 'array)
     '(array structure-object standard-object t))
    ((eq type-name 'condition) '(condition standard-object t))
    ((eq type-name 'cons)      '(cons list sequence t))
    ((eq type-name 'list)      '(list sequence t))
    ((eq type-name 'null)      '(null symbol list sequence t))
    ((eq type-name 'symbol)    '(symbol t))
    ((eq type-name 'character) '(character t))
    ((eq type-name 'function)  '(function t))
    ((eq type-name 'boolean)   '(boolean symbol t))
    (t                         (list type-name 't))))

(defun %type-of-for-dispatch (obj)
  "Return the most-specific built-in class name for OBJ."
  (cond
    ((null obj)        'null)
    ((eq obj t)        'boolean)
    ((fixnump obj)     'integer)
    ((stringp obj)     'string)
    ((characterp obj)  'character)
    ((symbolp obj)     'symbol)
    ;; ratio check before general cons (ratiop also checks consp)
    ((ratiop obj)      'ratio)
    ;; float check — floatp-impl checks for float objects
    ((floatp-impl obj) 'float)
    ;; complex: 3-slot %complex-marker array (cl-sequences.lisp).
    ;; Without this case complex args fell through to standard-object
    ;; and GF methods specialized on COMPLEX / NUMBER never matched
    ;; (dg-mc.N.6/.8 once complex literals stopped compiling as 0).
    ((%complex-p obj)  'complex)
    ((consp obj)       'cons)
    ;; conditions — detectable via the condition-type registry; their
    ;; CPL keeps STANDARD-OBJECT so methods that matched under the old
    ;; blanket fallback still match (make-load-form.6/9).
    ((%condition-p obj) 'condition)
    ;; struct instances carry a %struct-instance marker in slot 0; their
    ;; most-specific "class" is their struct type-name.  Must precede the
    ;; vector/array cases (a struct instance is a #x32 array).
    ((%struct-instance-p obj) (%struct-type-name obj))
    ;; vectors / arrays — without these every array dispatched as
    ;; STANDARD-OBJECT, so methods specialized on (y array) / (y vector)
    ;; were never applicable (defgeneric.33).  CLOS instances never
    ;; reach here (%obj-cpl handles them first).
    ((vectorp obj)     'vector)
    ((arrayp obj)      'array)
    (t 'standard-object)))

(defun %obj-class-name (obj)
  "Return most-specific class name for OBJ (CLOS or built-in)."
  (if (%clos-instance-p obj)
    (aref obj 1)
    (%type-of-for-dispatch obj)))

(defun %obj-cpl (obj)
  "Return CPL for OBJ as list of class names."
  (cond
    ((%clos-instance-p obj)
     (let ((cls-name (aref obj 1)))
       (let ((cls (%find-clos-class cls-name)))
         (if cls
           (aref cls 4)
           (list cls-name 't)))))
    ;; A CLOS class descriptor itself is an instance of standard-class
    ;; (per CLHS) for dispatch purposes. Tests like change-class.6.x
    ;; specialize on (new-class standard-class) — so class objects need
    ;; standard-class in their CPL.
    ((%clos-class-p obj)
     '(standard-class class standard-object t))
    ;; Struct instance: type-name chain + STRUCTURE-OBJECT STANDARD-OBJECT T.
    ;; Must precede the builtin-cpl fallback — %type-of-for-dispatch returns
    ;; the bare struct type-name, and %builtin-cpl's default would drop the
    ;; structure-object/standard-object tail that make-load-form's default
    ;; method (and any STRUCTURE-OBJECT specializer) dispatches on.
    ((%struct-instance-p obj)
     (%struct-cpl obj))
    (t
     (%builtin-cpl (%type-of-for-dispatch obj)))))

(defun %clos-instance-p (x)
  "True if X is a CLOS instance array."
  (if (or (fixnump x) (consp x) (null x)) nil
    (if (= (obj-subtag x) #x32)
      (if (>= (array-length x) 2)
        (eq (aref x 0) '%clos-instance)
        nil)
      nil)))

(defun %clos-class-p (x)
  "True if X is a CLOS class descriptor array."
  (if (or (fixnump x) (consp x) (null x)) nil
    (if (= (obj-subtag x) #x32)
      (if (>= (array-length x) 2)
        (eq (aref x 0) '%clos-class)
        nil)
      nil)))

;;; ============================================================
;;; Class registration with inheritance + CPL computation
;;; ============================================================

(defun %cpl-head-in-tail-p (head lists)
  "True iff HEAD appears in the tail (cdr) of any list in LISTS."
  (let ((cur lists) (found nil))
    (loop
      (when (null cur) (return found))
      (when (member head (cdr (car cur)) :test #'eq)
        (setq found t)
        (return found))
      (setq cur (cdr cur)))))

(defun %cpl-merge (lists)
  "C3 merge of multiple precedence lists.  Returns merged CPL.
   At each step, pick the first list's head that doesn't appear in
   the tail of any other list, append to result, remove from heads
   of all lists.  Repeat until all lists are empty."
  (let ((result nil))
    (loop
      ;; Drop empty lists.
      (let ((non-empty nil) (cur lists))
        (loop
          (when (null cur) (return nil))
          (when (consp (car cur)) (setq non-empty (cons (car cur) non-empty)))
          (setq cur (cdr cur)))
        (setq lists (nreverse non-empty)))
      (when (null lists) (return (nreverse result)))
      ;; Find a good head — head of some list, not in tail of any other.
      (let ((found nil) (cur lists))
        (loop
          (when (null cur) (return nil))
          (when (null found)
            (let ((cand (car (car cur))))
              (unless (%cpl-head-in-tail-p cand lists)
                (setq found cand))))
          (setq cur (cdr cur)))
        (when (null found)
          ;; Inconsistent precedence graph — fall back to first head.
          (setq found (car (car lists))))
        (setq result (cons found result))
        ;; Remove found from heads of all lists.
        (setq lists (mapcar (lambda (lst)
                              (if (eq (car lst) found) (cdr lst) lst))
                            lists))))))

(defun %compute-cpl (name supers)
  "Compute CPL for NAME with SUPERS via C3 linearization (CLHS 4.3.5).
   Strategy:
     L(name) = (name) + merge(L(s1), L(s2), …, (s1 s2 …))
   For built-in supers like T or STANDARD-OBJECT, use %builtin-cpl.

   Diamond inheritance ordering: (D inherits (B C); B, C share A) →
   (D B C A standard-object T).  Critical for initform lookup: the
   most-specific class declaring an initform wins, and C must come
   before A in the CPL.

   Previous naive DFS-and-dedup produced (D B A C standard-object T),
   making A's initform win over C's — wrong per CLHS."
  (let* ((super-cpls
          (let ((acc nil) (cur supers))
            (loop
              (when (null cur) (return (nreverse acc)))
              (let* ((sup-name (car cur))
                     (sup-cls (%find-clos-class sup-name))
                     (cpl (if sup-cls
                              (aref sup-cls 4)
                              (%builtin-cpl sup-name))))
                (setq acc (cons cpl acc)))
              (setq cur (cdr cur)))))
         (merge-input
          (let ((acc nil) (cur super-cpls))
            (loop
              (when (null cur) (return (nreverse (cons supers acc))))
              (setq acc (cons (car cur) acc))
              (setq cur (cdr cur))))))
    (cons name (%cpl-merge merge-input))))

(defun %defclass (name slot-names supers)
  "Register CLOS class NAME with SLOT-NAMES list and SUPERS.
   SLOT-NAMES is the directly-declared list; the effective slots are
   computed by walking the CPL and unioning names — most-specific class
   first.  This gives instances enough storage for inherited slots."
  (let* ((cpl (%compute-cpl name (if (null supers) '(standard-object) supers)))
         ;; Effective slots: this class's slots + each ancestor's slots
         ;; (de-duplicated, in CPL order).  Walk cpl skipping the leading
         ;; reference to NAME itself (since slot-names is its own contribution).
         (effective-slots
          (let ((acc nil) (seen nil))
            (dolist (n slot-names)
              (unless (member n seen :test #'eq)
                (setq seen (cons n seen))
                (setq acc (cons n acc))))
            (dolist (cls-name cpl)
              (unless (eq cls-name name)
                (let ((parent (%find-clos-class cls-name)))
                  (when parent
                    (let ((p-slots (aref parent 2)))
                      (dolist (n p-slots)
                        (unless (member n seen :test #'eq)
                          (setq seen (cons n seen))
                          (setq acc (cons n acc)))))))))
            (nreverse acc))))
    ;; CLHS 4.3.6: redefining a class must preserve the class object's
    ;; identity — (eq (find-class N) (find-class N)) stays T and any cobj
    ;; captured before the redefine still denotes the same (now updated)
    ;; class.  So when NAME is already registered, MUTATE the existing
    ;; array in place rather than allocating a fresh one.
    (let ((cls (%find-clos-class name)))
      (if (and cls (%clos-class-p cls))
          (progn
            (aset cls 2 effective-slots)
            (aset cls 3 supers)
            (aset cls 4 cpl))
          (progn
            (setq cls (make-array 5))
            (aset cls 0 '%clos-class)
            (aset cls 1 name)
            (aset cls 2 effective-slots)
            (aset cls 3 supers)
            (aset cls 4 cpl)
            ;; Remove any stale entry (different array object for same
            ;; name) then add the fresh one.
            (let ((new-registry nil)
                  (cur *clos-classes*))
              (loop
                (when (null cur) (return nil))
                (when (not (eq (car (car cur)) name))
                  (setq new-registry (cons (car cur) new-registry)))
                (setq cur (cdr cur)))
              (setq *clos-classes* (cons (cons name cls) new-registry)))))
      name)))

(defun %find-clos-class (name)
  "Return class descriptor for NAME, or nil."
  (let ((cur *clos-classes*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

;;; ============================================================
;;; DEFSTRUCT — runtime structure type support
;;; ============================================================
;;;
;;; A struct instance is a heap array shaped exactly like a CLOS
;;; instance so the GC, ARRAYP, and the dispatch machinery all keep
;;; working:
;;;   slot [0] = '%struct-instance   (marker, distinct from %clos-instance)
;;;   slot [1] = type-name           (symbol; the most specific struct type)
;;;   slot [2..] = user slot values  (in effective-slot order)
;;;
;;; The registry maps a struct type's NAME-HASH to a descriptor array:
;;;   #(name-hash type-name parent-name slot-names conc-name)
;;; where slot-names is the EFFECTIVE slot list (own + inherited, parent
;;; slots first per CLHS 3.4.5).  Keying by name-hash (not eq on the
;;; symbol object) keeps the lookup robust across the native-MVM-sym /
;;; CL-sym-wrapper boundary — the same reason the condition registry
;;; and the SFT key by hash.
(defvar *struct-types* nil)

(defun %struct-type-desc-hash (d)   (aref d 0))
(defun %struct-type-desc-name (d)   (aref d 1))
(defun %struct-type-desc-parent (d) (aref d 2))
(defun %struct-type-desc-slots (d)  (aref d 3))
(defun %struct-type-desc-conc (d)   (aref d 4))

(defun %struct-name-hash (name)
  "Return the canonical hash key for a struct type NAME (any symbol
   flavor) or NIL if NAME isn't a symbol."
  (let ((nh (%sym-name-or-hash name)))
    (if nh (cdr nh) nil)))

(defun %find-struct-type (name)
  "Return the struct-type descriptor for NAME (a symbol), or NIL."
  (let ((key (%struct-name-hash name)))
    (if (null key) nil
      (let ((cur *struct-types*))
        (loop
          (when (null cur) (return nil))
          (when (= (car (car cur)) key) (return (cdr (car cur))))
          (setq cur (cdr cur)))))))

(defun %register-struct-type (name parent-name slot-names conc-name)
  "Register (or redefine) struct type NAME.  PARENT-NAME is the :include
   parent (or NIL).  SLOT-NAMES is the EFFECTIVE slot list.  Returns NAME."
  (let* ((key (%struct-name-hash name))
         (desc (make-array 5)))
    (aset desc 0 key)
    (aset desc 1 name)
    (aset desc 2 parent-name)
    (aset desc 3 slot-names)
    (aset desc 4 conc-name)
    ;; Drop any stale entry with the same key, then prepend.
    (let ((new-reg nil) (cur *struct-types*))
      (loop
        (when (null cur) (return nil))
        (when (not (= (car (car cur)) key))
          (setq new-reg (cons (car cur) new-reg)))
        (setq cur (cdr cur)))
      (setq *struct-types* (cons (cons key desc) new-reg)))
    name))

(defun %struct-instance-p (x)
  "True if X is a struct instance array (slot-0 = '%struct-instance)."
  (if (or (fixnump x) (consp x) (null x)) nil
    (if (= (obj-subtag x) #x32)
      (if (>= (array-length x) 2)
        (eq (aref x 0) '%struct-instance)
        nil)
      nil)))

(defun %struct-type-name (x)
  "Return the type-name of struct instance X (slot 1)."
  (aref x 1))

(defun %struct-type-ancestor-p (type-name ancestor-name)
  "True if struct type TYPE-NAME is, or descends (via :include) from,
   ANCESTOR-NAME.  Compares by name-hash so symbol-flavor doesn't matter."
  (let ((target (%struct-name-hash ancestor-name))
        (cur-name type-name))
    (if (null target) nil
      (let ((result nil))
        (loop
          (when (null cur-name) (return result))
          (let ((ch (%struct-name-hash cur-name)))
            (when (and ch (= ch target)) (setq result t) (return result)))
          (let ((desc (%find-struct-type cur-name)))
            (if (null desc)
                (return result)
                (setq cur-name (%struct-type-desc-parent desc)))))))))

(defun %struct-instance-typep (x type-name)
  "True if struct instance X is of struct type TYPE-NAME (or a subtype)."
  (if (not (%struct-instance-p x)) nil
    (%struct-type-ancestor-p (%struct-type-name x) type-name)))

(defun %struct-cpl (x)
  "CPL for struct instance X: its own type-name, then each registered
   :include ancestor, then STRUCTURE-OBJECT STANDARD-OBJECT T.  The
   structure-object / standard-object tail preserves the pre-marker
   behavior where struct instances (markerless arrays) matched a default
   make-load-form method specialized on STRUCTURE-OBJECT / STANDARD-OBJECT
   (make-load-form.struct.02 etc.).  The leading type-name chain lets
   struct-type method specializers work once the registry knows the type."
  (let ((chain nil) (cur (%struct-type-name x)))
    ;; Walk own type + :include ancestors (registry-dependent; the
    ;; compile-time defstruct path doesn't register, so chain is usually
    ;; just the single own type-name — that's fine, the tail still matches).
    (loop
      (when (null cur) (return nil))
      (setq chain (cons cur chain))
      (let ((desc (%find-struct-type cur)))
        (if (null desc)
            (setq cur nil)
            (setq cur (%struct-type-desc-parent desc)))))
    ;; chain is now most-general .. most-specific; reverse to specific-first
    (let ((cpl '(structure-object standard-object t)) (c chain))
      (loop
        (when (null c) (return nil))
        (setq cpl (cons (car c) cpl))
        (setq c (cdr c)))
      cpl)))

(defun %struct-instance-named-p (x type-name)
  "True if X is a struct instance whose own type-name is TYPE-NAME, OR
   (via the registry) a subtype of TYPE-NAME.  Registry-independent for
   the direct-type case: compares slot-1 to TYPE-NAME by hash so the
   predicate works even when the boot-time %register-struct-type thunk
   for this type didn't run."
  (if (not (%struct-instance-p x)) nil
    (let ((th (%struct-name-hash (%struct-type-name x)))
          (nh (%struct-name-hash type-name)))
      (cond
        ((and th nh (= th nh)) t)
        ((%struct-type-ancestor-p (%struct-type-name x) type-name) t)
        (t nil)))))

(defun %alloc-struct (type-name slot-values)
  "Allocate a struct instance of TYPE-NAME with SLOT-VALUES (a list in
   effective-slot order)."
  (let* ((n (length slot-values))
         (inst (make-array (+ 2 n))))
    (aset inst 0 '%struct-instance)
    (aset inst 1 type-name)
    (let ((i 0) (cur slot-values))
      (loop
        (when (null cur) (return nil))
        ;; force dest=frame-slot for variable-index aset (CLAUDE.md item 2)
        (let ((dummy (aset inst (+ 2 i) (car cur)))) dummy)
        (setq i (+ i 1))
        (setq cur (cdr cur))))
    inst))

(defun %struct-slot-index (type-name slot-name)
  "0-based index of SLOT-NAME within TYPE-NAME's effective slots, or -1."
  (let ((desc (%find-struct-type type-name)))
    (if (null desc) -1
      (let ((slots (%struct-type-desc-slots desc))
            (target (%struct-name-hash slot-name))
            (i 0) (result -1))
        (if (null target) -1
          (progn
            (let ((cur slots))
              (loop
                (when (null cur) (return nil))
                (let ((sh (%struct-name-hash (car cur))))
                  (when (and sh (= sh target)) (setq result i) (return nil)))
                (setq i (+ i 1))
                (setq cur (cdr cur))))
            result))))))

(defun %struct-keyword-matches-slot-p (kw slot-name)
  "True if constructor keyword KW (a keyword like :FOO, or any symbol)
   names struct slot SLOT-NAME.  Compares by name string so the keyword
   (#x53, KEYWORD package) and the slot symbol (CL-TEST package) match
   despite differing package hashes."
  (let ((kn (symbol-name kw))
        (sn (symbol-name slot-name)))
    (if (and kn sn (> (length kn) 0) (string= kn sn)) t nil)))

(defun %struct-ref (x slot-name)
  "Read SLOT-NAME from struct instance X."
  (let ((idx (%struct-slot-index (%struct-type-name x) slot-name)))
    (if (< idx 0) nil (aref x (+ 2 idx)))))

(defun %struct-set (x slot-name val)
  "Write VAL to SLOT-NAME of struct instance X.  Returns VAL."
  (let ((idx (%struct-slot-index (%struct-type-name x) slot-name)))
    (when (>= idx 0)
      (let ((dummy (aset x (+ 2 idx) val))) dummy))
    val))

(defun %struct-copy (x)
  "Shallow-copy struct instance X."
  (let* ((n (array-length x))
         (new (make-array n))
         (i 0))
    (loop
      (when (= i n) (return nil))
      (let ((dummy (aset new i (aref x i)))) dummy)
      (setq i (+ i 1)))
    new))

(defun %register-clos-slot-info (class-name initarg-map initform-map)
  "Register per-slot initarg→slot mapping and initform thunks for CLASS-NAME.
   Called from rewriter-emitted code right after %defclass."
  ;; Replace existing entry if present
  (let ((new-reg nil)
        (cur *clos-slot-info*))
    (loop
      (when (null cur) (return nil))
      (when (not (eq (car (car cur)) class-name))
        (setq new-reg (cons (car cur) new-reg)))
      (setq cur (cdr cur)))
    (setq *clos-slot-info*
          (cons (cons class-name (cons initarg-map initform-map))
                new-reg)))
  class-name)

;;; ============================================================
;;; :allocation :class — per-class shared slot storage
;;; ============================================================
;;; *clos-class-slots* maps class-name → list of slot names with
;;;   :allocation :class.  Walked through the CPL when checking
;;;   whether a slot reference should hit class-shared storage.
;;; *clos-class-slot-values* maps (class-name . slot-name) → value.
;;;   For a class-allocated slot, the storage lives in the FIRST
;;;   class in the CPL that declares it (the "owning" class).
;;;
;;; CLHS 7.5.2: instances of subclasses share the inherited
;;; class-allocated slot.  We find the owning class via CPL walk
;;; (first ancestor that declared :allocation :class for the slot).
(defvar *clos-class-slots* nil)
(defvar *clos-class-slot-values* nil)

(defun %register-clos-class-slots (class-name class-slot-names)
  "Register CLASS-SLOT-NAMES as the :allocation :class slots of CLASS-NAME."
  (let ((new-reg nil) (cur *clos-class-slots*))
    (loop
      (when (null cur) (return nil))
      (when (not (eq (car (car cur)) class-name))
        (setq new-reg (cons (car cur) new-reg)))
      (setq cur (cdr cur)))
    (setq *clos-class-slots*
          (cons (cons class-name class-slot-names) new-reg)))
  class-name)

(defun %class-slots-for (class-name)
  "Return list of class-allocated slot names for CLASS-NAME (no CPL walk)."
  (let ((cur *clos-class-slots*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) class-name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

;; Per-class directly-declared-slot-names registry: alist
;;   (class-name . direct-slot-names).
;; Records every slot a class directly declares (any allocation).  Needed
;; so %slot-class-owner can detect when a subclass shadows an ancestor's
;; :allocation :class slot with its own :allocation :instance declaration
;; — CLHS 7.5.2: the most-specific class's allocation wins.
(defvar *clos-direct-slots* nil)

(defun %register-clos-direct-slots (class-name direct-slot-names)
  "Register DIRECT-SLOT-NAMES as the slots CLASS-NAME directly declares
   (any allocation).  Replaces any prior entry."
  (let ((new-reg nil) (cur *clos-direct-slots*))
    (loop
      (when (null cur) (return nil))
      (when (not (eq (car (car cur)) class-name))
        (setq new-reg (cons (car cur) new-reg)))
      (setq cur (cdr cur)))
    (setq *clos-direct-slots*
          (cons (cons class-name direct-slot-names) new-reg)))
  class-name)

(defun %direct-slots-for (class-name)
  "Return list of direct-slot-names for CLASS-NAME (any allocation)."
  (let ((cur *clos-direct-slots*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) class-name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun %slot-class-owner (class-name slot-name)
  "Return the class-name in CLASS-NAME's CPL that declared SLOT-NAME
   as :allocation :class — or NIL if no class-allocation found, or NIL
   if a more-specific class shadows an ancestor's :class allocation with
   its own :instance declaration (CLHS 7.5.2).

   Walk CPL most-specific first.  For each class:
   - If class directly declares the slot AS :class → return class
   - If class directly declares the slot AS :instance → return NIL
     (shadowing — slot lives on the instance, no class storage)
   - Otherwise continue walking."
  (let ((cls (%find-clos-class class-name)))
    (when (null cls) (return-from %slot-class-owner nil))
    (let ((cpl (aref cls 4))
          (result nil)
          (decided nil))
      (let ((cur cpl))
        (loop
          (when (or decided (null cur)) (return nil))
          (let ((cn (car cur)))
            (when (member slot-name (%direct-slots-for cn) :test #'eq)
              (cond
                ((member slot-name (%class-slots-for cn) :test #'eq)
                 (setq result cn)
                 (setq decided t))
                (t
                 ;; :instance at this class → no class storage above
                 (setq decided t)))))
          (setq cur (cdr cur))))
      result)))

(defun %class-slot-get (class-name slot-name default)
  "Return the class-shared value for SLOT-NAME on CLASS-NAME (resolves
   to the owning class in the CPL).  Returns DEFAULT if unset."
  (let ((owner (%slot-class-owner class-name slot-name)))
    (when owner
      (let ((cur *clos-class-slot-values*))
        (loop
          (when (null cur) (return default))
          (let ((entry (car cur)))
            (when (and (eq (car (car entry)) owner)
                       (eq (cdr (car entry)) slot-name))
              (return (cdr entry))))
          (setq cur (cdr cur)))))))

(defun %class-slot-set (class-name slot-name value)
  "Set the class-shared value for SLOT-NAME on CLASS-NAME (writes
   to the owning class in the CPL)."
  (let ((owner (%slot-class-owner class-name slot-name)))
    (when owner
      ;; Update existing entry or prepend new one.
      (let ((found nil) (cur *clos-class-slot-values*))
        (loop
          (when (null cur) (return nil))
          (let ((entry (car cur)))
            (when (and (eq (car (car entry)) owner)
                       (eq (cdr (car entry)) slot-name))
              (setf (cdr entry) value)
              (setq found t)
              (return nil)))
          (setq cur (cdr cur)))
        (unless found
          (setq *clos-class-slot-values*
                (cons (cons (cons owner slot-name) value)
                      *clos-class-slot-values*))))
      value)))

(defun %class-slot-bound-p (class-name slot-name)
  "True iff SLOT-NAME on CLASS-NAME (or an ancestor) has been set."
  (let ((owner (%slot-class-owner class-name slot-name)))
    (when owner
      (let ((cur *clos-class-slot-values*))
        (loop
          (when (null cur) (return nil))
          (let ((entry (car cur)))
            (when (and (eq (car (car entry)) owner)
                       (eq (cdr (car entry)) slot-name))
              (return t)))
          (setq cur (cdr cur)))))))

(defun %clos-slot-info-for (class-name)
  "Return (initarg-map . initform-map) for CLASS-NAME, or nil."
  (let ((cur *clos-slot-info*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) class-name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun %clos-initarg-lookup-1 (class-name initarg-key)
  "Look up INITARG-KEY in the initarg-map of just this CLASS-NAME
   (no super walk).  Returns slot-name or nil."
  (let ((info (%clos-slot-info-for class-name)))
    (when (null info) (return-from %clos-initarg-lookup-1 nil))
    (let ((iar (car info)))
      (let ((cur iar))
        (loop
          (when (null cur) (return nil))
          (let ((entry (car cur)))
            (let ((stored-key (car entry)))
              (when (cond
                      ((eq stored-key initarg-key) t)
                      ((and (%native-mvm-sym-p stored-key)
                            (%native-mvm-sym-p initarg-key))
                       (= (%native-mvm-sym-hash stored-key)
                          (%native-mvm-sym-hash initarg-key)))
                      ((and (%cl-sym-p stored-key) (%cl-sym-p initarg-key))
                       (string-equal (%cl-sym-name stored-key)
                                     (%cl-sym-name initarg-key)))
                      (t nil))
                (return (cdr entry)))))
          (setq cur (cdr cur)))))))

(defun %clos-initarg-to-slot (class-name initarg-key)
  "Map INITARG-KEY to slot-name walking the class's CPL — inherited
   slots get their parent's initarg map.  Without the CPL walk,
   (make-instance 'subclass :inherited-arg V) would silently drop V
   because the subclass's own initarg-map only knows its OWN slots."
  (let ((direct (%clos-initarg-lookup-1 class-name initarg-key)))
    (when direct (return-from %clos-initarg-to-slot direct)))
  (let ((cls (%find-clos-class class-name)))
    (when (null cls) (return-from %clos-initarg-to-slot nil))
    ;; aref cls 4 is the CPL.  Skip the leading class itself (already checked).
    (let ((cpl (cdr (aref cls 4))))
      (let ((cur cpl))
        (loop
          (when (null cur) (return nil))
          (let ((found (%clos-initarg-lookup-1 (car cur) initarg-key)))
            (when found (return found)))
          (setq cur (cdr cur)))))))

(defun %clos-initarg-lookup-all-1 (class-name initarg-key acc)
  "Collect ALL slot-names INITARG-KEY maps to in just CLASS-NAME's initarg
   map (no super walk), prepending onto ACC.  Per CLHS 7.1.4 a single
   initarg may initialize several slots (e.g. slot s1 and s2 both declare
   :initarg :s1b) — every such slot must be set."
  (let ((info (%clos-slot-info-for class-name)))
    (when (null info) (return-from %clos-initarg-lookup-all-1 acc))
    (let ((cur (car info)))
      (loop
        (when (null cur) (return acc))
        (let* ((entry (car cur))
               (stored-key (car entry)))
          (when (cond
                  ((eq stored-key initarg-key) t)
                  ((and (%native-mvm-sym-p stored-key)
                        (%native-mvm-sym-p initarg-key))
                   (= (%native-mvm-sym-hash stored-key)
                      (%native-mvm-sym-hash initarg-key)))
                  ((and (%cl-sym-p stored-key) (%cl-sym-p initarg-key))
                   (string-equal (%cl-sym-name stored-key)
                                 (%cl-sym-name initarg-key)))
                  (t nil))
            (let ((sn (cdr entry)))
              (when (null (member sn acc :test #'eq))
                (setq acc (cons sn acc))))))
        (setq cur (cdr cur))))))

(defun %clos-initarg-to-slots (class-name initarg-key)
  "Map INITARG-KEY to the LIST of all slot-names it initializes, walking
   the class's CPL.  CLHS 7.1.4: one initarg may set several slots."
  (let ((acc (%clos-initarg-lookup-all-1 class-name initarg-key nil)))
    (let ((cls (%find-clos-class class-name)))
      (when cls
        (let ((cur (cdr (aref cls 4))))
          (loop
            (when (null cur) (return nil))
            (setq acc (%clos-initarg-lookup-all-1 (car cur) initarg-key acc))
            (setq cur (cdr cur))))))
    (nreverse acc)))

(defun %clos-initform-thunk-1 (class-name slot-name)
  "Look up initform thunk for SLOT-NAME on just CLASS-NAME (no super walk)."
  (let ((info (%clos-slot-info-for class-name)))
    (when (null info) (return-from %clos-initform-thunk-1 nil))
    (let ((ifm (cdr info)))
      (let ((cur ifm))
        (loop
          (when (null cur) (return nil))
          (let ((entry (car cur)))
            (when (eq (car entry) slot-name)
              (return (cdr entry))))
          (setq cur (cdr cur)))))))

(defun %clos-initform-thunk (class-name slot-name)
  "Return the initform thunk for SLOT-NAME in CLASS-NAME, walking the
   CPL — inherited slots get their parent's initform.  Without the
   walk, subclass instances would leave inherited slots unbound."
  (let ((direct (%clos-initform-thunk-1 class-name slot-name)))
    (when direct (return-from %clos-initform-thunk direct)))
  (let ((cls (%find-clos-class class-name)))
    (when (null cls) (return-from %clos-initform-thunk nil))
    (let ((cpl (cdr (aref cls 4))))
      (let ((cur cpl))
        (loop
          (when (null cur) (return nil))
          (let ((found (%clos-initform-thunk-1 (car cur) slot-name)))
            (when found (return found)))
          (setq cur (cdr cur)))))))

;;; ============================================================
;;; Default-initargs registry (per CLHS 7.1.4)
;;; ============================================================
;;; *clos-default-initargs* maps class-name → list of (initarg-key . thunk).
;;; Per CLHS the value form is re-evaluated for each make-instance call —
;;; we store a 0-arity thunk so the eval-each-time semantics is preserved.

(defun %register-clos-default-initargs (class-name initarg-thunks)
  "Register INITARG-THUNKS (list of (initarg-key . thunk) pairs) as the
   :default-initargs of CLASS-NAME.  Always replaces — re-defining a
   class with no default-initargs clears any prior entry."
  (let ((new-reg nil) (cur *clos-default-initargs*))
    (loop
      (when (null cur) (return nil))
      (when (not (eq (car (car cur)) class-name))
        (setq new-reg (cons (car cur) new-reg)))
      (setq cur (cdr cur)))
    (setq *clos-default-initargs*
          (cons (cons class-name initarg-thunks) new-reg)))
  class-name)

(defun %clos-default-initargs-1 (class-name)
  "Return the directly-declared default-initarg thunks for CLASS-NAME
   (no CPL walk).  Returns nil if none registered."
  (let ((cur *clos-default-initargs*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) class-name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun %clos-default-initarg-key-in-p (key lst)
  "True iff KEY (initarg keyword) is already in LST.  Uses hash/name
   comparison so cross-file cl-sym vs native-MVM-sym shapes for the same
   :foo compare correctly (eq alone is fragile across calls)."
  (let ((cur lst))
    (loop
      (when (null cur) (return nil))
      (let ((a (car cur)))
        (when (cond
                ((eq a key) t)
                ((and (%native-mvm-sym-p a) (%native-mvm-sym-p key))
                 (= (%native-mvm-sym-hash a) (%native-mvm-sym-hash key)))
                ((and (%cl-sym-p a) (%cl-sym-p key))
                 (string-equal (%cl-sym-name a) (%cl-sym-name key)))
                (t nil))
          (return t)))
      (setq cur (cdr cur)))))

(defun %clos-default-initargs-for-class (class-name)
  "Return the combined default-initargs alist (initarg-key . thunk) for
   CLASS-NAME walking its CPL.  Per CLHS 7.1.4, more-specific classes
   win — when two classes in the CPL declare the same key, the
   more-specific class's thunk is used.  Returns a fresh list in CPL
   (most-specific first) order."
  (let ((cls (%find-clos-class class-name)))
    (when (null cls) (return-from %clos-default-initargs-for-class nil))
    (let ((seen-keys nil)
          (result nil))
      (let ((cur (aref cls 4)))
        (loop
          (when (null cur) (return nil))
          (let* ((cn (car cur))
                 (entries (%clos-default-initargs-1 cn)))
            (let ((ec entries))
              (loop
                (when (null ec) (return nil))
                (let* ((entry (car ec))
                       (k (car entry)))
                  (unless (%clos-default-initarg-key-in-p k seen-keys)
                    (setq seen-keys (cons k seen-keys))
                    (setq result (cons entry result))))
                (setq ec (cdr ec)))))
          (setq cur (cdr cur))))
      (nreverse result))))

(defun %clos-slot-index (cls slot-name)
  "Return 0-based index of SLOT-NAME in cls, or -1 if not found.
   Uses a NEGATIVE sentinel (was nil) to avoid the AArch64 truthiness
   collision: on AArch64, NIL register x26 = raw 0 and fixnum 0 also
   has bit-pattern 0, so callers doing `(when (null idx) …slot-missing…)`
   wrongly entered the not-found path for slot 0 (the first slot of
   every class) — which then either signaled slot-missing or dereferenced
   NIL.  See reference_aa64_fixnum_zero_nil.md.  Numeric comparisons
   work correctly on fixnum 0 vs -1, so callers now use `(< idx 0)`
   to detect the not-found case."
  (let ((slots (aref cls 2))
        (idx 0))
    (let ((cur slots))
      (loop
        (when (null cur) (return -1))
        ;; Use name/hash-robust equality (not raw EQ): a class defined at
        ;; runtime-EVAL time interns its slot-name symbols as one object,
        ;; while a query symbol arriving from a *different* compilation unit
        ;; (e.g. map-slot-exists-p* walking a quoted list in ansi-bridge, or
        ;; an eval'd test body) can be a distinct native-MVM-sym / CL-sym
        ;; wrapper with the SAME name+hash.  Mirrors the condition-slot path
        ;; and %collect-all-slots, which already key by name/hash.
        (when (%clos-sym-name-eq (car cur) slot-name) (return idx))
        (setq idx (+ idx 1))
        (setq cur (cdr cur))))))

;;; make-instance: user-facing function (build-time rewriter expands
;;; the common (make-instance 'class :k v ...) shape inline; this defun
;;; handles the rest — class-object args, missing/extra args, runtime
;;; (eval `(make-instance ...)) callers).
(defun %clos-initarg-key-valid-p (class-name key)
  "True if KEY is a declared :initarg for some slot of CLASS-NAME (walking
   the CPL), or one of the always-accepted standard initargs.  Used to
   reject unknown initargs in MAKE-INSTANCE per CLHS 7.1.2."
  (cond
    ;; :allow-other-keys is always accepted (CLHS 7.1.2).
    ((%clos-sym-name-eq key :allow-other-keys) t)
    ;; Maps to at least one slot anywhere in the CPL → valid.
    ((%clos-initarg-to-slot class-name key) t)
    (t nil)))

(defun %clos-validate-initargs-d (class-or-name initargs)
  "Like %clos-validate-initargs but accepts a class designator (name
   symbol OR class object).  Used by the compiled make-instance
   expansion, which may pass a (find-class …) result.  Resolves the
   class-name, then validates only when the class is registered."
  (let ((class-name (if (%clos-class-p class-or-name)
                        (aref class-or-name 1)
                        class-or-name)))
    (when (%find-clos-class class-name)
      (%clos-validate-initargs class-name initargs))))

(defun %clos-validate-initargs (class-name initargs)
  "Validate INITARGS (a plist) for MAKE-INSTANCE on CLASS-NAME.  Signals
   PROGRAM-ERROR for a malformed (odd-length) initarg list, and ERROR for
   an unrecognised initarg keyword unless :allow-other-keys is non-NIL.
   Per CLHS 7.1.2 / 7.1.1."
  ;; 1. The initarg list must be a proper even-length plist.  CLHS 7.1.2:
  ;; a malformed / odd-length initarg list is a PROGRAM-ERROR
  ;; (make-instance.error.2), not a plain simple-error.
  (let ((cur initargs) (count 0))
    (loop
      (when (null cur) (return nil))
      (when (not (consp cur))
        (%signal-program-error))
      (setq count (+ count 1))
      (setq cur (cdr cur)))
    (when (not (= 0 (mod count 2)))
      (%signal-program-error)))
  ;; 2. Determine whether :allow-other-keys is non-NIL.  CLHS 7.1.2: a
  ;; supplied :allow-other-keys in the CALL takes precedence; if absent
  ;; from the call, a :default-initargs :allow-other-keys on the class
  ;; (or an ancestor) supplies it (class-24.2: :nonsense is allowed
  ;; because the default-initarg sets :allow-other-keys t).  An explicit
  ;; :allow-other-keys nil in the call overrides the default (class-24.3).
  (let ((aok nil)
        (aok-supplied nil)
        (cur initargs))
    (loop
      (when (null cur) (return nil))
      (when (and (consp (cdr cur))
                 (%clos-sym-name-eq (car cur) :allow-other-keys))
        (setq aok-supplied t)
        (when (cadr cur) (setq aok t)))
      (setq cur (cddr cur)))
    ;; Not in the call?  Consult the class's combined default-initargs.
    (when (not aok-supplied)
      (let ((dis (%clos-default-initargs-for-class class-name)))
        (let ((dc dis))
          (loop
            (when (null dc) (return nil))
            (let ((entry (car dc)))
              (when (%clos-sym-name-eq (car entry) :allow-other-keys)
                ;; entry = (key . thunk); evaluate the thunk for its value.
                (let ((v (funcall (cdr entry))))
                  (when v (setq aok t)))
                (return nil)))
            (setq dc (cdr dc))))))
    ;; 3. Unless :allow-other-keys, every supplied initarg must be valid.
    (unless aok
      (let ((c2 initargs))
        (loop
          (when (null c2) (return nil))
          (let ((key (car c2)))
            (unless (%clos-initarg-key-valid-p class-name key)
              (error "make-instance: invalid initarg ~S" key)))
          (setq c2 (cddr c2)))))))

(defun make-instance (&rest args)
  "Per CLHS 7.1.1, MAKE-INSTANCE allocates an instance and dispatches
   INITIALIZE-INSTANCE on it with the initargs.  We bypass apply on
   initialize-instance — that goes through funcall+&rest, which in
   modus loses trailing args when nargs > +max-reg-args+.  Instead we
   call the dispatcher directly with the args list."
  (cond
    ((null args) (error "make-instance: requires a class designator"))
    (t
     (let* ((class-or-name (car args))
            (initargs (cdr args))
            (class-name (if (%clos-class-p class-or-name)
                            (aref class-or-name 1)
                            class-or-name))
            (inst (%make-instance class-or-name))
            ;; make-instance applies default-initargs (CLHS 7.1.4); bind
            ;; the flag so the shared-initialize default body picks them up.
            (*clos-applying-defaults* t))
       (when (null inst) (return-from make-instance nil))
       ;; CLHS 7.1.2: reject malformed / unknown initargs.  Only validate
       ;; for genuine CLOS classes (registered); skip when inst allocation
       ;; succeeded but the class has no slot-info (defensive).
       (when (%find-clos-class class-name)
         (%clos-validate-initargs class-name initargs))
       (%dispatch-initialize-instance (cons inst initargs))
       inst))))

(defun %make-instance (class-or-name)
  "Allocate a new CLOS instance.  Accepts either a class name (symbol)
   or a class-object (returned by find-class).  Initargs are handled
   at build time by the SBCL-side rewriter for the (make-instance
   'symbol :k v ...) shape; the class-object shape is for runtime
   (eval `(make-instance (find-class 'X))) and similar."
  (let* ((cls (if (%clos-class-p class-or-name)
                  class-or-name
                  (%find-clos-class class-or-name)))
         (class-name (if (%clos-class-p class-or-name)
                         (aref class-or-name 1)
                         class-or-name)))
    (when (null cls) (return-from %make-instance nil))
    (let* ((slot-names (aref cls 2))
           (n (length slot-names))
           (inst (make-array (+ 2 n))))
      (aset inst 0 '%clos-instance)
      (aset inst 1 class-name)
      (let ((i 0))
        (loop
          (when (= i n) (return nil))
          ;; Use literal -999 (unbound sentinel) to avoid SYMBOL-VALUE clobbering
          ;; arr-reg (V0) in variable-index aset compilation
          (aset inst (+ 2 i) -999)
          (setq i (+ i 1))))
      inst)))

(defun %slot-value (obj slot-name)
  "Read slot SLOT-NAME from CLOS instance OBJ.
   Calls slot-unbound if the slot has no value, slot-missing when
   SLOT-NAME doesn't name a slot of OBJ's class.

   :allocation :class slots route to per-class shared storage via
   %slot-class-owner / %class-slot-get."
  (when (or (null obj) (not (%clos-instance-p obj)))
    (return-from %slot-value nil))
  (let* ((class-name (aref obj 1))
         (cls (%find-clos-class class-name)))
    (when (null cls) (return-from %slot-value nil))
    ;; Class-allocated slot?  Walk CPL via %slot-class-owner.
    (when (%slot-class-owner class-name slot-name)
      (if (%class-slot-bound-p class-name slot-name)
          (return-from %slot-value
            (%class-slot-get class-name slot-name nil))
          (return-from %slot-value
            (%dispatch-slot-unbound cls obj slot-name))))
    (let ((idx (%clos-slot-index cls slot-name)))
      ;; AArch64 fixnum-0 / NIL bit-pattern collision (see
      ;; reference_aa64_fixnum_zero_nil.md): `(null 0)` returns T on
      ;; AArch64, so for slot 0 (first slot of every class) `(null idx)`
      ;; was wrongly triggering slot-missing and crashing the fork.
      ;; Use (not (integerp idx)) — the only non-integer return from
      ;; %clos-slot-index is NIL (not found).
      (when (< idx 0)
        (return-from %slot-value
          (%dispatch-slot-missing cls obj slot-name 'slot-value nil nil)))
      (let ((val (aref obj (+ 2 idx))))
        ;; -999 is the unbound slot sentinel (fixnum, no global lookup needed)
        ;; Guard with fixnump to avoid type error when slot contains non-fixnum
        (if (and (fixnump val) (= val -999))
          ;; Call slot-unbound method
          (%dispatch-slot-unbound cls obj slot-name)
          val)))))

(defun set-slot-value (obj slot-name new-val)
  "Set slot SLOT-NAME in CLOS instance OBJ to NEW-VAL. Returns NEW-VAL.
   Calls slot-missing when SLOT-NAME doesn't name a slot.

   :allocation :class slots route to per-class shared storage."
  (when (or (null obj) (not (%clos-instance-p obj)))
    (return-from set-slot-value new-val))
  (let* ((class-name (aref obj 1))
         (cls (%find-clos-class class-name)))
    (when (null cls) (return-from set-slot-value new-val))
    ;; Class-allocated slot?
    (when (%slot-class-owner class-name slot-name)
      (%class-slot-set class-name slot-name new-val)
      (return-from set-slot-value new-val))
    (let ((idx (%clos-slot-index cls slot-name)))
      ;; See %slot-value for the AArch64 fixnum-0/NIL rationale.
      (when (< idx 0)
        (%dispatch-slot-missing cls obj slot-name 'setf new-val t)
        (return-from set-slot-value new-val))
      (aset obj (+ 2 idx) new-val)
      new-val)))

(defun %slot-boundp (obj slot-name)
  "True if slot SLOT-NAME of OBJ is bound.  Calls slot-missing for
   nonexistent slots; whatever it returns is taken as the boundp answer."
  (let* ((class-name (aref obj 1))
         (cls (%find-clos-class class-name)))
    (when (null cls) (return-from %slot-boundp nil))
    ;; Class-allocated slot?
    (when (%slot-class-owner class-name slot-name)
      (return-from %slot-boundp (%class-slot-bound-p class-name slot-name)))
    (let ((idx (%clos-slot-index cls slot-name)))
      ;; See %slot-value for the AArch64 fixnum-0/NIL rationale.
      (when (< idx 0)
        (return-from %slot-boundp
          (%dispatch-slot-missing cls obj slot-name 'slot-boundp nil nil)))
      ;; -999 is the unbound slot sentinel (fixnum guard prevents type error)
      (let ((v (aref obj (+ 2 idx))))
        (not (and (fixnump v) (= v -999)))))))

(defun %slot-makunbound (obj slot-name)
  "Mark slot SLOT-NAME in OBJ as unbound.  Calls slot-missing for
   nonexistent slots; either way returns OBJ.  Routes :allocation :class
   slots to per-class storage so the shared slot is actually cleared
   (otherwise slot-value would still return the class-bound value via
   %class-slot-get — defeating class-0203/0207-style makunbound semantics)."
  (let* ((class-name (aref obj 1))
         (cls (%find-clos-class class-name)))
    (when (null cls) (return-from %slot-makunbound obj))
    ;; Class-allocated slot? Remove the per-class storage entry.
    (when (%slot-class-owner class-name slot-name)
      (%class-slot-makunbound class-name slot-name)
      (return-from %slot-makunbound obj))
    (let ((idx (%clos-slot-index cls slot-name)))
      ;; See %slot-value for the AArch64 fixnum-0/NIL rationale.
      (when (< idx 0)
        (%dispatch-slot-missing cls obj slot-name 'slot-makunbound nil nil)
        (return-from %slot-makunbound obj))
      ;; Use literal -999 to avoid SYMBOL-VALUE clobber in variable-index aset
      (aset obj (+ 2 idx) -999)
      obj)))

(defun %class-slot-makunbound (class-name slot-name)
  "Remove the per-class storage entry for SLOT-NAME on CLASS-NAME (resolves
   via the owning class in the CPL).  After this %class-slot-bound-p
   returns NIL so slot-value will signal unbound-slot."
  (let ((owner (%slot-class-owner class-name slot-name)))
    (when owner
      (let ((new-list nil) (cur *clos-class-slot-values*))
        (loop
          (when (null cur) (return nil))
          (let ((entry (car cur)))
            (unless (and (eq (car (car entry)) owner)
                         (eq (cdr (car entry)) slot-name))
              (setq new-list (cons entry new-list))))
          (setq cur (cdr cur)))
        (setq *clos-class-slot-values* new-list)))))

(defun %cond-type-name-of (obj)
  "If OBJ is structurally a condition instance (a 2-element #x32 array whose
   slot-0 is a non-NIL symbol that names a registered condition type), return
   that registry type-name; else NIL.  Matches the type-name by NAME/HASH
   (not raw EQ) so a condition built via (make-condition 'X) in an eval'd test
   body still resolves to the registry entry that an init-form's
   define-condition created under a DIFFERENT native-MVM-sym / CL-sym object.
   %condition-p / %cond-reg-find use raw EQ assoc and miss that drift, which
   poisoned slot-exists-p.15/.16 (all-NIL)."
  (when (or (fixnump obj) (consp obj) (null obj)) (return-from %cond-type-name-of nil))
  (when (not (= (obj-subtag obj) #x32)) (return-from %cond-type-name-of nil))
  (when (not (= (array-length obj) 2)) (return-from %cond-type-name-of nil))
  (let ((tn (aref obj 0)))
    (when (or (null tn) (not (symbolp tn))) (return-from %cond-type-name-of nil))
    (let ((cur *condition-type-registry*))
      (loop
        (when (null cur) (return-from %cond-type-name-of nil))
        (let ((reg-name (car (car cur))))
          (when (%clos-sym-name-eq reg-name tn)
            (return-from %cond-type-name-of reg-name)))
        (setq cur (cdr cur))))))

(defun %slot-exists-p (obj slot-name)
  "True if OBJ has a slot named SLOT-NAME.  Per CLHS, SLOT-EXISTS-P may
   be called on any object, not just standard objects — in particular it
   must work on condition objects (slot-exists-p.15/.16)."
  (when (null obj) (return-from %slot-exists-p nil))
  ;; Condition objects: query the condition-type slot registry.  Conditions
  ;; are #x32 arrays too, so check this before the CLOS-instance path.  Use a
  ;; NAME/HASH-robust registry resolve (%cond-type-name-of) rather than
  ;; %condition-p, which would miss a drifted type-name symbol and report
  ;; every slot as absent.
  (let ((cond-tn (%cond-type-name-of obj)))
    (when cond-tn
      (let ((cur (%collect-all-slots cond-tn)))
        (loop
          (when (null cur) (return-from %slot-exists-p nil))
          (when (%clos-sym-name-eq (car (car cur)) slot-name)
            (return-from %slot-exists-p t))
          (setq cur (cdr cur))))))
  ;; Struct objects: SLOT-EXISTS-P must work on structure instances too
  ;; (slot-exists-p.11-.14).  Structs are #x32 arrays with slot-0 =
  ;; '%struct-instance; query the struct-type effective slot list by hash.
  (when (%struct-instance-p obj)
    (return-from %slot-exists-p
      (>= (%struct-slot-index (%struct-type-name obj) slot-name) 0)))
  (when (not (%clos-instance-p obj))
    (return-from %slot-exists-p nil))
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-exists-p nil))
    (let ((idx (%clos-slot-index cls slot-name)))
      ;; See %slot-value for the AArch64 fixnum-0/NIL rationale —
      ;; (if 0 t nil) returns nil on AArch64 (fixnum 0 == NIL bits).
      (if (< idx 0) nil t))))

;; slot-missing dispatch
;; Methods stored as: (class-name . fn).  Class-name = T matches all
;; classes; otherwise eq match on instance class.  fn signature is
;; (class instance slot-name operation new-value-list) where
;; new-value-list is nil for slot-value/slot-boundp/slot-makunbound
;; or (NEW) for setf — letting slot-missing distinguish via &rest.

(defvar *slot-missing-methods* nil)

(defun %add-slot-missing-method (class-name fn &optional (slot-eql :any) (op-eql :any))
  "Register a slot-missing method.  SLOT-EQL / OP-EQL are :any (no
   constraint) or a 1-element list (VALUE) giving an eql-specializer on
   the slot-name / operation argument.  Entry shape:
   (class-name slot-eql op-eql . fn)."
  (setq *slot-missing-methods*
        (cons (cons class-name (cons slot-eql (cons op-eql fn)))
              *slot-missing-methods*)))

(defun %clos-sym-name-eq (a b)
  "True if class-name symbols A and B denote the same name, robust to
   native-MVM-sym vs CL-sym identity drift across init-form / test
   boundaries (same name/hash, different object).  NOT a substitute for
   EQ on genuinely distinct symbols — callers eq-check first."
  (cond
    ((eq a b) t)
    ((and (%native-mvm-sym-p a) (%native-mvm-sym-p b))
     (= (%native-mvm-sym-hash a) (%native-mvm-sym-hash b)))
    ((and (%cl-sym-p a) (%cl-sym-p b))
     (string-equal (%cl-sym-name a) (%cl-sym-name b)))
    (t nil)))

(defun %dispatch-slot-missing (cls obj slot-name op new-val new-val-p)
  "Call the most-specific slot-missing method for OBJ's class.  When
   no user method is registered, fall through to the documented default
   of erroring on slot-value / setf and returning NIL on the others."
  (let ((class-name (aref cls 1))
        (best-fn nil)         ; most-specific applicable, eql-specialized
        (best-any-fn nil))    ; class-only fallback (slot-eql/op-eql = :any)
    (let ((cur *slot-missing-methods*))
      (loop
        (when (null cur) (return nil))
        (let ((m (car cur)))
          (let ((m-class (car m))
                (m-slot-eql (cadr m))
                (m-op-eql   (caddr m))
                (m-fn       (cdddr m)))
            ;; Robust class match: a method registered from a different
            ;; top-level init-form may hold a different symbol OBJECT for the
            ;; same class name (native-MVM-sym vs CL-sym drift across the
            ;; defmethod-rewrite / %defclass boundary).  Compare by name/hash
            ;; as well as eq so the user slot-missing method actually fires.
            (when (or (eq m-class t)
                      (eq m-class class-name)
                      (%clos-sym-name-eq m-class class-name))
              ;; Honor eql-specializers on slot-name / operation.  :any =
              ;; no constraint; (VALUE) = must eql VALUE.
              (let ((slot-ok (or (eq m-slot-eql :any)
                                 (eql (car m-slot-eql) slot-name)))
                    (op-ok   (or (eq m-op-eql :any)
                                 (eql (car m-op-eql) op))))
                (when (and slot-ok op-ok)
                  (if (and (eq m-slot-eql :any) (eq m-op-eql :any))
                      ;; class-only method — record as fallback only
                      (when (null best-any-fn) (setq best-any-fn m-fn))
                      ;; eql-specialized and applicable — most specific
                      (when (null best-fn) (setq best-fn m-fn))))))))
        (setq cur (cdr cur))))
    (let ((fn (or best-fn best-any-fn)))
      (if fn
        ;; The rewriter emits a fixed 6-param method (… new-value supplied-p)
        ;; so we always pass both — MVM's &optional default for a missing
        ;; arg reads stack garbage, so we never rely on it.
        (funcall fn cls obj slot-name op new-val (and new-val-p t))
        ;; Default slot-missing errors regardless of op (per CLHS).
        (error "no slot named ~S in class ~S" slot-name class-name)))))

;; slot-unbound dispatch
;; Methods stored as: (class-name slot-spec fn)
;; slot-spec: nil = any, or a symbol to match specific slot

(defun %add-slot-unbound-method (class-name slot-spec fn)
  "Register a slot-unbound method."
  (setq *slot-unbound-methods*
        (cons (cons class-name (cons slot-spec fn)) *slot-unbound-methods*)))

(defun %dispatch-slot-unbound (cls obj slot-name)
  "Find and call the most specific slot-unbound method."
  (let ((class-name (aref cls 1))
        (best-fn nil)
        (best-specific nil))
    ;; Search methods (most recently added = most specific wins for eql specializer)
    (let ((cur *slot-unbound-methods*))
      (loop
        (when (null cur) (return nil))
        (let ((m (car cur)))
          (let ((m-class (car m))
                (m-slot  (cadr m))
                (m-fn    (cddr m)))
            ;; Check class match: t matches any, or eq check
            (when (or (eq m-class t) (eq m-class class-name))
              ;; Check slot specializer
              (cond
                ;; Specific slot match: overrides general
                ((and (not (null m-slot)) (eq m-slot slot-name))
                 (when (null best-specific)
                   (setq best-specific m-fn)))
                ;; General (t) match: only use if no specific found yet
                ((null m-slot)
                 (when (null best-fn)
                   (setq best-fn m-fn)))))))
        (setq cur (cdr cur))))
    ;; Call best match: specific > general > default error
    (let ((fn (if best-specific best-specific best-fn)))
      (if fn
        (funcall fn cls obj slot-name)
        ;; Default: signal unbound-slot condition
        (error 'unbound-slot :name slot-name :instance obj)))))

(defun slot-unbound (class obj slot-name)
  "Default slot-unbound: signals an unbound-slot condition with
   :instance OBJ and :name SLOT-NAME so handler-case can pull them
   back out via unbound-slot-instance / cell-error-name."
  (declare (ignore class))
  (error 'unbound-slot :name slot-name :instance obj))

(defun class-name (cls)
  "Return the name of class CLS.

   CLASS-NAME is a standard generic function (CLHS), so a user may add
   methods to it — e.g. (defmethod class-name ((x my-class)) 'silly).
   When such a user method is applicable to CLS, dispatch to it; this
   covers class-name.2 where the arg is an INSTANCE, not a class object.
   The builtin behaviour (read slot-1 of a class / class-proxy) is the
   fallback when no user method applies, which is the common internal
   path and avoids dispatch overhead on every class-object call."
  (let ((gf (%find-gf 'class-name)))
    (when (and gf (%gf-p gf) (%gf-methods gf))
      (let ((applicable (%collect-applicable-methods gf (list cls))))
        (when applicable
          (return-from class-name
            (%gf-dispatch-standard gf (list cls) applicable))))))
  (if (%clos-class-p cls)
    (aref cls 1)
    (if (%class-proxy-p cls)
      (%class-proxy-name cls)
      nil)))

(defun class-of (x)
  "Return the class of X.  Per ANSI 4.3.1, every object has a class.
   For CLOS instances we look up the registered class; for everything
   else we hand back a class-proxy keyed on the built-in type so
   things like (class-of 5) → integer-class behave sensibly with
   (typep ... 'class) and (class-name ...)."
  (cond
    ((%clos-instance-p x)
     (%find-clos-class (aref x 1)))
    ((%clos-class-p x)
     ;; A class object is itself an instance of standard-class.
     (or (%find-clos-class 'standard-class)
         (let ((proxy (make-array 2)))
           (aset proxy 0 '%class-proxy)
           (aset proxy 1 'standard-class)
           proxy)))
    (t
     ;; Any other value — synthesize a class-proxy for its type-of name
     ;; so callers get an object that responds to class-name and typep.
     (let ((type-name (%type-of-for-dispatch x)))
       (or (%find-clos-class type-name)
           (let ((proxy (make-array 2)))
             (aset proxy 0 '%class-proxy)
             (aset proxy 1 type-name)
             proxy))))))

;;; ============================================================
;;; MOP accessors — class introspection
;;; ============================================================

(defun class-precedence-list (cls)
  "Return the CPL of CLS as a list of class names (most-specific first,
   ending in T).  Per MOP / AMOP §5.5.1 this should be a list of class
   objects, but our CPL is name-keyed; tests only check the structure."
  (cond
    ((%clos-class-p cls) (aref cls 4))
    ((symbolp cls)
     (let ((c (%find-clos-class cls)))
       (if c (aref c 4) (%builtin-cpl cls))))
    (t nil)))

(defun class-direct-superclasses (cls)
  "Return the direct superclasses of CLS as a list of class names."
  (cond
    ((%clos-class-p cls) (aref cls 3))
    ((symbolp cls)
     (let ((c (%find-clos-class cls)))
       (if c (aref c 3) nil)))
    (t nil)))

(defun class-direct-subclasses (cls)
  "Return the direct subclasses of CLS — classes that name CLS in
   their direct supers."
  (let* ((name (cond ((%clos-class-p cls) (aref cls 1))
                     ((symbolp cls) cls)
                     (t nil)))
         (acc nil))
    (when name
      (dolist (entry *clos-classes*)
        (let ((c (cdr entry)))
          (when (member name (aref c 3) :test #'eq)
            (setq acc (cons (car entry) acc))))))
    acc))

(defun class-direct-slots (cls)
  "Return the direct slots declared on CLS — for modus this means the
   effective-slot list (we don't track 'direct' vs 'inherited' separately)."
  (cond
    ((%clos-class-p cls) (aref cls 2))
    ((symbolp cls)
     (let ((c (%find-clos-class cls)))
       (if c (aref c 2) nil)))
    (t nil)))

(defun class-slots (cls)
  "Return effective slots — same as class-direct-slots in modus."
  (class-direct-slots cls))

(defun compute-class-precedence-list (cls)
  "MOP function: recompute the CPL.  We just return the stored CPL."
  (class-precedence-list cls))

(defun class-default-initargs (cls)
  "ANSI: list of default initargs for CLS.  We don't currently track
   per-class default initargs, so the list is always NIL."
  (declare (ignore cls))
  nil)

(defun class-direct-default-initargs (cls)
  "ANSI: list of directly-declared default initargs.  Same NIL story."
  (declare (ignore cls))
  nil)

(defun class-finalized-p (cls)
  "MOP: classes are eagerly finalized in modus, so always T."
  (declare (ignore cls))
  t)

(defun finalize-inheritance (cls)
  "MOP no-op — modus finalizes inheritance at %defclass time."
  cls)

(defun standard-instance-access (instance location)
  "MOP raw slot access by LOCATION (integer slot index).  Returns
   the unbound sentinel if the slot is unbound."
  (let ((idx (+ 2 location)))
    (when (and (%clos-instance-p instance)
               (< idx (array-length instance)))
      (aref instance idx))))

(defun (setf standard-instance-access) (new-val instance location)
  (let ((idx (+ 2 location)))
    (when (and (%clos-instance-p instance)
               (< idx (array-length instance)))
      (aset instance idx new-val)
      new-val)))

(defun ensure-class (name &rest options)
  "Lightweight ensure-class — looks up the class by NAME and returns it,
   or creates a new one with the supplied :direct-superclasses /
   :direct-slot-names options.  Full MOP semantics aren't implemented."
  (let ((existing (%find-clos-class name)))
    (cond
      (existing existing)
      (t
       (let ((supers (or (cadr (member :direct-superclasses options :test #'eq))
                         '(standard-object)))
             (slots (cadr (member :direct-slot-names options :test #'eq))))
         (%defclass name slots supers)
         (%find-clos-class name))))))

(defun slot-value (obj slot-name)
  "Read slot SLOT-NAME from CLOS instance OBJ."
  (%slot-value obj slot-name))

(defun slot-boundp (obj slot-name)
  "True if slot SLOT-NAME of OBJ is bound."
  (%slot-boundp obj slot-name))

(defun slot-exists-p (obj slot-name)
  "True if OBJ has a slot named SLOT-NAME."
  (%slot-exists-p obj slot-name))

(defun slot-makunbound (obj slot-name)
  "Unset slot SLOT-NAME in OBJ."
  (%slot-makunbound obj slot-name))

;;; ============================================================
;;; Generic Function System
;;; ============================================================

;; GF storage: vector #(%generic-function name methods-alist combination)
;;   methods-alist: list of method-records
;;   method-record: list (qualifier specializer-list fn)
;;     qualifier: nil = primary, :before, :after, :around, or custom symbol
;;     specializer-list: list of class-names (or (eql val) forms)
;;   combination: nil = standard, or method-combination name symbol

(defun %make-gf (name)
  "Create a new generic function object.
   5-slot vector: #(%generic-function name methods combination lambda-list).
   The lambda-list slot stores the GF's full lambda-list (set by
   %defgeneric) so %defmethod / find-method can validate method
   congruence (CLHS 7.6.4).  NIL means unknown — auto-created GFs
   from a leading %defmethod skip the check."
  (let ((gf (make-array 8)))
    (aset gf 0 '%generic-function)
    (aset gf 1 name)
    (aset gf 2 nil)  ; methods-alist
    (aset gf 3 nil)  ; method-combination name
    (aset gf 4 nil)  ; lambda-list (NIL = unknown)
    (aset gf 5 nil)  ; methods added by a DEFGENERIC form (CLHS removal)
    (aset gf 6 nil)  ; :argument-precedence-order as arg-index list
    (aset gf 7 nil)  ; method-meta alist: (method . (key rest aok keynames))
    gf))

(defun %gf-p (x)
  "True if X is a generic function object."
  (if (or (fixnump x) (consp x) (null x)) nil
    (if (= (obj-subtag x) #x32)
      (if (>= (array-length x) 1)
        (eq (aref x 0) '%generic-function)
        nil)
      nil)))

(defun %gf-name (gf)     (aref gf 1))
(defun %gf-methods (gf)  (aref gf 2))
(defun %gf-combination (gf) (aref gf 3))
;; %gf-lambda-list: safe-read — old 4-slot GFs (deserialized images) may
;; lack slot 4.  Guard via array-length.
(defun %gf-lambda-list (gf)
  (if (>= (array-length gf) 5) (aref gf 4) nil))
(defun %gf-set-methods (gf m) (aset gf 2 m))
(defun %gf-set-combination (gf c) (aset gf 3 c))
(defun %gf-set-lambda-list (gf ll)
  (when (>= (array-length gf) 5) (aset gf 4 ll)))

;; Slot 5: methods added by a DEFGENERIC form for NAME.  CLHS DEFGENERIC:
;; redefining a GF removes the methods the PREVIOUS defgeneric form added
;; (but keeps DEFMETHOD-added ones).  Guarded reads for old 5-slot GFs.
(defun %gf-defgeneric-methods (gf)
  (if (>= (array-length gf) 6) (aref gf 5) nil))
(defun %gf-set-defgeneric-methods (gf ms)
  (when (>= (array-length gf) 6) (aset gf 5 ms)))

;; Slot 6: :argument-precedence-order, stored as a list of required-arg
;; INDICES (e.g. (1 0) for (:argument-precedence-order y x) on (x y)).
(defun %gf-apo (gf)
  (if (>= (array-length gf) 7) (aref gf 6) nil))
(defun %gf-set-apo (gf v)
  (when (>= (array-length gf) 7) (aset gf 6 v)))

;; Slot 7: method-meta alist mapping method-record → (has-key has-rest
;; aok key-name-strings), harvested from the method's full lambda-list.
;; Used by %gf-check-keys for CLHS 7.6.5 keyword validity checking.
(defun %gf-method-meta (gf)
  (if (>= (array-length gf) 8) (aref gf 7) nil))
(defun %gf-set-method-meta (gf v)
  (when (>= (array-length gf) 8) (aset gf 7 v)))
(defun %gf-method-meta-for (gf m)
  "Meta entry for method record M on GF, or NIL."
  (let ((cur (%gf-method-meta gf)))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) m) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun %lambda-list-required-count (ll)
  "Count required parameters in lambda-list LL — items before any
   &optional/&rest/&key/&aux/&allow-other-keys keyword."
  (let ((n 0) (cur ll))
    (loop
      (when (null cur) (return n))
      (let ((p (car cur)))
        (when (and (symbolp p)
                   (let ((nm (symbol-name p)))
                     (or (string= nm "&OPTIONAL")
                         (string= nm "&REST")
                         (string= nm "&KEY")
                         (string= nm "&AUX")
                         (string= nm "&ALLOW-OTHER-KEYS")
                         (string= nm "&BODY"))))
          (return n)))
      (setq n (+ n 1))
      (setq cur (cdr cur)))))

(defun %lambda-list-has-rest-or-key (ll)
  "True if LL has &REST, &KEY, or &OPTIONAL — i.e., accepts a
   variable / open-ended number of args."
  (let ((cur ll))
    (loop
      (when (null cur) (return nil))
      (let ((p (car cur)))
        (when (and (symbolp p)
                   (let ((nm (symbol-name p)))
                     (or (string= nm "&OPTIONAL")
                         (string= nm "&REST")
                         (string= nm "&KEY")
                         (string= nm "&BODY"))))
          (return t)))
      (setq cur (cdr cur)))))

(defun %lambda-list-keyword-p (sym name)
  "True iff SYM is a symbol whose name is NAME (case-sensitive)."
  (and (symbolp sym) (string= (symbol-name sym) name)))

(defun %lambda-list-shape (ll)
  "Return (req-count optional-count has-rest has-key has-allow-other-keys).
   Used for CLHS 7.6.4 method-vs-GF congruence checks."
  (let ((mode :required)
        (req 0) (opt 0)
        (has-rest nil) (has-key nil) (allow-other nil)
        (cur ll))
    (loop
      (when (null cur)
        (return (list req opt has-rest has-key allow-other)))
      (let ((p (car cur)))
        (cond
          ((%lambda-list-keyword-p p "&OPTIONAL") (setq mode :optional))
          ((%lambda-list-keyword-p p "&REST") (setq mode :rest) (setq has-rest t))
          ((%lambda-list-keyword-p p "&BODY") (setq mode :rest) (setq has-rest t))
          ((%lambda-list-keyword-p p "&KEY") (setq mode :key) (setq has-key t))
          ((%lambda-list-keyword-p p "&AUX") (setq mode :aux))
          ((%lambda-list-keyword-p p "&ALLOW-OTHER-KEYS") (setq allow-other t))
          (t
           (cond ((eq mode :required) (setq req (+ req 1)))
                 ((eq mode :optional) (setq opt (+ opt 1)))))))
      (setq cur (cdr cur)))))

(defun %method-ll-congruent-p (gf-shape spec-count method-ll-shape)
  "CLHS 7.6.4 congruence rules.
   GF-SHAPE = (req opt rest key aok) from %lambda-list-shape.
   SPEC-COUNT = method's specializer count (must equal GF req).
   METHOD-LL-SHAPE = (req opt rest key aok) from method's full LL.

   Rules:
   1. Specializer count = GF required count.
   2. Method optional count = GF optional count.
   3. If GF has &rest or &key, method must have &rest or &key.
   4. If GF has neither &rest nor &key, method must have neither."
  (let ((g-req  (nth 0 gf-shape))
        (g-opt  (nth 1 gf-shape))
        (g-rest (nth 2 gf-shape))
        (g-key  (nth 3 gf-shape))
        (m-req  (nth 0 method-ll-shape))
        (m-opt  (nth 1 method-ll-shape))
        (m-rest (nth 2 method-ll-shape))
        (m-key  (nth 3 method-ll-shape)))
    (declare (ignore m-req))   ; spec-count carries the required count
    (and (= spec-count g-req)
         (= m-opt g-opt)
         (eq (and (or g-rest g-key) t)
             (and (or m-rest m-key) t)))))

(defun %method-accepts-gf-keys-p (gf-ll method-ll)
  "CLHS 7.6.4: if the GF lambda-list names keyword parameters after &KEY,
   each must be accepted by the method — explicitly in the method's &KEY
   list, or via &ALLOW-OTHER-KEYS, or via &REST-without-&KEY.  Returns
   NIL when some GF key is unaccepted (defmethod.error.10)."
  (let ((gf-shape (%lambda-list-shape gf-ll))
        (m-shape  (%lambda-list-shape method-ll)))
    ;; GF declares no &key names to enforce → trivially OK.
    (if (not (nth 3 gf-shape))
        t
        (cond
          ;; Method has &allow-other-keys → accepts anything.
          ((nth 4 m-shape) t)
          ;; Method has &rest but not &key → accepts any keyword.
          ((and (nth 2 m-shape) (not (nth 3 m-shape))) t)
          ;; Method has no &key (and no &rest) → accepts nothing extra:
          ;; OK only if the GF named no keys (handled above).
          ((not (nth 3 m-shape)) nil)
          ;; Both have &key: every GF key name must be in the method's.
          (t
           (let ((gf-keys (%lambda-list-key-names gf-ll))
                 (m-keys  (%lambda-list-key-names method-ll))
                 (ok t))
             (let ((cur gf-keys))
               (loop
                 (when (null cur) (return nil))
                 (unless (%dg-string-member (car cur) m-keys)
                   (setq ok nil) (return nil))
                 (setq cur (cdr cur))))
             ok))))))

;;; ============================================================
;;; DEFGENERIC option validation (CLHS macro DEFGENERIC + 7.6.4)
;;; ============================================================

(defun %dg-param-name (p)
  "Name string of a lambda-list element.  Handles native MVM symbols,
   CL-symbol wrappers (which are conses), and the (var ...) /
   ((:key var) ...) list shapes — recursing on the head."
  (cond
    ((null p) "")
    ((symbolp p) (symbol-name p))
    ((consp p) (%dg-param-name (car p)))
    (t "")))

(defun %dg-amp-p (p)
  "True iff P is a lambda-list keyword (&optional / &rest / &key /...)."
  (and (symbolp p)
       (let ((n (symbol-name p)))
         (and (stringp n) (> (length n) 0) (char= (char n 0) #\&)))))

(defun %dg-string-member (s lst)
  "True iff string S is STRING= to some element of LST (strings)."
  (let ((cur lst))
    (loop
      (when (null cur) (return nil))
      (when (string= s (car cur)) (return t))
      (setq cur (cdr cur)))))

(defun %lambda-list-key-names (ll)
  "Names (strings) of the keyword parameters declared after &KEY in LL.
   ((:bar foo) ...) shapes yield the exposed key name (BAR)."
  (let ((mode nil) (names nil) (cur ll))
    (loop
      (when (null cur) (return (nreverse names)))
      (let ((p (car cur)))
        (cond
          ((%lambda-list-keyword-p p "&KEY") (setq mode t))
          ((%dg-amp-p p)
           (unless (%lambda-list-keyword-p p "&ALLOW-OTHER-KEYS")
             (setq mode nil)))
          (mode (setq names (cons (%dg-param-name p) names)))))
      (setq cur (cdr cur)))))

(defun %dg-method-ll (mopt)
  "Specialized lambda-list of a (:method ...) defgeneric option — the
   first list element after the head, skipping qualifier symbols.
   CL-symbol-wrapper qualifiers (conses) are NOT mistaken for the ll."
  (let ((cur (cdr mopt)))
    (loop
      (when (null cur) (return nil))
      (let ((x (car cur)))
        (when (or (null x) (and (consp x) (not (%cl-sym-p x))))
          (return x)))
      (setq cur (cdr cur)))))

(defun %dg-congruent-p (gf-ll m-ll)
  "CLHS 7.6.4 congruence between a GF lambda-list and a method's
   specialized lambda-list: same required count, same optional count,
   matching &rest/&key acceptance, and the method accepts every keyword
   name the GF declares (explicitly, via &allow-other-keys, or via
   &rest without &key)."
  (let ((g (%lambda-list-shape gf-ll))
        (m (%lambda-list-shape m-ll)))
    (let ((g-req  (nth 0 g)) (g-opt (nth 1 g))
          (g-rest (nth 2 g)) (g-key (nth 3 g))
          (m-req  (nth 0 m)) (m-opt (nth 1 m))
          (m-rest (nth 2 m)) (m-key (nth 3 m)) (m-aok (nth 4 m)))
      (cond
        ((not (= m-req g-req)) nil)
        ((not (= m-opt g-opt)) nil)
        ((not (eq (if (or g-rest g-key) t nil)
                  (if (or m-rest m-key) t nil)))
         nil)
        ;; Keyword-name acceptance (rule 4).  Only binds when the GF
        ;; declares &key and the method uses &key without
        ;; &allow-other-keys (a &rest-only method accepts everything).
        ((and g-key m-key (not m-aok))
         (let ((g-names (%lambda-list-key-names gf-ll))
               (m-names (%lambda-list-key-names m-ll))
               (ok t)
               (cur nil))
           (setq cur g-names)
           (loop
             (when (null cur) (return nil))
             (unless (%dg-string-member (car cur) m-names)
               (setq ok nil))
             (setq cur (cdr cur)))
           ok))
        (t t)))))

(defun %dg-sym-eq (a b)
  "Lambda-list element identity: EQ first (same quoted form → same
   interned symbol), then non-empty name STRING= as a cross-flavor
   fallback.  Robust against native MVM syms whose names aren't in
   *sym-name-table* (SYMBOL-NAME returns \"\")."
  (or (eq a b)
      (let ((na (%dg-param-name a))
            (nb (%dg-param-name b)))
        (and (> (length na) 0) (string= na nb)))))

(defun %dg-member-sym (x lst)
  "Member via %dg-sym-eq."
  (let ((cur lst))
    (loop
      (when (null cur) (return nil))
      (when (%dg-sym-eq x (car cur)) (return t))
      (setq cur (cdr cur)))))

(defun %dg-required-params (ll)
  "Required parameter SYMBOLS (heads) of lambda-list LL —
   identity-preserving so callers can compare with EQ."
  (let ((acc nil) (cur ll))
    (loop
      (when (null cur) (return (nreverse acc)))
      (let ((p (car cur)))
        (when (%dg-amp-p p) (return (nreverse acc)))
        (setq acc (cons (if (and (consp p) (not (%cl-sym-p p)))
                            (car p)
                            p)
                        acc)))
      (setq cur (cdr cur)))))

(defun %dg-check-apo (lambda-list apo)
  "CLHS DEFGENERIC: :argument-precedence-order must name each required
   parameter of LAMBDA-LIST exactly once and nothing else.  Signals
   PROGRAM-ERROR on violation.  Comparison is EQ-based (with name
   fallback) so it works even when symbol names aren't recoverable."
  (let ((req (%dg-required-params lambda-list)))
    (let ((seen nil) (count 0) (cur apo))
      (loop
        (when (null cur) (return nil))
        (let ((a (car cur)))
          (unless (%dg-member-sym a req)
            (%signal-program-error))
          (when (%dg-member-sym a seen)
            (%signal-program-error))
          (setq seen (cons a seen))
          (setq count (+ count 1)))
        (setq cur (cdr cur)))
      (unless (= count (length req))
        (%signal-program-error)))))

(defun %validate-defgeneric-options (gf-name lambda-list options)
  "CLHS DEFGENERIC validation, signaled as PROGRAM-ERROR:
   - unknown options (defgeneric.error.6)
   - repeated singleton options (:documentation etc. — error.5)
   - bad :argument-precedence-order (error.4 / error.8)
   - inline :method lambda-lists not congruent with LAMBDA-LIST
     (error.9-19, CLHS 7.6.4)."
  (let ((apo-seen 0) (doc-seen 0) (mc-seen 0) (gfc-seen 0) (mcl-seen 0)
        (cur options))
    (loop
      (when (null cur) (return nil))
      (let ((opt (car cur)))
        (let ((nm (if (and (consp opt) (symbolp (car opt)))
                      (symbol-name (car opt))
                      nil)))
          (cond
            ((null nm) (%signal-program-error))
            ;; Name unrecoverable (native MVM symbol whose name isn't in
            ;; *sym-name-table*) — can't classify the option; be lenient
            ;; rather than mis-reporting it as unknown.
            ((string= nm "") nil)
            ((string= nm "METHOD")
             (unless (%dg-congruent-p lambda-list (%dg-method-ll opt))
               (%signal-program-error)))
            ((string= nm "ARGUMENT-PRECEDENCE-ORDER")
             (setq apo-seen (+ apo-seen 1))
             (when (> apo-seen 1) (%signal-program-error))
             (%dg-check-apo lambda-list (cdr opt)))
            ((string= nm "DOCUMENTATION")
             (setq doc-seen (+ doc-seen 1))
             (when (> doc-seen 1) (%signal-program-error)))
            ((string= nm "METHOD-COMBINATION")
             (setq mc-seen (+ mc-seen 1))
             (when (> mc-seen 1) (%signal-program-error)))
            ((string= nm "GENERIC-FUNCTION-CLASS")
             (setq gfc-seen (+ gfc-seen 1))
             (when (> gfc-seen 1) (%signal-program-error)))
            ((string= nm "METHOD-CLASS")
             (setq mcl-seen (+ mcl-seen 1))
             (when (> mcl-seen 1) (%signal-program-error)))
            ((string= nm "DECLARE") nil)
            (t (%signal-program-error)))))
      (setq cur (cdr cur))))
  nil)

(defun %gf-set-arg-precedence (gf-name apo lambda-list)
  "Record :argument-precedence-order on GF-NAME's gf as a list of
   required-argument indices (used by %method-more-specific-p)."
  (let ((gf (%find-gf gf-name)))
    (when gf
      (let ((req (%dg-required-params lambda-list))
            (idxs nil)
            (cur apo))
        (loop
          (when (null cur) (return nil))
          (let ((a (car cur))
                (pos nil))
            (let ((i 0) (rcur req))
              (loop
                (when (null rcur) (return nil))
                (when (%dg-sym-eq a (car rcur)) (setq pos i) (return nil))
                (setq i (+ i 1))
                (setq rcur (cdr rcur))))
            (when pos (setq idxs (cons pos idxs))))
          (setq cur (cdr cur)))
        (%gf-set-apo gf (nreverse idxs))))))

(defun %gf-record-method-meta (gf m method-ll)
  "Stash key-acceptance meta for method record M on GF, harvested from
   METHOD-LL: (has-key has-rest aok key-name-strings)."
  (when (and gf m)
    (let ((shape (%lambda-list-shape method-ll))
          (keys (%lambda-list-key-names method-ll)))
      (let ((meta (list (nth 3 shape) (nth 2 shape) (nth 4 shape) keys)))
        (%gf-set-method-meta gf (cons (cons m meta)
                                      (%gf-method-meta gf)))))))

(defun %find-gf (name)
  "Find generic function by name.  (setf X) function names are LISTS —
   each quoted occurrence is a distinct cons, so EQ never matches across
   call sites; compare those structurally by their inner symbol."
  (let ((cur *generic-functions*))
    (loop
      (when (null cur) (return nil))
      (let ((k (car (car cur))))
        (when (or (eq k name)
                  (and (consp k) (consp name)
                       (not (%cl-sym-p k)) (not (%cl-sym-p name))
                       (consp (cdr k)) (consp (cdr name))
                       (eq (car (cdr k)) (car (cdr name)))))
          (return (cdr (car cur)))))
      (setq cur (cdr cur)))))

(defun %defgeneric (name lambda-list combination)
  "Register or update a generic function.
   Stores the lambda-list so %defmethod / find-method can verify
   method-vs-GF congruence per CLHS 7.6.4.  LAMBDA-LIST = NIL means
   the GF is being auto-created by an early %defmethod (no declared
   shape known yet); congruence validation is skipped in that case."
  (let ((existing (%find-gf name)))
    (cond
      (existing
       (when lambda-list
         ;; CLHS DEFGENERIC redefinition: methods added by the PREVIOUS
         ;; defgeneric form are removed (defgeneric.31/32); methods added
         ;; by DEFMETHOD survive.
         (let ((dg-methods (%gf-defgeneric-methods existing)))
           (when dg-methods
             (let ((kept nil) (cur (%gf-methods existing)))
               (loop
                 (when (null cur) (return nil))
                 (let ((m (car cur))
                       (drop nil))
                   (let ((dcur dg-methods))
                     (loop
                       (when (null dcur) (return nil))
                       (when (eq m (car dcur)) (setq drop t) (return nil))
                       (setq dcur (cdr dcur))))
                   (unless drop (setq kept (cons m kept))))
                 (setq cur (cdr cur)))
               (%gf-set-methods existing (nreverse kept))))
           (%gf-set-defgeneric-methods existing nil))
         ;; Surviving (DEFMETHOD-added) methods must be congruent with
         ;; the new lambda-list (defgeneric.error.22): specializer count
         ;; must equal the new required-parameter count.
         (let ((req (%lambda-list-required-count lambda-list))
               (cur (%gf-methods existing)))
           (loop
             (when (null cur) (return nil))
             (unless (= (length (%method-specializers (car cur))) req)
               (%signal-program-error))
             (setq cur (cdr cur))))
         ;; A redefinition resets :argument-precedence-order (the new
         ;; form re-establishes it via %gf-set-arg-precedence if given).
         (%gf-set-apo existing nil))
       (%gf-set-combination existing combination)
       ;; Only update lambda-list when we actually got one — don't clear
       ;; an existing declaration with an implicit auto-create call.
       (when lambda-list
         (%gf-set-lambda-list existing lambda-list))
       existing)
      (t
       (let ((gf (%make-gf name)))
         (%gf-set-combination gf combination)
         (when lambda-list
           (%gf-set-lambda-list gf lambda-list))
         (setq *generic-functions* (cons (cons name gf) *generic-functions*))
         gf)))))

;; Make a method record: (qualifier specializer-list . fn)
(defun %make-method (qualifier specializers fn)
  (cons qualifier (cons specializers fn)))

(defun %method-qualifier (m)    (car m))
(defun %method-specializers (m) (cadr m))
(defun %method-fn (m)           (cddr m))

(defun %defmethod (gf-name qualifier specializers fn)
  "Add or replace a method on a generic function.
   CLHS 7.6.4: method specializers list must have the same length as
   the GF's required-parameter count.  When the GF has a declared
   lambda-list, validate.  Skip the check when the lambda-list is
   unknown (e.g., the auto-create case from a leading %defmethod)."
  ;; Ensure GF exists
  (when (null (%find-gf gf-name))
    (%defgeneric gf-name nil nil))
  (let ((gf (%find-gf gf-name)))
    (let ((decl-ll (%gf-lambda-list gf)))
      (when decl-ll
        (let ((req-count (%lambda-list-required-count decl-ll))
              (spec-count (length specializers)))
          (unless (= spec-count req-count)
            (%signal-program-error)))))
    (let ((new-method (%make-method qualifier specializers fn)))
      ;; Remove existing method with same qualifier+specializers, then prepend
      (let ((old (%gf-methods gf))
            (filtered nil))
        (let ((cur old))
          (loop
            (when (null cur) (return nil))
            (let ((m (car cur)))
              (let ((same-qual  (eq (%method-qualifier m) qualifier))
                    (same-specs (let ((s1 (%method-specializers m))
                                     (s2 specializers)
                                     (match t))
                                  (loop
                                    (when (and (null s1) (null s2)) (return match))
                                    (when (or (null s1) (null s2))
                                      (setq match nil) (return match))
                                    (when (not (eq (car s1) (car s2)))
                                      (setq match nil) (return match))
                                    (setq s1 (cdr s1))
                                    (setq s2 (cdr s2))))))
                (when (not (and same-qual same-specs))
                  (setq filtered (cons m filtered)))))
            (setq cur (cdr cur))))
        (%gf-set-methods gf (cons new-method (nreverse filtered))))
      new-method)))

(defun %dg-gf-callable (gf-name)
  "Value of a DEFGENERIC form: the registered dispatch fn for GF-NAME
   (callable at any arity — compile-funcall's GF-struct path caps at 4
   args) when one is recorded in *gf-fn-to-name*, else the GF struct."
  (let ((found nil))
    (let ((cur *gf-fn-to-name*))
      (loop
        (when (null cur) (return nil))
        (when (eq (cdr (car cur)) gf-name)
          (setq found (car (car cur)))
          (return nil))
        (setq cur (cdr cur))))
    (if (and found (not (eql found 0)))
        found
        (%find-gf gf-name))))

(defun %defgeneric-method (gf-name qualifier specializers fn method-ll)
  "Add an inline (:method ...) from a DEFGENERIC form.  Like %defmethod
   but (a) records the method as defgeneric-owned so a redefinition of
   the GF removes it (CLHS DEFGENERIC), and (b) stashes key-acceptance
   meta from METHOD-LL for CLHS 7.6.5 keyword checking."
  (let ((m (%defmethod gf-name qualifier specializers fn)))
    (let ((gf (%find-gf gf-name)))
      (when gf
        (%gf-set-defgeneric-methods
         gf (cons m (%gf-defgeneric-methods gf)))
        (%gf-record-method-meta gf m method-ll)))
    m))

;;; ============================================================
;;; Method Combination (short form)
;;; ============================================================

;; mc-record: vector #(%method-combination name operator identity-with-one)
(defun %make-mc (name operator identity-with-one)
  (let ((mc (make-array 4)))
    (aset mc 0 '%method-combination)
    (aset mc 1 name)
    (aset mc 2 operator)
    (aset mc 3 identity-with-one)
    mc))

(defun %mc-p (x)
  (if (or (fixnump x) (consp x) (null x)) nil
    (if (= (obj-subtag x) #x32)
      (if (>= (array-length x) 1)
        (eq (aref x 0) '%method-combination)
        nil)
      nil)))

(defun %mc-name (mc)               (aref mc 1))
(defun %mc-operator (mc)           (aref mc 2))
(defun %mc-identity-with-one (mc)  (aref mc 3))

(defun %find-mc (name)
  "Look up a registered method combination by NAME.
   Uses a symbol-name comparison instead of EQ because the runtime
   intern table doesn't always unify non-keyword #x50 symbols by
   eq (see MEMORY/feedback_eq_works_on_symbols caveats) — the
   `'and` symbol the cl-eval defgeneric handler stashes in the GF's
   combination slot may be a different object than the `'and`
   %init-method-combinations registered at build time.  Falling
   back to a name-equality probe makes the dispatch route to
   %gf-dispatch-custom even when the two symbols are not EQ."
  (let ((cur *method-combinations*)
        (name-str (cond ((stringp name) name)
                        ((symbolp name) (symbol-name name))
                        (t nil))))
    (loop
      (when (null cur) (return nil))
      (let ((k (car (car cur))))
        (when (or (eq k name)
                  (and name-str (symbolp k)
                       (string= (symbol-name k) name-str)))
          (return (cdr (car cur)))))
      (setq cur (cdr cur)))))

(defun %define-method-combination (name operator identity-with-one)
  "Register a method combination (short form)."
  (let ((mc (%make-mc name operator identity-with-one)))
    ;; Remove old entry if any
    (let ((new-reg nil)
          (cur *method-combinations*))
      (loop
        (when (null cur) (return nil))
        (when (not (eq (car (car cur)) name))
          (setq new-reg (cons (car cur) new-reg)))
        (setq cur (cdr cur)))
      (setq *method-combinations* (cons (cons name mc) new-reg)))
    ;; Return the name (CLHS says define-method-combination returns the name)
    name))

;;; ============================================================
;;; Method Combination (long form)
;;; ============================================================
;;;
;;; A long-form combination is stored as a 5-slot array, distinct from the
;;; 4-slot short-form mc so %mc-long-p can tell them apart while both live
;;; in the single *method-combinations* registry (%find-mc finds either):
;;;   slot [0] = '%method-combination-long  (marker)
;;;   slot [1] = name
;;;   slot [2] = builder-fn   — fn (applicable-methods combination-args) →
;;;                             effective-method FORM.  The builder does the
;;;                             method-group partition (so :order / :required
;;;                             forms evaluate in the combination's lexical
;;;                             scope), binds the group vars + the combination
;;;                             lambda-list params, and runs the body, which
;;;                             returns the emf form.
;;;   slot [3] = nparams      — number of required combination lambda-list
;;;                             params (for arity validation against the
;;;                             defgeneric :method-combination args)
;;;   slot [4] = doc          — documentation string or NIL
(defun %make-mc-long (name builder-fn nparams doc)
  (let ((mc (make-array 5)))
    (aset mc 0 '%method-combination-long)
    (aset mc 1 name)
    (aset mc 2 builder-fn)
    (aset mc 3 nparams)
    (aset mc 4 doc)
    mc))

(defun %mc-long-p (x)
  (if (or (fixnump x) (consp x) (null x)) nil
    (if (= (obj-subtag x) #x32)
      (if (>= (array-length x) 1)
        (eq (aref x 0) '%method-combination-long)
        nil)
      nil)))

(defun %mc-long-name (mc)    (aref mc 1))
(defun %mc-long-builder (mc) (aref mc 2))
(defun %mc-long-nparams (mc) (aref mc 3))

(defun %define-method-combination-long (name builder-fn nparams)
  "Register a long-form method combination NAME.  BUILDER-FN, given the
   applicable methods and the combination args, returns the effective
   method FORM.  Returns NAME (CLHS)."
  (let ((mc (%make-mc-long name builder-fn nparams nil)))
    (let ((new-reg nil)
          (cur *method-combinations*))
      (loop
        (when (null cur) (return nil))
        (when (not (eq (car (car cur)) name))
          (setq new-reg (cons (car cur) new-reg)))
        (setq cur (cdr cur)))
      (setq *method-combinations* (cons (cons name mc) new-reg)))
    name))

;;; --- Method-group qualifier-pattern matching ---
;;;
;;; A group specifier lists one or more qualifier patterns.  Per CLHS
;;; 7.6.6.2:
;;;   *            matches ANY method (used for the catch-all group)
;;;   ()  / nil    matches an UNQUALIFIED method (qualifier list is empty)
;;;   (sym)        matches a method with exactly that one qualifier
;;;   sym          (bare symbol) means "single qualifier = sym"
;;; Modus stores a method's qualifier as a single value (nil = unqualified,
;;; or a symbol like FOO / :AROUND).  We match accordingly.
(defun %dmc-qual-pattern-matches-p (pattern method-qualifier)
  "True if PATTERN (one qualifier-pattern from a method-group spec) matches
   a method whose stored qualifier is METHOD-QUALIFIER (nil = unqualified)."
  (cond
    ;; * matches everything.
    ((and (symbolp pattern) (not (null pattern))
          (string= (symbol-name pattern) "*")) t)
    ;; nil / () pattern matches an unqualified method.
    ((null pattern) (null method-qualifier))
    ;; (sym) list pattern — single qualifier match.
    ((consp pattern)
     (and (null (cdr pattern))
          (not (null method-qualifier))
          (symbolp method-qualifier)
          (symbolp (car pattern))
          (string= (symbol-name (car pattern)) (symbol-name method-qualifier))))
    ;; bare symbol pattern — single qualifier match.
    ((and (symbolp pattern) (not (null method-qualifier))
          (symbolp method-qualifier))
     (string= (symbol-name pattern) (symbol-name method-qualifier)))
    (t nil)))

(defun %dmc-method-matches-group-p (patterns method)
  "True if METHOD matches any of PATTERNS (a method-group's qualifier
   patterns)."
  (let ((q (%method-qualifier method))
        (cur patterns))
    (loop
      (when (null cur) (return nil))
      (when (%dmc-qual-pattern-matches-p (car cur) q) (return t))
      (setq cur (cdr cur)))))

(defun %dmc-apply-order (methods order)
  "Order a group's METHODS per ORDER.  METHODS arrive most-specific-first
   (the order %collect-applicable-methods produced).  :most-specific-last
   reverses; anything else keeps most-specific-first."
  (cond
    ((null order) methods)
    ((and (symbolp order)
          (string= (symbol-name order) "MOST-SPECIFIC-LAST"))
     (reverse methods))
    (t methods)))

;;; --- Effective-method-form evaluation ---
;;;
;;; The builder body returns an effective-method FORM built from the
;;; selected method objects.  Rather than route that form through the full
;;; runtime evaluator (the embedded method records are conses that EVAL
;;; would try to call), %dmc-eval-emf interprets the limited sublanguage
;;; the long-form bodies use: VECTOR, LIST, CALL-METHOD, MAKE-METHOD,
;;; QUOTE, PROGN, AND, OR, plus self-evaluating leaves.

;; Holds the generic function's runtime arguments while a long-form
;; effective method runs, so CALL-METHOD invokes each method with the
;; original GF args (CLHS 7.6.6.1 — a method is run on the GF args).
;; make-method-wrapped fns ignore args; ordinary methods need them.
;; defvar default NIL (CLAUDE.md item 7); %gf-dispatch-custom-long sets it
;; with save/restore (Modus LET doesn't dynamically bind a defvar).
(defvar *%dmc-call-args* nil)

(defun %dmc-emf-call-method (method)
  "Invoke METHOD (a method record) with the current GF args (held in
   *%dmc-call-args*) for CALL-METHOD inside an effective-method form."
  (let ((fn (%method-fn method)))
    (apply fn *%dmc-call-args*)))

(defun %dmc-eval-emf (form)
  "Interpret an effective-method FORM produced by a long-form combination
   body.  Supports VECTOR / LIST / CALL-METHOD / MAKE-METHOD / QUOTE /
   PROGN / AND / OR and self-evaluating leaves."
  (cond
    ((null form) nil)
    ((not (consp form)) form)
    (t
     (let ((head (car form)))
       (cond
         ((not (symbolp head))
          ;; Non-symbol head — a method record or other datum; pass through.
          form)
         ((string= (symbol-name head) "QUOTE")
          (cadr form))
         ((string= (symbol-name head) "CALL-METHOD")
          ;; (call-method METHOD [next-methods]) — METHOD is an embedded
          ;; method record (literal), not a sub-form to evaluate.
          (%dmc-emf-call-method (cadr form)))
         ((string= (symbol-name head) "MAKE-METHOD")
          ;; (make-method FORM) → a method record wrapping FORM; when the
          ;; surrounding emf call-methods it, FORM is evaluated.
          (make-method (cadr form)))
         ((string= (symbol-name head) "VECTOR")
          (%dmc-list-to-vector (%dmc-eval-emf-args (cdr form))))
         ((string= (symbol-name head) "LIST")
          (%dmc-eval-emf-args (cdr form)))
         ((string= (symbol-name head) "PROGN")
          (let ((vals (%dmc-eval-emf-args (cdr form))) (last nil) (c nil))
            (setq c vals)
            (loop (when (null c) (return last))
                  (setq last (car c)) (setq c (cdr c)))))
         ((string= (symbol-name head) "AND")
          (let ((c (cdr form)) (acc t))
            (loop (when (null c) (return acc))
                  (setq acc (%dmc-eval-emf (car c)))
                  (when (null acc) (return nil))
                  (setq c (cdr c)))))
         ((string= (symbol-name head) "OR")
          (let ((c (cdr form)) (acc nil))
            (loop (when (null c) (return acc))
                  (setq acc (%dmc-eval-emf (car c)))
                  (when acc (return acc))
                  (setq c (cdr c)))))
         (t
          ;; Unknown operator — fall back to the general evaluator.
          (eval form)))))))

(defun %dmc-eval-emf-args (forms)
  "Evaluate each form in FORMS via %dmc-eval-emf, returning a fresh list."
  (let ((acc nil) (cur forms))
    (loop
      (when (null cur) (return (nreverse acc)))
      (setq acc (cons (%dmc-eval-emf (car cur)) acc))
      (setq cur (cdr cur)))))

(defun %dmc-list-to-vector (lst)
  "Build a simple-vector from list LST."
  (let* ((n (length lst))
         (v (make-array n))
         (i 0)
         (cur lst))
    (loop
      (when (null cur) (return v))
      (let ((dummy (aset v i (car cur)))) dummy)
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

;;; --- Method-group partition (CLHS 7.6.6.2) ---
;;;
;;; GROUP-RECS is a list of (patterns order required-p) — one per method
;;; group, in declaration order.  Each APPLICABLE method is assigned to the
;;; FIRST group whose patterns match it.  A method matching no group is an
;;; error.  A :required group left empty is an error.  Returns the list of
;;; per-group ordered method lists (parallel to GROUP-RECS).
(defun %dmc-group-rec-patterns (r) (car r))
(defun %dmc-group-rec-order (r)    (cadr r))
(defun %dmc-group-rec-required (r) (caddr r))

(defun %dmc-partition-groups (applicable group-recs)
  "Partition APPLICABLE methods into the method groups GROUP-RECS.  Signals
   an error on a method matching no group, or an empty :required group.
   Returns a list of ordered method-lists (one per group)."
  ;; Build per-group accumulators (most-specific-first preserved by walking
  ;; APPLICABLE in order and appending).  We collect reversed then nreverse.
  (let ((rev-lists nil) (gr group-recs))
    ;; Initialize one empty accumulator per group.
    (loop (when (null gr) (return nil))
          (setq rev-lists (cons nil rev-lists))
          (setq gr (cdr gr)))
    (setq rev-lists (nreverse rev-lists))   ; list of NILs, parallel to recs
    ;; Assign each applicable method to its first matching group.
    (let ((cur applicable))
      (loop
        (when (null cur) (return nil))
        (let ((m (car cur)) (gi 0) (placed nil) (recs group-recs) (accs rev-lists))
          (loop
            (when (or placed (null recs)) (return nil))
            (when (%dmc-method-matches-group-p (%dmc-group-rec-patterns (car recs)) m)
              ;; Prepend m onto this group's accumulator (in-place via nth).
              (%dmc-nth-cons-push accs gi m)
              (setq placed t))
            (setq gi (+ gi 1))
            (setq recs (cdr recs)))
          (when (null placed)
            (error "method matches no method-combination group")))
        (setq cur (cdr cur))))
    ;; Finalize: reverse each accumulator (we pushed → reversed), apply
    ;; :order, and check :required.
    (let ((result nil) (recs group-recs) (accs rev-lists))
      (loop
        (when (null recs) (return (nreverse result)))
        (let* ((methods (nreverse (car accs)))
               (rec (car recs)))
          (when (and (%dmc-group-rec-required rec) (null methods))
            (error "required method-combination group is empty"))
          (setq result (cons (%dmc-apply-order methods (%dmc-group-rec-order rec))
                             result)))
        (setq recs (cdr recs))
        (setq accs (cdr accs))))))

;; Because Modus's mutable-closure cells are global, we can't reliably
;; mutate a captured per-group accumulator inside the partition loop.  We
;; instead keep the accumulators in a single list and splice via index.
;; %dmc-nth-cons-push prepends VAL to the GI-th element of the list ACCS,
;; mutating that cons in place with setf-of-car.
(defun %dmc-nth-cons-push (accs gi val)
  "Prepend VAL onto (nth GI ACCS), mutating the GI-th cons of ACCS."
  (let ((cur accs) (i 0))
    (loop
      (when (= i gi) (return nil))
      (setq cur (cdr cur))
      (setq i (+ i 1)))
    (setf (car cur) (cons val (car cur))))
  accs)

(defun %dmc-nth (lst n)
  "0-based nth element of LST (local helper to avoid relying on global nth
   semantics during dispatch)."
  (let ((cur lst) (i 0))
    (loop
      (when (null cur) (return nil))
      (when (= i n) (return (car cur)))
      (setq cur (cdr cur))
      (setq i (+ i 1)))))

;;; --- Long-form dispatch ---
(defun %gf-dispatch-custom-long (gf args applicable mc)
  "Long-form method combination dispatch.  Runs the combination's builder
   (which partitions APPLICABLE into method groups, binds params + group
   vars, and returns the effective-method FORM) then interprets that form.
   Binds *%dmc-call-args* to ARGS (save/restore — Modus LET doesn't
   dynamically bind a defvar) so CALL-METHOD runs each method on the GF's
   own arguments."
  (let* ((comb-raw (%gf-combination gf))
         (cargs (if (consp comb-raw) (cdr comb-raw) nil))
         (builder (%mc-long-builder mc))
         (saved-args *%dmc-call-args*))
    (setq *%dmc-call-args* args)
    (let ((emf (funcall builder applicable cargs)))
      ;; emf is built before any method runs; partition errors propagate
      ;; here (caught by the test's handler-case).  Restore on normal exit;
      ;; an error unwinds straight past (the next dispatch re-sets it).
      (let ((result (%dmc-eval-emf emf)))
        (setq *%dmc-call-args* saved-args)
        result))))

(defun %register-gf-default-primary (gf-name default-fn-sym)
  "Register DEFAULT-FN-SYM as the standard primary fallback for GF-NAME.
   When %gf-dispatch-standard finds no applicable primary method (only
   :before / :after / :around), it consults this registry and applies
   the named function to the runtime args list."
  (let ((new-reg nil)
        (cur *gf-default-primaries*))
    (loop
      (when (null cur) (return nil))
      (when (not (eq (car (car cur)) gf-name))
        (setq new-reg (cons (car cur) new-reg)))
      (setq cur (cdr cur)))
    (setq *gf-default-primaries*
          (cons (cons gf-name default-fn-sym) new-reg)))
  gf-name)

(defun %gf-default-primary (gf-name)
  "Look up the registered default-primary symbol for GF-NAME, or NIL."
  (let ((cur *gf-default-primaries*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) gf-name)
        (return (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun %gf-name-is-p (gf-name target-name target-name-str)
  "True iff GF-NAME refers to the symbol TARGET-NAME.  Uses both eq
   and symbol-name string-equal because Modus's intern can produce
   distinct symbol objects for the same name (CLAUDE.md: 'Two symbols
   with identical hashes may be eql but not eq').  The same dual
   compare is used by %find-mc — see its docstring."
  (cond
    ((eq gf-name target-name) t)
    ((and (symbolp gf-name)
          (string= (symbol-name gf-name) target-name-str)) t)
    (t nil)))

(defun %has-default-primary-p (gf-name)
  "True iff GF-NAME has a registered default-primary fallback.  Driven
   by a small symbol set rather than the *gf-default-primaries* alist
   so the test is cheap and the supported set is visible at one spot."
  (cond
    ((%gf-name-is-p gf-name 'shared-initialize "SHARED-INITIALIZE")    t)
    ((%gf-name-is-p gf-name 'change-class "CHANGE-CLASS")              t)
    ((%gf-name-is-p gf-name 'initialize-instance "INITIALIZE-INSTANCE") t)
    ((%gf-name-is-p gf-name 'reinitialize-instance "REINITIALIZE-INSTANCE") t)
    ((%gf-name-is-p gf-name 'update-instance-for-different-class
                    "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS")             t)
    ((%gf-name-is-p gf-name 'update-instance-for-redefined-class
                    "UPDATE-INSTANCE-FOR-REDEFINED-CLASS")             t)
    ((%gf-name-is-p gf-name 'print-object "PRINT-OBJECT")              t)
    (t                                                                 nil)))

(defun %dispatch-default-primary (gf-name args)
  "Run the standard primary body for GF-NAME on ARGS, bypassing GF
   dispatch.  Called from the synthetic primary built by
   %gf-dispatch-standard when no user primary is applicable.

   Dispatches on the GF name and calls the matching default helper in
   ansi-bridge.lisp directly — those helpers internally bypass GF
   dispatch so this can't loop back into %gf-dispatch-standard.

   Uses %gf-name-is-p (eq + symbol-name string-equal) because Modus's
   intern can produce distinct symbol objects for the same name."
  (cond
    ((%gf-name-is-p gf-name 'shared-initialize "SHARED-INITIALIZE")
     (%shared-initialize-via-args args))
    ((%gf-name-is-p gf-name 'change-class "CHANGE-CLASS")
     (%change-class-via-args args))
    ((%gf-name-is-p gf-name 'initialize-instance "INITIALIZE-INSTANCE")
     (%initialize-instance-via-args args))
    ((%gf-name-is-p gf-name 'reinitialize-instance "REINITIALIZE-INSTANCE")
     (%reinitialize-instance-via-args args))
    ((%gf-name-is-p gf-name 'update-instance-for-different-class
                    "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS")
     (%uifdc-via-args args))
    ((%gf-name-is-p gf-name 'update-instance-for-redefined-class
                    "UPDATE-INSTANCE-FOR-REDEFINED-CLASS")
     (%uifrc-via-args args))
    ((%gf-name-is-p gf-name 'print-object "PRINT-OBJECT")
     (%print-object-via-args args))
    (t nil)))

;; Default-primary trampolines.  Each forwards an args list (instance
;; / new-class / etc., followed by initargs) to the matching default-fn
;; in ansi-bridge.lisp without going through GF dispatch.  Defined as
;; weak forward-references via top-level defun stubs — the real bodies
;; are defined later via last-defun-wins; if ansi-bridge.lisp's
;; %shared-initialize-default isn't loaded yet for some reason, the
;; stub returns NIL rather than crashing.
(defun %shared-initialize-via-args (args)
  ;; (apply #'%shared-initialize-default args)
  ;; %shared-initialize-default exists in ansi-bridge.lisp; here we
  ;; spread by hand since (apply fn args) on a captured arg can lose
  ;; trailing elements past the register-arg limit (CLAUDE.md known
  ;; bug 7).  Same shape as %shared-init-default-spread.
  (cond
    ((null args) nil)
    ((null (cdr args)) (%shared-initialize-default (car args) nil))
    (t (%shared-init-default-spread args))))

(defun %change-class-via-args (args)
  (cond
    ((null args) nil)
    (t (%change-class-default-spread args))))

(defun %initialize-instance-via-args (args)
  (cond
    ((null args) nil)
    (t (apply #'%initialize-instance-default args))))

(defun %reinitialize-instance-via-args (args)
  (cond
    ((null args) nil)
    ;; reinitialize-instance default body lives in ansi-bridge.lisp;
    ;; the canonical bypass path there is `reinitialize-instance` (see
    ;; ansi-bridge:3035) which itself dispatches.  Instead, follow the
    ;; same pattern as shared-initialize: instance + (t) covers the
    ;; "init non-changed slots" semantics per CLHS.
    (t (apply #'%shared-initialize-default
              (car args) t (cdr args)))))

(defun %uifdc-via-args (args)
  (cond
    ((null args) nil)
    (t (apply #'%uifdc-default args))))

(defun %uifrc-via-args (args)
  (cond
    ((null args) nil)
    (t (apply #'%uifrc-default args))))

(defun %print-object-via-args (args)
  (cond
    ((null args) nil)
    (t (apply #'%print-object-default args))))

(defun %synthetic-primary-closure-fn (&rest %actual-ignored)
  "Top-level closure body for the synthetic primary built by
   %gf-dispatch-standard when no user primary is applicable.
   Reads (gf-name . (args . nil)) from %get-cenv and forwards to
   %dispatch-default-primary.  Lives at top level so the closure
   captures via heap cells (is-eql-p pattern) rather than the
   broken naïve let-bound + funcall path."
  (declare (ignore %actual-ignored))
  (let* ((env (%get-cenv))
         (gf-name (car env))
         (args (car (cdr env))))
    (%dispatch-default-primary gf-name args)))

(defun %init-method-combinations ()
  "Register the nine ANSI-defined short-form method combinations:
   AND, OR, APPEND, LIST, MAX, MIN, NCONC, PROGN, +.
   Each takes the same name as its operator; the :identity-with-one-argument
   semantics matches CLHS 7.6.6.4 (PROGN/AND/OR don't fold for one method,
   the rest do).  Without these, %gf-dispatch falls through to standard
   dispatch which silently calls primary methods on a custom-combination
   GF — wrong per ANSI (DG-MC.APPEND.10/etc. should error out).

   Also registers built-in default primaries for the standard CLOS GFs
   (shared-initialize, initialize-instance, reinitialize-instance,
   change-class).  Without these, a user-supplied :before/:after/:around
   method WITHOUT a corresponding primary method causes
   %gf-dispatch-standard to error 'no applicable primary method' — but
   CLHS says the system-supplied primary still runs.  See
   shared-initialize.7.x / 8.x / change-class.5.x / etc.

   These initializations are called from kernel-main AFTER every defun
   has been compiled, so (symbol-function 'NAME) succeeds at the lookup
   site below."
  (%define-method-combination 'and    'and    t)
  (%define-method-combination 'or     'or     t)
  (%define-method-combination 'append 'append t)
  (%define-method-combination 'list   'list   nil)
  (%define-method-combination 'max    'max    t)
  (%define-method-combination 'min    'min    t)
  (%define-method-combination 'nconc  'nconc  t)
  (%define-method-combination 'progn  'progn  t)
  (%define-method-combination '+      '+      t)
  ;; Default primaries: gf-name → SYMBOL naming the function that runs
  ;; the standard primary for that GF when no user primary is
  ;; applicable.  %gf-dispatch-standard calls (apply (symbol-function
  ;; SYM) args) with the GF's runtime args.  The functions named here
  ;; live in ansi-bridge.lisp (which loads after cl-clos.lisp) and MUST
  ;; bypass GF dispatch internally — otherwise the fallback would
  ;; re-enter %gf-dispatch-standard infinitely.
  ;;
  ;; %init-method-combinations is invoked from kernel-main AFTER every
  ;; runtime defun has compiled, so symbol-function lookup at the
  ;; fallback call site resolves correctly.
  (%register-gf-default-primary 'shared-initialize        '%shared-initialize-default)
  (%register-gf-default-primary 'change-class             '%change-class-default)
  (%register-gf-default-primary 'initialize-instance      '%initialize-instance-default)
  (%register-gf-default-primary 'update-instance-for-different-class
                                '%uifdc-default)
  (%register-gf-default-primary 'update-instance-for-redefined-class
                                '%uifrc-default)
  (%register-gf-default-primary 'print-object             '%print-object-default)
  nil)

;;; ============================================================
;;; Specializer matching
;;; ============================================================

(defun %specializer-matches-p (spec obj)
  "Return true if specializer SPEC matches OBJ.
   SPEC is a class name symbol, (eql val), or t."
  (cond
    ((eq spec 't) t)
    ;; (eql val) specializer
    ((and (consp spec) (eq (car spec) 'eql))
     (eql obj (cadr spec)))
    ;; Class name: check if obj's CPL includes it
    (t
     (let ((cpl (%obj-cpl obj)))
       (let ((cur cpl) (found nil))
         (loop
           (when (null cur) (return found))
           (let ((c (car cur)))
             (when (eq c spec) (setq found t) (return found))
             ;; FRAGILITY DIAG: detect the same-shape sixth bug.
             ;; If two symbols with the same name-hash failed to
             ;; compare eq, that's cross-function intern
             ;; non-determinism — the very thing the
             ;; contact predicted for the call-NIL path.
             ;; Only fire on eq-mismatch + name-hash match (rare
             ;; diagnostic event, doesn't perturb the hot success
             ;; path).  Budget at slot 0x10000C60 caps prints to
             ;; prevent flood + Heisenberg; kernel-main initializes
             ;; it to 5 (defvar init-thunks don't run on bare metal).
             ;; Gated on symbolp on both sides so we skip conses/etc.
             ;; that wouldn't have name-hashes anyway.  NOTE: T and NIL
             ;; ARE symbols (symbolp now correctly returns T for the
             ;; immediates) but they have NO slot-0 hash — (aref t 0) /
             ;; (aref nil 0) would deref the immediate as a heap pointer
             ;; and SEGV.  Exclude them explicitly before the aref.
             (let ((budget (mem-ref #x10000C60 :u64)))
               (when (and (> budget 0)
                          (symbolp c) (symbolp spec)
                          (not (null c)) (not (eq c t))
                          (not (null spec)) (not (eq spec t))
                          (= (aref c 0) (aref spec 0)))
                 (write-string-serial "EQ-COLL hash=")
                 (print-dec (aref spec 0))
                 (write-string-serial " a/4=")
                 (print-dec (ash c -1))
                 (write-string-serial " b/4=")
                 (print-dec (ash spec -1))
                 (write-char-serial 10)
                 (setf (mem-ref #x10000C60 :u64) (- budget 1)))))
           (setq cur (cdr cur))))))))

(defun %specializers-match-p (specs args)
  "True if all specializers in SPECS match corresponding ARGS."
  (let ((s specs) (a args) (ok t))
    (loop
      (when (or (null s) (null a)) (return ok))
      (when (not (%specializer-matches-p (car s) (car a)))
        (setq ok nil) (return ok))
      (setq s (cdr s))
      (setq a (cdr a)))
    ok))

;;; ============================================================
;;; Method applicability + ordering
;;; ============================================================

(defun %method-specificity (m args)
  "Return a specificity score for method M on ARGS.
   Lower = more specific. Based on position of primary specializer in CPL."
  (let ((specs (%method-specializers m))
        (score 0))
    (let ((s specs) (a args))
      (loop
        (when (or (null s) (null a)) (return score))
        (let ((spec (car s))
              (arg  (car a)))
          (cond
            ;; (eql val) — extremely specific (score 0)
            ((and (consp spec) (eq (car spec) 'eql))
             (setq score score))
            ((eq spec 't)
             (setq score (+ score 10000)))
            (t
             ;; Position in CPL (0 = most specific)
             (let ((cpl (%obj-cpl arg))
                   (pos 0)
                   (found nil))
               (let ((c cpl))
                 (loop
                   (when (null c) (return nil))
                   (when (eq (car c) spec) (setq found t) (return nil))
                   (setq pos (+ pos 1))
                   (setq c (cdr c))))
               (if found
                 (setq score (+ score pos))
                 (setq score (+ score 10000)))))))
        (setq s (cdr s))
        (setq a (cdr a))))
    score))

(defun %spec-rank (spec arg)
  "Rank of specializer SPEC for argument ARG: lower = more specific.
   (eql v) = -1, class = its position in ARG's CPL, T / not-in-CPL =
   10000.  Mirrors the matching rules of %method-specificity."
  (cond
    ((and (consp spec) (eq (car spec) 'eql)) -1)
    ((eq spec 't) 10000)
    (t
     (let ((cpl (%obj-cpl arg))
           (pos 0)
           (found nil))
       (let ((c cpl))
         (loop
           (when (null c) (return nil))
           (when (eq (car c) spec) (setq found t) (return nil))
           (setq pos (+ pos 1))
           (setq c (cdr c))))
       (if found pos 10000)))))

(defun %dg-nth-or-t (n lst)
  "NTH that falls back to the symbol T past the end of LST — missing
   specializer positions rank as T (least specific)."
  (let ((cur lst) (i 0))
    (loop
      (when (null cur) (return 't))
      (when (= i n) (return (car cur)))
      (setq i (+ i 1))
      (setq cur (cdr cur)))))

(defun %method-more-specific-p (m1 m2 args apo)
  "CLHS 7.6.6.1.2: T iff M1 is strictly more specific than M2 for ARGS.
   Specializers are compared POSITION BY POSITION (lexicographically),
   in argument-precedence order when APO (a list of arg indices) is
   non-NIL, left-to-right otherwise.  The old sum-of-scores comparator
   got two-arg cases like defgeneric.3 backwards."
  (let ((s1 (%method-specializers m1))
        (s2 (%method-specializers m2))
        (idxs apo))
    (when (null idxs)
      ;; Build (0 1 ... n-1) for left-to-right comparison.
      (let ((n (length args))
            (acc nil))
        (let ((i (- n 1)))
          (loop
            (when (< i 0) (return nil))
            (setq acc (cons i acc))
            (setq i (- i 1))))
        (setq idxs acc)))
    (let ((cur idxs))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)))
          (let ((r1 (%spec-rank (%dg-nth-or-t k s1) (%dg-nth-or-t k args)))
                (r2 (%spec-rank (%dg-nth-or-t k s2) (%dg-nth-or-t k args))))
            (when (< r1 r2) (return t))
            (when (> r1 r2) (return nil))))
        (setq cur (cdr cur))))))

(defun %collect-applicable-methods (gf args)
  "Return all applicable methods sorted most-specific first.
   Sorting is the CLHS lexicographic specializer comparison
   (%method-more-specific-p), honoring :argument-precedence-order."
  (let ((methods (%gf-methods gf))
        (applicable nil)
        (apo (%gf-apo gf)))
    ;; Collect applicable
    (let ((cur methods))
      (loop
        (when (null cur) (return nil))
        (let ((m (car cur)))
          (when (%specializers-match-p (%method-specializers m) args)
            (setq applicable (cons m applicable))))
        (setq cur (cdr cur))))
    ;; Insertion sort, most-specific first (usually few methods)
    (let ((sorted nil))
      (let ((cur applicable))
        (loop
          (when (null cur) (return nil))
          (let ((m (car cur)))
            ;; Insert m into sorted before the first method it beats
            (let ((new-sorted nil)
                  (inserted nil)
                  (prev sorted))
              (loop
                (when (null prev)
                  (if inserted
                    (setq new-sorted (nreverse new-sorted))
                    (setq new-sorted (nreverse (cons m new-sorted))))
                  (return nil))
                (let ((pm (car prev)))
                  (if (and (not inserted)
                           (%method-more-specific-p m pm args apo))
                    (progn
                      (setq new-sorted (cons m new-sorted))
                      (setq new-sorted (cons pm new-sorted))
                      (setq inserted t))
                    (setq new-sorted (cons pm new-sorted))))
                (setq prev (cdr prev)))
              (setq sorted new-sorted)))
          (setq cur (cdr cur))))
      sorted)))

;;; ============================================================
;;; Standard method combination dispatch
;;; ============================================================

(defun %gf-dispatch-standard (gf args applicable)
  "Standard method combination: :around > :before + primary + :after."
  ;; Record the GF being dispatched so CALL-NEXT-METHOD can recompute the
  ;; applicable-method set on replacement args (CLHS 7.6.6.2).  Save/restore
  ;; for nested GF calls from within method bodies.
  (setq *%current-gf* gf)
  (let ((around-methods nil)
        (before-methods nil)
        (primary-methods nil)
        (after-methods nil))
    ;; Partition by qualifier
    (let ((cur applicable))
      (loop
        (when (null cur) (return nil))
        (let ((m (car cur)))
          (let ((q (%method-qualifier m)))
            (cond
              ((eq q :around)
               (setq around-methods (cons m around-methods)))
              ((eq q :before)
               (setq before-methods (cons m before-methods)))
              ((eq q :after)
               (setq after-methods (cons m after-methods)))
              ;; nil or primary
              (t
               (setq primary-methods (cons m primary-methods))))))
        (setq cur (cdr cur))))
    (setq around-methods  (nreverse around-methods))
    (setq before-methods  (nreverse before-methods))
    (setq primary-methods (nreverse primary-methods))
    ;; after methods run in reverse order (most specific last)
    ;; but we collected them in most-specific-first order, so reverse = least-specific-first
    ;; Actually CL spec: :after most specific last, so keep them reversed:
    ;; after-methods currently in most-specific-first; run least-specific-first
    ;;
    ;; CLHS standard method combination: the system-supplied primary
    ;; runs even when the user has only added :before / :after / :around
    ;; methods.  When no user primary is applicable, look up the GF's
    ;; default-primary symbol and synthesize a method record that calls
    ;; it with the runtime args.  This is what makes
    ;; shared-initialize.7.x / 8.x pass — defmethod shared-initialize
    ;; :before runs alongside the standard primary, not instead of it.
    (let ((gf-name (%gf-name gf)))
      ;; Hardwired fallbacks for the standard CLOS GFs.  When the GF
      ;; has only auxiliary methods (:before / :after / :around) the
      ;; CLHS-mandated system primary still runs — we synthesize a
      ;; fake primary that delegates to a bypass helper in
      ;; ansi-bridge.lisp (%shared-initialize-via-args et al).
      ;;
      ;; Closure representation: use the is-eql-p heap-cell pattern
      ;; (%make-closure + top-level helper + %get-cenv) because the
      ;; naïve (lambda ...) capture pattern breaks when the closure
      ;; is let-bound + funcalled via a method record — see
      ;; CLAUDE.md Known Bug "Closure-mutation: let-bound + funcall
      ;; fails" and feedback_closure_mutation_let_bound.  Without
      ;; this dodge the synthetic primary's captured gf-name / args
      ;; were stale by the time run-primary did (apply method-fn
      ;; args), and the fallback silently no-op'd — sweeps came
      ;; back exactly == baseline.
      (cond
        ((null primary-methods)
         (cond
           ((%has-default-primary-p gf-name)
            (let ((synthetic
                   (%make-closure #'%synthetic-primary-closure-fn
                                  (cons gf-name (cons args nil)))))
              (setq primary-methods
                    (list (%make-method nil '(t) synthetic)))))
           (t (error "no applicable primary method"))))
        ;; User primaries exist AND the GF has a system-supplied
        ;; default primary: append a synthetic tail method so
        ;; CALL-NEXT-METHOD from the least-specific user primary
        ;; reaches the standard behavior (CLHS 7.6.6.2 — the
        ;; system primary on standard-object is always least
        ;; specific; change-class.6.x relies on this).
        ((%has-default-primary-p gf-name)
         (let ((synthetic
                (%make-closure #'%synthetic-primary-closure-fn
                               (cons gf-name (cons args nil)))))
           (setq primary-methods
                 (append primary-methods
                         (list (%make-method nil '(t) synthetic))))))))
    ;; Build the effective method chain
    (let ((run-primary
           ;; Accept &rest so when the :around chain wraps run-primary in
           ;; a primary-sentinel and call-next-method does
           ;; (apply (method-fn sentinel) actual-args), the runtime args
           ;; (e.g. the instance) are happily ignored — run-primary
           ;; already closed over `args' at outer dispatch time.
           (lambda (&rest _ignored)
             (declare (ignore _ignored))
             ;; Run before methods
             (let ((cur before-methods))
               (loop
                 (when (null cur) (return nil))
                 (apply (%method-fn (car cur)) args)
                 (setq cur (cdr cur))))
             ;; Run primary methods with call-next-method chain.
             ;; Use setq+save/restore so the binding is visible to method
             ;; bodies in other functions; bare-metal LET on a defvar
             ;; doesn't create dynamic scope by default.
             ;; Save primary chain's MV via multiple-value-list so :after
             ;; can run between the primary call and the value return
             ;; without collapsing MV to a single value.
             (let ((result-list
                    (let ((saved-nm *%next-methods*)
                          (saved-args *%current-gf-args*))
                      (setq *%next-methods* (cdr primary-methods))
                      (setq *%current-gf-args* args)
                      (setq *%current-gf* gf)
                      (let ((rl (multiple-value-list
                                 (apply (%method-fn (car primary-methods)) args))))
                        (setq *%next-methods* saved-nm)
                        (setq *%current-gf-args* saved-args)
                        rl))))
               ;; Run after methods (least specific first = most-specific last)
               (let ((cur (nreverse after-methods)))
                 (loop
                   (when (null cur) (return nil))
                   (apply (%method-fn (car cur)) args)
                   (setq cur (cdr cur))))
               (values-list result-list)))))
      (if around-methods
        ;; Run around methods wrapping the primary chain.  Modus's LET
        ;; on a defvar doesn't create dynamic scope, so we have to use
        ;; setq+save/restore around the call — same pattern as the
        ;; primary chain above.  Without this, call-next-method inside
        ;; an :around method reads the GLOBAL *%next-methods* (still
        ;; whatever the OUTER dispatch left it at) and gets the wrong
        ;; chain.
        (let ((primary-sentinel (%make-method nil nil run-primary)))
          (let ((next-chain
                 (let ((rev-around (cdr around-methods)))
                   ;; Append primary-sentinel after the remaining arounds.
                   (let ((result nil) (cur rev-around))
                     (loop
                       (when (null cur)
                         (setq result (cons primary-sentinel result))
                         (return nil))
                       (setq result (cons (car cur) result))
                       (setq cur (cdr cur)))
                     (nreverse result)))))
            (let ((saved-nm   *%next-methods*)
                  (saved-args *%current-gf-args*))
              (setq *%next-methods*    next-chain)
              (setq *%current-gf-args* args)
              (setq *%current-gf* gf)
              (multiple-value-prog1
                  (apply (%method-fn (car around-methods)) args)
                (setq *%next-methods*    saved-nm)
                (setq *%current-gf-args* saved-args)))))
        ;; No around: just run primary + before/after
        (funcall run-primary)))))

;;; ============================================================
;;; Custom method combination dispatch (short form)
;;; ============================================================

(defun %gf-dispatch-custom (gf args applicable mc &optional msl)
  "Short-form method combination: collect qualifying methods and apply
   operator.  MSL non-nil = :most-specific-last — primary methods run
   in reverse (least-specific-first) order; :around ordering is
   unaffected (CLHS 7.6.6.4)."
  (let ((comb-name (%mc-name mc))
        (operator (%mc-operator mc))
        (identity-with-one (%mc-identity-with-one mc)))
    ;; Collect :around and comb-name-qualified methods.  Any OTHER
    ;; qualifier (including NIL/unqualified) on an APPLICABLE method is
    ;; invalid for a short-form combination — CLHS 7.6.6.4 requires the
    ;; CALL to signal (dg-mc.append.13: a `nonsense`-qualified method
    ;; only errors for argument classes where it's applicable; calls
    ;; that don't reach it return normally).
    (let ((around-methods nil)
          (primary-methods nil)
          (invalid-methods nil))
      (let ((cur applicable))
        (loop
          (when (null cur) (return nil))
          (let ((m (car cur)))
            (let ((q (%method-qualifier m)))
              (cond
                ((eq q :around)
                 (setq around-methods (cons m around-methods)))
                ((or (eq q comb-name)
                     (and (symbolp q) (symbolp comb-name)
                          (string= (symbol-name q) (symbol-name comb-name))))
                 ;; Symbol-name comparison covers the case where two
                 ;; non-keyword symbols carry the same name but aren't
                 ;; EQ (see %find-mc docstring for the cl-eval
                 ;; intern-table caveat).
                 (setq primary-methods (cons m primary-methods)))
                (t
                 (setq invalid-methods (cons m invalid-methods))))))
          (setq cur (cdr cur))))
      (when invalid-methods
        (error "invalid method qualifier for method combination"))
      (setq around-methods  (nreverse around-methods))
      (setq primary-methods (nreverse primary-methods))
      ;; :most-specific-last — reverse the primary order (collected
      ;; most-specific-first above).  Arounds keep their order.
      (when msl
        (setq primary-methods (reverse primary-methods)))
      (when (null primary-methods)
        (error "no applicable method for combination"))
      ;; Compute the combined result.
      ;; &rest in the lambda list because call-next-method in an :around
      ;; method does `(apply (%method-fn sentinel) actual-args)` —
      ;; passing the GF's runtime args — and the standard run-primary
      ;; (line ~1326) takes &rest for the same reason.  Without &rest
      ;; here, calling combined-thunk with the GF args triggered an
      ;; arity mismatch that surfaced as SIGSEGV inside the heap on
      ;; defgeneric-method-combination.and.{4,5,6,7,8} — the
      ;; user-pointed-out crashes.  The body ignores the runtime args
      ;; and uses the captured `args` list (closed over the outer let),
      ;; matching standard-method semantics for AROUND→primary chains.
      ;; combined-thunk: invoke applicable primary methods one at a time
      ;; and short-circuit per the combination operator's semantics.
      ;;
      ;; Why inline per-method evaluation instead of collect-then-fold?
      ;; CLHS 7.6.6.4 mandates the effective method behave as
      ;;   (OP (call-method m1) (call-method m2) ...)
      ;; which for AND / OR is short-circuiting — methods AFTER the
      ;; first NIL (for AND) or first non-NIL (for OR) MUST NOT run.
      ;; Tests defgeneric-method-combination.and.1 expect *x* = (3 4)
      ;; for input 1 (only integer + rational push, AND sees NIL from
      ;; rational and stops); the prior collect-then-fold incorrectly
      ;; pushed 1 2 3 4 because all four methods ran.
      ;;
      ;; The previous lambda took &rest so call-next-method's
      ;; `(apply sentinel actual-args)` wouldn't arity-mismatch.  We
      ;; keep that shape — the body still ignores the runtime args and
      ;; uses the captured `args` list from the outer let, matching
      ;; standard-method semantics for AROUND->primary chains.
      (let ((combined-thunk
             (lambda (&rest %ignored-actual-args)
               (declare (ignore %ignored-actual-args))
               ;; identity-with-one + single method: pass MV through.
               (if (and identity-with-one (null (cdr primary-methods)))
                 (apply (%method-fn (car primary-methods)) args)
                 ;; Multi-method: per-operator inline fold with the
                 ;; short-circuit semantics the operator demands.
                 (cond
                   ((eq operator 'and)
                    ;; Short-circuit on NIL; value is last non-NIL or NIL.
                    (let ((acc t) (cur primary-methods))
                      (loop
                        (when (null cur) (return acc))
                        (setq acc (apply (%method-fn (car cur)) args))
                        (when (null acc) (return nil))
                        (setq cur (cdr cur)))))
                   ((eq operator 'or)
                    ;; Short-circuit on truthy; value is first non-NIL or NIL.
                    (let ((acc nil) (cur primary-methods))
                      (loop
                        (when (null cur) (return acc))
                        (setq acc (apply (%method-fn (car cur)) args))
                        (when acc (return acc))
                        (setq cur (cdr cur)))))
                   ((eq operator 'progn)
                    ;; Run all, return last; matches (progn ...) semantics.
                    (let ((acc nil) (cur primary-methods))
                      (loop
                        (when (null cur) (return acc))
                        (setq acc (apply (%method-fn (car cur)) args))
                        (setq cur (cdr cur)))))
                   ((eq operator 'list)
                    (let ((acc nil) (cur primary-methods))
                      (loop
                        (when (null cur) (return (nreverse acc)))
                        (setq acc (cons (apply (%method-fn (car cur)) args) acc))
                        (setq cur (cdr cur)))))
                   ((eq operator '+)
                    (let ((acc 0) (cur primary-methods))
                      (loop
                        (when (null cur) (return acc))
                        (setq acc (+ acc (apply (%method-fn (car cur)) args)))
                        (setq cur (cdr cur)))))
                   ((eq operator '*)
                    (let ((acc 1) (cur primary-methods))
                      (loop
                        (when (null cur) (return acc))
                        (setq acc (* acc (apply (%method-fn (car cur)) args)))
                        (setq cur (cdr cur)))))
                   ((eq operator 'max)
                    (let ((acc (apply (%method-fn (car primary-methods)) args))
                          (cur (cdr primary-methods)))
                      (loop
                        (when (null cur) (return acc))
                        (let ((v (apply (%method-fn (car cur)) args)))
                          (when (> v acc) (setq acc v)))
                        (setq cur (cdr cur)))))
                   ((eq operator 'min)
                    (let ((acc (apply (%method-fn (car primary-methods)) args))
                          (cur (cdr primary-methods)))
                      (loop
                        (when (null cur) (return acc))
                        (let ((v (apply (%method-fn (car cur)) args)))
                          (when (< v acc) (setq acc v)))
                        (setq cur (cdr cur)))))
                   ((eq operator 'append)
                    (let ((acc nil) (cur primary-methods))
                      (loop
                        (when (null cur) (return acc))
                        (setq acc (append acc (apply (%method-fn (car cur)) args)))
                        (setq cur (cdr cur)))))
                   ((eq operator 'nconc)
                    (let ((acc nil) (cur primary-methods))
                      (loop
                        (when (null cur) (return acc))
                        (setq acc (nconc acc (apply (%method-fn (car cur)) args)))
                        (setq cur (cdr cur)))))
                   (t
                    ;; Unknown operator -- fall back to collect-then-apply.
                    (let ((results nil) (cur primary-methods))
                      (loop
                        (when (null cur) (return nil))
                        (setq results (cons (apply (%method-fn (car cur)) args) results))
                        (setq cur (cdr cur)))
                      (apply operator (nreverse results)))))))))
        (if around-methods
          ;; Around wraps combined.  Modus's LET on a defvar doesn't create
          ;; dynamic scope, so we have to use setq+save/restore around the
          ;; call — same pattern as the standard run-primary path above.
          ;; Without this, call-next-method inside an :around method reads
          ;; the GLOBAL *%next-methods* (whatever the OUTER dispatch left
          ;; it at) and gets the wrong chain — corrupting tests like
          ;; AND.6 with two layered around methods.
          (let ((primary-sentinel (%make-method nil nil combined-thunk)))
            (let ((next-chain
                   (let ((result nil)
                         (cur (cdr around-methods)))
                     (loop
                       (when (null cur)
                         (setq result (cons primary-sentinel result))
                         (return nil))
                       (setq result (cons (car cur) result))
                       (setq cur (cdr cur)))
                     (nreverse result))))
              (let ((saved-nm   *%next-methods*)
                    (saved-args *%current-gf-args*))
                (setq *%next-methods*    next-chain)
                (setq *%current-gf-args* args)
                (multiple-value-prog1
                    (apply (%method-fn (car around-methods)) args)
                  (setq *%next-methods*    saved-nm)
                  (setq *%current-gf-args* saved-args)))))
          ;; No around
          (funcall combined-thunk))))))

;;; ============================================================
;;; Main dispatch entry point
;;; ============================================================

(defun %gf-check-keys (gf args applicable)
  "CLHS 7.6.5: when the GF's lambda-list mentions &KEY (and not
   &ALLOW-OTHER-KEYS), each keyword argument must be accepted by the GF
   or by some applicable method.  :ALLOW-OTHER-KEYS <true> at the call
   site disables the check.  Methods with &ALLOW-OTHER-KEYS, &REST
   without &KEY, or unknown lambda-lists (no meta) make the call
   lenient.  Signals PROGRAM-ERROR on an invalid keyword
   (defgeneric.error.20/21)."
  (let ((ll (%gf-lambda-list gf)))
    (when ll
      (let ((shape (%lambda-list-shape ll)))
        (when (and (nth 3 shape) (not (nth 4 shape)))
          ;; Skip required + optional args to reach the keyword portion.
          (let ((skip (+ (nth 0 shape) (nth 1 shape)))
                (kp args))
            (let ((i 0))
              (loop
                (when (or (null kp) (>= i skip)) (return nil))
                (setq kp (cdr kp))
                (setq i (+ i 1))))
            (when kp
              ;; CLHS 3.5.1.6: the keyword portion must be a proper
              ;; even-length plist — an odd trailing keyword (e.g.
              ;; (gf 1 :y) with no value) is a program-error
              ;; (defmethod.error.15).
              (let ((klen 0) (kc kp))
                (loop
                  (when (null kc) (return nil))
                  (setq klen (+ klen 1))
                  (setq kc (cdr kc)))
                (when (not (= 0 (mod klen 2)))
                  (%signal-program-error)))
              ;; Call-site :allow-other-keys — leftmost occurrence wins.
              (let ((aok-seen nil) (aok-val nil))
                (let ((cur kp))
                  (loop
                    (when (or (null cur) (null (cdr cur))) (return nil))
                    (let ((k (car cur)))
                      (when (and (not aok-seen) (symbolp k)
                                 (string= (symbol-name k)
                                          "ALLOW-OTHER-KEYS"))
                        (setq aok-seen t)
                        (setq aok-val (car (cdr cur)))))
                    (setq cur (cdr (cdr cur)))))
                (unless (and aok-seen aok-val)
                  ;; Valid names = GF keys + applicable methods' keys.
                  (let ((valid (%lambda-list-key-names ll))
                        (lenient nil))
                    (let ((mcur applicable))
                      (loop
                        (when (null mcur) (return nil))
                        (let ((meta (%gf-method-meta-for gf (car mcur))))
                          (cond
                            ((null meta) (setq lenient t))
                            ((nth 2 meta) (setq lenient t))
                            ((and (nth 1 meta) (not (nth 0 meta)))
                             (setq lenient t))
                            (t
                             (let ((c (nth 3 meta)))
                               (loop
                                 (when (null c) (return nil))
                                 (setq valid (cons (car c) valid))
                                 (setq c (cdr c)))))))
                        (setq mcur (cdr mcur))))
                    (unless lenient
                      (let ((cur kp))
                        (loop
                          (when (null cur) (return nil))
                          (let ((k (car cur)))
                            (let ((kn (if (symbolp k) (symbol-name k) nil)))
                              (when (null kn) (%signal-program-error))
                              (unless (or (string= kn "ALLOW-OTHER-KEYS")
                                          (%dg-string-member kn valid))
                                (%signal-program-error))))
                          (setq cur (cdr (cdr cur))))))))))))))
    nil))

(defun %derive-gf-ll-from-method (params)
  "Derive an implicit generic-function lambda-list from a method's
   lambda-list PARAMS (CLHS 7.6.4).  Only the SHAPE matters for arity
   checking: keep the required parameter names, and if the method has
   any &optional/&rest/&key, give the GF a compatible variadic tail
   (&optional o1 o2 … for each optional, plus &rest for open-endedness
   when the method has &rest/&key).  Method-only &aux is dropped (it is
   not part of the congruence-relevant lambda list)."
  (let ((required nil)
        (optionals nil)
        (has-rest nil)
        (has-key nil)
        (has-aok nil)
        (mode :required)
        (cur params))
    (loop
      (when (null cur) (return nil))
      (let ((p (car cur)))
        (cond
          ((%lambda-list-keyword-p p "&OPTIONAL") (setq mode :optional))
          ((%lambda-list-keyword-p p "&REST")
           (setq mode :rest) (setq has-rest t))
          ((%lambda-list-keyword-p p "&BODY")
           (setq mode :rest) (setq has-rest t))
          ((%lambda-list-keyword-p p "&KEY")
           (setq mode :key) (setq has-key t))
          ((%lambda-list-keyword-p p "&AUX") (setq mode :aux))
          ((%lambda-list-keyword-p p "&ALLOW-OTHER-KEYS") (setq has-aok t))
          (t
           (cond
             ((eq mode :required)
              (setq required (cons (if (consp p) (car p) p) required)))
             ((eq mode :optional)
              (setq optionals (cons (if (consp p) (car p) p) optionals)))))))
      (setq cur (cdr cur)))
    (let ((ll (nreverse required)))
      (when optionals
        (setq ll (append ll (cons '&optional (nreverse optionals)))))
      ;; CLHS 7.6.4: the GF lambda-list must contain &rest / &key when the
      ;; method does, so the GF is variadic AND so %gf-check-keys (which
      ;; only activates when the GF declares &KEY) runs keyword validation
      ;; against the applicable methods' accepted keys (defmethod.error.14/.15).
      (cond
        (has-key
         (setq ll (append ll (list '&key)))
         (when has-aok (setq ll (append ll (list '&allow-other-keys)))))
        (has-rest
         (setq ll (append ll (list '&rest '%gf-derived-rest)))))
      ll)))

(defun %gf-check-arity (gf args)
  "CLHS 7.6.4: a call to a generic function must supply at least as many
   args as the GF has required parameters, and — unless the GF lambda-
   list has &optional/&rest/&key — exactly that many.  A mismatch is a
   PROGRAM-ERROR (defmethod.error.13/.14/.15: the implicitly-created GF
   from a lone DEFMETHOD enforces its (x) / (x &key) shape).  Skipped
   when the GF has no declared lambda-list (auto-create case, shape
   unknown)."
  (let ((ll (%gf-lambda-list gf)))
    (when ll
      (let ((req (%lambda-list-required-count ll))
            (variadic (%lambda-list-has-rest-or-key ll))
            (n (length args)))
        (when (< n req) (%signal-program-error))
        (when (and (not variadic) (> n req)) (%signal-program-error))))))

(defun %gf-dispatch (name args)
  "Dispatch generic function NAME with ARGS.
   The combination slot holds either a bare combination name (symbol)
   or (NAME . :MOST-SPECIFIC-LAST) when the defgeneric supplied the
   ordering option — primaries then run least-specific-first per
   CLHS 7.6.6.4 (the :around chain ordering is NOT affected)."
  (let ((gf (%find-gf name)))
    (when (null gf)
      (error "undefined generic function"))
    ;; CLHS 7.6.4: validate the supplied arg count against the GF's
    ;; required-parameter count BEFORE method selection — too few/many
    ;; required args is a program-error, not a no-applicable-method.
    (%gf-check-arity gf args)
    (let ((applicable (%collect-applicable-methods gf args)))
      (when (null applicable)
        ;; Try no-applicable-method hook
        (error "no applicable method"))
      ;; CLHS 7.6.5 keyword validity (no-op unless the GF declares &KEY).
      (%gf-check-keys gf args applicable)
      (let* ((comb-raw (%gf-combination gf))
             (comb-name (if (consp comb-raw) (car comb-raw) comb-raw))
             (msl (and (consp comb-raw)
                       (let ((o (cdr comb-raw)))
                         (and (symbolp o)
                              (string= (symbol-name o) "MOST-SPECIFIC-LAST"))))))
        (if comb-name
          ;; Custom method combination
          (let ((mc (%find-mc comb-name)))
            (cond
              ((null mc)
               ;; Unknown combination — fall through to standard
               (%gf-dispatch-standard gf args applicable))
              ;; Long-form combination: route to the long dispatcher, which
              ;; partitions APPLICABLE into method groups and runs the body.
              ((%mc-long-p mc)
               (%gf-dispatch-custom-long gf args applicable mc))
              (t
               (%gf-dispatch-custom gf args applicable mc msl))))
          ;; Standard method combination
          (%gf-dispatch-standard gf args applicable))))))

;;; ============================================================
;;; Runtime GF stub — for (eval `(defgeneric ...)) and friends.
;;;
;;; Build-time defgeneric expands to (defun NAME (&rest args)
;;; (%gf-dispatch 'NAME args)), creating a real compiled defun for the
;;; gf.  Runtime defgeneric (via eval) doesn't have the build-time
;;; compiler, so we instead install a closure that captures the gf-name
;;; in its env-list.  set-symbol-function on that closure makes
;;; (funcall sym ...) dispatch correctly through compile-funcall's
;;; native-sym-resolve → closure subtag #x52 path.
;;;
;;; 8-arg fixed arity (no &rest) avoids the documented
;;; captured-fn + &rest miscompile.  %get-nargs tells us how many
;;; were actually passed; we build an args list of that length and
;;; hand it to %gf-dispatch.
;;; ============================================================

;;; Registry of "anything that should typep as 'generic-function".
;;; Holds whatever objects (typically closures or raw fn-addrs) live in
;;; the symbol-function table for known GF names.  Without this,
;;; (typep #'foo 'generic-function) returns NIL because %generic-function-p
;;; only recognizes the underlying gf-object (4-slot array, %generic-function
;;; marker), not the dispatch wrapper that #'foo actually resolves to.
(defvar *gf-stub-closures* nil)
;; Reverse-lookup alist for COMPUTE-APPLICABLE-METHODS / FIND-METHOD
;; when called with #'name — maps fn-pointer → gf-name symbol so the
;; GF object can be resolved via %find-gf.
(defvar *gf-fn-to-name* nil)

(defun %register-gf-fn (val &optional gf-name)
  "Add VAL (a closure or raw fn-addr) to *gf-stub-closures* so
   %generic-function-p recognises it.  Idempotent — won't add duplicates.
   When GF-NAME is supplied (or recoverable from a closure's body),
   also record the fn → name mapping for reverse lookup."
  (unless (member val *gf-stub-closures*)
    (setq *gf-stub-closures* (cons val *gf-stub-closures*)))
  (when gf-name
    (let ((existing (assoc val *gf-fn-to-name*)))
      (when (null existing)
        (setq *gf-fn-to-name* (cons (cons val gf-name) *gf-fn-to-name*)))))
  val)

(defun %fn-to-gf (fn)
  "Resolve a function pointer / closure to its underlying GF object,
   or NIL if FN is not a registered GF dispatch stub."
  (let ((entry (assoc fn *gf-fn-to-name*)))
    (when entry
      (%find-gf (cdr entry)))))

(defun %make-gf-stub (gf-name)
  "Build a runtime gf-dispatch closure that captures GF-NAME.  Returns
   a closure (subtag #x52) suitable for set-symbol-function.

   Uses &rest because we need variable arity; this mirrors the
   build-time defgeneric expansion (defun NAME (&rest args)
   (%gf-dispatch 'NAME args)).  The build-time form embeds NAME as a
   literal — no capture — so it dodges the captured-fn + &rest
   miscompile that bites pure closures.  At runtime we have to capture
   GF-NAME, so we accept the slight risk; the dispatch is one bare
   funcall with no other captured-var reads in the body, which is the
   minimal exposure to that miscompile.

   Registers the stub in *gf-stub-closures* so %generic-function-p
   recognizes (typep #'gf-name 'generic-function) → T.

   Also records the reverse stub → gf-name mapping in *gf-fn-to-name*
   so COMPUTE-APPLICABLE-METHODS / FIND-METHOD / ADD-METHOD /
   REMOVE-METHOD can resolve the dispatch closure back to its GF
   (defgeneric.35 / find-method.lsp etc.)."
  (let ((stub (lambda (&rest args) (%gf-dispatch gf-name args))))
    (%register-gf-fn stub gf-name)
    stub))

;;; ============================================================
;;; call-next-method / next-method-p
;;; ============================================================

(defun %applicable-sets-eq (a b)
  "True if applicable-method lists A and B are the same ordered set of
   method records (element-wise EQ, same length).  Used to enforce CLHS
   7.6.6.2 in CALL-NEXT-METHOD."
  (let ((ca a) (cb b))
    (loop
      (cond
        ((and (null ca) (null cb)) (return t))
        ((or (null ca) (null cb))  (return nil))
        ((not (eq (car ca) (car cb))) (return nil))
        (t (setq ca (cdr ca)) (setq cb (cdr cb)))))))

(defun call-next-method (&rest new-args)
  "Call the next method in the applicable method list.
   Uses setq+save/restore around the inner call so the rebinding
   of *%next-methods* is visible to the called method's body
   (bare-metal LET on a defvar doesn't create dynamic scope).

   multiple-value-prog1 preserves the next method's MV state so
   (call-next-method) inside a chain of methods returning (values …)
   produces those values, not just the primary."
  ;; CLHS 7.6.6.2: when CALL-NEXT-METHOD is given replacement arguments,
  ;; the ordered set of applicable methods for the new arguments must be
  ;; the same as for the original generic-function arguments; otherwise an
  ;; error is signaled.  We recompute both applicable lists from the GF
  ;; recorded by the dispatcher (*%current-gf*) and compare them as ordered
  ;; method-record lists (EQ on the records — they come from the same GF's
  ;; method table, so identity is a sound ordered-set comparison).
  (when (and new-args *%current-gf* (%gf-p *%current-gf*))
    (let ((orig-set (%collect-applicable-methods *%current-gf* *%current-gf-args*))
          (new-set  (%collect-applicable-methods *%current-gf* new-args)))
      (unless (%applicable-sets-eq orig-set new-set)
        (error "call-next-method: changed arguments yield a different set of applicable methods"))))
  (let ((next *%next-methods*))
    (if (null next)
      (error "no next method")
      (let ((m (car next))
            (remaining (cdr next))
            (saved-nm *%next-methods*)
            (saved-args *%current-gf-args*))
        (let ((actual-args (if new-args new-args *%current-gf-args*)))
          (setq *%next-methods* remaining)
          (setq *%current-gf-args* actual-args)
          (multiple-value-prog1 (apply (%method-fn m) actual-args)
            (setq *%next-methods* saved-nm)
            (setq *%current-gf-args* saved-args)))))))

(defun next-method-p (&rest args)
  "True if there is a next method available.  NEXT-METHOD-P takes no
   arguments (CLHS 7.6.6.2); calling it with any is a program-error
   (next-method-p.error.1)."
  (when args (%signal-program-error))
  (not (null *%next-methods*)))

;;; ============================================================
;;; ensure-generic-function
;;; ============================================================

(defun %egf-plist-get (args key-name)
  "Scan keyword-arg plist ARGS for the keyword whose symbol-name is
   KEY-NAME (string), returning its value or NIL.  Keyword comparison is
   by symbol-name so it is robust regardless of symbol-object identity."
  (let ((cur args) (result nil) (found nil))
    (loop
      (when (or (null cur) (null (cdr cur))) (return nil))
      (let ((k (car cur)))
        (when (and (not found) (symbolp k) (not (null k))
                   (string= (symbol-name k) key-name))
          (setq result (car (cdr cur)))
          (setq found t)))
      (setq cur (cdr (cdr cur))))
    result))

(defun %egf-plist-has-p (args key-name)
  "True iff the keyword named KEY-NAME (string) appears in plist ARGS."
  (let ((cur args) (found nil))
    (loop
      (when (or (null cur) (null (cdr cur))) (return found))
      (let ((k (car cur)))
        (when (and (symbolp k) (not (null k))
                   (string= (symbol-name k) key-name))
          (setq found t) (return found)))
      (setq cur (cdr (cdr cur))))))

(defun ensure-generic-function (name &rest args)
  "Ensure generic function NAME exists.  ANSI: signals an error if NAME
   names a special operator, macro, or ordinary (non-generic) function.
   Returns the GF object.

   When NAME has no current GF, also installs a runtime dispatch stub
   via set-symbol-function so `(funcall (symbol-function NAME) …)` and
   `(typep #'NAME 'generic-function)` both work — EGF tests .4 and up
   probe `(symbol-function f)` after a fresh EGF call and expect a
   generic-function.

   Honors the :lambda-list and :argument-precedence-order keyword args:
   - On an EXISTING gf that already has methods, an incongruent
     :lambda-list (different required-arg count) signals an error
     (EGF.5/.6).
   - :argument-precedence-order is applied to the gf's dispatch order
     (EGF.8)."
  (let ((ll (%egf-plist-get args "LAMBDA-LIST"))
        (apo (%egf-plist-get args "ARGUMENT-PRECEDENCE-ORDER"))
        (ll-given (%egf-plist-has-p args "LAMBDA-LIST")))
    (let ((existing (%find-gf name)))
      (when existing
        ;; CLHS: a supplied :lambda-list must be congruent with the
        ;; existing gf (same required-arg count) when methods exist.
        (when ll-given
          (let ((decl-ll (%gf-lambda-list existing))
                (methods (%gf-methods existing)))
            (when (and decl-ll methods)
              (unless (= (%lambda-list-required-count ll)
                         (%lambda-list-required-count decl-ll))
                (%signal-program-error)))
            ;; Adopt the supplied lambda-list (CLHS allows reconfiguring).
            (%gf-set-lambda-list existing ll)))
        ;; Apply :argument-precedence-order to dispatch ordering.
        (when apo
          (let ((eff-ll (if ll ll (%gf-lambda-list existing))))
            (when eff-ll
              (%gf-set-arg-precedence name apo eff-ll))))
        ;; Return the same callable a DEFGENERIC for NAME returned (the
        ;; registered dispatch fn / stub) so (eqlt (defgeneric ...)
        ;; (ensure-generic-function name)) holds — EGF.7/9/10/12/13
        ;; compare with EQL.  Falls back to the GF struct when no
        ;; callable was registered.
        (return-from ensure-generic-function (%dg-gf-callable name)))
      ;; No GF yet — refuse if NAME is already bound to a non-GF function,
      ;; macro, or special operator.
      (cond
        ((and (symbolp name) (macro-function name))
         (error "ensure-generic-function: ~S names a macro" name))
        ((and (symbolp name) (special-operator-p name))
         (error "ensure-generic-function: ~S names a special operator" name))
        ((and (fboundp name)
              (not (%generic-function-p (fdefinition name))))
         (error "ensure-generic-function: ~S already names a non-generic function" name)))
      (let ((gf (%defgeneric name nil nil)))
        (set-symbol-function name (%make-gf-stub name))
        ;; Record a supplied :lambda-list so a later incongruent EGF call
        ;; (once methods exist) can be detected (EGF.5/.6).
        (when ll-given (%gf-set-lambda-list gf ll))
        (when (and apo ll) (%gf-set-arg-precedence name apo ll))
        ;; Same callable-return contract as the existing-GF branch.
        (let ((callable (%dg-gf-callable name)))
          (if callable callable gf))))))

;;; ============================================================
;;; find-method / remove-method / add-method
;;; ============================================================

(defun %spec-name (spec)
  "Resolve a specializer designator to its underlying name (or list for
   eql).  Accepts a class proxy, a CLOS class descriptor, or the bare
   name/list form."
  (cond
    ((%clos-class-p spec) (aref spec 1))
    ((%class-proxy-p spec) (aref spec 1))
    (t spec)))

(defun %spec-equal (a b)
  "Compare two method specializer designators by their resolved name."
  (let ((na (%spec-name a)) (nb (%spec-name b)))
    (cond
      ((eq na nb) t)
      ;; (eql VAL) specializers compare by val (eql)
      ((and (consp na) (consp nb)
            (eq (car na) 'eql) (eq (car nb) 'eql))
       (eql (cadr na) (cadr nb)))
      ;; Cross-file symbol identity dodge
      ((and (symbolp na) (symbolp nb)
            (not (null na)) (not (null nb))
            (not (eq na t)) (not (eq nb t))
            (= (aref na 0) (aref nb 0))) t)
      (t nil))))

(defun find-method (gf qualifiers specializers &rest args)
  "Find a method on GF.  ARGS is (errorp); when non-nil and no method
   matches, signal an error.  Accepts either the GF-array or the
   dispatch closure (#'name shape) — the latter resolves through
   *gf-fn-to-name*.

   CLHS: specializers length must match GF's required-parameter count.
   Validate against the GF's stored lambda-list when available
   (find-method.lsp 27115/27116)."
  ;; CLHS lambda-list is (gf qualifiers specializers &optional errorp): a
  ;; second trailing argument is a program-error (find-method.error.4).
  (when (and (consp args) (consp (cdr args)))
    (%signal-program-error))
  (let ((errorp (if args (car args) t)))
    ;; Resolve fn-pointer to real gf-array if needed.
    (unless (%gf-p gf)
      (let ((real (%fn-to-gf gf)))
        (when real (setq gf real))))
    (when (%gf-p gf)
      (let ((decl-ll (%gf-lambda-list gf)))
        (when decl-ll
          (let ((req-count (%lambda-list-required-count decl-ll)))
            (unless (= (length specializers) req-count)
              (%signal-program-error))))))
    (if (%gf-p gf)
      (let ((methods (%gf-methods gf))
            (result nil))
        (let ((cur methods))
          (loop
            (when (null cur) (return nil))
            (let ((m (car cur)))
              (let ((mq (%method-qualifier m))
                    (ms (%method-specializers m)))
                (let ((q-match (eq mq (if qualifiers (car qualifiers) nil)))
                      (s-match t))
                  (let ((s1 ms) (s2 specializers))
                    (loop
                      (when (and (null s1) (null s2)) (return nil))
                      (when (or (null s1) (null s2))
                        (setq s-match nil) (return nil))
                      (unless (%spec-equal (car s1) (car s2))
                        (setq s-match nil) (return nil))
                      (setq s1 (cdr s1))
                      (setq s2 (cdr s2))))
                  (when (and q-match s-match)
                    (setq result m)
                    (return nil)))))
            (setq cur (cdr cur))))
        (if (and (null result) errorp)
          (error "find-method: no matching method")
          result))
      (if errorp (error "find-method: not a generic function") nil))))

(defun remove-method (gf method)
  "Remove METHOD from GF.  Accepts either a GF-array or the dispatch
   closure / fn (#'name) via *gf-fn-to-name* reverse-lookup.  Returns
   the GF AS PASSED (CLHS: returns the generic function) — callers like
   remove-method.1 compare the return value with EQ against the
   designator they passed in."
  (let ((orig gf))
    (unless (%gf-p gf)
      (let ((real (%fn-to-gf gf)))
        (when real (setq gf real))))
    (when (%gf-p gf)
      (let ((new-methods nil)
            (cur (%gf-methods gf)))
        (loop
          (when (null cur) (return nil))
          (when (not (eq (car cur) method))
            (setq new-methods (cons (car cur) new-methods)))
          (setq cur (cdr cur)))
        (%gf-set-methods gf (nreverse new-methods))))
    orig))

(defun %method-meta-anywhere (method)
  "Find the key-acceptance meta (has-key has-rest aok keynames) for
   METHOD on ANY registered GF (the method may have been REMOVE-METHOD'd
   from its original GF, but its meta entry survives there).  Returns
   the meta list or NIL if unknown."
  (let ((cur *generic-functions*))
    (loop
      (when (null cur) (return nil))
      (let* ((gf (cdr (car cur)))
             (meta (%gf-method-meta-for gf method)))
        (when meta (return meta)))
      (setq cur (cdr cur)))))

(defun add-method (gf method)
  "Add METHOD to GF.  Accepts either a GF-array or the dispatch
   closure / fn (#'name) via *gf-fn-to-name* reverse-lookup.  Returns
   the GF AS PASSED (same contract as REMOVE-METHOD).

   CLHS ADD-METHOD signals an error when:
   - METHOD is already attached to another generic function
     (add-method.error.1);
   - METHOD's lambda-list is not congruent with GF's (CLHS 7.6.4):
     the specializer count must equal GF's required-parameter count
     (add-method.error.2/.3/.7), and if GF has neither &rest nor &key
     then METHOD must accept no &key either (add-method.error.8)."
  (let ((orig gf))
    (unless (%gf-p gf)
      (let ((real (%fn-to-gf gf)))
        (when real (setq gf real))))
    (when (%gf-p gf)
      ;; 1. Already on another GF?  method-generic-function walks the
      ;;    registry; if it finds a *different* GF holding METHOD, error.
      (let ((owner (method-generic-function method)))
        (when (and owner (not (eq owner gf)))
          (error "add-method: method already belongs to a generic function")))
      ;; 2. Congruence: specializer count must equal GF required count.
      (let ((decl-ll (%gf-lambda-list gf)))
        (when decl-ll
          (let ((req-count (%lambda-list-required-count decl-ll))
                (spec-count (length (%method-specializers method))))
            (unless (= spec-count req-count)
              (error "add-method: lambda lists not congruent (specializer count)"))
            ;; If GF accepts no &rest/&key, the method must not accept &key.
            (unless (%lambda-list-has-rest-or-key decl-ll)
              (let ((meta (%method-meta-anywhere method)))
                ;; meta = (has-key has-rest aok keynames)
                (when (and meta (or (nth 0 meta) (nth 1 meta)))
                  (error "add-method: lambda lists not congruent (&key/&rest)")))))))
      (%gf-set-methods gf (cons method (%gf-methods gf))))
    orig))

(defun method-qualifiers (m)
  "Return qualifiers of METHOD M."
  (let ((q (%method-qualifier m)))
    (if q (list q) nil)))

(defun method-specializers (m)
  "Return specializer class objects for METHOD M."
  (mapcar (lambda (s)
            (let ((cls (%find-clos-class s)))
              (or cls s)))
          (%method-specializers m)))

;;; MOP-ish method accessors that ANSI tests probe.
(defun method-function (m)
  "Return the function implementing METHOD M."
  (%method-fn m))

(defun method-generic-function (m)
  "Return the GF on which METHOD M is installed.  We don't store a
   back-pointer per method, so walk the GF registry looking for one
   whose method list contains M."
  (let ((cur *generic-functions*))
    (loop
      (when (null cur) (return nil))
      (let ((gf (cdr (car cur))))
        (when (member m (%gf-methods gf))
          (return gf)))
      (setq cur (cdr cur)))))

(defun method-lambda-list (m)
  "Return the lambda-list for METHOD M.  We don't separately track
   the lambda-list; reconstruct it from specializers."
  (let ((specs (%method-specializers m))
        (i 0))
    (mapcar (lambda (s)
              (declare (ignore s))
              (let ((sym (cond ((= i 0) 'arg0) ((= i 1) 'arg1)
                               ((= i 2) 'arg2) (t 'argn))))
                (setq i (+ i 1))
                sym))
            specs)))

;;; ============================================================
;;; compute-applicable-methods (public API)
;;; ============================================================

(defun compute-applicable-methods (gf args)
  "Return applicable methods for GF called with ARGS.
   Accepts either the gf-array (4-slot %generic-function descriptor)
   or the dispatch closure (#'gf-name shape) — the latter is resolved
   via *gf-fn-to-name* set up by %register-gf-fn."
  (cond
    ((%gf-p gf) (%collect-applicable-methods gf args))
    (t (let ((real-gf (%fn-to-gf gf)))
         (if real-gf
             (%collect-applicable-methods real-gf args)
             nil)))))

;;; ============================================================
;;; typep support for generic-function / standard-method
;;; ============================================================

(defun %generic-function-p (x)
  "True if X is a generic function — either the underlying gf-object
   (4-slot array, %generic-function marker in slot 0) or a registered
   gf-stub (closure or raw fn-addr in *gf-stub-closures*).

   The member check goes BEFORE any type-shape filtering: raw native
   function addresses end in even bits (NOP-aligned per CLAUDE.md to
   dodge funcall-tag collisions), so fixnump returns T on them and an
   earlier (not (fixnump x)) gate wrongly excluded all #'NAME results.
   Eq on the registry is safe regardless of type — for fixnum-shaped
   addresses it's just a value comparison."
  (cond
    ((%gf-p x) t)
    ((null x) nil)
    ((member x *gf-stub-closures*) t)
    (t nil)))

(defun %standard-method-p (x)
  "True if X is a standard method.  Method records have shape
   (qualifier specializers . fn) where fn is either a real function
   (functionp returns T) or an interp-closure (cons-tagged with
   car=%interp-closure — eval-defmethod produces these)."
  (if (or (fixnump x) (null x)) nil
    (if (consp x)
      (let ((q (car x)))
        (if (or (null q) (eq q :before) (eq q :after) (eq q :around)
                (symbolp q))
          (if (consp (cdr x))
            (let ((fn (cddr x)))
              (cond
                ((and (consp fn) (eq (car fn) '%interp-closure)) t)
                ((functionp fn) t)
                (t nil)))
            nil)
          nil))
      nil)))

;;; ============================================================
;;; Method-combination API + error signalers
;;; ============================================================

(defun find-method-combination (gf type-name options)
  "Look up the method-combination object named TYPE-NAME.  ANSI takes
   a GF arg (used to scope short-form lookups); we ignore it since
   method-combinations are stored in a single global registry."
  (declare (ignore gf options))
  (%find-mc type-name))

(defun method-combination-error (format-string &rest format-args)
  "Signal an error from inside a method-combination body.  Per CLHS,
   this is only meaningful inside DEFINE-METHOD-COMBINATION; outside
   it just signals a simple-error.  We accept it everywhere."
  (declare (ignore format-args))
  (error format-string))

(defun invalid-method-error (method format-string &rest format-args)
  "Signal an error indicating METHOD is invalid for its method
   combination."
  (declare (ignore method format-args))
  (error format-string))

(defun compute-applicable-methods-using-classes (gf classes)
  "AMOP variant of compute-applicable-methods that takes a list of
   class objects (or class names) instead of argument values.
   Returns (values methods definitive-p).  Modus matches by class
   name through the specializer; definitive-p is always T."
  (unless (%gf-p gf)
    (let ((real (%fn-to-gf gf)))
      (when real (setq gf real))))
  (if (%gf-p gf)
      (let* ((proxies (mapcar (lambda (c)
                                (let ((cls (cond ((symbolp c) (%find-clos-class c))
                                                 ((%clos-class-p c) c)
                                                 (t nil))))
                                  (if cls
                                      ;; Build a fake instance whose class slot is the class name
                                      ;; so %obj-cpl returns the right CPL.  We just need the
                                      ;; same shape — a 2-slot array with %clos-instance marker.
                                      (let ((fake (make-array 2)))
                                        (aset fake 0 '%clos-instance)
                                        (aset fake 1 (aref cls 1))
                                        fake)
                                      c)))
                              classes)))
        (values (%collect-applicable-methods gf proxies) t))
      (values nil t)))

;;; MAKE-METHOD / CALL-METHOD — used inside DEFINE-METHOD-COMBINATION
;;; bodies.  We support a minimal interpretation: MAKE-METHOD wraps a
;;; form into a method record; CALL-METHOD invokes one.

(defun make-method (form)
  "Wrap FORM in a method record that, when CALL-METHOD-invoked,
   evaluates FORM (no specialization).  ANSI uses this inside
   DEFINE-METHOD-COMBINATION body to fabricate methods."
  (cons nil (cons '(t) (lambda (&rest args)
                         (declare (ignore args))
                         (eval form)))))

(defun call-method (method &optional next-methods)
  "Invoke METHOD with no args, in a context where the next-methods
   chain is NEXT-METHODS.  Modus doesn't carry a method-context dynvar
   here, so call-next-method may not see the next chain — best-effort."
  (declare (ignore next-methods))
  (let ((fn (%method-fn method)))
    (funcall fn)))

(defun function-keywords (method)
  "Return (values keywords allow-other-keys-p) for METHOD's
   keyword-arg signature.  We don't track per-method keyword
   signatures, so report empty + NIL."
  (declare (ignore method))
  (values nil nil))

;;; MOP slot-access wrappers — read/write via class+slot-name.  Modus
;;; doesn't store slot-definition objects separately from slot-name
;;; symbols, so SLOT-NAME-OR-DEFINITION here is just the slot name.

(defun slot-value-using-class (class instance slot-name)
  "MOP entry point for slot read; defaults to %slot-value."
  (declare (ignore class))
  (%slot-value instance slot-name))

(defun (setf slot-value-using-class) (new-val class instance slot-name)
  (declare (ignore class))
  (set-slot-value instance slot-name new-val))

(defun slot-boundp-using-class (class instance slot-name)
  (declare (ignore class))
  (%slot-boundp instance slot-name))

(defun slot-makunbound-using-class (class instance slot-name)
  (declare (ignore class))
  (%slot-makunbound instance slot-name))

(defun slot-exists-p-using-class (class instance slot-name)
  (declare (ignore class))
  (%slot-exists-p instance slot-name))

;;; Slot-definition accessors — modus uses bare symbols as slot
;;; definitions, so most of these return the symbol unchanged.

(defun slot-definition-name (slot)         slot)
(defun slot-definition-allocation (slot)   (declare (ignore slot)) :instance)
(defun slot-definition-initargs (slot)     (declare (ignore slot)) nil)
(defun slot-definition-initform (slot)     (declare (ignore slot)) nil)
(defun slot-definition-initfunction (slot) (declare (ignore slot)) nil)
(defun slot-definition-readers (slot)      (declare (ignore slot)) nil)
(defun slot-definition-writers (slot)      (declare (ignore slot)) nil)
(defun slot-definition-type (slot)         (declare (ignore slot)) t)
(defun slot-definition-location (slot)
  "Return the integer location of SLOT in its class — modus doesn't
   precompute these, so callers that need a constant location must
   look it up via %clos-slot-index themselves."
  (declare (ignore slot))
  nil)

(defun nstring-parse-start-end (args len)
  "Parse :start/:end keyword args from ARGS plist. Returns (start . end).
   Per CLHS §3.4.1.4: an odd-length plist, an unknown keyword (with
   :allow-other-keys NIL/absent), or :end with a NIL-trailing pair
   signals PROGRAM-ERROR.  ANSI tests like NSTRING-UPCASE.ERROR.5
   call (NSTRING-UPCASE str :BAD T) expecting that to signal."
  (let ((start 0) (end len)
        (allow-other-keys nil) (aok-set nil))
    ;; First pass: probe for :allow-other-keys (CLHS §3.4.1.4.1.1.2:
    ;; leftmost wins).
    (let ((p args))
      (loop (when (null p) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other-keys (and (consp (cdr p)) (cadr p)))
          (setq aok-set t))
        (setq p (cdr p))))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        ;; Odd plist — keyword with no value.
        (when (null (cdr cur)) (%signal-program-error))
        (let ((key (car cur)) (val (cadr cur)))
          (cond
            ((eq key :start) (when val (setq start val)))
            ((eq key :end)   (when val (setq end val)))
            ((eq key :allow-other-keys) nil)
            (t (unless allow-other-keys (%signal-program-error)))))
        (setq cur (cddr cur))))
    (cons start end)))

(defun nstring-upcase-raw (str start end)
  "Internal: upcase chars in STR from START to END."
  (let ((i start))
    (loop
      (when (>= i end) (return str))
      (let ((ch (%ensure-char-code (aref str i))))   ; AREF→char; want raw code
        (when (lower-case-p (code-char ch))
          (aset str i (- ch 32))))
      (setq i (+ i 1)))))

(defun nstring-downcase-raw (str start end)
  "Internal: downcase chars in STR from START to END."
  (let ((i start))
    (loop
      (when (>= i end) (return str))
      (let ((ch (%ensure-char-code (aref str i))))   ; AREF→char; want raw code
        (when (upper-case-p (code-char ch))
          (aset str i (+ ch 32))))
      (setq i (+ i 1)))))

(defun nstring-capitalize-raw (str start end)
  "Internal: capitalize chars in STR from START to END."
  (let ((i start) (in-word nil))
    (loop
      (when (>= i end) (return str))
      (let ((ch (%ensure-char-code (aref str i))))   ; AREF→char; want raw code
        (if (alphanumericp (code-char ch))
            (if in-word
                (when (upper-case-p (code-char ch))
                  (aset str i (+ ch 32)))
                (progn
                  (when (lower-case-p (code-char ch))
                    (aset str i (- ch 32)))
                  (setq in-word t)))
            (setq in-word nil)))
      (setq i (+ i 1)))))

(defun nstring-upcase (str &rest args)
  "Destructive upcase — modifies STR in place, with :start/:end support."
  (if (array-wrapper-p str)
      (let ((elen (wrapper-effective-length str)))
        (let ((bounds (nstring-parse-start-end args elen)))
          (let ((start (car bounds)) (end (cdr bounds)))
            (if (fp-array-p str)
                (nstring-upcase-raw (cdr str) start end)
                (let ((offset (cdr (car str))))
                  (nstring-upcase-raw (cdr str) (+ offset start) (+ offset end))))))
        str)
      (let ((len (array-length str)))
        (let ((bounds (nstring-parse-start-end args len)))
          (nstring-upcase-raw str (car bounds) (cdr bounds))
          str))))

(defun nstring-downcase (str &rest args)
  "Destructive downcase — modifies STR in place, with :start/:end support."
  (if (array-wrapper-p str)
      (let ((elen (wrapper-effective-length str)))
        (let ((bounds (nstring-parse-start-end args elen)))
          (let ((start (car bounds)) (end (cdr bounds)))
            (if (fp-array-p str)
                (nstring-downcase-raw (cdr str) start end)
                (let ((offset (cdr (car str))))
                  (nstring-downcase-raw (cdr str) (+ offset start) (+ offset end))))))
        str)
      (let ((len (array-length str)))
        (let ((bounds (nstring-parse-start-end args len)))
          (nstring-downcase-raw str (car bounds) (cdr bounds))
          str))))

(defun nstring-capitalize (str &rest args)
  "Destructive capitalize — modifies STR in place, with :start/:end support."
  (if (array-wrapper-p str)
      (let ((elen (wrapper-effective-length str)))
        (let ((bounds (nstring-parse-start-end args elen)))
          (let ((start (car bounds)) (end (cdr bounds)))
            (if (fp-array-p str)
                (nstring-capitalize-raw (cdr str) start end)
                (let ((offset (cdr (car str))))
                  (nstring-capitalize-raw (cdr str) (+ offset start) (+ offset end))))))
        str)
      (let ((len (array-length str)))
        (let ((bounds (nstring-parse-start-end args len)))
          (nstring-capitalize-raw str (car bounds) (cdr bounds))
          str))))
;;; ============================================================
;;; Native multi-dim array (MDA) header — Phase 1 foundation
;;;
;;; Subtag #x34, 7 slots: [rank dims fp displaced-to disp-offset etype data]
;;;
;;; Replaces the cons-wrapper representations (9867654 / 8765432) used
;;; by the build-side rewrite-make-array-* passes.  Phase 1 just adds
;;; the object header + accessors + predicate, and makes the reader fns
;;; (array-rank / array-dimensions / array-dimension / array-total-size
;;; / arrayp) recognize it BEFORE falling through to the cons-wrapper
;;; path.  No make-array call produces these objects yet — that's Phase
;;; 2.  See project_multidim_arrays.md.
;;;
;;; Probes 56400+ exercise %alloc-mda + the accessors + reader-fn
;;; recognition.
;;; ============================================================

(defun %mda-p (x)
  "True iff X is a native multi-dim array header (subtag #x34).
   Note: DON'T early-out on (stringp x) — my new compile-prim-stringp
   recognizes MDA-with-string-data as a string, which would create
   mutual recursion (%mda-p → stringp → %mda-stringp → %mda-p).  The
   subtag check is fast and unambiguous; the leading non-object filters
   are enough to keep it cheap on the hot common-non-object path."
  (cond
    ((null x) nil)
    ((eq x t) nil)
    ((fixnump x) nil)
    ((consp x) nil)
    ((characterp x) nil)
    (t (= (obj-subtag x) #x34))))

;; All MDA accessors use %prim-aref / %prim-aset directly — they read
;; the HEADER slots of the MDA object, not the underlying data vector.
;; The generic aref/aset operators have an MDA fast path that redirects
;; through %mda-data, so using them here would create a recursion:
;; (%mda-data m) → (aref m 6) → MDA fast path → (%mda-data m) again.
(defun %mda-rank      (m) (%prim-aref m 0))
(defun %mda-dims      (m) (%prim-aref m 1))
(defun %mda-fp        (m) (%prim-aref m 2))
(defun %mda-displaced (m) (%prim-aref m 3))
(defun %mda-offset    (m) (%prim-aref m 4))
(defun %mda-etype     (m) (%prim-aref m 5))
(defun %mda-data      (m) (%prim-aref m 6))

(defun %mda-set-fp        (m v) (%prim-aset m 2 v))
(defun %mda-set-displaced (m v) (%prim-aset m 3 v))
(defun %mda-set-offset    (m v) (%prim-aset m 4 v))

(defun %mda-stringp (x)
  "Called from compile-stringp's emitted cond when the input is neither
   a cons-headed wrapper nor a primitive string.  Returns T iff X is a
   native MDA whose data slot is a string (char-element-typed).  This
   function is only called on guaranteed non-cons inputs."
  (cond
    ((%mda-p x) (%prim-stringp (%mda-data x)))
    (t nil)))

(defun %array-raw-length (arr)
  "Like array-length but ignores fill-pointer — returns the underlying
   storage capacity.  Used by array-in-bounds-p which per CLHS checks
   against the array's full dimension, not the fp-truncated length.
   For MDA, walks the dim list (NOT data slot — displaced MDAs share
   data with a larger underlying)."
  (cond
    ((%mda-p arr) (%array-raw-length-mda arr))
    ((consp arr) (%wrapper-array-length arr))
    (t (%prim-array-length arr))))

(defun %array-in-bounds-multi (arr indices)
  "Multi-subscript array-in-bounds-p.  For native MDA, walks dims
   against indices: each index must be a non-negative integer < its
   corresponding dimension.  For non-MDA, returns T (per the prior
   stub behavior — multi-sub on non-MDA isn't really meaningful)."
  (cond
    ((%mda-p arr)
     (let ((dims (%mda-dims arr))
           (subs indices)
           (ok t))
       (loop
         (when (or (null dims) (null subs) (not ok)) (return ok))
         (let ((sub (car subs)) (d (car dims)))
           (when (or (not (integerp sub)) (< sub 0) (>= sub d))
             (setq ok nil)))
         (setq dims (cdr dims))
         (setq subs (cdr subs)))
       ;; both dims and subs must be exhausted together
       (and ok (null dims) (null subs))))
    (t t)))

(defun %mda-array-length (x)
  "MDA-aware replacement for raw %prim-array-length called from
   compile-array-length on non-cons inputs.  For an MDA: fp first,
   else the MDA's own dim product (NOT the data slot's length —
   displaced MDAs share data with a larger underlying, so data
   length would over-iterate).  Non-MDA arrays return
   %prim-array-length verbatim."
  (cond
    ((%mda-p x)
     (let ((fp (%mda-fp x)))
       (cond
         (fp fp)
         (t (let ((dims (%mda-dims x)) (total 1))
              (loop (when (null dims) (return total))
                (setq total (* total (car dims)))
                (setq dims (cdr dims))))))))
    (t (%prim-array-length x))))

(defun %array-raw-length-mda (x)
  "Like %array-raw-length but resolves MDA dims (NOT fp, NOT data)
   — used inside %array-raw-length so that array-in-bounds-p sees
   the array's declared dim, not the underlying displaced storage."
  (let ((dims (%mda-dims x)) (total 1))
    (loop (when (null dims) (return total))
      (setq total (* total (car dims)))
      (setq dims (cdr dims)))))

(defun %alloc-mda (rank dims fp displaced offset etype data)
  "Allocate a native multi-dim array header object and fill all 7
   slots.  RANK is a non-negative integer (0 for scalar arrays), DIMS
   is the list of dimensions (NIL for 0-dim), FP is a fill-pointer or
   NIL, DISPLACED is the underlying array we're displaced to or NIL,
   OFFSET is the displacement offset (0 if not displaced), ETYPE is
   the element-type designator (T for general), DATA is the underlying
   1-D vector holding the flat storage in row-major order.

   Slot writes use %prim-aset to bypass the MDA fast path in
   compile-aset — otherwise this would recurse: the fast path checks
   (%mda-displaced m) which reads slot 3, but slot 3 is uninitialized
   here until our own aset writes it."
  (let ((m (%alloc-mda-raw)))
    (%prim-aset m 0 rank)
    (%prim-aset m 1 dims)
    (%prim-aset m 2 fp)
    (%prim-aset m 3 displaced)
    (%prim-aset m 4 offset)
    (%prim-aset m 5 etype)
    (%prim-aset m 6 data)
    m))

(defun %mda-row-major-index (dims subs)
  "Compute row-major linear index for SUBS against DIMS.  Returns
   the integer slot index in the underlying data vector.  Both DIMS
   and SUBS are lists of equal length; this is the inner of
   `(array-row-major-index a s1 s2 …)` and the helper that drives
   multi-subscript AREF/ASET on an MDA.

   Algorithm: for dims (D0 D1 D2 …), subs (s0 s1 s2 …), the index is
       s0 * (D1*D2*…) + s1 * (D2*…) + … + sN-1.
   Computed by walking dims and subs in parallel, with `tail-product`
   recomputed from a pass that pre-sums dim products."
  (let ((idx 0) (cur-d dims) (cur-s subs))
    (loop
      (when (null cur-d) (return idx))
      ;; tail-product = product of dims AFTER the current one
      (let ((tail 1) (rest (cdr cur-d)))
        (loop (when (null rest) (return nil))
          (setq tail (* tail (car rest)))
          (setq rest (cdr rest)))
        (setq idx (+ idx (* (car cur-s) tail))))
      (setq cur-d (cdr cur-d))
      (setq cur-s (cdr cur-s)))))

(defun %aref-multi (a &rest subs)
  "Multi-subscript AREF.  For an MDA (subtag #x34), compute row-major
   index from SUBS + the MDA's dims, then ref the underlying data
   vector (honouring displaced-to + offset if set).  Falls back to
   the existing single-sub paths (%wrapper-aref / %prim-aref) for
   non-MDA inputs."
  (cond
    ((%mda-p a)
     (let* ((dims (%mda-dims a))
            (idx (cond
                   ((null dims) 0)             ; 0-dim — single slot
                   ((null (cdr subs)) (car subs))  ; 1-D fast path
                   (t (%mda-row-major-index dims subs))))
            (disp (%mda-displaced a)))
       (if disp
           (let ((off (%mda-offset a)))
             (if (%mda-p disp)
                 (apply #'%aref-multi disp (+ idx off) nil)
                 (%prim-aref disp (+ idx off))))
           (%prim-aref (%mda-data a) idx))))
    ;; Non-MDA — single-sub semantic only.
    ((null (cdr subs))
     (if (consp a) (%wrapper-aref a (car subs)) (%prim-aref a (car subs))))
    ;; Multi-sub on a non-MDA: shouldn't normally happen.  Take the
    ;; first sub as the index (matches the historical pre-MDA
    ;; behavior where multi-sub forms got the trailing subs dropped).
    (t (if (consp a) (%wrapper-aref a (car subs)) (%prim-aref a (car subs))))))

(defun %aref-multi-public (a &rest subs)
  "Public multi-subscript AREF: like %aref-multi but lifts a string-typed
   element (raw char-CODE in the u8 store) to a CHARACTER, matching the
   single-subscript public AREF.  Used by compile-aref-form for the 0-sub
   and ≥2-sub cases.  Non-string arrays return the raw element unchanged."
  (let ((raw (apply #'%aref-multi a subs))
        ;; String-ness: native MDA with string data, cons-wrapped string,
        ;; or a plain string passed straight through %aref-multi.
        (str-p (cond
                 ((%mda-p a) (%prim-stringp (%mda-data a)))
                 ((consp a) (%wrapper-stringp a))
                 ((not (fixnump a)) (%prim-stringp a))
                 (t nil))))
    (if (and str-p (integerp raw)) (code-char raw) raw)))

(defun %aset-multi (a val &rest subs)
  "Multi-subscript ASET.  Value-first (val before subs) to make the
   &rest binding clean.  Compile-time dispatcher rearranges
   `(aset a i j val)` into `(%aset-multi a val i j)`.

   Returns VAL (matching aset's convention)."
  (cond
    ((%mda-p a)
     (let* ((dims (%mda-dims a))
            (idx (cond
                   ((null dims) 0)
                   ((null (cdr subs)) (car subs))
                   (t (%mda-row-major-index dims subs))))
            (disp (%mda-displaced a))
            ;; (setf (aref STRING-MDA …) ch): coerce a CHARACTER to its
            ;; char-CODE before the u8 store (mirrors single-sub ASET).
            (sto (if (and (characterp val) (%prim-stringp (%mda-data a)))
                     (char-code val) val)))
       (if disp
           (let ((off (%mda-offset a)))
             (if (%mda-p disp)
                 (apply #'%aset-multi disp sto (+ idx off) nil)
                 (%prim-aset disp (+ idx off) sto)))
           (%prim-aset (%mda-data a) idx sto))
       val))
    ;; Non-MDA single-sub.
    ((null (cdr subs))
     (let ((sto (%aset-store-val a val)))
       (if (consp a) (%wrapper-aset a (car subs) sto) (%prim-aset a (car subs) sto)))
     val)
    (t
     (let ((sto (%aset-store-val a val)))
       (if (consp a) (%wrapper-aset a (car subs) sto) (%prim-aset a (car subs) sto)))
     val)))

(defun array-dimension (a n)
  (cond
    ;; Native MDA: rank/dims are in the header.
    ((%mda-p a)
     (let ((dims (%mda-dims a)) (i 0))
       (loop
         (when (null dims) (return 0))
         (when (= i n) (return (car dims)))
         (setq dims (cdr dims))
         (setq i (+ i 1)))))
    (t
     (let ((a (if (and (consp a) (eql (car a) 8765432)) (cdr a) a)))
       (cond
         ((and (consp a) (eql (car a) 9867654) (consp (cdr a)))
          (let ((dims (cadr a)) (i 0))
            (loop
              (when (null dims) (return 0))
              (when (= i n) (return (car dims)))
              (setq dims (cdr dims))
              (setq i (+ i 1)))))
         ((and (consp a) (fixnump (car a)))
          ;; fill-pointer wrapper: dimension 0 is underlying length
          (if (= n 0) (array-length (cdr a)) 0))
         ((and (consp a) (consp (car a)))
          ;; displaced wrapper: dimension 0 is declared size
          (if (= n 0) (car (car a)) 0))
         ((= n 0) (array-length a))
         (t 0))))))
(defun array-total-size (a)
  ;; CLHS: signal TYPE-ERROR on non-array.
  (cond
    ((%mda-p a)
     ;; Walk dims (NOT data length — for displaced MDAs data slot
     ;; holds the displaced-to target which may be larger).
     (let ((dims (%mda-dims a)) (total 1))
       (loop (when (null dims) (return total))
         (setq total (* total (car dims)))
         (setq dims (cdr dims)))))
    (t
     (let ((a (if (and (consp a) (eql (car a) 8765432)) (cdr a) a)))
       (cond
         ((and (consp a) (eql (car a) 9867654) (consp (cdr a)))
          (array-length (cddr a)))
         ((and (consp a) (fixnump (car a)))
          (array-length (cdr a)))
         ((and (consp a) (consp (car a)))
          (car (car a)))
         ((stringp a) (array-length a))
         ((arrayp a) (array-length a))
         (t (error "ARRAY-TOTAL-SIZE: ~S is not an array" a)))))))
(defun array-rank (a)
  ;; CLHS: signal TYPE-ERROR if A is not an array (CLHS array-rank).
  ;; Without this guard, (array-rank NIL) silently returned 1 — defeats
  ;; the (handler-case ... (error (c) t)) tests in array-rank.lsp 19851/2.
  (cond
    ((%mda-p a) (%mda-rank a))
    ((and (consp a) (eql (car a) 8765432))
     (array-rank (cdr a)))
    ((and (consp a) (eql (car a) 9867654) (consp (cdr a)))
     (let ((n 0) (dims (cadr a)))
       (loop (when (null dims) (return n))
         (setq n (+ n 1))
         (setq dims (cdr dims)))))
    ((stringp a) 1)
    ((arrayp a) 1)
    (t (error "ARRAY-RANK: ~S is not an array" a))))
(defun adjustable-array-p (a)
  "True iff A was created with :adjustable t.  Detected by the outer
   marker (cons 8765432 ...) the build-ansi-test rewriter emits.
   Native MDA #x34: per CL we can return T (adjustability is allowed)
   but to match the suite's expectations we only return T for MDAs that
   have a fp or were created with explicit adjustable hint.  Pragmatic:
   any MDA is adjustable by construction (slot setters can mutate).

   CLHS: signals TYPE-ERROR on non-array input (adjustable-array-p.lsp
   tests 19722/3)."
  (cond
    ((%mda-p a) t)
    ((and (consp a) (eql (car a) 8765432)) t)
    ;; multi-dim wrapper or fp/displaced wrapper — treat as array
    ((and (consp a) (eql (car a) 9867654)) nil)
    ((stringp a) nil)
    ((arrayp a) nil)
    (t (error "ADJUSTABLE-ARRAY-P: ~S is not an array" a))))

(defun %unadj (a)
  "Peel the (cons 8765432 ...) adjustable wrapper if present."
  (if (and (consp a) (eql (car a) 8765432)) (cdr a) a))

(defun array-displacement (a)
  "Return (values displaced-to displaced-index-offset) or (values nil 0)."
  ;; Native MDA: read displaced-to + offset from header slots.
  (cond
    ((%mda-p a) (values (%mda-displaced a) (%mda-offset a)))
    (t (let ((a (%unadj a)))
         (if (and (consp a) (consp (car a)))
             (values (cdr a) (cdr (car a)))
             (values nil 0))))))

(defun %mda-nd-total (dims)
  "Product of DIMS (a dimension list)."
  (let ((total 1) (cur dims))
    (loop (when (null cur) (return total))
      (setq total (* total (car cur)))
      (setq cur (cdr cur)))))

(defun %mda-nd-index-in-bounds (subs dims)
  "T iff every sub in SUBS is < its corresponding dim in DIMS."
  (let ((s subs) (d dims) (ok t))
    (loop (when (or (null s) (null d) (not ok)) (return ok))
      (when (>= (car s) (car d)) (setq ok nil))
      (setq s (cdr s)) (setq d (cdr d)))))

(defun %mda-nd-next-subs (subs dims)
  "Increment the multi-subscript SUBS (a list) odometer-style against DIMS.
   Returns the next subs list, or NIL when the odometer wraps (done).
   Both SUBS and DIMS are same-length lists; least-significant is rightmost."
  (let ((rsubs (reverse subs)) (rdims (reverse dims))
        (carry 1) (out nil))
    (loop (when (null rsubs) (return nil))
      (let ((v (+ (car rsubs) carry)) (d (car rdims)))
        (if (>= v d)
            (progn (setq out (cons 0 out)) (setq carry 1))
            (progn (setq out (cons v out)) (setq carry 0))))
      (setq rsubs (cdr rsubs)) (setq rdims (cdr rdims)))
    (if (= carry 1) nil out)))

(defun %adjust-mda-nd (a new-dims args)
  "Adjust a rank≥2 MDA to NEW-DIMS, in place (eq holds — MDA headers are
   mutable).  Per CLHS, elements at multi-subscripts in-bounds for BOTH the
   old and new arrays are retained; the rest get :initial-element (or are
   left default).  Honors :initial-contents (row-major flatten) and
   :initial-element.  Displacement/fill-pointer on rank≥2 not handled."
  (let ((init-elem :unset) (init-contents :unset) (etype :unset)
        (cur args))
    (loop (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :initial-element)  (setq init-elem v))
          ((eq k :initial-contents) (setq init-contents v))
          ((eq k :element-type)     (setq etype v))))
      (setq cur (cddr cur)))
    (let* ((old-dims (%mda-dims a))
           (old-data (%mda-data a))
           (str-data (stringp old-data))
           (new-total (%mda-nd-total new-dims))
           (sz (if (= new-total 0) 1 new-total))
           (new-data (cond
                       ((and (not (eq etype :unset))
                             (or (eq etype 'character) (eq etype 'base-char)))
                        (%make-string-array sz))
                       (str-data (%make-string-array sz))
                       (t (make-array sz)))))
      (cond
        ;; :initial-contents — flatten nested lists row-major into new-data.
        ((not (eq init-contents :unset))
         (%mda-fill-contents-flat new-data init-contents new-dims))
        (t
         ;; Walk every new multi-subscript; copy old value when in-bounds for
         ;; old, else fill with init-elem (when given).
         (let ((subs nil) (nd new-dims))
           ;; initialise subs to all-zeros of new rank
           (loop (when (null nd) (return nil))
             (setq subs (cons 0 subs)) (setq nd (cdr nd)))
           (setq subs (reverse subs))
           (when (> new-total 0)
             (let ((ni 0))
               (loop
                 (let ((val (cond
                              ((%mda-nd-index-in-bounds subs old-dims)
                               (apply #'%aref-multi a subs))
                              ((not (eq init-elem :unset))
                               (if (and str-data (characterp init-elem))
                                   (char-code init-elem) init-elem))
                              (t (if str-data 0 nil)))))
                   (aset new-data ni val))
                 (setq ni (+ ni 1))
                 (let ((nxt (%mda-nd-next-subs subs new-dims)))
                   (when (null nxt) (return nil))
                   (setq subs nxt))))))))
      ;; Commit: data ← new-data, dims ← new-dims, rank ← length, drop
      ;; displacement (we materialised fresh storage).
      (%prim-aset a 6 new-data)
      (%prim-aset a 1 new-dims)
      (%prim-aset a 0 (length new-dims))
      (%prim-aset a 3 nil)
      (%prim-aset a 4 0)
      a)))

(defun %adjust-mda-1d (a new-size args)
  "Adjust a rank-1 MDA in place: realloc data when growing, copy old
   contents, honor :initial-element / :initial-contents / :fill-pointer
   / :displaced-to.  Returns A (eq holds — MDAs are always adjustable
   in our impl since the header is mutable)."
  (let ((displaced-to nil) (displaced-offset 0)
        (fp-arg :unset) (init-elem :unset) (init-contents :unset)
        (cur args))
    (loop (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :displaced-to)            (setq displaced-to v))
          ((eq k :displaced-index-offset)  (setq displaced-offset v))
          ((eq k :fill-pointer)            (setq fp-arg v))
          ((eq k :initial-element)         (setq init-elem v))
          ((eq k :initial-contents)        (setq init-contents v))))
      (setq cur (cddr cur)))
    (let* ((old-data (%mda-data a))
           ;; Logical length / displacement offset of the array's CURRENT
           ;; view.  For a displaced source, old-data is the displaced-to
           ;; target and we must apply %mda-offset when copying, reading at
           ;; most the source array's own declared dim-0 (not the larger
           ;; target's length).  adjust-array.11 / .adjustable.11.
           (old-off  (if (%mda-displaced a) (%mda-offset a) 0))
           (old-dim0 (let ((d (%mda-dims a))) (if (consp d) (car d) (array-length old-data))))
           (old-len  (array-length old-data))
           (str-data (or (stringp old-data)
                         (and (%mda-displaced a) (stringp (%mda-displaced a)))))
           ;; Allocate new data of exactly new-size (preserve elt type).
           (new-data (cond
                       (displaced-to nil)
                       (str-data (%make-string-array new-size))
                       (t (make-array new-size)))))
      (cond
        (displaced-to
         (%prim-aset a 3 displaced-to)
         (%prim-aset a 4 displaced-offset))
        ((not (eq init-contents :unset))
         (let ((i 0) (lst init-contents))
           (loop (when (>= i new-size) (return))
             (when (consp lst)
               (aset new-data i (car lst))
               (setq lst (cdr lst)))
             (setq i (+ i 1))))
         (%prim-aset a 6 new-data)
         (%prim-aset a 3 nil) (%prim-aset a 4 0))
        ((not (eq init-elem :unset))
         (let ((i 0)
               (store (if (and str-data (characterp init-elem))
                          (char-code init-elem) init-elem)))
           (loop (when (>= i new-size) (return))
             (if (< i old-dim0)
                 (aset new-data i (aref old-data (+ old-off i)))
                 (aset new-data i store))
             (setq i (+ i 1))))
         (%prim-aset a 6 new-data)
         (%prim-aset a 3 nil) (%prim-aset a 4 0))
        (t
         (let ((i 0))
           (loop (when (>= i new-size) (return))
             (when (< i old-dim0)
               (aset new-data i (aref old-data (+ old-off i))))
             (setq i (+ i 1))))
         (%prim-aset a 6 new-data)
         (%prim-aset a 3 nil) (%prim-aset a 4 0)))
      ;; Update dims slot to reflect the new size (rank stays 1).
      (%prim-aset a 1 (list new-size))
      ;; Update fp per CLHS adjust-array.  :fill-pointer nil / unsupplied
      ;; RETAINS the array's existing fill pointer (clamped to new size) —
      ;; it does NOT clear it.  t sets fp to new size.  An integer sets it
      ;; explicitly.  adjust-array.6 / .adjustable.6: (adjust-array fp3-array
      ;; 4 :fill-pointer nil) keeps fp = 3.
      (let ((old-fp (%mda-fp a)))
        (cond
          ((eq fp-arg t) (%mda-set-fp a new-size))
          ((or (eq fp-arg :unset) (null fp-arg))
           ;; Retain existing fp, clamped so it never exceeds new size.
           (when old-fp
             (%mda-set-fp a (if (> old-fp new-size) new-size old-fp))))
          (t (%mda-set-fp a fp-arg))))
      a)))

(defun adjust-array (a new-size &rest args)
  "Return an array of NEW-SIZE with elements from A.
   When A is adjustable (outer (cons 8765432 ...)), we modify A in place so
   (eq a (adjust-array a ...)) holds, as required by ANSI for adjustable
   arrays.  For non-adjustable A, return a fresh array.
   Handles :displaced-to, :displaced-index-offset, :fill-pointer,
   :initial-element, :initial-contents.

   Native MDA: always-adjustable.  Rank 0/1 → %adjust-mda-1d; rank ≥ 2 →
   %adjust-mda-nd (reshape dims, preserve common multi-indices per CLHS).
   "
  ;; Multi-dim MDA: NEW-SIZE is a dims list with length ≥ 2 — must be
  ;; handled BEFORE the (car new-size) collapse below (which would drop
  ;; all but dim 0).  adjust-array.21/22/23.  Only plain reshape (no
  ;; displaced-to) is supported.
  (when (and (%mda-p a) (consp new-size) (consp (cdr new-size))
             (>= (%mda-rank a) 2)
             (not (member :displaced-to args)))
    (return-from adjust-array (%adjust-mda-nd a new-size args)))
  (when (consp new-size) (setq new-size (car new-size)))
  (when (%mda-p a)
    (let ((rank (%mda-rank a)))
      (when (or (= rank 0) (= rank 1))
        (return-from adjust-array (%adjust-mda-1d a new-size args)))))
  (let* ((displaced-to nil)
         (displaced-offset 0)
         (fp-arg :unset)
         (init-elem :unset)
         (init-contents :unset)
         (cur args))
    ;; Parse keyword args
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :displaced-to)            (setq displaced-to v)       (setq cur (cddr cur)))
          ((eq k :displaced-index-offset)  (setq displaced-offset v)   (setq cur (cddr cur)))
          ((eq k :fill-pointer)            (setq fp-arg v)             (setq cur (cddr cur)))
          ((eq k :initial-element)         (setq init-elem v)          (setq cur (cddr cur)))
          ((eq k :initial-contents)        (setq init-contents v)      (setq cur (cddr cur)))
          (t                               (setq cur (cddr cur))))))
    (let* ((adj-p (and (consp a) (eql (car a) 8765432)))
           (inner (if adj-p (cdr a) a))
           (had-fp (and (consp inner) (fixnump (car inner))))
           ;; Compute the source plain array & its current length / offset.
           (src-arr (cond
                      ((and (consp inner) (consp (car inner))) (cdr inner))   ; displaced
                      ((consp inner) (cdr inner))                              ; fp wrapper
                      (t inner)))
           (src-offset (cond
                         ((and (consp inner) (consp (car inner))) (cdr (car inner)))
                         (t 0)))
           (src-len (array-length src-arr))
           (string-elt (stringp src-arr)))
      ;; Build the new flat array contents.
      (let ((new-arr (cond
                       (displaced-to
                        ;; Displaced: don't allocate; just point at base
                        nil)
                       ((and (eq init-contents :unset) (eq init-elem :unset))
                        ;; Copy first min(new-size, src-len) from src
                        (let ((nb (if string-elt
                                      (%make-string-array new-size)
                                      (make-array new-size)))
                              (i 0))
                          (loop
                            (when (>= i new-size) (return nb))
                            (let ((si (+ src-offset i)))
                              (when (< si src-len)
                                (aset nb i (aref src-arr si))))
                            (setq i (+ i 1)))))
                       ((not (eq init-elem :unset))
                        (let ((nb (if string-elt
                                      (%make-string-array new-size)
                                      (make-array new-size)))
                              (i 0))
                          (loop
                            (when (>= i new-size) (return nb))
                            (let ((si (+ src-offset i)))
                              (if (< si src-len)
                                  (aset nb i (aref src-arr si))
                                  (aset nb i init-elem)))
                            (setq i (+ i 1)))))
                       ((not (eq init-contents :unset))
                        (let ((nb (if string-elt
                                      (%make-string-array new-size)
                                      (make-array new-size))))
                          ;; Walk init-contents and aset
                          (let ((i 0) (cur init-contents))
                            (loop
                              (when (>= i new-size) (return nb))
                              (when (consp cur)
                                (aset nb i (car cur))
                                (setq cur (cdr cur)))
                              (setq i (+ i 1)))
                            nb))))))
        ;; Build the inner shape (fp-wrapper, displaced-wrapper, or plain).
        (let ((new-inner
               (cond
                 (displaced-to
                  (cons (cons new-size displaced-offset) displaced-to))
                 (had-fp
                  ;; preserve / update fill pointer
                  (let ((new-fp (cond
                                  ((eq fp-arg :unset) (if (< (car inner) new-size) (car inner) new-size))
                                  ((eq fp-arg t) new-size)
                                  ((null fp-arg) (if (< (car inner) new-size) (car inner) new-size))
                                  (t fp-arg))))
                    (cons new-fp new-arr)))
                 ((and (not (eq fp-arg :unset)) fp-arg (not (eq fp-arg nil)))
                  ;; new fill pointer
                  (cons (if (eq fp-arg t) new-size fp-arg) new-arr))
                 (t new-arr))))
          (cond
            (adj-p
             ;; In-place update: mutate the outer cons so EQ holds.
             (set-cdr a new-inner)
             a)
            (t
             ;; Non-adjustable: return a fresh value.
             new-inner)))))))

(defun row-major-aref (a idx) (aref a idx))
(defun set-row-major-aref (a idx val) (aset a idx val) val)
(defun char-type-error-check (fn x) nil)
(defun fp-array-p (x)
  "Check if x is a fill-pointer array wrapper (cons fixnum string).
   Peels (cons 8765432 ...) adjustable layer first."
  (let ((y (%unadj x)))
    (if (consp y)
        (if (fixnump (car y))
            (stringp (cdr y))
            nil)
        nil)))
(defun displaced-array-p (x)
  "Check if x is a displaced array wrapper (cons (cons :displaced offset) string)."
  (let ((y (%unadj x)))
    (if (consp y)
        (if (consp (car y))
            (stringp (cdr y))
            nil)
        nil)))
(defun array-wrapper-p (x)
  "Check if x is a fill-pointer or displaced array wrapper.
   Accepts wrappers around strings AND general arrays."
  (let ((y (%unadj x)))
    (if (consp y)
        (let ((cdr-y (cdr y)))
          (cond
            ((stringp cdr-y) t)
            ((%prim-arrayp cdr-y) t)
            (t nil)))
        nil)))
(defun wrapper-effective-length (w)
  "Get effective length of a fill-pointer or displaced array wrapper."
  (let ((y (%unadj w)))
    (if (fixnump (car y))
        (car y)   ; fill-pointer
        (car (car y)))))
(defun wrapper-offset (w)
  "Get offset for displaced array wrapper, 0 for fill-pointer."
  (let ((y (%unadj w)))
    (if (fixnump (car y)) 0 (cdr (car y)))))
(defun wrapper-aref (w i)
  "AREF on a fill-pointer or displaced array wrapper."
  (let ((y (%unadj w)))
    (aref (cdr y) (+ (wrapper-offset y) i))))
(defun wrapper-aset (w i val)
  "ASET on a fill-pointer or displaced array wrapper."
  (let ((y (%unadj w)))
    (aset (cdr y) (+ (wrapper-offset y) i) val)))

;;; ===========================================================================
;;; %WRAPPER-AREF / %WRAPPER-ASET / %WRAPPER-ARRAY-LENGTH
;;;
;;; Universal wrapper-peeling helpers — handle every array-wrapper variant
;;; (adjustable / fp / displaced / multi-dim).  Used by the wrapper-aware
;;; AREF/ASET/ARRAY-LENGTH front-ends in compiler.lisp; they emit a
;;; (consp arr) test that routes to these helpers when the input is a
;;; cons-wrapped array.
;;;
;;; Each helper accepts ANY value (cons or not) and falls back to the
;;; primitive op via %prim-aref / %prim-aset / %prim-array-length when the
;;; input is not a wrapper.  This makes them safe to call directly from
;;; user code that passes mixed-shape arrays.
;;; ===========================================================================

;; Disambiguate a (cons fp underlying) or (cons (cons size off) underlying)
;; wrapper from an ordinary list by checking that CDR (chain) points at a
;; real array or string within one cons hop.  This is what keeps
;; `(aref '(1 2 3) 0)` etc. from spuriously routing into the wrapper path.
;;
;; Bounded at one cons hop (O(1), not O(list-length)) — recursing through a
;; long list to reject it would blow the stack on `(length (make-list N))`
;; for large N.  Adjustable wrappers (car == 8765432) are tested before
;; this helper is reached, so the deepest legitimate shape we need to
;; recognise is one cons (fp or displaced) over array/string.
(defun %cdr-is-array-or-wrapper-p (x)
  ;; Use %prim-arrayp internally to avoid recursion through arrayp →
  ;; %wrapper-arrayp → here on every cons cell encountered.
  (let ((u (cdr x)))
    (cond
      ((null u) nil)
      ((%prim-stringp u) t)
      ((%prim-arrayp u) t)
      ((not (consp u)) nil)
      ;; one cons deep — recognise nested wrapper terminating in array/string
      (t (let ((uu (cdr u)))
           (cond
             ((null uu) nil)
             ((%prim-stringp uu) t)
             ((%prim-arrayp uu) t)
             (t nil)))))))

(defun %wrapper-arrayp (w)
  "Return T if W is a wrapper around an array (multi-dim, 0-dim,
   adjustable, fill-pointer, displaced).  Uses %prim-arrayp internally
   to avoid recursion through arrayp."
  (cond
    ((eql (car w) 8765432) t)
    ((and (eql (car w) 9867654) (consp (cdr w))) t)
    ((and (fixnump (car w)) (%cdr-is-array-or-wrapper-p w)) t)
    ((and (consp (car w)) (%cdr-is-array-or-wrapper-p w)) t)
    (t nil)))

(defun %wrapper-aref (w i)
  (cond
    ;; adjustable wrapper: peel and recurse
    ((eql (car w) 8765432)
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-aref u i) (%prim-aref u i))))
    ;; multi-dim: index into flat backing array
    ((and (eql (car w) 9867654) (consp (cdr w)))
     (%prim-aref (cddr w) i))
    ;; fp-wrapper: aref underlying
    ((and (fixnump (car w)) (%cdr-is-array-or-wrapper-p w))
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-aref u i) (%prim-aref u i))))
    ;; displaced wrapper: aref underlying at offset+i
    ((and (consp (car w)) (%cdr-is-array-or-wrapper-p w))
     (let ((u (cdr w)) (off (cdr (car w))))
       (if (consp u)
           (%wrapper-aref u (+ off i))
           (%prim-aref u (+ off i)))))
    ;; not a wrapper — fall back to primitive (caller passed a real list etc.)
    (t (%prim-aref w i))))

(defun %wrapper-aset (w i val)
  (cond
    ((eql (car w) 8765432)
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-aset u i val) (%prim-aset u i val))))
    ((and (eql (car w) 9867654) (consp (cdr w)))
     (%prim-aset (cddr w) i val))
    ((and (fixnump (car w)) (%cdr-is-array-or-wrapper-p w))
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-aset u i val) (%prim-aset u i val))))
    ((and (consp (car w)) (%cdr-is-array-or-wrapper-p w))
     (let ((u (cdr w)) (off (cdr (car w))))
       (if (consp u)
           (%wrapper-aset u (+ off i) val)
           (%prim-aset u (+ off i) val))))
    (t (%prim-aset w i val))))

;;; %wrapper-array-length: ARRAY-LENGTH semantics — returns the underlying
;;; storage size (NOT the fill-pointer).  ARRAY-IN-BOUNDS-P relies on this
;;; matching the actual array dimension (e.g. fp 5 in length-10 backing →
;;; in-bounds for indices 0..9).
(defun %wrapper-array-length (w)
  (cond
    ((eql (car w) 8765432)
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-array-length u) (%prim-array-length u))))
    ((and (eql (car w) 9867654) (consp (cdr w)))
     (%prim-array-length (cddr w)))
    ((and (fixnump (car w)) (%cdr-is-array-or-wrapper-p w))
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-array-length u) (%prim-array-length u))))
    ((and (consp (car w)) (%cdr-is-array-or-wrapper-p w))
     ;; displaced wrapper: declared SIZE is car of car
     (car (car w)))
    (t (%prim-array-length w))))

;;; %wrapper-stringp: STRINGP semantics — true iff the underlying storage is
;;; a string.  Handles all wrapper variants by recursing through the layers.
(defun %wrapper-stringp (w)
  (cond
    ((eql (car w) 8765432)
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-stringp u) (%prim-stringp u))))
    ((and (eql (car w) 9867654) (consp (cdr w)))
     (%prim-stringp (cddr w)))
    ((and (fixnump (car w)) (%cdr-is-array-or-wrapper-p w))
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-stringp u) (%prim-stringp u))))
    ((and (consp (car w)) (%cdr-is-array-or-wrapper-p w))
     (let ((u (cdr w)))
       (if (consp u) (%wrapper-stringp u) (%prim-stringp u))))
    (t nil)))
(defun copy-seq (seq)
  (cond
    ((null seq) nil)
    ((consp seq)
     (if (array-wrapper-p seq)
         (let ((len (wrapper-effective-length seq)))
           (let ((r (%make-string-array len)))
             (dotimes (i len r) (aset r i (wrapper-aref seq i)))))
         (copy-list seq)))
    ((stringp seq)
     (let ((r (%make-string-array (length seq)))) (dotimes (i (length seq) r) (aset r i (aref seq i)))))
    (t (let ((r (make-array (length seq)))) (dotimes (i (length seq) r) (aset r i (aref seq i)))))))
(defun sqrt (n)
  "Square root.  Returns:
   - exact integer for perfect-square integer input,
   - rational approximation otherwise (Newton's method on n/1
     with 10000:1 precision scaling — gives ~4 digit precision).
   - For rational/float input %r/d% computes sqrt(r*d)/d scaled."
  (cond
    ((integerp n)
     (when (< n 0) (error "sqrt of negative"))
     (let ((s (isqrt n)))
       (if (= (* s s) n)
           s
           ;; Newton's method on scaled value: sqrt(n*K^2)/K for precision.
           (let* ((K 10000)
                  (scaled (* n K K))
                  (approx (isqrt scaled)))
             (%make-rat approx K)))))
    ((ratiop n)
     (let* ((num (ratio-numerator n))
            (den (ratio-denominator n)))
       (when (< num 0) (error "sqrt of negative"))
       ;; sqrt(a/b) = sqrt(a*b)/b for b>0.
       (let ((s (isqrt (* num den))))
         (if (= (* s s) (* num den))
             (%make-rat s den)
             (let* ((K 10000)
                    (approx (isqrt (* num den K K))))
               (%make-rat approx (* den K)))))))
    ;; Boxed float-as-array (subtag #x32 with 2 slots, slot0=num slot1=den):
    ;; treat as ratio and recurse.
    ((and (not (fixnump n)) (not (consp n)) (not (null n))
          (= (obj-subtag n) #x32)
          (= (array-length n) 2))
     (let ((num (aref n 0)) (den (aref n 1)))
       (sqrt (if (= den 1) num (%make-rat num den)))))
    ;; IEEE float input — Newton's method via %float-mul / -add / -div.
    ;; Converges in ~4 iterations for typical inputs.  Without this
    ;; branch (sqrt 4.0) fell through to (t 0).
    ((%ieee-float-p n)
     (cond
       ((%float-zero-p n) n)
       (t (let ((half (%float-div (%float-from-int 1) (%float-from-int 2)))
                (x n))
            ;; x_{i+1} = (x_i + n/x_i) / 2
            (let ((i 0))
              (loop
                (when (>= i 8) (return x))
                (setq x (%float-mul (%float-add x (%float-div n x)) half))
                (setq i (+ i 1))))
            x))))
    (t 0)))
(defun %aset-store-val (arr val)
  "Compute the raw value to store via ASET for destination array ARR.
   Public (setf (aref STRING i) ch) accepts a CHARACTER; the underlying
   u8 store holds char-CODES, so coerce a character VAL to its code when
   ARR is string-typed (plain, cons-wrapped, or native MDA with string
   data).  Non-string destinations — and non-character values (raw codes
   from internal callers) — store unchanged.  Mirror of compile-aref's
   code→char lift on the read side."
  (if (characterp val)
      (cond
        ((consp arr) (if (%wrapper-stringp arr) (char-code val) val))
        ((%mda-p arr) (if (%prim-stringp (%mda-data arr)) (char-code val) val))
        ((%prim-stringp arr) (char-code val))
        (t val))
      val))

(defun set-char (str idx ch) (aset str idx (char-code ch)) ch)
(defun set-subseq (seq start &rest rest)
  "Destructively replace seq[start..end] with val.  Returns seq.
   Per CLHS, this is the SETF expansion of subseq with two arities:
     (set-subseq seq start val)         ; SETF (SUBSEQ seq start)
     (set-subseq seq start end val)     ; SETF (SUBSEQ seq start end)
   The compiler's SETF macro emits the longer form for compound place
   but the shorter form for 2-arg place — handle both via &rest."
  (let* ((end (if (null (cdr rest)) nil (car rest)))
         (val (if (null (cdr rest)) (car rest) (cadr rest)))
         (seq-len (length seq))
         (effective-end (if end (if (< end seq-len) end seq-len) seq-len))
         (val-len (length val))
         (copy-len (min (- effective-end start) val-len))
         (i 0))
    (loop
      (when (>= i copy-len) (return seq))
      (setf (elt seq (+ start i)) (elt val i))
      (setq i (+ i 1)))))
(defun is-ordered-by (pred) (lambda (x y) (funcall pred x y)))
;; NTH-VALUE — compile-time macro at compiler.lisp:847 handles direct
;; call; this runtime defun is for #'NTH-VALUE / funcall-on-symbol.
;; Reads N'th MV slot via mem-ref.
(defun nth-value (n form)
  (cond
    ((= n 0) form)
    ;; FORM has already been evaluated, MV slots populated.  Read from
    ;; the MV-values area at 0x10000098+.
    ((<= n 15)
     (let ((mv-count (ash (mem-ref #x10000090 :u64) -1)))
       (if (>= n mv-count)
           nil
           (let ((bits (mem-ref (+ #x10000098 (* n 8)) :u64)))
             (ash bits -1)))))
    (t nil)))
(defun copy-symbol (sym &optional copy-props)
  "Create a fresh uninterned symbol with the same name as SYM.
   Per CLHS 12.7.5, the new symbol has no function/value bindings;
   if COPY-PROPS is true the property list is shared (modus has no
   symbol plists, so this is a no-op)."
  (declare (ignore copy-props))
  (cond
    ((null sym) (make-symbol "NIL"))
    ((eq sym t) (make-symbol "T"))
    (t
     (let ((name (cond ((symbolp sym) (symbol-name sym))
                       ((stringp sym) sym)
                       (t "G"))))
       (make-symbol name)))))
;; realpart / imagpart / conjugate / complexp live in cl-sequences.lisp
;; with the proper 3-slot complex array recognition.  The stubs that
;; were here unconditionally returned z / 0 and shadowed the real
;; implementations via last-defun-wins.
(defun numerator (r) r)
(defun denominator (r) 1)
(defun float (n &optional proto)
  "Convert N to a float (CLHS).  With no PROTO: an existing float is
   returned unchanged, otherwise N becomes a SINGLE-FLOAT (the CLHS
   default).  With PROTO: the result has PROTO's float format."
  (let ((f (cond
             ((%ieee-float-p n) n)
             ((integerp n) (%float-from-int n))
             ((ratiop n)
              (%float-div (%float-from-int (aref n 0))
                          (%float-from-int (aref n 1))))
             (t n))))
    (cond
      ((not (%ieee-float-p f)) f)
      (proto (if (double-float-p proto)
                 (%make-typed-float (aref f 0) (aref f 1) 'double-float)
                 (%round-to-single f)))
      ;; No prototype: keep an existing float's format; coerce others to single.
      ((%ieee-float-p n) n)
      (t (%round-to-single f)))))
(defun rational (n) n)
(defun rationalize (n) n)

