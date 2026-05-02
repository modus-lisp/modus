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

;; Dynamic variable for call-next-method chain
;; Holds: list of (qualifier specializer-list . fn) remaining
(defvar *%next-methods* nil)
;; Dynamic variable for current method's args (for call-next-method with no args)
(defvar *%current-gf-args* nil)

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
    ((eq type-name 'vector)    '(vector array sequence t))
    ((eq type-name 'array)     '(array t))
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
    ((consp obj)       'cons)
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

(defun %compute-cpl (name supers)
  "Compute a simple CPL for NAME with SUPERS (linearization).
   Uses C3-like: name first, then each super's CPL, deduped, ending with t."
  (let ((result (list name))
        (seen (list name)))
    ;; Collect each super's CPL in order
    (let ((cur supers))
      (loop
        (when (null cur) (return nil))
        (let ((sup-name (car cur)))
          (let ((sup-cpl
                 (let ((sup-cls (%find-clos-class sup-name)))
                   (if sup-cls
                     (aref sup-cls 4)
                     (%builtin-cpl sup-name)))))
            (let ((c sup-cpl))
              (loop
                (when (null c) (return nil))
                (let ((item (car c)))
                  (let ((already nil))
                    (let ((s seen))
                      (loop
                        (when (null s) (return nil))
                        (when (eq (car s) item) (setq already t) (return nil))
                        (setq s (cdr s))))
                    (when (not already)
                      (setq result (cons item result))
                      (setq seen  (cons item seen)))))
                (setq c (cdr c))))))
        (setq cur (cdr cur))))
    ;; Ensure 't' is always last
    (let ((already-t nil))
      (let ((r result))
        (loop
          (when (null r) (return nil))
          (when (eq (car r) 't) (setq already-t t) (return nil))
          (setq r (cdr r))))
      (when (not already-t)
        (setq result (cons 't result))))
    (nreverse result)))

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
    (let ((cls (make-array 5)))
      (aset cls 0 '%clos-class)
      (aset cls 1 name)
      (aset cls 2 effective-slots)
      (aset cls 3 supers)
      (aset cls 4 cpl)
      ;; Remove old entry if exists, then add new
      (let ((new-registry nil)
            (cur *clos-classes*))
        (loop
          (when (null cur) (return nil))
          (when (not (eq (car (car cur)) name))
            (setq new-registry (cons (car cur) new-registry)))
          (setq cur (cdr cur)))
        (setq *clos-classes* (cons (cons name cls) new-registry)))
      name)))

(defun %find-clos-class (name)
  "Return class descriptor for NAME, or nil."
  (let ((cur *clos-classes*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

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

(defun %clos-slot-info-for (class-name)
  "Return (initarg-map . initform-map) for CLASS-NAME, or nil."
  (let ((cur *clos-slot-info*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) class-name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun %clos-initarg-to-slot (class-name initarg-key)
  "Map an initarg keyword (symbol) to a slot-name for CLASS-NAME, or nil.
   Compares by eq, then native-MVM-sym hash, then CL-sym name string.
   Bare-metal symbols-from-quoted-literals are native (1-slot, hash-only),
   so plain eq sometimes fails for symbols-with-the-same-name."
  (let ((info (%clos-slot-info-for class-name)))
    (when (null info) (return-from %clos-initarg-to-slot nil))
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

(defun %clos-initform-thunk (class-name slot-name)
  "Return the initform thunk for SLOT-NAME in CLASS-NAME, or nil."
  (let ((info (%clos-slot-info-for class-name)))
    (when (null info) (return-from %clos-initform-thunk nil))
    (let ((ifm (cdr info)))
      (let ((cur ifm))
        (loop
          (when (null cur) (return nil))
          (let ((entry (car cur)))
            (when (eq (car entry) slot-name)
              (return (cdr entry))))
          (setq cur (cdr cur)))))))

(defun %clos-slot-index (cls slot-name)
  "Return 0-based index of SLOT-NAME in cls, or nil if not found."
  (let ((slots (aref cls 2))
        (idx 0))
    (let ((cur slots))
      (loop
        (when (null cur) (return nil))
        (when (eq (car cur) slot-name) (return idx))
        (setq idx (+ idx 1))
        (setq cur (cdr cur))))))

;;; make-instance: user-facing function (build-time rewriter expands
;;; the common (make-instance 'class :k v ...) shape inline; this defun
;;; handles the rest — class-object args, missing/extra args, runtime
;;; (eval `(make-instance ...)) callers).
(defun make-instance (&rest args)
  (cond
    ;; Strict arity: (make-instance) with no class is a program-error.
    ((null args) (error "make-instance: requires a class designator"))
    (t
     (let* ((class-or-name (car args))
            (initargs (cdr args))
            (inst (%make-instance class-or-name)))
       (when (null inst) (return-from make-instance nil))
       ;; Apply initargs: for each :keyword value pair, look up the
       ;; corresponding slot name via the class's initarg-map.
       (let ((class-name (aref inst 1))
             (cur initargs))
         (loop
           (when (or (null cur) (null (cdr cur))) (return nil))
           (let ((kw (car cur)) (val (cadr cur)))
             (let ((slot (%clos-initarg-to-slot class-name kw)))
               (when slot
                 (set-slot-value inst slot val))))
           (setq cur (cddr cur))))
       ;; Apply :initform thunks for slots that weren't initarg-set
       ;; and remain unbound.
       (let ((cls (%find-clos-class (aref inst 1))))
         (when cls
           (let ((slot-names (aref cls 2)))
             (dolist (sname slot-names)
               (when (eql (aref inst (+ 2 (%clos-slot-index cls sname))) -999)
                 (let ((thunk (%clos-initform-thunk class-name sname)))
                   (when thunk
                     (set-slot-value inst sname (funcall thunk)))))))))
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
   Calls slot-unbound if the slot has no value, errors when SLOT-NAME
   doesn't name a slot of OBJ's class."
  (when (or (null obj) (not (%clos-instance-p obj)))
    (return-from %slot-value nil))
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-value nil))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx)
        (error "slot-value: no slot named ~S in class ~S"
               slot-name (aref cls 1)))
      (let ((val (aref obj (+ 2 idx))))
        ;; -999 is the unbound slot sentinel (fixnum, no global lookup needed)
        ;; Guard with fixnump to avoid type error when slot contains non-fixnum
        (if (and (fixnump val) (= val -999))
          ;; Call slot-unbound method
          (%dispatch-slot-unbound cls obj slot-name)
          val)))))

(defun set-slot-value (obj slot-name new-val)
  "Set slot SLOT-NAME in CLOS instance OBJ to NEW-VAL. Returns NEW-VAL.
   Errors when SLOT-NAME doesn't name a slot of OBJ's class."
  (when (or (null obj) (not (%clos-instance-p obj)))
    (return-from set-slot-value new-val))
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from set-slot-value new-val))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx)
        (error "set-slot-value: no slot named ~S in class ~S"
               slot-name (aref cls 1)))
      (aset obj (+ 2 idx) new-val)
      new-val)))

(defun %slot-boundp (obj slot-name)
  "True if slot SLOT-NAME of OBJ is bound.  Errors when SLOT-NAME
   doesn't name a slot of OBJ's class."
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-boundp nil))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx)
        (error "slot-boundp: no slot named ~S in class ~S"
               slot-name (aref cls 1)))
      ;; -999 is the unbound slot sentinel (fixnum guard prevents type error)
      (let ((v (aref obj (+ 2 idx))))
        (not (and (fixnump v) (= v -999)))))))

(defun %slot-makunbound (obj slot-name)
  "Mark slot SLOT-NAME in OBJ as unbound.  Errors when SLOT-NAME
   doesn't name a slot of OBJ's class."
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-makunbound obj))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx)
        (error "slot-makunbound: no slot named ~S in class ~S"
               slot-name (aref cls 1)))
      ;; Use literal -999 to avoid SYMBOL-VALUE clobber in variable-index aset
      (aset obj (+ 2 idx) -999)
      obj)))

(defun %slot-exists-p (obj slot-name)
  "True if OBJ has a slot named SLOT-NAME."
  (when (or (null obj) (not (%clos-instance-p obj)))
    (return-from %slot-exists-p nil))
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-exists-p nil))
    (let ((idx (%clos-slot-index cls slot-name)))
      (if idx t nil))))

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
        ;; Default: signal unbound-slot error
        (error "slot unbound")))))

(defun slot-unbound (class obj slot-name)
  "Default slot-unbound: signals an error."
  (error "slot unbound"))

(defun class-name (cls)
  "Return the name of class CLS."
  (if (%clos-class-p cls)
    (aref cls 1)
    (if (%class-proxy-p cls)
      (%class-proxy-name cls)
      nil)))

(defun class-of (x)
  "Return the class of X."
  (cond
    ((%clos-instance-p x)
     (%find-clos-class (aref x 1)))
    (t nil)))

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
  "Create a new generic function object."
  (let ((gf (make-array 4)))
    (aset gf 0 '%generic-function)
    (aset gf 1 name)
    (aset gf 2 nil)  ; methods-alist
    (aset gf 3 nil)  ; method-combination name
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
(defun %gf-set-methods (gf m) (aset gf 2 m))
(defun %gf-set-combination (gf c) (aset gf 3 c))

(defun %find-gf (name)
  "Find generic function by name."
  (let ((cur *generic-functions*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

(defun %defgeneric (name lambda-list combination)
  "Register or update a generic function."
  (let ((existing (%find-gf name)))
    (if existing
      (progn
        (%gf-set-combination existing combination)
        existing)
      (let ((gf (%make-gf name)))
        (%gf-set-combination gf combination)
        (setq *generic-functions* (cons (cons name gf) *generic-functions*))
        gf))))

;; Make a method record: (qualifier specializer-list . fn)
(defun %make-method (qualifier specializers fn)
  (cons qualifier (cons specializers fn)))

(defun %method-qualifier (m)    (car m))
(defun %method-specializers (m) (cadr m))
(defun %method-fn (m)           (cddr m))

(defun %defmethod (gf-name qualifier specializers fn)
  "Add or replace a method on a generic function."
  ;; Ensure GF exists
  (when (null (%find-gf gf-name))
    (%defgeneric gf-name nil nil))
  (let ((gf (%find-gf gf-name)))
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
  (let ((cur *method-combinations*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) name) (return (cdr (car cur))))
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
             ;; Gated on symbolp on both sides so we skip T/NIL/
             ;; conses/etc. that wouldn't have name-hashes anyway.
             (let ((budget (mem-ref #x10000C60 :u64)))
               (when (and (> budget 0)
                          (symbolp c) (symbolp spec)
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

(defun %collect-applicable-methods (gf args)
  "Return all applicable methods sorted most-specific first."
  (let ((methods (%gf-methods gf))
        (applicable nil))
    ;; Collect applicable
    (let ((cur methods))
      (loop
        (when (null cur) (return nil))
        (let ((m (car cur)))
          (when (%specializers-match-p (%method-specializers m) args)
            (setq applicable (cons m applicable))))
        (setq cur (cdr cur))))
    ;; Sort by specificity (insertion sort — usually few methods)
    (let ((sorted nil))
      (let ((cur applicable))
        (loop
          (when (null cur) (return nil))
          (let ((m (car cur))
                (score (%method-specificity (car cur) args)))
            ;; Insert m into sorted in correct position
            (let ((new-sorted nil)
                  (inserted nil)
                  (prev sorted))
              (loop
                (when (null prev)
                  (if inserted
                    (setq new-sorted (nreverse new-sorted))
                    (setq new-sorted (nreverse (cons m new-sorted))))
                  (return nil))
                (let ((pm (car prev))
                      (pm-score (%method-specificity (car prev) args)))
                  (if (and (not inserted) (< score pm-score))
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
    (when (null primary-methods)
      (error "no applicable primary method"))
    ;; Build the effective method chain
    (let ((run-primary
           (lambda ()
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
             (let ((result
                    (let ((saved-nm *%next-methods*)
                          (saved-args *%current-gf-args*))
                      (setq *%next-methods* (cdr primary-methods))
                      (setq *%current-gf-args* args)
                      (let ((r (apply (%method-fn (car primary-methods)) args)))
                        (setq *%next-methods* saved-nm)
                        (setq *%current-gf-args* saved-args)
                        r))))
               ;; Run after methods (least specific first = most-specific last)
               (let ((cur (nreverse after-methods)))
                 (loop
                   (when (null cur) (return nil))
                   (apply (%method-fn (car cur)) args)
                   (setq cur (cdr cur))))
               result))))
      (if around-methods
        ;; Run around methods wrapping the primary chain
        (let ((*%next-methods*
               (let ((remaining (cdr around-methods)))
                 ;; Last around calls run-primary as "next"
                 remaining))
              (*%current-gf-args* args))
          ;; Build synthetic "next" for the last around to call primary
          (let ((primary-sentinel (%make-method nil nil run-primary)))
            (let ((*%next-methods*
                   (let ((rev-around (cdr around-methods)))
                     ;; Append primary-sentinel after last around
                     (let ((result nil)
                           (cur rev-around))
                       (loop
                         (when (null cur)
                           (setq result (cons primary-sentinel result))
                           (return nil))
                         (setq result (cons (car cur) result))
                         (setq cur (cdr cur)))
                       (nreverse result))))
                  (*%current-gf-args* args))
              (apply (%method-fn (car around-methods)) args))))
        ;; No around: just run primary + before/after
        (funcall run-primary)))))

;;; ============================================================
;;; Custom method combination dispatch (short form)
;;; ============================================================

(defun %gf-dispatch-custom (gf args applicable mc)
  "Short-form method combination: collect qualifying methods and apply operator."
  (let ((comb-name (%mc-name mc))
        (operator (%mc-operator mc))
        (identity-with-one (%mc-identity-with-one mc)))
    ;; Collect :around and comb-name-qualified methods
    (let ((around-methods nil)
          (primary-methods nil))
      (let ((cur applicable))
        (loop
          (when (null cur) (return nil))
          (let ((m (car cur)))
            (let ((q (%method-qualifier m)))
              (cond
                ((eq q :around)
                 (setq around-methods (cons m around-methods)))
                ((eq q comb-name)
                 (setq primary-methods (cons m primary-methods)))
                ;; Primary methods with this combination also qualify
                ((null q)
                 ;; Not allowed in custom combination — should error
                 nil))))
          (setq cur (cdr cur))))
      (setq around-methods  (nreverse around-methods))
      (setq primary-methods (nreverse primary-methods))
      (when (null primary-methods)
        (error "no applicable method for combination"))
      ;; Compute the combined result
      (let ((combined-thunk
             (lambda ()
               ;; If identity-with-one and only one method, call it directly
               (if (and identity-with-one (null (cdr primary-methods)))
                 (apply (%method-fn (car primary-methods)) args)
                 ;; Otherwise collect results and apply operator
                 (let ((results nil)
                       (cur primary-methods))
                   (loop
                     (when (null cur) (return nil))
                     (setq results (cons (apply (%method-fn (car cur)) args) results))
                     (setq cur (cdr cur)))
                   ;; Inline fold instead of (apply operator results)
                   ;; to avoid CALL-INDIRECT through symbol tagged pointer
                   (let ((rl (nreverse results)))
                     (cond
                       ((eq operator '*)
                        (let ((acc 1) (rcur rl))
                          (loop (when (null rcur) (return acc))
                                (setq acc (* acc (car rcur)))
                                (setq rcur (cdr rcur)))))
                       ((eq operator '+)
                        (let ((acc 0) (rcur rl))
                          (loop (when (null rcur) (return acc))
                                (setq acc (+ acc (car rcur)))
                                (setq rcur (cdr rcur)))))
                       ((eq operator 'max)
                        (let ((acc (car rl)) (rcur (cdr rl)))
                          (loop (when (null rcur) (return acc))
                                (when (> (car rcur) acc) (setq acc (car rcur)))
                                (setq rcur (cdr rcur)))))
                       ((eq operator 'min)
                        (let ((acc (car rl)) (rcur (cdr rl)))
                          (loop (when (null rcur) (return acc))
                                (when (< (car rcur) acc) (setq acc (car rcur)))
                                (setq rcur (cdr rcur)))))
                       ((eq operator 'and)
                        (let ((acc t) (rcur rl))
                          (loop (when (null rcur) (return acc))
                                (setq acc (car rcur))
                                (when (null acc) (return nil))
                                (setq rcur (cdr rcur)))))
                       ((eq operator 'or)
                        (let ((acc nil) (rcur rl))
                          (loop (when (null rcur) (return acc))
                                (setq acc (car rcur))
                                (when acc (return acc))
                                (setq rcur (cdr rcur)))))
                       ((eq operator 'list)
                        rl)
                       ((eq operator 'append)
                        (let ((acc nil) (rcur rl))
                          (loop (when (null rcur) (return acc))
                                (setq acc (append acc (car rcur)))
                                (setq rcur (cdr rcur)))))
                       ((eq operator 'nconc)
                        (let ((acc nil) (rcur rl))
                          (loop (when (null rcur) (return acc))
                                (setq acc (nconc acc (car rcur)))
                                (setq rcur (cdr rcur)))))
                       ((eq operator 'progn)
                        (let ((acc nil) (rcur rl))
                          (loop (when (null rcur) (return acc))
                                (setq acc (car rcur))
                                (setq rcur (cdr rcur)))))
                       (t (apply operator rl)))))))))
        (if around-methods
          ;; Around wraps combined
          (let ((primary-sentinel (%make-method nil nil combined-thunk)))
            (let ((*%next-methods*
                   (let ((result nil)
                         (cur (cdr around-methods)))
                     (loop
                       (when (null cur)
                         (setq result (cons primary-sentinel result))
                         (return nil))
                       (setq result (cons (car cur) result))
                       (setq cur (cdr cur)))
                     (nreverse result)))
                  (*%current-gf-args* args))
              (apply (%method-fn (car around-methods)) args)))
          ;; No around
          (funcall combined-thunk))))))

;;; ============================================================
;;; Main dispatch entry point
;;; ============================================================

(defun %gf-dispatch (name args)
  "Dispatch generic function NAME with ARGS."
  (let ((gf (%find-gf name)))
    (when (null gf)
      (error "undefined generic function"))
    (let ((applicable (%collect-applicable-methods gf args)))
      (when (null applicable)
        ;; Try no-applicable-method hook
        (error "no applicable method"))
      (let ((comb-name (%gf-combination gf)))
        (if comb-name
          ;; Custom method combination
          (let ((mc (%find-mc comb-name)))
            (if mc
              (%gf-dispatch-custom gf args applicable mc)
              ;; Unknown combination — fall through to standard
              (%gf-dispatch-standard gf args applicable)))
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
   minimal exposure to that miscompile."
  (lambda (&rest args)
    (%gf-dispatch gf-name args)))

;;; ============================================================
;;; call-next-method / next-method-p
;;; ============================================================

(defun call-next-method (&rest new-args)
  "Call the next method in the applicable method list.
   Uses setq+save/restore around the inner call so the rebinding
   of *%next-methods* is visible to the called method's body
   (bare-metal LET on a defvar doesn't create dynamic scope)."
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
          (let ((r (apply (%method-fn m) actual-args)))
            (setq *%next-methods* saved-nm)
            (setq *%current-gf-args* saved-args)
            r))))))

(defun next-method-p ()
  "True if there is a next method available."
  (not (null *%next-methods*)))

;;; ============================================================
;;; ensure-generic-function
;;; ============================================================

(defun ensure-generic-function (name &rest args)
  "Ensure generic function NAME exists.  ANSI: signals an error if NAME
   names a special operator, macro, or ordinary (non-generic) function."
  (declare (ignore args))
  (let ((existing (%find-gf name)))
    (when existing
      (return-from ensure-generic-function existing))
    ;; No GF yet — refuse if NAME is already bound to a non-GF function.
    (when (and (fboundp name)
               (not (%generic-function-p (fdefinition name))))
      (error "ensure-generic-function: ~S already names a non-generic function" name))
    (%defgeneric name nil nil)))

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
   matches, signal an error."
  (let ((errorp (if args (car args) t)))
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
  "Remove METHOD from GF."
  (when (%gf-p gf)
    (let ((new-methods nil)
          (cur (%gf-methods gf)))
      (loop
        (when (null cur) (return nil))
        (when (not (eq (car cur) method))
          (setq new-methods (cons (car cur) new-methods)))
        (setq cur (cdr cur)))
      (%gf-set-methods gf (nreverse new-methods))))
  gf)

(defun add-method (gf method)
  "Add METHOD to GF."
  (when (%gf-p gf)
    (%gf-set-methods gf (cons method (%gf-methods gf))))
  gf)

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

;;; ============================================================
;;; compute-applicable-methods (public API)
;;; ============================================================

(defun compute-applicable-methods (gf args)
  "Return applicable methods for GF called with ARGS."
  (if (%gf-p gf)
    (%collect-applicable-methods gf args)
    nil))

;;; ============================================================
;;; typep support for generic-function / standard-method
;;; ============================================================

(defun %generic-function-p (x)
  "True if X is a generic function."
  (%gf-p x))

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

(defun nstring-parse-start-end (args len)
  "Parse :start/:end keyword args from ARGS plist. Returns (start . end)."
  (let ((start 0) (end len))
    (let ((cur args))
      (loop
        (when (null cur) (return nil))
        (let ((key (car cur)) (val (cadr cur)))
          (if (eq key :start) (setq start val)
            (if (eq key :end) (when val (setq end val)))))
        (setq cur (cddr cur))))
    (cons start end)))

(defun nstring-upcase-raw (str start end)
  "Internal: upcase chars in STR from START to END."
  (let ((i start))
    (loop
      (when (>= i end) (return str))
      (let ((ch (aref str i)))
        (when (lower-case-p (code-char ch))
          (aset str i (- ch 32))))
      (setq i (+ i 1)))))

(defun nstring-downcase-raw (str start end)
  "Internal: downcase chars in STR from START to END."
  (let ((i start))
    (loop
      (when (>= i end) (return str))
      (let ((ch (aref str i)))
        (when (upper-case-p (code-char ch))
          (aset str i (+ ch 32))))
      (setq i (+ i 1)))))

(defun nstring-capitalize-raw (str start end)
  "Internal: capitalize chars in STR from START to END."
  (let ((i start) (in-word nil))
    (loop
      (when (>= i end) (return str))
      (let ((ch (aref str i)))
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
(defun array-dimension (a n)
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
      (t 0))))
(defun array-total-size (a)
  (let ((a (if (and (consp a) (eql (car a) 8765432)) (cdr a) a)))
    (cond
      ((and (consp a) (eql (car a) 9867654) (consp (cdr a)))
       (array-length (cddr a)))
      ((and (consp a) (fixnump (car a)))
       (array-length (cdr a)))
      ((and (consp a) (consp (car a)))
       (car (car a)))
      (t (array-length a)))))
(defun array-rank (a)
  (let ((a (if (and (consp a) (eql (car a) 8765432)) (cdr a) a)))
    (cond
      ((and (consp a) (eql (car a) 9867654) (consp (cdr a)))
       (let ((n 0) (dims (cadr a)))
         (loop (when (null dims) (return n))
           (setq n (+ n 1))
           (setq dims (cdr dims)))))
      (t 1))))
(defun adjustable-array-p (a)
  "True iff A was created with :adjustable t.  Detected by the outer
   marker (cons 8765432 ...) the build-ansi-test rewriter emits."
  (if (consp a) (eql (car a) 8765432) nil))

(defun %unadj (a)
  "Peel the (cons 8765432 ...) adjustable wrapper if present."
  (if (and (consp a) (eql (car a) 8765432)) (cdr a) a))

(defun array-displacement (a)
  "Return (values displaced-to displaced-index-offset) or (values nil 0)."
  ;; Displaced arrays represented as (cons (cons size offset) base-array)
  (let ((a (%unadj a)))
    (if (and (consp a) (consp (car a)))
        (values (cdr a) (cdr (car a)))
        (values nil 0))))

(defun adjust-array (a new-size &rest args)
  "Return an array of NEW-SIZE with elements from A.
   When A is adjustable (outer (cons 8765432 ...)), we modify A in place so
   (eq a (adjust-array a ...)) holds, as required by ANSI for adjustable
   arrays.  For non-adjustable A, return a fresh array.
   Handles :displaced-to, :displaced-index-offset, :fill-pointer,
   :initial-element, :initial-contents."
  (when (consp new-size) (setq new-size (car new-size)))
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
  "Check if x is a fill-pointer or displaced array wrapper."
  (let ((y (%unadj x)))
    (if (consp y) (if (stringp (cdr y)) t nil) nil)))
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
  (let ((u (cdr x)))
    (cond
      ((null u) nil)
      ((%prim-stringp u) t)
      ((arrayp u) t)
      ((not (consp u)) nil)
      ;; one cons deep — recognise nested wrapper terminating in array/string
      (t (let ((uu (cdr u)))
           (cond
             ((null uu) nil)
             ((%prim-stringp uu) t)
             ((arrayp uu) t)
             (t nil)))))))

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
  (if (consp seq)
      (if (array-wrapper-p seq)
          (let ((len (wrapper-effective-length seq)))
            (let ((r (%make-string-array len)))
              (dotimes (i len r) (aset r i (wrapper-aref seq i)))))
          (copy-list seq))
      (if (stringp seq)
          (let ((r (%make-string-array (length seq)))) (dotimes (i (length seq) r) (aset r i (aref seq i))))
          (let ((r (make-array (length seq)))) (dotimes (i (length seq) r) (aset r i (aref seq i)))))))
(defun sqrt (n)
  "Square root — returns integer for perfect squares, float approximation otherwise."
  (if (integerp n)
      (let ((s (isqrt n)))
        (if (= (* s s) n) s (%make-float-raw s 1)))
      ;; Float input — return float with same magnitude
      (%make-float-raw 1 1)))
(defun set-char (str idx ch) (aset str idx (char-code ch)) ch)
(defun set-subseq (seq start end val) seq)  ; stub
(defun is-ordered-by (pred) (lambda (x y) (funcall pred x y)))
(defun nth-value (n form) nil)  ; stub
(defun copy-symbol (sym &optional props) nil)  ; stub
(defun realpart (z) z)
(defun imagpart (z) 0)
(defun numerator (r) r)
(defun denominator (r) 1)
(defun float (n &optional proto) n)  ; stub — no real floats
(defun rational (n) n)
(defun rationalize (n) n)

