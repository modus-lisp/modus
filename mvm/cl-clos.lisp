;;;; cl-clos.lisp — Minimal CLOS implementation
;;;; Part of the Modus CL runtime. Depends on cl-eval.lisp.

;;; ===================================================
;;; Minimal CLOS implementation for ANSI test suite
;;; ===================================================

;; Class registry: alist of (class-name . cls-array)
;; cls-array: #(%clos-class name slot-names-list)
(defvar *clos-classes* nil)

;; slot-unbound methods: list of (class-name slot-spec fn)
;; slot-spec: nil = any slot; symbol = that specific slot name
(defvar *slot-unbound-methods* nil)

;; Unbound slot sentinel: fixnum -999.
;; Using a fixnum literal avoids SYMBOL-VALUE call clobbering arr-reg
;; in variable-index aset during %make-instance initialization loop.
(defvar *%unbound-slot* -999)

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

(defun %defclass (name slot-names)
  "Register CLOS class NAME with SLOT-NAMES list."
  (let ((cls (make-array 3)))
    (aset cls 0 '%clos-class)
    (aset cls 1 name)
    (aset cls 2 slot-names)
    (setq *clos-classes* (cons (cons name cls) *clos-classes*))
    name))

(defun %find-clos-class (name)
  "Return class descriptor for NAME, or nil."
  (let ((cur *clos-classes*))
    (loop
      (when (null cur) (return nil))
      (when (eq (car (car cur)) name) (return (cdr (car cur))))
      (setq cur (cdr cur)))))

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

(defun %make-instance (class-name)
  "Allocate a new CLOS instance of CLASS-NAME with all slots unbound.
   Initargs are handled at build time by the SBCL-side rewriter."
  (let ((cls (%find-clos-class class-name)))
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
   Calls slot-unbound if the slot has no value."
  (when (or (null obj) (not (%clos-instance-p obj)))
    (return-from %slot-value nil))
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-value nil))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx) (return-from %slot-value nil))
      (let ((val (aref obj (+ 2 idx))))
        ;; -999 is the unbound slot sentinel (fixnum, no global lookup needed)
        ;; Guard with fixnump to avoid type error when slot contains non-fixnum
        (if (and (fixnump val) (= val -999))
          ;; Call slot-unbound method
          (%dispatch-slot-unbound cls obj slot-name)
          val)))))

(defun set-slot-value (obj slot-name new-val)
  "Set slot SLOT-NAME in CLOS instance OBJ to NEW-VAL. Returns NEW-VAL."
  (when (or (null obj) (not (%clos-instance-p obj)))
    (return-from set-slot-value new-val))
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from set-slot-value new-val))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx) (return-from set-slot-value new-val))
      (aset obj (+ 2 idx) new-val)
      new-val)))

(defun %slot-boundp (obj slot-name)
  "True if slot SLOT-NAME of OBJ is bound."
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-boundp nil))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx) (return-from %slot-boundp nil))
      ;; -999 is the unbound slot sentinel (fixnum guard prevents type error)
      (let ((v (aref obj (+ 2 idx))))
        (not (and (fixnump v) (= v -999)))))))

(defun %slot-makunbound (obj slot-name)
  "Mark slot SLOT-NAME in OBJ as unbound."
  (let ((cls (%find-clos-class (aref obj 1))))
    (when (null cls) (return-from %slot-makunbound obj))
    (let ((idx (%clos-slot-index cls slot-name)))
      (when (null idx) (return-from %slot-makunbound obj))
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
(defun array-dimension (a n) (if (= n 0) (array-length a) 0))
(defun array-total-size (a) (array-length a))
(defun array-rank (a) 1)
(defun adjustable-array-p (a) nil)

(defun array-displacement (a)
  "Return (values displaced-to displaced-index-offset) or (values nil 0)."
  ;; Displaced arrays represented as (cons (cons size offset) base-array)
  (if (and (consp a) (consp (car a)))
      (values (cdr a) (cdr (car a)))
      (values nil 0)))

(defun adjust-array (a new-size &rest args)
  "Return new array of NEW-SIZE with elements from A (or displacement target).
   Does not modify A in place (since adjustable-array-p always returns nil).
   Handles :displaced-to and :displaced-index-offset keywords."
  (let* ((displaced-to nil)
         (displaced-offset 0)
         (cur args))
    ;; Parse keyword args
    (loop
      (when (null cur) (return nil))
      (let ((k (car cur)) (v (cadr cur)))
        (cond
          ((eq k :displaced-to)
           (setq displaced-to v)
           (setq cur (cddr cur)))
          ((eq k :displaced-index-offset)
           (setq displaced-offset v)
           (setq cur (cddr cur)))
          (t (setq cur (cddr cur))))))
    (if displaced-to
        ;; Return displaced array: (cons (cons new-size offset) base-array)
        (cons (cons new-size displaced-offset) displaced-to)
        ;; Return new array with elements from a (up to new-size)
        (let ((new-arr (make-array new-size))
              (src-arr (if (and (consp a) (consp (car a)))
                           ;; a is displaced: get base array
                           (cdr a)
                           a))
              (src-offset (if (and (consp a) (consp (car a)))
                              (cdr (car a))
                              0)))
          (let ((i 0) (old-size (array-length src-arr)))
            (loop
              (when (>= i new-size) (return new-arr))
              (let ((src-idx (+ src-offset i)))
                (when (< src-idx old-size)
                  (aset new-arr i (aref src-arr src-idx))))
              (setq i (+ i 1))))))))

(defun row-major-aref (a idx) (aref a idx))
(defun set-row-major-aref (a idx val) (aset a idx val) val)
(defun char-type-error-check (fn x) nil)
(defun fp-array-p (x)
  "Check if x is a fill-pointer array wrapper (cons fixnum string)."
  (if (consp x)
      (if (fixnump (car x))
          (stringp (cdr x))
          nil)
      nil))
(defun displaced-array-p (x)
  "Check if x is a displaced array wrapper (cons (cons :displaced offset) string)."
  (if (consp x)
      (if (consp (car x))
          (stringp (cdr x))
          nil)
      nil))
(defun array-wrapper-p (x)
  "Check if x is a fill-pointer or displaced array wrapper."
  (if (consp x) (if (stringp (cdr x)) t nil) nil))
(defun wrapper-effective-length (w)
  "Get effective length of a fill-pointer or displaced array wrapper."
  (if (fixnump (car w))
      (car w)   ; fill-pointer
      (car (car w))))
(defun wrapper-offset (w)
  "Get offset for displaced array wrapper, 0 for fill-pointer."
  (if (fixnump (car w)) 0 (cdr (car w))))
(defun wrapper-aref (w i)
  "AREF on a fill-pointer or displaced array wrapper."
  (aref (cdr w) (+ (wrapper-offset w) i)))
(defun wrapper-aset (w i val)
  "ASET on a fill-pointer or displaced array wrapper."
  (aset (cdr w) (+ (wrapper-offset w) i) val))
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

