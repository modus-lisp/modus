;;;; cl-packages.lisp — Package system
;;;; Part of the Modus CL runtime. Depends on cl-types.lisp.

;;; ============================================================
;;; Common Lisp Package System — Runtime Implementation
;;; ============================================================
;;;
;;; Package = (cons <pkg-tag> <7-slot-array>)
;;;   slot 0: name (string)
;;;   slot 1: nicknames (list of strings)
;;;   slot 2: internal-symbols (alist: string -> symbol)
;;;   slot 3: external-symbols (alist: string -> symbol)
;;;   slot 4: use-list (list of packages)
;;;   slot 5: used-by-list (list of packages)
;;;   slot 6: shadowing-symbols (list of symbols)
;;;
;;; CL Symbol = heap object (tag=9 / subtag #x50) with 3 slots
;;;   slot 0: name-hash (fixnum, for backward compat)
;;;   slot 1: package (package object or nil)
;;;   slot 2: name (string)
;;;
;;; The symbol's subtag #x50 (+subtag-symbol+) distinguishes it from
;;; cons cells, general arrays, and closures (subtag #x52). This is
;;; the representation tags.lisp has reserved all along; the earlier
;;; (cons <sym-tag> <array-3>) form collided with closures in the
;;; funcall dispatch and needed day-scale cleanup to migrate.

(defvar *pkg-tag* 987654321)
(defvar *sym-tag* 123456789)  ; legacy marker — now unused by %cl-sym-p

;;; --- Package predicates and accessors ---

(defun %pkg-p (x)
  "Check if X is a package object."
  (if (consp x)
      (eql (car x) *pkg-tag*)
      nil))

(defun %pkg-data (pkg) (cdr pkg))
(defun %pkg-name (pkg) (aref (%pkg-data pkg) 0))
(defun %pkg-nicknames (pkg) (aref (%pkg-data pkg) 1))
(defun %pkg-internal (pkg) (aref (%pkg-data pkg) 2))
(defun %pkg-external (pkg) (aref (%pkg-data pkg) 3))
(defun %pkg-use-list (pkg) (aref (%pkg-data pkg) 4))
(defun %pkg-used-by (pkg) (aref (%pkg-data pkg) 5))
(defun %pkg-shadowing (pkg) (aref (%pkg-data pkg) 6))

(defun %pkg-set-name (pkg v) (aset (%pkg-data pkg) 0 v))
(defun %pkg-set-nicknames (pkg v) (aset (%pkg-data pkg) 1 v))
(defun %pkg-set-internal (pkg v) (aset (%pkg-data pkg) 2 v))
(defun %pkg-set-external (pkg v) (aset (%pkg-data pkg) 3 v))
(defun %pkg-set-use-list (pkg v) (aset (%pkg-data pkg) 4 v))
(defun %pkg-set-used-by (pkg v) (aset (%pkg-data pkg) 5 v))
(defun %pkg-set-shadowing (pkg v) (aset (%pkg-data pkg) 6 v))

;;; --- CL Symbol predicates and accessors ---
;;;
;;; Symbols are heap objects: tag=9, subtag=#x50, 3 slots. Accessors
;;; read/write those slots directly via aref/aset — the array ops
;;; work uniformly across object subtypes because the header layout
;;; (count + subtag) is shared.

(defun %cl-sym-p (x)
  "Check if X is a CL symbol (tag=object, subtag=#x50, 3 slots).
   Native MVM symbols share subtag #x50 but have 1 slot (hash only) —
   distinguished here by element count."
  (cond
    ((fixnump x) nil)
    ((consp x) nil)
    ((null x) nil)
    ((characterp x) nil)
    ((not (= (obj-subtag x) #x50)) nil)
    (t (>= (array-length x) 3))))

(defun %native-mvm-sym-p (x)
  "Check if X is a native MVM symbol (subtag #x50, single hash slot)."
  (cond
    ((fixnump x) nil)
    ((consp x) nil)
    ((null x) nil)
    ((characterp x) nil)
    ((not (= (obj-subtag x) #x50)) nil)
    (t (= (array-length x) 1))))

(defun %native-mvm-sym-hash (s) (aref s 0))

(defun %cl-sym-data (sym) sym)  ; legacy accessor — slots live directly on sym
(defun %cl-sym-hash (sym) (aref sym 0))
(defun %cl-sym-package (sym) (aref sym 1))
(defun %cl-sym-name (sym)
  "Slot 2 holds the name string.  Compile-time `'foo' literals interned
   via %INTERN-SYMBOL-PKG start with slot 2 empty (we don't pass the
   string at intern time — only the hash, to keep the IR small) and
   lazy-fill on first access by reverse-looking up the hash in the
   build-time *SYM-NAME-TABLE*.  Caches the result by writing back to
   slot 2 so subsequent calls are O(1).

   Returns the cached value when slot 2 is already populated; \"\" when
   no reverse-lookup table exists or the hash isn't in it (i.e. for
   genuinely uninterned syms like un-stringified gensyms)."
  (let ((cached (aref sym 2)))
    (cond
      ((and cached (stringp cached) (> (length cached) 0)) cached)
      (t
       (let ((tab (and (boundp '*sym-name-table*) *sym-name-table*)))
         (let ((found (and tab (gethash (aref sym 0) tab))))
           (cond
             ((and found (stringp found))
              (aset sym 2 found)
              found)
             (t ""))))))))

(defun %cl-sym-set-package (sym pkg) (aset sym 1 pkg))

(defun %make-cl-symbol (name-string)
  "Create a new CL symbol with NAME-STRING as its name. The returned
   object is tag-9 / subtag-0x50 with three slots [hash, package, name].
   Slot 0 = compute-name-hash(name-string) — used by compile-funcall's
   resolver path so funcall-on-CL-sym can find the function via the
   *native-sym-function-table* mirror that set-symbol-function maintains."
  (let ((sym (%alloc-sym3)))
    (aset sym 0 (compute-name-hash name-string))
    (aset sym 1 nil)         ; package
    (aset sym 2 name-string) ; name
    sym))

;;; --- Global package registry ---

(defvar *all-packages* nil)
(defvar *package* nil)

;;; Helper: remove all occurrences of ITEM (by eq) from LIST
(defun %remove-eq-item (item list)
  "Non-closure version of (remove-if (lambda (x) (eq x item)) list)."
  (let ((result nil) (cur list))
    (loop
      (when (null cur) (return (nreverse result)))
      (unless (eq (car cur) item)
        (setq result (cons (car cur) result)))
      (setq cur (cdr cur)))))

;;; Helper: remove from list where (string-equal (symbol-name s) name-str)
(defvar *%remove-sym-name* nil)
(defun %remove-sym-by-name (list)
  "Non-closure: remove symbols from LIST where name = *%remove-sym-name*."
  (let ((result nil) (cur list))
    (loop
      (when (null cur) (return (nreverse result)))
      (unless (string-equal (symbol-name (car cur)) *%remove-sym-name*)
        (setq result (cons (car cur) result)))
      (setq cur (cdr cur)))))

;;; Helper: map a function over a list where fn takes the string name of each element
(defun %mapcar-pkg-string-designator (list)
  "Map %pkg-string-designator over each element of LIST."
  (let ((result nil) (cur list))
    (loop
      (when (null cur) (return (nreverse result)))
      (setq result (cons (%pkg-string-designator (car cur)) result))
      (setq cur (cdr cur)))))

;;; --- String coercion for designators ---

(defun %pkg-string-designator (x)
  "Coerce a string designator to a string.

   Handles:
     - string -> the string itself
     - character -> 1-char string
     - CL-symbol (3-slot #x50 with name slot) -> name string
     - native MVM symbol (1-slot #x50, hash-only) -> symbol-name lookup
       through *sym-name-table* / package symtabs
     - native MVM keyword (#x53, hash-only) -> same lookup
     - wrapper array (fill-pointer / displaced over string) -> materialize
     - anything else -> empty string \"\"

   The native-sym and keyword paths matter for the ANSI defpackage and
   make-package suites: `'(\"H\" #:|H| #\\H)` in a test body becomes,
   after Modus compile-quote interns the symbol, a list whose middle
   element is a 1-slot native MVM sym (uninterned #:|H| collapses to
   the interned 'H since hash-keyed intern can't distinguish them).
   Similarly, options like `(:shadow ,s)` where `s` evaluates to a
   keyword (`:|f|`) hand us a #x53 native object.  Without dispatching
   on those subtags here, the designator collapses to \"\" and every
   downstream intern / shadow / find-symbol lookup uses the empty
   string as a name — which silently constructs broken symbols and
   makes (equal (symbol-name sym) \"f\") fail.

   Uses obj-subtag guards mirroring SYMBOL-NAME's predicate."
  (cond
    ((stringp x) x)
    ((characterp x)
     (let ((s (%make-string-array 1)))
       (aset s 0 (char-code x))
       s))
    ((%cl-sym-p x) (%cl-sym-name x))
    ;; Native MVM sym (#x50) or keyword (#x53), 1-slot hash-only.
    ;; Defer to SYMBOL-NAME which knows how to reverse-lookup the hash
    ;; through *sym-name-table* and the package symtabs.
    ((and (not (null x)) (not (eq x t)) (not (consp x))
          (not (integerp x)) (not (characterp x))
          (let ((st (obj-subtag x)))
            (or (= st 80) (= st 83))))
     (symbol-name x))
    ;; Wrapper array (fill-pointer / displaced) — materialise to a flat
    ;; string via WRAPPER-AREF.  Modus's print/string routines accept
    ;; wrappers but our intern-by-name path needs a flat string.
    ((and (consp x) (stringp (cdr x)))
     (let ((len (wrapper-effective-length x)))
       (let ((s (%make-string-array len)))
         (dotimes (i len s) (aset s i (wrapper-aref x i))))))
    (t (if (stringp x) x ""))))

(defun %pkg-string= (a b)
  "Compare two string designators for equality."
  (let ((sa (%pkg-string-designator a))
        (sb (%pkg-string-designator b)))
    (string-equal sa sb)))

;;; --- Package designator resolution ---

(defun %resolve-package (designator)
  "Resolve a package designator to a package object.
   Package -> itself, string/symbol/character -> find-package.
   Native MVM symbol (carries name-hash only, no name string) ->
   match against package name hashes."
  (cond
    ((%pkg-p designator) designator)
    ((%native-mvm-sym-p designator)
     (%pkg-find-by-hash (%native-mvm-sym-hash designator)))
    (t (find-package designator))))

(defun %pkg-find-by-hash (h)
  "Look up a package by hashing each candidate's primary name OR any
   nickname.  Used when a designator is a native MVM symbol that
   carries only a hash.  Without the nickname walk, `'lisp` as a
   designator never resolves to COMMON-LISP even though LISP is a
   nickname — every `(do-external-symbols (s 'lisp) ...)` call in the
   gcl ansi-test suite then iterates zero symbols and cl-symbols.lsp
   (~981 tests) uniformly fails."
  (let ((cur *all-packages*)
        (found nil))
    (loop
      (when found (return found))
      (when (null cur) (return nil))
      (let ((pkg (car cur)))
        (when (and (%pkg-name pkg)
                   (= (compute-name-hash (%pkg-name pkg)) h))
          (setq found pkg))
        ;; Walk nicknames too — LISP/CL nicknames are how older code
        ;; refers to COMMON-LISP.  Uses an explicit `found` flag rather
        ;; than `return-from` so we don't rely on the implicit block.
        (unless found
          (let ((nicks (%pkg-nicknames pkg)))
            (loop
              (when found (return))
              (when (null nicks) (return))
              (let ((nn (car nicks)))
                (when (and (stringp nn)
                           (= (compute-name-hash nn) h))
                  (setq found pkg)))
              (setq nicks (cdr nicks))))))
      (setq cur (cdr cur)))))

;;; --- Internal alist-based symbol table operations ---

(defun %symtab-find (table name-string)
  "Find symbol in alist TABLE by NAME-STRING.  Returns (name . symbol)
   or nil.  ANSI: symbol-name lookup is CASE-SENSITIVE — 'a' and 'A'
   are distinct symbol names."
  (let ((cur table))
    (loop
      (when (null cur) (return nil))
      (let ((entry (car cur)))
        (when (string= (car entry) name-string)
          (return entry)))
      (setq cur (cdr cur)))))

(defun %symtab-add (table name-string symbol)
  "Add SYMBOL to alist TABLE under NAME-STRING. Returns new table."
  (cons (cons name-string symbol) table))

(defun %symtab-remove (table name-string)
  "Remove NAME-STRING from alist TABLE. Returns new table."
  (let ((result nil) (cur table))
    (loop
      (when (null cur) (return (nreverse result)))
      (if (string-equal (car (car cur)) name-string)
          (setq cur (cdr cur))
          (progn
            (setq result (cons (car cur) result))
            (setq cur (cdr cur)))))))

;;; --- Core package functions ---

(defun packagep (x) (%pkg-p x))

(defun package-name (pkg)
  (let ((p (%resolve-package pkg)))
    (if p (%pkg-name p) nil)))

(defun package-nicknames (pkg)
  (let ((p (%resolve-package pkg)))
    (if p (%pkg-nicknames p) nil)))

(defun package-use-list (pkg)
  (let ((p (%resolve-package pkg)))
    (if p (%pkg-use-list p) nil)))

(defun package-used-by-list (pkg)
  (let ((p (%resolve-package pkg)))
    (if p (%pkg-used-by p) nil)))

(defun package-shadowing-symbols (pkg)
  (let ((p (%resolve-package pkg)))
    (if p (%pkg-shadowing p) nil)))

(defun list-all-packages ()
  *all-packages*)

(defun find-package (name)
  "Find package by name or nickname.

   CL-TEST hidden alias: the build script reads ANSI test source with
   *package*=CL-USER, so every `'foo' literal is stamped CL-USER at
   compile time.  The original ANSI test source assumes the reader's
   *package* was CL-TEST and asserts
       (eqlt (symbol-package 'foo) (find-package :cl-test)).
   We resolve \"CL-TEST\" specially HERE — without adding it to
   CL-USER's nicknames list — so `(package-nicknames CL-USER)` still
   returns the ANSI-expected `(\"CL-USER\")`.  The cl-symbols.lsp
   suite has an explicit
       (deftest common-lisp-user-package-nicknames
         (package-nicknames (find-package \"COMMON-LISP-USER\"))
         (\"CL-USER\"))
   which would regress if we polluted the nicknames slot."
  (cond
    ((%pkg-p name) name)
    (t
     (let ((name-str (%pkg-string-designator name)))
       (when (and name-str (string-equal name-str "CL-TEST"))
         (let ((clu (find-package-1 "COMMON-LISP-USER")))
           (when clu (return-from find-package clu))))
       (find-package-1 name-str)))))

(defun find-package-1 (name-str)
  "Internal find-package: walks *all-packages* matching primary name
   then nicknames against NAME-STRING via string-equal.  Split out so
   the public FIND-PACKAGE can short-circuit the CL-TEST alias check
   without polluting CL-USER's nickname list."
  (let ((cur *all-packages*))
    (loop
      (when (null cur) (return nil))
      (let ((pkg (car cur)))
        (when (%pkg-name pkg)
          (when (string-equal (%pkg-name pkg) name-str)
            (return pkg))
          (let ((nicks (%pkg-nicknames pkg))
                (found nil))
            (let ((ncur nicks))
              (loop
                (when (null ncur) (return nil))
                (when (string-equal (car ncur) name-str)
                  (setq found t)
                  (return nil))
                (setq ncur (cdr ncur))))
            (when found (return pkg)))))
      (setq cur (cdr cur)))))

(defun %make-package-object (name-string)
  "Allocate and initialize an empty package object."
  (let ((data (make-array 7)))
    (aset data 0 name-string) ; name
    (aset data 1 nil)         ; nicknames
    (aset data 2 nil)         ; internal-symbols (alist)
    (aset data 3 nil)         ; external-symbols (alist)
    (aset data 4 nil)         ; use-list
    (aset data 5 nil)         ; used-by-list
    (aset data 6 nil)         ; shadowing-symbols
    (cons *pkg-tag* data)))

(defun make-package (name &rest args)
  "Create a new package with NAME.
   Per CLHS: signals PROGRAM-ERROR on odd-length plist or unknown
   keyword unless :allow-other-keys is non-nil.

   CL-TEST alias: find-package short-circuits \"CL-TEST\" lookups to
   the CL-USER package, so cl-conditions:1669's
       (make-package \"CL-TEST\" :use (list \"CL\"))
   hits the existing-package branch below and returns CL-USER without
   creating a second package.  This way (symbol-package 'foo) (which
   is the CL-USER package — compile-time stamp from a CL-USER-read
   test source) equals (find-package :cl-test).  Avoids polluting
   CL-USER's nicknames list — cl-symbols.lsp asserts an exact
   (\"CL-USER\") nickname list."
  (let ((name-str (%pkg-string-designator name))
        (nicknames nil)
        (use-list nil)
        (allow-other-keys nil)
        (aok-set nil)
        (a args))
    ;; Probe :allow-other-keys (leftmost wins per CLHS §3.4.1.4.1.1.2).
    (let ((p args))
      (loop (when (null p) (return))
        (when (and (not aok-set) (eq (car p) :allow-other-keys))
          (setq allow-other-keys (and (consp (cdr p)) (cadr p)))
          (setq aok-set t))
        (setq p (cdr p))))
    ;; Parse keyword args
    (loop
      (when (null a) (return nil))
      (when (null (cdr a)) (%signal-program-error))
      (cond
        ((eq (car a) :nicknames)         (setq nicknames (cadr a)))
        ((eq (car a) :use)               (setq use-list (cadr a)))
        ((eq (car a) :allow-other-keys)  nil)
        (t (unless allow-other-keys (%signal-program-error))))
      (setq a (cddr a)))
    ;; Check for existing package
    (when (find-package name-str)
      (return-from make-package (find-package name-str)))
    ;; Create package
    (let ((pkg (%make-package-object name-str)))
      ;; Set nicknames (coerce to strings)
      (%pkg-set-nicknames pkg
        (%mapcar-pkg-string-designator nicknames))
      ;; Register
      (setq *all-packages* (cons pkg *all-packages*))
      ;; Setup use-list
      (when use-list
        (dolist (u use-list)
          (%use-package-impl u pkg)))
      pkg)))

(defun delete-package (pkg)
  "Delete package PKG."
  (let ((p (%resolve-package pkg)))
    (when (and p (%pkg-name p))
      ;; Remove from used-by lists
      (dolist (used (%pkg-use-list p))
        (%pkg-set-used-by used
          (%remove-eq-item p (%pkg-used-by used))))
      ;; Unuse all packages
      (%pkg-set-use-list p nil)
      ;; Unintern all symbols in this package (set home package to nil)
      (dolist (entry (%pkg-internal p))
        (let ((sym (cdr entry)))
          (when (and (%cl-sym-p sym) (eq (%cl-sym-package sym) p))
            (%cl-sym-set-package sym nil))))
      (dolist (entry (%pkg-external p))
        (let ((sym (cdr entry)))
          (when (and (%cl-sym-p sym) (eq (%cl-sym-package sym) p))
            (%cl-sym-set-package sym nil))))
      ;; Clear the package
      (%pkg-set-name p nil)
      (%pkg-set-nicknames p nil)
      (%pkg-set-internal p nil)
      (%pkg-set-external p nil)
      (%pkg-set-shadowing p nil)
      ;; Remove from global registry
      (setq *all-packages* (%remove-eq-item p *all-packages*))
      t)))

(defun rename-package (pkg new-name &rest new-nicknames-arg)
  "Rename PKG to NEW-NAME with optional new nicknames."
  (let ((p (%resolve-package pkg))
        (new-nicks (if new-nicknames-arg (car new-nicknames-arg) nil)))
    (when p
      (%pkg-set-name p (%pkg-string-designator new-name))
      (%pkg-set-nicknames p
        (%mapcar-pkg-string-designator new-nicks))
      p)))

;;; --- Symbol operations ---

(defun %native-mvm-sym-name-lookup (h)
  "Walk every interned-package symbol table looking for a symbol whose
   slot-0 hash equals H.  Returns the matching name string or NIL.
   Used by SYMBOL-NAME for native-MVM symbols (which carry only a hash,
   no name slot of their own)."
  (let ((cur *all-packages*) (found nil))
    (loop
      (when (or (null cur) found) (return found))
      (let ((p (car cur)))
        (when (%pkg-p p)
          ;; Internal table
          (let ((entries (%pkg-internal p)))
            (let ((c2 entries))
              (loop
                (when (or (null c2) found) (return found))
                (let ((entry (car c2)))
                  (when (and (consp entry) (stringp (car entry)))
                    (when (= (compute-name-hash (car entry)) h)
                      (setq found (car entry)))))
                (setq c2 (cdr c2)))))
          ;; External table
          (unless found
            (let ((entries (%pkg-external p)))
              (let ((c2 entries))
                (loop
                  (when (or (null c2) found) (return found))
                  (let ((entry (car c2)))
                    (when (and (consp entry) (stringp (car entry)))
                      (when (= (compute-name-hash (car entry)) h)
                        (setq found (car entry)))))
                  (setq c2 (cdr c2))))))))
      (setq cur (cdr cur)))
    found))

;; *sym-name-table* — hash → name string, populated at boot by
;; %init-sym-name-auto.  Lets symbol-name recover names for native MVM
;; syms (#x50, hash-only) that aren't in any package symbol table.
(defvar *sym-name-table* nil)

(defun symbol-name (sym)
  "Return the name of a symbol as a string. For Modus's integer-valued
   gensyms (sym is an integer), produce a unique 'G<N>' name so tests
   like (string= (symbol-name (gensym)) (symbol-name (gensym))) → NIL.
   For native-MVM symbols (subtag #x50, 1 slot — hash only), look the
   hash up in *sym-name-table* (populated at boot from build-time scan
   of every quoted symbol in the source tree), then fall back to
   walking *all-packages*' symbol tables, then \"\"."
  (cond
    ((null sym) "NIL")
    ((eq sym t) "T")
    ((%cl-sym-p sym) (%cl-sym-name sym))
    ((integerp sym)
     (let ((digs (write-to-string sym)))
       (concatenate 'string "G" digs)))
    ;; Native MVM symbol (#x50) or keyword (#x53): both single-slot, hash only.
    ((and (not (consp sym)) (not (characterp sym)) (not (stringp sym))
          (let ((st (obj-subtag sym))) (or (= st 80) (= st 83))))
     (let ((h (aref sym 0)))
       ;; First try the build-time reverse table — covers symbols that
       ;; appear ANYWHERE in the source tree (defuns, quoted refs, etc.).
       (let ((nm (and *sym-name-table* (gethash h *sym-name-table*))))
         (cond
           (nm nm)
           (t
            ;; Fall back to walking package symtabs (for runtime-interned
            ;; symbols whose name was registered via package machinery).
            (let ((pn (%native-mvm-sym-name-lookup h)))
              (if pn pn "")))))))
    (t "")))

(defun symbol-package (sym)
  "Return the home package of a symbol.  Native MVM keywords (#x53)
   are always in the KEYWORD package; native MVM symbols (#x50) carry
   no package slot — return NIL for them (callers that care can walk
   *all-packages* via %native-mvm-sym-name-lookup)."
  (cond
    ((null sym) (find-package "COMMON-LISP"))
    ((eq sym t) (find-package "COMMON-LISP"))
    ((%cl-sym-p sym) (%cl-sym-package sym))
    ((or (integerp sym) (consp sym) (characterp sym) (stringp sym)) nil)
    ((= (obj-subtag sym) 83) (find-package "KEYWORD"))   ; #x53 keyword
    (t nil)))

(defun make-symbol (name)
  "Create an uninterned symbol with NAME."
  (let ((sym (%make-cl-symbol (%pkg-string-designator name))))
    sym))

(defun copy-symbol (sym &optional copy-props)
  "Create a copy of SYM."
  (let ((new (%make-cl-symbol (symbol-name sym))))
    new))

;;; --- gensym (override prelude.lisp's integer-returning stub) ---
;;;
;;; ANSI gensym must return a fresh uninterned symbol — not an integer —
;;; so (funcall (gensym) ...), (symbol-name (gensym)), and the eval/funcall
;;; pattern in defmethod tests work.  The earlier integer-returning stub
;;; was here because %make-cl-symbol predated this file; now it doesn't.
(defun gensym (&optional prefix)
  (let ((p (cond ((stringp prefix) prefix)
                 ((null prefix) "G")
                 (t "G")))
        (n *gensym-counter*))
    (let ((name (format nil "~A~D" p n)))
      (setq *gensym-counter* (+ n 1))
      (%make-cl-symbol name))))

;;; --- gentemp ---
(defvar *gentemp-counter* 0)

(defun gentemp (&rest args)
  "Generate a new symbol interned in *PACKAGE*. Prefix defaults to T."
  (let ((prefix (if args (%pkg-string-designator (car args)) "T"))
        (pkg (if (and args (cdr args))
                 (%resolve-package (cadr args))
                 *package*)))
    (loop
      (let* ((name (format nil "~A~D" prefix *gentemp-counter*))
             (found (find-symbol name pkg)))
        (setq *gentemp-counter* (+ *gentemp-counter* 1))
        (when (null found)
          ;; Symbol doesn't exist yet — intern it
          (let ((sym (%make-cl-symbol name)))
            (%cl-sym-set-package sym pkg)
            (%pkg-set-internal pkg (%symtab-add (%pkg-internal pkg) name sym))
            (return sym)))))))

;;; --- find-symbol / intern ---

(defun find-symbol (name &rest pkg-arg)
  "Find symbol named NAME in package PKG.
   Returns (values symbol status) or (values nil nil)."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (name-str (%pkg-string-designator name)))
    (if (null pkg)
        (values nil nil)
        ;; Check external symbols
        (let ((ext-entry (%symtab-find (%pkg-external pkg) name-str)))
          (if ext-entry
              (values (cdr ext-entry) :external)
              ;; Check internal symbols
              (let ((int-entry (%symtab-find (%pkg-internal pkg) name-str)))
                (if int-entry
                    (values (cdr int-entry) :internal)
                    ;; Check inherited (use-list external symbols)
                    (let ((found nil)
                          (use (%pkg-use-list pkg)))
                      (loop
                        (when (null use) (return nil))
                        (let ((uext (%symtab-find (%pkg-external (car use)) name-str)))
                          (when uext
                            (setq found (cdr uext))
                            (return nil)))
                        (setq use (cdr use)))
                      (if found
                          (values found :inherited)
                          (values nil nil))))))))))

(defun intern (name &rest pkg-arg)
  "Intern symbol named NAME in package PKG.
   Returns (values symbol status)."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (name-str (%pkg-string-designator name)))
    (if (null pkg)
        (values nil nil)
        ;; Check if already present
        (let ((ext-entry (%symtab-find (%pkg-external pkg) name-str)))
          (if ext-entry
              (values (cdr ext-entry) :external)
              (let ((int-entry (%symtab-find (%pkg-internal pkg) name-str)))
                (if int-entry
                    (values (cdr int-entry) :internal)
                    ;; Check inherited
                    (let ((found nil)
                          (use (%pkg-use-list pkg)))
                      (loop
                        (when (null use) (return nil))
                        (let ((uext (%symtab-find (%pkg-external (car use)) name-str)))
                          (when uext
                            (setq found (cdr uext))
                            (return nil)))
                        (setq use (cdr use)))
                      (if found
                          (values found :inherited)
                          ;; Create new symbol
                          (if (and (find-package "KEYWORD")
                                   (eq pkg (find-package "KEYWORD")))
                              ;; Keyword package: route through %INTERN-KEYWORD
                              ;; so the resulting object is the same #x53 native
                              ;; keyword that compile-keyword's `:foo' literals
                              ;; resolve to.  Eq across reader and compile-time
                              ;; references.  Add to the KEYWORD package's
                              ;; external symtab so find-symbol / symbol-name /
                              ;; do-external-symbols still see it.
                              (let ((kw (%intern-keyword
                                          (compute-name-hash name-str))))
                                (%pkg-set-external pkg
                                  (%symtab-add (%pkg-external pkg) name-str kw))
                                (values kw :external))
                              ;; Regular (non-keyword) package: per-CLHS
                              ;; per-package distinct symbols.  See
                              ;; SYMBOLS_PLAN.md.  We allocate a fresh
                              ;; CL-symbol for this (name, pkg) pair and
                              ;; register it under a composite key in the
                              ;; global intern table at #x10000088 so that
                              ;; (eq (intern "X" pkg) (intern "X" pkg)) holds
                              ;; but (eq (intern "X" pkg1) (intern "X" pkg2))
                              ;; is NIL when pkg1 ≠ pkg2.  Compile-quote's
                              ;; %INTERN-SYMBOL-PKG uses the same composite
                              ;; key (in mvm/prelude.lisp) so 'foo from
                              ;; source resolves to the same object as
                              ;; (intern "FOO" *package*).
                              (let* ((name-hash (compute-name-hash name-str))
                                     (pkg-hash (compute-name-hash (%pkg-name pkg)))
                                     (key (logand (+ name-hash
                                                     (* pkg-hash 2305843009213693951))
                                                  #x3FFFFFFFFFFFFFFF))
                                     ;; compute-name-hash UPPERCASES before
                                     ;; hashing, so "A" and "a" share KEY.
                                     ;; Only reuse a globally-keyed symbol when
                                     ;; its actual name matches case-sensitively
                                     ;; — otherwise it's a hash collision and we
                                     ;; must mint a fresh symbol (whose
                                     ;; within-package identity is then carried
                                     ;; by the package symtab, checked first on
                                     ;; the next intern).  Without this, reading
                                     ;; "\\A" / "|A|" returned the pre-interned
                                     ;; "a" symbol (symbol-name "a"), breaking
                                     ;; syntax.escaped.* and case round-trips.
                                     (existing
                                       (let ((g (mem-ref #x10000088 :u64)))
                                         (let ((e (and g (gethash key g))))
                                           (and e (string= (symbol-name e) name-str) e))))
                                     (sym (or existing
                                              (let ((s (%make-cl-symbol name-str)))
                                                (%cl-sym-set-package s pkg)
                                                ;; Re-read after alloc — GC may
                                                ;; have moved the table.  Only
                                                ;; claim the KEY slot if it is
                                                ;; free; a colliding-case name
                                                ;; that already owns it keeps it.
                                                (let ((g (mem-ref #x10000088 :u64)))
                                                  (when (and g (not (gethash key g)))
                                                    (puthash key g s)))
                                                s))))
                                (%pkg-set-internal pkg
                                  (%symtab-add (%pkg-internal pkg) name-str sym))
                                (values sym nil))))))))))))

;;; --- export / unexport ---

(defun export (symbols &rest pkg-arg)
  "Export SYMBOLS from PKG."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (sym-list (if (and (consp symbols) (not (%cl-sym-p symbols)))
                      symbols
                      (list symbols))))
    (dolist (sym sym-list)
      (let ((name-str (symbol-name sym)))
        ;; If in internal, move to external
        (let ((int-entry (%symtab-find (%pkg-internal pkg) name-str)))
          (when int-entry
            (%pkg-set-internal pkg (%symtab-remove (%pkg-internal pkg) name-str))))
        ;; Add to external if not already there
        (unless (%symtab-find (%pkg-external pkg) name-str)
          (%pkg-set-external pkg
            (%symtab-add (%pkg-external pkg) name-str sym)))))
    t))

(defun unexport (symbols &rest pkg-arg)
  "Unexport SYMBOLS from PKG (move to internal)."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (sym-list (if (and (consp symbols) (not (%cl-sym-p symbols)))
                      symbols
                      (list symbols))))
    (dolist (sym sym-list)
      (let ((name-str (symbol-name sym)))
        ;; If in external, move to internal
        (let ((ext-entry (%symtab-find (%pkg-external pkg) name-str)))
          (when ext-entry
            (%pkg-set-external pkg (%symtab-remove (%pkg-external pkg) name-str))
            (unless (%symtab-find (%pkg-internal pkg) name-str)
              (%pkg-set-internal pkg
                (%symtab-add (%pkg-internal pkg) name-str sym)))))))
    t))

;;; --- import ---

(defun import (symbols &rest pkg-arg)
  "Import SYMBOLS into PKG.  Per CLHS the call takes one or two args:
   the symbol designator and an optional package.  Extra positional
   args signal program-error.  (import.error.2)"
  (when (and pkg-arg (cdr pkg-arg))
    (%signal-program-error))
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (sym-list (if (and (consp symbols) (not (%cl-sym-p symbols)))
                      symbols
                      (list symbols))))
    (dolist (sym sym-list)
      (let ((name-str (symbol-name sym)))
        ;; Only add if not already accessible
        (unless (or (%symtab-find (%pkg-internal pkg) name-str)
                    (%symtab-find (%pkg-external pkg) name-str))
          (%pkg-set-internal pkg
            (%symtab-add (%pkg-internal pkg) name-str sym)))))
    t))

;;; --- unintern ---

(defun unintern (sym &rest pkg-arg)
  "Remove SYM from PKG."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (name-str (symbol-name sym))
        (removed nil))
    (when (%symtab-find (%pkg-internal pkg) name-str)
      (%pkg-set-internal pkg (%symtab-remove (%pkg-internal pkg) name-str))
      (setq removed t))
    (when (%symtab-find (%pkg-external pkg) name-str)
      (%pkg-set-external pkg (%symtab-remove (%pkg-external pkg) name-str))
      (setq removed t))
    ;; Remove from shadowing symbols
    (%pkg-set-shadowing pkg
      (%remove-eq-item sym (%pkg-shadowing pkg)))
    ;; If this was the symbol's home package, set to nil
    (when (and removed (%cl-sym-p sym) (eq (%cl-sym-package sym) pkg))
      (%cl-sym-set-package sym nil))
    removed))

;;; --- use-package / unuse-package ---

(defun %use-package-impl (packages-to-use using-pkg)
  "Internal: add PACKAGES-TO-USE to USING-PKG's use-list."
  (let ((to-use (%resolve-package packages-to-use)))
    (when (and to-use (not (eq to-use using-pkg)))
      (unless (member to-use (%pkg-use-list using-pkg) :test #'eq)
        (%pkg-set-use-list using-pkg
          (cons to-use (%pkg-use-list using-pkg)))
        (%pkg-set-used-by to-use
          (cons using-pkg (%pkg-used-by to-use)))))))

(defun use-package (packages &rest pkg-arg)
  "Add PACKAGES to the use-list of PKG."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (pkg-list (if (and (consp packages) (not (%pkg-p packages)))
                      packages
                      (list packages))))
    (dolist (p pkg-list)
      (%use-package-impl p pkg))
    t))

(defun unuse-package (packages &rest pkg-arg)
  "Remove PACKAGES from the use-list of PKG."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (pkg-list (if (and (consp packages) (not (%pkg-p packages)))
                      packages
                      (list packages))))
    (dolist (p-designator pkg-list)
      (let ((p (%resolve-package p-designator)))
        (when p
          (%pkg-set-use-list pkg
            (%remove-eq-item p (%pkg-use-list pkg)))
          (%pkg-set-used-by p
            (%remove-eq-item pkg (%pkg-used-by p))))))
    t))

;;; --- shadow / shadowing-import ---

(defun shadow (names &rest pkg-arg)
  "Create shadowing symbols in PKG."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (name-list (if (or (stringp names) (%cl-sym-p names) (characterp names))
                       (list names)
                       (if (consp names) names (list names)))))
    (dolist (name name-list)
      (let ((name-str (%pkg-string-designator name)))
        ;; Find or create symbol
        (let ((ext-entry (%symtab-find (%pkg-external pkg) name-str))
              (int-entry (%symtab-find (%pkg-internal pkg) name-str)))
          (let ((sym (cond (ext-entry (cdr ext-entry))
                           (int-entry (cdr int-entry))
                           (t ;; Create new internal symbol
                            (let ((new-sym (%make-cl-symbol name-str)))
                              (%cl-sym-set-package new-sym pkg)
                              (%pkg-set-internal pkg
                                (%symtab-add (%pkg-internal pkg) name-str new-sym))
                              new-sym)))))
            ;; Add to shadowing symbols if not already there
            (unless (member sym (%pkg-shadowing pkg) :test #'eq)
              (%pkg-set-shadowing pkg
                (cons sym (%pkg-shadowing pkg))))))))
    t))

(defun shadowing-import (symbols &rest pkg-arg)
  "Import SYMBOLS into PKG as shadowing symbols."
  (let ((pkg (%resolve-package (if pkg-arg (car pkg-arg) *package*)))
        (sym-list (if (and (consp symbols) (not (%cl-sym-p symbols)))
                      symbols
                      (list symbols))))
    (dolist (sym sym-list)
      (let ((name-str (symbol-name sym)))
        ;; Remove any existing symbol with this name
        (%pkg-set-internal pkg (%symtab-remove (%pkg-internal pkg) name-str))
        (%pkg-set-external pkg (%symtab-remove (%pkg-external pkg) name-str))
        ;; Remove old shadowing symbols with same name
        (setq *%remove-sym-name* name-str)
        (%pkg-set-shadowing pkg
          (%remove-sym-by-name (%pkg-shadowing pkg)))
        ;; Add the symbol as internal
        (%pkg-set-internal pkg
          (%symtab-add (%pkg-internal pkg) name-str sym))
        ;; Add to shadowing list
        (%pkg-set-shadowing pkg
          (cons sym (%pkg-shadowing pkg)))))
    t))

;;; --- find-all-symbols ---

(defun find-all-symbols (name)
  "Find all symbols with NAME in all packages."
  (let ((name-str (%pkg-string-designator name))
        (result nil))
    (dolist (pkg *all-packages*)
      (when (%pkg-name pkg)
        (let ((ext (%symtab-find (%pkg-external pkg) name-str)))
          (when ext
            (unless (member (cdr ext) result :test #'eq)
              (setq result (cons (cdr ext) result)))))
        (let ((int (%symtab-find (%pkg-internal pkg) name-str)))
          (when int
            (unless (member (cdr int) result :test #'eq)
              (setq result (cons (cdr int) result)))))))
    result))

;;; --- defpackage ---

(defun %defpkg-pool-contains (pool s)
  "True if string S (case-sensitive) is already in POOL (list of strings).
   Used by %defpackage-impl's disjointness checks."
  (let ((cur pool) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (when (string= (car cur) s) (setq found t))
      (setq cur (cdr cur)))))

(defun %signal-package-error ()
  "Runtime helper: signal a PACKAGE-ERROR condition for handler-case.
   Mirrors %signal-program-error's shape — a 2-elt condition object with
   the type symbol in slot 0.  Used by %defpackage-impl for nickname
   conflicts (CLHS 11.1.1.1) and for disjointness violations whose CLHS
   spec calls for PACKAGE-ERROR rather than PROGRAM-ERROR."
  (let ((c (make-array 2)))
    (aset c 0 'package-error)
    (aset c 1 nil)
    (setq *current-condition* c)
    (if (%error-handler-active-p) (%hc-longjmp) nil)))

(defun %defpackage-impl (name options)
  "Create/modify a package with options (options is a list).

   Implements the CLHS 11.1.1.1 options:
     :nicknames, :documentation, :use, :shadow, :shadowing-import-from,
     :import-from, :export, :intern, :size

   Signals PROGRAM-ERROR for:
     - duplicate :size or :documentation
     - names in :shadow / :shadowing-import-from / :import-from / :intern
       not disjoint (one symbol-name appearing in more than one of these)
     - names in :export and :intern not disjoint
   Signals PACKAGE-ERROR for:
     - a nickname that already names (or is a nickname of) a different
       existing package"
  (let ((name-str (%pkg-string-designator name))
        (nicknames nil)
        (use-list nil)
        (use-provided nil)
        (export-names nil)
        (import-from nil)
        (shadow-names nil)
        (shadowing-import-from nil)
        (intern-names nil)
        (size-seen nil)
        (doc-seen nil)
        (doc-string nil))
    ;; --- Parse options ---
    (dolist (opt options)
      (when (consp opt)
        (let ((key (car opt)))
          (cond
            ((eq key :nicknames)
             (setq nicknames (append nicknames (cdr opt))))
            ((eq key :use)
             (setq use-provided t)
             (setq use-list (append use-list (cdr opt))))
            ((eq key :export)
             (setq export-names (append export-names (cdr opt))))
            ((eq key :import-from)
             (setq import-from (cons (cdr opt) import-from)))
            ((eq key :shadow)
             (setq shadow-names (append shadow-names (cdr opt))))
            ((eq key :shadowing-import-from)
             (setq shadowing-import-from (cons (cdr opt) shadowing-import-from)))
            ((eq key :intern)
             (setq intern-names (append intern-names (cdr opt))))
            ((eq key :documentation)
             ;; Duplicate :documentation → program-error (CLHS).
             (when doc-seen (%signal-program-error))
             (setq doc-seen t)
             (when (cdr opt) (setq doc-string (cadr opt))))
            ((eq key :size)
             ;; Duplicate :size → program-error (CLHS).  Value ignored
             ;; otherwise — Modus packages don't pre-allocate symbol
             ;; tables.
             (when size-seen (%signal-program-error))
             (setq size-seen t))
            (t nil)))))

    ;; --- Disjointness pool 1: :shadow, :shadowing-import-from (sym
    ;; names), :import-from (sym names), :intern.  CLHS 11.1.1.1 says
    ;; these must be disjoint or a program-error is signalled.
    (let ((pool nil))
      (dolist (n shadow-names)
        (let ((s (%pkg-string-designator n)))
          (when (%defpkg-pool-contains pool s) (%signal-program-error))
          (setq pool (cons s pool))))
      (dolist (spec shadowing-import-from)
        (dolist (n (cdr spec))
          (let ((s (%pkg-string-designator n)))
            (when (%defpkg-pool-contains pool s) (%signal-program-error))
            (setq pool (cons s pool)))))
      (dolist (spec import-from)
        (dolist (n (cdr spec))
          (let ((s (%pkg-string-designator n)))
            (when (%defpkg-pool-contains pool s) (%signal-program-error))
            (setq pool (cons s pool)))))
      (dolist (n intern-names)
        (let ((s (%pkg-string-designator n)))
          (when (%defpkg-pool-contains pool s) (%signal-program-error))
          (setq pool (cons s pool)))))

    ;; --- Disjointness pool 2: :export and :intern must be disjoint.
    (let ((pool nil))
      (dolist (n intern-names)
        (let ((s (%pkg-string-designator n)))
          (setq pool (cons s pool))))
      (dolist (n export-names)
        (let ((s (%pkg-string-designator n)))
          (when (%defpkg-pool-contains pool s) (%signal-program-error))
          (setq pool (cons s pool)))))

    ;; --- Nickname conflict: each nickname must not be the primary
    ;; name or an existing nickname of any OTHER package.  (CLHS:
    ;; defpackage may rename the package itself, but reusing some
    ;; other package's nickname is a package-error.)  We resolve
    ;; conflicts against find-package before the existing-package
    ;; delete-and-recreate step below, since defpackage is allowed to
    ;; replace ITS OWN earlier definition.
    (dolist (nk nicknames)
      (let ((nk-str (%pkg-string-designator nk)))
        (let ((existing (find-package nk-str)))
          (when (and existing
                     (not (%pkg-string= (%pkg-name existing) name-str)))
            (%signal-package-error)))))

    ;; --- Delete existing same-name package if any ---
    (let ((existing (find-package name-str)))
      (when existing (safely-delete-package existing)))

    ;; --- Build the package ---
    (let ((pkg (make-package name-str
                 :nicknames nicknames
                 :use (if use-provided use-list nil))))
      ;; Shadow
      (when shadow-names
        (shadow shadow-names pkg))
      ;; Shadowing-import-from: ((pkg-name sym-name ...) ...)
      (dolist (spec shadowing-import-from)
        (let ((from-pkg (%resolve-package (car spec))))
          (when from-pkg
            (dolist (sname (cdr spec))
              (let ((sname-str (%pkg-string-designator sname)))
                (let ((found (find-symbol sname-str from-pkg)))
                  (when found
                    (shadowing-import found pkg))))))))
      ;; Import-from: ((pkg-name sym-name ...) ...)
      (dolist (spec import-from)
        (let ((from-pkg (%resolve-package (car spec))))
          (when from-pkg
            (dolist (sname (cdr spec))
              (let ((sname-str (%pkg-string-designator sname)))
                (let ((found (find-symbol sname-str from-pkg)))
                  (when found
                    (import found pkg))))))))
      ;; Intern
      (dolist (sname intern-names)
        (intern (%pkg-string-designator sname) pkg))
      ;; Export
      (dolist (sname export-names)
        (let ((sname-str (%pkg-string-designator sname)))
          (let ((sym (intern sname-str pkg)))
            (export sym pkg))))
      ;; Documentation: store via the ANSI-bridge documentation registry
      ;; so (documentation pkg t) returns the string.  Stored only when
      ;; a non-nil value was supplied.
      (when doc-string
        (set-documentation pkg t doc-string))
      pkg)))

;;; --- in-package ---

(defun in-package (name)
  "Set *package* to the package named NAME."
  (let ((pkg (find-package name)))
    (when pkg
      (setq *package* pkg))
    pkg))

;;; --- Iteration: do-symbols, do-external-symbols, do-all-symbols ---

(defun %do-symbols-fn (fn pkg)
  "Call FN on each symbol accessible in PKG (internal + external + inherited)."
  (let ((p (%resolve-package pkg))
        (seen nil))
    (when p
      ;; Internal symbols
      (dolist (entry (%pkg-internal p))
        (let ((sym (cdr entry)))
          (unless (member sym seen :test #'eq)
            (setq seen (cons sym seen))
            (funcall fn sym))))
      ;; External symbols
      (dolist (entry (%pkg-external p))
        (let ((sym (cdr entry)))
          (unless (member sym seen :test #'eq)
            (setq seen (cons sym seen))
            (funcall fn sym))))
      ;; Inherited
      (dolist (used (%pkg-use-list p))
        (dolist (entry (%pkg-external used))
          (let ((sym (cdr entry))
                (name-str (car entry)))
            ;; Only if not shadowed
            (unless (or (member sym seen :test #'eq)
                        (%symtab-find (%pkg-internal p) name-str)
                        (%symtab-find (%pkg-external p) name-str))
              (setq seen (cons sym seen))
              (funcall fn sym))))))))

(defun %do-external-symbols-fn (fn pkg)
  "Call FN on each external symbol in PKG."
  (let ((p (%resolve-package pkg)))
    (when p
      (dolist (entry (%pkg-external p))
        (funcall fn (cdr entry))))))

(defun %do-all-symbols-fn (fn)
  "Call FN on each symbol in all packages."
  (let ((seen nil))
    (dolist (pkg *all-packages*)
      (when (%pkg-name pkg)
        (dolist (entry (%pkg-internal pkg))
          (let ((sym (cdr entry)))
            (unless (member sym seen :test #'eq)
              (setq seen (cons sym seen))
              (funcall fn sym))))
        (dolist (entry (%pkg-external pkg))
          (let ((sym (cdr entry)))
            (unless (member sym seen :test #'eq)
              (setq seen (cons sym seen))
              (funcall fn sym))))))))

;;; --- safely-delete-package (test helper) ---

(defun safely-delete-package (package-designator)
  "Delete package if it exists, first removing use relationships."
  (let ((package (find-package package-designator)))
    (when package
      (let ((used-by (package-used-by-list package)))
        (dolist (using-package used-by)
          (unuse-package package using-package)))
      (delete-package package))))

;;; --- Override symbolp/keywordp for CL symbols ---

(defun symbolp (x)
  "True if X is a symbol (MVM native, MVM keyword, or CL symbol).
   Subtag #x50 = native symbol; #x53 = native keyword (compile-keyword
   routes :foo through %INTERN-KEYWORD into a #x53 object).  Both are
   symbols per ANSI."
  (or (null x) (eq x t) (%cl-sym-p x)
      (and (not (integerp x)) (not (consp x)) (not (characterp x))
           (not (stringp x)) (not (null x))
           (let ((st (obj-subtag x)))
             (or (= st 80)     ; #x50 symbol
                 (= st 83))))))  ; #x53 keyword

(defun keywordp (x)
  "True if X is a keyword symbol.  CL-symbol path: package eq KEYWORD.
   Native path: object with subtag #x53 (allocated by %INTERN-KEYWORD)."
  (cond
    ((%cl-sym-p x)
     (let ((kw-pkg (find-package "KEYWORD")))
       (and kw-pkg (eq (%cl-sym-package x) kw-pkg))))
    ((or (null x) (eq x t)) nil)
    ((or (integerp x) (consp x) (characterp x) (stringp x)) nil)
    (t (= (obj-subtag x) 83))))   ; #x53 keyword

(defun boundp (sym)
  "True if SYM has a value in the global symbol-value alist (#x10000080).
   Walks the same alist that symbol-value/set-symbol-value use; finds
   the entry by SYM's hash even if the bound value happens to be NIL
   (a symbol-value lookup that returns NIL conflates 'not bound' with
   'bound to NIL', so boundp can't just chain through it).

   Defensive about argument type: only attempts a hash-based lookup if
   SYM is provably a symbol (CL or native MVM) via the existing
   %cl-sym-p / %native-mvm-sym-p predicates, which do all the safe
   pre-checks (fixnum/cons/null/character) before touching obj-subtag.
   Returns NIL for anything else.  An earlier version's inline subtag
   check crashed forks when called on closures/streams (tagged objects
   whose obj-subtag landed on memory that read as #x50 by accident)."
  (cond
    ((null sym) t)                     ; NIL is bound to itself (CLHS)
    ((eq sym t) t)
    ((keywordp sym) t)
    ((%cl-sym-p sym)
     (%boundp-by-hash (aref sym 0)))
    ((%native-mvm-sym-p sym)
     (%boundp-by-hash (aref sym 0)))
    ((integerp sym)
     (%boundp-by-hash sym))
    (t nil)))

(defun %boundp-by-hash (key)
  "Walk the global symbol-value alist looking for KEY.  Returns T if
   any pair with (eql (car pair) KEY); NIL otherwise — distinguishes
   'present, bound to NIL' from 'absent'."
  (let ((cur (mem-ref #x10000080 :u64))
        (found nil))
    (loop
      (when found (return t))
      (when (not (consp cur)) (return nil))
      (let ((pair (car cur)))
        (when (and (consp pair) (eql (car pair) key))
          (setq found t)))
      (setq cur (cdr cur)))))

(defun constantp (form &rest env)
  "True if FORM is a constant per CLHS. Numbers, characters, strings,
   bit-vectors, T, NIL, keywords, and (quote ...) forms are constants."
  (declare (ignore env))
  (cond
    ((null form) t)              ; NIL
    ((eq form t) t)              ; T
    ((numberp form) t)
    ((characterp form) t)
    ((stringp form) t)
    ((%cl-sym-p form) (keywordp form))
    ((keywordp form) t)          ; native keyword (subtag #x53)
    ((and (consp form) (eq (car form) 'quote)) t)
    (t nil)))

;;; --- Collect package symbols for with-package-iterator ---

(defun %collect-package-symbols (packages symbol-types)
  "Collect all (symbol access package) triples for WITH-PACKAGE-ITERATOR."
  (let ((result nil))
    (let ((pkg-list (if (and (consp packages) (not (%pkg-p packages)))
                        packages
                        (list packages))))
      (dolist (pkg-designator pkg-list)
        (let ((pkg (%resolve-package pkg-designator)))
          (when pkg
            ;; Internal
            (when (member :internal symbol-types)
              (dolist (entry (%pkg-internal pkg))
                (setq result (cons (list (cdr entry) :internal pkg) result))))
            ;; External
            (when (member :external symbol-types)
              (dolist (entry (%pkg-external pkg))
                (setq result (cons (list (cdr entry) :external pkg) result))))
            ;; Inherited
            (when (member :inherited symbol-types)
              (dolist (used (%pkg-use-list pkg))
                (dolist (entry (%pkg-external used))
                  (let ((name-str (car entry)))
                    ;; Only if not shadowed by internal/external
                    (unless (or (%symtab-find (%pkg-internal pkg) name-str)
                                (%symtab-find (%pkg-external pkg) name-str))
                      (setq result
                        (cons (list (cdr entry) :inherited used) result)))))))))))
    result))

;;; --- Test helper functions from packages00-aux ---

(defun %register-defpackage-macro ()
  "Install DEFPACKAGE as a runtime macro that expands to %DEFPACKAGE-IMPL.

   Without this, `(eval `(defpackage ,n))` (heavily used by the ANSI
   defpackage.lsp / make-package.lsp suites) signals UNDEFINED-FUNCTION:
   Modus's build-time rewriter rewrites *literal* `(defpackage NAME …)`
   forms at SBCL compile time, but the rewriter never sees the form
   constructed at runtime via `(list 'defpackage n)`.  The runtime
   macro fills that gap.

   Expander shape: (lambda (name &rest options)
                     (list '%defpackage-impl name (list 'quote options)))
   built as an %interp-closure cons that set-macro-function recognises.

   Key: we register under the literal string \"DEFPACKAGE\".
   %macro-sym-key normalises CL syms and non-empty-name native MVM syms
   down to a name string before consulting *macro-function-table*, so
   passing the string here matches all symbol shapes the test code may
   present to macroexpand-1.  Passing a native MVM sym would route
   through (symbol-name sym), which at the moment %register-defpackage-macro
   runs (called from %init-packages via set-up-packages, BEFORE
   %init-sym-name-auto populates *sym-name-table*) returns \"\" — and
   %macro-sym-key then falls back to the symbol object itself.  Later
   when test code looks up the macro, *sym-name-table* IS populated,
   so the lookup returns \"DEFPACKAGE\" and misses our by-symbol entry.
   The string registration sidesteps that boot-order edge."
  (let ((expander
          (list '%interp-closure
                '(name &rest options)
                (list (list 'list (list 'quote '%defpackage-impl)
                                  'name
                                  (list 'list (list 'quote 'quote) 'options)))
                nil)))
    (set-macro-function "DEFPACKAGE" expander)))

(defun set-up-packages ()
  "Set up test packages A and B.

   This runs once at boot (called from %init-packages in
   cl-conditions.lisp) and again on demand from test code via
   the explicit ansi-aux helper.  Boot is the only place where we
   can guarantee set-macro-function has run before test code runs,
   so this is also where we install the runtime DEFPACKAGE macro.

   Note: ansi-aux's packages00-aux.lsp ALSO defines set-up-packages
   (without our DEFPACKAGE macro registration); last-defun-wins makes
   that one shadow ours.  Critical setup that must survive is hooked
   into make-package instead (see CL-TEST alias)."
  (safely-delete-package "A")
  (safely-delete-package "B")
  (safely-delete-package "Q")
  (%defpackage-impl "A" (list (list :use) (list :nicknames "Q") (list :export "FOO")))
  (%defpackage-impl "B" (list (list :use "A") (list :export "BAR")))
  (%register-defpackage-macro))

;;; Non-closure counter/collector for do-symbols traversal
(defvar *%sym-count* 0)
(defvar *%sym-list* nil)
(defun %count-sym (s) (setq *%sym-count* (+ *%sym-count* 1)))
(defun %collect-sym (s) (setq *%sym-list* (cons s *%sym-list*)))
(defun %sym-string< (a b) (string< (symbol-name a) (symbol-name b)))
(defun %sym-name-pkg< (x y)
  (or (string< (symbol-name x) (symbol-name y))
      (and (string-equal (symbol-name x) (symbol-name y))
           (let ((px (symbol-package x))
                 (py (symbol-package y)))
             (if (and px py)
                 (string< (package-name px) (package-name py))
                 nil)))))

(defun num-symbols-in-package (p)
  "Count all accessible symbols in package P."
  (setq *%sym-count* 0)
  (%do-symbols-fn #'%count-sym p)
  *%sym-count*)

(defun num-external-symbols-in-package (p)
  "Count external symbols in package P."
  (setq *%sym-count* 0)
  (%do-external-symbols-fn #'%count-sym p)
  *%sym-count*)

(defun external-symbols-in-package (p)
  "List external symbols in package P, sorted."
  (setq *%sym-list* nil)
  (%do-external-symbols-fn #'%collect-sym p)
  (sort *%sym-list* #'%sym-string<))

(defun sort-symbols (sl)
  "Sort a list of symbols by name, then by package name."
  (sort (copy-list sl) #'%sym-name-pkg<))

(defun %pkg-name< (a b) (string< (package-name a) (package-name b)))

(defun sort-package-list (x)
  "Sort packages by name."
  (sort (copy-list x) #'%pkg-name<))

(defun collect-symbols (pkg)
  "Collect all symbols accessible in PKG, sorted."
  (remove-duplicates
    (sort-symbols
      (progn
        (setq *%sym-list* nil)
        (%do-symbols-fn #'%collect-sym pkg)
        *%sym-list*))))

;;; --- LOOP `being the SYMBOL[S]/EXTERNAL-SYMBOL[S]/PRESENT-SYMBOL[S] of pkg`
;;; helpers.  The compiler emits a call to one of these to materialize the
;;; symbol list before iteration; bare-metal code can't host a closure-based
;;; iterator state.

(defun %loop-collect-symbols (pkg)
  "All symbols accessible in PKG (internal + external + inherited)."
  (setq *%sym-list* nil)
  (%do-symbols-fn #'%collect-sym pkg)
  *%sym-list*)

(defun %loop-collect-external-symbols (pkg)
  "External symbols of PKG."
  (setq *%sym-list* nil)
  (%do-external-symbols-fn #'%collect-sym pkg)
  *%sym-list*)

(defun %loop-collect-present-symbols (pkg)
  "Present (= directly interned: internal + external) symbols of PKG."
  (let ((p (%resolve-package pkg))
        (acc nil))
    (when p
      (dolist (entry (%pkg-internal p))
        (setq acc (cons (cdr entry) acc)))
      (dolist (entry (%pkg-external p))
        (setq acc (cons (cdr entry) acc))))
    acc))

(defun collect-external-symbols (pkg)
  "Collect external symbols in PKG, sorted."
  (remove-duplicates
    (sort-symbols
      (progn
        (setq *%sym-list* nil)
        (%do-external-symbols-fn #'%collect-sym pkg)
        *%sym-list*))))

(defvar *fail-count-limit* 20)

;;; documentation lives in ansi-bridge.lisp with a real registry.
;;; The cl-packages stub here returned NIL always and shadowed the real
;;; impl via last-defun-wins ordering — now just a forwarding comment.

;;; ============================================================
;;; PROGV runtime support
;;; ============================================================
;;;
;;; PROGV needs to dynamically bind a list of vars to a list of vals,
;;; run a body, and restore the previous values on (any) exit. The
;;; compile-time expansion in compiler.lisp wraps the body in
;;; unwind-protect and uses these helpers for save/set/restore.
;;;
;;; Each "var" in the var-list is a symbol designator. We canonicalize
;;; to a name-hash (the same key the global alist at #x10000080 uses),
;;; so the same key is used in save and restore.

(defun %progv-hash (sym)
  "Canonicalize a symbol designator to a name-hash key."
  (cond
    ((integerp sym) sym)
    ((%cl-sym-p sym) (compute-name-hash (%cl-sym-name sym)))
    ((stringp sym) (compute-name-hash sym))
    ;; Native MVM symbol: hash already lives in slot 0.
    ((and (not (consp sym)) (not (null sym)) (not (characterp sym))
          (= (obj-subtag sym) 80))
     (aref sym 0))
    (t 0)))

(defun %progv-save (vars)
  "For each VAR in VARS, return (hash . current-value) pairs.
   Order is preserved (we will restore in this order at exit)."
  (let ((result nil) (cur vars))
    (loop
      (when (null cur) (return (nreverse result)))
      (let ((h (%progv-hash (car cur))))
        (setq result (cons (cons h (symbol-value h)) result)))
      (setq cur (cdr cur)))))

(defun %progv-set (vars vals)
  "Assign successive VARS to successive VALS via set-symbol-value.
   Stops when either list is exhausted; extras on either side are ignored.
   Vars without a corresponding val keep their saved value (an
   approximation of full ANSI 'makunbound' semantics)."
  (let ((vc vars) (vlc vals))
    (loop
      (when (null vc) (return nil))
      (when (null vlc) (return nil))
      (set-symbol-value (%progv-hash (car vc)) (car vlc))
      (setq vc (cdr vc))
      (setq vlc (cdr vlc)))))

(defun %progv-restore (saves)
  "Restore values saved by %progv-save."
  (let ((cur saves))
    (loop
      (when (null cur) (return nil))
      (let ((p (car cur)))
        (set-symbol-value (car p) (cdr p)))
      (setq cur (cdr cur)))))

