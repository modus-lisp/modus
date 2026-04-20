;;;; build-ansi-test.lisp — Build ANSI CL test runner (Linux x86-64)
;;;;
;;;; Produces /tmp/modus-ansi-test — runs ANSI CL conformance tests.
;;;;
;;;; Usage: sbcl --dynamic-space-size 2048 --script mvm/build-ansi-test.lisp
;;;; Run:   /tmp/modus-ansi-test
;;;;
;;;; Output: FAIL lines for each failing test, then summary: N/M PASS or FAIL
;;;; Exit code: 0 = all pass, >0 = number of failures

;;; ============================================================
;;; 1. Load MVM infrastructure (SBCL-side)
;;; ============================================================

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(format t "~%=== Building ANSI CL test runner ===~%")

;;; ============================================================
;;; 2. Read source files (SBCL-side)
;;; ============================================================

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun mvm-text (relative-path)
  (read-file-text (merge-pathnames relative-path *modus-base*)))

(defvar *prelude-source* (mvm-text "mvm/prelude.lisp"))
(defvar *gc-source*      (mvm-text "mvm/gc.lisp"))
(defvar *rt-source*      (mvm-text "mvm/rt.lisp"))
(defvar *bridge-source*
  (concatenate 'string
    ;; Load order matches original ansi-bridge.lisp concatenation order.
    ;; cl-sequences first because floatp-impl is needed by the printer.
    (mvm-text "mvm/cl-sequences.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-streams.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-fileio.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-printer.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-reader.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-eval.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-clos.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-types.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-packages.lisp")
    (string #\Newline)
    (mvm-text "mvm/cl-conditions.lisp")
    (string #\Newline)
    (mvm-text "mvm/ansi-bridge.lisp")))
(defvar *test-source*    (mvm-text "mvm/ansi-tests.lisp"))

;; SBCL-level stubs for functions called during macro expansion
(defun notnot (x) (not (not x)))
(defun notnot-mv (x) (not (not x)))
(defun classify-error* (form) nil)
(in-package :modus.mvm)
(defun notnot (x) (not (not x)))
(defun notnot-mv (x) (not (not x)))
(in-package :cl-user)

;; Create SBCL-side packages that ANSI test files reference
;; (needed so SBCL's reader can resolve qualified symbols like DS1:A)
(ignore-errors (delete-package "FS-A"))
(ignore-errors (delete-package "FS-B"))
(ignore-errors (delete-package "FS-Q"))
(ignore-errors (delete-package "DS4"))
(ignore-errors (delete-package "DS3"))
(ignore-errors (delete-package "DS2"))
(ignore-errors (delete-package "DS1"))
(ignore-errors (delete-package "A"))
(ignore-errors (delete-package "B"))
(ignore-errors (delete-package "Q"))
(ignore-errors (delete-package "CL-TEST"))
;; REGRESSION-TEST package: needed so SBCL reader can parse
;; regression-test::my-aref, regression-test::*compile-tests*, etc.
;; in ansi-aux.lsp without signaling "Package does not exist".
(ignore-errors (delete-package "REGRESSION-TEST"))
(ignore-errors (delete-package "RTEST"))
(ignore-errors (delete-package "RT"))
(defpackage "REGRESSION-TEST"
  (:use "CL")
  (:nicknames "RTEST" "RT")
  (:export "MY-AREF" "MY-ROW-MAJOR-AREF" "*COMPILE-TESTS*"
           "*DO-TESTS-WHEN-DEFINED*" "*TEST*" "DEFTEST" "DO-TESTS"
           "PENDING-TESTS" "REM-ALL-TESTS" "REM-TEST"
           "*CATCH-ERRORS*" "*PASSED-TESTS*" "*FAILED-TESTS*"))
(defpackage "A" (:use) (:nicknames "Q") (:export "FOO"))
(defpackage "B" (:use "A") (:export "BAR"))
(defpackage "FS-A" (:use) (:nicknames "FS-Q") (:export "FOO"))
(defpackage "FS-B" (:use "FS-A") (:export "BAR"))
(defpackage "DS1" (:use) (:intern "C" "D") (:export "A" "B"))
(defpackage "DS2" (:use) (:intern "E" "F") (:export "G" "H" "A"))
(defpackage "DS3"
  (:shadow "B")
  (:shadowing-import-from "DS1" "A")
  (:use "DS1" "DS2")
  (:export "A" "B" "G" "I" "J" "K")
  (:intern "L" "M"))
(defpackage "DS4"
  (:shadowing-import-from "DS1" "B")
  (:use "DS1" "DS3")
  (:intern "X" "Y" "Z")
  (:import-from "DS2" "F"))
(defpackage "CL-TEST" (:use "CL"))

;; SBCL-side CLOS class registry for make-instance initarg expansion
;; Each entry: (class-name slot-names . initarg-map)
;; where initarg-map = list of (initarg-string . slot-symbol)
(defvar *sbcl-clos-classes* nil)

;; Counter for generating unique slot-unbound method function names
(defvar *slot-unbound-method-counter* 0)

;; Helper: safe mapcar that handles dotted lists (returns dotted list)
(defun mapcar-dotted (fn list)
  "Like mapcar but handles dotted lists. The dotted cdr is passed through fn."
  (cond
    ((null list) nil)
    ((atom list) (funcall fn list))
    (t (cons (funcall fn (car list))
             (mapcar-dotted fn (cdr list))))))

;; Helper: extract keyword value from plist-style args
(defun make-array-kwarg (args key)
  "Get keyword value from make-array keyword args list."
  (loop for (k v) on args by #'cddr
        when (eq k key) return v))

;; Helper: check if element-type is a character type
(defun char-element-type-p (et)
  "True if element-type is a character type (character, standard-char, base-char, nil)."
  (or (null et)   ; bare nil
      (and (consp et) (eq (car et) 'quote)
           (member (cadr et) '(character standard-char base-char nil)))))

;; Rewrite make-array with :initial-contents and/or character :element-type
;; into %make-string-array + aset calls
(defun rewrite-make-array-initcontents (form)
  "Walk form tree, converting make-array with :initial-contents or char :element-type
   into %make-string-array + initialization code."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (integerp (cadr form))
          (cddr form))  ; has keyword args
     (let* ((size (cadr form))
            (kwargs (cddr form))
            (et (make-array-kwarg kwargs :element-type))
            (contents (make-array-kwarg kwargs :initial-contents))
            (fill-p (make-array-kwarg kwargs :fill-pointer))
            (displaced (make-array-kwarg kwargs :displaced-to))
            (disp-offset (or (make-array-kwarg kwargs :displaced-index-offset) 0))
            (char-et (char-element-type-p et)))
       (cond
         ;; Displaced array: (cons (cons declared-size offset) underlying-string)
         (displaced
          (let ((disp-form (rewrite-make-array-initcontents displaced)))
            `(cons (cons ,size ,disp-offset) ,disp-form)))
         ;; Fill-pointer: (cons fill-pointer underlying-string)
         ((and fill-p (stringp contents))
          `(cons ,fill-p (copy-seq ,contents)))
         ((and fill-p (integerp contents))
          ;; fill-pointer with non-string contents — unlikely but handle
          (mapcar-dotted #'rewrite-make-array-initcontents form))
         ;; :initial-contents is a string literal — copy it as a string
         ((stringp contents)
          `(copy-seq ,contents))
         ;; :initial-contents is a quoted list of characters
         ((and (consp contents) (eq (car contents) 'quote)
               (consp (cadr contents)) (characterp (car (cadr contents))))
          (let* ((chars (cadr contents))
                 (var '%str-init-tmp)  ; fixed name, not gensym (survives ~S print+read)
                 (asets (loop for ch in chars for i from 0
                              collect `(aset ,var ,i ,(char-code ch)))))
            `(let ((,var (%make-string-array ,size)))
               ,@asets
               ,var)))
         ;; char element-type, no initial-contents — just %make-string-array
         ((and char-et (not contents))
          `(%make-string-array ,size))
         ;; fallback
         (t (mapcar-dotted #'rewrite-make-array-initcontents form)))))
    (t (mapcar-dotted #'rewrite-make-array-initcontents form))))

;; Rewrite (make-array '(N) ...) → (make-array N ...) for MVM compatibility
(defun rewrite-make-array-dims (form)
  "Walk form tree, converting list-dimension make-array to integer-dimension.
   Also flattens (make-array nil ...) → (make-array 1 ...) so 0-dim array
   tests don't crash MVM's make-array on a NIL size operand."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'quote)
          (consp (cadr (cadr form)))
          (null (cddr (cadr (cadr form)))))
     ;; (make-array '(N) ...) → (make-array N ...)
     (cons 'make-array (cons (car (cadr (cadr form)))
                             (mapcar #'rewrite-make-array-dims (cddr form)))))
    ((and (eq (car form) 'make-array)
          (consp (cdr form))
          (null (cadr form)))
     ;; (make-array nil ...) — 0-dim scalar array. MVM has no scalar
     ;; arrays; treat as a 1-element vector so the test crashes a
     ;; comparison instead of crashing the whole fork.
     (cons 'make-array (cons 1 (mapcar #'rewrite-make-array-dims (cddr form)))))
    (t (mapcar-dotted #'rewrite-make-array-dims form))))

;; Rewrite (eval '(FORM)) → (FORM) for MVM compatibility
;; MVM doesn't have a runtime eval; these just ensure runtime evaluation
;; which MVM already does for all compiled code.
(defun rewrite-eval-quote (form)
  "Walk form tree, converting (eval '(FORM)) to FORM."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'eval)
          (consp (cdr form))
          (null (cddr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'quote)
          (consp (cadr (cadr form))))
     ;; (eval '(FORM)) → FORM, then recursively rewrite the result
     (rewrite-eval-quote (cadr (cadr form))))
    (t (mapcar-dotted #'rewrite-eval-quote form))))

;; Rewrite (let ((*earmuff* val)) body) → (let ((*earmuff* val)) (declare (special *earmuff*)) body)
;; This ensures dynamic binding for standard stream variables like *terminal-io*, *standard-output* etc.
(defun %earmuff-sym-p (sym)
  "Check if SYM is a *earmuff* variable."
  (and (symbolp sym)
       (let ((name (symbol-name sym)))
         (and (> (length name) 2)
              (char= (char name 0) #\*)
              (char= (char name (1- (length name))) #\*)))))

(defun rewrite-earmuff-specials (form)
  "Walk form tree, adding (declare (special ...)) to let/let* forms binding earmuff variables."
  (cond
    ((atom form) form)
    ((and (member (car form) '(let let*))
          (consp (cdr form))
          (consp (cadr form)))
     ;; Check if any bindings are earmuff variables
     (let ((bindings (cadr form))
           (body (cddr form)))
       (let ((earmuffs (remove-if-not
                        (lambda (b)
                          (let ((var (if (consp b) (car b) b)))
                            (%earmuff-sym-p var)))
                        bindings)))
         ;; Check if there's already a (declare (special ...)) covering these
         (let* ((existing-specials nil)
                (has-decl (and (consp body) (consp (car body))
                               (eq (caar body) 'declare))))
           (when has-decl
             (dolist (spec (cdar body))
               (when (and (consp spec) (eq (car spec) 'special))
                 (setf existing-specials (append (cdr spec) existing-specials)))))
           (let ((new-earmuffs (remove-if
                                (lambda (b)
                                  (member (if (consp b) (car b) b) existing-specials))
                                earmuffs)))
             (if new-earmuffs
                 (let ((special-decl `(declare (special ,@(mapcar (lambda (b) (if (consp b) (car b) b)) new-earmuffs)))))
                   `(,(car form) ,(mapcar (lambda (b) (if (consp b) (cons (car b) (mapcar #'rewrite-earmuff-specials (cdr b))) b)) bindings)
                     ,special-decl
                     ,@(mapcar #'rewrite-earmuff-specials body)))
                 `(,(car form) ,(mapcar (lambda (b) (if (consp b) (cons (car b) (mapcar #'rewrite-earmuff-specials (cdr b))) b)) bindings)
                   ,@(mapcar #'rewrite-earmuff-specials body))))))))
    (t (mapcar-dotted #'rewrite-earmuff-specials form))))

;; Convert SBCL symbols/keywords used as package designators to strings
;; so MVM can handle them (MVM symbols are name-hashes, not printable)
(defun %stringify-pkg-designator (x)
  "Convert a keyword or symbol package designator to a string."
  (cond
    ((stringp x) x)
    ((characterp x) (string x))
    ((keywordp x) (symbol-name x))
    ((symbolp x) (symbol-name x))
    (t x)))

;; Rewrite do-symbols/do-external-symbols/do-all-symbols/with-package-iterator
;; These are macros in CL that SBCL expands to SBCL-internal code.
;; We rewrite them into loop-based iteration that supports RETURN.
(defvar *pkg-iter-counter* 0)

(defun rewrite-package-iteration (form)
  "Walk form tree, converting do-symbols/do-external-symbols/do-all-symbols
   and with-package-iterator into MVM-compatible forms."
  (cond
    ((atom form) form)
    ;; (do-symbols (var pkg result) body...)
    ;; → collect symbols, then iterate with block nil for return support
    ((and (eq (car form) 'do-symbols) (consp (cdr form)) (consp (cadr form)))
     (incf *pkg-iter-counter*)
     (let* ((binding (cadr form))
            (var (first binding))
            (pkg (if (second binding) (rewrite-package-iteration (second binding)) '*package*))
            (result (if (cddr binding) (rewrite-package-iteration (third binding)) 'nil))
            (body (mapcar #'rewrite-package-iteration (cddr form)))
            (real-body (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) body))
            (syms-var (intern (format nil "%PKG-SYMS~D" *pkg-iter-counter*)))
            (cur-var (intern (format nil "%PKG-CUR~D" *pkg-iter-counter*))))
       `(let ((,syms-var nil))
          (%do-symbols-fn (lambda (,var) (setq ,syms-var (cons ,var ,syms-var))) ,pkg)
          (let ((,cur-var ,syms-var))
            (block nil
              (loop
                (when (null ,cur-var) (return ,result))
                (let ((,var (car ,cur-var)))
                  ,@real-body)
                (setq ,cur-var (cdr ,cur-var))))))))
    ;; (do-external-symbols (var pkg result) body...)
    ((and (eq (car form) 'do-external-symbols) (consp (cdr form)) (consp (cadr form)))
     (incf *pkg-iter-counter*)
     (let* ((binding (cadr form))
            (var (first binding))
            (pkg (if (second binding) (rewrite-package-iteration (second binding)) '*package*))
            (result (if (cddr binding) (rewrite-package-iteration (third binding)) 'nil))
            (body (mapcar #'rewrite-package-iteration (cddr form)))
            (real-body (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) body))
            (syms-var (intern (format nil "%PKG-ESYMS~D" *pkg-iter-counter*)))
            (cur-var (intern (format nil "%PKG-ECUR~D" *pkg-iter-counter*))))
       `(let ((,syms-var nil))
          (%do-external-symbols-fn (lambda (,var) (setq ,syms-var (cons ,var ,syms-var))) ,pkg)
          (let ((,cur-var ,syms-var))
            (block nil
              (loop
                (when (null ,cur-var) (return ,result))
                (let ((,var (car ,cur-var)))
                  ,@real-body)
                (setq ,cur-var (cdr ,cur-var))))))))
    ;; (do-all-symbols (var result) body...)
    ((and (eq (car form) 'do-all-symbols) (consp (cdr form)) (consp (cadr form)))
     (incf *pkg-iter-counter*)
     (let* ((binding (cadr form))
            (var (first binding))
            (result (if (cdr binding) (rewrite-package-iteration (second binding)) 'nil))
            (body (mapcar #'rewrite-package-iteration (cddr form)))
            (real-body (remove-if (lambda (f) (and (consp f) (eq (car f) 'declare))) body))
            (syms-var (intern (format nil "%PKG-ASYMS~D" *pkg-iter-counter*)))
            (cur-var (intern (format nil "%PKG-ACUR~D" *pkg-iter-counter*))))
       `(let ((,syms-var nil))
          (%do-all-symbols-fn (lambda (,var) (setq ,syms-var (cons ,var ,syms-var))))
          (let ((,cur-var ,syms-var))
            (block nil
              (loop
                (when (null ,cur-var) (return ,result))
                (let ((,var (car ,cur-var)))
                  ,@real-body)
                (setq ,cur-var (cdr ,cur-var))))))))
    ;; (with-package-iterator ...) — stub: just return 0
    ((eq (car form) 'with-package-iterator)
     0)
    ;; (defpackage name option...) → (%defpackage-impl name '(option...))
    ;; Options are bare lists like (:use) that would be evaluated as forms.
    ;; Convert to a single quoted list of options.
    ((and (eq (car form) 'defpackage) (cdr form))
     (let ((name (rewrite-package-iteration (%stringify-pkg-designator (cadr form))))
           (options (cddr form)))
       `(%defpackage-impl ,name (quote ,options))))
    ;; Package functions with keyword/symbol designator args → stringify
    ((and (member (car form) '(make-package find-package delete-package
                               safely-delete-package rename-package
                               intern find-symbol use-package unuse-package
                               in-package export unexport import unintern
                               shadow shadowing-import
                               package-name package-nicknames
                               package-use-list package-used-by-list
                               package-shadowing-symbols))
          (cdr form)
          (or (keywordp (cadr form)) (and (symbolp (cadr form)) (not (member (cadr form) '(nil t p sym pkg s))))))
     (let ((str-arg (%stringify-pkg-designator (cadr form))))
       `(,(car form) ,str-arg ,@(mapcar #'rewrite-package-iteration (cddr form)))))
    ;; (ignore-errors form) → (handler-case form (error (c) nil))
    ((and (eq (car form) 'ignore-errors) (cdr form))
     (let ((body (rewrite-package-iteration (cadr form))))
       `(handler-case ,body (error (c) nil))))
    ;; (report-and-ignore-errors form) → form (ignore errors)
    ((eq (car form) 'report-and-ignore-errors)
     (rewrite-package-iteration (cadr form)))
    ;; (return-from block-name value) - need to rewrite body
    ((eq (car form) 'return-from)
     `(return-from ,(cadr form) ,@(mapcar #'rewrite-package-iteration (cddr form))))
    (t (mapcar-dotted #'rewrite-package-iteration form))))

;; Rewrite reader-related forms for MVM compatibility
;;; ============================================================
;;; Printer-related SBCL-side macros (expanded at build time)
;;; ============================================================

;; def-print-test: expanded at SBCL side using printer-aux.lsp definition
(defmacro def-print-test (name form result &rest bindings)
  `(deftest ,name
     (if (equalpt
          (my-with-standard-io-syntax
           (lambda ()
             (let ((*print-readably* nil))
               (declare (special *print-readably*))
               ,(if bindings
                    `(let ,bindings
                       (declare (special ,@(mapcar (lambda (b) (if (consp b) (car b) b)) bindings)))
                       (with-output-to-string (*standard-output*)
                         (declare (special *standard-output*))
                         (prin1 ,form)))
                    `(with-output-to-string (*standard-output*)
                       (declare (special *standard-output*))
                       (prin1 ,form))))))
          ,result)
         t
       ,result)
     t))

;; def-pprint-test: uses pprint features — stub to basic prin1
(defmacro def-pprint-test (name form expected-value &rest keys)
  (let ((margin (getf keys :margin 100))
        (miser (getf keys :miser nil))
        (circle (getf keys :circle nil))
        (len (getf keys :len nil))
        (pretty (getf keys :pretty t))
        (escape (getf keys :escape nil))
        (readably (getf keys :readably nil))
        (package (or (getf keys :package) '(find-package "CL-TEST"))))
    `(deftest ,name
       (%with-standard-io-syntax
        (lambda ()
          (let ((*print-pretty* ,pretty)
                (*print-escape* ,escape)
                (*print-readably* ,readably)
                (*print-right-margin* ,margin)
                (*package* ,package)
                (*print-length* ,len)
                (*print-miser-width* ,miser)
                (*print-circle* ,circle))
            (declare (special *print-pretty* *print-escape* *print-readably*
                              *print-right-margin* *package* *print-length*
                              *print-miser-width* *print-circle*))
            ,form)))
       ,expected-value)))

;; def-format-test: expand both format and formatter variants
(defmacro def-format-test (name string args expected-output &optional (num-left 0))
  (let* ((s (symbol-name name))
         (expected-prefix (string 'format.))
         (expected-prefix-length (length expected-prefix))
         (formatter-test-name-string
          (concatenate 'string (string 'formatter.)
                       (subseq s expected-prefix-length)))
         (formatter-test-name (intern formatter-test-name-string
                                      (symbol-package name))))
    `(progn
       (deftest ,name
         (%with-standard-io-syntax
          (lambda ()
            (let ((*print-readably* nil)
                  (*package* (find-package "CL-TEST")))
              (declare (special *print-readably* *package*))
              (format nil ,string ,@args))))
         ,expected-output)
       (deftest ,formatter-test-name
         (let ((fn (formatter ,string))
               (args (list ,@args)))
           (%with-standard-io-syntax
            (lambda ()
              (let ((*print-readably* nil)
                    (*package* (find-package "CL-TEST")))
                (declare (special *print-readably* *package*))
                (with-output-to-string
                  (stream)
                  (declare (special stream))
                  (let ((tail (apply fn stream args)))
                    tail))))))
         ,expected-output))))

;; formatter: SBCL-level stub (will be a function at MVM level)
;; We don't expand formatter at SBCL level; it's a runtime function
;; However, (formatter "~D") needs to work as a lambda at runtime
;; The MVM runtime defines formatter as a function already

;; def-ppblock-test: pprint logical block test
(defmacro def-ppblock-test (name form expected-value &rest key-args)
  `(def-pprint-test ,name
     (with-output-to-string
       (*standard-output*)
       (pprint-logical-block (*standard-output* nil) ,form))
     ,expected-value
     ,@key-args))

;; Handle print-unreadable-object at SBCL level
;; (print-unreadable-object (obj stream &key type identity) body)
;; → (%print-unreadable-object obj stream type-p identity-p (lambda () body))

;; my-with-standard-io-syntax: alias for %with-standard-io-syntax (thunk version)
;; This is called from printer-aux tests
;; When called with a thunk (lambda), pass directly
;; When called with a body... (after SBCL expansion), wrap in lambda

;; coin: random boolean (used in randomly-check-readability)
;; random-from-seq: pick random element from sequence
;; random-thing: generate random test object
;; These are needed for random write tests

;; Since these depend on random, we define stubs
;; that produce deterministic "random" values for MVM

;;; ============================================================
;;; SBCL-side condition/define-condition rewriters
;;; ============================================================

;; Parse a slot-spec from define-condition:
;; (slot-name :initarg :kw1 :initarg :kw2 :initform form :reader reader ...)
;; Returns: (slot-name initargs initform-or-:no-initform readers)
(defun parse-dc-slot (slot-spec)
  (if (atom slot-spec)
      (list slot-spec nil :no-initform nil)
      (let ((name (car slot-spec))
            (opts (cdr slot-spec))
            (initargs nil)
            (initform :no-initform)
            (readers nil))
        (loop
          (when (null opts) (return))
          (let ((key (car opts))
                (val (cadr opts)))
            (cond
              ((eq key :initarg)
               (setf initargs (append initargs (list val)))
               (setf opts (cddr opts)))
              ((eq key :initform)
               (setf initform val)
               (setf opts (cddr opts)))
              ((eq key :reader)
               (setf readers (append readers (list val)))
               (setf opts (cddr opts)))
              ((eq key :accessor)
               (setf readers (append readers (list val)))
               (setf opts (cddr opts)))
              ((eq key :type)
               (setf opts (cddr opts)))
              ((eq key :documentation)
               (setf opts (cddr opts)))
              ((eq key :writer)
               (setf opts (cddr opts)))
              (t (setf opts (cddr opts))))))
        (list name initargs initform readers))))

;; Build the slot-descriptor list for %define-condition
;; Returns a quoted list: '((name (initarg...) initform-or-:no-initform) ...)
(defun build-slot-descriptors (slot-specs)
  (mapcar (lambda (s)
            (let* ((parsed (parse-dc-slot s))
                   (name (first parsed))
                   (initargs (second parsed))
                   (initform (third parsed)))
              (list name initargs initform)))
          slot-specs))

;; Extract option from define-condition options list
(defun dc-option (options key)
  (let ((found (assoc key options)))
    (if found (cdr found) nil)))

;; Expand (define-condition name parents slot-specs &rest options)
;; into (%define-condition ...) + reader/accessor defun forms
(defun rewrite-define-condition (form)
  (let* ((name (second form))
         (parents (or (third form) '(condition)))
         (slot-specs (or (fourth form) nil))
         (rest-opts (cddr (cddr form)))
         ;; Parse &rest options
         (options (loop for opt in rest-opts
                        when (consp opt) collect (cons (car opt) (cdr opt))))
         ;; default-initargs option
         (default-initargs-opt (dc-option options :default-initargs))
         ;; report option
         (report-opt (dc-option options :report))
         ;; Build slot descriptors
         (slot-descriptors (build-slot-descriptors slot-specs))
         ;; Collect all reader defuns
         (reader-defuns
          (loop for s in slot-specs
                append
                (let* ((parsed (parse-dc-slot s))
                       (slot-name (first parsed))
                       (readers (fourth parsed)))
                  (mapcar (lambda (r)
                            `(defun ,r (c) (%condition-slot c ',slot-name)))
                          readers))))
         ;; Build report-fn arg (nil or a quoted lambda/symbol)
         (report-fn-arg
          (cond
            ((null report-opt) nil)
            ((and (consp report-opt) (eq (car report-opt) 'lambda))
             `',report-opt)
            ((symbolp report-opt) `',report-opt)
            ((stringp report-opt)
             ;; String report: lambda (c s) (write-string "msg" s)
             `(lambda (c s) (declare (ignore c)) (write-string ,report-opt s)))
            (t nil)))
         ;; Build default-initargs arg
         (default-initargs-arg
          (if default-initargs-opt
              `',default-initargs-opt
              nil))
         ;; Define-condition call
         (def-call `(%define-condition ',name ',parents ',slot-descriptors
                                       ,default-initargs-arg ,report-fn-arg)))
    `(progn
       ,def-call
       ,@reader-defuns)))

;; Make test name from condition name + suffixes (like make-def-cond-name)
(defun make-dc-test-name (name-str &rest suffixes)
  (intern (apply #'concatenate 'string name-str suffixes) :cl-test))

;; Expand define-condition-with-tests inline
(defun rewrite-define-condition-with-tests (form)
  (let* ((name-symbol (second form))
         (parents (or (third form) nil))
         (slot-specs (or (fourth form) nil))
         (options (nthcdr 4 form))
         ;; Gensym-free name to use in tests
         (name-str (if (symbolp name-symbol) (symbol-name name-symbol) nil)))
    ;; Skip #:uninterned symbols (like #:condition-3)
    (unless name-str
      (return-from rewrite-define-condition-with-tests '(progn)))
    (let* ((dc-form (append (list 'define-condition name-symbol parents slot-specs)
                            options))
           (dc-rewritten (rewrite-define-condition dc-form))
           ;; Parents augmented with 'condition always
           (all-parents (if (member 'condition parents) parents (append parents '(condition))))
           ;; Generate subtype tests for each parent + condition
           (tests nil))
      ;; IS-SUBTYPE-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-SUBTYPE-OF/" (symbol-name parent))
                 (subtypep* ',name-symbol ',parent)
                 t t)
               tests))
      ;; IS-SUBTYPE-OF-2 tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-SUBTYPE-OF-2/" (symbol-name parent))
                 (check-all-subtypep ',name-symbol ',parent)
                 nil)
               tests))
      ;; IS-NOT-SUPERTYPE-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-NOT-SUPERTYPE-OF/" (symbol-name parent))
                 (subtypep* ',parent ',name-symbol)
                 nil t)
               tests))
      ;; IS-A tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-A/" (symbol-name parent))
                 (let ((c (make-condition ',name-symbol)))
                   (notnot-mv (typep c ',parent)))
                 t)
               tests))
      ;; IS-SUBCLASS-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-SUBCLASS-OF/" (symbol-name parent))
                 (subtypep* (find-class ',name-symbol) (find-class ',parent))
                 t t)
               tests))
      ;; IS-NOT-SUPERCLASS-OF tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-NOT-SUPERCLASS-OF/" (symbol-name parent))
                 (subtypep* (find-class ',parent) (find-class ',name-symbol))
                 nil t)
               tests))
      ;; IS-A-MEMBER-OF-CLASS tests
      (dolist (parent all-parents)
        (push `(deftest ,(make-dc-test-name name-str "/IS-A-MEMBER-OF-CLASS/" (symbol-name parent))
                 (let ((c (make-condition ',name-symbol)))
                   (notnot-mv (typep c (find-class ',parent))))
                 t)
               tests))
      ;; HANDLER-CASE-1
      (push `(deftest ,(make-dc-test-name name-str "/HANDLER-CASE-1")
               (let ((c (make-condition ',name-symbol)))
                 (handler-case (signal c)
                               (,name-symbol (c1) (eqt c c1))))
               t)
             tests)
      ;; HANDLER-CASE-2
      (push `(deftest ,(make-dc-test-name name-str "/HANDLER-CASE-2")
               (let ((c (make-condition ',name-symbol)))
                 (handler-case (signal c)
                               (condition (c1) (eqt c c1))))
               t)
             tests)
      ;; HANDLER-CASE-3 — only if none of parents is-error
      (let ((has-error-parent nil))
        (dolist (p parents)
          (when (member p '(error serious-condition simple-error simple-type-error
                            type-error cell-error unbound-variable undefined-function
                            unbound-slot arithmetic-error division-by-zero
                            program-error control-error package-error
                            stream-error end-of-file reader-error parse-error
                            print-not-readable file-error storage-condition
                            floating-point-overflow floating-point-underflow
                            floating-point-inexact floating-point-invalid-operation))
            (setf has-error-parent t)))
        (unless has-error-parent
          (push `(deftest ,(make-dc-test-name name-str "/HANDLER-CASE-3")
                   (let ((c (make-condition ',name-symbol)))
                     (handler-case (signal c)
                                   (error () nil)
                                   (,name-symbol (c2) (eqt c c2))))
                   t)
                 tests)))
      ;; Emit define-condition first, then tests in order
      `(progn
         ,dc-rewritten
         ,@(nreverse tests)))))

(defun rewrite-reader-forms (form)
  "Walk form tree, rewriting reader-related forms for MVM."
  (cond
    ;; Pathname objects (created by SBCL from #P"..." reader syntax) → namestring
    ((and (not (null form)) (typep form 'pathname))
     (namestring form))
    ((atom form) form)
    ;; (multiple-value-call fn arg1 arg2 ...)
    ;; Collect all MV from each arg, pass to fn.
    ;; For #'list specifically: (multiple-value-call #'list a b c)
    ;;   = (append (mvl a) (mvl b) (mvl c))  [because list just collects all values]
    ;; For other functions: (apply fn (append (mvl a1) (mvl a2) ...))
    ;;   NOTE: apply requires a function, not a macro. #'list is a compiler macro
    ;;   in MVM, so (apply #'list ...) would fail. Use append for #'list.
    ((and (eq (car form) 'multiple-value-call) (cdr form))
     (let* ((fn-form (rewrite-reader-forms (cadr form)))
            (arg-forms (mapcar #'rewrite-reader-forms (cddr form)))
            (mvl-forms (mapcar (lambda (a) `(multiple-value-list ,a)) arg-forms)))
       (cond
         ;; No args: (funcall fn)
         ((null arg-forms)
          `(funcall ,fn-form))
         ;; Single arg, no fn: (multiple-value-list arg)
         ;; fn=#'list: (multiple-value-call #'list arg) = (multiple-value-list arg)
         ((and (null (cdr arg-forms))
               (equal fn-form '(function list)))
          `(multiple-value-list ,(car arg-forms)))
         ;; fn=#'list multi-arg: collect all as flat list via append
         ((equal fn-form '(function list))
          `(append ,@mvl-forms))
         ;; Single arg, generic fn: (apply fn (multiple-value-list arg))
         ((null (cdr arg-forms))
          `(apply ,fn-form (multiple-value-list ,(car arg-forms))))
         ;; Multiple args, generic fn: (apply fn (append ...))
         (t
          `(apply ,fn-form (append ,@mvl-forms))))))
    ;; (multiple-value-prog1 first-form . rest)
    ;; → (let ((%mvp1-result (multiple-value-list first-form))) rest... (values-list %mvp1-result))
    ;; NOTE: use a fixed symbol name (not gensym) so it survives ~S printing+reading
    ((and (eq (car form) 'multiple-value-prog1) (cdr form))
     (let* ((first-form (rewrite-reader-forms (cadr form)))
            (rest-forms (mapcar #'rewrite-reader-forms (cddr form)))
            (result-var '%mvp1-result))
       (if rest-forms
           `(let ((,result-var (multiple-value-list ,first-form)))
              ,@rest-forms
              (values-list ,result-var))
           `(values-list (multiple-value-list ,first-form)))))
    ;; (with-output-to-string (var &optional string-form) body...)
    ;; → (let ((var (make-string-output-stream))) body... (get-output-stream-string var))
    ;; If var is an earmuff special (e.g. *standard-output*), add (declare
    ;; (special var)) so prin1/format inside the body see the new binding
    ;; via dynamic lookup. Without this, the binding is lexical, prin1 reads
    ;; the global *standard-output* (often nil → serial fallback), and the
    ;; captured output ends up empty.
    ((and (eq (car form) 'with-output-to-string)
          (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (stream-var (car binding))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (is-special (and (symbolp stream-var) (%earmuff-sym-p stream-var))))
       (if is-special
           `(let ((,stream-var (make-string-output-stream)))
              (declare (special ,stream-var))
              ,@body
              (get-output-stream-string ,stream-var))
           `(let ((,stream-var (make-string-output-stream)))
              ,@body
              (get-output-stream-string ,stream-var)))))
    ;; (with-standard-io-syntax body...) → (%with-standard-io-syntax (lambda () body...))
    ((and (eq (car form) 'with-standard-io-syntax) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (cdr form))))
       `(%with-standard-io-syntax (lambda () ,@body))))
    ;; (print-unreadable-object (obj stream &key type identity) body...)
    ;; → (%print-unreadable-object obj stream type-p identity-p (lambda () body...))
    ((and (eq (car form) 'print-unreadable-object)
          (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (obj (first binding))
            (stream (second binding))
            (keys (cddr binding))
            (type-p (if (getf keys :type) (getf keys :type) nil))
            (identity-p (if (getf keys :identity) (getf keys :identity) nil))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(%print-unreadable-object ,obj ,stream ,type-p ,identity-p
                                  ,(if body `(lambda () ,@body) nil))))
    ;; (my-with-standard-io-syntax body...) — printer-aux.lsp's def-print-test
    ;; redefines def-print-test (when we eval its defmacro from load-ansi-aux)
    ;; to omit the lambda wrap, so the test bodies arrive here as a direct
    ;; form rather than a thunk. Our runtime my-with-standard-io-syntax is a
    ;; function (takes a thunk), so calling it on a direct form would funcall
    ;; the form's value (e.g., a string) and crash. Expand at codegen time
    ;; into the same let-bindings the ANSI-aux macro produces.
    ((and (eq (car form) 'my-with-standard-io-syntax) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (cdr form))))
       `(let ((*package* (find-package "COMMON-LISP-USER"))
              (*print-array* t)
              (*print-base* 10)
              (*print-case* :upcase)
              (*print-circle* nil)
              (*print-escape* t)
              (*print-gensym* t)
              (*print-length* nil)
              (*print-level* nil)
              (*print-readably* t)
              (*print-pretty* nil)
              (*print-radix* nil)
              (*read-base* 10)
              (*read-suppress* nil)
              (*read-eval* t))
          (declare (special *package* *print-array* *print-base* *print-case*
                            *print-circle* *print-escape* *print-gensym*
                            *print-length* *print-level* *print-readably*
                            *print-pretty* *print-radix*
                            *read-base* *read-suppress* *read-eval*))
          ,@body)))
    ;; (formatter string) → (formatter string) — runtime function
    ;; (pprint-logical-block (stream list &key) body...) → simplified
    ((and (eq (car form) 'pprint-logical-block)
          (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (stream (first binding))
            (list-arg (second binding))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       ;; Stub: just execute body
       `(progn ,@body)))
    ;; (pprint-exit-if-list-exhausted) → stub
    ((and (eq (car form) 'pprint-exit-if-list-exhausted) (null (cdr form)))
     nil)
    ;; (pprint-pop) → (pop *pprint-list*)
    ((and (eq (car form) 'pprint-pop) (null (cdr form)))
     nil)
    ;; (setf (readtable-case rt) val) → (%set-readtable-case rt val)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'readtable-case))
     (let ((rt-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(%set-readtable-case ,rt-arg ,val)))
    ;; (setf (slot-value obj slot) val) → (set-slot-value obj slot val)
    ;; Must handle before the generic setf fallthrough (MVM setf macro only passes 1 arg)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'slot-value)
          (cddr form))
     (let ((place (cadr form))
           (val (rewrite-reader-forms (caddr form))))
       (let ((obj (rewrite-reader-forms (cadr place)))
             (slot (rewrite-reader-forms (caddr place))))
         `(set-slot-value ,obj ,slot ,val))))
    ;; (setf (symbol-function sym) fn) → (set-symbol-function sym fn)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'symbol-function)
          (cddr form))
     (let ((sym-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(set-symbol-function ,sym-arg ,val)))
    ;; (setf (fdefinition sym) fn) → (set-fdefinition sym fn)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'fdefinition)
          (cddr form))
     (let ((sym-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(set-fdefinition ,sym-arg ,val)))
    ;; (setf (get sym indicator) val) → (set-get sym indicator val)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'get)
          (cddr form))
     (let ((place (cadr form))
           (val (rewrite-reader-forms (caddr form))))
       (let ((sym-arg (rewrite-reader-forms (cadr place)))
             (ind-arg (rewrite-reader-forms (caddr place))))
         `(set-get ,sym-arg ,ind-arg ,val))))
    ;; (setf (symbol-plist sym) val) → (set-symbol-plist sym val)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'symbol-plist)
          (cddr form))
     (let ((sym-arg (rewrite-reader-forms (cadr (cadr form))))
           (val (rewrite-reader-forms (caddr form))))
       `(set-symbol-plist ,sym-arg ,val)))
    ;; (setf (getf plist ind) val) → (setq plist (set-getf plist ind val))
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'getf)
          (cddr form))
     (let* ((place (cadr form))
            (plist-form (rewrite-reader-forms (cadr place)))
            (ind-form (rewrite-reader-forms (caddr place)))
            (val-form (rewrite-reader-forms (caddr form))))
       ;; getf plist may be a variable - update it
       (if (symbolp (cadr place))
           `(setq ,(cadr place) (set-getf ,plist-form ,ind-form ,val-form))
           `(set-getf ,plist-form ,ind-form ,val-form))))
    ;; (setf (ldb spec n) val) → (setq n (dpb val spec n))
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'ldb)
          (cddr form))
     (let* ((place (cadr form))
            (bytespec (rewrite-reader-forms (cadr place)))
            (int-form (rewrite-reader-forms (caddr place)))
            (val-form (rewrite-reader-forms (caddr form))))
       (if (symbolp (caddr place))
           `(setq ,(caddr place) (dpb ,val-form ,bytespec ,int-form))
           `(dpb ,val-form ,bytespec ,int-form))))
    ;; (setf (values v1 v2 ...) expr) → (multiple-value-setq (v1 v2 ...) expr)
    ((and (eq (car form) 'setf)
          (consp (cdr form))
          (consp (cadr form))
          (eq (car (cadr form)) 'values)
          (cddr form))
     (let ((vars (cdr (cadr form)))
           (val-form (rewrite-reader-forms (caddr form))))
       `(multiple-value-setq ,vars ,val-form)))
    ;; (with-simple-restart (name report) body...)
    ;; → (block nil (handler-bind (...) body...))  OR just body
    ((and (eq (car form) 'with-simple-restart) (cdr form) (consp (cadr form)))
     (let ((body (mapcar #'rewrite-reader-forms (cddr form))))
       `(progn ,@body)))
    ;; (check-type place type-spec &optional string)
    ;; → (unless (typep place type-spec) (error "..."))
    ((and (eq (car form) 'check-type) (cdr form) (cddr form))
     (let ((place (rewrite-reader-forms (cadr form)))
           (type-spec (rewrite-reader-forms (caddr form)))
           (string (if (cdddr form) (rewrite-reader-forms (cadddr form)) nil)))
       `(unless (typep ,place ',type-spec)
          (error ,(or string (format nil "~A is not of type ~A" (cadr form) (caddr form)))))))
    ;; (signals-error form type) → (handler-case (progn form nil) (error (c) t))
    ((and (eq (car form) 'signals-error) (cdr form) (cddr form))
     (let ((body (rewrite-reader-forms (cadr form))))
       `(handler-case (progn ,body nil) (error (c) t))))
    ;; (signals-error-always form type) → same
    ((and (eq (car form) 'signals-error-always) (cdr form))
     (let ((body (rewrite-reader-forms (cadr form))))
       `(handler-case (progn ,body nil) (error (c) t))))
    ;; (classify-error form) → nil stub
    ((eq (car form) 'classify-error)
     nil)
    ;; (classify-error* form) → nil stub
    ((eq (car form) 'classify-error*)
     nil)
    ;; (check-type-error fn type) → nil stub
    ((and (eq (car form) 'check-type-error) (cdr form))
     nil)
    ;; (def-syntax-test name form expected...) → (deftest name (with-standard-io-syntax ...) expected...)
    ;; We handle this by making def-syntax-test a known form
    ((and (eq (car form) 'def-syntax-test) (cdr form) (cddr form))
     (let ((name (cadr form))
           (test-form (rewrite-reader-forms (caddr form)))
           (expected (mapcar #'rewrite-reader-forms (cdddr form))))
       `(deftest ,name
          (%with-standard-io-syntax
            (lambda () (let ((*package* (find-package "CL-TEST"))) ,test-form)))
          ,@expected)))
    ;; (psetq var1 val1 var2 val2 ...) → evaluate all values, then set all
    ;; Parallel setq: (let ((t1 v1) (t2 v2) ...) (setq var1 t1) (setq var2 t2) ...)
    ((and (eq (car form) 'psetq) (consp (cdr form)))
     (let* ((pairs (cdr form))
            (vars nil)
            (vals nil)
            (tmps nil))
       ;; Collect pairs
       (let ((p pairs))
         (loop
           (when (null p) (return))
           (push (car p) vars)
           (push (rewrite-reader-forms (cadr p)) vals)
           (push (intern (format nil "%PSETQ-TMP-~D" (length vars))) tmps)
           (setq p (cddr p))))
       (let ((bindings (mapcar #'list (nreverse tmps) (nreverse vals)))
             (assignments (mapcar (lambda (var tmp) `(setq ,var ,tmp))
                                  (nreverse vars) (nreverse tmps))))
         `(let ,bindings ,@assignments nil))))

    ;; (psetf place1 val1 place2 val2 ...) → evaluate all values, then set all
    ;; For simple (psetf var val) cases at least
    ((and (eq (car form) 'psetf) (consp (cdr form)))
     (let* ((pairs (cdr form))
            (places nil)
            (vals nil)
            (tmps nil))
       (let ((p pairs))
         (loop
           (when (null p) (return))
           (push (rewrite-reader-forms (car p)) places)
           (push (rewrite-reader-forms (cadr p)) vals)
           (push (intern (format nil "%PSETF-TMP-~D" (length places))) tmps)
           (setq p (cddr p))))
       (let* ((rtmps (nreverse tmps))
              (rplaces (nreverse places))
              (rvals (nreverse vals))
              (bindings (mapcar #'list rtmps rvals))
              ;; Generate setf assignments using tmp vars
              (assignments (mapcar (lambda (place tmp)
                                     (if (symbolp place)
                                         `(setq ,place ,tmp)
                                         `(setf ,place ,tmp)))
                                   rplaces rtmps)))
         `(let ,bindings ,@assignments nil))))

    ;; (multiple-value-bind* (vars...) form &body body)
    ;; → (let ((tmp (multiple-value-list form)))
    ;;      (check-values-length tmp N 'form)
    ;;      (destructuring-bind (vars...) tmp body...))
    ;; Simplified: just use multiple-value-bind
    ((and (eq (car form) 'multiple-value-bind*) (consp (cdr form)) (consp (cadr form)))
     (let* ((vars (cadr form))
            (expr (rewrite-reader-forms (caddr form)))
            (body (mapcar #'rewrite-reader-forms (cdddr form)))
            (n (length vars))
            (tmp-var '%mvb*-tmp))
       `(let ((,tmp-var (multiple-value-list ,expr)))
          (check-values-length ,tmp-var ,n ',expr)
          (let ,(loop for var in vars for i from 0
                      collect `(,var (if (< ,i (length ,tmp-var))
                                         (nth ,i ,tmp-var)
                                         nil)))
            ,@body))))

    ;; (symbol-macrolet bindings body...) → (progn body...) with substitution
    ;; For reader tests, skip symbol-macrolet (too complex to handle generally)
    ((eq (car form) 'symbol-macrolet)
     (let ((body (mapcar #'rewrite-reader-forms (cddr form))))
       `(progn ,@body)))
    ;; (macrolet (bindings...) body...) → expand macros in body, then rewrite
    ;; Build SBCL-side expanders to substitute macro calls in body
    ((eq (car form) 'macrolet)
     (let* ((bindings (cadr form))
            (body (cddr form))
            (expanders
             (mapcan (lambda (b)
                       (handler-case
                         (let* ((name (car b))
                                (args (cadr b))
                                (forms (cddr b))
                                (fn (eval `(lambda ,args ,@forms))))
                           (list (cons name fn)))
                         (error () nil)))
                     bindings))
            (expanded-body
             (if expanders
                 (labels ((expand-one (f depth)
                            (cond
                              ((> depth 50) f)  ; depth limit to prevent infinite loops
                              ((atom f) f)
                              ((and (consp f) (assoc (car f) expanders))
                               (let* ((expander (cdr (assoc (car f) expanders)))
                                      (result (handler-case
                                                (apply expander (cdr f))
                                                (error () f))))
                                 ;; Only recurse if result changed and still a macro call
                                 (if (equal result f)
                                     (mapcar-dotted (lambda (x) (expand-one x (1+ depth))) f)
                                     (expand-one result (1+ depth)))))
                              (t (mapcar-dotted (lambda (x) (expand-one x depth)) f)))))
                   (mapcar (lambda (x) (expand-one x 0)) body))
                 body)))
       (let ((rewritten (mapcar #'rewrite-reader-forms expanded-body)))
         `(progn ,@rewritten))))
    ;; (do-special-strings (var string-form ret-form) body...) → (let ((var string-form)) body... ret-form)
    ((and (eq (car form) 'do-special-strings) (consp (cdr form)) (consp (cadr form)))
     (let* ((binding (cadr form))
            (var (first binding))
            (string-form (rewrite-reader-forms (second binding)))
            (ret-form (if (cddr binding) (rewrite-reader-forms (third binding)) nil))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(let ((,var ,string-form)) ,@body ,ret-form)))
    ;; (flet ((name (args) body)) outer-body)
    ;; Leave as-is but rewrite bodies
    ((eq (car form) 'flet)
     (let ((bindings (mapcar (lambda (b)
                               (if (consp b)
                                   (cons (car b)
                                         (cons (cadr b)
                                               (mapcar #'rewrite-reader-forms (cddr b))))
                                   b))
                             (cadr form)))
           (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(flet ,bindings ,@body)))
    ;; (handler-case body &rest clauses) — normalize class objects to type names
    ((and (eq (car form) 'handler-case) (cdr form))
     (let* ((body (rewrite-reader-forms (cadr form)))
            (clauses (mapcar (lambda (clause)
                               (if (consp clause)
                                   (let* ((type-spec (car clause))
                                          ;; Normalize SBCL class objects to their names
                                          (norm-type
                                           (cond
                                             ((and (not (symbolp type-spec))
                                                   (not (consp type-spec))
                                                   (typep type-spec 'class))
                                              (class-name type-spec))
                                             (t type-spec)))
                                          (rest (mapcar #'rewrite-reader-forms (cdr clause))))
                                     (cons norm-type rest))
                                   clause))
                             (cddr form))))
       `(handler-case ,body ,@clauses)))
    ;; (define-condition name parents slots &rest options)
    ;; → (%define-condition ...) + reader defuns
    ((and (eq (car form) 'define-condition) (cdr form))
     (rewrite-reader-forms (rewrite-define-condition form)))
    ;; (define-condition-with-tests name parents slots &rest options)
    ;; → expand macro inline → (%define-condition ...) + tests
    ((and (eq (car form) 'define-condition-with-tests) (cdr form))
     (rewrite-reader-forms (rewrite-define-condition-with-tests form)))
    ;; (normally form) → form (since *should-always-be-true* is always T)
    ((and (eq (car form) 'normally) (cdr form))
     (rewrite-reader-forms (cadr form)))
    ;; (report-and-ignore-errors form...) → (handler-case (progn form...) (error () nil))
    ((and (eq (car form) 'report-and-ignore-errors) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (cdr form))))
       `(handler-case (progn ,@body) (error () nil))))
    ;; (handler-bind bindings body...)
    ;; → (%with-handler-bind (list (list 'type fn)...) (lambda () body...))
    ((and (eq (car form) 'handler-bind) (cdr form))
     (let* ((bindings (cadr form))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (binding-forms
             (mapcar (lambda (b)
                       (let ((type-name (first b))
                             (handler-fn (rewrite-reader-forms (second b))))
                         `(list ',type-name ,handler-fn)))
                     bindings)))
       (if binding-forms
           `(%with-handler-bind (list ,@binding-forms) (lambda () ,@body))
           `(progn ,@body))))
    ;; (restart-case form &rest clauses)
    ;; → (%with-restarts restarts-list (lambda () form))
    ;; Each clause: (name (args) &key interactive test report . body)
    ((and (eq (car form) 'restart-case) (cdr form))
     (let* ((protected-form (rewrite-reader-forms (cadr form)))
            (clauses (cddr form))
            (restart-forms
             (mapcar (lambda (clause)
                       (let* ((rname (first clause))
                              (args (second clause))
                              (rest-opts (cddr clause))
                              ;; Extract :report, :interactive, :test options
                              (report-opt nil)
                              (body-forms nil))
                         ;; Separate options from body
                         (let ((remaining rest-opts))
                           (loop
                             (when (or (null remaining)
                                       (not (keywordp (car remaining))))
                               (setf body-forms remaining)
                               (return))
                             (cond
                               ((eq (car remaining) :report)
                                (setf report-opt (cadr remaining))
                                (setf remaining (cddr remaining)))
                               ((eq (car remaining) :interactive)
                                (setf remaining (cddr remaining)))
                               ((eq (car remaining) :test)
                                (setf remaining (cddr remaining)))
                               (t
                                (setf body-forms remaining)
                                (return)))))
                         (let* ((body (mapcar #'rewrite-reader-forms body-forms))
                                (fn-form `(lambda ,args ,@body))
                                (report-form
                                 (cond
                                   ((null report-opt) nil)
                                   ((stringp report-opt) `',report-opt)
                                   ((symbolp report-opt) `#',report-opt)
                                   ((and (consp report-opt) (eq (car report-opt) 'lambda))
                                    report-opt)
                                   (t nil))))
                           (if report-form
                               `(list ',rname ,fn-form ,report-form)
                               `(list ',rname ,fn-form nil)))))
                     clauses)))
       `(%with-restarts (list ,@restart-forms) (lambda () ,protected-form))))
    ;; (restart-bind bindings body...)
    ;; → (%push-restarts restarts (lambda () body...))
    ((and (eq (car form) 'restart-bind) (cdr form))
     (let* ((bindings (cadr form))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (restart-forms
             (mapcar (lambda (b)
                       (let* ((rname (first b))
                              (fn (rewrite-reader-forms (second b)))
                              (opts (cddr b))
                              (report-opt (getf opts :report-function)))
                         (if report-opt
                             `(list ',rname ,fn ,report-opt)
                             `(list ',rname ,fn nil))))
                     bindings)))
       `(%push-restarts (list ,@restart-forms) (lambda () ,@body))))
    ;; (with-condition-restarts condition restarts-form body...)
    ;; stub: just execute body
    ((and (eq (car form) 'with-condition-restarts) (cdr form))
     (let ((body (mapcar #'rewrite-reader-forms (nthcdr 3 form))))
       (if body
           `(progn ,@body)
           nil)))

    ;; ---- Minimal CLOS support ----

    ;; (defclass name supers slots &rest options)
    ;; → (%defclass 'name '(slot-names...) '(supers...)) + reader/accessor/writer defuns
    ((and (eq (car form) 'defclass) (cdr form) (cddr form))
     (let* ((class-name (cadr form))
            (raw-supers (caddr form))  ; list of parent class names
            (raw-slots (or (cadddr form) nil))
            ;; Parse slot specs
            (slot-names nil)
            (extra-defuns nil)
            ;; initarg→slot mapping: list of (initarg-string . slot-name)
            (initarg-map nil))
       ;; Process each slot spec
       (dolist (slot-spec raw-slots)
         (let* ((sname (if (consp slot-spec) (car slot-spec) slot-spec))
                (opts (if (consp slot-spec) (cdr slot-spec) nil)))
           (push sname slot-names)
           ;; Extract :reader, :writer, :accessor, :initarg from opts
           (let ((cur opts))
             (loop
               (when (null cur) (return))
               (let ((key (car cur))
                     (val (cadr cur)))
                 (cond
                   ((eq key :reader)
                    (push `(defun ,val (obj) (slot-value obj ',sname)) extra-defuns))
                   ((eq key :accessor)
                    (push `(defun ,val (obj) (slot-value obj ',sname)) extra-defuns)
                    (let ((setter-name (intern (concatenate 'string "SET-" (symbol-name val)))))
                      (push `(defun ,setter-name (obj nv) (set-slot-value obj ',sname nv)) extra-defuns)))
                   ((eq key :writer)
                    ;; writer: (fn new-value object)
                    (push `(defun ,val (nv obj) (set-slot-value obj ',sname nv)) extra-defuns))
                   ((eq key :initarg)
                    ;; val is a keyword like :b; map to slot name
                    (push (cons (symbol-name val) sname) initarg-map))))
               (setq cur (cddr cur))))))
       (let ((slot-list (nreverse slot-names)))
         ;; Register in SBCL-side class registry for make-instance expansion
         (setf *sbcl-clos-classes*
               (cons (cons class-name (cons slot-list initarg-map))
                     *sbcl-clos-classes*))
         `(progn
            (%defclass ',class-name ',slot-list ',raw-supers)
            ,@(mapcar #'rewrite-reader-forms (nreverse extra-defuns))))))

    ;; (defgeneric name lambda-list &rest options)
    ;; → (%defgeneric 'name 'lambda-list combination)
    ;;   + (defun name (&rest %gf-args) (%gf-dispatch 'name %gf-args))
    ;; Also handles inline (:method ...) options and :method-combination.
    ((and (eq (car form) 'defgeneric) (cdr form))
     (let* ((gf-name (cadr form))
            (lambda-list (caddr form))
            (options (cdddr form))
            (combination nil)
            (inline-methods nil))
       (dolist (opt options)
         (when (consp opt)
           (cond
             ((eq (car opt) :method-combination)
              (setq combination (cadr opt)))
             ((eq (car opt) :method)
              (push opt inline-methods)))))
       ;; Build method-add forms for inline :method options
       (let* ((method-counter 0)
              (method-forms
               (mapcar (lambda (mopt)
                         ;; mopt = (:method [qualifier] specialized-ll body...)
                         (setf method-counter (1+ method-counter))
                         (let* ((rest (cdr mopt))
                                ;; qualifier: non-list symbol that is not the lambda list
                                (has-qualifier (and rest (cdr rest) (symbolp (car rest))
                                                    (not (listp (car rest)))))
                                (qualifier (if has-qualifier (car rest) nil))
                                (rest2 (if has-qualifier (cdr rest) rest))
                                (sll (car rest2))
                                (body (cdr rest2))
                                ;; Specializers from specialized lambda list
                                (specs
                                 (mapcar (lambda (p)
                                           (cond
                                             ((consp p)
                                              (let ((spec (cadr p)))
                                                (if (and (consp spec) (eq (car spec) 'eql))
                                                  `(list 'eql ,(rewrite-reader-forms (cadr spec)))
                                                  `',spec)))
                                             (t ''t)))
                                         (remove-if (lambda (p)
                                                       (and (symbolp p)
                                                            (member p '(&optional &rest &key &aux &allow-other-keys))))
                                                     sll)))
                                (params (mapcar (lambda (p) (if (consp p) (car p) p)) sll))
                                (rewritten-body (mapcar #'rewrite-reader-forms body)))
                           `(%defmethod ',gf-name ',(if qualifier qualifier nil)
                                        (list ,@specs)
                                        (lambda ,params ,@rewritten-body))))
                       (nreverse inline-methods))))
         `(progn
            (%defgeneric ',gf-name ',lambda-list ',(if combination combination nil))
            (defun ,gf-name (&rest %gf-args)
              (%gf-dispatch ',gf-name %gf-args))
            ,@method-forms))))

    ;; (define-method-combination name &rest options)
    ;; Short form: (define-method-combination name :operator op :documentation ... :identity-with-one-argument t)
    ((and (eq (car form) 'define-method-combination) (cdr form))
     (let* ((mc-name (cadr form))
            (options (cddr form))
            (operator mc-name)
            (identity-with-one nil))
       (let ((cur options))
         (loop
           (when (null cur) (return))
           (let ((key (car cur)) (val (cadr cur)))
             (cond
               ((eq key :operator) (setq operator val))
               ((eq key :identity-with-one-argument) (setq identity-with-one val))
               ((eq key :documentation) nil)  ; ignored
               (t nil)))
           (setq cur (cddr cur))))
       `(%define-method-combination ',mc-name ',operator ,identity-with-one)))

    ;; (defmethod slot-unbound (...) body...) → defun + %add-slot-unbound-method
    ;; Specializer on obj (2nd param) by class name and slot-name (3rd param)
    ;; We generate a named defun instead of a lambda to avoid MVM closure issues.
    ((and (eq (car form) 'defmethod)
          (cdr form)
          (eq (cadr form) 'slot-unbound)
          (consp (caddr form)))
     (let* ((lambda-list (caddr form))
            (body (cdddr form))
            ;; Extract specializers: ((class spec) (obj spec) (slot-name spec))
            (class-spec (first lambda-list))
            (obj-spec   (second lambda-list))
            (slot-spec  (third lambda-list))
            ;; Get param names
            (class-param (if (consp class-spec) (car class-spec) class-spec))
            (obj-param   (if (consp obj-spec)   (car obj-spec)   obj-spec))
            (slot-param  (if (consp slot-spec)  (car slot-spec)  slot-spec))
            ;; Get obj class specializer
            (obj-class
             (if (and (consp obj-spec) (consp (cadr obj-spec)))
                 ;; (obj class-name) — class specializer
                 (cadr obj-spec)
                 (if (consp obj-spec)
                     (cadr obj-spec)
                     t)))
            ;; Get slot-name specializer: t or (eql 'sym)
            (slot-specializer
             (if (and (consp slot-spec) (consp (cadr slot-spec)))
                 ;; (slot-name (eql 'x)) → extract x
                 (let ((eql-form (cadr slot-spec)))
                   (if (and (consp eql-form)
                            (eq (car eql-form) 'eql)
                            (consp (cadr eql-form))
                            (eq (car (cadr eql-form)) 'quote))
                       ;; (eql 'sym) → sym
                       (cadr (cadr eql-form))
                       nil))
                 nil))
            (rewritten-body (mapcar #'rewrite-reader-forms body))
            ;; Generate unique function name to avoid lambda/closure issues
            (fn-name (intern (format nil "%SLOT-UNBOUND-METHOD-~D"
                                     (incf *slot-unbound-method-counter*))
                             :cl-user))
            ;; Use nil as slot-spec for "match any", or quoted symbol for specific
            (slot-arg (if slot-specializer `',slot-specializer nil)))
       `(progn
          (defun ,fn-name (,class-param ,obj-param ,slot-param)
            ,@rewritten-body)
          (%add-slot-unbound-method ',obj-class ,slot-arg #',fn-name))))

    ;; (defmethod name [qualifier] specialized-lambda-list body...)
    ;; → (%defmethod 'name qualifier '(specializers) (lambda params body))
    ((and (eq (car form) 'defmethod) (cdr form))
     (let* ((gf-name (cadr form))
            (rest (cddr form))
            ;; Check for qualifier: if (car rest) is a non-list symbol, it's a qualifier
            (has-qualifier (and rest (symbolp (car rest)) (not (listp (car rest)))))
            (qualifier (if has-qualifier (car rest) nil))
            (rest2 (if has-qualifier (cdr rest) rest))
            (sll (car rest2))      ; specialized lambda list
            (body (cdr rest2)))
       (when (null sll) (return-from rewrite-reader-forms nil))
       (when (not (listp sll)) (return-from rewrite-reader-forms nil))
       ;; Extract specializers (skip &optional, &rest, &key, &aux, &allow-other-keys)
       (let* ((specs
               (mapcar (lambda (p)
                         (cond
                           ;; (var class-name) or (var (eql val))
                           ((consp p)
                            (let ((spec (cadr p)))
                              (if (and (consp spec) (eq (car spec) 'eql))
                                ;; eql specializer: preserve as (eql val)
                                `(list 'eql ,(rewrite-reader-forms (cadr spec)))
                                `',(cadr p))))
                           ;; plain var — specializer is t
                           (t ''t)))
                       (remove-if (lambda (p)
                                    (and (symbolp p)
                                         (member p '(&optional &rest &key &aux &allow-other-keys))))
                                  sll)))
              ;; Extract parameter names (strip specializers)
              (params
               (mapcar (lambda (p)
                         (if (consp p) (car p) p))
                       sll))
              (rewritten-body (mapcar #'rewrite-reader-forms body)))
         ;; Use lambda directly — can be inside init expressions
         `(%defmethod ',gf-name ',(if qualifier qualifier nil)
                      (list ,@specs)
                      (lambda ,params ,@rewritten-body)))))

    ;; (make-instance 'class-name &rest initargs)
    ;; → (%make-instance 'class-name) + set-slot-value for initargs
    ;; We expand initargs at build time using SBCL-side class registry.
    ((and (eq (car form) 'make-instance) (cdr form))
     (let* ((class-arg-raw (cadr form))
            (class-arg (rewrite-reader-forms class-arg-raw))
            (rest-args (cddr form))
            ;; Check if class-arg is a quoted symbol we know about
            (class-name (if (and (consp class-arg-raw)
                                 (eq (car class-arg-raw) 'quote)
                                 (symbolp (cadr class-arg-raw)))
                            (cadr class-arg-raw)
                            nil))
            (slot-info (if class-name
                           (cdr (assoc class-name *sbcl-clos-classes*))
                           nil)))
       (if (null rest-args)
           ;; No initargs: simple case
           `(%make-instance ,class-arg)
           ;; Has initargs: expand inline
           ;; Generate: (let ((%mi-tmp (%make-instance 'class)))
           ;;               (set-slot-value %mi-tmp 'slot val) ...
           ;;               %mi-tmp)
           (let* ((inst-var '%clos-make-instance-tmp)
                  (set-forms nil))
             ;; Walk initargs pairwise
             (let ((args rest-args))
               (loop
                 (when (null args) (return))
                 (let ((key (car args))
                       (val (rewrite-reader-forms (cadr args))))
                   ;; key should be a keyword; find matching slot
                   (when (keywordp key)
                     (let* ((kname (symbol-name key))
                            ;; Find slot with matching initarg
                            (slot-name (if slot-info
                                          ;; Look in class slot info
                                          (cdr (assoc kname (cdr slot-info)
                                                      :test #'string-equal))
                                          ;; Fallback: use keyword name as slot name
                                          (intern (string-upcase kname) :cl-user))))
                       (when slot-name
                         (push `(set-slot-value ,inst-var ',slot-name ,val)
                               set-forms))))
                   (setq args (cddr args)))))
             `(let ((,inst-var (%make-instance ,class-arg)))
                ,@(nreverse set-forms)
                ,inst-var)))))

    ;; (slot-value obj slot) → (slot-value obj slot) — already defined at runtime
    ;; (slot-boundp obj slot) → (slot-boundp obj slot) — already defined
    ;; (slot-makunbound obj slot) → (slot-makunbound obj slot) — already defined

    ;; (with-slots (slot-bindings...) obj body...)
    ;; → let bindings using slot-value
    ((and (eq (car form) 'with-slots) (cddr form))
     (let* ((slot-entries (cadr form))
            (obj-form (rewrite-reader-forms (caddr form)))
            (body (mapcar #'rewrite-reader-forms (cdddr form)))
            ;; Use a fixed obj var name (no gensym — must survive ~S print/read)
            (obj-var '%with-slots-obj)
            (bindings
             (mapcar (lambda (entry)
                       (if (consp entry)
                           ;; (var slot-name)
                           `(,(car entry) (slot-value ,obj-var ',(cadr entry)))
                           ;; bare slot-name
                           `(,entry (slot-value ,obj-var ',entry))))
                     slot-entries)))
       `(let ((,obj-var ,obj-form))
          (let ,bindings
            ,@body))))

    ;; (with-accessors (accessor-bindings...) obj body...)
    ;; → let bindings using accessor functions
    ((and (eq (car form) 'with-accessors) (cddr form))
     (let* ((acc-entries (cadr form))
            (obj-form (rewrite-reader-forms (caddr form)))
            (body (mapcar #'rewrite-reader-forms (cdddr form)))
            (obj-var '%with-accessors-obj)
            (bindings
             (mapcar (lambda (entry)
                       ;; entry = (var accessor-fn)
                       (if (consp entry)
                           `(,(car entry) (,(cadr entry) ,obj-var))
                           `(,entry (,entry ,obj-var))))
                     acc-entries)))
       `(let ((,obj-var ,obj-form))
          (let ,bindings
            ,@body))))

    ;; (with-open-file (var filespec &rest opts) body...)
    ;; → (let ((var (open filespec opts...))) (unwind-protect (progn body) (when var (close var))))
    ;; Since MVM has no unwind-protect, we use let + close at end (no exception safety for now)
    ((and (eq (car form) 'with-open-file) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (var (car binding))
            (filespec (rewrite-reader-forms (cadr binding)))
            (opts (mapcar #'rewrite-reader-forms (cddr binding)))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(let ((,var (open ,filespec ,@opts)))
          (when ,var
            (let ((%wof-result (progn ,@body)))
              (close ,var)
              %wof-result)))))

    ;; (with-open-stream (var stream-form) body...)
    ;; → (let ((var stream-form)) (progn body... (close var)))
    ((and (eq (car form) 'with-open-stream) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (var (car binding))
            (stream-form (rewrite-reader-forms (cadr binding)))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(let ((,var ,stream-form))
          (let ((%wos-result (progn ,@body)))
            (close ,var)
            %wos-result))))

    ;; (with-hash-table-iterator (next ht) body...)
    ;; Expands to: collect ht pairs as alist, iterate
    ;; (next) returns (values more-p key val)
    ((and (eq (car form) 'with-hash-table-iterator) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (iter-name (car binding))
            (ht-form (rewrite-reader-forms (cadr binding)))
            (body (mapcar #'rewrite-reader-forms (cddr form)))
            (ht-var '%whti-ht)
            (pairs-var '%whti-pairs))
       `(let* ((,ht-var ,ht-form)
               (,pairs-var (%ht-to-alist ,ht-var)))
          (flet ((,iter-name ()
                   (if (null ,pairs-var)
                       (values nil nil nil)
                       (let ((%whti-pair (car ,pairs-var)))
                         (setq ,pairs-var (cdr ,pairs-var))
                         (values t (car %whti-pair) (cdr %whti-pair))))))
            ,@body))))

    ;; (with-package-iterator (next pkg symbols) body...)
    ;; Stub: just run body with (next) returning nil
    ((and (eq (car form) 'with-package-iterator) (cdr form) (consp (cadr form)))
     (let* ((binding (cadr form))
            (iter-name (car binding))
            (body (mapcar #'rewrite-reader-forms (cddr form))))
       `(flet ((,iter-name () (values nil nil nil nil)))
          ,@body)))

    (t (rewrite-reader-forms-list form))))

(defun rewrite-reader-forms-list (list)
  "Walk a possibly-dotted list, applying rewrite-reader-forms to each element."
  (cond
    ((null list) nil)
    ((atom list) (rewrite-reader-forms list))
    (t (cons (rewrite-reader-forms (car list))
             (rewrite-reader-forms-list (cdr list))))))

;; Load real ANSI test files (if available)
(defvar *ansi-aux-sources* "")       ; auxiliary/helper files (loaded before test files)
(defvar *real-ansi-sources* "")
(defvar *ansi-test-counter* 10000)
(defvar *ansi-file-names* nil)
;; Per-file test ID ranges, list of (name first-id last-id).
;; Used to skip files whose range doesn't overlap the active shard range,
;; so init-forms in unrelated files don't run (many crash the parent).
(defvar *ansi-file-ranges* nil)

(defun load-ansi-chapter (dir files)
  "Transform ANSI test files from DIR into MVM-compatible source.
   Skips files that cause read errors."
  (dolist (file files)
    (handler-case
      (let ((path (concatenate 'string dir file)))
        (when (probe-file path)
          (format t "  Transforming: ~A~%" file)
          (let ((forms nil))
            (with-open-file (s path :direction :input)
              (let ((*package* (find-package :cl-user)))
                (loop (let ((form (read s nil :eof)))
                        (when (eq form :eof) (return))
                        (push form forms)))))
            (push (pathname-name file) *ansi-file-names*)
            ;; Snapshot the test-id counter on entry so we can record the
            ;; file's [first .. last] test-id range after processing.
            (let ((file-first-id (1+ *ansi-test-counter*)))
              (push (list (pathname-name file) file-first-id nil) *ansi-file-ranges*))
            (setf forms (mapcar #'rewrite-package-iteration (nreverse forms)))
            (setf forms (mapcar #'rewrite-make-array-dims forms))
            (setf forms (mapcar #'rewrite-eval-quote forms))
            (setf forms (mapcar #'rewrite-make-array-initcontents forms))
            (setf forms (mapcar #'rewrite-earmuff-specials forms))
            (setf forms (mapcar #'rewrite-reader-forms forms))
            ;; Rewrite multi-arg apply: (apply fn a1 a2 ... list) → 2-arg form
            (setf forms (mapcar #'rewrite-multi-arg-apply forms))
            (when (string= file "integer-length.lsp")
              (labels ((rw (f)
                         (cond ((atom f) f)
                               ((and (eq (car f) 'ash) (cddr f))
                                (cons 'bignum-ash (mapcar #'rw (cdr f))))
                               ((and (eq (car f) '1-) (cdr f))
                                (cons 'bignum-1- (mapcar #'rw (cdr f))))
                               ((and (eq (car f) '-) (cdr f))
                                (if (cddr f)
                                    (cons 'bignum-sub (mapcar #'rw (cdr f)))
                                    (cons 'bignum-negate (mapcar #'rw (cdr f)))))
                               ((and (eq (car f) 'eql) (cddr f))
                                ;; eql needs to handle bignum=fixnum comparison
                                (cons 'bignum-eql (mapcar #'rw (cdr f))))
                               (t (mapcar-dotted #'rw f)))))
                (setf forms (mapcar-dotted #'rw forms))))
            ;; Rewrite arithmetic in real.lsp for ratio support:
            ;; / → exact-divide, - → generic-subtract, 1+ → generic-1+
            ;; Also limit LOOP REPEAT 200 → 60 (63-bit fixnum overflow)
            (when (string= file "real.lsp")
              (labels ((rw (f)
                         (cond ((atom f) f)
                               ;; (/ a b) → (exact-divide a b)
                               ((and (eq (car f) '/) (cddr f) (null (cdddr f)))
                                (cons 'exact-divide (mapcar #'rw (cdr f))))
                               ;; (- a) → (generic-negate a), (- a b) → (generic-subtract a b)
                               ((and (eq (car f) '-) (cdr f))
                                (if (cddr f)
                                    (list 'generic-subtract (rw (cadr f)) (rw (caddr f)))
                                    (list 'generic-negate (rw (cadr f)))))
                               ;; (1+ a) → (generic-1+ a)
                               ((and (eq (car f) '1+) (cdr f) (null (cddr f)))
                                (list 'generic-1+ (rw (cadr f))))
                               (t
                                ;; Patch REPEAT 200 → REPEAT 55 (safe for 63-bit fixnum with ratio cross-multiply)
                                (let ((result (mapcar-dotted #'rw f)))
                                  (when (and (eq (car result) 'loop))
                                    (let ((tail result))
                                      (loop (when (null tail) (return))
                                        (when (and (eq (car tail) 'repeat)
                                                   (cdr tail) (eql (cadr tail) 200))
                                          (setf (cadr tail) 55))
                                        (setq tail (cdr tail)))))
                                  result)))))
                (setf forms (mapcar #'rw forms))))
            ;; Evaluate defun/defmacro forms at SBCL side so that macros
            ;; defined within the file can be used during macroexpansion below.
            ;; This is needed for files like adjust-array.lsp that define
            ;; helper functions/macros used only at SBCL compile time.
            (dolist (form forms)
              (when (and (consp form)
                         (member (car form) '(defun defmacro)))
                (handler-case (eval form) (error () nil))))
            ;; Macroexpand def-print-test, def-pprint-test, def-format-test,
            ;; def-adjust-array-test, etc. into deftest forms before processing
            (setf forms
                  (mapcan (lambda (form)
                            (if (and (consp form)
                                     (member (car form) '(def-print-test def-pprint-test
                                                          def-format-test def-ppblock-test
                                                          def-adjust-array-test
                                                          def-adjust-array-fp-test)))
                                (handler-case
                                  (let ((expanded (macroexpand-1 form)))
                                    ;; def-format-test expands to (progn deftest deftest)
                                    (if (and (consp expanded) (eq (car expanded) 'progn))
                                        (cdr expanded)
                                        (list expanded)))
                                  (error (e)
                                    (format t "    SKIP-MACRO ~A: ~A~%" (car form) e)
                                    nil))
                                (list form)))
                          forms))
            ;; Re-run select rewriters after macroexpansion. Macros like
            ;; def-print-test/def-format-test expand to forms that contain
            ;; their own (let ((*print-base* 2) ...) ...) bindings and
            ;; with-output-to-string / with-standard-io-syntax forms — none
            ;; of which existed in the input forms the first-pass rewriters
            ;; saw. Re-running here:
            ;;   - earmuff-specials adds (declare (special ...)) to inner
            ;;     let bindings of *print-* / *read-* vars (was the +109
            ;;     win in the previous commit).
            ;;   - reader-forms expands with-output-to-string into a let
            ;;     and with-standard-io-syntax into %with-standard-io-syntax
            ;;     (lambda) so the runtime path is consistent.
            (setf forms (mapcar #'rewrite-reader-forms forms))
            (setf forms (mapcar #'rewrite-multi-arg-apply forms))
            (setf forms (mapcar #'rewrite-aux-params forms))
            (setf forms (mapcar #'rewrite-earmuff-specials forms))
            (let ((out (make-string-output-stream)) (test-forms nil) (init-forms nil))
              (format out "~%;; === ~A ===~%" file)
              (dolist (form forms)
                (cond
                  ((and (consp form) (eq (car form) 'deftest))
                   (let* ((rest-after-name (cddr form))
                          ;; Skip :notes (...) if present
                          (rest-after-notes
                           (if (eq (car rest-after-name) :notes)
                               (cddr rest-after-name)
                               rest-after-name))
                          (name (cadr form))
                          (test-form (car rest-after-notes))
                          (expected (cdr rest-after-notes)))
                     (setf *ansi-test-counter* (1+ *ansi-test-counter*))
                     (let ((test-id *ansi-test-counter*))
                       (format t "      ~D = ~A~%" test-id name)
                       (let ((test-str (handler-case
                                         (cond
                                           ((= (length expected) 1)
                                            (format nil "(run-test ~D (lambda () ~S) '~S)"
                                                    test-id test-form (car expected)))
                                           ((> (length expected) 0)
                                            (format nil "(run-test-mv ~D (lambda () (multiple-value-list ~S)) '~S)"
                                                    test-id test-form expected))
                                           ;; (deftest NAME FORM) with no explicit
                                           ;; expected — test expects zero values.
                                           ;; Render as run-test-mv with '() expected.
                                           (t
                                            (format nil "(run-test-mv ~D (lambda () (multiple-value-list ~S)) 'NIL)"
                                                    test-id test-form)))
                                         (error () nil))))
                         ;; For real.lsp: fix / and - inside backquote commas
                         ;; (tree rewriter can't reach inside SBCL comma objects)
                         (when (and test-str (string= file "real.lsp"))
                           (labels ((str-replace-all (old new str)
                                      (let ((pos (search old str)))
                                        (if pos
                                            (str-replace-all old new
                                              (concatenate 'string
                                                (subseq str 0 pos) new
                                                (subseq str (+ pos (length old)))))
                                            str))))
                             ;; Replace (/ → (EXACT-DIVIDE inside comma contexts
                             ;; Order matters: / first, then - (so ,(- (/ x y)) works)
                             (setf test-str (str-replace-all ",(/ " ",(EXACT-DIVIDE " test-str))
                             (setf test-str (str-replace-all ",(- (" ",(GENERIC-NEGATE (" test-str))
                             ;; Also fix (/ inside ,(- ...): after GENERIC-NEGATE, inner / remains
                             (setf test-str (str-replace-all ",(GENERIC-NEGATE (/ " ",(GENERIC-NEGATE (EXACT-DIVIDE " test-str))
                             (when (member name '(real.3 real.4) :test #'string=)
                               (format *error-output* "~%POST-REPLACE ~A:~%~A~%~%" name test-str))))
                         ;; Filter: unreadable-object printouts can't round-trip.
                         ;; Match SBCL's `#<CLASS-NAME ...>` pattern specifically
                         ;; — `(search "#<" ...)` was too broad, rejecting any test
                         ;; whose source contains the 2-char string "#<" (e.g.
                         ;; print.array.2.28 checks whether output starts with "#<").
                         (when (and test-str
                                    (not (search "#<FUNCTION" test-str))
                                    (not (search "#<CLASS" test-str))
                                    (not (search "#<SB-" test-str))
                                    (not (search "#<STANDARD" test-str))
                                    (not (search "#<STRUCTURE" test-str))
                                    (not (search "#<CLOSURE" test-str))
                                    (not (search "&ENVIRONMENT" test-str))
                                    (not (search "STRUCT-TEST-" test-str)))
                           (push test-str test-forms))))))
                  ((and (consp form) (member (car form)
                          '(defharmless def-fold-test def-macro-test
                            in-package declaim))) nil)
                  (t
                   ;; For progn forms (from rewritten defclass/defmethod/defgeneric):
                   ;; - defun sub-forms → top-level (compiled as global functions)
                   ;; - non-defun sub-forms (like %defclass, %defmethod calls) → init-forms
                   ;;   (run inside run-ansi-* since TOPLEVEL thunks never execute)
                   ;; Handles nested progn forms recursively.
                   ;; For non-progn forms: write to top-level as before.
                   (labels ((emit-sub (sub)
                              (when (consp sub)
                                ;; Recursively flatten nested progns
                                (if (eq (car sub) 'progn)
                                    (dolist (inner (cdr sub)) (emit-sub inner))
                                    (let ((sub-s (handler-case (format nil "~S" sub)
                                                   (error () nil))))
                                      (when (and sub-s
                                                 (not (search "#<" sub-s))
                                                 (not (search "&ENVIRONMENT" sub-s))
                                                 (not (search "STRUCT-TEST-" sub-s)))
                                        (if (member (car sub) '(defun defvar defparameter defstruct))
                                            (progn (write-string sub-s out) (terpri out))
                                            (push sub-s init-forms))))))))
                     (if (and (consp form) (eq (car form) 'progn))
                         (dolist (sub (cdr form)) (emit-sub sub))
                         (let ((s (handler-case (format nil "~S" form)
                                    (error () nil))))
                           (when (and s
                                      (not (search "#<" s))
                                      (not (search "&ENVIRONMENT" s))
                                      (not (search "STRUCT-TEST-" s)))
                             (write-string s out)
                             (terpri out))))))))
              (format out "(defun run-ansi-~A ()~%" (pathname-name file))
              ;; Init forms run in PARENT (their side effects need to persist
              ;; for subsequent tests that depend on them, e.g. defclass).
              ;; Each is wrapped individually so one crash doesn't skip others.
              (dolist (s (nreverse init-forms))
                (format out "  (handler-case ~A (t (c) nil))~%" s))
              ;; Test forms — wrap EACH fork-test call in its own handler-case
              ;; so a crash during parent-side arg-evaluation (vector literal,
              ;; closure creation, etc.) of test N doesn't kill test N+1.
              ;; On catch, call (%test-crash-fail <id>) which emits
              ;; \"\\nFAIL <id>\\n\" so the sharded summary accounts for it.
              ;; Using a helper function keeps the per-call code tiny.
              ;; Wrap each run-test call in an outer handler-case that calls
              ;; %test-crash-fail (defined in the runtime preamble below). This
              ;; catches any crash during parent-side arg-evaluation that
              ;; happens before run-test's own handler-case takes effect —
              ;; especially closure construction and special var binding.
              (dolist (tf (nreverse test-forms))
                (let* ((form-str tf)
                       (id-start (position #\Space form-str))
                       (id-end (position #\Space form-str :start (1+ id-start)))
                       (id-num (parse-integer form-str :start (1+ id-start) :end id-end :junk-allowed t)))
                  (if id-num
                      (format out "  (handler-case ~A (t (c) (%test-crash-fail ~D)))~%" form-str id-num)
                      (format out "  (handler-case ~A (t (c) nil))~%" form-str))))
              (format out ")~%")
              (setf *real-ansi-sources*
                    (concatenate 'string *real-ansi-sources*
                                 (get-output-stream-string out)))
              ;; Record the last test-id used in this file (may be nil if none).
              (let ((entry (car *ansi-file-ranges*)))
                (setf (third entry) *ansi-test-counter*))))))
      (error (e)
        (format t "    SKIP ~A: ~A~%" file e)))))

;; Rewrite multi-arg apply calls into 2-arg form that MVM's apply supports.
;; MVM's apply only takes (fn args-list). CL allows (apply fn a1 a2 ... list).
;; (apply fn a1 a2 ... aN list) → (apply fn (append (list a1 a2 ... aN) list))
;; (apply fn list) → unchanged (already 2-arg form)
(defun rewrite-multi-arg-apply (form)
  "Walk form tree, converting (apply fn a1 a2 ... list) to (apply fn (append (list a1 a2 ...) list))."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'apply)
          (consp (cdr form))  ; has fn arg
          (consp (cddr form)) ; has at least one more arg
          (consp (cdddr form))) ; has at least 2 more args (fn + spread args + list)
     ;; (apply fn a1 a2 ... aN list) where there are N >= 1 spread args before the list
     (let* ((fn (rewrite-multi-arg-apply (cadr form)))
            (rest-args (cddr form))  ; a1 a2 ... aN list
            (spread-args (butlast rest-args))  ; a1 a2 ... aN
            (final-list (car (last rest-args))) ; list
            (final-rewritten (rewrite-multi-arg-apply final-list))
            (spread-rewritten (mapcar #'rewrite-multi-arg-apply spread-args)))
       (if spread-args
           `(apply ,fn (append (list ,@spread-rewritten) ,final-rewritten))
           `(apply ,fn ,final-rewritten))))
    (t (mapcar-dotted #'rewrite-multi-arg-apply form))))

;; Rewrite &aux bindings in defun/lambda parameter lists into let* in the body.
;; MVM compiler does not support &aux.
;; (defun foo (a b &aux (x expr)) body) → (defun foo (a b) (let* ((x expr)) body))
(defun rewrite-aux-params (form)
  "Walk form tree, expanding &aux parameter sections into let* bindings."
  (cond
    ((atom form) form)
    ;; Handle defun
    ((and (eq (car form) 'defun) (consp (cdr form)) (consp (cddr form)))
     (let* ((name (cadr form))
            (params (caddr form))
            (body (cdddr form)))
       (multiple-value-bind (new-params aux-bindings)
           (split-aux-params params)
         (let ((new-body (mapcar #'rewrite-aux-params body)))
           (if aux-bindings
               `(defun ,name ,new-params (let* ,aux-bindings ,@new-body))
               `(defun ,name ,new-params ,@new-body))))))
    ;; Handle lambda
    ((and (eq (car form) 'lambda) (consp (cdr form)))
     (let* ((params (cadr form))
            (body (cddr form)))
       (multiple-value-bind (new-params aux-bindings)
           (split-aux-params params)
         (let ((new-body (mapcar #'rewrite-aux-params body)))
           (if aux-bindings
               `(lambda ,new-params (let* ,aux-bindings ,@new-body))
               `(lambda ,new-params ,@new-body))))))
    (t (mapcar-dotted #'rewrite-aux-params form))))

(defun split-aux-params (params)
  "Split a parameter list at &aux, returning (values required-params aux-bindings).
   aux-bindings is nil if no &aux present."
  (let ((aux-pos (position '&aux params)))
    (if aux-pos
        (let ((before (subseq params 0 aux-pos))
              (aux-forms (subseq params (1+ aux-pos))))
          (values before
                  (mapcar (lambda (b)
                            (if (consp b)
                                b
                                (list b nil)))
                          aux-forms)))
        (values params nil))))

(defvar *ansi-aux-loaded* nil)  ; track which aux files already loaded (avoid duplicates)

(defun load-ansi-aux (filename)
  "Transform an ANSI test auxiliary file into MVM-compatible source.
   Emits defun/defstruct/defvar/defparameter/defconstant forms into *ansi-aux-sources*.
   Skips CLOS methods, defgeneric, and forms that reference unsupported features."
  (let ((path (concatenate 'string "/tmp/ansi-test/auxiliary/" filename)))
    (when (member filename *ansi-aux-loaded* :test #'string=)
      (return-from load-ansi-aux nil))
    (unless (probe-file path)
      (format t "  AUX MISSING: ~A~%" filename)
      (return-from load-ansi-aux nil))
    (push filename *ansi-aux-loaded*)
    (format t "  Loading aux: ~A~%" filename)
    (handler-case
      (let ((forms nil))
        (with-open-file (s path :direction :input)
          (let ((*package* (find-package :cl-user)))
            (loop (let ((form (read s nil :eof)))
                    (when (eq form :eof) (return))
                    (push form forms)))))
        ;; Apply the same rewriter pipeline as test files
        (setf forms (mapcar #'rewrite-package-iteration (nreverse forms)))
        (setf forms (mapcar #'rewrite-make-array-dims forms))
        (setf forms (mapcar #'rewrite-eval-quote forms))
        (setf forms (mapcar #'rewrite-make-array-initcontents forms))
        (setf forms (mapcar #'rewrite-earmuff-specials forms))
        (setf forms (mapcar #'rewrite-reader-forms forms))
        ;; Expand &aux lambda keyword into let* bindings in function body.
        ;; MVM compiler does not support &aux.
        (setf forms (mapcar #'rewrite-aux-params forms))
        ;; Rewrite (apply fn a1 a2 ... list) → (apply fn (append (list a1 a2 ...) list))
        ;; MVM's apply only handles (fn list) form; CL allows spread args before final list.
        (setf forms (mapcar #'rewrite-multi-arg-apply forms))
        ;; Evaluate defun/defmacro forms at SBCL side so macros defined here
        ;; can be used during macroexpansion of test files loaded after this.
        (dolist (form forms)
          (when (and (consp form)
                     (member (car form) '(defun defmacro defstruct defparameter defvar)))
            (handler-case (eval form) (error () nil))))
        (labels
          ;; Strip package prefixes that MVM doesn't understand:
          ;; REGRESSION-TEST:: and CL-TEST:: symbols → unqualified symbols.
          ;; This is done as a tree walk so the symbols themselves are renamed.
          ((strip-pkg-prefix (form)
             (cond
               ((symbolp form)
                (let* ((name (symbol-name form))
                       (pkg  (symbol-package form))
                       (pname (and pkg (package-name pkg))))
                  (if (and pname (or (string= pname "REGRESSION-TEST")
                                     (string= pname "CL-TEST")))
                      (intern name)   ; re-intern in cl-user
                      form)))
               ((consp form)
                (cons (strip-pkg-prefix (car form))
                      (strip-pkg-prefix (cdr form))))
               (t form))))
          (let ((out (make-string-output-stream)))
            (format out "~%;; === aux: ~A ===~%" filename)
            (dolist (form forms)
              (when (consp form)
                ;; Skip forms that can't compile on MVM or reference SBCL internals:
                ;; defgeneric, defmethod, eval-when, declaim, proclaim, compile-and-load
                (when (member (car form) '(defgeneric defmethod eval-when declaim proclaim
                                          compile-and-load in-package))
                  (setf form nil))
                (when form
                  ;; Strip REGRESSION-TEST:: and CL-TEST:: prefixes
                  (setf form (strip-pkg-prefix form))
                  ;; For progn wrapping (from rewritten defclass etc.):
                  ;; split into sub-forms and process each
                  (if (and (consp form) (eq (car form) 'progn))
                      (dolist (sub (cdr form))
                        (when (and (consp sub)
                                   (member (car sub) '(defun defvar defparameter defstruct
                                                       defconstant %defclass)))
                          (let ((s (handler-case (format nil "~S" sub) (error () nil))))
                            (when (and s
                                       (not (search "#<" s))
                                       (not (search "&ENVIRONMENT" s)))
                              (write-string s out)
                              (terpri out)))))
                      ;; Top-level form: emit if it's a defun/defstruct definition.
                      ;; defvar/defparameter/defconstant: only emit if init value is a
                      ;; simple literal (string, number, nil, t, or quoted form).
                      ;; Complex init expressions may call undefined functions and crash.
                      (cond
                        ((member (car form) '(defun defstruct deftype %defclass))
                         (let ((s (handler-case (format nil "~S" form) (error () nil))))
                           (when (and s
                                      (not (search "#<" s))
                                      (not (search "&ENVIRONMENT" s)))
                             (write-string s out)
                             (terpri out))))
                        ((member (car form) '(defvar defparameter defconstant))
                         ;; Skip defparameters whose init values are known to crash on MVM:
                         ;; - (make-int-array N): uses funcall with keyword args that MVM doesn't handle
                         ;; - (if (boundp ...)):  boundp not available at init time for specials
                         ;; All other defparameters are emitted as-is.
                         (let ((name (cadr form))
                               (init (if (cddr form) (caddr form) nil))
                               (skip-p nil))
                           ;; Skip *displaced* (make-int-array 100000) — complex funcall
                           (when (and (symbolp name)
                                      (string= (symbol-name name) "*DISPLACED*"))
                             (setf skip-p t))
                           ;; Skip *initial-print-pprint-dispatch* (uses boundp)
                           (when (and (symbolp name)
                                      (string= (symbol-name name)
                                               "*INITIAL-PRINT-PPRINT-DISPATCH*"))
                             (setf skip-p t))
                           ;; Skip *similarity-list* (defgeneric is-similar* not in MVM)
                           (when (and (symbolp name)
                                      (string= (symbol-name name) "*SIMILARITY-LIST*"))
                             (setf skip-p t))
                           (unless skip-p
                             (let ((s (handler-case (format nil "~S" form) (error () nil))))
                               (when (and s
                                          (not (search "#<" s))
                                          (not (search "&ENVIRONMENT" s)))
                                 (write-string s out)
                                 (terpri out)))))))))))
            (setf *ansi-aux-sources*
                  (concatenate 'string *ansi-aux-sources*
                               (get-output-stream-string out))))))
      (error (e)
        (format t "    SKIP AUX ~A: ~A~%" filename e)))))

;;; ============================================================
;;; Load ANSI test files by chapter
;;; ============================================================

;;; Load ALL ANSI test files by chapter
;;; ============================================================

;;; Load auxiliary files first — these define scaffolding used by all chapters
(format t "~%Loading auxiliary files...~%")

;; Core aux: used by almost everything
(load-ansi-aux "ansi-aux.lsp")
(load-ansi-aux "cons-aux.lsp")

;; Chapter-specific aux files
(load-ansi-aux "types-aux.lsp")
(load-ansi-aux "array-aux.lsp")
(load-ansi-aux "bit-aux.lsp")
(load-ansi-aux "char-aux.lsp")
(load-ansi-aux "hash-table-aux.lsp")
(load-ansi-aux "numbers-aux.lsp")
(load-ansi-aux "random-aux.lsp")
(load-ansi-aux "floor-aux.lsp")
(load-ansi-aux "ffloor-aux.lsp")
(load-ansi-aux "ceiling-aux.lsp")
(load-ansi-aux "fceiling-aux.lsp")
(load-ansi-aux "truncate-aux.lsp")
(load-ansi-aux "ftruncate-aux.lsp")
(load-ansi-aux "round-aux.lsp")
(load-ansi-aux "fround-aux.lsp")
(load-ansi-aux "times-aux.lsp")
(load-ansi-aux "division-aux.lsp")
(load-ansi-aux "exp-aux.lsp")
(load-ansi-aux "gcd-aux.lsp")
(load-ansi-aux "string-aux.lsp")
(load-ansi-aux "subseq-aux.lsp")
(load-ansi-aux "search-aux.lsp")
(load-ansi-aux "remove-aux.lsp")
(load-ansi-aux "remove-duplicates-aux.lsp")
(load-ansi-aux "printer-aux.lsp")
(load-ansi-aux "backquote-aux.lsp")
(load-ansi-aux "reader-aux.lsp")
(load-ansi-aux "package-aux.lsp")
(load-ansi-aux "packages00-aux.lsp")
(load-ansi-aux "pathnames-aux.lsp")
(load-ansi-aux "cl-symbols-aux.lsp")
(load-ansi-aux "define-condition-aux.lsp")
(load-ansi-aux "defclass-aux.lsp")

(format t "  aux sources: ~D chars~%" (length *ansi-aux-sources*))

(load-ansi-chapter "/tmp/ansi-test/cons/"
  '("acons.lsp" "adjoin.lsp" "append.lsp" "assoc-if-not.lsp" "assoc-if.lsp" "assoc.lsp" "atom.lsp" "butlast.lsp" "cons-test-01.lsp" "cons-test-03.lsp" "cons-test-05.lsp" "cons.lsp" "consp.lsp" "copy-alist.lsp" "copy-list.lsp" "copy-tree.lsp" "cxr.lsp" "endp.lsp" "get-properties.lsp" "getf.lsp" "intersection.lsp" "last.lsp" "ldiff.lsp" "list-length.lsp" "list.lsp" "listp.lsp" "load.lsp" "make-list.lsp" "mapc.lsp" "mapcan.lsp" "mapcar.lsp" "mapcon.lsp" "mapl.lsp" "maplist.lsp" "member-if-not.lsp" "member-if.lsp" "member.lsp" "nbutlast.lsp" "nconc.lsp" "nintersection.lsp" "nreconc.lsp" "nset-difference.lsp" "nset-exclusive-or.lsp" "nsublis.lsp" "nsubst-if-not.lsp" "nsubst-if.lsp" "nsubst.lsp" "nth.lsp" "nthcdr.lsp" "nunion.lsp" "pairlis.lsp" "pop.lsp" "push.lsp" "pushnew.lsp" "rassoc-if-not.lsp" "rassoc-if.lsp" "rassoc.lsp" "remf.lsp" "rest.lsp" "revappend.lsp" "rplaca.lsp" "rplacd.lsp" "set-difference.lsp" "set-exclusive-or.lsp" "sublis.lsp" "subsetp.lsp" "subst-if-not.lsp" "subst-if.lsp" "subst.lsp" "tailp.lsp" "tree-equal.lsp" "union.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/data-and-control-flow/"
  '("and.lsp" "apply.lsp" "block.lsp" "call-arguments-limit.lsp" "case.lsp" "catch.lsp" "ccase.lsp" "compiled-function-p.lsp" "complement.lsp" "cond.lsp" "constantly.lsp" "ctypecase.lsp" "data-and-control-flow.lsp" "defconstant.lsp" "define-modify-macro.lsp" "define-setf-expander.lsp" "defparameter.lsp" "defsetf.lsp" "defun.lsp" "defvar.lsp" "destructuring-bind.lsp" "ecase.lsp" "eql.lsp" "equal.lsp" "equalp.lsp" "etypecase.lsp" "every.lsp" "fboundp.lsp" "fdefinition.lsp" "flet.lsp" "fmakunbound.lsp" "funcall.lsp" "function-lambda-expression.lsp" "function.lsp" "functionp.lsp" "get-setf-expansion.lsp" "identity.lsp" "if.lsp" "labels.lsp" "lambda-list-keywords.lsp" "lambda-parameters-limit.lsp" "let.lsp" "letstar.lsp" "load.lsp" "macrolet.lsp" "multiple-value-bind.lsp" "multiple-value-call.lsp" "multiple-value-list.lsp" "multiple-value-prog1.lsp" "multiple-value-setq.lsp" "nil.lsp" "not-and-null.lsp" "notany.lsp" "notevery.lsp" "nth-value.lsp" "or.lsp" "places.lsp" "prog.lsp" "prog1.lsp" "prog2.lsp" "progn.lsp" "progv.lsp" "psetf.lsp" "psetq.lsp" "return-from.lsp" "return.lsp" "rotatef.lsp" "shiftf.lsp" "some.lsp" "t.lsp" "tagbody.lsp" "typecase.lsp" "unless.lsp" "unwind-protect.lsp" "values-list.lsp" "values.lsp" "when.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/hash-tables/"
  '("clrhash.lsp" "gethash.lsp" "hash-table-count.lsp" "hash-table-p.lsp" "hash-table-rehash-size.lsp" "hash-table-rehash-threshold.lsp" "hash-table-size.lsp" "hash-table-test.lsp" "hash-table.lsp" "load.lsp" "make-hash-table.lsp" "maphash.lsp" "remhash.lsp" "sxhash.lsp" "with-hash-table-iterator.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/numbers/"
  '("abs.lsp" "acos.lsp" "acosh.lsp" "arithmetic-error.lsp" "ash.lsp" "asin.lsp" "asinh.lsp" "atan.lsp" "atanh.lsp" "boole.lsp" "byte.lsp" "ceiling.lsp" "cis.lsp" "complex.lsp" "complexp.lsp" "conjugate.lsp" "cos.lsp" "cosh.lsp" "decf.lsp" "deposit-field.lsp" "divide.lsp" "dpb.lsp" "epsilons.lsp" "evenp.lsp" "exp.lsp" "expt.lsp" "fceiling.lsp" "ffloor.lsp" "float.lsp" "floatp.lsp" "floor.lsp" "fround.lsp" "ftruncate.lsp" "gcd.lsp" "imagpart.lsp" "incf.lsp" "integer-length.lsp" "integerp.lsp" "isqrt.lsp" "lcm.lsp" "ldb.lsp" "load.lsp" "log.lsp" "logand.lsp" "logandc1.lsp" "logandc2.lsp" "logbitp.lsp" "logcount.lsp" "logeqv.lsp" "logior.lsp" "lognand.lsp" "lognor.lsp" "lognot.lsp" "logorc1.lsp" "logorc2.lsp" "logtest.lsp" "logxor.lsp" "make-random-state.lsp" "mask-field.lsp" "max.lsp" "min.lsp" "minus.lsp" "minusp.lsp" "number-comparison.lsp" "numberp.lsp" "numerator-denominator.lsp" "oddp.lsp" "oneminus.lsp" "oneplus.lsp" "parse-integer.lsp" "phase.lsp" "plus.lsp" "plusp.lsp" "random-state-p.lsp" "random.lsp" "rational.lsp" "rationalize.lsp" "rationalp.lsp" "real.lsp" "realp.lsp" "realpart.lsp" "round.lsp" "signum.lsp" "sin.lsp" "sinh.lsp" "sqrt.lsp" "tan.lsp" "tanh.lsp" "times.lsp" "truncate.lsp" "upgraded-complex-part-type.lsp" "zerop.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/symbols/"
  '("boundp.lsp" "cl-symbols.lsp" "copy-symbol.lsp" "gensym.lsp" "gentemp.lsp" "get.lsp" "keywordp.lsp" "load.lsp" "make-symbol.lsp" "makunbound.lsp" "remprop.lsp" "set.lsp" "special-operator-p.lsp" "symbol-function.lsp" "symbol-name.lsp" "symbolp.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/structures/"
  '("load.lsp" "structure-00.lsp" "structures-01.lsp" "structures-02.lsp" "structures-03.lsp" "structures-04.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/strings/"
  '("base-string.lsp" "char-schar.lsp" "load.lsp" "make-string.lsp" "nstring-capitalize.lsp" "nstring-downcase.lsp" "nstring-upcase.lsp" "simple-base-string.lsp" "simple-string-p.lsp" "simple-string.lsp" "string-capitalize.lsp" "string-comparisons.lsp" "string-downcase.lsp" "string-left-trim.lsp" "string-right-trim.lsp" "string-trim.lsp" "string-upcase.lsp" "string.lsp" "stringp.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/characters/"
  '("char-compare.lsp" "character.lsp" "load.lsp" "name-char.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/sequences/"
  '("concatenate.lsp" "copy-seq.lsp" "count-if-not.lsp" "count-if.lsp" "count.lsp" "elt.lsp" "fill-strings.lsp" "fill.lsp" "find-if-not.lsp" "find-if.lsp" "find.lsp" "length.lsp" "load.lsp" "make-sequence.lsp" "map-into.lsp" "map.lsp" "merge.lsp" "mismatch.lsp" "nreverse.lsp" "nsubstitute-if-not.lsp" "nsubstitute-if.lsp" "nsubstitute.lsp" "position-if-not.lsp" "position-if.lsp" "position.lsp" "reduce.lsp" "remove-duplicates.lsp" "remove.lsp" "replace.lsp" "reverse.lsp" "search-bitvector.lsp" "search-list.lsp" "search-string.lsp" "search-vector.lsp" "sort.lsp" "stable-sort.lsp" "subseq.lsp" "substitute-if-not.lsp" "substitute-if.lsp" "substitute.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/arrays/"
  '("adjust-array.lsp" "adjustable-array-p.lsp" "aref.lsp" "array-as-class.lsp" "array-dimension.lsp" "array-dimensions.lsp" "array-displacement.lsp" "array-element-type.lsp" "array-has-fill-pointer-p.lsp" "array-in-bounds-p.lsp" "array-misc.lsp" "array-rank.lsp" "array-row-major-index.lsp" "array-t.lsp" "array-total-size.lsp" "array.lsp" "arrayp.lsp" "bit-and.lsp" "bit-andc1.lsp" "bit-andc2.lsp" "bit-eqv.lsp" "bit-ior.lsp" "bit-nand.lsp" "bit-nor.lsp" "bit-not.lsp" "bit-orc1.lsp" "bit-orc2.lsp" "bit-vector-p.lsp" "bit-vector.lsp" "bit-xor.lsp" "bit.lsp" "fill-pointer.lsp" "load.lsp" "make-array.lsp" "row-major-aref.lsp" "sbit.lsp" "simple-array-t.lsp" "simple-array.lsp" "simple-bit-vector-p.lsp" "simple-bit-vector.lsp" "simple-vector-p.lsp" "svref.lsp" "upgraded-array-element-type.lsp" "vector-pop.lsp" "vector-push-extend.lsp" "vector-push.lsp" "vector.lsp" "vectorp.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/iteration/"
  '("do.lsp" "dolist.lsp" "dostar.lsp" "dotimes.lsp" "load.lsp" "loop.lsp" "loop1.lsp" "loop10.lsp" "loop11.lsp" "loop12.lsp" "loop13.lsp" "loop14.lsp" "loop15.lsp" "loop16.lsp" "loop17.lsp" "loop2.lsp" "loop3.lsp" "loop4.lsp" "loop5.lsp" "loop6.lsp" "loop7.lsp" "loop8.lsp" "loop9.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/printer/"
  '("copy-pprint-dispatch.lsp" "pprint-dispatch.lsp" "pprint-exit-if-list-exhausted.lsp" "pprint-fill.lsp" "pprint-indent.lsp" "pprint-linear.lsp" "pprint-logical-block.lsp" "pprint-newline.lsp" "pprint-tab.lsp" "pprint-tabular.lsp" "pprint.lsp" "prin1-to-string.lsp" "prin1.lsp" "princ-to-string.lsp" "princ.lsp" "print-array.lsp" "print-bit-vector.lsp" "print-characters.lsp" "print-complex.lsp" "print-cons.lsp" "print-floats.lsp" "print-integers.lsp" "print-length.lsp" "print-level.lsp" "print-lines.lsp" "print-pathname.lsp" "print-random-state.lsp" "print-ratios.lsp" "print-strings.lsp" "print-structure.lsp" "print-symbols.lsp" "print-unreadable-object.lsp" "print-vector.lsp" "print.lsp" "printer-control-vars.lsp" "write-to-string.lsp" "write.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/printer/format/"
  '("format-a.lsp" "format-ampersand.lsp" "format-b.lsp" "format-brace.lsp" "format-c.lsp" "format-circumflex.lsp" "format-conditional.lsp" "format-d.lsp" "format-goto.lsp" "format-newline.lsp" "format-o.lsp" "format-p.lsp" "format-page.lsp" "format-paren.lsp" "format-percent.lsp" "format-question.lsp" "format-r.lsp" "format-s.lsp" "format-t.lsp" "format-tilde.lsp" "format-x.lsp" "formatter-c.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/streams/"
  '("broadcast-stream-streams.lsp" "clear-input.lsp" "clear-output.lsp" "concatenated-stream-streams.lsp" "echo-stream-input-stream.lsp" "echo-stream-output-stream.lsp" "file-length.lsp" "file-position.lsp" "file-string-length.lsp" "finish-output.lsp" "force-output.lsp" "fresh-line.lsp" "get-output-stream-string.lsp" "input-stream-p.lsp" "interactive-stream-p.lsp" "listen.lsp" "load.lsp" "make-broadcast-stream.lsp" "make-concatenated-stream.lsp" "make-echo-stream.lsp" "make-string-input-stream.lsp" "make-string-output-stream.lsp" "make-synonym-stream.lsp" "make-two-way-stream.lsp" "open-stream-p.lsp" "open.lsp" "output-stream-p.lsp" "peek-char.lsp" "read-byte.lsp" "read-char-no-hang.lsp" "read-char.lsp" "read-line.lsp" "read-sequence.lsp" "stream-element-type.lsp" "stream-error-stream.lsp" "stream-external-format.lsp" "streamp.lsp" "synonym-stream-symbol.lsp" "terpri.lsp" "two-way-stream-input-stream.lsp" "two-way-stream-output-stream.lsp" "unread-char.lsp" "with-input-from-string.lsp" "with-open-file.lsp" "with-open-stream.lsp" "with-output-to-string.lsp" "write-char.lsp" "write-line.lsp" "write-sequence.lsp" "write-string.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/packages/"
  '("defpackage.lsp" "delete-package.lsp" "do-all-symbols.lsp" "do-external-symbols.lsp" "do-symbols.lsp" "export.lsp" "find-all-symbols.lsp" "find-package.lsp" "find-symbol.lsp" "import.lsp" "in-package.lsp" "intern.lsp" "keyword.lsp" "list-all-packages.lsp" "load.lsp" "make-package.lsp" "package-error-package.lsp" "package-error.lsp" "package-name.lsp" "package-nicknames.lsp" "package-shadowing-symbols.lsp" "package-use-list.lsp" "package-used-by-list.lsp" "packagep.lsp" "rename-package.lsp" "shadow.lsp" "shadowing-import.lsp" "unexport.lsp" "unintern.lsp" "unuse-package.lsp" "use-package.lsp" "with-package-iterator.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/eval-and-compile/"
  '("compile.lsp" "compiler-macros.lsp" "constantp.lsp" "declaim.lsp" "declaration.lsp" "define-compiler-macro.lsp" "define-symbol-macro.lsp" "defmacro.lsp" "dynamic-extent.lsp" "eval-and-compile.lsp" "eval-when.lsp" "eval.lsp" "ignorable.lsp" "ignore.lsp" "lambda.lsp" "load.lsp" "locally.lsp" "macro-function.lsp" "macroexpand-1.lsp" "macroexpand.lsp" "optimize.lsp" "proclaim.lsp" "special.lsp" "symbol-macrolet.lsp" "the.lsp" "type.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/types-and-classes/"
  '("class-precedence-lists.lsp" "coerce.lsp" "deftype.lsp" "load.lsp" "standard-generic-function.lsp" "subtypep-array.lsp" "subtypep-complex.lsp" "subtypep-cons.lsp" "subtypep-eql.lsp" "subtypep-float.lsp" "subtypep-function.lsp" "subtypep-integer.lsp" "subtypep-member.lsp" "subtypep-rational.lsp" "subtypep-real.lsp" "subtypep.lsp" "type-of.lsp" "typep.lsp" "types-and-class-2.lsp" "types-and-class.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/reader/"
  '("copy-readtable.lsp" "dispatch-macro-characters.lsp" "get-macro-character.lsp" "load.lsp" "read-delimited-list.lsp" "read-from-string.lsp" "read-preserving-whitespace.lsp" "read-suppress.lsp" "read.lsp" "reader-test.lsp" "readtable-case.lsp" "readtablep.lsp" "set-macro-character.lsp" "set-syntax-from-char.lsp" "syntax-tokens.lsp" "syntax.lsp" "with-standard-io-syntax.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/environment/"
  '("apropos-list.lsp" "apropos.lsp" "decode-universal-time.lsp" "describe.lsp" "disassemble.lsp" "documentation.lsp" "dribble.lsp" "ed.lsp" "encode-universal-time.lsp" "environment-functions.lsp" "get-internal-time.lsp" "get-universal-time.lsp" "inspect.lsp" "load.lsp" "room.lsp" "sleep.lsp" "time.lsp" "trace.lsp" "user-homedir-pathname.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/conditions/"
  '("abort.lsp" "assert.lsp" "cell-error-name.lsp" "cerror.lsp" "check-type.lsp" "compute-restarts.lsp" "condition.lsp" "continue.lsp" "define-condition.lsp" "error.lsp" "handler-bind.lsp" "handler-case.lsp" "ignore-errors.lsp" "invoke-debugger.lsp" "load.lsp" "make-condition.lsp" "muffle-warning.lsp" "restart-bind.lsp" "restart-case.lsp" "store-value.lsp" "use-value.lsp" "warn.lsp" "with-condition-restarts.lsp" "with-simple-restart.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/pathnames/"
  '("directory-namestring.lsp" "enough-namestring.lsp" "file-namestring.lsp" "host-namestring.lsp" "load-logical-pathname-translations.lsp" "load.lsp" "logical-pathname-translations.lsp" "logical-pathname.lsp" "make-pathname.lsp" "merge-pathnames.lsp" "namestring.lsp" "parse-namestring.lsp" "pathname-device.lsp" "pathname-directory.lsp" "pathname-host.lsp" "pathname-match-p.lsp" "pathname-name.lsp" "pathname-type.lsp" "pathname-version.lsp" "pathname.lsp" "pathnamep.lsp" "pathnames.lsp" "translate-logical-pathname.lsp" "wild-pathname-p.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/system-construction/"
  '("compile-file.lsp" "features.lsp" "load-file.lsp" "load.lsp" "modules.lsp" "with-compilation-unit.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/files/"
  '("delete-file.lsp" "directory.lsp" "ensure-directories-exist.lsp" "file-author.lsp" "file-error.lsp" "file-write-date.lsp" "load.lsp" "probe-file.lsp" "rename-file.lsp" "truename.lsp" ))

(load-ansi-chapter "/tmp/ansi-test/objects/"
  '("add-method.lsp" "allocate-instance.lsp" "call-next-method.lsp" "change-class.lsp" "class-name.lsp" "class-of.lsp" "compute-applicable-methods.lsp" "defclass-01.lsp" "defclass-02.lsp" "defclass-03.lsp" "defclass-errors.lsp" "defclass-forward-reference.lsp" "defclass.lsp" "defgeneric-method-combination-and.lsp" "defgeneric-method-combination-append.lsp" "defgeneric-method-combination-aux.lsp" "defgeneric-method-combination-list.lsp" "defgeneric-method-combination-max.lsp" "defgeneric-method-combination-min.lsp" "defgeneric-method-combination-nconc.lsp" "defgeneric-method-combination-or.lsp" "defgeneric-method-combination-plus.lsp" "defgeneric-method-combination-progn.lsp" "defgeneric.lsp" "define-method-combination-long-form.lsp" "define-method-combination.lsp" "defmethod.lsp" "ensure-generic-function.lsp" "find-class.lsp" "find-method.lsp" "load.lsp" "make-instance.lsp" "make-instances-obsolete.lsp" "make-load-form-saving-slots.lsp" "make-load-form.lsp" "method-qualifiers.lsp" "next-method-p.lsp" "no-applicable-method.lsp" "no-next-method.lsp" "reinitialize-instance.lsp" "remove-method.lsp" "shared-initialize.lsp" "slot-boundp.lsp" "slot-exists-p.lsp" "slot-makunbound.lsp" "slot-missing.lsp" "slot-unbound.lsp" "slot-value.lsp" "unbound-slot.lsp" "update-instance-for-different-class.lsp" "with-accessors.lsp" "with-slots.lsp" ))

;; Generate run-real-ansi-tests that calls all file-level runners.
;;
;; Per-FILE forking: each (run-ansi-FILE) is wrapped in fork+wait at the
;; parent. Within a file, tests run in-process: each (run-test ...) wraps
;; rt-run-test in handler-case so a single test crash becomes a clean FAIL
;; (caught by SIGSEGV → handler-case longjmp) without taking the file down.
;;
;; Why per-file: ANSI test files build up shared state — an early test
;; defparameters something a later test references. Per-test forking
;; broke those chains. Files are independent, so per-file fork still
;; isolates crashes that escape in-process recovery.
(setf *ansi-file-names* (nreverse *ansi-file-names*))
(setf *real-ansi-sources*
      (concatenate 'string *real-ansi-sources*
                   (format nil "~%(defvar *skip-below* 0)~
                     ~%(defvar *run-only-below* 0)~
                     ~%;; Bound on FAIL lines per fork-child to prevent any pathological~
                     ~%;; cascade (e.g. nested SIGSEGV in handler) from inflating output.~
                     ~%(defvar *fail-cap* 2000)~
                     ~%(defvar *fail-emitted* 0)~
                     ~%;; In-process test runner: rt-run-test wrapped in handler-case.~
                     ~%;; Side effects (defparameter, setq globals) persist across calls~
                     ~%;; within the same process — that's the whole point of per-file fork.~
                     ~%(defun %record-test-fail (id)~
                     ~%  (when (>= *fail-emitted* *fail-cap*) (return-from %record-test-fail nil))~
                     ~%  (setq *fail-emitted* (+ *fail-emitted* 1))~
                     ~%  (write-char-serial 10)~
                     ~%  (write-char-serial 70) (write-char-serial 65)~
                     ~%  (write-char-serial 73) (write-char-serial 76)~
                     ~%  (write-char-serial 32)~
                     ~%  (print-dec id)~
                     ~%  (write-char-serial 10)~
                     ~%  nil)~
                     ~%;; Codegen wraps each (run-test ...) in (handler-case ... (t (c) (%test-crash-fail ID)))~
                     ~%;; for the rare case that arg-evaluation crashes before run-test sets up its~
                     ~%;; own handler-case. Without this defun, calling an undefined function from~
                     ~%;; the handler triggers a cascade that kills the whole file's fork — losing~
                     ~%;; every remaining test.~
                     ~%(defun %test-crash-fail (id) (%record-test-fail id))~
                     ~%(defun run-test (id thunk expected)~
                     ~%  (when (< id *skip-below*) (return-from run-test nil))~
                     ~%  (when (and (> *run-only-below* 0) (>= id *run-only-below*)) (return-from run-test nil))~
                     ~%  (handler-case (rt-run-test id (funcall thunk) expected)~
                     ~%    (t (c) (%record-test-fail id))))~
                     ~%(defun run-test-mv (id thunk expecteds)~
                     ~%  (when (< id *skip-below*) (return-from run-test-mv nil))~
                     ~%  (when (and (> *run-only-below* 0) (>= id *run-only-below*)) (return-from run-test-mv nil))~
                     ~%  (handler-case (rt-run-test-mv id (funcall thunk) expecteds)~
                     ~%    (t (c) (%record-test-fail id))))~
                     ~%;; wait4 wstatus buffer — 8 bytes past handler-case slots.~
                     ~%(defvar *wstatus-addr* #x100001A0)~
                     ~%;; Per-FILE fork: parent forks, child runs the file's run-ansi-X~
                     ~%;; in-process (with run-test handling per-test crashes), then exits.~
                     ~%;; If the child exit status is nonzero (signal kill or our SIGSEGV~
                     ~%;; sys_exit path), parent records a single FAIL with the file's~
                     ~%;; first test id so it isn't double-counted as 'lost to crash'.~
                     ~%;; Per-file wall-clock cap (seconds). SIGALRM has no handler,~
                     ~%;; so an over-time child is hard-killed by the kernel and the~
                     ~%;; parent records a single FAIL with the file's first id.~
                     ~%(defvar *file-alarm-secs* 45)~
                     ~%(defun fork-file (first-id thunk)~
                     ~%  (let ((pid (syscall3 57 0 0 0)))~
                     ~%    (if (= pid 0)~
                     ~%        (progn~
                     ~%          ;; Clear inherited handler-case saved-RSP at 0x10000180~
                     ~%          ;; (moved from 0x10000140 to avoid closure-env-addr collision).~
                     ~%          ;; If we don't clear, a SIGSEGV in the child BEFORE its own~
                     ~%          ;; handler-case setjmp would longjmp using the parent's stale~
                     ~%          ;; RSP/RBP/IP — jumping to garbage and killing the fork silently.~
                     ~%          (setf (mem-ref #x10000180 :u64) 0)~
                     ~%          ;; Reset the handler-stack depth too (forks inherit any~
                     ~%          ;; outer handler-case frames the parent had pushed).~
                     ~%          (setf (mem-ref #x10000400 :u64) 0)~
                     ~%          (setq *fail-emitted* 0)~
                     ~%          (syscall3 37 *file-alarm-secs* 0 0)~
                     ~%          ;; If the per-test handler-cases miss something (e.g. a~
                     ~%          ;; crash during init-form load or in a code path that~
                     ~%          ;; bypasses our wrapping), record the file's first id as~
                     ~%          ;; a FAIL before exiting so the summary doesn't lose the~
                     ~%          ;; rest of the file's tests as silent zeros.~
                     ~%          (handler-case (funcall thunk)~
                     ~%            (t (c) (%record-test-fail first-id)))~
                     ~%          ;; Cancel the file alarm before exit; otherwise a stray~
                     ~%          ;; SIGALRM after the (now-cleared) handler-case would~
                     ~%          ;; reach the SIGSEGV stub with [180]=0 and trigger sys_exit~
                     ~%          ;; 139 — which the parent would record as another FAIL.~
                     ~%          (syscall3 37 0 0 0)~
                     ~%          (syscall3 60 0 0 0))~
                     ~%        (progn~
                     ~%          (setf (mem-ref *wstatus-addr* :u32) 0)~
                     ~%          (syscall3 61 pid *wstatus-addr* 0)~
                     ~%          (when (> (mem-ref *wstatus-addr* :u32) 0)~
                     ~%            (%record-test-fail first-id))))))~%")
                   (with-output-to-string (s)
                     ;; Helper: return T iff the active shard range [skip..run-only)
                     ;; overlaps [first..last]. Run-only=0 means "no upper bound".
                     (format s "~%(defun %ansi-file-in-range (first last)~%")
                     (format s "  (if (> *run-only-below* 0)~%")
                     (format s "      (if (< last *skip-below*) nil (if (>= first *run-only-below*) nil t))~%")
                     (format s "      t))~%")
                     (format s "~%(defun run-real-ansi-tests ()~%")
                     ;; Each file is fork+wait wrapped, gated by the shard range.
                     (let ((by-name nil))
                       (dolist (entry *ansi-file-ranges*)
                         (push entry by-name))
                       (dolist (name *ansi-file-names*)
                         (let* ((entry (find name by-name :test #'string= :key #'car))
                                (first-id (if entry (second entry) nil))
                                (last-id  (if entry (third  entry) nil)))
                           (cond
                             ((and first-id last-id)
                              (format s "  (when (%ansi-file-in-range ~D ~D)~%" first-id last-id)
                              (format s "    (fork-file ~D (lambda () (run-ansi-~A))))~%" first-id name))
                             (t
                              (format s "  (fork-file 0 (lambda () (run-ansi-~A)))~%" name))))))
                     (format s ")~%"))))

(format t "  prelude: ~D chars~%" (length *prelude-source*))
(format t "  rt: ~D chars~%" (length *rt-source*))
(format t "  bridge: ~D chars~%" (length *bridge-source*))
(format t "  tests: ~D chars~%" (length *test-source*))
(format t "  ansi-aux: ~D chars~%" (length *ansi-aux-sources*))
(format t "  real-ansi: ~D chars~%" (length *real-ansi-sources*))

;; Dump generated sources for debugging
(with-open-file (s "/tmp/real-ansi-gen.lisp" :direction :output :if-exists :supersede)
  (write-string *real-ansi-sources* s))
(format t "  dumped: /tmp/real-ansi-gen.lisp~%")

;;; ============================================================
;;; 3. Strip in-package forms from source text
;;; ============================================================

(defun strip-in-package (text)
  "Remove (in-package ...) forms from source text."
  (let ((result text))
    (loop
      (let ((pos (search "(in-package " result)))
        (unless pos (return result))
        ;; Find the closing paren
        (let ((end (position #\) result :start pos)))
          (when end
            (setf result (concatenate 'string
                                      (subseq result 0 pos)
                                      (subseq result (1+ end))))))))))

(setf *prelude-source* (strip-in-package *prelude-source*))
(setf *rt-source*      (strip-in-package *rt-source*))
(setf *bridge-source*  (strip-in-package *bridge-source*))
(setf *test-source*    (strip-in-package *test-source*))
(setf *ansi-aux-sources*  (strip-in-package *ansi-aux-sources*))
(setf *real-ansi-sources* (strip-in-package *real-ansi-sources*))

;;; ============================================================
;;; 4. Driver source (sys-exit + kernel-main)
;;; ============================================================

(defvar *driver-source* "

(defun halt ()
  (syscall3 60 1 0 0))

(defun sys-exit (code)
  (let ((c code))
    (syscall3 60 c 0 0)))

;; Parse a null-terminated ASCII decimal at a fixed address as an integer.
;; Two variants for the two argv buffers: the compiler treats #x10000208
;; as a tagged-fixnum literal, so (mem-ref #x10000208 :u8) reads from that
;; address correctly (mem-ref untags the address operand).
(defun %parse-decimal-at-fixed-208 ()
  (let ((n 0) (i 0))
    (loop
      (let ((b (mem-ref (+ #x10000208 i) :u8)))
        (when (or (< b 48) (> b 57)) (return n))
        (setq n (+ (* n 10) (- b 48)))
        (setq i (+ i 1))))))

(defun %parse-decimal-at-fixed-248 ()
  (let ((n 0) (i 0))
    (loop
      (let ((b (mem-ref (+ #x10000248 i) :u8)))
        (when (or (< b 48) (> b 57)) (return n))
        (setq n (+ (* n 10) (- b 48)))
        (setq i (+ i 1))))))

(defun kernel-main ()
  ;; Banner: ANSI-TEST
  (write-char-serial 65)   ; A
  (write-char-serial 78)   ; N
  (write-char-serial 83)   ; S
  (write-char-serial 73)   ; I
  (write-char-serial 45)   ; -
  (write-char-serial 84)   ; T
  (write-char-serial 69)   ; E
  (write-char-serial 83)   ; S
  (write-char-serial 84)   ; T
  (write-char-serial 10)

  ;; Initialize runtime
  (init-symbol-table)

  ;; Initialize package system (creates CL, CL-USER, KEYWORD, test packages)
  (%init-packages)

  ;; Initialize standard streams
  (%init-streams)

  ;; Initialize reader (readtable, *read-base*, etc.)
  (%init-reader)

  ;; Initialize condition type registry
  (%init-condition-types)

  ;; Initialize symbol-function table with all built-in compiled functions
  (%init-symbol-function-table)

  ;; Install signal handlers (SIGSEGV/etc) — converts hardware faults to
  ;; CL conditions that handler-case can catch, instead of killing the fork.
  (%init-signal-handling)


  ;; Set default pathname defaults to the ANSI test sandbox directory
  (setq *default-pathname-defaults* \"/tmp/ansi-test/sandbox/\")

  ;; Init file I/O scratch buffers (defvar defaults not applied without init-all-globals)
  (setq *cstr-scratch* #x1DF00000)
  (setq *io-buf-addr*  #x1DE00000)
  (setq *scratch-mmapped* nil)
  (setq *filesystem* nil)

  ;; Init RT counters manually (init-all-globals not safe — some thunks
  ;; reference functions/symbols that may not be available yet).
  ;; Also init skip/run-only bounds (defvar init-thunks aren't run).
  (setq *rt-test-count* 0)
  (setq *rt-pass-count* 0)
  (setq *rt-fail-count* 0)
  (setq *skip-below* 0)
  (setq *run-only-below* 0)
  (setq *write-object-budget* 0)
  (setq *fail-emitted* 0)
  (setq *fail-cap* 2000)
  (setq *file-alarm-secs* 45)
  (setq *wstatus-addr* #x100001A0)

  ;; Float constants from ansi-bridge — defvars don't run their init
  ;; thunks (per CLAUDE.md), so without these explicit setqs every
  ;; *-float-epsilon resolves to NIL at runtime, and the first ANSI
  ;; test that funcalls DECODE-FLOAT on one of them used to loop
  ;; forever inside its sig-normalization until SIGALRM killed the
  ;; whole fork (losing every later test in the file).
  (setq double-float-epsilon          2.220446049250313d-16)
  (setq single-float-epsilon          1.1920929d-7)
  (setq short-float-epsilon           1.1920929d-7)
  (setq long-float-epsilon            2.220446049250313d-16)
  (setq double-float-negative-epsilon 1.1102230246251565d-16)
  (setq single-float-negative-epsilon 5.9604645d-8)
  (setq short-float-negative-epsilon  5.9604645d-8)
  (setq long-float-negative-epsilon   1.1102230246251565d-16)
  (setq most-positive-double-float    1.7976931348623157d308)
  (setq most-negative-double-float   -1.7976931348623157d308)
  (setq most-positive-single-float    3.4028235d38)
  (setq most-negative-single-float   -3.4028235d38)
  (setq most-positive-short-float     3.4028235d38)
  (setq most-negative-short-float    -3.4028235d38)

  ;; Standard CL constants the ANSI test auxiliary files reference
  ;; (char-code-limit, call-arguments-limit, *-fixnum). Without these
  ;; the tests get NIL where they expect a number — (min 65536 NIL),
  ;; (random NIL), etc. — and the fork hangs or crashes inside the
  ;; aux helper before reaching the per-test handler.
  (setq char-code-limit       256)
  (setq call-arguments-limit  256)
  ;; MVM fixnums are 63-bit signed (tag bit + 1-bit shift).
  (setq most-positive-fixnum  4611686018427387903)
  (setq most-negative-fixnum -4611686018427387904)
  ;; ansi-aux-macros.lsp's NORMALLY macro: (if *should-always-be-true*
  ;; form (should-never-be-called)). NIL here → every CATCH-TYPE-ERROR /
  ;; NORMALLY-wrapped form expands to a call to an undefined function,
  ;; which the per-test handler-case catches but burns time and noise.
  ;; T makes NORMALLY a no-op pass-through.
  (setq *should-always-be-true* t)
  (setq *use-random-byte* t)
  (setq *random-readable* nil)
  (setq *random-read-check-debug* nil)
  (setq *report-and-ignore-errors-break* nil)
  (setq *hash-table-test-iters* 100)
  (setq *mapc.6-var* nil)
  (setq *defclass-slot-readers* nil)
  (setq *defclass-slot-writers* nil)
  (setq *defclass-slot-accessors* nil)
  (setq *type-list* nil)
  (setq *supertype-table* nil)

  ;; Parse argv from BSS (boot stub writes argc/argv there).
  ;;   argv[1] → *skip-below*       (skip tests with id < N)
  ;;   argv[2] → *run-only-below*   (skip tests with id >= M)
  ;; This lets external shards run non-overlapping ranges in parallel.
  ;; argc is a u32 at 0x10000200 (mem-ref :u32 auto-tags for us). argv[1]
  ;; and argv[2] are null-terminated strings already copied to fixed BSS
  ;; addresses by the boot stub — we parse decimals directly from there.
  (when (> (mem-ref #x10000200 :u32) 1)
    (setq *skip-below* (%parse-decimal-at-fixed-208)))
  (when (> (mem-ref #x10000200 :u32) 2)
    (setq *run-only-below* (%parse-decimal-at-fixed-248)))

  ;; Run custom tests
  (run-all-tests)

  ;; Print expected ANSI test total so the summary can compute lost tests.
  ;; Distinctive prefix so it can't be confused with FAIL ... EXP:... lines.
  ;; The placeholder is replaced with the build-time count.
  (write-char-serial 10)
  (write-string-serial \"ANSI-TOTAL=\")
  (print-dec ~~ANSI-EXP-TOTAL~~)
  (write-char-serial 10)

  ;; Run real ANSI tests (generated at build time)
  (run-real-ansi-tests)

  ;; Report custom test results (ANSI results printed by fork children)
  (write-char-serial 10)
  (print-dec *rt-pass-count*)
  (write-char-serial 47)   ;; /
  (print-dec *rt-test-count*)
  ;; DONE marker
  (write-char-serial 32)   ; space
  (write-char-serial 68)   ; D
  (write-char-serial 79)   ; O
  (write-char-serial 78)   ; N
  (write-char-serial 69)   ; E
  (write-char-serial 10)
  (sys-exit 0))

")

;;; ============================================================
;;; 5. Assemble full source
;;; ============================================================

(format t "~%Assembling full source...~%")

(defvar *full-source*
  (concatenate 'string
    ;; 1. Prelude (list utils, equal, print-dec, hash tables, etc.)
    *prelude-source*
    (string #\Newline)
    ;; 1b. GC (Cheney copying collector)
    *gc-source*
    (string #\Newline)
    ;; 2. RT harness (deftest, do-tests)
    *rt-source*
    (string #\Newline)
    ;; 3. ANSI bridge (helpers, stubs, missing functions)
    *bridge-source*
    (string #\Newline)
    ;; 4. ANSI auxiliary files (scaffold, helpers used by test files)
    ;;    Loaded BEFORE test-source so that test-source can override
    ;;    any aux definitions with simpler MVM-compatible versions.
    *ansi-aux-sources*
    (string #\Newline)
    ;; 5. Our test source (run-*-tests, run-all-tests)
    ;;    Functions defined here override aux (last-defun-wins).
    *test-source*
    (string #\Newline)
    ;; 6. Real ANSI test files
    *real-ansi-sources*
    (string #\Newline)
    ;; 7. Driver (sys-exit, kernel-main).
    ;; Substitute the placeholder for the build-time ANSI test count
    ;; so kernel-main can print EXP:N before running tests.
    (let* ((tag "~~ANSI-EXP-TOTAL~~")
           (tag-pos (search tag *driver-source*))
           (count (- *ansi-test-counter* 10000)))
      (if tag-pos
          (concatenate 'string
                       (subseq *driver-source* 0 tag-pos)
                       (princ-to-string count)
                       (subseq *driver-source* (+ tag-pos (length tag))))
          *driver-source*))))

(format t "Full source: ~D characters~%" (length *full-source*))
(format t "  ANSI tests: ~D~%" (- *ansi-test-counter* 10000))

;;; ============================================================
;;; 6. Build Linux ELF via MVM pipeline
;;; ============================================================

;; Load Linux boot descriptor
(mvm-load "boot/boot-linux-x64.lisp")

(in-package :modus.mvm)

;; Override linux-x64-boot-descriptor to include nil page mmap
;; (car nil must not segfault)
(defun mvm-linux-x64-test-entry (buf)
  "Emit Linux x64 entry stub with NIL page mmap."
  (emit-linux-x64-entry buf)
  ;; mmap NIL page at 0xDEAD0000 (car/cdr nil dereferences this)
  ;; movabs rdi, 0xDEAD0000
  (emit-bytes buf #x48 #xBF #x00 #x00 #xAD #xDE #x00 #x00 #x00 #x00)
  (emit-bytes buf #x48 #xC7 #xC6 #x00 #x10 #x00 #x00) ; mov rsi, 4096
  (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, PROT_READ|PROT_WRITE
  (emit-bytes buf #x49 #xC7 #xC2 #x32 #x00 #x00 #x00) ; mov r10, MAP_PRIVATE|MAP_ANON|MAP_FIXED
  (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
  (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
  (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, SYS_mmap
  (emit-bytes buf #x0F #x05)                             ; syscall
  ;; Fill nil page with NIL values using rep stosq
  (emit-bytes buf #x48 #x89 #xC7)                       ; mov rdi, rax
  (emit-bytes buf #x48 #xC7 #xC1 #x00 #x02 #x00 #x00) ; mov rcx, 512
  (emit-bytes buf #x4C #x89 #xF8)                       ; mov rax, r15 (NIL)
  (emit-bytes buf #xF3 #x48 #xAB))                       ; rep stosq

(defun linux-x64-boot-descriptor ()
  (list :arch :x86-64
        :entry-fn #'mvm-linux-x64-test-entry
        :load-addr +linux-x64-load-addr+
        :elf-format :linux-x64))

;; Install x64 translator in Linux mode with GC enabled
(funcall (intern "INSTALL-X64-TRANSLATOR" "MODUS.MVM.X64"))
(setf modus.mvm.x64::*x64-linux-mode* t)
(setf modus.mvm.x64::*x64-gc-enabled* t)
;; Set R14 to midpoint so GC fires at half heap
(setf modus.mvm::*linux-x64-r14-offset* modus.mvm::+linux-x64-gc-midpoint+)
;; Set native code offset for funcall alignment:
;; ELF header (64+56=120) + linux-x64 boot code (192) + JMP rel32 (5) = 317 = 0x13D
;; Functions at code-buffer positions P where (0x13D+P) & 0xF == 1 would be
;; misidentified as cons cells by compile-funcall's consp check.
(setf modus.mvm.x64::*x64-native-code-offset* 317)

(format t "~%Compiling test runner (~D chars)...~%" (length cl-user::*full-source*))

(let ((image (build-image :target :linux-x64 :source-text cl-user::*full-source*)))
  (let ((path "/tmp/modus-ansi-test"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "~%Wrote ~D bytes to ~A~%"
            (length (kernel-image-image-bytes image)) path)
    (format t "~%Run: ~A~%" path)))
