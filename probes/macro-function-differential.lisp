;;;; macro-function-differential.lisp — CROSS-IMPLEMENTATION macro/function battery.
;;;;
;;;; PURPOSE
;;;;   CLHS 11.1.2.1.2 / 3.1.2.1.2.2: a standard CL *function* may never have a
;;;;   macro definition in a conforming implementation, and (MACRO-FUNCTION 'f)
;;;;   must be NIL for it.  Modus compiles several standard functions (LIST,
;;;;   VECTOR, FIRST..TENTH, MIN, MAX, ABS, …) through the compiler's INLINING
;;;;   macro table — legitimate as a compile-time optimisation, but the build
;;;;   used to auto-scan every one of those inlining macros into the RUNTIME
;;;;   *macro-table*, which MACRO-FUNCTION consults.  The result: standard
;;;;   functions reported as macros to user code.  Only code that INSPECTS
;;;;   before calling notices (code walkers: iterate's sharpL-reader, alexandria,
;;;;   any (if (macro-function x) …) dispatch), which is why it went unseen.
;;;;
;;;; HOW TO USE
;;;;   ./probes/run-macro-function-differential.sh <modus-binary>
;;;;   Runs this same file under SBCL (the oracle) and under Modus, diffs.
;;;;     left  (<) = SBCL / ANSI-correct       right (>) = Modus
;;;;
;;;; HOW TO ADD A CASE
;;;;   One (chk-mf NAME) line per symbol.  Values are :MACRO / :FUNCTION only —
;;;;   never a function object (printed representations differ between
;;;;   implementations and would produce pure-noise diffs).
;;;;
;;;;   Section 1 = standard FUNCTIONS: every one must print :FUNCTION.
;;;;   Section 2 = standard MACROS and SPECIAL OPERATORS: these are the
;;;;   OVERREACH GUARD.  A denylist that is too wide silently deletes a real
;;;;   macro, which is a far worse regression than the bug being fixed, and
;;;;   section 2 is what catches it.
;;;;
;;;; Every form is EVAL'd inside a handler-case, so a case that errors prints
;;;; :ERROR and the battery continues — a MISSING line is a FAILED case, never
;;;; a passing one.

(defmacro chk (label form)
  `(let ((v (handler-case ,form
              (error (c) (progn c :ERROR))
              (condition (c) (progn c :SIGNALLED)))))
     (format t "~a	~s~%" ,label v)))

(defmacro chk-mf (name)
  "Print `mf.NAME<TAB>:MACRO` or `:FUNCTION` for the standard symbol NAME."
  `(chk ,(concatenate 'string "mf." (string-downcase (symbol-name name)))
        (if (macro-function ',name) :MACRO :FUNCTION)))

;;; ------------------------------------------------------------------
;;; 1. STANDARD CL FUNCTIONS.  Every line MUST read :FUNCTION.
;;;    The first block is exactly the set the compiler registers as an
;;;    inlining macro (mvm/compiler.lisp mvm-define-macro) — the suspects.
;;; ------------------------------------------------------------------
(chk-mf list)     (chk-mf vector)
(chk-mf first)    (chk-mf second)   (chk-mf third)    (chk-mf fourth)
(chk-mf fifth)    (chk-mf sixth)    (chk-mf seventh)  (chk-mf eighth)
(chk-mf ninth)    (chk-mf tenth)    (chk-mf rest)
(chk-mf caddr)    (chk-mf cadddr)   (chk-mf cdddr)
(chk-mf abs)      (chk-mf min)      (chk-mf max)
(chk-mf minusp)   (chk-mf plusp)    (chk-mf /=)
(chk-mf lognot)   (chk-mf ldb)      (chk-mf maphash)
(chk-mf make-instance)              (chk-mf get-setf-expansion)

;;; Control block: standard functions the compiler does NOT inline this way.
;;; They were already clean; if one of them flips, the fix overreached into
;;; a code path it had no business touching.
(chk-mf cons)     (chk-mf car)      (chk-mf cdr)      (chk-mf append)
(chk-mf length)   (chk-mf cadr)     (chk-mf cddr)     (chk-mf nth)
(chk-mf nthcdr)   (chk-mf last)     (chk-mf reverse)  (chk-mf nreverse)
(chk-mf mapcar)   (chk-mf apply)    (chk-mf funcall)  (chk-mf eval)
(chk-mf +)        (chk-mf -)        (chk-mf *)        (chk-mf /)
(chk-mf =)        (chk-mf <)        (chk-mf >)        (chk-mf zerop)
(chk-mf logand)   (chk-mf logior)   (chk-mf logxor)   (chk-mf ash)
(chk-mf aref)     (chk-mf elt)      (chk-mf svref)    (chk-mf gethash)
(chk-mf intern)   (chk-mf symbol-name)                (chk-mf symbol-value)
(chk-mf format)   (chk-mf print)    (chk-mf read)     (chk-mf values)
(chk-mf sort)     (chk-mf find)     (chk-mf position) (chk-mf remove)
(chk-mf concatenate)                (chk-mf coerce)   (chk-mf typep)
;; Deliberately-undefined name: proves an unknown symbol reads as :FUNCTION
;; on BOTH sides, so a diff here would mean the reader/lookup, not the table.
(chk-mf no-such-operator-xyzzy)

;;; ------------------------------------------------------------------
;;; 2. STANDARD MACROS — the OVERREACH GUARD.  Every line MUST read :MACRO
;;;    on SBCL; any Modus :FUNCTION here is a real macro that went missing.
;;; ------------------------------------------------------------------
(chk-mf when)     (chk-mf unless)   (chk-mf cond)     (chk-mf case)
(chk-mf ecase)    (chk-mf ccase)    (chk-mf typecase) (chk-mf etypecase)
(chk-mf and)      (chk-mf or)       (chk-mf push)     (chk-mf pop)
(chk-mf pushnew)  (chk-mf incf)     (chk-mf decf)     (chk-mf setf)
(chk-mf psetf)    (chk-mf psetq)    (chk-mf rotatef)  (chk-mf shiftf)
(chk-mf remf)     (chk-mf dolist)   (chk-mf dotimes)  (chk-mf do)
(chk-mf do*)      (chk-mf loop)     (chk-mf prog1)    (chk-mf prog2)
(chk-mf defun)    (chk-mf defmacro) (chk-mf defvar)   (chk-mf defparameter)
(chk-mf defconstant)                (chk-mf defclass) (chk-mf defgeneric)
(chk-mf defmethod)                  (chk-mf defstruct)
(chk-mf deftype)  (chk-mf defsetf)  (chk-mf defpackage)
(chk-mf in-package)                 (chk-mf destructuring-bind)
(chk-mf multiple-value-bind)        (chk-mf multiple-value-setq)
(chk-mf nth-value)                  (chk-mf handler-case)
(chk-mf handler-bind)               (chk-mf restart-case)
(chk-mf ignore-errors)              (chk-mf assert)   (chk-mf check-type)
(chk-mf with-slots)                 (chk-mf with-accessors)
(chk-mf with-open-file)             (chk-mf with-output-to-string)
(chk-mf with-input-from-string)     (chk-mf define-modify-macro)
(chk-mf define-compiler-macro)      (chk-mf define-symbol-macro)
(chk-mf define-setf-expander)       (chk-mf define-condition)
(chk-mf define-method-combination)  (chk-mf do-symbols)
(chk-mf do-external-symbols)        (chk-mf do-all-symbols)
(chk-mf with-package-iterator)      (chk-mf with-compilation-unit)
(chk-mf declaim)  (chk-mf lambda)   (chk-mf return)   (chk-mf step)
(chk-mf time)     (chk-mf trace)    (chk-mf untrace)  (chk-mf pprint-logical-block)

;;; ------------------------------------------------------------------
;;; 3. MACROEXPAND behaviour.  A code walker calls MACROEXPAND on user forms;
;;;    if a standard function call rewrites, the walker sees a form the user
;;;    never wrote.  The value compared is the OPERATOR of the expansion, so
;;;    the case is stable across implementations that legitimately differ in
;;;    the expansion's shape.
;;; ------------------------------------------------------------------
(chk "expand.list-op"   (let ((x (macroexpand '(list 1 2))))
                          (if (consp x) (string (car x)) :NOT-A-CONS)))
(chk "expand.first-op"  (let ((x (macroexpand '(first y))))
                          (if (consp x) (string (car x)) :NOT-A-CONS)))
(chk "expand.vector-op" (let ((x (macroexpand '(vector 1 2))))
                          (if (consp x) (string (car x)) :NOT-A-CONS)))
(chk "expand.min-op"    (let ((x (macroexpand '(min 1 2))))
                          (if (consp x) (string (car x)) :NOT-A-CONS)))
(chk "expand.when-op"   (let ((x (macroexpand '(when a b))))
                          (if (consp x) (string (car x)) :NOT-A-CONS)))

;;; ------------------------------------------------------------------
;;; 4. The functions must still WORK — the fix removes a macro-table entry,
;;;    and a call path that silently depended on the inlining would show up
;;;    here as a wrong value rather than as a macro-ness diff.
;;; ------------------------------------------------------------------
(chk "call.list"         (list 1 2 3))
(chk "call.list-funcall" (funcall #'list 1 2))
(chk "call.list-apply"   (apply #'list '(1 2)))
(chk "call.list-mapcar"  (mapcar #'list '(1 2)))
(chk "call.vector"       (coerce (vector 1 2 3) 'list))
(chk "call.first"        (first '(9 8 7)))
(chk "call.rest"         (rest '(9 8 7)))
(chk "call.third"        (third '(9 8 7)))
(chk "call.min"          (min 3 1 2))
(chk "call.max"          (max 3 1 2))
(chk "call.abs"          (abs -4))
(chk "call.minusp"       (minusp -1))
(chk "call.plusp"        (plusp 1))
(chk "call./="           (/= 1 2))
(chk "call.lognot"       (lognot 0))
(chk "call.ldb"          (ldb (byte 4 0) 255))
(chk "call.caddr"        (caddr '(1 2 3 4)))
(chk "call.cdddr"        (cdddr '(1 2 3 4)))
(chk "call.mapcar-first" (mapcar #'first '((1 2) (3 4))))
(chk "call.reduce-max"   (reduce #'max '(1 5 2)))

;;; ------------------------------------------------------------------
;;; 5. FBOUNDP / SPECIAL-OPERATOR-P sanity.  Removing a macro-table entry
;;;    must not make the name unbound: a standard function stays FBOUNDP.
;;; ------------------------------------------------------------------
(chk "fboundp.list"   (if (fboundp 'list) :YES :NO))
(chk "fboundp.vector" (if (fboundp 'vector) :YES :NO))
(chk "fboundp.first"  (if (fboundp 'first) :YES :NO))
(chk "fboundp.min"    (if (fboundp 'min) :YES :NO))
(chk "fboundp.when"   (if (fboundp 'when) :YES :NO))

;;; ------------------------------------------------------------------
;;; 6. The code-walker shape that actually bites: filtering a list of
;;;    operators by macro-ness.  This is what iterate/alexandria-style
;;;    walkers do, and it is the user-visible consequence of the bug.
;;; ------------------------------------------------------------------
(chk "walker.fns-seen-as-macros"
     (let ((bad nil))
       (dolist (s '(list vector first second third rest min max abs))
         (when (macro-function s) (push (string s) bad)))
       (sort bad #'string<)))
(chk "walker.macros-seen-as-macros"
     (let ((good nil))
       (dolist (s '(when unless cond push pop incf setf dolist))
         (when (macro-function s) (push (string s) good)))
       (length good)))

(format t "MACRO-FUNCTION-DIFFERENTIAL-DONE~%")
