;;;; registry-differential.lisp — CROSS-IMPLEMENTATION registry battery.
;;;;
;;;; PURPOSE
;;;;   Modus keeps roughly a dozen "registries" (name -> definition tables):
;;;;   the function table, the macro table, deftype, defstruct, setf
;;;;   expanders, symbol-macros, compiler-macros, method combinations, the
;;;;   CLOS class table, the CLOS generic-function table and the condition
;;;;   type table.  Every one of them has been written independently, and
;;;;   four of them have now been found keyed by the symbol's NAME with no
;;;;   regard for its PACKAGE (#211 fn table, e4d26a8 macro table, 101c5eb
;;;;   GF table, and this file's subject: the CLOS class + condition tables).
;;;;
;;;;   CLHS 11.1: symbols in different packages ARE DIFFERENT SYMBOLS.
;;;;   Classes, condition types, types, functions, macros and every other
;;;;   named thing are named BY SYMBOL, never by name-string.  So the whole
;;;;   family shares ONE semantics, and one battery can check all of them at
;;;;   once against a conforming implementation.
;;;;
;;;; HOW TO USE
;;;;   ./probes/run-registry-differential.sh <modus-binary>
;;;;   which runs this same file under SBCL (the oracle) and under Modus and
;;;;   diffs the two transcripts line by line.  Any diff line is a Modus
;;;;   conformance bug (or an oracle case that needs its expectation fixed).
;;;;
;;;; HOW TO ADD A CASE
;;;;   Add one (chk "label" form) line.  Rules:
;;;;     * portable ANSI CL only -- no Modus internals (%find-gf & co.), or
;;;;       the oracle cannot run it;
;;;;     * the form must return a SMALL PRINTABLE value: a boolean, keyword,
;;;;       integer or symbol name string.  Never a class/instance/function
;;;;       object -- their printed representations legitimately differ
;;;;       between implementations and would produce noise diffs;
;;;;     * define the two same-named things in packages DA and DB and then
;;;;       ask DA for its own.  A package-blind registry answers with DB's.
;;;;
;;;; Every form is EVAL'd inside a handler-case, so a case that errors
;;;; prints :ERROR and the battery keeps going -- one broken case can never
;;;; truncate the transcript (a missing line would otherwise read as a
;;;; passing case, which is exactly the failure mode this file exists to
;;;; eliminate).

(defpackage "DA" (:use "CL"))
(defpackage "DB" (:use "CL"))

(defmacro chk (label form)
  "Print LABEL<TAB>value-of-FORM.  Never lets FORM abort the battery."
  `(let ((v (handler-case ,form
              (error (c) (progn c :ERROR))
              (condition (c) (progn c :SIGNALLED)))))
     ;; ~S, not ~A: PRINC of a keyword drops the colon, and Modus and SBCL
     ;; disagree about that -- an unrelated printer difference that would
     ;; otherwise show up as a diff on every keyword-valued case.
     (format t "~a	~s~%" ,label v)))

;;; ------------------------------------------------------------------
;;; 0. SYMBOL IDENTITY -- the premise everything below rests on.  If these
;;;    two lines diverge, no registry fix can be correct: the reader itself
;;;    is collapsing the packages and every "registry" bug downstream is a
;;;    symptom rather than a cause.  Check these FIRST.
;;; ------------------------------------------------------------------
(chk "symbol.not-eq"    (not (eq 'da::zz 'db::zz)))
(chk "symbol.package"   (package-name (symbol-package 'da::zz)))
(chk "symbol.name"      (symbol-name 'da::zz))

;;; ------------------------------------------------------------------
;;; 1. FUNCTION table  (fixed for Modus in #211)
;;; ------------------------------------------------------------------
(defun da::fn () :FROM-DA)
(defun db::fn () :FROM-DB)
(chk "fn.da"            (da::fn))
(chk "fn.db"            (db::fn))
(chk "fn.not-eq"        (not (eq #'da::fn #'db::fn)))

;;; ------------------------------------------------------------------
;;; 2. MACRO table  (fixed for Modus in e4d26a8)
;;; ------------------------------------------------------------------
(defmacro da::mac () :MAC-FROM-DA)
(defmacro db::mac () :MAC-FROM-DB)
(chk "macro.da"         (da::mac))
(chk "macro.db"         (db::mac))

;;; ------------------------------------------------------------------
;;; 3. GENERIC FUNCTION table  (fixed for Modus in 101c5eb)
;;; ------------------------------------------------------------------
(defgeneric da::gf (x))
(defmethod  da::gf ((x integer)) :FROM-DA)
(defgeneric db::gf (x))
(defmethod  db::gf ((x integer)) :FROM-DB)
(chk "gf.da"            (da::gf 1))
(chk "gf.db"            (db::gf 1))
(chk "gf.not-eq"        (not (eq #'da::gf #'db::gf)))

;;; A generic function of one name may be defined with an INCOMPATIBLE
;;; lambda list in another package; congruence is per-GF, so neither
;;; defgeneric may complain about the other's arity.
(defgeneric da::gfw (a b c d))
(defmethod  da::gfw ((a t) (b t) (c t) (d t)) :FROM-DA)
(chk "gf.wide.other-arity-ok"
     (progn (eval '(defgeneric db::gfw (a)))
            (eval '(defmethod db::gfw ((a t)) :FROM-DB))
            :OK))
(chk "gf.wide.da"       (da::gfw 1 2 3 4))
(chk "gf.wide.db"       (db::gfw 1))

;;; ------------------------------------------------------------------
;;; 4. CLOS CLASS table  (the subject of task #241)
;;; ------------------------------------------------------------------
(defclass da::cls () ((da::aa :initarg :aa :initform 1)))
(defclass db::cls () ((db::bb :initarg :bb :initform 2)))
(chk "class.not-eq"     (not (eq (find-class 'da::cls) (find-class 'db::cls))))
(chk "class.own-slot"   (slot-exists-p (make-instance 'da::cls) 'da::aa))
(chk "class.other-slot" (slot-exists-p (make-instance 'da::cls) 'db::bb))
(chk "class.cross-typep"      (typep (make-instance 'da::cls) 'db::cls))
(chk "class.own-typep"        (typep (make-instance 'da::cls) 'da::cls))
(chk "class.cross-subtypep"   (multiple-value-bind (s v) (subtypep 'da::cls 'db::cls)
                                (list s v)))
(chk "class.own-subtypep"     (multiple-value-bind (s v) (subtypep 'da::cls 'da::cls)
                                (list s v)))
(chk "class.class-name.da"    (symbol-name (class-name (find-class 'da::cls))))
(chk "class.slot-value.da"    (slot-value (make-instance 'da::cls) 'da::aa))

;;; An inheritance chain in one package must not be visible from the
;;; same-named class in the other.
(defclass da::sub (da::cls) ())
(chk "class.sub-of-own"       (typep (make-instance 'da::sub) 'da::cls))
(chk "class.sub-of-other"     (typep (make-instance 'da::sub) 'db::cls))

;;; ------------------------------------------------------------------
;;; 5. CONDITION type table  (the headline bug of task #241)
;;; ------------------------------------------------------------------
(define-condition da::ce (error) ())
(define-condition db::ce (error) ())
(chk "cond.own-catch"
     (handler-case (error 'da::ce) (da::ce () :CAUGHT) (error () :FELL-THROUGH)))
(chk "cond.cross-catch"
     (handler-case (error 'da::ce) (db::ce () :WRONGLY-CAUGHT) (error () :CORRECTLY-UNCAUGHT)))
(chk "cond.cross-typep"       (typep (make-condition 'da::ce) 'db::ce))
(chk "cond.own-typep"         (typep (make-condition 'da::ce) 'da::ce))
(chk "cond.not-eq-class"      (not (eq (find-class 'da::ce) (find-class 'db::ce))))
(chk "cond.type-of.da"        (symbol-name (type-of (make-condition 'da::ce))))
(chk "cond.cross-subtypep"    (multiple-value-bind (s v) (subtypep 'da::ce 'db::ce)
                                (list s v)))

;;; Distinct SLOTS on same-named conditions: reading DB's slot off a DA
;;; condition must not silently work.
(define-condition da::cs (error) ((da::x :initarg :x :initform 10 :reader da::cs-x)))
(define-condition db::cs (error) ((db::y :initarg :y :initform 20 :reader db::cs-y)))
(chk "cond.slot.own"          (da::cs-x (make-condition 'da::cs)))
(chk "cond.slot.other-reader" (db::cs-y (make-condition 'da::cs)))

;;; A HANDLER-BIND (rather than handler-case) must discriminate too.
(chk "cond.handler-bind"
     (let ((hit :NONE))
       (ignore-errors
         (handler-bind ((db::ce (lambda (c) (progn c (setq hit :WRONGLY-RAN)))))
           (error 'da::ce)))
       hit))

;;; ------------------------------------------------------------------
;;; 6. DEFTYPE table
;;; ------------------------------------------------------------------
(deftype da::ty () 'integer)
(deftype db::ty () 'string)
(chk "deftype.da.int"   (typep 5 'da::ty))
(chk "deftype.da.str"   (typep "x" 'da::ty))
(chk "deftype.db.int"   (typep 5 'db::ty))
(chk "deftype.db.str"   (typep "x" 'db::ty))

;;; ------------------------------------------------------------------
;;; 7. DEFSTRUCT table
;;; ------------------------------------------------------------------
;; The constructor/accessor names DEFSTRUCT builds are interned in *PACKAGE*,
;; not in the structure name's package, so name them explicitly -- otherwise
;; DA::ST and DB::ST would both try to define CL-USER::MAKE-ST.
(defstruct (da::st (:constructor da::make-st) (:conc-name "STA-")) (p 1))
(defstruct (db::st (:constructor db::make-st) (:conc-name "STB-")) (q 2))
(chk "struct.own-typep"   (typep (da::make-st) 'da::st))
(chk "struct.cross-typep" (typep (da::make-st) 'db::st))
(chk "struct.own-slot"    (sta-p (da::make-st)))
(chk "struct.other-accessor" (stb-q (da::make-st)))

;;; ------------------------------------------------------------------
;;; 8. SYMBOL-MACRO table
;;; ------------------------------------------------------------------
(define-symbol-macro da::sm 111)
(define-symbol-macro db::sm 222)
(chk "symbol-macro.da"  da::sm)
(chk "symbol-macro.db"  db::sm)

;;; ------------------------------------------------------------------
;;; 9. SETF-expander table
;;; ------------------------------------------------------------------
(defun da::acc (c) (car c))
(defun db::acc (c) (car c))
(defun (setf da::acc) (v c) (setf (car c) (list :DA v)))
(defun (setf db::acc) (v c) (setf (car c) (list :DB v)))
(chk "setf.da"          (let ((c (list nil))) (setf (da::acc c) 1) (car (car c))))
(chk "setf.db"          (let ((c (list nil))) (setf (db::acc c) 1) (car (car c))))

;;; ------------------------------------------------------------------
;;; 10. COMPILER-MACRO table
;;; ------------------------------------------------------------------
(defun da::cm (x) x)
(defun db::cm (x) x)
(define-compiler-macro da::cm (x) (list '+ x 10))
(define-compiler-macro db::cm (x) (list '+ x 20))
(chk "compiler-macro.da" (funcall (compiler-macro-function 'da::cm) '(da::cm 1) nil))
(chk "compiler-macro.db" (funcall (compiler-macro-function 'db::cm) '(db::cm 1) nil))

;;; ------------------------------------------------------------------
;;; 11. Special-variable / SYMBOL-VALUE table
;;; ------------------------------------------------------------------
(defparameter da::*v* :V-FROM-DA)
(defparameter db::*v* :V-FROM-DB)
(chk "special.da"       da::*v*)
(chk "special.db"       db::*v*)

;;; ------------------------------------------------------------------
;;; 12. CLOS method dispatch across same-named classes: a method
;;;     specialized on DA::CLS must not run for a DB::CLS instance.
;;; ------------------------------------------------------------------
(defgeneric da::which (x))
(defmethod  da::which ((x da::cls)) :DISPATCHED-DA)
(defmethod  da::which ((x db::cls)) :DISPATCHED-DB)
(chk "dispatch.da"      (da::which (make-instance 'da::cls)))
(chk "dispatch.db"      (da::which (make-instance 'db::cls)))

(format t "REGISTRY-DIFFERENTIAL-DONE~%")
