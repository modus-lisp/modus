;;;; binding-differential.lisp — CROSS-IMPLEMENTATION battery for the two
;;;; places a BINDING FORM's options and declarations decide what gets bound
;;;; and under what name.
;;;;
;;;; PURPOSE
;;;;   Section A — DEFSTRUCT options (CLHS 3.4.6).  Modus's DEFSTRUCT parser
;;;;   long accepted only :CONC-NAME, :INCLUDE and :CONSTRUCTOR; every other
;;;;   standard option was read past in silence.  A silently-ignored option is
;;;;   worse than an unimplemented one — `(:predicate ptp)` compiled clean and
;;;;   then PTP was UNDEFINED-FUNCTION at the call site, and `(:type list)`
;;;;   compiled clean and then handed back a struct where the program wanted a
;;;;   list.  The rows below measure EVERY standard option, including the ones
;;;;   Modus does not implement, so the gap is a number rather than a memory.
;;;;
;;;;   Section B — a lambda/DEFUN parameter declared SPECIAL (CLHS 3.3.4 /
;;;;   3.4.1).  `(defun f (*v*) (declare (special *v*)) …)` must establish a
;;;;   DYNAMIC binding for the call's extent, so a callee that reads *v* sees
;;;;   the argument.  Modus gave the parameter a lexical slot only, so the
;;;;   callee read the global — a wrong ANSWER, not an error, which is the
;;;;   worst failure mode there is.  The binding must also be escape-safe:
;;;;   a THROW or ERROR out of the body must still restore the outer value
;;;;   (the LET-family half of this was task #240 / a311671).
;;;;
;;;; HOW TO USE
;;;;   ./probes/run-binding-differential.sh <modus-binary>
;;;;   Runs this same file under SBCL (the oracle) and under Modus, diffs.
;;;;     left  (<) = SBCL / ANSI-correct       right (>) = Modus
;;;;
;;;; HOW TO ADD A CASE
;;;;   One (chk "label" form) line.  Print only IMPLEMENTATION-NEUTRAL values:
;;;;   keywords, numbers, T/NIL, lists of those.  NEVER a struct instance, a
;;;;   function object or a printed #S(...) — the representations differ
;;;;   between implementations and would produce pure-noise diffs.  Use
;;;;   (and X t), (listp x), (typep x 'foo) to reduce to a neutral value.
;;;;
;;;; Every form is EVAL'd inside a handler-case, so a case that errors prints
;;;; :ERROR and the battery continues — a MISSING line is a FAILED case, never
;;;; a passing one.  The DONE marker at the bottom proves the file ran out.

(defmacro chk (label form)
  `(let ((v (handler-case ,form
              (error (c) (progn c :ERROR))
              (condition (c) (progn c :SIGNALLED)))))
     (format t "~a	~s~%" ,label v)))

;;; ==================================================================
;;; SECTION A — DEFSTRUCT OPTIONS
;;; ==================================================================

;;; --- A1. The DEFAULT names, with no options at all.  Control group: if
;;;         one of these moves, an option fix has overreached. -----------
(defstruct d0 a b)
(chk "ds.default.ctor"   (d0-a (make-d0 :a 1 :b 2)))
(chk "ds.default.pred"   (and (d0-p (make-d0)) t))
(chk "ds.default.pred-neg" (and (d0-p 5) t))
(chk "ds.default.copier" (d0-b (copy-d0 (make-d0 :a 1 :b 2))))

;;; --- A2. (:PREDICATE NAME) / (:COPIER NAME) — the reported gap.
;;;         CLHS: the given name REPLACES the default; the default name is
;;;         then NOT defined (checked with FBOUNDP below). ---------------
(defstruct (d1 (:constructor mk1 (x)) (:predicate d1p) (:copier cp1)) x)
(chk "ds.pred.named"        (and (d1p (mk1 1)) t))
(chk "ds.pred.named-neg"    (and (d1p 5) t))
(chk "ds.copier.named"      (d1-x (cp1 (mk1 7))))
(chk "ds.pred.default-gone" (and (fboundp 'd1-p) t))
(chk "ds.copier.default-gone" (and (fboundp 'copy-d1) t))

;;; --- A3. (:PREDICATE NIL) / (:COPIER NIL) — CLHS: generate NOTHING.
;;;         NIL is a STATE, not a name. --------------------------------
(defstruct (d2 (:predicate nil) (:copier nil)) x)
(chk "ds.pred.nil"   (and (fboundp 'd2-p) t))
(chk "ds.copier.nil" (and (fboundp 'copy-d2) t))

;;; --- A4. (:PREDICATE) / (:COPIER) with NO argument — the default name,
;;;         which is NOT the same as (:predicate nil).  Both have a NIL
;;;         CADR, so a parser that keys on CADR conflates them. --------
(defstruct (d3 (:predicate) (:copier)) x)
(chk "ds.pred.noarg"   (and (d3-p (make-d3)) t))
(chk "ds.copier.noarg" (d3-x (copy-d3 (make-d3 :x 6))))

;;; --- A5. Named predicate/copier ALONGSIDE :CONC-NAME and :INCLUDE ----
(defstruct (d4 (:conc-name d4zz-) (:predicate d4p) (:copier cp4)) x)
(chk "ds.conc+named.acc"    (d4zz-x (cp4 (make-d4 :x 8))))
(chk "ds.conc+named.pred"   (and (d4p (make-d4)) t))
(defstruct (d5 (:include d4) (:predicate d5p)) y)
(chk "ds.include.child-pred"      (and (d5p (make-d5)) t))
(chk "ds.include.parent-pred-yes" (and (d4p (make-d5)) t))
(chk "ds.include.child-pred-neg"  (and (d5p (make-d4)) t))
(chk "ds.include.inherited-slot"  (d4zz-x (make-d5 :x 3 :y 4)))

;;; --- A6. :CONC-NAME forms ------------------------------------------
(defstruct (d6 (:conc-name nil)) zz)
(chk "ds.conc.nil" (zz (make-d6 :zz 4)))
(defstruct (d7 (:conc-name "PRE-")) w)
(chk "ds.conc.string" (pre-w (make-d7 :w 5)))

;;; --- A7. :CONSTRUCTOR forms ----------------------------------------
(defstruct (d8 (:constructor mk8 (a b))) a b)
(chk "ds.ctor.boa"      (list (d8-a (mk8 1 2)) (d8-b (mk8 1 2))))
(defstruct (d9 (:constructor mk9)) a)
(chk "ds.ctor.keyword"  (d9-a (mk9 :a 9)))
(defstruct (d10 (:constructor mk10 (&optional (a 11) &key (b 12)))) a b)
(chk "ds.ctor.boa-opt"  (list (d10-a (mk10)) (d10-b (mk10))))
(chk "ds.ctor.boa-opt2" (list (d10-a (mk10 1 :b 2)) (d10-b (mk10 1 :b 2))))
(defstruct (d11 (:constructor mk11 nil)) (a 21))
(chk "ds.ctor.nil-arglist" (d11-a (mk11)))

;;; --- A8. Options Modus does NOT implement.  These rows are expected to
;;;         DIFFER; they exist so the gap is measured and so a future fix
;;;         gets credit automatically.  Values are reduced to neutral
;;;         predicates — never a printed instance.
;;;
;;;         EVERY form here goes through (chk … (eval '…)) rather than sitting
;;;         at toplevel.  Modus DELIBERATELY signals on (:TYPE …) — an honest
;;;         "not supported" rather than a silent wrong answer — and an
;;;         unhandled toplevel error ABORTS the whole --load, which would
;;;         truncate the transcript and silently delete Section B from the
;;;         measurement.  A truncated battery is a lie, so the risky forms
;;;         are evaluated inside the handler-case. -----------------------
(chk "ds.type.list.defstruct" (progn (eval '(defstruct (d12 (:type list)) x y)) :OK))
(chk "ds.type.list.is-list"   (and (listp (eval '(make-d12 :x 1 :y 2))) t))
(chk "ds.type.list.contents"  (let ((v (eval '(make-d12 :x 1 :y 2))))
                                (if (listp v) v :NOT-A-LIST)))
(chk "ds.type.list.no-pred"   (and (fboundp 'd12-p) t))
(chk "ds.type.vector.defstruct" (progn (eval '(defstruct (d13 (:type vector)) x)) :OK))
(chk "ds.type.vector.is-vector" (and (vectorp (eval '(make-d13 :x 1))) t))
(chk "ds.type.named.defstruct" (progn (eval '(defstruct (d14 (:type list) :named) x)) :OK))
(chk "ds.type.named.pred"      (and (fboundp 'd14-p) t))
(chk "ds.type.named.contents"  (let ((v (eval '(make-d14 :x 3))))
                                 (if (listp v) v :NOT-A-LIST)))
(chk "ds.initial-offset.defstruct"
     (progn (eval '(defstruct (d15 (:type vector) (:initial-offset 2) :named) x)) :OK))
(chk "ds.initial-offset.len" (let ((v (eval '(make-d15 :x 9))))
                               (if (vectorp v) (length v) :NOT-A-VECTOR)))
(chk "ds.print-function.defstruct"
     (progn (eval '(defun d16-printer (obj stream depth)
                     (declare (ignore depth))
                     (format stream "<D16 ~a>" (d16-x obj))))
            (eval '(defstruct (d16 (:print-function d16-printer)) (x 1)))
            :OK))
(chk "ds.print-function" (let ((s (with-output-to-string (o)
                                    (princ (eval '(make-d16)) o))))
                           (if (search "<D16" s) :CUSTOM :DEFAULT)))
(chk "ds.print-object.defstruct"
     (progn (eval '(defstruct (d17 (:print-object (lambda (obj stream)
                                                    (declare (ignore obj))
                                                    (write-string "<D17>" stream))))
                    (x 1)))
            :OK))
(chk "ds.print-object" (let ((s (with-output-to-string (o)
                                  (princ (eval '(make-d17)) o))))
                         (if (search "<D17" s) :CUSTOM :DEFAULT)))

;;; --- A9. SLOT options (CLHS 3.4.6 slot-description).  Modus parses a slot
;;;         spec as (NAME DEFAULT) and discards anything after, so :TYPE is
;;;         unchecked and :READ-ONLY is unenforced. ------------------------
(defstruct d18 (a 1 :type fixnum) (b 2 :read-only t))
(chk "ds.slot.defaults"      (list (d18-a (make-d18)) (d18-b (make-d18))))
(chk "ds.slot.read-only"     (let ((s (make-d18)))
                               (handler-case (progn (setf (d18-b s) 9) (d18-b s))
                                 (error (c) (progn c :REFUSED)))))

;;; ==================================================================
;;; SECTION B — SPECIAL DECLARATIONS ON PARAMETERS
;;; ==================================================================

(defvar *lv* :outer)
(defun peek () *lv*)

;;; --- B1. The reported gap: a required parameter declared special ----
(defun takes-req (*lv*) (declare (special *lv*)) (peek))
(chk "sp.req.callee"   (takes-req :INNER))
(chk "sp.req.restored" *lv*)

;;; --- B2. The body's OWN reference must be dynamic too, and a SETQ in
;;;         the body must be visible to a callee (it is the dynamic
;;;         binding that is assigned, not a lexical slot). -------------
(defun takes-self (*lv*) (declare (special *lv*)) *lv*)
(chk "sp.req.self" (takes-self :SELF))
(defun takes-setq (*lv*) (declare (special *lv*)) (setq *lv* :ASSIGNED) (peek))
(chk "sp.req.setq"          (takes-setq :INNER))
(chk "sp.req.setq-restored" *lv*)

;;; --- B3. ESCAPE SAFETY.  A non-local exit out of the body must still
;;;         restore the outer value (the a311671 property, now required
;;;         of the parameter path too). ------------------------------
(defun takes-throw (*lv*) (declare (special *lv*)) (throw 'esc (peek)))
(chk "sp.escape.throw.value"    (catch 'esc (takes-throw :THROWN)))
(chk "sp.escape.throw.restored" *lv*)
(defun takes-error (*lv*) (declare (special *lv*)) (error "boom"))
(chk "sp.escape.error.caught"   (handler-case (takes-error :ERRED)
                                  (error (c) (progn c :CAUGHT))))
(chk "sp.escape.error.restored" *lv*)
(defun takes-rf (*lv*) (declare (special *lv*)) (return-from takes-rf (peek)))
(chk "sp.escape.return-from"    (takes-rf :RF))
(chk "sp.escape.rf.restored"    *lv*)

;;; --- B4. Nesting: an inner call rebinds, and unwinding restores the
;;;         OUTER call's binding, not the global. --------------------
(defun takes-nest (*lv* depth)
  (declare (special *lv*))
  (if (> depth 0)
      (list (peek) (takes-nest (list :d depth) (1- depth)) (peek))
      (peek)))
(chk "sp.nested"          (takes-nest :N0 2))
(chk "sp.nested.restored" *lv*)

;;; --- B5. Every lambda-list section ---------------------------------
(defun takes-opt (&optional (*lv* :DEFAULTED)) (declare (special *lv*)) (peek))
(chk "sp.optional.supplied"   (takes-opt :OPT))
(chk "sp.optional.defaulted"  (takes-opt))
(chk "sp.optional.restored"   *lv*)
(defun takes-key (&key (*lv* :KDEFAULT)) (declare (special *lv*)) (peek))
;; NB the keyword derived from the parameter *LV* is :*LV* — a rename of the
;; parameter variable must NOT change the keyword the caller passes.
(chk "sp.key.supplied"  (takes-key :*lv* :KEYED))
(chk "sp.key.defaulted" (takes-key))
(chk "sp.key.restored"  *lv*)
(defun takes-rest (&rest *lv*) (declare (special *lv*)) (peek))
(chk "sp.rest"          (takes-rest 1 2 3))
(chk "sp.rest.restored" *lv*)
(defun takes-aux (x &aux (*lv* (list :aux x))) (declare (special *lv*)) (peek))
(chk "sp.aux"          (takes-aux 5))
(chk "sp.aux.restored" *lv*)

;;; --- B6. Two special parameters at once, and a mixed lexical/special
;;;         lambda list (the lexical one must stay lexical). ----------
(defvar *lw* :outer-w)
(defun peek2 () (list *lv* *lw*))
(defun takes-two (*lv* lex *lw*) (declare (special *lv* *lw*))
  (list (peek2) lex))
(chk "sp.two"          (takes-two :A :LEXICAL :B))
(chk "sp.two.restored" (peek2))

;;; --- B7. LAMBDA (not DEFUN) and FLET parameters --------------------
(chk "sp.lambda"          (funcall (lambda (*lv*) (declare (special *lv*)) (peek))
                                   :LAMBDA))
(chk "sp.lambda.restored" *lv*)
(chk "sp.flet"            (flet ((f (*lv*) (declare (special *lv*)) (peek)))
                            (f :FLET)))
(chk "sp.flet.restored"   *lv*)
(chk "sp.labels"          (labels ((g (*lv*) (declare (special *lv*)) (peek)))
                            (g :LABELS)))

;;; --- B8. FREE declaration (CLHS 3.3.4): the declared name is NOT a
;;;         parameter, so NO binding is established — the body's
;;;         references are dynamic and a SETQ must PERSIST after return.
(defvar *free* :free-outer)
(defun peek-free () *free*)
(defun uses-free (x) (declare (special *free*)) (list x (peek-free)))
(chk "sp.free.read" (uses-free 1))
(defun sets-free (x) (declare (special *free*)) (setq *free* x) :SET)
(chk "sp.free.setq"      (sets-free :FREE-ASSIGNED))
(chk "sp.free.persisted" *free*)

;;; --- B9. Regression guard for the LET family (a311671).  If a
;;;         parameter-path change reaches into compile-let-with-specials
;;;         these move. -------------------------------------------
(defvar *lz* :z-outer)
(defun peek-z () *lz*)
(chk "sp.let.callee"    (let ((*lz* :Z-LET)) (declare (special *lz*)) (peek-z)))
(chk "sp.let.restored"  *lz*)
(chk "sp.let.throw"     (catch 'ez (let ((*lz* :Z-T)) (declare (special *lz*))
                                     (throw 'ez (peek-z)))))
(chk "sp.let.throw.restored" *lz*)
(chk "sp.letstar"       (let* ((a 1) (*lz* (list :Z a)))
                          (declare (special *lz*)) (peek-z)))
(chk "sp.letstar.restored" *lz*)
(chk "sp.progv"         (progv (list '*lz*) (list :Z-PROGV) (peek-z)))
(chk "sp.progv.restored" *lz*)

;;; --- B10. Multiple values must survive the restore (the unwind-protect
;;;          cleanup must not eat them). ------------------------------
(defun takes-mv (*lv*) (declare (special *lv*)) (values (peek) 2 3 4 5))
(chk "sp.mv" (multiple-value-list (takes-mv :MV)))

(format t "BINDING-DIFFERENTIAL-DONE~%")
