;;;; initarg-differential.lisp — CROSS-IMPLEMENTATION initarg-validity battery.
;;;;
;;;; PURPOSE
;;;;   CLHS 7.1.2 says the initargs MAKE-INSTANCE accepts are the UNION of
;;;;     (a) the :INITARG names of the class's slots (walking the CPL),
;;;;     (b) the &KEY parameter names of every APPLICABLE method on
;;;;         INITIALIZE-INSTANCE / SHARED-INITIALIZE / ALLOCATE-INSTANCE,
;;;;     (c) :ALLOW-OTHER-KEYS,
;;;;   and that an applicable method with &ALLOW-OTHER-KEYS makes every
;;;;   keyword valid.  CLHS 7.3 says REINITIALIZE-INSTANCE validates the same
;;;;   way but against REINITIALIZE-INSTANCE / SHARED-INITIALIZE methods, and
;;;;   CLHS 7.2.2 says CHANGE-CLASS validates against
;;;;   UPDATE-INSTANCE-FOR-DIFFERENT-CLASS / SHARED-INITIALIZE — notably NOT
;;;;   INITIALIZE-INSTANCE.
;;;;
;;;;   This battery is deliberately TWO-DIRECTIONAL.  A validator that
;;;;   accepts everything passes every "valid initarg accepted" case and is a
;;;;   conformance REGRESSION; a validator that consults only slot initargs
;;;;   passes every "invalid initarg rejected" case and rejects code the
;;;;   standard says is legal (babel, asdf and friends all pass configuration
;;;;   to an :AFTER method with no backing slot).  So every "accepted" row is
;;;;   paired with an OVERREACH row that must still be rejected.
;;;;
;;;; HOW TO USE
;;;;   ./probes/run-initarg-differential.sh <modus-binary>
;;;;   Runs this file under SBCL (the ORACLE) and under Modus and diffs the
;;;;   two transcripts.  Left (<) = SBCL/ANSI, right (>) = Modus.
;;;;
;;;; HOW TO ADD A CASE
;;;;   One (chk "label" form) line.  Portable ANSI CL only (the oracle has to
;;;;   run it), and the form must return a SMALL PRINTABLE value — a keyword,
;;;;   boolean or integer.  Never an instance or class object: their printed
;;;;   representations legitimately differ between implementations.
;;;;
;;;;   :ERROR is a real expected value here, not a failure — most rows exist
;;;;   precisely to check that something IS rejected.
;;;;
;;;; Every form is EVAL'd inside a handler-case, so a case that errors prints
;;;; :ERROR and the battery keeps going.  A MISSING line is a FAILED form,
;;;; not a negative result — hence the DONE marker at the bottom.

(defmacro chk (label form)
  "Print LABEL<TAB>value-of-FORM.  Never lets FORM abort the battery."
  `(let ((v (handler-case ,form
              (error (c) (progn c :ERROR))
              (condition (c) (progn c :SIGNALLED)))))
     (format t "~a	~s~%" ,label v)))

;;; ------------------------------------------------------------------
;;; 0. Slot-initarg term (CLHS 7.1.2a) — the part every implementation,
;;;    including a slot-initargs-only validator, gets right.  Present so a
;;;    change that breaks the BASELINE is visible immediately.
;;; ------------------------------------------------------------------
(defclass ia-base () ((s1 :initarg :s1 :initform :none)))
(defclass ia-sub (ia-base) ((s2 :initarg :s2 :initform :none)))

(chk "slot.own"            (slot-value (make-instance 'ia-base :s1 7) 's1))
(chk "slot.inherited"      (slot-value (make-instance 'ia-sub :s1 7) 's1))
(chk "slot.sub-own"        (slot-value (make-instance 'ia-sub :s2 8) 's2))
(chk "slot.none"           (slot-value (make-instance 'ia-base) 's1))
;; A name no slot declares and no method names → PROGRAM-ERROR.
(chk "slot.bogus-rejected" (progn (make-instance 'ia-base :nosuch 1) :ACCEPTED))
;; :ALLOW-OTHER-KEYS at the CALL SITE suppresses the check (CLHS 7.1.2c).
(chk "callsite.aok-t"      (progn (make-instance 'ia-base :nosuch 1 :allow-other-keys t) :ACCEPTED))
(chk "callsite.aok-nil"    (progn (make-instance 'ia-base :nosuch 1 :allow-other-keys nil) :ACCEPTED))

;;; ------------------------------------------------------------------
;;; 1. INITIALIZE-INSTANCE &KEY term (CLHS 7.1.2b).  This is the term that
;;;    was inert for COMPILED methods before the build-time DEFMETHOD
;;;    expansion recorded method metadata.
;;; ------------------------------------------------------------------
(defclass ia-ikey () ((got :initform :none)))
(defmethod initialize-instance :after ((x ia-ikey) &key ik1)
  (setf (slot-value x 'got) ik1))

(chk "ii-key.accepted"     (slot-value (make-instance 'ia-ikey :ik1 11) 'got))
;; OVERREACH: a DIFFERENT name is still invalid for the same class.
(chk "ii-key.other-name"   (progn (make-instance 'ia-ikey :ik2 1) :ACCEPTED))
;; OVERREACH: the name is valid only for classes the method applies to.
(chk "ii-key.other-class"  (progn (make-instance 'ia-base :ik1 1) :ACCEPTED))

;;; Inheritance: a method on the SUPERCLASS is applicable to the subclass,
;;; so its keys are valid there; the reverse is not true.
(defclass ia-sup () ())
(defclass ia-inf (ia-sup) ())
(defmethod initialize-instance :after ((x ia-sup) &key supk) (progn supk nil))
(defmethod initialize-instance :after ((x ia-inf) &key infk) (progn infk nil))

(chk "ii-key.super-to-sub" (progn (make-instance 'ia-inf :supk 1) :ACCEPTED))
(chk "ii-key.sub-not-super" (progn (make-instance 'ia-sup :infk 1) :ACCEPTED))

;;; Keyword-name form ((:kw var)) exposes the KEYWORD, not the variable.
(defclass ia-kwform () ())
(defmethod initialize-instance :after ((x ia-kwform) &key ((:exposed hidden) nil))
  (progn hidden nil))
(chk "ii-key.kwform-exposed" (progn (make-instance 'ia-kwform :exposed 1) :ACCEPTED))
(chk "ii-key.kwform-hidden"  (progn (make-instance 'ia-kwform :hidden 1) :ACCEPTED))

;;; ------------------------------------------------------------------
;;; 2. SHARED-INITIALIZE &KEY term (CLHS 7.1.2b).
;;; ------------------------------------------------------------------
(defclass ia-skey () ((got :initform :none)))
(defmethod shared-initialize :after ((x ia-skey) slot-names &key sk1)
  (progn slot-names (setf (slot-value x 'got) sk1)))

(chk "si-key.accepted"     (slot-value (make-instance 'ia-skey :sk1 22) 'got))
(chk "si-key.other-name"   (progn (make-instance 'ia-skey :sk2 1) :ACCEPTED))
(chk "si-key.other-class"  (progn (make-instance 'ia-base :sk1 1) :ACCEPTED))

;;; ------------------------------------------------------------------
;;; 3. &ALLOW-OTHER-KEYS on an applicable method (CLHS 7.1.2) — makes EVERY
;;;    keyword a valid initarg for that class.  The asdf plan-traversal shape.
;;; ------------------------------------------------------------------
(defclass ia-aok () ())
(defmethod initialize-instance :after ((x ia-aok) &key aokk &allow-other-keys)
  (progn aokk nil))

(chk "aok.anything"        (progn (make-instance 'ia-aok :utterly-unknown 1) :ACCEPTED))
;; OVERREACH: one class's &allow-other-keys method must not make OTHER
;; classes lenient.
(chk "aok.other-class"     (progn (make-instance 'ia-base :utterly-unknown 1) :ACCEPTED))

;;; ------------------------------------------------------------------
;;; 4. REINITIALIZE-INSTANCE (CLHS 7.3): slot initargs + the &KEY names of
;;;    applicable REINITIALIZE-INSTANCE / SHARED-INITIALIZE methods.
;;;    INITIALIZE-INSTANCE keys are NOT in this union.
;;; ------------------------------------------------------------------
(defclass ia-re () ((s1 :initarg :s1 :initform :none) (got :initform :none)))
(defmethod reinitialize-instance :after ((x ia-re) &key rk1)
  (setf (slot-value x 'got) rk1))

(chk "reinit.slot"         (let ((i (make-instance 'ia-re)))
                             (reinitialize-instance i :s1 5)
                             (slot-value i 's1)))
(chk "reinit.method-key"   (let ((i (make-instance 'ia-re)))
                             (reinitialize-instance i :rk1 6)
                             (slot-value i 'got)))
(chk "reinit.bogus"        (let ((i (make-instance 'ia-re)))
                             (reinitialize-instance i :nosuch 1)
                             :ACCEPTED))
(chk "reinit.aok-callsite" (let ((i (make-instance 'ia-re)))
                             (reinitialize-instance i :nosuch 1 :allow-other-keys t)
                             :ACCEPTED))
;; An INITIALIZE-INSTANCE-only key is NOT valid for reinitialize-instance.
(chk "reinit.ii-key-not-valid"
     (let ((i (make-instance 'ia-ikey :ik1 1)))
       (reinitialize-instance i :ik1 2)
       :ACCEPTED))
;; A SHARED-INITIALIZE key IS valid for reinitialize-instance.
(chk "reinit.si-key-valid"
     (let ((i (make-instance 'ia-skey :sk1 1)))
       (reinitialize-instance i :sk1 2)
       (slot-value i 'got)))

;;; ------------------------------------------------------------------
;;; 5. CHANGE-CLASS (CLHS 7.2.2): slot initargs of the NEW class + the &KEY
;;;    names of applicable UPDATE-INSTANCE-FOR-DIFFERENT-CLASS /
;;;    SHARED-INITIALIZE methods.  NOT initialize-instance.
;;; ------------------------------------------------------------------
(defclass cc-a () ((x :initarg :x :initform 1)))
(defclass cc-b () ((y :initarg :y :initform 2) (got :initform :none)))
(defmethod update-instance-for-different-class :after ((old cc-a) (new cc-b) &key uk1)
  (setf (slot-value new 'got) uk1))
;; An INITIALIZE-INSTANCE key on the NEW class — must NOT become valid.
(defmethod initialize-instance :after ((n cc-b) &key ccik) (progn ccik nil))

(chk "change.slot"         (let ((i (make-instance 'cc-a)))
                             (change-class i 'cc-b :y 3)
                             (slot-value i 'y)))
(chk "change.uifdc-key"    (let ((i (make-instance 'cc-a)))
                             (change-class i 'cc-b :uk1 4)
                             (slot-value i 'got)))
(chk "change.bogus"        (let ((i (make-instance 'cc-a)))
                             (change-class i 'cc-b :nosuch 1)
                             :ACCEPTED))
(chk "change.ii-key-not-valid"
     (let ((i (make-instance 'cc-a)))
       (change-class i 'cc-b :ccik 1)
       :ACCEPTED))
(chk "change.aok-callsite" (let ((i (make-instance 'cc-a)))
                             (change-class i 'cc-b :nosuch 1 :allow-other-keys t)
                             :ACCEPTED))

;;; ------------------------------------------------------------------
;;; 6. CLHS 7.6.5 — the SAME metadata drives call-time keyword validity for
;;;    an ordinary generic function: a keyword named by NO applicable method
;;;    and not by the GF is a PROGRAM-ERROR, but one named by ANY applicable
;;;    method is accepted by ALL of them.
;;; ------------------------------------------------------------------
(defgeneric ia-gf (x &key))
(defmethod ia-gf ((x integer) &key gk1) (progn gk1 :INT))
(defmethod ia-gf ((x number) &key gk2) (progn gk2 :NUM))

(chk "gf.own-key"          (ia-gf 1 :gk1 1))
;; :gk2 is named by the applicable NUMBER method, so the INTEGER method must
;; tolerate it too (7.6.5) — accepted, and the INTEGER method still runs.
(chk "gf.sibling-key"      (ia-gf 1 :gk2 1))
(chk "gf.unknown-key"      (ia-gf 1 :nosuch 1))
;; A method applicable to a DIFFERENT argument contributes nothing.
(chk "gf.other-arg-key"    (ia-gf 1.5 :gk1 1))
(chk "gf.aok-callsite"     (ia-gf 1 :nosuch 1 :allow-other-keys t))

(format t "INITARG-DIFFERENTIAL-DONE~%")
