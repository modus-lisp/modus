;;; AUDIT: which name-keyed registries are package-blind?
;;; Every check registers the SAME name in two packages with DIFFERENT
;;; contents, then asks package A for its own.
(defpackage "RA" (:use "CL"))
(defpackage "RB" (:use "CL"))

(defmacro try (form)
  `(handler-case ,form (t (c) (list :err (type-of c)))))

;;; ---------------- R1  DEFTYPE registry ----------------
(format t "~%=== R1 deftype ===~%")
(format t "r1-setup ~a ~a~%"
        (try (eval '(deftype ra::tt () 'integer)))
        (try (eval '(deftype rb::tt () 'string))))
(format t "r1a (typep 5 'ra::tt)     -> ~a  [want T]~%"   (try (eval '(typep 5 'ra::tt))))
(format t "r1b (typep \"x\" 'ra::tt)   -> ~a  [want NIL]~%" (try (eval '(typep "x" 'ra::tt))))
(format t "r1c (typep 5 'rb::tt)     -> ~a  [want NIL]~%" (try (eval '(typep 5 'rb::tt))))
(format t "r1d (typep \"x\" 'rb::tt)   -> ~a  [want T]~%"   (try (eval '(typep "x" 'rb::tt))))

;;; ---------------- R2  DEFSTRUCT registry ----------------
(format t "~%=== R2 defstruct ===~%")
(format t "r2-setup ~a ~a~%"
        (try (progn (eval '(defstruct ra::sss aa)) :ok))
        (try (progn (eval '(defstruct rb::sss bb cc)) :ok)))
(format t "r2a ra::sss slots -> ~a  [want (AA)]~%"
        (try (%struct-type-desc-slots (%find-struct-type 'ra::sss))))
(format t "r2b rb::sss slots -> ~a  [want (BB CC)]~%"
        (try (%struct-type-desc-slots (%find-struct-type 'rb::sss))))
(format t "r2c (typep (ra::make-sss) 'rb::sss) -> ~a  [want NIL]~%"
        (try (eval '(typep (ra::make-sss) 'rb::sss))))

;;; ---------------- R3  COMPILER-MACRO registry ----------------
(format t "~%=== R3 compiler-macro ===~%")
(format t "r3-setup ~a ~a~%"
        (try (progn (eval '(define-compiler-macro ra::cm (x) (list '+ x 10))) :ok))
        (try (progn (eval '(define-compiler-macro rb::cm (x) (list '+ x 20))) :ok)))
(format t "r3a expand ra::cm -> ~a  [want (+ 1 10)]~%"
        (try (funcall (compiler-macro-function 'ra::cm) '(ra::cm 1) nil)))
(format t "r3b expand rb::cm -> ~a  [want (+ 1 20)]~%"
        (try (funcall (compiler-macro-function 'rb::cm) '(rb::cm 1) nil)))

;;; ---------------- R4  METHOD-COMBINATION registry ----------------
(format t "~%=== R4 method-combination ===~%")
(format t "r4-setup ~a ~a~%"
        (try (progn (eval '(define-method-combination ra::mmc :identity-with-one-argument nil)) :ok))
        (try (progn (eval '(define-method-combination rb::mmc :identity-with-one-argument t)) :ok)))
(format t "r4a ra::mmc iwo -> ~a  [want NIL]~%"
        (try (%mc-identity-with-one (%find-mc 'ra::mmc))))
(format t "r4b rb::mmc iwo -> ~a  [want T]~%"
        (try (%mc-identity-with-one (%find-mc 'rb::mmc))))
(format t "r4c (eq mc-a mc-b) -> ~a  [want NIL]~%"
        (try (eq (%find-mc 'ra::mmc) (%find-mc 'rb::mmc))))

;;; ---------------- R5  RUNTIME SETF-EXPANDER registry ----------------
(format t "~%=== R5 defsetf / setf-expander ===~%")
(format t "r5-setup ~a ~a~%"
        (try (progn (eval '(defsetf ra::acc ra::set-acc)) :ok))
        (try (progn (eval '(defsetf rb::acc rb::set-acc)) :ok)))
(format t "r5a ra::acc expander -> ~a  [want (:SHORT . RA::SET-ACC)]~%"
        (try (%find-setf-expander 'ra::acc)))
(format t "r5b rb::acc expander -> ~a  [want (:SHORT . RB::SET-ACC)]~%"
        (try (%find-setf-expander 'rb::acc)))
(format t "r5c setf ra::acc expands to -> ~a~%"
        (try (macroexpand-1 '(setf (ra::acc x) 9))))
(format t "r5d setf rb::acc expands to -> ~a~%"
        (try (macroexpand-1 '(setf (rb::acc x) 9))))

;;; ---------------- R6  CLOS CLASS registry ----------------
(format t "~%=== R6 defclass ===~%")
(format t "r6-setup ~a ~a~%"
        (try (progn (eval '(defclass ra::cc () ((aa :initarg :aa :initform 1)))) :ok))
        (try (progn (eval '(defclass rb::cc () ((bb :initarg :bb :initform 2)))) :ok)))
(format t "r6a (slot-exists-p (make-instance 'ra::cc) 'aa) -> ~a  [want T]~%"
        (try (eval '(slot-exists-p (make-instance 'ra::cc) 'ra::aa))))
(format t "r6b (slot-exists-p (make-instance 'ra::cc) 'bb) -> ~a  [want NIL]~%"
        (try (eval '(slot-exists-p (make-instance 'ra::cc) 'rb::bb))))
(format t "r6c (typep (make-instance 'ra::cc) 'rb::cc) -> ~a  [want NIL]~%"
        (try (eval '(typep (make-instance 'ra::cc) 'rb::cc))))
(format t "r6d (eq (find-class 'ra::cc) (find-class 'rb::cc)) -> ~a  [want NIL]~%"
        (try (eq (%find-clos-class 'ra::cc) (%find-clos-class 'rb::cc))))

;;; ---------------- R7  CONDITION types ----------------
(format t "~%=== R7 define-condition ===~%")
(format t "r7-setup ~a ~a~%"
        (try (progn (eval '(define-condition ra::ce (error) ())) :ok))
        (try (progn (eval '(define-condition rb::ce (error) ())) :ok)))
(format t "r7a signal ra::ce caught by rb::ce handler -> ~a  [want :NOT-CAUGHT]~%"
        (try (eval '(handler-case (progn (signal (make-condition 'ra::ce)) :not-caught)
                      (rb::ce (c) :WRONGLY-CAUGHT)))))
(format t "r7b signal ra::ce caught by ra::ce handler -> ~a  [want :CAUGHT]~%"
        (try (eval '(handler-case (progn (signal (make-condition 'ra::ce)) :not-caught)
                      (ra::ce (c) :CAUGHT)))))

;;; ---------------- R8  GF registry (the fix) ----------------
(format t "~%=== R8 generic functions ===~%")
(format t "r8-setup ~a ~a~%"
        (try (progn (eval '(defgeneric ra::gg (a b c d)))
                    (eval '(defmethod ra::gg ((a t) (b t) (c t) (d t)) :ra)) :ok))
        (try (progn (eval '(defgeneric rb::gg (a)))
                    (eval '(defmethod rb::gg ((a t)) :rb)) :ok)))
(format t "r8a (ra::gg 1 2 3 4) -> ~a  [want :RA]~%" (try (eval '(ra::gg 1 2 3 4))))
(format t "r8b (rb::gg 1)       -> ~a  [want :RB]~%" (try (eval '(rb::gg 1))))
(format t "r8c (eq gf-a gf-b)   -> ~a  [want NIL]~%"
        (try (eq (%find-gf 'ra::gg) (%find-gf 'rb::gg))))

;;; ---------------- R9  SYMBOL-MACRO registry ----------------
(format t "~%=== R9 define-symbol-macro ===~%")
(format t "r9-setup ~a ~a~%"
        (try (progn (eval '(define-symbol-macro ra::sm 111)) :ok))
        (try (progn (eval '(define-symbol-macro rb::sm 222)) :ok)))
(format t "r9a ra::sm -> ~a  [want 111]~%" (try (eval 'ra::sm)))
(format t "r9b rb::sm -> ~a  [want 222]~%" (try (eval 'rb::sm)))

(format t "~%PROBE-AUDIT-DONE~%")
