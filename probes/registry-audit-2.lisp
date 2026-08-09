;;; AUDIT round 2 — sharper probes for the registries round 1 left ambiguous,
;;; plus CONTROLS for the two registries already fixed (#211 fn table, e4d26a8
;;; macro table).
(defpackage "SA" (:use "CL"))
(defpackage "SB" (:use "CL"))

(defmacro try (form)
  `(handler-case ,form (t (c) (list :err (type-of c)))))

;;; ---- C1 CONTROL: function table (#211, expected CORRECT) ----
(format t "~%=== C1 function table (control, fixed in #211) ===~%")
(format t "c1-setup ~a ~a~%"
        (try (progn (eval '(defun sa::f1 () :sa)) :ok))
        (try (progn (eval '(defun sb::f1 () :sb)) :ok)))
(format t "c1a (sa::f1) -> ~a  [want :SA]~%" (try (eval '(sa::f1))))
(format t "c1b (sb::f1) -> ~a  [want :SB]~%" (try (eval '(sb::f1))))

;;; ---- C2 CONTROL: macro table (e4d26a8, expected CORRECT) ----
(format t "~%=== C2 macro table (control, fixed in e4d26a8) ===~%")
(format t "c2-setup ~a ~a~%"
        (try (progn (eval '(defmacro sa::m1 (x) (list '+ x 100))) :ok))
        (try (progn (eval '(defmacro sb::m1 (x) (list '+ x 200))) :ok)))
(format t "c2a (sa::m1 1) -> ~a  [want 101]~%" (try (eval '(sa::m1 1))))
(format t "c2b (sb::m1 1) -> ~a  [want 201]~%" (try (eval '(sb::m1 1))))

;;; ---- S2 DEFSTRUCT: registry key vs typep walk ----
(format t "~%=== S2 defstruct (registry vs typep) ===~%")
(format t "s2-setup ~a ~a~%"
        (try (progn (eval '(defstruct sa::st aa)) :ok))
        (try (progn (eval '(defstruct sb::st bb cc)) :ok)))
(format t "s2a hash-a=~a hash-b=~a  [want DIFFERENT]~%"
        (try (%struct-name-hash 'sa::st)) (try (%struct-name-hash 'sb::st)))
(format t "s2b sa::st slots -> ~a  [want (AA)]~%"
        (try (%struct-type-desc-slots (%find-struct-type 'sa::st))))
(format t "s2c sb::st slots -> ~a  [want (BB CC)]~%"
        (try (%struct-type-desc-slots (%find-struct-type 'sb::st))))
(format t "s2d (%struct-instance-typep (sa::make-st) 'sb::st) -> ~a  [want NIL]~%"
        (try (eval '(%struct-instance-typep (sa::make-st) 'sb::st))))
(format t "s2e (typep (sa::make-st) 'sb::st) -> ~a  [want NIL]~%"
        (try (eval '(typep (sa::make-st) 'sb::st))))

;;; ---- S5 SETF expander through the COMPILER (compiler.lisp *setf-expanders*) ----
(format t "~%=== S5 defsetf through the compiler ===~%")
(format t "s5-setup ~a~%"
        (try (progn
               (eval '(defvar sa::*log* nil))
               (eval '(defun sa::seta (o v) (setq sa::*log* (cons :sa sa::*log*)) v))
               (eval '(defun sb::seta (o v) (setq sa::*log* (cons :sb sa::*log*)) v))
               (eval '(defun sa::acc (o) o))
               (eval '(defun sb::acc (o) o))
               (eval '(defsetf sa::acc sa::seta))
               (eval '(defsetf sb::acc sb::seta))
               :ok)))
(format t "s5a (setf (sa::acc x) 1) log -> ~a  [want (:SA)]~%"
        (try (progn (eval '(setq sa::*log* nil))
                    (eval '(let ((x 5)) (setf (sa::acc x) 1)))
                    (eval 'sa::*log*))))
(format t "s5b (setf (sb::acc x) 1) log -> ~a  [want (:SB)]~%"
        (try (progn (eval '(setq sa::*log* nil))
                    (eval '(let ((x 5)) (setf (sb::acc x) 1)))
                    (eval 'sa::*log*))))

;;; ---- S7 CONDITION types, sharper ----
(format t "~%=== S7 define-condition (typep/subtypep) ===~%")
(format t "s7-setup ~a ~a~%"
        (try (progn (eval '(define-condition sa::ce (error) ())) :ok))
        (try (progn (eval '(define-condition sb::ce (error) ())) :ok)))
(format t "s7a (typep (make-condition 'sa::ce) 'sa::ce) -> ~a  [want T]~%"
        (try (eval '(typep (make-condition 'sa::ce) 'sa::ce))))
(format t "s7b (typep (make-condition 'sa::ce) 'sb::ce) -> ~a  [want NIL]~%"
        (try (eval '(typep (make-condition 'sa::ce) 'sb::ce))))
(format t "s7c (subtypep 'sa::ce 'sb::ce) -> ~a  [want NIL]~%"
        (try (eval '(subtypep 'sa::ce 'sb::ce))))
(format t "s7d handler-case error sa::ce with sa::ce -> ~a  [want :A]~%"
        (try (eval '(handler-case (error 'sa::ce) (sa::ce (c) :a) (t (c) :other)))))
(format t "s7e handler-case error sa::ce with sb::ce -> ~a  [want :OTHER]~%"
        (try (eval '(handler-case (error 'sa::ce) (sb::ce (c) :b) (t (c) :other)))))

;;; ---- S10 slot accessors / with-slots across packages ----
(format t "~%=== S10 CLOS slot readers across packages ===~%")
(format t "s10-setup ~a ~a~%"
        (try (progn (eval '(defclass sa::k () ((v :initform :sa-slot :reader sa::getv)))) :ok))
        (try (progn (eval '(defclass sb::k () ((v :initform :sb-slot :reader sb::getv)))) :ok)))
(format t "s10a (sa::getv (make-instance 'sa::k)) -> ~a  [want :SA-SLOT]~%"
        (try (eval '(sa::getv (make-instance 'sa::k)))))
(format t "s10b (sb::getv (make-instance 'sb::k)) -> ~a  [want :SB-SLOT]~%"
        (try (eval '(sb::getv (make-instance 'sb::k)))))

(format t "~%PROBE-AUDIT2-DONE~%")
