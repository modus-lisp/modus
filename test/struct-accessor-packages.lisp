;;;; test/struct-accessor-packages.lisp -- a DEFSTRUCT accessor is its SYMBOL's, not its NAME's.
;;;;   modus --script test/struct-accessor-packages.lisp   => STRUCT-ACCESSOR-PACKAGES PASS
;;;;
;;;; The compiler inlines a call to a struct accessor as the slot read (*STRUCT-ACCESSORS*, compiler.lisp),
;;;; and found the record by the NAME's hash: P1:THING-NAME, an ordinary function, compiled as P2's slot read
;;;; once P2 defined a struct THING, and signalled "#<HASH-TABLE> is not of type P2:THING".  Found as
;;;; operandi's SESSION-HISTORY (a GETHASH) against quill's (a SESSION slot).
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defpackage :sap-p1 (:use :cl) (:export #:thing-name #:set-thing-name))
(in-package :sap-p1)
(defun thing-name (h) (gethash :name h))
(defun set-thing-name (h v) (setf (gethash :name h) v))
(in-package :cl-user)
(defpackage :sap-p2 (:use :cl) (:export #:thing #:thing-name #:make-thing))
(in-package :sap-p2)
(defstruct thing name)
(in-package :cl-user)

(defvar *h* (let ((h (make-hash-table))) (setf (gethash :name h) "hash") h))
(defun call-p1 (h) (sap-p1:thing-name h))
(defun call-p2 (s) (sap-p2:thing-name s))
(defun call-p2-declared (s) (declare (type sap-p2:thing s)) (sap-p2:thing-name s))

(check "another package's function of the same name" (call-p1 *h*) "hash")
(check "at the toplevel too" (sap-p1:thing-name *h*) "hash")
(check "its setter-shaped neighbour" (progn (sap-p1:set-thing-name *h* "set") (call-p1 *h*)) "set")
(check "the accessor still reads its slot" (call-p2 (sap-p2:make-thing :name "struct")) "struct")
(check "and inlined on a declared argument" (call-p2-declared (sap-p2:make-thing :name "decl")) "decl")
(check "and still checks its argument"
       (handler-case (progn (call-p2 5) :no-error) (type-error () :type-error))
       :type-error)
(dotimes (i 200) (call-p1 *h*) (call-p2 (sap-p2:make-thing :name "x")))
(check "after the hot JIT" (list (call-p1 *h*) (call-p2 (sap-p2:make-thing :name "j"))) '("set" "j"))

(format t "~&STRUCT-ACCESSOR-PACKAGES ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
