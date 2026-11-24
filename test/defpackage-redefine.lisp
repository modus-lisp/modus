;;;; defpackage-redefine.lisp -- DEFPACKAGE of an existing package updates it IN PLACE
;;;; (CLHS 11.1.1.1): its symbols, functions, nicknames and use list are kept, new ones added.
;;;;   modus --script test/defpackage-redefine.lisp      (sbcl --script: the same answers)
;;;; Before: modus deleted the package and made a new one, so every symbol interned before
;;;; was lost -- reloading a system lost its functions, and a build running on modus that
;;;; evaluated (DEFPACKAGE :MODUS-INT ...) deleted the running kernel's own package.
(defvar *fail* 0)
(defmacro check (name form) `(unless ,form (incf *fail*) (format t "~&FAIL: ~a~%" ,name)))

(defpackage :dpr-a (:use :cl) (:export #:alpha))
(in-package :dpr-a)
(defun alpha () 1)
(defun beta () 2)
(defvar *v* :kept)
(in-package :cl-user)
(defvar *before* (find-package "DPR-A"))
(defvar *alpha* (find-symbol "ALPHA" "DPR-A"))

(defpackage :dpr-a (:use :cl) (:nicknames :dpr-nick) (:export #:alpha #:gamma))

(check "the same package object" (eq (find-package "DPR-A") *before*))
(check "the same symbol" (eq (find-symbol "ALPHA" "DPR-A") *alpha*))
(check "an exported function still runs" (eql (funcall (find-symbol "ALPHA" "DPR-A")) 1))
(check "an internal function still runs" (eql (funcall (find-symbol "BETA" "DPR-A")) 2))
(check "a variable keeps its value" (eq (symbol-value (find-symbol "*V*" "DPR-A")) :kept))
(check "a new export" (eq (nth-value 1 (find-symbol "GAMMA" "DPR-A")) :external))
(check "a new nickname" (eq (find-package "DPR-NICK") *before*))

;; A package another one uses: redefining it must not cut the user off.
(defpackage :dpr-b (:use :cl :dpr-a))
(defpackage :dpr-a (:use :cl) (:export #:alpha #:gamma))
(check "a user of it still sees its exports" (eq (find-symbol "ALPHA" "DPR-B") *alpha*))

(format t "~&defpackage-redefine: ~:[PASS~;FAIL (~:*~a)~]~%" (and (plusp *fail*) *fail*))
#+modus (sb-ext:exit :code (if (zerop *fail*) 0 1) :abort t)
