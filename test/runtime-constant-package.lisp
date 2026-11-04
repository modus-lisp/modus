;;;; test/runtime-constant-package.lisp -- a runtime DEFCONSTANT is one SYMBOL's,
;;;; not every symbol of that name.
;;;;
;;;;   ./modus --script test/runtime-constant-package.lisp
;;;;
;;;; %RUNTIME-FOLD-CONSTANT recorded a library's constant under its NAME hash, so
;;;; after cl-consensus's (defconstant +empty+ 0) every package's +EMPTY+ compiled
;;;; as 0 -- cl-payments' (defparameter +empty+ #()) included, which broke BOLT #8.
;;;; Fails before the fix ("other package sees its own value" got 0), passes after.

(defpackage :rcp-a (:use :cl))
(defpackage :rcp-b (:use :cl))
(defconstant rcp-a::+empty+ 0)
(defparameter rcp-b::+empty+ (make-array 0 :element-type '(unsigned-byte 8)))
(defun rcp-b::read-it () rcp-b::+empty+)
(defun rcp-a::read-it () rcp-a::+empty+)

(defvar *fails* 0)
(defun check (name got want)
  (if (equalp got want)
      (format t "ok    ~a~%" name)
      (progn (incf *fails*) (format t "FAIL  ~a: got ~s want ~s~%" name got want))))

(check "the constant's own package folds it" (rcp-a::read-it) 0)
(check "other package sees its own value" (rcp-b::read-it) #())
(check "other package's value is a vector" (vectorp (rcp-b::read-it)) t)

(format t "~&runtime-constant-package: ~a~%" (if (zerop *fails*) "PASS" "FAIL"))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
