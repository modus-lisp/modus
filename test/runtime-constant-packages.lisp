;;;; test/runtime-constant-packages.lisp -- a runtime DEFCONSTANT folds into ITS symbol's references only.
;;;;   modus --script test/runtime-constant-packages.lisp   => RUNTIME-CONSTANT-PACKAGES PASS
;;;;
;;;; %RUNTIME-FOLD-CONSTANT recorded a library's constant under its NAME's hash, so another package's
;;;; same-named variable compiled to the constant's value: glass's (DEFCONSTANT +WHITE+ #xFFFFFF) made
;;;; warren's (DEFPARAMETER +WHITE+ '(255 255 255)) read as #xFFFFFF, and every warren paint signalled
;;;; TYPE-ERROR on the phone.  Same shape as test/struct-accessor-packages.lisp.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defpackage :rcp-a (:use :cl))
(defpackage :rcp-b (:use :cl))
(in-package :rcp-a)
(defconstant +white+ 16777215)
(defun a-white () +white+)
(in-package :rcp-b)
(defparameter +white+ '(255 255 255))
(defun b-white () +white+)
(defconstant +both+ 7)
(defun b-both () +both+)
(in-package :rcp-a)
(defconstant +both+ 9)
(defun a-both () +both+)
(in-package :cl-user)

(check "the constant's own package reads it" (rcp-a::a-white) 16777215)
(check "another package's variable of the same name is its own" (rcp-b::b-white) '(255 255 255))
(check "two packages' constants of one name, the first" (rcp-b::b-both) 7)
(check "two packages' constants of one name, the second" (rcp-a::a-both) 9)
(dotimes (i 200) (rcp-a::a-white) (rcp-b::b-white))
(check "after the hot JIT" (list (rcp-a::a-white) (rcp-b::b-white)) '(16777215 (255 255 255)))

(format t "~&RUNTIME-CONSTANT-PACKAGES ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
