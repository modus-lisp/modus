;;;; test/defvar-special.lisp -- DEFVAR, DEFPARAMETER and DECLAIM SPECIAL
;;;; proclaim a variable special, so a LET of it binds dynamically and a
;;;; function called inside the LET sees the binding (CLHS 3.1.2.1.1.2).
;;;;   modus --script test/defvar-special.lisp   => DEFVAR-SPECIAL PASS
;;;; The kernel's own globals are DEFVARed in the image's source: a user LET of
;;;; one used to bind it LEXICALLY (the kernel function below read NIL).
;;;; MODUS-INT::%CAB-ON (mvm/cl-fileio.lisp) is (defun %cab-on () *cab-call*).
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

;; 1. a script's own specials
(defvar *v* :global)
(defun read-v () *v*)
(check "DEFVAR: a callee sees the LET's binding" (let ((*v* :bound)) (read-v)) :bound)
(check "and the global comes back" (read-v) :global)
(defparameter *p* 1)
(defun read-p () *p*)
(check "DEFPARAMETER: a callee sees the binding" (let ((*p* 2)) (read-p)) 2)
(declaim (special *d*))
(setq *d* :global)
(defun read-d () *d*)
(check "DECLAIM SPECIAL: a callee sees the binding" (let ((*d* :bound)) (read-d)) :bound)
(check "LET*: a later init sees the earlier binding"
       (let* ((*v* :first) (x (read-v))) x) :first)
(check "a non-local exit restores it"
       (progn (catch 'out (let ((*v* :thrown)) (throw 'out nil))) (read-v)) :global)

;; 2. the kernel's globals
(check "a LET of a kernel global reaches the kernel function reading it"
       (let ((modus-int::*cab-call* 42)) (modus-int::%cab-on)) 42)
(check "and is undone" (modus-int::%cab-on) nil)

;; 3. a same-named variable of another package is another variable
(defpackage :defvar-special-user (:use :cl))
(in-package :defvar-special-user)
(defvar *cab-call* :mine)
(defun mine () *cab-call*)
(cl-user::check "another package's variable of the same name is bound alone"
                (let ((*cab-call* 7)) (list (mine) (modus-int::%cab-on))) '(7 nil))
(cl-user::check "and keeps its own value" (mine) :mine)
(in-package :cl-user)

(format t "~A~%" (if (zerop *fails*) "DEFVAR-SPECIAL PASS" "DEFVAR-SPECIAL FAIL"))
(unless (zerop *fails*) (modus-int::sys-exit 1))
