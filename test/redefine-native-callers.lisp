;;;; test/redefine-native-callers.lisp -- a redefinition reaches callers that are
;;;; already NATIVE.
;;;;
;;;;   ./modus --script test/redefine-native-callers.lisp     -> REDEFINE PASS / FAIL
;;;;
;;;; After JIT-EAGER (every saved core) OUTER calls INNER through a linkage cell.
;;;; The cell used to move only when INNER got native code, so an interpreted
;;;; DEFUN, a (SETF FDEFINITION) of a closure and a (SETF SYMBOL-FUNCTION) were
;;;; all invisible to OUTER: it kept calling the old INNER.  cl-fips's negative
;;;; controls (swap in a broken function, require checks to fail) all "passed".
;;;; aarch64 only: x64's JIT bakes the callee address into the caller (no linkage
;;;; cells, see "LINKAGE CELL" in mvm-eval.lisp), so x64 FAILS this today --
;;;; measured, 2026-10-10.  Porting the cells is the fix there.

(defpackage :rnc-lib (:use :cl))
(in-package :rnc-lib)
(defun inner (x) (+ x 1))
(defun outer (x) (inner x))
(in-package :cl-user)

(defvar *rnc-fail* 0)
(defun rnc-check (what got want)
  (format t "~&  ~a ~a: ~s (want ~s)~%" (if (eql got want) "ok  " "FAIL") what got want)
  (unless (eql got want) (incf *rnc-fail*)))

(ignore-errors (jit-eager))
(rnc-check "native outer, original inner" (rnc-lib::outer 1) 2)
(let ((orig (fdefinition 'rnc-lib::inner)))
  (setf (fdefinition 'rnc-lib::inner) (lambda (x) (* x 100)))
  (rnc-check "after (setf fdefinition) of a closure" (rnc-lib::outer 1) 100)
  (setf (fdefinition 'rnc-lib::inner) orig)
  (rnc-check "after restoring the original" (rnc-lib::outer 1) 2))
(setf (symbol-function 'rnc-lib::inner) (lambda (x) (* x 7)))
(rnc-check "after (setf symbol-function)" (rnc-lib::outer 1) 7)
(defun rnc-lib::inner (x) (* x 9))
(rnc-check "after an interpreted DEFUN" (rnc-lib::outer 1) 9)
(format t "~&REDEFINE ~a~%" (if (zerop *rnc-fail*) "PASS" "FAIL"))
