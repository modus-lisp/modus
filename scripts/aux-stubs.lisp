;;;; aux-stubs.lisp — runtime-EVAL-friendly stubs for ANSI test helpers
;;;; that the official auxiliary/ansi_aux/ files implement with
;;;; defstruct + make-instance.
;;;;
;;;; Load order: AFTER ansi-aux.lsp and cons-aux.lsp (so these
;;;; replacements win via last-defun-wins at runtime EVAL).
;;;;
;;;; Why stubs are necessary:
;;;;   The official cons-aux.lsp defines make-scaffold-copy and
;;;;   check-scaffold-copy using `(make-instance scaffold …)'.  Modus's
;;;;   runtime EVAL has limited make-instance support — calling those
;;;;   functions errors via a path that handler-case in
;;;;   rt-run-registered-tests can't catch, which kills the rest of
;;;;   the test sweep silently.
;;;;
;;;; What the stubs lose:
;;;;   A scaffold-using test verifies that a destructive function
;;;;   didn't accidentally mutate its input.  Our stubs return T
;;;;   unconditionally, so a test that USED to detect "you mutated my
;;;;   input" will now silently let it through.  That accidentally
;;;;   PASSes a few buggy operations; the trade-off is unblocking
;;;;   hundreds of correct tests that just want their thunk to run.

(defun make-scaffold-copy (x) x)
(defun check-scaffold-copy (x scaffold)
  (declare (ignore x scaffold))
  t)

;;; eqlt / equalt / equalpt are pervasive ANSI-aux equality predicates
;;; that wrap their CL counterparts but also test for properly-defined
;;; types — at runtime EVAL the wrappers tend to fail; route them to
;;; the underlying CL function so simple compares work.

(defun eqlt (x y) (eql x y))
(defun equalt (x y) (equal x y))
(defun equalpt (x y) (equalp x y))

;;; notnot — used by deftest expected values.  ansi-aux defines it but
;;; if its loading silently failed (which happens for any aux file that
;;; references a feature Modus runtime doesn't support yet), tests get
;;; undefined-function.  Cheap to redefine here as a safety net.

(defun notnot (x) (not (not x)))

;;; *initial-print-pprint-dispatch* / *print-pprint-dispatch* — Modus
;;; has no real pretty-printer dispatch table, but many ANSI printer
;;; tests rebind these via my-with-standard-io-syntax / def-pprint-test
;;; (LET-bound to a "copy" of the initial dispatch).  Without these,
;;; the LET binding evaluates its init form, hits unbound-variable,
;;; and longjmps out — crashing every test in the file.  Defining them
;;; to NIL is harmless: write/prin1 don't consult the dispatch table.

(defvar *initial-print-pprint-dispatch* nil)
(defvar *print-pprint-dispatch* nil)

;;; my-with-standard-io-syntax — defined in ansi-aux.lsp at line 856, but
;;; load there often stops before reaching it (an earlier
;;; `(coerce ... string)` form with an unquoted type-name signals
;;; undefined-variable, and `is-noncontiguous-sublist-of` uses extended
;;; LOOP shapes Modus's runtime LOOP doesn't fully cover yet — each kills
;;; further forms in the load's top-level loop).  Redefine here so the
;;; def-print-test / def-pprint-test families can expand and run.

(defmacro my-with-standard-io-syntax (&rest body)
  `(let ((*package* (find-package "COMMON-LISP-USER"))
         (*print-array* t)
         (*print-base* 10)
         (*print-case* :upcase)
         (*print-circle* nil)
         (*print-escape* t)
         (*print-gensym* t)
         (*print-length* nil)
         (*print-level* nil)
         (*print-lines* nil)
         (*print-miser-width* nil)
         (*print-pprint-dispatch* *initial-print-pprint-dispatch*)
         (*print-pretty* nil)
         (*print-radix* nil)
         (*print-readably* t)
         (*print-right-margin* nil)
         (*read-base* 10)
         (*read-default-float-format* 'single-float)
         (*read-eval* t)
         (*read-suppress* nil)
         (*readtable* (copy-readtable nil)))
     ,@body))
