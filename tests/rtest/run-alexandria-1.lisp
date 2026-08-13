;;;; Run alexandria's OWN test suite (alexandria-1/tests.lisp, UNMODIFIED) on
;;;; Modus, through the RTEST package (mvm/rtest.lisp).
;;;;
;;;; Ground truth from the SBCL oracle: 230 tests register on the non-SBCL
;;;; branch (231 on SBCL; gaussian-random.2 is #+sbcl-gated) and ALL of them
;;;; pass.  So every RT:FAIL / RT:ERR line below is a Modus defect or a
;;;; genuinely missing feature — never a flaky test.
;;;;
;;;; No DEFMACRO anywhere in this driver on purpose: runtime macro support is
;;;; part of what is being measured.

(defun sy (k v)
  (princ (concatenate 'string "@@" k "=" (princ-to-string v)))
  (terpri) (finish-output))

(defun sy-safe (k thunk)
  (sy k (handler-case (funcall thunk)
          (t (c) (list :ERR (handler-case (type-of c) (t (c2) :UNKNOWN)))))))

(defun load-source-file (path)
  "Read+eval every top-level form of PATH.  Per-form errors are REPORTED
   (`!! form eval error in <tag>') and do not abort the file, so the log shows
   exactly which top-level forms Modus could not take.  *PACKAGE* is saved and
   restored, per CLHS 24.2."
  (handler-case
      (%it-eval-source (tar-bytes-to-string (%it-slurp-bytes path)) path)
    (t (c) (list :FILE-ABORT (handler-case (type-of c) (t (c2) :UNKNOWN))))))

(sy "SUITE" "alexandria-1")
(sy "PHASE" "install")
(sy "INSTALL"
    (handler-case (progn (install-tarball "/home/claude/lf/tars/alexandria.tar"
                                          "alexandria")
                         :LOADED)
      (t (c) (list :LOAD-ABORT (handler-case (type-of c) (t (c2) :UNKNOWN))))))
(sy "PKG-ALEXANDRIA" (if (find-package "ALEXANDRIA") t nil))
(sy "PKG-RTEST" (if (find-package "RTEST") t nil))
(sy "RTEST-CATCH-ERRORS" *catch-errors*)
(sy "RTEST-CATCH-ALL" *rtest-catch-all*)

(sy "PHASE" "load-tests")
(sy "TESTS-FORMS-EVALED"
    (load-source-file
     "/home/claude/quicklisp/dists/quicklisp/software/alexandria-20241012-git/alexandria-1/tests.lisp"))
(sy "PKG-ALEXANDRIA-TESTS" (if (find-package "ALEXANDRIA/TESTS") t nil))
(sy "REGISTERED" (length *rtest-entries*))
(sy "ORACLE-REGISTERED" 230)
(sy "EXPECTED-FAILURES" *expected-failures*)

(sy "PHASE" "run")
(sy "DO-TESTS-RETURN" (handler-case (do-tests)
                        (t (c) (list :RUN-ABORT
                                     (handler-case (type-of c)
                                       (t (c2) :UNKNOWN))))))
(sy "PHASE" "done")
