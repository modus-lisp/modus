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


;; The skip list is EMPTY: all 230 tests terminate and pass.  It is kept
;; (rather than deleted) because RT:SKIP counts as a FAILURE and any test
;; parked here stays in the 230 denominator -- so the list is the honest
;; inventory of non-terminating tests, and it is currently empty.  Populate
;; it mechanically from the last RT:RUN line with no following verdict.
;; UNSKIPPED: BINOMIAL-COEFFICIENT.1/.2 -- did not complete after ~217
;;   preceding tests; blamed on "bignum arithmetic under heap pressure".
;;   The real cause was the native-bridge argument wrapper turning any data
;;   fixnum equal to an in-module lambda bytecode offset into a closure
;;   (%MVM-WRAP-ESCAPING, mvm/interp.lisp).  %multiply-range's bisection
;;   passes small integers (bounds, (truncate (- k j) 2)) across that
;;   bridge, so one poisoned index derailed the recursion.  Same root cause
;;   as EXTREMUM.1; both terminate and pass now.
;; UNSKIPPED: GAUSSIAN-RANDOM.1 -- was "(random 1.0) out of [0,1)"; the real
;;   cause was the free-variable walker not macroexpanding, so the MACROLET
;;   `valid' inside gaussian-random's LABELS read MIN/MAX as NIL and the
;;   rejection sampler accepted every sample.  Fixed in the compiler; the
;;   test terminates and passes.
(defun test-name-by-string (str)
  "Look the SYMBOL up in the live registry so the skip list matches by
   identity, not by a re-READ that might intern a different symbol."
  (let ((cur *rtest-entries*) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (when (string-equal (symbol-name (rtest-entry-name (car cur))) str)
        (setq found (rtest-entry-name (car cur))))
      (setq cur (cdr cur)))))

(defun skip-these (names)
  (let ((cur names) (out nil))
    (loop
      (when (null cur) (return nil))
      (let ((s (test-name-by-string (car cur))))
        (if s (setq out (cons s out)) (sy "SKIP-NOT-FOUND" (car cur))))
      (setq cur (cdr cur)))
    (setq *rtest-skip-names* out)))

(skip-these (list))
(sy "SKIP-NAMES" *rtest-skip-names*)

(sy "PHASE" "run")
(sy "DO-TESTS-RETURN" (handler-case (do-tests)
                        (t (c) (list :RUN-ABORT
                                     (handler-case (type-of c)
                                       (t (c2) :UNKNOWN))))))
(sy "PHASE" "done")
