;;;; rtest.lisp — RT, Paul Dietz's regression tester, as the RTEST package.
;;;;
;;;; WHY THIS FILE EXISTS
;;;;
;;;; Real Common Lisp libraries ship real test suites, and a large family of
;;;; them (alexandria, and everything that copied alexandria's layout) opens
;;;; its tests.lisp with
;;;;
;;;;   (defpackage :foo/tests
;;;;     (:use :cl :foo #+sbcl :sb-rt #-sbcl :rtest)
;;;;     (:import-from #+sbcl :sb-rt #-sbcl :rtest
;;;;                   #:*compile-tests* #:*expected-failures*))
;;;;
;;;; RT is a real, public-domain regression tester whose package is literally
;;;; named RTEST (SBCL ships it as the contrib SB-RT).  Modus is not SBCL — it
;;;; advertises :GENERA — so it takes the `#-sbcl :rtest' branch and needs an
;;;; RTEST package.  Providing one is providing a DEPENDENCY, exactly like
;;;; providing ASDF; it is NOT a shim that bends a test framework to make a
;;;; library look better than it is.
;;;;
;;;; SEMANTICS ARE RT's, NOT WHATEVER IS CONVENIENT.  A wrong DEFTEST produces
;;;; a confident, meaningless number, so this file is a faithful port of RT's
;;;; observable contract, verified against SB-RT as an oracle:
;;;;
;;;;   * (deftest NAME FORM &rest EXPECTED-VALUES) is a MACRO.  FORM is NOT
;;;;     evaluated at definition time; it is stored and EVALuated by DO-TESTS.
;;;;     EXPECTED-VALUES are literals, never evaluated.
;;;;   * The test's MULTIPLE VALUES are collected with MULTIPLE-VALUE-LIST and
;;;;     compared against the expected-value LIST.  A test returning 2 values
;;;;     where 1 was expected FAILS (SB-RT: mvshort.1 fails).
;;;;   * The comparison is RT's EQUALP-WITH-CASE — like EQUALP but WITHOUT
;;;;     case-insensitive characters and WITHOUT cross-type number equality.
;;;;     Verified against SB-RT: ("abc" "ABC")=>NIL, (#\a #\A)=>NIL,
;;;;     (1 1.0)=>NIL, ((1 2) #(1 2))=>NIL, (#(1 2) #(1 2))=>T.
;;;;   * A test that signals an ERROR is a FAILURE, not a crash of the run
;;;;     (RT's *CATCH-ERRORS*, default T).  The condition becomes the actual
;;;;     value.
;;;;   * DO-TESTS runs only PENDING entries (fresh ones and previously failed
;;;;     ones), prints the RT report, and returns T iff nothing is pending.
;;;;   * *EXPECTED-FAILURES* names tests whose failure is not "unexpected";
;;;;     they still count as failures.
;;;;   * Redefining a test by name REPLACES the earlier entry in place, keeping
;;;;     definition order.
;;;;
;;;; This is deliberately NOT built on mvm/rt.lisp's RT-EQUAL.  RT-EQUAL knows
;;;; about Modus's internal array-wrapper representations and would paper over
;;;; exactly the representation leaks a library test suite is supposed to
;;;; catch.  Everything here goes through the PUBLIC CL API (CONSP, ARRAYP,
;;;; ARRAY-RANK, LENGTH, AREF, ROW-MAJOR-AREF, EQL), so if a Modus internal
;;;; representation leaks into a value, the test FAILS — which is the honest
;;;; answer.
;;;;
;;;; DO-TESTS here intentionally shadows mvm/rt.lisp's counter-printing
;;;; DO-TESTS via last-defun-wins.  rt.lisp's DO-TESTS has no in-image callers
;;;; (the ANSI harness drives RT-RUN-TEST / RT-RUN-REGISTERED-TESTS directly),
;;;; and this file is only concatenated into images that want the RTEST
;;;; package, after rt.lisp.

;;; ============================================================
;;; State
;;;
;;; NOTE (MVM Active Limitation 7): DEFVAR init forms do NOT run at boot.
;;; Every variable that needs a non-NIL initial value is SETQ'd explicitly in
;;; %INIT-RTEST below.  *CATCH-ERRORS* defaulting to NIL instead of T would
;;; silently turn "this test signalled an error" into "the whole run died", so
;;; this is load-bearing, not cosmetic.
;;; ============================================================

(defvar *rtest-entries* nil
  "Registered test entries, MOST-RECENT-FIRST.  Definition order is the
   REVERSE of this list; DO-TESTS reverses before running so the report
   follows source order like RT's does.
   Entry shape: (PEND-CELL NAME FORM VALS) where PEND-CELL is a mutable
   cons whose CAR is the pending flag (RT stores PEND in the entry struct;
   Modus has no struct setf here, so a cons cell carries the mutation).")

(defvar *compile-tests* nil
  "RT: when true, DO-ENTRY COMPILEs the test form instead of EVALing it.
   Modus's EVAL already compiles to MVM bytecode, so this is accepted and
   recorded for source compatibility but selects no different path.")

(defvar *expected-failures* nil
  "RT: list of test names whose failure is expected.  They still FAIL; they
   are just excluded from the `unexpected failures' report.")

(defvar *catch-errors* t
  "RT: when true (the default), an ERROR signalled by a test form is caught
   and turned into a test FAILURE with the condition as the actual value.")

(defvar *rtest-catch-all* nil
  "MODUS EXTENSION, DEFAULT NIL = RT-FAITHFUL.  When NIL, DO-ENTRY catches
   only ERROR, exactly like RT — so a test that merely WARNs runs to
   completion instead of being aborted by an over-broad handler.  Set to T
   only to measure how many tests are lost to a condition-less hardware-fault
   escape (see reference_unhandled_escape_report); that reading is NOT
   RT-faithful because it also aborts on WARN, and must be reported as a
   separate number.")

(defvar *test* nil "RT: name of the most recently defined or run test.")

(defvar *do-tests-when-defined* nil
  "RT: when true, DEFTEST runs the test immediately at definition time.")

(defvar *print-circle-on-failure* nil
  "RT: value of *PRINT-CIRCLE* while printing a failure report.")

(defvar *rtest-report-limit* 400
  "Truncate each printed form/value to this many characters.  A test whose
   expected value is a 10000-element list must not drown the log.")

;;; ============================================================
;;; Entry accessors
;;; ============================================================

(defun rtest-entry-pend (e) (car (car e)))
(defun rtest-set-pend (e v) (rplaca (car e) v) v)
(defun rtest-entry-name (e) (car (cdr e)))
(defun rtest-entry-form (e) (car (cdr (cdr e))))
(defun rtest-entry-vals (e) (car (cdr (cdr (cdr e)))))

;;; ============================================================
;;; EQUALP-WITH-CASE — RT's comparison predicate
;;;
;;; Faithful port of RT's definition.  The observable contract, confirmed
;;; against SB-RT:
;;;   (eq x y)                         => T
;;;   conses recurse on CAR and CDR
;;;   rank-0 arrays compare their single element
;;;   rank-1 arrays: same LENGTH and elementwise (so a STRING and a vector of
;;;      the same CHARACTERs are equal — RT coerces both to simple-vector)
;;;   rank-N arrays: same rank, same dimensions, elementwise row-major
;;;   everything else: EQL  (so 1 and 1.0 are NOT equal, and #\a and #\A are
;;;      NOT equal — this is the whole point of "with case")
;;; ============================================================

(defun rtest-equalp-with-case (x y)
  (cond
    ((eq x y) t)
    ((consp x)
     (if (consp y)
         (if (rtest-equalp-with-case (car x) (car y))
             (rtest-equalp-with-case (cdr x) (cdr y))
             nil)
         nil))
    ((consp y) nil)
    ((and (arrayp x) (arrayp y))
     (let ((rx (array-rank x))
           (ry (array-rank y)))
       (cond
         ((not (eql rx ry)) nil)
         ((eql rx 0)
          (rtest-equalp-with-case (row-major-aref x 0) (row-major-aref y 0)))
         ((eql rx 1)
          (let ((lx (length x))
                (ly (length y)))
            (if (eql lx ly)
                (let ((i 0) (res t))
                  (loop
                    (when (or (>= i lx) (null res)) (return res))
                    (unless (rtest-equalp-with-case (aref x i) (aref y i))
                      (setq res nil))
                    (setq i (+ i 1))))
                nil)))
         (t
          ;; Same rank > 1: dimensions must match, then row-major elementwise.
          (let ((k 0) (dims-ok t))
            (loop
              (when (or (>= k rx) (null dims-ok)) (return nil))
              (unless (eql (array-dimension x k) (array-dimension y k))
                (setq dims-ok nil))
              (setq k (+ k 1)))
            (if (null dims-ok)
                nil
                (let ((n (array-total-size x)) (i 0) (res t))
                  (loop
                    (when (or (>= i n) (null res)) (return res))
                    (unless (rtest-equalp-with-case (row-major-aref x i)
                                                    (row-major-aref y i))
                      (setq res nil))
                    (setq i (+ i 1))))))))))
    ((arrayp x) nil)
    ((arrayp y) nil)
    (t (eql x y))))

;;; ============================================================
;;; Registration — RT's ADD-ENTRY
;;; ============================================================

(defun rtest-find-entry (name)
  "Return the registered entry named NAME, or NIL."
  (let ((cur *rtest-entries*) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (when (eql (rtest-entry-name (car cur)) name)
        (setq found (car cur)))
      (setq cur (cdr cur)))))

(defun rtest-add-entry (name form vals)
  "RT's ADD-ENTRY.  Registering a name that already exists REPLACES that
   entry in place (keeping its position in definition order) and marks it
   pending again — RT prints a `Redefining test' note and does the same."
  (let ((old (rtest-find-entry name)))
    (if old
        (progn
          (rplaca (cdr (cdr old)) form)
          (rplaca (cdr (cdr (cdr old))) vals)
          (rtest-set-pend old t))
        (setq *rtest-entries*
              (cons (list (cons t nil) name form vals) *rtest-entries*))))
  (setq *test* name)
  (when *do-tests-when-defined*
    (rtest-do-entry (rtest-find-entry name)))
  name)

(defun rtest-ordered-entries ()
  "Entries in DEFINITION order."
  (reverse *rtest-entries*))

(defun rem-all-tests ()
  "RT: forget every registered test."
  (setq *rtest-entries* nil)
  nil)

(defun rem-test (name)
  "RT: forget the test named NAME.  Returns NAME if it was present."
  (let ((cur *rtest-entries*) (out nil) (hit nil))
    (loop
      (when (null cur) (return nil))
      (if (eql (rtest-entry-name (car cur)) name)
          (setq hit t)
          (setq out (cons (car cur) out)))
      (setq cur (cdr cur)))
    (setq *rtest-entries* (reverse out))
    (if hit name nil)))

(defun get-test (name)
  "RT: return (NAME FORM . VALS) for the named test, or NIL."
  (let ((e (rtest-find-entry name)))
    (if e
        (cons (rtest-entry-name e)
              (cons (rtest-entry-form e) (rtest-entry-vals e)))
        nil)))

(defun pending-tests ()
  "RT: names of all tests still marked pending (never run, or last failed),
   in definition order."
  (let ((cur (rtest-ordered-entries)) (out nil))
    (loop
      (when (null cur) (return (reverse out)))
      (when (rtest-entry-pend (car cur))
        (setq out (cons (rtest-entry-name (car cur)) out)))
      (setq cur (cdr cur)))))

;;; ============================================================
;;; Reporting
;;; ============================================================

(defun rtest-safe-str (x)
  "PRIN1 X to a string, bounded, never signalling.  A failing test's actual
   value is arbitrary user data and may not be printable at all."
  (handler-case
      (let ((s (prin1-to-string x)))
        (if (> (length s) *rtest-report-limit*)
            (concatenate 'string (subseq s 0 *rtest-report-limit*) "...")
            s))
    (t (c) "#<unprintable>")))

(defun rtest-cond-type (c)
  (handler-case (prin1-to-string (type-of c)) (t (c2) "#<unknown-type>")))

(defun rtest-cond-text (c)
  (handler-case
      (let ((s (princ-to-string c)))
        (if (> (length s) *rtest-report-limit*)
            (concatenate 'string (subseq s 0 *rtest-report-limit*) "...")
            s))
    (t (c2) "#<unreportable>")))

(defun rtest-report-entry (e aborted r)
  "Print one machine-readable result line, plus RT-style detail on failure.
   Line shapes (one per test, always at column 0):
     RT:PASS <name>
     RT:FAIL <name>          — ran to completion, wrong values
     RT:ERR  <name>          — signalled; the condition is the actual value
   Failure detail follows on `  Form:' / `  Expected:' / `  Actual:' lines
   so a grep-based cluster pass can key on the condition type or the form."
  (let ((name (rtest-entry-name e)))
    (if (rtest-entry-pend e)
        (progn
          (terpri)
          (princ (if aborted "RT:ERR " "RT:FAIL "))
          (princ name)
          (terpri)
          (princ "  Form: ") (princ (rtest-safe-str (rtest-entry-form e))) (terpri)
          (princ "  Expected: ") (princ (rtest-safe-str (rtest-entry-vals e))) (terpri)
          (if aborted
              (progn
                (princ "  Cond: ") (princ (rtest-cond-type (car r))) (terpri)
                (princ "  Text: ") (princ (rtest-cond-text (car r))) (terpri))
              (progn
                (princ "  Actual: ") (princ (rtest-safe-str r)) (terpri)))
          (finish-output))
        (progn
          (terpri)
          (princ "RT:PASS ")
          (princ name)
          (terpri)
          (finish-output)))))

;;; ============================================================
;;; Running — RT's DO-ENTRY / DO-ENTRIES / DO-TESTS
;;; ============================================================

(defun rtest-eval-form (form)
  (multiple-value-list (eval form)))

(defun rtest-do-entry (e)
  "RT's DO-ENTRY.  Evaluate the entry's FORM, collect its multiple values,
   compare to the expected list with EQUALP-WITH-CASE, set the pending flag,
   report.  Returns :PASS, :FAIL (ran, wrong values) or :ERR (signalled).
   RT's DO-ENTRY returns *TEST* on success and NIL otherwise; nothing in a
   library suite consumes that, and the three-way split is what the summary
   and the failure clustering need."
  (setq *test* (rtest-entry-name e))
  (rtest-set-pend e t)
  (let ((aborted nil) (r nil))
    (if *catch-errors*
        (if *rtest-catch-all*
            ;; NON-RT diagnostic mode: also aborts on WARN.  See the
            ;; *rtest-catch-all* docstring — never the reported number.
            (handler-case (setq r (rtest-eval-form (rtest-entry-form e)))
              (t (c) (progn (setq aborted t) (setq r (list c)))))
            (handler-case (setq r (rtest-eval-form (rtest-entry-form e)))
              (error (c) (progn (setq aborted t) (setq r (list c))))))
        (setq r (rtest-eval-form (rtest-entry-form e))))
    (rtest-set-pend e
                    (if aborted
                        t
                        (not (rtest-equalp-with-case r (rtest-entry-vals e)))))
    (rtest-report-entry e aborted r)
    (cond
      ((null (rtest-entry-pend e)) :pass)
      (aborted :err)
      (t :fail))))

(defun do-test (name)
  "RT: run the single test named NAME."
  (let ((e (rtest-find-entry name)))
    (if e (progn (rtest-do-entry e) (rtest-entry-name e)) nil)))

(defun rtest-name-member (name names)
  (let ((cur names) (hit nil))
    (loop
      (when (or hit (null cur)) (return hit))
      (when (eql (car cur) name) (setq hit t))
      (setq cur (cdr cur)))))

(defun rtest-print-name-list (label names)
  (princ label)
  (let ((cur names))
    (loop
      (when (null cur) (return nil))
      (princ " ")
      (princ (car cur))
      (setq cur (cdr cur))))
  (terpri))

(defun rtest-run-pending ()
  "Run every pending entry in definition order.  Returns (values passed
   failed errored) for THIS run — `errored' counts entries whose form
   signalled, which are a subset of the failures."
  (let ((cur (rtest-ordered-entries))
        (passed 0) (failed 0) (errored 0))
    (loop
      (when (null cur) (return (values passed failed errored)))
      (let ((e (car cur)))
        (when (rtest-entry-pend e)
          (let ((res (rtest-do-entry e)))
            (cond
              ((eq res :pass) (setq passed (+ passed 1)))
              ((eq res :err) (progn (setq failed (+ failed 1))
                                    (setq errored (+ errored 1))))
              (t (setq failed (+ failed 1)))))))
      (setq cur (cdr cur)))))

(defun do-tests ()
  "RT's DO-TESTS.  Runs every PENDING test (all of them, on a fresh
   registration), prints the report, and returns T iff nothing is pending
   afterwards.  Machine-readable summary lines:
     RT:START total=<N> pending=<N>
     RT:PASS/RT:FAIL/RT:ERR <name>       (one per test run)
     RT:SUMMARY total=<N> ran=<N> passed=<N> failed=<N> errored=<N>
     RT:FAILED <name> ...                (all still-pending names)
     RT:UNEXPECTED <name> ...            (pending minus *expected-failures*)"
  (let ((all (rtest-ordered-entries)))
    (let ((total (length all))
          (npend (length (pending-tests))))
      (terpri)
      (princ "RT:START total=") (princ total)
      (princ " pending=") (princ npend)
      (terpri)
      (finish-output)
      (multiple-value-bind (passed failed errored) (rtest-run-pending)
        (let ((pending (pending-tests)))
          (let ((unexpected nil) (cur pending))
            (loop
              (when (null cur) (return nil))
              (unless (rtest-name-member (car cur) *expected-failures*)
                (setq unexpected (cons (car cur) unexpected)))
              (setq cur (cdr cur)))
            (setq unexpected (reverse unexpected))
            (terpri)
            (princ "RT:SUMMARY total=") (princ total)
            (princ " ran=") (princ (+ passed failed))
            (princ " passed=") (princ passed)
            (princ " failed=") (princ failed)
            (princ " errored=") (princ errored)
            (terpri)
            (rtest-print-name-list "RT:FAILED" pending)
            (rtest-print-name-list "RT:UNEXPECTED" unexpected)
            (if (null pending)
                (princ "No tests failed.")
                (progn
                  (princ (length pending))
                  (princ " out of ")
                  (princ total)
                  (princ " total tests failed.")))
            (terpri)
            (finish-output)
            (null pending)))))))

(defun continue-testing ()
  "RT: re-run only the tests still pending."
  (do-tests))

;;; ============================================================
;;; Package + boot init
;;; ============================================================

(defun %install-rtest-deftest-macro ()
  "Register the runtime DEFTEST macro with RT semantics:
     (deftest NAME FORM . VALS) => (rtest-add-entry 'NAME 'FORM 'VALS)
   FORM is QUOTED — RT evaluates it at DO-TESTS time, not at definition
   time — and VALS are literals, never evaluated.

   This is the runtime macro path (set-macro-function over an EVALed
   lambda): a `(defmacro deftest ...)' in built source would only land in
   the BUILD-TIME macro table and never reach runtime EVAL.  Pattern mirrors
   %install-deftest-macro in rt.lisp, which this deliberately overrides —
   that one registers an eagerly-thunked entry for the ANSI harness's
   RT-RUN-REGISTERED-TESTS, which is a different (Modus-internal) contract.

   THE EXPANDER ACCEPTS BOTH OF MODUS'S TWO CALLING CONVENTIONS.  Modus has
   two, and which one you get depends on what shape the expander object is:

     * a compiled expander (which is what `(eval '(lambda …))' produces —
       eval2 COMPILES the lambda) is called with the WHOLE FORM, the
       CL-standard convention: macroexpand-1 (cl-eval.lisp) and
       macroexpand-1-mvm (compiler.lisp) both do `(funcall mf form)'.
     * an %interp-closure expander (what a runtime DEFMACRO produces) is
       called with `(cdr form)', i.e. one parameter per macro argument.

   rt.lisp's %install-deftest-macro assumes the SECOND for a lambda that
   actually takes the FIRST, so its `name' parameter receives the entire
   `(deftest NAME FORM . VALS)' list.  Measured, not theorised: with the
   single-convention expander every test registered under the whole form as
   its NAME, with a NIL body, and all 6 smoke tests failed.  Discriminating
   on `(consp a)' is exact — a whole form is always a CONS and an RT test
   NAME is always a SYMBOL — so this is correct under either convention and
   stays correct if the dispatch changes."
  (set-macro-function 'deftest
    (eval '(lambda (a &rest more)
             (if (consp a)
                 ;; Whole-form convention: A = (DEFTEST NAME FORM . VALS).
                 (list 'rtest-add-entry
                       (list 'quote (car (cdr a)))
                       (list 'quote (car (cdr (cdr a))))
                       (list 'quote (cdr (cdr (cdr a)))))
                 ;; Arg-list convention: A = NAME, MORE = (FORM . VALS).
                 (list 'rtest-add-entry
                       (list 'quote a)
                       (list 'quote (car more))
                       (list 'quote (cdr more))))))))

(defun %init-rtest ()
  "Create the RTEST package and initialize RT's specials.

   The package MUST be created here, at BOOT, from image code — not at
   runtime by a driver.  Packages born while *MVM-EVAL-RUNTIME-P* is true
   are marked runtime-born and their symbols get PACKAGE-FOLDED
   function-table keys (see *runtime-born-pkgs* in cl-packages.lisp).  An
   RTEST born that way would give RTEST:DO-TESTS a different fn key than
   the image's DO-TESTS defined right here, and every suite calling
   (do-tests) would get UNDEFINED-FUNCTION.  Boot-born keeps the bare
   name-hash key, so a suite that inherits DO-TESTS through (:use :rtest)
   resolves to this implementation."
  (%defpackage-impl "RTEST"
    (list (list :use "COMMON-LISP")
          (list :nicknames "RT" "REGRESSION-TEST")
          (list :export
                "DEFTEST" "DO-TEST" "DO-TESTS" "GET-TEST" "PENDING-TESTS"
                "REM-TEST" "REM-ALL-TESTS" "CONTINUE-TESTING"
                "*COMPILE-TESTS*" "*EXPECTED-FAILURES*" "*CATCH-ERRORS*"
                "*TEST*" "*DO-TESTS-WHEN-DEFINED*"
                "*PRINT-CIRCLE-ON-FAILURE*")))
  ;; Limitation #7: defvar init forms do not run at boot.  Set every
  ;; non-NIL default explicitly.
  (setq *rtest-entries* nil)
  (setq *compile-tests* nil)
  (setq *expected-failures* nil)
  (setq *catch-errors* t)
  (setq *rtest-catch-all* nil)
  (setq *test* nil)
  (setq *do-tests-when-defined* nil)
  (setq *print-circle-on-failure* nil)
  (setq *rtest-report-limit* 400)
  (%install-rtest-deftest-macro)
  t)
