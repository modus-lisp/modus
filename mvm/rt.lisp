;;;; rt.lisp — Regression Test framework for MVM
;;;;
;;;; Minimal RT harness compatible with the ANSI CL test suite pattern.
;;;; Phase 1: eager evaluation (deftest as a 3-arg function)
;;;; Phase 2: deferred evaluation via closures (deftest with thunks)
;;;; Goal: evolve toward full RT (Paul Dietz) compatibility.
;;;;
;;;; Usage:
;;;;   (deftest <id> <form-result> <expected-result>)
;;;;   (do-tests)  ; prints summary, returns fail count

;;; ============================================================
;;; Test State
;;; ============================================================

(defvar *rt-test-count* 0)
(defvar *rt-pass-count* 0)
(defvar *rt-fail-count* 0)

;;; ============================================================
;;; Output Helpers (serial — no format available)
;;; ============================================================

(defun rt-print-string (chars)
  "Print a list of char codes to serial."
  (let ((cur chars))
    (loop
      (when (null cur) (return nil))
      (write-char-serial (car cur))
      (setq cur (cdr cur)))))

;;; ============================================================
;;; Core Test Functions
;;; ============================================================

(defun rt-floatp (x)
  "Check if x is a boxed float (subtag 96 = #x60).

   The obj-subtag IR-op is tag-safe (translate-x64.lisp's +op-obj-subtag+
   guards the deref), so reaching this on T no longer crashes — it
   returns 0 which won't match 96."
  (if (fixnump x) nil
    (if (consp x) nil
      (if (null x) nil
        (= (obj-subtag x) 96)))))

(defun rt-float-equal (a b)
  "Compare two boxed floats by their hi32/lo32 slots."
  (if (= (aref a 0) (aref b 0))
      (= (aref a 1) (aref b 1))
      nil))

(defun %rt-unadj (x)
  "Peel (cons 8765432 ...) adjustable wrapper if present."
  (if (and (consp x) (eql (car x) 8765432)) (cdr x) x))

(defun rt-array-wrapper-p (x)
  "Check if x is a fill-pointer or displaced array wrapper (cons _ string)."
  (let ((y (%rt-unadj x)))
    (if (consp y) (if (stringp (cdr y)) t nil) nil)))

(defun rt-wrapper-to-string (w)
  "Convert an array wrapper to a plain string for comparison."
  (let ((y (%rt-unadj w)))
    (let ((len (if (fixnump (car y))
                   (car y)
                   (car (car y))))
          (offset (if (fixnump (car y)) 0 (cdr (car y)))))
      (let ((s (%make-string-array len)))
        (dotimes (i len s)
          (aset s i (aref (cdr y) (+ offset i))))))))

(defun rt-mda-visible-data (m)
  "Return the user-visible data vector of an MDA: the underlying data
   sliced to (length m) — fp if set, else dim product.  For displaced
   MDAs the data slot holds the displaced-to target which may be
   larger; we always materialize a fresh vector of the visible length
   walked through aref (which routes displacement)."
  (let* ((m-len (length m))
         (out (make-array m-len)) (i 0))
    (loop (when (>= i m-len) (return out))
      (aset out i (aref m i))
      (setq i (+ i 1)))))

(defun rt-arrayp (x)
  "Check if x is a plain array (object with subtag #x32) or a native
   multi-dim array (subtag #x34).  MDAs are NOT plain arrays for
   element-by-element comparison — they get peeled to (%mda-data x)
   first by rt-equal — but we still report rt-arrayp T so the dispatch
   matches what `(arrayp x)` would say."
  (if (fixnump x) nil
    (if (consp x) nil
      (if (null x) nil
        (let ((st (obj-subtag x)))
          (if (= st #x32) t (= st #x34)))))))

(defun rt-array-equal (a b)
  "Compare two arrays element-by-element."
  (let ((la (array-length a))
        (lb (array-length b)))
    (if (= la lb)
        (let ((i 0))
          (loop
            (when (= i la) (return t))
            (unless (rt-equal (aref a i) (aref b i))
              (return nil))
            (setq i (+ i 1))))
        nil)))

(defun rt-fp-array-wrapper-p (x)
  "True if x is a fill-pointer wrapper around a non-string array.
   Shape: (cons fixnum array) or (cons 8765432 (cons fixnum array))."
  (let ((y (if (and (consp x) (eql (car x) 8765432)) (cdr x) x)))
    (if (consp y)
        (if (fixnump (car y))
            (if (stringp (cdr y)) nil (rt-arrayp (cdr y)))
            nil)
        nil)))

(defun rt-fp-wrapper-to-array (w)
  "Truncate fp-wrapped array to its fill pointer length."
  (let ((y (if (and (consp w) (eql (car w) 8765432)) (cdr w) w)))
    (let ((fp (car y))
          (arr (cdr y)))
      (let ((out (make-array fp)))
        (dotimes (i fp out) (aset out i (aref arr i)))))))

;; Displaced array wrappers around a non-string array.
;; Shape: (cons (cons SIZE OFFSET) array) — the head cons describes the
;; declared dimension (size) plus an offset into the underlying base array.
(defun rt-disp-array-wrapper-p (x)
  (let ((y (if (and (consp x) (eql (car x) 8765432)) (cdr x) x)))
    (if (consp y)
        (if (consp (car y))
            (if (stringp (cdr y)) nil (rt-arrayp (cdr y)))
            nil)
        nil)))

(defun rt-disp-wrapper-to-array (w)
  "Convert a displaced array wrapper to a plain array of the declared size,
   slicing offset..offset+size out of the underlying."
  (let ((y (if (and (consp w) (eql (car w) 8765432)) (cdr w) w)))
    (let ((size (car (car y)))
          (off (cdr (car y)))
          (arr (cdr y)))
      (let ((out (make-array size)))
        (dotimes (i size out)
          (aset out i (aref arr (+ off i))))))))

(defun rt-equal (a b)
  "Structural equality for RT comparisons.
   Flattened to cond so the compiler doesn't run out of registers on
   the deeply-nested IF chain (which crashed inside (equalpt STR STR)
   when called from a lambda body with lots of ambient special vars)."
  (cond
    ((eql a b) t)
    ;; Adjustable wrapper (cons 8765432 inner): peel and recurse.
    ((and (consp a) (eql (car a) 8765432))
     (rt-equal (cdr a) b))
    ((and (consp b) (eql (car b) 8765432))
     (rt-equal a (cdr b)))
    ;; Multi-dim wrapper (cons 9867654 (cons DIMS FLAT)): peel to flat array.
    ((and (consp a) (eql (car a) 9867654) (consp (cdr a)))
     (rt-equal (cddr a) b))
    ((and (consp b) (eql (car b) 9867654) (consp (cdr b)))
     (rt-equal a (cddr b)))
    ;; Native MDA (subtag #x34): peel to (%mda-data X) and recurse.
    ;; If both sides are MDA we compare data vectors; if one side is a
    ;; plain vector or string the underlying data still matches.  Note:
    ;; %mda-p must be called BEFORE the rt-arrayp branch below since
    ;; rt-arrayp returns T for #x34 too — without this peel, we'd hit
    ;; rt-array-equal which would compare HEADER slots, not data.
    ;; Fill-pointer aware: if the MDA has a fp, slice the data to fp length
    ;; before comparing — CL's length on fp-vectors returns fp, so the
    ;; user-visible "contents" stop at fp.  Without this, push tests fail
    ;; because the underlying data has trailing pre-init bytes.
    ((and (not (consp a)) (not (fixnump a)) (%mda-p a))
     (rt-equal (rt-mda-visible-data a) b))
    ((and (not (consp b)) (not (fixnump b)) (%mda-p b))
     (rt-equal a (rt-mda-visible-data b)))
    ;; Cons + wrapper handling.
    ((and (consp a) (rt-array-wrapper-p a))
     (rt-equal (rt-wrapper-to-string a) b))
    ((and (consp b) (rt-array-wrapper-p b))
     (rt-equal a (rt-wrapper-to-string b)))
    ;; Fill-pointer-wrapped non-string array (e.g. #(a b x) wrapper)
    ((and (consp a) (rt-fp-array-wrapper-p a))
     (rt-equal (rt-fp-wrapper-to-array a) b))
    ((and (consp b) (rt-fp-array-wrapper-p b))
     (rt-equal a (rt-fp-wrapper-to-array b)))
    ;; Displaced array wrapper around a non-string array
    ((and (consp a) (rt-disp-array-wrapper-p a))
     (rt-equal (rt-disp-wrapper-to-array a) b))
    ((and (consp b) (rt-disp-array-wrapper-p b))
     (rt-equal a (rt-disp-wrapper-to-array b)))
    ;; Plain cons-cell equality.
    ((and (consp a) (consp b))
     (if (rt-equal (car a) (car b))
         (rt-equal (cdr a) (cdr b))
         nil))
    ;; One is cons, one isn't — not equal.
    ((or (consp a) (consp b)) nil)
    ;; Float equality.
    ((and (rt-floatp a) (rt-floatp b)) (rt-float-equal a b))
    ;; Strings.
    ((and (stringp a) (stringp b)) (string-equal a b))
    ;; Plain arrays.
    ((and (rt-arrayp a) (rt-arrayp b)) (rt-array-equal a b))
    ;; String vs vector-of-chars: ANSI tests pass mixed-type sequences when
    ;; (make-array N :initial-contents "abc") is expected to equal
    ;; #(#\a #\b #\c).  Compare by aref.
    ((and (stringp a) (rt-arrayp b)) (rt-string-array-equal a b))
    ((and (rt-arrayp a) (stringp b)) (rt-string-array-equal b a))
    (t nil)))

(defun rt-string-array-equal (s a)
  "Compare a string S to a general array A of characters.  Returns T iff
   they have the same length and each char in A matches the corresponding
   char-code in S (using char= regardless of how the elements are encoded)."
  (let ((ls (array-length s))
        (la (array-length a)))
    (if (= ls la)
        (let ((i 0))
          (loop
            (when (= i ls) (return t))
            (let ((cs (aref s i)) (ca (aref a i)))
              ;; AREF on a string returns a character (immediate), AREF on
              ;; a general T-array returns whatever was stored — usually a
              ;; character.  Compare via char-code if both are characters,
              ;; else fall back to eql.
              (cond
                ((and (characterp cs) (characterp ca))
                 (unless (eql (char-code cs) (char-code ca)) (return nil)))
                ((and (fixnump cs) (characterp ca))
                 (unless (eql cs (char-code ca)) (return nil)))
                ((and (characterp cs) (fixnump ca))
                 (unless (eql (char-code cs) ca) (return nil)))
                (t (unless (eql cs ca) (return nil)))))
            (setq i (+ i 1))))
        nil)))

(defun deftest (id actual expected &rest extra-expected)
  "Run a test: compare ACTUAL with EXPECTED using rt-equal.
   ID is an integer test number OR a symbol (for define-condition tests).
   ANSI's deftest takes (name form &rest expected-values), so we accept
   extra trailing expected values via &rest and ignore them — the test
   harness's MVM-side bridge passes either 1 expected (rt-equal) or
   collects multi-value expected via fork-test-mv, never reaches here
   with extras except when load-time deftest forms slip through.
   Prints FAIL line on mismatch."
  (declare (ignore extra-expected))
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (rt-equal actual expected)
      (progn
        (setq *rt-pass-count* (+ *rt-pass-count* 1))
        ;; Mirror rt-run-test: emit \"\\nP:<id>\\n\" so the shard summary
        ;; counts passes accurately regardless of deftest vs rt-run-test.
        (write-char-serial 10)
        (write-char-serial 80)    ; P
        (write-char-serial 58)    ; :
        (if (fixnump id) (print-dec id) (write-object id))
        (write-char-serial 10))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        (write-char-serial 10)   ; guarantee a newline before FAIL
        (write-char-serial 70)   ; F
        (write-char-serial 65)   ; A
        (write-char-serial 73)   ; I
        (write-char-serial 76)   ; L
        (write-char-serial 32)   ; space
        ;; Use write-object for non-fixnums so symbols print correctly.
        (if (fixnump id) (print-dec id) (write-object id))
        (write-char-serial 10))))

(defun deftest-eq (id actual expected &rest extra-expected)
  "Test with eq comparison (pointer identity)."
  (declare (ignore extra-expected))
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (eq actual expected)
      (progn
        (setq *rt-pass-count* (+ *rt-pass-count* 1))
        (write-char-serial 10)
        (write-char-serial 80) (write-char-serial 58)
        (if (fixnump id) (print-dec id) (write-object id))
        (write-char-serial 10))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        (write-char-serial 10)
        (write-char-serial 70)
        (write-char-serial 65)
        (write-char-serial 73)
        (write-char-serial 76)
        (write-char-serial 32)
        (if (fixnump id) (print-dec id) (write-object id))
        (write-char-serial 10))))

(defun rt-run-test (name actual expected)
  "Run a single RT-style test. NAME is a symbol, ACTUAL is the form result,
   EXPECTED is the expected value. Compares using rt-equal."
  ;; "T:N\n" — last successful T: + 1 = test that crashed the fork.
  ;; Printed after args eval, so it tells us "the previous test's args
  ;; finished computing and we entered rt-run-test for THIS test."
  (write-char-serial 84) (write-char-serial 58)
  (print-dec name)
  (write-char-serial 10)
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (rt-equal actual expected)
      (progn
        (setq *rt-pass-count* (+ *rt-pass-count* 1))
        ;; "P:<id>\n" per pass — ID-tagged so the summary can count exactly
        ;; even when other output contains spurious "+" characters from
        ;; cyclic/huge print output.
        (write-char-serial 10)    ; \n (in case prior line unterminated)
        (write-char-serial 80)    ; P
        (write-char-serial 58)    ; :
        (print-dec name)
        (write-char-serial 10))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        (write-char-serial 10)   ; newline before FAIL (since "+" has none)
        (write-char-serial 70)   ; F
        (write-char-serial 65)   ; A
        (write-char-serial 73)   ; I
        (write-char-serial 76)   ; L
        (write-char-serial 32)   ; space
        (write-object name)
        ;; Print actual value for first 5 failures — bounded.
        (when (< *rt-fail-count* 50)
          (write-char-serial 32)   ; space
          (write-string-serial "GOT:")
          (setq *write-object-budget* 200)
          (write-object actual)
          (write-string-serial " EXP:")
          (setq *write-object-budget* 200)
          (write-object expected))
        (write-char-serial 10))))

(defun rt-run-test-mv (name actuals expecteds)
  "Run a multi-value RT test. ACTUALS and EXPECTEDS are lists."
  (write-char-serial 84) (write-char-serial 58)
  (print-dec name)
  (write-char-serial 10)
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (rt-equal actuals expecteds)
      (progn
        (setq *rt-pass-count* (+ *rt-pass-count* 1))
        ;; Emit \"\\nP:<name>\\n\" — same format as rt-run-test so the
        ;; sharded summary counts multi-value passes the same way.
        (write-char-serial 10)
        (write-char-serial 80) (write-char-serial 58)
        (write-object name)
        (write-char-serial 10))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        (write-char-serial 10)
        (write-char-serial 70)
        (write-char-serial 65)
        (write-char-serial 73)
        (write-char-serial 76)
        (write-char-serial 32)
        (write-object name)
        (when (< *rt-fail-count* 50)
          (write-char-serial 32)
          (write-string-serial "GOT:")
          (setq *write-object-budget* 200)
          (write-object actuals)
          (write-string-serial " EXP:")
          (setq *write-object-budget* 200)
          (write-object expecteds))
        (write-char-serial 10))))

(defun do-tests ()
  "Print test summary. Returns fail count (0 = all pass)."
  (write-char-serial 10)
  (print-dec *rt-pass-count*)
  (write-char-serial 47)   ; /
  (print-dec *rt-test-count*)
  (write-char-serial 32)
  (if (= *rt-fail-count* 0)
      (progn
        ;; PASS
        (write-char-serial 80)
        (write-char-serial 65)
        (write-char-serial 83)
        (write-char-serial 83))
      (progn
        ;; FAIL
        (write-char-serial 70)
        (write-char-serial 65)
        (write-char-serial 73)
        (write-char-serial 76)))
  (write-char-serial 10)
  *rt-fail-count*)

;;; ============================================================
;;; Unmodified-suite path: register-then-run
;;; ============================================================
;;;
;;; ANSI suite source has (deftest NAME FORM &rest EXPECTED-VALUES) at
;;; load time.  CL's RT treats deftest as a MACRO so EXPECTED-VALUES is
;;; a literal, not evaluated.  rt-register-test stores (name thunk
;;; expected) on *rt-registered-tests* without running.
;;;
;;; rt-run-registered-tests walks the registry, calls each thunk inside
;;; a handler-case, compares to expected via rt-equal, and emits the
;;; same P:<id> / FAIL <id> lines the build-time pipeline already emits
;;; so any harness reading those stays compatible.
;;;
;;; The deftest defmacro itself is registered at boot via
;;; %install-deftest-macro (called from kernel-main of any image that
;;; supports the runtime-load path) — we can't define a defmacro from
;;; source and have it reach the runtime macro table, since defmacros
;;; in built source only register in the BUILD-TIME *macro-table*.

(defvar *rt-registered-tests* nil
  "List of registered tests, head-most-recent.
   Each entry: (name thunk expected-list).")

(defun rt-register-test (name thunk expected)
  "Push a deftest registration."
  (setq *rt-registered-tests*
        (cons (list name thunk expected) *rt-registered-tests*)))

(defun %install-runtime-cl-macros-late ()
  "Re-attempt installing runtime CL macros after boot.  The compiled-
   context install at boot-time has a stability bug — most macros
   register cleanly, but ones whose bodies are wider (longer source
   strings) trip an as-yet-undiagnosed condition flow inside
   handler-case.  Calling the same loop from a non-boot context
   (here, just before tests run) succeeds: same source forms, same
   eval, but no failures.  Until the root cause is found, do the
   install twice — boot gets the easy ones, this pass mops up the
   rest."
  (when (boundp '*modus-runtime-macros*)
    (let ((forms *modus-runtime-macros*))
      (loop
        (when (null forms) (return nil))
        (handler-case (eval (read-from-string (car forms))) (t (c) nil))
        (setq forms (cdr forms))))))

(defun %install-modus-test-aux-overrides ()
  "Replace ansi-aux.lsp's implementation-specific helper macros with
   Modus-friendly versions.

   ansi-aux's signals-error is `(not (catch 0 ,form t))`, which relies
   on the implementation translating errors into throws — that holds
   for some Lisps but not Modus (catch and handler-case use the same
   setjmp/longjmp mechanism but compile-time catch's handler re-raises
   non-throw conditions per strict CL semantics).  Override with a
   handler-case form that's correct everywhere.

   Tests survive: the test thunks captured at deftest-expansion time
   embed signals-error CALL FORMS (not the expanded macro), so once
   we re-install the macro here the next runtime EVAL inside each
   thunk picks up the new expander."
  (set-macro-function 'signals-error
    (eval '(lambda (form &rest ignored)
             (declare (ignore ignored))
             (list 'handler-case
                   (list 'progn form 'nil)
                   '(t (c) t))))))

(defun rt-run-registered-tests ()
  "Walk *rt-registered-tests* in registration order (i.e. reverse the
   head-most-recent list) and run each thunk.  Compare its multiple-
   value return to expected via rt-equal.  Emit P:<id> / FAIL <id>
   lines compatible with the existing shard summary scripts.  Returns
   (values pass-count fail-count crash-count)."
  ;; Install Modus-friendly signals-error (overwriting ansi-aux's
  ;; catch-based version) so .error.N tests behave correctly here.
  (%install-modus-test-aux-overrides)
  ;; Mop up any runtime CL macros that the boot-time install missed
  ;; (the compiled-context loop has a stability bug — see
  ;; %install-runtime-cl-macros-late docstring).
  (%install-runtime-cl-macros-late)
  (let ((all (nreverse *rt-registered-tests*))
        (pass 0) (fail 0) (crash 0))
    (setq *rt-registered-tests* nil)
    (let ((cur all))
      (loop
        (when (null cur) (return nil))
        (let ((entry (car cur)))
          (let ((name (car entry))
                (thunk (cadr entry))
                (expected (caddr entry)))
            ;; Each test runs inside a fresh BLOCK / RETURN-FROM frame
            ;; so a thunk that does (return X) or (return-from FOO Y)
            ;; without an enclosing block lands here as a "?" rather
            ;; than escaping past the handler-case below.  Clear the
            ;; eval-escape stack before each test so a stale escape
            ;; left over from a previous test's non-condition unwind
            ;; doesn't poison the current one.
            (setq *%eval-escape-stack* nil)
            (handler-case
              ;; Route through %do-funcall — the test thunk is an
              ;; interp-closure from deftest's runtime expansion, and
              ;; compile-funcall's compiled dispatch doesn't handle that
              ;; representation.  Wrap in multiple-value-list to capture
              ;; (values …) results — order.N tests rely on that.
              (let ((actual (multiple-value-list (%do-funcall thunk nil))))
                (if (rt-equal actual expected)
                    (progn
                      (setq pass (+ pass 1))
                      (write-char-serial 10)
                      (write-char-serial 80)    ; P
                      (write-char-serial 58)    ; :
                      (if (fixnump name) (print-dec name) (write-object name))
                      (write-char-serial 10))
                    (progn
                      (setq fail (+ fail 1))
                      (write-char-serial 10)
                      (write-char-serial 70)    ; F
                      (write-char-serial 65)    ; A
                      (write-char-serial 73)    ; I
                      (write-char-serial 76)    ; L
                      (write-char-serial 32)    ; space
                      (if (fixnump name) (print-dec name) (write-object name))
                      (write-char-serial 10))))
              (t (c)
                 (setq crash (+ crash 1))
                 (write-char-serial 10)
                 (write-char-serial 67)    ; C
                 (write-char-serial 82)    ; R
                 (write-char-serial 65)    ; A
                 (write-char-serial 83)    ; S
                 (write-char-serial 72)    ; H
                 (write-char-serial 32)    ; space
                 (if (fixnump name) (print-dec name) (write-object name))
                 (write-char-serial 10)))))
        (setq cur (cdr cur))))
    (values pass fail crash)))

(defun %install-deftest-macro ()
  "Register the runtime deftest macro: (deftest NAME FORM &rest EXPECTED)
   expands to (rt-register-test 'NAME (lambda () FORM) '(EXPECTED...)).
   This is the runtime macro path — built source's `(defmacro deftest ...)`
   would only register in the compile-time *macro-table*, which doesn't
   reach runtime EVAL.  Pattern mirrors %install-runtime-backquote.

   Expander signature matches CL defmacro semantics (one param per macro
   arg).  Modus's macroexpand-1 calls interp-closure expanders with
   (cdr whole-form) — so &rest tail picks up the expected values."
  (set-macro-function 'deftest
    (eval '(lambda (name form-arg &rest expected)
             (list 'rt-register-test
                   (list 'quote name)
                   (list 'lambda nil form-arg)
                   (list 'quote expected))))))
