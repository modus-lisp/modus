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

(defun rt-array-wrapper-p (x)
  "Check if x is a fill-pointer or displaced array wrapper (cons _ string)."
  (if (consp x) (if (stringp (cdr x)) t nil) nil))

(defun rt-wrapper-to-string (w)
  "Convert an array wrapper to a plain string for comparison."
  (let ((len (if (fixnump (car w))
                 (car w)
                 (car (car w))))
        (offset (if (fixnump (car w)) 0 (cdr (car w)))))
    (let ((s (%make-string-array len)))
      (dotimes (i len s)
        (aset s i (aref (cdr w) (+ offset i)))))))

(defun rt-arrayp (x)
  "Check if x is a plain array (object with subtag #x32)."
  (if (fixnump x) nil
    (if (consp x) nil
      (if (null x) nil
        (= (obj-subtag x) #x32)))))

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

(defun rt-equal (a b)
  "Structural equality for RT comparisons.
   Flattened to cond so the compiler doesn't run out of registers on
   the deeply-nested IF chain (which crashed inside (equalpt STR STR)
   when called from a lambda body with lots of ambient special vars)."
  (cond
    ((eql a b) t)
    ;; Cons + wrapper handling.
    ((and (consp a) (rt-array-wrapper-p a))
     (rt-equal (rt-wrapper-to-string a) b))
    ((and (consp b) (rt-array-wrapper-p b))
     (rt-equal a (rt-wrapper-to-string b)))
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
    (t nil)))

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
        (when (< *rt-fail-count* 6)
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
        (when (< *rt-fail-count* 6)
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
