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

(defun rt-equal (a b)
  "Structural equality for RT comparisons."
  (if (eql a b)
      t
      (if (consp a)
          (if (consp b)
              (if (rt-equal (car a) (car b))
                  (rt-equal (cdr a) (cdr b))
                  nil)
              nil)
          nil)))

(defun deftest (id actual expected)
  "Run a test: compare ACTUAL with EXPECTED using rt-equal.
   ID is an integer test number. Prints FAIL line on mismatch."
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (rt-equal actual expected)
      (setq *rt-pass-count* (+ *rt-pass-count* 1))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        ;; Print: FAIL <id>\n
        (write-char-serial 70)   ; F
        (write-char-serial 65)   ; A
        (write-char-serial 73)   ; I
        (write-char-serial 76)   ; L
        (write-char-serial 32)   ; space
        (print-dec id)
        (write-char-serial 10))))

(defun deftest-eq (id actual expected)
  "Test with eq comparison (pointer identity)."
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (eq actual expected)
      (setq *rt-pass-count* (+ *rt-pass-count* 1))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        (write-char-serial 70)
        (write-char-serial 65)
        (write-char-serial 73)
        (write-char-serial 76)
        (write-char-serial 32)
        (print-dec id)
        (write-char-serial 10))))

(defun rt-run-test (name actual expected)
  "Run a single RT-style test. NAME is a symbol, ACTUAL is the form result,
   EXPECTED is the expected value. Compares using rt-equal."
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (rt-equal actual expected)
      (setq *rt-pass-count* (+ *rt-pass-count* 1))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        (write-char-serial 70)   ; F
        (write-char-serial 65)   ; A
        (write-char-serial 73)   ; I
        (write-char-serial 76)   ; L
        (write-char-serial 32)   ; space
        (write-object name)
        (write-char-serial 10))))

(defun rt-run-test-mv (name actuals expecteds)
  "Run a multi-value RT test. ACTUALS and EXPECTEDS are lists."
  (setq *rt-test-count* (+ *rt-test-count* 1))
  (if (rt-equal actuals expecteds)
      (setq *rt-pass-count* (+ *rt-pass-count* 1))
      (progn
        (setq *rt-fail-count* (+ *rt-fail-count* 1))
        (write-char-serial 70)
        (write-char-serial 65)
        (write-char-serial 73)
        (write-char-serial 76)
        (write-char-serial 32)
        (write-object name)
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
