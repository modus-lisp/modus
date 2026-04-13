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
  "Check if x is a boxed float (subtag 96 = #x60)."
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

(defun rt-equal (a b)
  "Structural equality for RT comparisons."
  (if (eql a b)
      t
      (if (consp a)
          (if (rt-array-wrapper-p a)
              ;; Convert wrapper to string for comparison
              (rt-equal (rt-wrapper-to-string a) b)
              (if (consp b)
                  (if (rt-array-wrapper-p b)
                      (rt-equal a (rt-wrapper-to-string b))
                      (if (rt-equal (car a) (car b))
                          (rt-equal (cdr a) (cdr b))
                          nil))
                  nil))
          (if (rt-floatp a)
              (if (rt-floatp b)
                  (rt-float-equal a b)
                  nil)
              (if (stringp a)
                  (if (stringp b)
                      (string-equal a b)
                      (if (consp b)
                          (if (rt-array-wrapper-p b)
                              (rt-equal a (rt-wrapper-to-string b))
                              nil)
                          nil))
                  nil)))))

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
