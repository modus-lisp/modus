;;;; Differential oracle: run alexandria's REAL tests.lisp through a
;;;; line-for-line SBCL port of mvm/rtest.lisp's semantics, and compare the
;;;; per-test verdict against SB-RT's ground truth (which is all-pass).
;;;; Any disagreement is a defect in MY RT semantics, not in Modus.

(require :asdf)
(push #p"/home/claude/quicklisp/dists/quicklisp/software/alexandria-20241012-git/"
      asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :alexandria))

(defpackage :rtest
  (:use :cl)
  (:nicknames :rt :regression-test)
  (:export #:deftest #:do-test #:do-tests #:get-test #:pending-tests
           #:rem-test #:rem-all-tests #:continue-testing
           #:*compile-tests* #:*expected-failures* #:*catch-errors*
           #:*test* #:*do-tests-when-defined* #:*print-circle-on-failure*))
(in-package :rtest)

(defvar *rtest-entries* nil)
(defvar *compile-tests* nil)
(defvar *expected-failures* nil)
(defvar *catch-errors* t)
(defvar *rtest-catch-all* nil)
(defvar *test* nil)
(defvar *do-tests-when-defined* nil)
(defvar *print-circle-on-failure* nil)
(defvar *rtest-report-limit* 400)

(defun rtest-entry-pend (e) (car (car e)))
(defun rtest-set-pend (e v) (rplaca (car e) v) v)
(defun rtest-entry-name (e) (car (cdr e)))
(defun rtest-entry-form (e) (car (cdr (cdr e))))
(defun rtest-entry-vals (e) (car (cdr (cdr (cdr e)))))

;;; --- EXACT copy of the algorithm in mvm/rtest.lisp -------------------------
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
     (let ((rx (array-rank x)) (ry (array-rank y)))
       (cond
         ((not (eql rx ry)) nil)
         ((eql rx 0)
          (rtest-equalp-with-case (row-major-aref x 0) (row-major-aref y 0)))
         ((eql rx 1)
          (let ((lx (length x)) (ly (length y)))
            (if (eql lx ly)
                (let ((i 0) (res t))
                  (loop
                    (when (or (>= i lx) (null res)) (return res))
                    (unless (rtest-equalp-with-case (aref x i) (aref y i))
                      (setq res nil))
                    (setq i (+ i 1))))
                nil)))
         (t
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

(defun rtest-find-entry (name)
  (let ((cur *rtest-entries*) (found nil))
    (loop
      (when (or found (null cur)) (return found))
      (when (eql (rtest-entry-name (car cur)) name) (setq found (car cur)))
      (setq cur (cdr cur)))))

(defun rtest-add-entry (name form vals)
  (let ((old (rtest-find-entry name)))
    (if old
        (progn (rplaca (cdr (cdr old)) form)
               (rplaca (cdr (cdr (cdr old))) vals)
               (rtest-set-pend old t))
        (setq *rtest-entries*
              (cons (list (cons t nil) name form vals) *rtest-entries*))))
  (setq *test* name)
  name)

(defmacro deftest (name form &rest vals)
  (list 'rtest-add-entry (list 'quote name) (list 'quote form) (list 'quote vals)))

(defun rtest-ordered-entries () (reverse *rtest-entries*))

(defun pending-tests ()
  (let ((cur (rtest-ordered-entries)) (out nil))
    (loop
      (when (null cur) (return (reverse out)))
      (when (rtest-entry-pend (car cur))
        (setq out (cons (rtest-entry-name (car cur)) out)))
      (setq cur (cdr cur)))))

(defun rem-all-tests () (setq *rtest-entries* nil))

(defun rtest-eval-form (form) (multiple-value-list (eval form)))

(defun rtest-do-entry (e)
  (setq *test* (rtest-entry-name e))
  (rtest-set-pend e t)
  (let ((aborted nil) (r nil))
    (if *catch-errors*
        (handler-case (setq r (rtest-eval-form (rtest-entry-form e)))
          (error (c) (progn (setq aborted t) (setq r (list c)))))
        (setq r (rtest-eval-form (rtest-entry-form e))))
    (rtest-set-pend e (if aborted t
                          (not (rtest-equalp-with-case r (rtest-entry-vals e)))))
    (format t "~&~a ~a~%" (cond ((null (rtest-entry-pend e)) "RT:PASS")
                                (aborted "RT:ERR ")
                                (t "RT:FAIL"))
            (rtest-entry-name e))
    (cond ((null (rtest-entry-pend e)) :pass) (aborted :err) (t :fail))))

(defun do-tests ()
  (let* ((all (rtest-ordered-entries)) (total (length all))
         (passed 0) (failed 0) (errored 0))
    (dolist (e all)
      (when (rtest-entry-pend e)
        (let ((res (rtest-do-entry e)))
          (cond ((eq res :pass) (incf passed))
                ((eq res :err) (incf failed) (incf errored))
                (t (incf failed))))))
    (format t "~&RT:SUMMARY total=~a ran=~a passed=~a failed=~a errored=~a~%"
            total (+ passed failed) passed failed errored)
    (format t "RT:FAILED~{ ~a~}~%" (pending-tests))
    (null (pending-tests))))

;;; --- Load the UNMODIFIED library test suite through MY rtest ---------------
(in-package :cl-user)
;; Temporarily hide :SBCL so tests.lisp takes its `#-sbcl :rtest' branch —
;; the same branch Modus takes.  The library file itself is untouched.
(let ((*features* (remove :sbcl *features*)))
  (handler-bind ((warning #'muffle-warning))
    (load "/home/claude/quicklisp/dists/quicklisp/software/alexandria-20241012-git/alexandria-1/tests.lisp")))
(format t "~&MY-REGISTERED=~a~%" (length rtest::*rtest-entries*))
(rtest:do-tests)
(sb-ext:quit)
