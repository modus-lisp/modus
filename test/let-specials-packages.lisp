;;;; test/let-specials-packages.lisp -- one LET binding same-named specials of different packages.
;;;;   modus --script test/let-specials-packages.lisp   => LET-SPECIALS-PACKAGES PASS
;;;;
;;;; COMPILE-LET-WITH-SPECIALS paired each binding with its special BY NAME HASH, so in
;;;;   (let ((r:*progress* f) (fetch:*progress* nil) (seal:*progress* nil)) ...)
;;;; every binding took the first special's temp, and the other two set-forms read temps nothing bound:
;;;; "The variable #:SPECTMP-*PROGRESS* is unbound" -- loom's page render, every page.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defpackage :lsp-a (:use :cl) (:export #:*progress*))
(defpackage :lsp-b (:use :cl) (:export #:*progress*))
(defpackage :lsp-c (:use :cl) (:export #:*progress*))
(defvar lsp-a:*progress* :a0)
(defvar lsp-b:*progress* :b0)
(defvar lsp-c:*progress* :c0)
(defun inside () (list lsp-a:*progress* lsp-b:*progress* lsp-c:*progress*))
(defun bind-three ()
  (let ((lsp-a:*progress* :a1) (lsp-b:*progress* :b1) (lsp-c:*progress* :c1))
    (inside)))
(defun bind-three* ()
  (let* ((lsp-a:*progress* :a2) (lsp-b:*progress* lsp-a:*progress*) (lsp-c:*progress* :c2))
    (inside)))

(check "LET binds each package's special" (ignore-errors (bind-three)) '(:a1 :b1 :c1))
(check "and restores each" (inside) '(:a0 :b0 :c0))
(check "LET* sees the earlier binding" (ignore-errors (bind-three*)) '(:a2 :a2 :c2))
(check "and restores each again" (inside) '(:a0 :b0 :c0))
(dotimes (i 200) (bind-three))
(check "after the hot JIT" (list (bind-three) (inside)) '((:a1 :b1 :c1) (:a0 :b0 :c0)))

(format t "~&LET-SPECIALS-PACKAGES ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
