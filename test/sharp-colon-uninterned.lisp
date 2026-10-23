;;; Regression: #:NAME interns nothing.  The reader used to read the token as an ordinary
;;; symbol -- interning it in *PACKAGE* -- and return an uninterned copy of its name, so a
;;; later IMPORT of a symbol by that name signalled a conflict (glass's transports.lisp lists
;;; the names it imports as #:NAMEs; on modus every import failed).  Upstream ansi-test does
;;; not look at the package after a #:, so it passed either way.
(defvar *fails* 0)
(defmacro chk (name form)
  `(let ((v (handler-case ,form (error (c) (list :error c)))))
     (unless (eq v t) (setq *fails* (+ *fails* 1)) (format t "FAIL ~A => ~S~%" ,name v))))

(defpackage :sharp-colon-test (:use :cl))
(defpackage :sharp-colon-src (:use :cl) (:export #:exported-thing))
(defun rd (string) (let ((*package* (find-package :sharp-colon-test))) (read-from-string string)))
(defun fs (name) (find-symbol name :sharp-colon-test))

(chk "nothing-interned"
     (let ((r (rd "(#:zzqq-one #:|zzqq Two| #:zzqq-three)")))
       (and (equal (mapcar #'symbol-name r) '("ZZQQ-ONE" "zzqq Two" "ZZQQ-THREE"))
            (every (lambda (s) (null (symbol-package s))) r)
            (null (fs "ZZQQ-ONE")) (null (fs "zzqq Two")) (null (fs "ZZQQ-THREE")))))
(chk "number-and-dot-names"
     (equal (mapcar #'symbol-name (rd "(#:123 #:.)")) '("123" ".")))
(chk "import-after-sharp-colon"
     (progn (rd "#:exported-thing")
            (import (find-symbol "EXPORTED-THING" :sharp-colon-src) :sharp-colon-test)
            (eq (fs "EXPORTED-THING") (find-symbol "EXPORTED-THING" :sharp-colon-src))))
(chk "suppressed" (null (let ((*read-suppress* t)) (rd "#:abc"))))

(format t "~&sharp-colon-uninterned: ~A~%" (if (zerop *fails*) "PASS" (format nil "~D FAIL" *fails*)))
(sys-exit (if (zerop *fails*) 0 1))
