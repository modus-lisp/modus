;;;; test/shadowed-branch-heads.lisp -- a function shadowing a CL predicate is
;;;; called when it is an IF test, as it is when it is a value (SBCL's answer).
;;;; Run: ./modus --load test/shadowed-branch-heads.lisp

(defpackage :sbh (:use :cl)
  (:shadow #:characterp #:consp #:zerop #:not #:eq #:null)
  (:export #:characterp #:consp #:zerop #:not #:eq #:null))
(defpackage :sbh-user (:use :sbh :cl)
  (:shadowing-import-from :sbh #:characterp #:consp #:zerop #:not #:eq #:null))

(defvar cl-user::*fails* 0)
(defun cl-user::check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf cl-user::*fails*))
  (finish-output))

;; Each answers the OPPOSITE of CL's on the arguments below, so a test that
;; runs CL's instead takes the wrong branch.
(defun sbh:characterp (x) (or (cl:characterp x) (and (integerp x) (>= x 0))))
(defun sbh:consp (x) (cl:null (cl:consp x)))
(defun sbh:zerop (x) (= x 1))
(defun sbh:not (x) x)
(defun sbh:eq (a b) (cl:not (cl:eq a b)))
(defun sbh:null (x) x)

(in-package :sbh-user)

(defun t-char (x) (if (characterp x) :yes :no))
(defun t-cons (x) (if (consp x) :yes :no))
(defun t-zerop (x) (if (zerop x) :yes :no))
(defun t-not (x) (if (not x) :yes :no))
(defun t-eq (a b) (if (eq a b) :yes :no))
(defun t-null (x) (if (null x) :yes :no))
(defun t-when-and (x) (when (and (characterp x) (zerop 1)) :yes))
(defun v-char (x) (characterp x))

(dolist (round '("interpreted or first compile" "after many calls"))
  (when (string= round "after many calls")
    (dotimes (i 3000) (t-char 0) (t-cons 1) (t-zerop 1) (t-not 1) (t-eq 1 2) (t-null 1) (t-when-and 0)))
  (cl-user::check (format nil "~a: (IF (CHARACTERP 0) ...) calls the package's CHARACTERP" round) (cl:eq (t-char 0) :yes))
  (cl-user::check (format nil "~a: (IF (CONSP 1) ...)" round) (cl:eq (t-cons 1) :yes))
  (cl-user::check (format nil "~a: (IF (ZEROP 1) ...)" round) (cl:eq (t-zerop 1) :yes))
  (cl-user::check (format nil "~a: (IF (NOT 1) ...)" round) (cl:eq (t-not 1) :yes))
  (cl-user::check (format nil "~a: (IF (EQ 1 2) ...)" round) (cl:eq (t-eq 1 2) :yes))
  (cl-user::check (format nil "~a: (IF (NULL 1) ...)" round) (cl:eq (t-null 1) :yes))
  (cl-user::check (format nil "~a: (WHEN (AND (CHARACTERP 0) (ZEROP 1)) ...)" round) (cl:eq (t-when-and 0) :yes))
  (cl-user::check (format nil "~a: and as a value, as before" round) (cl:eq (v-char 0) t)))

(in-package :cl-user)
(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
