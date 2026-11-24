;;; Float comparisons in generic code: < > <= >= = /= ZEROP on boxed floats, through NUMERIC-EQUAL-P and
;;; NUMERIC-VALUE-LESS-P (cl-types.lisp %FLOAT-LT-P / %FLOAT-EQ-P, native where the target has the fpd ops).
;;; NaN unordered, +0 = -0, infinities, singles against doubles and against integers.  Every result is the same
;;; table on SBCL.  Run: ./modus --script THIS
(defpackage :float-compare-test (:use :cl))
(in-package :float-compare-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error () :error))))
     (incf *checks*)
     (unless (equalp got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))
(defun inf () #+sbcl (sb-int:with-float-traps-masked (:overflow) (* most-positive-double-float 2d0))
              #-sbcl (* most-positive-double-float 2d0))
(defun nan () #+sbcl (sb-int:with-float-traps-masked (:invalid) (- (inf) (inf)))
              #-sbcl (- (inf) (inf)))
(defmacro masked (form) #+sbcl `(sb-int:with-float-traps-masked (:invalid :overflow) ,form) #-sbcl form)
(defvar *vals* (list 0 1 -1 0.0 -0.0 0d0 -0d0 1.5 1.5d0 -1.5 2.5d0 0.1 0.1d0 1/2 (expt 2 60) most-positive-double-float))
;; untyped two-argument functions: the generic paths, not the open-coded fixnum compare
(defun lt (a b) (< a b)) (defun gt (a b) (> a b)) (defun le (a b) (<= a b)) (defun ge (a b) (>= a b))
(defun eqn (a b) (= a b)) (defun ne (a b) (/= a b)) (defun zp (a) (zerop a))
(defun table (vals)
  (loop for a in vals collect
    (loop for b in vals collect (list (lt a b) (gt a b) (le a b) (ge a b) (eqn a b) (ne a b)))))
(check "finite table"
       (table *vals*)
       '#.(let ((vals (list 0 1 -1 0.0 -0.0 0d0 -0d0 1.5 1.5d0 -1.5 2.5d0 0.1 0.1d0 1/2 (expt 2 60) most-positive-double-float)))
            (loop for a in vals collect
              (loop for b in vals collect (list (< a b) (> a b) (<= a b) (>= a b) (= a b) (/= a b))))))
(check "zerop" (mapcar #'zp *vals*) (list t nil nil t t t t nil nil nil nil nil nil nil nil nil))
(let ((inf (inf)) (nan (nan)))
  (check "inf" (list (lt 1d0 inf) (gt inf 1d300) (eqn inf inf) (lt inf inf) (le inf inf) (lt (- inf) inf) (lt 5 inf) (gt (- inf) -5))
         (list t t t nil t t t nil))
  (check "nan unordered" (masked (list (lt nan 1d0) (gt nan 1d0) (le nan nan) (ge nan 0d0) (eqn nan nan) (ne nan nan) (zp nan) (lt 1 nan)))
         (list nil nil nil nil nil t nil nil)))
(check "single vs double exact" (list (eqn 0.1 0.1d0) (lt 0.1d0 0.1) (eqn 0.5 0.5d0)) (list nil t t))

(format t "float-compare: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
