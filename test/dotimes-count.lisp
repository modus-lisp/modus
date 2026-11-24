;;; DOTIMES reading its count in place, and THE / SVREF as pure operands (compiler.lisp
;;; %DOTIMES-DIRECT-EXPANSION, %PURE-SIMPLE-EXPR-P).  A count that is a lexical variable the body never assigns
;;; is tested where it lives instead of through a copy; every case where it must still be copied is here, and
;;; the values are checked on SBCL as well.  Run: ./modus --script THIS
(defpackage :dotimes-count-test (:use :cl))
(in-package :dotimes-count-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error () :error))))
     (incf *checks*)
     (unless (equalp got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))

(defun plain (n) (let ((s 0)) (dotimes (i n s) (incf s i))))
(check "plain" (list (plain 10) (plain 0) (plain -3) (plain 1)) (list 45 0 0 0))
(defun result-var (n) (dotimes (i n i)))
(check "result sees the count" (list (result-var 7) (result-var -2)) (list 7 0))
(defun early (n) (dotimes (i n :none) (when (= i 3) (return (* i 10)))))
(check "RETURN" (list (early 10) (early 2)) (list 30 :none))
(defun with-tags (n) (let ((s 0)) (dotimes (i n s) (when (oddp i) (go skip)) (incf s i) skip)))
(check "tagbody" (with-tags 10) 20)

;; the count is evaluated once: a body that changes the variable does not change the trip count
(defun assigned (n) (let ((k 0)) (dotimes (i n k) (incf k) (when (= i 0) (setq n 100)))))
(check "count assigned in the body" (assigned 5) 5)
(defun closure-assigned (n) (let ((k 0) (f (lambda () (setq n 1000)))) (dotimes (i n k) (incf k) (when (= i 1) (funcall f)))))
(check "count assigned by a closure" (closure-assigned 4) 4)
(defvar *dyn-n* 3)
(defun special-shadow (n) (let ((k 0)) (dotimes (i n k) (declare (special n)) (incf k))))
(check "body declares the count special" (let ((*dyn-n* 99)) (special-shadow 6)) 6)

;; THE and SVREF operands keep their values across the other operand's evaluation
(defun xor-words (a b)
  (declare (type simple-vector a b) (optimize (speed 3) (safety 0)))
  (let ((r (make-array (length a))) (n (length a)))
    (declare (type fixnum n))
    (dotimes (j n r) (setf (svref r j) (logxor (the fixnum (svref a j)) (the fixnum (svref b j)))))))
(check "svref operands" (xor-words (vector 1 2 3 65535) (vector 3 2 1 255)) (vector 2 0 2 65280))
(defun sum-the (v) (declare (type simple-vector v)) (let ((s 0)) (dotimes (i (length v) s) (setq s (+ (the fixnum (svref v i)) (the fixnum s))))))
(check "THE operands" (sum-the (vector 5 6 7)) 18)
(check "bit ops" (let ((a (make-array 40 :element-type 'bit :initial-element 1)) (b (make-array 40 :element-type 'bit :initial-element 0)))
                   (setf (sbit b 3) 1 (sbit b 39) 1)
                   (list (count 1 (bit-xor a b)) (count 1 (bit-and a b)) (count 1 (bit-nand a b)) (sbit (bit-ior a b) 39)))
       (list 38 2 38 1))

(format t "dotimes-count: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
