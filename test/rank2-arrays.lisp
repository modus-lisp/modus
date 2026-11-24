;;; Two-subscript AREF and (SETF AREF) read and written in place in run-time compiled code (compiler.lisp
;;; %RANK2-ACCESS, and %MAKE-ARRAY-DTYPE's two dimensions and upgraded element types).  An array a MAKE-ARRAY
;;; of (LIST A B) provably made, or one declared (SIMPLE-ARRAY ET (* *)), was a call of %AREF2 per element:
;;; cl-bench 2D-ARRAYS 11x SBCL.  Subscripts out of range still signal; values are checked on SBCL too, and on
;;; modus the kernels' disassembly is checked to have no CALL.  Run: ./modus --script THIS
(defpackage :rank2-test (:use :cl))
(in-package :rank2-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error () :error))))
     (incf *checks*)
     (unless (equal got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))

(defvar *forms* (make-hash-table))
(defmacro defk (name args &body body)
  "DEFUN, keeping the lambda: modus's DISASSEMBLE has bytecode for a lambda form, not a DEFUN."
  `(progn (setf (gethash ',name *forms*) '(lambda ,args ,@body)) (defun ,name ,args ,@body)))
(defun %calls (form)
  #+modus (let ((s (with-output-to-string (*standard-output*) (disassemble form))) (n 0) (p 0))
            (unless (search "SET-NARGS" s) (return-from %calls :no-bytecode))
            (loop (let ((i (search "CALL 0" s :start2 p))) (unless i (return n)) (incf n) (setq p (1+ i)))))
  #-modus (progn form 0))
(defun calls-in (name)
  (let* ((form (gethash name *forms*)) (n (%calls form)) (base (%calls `(lambda ,(cadr form) nil))))
    (if (and (integerp n) (integerp base)) (- n base) (list n base))))

;; cl-bench's own shape: undeclared arrays from MAKE-ARRAY of (LIST N N), an integer-range element type
(defk sum3 (n)
  (declare (fixnum n))
  (let ((a (make-array (list n n) :element-type '(integer 0 1000) :initial-element 1))
        (b (make-array (list n n) :element-type '(integer 0 1000) :initial-element 2))
        (c (make-array (list n n) :element-type '(integer 0 2000))))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref c i j) (+ (aref a i j) (aref b i j) i (* 10 j)))))
    (let ((s 0)) (dotimes (i n) (dotimes (j n) (incf s (aref c i j)))) s)))
(check "sum3 7" (sum3 7) (+ (* 49 3) (* 7 21) (* 10 7 21)))
(check "sum3 1" (sum3 1) 3)

;; a non-square array: row length and column count are not confused
(defk rect (r c)
  (declare (fixnum r c))
  (let ((a (make-array (list r c) :initial-element 0)))
    (dotimes (i r) (dotimes (j c) (setf (aref a i j) (+ (* 100 i) j))))
    (list (aref a 0 (1- c)) (aref a (1- r) 0) (aref a (1- r) (1- c)) (array-dimensions a))))
(check "rect 3x5" (rect 3 5) (list 4 200 204 (list 3 5)))
(check "rect 5x3" (rect 5 3) (list 2 400 402 (list 5 3)))

;; out of range: the column, the row, a negative, a non-integer -- each an error, never another element
(defk probe (i j)
  (let ((a (make-array (list 3 4) :initial-element 7)))
    (aref a i j)))
(defk poke (i j)
  (let ((a (make-array '(3 4) :initial-element 7)))
    (setf (aref a i j) 9)
    (aref a i j)))
(check "in range" (list (probe 2 3) (poke 1 1)) (list 7 9))
(check "column past end" (probe 0 4) :error)
(check "row past end" (probe 3 0) :error)
(check "negative" (probe -1 0) :error)
(check "set column past end" (poke 0 4) :error)
(check "set row past end" (poke 3 0) :error)
(check "bignum" (probe (expt 2 70) 0) :error)

;; a declared (simple-array T (* *)), and the value of SETF AREF
(defk decl (a i j v)
  (declare (type (simple-array t (* *)) a) (fixnum i j))
  (list (setf (aref a i j) v) (aref a i j)))
(check "declared" (decl (make-array '(2 3) :initial-element nil) 1 2 :x) (list :x :x))

;; types that are not word stores keep the general path: characters, bits, floats
(defk chars ()
  (let ((a (make-array (list 2 2) :element-type 'character :initial-element #\a)))
    (setf (aref a 1 1) #\z) (list (aref a 0 0) (aref a 1 1))))
(check "character" (chars) (list #\a #\z))
(defk bits ()
  (let ((a (make-array (list 2 3) :element-type 'bit :initial-element 0)))
    (setf (aref a 1 2) 1) (list (aref a 1 2) (aref a 0 2))))
(check "bit" (bits) (list 1 0))

;; 1-D: an integer-range element type is a T array too
(defk v1 (n)
  (declare (fixnum n))
  (let ((v (make-array n :element-type '(integer 0 1000) :initial-element 5)) (s 0))
    (dotimes (i n) (incf s (aref v i))) s))
(check "1-D range" (v1 10) 50)

#+modus
(check "rank-2 AREF in place: one word load, no checked multiply"
       (let ((s (with-output-to-string (*standard-output*)
                  (disassemble '(lambda (a i j) (declare (type (simple-array t (* *)) a)) (aref a i j))))))
         (list (and (search "AREF VR" s) t) (and (search "MUL-CHECKED" s) t)))
       (list t nil))

(format t "rank2-arrays: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
