;;; EQ and EQL tables of object keys (prelude.lisp %HTD-EQ-OBJECT-FIND, %HTD-REUSE-DEAD; cl-eval.lisp NUMBERP).
;;; Keys of every kind an address index holds -- vectors, conses, strings, packed float arrays -- found across
;;; collections that move them all; REMHASH, and a key put back into its removed entry; a table that loses and
;;; gains keys does not grow; NUMBERP of every kind of number and of the vectors that share a complex's subtag.
;;; Run: ./modus --script THIS (also SBCL).
(defpackage :eq-table-test (:use :cl))
(in-package :eq-table-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (equal got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))

(defvar *n* 4000)
(defvar *objs* (coerce (loop for i below *n*
                             collect (case (mod i 4)
                                       (0 (make-array 4 :element-type 'single-float))
                                       (1 (list i))
                                       (2 (format nil "s~d" i))
                                       (3 (make-array 2))))
                       'vector))
(defun churn () (let ((junk nil)) (dotimes (j 200000) (push (make-array 8) junk)) (length junk)))

(let ((h (make-hash-table :test #'eq)) (l (make-hash-table :test #'eql)))
  (dotimes (i *n*) (setf (gethash (aref *objs* i) h) i (gethash (aref *objs* i) l) i))
  (dotimes (r 3)
    (churn)                                             ; every key moves
    (check (format nil "found after collection ~d" r)
           (loop for i below *n* count (not (and (eql (gethash (aref *objs* i) h) i) (eql (gethash (aref *objs* i) l) i))))
           0)
    (check (format nil "absent after collection ~d" r)
           (list (gethash (list 1) h) (gethash (copy-seq "s2") h) (gethash (make-array 2) l))
           '(nil nil nil)))
  (dotimes (i *n*) (when (evenp i) (remhash (aref *objs* i) h)))
  (check "remhash half" (list (hash-table-count h)
                              (loop for i below *n* count (not (eql (gethash (aref *objs* i) h) (if (evenp i) nil i)))))
         (list (/ *n* 2) 0))
  (dotimes (i *n*) (when (evenp i) (setf (gethash (aref *objs* i) h) (- i))))
  (churn)
  (check "put back" (list (hash-table-count h)
                          (loop for i below *n* count (not (eql (gethash (aref *objs* i) h) (if (evenp i) (- i) i)))))
         (list *n* 0)))

;; a table that loses and gains its keys over and over reuses its removed entries: same answers, and it ends
;; as many as it holds
(let ((h (make-hash-table :test #'eq)) (keys (loop for i below 500 collect (make-array 2))))
  (dolist (k keys) (setf (gethash k h) 0))
  (dotimes (r 200)
    (dolist (k keys) (remhash k h))
    (when (= r 100) (churn))
    (dolist (k keys) (setf (gethash k h) r)))
  (check "churned table" (list (hash-table-count h) (every (lambda (k) (eql (gethash k h) 199)) keys)) '(500 t)))

(check "numberp"
       (mapcar #'numberp (list 1 (expt 2 70) (- (expt 2 70)) (/ 1 3) 1.5 1.5d0 (complex 1 2) (complex 1.0 2.0)
                               (vector 1 2 3) (vector 1 2) #\a nil 'x "s" (list 1)
                               (make-array 3 :element-type 'single-float)))
       '(t t t t t t t t nil nil nil nil nil nil nil nil))

(format t "eq-table: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
