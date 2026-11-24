;;; The string hash (prelude.lisp %STRING-HASH: EQUAL tables and SXHASH).  Equal strings hash alike however they
;;; were made -- narrow, wide, adjustable, fill-pointered, through a stream -- and an EQUAL table finds a key by a
;;; copy of it; the hash spreads mill's tensor names (which differ only in their digits) like a random one.
;;; Run: ./modus --script THIS (also SBCL, where it checks SBCL's SXHASH).
(defpackage :string-hash-test (:use :cl))
(in-package :string-hash-test)
(defvar *fails* 0)
(defvar *checks* 0)
(defmacro check (name form expect)
  `(let ((got (handler-case ,form (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (equal got ,expect)
       (incf *fails*)
       (format t "FAIL ~a: got ~s want ~s~%" ,name got ,expect))))

(defun variants (s)
  (list (copy-seq s) (coerce (coerce s 'list) 'string) (concatenate 'string s "")
        (subseq (concatenate 'string "zz" s) 2) (with-output-to-string (o) (write-string s o))
        (make-array (length s) :element-type 'character :initial-contents s :adjustable t)
        (make-array (length s) :element-type 'character :initial-contents s :fill-pointer t)
        (if (every (lambda (c) (< (char-code c) 128)) s) (coerce s 'base-string) (copy-seq s))))

(check "equal strings, equal hashes"
       (loop for s in (list "" "a" "ab" "abc" "abcd" "abcde" "hello world" (make-string 7 :initial-element #\q)
                            (format nil "a~cz" (code-char 955)) (format nil "~c~c" (code-char 955) (code-char 8364))
                            "/encoder/encoders.3/layers.4/self_attn/MatMul_1234_output_0")
             nconc (loop for v in (variants s) unless (= (sxhash v) (sxhash s)) collect (list s (type-of v))))
       nil)
(check "sxhash a non-negative fixnum" (every (lambda (s) (typep (sxhash s) '(and fixnum (integer 0))))
                                             (list "" "x" (make-string 1000 :initial-element #\z)))
       t)

(defvar *names* (loop for i below 9000
                      collect (format nil "/encoder/encoders.~d/layers.~d/self_attn/MatMul_~d_output_0" (mod i 5) (mod i 7) i)))
(let ((h (make-hash-table :test #'equal)))
  (dolist (s *names*) (setf (gethash s h) s))
  (check "lookup by a copy" (every (lambda (s) (eq (gethash (copy-seq s) h) s)) *names*) t)
  (dolist (s (subseq *names* 0 4000)) (remhash (copy-seq s) h))
  (check "remhash by a copy" (list (hash-table-count h) (gethash (first *names*) h) (gethash (car (last *names*)) h))
         (list 5000 nil (car (last *names*)))))
(check "9000 names, 9000 hashes" (length (remove-duplicates (mapcar #'sxhash *names*))) 9000)
;; 9000 keys into 4096 buckets: a random hash uses 4096 (1 - e^-2.2) ~ 3643, the longest bucket ~ 9
(let ((counts (make-array 4096 :initial-element 0)))
  (dolist (s *names*) (incf (aref counts (logand (sxhash s) 4095))))
  (check "spread over the low bits" (list (> (count-if #'plusp counts) 3550) (< (reduce #'max counts) 16)) '(t t)))

(format t "string-hash: ~d checks, ~d failed~%" *checks* *fails*)
(if (zerop *fails*) (format t "PASS~%") (format t "FAIL~%"))
