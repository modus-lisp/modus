;;; static-vector.lisp — MODUS:MAKE-STATIC-VECTOR: arrays outside the copying heap.
;;;   ./modus --script tests/static-vector.lisp
;;; A static vector must be an ordinary (simple-array ...) to every array path,
;;; zero-filled, and must keep its contents across collections (the collector
;;; only moves what points into from-space).  700 MB is more than a semispace.
(defvar *fails* 0)
(defun check (name ok) (format t "~&~:[FAIL~;PASS~] ~A~%" ok name) (unless ok (incf *fails*)))
(defvar *u* (modus:make-static-vector 700000000))
(defvar *f* (modus:make-static-vector 1000 :element-type 'single-float))
(check "u8: type and length" (and (typep *u* '(simple-array (unsigned-byte 8) (*))) (= (length *u*) 700000000)))
(check "f32: type, length, zero-filled" (and (typep *f* '(simple-array single-float (*))) (= (length *f*) 1000) (= (aref *f* 0) 0.0)))
(setf (aref *u* 0) 7 (aref *u* 699999999) 200 (aref *f* 999) 2.5)
(defun lane15 (v i) (declare (type (simple-array (unsigned-byte 8) (*)) v)) (%vi-umov8 (%vi-ld16 v i) 15))
(check "u8: aref and %vi-ld16" (and (= (aref *u* 0) 7) (= (lane15 *u* 699999984) 200)))
(let ((junk nil) (k 0))
  (dotimes (i 2000000) (push (make-list 3) junk) (incf k) (when (> k 100000) (setq junk nil k 0))))
(check "contents survive collections" (and (= (length *u*) 700000000) (= (aref *u* 699999999) 200) (= (aref *f* 999) 2.5)))
(format t "~%STATIC-VECTOR ~:[~D FAILURES~;ALL PASS~]~%" (zerop *fails*) *fails*)
