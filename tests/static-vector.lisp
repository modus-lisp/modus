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
;; FREE-STATIC-VECTOR unmaps what MAKE-STATIC-VECTOR mapped and leaves anything
;; else alone; 40 x 400 MB allocate-and-free cycles reuse the space.  A freed
;; vector must not be used again -- not even passed to a function -- but a
;; reference left in a heap object does not trouble the collector.
(let* ((v (modus:make-static-vector 1000 :element-type 'single-float))
       (n (length modus::*static-vector-maps*)))
  (setf (aref v 3) 1.5)
  (check "free: a static vector" (and (= (aref v 3) 1.5) (modus:free-static-vector v)
                                      (= (length modus::*static-vector-maps*) (1- n)))))
(let ((holder (vector (modus:make-static-vector 64 :element-type 'single-float) 1)))
  (modus:free-static-vector (svref holder 0))
  (let ((junk nil) (k 0))
    (dotimes (i 2000000) (push (make-list 3) junk) (incf k) (when (> k 100000) (setq junk nil k 0))))
  (check "free: a dangling reference survives collections" (= (svref holder 1) 1)))
(check "free: not a heap vector" (not (modus:free-static-vector (make-array 10 :element-type 'single-float))))
(check "free: 40 cycles of 400 MB"
       (dotimes (i 40 t)
         (let ((v (modus:make-static-vector 100000000 :element-type 'single-float)))
           (setf (aref v 99999999) 1.0)
           (unless (and (= (aref v 99999999) 1.0) (modus:free-static-vector v)) (return nil)))))
(format t "~%STATIC-VECTOR ~:[~D FAILURES~;ALL PASS~]~%" (zerop *fails*) *fails*)
