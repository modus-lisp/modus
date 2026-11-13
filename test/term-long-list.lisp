;;;; term-long-list.lisp -- a list is encoded, sized and decoded along its SPINE, so its length is not a
;;;; stack depth.  Recursing on the cdr took two frames an element, and a folder of 5000 rows sent to the iOS
;;;; UI in one call ran off an iPhone thread's stack (a memory fault: the app gone).  Also: dotted and nested
;;;; lists keep their bytes -- the spine walk writes exactly what the recursion wrote.
;;;;   ./modus --script test/term-long-list.lisp        (run from the modus checkout)

(load "test/term-harness.lisp")
(setf *buf* (make-array (* 8 1024 1024) :element-type '(unsigned-byte 8) :initial-element 0))

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name ok)
  (if ok
      (progn (incf *pass*) (format t "~&PASS ~A~%" name))
      (progn (incf *fail*) (format t "~&FAIL ~A~%" name))))

(defun wire-round-trip (value)
  (let ((n (term-encode value (%addr *buf*))))
    (setf (mem-ref (%addr *cell*) :u64) (%addr *buf*))
    (values (term-decode-step) n)))

;; the bytes of nested and dotted lists, by hand: tag 2 per cell, its car, then on down the cdr
(check "a dotted pair's bytes" (equal (term-hex '(1 . 2)) "0201000000000000000001020000000000000000"))
(check "a nested list's bytes"
       (equal (term-hex '((1) 2)) "020201000000000000000000020102000000000000000000"))
(dolist (v (list '(1 2 3) '(1 (2 (3 . 4)) . 5) '(nil nil) '("a" (:k "v") . "tail") '((((1))))))
  (multiple-value-bind (back n) (wire-round-trip v)
    (check (format nil "round trip ~S" v) (equal back v))
    (check (format nil "size of ~S predicted" v) (= n (term-size v)))))

(let ((long (loop for i below 200000 collect i)))
  (multiple-value-bind (back n) (wire-round-trip long)
    (check "200000 fixnums round trip" (equal back long))
    (check "200000 fixnums sized" (= n (term-size long) (+ 1 (* 10 200000))))))

(let ((rows (loop for i below 20000
                  collect (list :id i :text (format nil "file-~D.txt" i) :detail "0 bytes" :sort (list 1 i)))))
  (multiple-value-bind (back n) (wire-round-trip rows)
    (check "20000 rows (a big folder) round trip" (equal back rows))
    (check "20000 rows sized" (= n (term-size rows)))))

(format t "~&~D passed, ~D failed~%" *pass* *fail*)
