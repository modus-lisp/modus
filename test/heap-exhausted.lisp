;;;; test/heap-exhausted.lisp -- an allocation that does not fit after a full collection stops
;;;; the process, "heap exhausted", exit 137 (test/run-heap-exhausted.sh).  Before the sized
;;;; checks re-tested, it ran past the limit into memory the next collection copied over.
(defvar *keep* nil)
(defvar *live* (let ((l nil)) (dotimes (i 100000 l) (push (cons i (* i 3)) l))))
(defun check () (let ((bad 0)) (dolist (c *live*) (unless (and (consp c) (eql (cdr c) (* 3 (car c)))) (incf bad))) bad))
(dotimes (k 8) (push (make-array (* 2 1048576)) *keep*))   ; 8 x 16 MB live
(format t "~&live ~d MB bad ~d~%" (* 16 (length *keep*)) (check)) (finish-output)
(defun big () (make-array (* 13 1048576)))                   ; 104 MB at once
(let ((a (big))) (format t "~&got ~d bad ~d~%" (length a) (check)))
(push (big) *keep*)
(format t "~&second ~d bad ~d~%" (length (car *keep*)) (check))
