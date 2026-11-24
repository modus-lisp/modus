;;; mvm32.js: a list that outgrows the semispace must stop with "heap exhausted"
;;; (run with --semi 64), never come back short.  Before: 4000000 -> LEN 1022050.
(defun mk (n) (let ((l nil)) (dotimes (i n) (push i l)) l))
(dolist (n '(1000000 4000000 8000000 12000000 20000000))
  (let ((l (handler-case (mk n) (error (e) (format nil "ERR ~A" e)))))
    (format t "~&~D -> ~A~%" n (if (listp l) (list :len (length l) :ok (eql (length l) n) :first (car l)) l))))
