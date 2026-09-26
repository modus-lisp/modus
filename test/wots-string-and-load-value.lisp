;;; WITH-OUTPUT-TO-STRING with a target string appends to it and returns the
;;; BODY's values; LOAD returns T.
(defparameter *f* (format nil "/tmp/modus-wots-test-~d.lisp" (get-universal-time)))
(with-open-file (s *f* :direction :output :if-exists :supersede)
  (write-string "(defparameter *wots-loaded* 42)" s))
(defparameter *ok*
  (let ((str (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (and (equal (multiple-value-list
                 (with-output-to-string (s str) (write-string "ab" s) (values 1 2 3)))
                '(1 2 3))
         (string= str "ab")
         (equal (multiple-value-list
                 (with-output-to-string (*standard-output* str) (princ "cd") :v))
                '(:v))
         (string= str "abcd")
         (equal (with-output-to-string (s) (write-string "x" s)) "x")
         (eq (load *f*) t))))
(ignore-errors (delete-file *f*))
(format t "~&wots-string-and-load-value: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
