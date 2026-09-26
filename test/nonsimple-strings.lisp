;;; Fill-pointer / adjustable / displaced strings: READ-FROM-STRING and
;;; string streams read their CHARACTERS (not the array header), and they
;;; are not SIMPLE-STRINGs.
(defparameter *fp* (make-array 4 :element-type 'character :initial-contents "abcd" :fill-pointer 2))
(defparameter *ok*
  (and (equal (multiple-value-list (read-from-string (make-array 3 :element-type 'character :initial-contents "123" :fill-pointer 3))) '(123 3))
       (equal (multiple-value-list (read-from-string (make-array 3 :element-type 'character :displaced-to "x123" :displaced-index-offset 1))) '(123 3))
       (equal (multiple-value-list (read-from-string *fp*)) '(ab 2))
       (eql (read-char (make-string-input-stream *fp*)) #\a)
       (not (simple-string-p *fp*))
       (not (typep *fp* 'simple-string))
       (simple-string-p "abc")
       (typep "abc" 'simple-string)))
(format t "~&nonsimple-strings: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
