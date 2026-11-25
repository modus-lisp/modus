;;; READ-FROM-STRING with :START reads in place (no copy of the rest): the
;;; forms and the positions must be exactly what the copying path gave.
(defun forms (str &rest keys)
  (let ((pos 0) (out nil))
    (loop (multiple-value-bind (f p) (apply #'read-from-string str nil :eof :start pos keys)
            (when (eq f :eof) (return (nreverse out)))
            (push (list f p) out) (setq pos p)))))
(print (list :seq (forms "(a b) foo   \"str\" 42 #(1 2) 'q ;c
 :kw")))
(print (list :pw (forms "x y  z" :preserve-whitespace t)))
(print (list :macro (multiple-value-list (read-from-string "abc(def)" t nil :start 0)) (multiple-value-list (read-from-string "zz abc(def)" t nil :start 3))))
(print (list :end (multiple-value-list (read-from-string "abcdef ghi" t nil :start 2 :end 4))))
(print (list :mid (multiple-value-list (read-from-string "123 456" t nil :start 1))))
(print (list :atend (multiple-value-list (read-from-string "abc" nil :eof :start 3))))
(print (list :eoferr (handler-case (read-from-string "abc" t nil :start 3) (end-of-file () :eof-error))))
(print (list :beyond (handler-case (read-from-string "abc" t nil :start 9) (error () :error))))
(let ((fp (make-array 10 :element-type 'character :fill-pointer 7 :initial-contents "x yy zzzww")))
  (print (list :fill (forms fp))))
