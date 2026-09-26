;;; ~( ... ~) nesting: an inner ~:( / ~@( must be recognised as nested, so
;;; the OUTER conversion covers the text after the inner ~).
(defparameter *ok*
  (and (string= (format nil "~(aBc ~:(def~) GHi~)") "abc def ghi")
       (string= (format nil "~@(aBc ~:(def~) GHi~)") "Abc def ghi")
       (string= (format nil "~:@(aBc ~(DEF~) GHi~)") "ABC DEF GHI")
       (string= (format nil "~:(~a b~)" "hi") "Hi B")))
(format t "~&format-nested-case: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
