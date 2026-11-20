;;;; test/backquote-vector.lisp -- commas inside a backquoted vector literal.
;;;;   modus --script test/backquote-vector.lisp   => BACKQUOTE-VECTOR PASS
;;;;
;;;; `#(1 ,x ,@ys) was data: the vector kept (COMMA X) and (COMMA-AT YS) as elements.  weft's
;;;; (defmacro %u16vec (&rest xs) `(coerce #(,@xs) ...)) built every DEFLATE table from that.
(defvar *fails* 0)
(defun check (name got want)
  (if (equalp got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defmacro vv (&rest xs) `(coerce #(,@xs) 'list))
(check "a spliced macro argument list" (vv 1 2 3) '(1 2 3))
(let ((x 5) (ys '(6 7)))
  (check "comma and splice in a vector" `#(1 ,x ,@ys 9) #(1 5 6 7 9))
  (check "a vector inside a list" `(a #(,x b)) '(a #(5 b)))
  (check "a vector with no comma stays as written" `#(1 2) #(1 2)))
(check "a string is not a vector template" `("a,b" ,(+ 1 2)) '("a,b" 3))
(format t "~&BACKQUOTE-VECTOR ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
