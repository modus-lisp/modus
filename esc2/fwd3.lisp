(defun say (k v) (princ (concatenate (quote string) k "=" (princ-to-string v))) (terpri) (finish-output))
(say "MK" (handler-case (eval (read-from-string "(defpackage \"ZTEST\" (:use \"COMMON-LISP\"))")) (t (c) (list :ERR c))))
(say "H1.def" (handler-case (eval (read-from-string
  "(eval-when (:compile-toplevel :execute)
     (defun ztest::ha1 (l) (list :int (integerp (function ztest::ha2))
                                 :res (mapcar (function ztest::ha2) l)))
     (defun ztest::ha2 (x) (+ x 100)))")) (t (c) (list :ERR c))))
(say "H1.run" (handler-case (eval (read-from-string "(ztest::ha1 (list 1 2))")) (t (c) (list :ERR c))))
(say "END" "ok")
