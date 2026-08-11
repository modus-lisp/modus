(defun say (k v) (princ (concatenate (quote string) k "=" (princ-to-string v))) (terpri) (finish-output))
(say "n1.toplevel-nested-bq" (handler-case (eval (read-from-string
  "(let ((ig (list 'g1 'g2))) `((declare (ignore ,@ig))))")) (t (c) (list :ERR c))))
(say "n2.defun-nested-bq" (handler-case (eval (read-from-string
  "(defun zbq (ig) `((declare (ignore ,@ig))))")) (t (c) (list :ERR c))))
(say "n3.call-nil" (handler-case (eval (read-from-string "(zbq nil)")) (t (c) (list :ERR c))))
(say "n4.call-list" (handler-case (eval (read-from-string "(zbq (list 'g1))")) (t (c) (list :ERR c))))
(say "n5.defun-outer-inner" (handler-case (eval (read-from-string
  "(defun zbq2 (formals ig body)
     `#'(lambda ,formals
          ,@(if ig `((declare (ignore ,@ig))))
          ,@body))")) (t (c) (list :ERR c))))
(say "n6.zbq2-nil" (handler-case (eval (read-from-string "(zbq2 nil nil (list '(list 1)))")) (t (c) (list :ERR c))))
(say "n7.zbq2-ig"  (handler-case (eval (read-from-string "(zbq2 (list 'g1) (list 'g1) (list '(list 1)))")) (t (c) (list :ERR c))))
(say "END" "ok")
