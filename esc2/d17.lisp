(defun say (k v) (princ (concatenate (quote string) k "=" (princ-to-string v))) (terpri) (finish-output))
(say "S2" (handler-case (install-tarball "/home/claude/lf/tars/iterate.tar" "iterate") (t (c) :abort)))
(say "u1.mapcan-closure" (handler-case (eval (read-from-string
  "(let* ((refd (list (iterate::make-bang-var 1)))
          (all  (list (iterate::make-bang-var 1)))
          (formals (list (gensym))))
     (mapcan (lambda (v tv) (unless (member v refd) (list tv))) all formals))")) (t (c) (list :ERR c))))
(say "u2.mapcan-when" (handler-case (eval (read-from-string
  "(let* ((refd (list (iterate::make-bang-var 1)))
          (all  (list (iterate::make-bang-var 1)))
          (formals (list (gensym))))
     (mapcan (lambda (v tv) (when (member v refd) (list (list v tv)))) all formals))")) (t (c) (list :ERR c))))
(say "u3.backquote" (handler-case (eval (read-from-string
  "(let* ((form (read (make-string-input-stream \"(list iterate::!1)\") t nil t))
          (refd (list (iterate::make-bang-var 1)))
          (all  (list (iterate::make-bang-var 1)))
          (formals (list (gensym))))
     `#'(lambda ,formals
          ,@(let ((ig (mapcan (lambda (v tv) (unless (member v refd) (list tv))) all formals)))
              (if ig `((declare (ignore ,@ig)))))
          (symbol-macrolet ,(mapcan (lambda (v tv) (when (member v refd) (list (list v tv)))) all formals)
            ,@(if (iterate::list-of-forms? form) form (list form)))))")) (t (c) (list :ERR c))))
(say "u4.lof" (handler-case (eval (read-from-string
  "(iterate::list-of-forms? (read (make-string-input-stream \"(list iterate::!1)\") t nil t))")) (t (c) (list :ERR c))))
(say "END" "ok")
