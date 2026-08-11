(defun say (k v) (princ (concatenate (quote string) k "=" (princ-to-string v))) (terpri) (finish-output))
(say "S2" (handler-case (install-tarball "/home/claude/lf/tars/iterate.tar" "iterate") (t (c) :abort)))
(say "t1.read" (handler-case (eval (read-from-string
  "(read (make-string-input-stream \"(list iterate::!1)\") t nil t)")) (t (c) (list :ERR c))))
(say "t2.macroexpand" (handler-case (eval (read-from-string
  "(macroexpand (read (make-string-input-stream \"(list iterate::!1)\") t nil t))")) (t (c) (list :ERR c))))
(say "t3.bangvars" (handler-case (eval (read-from-string
  "(iterate::bang-vars (macroexpand (read (make-string-input-stream \"(list iterate::!1)\") t nil t)))")) (t (c) (list :ERR c))))
(say "t4.sort" (handler-case (eval (read-from-string
  "(sort (iterate::bang-vars (macroexpand (read (make-string-input-stream \"(list iterate::!1)\") t nil t))) (function <) :key (function iterate::bang-var-num))")) (t (c) (list :ERR c))))
(say "t5.full-let" (handler-case (eval (read-from-string
  "(let* ((form (read (make-string-input-stream \"(list iterate::!1)\") t nil t))
          (refd (sort (iterate::bang-vars (macroexpand form)) (function <) :key (function iterate::bang-var-num)))
          (nums (mapcar (function iterate::bang-var-num) refd))
          (maxn (if refd (car (last nums)) 0)))
     (list :form form :refd refd :nums nums :maxn maxn))")) (t (c) (list :ERR c))))
(say "t6.stage2" (handler-case (eval (read-from-string
  "(let* ((n-args 1)
          (all (loop for i from 1 to n-args collect (iterate::make-bang-var i)))
          (formals (mapcar (lambda (x) (declare (ignore x)) (gensym)) all)))
     (list :all all :formals formals))")) (t (c) (list :ERR c))))
(say "END" "ok")
