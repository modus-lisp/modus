(defun say (k v) (princ (concatenate (quote string) k "=" (princ-to-string v))) (terpri) (finish-output))
;; F1: forward #'fn in one eval-when module, used by MAPCAR (native HOF)
(say "F1.def" (handler-case (eval (read-from-string
  "(eval-when (:compile-toplevel :execute)
     (defun fa1 (l) (mapcar (function fa2) l))
     (defun fa2 (x) (+ x 100)))")) (t (c) (list :ERR c))))
(say "F1.run" (handler-case (eval (read-from-string "(fa1 (list 1 2 3))")) (t (c) (list :ERR c))))
;; F2: forward ref used by SORT :key and SOME
(say "F2.def" (handler-case (eval (read-from-string
  "(eval-when (:compile-toplevel :execute)
     (defun fb1 (l) (list (sort (copy-list l) (function <) :key (function fb2))
                          (some (function fb3) l)))
     (defun fb2 (x) (- 0 x))
     (defun fb3 (x) (if (> x 2) x nil)))")) (t (c) (list :ERR c))))
(say "F2.run" (handler-case (eval (read-from-string "(fb1 (list 1 3 2))")) (t (c) (list :ERR c))))
;; F3: backward ref (already-compiled) must still work
(say "F3.def" (handler-case (eval (read-from-string
  "(eval-when (:compile-toplevel :execute)
     (defun fc2 (x) (* x 2))
     (defun fc1 (l) (mapcar (function fc2) l)))")) (t (c) (list :ERR c))))
(say "F3.run" (handler-case (eval (read-from-string "(fc1 (list 1 2 3))")) (t (c) (list :ERR c))))
;; F4: out-of-module #'NATIVE must still resolve to the native fn object
(say "F4.def" (handler-case (eval (read-from-string
  "(defun fd1 (l) (mapcar (function car) l))")) (t (c) (list :ERR c))))
(say "F4.run" (handler-case (eval (read-from-string "(fd1 (list (cons 1 2) (cons 3 4)))")) (t (c) (list :ERR c))))
;; F5: mutual recursion across the module
(say "F5.def" (handler-case (eval (read-from-string
  "(eval-when (:compile-toplevel :execute)
     (defun fe-even (n) (if (= n 0) t (funcall (function fe-odd) (- n 1))))
     (defun fe-odd (n) (if (= n 0) nil (funcall (function fe-even) (- n 1)))))")) (t (c) (list :ERR c))))
(say "F5.run" (handler-case (eval (read-from-string "(list (fe-even 4) (fe-odd 4))")) (t (c) (list :ERR c))))
;; F6: forward ref, function called LATER from a fresh eval (trampoline path)
(say "F6.run" (handler-case (eval (read-from-string "(mapcar (function fa2) (list 1 2))")) (t (c) (list :ERR c))))
;; F7: self reference
(say "F7.def" (handler-case (eval (read-from-string
  "(defun ff1 (l) (if (null l) nil (cons (car l) (funcall (function ff1) (cdr l)))))")) (t (c) (list :ERR c))))
(say "F7.run" (handler-case (eval (read-from-string "(ff1 (list 1 2 3))")) (t (c) (list :ERR c))))
;; F8: eval-when with only :compile-toplevel (scan sees it, toplevel compiles it as execute)
(say "F8.def" (handler-case (eval (read-from-string
  "(eval-when (:compile-toplevel)
     (defun fg1 (l) (mapcar (function fg2) l))
     (defun fg2 (x) (list x)))")) (t (c) (list :ERR c))))
(say "F8.run" (handler-case (eval (read-from-string "(fg1 (list 1 2))")) (t (c) (list :ERR c))))
(say "END" "ok")
