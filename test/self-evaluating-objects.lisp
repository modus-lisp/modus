;;; Objects the runtime represents as conses (packages, ...) are
;;; SELF-EVALUATING when spliced into code as literals (CLHS 3.1.2.1.3) --
;;; they used to be compiled as a function call.  Every upstream
;;; def-pprint-test binds *PACKAGE* to the package object itself.
(defparameter *pk* (find-package "KEYWORD"))
(defparameter *ok*
  (and (eq (eval *pk*) *pk*)
       (equal (eval (list 'let (list (list '*package* *pk*)) '(package-name *package*))) "KEYWORD")
       (equal (multiple-value-list
               (eval (list 'let (list (list '*package* *pk*)) '(loop for i from 1 to 3 unless t collect i))))
              '(nil))
       (eq (funcall (compile nil (list 'lambda () *pk*))) *pk*)))
(format t "~&self-evaluating-objects: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
