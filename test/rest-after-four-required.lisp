;;; &rest after >= 4 required params, reached through FUNCALL / APPLY / a
;;; runtime-compiled direct call (all pass the true nargs, so the callee's own
;;; rest prologue must build the list -- it used to be skipped for req >= 4).
(defun r3 (a b c &rest r) (list a b c r))
(defun r4 (a b c d &rest r) (list a b c d r))
(defun r5 (a b c d e &rest r) (list a b c d e r))
(defun r6 (a b c d e f &rest r) (list a b c d e f r))
(defun %rest-direct () (list (r4 1 2 3 4) (r4 1 2 3 4 5) (r6 1 2 3 4 5 6 7 8)))
(defparameter *rest-ok*
  (and (equal (r3 1 2 3) '(1 2 3 nil))
       (equal (r4 1 2 3 4) '(1 2 3 4 nil))
       (equal (r4 1 2 3 4 5) '(1 2 3 4 (5)))
       (equal (r5 1 2 3 4 5 6 7) '(1 2 3 4 5 (6 7)))
       (equal (funcall #'r4 1 2 3 4) '(1 2 3 4 nil))
       (equal (apply #'r4 '(1 2 3 4 5 6)) '(1 2 3 4 (5 6)))
       (equal (apply #'r6 1 2 '(3 4 5 6 7)) '(1 2 3 4 5 6 (7)))
       (equal (%rest-direct) '((1 2 3 4 nil) (1 2 3 4 (5)) (1 2 3 4 5 6 (7 8))))
       (equal (merge 'list (list 1 3 7) (list 2 4) #'<) '(1 2 3 4 7))
       (equal (merge 'string (list #\1 #\3) "2" #'char<) "123")))
(format t "~&rest-after-four-required: ~a~%" (if *rest-ok* "PASS" "FAIL"))
(sys-exit (if *rest-ok* 0 1))
