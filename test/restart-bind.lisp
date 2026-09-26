;;; RESTART-BIND exists (it was UNDEFINED-FUNCTION in the CLI) and its frame
;;; is popped even when the body exits non-locally.
(defparameter *ok*
  (and (equal (multiple-value-list (restart-bind () (values 'a 'b 'c))) '(a b c))
       (equal (restart-bind ((foo (lambda (&rest a) (list :inv a))))
                (invoke-restart 'foo 1 2))
              '(:inv (1 2)))
       (eq (block nil (restart-bind ((foo (lambda () 1))) (return 'good))) 'good)
       (null (find-restart 'foo))
       (eql (with-condition-restarts nil nil 3) 3)))
(format t "~&restart-bind: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
