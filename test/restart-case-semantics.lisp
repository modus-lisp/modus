;;; RESTART-CASE / RESTART-BIND semantics fixed together:
;;; clause bodies see (and mutate) the enclosing lexicals, :TEST is honoured,
;;; all values of the form come back, an outer ERROR handler does not see the
;;; internal invocation, &AUX in a clause lambda list, restarts print their
;;; report, and a RESTART-BIND restart may invoke itself recursively.
(defun rc-outer ()
  (block aborted
    (handler-bind ((error (lambda (c) (return-from aborted (list :outer (type-of c))))))
      (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'foo))))
        (restart-case (error "Boo!") (foo () 'good))))))
(defparameter *ok*
  (and (equal (let ((i 10)) (list (restart-case (progn (invoke-restart 'foo) 'bad)
                                     (foo () (incf i 100) 'good))
                                   i))
              '(good 110))
       (eq (restart-case (invoke-restart 'foo)
             (foo () :test (lambda (c) (declare (ignore c)) nil) 'bad)
             (foo () 'good))
           'good)
       (equal (multiple-value-list (restart-case (values 1 2 3) (foo () nil))) '(1 2 3))
       (eq (rc-outer) 'good)
       (eql (destructuring-bind (&aux (y 5)) nil y) 5)
       (equal (with-output-to-string (s)
                (restart-case (let ((*print-escape* nil)) (format s "~A" (find-restart 'foo)))
                  (foo () :report "A report")))
              "A report")
       (equal (let ((x 3) (y nil))
                (restart-bind ((foo (lambda () (when (> x 0) (push 'a y) (decf x) (invoke-restart 'foo)) y)))
                  (invoke-restart 'foo)))
              '(a a a))))
(format t "~&restart-case-semantics: ~a~%" (if *ok* "PASS" "FAIL"))
(sys-exit (if *ok* 0 1))
