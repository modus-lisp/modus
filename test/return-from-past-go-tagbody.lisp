;;;; test/return-from-past-go-tagbody.lisp — RETURN-FROM out of a closure, past a TAGBODY a
;;;; closure can GO to
;;;; Run: ./modus --load test/return-from-past-go-tagbody.lisp
;;;; Prints one line per check and ALL PASS or N FAILED last.
;;;;
;;;; A BLOCK whose body had a GO inside a lambda got no catch frame, and a RETURN-FROM it from
;;;; inside a lambda fell back to returning from that lambda: the block went on as if nothing
;;;; had happened.  Silently, with the wrong value.  HANDLER-CASE's standard expansion (a
;;;; handler that GOes to a clause, a body that returns from the block) is exactly this shape.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(check "RETURN-FROM in a closure, past a TAGBODY another closure GOes into"
       (eql (eval '(block n
                     (tagbody (let ((f (lambda () (go x))))
                                (funcall (lambda () f (return-from n 1))))
                      x)
                     :fell-through))
            1))

(check "the HANDLER-CASE shape: from HANDLER-BIND's body, past a handler's GO"
       (eql (eval '(block n
                     (tagbody (handler-bind ((error (lambda (c) c (go x))))
                                (return-from n 1))
                      x)
                     :fell-through))
            1))

(check "with multiple values"
       (equal (eval '(multiple-value-list
                      (block n
                        (tagbody (handler-bind ((error (lambda (c) c (go x))))
                                   (return-from n (values 1 2)))
                         x))))
              '(1 2)))

(check "the handler's GO still lands"
       (eq (eval '(block n
                    (tagbody (handler-bind ((error (lambda (c) c (go x))))
                               (error "e"))
                     x (return-from n :went))
                    :fell-through))
           :went))

(check "a normal exit still falls through"
       (eq (eval '(block n
                    (tagbody (handler-bind ((error (lambda (c) c (go x)))) 1) x)
                    :fell-through))
           :fell-through))

(check "compiled in a DEFUN"
       (progn
         (eval '(defun rfpgt () (block n
                                  (tagbody (let ((f (lambda () (go x))))
                                             (funcall (lambda () f (return-from n 1))))
                                   x)
                                  :fell-through)))
         (eql (funcall 'rfpgt) 1)))

(check "from FLET and LABELS functions"
       (equal (eval '(list (block n (tagbody (flet ((g () (go x)) (r () (return-from n :flet)))
                                               (if nil (g) (r)))
                                     x)
                                  :fell-through)
                           (block n (tagbody (labels ((g () (go x)) (r () (return-from n :labels)))
                                               (if nil (g) (r)))
                                     x)
                                  :fell-through)))
              '(:flet :labels)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
