;;;; test/remove-linear.lisp -- REMOVE / REMOVE-IF / -IF-NOT / DELETE on long
;;;; sequences are linear, and still right.
;;;;
;;;;   ./modus --script test/remove-linear.lisp
;;;;
;;;; The family picks the indices to drop and then walks the sequence, and the
;;;; walk asked (MEMBER I TO-DROP) per element: O(length x drops).  A
;;;; REMOVE-IF-NOT keeping 10k of 60k list elements took 9 s (beacon's
;;;; reference-model query).  The semantics half compares against SBCL.

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name got want)
  (if (equalp got want) (incf *pass*)
      (progn (incf *fail*) (format t "~&FAIL ~A: got ~S want ~S~%" name got want))))

(check "semantics (SBCL's answers)"
       (list (remove-if #'evenp '(1 2 3 4 5 6)) (remove-if-not #'evenp '(1 2 3 4 5 6)) (remove 3 '(1 3 2 3 4 3))
             (remove 3 '(1 3 2 3 4 3) :count 2) (remove 3 '(1 3 2 3 4 3) :count 2 :from-end t)
             (remove-if #'evenp #(1 2 3 4 5 6)) (remove 3 #(1 3 2 3 4 3) :count 1 :from-end t) (remove #\a "banana")
             (remove-if #'evenp '(1 2 3 4 5 6) :start 1 :end 4)
             (let ((x (list 1 2 3 2 1))) (delete 2 x)) (let ((x (list 1 2 3 2 1))) (delete 1 x :count 1 :from-end t))
             (delete-if #'oddp (list 1 2 3 4 5)) (remove-if-not #'oddp #(1 2 3 4 5) :count 1))
       '((1 3 5) (2 4 6) (1 2 4) (1 2 4 3) (1 3 2 4) #(1 3 5) #(1 3 2 3 4) "bnn" (1 3 5 6) (1 3 1) (1 2 3 2) (2 4) #(1 3 4 5)))

(defun timed (thunk)
  (let ((t0 (get-internal-real-time)))
    (values (funcall thunk) (/ (- (get-internal-real-time) t0) internal-time-units-per-second))))
(let ((l (loop for i below 200000 collect i))
      (v (make-array 200000 :initial-element 1)))
  (multiple-value-bind (r s) (timed (lambda () (remove-if-not (lambda (x) (zerop (mod x 7))) l)))
    (check "remove-if-not 200k list" (length r) 28572)
    (check "...in under 2 s (was quadratic: minutes)" (< s 2) t))
  (multiple-value-bind (r s) (timed (lambda () (remove 1 v :end 150000)))
    (check "remove 150k of a vector" (length r) 50000)
    (check "...in under 2 s" (< s 2) t))
  (multiple-value-bind (r s) (timed (lambda () (delete-if #'oddp (copy-list l))))
    (check "delete-if 200k list" (length r) 100000)
    (check "...in under 2 s" (< s 2) t)))

(format t "~&remove-linear: ~D passed, ~D failed~%" *pass* *fail*)
(format t "~&~:[FAIL~;PASS~]~%" (zerop *fail*))
