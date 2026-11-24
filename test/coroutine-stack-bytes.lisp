;;;; test/coroutine-stack-bytes.lisp — SET-COROUTINE-STACK-BYTES before COROUTINES-START
;;;; sizes every coroutine's stack.  Run: ./modus --load test/coroutine-stack-bytes.lisp
;;;; Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun deep (n) (if (zerop n) 0 (+ 1 (deep (- n 1)))))
(defun refused-p (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))

(check "the default is 32 MB" (= (coroutine-stack-bytes) (* 32 1024 1024)))
(check "a size that is not a multiple of 4096 is refused"
       (refused-p (lambda () (set-coroutine-stack-bytes (+ (* 64 1024 1024) 1)))))
(check "and one below 1 MB" (refused-p (lambda () (set-coroutine-stack-bytes 4096))))
(check "256 MB is accepted before the first coroutine"
       (= (set-coroutine-stack-bytes (* 256 1024 1024)) (* 256 1024 1024)))
(let ((c (coroutine-create (lambda (n) (deep n)))))
  (check "a coroutine then recurses deeper than the default stack allowed"
         (eql (handler-case (coroutine-transfer c 1500000) (serious-condition () :exhausted)) 1500000)))
(check "and the size cannot change once coroutines are started"
       (refused-p (lambda () (set-coroutine-stack-bytes (* 64 1024 1024)))))
(check "and still reads what was set" (= (coroutine-stack-bytes) (* 256 1024 1024)))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
