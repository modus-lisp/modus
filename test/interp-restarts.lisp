;;;; test/interp-restarts.lisp — conditions signalled in EVAL'd (interpreted) code reach
;;;; outer handlers with the stack, and its restarts, still standing.
;;;; Run: ./modus --load test/interp-restarts.lisp   Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defun resume-defining (thunk name)
  "Call THUNK; on UNDEFINED-FUNCTION NAME, define it and take CONTINUE."
  (handler-case
      (handler-bind ((undefined-function
                       (lambda (c)
                         (when (eq (cell-error-name c) name)
                           (setf (fdefinition name) (lambda (x) (list :defined x)))
                           (let ((r (find-restart 'continue c)))
                             (when r (invoke-restart r)))))))
        (funcall thunk))
    (error (e) (list :error (type-of e)))))

(fmakunbound 'ir-late-1)
(check "an undefined call in EVAL'd code offers CONTINUE, and resumes in place"
       (equal (resume-defining (lambda () (eval '(list :got (ir-late-1 7)))) 'ir-late-1)
              '(:got (:defined 7))))

(fmakunbound 'ir-late-2)
(check "and USE-VALUE"
       (equal (handler-bind ((undefined-function
                               (lambda (c) (use-value (lambda (x) (* x 10)) c))))
                (eval '(list :got (ir-late-2 4))))
              '(:got 40)))

(makunbound '*ir-unbound*)
(check "an unbound variable in EVAL'd code offers USE-VALUE"
       (equal (handler-bind ((unbound-variable (lambda (c) (use-value 5 c))))
                (eval '(list :got *ir-unbound*)))
              '(:got 5)))

(check "an outer HANDLER-BIND sees the condition before anything unwinds"
       (let ((seen nil))
         (catch :out
           (handler-bind ((error (lambda (c) (declare (ignore c))
                                   (setq seen (find-restart 'ir-inside))
                                   (throw :out nil))))
             (eval '(restart-case (error "boom") (ir-inside () :x)))))
         (and seen t)))

(check "EVAL'd HANDLER-CASE still catches its own error"
       (equal (eval '(handler-case (error "inner") (error (e) (list :inner (princ-to-string e)))))
              '(:inner "inner")))

(check "EVAL'd HANDLER-CASE with a non-matching clause lets the error out to ours"
       (eq (handler-case (eval '(handler-case (error "x") (type-error () :wrong)))
             (simple-error () :outer))
           :outer))

(check "an error EVAL'd code does not handle reaches an outer HANDLER-CASE"
       (eq (handler-case (eval '(car 5)) (type-error () :outer)) :outer))

(check "a hardware fault in EVAL'd code is caught by its own HANDLER-CASE"
       (eq (eval '(handler-case (mem-ref 8 :u64) (error () :caught))) :caught))

(check "and by ours when it has none"
       (eq (handler-case (eval '(mem-ref 8 :u64)) (error () :outer)) :outer))

(check "a WARN in EVAL'd code returns after an outer handler declines"
       (equal (handler-bind ((warning (lambda (c) (declare (ignore c)) nil)))
                (let ((*error-output* (make-broadcast-stream)))
                  (eval '(progn (warn "w") :after))))
              :after))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
