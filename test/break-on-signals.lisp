;;;; test/break-on-signals.lisp — *BREAK-ON-SIGNALS* enters BREAK before any handler, and BREAK
;;;; runs SB-EXT:*INVOKE-DEBUGGER-HOOK* with a CONTINUE restart (was: neither did anything).
;;;; Run: ./modus --load test/break-on-signals.lisp   Prints ALL PASS or N FAILED last.

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *fails*))
  (finish-output))

(defvar *seen* nil)
(defun recorder (c hook)
  (declare (ignore hook))
  (push (list (type-of c) (princ-to-string c) *break-on-signals* sb-ext:*invoke-debugger-hook*
              (and (find-restart 'continue) t))
        *seen*)
  (continue))

(setq *seen* nil)
(let ((sb-ext:*invoke-debugger-hook* #'recorder))
  (check "BREAK returns NIL through its CONTINUE restart" (null (break "stop ~d" 7)))
  (let ((r (first *seen*)))
    (check "and hands the hook a SIMPLE-CONDITION with its message"
           (and r (eq (first r) 'simple-condition) (string= (second r) "stop 7")))
    (check "with the hook bound to NIL while it runs" (and r (null (fourth r))))
    (check "and a CONTINUE restart in force" (and r (fifth r)))))

(setq *seen* nil)
(let ((sb-ext:*invoke-debugger-hook* #'recorder) (*break-on-signals* 'error))
  (check "the handler still gets the error afterwards"
         (eq (handler-case (error "swallowed") (error () :handled)) :handled))
  (check "but BREAK ran first, for the signalled error"
         (and (= (length *seen*) 1) (search "swallowed" (second (first *seen*)))))
  (check "with *BREAK-ON-SIGNALS* bound to NIL inside it" (null (third (first *seen*)))))

(setq *seen* nil)
(let ((sb-ext:*invoke-debugger-hook* #'recorder) (*break-on-signals* 'warning))
  (handler-case (error "not a warning") (error () nil))
  (check "a condition not of the type does not break" (null *seen*))
  (handler-case (signal 'simple-warning :format-control "w") (warning () nil))
  (check "SIGNAL of one that is does" (= (length *seen*) 1)))

(setq *seen* nil)
(let ((sb-ext:*invoke-debugger-hook* #'recorder) (*break-on-signals* '(or type-error simple-error)))
  (handler-case (car (read-from-string "x")) (error () nil))
  (check "a compound type and a runtime TYPE-ERROR" (eq (first (first *seen*)) 'simple-condition)))

(setq *seen* nil)
(let ((sb-ext:*invoke-debugger-hook* #'recorder) (*break-on-signals* 'error))
  (check "THROW is not a condition it breaks on" (and (eq (catch :k (throw :k :thrown)) :thrown) (null *seen*)))
  (check "nor INVOKE-RESTART" (and (eq (restart-case (invoke-restart 'go-on) (go-on () :restarted)) :restarted)
                                   (null *seen*))))

(setq *seen* nil)
(let ((*break-on-signals* nil) (sb-ext:*invoke-debugger-hook* #'recorder))
  (handler-case (error "plain") (error () nil))
  (check "off by default: NIL breaks on nothing" (null *seen*)))

;; modus has no interactive debugger: with no hook BREAK says so and returns, as continuing
;; would.  (SBCL enters its debugger here, so this one is modus's alone.)
(when (find-package "MODUS")
  (let ((*error-output* (make-string-output-stream)))
    (break "no hook ~a" "here")
    (check "with no hook BREAK writes the message and returns"
           (search "no hook here" (get-output-stream-string *error-output*)))))

(setq *seen* nil)
(let ((sb-ext:*invoke-debugger-hook* (lambda (c h) (declare (ignore h)) (push c *seen*) (throw :out :thrown))))
  (check "INVOKE-DEBUGGER runs the hook first"
         (and (eq (catch :out (invoke-debugger (make-condition 'simple-error :format-control "d"))) :thrown)
              (= (length *seen*) 1))))

(format t "~&~:[~d FAILED~;ALL PASS~]~%" (zerop *fails*) *fails*)
