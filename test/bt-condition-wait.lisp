;;;; test/bt-condition-wait.lisp -- bordeaux-threads' CONDITION-WAIT on modus keeps
;;;; its documented contract.
;;;;
;;;;   ./modus --script test/bt-condition-wait.lisp
;;;;
;;;; bordeaux-threads takes its GENERA backend on modus, whose condition wait (v2's
;;;; %CONDITION-WAIT, v1's CONDITION-WAIT) returned T after a TIMEOUT (the API
;;;; promises T only for a notification) and did not re-acquire the lock after one.
;;;; Fixed by a system quirk (net/asdf-interface.lisp, %QUIRK-BORDEAUX-THREADS).
;;;; Also checks each API's constructor makes its own CONDITION-VARIABLE: v1's
;;;; keyword constructor %MAKE-CONDITION-VARIABLE used to be a macro keyed by the
;;;; bare name, which rewrote v2's (mvm/compiler.lisp, DEFSTRUCT).  Runs the real
;;;; library, from the ladder tarballs.

(defvar *deps* (format nil "/tmp/bt-condition-wait-~d/" (random 1000000 (make-random-state t))))
(ensure-directories-exist *deps*)
(dolist (tar '("alexandria" "global-vars" "trivial-features" "trivial-garbage" "bordeaux-threads"))
  (uiop:run-program (list "tar" "xf" (namestring (truename (format nil "test/ladder/tars/~a.tar" tar)))
                          "-C" *deps*)))
(dolist (d (directory (concatenate 'string *deps* "*/"))) (push d asdf:*central-registry*))
(asdf:load-system "bordeaux-threads")

(defvar *fails* 0)
(defun chk (name got want)
  (format t "~&  ~a ~a: ~s~%" (if (equal got want) "ok  " "FAIL") name got)
  (unless (equal got want) (incf *fails*)))
(defun secs (t0) (/ (- (get-internal-real-time) t0) (* 1.0 internal-time-units-per-second)))

(defun waiter-thread (fn) (bt2:make-thread (lambda () (sleep 0.3) (funcall fn) 0)))

;;; Both APIs: v2 (BT2:) and the v1 compatibility API (BT:), which on modus is a
;;; second Genera backend with its own CONDITION-VARIABLE struct and its own copy of
;;; the wait.  Each is driven through its own lock and condition-variable.
(defmacro cv-suite (api make-lock make-cv with-lock wait notify try-lock release cv-type)
  `(progn
     (format t "~&~a~%" ,api)
     (let ((lock (,make-lock)) (cv (,make-cv)))
       ;; 0. the constructor makes ITS package's struct, not the other API's
       ;;    same-named one (modus once stamped every BT2 cv a BT::CONDITION-VARIABLE)
       (chk "the condition variable is of this API's type" (typep cv ',cv-type) t)
       ;; 1. a timeout says so, after the timeout, with the lock held again
       (let* ((t0 (get-internal-real-time))
              (r (,with-lock (lock)
                   (list (,wait cv lock :timeout 1)
                         ;; still ours: a second grab must fail
                         ,try-lock))))
         (chk "timed out: CONDITION-WAIT is NIL" (first r) nil)
         (chk "after about the timeout" (and (>= (secs t0) 0.9) (< (secs t0) 3)) t)
         (chk "and the lock is held again (another try fails)" (second r) nil))
       (chk "WITH-LOCK-HELD released it on the way out" ,try-lock t)
       (,release lock)
       ;; 2. a notification says so
       (let* ((th (waiter-thread (lambda () (,with-lock (lock) (,notify cv)))))
              (r (,with-lock (lock) (,wait cv lock :timeout 5))))
         (bt2:join-thread th)
         (chk "notified: CONDITION-WAIT is T" r t))
       ;; 3. a timed-out waiter is gone, so the next notify reaches the next waiter
       (,with-lock (lock) (,wait cv lock :timeout 0.2))
       (let* ((th (waiter-thread (lambda () (,with-lock (lock) (,notify cv)))))
              (r (,with-lock (lock) (,wait cv lock :timeout 5))))
         (bt2:join-thread th)
         (chk "a notify after a timed-out waiter still wakes the next one" r t)))))

;;; bt2's thread wrapper hands the result back by storing it into the THREAD
;;; instance, which is the creating thread's object: modus refused that store, so
;;; JOIN-THREAD returned NIL (and the worker died saying why).
(format t "~&BT2:JOIN-THREAD~%")
(chk "a worker's return values reach JOIN-THREAD"
     (multiple-value-list (bt2:join-thread (bt2:make-thread (lambda () (values (list 1 "two") 3)))))
     '((1 "two") 3))
(chk "and an error it dies of is signalled in the joiner"
     (handler-case (progn (bt2:join-thread (bt2:make-thread (lambda () (error "boom ~a" 7)))) :no-error)
       (bt2:abnormal-exit (c)
         (princ-to-string (bt2:abnormal-exit-condition c))))
     "boom 7")

(cv-suite "bordeaux-threads v2 (BT2:)" bt2:make-lock bt2:make-condition-variable
          bt2:with-lock-held bt2:condition-wait bt2:condition-notify
          (bt2:acquire-lock lock :wait nil) bt2:release-lock bt2::condition-variable)
(cv-suite "bordeaux-threads v1 (BT:)" bt:make-lock bt:make-condition-variable
          bt:with-lock-held bt:condition-wait bt:condition-notify
          (bt:acquire-lock lock nil) bt:release-lock bt::condition-variable)

(format t "~&BT-CONDITION-WAIT: ~a failed~%" *fails*)
(sys-exit (if (zerop *fails*) 0 1))
