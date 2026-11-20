;;;; test/sb-sys-enable-interrupt.lisp -- SB-SYS:ENABLE-INTERRUPT :IGNORE / :DEFAULT are real.
;;;;   modus --script test/sb-sys-enable-interrupt.lisp   => SB-SYS-ENABLE-INTERRUPT PASS
;;;;
;;;; beacon does (sb-sys:enable-interrupt sb-unix:sigpipe :ignore).  After :IGNORE, a write to a
;;;; socket whose peer has closed comes back as an error (EPIPE), and the process lives.  After
;;;; :DEFAULT, the same write kills the process with SIGPIPE: shown in a child image (this same
;;;; binary, /proc/self/exe), which must NOT get to print its "survived" line.  A Lisp handler
;;;; function and the runtime's own fault signals are refused loudly.  Hosted Linux x86-64
;;;; (SB-EXT:RUN-PROGRAM for the child).
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defmacro safely (form) `(handler-case ,form (error (e) (list :error (princ-to-string e)))))

(defun broken-pipe-write ()
  "Connect to a loopback listener, have the peer close, then write until the kernel refuses.
   The errno of the refusal (EPIPE = 32; ECONNRESET = 104 can come first), or :NEVER."
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
    (sb-bsd-sockets:socket-bind server #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen server 1)
    (let ((port (nth-value 1 (sb-bsd-sockets:socket-name server))))
      (sb-bsd-sockets:socket-connect client #(127 0 0 1) port)
      (let ((peer (sb-bsd-sockets:socket-accept server)))
        (sb-bsd-sockets:socket-close peer)
        (sb-bsd-sockets:socket-close server)
        (sleep 0.2)
        (let ((buf (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 120))
              (errnos nil))
          (dotimes (i 50)
            (handler-case (sb-bsd-sockets:socket-send client buf nil)
              (sb-bsd-sockets:socket-error (e)
                (push (sb-bsd-sockets::socket-error-errno e) errnos)
                (when (eql (car errnos) 32) (return nil)))))
          (sb-bsd-sockets:socket-close client)
          (if errnos (car errnos) :never))))))

(defvar *child-mode* (sb-ext:posix-getenv "ENABLE_INTERRUPT_CHILD"))

(if *child-mode*
    ;; THE CHILD: SIGPIPE back to its default action, then the same write.
    (progn
      (sb-sys:enable-interrupt sb-unix:sigpipe :default)
      (broken-pipe-write)
      (format t "CHILD SURVIVED~%")
      (finish-output))
    (progn
      (check "SB-UNIX:SIGPIPE is 13" sb-unix:sigpipe 13)
      (check ":ignore returns NIL, as SBCL" (safely (sb-sys:enable-interrupt sb-unix:sigpipe :ignore)) nil)
      (check "after :ignore, a write to a closed peer is EPIPE and the process lives"
             (safely (broken-pipe-write)) 32)
      (check "a Lisp handler function is refused loudly"
             (let ((r (safely (sb-sys:enable-interrupt sb-unix:sigpipe (lambda (&rest a) a)))))
               (and (consp r) (eq (car r) :error) (search "only :IGNORE and :DEFAULT" (cadr r)) t))
             t)
      (check "SIGSEGV belongs to the runtime"
             (let ((r (safely (sb-sys:enable-interrupt 11 :default)))) (and (consp r) (eq (car r) :error)))
             t)
      (check "SIGKILL: rt_sigaction refuses, and so do we"
             (let ((r (safely (sb-sys:enable-interrupt 9 :ignore))))
               (and (consp r) (search "errno 22" (cadr r)) t))
             t)
      ;; THE CHILD: this binary again, on this file, with :DEFAULT.  Through /bin/sh so the
      ;; environment variable reaches it.
      (let* ((self (namestring (or *load-truename* "test/sb-sys-enable-interrupt.lisp")))
             (out (make-string-output-stream))
             (p (safely (sb-ext:run-program
                         "/bin/sh"
                         (list "-c" (format nil "ENABLE_INTERRUPT_CHILD=1 exec /proc/~D/exe --script '~A' </dev/null"
                                            (sb-posix:getpid) self))
                         :output out :error nil :wait t)))
             (text (get-output-stream-string out)))
        (check "after :default the child is killed by SIGPIPE (never prints CHILD SURVIVED)"
               (list (not (search "CHILD SURVIVED" text))
                     (and (not (consp p)) (sb-ext:process-exit-code p))
                     (and (not (consp p)) (sb-ext:process-status p)))
               (list t 13 :signaled))
        (unless (zerop *fails*) (format t "child output: ~S~%" text)))
      (format t "~&SB-SYS-ENABLE-INTERRUPT ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)))
