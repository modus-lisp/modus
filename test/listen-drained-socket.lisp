;;;; test/listen-drained-socket.lisp -- LISTEN on a socket stream answers what poll(2) does.
;;;;   ./modus --script test/listen-drained-socket.lisp      (exit 0 = pass)
;;;; A loopback pair: LISTEN must be NIL before anything is sent, T with a byte waiting, and
;;;; NIL again once it is read.  AArch64 images answered T on a drained socket (the
;;;; conservative %FD-INPUT-READY-P), which hung glass's one-thread RFB server.

(defvar *fails* 0)
(defun check (name got want)
  (format t "~&~:[FAIL~;ok  ~] ~a: ~s~%" (eq got want) name got)
  (unless (eq got want) (incf *fails*)))

(let* ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
  (sb-bsd-sockets:socket-bind server #(127 0 0 1) 0)
  (sb-bsd-sockets:socket-listen server 1)
  (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name server)))
         (client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect client #(127 0 0 1) port)
    (let* ((peer (sb-bsd-sockets:socket-accept server))
           (in (sb-bsd-sockets:socket-make-stream peer :input t :element-type '(unsigned-byte 8)))
           (out (sb-bsd-sockets:socket-make-stream client :output t :element-type '(unsigned-byte 8))))
      (check "listen, nothing sent" (and (listen in) t) nil)
      (write-byte 42 out) (finish-output out)
      (sleep 0.2)
      (check "listen, a byte waiting" (and (listen in) t) t)
      (check "read it" (eql (read-byte in) 42) t)
      (check "listen, drained again" (and (listen in) t) nil)
      (ignore-errors (close out)) (ignore-errors (close in))
      (ignore-errors (sb-bsd-sockets:socket-close server)))))
(format t "~&listen-drained-socket: ~:[FAIL~;PASS~]~%" (zerop *fails*))
(finish-output)
(modus-int::sys-exit (if (zerop *fails*) 0 1))
