;;;; listen-eof.lisp -- LISTEN on a socket stream whose peer sent bytes and closed.
;;;; CLHS: LISTEN is false at end of file.  sbcl --script / modus --script
(require :asdf) (require :sb-bsd-sockets)
(let* ((l (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
  (sb-bsd-sockets:socket-bind l #(127 0 0 1) 0) (sb-bsd-sockets:socket-listen l 1)
  (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name l)))
         (c (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect c #(127 0 0 1) port)
    (let* ((a (sb-bsd-sockets:socket-accept l))
           (out (sb-bsd-sockets:socket-make-stream c :output t :element-type '(unsigned-byte 8)))
           (in (sb-bsd-sockets:socket-make-stream a :input t :element-type '(unsigned-byte 8))))
      (write-sequence (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)) out)
      (finish-output out) (sb-bsd-sockets:socket-close c) (sleep 0.2)
      (let ((bytes (loop repeat 3 collect (read-byte in))))
        (format t "read ~a; then LISTEN => ~a (want NIL); READ-BYTE => ~a~%"
                bytes (listen in) (read-byte in nil :eof)))
      (sb-bsd-sockets:socket-close a) (sb-bsd-sockets:socket-close l))))
