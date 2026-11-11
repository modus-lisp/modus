;;;; stream-read-gc.lisp -- a thread blocked reading a SOCKET STREAM must not stop
;;;; the world from collecting.  Glass's window manager, in one process with a
;;;; WebRTC gateway, waited in READ-SEQUENCE for an RFB handshake; the gateway's
;;;; next collection spun main in the stop-the-world rendezvous forever, because
;;;; the stream path's read(2) was a bare syscall rather than a GC-safe one.
;;;; Run: timeout 300 ./modus --script test/stream-read-gc.lisp
;;;;   (prints PASS or FAIL; before the fix it does not return at all)
(require :sb-bsd-sockets)
(defvar *fails* 0)
(defun check (name ok)
  (unless ok (incf *fails*))
  (format t "~:[FAIL~;ok  ~] ~a~%" ok name)
  (finish-output))

;; As glass's window manager does it: the READER listens, accepts, and reads the
;; accepted socket through a bidirectional, fully buffered binary stream -- the
;; stream path down to read(2) (READ-SEQUENCE -> %FS-READ-U8-BULK -> %SYS-READ-RAW).
(let* ((port (+ 20000 (random 20000)))
       (reader (sb-thread:make-thread
                (lambda (port)
                  (let ((l (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                    (setf (sb-bsd-sockets:sockopt-reuse-address l) t)
                    (sb-bsd-sockets:socket-bind l #(127 0 0 1) port)
                    (sb-bsd-sockets:socket-listen l 1)
                    (let* ((c (sb-bsd-sockets:socket-accept l))
                           (io (sb-bsd-sockets:socket-make-stream
                                c :input t :output t :element-type '(unsigned-byte 8)
                                  :buffering :full))
                           (buf (make-array 16 :element-type '(unsigned-byte 8))))
                      (prog1 (read-sequence buf io)       ; blocks until main writes
                        (sb-bsd-sockets:socket-close c)
                        (sb-bsd-sockets:socket-close l)))))
                :arguments (list port) :name "stream-reader"))
       (s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
  (loop repeat 100
        until (ignore-errors (sb-bsd-sockets:socket-connect s #(127 0 0 1) port) t)
        do (sleep 0.05))
  (let ((out (sb-bsd-sockets:socket-make-stream s :output t :element-type '(unsigned-byte 8))))
    (sleep 0.5)                                         ; the reader is in read(2) now
    ;; Collections, each a stop-the-world rendezvous the blocked reader has to
    ;; take part in.
    (let ((kept 0)
          (gc0 (getf (modus-int::%gc-stats) :count)))
      (dotimes (i 3)
        (setq kept (length (make-list 20000 :initial-element i)))
        (sb-ext:gc :full t))
      (let ((gcs (- (getf (modus-int::%gc-stats) :count) gc0)))
        (check (format nil "main collected (~d times) while the reader was blocked in a socket stream read" gcs)
               (and (= kept 20000) (plusp gcs)))))
    (write-sequence (make-array 16 :element-type '(unsigned-byte 8) :initial-element 7) out)
    (finish-output out)
    (check "the reader then got its 16 bytes" (eql (sb-thread:join-thread reader) 16))
    (sb-bsd-sockets:socket-close s)))

(format t "~:[FAIL (~d)~;PASS~]~%" (zerop *fails*) *fails*)
