;;;; test/sb-surface-gaps.lisp -- six SBCL-surface gaps found moving cl-consensus and cl-payments
;;;; onto one net thread; each line must read the same under SBCL and modus
;;;; (test/run-sb-surface-gaps.sh compares them).
;;;;   1 WITH-RECURSIVE-LOCK :WAIT-P NIL on a lock another thread holds: NIL at once (modus blocked)
;;;;   2 SB-THREAD:TRY-SEMAPHORE (modus: undefined)
;;;;   3 SOCKET-SEND :DONTWAIT (modus: unknown keyword)
;;;;   4 WAIT-UNTIL-FD-USABLE fd :OUTPUT 0 on a full socket: NIL (modus: T)
;;;;   5 a non-blocking SOCKET-SEND on a full socket: NIL (modus: SOCKET-ERROR)
;;;;   6 CL_SOURCE_REGISTRY read on first use (modus: only after INITIALIZE-SOURCE-REGISTRY);
;;;;     SYSTEM-SOURCE-DIRECTORY a pathname (modus: a string)
(require :asdf) (require :sb-bsd-sockets)
(defvar *m* (sb-thread:make-mutex :name "held"))
(defvar *r* :unset)
(defun try-held () (sb-thread:with-recursive-lock (*m* :wait-p nil) :ran-body))
(sb-thread:grab-mutex *m*)
(let ((th (sb-thread:make-thread #'try-held)))
  (sleep 0.5)
  (format t "1 with-recursive-lock :wait-p nil: ~:[still blocked~;~s~]~%"
          (not (sb-thread:thread-alive-p th)) (and (not (sb-thread:thread-alive-p th)) (sb-thread:join-thread th)))
  (sb-thread:release-mutex *m*) (sb-thread:join-thread th))
(format t "1 recursive re-entry by the holder: ~s~%"
        (sb-thread:with-recursive-lock (*m*) (sb-thread:with-recursive-lock (*m* :wait-p nil) :inner)))
(let ((s (sb-thread:make-semaphore)))
  (format t "2 try-semaphore empty: ~s~%" (sb-thread:try-semaphore s))
  (sb-thread:signal-semaphore s 2)
  (format t "2 try-semaphore: ~s then ~s then ~s~%" (sb-thread:try-semaphore s) (sb-thread:try-semaphore s) (sb-thread:try-semaphore s)))
(defun pair ()
  (let ((l (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (c (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-bind l #(127 0 0 1) 0) (sb-bsd-sockets:socket-listen l 4)
    (sb-bsd-sockets:socket-connect c #(127 0 0 1) (nth-value 1 (sb-bsd-sockets:socket-name l)))
    (let ((a (sb-bsd-sockets:socket-accept l))) (sb-bsd-sockets:socket-close l) (list c a))))
(let* ((p (pair)) (c (first p)) (buf (make-array 65536 :element-type '(unsigned-byte 8))) (sent 0))
  (format t "3 socket-send :dontwait on an empty socket: ~s~%" (sb-bsd-sockets:socket-send c buf 10 :dontwait t))
  (format t "3 ... and the socket is still blocking: ~s~%" (sb-bsd-sockets:non-blocking-mode c))
  (loop repeat 4000 for n = (sb-bsd-sockets:socket-send c buf 65536 :dontwait t)
        while (integerp n) do (incf sent n))
  (format t "3 :dontwait fills the socket, then: ~s (more than 64 KB went first: ~s)~%"
          (sb-bsd-sockets:socket-send c buf 65536 :dontwait t) (> sent 65536))
  (format t "4 wait-until-fd-usable :output 0 on the full socket: ~s~%"
          (sb-sys:wait-until-fd-usable (sb-bsd-sockets:socket-file-descriptor c) :output 0))
  (format t "4 ... and :input 0 on its empty peer's side: ~s~%"
          (sb-sys:wait-until-fd-usable (sb-bsd-sockets:socket-file-descriptor c) :input 0))
  (setf (sb-bsd-sockets:non-blocking-mode c) t)
  (format t "5 non-blocking socket-send on the full socket: ~s~%"
          (handler-case (sb-bsd-sockets:socket-send c buf 65536) (error (e) (list :signalled (type-of e)))))
  (mapc #'sb-bsd-sockets:socket-close p))
(let* ((dir (sb-ext:posix-getenv "SR_TEST_DIR")))
  (format t "6 CL_SOURCE_REGISTRY on first use: ~s~%"
          (and dir (equal (namestring (asdf:system-source-directory "sr-probe"))
                          (concatenate 'string dir "sr-probe/"))))
  (format t "6 system-source-directory is a pathname: ~s~%" (pathnamep (asdf:system-source-directory "sr-probe"))))
