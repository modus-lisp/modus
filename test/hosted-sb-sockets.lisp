;;;; hosted-sb-sockets.lisp — SB-BSD-SOCKETS / SB-POSIX / SB-SYS / SB-ALIEN.
;;;;
;;;;   ./modus --script test/hosted-sb-sockets.lisp
;;;;
;;;; net/sb-sys-shim.lisp claims to be the sb-bsd-sockets surface glass does
;;;; ALL of its RFB I/O through.  This drives it the way glass does: a listener,
;;;; an accepted connection, a Lisp STREAM over each end, WRITE-SEQUENCE and
;;;; READ-BYTE, FORCE-OUTPUT, and a shutdown.
;;;;
;;;; NETWORK RULES, and they are checked rather than asserted:
;;;;   * 127.0.0.1 only.
;;;;   * the port is bind(0) + SOCKET-NAME, so no number is chosen or raced for.
;;;;   * 5900-5920 is refused outright — that is where this box's desktops live.
;;;;   * everything is closed, and the test says so.
;;;;
;;;; AND THE LISTEN BUG IS THE POINT OF ONE SECTION.  Before this step, (LISTEN
;;;; stream) on a DRAINED socket answered T because the fd was valid, while poll
;;;; on the same fd said nothing was ready.  The check below is the difference:
;;;; drained must be NIL, with data must be T, and after a peer close it must be
;;;; T (end of file does not block).

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

;;; ELEMENT-WISE, BECAUSE EQUAL ON TWO VECTORS IS NIL HERE.  CLHS says EQUAL
;;; descends conses and strings and bit-vectors but compares other arrays with
;;; EQ, so #(127 0 0 1) is not EQUAL to another #(127 0 0 1) in ANY conforming
;;; Lisp — the first draft of this test was simply wrong, not modus.
(defun vec= (a b)
  (if (= (length a) (length b))
      (let ((i 0) (ok t))
        (loop
          (when (>= i (length a)) (return nil))
          (if (= (aref a i) (aref b i)) nil (setq ok nil))
          (setq i (+ i 1)))
        ok)
      nil))

(defun chk-vec (name got want)
  (setq *checks* (+ *checks* 1))
  (if (vec= got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v
      (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A~%" name))))

(format t "~%=== SB-BSD-SOCKETS, SB-POSIX, SB-SYS, SB-ALIEN ===========~%")

(format t "~%-- addresses --------------------------------------------~%")
(chk-vec "make-inet-address" (sb-bsd-sockets:make-inet-address "127.0.0.1")
         #(127 0 0 1))
(chk-vec "make-inet-address, three digits"
         (sb-bsd-sockets:make-inet-address "192.168.100.254") #(192 168 100 254))
(chk-vec "get-host-by-name on a dotted quad"
         (sb-bsd-sockets:host-ent-address
          (sb-bsd-sockets:get-host-by-name "127.0.0.1"))
         #(127 0 0 1))
(chk "get-host-by-name on a NAME signals (stated PARTIAL)"
     (handler-case (progn (sb-bsd-sockets:get-host-by-name "example.invalid")
                          :returned)
       (sb-bsd-sockets:socket-error (c) c :signalled))
     :signalled)

(format t "~%-- a listener on an EPHEMERAL loopback port -------------~%")
(let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                               :sock-type :stream :sock-protocol :tcp)))
  (chk-true "the listener is a socket" (typep listener 'sb-bsd-sockets:socket))
  (chk-true "... and an inet-socket" (typep listener 'sb-bsd-sockets:inet-socket))
  (chk-true "setf sockopt-reuse-address"
            (setf (sb-bsd-sockets:sockopt-reuse-address listener) t))
  (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
  (sb-bsd-sockets:socket-listen listener 4)
  (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name listener)
    (chk-vec "bound to loopback" addr #(127 0 0 1))
    (chk-true (format nil "kernel chose port ~D, outside 5900-5920" port)
              (and (> port 0) (or (< port 5900) (> port 5920))))
    (format t "  ... port ~D~%" port)

    ;; ---- connect to it ----
    (let ((client (make-instance 'sb-bsd-sockets:inet-socket
                                 :sock-type :stream :sock-protocol :tcp)))
      (sb-bsd-sockets:socket-connect client #(127 0 0 1) port)
      (let ((server (sb-bsd-sockets:socket-accept listener)))
        (chk-true "socket-accept returned a socket"
                  (typep server 'sb-bsd-sockets:inet-socket))
        (chk-true "the accepted socket has its own fd"
                  (not (= (sb-bsd-sockets:socket-file-descriptor server)
                          (sb-bsd-sockets:socket-file-descriptor listener))))
        (chk-true "setf sockopt-tcp-nodelay on the accepted socket"
                  (setf (sb-bsd-sockets:sockopt-tcp-nodelay server) t))
        (multiple-value-bind (paddr pport) (sb-bsd-sockets:socket-peername server)
          (chk-vec "socket-peername address" paddr #(127 0 0 1))
          (chk-true "socket-peername port is the client's ephemeral one"
                    (> pport 0)))

        ;; ---- STREAMS, the way glass does all of its RFB I/O ----
        (let ((cs (sb-bsd-sockets:socket-make-stream
                   client :input t :output t
                   :element-type '(unsigned-byte 8) :buffering :full))
              (ss (sb-bsd-sockets:socket-make-stream
                   server :input t :output t
                   :element-type '(unsigned-byte 8) :buffering :full)))
          (chk-true "socket-make-stream returns a stream" (streamp cs))
          (chk-true "sb-sys:fd-stream-p on it" (sb-sys:fd-stream-p cs))
          (chk-true "typep against the SB-SYS:FD-STREAM type"
                    (typep cs 'sb-sys:fd-stream))
          (chk "sb-sys:fd-stream-fd is the socket's fd"
               (sb-sys:fd-stream-fd cs)
               (sb-bsd-sockets:socket-file-descriptor client))

          ;; ---- THE LISTEN BUG ----
          (chk "LISTEN on a DRAINED socket stream is NIL" (listen ss) nil)
          (write-byte 65 cs)
          (write-byte 66 cs)
          (force-output cs)
          (%sleep-ms 50)
          (chk-true "LISTEN with data waiting is T" (listen ss))
          (chk "read-byte" (read-byte ss) 65)
          (chk-true "LISTEN is still T with one byte left" (listen ss))
          (chk "read-byte again" (read-byte ss) 66)
          (chk "LISTEN is NIL once drained again" (listen ss) nil)

          ;; ---- a real payload, write-sequence / read-sequence ----
          (let ((out (make-array 4096))
                (in (make-array 4096))
                (i 0))
            (loop
              (when (>= i 4096) (return nil))
              (aset out i (logand (* i 7) 255))
              (setq i (+ i 1)))
            (write-sequence out cs)
            (force-output cs)
            (let ((got (read-sequence in ss)))
              (chk "write-sequence / read-sequence length" got 4096)
              (let ((bad 0) (j 0))
                (loop
                  (when (>= j 4096) (return nil))
                  (if (= (aref in j) (aref out j)) nil (setq bad (+ bad 1)))
                  (setq j (+ j 1)))
                (chk "every byte survived the round trip" bad 0))))

          ;; ---- SB-ALIEN: SIOCOUTQ through the ioctl emulation ----
          (let ((q (handler-case
                       (sb-alien:with-alien ((n sb-alien:int 0))
                         (if (zerop (sb-alien:alien-funcall
                                     (sb-alien:extern-alien "ioctl"
                                       (function sb-alien:int sb-alien:int
                                                 sb-alien:unsigned-long
                                                 (* sb-alien:int)))
                                     (sb-sys:fd-stream-fd cs) #x5411
                                     (sb-alien:addr n)))
                             (sb-alien:deref n) -1))
                     (t (c) c :error))))
            (format t "  ... SIOCOUTQ on the client fd: ~A~%" q)
            (chk-true "sb-alien ioctl emulation answered a number"
                      (and (integerp q) (>= q 0))))
          ;; SO_PEERCRED on a TCP socket: the syscall succeeds and Linux answers
          ;; pid 0, which is exactly the case glass's peer-credentials turns into
          ;; NIL.  What is checked here is that the CALL works, not the answer.
          (let ((r (handler-case
                       (sb-alien:with-alien ((buf (sb-alien:array sb-alien:int 3))
                                             (len sb-alien:int 12))
                         (sb-alien:alien-funcall
                          (sb-alien:extern-alien "getsockopt"
                            (function sb-alien:int sb-alien:int sb-alien:int
                                      sb-alien:int (* t) (* sb-alien:int)))
                          (sb-sys:fd-stream-fd cs) 1 17
                          (sb-alien:cast (sb-alien:addr buf) (* t))
                          (sb-alien:addr len)))
                     (t (c) c :error))))
            (chk "sb-alien getsockopt(SO_PEERCRED) returns 0 on a TCP socket" r 0))
          (chk "an UNKNOWN extern-alien signals"
               (handler-case
                   (progn (sb-alien:alien-funcall
                           (sb-alien:extern-alien "printf" (function sb-alien:int))
                           ) :returned)
                 (t (c) c :signalled))
               :signalled)

          ;; ---- shutdown and close ----
          (chk-true "socket-shutdown :io" (sb-bsd-sockets:socket-shutdown
                                           client :direction :io))
          (close cs)
          (chk-true "socket-close the accepted end" (sb-bsd-sockets:socket-close server))
          (chk "closing twice is a no-op, not an error"
               (sb-bsd-sockets:socket-close server) nil)))
      (sb-bsd-sockets:socket-close client)))
  (chk-true "socket-close the listener" (sb-bsd-sockets:socket-close listener)))

(format t "~%-- connecting to a port nobody is on --------------------~%")
(chk "connection-refused-error is signalled and is a socket-error"
     (let ((s (make-instance 'sb-bsd-sockets:inet-socket :sock-type :stream)))
       (handler-case (progn (sb-bsd-sockets:socket-connect s #(127 0 0 1) 1) :connected)
         (sb-bsd-sockets:connection-refused-error (c) c :refused)
         (sb-bsd-sockets:socket-error (c) c :other-socket-error)
         (t (c) c :other)))
     :refused)

(format t "~%-- SB-POSIX ---------------------------------------------~%")
(chk-true "getuid" (>= (sb-posix:getuid) 0))
(chk-true "getpid" (> (sb-posix:getpid) 0))
(let ((path "/tmp/modus-sb-posix-probe.txt"))
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string "hello" s))
  (let ((st (sb-posix:stat path)))
    (chk "stat-size" (sb-posix:stat-size st) 5)
    (chk "stat-uid is ours" (sb-posix:stat-uid st) (sb-posix:getuid))
    (chk-true "stat-ino is non-zero" (> (sb-posix:stat-ino st) 0))
    (chk-true "s-isreg" (sb-posix:s-isreg (sb-posix:stat-mode st)))
    (chk-true "not s-isdir" (not (sb-posix:s-isdir (sb-posix:stat-mode st))))
    (chk-true "not s-issock" (not (sb-posix:s-issock (sb-posix:stat-mode st)))))
  (sb-posix:chmod path 384)               ; 0600
  (chk "chmod took" (logand (sb-posix:stat-mode (sb-posix:stat path)) 511) 384)
  (sb-posix:unlink path)
  (chk "stat after unlink signals"
       (handler-case (progn (sb-posix:stat path) :returned)
         (sb-posix:syscall-error (c) c :signalled))
       :signalled))
(chk-true "s-isdir on /tmp" (sb-posix:s-isdir (sb-posix:stat-mode
                                               (sb-posix:stat "/tmp"))))

(format t "~%-- SB-EXT -----------------------------------------------~%")
(chk-true "posix-getenv HOME" (and (sb-ext:posix-getenv "HOME") t))
(chk "posix-getenv of something unset"
     (sb-ext:posix-getenv "MODUS_DEFINITELY_NOT_SET_XYZZY") nil)
(chk "*exit-hooks* exists and is a list" sb-ext:*exit-hooks* nil)
(chk "run-program signals (stated NOT IMPLEMENTED)"
     (handler-case (progn (sb-ext:run-program "/bin/true" nil) :returned)
       (t (c) c :signalled))
     :signalled)

(format t "~%-- an explicit-port listener, and it is a DIFFERENT call -~%")
(chk "socket-listen-port refuses port 0" (socket-listen-port 0) -2)
(chk "socket-listen-port refuses port 70000" (socket-listen-port 70000) -2)
(chk "socket-listen-port-on-address refuses port 0"
     (socket-listen-port-on-address (%sock-loopback) 0) -2)

(format t "~%=== VERDICT ==============================================~%")
(if (zerop *fail*)
    (format t "SB-BSD-SOCKETS AND FRIENDS: PASS (~A checks)~%" *checks*)
    (format t "SB-BSD-SOCKETS AND FRIENDS: FAIL (~A of ~A checks)~%"
            (- *checks* *fail*) *checks*))
