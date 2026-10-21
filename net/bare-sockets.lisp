;;;; bare-sockets.lisp - sb-bsd-sockets on bare metal: the %SBS-* primitives under
;;;; net/sb-sys-shim.lisp, over net/tcp-conns.lisp instead of Linux syscalls.
;;;;
;;;; The shim is the SB-BSD-SOCKETS surface (classes, conditions, SOCKET-CONNECT, ...); it
;;;; calls a small set of descriptor primitives (%SBS-OPEN, %SBS-CONNECT-IN, SOCKET-SEND,
;;;; ...) that hosted images implement with syscalls.  These implement the same primitives,
;;;; same arguments, same "negative = -errno" answers, over a descriptor table: a socket
;;;; descriptor is an integer >= 2000 naming a BSOCK record.  File streams reach the same
;;;; records through *NET-FD-CALL* (mvm/cl-fileio.lisp, the cabinet's pattern), so
;;;; SOCKET-MAKE-STREAM's stream reads and writes the connection.
;;;;
;;;; Blocking is pumping: a read waits by running NET-PUMP until there is data, the peer has
;;;; closed, or (non-blocking) at once.  Stream output is buffered per descriptor and goes
;;;; out a segment (1460 bytes) at a time, on FINISH-OUTPUT, or before the next read.
;;;; Unix-domain sockets and the stat/chmod/credentials primitives signal: nothing here has a
;;;; filesystem namespace for them.

(defvar *bsocks* (make-hash-table) "fd -> BSOCK.")
(defvar *bsock-next* 2000)
(defvar *sbs-from* (cons 0 0) "(ip . port) of the last datagram RECVFROM returned.")

(defclass bsock ()
  ((kind :initarg :kind :accessor bs-kind)          ; :stream or :datagram
   (lport :initform 0 :accessor bs-lport)
   (conn :initform nil :accessor bs-conn)            ; TCP-CONN once connected / accepted
   (listener :initform nil :accessor bs-listener)
   (nonblocking :initform nil :accessor bs-nonblocking)
   (out :initform (make-array 1460 :element-type '(unsigned-byte 8) :fill-pointer 0)
        :accessor bs-out)))

(defun %bs (fd) (gethash fd *bsocks*))
(defun %bs-new (kind)
  (let ((fd *bsock-next*))
    (setq *bsock-next* (+ fd 1))
    (setf (gethash fd *bsocks*) (make-instance 'bsock :kind kind))
    fd))
(defun %ephemeral-port () (+ 49152 (setq *tcp-port-next* (logand (+ *tcp-port-next* 1) #x3FFF))))

;;; ---- the primitives the shim calls -----------------------------------------------------

(defun %sbs-open (family type)
  (if (/= family 2) -97                               ; EAFNOSUPPORT: inet only
      (%bs-new (if (= type 2) :datagram :stream))))

(defun %sbs-bind-in (fd ip port)
  ip
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          (t (setf (bs-lport b) (if (zerop port) (%ephemeral-port) port))
             (when (eq (bs-kind b) :datagram) (udp-open (bs-lport b)))
             0))))

(defun %sbs-listen (fd backlog)
  backlog
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          ((zerop (bs-lport b)) -22)                  ; EINVAL: listen before bind
          (t (setf (bs-listener b) (tcpc-listen (bs-lport b))) 0))))

(defun %sbs-accept (fd)
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          ((null (bs-listener b)) -22)
          (t (let ((c (tcpc-accept (bs-listener b) :timeout-ms (if (bs-nonblocking b) 0 86400000))))
               (if (null c)
                   -11                                ; EAGAIN
                   (let ((nfd (%bs-new :stream)))
                     (setf (bs-conn (%bs nfd)) c (bs-lport (%bs nfd)) (tc-lport c))
                     nfd)))))))

(defun %sbs-connect-in (fd ip port)
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          ((eq (bs-kind b) :datagram)                 ; a connected datagram socket: remember the peer
           (when (zerop (bs-lport b)) (setf (bs-lport b) (%ephemeral-port)) (udp-open (bs-lport b)))
           (setf (bs-conn b) (cons ip port)) 0)
          (t (let ((c (tcpc-open ip port :timeout-ms 10000)))
               (if c (progn (setf (bs-conn b) c (bs-lport b) (tc-lport c)) 0) -111))))))

(defun %bs-flush (b)
  "Send what B's stream has buffered."
  (let ((o (bs-out b)) (c (bs-conn b)))
    (when (and (> (fill-pointer o) 0) (typep c 'tcp-conn))
      (tcpc-write c o :end (fill-pointer o))
      (setf (fill-pointer o) 0))
    t))

(defun %sbs-close (fd)
  (let ((b (%bs fd)))
    (when b
      (%bs-flush b)
      (let ((c (bs-conn b))) (when (typep c 'tcp-conn) (tcpc-close c)))
      (when (bs-listener b) (setq *tcp-listeners* (remove (bs-listener b) *tcp-listeners*)))
      (when (eq (bs-kind b) :datagram) (setq *udp-ports* (remove (bs-lport b) *udp-ports* :key #'car)))
      (remhash fd *bsocks*))
    0))

(defun %sbs-shutdown (fd how)
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          (t (%bs-flush b)
             (let ((c (bs-conn b)))
               (when (and (typep c 'tcp-conn) (>= how 1)) (tcpc-close c :timeout-ms 0)))
             0))))

(defun %sbs-getname (fd which)
  "(ip * 65536) + port: ours for WHICH 51, the peer's for 52."
  (let* ((b (%bs fd)) (c (and b (bs-conn b))))
    (cond ((null b) -9)
          ((= which 51) (+ (* (%our-ip) 65536) (bs-lport b)))
          ((typep c 'tcp-conn) (+ (* (tc-rip c) 65536) (tc-rport c)))
          ((consp c) (+ (* (car c) 65536) (cdr c)))
          (t -107))))                                 ; ENOTCONN

(defun %sbs-sendto (fd arr len ip port)
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          (t (when (zerop (bs-lport b)) (setf (bs-lport b) (%ephemeral-port)) (udp-open (bs-lport b)))
             (udp-send-to ip port (bs-lport b) arr len)
             len))))

(defun %sbs-recvfrom (fd arr max)
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          (t (let ((d (udp-recv (bs-lport b) :timeout-ms (if (bs-nonblocking b) 0 86400000))))
               (if (null d)
                   -11
                   (let* ((v (third d)) (n (min max (length v))))
                     (dotimes (i n) (setf (aref arr i) (aref v i)))
                     (setq *sbs-from* (cons (first d) (second d)))
                     n)))))))
(defun %sbs-from-ip () (car *sbs-from*))
(defun %sbs-from-port () (cdr *sbs-from*))

(defun socket-send (fd arr len)
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          ((consp (bs-conn b))                         ; connected datagram socket
           (%sbs-sendto fd arr len (car (bs-conn b)) (cdr (bs-conn b))))
          ((not (typep (bs-conn b) 'tcp-conn)) -107)
          (t (%bs-flush b)
             (if (tcpc-write (bs-conn b) arr :end len) len -32)))))   ; EPIPE

(defun socket-recv (fd arr max)
  (let ((b (%bs fd)))
    (cond ((null b) -9)
          ((eq (bs-kind b) :datagram) (%sbs-recvfrom fd arr max))
          ((not (typep (bs-conn b) 'tcp-conn)) -107)
          (t (%bs-flush b)
             (let ((c (bs-conn b)))
               (loop
                 (let ((r (tcpc-read c :max max :timeout-ms (if (bs-nonblocking b) 0 1000))))
                   (cond ((eq r :eof) (return 0))
                         ((> (length r) 0)
                          (dotimes (i (length r)) (setf (aref arr i) (aref r i)))
                          (return (length r)))
                         ((bs-nonblocking b) (return -11))))))))))

(defun %sbs-poll-in (fd ms)
  "1 when a read on FD would not block, 0 when MS (-1: forever) passes first."
  (let ((b (%bs fd)))
    (if (null b) -9
        (let ((ready (lambda ()
                       (let ((c (bs-conn b)))
                         (cond ((bs-listener b) (tl-ready (bs-listener b)))
                               ((eq (bs-kind b) :datagram) (cdr (assoc (bs-lport b) *udp-ports*)))
                               ((typep c 'tcp-conn) (or (> (fill-pointer (tc-rx c)) 0) (tc-rx-eof c)))
                               (t t))))))
          (if (%pump-until ready (if (< ms 0) 86400000 ms)) 1 0)))))

(defun %sbs-getopt (fd level opt) fd level opt 0)
(defun %sbs-setopt (fd level opt value) fd level opt value 0)

(defun %bs-nonblocking-of (socket)
  (let ((b (%bs (funcall (intern "SOCKET-FILE-DESCRIPTOR" "SB-BSD-SOCKETS") socket)))) (and b (bs-nonblocking b))))
(defun %bs-set-nonblocking (value socket)
  (let ((b (%bs (funcall (intern "SOCKET-FILE-DESCRIPTOR" "SB-BSD-SOCKETS") socket))))
    (when b (setf (bs-nonblocking b) (and value t)))
    value))
;; SB-BSD-SOCKETS:NON-BLOCKING-MODE and its SETF, named at load time: the build's source gate
;; reads this file with SBCL, where that symbol need not exist yet.
(setf (fdefinition (intern "NON-BLOCKING-MODE" "SB-BSD-SOCKETS")) (function %bs-nonblocking-of))
(setf (fdefinition (list 'setf (intern "NON-BLOCKING-MODE" "SB-BSD-SOCKETS"))) (function %bs-set-nonblocking))

(defun %bare-no-namespace (what)
  (error "~A: no filesystem namespace on bare metal (unix-domain sockets are hosted only)" what))
(defun %sbs-bind-un (fd path) fd path (%bare-no-namespace "bind to a local socket"))
(defun %sbs-connect-un (fd path) fd path (%bare-no-namespace "connect to a local socket"))
(defun %sbs-peercred (fd) fd (%bare-no-namespace "peer credentials"))

;;; ---- file streams over socket descriptors (mvm/cl-fileio.lisp's *NET-FD-CALL*) --------

(defun %net-fd-dispatch (op fd &rest args)
  (let ((b (%bs fd)))
    (case op
      (:fd-p (and b t))
      (:close (%sbs-close fd))
      (:flush (when b (%bs-flush b)) t)
      (:ready-p (and b (> (%sbs-poll-in fd 0) 0)))
      (:write-byte
       (let ((o (bs-out b)))
         (vector-push (first args) o)
         (when (>= (fill-pointer o) 1460) (%bs-flush b))
         1))
      (:read-into                                    ; (buf count): into the stream's string buffer
       (let* ((buf (first args)) (count (second args))
              (tmp (make-array count :element-type '(unsigned-byte 8)))
              (n (socket-recv fd tmp count)))
         (when (> n 0) (dotimes (i n) (%prim-aset buf i (aref tmp i))))
         n)))))

(setq *net-fd-call* (function %net-fd-dispatch))
