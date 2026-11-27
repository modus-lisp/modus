;;; net/hosted-sockets-i386.lisp -- the socket floor for the hosted Linux/i386 image.
;;;
;;; net/hosted-sockets.lisp is written in x86-64's syscall numbers (socket 41, connect 42,
;;; read 0, write 1, close 3), and on i386 those are other calls: 1 is exit, 3 is read, 41
;;; is dup.  net/hosted-sockets-post.lisp (x86-64) and net/hosted-sockets-a64.lisp (AArch64)
;;; are the floors the SB-BSD-SOCKETS shim stands on; i386 had none, so the shim's first
;;; MAKE-INSTANCE called an undefined %SBS-OPEN.  This file is that floor under i386's
;;; numbers, with the same names and contracts, baked right after hosted-sockets.lisp in the
;;; hosted i386 CLI only (mvm/build-cli-common.lisp), so its DEFUNs replace that file's
;;; x86-64 ones (each is a declared seam there).
;;;
;;; EVERY SOCKET CALL GOES THROUGH socketcall(2), syscall 102: (call-number, pointer to the
;;; argument words).  i386 Linux also has the direct calls (socket 359 ..), but sendto,
;;; recvfrom, setsockopt and getsockopt take five or six arguments, and the sixth goes in
;;; EBP, which is this translator's frame pointer -- the reason the image has SYSCALL3 and
;;; no SYSCALL6.  socketcall takes two, so one 3-argument trap reaches all of them.
;;;
;;; Memory: the top kilobyte of the 4 KB C-string page (*CSTR-SCRATCH*, below 2^30 as every
;;; syscall address must be; paths never reach +3072): the sockaddr at +3072 (the default
;;; %SOCK-ADDR-BUF), socketcall's argument words at +3200, and %SBS-SCRATCH's 256 bytes at
;;; +3584.  Data goes through the 4 KB io page (%SOCK-IO-BUF), as on every one-thread image.

;;; ---- socketcall(2) ----------------------------------------------------------------------

(defun %sk386-args () (+ *cstr-scratch* 3200))

(defun %sk386-call (op a b c d e f)
  "socketcall(OP, {A B C D E F}): the call's result, or -errno."
  (let ((p (%sk386-args)))
    (setf (mem-ref p :u32) a)
    (setf (mem-ref (+ p 4) :u32) b)
    (setf (mem-ref (+ p 8) :u32) c)
    (setf (mem-ref (+ p 12) :u32) d)
    (setf (mem-ref (+ p 16) :u32) e)
    (setf (mem-ref (+ p 20) :u32) f)
    (syscall3 102 op p 0)))

;;; socketcall numbers (linux/net.h)
(defun %sk386-socket (family type)       (%sk386-call 1 family type 0 0 0 0))
(defun %sk386-bind (fd addr len)         (%sk386-call 2 fd addr len 0 0 0))
(defun %sk386-connect (fd addr len)      (%sk386-call 3 fd addr len 0 0 0))
(defun %sk386-listen (fd backlog)        (%sk386-call 4 fd backlog 0 0 0 0))
(defun %sk386-accept (fd)                (%sk386-call 5 fd 0 0 0 0 0))
(defun %sk386-getname (nr fd addr lenp)  (%sk386-call nr fd addr lenp 0 0 0)) ; 6 sock, 7 peer
(defun %sk386-sendto (fd buf len addr alen) (%sk386-call 11 fd buf len 0 addr alen))
(defun %sk386-recvfrom (fd buf len addr lenp) (%sk386-call 12 fd buf len 0 addr lenp))
(defun %sk386-shutdown (fd how)          (%sk386-call 13 fd how 0 0 0 0))
(defun %sk386-setopt (fd level opt p n)  (%sk386-call 14 fd level opt p n 0))
(defun %sk386-getopt (fd level opt p lp) (%sk386-call 15 fd level opt p lp 0))

;;; ---- hosted-sockets.lisp's primitives, in i386's numbers ---------------------------------

(defun %sock-open (type)       (%sk386-socket 2 type))
(defun %sock-connect-fd (fd)   (%sk386-connect fd (%sock-addr-buf) 16))
(defun %sock-write (fd len)    (syscall3 4 fd (%sock-io-buf) len))
(defun %sock-read  (fd len)    (syscall3 3 fd (%sock-io-buf) len))
(defun socket-close (fd)       (syscall3 6 fd 0 0))

(defun %sock-set-reuseaddr (fd)
  (let ((opt (+ (%sock-addr-buf) 16)))
    (setf (mem-ref opt :u32) 1)
    (%sk386-setopt fd 1 2 opt 4)))

(defun socket-listen-on (ip port backlog)
  (let ((fd (%sock-open 1)))
    (if (< fd 0)
        -1
        (progn
          (%sock-set-reuseaddr fd)
          (%sock-build-addr ip port)
          (if (< (%sk386-bind fd (%sock-addr-buf) 16) 0)
              (progn (socket-close fd) -1)
              (if (< (%sk386-listen fd backlog) 0)
                  (progn (socket-close fd) -1)
                  fd))))))

(defun socket-local-port (fd)
  (let ((a (%sock-addr-buf)))
    (setf (mem-ref (+ a 32) :u32) 16)
    (if (< (%sk386-getname 6 fd a (+ a 32)) 0)
        -1
        (+ (ash (mem-ref (+ a 2) :u8) 8) (mem-ref (+ a 3) :u8)))))

(defun udp-sendto (fd arr len ip port)
  (if (> len (%sock-io-cap))
      -90
      (progn
        (let ((io (%sock-io-buf)) (i 0))
          (loop
            (when (>= i len) (return nil))
            (setf (mem-ref (+ io i) :u8) (aref arr i))
            (setq i (+ i 1))))
        (%sock-build-addr ip port)
        (%sk386-sendto fd (%sock-io-buf) len (%sock-addr-buf) 16))))

(defun udp-recvfrom (fd arr max)
  (let ((want max))
    (when (> want (%sock-io-cap)) (setq want (%sock-io-cap)))
    (let ((n (%sk386-recvfrom fd (%sock-io-buf) want 0 0)))
      (if (< n 1)
          n
          (let ((io (%sock-io-buf)) (i 0))
            (loop
              (when (>= i n) (return n))
              (aset arr i (mem-ref (+ io i) :u8))
              (setq i (+ i 1))))))))

(defun socket-accept (lfd)
  (let ((c (%sk386-accept lfd)))
    (if (< c 0) -1 c)))

(defun vsock-connect (cid port)
  (let ((fd (%sk386-socket 40 1)))
    (if (< fd 0)
        -1
        (progn (%vsock-build-addr cid port)
               (if (< (%sk386-connect fd (%sock-addr-buf) 16) 0)
                   (progn (socket-close fd) -1)
                   fd)))))

(defun vsock-listen (port backlog)
  (let ((fd (%sk386-socket 40 1)))
    (if (< fd 0)
        -1
        (progn (%vsock-build-addr #xFFFFFFFF port)
               (if (< (%sk386-bind fd (%sock-addr-buf) 16) 0)
                   (progn (socket-close fd) -1)
                   (if (< (%sk386-listen fd backlog) 0)
                       (progn (socket-close fd) -1)
                       fd))))))

;;; ---- the SB-BSD-SOCKETS shim's floor (net/hosted-sockets-a64.lisp's names) ---------------

(defun %sbs-zero (from to)
  (let ((i from))
    (loop
      (when (>= i to) (return nil))
      (setf (mem-ref i :u8) 0)
      (setq i (+ i 1)))))

(defun %sbs-scratch () (+ *cstr-scratch* 3584))

(defun %sbs-addr-in (ip port)
  "sockaddr_in(AF_INET, PORT, IP) in the scratch; returns its address."
  (let ((a (%sbs-scratch)))
    (%sbs-zero a (+ a 16))
    (setf (mem-ref a :u16) 2)
    (setf (mem-ref (+ a 2) :u8) (logand (ash port -8) 255))
    (setf (mem-ref (+ a 3) :u8) (logand port 255))
    (setf (mem-ref (+ a 4) :u8) (logand (ash ip -24) 255))
    (setf (mem-ref (+ a 5) :u8) (logand (ash ip -16) 255))
    (setf (mem-ref (+ a 6) :u8) (logand (ash ip -8) 255))
    (setf (mem-ref (+ a 7) :u8) (logand ip 255))
    a))

(defun %sbs-addr-un (path)
  "sockaddr_un(AF_UNIX, PATH) in the scratch; the LENGTH to pass, or -2 if PATH is longer
   than sun_path's 107 bytes."
  (let ((a (%sbs-scratch)) (n (length path)))
    (if (> n 107)
        -2
        (progn
          (%sbs-zero a (+ a 112))
          (setf (mem-ref a :u16) 1)
          (let ((i 0))
            (loop
              (when (>= i n) (return nil))
              (setf (mem-ref (+ a (+ 2 i)) :u8) (char-code (char path i)))
              (setq i (+ i 1))))
          (+ 3 n)))))

;;; fcntl(2) is 55 on i386: F_GETFL 3, F_SETFL 4, O_NONBLOCK 2048 (as x86-64).
(defun %sbs-fcntl (fd cmd arg) (syscall3 55 fd cmd arg))

(defun socket-set-nonblock (fd)
  (let ((fl (%sbs-fcntl fd 3 0)))
    (if (< fl 0)
        fl
        (if (zerop (logand fl 2048)) (%sbs-fcntl fd 4 (+ fl 2048)) 0))))

(defun socket-set-block (fd)
  (let ((fl (%sbs-fcntl fd 3 0)))
    (if (< fl 0)
        fl
        (if (zerop (logand fl 2048)) 0 (%sbs-fcntl fd 4 (- fl 2048))))))

(defun %sbs-open (family type) (%sk386-socket family type))
(defun %sbs-close (fd) (syscall3 6 fd 0 0))
(defun %sbs-listen (fd backlog) (%sk386-listen fd backlog))
(defun %sbs-accept (fd) (%sk386-accept fd))
(defun %sbs-shutdown (fd how) (%sk386-shutdown fd how))

(defun %sbs-bind-in (fd ip port)
  (%sk386-bind fd (%sbs-addr-in ip port) 16))

(defun %sbs-bind-un (fd path)
  (let ((len (%sbs-addr-un path)))
    (if (< len 0) -36 (%sk386-bind fd (%sbs-scratch) len))))

(defun %sbs-connect-un (fd path)
  (let ((len (%sbs-addr-un path)))
    (if (< len 0) -36 (%sk386-connect fd (%sbs-scratch) len))))

;;; poll(2) is 168 on i386, three arguments, the timeout in milliseconds (< 0 = forever).
;;; One pollfd at scratch+200.  > 0 ready, 0 timed out, < 0 -errno.
(defun %sk386-poll1 (fd events ms)
  (let ((p (+ (%sbs-scratch) 200)))
    (%sbs-zero p (+ p 8))
    (setf (mem-ref p :u32) fd)
    (setf (mem-ref (+ p 4) :u16) events)
    (syscall3 168 p 1 ms)))

(defun socket-wait-readable (fd ms) (%sk386-poll1 fd 1 ms))
(defun %sbs-poll-in (fd ms) (socket-wait-readable fd ms))
(defun %sbs-wait-writable (fd ms) (%sk386-poll1 fd 4 ms))

;;; A connect that cannot finish is an error, not a frozen image: non-blocking, wait up to 30
;;; s for the handshake, SO_ERROR says how it ended, then blocking again for the transfers.
(defun %sbs-connect-in (fd ip port)
  (let ((a (%sbs-addr-in ip port)))
    (socket-set-nonblock fd)
    (let ((r (%sk386-connect fd a 16)))
      (when (= r -115)                              ; EINPROGRESS
        (let ((w (%sbs-wait-writable fd 30000)))
          (setq r (cond ((< w 0) w)
                        ((= w 0) -110)              ; ETIMEDOUT
                        (t (let ((e (%sbs-getopt fd 1 4)))   ; SO_ERROR
                             (cond ((< e 0) e) ((= e 0) 0) (t (- e)))))))))
      (socket-set-block fd)
      r)))

(defun %sbs-sendto (fd arr len ip port)
  "sendto(2): LEN bytes of ARR as ONE datagram to IP:PORT.  The byte count, or -errno."
  (if (> len (%sock-io-cap))
      -90
      (let ((io (%sock-io-buf)) (i 0))
        (loop
          (when (>= i len) (return nil))
          (setf (mem-ref (+ io i) :u8) (aref arr i))
          (setq i (+ i 1)))
        (%sk386-sendto fd io len (%sbs-addr-in ip port) 16))))

(defun %sbs-recvfrom (fd arr max)
  "recvfrom(2): one datagram into ARR, at most MAX bytes; the sender stays in the scratch."
  (let ((a (%sbs-scratch)) (want max))
    (when (> want (%sock-io-cap)) (setq want (%sock-io-cap)))
    (%sbs-zero a (+ a 24))
    (setf (mem-ref (+ a 16) :u32) 16)
    (let ((n (%sk386-recvfrom fd (%sock-io-buf) want a (+ a 16))))
      (when (> n 0)
        (let ((io (%sock-io-buf)) (i 0))
          (loop
            (when (>= i n) (return nil))
            (aset arr i (mem-ref (+ io i) :u8))
            (setq i (+ i 1)))))
      n)))

(defun %sbs-from-port ()
  (let ((a (%sbs-scratch)))
    (+ (ash (mem-ref (+ a 2) :u8) 8) (mem-ref (+ a 3) :u8))))

(defun %sbs-from-ip ()
  (let ((a (%sbs-scratch)))
    (+ (* (mem-ref (+ a 4) :u8) 16777216) (* (mem-ref (+ a 5) :u8) 65536)
       (* (mem-ref (+ a 6) :u8) 256) (mem-ref (+ a 7) :u8))))

(defun %sbs-getname (fd which)
  "getsockname / getpeername, PACKED: (ip * 65536) + port, or -errno.  WHICH is x86-64's
   number (51 or 52) from the callers."
  (let ((a (%sbs-scratch)))
    (%sbs-zero a (+ a 64))
    (setf (mem-ref (+ a 128) :u32) 16)
    (let ((r (%sk386-getname (if (= which 52) 7 6) fd a (+ a 128))))
      (if (< r 0)
          r
          (+ (* (+ (* (mem-ref (+ a 4) :u8) 16777216)
                   (+ (* (mem-ref (+ a 5) :u8) 65536)
                      (+ (* (mem-ref (+ a 6) :u8) 256)
                         (mem-ref (+ a 7) :u8))))
                65536)
             (+ (* (mem-ref (+ a 2) :u8) 256) (mem-ref (+ a 3) :u8)))))))

(defun %sbs-setopt (fd level opt value)
  (let ((a (%sbs-scratch)))
    (setf (mem-ref (+ a 160) :u32) value)
    (%sk386-setopt fd level opt (+ a 160) 4)))

(defun %sbs-getopt (fd level opt)
  (let ((a (%sbs-scratch)))
    (setf (mem-ref (+ a 160) :u32) 0)
    (setf (mem-ref (+ a 168) :u32) 4)
    (let ((r (%sk386-getopt fd level opt (+ a 160) (+ a 168))))
      (if (< r 0) r (mem-ref (+ a 160) :u32)))))

;;; LISTEN's readiness check (mvm/cl-fileio.lisp's seam, which answers T for any fd): ready
;;; when poll(2) says so now, or reports the fd hung up or in error (a read will not block).
(defun %fd-input-ready-p (fd)
  (let ((r (%sk386-poll1 fd 1 0)))
    (if (< r 0) t (> r 0))))
