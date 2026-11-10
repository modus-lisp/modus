;;; net/hosted-sockets-a64.lisp -- the socket floor for the AArch64 hosted image.
;;;
;;; hosted-sockets-post.lisp is the x86-64 floor: its syscall numbers are x86-64's
;;; (socket 41, bind 49, fcntl 72, poll 7), and it is left out of the AArch64 build.  The
;;; AArch64 generic ABI numbers these differently (socket 198, bind 200, fcntl 25, ppoll
;;; 73), which is what the Darwin shim (host/macos/modus-shim.c) serves.  This file is the
;;; same floor under those numbers, with the same names and contracts, and it is baked into
;;; the AArch64 CLI only (mvm/build-cli-common.lisp).
;;;
;;; Two calling rules, as in hosted-sockets-post.lisp: the syscalls go through the image's
;;; baked syscall path (%GC-SAFE-BLOCK-3 / -6 / %GC-SAFE-SYSCALL6), because an intrinsic
;;; SYSCALL in runtime-compiled code does not return the kernel's answer; and a function
;;; returns one packed value, not (VALUES ...).

;;; Zero the bytes [FROM, TO).  Plain stores: this file does not depend on the actor layer.
(defun %sbs-zero (from to)
  (let ((i from))
    (loop
      (when (>= i to) (return nil))
      (setf (mem-ref i :u8) 0)
      (setq i (+ i 1)))))

(defun %sbs-scratch ()
  "This CPU's 256-byte socket scratch (%SOCKADDR-SCRATCH).  0 if the thread page could not
   be mapped."
  (%sockaddr-scratch))

(defun %sbs-addr-in (ip port)
  "sockaddr_in(AF_INET, PORT, IP) in this CPU's scratch; returns its address."
  (let ((a (%sbs-scratch)))
    (if (zerop a)
        0
        (progn
          (%sbs-zero a (+ a 16))
          (setf (mem-ref a :u16) 2)
          (setf (mem-ref (+ a 2) :u8) (logand (ash port -8) 255))
          (setf (mem-ref (+ a 3) :u8) (logand port 255))
          (setf (mem-ref (+ a 4) :u8) (logand (ash ip -24) 255))
          (setf (mem-ref (+ a 5) :u8) (logand (ash ip -16) 255))
          (setf (mem-ref (+ a 6) :u8) (logand (ash ip -8) 255))
          (setf (mem-ref (+ a 7) :u8) (logand ip 255))
          a))))

(defun %sbs-addr-un (path)
  "sockaddr_un(AF_UNIX, PATH) in this CPU's scratch; returns the LENGTH to pass to
   bind/connect, or -1 (no scratch) or -2 (the path is longer than sun_path's 107 bytes)."
  (let ((a (%sbs-scratch))
        (n (length path)))
    (if (zerop a)
        -1
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
              (+ 3 n))))))

;;; fcntl(2), 4 arguments: F_GETFL = 3, F_SETFL = 4.  The shim's NON-BLOCKING-MODE reads
;;; through this too (net/sb-bsd-sockets-shim.lisp), so it is the one place the number lives.
(defun %sbs-fcntl (fd cmd arg) (%gc-safe-block-6 25 fd cmd arg 0))

;;; O_NONBLOCK = 2048.  Read the flags first: clearing a flag the kernel set (O_RDWR,
;;; O_CLOEXEC) would be a different bug every time.
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

(defun %sbs-open (family type) (%gc-safe-block-3 198 family type 0))           ; socket
(defun %sbs-close (fd) (%gc-safe-block-3 57 fd 0 0))                           ; close
(defun %sbs-listen (fd backlog) (%gc-safe-block-3 201 fd backlog 0))           ; listen
(defun %sbs-accept (fd) (%gc-safe-block-3 202 fd 0 0))                         ; accept
(defun %sbs-shutdown (fd how) (%gc-safe-block-3 210 fd how 0))                 ; shutdown

(defun %sbs-bind-in (fd ip port)
  (let ((a (%sbs-addr-in ip port)))
    (if (zerop a) -12 (%gc-safe-block-3 200 fd a 16))))                        ; bind

;;; POLLOUT, as ppoll(2): > 0 writable (a connect has finished), 0 timed out, < 0 -errno.
(defun %sbs-wait-writable (fd ms)
  (let ((a (%sbs-scratch)))
    (if (zerop a)
        1
        (progn
          (%sbs-zero (+ a 200) (+ a 240))
          (setf (mem-ref (+ a 200) :u32) fd)
          (setf (mem-ref (+ a 204) :u16) 4)         ; pollfd.events = POLLOUT
          (let ((tmo 0))
            (when (>= ms 0)
              (setf (mem-ref (+ a 216) :u32) (floor ms 1000))
              (setf (mem-ref (+ a 224) :u32) (* 1000000 (mod ms 1000)))
              (setq tmo (+ a 216)))
            (%gc-safe-syscall6 73 (+ a 200) 1 tmo 0 0))))))

;;; Waits for a non-blocking connect on FD to finish, up to SECONDS: 0 connected (getpeername
;;; answers), the error if SO_ERROR shows one, -110 if the time runs out.  It asks the socket
;;; itself rather than poll(2), which did not report the handshake's completion here.
(defun %sbs-await-connect (fd seconds)
  (let ((t0 (get-universal-time)))
    (loop
      (when (>= (%sbs-getname fd 52) 0) (return 0))
      (let ((e (%sbs-getopt fd 1 4)))
        (when (and (>= e 0) (/= e 0)) (return (- e))))
      (when (>= (- (get-universal-time) t0) seconds) (return -110)))))

;;; A connect that cannot finish is an error, not a frozen image: the socket goes non-blocking,
;;; the handshake is waited for up to 10 s, SO_ERROR says how it ended, and the socket goes back
;;; to blocking for the reads and writes that follow.
(defun %sbs-connect-in (fd ip port)
  (let ((a (%sbs-addr-in ip port)))
    (if (zerop a)
        -12
        (progn
          (socket-set-nonblock fd)
          (let ((r (%gc-safe-block-3 203 fd a 16)))
            (when (= r -115)                        ; EINPROGRESS: the handshake runs on
              (setq r (%sbs-await-connect fd 30)))
            (socket-set-block fd)
            r)))))

;;; UDP, unconnected: sendto(2) and recvfrom(2) with the sender.  As in hosted-sockets-post.lisp
;;; the datagram is staged through %SOCK-IO-BUF and the sender is left in this CPU's scratch.

(defun %sbs-sendto (fd arr len ip port)
  "sendto(2): LEN bytes of ARR as ONE datagram to IP:PORT (host-order IP).  The byte count,
   or -errno; -90 (EMSGSIZE) if it does not fit the staging buffer."
  (if (> len (%sock-io-cap))
      -90
      (let ((io (%sock-io-buf)) (i 0))
        (loop
          (when (>= i len) (return nil))
          (setf (mem-ref (+ io i) :u8) (aref arr i))
          (setq i (+ i 1)))
        (let ((a (%sbs-addr-in ip port)))
          (if (zerop a) -12 (%gc-safe-syscall6 206 fd io len 0 a 16))))))            ; sendto

(defun %sbs-recvfrom (fd arr max)
  "recvfrom(2): one datagram into ARR, at most MAX bytes.  The byte count, or -errno.  The
   sender's sockaddr_in stays in this CPU's scratch."
  (let ((a (%sbs-scratch)) (want max))
    (when (> want (%sock-io-cap)) (setq want (%sock-io-cap)))
    (if (zerop a)
        -12
        (progn
          (%sbs-zero a (+ a 24))
          (setf (mem-ref (+ a 16) :u32) 16)        ; socklen_t, in and out
          (let ((n (%gc-safe-syscall6 207 fd (%sock-io-buf) want 0 a (+ a 16))))           ; recvfrom
            (when (> n 0)
              (let ((io (%sock-io-buf)) (i 0))
                (loop
                  (when (>= i n) (return nil))
                  (aset arr i (mem-ref (+ io i) :u8))
                  (setq i (+ i 1)))))
            n)))))

(defun %sbs-from-port ()
  "The port of the last %SBS-RECVFROM's sender, on this CPU."
  (let ((a (%sbs-scratch)))
    (+ (ash (mem-ref (+ a 2) :u8) 8) (mem-ref (+ a 3) :u8))))

(defun %sbs-from-ip ()
  "The host-order IPv4 address of the last %SBS-RECVFROM's sender, on this CPU."
  (let ((a (%sbs-scratch)))
    (+ (* (mem-ref (+ a 4) :u8) 16777216) (* (mem-ref (+ a 5) :u8) 65536)
       (* (mem-ref (+ a 6) :u8) 256) (mem-ref (+ a 7) :u8))))

;;; poll(2) for input, as ppoll(2) (aarch64 has no poll): one pollfd and one timespec in the
;;; scratch, at offsets 200 and 216.  > 0 ready, 0 timed out, < 0 -errno; MS < 0 waits forever.
(defun socket-wait-readable (fd ms)
  (let ((a (%sbs-scratch)))
    (if (zerop a)
        1
        (progn
          (%sbs-zero (+ a 200) (+ a 240))
          (setf (mem-ref (+ a 200) :u32) fd)        ; pollfd.fd
          (setf (mem-ref (+ a 204) :u16) 1)         ; pollfd.events = POLLIN
          (let ((tmo 0))
            (when (>= ms 0)
              (setf (mem-ref (+ a 216) :u32) (floor ms 1000))            ; timespec.tv_sec
              (setf (mem-ref (+ a 224) :u32) (* 1000000 (mod ms 1000)))  ; timespec.tv_nsec
              (setq tmo (+ a 216)))
            (%gc-safe-syscall6 73 (+ a 200) 1 tmo 0 0))))))               ; ppoll (the pollfd is at a+200)

(defun %sbs-poll-in (fd ms) (socket-wait-readable fd ms))

(defun %sbs-bind-un (fd path)
  (let ((len (%sbs-addr-un path)))
    (if (< len 0) -36 (%gc-safe-block-3 200 fd (%sbs-scratch) len))))

(defun %sbs-connect-un (fd path)
  (let ((len (%sbs-addr-un path)))
    (if (< len 0) -36 (%gc-safe-block-3 203 fd (%sbs-scratch) len))))

(defun %sbs-getname (fd which)
  "getsockname / getpeername, PACKED: (ip * 65536) + port, both host order, or a negative
   -errno.  WHICH is x86-64's number (51 or 52) from the callers, mapped to 204 / 205."
  (let ((a (%sbs-scratch))
        (nr (cond ((= which 51) 204) ((= which 52) 205) (t which))))
    (if (zerop a)
        -12
        (progn
          (%sbs-zero a (+ a 64))
          (setf (mem-ref (+ a 128) :u32) 16)
          (let ((r (%gc-safe-block-3 nr fd a (+ a 128))))
            (if (< r 0)
                r
                (+ (* (+ (* (mem-ref (+ a 4) :u8) 16777216)
                         (+ (* (mem-ref (+ a 5) :u8) 65536)
                            (+ (* (mem-ref (+ a 6) :u8) 256)
                               (mem-ref (+ a 7) :u8))))
                      65536)
                   (+ (* (mem-ref (+ a 2) :u8) 256) (mem-ref (+ a 3) :u8)))))))))

(defun %sbs-setopt (fd level opt value)
  "setsockopt(2) with a 4-byte int VALUE."
  (let ((a (%sbs-scratch)))
    (if (zerop a)
        -12
        (progn (setf (mem-ref (+ a 160) :u32) value)
               (%gc-safe-syscall6 208 fd level opt (+ a 160) 4 0)))))

(defun %sbs-getopt (fd level opt)
  "getsockopt(2) of a 4-byte int.  The VALUE, or a negative -errno."
  (let ((a (%sbs-scratch)))
    (if (zerop a)
        -12
        (progn (setf (mem-ref (+ a 160) :u32) 0)
               (setf (mem-ref (+ a 168) :u32) 4)
               (let ((r (%gc-safe-syscall6 209 fd level opt (+ a 160) (+ a 168) 0)))
                 (if (< r 0) r (mem-ref (+ a 160) :u32)))))))
