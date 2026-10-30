;;;; sb-bsd-sockets-shim.lisp — the SB-BSD-SOCKETS surface: classes, conditions, addresses,
;;;; SOCKET-CONNECT and friends, GET-HOST-BY-NAME.
;;;;
;;;; Split out of net/sb-sys-shim.lisp so a BARE-METAL image can carry it without the Linux-only
;;;; SB-POSIX / SB-SYS / SB-EXT surface beside it.  It stands on descriptor primitives
;;;; (%SBS-OPEN, %SBS-CONNECT-IN, SOCKET-SEND, SOCKET-RECV, ...): syscalls in hosted images
;;;; (net/hosted-sockets*.lisp), net/bare-sockets.lisp over net/tcp-conns.lisp on bare metal.
;;;; The notes on each function, and on what is PARTIAL, are the ones it had in sb-sys-shim.

;;; Kernel code loaded at runtime (kiln zero carries it), so read in the kernel's
;;; package: the internals it calls and defines are MODUS-INT's, not CL-USER's.
(in-package :modus-int)

(defpackage "SB-BSD-SOCKETS"
  (:use)
  (:export "SOCKET" "INET-SOCKET" "INET6-SOCKET" "LOCAL-SOCKET"
           "SOCKET-BIND" "SOCKET-LISTEN" "SOCKET-ACCEPT" "SOCKET-CONNECT"
           "SOCKET-CLOSE" "SOCKET-SHUTDOWN" "SOCKET-MAKE-STREAM"
           "SOCKET-FILE-DESCRIPTOR" "SOCKET-NAME" "SOCKET-PEERNAME"
           "SOCKOPT-TCP-NODELAY" "SOCKOPT-REUSE-ADDRESS" "SOCKOPT-KEEP-ALIVE"
           "MAKE-INET-ADDRESS" "GET-HOST-BY-NAME" "HOST-ENT" "HOST-ENT-ADDRESS"
           "HOST-ENT-ADDRESSES" "HOST-ENT-NAME"
           "SOCKET-ERROR" "SOCKET-ERROR-ERRNO" "CONNECTION-REFUSED-ERROR"
           "ADDRESS-IN-USE-ERROR" "INTERRUPTED-ERROR"
           "SOCKET-SEND" "SOCKET-RECEIVE" "NON-BLOCKING-MODE" "OPERATION-IN-PROGRESS"))


(pushnew :sb-bsd-sockets *features*)

;;; ============================================================
;;; SB-BSD-SOCKETS
;;; ============================================================

(define-condition sb-bsd-sockets:socket-error (error)
  ((errno :initarg :errno :initform 0 :reader sb-bsd-sockets:socket-error-errno)))
(define-condition sb-bsd-sockets:connection-refused-error
    (sb-bsd-sockets:socket-error) ())
(define-condition sb-bsd-sockets:address-in-use-error
    (sb-bsd-sockets:socket-error) ())
(define-condition sb-bsd-sockets:interrupted-error
    (sb-bsd-sockets:socket-error) ())
;; EINPROGRESS: a non-blocking connect that has not finished (webrtc-data, cl-transport
;; handle it by name).
(define-condition sb-bsd-sockets:operation-in-progress
    (sb-bsd-sockets:socket-error) ())

(defun %sock-signal (r)
  "Turn a negative -errno into the condition SBCL would signal.  ECONNREFUSED
   is 111, EADDRINUSE 98, EINTR 4; everything else is the base SOCKET-ERROR
   carrying its number, which is better than inventing a class for it."
  (let ((e (- 0 r)))
    (cond ((= e 111) (error 'sb-bsd-sockets:connection-refused-error :errno e))
          ((= e 98)  (error 'sb-bsd-sockets:address-in-use-error :errno e))
          ((= e 4)   (error 'sb-bsd-sockets:interrupted-error :errno e))
          (t (error 'sb-bsd-sockets:socket-error :errno e)))))

(defclass sb-bsd-sockets:socket ()
  ((fd :initarg :fd :initform nil :accessor sb-bsd-sockets:socket-file-descriptor)
   ;; SLOT NAMES SOCK-TYPE / SOCK-PROTOCOL AND NOT TYPE / PROTOCOL: `:type' is
   ;; also a DEFCLASS slot OPTION, and a slot whose name collides with an option
   ;; keyword is the kind of thing that reads correctly and parses differently.
   ;;
   ;; THE INITARGS ARE :TYPE AND :PROTOCOL, WHICH ARE NOT THE SLOT NAMES, and
   ;; that is the entire point.  A slot's initarg is independent of its name,
   ;; and the initarg is not ours to choose: `sb-bsd-sockets' spells it :TYPE,
   ;; every caller in the world writes
   ;;     (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)
   ;; and glass writes it five times (src/socket.lisp:280,300,332,513,514).
   ;; The first draft named the initargs after the slots, so every one of those
   ;; five call sites died with `invalid initarg' at the first socket a real
   ;; program opened — the shim was portable-SHAPED in its arguments and
   ;; shim-shaped in its initargs, which is the same mistake one level down.
   (family :initarg :family :initform 2 :accessor %socket-family)
   (sock-type :initarg :type :initform :stream :accessor %socket-type)
   (sock-protocol :initarg :protocol :initform :tcp
                  :accessor %socket-protocol)))

(defclass sb-bsd-sockets:inet-socket (sb-bsd-sockets:socket) ())
(defclass sb-bsd-sockets:inet6-socket (sb-bsd-sockets:socket) ())
(defclass sb-bsd-sockets:local-socket (sb-bsd-sockets:socket) ())

(defun %socket-family-of (socket)
  (cond ((typep socket 'sb-bsd-sockets:local-socket) 1)   ; AF_UNIX
        ((typep socket 'sb-bsd-sockets:inet6-socket)
         (error "sb-bsd-sockets:inet6-socket is not implemented on modus ~
                 (its address builder is sockaddr_in only)."))
        (t 2)))                                            ; AF_INET

(defun %socket-fd (socket)
  "The descriptor, opening it if it does not have one yet.

   IT NORMALLY HAS ONE ALREADY: INITIALIZE-INSTANCE opens it, as SBCL's does.
   This remains the single place a descriptor is created, so a socket that
   somehow reached here without one still gets a working one rather than
   handing NIL to a syscall."
  (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (if fd
        fd
        (let* ((fam (%socket-family-of socket))
               (ty (if (eq (%socket-type socket) :datagram) 2 1))
               (r (%sbs-open fam ty)))
          (if (< r 0)
              (%sock-signal r)
              (progn (setf (slot-value socket 'fd) r) r))))))

(defmethod initialize-instance :after ((socket sb-bsd-sockets:socket) &rest initargs)
  "OPEN THE DESCRIPTOR AT MAKE-INSTANCE, WHICH IS WHERE SBCL OPENS IT.

   The first draft of this shim opened it lazily, at the first bind/connect, and
   argued that nothing in glass depended on the earlier point.  That was true of
   glass and false of the SURFACE: `(sb-bsd-sockets:socket-file-descriptor
   sock)' on a freshly made socket answers a descriptor under SBCL and answered
   NIL here — so any caller that reaches for the fd before binding (to set an
   option this shim does not name, to hand it to a poll set, to log it) got NIL
   and then handed NIL to a syscall.  A shim that is portable-SHAPED in its
   arguments has to be portable-shaped in WHEN things happen too, wherever that
   is observable, and this one was observable with a single accessor call.

   A SOCKET MADE WITH :FD ALREADY HAS ONE — that is SOCKET-ACCEPT wrapping a
   descriptor the kernel just handed it — so this must not open a second one and
   leak the first.  %SOCKET-FD is exactly that test, so it is what runs here.

   The cost of being faithful is that MAKE-INSTANCE can now SIGNAL (EMFILE, or
   the INET6 refusal), which is also what SBCL's does."
  initargs
  (%socket-fd socket)
  socket)

;;; ---- addresses ----------------------------------------------------------

(defun sb-bsd-sockets:make-inet-address (dotted)
  "\"127.0.0.1\" -> #(127 0 0 1), the vector shape sb-bsd-sockets uses."
  (let ((v (make-array 4))
        (i 0) (n 0) (part 0))
    (loop
      (when (>= i (length dotted)) (return nil))
      (let ((c (char dotted i)))
        (if (char= c #\.)
            (progn (aset v part n) (setq part (+ part 1)) (setq n 0))
            (setq n (+ (* n 10) (- (char-code c) 48)))))
      (setq i (+ i 1)))
    (aset v part n)
    v))

(defun %inet-host-order (addr)
  "An sb-bsd-sockets address vector, or a host-order integer, as a host-order
   integer."
  (if (integerp addr)
      addr
      (+ (* (aref addr 0) 16777216)
         (+ (* (aref addr 1) 65536)
            (+ (* (aref addr 2) 256) (aref addr 3))))))

(defun %local-socket-p (socket) (typep socket 'sb-bsd-sockets:local-socket))

(defun sb-bsd-sockets:socket-bind (socket &rest address)
  "bind(2).  ADDRESS is (host port) for an inet socket and (path) for a local
   one, exactly as sb-bsd-sockets spells it."
  (let* ((fd (%socket-fd socket))
         (r (if (%local-socket-p socket)
                (%sbs-bind-un fd (car address))
                (%sbs-bind-in fd (%inet-host-order (car address)) (cadr address)))))
    (if (< r 0) (%sock-signal r) socket)))

(defun sb-bsd-sockets:socket-listen (socket backlog)
  "listen(2)."
  (let ((r (%sbs-listen (%socket-fd socket) backlog)))
    (if (< r 0) (%sock-signal r) socket)))

(defun sb-bsd-sockets:socket-accept (socket)
  "accept(2) — syscall 43.  Returns a NEW socket of the same class, or NIL when
   the listener is non-blocking and nothing was waiting (EAGAIN), which is
   sb-bsd-sockets' contract."
  (let ((r (%sbs-accept (%socket-fd socket))))
    (cond ((>= r 0)
           ;; THE SAME CLASS, BY EXPLICIT TEST.  (make-instance (class-of x))
           ;; would be the obvious spelling; this shim does not use it because
           ;; MAKE-INSTANCE of a class OBJECT rather than a class NAME is not
           ;; something this image is known to do, and there are exactly two
           ;; classes it can be.
           (if (typep socket 'sb-bsd-sockets:local-socket)
               (make-instance 'sb-bsd-sockets:local-socket :fd r
                              :type (%socket-type socket)
                              :protocol (%socket-protocol socket))
               (make-instance 'sb-bsd-sockets:inet-socket :fd r
                              :type (%socket-type socket)
                              :protocol (%socket-protocol socket))))
          ((= r -11) nil)                       ; EAGAIN
          (t (%sock-signal r)))))

(defun sb-bsd-sockets:socket-connect (socket &rest address)
  "connect(2)."
  (let* ((fd (%socket-fd socket))
         (r (if (%local-socket-p socket)
                (%sbs-connect-un fd (car address))
                (%sbs-connect-in fd (%inet-host-order (car address))
                                 (cadr address)))))
    (if (< r 0) (%sock-signal r) socket)))

(defun sb-bsd-sockets:socket-close (socket)
  "close(2).  Idempotent: closing an already-closed socket is a no-op rather
   than an error, which is what every cleanup path in portable code expects."
  (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (if (null fd)
        nil
        (progn (%sbs-close fd)
               (setf (slot-value socket 'fd) nil)
               t))))

(defun sb-bsd-sockets:socket-shutdown (socket &key direction)
  "shutdown(2) — syscall 48.  DIRECTION is :INPUT, :OUTPUT or :IO.

   LOAD-BEARING FOR GLASS: it is how a parked ACCEPT is cancelled
   (src/socket.lisp:420-440), so a wrong `how' here is a listener that never
   comes back."
  (let ((how (cond ((eq direction :input) 0)
                   ((eq direction :output) 1)
                   ((eq direction :io) 2)
                   ((null direction) 2)
                   (t (error "sb-bsd-sockets:socket-shutdown: unknown ~
                              direction ~S" direction))))
        (fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (if (null fd)
        nil
        (let ((r (%sbs-shutdown fd how)))
          ;; ENOTCONN (107) on shutdown is not an error worth signalling: the
          ;; peer is already gone, which is the state the caller wanted.
          (if (or (>= r 0) (= r -107)) socket (%sock-signal r))))))

(defun %sock-getname (socket which)
  "getsockname / getpeername as (values address port).  The syscall is issued
   from AOT code (%SBS-GETNAME) and comes back PACKED as (ip * 65536) + port;
   this unpacks it.  See THE SYSCALL FLOOR in net/hosted-sockets-post.lisp for
   why a shim function may not issue it itself."
  (let ((r (%sbs-getname (%socket-fd socket) which)))
    (if (< r 0)
        (%sock-signal r)
        (let ((ip (truncate r 65536))
              (v (make-array 4)))
          (aset v 0 (logand (ash ip -24) 255))
          (aset v 1 (logand (ash ip -16) 255))
          (aset v 2 (logand (ash ip -8) 255))
          (aset v 3 (logand ip 255))
          (values v (- r (* ip 65536)))))))

(defun sb-bsd-sockets:socket-name (socket) (%sock-getname socket 51))
(defun sb-bsd-sockets:socket-peername (socket) (%sock-getname socket 52))

(defun sb-bsd-sockets:socket-send (socket buffer length &key address external-format)
  "Write LENGTH bytes (all of BUFFER when NIL) of the (unsigned-byte 8) vector
   BUFFER; returns the count written.  ADDRESS, (host port), sends one datagram
   there (sendto).  PARTIAL: EXTERNAL-FORMAT is not supported and must be NIL."
  external-format
  (when address
    ;; a datagram to (host port): sendto(2), one datagram or none
    (let ((n (%sbs-sendto (%socket-fd socket) buffer (or length (length buffer))
                          (%inet-host-order (car address)) (cadr address))))
      (when (< n 0) (error 'sb-bsd-sockets:socket-error :errno (- n)))
      (return-from sb-bsd-sockets:socket-send n)))
  (let ((n (socket-send (%socket-fd socket) buffer (or length (length buffer)))))
    (if (< n 0)
        (error 'sb-bsd-sockets:socket-error :errno (- n))
        n)))

(defun sb-bsd-sockets:socket-receive (socket buffer length &key oob peek waitall element-type)
  "One read(2) of up to LENGTH bytes (BUFFER's length when NIL) into BUFFER,
   or into a fresh (unsigned-byte 8) vector of LENGTH when BUFFER is NIL.
   Returns (values buffer count nil); count 0 = the peer closed.  On a
   :DATAGRAM socket, one datagram and its sender: (values buffer count address
   port), as SBCL returns them.  PARTIAL: OOB,
   PEEK, WAITALL and ELEMENT-TYPE are accepted and ignored."
  oob peek waitall element-type
  (when (eq (%socket-type socket) :datagram)
    ;; a datagram socket says who sent it: (values buffer count address port),
    ;; SBCL's contract, the address a 4-element vector
    (let* ((buf (or buffer (make-array length :element-type '(unsigned-byte 8))))
           (n (%sbs-recvfrom (%socket-fd socket) buf (or length (length buf)))))
      (when (< n 0) (error 'sb-bsd-sockets:socket-error :errno (- n)))
      (let ((ip (%sbs-from-ip)) (port (%sbs-from-port)) (v (make-array 4)))
        (aset v 0 (logand (ash ip -24) 255)) (aset v 1 (logand (ash ip -16) 255))
        (aset v 2 (logand (ash ip -8) 255)) (aset v 3 (logand ip 255))
        (return-from sb-bsd-sockets:socket-receive (values buf n v port)))))
  (let* ((buf (or buffer (make-array length :element-type '(unsigned-byte 8))))
         (n (socket-recv (%socket-fd socket) buf (or length (length buf)))))
    (if (< n 0)
        (error 'sb-bsd-sockets:socket-error :errno (- n))
        (values buf n nil))))

(defun sb-bsd-sockets:socket-make-stream (socket &key input output
                                                      (element-type 'character)
                                                      buffering timeout
                                                      external-format)
  "A Lisp stream over the socket's descriptor.

   PARTIAL: :BUFFERING, :TIMEOUT and :EXTERNAL-FORMAT are accepted and IGNORED.
   modus's fd streams are always buffered, never time out, and pass 8-bit bytes
   through unchanged.  A :TIMEOUT that was silently honoured as `no timeout'
   would be a read that blocks where the caller asked it not to — glass's
   `serve' never passes one (src/rfb.lisp:1047 takes every default), so the
   honest thing is to say so here rather than to refuse the argument and break
   a caller that passes it harmlessly."
  buffering timeout external-format
  (%make-file-stream-full (%socket-fd socket)
                          (cond ((and input (not output)) 0)
                                ((and output (not input)) 1)
                                (t 2))
                          element-type))

;;; ---- socket options -----------------------------------------------------
;;;
;;; SETF OF A FUNCTION AND NOT OF AN ACCESSOR, and it is not a style choice:
;;; measured on this image, `(setf (foreign-package:accessor obj) v)' expands to
;;; a call to FOREIGN-PACKAGE::SET-ACCESSOR, which DEFCLASS :ACCESSOR does not
;;; define — while a `(defun (setf name) …)' IS found.  So every setf-able
;;; option here is written as an explicit (setf …) function.

(defun sb-bsd-sockets:sockopt-tcp-nodelay (socket)
  (let ((v (%sbs-getopt (%socket-fd socket) 6 1)))
    (if (< v 1) nil t)))

(defun (setf sb-bsd-sockets:sockopt-tcp-nodelay) (value socket)
  "TCP_NODELAY.  Load-bearing for an interactive protocol: without it every RFB
   frame pays a 40 ms Nagle delay between its header and its payload."
  (%sbs-setopt (%socket-fd socket) 6 1 (if value 1 0))
  value)

(defun sb-bsd-sockets:sockopt-reuse-address (socket)
  (let ((v (%sbs-getopt (%socket-fd socket) 1 2)))
    (if (< v 1) nil t)))

(defun (setf sb-bsd-sockets:sockopt-reuse-address) (value socket)
  "SO_REUSEADDR — setsockopt(fd, SOL_SOCKET = 1, SO_REUSEADDR = 2, &v, 4)."
  (%sbs-setopt (%socket-fd socket) 1 2 (if value 1 0))
  value)

(defun sb-bsd-sockets:sockopt-keep-alive (socket)
  (let ((v (%sbs-getopt (%socket-fd socket) 1 9)))
    (if (< v 1) nil t)))

(defun (setf sb-bsd-sockets:sockopt-keep-alive) (value socket)
  "SO_KEEPALIVE = 9."
  (%sbs-setopt (%socket-fd socket) 1 9 (if value 1 0))
  value)

;;; ---- names --------------------------------------------------------------

(defclass sb-bsd-sockets:host-ent ()
  ((name :initarg :name :accessor sb-bsd-sockets:host-ent-name)
   (addresses :initarg :addresses :accessor sb-bsd-sockets:host-ent-addresses)))

(defun sb-bsd-sockets:host-ent-address (host-ent)
  (car (sb-bsd-sockets:host-ent-addresses host-ent)))

(defun %shim-split-ws (line)
  "LINE's whitespace-separated fields, up to a # comment."
  (let ((fields nil) (cur nil) (i 0) (n (length line)))
    (loop
      (when (>= i n) (return nil))
      (let ((c (char line i)))
        (cond ((char= c #\#) (return nil))
              ((or (char= c #\Space) (char= c #\Tab) (char= c #\Return))
               (when cur (push (coerce (nreverse cur) 'string) fields) (setq cur nil)))
              (t (push c cur))))
      (setq i (+ i 1)))
    (when cur (push (coerce (nreverse cur) 'string) fields))
    (nreverse fields)))

(defun %shim-dotted-quad-int (s)
  "The host-order integer of a dotted-quad STRING, or NIL if it is not one."
  (let ((parts nil) (acc 0) (digits 0) (i 0) (n (length s)) (ok t))
    (loop
      (when (>= i n) (return nil))
      (let ((c (char s i)))
        (cond ((digit-char-p c)
               (setq acc (+ (* acc 10) (digit-char-p c)) digits (+ digits 1)))
              ((and (char= c #\.) (> digits 0))
               (push acc parts) (setq acc 0 digits 0))
              (t (setq ok nil) (return nil))))
      (setq i (+ i 1)))
    (when (and ok (> digits 0)) (push acc parts))
    (setq parts (nreverse parts))
    (if (and ok (= (length parts) 4) (every (lambda (x) (<= x 255)) parts))
        (+ (* (first parts) 16777216) (* (second parts) 65536)
           (* (third parts) 256) (fourth parts))
        nil)))

(defun %shim-file-lines (path)
  "The lines of the file at PATH, or NIL if it cannot be read."
  (handler-case
      (with-open-file (in path)
        (let ((acc nil))
          (loop
            (let ((l (read-line in nil nil)))
              (when (null l) (return nil))
              (push l acc)))
          (nreverse acc)))
    (error () nil)))

(defun %shim-hosts-lookup (name)
  "NAME's IPv4 address string from /etc/hosts, or NIL."
  (dolist (line (%shim-file-lines "/etc/hosts") nil)
    (let ((f (%shim-split-ws line)))
      (when (and f (%shim-dotted-quad-int (car f))
                 (member name (cdr f) :test #'string-equal))
        (return (car f))))))

(defun %shim-nameservers ()
  "The IPv4 nameservers /etc/resolv.conf names, as host-order integers, in its
   order; 8.8.8.8 last, as the fallback when it names none that answer."
  (let ((acc nil))
    (dolist (line (%shim-file-lines "/etc/resolv.conf"))
      (let ((f (%shim-split-ws line)))
        (when (and (= (length f) 2) (string= (car f) "nameserver"))
          (let ((ip (%shim-dotted-quad-int (cadr f))))
            (when ip (push ip acc))))))
    (nreverse (cons 134744072 acc))))

(defun sb-bsd-sockets:get-host-by-name (name)
  "Resolve NAME to its IPv4 address.

   A dotted quad answers itself; then /etc/hosts; then each IPv4 nameserver in
   /etc/resolv.conf in order (UDP, then TCP), with 8.8.8.8 last.  This shim
   used to refuse every non-numeric name because the image had no resolv.conf
   reader and it would not pick a public resolver on the caller's behalf --
   the configured resolver is the one to ask, and now it is asked.  IPv4 only
   (DNS-LOOKUP asks for A records).  Signals SOCKET-ERROR when nothing answers."
  (let ((quad (cond ((%shim-dotted-quad-int name) name)
                    ((%shim-hosts-lookup name))
                    (t (let* ((n (length name))
                              (codes (make-array n))
                              (found nil))
                         (dotimes (i n) (aset codes i (char-code (char name i))))
                         (dolist (ns (%shim-nameservers))
                           (unless found
                             (let ((ip (handler-case (dns-lookup codes n ns nil) (error () 0))))
                               (when (eql ip 0)
                                 (setq ip (handler-case (dns-lookup codes n ns t) (error () 0))))
                               (when (and (integerp ip) (> ip 0))
                                 (setq found (%format-dotted-ip ip))))))
                         found)))))
    (if quad
        (make-instance 'sb-bsd-sockets:host-ent :name name
                       :addresses (list (sb-bsd-sockets:make-inet-address quad)))
        (error 'sb-bsd-sockets:socket-error :errno 2))))

