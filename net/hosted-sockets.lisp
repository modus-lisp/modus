;;;; hosted-sockets.lisp — Linux TCP + UDP sockets for the HOSTED modus CLI.
;;;;
;;;; The bare-metal images talk to a NIC driver (E1000/DWC2) and our own
;;;; TCP/IP stack (net/ip.lisp).  The Linux-hosted `./modus' CLI instead uses
;;;; the KERNEL's networking via plain syscalls.  Everything here is
;;;; CONNECTED-socket only, so socket/connect/read/write/close are each <= 3
;;;; args and ride the existing `syscall3' trap (num->RAX, args->RDI/RSI/RDX,
;;;; result tagged).  No 6-arg sendto/recvfrom needed — a connected UDP socket
;;;; gives us DNS, and connected TCP gives us HTTP.
;;;;
;;;; x86-64 Linux syscalls used: socket=41, connect=42, read=0, write=1,
;;;; close=3.  sockaddr_in is 16 bytes: [family:u16 host-order][port:u16
;;;; net-order][addr:u32 net-order][8 zero].  IPs here are host-order 32-bit
;;;; ints (a<<24|b<<16|c<<8|d), matching net/http-client's parse-ip-addr, and
;;;; are emitted big-endian byte-by-byte into the sockaddr.

;;; ============================================================
;;; THE BOUNCE-BUFFER SEAM
;;; ============================================================
;;;
;;; Every socket transfer copies through a raw buffer, because the syscall
;;; wants a machine address and a Lisp array is a managed object with no
;;; exposed data pointer.  WHERE that buffer lives is the whole thread-safety
;;; question, so it is a SEAM: three one-line functions that a later file
;;; overrides.  Everything below is written against the seam and never against
;;; a particular address, so the same source is correct single-threaded and
;;; correct on two CPUs.
;;;
;;; THE DEFAULT, kept here for every target that has no threads (aarch64 and
;;; i386 CLIs, and any bare-metal image that bakes this file): carve from the
;;; CLI's already-mapped cstr/io pages (setup.lisp file-load proves they're
;;; backed).  Sockaddr goes at the top of the cstr page (paths never reach
;;; +3072); socket data reuses the io buffer.
;;;
;;; THREE THINGS ARE WRONG WITH THAT DEFAULT ON A THREADED IMAGE, and
;;; net/hosted-sockets-post.lisp answers all three by overriding these three
;;; functions.  They are recorded here because the default is still what a
;;; single-CPU image runs:
;;;
;;;   1. IT IS ONE BUFFER FOR EVERY CPU.  Two threads inside SOCKET-SEND at
;;;      once write the same page and each other's bytes go out the wrong fd.
;;;   2. IT IS SHARED WITH FILE I/O.  mvm/cl-fileio.lisp reads and writes
;;;      through *IO-BUF-ADDR* as well.  The original comment here claimed "no
;;;      file I/O interleaves a socket op in the fetch->write flow", which was
;;;      true of the HTTP client it was written for and is false of a server
;;;      that logs, or of anything that reads a file while a connection is open.
;;;   3. IT IS ONE PAGE.  %SOCK-IO-CAP names that limit instead of leaving it
;;;      implicit, and the transfer loops below CHUNK against it — so a
;;;      transfer is bounded by nothing at all, whatever the buffer's size.
(defun %sock-addr-buf () (+ *cstr-scratch* 3072))   ; >= 64 bytes of address scratch
(defun %sock-io-buf   () *io-buf-addr*)             ; the data staging buffer
(defun %sock-io-cap   () 4096)                      ; ... and how big it is

;;; Build sockaddr_in(AF_INET, port, ip) at the addr buffer; return its addr.
;;; PORT is host-order (e.g. 53); IP is host-order (0x08080808 for 8.8.8.8).
(defun %sock-build-addr (ip port)
  (let ((a (%sock-addr-buf)))
    (setf (mem-ref a :u16) 2)                                ; sin_family = AF_INET (host order)
    (setf (mem-ref (+ a 2) :u8) (logand (ash port -8) 255))  ; sin_port hi (network order)
    (setf (mem-ref (+ a 3) :u8) (logand port 255))           ; sin_port lo
    (setf (mem-ref (+ a 4) :u8) (logand (ash ip -24) 255))   ; sin_addr octet 1
    (setf (mem-ref (+ a 5) :u8) (logand (ash ip -16) 255))
    (setf (mem-ref (+ a 6) :u8) (logand (ash ip -8) 255))
    (setf (mem-ref (+ a 7) :u8) (logand ip 255))
    (setf (mem-ref (+ a 8) :u32) 0)                          ; 8-byte zero pad
    (setf (mem-ref (+ a 12) :u32) 0)
    a))

;;; ---- raw socket primitives (fd < 0 = error / -errno) ----
(defun %sock-open (type)       (syscall3 41 2 type 0))   ; socket(AF_INET, type, 0)
(defun %sock-connect-fd (fd)   (syscall3 42 fd (%sock-addr-buf) 16))
(defun %sock-write (fd len)    (syscall3 1 fd (%sock-io-buf) len))
(defun %sock-read  (fd len)    (syscall3 0 fd (%sock-io-buf) len))
(defun socket-close (fd)       (syscall3 3 fd 0 0))

;;; Open + connect a socket.  STREAM-P non-nil -> TCP (SOCK_STREAM=1), else
;;; UDP (SOCK_DGRAM=2).  Returns the fd, or -1 on any failure.
(defun socket-connect (ip port stream-p)
  (let ((fd (%sock-open (if stream-p 1 2))))
    (if (< fd 0)
        -1
        (progn
          (%sock-build-addr ip port)
          (if (< (%sock-connect-fd fd) 0)
              (progn (socket-close fd) -1)
              fd)))))

;;; ---- transfers, bounded by nothing ----
;;;
;;; The old SOCKET-SEND copied LEN bytes into the buffer and issued ONE
;;; write(2).  That was wrong twice over: LEN above the buffer's size wrote
;;; whatever followed the buffer in memory, and write(2) on a socket is allowed
;;; to accept FEWER bytes than it was offered whenever the send queue fills —
;;; which is exactly what a large framebuffer update does.  The old code
;;; returned that short count and every caller in the tree ignored it, so the
;;; tail of the message was silently dropped.  Both are gone: the loop below
;;; chunks against %SOCK-IO-CAP and advances by what write(2) actually took.

;;; Write LEN bytes of ARR (an (unsigned-byte 8) array) starting at OFF to
;;; connected FD, looping until they are all gone.  Returns the number of bytes
;;; written — LEN on success, a SHORT count if the peer went away mid-transfer,
;;; and a negative -errno only if NOTHING was written (write(2)'s own
;;; convention, scaled up to the whole message).
(defun socket-send-from (fd arr off len)
  (let ((cap (%sock-io-cap)) (sent 0) (bad 0))
    (loop
      (when (>= sent len) (return nil))
      (let ((chunk (- len sent)))
        (when (> chunk cap) (setq chunk cap))
        (let ((io (%sock-io-buf)) (i 0))
          (loop
            (when (>= i chunk) (return nil))
            (setf (mem-ref (+ io i) :u8) (aref arr (+ off (+ sent i))))
            (setq i (+ i 1))))
        (let ((n (%sock-write fd chunk)))
          (when (< n 1) (setq bad n) (return nil))
          (setq sent (+ sent n)))))
    (if (> sent 0) sent bad)))

(defun socket-send (fd arr len) (socket-send-from fd arr 0 len))

;;; Receive up to MAX bytes into ARR at OFF.  Returns byte count (0 = peer
;;; closed, negative = -errno).  ONE read(2), so a caller that wants an exact
;;; count loops (SOCKET-RECV-FULLY does).  MAX above the staging buffer's
;;; capacity is clamped rather than overrunning it: a single read(2) cannot be
;;; split, and a short read is what read(2) means anyway.
(defun socket-recv-into (fd arr off max)
  (let ((cap (%sock-io-cap)) (want max))
    (when (> want cap) (setq want cap))
    (let ((n (%sock-read fd want)))
      (if (< n 1)
          n
          (let ((io (%sock-io-buf)) (i 0))
            (loop
              (when (>= i n) (return n))
              (aset arr (+ off i) (mem-ref (+ io i) :u8))
              (setq i (+ i 1))))))))

(defun socket-recv (fd arr max) (socket-recv-into fd arr 0 max))

;;; Read EXACTLY N bytes into ARR at OFF, looping over as many read(2)s as it
;;; takes.  Returns N; a SHORT count if the peer closed first; a negative
;;; -errno only if the first read failed.
(defun socket-recv-fully (fd arr off n)
  (let ((got 0) (bad 0))
    (loop
      (when (>= got n) (return nil))
      (let ((r (socket-recv-into fd arr (+ off got) (- n got))))
        (when (< r 1) (setq bad r) (return nil))
        (setq got (+ got r))))
    (if (> got 0) got (if (< bad 0) bad 0))))

;;; ============================================================
;;; Server sockets — bind / listen / accept (TCP)
;;; ============================================================
;;; For the modus-lisp relays/servers (glass VNC, skep Nostr relay,
;;; cl-frpc, cl-rustdesk).  bind(49)/listen(50)/accept(43) are all <= 3
;;; args, so they ride syscall3 too.  (SO_REUSEADDR needs setsockopt =
;;; 5 args -> a syscall6 trap; deferred with sendto/recvfrom for UDP
;;; multi-peer.  Without it a just-closed listen port sits in TIME_WAIT
;;; for ~60s before it can rebind.)
;;;
;;; ***NOTHING HERE LISTENS UNLESS A CALLER ASKS IT TO, IN SO MANY WORDS.***
;;; Loading this file opens no socket; starting the `./modus' CLI opens no
;;; socket; there is no autostart, no default port and no environment variable
;;; that turns one on.  A listening socket exists only because some caller
;;; evaluated SOCKET-LISTEN, and the process holds it only until SOCKET-CLOSE.
;;;
;;; AND THE ADDRESS IS TWO SEPARATE DECISIONS, not one parameter with a
;;; permissive default.  SOCKET-LISTEN binds 127.0.0.1 and nothing else, so the
;;; ordinary spelling is reachable only from this machine.  Serving the network
;;; is SOCKET-LISTEN-ON with an address the caller wrote down — a different
;;; function name, so it appears in a diff and cannot be arrived at by leaving
;;; an argument off.  (This file previously bound INADDR_ANY by default, which
;;; is the reverse of that.)

;;; setsockopt(fd, SOL_SOCKET=1, SO_REUSEADDR=2, &1, 4) via syscall6 (5 args,
;;; a6 ignored).  Lets a just-closed listen port rebind without the ~60s
;;; TIME_WAIT wait.  Returns the syscall result (0 = ok).
(defun %sock-set-reuseaddr (fd)
  (let ((opt (+ (%sock-addr-buf) 16)))       ; 4 bytes above the sockaddr scratch
    (setf (mem-ref opt :u32) 1)
    (syscall6 54 fd 1 2 opt 4 0)))

;;; Host-order IPv4 constants, so a bind address is never a bare magic number.
(defun %sock-loopback ()  2130706433)   ; 127.0.0.1  — this machine only
(defun %sock-any-addr ()  0)            ; 0.0.0.0    — every interface

;;; Open a TCP socket, set SO_REUSEADDR, bind IP:PORT, and listen with BACKLOG.
;;; Returns the listening fd, or -1 on any failure.  IP is a host-order 32-bit
;;; address; PORT 0 asks the kernel for an ephemeral port, which
;;; SOCKET-LOCAL-PORT then reads back.
;;;
;;; THIS IS THE OFF-LOOPBACK SPELLING.  Calling it with anything other than
;;; (%SOCK-LOOPBACK) exposes the port to the network, and the whole reason it
;;; is a separate function from SOCKET-LISTEN is that doing so has to be
;;; written down.  Prefer SOCKET-LISTEN unless you mean it.
(defun socket-listen-on (ip port backlog)
  (let ((fd (%sock-open 1)))                 ; SOCK_STREAM
    (if (< fd 0)
        -1
        (progn
          (%sock-set-reuseaddr fd)           ; rebind without TIME_WAIT
          (%sock-build-addr ip port)
          (if (< (syscall3 49 fd (%sock-addr-buf) 16) 0)   ; bind
              (progn (socket-close fd) -1)
              (if (< (syscall3 50 fd backlog 0) 0)         ; listen
                  (progn (socket-close fd) -1)
                  fd))))))

;;; The ordinary way to listen: 127.0.0.1:PORT, reachable from this machine and
;;; from nowhere else.  PORT 0 = let the kernel choose (see SOCKET-LOCAL-PORT).
;;; Returns the listening fd, or -1.
(defun socket-listen (port backlog)
  (socket-listen-on (%sock-loopback) port backlog))

;;; The port this fd is actually bound to, via getsockname(51).  The point of
;;; it is bind-port-0-and-ask: a caller never has to pick a number, guess
;;; whether it is free, or race another process for it.  Returns the host-order
;;; port, or -1.
(defun socket-local-port (fd)
  (let ((a (%sock-addr-buf)))
    (setf (mem-ref (+ a 32) :u32) 16)        ; socklen_t in/out, above the sockaddr
    (if (< (syscall3 51 fd a (+ a 32)) 0)
        -1
        (+ (ash (mem-ref (+ a 2) :u8) 8) (mem-ref (+ a 3) :u8)))))

;;; ---- unconnected UDP (sendto/recvfrom, 6-arg) — for multi-peer datagrams
;;; (webrtc STUN/ICE) where a single connected socket won't do ----

;;; sendto LEN bytes of ARR to IP:PORT on unconnected UDP FD.  Returns bytes
;;; sent, or negative.  sendto(fd, buf, len, 0, dest_addr, 16).
;;; A datagram is one write or none, so this CLAMPS to the staging capacity
;;; rather than chunking: splitting it would send two datagrams and change the
;;; meaning.  Returns a negative -errno if LEN does not fit.
(defun udp-sendto (fd arr len ip port)
  (if (> len (%sock-io-cap))
      -90                                    ; -EMSGSIZE: would not fit the buffer
      (progn
        (let ((io (%sock-io-buf)) (i 0))
          (loop
            (when (>= i len) (return nil))
            (setf (mem-ref (+ io i) :u8) (aref arr i))
            (setq i (+ i 1))))
        (%sock-build-addr ip port)
        (syscall6 44 fd (%sock-io-buf) len 0 (%sock-addr-buf) 16))))

;;; recvfrom up to MAX bytes into ARR on unconnected UDP FD (peer addr
;;; discarded).  Returns byte count, or negative.  recvfrom(fd,buf,max,0,0,0).
(defun udp-recvfrom (fd arr max)
  (let ((want max))
    (when (> want (%sock-io-cap)) (setq want (%sock-io-cap)))
    (let ((n (syscall6 45 fd (%sock-io-buf) want 0 0 0)))
      (if (< n 1)
          n
          (let ((io (%sock-io-buf)) (i 0))
            (loop
              (when (>= i n) (return n))
              (aset arr i (mem-ref (+ io i) :u8))
              (setq i (+ i 1))))))))

;;; syscall6 self-proof: resolve NAME via an UNCONNECTED UDP socket using
;;; sendto+recvfrom (vs dns-lookup's connected send/recv).  Returns host-order
;;; IP int, or 0.  Exercises the 6-arg trap end-to-end against a live resolver.
(defun dns-lookup-unconnected (name nlen resip)
  (let ((fd (%sock-open 2)))                 ; UDP, NOT connected
    (if (< fd 0)
        0
        (let ((qbuf (make-array 300)) (rbuf (make-array 600)))
          (let ((qlen (%dns-build-query name nlen qbuf nil)))
            (udp-sendto fd qbuf qlen resip 53)
            (let ((n (udp-recvfrom fd rbuf 600)))
              (socket-close fd)
              (if (< n 12) 0 (%dns-parse-a rbuf n 0))))))))

;;; Accept one connection on listening LFD.  Peer address is discarded
;;; (accept(fd, NULL, NULL)).  Returns the connected client fd, or -1.
;;; Blocks until a client connects.
(defun socket-accept (lfd)
  (let ((c (syscall3 43 lfd 0 0)))
    (if (< c 0) -1 c)))

;;; Self-test / minimal server: listen on 127.0.0.1:PORT, accept ONE
;;; connection, read up to MAX bytes, echo them straight back, close both fds.
;;; Returns the number of bytes echoed, or -1 on a setup error.  Proves
;;; bind+listen+accept+recv+send against an external client.  Nothing calls
;;; this; it runs when a human or a test asks for it and it closes both fds
;;; before it returns.  The real server is net/hosted-sockets-post.lisp.
(defun socket-echo-once (port max)
  (let ((lfd (socket-listen port 4)))
    (if (< lfd 0)
        -1
        (let ((cfd (socket-accept lfd)))
          (if (< cfd 0)
              (progn (socket-close lfd) -1)
              (let ((buf (make-array max)))
                (let ((n (socket-recv cfd buf max)))
                  (when (> n 0) (socket-send cfd buf n))
                  (socket-close cfd)
                  (socket-close lfd)
                  n)))))))

;;; ============================================================
;;; DNS over UDP (or TCP) — the first real use of the socket layer
;;; ============================================================

;;; Build a DNS standard A-query for NAME (a byte array of len NLEN) into
;;; QBUF; return total length.  Header (12) + QNAME labels + QTYPE/QCLASS.
(defun %dns-build-query (name nlen qbuf tcp-p)
  ;; TCP DNS prefixes a 2-byte length; leave room and fill it after.
  (let ((base (if tcp-p 2 0)) (p 0))
    (let ((b base))
      ;; --- 12-byte header: id=0x2A2A, flags=0x0100 (RD), qd=1, others 0 ---
      (aset qbuf b 42)        (aset qbuf (+ b 1) 42)     ; id
      (aset qbuf (+ b 2) 1)   (aset qbuf (+ b 3) 0)      ; flags: recursion desired
      (aset qbuf (+ b 4) 0)   (aset qbuf (+ b 5) 1)      ; qdcount = 1
      (aset qbuf (+ b 6) 0)   (aset qbuf (+ b 7) 0)      ; ancount
      (aset qbuf (+ b 8) 0)   (aset qbuf (+ b 9) 0)      ; nscount
      (aset qbuf (+ b 10) 0)  (aset qbuf (+ b 11) 0)     ; arcount
      (setq p (+ b 12))
      ;; --- QNAME: length-prefixed labels split on '.' (46) ---
      (let ((i 0) (label-start 0))
        (loop
          (when (> i nlen) (return nil))
          (if (or (= i nlen) (= (aref name i) 46))
              (let ((llen (- i label-start)))
                (aset qbuf p llen)
                (setq p (+ p 1))
                (let ((j label-start))
                  (loop
                    (when (>= j i) (return nil))
                    (aset qbuf p (aref name j))
                    (setq p (+ p 1))
                    (setq j (+ j 1))))
                (setq label-start (+ i 1)))
              nil)
          (setq i (+ i 1))))
      (aset qbuf p 0) (setq p (+ p 1))                   ; root label
      (aset qbuf p 0) (aset qbuf (+ p 1) 1) (setq p (+ p 2))  ; QTYPE = A
      (aset qbuf p 0) (aset qbuf (+ p 1) 1) (setq p (+ p 2))  ; QCLASS = IN
      ;; TCP length prefix = payload length (p - 2).
      (when tcp-p
        (let ((plen (- p 2)))
          (aset qbuf 0 (logand (ash plen -8) 255))
          (aset qbuf 1 (logand plen 255))))
      p)))

;;; Parse the first A record's IPv4 out of a DNS response RESP of length RLEN.
;;; Skips the header + question, then walks answers for TYPE=A (1), CLASS=IN
;;; (1), RDLENGTH=4.  Handles compressed names (0xC0 pointer) in the NAME
;;; field.  OFF0 = start of the DNS message (2 for TCP framing, else 0).
;;; Returns host-order IP int, or 0 if none.
(defun %dns-parse-a (resp rlen off0)
  (let ((ancount (+ (ash (aref resp (+ off0 6)) 8) (aref resp (+ off0 7))))
        (p (+ off0 12)))
    ;; Skip the single question: QNAME (labels to 0), then QTYPE+QCLASS (4).
    (loop
      (when (>= p rlen) (return nil))
      (let ((len (aref resp p)))
        (when (= len 0) (setq p (+ p 1)) (return nil))
        (setq p (+ p 1 len))))
    (setq p (+ p 4))
    ;; Walk ANCOUNT answer RRs.
    (let ((seen 0) (result 0))
      (loop
        (when (>= seen ancount) (return result))
        (when (>= p rlen) (return result))
        ;; NAME: a 0xC0-pointer (2 bytes) or a label sequence ending in 0.
        (if (>= (aref resp p) 192)
            (setq p (+ p 2))
            (loop
              (when (>= p rlen) (return nil))
              (let ((len (aref resp p)))
                (when (= len 0) (setq p (+ p 1)) (return nil))
                (setq p (+ p 1 len)))))
        ;; TYPE(2) CLASS(2) TTL(4) RDLENGTH(2)
        (let ((rtype (+ (ash (aref resp p) 8) (aref resp (+ p 1))))
              (rdlen (+ (ash (aref resp (+ p 8)) 8) (aref resp (+ p 9)))))
          (let ((rdata (+ p 10)))
            (if (and (= rtype 1) (= rdlen 4) (= result 0))
                (setq result (logior (ash (aref resp rdata) 24)
                                     (ash (aref resp (+ rdata 1)) 16)
                                     (ash (aref resp (+ rdata 2)) 8)
                                     (aref resp (+ rdata 3)))))
            (setq p (+ rdata rdlen))))
        (setq seen (+ seen 1)))
      result)))

;;; Resolve NAME (byte array, len NLEN) to a host-order IPv4 int via a
;;; connected UDP query to resolver IP RESIP:53 (host-order int).  TCP-P
;;; non-nil uses DNS-over-TCP instead.  Returns 0 on failure.
(defun dns-lookup (name nlen resip tcp-p)
  (let ((fd (socket-connect resip 53 tcp-p)))
    (if (< fd 0)
        0
        (let ((qbuf (make-array 300)) (rbuf (make-array 600)))
          (let ((qlen (%dns-build-query name nlen qbuf tcp-p)))
            (socket-send fd qbuf qlen)
            (let ((n (socket-recv fd rbuf 600)))
              (socket-close fd)
              (if (< n 12)
                  0
                  ;; TCP frames the message with a 2-byte length; UDP doesn't.
                  (%dns-parse-a rbuf n (if tcp-p 2 0)))))))))

;;; Google DNS 8.8.8.8 as a host-order int, for a default resolver.
(defun %default-resolver () 134744072)   ; 8<<24 | 8<<16 | 8<<8 | 8

;;; Convenience: resolve a Lisp STRING host name to a printed dotted IP, or
;;; "0.0.0.0" on failure.  Proves the whole socket+DNS path from the REPL.
(defun resolve-name (host-string tcp-p)
  (let* ((n (length host-string))
         (name (make-array n)) (i 0))
    (loop
      (when (>= i n) (return nil))
      (aset name i (char-code (char host-string i)))
      (setq i (+ i 1)))
    (let ((ip (dns-lookup name n (%default-resolver) tcp-p)))
      (if (= ip 0)
          "0.0.0.0"
          (%format-dotted-ip ip)))))

(defun %cat3 (a b c) (concatenate-strings (concatenate-strings a b) c))
(defun %format-dotted-ip (ip)
  ;; concatenate-strings is strictly 2-arg — nest.
  (%cat3
   (%cat3 (%int-to-decstr (logand (ash ip -24) 255)) "."
          (%int-to-decstr (logand (ash ip -16) 255)))
   "."
   (%cat3 (%int-to-decstr (logand (ash ip -8) 255)) "."
          (%int-to-decstr (logand ip 255)))))

(defun %int-to-decstr (n)
  (if (= n 0) "0"
      (let ((digits nil))
        (loop
          (when (= n 0) (return nil))
          (setq digits (cons (code-char (+ 48 (rem n 10))) digits))
          (setq n (truncate n 10)))
        (coerce digits 'string))))
