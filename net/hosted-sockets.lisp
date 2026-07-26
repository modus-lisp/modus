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

;;; Scratch: carve from the CLI's already-mapped cstr/io pages (setup.lisp
;;; file-load proves they're backed).  Sockaddr goes at the top of the cstr
;;; page (paths never reach +3072); socket data reuses the io buffer (no
;;; file I/O interleaves a socket op in the fetch->write flow).
(defun %sock-addr-buf () (+ *cstr-scratch* 3072))   ; 16 bytes, isolated from cstr paths
(defun %sock-io-buf   () *io-buf-addr*)              ; up to one page of data

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

;;; Send LEN bytes of ARR (an (unsigned-byte 8) array) on connected FD.
;;; Returns bytes written, or negative on error.
(defun socket-send (fd arr len)
  (let ((io (%sock-io-buf)) (i 0))
    (loop
      (when (>= i len) (return nil))
      (setf (mem-ref (+ io i) :u8) (aref arr i))
      (setq i (+ i 1))))
  (%sock-write fd len))

;;; Receive up to MAX bytes into ARR.  Returns byte count (0 = peer closed,
;;; negative = error).  One read(2); caller loops for more.
(defun socket-recv (fd arr max)
  (let ((n (%sock-read fd max)))
    (if (< n 1)
        n
        (let ((io (%sock-io-buf)) (i 0))
          (loop
            (when (>= i n) (return n))
            (aset arr i (mem-ref (+ io i) :u8))
            (setq i (+ i 1)))))))

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
