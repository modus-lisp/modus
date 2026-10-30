;;;; tcp-conns.lisp - many TCP connections (and UDP ports) at once, over ONE receive pump.
;;;;
;;;; net/ip.lisp's client TCP is a single connection in the NIC state block (+#x30..#x50):
;;;; TCP-RECEIVE takes whatever TCP segment arrives as its own, so a second connection's
;;;; segments are swallowed while the first waits.  Here every frame goes through NET-PUMP,
;;;; which hands each TCP segment to the connection its addresses and ports name, and each
;;;; UDP datagram to its port's queue.  Nothing else may read the NIC while these are in use.
;;;;
;;;; Connections are CLOS objects (redefinition migrates live ones).  Ownership: whoever
;;;; drives the NIC runs the pump and owns the connections -- on the threaded Zero that is
;;;; the USB thread, reached from elsewhere through its call-on-thread seam.
;;;;
;;;; Deliberately small: in-order receive (an out-of-order segment is dropped and the ACK
;;;; repeated, the peer retransmits), immediate ACKs, a fixed 32 KB send window over the
;;;; peer's, retransmission with doubling timeout, no SACK, no window scaling.
;;;;
;;;; LOOPBACK: a connection to 127.x.x.x, or to this board's own address, on a port something
;;;; here listens on is a PAIR of connections joined to each other, no frames at all: a write
;;;; appends to the peer's receive buffer, a close is the peer's EOF.  Reads, polls and accepts
;;;; are the network ones unchanged (they look only at the receive buffer, its EOF and the
;;;; listener's ready list), so an image can talk to itself over TCP -- a gateway to the desktop
;;;; it serves, an admission client to the admission service beside it.  Both ends belong to the
;;;; one thread that owns every connection (above), so nothing here races.
;;;;
;;;; ANY THREAD MAY RUN THE NETWORK, ONE STEP AT A TIME.  The connection table, the UDP queues,
;;;; the receive buffers and the NIC's receive ring (one consumer) are guarded by *NET-LOCK*, a
;;;; recursive lock taken for one pump step or one connection operation; a blocking wait
;;;; (%PUMP-UNTIL) releases it between steps, so a gateway's relay pool, its ICE checks and its
;;;; sessions -- each on a thread of its own -- interleave on the one network, and a frame goes
;;;; to the connection it names whichever thread happened to pump it.  On an image without
;;;; threads the lock is modus's single-threaded flag.

(defvar *tcp-conns* nil "Every connection the pump knows, most recent first.")
(defvar *tcp-listeners* nil)
(defvar *udp-ports* nil "Alist port -> list of (ip port bytes), oldest first.")
(defvar *tcp-port-next* 0)
(defvar *net-lock* (sb-thread:make-mutex :name "net") "See ANY THREAD MAY RUN THE NETWORK above.")
(defmacro with-net-lock (&body body)
  `(sb-thread:with-recursive-lock (*net-lock*) ,@body))
(defvar *net-pump-other* nil "Called with (buf len) for frames that are not TCP/UDP for us (ARP).")

(defclass tcp-conn ()
  ((lport :initarg :lport :accessor tc-lport)
   (rip :initarg :rip :accessor tc-rip)
   (rport :initarg :rport :accessor tc-rport)
   (snd-nxt :initarg :snd-nxt :accessor tc-snd-nxt)
   (snd-una :initarg :snd-nxt :accessor tc-snd-una)
   (rcv-nxt :initform 0 :accessor tc-rcv-nxt)
   (state :initform :closed :initarg :state :accessor tc-state)
   (rx :initform (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
       :accessor tc-rx)
   (rx-eof :initform nil :accessor tc-rx-eof)
   (unacked :initform nil :accessor tc-unacked)   ; list of #(seq bytes flags sent-at rto tries)
   (peer-window :initform 65535 :accessor tc-peer-window)
   (listener :initform nil :initarg :listener :accessor tc-listener)
   ;; the other end of a LOOPBACK connection (above), or NIL for one over the network
   (peer :initform nil :accessor tc-peer)))

(defclass tcp-listener ()
  ((port :initarg :port :accessor tl-port)
   (ready :initform nil :accessor tl-ready)))

(defmethod print-object ((c tcp-conn) s)
  (format s "#<TCP ~D -> ~D.~D.~D.~D:~D ~A>" (tc-lport c)
          (ldb (byte 8 24) (tc-rip c)) (ldb (byte 8 16) (tc-rip c))
          (ldb (byte 8 8) (tc-rip c)) (ldb (byte 8 0) (tc-rip c)) (tc-rport c) (tc-state c)))

(defvar *net-ms-fn* nil
  "A function answering milliseconds, for an image whose GET-INTERNAL-REAL-TIME does not tick
   (bare x86-64: it counted 31 -> 32 across a 3M-iteration loop); NIL uses the real-time clock.")
(defun %now-ms ()
  (if *net-ms-fn*
      (funcall *net-ms-fn*)
      (floor (get-internal-real-time) (floor internal-time-units-per-second 1000))))
(defun %u32+ (a b) (logand (+ a b) #xFFFFFFFF))
(defun %seq< (a b) (let ((d (logand (- b a) #xFFFFFFFF))) (and (/= d 0) (< d #x80000000))))
(defun %seq<= (a b) (or (= a b) (%seq< a b)))

(defun %our-ip ()
  (let ((st (e1000-state-base)))
    (logior (ash (mem-ref (+ st #x18) :u8) 24) (ash (mem-ref (+ st #x19) :u8) 16)
            (ash (mem-ref (+ st #x1A) :u8) 8) (mem-ref (+ st #x1B) :u8))))

(defun %sum16 (sum v start n)
  (let ((i 0))
    (loop (when (>= (+ i 1) n) (return))
          (setq sum (+ sum (logior (ash (aref v (+ start i)) 8) (aref v (+ start i 1)))))
          (setq i (+ i 2)))
    (when (< i n) (setq sum (+ sum (ash (aref v (+ start i)) 8))))
    sum))

(defun %l4-checksum (proto src dst seg len)
  (let ((sum (+ (ash src -16) (logand src #xFFFF) (ash dst -16) (logand dst #xFFFF) proto len)))
    (setq sum (%sum16 sum seg 0 len))
    (loop (when (< sum #x10000) (return)) (setq sum (+ (logand sum #xFFFF) (ash sum -16))))
    (logand (lognot sum) #xFFFF)))

(defun %put16 (v o x) (setf (aref v o) (ldb (byte 8 8) x) (aref v (+ o 1)) (ldb (byte 8 0) x)))
(defun %put32 (v o x) (%put16 v o (ldb (byte 16 16) x)) (%put16 v (+ o 2) (ldb (byte 16 0) x)))

;;; FLOW CONTROL.  The window advertised is what is left of a *TCP-RCV-BUF* receive buffer
;;; after the bytes nobody has read yet.  It was a fixed 65535 whatever was unread, so a relay
;;; replaying a backlog faster than the board parsed it grew RX without bound -- by doubling,
;;; and an adjustable (UNSIGNED-BYTE 8) vector here is backed by a general vector, 8 bytes a
;;; byte: three relay connections held 2-4 MB each, kept after draining, and pushed region 0 of
;;; the Zero gateway core into collecting every few hundred ms.  TCPC-READ announces the window
;;; again once a read has reopened it (the peer would otherwise wait for its persist timer).
(defparameter *tcp-rcv-buf* 65536 "Bytes a connection buffers before its window closes.")
(defun %rcv-window (c)
  (max 0 (min 65535 (- *tcp-rcv-buf* (fill-pointer (tc-rx c))))))

(defun %tcp-segment (c flags seq data start n)
  "Build and send one segment of C: FLAGS, sequence SEQ, N bytes of DATA from START."
  (let* ((syn (logbitp 1 flags)) (hdr (if syn 24 20)) (len (+ hdr n))
         (seg (make-array len :initial-element 0)))
    (%put16 seg 0 (tc-lport c)) (%put16 seg 2 (tc-rport c))
    (%put32 seg 4 seq) (%put32 seg 8 (if (logbitp 4 flags) (tc-rcv-nxt c) 0))
    (setf (aref seg 12) (if syn #x60 #x50) (aref seg 13) flags)
    (%put16 seg 14 (%rcv-window c))
    (when syn (setf (aref seg 20) 2 (aref seg 21) 4 (aref seg 22) #x05 (aref seg 23) #xB4))
    (dotimes (i n) (setf (aref seg (+ hdr i)) (aref data (+ start i))))
    (%put16 seg 16 (%l4-checksum 6 (%our-ip) (tc-rip c) seg len))
    (ip-send (tc-rip c) 6 seg len)))

(defun %tcp-emit (c flags &optional (data #()) (start 0) (n 0))
  "Send FLAGS (+ data) at SND-NXT, queue it for retransmission when it occupies sequence space."
  (let ((seq (tc-snd-nxt c))
        (space (+ n (if (logbitp 1 flags) 1 0) (if (logbitp 0 flags) 1 0))))
    (%tcp-segment c flags seq data start n)
    (when (> space 0)
      (let ((bytes (make-array n)))
        (dotimes (i n) (setf (aref bytes i) (aref data (+ start i))))
        (setf (tc-unacked c) (append (tc-unacked c) (list (vector seq bytes flags (%now-ms) 300 0)))))
      (setf (tc-snd-nxt c) (%u32+ seq space)))))

(defun %tcp-ack (c) (%tcp-segment c #x10 (tc-snd-nxt c) #() 0 0))

(defun %tcp-find (rip rport lport)
  (dolist (c *tcp-conns* nil)
    (when (and (= (tc-lport c) lport) (= (tc-rport c) rport) (= (tc-rip c) rip)
               (not (eq (tc-state c) :closed)))
      (return c))))

(defun %tcp-input (buf ihl rip)
  "One TCP segment at BUF (Ethernet frame), IP header length IHL, from RIP."
  (let* ((t0 (+ buf 14 ihl))
         (total (logior (ash (mem-ref (+ buf 16) :u8) 8) (mem-ref (+ buf 17) :u8)))
         (sport (logior (ash (mem-ref t0 :u8) 8) (mem-ref (+ t0 1) :u8)))
         (dport (logior (ash (mem-ref (+ t0 2) :u8) 8) (mem-ref (+ t0 3) :u8)))
         (seq (logior (ash (mem-ref (+ t0 4) :u8) 24) (ash (mem-ref (+ t0 5) :u8) 16)
                      (ash (mem-ref (+ t0 6) :u8) 8) (mem-ref (+ t0 7) :u8)))
         (ack (logior (ash (mem-ref (+ t0 8) :u8) 24) (ash (mem-ref (+ t0 9) :u8) 16)
                      (ash (mem-ref (+ t0 10) :u8) 8) (mem-ref (+ t0 11) :u8)))
         (doff (* 4 (ash (mem-ref (+ t0 12) :u8) -4)))
         (flags (mem-ref (+ t0 13) :u8))
         (win (logior (ash (mem-ref (+ t0 14) :u8) 8) (mem-ref (+ t0 15) :u8)))
         (dlen (- total ihl doff))
         (c (%tcp-find rip sport dport)))
    (cond
      ;; a SYN for a listening port: a new connection, SYN-ACK
      ((and (null c) (logbitp 1 flags) (not (logbitp 4 flags)))
       (let ((l (find dport *tcp-listeners* :key #'tl-port)))
         (when l
           (let ((nc (make-instance 'tcp-conn :lport dport :rip rip :rport sport
                                    :snd-nxt (logand (* 2654435761 (%now-ms)) #xFFFFFFFF)
                                    :state :syn-rcvd :listener l)))
             (setf (tc-rcv-nxt nc) (%u32+ seq 1) (tc-peer-window nc) win)
             (push nc *tcp-conns*)
             (%tcp-emit nc #x12)))))
      ((null c) nil)
      ((logbitp 2 flags) (setf (tc-state c) :closed (tc-rx-eof c) t))      ; RST
      (t
       (setf (tc-peer-window c) win)
       ;; our side: SYN-ACK completes an open
       ;; ACK IT AT ONCE: the handshake's third segment.  Waiting for the first data to carry
       ;; it worked while that data came at once; seal spends seconds making its ClientHello on
       ;; the Zero, and the peer (QEMU's slirp, under capture) re-sent its SYN-ACK at 6 s and
       ;; 18 s and then answered the late data with a FIN.  A repeated SYN-ACK is ACKed again.
       (when (and (logbitp 1 flags) (logbitp 4 flags))
         (when (eq (tc-state c) :syn-sent)
           (setf (tc-rcv-nxt c) (%u32+ seq 1) (tc-state c) :established))
         (%tcp-ack c))
       ;; ACKs retire what they cover
       (when (logbitp 4 flags)
         (when (%seq< (tc-snd-una c) ack) (setf (tc-snd-una c) ack))
         (setf (tc-unacked c)
               (remove-if (lambda (u) (%seq<= (%u32+ (svref u 0)
                                                     (+ (length (svref u 1))
                                                        (if (logbitp 1 (svref u 2)) 1 0)
                                                        (if (logbitp 0 (svref u 2)) 1 0)))
                                              ack))
                          (tc-unacked c)))
         (when (eq (tc-state c) :syn-rcvd)
           (setf (tc-state c) :established)
           (let ((l (tc-listener c))) (when l (setf (tl-ready l) (append (tl-ready l) (list c))))))
         (when (and (eq (tc-state c) :fin-wait) (null (tc-unacked c))) (setf (tc-state c) :fin-wait-2))
         (when (and (eq (tc-state c) :last-ack) (null (tc-unacked c))) (setf (tc-state c) :closed)))
       ;; data, in order only
       (when (and (> dlen 0) (member (tc-state c) '(:established :fin-wait :fin-wait-2)))
         (if (= seq (tc-rcv-nxt c))
             (let ((p (+ t0 doff)) (rx (tc-rx c)))
               (dotimes (i dlen) (vector-push-extend (mem-ref (+ p i) :u8) rx))
               (setf (tc-rcv-nxt c) (%u32+ seq dlen)))
             nil)
         (%tcp-ack c))
       ;; FIN, in order
       (when (and (logbitp 0 flags) (= (%u32+ seq (max dlen 0)) (tc-rcv-nxt c)))
         (setf (tc-rcv-nxt c) (%u32+ (tc-rcv-nxt c) 1) (tc-rx-eof c) t)
         (%tcp-ack c)
         (setf (tc-state c) (case (tc-state c)
                              (:established :close-wait)
                              ((:fin-wait :fin-wait-2) :closed)
                              (t (tc-state c)))))))))

(defun %udp-input (buf ihl rip)
  (let* ((u (+ buf 14 ihl))
         (sport (logior (ash (mem-ref u :u8) 8) (mem-ref (+ u 1) :u8)))
         (dport (logior (ash (mem-ref (+ u 2) :u8) 8) (mem-ref (+ u 3) :u8)))
         (n (- (logior (ash (mem-ref (+ u 4) :u8) 8) (mem-ref (+ u 5) :u8)) 8))
         (q (assoc dport *udp-ports*)))
    (when (and q (>= n 0))
      (let ((v (make-array n :element-type '(unsigned-byte 8))))
        (dotimes (i n) (setf (aref v i) (mem-ref (+ u 8 i) :u8)))
        (setf (cdr q) (append (cdr q) (list (list rip sport v))))))))

(defun %tcp-retransmit ()
  (let ((now (%now-ms)))
    (dolist (c *tcp-conns*)
      (dolist (u (tc-unacked c))
        (when (> (- now (svref u 3)) (svref u 4))
          (when (> (svref u 5) 8) (setf (tc-state c) :closed (tc-rx-eof c) t) (return))
          (let ((b (svref u 1)))
            (%tcp-segment c (svref u 2) (svref u 0) b 0 (length b)))
          (setf (svref u 3) now (svref u 4) (min 4000 (* 2 (svref u 4))) (svref u 5) (+ 1 (svref u 5))))))
    (setf *tcp-conns* (remove :closed *tcp-conns* :key #'tc-state))))

(defun net-pump (&optional (max 64))
  "Take up to MAX frames off the NIC and give each to its connection or port; then
   retransmit what is overdue.  The number of frames taken."
  (with-net-lock (%net-pump-1 max)))

(defun %net-pump-1 (max)
  (let ((n 0) (me (%our-ip)))
    (loop
      (when (>= n max) (return))
      (let ((len (modus.net:nic-receive)))
        (when (zerop len) (return))
        (setq n (+ n 1))
        (let ((buf (modus.net:nic-rx-buf)))
          (if (and (= (mem-ref (+ buf 12) :u8) #x08) (= (mem-ref (+ buf 13) :u8) 0))
              (let* ((ihl (* 4 (logand (mem-ref (+ buf 14) :u8) 15)))
                     (proto (mem-ref (+ buf 23) :u8))
                     (src (logior (ash (mem-ref (+ buf 26) :u8) 24) (ash (mem-ref (+ buf 27) :u8) 16)
                                  (ash (mem-ref (+ buf 28) :u8) 8) (mem-ref (+ buf 29) :u8)))
                     (dst (logior (ash (mem-ref (+ buf 30) :u8) 24) (ash (mem-ref (+ buf 31) :u8) 16)
                                  (ash (mem-ref (+ buf 32) :u8) 8) (mem-ref (+ buf 33) :u8))))
                (when (= dst me)
                  (cond ((= proto 6) (%tcp-input buf ihl src))
                        ((= proto 17) (%udp-input buf ihl src))
                        (*net-pump-other* (funcall *net-pump-other* buf len)))))
              (when *net-pump-other* (funcall *net-pump-other* buf len))))))
    (%tcp-retransmit)
    n))

(defun %pump-until (pred timeout-ms)
  "Pump until PRED is true or TIMEOUT-MS pass; PRED's last value.  The network lock is held
   for each test and pump step and released between them (see the top of the file)."
  (let ((end (+ (%now-ms) timeout-ms)))
    (loop
      (let ((v (with-net-lock (funcall pred)))) (when v (return v)))
      (when (> (%now-ms) end) (return nil))
      (when (zerop (net-pump)) (%net-idle)))))

(defun %net-idle ()
  "Nothing arrived: let this core's other threads run (net/bare-threads.lisp's BT-YIELD --
   on the main thread, core 0's; the other end of a LOOPBACK pair is often one of them),
   and only when none could, wait (IO-DELAY: QEMU's event loop gets its moment, and it is
   5000 device reads -- paid between every chunk of a loopback transfer, it was most of
   the time an RFB frame took)."
  (unless (and (fboundp 'bt-yield) (bt-yield))
    (io-delay)))

;;; ---- the API ------------------------------------------------------------------------------

(defun %loopback-ip-p (ip)
  "127.0.0.0/8, or this board's own address."
  (or (= (ldb (byte 8 24) ip) 127) (= ip (%our-ip))))

(defun %tcpc-open-loopback (ip port)
  "A LOOPBACK pair to local listener PORT (see the top of the file): the client end, with
   the server end on the listener's ready list.  NIL when nothing here listens on PORT."
  (with-net-lock
  (let ((l (find port *tcp-listeners* :key #'tl-port)))
    (when l
      (let* ((lport (+ 49152 (setq *tcp-port-next* (logand (+ *tcp-port-next* 1) #x3FFF))))
             (client (make-instance 'tcp-conn :lport lport :rip ip :rport port
                                              :state :established :snd-nxt 0))
             (server (make-instance 'tcp-conn :lport port :rip ip :rport lport
                                              :state :established :snd-nxt 0 :listener l)))
        (setf (tc-peer client) server (tc-peer server) client)
        (setf (tl-ready l) (append (tl-ready l) (list server)))
        client)))))

(defun tcpc-open (ip port &key (timeout-ms 3000))
  "A connection to IP:PORT, established, or NIL."
  (when (%loopback-ip-p ip)
    (return-from tcpc-open (%tcpc-open-loopback ip port)))
  (let ((c (make-instance 'tcp-conn :lport (+ 49152 (setq *tcp-port-next* (logand (+ *tcp-port-next* 1) #x3FFF)))
                          :rip ip :rport port :state :syn-sent
                          :snd-nxt (logand (* 2654435761 (+ (%now-ms) *tcp-port-next*)) #xFFFFFFFF))))
    (with-net-lock (push c *tcp-conns*) (%tcp-emit c #x02))
    (if (%pump-until (lambda () (member (tc-state c) '(:established :closed))) timeout-ms)
        (if (eq (tc-state c) :established) c (progn (setf (tc-state c) :closed) nil))
        (progn (setf (tc-state c) :closed) nil))))

(defun tcpc-write (c data &key (start 0) end (timeout-ms 10000))
  "Send DATA[START, END) on C, segment by segment, keeping at most 32 KB unacknowledged."
  (when (tc-peer c)
    ;; LOOPBACK: straight into the other end's receive buffer
    (let ((p (tc-peer c)) (end (or end (length data))))
      (unless (and (eq (tc-state c) :established) (not (eq (tc-state p) :closed)))
        (return-from tcpc-write nil))
      ;; One copy, not a VECTOR-PUSH-EXTEND per byte: grow the buffer once (keeping what
      ;; ADJUST-ARRAY returns), then REPLACE the slice in.
      (with-net-lock
        (let* ((rx (tc-rx p)) (fp (fill-pointer rx)) (need (+ fp (- end start))))
          (when (> need (array-dimension rx 0))
            (setf rx (adjust-array rx (max need (* 2 (array-dimension rx 0))))
                  (tc-rx p) rx))
          (setf (fill-pointer rx) need)
          (replace rx data :start1 fp :start2 start :end2 end)))
      (return-from tcpc-write t)))
  (let ((end (or end (length data))) (pos start))
    (loop
      (when (>= pos end) (return t))
      (unless (member (tc-state c) '(:established :close-wait)) (return nil))
      (unless (%pump-until (lambda () (< (logand (- (tc-snd-nxt c) (tc-snd-una c)) #xFFFFFFFF)
                                         (min 32768 (max 1460 (tc-peer-window c)))))
                           timeout-ms)
        (return nil))
      (let ((n (min 1460 (- end pos))))
        (with-net-lock (%tcp-emit c #x18 data pos n))
        (setq pos (+ pos n))))))

(defun tcpc-read (c &key (max 65536) (timeout-ms 5000) into)
  "Up to MAX bytes C has received, waiting up to TIMEOUT-MS for any; :EOF once the peer
   has closed and everything is read; an empty vector on timeout.  With INTO, an octet
   vector of at least MAX, the bytes go there and the answer is their count (0 on timeout):
   a socket read then allocates nothing."
  (%pump-until (lambda () (or (> (fill-pointer (tc-rx c)) 0) (tc-rx-eof c))) timeout-ms)
  (with-net-lock
    (let* ((rx (tc-rx c)) (have (fill-pointer rx)))
      (if (and (zerop have) (tc-rx-eof c))
          :eof
          (let* ((n (min max have)) (out (or into (make-array n :element-type '(unsigned-byte 8))))
                 (was (%rcv-window c)))
            (replace out rx :end2 n)
            (replace rx rx :start2 n :end2 have)
            (setf (fill-pointer rx) (- have n))
            ;; drained: give back a buffer a burst grew (it costs 8 bytes a byte) -- a big one
            ;; only: below that, a desk's every update grew it again and it churned
            (when (and (= have n) (> (array-dimension rx 0) 262144))
              (setf (tc-rx c) (make-array 4096 :element-type '(unsigned-byte 8)
                                               :adjustable t :fill-pointer 0)))
            ;; window update, over the network only, once the read reopened a quarter of it
            (when (and (null (tc-peer c)) (member (tc-state c) '(:established :fin-wait :fin-wait-2))
                       (< was (ash *tcp-rcv-buf* -2)) (>= (%rcv-window c) (ash *tcp-rcv-buf* -2)))
              (%tcp-ack c))
            (if into n out))))))

(defun tcpc-close (c &key (timeout-ms 2000))
  "Send FIN and wait (briefly) for it to be acknowledged."
  (when (tc-peer c)
    ;; LOOPBACK: the other end reads EOF once it has read what was written
    (with-net-lock (setf (tc-rx-eof (tc-peer c)) t (tc-state c) :closed))
    (return-from tcpc-close t))
  (with-net-lock
    (case (tc-state c)
      (:established (%tcp-emit c #x11) (setf (tc-state c) :fin-wait))
      (:close-wait (%tcp-emit c #x11) (setf (tc-state c) :last-ack))))
  (%pump-until (lambda () (null (tc-unacked c))) timeout-ms)
  (when (member (tc-state c) '(:fin-wait-2 :last-ack :fin-wait)) (setf (tc-state c) :closed))
  t)

(defun tcpc-listen (port)
  (with-net-lock
  (or (find port *tcp-listeners* :key #'tl-port)
      (let ((l (make-instance 'tcp-listener :port port))) (push l *tcp-listeners*) l))))

(defun tcpc-accept (l &key (timeout-ms 5000))
  "The next established connection to listener L, or NIL."
  (when (%pump-until (lambda () (tl-ready l)) timeout-ms)
    (with-net-lock (pop (tl-ready l)))))

(defun udp-open (port)
  (with-net-lock (or (assoc port *udp-ports*) (car (push (list port) *udp-ports*))))
  port)

(defun udp-send-to (ip port lport data &optional (n (length data)))
  (with-net-lock
  (let* ((len (+ 8 n)) (seg (make-array len :initial-element 0)))
    (%put16 seg 0 lport) (%put16 seg 2 port) (%put16 seg 4 len)
    (dotimes (i n) (setf (aref seg (+ 8 i)) (aref data i)))
    (%put16 seg 6 (%l4-checksum 17 (%our-ip) ip seg len))
    (ip-send ip 17 seg len))))

(defun udp-recv (port &key (timeout-ms 1000))
  "(ip port bytes) for the oldest datagram to PORT, or NIL."
  (let ((q (assoc port *udp-ports*)))
    (when (%pump-until (lambda () (cdr q)) timeout-ms)
      (with-net-lock (pop (cdr q))))))
