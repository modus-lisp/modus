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

(defvar *tcp-conns* nil "Every connection the pump knows, most recent first.")
(defvar *tcp-listeners* nil)
(defvar *udp-ports* nil "Alist port -> list of (ip port bytes), oldest first.")
(defvar *tcp-port-next* 0)
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
   (listener :initform nil :initarg :listener :accessor tc-listener)))

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

(defun %tcp-segment (c flags seq data start n)
  "Build and send one segment of C: FLAGS, sequence SEQ, N bytes of DATA from START."
  (let* ((syn (logbitp 1 flags)) (hdr (if syn 24 20)) (len (+ hdr n))
         (seg (make-array len :initial-element 0)))
    (%put16 seg 0 (tc-lport c)) (%put16 seg 2 (tc-rport c))
    (%put32 seg 4 seq) (%put32 seg 8 (if (logbitp 4 flags) (tc-rcv-nxt c) 0))
    (setf (aref seg 12) (if syn #x60 #x50) (aref seg 13) flags)
    (%put16 seg 14 65535)
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
  "Pump until PRED is true or TIMEOUT-MS pass; PRED's last value."
  (let ((end (+ (%now-ms) timeout-ms)))
    (loop
      (let ((v (funcall pred))) (when v (return v)))
      (when (> (%now-ms) end) (return nil))
      (when (zerop (net-pump)) (io-delay)))))

;;; ---- the API ------------------------------------------------------------------------------

(defun tcpc-open (ip port &key (timeout-ms 3000))
  "A connection to IP:PORT, established, or NIL."
  (let ((c (make-instance 'tcp-conn :lport (+ 49152 (setq *tcp-port-next* (logand (+ *tcp-port-next* 1) #x3FFF)))
                          :rip ip :rport port :state :syn-sent
                          :snd-nxt (logand (* 2654435761 (+ (%now-ms) *tcp-port-next*)) #xFFFFFFFF))))
    (push c *tcp-conns*)
    (%tcp-emit c #x02)
    (if (%pump-until (lambda () (member (tc-state c) '(:established :closed))) timeout-ms)
        (if (eq (tc-state c) :established) c (progn (setf (tc-state c) :closed) nil))
        (progn (setf (tc-state c) :closed) nil))))

(defun tcpc-write (c data &key (start 0) end (timeout-ms 10000))
  "Send DATA[START, END) on C, segment by segment, keeping at most 32 KB unacknowledged."
  (let ((end (or end (length data))) (pos start))
    (loop
      (when (>= pos end) (return t))
      (unless (member (tc-state c) '(:established :close-wait)) (return nil))
      (unless (%pump-until (lambda () (< (logand (- (tc-snd-nxt c) (tc-snd-una c)) #xFFFFFFFF)
                                         (min 32768 (max 1460 (tc-peer-window c)))))
                           timeout-ms)
        (return nil))
      (let ((n (min 1460 (- end pos))))
        (%tcp-emit c #x18 data pos n)
        (setq pos (+ pos n))))))

(defun tcpc-read (c &key (max 65536) (timeout-ms 5000))
  "Up to MAX bytes C has received, waiting up to TIMEOUT-MS for any; :EOF once the peer
   has closed and everything is read; an empty vector on timeout."
  (%pump-until (lambda () (or (> (fill-pointer (tc-rx c)) 0) (tc-rx-eof c))) timeout-ms)
  (let* ((rx (tc-rx c)) (have (fill-pointer rx)))
    (if (and (zerop have) (tc-rx-eof c))
        :eof
        (let* ((n (min max have)) (out (make-array n :element-type '(unsigned-byte 8))))
          (dotimes (i n) (setf (aref out i) (aref rx i)))
          (replace rx rx :start2 n :end2 have)
          (setf (fill-pointer rx) (- have n))
          out))))

(defun tcpc-close (c &key (timeout-ms 2000))
  "Send FIN and wait (briefly) for it to be acknowledged."
  (case (tc-state c)
    (:established (%tcp-emit c #x11) (setf (tc-state c) :fin-wait))
    (:close-wait (%tcp-emit c #x11) (setf (tc-state c) :last-ack)))
  (%pump-until (lambda () (null (tc-unacked c))) timeout-ms)
  (when (member (tc-state c) '(:fin-wait-2 :last-ack :fin-wait)) (setf (tc-state c) :closed))
  t)

(defun tcpc-listen (port)
  (or (find port *tcp-listeners* :key #'tl-port)
      (let ((l (make-instance 'tcp-listener :port port))) (push l *tcp-listeners*) l)))

(defun tcpc-accept (l &key (timeout-ms 5000))
  "The next established connection to listener L, or NIL."
  (when (%pump-until (lambda () (tl-ready l)) timeout-ms)
    (pop (tl-ready l))))

(defun udp-open (port) (or (assoc port *udp-ports*) (car (push (list port) *udp-ports*))) port)

(defun udp-send-to (ip port lport data &optional (n (length data)))
  (let* ((len (+ 8 n)) (seg (make-array len :initial-element 0)))
    (%put16 seg 0 lport) (%put16 seg 2 port) (%put16 seg 4 len)
    (dotimes (i n) (setf (aref seg (+ 8 i)) (aref data i)))
    (%put16 seg 6 (%l4-checksum 17 (%our-ip) ip seg len))
    (ip-send ip 17 seg len)))

(defun udp-recv (port &key (timeout-ms 1000))
  "(ip port bytes) for the oldest datagram to PORT, or NIL."
  (let ((q (assoc port *udp-ports*)))
    (when (%pump-until (lambda () (cdr q)) timeout-ms)
      (pop (cdr q)))))
