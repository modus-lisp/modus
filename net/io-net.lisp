;;;; io-net.lisp - the network stack ON THE I/O CORE: ARP, ICMP echo, IPv4, TCP and UDP,
;;;; run by the USB engine's own loop (net/usb-io-core.lisp USBE-MAIN calls ION-STEP).
;;;;
;;;; WHY HERE.  Core 0 is the Lisp system: the REPL, the desk, every evaluation, anything that
;;;; interns or sets a global.  A busy evaluation there must hold up only Lisp, never the
;;;; network, and nothing on core 1 ever waits for Lisp.  So the network lives with the engine
;;;; that owns the NIC, and the rest of the image reaches it through memory.
;;;;
;;;; THE RULES OF CORE 1 (the engine's, docs/usb-io-core.md): no allocation, no heap pointer
;;;; held, no global set, no symbol, keyword or string literal (each is a heap object).  Only
;;;; fixnums, raw memory and the controller.  Arithmetic with two arguments only: MIN, MAX and
;;;; 3-argument LOGIOR take &REST and cons.  Everything core 1 runs is in this file or the
;;;; engine's, and the engine counts any pass that moved the allocation pointer (+0x48).
;;;;
;;;; THE BLOCK: a static region at ION-BASE that no collector owns.  It holds no Lisp object
;;;; that points anywhere -- only words and bytes -- so any core may read and write it and no
;;;; collection ever needs to know about it.  Who writes what is fixed, so most of it needs
;;;; no lock: a ring has one producer and one consumer.  The LOCK word guards only allocating a
;;;; connection, listener or UDP port, which a client and core 1 both do; core 1 only ever
;;;; TRIES it (a SYN that finds it held is dropped and the peer sends it again).
;;;;
;;;; TCP DATA NEVER LEAVES THE BLOCK UNTIL READ.  Each connection has a 64 KB receive ring and a
;;;; 64 KB send ring, indexed by sequence number: the byte with sequence ISS+1+K is at K mod
;;;; 64 KB.  Core 1 copies a segment's payload into the receive ring once; the reader copies it
;;;; out once, into its own array.  A writer copies into the send ring once; core 1 segments
;;;; straight from it, and retransmits from it -- the unacknowledged bytes are still there, so
;;;; nothing is kept per segment.  The advertised window is the receive ring's free space.
;;;;
;;;; Deliberately small, as net/tcp-conns.lisp was, with one exception the board measured: data
;;;; past ONE hole is kept (written into the ring where it belongs, the range remembered) -- the
;;;; peer bursts up to 8 KB and a segment goes missing now and then, and a capture showed what
;;;; dropping everything after it does: the sender (no SACK here) recovering one at a time.  ACKs are one per connection per pass (a
;;;; duplicate ACK for out-of-order data goes at once, for fast retransmit); go-back-N
;;;; retransmission with a doubling
;;;; timeout, an MSS of 1460, at most 32 KB in flight, no SACK, no window scaling, TIME-WAIT
;;;; skipped.  ARP: one next hop, as ip.lisp has always had (the board's link is point to
;;;; point to the machine it is wired to): its MAC is learnt from ARP and used for every
;;;; destination.
;;;;
;;;; LAYOUT (u32 words unless said; ION-BASE = 0x11800000, Normal-WB, 8 MB free on the board):
;;;;   +0x00 'IONT'  +0x04 on  +0x08 lock (8 bytes, XCHG-MEM)  +0x10 GENERATION: bumped by core 1
;;;;   whenever something a client may be waiting for changed -- every wait watches it
;;;;   +0x14 counter ticks per ms  +0x18 now (ms, core 1's clock)  +0x1C next local port
;;;;   +0x20.. counters (ION-STATUS)  +0x5C next ARP request (ms)  +0x60 boot started under
;;;   +0x80 legacy ring head (core 1), +0x84 tail (reader),
;;;;   +0x88 reader holds a frame
;;;;   +0x100 8 listeners x 0x20: +0 port (0 free) +4 accept head (core 1) +8 tail (client)
;;;;          +0x10 four connection indices
;;;;   +0x200 8 UDP ports x 0x10: +0 port (0 free) +4 head (core 1) +8 tail (client)
;;;;   +0x1000 32 connections x 0x80 (ION-C), +0x3000 core 1's transmit frame,
;;;;   +0x3800 + core*0x800 a client core's UDP transmit frame
;;;;   +0x10000 legacy ring: 32 x 2 KB (+0 length, +8 frame) -- what core 1 does not take, for
;;;;            the old interface (ip.lisp's ARP-RESOLVE, its one-connection TCP, DHCP)
;;;;   +0x20000 UDP rings: port P's 16 slots x 2 KB at +0x20000 + P*0x8000 (+0 length,
;;;;            +4 source IP, +8 source port, +16 data)
;;;;   +0x100000 + i*0x20000 connection i's receive ring, +0x10000 its send ring (64 KB each)
;;;;
;;;; CONNECTION (ION-C i), the writer of each word in brackets:
;;;;   +0x00 state [both, see below]  +0x04 local port  +0x08 remote IP  +0x0C remote port
;;;;   +0x10 ISS  +0x14 SND.UNA  +0x18 SND.NXT  +0x1C RCV.NXT  +0x20 peer window  [core 1]
;;;;   +0x24 RTO ms  +0x28 retransmit deadline (ms, 0 none)  +0x2C tries  [core 1]
;;;;   +0x30 bytes written, ever (mod 2^32) [client]  +0x34 bytes received, ever [core 1]
;;;;   +0x38 bytes read, ever [client]  +0x3C client flags: 1 close, 2 abort, 4 released
;;;;   +0x40 core-1 flags: 1 peer finished (EOF), 2 reset or failed, 4 our FIN is out
;;;;   +0x44 generation (bumped when the slot is freed: a stale handle is caught)
;;;;   +0x48 listener + 1  +0x4C window last advertised  +0x50/+0x54 the one out-of-order range
;;;;   held past a hole, [start, end) in sequence space (empty when equal)  +0x58 an ACK is due
;;;; States: 0 free, 1 open requested (by a client), 2 SYN-SENT, 3 SYN-RCVD, 4 ESTABLISHED,
;;;; 5 FIN-WAIT-1, 6 FIN-WAIT-2, 7 CLOSE-WAIT, 8 LAST-ACK, 9 CLOSING, 10 CLOSED.  A client
;;;; writes only 0 -> 1 (under the lock); core 1 writes the rest, and frees a CLOSED slot once
;;;; the client has released it.
;;;;
;;;; A HANDLE is a fixnum, generation * 64 + index.

(in-package :modus-int)

(defun ion-base () #x11800000)
(defun ion-get (off) (mem-ref (+ (ion-base) off) :u32))
(defun ion-put (off v) (setf (mem-ref (+ (ion-base) off) :u32) (logand v #xFFFFFFFF)))
(defun ion-inc (off) (ion-put off (+ (ion-get off) 1)))
;; ON means on IN THIS BOOT: the block is not cleared by a reset, and DRAM decays during one
;; (a reset left records of 0xAAAAAAAA that core 1 then served as connections).  +0x60 holds
;; the kmsg boot number it started under, as the engine's +0x64 does.
(defun ion-on-p ()
  (and (= (ion-get 0) #x544E4F49) (= (ion-get 4) 1) (= (ion-get #x60) (%usbe-boot))))

(defun ion-c (i) (+ (ion-base) #x1000 (* i #x80)))
(defun ion-cget (i off) (mem-ref (+ (ion-c i) off) :u32))
(defun ion-cput (i off v) (setf (mem-ref (+ (ion-c i) off) :u32) (logand v #xFFFFFFFF)))
(defun ion-rx-ring (i) (+ (ion-base) #x100000 (* i #x20000)))
(defun ion-tx-ring (i) (+ (ion-base) #x110000 (* i #x20000)))
(defun ion-listener (k) (+ (ion-base) #x100 (* k #x20)))
(defun ion-udp-port (k) (+ (ion-base) #x200 (* k #x10)))
(defun ion-udp-slot (k n) (+ (ion-base) #x20000 (* k #x8000) (* (logand n 15) #x800)))
(defun ion-legacy-slot (n) (+ (ion-base) #x10000 (* (logand n 31) #x800)))
(defun ion-frame () (+ (ion-base) #x3000))

(defun ion-nconns () 32)
(defun ion-mss () 1460)

;; two-argument helpers: MIN and MAX take &REST
(defun %ion-min (a b) (if (< a b) a b))
(defun %ion-u32+ (a b) (logand (+ a b) #xFFFFFFFF))
(defun %ion-u32- (a b) (logand (- a b) #xFFFFFFFF))
;; A after B in sequence space (mod 2^32), and A at or after B
(defun %ion-seq> (a b) (let ((d (%ion-u32- a b))) (and (/= d 0) (< d #x80000000))))
(defun %ion-seq>= (a b) (or (= a b) (%ion-seq> a b)))

;;; ---- bytes on the wire ------------------------------------------------------------------
(defun %ion-u16 (a) (logior (ash (mem-ref a :u8) 8) (mem-ref (+ a 1) :u8)))
(defun %ion-u32 (a) (logior (ash (%ion-u16 a) 16) (%ion-u16 (+ a 2))))
(defun %ion-put16 (a v)
  (setf (mem-ref a :u8) (logand (ash v -8) #xFF))
  (setf (mem-ref (+ a 1) :u8) (logand v #xFF)))
(defun %ion-put32 (a v)
  (%ion-put16 a (logand (ash v -16) #xFFFF))
  (%ion-put16 (+ a 2) (logand v #xFFFF)))
(defun %ion-copy (dst src n)
  (let ((i 0))
    (loop (when (>= i n) (return nil))
          (setf (mem-ref (+ dst i) :u8) (mem-ref (+ src i) :u8))
          (setq i (+ i 1)))))

;; The NIC's addresses, where ip.lisp keeps them (E1000-STATE-BASE): our MAC +0x08, our IP
;; +0x18 (network order), the next hop's MAC +0x28.
(defun %ion-st () (e1000-state-base))
(defun ion-our-ip () (%ion-u32 (+ (%ion-st) #x18)))

;;; The ones'-complement sum of N bytes at A, added to SUM (unfolded).
(defun %ion-sum (sum a n)
  (let ((i 0))
    (loop (when (>= (+ i 1) n) (return nil))
          (setq sum (+ sum (%ion-u16 (+ a i))))
          (setq i (+ i 2)))
    (if (< i n) (+ sum (ash (mem-ref (+ a i) :u8) 8)) sum)))
(defun %ion-fold (sum)
  (loop (when (< sum #x10000) (return nil))
        (setq sum (+ (logand sum #xFFFF) (ash sum -16))))
  (logand (logxor sum #xFFFF) #xFFFF))

;;; Ethernet + IPv4 around the L4 bytes already at F+34 (L4LEN of them), then out through the
;;; engine.  Core 1 only: it sends on the controller itself.
(defun %ion-ip-header (f dst proto l4len)
  (let ((st (%ion-st)))
    (%ion-copy f (+ st #x28) 6)
    (%ion-copy (+ f 6) (+ st #x08) 6)
    (%ion-put16 (+ f 12) #x0800)
    (setf (mem-ref (+ f 14) :u8) #x45) (setf (mem-ref (+ f 15) :u8) 0)
    (%ion-put16 (+ f 16) (+ 20 l4len))
    (%ion-put16 (+ f 18) 0) (%ion-put16 (+ f 20) #x4000)
    (setf (mem-ref (+ f 22) :u8) 64) (setf (mem-ref (+ f 23) :u8) proto)
    (%ion-put16 (+ f 24) 0)
    (%ion-copy (+ f 26) (+ st #x18) 4)
    (%ion-put32 (+ f 30) dst)
    (%ion-put16 (+ f 24) (%ion-fold (%ion-sum 0 (+ f 14) 20)))))

(defun %ion-l4-checksum (f proto l4len)
  "The TCP/UDP checksum of the segment at F+34, over the pseudo header from F's IP header."
  (%ion-fold (%ion-sum (+ (%ion-sum 0 (+ f 26) 8) (+ proto l4len)) (+ f 34) l4len)))

(defun %ion-transmit (f len)
  (unless (zerop (usbe-nic))
    (usbe-tx-one f len)
    (ion-inc #x3C)))

;;; ---- ARP and ICMP --------------------------------------------------------------------------
(defun %ion-arp (buf len)
  (let ((op (%ion-u16 (+ buf 20))) (st (%ion-st)))
    (ion-inc #x24)
    (when (= (%ion-u32 (+ buf 38)) (ion-our-ip))
      (%ion-copy (+ st #x28) (+ buf 22) 6))              ; the next hop is whoever asks us
    (if (and (= op 1) (= (%ion-u32 (+ buf 38)) (ion-our-ip)))
        (let ((f (ion-frame)))
          (%ion-copy f (+ buf 22) 6)
          (%ion-copy (+ f 6) (+ st #x08) 6)
          (%ion-put16 (+ f 12) #x0806)
          (%ion-put16 (+ f 14) 1) (%ion-put16 (+ f 16) #x0800)
          (setf (mem-ref (+ f 18) :u8) 6) (setf (mem-ref (+ f 19) :u8) 4)
          (%ion-put16 (+ f 20) 2)
          (%ion-copy (+ f 22) (+ st #x08) 6)
          (%ion-copy (+ f 28) (+ st #x18) 4)
          (%ion-copy (+ f 32) (+ buf 22) 6)
          (%ion-copy (+ f 38) (+ buf 28) 4)
          (%ion-transmit f 42)
          1)
        ;; a reply (ARP-RESOLVE waits for it) or anything else: the old interface's
        (progn (when (= op 2) (%ion-copy (+ st #x28) (+ buf 22) 6))
               (%ion-legacy-put buf len)
               1))))

(defun %ion-icmp (buf len ihl)
  "Answer an echo request."
  (let* ((total (%ion-u16 (+ buf 16))) (f (ion-frame)) (n (+ 14 total)))
    (when (and (<= n len) (<= n 1514) (= ihl 20))
      (ion-inc #x28)
      (%ion-copy f buf n)
      (%ion-copy f (+ buf 6) 6)
      (%ion-copy (+ f 6) (+ (%ion-st) #x08) 6)
      (%ion-copy (+ f 26) (+ buf 30) 4)
      (%ion-copy (+ f 30) (+ buf 26) 4)
      (setf (mem-ref (+ f 22) :u8) 64)
      (%ion-put16 (+ f 24) 0)
      (%ion-put16 (+ f 24) (%ion-fold (%ion-sum 0 (+ f 14) 20)))
      (setf (mem-ref (+ f 34) :u8) 0)                     ; echo reply
      (%ion-put16 (+ f 36) 0)
      (%ion-put16 (+ f 36) (%ion-fold (%ion-sum 0 (+ f 34) (- total 20))))
      (%ion-transmit f n))
    1))

;;; ---- the legacy ring: frames for the old interface ------------------------------------------
(defun %ion-legacy-put (buf len)
  (let ((h (ion-get #x80)))
    (if (or (>= (%ion-u32- h (ion-get #x84)) 31) (> len 2040))
        (ion-inc #x38)
        (let ((slot (ion-legacy-slot h)))
          (%ion-copy (+ slot 8) buf len)
          (setf (mem-ref slot :u32) len)
          (memory-barrier)
          (ion-put #x80 (+ h 1))
          (ion-inc #x34)))))

;;; ---- UDP -----------------------------------------------------------------------------------
(defun %ion-udp-find (port)
  (let ((k 0) (r -1))
    (loop (when (or (>= k 8) (>= r 0)) (return r))
          (when (= (mem-ref (ion-udp-port k) :u32) port) (setq r k))
          (setq k (+ k 1)))))

(defun %ion-udp-input (buf len ihl src)
  (let* ((u (+ buf 14 ihl)) (dport (%ion-u16 (+ u 2))) (k (%ion-udp-find dport))
         (n (- (%ion-u16 (+ u 4)) 8)))
    (if (< k 0)
        0
        (let* ((p (ion-udp-port k)) (h (mem-ref (+ p 4) :u32)))
          (ion-inc #x30)
          (if (or (< n 0) (> n 2032) (> (+ 14 ihl 8 n) len)
                  (>= (%ion-u32- h (mem-ref (+ p 8) :u32)) 15))
              (ion-inc #x50)
              (let ((slot (ion-udp-slot k h)))
                (%ion-copy (+ slot 16) (+ u 8) n)
                (setf (mem-ref slot :u32) n)
                (setf (mem-ref (+ slot 4) :u32) src)
                (setf (mem-ref (+ slot 8) :u32) (%ion-u16 u))
                (memory-barrier)
                (setf (mem-ref (+ p 4) :u32) (%ion-u32+ h 1))
                (ion-bump)))
          1))))

;;; ---- TCP, core 1 ---------------------------------------------------------------------------
(defun ion-bump () (ion-put #x10 (+ (ion-get #x10) 1)))
(defun ion-now () (ion-get #x18))

(defun %ion-rx-free (i)
  (- 65536 (%ion-u32- (ion-cget i #x34) (ion-cget i #x38))))
(defun %ion-window (i) (%ion-min (%ion-rx-free i) 65535))

;; data index K (bytes since ISS+1) of the first unacknowledged / next unsent byte
(defun %ion-acked (i) (%ion-u32- (ion-cget i #x14) (%ion-u32+ (ion-cget i #x10) 1)))
(defun %ion-sent (i)
  (let ((k (%ion-u32- (ion-cget i #x18) (%ion-u32+ (ion-cget i #x10) 1))))
    (if (zerop (logand (ion-cget i #x40) 4)) k (%ion-u32- k 1))))       ; less our FIN

(defun %ion-tcp-send (i flags seq k n)
  "One segment of connection I: FLAGS, sequence SEQ, N bytes from its send ring at data index K."
  (let* ((f (ion-frame)) (tcp (+ f 34)) (syn (logbitp 1 flags)) (hl (if syn 24 20))
         (ring (ion-tx-ring i)))
    (%ion-put16 tcp (ion-cget i #x04))
    (%ion-put16 (+ tcp 2) (ion-cget i #x0C))
    (%ion-put32 (+ tcp 4) seq)
    (%ion-put32 (+ tcp 8) (if (logbitp 4 flags) (ion-cget i #x1C) 0))
    (setf (mem-ref (+ tcp 12) :u8) (if syn #x60 #x50))
    (setf (mem-ref (+ tcp 13) :u8) flags)
    (let ((w (%ion-window i))) (%ion-put16 (+ tcp 14) w) (ion-cput i #x4C w))
    (%ion-put32 (+ tcp 16) 0)
    (when syn (%ion-put32 (+ tcp 20) (logior #x02040000 (ion-mss))))
    (let ((j 0))
      (loop (when (>= j n) (return nil))
            (setf (mem-ref (+ tcp hl j) :u8) (mem-ref (+ ring (logand (+ k j) #xFFFF)) :u8))
            (setq j (+ j 1))))
    (%ion-ip-header f (ion-cget i #x08) 6 (+ hl n))
    (%ion-put16 (+ tcp 16) (%ion-l4-checksum f 6 (+ hl n)))
    (%ion-transmit f (+ 34 hl n))))

(defun %ion-ack (i) (ion-cput i #x58 0) (%ion-tcp-send i #x10 (ion-cget i #x18) 0 0))

(defun %ion-rx-store (i h t0 doff n)
  "N payload bytes of the segment at T0 (header DOFF long) into I's receive ring at index H."
  (let ((ring (ion-rx-ring i)) (p (+ t0 doff)) (j 0))
    (loop (when (>= j n) (return nil))
          (setf (mem-ref (+ ring (logand (+ h j) #xFFFF)) :u8) (mem-ref (+ p j) :u8))
          (setq j (+ j 1)))))

(defun %ion-arm-rtx (i)
  (when (zerop (ion-cget i #x28)) (ion-cput i #x28 (+ (ion-now) (ion-cget i #x24)))))

(defun %ion-close-conn (i why)
  "Connection I is over: WHY 2 = reset or failed."
  (ion-cput i #x40 (logior (ion-cget i #x40) (logior 1 why)))
  (ion-cput i 0 10)
  (ion-cput i #x28 0)
  (ion-bump))

(defun %ion-iss () (ion-put #x54 (+ (ion-get #x54) 64019)) (logand (+ (rdtsc) (ion-get #x54)) #xFFFFFFFF))

(defun %ion-find-conn (rip rport lport)
  (let ((i 0) (r -1))
    (loop (when (or (>= i (ion-nconns)) (>= r 0)) (return r))
          (let ((s (ion-cget i 0)))
            (when (and (> s 1) (< s 10) (= (ion-cget i #x04) lport) (= (ion-cget i #x0C) rport)
                       (= (ion-cget i #x08) rip))
              (setq r i)))
          (setq i (+ i 1)))))

(defun %ion-find-listener (port)
  (let ((k 0) (r -1))
    (loop (when (or (>= k 8) (>= r 0)) (return r))
          (when (= (mem-ref (ion-listener k) :u32) port) (setq r k))
          (setq k (+ k 1)))))

(defun %ion-try-lock () (zerop (xchg-mem (+ (ion-base) 8) 1)))
(defun %ion-unlock () (memory-barrier) (xchg-mem (+ (ion-base) 8) 0))

(defun %ion-free-slot ()
  "A free connection slot, claimed (state 1 placeholder) -- under the lock.  -1 for none."
  (let ((i 0) (r -1))
    (loop (when (or (>= i (ion-nconns)) (>= r 0)) (return r))
          (when (zerop (ion-cget i 0)) (setq r i))
          (setq i (+ i 1)))))

(defun %ion-reset-slot (i lport rip rport)
  (ion-cput i #x04 lport) (ion-cput i #x08 rip) (ion-cput i #x0C rport)
  (ion-cput i #x20 65535) (ion-cput i #x24 1000) (ion-cput i #x28 0) (ion-cput i #x2C 0)
  (ion-cput i #x30 0) (ion-cput i #x34 0) (ion-cput i #x38 0) (ion-cput i #x3C 0)
  (ion-cput i #x40 0) (ion-cput i #x48 0) (ion-cput i #x4C 0)
  (ion-cput i #x50 0) (ion-cput i #x54 0) (ion-cput i #x58 0))

(defun %ion-syn (buf t0 src sport dport seq win)
  "A SYN for DPORT: a new connection for its listener, or nothing."
  (let ((l (%ion-find-listener dport)))
    (if (< l 0)
        0
        (if (not (%ion-try-lock))
            (progn (ion-inc #x4C) 1)                       ; the peer sends its SYN again
            (let ((i (%ion-free-slot)))
              (if (< i 0)
                  (progn (%ion-unlock) (ion-inc #x4C) 1)
                  (let ((iss (%ion-iss)))
                    (%ion-reset-slot i dport src sport)
                    (ion-cput i #x10 iss) (ion-cput i #x14 iss) (ion-cput i #x18 (%ion-u32+ iss 1))
                    (ion-cput i #x1C (%ion-u32+ seq 1)) (ion-cput i #x20 win)
                    (ion-cput i #x48 (+ l 1))
                    (ion-cput i 0 3)                        ; SYN-RCVD
                    (%ion-unlock)
                    (%ion-tcp-send i #x12 iss 0 0)
                    (%ion-arm-rtx i)
                    1)))))))

(defun %ion-accept-queue (i)
  "Connection I is established: onto its listener's accept queue (or reset when full)."
  (let ((l (- (ion-cget i #x48) 1)))
    (when (>= l 0)
      (let* ((a (ion-listener l)) (h (mem-ref (+ a 4) :u32)))
        (if (>= (%ion-u32- h (mem-ref (+ a 8) :u32)) 4)
            (progn (%ion-tcp-send i #x14 (ion-cget i #x18) 0 0) (%ion-close-conn i 2))
            (progn (setf (mem-ref (+ a #x10 (* 4 (logand h 3))) :u32) i)
                   (memory-barrier)
                   (setf (mem-ref (+ a 4) :u32) (%ion-u32+ h 1))
                   (ion-bump)))))))

(defun %ion-tcp-input (buf len ihl src)
  "One TCP segment for us.  1 when it was ours, 0 for the old interface."
  (let* ((t0 (+ buf 14 ihl))
         (total (%ion-u16 (+ buf 16)))
         (sport (%ion-u16 t0)) (dport (%ion-u16 (+ t0 2)))
         (seq (%ion-u32 (+ t0 4))) (ack (%ion-u32 (+ t0 8)))
         (doff (* 4 (ash (mem-ref (+ t0 12) :u8) -4)))
         (flags (mem-ref (+ t0 13) :u8))
         (win (%ion-u16 (+ t0 14)))
         (dlen (- total (+ ihl doff)))
         (i (%ion-find-conn src sport dport)))
    (cond
      ((< i 0)
       (if (and (logbitp 1 flags) (not (logbitp 4 flags)))
           (%ion-syn buf t0 src sport dport seq win)
           0))
      ((logbitp 2 flags) (%ion-close-conn i 2) 1)               ; RST
      (t
       (ion-inc #x2C)
       (ion-cput i #x20 win)
       (let ((st (ion-cget i 0)) (iss (ion-cget i #x10)))
         ;; the handshake
         (cond ((= st 2)
                (when (and (logbitp 1 flags) (logbitp 4 flags) (= ack (%ion-u32+ iss 1)))
                  (ion-cput i #x1C (%ion-u32+ seq 1))
                  (ion-cput i #x14 ack)
                  (ion-cput i #x28 0) (ion-cput i #x2C 0)
                  (ion-cput i 0 4)
                  (%ion-ack i)
                  (ion-bump))
                (setq dlen 0))
               ((and (= st 3) (logbitp 4 flags) (= ack (%ion-u32+ iss 1)))
                (ion-cput i #x14 ack)
                (ion-cput i #x28 0) (ion-cput i #x2C 0)
                (ion-cput i 0 4)
                (%ion-accept-queue i))
               ((and (logbitp 1 flags) (>= st 4))                ; their SYN-ACK again
                (%ion-ack i)
                (setq dlen 0)))
         (setq st (ion-cget i 0))
         ;; what the ACK covers
         (when (and (logbitp 4 flags) (>= st 4) (< st 10))
           (when (and (%ion-seq> ack (ion-cget i #x14)) (%ion-seq>= (ion-cget i #x18) ack))
             (ion-cput i #x14 ack)
             (ion-cput i #x2C 0) (ion-cput i #x24 1000)
             (ion-cput i #x28 (if (= ack (ion-cget i #x18)) 0 (+ (ion-now) 1000)))
             (ion-bump))
           (when (and (logbitp 2 (ion-cget i #x40)) (= (ion-cget i #x14) (ion-cget i #x18)))
             (cond ((= st 5) (ion-cput i 0 6) (ion-bump))      ; FIN-WAIT-2
                   ((or (= st 8) (= st 9)) (%ion-close-conn i 0)))))
         (setq st (ion-cget i 0))
         ;; data: in order, or past one hole
         (when (and (> dlen 0) (or (= st 4) (= st 5) (= st 6)))
           (let* ((rn (ion-cget i #x1C)) (off (%ion-u32- seq rn)) (free (%ion-rx-free i)))
             (cond
               ((zerop off)
                (let* ((n (%ion-min dlen free)) (h (ion-cget i #x34)))
                  (%ion-rx-store i h t0 doff n)
                  (setq rn (%ion-u32+ rn n) h (%ion-u32+ h n))
                  ;; the hole is filled: everything held past it is in order now
                  (let ((os (ion-cget i #x50)) (oe (ion-cget i #x54)))
                    (when (and (/= os oe) (%ion-seq>= rn os))
                      (when (%ion-seq> oe rn)
                        (setq h (%ion-u32+ h (%ion-u32- oe rn)) rn oe))
                      (ion-cput i #x50 0) (ion-cput i #x54 0)))
                  (memory-barrier)
                  (ion-cput i #x34 h)
                  (ion-cput i #x1C rn)
                  (setq dlen n)
                  (ion-cput i #x58 1)                    ; ACK at the end of the pass
                  (ion-bump)))
               ((< off free)                             ; past a hole, inside the window
                (let* ((n (%ion-min dlen (- free off))) (e (%ion-u32+ seq n))
                       (os (ion-cget i #x50)) (oe (ion-cget i #x54)))
                  (cond ((= os oe)
                         (%ion-rx-store i (%ion-u32+ (ion-cget i #x34) off) t0 doff n)
                         (ion-cput i #x50 seq) (ion-cput i #x54 e))
                        ((and (%ion-seq>= oe seq) (%ion-seq>= e os))   ; touches the range
                         (%ion-rx-store i (%ion-u32+ (ion-cget i #x34) off) t0 doff n)
                         (when (%ion-seq> os seq) (ion-cput i #x50 seq))
                         (when (%ion-seq> e oe) (ion-cput i #x54 e))))
                  (setq dlen 0)
                  (%ion-ack i)))                          ; a duplicate ACK, at once
               (t (setq dlen 0) (%ion-ack i)))))          ; old data again
         ;; their FIN, in order
         (when (and (logbitp 0 flags) (>= st 4) (< st 10)
                    (= (%ion-u32+ seq (if (> dlen 0) dlen 0)) (ion-cget i #x1C)))
           (ion-cput i #x1C (%ion-u32+ (ion-cget i #x1C) 1))
           (ion-cput i #x40 (logior (ion-cget i #x40) 1))
           (%ion-ack i)
           (cond ((= st 4) (ion-cput i 0 7))                      ; CLOSE-WAIT
                 ((= st 5) (ion-cput i 0 9))                      ; CLOSING
                 ((= st 6) (%ion-close-conn i 0)))
           (ion-bump))
         1)))))

;;; A connection's own work each pass: open, send, close, retransmit, window, free.
(defun %ion-conn-service (i now)
  (let ((st (ion-cget i 0)) (cf (ion-cget i #x3C)) (work 0))
    ;; the ACK the frames of this pass earned, one for all of them
    (when (and (= (ion-cget i #x58) 1) (>= st 4) (< st 10)) (%ion-ack i) (setq work 1))
    (cond
      ((= st 0) nil)
      ((= st 1)                                           ; a client asked to open
       (let ((iss (%ion-iss)))
         (ion-cput i #x10 iss) (ion-cput i #x14 iss) (ion-cput i #x18 (%ion-u32+ iss 1))
         (ion-cput i 0 2)
         (%ion-tcp-send i #x02 iss 0 0)
         (%ion-arm-rtx i)
         (setq work 1)))
      ((and (= st 6) (logbitp 2 cf))                      ; released in FIN-WAIT-2: their FIN
       (%ion-close-conn i 0)                              ; is not worth waiting for
       (setq work 1))
      ((= st 10)
       (when (logbitp 2 cf)                               ; released: free the slot
         (ion-cput i #x44 (+ (ion-cget i #x44) 1))
         (ion-cput i 0 0)
         (setq work 1)))
      ((logbitp 1 cf)                                     ; abort
       (%ion-tcp-send i #x14 (ion-cget i #x18) 0 0)
       (%ion-close-conn i 2)
       (setq work 1))
      (t
       ;; retransmission: go back to the first unacknowledged byte
       (let ((dl (ion-cget i #x28)))
         (when (and (/= dl 0) (%ion-seq>= now dl))
           (ion-inc #x40)
           (ion-cput i #x2C (+ (ion-cget i #x2C) 1))
           (if (> (ion-cget i #x2C) 8)
               (%ion-close-conn i 2)
               (progn
                 (ion-cput i #x24 (%ion-min 8000 (* 2 (ion-cget i #x24))))
                 (ion-cput i #x28 (+ now (ion-cget i #x24)))
                 (cond ((= st 2) (%ion-tcp-send i #x02 (ion-cget i #x10) 0 0))
                       ((= st 3) (%ion-tcp-send i #x12 (ion-cget i #x10) 0 0))
                       (t ;; data and our FIN again from SND.UNA
                        (when (logbitp 2 (ion-cget i #x40))
                          (ion-cput i #x40 (logand (ion-cget i #x40) (lognot 4)))
                          (ion-cput i 0 (cond ((= st 5) 4) ((= st 9) 7) ((= st 8) 7) (t st))))
                        (ion-cput i #x18 (ion-cget i #x14))))))
           (setq work 1)))
       (setq st (ion-cget i 0))
       ;; new data, as far as the peer's window and 32 KB in flight allow
       (when (or (= st 4) (= st 7))
         (let ((segs 0))
           (loop
             (let* ((k (%ion-sent i)) (avail (%ion-u32- (ion-cget i #x30) k))
                    (flight (%ion-u32- (ion-cget i #x18) (ion-cget i #x14)))
                    (room (- (%ion-min (ion-cget i #x20) 32768) flight)))
               (when (or (zerop avail) (<= room 0) (>= segs 4)) (return nil))
               (let ((n (%ion-min (%ion-min avail room) (ion-mss))))
                 (%ion-tcp-send i #x18 (ion-cget i #x18) k n)
                 (ion-cput i #x18 (%ion-u32+ (ion-cget i #x18) n))
                 (%ion-arm-rtx i)
                 (setq segs (+ segs 1) work 1)))))
         ;; and our FIN once everything written is out
         (when (and (logbitp 0 cf) (zerop (logand (ion-cget i #x40) 4))
                    (= (%ion-sent i) (ion-cget i #x30)))
           (%ion-tcp-send i #x11 (ion-cget i #x18) 0 0)
           (ion-cput i #x18 (%ion-u32+ (ion-cget i #x18) 1))
           (ion-cput i #x40 (logior (ion-cget i #x40) 4))
           (ion-cput i 0 (if (= st 4) 5 8))
           (%ion-arm-rtx i)
           (setq work 1)))
       ;; a read reopened the window: tell the peer (it may be waiting on a closed one)
       (when (and (>= st 4) (<= st 7) (>= (- (%ion-window i) (ion-cget i #x4C)) 16384))
         (%ion-ack i)
         (setq work 1))))
    work))

;;; The next hop's MAC, when nothing has told us yet: ask for the gateway's (state +0x1C, network
;;; order), once a second.  The reply is learnt by %ION-ARP.
(defun %ion-next-hop-known-p ()
  (let ((a (+ (%ion-st) #x28)))
    (/= 0 (logior (mem-ref a :u32) (mem-ref (+ a 4) :u8)))))

(defun %ion-arp-request (now)
  (when (%ion-seq>= now (ion-get #x5C))
    (ion-put #x5C (+ now 1000))
    (let ((f (ion-frame)) (st (%ion-st)) (j 0))
      (loop (when (>= j 6) (return nil)) (setf (mem-ref (+ f j) :u8) #xFF) (setq j (+ j 1)))
      (%ion-copy (+ f 6) (+ st #x08) 6)
      (%ion-put16 (+ f 12) #x0806)
      (%ion-put16 (+ f 14) 1) (%ion-put16 (+ f 16) #x0800)
      (setf (mem-ref (+ f 18) :u8) 6) (setf (mem-ref (+ f 19) :u8) 4)
      (%ion-put16 (+ f 20) 1)
      (%ion-copy (+ f 22) (+ st #x08) 6)
      (%ion-copy (+ f 28) (+ st #x18) 4)
      (%ion-put32 (+ f 32) 0) (%ion-put16 (+ f 36) 0)
      (%ion-copy (+ f 38) (+ st #x1C) 4)
      (%ion-transmit f 42)
      1)))

;;; ---- the step: what the engine runs each pass -----------------------------------------------
(defun %ion-input (buf len)
  (ion-inc #x20)
  (if (< len 14)
      0
      (let ((et (%ion-u16 (+ buf 12))))
        (cond
          ((= et #x0806) (%ion-arp buf len))
          ((/= et #x0800) (%ion-legacy-put buf len))
          (t
           (let* ((ihl (* 4 (logand (mem-ref (+ buf 14) :u8) 15)))
                  (proto (mem-ref (+ buf 23) :u8))
                  (src (%ion-u32 (+ buf 26)))
                  (dst (%ion-u32 (+ buf 30))))
             (cond
               ((/= dst (ion-our-ip)) (%ion-legacy-put buf len))          ; broadcast, DHCP
               ((and (= proto 1) (= (mem-ref (+ buf 14 ihl) :u8) 8)) (%ion-icmp buf len ihl))
               ((= proto 6)
                (when (zerop (%ion-tcp-input buf len ihl src)) (%ion-legacy-put buf len)))
               ((= proto 17)
                (when (zerop (%ion-udp-input buf len ihl src)) (%ion-legacy-put buf len)))
               (t (%ion-legacy-put buf len))))))
        1)))

(defun ion-step ()
  "Core 1, once a pass of the engine: the frames received, then each connection's work.  How
   much was done (0: the engine may sleep)."
  (if (or (not (ion-on-p))
          ;; a keyboard/mouse split transaction is in flight: its complete-split has a window
          ;; of a few microframes, and a network pass (up to 1.5 ms measured) made it miss it --
          ;; 99 failures over a 5 MB transfer.  The receive is paused for the round anyway.
          (/= (usbe-get #x100) 0) (= (usbe-get #x12C) 1))
      0
      (let ((work 0) (now (truncate (rdtsc) (ion-get #x14))))
        (ion-put #x18 now)
        ;; the engine's receive ring: core 1 is its consumer now
        (let ((n 0))
          (loop
            (let ((tl (usbe-get #x1004)))
              (when (or (= (usbe-get #x1000) tl) (>= n 12)) (return nil))
              (memory-barrier)
              (let ((slot (usbe-rx-slot tl)))
                (%ion-input (+ slot 8) (mem-ref slot :u32)))
              (usbe-put #x1004 (+ tl 1))
              (setq n (+ n 1))))
          (setq work n))
        (if (not (%ion-next-hop-known-p))
            (%ion-arp-request now)                      ; nothing can be sent yet
            (let ((i 0))
              (loop (when (>= i (ion-nconns)) (return nil))
                    (setq work (+ work (%ion-conn-service i now)))
                    (setq i (+ i 1)))))
        (when (> work 0) (ion-inc #x58) (ion-bump))
        work)))

;;; ===================================================================================
;;; The other side: clients, on any core but the engine's.  These may allocate.
;;; ===================================================================================

(defun %ion-lock ()
  (loop (when (%ion-try-lock) (return t))))

(defun %ion-wait (old ms)
  "Until the generation word is not OLD or MS pass, letting this core's other threads run."
  (let ((dl (+ (%bt-ticks) (%bt-ms-ticks ms))) (a (+ (ion-base) #x10)))
    (if (%bt-slot-p) (%bt-block a old dl) (%bt-main-wait a old dl))))

(defun %ion-until (pred timeout-ms)
  "PRED's value once true, or NIL after TIMEOUT-MS."
  (let ((end (+ (%bt-ticks) (%bt-ms-ticks timeout-ms))) (v nil))
    (loop
      (let ((g (ion-get #x10)))
        (setq v (funcall pred))
        (when v (return v))
        (let ((left (- end (%bt-ticks))))
          (when (<= left 0) (return nil))
          (%ion-wait g (max 1 (min 50 (floor left (%bt-ms-ticks 1))))))))))

(defun %ion-h (i) (+ (* (ion-cget i #x44) 64) i))
(defun %ion-i (h)
  "The slot of handle H, or NIL when it is stale (the slot was freed and reused)."
  (let ((i (logand h 63)))
    (and (< i (ion-nconns)) (= (ion-cget i #x44) (ash h -6)) (/= (ion-cget i 0) 0) i)))

(defun ion-start ()
  "From the MAIN thread, the engine running: core 1 takes the network.  :STARTED or why not."
  (cond ((ion-on-p) :already)
        ((not (usbe-running-p)) :no-engine)
        (t
         (let ((b (ion-base)))
           (dotimes (k (/ #x3000 8)) (setf (mem-ref (+ b (* 8 k)) :u64) 0))
           (dotimes (i (ion-nconns)) (ion-cput i #x44 (random 1000)))
           (ion-put #x14 (%bt-ms-ticks 1))
           (ion-put #x1C (random 16384))
           (ion-put #x80 0) (ion-put #x84 0) (ion-put #x88 0)
           (ion-put #x60 (%usbe-boot))
           (ion-put 0 #x544E4F49)
           ;; the old interface reads what core 1 leaves it
           (setf (fdefinition 'modus.net:nic-receive) #'ion-legacy-receive
                 (fdefinition 'modus.net:nic-rx-buf) #'ion-legacy-rx-buf)
           (memory-barrier)
           (ion-put 4 1)
           (usbe-poke)
           (kmsg-event 1 "IONS" (ion-our-ip) 0 0)
           :started))))

;;; the old interface: one reader at a time, the frame stays put until the next call
(defun ion-legacy-receive ()
  (let ((tl (ion-get #x84)))
    (when (= (ion-get #x88) 1)
      (setq tl (logand (+ tl 1) #xFFFFFFFF))
      (ion-put #x84 tl)
      (ion-put #x88 0))
    (if (= (ion-get #x80) tl)
        (progn (if (%bt-slot-p) (bt-yield) (%bt-main-idle)) 0)
        (progn (memory-barrier)
               (ion-put #x88 1)
               (let ((n (mem-ref (ion-legacy-slot tl) :u32)))
                 (setf (mem-ref (+ (e1000-state-base) #x44) :u32) n)
                 n)))))
(defun ion-legacy-rx-buf () (+ (ion-legacy-slot (ion-get #x84)) 8))
(defun ion-legacy-wait ()
  "The old interface's idle poll: T once a frame waits for it, NIL after a short while; this
   core's other threads run meanwhile."
  (let ((i 0))
    (loop
      (when (/= (ion-get #x80) (%ion-u32+ (ion-get #x84) (ion-get #x88))) (return t))
      (when (>= i 5000) (return nil))
      (when (zerop (logand i 255)) (if (%bt-slot-p) (bt-yield) (%bt-main-idle)))
      (setq i (+ i 1)))))

;;; ---- TCP -----------------------------------------------------------------------------------
(defun ion-listen (port)
  "Accept connections to PORT: a listener index."
  (%ion-lock)
  (unwind-protect
       (or (let ((k (%ion-find-listener port))) (and (>= k 0) k))
           (let ((k (%ion-find-listener 0)))
             (when (< k 0) (error "io-net: no free listener"))
             (let ((a (ion-listener k)))
               (setf (mem-ref (+ a 4) :u32) 0 (mem-ref (+ a 8) :u32) 0)
               (memory-barrier)
               (setf (mem-ref a :u32) port))
             k))
    (%ion-unlock)))

(defun ion-unlisten (k) (setf (mem-ref (ion-listener k) :u32) 0))

(defun ion-acceptable-p (k)
  (let ((a (ion-listener k))) (/= (mem-ref (+ a 4) :u32) (mem-ref (+ a 8) :u32))))

(defun ion-accept (k &key (timeout-ms 5000))
  "The next connection to listener K, as a handle, or NIL."
  (when (%ion-until (lambda () (ion-acceptable-p k)) timeout-ms)
    (let* ((a (ion-listener k)) (tl (mem-ref (+ a 8) :u32))
           (i (mem-ref (+ a #x10 (* 4 (logand tl 3))) :u32)))
      (setf (mem-ref (+ a 8) :u32) (logand (+ tl 1) #xFFFFFFFF))
      (%ion-h i))))

(defun ion-open (ip port &key (timeout-ms 3000))
  "A connection to IP:PORT (IP a big-endian integer), established, as a handle; or NIL."
  (let ((i (progn (%ion-lock)
                  (unwind-protect
                       (let ((i (%ion-free-slot)))
                         (when (>= i 0)
                           (ion-put #x1C (+ (ion-get #x1C) 1))
                           (%ion-reset-slot i (+ 49152 (logand (ion-get #x1C) #x3FFF)) ip port)
                           (memory-barrier)
                           (ion-cput i 0 1))
                         i)
                    (%ion-unlock)))))
    (when (< i 0) (error "io-net: no free connection"))
    (let ((h (%ion-h i)))
      (usbe-poke)
      (if (%ion-until (lambda () (> (ion-cget i 0) 2)) timeout-ms)
          (if (member (ion-cget i 0) '(4 7)) h (progn (ion-release h) nil))
          (progn (ion-abort h) nil)))))

(defun ion-state (h)
  (let ((i (%ion-i h)))
    (if i
        (case (ion-cget i 0)
          (1 :opening) (2 :syn-sent) (3 :syn-rcvd) (4 :established) (5 :fin-wait-1)
          (6 :fin-wait-2) (7 :close-wait) (8 :last-ack) (9 :closing) (t :closed))
        :closed)))

(defun ion-peer (h)
  "(values remote-ip remote-port local-port) of connection H."
  (let ((i (%ion-i h)))
    (if i (values (ion-cget i #x08) (ion-cget i #x0C) (ion-cget i #x04)) (values 0 0 0))))

(defun ion-readable-p (h)
  "Data to read, or the peer has finished: a read will not wait."
  (let ((i (%ion-i h)))
    (or (null i) (/= (ion-cget i #x34) (ion-cget i #x38)) (logtest (ion-cget i #x40) 3))))

(defun ion-read (h &key (max 65536) (timeout-ms 5000) into)
  "Up to MAX bytes from connection H, waiting up to TIMEOUT-MS for any: a fresh octet vector,
   or with INTO (octets, at least MAX) the count; :EOF once the peer has finished and
   everything is read; an empty vector (or 0) on timeout."
  (%ion-until (lambda () (ion-readable-p h)) timeout-ms)
  (let ((i (%ion-i h)))
    (if (null i)
        :eof
        ;; THE FLAG BEFORE THE COUNT.  Core 1 stores a segment's bytes, then the count (+0x34),
        ;; then, for a FIN, the flag (+0x40).  Read the other way round, a peer's last segment
        ;; -- data and FIN together, which is how GitHub ends a response -- could land between
        ;; the two reads: an old count (nothing new) and a new flag (finished), so :EOF with the
        ;; last bytes still in the ring.  A chunked body lost its terminating chunk that way
        ;; ("chunked body cut off" on a Pi Zero, every byte acknowledged on the wire).  A flag
        ;; read first covers every byte counted before it, so a zero count after it is the end.
        (let* ((done (logtest (progn (memory-barrier) (ion-cget i #x40)) 3))
               (tl (progn (memory-barrier) (ion-cget i #x38)))
               (have (%ion-u32- (ion-cget i #x34) tl)))
          (if (zerop have)
              (if done :eof (if into 0 (make-array 0 :element-type '(unsigned-byte 8))))
              (let* ((n (min max have)) (out (or into (make-array n :element-type '(unsigned-byte 8))))
                     (ring (ion-rx-ring i)))
                (memory-barrier)
                (dotimes (j n) (setf (aref out j) (mem-ref (+ ring (logand (+ tl j) #xFFFF)) :u8)))
                (ion-cput i #x38 (%ion-u32+ tl n))
                (when (< (ion-cget i #x4C) 16384) (usbe-poke))   ; the window reopened
                (if into n out)))))))

(defun ion-write (h data &key (start 0) end (timeout-ms 10000))
  "Queue DATA[START, END) on connection H, waiting for room as long as TIMEOUT-MS.  T, or NIL
   when the connection is gone or the time ran out."
  (let ((end (or end (length data))) (pos start))
    (loop
      (when (>= pos end) (return t))
      (let ((i (%ion-i h)))
        (unless (and i (member (ion-cget i 0) '(4 7))) (return nil))
        (let ((room (- 65536 (%ion-u32- (ion-cget i #x30) (%ion-acked i)))))
          (if (<= room 0)
              (progn (usbe-poke)
                     (unless (%ion-until (lambda ()
                                           (let ((i (%ion-i h)))
                                             (or (null i) (not (member (ion-cget i 0) '(4 7)))
                                                 (< (%ion-u32- (ion-cget i #x30) (%ion-acked i)) 65536))))
                                         timeout-ms)
                       (return nil)))
              (let* ((n (min room (- end pos))) (k (ion-cget i #x30)) (ring (ion-tx-ring i)))
                (dotimes (j n)
                  (setf (mem-ref (+ ring (logand (+ k j) #xFFFF)) :u8) (logand (aref data (+ pos j)) 255)))
                (memory-barrier)
                (ion-cput i #x30 (%ion-u32+ k n))
                (usbe-poke)
                (setq pos (+ pos n)))))))))

(defun ion-flushed-p (h)
  "Everything written has been acknowledged."
  (let ((i (%ion-i h)))
    (or (null i) (not (member (ion-cget i 0) '(4 7))) (= (%ion-acked i) (ion-cget i #x30)))))

(defun ion-close (h &key (timeout-ms 2000))
  "Send what is queued, then FIN; wait (briefly) for the FIN to be acknowledged; release H."
  (let ((i (%ion-i h)))
    (when i
      (ion-cput i #x3C (logior (ion-cget i #x3C) 1))
      (usbe-poke)
      (%ion-until (lambda () (let ((i (%ion-i h)))
                               (or (null i) (member (ion-cget i 0) '(6 10))
                                   (and (logbitp 2 (ion-cget i #x40))
                                        (= (ion-cget i #x14) (ion-cget i #x18))))))
                  timeout-ms)
      (ion-release h))
    t))

(defun ion-abort (h)
  (let ((i (%ion-i h)))
    (when i (ion-cput i #x3C (logior (ion-cget i #x3C) 6)) (usbe-poke))))

(defun ion-release (h)
  "H's slot may be reused once the connection is over (core 1 frees it, closing it if need be)."
  (let ((i (%ion-i h)))
    (when i
      (ion-cput i #x3C (logior (ion-cget i #x3C) (if (member (ion-cget i 0) '(6 10)) 4 5)))
      (usbe-poke))))

;;; ---- UDP -----------------------------------------------------------------------------------
(defun ion-udp-bind (port)
  "Datagrams to PORT queue for UDP-RECV.  The port's index."
  (%ion-lock)
  (unwind-protect
       (or (let ((k (%ion-udp-find port))) (and (>= k 0) k))
           (let ((k (%ion-udp-find 0)))
             (when (< k 0) (error "io-net: no free UDP port"))
             (let ((p (ion-udp-port k)))
               (setf (mem-ref (+ p 4) :u32) 0 (mem-ref (+ p 8) :u32) 0)
               (memory-barrier)
               (setf (mem-ref p :u32) port))
             k))
    (%ion-unlock)))

(defun ion-udp-unbind (port)
  (let ((k (%ion-udp-find port))) (when (>= k 0) (setf (mem-ref (ion-udp-port k) :u32) 0))))

(defun ion-udp-readable-p (port)
  (let ((k (%ion-udp-find port)))
    (and (>= k 0) (let ((p (ion-udp-port k))) (/= (mem-ref (+ p 4) :u32) (mem-ref (+ p 8) :u32))))))

(defun ion-udp-recv (port &key (timeout-ms 1000))
  "(ip port bytes) for the oldest datagram to PORT, or NIL."
  (when (%ion-until (lambda () (ion-udp-readable-p port)) timeout-ms)
    (let* ((k (%ion-udp-find port)) (p (ion-udp-port k)) (tl (mem-ref (+ p 8) :u32))
           (slot (ion-udp-slot k tl)) (n (mem-ref slot :u32))
           (v (make-array n :element-type '(unsigned-byte 8))))
      (memory-barrier)
      (dotimes (j n) (setf (aref v j) (mem-ref (+ slot 16 j) :u8)))
      (let ((r (list (mem-ref (+ slot 4) :u32) (mem-ref (+ slot 8) :u32) v)))
        (setf (mem-ref (+ p 8) :u32) (logand (+ tl 1) #xFFFFFFFF))
        r))))

(defun ion-udp-send (ip port lport data &optional (n (length data)))
  "One datagram, built in this core's own frame and queued on its engine TX ring."
  (let* ((f (+ (ion-base) #x3800 (* (logand (%kmsg-core) 3) #x800))) (u (+ f 34)) (len (+ 8 n)))
    (when (> n 1472) (error "io-net: a datagram of ~D bytes" n))
    (%ion-put16 u lport) (%ion-put16 (+ u 2) port) (%ion-put16 (+ u 4) len) (%ion-put16 (+ u 6) 0)
    (dotimes (j n) (setf (mem-ref (+ u 8 j) :u8) (logand (aref data j) 255)))
    (%ion-ip-header f ip 17 len)
    (%ion-put16 (+ u 6) (let ((c (%ion-l4-checksum f 17 len))) (if (zerop c) #xFFFF c)))
    (let ((v (make-array (+ 34 len) :element-type '(unsigned-byte 8))))
      (dotimes (j (+ 34 len)) (setf (aref v j) (mem-ref (+ f j) :u8)))
      (usbe-send v (+ 34 len)))))

;;; ---- what it is doing -------------------------------------------------------------------------
(defun ion-status ()
  (if (not (= (ion-get 0) #x544E4F49))
      (list :io-net :never-started)
      (list :on (ion-on-p) :frames (ion-get #x20) :arp (ion-get #x24) :icmp (ion-get #x28)
            :tcp (ion-get #x2C) :udp (ion-get #x30) :legacy (ion-get #x34)
            :legacy-dropped (ion-get #x38) :tx (ion-get #x3C) :retransmits (ion-get #x40)
            :syn-dropped (ion-get #x4C) :udp-dropped (ion-get #x50) :busy-passes (ion-get #x58)
            :connections (loop for i below (ion-nconns) unless (zerop (ion-cget i 0))
                               collect (list i (ion-cget i 0) (ion-cget i #x04) (ion-cget i #x0C)))
            :listening (loop for k below 8 unless (zerop (mem-ref (ion-listener k) :u32))
                             collect (mem-ref (ion-listener k) :u32)))))
