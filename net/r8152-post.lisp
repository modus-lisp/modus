;;;; r8152-post.lisp - the Pi's TCP fast paths over the RTL8153, spliced AFTER
;;;; ip.lisp so these overrides of its seams win (r8152.lisp itself is spliced
;;;; before ip.lisp, with the other NIC drivers).

;; After an empty receive poll, spin on the bulk-IN channel's HCINT (a read;
;; consumes nothing) for at most IO-DELAY's 5000 reads, leaving as soon as the
;; transfer halts or completes.  R8152-RECEIVE re-arms that channel only once
;; a transfer's frames are walked, so the poll after a batch is empty by
;; construction and used to sit out a whole IO-DELAY (1.5 ms per segment).
;; True when a transfer is ready, NIL on timeout.
(defun net-rx-idle ()
  (let ((i 0))
    (loop
      (when (>= i 5000) (return nil))
      (when (not (zerop (logand (dwc2-read (dwc2-hcint 1)) 3))) (return t))
      (setq i (+ i 1)))))

;; The advertised receive window.  r8152-meta+8 overrides it live (0 = the
;; default); it lives in RAM the driver zeroes at bring-up.  16 KB: 8/16/32 KB
;; measured within 2% of each other on a 4 MB fetch, and 64 KB collapsed to
;; ~1 MB/s (the RTL8153's RX FIFO overflows and segments are lost).
(defun tcp-rx-window ()
  (let ((w (mem-ref (+ (r8152-meta) 8) :u32)))
    (if (and (> w 0) (< w 65536)) w 16384)))

(defun %ack-be16 (b i v)
  (setf (aref b i) (logand (ash v -8) 255))
  (setf (aref b (+ i 1)) (logand v 255)))

(defun %ack-sum16 (b start n sum)
  (let ((i 0) (s sum))
    (loop
      (when (>= i n) (return s))
      (setq s (+ s (logior (ash (aref b (+ start i)) 8) (aref b (+ start i 1)))))
      (setq i (+ i 2)))))

(defun %ack-fold (s)
  (let ((f (+ (logand s #xFFFF) (ash s -16))))
    (logand (logxor (+ (logand f #xFFFF) (ash f -16)) #xFFFF) #xFFFF)))

;; A bare ACK, built in a 56-byte byte vector and written straight into the TX
;; staging buffer as aligned words (it is Device memory).  The general path
;; built it in two generic arrays (TCP-SEND-SEGMENT's, then IP-SEND's), copied
;; it byte by byte into the TX buffer and checksummed with generic AREF: ~62 us
;; an ACK, the largest per-segment cost after the RX copy.  Same bytes on the
;; wire.  NIL (do it the general way) unless the RTL8153 is the bound NIC.
(defun tcp-send-ack-fast ()
  (if (not (eq (usb-netdev-get) 2))
      nil
      (let ((st (e1000-state-base))
            (b (make-array 56 :element-type (quote (unsigned-byte 8)) :initial-element 0))
            (tx (r8152-ack-buf)))
        (dotimes (i 6)
          (setf (aref b i) (mem-ref (+ st #x28 i) :u8))
          (setf (aref b (+ 6 i)) (mem-ref (+ st #x08 i) :u8)))
        (setf (aref b 12) 8)
        (setf (aref b 14) #x45) (setf (aref b 17) 40)
        (setf (aref b 20) #x40) (setf (aref b 22) 64) (setf (aref b 23) 6)
        (dotimes (i 4) (setf (aref b (+ 26 i)) (mem-ref (+ st #x18 i) :u8)))
        (let ((nip (htonl (mem-ref (+ st #x38) :u32))))
          (dotimes (i 4) (setf (aref b (+ 30 i)) (logand (ash nip (* -8 i)) 255))))
        (%ack-be16 b 24 (%ack-fold (%ack-sum16 b 14 20 0)))
        (%ack-be16 b 34 (mem-ref (+ st #x34) :u16))
        (%ack-be16 b 36 (mem-ref (+ st #x36) :u16))
        (let ((seq (mem-ref (+ st #x3C) :u32)) (ack (mem-ref (+ st #x40) :u32)))
          (%ack-be16 b 38 (ash seq -16)) (%ack-be16 b 40 seq)
          (%ack-be16 b 42 (ash ack -16)) (%ack-be16 b 44 ack))
        (setf (aref b 46) #x50) (setf (aref b 47) #x10)
        (%ack-be16 b 48 (tcp-rx-window))
        ;; pseudo-header: src+dst IP, protocol 6, TCP length 20
        (%ack-be16 b 50 (%ack-fold (%ack-sum16 b 34 20 (+ (%ack-sum16 b 26 8 0) 6 20))))
        (setf (mem-ref tx :u32) (logior 54 #xC0000000))
        (setf (mem-ref (+ tx 4) :u32) 0)
        (r8152-tx-async-wait)                 ; the ACK buffer may still be in flight
        (let ((src (+ (%val->word b) 7)) (i 0))
          (loop
            (when (>= i 56) (return nil))
            (setf (mem-ref (+ tx 8 i) :u32) (mem-ref (+ src i) :u32))
            (setq i (+ i 4))))
        (r8152-tx-async-start tx 62)
        t)))
