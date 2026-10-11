;;;; isolated-net.lisp - Qubes-like actor isolation for networking
;;;;
;;;; Loaded LAST (after actors-net-overrides.lisp). Establishes isolation
;;;; boundary at the TCP byte stream level:
;;;;
;;;;   net-domain (actor 2): sole hardware accessor, owns E1000/TCP/IP
;;;;   handler actors (3+):  SSH protocol only, communicate via mailbox
;;;;
;;;; Inbound:  E1000 → net-domain → send(handler, (cons conn data-array))
;;;; Outbound: handler → send(net-domain, (cons conn data-array))
;;;;
;;;; Overrides:
;;;;   receive         - block on mailbox, copy data to SSH recv buf
;;;;   tcp-send-conn   - send outbound data as message to net-domain
;;;;   net-actor-main  - interleave E1000 poll + outbound message dispatch
;;;;   net-deliver-data - send inbound TCP data as message to handler

;;; ============================================================
;;; conn-index-from-base: recover connection index from conn-base address
;;; ============================================================
;;;
;;; conn-base(i) = ssh-conn-base + (i << 14)
;;; So i = (cb - ssh-conn-base) >> 14

(defun conn-index-from-base (cb)
  (ash (- cb (ssh-conn-base)) -14))

;;; ============================================================
;;; receive: handler actors block on mailbox for inbound data
;;; ============================================================
;;;
;;; Poll mailbox via try-receive + yield (can't use actors.lisp receive
;;; because MVM resolves ALL calls to the last defun, which is THIS one).
;;;
;;; Message protocol from net-domain:
;;;   (cons conn-id byte-array) - TCP payload data
;;;   (cons conn-id 0)          - connection close
;;;
;;; Returns: 1 to continue ssh-receive-packet retry loop, 0 to close.

(defun ssh-net-poll () (receive))   ; net/ssh.lisp's SSH-WAIT-DATA waits here
(defun receive ()
  ;; Poll mailbox via try-receive + yield
  (let ((msg 0))
    (loop
      (setq msg (try-receive))
      (when (not (zerop msg)) (return 0))
      (yield))
    (let ((conn (car msg))
          (data (cdr msg)))
      (if (zerop data)
          0
          (let ((ssh (conn-ssh conn)))
            (let ((buf-len (mem-ref (+ ssh #x6D4) :u32))
                  (arr-len (array-length data)))
              (let ((i 0))
                (loop
                  (when (>= i arr-len) (return 0))
                  (setf (mem-ref (+ (+ ssh #x6D8) (+ buf-len i)) :u8)
                        (aref data i))
                  (setq i (+ i 1))))
              (setf (mem-ref (+ ssh #x6D4) :u32) (+ buf-len arr-len)))
            1)))))

;;; ============================================================
;;; tcp-send-conn: handler actors send outbound data via message
;;; ============================================================
;;;
;;; Instead of calling tcp-send-segment-conn (which touches hardware),
;;; pack the data into an array and send to net-domain (actor 2).
;;;
;;; Message protocol to net-domain:
;;;   (cons conn-id byte-array)

(defun tcp-send-conn (cb data len)
  (let ((conn (conn-index-from-base cb)))
    (let ((arr (make-array len)))
      (let ((i 0))
        (loop
          (when (>= i len) (return 0))
          (aset arr i (aref data i))
          (setq i (+ i 1))))
      (send 2 (cons conn arr)))))

;;; ============================================================
;;; net-deliver-data: net-domain sends inbound data to handler
;;; ============================================================
;;;
;;; If handler actor has been spawned (cb+0x18 nonzero), send as message.
;;; If handler not yet spawned (during TCP handshake), write directly to
;;; SSH recv buffer (fallback for piggybacked data in net-wait-ack).

(defun net-deliver-data (conn buf pkt-len tcp-flags)
  (let ((cb (conn-base conn))
        (ssh (conn-ssh conn)))
    (let ((ip-total (buf-read-u16-mem buf 16))
          (tcp-data-off (ash (logand (mem-ref (+ buf 46) :u8) #xF0) -2)))
      (let ((data-len (- ip-total (+ 20 tcp-data-off))))
        (when (> data-len 0)
          (let ((their-seq (buf-read-u32-mem buf 38)))
            (setf (mem-ref (+ cb #x014) :u32) (+ their-seq data-len))
            (tcp-ack-conn cb))
          (let ((data-start (+ (+ (+ buf 14) 20) tcp-data-off)))
            (let ((handler-id (mem-ref (+ cb #x18) :u32)))
              (if (zerop handler-id)
                  ;; No handler yet (TCP handshake) — write directly
                  (let ((buf-len (mem-ref (+ ssh #x6D4) :u32)))
                    (let ((i 0))
                      (loop
                        (when (>= i data-len) (return 0))
                        (setf (mem-ref (+ (+ ssh #x6D8) (+ buf-len i)) :u8)
                              (mem-ref (+ data-start i) :u8))
                        (setq i (+ i 1))))
                    (setf (mem-ref (+ ssh #x6D4) :u32) (+ buf-len data-len)))
                  ;; Handler spawned — send as message
                  (let ((arr (make-array data-len)))
                    (let ((i 0))
                      (loop
                        (when (>= i data-len) (return 0))
                        (aset arr i (mem-ref (+ data-start i) :u8))
                        (setq i (+ i 1))))
                    (send handler-id (cons conn arr)))))))))))  ;; closes let,if,let,let,when,let,let,let,defun

;;; ============================================================
;;; net-actor-main: net-domain with outbound message dispatch
;;; ============================================================
;;;
;;; Interleaves E1000 polling (inbound) with checking mailbox for
;;; outbound messages from handler actors. Uses try-receive (non-blocking).
;;;
;;; Message types (distinguished by car):
;;;   (cons conn-index data-array) — outbound TCP data (conn 0-3)
;;;   (cons 255 (cons sender-id (cons url url-len))) — HTTP fetch request

(defun net-actor-main ()
  (loop
    (io-delay)
    ;; Check for messages from handler actors
    (let ((msg (try-receive)))
      (when (not (zerop msg))
        (let ((tag (car msg)))
          (if (eq tag 255)
              ;; HTTP fetch request
              (let ((inner (cdr msg)))
                (let ((sender (car inner))
                      (url-info (cdr inner)))
                  (let ((url (car url-info))
                        (url-len (cdr url-info)))
                    (let ((result (http-fetch-impl url url-len)))
                      (send sender result)))))
              ;; Outbound TCP data
              (let ((data (cdr msg)))
                (tcp-send-segment-conn (conn-base tag) 24 data (array-length data)))))))
    ;; Poll E1000 for inbound packets
    (let ((pkt-len (e1000-receive)))
      (if (zerop pkt-len)
          (yield)
          (let ((buf (e1000-rx-buf)))
            (let ((et-hi (mem-ref (+ buf 12) :u8))
                  (et-lo (mem-ref (+ buf 13) :u8)))
              (if (eq et-hi #x08)
                  (if (eq et-lo #x06)
                      (let ((arp-op (buf-read-u16-mem buf 20)))
                        (when (eq arp-op 1)
                          (arp-reply buf)))
                      (when (eq et-lo 0)
                        ;; Learn source MAC as gateway MAC (fixes broadcast dst on real hardware)
                        (let ((st (e1000-state-base)))
                          (dotimes (m 6)
                            (setf (mem-ref (+ st (+ #x28 m)) :u8)
                                  (mem-ref (+ buf (+ 6 m)) :u8))))
                        (let ((proto (mem-ref (+ buf 23) :u8)))
                          (if (eq proto 1)
                              (icmp-handle buf 14)
                              (if (eq proto 17)
                                  (udp-handle buf 14)
                                  (when (eq proto 6)
                                    (net-handle-tcp buf pkt-len)))))))
                  ())))))))
