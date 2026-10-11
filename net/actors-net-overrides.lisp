;;;; actors-net-overrides.lisp - Actor-aware overrides for AArch64
;;;;
;;;; Loaded LAST (after ssh.lisp) to re-override functions
;;;; that need actor-aware behavior instead of single-threaded operation.
;;;;
;;;; Overrides:
;;;;   receive              - yield instead of inline E1000 poll
;;;;   net-actor-main       - yield on idle + fetch message dispatch
;;;;   net-accept-connection - spawn handler actor (SSH or HTTP by port)
;;;;   ssh-connection-handler - call actor-exit when done
;;;;   http-fetch           - route through net-domain actor via message

;;; ============================================================
;;; receive: yield to let net-actor-main poll E1000
;;; ============================================================
;;;
;;; In single-threaded mode, receive() polls E1000 inline.
;;; With actors, net-actor-main (actor 2) polls E1000 in its own loop.
;;; SSH handler actors call receive() when waiting for data.
;;; Yield gives net-actor-main CPU time, return 1 so the retry loop continues.

(defun receive ()
  (yield)
  1)
(defun ssh-net-poll () (receive))   ; net/ssh.lisp's SSH-WAIT-DATA waits here

;;; ============================================================
;;; net-actor-main: actor-aware network polling loop
;;; ============================================================
;;;
;;; Yields on idle instead of busy-waiting, allowing handler actors to run.
;;; Also checks mailbox for fetch requests from handler actors.
;;; Fetch message format: (cons 255 (cons sender-id (cons url url-len)))

(defun net-actor-main ()
  (loop
    (io-delay)
    ;; Check for fetch requests from handler actors
    (let ((msg (try-receive)))
      (when (not (zerop msg))
        ;; Extract sender and URL from message
        (let ((inner (cdr msg)))
          (let ((sender (car inner))
                (url-info (cdr inner)))
            (let ((url (car url-info))
                  (url-len (cdr url-info)))
              (let ((result (http-fetch-impl url url-len)))
                (send sender result)))))))
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

;;; ============================================================
;;; Per-connection handler entry functions
;;; ============================================================
;;;
;;; Each connection handler is a separate function so fn-addr can resolve it.
;;; conn-alloc returns 0-3; actor-spawn-handler dispatches to the right one.
;;; Entry functions MUST NOT return (actors have no return address on stack).

(defun ssh-handler-0 ()
  (ssh-connection-handler 0))

(defun ssh-handler-1 ()
  (ssh-connection-handler 1))

(defun ssh-handler-2 ()
  (ssh-connection-handler 2))

(defun ssh-handler-3 ()
  (ssh-connection-handler 3))

(defun http-handler-0 ()
  (http-connection-handler 0))

(defun http-handler-1 ()
  (http-connection-handler 1))

(defun http-handler-2 ()
  (http-connection-handler 2))

(defun http-handler-3 ()
  (http-connection-handler 3))

;;; ============================================================
;;; Spawn dispatch (fn-addr requires static function names)
;;; ============================================================
;;; conn-base+0x1C: protocol type (0=SSH, 1=HTTP)

(defun actor-spawn-http (conn)
  (if (eq conn 0) (actor-spawn (fn-addr http-handler-0))
      (if (eq conn 1) (actor-spawn (fn-addr http-handler-1))
          (if (eq conn 2) (actor-spawn (fn-addr http-handler-2))
              (actor-spawn (fn-addr http-handler-3))))))

(defun actor-spawn-handler (conn)
  (let ((protocol (mem-ref (+ (conn-base conn) #x1C) :u32)))
    (if (eq protocol 1)
        (actor-spawn-http conn)
        (if (eq conn 0) (actor-spawn (fn-addr ssh-handler-0))
            (if (eq conn 1) (actor-spawn (fn-addr ssh-handler-1))
                (if (eq conn 2) (actor-spawn (fn-addr ssh-handler-2))
                    (actor-spawn (fn-addr ssh-handler-3))))))))

;;; ============================================================
;;; ssh-connection-handler: actor-aware (calls actor-exit)
;;; ============================================================

(defun ssh-connection-handler (conn)
  (let ((ssh (conn-ssh conn))
        (cb (conn-base conn)))
    (ssh-handle-connection ssh)
    (pre-compute-server-eph ssh)
    (tcp-close-conn cb)
    (conn-free conn)
    (actor-exit)))

;;; ============================================================
;;; net-accept-connection: spawn handler actor
;;; ============================================================
;;;
;;; Same TCP handshake as the ip.lisp version, but spawns a
;;; handler actor instead of calling ssh-connection-handler inline.

(defun net-accept-connection (src-ip src-port dst-port buf)
  (let ((conn (conn-alloc)))
    (when (not (= conn (- 0 1)))
      (conn-init conn dst-port src-port src-ip)
      ;; Clear protocol type (uninitialized RAM on real hardware)
      (setf (mem-ref (+ (conn-base conn) #x1C) :u32) 0)
      ;; Set protocol type: port 80 → HTTP (1), else SSH (0)
      (when (eq dst-port 80)
        (setf (mem-ref (+ (conn-base conn) #x1C) :u32) 1))
      (let ((their-seq (buf-read-u32-mem buf 38)))
        (setf (mem-ref (+ (conn-base conn) #x014) :u32) (+ their-seq 1)))
      ;; Init recv buffer BEFORE net-wait-ack so piggybacked data works
      (let ((ssh (conn-ssh conn)))
        (setf (mem-ref (+ ssh #x6D4) :u32) 0))
      (tcp-send-segment-conn (conn-base conn) 18 (make-array 0) 0)
      (net-wait-ack conn)
      (when (eq (mem-ref (conn-base conn) :u32) 2)
        ;; SSH-specific init (skip for HTTP)
        (when (eq (mem-ref (+ (conn-base conn) #x1C) :u32) 0)
          (ssh-copy-host-key conn)
          (let ((ssh (conn-ssh conn)))
            (setf (mem-ref ssh :u32) 0)
            (setf (mem-ref (+ ssh #x04) :u32) 0)
            (setf (mem-ref (+ ssh #x08) :u32) 0)
            (setf (mem-ref (+ ssh #x0C) :u32) 0)
            (setf (mem-ref (+ ssh #x10) :u32) 0)
            (setf (mem-ref (+ ssh #x28) :u32) 0)
            (setf (mem-ref (+ ssh #x150) :u32) 0)
            (setf (mem-ref (+ ssh #x154) :u32) 0)
            (setf (mem-ref (+ ssh #x2C) :u32)
                  (+ src-port (* src-ip 7)))))
        (setf (mem-ref (+ (ssh-ipc-base) #x60448) :u32) conn)
        ;; Spawn handler actor and store its ID for message routing
        (let ((handler-id (actor-spawn-handler conn)))
          (setf (mem-ref (+ (conn-base conn) #x18) :u32) handler-id))))))

;;; ============================================================
;;; usb-keepalive: actor-aware (yield instead of direct USB poll)
;;; ============================================================
;;;
;;; On USB gadget builds, usb-keepalive is called during crypto to prevent
;;; host NETDEV WATCHDOG timeout. With actors, yielding gives the net-domain
;;; actor CPU time to poll USB, avoiding the need for inline USB polling.

(defun usb-keepalive ()
  (let ((i 0))
    (loop
      (when (>= i 10) (return 0))
      (yield)
      (setq i (+ i 1)))))

;;; ============================================================
;;; http-fetch: actor-aware (route through net-domain)
;;; ============================================================
;;;
;;; Handler actors can't call e1000-receive directly (net-domain owns it).
;;; Send fetch request to net-domain (actor 2), wait for response.
;;; Message format: (cons 255 (cons sender-id (cons url url-len)))
;;; Response: (cons resp-array resp-length) or 0

(defun http-fetch (url url-len)
  ;; Send fetch request to net-domain (actor 2)
  (let ((self-id (actor-self)))
    (let ((url-pair (cons url url-len)))
      (let ((inner (cons self-id url-pair)))
        (send 2 (cons 255 inner)))))
  ;; Wait for response
  (let ((result 0) (done 0))
    (loop
      (when (not (zerop done)) (return 0))
      (let ((msg (try-receive)))
        (if (not (zerop msg))
            (progn (setq result msg) (setq done 1))
            (yield))))
    (http-print-result result)))
