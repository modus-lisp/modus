;;;; isolated-cl.lisp - isolated networking for the CL image: the net domain and
;;;; each SSH session are separate threads, and bytes cross between them only by
;;;; COPY (MODUS:CHANNEL).
;;;;
;;;; The CL successor of net/isolated-net.lisp, on the substrate of
;;;; docs/actors-on-coroutines.md (net/bare-threads.lisp on the Pi): the old file
;;;; did this with net/actors.lisp's per-actor mailboxes, which no CL image runs.
;;;;
;;;;   NET DOMAIN  the main thread (core 0).  The only code that touches the NIC
;;;;               and the TCP state machine: net-actor-main's poll loop, accept,
;;;;               the TCP send path.  It is also where a session's commands are
;;;;               EVALUATED -- a worker thread may not intern a fresh symbol, set
;;;;               a global or bind a special (bare-threads.lisp, WHAT A THREAD MAY
;;;;               DO), and the main thread is the image's Lisp domain anyway.
;;;;   SESSION     one worker thread per connection: the whole SSH protocol --
;;;;               key exchange, host-key signature, ciphers, channel handling --
;;;;               in its own core's heap.  It never touches the NIC: inbound bytes
;;;;               arrive as copies on its own channel, outbound bytes leave as
;;;;               copies on the net domain's.
;;;;
;;;; Messages (plain data, so MODUS:CHANNEL can copy them):
;;;;   net -> session channel (one per connection slot):
;;;;     (:data BYTES)      TCP payload, in order        (:close)   FIN / RST
;;;;     (:result BYTES)    a command's output, ready to send
;;;;   session -> net (one channel):
;;;;     (:send CONN BYTES) send on the connection       (:eval CONN STRING)
;;;;     (:close CONN)      the session is over
;;;;
;;;; Loaded LAST in the SSH transport (after usb-netdev.lisp): every definition
;;;; here replaces one by last-defun-wins.  Shared by design, and stated: the
;;;; server's ephemeral X25519 key (one slot, refreshed after each key exchange)
;;;; and the interactive line buffer in ssh-ipc scratch, so two INTERACTIVE
;;;; sessions at once would share a line; `ssh host EXPR' (exec) does not use it.

;;; ------------------------------------------------------------
;;; Plumbing.  MODUS and SB-THREAD exist only once the image has booted (their
;;; shims are evaluated then), so this file names their functions at run time,
;;; once, on the main thread, and keeps them in globals the workers only READ.
;;; ------------------------------------------------------------
(defvar *iso-up* nil)
(defvar *iso-send* nil)          ; MODUS:CHANNEL-SEND
(defvar *iso-recv* nil)          ; MODUS:CHANNEL-RECEIVE
(defvar *iso-try* nil)           ; MODUS:CHANNEL-TRY-RECEIVE
(defvar *iso-spawn* nil)         ; SB-THREAD:MAKE-THREAD
(defvar *iso-join* nil)          ; SB-THREAD:JOIN-THREAD
(defvar *iso-to-net* nil)
(defvar *iso-to-session* nil)    ; one channel per connection slot (4)
(defvar *iso-threads* nil)       ; the session thread of each slot, or NIL
(defvar *iso-slot-conn* nil)     ; thread slot (percpu 16) -> its connection, or -1

;; Each session's own "the peer has gone" word.  SSH-PEER-CLOSED-P's word at
;; ssh-ipc+0x60458 is ONE for the server -- right when one connection is served
;; at a time, wrong here: the first client to hang up ended every session.
(defun %iso-closed-addr (conn) (+ (conn-ssh conn) #x3FC8))

(defun %iso-fn (name pkg) (symbol-function (find-symbol name pkg)))

(defun %iso-main-p () (zerop (percpu-ref 16)))

(defun %iso-init ()
  (unless *iso-up*
    (setq *iso-send* (%iso-fn "CHANNEL-SEND" "MODUS"))
    (setq *iso-recv* (%iso-fn "CHANNEL-RECEIVE" "MODUS"))
    (setq *iso-try* (%iso-fn "CHANNEL-TRY-RECEIVE" "MODUS"))
    (setq *iso-spawn* (%iso-fn "MAKE-THREAD" "SB-THREAD"))
    (setq *iso-join* (%iso-fn "JOIN-THREAD" "SB-THREAD"))
    (let ((mk (%iso-fn "MAKE-CHANNEL" "MODUS")))
      (setq *iso-to-net* (funcall mk :capacity 262144 :name "net domain"))
      (setq *iso-to-session*
            (vector (funcall mk :capacity 262144 :name "session 0")
                    (funcall mk :capacity 262144 :name "session 1")
                    (funcall mk :capacity 262144 :name "session 2")
                    (funcall mk :capacity 262144 :name "session 3"))))
    (setq *iso-threads* (make-array 4 :initial-element nil))
    (setq *iso-slot-conn* (make-array 16 :initial-element -1))
    (setq *iso-up* t)
    (write-string-serial "ISOLATED: net domain on core 0, sessions on worker threads")
    (write-char-serial 10)))

(defun %iso-conn-of-ssh (ssh) (ash (- (- ssh #x20) (ssh-conn-base)) -14))

(defun %iso-bytes (data len)
  "A fresh (unsigned-byte 8) copy of the first LEN elements of DATA."
  (let ((v (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len) (setf (aref v i) (logand (aref data i) 255)))
    v))

(defun %iso-to-session (conn msg) (funcall *iso-send* (aref *iso-to-session* conn) msg))
(defun %iso-to-net (msg) (funcall *iso-send* *iso-to-net* msg))

;;; ------------------------------------------------------------
;;; NET DOMAIN (main thread)
;;; ------------------------------------------------------------

;; ip.lisp's TCP-SEND-CONN, verbatim: the real send, which only the net domain
;; may run.  (A segment must fit one frame; split at 1360 bytes.)
(defun %iso-tcp-send (cb data len)
  (if (<= len 1360)
      (tcp-send-segment-conn cb 24 data len)
      (let ((off 0))
        (loop
          (when (>= off len) (return ()))
          (let ((n (- len off)))
            (when (> n 1360) (setq n 1360))
            (let ((chunk (make-array n)))
              (dotimes (i n) (aset chunk i (aref data (+ off i))))
              (tcp-send-segment-conn cb 24 chunk n))
            (setq off (+ off n)))))))

;; A session sends by message; the net domain sends for real.
(defun tcp-send-conn (cb data len)
  (if (%iso-main-p)
      (%iso-tcp-send cb data len)
      (%iso-to-net (list :send (ash (- cb (ssh-conn-base)) -14) (%iso-bytes data len)))))

;; ip.lisp's NET-DELIVER-DATA, but the in-order payload goes to the connection's
;; SESSION as a copy instead of into its receive buffer, and a FIN / RST tells
;; the session the peer has gone.
(defun net-deliver-data (conn buf pkt-len tcp-flags)
  (let ((cb (conn-base conn)))
    (let ((ip-total (buf-read-u16-mem buf 16))
          (tcp-data-off (ash (logand (mem-ref (+ buf 46) :u8) #xF0) -2)))
      (let ((data-len (- ip-total (+ 20 tcp-data-off))))
        (when (> data-len 0)
          (let ((their-seq (buf-read-u32-mem buf 38))
                (expected (mem-ref (+ cb #x014) :u32)))
            (if (eql their-seq expected)
                (let ((v (make-array data-len :element-type '(unsigned-byte 8)))
                      (data-start (+ (+ (+ buf 14) 20) tcp-data-off)))
                  (dotimes (i data-len) (setf (aref v i) (mem-ref (+ data-start i) :u8)))
                  (setf (mem-ref (+ cb #x014) :u32) (+ their-seq data-len))
                  (tcp-ack-conn cb)
                  (%iso-to-session conn (list :data v)))
                (tcp-ack-conn cb))))))
    (when (not (zerop (logand tcp-flags 5)))
      (%iso-to-session conn (list :close)))))

;; Accept spawns the session instead of running it inline: NET-ACCEPT-CONNECTION
;; (ip.lisp) calls this once the handshake is done, and returns to the poll loop.
;; EACH SESSION HAS ITS OWN EPHEMERAL X25519 PAIR, in its SSH block at +0x3F80
;; (the private key) / +0x3FA0 (the public key).  The server-wide slot was
;; refreshed by the net domain after every key exchange: on a board where an
;; X25519 takes seconds (QEMU virt: ~40 s) that held up every accept for as
;; long, and it rewrote the key under any session still in its exchange.  The
;; net domain draws the private key (the DRBG is its own); the session computes
;; the public half on its own core before it speaks.
(defun ssh-eph-base (ssh) (+ ssh #x3F80))
(defun ssh-refresh-ephemeral () nil)

(defun %iso-session-eph (ssh)
  (let ((priv (make-array 32)))
    (dotimes (i 32) (setf (aref priv i) (mem-ref (+ ssh #x3F80 i) :u8)))
    (let ((pub (x25519-public-key priv)))
      (dotimes (i 32) (setf (mem-ref (+ ssh #x3FA0 i) :u8) (aref pub i))))))

(defun ssh-connection-handler (conn)
  (%iso-init)
  (let ((ssh (conn-ssh conn)))
    (dotimes (i 32) (setf (mem-ref (+ ssh #x3F80 i) :u8) (ssh-random ssh))))
  ;; a reused slot: drop whatever the last session left unread
  (let ((ch (aref *iso-to-session* conn)))
    (loop (when (null (funcall *iso-try* ch)) (return nil))))
  (setf (mem-ref (%iso-closed-addr conn) :u32) 0)
  (let ((c conn))
    (setf (aref *iso-threads* conn)
          (funcall *iso-spawn* (lambda () (%iso-session c)) :name "ssh session"))))

(defun %iso-eval (s)
  "The reply bytes for command S, as the single-threaded SSH-EVAL-LINE builds
   them: what it printed, then `= VALUE'."
  (let ((out nil))
    (let ((result (handler-case
                      (let ((so (make-string-output-stream)))
                        (let ((r (let ((*standard-output* so)) (eval (read-from-string s)))))
                          (setq out (get-output-stream-string so))
                          r))
                    (t (c) (list (quote error) c)))))
      (let ((rs (handler-case (prin1-to-string result)
                  (t (c) (prin1-to-string (quote unprintable))))))
        (let* ((rl (length rs)) (ol (if out (length out) 0))
               (arr (make-array (+ ol rl 3) :element-type '(unsigned-byte 8))))
          (dotimes (i ol) (setf (aref arr i) (logand (char-code (aref out i)) 255)))
          (setf (aref arr ol) 61) (setf (aref arr (+ ol 1)) 32)
          (dotimes (i rl) (setf (aref arr (+ ol 2 i)) (logand (char-code (aref rs i)) 255)))
          (setf (aref arr (+ ol 2 rl)) 10)
          arr)))))

(defun %iso-net-step ()
  "Serve what the sessions sent: bounded, so the NIC is polled between."
  (when *iso-up*
    (let ((n 0))
      (loop
        (when (> n 32) (return nil))
        (let ((m (funcall *iso-try* *iso-to-net*)))
          (when (null m) (return nil))
          (setq n (+ n 1))
          (let ((kind (car m)) (conn (cadr m)))
            (cond
              ((eq kind :send)
               (let ((bytes (caddr m)))
                 (%iso-tcp-send (conn-base conn) bytes (length bytes))))
              ((eq kind :eval)
               (%iso-to-session conn (list :result (%iso-eval (caddr m)))))
              ((eq kind :close)
               (tcp-close-conn (conn-base conn))
               (conn-free conn)
               (let ((th (aref *iso-threads* conn)))
                 (when th
                   (setf (aref *iso-threads* conn) nil)
                   (handler-case (funcall *iso-join* th) (t (c) nil))))
               (ssh-refresh-ephemeral)))))))))

;; NET-ACTOR-MAIN calls this on every pass of its poll loop: the hot-plug check
;; it always was (usb-netdev.lisp, verbatim), then the sessions' traffic.
(defun usb-netdev-hotplug-poll ()
  (let ((hprt (dwc2-read (dwc2-hprt0))))
    (when (not (zerop (logand hprt (hprt0-prtconndet))))
      (dwc2-write (dwc2-hprt0)
                  (logior (logand hprt (dwc2-hprt0-mask)) (hprt0-prtconndet)))
      (if (zerop (logand hprt (hprt0-prtconnsts)))
          (progn (usb-netdev-set 0)
                 (%serial-byte 78) (%serial-byte 68) (%serial-byte 58)
                 (%serial-byte 85) (%serial-byte 10))   ; "ND:U" unplug
          (net-usb-probe))))
  (%iso-net-step))

;;; ------------------------------------------------------------
;;; SESSION (worker thread)
;;; ------------------------------------------------------------

(defun %iso-session (conn)
  (setf (aref *iso-slot-conn* (percpu-ref 16)) conn)   ; a fixnum: no pointer
  (handler-case (progn (%iso-session-eph (conn-ssh conn))
                       (ssh-handle-connection (conn-ssh conn)))
    ;; Said on the console: the session just ends, and nothing else would show why.
    (t (c) (handler-case (progn (write-string-serial "ISOLATED: session error: ")
                                (write-string-serial (princ-to-string c))
                                (write-char-serial 10))
             (t (c2) nil))))
  (%iso-to-net (list :close conn))
  :done)

(defun %iso-take (ssh msg)
  "Apply one net-domain message to this session.  0 = the peer is gone."
  (let ((kind (car msg)))
    (cond
      ((eq kind :data)
       (let* ((v (cadr msg)) (n (length v))
              (buf-len (mem-ref (+ ssh #x6D4) :u32)))
         (dotimes (i n)
           (setf (mem-ref (+ (+ ssh #x6D8) (+ buf-len i)) :u8) (aref v i)))
         (setf (mem-ref (+ ssh #x6D4) :u32) (+ buf-len n))
         1))
      ((eq kind :close)
       (setf (mem-ref (+ ssh #x3FC8) :u32) 1)   ; this session's SSH-PEER-CLOSED-P
       0)
      (t 1))))

;; ssh.lisp's SSH-MESSAGE-LOOP, verbatim but for FLAG-ADDR: its "session over"
;; word was one for the server (conn 3's struct +0x700), so the first session to
;; finish ended every other one.  Here each session has its own.
(defun ssh-message-loop (ssh)
  (let ((flag-addr (+ ssh #x3FCC))
        (idle 0)
        (since (ssh-preauth-clock)))
    (setf (mem-ref flag-addr :u32) 1)
    (loop
      (when (zerop (mem-ref flag-addr :u32)) (return ()))
      (when (ssh-peer-closed-p) (return ()))
      (when (and (zerop (mem-ref (+ ssh #x10) :u32))
                 (ssh-wait-expired-p idle (ssh-preauth-idle-limit) since))
        (return ()))
      (let ((pkt (ssh-receive-packet ssh 50000)))
        (if pkt
            (progn
              (setq idle 0)
              (setq since (ssh-preauth-clock))
              (ssh-dispatch-msg ssh (car pkt) (cdr pkt) flag-addr))
            (setq idle (+ idle 1)))))))

;; A session asks about ITS peer; the net domain keeps ip.lisp's word.
(defun ssh-peer-closed-p ()
  (if (%iso-main-p)
      (= (mem-ref (+ (ssh-ipc-base) #x60458) :u32) 1)
      (let ((c (aref *iso-slot-conn* (percpu-ref 16))))
        (and (>= c 0) (= (mem-ref (%iso-closed-addr c) :u32) 1)))))

;; SSH-RECEIVE-PACKET's wait: the session's channel, never the NIC.
(defun ssh-wait-data (ssh)
  (if (%iso-main-p)
      (ssh-net-poll)
      (let ((m (funcall *iso-recv* (aref *iso-to-session* (%iso-conn-of-ssh ssh)) :timeout 0.01)))
        (if (null m) 1 (%iso-take ssh m)))))

;; A command: evaluated in the net domain (see the header), the reply sent from
;; here.  Data that arrives meanwhile is applied, not dropped.
(defun ssh-eval-line (ssh cmd cmd-len)
  (let ((s (make-string cmd-len)) (conn (%iso-conn-of-ssh ssh)))
    (dotimes (i cmd-len) (setf (aref s i) (code-char (aref cmd i))))
    (%iso-to-net (list :eval conn s))
    (let ((reply nil))
      (loop
        (when reply (return nil))
        (let ((m (funcall *iso-recv* (aref *iso-to-session* conn) :timeout 1)))
          (when m
            (if (eq (car m) :result)
                (setq reply (cadr m))
                (%iso-take ssh m)))))
      (ssh-send-string ssh reply (length reply)))))
