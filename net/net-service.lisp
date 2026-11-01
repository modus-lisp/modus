;;;; net-service.lisp - the network, served by ONE core-0 thread.
;;;;
;;;; Before this, the NIC was read by whoever happened to be waiting: net/ip.lisp's
;;;; NET-ACTOR-MAIN or its TCP-RECEIVE, net/tcp-conns.lisp's NET-PUMP from inside any blocking
;;;; socket call ("blocking is pumping"), a desk's idle hook.  A frame went to whichever of
;;;; them read it, and nothing answered ARP or ping while nobody happened to be waiting.
;;;;
;;;; Now one thread owns the receive side.  It is a CORE-0 THREAD (net/bare-threads.lisp,
;;;; CORE 0): a coroutine of the main thread that shares its heap, so it may keep the
;;;; connection table, the receive buffers and the UDP queues as ordinary Lisp objects, and
;;;; the main thread runs it wherever main waits (SLEEP, a lock, the console, BT-YIELD).
;;;; It does not poll: with nothing to do it BLOCKS on the USB I/O core's receive ring
;;;; (net/usb-io-core.lisp), so it runs when a frame has arrived or a retransmission is due.
;;;;
;;;; THE POLICY THAT MAKES THIS WORK: NOTHING ON CORE 0 HOLDS THE CORE.  The main thread is
;;;; never preempted, so a main that computes for a second without waiting holds the network
;;;; for that second.  The rule is not enforced; it is WATCHED.  A second core-0 thread asks
;;;; to run every 10 ms and logs (kmsg tag PAUS) every time it was kept waiting past
;;;; *NS-PAUSE-MS*: how long, whether a collection happened meanwhile, and how many frames
;;;; were waiting in the ring.  So a stall is a log line with a size, not a dead network.
;;;;
;;;; Every frame goes to exactly one place:
;;;;   ARP request for us        answered here (ARP-REPLY)
;;;;   ICMP echo for us          answered here (ICMP-HANDLE)
;;;;   TCP for a tcp-conns connection or listener, UDP for a bound port: tcp-conns
;;;;   everything else           the LEGACY queue: net/ip.lisp's own TCP (the one client
;;;;                             connection, the SSH server), ARP replies, DHCP, broadcasts.
;;;; MODUS.NET:NIC-RECEIVE is rebound to read the legacy queue, so every caller of the old
;;;; interface keeps working and none of them touches the NIC.  They move to tcp-conns one by
;;;; one; when the queue is never used, it goes.
;;;;
;;;; Clients block on the service, not on the NIC: %PUMP-UNTIL (every blocking socket call)
;;;; waits on a generation word the service bumps after each pass that delivered something
;;;; (net/tcp-conns.lisp, with *NS-THREAD*, so a reloaded copy of it still agrees).
;;;;
;;;; Loaded at runtime after net/tcp-conns.lisp (and net/bare-sockets.lisp when wanted).
;;;;   (modus-int::net-service-start)    (modus-int::net-service-status)

(in-package :modus-int)

(defvar *ns-watchdog* nil)
(defvar *ns-shipper* nil)
(defvar *ns-stop* nil)
(defvar *ns-receive* nil "The NIC's NIC-RECEIVE, taken when the service starts: only the service calls it.")
(defvar *ns-rx-buf* nil)
(defvar *ns-pause-ms* 100 "A wait longer than this is logged as a pause.")
(defvar *ns-stats* (make-array 16 :initial-element 0))
;; 0 frames 1 tcp 2 udp 3 arp 4 icmp 5 legacy 6 legacy dropped 7 passes 9 waits
;; 10 accepted 11 evaluations 12 refused (no free slot) 13 pass errors 14 restarts
(defvar *ns-wd* (make-array 8 :initial-element 0))
(defvar *ns-legacy-bufs* (let ((v (make-array 32)))
                           (dotimes (i 32 v)
                             (setf (svref v i) (make-array 2048 :element-type '(unsigned-byte 8))))))
(defvar *ns-legacy-lens* (make-array 32 :initial-element 0))
(defvar *ns-legacy-idx* (make-array 2 :initial-element 0) "head, tail")
(defvar *ns-staged* (make-array 1 :initial-element 0) "1: the frame handed out is in %NS-STAGE")

;; Raw memory, because callers of the old interface read the frame with MEM-REF at the
;; address NIC-RX-BUF gives and may cons (a collection would move a Lisp vector under them).
;; Inside the E1000 RX-buffer block, which a USB NIC does not use (CDC takes its first 2 KB,
;; kiln's executor-mode staging 0x11010000).
(defun %ns-stage () #x11030000)

(defmacro %ns-incf (vec i &optional (n 1)) `(setf (svref ,vec ,i) (+ (svref ,vec ,i) ,n)))

(defun %ns-bump-gen ()
  (setf (mem-ref (%ns-gen-addr) :u32) (logand (+ (%ns-gen) 1) #xFFFFFFF)))

(defun %ns-engine-p () (and (fboundp 'usbe-running-p) (usbe-running-p)))


;;; ---- the legacy queue ------------------------------------------------------------------

(defun %ns-legacy-put (buf len)
  (let* ((idx *ns-legacy-idx*) (tail (svref idx 1)) (next (mod (1+ tail) 32)))
    (if (= next (svref idx 0))
        (%ns-incf *ns-stats* 6)                                   ; full: dropped
        (let ((v (svref *ns-legacy-bufs* tail)) (n (min len 2048)))
          (dotimes (k n) (setf (aref v k) (mem-ref (+ buf k) :u8)))
          (setf (svref *ns-legacy-lens* tail) n (svref idx 1) next)
          (%ns-incf *ns-stats* 5)))))

(defun %ns-nic-receive ()
  "MODUS.NET:NIC-RECEIVE while the service runs: the oldest legacy frame, copied to the
   stage.  Its length, or 0."
  (if (null *ns-thread*)
      (funcall *ns-receive*)
      (let* ((idx *ns-legacy-idx*) (head (svref idx 0)))
        (if (= head (svref idx 1))
            ;; Nothing yet: let core 0's threads run once -- this service is one of them, and
            ;; a caller polling here on the MAIN thread (ip.lisp's TCP, the HTTP fetch, inside
            ;; an SSH evaluation) would otherwise spin with the queue never filled.
            (progn (setf (svref *ns-staged* 0) 0)
                   (if (%bt-slot-p) (bt-yield) (%bt-main-idle))
                   0)
            (let ((v (svref *ns-legacy-bufs* head)) (n (svref *ns-legacy-lens* head)) (b (%ns-stage)))
              (dotimes (k n) (setf (mem-ref (+ b k) :u8) (aref v k)))
              (setf (svref idx 0) (mod (1+ head) 32)
                    (svref *ns-staged* 0) 1
                    (mem-ref (+ (e1000-state-base) #x44) :u32) n)
              n)))))

(defun %ns-nic-rx-buf ()
  (if (= (svref *ns-staged* 0) 1) (%ns-stage) (funcall *ns-rx-buf*)))

;;; ---- dispatch --------------------------------------------------------------------------

(defun %ns-u16 (a) (logior (ash (mem-ref a :u8) 8) (mem-ref (+ a 1) :u8)))
(defun %ns-u32 (a) (logior (ash (%ns-u16 a) 16) (%ns-u16 (+ a 2))))

(defun %ns-dispatch (buf len me)
  (let ((et (%ns-u16 (+ buf 12))))
    (cond
      ((= et #x0806)                                              ; ARP
       (%ns-incf *ns-stats* 3)
       (if (and (= (%ns-u16 (+ buf 20)) 1) (= (%ns-u32 (+ buf 38)) me))
           (arp-reply buf)
           (%ns-legacy-put buf len)))                             ; replies: ARP-RESOLVE waits
      ((/= et #x0800) nil)
      (t
       (let* ((ihl (* 4 (logand (mem-ref (+ buf 14) :u8) 15)))
              (proto (mem-ref (+ buf 23) :u8))
              (src (%ns-u32 (+ buf 26)))
              (dst (%ns-u32 (+ buf 30)))
              (l4 (+ buf 14 ihl)))
         (cond
           ((/= dst me) (%ns-legacy-put buf len))                 ; broadcast, DHCP
           ((and (= proto 1) (= (mem-ref l4 :u8) 8))              ; echo request
            (%ns-incf *ns-stats* 4) (icmp-handle buf 14))
           ((and (= proto 6)
                 (let ((sport (%ns-u16 l4)) (dport (%ns-u16 (+ l4 2))))
                   (or (%tcp-find src sport dport)
                       (and (logbitp 1 (mem-ref (+ l4 13) :u8)) (not (logbitp 4 (mem-ref (+ l4 13) :u8)))
                            (find dport *tcp-listeners* :key #'tl-port)))))
            (%ns-incf *ns-stats* 1) (%tcp-input buf ihl src))
           ((and (= proto 17) (assoc (%ns-u16 (+ l4 2)) *udp-ports*))
            (%ns-incf *ns-stats* 2) (%udp-input buf ihl src))
           (t (%ns-legacy-put buf len))))))))

(defun %ns-drain (max)
  "Up to MAX frames off the NIC, each to its place.  How many."
  (let ((n 0) (me (%our-ip)))
    (with-net-lock
      (loop
        (when (>= n max) (return))
        (let ((len (funcall *ns-receive*)))
          (when (or (null len) (<= len 0)) (return))
          (setq n (+ n 1))
          (%ns-dispatch (funcall *ns-rx-buf*) len me)))
      (%tcp-retransmit))
    n))

(defun %ns-pending-p ()
  "Something is waiting on a timer: unacknowledged segments."
  (dolist (c *tcp-conns* nil) (when (tc-unacked c) (return t))))

(defun %ns-wait-for-frames (ms)
  "Block until the I/O core's receive ring has a frame or MS pass.  Without the I/O core
   there is nothing to block on, and this is a short sleep: the old interface's poll."
  (%ns-incf *ns-stats* 9)
  (if (%ns-engine-p)
      (%ns-wait (%usbe-addr #x1000) (usbe-get #x1004) ms)
      (%ns-wait 0 0 (min ms 5))))

(defun %ns-pass ()
  "One pass: the NIC, then the handlers' traffic; then wait if there was nothing."
  (let ((n (%ns-drain 64))
        (r (%ns-relays-step)))
    (%ns-incf *ns-stats* 7)
    (%ns-incf *ns-stats* 0 n)
    (if (or (> n 0) r)
        (progn (when (> n 0) (%ns-bump-gen)) (bt-yield))          ; let the clients see it
        ;; A handler's messages do not wake the service (its channel has no word the
        ;; ring's waiter can watch), so with handlers live it looks every 2 ms.
        (%ns-wait-for-frames (cond ((%ns-relays-live-p) 2) ((%ns-pending-p) 20) (t 250))))))

(defun %ns-main ()
  ;; AN ERROR ENDS A PASS, NOT THE SERVICE: logged (kmsg NETE) and the next pass runs.  A
  ;; reload of net/tcp-conns.lisp under a live service (kiln zero carries one) killed it once,
  ;; silently, and with it every connection.
  (loop
    (when *ns-stop* (return :stopped))
    (handler-case (%ns-pass)
      (error (e)
        (%ns-incf *ns-stats* 13)
        (handler-case (kmsg 3 "NETE" (princ-to-string e)) (error () nil))
        (%ns-wait 0 0 10)))))

;;; ---- inetd: the service accepts, a handler thread serves -----------------------------------
;;;
;;; (NET-SERVICE-LISTEN PORT SPAWN): the service listens on PORT, accepts each connection
;;; itself, and calls SPAWN with (SLOT CHANNEL) on the service thread.  SPAWN starts a
;;; handler -- a thread anywhere, cores 1..3 included, since it shares nothing with the
;;; service but two channels -- and returns its thread, or NIL to refuse.  Then:
;;;   service -> handler, on CHANNEL (the slot's own):
;;;     (:data BYTES)   what the peer sent, in order   (:close)  the peer has finished
;;;     (:result BYTES) an evaluation's reply (below)
;;;   handler -> service, on *NS-TO-SERVICE* (one for all):
;;;     (:send SLOT BYTES)   (:close SLOT)   (:eval SLOT STRING)
;;; The service never blocks on a handler: what it is given to send waits in the slot's
;;; queue for window, and a handler that died without (:close) is closed for it.
;;; (:eval SLOT STRING) reads and evaluates STRING on the MAIN THREAD, in the order they
;;; arrive, while it waits at the console for a key (%BT-MAIN-JOBS, below) -- what the old
;;; server did, and what a client like kiln's desk relies on: `(kiln-zero-main)' sent over
;;; SSH takes the main thread over.  Not on a thread of its own: evaluation off the main
;;; thread loses SETQs under DOTIMES/DOLIST and can wedge core 0 (measured on the Zero:
;;; (let ((s 0)) (dotimes (i 3) (setq s (+ s 1))) s) is 3 on main and 0 on a core-0 thread,
;;; and EVAL of a fresh form on a thread hung the board).  While main evaluates, core 0's
;;; other threads -- this service among them -- wait for it to sleep, wait or finish (the
;;; watchdog logs the pause, PAUS); a busy evaluation holds up the network, as it did before.

(defun %ns-nslots () 8)
(defvar *ns-listeners* nil "Alist port -> (listener . spawn).")
(defvar *ns-to-service* nil)
(defvar *ns-slot-ch* nil "Each slot's channel to its handler, made once.")
(defvar *ns-slots* nil "Each slot: NIL, or #(conn thread queue state eof-told).")

;; MODUS:MAKE-CHANNEL and friends, found when the service starts: the MODUS package exists
;; only in the image (its surface is installed at boot), and the build host reads this file.
(defvar *ns-chan-send* nil)
(defvar *ns-chan-try* nil)
(defvar *ns-chan-recv* nil)
(defun %ns-modus-fn (name) (symbol-function (find-symbol name "MODUS")))
(defun %ns-send (ch msg) (funcall *ns-chan-send* ch msg))
(defun %ns-try (ch) (funcall *ns-chan-try* ch))

(defun %ns-relay-init ()
  (unless *ns-to-service*
    (setq *ns-chan-send* (%ns-modus-fn "CHANNEL-SEND")
          *ns-chan-try* (%ns-modus-fn "CHANNEL-TRY-RECEIVE")
          *ns-chan-recv* (%ns-modus-fn "CHANNEL-RECEIVE"))
    (setq *ns-main-jobs* (funcall (%ns-modus-fn "MAKE-CHANNEL") :capacity 262144 :name "to the main thread"))
    (setq *ns-to-service* (funcall (%ns-modus-fn "MAKE-CHANNEL") :capacity 262144 :name "to the network service")
          *ns-slot-ch* (make-array (%ns-nslots))
          *ns-slots* (make-array (%ns-nslots) :initial-element nil))
    (dotimes (i (%ns-nslots))
      (setf (svref *ns-slot-ch* i) (funcall (%ns-modus-fn "MAKE-CHANNEL") :capacity 262144 :name "to a handler")))))

(defun net-service-listen (port spawn)
  "Serve PORT: each connection to it goes to a thread SPAWN starts (see above)."
  (%ns-relay-init)
  (let ((l (tcpc-listen port)) (e (assoc port *ns-listeners*)))
    (if e (setf (cdr e) (cons l spawn)) (push (cons port (cons l spawn)) *ns-listeners*))
    port))

(defun %ns-relays-live-p ()
  (and *ns-slots* (find-if #'identity *ns-slots*)))

(defun %ns-free-slot ()
  (dotimes (i (%ns-nslots) nil) (unless (svref *ns-slots* i) (return i))))

(defun %ns-accept ()
  (let ((any nil))
    (dolist (e *ns-listeners* any)
      (let ((l (cadr e)) (spawn (cddr e)))
        (loop
          (let ((c (with-net-lock (pop (tl-ready l)))))
            (unless c (return))
            (setq any t)
            (let ((i (%ns-free-slot)))
              (if (null i)
                  (progn (%ns-incf *ns-stats* 12) (tcpc-close c :timeout-ms 0))
                  (let ((ch (svref *ns-slot-ch* i)))
                    (loop (unless (%ns-try ch) (return)))   ; a reused slot
                    (setf (svref *ns-slots* i) (vector c nil nil :open nil))
                    (%ns-incf *ns-stats* 10)
                    (let ((th (handler-case (funcall spawn i ch) (error () nil))))
                      (if th
                          (setf (svref (svref *ns-slots* i) 1) th)
                          (setf (svref (svref *ns-slots* i) 3) :closing))))))))))))

(defun %ns-flush (s)
  "Send what slot S has queued, as far as the peer's window lets it."
  (let ((c (svref s 0)))
    (with-net-lock
      (loop
        (let ((q (svref s 2)))
          (when (null q) (return))
          (unless (member (tc-state c) '(:established :close-wait)) (setf (svref s 2) nil) (return))
          (unless (< (logand (- (tc-snd-nxt c) (tc-snd-una c)) #xFFFFFFFF)
                     (min 32768 (max 1460 (tc-peer-window c))))
            (return))
          (let* ((b (car (car q))) (pos (cdr (car q))) (n (min 1460 (- (length b) pos))))
            (%tcp-emit c #x18 b pos n)
            (if (>= (+ pos n) (length b))
                (setf (svref s 2) (cdr q))
                (setf (cdr (car q)) (+ pos n)))))))))

(defun %ns-eval-bytes (s)
  "The reply to evaluating S: what it printed, then `= VALUE'."
  (let* ((out nil)
         (result (handler-case
                     (let ((so (make-string-output-stream)))
                       (let ((r (let ((*standard-output* so)) (eval (read-from-string s)))))
                         (setq out (get-output-stream-string so))
                         r))
                   (error (c) (list 'error c))))
         (rs (handler-case (prin1-to-string result) (error () "#<unprintable>")))
         (text (concatenate 'string (or out "") "= " rs (string #\Newline)))
         (v (make-array (length text) :element-type '(unsigned-byte 8))))
    (dotimes (i (length text) v) (setf (aref v i) (logand (char-code (char text i)) 255)))))

(defun %ns-take-messages ()
  (let ((any nil))
    (loop
      (let ((m (%ns-try *ns-to-service*)))
        (unless m (return any))
        (setq any t)
        (let ((s (svref *ns-slots* (cadr m))))
          (when s
            (case (car m)
              (:send (setf (svref s 2) (append (svref s 2) (list (cons (caddr m) 0)))))
              (:close (setf (svref s 3) :closing))
              (:eval (%ns-incf *ns-stats* 11)
                     (%ns-send *ns-main-jobs* (list (cadr m) (caddr m)))))))))))

;;; The main thread's side: the console's wait (net/bare-threads.lisp %CONSOLE-IDLE) runs one
;;; job at a time, never nested -- an evaluation that itself waits at the console does not
;;; start the next one.
(defvar *ns-main-jobs* nil "(SLOT STRING) evaluations for the main thread, oldest first.")
(defvar *ns-main-busy* nil)

(defun %bt-main-jobs ()
  (when (and *ns-main-jobs* (not *ns-main-busy*) (zerop (percpu-ref 16)))
    (let ((m (%ns-try *ns-main-jobs*)))
      (when m
        (setq *ns-main-busy* t)
        (unwind-protect
             (%ns-send (svref *ns-slot-ch* (car m)) (list :result (%ns-eval-bytes (cadr m))))
          (setq *ns-main-busy* nil))
        t))))

(defun %ns-relays-step ()
  "Accept, carry bytes both ways, retire finished slots.  T when anything moved."
  (when *ns-slots*
    (let ((any (or (%ns-accept) (%ns-take-messages))))
      (dotimes (i (%ns-nslots) any)
        (let ((s (svref *ns-slots* i)))
          (when s
            (let ((c (svref s 0)) (th (svref s 1)) (ch (svref *ns-slot-ch* i)))
              ;; peer -> handler
              (let ((v (with-net-lock
                         (let* ((rx (tc-rx c)) (n (fill-pointer rx)))
                           (when (> n 0)
                             (let ((v (make-array n :element-type '(unsigned-byte 8)))
                                   (was (%rcv-window c)))
                               (replace v rx)
                               (setf (fill-pointer rx) 0)
                               (when (< was (ash *tcp-rcv-buf* -2)) (%tcp-ack c))
                               v))))))
                (when v (setq any t) (%ns-send ch (list :data v))))
              (when (and (tc-rx-eof c) (not (svref s 4)))
                (setf (svref s 4) t) (%ns-send ch (list :close)))
              ;; handler -> peer
              (%ns-flush s)
              ;; a handler that is gone is finished, whether or not it said so
              ;; (its last messages may still be in the channel: take them first, or the
              ;; FIN went out ahead of the session's exit status and channel close)
              (when (and th (not (sb-thread:thread-alive-p th)) (eq (svref s 3) :open))
                (%ns-take-messages)
                (when (eq (svref s 3) :open) (setf (svref s 3) :closing)))
              (when (and (eq (svref s 3) :closing) (null (svref s 2)))
                (with-net-lock
                  (case (tc-state c)
                    (:established (%tcp-emit c #x11) (setf (tc-state c) :fin-wait))
                    (:close-wait (%tcp-emit c #x11) (setf (tc-state c) :last-ack))))
                (setf (svref s 3) :fin))
              (when (and (eq (svref s 3) :fin)
                         (or (eq (tc-state c) :closed) (null (tc-unacked c)))
                         (or (null th) (not (sb-thread:thread-alive-p th))))
                (setf (tc-state c) :closed)
                (when th (sb-thread:join-thread th :default nil))
                (setf (svref *ns-slots* i) nil)
                (setq any t)))))))))

;;; ---- the watchdog ---------------------------------------------------------------------
;;;
;;; *NS-WD*: 0 checks, 1 pauses, 2 longest (ms), 3 last pause (ms), 4 total paused (ms).

(defun %ns-ring-waiting ()
  (if (%ns-engine-p) (logand (- (usbe-get #x1000) (usbe-get #x1004)) #xFFFFFFFF) 0))

(defun %ns-watchdog-main ()
  (let ((tick (%bt-ms-ticks 10)) (ms (max 1 (%bt-ms-ticks 1))) (wd *ns-wd*))
    (loop
      (when *ns-stop* (return :stopped))
      (let ((dl (+ (%bt-ticks) tick)) (gc (%gc-epoch)))
        (%bt-block 0 0 dl)
        (let ((late (floor (- (%bt-ticks) dl) ms)))
          (%ns-incf wd 0)
          (when (> late (svref wd 2)) (setf (svref wd 2) late))
          (when (>= late *ns-pause-ms*)
            (%ns-incf wd 1) (%ns-incf wd 4 late) (setf (svref wd 3) late)
            (kmsg-event 2 "PAUS" late (logand (- (%gc-epoch) gc) #xFFFF) (%ns-ring-waiting)))
          ;; and the service itself: dead without being stopped, it starts again (kmsg NETR)
          (when (and (zerop (mod (svref wd 0) 100)) *ns-thread* (not *ns-stop*)
                     (not (sb-thread:thread-alive-p *ns-thread*)))
            (%ns-incf *ns-stats* 14)
            (kmsg-event 3 "NETR" (svref *ns-stats* 14) (svref *ns-stats* 7) 0)
            (setq *ns-thread* (sb-thread:make-thread #'%ns-main :name "net"))))))))

;;; ---- log shipping, a client like any other --------------------------------------------

(defun %ns-shipper-main ()
  (loop
    (when *ns-stop* (return :stopped))
    (when (fboundp 'kmsg-ship-step) (kmsg-ship-step))
    (%ns-wait 0 0 1000)))

;;; ---- control --------------------------------------------------------------------------

(defun net-service-start (&key (pause-ms *ns-pause-ms*) (ship t))
  "Give the NIC to the network service.  :STARTED or :ALREADY."
  (if *ns-thread*
      :already
      (progn
        (setq *ns-pause-ms* pause-ms *ns-stop* nil
              *ns-receive* (fdefinition 'modus.net:nic-receive)
              *ns-rx-buf* (fdefinition 'modus.net:nic-rx-buf))
        (setf (svref *ns-legacy-idx* 0) 0 (svref *ns-legacy-idx* 1) 0 (svref *ns-staged* 0) 0)
        (fill *ns-stats* 0) (fill *ns-wd* 0)
        (setf (mem-ref (%ns-gen-addr) :u32) 0)
        (setf (fdefinition 'modus.net:nic-receive) #'%ns-nic-receive
              (fdefinition 'modus.net:nic-rx-buf) #'%ns-nic-rx-buf)
        (setq *ns-thread* (sb-thread:make-thread #'%ns-main :name "net")
              *ns-watchdog* (sb-thread:make-thread #'%ns-watchdog-main :name "core-0 watchdog")
              *ns-shipper* (and ship (sb-thread:make-thread #'%ns-shipper-main :name "kmsg shipper")))
        (kmsg-event 1 "NETS" (if (%ns-engine-p) 1 0) pause-ms 0)
        :started)))

(defun net-service-stop ()
  "Hand the NIC back to whoever reads it (the old interface)."
  (when *ns-thread*
    (setq *ns-stop* t)
    (dolist (th (list *ns-thread* *ns-watchdog* *ns-shipper*))
      (when th (sb-thread:join-thread th :default nil)))
    (setf (fdefinition 'modus.net:nic-receive) *ns-receive*
          (fdefinition 'modus.net:nic-rx-buf) *ns-rx-buf*)
    (setq *ns-thread* nil *ns-watchdog* nil *ns-shipper* nil)
    :stopped))

(defun net-service-status ()
  (let ((s *ns-stats*) (wd *ns-wd*))
    (list :running (and *ns-thread* t) :engine (%ns-engine-p)
          :frames (svref s 0) :tcp (svref s 1) :udp (svref s 2) :arp (svref s 3) :icmp (svref s 4)
          :legacy (svref s 5) :legacy-dropped (svref s 6)
          :passes (svref s 7) :waits (svref s 9) :generation (%ns-gen)
          :connections (length *tcp-conns*)
          :accepted (svref s 10) :refused (svref s 12) :evaluations (svref s 11)
          :handlers (if *ns-slots* (count-if #'identity *ns-slots*) 0)
          :pass-errors (svref s 13) :restarts (svref s 14)
          :watchdog (list :checks (svref wd 0) :pauses (svref wd 1) :longest-ms (svref wd 2)
                          :last-ms (svref wd 3) :total-ms (svref wd 4) :threshold-ms *ns-pause-ms*))))
