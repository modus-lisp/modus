;;;; ssh-inetd.lisp - the SSH server as an inetd client of the network service.
;;;;
;;;; The network service (net/net-service.lisp) accepts on port 22 and hands each connection to
;;;; a SESSION thread on cores 2..3 (never core 1: the I/O core runs the network and the USB
;;;; engine and never yields).  The session runs the whole SSH protocol (key exchange, host-key
;;;; signature, ciphers, channels: net/ssh.lisp) in its own core's heap and reads and writes its
;;;; connection directly in the I/O core's static block (net/io-net.lisp ION-READ / ION-WRITE):
;;;; no relay, no channel for the bytes.  Commands are evaluated on the MAIN thread
;;;; (NET-SERVICE-EVAL), in turn with the serial REPL, as the old server did.
;;;;
;;;; This is net/isolated-cl.lisp's session half: the same per-session words in the SSH block --
;;;; +0x3F80/+0x3FA0 the session's own X25519 pair, +0x3FC8 "the peer has gone", +0x3FCC "the
;;;; session is over" -- and +0x3FD0 the service slot it is served on, +0x3FD4 its connection
;;;; handle.  The SSH block is chosen by connection slot (conn-ssh K, K = 0..3), one per session.
;;;;
;;;; Loaded after net/net-service.lisp.   (modus-int::ssh-serve)

(in-package :modus-int)

(defvar *sd-up* nil)
(defvar *sd-conn-slot* (make-array 4 :initial-element -1) "SSH block K -> service slot, or -1.")
(defvar *sd-threads* (make-array 16 :initial-element -1) "Thread record -> SSH block, or -1.")

(defun %sd-slot-addr (ssh) (+ ssh #x3FD0))
(defun %sd-k (ssh) (ash (- ssh #x20 (ssh-conn-base)) -14))
(defun %sd-me () (percpu-ref 16))

;; What every other caller keeps getting: the definitions in force when this file loaded
;; (the board's own server on the main thread, until it is retired).  A caller holding a
;; function object keeps its code, so these stay the old ones.
(defvar *sd-old-tcp-send-conn* (fdefinition 'tcp-send-conn))
(defvar *sd-old-eval-line* (fdefinition 'ssh-eval-line))
(defvar *sd-old-seams* nil)

(defun %sd-session-p ()
  "This thread is one of ours: a session started by %SD-SPAWN."
  (let ((me (%sd-me))) (and (> me 0) (< me 16) (>= (svref *sd-threads* me) 0))))

(defun %sd-bytes (data len)
  (let ((v (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len v) (setf (aref v i) (logand (aref data i) 255)))))

;;; ---- the transport, as ssh.lisp sees it ----------------------------------------------

(defun tcp-send-conn (cb data len)
  (if (not (%sd-session-p))
      (funcall *sd-old-tcp-send-conn* cb data len)
      (%sd-tcp-send-conn cb data len)))

(defun %sd-handle (ssh) (mem-ref (+ ssh #x3FD4) :u32))

(defun %sd-tcp-send-conn (cb data len)
  (when (> len 0) (ion-write (%sd-handle (+ cb #x20)) data :end len))
  0)

(defun %sd-channel (ssh) (net-service-slot-channel (mem-ref (%sd-slot-addr ssh) :u32)))

(defun %sd-old (i) (svref *sd-old-seams* i))

(defvar *sd-rxbufs* (let ((v (make-array 4)))
                      (dotimes (i 4 v) (setf (svref v i) (make-array 4096 :element-type '(unsigned-byte 8))))))

(defun %sd-wait-data (ssh)
  "Bytes from the peer into the SSH receive buffer (4 KB at +0x6D8): 1 to keep waiting, 0 when
   the peer has gone."
  (if (not (%sd-session-p)) (funcall (%sd-old 0) ssh)
  (let* ((blen (mem-ref (+ ssh #x6D4) :u32)) (room (- 4096 blen)))
    (if (<= room 0)
        1
        (let* ((buf (svref *sd-rxbufs* (%sd-k ssh)))
               (n (ion-read (%sd-handle ssh) :max room :timeout-ms 10 :into buf)))
          (cond ((eq n :eof) (setf (mem-ref (+ ssh #x3FC8) :u32) 1) 0)
                ((zerop n) 1)
                (t (dotimes (i n) (setf (mem-ref (+ ssh #x6D8 blen i) :u8) (aref buf i)))
                   (setf (mem-ref (+ ssh #x6D4) :u32) (+ blen n))
                   1)))))))

(defun %sd-peer-closed-p ()
  (if (not (%sd-session-p)) (funcall (%sd-old 4))
  (let ((k (svref *sd-threads* (%sd-me))))
    (and (>= k 0) (= (mem-ref (+ (conn-ssh k) #x3FC8) :u32) 1)))))

;; How long a session waits for its peer, in real time.  ssh.lisp counts polls, and here a
;; poll is a 10 ms channel wait, so its 50-poll wait for the client's version string was
;; 0.5 s: a client further away than that (or QEMU's user network) was dropped right
;; after its KEXINIT.  OpenSSH gives a client 120 s to log in; a session holds one of four
;; SSH blocks, so these are shorter.
(defun %sd-version-wait-s () 30)
(defun %sd-preauth-idle-s () 60)
(defun %sd-clock () (if (%sd-session-p) (%bt-ticks) (funcall (%sd-old 5))))
(defun %sd-waited-s (since) (floor (- (%bt-ticks) since) (%bt-ms-ticks 1000)))
(defun %sd-wait-expired-p (count limit since)
  (if (not (%sd-session-p)) (funcall (%sd-old 6) count limit since)
      (or (modus.ssh::ssh-peer-closed-p) (> (%sd-waited-s since) (%sd-preauth-idle-s)))))
(defun %sd-version-wait-expired-p (count since)
  (if (not (%sd-session-p)) (funcall (%sd-old 7) count since)
      (or (modus.ssh::ssh-peer-closed-p) (> (%sd-waited-s since) (%sd-version-wait-s)))))

(defun %sd-eph-base (ssh) (if (%sd-session-p) (+ ssh #x3F80) (funcall (%sd-old 2) ssh)))
(defun %sd-refresh-ephemeral () (unless (%sd-session-p) (funcall (%sd-old 1))))

;; ssh.lisp's SSH-MESSAGE-LOOP with this session's own "over" word (isolated-cl.lisp's).
(defun %sd-message-loop (ssh)
  (if (not (%sd-session-p)) (funcall (%sd-old 3) ssh)
  (let ((flag-addr (+ ssh #x3FCC)) (idle 0) (since (modus.ssh::ssh-preauth-clock)))
    (setf (mem-ref flag-addr :u32) 1)
    (loop
      (when (zerop (mem-ref flag-addr :u32)) (return ()))
      (when (modus.ssh::ssh-peer-closed-p) (return ()))
      (when (and (zerop (mem-ref (+ ssh #x10) :u32))
                 (modus.ssh::ssh-wait-expired-p idle (modus.ssh::ssh-preauth-idle-limit) since))
        (return ()))
      (let ((pkt (modus.ssh::ssh-receive-packet ssh 50000)))
        (if pkt
            (progn (setq idle 0 since (modus.ssh::ssh-preauth-clock))
                   (modus.ssh::ssh-dispatch-msg ssh (car pkt) (cdr pkt) flag-addr))
            (setq idle (+ idle 1))))))))

;; A command: evaluated on core 0 by the service; data that arrives meanwhile is applied.
(defun ssh-eval-line (ssh cmd cmd-len)
  (if (not (%sd-session-p))
      (funcall *sd-old-eval-line* ssh cmd cmd-len)
      (%sd-eval-line ssh cmd cmd-len)))

(defun %sd-eval-line (ssh cmd cmd-len)
  (let ((s (make-string cmd-len)) (slot (mem-ref (%sd-slot-addr ssh) :u32)))
    (dotimes (i cmd-len) (setf (aref s i) (code-char (aref cmd i))))
    (net-service-eval slot s)
    (let ((reply nil))
      (loop
        (when reply (return))
        (when (= (mem-ref (+ ssh #x3FC8) :u32) 1) (return-from %sd-eval-line nil))
        (let ((m (funcall *ns-chan-recv* (%sd-channel ssh) :timeout 1)))
          (when (and m (eq (car m) :result)) (setq reply (cadr m)))))
      (modus.ssh::ssh-send-string ssh reply (length reply)))))

;;; ---- the interactive shell's line editor ----------------------------------------------
;; Typed bytes collect in the session's own line (one per SSH block, so two shells never share
;; a line).  With a terminal (the client sent pty-req, +0x3FD8 = 1) the client echoes nothing,
;; so the editor does: characters, backspace, ^C (drop the line), ^U (erase it), ^D on an empty
;; line or (quit) / (exit) (end the session), and a line whose parentheses are still open
;; continues on the next one.  Escape sequences (arrow keys) are swallowed.  Without a terminal
;; (`echo FORM | ssh ...`) nothing is echoed, as before.

(defvar *sd-lines* (let ((v (make-array 4)))
                     (dotimes (i 4 v) (setf (svref v i) (make-array 8192 :element-type '(unsigned-byte 8))))))
(defvar *sd-line-st* (make-array 8 :initial-element 0) "Per SSH block K: 2K length, 2K+1 escape state.")

(defun %sd-out (ssh &rest codes)
  (when (= (mem-ref (+ ssh #x3FD8) :u32) 1)
    (let ((v (coerce codes 'simple-vector)))
      (modus.ssh::ssh-send-string ssh v (length v)))))

(defun %sd-line-complete-p (buf n)
  "The line's parentheses are closed (outside strings, comments and #\ characters)."
  (let ((depth 0) (str nil) (esc nil) (comment nil) (i 0))
    (loop
      (when (>= i n) (return (and (<= depth 0) (not str))))
      (let ((b (aref buf i)))
        (cond (comment (when (= b 10) (setq comment nil)))
              (str (cond (esc (setq esc nil)) ((= b 92) (setq esc t)) ((= b 34) (setq str nil))))
              ((= b 34) (setq str t))
              ((= b 59) (setq comment t))
              ((and (= b 35) (< (+ i 1) n) (= (aref buf (+ i 1)) 92)) (setq i (+ i 2)))
              ((= b 40) (setq depth (+ depth 1)))
              ((= b 41) (setq depth (- depth 1)))))
      (setq i (+ i 1)))))

(defun %sd-line-is-p (buf n word)
  "The line, without surrounding blanks, is WORD."
  (let ((a 0) (z n))
    (loop (when (or (>= a z) (> (aref buf a) 32)) (return)) (setq a (+ a 1)))
    (loop (when (or (<= z a) (> (aref buf (- z 1)) 32)) (return)) (setq z (- z 1)))
    (and (= (- z a) (length word))
         (dotimes (i (length word) t)
           (unless (= (aref buf (+ a i)) (char-code (char-downcase (char word i)))) (return nil))))))

(defun %sd-shell-byte (ssh k buf b)
  (let ((n (svref *sd-line-st* (* 2 k))) (esc (svref *sd-line-st* (+ (* 2 k) 1))))
    (flet ((len (x) (setf (svref *sd-line-st* (* 2 k)) x)))
      (cond ((= esc 1) (setf (svref *sd-line-st* (+ (* 2 k) 1)) (if (or (= b 91) (= b 79)) 2 0)))
            ((= esc 2) (when (and (>= b 64) (<= b 126)) (setf (svref *sd-line-st* (+ (* 2 k) 1)) 0)))
            ((= b 27) (setf (svref *sd-line-st* (+ (* 2 k) 1)) 1))
            ((= b 3) (len 0) (%sd-out ssh 94 67 10) (modus.ssh::ssh-send-prompt ssh))
            ((= b 21) (len 0) (%sd-out ssh 94 85 10) (modus.ssh::ssh-send-prompt ssh))
            ((= b 4) (when (zerop n) (modus.ssh::ssh-handle-ctrl-d ssh)))
            ((or (= b 127) (= b 8))
             (when (and (> n 0) (/= (aref buf (- n 1)) 10)) (len (- n 1)) (%sd-out ssh 8 32 8)))
            ((or (= b 13) (= b 10))
             (%sd-out ssh 10)
             (cond ((zerop n) (when (or (= b 13) (/= (mem-ref (+ ssh #x3FD8) :u32) 1))
                                (modus.ssh::ssh-send-prompt ssh)))
                   ((or (%sd-line-is-p buf n "(quit)") (%sd-line-is-p buf n "(exit)"))
                    (len 0) (modus.ssh::ssh-handle-ctrl-d ssh))
                   ((%sd-line-complete-p buf n)
                    (len 0) (ssh-eval-line ssh buf n) (modus.ssh::ssh-send-prompt ssh))
                   ((< n 8191) (setf (aref buf n) 10) (len (+ n 1)))))
            ((or (>= b 32) (= b 9))
             (when (< n 8191) (setf (aref buf n) b) (len (+ n 1)) (%sd-out ssh b)))))))

(defun %sd-channel-data (ssh payload plen)
  (if (not (%sd-session-p)) (funcall (%sd-old 8) ssh payload plen)
      (let ((k (%sd-k ssh)) (len (modus.ssh::ssh-get-u32 payload 5)))
        (dotimes (i len) (%sd-shell-byte ssh k (svref *sd-lines* k) (aref payload (+ 9 i)))))))

;;; ---- a session ----------------------------------------------------------------------

(defun %sd-session (k)
  (setf (svref *sd-threads* (%sd-me)) k)              ; a fixnum: no pointer
  (kmsg-event 1 "SSHS" k (%bt-core-id) (%sd-me))
  (let ((ssh (conn-ssh k)))
    (handler-case
        (progn
          (let ((priv (make-array 32)))
            (dotimes (i 32) (setf (aref priv i) (mem-ref (+ ssh #x3F80 i) :u8)))
            (let ((pub (modus.crypto::x25519-public-key priv)))
              (dotimes (i 32) (setf (mem-ref (+ ssh #x3FA0 i) :u8) (aref pub i)))))
          (modus.ssh::ssh-handle-connection ssh))
      (error (c) (handler-case (kmsg 3 "SSHE" (princ-to-string c)) (error () nil))))
    ;; Let the client hang up first: it answers our CHANNEL_CLOSE with its own and a
    ;; DISCONNECT, and a FIN that overtakes them reads as "closed by remote host" to it.
    (let ((h (%sd-handle ssh)) (end (+ (%bt-ticks) (%bt-ms-ticks 2000))))
      (loop (when (or (eq (ion-read h :max 4096 :timeout-ms 100 :into (svref *sd-rxbufs* k)) :eof)
                      (> (%bt-ticks) end))
              (return)))
      (ion-close h))
    (kmsg-event 1 "SSHX" k (mem-ref (%sd-slot-addr ssh) :u32) 0)
    (net-service-slot-done (mem-ref (%sd-slot-addr ssh) :u32))
    (setf (svref *sd-conn-slot* k) -1)                 ; the block is free again
    (setf (svref *sd-threads* (%sd-me)) -1)
    :done))

(defun %sd-spawn (h slot)
  "The service's SPAWN for port 22, for connection H: an SSH block, then a session on cores
   2..3.  NIL when all four blocks are busy."
  (let ((k (dotimes (i 4 nil) (when (< (svref *sd-conn-slot* i) 0) (return i)))))
    (unless k (kmsg-event 2 "SSHR" slot 0 0))
    (when k
      (setf (svref *sd-conn-slot* k) slot)
      (let ((ssh (conn-ssh k)))
        (ssh-copy-host-key k)
        (dolist (off '(#x00 #x04 #x08 #x0C #x10 #x28 #x150 #x154 #x6D4 #x3FC8 #x3FCC #x3FD8))
          (setf (mem-ref (+ ssh off) :u32) 0))
        (setf (svref *sd-line-st* (* 2 k)) 0 (svref *sd-line-st* (+ (* 2 k) 1)) 0)
        (setf (mem-ref (+ ssh #x2C) :u32) (logand (+ (* slot 7919) (rdtsc)) #xFFFFFFFF))
        (setf (mem-ref (%sd-slot-addr ssh) :u32) slot)
        (setf (mem-ref (+ ssh #x3FD4) :u32) h)
        ;; the session's ephemeral private key: drawn here, from the server's DRBG
        (dotimes (i 32) (setf (mem-ref (+ ssh #x3F80 i) :u8) (modus.ssh::ssh-random ssh))))
      ;; Not core 1: the I/O core never gives the core up, so a session started on it never
      ;; ran (2 of 4 concurrent sessions, on the Zero).
      (bt-pin-next (+ 2 (mod slot 2)))
      (let ((kk k))
        (sb-thread:make-thread (lambda () (%sd-session kk)) :name "ssh session")))))

(defun ssh-serve (&key (port 22))
  "Serve SSH on PORT through the network service (started if it is not running)."
  (net-service-start)
  (unless *sd-up*
    (fill *sd-conn-slot* -1) (fill *sd-threads* -1)
    (setq *sd-old-seams* (vector (fdefinition 'modus.ssh::ssh-wait-data)
                                 (fdefinition 'modus.ssh::ssh-refresh-ephemeral)
                                 (fdefinition 'modus.ssh::ssh-eph-base)
                                 (fdefinition 'modus.ssh::ssh-message-loop)
                                 (fdefinition 'modus.ssh::ssh-peer-closed-p)
                                 (fdefinition 'modus.ssh::ssh-preauth-clock)
                                 (fdefinition 'modus.ssh::ssh-wait-expired-p)
                                 (fdefinition 'modus.ssh::ssh-version-wait-expired-p)
                                 (fdefinition 'ssh-handle-channel-data)))
    (setf (fdefinition 'modus.ssh::ssh-wait-data) #'%sd-wait-data
          (fdefinition 'modus.ssh::ssh-refresh-ephemeral) #'%sd-refresh-ephemeral
          (fdefinition 'modus.ssh::ssh-eph-base) #'%sd-eph-base
          (fdefinition 'modus.ssh::ssh-message-loop) #'%sd-message-loop
          (fdefinition 'modus.ssh::ssh-peer-closed-p) #'%sd-peer-closed-p
          (fdefinition 'modus.ssh::ssh-preauth-clock) #'%sd-clock
          (fdefinition 'modus.ssh::ssh-wait-expired-p) #'%sd-wait-expired-p
          (fdefinition 'modus.ssh::ssh-version-wait-expired-p) #'%sd-version-wait-expired-p
          (fdefinition 'ssh-handle-channel-data) #'%sd-channel-data)
    (setq *sd-up* t))
  (net-service-listen port #'%sd-spawn)
  (kmsg-event 1 "SSHD" port 0 0)
  port)
