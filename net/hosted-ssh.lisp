;;;; net/hosted-ssh.lisp -- the SSH-2 server (net/ssh.lisp) over a LINUX STREAM,
;;;; hosted x86-64: a loopback/any-address TCP listener for a plain `./modus',
;;;; and a vsock listener for a Nitro enclave, whose only network is vsock to
;;;; its parent.  Baked after crypto.lisp, crypto-fast.lisp and ssh.lisp, which
;;;; this file gives their arch adapter (the address map, the two transport
;;;; touch points, entropy, the shell) -- last-defun-wins, so every override
;;;; here replaces a bare-metal definition of the same name.
;;;;
;;;; The server's state is RAW MEMORY at addresses the arch adapter hands out
;;;; (ssh.lisp's header: per-connection block at conn-base+0x20, host key and
;;;; precomputed signing material in e1000-state, line buffers in ssh-ipc).  A
;;;; hosted process owns no fixed RAM, so all of it lives in one 2 MB anonymous
;;;; mapping taken on first use:
;;;;     +0x000000  4 x 16 KB connection slots            (ssh-conn-base)
;;;;     +0x010000  the SSH-side "e1000 state": +0x624 key-set, +0x62C PRNG,
;;;;                +0x680.. precomputed Ed25519 s/prefix, +0x6C4/+0x6E4 the
;;;;                server X25519 ephemeral, +0x710/+0x730 host private/public
;;;;                key, +0x1000 the version string         (e1000-state-base)
;;;;     +0x012000  fe-scratch
;;;;     +0x020000  ssh-ipc: 76800 words; +0x60438 port, +0x60500/+0x60510 the
;;;;                interactive line buffer                  (ssh-ipc-base)
;;;;     +0x1F0000  entropy scratch for getrandom(2)
;;;; Only the two transport functions know there is a socket: TCP-SEND-CONN
;;;; writes to the connection's fd, SSH-WAIT-DATA read(2)s straight into the
;;;; recv buffer at ssh+0x6D8 and bumps ssh+0x6D4.  The fd sits at the top of
;;;; the 16 KB slot (cb+0x3FF0), above everything ssh.lisp uses (<= cb+0x2100).
;;;;
;;;; KEYS.  The bare images install an all-zero host key (SSH-USE-DEFAULT-KEY)
;;;; and draw the X25519 ephemeral and packet padding from a 32-bit xorshift
;;;; seeded from a timer; for an ATTESTED server both are unacceptable -- a
;;;; known host key lets a man in the middle complete the handshake, and a
;;;; 32-bit ephemeral is brute-forceable.  Here the host key is 32 bytes of
;;;; getrandom(2) per process and SSH-RANDOM is getrandom(2) too (buffered).
;;;; The attestation then binds user_data = SHA-512(host public key), the same
;;;; binding the SNP side uses (net/snp-ssh-attest.lisp), so a client that has
;;;; completed a handshake can check the enclave vouched for THAT key.

(defvar *hssh-base* 0)
(defconstant +hssh-bytes+ #x200000)
(defun %hssh-base ()
  (when (zerop *hssh-base*)
    (setq *hssh-base* (%mmap-shared-page +hssh-bytes+)))
  *hssh-base*)

;;; ---- the address map ssh.lisp asks its arch adapter for ----
(defun ssh-conn-base ()    (%hssh-base))
(defun e1000-state-base () (+ (%hssh-base) #x10000))
(defun fe-scratch-base ()  (+ (%hssh-base) #x12000))
(defun ssh-ipc-base ()     (+ (%hssh-base) #x20000))
(defun conn-base (conn)    (+ (ssh-conn-base) (ash conn 14)))
(defun conn-ssh (conn)     (+ (conn-base conn) #x20))

;;; ---- bare-metal names ssh.lisp references that mean nothing here ----
;;; (ssh-server, the net-actor entry, is never called hosted; the line editor
;;; and the legacy evaluator are replaced by the shell at the bottom.)
(defun %serial-byte (b) (write-char-serial b))
(defun hash-of (name) 0)
(defun nfn-lookup (hash) 0)
(defun init-gc-helper () nil)
(defun net-init-dhcp () 0)
(defun edit-line-len () 0)
(defun edit-set-line-len (n) 0)
(defun edit-set-cursor-pos (n) 0)
(defun handle-edit-byte (ssh b) 0)
(defun native-eval (form) nil)
(defun print-obj (x) nil)
(defun rt-compile-defun (form) nil)
(defun edit-cursor-pos () 0)
(defun emit-prompt () 0)
(defun read-list () nil)
(defun eval-line-expr () nil)
;; The handshake calls this between its expensive steps to keep a USB NIC's host
;; from declaring the link dead (Pi Zero 2 W); here there is nothing to keep alive.
(defun usb-keepalive () 0)

;;; ---- byte-buffer helpers crypto.lisp takes from net/ip.lisp (not baked
;;; hosted: it is the IP stack), and the hex printer its self-test uses ----
(defun buf-read-u32 (buf off)
  (logior (ash (aref buf off) 24) (ash (aref buf (+ off 1)) 16)
          (ash (aref buf (+ off 2)) 8) (aref buf (+ off 3))))
(defun buf-write-u32 (buf off val)
  (aset buf off (logand (ash val -24) #xFF))
  (aset buf (+ off 1) (logand (ash val -16) #xFF))
  (aset buf (+ off 2) (logand (ash val -8) #xFF))
  (aset buf (+ off 3) (logand val #xFF)))
(defun print-hex-byte (b) (format t "~(~2,'0x~)" b))

;;; ---- entropy: getrandom(2) = 318 ----
(defun %hssh-getrandom (addr n) (syscall3 318 addr n 0))
(defun %hssh-rand-page () (+ (%hssh-base) #x1F0000))
(defun %hssh-random-bytes (n)
  "N fresh bytes from the kernel as a generic array (what ssh.lisp's ASET code wants)."
  (let ((pg (%hssh-rand-page)) (v (make-array n)))
    (%hssh-getrandom pg n)
    (dotimes (i n v) (aset v i (mem-ref (+ pg i) :u8)))))
(defun arch-seed-random ()
  (let ((v (%hssh-random-bytes 4)))
    (logior (ash (aref v 0) 24) (ash (aref v 1) 16) (ash (aref v 2) 8) (aref v 3))))
;;; SSH-RANDOM (one byte; the ephemeral key, padding, nonces) -- a 256-byte
;;; getrandom buffer at rand-page+0x100, index at rand-page+0x200.
(defun ssh-random (ssh)
  (let* ((pg (%hssh-rand-page)) (buf (+ pg #x100)) (ix (mem-ref (+ pg #x200) :u32)))
    (when (or (zerop ix) (>= ix 256))
      (%hssh-getrandom buf 256)
      (setq ix 0))
    (setf (mem-ref (+ pg #x200) :u32) (+ ix 1))
    (mem-ref (+ buf ix) :u8)))

;;; ---- the two transport touch points ----
(defun %hssh-fd (cb) (mem-ref (+ cb #x3FF0) :u32))
(defun %hssh-set-fd (cb fd) (setf (mem-ref (+ cb #x3FF0) :u32) fd))
(defun %hssh-read (fd addr len) (syscall3 0 fd addr len))
(defun %hssh-shutdown-wr (fd) (syscall3 48 fd 1 0))          ; shutdown(fd, SHUT_WR)
(defun tcp-send-conn (cb data len)
  (when (> len 0) (socket-send-from (%hssh-fd cb) data 0 len))
  0)
(defun ssh-wait-data (ssh)
  "Append whatever read(2) gives to this connection's recv buffer; 0 = gone."
  (let* ((cb (- ssh #x20))
         (blen (mem-ref (+ ssh #x6D4) :u32))
         (room (- 4096 blen)))
    (if (<= room 0)
        1
        (let ((n (%hssh-read (%hssh-fd cb) (+ ssh #x6D8 blen) room)))
          (if (< n 1)
              0
              (progn (setf (mem-ref (+ ssh #x6D4) :u32) (+ blen n)) 1))))))

;;; ---- host-key material (from net/ip.lisp and net/aarch64-overrides.lisp,
;;; which are not baked hosted: they are the IP stack and the NIC poll loop) ----
(defun ssh-copy-host-key (conn)
  (let ((state (e1000-state-base)) (ssh (conn-ssh conn)))
    (dotimes (i 32)
      (setf (mem-ref (+ ssh #x110 i) :u8) (mem-ref (+ state #x710 i) :u8))
      (setf (mem-ref (+ ssh #x130 i) :u8) (mem-ref (+ state #x730 i) :u8)))))
(defun pre-compute-host-sign ()
  (let ((state (e1000-state-base)))
    (sha512-init)
    (let ((privkey (make-array 32)))
      (dotimes (i 32) (aset privkey i (mem-ref (+ state (+ #x710 i)) :u8)))
      (let ((hash (sha512 privkey)))
        (dotimes (i 32) (setf (mem-ref (+ state (+ #x680 i)) :u8) (aref hash i)))
        (setf (mem-ref (+ state #x680) :u8) (logand (mem-ref (+ state #x680) :u8) #xF8))
        (let ((b31 (mem-ref (+ state #x69F) :u8)))
          (setf (mem-ref (+ state #x69F) :u8) (logior (logand b31 #x7F) #x40)))
        (dotimes (i 32) (setf (mem-ref (+ state (+ #x6A0 i)) :u8) (aref hash (+ i 32))))
        (setf (mem-ref (+ state #x6C0) :u32) 1)))))
(defun pre-compute-server-eph (ssh)
  "A FRESH X25519 ephemeral for this connection at state+0x6C4 / +0x6E4 (ssh-handle-kex reads it there)."
  (let ((state (e1000-state-base)) (priv (make-array 32)))
    (dotimes (i 32) (aset priv i (ssh-random ssh)))
    (dotimes (i 32) (setf (mem-ref (+ state (+ #x6C4 i)) :u8) (aref priv i)))
    (let ((pub (x25519-public-key priv)))
      (dotimes (i 32) (setf (mem-ref (+ state (+ #x6E4 i)) :u8) (aref pub i))))))
(defun ed25519-sign-fast (message msg-len)
  (let ((state (e1000-state-base)) (s (make-array 32)) (prefix (make-array 32)) (a-enc (make-array 32)))
    (dotimes (i 32)
      (aset s i (mem-ref (+ state (+ #x680 i)) :u8))
      (aset prefix i (mem-ref (+ state (+ #x6A0 i)) :u8))
      (aset a-enc i (mem-ref (+ state (+ #x730 i)) :u8)))
    (let* ((r (ed-reduce-scalar (sha512 (concat-bytes prefix 32 message msg-len))))
           (r-enc (ed-encode-point (ed-base-mult r)))
           (k (ed-reduce-scalar (sha512 (concat3-bytes r-enc 32 a-enc 32 message msg-len))))
           (sig-s (ed-scalar-add r (ed-scalar-mult-mod-l k s))))
      (concat-bytes r-enc 32 sig-s 32))))

;;; ---- the shell: what a client's exec line or typed line does.  Same as the
;;; bare x64 CL image (mvm/build-cl-repl-common.lisp): read -> eval -> prin1,
;;; with *standard-output* captured so printed lines precede the `= VALUE'. ----
(defun ssh-eval-line (ssh cmd cmd-len)
  (let ((s (make-string cmd-len)) (out nil))
    (dotimes (i cmd-len) (aset s i (code-char (aref cmd i))))
    (let ((result (handler-case
                      (let ((so (make-string-output-stream)))
                        (let ((r (let ((*standard-output* so)) (eval (read-from-string s)))))
                          (setq out (get-output-stream-string so))
                          r))
                    (t (c) (list 'error c)))))
      (let ((rs (handler-case (prin1-to-string result)
                  (t (c) (prin1-to-string 'unprintable)))))
        (let ((rl (length rs)) (ol (if out (length out) 0)))
          (let ((arr (make-array (+ ol rl 3))))
            (dotimes (i ol) (aset arr i (char-code (aref out i))))
            (aset arr ol 61) (aset arr (+ ol 1) 32)
            (dotimes (i rl) (aset arr (+ ol 2 i) (char-code (aref rs i))))
            (aset arr (+ ol 2 rl) 10)
            (ssh-send-string ssh arr (+ ol rl 3))))))))
(defun ssh-do-eval-expr (ssh) nil)
(defun ssh-handle-channel-data (ssh payload plen)
  (let ((data-len (ssh-get-u32 payload 5))
        (naddr (+ (ssh-ipc-base) #x60500))
        (baddr (+ (ssh-ipc-base) #x60510)))
    (let ((i 0))
      (loop
        (when (>= i data-len) (return nil))
        (let ((b (aref payload (+ 9 i))))
          (if (or (eq b 10) (eq b 13))
              (let ((n (mem-ref naddr :u32)))
                (when (> n 0)
                  (let ((cmd (make-array n)))
                    (dotimes (k n) (aset cmd k (mem-ref (+ baddr k) :u8)))
                    (ssh-eval-line ssh cmd n)))
                (setf (mem-ref naddr :u32) 0)
                (ssh-send-prompt ssh))
              (let ((n (mem-ref naddr :u32)))
                (when (< n 4000)
                  (setf (mem-ref (+ baddr n) :u8) b)
                  (setf (mem-ref naddr :u32) (+ n 1))))))
        (setq i (+ i 1))))))

;;; ---- bring-up and the accept loop ----
(defun ssh-boot-hosted ()
  "Fresh host key and signing material for this process.  Idempotent per process."
  (%hssh-base)
  (when (zerop (mem-ref (+ (e1000-state-base) #x624) :u32))
    (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)
    (ssh-seed-random)
    (ssh-init-strings)
    (ssh-set-host-key (%hssh-random-bytes 32))
    (pre-compute-host-sign))
  0)
(defun %hssh-serve-one (fd)
  "Serve one connection on slot 0, inline, start to finish; close FD."
  (let* ((cb (conn-base 0)) (ssh (+ cb #x20)))
    (dotimes (i 2048) (setf (mem-ref (+ cb (* i 8)) :u64) 0))
    (%hssh-set-fd cb fd)
    (setf (mem-ref cb :u32) 2)                                ; established
    (ssh-copy-host-key 0)
    (setf (mem-ref (+ ssh #x2C) :u32) (arch-seed-random))
    (setf (mem-ref (+ (ssh-ipc-base) #x60500) :u32) 0)
    (setf (mem-ref (+ (ssh-ipc-base) #x60448) :u32) 0)
    (pre-compute-server-eph ssh)
    (handler-case (ssh-handle-connection ssh)
      (error (c) (format t "SSH: connection ended: ~A~%" c)))
    ;; LINGER BEFORE CLOSE.  After an exec the server has just queued the
    ;; reply + CHANNEL_EOF + CHANNEL_CLOSE and the client's own CLOSE/DISCONNECT
    ;; is still unread on our side; close(2) on a socket with unread input makes
    ;; Linux send RST, and an RST discards whatever the client had not yet
    ;; received -- the reply.  (The bare-metal TCP stack never RSTs here, which
    ;; is why the exec path only broke hosted.)  So: shutdown(SHUT_WR), then
    ;; read until the client finishes, bounded, then close.
    (%hssh-shutdown-wr fd)
    (let ((pg (%hssh-rand-page)) (n 0))
      (loop
        (when (or (>= n 64) (< (%hssh-read fd (+ pg #x400) 1024) 1)) (return))
        (setq n (+ n 1))))
    (socket-close fd)
    0))
(defun ssh-serve-fd (lfd)
  "Accept forever on listening fd LFD, one connection at a time."
  (loop
    (let ((fd (socket-accept lfd)))
      (if (< fd 0) (return -1) (%hssh-serve-one fd)))))
(defun %hssh-announce (lfd what)
  (format t "SSH: listening ~A:~D~%" what (socket-local-port lfd))
  (finish-output))
(defun ssh-serve-tcp (port)
  "127.0.0.1:PORT (0 = kernel-chosen, printed).  Never returns while clients come."
  (ssh-boot-hosted)
  (let ((l (socket-listen port 4)))
    (if (< l 0) -1 (progn (%hssh-announce l "127.0.0.1") (ssh-serve-fd l)))))
(defun ssh-serve-tcp-on (ip port)
  "IP as a 32-bit integer; 0 = every interface.  Serving the network is a different name on purpose."
  (ssh-boot-hosted)
  (let ((l (socket-listen-on ip port 4)))
    (if (< l 0) -1 (progn (%hssh-announce l ip) (ssh-serve-fd l)))))
(defun ssh-serve-vsock (port)
  "An enclave's SSH: vsock PORT, reached from the parent as CID <enclave>:PORT."
  (ssh-boot-hosted)
  (let ((l (vsock-listen port 4)))
    (if (< l 0) -1 (progn (format t "SSH: listening vsock:~D~%" port) (finish-output) (ssh-serve-fd l)))))

;;; ---- after a SAVE-AND-DIE restore (lib/save-image.lisp): the per-process
;;; mappings this file and net/nsm-attest.lisp cache in globals belong to the
;;; process that saved; forget them so the next use maps afresh.  A saving run
;;; should not have served SSH anyway (its host key would be baked into the
;;; core, i.e. the same key in every enclave started from it); the guard makes
;;; the state honest even if it did.  Signal handlers are per process too.
(defun %core-post-restore ()
  (%init-signal-handling)
  (setq *hssh-base* 0)
  (setq *nsm-page* 0)
  (when (> (mem-ref (+ (%hssh-base) #x10000 #x624) :u32) 0)   ; a host key WAS baked: drop it
    (setf (mem-ref (+ (%hssh-base) #x10000 #x624) :u32) 0))
  0)

;;; ---- the Nitro binding: attest THIS host key over the session ----
(defun nitro-ssh-host-pubkey ()
  "The 32-byte Ed25519 host public key as a u8 vector, or NIL before SSH-BOOT-HOSTED."
  (when (= (mem-ref (+ (e1000-state-base) #x624) :u32) 1)
    (let ((k (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (i 32 k) (setf (aref k i) (mem-ref (+ (e1000-state-base) #x730 i) :u8))))))
(defun nitro-ssh-user-data ()
  "SHA-512 of the raw host public key, 64 bytes -- the attestation's user_data."
  (let ((k (nitro-ssh-host-pubkey)))
    (when k
      (let ((h (sha512 k)) (v (make-array 64 :element-type '(unsigned-byte 8))))
        (dotimes (i 64 v) (setf (aref v i) (aref h i)))))))
(defun %nitro-hex-lines (tag v)
  (let ((n (length v)) (i 0))
    (loop
      (when (>= i n) (return))
      (format t "~A " tag)
      (dotimes (j (min 64 (- n i))) (format t "~(~2,'0x~)" (aref v (+ i j))))
      (terpri)
      (setq i (+ i 64)))))
(defun nitro-attest-ssh (&optional nonce-hex)
  "Print NITRO-HOSTKEY, NITRO-USER-DATA and the attestation document binding them
   (NITRO-DOC, 64 bytes per line) or NITRO-STATUS when there is no NSM.  NONCE-HEX
   is the verifier's nonce.  Returns :DOCUMENT or the status."
  (let ((k (nitro-ssh-host-pubkey)))
    (cond
      ((null k) (format t "NITRO-STATUS (:NO-HOST-KEY)~%") :no-host-key)
      (t
       (let ((ud (nitro-ssh-user-data))
             (nonce (if nonce-hex (cbor-from-hex nonce-hex) nil)))
         (%nitro-hex-lines "NITRO-HOSTKEY" k)
         (%nitro-hex-lines "NITRO-USER-DATA" ud)
         (let ((d (nsm-attestation-document ud nonce)))
           (cond (d (%nitro-hex-lines "NITRO-DOC" d) :document)
                 (t (format t "NITRO-STATUS ~A~%" *nsm-last-status*) *nsm-last-status*))))))))
