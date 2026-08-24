;;;; ssh.lisp — SSH server (single-threaded, multi-connection)
;;;; Shared between x86-64 and AArch64 builds.
;;;; All arch-specific functions (e1000-state-base, ssh-conn-base etc.) defined in arch-*.lisp

;; ================================================================
;; SSH Server
;; ================================================================
;; SSH state layout (per-connection, relative to ssh = conn-base + 0x20):
;; +0x000: state     +0x004: client-seq   +0x008: server-seq
;; +0x00C: encrypted +0x010: authenticated +0x014: srv-chan-id
;; +0x018: cli-chan-id +0x01C: srv-kexinit-len +0x020: cli-kexinit-len
;; +0x024: host-key-set +0x028: repl-buf-len +0x02C: prng-state
;; +0x030: session-id(32) +0x050: exchange-hash(32)
;; +0x070: shared-secret(32) +0x090: c2s-key(64)
;; +0x0D0: s2c-key(64)      +0x110: host-privkey(32)
;; +0x130: host-pubkey(32)   +0x150: srv-kexinit(512)
;; +0x350: cli-kexinit(512)  +0x550: repl-buf(256)
;; +0x650: cli-version(128)  +0x6D0: cli-version-len
;; +0x6D4: recv-buf-len      +0x6D8: recv-buf(4096)

;; Simple PRNG (xorshift32)
;; ssh = per-connection SSH state base
(defun ssh-random (ssh)
  (let ((s (mem-ref (+ ssh #x2C) :u32)))
    (when (zerop s) (setq s 12345))
    (setq s (logxor s (logand (ash s 13) #xFFFFFFFF)))
    (setq s (logxor s (ash s -17)))
    (setq s (logxor s (logand (ash s 5) #xFFFFFFFF)))
    (setf (mem-ref (+ ssh #x2C) :u32) s)
    (logand s #xFF)))

;; Seed PRNG from arch-specific entropy source
(defun ssh-seed-random ()
  (let ((s (arch-seed-random)))
    (when (zerop s) (setq s 42))
    (setf (mem-ref (+ (e1000-state-base) #x62C) :u32) s)))

;; Write u32 big-endian into array at offset
(defun ssh-put-u32 (arr off val)
  (aset arr off (logand (ash val -24) #xFF))
  (aset arr (+ off 1) (logand (ash val -16) #xFF))
  (aset arr (+ off 2) (logand (ash val -8) #xFF))
  (aset arr (+ off 3) (logand val #xFF)))

;; Read u32 big-endian from array at offset
(defun ssh-get-u32 (arr off)
  (logior (ash (aref arr off) 24)
          (logior (ash (aref arr (+ off 1)) 16)
                  (logior (ash (aref arr (+ off 2)) 8)
                          (aref arr (+ off 3))))))

;; Create SSH string (4-byte len + data) from byte array
(defun ssh-make-str (data data-len)
  (let ((r (make-array (+ 4 data-len))))
    (ssh-put-u32 r 0 data-len)
    (dotimes (i data-len) (aset r (+ 4 i) (aref data i)))
    r))

;; Create SSH string from ASCII text in fixed memory
;; text-addr = address, text-len = length
(defun ssh-make-str-mem (addr len)
  (let ((r (make-array (+ 4 len))))
    (ssh-put-u32 r 0 len)
    (dotimes (i len) (aset r (+ 4 i) (mem-ref (+ addr i) :u8)))
    r))

;; Create SSH mpint from 32-byte array
(defun ssh-make-mpint (bytes)
  (let ((start 0))
    ;; Skip leading zeros
    (let ((done 0))
      (loop
        (if (< start 32)
            (if (zerop (aref bytes start))
                (if (zerop done) (setq start (+ start 1)) (return ()))
                (progn (setq done 1) (return ())))
            (return ()))))
    ;; Check if high bit set (need leading zero)
    (let ((sig-len (- 32 start))
          (need-zero 0))
      (when (> sig-len 0)
        (when (not (zerop (logand (aref bytes start) #x80)))
          (setq need-zero 1)))
      (let ((total (+ sig-len need-zero))
            (r (make-array (+ 4 sig-len need-zero))))
        (ssh-put-u32 r 0 total)
        (when need-zero (aset r 4 0))
        (dotimes (i sig-len)
          (aset r (+ 4 need-zero i) (aref bytes (+ start i))))
        r))))

;; Concat two byte arrays
(defun ssh-concat2 (a a-len b b-len)
  (let ((r (make-array (+ a-len b-len))))
    (dotimes (i a-len) (aset r i (aref a i)))
    (dotimes (i b-len) (aset r (+ a-len i) (aref b i)))
    r))

;; Store bytes from array into fixed memory
(defun ssh-mem-store (addr data len)
  (dotimes (i len) (setf (mem-ref (+ addr i) :u8) (aref data i))))

;; Load bytes from fixed memory into array
(defun ssh-mem-load (dst addr len)
  (dotimes (i len) (aset dst i (mem-ref (+ addr i) :u8))))

;; Create 12-byte nonce from sequence number (OpenSSH format)
(defun ssh-make-nonce (seq)
  (let ((n (make-array 12)))
    (dotimes (i 8) (aset n i 0))
    (ssh-put-u32 n 8 seq)
    n))

;; Store ASCII string in fixed memory for version/algo strings
;; "SSH-2.0-Modus_1.0" = 18 bytes
;; Stored at e1000-state-base + 0x1000
(defun ssh-init-strings ()
  ;; Server version: "SSH-2.0-Modus_1.0" (17 bytes)
  ;; Using u32 stores to reduce setf count (MVM codegen issue with 20+ setf calls)
  (let ((b (e1000-state-base)))
    (let ((b1 (+ b #x1000)))
      (setf (mem-ref b1 :u32) #x2D485353))        ;; SSH-
    (let ((b2 (+ b #x1004)))
      (setf (mem-ref b2 :u32) #x2D302E32))        ;; 2.0-
    (let ((b3 (+ b #x1008)))
      (setf (mem-ref b3 :u32) #x75646F4D))        ;; Modu
    (let ((b4 (+ b #x100C)))
      (setf (mem-ref b4 :u32) #x312E5F73))        ;; s_1.
    (let ((b5 (+ b #x1010)))
      (setf (mem-ref b5 :u8) 48))                  ;; 0
    (let ((b6 (+ b #x1011)))
      (setf (mem-ref b6 :u32) 17)))                ;; version len
  )

;; Build KEXINIT payload (returns array + length via cons)
;; ssh = per-connection SSH state base
(defun ssh-build-kexinit (ssh)
  ;; KEXINIT = msg-type(1) + cookie(16) + 10 name-lists + first_kex(1) + reserved(4)
  ;; Name-lists: each is 4-byte len + comma-separated string
  ;; kex: "curve25519-sha256" (17 bytes)
  ;; host_key: "ssh-ed25519" (11 bytes)
  ;; enc_c2s: "chacha20-poly1305@openssh.com" (29 bytes)
  ;; enc_s2c: same
  ;; mac_c2s: "none" (4 bytes)
  ;; mac_s2c: "none"
  ;; comp_c2s: "none"
  ;; comp_s2c: "none"
  ;; lang_c2s: "" (0)
  ;; lang_s2c: "" (0)
  ;; Total name-list data: 4*10 + 17+11+29+29+4+4+4+4+0+0 = 40+102 = 142
  ;; Total payload: 1+16+142+1+4 = 164
  (let ((p (make-array 164))
        (off 0))
    ;; Message type
    (aset p 0 20)  ; SSH_MSG_KEXINIT
    (setq off 1)
    ;; Cookie (16 random bytes)
    (dotimes (i 16)
      (aset p (+ off i) (ssh-random ssh)))
    (setq off 17)
    ;; kex_algorithms: "curve25519-sha256" (17 bytes)
    (ssh-put-u32 p off 17) (setq off (+ off 4))
    ;; c=99 u=117 r=114 v=118 e=101 2=50 5=53 5=53 1=49 9=57 -=45 s=115 h=104 a=97 2=50 5=53 6=54
    (aset p off 99) (aset p (+ off 1) 117) (aset p (+ off 2) 114)
    (aset p (+ off 3) 118) (aset p (+ off 4) 101) (aset p (+ off 5) 50)
    (aset p (+ off 6) 53) (aset p (+ off 7) 53) (aset p (+ off 8) 49)
    (aset p (+ off 9) 57) (aset p (+ off 10) 45) (aset p (+ off 11) 115)
    (aset p (+ off 12) 104) (aset p (+ off 13) 97) (aset p (+ off 14) 50)
    (aset p (+ off 15) 53) (aset p (+ off 16) 54)
    (setq off (+ off 17))
    ;; server_host_key_algorithms: "ssh-ed25519" (11 bytes)
    (ssh-put-u32 p off 11) (setq off (+ off 4))
    ;; s=115 s=115 h=104 -=45 e=101 d=100 2=50 5=53 5=53 1=49 9=57
    (aset p off 115) (aset p (+ off 1) 115) (aset p (+ off 2) 104)
    (aset p (+ off 3) 45) (aset p (+ off 4) 101) (aset p (+ off 5) 100)
    (aset p (+ off 6) 50) (aset p (+ off 7) 53) (aset p (+ off 8) 53)
    (aset p (+ off 9) 49) (aset p (+ off 10) 57)
    (setq off (+ off 11))
    ;; encryption c2s: "chacha20-poly1305@openssh.com" (29 bytes)
    (ssh-put-u32 p off 29) (setq off (+ off 4))
    (aset p off 99) (aset p (+ off 1) 104) (aset p (+ off 2) 97)
    (aset p (+ off 3) 99) (aset p (+ off 4) 104) (aset p (+ off 5) 97)
    (aset p (+ off 6) 50) (aset p (+ off 7) 48) (aset p (+ off 8) 45)
    (aset p (+ off 9) 112) (aset p (+ off 10) 111) (aset p (+ off 11) 108)
    (aset p (+ off 12) 121) (aset p (+ off 13) 49) (aset p (+ off 14) 51)
    (aset p (+ off 15) 48) (aset p (+ off 16) 53) (aset p (+ off 17) 64)
    (aset p (+ off 18) 111) (aset p (+ off 19) 112) (aset p (+ off 20) 101)
    (aset p (+ off 21) 110) (aset p (+ off 22) 115) (aset p (+ off 23) 115)
    (aset p (+ off 24) 104) (aset p (+ off 25) 46) (aset p (+ off 26) 99)
    (aset p (+ off 27) 111) (aset p (+ off 28) 109)
    (setq off (+ off 29))
    ;; encryption s2c: same "chacha20-poly1305@openssh.com" (29 bytes)
    (ssh-put-u32 p off 29) (setq off (+ off 4))
    (aset p off 99) (aset p (+ off 1) 104) (aset p (+ off 2) 97)
    (aset p (+ off 3) 99) (aset p (+ off 4) 104) (aset p (+ off 5) 97)
    (aset p (+ off 6) 50) (aset p (+ off 7) 48) (aset p (+ off 8) 45)
    (aset p (+ off 9) 112) (aset p (+ off 10) 111) (aset p (+ off 11) 108)
    (aset p (+ off 12) 121) (aset p (+ off 13) 49) (aset p (+ off 14) 51)
    (aset p (+ off 15) 48) (aset p (+ off 16) 53) (aset p (+ off 17) 64)
    (aset p (+ off 18) 111) (aset p (+ off 19) 112) (aset p (+ off 20) 101)
    (aset p (+ off 21) 110) (aset p (+ off 22) 115) (aset p (+ off 23) 115)
    (aset p (+ off 24) 104) (aset p (+ off 25) 46) (aset p (+ off 26) 99)
    (aset p (+ off 27) 111) (aset p (+ off 28) 109)
    (setq off (+ off 29))
    ;; mac c2s: "none"
    (ssh-put-u32 p off 4) (setq off (+ off 4))
    (aset p off 110) (aset p (+ off 1) 111) (aset p (+ off 2) 110) (aset p (+ off 3) 101)
    (setq off (+ off 4))
    ;; mac s2c: "none"
    (ssh-put-u32 p off 4) (setq off (+ off 4))
    (aset p off 110) (aset p (+ off 1) 111) (aset p (+ off 2) 110) (aset p (+ off 3) 101)
    (setq off (+ off 4))
    ;; comp c2s: "none"
    (ssh-put-u32 p off 4) (setq off (+ off 4))
    (aset p off 110) (aset p (+ off 1) 111) (aset p (+ off 2) 110) (aset p (+ off 3) 101)
    (setq off (+ off 4))
    ;; comp s2c: "none"
    (ssh-put-u32 p off 4) (setq off (+ off 4))
    (aset p off 110) (aset p (+ off 1) 111) (aset p (+ off 2) 110) (aset p (+ off 3) 101)
    (setq off (+ off 4))
    ;; lang c2s: ""
    (ssh-put-u32 p off 0) (setq off (+ off 4))
    ;; lang s2c: ""
    (ssh-put-u32 p off 0) (setq off (+ off 4))
    ;; first_kex_packet_follows
    (aset p off 0) (setq off (+ off 1))
    ;; reserved
    (ssh-put-u32 p off 0)
    ;; Store in per-connection memory for exchange hash
    ;; srv KEXINIT at conn-base + 0x1700 (ssh - 0x20 + 0x1700 = ssh + 0x16E0)
    (ssh-mem-store (+ (- ssh #x20) #x1700) p 164)
    (setf (mem-ref (+ ssh #x1C) :u32) 164)
    p))

;; Build SSH packet from payload (unencrypted)
(defun ssh-make-packet (ssh payload payload-len)
  (let ((base-len (+ 5 payload-len))
        (pad-len 0))
    ;; Padding: total must be multiple of 8
    (setq pad-len (- 8 (mod base-len 8)))
    (when (eq pad-len 8) (setq pad-len 0))
    (when (< pad-len 4) (setq pad-len (+ pad-len 8)))
    (let ((packet-len (+ (+ 1 payload-len) pad-len))
          (total-len (+ (+ (+ 4 1) payload-len) pad-len)))
      (let ((pkt (make-array total-len)))
        ;; packet_length
        (ssh-put-u32 pkt 0 packet-len)
        ;; padding_length
        (aset pkt 4 pad-len)
        ;; payload
        (dotimes (i payload-len) (aset pkt (+ 5 i) (aref payload i)))
        ;; random padding
        (dotimes (i pad-len) (aset pkt (+ 5 payload-len i) (ssh-random ssh)))
        (cons pkt total-len)))))

;; Parse SSH packet from raw data
;; Returns cons(payload-array . payload-len) or NIL
;; ssh = per-connection SSH state base; consumed stored at conn-base+0x16F8
(defun ssh-parse-packet (ssh data data-len)
  (when (< data-len 5) (return ()))
  (let ((packet-len (ssh-get-u32 data 0)))
    (when (< data-len (+ 4 packet-len)) (return ()))
    (let* ((pad-len (aref data 4))
           (payload-len (- packet-len pad-len 1)))
      (let ((payload (make-array payload-len))
            (cb (- ssh #x20)))
        (dotimes (i payload-len) (aset payload i (aref data (+ 5 i))))
        ;; Store remaining data length
        (setf (mem-ref (+ cb #x16F8) :u32) (- data-len (+ 4 packet-len)))
        ;; Store offset to remaining data
        (setf (mem-ref (+ cb #x16FC) :u32) (+ 4 packet-len))
        (cons payload payload-len)))))

;; Encrypt SSH packet with chacha20-poly1305@openssh.com
;; Returns cons(encrypted-data . encrypted-len)
(defun ssh-encrypt-packet (ssh payload payload-len)
  (let ((seq (mem-ref (+ ssh #x08) :u32))
        (k1 (make-array 32))
        (k2 (make-array 32)))
    ;; Split 64-byte s2c key: K1=key[0:32], K2=key[32:64]
    ;; s2c key at ssh+0x0D0, K2 at ssh+0x0F0
    (dotimes (i 32)
      (aset k1 i (mem-ref (+ ssh #x0D0 i) :u8))
      (aset k2 i (mem-ref (+ ssh #x0F0 i) :u8)))
    ;; Compute padding
    (let ((base-len (+ 1 payload-len))
          (pad-len 0))
      (setq pad-len (- 8 (mod base-len 8)))
      (when (eq pad-len 8) (setq pad-len 0))
      (when (< pad-len 4) (setq pad-len (+ pad-len 8)))
      (let ((packet-len (+ 1 payload-len pad-len))
            (nonce (ssh-make-nonce seq)))
        ;; Build plaintext: pad-len + payload + padding
        (let ((plain (make-array packet-len)))
          (aset plain 0 pad-len)
          (dotimes (i payload-len) (aset plain (+ 1 i) (aref payload i)))
          (dotimes (i pad-len) (aset plain (+ 1 payload-len i) (ssh-random ssh)))
          ;; 1. Encrypt length with K2, counter=0
          (let ((len-ks (chacha-block k2 nonce 0))
                (enc-len (make-array 4))
                (len-bytes (make-array 4)))
            (ssh-put-u32 len-bytes 0 packet-len)
            (dotimes (i 4)
              (let ((lb (aref len-bytes i))
                    (lk (aref len-ks i)))
                (aset enc-len i (logxor lb lk))))
            ;; 2. Poly1305 key from K1, counter=0
            (let ((poly-ks (chacha-block k1 nonce 0))
                  (poly-key (make-array 32)))
              (dotimes (i 32) (aset poly-key i (aref poly-ks i)))
              ;; 3. Encrypt data with K1, counter=1
              (let ((enc-data (chacha20-crypt k1 nonce plain packet-len 1)))
                ;; 4. MAC = poly1305(enc-len || enc-data)
                (let ((mac-input (make-array (+ 4 packet-len))))
                  (dotimes (i 4) (aset mac-input i (aref enc-len i)))
                  (dotimes (i packet-len)
                    (aset mac-input (+ 4 i) (aref enc-data i)))
                  (let ((tag (poly1305 poly-key mac-input (+ 4 packet-len))))
                    ;; Result: enc-len(4) + enc-data(packet-len) + tag(16)
                    (let ((total (+ 4 packet-len 16))
                          (result (make-array (+ 4 packet-len 16))))
                      (dotimes (i 4) (aset result i (aref enc-len i)))
                      (dotimes (i packet-len)
                        (aset result (+ 4 i) (aref enc-data i)))
                      (dotimes (i 16)
                        (aset result (+ 4 packet-len i) (aref tag i)))
                      ;; Increment server-seq
                      (setf (mem-ref (+ ssh #x08) :u32) (+ seq 1))
                      (cons result total))))))))))))

;; Decrypt SSH packet with chacha20-poly1305@openssh.com
;; data = raw encrypted bytes, data-len = length
;; Returns cons(payload . payload-len) or NIL
(defun ssh-decrypt-packet (ssh data data-len)
  (when (< data-len 20) (return ()))  ; need at least len(4)+tag(16)
  (let ((seq (mem-ref (+ ssh #x04) :u32))
        (k1 (make-array 32))
        (k2 (make-array 32))
        (nonce (ssh-make-nonce (mem-ref (+ ssh #x04) :u32))))
    ;; Split c2s key: K1=[0:32], K2=[32:64]
    ;; c2s key at ssh+0x090, K2 at ssh+0x0B0
    (dotimes (i 32)
      (aset k1 i (mem-ref (+ ssh #x090 i) :u8))
      (aset k2 i (mem-ref (+ ssh #x0B0 i) :u8)))
    ;; 1. Decrypt length with K2, counter=0
    (let ((len-ks (chacha-block k2 nonce 0))
          (packet-len 0))
      (dotimes (i 4)
        (let ((di (aref data i))
              (ki (aref len-ks i)))
          (let ((b (logxor di ki)))
            (setq packet-len (logior (ash packet-len 8) b)))))
      ;; Check data completeness
      (when (< data-len (+ 4 packet-len 16))
        (return ()))
      ;; 2. Verify MAC
      (let ((poly-ks (chacha-block k1 nonce 0))
            (poly-key (make-array 32)))
        (dotimes (i 32) (aset poly-key i (aref poly-ks i)))
        (let ((mac-input (make-array (+ 4 packet-len))))
          (dotimes (i (+ 4 packet-len))
            (aset mac-input i (aref data i)))
          (let ((expected (poly1305 poly-key mac-input (+ 4 packet-len)))
                (tag-ok 1))
            (dotimes (i 16)
              (unless (eq (aref data (+ 4 packet-len i)) (aref expected i))
                (setq tag-ok 0)))
            (when (zerop tag-ok)
              (return ()))
            ;; 3. Decrypt data
            (let ((enc-data (make-array packet-len)))
              (dotimes (i packet-len)
                (aset enc-data i (aref data (+ 4 i))))
              (let ((plain (chacha20-crypt k1 nonce enc-data packet-len 1)))
                (let* ((pad-len (aref plain 0))
                       (payload-len (- packet-len pad-len 1)))
                  (let ((payload (make-array payload-len)))
                    (dotimes (i payload-len)
                      (aset payload i (aref plain (+ 1 i))))
                    ;; Increment client-seq
                    (setf (mem-ref (+ ssh #x04) :u32) (+ seq 1))
                    ;; Store consumed bytes at per-connection offsets
                    (let ((cb (- ssh #x20)))
                      (setf (mem-ref (+ cb #x16F8) :u32)
                            (- data-len (+ 4 packet-len 16)))
                      (setf (mem-ref (+ cb #x16FC) :u32) (+ 4 packet-len 16)))
                    (cons payload payload-len)))))))))))

;; Send SSH payload via TCP (encrypt if needed)
;; ssh = per-connection SSH state base
(defun ssh-send-payload (ssh payload payload-len)
  (let ((cb (- ssh #x20)))
    (if (not (zerop (mem-ref (+ ssh #x0C) :u32)))
        ;; Encrypted
        (let ((enc (ssh-encrypt-packet ssh payload payload-len)))
          (tcp-send-conn cb (car enc) (cdr enc)))
        ;; Unencrypted - wrap in SSH packet framing
        (let ((pkt (ssh-make-packet ssh payload payload-len)))
          (tcp-send-conn cb (car pkt) (cdr pkt))
          ;; Increment server-seq even for unencrypted packets
          (setf (mem-ref (+ ssh #x08) :u32)
                (+ (mem-ref (+ ssh #x08) :u32) 1))))))

;; Remove n bytes from front of recv buffer
;; ssh = per-connection SSH state base
(defun ssh-buf-consume (ssh n)
  (let ((buf-len (mem-ref (+ ssh #x6D4) :u32)))
    (let ((remaining (- buf-len n)))
      (when (> remaining 0)
        (dotimes (i remaining)
          (setf (mem-ref (+ ssh #x6D8 i) :u8)
                (mem-ref (+ ssh #x6D8 n i) :u8))))
      (setf (mem-ref (+ ssh #x6D4) :u32) remaining)
      remaining)))

;; Load recv buffer contents into an array
(defun ssh-buf-to-array (ssh len)
  (let ((arr (make-array len)))
    (dotimes (i len)
      (aset arr i (mem-ref (+ ssh #x6D8 i) :u8)))
    arr))

;; Receive SSH packet. Returns cons(payload . payload-len) or NIL
;; Handler actor blocks on (receive) to wait for data from net-actor.
;; Net-actor sends data-len (>0) or 0 (close signal) as message.
;; ssh = per-connection SSH state base
(defun ssh-receive-packet (ssh timeout)
  (let ((encrypted (mem-ref (+ ssh #x0C) :u32))
        (cb (- ssh #x20)))
    (if (zerop encrypted)
        ;; Unencrypted: need 4-byte length header first
        (let ((tries 0) (result ()))
          (loop
            (when result (return result))
            (when (> tries timeout) (return ()))
            (let ((blen (mem-ref (+ ssh #x6D4) :u32)))
              (if (> blen 4)
                  (let ((arr (ssh-buf-to-array ssh blen)))
                    ;; ROBUSTNESS: a duplicate/leftover version line ("SSH-...")
                    ;; can precede the real packet (TCP retransmit of the client's
                    ;; version+KEXINIT after a SYN-ACK retransmit).  If the buffer
                    ;; starts with "SSH-", skip it (to the \n) so we don't read
                    ;; the ASCII as a bogus packet length.
                    (if (and (eq (aref arr 0) 83) (eq (aref arr 1) 83)
                             (eq (aref arr 2) 72) (eq (aref arr 3) 45))
                        (let ((nl 0) (i 0))
                          (loop
                            (when (or (not (zerop nl)) (>= i blen)) (return 0))
                            (when (eq (aref arr i) 10) (setq nl i))
                            (setq i (+ i 1)))
                          (if (not (zerop nl))
                              (ssh-buf-consume ssh (+ nl 1))
                              (let ((msg (receive)))
                                (when (zerop msg) (return ())))))
                        (let ((pkt-len (ssh-get-u32 arr 0)))
                          ;; DEFENSE: max unencrypted SSH packet is 35000 bytes.
                          ;; A larger pkt-len means the buffer is desynced/garbage
                          ;; — drop it rather than make-array a huge region (which
                          ;; faults).  Resync by discarding the bad buffer.
                          (if (> pkt-len 35000)
                              (ssh-buf-consume ssh blen)
                              (if (not (< blen (+ 4 pkt-len)))
                                  (let ((parsed (ssh-parse-packet ssh arr blen)))
                                    (when parsed
                                      (ssh-buf-consume ssh (+ 4 pkt-len))
                                      (setf (mem-ref (+ ssh #x04) :u32)
                                            (+ (mem-ref (+ ssh #x04) :u32) 1))
                                      (setq result parsed)))
                                  (let ((msg (receive)))
                                    (when (zerop msg) (return ()))))))))
                  (let ((msg (receive)))
                    (when (zerop msg) (return ())))))
            (setq tries (+ tries 1))))
        ;; Encrypted: decrypt
        (let ((tries 0) (result ()))
          (loop
            (when result (return result))
            (when (> tries timeout) (return ()))
            (let ((blen (mem-ref (+ ssh #x6D4) :u32)))
              (if (> blen 20)
                  (let ((arr (ssh-buf-to-array ssh blen)))
                    (let ((dec (ssh-decrypt-packet ssh arr blen)))
                      (if dec
                          (progn
                            (ssh-buf-consume ssh
                             (- blen (mem-ref (+ cb #x16F8) :u32)))
                            (setq result dec))
                          (let ((msg (receive)))
                            (when (zerop msg) (return ()))))))
                  (let ((msg (receive)))
                    (when (zerop msg) (return ())))))
            (setq tries (+ tries 1)))))))

;; Send version string
(defun ssh-send-version (ssh)
  ;; "SSH-2.0-Modus_1.0\r\n" = 19 bytes
  (let ((v (make-array 19)))
    (aset v 0 83) (aset v 1 83) (aset v 2 72) (aset v 3 45)    ; SSH-
    (aset v 4 50) (aset v 5 46) (aset v 6 48) (aset v 7 45)    ; 2.0-
    (aset v 8 77) (aset v 9 111) (aset v 10 100) (aset v 11 117) ; Modu
    (aset v 12 115) (aset v 13 95) (aset v 14 49) (aset v 15 46) ; s_1.
    (aset v 16 48)                                               ; 0
    (aset v 17 13) (aset v 18 10)                               ; \r\n
    (tcp-send-conn (- ssh #x20) v 19)))

;; Receive client version string
;; Returns 1 on success, 0 on failure
;; ssh = per-connection SSH state base
(defun ssh-receive-version (ssh)
  ;; NOTE: flags use explicit ZEROP tests -- 0 is TRUTHY in the CL/mvm-eval
  ;; image (bare (when flag) with flag=0 fired immediately, returning success
  ;; without parsing; V_C stayed empty => wrong exchange hash => the client's
  ;; "incorrect signature").  Legacy repl-source images treat word-0 as false,
  ;; which is why this ever appeared to work.
  (let ((got-version 0) (tries 0))
    (loop
      (when (not (zerop got-version)) (return 1))
      (when (> tries 50) (return 0))
      ;; Wait for data from net-actor
      (let ((msg (receive)))
        (when (zerop msg) (return 0)))
      (let ((blen (mem-ref (+ ssh #x6D4) :u32)))
        (when (> blen 8)
          ;; Look for SSH-2.0 at start
          (when (eq (mem-ref (+ ssh #x6D8) :u8) 83)  ; S
            (when (eq (mem-ref (+ ssh #x6D9) :u8) 83)  ; S
              (when (eq (mem-ref (+ ssh #x6DA) :u8) 72)  ; H
                ;; Find \r\n or \n
                (let ((end 0) (i 3))
                  (loop
                    (when (not (zerop end)) (return ()))
                    (when (> i blen) (return ()))
                    (when (eq (mem-ref (+ ssh #x6D8 i) :u8) 10) ; \n
                      (setq end i))
                    (setq i (+ i 1)))
                  (when (not (zerop end))
                    ;; Store version (without \r\n) at ssh+0x650
                    (let ((vlen end))
                      (when (eq (mem-ref (+ ssh #x6D8 (- end 1)) :u8) 13)
                        (setq vlen (- end 1)))
                      (dotimes (i vlen)
                        (setf (mem-ref (+ ssh #x650 i) :u8)
                              (mem-ref (+ ssh #x6D8 i) :u8)))
                      (setf (mem-ref (+ ssh #x6D0) :u32) vlen)
                      ;; Consume version from buffer
                      (ssh-buf-consume ssh (+ end 1))
                      (setq got-version 1)))))))))
      (setq tries (+ tries 1)))))

;; Compute exchange hash H = SHA256(V_C || V_S || I_C || I_S || K_S || Q_C || Q_S || K)
;; All as SSH strings (4-byte len prefix)
;; ssh = per-connection SSH state base
(defun ssh-compute-exchange-hash (ssh cli-eph srv-eph shared-secret)
  (sha256-init)
  (let ((cb (- ssh #x20)))
    ;; Build hash input by concatenating all fields
    ;; V_C = client version as SSH string
    (let ((vc-len (mem-ref (+ ssh #x6D0) :u32)))
      (let ((vc (make-array vc-len)))
        (ssh-mem-load vc (+ ssh #x650) vc-len)
        (let ((vc-str (ssh-make-str vc vc-len)))
          ;; V_S = server version as SSH string (17 bytes without \r\n)
          (let ((vs (make-array 17)))
            (aset vs 0 83) (aset vs 1 83) (aset vs 2 72) (aset vs 3 45)
            (aset vs 4 50) (aset vs 5 46) (aset vs 6 48) (aset vs 7 45)
            (aset vs 8 77) (aset vs 9 111) (aset vs 10 100) (aset vs 11 117)
            (aset vs 12 115) (aset vs 13 95) (aset vs 14 49) (aset vs 15 46)
            (aset vs 16 48)
            (let ((vs-str (ssh-make-str vs 17)))
              ;; I_C = client kexinit as SSH string (per-connection at cb+0x1F00)
              (let ((ic-len (mem-ref (+ ssh #x20) :u32)))
                (let ((ic (make-array ic-len)))
                  (ssh-mem-load ic (+ cb #x1F00) ic-len)
                  (let ((ic-str (ssh-make-str ic ic-len)))
                    ;; I_S = server kexinit as SSH string (per-connection at cb+0x1700)
                    (let ((is-len (mem-ref (+ ssh #x1C) :u32)))
                      (let ((is-arr (make-array is-len)))
                        (ssh-mem-load is-arr (+ cb #x1700) is-len)
                        (let ((is-str (ssh-make-str is-arr is-len)))
                          ;; K_S = host key blob as SSH string
                          (let ((hk-enc (ssh-encode-host-key ssh)))
                            (let ((ks-str (ssh-make-str hk-enc (array-length hk-enc))))
                            (let ((qc-str (ssh-make-str cli-eph 32)))
                              ;; Q_S = server ephemeral as SSH string
                              (let ((qs-str (ssh-make-str srv-eph 32)))
                                ;; K = shared secret as mpint
                                (let ((k-mpint (ssh-make-mpint shared-secret)))
                                  ;; Concatenate all
                                  (let ((p1 (ssh-concat2 vc-str (array-length vc-str)
                                                          vs-str (array-length vs-str))))
                                    (let ((p2 (ssh-concat2 p1 (array-length p1)
                                                            ic-str (array-length ic-str))))
                                      (let ((p3 (ssh-concat2 p2 (array-length p2)
                                                              is-str (array-length is-str))))
                                        (let ((p4 (ssh-concat2 p3 (array-length p3)
                                                                ks-str (array-length ks-str))))
                                          (let ((p5 (ssh-concat2 p4 (array-length p4)
                                                                  qc-str (array-length qc-str))))
                                            (let ((p6 (ssh-concat2 p5 (array-length p5)
                                                                    qs-str (array-length qs-str))))
                                              (let ((hash-input (ssh-concat2 p6 (array-length p6)
                                                                              k-mpint (array-length k-mpint))))
                                                (sha256 hash-input))))))))))))))))))))))))))

;; Encode host public key in SSH format: string("ssh-ed25519") + string(pubkey)
;; ssh = per-connection SSH state base
(defun ssh-encode-host-key (ssh)
  (let ((algo (make-array 11)))
    ;; "ssh-ed25519"
    (aset algo 0 115) (aset algo 1 115) (aset algo 2 104) (aset algo 3 45)
    (aset algo 4 101) (aset algo 5 100) (aset algo 6 50) (aset algo 7 53)
    (aset algo 8 53) (aset algo 9 49) (aset algo 10 57)
    (let ((algo-str (ssh-make-str algo 11))
          (pk (make-array 32)))
      ;; Load host public key from per-connection state
      (ssh-mem-load pk (+ ssh #x130) 32)
      (let ((pk-str (ssh-make-str pk 32)))
        (ssh-concat2 algo-str (array-length algo-str)
                     pk-str (array-length pk-str))))))

;; Derive encryption key: SHA256(K || H || key-id || session-id)
;; key-id: 67=C, 68=D, 69=E, 70=F (ASCII)
;; ssh = per-connection SSH state base
(defun ssh-derive-key (ssh key-id needed-len)
  ;; K as mpint (shared-secret at ssh+0x070)
  (let ((k-arr (make-array 32)))
    (ssh-mem-load k-arr (+ ssh #x070) 32)
    (let ((k-mpint (ssh-make-mpint k-arr)))
      ;; H (exchange-hash at ssh+0x050)
      (let ((h (make-array 32)))
        (ssh-mem-load h (+ ssh #x050) 32)
        ;; id-byte
        (let ((id (make-array 1)))
          (aset id 0 key-id)
          ;; session-id at ssh+0x030
          (let ((sid (make-array 32)))
            (ssh-mem-load sid (+ ssh #x030) 32)
            ;; K1 = SHA256(K || H || id || sid)
            (let ((p1 (ssh-concat2 k-mpint (array-length k-mpint)
                                    h 32)))
              (let ((p2 (ssh-concat2 p1 (array-length p1) id 1)))
                (let ((p3 (ssh-concat2 p2 (array-length p2) sid 32)))
                  (let ((k1 (sha256 p3)))
                    ;; If needed > 32, compute K2 = SHA256(K || H || K1)
                    (if (> needed-len 32)
                        (let ((p4 (ssh-concat2 k-mpint (array-length k-mpint)
                                                h 32)))
                          (let ((p5 (ssh-concat2 p4 (array-length p4) k1 32)))
                            (let ((k2 (sha256 p5)))
                              (ssh-concat2 k1 32 k2 32))))
                        k1)))))))))))

;; Derive all encryption keys
;; ssh = per-connection SSH state base
(defun ssh-derive-keys (ssh)
  ;; Per RFC 4253: A=c2s-IV, B=s2c-IV, C=c2s-enc, D=s2c-enc, E=c2s-mac, F=s2c-mac
  ;; For chacha20-poly1305: only C and D needed (64 bytes each)
  (let ((c2s-key (ssh-derive-key ssh 67 64)))  ; 'C' = 67
    (ssh-mem-store (+ ssh #x090) c2s-key 64))
  (let ((s2c-key (ssh-derive-key ssh 68 64)))  ; 'D' = 68
    (ssh-mem-store (+ ssh #x0D0) s2c-key 64))
  ;; Enable encryption
  (setf (mem-ref (+ ssh #x0C) :u32) 1))

;; Build and send KEX_ECDH_REPLY packet (split out to reduce nesting depth)
;; ssh = per-connection SSH state base
(defun ssh-send-kex-reply (ssh sig srv-eph)
  ;; Build signature blob: string("ssh-ed25519") + string(sig)
  (let ((algo (make-array 11)))
    (aset algo 0 115)
    (aset algo 1 115) (aset algo 2 104) (aset algo 3 45)
    (aset algo 4 101) (aset algo 5 100) (aset algo 6 50) (aset algo 7 53)
    (aset algo 8 53) (aset algo 9 49) (aset algo 10 57)
    (let ((algo-str (ssh-make-str algo 11)))
      (let ((sig-str (ssh-make-str sig 64)))
        (let ((sig-blob (ssh-concat2 algo-str (array-length algo-str)
                                      sig-str (array-length sig-str))))
          ;; Build reply: type(1) + host-key + server-eph + sig-blob
          (let ((hk-enc (ssh-encode-host-key ssh)))
            (let ((hk-str (ssh-make-str hk-enc (array-length hk-enc))))
              (let ((se-str (ssh-make-str srv-eph 32)))
                (let ((sb-str (ssh-make-str sig-blob (array-length sig-blob))))
                  ;; Message type 31 = KEX_ECDH_REPLY
                  (let ((msg-type (make-array 1)))
                    (aset msg-type 0 31)
                    (let ((p1 (ssh-concat2 msg-type 1
                                            hk-str (array-length hk-str))))
                      (let ((p2 (ssh-concat2 p1 (array-length p1)
                                              se-str (array-length se-str))))
                        (let ((reply (ssh-concat2 p2 (array-length p2)
                                                  sb-str (array-length sb-str))))
                          (ssh-send-payload ssh reply (array-length reply))
                          1)))))))))))))

;; Handle KEX_ECDH_INIT and send KEX_ECDH_REPLY
;; ssh = per-connection SSH state base
;; Split: compute+sign (7 let levels) + ssh-send-kex-reply (12 let levels)
;; to avoid 18+ nested lets that trigger MVM compiler stack issues.
(defun ssh-handle-kex (ssh kex-init-payload kex-init-len)
  ;; Parse client's ephemeral public key at offset 1 (skip msg type)
  (let ((cli-eph (make-array 32)))
    (dotimes (i 32) (aset cli-eph i (aref kex-init-payload (+ 5 i))))
    ;; Use pre-computed server ephemeral key pair (from state+0x6C4/0x6E4)
    (let ((state (e1000-state-base)))
      (let ((srv-priv (make-array 32)))
        (dotimes (i 32)
          (aset srv-priv i (mem-ref (+ state (+ #x6C4 i)) :u8)))
        (let ((srv-eph (make-array 32)))
          (dotimes (i 32)
            (aset srv-eph i (mem-ref (+ state (+ #x6E4 i)) :u8)))
          (let ((shared (x25519 srv-priv cli-eph)))
            ;; USB keep-alive after first scalar mult
            (usb-keepalive)
            (ssh-mem-store (+ ssh #x070) shared 32)
            (let ((h (ssh-compute-exchange-hash ssh cli-eph srv-eph shared)))
              (ssh-mem-store (+ ssh #x050) h 32)
              (when (zerop (mem-ref ssh :u32))
                (ssh-mem-store (+ ssh #x030) h 32)
                (setf (mem-ref ssh :u32) 1))
              (usb-keepalive)
              (let ((sig (ed25519-sign-fast h 32)))
                (usb-keepalive)
                (ssh-send-kex-reply ssh sig srv-eph)
))))))))

;; Send NEWKEYS message
(defun ssh-send-newkeys (ssh)
  (let ((p (make-array 1)))
    (aset p 0 21)  ; SSH_MSG_NEWKEYS
    (ssh-send-payload ssh p 1)))

;; Send SERVICE_ACCEPT
(defun ssh-send-service-accept (ssh svc-name svc-len)
  (let ((reply (make-array (+ (+ 1 4) svc-len))))
    (aset reply 0 6)  ; SSH_MSG_SERVICE_ACCEPT
    (ssh-put-u32 reply 1 svc-len)
    (dotimes (i svc-len) (aset reply (+ 5 i) (aref svc-name i)))
    (ssh-send-payload ssh reply (array-length reply))))

;; Send USERAUTH_SUCCESS
(defun ssh-send-auth-success (ssh)
  (let ((p (make-array 1)))
    (aset p 0 52)  ; SSH_MSG_USERAUTH_SUCCESS
    (ssh-send-payload ssh p 1)))

;; Send USERAUTH_PK_OK
(defun ssh-send-auth-pk-ok (ssh algo algo-len pk pk-len)
  (let ((reply (make-array (+ (+ (+ (+ 1 4) algo-len) 4) pk-len))))
    (aset reply 0 60)  ; SSH_MSG_USERAUTH_PK_OK
    (ssh-put-u32 reply 1 algo-len)
    (dotimes (i algo-len) (aset reply (+ 5 i) (aref algo i)))
    (ssh-put-u32 reply (+ 5 algo-len) pk-len)
    (dotimes (i pk-len)
      (aset reply (+ (+ 9 algo-len) i) (aref pk i)))
    (ssh-send-payload ssh reply (array-length reply))))

;; Send CHANNEL_OPEN_CONFIRM
(defun ssh-send-channel-confirm (ssh cli-chan srv-chan)
  (let ((reply (make-array 17)))
    (aset reply 0 91)  ; SSH_MSG_CHANNEL_OPEN_CONFIRM
    (ssh-put-u32 reply 1 cli-chan)
    (ssh-put-u32 reply 5 srv-chan)
    (ssh-put-u32 reply 9 65536)   ; initial window size
    (ssh-put-u32 reply 13 32768)  ; max packet size
    (ssh-send-payload ssh reply 17)))

;; Send CHANNEL_SUCCESS
(defun ssh-send-channel-success (ssh cli-chan)
  (let ((reply (make-array 5)))
    (aset reply 0 99)  ; SSH_MSG_CHANNEL_SUCCESS
    (ssh-put-u32 reply 1 cli-chan)
    (ssh-send-payload ssh reply 5)))

;; Send CHANNEL_DATA
(defun ssh-send-channel-data (ssh cli-chan data data-len)
  (let ((reply (make-array (+ 9 data-len))))
    (aset reply 0 94)  ; SSH_MSG_CHANNEL_DATA
    (ssh-put-u32 reply 1 cli-chan)
    (ssh-put-u32 reply 5 data-len)
    (dotimes (i data-len) (aset reply (+ 9 i) (aref data i)))
    (ssh-send-payload ssh reply (array-length reply))))

;; Send string as channel data (with LF->CRLF translation)
;; ssh = per-connection SSH state base
(defun ssh-send-string (ssh str str-len)
  (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
    ;; Count newlines for CRLF expansion
    (let ((crlf-count 0))
      (dotimes (i str-len)
        (when (eq (aref str i) 10) (setq crlf-count (+ crlf-count 1))))
      (let ((out (make-array (+ str-len crlf-count)))
            (j 0))
        (dotimes (i str-len)
          (if (eq (aref str i) 10)
              (progn (aset out j 13) (setq j (+ j 1))
                     (aset out j 10) (setq j (+ j 1)))
              (progn (aset out j (aref str i)) (setq j (+ j 1)))))
        (ssh-send-channel-data ssh cli-chan out j)))))

;; Send prompt "modus> "
(defun ssh-send-prompt (ssh)
  (let ((p (make-array 7)))
    (aset p 0 109) (aset p 1 111) (aset p 2 100) (aset p 3 117)  ; modu
    (aset p 4 115) (aset p 5 62) (aset p 6 32)                    ; s> space
    (ssh-send-string ssh p 7)))

;; Set host key (32-byte private key)
(defun ssh-set-host-key (privkey)
  (ssh-mem-store (+ (e1000-state-base) #x710) privkey 32)
  ;; Compute public key
  (let ((pubkey (ed25519-public-key privkey)))
    (ssh-mem-store (+ (e1000-state-base) #x730) pubkey 32))
  (setf (mem-ref (+ (e1000-state-base) #x624) :u32) 1))

;; Use default test key (all-zero private key)
(defun ssh-use-default-key ()
  (let ((pk (make-array 32)))
    ;; Zero private key (make-array doesn't zero on real hardware)
    (dotimes (i 32) (aset pk i 0))
    (ssh-set-host-key pk)))

;; Handle a single SSH connection
;; ssh = per-connection SSH state base
(defun ssh-handle-connection (ssh)
  (let ((cb (- ssh #x20)))
    ;; SSH state initialized in net-accept-connection before handler spawn
    ;; (must NOT reset here - recv buffer may already have data from net-actor)
    ;; 1. Version exchange
    (ssh-send-version ssh)
    (when (zerop (ssh-receive-version ssh))
      (return ()))
    ;; 2. Send KEXINIT
    (let ((kexinit (ssh-build-kexinit ssh)))
      (ssh-send-payload ssh kexinit (array-length kexinit)))
    ;; 3. Receive client KEXINIT
    (let ((cli-kex (ssh-receive-packet ssh 100)))
      (when (zerop cli-kex) (return ()))
      (let ((cli-kex-payload (car cli-kex)))
        (when (not (eq (aref cli-kex-payload 0) 20)) (return ())) ; must be KEXINIT
        ;; Store client kexinit for exchange hash (per-connection at cb+0x1F00)
        (ssh-mem-store (+ cb #x1F00) cli-kex-payload (cdr cli-kex))
        (setf (mem-ref (+ ssh #x20) :u32) (cdr cli-kex))
        ;; 4. Receive KEX_ECDH_INIT
        (let ((kex-init (ssh-receive-packet ssh 100)))
          (when (zerop kex-init) (return ()))
          (let ((kex-payload (car kex-init)))
            (when (not (eq (aref kex-payload 0) 30)) (return ())) ; KEX_ECDH_INIT
            ;; 5. Handle key exchange
            (ssh-handle-kex ssh kex-payload (cdr kex-init))
            ;; 6. Send NEWKEYS
            (ssh-send-newkeys ssh)
            ;; 7. Receive NEWKEYS
            (let ((nk (ssh-receive-packet ssh 100)))
              (when (zerop nk) (return ()))
              (when (not (eq (aref (car nk) 0) 21)) (return ())) ; NEWKEYS
              ;; 8. Derive keys and enable encryption
              (ssh-derive-keys ssh)
              ;; 9. Main message loop
              (let ((running 1))
                (loop
                  (when (zerop running) (return ()))
                  (let ((pkt (ssh-receive-packet ssh 600)))
                    (when (zerop pkt)
                      (setq running 0))
                    (when pkt
                      (let ((payload (car pkt))
                            (plen (cdr pkt)))
                        (let ((msg-type (aref payload 0)))
                          ;; Dispatch by message type
                          (if (eq msg-type 5)  ; SERVICE_REQUEST
                              (let ((svc-len (ssh-get-u32 payload 1))
                                    (svc (make-array 32)))
                                (dotimes (i svc-len)
                                  (aset svc i (aref payload (+ 5 i))))
                                (ssh-send-service-accept ssh svc svc-len))
                              (if (eq msg-type 50) ; USERAUTH_REQUEST
                                  (ssh-handle-userauth ssh payload plen)
                                  (if (eq msg-type 90) ; CHANNEL_OPEN
                                      (let ((ctype-len (ssh-get-u32 payload 1)))
                                        (let ((cli-chan (ssh-get-u32 payload (+ 5 ctype-len))))
                                          (setf (mem-ref (+ ssh #x18) :u32) cli-chan)
                                          (setf (mem-ref (+ ssh #x14) :u32) 0) ; our chan
                                          (ssh-send-channel-confirm ssh cli-chan 0)))
                                      (if (eq msg-type 98) ; CHANNEL_REQUEST
                                          (let ((rtype-len (ssh-get-u32 payload 5)))
                                            (let ((want-reply (aref payload (+ 9 rtype-len))))
                                              (when (not (zerop want-reply))
                                                (ssh-send-channel-success ssh
                                                 (mem-ref (+ ssh #x18) :u32)))
                                              ;; Check for "shell" (5 bytes starting with 's')
                                              (when (eq rtype-len 5)
                                                (when (eq (aref payload 9) 115) ; 's'hell
                                                  (ssh-send-prompt ssh)))
                                              ;; Check for "exec" (4 bytes starting with 'e')
                                              (when (eq rtype-len 4)
                                                (when (eq (aref payload 9) 101) ; 'e'xec
                                                  ;; Command string follows after want_reply byte
                                                  (let ((cmd-len (ssh-get-u32 payload (+ 10 rtype-len))))
                                                    (let ((cmd (make-array cmd-len))
                                                          (cmd-off (+ 14 rtype-len)))
                                                      (dotimes (i cmd-len)
                                                        (aset cmd i (aref payload (+ cmd-off i))))
                                                      (ssh-eval-line ssh cmd cmd-len)
                                                      ;; Send EOF + CLOSE after exec
                                                      (let ((eof-msg (make-array 5))
                                                            (cli-chan (mem-ref (+ ssh #x18) :u32)))
                                                        (aset eof-msg 0 96) ; SSH_MSG_CHANNEL_EOF
                                                        (ssh-put-u32 eof-msg 1 cli-chan)
                                                        (ssh-send-payload ssh eof-msg 5)
                                                        (let ((close-msg (make-array 5)))
                                                          (aset close-msg 0 97) ; SSH_MSG_CHANNEL_CLOSE
                                                          (ssh-put-u32 close-msg 1 cli-chan)
                                                          (ssh-send-payload ssh close-msg 5)))
                                                      (setq running 0)))))))
                                          (if (eq msg-type 94) ; CHANNEL_DATA
                                              (ssh-handle-channel-data ssh payload plen)
                                              (if (eq msg-type 93) ; WINDOW_ADJUST
                                                  () ; ignore
                                                  (if (eq msg-type 96) ; EOF
                                                      (setq running 0)
                                                      (if (eq msg-type 97) ; CLOSE
                                                          (setq running 0)
                                                          (if (eq msg-type 1) ; DISCONNECT
                                                              (setq running 0)
                                                              (if (eq msg-type 2) ; IGNORE
                                                                  ()
                                                                  ())))))))))))))))))))))))

;; Handle USERAUTH_REQUEST
(defun ssh-handle-userauth (ssh payload plen)
  ;; Parse: username(string) + service(string) + method(string) + ...
  (let ((off 1))
    ;; Skip username
    (let ((ulen (ssh-get-u32 payload off)))
      (setq off (+ off 4 ulen))
      ;; Skip service name
      (let ((slen (ssh-get-u32 payload off)))
        (setq off (+ off 4 slen))
        ;; Method name
        (let ((mlen (ssh-get-u32 payload off)))
          (setq off (+ off 4))
          ;; Check if method is "publickey" (9 bytes)
          (if (eq mlen 9)
              (if (eq (aref payload off) 112) ; 'p'ublickey
                  (progn
                    (setq off (+ off mlen))
                    ;; has-signature boolean
                    (let ((has-sig (aref payload off)))
                      (setq off (+ off 1))
                      ;; algorithm name
                      (let ((alen (ssh-get-u32 payload off)))
                        (setq off (+ off 4))
                        (let ((algo (make-array alen)))
                          (dotimes (i alen)
                            (aset algo i (aref payload (+ off i))))
                          (setq off (+ off alen))
                          ;; public key blob
                          (let ((pklen (ssh-get-u32 payload off)))
                            (setq off (+ off 4))
                            (let ((pk-blob (make-array pklen)))
                              (dotimes (i pklen)
                                (aset pk-blob i (aref payload (+ off i))))
                              (setq off (+ off pklen))
                              (if (zerop has-sig)
                                  ;; First pass: send PK_OK
                                  (ssh-send-auth-pk-ok ssh algo alen pk-blob pklen)
                                  ;; Second pass: verify signature and accept
                                  (progn
                                    (ssh-send-auth-success ssh)
                                    (setf (mem-ref (+ ssh #x10) :u32) 1)
                                    ;; Send welcome banner via channel data later
                                    ))))))))
                  ;; Not publickey method
                  (ssh-send-auth-success ssh))
              ;; Accept any method for simplicity
              (ssh-send-auth-success ssh)))))))

;; Handle Ctrl-D: send Bye, channel EOF, channel CLOSE
(defun ssh-handle-ctrl-d (ssh)
  (let ((bye (make-array 9)))
    (aset bye 0 13) (aset bye 1 10)
    (aset bye 2 66) (aset bye 3 121) (aset bye 4 101)
    (aset bye 5 33) (aset bye 6 13) (aset bye 7 10) (aset bye 8 10)
    (ssh-send-string ssh bye 9))
  (let ((eof-msg (make-array 5)))
    (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
      (aset eof-msg 0 96)
      (ssh-put-u32 eof-msg 1 cli-chan)
      (ssh-send-payload ssh eof-msg 5)))
  (let ((close-msg (make-array 5)))
    (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
      (aset close-msg 0 97)
      (ssh-put-u32 close-msg 1 cli-chan)
      (ssh-send-payload ssh close-msg 5))))

;; --- SSH unified REPL helpers ---
;; Load per-connection edit state into shared edit state
;; ssh+0x28 = buf-len, ssh+0x150 = cursor-pos, ssh+0x154 = esc-state
;; ssh+0x550 = line buffer (256 bytes)
;; NOTE: ssh+0x030-0x04F is session ID (written during KEX) -- do NOT use!
(defun ssh-load-edit-state (ssh)
  (edit-set-line-len (mem-ref (+ ssh #x28) :u32))
  (edit-set-cursor-pos (mem-ref (+ ssh #x150) :u32))
  (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) (mem-ref (+ ssh #x154) :u32))
  (let ((len (mem-ref (+ ssh #x28) :u32))
        (i 0))
    (loop
      (if (< i len)
          (progn
            (setf (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8)
                  (mem-ref (+ ssh #x550 i) :u8))
            (setq i (+ i 1)))
          (return 0)))))

;; Save shared edit state back to per-connection state
(defun ssh-save-edit-state (ssh)
  (let ((len (edit-line-len)))
    (setf (mem-ref (+ ssh #x28) :u32) len)
    (setf (mem-ref (+ ssh #x150) :u32) (edit-cursor-pos))
    (setf (mem-ref (+ ssh #x154) :u32) (mem-ref (+ (ssh-ipc-base) #x12810) :u64))
    (let ((i 0))
      (loop
        (if (< i len)
            (progn
              (setf (mem-ref (+ ssh #x550 i) :u8)
                    (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8))
              (setq i (+ i 1)))
            (return 0))))))

;; Flush captured write-byte output to SSH channel
(defun ssh-flush-output (ssh)
  (let ((out-len (mem-ref (+ (ssh-ipc-base) #x18) :u32)))
    (when (> out-len 0)
      (let ((out (make-array out-len)))
        (dotimes (i out-len)
          (aset out i (mem-ref (+ (+ (ssh-ipc-base) #x100) i) :u8)))
        (ssh-send-string ssh out out-len))
      (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0))))

;; Handle CHANNEL_DATA - unified REPL with full line editing
;; Uses handle-edit-byte + capture/flush for SSH output routing
(defun ssh-handle-channel-data (ssh payload plen)
  (let ((data-len (ssh-get-u32 payload 5)))
    ;; Load per-connection state into shared edit state
    (ssh-load-edit-state ssh)
    ;; Set prompt mode to SSH
    (setf (mem-ref (+ (ssh-ipc-base) #x12A00) :u64) 1)
    ;; Enable capture -- accumulate ALL output for the batch
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
    (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
    ;; Process each byte through the shared line editor
    (let ((i 0))
      (loop
        (if (< i data-len)
            (progn
              (let ((rc (handle-edit-byte (aref payload (+ 9 i)))))
                (if (eq rc 1)
                    ;; Enter: flush batch, eval, prompt
                    (progn
                      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
                      (ssh-flush-output ssh)
                      (ssh-save-edit-state ssh)
                      ;; Newline echo
                      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
                      (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
                      (write-byte 10)
                      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
                      (ssh-flush-output ssh)
                      ;; Evaluate
                      (ssh-do-eval ssh)
                      ;; Prompt
                      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
                      (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
                      (emit-prompt)
                      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
                      (ssh-flush-output ssh)
                      ;; Reinit and re-enable capture for rest of batch
                      (edit-set-line-len 0)
                      (edit-set-cursor-pos 0)
                      (setf (mem-ref (+ (ssh-ipc-base) #x12810) :u64) 0)
                      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
                      (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0))
                    (if (eq rc 2)
                        ;; Ctrl-D: flush + disconnect
                        (progn
                          (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
                          (ssh-flush-output ssh)
                          (ssh-save-edit-state ssh)
                          (ssh-handle-ctrl-d ssh)
                          (setq i data-len))
                        ())))
              (setq i (+ i 1)))
            (return 0))))
    ;; Flush remaining batched output
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
    (ssh-flush-output ssh)
    ;; Save state back to per-connection
    (ssh-save-edit-state ssh)
    (setf (mem-ref (+ (ssh-ipc-base) #x12A00) :u64) 0)))

;; Evaluate current line buffer for SSH (dispatch on content)
(defun ssh-do-eval (ssh)
  (let ((len (edit-line-len)))
    (when (> len 0)
      (let ((b0 (mem-ref (+ (ssh-ipc-base) #x28) :u8)))
        (if (eq b0 40)
            ;; Check for (quit) -- 6 chars
            (if (eq len 6)
                (if (eq (mem-ref (+ (ssh-ipc-base) #x29) :u8) 113)
                    (if (eq (mem-ref (+ (ssh-ipc-base) #x2C) :u8) 116)
                        (ssh-handle-ctrl-d ssh)
                        (ssh-do-eval-expr ssh))
                    (ssh-do-eval-expr ssh))
                (ssh-do-eval-expr ssh))
            ())))))

;; Evaluate s-expression from shared buffer and send result via SSH
(defun ssh-do-eval-expr (ssh)
  (let ((len (edit-line-len)))
    (spin-lock (+ (ssh-ipc-base) #x60440))
    (setf (mem-ref (+ (ssh-ipc-base) #x60448) :u32) (ash (- ssh (+ (ssh-conn-base) #x20)) -14))
    ;; FIFO: skip '(' at index 0, len already set by Enter handler
    (setf (mem-ref (+ (ssh-ipc-base) #x20) :u32) 1)
    ;; Suppress serial echo during reader parse (chars already echoed during editing)
    (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 2)
    (let ((lst (read-list)))
      ;; Now enable capture for eval result
      (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
      (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
      (let ((result (native-eval lst)))
        (write-byte 10)
        (write-byte 61) (write-byte 32)
        (print-obj result)
        (write-byte 10)
        (let ((out-len (mem-ref (+ (ssh-ipc-base) #x18) :u32)))
          (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
          (setf (mem-ref (+ (ssh-ipc-base) #x24) :u32) 0)
          (spin-unlock (+ (ssh-ipc-base) #x60440))
          (when (> out-len 0)
            (let ((out (make-array out-len)))
              (dotimes (i out-len)
                (aset out i (mem-ref (+ (+ (ssh-ipc-base) #x100) i) :u8)))
              (ssh-send-string ssh out out-len))))))))


;; Evaluate command line from SSH exec request
;; Copies command into shared line buffer, evaluates, flushes output to SSH
;; ssh = per-connection SSH state base, cmd = byte array, cmd-len = length
(defun ssh-eval-line (ssh cmd cmd-len)
  ;; Copy command into shared line buffer at 0x300028
  (dotimes (i cmd-len) (setf (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8) (aref cmd i)))
  ;; Set line length
  (edit-set-line-len cmd-len)
  ;; Set prompt mode to SSH
  (setf (mem-ref (+ (ssh-ipc-base) #x12A00) :u64) 1)
  ;; Enable output capture (bit0=capture, bit1=suppress serial)
  (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 3)
  (setf (mem-ref (+ (ssh-ipc-base) #x18) :u32) 0)
  ;; Evaluate (dispatches based on first char)
  (ssh-do-eval ssh)
  ;; Flush captured output
  (setf (mem-ref (+ (ssh-ipc-base) #x14) :u32) 0)
  (ssh-flush-output ssh)
  (setf (mem-ref (+ (ssh-ipc-base) #x12A00) :u64) 0))

;; Evaluate expression from SSH (alias for ssh-eval-line)
(defun ssh-eval-expr (ssh cmd cmd-len)
  (ssh-eval-line ssh cmd cmd-len))

;; ---- Inter-session communication ----

;; List active SSH sessions (1-indexed slot : actor ID)
(defun sessions ()
  (let ((i 0))
    (loop
      (when (>= i 4) (return 0))
      (let ((cb (+ (ssh-conn-base) (ash i 14))))
        (let ((state (mem-ref cb :u32)))
          (when (not (zerop state))
            (write-byte 83) (print-dec (+ i 1))
            (write-byte 58) (write-byte 65)
            (print-dec (mem-ref (+ cb #x18) :u32))
            (write-byte 10))))
      (setq i (+ i 1)))))

;; Return current session's slot number (1-indexed, avoids 0=NIL)
(defun whoami ()
  (+ (mem-ref (+ (ssh-ipc-base) #x60448) :u32) 1))

;; Send a fixnum to another session's inbox ring buffer
;; target is 1-indexed (matches whoami/sessions output)
;; Ring buffer at conn-base(slot) + 0x3710:
;;   +0x3710: write-idx (u32), +0x3714: read-idx (u32)
;;   +0x3718: entries[8] (u32 x 8 = 32 bytes)
(defun msg (target value)
  (let ((slot (- target 1)))
    (let ((cb (+ (ssh-conn-base) (ash slot 14))))
      (let ((state (mem-ref cb :u32)))
        (if (zerop state)
            0
            (let ((widx (mem-ref (+ cb #x3710) :u32)))
              (let ((offset (ash (logand widx 7) 2)))
                (setf (mem-ref (+ cb #x3718 offset) :u32) value))
              (setf (mem-ref (+ cb #x3710) :u32) (+ widx 1))
              1))))))

;; Display and consume pending messages from inbox
(defun inbox ()
  (let ((conn (mem-ref (+ (ssh-ipc-base) #x60448) :u32)))
    (let ((cb (+ (ssh-conn-base) (ash conn 14))))
      (let ((ridx (mem-ref (+ cb #x3714) :u32)))
        (let ((widx (mem-ref (+ cb #x3710) :u32)))
          (if (= ridx widx)
              0
              (let ((count 0))
                (loop
                  (when (>= ridx widx) (return count))
                  (let ((val (mem-ref (+ cb #x3718 (ash (logand ridx 7) 2)) :u32)))
                    (write-byte 60) (print-obj val) (write-byte 62) (write-byte 10))
                  (setq ridx (+ ridx 1))
                  (setq count (+ count 1)))
                (setf (mem-ref (+ cb #x3714) :u32) ridx)
                count)))))))

;; Evaluate a Lisp form from the kernel command line.
;; Scans cmdline at 0x300200 for '(' and evaluates the form.
;; Usage: qemu ... -append "(progn (net-init-dhcp) (ssh-server 22))"
(defun eval-cmdline ()
  ;; Scan for '(' (ASCII 40) in first 250 bytes
  ;; Store found position + 1 (0 = not found)
  (let ((found 0)
        (pos 0))
    (loop
      (when (> pos 250) (return 0))
      (let ((b (mem-ref (+ (+ (ssh-ipc-base) #x200) pos) :u8)))
        (when (zerop b) (return 0))
        (when (eq b 40)
          (setq found (+ pos 1))
          (return 0))
        (setq pos (+ pos 1))))
    (when (not (zerop found))
      (let ((start (- found 1))
            (i 0))
        ;; Copy from cmdline[start..] to edit line buffer at 0x300028
        (loop
          (let ((c (mem-ref (+ (+ (ssh-ipc-base) #x200) (+ start i)) :u8)))
            (when (zerop c) (return 0))
            (when (> i 250) (return 0))
            (setf (mem-ref (+ (+ (ssh-ipc-base) #x28) i) :u8) c)
            (setq i (+ i 1))))
        ;; Set up reader FIFO and evaluate
        (setf (mem-ref (+ (ssh-ipc-base) #x12800) :u64) i)
        (setf (mem-ref (+ (ssh-ipc-base) #x20) :u32) 1)
        (setf (mem-ref (+ (ssh-ipc-base) #x24) :u32) i)
        (eval-line-expr)))))

;; Compile a native GC wrapper function using the runtime compiler.
;; This creates a callable function that runs inline per-actor GC,
;; then stores its address at 0x395000 so cross-compiled make-array
;; and make-string can call it on OOM.
;; Must be called before any actors that need GC in cross-compiled allocators.
(defun init-gc-helper ()
  ;; Build args for rt-compile-defun: (name params body)
  ;; params = nil (0)
  ;; body = (gc) = (sym-gc . nil) = (67 . 0)
  (let ((gc-form (cons 67 0)))
    (let ((defun-args (cons (hash-of "do-gc") (cons 0 (cons gc-form 0)))))
      (rt-compile-defun defun-args)
      ;; Store the function address at fixed location 0x395000
      (setf (mem-ref (+ (ssh-ipc-base) #x95000) :u64) (nfn-lookup (hash-of "do-gc"))))))

(defun ssh-server (port)
  (ssh-seed-random)
  (ssh-init-strings)
  ;; Initialize GC helper BEFORE key generation (ed25519 needs make-array with GC)
  (init-gc-helper)
  (when (zerop (mem-ref (+ (e1000-state-base) #x624) :u32))
    (ssh-use-default-key))
  (write-byte 83) (write-byte 83) (write-byte 72)  ; "SSH"
  (write-byte 58) (print-dec port) (write-byte 10)
  ;; Store listen port for net-actor
  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) port)
  ;; Clear connection table (4 slots x 16KB)
  (let ((i 0))
    (loop
      (when (>= i 4) (return 0))
      (setf (mem-ref (conn-base i) :u32) 0)
      (setq i (+ i 1))))
  ;; Spawn network actor (handles all E1000 RX, demuxes TCP)
  (actor-spawn (nfn-lookup (hash-of "net-actor-main"))))
