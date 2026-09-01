;;;; 32bit-overrides.lisp — 30-bit fixnum safety for 32-bit networking
;;;;
;;;; MUST be loaded LAST (after ip.lisp, ssh.lisp, aarch64-overrides.lisp)
;;;; because MVM uses last-defun-wins resolution.
;;;;
;;;; Problem: (ash byte 24) overflows on i386 when byte >= 64, because the
;;;; tagged representation (value << 1) exceeds 32 bits.
;;;; Fix: Mask the high byte to 6 bits, giving a 30-bit result.
;;;; TCP seq numbers lose top 2 bits but relative offsets still work.

;; Read u32 big-endian from memory — 30-bit safe
;; Flat let bindings to avoid nested logior clobber (MVM compiler limitation)
(defun buf-read-u32-mem (addr off)
  (let ((b0 (logand (mem-ref (+ addr off) :u8) #x3F)))
    (let ((b1 (mem-ref (+ addr (+ off 1)) :u8)))
      (let ((b2 (mem-ref (+ addr (+ off 2)) :u8)))
        (let ((b3 (mem-ref (+ addr (+ off 3)) :u8)))
          (let ((hi (ash b0 24)))
            (let ((mid (ash b1 16)))
              (let ((lo (ash b2 8)))
                (let ((hm (logior hi mid)))
                  (let ((hml (logior hm lo)))
                    (logior hml b3)))))))))))

;; Read u32 big-endian from array — 30-bit safe
(defun buf-read-u32 (buf off)
  (let ((b0 (logand (aref buf off) #x3F)))
    (let ((b1 (aref buf (+ off 1))))
      (let ((b2 (aref buf (+ off 2))))
        (let ((b3 (aref buf (+ off 3))))
          (let ((hi (ash b0 24)))
            (let ((mid (ash b1 16)))
              (let ((lo (ash b2 8)))
                (let ((hm (logior hi mid)))
                  (let ((hml (logior hm lo)))
                    (logior hml b3)))))))))))

;; SSH read u32 big-endian from array — 30-bit safe
(defun ssh-get-u32 (arr off)
  (let ((b0 (logand (aref arr off) #x3F)))
    (let ((b1 (aref arr (+ off 1))))
      (let ((b2 (aref arr (+ off 2))))
        (let ((b3 (aref arr (+ off 3))))
          (let ((hi (ash b0 24)))
            (let ((mid (ash b1 16)))
              (let ((lo (ash b2 8)))
                (let ((hm (logior hi mid)))
                  (let ((hml (logior hm lo)))
                    (logior hml b3)))))))))))

;; Read u32 little-endian from array — 30-bit safe
;; Used by Poly1305 (poly-from-17) and X25519 (fe-from-bytes)
(defun buf-read-u32-le (buf offset)
  (let ((b0 (aref buf offset)))
    (let ((b1 (aref buf (+ offset 1))))
      (let ((b2 (aref buf (+ offset 2))))
        (let ((b3 (logand (aref buf (+ offset 3)) #x3F)))
          (let ((hi (ash b3 24)))
            (let ((mid (ash b2 16)))
              (let ((lo (ash b1 8)))
                (let ((hm (logior hi mid)))
                  (let ((hml (logior hm lo)))
                    (logior hml b0)))))))))))

;; i386-safe fe-from-bytes: extracts 26/25-bit limbs byte-by-byte
;; to avoid 32-bit intermediate overflow in buf-read-u32-le.
;; Donna representation: 10 limbs alternating 26/25 bits.
;; bit_start = [0, 26, 51, 77, 102, 128, 153, 179, 204, 230]
(defun fe-from-bytes (bytes)
  (let ((fe (make-array 40)))
    ;; Pre-compute each limb to avoid register clobber when passing
    ;; nested logior expressions as function call arguments (MVM bug).
    (let ((l0 (logior (aref bytes 0)
                      (logior (ash (aref bytes 1) 8)
                              (logior (ash (aref bytes 2) 16)
                                      (ash (logand (aref bytes 3) #x03) 24))))))
      (buf-write-u32 fe 0 l0))
    (let ((l1 (logior (ash (aref bytes 3) -2)
                      (logior (ash (aref bytes 4) 6)
                              (logior (ash (aref bytes 5) 14)
                                      (ash (logand (aref bytes 6) #x07) 22))))))
      (buf-write-u32 fe 4 l1))
    (let ((l2 (logior (ash (aref bytes 6) -3)
                      (logior (ash (aref bytes 7) 5)
                              (logior (ash (aref bytes 8) 13)
                                      (ash (logand (aref bytes 9) #x1F) 21))))))
      (buf-write-u32 fe 8 l2))
    (let ((l3 (logior (ash (aref bytes 9) -5)
                      (logior (ash (aref bytes 10) 3)
                              (logior (ash (aref bytes 11) 11)
                                      (ash (logand (aref bytes 12) #x3F) 19))))))
      (buf-write-u32 fe 12 l3))
    (let ((l4 (logior (ash (aref bytes 12) -6)
                      (logior (ash (aref bytes 13) 2)
                              (logior (ash (aref bytes 14) 10)
                                      (ash (aref bytes 15) 18))))))
      (buf-write-u32 fe 16 l4))
    (let ((l5 (logior (aref bytes 16)
                      (logior (ash (aref bytes 17) 8)
                              (logior (ash (aref bytes 18) 16)
                                      (ash (logand (aref bytes 19) #x01) 24))))))
      (buf-write-u32 fe 20 l5))
    (let ((l6 (logior (ash (aref bytes 19) -1)
                      (logior (ash (aref bytes 20) 7)
                              (logior (ash (aref bytes 21) 15)
                                      (ash (logand (aref bytes 22) #x07) 23))))))
      (buf-write-u32 fe 24 l6))
    (let ((l7 (logior (ash (aref bytes 22) -3)
                      (logior (ash (aref bytes 23) 5)
                              (logior (ash (aref bytes 24) 13)
                                      (ash (logand (aref bytes 25) #x0F) 21))))))
      (buf-write-u32 fe 28 l7))
    (let ((l8 (logior (ash (aref bytes 25) -4)
                      (logior (ash (aref bytes 26) 4)
                              (logior (ash (aref bytes 27) 12)
                                      (ash (logand (aref bytes 28) #x3F) 20))))))
      (buf-write-u32 fe 32 l8))
    (let ((l9 (logand (logior (ash (aref bytes 28) -6)
                              (logior (ash (aref bytes 29) 2)
                                      (logior (ash (aref bytes 30) 10)
                                              (ash (aref bytes 31) 18))))
                      #x1FFFFFF)))
      (buf-write-u32 fe 36 l9))
    fe))

;; i386-safe fe-to-bytes: avoids (ash l4 6) and (ash l9 6) overflow.
;; Pre-masks limb before shifting where intermediate would exceed 30-bit fixnum.
(defun fe-to-bytes (fe)
  (fe-reduce fe)
  (let ((r (make-array 32))
        (l0 (buf-read-u32 fe 0))
        (l1 (buf-read-u32 fe 4))
        (l2 (buf-read-u32 fe 8))
        (l3 (buf-read-u32 fe 12))
        (l4 (buf-read-u32 fe 16))
        (l5 (buf-read-u32 fe 20))
        (l6 (buf-read-u32 fe 24))
        (l7 (buf-read-u32 fe 28))
        (l8 (buf-read-u32 fe 32))
        (l9 (buf-read-u32 fe 36)))
    (aset r 0 (logand l0 #xFF))
    (aset r 1 (logand (ash l0 -8) #xFF))
    (aset r 2 (logand (ash l0 -16) #xFF))
    (aset r 3 (logand (logior (ash l0 -24) (ash l1 2)) #xFF))
    (aset r 4 (logand (ash l1 -6) #xFF))
    (aset r 5 (logand (ash l1 -14) #xFF))
    (aset r 6 (logand (logior (ash l1 -22) (ash l2 3)) #xFF))
    (aset r 7 (logand (ash l2 -5) #xFF))
    (aset r 8 (logand (ash l2 -13) #xFF))
    (aset r 9 (logand (logior (ash l2 -21) (ash l3 5)) #xFF))
    (aset r 10 (logand (ash l3 -3) #xFF))
    (aset r 11 (logand (ash l3 -11) #xFF))
    ;; byte 12: (ash l4 6) overflows — pre-mask l4 to 2 bits
    (aset r 12 (logand (logior (ash l3 -19) (ash (logand l4 #x03) 6)) #xFF))
    (aset r 13 (logand (ash l4 -2) #xFF))
    (aset r 14 (logand (ash l4 -10) #xFF))
    (aset r 15 (logand (ash l4 -18) #xFF))
    (aset r 16 (logand l5 #xFF))
    (aset r 17 (logand (ash l5 -8) #xFF))
    (aset r 18 (logand (ash l5 -16) #xFF))
    (aset r 19 (logand (logior (ash l5 -24) (ash l6 1)) #xFF))
    (aset r 20 (logand (ash l6 -7) #xFF))
    (aset r 21 (logand (ash l6 -15) #xFF))
    (aset r 22 (logand (logior (ash l6 -23) (ash l7 3)) #xFF))
    (aset r 23 (logand (ash l7 -5) #xFF))
    (aset r 24 (logand (ash l7 -13) #xFF))
    (aset r 25 (logand (logior (ash l7 -21) (ash l8 4)) #xFF))
    (aset r 26 (logand (ash l8 -4) #xFF))
    (aset r 27 (logand (ash l8 -12) #xFF))
    ;; byte 28: (ash l9 6) overflows — pre-mask l9 to 2 bits
    (aset r 28 (logand (logior (ash l8 -20) (ash (logand l9 #x03) 6)) #xFF))
    (aset r 29 (logand (ash l9 -2) #xFF))
    (aset r 30 (logand (ash l9 -10) #xFF))
    (aset r 31 (logand (ash l9 -18) #xFF))
    r))

;; i386-safe poly-from-17: byte-by-byte extraction avoids buf-read-u32-le overflow.
;; Poly1305 uses 5 × 26-bit limbs from 17-byte (130-bit + 1 sentinel) input.
(defun poly-from-17 (block limbs)
  ;; Pre-compute limbs to avoid register clobber (MVM bug with nested
  ;; expressions as function call arguments).
  (let ((l0 (logand (logior (aref block 0)
                            (logior (ash (aref block 1) 8)
                                    (logior (ash (aref block 2) 16)
                                            (ash (logand (aref block 3) #x03) 24))))
                    #x3FFFFFF)))
    (buf-write-u32 limbs 0 l0))
  (let ((l1 (logand (logior (ash (aref block 3) -2)
                            (logior (ash (aref block 4) 6)
                                    (logior (ash (aref block 5) 14)
                                            (ash (logand (aref block 6) #x0F) 22))))
                    #x3FFFFFF)))
    (buf-write-u32 limbs 4 l1))
  (let ((l2 (logand (logior (ash (aref block 6) -4)
                            (logior (ash (aref block 7) 4)
                                    (logior (ash (aref block 8) 12)
                                            (ash (logand (aref block 9) #x3F) 20))))
                    #x3FFFFFF)))
    (buf-write-u32 limbs 8 l2))
  (let ((l3 (logand (logior (ash (aref block 9) -6)
                            (logior (ash (aref block 10) 2)
                                    (logior (ash (aref block 11) 10)
                                            (ash (aref block 12) 18))))
                    #x3FFFFFF)))
    (buf-write-u32 limbs 12 l3))
  (let ((l4 (logand (logior (aref block 13)
                            (logior (ash (aref block 14) 8)
                                    (logior (ash (aref block 15) 16)
                                            (ash (logand (aref block 16) #x03) 24))))
                    #x3FFFFFF)))
    (buf-write-u32 limbs 16 l4)))

;; Override ssh-buf-to-array: original uses 3-arg + (broken on MVM)
(defun ssh-buf-to-array (ssh len)
  (let ((arr (make-array len))
        (base (+ ssh #x6D8)))
    (dotimes (i len)
      (aset arr i (mem-ref (+ base i) :u8)))
    arr))

;; Override ssh-buf-consume: original uses 3/4-arg + (broken on MVM)
(defun ssh-buf-consume (ssh n)
  (let ((buf-len (mem-ref (+ ssh #x6D4) :u32)))
    (let ((remaining (- buf-len n)))
      (when (> remaining 0)
        (let ((dst (+ ssh #x6D8))
              (src (+ (+ ssh #x6D8) n)))
          (dotimes (i remaining)
            (setf (mem-ref (+ dst i) :u8)
                  (mem-ref (+ src i) :u8)))))
      (setf (mem-ref (+ ssh #x6D4) :u32) remaining)
      remaining)))

;; Override ssh-copy-host-key: original uses 3-arg + (broken on MVM)
(defun ssh-copy-host-key (conn)
  (let ((state (e1000-state-base))
        (ssh (conn-ssh conn)))
    (let ((src-priv (+ state #x710))
          (dst-priv (+ ssh #x110)))
      (dotimes (i 32)
        (setf (mem-ref (+ dst-priv i) :u8) (mem-ref (+ src-priv i) :u8))))
    (let ((src-pub (+ state #x730))
          (dst-pub (+ ssh #x130)))
      (dotimes (i 32)
        (setf (mem-ref (+ dst-pub i) :u8) (mem-ref (+ src-pub i) :u8))))
    (setf (mem-ref (+ ssh #x24) :u32) 1)))

;; Override ssh-parse-packet: original uses 3-arg - (broken on MVM)
(defun ssh-parse-packet (ssh data data-len)
  (when (< data-len 5) (return ()))
  (let ((packet-len (ssh-get-u32 data 0)))
    (when (< data-len (+ 4 packet-len)) (return ()))
    (let ((pad-len (aref data 4)))
      (let ((payload-len (- (- packet-len pad-len) 1)))
        (let ((payload (make-array payload-len))
              (cb (- ssh #x20)))
          (dotimes (i payload-len) (aset payload i (aref data (+ 5 i))))
          (setf (mem-ref (+ cb #x16F8) :u32) (- data-len (+ 4 packet-len)))
          (setf (mem-ref (+ cb #x16FC) :u32) (+ 4 packet-len))
          (cons payload payload-len))))))

;; Override ssh-make-packet: original uses 3-arg + for padding (broken on MVM)
(defun ssh-make-packet (ssh payload payload-len)
  (let ((base-len (+ 5 payload-len))
        (pad-len 0))
    (setq pad-len (- 8 (mod base-len 8)))
    (when (eq pad-len 8) (setq pad-len 0))
    (when (< pad-len 4) (setq pad-len (+ pad-len 8)))
    (let ((packet-len (+ (+ 1 payload-len) pad-len))
          (total-len (+ (+ (+ 4 1) payload-len) pad-len)))
      (let ((pkt (make-array total-len)))
        (ssh-put-u32 pkt 0 packet-len)
        (aset pkt 4 pad-len)
        (dotimes (i payload-len) (aset pkt (+ 5 i) (aref payload i)))
        (let ((pad-off (+ 5 payload-len)))
          (dotimes (i pad-len) (aset pkt (+ pad-off i) (ssh-random ssh))))
        (cons pkt total-len)))))

;; Override ssh-encrypt-packet: original uses 3-arg + (broken on MVM)
(defun ssh-encrypt-packet (ssh payload payload-len)
  (let ((seq (mem-ref (+ ssh #x08) :u32))
        (k1 (make-array 32))
        (k2 (make-array 32)))
    (let ((k1-base (+ ssh #x0D0))
          (k2-base (+ ssh #x0F0)))
      (dotimes (i 32)
        (aset k1 i (mem-ref (+ k1-base i) :u8))
        (aset k2 i (mem-ref (+ k2-base i) :u8))))
    (let ((base-len (+ 1 payload-len))
          (pad-len 0))
      (setq pad-len (- 8 (mod base-len 8)))
      (when (eq pad-len 8) (setq pad-len 0))
      (when (< pad-len 4) (setq pad-len (+ pad-len 8)))
      (let ((packet-len (+ (+ 1 payload-len) pad-len))
            (nonce (ssh-make-nonce seq)))
        (let ((plain (make-array packet-len)))
          (aset plain 0 pad-len)
          (dotimes (i payload-len) (aset plain (+ 1 i) (aref payload i)))
          (let ((pad-off (+ 1 payload-len)))
            (dotimes (i pad-len) (aset plain (+ pad-off i) (ssh-random ssh))))
          (let ((len-ks (chacha-block k2 nonce 0))
                (enc-len (make-array 4))
                (len-bytes (make-array 4)))
            (ssh-put-u32 len-bytes 0 packet-len)
            (dotimes (i 4)
              (let ((lb (aref len-bytes i))
                    (lk (aref len-ks i)))
                (aset enc-len i (logxor lb lk))))
            (let ((poly-ks (chacha-block k1 nonce 0))
                  (poly-key (make-array 32)))
              (dotimes (i 32) (aset poly-key i (aref poly-ks i)))
              (let ((enc-data (chacha20-crypt k1 nonce plain packet-len 1)))
                (let ((mac-input (make-array (+ 4 packet-len))))
                  (dotimes (i 4) (aset mac-input i (aref enc-len i)))
                  (dotimes (i packet-len)
                    (aset mac-input (+ 4 i) (aref enc-data i)))
                  (let ((tag (poly1305 poly-key mac-input (+ 4 packet-len))))
                    (let ((total (+ (+ 4 packet-len) 16))
                          (result (make-array (+ (+ 4 packet-len) 16))))
                      (dotimes (i 4) (aset result i (aref enc-len i)))
                      (dotimes (i packet-len)
                        (aset result (+ 4 i) (aref enc-data i)))
                      (let ((tag-off (+ 4 packet-len)))
                        (dotimes (i 16)
                          (aset result (+ tag-off i) (aref tag i))))
                      (setf (mem-ref (+ ssh #x08) :u32) (+ seq 1))
                      (cons result total))))))))))))

;; Override ssh-decrypt-packet: original uses 3-arg + (broken on MVM)
(defun ssh-decrypt-packet (ssh data data-len)
  (when (< data-len 20) (return ()))
  (let ((seq (mem-ref (+ ssh #x04) :u32))
        (k1 (make-array 32))
        (k2 (make-array 32))
        (nonce (ssh-make-nonce (mem-ref (+ ssh #x04) :u32))))
    (let ((k1-base (+ ssh #x090))
          (k2-base (+ ssh #x0B0)))
      (dotimes (i 32)
        (aset k1 i (mem-ref (+ k1-base i) :u8))
        (aset k2 i (mem-ref (+ k2-base i) :u8))))
    (let ((len-ks (chacha-block k2 nonce 0))
          (packet-len 0))
      (dotimes (i 4)
        (let ((di (aref data i))
              (ki (aref len-ks i)))
          (let ((b (logxor di ki)))
            (setq packet-len (logior (ash packet-len 8) b)))))
      ;; Bounds check: packet-len can overflow 31-bit fixnums on ARM32
      ;; via (logior (ash packet-len 8) b), producing negative values
      (when (< packet-len 4) (return ()))
      (when (> packet-len 35000) (return ()))
      (when (< data-len (+ (+ 4 packet-len) 16)) (return ()))
      (let ((poly-ks (chacha-block k1 nonce 0))
            (poly-key (make-array 32)))
        (dotimes (i 32) (aset poly-key i (aref poly-ks i)))
        (let ((mac-input (make-array (+ 4 packet-len))))
          (dotimes (i (+ 4 packet-len))
            (aset mac-input i (aref data i)))
          (let ((expected (poly1305 poly-key mac-input (+ 4 packet-len)))
                (tag-ok 1))
            (let ((tag-off (+ 4 packet-len)))
              (dotimes (i 16)
                (unless (eq (aref data (+ tag-off i)) (aref expected i))
                  (setq tag-ok 0))))
            (when (zerop tag-ok)
              (return ()))
            (let ((enc-data (make-array packet-len)))
              (dotimes (i packet-len)
                (aset enc-data i (aref data (+ 4 i))))
              (let ((plain (chacha20-crypt k1 nonce enc-data packet-len 1)))
                (let ((pad-len (aref plain 0)))
                  (let ((payload-len (- (- packet-len pad-len) 1)))
                    (let ((payload (make-array payload-len)))
                      (dotimes (i payload-len)
                        (aset payload i (aref plain (+ 1 i))))
                      (setf (mem-ref (+ ssh #x04) :u32) (+ seq 1))
                      (let ((cb (- ssh #x20)))
                        (setf (mem-ref (+ cb #x16F8) :u32)
                              (- data-len (+ (+ 4 packet-len) 16)))
                        (cons payload payload-len)))))))))))))

;; Helper: write big-endian u32 to array at pos, return pos+4
(defun ssh-eh-write-u32 (buf pos val)
  (aset buf pos (logand (ash val -24) #xFF))
  (aset buf (+ pos 1) (logand (ash val -16) #xFF))
  (aset buf (+ pos 2) (logand (ash val -8) #xFF))
  (aset buf (+ pos 3) (logand val #xFF))
  (+ pos 4))

;; Helper: write SSH string (4-byte len + data from memory) to buf at pos
(defun ssh-eh-write-mem (buf pos src len)
  (let ((p (ssh-eh-write-u32 buf pos len)))
    (dotimes (i len)
      (aset buf (+ p i) (mem-ref (+ src i) :u8)))
    (+ p len)))

;; Helper: write SSH string (4-byte len + data from array) to buf at pos
(defun ssh-eh-write-arr (buf pos src len)
  (let ((p (ssh-eh-write-u32 buf pos len)))
    (dotimes (i len)
      (aset buf (+ p i) (aref src i)))
    (+ p len)))

;; ================================================================
;; SHA-256 overrides for i386
;; Original sha256-block has 8-var let + 8 nested lets in round loop
;; = 17+ nested lets, which miscompiles on i386 MVM translator.
;; Fix: store working state in array, flatten round computation.
;; ================================================================

;; Read w32 pair from state array (4 bytes per w32, big-endian)
(defun sha256-st-get (st idx)
  (let ((off (* idx 4)))
    (w32-from-be st off)))

;; Write w32 pair to state array
(defun sha256-st-set (st idx val)
  (let ((off (* idx 4)))
    (w32-to-be st off val)))

;; One SHA-256 round: reads/writes working vars from st[0..7]
;; st = 32-byte array (a=0, b=1, c=2, d=3, e=4, f=5, g=6, h=7)
(defun sha256-one-round (w st i)
  (let ((ee (sha256-st-get st 4)))
    (let ((bsig1 (sha256-bsig1 ee)))
      (let ((ch-val (sha256-ch ee (sha256-st-get st 5) (sha256-st-get st 6))))
        (let ((ki (sha256-load-k i)))
          (let ((wi (w32-wload w (* i 4))))
            (let ((t1 (w32+ (sha256-st-get st 7) (w32+ bsig1 (w32+ ch-val (w32+ ki wi))))))
              (let ((aa (sha256-st-get st 0)))
                (let ((t2 (w32+ (sha256-bsig0 aa) (sha256-maj aa (sha256-st-get st 1) (sha256-st-get st 2)))))
                  ;; Shift: h=g, g=f, f=e, e=d+t1, d=c, c=b, b=a, a=t1+t2
                  (sha256-st-set st 7 (sha256-st-get st 6))
                  (sha256-st-set st 6 (sha256-st-get st 5))
                  (sha256-st-set st 5 ee)
                  (sha256-st-set st 4 (w32+ (sha256-st-get st 3) t1))
                  (sha256-st-set st 3 (sha256-st-get st 2))
                  (sha256-st-set st 2 (sha256-st-get st 1))
                  (sha256-st-set st 1 aa)
                  (sha256-st-set st 0 (w32+ t1 t2)))))))))))

;; Override sha256-block: use array-based working state
(defun sha256-block (block bo h)
  (let ((w (make-array 256)))
    ;; Expand W[0..15]
    (dotimes (i 16)
      (let ((j (+ bo (* i 4))))
        (w32-wstore w (* i 4) (w32-from-be block j))))
    ;; Expand W[16..63]
    (let ((i 16))
      (loop
        (when (not (< i 64)) (return ()))
        (let ((s0 (sha256-lsig0 (w32-wload w (* (- i 15) 4)))))
          (let ((s1 (sha256-lsig1 (w32-wload w (* (- i 2) 4)))))
            (let ((w7 (w32-wload w (* (- i 7) 4))))
              (let ((w16 (w32-wload w (* (- i 16) 4))))
                (w32-wstore w (* i 4)
                  (w32+ s1 (w32+ w7 (w32+ s0 w16))))))))
        (setq i (+ i 1))))
    ;; Initialize working state from h
    (let ((st (make-array 32)))
      (dotimes (i 32) (aset st i (aref h i)))
      ;; 64 rounds
      (dotimes (i 64) (sha256-one-round w st i))
      ;; Add back to hash state
      (dotimes (idx 8)
        (let ((off (* idx 4)))
          (w32-to-be h off (w32+ (w32-from-be h off) (sha256-st-get st idx))))))))

;; Print w32 pair as 8 hex chars
(defun print-w32 (w)
  (print-hex-byte (logand (ash (car w) -8) #xFF))
  (print-hex-byte (logand (car w) #xFF))
  (print-hex-byte (logand (ash (cdr w) -8) #xFF))
  (print-hex-byte (logand (cdr w) #xFF)))

;; SHA-256 diagnostic: test K[0], W[0], ash, rotations, BSIG1
(defun sha256-diag ()
  ;; Test K[0] = 0x428A2F98
  (%serial-byte 75) (%serial-byte 48) (%serial-byte 58) ;; K0:
  (print-w32 (sha256-load-k 0))
  (%serial-byte 10)
  ;; Test w32-from-be on padded[0..3] = [0x80, 0, 0, 0]
  (let ((p (make-array 4)))
    (aset p 0 #x80) (aset p 1 0) (aset p 2 0) (aset p 3 0)
    (%serial-byte 87) (%serial-byte 48) (%serial-byte 58) ;; W0:
    (print-w32 (w32-from-be p 0))
    (%serial-byte 10))
  ;; Test ash with variable count: (ash 100 3) should be 800
  (let ((v 100) (n 3))
    (%serial-byte 65) (%serial-byte 49) (%serial-byte 58) ;; A1:
    (print-dec (ash v n))
    (%serial-byte 10))
  ;; Test ash with negative variable: (ash 100 -3) should be 12
  (let ((v 100) (n 3))
    (%serial-byte 65) (%serial-byte 50) (%serial-byte 58) ;; A2:
    (print-dec (ash v (- 0 n)))
    (%serial-byte 10))
  ;; Test w32-rotr-small step by step: rotr(0x510E527F, 6)
  ;; Expected: 0xFD443949
  (let ((x (cons #x510e #x527f)))
    (let ((hi (car x)) (lo (cdr x)))
      ;; hi >> 6 = 0x510E >> 6 = 0x0144
      (let ((hi-shr (ash hi (- 0 6))))
        (%serial-byte 82) (%serial-byte 49) (%serial-byte 58) ;; R1: hi>>6
        (print-dec hi-shr)
        (%serial-byte 10))
      ;; lo & 63 = 0x527F & 63 = 0x3F
      (let ((lo-bits (logand lo 63)))
        (%serial-byte 82) (%serial-byte 50) (%serial-byte 58) ;; R2: lo&mask
        (print-dec lo-bits)
        (%serial-byte 10))
      ;; (lo & 63) << 10 = 0x3F << 10 = 0xFC00
      (let ((lo-bits (logand lo 63)))
        (let ((lo-shl (ash lo-bits 10)))
          (%serial-byte 82) (%serial-byte 51) (%serial-byte 58) ;; R3: lo_bits<<10
          (print-dec lo-shl)
          (%serial-byte 10)))
      ;; Full w32-rotr-small(x, 6, 63, 10) result
      (%serial-byte 82) (%serial-byte 54) (%serial-byte 58) ;; R6:
      (print-w32 (w32-rotr-small x 6 63 10))
      (%serial-byte 10)))
  ;; Test BSIG1(510e527f) — expected: 3a6fe67d
  (let ((e-val (cons #x510e #x527f)))
    (let ((bs1 (sha256-bsig1 e-val)))
      (%serial-byte 66) (%serial-byte 49) (%serial-byte 58) ;; B1:
      (print-w32 bs1)
      (%serial-byte 10))))

;; Override sha256: original uses 3-arg + with (if ...) (broken on MVM i386)
(defun sha256 (msg)
  (let ((msg-len (array-length msg)))
    (let ((r (mod (+ msg-len 9) 64)))
      (let ((pad (if (zerop r) 0 (- 64 r))))
        (let ((total (+ (+ msg-len 9) pad)))
          (let ((padded (make-array total)))
            (dotimes (i total) (aset padded i 0))
            (dotimes (i msg-len) (aset padded i (aref msg i)))
            (aset padded msg-len #x80)
            (let ((bits (* msg-len 8)))
              (aset padded (- total 4) (logand (ash bits -24) #xFF))
              (aset padded (- total 3) (logand (ash bits -16) #xFF))
              (aset padded (- total 2) (logand (ash bits -8) #xFF))
              (aset padded (- total 1) (logand bits #xFF)))
            (let ((h (make-array 32)))
              (aset h 0 #x6a) (aset h 1 #x09) (aset h 2 #xe6) (aset h 3 #x67)
              (aset h 4 #xbb) (aset h 5 #x67) (aset h 6 #xae) (aset h 7 #x85)
              (aset h 8 #x3c) (aset h 9 #x6e) (aset h 10 #xf3) (aset h 11 #x72)
              (aset h 12 #xa5) (aset h 13 #x4f) (aset h 14 #xf5) (aset h 15 #x3a)
              (aset h 16 #x51) (aset h 17 #x0e) (aset h 18 #x52) (aset h 19 #x7f)
              (aset h 20 #x9b) (aset h 21 #x05) (aset h 22 #x68) (aset h 23 #x8c)
              (aset h 24 #x1f) (aset h 25 #x83) (aset h 26 #xd9) (aset h 27 #xab)
              (aset h 28 #x5b) (aset h 29 #xe0) (aset h 30 #xcd) (aset h 31 #x19)
              (let ((offset 0))
                (loop
                  (when (not (< offset total)) (return ()))
                  (sha256-block padded offset h)
                  (setq offset (+ offset 64))))
              h)))))))

;; Override ssh-compute-exchange-hash: original has 24 nested lets (miscompiles)
;; Flat buffer-based approach: max 8 nested lets
(defun ssh-compute-exchange-hash (ssh cli-eph srv-eph shared-secret)
  (sha256-init)
  (let ((cb (- ssh #x20))
        (buf (make-array 2048))
        (pos 0))
    ;; V_C: client version string
    (let ((vc-len (mem-ref (+ ssh #x6D0) :u32)))
      (setq pos (ssh-eh-write-mem buf pos (+ ssh #x650) vc-len)))
    ;; V_S: server version "SSH-2.0-Modus_1.0" (17 bytes)
    (let ((vs (make-array 17)))
      (aset vs 0 83) (aset vs 1 83) (aset vs 2 72) (aset vs 3 45)
      (aset vs 4 50) (aset vs 5 46) (aset vs 6 48) (aset vs 7 45)
      (aset vs 8 77) (aset vs 9 111) (aset vs 10 100) (aset vs 11 117)
      (aset vs 12 115) (aset vs 13 95) (aset vs 14 49) (aset vs 15 46)
      (aset vs 16 48)
      (setq pos (ssh-eh-write-arr buf pos vs 17)))
    ;; I_C: client kexinit
    (let ((ic-len (mem-ref (+ ssh #x20) :u32)))
      (setq pos (ssh-eh-write-mem buf pos (+ cb #x1F00) ic-len)))
    ;; I_S: server kexinit
    (let ((is-len (mem-ref (+ ssh #x1C) :u32)))
      (setq pos (ssh-eh-write-mem buf pos (+ cb #x1700) is-len)))
    ;; K_S: host key blob
    (let ((hk-enc (ssh-encode-host-key ssh)))
      (setq pos (ssh-eh-write-arr buf pos hk-enc (array-length hk-enc))))
    ;; Q_C: client ephemeral public key
    (setq pos (ssh-eh-write-arr buf pos cli-eph 32))
    ;; Q_S: server ephemeral public key
    (setq pos (ssh-eh-write-arr buf pos srv-eph 32))
    ;; K: shared secret as mpint (raw bytes, not SSH string)
    (let ((k-mpint (ssh-make-mpint shared-secret)))
      (let ((klen (array-length k-mpint)))
        (dotimes (i klen)
          (aset buf (+ pos i) (aref k-mpint i)))
        (setq pos (+ pos klen))))
    ;; Trim buffer and hash
    (let ((input (make-array pos)))
      (dotimes (i pos)
        (aset input i (aref buf i)))
      (sha256 input))))

;; Override ssh-receive-version: original uses (let ((end 0) ...)) where
;; 0 is truthy (not nil) on MVM bare metal, breaking the \n scan.
;; Fix: use -1 as sentinel (always < 0, so (when (>= end 0) ...) works).
(defun ssh-receive-version (ssh)
  (let ((got-version 0) (tries 0))
    (loop
      (when (not (zerop got-version)) (return 1))
      (when (> tries 50) (return 0))
      (let ((msg (receive)))
        (when (zerop msg) (return 0)))
      (let ((blen (mem-ref (+ ssh #x6D4) :u32)))
        (when (> blen 8)
          (when (eq (mem-ref (+ ssh #x6D8) :u8) 83)
            (when (eq (mem-ref (+ ssh #x6D9) :u8) 83)
              (when (eq (mem-ref (+ ssh #x6DA) :u8) 72)
                ;; Scan for \n starting at position 3
                (let ((end (- 0 1)) (i 3))
                  (loop
                    (when (>= end 0) (return 0))
                    (when (> i blen) (return 0))
                    (when (eq (mem-ref (+ (+ ssh #x6D8) i) :u8) 10)
                      (setq end i))
                    (setq i (+ i 1)))
                  (when (>= end 0)
                    (let ((vlen end))
                      (when (> end 0)
                        (when (eq (mem-ref (+ (+ ssh #x6D8) (- end 1)) :u8) 13)
                          (setq vlen (- end 1))))
                      (dotimes (j vlen)
                        (setf (mem-ref (+ (+ ssh #x650) j) :u8)
                              (mem-ref (+ (+ ssh #x6D8) j) :u8)))
                      (setf (mem-ref (+ ssh #x6D0) :u32) vlen)
                      (ssh-buf-consume ssh (+ end 1))
                      (setq got-version 1)))))))))
      (setq tries (+ tries 1)))))

;; Override ssh-receive-packet: original uses (zerop result) to check nil,
;; but on i386 bare metal nil=0x09 not fixnum 0.
;; Fix: use (not result) instead of (zerop result) for nil checks.
(defun ssh-receive-packet (ssh timeout)
  (let ((encrypted (mem-ref (+ ssh #x0C) :u32))
        (cb (- ssh #x20)))
    (if (zerop encrypted)
        (let ((tries 0) (result ()))
          (loop
            (when result (return result))
            (when (> tries timeout) (return 0))
            (let ((blen (mem-ref (+ ssh #x6D4) :u32)))
              (if (> blen 4)
                  (let ((arr (ssh-buf-to-array ssh blen)))
                    (let ((pkt-len (ssh-get-u32 arr 0)))
                      (if (not (< blen (+ 4 pkt-len)))
                          (let ((parsed (ssh-parse-packet ssh arr blen)))
                            (when parsed
                              (ssh-buf-consume ssh (+ 4 pkt-len))
                              (setf (mem-ref (+ ssh #x04) :u32)
                                    (+ (mem-ref (+ ssh #x04) :u32) 1))
                              (setq result parsed)))
                          (let ((msg (receive)))
                            (when (zerop msg) (return 0))))))
                  (let ((msg (receive)))
                    (when (zerop msg) (return 0)))))
            (setq tries (+ tries 1))))
        (let ((tries 0) (result ()))
          (loop
            (when result (return result))
            (when (> tries timeout) (return 0))
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
                            (when (zerop msg) (return 0))))))
                  (let ((msg (receive)))
                    (when (zerop msg) (return 0)))))
            (setq tries (+ tries 1)))))))

;; ================================================================
;; fe-carry / fe-reduce overrides
;; Original fe-carry has let* with 34 bindings → 34 nested lets
;; which miscompiles on i386 (18+ nested let limit).
;; Fix: split into fe-carry-phase1 (limbs 0-4) and fe-carry-phase2 (5-9).
;; ================================================================
;; ed-reduce-scalar override: original has 19+ nested scopes (miscompiles)
;; Split into helper functions to keep nesting under 15.
;; ================================================================

;; Check if 64-byte x array fits in 252 bits (bytes 32+ all zero, byte 31 < 0x10)
(defun ed-reduce-fits (x)
  (let ((fits 1) (j 32))
    (loop
      (if (< j 64)
          (progn
            (unless (zerop (aref x j)) (setq fits 0))
            (setq j (+ j 1)))
          (return ())))
    (when fits
      (unless (< (aref x 31) #x10) (setq fits 0)))
    fits))

;; Extract x_high (bits 252+) from x into xhigh (33 bytes)
(defun ed-reduce-extract-high (x xhigh)
  (dotimes (i 33)
    (let ((src (+ 31 i)))
      (let ((lo (if (< src 64) (ash (aref x src) -4) 0)))
        (let ((hi (if (< (+ src 1) 64)
                      (logand (ash (aref x (+ src 1)) 4) #xF0)
                      0)))
          (aset xhigh i (logior lo hi)))))))

;; Multiply one byte of xhigh by c, accumulate into prod starting at offset i
(defun ed-reduce-mul-byte (prod xhigh i)
  (let ((hi-byte (aref xhigh i)))
    (unless (zerop hi-byte)
      (let ((carry 0))
        (dotimes (j 16)
          (let ((k (+ i j)))
            (let ((cj (ed-c-byte j)))
              (let ((pp (+ (* hi-byte cj) (+ (aref prod k) carry))))
                (aset prod k (logand pp #xFF))
                (setq carry (ash pp -8))))))
        ;; Propagate carry
        (let ((k2 (+ i 16)))
          (loop
            (if (< k2 50)
                (if (zerop carry)
                    (return ())
                    (let ((s (+ (aref prod k2) carry)))
                      (aset prod k2 (logand s #xFF))
                      (setq carry (ash s -8))
                      (setq k2 (+ k2 1))))
                (return ()))))))))

;; Compare xlow vs prod (50 bytes), return 1 if xlow >= prod
(defun ed-reduce-compare (xlow prod)
  (let ((ge 1) (ci 49))
    (loop
      (if (< ci 0) (return ())
          (let ((pb (aref prod ci)))
            (let ((xlb (if (< ci 32) (aref xlow ci) 0)))
              (if (< xlb pb)
                  (progn (setq ge 0) (return ()))
                  (if (< pb xlb)
                      (return ())
                      (setq ci (- ci 1))))))))
    ge))

;; Subtract: x = xlow - prod (50 bytes), return 1 if negated
(defun ed-reduce-sub-fwd (x xlow prod)
  (let ((borrow 0))
    (dotimes (i 64) (aset x i 0))
    (dotimes (i 50)
      (let ((xlb (if (< i 32) (aref xlow i) 0)))
        (let ((pb (aref prod i)))
          (let ((diff (- (- xlb pb) borrow)))
            (if (< diff 0)
                (progn (aset x i (+ diff 256)) (setq borrow 1))
                (progn (aset x i diff) (setq borrow 0)))))))
    0))

;; Subtract: x = prod - xlow (50 bytes)
(defun ed-reduce-sub-rev (x xlow prod)
  (let ((borrow 0))
    (dotimes (i 64) (aset x i 0))
    (dotimes (i 50)
      (let ((xlb (if (< i 32) (aref xlow i) 0)))
        (let ((pb (aref prod i)))
          (let ((diff (- (- pb xlb) borrow)))
            (if (< diff 0)
                (progn (aset x i (+ diff 256)) (setq borrow 1))
                (progn (aset x i diff) (setq borrow 0)))))))
    1))

;; Final: subtract L from out while out >= L
(defun ed-reduce-sub-l (out)
  (let ((done nil))
    (loop
      (when done (return ()))
      (let ((ge-l 1) (ci 31))
        (loop
          (if (< ci 0) (return ())
              (let ((ob (aref out ci)))
                (let ((lb (ed-l-byte ci)))
                  (if (< lb ob) (return ())
                      (if (< ob lb)
                          (progn (setq ge-l nil) (return ()))
                          (setq ci (- ci 1))))))))
        (if ge-l
            (let ((borrow 0))
              (dotimes (i 32)
                (let ((oi (aref out i)))
                  (let ((li (ed-l-byte i)))
                    (let ((diff (- (- oi li) borrow)))
                      (if (< diff 0)
                          (progn (aset out i (+ diff 256)) (setq borrow 1))
                          (progn (aset out i diff) (setq borrow 0))))))))
            (setq done 1))))))

;; Negate: out = L - out
(defun ed-reduce-negate (out)
  (let ((is-zero 1))
    (dotimes (i 32)
      (unless (zerop (aref out i)) (setq is-zero nil)))
    (unless is-zero
      (let ((borrow 0))
        (dotimes (i 32)
          (let ((li (ed-l-byte i)))
            (let ((oi (aref out i)))
              (let ((diff (- (- li oi) borrow)))
                (if (< diff 0)
                    (progn (aset out i (+ diff 256)) (setq borrow 1))
                    (progn (aset out i diff) (setq borrow 0)))))))))))

;; Main ed-reduce-scalar: split into helpers to avoid 18+ nested lets
(defun ed-reduce-scalar (hash)
  (let ((x (make-array 64))
        (xlow (make-array 32))
        (xhigh (make-array 36))
        (prod (make-array 52))
        (negated 0))
    ;; Copy input
    (dotimes (i 64) (aset x i (aref hash i)))
    ;; Iterate reduction
    (let ((iter 0))
      (loop
        (when (>= iter 10) (return ()))
        (when (not (zerop (ed-reduce-fits x))) (return ()))
        ;; Clear product
        (dotimes (i 52) (aset prod i 0))
        ;; Extract x_low
        (dotimes (i 31) (aset xlow i (aref x i)))
        (aset xlow 31 (logand (aref x 31) #x0F))
        ;; Extract x_high
        (ed-reduce-extract-high x xhigh)
        ;; Multiply x_high * c
        (dotimes (i 33) (ed-reduce-mul-byte prod xhigh i))
        ;; Compare and subtract
        (if (not (zerop (ed-reduce-compare xlow prod)))
            (ed-reduce-sub-fwd x xlow prod)
            (progn
              (setq negated (logxor negated 1))
              (ed-reduce-sub-rev x xlow prod)))
        (setq iter (+ iter 1))))
    ;; Copy to output
    (let ((out (make-array 32)))
      (dotimes (i 32) (aset out i (aref x i)))
      ;; Subtract L while out >= L
      (ed-reduce-sub-l out)
      ;; Negate if needed
      (when (not (zerop negated))
        (ed-reduce-negate out))
      out)))

;; ================================================================

;; Carry-propagate one even limb (26-bit), add carry-in, write back.
;; Returns carry-out.
(defun fe-carry-even (h off cin)
  (let ((v (+ (buf-read-u32 h off) cin)))
    (let ((c (ash v -26)))
      (buf-write-u32 h off (logand v #x3FFFFFF))
      c)))

;; Carry-propagate one odd limb (25-bit), add carry-in, write back.
;; Returns carry-out.
(defun fe-carry-odd (h off cin)
  (let ((v (+ (buf-read-u32 h off) cin)))
    (let ((c (ash v -25)))
      (buf-write-u32 h off (logand v #x1FFFFFF))
      c)))

(defun fe-carry (h)
  (let ((c (fe-carry-even h 0 0)))
    (setq c (fe-carry-odd h 4 c))
    (setq c (fe-carry-even h 8 c))
    (setq c (fe-carry-odd h 12 c))
    (setq c (fe-carry-even h 16 c))
    (setq c (fe-carry-odd h 20 c))
    (setq c (fe-carry-even h 24 c))
    (setq c (fe-carry-odd h 28 c))
    (setq c (fe-carry-even h 32 c))
    (setq c (fe-carry-odd h 36 c))
    ;; Wrap: c * 19 back to limb 0
    (let ((r0w (+ (buf-read-u32 h 0) (* c 19))))
      (let ((c0b (ash r0w -26)))
        (buf-write-u32 h 0 (logand r0w #x3FFFFFF))
        (buf-write-u32 h 4 (+ (buf-read-u32 h 4) c0b)))))
  h)

;; fe-reduce: carry twice then conditional reduction
;; Split into helpers to avoid deep nesting
(defun fe-reduce-check (h)
  ;; Try h + 19, carry through all limbs
  ;; Returns carry out of limb 9 (nonzero means h >= p)
  (let ((t0 (+ (buf-read-u32 h 0) 19)))
    (let ((c0 (ash t0 -26)))
      (let ((t1 (+ (buf-read-u32 h 4) c0)))
        (let ((c1 (ash t1 -25)))
          (let ((t2 (+ (buf-read-u32 h 8) c1)))
            (let ((c2 (ash t2 -26)))
              (let ((t3 (+ (buf-read-u32 h 12) c2)))
                (let ((c3 (ash t3 -25)))
                  (fe-reduce-check2 h c3))))))))))

(defun fe-reduce-check2 (h c3)
  (let ((t4 (+ (buf-read-u32 h 16) c3)))
    (let ((c4 (ash t4 -26)))
      (let ((t5 (+ (buf-read-u32 h 20) c4)))
        (let ((c5 (ash t5 -25)))
          (let ((t6 (+ (buf-read-u32 h 24) c5)))
            (let ((c6 (ash t6 -26)))
              (let ((t7 (+ (buf-read-u32 h 28) c6)))
                (let ((c7 (ash t7 -25)))
                  (fe-reduce-check3 h c7))))))))))

(defun fe-reduce-check3 (h c7)
  (let ((t8 (+ (buf-read-u32 h 32) c7)))
    (let ((c8 (ash t8 -26)))
      (let ((t9 (+ (buf-read-u32 h 36) c8)))
        (ash t9 -25)))))

(defun fe-reduce-apply (h)
  ;; Apply h + 19 and write reduced values
  (let ((t0 (+ (buf-read-u32 h 0) 19)))
    (let ((c0 (ash t0 -26)))
      (buf-write-u32 h 0 (logand t0 #x3FFFFFF))
      (let ((t1 (+ (buf-read-u32 h 4) c0)))
        (let ((c1 (ash t1 -25)))
          (buf-write-u32 h 4 (logand t1 #x1FFFFFF))
          (let ((t2 (+ (buf-read-u32 h 8) c1)))
            (let ((c2 (ash t2 -26)))
              (buf-write-u32 h 8 (logand t2 #x3FFFFFF))
              (fe-reduce-apply2 h c2))))))))

(defun fe-reduce-apply2 (h c2)
  (let ((t3 (+ (buf-read-u32 h 12) c2)))
    (let ((c3 (ash t3 -25)))
      (buf-write-u32 h 12 (logand t3 #x1FFFFFF))
      (let ((t4 (+ (buf-read-u32 h 16) c3)))
        (let ((c4 (ash t4 -26)))
          (buf-write-u32 h 16 (logand t4 #x3FFFFFF))
          (let ((t5 (+ (buf-read-u32 h 20) c4)))
            (let ((c5 (ash t5 -25)))
              (buf-write-u32 h 20 (logand t5 #x1FFFFFF))
              (fe-reduce-apply3 h c5))))))))

(defun fe-reduce-apply3 (h c5)
  (let ((t6 (+ (buf-read-u32 h 24) c5)))
    (let ((c6 (ash t6 -26)))
      (buf-write-u32 h 24 (logand t6 #x3FFFFFF))
      (let ((t7 (+ (buf-read-u32 h 28) c6)))
        (let ((c7 (ash t7 -25)))
          (buf-write-u32 h 28 (logand t7 #x1FFFFFF))
          (let ((t8 (+ (buf-read-u32 h 32) c7)))
            (let ((c8 (ash t8 -26)))
              (buf-write-u32 h 32 (logand t8 #x3FFFFFF))
              (let ((t9 (+ (buf-read-u32 h 36) c8)))
                (buf-write-u32 h 36 (logand t9 #x1FFFFFF))))))))))

(defun fe-reduce (h)
  (fe-carry h)
  (fe-carry h)
  (when (not (zerop (fe-reduce-check h)))
    (fe-reduce-apply h))
  h)

;; Override w32-rotr-small: original has deeply nested exprs in cons args
;; that miscompile on i386 due to register pressure. Flatten with let bindings.
(defun w32-rotr-small (x n mask shift)
  (let ((hi (car x)) (lo (cdr x)))
    (let ((hi-shr (ash hi (- 0 n))))
      (let ((lo-bits (logand lo mask)))
        (let ((lo-shl (ash lo-bits shift)))
          (let ((new-hi (logand (logior hi-shr lo-shl) #xFFFF)))
            (let ((lo-shr (ash lo (- 0 n))))
              (let ((hi-bits (logand hi mask)))
                (let ((hi-shl (ash hi-bits shift)))
                  (let ((new-lo (logand (logior lo-shr hi-shl) #xFFFF)))
                    (cons new-hi new-lo)))))))))))

;; Override w32-rotr-big: same nesting issue
(defun w32-rotr-big (x n2 mask shift)
  (let ((hi (cdr x)) (lo (car x)))
    (let ((hi-shr (ash hi (- 0 n2))))
      (let ((lo-bits (logand lo mask)))
        (let ((lo-shl (ash lo-bits shift)))
          (let ((new-hi (logand (logior hi-shr lo-shl) #xFFFF)))
            (let ((lo-shr (ash lo (- 0 n2))))
              (let ((hi-bits (logand hi mask)))
                (let ((hi-shl (ash hi-bits shift)))
                  (let ((new-lo (logand (logior lo-shr hi-shl) #xFFFF)))
                    (cons new-hi new-lo)))))))))))

;; Override w32-shr-small: flatten nested expressions
(defun w32-shr-small (x n)
  (let ((hi (car x)) (lo (cdr x)))
    (let ((new-hi (ash hi (- 0 n))))
      (let ((lo-shr (ash lo (- 0 n))))
        (let ((hi-bits (logand hi (- (ash 1 n) 1))))
          (let ((hi-shl (ash hi-bits (- 16 n))))
            (let ((new-lo (logand (logior lo-shr hi-shl) #xFFFF)))
              (cons new-hi new-lo))))))))

;; Override w32-shl-small: flatten
(defun w32-shl-small (x n)
  (let ((hi (car x)) (lo (cdr x)))
    (let ((lo-bits (ash lo (- n 16))))
      (let ((new-hi (logand (logior (ash hi n) lo-bits) #xFFFF)))
        (let ((new-lo (logand (ash lo n) #xFFFF)))
          (cons new-hi new-lo))))))

;; Stub out actor primitives (single-threaded on i386)
(defun spin-lock (addr) 0)
(defun spin-unlock (addr) 0)
(defun yield () 0)
(defun actor-spawn (fn) 0)
(defun actor-exit () 0)

;; ssh-random — xorshift PRNG with 30-bit mask (not 32-bit 0xFFFFFFFF)
(defun ssh-random (ssh)
  (let ((s (mem-ref (+ ssh #x2C) :u32)))
    (when (zerop s) (setq s 12345))
    (setq s (logxor s (logand (ash s 13) #x3FFFFFFF)))
    (setq s (logxor s (ash s -17)))
    (setq s (logxor s (logand (ash s 5) #x3FFFFFFF)))
    (setf (mem-ref (+ ssh #x2C) :u32) s)
    (logand s #xFF)))

;; htonl — byte-swap host to network order, 30-bit safe
;; Low byte of input becomes high byte of output, mask to 6 bits.
(defun htonl (v)
  (logior (ash (logand (logand v #xFF) #x3F) 24)
          (logior (ash (logand (ash v -8) #xFF) 16)
                  (logior (ash (logand (ash v -16) #xFF) 8)
                          (logand (ash v -24) #xFF)))))

;; ================================================================
;; Triple-based field multiplication for i386
;;
;; Pairs (hi . lo26) overflow because pmul19 output needs >30-bit hi.
;; Triples (hi . (mid . lo)) keep mid/lo normalized to ≤26 bits.
;; hi is allowed to grow (up to ~12 bits after accumulation).
;; ================================================================

;; Triple add: normalize lo and mid to 26 bits, carry propagates up
(defun tadd (a b)
  (let ((lo (+ (cddr a) (cddr b))))
    (let ((mid (+ (cadr a) (+ (cadr b) (ash lo -26)))))
      (cons (+ (car a) (+ (car b) (ash mid -26)))
            (cons (logand mid 67108863) (logand lo 67108863))))))

;; Promote pair (hi . lo) to triple (hi2 . (mid . lo))
;; Splits pair hi (up to 28 bits) into triple hi (≤2 bits) and mid (≤26 bits)
(defun pair-to-triple (p)
  (let ((hi (car p)))
    (cons (ash hi -26)
          (cons (logand hi 67108863) (cdr p)))))

;; Triple shift left 1: triple * 2
(defun tpshl1 (tr)
  (let ((lo (ash (cddr tr) 1)))
    (let ((mid (+ (ash (cadr tr) 1) (ash lo -26))))
      (cons (+ (ash (car tr) 1) (ash mid -26))
            (cons (logand mid 67108863) (logand lo 67108863))))))

;; Triple shift left 4: triple * 16
;; Safe when mid,lo ≤ 26 bits: (26-bit << 4) + 4-bit carry ≤ 2^30-1
(defun tpshl4 (tr)
  (let ((lo (ash (cddr tr) 4)))
    (let ((mid (+ (ash (cadr tr) 4) (ash lo -26))))
      (cons (+ (ash (car tr) 4) (ash mid -26))
            (cons (logand mid 67108863) (logand lo 67108863))))))

;; Triple * 19 = tr + 2*tr + 16*tr
(defun tmul19 (tr)
  (tadd tr (tadd (tpshl1 tr) (tpshl4 tr))))

;; Product as triple (no scaling)
(defun prod26 (a b)
  (pair-to-triple (mul26 a b)))

;; Product as triple, scaled by 19
(defun prod26-19 (a b)
  (tmul19 (pair-to-triple (mul26 a b))))

;; Accumulate 5 triples
(defun fe-h-5 (p0 p1 p2 p3 p4)
  (tadd p0 (tadd p1 (tadd p2 (tadd p3 p4)))))

;; Override all fe-hN to use tadd instead of padd for combining halves
(defun fe-h0 (f0 f1-2 f2 f3-2 f4 f5-2 f6 f7-2 f8 f9-2
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g0) (prod26-19 f1-2 g9) (prod26-19 f2 g8)
            (prod26-19 f3-2 g7) (prod26-19 f4 g6))
    (fe-h-5 (prod26-19 f5-2 g5) (prod26-19 f6 g4) (prod26-19 f7-2 g3)
            (prod26-19 f8 g2) (prod26-19 f9-2 g1))))

(defun fe-h1 (f0 f1 f2 f3 f4 f5 f6 f7 f8 f9
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g1) (prod26 f1 g0) (prod26-19 f2 g9)
            (prod26-19 f3 g8) (prod26-19 f4 g7))
    (fe-h-5 (prod26-19 f5 g6) (prod26-19 f6 g5) (prod26-19 f7 g4)
            (prod26-19 f8 g3) (prod26-19 f9 g2))))

(defun fe-h2 (f0 f1-2 f2 f3-2 f4 f5-2 f6 f7-2 f8 f9-2
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g2) (prod26 f1-2 g1) (prod26 f2 g0)
            (prod26-19 f3-2 g9) (prod26-19 f4 g8))
    (fe-h-5 (prod26-19 f5-2 g7) (prod26-19 f6 g6) (prod26-19 f7-2 g5)
            (prod26-19 f8 g4) (prod26-19 f9-2 g3))))

(defun fe-h3 (f0 f1 f2 f3 f4 f5 f6 f7 f8 f9
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g3) (prod26 f1 g2) (prod26 f2 g1)
            (prod26 f3 g0) (prod26-19 f4 g9))
    (fe-h-5 (prod26-19 f5 g8) (prod26-19 f6 g7) (prod26-19 f7 g6)
            (prod26-19 f8 g5) (prod26-19 f9 g4))))

(defun fe-h4 (f0 f1-2 f2 f3-2 f4 f5-2 f6 f7-2 f8 f9-2
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g4) (prod26 f1-2 g3) (prod26 f2 g2)
            (prod26 f3-2 g1) (prod26 f4 g0))
    (fe-h-5 (prod26-19 f5-2 g9) (prod26-19 f6 g8) (prod26-19 f7-2 g7)
            (prod26-19 f8 g6) (prod26-19 f9-2 g5))))

(defun fe-h5 (f0 f1 f2 f3 f4 f5 f6 f7 f8 f9
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g5) (prod26 f1 g4) (prod26 f2 g3)
            (prod26 f3 g2) (prod26 f4 g1))
    (fe-h-5 (prod26 f5 g0) (prod26-19 f6 g9) (prod26-19 f7 g8)
            (prod26-19 f8 g7) (prod26-19 f9 g6))))

(defun fe-h6 (f0 f1-2 f2 f3-2 f4 f5-2 f6 f7-2 f8 f9-2
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g6) (prod26 f1-2 g5) (prod26 f2 g4)
            (prod26 f3-2 g3) (prod26 f4 g2))
    (fe-h-5 (prod26 f5-2 g1) (prod26 f6 g0) (prod26-19 f7-2 g9)
            (prod26-19 f8 g8) (prod26-19 f9-2 g7))))

(defun fe-h7 (f0 f1 f2 f3 f4 f5 f6 f7 f8 f9
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g7) (prod26 f1 g6) (prod26 f2 g5)
            (prod26 f3 g4) (prod26 f4 g3))
    (fe-h-5 (prod26 f5 g2) (prod26 f6 g1) (prod26 f7 g0)
            (prod26-19 f8 g9) (prod26-19 f9 g8))))

(defun fe-h8 (f0 f1-2 f2 f3-2 f4 f5-2 f6 f7-2 f8 f9-2
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g8) (prod26 f1-2 g7) (prod26 f2 g6)
            (prod26 f3-2 g5) (prod26 f4 g4))
    (fe-h-5 (prod26 f5-2 g3) (prod26 f6 g2) (prod26 f7-2 g1)
            (prod26 f8 g0) (prod26-19 f9-2 g9))))

(defun fe-h9 (f0 f1 f2 f3 f4 f5 f6 f7 f8 f9
              g0 g1 g2 g3 g4 g5 g6 g7 g8 g9)
  (tadd
    (fe-h-5 (prod26 f0 g9) (prod26 f1 g8) (prod26 f2 g7)
            (prod26 f3 g6) (prod26 f4 g5))
    (fe-h-5 (prod26 f5 g4) (prod26 f6 g3) (prod26 f7 g2)
            (prod26 f8 g1) (prod26 f9 g0))))

;; Extract 26-bit limb from triple, return (carry_triple . limb26)
(defun textract26 (tr)
  ;; Extract 26-bit limb from triple, return (carry_triple . limb26).
  ;; Triple value = hi*2^52 + mid*2^26 + lo.
  ;; Limb = lo (already ≤ 26 bits from tadd normalization).
  ;; Carry scalar = hi*2^26 + mid (since lo>>26 = 0).
  ;; As triple: (0 . (hi . mid)).
  (cons (cons 0 (cons (car tr) (cadr tr)))
        (cddr tr)))

;; Extract 25-bit limb from triple, return (carry_triple . limb25)
(defun textract25 (tr)
  ;; Limb = lo & 0x1FFFFFF.
  ;; Carry scalar = hi*2^27 + mid*2 + (lo>>25).
  ;; As triple: (0 . (carry_mid . carry_lo)) where:
  ;;   carry_lo = ((mid & 0x1FFFFFF) * 2) + (lo >> 25)  [≤ 26 bits]
  ;;   carry_mid = hi * 2 + (mid >> 25)                  [small, fits in 26 bits]
  (let ((lo (cddr tr))
        (mid (cadr tr))
        (hi (car tr)))
    (let ((carry-lo (+ (* (logand mid 33554431) 2) (ash lo -25)))
          (carry-mid (+ (* hi 2) (ash mid -25))))
      (cons (cons 0 (cons carry-mid carry-lo))
            (logand lo 33554431)))))

;; Carry chain: triples → 26/25-bit limbs
;; Override fe-carry-lo32 to use triples
(defun fe-carry-lo32 (h0 h1 h2 h3 h4)
  (let ((e0 (textract26 h0)))
    (let ((h1b (tadd h1 (car e0))))
      (let ((e1 (textract25 h1b)))
        (let ((h2b (tadd h2 (car e1))))
          (let ((e2 (textract26 h2b)))
            (let ((h3b (tadd h3 (car e2))))
              (let ((e3 (textract25 h3b)))
                (let ((h4b (tadd h4 (car e3))))
                  (let ((e4 (textract26 h4b)))
                    (cons (car e4)
                      (cons (cdr e0) (cons (cdr e1) (cons (cdr e2)
                        (cons (cdr e3) (cons (cdr e4) nil))))))))))))))))

;; Override fe-carry-hi32 to use triples
(defun fe-carry-hi32 (c4 h5 h6 h7 h8 h9)
  (let ((h5b (tadd h5 c4)))
    (let ((e5 (textract25 h5b)))
      (let ((h6b (tadd h6 (car e5))))
        (let ((e6 (textract26 h6b)))
          (let ((h7b (tadd h7 (car e6))))
            (let ((e7 (textract25 h7b)))
              (let ((h8b (tadd h8 (car e7))))
                (let ((e8 (textract26 h8b)))
                  (let ((h9b (tadd h9 (car e8))))
                    (let ((e9 (textract25 h9b)))
                      (cons (car e9)
                        (cons (cdr e5) (cons (cdr e6) (cons (cdr e7)
                          (cons (cdr e8) (cons (cdr e9) nil)))))))))))))))))


;; Final c9 wrap-around: multiply carry triple by 19, add to r0/r1
;; Safe multiply-by-19 for values up to 26 bits: split to avoid overflow.
;; n*19 = n*16 + n*2 + n. Max: 67108863*19 = 1275068397 (overflows i386 fixnum).
;; Split: lo13 = n & 8191, hi13 = n >> 13. Each ≤ 13 bits.
;; n*19 = (hi13*19) << 13 + lo13*19. hi13*19 ≤ 8191*19 = 155629 (fits).
;; lo13*19 ≤ 8191*19 = 155629 (fits). Result is a pair (hi . lo26).
(defun safe-mul19 (n)
  (let ((lo13 (logand n 8191))
        (hi13 (ash n -13)))
    (let ((plo (* lo13 19))
          (phi (* hi13 19)))
      (let ((lo-raw (+ plo (* (logand phi 8191) 8192))))
        (let ((lo26 (logand lo-raw 67108863))
              (carry (ash lo-raw -26)))
          (cons (+ (ash phi -13) carry) lo26))))))

(defun fe-carry-wrap (dst c9t r0 r1 r2 r3 r4 r5 r6 r7 r8 r9)
  ;; Multiply each part of c9 triple by 19 using safe pair arithmetic
  ;; c9 value = hi*2^52 + mid*2^26 + lo
  ;; c9*19 wraps mod 2^255-19, adding to r0 (bits 0-25), r1 (bits 26-50), r2 (bits 52-77)
  (let ((lo19 (safe-mul19 (cddr c9t))))    ;; pair (hi . lo26)
    (let ((r0w (+ r0 (cdr lo19))))         ;; add lo19.lo to r0
      (let ((c0a (+ (car lo19) (ash r0w -26)))  ;; carry from r0 + lo19.hi
            (r0f (logand r0w 67108863)))
        (let ((mid19 (safe-mul19 (cadr c9t))))  ;; pair (hi . lo26)
          (let ((r1w (+ r1 (+ c0a (cdr mid19)))))  ;; add carry + mid19.lo to r1
            (let ((c1a (+ (car mid19) (ash r1w -26)))
                  (r1f (logand r1w 67108863)))
              (let ((hi19 (safe-mul19 (car c9t))))  ;; pair (hi . lo26)
                (let ((r2w (+ r2 (+ c1a (cdr hi19)))))
                  (fe-carry-write dst r0f r1f r2w r3 r4 r5 r6 r7 r8 r9))))))))))

;; Helper: extract hi-half results and finish carry
(defun fe-carry-finish (dst hi r0 r1 r2 r3 r4)
  (let ((c9t (car hi))
        (r5 (car (cdr hi)))
        (r6 (car (cdr (cdr hi))))
        (r7 (car (cdr (cdr (cdr hi)))))
        (r8 (car (cdr (cdr (cdr (cdr hi))))))
        (r9 (car (cdr (cdr (cdr (cdr (cdr hi))))))))
    (fe-carry-wrap dst c9t r0 r1 r2 r3 r4 r5 r6 r7 r8 r9)))

;; Override fe-carry32 to use triples (split to avoid 18+ nested lets)
(defun fe-carry32 (dst h0 h1 h2 h3 h4 h5 h6 h7 h8 h9)
  (let ((lo (fe-carry-lo32 h0 h1 h2 h3 h4)))
    (let ((c4 (car lo))
          (r0 (car (cdr lo)))
          (r1 (car (cdr (cdr lo))))
          (r2 (car (cdr (cdr (cdr lo)))))
          (r3 (car (cdr (cdr (cdr (cdr lo))))))
          (r4 (car (cdr (cdr (cdr (cdr (cdr lo))))))))
      (let ((hi (fe-carry-hi32 c4 h5 h6 h7 h8 h9)))
        (fe-carry-finish dst hi r0 r1 r2 r3 r4)))))

;; Debug override: ed-scalar-mult with progress markers
;;; Arena-based ed-scalar-mult: pre-allocate point storage,
;;; reset alloc pointer each byte to free triple garbage.
;;; VA (alloc pointer) is at physical address 0x600.
(defun ed-scalar-mult (scalar point)
  (let ((rx (make-array 40)) (ry (make-array 40))
        (rz (make-array 40)) (rt0 (make-array 40))
        (tx (make-array 40)) (ty (make-array 40))
        (tz (make-array 40)) (tt0 (make-array 40)))
    ;; Initialize result = identity point (0, 1, 1, 0)
    (fe-copy rx (fe-from-int 0))
    (fe-copy ry (fe-from-int 1))
    (fe-copy rz (fe-from-int 1))
    (fe-copy rt0 (fe-from-int 0))
    ;; Initialize temp = input point
    (fe-copy tx (car (car point)))
    (fe-copy ty (cdr (car point)))
    (fe-copy tz (car (cdr point)))
    (fe-copy tt0 (cdr (cdr point)))
    ;; Save arena after pre-allocations
    (let ((arena-save (get-alloc-ptr)))
      (dotimes (byte-idx 32)
        (let ((byte-val (aref scalar byte-idx)))
          (dotimes (bit-idx 8)
            (unless (zerop (logand byte-val (ash 1 bit-idx)))
              (let ((sum (ed-add (cons (cons rx ry) (cons rz rt0))
                                 (cons (cons tx ty) (cons tz tt0)))))
                (fe-copy rx (car (car sum)))
                (fe-copy ry (cdr (car sum)))
                (fe-copy rz (car (cdr sum)))
                (fe-copy rt0 (cdr (cdr sum)))))
            (let ((dbl (ed-double (cons (cons tx ty) (cons tz tt0)))))
              (fe-copy tx (car (car dbl)))
              (fe-copy ty (cdr (car dbl)))
              (fe-copy tz (car (cdr dbl)))
              (fe-copy tt0 (cdr (cdr dbl))))))
        ;; Reset arena — free all triple garbage from this byte
        (set-alloc-ptr arena-save)))
    (cons (cons rx ry) (cons rz rt0))))

;;; Arena-based x25519: reset alloc pointer each iteration.
;;; All working arrays are pre-allocated; only triple garbage accumulates.
(defun x25519 (k u)
  (let ((kc (make-array 32))
        (x1 (make-array 40))
        (x2 (make-array 40)) (z2 (make-array 40))
        (x3 (make-array 40)) (z3 (make-array 40))
        (a (make-array 40)) (aa (make-array 40))
        (b (make-array 40)) (bb (make-array 40))
        (fe-e (make-array 40)) (c (make-array 40))
        (d (make-array 40)) (da (make-array 40))
        (cb (make-array 40)) (t1 (make-array 40))
        (t2 (make-array 40)) (a24 (make-array 40))
        (swap 0) (pos 254))
    ;; Clamp scalar
    (dotimes (i 32) (aset kc i (aref k i)))
    (aset kc 0 (logand (aref kc 0) #xF8))
    (aset kc 31 (logand (aref kc 31) #x7F))
    (aset kc 31 (logior (aref kc 31) #x40))
    ;; Initialize
    (fe-copy x1 (fe-from-bytes u))
    (dotimes (i 40) (aset x2 i 0))
    (buf-write-u32 x2 0 1)
    (dotimes (i 40) (aset z2 i 0))
    (fe-copy x3 x1)
    (dotimes (i 40) (aset z3 i 0))
    (buf-write-u32 z3 0 1)
    (dotimes (i 40) (aset a24 i 0))
    (buf-write-u32 a24 0 121665)
    ;; Save arena after pre-allocations
    (let ((arena-save (get-alloc-ptr)))
      ;; Montgomery ladder: 255 iterations
      (dotimes (iter 255)
        (let ((byte-idx (ash pos -3))
              (bit-idx (logand pos 7)))
          (let ((byte-val (aref kc byte-idx))
                (mask (ash 1 bit-idx)))
            (let ((kt (if (zerop (logand byte-val mask)) 0 1)))
              (when (not (= kt swap))
                (fe-copy t1 x2) (fe-copy x2 x3) (fe-copy x3 t1)
                (fe-copy t1 z2) (fe-copy z2 z3) (fe-copy z3 t1))
              (setq swap kt)
              ;; Ladder step
              (fe-add a x2 z2)
              (fe-sq aa a)
              (fe-sub b x2 z2)
              (fe-sq bb b)
              (fe-sub fe-e aa bb)
              (fe-add c x3 z3)
              (fe-sub d x3 z3)
              (fe-mul da d a)
              (fe-mul cb c b)
              (fe-add t1 da cb)
              (fe-sq x3 t1)
              (fe-sub t1 da cb)
              (fe-sq t2 t1)
              (fe-mul z3 x1 t2)
              (fe-mul x2 aa bb)
              (fe-mul t1 a24 fe-e)
              (fe-add t2 aa t1)
              (fe-mul z2 fe-e t2))))
        (setq pos (- pos 1))
        ;; Reset arena — free triple garbage from this iteration
        (set-alloc-ptr arena-save))
      ;; Final swap
      (when (not (zerop swap))
        (fe-copy t1 x2) (fe-copy x2 x3) (fe-copy x3 t1)
        (fe-copy t1 z2) (fe-copy z2 z3) (fe-copy z3 t1))
      ;; Result = x2 * z2^(-1)
      (let ((zi (fe-invert z2)))
        (fe-mul t1 x2 zi)
        ;; Reset arena before final allocation (fe-to-bytes result)
        (set-alloc-ptr arena-save)
        (fe-to-bytes t1)))))

;;; fe-sq-iter without usb-keepalive (not needed on i386 NE2000)
(defun fe-sq-iter (dst n)
  (dotimes (i n) (fe-sq dst dst)))

;;; Arena-based fe-invert: exact same addition chain as crypto.lisp,
;;; with periodic arena resets to free triple garbage.
;;; Working arrays (z2, z9, z11, t0, t1) are allocated BEFORE arena-save
;;; so they survive resets.
(defun fe-invert (z)
  (let ((z2 (make-array 40))
        (z9 (make-array 40))
        (z11 (make-array 40))
        (t0 (make-array 40))
        (t1 (make-array 40)))
    (let ((arena-save (get-alloc-ptr)))
      (fe-sq z2 z)
      (fe-sq t0 z2)
      (fe-sq t1 t0)
      (fe-mul z9 z t1)
      (fe-mul z11 z2 z9)
      (fe-sq t0 z11)
      (fe-mul t0 z9 t0)
      (set-alloc-ptr arena-save)
      ;; z^(2^10-1)
      (fe-sq t1 t0) (fe-sq-iter t1 4)
      (fe-mul t1 t1 t0)
      (set-alloc-ptr arena-save)
      ;; z^(2^20-1)
      (fe-sq z2 t1) (fe-sq-iter z2 9)
      (fe-mul z2 z2 t1)
      (set-alloc-ptr arena-save)
      ;; z^(2^40-1)
      (fe-sq z9 z2) (fe-sq-iter z9 19)
      (fe-mul z9 z9 z2)
      (set-alloc-ptr arena-save)
      ;; z^(2^50-1)
      (fe-sq t0 z9) (fe-sq-iter t0 9)
      (fe-mul t0 t0 t1)
      (set-alloc-ptr arena-save)
      ;; z^(2^100-1)
      (fe-sq t1 t0) (fe-sq-iter t1 49)
      (fe-mul t1 t1 t0)
      (set-alloc-ptr arena-save)
      ;; z^(2^200-1)
      (fe-sq z2 t1) (fe-sq-iter z2 99)
      (fe-mul z2 z2 t1)
      (set-alloc-ptr arena-save)
      ;; z^(2^250-1)
      (fe-sq z9 z2) (fe-sq-iter z9 49)
      (fe-mul z9 z9 t0)
      (set-alloc-ptr arena-save)
      ;; z^(2^255-21) = z^(p-2)
      (fe-sq z9 z9) (fe-sq-iter z9 4)
      (fe-mul t0 z9 z11)
      t0)))

;; i386-safe poly-to-16: avoids (ash l3 6) overflow (26-bit << 6 = 32-bit)
(defun poly-to-16 (limbs result)
  (let ((l0 (buf-read-u32 limbs 0))
        (l1 (buf-read-u32 limbs 4))
        (l2 (buf-read-u32 limbs 8))
        (l3 (buf-read-u32 limbs 12))
        (l4 (buf-read-u32 limbs 16)))
    (aset result 0 (logand l0 #xFF))
    (aset result 1 (logand (ash l0 -8) #xFF))
    (aset result 2 (logand (ash l0 -16) #xFF))
    (aset result 3 (logand (logior (ash l0 -24) (ash l1 2)) #xFF))
    (aset result 4 (logand (ash l1 -6) #xFF))
    (aset result 5 (logand (ash l1 -14) #xFF))
    (aset result 6 (logand (logior (ash l1 -22) (ash l2 4)) #xFF))
    (aset result 7 (logand (ash l2 -4) #xFF))
    (aset result 8 (logand (ash l2 -12) #xFF))
    ;; byte 9: (ash l3 6) would overflow — pre-mask l3 to 2 bits
    (aset result 9 (logand (logior (ash l2 -20) (ash (logand l3 #x03) 6)) #xFF))
    (aset result 10 (logand (ash l3 -2) #xFF))
    (aset result 11 (logand (ash l3 -10) #xFF))
    (aset result 12 (logand (ash l3 -18) #xFF))
    (aset result 13 (logand l4 #xFF))
    (aset result 14 (logand (ash l4 -8) #xFF))
    (aset result 15 (logand (ash l4 -16) #xFF))))

;; i386-safe poly-mul: uses triple arithmetic for 26×26→52 bit products
;; Each d_i = sum of 5 products, accumulated as triples to avoid overflow.
;; Carry propagation via triple (hi, mid) pairs.
(defun poly-mul-acc (a0 a1 a2 a3 a4 r0 r1 r2 r3 r4)
  ;; Returns d0 as triple
  (tadd (tadd (prod26 a0 r0) (prod26 a1 r4))
        (tadd (prod26 a2 r3) (tadd (prod26 a3 r2) (prod26 a4 r1)))))

(defun poly-carry-step (a off d carry-hi carry-mid)
  ;; Add carry from previous limb, write 26-bit limb, return new carry
  (let ((dc (tadd d (cons 0 (cons carry-hi carry-mid)))))
    (buf-write-u32 a off (cddr dc))
    (cons (car dc) (cadr dc))))

(defun poly-mul (a r)
  (let ((a0 (buf-read-u32 a 0))
        (a1 (buf-read-u32 a 4))
        (a2 (buf-read-u32 a 8))
        (a3 (buf-read-u32 a 12))
        (a4 (buf-read-u32 a 16))
        (r0 (buf-read-u32 r 0))
        (r1 (buf-read-u32 r 4))
        (r2 (buf-read-u32 r 8))
        (r3 (buf-read-u32 r 12))
        (r4 (buf-read-u32 r 16)))
    (let ((s1 (* r1 5))
          (s2 (* r2 5))
          (s3 (* r3 5))
          (s4 (* r4 5)))
      ;; d0 = a0*r0 + a1*s4 + a2*s3 + a3*s2 + a4*s1
      (let ((d0 (tadd (tadd (prod26 a0 r0) (prod26 a1 s4))
                       (tadd (prod26 a2 s3) (tadd (prod26 a3 s2) (prod26 a4 s1))))))
        ;; Write limb 0, get carry
        (buf-write-u32 a 0 (cddr d0))
        (let ((c0h (car d0)) (c0m (cadr d0)))
          ;; d1 = a0*r1 + a1*r0 + a2*s4 + a3*s3 + a4*s2 + carry
          (let ((d1 (tadd (cons 0 (cons c0h c0m))
                          (tadd (tadd (prod26 a0 r1) (prod26 a1 r0))
                                (tadd (prod26 a2 s4) (tadd (prod26 a3 s3) (prod26 a4 s2)))))))
            (buf-write-u32 a 4 (cddr d1))
            (let ((c1h (car d1)) (c1m (cadr d1)))
              ;; d2 = a0*r2 + a1*r1 + a2*r0 + a3*s4 + a4*s3 + carry
              (let ((d2 (tadd (cons 0 (cons c1h c1m))
                              (tadd (tadd (prod26 a0 r2) (prod26 a1 r1))
                                    (tadd (prod26 a2 r0) (tadd (prod26 a3 s4) (prod26 a4 s3)))))))
                (buf-write-u32 a 8 (cddr d2))
                (let ((c2h (car d2)) (c2m (cadr d2)))
                  ;; d3 = a0*r3 + a1*r2 + a2*r1 + a3*r0 + a4*s4 + carry
                  (let ((d3 (tadd (cons 0 (cons c2h c2m))
                                  (tadd (tadd (prod26 a0 r3) (prod26 a1 r2))
                                        (tadd (prod26 a2 r1) (tadd (prod26 a3 r0) (prod26 a4 s4)))))))
                    (buf-write-u32 a 12 (cddr d3))
                    (let ((c3h (car d3)) (c3m (cadr d3)))
                      ;; d4 = a0*r4 + a1*r3 + a2*r2 + a3*r1 + a4*r0 + carry
                      (let ((d4 (tadd (cons 0 (cons c3h c3m))
                                      (tadd (tadd (prod26 a0 r4) (prod26 a1 r3))
                                            (tadd (prod26 a2 r2) (tadd (prod26 a3 r1) (prod26 a4 r0)))))))
                        (buf-write-u32 a 16 (cddr d4))
                        ;; Wrap: carry * 5 added to limb 0
                        (let ((wm (cadr d4)) (wh (car d4)))
                          (when (not (zerop (+ wm wh)))
                            (let ((c5m (* wm 5))
                                  (c5h (* wh 5)))
                              (let ((l0 (+ (buf-read-u32 a 0) c5m)))
                                (buf-write-u32 a 0 (logand l0 #x3FFFFFF))
                                (let ((c (+ (ash l0 -26) c5h)))
                                  (when (> c 0)
                                    (buf-write-u32 a 4 (+ (buf-read-u32 a 4) c))))))))))))))))))))

;; i386-safe ssh-dispatch-msg: fixes 3-arg + in exec command parsing
(defun ssh-dispatch-msg (ssh payload plen flag-addr)
  (let ((msg-type (aref payload 0)))
    (when (eq msg-type 5)
      (let ((svc-len (ssh-get-u32 payload 1))
            (svc (make-array 32)))
        (dotimes (i svc-len)
          (aset svc i (aref payload (+ 5 i))))
        (ssh-send-service-accept ssh svc svc-len)))
    (when (eq msg-type 50)
      (ssh-handle-userauth ssh payload plen))
    (when (eq msg-type 90)
      (let ((ctype-len (ssh-get-u32 payload 1)))
        (let ((cli-chan (ssh-get-u32 payload (+ 5 ctype-len))))
          (setf (mem-ref (+ ssh #x18) :u32) cli-chan)
          (setf (mem-ref (+ ssh #x14) :u32) 0)
          (ssh-send-channel-confirm ssh cli-chan 0))))
    (when (eq msg-type 98)
      (let ((rtype-len (ssh-get-u32 payload 5)))
        (let ((want-reply (aref payload (+ 9 rtype-len))))
          (when (not (zerop want-reply))
            (ssh-send-channel-success ssh
             (mem-ref (+ ssh #x18) :u32)))
          ;; "shell" (rtype_len=5, 's'=115) → send prompt, enter interactive
          (when (eq rtype-len 5)
            (when (eq (aref payload 9) 115)
              (ssh-send-prompt ssh)))
          ;; "exec" (rtype_len=4, 'e'=101) → execute command, send result
          (when (eq rtype-len 4)
            (when (eq (aref payload 9) 101)
              (let ((cmd-off (+ 10 rtype-len)))
                (let ((cmd-len (ssh-get-u32 payload cmd-off)))
                  (let ((cmd (make-array cmd-len)))
                    (let ((cmd-data-off (+ cmd-off 4)))
                      (dotimes (i cmd-len)
                        (aset cmd i (aref payload (+ cmd-data-off i)))))
                    (ssh-eval-line ssh cmd cmd-len)
                    ;; Send EOF + CLOSE after exec
                    (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
                      (let ((eof-msg (make-array 5)))
                        (aset eof-msg 0 96)
                        (ssh-put-u32 eof-msg 1 cli-chan)
                        (ssh-send-payload ssh eof-msg 5))
                      (let ((close-msg (make-array 5)))
                        (aset close-msg 0 97)
                        (ssh-put-u32 close-msg 1 cli-chan)
                        (ssh-send-payload ssh close-msg 5)))
                    (setf (mem-ref flag-addr :u32) 0)))))))))
    (when (eq msg-type 94)
      (ssh-handle-channel-data ssh payload plen))
    (when (eq msg-type 96)
      ;; Client sent CHANNEL_EOF — send our EOF + CLOSE back
      (let ((cli-chan (mem-ref (+ ssh #x18) :u32)))
        (let ((eof-msg (make-array 5)))
          (aset eof-msg 0 96)
          (ssh-put-u32 eof-msg 1 cli-chan)
          (ssh-send-payload ssh eof-msg 5))
        (let ((close-msg (make-array 5)))
          (aset close-msg 0 97)
          (ssh-put-u32 close-msg 1 cli-chan)
          (ssh-send-payload ssh close-msg 5)))
      (setf (mem-ref flag-addr :u32) 0))
    (when (eq msg-type 97)
      (setf (mem-ref flag-addr :u32) 0))
    (when (eq msg-type 1)
      (setf (mem-ref flag-addr :u32) 0))))

;; Diagnostic ssh-message-loop with markers
(defun ssh-message-loop (ssh)
  (%serial-byte 77) (%serial-byte 76) (%serial-byte 10)
  (let ((flag-addr (+ (conn-ssh 3) #x700))
        (cb (- ssh #x20)))
    (setf (mem-ref flag-addr :u32) 1)
    ;; Print encrypted flag
    (%serial-byte 69) (%serial-byte 58)
    (print-dec (mem-ref (+ ssh #x0C) :u32)) (%serial-byte 10)
    ;; Print c2s key (first 8 bytes) for verification
    (%serial-byte 67) (%serial-byte 75) (%serial-byte 58)
    (dotimes (i 8) (print-hex-byte (mem-ref (+ (+ ssh #x090) i) :u8)))
    (%serial-byte 10)
    ;; Print c2s K2 key (second 32 bytes of c2s key, for length cipher)
    (%serial-byte 75) (%serial-byte 50) (%serial-byte 58)
    (dotimes (i 8) (print-hex-byte (mem-ref (+ (+ ssh #x0B0) i) :u8)))
    (%serial-byte 10)
    (loop
      (when (zerop (mem-ref flag-addr :u32)) (return ()))
      (let ((pkt (ssh-receive-packet ssh 50000)))
        (if pkt
            (progn
              (%serial-byte 77) (print-dec (aref (car pkt) 0)) (%serial-byte 10)
              (ssh-dispatch-msg ssh (car pkt) (cdr pkt) flag-addr))
            (progn
              ;; Print buffer state on timeout/nil
              (%serial-byte 77) (%serial-byte 48) (%serial-byte 58)
              (print-dec (mem-ref (+ ssh #x6D4) :u32)) (%serial-byte 10)
              (setf (mem-ref flag-addr :u32) 0)))))))

;; Override ed25519-verify to avoid ASET+AREF register clobber bug.
;; Original: (aset s-bytes i (aref signature (+ i 32))) reads wrong bytes
;; on 32-bit targets because (+ i 32) inside aset value clobbers registers.
;; Fix: pre-compute index and value in let bindings.
(defun ed25519-verify (pubkey signature message msg-len)
  (ed25519-init)
  (sha512-init)
  (let ((r-bytes (make-array 32))
        (s-bytes (make-array 32)))
    ;; Extract R and S — use let bindings to avoid register clobber
    (dotimes (i 32)
      (let ((r-val (aref signature i)))
        (aset r-bytes i r-val))
      (let ((s-idx (+ i 32)))
        (let ((s-val (aref signature s-idx)))
          (aset s-bytes i s-val))))
    ;; Decode points
    (let ((r-point (ed-decode-point r-bytes))
          (a-point (ed-decode-point pubkey)))
      ;; k = SHA-512(R || A || message) mod L
      (let ((k-input (concat3-bytes r-bytes 32 pubkey 32 message msg-len)))
        (let ((k (ed-reduce-scalar (sha512 k-input))))
          ;; Check: [S]B = R + [k]A
          (let ((sb-point (ed-base-mult s-bytes))
                (ka-point (ed-scalar-mult k a-point)))
            (let ((rka-point (ed-add r-point ka-point)))
              (let ((sb-e (ed-encode-point sb-point))
                    (rka-e (ed-encode-point rka-point))
                    (equal 1))
                (dotimes (i 32)
                  (unless (eq (aref sb-e i) (aref rka-e i))
                    (setq equal nil)))
                equal))))))))
