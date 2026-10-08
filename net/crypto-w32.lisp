;;;; crypto-w32.lisp — SHA-256, ChaCha20, SHA-512 for 31-bit fixnum (32-bit targets)
;;;;
;;;; Load AFTER crypto.lisp + crypto-32.lisp (last-defun-wins).
;;;;
;;;; On i386, fixnums are 31-bit (max positive ~0x3FFFFFFF). Values like
;;;; SHA-256 K constants (0xb5c0fbcf) and #xFFFFFFFF overflow. This file
;;;; uses w32 = (hi16 . lo16) pair arithmetic for all 32-bit operations.
;;;;
;;;; Each 16-bit half fits safely in a fixnum (max 65535 << 1 = 131070).
;;;; Intermediate sums (max ~131070) also fit.

;; ================================================================
;; w32: 32-bit value as (hi16 . lo16) pair
;; ================================================================

(defun w32 (hi lo) (cons hi lo))

(defun w32+ (a b)
  (let ((lo (+ (cdr a) (cdr b))))
    (cons (logand (+ (car a) (+ (car b) (ash lo -16))) #xFFFF)
          (logand lo #xFFFF))))

(defun w32-xor (a b)
  (cons (logxor (car a) (car b))
        (logxor (cdr a) (cdr b))))

(defun w32-and (a b)
  (cons (logand (car a) (car b))
        (logand (cdr a) (cdr b))))

(defun w32-not (a)
  (cons (logxor (car a) #xFFFF)
        (logxor (cdr a) #xFFFF)))

;; Right rotate 32-bit value by n bits (0 < n < 16)
;; Formula: each half does (half >> n) | ((other & mask) << (16-n))
(defun w32-rotr-small (x n mask shift)
  (let ((hi (car x)) (lo (cdr x)))
    (cons (logand (logior (ash hi (- 0 n)) (ash (logand lo mask) shift)) #xFFFF)
          (logand (logior (ash lo (- 0 n)) (ash (logand hi mask) shift)) #xFFFF))))

;; Right rotate by n >= 16: swap halves then rotate by (n-16)
(defun w32-rotr-big (x n2 mask shift)
  (let ((hi (cdr x)) (lo (car x)))
    (cons (logand (logior (ash hi (- 0 n2)) (ash (logand lo mask) shift)) #xFFFF)
          (logand (logior (ash lo (- 0 n2)) (ash (logand hi mask) shift)) #xFFFF))))

;; Right shift (not rotate) by n bits, n < 16
(defun w32-shr-small (x n)
  (let ((hi (car x)) (lo (cdr x)))
    (cons (ash hi (- 0 n))
          (logand (logior (ash lo (- 0 n))
                          (ash (logand hi (- (ash 1 n) 1)) (- 16 n)))
                  #xFFFF))))

;; Right shift by n >= 16
(defun w32-shr-big (x n2)
  (cons 0
        (logand (logior (ash (car x) (- 0 n2))
                        (ash (logand (cdr x) 0) 0))
                #xFFFF)))

;; ================================================================
;; w32 byte I/O
;; ================================================================

;; Read big-endian u32 from byte array
(defun w32-from-be (buf off)
  (cons (logior (ash (aref buf off) 8) (aref buf (+ off 1)))
        (logior (ash (aref buf (+ off 2)) 8) (aref buf (+ off 3)))))

;; Write big-endian u32 to byte array
(defun w32-to-be (buf off w)
  (let ((hi (car w)) (lo (cdr w)))
    (aset buf off (logand (ash hi -8) #xFF))
    (aset buf (+ off 1) (logand hi #xFF))
    (aset buf (+ off 2) (logand (ash lo -8) #xFF))
    (aset buf (+ off 3) (logand lo #xFF))))

;; Read little-endian u32 from byte array
(defun w32-from-le (buf off)
  (cons (logior (ash (aref buf (+ off 3)) 8) (aref buf (+ off 2)))
        (logior (ash (aref buf (+ off 1)) 8) (aref buf off))))

;; Write little-endian u32 to byte array
(defun w32-to-le (buf off w)
  (let ((hi (car w)) (lo (cdr w)))
    (aset buf off (logand lo #xFF))
    (aset buf (+ off 1) (logand (ash lo -8) #xFF))
    (aset buf (+ off 2) (logand hi #xFF))
    (aset buf (+ off 3) (logand (ash hi -8) #xFF))))

;; w32 from constant (hi16, lo16 specified directly)
(defun w32-const (hi lo) (cons hi lo))

;; Store/load w32 in w32 working array (4 bytes per w32)
;; Working arrays store as big-endian bytes via aset/aref
(defun w32-wstore (arr off w)
  (w32-to-be arr off w))

(defun w32-wload (arr off)
  (w32-from-be arr off))

;; ================================================================
;; SHA-256 (w32 pair-based)
;; ================================================================

;; SHA-256 K constants — stored at e1000-state-base + 0x100 as raw bytes
;; Each K is 4 bytes (big-endian). We write via u8 stores to avoid fixnum overflow.
(defun sha256-init-k (off b0 b1 b2 b3)
  (let ((addr (+ (e1000-state-base) (+ #x100 off))))
    (setf (mem-ref addr :u8) b0)
    (setf (mem-ref (+ addr 1) :u8) b1)
    (setf (mem-ref (+ addr 2) :u8) b2)
    (setf (mem-ref (+ addr 3) :u8) b3)))

;; Load K constant as w32 pair
(defun sha256-load-k (i)
  (let ((addr (+ (e1000-state-base) (+ #x100 (* i 4)))))
    (cons (logior (ash (mem-ref addr :u8) 8) (mem-ref (+ addr 1) :u8))
          (logior (ash (mem-ref (+ addr 2) :u8) 8) (mem-ref (+ addr 3) :u8)))))

(defun sha256-init ()
  (sha256-init-k 0 #x42 #x8a #x2f #x98) (sha256-init-k 4 #x71 #x37 #x44 #x91)
  (sha256-init-k 8 #xb5 #xc0 #xfb #xcf) (sha256-init-k 12 #xe9 #xb5 #xdb #xa5)
  (sha256-init-k 16 #x39 #x56 #xc2 #x5b) (sha256-init-k 20 #x59 #xf1 #x11 #xf1)
  (sha256-init-k 24 #x92 #x3f #x82 #xa4) (sha256-init-k 28 #xab #x1c #x5e #xd5)
  (sha256-init-k 32 #xd8 #x07 #xaa #x98) (sha256-init-k 36 #x12 #x83 #x5b #x01)
  (sha256-init-k 40 #x24 #x31 #x85 #xbe) (sha256-init-k 44 #x55 #x0c #x7d #xc3)
  (sha256-init-k 48 #x72 #xbe #x5d #x74) (sha256-init-k 52 #x80 #xde #xb1 #xfe)
  (sha256-init-k 56 #x9b #xdc #x06 #xa7) (sha256-init-k 60 #xc1 #x9b #xf1 #x74)
  (sha256-init-k 64 #xe4 #x9b #x69 #xc1) (sha256-init-k 68 #xef #xbe #x47 #x86)
  (sha256-init-k 72 #x0f #xc1 #x9d #xc6) (sha256-init-k 76 #x24 #x0c #xa1 #xcc)
  (sha256-init-k 80 #x2d #xe9 #x2c #x6f) (sha256-init-k 84 #x4a #x74 #x84 #xaa)
  (sha256-init-k 88 #x5c #xb0 #xa9 #xdc) (sha256-init-k 92 #x76 #xf9 #x88 #xda)
  (sha256-init-k 96 #x98 #x3e #x51 #x52) (sha256-init-k 100 #xa8 #x31 #xc6 #x6d)
  (sha256-init-k 104 #xb0 #x03 #x27 #xc8) (sha256-init-k 108 #xbf #x59 #x7f #xc7)
  (sha256-init-k 112 #xc6 #xe0 #x0b #xf3) (sha256-init-k 116 #xd5 #xa7 #x91 #x47)
  (sha256-init-k 120 #x06 #xca #x63 #x51) (sha256-init-k 124 #x14 #x29 #x29 #x67)
  (sha256-init-k 128 #x27 #xb7 #x0a #x85) (sha256-init-k 132 #x2e #x1b #x21 #x38)
  (sha256-init-k 136 #x4d #x2c #x6d #xfc) (sha256-init-k 140 #x53 #x38 #x0d #x13)
  (sha256-init-k 144 #x65 #x0a #x73 #x54) (sha256-init-k 148 #x76 #x6a #x0a #xbb)
  (sha256-init-k 152 #x81 #xc2 #xc9 #x2e) (sha256-init-k 156 #x92 #x72 #x2c #x85)
  (sha256-init-k 160 #xa2 #xbf #xe8 #xa1) (sha256-init-k 164 #xa8 #x1a #x66 #x4b)
  (sha256-init-k 168 #xc2 #x4b #x8b #x70) (sha256-init-k 172 #xc7 #x6c #x51 #xa3)
  (sha256-init-k 176 #xd1 #x92 #xe8 #x19) (sha256-init-k 180 #xd6 #x99 #x06 #x24)
  (sha256-init-k 184 #xf4 #x0e #x35 #x85) (sha256-init-k 188 #x10 #x6a #xa0 #x70)
  (sha256-init-k 192 #x19 #xa4 #xc1 #x16) (sha256-init-k 196 #x1e #x37 #x6c #x08)
  (sha256-init-k 200 #x27 #x48 #x77 #x4c) (sha256-init-k 204 #x34 #xb0 #xbc #xb5)
  (sha256-init-k 208 #x39 #x1c #x0c #xb3) (sha256-init-k 212 #x4e #xd8 #xaa #x4a)
  (sha256-init-k 216 #x5b #x9c #xca #x4f) (sha256-init-k 220 #x68 #x2e #x6f #xf3)
  (sha256-init-k 224 #x74 #x8f #x82 #xee) (sha256-init-k 228 #x78 #xa5 #x63 #x6f)
  (sha256-init-k 232 #x84 #xc8 #x78 #x14) (sha256-init-k 236 #x8c #xc7 #x02 #x08)
  (sha256-init-k 240 #x90 #xbe #xff #xfa) (sha256-init-k 244 #xa4 #x50 #x6c #xeb)
  (sha256-init-k 248 #xbe #xf9 #xa3 #xf7) (sha256-init-k 252 #xc6 #x71 #x78 #xf2))

;; SHA-256 boolean functions (w32)
(defun sha256-ch (x y z)
  (w32-xor (w32-and x y) (w32-and (w32-not x) z)))

(defun sha256-maj (x y z)
  (w32-xor (w32-xor (w32-and x y) (w32-and x z)) (w32-and y z)))

;; SHA-256 sigma functions (w32)
;; BSIG0: rotr(2) XOR rotr(13) XOR rotr(22)
(defun sha256-bsig0 (x)
  (w32-xor (w32-xor (w32-rotr-small x 2 3 14)
                     (w32-rotr-small x 13 8191 3))
           (w32-rotr-big x 6 63 10)))

;; BSIG1: rotr(6) XOR rotr(11) XOR rotr(25)
(defun sha256-bsig1 (x)
  (w32-xor (w32-xor (w32-rotr-small x 6 63 10)
                     (w32-rotr-small x 11 2047 5))
           (w32-rotr-big x 9 511 7)))

;; LSIG0: rotr(7) XOR rotr(18) XOR shr(3)
(defun sha256-lsig0 (x)
  (w32-xor (w32-xor (w32-rotr-small x 7 127 9)
                     (w32-rotr-big x 2 3 14))
           (w32-shr-small x 3)))

;; LSIG1: rotr(17) XOR rotr(19) XOR shr(10)
(defun sha256-lsig1 (x)
  (w32-xor (w32-xor (w32-rotr-big x 1 1 15)
                     (w32-rotr-big x 3 7 13))
           (w32-shr-small x 10)))

;; SHA-256 block compression (w32 pair-based)
;; The 64-word schedule is scratch, owned by the caller: one buffer serves a whole hash
;; (it was allocated once per BLOCK before).  Same shape as SHA512-BLOCK.
(defun sha256-block (block bo h w)
  ;; Expand W[0..15] from block (big-endian)
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
  ;; Initialize working variables
  (let ((a (w32-from-be h 0))
        (b (w32-from-be h 4))
        (cc (w32-from-be h 8))
        (d (w32-from-be h 12))
        (e (w32-from-be h 16))
        (f (w32-from-be h 20))
        (g (w32-from-be h 24))
        (hh (w32-from-be h 28)))
    ;; 64 rounds
    (dotimes (i 64)
      (let ((bsig1 (sha256-bsig1 e)))
        (let ((ch-val (sha256-ch e f g)))
          (let ((ki (sha256-load-k i)))
            (let ((wi (w32-wload w (* i 4))))
              (let ((t1 (w32+ hh (w32+ bsig1 (w32+ ch-val (w32+ ki wi))))))
                (let ((bsig0 (sha256-bsig0 a)))
                  (let ((maj-val (sha256-maj a b cc)))
                    (let ((t2 (w32+ bsig0 maj-val)))
                      (setq hh g)
                      (setq g f)
                      (setq f e)
                      (setq e (w32+ d t1))
                      (setq d cc)
                      (setq cc b)
                      (setq b a)
                      (setq a (w32+ t1 t2)))))))))))
    ;; Add back to hash state
    (w32-to-be h 0 (w32+ (w32-from-be h 0) a))
    (w32-to-be h 4 (w32+ (w32-from-be h 4) b))
    (w32-to-be h 8 (w32+ (w32-from-be h 8) cc))
    (w32-to-be h 12 (w32+ (w32-from-be h 12) d))
    (w32-to-be h 16 (w32+ (w32-from-be h 16) e))
    (w32-to-be h 20 (w32+ (w32-from-be h 20) f))
    (w32-to-be h 24 (w32+ (w32-from-be h 24) g))
    (w32-to-be h 28 (w32+ (w32-from-be h 28) hh))))

;; SHA-256 hash (byte array → 32-byte hash)
(defun sha256 (msg)
  (let ((msg-len (array-length msg)))
    (let ((r (mod (+ msg-len 9) 64)))
      (let ((pad-extra (if (zerop r) 0 (- 64 r))))
      (let ((total (+ (+ msg-len 9) pad-extra)))
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
            ;; Initial hash values (big-endian bytes, safe on i386)
            (aset h 0 #x6a) (aset h 1 #x09) (aset h 2 #xe6) (aset h 3 #x67)
            (aset h 4 #xbb) (aset h 5 #x67) (aset h 6 #xae) (aset h 7 #x85)
            (aset h 8 #x3c) (aset h 9 #x6e) (aset h 10 #xf3) (aset h 11 #x72)
            (aset h 12 #xa5) (aset h 13 #x4f) (aset h 14 #xf5) (aset h 15 #x3a)
            (aset h 16 #x51) (aset h 17 #x0e) (aset h 18 #x52) (aset h 19 #x7f)
            (aset h 20 #x9b) (aset h 21 #x05) (aset h 22 #x68) (aset h 23 #x8c)
            (aset h 24 #x1f) (aset h 25 #x83) (aset h 26 #xd9) (aset h 27 #xab)
            (aset h 28 #x5b) (aset h 29 #xe0) (aset h 30 #xcd) (aset h 31 #x19)
            (let ((offset 0) (w (make-array 256)))
              (loop
                (when (not (< offset total)) (return ()))
                (sha256-block padded offset h w)
                (setq offset (+ offset 64))))
            h)))))))

;; ================================================================
;; ChaCha20 (w32 pair-based)
;; ================================================================

;; Left rotations for ChaCha20
;; rotl(n) = rotr(32-n)
(defun chacha-rotl16 (x)
  ;; rotr(16) = swap halves
  (cons (cdr x) (car x)))

(defun chacha-rotl12 (x)
  ;; rotr(20) = rotr-big(4)
  (w32-rotr-big x 4 15 12))

(defun chacha-rotl8 (x)
  ;; rotr(24) = rotr-big(8)
  (w32-rotr-big x 8 255 8))

(defun chacha-rotl7 (x)
  ;; rotr(25) = rotr-big(9)
  (w32-rotr-big x 9 511 7))

;; ChaCha20 quarter round on w32 state array
(defun chacha-qr (s a b c d)
  (let ((sa (w32-wload s a))
        (sb (w32-wload s b))
        (sc (w32-wload s c))
        (sd (w32-wload s d)))
    (setq sa (w32+ sa sb))
    (setq sd (chacha-rotl16 (w32-xor sd sa)))
    (setq sc (w32+ sc sd))
    (setq sb (chacha-rotl12 (w32-xor sb sc)))
    (setq sa (w32+ sa sb))
    (setq sd (chacha-rotl8 (w32-xor sd sa)))
    (setq sc (w32+ sc sd))
    (setq sb (chacha-rotl7 (w32-xor sb sc)))
    (w32-wstore s a sa)
    (w32-wstore s b sb)
    (w32-wstore s c sc)
    (w32-wstore s d sd)))

;; 20 rounds (10 double-rounds)
(defun chacha-inner (state)
  (dotimes (i 10)
    (chacha-qr state 0 16 32 48)
    (chacha-qr state 4 20 36 52)
    (chacha-qr state 8 24 40 56)
    (chacha-qr state 12 28 44 60)
    (chacha-qr state 0 20 40 60)
    (chacha-qr state 4 24 44 48)
    (chacha-qr state 8 28 32 52)
    (chacha-qr state 12 16 36 56)))

;; ChaCha20 state setup
(defun chacha-setup (key nonce counter)
  (let ((s (make-array 64)))
    ;; Constants "expand 32-byte k" as LE w32
    (w32-wstore s 0 (w32-const #x6170 #x7865))
    (w32-wstore s 4 (w32-const #x3320 #x646e))
    (w32-wstore s 8 (w32-const #x7962 #x2d32))
    (w32-wstore s 12 (w32-const #x6b20 #x6574))
    ;; Key (8 words, little-endian from bytes)
    (dotimes (i 8)
      (let ((j (* i 4)))
        (w32-wstore s (+ 16 j) (w32-from-le key j))))
    ;; Counter
    (w32-wstore s 48 (w32-const 0 counter))
    ;; Nonce (3 words)
    (dotimes (i 3)
      (let ((j (* i 4)))
        (w32-wstore s (+ 52 j) (w32-from-le nonce j))))
    s))

;; Generate one 64-byte keystream block
(defun chacha-block (key nonce counter)
  (let ((state (chacha-setup key nonce counter))
        (work (make-array 64)))
    (dotimes (i 64) (aset work i (aref state i)))
    (chacha-inner work)
    (let ((out (make-array 64)))
      (dotimes (i 16)
        (let ((off (* i 4)))
          (let ((sum (w32+ (w32-wload work off) (w32-wload state off))))
            (w32-to-le out off sum))))
      out)))

;; Encrypt/decrypt
(defun chacha20-crypt (key nonce data data-len counter)
  (let ((result (make-array data-len))
        (block-num 0))
    (loop
      (when (not (< (* block-num 64) data-len)) (return ()))
      (let ((ks (chacha-block key nonce (+ counter block-num)))
            (base (* block-num 64)))
        (dotimes (i 64)
          (when (< (+ base i) data-len)
            (aset result (+ base i)
                  (logxor (aref data (+ base i)) (aref ks i))))))
      (setq block-num (+ block-num 1)))
    result))

;; ================================================================
;; SHA-512 (w64 = (w32-hi . w32-lo) pair-based)
;; ================================================================

;; w64 = (w32 . w32) where each w32 = (hi16 . lo16)

(defun w64-add (a b)
  (let ((lo-a (cdr a)) (lo-b (cdr b)))
    (let ((lo-sum-lo (+ (cdr lo-a) (cdr lo-b))))
      (let ((lo-carry1 (ash lo-sum-lo -16))
            (lo-sum-lo16 (logand lo-sum-lo #xFFFF)))
        (let ((lo-sum-hi (+ (car lo-a) (+ (car lo-b) lo-carry1))))
          (let ((lo-carry2 (ash lo-sum-hi -16))
                (lo-sum-hi16 (logand lo-sum-hi #xFFFF)))
            (let ((lo-result (cons lo-sum-hi16 lo-sum-lo16)))
              (let ((hi-a (car a)) (hi-b (car b)))
                (let ((hi-sum-lo (+ (cdr hi-a) (+ (cdr hi-b) lo-carry2))))
                  (let ((hi-carry1 (ash hi-sum-lo -16))
                        (hi-sum-lo16 (logand hi-sum-lo #xFFFF)))
                    (let ((hi-sum-hi (+ (car hi-a) (+ (car hi-b) hi-carry1))))
                      (cons (cons (logand hi-sum-hi #xFFFF) hi-sum-lo16)
                            lo-result))))))))))))

(defun w64-xor (a b)
  (cons (w32-xor (car a) (car b))
        (w32-xor (cdr a) (cdr b))))

(defun w64-and (a b)
  (cons (w32-and (car a) (car b))
        (w32-and (cdr a) (cdr b))))

(defun w64-not (a)
  (cons (w32-not (car a))
        (w32-not (cdr a))))

;; SHA-512 Ch, Maj
(defun sha512-ch (x y z)
  (w64-xor (w64-and x y) (w64-and (w64-not x) z)))

(defun sha512-maj (x y z)
  (w64-xor (w64-xor (w64-and x y) (w64-and x z)) (w64-and y z)))

;; 64-bit right rotation: rotr-n for n < 32 operates on (hi32 . lo32)
;; hi' = (hi>>n) | (lo & mask) << (32-n)  → as w32 rotation
;; lo' = (lo>>n) | (hi & mask) << (32-n)  → as w32 rotation
;; For n >= 32: swap hi/lo then rotr by (n-32)

(defun w64-rotr-sub32 (x n)
  ;; n < 32: rotate each w32 "half" by borrowing bits from the other
  ;; This is complex with w32 pairs. Implement via byte-level extraction.
  ;; For SHA-512 sigma, we only need specific rotation amounts.
  ;; Delegate to specific helpers.
  0)

;; SHA-512 sigma functions — specific rotation amounts
;; BSIG0: rotr(28) XOR rotr(34) XOR rotr(39)
;; BSIG1: rotr(14) XOR rotr(18) XOR rotr(41)
;; LSIG0: rotr(1) XOR rotr(8) XOR shr(7)
;; LSIG1: rotr(19) XOR rotr(61) XOR shr(6)

;; For 64-bit rotr by n < 32:
;; Result.hi32 = (hi32 >> n) | (lo32 << (32-n))
;; Result.lo32 = (lo32 >> n) | (hi32 << (32-n))
;; Each ">> n" and "<< (32-n)" on w32 pairs uses the same w32-rotr logic

;; w32 left shift by n (< 16): shift left, overflow to hi
(defun w32-shl-small (x n)
  (let ((hi (car x)) (lo (cdr x)))
    (cons (logand (logior (ash hi n) (ash lo (- n 16))) #xFFFF)
          (logand (ash lo n) #xFFFF))))

;; w32 left shift by n >= 16
(defun w32-shl-big (x n2)
  (cons (logand (ash (cdr x) n2) #xFFFF)
        0))

;; 64-bit rotate right by n (0 < n < 16)
(defun w64-rotr-lt16 (x n)
  (let ((hi (car x)) (lo (cdr x)))
    (let ((shift-back (- 16 n)))
      ;; hi32 >> n
      (let ((hi-shr (w32-rotr-small hi n 0 0)))
        ;; Actually, need full w32 shift not rotate.
        ;; Let me use w32-shr-small and w32-shl-small properly.
        (let ((hi-right (w32-shr-small hi n))
              (lo-left (w32-shl-big lo (- 16 n))))
          ;; Hmm this is getting complex. Let me use a different approach.
          ;; Convert to bytes, rotate, convert back.
          0)))))

;; Actually, let's use a byte-based approach for SHA-512 rotations.
;; Convert w64 to 8-byte array, rotate bits, convert back.
;; This is slower but correct and avoids complex w32 shift composition.

(defun w64-to-bytes (w buf off)
  (w32-to-be buf off (car w))
  (w32-to-be buf (+ off 4) (cdr w)))

(defun w64-from-bytes (buf off)
  (cons (w32-from-be buf off) (w32-from-be buf (+ off 4))))

;; Rotate right 8 bytes by n bits
;; Uses scratch arrays. n < 64.
(defun bytes8-rotr (src dst n)
  ;; Zero dst
  (dotimes (i 8) (aset dst i 0))
  ;; Byte-level approach: shift bytes, then handle sub-byte shifts
  ;; Flattened to avoid deeply nested logand/logior/ash (MVM register clobber)
  (let ((byte-shift (ash n -3))     ;; n / 8
        (bit-shift (logand n 7)))   ;; n % 8
    (if (zerop bit-shift)
        ;; Pure byte rotation
        (dotimes (i 8)
          (let ((di (logand (+ i byte-shift) 7)))
            (aset dst di (aref src i))))
        ;; Byte rotation + sub-byte shift
        (dotimes (i 8)
          (let ((di (logand (+ i byte-shift) 7))
                (di1 (logand (+ (+ i byte-shift) 1) 7)))
            (let ((sv (aref src i)))
              ;; dst[di] |= (sv >> bit-shift) & 0xFF
              (let ((cur-di (aref dst di)))
                (let ((shifted (ash sv (- 0 bit-shift))))
                  (let ((ored (logior cur-di shifted)))
                    (aset dst di (logand ored #xFF)))))
              ;; dst[di1] |= ((sv & mask) << (8 - bit-shift)) & 0xFF
              (let ((cur-di1 (aref dst di1)))
                (let ((mask-val (- (ash 1 bit-shift) 1)))
                  (let ((masked (logand sv mask-val)))
                    (let ((shift-amt (- 8 bit-shift)))
                      (let ((shifted2 (ash masked shift-amt)))
                        (let ((ored2 (logior cur-di1 shifted2)))
                          (aset dst di1 (logand ored2 #xFF))))))))))))))

;; Shift right 8 bytes by n bits
;; Flattened to avoid deeply nested logand/logior/ash (MVM register clobber)
(defun bytes8-shr (src dst n)
  (dotimes (i 8) (aset dst i 0))
  (let ((byte-shift (ash n -3))
        (bit-shift (logand n 7)))
    (if (zerop bit-shift)
        (let ((i byte-shift))
          (loop
            (when (>= i 8) (return ()))
            (aset dst i (aref src (- i byte-shift)))
            (setq i (+ i 1))))
        (let ((i byte-shift))
          (loop
            (when (>= i 8) (return ()))
            (let ((si (- i byte-shift)))
              ;; dst[i] = (src[si] >> bit-shift) & 0xFF
              (let ((cur (aref dst i)))
                (let ((sv (aref src si)))
                  (let ((shifted (ash sv (- 0 bit-shift))))
                    (let ((ored (logior cur shifted)))
                      (aset dst i (logand ored #xFF))))))
              ;; dst[i] |= ((src[si-1] & mask) << (8 - bit-shift)) & 0xFF
              (when (> si 0)
                (let ((cur2 (aref dst i)))
                  (let ((prev (aref src (- si 1))))
                    (let ((mask-val (- (ash 1 bit-shift) 1)))
                      (let ((masked (logand prev mask-val)))
                        (let ((shift-amt (- 8 bit-shift)))
                          (let ((shifted2 (ash masked shift-amt)))
                            (let ((ored2 (logior cur2 shifted2)))
                              (aset dst i (logand ored2 #xFF)))))))))))
            (setq i (+ i 1)))))))

;; SHA-512 sigma via byte arrays
(defun sha512-sigma-op (x r1 r2 r3 is-shift3)
  (let ((src (make-array 8))
        (d1 (make-array 8))
        (d2 (make-array 8))
        (d3 (make-array 8)))
    (w64-to-bytes x src 0)
    (bytes8-rotr src d1 r1)
    (bytes8-rotr src d2 r2)
    (if is-shift3
        (bytes8-shr src d3 r3)
        (bytes8-rotr src d3 r3))
    ;; XOR all three — flattened to avoid nested logxor/aref register clobber on arm32
    (dotimes (i 8)
      (let ((v1 (aref d1 i)))
        (let ((v2 (aref d2 i)))
          (let ((v3 (aref d3 i)))
            (let ((x23 (logxor v2 v3)))
              (aset d1 i (logxor v1 x23)))))))
    (w64-from-bytes d1 0)))

(defun sha512-sigma0 (x) (sha512-sigma-op x 28 34 39 nil))
(defun sha512-sigma1 (x) (sha512-sigma-op x 14 18 41 nil))
(defun sha512-lsig0 (x) (sha512-sigma-op x 1 8 7 1))
(defun sha512-lsig1 (x) (sha512-sigma-op x 19 61 6 1))

;; SHA-512 K constant init — store as raw bytes at state+0x200
(defun sha512-set-k (i b0 b1 b2 b3 b4 b5 b6 b7)
  (let ((addr (+ (e1000-state-base) (+ #x200 (* i 8)))))
    (setf (mem-ref addr :u8) b0) (setf (mem-ref (+ addr 1) :u8) b1)
    (setf (mem-ref (+ addr 2) :u8) b2) (setf (mem-ref (+ addr 3) :u8) b3)
    (setf (mem-ref (+ addr 4) :u8) b4) (setf (mem-ref (+ addr 5) :u8) b5)
    (setf (mem-ref (+ addr 6) :u8) b6) (setf (mem-ref (+ addr 7) :u8) b7)))

(defun sha512-load-k (i)
  (let ((addr (+ (e1000-state-base) (+ #x200 (* i 8)))))
    (cons (cons (logior (ash (mem-ref addr :u8) 8) (mem-ref (+ addr 1) :u8))
                (logior (ash (mem-ref (+ addr 2) :u8) 8) (mem-ref (+ addr 3) :u8)))
          (cons (logior (ash (mem-ref (+ addr 4) :u8) 8) (mem-ref (+ addr 5) :u8))
                (logior (ash (mem-ref (+ addr 6) :u8) 8) (mem-ref (+ addr 7) :u8))))))

(defun sha512-init ()
  (sha512-set-k 0 #x42 #x8a #x2f #x98 #xd7 #x28 #xae #x22)
  (sha512-set-k 1 #x71 #x37 #x44 #x91 #x23 #xef #x65 #xcd)
  (sha512-set-k 2 #xb5 #xc0 #xfb #xcf #xec #x4d #x3b #x2f)
  (sha512-set-k 3 #xe9 #xb5 #xdb #xa5 #x81 #x89 #xdb #xbc)
  (sha512-set-k 4 #x39 #x56 #xc2 #x5b #xf3 #x48 #xb5 #x38)
  (sha512-set-k 5 #x59 #xf1 #x11 #xf1 #xb6 #x05 #xd0 #x19)
  (sha512-set-k 6 #x92 #x3f #x82 #xa4 #xaf #x19 #x4f #x9b)
  (sha512-set-k 7 #xab #x1c #x5e #xd5 #xda #x6d #x81 #x18)
  (sha512-set-k 8 #xd8 #x07 #xaa #x98 #xa3 #x03 #x02 #x42)
  (sha512-set-k 9 #x12 #x83 #x5b #x01 #x45 #x70 #x6f #xbe)
  (sha512-set-k 10 #x24 #x31 #x85 #xbe #x4e #xe4 #xb2 #x8c)
  (sha512-set-k 11 #x55 #x0c #x7d #xc3 #xd5 #xff #xb4 #xe2)
  (sha512-set-k 12 #x72 #xbe #x5d #x74 #xf2 #x7b #x89 #x6f)
  (sha512-set-k 13 #x80 #xde #xb1 #xfe #x3b #x16 #x96 #xb1)
  (sha512-set-k 14 #x9b #xdc #x06 #xa7 #x25 #xc7 #x12 #x35)
  (sha512-set-k 15 #xc1 #x9b #xf1 #x74 #xcf #x69 #x26 #x94)
  (sha512-set-k 16 #xe4 #x9b #x69 #xc1 #x9e #xf1 #x4a #xd2)
  (sha512-set-k 17 #xef #xbe #x47 #x86 #x38 #x4f #x25 #xe3)
  (sha512-set-k 18 #x0f #xc1 #x9d #xc6 #x8b #x8c #xd5 #xb5)
  (sha512-set-k 19 #x24 #x0c #xa1 #xcc #x77 #xac #x9c #x65)
  (sha512-set-k 20 #x2d #xe9 #x2c #x6f #x59 #x2b #x02 #x75)
  (sha512-set-k 21 #x4a #x74 #x84 #xaa #x6e #xa6 #xe4 #x83)
  (sha512-set-k 22 #x5c #xb0 #xa9 #xdc #xbd #x41 #xfb #xd4)
  (sha512-set-k 23 #x76 #xf9 #x88 #xda #x83 #x11 #x53 #xb5)
  (sha512-set-k 24 #x98 #x3e #x51 #x52 #xee #x66 #xdf #xab)
  (sha512-set-k 25 #xa8 #x31 #xc6 #x6d #x2d #xb4 #x32 #x10)
  (sha512-set-k 26 #xb0 #x03 #x27 #xc8 #x98 #xfb #x21 #x3f)
  (sha512-set-k 27 #xbf #x59 #x7f #xc7 #xbe #xef #x0e #xe4)
  (sha512-set-k 28 #xc6 #xe0 #x0b #xf3 #x3d #xa8 #x8f #xc2)
  (sha512-set-k 29 #xd5 #xa7 #x91 #x47 #x93 #x0a #xa7 #x25)
  (sha512-set-k 30 #x06 #xca #x63 #x51 #xe0 #x03 #x82 #x6f)
  (sha512-set-k 31 #x14 #x29 #x29 #x67 #x0a #x0e #x6e #x70)
  (sha512-set-k 32 #x27 #xb7 #x0a #x85 #x46 #xd2 #x2f #xfc)
  (sha512-set-k 33 #x2e #x1b #x21 #x38 #x5c #x26 #xc9 #x26)
  (sha512-set-k 34 #x4d #x2c #x6d #xfc #x5a #xc4 #x2a #xed)
  (sha512-set-k 35 #x53 #x38 #x0d #x13 #x9d #x95 #xb3 #xdf)
  (sha512-set-k 36 #x65 #x0a #x73 #x54 #x8b #xaf #x63 #xde)
  (sha512-set-k 37 #x76 #x6a #x0a #xbb #x3c #x77 #xb2 #xa8)
  (sha512-set-k 38 #x81 #xc2 #xc9 #x2e #x47 #xed #xae #xe6)
  (sha512-set-k 39 #x92 #x72 #x2c #x85 #x14 #x82 #x35 #x3b)
  (sha512-set-k 40 #xa2 #xbf #xe8 #xa1 #x4c #xf1 #x03 #x64)
  (sha512-set-k 41 #xa8 #x1a #x66 #x4b #xbc #x42 #x30 #x01)
  (sha512-set-k 42 #xc2 #x4b #x8b #x70 #xd0 #xf8 #x97 #x91)
  (sha512-set-k 43 #xc7 #x6c #x51 #xa3 #x06 #x54 #xbe #x30)
  (sha512-set-k 44 #xd1 #x92 #xe8 #x19 #xd6 #xef #x52 #x18)
  (sha512-set-k 45 #xd6 #x99 #x06 #x24 #x55 #x65 #xa9 #x10)
  (sha512-set-k 46 #xf4 #x0e #x35 #x85 #x57 #x71 #x20 #x2a)
  (sha512-set-k 47 #x10 #x6a #xa0 #x70 #x32 #xbb #xd1 #xb8)
  (sha512-set-k 48 #x19 #xa4 #xc1 #x16 #xb8 #xd2 #xd0 #xc8)
  (sha512-set-k 49 #x1e #x37 #x6c #x08 #x51 #x41 #xab #x53)
  (sha512-set-k 50 #x27 #x48 #x77 #x4c #xdf #x8e #xeb #x99)
  (sha512-set-k 51 #x34 #xb0 #xbc #xb5 #xe1 #x9b #x48 #xa8)
  (sha512-set-k 52 #x39 #x1c #x0c #xb3 #xc5 #xc9 #x5a #x63)
  (sha512-set-k 53 #x4e #xd8 #xaa #x4a #xe3 #x41 #x8a #xcb)
  (sha512-set-k 54 #x5b #x9c #xca #x4f #x77 #x63 #xe3 #x73)
  (sha512-set-k 55 #x68 #x2e #x6f #xf3 #xd6 #xb2 #xb8 #xa3)
  (sha512-set-k 56 #x74 #x8f #x82 #xee #x5d #xef #xb2 #xfc)
  (sha512-set-k 57 #x78 #xa5 #x63 #x6f #x43 #x17 #x2f #x60)
  (sha512-set-k 58 #x84 #xc8 #x78 #x14 #xa1 #xf0 #xab #x72)
  (sha512-set-k 59 #x8c #xc7 #x02 #x08 #x1a #x64 #x39 #xec)
  (sha512-set-k 60 #x90 #xbe #xff #xfa #x23 #x63 #x1e #x28)
  (sha512-set-k 61 #xa4 #x50 #x6c #xeb #xde #x82 #xbd #xe9)
  (sha512-set-k 62 #xbe #xf9 #xa3 #xf7 #xb2 #xc6 #x79 #x15)
  (sha512-set-k 63 #xc6 #x71 #x78 #xf2 #xe3 #x72 #x53 #x2b)
  (sha512-set-k 64 #xca #x27 #x3e #xce #xea #x26 #x61 #x9c)
  (sha512-set-k 65 #xd1 #x86 #xb8 #xc7 #x21 #xc0 #xc2 #x07)
  (sha512-set-k 66 #xea #xda #x7d #xd6 #xcd #xe0 #xeb #x1e)
  (sha512-set-k 67 #xf5 #x7d #x4f #x7f #xee #x6e #xd1 #x78)
  (sha512-set-k 68 #x06 #xf0 #x67 #xaa #x72 #x17 #x6f #xba)
  (sha512-set-k 69 #x0a #x63 #x7d #xc5 #xa2 #xc8 #x98 #xa6)
  (sha512-set-k 70 #x11 #x3f #x98 #x04 #xbe #xf9 #x0d #xae)
  (sha512-set-k 71 #x1b #x71 #x0b #x35 #x13 #x1c #x47 #x1b)
  (sha512-set-k 72 #x28 #xdb #x77 #xf5 #x23 #x04 #x7d #x84)
  (sha512-set-k 73 #x32 #xca #xab #x7b #x40 #xc7 #x24 #x93)
  (sha512-set-k 74 #x3c #x9e #xbe #x0a #x15 #xc9 #xbe #xbc)
  (sha512-set-k 75 #x43 #x1d #x67 #xc4 #x9c #x10 #x0d #x4c)
  (sha512-set-k 76 #x4c #xc5 #xd4 #xbe #xcb #x3e #x42 #xb6)
  (sha512-set-k 77 #x59 #x7f #x29 #x9c #xfc #x65 #x7e #x2a)
  (sha512-set-k 78 #x5f #xcb #x6f #xab #x3a #xd6 #xfa #xec)
  (sha512-set-k 79 #x6c #x44 #x19 #x8c #x4a #x47 #x58 #x17))

;; SHA-512 working array I/O (w64 stored as 8 big-endian bytes)
(defun w64-wstore (arr off w)
  (w32-to-be arr off (car w))
  (w32-to-be arr (+ off 4) (cdr w)))

(defun w64-wload (arr off)
  (cons (w32-from-be arr off) (w32-from-be arr (+ off 4))))

;; SHA-512 block compression
(defun sha512-block (block bo h w)
  ;; Read 16 message words (big-endian u64)
  (dotimes (i 16)
    (let ((j (+ bo (* i 8))))
      (w64-wstore w (* i 8) (w64-from-bytes block j))))
  ;; Expand W[16..79]
  (let ((i 16))
    (loop
      (when (not (< i 80)) (return ()))
      (let ((s1 (sha512-lsig1 (w64-wload w (* (- i 2) 8)))))
        (let ((s0 (sha512-lsig0 (w64-wload w (* (- i 15) 8)))))
          (let ((w7 (w64-wload w (* (- i 7) 8))))
            (let ((w16 (w64-wload w (* (- i 16) 8))))
              (let ((t0 (w64-add s0 w16)))
                (let ((t1 (w64-add w7 t0)))
                  (w64-wstore w (* i 8) (w64-add s1 t1))))))))
      (setq i (+ i 1))))
  ;; Initialize working variables
  (let ((a (w64-wload h 0))
        (b (w64-wload h 8))
        (cc (w64-wload h 16))
        (d (w64-wload h 24))
        (e (w64-wload h 32))
        (f (w64-wload h 40))
        (g (w64-wload h 48))
        (hh (w64-wload h 56)))
    ;; 80 rounds
    (dotimes (i 80)
      (let ((ki (sha512-load-k i)))
        (let ((wi (w64-wload w (* i 8))))
          (let ((sig1 (sha512-sigma1 e)))
            (let ((ch-val (sha512-ch e f g)))
              (let ((s1 (w64-add ki wi)))
              (let ((s2 (w64-add ch-val s1)))
              (let ((s3 (w64-add sig1 s2)))
              (let ((t1 (w64-add hh s3)))
                (let ((sig0 (sha512-sigma0 a)))
                  (let ((maj-val (sha512-maj a b cc)))
                    (let ((t2 (w64-add sig0 maj-val)))
                      (setq hh g)
                      (setq g f)
                      (setq f e)
                      (setq e (w64-add d t1))
                      (setq d cc)
                      (setq cc b)
                      (setq b a)
                      (setq a (w64-add t1 t2))))))))))))))
    ;; Add back
    (w64-wstore h 0 (w64-add (w64-wload h 0) a))
    (w64-wstore h 8 (w64-add (w64-wload h 8) b))
    (w64-wstore h 16 (w64-add (w64-wload h 16) cc))
    (w64-wstore h 24 (w64-add (w64-wload h 24) d))
    (w64-wstore h 32 (w64-add (w64-wload h 32) e))
    (w64-wstore h 40 (w64-add (w64-wload h 40) f))
    (w64-wstore h 48 (w64-add (w64-wload h 48) g))
    (w64-wstore h 56 (w64-add (w64-wload h 56) hh))))

;; SHA-512 hash (byte array → 64-byte hash)
(defun sha512 (msg)
  (let ((msg-len (array-length msg)))
    (let ((r (mod (+ msg-len 17) 128)))
      (let ((pad-extra (if (zerop r) 0 (- 128 r))))
      (let ((total (+ (+ msg-len 17) pad-extra)))
        (let ((padded (make-array total)))
          (dotimes (i total) (aset padded i 0))
          (dotimes (i msg-len) (aset padded i (aref msg i)))
          (aset padded msg-len #x80)
          ;; Bit length as big-endian u128 (only low 32 bits matter for us)
          (let ((bits (* msg-len 8)))
            (aset padded (- total 4) (logand (ash bits -24) #xFF))
            (aset padded (- total 3) (logand (ash bits -16) #xFF))
            (aset padded (- total 2) (logand (ash bits -8) #xFF))
            (aset padded (- total 1) (logand bits #xFF)))
          ;; Initial hash values (big-endian bytes)
          (let ((h (make-array 64)))
            (aset h 0 #x6a) (aset h 1 #x09) (aset h 2 #xe6) (aset h 3 #x67)
            (aset h 4 #xf3) (aset h 5 #xbc) (aset h 6 #xc9) (aset h 7 #x08)
            (aset h 8 #xbb) (aset h 9 #x67) (aset h 10 #xae) (aset h 11 #x85)
            (aset h 12 #x84) (aset h 13 #xca) (aset h 14 #xa7) (aset h 15 #x3b)
            (aset h 16 #x3c) (aset h 17 #x6e) (aset h 18 #xf3) (aset h 19 #x72)
            (aset h 20 #xfe) (aset h 21 #x94) (aset h 22 #xf8) (aset h 23 #x2b)
            (aset h 24 #xa5) (aset h 25 #x4f) (aset h 26 #xf5) (aset h 27 #x3a)
            (aset h 28 #x5f) (aset h 29 #x1d) (aset h 30 #x36) (aset h 31 #xf1)
            (aset h 32 #x51) (aset h 33 #x0e) (aset h 34 #x52) (aset h 35 #x7f)
            (aset h 36 #xad) (aset h 37 #xe6) (aset h 38 #x82) (aset h 39 #xd1)
            (aset h 40 #x9b) (aset h 41 #x05) (aset h 42 #x68) (aset h 43 #x8c)
            (aset h 44 #x2b) (aset h 45 #x3e) (aset h 46 #x6c) (aset h 47 #x1f)
            (aset h 48 #x1f) (aset h 49 #x83) (aset h 50 #xd9) (aset h 51 #xab)
            (aset h 52 #xfb) (aset h 53 #x41) (aset h 54 #xbd) (aset h 55 #x6b)
            (aset h 56 #x5b) (aset h 57 #xe0) (aset h 58 #xcd) (aset h 59 #x19)
            (aset h 60 #x13) (aset h 61 #x7e) (aset h 62 #x21) (aset h 63 #x79)
            ;; Process blocks
            (let ((ww (make-array 640))
                  (offset 0))
              (loop
                (when (not (< offset total)) (return ()))
                (sha512-block padded offset h ww)
                (setq offset (+ offset 128))))
            h)))))))

;; ================================================================
;; Override buf-read-u32-le for Poly1305 on i386
;; ================================================================
;; Poly1305 calls buf-read-u32-le which builds LE u32 from bytes.
;; Values are 26-bit limbs (< 0x4000000), always safe on i386.
;; No override needed — the original works for values < 0x3FFFFFFF.
