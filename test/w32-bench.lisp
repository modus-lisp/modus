;;;; test/w32-bench.lisp -- SHA-256 and ChaCha20 throughput for the W32 work
;;;; (docs/w32-spec.md section 9.2).  Portable CL: the same file runs on SBCL
;;;; (the reference and the SBCL number) and is baked into a 32-bit Modus CLI.
;;;;
;;;; LOCAL COPIES, ON PURPOSE.  The block functions below are natrium's
;;;; (natrium cde4e44: src/sha256.lisp %SHA256-BLOCK, src/chacha20.lisp ROTL32 /
;;;; %QR / %CHACHA20-KEYSTREAM).  They are copied, not loaded, because the
;;;; point is to AOT-compile this exact source into an image that cannot load
;;;; natrium, and to keep the source fixed while the compiler changes under it.
;;;; Kept as close to natrium as possible: same names with a W32B- prefix, the
;;;; non-SBCL %ROTR32 shape, the same declarations except that H and W are
;;;; SIMPLE-VECTORs (natrium declares them (SIMPLE-ARRAY FIXNUM), which a u32
;;;; does not fit on a 30-bit Lisp).  test/w32-bench-check.lisp checks these
;;;; against natrium on SBCL; the digests are also checked against Python
;;;; hashlib (test/crypto-sha-vectors.lisp) and RFC 8439.
;;;;
;;;; Baked:   MODUS_EXTRA_SOURCE=test/w32-bench.lisp sbcl ... mvm/build-riscv32-cli.lisp
;;;;          qemu-riscv32-static /tmp/modus-riscv32-cli --eval '(w32b-run 16 16)' --quit
;;;; SBCL:    sbcl --script test/w32-bench-check.lisp

;;; ---- SHA-256 (natrium %sha256-block) -------------------------------------

(defvar *w32b-sha256-k*
  (let ((v (make-array 64)))
    (let ((ks '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
          (i 0))
      (dolist (k ks v) (setf (aref v i) k) (setq i (+ i 1))))))

(defmacro w32b-u32+ (&rest xs) `(ldb (byte 32 0) (+ ,@xs)))
(defmacro w32b-rotr32 (x n)
  `(logior (ash ,x ,(- n)) (ldb (byte 32 0) (ash ,x ,(- 32 n)))))

(defun w32b-sha256-block (h w data off)
  "Compress the 64-byte block DATA[OFF, OFF+64) into H, using W as the schedule."
  (declare (type simple-vector h w)
           (type (simple-array (unsigned-byte 8) (*)) data) (type fixnum off)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((k *w32b-sha256-k*))
    (dotimes (i 16)
      (let ((j (+ off (* 4 i))))
        (declare (type fixnum j))
        (setf (aref w i) (logior (ash (aref data j) 24) (ash (aref data (+ j 1)) 16)
                                 (ash (aref data (+ j 2)) 8) (aref data (+ j 3))))))
    (loop for i of-type fixnum from 16 below 64 do
      (let* ((w15 (aref w (- i 15))) (w2 (aref w (- i 2)))
             (s0 (logxor (w32b-rotr32 w15 7) (w32b-rotr32 w15 18) (ash w15 -3)))
             (s1 (logxor (w32b-rotr32 w2 17) (w32b-rotr32 w2 19) (ash w2 -10)))
             (w16 (aref w (- i 16))) (w7 (aref w (- i 7))))
        (declare (type (unsigned-byte 32) w15 w2 s0 s1 w16 w7))
        (setf (aref w i) (w32b-u32+ w16 s0 w7 s1))))
    (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
          (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
      (declare (type (unsigned-byte 32) a b c d e f g hh))
      (dotimes (i 64)
        (let* ((wi (aref w i))
               (s1 (logxor (w32b-rotr32 e 6) (w32b-rotr32 e 11) (w32b-rotr32 e 25)))
               (ch (logxor (logand e f) (logand (logxor e #xffffffff) g)))
               (t1 (w32b-u32+ hh s1 ch (aref k i) wi))
               (s0 (logxor (w32b-rotr32 a 2) (w32b-rotr32 a 13) (w32b-rotr32 a 22)))
               (maj (logxor (logand a b) (logand a c) (logand b c)))
               (t2 (w32b-u32+ s0 maj)))
          (declare (type (unsigned-byte 32) wi s1 ch t1 s0 maj t2))
          (setf hh g g f f e e (w32b-u32+ d t1) d c c b b a a (w32b-u32+ t1 t2))))
      (macrolet ((acc (i v) `(setf (aref h ,i) (w32b-u32+ (aref h ,i) ,v))))
        (acc 0 a) (acc 1 b) (acc 2 c) (acc 3 d) (acc 4 e) (acc 5 f) (acc 6 g) (acc 7 hh))))
  nil)

(defun w32b-sha256 (msg)
  "SHA-256 of the octet vector MSG, as a fresh 32-byte vector.  Pads a copy of
   the last one or two blocks only; full blocks are compressed in place."
  (let* ((n (length msg))
         (h (make-array 8))
         (w (make-array 64))
         (tail (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0))
         (full (* 64 (floor n 64)))
         (rem (- n full))
         (tlen (if (< rem 56) 64 128))
         (bits (* 8 n))
         (out (make-array 32 :element-type '(unsigned-byte 8))))
    (let ((iv '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
          (i 0))
      (dolist (x iv) (setf (aref h i) x) (setq i (+ i 1))))
    (let ((off 0))
      (loop (when (>= off full) (return))
            (w32b-sha256-block h w msg off)
            (setq off (+ off 64))))
    (dotimes (i rem) (setf (aref tail i) (aref msg (+ full i))))
    (setf (aref tail rem) #x80)
    (dotimes (i 8)
      (setf (aref tail (- tlen 1 i)) (ldb (byte 8 (* 8 i)) bits)))
    (w32b-sha256-block h w tail 0)
    (when (= tlen 128) (w32b-sha256-block h w tail 64))
    (dotimes (i 8 out)
      (let ((v (aref h i)))
        (setf (aref out (* 4 i))       (ldb (byte 8 24) v)
              (aref out (+ (* 4 i) 1)) (ldb (byte 8 16) v)
              (aref out (+ (* 4 i) 2)) (ldb (byte 8 8) v)
              (aref out (+ (* 4 i) 3)) (ldb (byte 8 0) v))))))

;;; ---- ChaCha20 (natrium rotl32 / %qr / %chacha20-keystream) ----------------

(declaim (inline w32b-rotl32))
(defun w32b-rotl32 (x n)
  (declare (type (unsigned-byte 32) x) (type (integer 0 31) n))
  (logand #xffffffff (logior (ash x n) (ash x (- n 32)))))

(defmacro w32b-qr (st a b c d)
  "ChaCha quarter-round on state array ST at indices A B C D."
  `(let ((va (aref ,st ,a)) (vb (aref ,st ,b)) (vc (aref ,st ,c)) (vd (aref ,st ,d)))
     (declare (type (unsigned-byte 32) va vb vc vd))
     (setf va (logand #xffffffff (+ va vb))  vd (w32b-rotl32 (logxor vd va) 16))
     (setf vc (logand #xffffffff (+ vc vd))  vb (w32b-rotl32 (logxor vb vc) 12))
     (setf va (logand #xffffffff (+ va vb))  vd (w32b-rotl32 (logxor vd va) 8))
     (setf vc (logand #xffffffff (+ vc vd))  vb (w32b-rotl32 (logxor vb vc) 7))
     (setf (aref ,st ,a) va (aref ,st ,b) vb (aref ,st ,c) vc (aref ,st ,d) vd)))

(defun w32b-chacha20-keystream (st w ks)
  "20 rounds over initial state ST (16 u32 words) in the scratch array W, add
   the original state, and serialize little-endian into the 64-byte KS."
  (dotimes (i 16) (setf (aref w i) (aref st i)))
  (dotimes (i 10)
    (w32b-qr w 0 4  8 12) (w32b-qr w 1 5  9 13) (w32b-qr w 2 6 10 14) (w32b-qr w 3 7 11 15)
    (w32b-qr w 0 5 10 15) (w32b-qr w 1 6 11 12) (w32b-qr w 2 7  8 13) (w32b-qr w 3 4  9 14))
  (dotimes (i 16 ks)
    (let ((v (logand #xffffffff (+ (aref w i) (aref st i)))) (o (* 4 i)))
      (setf (aref ks o)       (logand #xff v)
            (aref ks (+ o 1)) (logand #xff (ash v -8))
            (aref ks (+ o 2)) (logand #xff (ash v -16))
            (aref ks (+ o 3)) (logand #xff (ash v -24))))))

(defun w32b-u32le (bytes off)
  (logior (aref bytes off)
          (ash (aref bytes (+ off 1)) 8)
          (ash (aref bytes (+ off 2)) 16)
          (ash (aref bytes (+ off 3)) 24)))

(defun w32b-chacha20-xor (key nonce counter data)
  "RFC 8439 ChaCha20: DATA xor the keystream, as a fresh octet vector."
  (let* ((n (length data))
         (out (make-array n :element-type '(unsigned-byte 8)))
         (st (make-array 16 :initial-element 0))
         (w (make-array 16 :initial-element 0))
         (ks (make-array 64 :element-type '(unsigned-byte 8)))
         (ctr counter)
         (pos 0))
    (setf (aref st 0) #x61707865 (aref st 1) #x3320646e
          (aref st 2) #x79622d32 (aref st 3) #x6b206574)
    (dotimes (i 8) (setf (aref st (+ 4 i)) (w32b-u32le key (* 4 i))))
    (dotimes (i 3) (setf (aref st (+ 13 i)) (w32b-u32le nonce (* 4 i))))
    (loop (when (>= pos n) (return out))
          (setf (aref st 12) (logand #xffffffff ctr))
          (w32b-chacha20-keystream st w ks)
          (let ((m (min 64 (- n pos))))
            (dotimes (i m)
              (setf (aref out (+ pos i)) (logxor (aref data (+ pos i)) (aref ks i))))
            (setq pos (+ pos m) ctr (+ ctr 1))))))

;;; ---- inputs, output, driver -----------------------------------------------

(defun w32b-pattern (n)
  "The byte pattern (i*7+3) mod 256 -- test/crypto-sha-vectors.lisp's."
  (let ((a (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n a) (setf (aref a i) (mod (+ (* i 7) 3) 256)))))

(defun w32b-hex (v)
  (let ((s (make-string (* 2 (length v)))) (digits "0123456789abcdef"))
    (dotimes (i (length v) s)
      (setf (char s (* 2 i))       (char digits (ash (aref v i) -4))
            (char s (+ (* 2 i) 1)) (char digits (logand (aref v i) 15))))))

(defun w32b-key () (let ((k (make-array 32 :element-type '(unsigned-byte 8))))
                     (dotimes (i 32 k) (setf (aref k i) i))))
(defun w32b-nonce () (let ((v (make-array 12 :element-type '(unsigned-byte 8) :initial-element 0)))
                       (setf (aref v 7) #x4a) v))

(defun w32b-blocks (n)
  "Compress N blocks of one fixed 64-byte buffer and return H's first word.
   For instruction counts: run with two N and divide the difference, so the
   setup and everything around it cancels."
  (let ((h (make-array 8 :initial-element 0))
        (w (make-array 64 :initial-element 0))
        (blk (make-array 64 :element-type '(unsigned-byte 8) :initial-element 7)))
    (dotimes (i n (aref h 0))
      (w32b-sha256-block h w blk 0))))

(defun w32b-chacha-blocks (n)
  "N ChaCha20 keystream blocks over one fixed state; return the first word."
  (let ((st (make-array 16 :initial-element 5))
        (w (make-array 16 :initial-element 0))
        (ks (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (i n (aref w 0))
      (w32b-chacha20-keystream st w ks))))

(defun w32b-run (sha-kb chacha-kb)
  "Hash SHA-KB KiB and encrypt CHACHA-KB KiB of the pattern; print the digest
   of each result and the elapsed microseconds.  Lines are KEY=VALUE so the
   harness can grep them."
  (let* ((per internal-time-units-per-second)
         (abc (w32b-sha256 (let ((v (make-array 3 :element-type '(unsigned-byte 8))))
                             (setf (aref v 0) 97 (aref v 1) 98 (aref v 2) 99) v)))
         (rfc (w32b-chacha20-xor (w32b-key) (w32b-nonce) 1
                                 (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))))
    (format t "abc=~A~%" (w32b-hex abc))
    (format t "rfc8439-2.3.2=~A~%" (w32b-hex rfc))
    (let* ((msg (w32b-pattern (* 1024 sha-kb)))
           (t0 (get-internal-real-time))
           (d (w32b-sha256 msg))
           (t1 (get-internal-real-time)))
      (format t "sha256 bytes=~D us=~D digest=~A~%"
              (length msg) (floor (* (- t1 t0) 1000000) per) (w32b-hex d)))
    (let* ((msg (w32b-pattern (* 1024 chacha-kb)))
           (t0 (get-internal-real-time))
           (c (w32b-chacha20-xor (w32b-key) (w32b-nonce) 1 msg))
           (t1 (get-internal-real-time)))
      (format t "chacha20 bytes=~D us=~D sha256-of-ct=~A~%"
              (length msg) (floor (* (- t1 t0) 1000000) per) (w32b-hex (w32b-sha256 c))))
    t))
