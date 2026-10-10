;;;; ssh-x64-cl.lisp -- the x86-64 CL image's overrides of net/ssh.lisp.
;;;; Spliced right after ssh.lisp in SSH builds of the x64 CL image (UEFI and
;;;; QEMU pc), so these win by last-defun-wins.

;;; ------------------------------------------------------------------
;;; Waits measured in TSC cycles, not poll counts (ssh.lisp,
;;; SSH-WAIT-EXPIRED-P).  A wait still needs its poll count AND 2^35 cycles
;;; (10-17 s at 2-3.5 GHz): on the SNP VPS a native 50000-try poll took ~15 ms
;;; and the server closed every connection right after the client's KEXINIT.
;;; Bare x64 has no counter rate (CNTFRQ is 0 there), hence a cycle budget.
;;; ------------------------------------------------------------------
(defun ssh-preauth-clock () (rdtsc))
(defun ssh-wait-expired-p (count limit since)
  (or (ssh-peer-closed-p)
      (and (> count limit) (> (- (rdtsc) since) 34359738368))))
;; The client sends its version as soon as TCP connects, so ~1-2 s (2^32
;; cycles) is generous; it bounds what a dead client costs when its RST never
;; comes back (a NAT that has forgotten the mapping drops our SYN-ACK).
(defun ssh-version-wait-expired-p (count since)
  (or (ssh-peer-closed-p)
      (and (> count 50) (> (- (rdtsc) since) 4294967296))))

;;; ------------------------------------------------------------------
;;; Randomness.  ssh.lisp's SSH-RANDOM is a 32-bit xorshift whose state is the
;;; per-connection word ssh+0x2C, and SSH-SEED-RANDOM's seed went to a different
;;; word, so the state at boot was zero and the generator started from the
;;; constant 12345 -- which made the server's "ephemeral" X25519 key the same,
;;; computable value on every boot.  Here SSH-RANDOM is SHA-512 in counter mode
;;; over a 512-bit key drawn from RDRAND (the routine boot-x64 installs; see
;;; EMIT-X64-RDRAND-ROUTINE), and under SEV-SNP there is no fallback: the host
;;; can see and steer the PIT and the TSC, so without RDRAND no key is made.
;;;
;;; State, at e1000-state-base +0x1800 (CPU-only memory, zeroed by the pipeline):
;;;   +0x00 key (64)   +0x40 counter u32   +0x44 pool index u32 (64 = empty)
;;;   +0x48 pool (64)  +0x88 seeded flag u32
;;; ------------------------------------------------------------------
(defun %drbg-base () (+ (e1000-state-base) #x1800))

(defun %hw-random32 ()
  "32 bits from RDRAND as a fixnum, or NIL (no RDRAND, or it kept failing)."
  (if (= (logand (mem-ref #x1D100 :u32) 15) 3)
      (funcall (mem-ref #x1D100 :u64))
      nil))

(defun ssh-seed-random ()
  (let ((b (%drbg-base)) (weak 0))
    (dotimes (i 16)
      (let ((r (%hw-random32)))
        (when (null r)
          (when (snp-active-p)
            (error "SSH: no RDRAND under SEV-SNP; refusing to make keys"))
          (setq weak 1)
          (setq r (logand (logxor (rdtsc) (ash (arch-seed-random) 16)) #xFFFFFFFF)))
        (setf (mem-ref (+ b (* i 4)) :u32) r)))
    (when (= weak 1)
      (write-string-serial "SSH: NO RDRAND -- WEAK ENTROPY (PIT/TSC)") (write-char-serial 10))
    (setf (mem-ref (+ b #x40) :u32) 0)
    (setf (mem-ref (+ b #x44) :u32) 64)
    (setf (mem-ref (+ b #x88) :u32) 1)))

(defun %drbg-refill (b)
  (let ((in (make-array 68)) (c (mem-ref (+ b #x40) :u32)))
    (dotimes (k 64) (aset in k (mem-ref (+ b k) :u8)))
    (aset in 64 (logand c 255))
    (aset in 65 (logand (ash c -8) 255))
    (aset in 66 (logand (ash c -16) 255))
    (aset in 67 (logand (ash c -24) 255))
    (setf (mem-ref (+ b #x40) :u32) (logand (+ c 1) #xFFFFFFFF))
    (let ((h (sha512 in)))
      (dotimes (k 64) (setf (mem-ref (+ b #x48 k) :u8) (aref h k))))
    (setf (mem-ref (+ b #x44) :u32) 0)))

(defun ssh-random (ssh)
  (let ((b (%drbg-base)))
    (when (not (= (mem-ref (+ b #x88) :u32) 1))
      (ssh-seed-random))
    (when (>= (mem-ref (+ b #x44) :u32) 64)
      (%drbg-refill b))
    (let ((i (mem-ref (+ b #x44) :u32)))
      (setf (mem-ref (+ b #x44) :u32) (+ i 1))
      (mem-ref (+ b #x48 i) :u8))))
