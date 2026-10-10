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
;;; Hardware randomness for ssh.lisp's generator: RDRAND, through the routine
;;; boot-x64 installs (EMIT-X64-RDRAND-ROUTINE: tagged pointer at 0x1D100, 0 when
;;; CPUID says there is no RDRAND).  An SEV-SNP guest must not fall back: its
;;; host can see and steer the PIT and the TSC.
;;; ------------------------------------------------------------------
(defun %hw-random32 ()
  "32 bits from RDRAND as a fixnum, or NIL (no RDRAND, or it kept failing)."
  (if (= (logand (mem-ref #x1D100 :u32) 15) 3)
      (funcall (mem-ref #x1D100 :u64))
      nil))

(defun arch-hw-random-fill (addr n)
  (let ((ok 1) (i 0))
    (loop
      (when (or (>= i n) (zerop ok)) (return ok))
      (let ((w (%hw-random32)))
        (if (null w)
            (setq ok 0)
            (dotimes (k 4)
              (when (< (+ i k) n)
                (setf (mem-ref (+ addr i k) :u8) (logand (ash w (* -8 k)) 255))))))
      (setq i (+ i 4)))))

(defun ssh-no-hw-rng (b)
  (if (snp-active-p)
      (error "SSH: no RDRAND in an SEV-SNP guest; refusing to make keys")
      (%drbg-weak-key b)))
