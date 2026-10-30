;;;; ssh-x64-cl.lisp -- the x86-64 board's part of the SSH interface: TSC-timed
;;;; waits and RDRAND entropy.  Baked in SSH builds of the x64 CL image (UEFI
;;;; and QEMU pc); %BIND-BOARD-SSH-SEAMS, at the end, binds them at boot.

;;; ------------------------------------------------------------------
;;; Waits measured in TSC cycles, not poll counts (ssh.lisp,
;;; SSH-WAIT-EXPIRED-P).  A wait still needs its poll count AND 2^35 cycles
;;; (10-17 s at 2-3.5 GHz): on the SNP VPS a native 50000-try poll took ~15 ms
;;; and the server closed every connection right after the client's KEXINIT.
;;; Bare x64 has no counter rate (CNTFRQ is 0 there), hence a cycle budget.
;;; ------------------------------------------------------------------
(in-package :modus.board)

(defun %board-ssh-preauth-clock () (rdtsc))
(defun %board-ssh-wait-expired-p (count limit since)
  (or (modus.ssh:ssh-peer-closed-p)
      (and (> count limit) (> (- (rdtsc) since) 34359738368))))
;; The client sends its version as soon as TCP connects, so ~1-2 s (2^32
;; cycles) is generous; it bounds what a dead client costs when its RST never
;; comes back (a NAT that has forgotten the mapping drops our SYN-ACK).
(defun %board-ssh-version-wait-expired-p (count since)
  (or (modus.ssh:ssh-peer-closed-p)
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

(defun %board-arch-hw-random-fill (addr n)
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

(defun %board-ssh-no-hw-rng (b)
  (if (snp-active-p)
      (error "SSH: no RDRAND in an SEV-SNP guest; refusing to make keys")
      (modus.ssh:%drbg-weak-key b)))

(defun %bind-board-ssh-seams ()
  "Bind the SSH interface functions this file implements (modus.ssh's
   seams; MODUS.SSH::%BIND-SSH-SEAMS runs first)."
  (setf (fdefinition 'modus.ssh:arch-hw-random-fill) (function %board-arch-hw-random-fill))
  (setf (fdefinition 'modus.ssh:ssh-no-hw-rng) (function %board-ssh-no-hw-rng))
  (setf (fdefinition 'modus.ssh:ssh-preauth-clock) (function %board-ssh-preauth-clock))
  (setf (fdefinition 'modus.ssh:ssh-wait-expired-p) (function %board-ssh-wait-expired-p))
  (setf (fdefinition 'modus.ssh:ssh-version-wait-expired-p) (function %board-ssh-version-wait-expired-p)))
