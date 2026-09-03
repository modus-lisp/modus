;;;; arch-x86-cl.lisp — QEMU pc (x86-64) PCI/E1000 adapter for the BARE-METAL
;;;; CL/mvm image built by mvm/build-x64-cl-repl.lisp (platform :x64 of
;;;; mvm/build-cl-repl-common.lisp).
;;;;
;;;; This is the x86 sibling of net/arch-aarch64-cl.lisp — same contract
;;;; (the names net/e1000.lisp, ip.lisp and http-client.lisp call), retargeted
;;;; at the x86 machine model.  It is NOT net/arch-x86.lisp: that file carries
;;;; the legacy mini-Lisp runtime (its own make-array / aref / aset ...) for the
;;;; `repl-source' images and would clobber the real CL runtime here.
;;;;
;;;; Divergences from the aarch64 adapter, and nothing else:
;;;;   - PCI configuration space is reached through the legacy I/O ports
;;;;     0xCF8/0xCFC (the io-out-dword / io-in-dword primops), not ECAM.
;;;;   - BARs are left where SeaBIOS assigned them (PCI-ASSIGN-BARS is a no-op):
;;;;     the boot stub identity-maps the first 4 GB, so the E1000's MMIO BAR
;;;;     (typically FEBC0000) is addressable as-is.
;;;;   - DMA rings + driver state live at 0C000000..0C113000 — DRAM above the
;;;;     image (which loads at 100000 and is ~60 MB) and below the CL image's
;;;;     scratch buffers at 0FE00000 / 0FF00000 and the MCGC heap at 10000000;
;;;;     the build tail asserts the image never reaches it.
;;;;   - Delays and entropy come from the POST port (0x80) and the PIT counter
;;;;     (port 0x40), the way net/arch-x86.lisp does it.

;; ============================================================
;; PCI configuration space (legacy port I/O)
;; ============================================================
(defun pci-config-read (bus dev fn reg)
  (io-out-dword #xCF8
    (logior #x80000000
            (logior (ash bus 16)
                    (logior (ash dev 11)
                            (logior (ash fn 8)
                                    (logand reg #xFC))))))
  (io-in-dword #xCFC))

(defun pci-config-write (bus dev fn reg val)
  (io-out-dword #xCF8
    (logior #x80000000
            (logior (ash bus 16)
                    (logior (ash dev 11)
                            (logior (ash fn 8)
                                    (logand reg #xFC))))))
  (io-out-dword #xCFC val))

;; ============================================================
;; Timing / serial / entropy
;; ============================================================
;; A RAM-read spin (the classic ~1 us bus delay), no port I/O.  A port-0x80
;; spin here (5000 PIO exits per call) held the QEMU big lock and starved the
;; iothread that flushes slirp's queued reply into the RX ring, so the OFFER
;; landed only after the poll loop gave up.  Keep it a pure RAM read; the DHCP
;; post-reset settle is handled by re-running the client in run-net-pipeline.
(defun io-delay ()
  (dotimes (d 5000) (mem-ref #x0C060000 :u8)))

(defun %serial-byte (b)
  (write-char-serial b))

(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (io-in-byte #x80)
      (setq s (logxor (ash s 8) (logand (io-in-byte #x40) #xFF))))
    (when (zerop s) (setq s 42))
    s))

(defun print-hex-digit (n)
  (if (< n 10)
      (%serial-byte (+ n 48))
      (%serial-byte (+ n 55))))

(defun print-hex-byte (b)
  (let ((hi (logand (ash b -4) 15))
        (lo (logand b 15)))
    (print-hex-digit hi)
    (print-hex-digit lo)))

(defun print-hex32 (n)
  (print-hex-byte (logand (ash n -24) 255))
  (print-hex-byte (logand (ash n -16) 255))
  (print-hex-byte (logand (ash n -8) 255))
  (print-hex-byte (logand n 255)))

;; ============================================================
;; PCI BARs: SeaBIOS already assigned them; keep them.
;; ============================================================
(defun pci-assign-bars () nil)

;; ============================================================
;; E1000 DMA + driver state: DRAM hole between the image and the scratch
;; buffers (see the header).  Same layout/offsets as the aarch64 adapter.
;; ============================================================
(defun e1000-rx-desc-base () #x0C000000)
(defun e1000-rx-buf-base ()  #x0C001000)
(defun e1000-tx-desc-base () #x0C041000)
(defun e1000-tx-buf-base ()  #x0C041400)
(defun e1000-state-base ()   #x0C060000)
(defun fe-scratch-base ()    #x0C060900)
(defun ssh-conn-base ()      #x0C080000)
(defun ssh-ipc-base ()       #x0C100000)

;; ============================================================
;; Single-threaded stubs (see arch-aarch64-cl.lisp for why they exist)
;; ============================================================
(defun spin-lock (addr) nil)
(defun spin-unlock (addr) nil)
(defun yield () (io-delay))
(defun receive () (e1000-receive) nil)
(defun actor-spawn (fn) 0)
(defun actor-exit () nil)
(defun nfn-lookup (hash) 0)
(defun hash-of (name) 0)
(defun init-gc-helper () nil)
