;;;; arch-i386-cl.lisp — QEMU pc (i386) NE2000 adapter for the BARE-METAL
;;;; CL/mvm image built by mvm/build-i386-cl-repl.lisp (platform :i386 of
;;;; mvm/build-cl-repl-common.lisp).
;;;;
;;;; The i386 sibling of net/arch-x86-cl.lisp — same contract (the names
;;;; net/ip.lisp and http-client.lisp call), on the NE2000 ISA NIC that
;;;; net/ne2000.lisp drives.  It is NOT net/arch-i386.lisp: that file serves the
;;;; `repl-source' mini-Lisp images (a PIT-timer io-delay, an SSH capture
;;;; %serial-byte at #x540000, line-editor slots), all at addresses inside this
;;;; ~42 MB image.  Nor does this image take net/32bit-overrides.lisp, which
;;;; truncates 32-bit quantities to 30 bits: the CL runtime has bignums, and
;;;; MEM-REF :U32 reads and writes them whole on a 32-bit target.
;;;;
;;;; SPLICED AFTER net/ne2000.lisp, not before it as the other adapters are:
;;;; the driver defines its host buffers NE2K-RX-HOST / NE2K-TX-HOST at
;;;; #x780000 / #x781000 — inside this image — and under last-defun-wins only a
;;;; later definition moves them.
;;;;
;;;; Memory: 64 MB .. 66 MB (#x04000000..), DRAM above the image (1 MB ..
;;;; ~42 MB), above the handler-case block, page directory and I/O scratch
;;;; (#x03800000 .. #x03D00000), and below the heap at #x10100000.  NE2000 is
;;;; port I/O; nothing here is DMA'd by the NIC.

;; ============================================================
;; No PCI: the NE2000 is an ISA card at I/O base #x300.
;; ============================================================
(defun pci-config-read (bus dev fn reg) 0)
(defun pci-config-write (bus dev fn reg val) nil)
(defun pci-assign-bars () nil)

;; ============================================================
;; Timing / serial / entropy
;; ============================================================
;; A RAM-read spin, as on x64 (see arch-x86-cl.lisp: port-I/O spins hold QEMU's
;; big lock and starve the iothread that queues slirp's replies).
(defun io-delay ()
  (dotimes (d 5000) (mem-ref #x04060000 :u8)))

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
;; NE2000 host buffers and driver state
;; ============================================================
(defun ne2k-rx-host ()       #x04000000)
(defun ne2k-tx-host ()       #x04001000)
(defun e1000-state-base ()   #x04060000)
(defun fe-scratch-base ()    #x04060900)
(defun ssh-conn-base ()      #x04080000)
(defun ssh-ipc-base ()       #x04100000)
