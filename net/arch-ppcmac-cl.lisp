;;;; arch-ppcmac-cl.lisp -- the bare PowerPC Mac's board adapter for the net
;;;; stack: the names net/ip.lisp and net/ssh.lisp call, for the CL image
;;;; mvm/build-ppcmac-cl-repl.lisp builds (platform :ppcmac), whose NIC is the
;;;; GMAC (net/sungem.lisp).  The i386 adapter (arch-i386-cl.lisp) is its model.
;;;;
;;;; Memory: #x07200000.. is free DRAM in boot/boot-ppcmac.lisp's map (between
;;;; the handler-frame extension, which ends at #x07200000, and NIL's block at
;;;; #x07800000), identity-mapped by DBAT0, cacheable and snooped:
;;;;   #x07200000..#x07240000  the GMAC's rings and buffers (sungem.lisp)
;;;;   #x07300000..            the NIC state block (E1000-STATE-BASE: MAC at +8,
;;;;                           IP +#x18, gateway +#x1C, ...), FE-SCRATCH at +#x900
;;;;   #x07310000..            the SSH connection block
;;;;   #x07380000..#x07416000  SSH-IPC (76800 words)
;;;; None of it is inside the image (#x10000..~#x1620000), the native tier's area
;;;; (#x02800000..#x04000000), the kind maps' (#x04000000..#x04800000) or the
;;;; payload (#x04800000..).

(in-package :modus.board)

;; no PCI configuration: Open Firmware assigned and enabled the GMAC
(defun pci-config-read (bus dev fn reg) 0)
(defun pci-config-write (bus dev fn reg val) nil)
(defun pci-assign-bars () nil)

;; a RAM-read spin (no port I/O on a PowerPC)
(defun io-delay ()
  (dotimes (d 2000) (mem-ref #x07300000 :u8)))

(defun %serial-byte (b)
  (write-char-serial b))

;; the timebase's low bits, a few times over, spaced by RAM reads
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (dotimes (k (+ 50 (* i 37))) (mem-ref #x07300000 :u8))
      (setq s (logxor (ash (logand s #xFFFFFF) 8) (logand (get-internal-real-time) #xFF))))
    (when (zerop s) (setq s 42))
    s))

(defun print-hex-digit (n)
  (if (< n 10)
      (%serial-byte (+ n 48))
      (%serial-byte (+ n 55))))

(defun print-hex-byte (b)
  (print-hex-digit (logand (ash b -4) 15))
  (print-hex-digit (logand b 15)))

(defun print-hex32 (n)
  (print-hex-byte (logand (ash n -24) 255))
  (print-hex-byte (logand (ash n -16) 255))
  (print-hex-byte (logand (ash n -8) 255))
  (print-hex-byte (logand n 255)))

(declaim (overridable e1000-state-base))
(defun e1000-state-base ()   #x07300000)
(declaim (overridable fe-scratch-base))
(defun fe-scratch-base ()    #x07300900)
(declaim (overridable ssh-conn-base))
(defun ssh-conn-base ()      #x07310000)
(declaim (overridable ssh-ipc-base))
(defun ssh-ipc-base ()       #x07380000)
