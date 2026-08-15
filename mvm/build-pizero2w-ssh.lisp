;;;; build-pizero2w-ssh.lisp - Build Pi Zero 2 W SSH kernel (USB gadget mode)
;;;;
;;;; Usage: sbcl --script mvm/build-pizero2w-ssh.lisp
;;;;
;;;; Produces /tmp/piboot/kernel8.img — deploy with:
;;;;   sudo /tmp/usbboot/rpiboot -d /tmp/piboot
;;;;
;;;; DWC2 operates in USB device (gadget) mode with CDC-ECM Ethernet.
;;;; Host sees a USB Ethernet adapter. Static IP: 10.0.0.2 (host: 10.0.0.1)

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(defun read-file-text (path)
  "Read entire file as a string."
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (let ((n (read-sequence text s)))
        (subseq text 0 n)))))

(defvar *net-dir*
  (merge-pathnames "net/" *modus-base*))

;; Load: arch-raspi3b (adapter) + dwc2-device (USB gadget NIC)
;; + ip + crypto + ssh + http + overrides
(defvar *net-source*
  (format nil "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-raspi3b.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2-device.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ip.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-fast.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ssh.lisp" *net-dir*))
          (read-file-text (merge-pathnames "http.lisp" *net-dir*))
          (read-file-text (merge-pathnames "http-client.lisp" *net-dir*))
          (read-file-text (merge-pathnames "aarch64-overrides.lisp" *net-dir*))))

;;; ============================================================
;;; Build Pi Zero 2 W SSH image (DWC2 USB gadget + CDC-ECM)
;;; ============================================================

(in-package :modus.mvm)

;; Install the AArch64 translator
(install-aarch64-translator)

;; Use mini UART (0x3F215040) instead of PL011 (0x3F201000) for write-byte
;; Mini UART clock is always running; PL011 clock needs mailbox setup
(setf *aarch64-serial-base* #x3F215040)
;; BCM2837 peripherals require 32-bit stores (byte stores silently ignored)
(setf *aarch64-serial-width* 2)
;; Poll AUX_MU_LSR (base+0x14) bit 5 (TX empty) before each write
;; Mini UART TX FIFO is only 8 deep — writes without poll are silently dropped
(setf *aarch64-serial-tx-poll* '(#x14 5 :tbz))
;; Mini UART RX: AUX_MU_LSR bit 0 = data ready, so wait while CLEAR.
;; Without this the image PRINTS but never READS — the RX poll used to be
;; hardcoded to PL011 (0x18 / bit 4 / :tbnz), which QEMU raspi3b satisfies
;; and real Pi Zero 2 W hardware does not.
(setf *aarch64-serial-rx-poll* '(#x14 0 :tbz))

(let* ((ssh-main (format nil "~{~A~%~}"
                        (list
                         "(defun kernel-main ()"
                         ;; Clear SSH IPC flags — uninitialized memory may suppress serial output
                         "  (setf (mem-ref #x01100014 :u32) 0)"
                         ;; Clear eval globals pointer — uninitialized RAM on real hardware causes hang
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60000) :u64) 0)"
                         ;; === Set GPIO14=ALT5 (mini UART TX), GPIO15=ALT5 (mini UART RX) ===
                         ;; GPFSEL1: GPIO14 bits[14:12]=010(ALT5), GPIO15 bits[17:15]=010(ALT5)
                         "  (setf (mem-ref #x3F200004 :u32) #x12000)"
                         ;; === Mini UART init ===
                         ;; AUX_ENABLES (0x3F215004) bit 0 = mini UART enable
                         "  (setf (mem-ref #x3F215004 :u32) 1)"
                         ;; AUX_MU_CNTL = 0 (disable TX/RX during config)
                         "  (setf (mem-ref #x3F215060 :u32) 0)"
                         ;; AUX_MU_LCR = 3 (8-bit mode)
                         "  (setf (mem-ref #x3F21504C :u32) 3)"
                         ;; AUX_MU_MCR = 0
                         "  (setf (mem-ref #x3F215050 :u32) 0)"
                         ;; AUX_MU_IER = 0 (no interrupts)
                         "  (setf (mem-ref #x3F215044 :u32) 0)"
                         ;; AUX_MU_IIR = 0xC6 (clear FIFOs)
                         "  (setf (mem-ref #x3F215048 :u32) #xC6)"
                         ;; AUX_MU_BAUD = 270 (250MHz / 8 / 115200 - 1)
                         "  (setf (mem-ref #x3F215068 :u32) 270)"
                         ;; AUX_MU_CNTL = 3 (enable TX+RX)
                         "  (setf (mem-ref #x3F215060 :u32) 3)"
                         ;; UART ready marker
                         "  (write-byte 85) (write-byte 65) (write-byte 82)"
                         "  (write-byte 84) (write-byte 10)"
                         ;; Initialize USB gadget + CDC-ECM Ethernet
                         "  (cdc-ether-init)"
                         ;; Set DNS server to 8.8.8.8 (works with host NAT)
                         "  (setf (mem-ref (+ (e1000-state-base) #x58) :u32) #x08080808)"
                         "  (write-byte 91) (write-byte 49) (write-byte 93)"
                         "  (sha256-init)"
                         "  (write-byte 91) (write-byte 50) (write-byte 93)"
                         "  (sha512-init)"
                         "  (write-byte 91) (write-byte 51) (write-byte 93)"
                         ;; Clear ed25519 init flag — uninitialized RAM on real hardware
                         ;; may be non-zero, causing ed25519-init to skip initialization
                         "  (setf (mem-ref (+ (e1000-state-base) #x5D0) :u32) 0)"
                         "  (ed25519-init)"
                         "  (write-byte 91) (write-byte 52) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (write-byte 91) (write-byte 53) (write-byte 93)"
                         ;; Ed25519 self-test: sign empty message with zero key, verify
                         "  (ed25519-test)"
                         "  (write-byte 91) (write-byte 54) (write-byte 93)"
                         ;; Host key inline
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x710) :u64) 0)"
                         "    (setf (mem-ref (+ state #x718) :u64) 0)"
                         "    (setf (mem-ref (+ state #x720) :u64) 0)"
                         "    (setf (mem-ref (+ state #x728) :u64) 0)"
                         "    (setf (mem-ref (+ state #x730) :u32) #xBC276A3B)"
                         "    (setf (mem-ref (+ state #x734) :u32) #x2DA4B6CE)"
                         "    (setf (mem-ref (+ state #x738) :u32) #xD0A8A362)"
                         "    (setf (mem-ref (+ state #x73C) :u32) #x730D6F2A)"
                         "    (setf (mem-ref (+ state #x740) :u32) #x77153265)"
                         "    (setf (mem-ref (+ state #x744) :u32) #xA643E21D)"
                         "    (setf (mem-ref (+ state #x748) :u32) #xA148C03A)"
                         "    (setf (mem-ref (+ state #x74C) :u32) #x29DA598B)"
                         "    (setf (mem-ref (+ state #x624) :u32) 1))"
                         "  (write-byte 91) (write-byte 57) (write-byte 93)"
                         ;; Pre-compute ed25519 host key derivatives (s, prefix)
                         ;; This saves ~5s of crypto during SSH key exchange
                         "  (pre-compute-host-sign)"
                         "  (write-byte 91) (write-byte 66) (write-byte 93)"
                         "  (write-byte 83) (write-byte 83) (write-byte 72)"
                         "  (write-byte 58) (print-dec 22) (write-byte 10)"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         "  (write-byte 91) (write-byte 65) (write-byte 93)"
                         ;; Pre-compute server ephemeral X25519 key pair
                         ;; Must be after conn-base clear and ssh-seed-random
                         "  (pre-compute-server-eph (conn-ssh 0))"
                         "  (write-byte 91) (write-byte 67) (write-byte 93)"
                         ;; Enable ARM timer for WFI-based io-delay (after all crypto init)
                         "  (enable-rpi-timer)"
                         "  (net-actor-main))"
)))
       ;; SSH kernel-main MUST be LAST: "last-defun-wins" makes it the entry point.
       (combined-source (concatenate 'string
                                      cl-user::*net-source*
                                      *repl-source*
                                      ssh-main)))
  (format t "Building Pi Zero 2 W SSH image (DWC2 USB gadget)...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :rpi :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (write-kernel-image image "/tmp/piboot/kernel8.img")
    (format t "Done. Deploy with:~%")
    (format t "  sudo /tmp/usbboot/rpiboot -d /tmp/piboot~%")
    (format t "Then configure host networking:~%")
    (format t "  sudo ip addr add 10.0.0.1/24 dev usb0~%")
    (format t "  sudo ip link set usb0 up~%")
    (format t "  ssh test@10.0.0.2~%")))
