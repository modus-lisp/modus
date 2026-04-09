;;;; build-rpi-ssh.lisp - Build Raspberry Pi SSH kernel image (DWC2 USB + CDC Ethernet)
;;;;
;;;; Usage: sbcl --script mvm/build-rpi-ssh.lisp
;;;;
;;;; Produces /tmp/kernel8-ssh.img — boot with:
;;;;   qemu-system-aarch64 -machine raspi3b -kernel /tmp/kernel8-ssh.img \
;;;;     -display none -serial stdio \
;;;;     -device usb-net,netdev=net0 \
;;;;     -netdev 'user,id=net0,hostfwd=tcp::2222-:22'

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

;; Load: arch-raspi3b (adapter) + dwc2 (HCD) + usb (core) + cdc-ether (NIC)
;; + ip + crypto + ssh + overrides
;; Note: e1000.lisp is NOT loaded — cdc-ether.lisp provides the same interface
(defvar *net-source*
  (format nil "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-raspi3b.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2.lisp" *net-dir*))
          (read-file-text (merge-pathnames "usb.lisp" *net-dir*))
          (read-file-text (merge-pathnames "cdc-ether.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ip.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto-fast.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ssh.lisp" *net-dir*))
          (read-file-text (merge-pathnames "aarch64-overrides.lisp" *net-dir*))))

;;; ============================================================
;;; Build RPi SSH image (raspi3b + DWC2 USB + CDC Ethernet)
;;; ============================================================

(in-package :modus.mvm)

;; Install the AArch64 translator
(install-aarch64-translator)
;; NOTE: *aarch64-yield-nop* NOT set — SEV+WFE should work on raspi3b
;; as long as no WFI is used (io-delay and yield both busy-wait)

;; Combine: net source + REPL source + SSH kernel-main
;; SSH kernel-main MUST be LAST: "last-defun-wins" makes it the entry point.
(let* ((ssh-main (format nil "~{~A~%~}"
                        (list
                         "(defun kernel-main ()"
                         ;; No PCI on RPi — pci-assign-bars is a no-op
                         "  (pci-assign-bars)"
                         ;; Initialize USB + CDC Ethernet (replaces e1000-probe)
                         "  (cdc-ether-init)"
                         ;; "  (dwc2-enable-host-irq)"
                         "  (write-byte 91) (write-byte 49) (write-byte 93)"
                         "  (sha256-init)"
                         "  (write-byte 91) (write-byte 50) (write-byte 93)"
                         "  (sha512-init)"
                         "  (write-byte 91) (write-byte 51) (write-byte 93)"
                         "  (ed25519-init)"
                         "  (write-byte 91) (write-byte 52) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (write-byte 91) (write-byte 53) (write-byte 93)"
                         "  (dhcp-client)"
                         "  (write-byte 91) (write-byte 54) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (ssh-init-strings)"
                         ;; Embed pre-computed Ed25519 host key (same as AArch64 virt)
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
                         ;; Pre-compute ed25519 host key derivatives (s, prefix)
                         "  (pre-compute-host-sign)"
                         "  (write-byte 83) (write-byte 83) (write-byte 72)"
                         "  (write-byte 58) (print-dec 22) (write-byte 10)"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         ;; Embed pre-computed ephemeral X25519 key pair (same as AArch64 virt)
                         ;; pre-compute-server-eph too slow on QEMU raspi3b — hardcode instead
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x6C4) :u8) #x00)"
                         "    (dotimes (i 30)"
                         "      (setf (mem-ref (+ state (+ #x6C5 i)) :u8) #x01))"
                         "    (setf (mem-ref (+ state #x6E3) :u8) #x41))"
                         "  (let ((state (e1000-state-base)))"
                         "    (setf (mem-ref (+ state #x6E4) :u32) #x9292E0A4)"
                         "    (setf (mem-ref (+ state #x6E8) :u32) #x78C251B6)"
                         "    (setf (mem-ref (+ state #x6EC) :u32) #x562C77B9)"
                         "    (setf (mem-ref (+ state #x6F0) :u32) #xBBA95F9F)"
                         "    (setf (mem-ref (+ state #x6F4) :u32) #xB406D913)"
                         "    (setf (mem-ref (+ state #x6F8) :u32) #x9D8CB66A)"
                         "    (setf (mem-ref (+ state #x6FC) :u32) #x442BDCF9)"
                         "    (setf (mem-ref (+ state #x700) :u32) #x99F8D42C))"
                         ;; enable-rpi-timer skipped: WFI doesn't wake on QEMU raspi3b
                         "  (net-actor-main))")))
       ;; SSH kernel-main MUST be LAST: "last-defun-wins" makes it the entry point.
       (combined-source (concatenate 'string
                                      cl-user::*net-source*
                                      *repl-source*
                                      ssh-main)))
  (format t "Building RPi SSH image (raspi3b + DWC2 USB)...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :rpi :source-text combined-source)))
    ;; Debug: show entry point info
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (write-kernel-image image "/tmp/kernel8-ssh.img")
    (format t "Done. Boot with:~%")
    (format t "  qemu-system-aarch64 -machine raspi3b -kernel /tmp/kernel8-ssh.img \\~%")
    (format t "    -display none -serial stdio \\~%")
    (format t "    -device usb-net,netdev=net0 \\~%")
    (format t "    -netdev 'user,id=net0,hostfwd=tcp::2222-:22'~%")))
