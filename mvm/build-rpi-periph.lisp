;;;; build-rpi-periph.lisp - Build RPi BCM2835 peripheral diagnostic kernel
;;;;
;;;; Usage: sbcl --script mvm/build-rpi-periph.lisp
;;;;
;;;; Produces /tmp/piboot/kernel8.img — test with:
;;;;   qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \
;;;;       -serial null -serial stdio -display gtk
;;;;
;;;; Tests: Hardware RNG, System Timer, Activity LED, GPU Framebuffer.
;;;; All output via mini UART (serial1 in QEMU).

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

;; Load order: arch-raspi3b → dwc2-device (for mailbox) → bcm2835-periph
(defvar *periph-source*
  (format nil "~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-raspi3b.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2-device.lisp" *net-dir*))
          (read-file-text (merge-pathnames "bcm2835-periph.lisp" *net-dir*))))

;;; ============================================================
;;; Build diagnostic kernel
;;; ============================================================

(in-package :modus.mvm)

;; Install the AArch64 translator
(install-aarch64-translator)

;; Use mini UART (0x3F215040) instead of PL011 (0x3F201000)
(setf *aarch64-serial-base* #x3F215040)
;; BCM2837 peripherals require 32-bit stores
(setf *aarch64-serial-width* 2)
;; Poll AUX_MU_LSR (base+0x14) bit 5 (TX empty) before each write
(setf *aarch64-serial-tx-poll* '(#x14 5 :tbz))
;; Mini UART RX: AUX_MU_LSR bit 0 = data ready, so wait while CLEAR.
;; Without this the image PRINTS but never READS — the RX poll used to be
;; hardcoded to PL011 (0x18 / bit 4 / :tbnz), which QEMU raspi3b satisfies
;; and real Pi Zero 2 W hardware does not.
(setf *aarch64-serial-rx-poll* '(#x14 0 :tbz))

(let* ((diag-main (format nil "~{~A~%~}"
                        (list
                         "(defun kernel-main ()"
                         ;; NOTE: Do NOT write PM_RSTC (0x3F10001C) here — QEMU interprets
                         ;; 0x5A000020 as a full machine reset, causing an infinite reboot loop.
                         ;; Watchdog disable is only needed on real hardware (pizero2w build).
                         ;; Clear SSH IPC flags
                         "  (setf (mem-ref #x01100014 :u32) 0)"
                         ;; === Set GPIO14=ALT5 (mini UART TX), GPIO15=ALT5 (mini UART RX) ===
                         "  (setf (mem-ref #x3F200004 :u32) #x12000)"
                         ;; === Mini UART init ===
                         "  (setf (mem-ref #x3F215004 :u32) 1)"
                         "  (setf (mem-ref #x3F215060 :u32) 0)"
                         "  (setf (mem-ref #x3F21504C :u32) 3)"
                         "  (setf (mem-ref #x3F215050 :u32) 0)"
                         "  (setf (mem-ref #x3F215044 :u32) 0)"
                         "  (setf (mem-ref #x3F215048 :u32) #xC6)"
                         "  (setf (mem-ref #x3F215068 :u32) 270)"
                         "  (setf (mem-ref #x3F215060 :u32) 3)"
                         ;; Print "UART\n"
                         "  (write-byte 85) (write-byte 65) (write-byte 82)"
                         "  (write-byte 84) (write-byte 10)"
                         ;; === Test 1: Hardware RNG ===
                         ;; Print "RNG:"
                         "  (write-byte 82) (write-byte 78) (write-byte 71) (write-byte 58)"
                         "  (let ((rnd (arch-seed-random)))"
                         "    (print-hex32 rnd))"
                         "  (write-byte 10)"
                         ;; === Test 2: System Timer ===
                         ;; Print "TMR:"
                         "  (write-byte 84) (write-byte 77) (write-byte 82) (write-byte 58)"
                         "  (let ((t1 (timer-read-lo)))"
                         "    (print-hex32 t1))"
                         "  (write-byte 10)"
                         ;; Marker before io-delay: "D!"
                         "  (write-byte 68) (write-byte 33)"
                         ;; io-delay now pure counter loop (no MMIO)
                         "  (io-delay)"
                         ;; Print "DLY:" + elapsed time
                         "  (write-byte 68) (write-byte 76) (write-byte 89) (write-byte 58)"
                         "  (let ((t2 (timer-read-lo)))"
                         "    (print-hex32 t2))"
                         "  (write-byte 10)"
                         ;; === Test 3: Activity LED ===
                         ;; Print "LED:"
                         "  (write-byte 76) (write-byte 69) (write-byte 68) (write-byte 58)"
                         "  (led-init)"
                         "  (led-blink 3 200)"
                         "  (led-on)"
                         ;; Print "OK\n"
                         "  (write-byte 79) (write-byte 75) (write-byte 10)"
                         ;; === Test 4: GPU Framebuffer ===
                         ;; Print "FB:"
                         "  (write-byte 70) (write-byte 66) (write-byte 58)"
                         "  (let ((fb-addr (fb-init 640 480)))"
                         "    (print-hex32 fb-addr))"
                         "  (write-byte 10)"
                         ;; Draw test pattern: colored rectangles
                         "  (let ((fb-addr (mem-ref (fb-state) :u32)))"
                         "    (when (not (zerop fb-addr))"
                         ;; Clear to dark blue
                         "      (fb-clear 0 0 64)"
                         ;; Red rectangle top-left
                         "      (fb-fill-rect 20 20 200 100 255 0 0)"
                         ;; Green rectangle center
                         "      (fb-fill-rect 240 20 200 100 0 255 0)"
                         ;; Blue rectangle top-right
                         "      (fb-fill-rect 460 20 160 100 0 128 255)"
                         ;; White rectangle bottom
                         "      (fb-fill-rect 20 200 600 50 255 255 255)"
                         ;; Yellow rectangle
                         "      (fb-fill-rect 20 280 200 100 255 255 0)"
                         ;; Magenta rectangle
                         "      (fb-fill-rect 240 280 200 100 255 0 255)"
                         ;; Cyan rectangle
                         "      (fb-fill-rect 460 280 160 100 0 255 255)))"
                         ;; Print "DONE\n"
                         "  (write-byte 68) (write-byte 79) (write-byte 78)"
                         "  (write-byte 69) (write-byte 10)"
                         ;; Halt — infinite loop
                         "  (loop (delay-us 1000000)))")))
       (combined-source (concatenate 'string
                                      diag-main
                                      cl-user::*periph-source*
                                      *repl-source*)))
  (format t "Building RPi BCM2835 peripheral diagnostic...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :rpi :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (ensure-directories-exist "/tmp/piboot/kernel8.img")
    (write-kernel-image image "/tmp/piboot/kernel8.img")
    (format t "Done. Test with:~%")
    (format t "  qemu-system-aarch64 -M raspi3b -kernel /tmp/piboot/kernel8.img \\~%")
    (format t "    -serial null -serial stdio -display gtk~%")))
