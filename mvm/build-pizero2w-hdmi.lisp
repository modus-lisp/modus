;;;; build-pizero2w-hdmi.lisp - Build Pi Zero 2 W HDMI framebuffer demo
;;;;
;;;; Usage: sbcl --script mvm/build-pizero2w-hdmi.lisp
;;;;
;;;; Produces /tmp/piboot/kernel8.img — deploy via SD card:
;;;;   ./scripts/build-pizero2w.sh --hdmi
;;;;
;;;; Or manually:
;;;;   sudo dd if=/tmp/pizero2w.img of=/dev/sdX bs=4M status=progress
;;;;
;;;; Shows colored rectangles + "MODUS" text on HDMI display.
;;;; No UART needed — pure HDMI output.

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

;; We need arch-raspi3b.lisp for base primitives (write-byte, print-hex, alloc,
;; array ops) but NOT the full dwc2-device.lisp — just the mailbox helpers.
;; bcm2835-periph.lisp provides fb-init, fb-pixel, fb-clear, fb-fill-rect.
(defvar *hdmi-source*
  (format nil "~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-raspi3b.lisp" *net-dir*))
          ;; Minimal mailbox stubs (bcm2835-periph.lisp needs mbox-buf, dwc2-read, dwc2-write)
          "(defun mbox-base () #x3F00B880)
(defun mbox-read () (mbox-base))
(defun mbox-status () (+ (mbox-base) #x18))
(defun mbox-write () (+ (mbox-base) #x20))
(defun mbox-write-status () (+ (mbox-base) #x38))
(defun mbox-buf () #x01050100)
(defun dwc2-read (addr) (mem-ref addr :u32))
(defun dwc2-write (addr val) (setf (mem-ref addr :u32) val))
"
          (read-file-text (merge-pathnames "bcm2835-periph.lisp" *net-dir*))))

;;; ============================================================
;;; Build HDMI demo kernel
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

(let* ((hdmi-main (format nil "~{~A~%~}"
                        (list
                         "(defun kernel-main ()"
                         ;; Clear SSH IPC flags (uninitialized RAM)
                         "  (setf (mem-ref #x01100014 :u32) 0)"
                         ;; === Init framebuffer: 1280x720 32bpp ===
                         "  (fb-init 1280 720)"
                         ;; Draw test pattern
                         "  (let ((fb-addr (mem-ref (fb-state) :u32)))"
                         "    (when (not (zerop fb-addr))"
                         ;; Clear to dark background
                         "      (fb-clear 16 16 32)"
                         ;; === "MODUS" in large block letters ===
                         ;; Each letter is 80px wide, 120px tall, starting at y=80
                         ;; Total width: 7 letters * 80 + 6 gaps * 16 = 656px
                         ;; Center horizontally: (1280-656)/2 = 312
                         ;; M
                         "      (fb-fill-rect 312 80 16 120 0 200 255)"
                         "      (fb-fill-rect 376 80 16 120 0 200 255)"
                         "      (fb-fill-rect 328 96 16 40 0 200 255)"
                         "      (fb-fill-rect 344 112 16 24 0 200 255)"
                         "      (fb-fill-rect 360 96 16 40 0 200 255)"
                         ;; O
                         "      (fb-fill-rect 408 80 16 120 0 200 255)"
                         "      (fb-fill-rect 472 80 16 120 0 200 255)"
                         "      (fb-fill-rect 424 80 48 16 0 200 255)"
                         "      (fb-fill-rect 424 184 48 16 0 200 255)"
                         ;; D
                         "      (fb-fill-rect 504 80 16 120 0 200 255)"
                         "      (fb-fill-rect 520 80 32 16 0 200 255)"
                         "      (fb-fill-rect 520 184 32 16 0 200 255)"
                         "      (fb-fill-rect 552 96 16 88 0 200 255)"
                         ;; U
                         "      (fb-fill-rect 584 80 16 120 0 200 255)"
                         "      (fb-fill-rect 648 80 16 120 0 200 255)"
                         "      (fb-fill-rect 600 184 48 16 0 200 255)"
                         ;; S
                         "      (fb-fill-rect 680 80 80 16 0 200 255)"
                         "      (fb-fill-rect 680 80 16 56 0 200 255)"
                         "      (fb-fill-rect 680 128 80 16 0 200 255)"
                         "      (fb-fill-rect 744 144 16 56 0 200 255)"
                         "      (fb-fill-rect 680 184 80 16 0 200 255)"
                         ;; 6
                         "      (fb-fill-rect 776 80 80 16 0 200 255)"
                         "      (fb-fill-rect 776 80 16 120 0 200 255)"
                         "      (fb-fill-rect 776 128 80 16 0 200 255)"
                         "      (fb-fill-rect 840 144 16 56 0 200 255)"
                         "      (fb-fill-rect 776 184 80 16 0 200 255)"
                         ;; 4
                         "      (fb-fill-rect 872 80 16 72 0 200 255)"
                         "      (fb-fill-rect 936 80 16 120 0 200 255)"
                         "      (fb-fill-rect 872 128 80 16 0 200 255)"
                         ;; === Colored bars below text ===
                         "      (fb-fill-rect 312 240 100 40 255 0 0)"
                         "      (fb-fill-rect 424 240 100 40 0 255 0)"
                         "      (fb-fill-rect 536 240 100 40 64 128 255)"
                         "      (fb-fill-rect 648 240 100 40 255 255 0)"
                         "      (fb-fill-rect 760 240 100 40 255 0 255)"
                         "      (fb-fill-rect 872 240 80 40 0 255 255)"
                         ;; === Subtitle: "bare metal lisp" in smaller blocks ===
                         ;; Simple underline bar
                         "      (fb-fill-rect 412 300 530 4 100 100 140)"
                         ;; === Corner markers ===
                         "      (fb-fill-rect 0 0 40 4 255 255 255)"
                         "      (fb-fill-rect 0 0 4 40 255 255 255)"
                         "      (fb-fill-rect 1240 0 40 4 255 255 255)"
                         "      (fb-fill-rect 1276 0 4 40 255 255 255)"
                         "      (fb-fill-rect 0 716 40 4 255 255 255)"
                         "      (fb-fill-rect 0 680 4 40 255 255 255)"
                         "      (fb-fill-rect 1240 716 40 4 255 255 255)"
                         "      (fb-fill-rect 1276 680 4 40 255 255 255)"
                         "))"
                         ;; Activity LED on (visible confirmation)
                         "  (led-init) (led-on)"
                         ;; Halt — infinite loop
                         "  (loop (delay-us 1000000)))")))
       (combined-source (concatenate 'string
                                      hdmi-main
                                      cl-user::*hdmi-source*
                                      *repl-source*)))
  (format t "Building Pi Zero 2 W HDMI demo...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :rpi :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (ensure-directories-exist "/tmp/piboot/kernel8.img")
    (write-kernel-image image "/tmp/piboot/kernel8.img")
    (format t "Done. Create SD card image with:~%")
    (format t "  ./scripts/build-pizero2w.sh --hdmi~%")
    (format t "Or deploy via rpiboot:~%")
    (format t "  sudo rpiboot -d /tmp/piboot~%")))
