;;;; build-rpi-hid.lisp - Build Raspberry Pi HID kernel image (DWC2 USB + Keyboard/Mouse/Tablet)
;;;;
;;;; Usage: sbcl --script mvm/build-rpi-hid.lisp
;;;;
;;;; Produces /tmp/kernel8-hid.img — boot with:
;;;;   qemu-system-aarch64 -machine raspi3b -kernel /tmp/kernel8-hid.img \
;;;;     -display none -serial stdio \
;;;;     -device usb-kbd

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

;; Load: arch-raspi3b (adapter) + dwc2 (HCD) + usb (core) + hid (driver)
;; No networking stack (ip/crypto/ssh/cdc-ether) needed for HID-only build
(defvar *hid-source*
  (format nil "~A~%~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-raspi3b.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2.lisp" *net-dir*))
          (read-file-text (merge-pathnames "usb.lisp" *net-dir*))
          (read-file-text (merge-pathnames "hid.lisp" *net-dir*))))

;;; ============================================================
;;; Build RPi HID image (raspi3b + DWC2 USB + Keyboard)
;;; ============================================================

(in-package :modus.mvm)

;; Install the AArch64 translator
(install-aarch64-translator)

;; kernel-main: init HID, enter REPL
;; kernel-main MUST be first: boot code falls through to it.
;; HID source loads AFTER repl-source — its read-char-input wins.
(let* ((hid-main (format nil "~{~A~%~}"
                         (list
                          "(defun kernel-main ()"
                          "  (hid-init)"
                          "  (%serial-byte 82) (%serial-byte 69) (%serial-byte 80) (%serial-byte 76)"
                          "  (%serial-byte 10)"
                          "  (let ((globals (cons nil nil)))"
                          "    (repl globals)))")))
       ;; kernel-main first, then REPL source, then HID source.
       ;; Last defun of a given name wins. hid.lisp defines read-char-input
       ;; which overrides the default serial-only version from repl-source.
       (combined-source (concatenate 'string
                                      hid-main
                                      *repl-source*
                                      cl-user::*hid-source*)))
  (format t "Building RPi HID image (raspi3b + DWC2 USB)...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :rpi :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (write-kernel-image image "/tmp/kernel8-hid.img")
    (format t "Done. Boot with:~%")
    (format t "  qemu-system-aarch64 -machine raspi3b -kernel /tmp/kernel8-hid.img \\~%")
    (format t "    -display none -serial stdio \\~%")
    (format t "    -device usb-kbd~%")))
