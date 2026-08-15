;;;; build-uart-bootloader.lisp - Build permanent UART bootloader for Pi Zero 2 W
;;;;
;;;; Usage: sbcl --script mvm/build-uart-bootloader.lisp
;;;;
;;;; Produces /tmp/piboot/kernel8.img — flash to SD card once:
;;;;   bash scripts/make-sdcard.sh
;;;;   sudo dd if=/tmp/pizero2w-sdcard.img of=/dev/sdX bs=4M
;;;;
;;;; On boot: waits ~2s for UART kernel upload (magic 0x55).
;;;; If no upload: falls through to built-in SSH server.
;;;;
;;;; Deploy new kernels from Pi 5:
;;;;   python3 scripts/deploy-kernel.py /path/to/kernel8.img

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

;; Bootloader sources: periph drivers (timer, LED) + UART bootloader
(defvar *bootloader-source*
  (format nil "~A~%~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "arch-raspi3b.lisp" *net-dir*))
          (read-file-text (merge-pathnames "dwc2-device.lisp" *net-dir*))
          (read-file-text (merge-pathnames "bcm2835-periph.lisp" *net-dir*))
          (read-file-text (merge-pathnames "uart-bootloader.lisp" *net-dir*))))

;; SSH networking sources (same as pizero2w-ssh build)
(defvar *net-source*
  (format nil "~A~%~A~%~A~%"
          (read-file-text (merge-pathnames "ip.lisp" *net-dir*))
          (read-file-text (merge-pathnames "crypto.lisp" *net-dir*))
          (read-file-text (merge-pathnames "ssh.lisp" *net-dir*))
          (read-file-text (merge-pathnames "aarch64-overrides.lisp" *net-dir*))))

;;; ============================================================
;;; Build bootloader + SSH kernel
;;; ============================================================

(in-package :modus.mvm)

;; Install the AArch64 translator
(install-aarch64-translator)

;; Use mini UART (0x3F215040)
(setf *aarch64-serial-base* #x3F215040)
(setf *aarch64-serial-width* 2)
(setf *aarch64-serial-tx-poll* '(#x14 5 :tbz))

(let* ((boot-main (format nil "~{~A~%~}"
                        (list
                         "(defun kernel-main ()"
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
                         ;; Print "BOOT\n"
                         "  (write-byte 66) (write-byte 79) (write-byte 79)"
                         "  (write-byte 84) (write-byte 10)"
                         ;; Blink LED once (bootloader alive)
                         "  (led-init)"
                         "  (led-blink 1 200)"
                         ;; Print "RDY\n" — deploy script waits for this before sending magic
                         "  (write-byte 82) (write-byte 68) (write-byte 89)"
                         "  (write-byte 10)"
                         ;; Wait for UART kernel upload
                         "  (bootloader-wait)"
                         ;; If we get here, no upload — fall through to SSH
                         ;; Print "SSH\n"
                         "  (write-byte 83) (write-byte 83) (write-byte 72)"
                         "  (write-byte 10)"
                         ;; Initialize USB gadget + CDC-ECM Ethernet
                         "  (cdc-ether-init)"
                         "  (write-byte 91) (write-byte 49) (write-byte 93)"
                         "  (sha256-init)"
                         "  (write-byte 91) (write-byte 50) (write-byte 93)"
                         "  (sha512-init)"
                         "  (write-byte 91) (write-byte 51) (write-byte 93)"
                         "  (ed25519-init)"
                         "  (write-byte 91) (write-byte 52) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (write-byte 91) (write-byte 53) (write-byte 93)"
                         "  (ssh-seed-random)"
                         "  (ssh-init-strings)"
                         ;; Embed pre-computed Ed25519 host key
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
                         "  (write-byte 83) (write-byte 83) (write-byte 72)"
                         "  (write-byte 58) (print-dec 22) (write-byte 10)"
                         "  (setf (mem-ref (+ (ssh-ipc-base) #x60438) :u32) 22)"
                         "  (let ((i 0))"
                         "    (loop"
                         "      (when (>= i 4) (return 0))"
                         "      (setf (mem-ref (conn-base i) :u32) 0)"
                         "      (setq i (+ i 1))))"
                         "  (net-actor-main))")))
       ;; ORDER IS LOad-BEARING.  mvm/repl-source.lisp ALSO defines
       ;; `kernel-main', and CLAUDE.md limitation #1 is last-defun-wins — so
       ;; with boot-main FIRST (as it was) the REPL's kernel-main shadowed the
       ;; chain loader's and the whole boot-main body became dead code,
       ;; including the GPIO14/15 ALT5 setup and the magic-byte wait.  The
       ;; resulting image booted, printed a "> " prompt and echoed input: a
       ;; REPL wearing a bootloader's name.  On real hardware it was worse than
       ;; useless — GPIO14/15 never switched to ALT5, so the mini UART was
       ;; never connected to the header pins and the board looked dead.
       ;; boot-main MUST come last.
       (combined-source (concatenate 'string
                                      cl-user::*bootloader-source*
                                      cl-user::*net-source*
                                      *repl-source*
                                      boot-main)))
  (format t "Building UART bootloader + SSH image...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :rpi :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (ensure-directories-exist "/tmp/piboot/kernel8.img")
    (write-kernel-image image "/tmp/piboot/kernel8.img")
    (format t "Done. Flash SD card once:~%")
    (format t "  bash scripts/make-sdcard.sh~%")
    (format t "  sudo dd if=/tmp/pizero2w-sdcard.img of=/dev/sdX bs=4M~%")
    (format t "~%Then deploy new kernels:~%")
    (format t "  python3 scripts/deploy-kernel.py /path/to/kernel8.img~%")))
