;;;; build-x64-console-repl.lisp - Build x86-64 REPL with VGA + PS/2 keyboard
;;;;
;;;; Usage: sbcl --script mvm/build-x64-console-repl.lisp
;;;;
;;;; Produces /tmp/modus-x64-console.bin — a Multiboot kernel with:
;;;;   - VGA text mode output (80x25 at 0xB8000)
;;;;   - PS/2 keyboard input
;;;;   - Serial I/O (always active)
;;;;
;;;; Boot with QEMU:
;;;;   ./scripts/run.sh x64-console-repl
;;;;
;;;; Boot with GRUB on real hardware (ThinkPad T420 etc.):
;;;;   ./scripts/make-grub-usb.sh
;;;;   # Then boot from USB, select "Modus" in GRUB menu

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")

(defun read-file-text (path)
  "Read entire file as a string."
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (let ((n (read-sequence text s)))
        (subseq text 0 n)))))

(defvar *console-source*
  (read-file-text (merge-pathnames "net/uefi-console.lisp" *modus-base*)))

(in-package :modus.mvm)

(modus.mvm.x64:install-x64-translator)

;; Combine: REPL source first, then console overrides.
;; Last-defun-wins: console's write-char-output and read-char-input
;; override the REPL's serial-only defaults.
(let ((combined-source (concatenate 'string
                                    *repl-source*
                                    cl-user::*console-source*)))
  (format t "Building x86-64 console REPL image...~%")
  (format t "Combined source: ~D chars~%" (length combined-source))
  (let ((image (build-image :target :x64-console :source-text combined-source)))
    (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
    (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
    (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
    (format t "Image size: ~D~%" (length (kernel-image-image-bytes image)))
    (write-kernel-image image "/tmp/modus-x64-console.bin")
    (format t "Done. Boot with:~%")
    (format t "  ./scripts/run.sh x64-console-repl~%")))
