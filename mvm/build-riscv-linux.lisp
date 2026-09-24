;;;; build-riscv-linux.lisp — hosted Linux/RV64 image.
;;;;
;;;; Usage: sbcl --script mvm/build-riscv-linux.lisp [source.lisp]
;;;;   Output: /tmp/modus-riscv-linux (MODUS_RISCV_LINUX_OUT overrides)
;;;;   Run:    qemu-riscv64-static /tmp/modus-riscv-linux
;;;;
;;;; Counterpart of build-x64-linux.lisp / build-aarch64-linux.lisp, minus the
;;;; ANSI corpus: this builds a CLEAN image from whichever source file is named
;;;; (defaulting to a self-check), which is what the arch ladder wants.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
;; The generic ELF64-LE wrapper lives in boot-linux-aarch64.lisp (that is where
;; it was written; it is not AArch64-specific any more — see its docstring), and
;; boot-linux-riscv.lisp delegates to it rather than copying 125 lines.  Loading
;; it here is the price of that; a better home would be a shared elf.lisp.
(mvm-load "boot/boot-linux-aarch64.lisp")
(mvm-load "boot/boot-linux-riscv.lisp")
(in-package :modus.mvm)

(install-riscv-translator)
(riscv-set-linux-mode t)

(let* ((args (rest sb-ext:*posix-argv*))
       (src (first args))
       (out (or (sb-ext:posix-getenv "MODUS_RISCV_LINUX_OUT")
                "/tmp/modus-riscv-linux"))
       (text (if src
                 (with-open-file (s src)
                   (let ((b (make-string (file-length s))))
                     (subseq b 0 (read-sequence b s))))
                 ;; Default payload: prove the hosted path end to end — write a
                 ;; byte through the serial trap (now write(2)) and exit(0).
                 ;; Default payload: prove the hosted path end to end — bytes
                 ;; out through the serial trap (now write(2)) and then SYS-EXIT,
                 ;; because falling off the end of kernel-main runs into whatever
                 ;; follows it and segfaults.  Hosted images must exit, not
                 ;; return.
                 "(defun kernel-main () (write-char-serial 79) (write-char-serial 75) (write-char-serial 10) (sys-exit 0))")))
  (format t "~&Building hosted Linux/RV64 image...~%")
  (let ((image (build-image :target :linux-riscv :source-text text)))
    (format t "entry=~A native=~D boot=~D~%"
            (kernel-image-entry-point image)
            (length (kernel-image-native-code image))
            (length (kernel-image-boot-code image)))
    (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) o))
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) out)
    (format t "Run: qemu-riscv64-static ~A~%" out)))
