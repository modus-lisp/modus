;;;; build-riscv32-linux.lisp — hosted Linux/RV32 image.
;;;;
;;;; Usage: sbcl --script mvm/build-riscv32-linux.lisp [source.lisp]
;;;;   Output: /tmp/modus-riscv32-linux (MODUS_RISCV32_LINUX_OUT overrides)
;;;;   Run:    qemu-riscv32-static /tmp/modus-riscv32-linux
;;;;
;;;; The RV32 sibling of build-riscv-linux.lisp.  Same translator, same boot
;;;; shape; the width is chosen by INSTALL-RISCV32-TRANSLATOR, which clears
;;;; *RISCV-64-BIT*, and everything width-dependent in translate-riscv.lisp is
;;;; derived from that one flag.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
;; boot-linux-i386.lisp is where the generic ELF32-LE wrapper lives (it is not
;; i386-specific any more — only e_machine is), and boot-linux-riscv32.lisp
;; delegates to it rather than keeping a third copy of the program-header field
;; order.  Same arrangement the RV64 build has with the AArch64 wrapper.
(mvm-load "boot/boot-linux-i386.lisp")
(mvm-load "boot/boot-linux-riscv32.lisp")
(require :sb-posix)
(in-package :modus.mvm)

(install-riscv32-translator)
(riscv-set-linux-mode t)

(let* ((args (rest sb-ext:*posix-argv*))
       (src (first args))
       (out (or (sb-ext:posix-getenv "MODUS_RISCV32_LINUX_OUT")
                "/tmp/modus-riscv32-linux"))
       (text (if src
                 (with-open-file (s src)
                   (let ((b (make-string (file-length s))))
                     (subseq b 0 (read-sequence b s))))
                 ;; Default payload: prove the hosted path end to end — bytes
                 ;; out through the serial trap (write(2) here) and then
                 ;; SYS-EXIT, because falling off the end of kernel-main runs
                 ;; into whatever follows it.  Hosted images exit, not return.
                 "(defun kernel-main () (write-char-serial 79) (write-char-serial 75) (write-char-serial 10) (sys-exit 0))")))
  (format t "~&Building hosted Linux/RV32 image...~%")
  (let ((image (build-image :target :linux-riscv32 :source-text text)))
    (format t "entry=~A native=~D boot=~D~%"
            (kernel-image-entry-point image)
            (length (kernel-image-native-code image))
            (length (kernel-image-boot-code image)))
    (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) o))
    ;; MAKE IT EXECUTABLE.  qemu-user resolves its argument as a PROGRAM, and a
    ;; non-executable file is rejected SILENTLY -- exit 1, nothing on stderr,
    ;; indistinguishable from "file not found".  A hosted image that cannot be
    ;; run looks exactly like a hosted image that is broken, which is how this
    ;; cost a debugging cycle on the RV32 bring-up.
    ;; FIND-SYMBOL, not SB-POSIX:CHMOD.  check-source-parses READS every
    ;; first-party file in the tree, and at read time the sb-posix contrib has
    ;; not been required, so the qualified name is an unreadable symbol and the
    ;; whole build fails the parse sweep -- on files that have nothing to do
    ;; with the target being built.
    (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) out)
    (format t "Run: qemu-riscv32-static ~A~%" out)))
