;;;; build-arm32-linux.lisp — hosted Linux/ARM32 image.
;;;;
;;;; Usage: sbcl --script mvm/build-arm32-linux.lisp [source.lisp]
;;;;   Output: /tmp/modus-arm32-linux (MODUS_ARM32_LINUX_OUT overrides)
;;;;   Run:    qemu-arm-static /tmp/modus-arm32-linux
;;;;
;;;; Counterpart of build-x64-linux.lisp / build-aarch64-linux.lisp, minus the
;;;; ANSI corpus: this builds a CLEAN image from whichever source file is named
;;;; (defaulting to a self-check), which is what the arch ladder wants.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
;; The generic ELF64-LE wrapper lives in boot-linux-aarch64.lisp (that is where
;; it was written; it is not AArch64-specific any more — see its docstring), and
;; boot-linux-arm32.lisp delegates to it rather than copying 125 lines.  Loading
;; it here is the price of that; a better home would be a shared elf.lisp.
;; The generic ELF32-LE wrapper lives in boot-linux-i386.lisp (see its
;; docstring: not i386-specific any more), and boot-linux-arm32.lisp delegates.
(mvm-load "boot/boot-linux-i386.lisp")
(mvm-load "boot/boot-linux-arm32.lisp")
(require :sb-posix)
(in-package :modus.mvm)

(install-armv7-translator)
(arm32-set-linux-mode t)

(let* ((args (rest sb-ext:*posix-argv*))
       (src (first args))
       (out (or (sb-ext:posix-getenv "MODUS_ARM32_LINUX_OUT")
                "/tmp/modus-arm32-linux"))
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
  (format t "~&Building hosted Linux/ARM32 image...~%")
  (let ((image (build-image :target :linux-arm32 :source-text text)))
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
    (format t "Run: qemu-arm-static ~A~%" out)))
