;;;; build-68k-linux.lisp — hosted Linux/68K image (BIG-ENDIAN).
;;;;
;;;; Usage: sbcl --script mvm/build-68k-linux.lisp [source.lisp]
;;;;   Output: /tmp/modus-68k-linux (MODUS_68K_LINUX_OUT overrides)
;;;;   Run:    qemu-m68k-static /tmp/modus-68k-linux
;;;;
;;;; ORDER MATTERS: install the translator FIRST, then set hosted mode, because
;;;; the installer is what sets the bare-metal slot base that hosted mode moves.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "boot/boot-linux-68k.lisp")
(require :sb-posix)
(in-package :modus.mvm)

(install-68k-translator)
(m68k-set-linux-mode t)

(let* ((args (rest sb-ext:*posix-argv*))
       (src (first args))
       (out (or (sb-ext:posix-getenv "MODUS_68K_LINUX_OUT")
                "/tmp/modus-68k-linux"))
       (text (if src
                 (with-open-file (s src)
                   (let ((b (make-string (file-length s))))
                     (subseq b 0 (read-sequence b s))))
                 ;; Default payload: prove the hosted path end to end — bytes out
                 ;; through the serial trap (write(2) here) and then SYS-EXIT,
                 ;; because falling off the end of kernel-main runs into whatever
                 ;; follows it.  Hosted images exit, they do not return.
                 "(defun kernel-main () (write-char-serial 79) (write-char-serial 75) (write-char-serial 10) (sys-exit 0))")))
  (format t "~&Building hosted Linux/68K image...~%")
  (let ((image (build-image :target :linux-68k :source-text text)))
    (format t "entry=~A native=~D boot=~D~%"
            (kernel-image-entry-point image)
            (length (kernel-image-native-code image))
            (length (kernel-image-boot-code image)))
    (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) o))
    ;; FIND-SYMBOL, not SB-POSIX:CHMOD — check-source-parses READS every
    ;; first-party file before a build, and that symbol does not exist until the
    ;; contrib is required, which a load-time (require) does too late.
    (funcall (find-symbol "CHMOD" "SB-POSIX") out #o755)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) out)))
