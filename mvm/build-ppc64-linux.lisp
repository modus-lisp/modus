;;;; build-ppc64-linux.lisp — hosted Linux/PPC64 image (BIG-ENDIAN).
;;;;
;;;; Usage: sbcl --script mvm/build-ppc64-linux.lisp [source.lisp]
;;;;   Output: /tmp/modus-ppc64-linux (MODUS_PPC64_LINUX_OUT overrides)
;;;;   Run:    qemu-ppc64-static /tmp/modus-ppc64-linux
;;;;
;;;; ORDER MATTERS: install the translator FIRST, then set hosted mode.
;;;; PPC-SET-LINUX-MODE's OFF branch restores the bare slot base from
;;;; *PPC-64-BIT*, which the installer is what sets.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "boot/boot-linux-ppc.lisp")
(require :sb-posix)
(in-package :modus.mvm)

(install-ppc-translator)
(ppc-set-linux-mode t)

(let* ((args (rest sb-ext:*posix-argv*))
       (src (first args))
       (out (or (sb-ext:posix-getenv "MODUS_PPC64_LINUX_OUT")
                "/tmp/modus-ppc64-linux"))
       (text (if src
                 (with-open-file (s src)
                   (let ((b (make-string (file-length s))))
                     (subseq b 0 (read-sequence b s))))
                 ;; Default payload: prove the hosted path end to end — bytes out
                 ;; through the serial trap (write(2) here) and then SYS-EXIT,
                 ;; because falling off the end of kernel-main runs into whatever
                 ;; follows it.  Hosted images exit, they do not return.
                 "(defun kernel-main () (write-char-serial 79) (write-char-serial 75) (write-char-serial 10) (sys-exit 0))")))
  (format t "~&Building hosted Linux/PPC64 image...~%")
  (let ((image (build-image :target :linux-ppc64 :source-text text)))
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
