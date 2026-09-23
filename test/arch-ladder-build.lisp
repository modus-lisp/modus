;;;; arch-ladder-build.lisp — build ONE image, for ONE target, from ONE source
;;;; file.  Test infrastructure for scripts/arch-ladder-gate.sh.
;;;;
;;;;   sbcl --script test/arch-ladder-build.lisp <target> <out.bin> <source.lisp>
;;;;
;;;; TARGET is a build-image target keyword: x86-64 aarch64 i386 armv7-rpi
;;;; riscv64 ppc64 ppc32 68k.
;;;;
;;;; This deliberately does NOT go through mvm/build.lisp.  That file is the
;;;; single entry point for the SHIPPING images and its matrix is a table of
;;;; named cells; this builds an arbitrary source file for an arbitrary target,
;;;; which is a different job and should not dilute that table.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(in-package :modus.mvm)

(defun ladder-install-translator (arch)
  "Install ARCH's translator.  The i386 and x64 translators live in their own
   packages, so interning their installer name in :modus.mvm yields an unbound
   symbol and a FUNCALL error — hence the explicit package per case."
  (case arch
    (:x86-64    (funcall (intern "INSTALL-X64-TRANSLATOR" :modus.mvm.x64)))
    (:aarch64   (install-aarch64-translator))
    (:i386      (funcall (intern "INSTALL-I386-TRANSLATOR" :modus.mvm.i386)))
    (:riscv64   (install-riscv-translator))
    (:ppc64     (install-ppc-translator))
    (:ppc32     (install-ppc32-translator))
    (:68k       (install-68k-translator))
    (:arm32     (install-arm32-translator))
    (:armv7     (install-armv7-translator))
    (:armv7-rpi (install-armv7-rpi-translator))
    (t (error "arch-ladder-build: no translator installer for ~A" arch))))

(let* ((args (rest sb-ext:*posix-argv*))
       (arch (intern (string-upcase (first args)) :keyword))
       (out  (second args))
       (src  (third args)))
  (unless (and arch out src)
    (format *error-output*
            "usage: arch-ladder-build.lisp <target> <out.bin> <source.lisp>~%")
    (sb-ext:exit :code 2))
  (let ((text (with-open-file (s src)
                (let ((b (make-string (file-length s))))
                  (subseq b 0 (read-sequence b s))))))
    (ladder-install-translator arch)
    (let ((image (build-image :target arch :source-text text)))
      (format t "entry=~A native=~D boot=~D~%"
              (kernel-image-entry-point image)
              (length (kernel-image-native-code image))
              (length (kernel-image-boot-code image)))
      (write-kernel-image image out))))
