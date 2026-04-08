;;;; build-linux-hello.lisp - Build a Linux x86-64 hello world executable
;;;;
;;;; Usage: sbcl --script mvm/build-linux-hello.lisp
;;;;
;;;; Produces /tmp/modus-hello — run with: /tmp/modus-hello

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))

;; Install x64 translator in Linux mode
(modus.mvm.x64:install-x64-translator)
(setf modus.mvm.x64::*x64-linux-mode* t)

;; Load the Linux x64 boot descriptor
(load (merge-pathnames "../boot/boot-linux-x64.lisp"
                       (directory-namestring (truename *load-truename*))))

(in-package :modus.mvm)

;; Source: print "Hello from Modus!\n" and exit
(let* ((source "
(defun kernel-main ()
  (write-char-serial 72)
  (write-char-serial 101)
  (write-char-serial 108)
  (write-char-serial 108)
  (write-char-serial 111)
  (write-char-serial 32)
  (write-char-serial 102)
  (write-char-serial 114)
  (write-char-serial 111)
  (write-char-serial 109)
  (write-char-serial 32)
  (write-char-serial 77)
  (write-char-serial 111)
  (write-char-serial 100)
  (write-char-serial 117)
  (write-char-serial 115)
  (write-char-serial 33)
  (write-char-serial 10)
  (sys-exit 0))
")
       (image (build-image :target :linux-x64
                           :source-text source)))
  (format t "Entry point offset: ~A~%" (kernel-image-entry-point image))
  (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
  (format t "Boot code size: ~D~%" (length (kernel-image-boot-code image)))
  ;; Write ELF binary
  (let ((path "/tmp/modus-hello"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    ;; Make executable
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) path)
    (format t "Run with: ~A~%" path)))
