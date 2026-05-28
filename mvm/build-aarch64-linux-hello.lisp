;;;; Larger Linux/AArch64 toolchain probe — verify branches+if/cond work.

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "boot/boot-linux-aarch64.lisp")

(in-package :modus.mvm)

(install-aarch64-translator)

(setf *aarch64-stack-align-16* t)
(setf *aarch64-linux-mode* t)
(setf *aarch64-fn-align-offset* 120)

(defvar *hello-src* "
(defun count-to (n)
  (let ((i 0))
    (loop
      (when (>= i n) (return nil))
      (write-char-serial (+ 48 i))
      (setq i (+ i 1)))))

(defun kernel-main ()
  (count-to 5)
  (write-char-serial 10)
  (sys-exit 0))
")

(let ((image (build-image :target :linux-aarch64 :source-text *hello-src*)))
  (with-open-file (out "/tmp/modus-aa64-hello"
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (write-sequence (kernel-image-image-bytes image) out))
  (format t "~%Wrote ~D bytes to /tmp/modus-aa64-hello~%"
          (length (kernel-image-image-bytes image))))
