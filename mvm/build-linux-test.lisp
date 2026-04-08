;;;; build-linux-test.lisp - Debug test for Linux argv passing
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(modus.mvm.x64:install-x64-translator)
(setf modus.mvm.x64::*x64-linux-mode* t)
(load (merge-pathnames "../boot/boot-linux-x64.lisp"
                       (directory-namestring (truename *load-truename*))))
(in-package :modus.mvm)

(let* ((source "
(defun print-hex-byte (b)
  (let ((hi (ash b -4)))
    (let ((lo (logand b 15)))
      (write-char-serial (if (< hi 10) (+ 48 hi) (+ 55 hi)))
      (write-char-serial (if (< lo 10) (+ 48 lo) (+ 55 lo))))))

(defun kernel-main ()
  ;; Boot stub stores at heap base 0x10000000.
  ;; Tagged addr for 0x10000000 = 0x20000000.
  ;; Tagged addr for 0x10000018 = 0x20000030.

  ;; Test 1: argc via :u32
  (write-char-serial 65)  ;; 'A'
  (let ((argc (mem-ref #x10000000 :u32)))
    (write-char-serial (+ 48 argc))
    (write-char-serial 10))

  ;; Test 2: dump 8 bytes at raw 0x10000000 (argc area)
  (write-char-serial 66)  ;; 'B'
  (write-char-serial 58)
  (let ((i 0))
    (loop
      (when (>= i 8) (return 0))
      (print-hex-byte (mem-ref (+ #x10000000 (* i 2)) :u8))
      (setq i (+ i 1))))
  (write-char-serial 10)

  ;; Test 3: dump 8 bytes at raw 0x10000018 (argv[1] area)
  (write-char-serial 67)  ;; 'C'
  (write-char-serial 58)
  (let ((i 0))
    (loop
      (when (>= i 8) (return 0))
      (print-hex-byte (mem-ref (+ #x10000018 (* i 2)) :u8))
      (setq i (+ i 1))))
  (write-char-serial 10)

  (sys-exit 0))
")
       (image (build-image :target :linux-x64 :source-text source)))
  (let ((path "/tmp/modus-test"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) path)))
