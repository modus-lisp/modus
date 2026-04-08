;;;; build-linux-cat.lisp - Build a Linux cat utility via MVM with SAP FFI
;;;;
;;;; Usage: sbcl --script mvm/build-linux-cat.lisp
;;;; Run:   /tmp/modus-cat /etc/hostname

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(modus.mvm.x64:install-x64-translator)
(setf modus.mvm.x64::*x64-linux-mode* t)
(load (merge-pathnames "../boot/boot-linux-x64.lisp"
                       (directory-namestring (truename *load-truename*))))
(in-package :modus.mvm)

(let* ((source "
;; Globals at heap base 0x10000000 (stored by boot stub).
;; Source literals = raw byte addresses (compiler tags internally).

(defun sys-argc () (mem-ref #x10000000 :u32))

;; argv[1] as SAP (raw C string pointer from :u64 load)
(defun argv1-sap ()
  (make-sap-raw (mem-ref #x10000018 :u64)))

;; I/O buffer SAP near end of heap (well away from globals and alloc area)
(defun io-buf-sap ()
  (make-sap #x1DF00000))

;; All syscall wrappers use syscall3 (tagged args, tagged result).
;; sap-address returns tagged fixnum.

(defun sys-open (path-sap)
  (let ((p path-sap))
    (syscall3 2 (sap-address p) 0 0)))

(defun sys-read (fd buf-sap len)
  (let ((f fd))
    (let ((b buf-sap))
      (let ((l len))
        (syscall3 0 f (sap-address b) l)))))

(defun sys-write-buf (fd buf-sap len)
  (let ((f fd))
    (let ((b buf-sap))
      (let ((l len))
        (syscall3 1 f (sap-address b) l)))))

(defun sys-close (fd)
  (let ((f fd))
    (syscall3 3 f 0 0)))

(defun cat-fd (fd buf-sap)
  (let ((f fd))
    (let ((b buf-sap))
      (loop
        (let ((n (sys-read f b 4096)))
          (when (<= n 0)
            (sys-close f)
            (return 0))
          (sys-write-buf 1 b n))))))

(defun kernel-main ()
  (let ((argc (sys-argc)))
    (if (< argc 2)
        (progn
          (write-char-serial 85) (write-char-serial 115)
          (write-char-serial 97) (write-char-serial 103)
          (write-char-serial 101) (write-char-serial 10)
          (sys-exit 1))
        (let ((path (argv1-sap)))
          (let ((fd (sys-open path)))
            (if (< fd 0)
                (progn
                  (write-char-serial 69) (write-char-serial 114)
                  (write-char-serial 114) (write-char-serial 10)
                  (sys-exit 1))
                (let ((buf (io-buf-sap)))
                  (cat-fd fd buf)
                  (sys-exit 0))))))))
")
       (image (build-image :target :linux-x64 :source-text source)))
  (format t "Native code size: ~D~%" (length (kernel-image-native-code image)))
  (let ((path "/tmp/modus-cat"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) path)))
