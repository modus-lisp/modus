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
;; Boot stub stores at 0x600000 (raw u64):
;; +0x00: argc, +0x10: argv[0], +0x18: argv[1], +0x20: argv[2]

;; argc as tagged fixnum
(defun sys-argc () (mem-ref #x600000 :u32))

;; argv[1] as SAP (wraps the raw C string pointer)
(defun argv1-sap ()
  (make-sap (mem-ref #xC00030 :u64)))

;; Linux syscall wrappers using SAPs
;; SYS_open(path-sap, flags, mode) → fd (tagged)
(defun sys-open (path-sap flags mode)
  (let ((p path-sap))
    (let ((f flags))
      (let ((m mode))
        (let ((raw-path (sap-address p)))
          (let ((result (syscall3-raw 2 raw-path f m)))
            (let ((r result))
              (+ r r))))))))

;; SYS_read(fd, buf-sap, len) → bytes-read (tagged)
(defun sys-read (fd buf-sap len)
  (let ((f fd))
    (let ((b buf-sap))
      (let ((l len))
        (let ((raw-buf (sap-address b)))
          (let ((result (syscall3-raw 0 (ash f -1) raw-buf (ash l -1))))
            (let ((r result))
              (+ r r))))))))

;; SYS_write(fd, buf-sap, len) → bytes-written (tagged)
(defun sys-write (fd buf-sap len)
  (let ((f fd))
    (let ((b buf-sap))
      (let ((l len))
        (let ((raw-buf (sap-address b)))
          (let ((result (syscall3-raw 1 (ash f -1) raw-buf (ash l -1))))
            (let ((r result))
              (+ r r))))))))

;; SYS_close(fd)
(defun sys-close (fd)
  (let ((f fd))
    (syscall3-raw 3 (ash f -1) 0 0)))

;; Allocate a SAP-backed buffer via mmap
(defun alloc-buf (size)
  (let ((s size))
    ;; SYS_mmap(NULL, size, PROT_RW=3, MAP_PRIVATE|MAP_ANON=0x22, -1, 0)
    ;; This needs 6 args but we only have syscall3... use the heap instead
    ;; For now, allocate a Lisp array and get its raw data pointer as SAP
    (let ((arr (make-array s)))
      (let ((raw (+ (ash (logand arr (- 0 4)) 1) 8)))
        (make-sap raw)))))

(defun cat-fd (fd buf-sap)
  (let ((f fd))
    (let ((b buf-sap))
      (loop
        (let ((n (sys-read f b 4096)))
          (when (<= n 0)
            (sys-close f)
            (return 0))
          (sys-write 2 b n))))))

(defun kernel-main ()
  (let ((argc (sys-argc)))
    (if (< argc 2)
        (progn
          (write-char-serial 85) (write-char-serial 115)
          (write-char-serial 97) (write-char-serial 103)
          (write-char-serial 101) (write-char-serial 10)
          (sys-exit 1))
        (let ((path (argv1-sap)))
          (let ((fd (sys-open path 0 0)))
            (if (< fd 0)
                (progn
                  (write-char-serial 69) (write-char-serial 114)
                  (write-char-serial 114) (write-char-serial 10)
                  (sys-exit 1))
                (let ((buf (alloc-buf 4096)))
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
