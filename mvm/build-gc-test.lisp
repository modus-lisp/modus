;;;; build-gc-test.lisp - Minimal test for Cheney GC
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(modus.mvm.x64:install-x64-translator)
(setf modus.mvm.x64::*x64-linux-mode* t)
(setf modus.mvm.x64::*x64-gc-enabled* t)
(load (merge-pathnames "../boot/boot-linux-x64.lisp"
                       (directory-namestring (truename *load-truename*))))
(setf modus.mvm::*linux-x64-r14-offset* modus.mvm::+linux-x64-gc-midpoint+)
(in-package :modus.mvm)

(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defvar *prelude-source*
  (read-file-text (merge-pathnames "mvm/prelude.lisp" cl-user::*modus-base*)))
(defvar *gc-source*
  (read-file-text (merge-pathnames "mvm/gc.lisp" cl-user::*modus-base*)))

(let* ((test-source "
(defun print-hex-byte (b)
  (let ((hi (ash b -4)))
    (let ((lo (logand b 15)))
      (write-char-serial (if (< hi 10) (+ 48 hi) (+ 55 hi)))
      (write-char-serial (if (< lo 10) (+ 48 lo) (+ 55 lo))))))

(defun print-hex64 (val)
  (let ((i 56))
    (loop
      (when (< i 0) (return nil))
      (print-hex-byte (logand (ash val (- i)) 255))
      (setq i (- i 8)))))

(defun print-nl () (write-byte 10))

(defun print-str (s)
  (let ((i 0) (len (array-length s)))
    (loop
      (when (>= i len) (return nil))
      (write-char-serial (aref s i))
      (setq i (+ i 1)))))

(defun kernel-main ()
  (print-str \"GC-TEST\")
  (print-nl)

  ;; Test 1: Simple cons survives GC
  (let ((x (cons 42 nil))
        (i 0))
    (loop (when (>= i 60000000) (return nil)) (cons i nil) (setq i (+ i 1)))
    (print-str \"T1 car=\")
    (print-hex64 (car x))
    (print-nl))

  ;; Test 2: Nested cons survives GC
  (let ((x (cons 42 (cons 99 nil)))
        (i 0))
    ;; Print before GC
    (print-str \"T2 before: car=\")
    (print-hex64 (car x))
    (print-str \" cdr-raw=\")
    (print-hex64 (cdr x))
    (print-nl)
    (loop (when (>= i 60000000) (return nil)) (cons i nil) (setq i (+ i 1)))
    ;; Print after GC
    (print-str \"T2 after: car=\")
    (print-hex64 (car x))
    (print-nl))

  ;; Test 3: Array survives GC
  (let ((arr (make-array 3))
        (i 0))
    (aset arr 0 100)
    (loop (when (>= i 60000000) (return nil)) (cons i nil) (setq i (+ i 1)))
    (print-str \"T3 arr[0]=\")
    (print-hex64 (aref arr 0))
    (print-nl))

  (print-str \"gc=\")
  (print-hex64 (mem-ref #x10000060 :u64))
  (print-nl)
  (print-str \"DONE\")
  (print-nl)
  (sys-exit 0))
")
       (source (concatenate 'string *prelude-source* (string #\Newline)
                             *gc-source* (string #\Newline)
                             test-source))
       (image (build-image :target :linux-x64 :source-text source)))
  (let ((path "/tmp/modus-gc-test"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) path)))
