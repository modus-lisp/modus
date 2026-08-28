;;;; build-aarch64-fbprobe.lisp — list the fw_cfg file directory (find etc/ramfb).
;;;; sbcl --script mvm/build-aarch64-fbprobe.lisp

(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(mvm-load "mvm/repl-source.lisp")
(defun read-file-text (path)
  (with-open-file (s path :direction :input)
    (let ((text (make-string (file-length s)))) (subseq text 0 (read-sequence text s)))))
(defvar *net-dir* (merge-pathnames "net/" *modus-base*))
(defvar *arch-source*  (read-file-text (merge-pathnames "arch-aarch64.lisp" *net-dir*)))
(defvar *ramfb-source* (read-file-text (merge-pathnames "ramfb.lisp" *net-dir*)))

(in-package :modus.mvm)
(install-aarch64-translator)

(let* ((main "(defun kernel-main () (write-byte 61) (write-byte 61) (write-byte 10) (fwcfg-list) (write-byte 46) (write-byte 46) (write-byte 10) (hlt))")
       (combined (concatenate 'string cl-user::*arch-source* cl-user::*ramfb-source* *repl-source* main)))
  (format t "Building fb probe (~D chars)...~%" (length combined))
  (let ((image (build-image :target :aarch64 :source-text combined)))
    (write-kernel-image image "/tmp/modus-aarch64-fbprobe.bin")
    (format t "Done -> /tmp/modus-aarch64-fbprobe.bin~%")))
