;;;; build-aarch64-fb-test.lisp — configure ramfb and draw, for screendump/VNC.
;;;; sbcl --script mvm/build-aarch64-fb-test.lisp

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

(let* ((test (format nil "~{~A~%~}"
   (list
    "(defun fb-test ()"
    "  (let ((sel (ramfb-find)))"
    "    (if (eq sel 0)"
    "        (progn (write-byte 78) (write-byte 79) (write-byte 10))"   ; "NO\n"
    "        (progn"
    "          (write-byte 83) (write-byte 58) (print-dec sel) (write-byte 10)"  ; "S:<sel>\n"
    "          (ramfb-bands 640 480 #x00FF0000 #x000000FF)"             ; DRAW FIRST (red top/blue bottom)
    "          (ramfb-config sel 640 480)"                              ; then configure ramfb
    "          (write-byte 68) (write-byte 82) (write-byte 87) (write-byte 10)))))"  ; "DRW\n"
    "(defun kernel-main ()"
    "  (write-byte 61) (write-byte 61) (write-byte 10)"
    "  (fb-test)"
    "  (write-byte 46) (write-byte 46) (write-byte 10)"
    "  (hlt))")))
       (combined (concatenate 'string cl-user::*arch-source* cl-user::*ramfb-source* *repl-source* test)))
  (format t "Building fb test (~D chars)...~%" (length combined))
  (let ((image (build-image :target :aarch64 :source-text combined)))
    (write-kernel-image image "/tmp/modus-aarch64-fb.bin")
    (format t "Done -> /tmp/modus-aarch64-fb.bin~%")))
