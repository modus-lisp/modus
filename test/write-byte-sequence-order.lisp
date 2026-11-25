;;;; test/write-byte-sequence-order.lisp -- bytes written one at a time and as sequences, mixed on one file stream,
;;;; reach the file in the order they were written.
;;;;
;;;;   ./modus --script test/write-byte-sequence-order.lisp
;;;;
;;;; WRITE-BYTE goes through the stream's 4 KB write buffer; WRITE-SEQUENCE of a byte vector goes straight to the
;;;; descriptor (%FS-WRITE-BYTES).  Before the buffer was flushed ahead of the sequence, (write-byte 1)
;;;; (write-sequence #(2)) wrote 2 1: every single byte came out after every sequence, at the close.

(defvar *passed* 0)
(defvar *failed* 0)
(defun check (name ok)
  (if ok (incf *passed*) (incf *failed*))
  (format t "~&~:[FAIL~;PASS~] ~A~%" ok name))

(defvar *path* (format nil "~A/modus-wbso-~D.bin"
                       (or (modus-int::%cli-getenv "TMPDIR") (modus-int::%cli-getenv "TEMP") "/tmp")
                       (get-universal-time)))

(defun octets (list) (make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list))

(defun written (fn)
  "What FN writes to a fresh (UNSIGNED-BYTE 8) file, read back as a list."
  (with-open-file (o *path* :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (funcall fn o))
  (with-open-file (i *path* :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8))))
      (read-sequence v i)
      (coerce v 'list))))

(check "bytes and sequences interleaved"
       (equal (written (lambda (o)
                         (write-byte 1 o) (write-byte 2 o)
                         (write-sequence (octets '(10 11 12)) o)
                         (write-byte 3 o)
                         (write-sequence (octets '(20 21)) o)
                         (write-byte 4 o)))
              '(1 2 10 11 12 3 20 21 4)))

(check "a sequence's slice (:start :end) after a byte"
       (equal (written (lambda (o) (write-byte 7 o) (write-sequence (octets '(0 1 2 3 4)) o :start 1 :end 3)))
              '(7 1 2)))

;; more single bytes than the buffer holds (it flushes itself at 4096), then a sequence, then more bytes
(let ((got (written (lambda (o)
                      (dotimes (i 5000) (write-byte (mod i 251) o))
                      (write-sequence (octets '(255 254)) o)
                      (dotimes (i 3) (write-byte (+ 100 i) o))))))
  (check "5000 bytes, a sequence, 3 bytes: the length" (= (length got) 5005))
  (check "5000 bytes, a sequence, 3 bytes: the order"
         (and (every #'= (subseq got 0 5000) (loop for i below 5000 collect (mod i 251)))
              (equal (subseq got 5000) '(255 254 100 101 102)))))

(ignore-errors (delete-file *path*))
(format t "~&~D passed, ~D failed~%" *passed* *failed*)
(sb-ext:exit :code (if (zerop *failed*) 0 1))
