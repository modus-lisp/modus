;;;; ndif.lisp DATA APPLEDOUBLE OUT -- a Disk Copy 6 NDIF image (its data fork,
;;;; and its resource fork in an AppleDouble file) as the raw disk.
;;;;
;;;; The formats are fs-apple's (APPLEDOUBLE-READ, NDIF-READ; NDIF's ADC is cram's),
;;;; found in the workspace (WORKSPACE, default the directory above this
;;;; checkout).  It replaced ndif.py (2026-11-21), byte for byte on Allegro CL
;;;; 1.2.2's two floppies.
(require :asdf)
(defvar *workspace*
  (let ((w (sb-ext:posix-getenv "WORKSPACE")))
    (if (and w (plusp (length w)))
        (pathname (if (char= (char w (1- (length w))) #\/) w (concatenate 'string w "/")))
        (merge-pathnames "../../../" (make-pathname :name nil :type nil :defaults *load-truename*)))))
(push (lambda (name)
        (let ((base (subseq name 0 (or (position #\/ name) (length name)))))
          (probe-file (merge-pathnames (format nil "~A/~A.asd" base base) *workspace*))))
      asdf:*system-definition-search-functions*)
(let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
  (asdf:load-system "fs-apple"))

(defun octets (path)
  (with-open-file (i (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence v i) v)))

(destructuring-bind (data ad out) (rest sb-ext:*posix-argv*)
  (let ((disk (fs-apple:ndif-read (octets data) (getf (fs-apple:appledouble-read (octets ad)) :rsrc))))
    (with-open-file (o (sb-ext:parse-native-namestring out) :direction :output
                                                            :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence disk o))
    (format t "~A: ~D sectors, ~A~%" out (floor (length disk) 512) (map 'string #'code-char (subseq disk 1024 1026)))))
