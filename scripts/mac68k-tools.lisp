;;;; mac68k-tools.lisp -- what the Mac disk scripts share: fs-apple, files as
;;;; byte vectors, running a program, and assembling a .s file for its bytes and
;;;; symbols.  Loaded by scripts/mac68k-hd.lisp, mac68k-disk.lisp and the others
;;;; that replaced the Python ones; SBCL (the build host), for SB-EXT:RUN-PROGRAM.

(defpackage #:mac68k-tools
  (:use #:cl)
  (:export #:*scripts* #:run #:read-bytes #:write-bytes #:pad-to #:assemble
           #:env #:env-hex #:argv))
(in-package #:mac68k-tools)

(defparameter *scripts* (make-pathname :name nil :type nil :defaults *load-truename*)
  "This directory: scripts/.")

(defun env (name &optional default)
  (let ((v (sb-ext:posix-getenv name)))
    (if (and v (plusp (length v))) v default)))

(defun env-hex (name &optional (default 0))
  (let ((v (env name))) (if v (parse-integer v :radix 16) default)))

(defun argv () (rest sb-ext:*posix-argv*))

(defun fs-apple-dir ()
  "fs-apple, as vessel is found: $FS_APPLE, else .fs-apple/ in the tree, else
   a sibling checkout ../fs-apple."
  (let ((e (env "FS_APPLE")))
    (if e
        (pathname (if (char= (char e (1- (length e))) #\/) e (concatenate 'string e "/")))
        (or (probe-file (merge-pathnames "../.fs-apple/" *scripts*))
            (probe-file (merge-pathnames "../../fs-apple/" *scripts*))
            (error "no fs-apple (set FS_APPLE, or check it out beside modus)")))))

(unless (find-package "FS-APPLE")
  (dolist (f '("package" "apm" "packbits"))
    (load (merge-pathnames (format nil "lib/~A.lisp" f) (fs-apple-dir)))))

(defun run (program &rest args)
  "PROGRAM's standard output, or an error with its standard error."
  (let* ((err (make-string-output-stream))
         (out (with-output-to-string (s)
                (let ((p (sb-ext:run-program program args :search t :output s :error err)))
                  (unless (zerop (sb-ext:process-exit-code p))
                    (error "~A: ~A" program (get-output-stream-string err)))))))
    out))

(defun read-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

(defun write-bytes (path v)
  (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence v o))
  path)

(defun pad-to (v n &optional (at-least 0))
  "V, zero-padded to a multiple of N bytes and to at least AT-LEAST."
  (let ((len (max at-least (* n (ceiling (length v) n)))))
    (if (= len (length v))
        v
        (let ((w (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace w v)
          w))))

(defun assemble (source)
  "SOURCE (a file in scripts/) assembled for the 68000: (values its bytes, a
   hash of its symbols' offsets)."
  (let* ((dir (format nil "/tmp/mac68k-as-~D/" (random 1000000000 (make-random-state t))))
         (o (concatenate 'string dir "s.o")) (b (concatenate 'string dir "s.bin")))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (run "m68k-linux-gnu-as" "-m68000" "-o" o (namestring (merge-pathnames source *scripts*)))
           (run "m68k-linux-gnu-objcopy" "-O" "binary" o b)
           (let ((syms (make-hash-table :test 'equal)))
             (with-input-from-string (s (run "m68k-linux-gnu-nm" o))
               (loop for line = (read-line s nil) while line
                     do (let* ((sp1 (position #\Space line))
                               (sp2 (position #\Space line :start (1+ sp1))))
                          (setf (gethash (subseq line (1+ sp2)) syms)
                                (parse-integer line :end sp1 :radix 16)))))
             (values (read-bytes b) syms)))
      (dolist (f (list o b)) (when (probe-file f) (delete-file f)))
      (ignore-errors (sb-ext:delete-directory dir)))))
