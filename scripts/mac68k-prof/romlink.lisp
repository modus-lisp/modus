;;;; romlink.lisp MAMECMD SET... -- give each SET's ROMs their MAME names, matched
;;;; by CRC-32 (cram's) against the files under ROMSRC (default /tmp/rss/romsrc),
;;;; copied into MAMEHOME/roms/SET/ (default /tmp/rss/mame).  MAMECMD is the
;;;; command that runs MAME, its words space-separated.  It replaced romlink.py
;;;; (2026-11-21).
(load (merge-pathnames "prof-common.lisp" *load-truename*))
(in-package #:mac68k-prof)
(need "cram")

(defun file-bytes (p)
  (with-open-file (i p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence v i) v)))

(defun rom-line (line)
  "A -listroms line's (NAME . crc), as `NAME SIZE CRC(xxxxxxxx) ...', else NIL."
  (let ((w (words line)))
    (when (and (>= (length w) 3) (every #'digit-char-p (second w))
               (> (length (third w)) 5) (string= (subseq (third w) 0 4) "CRC("))
      (cons (first w) (string-downcase (subseq (third w) 4 (position #\) (third w))))))))

(defun driver-line (line)
  "A -listroms line's driver, as `ROMs required for driver \"NAME\".', else NIL."
  (let ((head "ROMs required for driver \""))
    (when (and (> (length line) (length head)) (string= head line :end2 (length head)))
      (subseq line (length head) (position #\" line :start (length head))))))

(let* ((args (argv)) (mame (words (first args))) (sets (rest args))
       (romsrc (env "ROMSRC" "/tmp/rss/romsrc")) (home (env "MAMEHOME" "/tmp/rss/mame"))
       (crc (make-hash-table :test 'equal)))
  (dolist (p (sort (mapcar #'namestring (directory (format nil "~A/**/*.*" romsrc))) #'string<))
    (when (and (pathname-name p)
               (<= (with-open-file (i p :element-type '(unsigned-byte 8)) (file-length i)) (* 4 1024 1024)))
      (let ((k (format nil "~(~8,'0x~)" (funcall (find-symbol "CRC32" "CRAM") (file-bytes p)))))
        (unless (gethash k crc) (setf (gethash k crc) p)))))
  (let ((out (with-output-to-string (s)
               (sb-ext:run-program (first mame) (append (rest mame) (list "-listroms") sets)
                                   :search t :output s
                                   :environment (cons (format nil "HOME=~A" home) (sb-ext:posix-environ)))))
        (cur nil))
    (with-input-from-string (in out)
      (loop for line = (read-line in nil) while line
            do (let ((d (driver-line line)) (r (rom-line line)))
                 (cond (d (setq cur d) (ensure-directories-exist (format nil "~A/roms/~A/" home cur)))
                       ((and r cur)
                        (let ((src (gethash (cdr r) crc)))
                          (format t "~A: ~A <- ~A~%" cur (car r) (or src "None"))
                          (when src
                            (with-open-file (o (format nil "~A/roms/~A/~A" home cur (car r))
                                               :direction :output :element-type '(unsigned-byte 8)
                                               :if-exists :supersede)
                              (write-sequence (file-bytes src) o)))))))))))
