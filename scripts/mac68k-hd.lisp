;;;; mac68k-hd.lisp -- a SCSI hard disk that boots a 68k Mac with no Mac OS.
;;;;
;;;;   sbcl --script scripts/mac68k-hd.lisp VOLUME OUT [--chd CHDMAN]
;;;;
;;;; VOLUME is a modus boot volume (mac68k-core.py, mac68k-disk.py or
;;;; mac68k-floppy.py output: boot blocks at byte 0).  OUT is a raw disk: block 0
;;;; the driver descriptor (one driver, Mac OS), a partition map in blocks 1-3,
;;;; the driver (scripts/mac68k-scsi.s, its BASE and VOLBLOCKS words patched in)
;;;; from block 4, VOLUME from block 64; with --chd, OUT.chd for MAME as well
;;;; (chdman createhd).  The ROM loads the driver and enters it; the driver adds
;;;; a drive that is VOLUME, and the ROM boots its boot blocks.
;;;;
;;;; The disk layout itself is fs-apple's (the Apple Partition Map: lib/apm.lisp),
;;;; found as $FS_APPLE, .fs-apple/ in the tree, or a sibling checkout
;;;; ../fs-apple.  It replaced mac68k-hd.py, byte for byte (2026-11-20).

(defpackage #:mac68k-hd (:use #:cl))
(in-package #:mac68k-hd)

(defparameter *here* (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *base* 64 "The volume's first block.")
(defparameter *mapn* 3 "Partition map entries: blocks 1-3.")
(defparameter *drv* (+ 1 *mapn*) "The driver's first block.")

(defun fs-apple-dir ()
  "fs-apple, as vessel is found: $FS_APPLE, else .fs-apple/ in the tree, else
   a sibling checkout ../fs-apple."
  (let ((e (sb-ext:posix-getenv "FS_APPLE")))
    (if (and e (plusp (length e)))
        (pathname (if (char= (char e (1- (length e))) #\/) e (concatenate 'string e "/")))
        (or (probe-file (merge-pathnames "../.fs-apple/" *here*))
            (probe-file (merge-pathnames "../../fs-apple/" *here*))
            (error "mac68k-hd: no fs-apple (set FS_APPLE, or check it out beside modus)")))))

(load (merge-pathnames "lib/apm.lisp" (fs-apple-dir)))

(defun run (program &rest args)
  "PROGRAM's standard output, or an error with its standard error."
  (let* ((err (make-string-output-stream))
         (out (with-output-to-string (s)
                (let ((p (sb-ext:run-program program args :search t :output s :error err)))
                  (unless (zerop (sb-ext:process-exit-code p))
                    (error "mac68k-hd: ~A: ~A" program (get-output-stream-string err)))))))
    out))

(defun read-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence v in)
      v)))

(defun driver ()
  "The assembled driver and its symbols (NAME -> offset)."
  (let* ((dir (format nil "/tmp/mac68k-hd-~D/" (random 1000000000 (make-random-state t))))
         (o (concatenate 'string dir "s.o")) (b (concatenate 'string dir "s.bin")))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (run "m68k-linux-gnu-as" "-m68000" "-o" o (namestring (merge-pathnames "mac68k-scsi.s" *here*)))
           (run "m68k-linux-gnu-objcopy" "-O" "binary" o b)
           (let ((syms (make-hash-table :test 'equal)))
             (with-input-from-string (s (run "m68k-linux-gnu-nm" o))
               (loop for line = (read-line s nil) while line
                     do (let* ((sp1 (position #\Space line)) (sp2 (position #\Space line :start (1+ sp1))))
                          (setf (gethash (subseq line (1+ sp2)) syms)
                                (parse-integer line :end sp1 :radix 16)))))
             (values (read-bytes b) syms)))
      (dolist (f (list o b)) (when (probe-file f) (delete-file f)))
      (ignore-errors (sb-ext:delete-directory dir)))))

(defun pad-to (v n)
  "V, zero-padded to a multiple of N bytes."
  (let ((len (* n (ceiling (length v) n))))
    (if (= len (length v))
        v
        (let ((w (make-array len :element-type '(unsigned-byte 8) :initial-element 0)))
          (replace w v)
          w))))

(defun main (volume out &optional chdman)
  (let* ((vol (pad-to (read-bytes volume) 512))
         (volblocks (floor (length vol) 512)))
    (multiple-value-bind (drv syms) (driver)
      (fs-apple:set-u32 drv (gethash "base" syms) *base*)
      (fs-apple:set-u32 drv (gethash "volblocks" syms) volblocks)
      (let* ((drv (pad-to drv 512))
             (nd (floor (length drv) 512))
             (total (let ((n (+ *base* volblocks))) (* 64 (ceiling n 64))))  ; whole cylinders (chdman)
             (disk (make-array (* total 512) :element-type '(unsigned-byte 8) :initial-element 0)))
        (when (> (+ *drv* nd) *base*)
          (error "mac68k-hd: the driver (~D blocks) runs into the volume" nd))
        (fs-apple:apm-write-ddm disk total (list (list *drv* nd 1)))
        ;; THE PARTITION MAP, as Apple's own disks have one.  Without a map a Plus,
        ;; a Classic, an SE/30 and a IIci boot from the driver descriptor alone
        ;; (MAME).  With one the Classic's ROM reads it, and boots only when the
        ;; volume's entry is Apple_HFS: an Apple_Free volume showed the
        ;; no-startup-disk icon.
        (fs-apple:apm-write-map
         disk (list (list :start 1 :count *mapn* :name "Apple" :type "Apple_partition_map")
                    (list :start *drv* :count nd :name "Macintosh" :type "Apple_Driver43"
                          :status #x37F :boot-size (length drv) :processor "68000"
                          :boot-checksum (fs-apple:apm-boot-checksum drv))
                    (list :start *base* :count (- total *base*) :name "Modus" :type "Apple_HFS")))
        (replace disk drv :start1 (* *drv* 512))
        (replace disk vol :start1 (* *base* 512))
        (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
          (write-sequence disk o))
        (format t "mac68k-hd: ~A: partition map at 1, driver ~D blocks at ~D, volume ~A (~D blocks) at ~D, ~D blocks~%"
                out nd *drv* volume volblocks *base* total)
        (when chdman
          (let ((chd (concatenate 'string out ".chd")))
            (when (probe-file chd) (delete-file chd))
            (let ((words (loop with s = chdman with start = 0
                               for sp = (position #\Space s :start start)
                               collect (subseq s start sp) while sp do (setq start (1+ sp)))))
              (apply #'run (first words)
                     (append (rest words)
                             (list "createhd" "-i" out "-o" chd "-c" "none"
                                   "-chs" (format nil "~D,4,16" (floor total 64)) "-ss" "512"))))
            (format t "mac68k-hd: ~A~%" chd)))))))

(let* ((args (rest sb-ext:*posix-argv*))
       (k (position "--chd" args :test #'string=))
       (chd (and k (nth (1+ k) args))))
  (when k (setq args (append (subseq args 0 k) (subseq args (+ k 2)))))
  (unless (= (length args) 2)
    (format *error-output* "usage: mac68k-hd.lisp VOLUME OUT [--chd CHDMAN]~%")
    (sb-ext:exit :code 2))
  (main (first args) (second args) chd))
