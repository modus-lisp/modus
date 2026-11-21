;;;; mac68k-hd.lisp -- a SCSI hard disk that boots a 68k Mac with no Mac OS.
;;;;
;;;;   sbcl --script scripts/mac68k-hd.lisp VOLUME OUT [--chd CHDMAN]
;;;;
;;;; VOLUME is a modus boot volume (mac68k-core.lisp, mac68k-disk.lisp or
;;;; mac68k-floppy.lisp output: boot blocks at byte 0).  OUT is a raw disk: block 0
;;;; the driver descriptor (one driver, Mac OS), a partition map in blocks 1-3,
;;;; the driver (scripts/mac68k-scsi.s, its BASE and VOLBLOCKS words patched in)
;;;; from block 4, VOLUME from block 64; with --chd, OUT.chd for MAME as well
;;;; (chdman createhd).  The ROM loads the driver and enters it; the driver adds
;;;; a drive that is VOLUME, and the ROM boots its boot blocks.
;;;;
;;;; The disk layout itself is fs-apple's (the Apple Partition Map, lib/apm.lisp;
;;;; scripts/mac68k-tools.lisp finds fs-apple).  It replaced mac68k-hd.py, byte
;;;; for byte (2026-11-20).

(load (merge-pathnames "mac68k-tools.lisp" *load-truename*))
(defpackage #:mac68k-hd (:use #:cl #:mac68k-tools))
(in-package #:mac68k-hd)

(defparameter *base* 64 "The volume's first block.")
(defparameter *mapn* 3 "Partition map entries: blocks 1-3.")
(defparameter *drv* (+ 1 *mapn*) "The driver's first block.")

(defun split-words (s)
  (loop with start = 0
        for sp = (position #\Space s :start start)
        collect (subseq s start sp) while sp do (setq start (1+ sp))))

(defun main (volume out &optional chdman)
  (let* ((vol (pad-to (read-bytes volume) 512))
         (volblocks (floor (length vol) 512)))
    (multiple-value-bind (drv syms) (assemble "mac68k-scsi.s")
      (fs-apple:set-u32 drv (gethash "base" syms) *base*)
      (fs-apple:set-u32 drv (gethash "volblocks" syms) volblocks)
      (let* ((drv (pad-to drv 512))
             (nd (floor (length drv) 512))
             (total (* 64 (ceiling (+ *base* volblocks) 64)))   ; whole cylinders (chdman)
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
        (write-bytes out disk)
        (format t "mac68k-hd: ~A: partition map at 1, driver ~D blocks at ~D, volume ~A (~D blocks) at ~D, ~D blocks~%"
                out nd *drv* volume volblocks *base* total)
        (when chdman
          (let ((chd (concatenate 'string out ".chd")) (words (split-words chdman)))
            (when (probe-file chd) (delete-file chd))
            (apply #'run (first words)
                   (append (rest words)
                           (list "createhd" "-i" out "-o" chd "-c" "none"
                                 "-chs" (format nil "~D,4,16" (floor total 64)) "-ss" "512")))
            (format t "mac68k-hd: ~A~%" chd)))))))

(let* ((args (argv))
       (k (position "--chd" args :test #'string=))
       (chd (and k (nth (1+ k) args))))
  (when k (setq args (append (subseq args 0 k) (subseq args (+ k 2)))))
  (unless (= (length args) 2)
    (format *error-output* "usage: mac68k-hd.lisp VOLUME OUT [--chd CHDMAN]~%")
    (sb-ext:exit :code 2))
  (main (first args) (second args) chd))
