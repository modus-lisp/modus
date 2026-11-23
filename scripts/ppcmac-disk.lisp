;;;; ppcmac-disk.lisp -- a disk a PowerPC Macintosh boots Modus from, through
;;;; Open Firmware, with no Mac OS.
;;;;
;;;;   sbcl --script scripts/ppcmac-disk.lisp IMAGE.elf OUT [--size MB]
;;;;
;;;; OUT is a raw disk: block 0 the driver descriptor, the Apple Partition Map in
;;;; blocks 1-2, and one Apple_HFS partition from block 64 holding an HFS volume
;;;; "Modus" with
;;;;   :modus.elf                    the image (the ELF Open Firmware loads)
;;;;   :System Folder:bootinfo.txt   a CHRP boot script, file type 'tbxi', that
;;;;                                 boots it: what a New World Mac's Open
;;;;                                 Firmware looks for in the blessed folder
;;;; and the System Folder blessed (the volume header's finder info word 0), so a
;;;; real Mac finds it by itself.  Under QEMU (OpenBIOS) give the path:
;;;;   -prom-env 'boot-device=hd:,\modus.elf'
;;;;
;;;; The disk layout is fs-apple's (the partition map, lib/apm.lisp; the volume,
;;;; lib/hfs.lisp); scripts/mac68k-tools.lisp finds fs-apple.

(load (merge-pathnames "mac68k-tools.lisp" *load-truename*))
(unless (fboundp (find-symbol "HFS-WRITE" "FS-APPLE"))
  (dolist (f '("macroman" "appledouble" "diskcopy" "hfs"))
    (load (merge-pathnames (format nil "lib/~A.lisp" f)
                           (mac68k-tools::sibling-dir "fs-apple" "FS_APPLE")))))
(defpackage #:ppcmac-disk (:use #:cl #:mac68k-tools))
(in-package #:ppcmac-disk)

(defparameter *base* 64 "The volume's first block.")

(defun bootinfo (elf-name)
  "The CHRP boot script: Open Firmware runs <BOOT-SCRIPT>.  &device; is the
   device it was loaded from, &partition; its partition."
  (format nil "<CHRP-BOOT>~C<COMPATIBLE>~CMacRISC MacRISC3 MacRISC4~C</COMPATIBLE>~C<DESCRIPTION>~CModus~C</DESCRIPTION>~C<BOOT-SCRIPT>~Cboot &device;:&partition;,\\~A~C</BOOT-SCRIPT>~C</CHRP-BOOT>~C"
          #\Return #\Return #\Return #\Return #\Return #\Return #\Return #\Return
          elf-name #\Return #\Return #\Return))

(defun ascii-bytes (s)
  (map '(vector (unsigned-byte 8)) #'char-code s))

(defun main (image out &key (size-mb nil))
  (let* ((elf (read-bytes image))
         (vol-bytes (* 512 (ceiling (max (* 4 1024 1024)
                                         (+ (* 2 (length elf)) (* 1024 1024)))
                                    512)))
         (vol-bytes (if size-mb (* size-mb 1024 1024) vol-bytes))
         (volume (fs-apple:make-hfs-folder
                  :name "Modus"
                  :entries
                  (list (cons "modus.elf" (fs-apple:make-hfs-file :type "BINA" :creator "MDUS" :data elf))
                        (cons "System Folder"
                              (fs-apple:make-hfs-folder
                               :name "System Folder"
                               :entries (list (cons "bootinfo.txt"
                                                    (fs-apple:make-hfs-file
                                                     :type "tbxi" :creator "chrp"
                                                     :data (ascii-bytes (bootinfo "modus.elf"))))))))))
         (vol (fs-apple:hfs-write volume :size vol-bytes :desktopdb nil))
         (volblocks (floor (length vol) 512))
         (total (+ *base* volblocks))
         (disk (make-array (* total 512) :element-type '(unsigned-byte 8) :initial-element 0)))
    (bless-system-folder vol)
    (fs-apple:apm-write-ddm disk total nil)
    (fs-apple:apm-write-map
     disk (list (list :start 1 :count 2 :name "Apple" :type "Apple_partition_map")
                (list :start *base* :count volblocks :name "Modus" :type "Apple_HFS")))
    (replace disk vol :start1 (* *base* 512))
    (write-bytes out disk)
    (format t "ppcmac-disk: ~A: ~A (~D bytes) on an HFS volume of ~D blocks at block ~D~%"
            out image (length elf) volblocks *base*)))

(defun bless-system-folder (vol)
  "Bless the folder named System Folder: its catalog node id into the volume
   header's drFndrInfo[0] (offset 1024 + 92), which is what Open Firmware
   reads to find the folder it searches for a 'tbxi' file."
  (let ((cnid (system-folder-cnid vol)))
    (when cnid
      (fs-apple:set-u32 vol (+ 1024 92) cnid))))

(defun system-folder-cnid (vol)
  "The CNID hfs-write gave the System Folder: the root is 2, then the
   entries in order from 16.  Found by reading the volume back."
  (declare (ignore vol))
  ;; hfs-write numbers the root's entries from 16 in the order given:
  ;; modus.elf 16, System Folder 17.
  17)

(let ((args (argv)))
  (let ((size (let ((p (position "--size" args :test #'string=)))
                (and p (parse-integer (nth (1+ p) args))))))
    (main (first args) (second args) :size-mb size)))
