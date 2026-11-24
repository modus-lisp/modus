;;;; ppcmac-disk.lisp -- a disk a PowerPC Macintosh boots Modus from, through
;;;; Open Firmware, with no Mac OS.
;;;;
;;;;   sbcl --script scripts/ppcmac-disk.lisp IMAGE.elf OUT [--size MB]
;;;;
;;;; OUT is a raw disk: block 0 the driver descriptor, the Apple Partition Map in
;;;; blocks 1-2, and one Apple_HFS partition from block 64 holding an HFS volume
;;;; "Modus" with
;;;;   :modus.elf                    the image
;;;;   :modus-boot.elf               the first stage (IMAGE.boot from the build,
;;;;                                 or --loader): Open Firmware boots it, and it
;;;;                                 reads modus.elf into place -- a real Mac's
;;;;                                 firmware cannot load a 37 MB ELF itself
;;;;                                 (boot-ppcmac.lisp, THE FIRST STAGE)
;;;;   :System Folder:bootinfo.txt   a CHRP boot script, file type 'tbxi', that
;;;;                                 boots it: what a New World Mac's Open
;;;;                                 Firmware looks for in the blessed folder
;;;; and the System Folder blessed (the volume header's finder info word 0), so a
;;;; real Mac finds it by itself.  Under QEMU (OpenBIOS) give the path:
;;;;   -prom-env 'boot-device=hd:,\modus.elf'
;;;;
;;;; --payload LIST adds a second segment to the ELF, loaded at #x07200000: the
;;;; boot payload the image reads before its REPL (build-cl-repl-common.lisp,
;;;; %PPCMAC-BOOT-PAYLOAD).  LIST has a line per entry, NAME<tab>PATH, in order;
;;;; a .lisp entry is evaluated at boot, any other kept as bytes by NAME.
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

(defun bootinfo (elf-name &optional loader)
  "The CHRP boot script: Open Firmware runs <BOOT-SCRIPT>.  &device; is the
   device it was loaded from, &partition; its partition.  With LOADER, the
   firmware boots it and passes the image's path as its argument."
  (if loader
      (format nil "<CHRP-BOOT>~C<COMPATIBLE>~CMacRISC MacRISC3 MacRISC4~C</COMPATIBLE>~C<DESCRIPTION>~CModus~C</DESCRIPTION>~C<BOOT-SCRIPT>~Cboot &device;:&partition;,\\~A &device;:&partition;,\\~A~C</BOOT-SCRIPT>~C</CHRP-BOOT>~C"
              #\Return #\Return #\Return #\Return #\Return #\Return #\Return #\Return
              loader elf-name #\Return #\Return #\Return)
  (format nil "<CHRP-BOOT>~C<COMPATIBLE>~CMacRISC MacRISC3 MacRISC4~C</COMPATIBLE>~C<DESCRIPTION>~CModus~C</DESCRIPTION>~C<BOOT-SCRIPT>~Cboot &device;:&partition;,\\~A~C</BOOT-SCRIPT>~C</CHRP-BOOT>~C"
          #\Return #\Return #\Return #\Return #\Return #\Return #\Return #\Return
          elf-name #\Return #\Return #\Return)))

(defconstant +payload-addr+ #x07200000)

(defun be32 (v n) (dotimes (i 4) (vector-push-extend (ldb (byte 8 (- 24 (* 8 i))) n) v)))

(defun payload-bytes (list-file)
  "The payload container: MDPL, a count, then per entry its name and data."
  (let ((entries (with-open-file (in list-file)
                   (loop for l = (read-line in nil) while l
                         for tab = (position #\Tab l)
                         when tab collect (cons (subseq l 0 tab) (subseq l (1+ tab))))))
        (v (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for c across "MDPL" do (vector-push-extend (char-code c) v))
    (be32 v (length entries))
    (flet ((blob (bytes)
             (be32 v (length bytes))
             (loop for b across bytes do (vector-push-extend b v))
             (loop until (zerop (mod (length v) 4)) do (vector-push-extend 0 v))))
      (dolist (e entries)
        (blob (ascii-bytes (car e)))
        (blob (read-bytes (cdr e)))))
    (format t "ppcmac-disk: payload: ~D entries, ~D bytes at #x~X~%" (length entries) (length v) +payload-addr+)
    (coerce v '(simple-array (unsigned-byte 8) (*)))))

(defun elf-with-payload (elf payload)
  "ELF (one PT_LOAD at offset 0, big-endian ELF32) with PAYLOAD as a second
   PT_LOAD at +PAYLOAD-ADDR+.  The original file moves to offset #x1000 and maps
   where it did, so its entry point and layout are unchanged."
  (flet ((u32 (b o) (logior (ash (aref b o) 24) (ash (aref b (+ o 1)) 16) (ash (aref b (+ o 2)) 8) (aref b (+ o 3))))
         (put32 (b o n) (dotimes (i 4) (setf (aref b (+ o i)) (ldb (byte 8 (- 24 (* 8 i))) n))))
         (put16 (b o n) (setf (aref b o) (ldb (byte 8 8) n) (aref b (+ o 1)) (ldb (byte 8 0) n))))
    (let* ((phoff (u32 elf 28))
           (vaddr (u32 elf (+ phoff 8))) (filesz (u32 elf (+ phoff 16))) (memsz (u32 elf (+ phoff 20)))
           (off1 #x1000)
           (off2 (* #x1000 (ceiling (+ off1 (length elf)) #x1000)))
           (out (make-array (+ off2 (length payload)) :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace out elf :end2 52)                       ; the ELF header ...
      (put32 out 28 52) (put16 out 42 32) (put16 out 44 2)   ; ... two program headers at 52
      (flet ((phdr (at off va fsz msz flags)
               (put32 out at 1) (put32 out (+ at 4) off) (put32 out (+ at 8) va) (put32 out (+ at 12) va)
               (put32 out (+ at 16) fsz) (put32 out (+ at 20) msz) (put32 out (+ at 24) flags)
               (put32 out (+ at 28) #x1000)))
        (phdr 52 off1 vaddr filesz memsz 7)
        (phdr 84 off2 +payload-addr+ (length payload) (length payload) 6))
      (replace out elf :start1 off1)
      (replace out payload :start1 off2)
      out)))

(defun ascii-bytes (s)
  (map '(vector (unsigned-byte 8)) #'char-code s))

(defun main (image out &key (size-mb nil) payload loader)
  (let* ((loader (or loader (let ((b (concatenate 'string image ".boot"))) (and (probe-file b) b))))
         (boot (and loader (read-bytes loader)))
         (elf (if payload (elf-with-payload (read-bytes image) (payload-bytes payload)) (read-bytes image)))
         (vol-bytes (* 512 (ceiling (max (* 4 1024 1024)
                                         (+ (* 2 (length elf)) (* 1024 1024)))
                                    512)))
         (vol-bytes (if size-mb (* size-mb 1024 1024) vol-bytes))
         (files (list (cons "modus.elf" (fs-apple:make-hfs-file :type "BINA" :creator "MDUS" :data elf))))
         (files (if boot
                    (append files (list (cons "modus-boot.elf"
                                              (fs-apple:make-hfs-file :type "BINA" :creator "MDUS" :data boot))))
                    files))
         (volume (fs-apple:make-hfs-folder
                  :name "Modus"
                  :entries
                  ;; the System Folder FIRST: hfs-write numbers the root's
                  ;; entries from 16 in order, so its CNID is 16 whatever follows
                  (cons (cons "System Folder"
                              (fs-apple:make-hfs-folder
                               :name "System Folder"
                               :entries (list (cons "bootinfo.txt"
                                                    (fs-apple:make-hfs-file
                                                     :type "tbxi" :creator "chrp"
                                                     :data (ascii-bytes (bootinfo "modus.elf" (and boot "modus-boot.elf"))))))))
                        files)))
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
    (format t "ppcmac-disk: ~A: ~A (~D bytes)~:[~; and its first stage~] on an HFS volume of ~D blocks at block ~D~%"
            out image (length elf) boot volblocks *base*)))

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
  ;; hfs-write numbers the root's entries from 16 in the order given, and
  ;; MAIN puts the System Folder first.
  16)

(let ((args (argv)))
  (let ((size (let ((p (position "--size" args :test #'string=)))
                (and p (parse-integer (nth (1+ p) args)))))
        (payload (let ((p (position "--payload" args :test #'string=)))
                   (and p (nth (1+ p) args))))
        (loader (let ((p (position "--loader" args :test #'string=)))
                  (and p (nth (1+ p) args)))))
    (main (first args) (second args) :size-mb size :payload payload :loader loader)))
