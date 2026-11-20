;;;; mac68k-files.lisp -- files on a bare Mac's floppies: FAT12 (fs-fat) on the
;;;; ROM's disk drivers.
;;;;
;;;; The floppy set's second disk (scripts/mac68k-floppy.lisp --core) is a FAT12
;;;; floppy: the cold code is one file on it, MODUS/COLD.BIN, and the rest is the
;;;; user's -- written on any modern machine with a USB floppy drive, read and
;;;; written here.  So is any other FAT floppy in any drive.
;;;;
;;;;   (floppy:drives)                 -> ((drive refnum) ...), the drive queue
;;;;   (floppy:mount &optional drive)  -> the drive mounted: DRIVE, or the first
;;;;                                      that holds a FAT floppy (*VOLUME*)
;;;;   (floppy:dir &optional path)     -> ("NAME" size) and "DIR/" entries
;;;;   (floppy:read-file path)         -> its bytes
;;;;   (floppy:read-text path)         -> a string (CR LF and CR read as newlines)
;;;;   (floppy:write-file path data)   -> DATA, a string or bytes, as the file
;;;;   (floppy:remove path) (floppy:rename old new) (floppy:mkdir path)
;;;;   (floppy:load-file path)         -> LOAD of the file's text
;;;;
;;;; THE DISK.  translate-68k's Mac disk service: SYSCALL3 5 selects a drive, 6
;;;; and 7 read and write whole 512-byte blocks of it through *IO-BUF-ADDR*
;;;; (+ 8 KB: the cold names read its first block), 8 reads the drive queue.
;;;; fs-fat's block device asks for any bytes and writes inside one sector, so a
;;;; sector cache sits between (the FAT's entries would otherwise be a floppy
;;;; read each); writes go through it to the disk at once.
;;;;
;;;; HOW IT GETS INTO THE IMAGE.  fs-fat is its own repository and package, and
;;;; the image's own source is kernel code, so fs-fat's lib/fat32.lisp and this
;;;; file are baked as TEXT (build-cl-repl-common.lisp, FLOPPY FILES) and LOADed
;;;; in the build's core run, before the freeze: the saved core carries them
;;;; compiled, and the text itself is init-only (the static heap goes over it).

(defpackage "FLOPPY"
  (:use "COMMON-LISP" "FS-FAT")
  (:export "DRIVES" "MOUNT" "DIR" "READ-FILE" "READ-TEXT" "WRITE-FILE" "REMOVE"
           "RENAME" "MKDIR" "LOAD-FILE" "*VOLUME*" "*DRIVE*")
  (:shadow "REMOVE"))
(in-package "FLOPPY")

(defvar *volume* nil "The mounted volume (fs-fat's), or NIL.")
(defvar *drive* nil "Its drive number.")
(defvar *cache* nil "(sector . its 512 bytes), the latest first.")
(defparameter *cache-max* 64)

(defun %buf () (+ modus-int::*io-buf-addr* 8192))

(defun drives ()
  "The Mac's drives, in the drive queue's order: (drive-number driver-refnum) each."
  (let ((out nil) (i 0))
    (loop
      (let ((d (modus-int::syscall3 8 i 6 0)))
        (when (eql d 0) (return (nreverse out)))
        (push (list d (modus-int::syscall3 8 i 8 0)) out)
        (setq i (+ i 1))))))

(defun %remember (s v)
  (setq *cache* (cons (cons s v) (delete s *cache* :key #'car)))
  (when (> (length *cache*) *cache-max*)
    (setq *cache* (subseq *cache* 0 *cache-max*)))
  v)

(defun %read-sector (s)
  (let ((hit (assoc s *cache*)))
    (if hit
        (cdr hit)
        (let ((b (%buf)))
          (modus-int::syscall3 5 *drive* 0 0)
          (let ((r (modus-int::syscall3 6 (* s 512) b 512)))
            (unless (eql r 0)
              (error "floppy: reading sector ~D of drive ~D: error ~D" s *drive* r)))
          (let ((v (make-array 512 :element-type '(unsigned-byte 8))))
            (dotimes (i 512) (setf (aref v i) (mem-ref (+ b i) :u8)))
            (%remember s v))))))

(defun %write-sector (s v)
  (let ((b (%buf)))
    (dotimes (i 512) (setf (mem-ref (+ b i) :u8) (aref v i)))
    (modus-int::syscall3 5 *drive* 0 0)
    (let ((r (modus-int::syscall3 7 (* s 512) b 512)))
      (unless (eql r 0)
        (error "floppy: writing sector ~D of drive ~D: error ~D (a locked disk is -44)" s *drive* r)))
    (%remember s v)))

(defun %dev-read (off n)
  "fs-fat's block device read: N bytes from byte OFF."
  (let ((out (make-array n :element-type '(unsigned-byte 8))) (pos 0))
    (loop while (< pos n)
          do (let* ((at (+ off pos)) (k (mod at 512))
                    (take (min (- 512 k) (- n pos))))
               (replace out (%read-sector (floor at 512)) :start1 pos :start2 k :end2 (+ k take))
               (setq pos (+ pos take))))
    out))

(defun %dev-write (off src)
  "fs-fat's block device write: SRC at OFF, inside one sector."
  (let* ((s (floor off 512)) (v (copy-seq (%read-sector s))))
    (replace v src :start1 (mod off 512))
    (%write-sector s v)))

(defun %u16 (v i) (+ (aref v i) (* 256 (aref v (+ i 1)))))

(defun mount (&optional drive)
  "Open the FAT floppy in DRIVE (a number from DRIVES), or in the first drive that
   holds one, as *VOLUME*.  Its drive number."
  (setq *volume* nil)
  (dolist (d (if drive (list drive) (mapcar #'first (drives)))
             (error "floppy: no FAT floppy in ~A" (if drive (format nil "drive ~D" drive) "any drive")))
    (setq *drive* d *cache* nil)
    (let ((boot (handler-case (%read-sector 0) (error () nil))))
      (when (and boot (= (aref boot 510) #x55) (= (aref boot 511) #xAA) (= (%u16 boot 11) 512))
        (let* ((tot16 (%u16 boot 19))
               (total (if (> tot16 0) tot16 (+ (%u16 boot 32) (* 65536 (%u16 boot 34)))))
               (v (handler-case
                      (fat-open boot (make-fat32-block-device
                                             (* total 512) #'%dev-read #'%dev-write))
                    (error () nil))))
          (when v (setq *volume* v) (return d)))))))

(defun %vol () (or *volume* (mount)))

(defun dir (&optional (path "/"))
  "PATH's entries: (\"NAME\" size) for a file, \"NAME/\" for a directory."
  (mapcar (lambda (e) (if (second e) (concatenate 'string (first e) "/") (list (first e) (third e))))
          (or (fat32-directory (%vol) path)
              (if (fat32-lookup (%vol) path) nil (error "floppy: no directory ~A" path)))))

(defun read-file (path)
  (or (fat32-read-file (%vol) path) (error "floppy: no file ~A" path)))

(defun read-text (path)
  "PATH's bytes as a string, a character a byte; CR LF and a lone CR read as newlines."
  (let ((b (read-file path)))
    (with-output-to-string (s)
      (let ((i 0) (n (length b)))
        (loop while (< i n)
              do (let ((c (aref b i)))
                   (cond ((= c 13)
                          (write-char #\Newline s)
                          (when (and (< (+ i 1) n) (= (aref b (+ i 1)) 10)) (setq i (+ i 1))))
                         (t (write-char (code-char c) s))))
                 (setq i (+ i 1)))))))

(defun write-file (path data)
  "DATA (a string, a byte a character, or bytes) as the file PATH, made or replaced."
  (let ((bytes (if (stringp data)
                   (map '(vector (unsigned-byte 8)) #'char-code data)
                   (coerce data '(vector (unsigned-byte 8))))))
    (fat32-write-file (%vol) path bytes)
    (length bytes)))

(defun remove (path) (fat32-delete (%vol) path) t)
(defun rename (old new) (fat32-rename (%vol) old new) t)
(defun mkdir (path) (fat32-mkdir (%vol) path) t)

(defun load-file (path)
  "LOAD the Lisp source in PATH."
  (load (make-string-input-stream (read-text path))))
