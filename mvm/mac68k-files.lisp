;;;; mac68k-files.lisp -- files on a bare Mac's floppies: FAT12 (fs-fat) on the
;;;; ROM's disk drivers.  Image (kernel) source, compiled native.
;;;;
;;;; The floppy set's second disk (scripts/mac68k-floppy.lisp --core) is a FAT12
;;;; floppy: the cold code is one file on it, MODUS/COLD.BIN, and the rest is the
;;;; user's -- written on any modern machine with a USB floppy drive, read and
;;;; written here.  So is any other FAT floppy in any drive.  The package FLOPPY:
;;;;
;;;;   (floppy:drives)                 -> ((drive refnum) ...), the drive queue
;;;;   (floppy:mount &optional drive)  -> the drive mounted: DRIVE, or the first
;;;;                                      that holds a FAT floppy
;;;;   (floppy:dir &optional path)     -> ("NAME" size) and "DIR/" entries
;;;;   (floppy:read-file path)         -> its bytes
;;;;   (floppy:read-text path)         -> a string (CR LF and CR read as newlines)
;;;;   (floppy:write-file path data)   -> DATA, a string or bytes, as the file
;;;;   (floppy:remove path) (floppy:rename old new) (floppy:mkdir path)
;;;;   (floppy:load-file path)         -> LOAD of the file's text
;;;;   (floppy:volume)                 -> the mounted fs-fat volume
;;;;
;;;; THE DISK.  translate-68k's Mac disk service: SYSCALL3 8 reads the drive
;;;; queue, and a block goes in or out through %MAC-DISK-IO
;;;; (mvm/mac68k-disk-io.lisp).  fs-fat's block device asks for any bytes and
;;;; writes inside one sector, so a sector cache sits between (the FAT's entries
;;;; would otherwise be a floppy read each); writes go through it at once.
;;;;
;;;; HOW IT GETS INTO THE IMAGE.  fs-fat's lib/fat32.lisp -- its own repository
;;;; -- is read at build time (FS_FAT, or a checkout beside this one) and
;;;; compiled into the image as kernel code, its DEFPACKAGE / IN-PACKAGE /
;;;; DECLAIM left out (build-cl-repl-common.lisp, FLOPPY FILES): its functions
;;;; are image functions like any other, native, cold until first called.  (It
;;;; was LOADed as text in the core run once: then it ran as bytecode, never
;;;; translated -- a Mac translates at DEFUN, and the core run cannot -- and a
;;;; MOUNT and a DIR took minutes on MAME's SE/30.)  The core run makes the
;;;; package FLOPPY and points its names at the functions below
;;;; (%MAC-FILES-INSTALL).

(defvar *floppy-volume* nil)
(defvar *floppy-drive* nil)
(defvar *floppy-cache* nil)

(defun %floppy-drives ()
  (let ((out nil) (i 0))
    (loop
      (let ((d (syscall3 8 i 6 0)))
        (when (eql d 0) (return (nreverse out)))
        (push (list d (syscall3 8 i 8 0)) out)
        (setq i (+ i 1))))))

(defun %floppy-remember (s v)
  (setq *floppy-cache* (cons (cons s v) (delete s *floppy-cache* :key #'car)))
  (when (> (length *floppy-cache*) 64)
    (setq *floppy-cache* (subseq *floppy-cache* 0 64)))
  v)

(defun %floppy-read-sector (s)
  (let ((hit (assoc s *floppy-cache*)))
    (if hit
        (cdr hit)
        (let* ((v (make-array 512 :element-type '(unsigned-byte 8)))
               (r (%mac-disk-io 6 *floppy-drive* (* s 512) v)))
          (unless (eql r 0)
            (error "floppy: reading sector ~D of drive ~D: error ~D" s *floppy-drive* r))
          (%floppy-remember s v)))))

(defun %floppy-write-sector (s v)
  (let ((r (%mac-disk-io 7 *floppy-drive* (* s 512) v)))
    (unless (eql r 0)
      (error "floppy: writing sector ~D of drive ~D: error ~D (a locked disk is -44)"
             s *floppy-drive* r)))
  (%floppy-remember s v))

(defun %floppy-dev-read (off n)
  "fs-fat's block device read: N bytes from byte OFF."
  (let ((out (make-array n :element-type '(unsigned-byte 8))) (pos 0))
    (loop while (< pos n)
          do (let* ((at (+ off pos)) (k (mod at 512))
                    (take (min (- 512 k) (- n pos))))
               (replace out (%floppy-read-sector (floor at 512)) :start1 pos :start2 k :end2 (+ k take))
               (setq pos (+ pos take))))
    out))

(defun %floppy-dev-write (off src)
  "fs-fat's block device write: SRC at OFF, inside one sector."
  (let* ((s (floor off 512)) (v (copy-seq (%floppy-read-sector s))))
    (replace v src :start1 (mod off 512))
    (%floppy-write-sector s v)))

(defun %floppy-u16 (v i) (+ (aref v i) (* 256 (aref v (+ i 1)))))

(defun %floppy-boot-sector (d)
  "Drive D's sector 0, or NIL when it cannot be read (no disk, our own drive)."
  (setq *floppy-drive* d *floppy-cache* nil)
  (handler-case (%floppy-read-sector 0) (error (c) c nil)))

(defun %floppy-open (boot)
  "fs-fat's volume over the drive BOOT was read from, or NIL if it is not FAT."
  (when (and (= (aref boot 510) #x55) (= (aref boot 511) #xAA) (= (%floppy-u16 boot 11) 512))
    (let* ((tot16 (%floppy-u16 boot 19))
           (total (if (> tot16 0) tot16 (+ (%floppy-u16 boot 32) (* 65536 (%floppy-u16 boot 34))))))
      (handler-case
          (fat-open boot (make-fat32-block-device (* total 512)
                                                  #'%floppy-dev-read #'%floppy-dev-write))
        (error (c) c nil)))))

(defun %floppy-mount (&optional drive)
  (setq *floppy-volume* nil)
  (dolist (d (if drive (list drive) (mapcar #'first (%floppy-drives)))
             (error "floppy: no FAT floppy in ~A" (if drive (format nil "drive ~D" drive) "any drive")))
    (let* ((boot (%floppy-boot-sector d))
           (v (and boot (%floppy-open boot))))
      (when v (setq *floppy-volume* v) (return d)))))

(defun %floppy-vol () (or *floppy-volume* (progn (%floppy-mount) *floppy-volume*)))

(defun %floppy-dir (&optional (path "/"))
  (mapcar (lambda (e) (if (second e) (concatenate 'string (first e) "/") (list (first e) (third e))))
          (or (fat32-directory (%floppy-vol) path)
              (if (fat32-lookup (%floppy-vol) path) nil (error "floppy: no directory ~A" path)))))

(defun %floppy-read-file (path)
  (or (fat32-read-file (%floppy-vol) path) (error "floppy: no file ~A" path)))

(defun %floppy-read-text (path)
  (let ((b (%floppy-read-file path)))
    (with-output-to-string (s)
      (let ((i 0) (n (length b)))
        (loop while (< i n)
              do (let ((c (aref b i)))
                   (cond ((= c 13)
                          (write-char #\Newline s)
                          (when (and (< (+ i 1) n) (= (aref b (+ i 1)) 10)) (setq i (+ i 1))))
                         (t (write-char (code-char c) s))))
                 (setq i (+ i 1)))))))

(defun %floppy-write-file (path data)
  (let* ((n (length data))
         (bytes (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n)
      (let ((x (elt data i)))
        (setf (aref bytes i) (if (characterp x) (char-code x) x))))
    (fat32-write-file (%floppy-vol) path bytes)
    n))

(defun %floppy-remove (path) (fat32-delete (%floppy-vol) path) t)
(defun %floppy-rename (old new) (fat32-rename (%floppy-vol) old new) t)
(defun %floppy-mkdir (path) (fat32-mkdir (%floppy-vol) path) t)
(defun %floppy-load-file (path) (load (make-string-input-stream (%floppy-read-text path))))
(defun %floppy-volume () *floppy-volume*)

(defun %mac-files-install ()
  "The core run: the package FLOPPY, its names this file's functions."
  (let ((p (or (find-package "FLOPPY") (make-package "FLOPPY" :use (list "COMMON-LISP")))))
    (shadow (list "REMOVE") p)
    (dolist (e (list (cons "DRIVES" (function %floppy-drives))
                     (cons "MOUNT" (function %floppy-mount))
                     (cons "DIR" (function %floppy-dir))
                     (cons "READ-FILE" (function %floppy-read-file))
                     (cons "READ-TEXT" (function %floppy-read-text))
                     (cons "WRITE-FILE" (function %floppy-write-file))
                     (cons "REMOVE" (function %floppy-remove))
                     (cons "RENAME" (function %floppy-rename))
                     (cons "MKDIR" (function %floppy-mkdir))
                     (cons "LOAD-FILE" (function %floppy-load-file))
                     (cons "VOLUME" (function %floppy-volume))))
      (let ((s (intern (car e) p)))
        (setf (fdefinition s) (cdr e))
        (export (list s) p)))
    t))
