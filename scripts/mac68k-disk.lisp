;;;; mac68k-disk.lisp -- a Mac boot disk that IS modus.
;;;;
;;;;   sbcl --script scripts/mac68k-disk.lisp IMAGE DISK [--paged]
;;;;
;;;; IMAGE is a flat bare-Mac image (built for the :mac68k target,
;;;; boot/boot-mac68k.lisp).  DISK is its boot blocks (scripts/mac68k-bootblocks.s,
;;;; assembled here) followed by IMAGE at offset 1024, padded to whole 512-byte
;;;; blocks and to at least 800 KB.  The Mac ROM runs the boot blocks, which read
;;;; IMAGE to its load address and jump to it: no System, no Finder, no
;;;; filesystem.  The boot blocks' SPLASH (boot/mac68k-splash.pbm) follows IMAGE.
;;;;
;;;; Also a library: (defvar cl-user::*mac68k-disk-library* t) before loading it,
;;;; then call MAC68K-DISK:WRITE-DISK / PAGED.  It replaced mac68k-disk.py, byte for byte
;;;; (2026-11-20).

(load (merge-pathnames "mac68k-tools.lisp" *load-truename*))
(defpackage #:mac68k-disk (:use #:cl #:mac68k-tools) (:export #:write-disk #:paged #:*load-addr*))
(in-package #:mac68k-disk)

(defparameter *load-addr*
  (+ (let ((m (string-downcase (env "MODUS_MAC68K_MACHINE" "lc"))))
       (cond ((string= m "lc") #x00100000)
             ((member m '("classic" "classicos" "classic2m" "plus1m") :test #'string=) #x00040000)
             ((string= m "quadra") #x00600000)
             ((string= m "plus1mjit") #x0003C000)
             (t (error "mac68k-disk: no load address for MODUS_MAC68K_MACHINE=~A" m))))
     ;; MODUS_MAC68K_SHIFT (boot-mac68k): the whole map moved
     (env-hex "MODUS_MAC68K_SHIFT"))
  "boot/boot-mac68k.lisp's image address for MODUS_MAC68K_MACHINE.")

(defparameter *splash* (merge-pathnames "../boot/mac68k-splash.pbm" *scripts*))

;;; ---- the splash ---------------------------------------------------------------

(defun pbm-screen (pbm)
  "A 512 x 342 binary PBM's 64 x 342 bytes, or an error."
  (let ((pos 0) (fields nil))
    (flet ((space-p (i) (member (aref pbm i) '(9 10 13 32))))
      (loop while (< (length fields) 3)
            do (loop while (space-p pos) do (incf pos))
               (if (= (aref pbm pos) (char-code #\#))
                   (setq pos (position 10 pbm :start pos))
                   (let ((end pos))
                     (loop until (space-p end) do (incf end))
                     (push (map 'string #'code-char (subseq pbm pos end)) fields)
                     (setq pos end)))))
    (unless (equal (reverse fields) '("P4" "512" "342"))
      (error "mac68k-disk: ~A is not a 512 x 342 binary PBM" *splash*))
    (subseq pbm (+ pos 1) (+ pos 1 (* 64 342)))))

(defun splash ()
  "The boot blocks' splash (mac68k-bootblocks.s): the picture's box of bytes
   that differ from the 50% grey the boot blocks draw first -- X0 (bytes), Y0,
   W (bytes), H as words, then the box XORed with itself two rows up and
   PackBits'd.  NIL when there is no picture or MAC68K_SPLASH=0."
  (when (and (not (equal (env "MAC68K_SPLASH") "0")) (probe-file *splash*))
    (let ((screen (pbm-screen (read-bytes *splash*)))
          (x0 64) (x1 -1) (y0 342) (y1 -1))
      (dotimes (y 342)
        (dotimes (x 64)
          (unless (= (aref screen (+ (* 64 y) x)) (if (evenp y) #xAA #x55))
            (setq x0 (min x0 x) x1 (max x1 x) y0 (min y0 y) y1 (max y1 y)))))
      (when (>= x1 0)
        (let* ((w (+ (- x1 x0) 1)) (h (+ (- y1 y0) 1))
               (rows (make-array (* w h) :element-type '(unsigned-byte 8)))
               (delta (make-array (* w h) :element-type '(unsigned-byte 8))))
          (dotimes (r h)
            (replace rows screen :start1 (* r w) :start2 (+ (* 64 (+ y0 r)) x0) :end2 (+ (* 64 (+ y0 r)) x0 w)))
          (dotimes (i (length rows))
            (setf (aref delta i) (logxor (aref rows i) (if (>= i (* 2 w)) (aref rows (- i (* 2 w))) 0))))
          (let ((packed (fs-apple:packbits delta)))
            (unless (equalp (fs-apple:unpackbits packed (length delta)) (coerce delta 'simple-vector))
              (error "mac68k-disk: the splash does not unpack to itself"))
            (let ((data (make-array (+ 8 (length packed)) :element-type '(unsigned-byte 8))))
              (fs-apple:set-u16 data 0 x0) (fs-apple:set-u16 data 2 y0)
              (fs-apple:set-u16 data 4 w) (fs-apple:set-u16 data 6 h)
              (replace data packed :start1 8)
              (when (> (length data) #x2000)          ; unpacked at DEST+#x2000
                (error "mac68k-disk: the splash packs to ~D bytes, over 8 KB" (length data)))
              data)))))))

;;; ---- the disk -----------------------------------------------------------------

(defun write-disk (img disk &key (load-addr *load-addr*) (min-size 819200) entry read-len
                                 (refslot 0) (memtop 0) (kmap 0))
  "Boot blocks that read IMG (bytes) to LOAD-ADDR and jump to ENTRY (default
   LOAD-ADDR), then IMG at offset 1024, padded to whole blocks and to at least
   MIN-SIZE.  The splash, when there is one, goes after IMG.  REFSLOT and
   MEMTOP, for an image that can be launched from Mac OS: where it wants the
   open file's reference number, and the RAM its map needs; KMAP the kind
   map's address when the layout puts the young heap last.  The disk's length."
  (multiple-value-bind (bb syms) (assemble "mac68k-bootblocks.s")
    (unless (= (gethash "start" syms) #x8A)
      (error "mac68k-disk: the loader is at #x~X, not #x8A" (gethash "start" syms)))
    (when (> (length bb) 1024)
      (error "mac68k-disk: boot blocks are ~D bytes" (length bb)))
    (flet ((patch (name v) (fs-apple:set-u32 bb (gethash name syms) v)))
      (patch "dest" load-addr)
      (patch "entry" (or entry load-addr))
      ;; a disk driver reads whole 512-byte blocks (a byte count is paramErr, -50)
      (patch "len" (* 512 (ceiling (or read-len (length img)) 512)))
      (patch "off" 1024)
      (patch "refslot" refslot)                ; 0: this image cannot be launched from Mac OS
      (patch "memtop" memtop)
      (patch "kmap" kmap))
    (let* ((sp (splash))
           (body (+ 1024 (* 512 (ceiling (length img) 512))))
           (splen (if sp (* 512 (ceiling (length sp) 512)) 0))
           (out (make-array (max min-size (+ body splen)) :element-type '(unsigned-byte 8)
                                                          :initial-element 0)))
      (replace out bb)
      (replace out img :start1 1024)
      (when sp
        (fs-apple:set-u32 out (gethash "spoff" syms) body)
        (fs-apple:set-u32 out (gethash "splen" syms) splen)
        (replace out sp :start1 body))
      (write-bytes disk out)
      (length out))))

(defun find-bytes (needle v &key (start 0))
  (search (map '(vector (unsigned-byte 8)) #'char-code needle) v :start2 start))

(defun paged (img)
  "A paged image (MODUS_68K_PAGED): (values IMG with its stream table marked
   NOT LOADED -- bit 0 of every entry --, the bytes the boot blocks must read:
   the resident part, up to the stream section, and the stream count)."
  (let ((k (find-bytes "modusPGD" img)))
    (when (or (null k) (find-bytes "modusPGD" img :start (1+ k)))
      (error "mac68k-disk: --paged needs one modusPGD descriptor in the image"))
    (let* ((img (copy-seq img))
           (table (fs-apple:u32-ref img (+ k 8))) (count (fs-apple:u32-ref img (+ k 12)))
           (section (fs-apple:u32-ref img (+ k 16)))
           (tab (- table *load-addr*)))
      (dotimes (i count)
        (let ((at (+ tab (* 4 i))))
          (fs-apple:set-u32 img at (logior (fs-apple:u32-ref img at) 1))))
      (values img (- section *load-addr*) count))))

(defun main (image disk &rest opts)
  (let ((img (read-bytes image)))
    (if (member "--paged" opts :test #'string=)
        (multiple-value-bind (img resident count) (paged img)
          (write-disk img disk :read-len resident)
          (format t "mac68k-disk: ~A paged: ~D resident bytes boot, ~D streams (~D bytes) on demand~%"
                  image resident count (- (length img) resident)))
        (let ((n (write-disk img disk)))
          (format t "mac68k-disk: ~A (~D bytes at #x~X) -> ~A, ~D bytes~%"
                  image (length img) *load-addr* disk n)))))

(unless (and (boundp 'cl-user::*mac68k-disk-library*) (symbol-value 'cl-user::*mac68k-disk-library*))
  (apply #'main (argv)))
