;;;; mac68k-floppy.lisp -- compressed Mac boot disks.
;;;;
;;;;   sbcl --script scripts/mac68k-floppy.lisp IMAGE DISK [--hd] [--load HEX]
;;;;
;;;; IMAGE is a flat bare-Mac image (built for :mac68k, linked at #x00100000), or
;;;; with --load HEX one that goes at HEX -- a saved core (mac68k-core.py's
;;;; MAC68K_CORE_RAW: the image, its static heap and low block, from the map's :LOAD).
;;;; It is compressed as a raw LZMA1 stream (cram's LZMA-COMPRESS, lc 2 lp 1 pb 2)
;;;; behind a loader, scripts/mac68k-unlzma.s: 68020 assembly, under 1 KB, its
;;;; decoder scripts/mac68k-lzma.inc (test/mac68k-unlzma/run.sh checks it).
;;;; The boot blocks (scripts/mac68k-disk.lisp) read the blob to #x01200000; the
;;;; loader unpacks IMAGE to #x00100000 (or --load) and jumps to it.
;;;;
;;;; THE FAST FLOPPY is a saved core (boot/boot-mac68k.lisp :FLOPPY): the REPL in
;;;; ~105 s on a 68040 (MAME's Quadra 950) where a fresh image takes ~400 s, all
;;;; but ~2 s of it reading the disk and unpacking.  Build the image with
;;;; MODUS_MAC68K_MACHINE=floppy and mac68k-snap-build.sh's quadra knobs but
;;;; MODUS_68K_COLD_KEEP= (empty: nothing may be left on a disk), take its core
;;;; with MAC68K_CORE_RAW=CORE scripts/mac68k-core.py IMAGE DISK, then
;;;;   MODUS_MAC68K_MACHINE=floppy sbcl --script scripts/mac68k-floppy.lisp CORE DISK --hd --load 100000
;;;;
;;;; Disks: one 800 KB floppy, DISK, if the blob fits; else, with --hd, one 1.44 MB
;;;; floppy; else TWO 800 KB floppies, DISK-1 and DISK-2.  A two-disk set boots
;;;; from DISK-1, whose boot blocks enter stage 2 (scripts/mac68k-disk2.s) instead
;;;; of the loader: it finds DISK-2 in any drive -- or ejects DISK-1 and asks for
;;;; it on screen -- and reads its share of the stream in after DISK-1's.
;;;; DISK-2's block 0 is a label: "MODUS-D2", the set's id, the data length.
;;;;
;;;; It replaced mac68k-floppy.py (2026-11-20): the same layout byte for byte but
;;;; the LZMA stream, which is cram's encoder's (any LZMA1 decoder reads it; xz's
;;;; -9e parse packs ~12% smaller).

(defvar cl-user::*mac68k-disk-library* t)
(load (merge-pathnames "mac68k-disk.lisp" *load-truename*))
(defpackage #:mac68k-floppy (:use #:cl #:mac68k-tools))
(in-package #:mac68k-floppy)
(load-cram)

(defparameter *root* (merge-pathnames "../" *scripts*))
(defconstant +loader-addr+ #x01200000 "Where the boot blocks put the blob")
(defconstant +pay-off+ #x4000 "mac68k-unlzma.s PAY")
(defconstant +out+ #x00100000 "mac68k-unlzma.s OUT")
(defconstant +state+ #x01A00000 "mac68k-unlzma.s STATE: the blob must end below it")
(defconstant +floppy+ 819200)
(defconstant +hd+ 1474560)
(defparameter *message* "INSERT MODUS DISK 2")

(defun jumper (out)
  "At +PAY+ + 8: CPUSHA BC; MOVEA.L #OUT,A0; JMP (A0); NOP (pad)."
  (let ((v (make-array 12 :element-type '(unsigned-byte 8))))
    (fs-apple:set-u16 v 0 #xF4F8) (fs-apple:set-u16 v 2 #x207C) (fs-apple:set-u32 v 4 out)
    (fs-apple:set-u16 v 8 #x4ED0) (fs-apple:set-u16 v 10 #x4E71)
    v))

(defun font ()
  "The 8x8 font the consoles draw in: boot/boot-uefi-x64.lisp, 95 glyphs, each 8
   bytes -- the first 95 parenthesised groups of eight #xNN after the defvar."
  (let* ((s (with-open-file (in (merge-pathnames "boot/boot-uefi-x64.lisp" *root*))
              (let ((str (make-string (file-length in))))
                (subseq str 0 (read-sequence str in)))))
         (i (or (search "(defvar *font-8x8-qwords*" s) (error "mac68k-floppy: no font")))
         (glyphs nil))
    (loop while (< (length glyphs) 95)
          do (let ((open (or (position #\( s :start (1+ i)) (error "mac68k-floppy: the font ends early"))))
               (setq i open)
               (let ((k (1+ open)) (row nil))
                 (loop repeat 8
                       do (loop while (member (char s k) '(#\Space #\Tab #\Newline #\Return)) do (incf k))
                          (if (and (char-equal (char s k) #\#) (char-equal (char s (1+ k)) #\x)
                                   (digit-char-p (char s (+ k 2)) 16) (digit-char-p (char s (+ k 3)) 16))
                              (progn (push (parse-integer s :start (+ k 2) :end (+ k 4) :radix 16) row)
                                     (incf k 4))
                              (return)))
                 (loop while (member (char s k) '(#\Space #\Tab #\Newline #\Return)) do (incf k))
                 (when (and (= (length row) 8) (char= (char s k) #\)))
                   (push (nreverse row) glyphs)))))
    (coerce (nreverse glyphs) 'vector)))

(defun octets (&rest parts)
  "PARTS (byte sequences) concatenated."
  (let ((v (make-array (reduce #'+ parts :key #'length) :element-type '(unsigned-byte 8))) (at 0))
    (dolist (p parts v) (replace v p :start1 at) (incf at (length p)))))

(defun zeros (n) (make-array (max n 0) :element-type '(unsigned-byte 8) :initial-element 0))
(defun pad512 (n) (* 512 (ceiling n 512)))

(defun main (image disk &key hd (out +out+))
  (multiple-value-bind (lo lsyms) (assemble "mac68k-unlzma.s" :cpu "68020")
   (fs-apple:set-u32 lo (gethash "outaddr" lsyms) out)
   (let ((img (read-bytes image)))
    (when (> (length lo) +pay-off+)
      (error "mac68k-floppy: the loader is ~D bytes, over ~D" (length lo) +pay-off+))
    (when (> (+ out (length img)) +loader-addr+)
      (error "mac68k-floppy: the image runs into the loader"))
    (let* ((stream (cram:lzma-compress img :lc 2 :lp 1 :pb 2 :dict-size (ash 1 23)))
           (sp (mac68k-disk::splash))      ; write-disk puts the splash after the data
           (room (- +floppy+ (if sp (pad512 (length sp)) 0)))
           (cap1 (- room 1024))
           (jumper (jumper out)))
      (flet ((blob (code)
               (let* ((head (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
                      (b nil))
                 (fs-apple:set-u16 head 0 (ash (length img) -16))
                 (fs-apple:set-u16 head 2 (logand (length img) #xFFFF))
                 (fs-apple:set-u16 head 6 (+ 8 (length jumper) (length code)))
                 (setq b (octets lo (zeros (- +pay-off+ (length lo))) head jumper code stream))
                 (when (> (+ +loader-addr+ (length b)) +state+)
                   (error "mac68k-floppy: loader + payload run into the coder state"))
                 b)))
        (let ((single (blob (zeros 0))))
          (if (or (<= (+ 1024 (length single)) room) hd)
              (let* ((fits (<= (+ 1024 (length single)) room))
                     (n (mac68k-disk:write-disk single disk :load-addr +loader-addr+
                                                            :min-size (if fits +floppy+ +hd+))))
                (when (> n +hd+)
                  (error "mac68k-floppy: ~D bytes do not fit a 1.44 MB floppy" n))
                (format t "mac68k-floppy: ~A, ~D bytes -> ~D LZMA + ~D loader -> ~A, ~D bytes (~A)~%"
                        image (length img) (length stream) (length lo) disk n
                        (if (= n +floppy+) "800K floppy" "1.44 MB floppy")))
              (multiple-value-bind (code syms) (assemble "mac68k-disk2.s")
                (let* ((b (blob code))
                       (part2 (subseq b cap1))
                       (len2 (pad512 (length part2))))
                  (when (> (+ 512 len2) +floppy+)
                    (error "mac68k-floppy: ~D bytes do not fit two 800 KB floppies" (+ 1024 (length b))))
                  (let ((setid (let ((c (cram:crc32 stream))) (if (zerop c) 1 c)))
                        (glyphs (font)))
                    (flet ((patch (name v) (fs-apple:set-u32 code (gethash name syms) v)))
                      (patch "loader" +loader-addr+)
                      (patch "dest" (+ +loader-addr+ cap1))
                      (patch "len" len2)
                      (patch "setid" setid))
                    (fs-apple:set-u16 code (gethash "msgn" syms) (length *message*))
                    (loop for ch across *message* for at from (gethash "msgbuf" syms) by 8
                          do (replace code (aref glyphs (- (char-code ch) 32)) :start1 at))
                    (let* ((b (blob code))            ; the same length, now patched
                           (part1 (subseq b 0 cap1)) (part2 (subseq b cap1))
                           (s2 (+ +loader-addr+ +pay-off+ 8 (length jumper)))
                           (d1 (concatenate 'string disk "-1")) (d2 (concatenate 'string disk "-2"))
                           (label (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
                      (mac68k-disk:write-disk part1 d1 :load-addr +loader-addr+ :min-size +floppy+ :entry s2)
                      (replace label (map 'vector #'char-code "MODUS-D2"))
                      (fs-apple:set-u32 label 8 setid)
                      (fs-apple:set-u32 label 12 (length part2))
                      (write-bytes d2 (pad-to (octets label part2) 1 +floppy+))
                      (format t "mac68k-floppy: ~A, ~D bytes -> ~D LZMA + ~D loader -> ~A (~D bytes) + ~A (~D bytes), two 800K floppies, ~D bytes to spare on the second~%"
                              image (length img) (length stream) (length lo) d1 (length part1) d2
                              (length part2) (- +floppy+ 512 len2)))))))))))))
(let* ((args (argv)) (hd (member "--hd" args :test #'string=))
       (k (position "--load" args :test #'string=))
       (out (if k (parse-integer (nth (1+ k) args) :radix 16) +out+)))
  (when k (setq args (append (subseq args 0 k) (subseq args (+ k 2)))))
  (setq args (remove "--hd" args :test #'string=))
  (main (first args) (second args) :hd (and hd t) :out out))
