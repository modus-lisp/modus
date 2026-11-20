;;;; mac68k-floppy.lisp -- compressed Mac boot disks.
;;;;
;;;;   sbcl --script scripts/mac68k-floppy.lisp IMAGE DISK [--hd] [--load HEX]
;;;;   sbcl --script scripts/mac68k-floppy.lisp CORE-DISK DISK --core [--one]
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
;;;; THE CORE FLOPPIES (--core; boot-mac68k :FLOPPY): a 68020+ Mac with 8 MB, the
;;;; REPL from a boot floppy and a FAT12 floppy (or one disk, --one) -- ~60 s on a 68040 (MAME's Quadra 950), ~131 s
;;;; on a 16 MHz 68030 (the SE/30).  The image is all native code, 3.9 MB of it
;;;; resident and 5.6 MB COLD (read at a function's first call); the floppy holds
;;;; the core disk's boot read as one LZMA stream and its tail as blocks a driver
;;;; unpacks on demand (MAIN-CORE below; scripts/mac68k-lzdisk.s).  Build:
;;;;   MODUS_MAC68K_MACHINE=floppy MODUS_68K_COLD_KEEP=mvm/68k-keep-quadra.txt
;;;;     MODUS_HOT_ORDER=mvm/68k-hotorder-floppy.txt (a session's cold code in the
;;;;     order it loads, so it shares few blocks) FS_FAT=fs-fat
;;;;     MODUS_68K_THREADED=0 MODUS_68K_PAGED=0 (and mac68k-snap-build.sh's other
;;;;     quadra knobs) sbcl ... mvm/build-mac68k-cl-repl.lisp
;;;;   MODUS_MAC68K_MACHINE=floppy scripts/mac68k-core.py IMAGE CORE-DISK
;;;;   MODUS_MAC68K_MACHINE=floppy sbcl --script scripts/mac68k-floppy.lisp CORE-DISK DISK --core
;;;; which writes DISK-1, the boot floppy, and DISK-2, a FAT12 floppy with the cold
;;;; code as MODUS/COLD.BIN and ~600 KB free for the user's code and files (--one:
;;;; both on one floppy, DISK, no swap and nothing free).
;;;; Needs cram with preset dictionaries (LZMA-PREPARE-DICT).  Less RAM than the
;;;; map needs stops in the boot blocks with _SysError 25 (a Sad Mac, 0000000F
;;;; 00000019 on an SE/30).
;;;;
;;;; --load HEX (a saved core as MAC68K_CORE_RAW writes it, everything resident)
;;;; is the 32 MB floppy this replaced: ~105 s on the Quadra 950.
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
  "At +PAY+ + 8: MOVEQ #1,D0; _HWPriv (FlushInstructionCache, which on a 68040
   pushes the data cache too -- the image was written as data); MOVEA.L
   #OUT,A0; JMP (A0).  (It was CPUSHA BC, a 68040 instruction: on a 68020 or
   68030 an F-line trap.)"
  (let ((v (make-array 12 :element-type '(unsigned-byte 8))))
    (fs-apple:set-u16 v 0 #x7001) (fs-apple:set-u16 v 2 #xA198)
    (fs-apple:set-u16 v 4 #x207C) (fs-apple:set-u32 v 6 out)
    (fs-apple:set-u16 v 10 #x4ED0)
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
   (fs-apple:set-u32 lo (gethash "stateaddr" lsyms) +state+)
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
;;; ---- THE CORE FLOPPIES (--core): 8 MB, cold code packed on a second floppy ------
;;;
;;; IMAGE is a core disk (mac68k-core.py's c.dsk) of a map whose cold native code
;;; stays on the disk (boot-mac68k :FLOPPY, MODUS_68K_COLD_KEEP).  Two floppies:
;;;   DISK-1, the boot floppy: the boot blocks, reading the BLOB to :BLOB --
;;;     the loader (mac68k-unlzma.s) | the payload: header, jumper, the DRIVER
;;;     (mac68k-lzdisk.s), the core disk's boot read as one LZMA stream
;;;   DISK-2, a FAT12 floppy (fs-fat) whose file MODUS/COLD.BIN is a label
;;;     sector ("MODUS-D2", the set's id) then the core disk's TAIL (from the boot
;;;     read's end: cold code, cold names) as 64 KB blocks, each an LZMA stream
;;;     against the resident code (the boot read's [LOAD, the resident functions'
;;;     end)); the rest of the disk is free, for code and files the user adds.
;;; The loader has the driver FIND the label in any drive (asking for DISK-2 on a
;;; Mac with one drive) before it unpacks; the image then reads its disk through
;;; the driver, which unpacks the block a read needs.  --one puts the label and
;;; the blocks on the boot floppy after the blob instead: one disk, no swap, the
;;; free space gone.
(defparameter *core-maps*
  '((:floppy :blob #x00580000 :driver #x007B0000 :state #x007E0000 :memneed #x007E8000
     :ncache 2))
  "Per map: where the boot blocks put the blob; the driver's address (its
   buffers follow it); the loader's state; the RAM the map needs (MemTop),
   past the driver's buffers; and how many unpacked blocks the driver keeps
   (mac68k-lzdisk.s NCACHE, 64 KB each).  boot-mac68k.lisp's :FLOPPY says why.")
(defconstant +block+ 65536 "mac68k-lzdisk.s BLOCK")

(defun cold-floppy (cold)
  "A FAT12 1.44 MB floppy holding COLD as MODUS/COLD.BIN, contiguous: (values
   its bytes, COLD's first byte on it)."
  (load-fs-fat)
  (flet ((f (name) (find-symbol name "FS-FAT")))
    (let* ((dev (funcall (f "MAKE-FAT32-DEVICE") (make-array +hd+ :initial-element 0)))
           (v (funcall (f "FAT-FORMAT-FLOPPY") dev :label "MODUS")))
      (funcall (f "FAT32-MKDIR") v "/MODUS")
      (funcall (f "FAT32-WRITE-FILE") v "/MODUS/COLD.BIN" cold)
      (let* ((first (fourth (funcall (f "FAT32-LOOKUP") v "/MODUS/COLD.BIN")))
             (chain (funcall (f "%FAT-CHAIN") v first)))
        ;; the driver reads it by byte offset: one run of clusters
        (loop for (a b) on chain while b
              unless (= b (1+ a)) do (error "mac68k-floppy: MODUS/COLD.BIN is not contiguous"))
        (values (coerce (funcall (f "DEVICE-READ") dev 0 +hd+) '(simple-array (unsigned-byte 8) (*)))
                (funcall (f "%FAT-CLUSTER-OFFSET") v first))))))

(defun main-core (image disk &key one)
  (let* ((m (or (cdr (assoc (intern (string-upcase (env "MODUS_MAC68K_MACHINE" "floppy")) "KEYWORD")
                            *core-maps*))
                (error "mac68k-floppy --core: no core map for MODUS_MAC68K_MACHINE")))
         (d (read-bytes image))
         ;; the boot blocks' record (mac68k-bootblocks.s): "modusCHN", then DEST
         ;; ENTRY LEN OFF ... -- by its marker, not by the symbols of boot blocks
         ;; assembled now, which may not be the ones the disk was written with
         (chn (or (search (map 'vector #'char-code "modusCHN") d :end2 1024)
                  (error "mac68k-floppy --core: ~A has no modusCHN record" image)))
         (load (fs-apple:u32-ref d (+ chn 8)))
         (len (fs-apple:u32-ref d (+ chn 16)))
         (off (fs-apple:u32-ref d (+ chn 20)))
         (core (subseq d off (+ off len)))
         (vbase (+ off len))
         (end (let ((e (length d))) (loop while (and (> e vbase) (zerop (aref d (1- e)))) do (decf e)) e))
         (nblk (ceiling (- end vbase) +block+))
         (tail (let ((v (zeros (* nblk +block+)))) (replace v d :start2 vbase :end2 end) v))
         (cld (or (search (map 'vector #'char-code "modusCLD") core)
                  (error "mac68k-floppy --core: no cold code descriptor (modusCLD) in ~A" image)))
         ;; the descriptor (translate-68k M68K-EMIT-COLD-NATIVE-RESIDENT): BIAS,
         ;; the section, the count, the index, the first stub, then the resident
         ;; FUNCTIONS' end -- the shared routines after them keep state inline
         ;; (measured: the console's, 13 bytes in a session), so the dictionary,
         ;; which must never change, stops there
         (dictend (fs-apple:u32-ref core (+ cld 8 20)))
         (dict (subseq core 0 (- dictend load)))
         (prep (cram:lzma-prepare-dict dict))
         (blocks (loop for i below nblk
                       collect (cram:lzma-compress (subseq tail (* i +block+) (* (1+ i) +block+))
                                                   :dict prep :lc 2 :lp 1 :pb 2 :end-marker nil
                                                   :dict-size (ash 1 23))))
         (stream (cram:lzma-compress core :lc 2 :lp 1 :pb 2 :end-marker nil :dict-size (ash 1 23)))
         (setid (let ((c (cram:crc32 stream))) (if (zerop c) 1 c)))
         (label (let ((l (zeros 512)))
                  (replace l (map 'vector #'char-code "MODUS-D2"))
                  (fs-apple:set-u32 l 8 setid)
                  (fs-apple:set-u32 l 12 nblk)
                  l))
         (cold (apply #'octets label blocks))
         (jumper (jumper load)))
    (when (> nblk 256) (error "mac68k-floppy --core: ~D blocks, over the driver's 256" nblk))
    (multiple-value-bind (dv dsyms) (assemble "mac68k-lzdisk.s" :cpu "68020"
                                              :defsyms (list (cons "NCACHE" (getf m :ncache 1))))
      (multiple-value-bind (lo lsyms) (assemble "mac68k-unlzma.s" :cpu "68020")
        (let* ((dv (pad-to dv 4))
               (bufs-end (+ (getf m :driver) (gethash "bufs" dsyms) 32 16384 +block+ 2048
                           (* (getf m :ncache 1) +block+)))
               (head (zeros 8))
               (drvsrc (+ 8 (length jumper)))
               (blob-len (+ +pay-off+ drvsrc (length dv) (length stream)))
               (disk2 nil)
               (labeloff (if one
                             (+ 1024 (pad512 blob-len))
                             (multiple-value-bind (bytes at) (cold-floppy cold)
                               (setq disk2 bytes)
                               at)))
               (table (gethash "table" dsyms))
               (glyphs (font)))
          (when (> (length lo) +pay-off+) (error "mac68k-floppy: the loader is over ~D bytes" +pay-off+))
          (when (> (+ load len) (getf m :blob)) (error "mac68k-floppy --core: the core runs into the blob"))
          (when (> (+ (getf m :blob) blob-len) (getf m :driver))
            (error "mac68k-floppy --core: the blob runs into the driver"))
          (when (> bufs-end (getf m :memneed)) (error "mac68k-floppy --core: the driver's buffers end past MEMNEED"))
          (flet ((dset (name v) (fs-apple:set-u32 dv (gethash name dsyms) v))
                 (lset (name v) (fs-apple:set-u32 lo (gethash name lsyms) v)))
            (dset "vbase" vbase) (dset "nblk" nblk) (dset "dictend" dictend)
            (dset "labeloff" labeloff) (dset "setid" setid)
            (let ((at (+ labeloff 512)))
              (loop for b in blocks for i from 0
                    do (fs-apple:set-u32 dv (+ table (* 4 i)) at) (incf at (length b))
                    finally (fs-apple:set-u32 dv (+ table (* 4 nblk)) at)))
            (fs-apple:set-u16 dv (gethash "msgn" dsyms) (length *message*))
            (loop for ch across *message* for at from (gethash "msgbuf" dsyms) by 8
                  do (replace dv (aref glyphs (- (char-code ch) 32)) :start1 at))
            (lset "outaddr" load) (lset "stateaddr" (getf m :state)) (lset "memneed" (getf m :memneed))
            (lset "drvsrc" drvsrc) (lset "drvlen" (length dv)) (lset "drvaddr" (getf m :driver)))
          (fs-apple:set-u16 head 0 (ash len -16))
          (fs-apple:set-u16 head 2 (logand len #xFFFF))
          (fs-apple:set-u16 head 6 (+ drvsrc (length dv)))
          (let* ((blob (octets lo (zeros (- +pay-off+ (length lo))) head jumper dv stream))
                 (packed (if one (octets blob (zeros (- (pad512 blob-len) blob-len)) cold) blob))
                 (d1 (if one disk (concatenate 'string disk "-1")))
                 (n (mac68k-disk:write-disk packed d1 :load-addr (getf m :blob) :read-len (length blob)
                                                      :min-size +hd+ :memtop (getf m :memneed)))
                 (sp (mac68k-disk::splash)))
            (assert (= (length blob) blob-len))
            (when (> n +hd+)
              (error "mac68k-floppy --core: ~D bytes, over a 1.44 MB floppy by ~D" n (- n +hd+)))
            (when disk2 (write-bytes (concatenate 'string disk "-2") disk2))
            (format t "mac68k-floppy: ~A: the boot read ~D bytes -> ~D LZMA; the tail ~D bytes -> ~D blocks, ~D LZMA against ~D bytes of resident code~%"
                    image len (length stream) (- end vbase) nblk (reduce #'+ blocks :key #'length) (length dict))
            (if one
                (format t "mac68k-floppy: ~A, one 1.44 MB floppy, ~D bytes free~%"
                        disk (- +hd+ 1024 (length packed) (if sp (pad512 (length sp)) 0)))
                (format t "mac68k-floppy: ~A-1 (boot, ~D bytes) and ~A-2 (FAT12, MODUS/COLD.BIN ~D bytes at ~D; ~D KB free)~%"
                        disk (+ 1024 (length blob)) disk (length cold) labeloff
                        (floor (- (- +hd+ 16896) (* 512 (ceiling (length cold) 512)) 512) 1024)))))))))

(let* ((args (argv)) (hd (member "--hd" args :test #'string=))
       (k (position "--load" args :test #'string=))
       (out (if k (parse-integer (nth (1+ k) args) :radix 16) +out+)))
  (when k (setq args (append (subseq args 0 k) (subseq args (+ k 2)))))
  (setq args (remove "--hd" args :test #'string=))
  (if (member "--core" args :test #'string=)
      (let ((args (remove "--one" (remove "--core" args :test #'string=) :test #'string=)))
        (main-core (first args) (second args) :one (and (member "--one" (argv) :test #'string=) t)))
      (main (first args) (second args) :hd (and hd t) :out out)))
