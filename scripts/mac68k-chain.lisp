;;;; mac68k-chain.lisp -- modus as a Mac application.
;;;;
;;;;   sbcl --script scripts/mac68k-chain.lisp DISK OUT.bin [--forks DIR]
;;;;
;;;; DISK is a modus boot disk that can run on top of Mac OS: mac68k-core's output
;;;; for MODUS_MAC68K_MACHINE=classicos or quadra (its boot blocks' record names a
;;;; REFSLOT).  OUT.bin is a MacBinary II file -- the usual way to carry a Mac file,
;;;; both forks, through anything that is not a Mac -- of an application, "Modus":
;;;;
;;;;   - its resource fork: CODE 0 (the jump table), CODE 1 (mac68k-chain.s, in
;;;;     vessel's kernel/scripts/, which starts modus), SIZE -1, and its icon,
;;;;     "(m)", with the bundle the Finder finds it by (FINDER-RESOURCES);
;;;;   - its data fork: DISK, byte for byte.
;;;;
;;;; Double-clicked on the Mac, it reads the image out of its own data fork to the
;;;; image's address and jumps to it, leaving Mac OS underneath, and the image
;;;; pages its code from the same data fork.  One file to copy; nothing is written
;;;; to the Mac's disk.  --forks DIR also writes DIR/data and DIR/rsrc, for tools
;;;; that build an HFS volume.  The resource fork and MacBinary are fs-apple's.
;;;;
;;;; MAC68K_CHAIN_DEFS="SYM=VAL ...": assembler symbols for mac68k-chain.s
;;;; (JITSHIFT / JITMAX: the code area's share of the room and its cap).
;;;; MODUS_PARTITION_KB / MODUS_PARTITION_FORCE: see SIZE-RSRC.  It replaced
;;;; mac68k-chain.py, byte for byte (2026-11-21).

(load (merge-pathnames "mac68k-tools.lisp" *load-truename*))
(defpackage #:mac68k-chain (:use #:cl #:mac68k-tools))
(in-package #:mac68k-chain)

(defparameter *name* "Modus")
(defparameter *type* "APPL")
(defparameter *creator* "MDUS")

(defun die (fmt &rest args)
  (format *error-output* "~&mac68k-chain: ~?~%" fmt args)
  (sb-ext:exit :code 1 :abort t))

(defun octets (&rest parts)
  "PARTS -- octet vectors, strings (as ASCII) and integers (bytes) -- as one octet vector."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (p parts (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase p
        (integer (vector-push-extend p out))
        (string (loop for c across p do (vector-push-extend (char-code c) out)))
        (vector (loop for x across p do (vector-push-extend x out)))))))

(defun be (n width)
  "N as WIDTH big-endian bytes (two's complement when negative)."
  (let ((v (make-array width :element-type '(unsigned-byte 8))))
    (dotimes (i width v) (setf (aref v i) (ldb (byte 8 (* 8 (- width 1 i))) n)))))

;;; The icon: a diamond with "(m)" in it and a hand writing below, a pen and a
;;; cuff.  # is black, o white inside the mask (the diamond is filled, not
;;; transparent on the desktop), . outside.  The small one is the diamond and
;;; its "(m)".
(defparameter *icon*
  '("...............##..............."
    "..............#oo#.............."
    ".............#oooo#............."
    "............#oooooo#............"
    "...........#oooooooo#..........."
    "..........#oooooooooo#.........."
    ".........#oooooooooooo#........."
    "........#o#oooooooooo#o#........"
    ".......#o##oooooooooo##o#......."
    "......#o##oooooooooooo##o#......"
    ".....#oo##oo######oooo##oo#....."
    "....#ooo##oo##o##o##oo##ooo#...."
    "...#oooo##oo##o##o##oo##oooo#..."
    "..#ooooo##oo##o##o##oo##ooooo#.."
    ".#oooooo##oo##o##o##oo##oooooo#."
    "#oooooooo##oooooooooo##oooooooo#"
    "#ooooooooo#oooooooooo#ooooooooo#"
    ".#ooooooooooooooooo#oooooooooo#."
    "..#ooooooooooooooo##ooooooooo#.."
    "...#ooooooooooooo##ooooooooo#..."
    "....#ooooooooooo##oo########...."
    ".....#ooooo######ooo#o#o#o#....."
    "......#ooo#ooo########o#o#......"
    ".......#o#ooo#oooooo#o#o#......."
    "........##oo#ooooooo##o#........"
    ".........#o###oooooo#o#........."
    "..........############.........."
    "...........#oooooooo#..........."
    "............#oooooo#............"
    ".............#oooo#............."
    "..............#oo#.............."
    "...............##..............."))

(defparameter *small-icon*
  '(".......##......."
    "......#oo#......"
    ".....#oooo#....."
    "....#oooooo#...."
    "...#oooooooo#..."
    "..#o#ooooo#oo#.."
    ".#o#o####oo#oo#."
    "#oo#o#o#o#o#ooo#"
    "#oo#o#o#o#o#ooo#"
    ".#o#o#o#o#o#oo#."
    "..#o#ooooo#oo#.."
    "...#oooooooo#..."
    "....#oooooo#...."
    ".....#oooo#....."
    "......#oo#......"
    ".......##......."))

(defun icon-list (rows)
  "ICN# / ics# data: the bitmap (#), then the mask (# and o)."
  (flet ((bits (on)
           (apply #'octets
                  (loop for r in rows
                        append (let ((v 0))
                                 (loop for c across r do (setq v (+ (* v 2) (if (find c on) 1 0))))
                                 (coerce (be v (floor (length r) 8)) 'list))))))
    (octets (bits "#") (bits "#o"))))

(defun finder-resources ()
  "What the Finder shows the application by: its icons (ICN# and ics# 128),
   FREF 128 (an APPL, local icon 0), BNDL 128 (signature MDUS, ICN# and ics#
   local 0 -> 128, FREF local 0 -> 128) and the signature's owner resource,
   MDUS 0.  The file needs the hasBundle and hasCustomIcon flags (period-lisps'
   volume builder sets them)."
  (let ((fref (octets "APPL" (be 0 2) 0))
        (bndl (apply #'octets *creator* (be 0 2) (be 2 2)
                     (loop for ty in '("ICN#" "ics#" "FREF")
                           append (list ty (be 0 2) (be 0 2) (be 128 2))))))
    ;; the same icons as the file's CUSTOM icon (-16455, with the file's
    ;; hasCustomIcon flag), which the Finder draws straight from the resource
    ;; fork: a volume's Desktop DB may never register the bundle
    (list (list "ICN#" 128 (icon-list *icon*)) (list "ics#" 128 (icon-list *small-icon*))
          (list "ICN#" -16455 (icon-list *icon*)) (list "ics#" -16455 (icon-list *small-icon*))
          (list "FREF" 128 fref) (list "BNDL" 128 bndl)
          (list *creator* 0 (octets (length *name*) *name*)))))

(defun code1 ()
  "mac68k-chain.s behind its segment header: the first jump-table entry it owns
   (offset 0), one entry."
  (let ((defs (loop for d in (let ((s (env "MAC68K_CHAIN_DEFS" "")))
                               (loop with words = nil and start = nil
                                     for i from 0 to (length s)
                                     for c = (if (< i (length s)) (char s i) #\Space)
                                     do (cond ((and (char= c #\Space) start) (push (subseq s start i) words) (setq start nil))
                                              ((and (char/= c #\Space) (null start)) (setq start i)))
                                     finally (return (nreverse words))))
                    collect (let ((eq (position #\= d))) (cons (subseq d 0 eq) (subseq d (1+ eq)))))))
    (octets (be 0 2) (be 1 2) (assemble "mac68k-chain.s" :defsyms defs))))

(defun code0 ()
  "The jump table: one entry, CODE 1 at offset 0 (from the code after the
   segment's header), unloaded -- MOVE.W #1,-(SP); _LoadSeg.  Above A5: 32
   bytes of application parameters, then the table.  Below A5: the QuickDraw
   globals InitGraf is given (-4(A5) is thePort)."
  (let ((entry (octets (be 0 2) (be #x3F3C 2) (be 1 2) (be #xA9F0 2))))
    (octets (be (+ 32 (length entry)) 4) (be 512 4) (be (length entry) 4) (be 32 4) entry)))

(defun size-rsrc (memtop &optional (span 0) (kmap 0))
  "SIZE -1, for MultiFinder / System 7: flags, then the preferred and minimum
   partition.  A map for a 4 MB Mac asks 64 KB (System 6 without MultiFinder
   ignores SIZE, and the image lives above the application zone).  A bigger,
   unrelocated map's image sits at fixed addresses in the middle of RAM, where
   System 7's Process Manager would otherwise put the Finder's partition and its
   own heap (measured, a 32 MB Quadra 950 under 7.5.3), so it asks for all of RAM.
   A RELOCATED image (mac68k-chain.s's relocating load) goes wherever the
   partition is: it asks for SPAN, plus the heap (the relocation table, then
   the Toolbox's), the 512 KB alignment of the image's base and its own stack --
   measured, SPAN + 576 KB runs every benchmark form, SPAN + 320 KB is refused by
   the loader's own check.  A map whose young heap is last (KMAP) takes its
   semispaces from whatever partition it gets, so it prefers 4 MB more (or
   MODUS_PARTITION_KB); MODUS_PARTITION_FORCE=1 with MODUS_PARTITION_KB makes
   that the partition, to test the floor."
  (cond ((plusp span)
         (let* ((need (+ span #x90000))
                (pref need))
           (when (plusp kmap)
             (let ((kb (env "MODUS_PARTITION_KB")))
               (setq pref (if kb (max need (* 1024 (parse-integer kb))) (+ need #x400000)))))
           (when (and (equal (env "MODUS_PARTITION_FORCE") "1") (env "MODUS_PARTITION_KB"))
             (setq pref (* 1024 (parse-integer (env "MODUS_PARTITION_KB"))) need pref))
           (octets (be #x0080 2) (be pref 4) (be need 4))))
        ((<= memtop (ash 4 20)) (octets (be 0 2) (be (* 64 1024) 4) (be (* 64 1024) 4)))
        ;; is32BitCompatible
        (t (octets (be #x0080 2) (be memtop 4) (be (ash 20 20) 4)))))

(defun main (disk out &optional forks)
  (let* ((data (read-bytes disk))
         (k (search (octets "modusCHN") data :end2 1024)))
    (unless k (die "~A has no launch record in its boot blocks" disk))
    (flet ((field (i) (fs-apple:u32-ref data (+ k 8 (* 4 i)))))
      ;; dest entry len off - - refslot memtop reloff - span kmap
      (let ((dest (field 0)) (ln (field 2)) (refslot (field 6)) (memtop (field 7))
            (reloff (field 8)) (span (field 10)) (kmap (field 11)))
        (when (zerop refslot)
          (die "~A cannot run on top of Mac OS (build it with MODUS_MAC68K_MACHINE=classicos)" disk))
        (let ((rsrc (fs-apple:resource-fork-write
                     (append (list (list "CODE" 0 (code0)) (list "CODE" 1 (code1))
                                   (list "SIZE" -1 (if (plusp reloff) (size-rsrc memtop span kmap) (size-rsrc memtop))))
                             (finder-resources)))))
          (write-bytes out (fs-apple:macbinary-write *name* *type* *creator* data rsrc))
          (when forks
            (ensure-directories-exist (concatenate 'string forks "/"))
            (write-bytes (concatenate 'string forks "/data") data)
            (write-bytes (concatenate 'string forks "/rsrc") rsrc))
          (format t "mac68k-chain: ~A: \"~A\", data fork ~D bytes (reads ~D at #~(~x~), needs ~D KB of RAM), resource fork ~D bytes~%"
                  out *name* (length data) ln dest (floor memtop 1024) (length rsrc)))))))

(let* ((args (argv)) (p (position "--forks" args :test #'string=)) (forks nil))
  (when p (setq forks (nth (1+ p) args) args (append (subseq args 0 p) (nthcdr (+ p 2) args))))
  (unless (= (length args) 2) (die "usage: mac68k-chain.lisp DISK OUT.bin [--forks DIR]"))
  (main (first args) (second args) forks))
