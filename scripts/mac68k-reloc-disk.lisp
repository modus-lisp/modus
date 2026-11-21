;;;; mac68k-reloc-disk.lisp -- make a Mac disk relocatable.
;;;;
;;;;   sbcl --script scripts/mac68k-reloc-disk.lisp A.dsk B.dsk SHIFT SPAN
;;;;
;;;; A and B are the same Mac disk (mac68k-core's) built SHIFT apart (hex, a
;;;; multiple of 512 KB; mac68k-snap-build.sh SHIFT=).  The part its loader reads
;;;; -- the image, the static heap and the low block, LEN bytes at disk byte OFF --
;;;; is classified with mac68k-reloc's CLASSIFY, the table is appended to A, and
;;;; A's launch record ("modusCHN", mac68k-bootblocks.s) gets RELOFF, RELLEN and
;;;; SPAN (hex: the bytes of map from DEST the loader must allocate).
;;;;
;;;; The table, for mac68k-chain.s: per KIND (8, 16, 7), a u32 KIND, a u32 COUNT,
;;;; then COUNT entries, each the gap from the previous offset (from 0): a byte G
;;;; in 1..255 = 2*G bytes on, or a 0 byte then the offset as 4 big-endian bytes.
;;;; The loader adds ((shift >> 16) * KIND) / 8 to the 16-bit word there.
;;;;
;;;; COLD NATIVE CODE (translate-68k M68K-EMIT-COLD-ENTER): per cold body, its
;;;; words that differ between A and B by a relocation, as COLD-ENTER applies them
;;;; -- n8 n16 n7, then each kind's offsets / 2 (u16s), each the GAP from the one
;;;; before -- after the table, and the resident index (blocks << 25 | disk byte)
;;;; that finds each.
;;;;
;;;; Fails, saying where, on any difference that is not a relocation, except text
;;;; that records the build (both words printable): MAC68K_RELOC_ALLOW (default 4)
;;;; others in the image, MAC68K_RELOC_COLD_ALLOW (0) in cold bodies.  It replaced
;;;; mac68k-reloc-disk.py, byte for byte (2026-11-21).

(defvar cl-user::*mac68k-reloc-library* t)
(load (merge-pathnames "mac68k-reloc.lisp" *load-truename*))
(defpackage #:mac68k-reloc-disk (:use #:cl #:mac68k-tools #:mac68k-reloc))
(in-package #:mac68k-reloc-disk)

(defun die (fmt &rest args)
  (format *error-output* "~&mac68k-reloc-disk: ~?~%" fmt args)
  (sb-ext:exit :code 1 :abort t))

(defun u32 (v i) (fs-apple:u32-ref v i))
(defun octets (s) (map '(vector (unsigned-byte 8)) #'char-code s))
(defun bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun byte-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
(defun put-u8 (buf x) (vector-push-extend (logand x #xFF) buf))
(defun put-u16 (buf x) (put-u8 buf (ash x -8)) (put-u8 buf x))
(defun put-u32 (buf x) (put-u16 buf (ldb (byte 16 16) x)) (put-u16 buf (ldb (byte 16 0) x)))
(defun put-bytes (buf v) (loop for x across v do (vector-push-extend x buf)))

(defun record (d)
  "The launch record's fields: just after 'modusCHN' in the boot blocks."
  (let ((k (search (octets "modusCHN") d :end2 1024)))
    (unless k (die "no launch record in the boot blocks"))
    (+ k 8)))

(defun encode (offsets)
  (let ((out (byte-buffer)) (prev 0))
    (dolist (o offsets out)
      (let ((g (- o prev)))
        (if (and (< 0 g 511) (evenp g))
            (put-u8 out (/ g 2))
            (progn (put-u8 out 0) (put-u32 out o))))
      (setq prev o))))

(defun printable-p (w)
  (flet ((ok (c) (or (<= #x20 c #x7E) (member c '(9 10 13)))))
    (and (ok (ash w -8)) (ok (logand w #xFF)))))

(defun kind-offsets (kinds k) (cdr (assoc k kinds)))

(defun cold-lists (a b dest off ln shift)
  "(values the index's disk byte, the per-body lists), or NIL when the image
   has no cold code."
  (let ((k (search (octets "modusCLD") a :start2 off :end2 (+ off ln))))
    (when k
      (let ((bias (u32 a (+ k 8))) (count (u32 a (+ k 16)))
            (xindex (u32 a (+ k 20))) (stubs (u32 a (+ k 24)))
            (lists nil) (bad 0))
        (dotimes (i count)
          (let* ((st (- (+ off stubs (* 16 i)) dest))
                 (home (u32 a (+ st 6))) (tl (u32 a (+ st 10)))
                 (at (- home bias)))                 ; the body's disk byte (A's BIAS)
            (multiple-value-bind (kinds odd)
                (classify (subseq a at (+ at tl)) (subseq b at (+ at tl)) shift)
              (when odd
                (incf bad (length odd))
                (loop for (o wa wb) in odd repeat 4
                      do (format t "  cold ~D +~X: ~4,'0X -> ~4,'0X~%" i o wa wb)))
              (let ((lst (byte-buffer)))
                (dolist (kk '(8 16 7)) (put-u16 lst (length (kind-offsets kinds kk))))
                (dolist (kk '(8 16 7))
                  (let ((prev 0))
                    (dolist (o (kind-offsets kinds kk))  ; each as the GAP from the last
                      (when (>= (- o prev) (ash 1 17))
                        (die "cold body ~D has a 128 KB stretch with no ~D-bit relocation" i kk))
                      (put-u16 lst (/ (- o prev) 2))
                      (setq prev o))))
                (push (coerce lst '(simple-array (unsigned-byte 8) (*))) lists)))))
        (when (> bad (parse-integer (env "MAC68K_RELOC_COLD_ALLOW" "0")))
          (die "~D differences in cold bodies that are not relocations" bad))
        (values (- (+ off xindex) dest) (nreverse lists))))))

(defun main (a-path b-path shift-hex span-hex)
  (let* ((shift (parse-integer shift-hex :radix 16)) (span (parse-integer span-hex :radix 16))
         (a0 (read-bytes a-path)) (b (read-bytes b-path))
         (r (record a0))
         (dest (u32 a0 r)) (ln (u32 a0 (+ r 8))) (off (u32 a0 (+ r 12))))
    (unless (zerop (u32 a0 (+ r 32))) (die "~A already has a table" a-path))
    (multiple-value-bind (kinds odd)
        (classify (subseq a0 off (+ off ln)) (subseq b off (+ off ln)) shift)
      ;; text that records the build (its settings, the names it harvested) and
      ;; the build id may differ; anything else is an address the table misses
      (let ((bad (remove-if (lambda (e) (and (printable-p (second e)) (printable-p (third e)))) odd)))
        (format t "mac68k-reloc-disk: ~D bytes from 0x~(~x~): ~{~A~^, ~}; ~D text differences, ~D others~%"
                ln dest (mapcar (lambda (k) (format nil "~D x~D/8" (length (kind-offsets kinds k)) k)) +kinds+)
                (- (length odd) (length bad)) (length bad))
        (loop for (o wa wb) in bad repeat 40
              do (format t "  +~7,'0X (#~X)  ~4,'0X -> ~4,'0X~%" o (+ dest o) wa wb))
        (when (> (length bad) (parse-integer (env "MAC68K_RELOC_ALLOW" "4")))
          (die "too many differences that are not relocations")))
      (multiple-value-bind (xindex lists) (cold-lists a0 b dest off ln shift)
        (let ((a (byte-buffer)) (table (byte-buffer)))
          (put-bytes a a0)
          (dolist (k +kinds+)
            (put-u32 table k) (put-u32 table (length (kind-offsets kinds k)))
            (put-bytes table (encode (kind-offsets kinds k))))
          (put-bytes a (bytes (mod (- (length a)) 512)))
          (let ((at (length a)))
            (put-bytes a table) (put-bytes a (bytes (mod (- (length table)) 512)))
            (fs-apple:set-u32 a (+ r 32) at) (fs-apple:set-u32 a (+ r 36) (length table))
            (fs-apple:set-u32 a (+ r 40) span)
            (when xindex
              (let ((base (length a)) (blob (byte-buffer)) (seen (make-hash-table :test 'equalp)))
                (loop for lst in lists for i from 0
                      do (let ((at-i (or (gethash lst seen)
                                         (setf (gethash lst seen)
                                               (prog1 (+ base (length blob)) (put-bytes blob lst))))))
                           (let ((blocks (floor (+ (mod at-i 512) (length lst) 511) 512)))
                             (when (or (> blocks 127) (>= at-i (ash 1 25)))
                               (die "cold list ~D (~D bytes at ~D) out of reach" i (length lst) at-i))
                             (fs-apple:set-u32 a (+ xindex (* 4 i)) (logior (ash blocks 25) at-i)))))
                (put-bytes a blob) (put-bytes a (bytes (mod (- (length blob)) 512)))
                (format t "mac68k-reloc-disk: ~D cold bodies: ~D bytes of external relocations at disk byte ~D~%"
                        (length lists) (length blob) base)))
            (write-bytes a-path (coerce a '(simple-array (unsigned-byte 8) (*))))
            (format t "mac68k-reloc-disk: ~A: table ~D bytes at disk byte ~D, span 0x~(~x~)~%"
                    a-path (length table) at span)))))))

(apply #'main (argv))
