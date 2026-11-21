;;;; mcl-join-cpt.lisp MCL1.dsk MCL2.dsk MCL3.dsk OUT.cpt -- join the three-segment
;;;; Compact Pro archive "MCL 2.0.sea" (floppies 1-3 of the MCL 2.0 set) into one
;;;; single-volume archive that unar can extract.
;;;;
;;;; Each segment starts with an 8-byte header (01 <vol> xx xx <dir offset>); only
;;;; the last segment's header points at the directory, and the directory's
;;;; per-file offsets are (volume number, offset within that volume's segment).  So:
;;;; keep segment 1 whole, append segments 2 and 3 without their 8-byte headers,
;;;; rebase every file offset into the joined stream, mark everything volume 1, and
;;;; recompute the directory CRC the way Compact Pro stores it (CRC-32 without the
;;;; final inversion: cram's CRC32-UPDATE register from #xFFFFFFFF).  The floppies
;;;; are read with fs-apple's HFS-READ.  It replaced mcl-join-cpt.py (machfs),
;;;; byte for byte.
(require :asdf)
(defvar *workspace*
  (let ((w (sb-ext:posix-getenv "WORKSPACE")))
    (if (and w (plusp (length w)))
        (pathname (if (char= (char w (1- (length w))) #\/) w (concatenate 'string w "/")))
        (merge-pathnames "../../../" (make-pathname :name nil :type nil :defaults *load-truename*)))))
(push (lambda (name)
        (let ((base (subseq name 0 (or (position #\/ name) (length name)))))
          (probe-file (merge-pathnames (format nil "~A/~A.asd" base base) *workspace*))))
      asdf:*system-definition-search-functions*)
(let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
  (asdf:load-system "fs-apple"))

(defun octets (path)
  (with-open-file (i (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence v i) v)))

(destructuring-bind (d1 d2 d3 out) (rest sb-ext:*posix-argv*)
  (let* ((parts (loop for d in (list d1 d2 d3)
                      for name in '("MCL 2.0.sea" "MCL 2.0.sea.#2" "MCL 2.0.sea.#3")
                      collect (fs-apple:hfs-file-data (fs-apple:hfs-ref (fs-apple:hfs-read (octets d)) name))))
         (l0 (length (first parts))) (l1 (length (second parts)))
         (a (concatenate '(vector (unsigned-byte 8)) (first parts) (subseq (second parts) 8) (subseq (third parts) 8))))
    (flet ((g (vol off) (cond ((= vol 1) off) ((= vol 2) (+ l0 (- off 8))) (t (+ l0 (- l1 8) (- off 8))))))
      (let* ((diroff (g 3 (fs-apple:u32-ref (third parts) 4)))
             (p (+ diroff 4))
             (nent (fs-apple:u16-ref a p)))
        (setf (aref a 1) 1)
        (fs-apple:set-u32 a 4 diroff)
        (incf p 2)
        (incf p (1+ (aref a p)))                       ; the archive comment
        (let ((done 0))
          (loop while (< done nent)
                do (let ((nl (aref a p)))
                     (incf p)
                     (if (logtest nl #x80)              ; a folder entry
                         (progn (incf p (+ (logand nl #x7F) 2)) (incf done))
                         (progn
                           (incf p nl)
                           (let ((vol (aref a p)) (off (fs-apple:u32-ref a (1+ p))))
                             (setf (aref a p) 1)
                             (fs-apple:set-u32 a (1+ p) (g vol off)))
                           (incf p 45) (incf done))))))
        (fs-apple:set-u32 a diroff (cram:crc32-update #xFFFFFFFF a (+ diroff 4)))
        (with-open-file (o (sb-ext:parse-native-namestring out) :direction :output
                                                                :element-type '(unsigned-byte 8) :if-exists :supersede)
          (write-sequence a o))
        (format t "~A: ~D bytes, ~D directory entries~%" out (length a) nent)))))
