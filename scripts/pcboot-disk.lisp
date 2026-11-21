;;;; pcboot-disk.lisp -- write a modus disk that boots without DOS.
;;;;
;;;;   sbcl --script scripts/pcboot-disk.lisp OUT STAGE1 STAGE2 [--sectors N] NAME=PATH...
;;;;
;;;; (scripts/pcboot.sh assembles the stages and calls this.)  The layout, in
;;;; 512-byte sectors:
;;;;
;;;;   0      STAGE1, pcboot1.s's boot sector, its partition entry sized to
;;;;          the disk
;;;;   1      the file table: "MODUSFS1", then a 32-byte entry per file -- the
;;;;          NAME (16 bytes, NUL-padded, upper case), its first sector, its
;;;;          size -- and an empty entry
;;;;   2-9    STAGE2, pcboot2.s's shim (at most 4 KB)
;;;;   10..   each file, from a sector boundary, in the order given
;;;;
;;;; --sectors N pads the disk to N sectors (e.g. 163296: the 162 x 16 x 63
;;;; disk the 86Box matrix uses); without it the disk ends after the last file.
;;;; A disk the size of a USB stick is simply written to the stick.

(defun slurp (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun put32 (v at n)
  (dotimes (i 4) (setf (aref v (+ at i)) (ldb (byte 8 (* 8 i)) n))))

(defun sectors-of (bytes) (ceiling bytes 512))

(defun main (args)
  (destructuring-bind (out stage1 stage2 &rest rest) args
    (let ((pad nil) (files nil))
      (loop while rest
            do (let ((a (pop rest)))
                 (if (string= a "--sectors")
                     (setq pad (parse-integer (pop rest)))
                     (let ((eq (position #\= a)))
                       (unless eq (error "pcboot-disk: ~A is not NAME=PATH" a))
                       (push (cons (string-upcase (subseq a 0 eq)) (subseq a (1+ eq))) files)))))
      (setq files (nreverse files))
      (let* ((s1 (slurp stage1))
             (s2 (slurp stage2))
             (data (mapcar (lambda (f) (cons (car f) (slurp (cdr f)))) files))
             (next 10)
             (placed nil))
        (assert (= (length s1) 512))
        (assert (and (= (aref s1 510) #x55) (= (aref s1 511) #xAA)))
        (when (> (length s2) (* 8 512))
          (error "pcboot-disk: the shim is ~D bytes, more than 4 KB" (length s2)))
        (when (> (length data) 15)
          (error "pcboot-disk: at most 15 files"))
        (dolist (d data)
          (when (> (length (car d)) 15)
            (error "pcboot-disk: ~A: names are at most 15 characters" (car d)))
          (push (list (car d) next (cdr d)) placed)
          (incf next (sectors-of (length (cdr d)))))
        (setq placed (nreverse placed))
        (let* ((total (max next (or pad 0)))
               (disk (make-array (* 512 total) :element-type '(unsigned-byte 8)
                                                :initial-element 0)))
          (when (and pad (> next pad))
            (error "pcboot-disk: the files need ~D sectors, more than --sectors ~D" next pad))
          (replace disk s1)
          (put32 disk (+ #x1BE 12) (1- total))      ; the partition's size
          (let ((tb 512))                           ; the table
            (replace disk (map 'vector #'char-code "MODUSFS1") :start1 tb)
            (let ((e (+ tb 8)))
              (dolist (p placed)
                (destructuring-bind (name lba bytes) p
                  (replace disk (map 'vector #'char-code name) :start1 e)
                  (put32 disk (+ e 16) lba)
                  (put32 disk (+ e 20) (length bytes))
                  (incf e 32)))))
          (replace disk s2 :start1 (* 2 512))
          (dolist (p placed)
            (replace disk (third p) :start1 (* 512 (second p))))
          (with-open-file (o out :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
            (write-sequence disk o))
          (format t "~A: ~D sectors (~,1F MB)~%" out total (/ (* 512 total) 1048576.0))
          (dolist (p placed)
            (format t "  ~16A sector ~8D  ~10D bytes~%" (first p) (second p) (length (third p)))))))))

(main (cdr sb-ext:*posix-argv*))
