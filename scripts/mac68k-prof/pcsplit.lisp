;;;; pcsplit.lisp PROFDIR IMAGE.bin [LOAD] -- per phase: where the PC was (ROM /
;;;; image / other RAM), from PROFDIR/samples.txt ("PHASE PC ..." lines).  It
;;;; replaced pcsplit.py (2026-11-21).
(load (merge-pathnames "prof-common.lisp" *load-truename*))
(in-package #:mac68k-prof)
(destructuring-bind (dir img &optional (load "0x40000")) (argv)
  (let ((load (num load)) (n (with-open-file (i img :element-type '(unsigned-byte 8)) (file-length i)))
        (phases (make-hash-table)))
    (dolist (l (lines (format nil "~A/samples.txt" dir)))
      (let ((q (words l)))
        (when (>= (length q) 4)
          (let ((ph (parse-integer (first q))) (pc (parse-integer (second q) :radix 16)))
            (bump (or (gethash ph phases) (setf (gethash ph phases) (make-counter)))
                  (cond ((<= #x400000 pc #x4FFFFF) "ROM")
                        ((and (<= load pc) (< pc (+ load n))) "image")
                        (t "other RAM")))))))
    (dolist (ph (sort (loop for k being the hash-keys of phases collect k) #'<))
      (let* ((c (gethash ph phases)) (tot (total c)))
        (format t "phase ~2D: ~5D samples  ~{~A~^  ~}~%" ph tot
                (mapcar (lambda (k) (format nil "~A ~4,1F%" k (pct (count-of c k) tot)))
                        (sort (copy-list (counter-order c)) #'string<)))))))
