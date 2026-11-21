;;;; tree-native.lisp RUNDIR [T0 T1] -- lisp.lua PROF samples (RUNDIR/prof.txt,
;;;; symbol map RUNDIR/cl.map, linked at #x600000) as a call tree from MVM-EVAL
;;;; down, and self time.  Without T0/T1: the span where the PC is in the image.
;;;; Samples inside the REPL's reader (MAME still typing) are dropped.  It
;;;; replaced tree.native.py (2026-11-21).
(load (merge-pathnames "prof-common.lisp" *load-truename*))
(in-package #:mac68k-prof)
(destructuring-bind (dir &optional t0 t1) (argv)
  (let* ((rd (lambda (s) (let ((*read-default-float-format* 'double-float)) (float (read-from-string s) 1d0))))
         (rows (read-map (format nil "~A/cl.map" (string-right-trim "/" dir))))
         (lo (first (aref rows 0))) (hi (let ((r (aref rows (1- (length rows))))) (+ (first r) (second r))))
         (d 0) (samples nil) (n 0) (selfc (make-counter)) (tree (list :root)))
    (dolist (l (lines (format nil "~A/prof.txt" (string-right-trim "/" dir))))
      (let ((q (words l)))
        (if (string= (first q) "BASE")
            (setq d (- (hex (second q)) #x600000))
            (push (cons (funcall rd (first q)) (loop for x in (rest q) unless (find #\= x) collect (- (hex x) d)))
                  samples))))
    (setq samples (nreverse samples))
    (if t1
        (setq t0 (funcall rd t0) t1 (funcall rd t1))
        (let ((busy (loop for (tm . a) in samples when (and (<= lo (first a)) (< (first a) hi)) collect tm)))
          (setq t0 (reduce #'min busy) t1 (reduce #'max busy))))
    (loop for (tm . addrs) in samples
          when (<= t0 tm t1)
            do (let* ((fs (mapcar (lambda (a) (map-name rows a)) addrs))
                      (leaf (or (first fs) "<outside>"))
                      (fs (remove nil fs)))
                 (unless (or (member "%SREPL-READ-FORM-TEXT" fs :test #'string=)
                             (not (member "MVM-EVAL" fs :test #'string=)))
                   (incf n) (bump selfc leaf)
                   (let ((path (collapse (reverse fs))))
                     (tree-add tree (nthcdr (position "MVM-EVAL" path :test #'string=) path))))))
    (format t "~,1F..~,1F s: ~D samples~%" t0 t1 n)
    (tree-walk tree n 3 40 "~A~4,1F% ~A~%")
    (format t "self: ~{~A~^, ~}~%"
            (loop for (f . c) in (most-common selfc 14) collect (format nil "~A ~,1F%" f (pct c n))))))
