;;;; callers.lisp PROFDIR PHASE NAME [DEPTH] -- in PHASE's samples, how often NAME
;;;; is on the stack and the DEPTH (default 3) frames above it, most common first.
;;;; Names from PROFDIR/idx.json (mkidx).  It replaced callers.py (2026-11-21).
(load (merge-pathnames "prof-common.lisp" *load-truename*))
(in-package #:mac68k-prof)
(destructuring-bind (dir phase f &optional (depth "3")) (argv)
  (let ((idx (read-idx dir)) (phase (parse-integer phase)) (depth (parse-integer depth))
        (c (make-counter)) (n 0) (tot 0))
    (flet ((nm (i) (if (>= i 0) (gethash i idx "?") "<res>")))
      (dolist (l (lines (format nil "~A/samples.txt" dir)))
        (let ((q (words l)))
          (when (and (>= (length q) 4) (= (parse-integer (first q)) phase))
            (incf n)
            (let* ((st (append (reverse (mapcar (lambda (x) (nm (parse-integer x))) (nthcdr 4 q)))
                               (list (nm (parse-integer (fourth q))))))
                   (path (collapse st))
                   (i (position f path :test #'string=)))
              (when i
                (incf tot)
                (bump c (format nil "~{~A~^ < ~}" (reverse (subseq path (max 0 (- i depth)) i))))))))))
    (format t "~A ~,1F%~%" f (pct tot n))
    (loop for (k . v) in (most-common c 12) do (format t "  ~4,1F%  ~A~%" (pct v n) k))))
