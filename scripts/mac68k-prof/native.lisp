;;;; native.lisp PROF.txt CL.map T0 T1 [LOAD] [TOP] -- a profile of the native
;;;; (quadra) image from lisp.lua's PROF samples between emulated seconds T0 and
;;;; T1: self and inclusive time per function, the PC and the A6 frame chain's
;;;; return addresses mapped through the build's symbol map (CL.map, linked at
;;;; LOAD, hex, default 600000; the samples' "BASE b" line is where the relocating
;;;; load put it).  Samples whose PC is outside the image count as <outside>.
;;;; EXCLUDE=NAME drops samples with NAME on the stack (the REPL's reader while
;;;; MAME types); CALLERS=NAME adds who calls NAME.  TOP rows (default 40).  It
;;;; replaced native.py (2026-11-21), less a bug: with EXCLUDE unset, native.py's
;;;; `None in fs' dropped every sample with an unmapped frame, so <outside> never
;;;; showed.
(load (merge-pathnames "prof-common.lisp" *load-truename*))
(in-package #:mac68k-prof)
(destructuring-bind (prof cmap t0 t1 &optional (load "600000") (top "40")) (argv)
  (let* ((rd (lambda (s) (let ((*read-default-float-format* 'double-float)) (float (read-from-string s) 1d0))))
         (t0 (funcall rd t0)) (t1 (funcall rd t1)) (load (hex load)) (top (parse-integer top))
         (rows (read-map cmap)) (delta 0) (n 0)
         (selfc (make-counter)) (incl (make-counter)) (edges (make-counter)) (icallers (make-counter))
         (exclude (env "EXCLUDE")) (want (env "CALLERS")))
    (dolist (l (lines prof))
      (let ((q (words l)))
        (if (string= (first q) "BASE")
            (setq delta (- (hex (second q)) load))
            (let ((tm (funcall rd (first q))))
              (when (and (<= t0 tm) (< tm t1))
                (incf n)
                (let* ((addrs (loop for x in (rest q) unless (find #\= x) collect (- (hex x) delta)))
                       (fs (mapcar (lambda (a) (map-name rows a)) addrs)))
                  (unless (and exclude (member exclude fs :test #'equal))
                    (let ((leaf (or (first fs) "<outside>")) (chain (remove nil fs)))
                      (bump selfc leaf)
                      (dolist (f (remove-duplicates (append chain (list leaf)) :test #'string= :from-end t))
                        (bump incl f))
                      (when (>= (length chain) 2) (bump edges (cons (second chain) (first chain))))
                      (when (and want (member want chain :test #'string=))
                        (let ((k (position want chain :test #'string= :from-end t)))
                          (bump icallers (if (< (1+ k) (length chain)) (nth (1+ k) chain) "<top>"))))))))))))
    (format t "~D samples, ~,1F s~%--- self~%" n (- t1 t0))
    (loop for (f . c) in (most-common selfc top) do (format t "~5,1F%  ~A~%" (pct c n) f))
    (format t "--- inclusive~%")
    (loop for (f . c) in (most-common incl top)
          do (format t "~5,1F%  self ~4,1F%  ~A~%" (pct c n) (pct (count-of selfc f) n) f))
    (when want
      (let ((callers (make-counter)))
        (dolist (e (reverse (counter-order edges)))           ; in the order they appeared
          (when (string= (cdr e) want) (bump callers (car e) (count-of edges e))))
        (format t "--- callers of ~A (leaf samples)~%" want)
        (loop for (f . c) in (most-common callers 15) do (format t "~5,1F%  ~A~%" (pct c n) f))
        (format t "--- callers of ~A (inclusive)~%" want)
        (loop for (f . c) in (most-common icallers 15) do (format t "~5,1F%  ~A~%" (pct c n) f))))))
