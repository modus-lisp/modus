;;;; strip.lisp -- the Listener band of a MAME run's screenshots, stacked into one
;;;; image to read all the results from.
;;;;
;;;;   sbcl --script scripts/period-lisps/strip.lisp RUNDIR [Y0 Y1]
;;;;
;;;; RUNDIR/shot-02.png .. shot-NN.png (run-lisp.sh's; shot-01 is before the first
;;;; form), rows Y0..Y1 of each (default 215..330), stacked with a 2-pixel grey
;;;; gap after each into RUNDIR/strip.png.  PNGs are decoded by pigment and
;;;; encoded by png-pure, found in the workspace (WORKSPACE, default the
;;;; directory above this checkout).  It replaced strip.py (Pillow), pixel for
;;;; pixel (2026-11-21).

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
  (asdf:load-system "pigment") (asdf:load-system "png-pure"))

(defun read-octets (path)
  (with-open-file (i path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence v i) v)))

(defun main (dir &optional (y0 "215") (y1 "330"))
  (let* ((dir (if (char= (char dir (1- (length dir))) #\/) dir (concatenate 'string dir "/")))
         (y0 (parse-integer y0)) (y1 (parse-integer y1))
         (shots (rest (sort (mapcar #'namestring (directory (concatenate 'string dir "shot-*.png"))) #'string<)))
         (ims (mapcar (lambda (p) (pigment:png-decode (read-octets p))) shots))
         (bands (mapcar (lambda (im) (- (min y1 (pigment:img-h im)) (min y0 (pigment:img-h im)))) ims))
         (w (reduce #'max ims :key #'pigment:img-w :initial-value 1))
         (h (max 1 (reduce #'+ bands :key (lambda (b) (+ b 2)))))
         (out (make-array (* w h 3) :element-type '(unsigned-byte 8) :initial-element 128)))   ; grey
    (let ((y 0))
      (loop for im in ims for band in bands
            do (let ((iw (pigment:img-w im)) (rgba (pigment:img-rgba im)) (top (min y0 (pigment:img-h im))))
                 (dotimes (r band)
                   (dotimes (x iw)
                     (let ((s (* 4 (+ (* (+ top r) iw) x))) (d (* 3 (+ (* (+ y r) w) x))))
                       (setf (aref out d) (aref rgba s) (aref out (+ d 1)) (aref rgba (+ s 1))
                             (aref out (+ d 2)) (aref rgba (+ s 2))))))
                 (incf y (+ band 2)))))
    (let ((path (concatenate 'string dir "strip.png")))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (png-pure:encode-png out w h :channels 3) o))
      (format t "~A ~D shots~%" path (length ims)))))

(apply #'main (rest sb-ext:*posix-argv*))
