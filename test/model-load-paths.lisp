;;;; test/model-load-paths.lisp -- floats from bits, stored floats from bits, bulk file reads.
;;;;   modus --script test/model-load-paths.lisp   => MODEL-LOAD-PATHS PASS
;;;;
;;;; A model loads by reading its weight file whole (READ-SEQUENCE into a byte vector: a regular
;;;; file is read straight into the vector) and decoding each weight from its IEEE-32 bits
;;;; (SB-KERNEL:MAKE-SINGLE-FLOAT, natively converted; stored into a single-float vector, the
;;;; bits written into the lane).  Every expected value here is SBCL's.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defun s32 (p) (if (>= p #x80000000) (- p #x100000000) p))
(defvar *normal* '(0 #x80000000 1 #x807FFFFF #x00800000 #x3F800000 #xBF800000 #x7F7FFFFF #x40490FDB #x3EAAAAAB))
(defvar *special* '(#x7F800000 #xFF800000 #x7FC00000))

(check "values" (mapcar (lambda (p) (sb-kernel:make-single-float (s32 p))) *normal*)
       (list 0.0 -0.0 1.4012985e-45 -1.1754942e-38 1.1754944e-38 1.0 -1.0 3.4028235e38 3.1415927 0.33333334))
(check "infinities" (mapcar (lambda (p) (let ((f (sb-kernel:make-single-float (s32 p)))) (list (> f 0) (= f f))))
                            '(#x7F800000 #xFF800000))
       '((t t) (nil t)))
(check "nan" (let ((f (sb-kernel:make-single-float (s32 #x7FC00000)))) (= f f)) nil)
;; stored straight into a vector: the bits in the lane exactly
(defun store-all (pats)
  (let ((v (make-array (length pats) :element-type 'single-float)) (i 0))
    (dolist (p pats v) (setf (aref v i) (sb-kernel:make-single-float (s32 p))) (setq i (+ i 1)))))
(check "stored" (map 'list (lambda (f) (logand (sb-kernel:single-float-bits f) #xFFFFFFFF))
                     (store-all (append *normal* *special*)))
       (append *normal* *special*))
(check "stored-bad-index" (handler-case (let ((v (make-array 2 :element-type 'single-float)))
                                          (setf (aref v (+ 1 (length *normal*))) (sb-kernel:make-single-float 0)) :no)
                            (error () :err))
       :err)

;; READ-SEQUENCE: a few bytes through the stream's buffer, then the rest in bulk
(defvar *path* (format nil "/tmp/modus-model-load-paths-~d.bin" (random 1000000 (make-random-state t))))
(with-open-file (o *path* :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
  (dotimes (i 3000000) (write-byte (mod (* i 7) 256) o)))
(with-open-file (s *path* :element-type '(unsigned-byte 8))
  (let ((a (make-array 10 :element-type '(unsigned-byte 8)))
        (b (make-array 2999990 :element-type '(unsigned-byte 8))))
    (read-sequence a s)
    (let ((n (read-sequence b s)) (bad 0))
      (dotimes (i 2999990) (unless (= (aref b i) (mod (* (+ i 10) 7) 256)) (incf bad)))
      (check "bulk-read" (list (coerce a 'list) n bad (read-byte s nil :eof) (file-position s))
             '((0 7 14 21 28 35 42 49 56 63) 2999990 0 :eof 3000000)))))
(with-open-file (s *path* :element-type '(unsigned-byte 8))
  (let ((b (make-array 3000010 :element-type '(unsigned-byte 8) :initial-element 9)))
    (check "short-read" (list (read-sequence b s) (aref b 2999999) (aref b 3000000)) (list 3000000 57 9))))
(delete-file *path*)

(format t "~&MODEL-LOAD-PATHS ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
