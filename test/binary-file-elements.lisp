;;;; binary-file-elements.lisp -- (UNSIGNED-BYTE N) / (SIGNED-BYTE N) files.
;;;;
;;;; Run: ./modus --script test/binary-file-elements.lisp   (writes ./tmp.dat)
;;;;
;;;; A file stream wrote and read ONE byte per element whatever N was, so every
;;;; value >= 256 came back truncated, FILE-LENGTH counted bytes, and signed
;;;; elements were never sign-extended.  ANSI FILE-POSITION.7/.8 and
;;;; FILE-LENGTH.3/.4 failed -- in some gate runs and not others, because the
;;;; truncation left the next test in the same process reading a short file.
;;;; Now: CEIL(N/8) little-endian bytes per element, FILE-POSITION and
;;;; FILE-LENGTH in ELEMENTS.  Expected results are SBCL's (0 failures each).

(defvar *bfe-fail* 0)
(defun ub-check (len)
  (let* ((n (ash 1 len)) (vals (loop for i from 0 below 17 collect (logand (1- n) (+ (* i 7919) (ash 1 (max 0 (- len 1)))))))
         (bad nil))
    (with-open-file (os "tmp.dat" :direction :output :if-exists :supersede :element-type `(unsigned-byte ,len))
      (loop for v in vals for i from 0
            do (let ((p (file-position os))) (unless (eql p i) (push (list :wpos i p) bad)))
               (write-byte v os)))
    (with-open-file (is "tmp.dat" :direction :input :element-type `(unsigned-byte ,len))
      (let ((fl (file-length is))) (unless (eql fl 17) (push (list :len fl) bad)))
      (loop for v in vals for i from 0
            do (let ((p (file-position is))) (unless (eql p i) (push (list :rpos i p) bad)))
               (let ((got (handler-case (read-byte is) (error (c) :err))))
                 (unless (eql got v) (push (list :val i v got) bad)))))
    bad))
(let ((fails nil))
  (loop for len from 1 to 100 do (let ((b (ub-check len))) (when b (push (cons len (first (last b))) fails))))
  (format t "~&binary-file-elements unsigned: ~D widths failed ~S~%" (length fails) (subseq (reverse fails) 0 (min 8 (length fails))))
  (setq *bfe-fail* (+ *bfe-fail* (length fails))))
(defun sb-check (len)
  (let* ((lim (ash 1 (- len 1))) (vals (list 0 -1 1 (- lim) (1- lim) -7 (floor lim 3))) (bad nil))
    (with-open-file (os "tmp.dat" :direction :output :if-exists :supersede :element-type `(signed-byte ,len))
      (dolist (v vals) (write-byte v os)))
    (with-open-file (is "tmp.dat" :direction :input :element-type `(signed-byte ,len))
      (unless (eql (file-length is) (length vals)) (push (list :len (file-length is)) bad))
      (dolist (v vals) (let ((g (read-byte is nil :eof))) (unless (eql g v) (push (list :val v g) bad))))
      (unless (eq (read-byte is nil :eof) :eof) (push :no-eof bad))
      (file-position is 3)
      (let ((g (read-byte is))) (unless (eql g (nth 3 vals)) (push (list :seek g) bad)))
      (unless (eql (file-position is) 4) (push (list :pos-after-seek (file-position is)) bad)))
    bad))
(let ((fails nil))
  (loop for len in '(2 7 8 9 15 16 17 24 31 32 33 47 63 64 65 100) do (let ((b (sb-check len))) (when b (push (cons len b) fails))))
  (format t "~&binary-file-elements signed: ~D widths failed ~S~%" (length fails) (subseq fails 0 (min 5 (length fails))))
  (setq *bfe-fail* (+ *bfe-fail* (length fails))))
(when (> *bfe-fail* 0) (error "binary-file-elements: ~D failure(s)" *bfe-fail*))
