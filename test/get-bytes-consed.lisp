;;;; test/get-bytes-consed.lisp -- SB-EXT:GET-BYTES-CONSED counts allocation,
;;;; exactly, across collections (hosted x86-64).
;;;;
;;;;   ./modus --script test/get-bytes-consed.lisp
;;;;
;;;; The collector adds each finished cycle's allocation to one word
;;;; (translate-x64.lisp +X64-BYTES-CONSED-ADDR+) and the reader adds the cycle
;;;; in progress.  Before it, the function was undefined and beacon's "huge
;;;; limit" section died; a stub answering 0 would have passed its allocation
;;;; bound without measuring anything, so the bound here is two-sided.

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name ok &optional detail)
  (if ok (incf *pass*) (progn (incf *fail*) (format t "~&FAIL ~A ~@[~S~]~%" name detail))))

(defun conses (n) (let ((x nil)) (dotimes (i n) (setq x (cons i x))) (length x)))
(defvar *keep* nil)

(defun consed-by (thunk)
  (let ((a (sb-ext:get-bytes-consed))) (funcall thunk) (- (sb-ext:get-bytes-consed) a)))

(check "an integer, positive" (let ((b (sb-ext:get-bytes-consed))) (and (integerp b) (plusp b))))
(check "monotonic" (let ((a (sb-ext:get-bytes-consed))) (conses 10) (>= (sb-ext:get-bytes-consed) a)))
;; 1000 conses are 16000 bytes; the loop's own frame and LENGTH add a little
(let ((d (consed-by (lambda () (conses 1000)))))
  (check "1000 conses: 16000..20000 bytes" (<= 16000 d 20000) d))
;; 300 x 1M-slot vectors (8 MB each) cross several collections
(let* ((c0 (modus-int::%gc-count))
       (d (consed-by (lambda () (dotimes (i 300) (setq *keep* (make-array 1000000))))))
       (gcs (- (modus-int::%gc-count) c0)))
  (check "300 big vectors: exact across collections" (<= (* 300 8000016) d (+ (* 300 8000016) 4096)) (list d gcs))
  (check "...and collections did happen" (plusp gcs) gcs))
(let ((d (consed-by (lambda () (dotimes (i 50) (conses 1000000))))))
  (check "50M conses: 800 MB" (<= 800000000 d 800100000) d))

(format t "~&get-bytes-consed: ~D passed, ~D failed~%" *pass* *fail*)
(format t "~&~:[FAIL~;PASS~]~%" (zerop *fail*))
