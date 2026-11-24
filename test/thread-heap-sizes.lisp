;;;; test/thread-heap-sizes.lisp -- regions of different sizes, and a thread asks for one.
;;;;   MODUS_THREAD_HEAPS=64x2,16x6 modus --script test/thread-heap-sizes.lisp  => THREAD-HEAP-SIZES PASS
;;;;   modus --script test/thread-heap-sizes.lisp                              => the uniform carve, unchanged
;;;;
;;;; One size for every region made a program choose: a phone's web-page worker needs 64 MB, and
;;;; every region that big left region 0 so little room over the world's live data that a video
;;;; decoder hung at its first collection.  *THREAD-HEAP-SIZES* / MODUS_THREAD_HEAPS lay the
;;;; regions out by size, and SB-THREAD:MAKE-THREAD :HEAP-BYTES picks the smallest free one that fits.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defun my-region-mb () (floor (modus-int::%ha-rsize (modus-int::%thr-cpu)) #x100000))
(defun on-thread (fn &rest keys)
  (sb-thread:join-thread (apply #'sb-thread:make-thread (lambda () (modus:share (funcall fn))) keys)))
(defun churn-and-hold (mb)
  "Hold MB megabytes live across collections, churning garbage around it; the held bytes summed."
  (let ((keep (loop repeat mb collect (make-array #x100000 :element-type '(unsigned-byte 8) :initial-element 3))))
    (dotimes (i 200) (make-list 20000))
    (reduce #'+ keep :key (lambda (v) (aref v 77)))))

(check "a thread conses at all" (on-thread (lambda () (length (make-list 1000)))) 1000)
(let ((n modus-int::*ha-nregions*)
      (sizes (loop for i below modus-int::*ha-nregions* collect (floor (modus-int::%ha-rsize i) #x100000))))
  (format t "~&regions: ~D, MB ~S~%" n sizes)
  (check "at least two regions" (>= n 2) t)
  ;; the regions are disjoint and in order, in both semispaces
  (check "regions are contiguous and disjoint"
         (loop for i from 1 below n
               always (and (= (modus-int::%ha-region-from i)
                              (+ (modus-int::%ha-region-from (1- i)) (modus-int::%ha-rsize (1- i))))
                           (= (modus-int::%ha-region-to i)
                              (+ (modus-int::%ha-region-to (1- i)) (modus-int::%ha-rsize (1- i))))))
         t)
  (if (not (modus-int::%ha-layout-p))
      (progn
        (check "uniform: every region the same size" (every (lambda (s) (= s (first sizes))) sizes) t)
        (check "uniform: :HEAP-BYTES changes nothing"
               (on-thread #'my-region-mb :heap-bytes (* 64 #x100000)) (first sizes))
        (format t "SKIP the layout cases: set MODUS_THREAD_HEAPS=64x2,16x6~%"))
      (progn
        (check "the layout: region 0 is 16 MB" (first sizes) 16)
        (check "the layout: slots 1 and 2 are 64 MB" (subseq sizes 1 3) '(64 64))
        (check "no size asked: the default (16 MB) slot" (on-thread #'my-region-mb) 16)
        (check ":HEAP-BYTES 64 MB: a 64 MB slot" (on-thread #'my-region-mb :heap-bytes (* 64 #x100000)) 64)
        (check ":HEAP-BYTES 20 MB: the smallest that fits, 64" (on-thread #'my-region-mb :heap-bytes (* 20 #x100000)) 64)
        (check ":HEAP-BYTES 16 MB: 16" (on-thread #'my-region-mb :heap-bytes #x1000000) 16)
        (check "a 64 MB worker holds 30 MB across collections"
               (on-thread (lambda () (churn-and-hold 30)) :heap-bytes (* 64 #x100000)) 90)
        ;; both sizes at once, each collecting its own region while main collects region 0
        (let ((big (sb-thread:make-thread (lambda () (modus:share (churn-and-hold 30))) :heap-bytes (* 64 #x100000)))
              (small1 (sb-thread:make-thread (lambda () (modus:share (churn-and-hold 6))) :heap-bytes #x1000000))
              (small2 (sb-thread:make-thread (lambda () (modus:share (churn-and-hold 6))) :heap-bytes #x1000000))
              (mine (churn-and-hold 40)))
          (check "main, a 64 MB and two 16 MB workers, collecting at once"
                 (list mine (sb-thread:join-thread big) (sb-thread:join-thread small1) (sb-thread:join-thread small2))
                 '(120 90 18 18))))))
(format t "~&THREAD-HEAP-SIZES ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
