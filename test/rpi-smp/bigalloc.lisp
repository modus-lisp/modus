;;;; bigalloc.lisp -- big allocations near the limit, from runtime-compiled code.
;;;;
;;;; The bare Pi's limit guard (*AARCH64-GC-LIMIT-GUARD*) is 1 MB: safe only while every
;;;; variable-size allocation checks its OWN size (:GC-CHECK-R), including code compiled
;;;; at runtime (mvm-eval's FUSE-GC-CHECKS).  This churns u8 vectors and arrays of 0.7 to
;;;; 6 MB through many collections while a 2000-cons chain stays live, and checks the
;;;; chain's sum and region 0's geometry words -- 0x10000040.. sits right past the upper
;;;; semispace's end (0x10000000), which is where an unchecked overshoot lands.
;;;;
;;;; (ba-run ROUNDS) => (:BAD n :GCS n :SIZE n).  Pass: BAD 0, GCS > 0.
;;;; NEGATIVE CONTROL: (setq *gc-check-fusion-enabled* nil) BEFORE loading this file
;;;; compiles the same code with the old unsized checks; it must fail.

(defun ba-chain (n)
  (let ((l nil)) (dotimes (i n l) (setq l (cons i l)))))

(defun ba-sum (l)
  (let ((s 0)) (dolist (x l s) (setq s (+ s x)))))

(defun ba-churn (n)
  (let ((v (%alloc-u8 n)))
    (setf (aref v 0) 1)
    (let ((a (%make-array-raw (floor n 8))))
      (if (and (= (aref v 0) 1) a) 1 0))))

(defun ba-run (rounds)
  (let ((keep (ba-chain 2000))
        (size0 (modus-int::%gc-read64 #x10000050))
        (gc0 (modus-int::%gc-epoch))
        (bad 0))
    (dotimes (r rounds)
      (ba-churn 700000)
      (ba-churn 1500000)
      (ba-churn 3000000)
      (ba-churn 6000000)
      (unless (= (ba-sum keep) 1999000) (setq bad (+ bad 1)))
      (unless (= (modus-int::%gc-read64 #x10000050) size0) (setq bad (+ bad 1000))))
    (list :bad bad :gcs (- (modus-int::%gc-epoch) gc0) :size (modus-int::%gc-read64 #x10000050))))
