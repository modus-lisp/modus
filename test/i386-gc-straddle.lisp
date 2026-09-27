;;;; i386-gc-straddle.lisp -- live large arrays allocated across the semispace
;;;; boundary must survive collection.  Run: ./modus-i386 --script test/i386-gc-straddle.lisp
;;;; Measured, same 8 collections: unfixed build BAD 16 (every run), with
;;;; *I386-GC-VL-MARGIN* BAD 0.  An earlier interpreted fill loop SIGSEGVd too.
;; Live large arrays allocated across the semispace boundary must survive.
(defun arr-ok (a v) (and (= (svref a 0) v) (= (svref a (1- (length a))) v) (= (svref a (floor (length a) 2)) v)))
(defun run (n)
  (let ((ring (make-array 8)) (bad 0) (gc0 (mem-ref #x10000060 :u32)))
    (dotimes (k n)
      (setf (svref ring (mod k 8)) (make-array 100000 :initial-element k))
      (dotimes (j 8)
        (let ((a (svref ring j)))
          (when (and a (not (arr-ok a (svref a 0))))
            (setq bad (+ bad 1))))))
    (list :iters n :bad bad :gcs (- (mem-ref #x10000060 :u32) gc0))))
(let ((r (run 1500)))
  (format t "~&STRADDLE ~S~%" r)
  ;; an uncaught ERROR is what makes --script exit 1 (SYS-EXIT does not)
  (when (> (getf r :bad) 0) (error "i386-gc-straddle: ~D corrupted live arrays" (getf r :bad))))
