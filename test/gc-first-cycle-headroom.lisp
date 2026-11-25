;;;; test/gc-first-cycle-headroom.lisp -- the first allocation cycle stops short of the semispace end.
;;;;   modus --script test/gc-first-cycle-headroom.lisp   => GC-FIRST-CYCLE-HEADROOM PASS
;;;;
;;;; The two semispaces are adjacent, and the first collection copies survivors to the start of the
;;;; upper one.  The trampoline leaves min(16 MB, space_size/16) of headroom below each semispace's
;;;; end after every flip, but aarch64's BOOT limit was exactly the lower semispace's end (its test
;;;; compared against the default midpoint, which no build uses): an allocation that passed its
;;;; unsized check just under the limit -- a 48-byte float box -- ran past the end, and the first
;;;; collection's survivors overwrote its tail.  A double in a 200000-element array came back as
;;;; another float's bits, or the collection hit a TYPE-ERROR, depending on alignment.
;;;;
;;;; Here: in the first cycle, fill the lower semispace with garbage to 400 KB short of its end,
;;;; aim the allocation pointer so the last 48-byte box would start 16 bytes below the end, and make
;;;; float boxes, each kept, until the collection comes.  With the old limit that box straddles
;;;; and the collection crashed or corrupted it.  Then the same with the limit forced AT the end:
;;;; the box's own check is size-aware now, so it collects first.  aarch64 only (the primitives are compiled ones);
;;;; elsewhere it reports SKIP.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defun rd (off) (ash (modus-int::%gc-read64 (+ (modus-int::%conv-addr #x10000040) off)) -1))
(defun gcs () (rd #x20))
(defun ptr () (modus-int::get-alloc-ptr))
(defvar *sink* nil)
(defun fill-to (end margin)
  (loop (let ((room (- end (ptr) margin)))
          (when (< room 4096) (return nil))
          (setq *sink* (make-array (min 4000000 (- (floor room 8) 4)))))))
(defun aim (end)
  "Conses (16 bytes each) until the distance to END is 16 mod 48: the last
   48-byte box then starts 16 bytes below END -- the straddle."
  (let ((pad nil))
    (dotimes (k (mod (floor (- (mod (- end (ptr)) 48) 16) 16) 3)) (push k pad))
    pad))
(defun boxes (keep n)
  (declare (simple-vector keep))
  (let ((x 0.25d0) (g (gcs)) (i 0))
    (declare (double-float x))
    (loop (when (or (>= i n) (/= (gcs) g)) (return i))
          (setq x (+ x 1d0))
          (setf (svref keep i) (+ x 0.5d0))
          (setq i (+ i 1)))))

(if (not (and (eql (gcs) 0) (< (rd 0) (rd 8)) (> (ptr) 0)))
    (format t "~&GC-FIRST-CYCLE-HEADROOM SKIP (not in the first cycle of a lower semispace)~%")
    (let* ((end (rd 8)) (keep (make-array 100000)))
      (fill-to end 400000)
      (aim end)
      (let* ((n (handler-case (boxes keep 100000) (error (e) (list :error (type-of e)))))
             (bad (if (integerp n)
                      (let ((b 0))
                        (dotimes (i n b)
                          (unless (eql (svref keep i) (+ (+ 0.25d0 (float (+ i 1) 1d0)) 0.5d0)) (incf b))))
                      n)))
        (check "collected" (> (gcs) 0) t)
        (check "boxes-intact" bad 0))
      ;; THE BOX'S OWN CHECK, with no headroom to hide behind: the lower
      ;; semispace active again, its limit put AT its end (what the boot limit
      ;; was), the same aimed boxes.  A float box's :gc-check is now sized
      ;; (fuse-gc-checks), so the collection comes before the box that would
      ;; have crossed.
      (modus-int::%gc-collect-here)
      (when (> (rd 0) (rd 8)) (modus-int::%gc-collect-here))
      (let* ((end (rd 8)) (keep (make-array 100000)))
        (modus-int::set-alloc-limit end)
        (fill-to end 400000)
        (aim end)
        (let* ((n (handler-case (boxes keep 100000) (error (e) (list :error (type-of e)))))
               (bad (if (integerp n)
                        (let ((b 0))
                          (dotimes (i n b)
                            (unless (eql (svref keep i) (+ (+ 0.25d0 (float (+ i 1) 1d0)) 0.5d0)) (incf b))))
                        n)))
          (check "sized-check-boxes-intact" bad 0)))
      (format t "~&GC-FIRST-CYCLE-HEADROOM ~A~%" (if (zerop *fails*) "PASS" "FAIL"))))
