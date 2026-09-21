;;;; #307 handler-semantics probe.  Every form must print the same thing under
;;;; SBCL, under Modus with the JIT on, and under Modus with the JIT off.
#-sbcl (setq *jit-hot-only* nil)
(defvar *log* nil)
(defvar *sv* 0)

;; H1: nested handler-case, inner returns NORMALLY, outer must still catch.
(defun inner-ok () (handler-case (+ 1 1) (error (c) (declare (ignore c)) :never)))
(defun h1 () (handler-case (progn (inner-ok) (error "x")) (error (c) (declare (ignore c)) :caught)))

;; H2: unwind-protect cleanup on the NORMAL path.
(defun h2 () (let ((r nil))
               (list (unwind-protect 7 (setq r :cleaned)) r)))

;; H3: unwind-protect cleanup on the ERROR path.
(defun h3 () (let ((r nil))
               (list (handler-case (unwind-protect (error "boom") (setq r :cleaned))
                       (error (c) (declare (ignore c)) :caught))
                     r)))

;; H4: an error thrown through SEVERAL frames, each with its own unwind-protect.
(defun h4c (acc) (unwind-protect (error "deep") (push :c acc)))
(defun h4b (acc) (unwind-protect (h4c acc) (push :b acc)))
(defun h4a () (let ((acc nil))
                (list (handler-case (unwind-protect (h4b acc) (push :a acc))
                        (error (c) (declare (ignore c)) :caught))
                      (length acc))))

;; H5: handler-case inside a LOOP -- arm/disarm 500 times, then the outer
;; handler must still be armed.  This is the exact #307 shape at scale.
(defun h5 () (handler-case
                 (progn (dotimes (i 500)
                          (handler-case (+ i 1) (error (c) (declare (ignore c)) :no)))
                        (error "after-loop"))
               (error (c) (declare (ignore c)) :outer-still-armed)))

;; H6: unwind-protect inside a loop, cleanup count.
(defun h6 () (let ((n 0))
               (dotimes (i 300) (unwind-protect (+ i 1) (setq n (+ n 1))))
               n))

;; H7: dynamic binding restored on the error path.
(defun h7 () (list (handler-case (let ((*sv* 99)) (error "e")) (error (c) (declare (ignore c)) :caught))
                   *sv*))

;; H8: three levels of handler-case, the MIDDLE one returns normally, the
;; error signalled afterwards must reach the OUTERMOST, not the middle.
(defun h8mid () (handler-case :ok (error (c) (declare (ignore c)) :mid)))
(defun h8 () (handler-case
                 (handler-case (progn (h8mid) (error "z"))
                   (type-error (c) (declare (ignore c)) :wrong))
               (error (c) (declare (ignore c)) :outer)))

;; H9: block/return-from out of an unwind-protect body; cleanup must run.
(defun h9 () (let ((r nil))
               (list (block b (unwind-protect (return-from b :early) (setq r :cleaned))) r)))

;; H10: nesting depth -- 50 handler-cases deep, then signal.
(defun h10r (d) (if (= d 0) (error "bottom")
                    (handler-case (h10r (- d 1)) (type-error (c) (declare (ignore c)) :wrong))))
(defun h10 () (handler-case (h10r 50) (error (c) (declare (ignore c)) :caught-at-top)))

#-sbcl (jit-eager)
(dolist (p (list (cons "H1" #'h1) (cons "H2" #'h2) (cons "H3" #'h3)
                 (cons "H4" #'h4a) (cons "H5" #'h5) (cons "H6" #'h6)
                 (cons "H7" #'h7) (cons "H8" #'h8) (cons "H9" #'h9)
                 (cons "H10" #'h10)))
  (format t "~&~a ~s~%" (car p) (funcall (cdr p))))
;; run the whole battery a SECOND time, so any frame leaked by the first pass
;; shows up as a different answer rather than as nothing at all.
(dolist (p (list (cons "H1b" #'h1) (cons "H3b" #'h3) (cons "H5b" #'h5) (cons "H8b" #'h8)))
  (format t "~&~a ~s~%" (car p) (funcall (cdr p))))
(format t "~&HDONE~%")
