;;;; thread-guard.lisp -- REPLACE / FILL on worker threads, and the shared-store
;;;; guard: a worker storing its OWN objects into shared memory is refused, as by
;;;; ASET.  BULK-MOVE checks every word before storing any, so after the refused
;;;; REPLACE SHARED-AFTER is #(0 0 0 0) (the element loop had stored the first two).
;; bulk ops on worker threads (the shared-store guard pass runs while threads are live)
(defvar *shared* (make-array 64 :initial-element 0))
(defun work (k)
  (let ((v (make-array 1000 :initial-element k)) (w (make-array 1000)) (ok t))
    (dotimes (r 200)
      (fill w (list k r) :start 10 :end 900)
      (replace v w :start1 5 :start2 10 :end2 900)
      (replace v v :start1 1 :end2 500)
      (unless (equal (aref v 6) (list k r)) (setq ok nil)))
    ;; fixnums into shared memory: allowed
    (fill *shared* k :start k :end (1+ k))
    ok))
(let ((ts (loop for k below 4 collect (let ((k k)) (sb-thread:make-thread (lambda () (work k)))))))
  (format t "THREADS ~S shared=~S~%" (mapcar #'sb-thread:join-thread ts) (subseq *shared* 0 4)))
;; a worker storing ITS OWN cons into shared memory through FILL: the guard should refuse it, like ASET
(format t "GUARD ~S~%"
        (sb-thread:join-thread
          (sb-thread:make-thread (lambda () (handler-case (progn (fill *shared* (list 'mine) :start 8 :end 9) :stored) (error (c) (list :refused (type-of c))))))))
(format t "GUARD-REPLACE ~S~%"
        (sb-thread:join-thread
          (sb-thread:make-thread (lambda () (let ((mine (vector 1 2 (list 'mine) 4)))
                                              (handler-case (progn (replace *shared* mine :start1 20) :stored)
                                                (error (c) (list :refused (type-of c)))))))))
(format t "SHARED-AFTER ~S~%" (subseq *shared* 20 24))
(format t "REPLACE-FIXNUMS ~S~%"
        (sb-thread:join-thread
          (sb-thread:make-thread (lambda () (handler-case (progn (replace *shared* (vector 7 8 9) :start1 30) (subseq *shared* 30 33))
                                              (error (c) (list :refused (type-of c))))))))
