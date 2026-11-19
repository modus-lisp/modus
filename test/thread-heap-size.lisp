;;;; test/thread-heap-size.lisp -- a thread's heap is as big as the program asks.
;;;;   MODUS_THREAD_HEAP=64 modus --script test/thread-heap-size.lisp   => THREAD-HEAP-SIZE PASS
;;;;   modus --script test/thread-heap-size.lisp                        => 16 MB regions, the big case SKIPs
;;;;
;;;; Every thread's region was 16 MB, so a worker holding more than that -- a picture, a web page --
;;;; ended the process (GC: HEAP EXHAUSTED).  *THREAD-HEAP-BYTES* / MODUS_THREAD_HEAP size them.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))
(defun on-thread (fn) (sb-thread:join-thread (sb-thread:make-thread (lambda () (modus:share (funcall fn))))))

(check "a thread conses at all" (on-thread (lambda () (length (make-list 1000)))) 1000)
(let ((rsize modus-int::*ha-rsize*) (n modus-int::*ha-nregions*))
  (format t "~&regions: ~D of ~D MB~%" n (floor rsize #x100000))
  (check "at least two regions" (>= n 2) t)
  (if (>= rsize (* 48 #x100000))
      (progn
        ;; 30 MB live on the worker, held across collections: more than a 16 MB region holds
        (check "a worker holds 30 MB"
               (on-thread (lambda ()
                            (let ((keep (loop repeat 30 collect (make-array #x100000 :element-type '(unsigned-byte 8)
                                                                                      :initial-element 7))))
                              (dotimes (i 20) (make-list 10000))
                              (reduce #'+ keep :key (lambda (v) (aref v 1000))))))
               210)
        (check "the size asked for" rsize (let ((e (sb-ext:posix-getenv "MODUS_THREAD_HEAP")))
                                            (if e (* (parse-integer e) #x100000) rsize))))
      (format t "SKIP the big case: 16 MB regions (set MODUS_THREAD_HEAP=64)~%")))
(format t "~&THREAD-HEAP-SIZE ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
