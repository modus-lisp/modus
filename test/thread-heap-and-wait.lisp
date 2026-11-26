;;;; test/thread-heap-and-wait.lisp -- two thread bugs, as checks; test/run-thread-heap-and-wait.sh
;;;; runs each part in the heap it needs:
;;;;   THW_PART=wait ./modus --script test/thread-heap-and-wait.lisp     (default heap: it must collect)
;;;;   THW_PART=heap ./modus --dynamic-space-size 8GB --script test/thread-heap-and-wait.lisp
;;;; 1. A worker's heap grows with --dynamic-space-size (net/hosted-actors.lisp,
;;;;    %HA-DEFAULT-REGION-SIZE).  It was a flat 16 MB: a 3M-character string on a worker was
;;;;    heap-exhausted at 8 GB while main had gigabytes.
;;;; 2. A worker waiting in RUN-PROGRAM does not stall main's collections (net/hosted-process.lisp:
;;;;    wait4 and the drain poll are stop-the-world safe regions).  Main's allocation loop went
;;;;    0.12 s -> 9.6 s while a worker waited on `sleep 10'.  The wait check runs first: on a
;;;;    build without fix 1 the heap check ends the process (heap exhausted).
(defun churn ()
  "2.4 GB of 800 KB vectors: several region-0 collections, each of which must stop every thread."
  (let ((n 0)) (dotimes (i 3000) (incf n (length (make-array 100000)))) n))
(defun secs (thunk) (let ((t0 (get-internal-real-time))) (funcall thunk)
                      (/ (- (get-internal-real-time) t0) internal-time-units-per-second 1.0)))
(defun big-string-length (n) (length (make-string n :initial-element #\a)))
(defun wait-sleep (s) (sb-ext:run-program "/bin/sleep" (list (princ-to-string s)) :wait t) :done)
(let ((fails 0))
  (flet ((check (name ok detail) (format t "~&~:[FAIL~;ok  ~] ~a  ~a~%" ok name detail) (unless ok (incf fails))))
    (when (equal (sb-ext:posix-getenv "THW_PART") "wait")
    (churn)                                     ; warm up (the JIT)
    (let* ((alone (secs #'churn))
           (th (sb-thread:make-thread #'wait-sleep :arguments (list 5)))
           (busy (progn (sleep 0.2) (secs #'churn))))
      (sb-thread:join-thread th)
      (check "main's collections do not wait for a worker in run-program"
             (< busy (+ (* 4 alone) 1.0))
             (format nil "alone ~,2fs, with a worker waiting ~,2fs" alone busy))))
    (when (equal (sb-ext:posix-getenv "THW_PART") "heap")
    (let ((n (handler-case (sb-thread:join-thread (sb-thread:make-thread #'big-string-length :arguments (list 3000000)))
               (error (e) (princ-to-string e)))))
      (check "a worker conses a 3M-character string" (eql n 3000000) n))))
  (format t "~&thread-heap-and-wait: ~:[FAIL~;PASS~] (~d failed)~%" (zerop fails) fails))
