;;;; test/macho-e2e-script.lisp -- what test/run-macho-e2e.sh (and host/macos/mac-test.sh)
;;;; runs inside the macOS CLI: each line it prints is "ok NAME" or "FAIL NAME ...", then a
;;;; summary.  Every check exercises host calls: files (openat/read/write/fstat/lseek/
;;;; unlinkat), directories (getdents64), time, the collector's mmaps, a caught fault (the
;;;; signal path into the image's handler), and the JIT (MAP_JIT and the write/exec flip).
(defvar *fails* 0)
(defvar *count* 0)
(defmacro check (name form)
  `(progn (incf *count*)
          (let ((v (handler-case ,form (error (e) (format nil "error: ~A" e)))))
            (if (eq v t)
                (format t "ok ~A~%" ',name)
                (progn (incf *fails*) (format t "FAIL ~A ~S~%" ',name v))))))

(defvar *dir* (or (sb-ext:posix-getenv "MODUS_TEST_DIR") "/tmp"))
(defvar *file* (concatenate 'string *dir* "/macho-e2e-test.txt"))

(check arithmetic (= (+ 1 2) 3))
(check bignum (= (expt 2 100) 1267650600228229401496703205376))
(check write-file
       (progn (with-open-file (o *file* :direction :output :if-exists :supersede)
                (format o "hello~%from modus~%"))
              t))
(check read-file
       (with-open-file (i *file*)
         (and (equal (read-line i) "hello") (equal (read-line i) "from modus"))))
(check file-length (with-open-file (i *file*) (= (file-length i) 17)))
(check probe-file (not (null (probe-file *file*))))
(check directory
       (not (null (member "macho-e2e-test" (directory (concatenate 'string *dir* "/*.txt"))
                          :key #'pathname-name :test #'equal))))
(check delete-file (progn (delete-file *file*) (null (probe-file *file*))))
(check clock
       (let ((a (get-internal-real-time)))
         (sleep 0.05)
         (>= (- (get-internal-real-time) a) (floor internal-time-units-per-second 40))))
(check universal-time (> (get-universal-time) 3900000000))
(check gc-churn
       (let ((keep nil))
         (dotimes (i 200000) (let ((c (make-list 10 :initial-element i))) (when (zerop (mod i 1000)) (push c keep))))
         (= (length keep) 200)))
(check big-vectors
       (let ((s 0)) (dotimes (i 40) (incf s (length (make-array 1000000 :element-type '(unsigned-byte 8))))) (= s 40000000)))
;; an unmapped address on the Mac (inside the 4 GB __PAGEZERO) and under qemu alike
(check caught-fault
       (eq :caught (handler-case (modus-int::%gc-read64 #x20000) (error () :caught))))
(check handler-case (eq :c (handler-case (error "boom") (error () :c))))
(defun e2e-fib (n) (if (< n 2) n (+ (e2e-fib (- n 1)) (e2e-fib (- n 2)))))
(check jit (progn (modus:jit-eager) (= (e2e-fib 22) 17711)))
(check jit-again (= (e2e-fib 20) 6765))
(check loopback-socket
       (let ((l (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
         (setf (sb-bsd-sockets:sockopt-reuse-address l) t)
         (sb-bsd-sockets:socket-bind l #(127 0 0 1) 0)
         (sb-bsd-sockets:socket-listen l 4)
         (let ((port (nth-value 1 (sb-bsd-sockets:socket-name l)))
               (c (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
           (sb-bsd-sockets:socket-connect c #(127 0 0 1) port)
           (let* ((s (sb-bsd-sockets:socket-accept l))
                  (cs (sb-bsd-sockets:socket-make-stream c :input t :output t
                                                         :element-type '(unsigned-byte 8)))
                  (ss (sb-bsd-sockets:socket-make-stream s :input t :output t
                                                         :element-type '(unsigned-byte 8)))
                  (peer (sb-bsd-sockets:socket-peername s)))
             (write-sequence #(104 101 108 108 111) cs)
             (force-output cs)
             (let ((buf (make-array 5 :element-type '(unsigned-byte 8))))
               (read-sequence buf ss)
               (prog1 (and (equalp buf #(104 101 108 108 111)) (equalp peer #(127 0 0 1)) (> port 0))
                 (sb-bsd-sockets:socket-close s) (sb-bsd-sockets:socket-close c)
                 (sb-bsd-sockets:socket-close l)))))))
;; MODUS_TEST_THREADS: native threads (pthreads on the Mac).  Not under qemu: Linux user
;; space cannot give a thread its own TPIDRRO_EL0, which the image reads its delta through.
(when (sb-ext:posix-getenv "MODUS_TEST_THREADS")
  (check thread
         (let ((th (sb-thread:make-thread (lambda () (let ((s 0)) (dotimes (i 100000) (incf s i)) s)))))
           (= (sb-thread:join-thread th) 4999950000)))
  (check threads-and-gc
         (let ((ths (loop for k below 4
                          collect (let ((k k))
                                    (sb-thread:make-thread
                                     (lambda () (let ((l nil)) (dotimes (i 50000) (push (* k i) l))
                                                  (reduce #'+ l))))))))
           (equal (mapcar #'sb-thread:join-thread ths)
                  (loop for k below 4 collect (* k 1249975000))))))
(format t "macho-e2e: ~D checks, ~D failed~%" *count* *fails*)
