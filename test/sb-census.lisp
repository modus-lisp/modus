;;;; test/sb-census.lisp -- the SB-* names a census of the org's code found modus lacked
;;;; (2026-11-27).  Each line must read the same under SBCL and modus;
;;;; test/run-sb-census.sh compares them.  The first argument is a scratch directory.
(require :asdf) (require :sb-posix)
(defvar *dir* (car (last (uiop:command-line-arguments))))
(defun line (what v) (format t "~a: ~s~%" what v))

;; SB-EXT:WITH-LOCKED-HASH-TABLE -- the table form is evaluated, not called
(let ((rounds (make-hash-table :test #'equal :synchronized t)) (plain (make-hash-table)))
  (line "with-locked-hash-table returns its body"
        (sb-ext:with-locked-hash-table (rounds) (setf (gethash "a" rounds) 1) :done))
  (line "...nested (recursive)"
        (sb-ext:with-locked-hash-table (rounds) (sb-ext:with-locked-hash-table (rounds) (gethash "a" rounds))))
  (line "hash-table-synchronized-p" (list (sb-ext:hash-table-synchronized-p rounds)
                                          (sb-ext:hash-table-synchronized-p plain))))

;; SB-THREAD
(defun quick () :quick)
(defun aborter () (sb-thread:abort-thread) :not-reached)
(defun parked (s) (sb-thread:wait-on-semaphore s) :parked)
(line "main-thread" (eq (sb-thread:main-thread) sb-thread:*current-thread*))
(let* ((s (sb-thread:make-semaphore))
       (th (sb-thread:make-thread #'parked :arguments (list s))))
  (line "list-all-threads has main and a live thread"
        (list (and (member (sb-thread:main-thread) (sb-thread:list-all-threads)) t)
              (and (member th (sb-thread:list-all-threads)) t)))
  (sb-thread:signal-semaphore s)
  (sb-thread:join-thread th)
  (line "...and not a finished one" (and (member th (sb-thread:list-all-threads)) t)))
(line "abort-thread: join answers the default"
      (sb-thread:join-thread (sb-thread:make-thread #'aborter) :default :aborted))
(line "abort-thread on main without :allow-exit signals"
      (handler-case (sb-thread:abort-thread) (error () :refused)))

;; SB-INT:ENCAPSULATE
(defun victim (x) (* x 10))
(sb-int:encapsulate 'victim 'plus1 (lambda (f x) (+ 1 (funcall f x))))
(sb-int:encapsulate 'victim 'twice (lambda (f x) (* 2 (funcall f x))))
(line "encapsulate: newest outermost" (victim 3))
(line "encapsulated-p" (list (sb-int:encapsulated-p 'victim 'plus1) (sb-int:encapsulated-p 'victim 'nope)))
(sb-int:unencapsulate 'victim 'plus1)
(line "unencapsulate an inner layer" (victim 3))
(sb-int:unencapsulate 'victim 'twice)
(line "unencapsulate the last" (victim 3))
(line "strerror" (sb-int:strerror 2))

;; SB-POSIX
(line "o- constants" (list sb-posix:o-rdonly sb-posix:o-wronly sb-posix:o-rdwr sb-posix:o-creat
                           sb-posix:o-excl sb-posix:o-trunc sb-posix:o-append sb-posix:o-nonblock))
(line "errno constants" (list sb-posix:eintr sb-posix:eagain sb-posix:eacces sb-posix:eexist
                              sb-posix:econnrefused))
(let ((f (concatenate 'string *dir* "f.txt")) (l (concatenate 'string *dir* "link"))
      (d (concatenate 'string *dir* "sub/")))
  (with-open-file (o f :direction :output :if-exists :supersede) (write-string "hello" o))
  (let ((fd (sb-posix:open f sb-posix:o-rdonly)))
    (line "open, fstat size, fsync, close"
          (list (integerp fd) (sb-posix:stat-size (sb-posix:fstat fd)) (sb-posix:fsync fd)
                (sb-posix:close fd))))
  (line "open a missing file signals"
        (handler-case (sb-posix:open (concatenate 'string *dir* "nope") sb-posix:o-rdonly)
          (sb-posix:syscall-error () :syscall-error)))
  (with-open-file (o f :direction :output :if-exists :append) (sb-posix:fsync o) (line "fsync a stream" :ok))
  (sb-posix:symlink "f.txt" l)
  (line "symlink, readlink" (sb-posix:readlink l))
  (line "lstat is the link, stat the file"
        (list (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat l)))
              (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:stat l)))
              (sb-posix:stat-size (sb-posix:stat l))))
  (line "stat times and links" (let ((st (sb-posix:stat f)))
                                 (list (integerp (sb-posix:stat-mtime st))
                                       (< (abs (- (sb-posix:stat-mtime st)
                                                  (- (get-universal-time) 2208988800)))
                                          600)
                                       (sb-posix:stat-nlink st)
                                       (= (sb-posix:stat-dev st) (sb-posix:stat-dev (sb-posix:stat *dir*))))))
  (ensure-directories-exist d)
  (sb-posix:rmdir d)
  (line "rmdir" (probe-file d)))
(line "kill pid, signal 0" (sb-posix:kill (sb-posix:getpid) 0))
(line "getgid" (integerp (sb-posix:getgid)))
(sb-posix:putenv "SB_CENSUS_PROBE=one")
(line "putenv: posix-getenv and uiop:getenv see it"
      (list (sb-ext:posix-getenv "SB_CENSUS_PROBE") (uiop:getenv "SB_CENSUS_PROBE")))
(line "posix-environ has it" (and (member "SB_CENSUS_PROBE=one" (sb-ext:posix-environ) :test #'string=) t))
(line "posix-environ has HOME" (and (find-if (lambda (e) (and (> (length e) 5) (string= "HOME=" e :end2 5)))
                                             (sb-ext:posix-environ))
                                    t))

;; SB-EXT / SB-KERNEL numbers
(line "*gc-run-time*" (integerp sb-ext:*gc-run-time*))
(line "bytes-consed-between-gcs" (plusp (sb-ext:bytes-consed-between-gcs)))
(line "dynamic-usage" (plusp (sb-kernel:dynamic-usage)))
(line "*after-gc-hooks*" (listp sb-ext:*after-gc-hooks*))

;; SB-SYS
(line "interactive-interrupt is serious" (subtypep 'sb-sys:interactive-interrupt 'serious-condition))
(line "io-timeout is a stream-error" (subtypep 'sb-sys:io-timeout 'stream-error))
(line "without-interrupts" (sb-sys:without-interrupts (+ 1 2)))
