;;;; test/shared-array-size.lisp -- a worker's MODUS:MAKE-SHARED-ARRAY reserves what its element
;;;; type needs.  It reserved 8 bytes an element for any type, so a worker making an 8 MB channel
;;;; (an 8 MB octet ring) asked the lock arena for 64 MB and the process stopped with "the lock
;;;; arena is exhausted"; and a locked allocation over ~900 KB never fit its slice (its limit is
;;;; the slice end less a 16th, and the slice was sized NEED + 64 KB), so it stopped the process as
;;;; "heap exhausted".  modus --script test/shared-array-size.lisp; exits 0 on PASS.
(defun mk-channel (n) (modus:make-channel :capacity n) :made)
(defun mk-octets (n) (length (modus:make-shared-array n :element-type '(unsigned-byte 8))))
(defun mk-words (n) (length (modus:make-shared-array n)))
(defun on-worker (f n) (sb-thread:join-thread (sb-thread:make-thread f :arguments (list n)) :default :died))
(let ((r (list (on-worker #'mk-channel (* 1024 1024))
               (on-worker #'mk-channel (* 8 1024 1024))
               (on-worker #'mk-octets (* 16 1024 1024))
               (on-worker #'mk-words 4096))))
  (format t "~&worker channels / arrays: ~s~%" r)
  (let ((ok (equal r (list :made :made (* 16 1024 1024) 4096))))
    (format t "shared-array-size: ~:[FAIL~;PASS~]~%" ok)
    (finish-output)
    (sb-ext:exit :code (if ok 0 1))))
