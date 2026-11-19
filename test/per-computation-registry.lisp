;;;; test/per-computation-registry.lisp -- a library can make a global per-computation, on every hosted image.
;;;;   modus --script test/per-computation-registry.lisp   => PER-COMPUTATION-REGISTRY PASS
;;;;
;;;; Libraries (natrium's DRBG, seal's GCM key cache and verified-signature cache) find
;;;; CL-USER::REGISTER-PER-COMPUTATION-SPECIAL at load time and register their lazily filled globals, so each
;;;; thread fills its own copy.  The aarch64 hosted image (macOS, iOS) linked the registry but did not
;;;; publish it, so FBOUNDP found nothing, the globals stayed shared, and the first worker to want a random
;;;; byte -- every TLS handshake on a worker -- had its store refused.
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(check "the registry is published" (and (fboundp 'cl-user::register-per-computation-special) t) t)

(defvar *lazy* nil)
(when (fboundp 'cl-user::register-per-computation-special)
  (funcall 'cl-user::register-per-computation-special '*lazy*))
(defun lazy () (or *lazy* (setf *lazy* (list :filled))))

(check "a worker fills its own copy"
       (sb-thread:join-thread (sb-thread:make-thread (lambda () (modus:share (copy-list (lazy))))))
       '(:filled))
(check "and the main thread's is untouched" *lazy* nil)
(check "the main thread fills its own" (lazy) '(:filled))
(check "a later worker starts empty again"
       (sb-thread:join-thread (sb-thread:make-thread (lambda () (and *lazy* t))))
       nil)

(format t "~&PER-COMPUTATION-REGISTRY ~:[FAIL (~D)~;PASS~]~%" (zerop *fails*) *fails*)
