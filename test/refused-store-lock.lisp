;;;; test/refused-store-lock.lisp -- a store the shared-store guard refuses INSIDE a locked section
;;;; releases the runtime lock.  A worker's (setf (symbol-value '*x*) 3d0) is refused (the double is
;;;; the worker's own object; the globals table is shared) inside SET-SYMBOL-VALUE's %RT-ENTER, and
;;;; the unwind used to leave the lock held: the next thread to need it waited 60 s and the process
;;;; stopped ("its owner leaked it").  Also SB-POSIX:GETENV, which was missing.
;;;;   ./modus --script test/refused-store-lock.lisp      (modus only: SBCL does not refuse the store)
(defvar *rate* 0d0)
(defun bad-store (r)
  (handler-case (progn (setf (symbol-value (find-symbol "*RATE*" "CL-USER")) (float r 1d0)) :stored)
    (error (e) (if (search "stored one of its own objects" (princ-to-string e)) :refused (list :other e)))))
(defun read-rate () (symbol-value '*rate*))
(defun secs (thunk) (let ((t0 (get-internal-real-time))) (values (funcall thunk) (/ (- (get-internal-real-time) t0) internal-time-units-per-second 1.0))))
(let ((fails 0))
  (flet ((check (name ok detail) (format t "~&~:[FAIL~;ok  ~] ~a  ~s~%" ok name detail) (finish-output) (unless ok (incf fails))))
    (let ((r (sb-thread:join-thread (sb-thread:make-thread #'bad-store :arguments (list 3)))))
      (check "the worker's store into the shared global is refused" (eq r :refused) r))
    (multiple-value-bind (v s) (secs (lambda () (sb-thread:join-thread (sb-thread:make-thread #'read-rate))))
      (check "the next thread takes the runtime lock at once" (and (eql v 0d0) (< s 5)) (list v s)))
    (check "main still interns" (symbolp (intern "REFUSED-STORE-LOCK-AFTER")) t)
    (check "sb-posix:getenv" (equal (sb-posix:getenv "HOME") (sb-ext:posix-getenv "HOME")) (sb-posix:getenv "HOME"))
    (sb-posix:setenv "REFUSED-STORE-LOCK-ZZ" "v" 1)
    (check "sb-posix:getenv sees setenv" (equal (sb-posix:getenv "REFUSED-STORE-LOCK-ZZ") "v") (sb-posix:getenv "REFUSED-STORE-LOCK-ZZ")))
  (format t "~&refused-store-lock: ~:[FAIL~;PASS~] (~d failed)~%" (zerop fails) fails))
