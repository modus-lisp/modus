;;;; worker-first-stub-call.lisp -- a worker's FIRST call through a stub of a module it did not
;;;; compile fills that module's stub cache under the runtime lock (mvm/interp.lisp, A WORKER
;;;; FILLS A SHARED MODULE'S STUB CACHE UNDER THE RUNTIME LOCK).
;;;;   modus --script test/worker-first-stub-call.lisp
;;;; Before: on AArch64 a thread started from a DEFVAR initform died before its first form --
;;;; "a thread stored one of its own objects into shared memory ... (at %MVM-RT-ENTRY)" -- and
;;;; JOIN-THREAD returned NIL, even with (+ 1 2) as its body; from a LET (after main had called
;;;; through the same stubs) it ran.
(defvar *fail* 0)
(defmacro check (name form) `(unless ,form (incf *fail*) (format t "~&FAIL: ~a~%" ,name)))

(defun %wfs-trivial () (+ 1 2))
(defun %wfs-conses () (let ((x nil)) (dotimes (i 50) (push (make-array 100) x)) (length x)))

;; Started inside a DEFVAR's initform: nothing has called through these stubs yet.
(defvar *t1* (sb-thread:make-thread #'%wfs-trivial))
(check "a thread started from a DEFVAR returns (+ 1 2)" (eql (sb-thread:join-thread *t1*) 3))
(defvar *t2* (sb-thread:make-thread #'%wfs-conses))
(check "and one that conses returns its count" (eql (sb-thread:join-thread *t2*) 50))

;; The same functions from a LET, and on main, after the workers filled the caches.
(check "from a LET" (eql (let ((th (sb-thread:make-thread #'%wfs-conses))) (sb-thread:join-thread th)) 50))
(check "and on main" (and (eql (%wfs-trivial) 3) (eql (%wfs-conses) 50)))

;; Several threads at once, each the first through a fresh function's stubs.
(defun %wfs-fresh (k) (let ((s 0)) (dotimes (i 100) (incf s (* k i))) s))
(defvar *many* (loop for k from 1 to 4 collect (let ((k k)) (sb-thread:make-thread (lambda () (%wfs-fresh k))))))
(check "four threads through a fresh function at once"
       (equal (mapcar #'sb-thread:join-thread *many*) (loop for k from 1 to 4 collect (* k 4950))))

(format t "~&worker-first-stub-call: ~:[PASS~;FAIL (~:*~a)~]~%" (and (plusp *fail*) *fail*))
(sb-ext:exit :code (if (zerop *fail*) 0 1) :abort t)
