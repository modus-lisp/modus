;;;; test/gc-major-threads.lisp -- generational GC majors with LIVE THREADS (aarch64, x64).
;;;;
;;;;   modus --load test/gc-major-threads.lisp                       ; 40 majors, 2 threads
;;;;   modus --eval '(defvar *op* :minor)' --load ...                ; control: minors
;;;;   modus --eval '(defvar *nthreads* 0)' --load ...               ; control: no threads
;;;;
;;;; Two workers keep a 3000-entry chain live in their own frames and INTERN a
;;;; fresh symbol every round (the runtime lock, so the lock arena), while main
;;;; runs 40 majors and checks a keyword literal's identity and a 5000-entry
;;;; list after each.  A threaded stop-the-world collection evacuates the lock
;;;; arena; a major's phase 1 promoted only what lay below from_end, left the
;;;; arena's objects YOUNG, and phase 2 copied the old area over them: 3 runs of 3
;;;; lost data (the list came back 454 long, a worker died of a TYPE-ERROR).
;;;; Threads start BEFORE tenure: starting one carves region 0, which an old
;;;; space forbids.  Pass = "GC-MAJOR-THREADS: PASS".

(defvar *op* :major)
(defvar *nthreads* 2)
(defun force-one ()
  (if (eq *op* :major)
      (gc-major)
      (let ((before (%gc-epoch)))
        (loop (set-alloc-limit (get-alloc-ptr))
              (let ((p (cons 1 2))) (when (/= (%gc-epoch) before) (return (car p))))))))
(defvar *stop* nil)
(defvar *bad* 0)
(defvar *iters* (make-array 4 :initial-element 0))
(defun worker (id)
  (let ((chain nil))
    (dotimes (i 3000) (push (list id i (format nil "w~d-~d" id i)) chain))
    (loop until *stop* do
      (let ((n 0))
        (dolist (c chain) (unless (and (eql (first c) id) (string= (third c) (format nil "w~d-~d" id (second c)))) (incf *bad*)) (incf n))
        (unless (= n 3000) (incf *bad*)))
      (intern (format nil "SYM-~d-~d" id (aref *iters* id)) "CL-USER")
      (incf (aref *iters* id)))))
(defvar *ths* (loop for i from 1 to *nthreads* collect (let ((id i)) (sb-thread:make-thread (lambda () (worker id))))))
(sleep 1)
(print (gc-tenure :young (* 16 1048576)))
(defvar *main* (let ((l nil)) (dotimes (i 5000) (push (cons i (format nil "m~d" i)) l)) l))
(defun main-check ()
  (let ((n 0) (b 0)) (dolist (c *main*) (unless (string= (cdr c) (format nil "m~d" (car c))) (incf b)) (incf n)) (list n b)))
(dotimes (k 40)
  (let ((r (list (force-one) :us (main-check))))
    (unless (and (eq (second r) :us) (equal (third r) '(5000 0)))
      (format t "MAJOR ~d WRONG: ~s~%" k r) (incf *bad*))))
(setq *stop* t)
(sleep 1)
(let ((au (%gc-old-audit)) (ca (%gc-card-audit)))
  (format t "~a ~s worker rounds ~s~%"
          (if (and (= *bad* 0) (= 0 (second au)) (= 0 (third au)) (= 0 (second ca))
                   (or (= *nthreads* 0) (> (aref *iters* 1) 0)))
              "GC-MAJOR-THREADS: PASS" "GC-MAJOR-THREADS: FAIL")
          (gc-gen-report) *iters*))
