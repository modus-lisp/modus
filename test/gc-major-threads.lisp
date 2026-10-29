;;;; test/gc-major-threads.lisp -- generational GC majors with LIVE THREADS (aarch64, x64).
;;;;
;;;;   modus --load test/gc-major-threads.lisp                       ; 40 majors, 2 threads
;;;;   modus --eval '(defvar *op* :minor)' --load ...                ; control: minors
;;;;   modus --eval '(defvar *nthreads* 0)' --load ...               ; control: no threads
;;;;   modus --eval '(defvar *audit-each* t)' --load ...             ; card audit before every major
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
;;;;
;;;; *AUDIT-EACH* is the sharp version: a young pointer on a clean card is
;;;; caught at the major that made it ("first bad", with the slots), not 40
;;;; majors later as a lost value.  It found the lock-arena chain (docs/
;;;; generational-gc.md, "Threads + majors"): 1 run in 4 audited, against 1 in
;;;; 75 failing at the end.  Measure it by RATE (hundreds of runs), not by 5.

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
(defun card-violations ()
  "%GC-CARD-AUDIT-1's walk, recording (slot object subtag-or-:cons card-byte)
   for the first few young pointers on CLEAN cards."
  (let* ((w (%gc-card-words)) (cards (%gc-read64 w)) (area (%gc-read64 (+ w 8)))
         (r (%gc-region-0)) (k (%gc-meta-scale))
         (f (%gc-meta-read r k)) (tt (%gc-meta-read (+ r 8) k))
         (sz (%gc-meta-read (+ r 16) k))
         (cbm (%census-cons-bitmap)) (page (%gc-meta-read #x10000E00 k))
         (a (%gc-old-lo)) (end (%gc-old-hi)) (out nil) (n 0))
    (flet ((slot (x obj kind)
             (when (and (< n 5)
                        (or (%gc-cand-addr (%gc-word-lo x) (%gc-word-hi x) f sz)
                            (%gc-cand-addr (%gc-word-lo x) (%gc-word-hi x) tt sz))
                        (/= 0 (mem-ref (+ cards (ash (- x area) -9)) :u8)))
               (setq n (+ n 1))
               (let ((v (%gc-word-lo x)))
                 (push (list :slot x :obj obj kind :off (- x obj)
                             :card (ash (- x area) -9)
                             :val-hi (%gc-word-hi x) :val-lo v
                             :in (if (%gc-cand-addr (%gc-word-lo x) (%gc-word-hi x) f sz) :from :to))
                       out)))))
      (loop
        (when (>= a end) (return nil))
        (if (%census-cons-p a page cbm)
            (progn (slot a a :cons) (slot (+ a 8) a :cons) (setq a (+ a 16)))
            (let* ((h (mem-ref a :u32)) (st (logand h 255)) (cnt (ash h -8)))
              (unless (%gc-old-leaf-p st)
                (dotimes (i cnt) (slot (+ a 16 (* 8 i)) a (list st cnt))))
              (setq a (+ a (%census-size st cnt)))))))
    (list :area area :ncards (%gc-read64 (+ w 16)) :old (list (%gc-old-lo) (%gc-old-hi))
          :from f :to tt :size sz :alloc (get-alloc-ptr) :v (nreverse out))))
(defun arena-audit ()
  "Old pointer slots into the LOCK ARENA [base, end): (total on-clean-cards).
   An arena object is not yet in region 0 and will be MOVED by the next
   evacuating collection, so a slot pointing at one must stay on a dirty card."
  (let* ((w (%gc-card-words)) (cards (%gc-read64 w)) (area (%gc-read64 (+ w 8)))
         (lo (%rt-arena-base)) (hi (%rt-arena-end))
         (cbm (%census-cons-bitmap)) (page (%gc-meta-read #x10000E00 (%gc-meta-scale)))
         (a (%gc-old-lo)) (end (%gc-old-hi)) (n 0) (clean 0))
    (flet ((slot (x)
             (let ((v (mem-ref x :u64)))
               (when (and (= 1 (logand v 7)) (>= v lo) (< v hi))
                 (setq n (+ n 1))
                 (unless (= 0 (mem-ref (+ cards (ash (- x area) -9)) :u8))
                   (setq clean (+ clean 1)))))))
      (loop
        (when (>= a end) (return nil))
        (if (%census-cons-p a page cbm)
            (progn (slot a) (slot (+ a 8)) (setq a (+ a 16)))
            (let* ((h (mem-ref a :u32)) (st (logand h 255)) (cnt (ash h -8)))
              (unless (%gc-old-leaf-p st)
                (dotimes (i cnt) (slot (+ a 16 (* 8 i)))))
              (setq a (+ a (%census-size st cnt)))))))
    (list :arena lo hi :slots n :clean clean)))
(defvar *audit-each* nil)               ; diagnosis: card audit after every collection
(defvar *first-bad* nil)
(dotimes (k 40)
  (when (and *audit-each* (not *first-bad*))
    (let ((ca (%gc-card-audit)))
      (unless (= 0 (second ca)) (setq *first-bad* (list :before k ca (card-violations) (arena-audit))))))
  (let ((r (list (force-one) :us (main-check))))
    (unless (and (eq (second r) :us) (equal (third r) '(5000 0)))
      (format t "MAJOR ~d WRONG: ~s~%" k r) (incf *bad*))))
(setq *stop* t)
(sleep 1)
(format t "arena audit ~s~%" (arena-audit))
(when *first-bad* (format t "first bad ~s~%" *first-bad*))
(let ((au (%gc-old-audit)) (ca (%gc-card-audit)))
  (unless (= 0 (second ca))
    (format t "violations (slot obj kind offset): ~s~%" (card-violations)))
  (format t "bad ~d, old audit ~s, card audit ~s~%" *bad* au ca)
  (format t "~a ~s worker rounds ~s~%"
          (if (and (= *bad* 0) (= 0 (second au)) (= 0 (third au)) (= 0 (second ca))
                   (or (= *nthreads* 0) (> (aref *iters* 1) 0)))
              "GC-MAJOR-THREADS: PASS" "GC-MAJOR-THREADS: FAIL")
          (gc-gen-report) *iters*))
