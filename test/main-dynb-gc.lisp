;;;; test/main-dynb-gc.lisp -- the MAIN thread's bindings survive region-0
;;;; collections (mvm/prelude.lisp, THE MAIN THREAD'S BINDING STACK).
;;;;
;;;;   ./modus --script test/main-dynb-gc.lisp        (hosted x86-64, AArch64)
;;;;
;;;; Main binds specials to fresh lists and churns region 0 until it collects:
;;;; first in LOG mode (no thread yet: main binds shallowly and logs the value
;;;; each binding replaced), then in DEEP mode (after a thread has run: the
;;;; bound value lives ONLY in main's binding stack).  The collector must scan
;;;; that stack (translate-x64 EMIT-MAIN-DYNB-SEG, translate-aarch64
;;;; A64-LOAD-MAIN-DYNB-DELTA).  Measured on the AArch64 CLI with the root scan
;;;; left out: the deep section dies (TYPE-ERROR) 4 of 4.  POSITIVE CONTROL:
;;;; each section requires region 0 to have collected.
(defvar *fail* 0)
(defun chk (name got want)
  (if (equal got want) (format t "  ok   ~A = ~A~%" name got)
      (progn (incf *fail*) (format t "  FAIL ~A: got ~A want ~A~%" name got want))))
(defun r0 () (modus-int::%gc-meta-read (+ (modus-int::%gc-region-0) #x20) (modus-int::%gc-meta-scale)))
(defun churn (n)
  "Allocate (~8 MB a round, bounded) until region 0 has collected N times."
  (let ((g (r0)) (i 0))
    (loop (when (or (>= (- (r0) g) n) (> i 4000)) (return (- (r0) g)))
      (let ((j 0)) (loop (when (>= j 50) (return nil)) (make-array 20000) (incf j)))
      (incf i))))
(defun lsum (l) (let ((s 0)) (dolist (e l) (incf s e)) s))
(defvar *pg* nil)
(defvar *pa* :ga)
(defvar *pb* :gb)
(setq *pg* (make-list 1000 :initial-element 3))
(defun log-mode ()
  (let ((*pa* (make-list 1000 :initial-element 7)) (*pg* (list 1 2)))
    (chk "log: region 0 collected twice" (churn 2) 2)
    (chk "log: bound list" (lsum *pa*) 7000)
    (chk "log: inner *pg*" *pg* '(1 2)))
  (chk "log: *pa* restored" *pa* :ga)
  (chk "log: *pg* global" (lsum *pg*) 3000))
(log-mode)
(sb-thread:join-thread (sb-thread:make-thread (lambda () 1)))
(defun deep-mode ()
  (let ((*pb* (make-list 1000 :initial-element 5)) (*pg* (list 9 9 9)))
    (chk "deep: region 0 collected 3 times" (churn 3) 3)
    (chk "deep: bound list" (lsum *pb*) 5000)
    (chk "deep: inner *pg*" *pg* '(9 9 9))
    (chk "deep: a worker sees the global *pb*"
         (sb-thread:join-thread (sb-thread:make-thread (lambda () *pb*))) :gb))
  (chk "deep: *pb* restored" *pb* :gb)
  (chk "deep: *pg* global" (lsum *pg*) 3000))
(deep-mode)
(format t "~A~%" (if (zerop *fail*) "MAIN-DYNB-GC: PASS" "MAIN-DYNB-GC: FAIL"))
