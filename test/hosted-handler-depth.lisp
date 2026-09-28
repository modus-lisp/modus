;;;; hosted-handler-depth.lisp — A WORKER'S CALL STATE STAYS ITS OWN.
;;;;
;;;;   ./modus --script test/hosted-handler-depth.lisp
;;;;
;;;; Two workers, each 20 HANDLER-CASEs deep, check multiple values, argument
;;;; counts and &rest lists in a loop while the other does the same.  On hosted
;;;; x86-64 three separate bugs each broke that, each a thread writing ANOTHER
;;;; thread's state (measured together as an interpreted capturing closure
;;;; going wrong within ~20 000 calls from two workers — unknown opcodes, stack
;;;; underflows, a thread returning the tail of an old argument list):
;;;;   1. The per-thread window blocks were 4 KB apart, but Linux keeps 512
;;;;      handler frames at window +0x1000..+0x5000: a worker's frames landed
;;;;      on the NEXT worker's multiple-value slots and nargs
;;;;      (net/hosted-sync.lisp %THR-TLS-BLOCK).
;;;;   2. JIT bridge thunks stored nargs to the absolute slot — MAIN's — with no
;;;;      FS override (AArch64: no thread delta); the #'NAME thunk's index slot
;;;;      was shared the same way (mvm/mvm-eval.lisp %JIT-THUNK-EMIT-TAIL).
;;;;   3. The JIT's co-init never set the per-CPU active-region mode, so a JIT
;;;;      page's own GC trampoline and allocation paths on a worker used CPU
;;;;      0's region: main's heap (mvm/build-cli-common.lisp).
;;;; 20 deep puts the frames past +0x1280, well past a 4 KB block, and the
;;;; functions here are JIT'd and call each other through bridges.

;;;; WHAT WOULD MAKE A PASS MEANINGLESS: the nesting not happening (each worker
;;;; reports the depth it reached), or too few checks (each must make many).

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~A~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~A want ~A~%" name got want))))

(%sb-threads-up)
(defvar *stop* nil)
(defvar *depth-reached* (make-array 4 :initial-element 0))

(defun three (a) (values a (* 2 a) (* 3 a)))
(defun count-args (&rest xs) (length xs))

(defun bottom (id)
  "At full depth: check MV and nargs until told to stop."
  (let ((n 0) (bad 0))
    (loop
      (when *stop* (return (list n bad)))
      (multiple-value-bind (a b c) (three (+ id n))
        (unless (and (eql a (+ id n)) (eql b (* 2 (+ id n))) (eql c (* 3 (+ id n))))
          (setq bad (+ bad 1))))
      (unless (eql (count-args 1 2 3 4 5 6 7) 7) (setq bad (+ bad 1)))
      (unless (eql (count-args id) 1) (setq bad (+ bad 1)))
      (setq n (+ n 1)))))

(defun nest (id d)
  (if (= d 0)
      (bottom id)
      (handler-case
          (progn (setf (aref *depth-reached* id) (max (aref *depth-reached* id) (- 21 d)))
                 (nest id (- d 1)))
        (error (e) (list :err (%escape-describe e))))))

(format t "~%=== TWO WORKERS, HANDLER-CASE 20 DEEP EACH ===============~%")
(let ((ths nil))
  (dotimes (w 2)
    (let ((id w)) (setq ths (cons (sb-thread:make-thread (lambda () (nest id 20))) ths))))
  (%sleep-ms 15000)
  (setq *stop* t)
  (let ((rs (reverse (mapcar (function sb-thread:join-thread) ths))))
    (format t "  worker results (checks bad): ~S~%" rs)
    (dotimes (w 2)
      (let ((r (nth w rs)))
        (chk (format nil "worker ~D reached handler depth" w) (aref *depth-reached* w) 20)
        (chk (format nil "worker ~D finished cleanly" w) (and (consp r) (integerp (car r))) t)
        (when (and (consp r) (integerp (car r)))
          (chk (format nil "worker ~D made plenty of checks" w) (> (car r) 1000) t)
          (chk (format nil "worker ~D values/nargs that changed under it" w) (cadr r) 0))))))

(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "HANDLER FRAMES STAY PER-THREAD: PASS" "HANDLER FRAMES STAY PER-THREAD: FAIL"))
