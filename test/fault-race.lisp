;;;; fault-race.lisp -- FAULTS ON SEVERAL THREADS AT ONCE EACH GET THEIR OWN CONDITION.
;;;;
;;;;   timeout 900 ./modus --script test/fault-race.lisp        (FAULT RACE: PASS)
;;;;   FAULT_RACE_ROUNDS=n scales the work (default 1000 per thread).
;;;;
;;;; A hardware fault reaches Lisp in two halves: the signal stub (trap #x0520,
;;;; translate-x64 / translate-aarch64) records WHAT happened -- the signal, or
;;;; #x0B0B for a stack overflow -- and longjmps to the faulting thread's armed
;;;; frame, and that frame's dispatch (%TAKE-PENDING-FAULT, mvm/cl-conditions)
;;;; turns the record into a condition.  A handler-frame push that cannot store
;;;; its frame uses the same record (#x0F0F, HANDLER-STACK-EXHAUSTED).  The
;;;; record was ONE word for the whole process (0x10000CB0), so two threads
;;;; faulting at the same moment read each other's code -- a worker's unmapped
;;;; read came out as STACK-EXHAUSTED because main had just overflowed -- or one
;;;; consumed (and zeroed) the other's, which then matched whatever condition
;;;; was last signalled on it.  Now the record is in each thread's window
;;;; (+0x5610), so main and every worker have their own.
;;;;
;;;; Main and FOUR workers run at the same time, each cycling through four
;;;; kinds, offset by its index, so at every moment different kinds are in
;;;; flight on different threads:
;;;;   STACK    unbounded recursion            -> STACK-EXHAUSTED
;;;;   MEMORY   a read of address 64           -> MEMORY-FAULT-ERROR
;;;;   HSTACK   nested UNWIND-PROTECTs past the per-window test limit
;;;;            (window +0x5608, as test/deep-unwind.lisp)  -> HANDLER-STACK-EXHAUSTED
;;;;   ERROR    (ERROR "plain")                -> SIMPLE-ERROR
;;;; Every attempt's condition must be exactly the kind that thread provoked;
;;;; a mismatch is printed with what came instead.  The process must survive
;;;; and the last line is printed by main.
;;;;
;;;; WHAT WOULD MAKE A PASS MEANINGLESS: the threads not overlapping.  So each
;;;; thread counts the attempts it made while at least one other thread was
;;;; still running, and the test requires most of them to have been made then;
;;;; and a SOLO control first checks each kind on main alone gives its type,
;;;; so a kind that never produced its condition cannot hide behind the race.

(defvar *fail* 0)
(defvar *checks* 0)
(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "  ok   ~A = ~S~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "  FAIL ~A: got ~S want ~S~%" name got want))))
(defun chk-true (name v)
  (setq *checks* (+ *checks* 1))
  (if v (format t "  ok   ~A~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "  FAIL ~A~%" name))))

(defun win-frames ()
  (let ((mt (machine-type)))
    (cond ((string= mt "x86_64") 512)
          ((string= mt "aarch64") 170)
          (t nil))))

(defun set-ext-limit (n)
  "THIS thread's test-only limit on handler-frame extension frames."
  (modus-int::%gc-write64 (+ (modus-int::%tls-self-base) (modus-int::%conv-addr #x10005608)) n))

(defun unbounded (n) (1+ (unbounded (1+ n))))

(defun nest (n lim)
  (unwind-protect (if (< n lim) (nest (+ n 1) lim) :bottom)
    (identity n)))

(defun kind-name (k) (aref #("STACK" "MEMORY" "HSTACK" "ERROR") k))
(defun kind-want (k)
  (aref #("STACK-EXHAUSTED" "MEMORY-FAULT-ERROR" "HANDLER-STACK-EXHAUSTED" "SIMPLE-ERROR") k))

(defun provoke (k)
  "Provoke kind K under a HANDLER-CASE; the name of the condition caught."
  (cond ((= k 0)
         (handler-case (progn (unbounded 0) "RETURNED")
           (condition (c) (symbol-name (type-of c)))))
        ((= k 1)
         (handler-case (progn (mem-ref 64 :u32) "RETURNED")
           (condition (c) (symbol-name (type-of c)))))
        ((= k 2)
         (set-ext-limit 5)
         (let ((r (handler-case (progn (nest 0 (+ (win-frames) 200)) "RETURNED")
                    (condition (c) (symbol-name (type-of c))))))
           (set-ext-limit 0)
           r))
        (t
         (handler-case (progn (error "plain") "RETURNED")
           (condition (c) (symbol-name (type-of c)))))))

(defvar *go* 0)
(defvar *running* 0)                    ; threads still in their loop (fixnum)
(defvar *rounds*
  (let ((v (sb-ext:posix-getenv "FAULT_RACE_ROUNDS")))
    (if (and v (> (length v) 0)) (parse-integer v) 1000)))

(defvar *lock* (sb-thread:make-mutex :name "fault-race"))

(defun racer (idx)
  "IDX's loop: ROUNDS attempts, kind (IDX + i) mod 4.  Returns
   (attempts mismatches overlapped first-mismatches) -- all fixnums or a
   short list made on this thread and handed back by JOIN-THREAD."
  (loop (when (= *go* 1) (return)))
  (let ((bad 0) (overlapped 0) (seen nil) (n 0))
    (dotimes (i *rounds*)
      (let* ((k (mod (+ idx i) 4))
             (got (provoke k)))
        (setq n (+ n 1))
        (when (> *running* 1) (setq overlapped (+ overlapped 1)))
        (unless (string= got (kind-want k))
          (setq bad (+ bad 1))
          (when (< (length seen) 6)
            (setq seen (cons (list (kind-name k) got) seen))))))
    (sb-thread:with-mutex (*lock*) (setq *running* (- *running* 1)))
    (list n bad overlapped (reverse seen))))

(format t "~%=== SOLO: each kind on main alone ===~%")
(unless (win-frames)
  (format t "  NOTE: no handler-frame geometry for ~S~%" (machine-type)))
(dotimes (k 4)
  (chk (format nil "solo ~A" (kind-name k)) (provoke k) (kind-want k)))

(defconstant +workers+ 4)
(format t "~%=== RACE: main + ~D workers, ~D attempts each ===~%" +workers+ *rounds*)
(setq *running* (+ +workers+ 1))
(let ((ths nil))
  (dotimes (i +workers+)
    (let ((idx (+ i 1)))
      (setq ths (cons (sb-thread:make-thread (lambda () (racer idx))) ths))))
  (setq ths (reverse ths))
  (setq *go* 1)
  (let ((results (list (cons "main" (racer 0)))))
    (let ((i 1))
      (dolist (th ths)
        (setq results (cons (cons (format nil "worker ~D" i) (sb-thread:join-thread th)) results))
        (setq i (+ i 1))))
    (setq results (reverse results))
    (let ((total 0) (over 0))
      (dolist (r results)
        (let ((who (first r)) (n (second r)) (bad (third r)) (ov (fourth r)) (seen (fifth r)))
          (setq total (+ total n) over (+ over ov))
          (format t "  ~A: ~D attempts, ~D while others ran, ~D wrong~@[ e.g. ~S~]~%"
                  who n ov bad seen)
          (chk (format nil "~A got its own condition every time" who) bad 0)
          (chk (format nil "~A made every attempt" who) n *rounds*)))
      (chk-true (format nil "the threads overlapped (~D of ~D attempts with another thread running)"
                        over total)
                (> (* 2 over) total)))))

(format t "~%=== THE PROCESS CONTINUES ===~%")
(dotimes (k 4)
  (chk (format nil "after the race, main ~A" (kind-name k)) (provoke k) (kind-want k)))
(format t "~%~D checks, ~D failed~%" *checks* *fail*)
(format t "~A~%" (if (zerop *fail*) "FAULT RACE: PASS" "FAULT RACE: FAIL"))
