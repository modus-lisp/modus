(defvar *big* nil)

(defun t-sum () (let ((s 0)) (dotimes (i 3000000) (setq s (+ s i))) s))

(defun r0-count () (modus-int::%gc-meta-read (+ #x10000040 #x20) 2))

(defun t-walk2 () (let ((ok 0) (bad 0) (c0 (r0-count)) (seen 0) (last (r0-count))) (dotimes (k 3000) (let ((s 0) (junk nil)) (dolist (x *big*) (setq s (+ s x)) (setq junk (cons x nil))) (unless (= last (r0-count)) (setq seen (+ seen 1)) (setq last (r0-count))) (if (= s 4999950000) (setq ok (+ ok 1)) (setq bad (+ bad 1))))) (list :ok ok :bad bad :r0-gcs-during (- (r0-count) c0) :changes-seen seen)))

(defun t-main-churn (n) (let ((before (modus-int::%gc-epoch))) (dotimes (i n) (modus-int::%gc-force) (let ((l nil)) (dotimes (j 20000) (setq l (cons j l))))) (list :r0-gcs (- (modus-int::%gc-epoch) before))))

(defun my-gcs () (let ((rcb (modus-int::%gc-read64 (+ (modus-int::%bt-rec (percpu-ref 16)) #x40)))) (modus-int::%gc-meta-read (+ rcb #x20) 2)))

(defun chain-sum (l) (let ((s 0)) (dolist (x l) (setq s (+ s x))) s))

(defvar *hide* nil)

(defun t-chain () (let ((l (loop for i below 2000 collect i)) (ok 0) (bad 0) (g0 (my-gcs)) (rec (modus-int::%bt-rec (percpu-ref 16)))) (dotimes (k 200) (let ((junk nil)) (dotimes (j 30000) (setq junk (cons j nil)))) (when *hide* (modus-int::%gc-write64 (+ rec #x40) 8)) (modus-int::bt-yield) (when *hide* (modus-int::%gc-write64 (+ rec #x40) (modus-int::%bt-rcb (mem-ref (+ rec #x20) :u32)))) (if (= (chain-sum l) 1999000) (setq ok (+ ok 1)) (setq bad (+ bad 1)))) (list :core (mem-ref (+ rec #x20) :u32) :ok ok :bad bad :gcs (- (my-gcs) g0))))

(defvar *cell* 0)
(defvar *ctr* #x10380800)

(defun t-locked () (dotimes (i 2000) (modus-int::%mutex-lock *cell*) (let ((v (mem-ref *ctr* :u32))) (modus-int::bt-yield) (setf (mem-ref *ctr* :u32) (+ v 1))) (modus-int::%mutex-unlock *cell*)) :done)

(defun t-unlocked () (dotimes (i 2000) (let ((v (mem-ref *ctr* :u32))) (modus-int::bt-yield) (setf (mem-ref *ctr* :u32) (+ v 1)))) :done)

(defun t-sleep () (let ((t0 (modus-int::%irt-ns))) (modus-int::%sleep-ms 300) (floor (- (modus-int::%irt-ns) t0) 1000000)))

(defun spawn-n (n fn) (loop for i below n collect (modus-int::bt-spawn fn)))
(defun join-all (l) (mapcar (function bt-join) l))

(defun t-list () (list :core (mem-ref (+ (modus-int::%bt-rec (percpu-ref 16)) #x20) :u32) "a string" (loop for i below 50 collect (* i i))))

(defun t-list2 () (list "a string" (loop for i below 50 collect (* i i))))

(defun res-bytes (i) (let* ((rec (modus-int::%bt-rec i)) (n (modus-int::%gc-read64 (+ rec #x18))) (buf (modus-int::%bt-result-buf i))) (list (modus-int::%gc-read64 (+ rec #x28)) n (loop for k below (min n 400) collect (mem-ref (+ buf k) :u8)))))

(defun main-bytes (v) (let ((out (%chan-encode v))) (list (cdr out) (loop for k below (min (cdr out) 400) collect (aref (car out) k)))))

(defun churn (n) (let ((junk nil)) (dotimes (j n) (setq junk (cons j nil))) nil))

(defun t-intern () (let* ((me (percpu-ref 16)) (syms (loop for k below 300 collect (intern (format nil "BT-SYM-~D-~D" me k)))) (bad 0)) (dotimes (r 20) (churn 100000) (modus-int::bt-yield) (let ((k 0)) (dolist (s syms) (unless (string= (symbol-name s) (format nil "BT-SYM-~D-~D" me k)) (setq bad (+ bad 1))) (setq k (+ k 1))))) (list :me me :bad bad :gcs (my-gcs))))

(defvar *sp* :global)

(defun t-special () (let ((ok 0)) (dotimes (r 50) (let ((*sp* (list r (percpu-ref 16) "x"))) (churn 50000) (modus-int::bt-yield) (when (and (consp *sp*) (= (car *sp*) r)) (setq ok (+ ok 1))))) (list :ok ok :after (if (eq *sp* :global) :global :leaked))))

(defun intern-check (n) (let ((found 0)) (dolist (me '(4 5 6 7 8 9)) (dotimes (k n) (when (find-symbol (format nil "BT-SYM-~D-~D" me k)) (setq found (+ found 1))))) found))
