;; sb-thread + modus:make-channel on the bare Pi's thread slots.  Load after slots.lisp.

(defun s-value () (sb-thread:join-thread (sb-thread:make-thread (lambda () (list 1 2 "three" (percpu-ref 16))))))

(defvar *ch* nil)

(defun s-producer (k) (dotimes (i 200) (modus:channel-send *ch* (list k i (format nil "msg-~D-~D" k i)))) :sent)

(defun s-channel () (setq *ch* (modus:make-channel :capacity 4096)) (let ((ths (loop for k below 3 collect (let ((k k)) (sb-thread:make-thread (lambda () (s-producer k)))))) (seen (make-array 3 :initial-element 0)) (bad 0)) (dotimes (n 600) (let ((m (modus:channel-receive *ch*))) (if (and (consp m) (string= (third m) (format nil "msg-~D-~D" (first m) (second m)))) (setf (aref seen (first m)) (+ 1 (aref seen (first m)))) (setq bad (+ bad 1))))) (list :seen (coerce seen 'list) :bad bad :joined (mapcar (function sb-thread:join-thread) ths))))

(defvar *ping* nil)
(defvar *pong* nil)

(defun s-pinger () (dotimes (i 300) (modus:channel-send *ping* i) (unless (eql (modus:channel-receive *pong*) (* i 2)) (return-from s-pinger :bad))) :ok)
(defun s-ponger () (dotimes (i 300) (modus:channel-send *pong* (* 2 (modus:channel-receive *ping*)))) :ok)

(defun s-pingpong () (setq *ping* (modus:make-channel :capacity 256) *pong* (modus:make-channel :capacity 256)) (let ((a (sb-thread:make-thread (function s-pinger))) (b (sb-thread:make-thread (function s-ponger)))) (list (sb-thread:join-thread a) (sb-thread:join-thread b))))

(defvar *m* nil)

(defun s-mutex () (setq *m* (sb-thread:make-mutex :name "ctr")) (setf (mem-ref #x10380800 :u32) 0) (let ((ths (loop for k below 6 collect (sb-thread:make-thread (lambda () (dotimes (i 1000) (sb-thread:with-mutex (*m*) (let ((v (mem-ref #x10380800 :u32))) (modus-int::bt-yield) (setf (mem-ref #x10380800 :u32) (+ v 1))))) :done))))) (list (mapcar (function sb-thread:join-thread) ths) (mem-ref #x10380800 :u32))))

(defun s-nested () (let ((ths (loop repeat 3 collect (modus-int::bt-spawn (function t-sum))))) (mapcar (function bt-join) ths)))

(defun s-nested2 () (sb-thread:join-thread (sb-thread:make-thread (function t-list2))))

(defun s-badclosure () (let ((x (list 1 2))) (handler-case (progn (modus-int::bt-spawn (lambda () x)) :started) (error () :refused))))

(defun stop-word () (mem-ref #x10380900 :u32))
(defun t-hog () (let ((n 0)) (loop (setq n (+ n 1)) (cons n nil) (unless (zerop (stop-word)) (return n)))))
(defun t-work () (let ((t0 (modus-int::%irt-ns))) (list :sum (t-sum) :ms (floor (- (modus-int::%irt-ns) t0) 1000000))))
(defun preempt-run (no-preempt) (setf (mem-ref #x10380900 :u32) 0) (setf (mem-ref (+ (modus-int::%bt-table) #x20) :u32) (if no-preempt 1 0)) (let* ((hogs (spawn-n 3 (function t-hog))) (work (spawn-n 3 (function t-work))) (t0 (modus-int::%irt-ns)) (done 0)) (loop (setq done (count-if (lambda (i) (zerop (modus-int::%native-thread-alive-p i))) work)) (when (or (= done 3) (> (- (modus-int::%irt-ns) t0) 20000000000)) (return nil)) (modus-int::%sleep-ms 50)) (let ((wait-ms (floor (- (modus-int::%irt-ns) t0) 1000000))) (setf (mem-ref #x10380900 :u32) 1) (list :workers-done done :after-ms wait-ms :work (if (= done 3) (mapcar (function bt-join) work) :stuck) :hogs (mapcar (function bt-join) hogs)))))

(defvar *shared-box* nil)
(defun s-guard-setup () (setq *shared-box* (list :empty)) :ok)
(defun s-guard () (let ((mine (list 1 2 3))) (handler-case (progn (setf (car *shared-box*) mine) :stored) (error (e) (list :refused (subseq (princ-to-string e) 0 40))))))
(defun s-guard2 () (let ((mine (list 1 2 3))) (handler-case (progn (setf (car *shared-box*) mine) :stored) (error () :refused))))
(defun s-print-cond () (princ-to-string (make-condition 'simple-error :format-control "x ~a" :format-arguments (list 1))))

(defun deep (n) (if (= n 0) 0 (+ 1 (deep (- n 1)))))
(defun t-deep () (handler-case (deep 1000000) (error () :overflow)))
(defun t-shallow () (deep 1000))

(defvar *ex* nil)
(defun ex-idle () (setf (mem-ref #x10380A00 :u32) (+ 1 (mem-ref #x10380A00 :u32))))
(defun ex-test () (setf (mem-ref #x10380A00 :u32) 0) (setq *ex* (modus-int::bt-start-executor 1 (function ex-idle))) (let ((a (modus-int::bt-call *ex* (lambda () (+ 1 2)))) (b (modus-int::bt-call *ex* (lambda () (list (percpu-ref 16) "str" (> (mem-ref #x10380A00 :u32) 0))))) (buf (make-array 10 :element-type '(unsigned-byte 8)))) (modus-int::bt-call *ex* (lambda () (dotimes (i 10) (setf (aref buf i) (* i 3))) :filled)) (list a b (coerce buf 'list) (handler-case (modus-int::bt-call *ex* (lambda () (error "boom"))) (error () :call-error)) (let ((n0 (mem-ref #x10380A00 :u32))) (modus-int::%sleep-ms 100) (> (mem-ref #x10380A00 :u32) n0)))))
(defun ex-many () (let ((s 0)) (dotimes (i 200) (setq s (+ s (modus-int::bt-call *ex* (lambda () i))))) s))
