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

(defun s-mutex () (setq *m* (sb-thread:make-mutex :name "ctr")) (setf (mem-ref #x10380800 :u32) 0) (let ((ths (loop for k below 6 collect (sb-thread:make-thread (lambda () (dotimes (i 1000) (sb-thread:with-mutex (*m*) (let ((v (mem-ref #x10380800 :u32))) (bt-yield) (setf (mem-ref #x10380800 :u32) (+ v 1))))) :done))))) (list (mapcar (function sb-thread:join-thread) ths) (mem-ref #x10380800 :u32))))

(defun s-nested () (let ((ths (loop repeat 3 collect (bt-spawn (function t-sum))))) (mapcar (function bt-join) ths)))

(defun s-nested2 () (sb-thread:join-thread (sb-thread:make-thread (function t-list2))))

(defun s-badclosure () (let ((x (list 1 2))) (handler-case (progn (bt-spawn (lambda () x)) :started) (error () :refused))))
