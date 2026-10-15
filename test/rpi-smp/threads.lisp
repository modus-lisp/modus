(defvar *big* nil)

(defun t-sum () (let ((s 0)) (dotimes (i 3000000) (setq s (+ s i))) s))

(defun t-cons () (let ((ok 0)) (dotimes (k 3000) (let ((l nil) (s 0)) (dotimes (i 1000) (setq l (cons i l))) (dolist (x l) (setq s (+ s x))) (when (= s 499500) (setq ok (+ ok 1))))) (list ok (length (make-list 5 :initial-element :x)))))

(defun t-walk () (let ((ok 0) (bad 0)) (dotimes (k 400) (let ((s 0) (junk nil)) (dolist (x *big*) (setq s (+ s x))) (setq junk (cons k junk)) (if (= s 4999950000) (setq ok (+ ok 1)) (setq bad (+ bad 1))))) (list ok bad)))

(defun t-main-churn (n) (let ((before (%gc-epoch))) (dotimes (i n) (%gc-force) (let ((l nil)) (dotimes (j 20000) (setq l (cons j l))))) (list :r0-gcs (- (%gc-epoch) before))))

(defun r0-count () (%gc-meta-read (+ #x10000040 #x20) 2))

(defun t-walk2 () (let ((ok 0) (bad 0) (c0 (r0-count)) (seen 0) (last (r0-count))) (dotimes (k 1000) (let ((s 0) (junk nil)) (dolist (x *big*) (setq s (+ s x)) (setq junk (cons x nil))) (unless (= last (r0-count)) (setq seen (+ seen 1)) (setq last (r0-count))) (if (= s 4999950000) (setq ok (+ ok 1)) (setq bad (+ bad 1))))) (list :ok ok :bad bad :r0-gcs-during (- (r0-count) c0) :changes-seen seen)))
