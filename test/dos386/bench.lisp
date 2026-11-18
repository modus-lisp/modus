;;;; bench.lisp -- small portable workloads for comparing modus with SBCL.
;;;;
;;;;   sbcl --script test/dos386/bench.lisp
;;;;   qemu-i386-static modus-i386-cli --script test/dos386/bench.lisp
;;;;
;;;; Each line: NAME  MILLISECONDS-PER-REPETITION  REPS  RESULT.  The result is checked against a known
;;;; answer (BAD if it differs), so a fast wrong answer cannot pass.  Each
;;;; workload runs once untimed first (on modus a DEFUN may go native after
;;;; 64 calls or 10 ms; i386 has no JIT, so there it stays interpreted).
;;;; BENCH_SCALE=N multiplies every repetition count (default 1).

(defvar *scale*
  (let ((s (ignore-errors (sb-ext:posix-getenv "BENCH_SCALE"))))
    (if (and s (plusp (length s))) (parse-integer s) 1)))

(defun now () (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

;; fixnum recursion
(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
;; a counted loop of fixnum arithmetic
(defun loop-sum (n) (let ((s 0)) (dotimes (i n s) (setq s (logand (+ s (* i 7)) #xFFFFF)))))
;; conses: build, reverse, sort, sum
(defun list-work (n)
  (let ((l nil))
    (dotimes (i n) (push (mod (* i 7919) 10007) l))
    (setq l (sort (reverse l) #'<))
    (reduce #'+ l)))
;; octet vector: an FNV-1a over a buffer
(defun fnv (v)
  (declare (type (simple-array (unsigned-byte 8) (*)) v))
  (let ((h 2166136261))
    (dotimes (i (length v) h)
      (setq h (logand (* (logxor h (aref v i)) 16777619) #xFFFFFFFF)))))
;; strings and an EQUAL hash table
(defun hash-work (n)
  (let ((tab (make-hash-table :test 'equal)) (hits 0))
    (dotimes (i n) (setf (gethash (format nil "k~D" i) tab) i))
    (dotimes (i n hits) (when (gethash (format nil "k~D" (* 2 i)) tab) (incf hits)))))
;; doubles
(defun float-work (n)
  (let ((s 0d0)) (dotimes (i n s) (setq s (+ s (sqrt (* (float i 1d0) 1.5d0)))))))
;; bignums
(defun fact (n) (let ((r 1)) (loop for i from 2 to n do (setq r (* r i))) r))
;; CLOS dispatch
(defclass shape () ())
(defclass sq (shape) ((s :initarg :s :reader side)))
(defclass circ (shape) ((r :initarg :r :reader rad)))
(defgeneric area (x))
(defmethod area ((x sq)) (* (side x) (side x)))
(defmethod area ((x circ)) (* 3 (rad x) (rad x)))
(defun clos-work (n)
  (let ((a (make-instance 'sq :s 3)) (b (make-instance 'circ :r 2)) (s 0))
    (dotimes (i n s) (incf s (+ (area a) (area b))))))

(defvar *octets*
  (let ((v (make-array 65536 :element-type '(unsigned-byte 8))))
    (dotimes (i 65536 v) (setf (aref v i) (logand (* i 131) 255)))))

(defun run (name reps thunk want)
  (funcall thunk)
  (let* ((reps (* reps *scale*)) (t0 (now)) (r nil))
    (dotimes (i reps) (setq r (funcall thunk)))
    (let ((dt (- (now) t0)))
      (format t "~14A ~12,4F ms ~6D  ~A~%" name (/ (* dt 1000) reps) reps
              (if (funcall (if (floatp want) (lambda (a b) (< (abs (- a b)) (* 1d-9 (abs b)))) #'equal) r want)
                  "ok" (format nil "BAD ~S" r)))
      (finish-output))))

(run "fib(24)"       1 (lambda () (fib 24)) 46368)
(run "loop 1e6"      1 (lambda () (loop-sum 1000000)) 806944)
(run "list 20k"      1 (lambda () (list-work 20000)) 100060907)
(run "fnv 64K"       4 (lambda () (fnv *octets*)) 429759941)
(run "hash 5k"       1 (lambda () (hash-work 5000)) 2500)
(run "float 200k"    1 (lambda () (float-work 200000)) 7.302940021824971d7)
(run "fact(1000)"    4 (lambda () (mod (fact 1000) 1000000007)) 641419708)
(run "clos 100k"     1 (lambda () (clos-work 100000)) 2100000)
