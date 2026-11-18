;;;; bench-defs.lisp -- the workloads of test/dos386/bench.lisp, as definitions only.
;;;;
;;;; Loaded by bench.lisp (SBCL, or a modus image at run time), or compiled INTO
;;;; a modus image by the build-time compiler -- AOT, native like the runtime's
;;;; own functions:
;;;;
;;;;   MODUS_EXTRA_SOURCE=test/dos386/bench-defs.lisp sbcl ... --script mvm/build-i386-cli.lisp
;;;;   qemu-i386-static IMAGE --non-interactive --eval '(modus-int::bench-main 1)'
;;;;
;;;; (baked functions are the kernel's, in MODUS-INT).  BENCH-MAIN's argument
;;;; multiplies every repetition count.  The CLOS workload is bench-clos.lisp,
;;;; always loaded at run time: the build-time compiler has no CLOS (it would
;;;; compile each DEFMETHOD as a DEFUN of the generic function's name).

(defvar *scale* 1)

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

(defun bench-main (scale)
  (setq *scale* scale)
  (run "fib(24)"       1 (lambda () (fib 24)) 46368)
  (run "loop 1e6"      1 (lambda () (loop-sum 1000000)) 806944)
  (run "list 20k"      1 (lambda () (list-work 20000)) 100060907)
  (run "fnv 64K"       4 (lambda () (fnv *octets*)) 429759941)
  (run "hash 5k"       1 (lambda () (hash-work 5000)) 2500)
  (run "float 200k"    1 (lambda () (float-work 200000)) 7.302940021824971d7)
  (run "fact(1000)"    4 (lambda () (mod (fact 1000) 1000000007)) 641419708)
  nil)
