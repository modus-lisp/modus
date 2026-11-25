;;;; test/ir-immediates.lisp -- constant operands folded into the IR's immediate forms.
;;;;   modus --script test/ir-immediates.lisp   => IR-IMMEDIATES PASS
;;;;
;;;; On aarch64 an (:LI T W) feeding an ADD / SUB / AND / OR / XOR becomes ADD-IMM / AND-IMM /
;;;; OR-IMM / XOR-IMM (%IR-FOLD-IMMEDIATES), encoded in the instruction when it can be (ADD/SUB
;;;; imm12, imm12 << 12, a logical bitmask) and loaded into a scratch register when not.  Each
;;;; function below is called cold (the bytecode interpreter) and then past the JIT threshold
;;;; (native code); both must match the same arithmetic done through a variable operand, which
;;;; is never folded, and the literal answers (SBCL's).
(defvar *fails* 0)
(defun check (name got want)
  (if (equal got want)
      (format t "ok   ~A~%" name)
      (progn (setq *fails* (+ *fails* 1)) (format t "FAIL ~A: ~S, want ~S~%" name got want))))

(defun folded (x)
  (declare (type (signed-byte 40) x) (optimize (speed 3) (safety 0)))
  (list (+ x 3) (- x 3) (+ 3 x) (+ x 2047) (- x 2048) (+ x 4096) (+ x 8192000) (- x 8192000)
        (+ x 300001) (- x 300001) (+ x -5)
        (logand x 255) (logand 255 x) (logand x #xFFFFFFFF) (logand x #xFF00) (logand x -16)
        (logand x #x5555555555) (logand x 123456789)
        (logior x 1) (logior x #xF0) (logior x #x30000000) (logior x 77777)
        (logxor x #xFF) (logxor x -1) (logxor x #x0F0F0F0F) (logxor x 999)))

(defvar *ks* (list 3 3 3 2047 2048 4096 8192000 8192000 300001 300001 -5
                   255 255 #xFFFFFFFF #xFF00 -16 #x5555555555 123456789
                   1 #xF0 #x30000000 77777 #xFF -1 #x0F0F0F0F 999))
(defun by-variable (x)
  (let ((k *ks*))
    (flet ((n () (pop k)))
      (list (+ x (n)) (- x (n)) (+ (n) x) (+ x (n)) (- x (n)) (+ x (n)) (+ x (n)) (- x (n))
            (+ x (n)) (- x (n)) (+ x (n))
            (logand x (n)) (logand (n) x) (logand x (n)) (logand x (n)) (logand x (n))
            (logand x (n)) (logand x (n))
            (logior x (n)) (logior x (n)) (logior x (n)) (logior x (n))
            (logxor x (n)) (logxor x (n)) (logxor x (n)) (logxor x (n))))))

(defvar *xs* (list 0 1 -1 12345 -12345 #xFFFFFFFF #x123456789 (- #x123456789) 4095 -4096))

(check "cold" (mapcar #'folded *xs*) (mapcar #'by-variable *xs*))
(dotimes (i 200) (folded i))
(check "warm" (mapcar #'folded *xs*) (mapcar #'by-variable *xs*))
(check "literal" (folded 12345)
       '(12348 12342 12348 14392 10297 16441 8204345 -8179655 312346 -287656 12340
         57 57 12345 12288 12336 4113 17 12345 12537 805318713 81913 12486 -12346 252657462 13278))
;; a loop's index arithmetic and mask, the shape of the string hash
(defun mix (v)
  (declare (simple-vector v) (optimize (speed 3)))
  (let ((h 2166136261) (i 0))
    (declare (type (unsigned-byte 32) h) (type (unsigned-byte 56) i))
    (loop (when (>= (+ i 1) (length v)) (return h))
          (setq h (logand (* (logxor h (the (unsigned-byte 32) (svref v (+ i 1)))) 16777619) #xFFFFFFFF))
          (setq i (+ i 1)))))
(defvar *v* (let ((v (make-array 50))) (dotimes (i 50 v) (setf (svref v i) (* i 7919)))))
(check "mix-cold" (mix *v*) 1161759758)
(dotimes (i 100) (mix *v*))
(check "mix-warm" (mix *v*) 1161759758)

(format t "~&IR-IMMEDIATES ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
