;;;; test/mul-split.lisp -- %MUL-SPLIT-LO / %MUL-SPLIT-HI and the runtime
;;;; variable-count ASH fast path, against exact integer arithmetic.
;;;;
;;;;   ./modus --script test/mul-split.lisp            ; JIT (native) arm
;;;;   MUL_SPLIT_MODE=interp ./modus --script test/mul-split.lisp
;;;;
;;;; The INTERP arm sets *JIT-INHIBIT* before anything here is compiled, so
;;;; every function below runs in mvm-interpret: the same bytecode, the other
;;;; back end.  A JIT/interpreter difference is therefore a failure in one of
;;;; them, and the reference (built from * and FLOOR on POW2, never ASH) is a
;;;; third opinion that shares no code with either.

(defvar *interp* (equal (sb-ext:posix-getenv "MUL_SPLIT_MODE") "interp"))
(when *interp* (setq *jit-inhibit* t))

(defvar *checks* 0)
(defvar *fails* 0)

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (unless (equal got want)
    (setq *fails* (+ *fails* 1))
    (when (< *fails* 40)
      (format t "~&FAIL ~a~%   got  ~a~%   want ~a~%" name got want))))

(defun pow2 (n) (let ((r 1)) (dotimes (i n r) (setq r (* r 2)))))
;; modus fixnums are SYMMETRIC: +fixnum-min+ is -(2^62 - 1), so -2^62 is a bignum
(defun fixnum-p (x) (and (integerp x) (<= (- 1 (pow2 62)) x (- (pow2 62) 1))))
(defun ref-lo (a b k) (mod (* a b) (pow2 k)))
(defun ref-hi (a b k) (floor (* a b) (pow2 k)))
(defun ref-ash (v c) (if (>= c 0) (* v (pow2 c)) (floor v (pow2 (- c)))))

;;; One function per K, because K must be a compile-time constant.
(defun lo1  (a b) (%mul-split-lo a b 1))
(defun hi1  (a b) (%mul-split-hi a b 1))
(defun lo26 (a b) (%mul-split-lo a b 26))
(defun hi26 (a b) (%mul-split-hi a b 26))
(defun lo51 (a b) (%mul-split-lo a b 51))
(defun hi51 (a b) (%mul-split-hi a b 51))
(defun lo52 (a b) (%mul-split-lo a b 52))
(defun hi52 (a b) (%mul-split-hi a b 52))
(defun lo62 (a b) (%mul-split-lo a b 62))
(defun hi62 (a b) (%mul-split-hi a b 62))
;;; A variable K is not an intrinsic: it must compile the portable definition.
(defun lo-var (a b k) (%mul-split-lo a b k))
(defun hi-var (a b k) (%mul-split-hi a b k))
;;; The result used inside a larger expression with live temporaries, so a
;;; clobbered register would show up as a wrong SUM, not just a wrong value.
(defun mac51 (a b c d)
  (let ((lo 7) (hi 11))
    (setq lo (+ lo (%mul-split-lo a b 51) (%mul-split-lo c d 51))
          hi (+ hi (%mul-split-hi a b 51) (%mul-split-hi c d 51)))
    (list lo hi)))

(defun var-ash (v c) (ash v c))
(defun var-ash-in-expr (v c w) (+ w (ash v c) (* 3 w)))

(defvar *edge*
  (list 0 1 -1 2 -2 3 -3 12345 -98765
        (- (pow2 26) 1) (pow2 26) (- (pow2 51) 1) (pow2 51) (- (pow2 51))
        (+ (pow2 52) 12345) (- (pow2 52)) (- (pow2 54) 1)
        (pow2 60) (- (pow2 60)) (- (pow2 62) 1) (- 1 (pow2 62))))

(defvar *rng* 88172645463325252)
(defun rnd (bits)
  ;; xorshift-ish LCG on a 61-bit state; deterministic, no RANDOM, no ASH.
  (setq *rng* (mod (+ (* *rng* 6364136223846793005) 1442695040888963407) (pow2 61)))
  (let ((v (mod *rng* (pow2 bits))))
    (if (evenp (floor *rng* (pow2 59))) v (- v))))

(defun check-split (name f a b k hi-p)
  (let ((want (if hi-p (ref-hi a b k) (ref-lo a b k))))
    (when (and (fixnum-p a) (fixnum-p b) (fixnum-p want))
      (chk (format nil "~a ~a ~a" name a b) (funcall f a b) want))))

(defun run-split ()
  (let ((pairs nil))
    (dolist (a *edge*) (dolist (b *edge*) (push (cons a b) pairs)))
    (dotimes (i 3000) (push (cons (rnd (+ 1 (mod i 61))) (rnd (+ 1 (mod (* 7 i) 61)))) pairs))
    (dolist (p pairs)
      (let ((a (car p)) (b (cdr p)))
        (check-split "lo1"  (function lo1)  a b 1  nil) (check-split "hi1"  (function hi1)  a b 1  t)
        (check-split "lo26" (function lo26) a b 26 nil) (check-split "hi26" (function hi26) a b 26 t)
        (check-split "lo51" (function lo51) a b 51 nil) (check-split "hi51" (function hi51) a b 51 t)
        (check-split "lo52" (function lo52) a b 52 nil) (check-split "hi52" (function hi52) a b 52 t)
        (check-split "lo62" (function lo62) a b 62 nil) (check-split "hi62" (function hi62) a b 62 t)))
    ;; variable K: the portable path, exact for any integers
    (dolist (k (list 1 7 51 64 100))
      (dolist (a (list 3 (- (pow2 60)) (+ (pow2 70) 5)))
        (chk (format nil "lo-var ~a ~a" a k) (lo-var a 1234567 k) (ref-lo a 1234567 k))
        (chk (format nil "hi-var ~a ~a" a k) (hi-var a 1234567 k) (ref-hi a 1234567 k))))
    ;; inside an expression with live temps
    (dotimes (i 500)
      (let ((a (rnd 52)) (b (rnd 56)) (c (rnd 52)) (d (rnd 56)))
        (chk (format nil "mac51 ~a ~a ~a ~a" a b c d) (mac51 a b c d)
             (list (+ 7 (ref-lo a b 51) (ref-lo c d 51))
                   (+ 11 (ref-hi a b 51) (ref-hi c d 51))))))))

(defun run-ash ()
  (let ((vals (append *edge*
                      (list (pow2 70) (- 3 (pow2 70)) (+ (pow2 64) 1) (- (pow2 64)) (pow2 200))))
        (counts (list -300 -200 -128 -100 -70 -65 -64 -63 -62 -61 -60 -52 -51 -33 -32 -31 -26
                      -13 -7 -2 -1 0 1 2 7 13 26 31 32 33 51 52 60 61 62 63 64 65 70 100 128)))
    (dolist (v vals)
      (dolist (c counts)
        (chk (format nil "ash ~a ~a" v c) (var-ash v c) (ref-ash v c))
        (chk (format nil "ash-expr ~a ~a" v c) (var-ash-in-expr v c 5) (+ 5 (ref-ash v c) 15))))
    (dotimes (i 3000)
      (let ((v (rnd (+ 1 (mod i 61)))) (c (- (mod (* 13 i) 140) 70)))
        (chk (format nil "ash-r ~a ~a" v c) (var-ash v c) (ref-ash v c))))
    ;; a non-integer still signals, exactly as before
    (chk "ash float value signals" (handler-case (progn (var-ash 1.5 3) :no-error) (error () :error)) :error)
    (chk "ash float count signals" (handler-case (progn (var-ash 3 2.0) :no-error) (error () :error)) :error)))

(defun run-all ()
  (run-split)
  (run-ash)
  (list *checks* *fails*))

(defvar *result* (if *interp* (run-all) (jit-eager (quote (run-all)))))
;; The JIT arm is only evidence if the intrinsic functions really are native.
(unless *interp*
  (dolist (nm (list "LO51" "HI51" "MAC51" "VAR-ASH"))
    (chk (format nil "~a is native" nm)
         (or (modus-int::%jit-fn-native-p nm) (modus-int::%jit-fn-native-p (concatenate (quote string) "COMMON-LISP-USER::" nm)))
         t)))
(setq *result* (list *checks* *fails*))
(format t "~&MUL-SPLIT ~a: ~a checks, ~a failed  (jit-fallbacks ~a)~%"
        (if *interp* "interp" "jit") (car *result*) (cadr *result*)
        (handler-case *jit-fallback-count* (error () :unbound)))
(sys-exit (if (zerop (cadr *result*)) 0 1))
