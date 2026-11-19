;;;; test/fp-high-registers.lisp -- float trees deep enough to use FP vregs 6..11.
;;;;
;;;;   ./modus --script test/fp-high-registers.lisp
;;;;
;;;; translate-x64 maps FP vregs 0-5 to xmm2..7 and 6-11 to xmm8..13 (X64-FPP).
;;;; xmm8 and up need REX in the encoding, and the register-register copy, the
;;;; single-float arithmetic, the f32 lane loads and stores and the 4-lane ops
;;;; built their ModRM byte by hand without it: a vreg past F5 read or wrote
;;;; xmm0..5 instead (xmm0 is the translator's scratch).  Shallow trees never
;;;; got that far, so nothing showed until mill-perf's in-register EXP held seven
;;;; doubles at once and (exp 0d0) came back -5/6.
;;;;
;;;; Measured on main d66762de: the single-float tree is 239.0 instead of 7679 once
;;;; the hot JIT has compiled DEEP-S (rounds 1 and 2), right on the interpreted round 0.
;;;;
;;;; Each tree is right-nested, so every level waits on the one below and the
;;;; register count grows with the depth.  The operands are small integers, so
;;;; every operation is exact and the right answer is integer arithmetic.

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name got want)
  (incf *checks*)
  (let ((ok (= got want)))
    (unless ok (incf *fails*))
    (format t "~:[FAIL~;ok  ~] ~a~@[ got ~a want ~a~]~%" ok name (unless ok (list got want)))))

(defun deep-d (a b c d e f g h i j)
  (declare (double-float a b c d e f g h i j))
  (+ a (* b (+ c (* d (+ e (* f (+ g (* h (+ i j))))))))))
(defun deep-s (a b c d e f g h i j)
  (declare (single-float a b c d e f g h i j))
  (+ a (* b (+ c (* d (+ e (* f (+ g (* h (+ i j))))))))))
(defun deep-mix (a b c d e f g h)
  (declare (double-float a b c d e f g h))
  ;; two deep subtrees live at once
  (- (+ a (* b (+ c (* d e)))) (* f (+ g (* h (+ a b))))))
(defun deep-int (a b c d e f g h i j)
  (+ a (* b (+ c (* d (+ e (* f (+ g (* h (+ i j))))))))))

(let ((args '(1 2 3 4 5 6 7 8 9 10)))
  (let ((want (apply #'deep-int args)))
    (dotimes (k 3)                      ; twice past the hot-JIT threshold too
      (dotimes (i 70)
        (apply #'deep-d (mapcar (lambda (x) (float x 1d0)) args))
        (apply #'deep-s (mapcar (lambda (x) (float x 1f0)) args)))
      (check (format nil "double tree, round ~d" k)
             (apply #'deep-d (mapcar (lambda (x) (float x 1d0)) args)) want)
      (check (format nil "single tree, round ~d" k)
             (apply #'deep-s (mapcar (lambda (x) (float x 1f0)) args)) want))))

(check "two live subtrees"
       (deep-mix 1d0 2d0 3d0 4d0 5d0 6d0 7d0 8d0)
       (- (+ 1 (* 2 (+ 3 (* 4 5)))) (* 6 (+ 7 (* 8 (+ 1 2))))))

(check "exp 0" (exp 0d0) 1)
(check "exp 0 single" (exp 0f0) 1)

(format t "~&fp-high-registers: ~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
