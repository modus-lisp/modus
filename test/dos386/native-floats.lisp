;;;; native-floats.lisp -- unboxed float trees through the i386 native tier.
;;;;
;;;;   sbcl --script test/dos386/native-floats.lisp > want.txt
;;;;   qemu-i386-static IMAGE --script test/dos386/native-floats.lisp > got.txt
;;;;   diff want.txt got.txt
;;;;
;;;; Functions with declared single / double operands compile to the FP-register
;;;; class (fp-* / fpd-*: unbox, arithmetic, literals, compares, abs, min/max,
;;;; the four roundings, single <-> double, f32 lanes, scale-float), which the
;;;; tier translates (translate-i386 I386-TRANSLATE-FP-REG).  Every result is
;;;; printed EXACTLY (INTEGER-DECODE-FLOAT, or the integer), so a diff against
;;;; SBCL is a bit-for-bit check.  On modus the last line says how many modules
;;;; went native and lists any refused, so "not translated" cannot pass silently.

(defun d-add (a b) (declare (double-float a b)) (+ a b))
(defun d-sub (a b) (declare (double-float a b)) (- a b))
(defun d-mul (a b) (declare (double-float a b)) (* a b))
(defun d-div (a b) (declare (double-float a b)) (/ a b))
(defun d-tree (a b c) (declare (double-float a b c)) (- (* a b) (/ c (+ a 3.25d0))))
(defun d-itof (i) (declare (fixnum i)) (+ (float i 1d0) 0.5d0))
(defun d-lt (a b) (declare (double-float a b)) (if (< a b) 1 0))
(defun d-le (a b) (declare (double-float a b)) (if (<= a b) 1 0))
(defun d-abs (a b) (declare (double-float a b)) (+ (abs a) b))
(defun d-max (a b) (declare (double-float a b)) (+ (max a b) 0.25d0))
(defun d-min (a b) (declare (double-float a b)) (+ (min a b) 0.25d0))
(defun d-round (a) (declare (double-float a)) (the fixnum (round a)))
(defun d-trunc (a) (declare (double-float a)) (the fixnum (truncate a)))
(defun d-floor (a) (declare (double-float a)) (the fixnum (floor a)))
(defun d-ceil (a) (declare (double-float a)) (the fixnum (ceiling a)))
(defun d-scale (a k) (declare (double-float a) (fixnum k)) (scale-float a k))
(defun d-sum (v) (declare (type (simple-array double-float (*)) v))
  (let ((s 0d0)) (declare (double-float s)) (dotimes (i (length v) s) (setq s (+ s (* (aref v i) 1.5d0))))))

(defun s-add (a b) (declare (single-float a b)) (+ a b))
(defun s-mul (a b) (declare (single-float a b)) (* a b))
(defun s-div (a b) (declare (single-float a b)) (/ a b))
(defun s-tree (a b c) (declare (single-float a b c)) (- (* a b) (/ c (+ a 2.5f0))))
(defun s-lt (a b) (declare (single-float a b)) (if (< a b) 1 0))
(defun s-le (a b) (declare (single-float a b)) (if (<= a b) 1 0))
(defun s-abs (a b) (declare (single-float a b)) (+ (abs a) b))
(defun s-max (a b) (declare (single-float a b)) (+ (max a b) 0.25f0))
(defun s-min (a b) (declare (single-float a b)) (+ (min a b) 0.25f0))
(defun s-round (a) (declare (single-float a)) (the fixnum (round a)))
(defun s-floor (a) (declare (single-float a)) (the fixnum (floor a)))
(defun s-ceil (a) (declare (single-float a)) (the fixnum (ceiling a)))
(defun s-to-d (a b) (declare (single-float a) (double-float b)) (+ (float a 1d0) b))
(defun s-sum (v) (declare (type (simple-array single-float (*)) v))
  (let ((s 0f0)) (declare (single-float s)) (dotimes (i (length v) s) (setq s (+ s (* (aref v i) 2f0))))))
(defun s-fill (v x) (declare (type (simple-array single-float (*)) v) (single-float x))
  (dotimes (i (length v) v) (setf (aref v i) (* x (float i 1f0)))))

(defun show (name x)
  (format t "~A ~S~%" name
          (if (floatp x)
              (multiple-value-list (integer-decode-float x))
              x)))

;; denormals built with SCALE-FLOAT: SBCL's reader misreads some subnormal
;; literals (4.9d-324 reads as 0.0), so a literal would test the readers
(defvar *ds* (list 0d0 -0d0 1d0 -2.5d0 3.141592653589793d0 1d30 -1d-5
                   (scale-float 0.7d0 -1025) (scale-float 1d0 -1074)
                   1.7976931348623157d308 0.1d0 -123456.789d0))
(defvar *ss* (list 0f0 -0f0 1f0 -2.5f0 3.1415927f0 1f30 -1f-5 0.1f0 -12345.678f0 1.1754944f-38))

(dolist (a *ds*)
  (dolist (b *ds*)
    ;; both huge: the sum overflows, which SBCL traps
    (unless (and (> (abs a) 1d300) (> (abs b) 1d300))
      (show "d-add" (d-add a b)) (show "d-sub" (d-sub a b)))
    (when (< (abs a) 1d300) (when (< (abs b) 1d300) (show "d-mul" (d-mul a b))))
    (unless (zerop b) (when (< (abs a) 1d300) (when (> (abs b) 1d-300) (show "d-div" (d-div a b)))))
    (show "d-lt" (d-lt a b)) (show "d-le" (d-le a b))
    (show "d-max" (d-max a b)) (show "d-min" (d-min a b))
    (when (< (abs a) 1d300) (show "d-abs" (d-abs a b)))
    (when (and (< (abs a) 1d100) (< (abs b) 1d100)) (show "d-tree" (d-tree a b a)))))
(dolist (a *ds*)
  (when (< (abs a) 1d8)
    (show "d-round" (d-round a)) (show "d-trunc" (d-trunc a))
    (show "d-floor" (d-floor a)) (show "d-ceil" (d-ceil a)))
  ;; not denormals: SBCL 2.2.9's SCALE-FLOAT of one is wrong ((scale-float
  ;; 2^-1074 10) => ~2^-1012), so it could not be the reference there
  (when (and (< (abs a) 1d290) (or (zerop a) (> (abs a) 1d-300)))
    (show "d-scale+" (d-scale a 10)) (show "d-scale-" (d-scale a -40))))
(dolist (r '(2.5d0 -2.5d0 3.5d0 0.5d0 -0.5d0 1.49999d0 -7.75d0))
  (show "d-round" (d-round r)) (show "d-floor" (d-floor r)) (show "d-ceil" (d-ceil r)))
(dolist (i '(0 1 -1 12345 -536870912 536870911)) (show "d-itof" (d-itof i)))
(show "d-sum" (d-sum (make-array 5 :element-type 'double-float :initial-contents '(1d0 2.5d0 -3d0 0.1d0 7d0))))

(dolist (a *ss*)
  (dolist (b *ss*)
    (show "s-add" (s-add a b))
    (when (and (< (abs a) 1f15) (< (abs b) 1f15)) (show "s-mul" (s-mul a b)))
    (unless (zerop b) (when (and (< (abs a) 1f15) (> (abs b) 1f-15)) (show "s-div" (s-div a b))))
    (show "s-lt" (s-lt a b)) (show "s-le" (s-le a b))
    (show "s-max" (s-max a b)) (show "s-min" (s-min a b)) (show "s-abs" (s-abs a b))
    ;; s-tree divides by (+ a 2.5)
    (when (and (< (abs a) 1f10) (< (abs b) 1f10) (/= a -2.5f0)) (show "s-tree" (s-tree a b a)))
    (show "s-to-d" (s-to-d a 0.5d0))))
(dolist (r '(2.5f0 -2.5f0 3.5f0 0.5f0 -7.75f0 123.4f0))
  (show "s-round" (s-round r)) (show "s-floor" (s-floor r)) (show "s-ceil" (s-ceil r)))
(let ((v (make-array 6 :element-type 'single-float :initial-element 0f0)))
  (s-fill v 1.25f0)
  (show "s-sum" (s-sum v))
  (dotimes (i 6) (show "s-fill" (aref v i))))

#+modus
(format t "native ~D refused ~S~%" modus-int::*n386-translated* modus-int::*n386-refused*)
