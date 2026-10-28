;;; q8-kernel.lisp — illatio's Q8_0 matrix kernel on the integer-lane primitives.
;;;
;;; Four rows of a Q8_0 matrix (int8 weights as two's-complement u8, one
;;; f32 scale per 32-block, regrouped four rows at a time) against one
;;; quantized activation vector.  Per block: SDOT each row's 32 products into
;;; s32 lanes, ADDP the four rows' lanes into one vector (lane r = row r's
;;; exact block sum), SCVTF it to floats and accumulate float(sum)*(dw*dx)
;;; in an FP-resident f32x4.  Each lane is the scalar kernel's arithmetic in
;;; its order, so the two must agree to the bit -- that is the check.  It
;;; also prints GMAC/s for both.
;;;
;;;   ./modus --script tests/simd/q8-kernel.lisp            (JIT: NEON arms)
;;;   MODUS_NO_JIT=1 ./modus --script tests/simd/q8-kernel.lisp   (interpreter arms)
(setq *jit-hot-only* nil)
(defun q8-dot-4rows (wq ws4 xq xs cols nb g xo xso out oo)
  (declare (type (simple-array (unsigned-byte 8) (*)) wq xq)
           (type (simple-array single-float (*)) ws4 xs out)
           (type (unsigned-byte 40) cols nb g xo xso oo) (optimize (speed 3) (safety 0)))
  (let ((acc (%f32v-broadcast 0.0)) (p0 (* 4 g cols)) (s0 (* g nb)))
    (declare (type f32v-pack acc) (type (unsigned-byte 40) p0 s0))
    (dotimes (b nb)
      (let* ((w0 (+ p0 (* b 32))) (w1 (+ w0 cols)) (w2 (+ w1 cols)) (w3 (+ w2 cols))
             (xb (+ xo (* b 32))))
        (declare (type (unsigned-byte 40) w0 w1 w2 w3 xb))
        (let ((xa (%vi-ld16 xq xb)) (xc (%vi-ld16 xq (+ xb 16))))
          (declare (type vi-pack xa xc))
          (setq acc
                (%f32v+ acc
                        (%f32v* (%vi-scvtf32
                                 (%vi-addp32
                                  (%vi-addp32 (%vi-sdot (%vi-sdot (%vi-movi8 0) (%vi-ld16 wq w0) xa) (%vi-ld16 wq (+ w0 16)) xc)
                                              (%vi-sdot (%vi-sdot (%vi-movi8 0) (%vi-ld16 wq w1) xa) (%vi-ld16 wq (+ w1 16)) xc))
                                  (%vi-addp32 (%vi-sdot (%vi-sdot (%vi-movi8 0) (%vi-ld16 wq w2) xa) (%vi-ld16 wq (+ w2 16)) xc)
                                              (%vi-sdot (%vi-sdot (%vi-movi8 0) (%vi-ld16 wq w3) xa) (%vi-ld16 wq (+ w3 16)) xc))))
                                (%f32v* (%f32v-ref ws4 (* 4 (+ s0 b))) (%f32v-broadcast-ref xs (+ xso b)))))))))
    (%f32v-set out oo acc)))

(defun q8dot (wq ws row cols xq xs)   ; the scalar reference, one row
  (declare (type (simple-array (unsigned-byte 8) (*)) wq xq) (type (simple-array single-float (*)) ws xs)
           (type fixnum row cols) (optimize (speed 3) (safety 0)))
  (let ((sum 0.0) (wbase (* row cols)) (sbase (* row (ash cols -5))))
    (declare (type single-float sum) (type fixnum wbase sbase))
    (dotimes (b (ash cols -5) sum)
      (let ((acc 0) (wo (+ wbase (* b 32))) (xo (* b 32)))
        (declare (type fixnum acc wo xo))
        (dotimes (j 32)
          (let ((w (aref wq (+ wo j))) (x (aref xq (+ xo j))))
            (declare (type fixnum w x))
            (when (> w 127) (setq w (- w 256))) (when (> x 127) (setq x (- x 256)))
            (setq acc (+ acc (* w x)))))
        (setq sum (+ sum (* (float acc 1.0) (* (aref ws (+ sbase b)) (aref xs b)))))))))

(defvar *rows* (if (uiop:getenv "MODUS_NO_JIT") 64 4864)) (defvar *cols* 896) (defvar *nb* (ash *cols* -5))
(defvar *wq* (let ((a (make-array (* *rows* *cols*) :element-type '(unsigned-byte 8)))) (dotimes (i (length a) a) (setf (aref a i) (mod (* (+ i 1) 7919) 256)))))
(defvar *ws* (let ((a (make-array (* *rows* *nb*) :element-type 'single-float))) (dotimes (i (length a) a) (setf (aref a i) (* 0.001 (+ 1 (mod i 13)))))))
(defvar *ws4* (let ((a (make-array (* *rows* *nb*) :element-type 'single-float)))
                (dotimes (g (floor *rows* 4) a) (dotimes (b *nb*) (dotimes (k 4) (setf (aref a (+ (* 4 (+ (* g *nb*) b)) k)) (aref *ws* (+ (* (+ (* 4 g) k) *nb*) b))))))))
(defvar *xq* (let ((a (make-array *cols* :element-type '(unsigned-byte 8)))) (dotimes (i (length a) a) (setf (aref a i) (mod (* (+ i 3) 131) 256)))))
(defvar *xs* (let ((a (make-array *nb* :element-type 'single-float))) (dotimes (i (length a) a) (setf (aref a i) (* 0.01 (+ 1 (mod i 5)))))))
(defvar *out* (make-array *rows* :element-type 'single-float :initial-element 0.0))
(defvar *ref* (make-array *rows* :element-type 'single-float :initial-element 0.0))

(defun run-neon (reps) (dotimes (k reps) (dotimes (g (floor *rows* 4)) (q8-dot-4rows *wq* *ws4* *xq* *xs* *cols* *nb* g 0 0 *out* (* 4 g)))))
(defun run-scalar (reps) (dotimes (k reps) (dotimes (r *rows*) (setf (aref *ref* r) (q8dot *wq* *ws* r *cols* *xq* *xs*)))))
(defun secs (t0) (/ (- (get-internal-real-time) t0) (float internal-time-units-per-second)))
(defun bench ()
  (let ((t0 (get-internal-real-time))) (run-scalar 1)
    (let ((s (secs t0))) (format t "~&scalar : ~,3F GMAC/s~%" (/ (* 1 *rows* *cols*) s 1e9))))
  (let ((t0 (get-internal-real-time))) (run-neon (if (uiop:getenv "MODUS_NO_JIT") 1 40))
    (let ((s (secs t0))) (format t "~&neon   : ~,3F GMAC/s~%" (/ (* (if (uiop:getenv "MODUS_NO_JIT") 1 40) *rows* *cols*) s 1e9))))
  (let ((same t)) (dotimes (r *rows*) (unless (= (aref *out* r) (aref *ref* r)) (setq same nil)))
    (format t "~&Q8-KERNEL ~:[FAIL: neon differs from scalar~;ALL PASS (bit-identical, ~D rows)~]~%" same *rows*)))
(if (uiop:getenv "MODUS_NO_JIT") (bench) (progn (modus:jit-eager) (modus:jit-eager '(bench))))
