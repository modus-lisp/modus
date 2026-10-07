;;;; draw.lisp — TAP TO CHANGE COLOURS, on Android: test/ios-draw.lisp's program
;;;; over the Android launcher's UI socket.
;;;;
;;;;   host/android/build-apk.sh IMAGE OUT.apk          (this is the default script)
;;;;
;;;; The iOS shim answers pseudo-syscalls 1001-1004 in-process.  On Android the
;;;; image is a child of the app (host/android/modus-launcher.c), so the same four
;;;; calls travel as 16-byte records <op a b c> on fd 3, and 1001 / 1004 read an
;;;; 8-byte reply.  Raw syscalls go through %GC-SAFE-BLOCK-6 (a compiled image
;;;; function; a script's own SYSCALL6 is the interpreter's stub).  aarch64
;;;; Linux: mmap 222, read 63, write 64.

(defun %sc (n a b c d) (%gc-safe-block-6 n a b c d))
(defvar *ui-buf* (%sc 222 0 4096 3 #x22))           ; one anonymous RW page
(defun sys (op a b c)
  (setf (mem-ref *ui-buf* :u32) op
        (mem-ref (+ *ui-buf* 4) :u32) a
        (mem-ref (+ *ui-buf* 8) :u32) b
        (mem-ref (+ *ui-buf* 12) :u32) c)
  (%sc 64 3 *ui-buf* 16 0)
  (if (or (= op 1001) (= op 1004))
      ;; Two :u32 halves: a :u64 MEM-REF reads the word as a TAGGED value
      ;; (a raw 720 comes back as 360), on every port.
      (progn (%sc 63 3 (+ *ui-buf* 16) 8 0)
             (+ (mem-ref (+ *ui-buf* 16) :u32)
                (* 4294967296 (mem-ref (+ *ui-buf* 20) :u32))))
      0))

(defvar *w* (sys 1001 0 0 0))
(defvar *h* (sys 1001 1 0 0))
(defun fill-rect (x y w h rgb)
  (when (< x 0) (setq w (+ w x) x 0))
  (when (< y 0) (setq h (+ h y) y 0))
  (when (and (> w 0) (> h 0))
    (sys 1002 (+ x (* y 65536)) (+ w (* h 65536)) rgb)))
(defun present () (sys 1003 0 0 0))
(defun next-event () (sys 1004 0 0 0))

(defvar *palette* (vector #xE63946 #xF4A261 #xE9C46A #x2A9D8F #x457B9D #x8338EC #xFF006E #x06D6A0))
(defvar *cols* 4)
(defvar *rows* 8)
(defvar *gap* 12)
(defvar *tile-w* (floor (- *w* (* *gap* (+ *cols* 1))) *cols*))
(defvar *tile-h* (floor (- *h* (* *gap* (+ *rows* 1))) *rows*))
(defvar *tiles* (make-array (* *cols* *rows*)))
(dotimes (i (* *cols* *rows*)) (setf (aref *tiles* i) (mod (+ i (* 3 (floor i *cols*))) 8)))

(defun tile-x (c) (+ *gap* (* c (+ *tile-w* *gap*))))
(defun tile-y (r) (+ *gap* (* r (+ *tile-h* *gap*))))
(defun draw-tile (c r)
  (fill-rect (tile-x c) (tile-y r) *tile-w* *tile-h*
             (aref *palette* (aref *tiles* (+ c (* r *cols*))))))
(defun draw-all ()
  (fill-rect 0 0 *w* *h* #x101018)
  (dotimes (r *rows*) (dotimes (c *cols*) (draw-tile c r)))
  (present))

(defun tile-at (x y)
  "The tile index under (X, Y), or NIL in a gap."
  (let ((c (floor (- x *gap*) (+ *tile-w* *gap*)))
        (r (floor (- y *gap*) (+ *tile-h* *gap*))))
    (when (and (>= c 0) (< c *cols*) (>= r 0) (< r *rows*)
               (< (- x (tile-x c)) *tile-w*) (< (- y (tile-y r)) *tile-h*))
      (+ c (* r *cols*)))))

(format t "modus draws on a ~Dx~D ~A screen~%" *w* *h* (machine-type))
(draw-all)
(let ((down-x 0) (down-y 0) (moved nil) (brush 0))
  (loop
    (let ((e (next-event)))
      (if (zerop e)
          (%sleep-ms 8)
          (let ((type (floor e 1099511627776))                 ; >> 40
                (y (logand (floor e 1048576) #xFFFFF))          ; >> 20
                (x (logand e #xFFFFF)))
            (cond
              ((= type 1)                                       ; down
               (setq down-x x down-y y moved nil)
               (let ((i (tile-at x y)))
                 (setq brush (if i (aref *palette* (mod (+ (aref *tiles* i) 3) 8)) #xFFFFFF))))
              ((= type 2)                                       ; move: paint
               (when (or moved (> (+ (abs (- x down-x)) (abs (- y down-y))) 24))
                 (setq moved t)
                 (fill-rect (- x 14) (- y 14) 28 28 brush)
                 (present)))
              ((= type 3)                                       ; up: a tap cycles the tile
               (unless moved
                 (let ((i (tile-at x y)))
                   (when i
                     (setf (aref *tiles* i) (mod (+ (aref *tiles* i) 1) 8))
                     (draw-tile (mod i *cols*) (floor i *cols*))
                     (present))))))))))))
