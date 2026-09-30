;;;; ios-draw.lisp — TAP TO CHANGE COLOURS: modus drawing on an iPhone or a watch.
;;;;
;;;;   host/ios/build-ios.sh … (MODUS_IOS_FILES=test/ios-draw.lisp)
;;;;   xcrun devicectl device process launch --device D org.modus-lisp.modus --script @ios-draw.lisp
;;;;
;;;; A grid of coloured tiles.  TAP a tile and it takes the next colour; DRAG
;;;; and you paint.  The screen is the iOS shim's framebuffer
;;;; (host/ios/modus-ui.m), reached through pseudo-syscalls 1001-1004 via
;;;; %GC-SAFE-BLOCK-6, a compiled image function a script can call.  Runs until
;;;; the app is closed.

(defun sys (n a b c) (%gc-safe-block-6 n a b c 0))
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
;; A watch-sized screen (host/watch/modus-watch.swift) gets fewer, bigger tiles.
(defvar *small* (< *w* 600))
(defvar *cols* (if *small* 3 4))
(defvar *rows* (if *small* 4 8))
(defvar *gap* (if *small* 8 12))
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

(format t "modus draws on a ~Dx~D screen~%" *w* *h*)
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
