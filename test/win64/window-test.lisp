;;;; window-test.lisp -- a native window from modus.exe (net/win32-window.lisp), in the guest.
;;;;
;;;; Opens a 320x240 window showing a gradient with a magenta 6x6 marker at its client
;;;; origin (window-test-drive.py finds the window by it), logs every event to
;;;; events.txt, draws a yellow 8x8 square where the left button goes down (a partial
;;;; present), and ends on `q', on close, or after 120 s.
(defvar *log* (open "events.txt" :direction :output :if-exists :supersede))
(defun log-line (x) (format *log* "~S~%" x) (finish-output *log*))
(defun fill-rect (px w x0 y0 x1 y1 c)
  (do ((y (max 0 y0) (+ y 1))) ((>= y y1))
    (do ((x (max 0 x0) (+ x 1))) ((>= x x1))
      (setf (aref px (+ (* y w) x)) c))))
(let* ((w 320) (h 240) (px (make-array (* w h) :element-type '(unsigned-byte 32))))
  (dotimes (y h)
    (dotimes (x w)
      (setf (aref px (+ (* y w) x))
            (logior (ash (floor (* x 255) w) 16) (ash (floor (* y 255) h) 8) 64))))
  (fill-rect px w 0 0 6 6 #xFF00FF)
  (let ((win (modus-int::w32-window-open w h "modus window test")))
    (log-line (list :open (if win t nil)))
    (when win
      (modus-int::w32-window-present win px w 0 0 w h)
      (let ((t0 (get-internal-real-time)) (done nil))
        (loop
          (when (or done (> (- (get-internal-real-time) t0) (* 120 internal-time-units-per-second)))
            (return nil))
          (dolist (e (modus-int::w32-window-events win 50))
            (log-line e)
            (case (first e)
              (:paint (modus-int::w32-window-present win px w 0 0 w h))
              (:close (setq done t))
              (:pointer
               (when (logtest (second e) 1)
                 (let ((x (third e)) (y (fourth e)))
                   (fill-rect px w (- x 4) (- y 4) (+ x 4) (+ y 4) #xFFFF00)
                   (modus-int::w32-window-present win px w (- x 4) (- y 4) (+ x 4) (+ y 4)))))
              (:key (when (and (second e) (eql (third e) 113)) (setq done t)))))))
      (modus-int::w32-window-close win)
      (log-line :closed))))
(close *log*)
