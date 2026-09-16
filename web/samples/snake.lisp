;;;; snake.lisp - a WebGL game driven from Lisp.  (load "gui.lisp") first.
;;;;   (snake)     ; arrow keys to steer, Space to restart, Esc to quit.
;;;;
;;;; The page runs a tiny colored-quad renderer on a real WebGL canvas; this
;;;; file runs the game.  Each frame the page posts a "tick DT" event and the
;;;; Lisp side advances the simulation, then ships one "sprites" command - a
;;;; list of "x,y,w,h,r,g,b" quads in canvas pixels - back to the GPU.

(defun %parse-int (s) (or (parse-integer s :junk-allowed t) 0))

(defun %member-cell (c lst)
  (dolist (x lst nil) (when (and (= (car x) (car c)) (= (cdr x) (cdr c))) (return t))))

(defun %snake-food (body g)
  (let ((c nil))
    (loop (setq c (cons (random g) (random g)))
      (when (not (%member-cell c body)) (return c)))))

(defun %cell-sprite (col row cell r g b)
  (concatenate 'string
    (%num (* col cell)) "," (%num (* row cell)) ","
    (%num (- cell 1)) "," (%num (- cell 1)) ","
    (%num r) "," (%num g) "," (%num b) ";"))

(defun %snake-render (body food cell)
  (let ((s (%cell-sprite (car food) (cdr food) cell 230 70 70)) (head t))
    (dolist (c body)
      (setq s (concatenate 'string s
                (%cell-sprite (car c) (cdr c) cell
                              (if head 120 60) (if head 240 200) 130)))
      (setq head nil))
    (gui-sprites s)))

(defun %snake-start () (list (cons 10 10) (cons 9 10) (cons 8 10)))

(defun snake ()
  (let ((g 20) (cell 24))
    (gui-reset)
    (gui-panel "Snake - arrow keys to steer, Space to restart, Esc to quit")
    (gui-canvas (* g cell) (* g cell))
    (gui-bg 18 18 26)
    (gui-keys t)
    (gui-el "score" "root" "div")
    (gui-style "score" "text-align:center;color:#ddd;font:16px system-ui,sans-serif;margin-top:10px")
    (let ((body (%snake-start)) (dir (cons 1 0))
          (alive t) (score 0) (done nil) (food nil))
      (setq food (%snake-food body g))
      (gui-text "score" "score: 0")
      (%snake-render body food cell)
      (loop
        (when done (gui-keys nil) (gui-close) (return :bye))
        (gui-wait 120)                             ; one frame: ~120 ms, or a key
        (dolist (ev (gui-events))
          (cond
            ((key-down-p ev "Escape") (setq done t))
            ((and alive (key-down-p ev "ArrowUp")    (/= (cdr dir) 1))  (setq dir (cons 0 -1)))
            ((and alive (key-down-p ev "ArrowDown")  (/= (cdr dir) -1)) (setq dir (cons 0 1)))
            ((and alive (key-down-p ev "ArrowLeft")  (/= (car dir) 1))  (setq dir (cons -1 0)))
            ((and alive (key-down-p ev "ArrowRight") (/= (car dir) -1)) (setq dir (cons 1 0)))
            ((and (not alive) (key-down-p ev "Space"))
             (setq body (%snake-start) dir (cons 1 0) score 0 alive t)
             (setq food (%snake-food body g))
             (gui-text "score" "score: 0")
             (%snake-render body food cell))))
        (when alive
          (let* ((h (car body))
                 (nh (cons (+ (car h) (car dir)) (+ (cdr h) (cdr dir)))))
            (if (or (< (car nh) 0) (>= (car nh) g) (< (cdr nh) 0) (>= (cdr nh) g)
                    (%member-cell nh body))
                (progn (setq alive nil)
                  (gui-text "score"
                    (concatenate 'string "game over - score " (%num score)
                                 " - press Space to restart")))
                (progn
                  (setq body (cons nh body))
                  (if (and (= (car nh) (car food)) (= (cdr nh) (cdr food)))
                      (progn (setq score (+ score 1))
                        (setq food (%snake-food body g))
                        (gui-text "score" (concatenate 'string "score: " (%num score))))
                      (setq body (butlast body)))
                  (%snake-render body food cell)))))))))

(format t "snake.lisp loaded - run (snake)~%")
