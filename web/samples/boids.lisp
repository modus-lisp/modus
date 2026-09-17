;;;; boids.lisp - 3D flocking (Reynolds boids), drawn with instanced cl-opengl
;;;; draws.  Needs gui.lisp + opengl.lisp (the :gl bridge) loaded first.
;;;;
;;;; The hard part for a bridge like ours is that boids are DYNAMIC: every boid
;;;; moves every frame and is drawn with its own transform, so none of the
;;;; static-geometry tricks apply.  We keep the Lisp side cheap two ways:
;;;;   * the boid shape is uploaded ONCE as a display list, drawn many times;
;;;;   * the page holds proj*view and multiplies each boid's model matrix itself
;;;;     (gl:pv! + gl:draw-model), so Lisp never does a per-boid matrix multiply
;;;;     -- it just assembles a [right|up|forward|pos] matrix from the heading.
;;;; What's left on the Lisp side is the real cost: the O(n^2) flocking sim.
;;;; Everything is double-float (d0) -- single-float is ~8x slower on this VM.

(in-package :cl-user)

(defparameter *nb* 18)              ; boid count (O(n^2) sim -> keep it modest)
(defparameter *bound* 5.0d0)        ; half-size of the cube the flock lives in
(defparameter *maxspeed* 0.10d0)
(defparameter *minspeed* 0.035d0)
(defparameter *percept* 2.4d0)      ; neighbour radius
(defparameter *sepr* 1.1d0)         ; separation radius
(defparameter *w-sep* 0.010d0)
(defparameter *w-ali* 0.055d0)
(defparameter *w-coh* 0.0009d0)
(defparameter *w-bound* 0.004d0)

(defvar *bx*) (defvar *by*) (defvar *bz*)   ; positions
(defvar *bu*) (defvar *bv*) (defvar *bw*)   ; velocities
;; per-boid steering accumulators, filled once per frame (see boids-step)
(defvar *sx*) (defvar *sy*) (defvar *sz*)   ; separation
(defvar *ax*) (defvar *ay*) (defvar *az*)   ; alignment sum (neighbour velocities)
(defvar *cx*) (defvar *cy*) (defvar *cz*)   ; cohesion sum (neighbour positions)
(defvar *cn*)                               ; neighbour count

;; tiny LCG so we don't depend on the host RNG state
(defvar *seed* 2463534242)
(defun %rnd (lo hi)
  (setq *seed* (logand (+ (* *seed* 1103515245) 12345) #x7fffffff))
  (+ lo (* (- hi lo) (/ *seed* 2147483647.0d0))))

(defun boids-init ()
  (flet ((v () (make-array *nb*)))
    (setq *bx* (v) *by* (v) *bz* (v) *bu* (v) *bv* (v) *bw* (v)
          *sx* (v) *sy* (v) *sz* (v) *ax* (v) *ay* (v) *az* (v)
          *cx* (v) *cy* (v) *cz* (v) *cn* (v)))
  (dotimes (i *nb*)
    (setf (aref *bx* i) (%rnd -3.0d0 3.0d0) (aref *by* i) (%rnd -3.0d0 3.0d0) (aref *bz* i) (%rnd -3.0d0 3.0d0)
          (aref *bu* i) (%rnd -0.05d0 0.05d0) (aref *bv* i) (%rnd -0.05d0 0.05d0) (aref *bw* i) (%rnd -0.05d0 0.05d0))))

(defun boids-step ()
  (let ((p2 (* *percept* *percept*)) (s2 (* *sepr* *sepr*)))
    ;; zero the accumulators
    (dotimes (i *nb*)
      (setf (aref *sx* i) 0.0d0 (aref *sy* i) 0.0d0 (aref *sz* i) 0.0d0
            (aref *ax* i) 0.0d0 (aref *ay* i) 0.0d0 (aref *az* i) 0.0d0
            (aref *cx* i) 0.0d0 (aref *cy* i) 0.0d0 (aref *cz* i) 0.0d0 (aref *cn* i) 0))
    ;; each unordered pair once; apply the interaction to BOTH boids
    (dotimes (i *nb*)
      (let ((xi (aref *bx* i)) (yi (aref *by* i)) (zi (aref *bz* i)))
        (dotimes (jj (- *nb* i 1))
          (let* ((j (+ i 1 jj))
                 (dx (- (aref *bx* j) xi)) (dy (- (aref *by* j) yi)) (dz (- (aref *bz* j) zi))
                 (d2 (+ (* dx dx) (* dy dy) (* dz dz))))
            (when (< d2 p2)
              (incf (aref *cn* i)) (incf (aref *cn* j))
              (incf (aref *cx* i) (aref *bx* j)) (incf (aref *cy* i) (aref *by* j)) (incf (aref *cz* i) (aref *bz* j))
              (incf (aref *cx* j) xi) (incf (aref *cy* j) yi) (incf (aref *cz* j) zi)
              (incf (aref *ax* i) (aref *bu* j)) (incf (aref *ay* i) (aref *bv* j)) (incf (aref *az* i) (aref *bw* j))
              (incf (aref *ax* j) (aref *bu* i)) (incf (aref *ay* j) (aref *bv* i)) (incf (aref *az* j) (aref *bw* i))
              (when (< d2 s2)
                (let* ((inv (/ 1.0d0 (+ d2 0.02d0))) (ix (* dx inv)) (iy (* dy inv)) (iz (* dz inv)))
                  (decf (aref *sx* i) ix) (decf (aref *sy* i) iy) (decf (aref *sz* i) iz)   ; i steers away from j
                  (incf (aref *sx* j) ix) (incf (aref *sy* j) iy) (incf (aref *sz* j) iz))))))))  ; and j away from i
    ;; combine steering, clamp speed, integrate
    (dotimes (i *nb*)
      (let ((xi (aref *bx* i)) (yi (aref *by* i)) (zi (aref *bz* i))
            (ui (aref *bu* i)) (vi (aref *bv* i)) (wi (aref *bw* i))
            (cnt (aref *cn* i))
            (sax (* *w-sep* (aref *sx* i))) (say (* *w-sep* (aref *sy* i))) (saz (* *w-sep* (aref *sz* i))))
        (when (> cnt 0)
          (let ((fc (/ 1.0d0 cnt)))
            (incf sax (* *w-ali* (- (* (aref *ax* i) fc) ui))) (incf say (* *w-ali* (- (* (aref *ay* i) fc) vi))) (incf saz (* *w-ali* (- (* (aref *az* i) fc) wi)))
            (incf sax (* *w-coh* (- (* (aref *cx* i) fc) xi))) (incf say (* *w-coh* (- (* (aref *cy* i) fc) yi))) (incf saz (* *w-coh* (- (* (aref *cz* i) fc) zi)))))
        (when (> xi *bound*) (decf sax *w-bound*)) (when (< xi (- *bound*)) (incf sax *w-bound*))
        (when (> yi *bound*) (decf say *w-bound*)) (when (< yi (- *bound*)) (incf say *w-bound*))
        (when (> zi *bound*) (decf saz *w-bound*)) (when (< zi (- *bound*)) (incf saz *w-bound*))
        (let* ((nu (+ ui sax)) (nv (+ vi say)) (nw (+ wi saz))
               (sp (sqrt (+ (* nu nu) (* nv nv) (* nw nw)))))
          (when (< sp 1.0d-9) (setq sp 1.0d0 nu 0.01d0))
          (let ((k (cond ((> sp *maxspeed*) (/ *maxspeed* sp))
                         ((< sp *minspeed*) (/ *minspeed* sp))
                         (t 1.0d0))))
            (setf (aref *bu* i) (* nu k) (aref *bv* i) (* nv k) (aref *bw* i) (* nw k)))
          (incf (aref *bx* i) (aref *bu* i)) (incf (aref *by* i) (aref *bv* i)) (incf (aref *bz* i) (aref *bw* i)))))))

;; model matrix from a boid's position + heading -- pure construction, no m*
(defun boid-matrix (i)
  (let* ((u (aref *bu* i)) (v (aref *bv* i)) (w (aref *bw* i))
         (sp (sqrt (+ (* u u) (* v v) (* w w)))) (sp (if (< sp 1.0d-9) 1.0d0 sp))
         (fx (/ u sp)) (fy (/ v sp)) (fz (/ w sp))               ; forward = heading
         (rl (sqrt (+ (* fx fx) (* fz fz)))) (rl (if (< rl 1.0d-6) 1.0d0 rl))
         (rx (/ fz rl)) (ry 0.0d0) (rz (/ (- fx) rl))            ; right = norm(cross(up,fwd))
         (ux (- (* fy rz) (* fz ry))) (uy (- (* fz rx) (* fx rz))) (uz (- (* fx ry) (* fy rx)))  ; up = cross(fwd,right)
         (m (make-array 16)))
    (setf (aref m 0) rx (aref m 1) ry (aref m 2) rz (aref m 3) 0.0d0
          (aref m 4) ux (aref m 5) uy (aref m 6) uz (aref m 7) 0.0d0
          (aref m 8) fx (aref m 9) fy (aref m 10) fz (aref m 11) 0.0d0
          (aref m 12) (aref *bx* i) (aref m 13) (aref *by* i) (aref m 14) (aref *bz* i) (aref m 15) 1.0d0)
    m))

;; boid shape: a little dart (tetrahedron) pointing +Z, flat-shaded per face
(defun boid-geometry (id)
  (gl:new-list id :compile)
  (gl:begin gl:+triangles+)
  (flet ((face (r g b ax ay az bx by bz cx cy cz)
           (gl:color r g b) (gl:vertex ax ay az) (gl:vertex bx by bz) (gl:vertex cx cy cz)))
    ;; nose A=(0,0,.5) back-left B=(-.22,-.12,-.22) back-right C=(.22,-.12,-.22) back-top D=(0,.2,-.22)
    (face 1.0d0 0.6d0  0.18d0   0.0d0 0.0d0 0.5d0   -0.22d0 -0.12d0 -0.22d0   0.0d0 0.2d0 -0.22d0)  ; left
    (face 1.0d0 0.78d0 0.24d0   0.0d0 0.0d0 0.5d0    0.0d0 0.2d0 -0.22d0       0.22d0 -0.12d0 -0.22d0) ; right
    (face 0.92d0 0.42d0 0.12d0  0.0d0 0.0d0 0.5d0    0.22d0 -0.12d0 -0.22d0   -0.22d0 -0.12d0 -0.22d0) ; belly
    (face 0.5d0 0.22d0 0.06d0  -0.22d0 -0.12d0 -0.22d0   0.22d0 -0.12d0 -0.22d0   0.0d0 0.2d0 -0.22d0)) ; base
  (gl:end)
  (gl:end-list))

;; faint wireframe box the flock lives in, for spatial reference
(defun box-geometry (id)
  (gl:new-list id :compile)
  (gl:begin gl:+lines+)
  (gl:color 0.25d0 0.3d0 0.45d0)
  (let ((b *bound*))
    (flet ((edge (ax ay az bx by bz) (gl:vertex ax ay az) (gl:vertex bx by bz)))
      (dolist (s (list (- b) b))
        (edge (- b) (- b) s b (- b) s) (edge (- b) b s b b s)
        (edge (- b) (- b) s (- b) b s) (edge b (- b) s b b s))
      (edge (- b) (- b) (- b) (- b) (- b) b) (edge b (- b) (- b) b (- b) b)
      (edge (- b) b (- b) (- b) b b) (edge b b (- b) b b b)))
  (gl:end)
  (gl:end-list))

;; The projection + camera pull-back + tilt never change, so bake them once; each
;; frame only the orbit rotation is applied -> a single 4x4 multiply for the PV.
(defvar *pv-base* nil)
(defun boids-pv-init ()
  (gl:matrix-mode gl:+projection+) (gl:load-identity)
  (gl:perspective 50.0d0 1.0d0 0.1d0 100.0d0)
  (gl:translate 0.0d0 0.0d0 -15.0d0)      ; camera pulls back
  (gl:rotate 20.0d0 1.0d0 0.0d0 0.0d0)    ; tilt down
  (setq *pv-base* (copy-seq gl::*proj*)))
(defun boids-pv-pack (angle)   ; compute proj*view (1 m*) and pack it into slot 0
  (setq gl::*proj* (copy-seq *pv-base*) gl::*proj-id* nil)  ; restore baked base
  (gl:rotate angle 0.0d0 1.0d0 0.0d0)                       ; slow orbit (1 m*)
  (gl:pack-at gl::*proj* 0))

;; The whole per-frame command stream is invariant (fixed list ids + slots): clear,
;; set proj*view, draw the box, draw each boid.  Build it once; each frame we only
;; refresh the matrix bytes and blit this block.
(defvar *cmds* "")
(defun build-cmds (dart box)
  (let ((nl (string (code-char 10))))
    (setq *cmds* (concatenate 'string (gl:clear-cmd 0.03d0 0.03d0 0.06d0) nl
                              (gl:pv-line) nl (gl:model-line box 1) nl))
    (dotimes (i *nb*) (setq *cmds* (concatenate 'string *cmds* (gl:model-line dart (+ i 2)) nl)))))

(defun boids-frame (dart box angle)
  (boids-step)
  (boids-pv-pack angle)                                ; slot 0 = proj*view
  (gl:pack-at (gl::midentity) 1)                       ; slot 1 = box (identity model)
  (dotimes (i *nb*) (gl:pack-at (boid-matrix i) (+ i 2)))
  (gl:blit *cmds*))

(defun boids ()
  (gui-reset)
  (gui-panel "Boids - 3D flocking, instanced cl-opengl draws.  Esc to quit")
  (gui-canvas 520 520)
  (gui-keys t)
  (boids-init)
  (boids-pv-init)
  (let ((dart (gl:gen-lists 1)) (box (gl:gen-lists 1)) (angle 0.0d0) (done nil))
    (boid-geometry dart) (box-geometry box)
    (gl:flush)                        ; ship the one-time geometry uploads before blitting frames
    (build-cmds dart box)
    (loop
      (when done (gui-keys nil) (gui-close) (return :bye))
      (gui-wait 16)
      (dolist (ev (gui-events)) (when (key-down-p ev "Escape") (setq done t)))
      (setq angle (+ angle 0.35d0))
      (boids-frame dart box angle))))

(format t "boids.lisp loaded - run (boids)~%")
