;;;; opengl.lisp - a slice of the cl-opengl API (immediate mode + matrix stack)
;;;; bridged to WebGL, plus a rotating-cube demo.  (load gui.lisp) first for the
;;;; command channel + gui-canvas; then gl: calls stream geometry to the page's
;;;; WebGL renderer (gldraw MODE MVP VERTS / glclear).

(defpackage :gl
  (:use :cl)
  (:export :clear-color :clear :matrix-mode :load-identity :push-matrix
           :pop-matrix :translate :rotate :scale :ortho :perspective
           :begin :end :vertex :color :flush :gen-lists :new-list :end-list :call-list
           :+projection+ :+modelview+ :+triangles+ :+quads+ :+lines+))
(in-package :gl)

;;; --- 4x4 matrices, column-major (WebGL order) ------------------------------
(defun midentity ()
  (make-array 16 :initial-contents '(1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0
                                     0.0d0 0.0d0 1.0d0 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0)))
(defun m* (a b)
  (let ((r (make-array 16)))
    (dotimes (col 4)
      (dotimes (row 4)
        (let ((s 0.0d0))
          (dotimes (k 4) (incf s (* (aref a (+ row (* k 4))) (aref b (+ k (* col 4))))))
          (setf (aref r (+ row (* col 4))) s))))
    r))
(defun d2r (d) (* d (/ 3.14159265358979d0 180.0d0)))
(defun m-translate (x y z)
  (let ((m (midentity))) (setf (aref m 12) (float x 1.0d0) (aref m 13) (float y 1.0d0) (aref m 14) (float z 1.0d0)) m))
(defun m-scale (x y z)
  (let ((m (midentity))) (setf (aref m 0) (float x 1.0d0) (aref m 5) (float y 1.0d0) (aref m 10) (float z 1.0d0)) m))
(defun m-rotate (deg x y z)
  (let* ((a (d2r deg)) (c (cos a)) (s (sin a))
         (len (sqrt (+ (* x x) (* y y) (* z z)))) (x (/ x len)) (y (/ y len)) (z (/ z len))
         (ic (- 1.0d0 c)) (m (midentity)))
    (setf (aref m 0) (+ c (* x x ic))   (aref m 1) (+ (* y x ic) (* z s)) (aref m 2) (- (* z x ic) (* y s))
          (aref m 4) (- (* x y ic) (* z s)) (aref m 5) (+ c (* y y ic))   (aref m 6) (+ (* z y ic) (* x s))
          (aref m 8) (+ (* x z ic) (* y s)) (aref m 9) (- (* y z ic) (* x s)) (aref m 10) (+ c (* z z ic)))
    m))
(defun m-perspective (fovy aspect near far)
  (let* ((f (/ 1.0d0 (tan (/ (d2r fovy) 2.0d0)))) (nf (/ 1.0d0 (- near far)))
         (m (make-array 16 :initial-element 0.0d0)))
    (setf (aref m 0) (/ f aspect) (aref m 5) f (aref m 10) (* (+ far near) nf)
          (aref m 11) -1.0d0 (aref m 14) (* 2.0d0 far near nf))
    m))
(defun m-ortho (l r b top near far)
  (let ((m (make-array 16 :initial-element 0.0d0)))
    (setf (aref m 0) (/ 2.0d0 (- r l)) (aref m 5) (/ 2.0d0 (- top b)) (aref m 10) (/ -2.0d0 (- far near))
          (aref m 12) (- (/ (+ r l) (- r l))) (aref m 13) (- (/ (+ top b) (- top b)))
          (aref m 14) (- (/ (+ far near) (- far near))) (aref m 15) 1.0d0)
    m))

;;; --- immediate-mode state --------------------------------------------------
(defparameter +projection+ :projection)
(defparameter +modelview+  :modelview)
(defparameter +triangles+  :triangles)
(defparameter +quads+      :quads)
(defparameter +lines+      :lines)
(defvar *proj* (midentity))
(defvar *mv*   (midentity))
(defvar *mm*   :modelview)
(defvar *pstk* nil)
(defvar *mstk* nil)
(defvar *col*  (list 1.0d0 1.0d0 1.0d0))
(defvar *prim* :triangles)
(defvar *vs*   nil)
(defvar *cc*   (list 0.0d0 0.0d0 0.0d0))
;; Track whether each matrix is currently the identity, so the FIRST transform
;; after load-identity is a plain store (I*M = M) instead of a 4x4 multiply.  The
;; cube does perspective-on-identity and translate-on-identity every frame, so
;; this drops the per-frame m* count from 4 to 2.
(defvar *proj-id* t)
(defvar *mv-id*   t)

(defun cget () (if (eq *mm* :projection) *proj* *mv*))
(defun cset (m) (if (eq *mm* :projection) (setq *proj* m) (setq *mv* m)))
(defun cmul (m)   ; current := (if identity M else current*M); no longer identity
  (if (eq *mm* :projection)
      (setq *proj* (if *proj-id* m (m* *proj* m)) *proj-id* nil)
      (setq *mv*   (if *mv-id*   m (m* *mv*   m)) *mv-id*   nil)))
(defun matrix-mode (m) (setq *mm* m))
(defun load-identity ()
  (if (eq *mm* :projection) (setq *proj* (midentity) *proj-id* t) (setq *mv* (midentity) *mv-id* t)))
(defun push-matrix () (if (eq *mm* :projection) (push (copy-seq *proj*) *pstk*) (push (copy-seq *mv*) *mstk*)))
(defun pop-matrix ()  ; identity state of the restored matrix is unknown -> assume not
  (if (eq *mm* :projection) (setq *proj* (pop *pstk*) *proj-id* nil) (setq *mv* (pop *mstk*) *mv-id* nil)))
(defun translate (x y z) (cmul (m-translate x y z)))
(defun rotate (deg x y z) (cmul (m-rotate deg x y z)))
(defun scale (x y z) (cmul (m-scale x y z)))
(defun perspective (fovy aspect near far) (cmul (m-perspective fovy aspect near far)))
(defun ortho (l r b top near far) (cmul (m-ortho l r b top near far)))
(defun color (r g b) (setq *col* (list (float r 1.0d0) (float g 1.0d0) (float b 1.0d0))))
(defun begin (mode) (setq *prim* mode *vs* nil))
(defun vertex (x y &optional (z 0.0d0))
  (push (list (float x 1.0d0) (float y 1.0d0) (float z 1.0d0) (first *col*) (second *col*) (third *col*)) *vs*))

;;; --- send to the WebGL bridge ----------------------------------------------
(defun %t () (string (code-char 9)))
;; A JS-parseable decimal with 4 fractional digits, WITHOUT format ~f: float
;; printing (Steele-White) is the single most expensive thing this demo does per
;; frame, so we round to fixed-point and print integers instead.
(defun %f (x)
  (let* ((x (float x 1.0d0))
         (neg (< x 0.0d0))
         (n (round (* (if neg (- x) x) 10000.0d0)))
         (ip (floor n 10000))
         (fp (mod n 10000))
         (fs (write-to-string fp)))
    (concatenate 'string (if neg "-" "") (write-to-string ip) "."
                 (make-string (- 4 (length fs)) :initial-element #\0) fs)))
(defun %mvp (m)
  (let ((s (%f (aref m 0)))) (dotimes (i 15) (setq s (concatenate 'string s "," (%f (aref m (1+ i)))))) s))
(defun %verts (vs)
  (let ((s ""))
    (dolist (v vs)
      (setq s (concatenate 'string s (%f (first v)) "," (%f (second v)) "," (%f (third v)) ","
                           (%f (fourth v)) "," (%f (fifth v)) "," (%f (sixth v)) ";")))
    s))
(defun %tri (mode vs)   ; -> (mode-string . triangle-verts)
  (cond ((eq mode :quads)
         (let ((vv (coerce vs 'vector)) (out nil) (i 0))
           (loop (when (> (+ i 4) (length vv)) (return nil))
             (let ((a (aref vv i)) (b (aref vv (+ i 1))) (c (aref vv (+ i 2))) (d (aref vv (+ i 3))))
               (setq out (append out (list a b c a c d))))
             (setq i (+ i 4)))
           (cons "triangles" out)))
        ((eq mode :lines) (cons "lines" vs))
        (t (cons "triangles" vs))))
;; Accumulate a whole frame's commands and ship them in ONE message, so the
;; page's per-animation-frame coalescer cannot split a clear from its draws.
(defvar *frame* "")
(defun %emit (line) (setq *frame* (concatenate 'string *frame* line (string (code-char 10)))))
(defun flush () (when (> (length *frame*) 0) (cl-user::gui-cmd *frame*) (setq *frame* "") (setq *foff* 0)))
(defun %num (n) (write-to-string n))

;; Native float bridge: instead of formatting the mvp matrix to a decimal string
;; every frame (the expensive path), write each element's raw IEEE-754 double
;; bits into a scratch buffer and let the page reinterpret those bytes as a
;; Float64Array.  ieee-float-{lo,hi}32 give the two 32-bit halves of the double;
;; mem-ref :u32 writes them little-endian, so the 8 bytes are a valid LE double.
(defconstant +fbuf+ #x10015000)   ; scratch below the gui command buffer; snapshot in host-browser.js guiSend
(defvar *foff* 0)                 ; rolling byte offset into +fbuf+, reset each flush
(defun %fput (addr x)
  (setf (cl-user::mem-ref addr :u32) (cl-user::ieee-float-lo32 x))
  (setf (cl-user::mem-ref (+ addr 4) :u32) (cl-user::ieee-float-hi32 x)))
(defun %mvp-bin (m off)           ; write m's 16 doubles at +fbuf+ + off; return off
  (dotimes (i 16) (%fput (+ +fbuf+ off (* i 8)) (aref m i)))
  off)

;; Display lists: cache geometry on the page ONCE (new-list ... end-list), then
;; each frame draw it with just the current matrix (call-list) — so the Lisp side
;; rebuilds only the mvp per frame, not the whole vertex stream.  This is what
;; keeps the demo fast even before the matrix code JIT-warms.
(defvar *cap* nil)      ; the list id currently being compiled, or NIL
(defvar *list-ctr* 0)
(defun gen-lists (n) (declare (ignore n)) (incf *list-ctr*))
(defun new-list (id &optional mode) (declare (ignore mode)) (setq *cap* id))
(defun end-list () (setq *cap* nil))
(defun call-list (id)
  (let ((off *foff*))
    (%mvp-bin (m* *proj* *mv*) off)
    (setq *foff* (+ off 128))
    (%emit (concatenate 'string "gldrawlist" (%t) (%num id) (%t) "f" (%t) (%num off)))))

(defun end ()
  (let ((tv (%tri *prim* (reverse *vs*))))
    (if *cap*
        (%emit (concatenate 'string "gllist" (%t) (%num *cap*) (%t) (car tv) (%t) (%verts (cdr tv))))
        (%emit (concatenate 'string "gldraw" (%t) (car tv) (%t) (%mvp (m* *proj* *mv*)) (%t) (%verts (cdr tv)))))))
;; The clear colour is almost always constant across frames, so cache its command
;; line and only rebuild it (3 %f calls) when the colour actually changes.
(defvar *cc-str* "glclear")
(defun clear-color (r g b)
  (let ((nc (list (float r 1.0d0) (float g 1.0d0) (float b 1.0d0))))
    (unless (equal nc *cc*)
      (setq *cc* nc
            *cc-str* (concatenate 'string "glclear" (%t) (%f (first nc)) (%t) (%f (second nc)) (%t) (%f (third nc)))))))
(defun clear (&rest bits) (declare (ignore bits)) (%emit *cc-str*))

;;; --- the demo: a rotating colored cube (in CL-USER) ------------------------
(in-package :cl-user)

(defun %cube-quad (r g b ax ay az bx by bz cx cy cz dx dy dz)
  (gl:color r g b)
  (gl:vertex ax ay az) (gl:vertex bx by bz) (gl:vertex cx cy cz) (gl:vertex dx dy dz))

(defun draw-cube ()
  (gl:begin gl:+quads+)
  (%cube-quad 1.0d0 0.35d0 0.35d0  -1.0d0 -1.0d0  1.0d0   1.0d0 -1.0d0  1.0d0   1.0d0  1.0d0  1.0d0  -1.0d0  1.0d0  1.0d0) ; front
  (%cube-quad 0.35d0 1.0d0 0.45d0  -1.0d0 -1.0d0 -1.0d0  -1.0d0  1.0d0 -1.0d0   1.0d0  1.0d0 -1.0d0   1.0d0 -1.0d0 -1.0d0) ; back
  (%cube-quad 0.4d0 0.5d0 1.0d0    -1.0d0  1.0d0 -1.0d0  -1.0d0  1.0d0  1.0d0   1.0d0  1.0d0  1.0d0   1.0d0  1.0d0 -1.0d0) ; top
  (%cube-quad 1.0d0 0.85d0 0.3d0   -1.0d0 -1.0d0 -1.0d0   1.0d0 -1.0d0 -1.0d0   1.0d0 -1.0d0  1.0d0  -1.0d0 -1.0d0  1.0d0) ; bottom
  (%cube-quad 1.0d0 0.5d0 0.9d0     1.0d0 -1.0d0 -1.0d0   1.0d0  1.0d0 -1.0d0   1.0d0  1.0d0  1.0d0   1.0d0 -1.0d0  1.0d0) ; right
  (%cube-quad 0.4d0 0.9d0 1.0d0    -1.0d0 -1.0d0 -1.0d0  -1.0d0 -1.0d0  1.0d0  -1.0d0  1.0d0  1.0d0  -1.0d0  1.0d0 -1.0d0) ; left
  (gl:end))

(defun glcube ()
  (gui-reset)
  (gui-panel "OpenGL cube - cl-opengl display list -> WebGL.  Esc to quit")
  (gui-canvas 480 480)
  (gui-keys t)
  (let ((cube (gl:gen-lists 1)) (angle 0.0d0) (done nil))
    (gl:new-list cube :compile) (draw-cube) (gl:end-list)   ; geometry uploaded ONCE
    ;; Projection and camera are constant, so set them ONCE outside the loop and
    ;; fold the camera translate into the projection matrix (perspective*translate).
    ;; The per-frame modelview is then just the rotation on a fresh identity -- a
    ;; plain store, no multiply -- so the whole frame costs a single 4x4 m*.
    (gl:matrix-mode gl:+projection+) (gl:load-identity)
    (gl:perspective 45.0d0 1.0d0 0.1d0 100.0d0) (gl:translate 0.0d0 0.0d0 -5.0d0)
    (gl:matrix-mode gl:+modelview+)
    (loop
      (when done (gui-keys nil) (gui-close) (return :bye))
      (gui-wait 16)
      (dolist (ev (gui-events)) (when (key-down-p ev "Escape") (setq done t)))
      (setq angle (+ angle 1.6d0))
      (gl:clear-color 0.05d0 0.05d0 0.09d0)
      (gl:clear)
      (gl:load-identity) (gl:rotate angle 1.0d0 0.6d0 0.35d0)   ; modelview = rotation only
      (gl:call-list cube)          ; per frame: one m* (proj*camera) * rotation
      (gl:flush))))

(format t "opengl.lisp loaded - run (glcube)~%")
