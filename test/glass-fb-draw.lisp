;;;; draw.lisp — the SAME drawing program, run under modus and under SBCL.
;;;;
;;;; Output is a deterministic text transcript: every pixel of the framebuffer as
;;;; six hex digits, plus the bookkeeping fields (generation, frameno, damage,
;;;; copy).  Nothing here reads a clock or an address, so two correct
;;;; implementations must produce BYTE-IDENTICAL output.

(in-package #:glass)

(defun hex6 (n)
  (let ((s (make-string 6 :initial-element #\0))
        (digits "0123456789abcdef"))
    (dotimes (i 6 s)
      (setf (char s (- 5 i)) (char digits (logand (ash n (* -4 i)) 15))))))

(defun dump-fb (label fb)
  (format t "~A ~Dx~D gen=~D~%" label (fb-width fb) (fb-height fb) (fb-generation fb))
  (dotimes (y (fb-height fb))
    (format t "  ~A:" (hex6 y))
    (dotimes (x (fb-width fb))
      (format t " ~A" (hex6 (fb-get fb x y))))
    (format t "~%")))


(defvar *fb* nil) (defvar *sprite* nil) (defvar *rz* nil) (defvar *mk* nil) (defvar *big* nil)

;;; EVERYTHING runs inside ONE COMPILED FUNCTION, deliberately.  CLAUDE.md:
;;; "%gc-collect-here at an interpreted --script toplevel breaks the next
;;; (format t ...) (it re-runs its control string)" — a pre-existing property of
;;; the mvm-eval toplevel.  This program allocates enough to collect, so run it
;;; from compiled in-image code exactly as that note prescribes.
(defun main ()
;;; ---- rgb ----------------------------------------------------------------
(format t "== rgb ==~%")
(dolist (c '((0 0 0) (255 255 255) (18 52 86) (255 0 0) (0 255 0) (0 0 255)
             (300 -1 128)))
  (format t "  rgb ~S = ~A~%" c (hex6 (rgb (first c) (second c) (third c)))))
(format t "  black=~A white=~A red=~A green=~A blue=~A~%"
        (hex6 +black+) (hex6 +white+) (hex6 +red+) (hex6 +green+) (hex6 +blue+))

;;; ---- the scene ----------------------------------------------------------
(setq *fb* (make-framebuffer 48 32 (rgb 8 8 8)))

(format t "== after make ==~%")
(format t "  gen=~D w=~D h=~D~%" (fb-generation *fb*) (fb-width *fb*) (fb-height *fb*))

;; whole-array fill
(fb-fill *fb* (rgb 16 32 48))

;; filled rects, including ones that hang off every edge
(fb-rect *fb* 2 2 10 6 (rgb 255 0 0))
(fb-rect *fb* -4 -4 8 8 (rgb 0 255 0))
(fb-rect *fb* 44 28 8 8 (rgb 0 0 255))
(fb-rect *fb* 20 5 0 5 (rgb 255 255 0))     ; zero width — must draw nothing
(fb-rect *fb* 20 5 5 0 (rgb 255 255 0))     ; zero height — must draw nothing

;; lines
(fb-hline *fb* 0 15 48 (rgb 200 200 0))
(fb-vline *fb* 24 0 32 (rgb 0 200 200))
(fb-hline *fb* 40 31 20 (rgb 255 128 0))    ; runs off the right edge

;; outline, thickness 1 and 3
(fb-frame *fb* 30 2 12 10 (rgb 255 255 255))
(fb-frame *fb* 4 20 16 10 (rgb 128 0 128) 3)

;; individual pixels, including out of bounds (must be no-ops)
(fb-put *fb* 0 0 (rgb 1 2 3))
(fb-put *fb* 47 31 (rgb 4 5 6))
(fb-put *fb* -1 0 (rgb 9 9 9))
(fb-put *fb* 0 -1 (rgb 9 9 9))
(fb-put *fb* 48 0 (rgb 9 9 9))
(fb-put *fb* 0 32 (rgb 9 9 9))

(format t "== fb-get out of bounds ==~%")
(format t "  (-1,0)=~A (0,-1)=~A (48,0)=~A (0,32)=~A (0,0)=~A~%"
        (hex6 (fb-get *fb* -1 0)) (hex6 (fb-get *fb* 0 -1))
        (hex6 (fb-get *fb* 48 0)) (hex6 (fb-get *fb* 0 32))
        (hex6 (fb-get *fb* 0 0)))

(dump-fb "SCENE-A" *fb*)

;;; ---- clipping -----------------------------------------------------------
(with-fb-clip (*fb* 10 10 12 12)
  (fb-rect *fb* 0 0 48 32 (rgb 60 60 60))      ; clipped to the box
  (with-fb-clip (*fb* 0 0 14 14)               ; nested: intersection
    (fb-fill *fb* (rgb 90 10 10))))
(format t "== clip restored: ~S ==~%" (fb-clip *fb*))
(dump-fb "SCENE-B" *fb*)

;;; ---- fb-move-rect: all three overlap orders -----------------------------
(fb-move-rect *fb* 0 0 8 0 12 6)      ; same rows -> scratch-row path
(dump-fb "MOVE-HORIZ" *fb*)
(fb-move-rect *fb* 0 0 0 6 16 8)      ; downwards -> bottom-up
(dump-fb "MOVE-DOWN" *fb*)
(fb-move-rect *fb* 0 10 0 4 16 8)     ; upwards -> top-down
(dump-fb "MOVE-UP" *fb*)
;; refused whole (out of bounds / degenerate), must change nothing
(fb-move-rect *fb* 40 0 0 0 20 4)
(fb-move-rect *fb* 0 0 0 0 8 8)
(fb-move-rect *fb* 0 0 4 4 0 0)
(fb-move-rect *fb* -1 0 4 4 4 4)
(dump-fb "MOVE-REFUSED" *fb*)

;;; ---- blit ---------------------------------------------------------------
(setq *sprite* (make-framebuffer 6 4 (rgb 250 250 0)))
(fb-rect *sprite* 1 1 4 2 (rgb 0 0 128))
(fb-put *sprite* 0 0 (rgb 255 0 255))
(dump-fb "SPRITE" *sprite*)
(fb-blit *fb* *sprite* 30 24)
(fb-blit *fb* *sprite* 45 30)          ; partly off the edge
(fb-blit *fb* *sprite* -2 -2)          ; partly off the origin
(dump-fb "SCENE-C" *fb*)

;;; ---- resize -------------------------------------------------------------
(setq *rz* (make-framebuffer 4 3 (rgb 1 1 1)))
(fb-resize *rz* 4 3 (rgb 2 2 2))       ; same size — a documented no-op
(dump-fb "RESIZE-NOOP" *rz*)
(fb-resize *rz* 6 2 (rgb 3 3 3))
(dump-fb "RESIZE-DONE" *rz*)

;;; ---- damage / copy bookkeeping -----------------------------------------
(format t "== damage union ==~%")
(dolist (pair '((nil nil) (nil (1 2 3 4)) ((1 2 3 4) nil)
                ((1 2 3 4) (0 5 2 9)) (:full (1 2 3 4)) ((1 2 3 4) :full)
                (:full :full)))
  (format t "  ~S + ~S = ~S~%" (first pair) (second pair)
          (%fb-damage-union (first pair) (second pair))))

(format t "== copy compose ==~%")
(dolist (pair '((nil nil)
                (nil (0 0 5 5 10 10))
                ((0 0 5 5 10 10) nil)
                ((0 0 5 5 10 10) (5 5 9 9 10 10))     ; dragged window: survives whole
                ((0 0 0 10 40 10) (0 10 0 16 40 10))  ; scroll: common sub-block
                ((0 0 50 50 4 4) (0 0 1 1 4 4))))     ; disjoint -> NIL
  (format t "  ~S then ~S = ~S~%" (first pair) (second pair)
          (%fb-copy-compose (first pair) (second pair))))

(setq *mk* (make-framebuffer 8 8))
(format t "== mark/take ==~%")
(format t "  fresh: frameno=~D damage=~S copy=~S~%"
        (fb-frameno *mk*) (fb-damage *mk*) (fb-copy *mk*))
(fb-mark-frame *mk* '(1 1 4 4))
(fb-mark-frame *mk* '(3 0 9 2) '(0 0 2 2 4 4))
(fb-mark-frame *mk* nil '(2 2 5 5 4 4))
(format t "  accumulated: frameno=~D damage=~S copy=~S~%"
        (fb-frameno *mk*) (fb-damage *mk*) (fb-copy *mk*))
(multiple-value-bind (f d c) (fb-take-frame *mk*)
  (format t "  taken: frameno=~D damage=~S copy=~S~%" f d c))
(format t "  after take: damage=~S copy=~S~%" (fb-damage *mk*) (fb-copy *mk*))
(fb-mark-frame *mk* :full '(1 1 2 2 3 3))
;; SEQUENCED ON PURPOSE.  Written as one 5-argument (format t ... (fb-take-copy
;; *mk*) (fb-damage *mk*) (fb-copy *mk*)) this measures a MODUS COMPILER BUG
;; rather than glass/fb: modus evaluates a call's 5th and later argument forms
;; FIRST, right-to-left, so (fb-copy *mk*) is read BEFORE fb-take-copy clears
;; it and the line reports the stale value.  CLHS 3.1.2.1.2.3 requires strict
;; left-to-right.  Only side-effecting argument lists are affected, so binding
;; the take first is enough to make this line about the framebuffer again.
(let* ((tc (fb-take-copy *mk*))
       (dm (fb-damage *mk*))
       (cp (fb-copy *mk*)))
  (format t "  take-copy=~S then damage=~S copy=~S~%" tc dm cp))

;;; ---- a whole-framebuffer checksum, so a big buffer is covered too -------
(setq *big* (make-framebuffer 320 200 (rgb 3 5 7)))
(fb-rect *big* 10 10 300 180 (rgb 200 30 30))
(fb-frame *big* 40 40 240 120 (rgb 255 255 255) 4)
(fb-move-rect *big* 0 0 20 20 200 100)
(fb-blit *big* *sprite* 100 100)
(let ((sum 0) (px (fb-pixels *big*)))
  (dotimes (i (length px))
    (setq sum (logand (+ (* sum 31) (aref px i)) #xffffffffffff)))
  (format t "== big checksum ==~%  320x200 fnv-ish = ~A~%" (hex6 (logand sum #xffffff)))
  (format t "  full = ~D~%" sum))

(format t "DONE~%")

  0)
(main)
