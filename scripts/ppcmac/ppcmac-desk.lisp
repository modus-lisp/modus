;;;; ppcmac-desk.lisp -- glass.wm on the PowerPC Mac's own screen.
;;;;
;;;; The last entry of the boot payload (scripts/ppcmac-disk.lisp --payload):
;;;; glass, scribe and glass/wm-core are already loaded (natively, under the
;;;; image's PPC tier), and ppcmac-input.lisp's CUDA/ADB keyboard and mouse.
;;;; The framebuffer is the one Open Firmware set up: QEMU mac99's at PA
;;;; #x81000000, VA #x09000000 under boot-ppcmac's DBAT3, 32-bit XRGB in a
;;;; big-endian word -- glass's own pixel -- 4096 bytes a row.
;;;;
;;;; Each turn: CUDA's packets become pointer and key events for the window
;;;; manager, WM-TICK composites, and the damaged rectangle is copied to the
;;;; screen; the pointer is drawn over it (an RFB client draws its own; a screen
;;;; needs one), with the pixels under it put back when it moves.

(in-package :cl-user)

(defvar *screen-base* #x09000000)
(defvar *screen-pitch* 4096)
(defvar *screen-w* 1024)
(defvar *screen-h* 768)

(defun %payload-file (name)
  ;; a KERNEL function: named here, it compiles to a direct call (kernel
  ;; functions have no runtime symbol, so FIND-SYMBOL cannot see them)
  (modus-int::ppcmac-file name))

(defun desk-fonts ()
  "glass's default faces from the payload's bytes (glass/text would read them
   from scribe's directory, and a Mac booted from its disk has no files)."
  (let ((r (%payload-file "LiberationSans-Regular.ttf"))
        (b (%payload-file "LiberationSans-Bold.ttf")))
    (when r (setf glass::*font-regular* (scribe:open-font r)))
    (when b (setf glass::*font-bold* (scribe:open-font b)))
    (and r b t)))

(defun desk-blit (fb x0 y0 x1 y1)
  (let ((px (glass::fb-pixels fb)) (w (glass::fb-width fb)))
    (loop for y from (max 0 y0) below (min y1 *screen-h*)
          do (let ((row (+ *screen-base* (* y *screen-pitch*))) (src (* y w)))
               (loop for x from (max 0 x0) below (min x1 *screen-w*)
                     do (setf (mem-ref (+ row (* x 4)) :u32) (aref px (+ src x))))))))

;;; --- the pointer ---------------------------------------------------------------
(defparameter *arrow*
  '("2          " "22         " "212        " "2112       " "21112      "
    "211112     " "2111112    " "21111112   " "211111112  " "2111111112 "
    "21111122222" "2112112    " "212 2112   " "22  2112   " "2    2112  " "     222   "))
(defvar *under* (make-array 176 :initial-element 0))
(defvar *under-at* nil)

(defun %spx (x y) (+ *screen-base* (* y *screen-pitch*) (* x 4)))

(defun pointer-hide ()
  (when *under-at*
    (destructuring-bind (x . y) *under-at*
      (dotimes (r 16)
        (dotimes (c 11)
          (let ((sx (+ x c)) (sy (+ y r)))
            (when (and (< sx *screen-w*) (< sy *screen-h*))
              (setf (mem-ref (%spx sx sy) :u32) (svref *under* (+ (* r 11) c))))))))
    (setq *under-at* nil)))

(defun pointer-show (x y)
  (dotimes (r 16)
    (let ((line (nth r *arrow*)))
      (dotimes (c 11)
        (let ((sx (+ x c)) (sy (+ y r)))
          (when (and (< sx *screen-w*) (< sy *screen-h*))
            (let ((a (%spx sx sy)))
              (setf (svref *under* (+ (* r 11) c)) (mem-ref a :u32))
              (case (char line c)
                (#\2 (setf (mem-ref a :u32) #x000000))
                (#\1 (setf (mem-ref a :u32) #xFFFFFF)))))))))
  (setq *under-at* (cons x y)))

;;; --- the desk ------------------------------------------------------------------
(defvar *fb* nil)
(defvar *wm* nil)
(defvar *desk-turns* 0)

(defun desk-make ()
  (desk-fonts)
  (setq *fb* (glass:make-framebuffer *screen-w* *screen-h* 0)
        *wm* (glass.wm:make-wm *fb*))
  ;; the desk's applications on the root menu (Media needs a player: not here)
  (dolist (a glass.desk:*standard-apps*)
    (unless (string= (first a) "Media")
      (glass.wm:wm-register-app *wm* (first a) (second a)
                                :width (getf (cddr a) :width 400) :height (getf (cddr a) :height 300))))
  (glass.wm:wm-redraw *wm*)
  *wm*)

(defun desk-present ()
  (let ((d (nth-value 1 (glass:fb-take-frame *fb*))))
    (when d
      (pointer-hide)
      (if (eq d :full)
          (desk-blit *fb* 0 0 *screen-w* *screen-h*)
          (desk-blit *fb* (first d) (second d) (third d) (fourth d)))
      t)))

(defun desk-turn ()
  (mac-input-poll)
  (let ((moved nil))
    (loop for e = (mac-next-event) while e
          do (case (first e)
               (:pointer (glass.wm:wm-pointer *wm* (second e) (third e) (fourth e)) (setq moved t))
               (:key (glass.wm:wm-key *wm* (second e) (third e)))))
    (glass.wm:wm-tick *wm*)
    (let ((painted (desk-present)))
      (when (or painted moved (null *under-at*))
        (pointer-hide)
        (pointer-show *mac-mouse-x* *mac-mouse-y*))))
  (incf *desk-turns*))

(defun desk (&optional turns)
  "glass.wm on the screen: TURNS turns, or for ever."
  (unless *wm*
    (cuda-init) (adb-reset) (cuda-autopoll t)
    (desk-make))
  (if turns
      (dotimes (i turns) (desk-turn))
      (loop (desk-turn))))

;; At boot: the desk once, so the screen shows it, then the REPL (DESK runs it).
(handler-case (progn (desk 1) (modus-int::write-string-serial "glass.wm: up") (modus-int::write-char-serial 10))
  (error (c) (modus-int::write-string-serial "glass.wm: ") (princ c) (modus-int::write-char-serial 10)))
