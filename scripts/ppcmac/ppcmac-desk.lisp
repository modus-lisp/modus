;;;; ppcmac-desk.lisp -- glass.wm on the PowerPC Mac's own screen.
;;;;
;;;; The last entry of the boot payload (scripts/ppcmac-disk.lisp --payload):
;;;; glass, scribe and glass/wm-core are already loaded (natively, under the
;;;; image's PPC tier), and ppcmac-input.lisp's CUDA/ADB keyboard and mouse.
;;;; The framebuffer is the one Open Firmware set up, as the boot stub found it
;;;; (boot-ppcmac.lisp's boot info at #x07F80000: its VA under the stub's BATs,
;;;; width, height, bytes a row, depth).  At 32 bits a pixel is glass's own
;;;; (XRGB in a big-endian word); at 16, 1-5-5-5; at 8, an index into the 6x6x6
;;;; colour cube the stub put in the CLUT (36r + 6g + b).
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
(defvar *screen-depth* 32)
(defvar *cube* (let ((v (make-array 256)))
                 (dotimes (i 256 v) (setf (svref v i) (floor (+ i 25) 51)))))

(defun screen-from-boot-info ()
  "The screen the boot stub found; NIL when there is none to draw on."
  (when (and (= (mem-ref #x07F80000 :u16) #x5050) (/= (mac-boot-info 4) 0))
    (setq *screen-base* (mac-boot-info 4) *screen-w* (mac-boot-info 8)
          *screen-h* (mac-boot-info 12) *screen-pitch* (mac-boot-info 16)
          *screen-depth* (mac-boot-info 20)
          *mac-screen-w* *screen-w* *mac-screen-h* *screen-h*
          *mac-mouse-x* (floor *screen-w* 2) *mac-mouse-y* (floor *screen-h* 2))
    t))

(declaim (inline screen-put screen-get))
(defun screen-put (a xrgb)
  (case *screen-depth*
    (32 (setf (mem-ref a :u32) xrgb))
    (16 (setf (mem-ref a :u16) (logior (ash (ldb (byte 5 19) xrgb) 10)
                                       (ash (ldb (byte 5 11) xrgb) 5)
                                       (ldb (byte 5 3) xrgb))))
    (t (setf (mem-ref a :u8) (+ (* 36 (svref *cube* (ldb (byte 8 16) xrgb)))
                                (* 6 (svref *cube* (ldb (byte 8 8) xrgb)))
                                (svref *cube* (ldb (byte 8 0) xrgb)))))))
(defun screen-get (a)
  (case *screen-depth* (32 (mem-ref a :u32)) (16 (mem-ref a :u16)) (t (mem-ref a :u8))))
(defun screen-raw-put (a v)
  (case *screen-depth* (32 (setf (mem-ref a :u32) v)) (16 (setf (mem-ref a :u16) v))
    (t (setf (mem-ref a :u8) v))))

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
    ;; the listener's monospace face (glass/term reads it from glass's fonts/)
    (let ((m (%payload-file "IosevkaTerm-Regular.ttf")))
      (when m (setf glass-term::*term-fonts* (list (scribe:open-font m)))))
    (and r b t)))

(defun desk-blit (fb x0 y0 x1 y1)
  (let ((px (glass::fb-pixels fb)) (w (glass::fb-width fb)) (bpp (floor *screen-depth* 8)))
    (loop for y from (max 0 y0) below (min y1 *screen-h*)
          do (let ((row (+ *screen-base* (* y *screen-pitch*))) (src (* y w)))
               (if (= bpp 4)
                   (loop for x from (max 0 x0) below (min x1 *screen-w*)
                         do (setf (mem-ref (+ row (* x 4)) :u32) (aref px (+ src x))))
                   (loop for x from (max 0 x0) below (min x1 *screen-w*)
                         do (screen-put (+ row (* x bpp)) (aref px (+ src x)))))))))

;;; --- the pointer ---------------------------------------------------------------
(defparameter *arrow*
  '("2          " "22         " "212        " "2112       " "21112      "
    "211112     " "2111112    " "21111112   " "211111112  " "2111111112 "
    "21111122222" "2112112    " "212 2112   " "22  2112   " "2    2112  " "     222   "))
(defvar *under* (make-array 176 :initial-element 0))
(defvar *under-at* nil)

(defun %spx (x y) (+ *screen-base* (* y *screen-pitch*) (* x (floor *screen-depth* 8))))

(defun pointer-hide ()
  (when *under-at*
    (destructuring-bind (x . y) *under-at*
      (dotimes (r 16)
        (dotimes (c 11)
          (let ((sx (+ x c)) (sy (+ y r)))
            (when (and (< sx *screen-w*) (< sy *screen-h*))
              (screen-raw-put (%spx sx sy) (svref *under* (+ (* r 11) c))))))))
    (setq *under-at* nil)))

(defun pointer-show (x y)
  (dotimes (r 16)
    (let ((line (nth r *arrow*)))
      (dotimes (c 11)
        (let ((sx (+ x c)) (sy (+ y r)))
          (when (and (< sx *screen-w*) (< sy *screen-h*))
            (let ((a (%spx sx sy)))
              (setf (svref *under* (+ (* r 11) c)) (screen-get a))
              (case (char line c)
                (#\2 (screen-put a #x000000))
                (#\1 (screen-put a #xFFFFFF)))))))))
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
  ;; Lisp: a listener in a window.  No listener thread (glass-repl:*listener* NIL):
  ;; a form is evaluated in place, on this thread -- the image's main one
  (glass.wm:wm-register-app *wm* "Lisp" 'glass-repl:repl-app :width 640 :height 400)
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

(defun serial-byte-waiting-p ()
  "A byte has arrived on the ESCC's channel A (RR0 bit 0): someone at a serial
   console wants the REPL."
  (logtest (mem-ref #x08013020 :u8) 1))

(defun desk (&optional turns)
  "glass.wm on the screen: TURNS turns, or until a byte arrives on the serial
   port (it is left for the REPL to read)."
  (unless *wm*
    (unless (screen-from-boot-info) (error "no screen: the firmware named no framebuffer"))
    (mac-input-init)
    (desk-make))
  (if turns
      (dotimes (i turns) (desk-turn))
      (loop (desk-turn) (when (serial-byte-waiting-p) (return)))))

;; At boot: the desk.  With a serial console (the firmware's was one: boot info
;; +48) it turns once, so the screen shows it, and the REPL follows -- (DESK) runs
;; it from there.  Without one (a PowerBook: its screen was the console) nobody
;; could type at that REPL, so the desk keeps the machine.
(handler-case (progn (desk 1)
                     (modus-int::write-string-serial "glass.wm: up") (modus-int::write-char-serial 10)
                     (when (and (= (mem-ref #x07F80000 :u16) #x5050) (= (mac-boot-info 48) 0))
                       ;; the screen is the console: open a listener on it
                       (glass.wm:wm-open *wm* "Lisp")
                       (desk)))
  (error (c) (modus-int::write-string-serial "glass.wm: ") (princ c) (modus-int::write-char-serial 10)))
