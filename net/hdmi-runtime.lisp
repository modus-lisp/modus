;;;; hdmi-runtime.lisp — runtime display helpers for the Zero, pushed over
;;;; serial after each boot (runtime defuns do not survive a reset; the baked
;;;; hdmi-* primitives in board-hdmi.img do).  Push with:  push.py hdmi-runtime.lisp
;;;;
;;;; Layout: a TALL framebuffer — 1920x1080 displayed (rows 0..1079), virtual
;;;; height 2160.  Rows 1080..2159 are OFF-SCREEN and do double duty:
;;;;   * the hardware-pan reservoir (setoff scrolls the display window down into
;;;;     them), and
;;;;   * the FLUSH scratch: a baked 8MB fill there evicts every dirty on-screen
;;;;     cache line to DRAM, so drawn pixels become coherent with no dc-cvac.

;;; --- tall framebuffer allocation (phys w x h shown, virtual w x vh) ---------
;;; mailbox tags: 0x48003=294915 phys, 0x48004=294916 virt, 0x48005=294917 depth,
;;; 0x48006=294918 pixel order, 0x40001=262145 allocate, 0x40008=262152 pitch
(defun fbtall (w h vh)
  (let ((b (hdmi-mbox-buf)))
    (let ((j 0)) (loop (when (>= j 140) (return nil)) (hdmi-wr (+ b j) 0) (setq j (+ j 4))))
    (hdmi-wr (+ b 0) 140) (hdmi-wr (+ b 4) 0)
    (hdmi-wr (+ b 8) 294915) (hdmi-wr (+ b 12) 8) (hdmi-wr (+ b 20) w) (hdmi-wr (+ b 24) h)
    (hdmi-wr (+ b 28) 294916) (hdmi-wr (+ b 32) 8) (hdmi-wr (+ b 40) w) (hdmi-wr (+ b 44) vh)
    (hdmi-wr (+ b 48) 294917) (hdmi-wr (+ b 52) 4) (hdmi-wr (+ b 60) 32)
    (hdmi-wr (+ b 64) 294918) (hdmi-wr (+ b 68) 4) (hdmi-wr (+ b 76) 0)
    (hdmi-wr (+ b 80) 262145) (hdmi-wr (+ b 84) 8) (hdmi-wr (+ b 92) 16)
    (hdmi-wr (+ b 100) 262152) (hdmi-wr (+ b 104) 4)
    (let ((i 0)) (loop (when (> i 1000000) (return 0))
      (when (zerop (logand (hdmi-rd (+ (hdmi-mbox-base) 56)) 2147483648)) (return nil))
      (setq i (+ i 1))))
    (hdmi-wr (hdmi-mbox-write) (logior (hdmi-mbox-buf-bus) 8))
    (let ((i 0)) (loop (when (> i 1000000) (return 0))
      (when (zerop (logand (hdmi-rd (hdmi-mbox-status)) 1073741824)) (hdmi-rd (hdmi-mbox-read)) (return nil))
      (setq i (+ i 1))))
    (let ((fb (logand (hdmi-rd (+ b 92)) 1073741823)) (pit (hdmi-rd (+ b 112))))
      (hdmi-wr (+ (hdmi-fb-state) 0) fb) (hdmi-wr (+ (hdmi-fb-state) 8) pit)
      (hdmi-wr (+ (hdmi-fb-state) 12) w) (hdmi-wr (+ (hdmi-fb-state) 16) h)
      (list fb pit))))

;;; --- hardware pan: which virtual row is the top of the display -------------
;;; tag 0x48009=294921 set-virtual-offset [x y]
(defun setoff (oy)
  (let ((b (hdmi-mbox-buf)))
    (hdmi-wr (+ b 0) 32) (hdmi-wr (+ b 4) 0)
    (hdmi-wr (+ b 8) 294921) (hdmi-wr (+ b 12) 8) (hdmi-wr (+ b 16) 0)
    (hdmi-wr (+ b 20) 0) (hdmi-wr (+ b 24) oy) (hdmi-wr (+ b 28) 0)
    (let ((i 0)) (loop (when (> i 100000) (return 0))
      (when (zerop (logand (hdmi-rd (+ (hdmi-mbox-base) 56)) 2147483648)) (return nil))
      (setq i (+ i 1))))
    (hdmi-wr (hdmi-mbox-write) (logior (hdmi-mbox-buf-bus) 8))
    (let ((i 0)) (loop (when (> i 100000) (return 0))
      (when (zerop (logand (hdmi-rd (hdmi-mbox-status)) 1073741824)) (hdmi-rd (hdmi-mbox-read)) (return nil))
      (setq i (+ i 1))))
    oy))

;;; --- FLUSH: make on-screen drawing coherent (use at pan offset 0) ----------
;;; A baked 8MB fill of the off-screen rows evicts all dirty on-screen lines.
(defun flush () (hdmi-fill-rect 0 1080 1920 1080 0) nil)

;;; --- init: tall 1080p buffer, offset 0, cleared + flushed -------------------
(defun init ()
  (fbtall 1920 1080 2160)
  (setoff 0)
  (hdmi-fill-rect 0 0 1920 1080 (hdmi-color 12 14 34))
  (flush)
  (list (hdmi-fb-addr) (hdmi-fb-w) (hdmi-fb-h)))

;;; --- a clean drawing: rects then flush ---------------------------------------
(defun target ()
  (hdmi-fill-rect 0 0 1920 1080 (hdmi-color 12 14 34))
  (hdmi-fill-rect 260 90 1400 900 (hdmi-color 210 60 40))
  (hdmi-fill-rect 450 230 1020 620 (hdmi-color 240 210 40))
  (hdmi-fill-rect 660 360 600 360 (hdmi-color 40 180 90))
  (hdmi-fill-rect 810 440 300 200 (hdmi-color 245 245 250))
  (flush))
