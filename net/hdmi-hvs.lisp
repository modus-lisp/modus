;;;; hdmi-hvs.lisp — modus's VideoCore IV Hardware Video Scaler (HVS) driver.
;;;;
;;;; The simple mailbox framebuffer (hdmi-fb.lisp) is a linear RGB scanout: to
;;;; show decoded video the CPU must convert YUV->RGB and upscale, ~128 ms/frame
;;;; on the Zero's A53 — nowhere near 60 fps.  The HVS composites SCALED planes
;;;; in HARDWARE, including YUV->RGB colorspace conversion, straight to scanout.
;;;; So we hand it the decoder's small YUV planes at a stable address and the
;;;; hardware does the per-frame pixel work: zero CPU cost, frees the cores for
;;;; decode.  This is the "get pixels on screen really fast" path.
;;;;
;;;; SCALER_BASE = 0x3F400000 (periph 0x3F000000 + 0x400000), Device-mapped by
;;;; boot-rpi-cl.lisp, reachable with (mem-ref addr :u32) like the mailbox.
;;;; Display lists live in on-chip SRAM at 0x3F402000 (4096 dwords).  A dlist is
;;;; a run of plane entries ending at a word with bit31 (END) set; the value in
;;;; SCALER_DISPLISTx is a dword SLOT INDEX into that SRAM.
;;;;
;;;; See docs/videocore-hvs-overlay.md for the full register/bitfield reference
;;;; and the milestone plan.  THIS FILE is milestone 0: a READ-ONLY dumper that
;;;; walks the firmware's live display list.  It writes nothing, so it cannot
;;;; tear the screen or wedge the pipeline — its whole job is to confirm register
;;;; access, learn which channel the firmware drives, and recover the FB plane's
;;;; format/position/pointer so later milestones can re-emit it under our list.

(in-package :modus.mvm)

(defun hvs-base () #x3F400000)
(defun hvs-rd (off) (mem-ref (+ (hvs-base) off) :u32))
;; dlist SRAM: slot index -> absolute address.
(defun hvs-dlist-sram () (+ (hvs-base) #x2000))       ; 0x3F402000
(defun hvs-slot-addr (slot) (+ (hvs-dlist-sram) (* slot 4)))
(defun hvs-slot-rd (slot) (mem-ref (hvs-slot-addr slot) :u32))

;; Per-channel register addresses (x in 0..2).
(defun hvs-displist (x) (hvs-rd (+ #x20 (* x 4))))    ; SCALER_DISPLISTx: next-dlist slot
(defun hvs-displact  (x) (hvs-rd (+ #x30 (* x 4))))   ; SCALER_DISPLACTx: active-dlist slot
(defun hvs-dispctrlx (x) (hvs-rd (+ #x40 (* x #x10))));  SCALER_DISPCTRLX: bit31 ENABLE

(defun hvs-chan-enabled-p (x)
  (not (zerop (logand (hvs-dispctrlx x) #x80000000))))

;;; Decode one CTL0 word into a readable plist.
(defun hvs-decode-ctl0 (w)
  (list :ctl0-hex w
        :end   (if (zerop (logand w #x80000000)) 0 1)
        :valid (if (zerop (logand w #x40000000)) 0 1)
        :size  (logand (ash w -24) #x3F)     ; dwords in this plane entry
        :scl1  (logand (ash w -8) 7)
        :scl0  (logand (ash w -5) 7)
        :unity (logand (ash w -4) 1)
        :fmt   (logand w #xF)))               ; 4=RGB565 7=RGBA8888 8=YUV420-3plane

;;; Walk the dlist that starts at SLOT and return a list of plane descriptors.
;;; Stops at an END word or after MAX planes (malformed-list guard).  Read-only.
(defun hvs-walk (slot max)
  (let ((planes nil) (s slot) (n 0))
    (loop
      (when (>= n max) (return nil))
      (let* ((ctl0 (hvs-slot-rd s))
             (info (hvs-decode-ctl0 ctl0))
             (size (getf info :size)))
        ;; capture the first few following words (pos/ptr) for context
        (let ((words nil) (k 1))
          (loop (when (or (>= k size) (>= k 8)) (return nil))
            (push (hvs-slot-rd (+ s k)) words)
            (setq k (+ k 1)))
          (push (list :slot s :info info :words (nreverse words)) planes))
        (when (= 1 (getf info :end)) (return nil))
        (when (or (zerop size) (> size 32)) (return nil))  ; sanity: bail on junk
        (setq s (+ s size))
        (setq n (+ n 1))))
    (nreverse planes)))

;;; The top-level probe: report each channel's enable + dlist heads, and walk
;;; the active list of whichever channel is enabled.  Returns a plist so the
;;; serial REPL prints it as "= (...)".  Pure reads — safe on the live display.
(defun hvs-dump ()
  (let ((chans nil) (active-chan nil))
    (dotimes (x 3)
      (let ((en (hvs-chan-enabled-p x)))
        (when (and en (null active-chan)) (setq active-chan x))
        (push (list :chan x :enabled (if en 1 0)
                    :displist (hvs-displist x) :displact (hvs-displact x))
              chans)))
    (list :scaler-base (hvs-base)
          :dispctrl (hvs-rd #x00)
          :dispstat (hvs-rd #x04)
          :channels (nreverse chans)
          :active-chan active-chan
          :planes (if active-chan
                      (hvs-walk (hvs-displact active-chan) 12)
                      :no-enabled-channel))))

;;; --- power / clock enable via the property mailbox --------------------------
;;; In full KMS the firmware hands the display to the ARM but leaves it
;;; UNPOWERED/UNCLOCKED, expecting the OS driver to bring it up.  These poke the
;;; firmware SET_DOMAIN_STATE / SET_CLOCK_STATE tags to power+clock the display
;;; block.  They gave us PARTIAL HVS access — a u32 write latches only its low 8
;;; bits, reads return 0x646F43 in the top 24, with the occasional clean full
;;; read of a real DISPCTRL (0x9A0F00FF, ENABLE set).  NOT stable 32-bit access:
;;; that needs the full KMS firmware handover over VCHIQ, which the property
;;; mailbox cannot do.  See docs/videocore-hvs-notes.md for the whole story.
;;; Domain IDs: VIDEO_SCALER=3 (the HVS), HDMI=5, VEC=7.  Clock IDs: CORE=4,
;;; PIXEL=9, DISP=16 (HVS rides the always-on CORE clock per Linux's vc4).
(defun hvs-mbox-2 (tag a b)
  "One property-mailbox tag carrying a 2-word value {A,B}; returns (resp0 resp1)."
  (let ((buf (hdmi-mbox-buf)))
    (hdmi-wr (+ buf 0) 32) (hdmi-wr (+ buf 4) 0)
    (hdmi-wr (+ buf 8) tag) (hdmi-wr (+ buf 12) 8) (hdmi-wr (+ buf 16) 8)
    (hdmi-wr (+ buf 20) a) (hdmi-wr (+ buf 24) b) (hdmi-wr (+ buf 28) 0)
    (let ((i 0)) (loop (when (> i 1000000) (return 0))
                   (when (zerop (logand (hdmi-rd (+ (hdmi-mbox-base) #x38)) #x80000000)) (return nil))
                   (setq i (+ i 1))))
    (hdmi-wr (hdmi-mbox-write) (logior (hdmi-mbox-buf-bus) 8))
    (let ((i 0)) (loop (when (> i 1000000) (return 0))
                   (when (zerop (logand (hdmi-rd (hdmi-mbox-status)) #x40000000))
                     (hdmi-rd (hdmi-mbox-read)) (return nil))
                   (setq i (+ i 1))))
    (list (hdmi-rd (+ buf 20)) (hdmi-rd (+ buf 24)))))

(defun hvs-power-domain (dom on) (hvs-mbox-2 #x00038030 dom (if on 3 0)))  ; SET_DOMAIN_STATE, bit1=wait
(defun hvs-clock-state  (clk on) (hvs-mbox-2 #x00038001 clk (if on 1 0)))   ; SET_CLOCK_STATE

(defun hvs-display-on ()
  "Power+clock the display block (CORE/DISP/PIXEL clocks; VIDEO_SCALER/HDMI/VEC
   domains).  Yields only PARTIAL HVS access — see the note above."
  (hvs-clock-state 4 t) (hvs-clock-state 16 t) (hvs-clock-state 9 t)
  (hvs-power-domain 3 t) (hvs-power-domain 5 t) (hvs-power-domain 7 t))
