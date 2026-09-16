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
(defun hvs-wr (off val) (setf (mem-ref (+ (hvs-base) off) :u32) val))
;; dlist SRAM: slot index -> absolute address.
(defun hvs-dlist-sram () (+ (hvs-base) #x2000))       ; 0x3F402000
(defun hvs-slot-addr (slot) (+ (hvs-dlist-sram) (* slot 4)))
(defun hvs-slot-rd (slot) (mem-ref (hvs-slot-addr slot) :u32))
(defun hvs-slot-wr (slot val) (setf (mem-ref (hvs-slot-addr slot) :u32) val))

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

;;; --- MILESTONE 1: atomic release-and-drive (2026-09-13) ----------------------
;;; The firmware pins the HVS to an 8-bit ARM view while it owns the framebuffer
;;; (reads come back 0x646472xx, only bit[7:0] is a real register).  The property
;;; mailbox FRAMEBUFFER_RELEASE hands the block over at full 32-bit — but the
;;; handover is ASYNCHRONOUS (~1s) and the firmware RECLAIMS an idle display ~1s
;;; later, AND lingering released-idle destabilizes the RTL8153 net.  So driving
;;; the HVS cannot be an interactive multi-step dance: it must be ONE atomic
;;; routine that releases, waits for the 32-bit window, then immediately writes our
;;; dlist and keeps the channel enabled, so nothing idle is left to reclaim.  This
;;; is exactly what mainline vc4 does.  See docs/videocore-hvs-notes.md.

(defun hvs-8bit-p ()
  "T while the HVS still reads the firmware-owned 8-bit byte-lane artifact (top 24
   bits stuck at 0x646472).  NIL once the ARM owns it at full 32-bit."
  (= (logand (hvs-rd #x00) #xFFFFFF00) #x64647200))

(defun hvs-wait-window (ms)
  "After a FB release, wait (bounded by REAL time via get-internal-real-time, so
   it is immune to interpret-vs-JIT speed) until the HVS reads 32-bit.  Returns T
   if the window opened within MS milliseconds, NIL on timeout."
  (let ((deadline (+ (get-internal-real-time)
                     (truncate (* ms internal-time-units-per-second) 1000)))
        (i 0))
    (loop
      (when (not (hvs-8bit-p)) (return t))
      (when (> (get-internal-real-time) deadline) (return nil))
      ;; hard iteration cap — a belt-and-suspenders guard so a stuck/unadvancing
      ;; timer can NEVER hang the routine (a hang here wedged the board once).
      (when (> i 200000000) (return nil))
      (setq i (+ i 1)))))

(defun hvs-active-channel ()
  "Which display channel the firmware left ENABLEd (bit31 of DISPCTRLX).  Meaningful
   only while the window is open (32-bit).  Defaults to 0 if none reads enabled."
  (cond ((not (zerop (logand (hvs-dispctrlx 0) #x80000000))) 0)
        ((not (zerop (logand (hvs-dispctrlx 1) #x80000000))) 1)
        ((not (zerop (logand (hvs-dispctrlx 2) #x80000000))) 2)
        (t 0)))

(defun hvs-rel-fb ()
  "Property-mailbox FRAMEBUFFER_RELEASE (tag 0x00048001): hand the display block
   from the firmware to the ARM.  Asynchronous — must be paired with hvs-wait-window
   and an immediate dlist write (see hvs-overlay-on)."
  (let ((buf (hdmi-mbox-buf)))
    (hdmi-wr (+ buf 0) 24) (hdmi-wr (+ buf 4) 0)
    (hdmi-wr (+ buf 8) #x00048001) (hdmi-wr (+ buf 12) 0)
    (hdmi-wr (+ buf 16) 0) (hdmi-wr (+ buf 20) 0)
    (let ((i 0)) (loop (when (> i 1000000) (return 0))
      (when (zerop (logand (hdmi-rd (+ (hdmi-mbox-base) #x38)) #x80000000)) (return nil))
      (setq i (+ i 1))))
    (hdmi-wr (hdmi-mbox-write) (logior (hdmi-mbox-buf-bus) 8))
    (let ((i 0)) (loop (when (> i 1000000) (return 0))
      (when (zerop (logand (hdmi-rd (hdmi-mbox-status)) #x40000000))
        (hdmi-rd (hdmi-mbox-read)) (return nil))
      (setq i (+ i 1))))
    (hdmi-rd (+ buf 16))))

(defun hvs-fill (phys n color)
  "Fill N consecutive u32 words at physical PHYS with COLOR (a 0xAARRGGBB word)."
  (let ((i 0))
    (loop (when (>= i n) (return nil))
      (setf (mem-ref (+ phys (* i 4)) :u32) color)
      (setq i (+ i 1)))))

(defun hvs-plane (slot ctl0 pos0 pos2 ptr pitch)
  "Write a 7-dword unity plane entry + END terminator into the dlist SRAM at SLOT.
   Word order (VC-IV unity, POS1 omitted): CTL0, POS0, POS2, POS3-scratch, PTR0,
   ptr-context, SRC_PITCH, then END(bit31)."
  (hvs-slot-wr (+ slot 0) ctl0)
  (hvs-slot-wr (+ slot 1) pos0)
  (hvs-slot-wr (+ slot 2) pos2)
  (hvs-slot-wr (+ slot 3) #xC0C0C0C0)
  (hvs-slot-wr (+ slot 4) ptr)
  (hvs-slot-wr (+ slot 5) #xC0C0C0C0)
  (hvs-slot-wr (+ slot 6) pitch)
  (hvs-slot-wr (+ slot 7) #x80000000))

(defun hvs-drive ()
  "Drive our dlist NOW.  Call ONLY when the 32-bit window is open (hvs-8bit-p is
   NIL).  Fills the scratch buffer, composes a unity RGBA8888 plane over a GREEN
   background on the firmware's active channel, and points the channel at it.
   Split out of hvs-overlay-on so a caller can poll the window itself (e.g. over
   serial) instead of relying on an in-routine spin-wait."
  (let* ((ch (hvs-active-channel)) (bw 320) (bh 180) (sx 800) (sy 450)
         (buf #x12000000) (slot 900) (bk (+ #x44 (* ch #x10))) (lst (+ #x20 (* ch 4))))
    (hvs-fill buf (* bw bh) #x00FF00FF)
    (memory-barrier)
    (hvs-plane slot
               (logior #x40000000 (ash 7 24) #x10 7)
               (logior #xFF000000 (ash sy 12) sx)
               (logior (ash bh 16) bw)
               (logior #xC0000000 buf)
               (* bw 4))
    (hvs-wr bk (logior #x01000000 #x00FF00))
    (hvs-wr lst slot)
    (list :chan ch :dispctrl (hvs-rd #x00) :dispctrlx (hvs-dispctrlx ch)
          :displist (hvs-displist ch) :ctl0 (hvs-slot-rd slot))))

(defun hvs-overlay-on ()
  "MILESTONE 1: atomically take the HVS from the firmware and DRIVE it ourselves.
   Releases the FB, catches the 32-bit window, fills a small scratch buffer, and
   composes a unity RGBA8888 plane (a 320x180 magenta rect, centered on 1920x1080)
   over a dark background on the firmware's active channel.  If it holds — the
   screen shows the rect and the board stays reachable — we own the HVS and can
   hold the window with no interactive round-trip.  :win :no-window means the
   firmware never handed over (display untouched)."
  (hvs-rel-fb)
  (if (not (hvs-wait-window 12000))
      (list :win :no-window)
      (let* ((ch (hvs-active-channel)) (bw 320) (bh 180) (sx 800) (sy 450)
             (buf #x12000000) (slot 900) (bk (+ #x44 (* ch #x10))) (lst (+ #x20 (* ch 4))))
        (hvs-fill buf (* bw bh) #x00FF00FF)          ; magenta rect content
        (memory-barrier)
        (hvs-plane slot
                   (logior #x40000000 (ash 7 24) #x10 7)   ; CTL0: VALID|WORDS(7)|UNITY|FMT7(RGBA8888)
                   (logior #xFF000000 (ash sy 12) sx)       ; POS0: ALPHA|START_Y|START_X
                   (logior (ash bh 16) bw)                  ; POS2: srcH|srcW
                   (logior #xC0000000 buf)                  ; PTR0: uncached VC bus alias
                   (* bw 4))                                ; SRC_PITCH bytes
        (hvs-wr bk (logior #x01000000 #x00FF00))     ; DISPBKGND: FILL | GREEN (low byte 0 -> 8-bit miss shows black, 32-bit shows green)
        (hvs-wr lst slot)                            ; point channel's dlist at ours (latches next frame)
        (list :win :open :chan ch :slot slot :dispctrl (hvs-rd #x00)
              :dispctrlx (hvs-dispctrlx ch) :bkgnd (hvs-rd bk)
              :displist (hvs-displist ch) :ctl0 (hvs-slot-rd slot)))))

(defun hvs-regtest ()
  "DIAGNOSTIC — the make-or-break question.  In a confirmed-open window, do the HVS
   CONTROL registers hold 32-bit writes, or only the dlist SRAM?  Atomic (no serial
   gap): release, spin to the window, then write DISPBKGND + a SRAM slot + DISPLIST
   and read all three back.  If :bg / :dl come back 0x646472xx (byte-narrow) while
   :slot is the full 0x80000000, then control-register writes DON'T stick even in
   the window -> we can build a dlist in SRAM but cannot point a channel at it, and
   the ARM-side FRAMEBUFFER_RELEASE path cannot render.  Run it as the FIRST release
   after a COLD power-cycle (the window is reliable only then)."
  (hvs-rel-fb)
  (if (not (hvs-wait-window 9000))
      (list :no-window)
      (progn
        (hvs-wr #x44 (logior #x01000000 #x00FF00))   ; DISPBKGND(0) green
        (hvs-slot-wr 900 #x80000000)                 ; SRAM slot 900 = END word
        (hvs-wr #x20 900)                            ; DISPLIST0 = slot 900
        (list :ctrl (hvs-rd #x00) :bg (hvs-rd #x44)
              :slot (hvs-slot-rd 900) :dl (hvs-rd #x20)))))
