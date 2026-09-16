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
  ;; Bound by the BCM SYSTEM TIMER (0x3F003004, a free-running 1 MHz microsecond
  ;; counter) rather than get-internal-real-time — the latter did NOT advance on
  ;; some boots, so the routine ground for minutes on the iteration cap.  The
  ;; system timer always advances; (logand ... #xFFFFFFFF) handles its 32-bit wrap.
  (let ((start (mem-ref #x3F003004 :u32)) (lim (* ms 1000)))
    (loop
      (when (not (hvs-8bit-p)) (return t))
      (when (> (logand (- (mem-ref #x3F003004 :u32) start) #xFFFFFFFF) lim)
        (return nil)))))

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

(defun hvs-rt ()
  "DRIVE-ONLY diagnostic — no release, no spin-wait, so it CANNOT hang (the atomic
   hvs-regtest/hvs-overlay-on reliably wedge the board).  The caller polls DISPCTRL
   over serial and calls this the instant the window is open.  Writes DISPBKGND + a
   SRAM slot + DISPLIST, then reads DISPCTRL ALONGSIDE them in one form: if :ctrl is
   32-bit (window still open) yet :bg / :dl read back 0x646472xx while :slot is the
   full 0x80000000, control-register writes genuinely don't stick -> ARM-side path
   cannot render."
  (hvs-wr #x44 (logior #x01000000 #x00FF00))
  (hvs-slot-wr 900 #x80000000)
  (hvs-wr #x20 900)
  (list :ctrl (hvs-rd #x00) :bg (hvs-rd #x44) :slot (hvs-slot-rd 900) :dl (hvs-rd #x20)))

;;; --- NATIVE BLIT into the firmware's live scanout FB (2026-09-16) ------------
;;; The compiled u32-per-iteration loops (hvs-fill, hdmi-fill-rect) cost ~100 ns
;;; per pixel — 227 ms for a 1920x1200 frame — and leave dirty lines the HVS
;;; never sees (it reads DRAM at the PoC; %jit-icache-flush only cleans to the
;;; PoU).  These two routines are hand-assembled AArch64 poked into an exec page
;;; (the same vehicle as the 128-bit STP register writes): 64 bytes/iteration
;;; through Q0/Q1 with a DC CVAC per line, DSB at the end.  Only X0-X3 and Q0/Q1
;;; are touched, so nothing the JIT keeps live is clobbered.  Measured on the
;;; Zero 2 W (BCM system timer): FILL 9.2 MB = 19.4 ms (475 MB/s); COPY 9.2 MB
;;; src->FB = 52 ms (176 MB/s).  Args travel through a scratch block:
;;;   fill: [dst u64][len u64][16-byte pixel pattern]      len multiple of 64
;;;   copy: [dst u64][src u64][len u64]                    len multiple of 64
;;; Requires (setq *jit-hot-only* nil) — %jit-call has no interpreter arm.

(defun hvs-scanout-fb ()
  "Physical address of the firmware's LIVE scanout framebuffer: PTR0 of the
   active plane on the channel the pixelvalve is fed from (channel 1 on this
   firmware), alias bits stripped.  Pitch is plane word 6 (7680 for 1920x1200)."
  (let ((slot (hvs-rd (+ (hvs-base) #x34))))          ; DISPLACT1
    (logand (hvs-rd (+ (hvs-base) #x2000 (* (+ slot 4) 4))) #x3FFFFFFF)))

(defun hvs-blit-words (kind scr)
  "Instruction words for KIND (:fill or :copy) reading its args at SCR."
  (let ((movz (logior #xD2800003 (ash (logand scr #xFFFF) 5)))
        (movk (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))))
    (if (eq kind :fill)
        ;; ldr x0,[x3]; ldr x1,[x3,#8]; ldr q0,[x3,#16]
        ;; L: stp q0,q0,[x0]; stp q0,q0,[x0,#32]; dc cvac,x0; add x0,#64; subs x1,#64; b.ne L
        ;; dsb sy; ret
        (list movz movk #xF9400060 #xF9400461 #x3DC00460
              #xAD000000 #xAD010000 #xD50B7A20 #x91010000 #xF1010021 #x54FFFF61
              #xD5033F9F #xD65F03C0)
        ;; ldr x0,[x3]; ldr x1,[x3,#8]; ldr x2,[x3,#16]
        ;; L: ldp q0,q1,[x1]; stp q0,q1,[x0]; ldp q0,q1,[x1,#32]; stp q0,q1,[x0,#32]
        ;;    dc cvac,x0; add x0,#64; add x1,#64; subs x2,#64; b.ne L
        ;; dsb sy; ret
        (list movz movk #xF9400060 #xF9400461 #xF9400862
              #xAD400420 #xAD000400 #xAD410420 #xAD010400
              #xD50B7A20 #x91010000 #x91010021 #xF1010042 #x54FFFF01
              #xD5033F9F #xD65F03C0))))

(defvar *hvs-blit* nil)   ; (code-page scratch fill-entry copy-entry)

(defun hvs-blit-init ()
  "Assemble both routines into a fresh exec page; returns (code scr fill copy)."
  (let* ((code (%mmap-exec-page 4096)) (scr (%mmap-exec-page 4096))
         (fill code) (copy (+ code 256)))
    (let ((p fill)) (dolist (w (hvs-blit-words :fill scr))
                      (setf (mem-ref p :u32) w) (setq p (+ p 4))))
    (let ((p copy)) (dolist (w (hvs-blit-words :copy scr))
                      (setf (mem-ref p :u32) w) (setq p (+ p 4))))
    (%jit-icache-flush code 512)
    (setq *hvs-blit* (list code scr fill copy))))

(defun hvs-scr-u64 (a v)
  (setf (mem-ref a :u32) (logand v #xFFFFFFFF))
  (setf (mem-ref (+ a 4) :u32) (logand (ash v -32) #xFFFFFFFF)))

(defun hvs-nfill (dst bytes color)
  "Native fill: BYTES (multiple of 64) at DST with 0x00RRGGBB COLOR, coherent."
  (when (null *hvs-blit*) (hvs-blit-init))
  (let ((scr (cadr *hvs-blit*)))
    (hvs-scr-u64 scr dst) (hvs-scr-u64 (+ scr 8) bytes)
    (let ((i 0)) (loop (when (>= i 4) (return nil))
      (setf (mem-ref (+ scr 16 (* i 4)) :u32) color) (setq i (+ i 1))))
    (%jit-call (caddr *hvs-blit*))))

(defun hvs-ncopy (dst src bytes)
  "Native copy: BYTES (multiple of 64) from SRC to DST, DST made coherent."
  (when (null *hvs-blit*) (hvs-blit-init))
  (let ((scr (cadr *hvs-blit*)))
    (hvs-scr-u64 scr dst) (hvs-scr-u64 (+ scr 8) src) (hvs-scr-u64 (+ scr 16) bytes)
    (%jit-call (cadddr *hvs-blit*))))

(defun hvs-frame (src)
  "Blit one full 1920x1200 RGBA frame at SRC onto the live scanout (~52 ms)."
  (hvs-ncopy (hvs-scanout-fb) src 9216000))

;;; --- HARDWARE DOUBLE BUFFER (2026-09-16) ---------------------------------------
;;; The HVS re-reads its display list every frame, and the live plane's PTR0 word
;;; sits at an 8-aligned dlist slot (1636+4 -> 0x3F4039A0), so ONE 128-bit STP
;;; retargets the scanout to another buffer at the next vsync.  Measured: 1-2 us.
;;; The buffer must be coherent (hvs-nfill/hvs-ncopy, or a non-cacheable mapping)
;;; and is given as a bus address (0xC0000000 | phys), like the firmware's own.
;;; Routine: ldr x0,[x3]; ldr x1,[x3,#8]; ldr x2,[x3,#16]; stp x1,x2,[x0]; dsb; ret
;;; scratch: [PTR0 reg addr][bus addr][0xC0C0C0C0 (ptr-context scratch word)]

(defvar *hvs-flip* nil)   ; (entry scratch)

(defun hvs-flip-init ()
  (let* ((code (%mmap-exec-page 4096)) (scr (+ code 256))
         (words (list (logior #xD2800003 (ash (logand scr #xFFFF) 5))
                      (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))
                      #xF9400060 #xF9400461 #xF9400862 #xA9000801 #xD5033F9F #xD65F03C0))
         (p code))
    (dolist (w words) (setf (mem-ref p :u32) w) (setq p (+ p 4)))
    (%jit-icache-flush code 64)
    (let ((slot (hvs-rd (+ (hvs-base) #x34))))
      (hvs-scr-u64 scr (+ (hvs-base) #x2000 (* (+ slot 4) 4)))
      (hvs-scr-u64 (+ scr 16) #xC0C0C0C0))
    (setq *hvs-flip* (list code scr))))

(defun hvs-flip (phys)
  "Scan out the coherent 1920x1200 RGBA buffer at PHYS from the next vsync (~1 us)."
  (when (null *hvs-flip*) (hvs-flip-init))
  (hvs-scr-u64 (+ (cadr *hvs-flip*) 8) (logior #xC0000000 phys))
  (%jit-call (car *hvs-flip*)))

;;; --- NON-CACHEABLE BACK BUFFERS: the general fast case (2026-09-16) -----------
;;; With the back buffer mapped Normal-Non-Cacheable the CPU's STP stream goes
;;; straight to DRAM through the write buffer: no DC CVAC pass, nothing for the
;;; HVS to miss.  Measured on the Zero 2 W: full 1920x1200 fill 9.75 ms (the
;;; DRAM ceiling), 640x360 fill 0.99 ms, cached->NC copy 15.5 ms full / 1.2 ms
;;; for 640x360, flip 1 us.  Remap at runtime (EL2; the boot's identity table
;;; has L1 at 0x70000 and 2 MB L2 blocks, MAIR attr0=Normal-WB attr1=Device):
;;;   1. DC CIVAC every line of the range (a stale dirty line evicted later
;;;      would land on top of NC writes),
;;;   2. rewrite each 2 MB block descriptor from AttrIdx0 (0x701) to AttrIdx2
;;;      (0x709) and clean the table lines,
;;;   3. MAIR_EL2 := 0x4400FF (attr2 = 0x44 Normal NC), TLBI ALLE2, DSB, ISB.
;;; Everything in the touched 2 MB blocks becomes NC, so give buffers their own
;;; blocks (mmap 9.2 MB each; the tail of a neighbour just gets slower).

(defun hvs-civac-words (scr)
  ;; ldr x0,[x3]; ldr x1,[x3,#8]; L: dc civac,x0; add x0,#64; subs x1,#64; b.ne L; dsb; ret
  (list (logior #xD2800003 (ash (logand scr #xFFFF) 5))
        (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))
        #xF9400060 #xF9400461 #xD50B7E20 #x91010000 #xF1010021 #x54FFFFA1
        #xD5033F9F #xD65F03C0))

(defun hvs-mair-words (scr)
  ;; ldr x0,[x3]; dsb sy; msr mair_el2,x0; tlbi alle2; dsb sy; isb; ret
  (list (logior #xD2800003 (ash (logand scr #xFFFF) 5))
        (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))
        #xF9400060 #xD5033F9F #xD51CA200 #xD50C871F #xD5033F9F #xD5033FDF #xD65F03C0))

(defun hvs-map-nc (phys bytes)
  "Remap the 2 MB blocks covering [PHYS, PHYS+BYTES) Normal-Non-Cacheable.
   Returns the number of blocks remapped, or NIL if a descriptor is not the
   identity Normal-WB block the boot installs (then nothing is touched)."
  (let* ((code (%mmap-exec-page 4096)) (scr (+ code 512))
         (civac code) (mairw (+ code 256))
         (l2 (logand (mem-ref #x70000 :u32) (lognot #xFFF)))
         (b0 (ash phys -21)) (b1 (ash (+ phys bytes -1) -21)) (b b0) (ok t))
    (let ((p civac)) (dolist (w (hvs-civac-words scr)) (setf (mem-ref p :u32) w) (setq p (+ p 4))))
    (let ((p mairw)) (dolist (w (hvs-mair-words (+ scr 16))) (setf (mem-ref p :u32) w) (setq p (+ p 4))))
    (%jit-icache-flush code 512)
    (loop (when (> b b1) (return nil))
      (when (/= (mem-ref (+ l2 (* 8 b)) :u32) (logior (ash b 21) #x701)) (setq ok nil))
      (setq b (+ b 1)))
    (when ok
      (hvs-scr-u64 scr (ash b0 21))
      (hvs-scr-u64 (+ scr 8) (ash (- (+ b1 1) b0) 21))
      (%jit-call civac)
      (setq b b0)
      (loop (when (> b b1) (return nil))
        (setf (mem-ref (+ l2 (* 8 b)) :u32) (logior (ash b 21) #x709))
        (setq b (+ b 1)))
      (%jit-icache-flush (+ l2 (* 8 b0)) (* 8 (- (+ b1 1) b0)))
      (hvs-scr-u64 (+ scr 16) #x4400FF)
      (%jit-call mairw)
      (- (+ b1 1) b0))))

(defun hvs-blit-nc-words (kind scr)
  "Like hvs-blit-words but without the DC CVAC — for NC destinations."
  (let ((movz (logior #xD2800003 (ash (logand scr #xFFFF) 5)))
        (movk (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))))
    (if (eq kind :fill)
        (list movz movk #xF9400060 #xF9400461 #x3DC00460
              #xAD000000 #xAD010000 #x91010000 #xF1010021 #x54FFFF81
              #xD5033F9F #xD65F03C0)
        (list movz movk #xF9400060 #xF9400461 #xF9400862
              #xAD400420 #xAD000400 #xAD410420 #xAD010400
              #x91010000 #x91010021 #xF1010042 #x54FFFF21
              #xD5033F9F #xD65F03C0))))

(defvar *hvs-blit-nc* nil)   ; (code scratch fill-entry copy-entry)

(defun hvs-blit-nc-init ()
  (let* ((code (%mmap-exec-page 4096)) (scr (+ code 512)) (fill code) (copy (+ code 256)))
    (let ((p fill)) (dolist (w (hvs-blit-nc-words :fill scr)) (setf (mem-ref p :u32) w) (setq p (+ p 4))))
    (let ((p copy)) (dolist (w (hvs-blit-nc-words :copy scr)) (setf (mem-ref p :u32) w) (setq p (+ p 4))))
    (%jit-icache-flush code 512)
    (setq *hvs-blit-nc* (list code scr fill copy))))

(defun hvs-nfill-nc (dst bytes color)
  "Fill an NC-mapped buffer: 9.2 MB in 9.75 ms, 640x360 in 0.99 ms."
  (when (null *hvs-blit-nc*) (hvs-blit-nc-init))
  (let ((scr (cadr *hvs-blit-nc*)))
    (hvs-scr-u64 scr dst) (hvs-scr-u64 (+ scr 8) bytes)
    (let ((i 0)) (loop (when (>= i 4) (return nil))
      (setf (mem-ref (+ scr 16 (* i 4)) :u32) color) (setq i (+ i 1))))
    (%jit-call (caddr *hvs-blit-nc*))))

(defun hvs-ncopy-nc (dst src bytes)
  "Copy a cached source into an NC-mapped buffer: 9.2 MB in 15.5 ms."
  (when (null *hvs-blit-nc*) (hvs-blit-nc-init))
  (let ((scr (cadr *hvs-blit-nc*)))
    (hvs-scr-u64 scr dst) (hvs-scr-u64 (+ scr 8) src) (hvs-scr-u64 (+ scr 16) bytes)
    (%jit-call (cadddr *hvs-blit-nc*))))

(defun hvs-double-buffer ()
  "Allocate two NC 1920x1200 back buffers; returns (a b).  Render into one with
   hvs-nfill-nc/hvs-ncopy-nc, then (hvs-flip it) — present costs ~1 us."
  (let ((a (%mmap-exec-page 9216000)) (b (%mmap-exec-page 9216000)))
    (hvs-map-nc a 9216000) (hvs-map-nc b 9216000)
    (list a b)))

;;; --- THE 4:1 CRUX, SOLVED (2026-09-16): it was the Device write path ----------
;;; With the HVS window mapped Device-nGnRnE (the boot default) a u32 store
;;; latches ONE byte, a u64 two, a 128-bit STP the low 32 bits of each 64-bit
;;; beat — so 4-mod-8 slots were unreachable.  With the HVS's 2 MB block
;;; temporarily mapped Normal-Non-Cacheable, a plain u32 store latches ALL 32
;;; bits at ANY slot (slot 2201 = 0x12345678 read back under Device).  READS
;;; under the NC mapping are garbage (Normal-memory reads get merged into
;;; bursts the bridge does not serve; DISPLACT1 read 0), so the protocol is:
;;;   (hvs-window-nc t) -> u32 writes -> (hvs-window-nc nil) -> read/verify.
;;; The PTR0 flip (an even slot) still works under Device via hvs-flip.

(defvar *hvs-attr* nil)   ; (code mairw-entry scratch)

(defun hvs-window-nc (nc)
  "Map the HVS's 2 MB block Normal-NC (NC true) or back to Device (NC nil)."
  (when (null *hvs-attr*)
    (let* ((code (%mmap-exec-page 4096)) (scr (+ code 256)) (p code))
      (dolist (w (hvs-mair-words scr)) (setf (mem-ref p :u32) w) (setq p (+ p 4)))
      (%jit-icache-flush code 256)
      (setq *hvs-attr* (list code code scr))))
  (let* ((l2 (logand (mem-ref #x70000 :u32) (lognot #xFFF)))
         (blk (ash (hvs-base) -21)) (e (+ l2 (* 8 blk))))
    (hvs-scr-u64 e (logior (ash blk 21) (if nc #x409 #x405)))
    (%jit-icache-flush e 8)
    (hvs-scr-u64 (caddr *hvs-attr*) #x4400FF)
    (%jit-call (cadr *hvs-attr*))))

(defun hvs-slot-wr32 (slot w)
  "Write a full 32-bit dlist word.  Caller brackets with (hvs-window-nc t/nil)."
  (setf (mem-ref (+ (hvs-base) #x2000 (* slot 4)) :u32) w))

;;; --- HARDWARE-SCALED PLANE (2026-09-16): 640x360 -> 1920x1200 on screen -------
;;; Word order per Linux vc4_plane_mode_set (non-unity RGB, PPF both axes):
;;;   ctl0 (VALID|SIZE=16|format/order bits from the firmware plane, UNITY clear,
;;;   SCL0=SCL1=0 H-PPF/V-PPF), pos0 (alpha 0xFF, x, y), pos1 (dst h<<16|w),
;;;   pos2 (ALPHA_MODE_FIXED<<30 | src h<<16 | w), pos3 ctx, ptr0, ptr ctx,
;;;   pitch, LBM base (0), H-PPF (AGC|(src<<16/dst)<<8), V-PPF, ctx,
;;;   4 x kernel offset, then END.  The PPF kernel (Mitchell-Netravali B=C=1/3,
;;;   11 words: 6 linear-phase words then the first 5 reversed) lives in dlist
;;;   SRAM at *hvs-kernel-slot*.  Switch the channel with DISPLIST1 (0x24).

(defvar *hvs-kernel-slot* 2100)
(defvar *hvs-plane-slot* 2000)

(defun hvs-ppf-word (c0 c1 c2)
  (logior (logand c0 #x1ff) (ash (logand c1 #x1ff) 9) (ash (logand c2 #x1ff) 18)))

(defun hvs-upload-kernel ()
  "Upload the 11-word PPF kernel at *hvs-kernel-slot* (window must be NC)."
  (let* ((c '(0 -2 -6 -8 -10 -8 -3 2 18 50 82 119 155 187 213 227))
         (k6 (list (hvs-ppf-word (nth 0 c) (nth 1 c) (nth 2 c))
                   (hvs-ppf-word (nth 3 c) (nth 4 c) (nth 5 c))
                   (hvs-ppf-word (nth 6 c) (nth 7 c) (nth 8 c))
                   (hvs-ppf-word (nth 9 c) (nth 10 c) (nth 11 c))
                   (hvs-ppf-word (nth 12 c) (nth 13 c) (nth 14 c))
                   (hvs-ppf-word (nth 15 c) (nth 15 c) 0)))
         (i 0))
    (loop (when (>= i 11) (return nil))
      (hvs-slot-wr32 (+ *hvs-kernel-slot* i) (nth (if (< i 6) i (- 10 i)) k6))
      (setq i (+ i 1)))))

(defun hvs-scaled-plane (phys sw sh pitch dw dh)
  "Compose at *hvs-plane-slot* a plane scanning the SWxSH RGBA buffer at PHYS,
   PPF-upscaled by the HVS to DWxDH at (0,0), and switch channel 1 to it.
   Copies format/order/alpha bits from the firmware's own plane."
  (let* ((fw (hvs-rd (+ (hvs-base) #x34)))
         (fctl0 (hvs-rd (+ (hvs-base) #x2000 (* fw 4))))
         (fpos0 (hvs-rd (+ (hvs-base) #x2000 (* (+ fw 1) 4))))
         (fpos2 (hvs-rd (+ (hvs-base) #x2000 (* (+ fw 2) 4))))
         (ppf-h (logior (ash 1 30) (ash (floor (* 65536 sw) dw) 8)))
         (ppf-v (logior (ash 1 30) (ash (floor (* 65536 sh) dh) 8)))
         (ks *hvs-kernel-slot*)
         (words (list (logior (logand fctl0 (lognot (logior #x10 #x3F000000 (ash 7 5) (ash 7 8))))
                              (ash 16 24))
                      (logand fpos0 #xFF000000)
                      (logior (ash dh 16) dw)
                      (logior (logand fpos2 #xF0000000) (ash sh 16) sw)
                      #xC0C0C0C0 (logior #xC0000000 phys) #xC0C0C0C0 pitch
                      0 ppf-h ppf-v #xC0C0C0C0 ks ks ks ks #x80000000))
         (i 0))
    (hvs-window-nc t)
    (hvs-upload-kernel)
    (dolist (w words) (hvs-slot-wr32 (+ *hvs-plane-slot* i) w) (setq i (+ i 1)))
    (setf (mem-ref (+ (hvs-base) #x24) :u32) *hvs-plane-slot*)   ; DISPLIST1
    (hvs-window-nc nil)
    (hvs-rd (+ (hvs-base) #x34))))
