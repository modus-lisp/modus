;;;; hdmi-console.lisp - a text console on the Zero 2 W's HDMI output
;;;;
;;;; Everything the board writes with WRITE-CHAR-SERIAL also appears on the
;;;; screen: the board net build sets the compiler's write-char-serial hook to
;;;; %CONSOLE-WRITE-CHAR (mvm/compiler.lisp), which sends the byte to the UART
;;;; and draws it here.  Together with net/usb-hid-split.lisp's keyboard input
;;;; that is a REPL usable with no serial cable.
;;;;
;;;; The framebuffer is the firmware's (property mailbox), 640 wide in the
;;;; display's aspect ratio (640x400 on this 1920x1200 screen, 640x360 when the
;;;; firmware reports no size) -- the firmware scales it up -- so 80 columns of
;;;; the 8x8 font (boot/boot-uefi-x64.lisp's,
;;;; generated into the build as HCON-FONT-BYTES).  It is remapped
;;;; Normal-Non-Cacheable (the same block rewrite as net/hdmi-hvs.lisp's
;;;; HVS-MAP-NC, measured there), so plain pixel stores reach DRAM and the
;;;; scanout sees them without any cache maintenance; clears and scrolls use
;;;; that file's native NC fill/copy loops.
;;;;
;;;; The ready word lives in Device RAM, which survives a board reset, so the
;;;; board kernel prologue zeroes it before the first character is written --
;;;; otherwise the first boot banner would be drawn into a stale framebuffer.

(defun hcon-st () #x111F0800)
(defun hcon-get (off) (mem-ref (+ (hcon-st) off) :u32))
(defun hcon-put (off v) (setf (mem-ref (+ (hcon-st) off) :u32) v))
(defun hcon-magic () #x48434F4E)
(defun hcon-ready-p () (= (hcon-get 0) (hcon-magic)))
;; +0 ready  +4 fb  +8 pitch  +C width  +10 height  +14 cols  +18 rows
;; +1C cx  +20 cy  +24 fg  +28 bg  +2C native code page  +30 scratch
(defun hcon-font-base () #x111F1000)        ; 95 glyphs x 8 row bytes

(defun hcon-u64 (a v)
  (setf (mem-ref a :u32) (logand v #xFFFFFFFF))
  (setf (mem-ref (+ a 4) :u32) (logand (ash v -32) #xFFFFFFFF)))

;;; --- mailbox ----------------------------------------------------------------

(defun hcon-mbox-call ()
  ;; Post the property buffer at (hdmi-mbox-buf) and wait for the answer.
  (let ((i 0))
    (loop (when (> i 1000000) (return nil))
      (when (zerop (logand (hdmi-rd (+ (hdmi-mbox-base) #x38)) #x80000000)) (return nil))
      (setq i (+ i 1))))
  (hdmi-wr (hdmi-mbox-write) (logior (hdmi-mbox-buf-bus) 8))
  (let ((i 0))
    (loop (when (> i 1000000) (return nil))
      (when (zerop (logand (hdmi-rd (hdmi-mbox-status)) #x40000000))
        (hdmi-rd (hdmi-mbox-read)) (return nil))
      (setq i (+ i 1)))))

(defun hcon-clear-mbox (n)
  (let ((b (hdmi-mbox-buf)) (j 0))
    (loop (when (>= j n) (return nil)) (hdmi-wr (+ b j) 0) (setq j (+ j 4)))))

(defun hcon-display-size ()
  ;; GET_PHYSICAL_DISPLAY (0x40003): (values w h), 0 if the firmware says nothing.
  (let ((b (hdmi-mbox-buf)))
    (hcon-clear-mbox 32)
    (hdmi-wr b 32) (hdmi-wr (+ b 8) #x40003) (hdmi-wr (+ b 12) 8)
    (hcon-mbox-call)
    (values (hdmi-rd (+ b 20)) (hdmi-rd (+ b 24)))))

(defun hcon-alloc (w h)
  ;; Physical = virtual = W x H, 32 bpp; returns the framebuffer address or 0.
  (let ((b (hdmi-mbox-buf)))
    (hcon-clear-mbox 140)
    (hdmi-wr (+ b 0) 140)
    (hdmi-wr (+ b 8) #x48003) (hdmi-wr (+ b 12) 8) (hdmi-wr (+ b 20) w) (hdmi-wr (+ b 24) h)
    (hdmi-wr (+ b 28) #x48004) (hdmi-wr (+ b 32) 8) (hdmi-wr (+ b 40) w) (hdmi-wr (+ b 44) h)
    (hdmi-wr (+ b 48) #x48005) (hdmi-wr (+ b 52) 4) (hdmi-wr (+ b 60) 32)
    (hdmi-wr (+ b 64) #x48006) (hdmi-wr (+ b 68) 4) (hdmi-wr (+ b 76) 0)
    (hdmi-wr (+ b 80) #x40001) (hdmi-wr (+ b 84) 8) (hdmi-wr (+ b 92) 16)
    (hdmi-wr (+ b 100) #x40008) (hdmi-wr (+ b 104) 4)
    (hcon-mbox-call)
    (let ((fb (logand (hdmi-rd (+ b 92)) #x3FFFFFFF)) (pitch (hdmi-rd (+ b 112))))
      (hcon-put #x04 fb) (hcon-put #x08 pitch)
      (hdmi-wr (+ (hdmi-fb-state) 0) fb) (hdmi-wr (+ (hdmi-fb-state) 8) pitch)
      (hdmi-wr (+ (hdmi-fb-state) 12) w) (hdmi-wr (+ (hdmi-fb-state) 16) h)
      fb)))

;;; --- native code: NC remap, NC fill, NC copy (net/hdmi-hvs.lisp) -----------

;; Instruction words, copied from net/hdmi-hvs.lisp (not in the image).
(defun hcon-civac-words (scr)
  ;; ldr x0,[x3]; ldr x1,[x3,#8]; L: dc civac,x0; add x0,#64; subs x1,#64; b.ne L; dsb; ret
  (list (logior #xD2800003 (ash (logand scr #xFFFF) 5))
        (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))
        #xF9400060 #xF9400461 #xD50B7E20 #x91010000 #xF1010021 #x54FFFFA1
        #xD5033F9F #xD65F03C0))
(defun hcon-mair-words (scr)
  ;; ldr x0,[x3]; dsb sy; msr mair_el2,x0; tlbi alle2; dsb sy; isb; ret
  (list (logior #xD2800003 (ash (logand scr #xFFFF) 5))
        (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))
        #xF9400060 #xD5033F9F #xD51CA200 #xD50C871F #xD5033F9F #xD5033FDF #xD65F03C0))
(defun hcon-blit-nc-words (kind scr)
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


(defun hcon-code () (hcon-get #x2C))
(defun hcon-scr () (+ (hcon-code) 1536))

(defun hcon-poke (p words)
  (dolist (w words) (setf (mem-ref p :u32) w) (setq p (+ p 4))))

(defun hcon-native-init ()
  ;; One exec page: civac @0, mair @256, fill @512, copy @768, scratch @1536.
  (let* ((code (%mmap-exec-page 4096)) (scr (+ code 1536)))
    (hcon-put #x2C code)
    (hcon-poke code (hcon-civac-words scr))
    (hcon-poke (+ code 256) (hcon-mair-words (+ scr 16)))
    (hcon-poke (+ code 512) (hcon-blit-nc-words :fill scr))
    (hcon-poke (+ code 768) (hcon-blit-nc-words :copy scr))
    (%jit-icache-flush code 1024)
    code))

(defun hcon-map-nc (phys bytes)
  ;; HVS-MAP-NC on our own code page.  Blocks remapped, or 0 if a descriptor
  ;; is not the boot's identity Normal-WB block (then nothing is touched).
  (let* ((scr (hcon-scr))
         (l2 (logand (mem-ref #x70000 :u32) (lognot #xFFF)))
         (b0 (ash phys -21)) (b1 (ash (+ phys bytes -1) -21)) (b b0) (ok t))
    (loop (when (> b b1) (return nil))
      (let ((d (mem-ref (+ l2 (* 8 b)) :u32)))
        (when (and (/= d (logior (ash b 21) #x701)) (/= d (logior (ash b 21) #x709)))
          (setq ok nil)))
      (setq b (+ b 1)))
    (if (not ok)
        0
        (progn
          (hcon-u64 scr (ash b0 21))
          (hcon-u64 (+ scr 8) (ash (- (+ b1 1) b0) 21))
          (%jit-call (hcon-code))
          (setq b b0)
          (loop (when (> b b1) (return nil))
            (setf (mem-ref (+ l2 (* 8 b)) :u32) (logior (ash b 21) #x709))
            (setq b (+ b 1)))
          (%jit-icache-flush (+ l2 (* 8 b0)) (* 8 (- (+ b1 1) b0)))
          (hcon-u64 (+ scr 16) #x4400FF)
          (%jit-call (+ (hcon-code) 256))
          (- (+ b1 1) b0)))))

(defun hcon-nfill (dst bytes color)
  ;; BYTES a multiple of 64.
  (let ((scr (hcon-scr)) (i 0))
    (hcon-u64 scr dst) (hcon-u64 (+ scr 8) bytes)
    (loop (when (>= i 4) (return nil))
      (setf (mem-ref (+ scr 16 (* i 4)) :u32) color) (setq i (+ i 1)))
    (%jit-call (+ (hcon-code) 512))))

(defun hcon-ncopy (dst src bytes)
  ;; Forward copy, BYTES a multiple of 64; safe for DST < SRC.
  (let ((scr (hcon-scr)))
    (hcon-u64 scr dst) (hcon-u64 (+ scr 8) src) (hcon-u64 (+ scr 16) bytes)
    (%jit-call (+ (hcon-code) 768))))

;;; --- text -------------------------------------------------------------------

(defun hcon-glyph (col row code fg bg)
  ;; Draw CODE (32..126; anything else is a space) in cell COL,ROW.
  (let* ((fb (hcon-get #x04)) (pitch (hcon-get #x08))
         (g (if (and (>= code 32) (<= code 126)) (- code 32) 0))
         (f (+ (hcon-font-base) (* g 8)))
         (p (+ fb (* row 8 pitch) (* col 32)))
         (r 0))
    (loop (when (>= r 8) (return nil))
      (let ((bits (mem-ref (+ f r) :u8)) (x 0))
        (loop (when (>= x 8) (return nil))
          (setf (mem-ref (+ p (* x 4)) :u32)
                (if (zerop (logand bits (ash #x80 (- x)))) bg fg))
          (setq x (+ x 1))))
      (setq p (+ p pitch))
      (setq r (+ r 1)))))

(defun hcon-cursor (on)
  (let ((cx (hcon-get #x1C)) (cy (hcon-get #x20)))
    (when (< cx (hcon-get #x14))
      (if on
          (hcon-glyph cx cy 32 (hcon-get #x28) (hcon-get #x24))
          (hcon-glyph cx cy 32 (hcon-get #x24) (hcon-get #x28))))))

(defun hcon-scroll ()
  ;; Move rows 1.. up one text row and clear the last.
  (let* ((fb (hcon-get #x04)) (pitch (hcon-get #x08)) (rows (hcon-get #x18))
         (line (* 8 pitch)))
    (hcon-ncopy fb (+ fb line) (* (- rows 1) line))
    (hcon-nfill (+ fb (* (- rows 1) line)) line (hcon-get #x28))))

(defun hcon-newline ()
  (hcon-put #x1C 0)
  (if (< (+ (hcon-get #x20) 1) (hcon-get #x18))
      (hcon-put #x20 (+ (hcon-get #x20) 1))
      (hcon-scroll)))

(defun hcon-write (code)
  (hcon-cursor nil)
  (cond
    ((= code 10) (hcon-newline))
    ((= code 13) (hcon-put #x1C 0))
    ((or (= code 8) (= code 127))
     (when (> (hcon-get #x1C) 0) (hcon-put #x1C (- (hcon-get #x1C) 1))))
    ((= code 9)
     (let ((n (- 8 (logand (hcon-get #x1C) 7))) (i 0))
       (loop (when (>= i n) (return nil)) (hcon-write 32) (setq i (+ i 1)))))
    ((and (>= code 32) (<= code 126))
     (when (>= (hcon-get #x1C) (hcon-get #x14)) (hcon-newline))
     (hcon-glyph (hcon-get #x1C) (hcon-get #x20) code (hcon-get #x24) (hcon-get #x28))
     (hcon-put #x1C (+ (hcon-get #x1C) 1)))
    (t nil))
  (hcon-cursor t))

(defun hcon-init ()
  ;; Allocate 640 wide with the display's aspect ratio (the firmware reports
  ;; its size before any allocation: 1920x1200 -> 640x400, 1920x1080 ->
  ;; 640x360), or 640x360 when it reports nothing; the firmware scales it up.
  ;; Remap it NC, clear.  Returns (cols rows), or NIL with no display.
  (hcon-put 0 0)
  (multiple-value-bind (dw dh) (hcon-display-size)
  (let ((w 640)
        (h (if (and (> dw 0) (> dh 0))
               (min 480 (max 200 (logand (floor (* 640 dh) dw) (lognot 7))))
               360)))
    (let ((fb (hcon-alloc w h)))
      (if (zerop fb)
          nil
          (progn
            (let ((i 0) (f (hcon-font-bytes)))
              (loop (when (null f) (return nil))
                (setf (mem-ref (+ (hcon-font-base) i) :u8) (car f))
                (setq f (cdr f)) (setq i (+ i 1))))
            (hcon-native-init)
            (hcon-map-nc fb (* h (hcon-get #x08)))
            (hcon-put #x0C w) (hcon-put #x10 h)
            (hcon-put #x14 (floor w 8)) (hcon-put #x18 (floor h 8))
            (hcon-put #x1C 0) (hcon-put #x20 0)
            (hcon-put #x24 #xD8D8D8) (hcon-put #x28 #x101418)
            (hcon-nfill fb (* h (hcon-get #x08)) (hcon-get #x28))
            (hcon-put 0 (hcon-magic))
            (hcon-cursor t)
            (list (hcon-get #x14) (hcon-get #x18))))))))

(defun %console-write-char (code)
  ;; The write-char-serial hook: the UART always, the screen once it is up.
  ;; (This function is exempt from the hook, so this is the raw trap.)
  (write-char-serial code)
  (when (hcon-ready-p) (hcon-write code))
  0)
