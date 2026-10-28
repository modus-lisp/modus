;;;; pc-console.lisp — screen + keyboard console for the bare x86-64 CL image.
;;;;
;;;; Output: every WRITE-CHAR-SERIAL compiled into the image also reaches
;;;; %CONSOLE-WRITE-CHAR (the build sets the compiler's write-char-serial hook,
;;;; as the Pi board build does for net/hdmi-console.lisp), which draws on
;;;;   - VGA text mode at #xB8000 (80x25; what a BIOS / multiboot boot shows),
;;;;   - the UEFI GOP framebuffer, when the build is a UEFI one and the boot
;;;;     stub found GOP (its words at #x1B000, below).
;;;; Input: lib/serial-repl's %CONSOLE-INIT / %CONSOLE-READ-CHAR seams, which
;;;; this file overrides by last-defun-wins: the PS/2 keyboard or COM1,
;;;; whichever has a character first.
;;;;
;;;; The port of net/uefi-console.lisp (the mini-Lisp UEFI / console-REPL
;;;; images) to the CL image.  Two things moved: the font and the scancode
;;;; tables are Lisp data baked by the build (PCC-FONT-BYTES / PCC-SCAN-*),
;;;; copied into low RAM by %CONSOLE-INIT, instead of being written by the
;;;; boot stub at #x601800 -- an address inside the CL image's own code.
;;;;
;;;; Low RAM (free: page tables #x10000, IDT #x18000, SNP status #x1A000 all
;;;; end below it).  The UEFI stub writes #x1B000..#x1B020; the rest is ours:
;;;;   #x1B000 fb_base (u64, pre-tagged)   #x1B008 pixels per scan line
;;;;   #x1B010/14 fb cursor x/y            #x1B018/1C text cols/rows
;;;;   #x1B020 fb_valid (1 if GOP found)   #x1B030/34 VGA cursor x/y
;;;;   #x1B040 PS/2 shift state            #x1B100 font (95 glyphs x 8 bytes)
;;;;   #x1B400 scancodes, unshifted        #x1B480 scancodes, shifted
;;;;
;;;; The build defines PCC-FONT-BYTES, PCC-SCAN-NORMAL, PCC-SCAN-SHIFTED and
;;;; PCC-FB-ENABLED-P before this text (mvm/build-cl-repl-common.lisp).

;;; ------------------------------------------------------------
;;; VGA text mode (80x25 at #xB8000, 2 bytes per cell: char, attribute)
;;; ------------------------------------------------------------

(defun vga-cursor-x ()  (mem-ref #x1B030 :u32))
(defun vga-cursor-y ()  (mem-ref #x1B034 :u32))
(defun vga-set-cursor-x (v) (setf (mem-ref #x1B030 :u32) v))
(defun vga-set-cursor-y (v) (setf (mem-ref #x1B034 :u32) v))

(defun vga-cell-addr (col row) (+ #xB8000 (* (+ (* row 80) col) 2)))

(defun vga-put-char (addr ch)
  (setf (mem-ref addr :u8) ch)
  (setf (mem-ref (+ addr 1) :u8) #x0F))   ; bright white on black

(defun vga-scroll ()
  ;; rows 1-24 up one (960 u32s = 2 cells each), then blank row 24
  (let ((i 0))
    (loop (when (>= i 960) (return nil))
      (setf (mem-ref (+ #xB8000 (* i 4)) :u32) (mem-ref (+ #xB80A0 (* i 4)) :u32))
      (setq i (+ i 1))))
  (let ((i 0))
    (loop (when (>= i 40) (return nil))
      (setf (mem-ref (+ #xB8F00 (* i 4)) :u32) #x0F200F20)
      (setq i (+ i 1)))))

(defun vga-newline ()
  (vga-set-cursor-x 0)
  (let ((ny (+ (vga-cursor-y) 1)))
    (if (>= ny 25)
        (progn (vga-scroll) (vga-set-cursor-y 24))
        (vga-set-cursor-y ny))))

(defun vga-sync-hw-cursor ()
  ;; Move the VGA controller's blinking cursor to ours (CRTC registers #x0E /
  ;; #x0F, the cursor's cell index high / low byte).
  (let ((pos (+ (* (vga-cursor-y) 80) (vga-cursor-x))))
    (io-out-byte #x3D4 #x0F) (io-out-byte #x3D5 (logand pos #xFF))
    (io-out-byte #x3D4 #x0E) (io-out-byte #x3D5 (logand (ash pos -8) #xFF))))

(defun vga-write-char (c)
  ;; The hook runs from the first character of boot, before %CONSOLE-INIT:
  ;; on real hardware the cursor words are whatever RAM held, and a wild
  ;; cursor would write anywhere.  Out of range means "start over".
  (unless (and (< (vga-cursor-x) 80) (< (vga-cursor-y) 25))
    (vga-set-cursor-x 0) (vga-set-cursor-y 0))
  (cond
    ((= c 10) (vga-newline))
    ((= c 13) (vga-set-cursor-x 0))
    ((= c 8)
     (let ((x (vga-cursor-x)))
       (when (> x 0)
         (vga-set-cursor-x (- x 1))
         (vga-put-char (vga-cell-addr (- x 1) (vga-cursor-y)) 32))))
    ((>= c 32)
     (vga-put-char (vga-cell-addr (vga-cursor-x) (vga-cursor-y)) c)
     (let ((nx (+ (vga-cursor-x) 1)))
       (if (>= nx 80) (vga-newline) (vga-set-cursor-x nx)))))
  (vga-sync-hw-cursor))

;;; ------------------------------------------------------------
;;; GOP framebuffer (32 bpp; one text cell = 8x8 pixels)
;;; ------------------------------------------------------------
;;; :u32 loads come back tagged and :u32 stores untag, so a raw pixel P is
;;; stored as (* P 2); fb_base is stored pre-tagged and read with :u64.

(defun fb-base ()      (mem-ref #x1B000 :u64))
(defun fb-ppsl ()      (mem-ref #x1B008 :u32))
(defun fb-cursor-x ()  (mem-ref #x1B010 :u32))
(defun fb-cursor-y ()  (mem-ref #x1B014 :u32))
(defun fb-text-cols () (mem-ref #x1B018 :u32))
(defun fb-text-rows () (mem-ref #x1B01C :u32))
(defun fb-set-cursor-x (v) (setf (mem-ref #x1B010 :u32) v))
(defun fb-set-cursor-y (v) (setf (mem-ref #x1B014 :u32) v))

(defun fb-on-p ()
  (and (pcc-fb-enabled-p) (= (mem-ref #x1B020 :u32) 1)))

(defun fb-render-glyph (code)
  ;; CODE (32..126, else a space) at the cursor
  (let* ((g (if (and (>= code 32) (<= code 126)) (- code 32) 0))
         (font (+ #x1B100 (* g 8)))
         (ppsl (fb-ppsl))
         (base (+ (fb-base) (* (+ (* (fb-cursor-y) ppsl) (fb-cursor-x)) 32)))
         (r 0))
    (loop (when (>= r 8) (return nil))
      (let ((bits (mem-ref (+ font r) :u8))
            (row (+ base (* r ppsl 4)))
            (x 0))
        (loop (when (>= x 8) (return nil))
          (setf (mem-ref (+ row (* x 4)) :u32)
                (if (zerop (logand bits (ash 128 (- x)))) 0 #x01FFFFFE))
          (setq x (+ x 1))))
      (setq r (+ r 1)))))

(defun fb-scroll ()
  ;; up one text row (8 pixel rows), then clear the last
  (let* ((fb (fb-base))
         (row-bytes (* (fb-ppsl) 32))
         (copy-bytes (* (- (fb-text-rows) 1) row-bytes))
         (i 0))
    (loop (when (>= i copy-bytes) (return nil))
      (setf (mem-ref (+ fb i) :u64) (mem-ref (+ fb row-bytes i) :u64))
      (setq i (+ i 8)))
    (let ((j 0))
      (loop (when (>= j row-bytes) (return nil))
        (setf (mem-ref (+ fb copy-bytes j) :u64) 0)
        (setq j (+ j 8))))))

(defun fb-newline ()
  (fb-set-cursor-x 0)
  (let ((ny (+ (fb-cursor-y) 1)))
    (if (>= ny (fb-text-rows))
        (progn (fb-scroll) (fb-set-cursor-y (- (fb-text-rows) 1)))
        (fb-set-cursor-y ny))))

(defun fb-write-char (c)
  ;; as VGA-WRITE-CHAR: a cursor outside the screen starts over
  (unless (and (< (fb-cursor-x) (fb-text-cols)) (< (fb-cursor-y) (fb-text-rows)))
    (fb-set-cursor-x 0) (fb-set-cursor-y 0))
  (cond
    ((= c 10) (fb-newline))
    ((= c 13) (fb-set-cursor-x 0))
    ((= c 8)
     (let ((x (fb-cursor-x)))
       (when (> x 0)
         (fb-set-cursor-x (- x 1))
         (fb-render-glyph 32))))
    ((>= c 32)
     (fb-render-glyph c)
     (let ((nx (+ (fb-cursor-x) 1)))
       (if (>= nx (fb-text-cols)) (fb-newline) (fb-set-cursor-x nx))))))

;;; ------------------------------------------------------------
;;; PS/2 keyboard and COM1
;;; ------------------------------------------------------------

(defun ps2-poll-key ()
  "The ASCII code of a key just pressed, or 0.  Tracks shift."
  (if (zerop (logand (io-in-byte #x64) 1))
      0
      (let ((scan (io-in-byte #x60)))
        (cond
          ((or (= scan #x2A) (= scan #x36)) (setf (mem-ref #x1B040 :u32) 1) 0)
          ((or (= scan #xAA) (= scan #xB6)) (setf (mem-ref #x1B040 :u32) 0) 0)
          ((> scan 127) 0)                                  ; key release
          (t (mem-ref (+ (if (zerop (mem-ref #x1B040 :u32)) #x1B400 #x1B480) scan)
                      :u8))))))

(defun pcc-serial-ready-p ()
  ;; LSR = #xFF: no UART (floating bus) -- never ready
  (let ((lsr (io-in-byte #x3FD)))
    (and (/= lsr #xFF) (= (logand lsr 1) 1))))

;;; ------------------------------------------------------------
;;; The seams
;;; ------------------------------------------------------------

(defun pcc-copy-bytes (list addr)
  (let ((p list) (i 0))
    (loop (when (null p) (return nil))
      (setf (mem-ref (+ addr i) :u8) (car p))
      (setq p (cdr p)) (setq i (+ i 1)))))

(defun %console-init ()
  (pcc-copy-bytes (pcc-font-bytes) #x1B100)
  (pcc-copy-bytes (pcc-scan-normal) #x1B400)
  (pcc-copy-bytes (pcc-scan-shifted) #x1B480)
  (setf (mem-ref #x1B040 :u32) 0)
  ;; A clean screen: what boot drew before the font was here is blanks at
  ;; best, and whatever firmware left at worst.
  (let ((i 0))
    (loop (when (>= i 1000) (return nil))
      (setf (mem-ref (+ #xB8000 (* i 4)) :u32) #x0F200F20)
      (setq i (+ i 1))))
  (vga-set-cursor-x 0) (vga-set-cursor-y 0)
  (when (fb-on-p)
    (let ((fb (fb-base)) (n (* (fb-text-rows) (fb-ppsl) 32)) (j 0))
      (loop (when (>= j n) (return nil))
        (setf (mem-ref (+ fb j) :u64) 0)
        (setq j (+ j 8))))
    (fb-set-cursor-x 0) (fb-set-cursor-y 0)))

(defun %console-write-char (code)
  ;; The write-char-serial hook: COM1 always, then the screens.
  (write-char-serial code)
  (vga-write-char code)
  (when (fb-on-p) (fb-write-char code)))

(defun %console-read-char ()
  (let ((c 0))
    (loop
      (when (pcc-serial-ready-p) (return (read-char-serial)))
      (setq c (ps2-poll-key))
      (when (> c 0) (return c)))))
