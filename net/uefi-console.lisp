;;; uefi-console.lisp — Display output + PS/2 keyboard input for UEFI x64
;;;
;;; Loaded AFTER repl-source.lisp so that write-char-output and read-char-input
;;; overrides take effect via last-defun-wins.
;;;
;;; Display: VGA text mode (always) + GOP framebuffer (if available).
;;; VGA text mode writes to 0xB8000 and works on all x86 hardware.
;;; GOP framebuffer provides higher quality graphics when UEFI GOP is present.
;;;
;;; Memory layout (set up by UEFI boot stub):
;;;   0x1B000: fb_base     (u64, pre-tagged = raw_addr << 1)
;;;   0x1B008: ppsl        (u32, raw pixels-per-scan-line)
;;;   0x1B00C: pixel_fmt   (u32, raw 0=RGBX, 1=BGRX)
;;;   0x1B010: cursor_x    (u32, raw text column — framebuffer)
;;;   0x1B014: cursor_y    (u32, raw text row — framebuffer)
;;;   0x1B018: text_cols   (u32, raw)
;;;   0x1B01C: text_rows   (u32, raw)
;;;   0x1B020: fb_valid    (u32, 1 if GOP found)
;;;   0x1B030: vga_cursor_x (u32, raw text column — VGA text mode)
;;;   0x1B034: vga_cursor_y (u32, raw text row — VGA text mode)
;;;   0x1B100: font data   (95 chars × 8 bytes)
;;;   0x601800: scancode normal  (128 bytes)
;;;   0x601880: scancode shifted (128 bytes)
;;;   0x601900: shift_state (u32, 0 or 1)

;;; ============================================================
;;; VGA text mode output (80x25, buffer at 0xB8000)
;;; ============================================================
;;; Each cell: 2 bytes = char + attribute.
;;; Attribute 0x0F = bright white on black.

(defun vga-cursor-x ()  (mem-ref #x1B030 :u32))
(defun vga-cursor-y ()  (mem-ref #x1B034 :u32))
(defun vga-set-cursor-x (v) (setf (mem-ref #x1B030 :u32) v))
(defun vga-set-cursor-y (v) (setf (mem-ref #x1B034 :u32) v))

(defun vga-cell-addr (col row)
  (let ((c col) (r row))
    (+ #xB8000 (* (+ (* r 80) c) 2))))

(defun vga-put-char (addr ch)
  (let ((a addr) (c ch))
    (setf (mem-ref a :u8) c)
    (setf (mem-ref (+ a 1) :u8) #x0F)))

(defun vga-scroll-copy ()
  ;; Copy rows 1-24 to rows 0-23 using :u32 (4 bytes = 2 cells at a time)
  ;; 24 rows * 160 bytes/row = 3840 bytes = 960 u32s
  (let ((i 0))
    (loop
      (when (>= i 960) (return 0))
      (let ((off (* i 4)))
        (let ((src (+ #xB80A0 off)))
          (let ((dst (+ #xB8000 off)))
            (let ((val (mem-ref src :u32)))
              (setf (mem-ref dst :u32) val)))))
      (setq i (+ i 1)))))

(defun vga-scroll-clear ()
  ;; Clear last row (row 24): 40 u32s of 0x0F200F20 (2 cells each)
  (let ((i 0))
    (loop
      (when (>= i 40) (return 0))
      (let ((addr (+ #xB8F00 (* i 4))))
        (setf (mem-ref addr :u32) #x0F200F20))
      (setq i (+ i 1)))))

(defun vga-scroll ()
  (vga-scroll-copy)
  (vga-scroll-clear))

(defun vga-newline ()
  (vga-set-cursor-x 0)
  (let ((ny (+ (vga-cursor-y) 1)))
    (if (>= ny 25)
        (progn (vga-scroll)
               (vga-set-cursor-y 24))
      (vga-set-cursor-y ny))))

(defun vga-advance-cursor ()
  (let ((nx (+ (vga-cursor-x) 1)))
    (if (>= nx 80)
        (vga-newline)
      (vga-set-cursor-x nx))))

(defun vga-write-char (ch)
  (let ((c ch))
    (cond
      ((= c 10) (vga-newline))
      ((= c 13) (vga-set-cursor-x 0))
      ((= c 8)
       (let ((x (vga-cursor-x)))
         (when (> x 0)
           (let ((nx (- x 1)))
             (vga-set-cursor-x nx)
             (let ((y (vga-cursor-y)))
               (let ((addr (vga-cell-addr nx y)))
                 (vga-put-char addr 32)))))))
      (t
       (when (>= c 32)
         (let ((cx (vga-cursor-x)))
           (let ((cy (vga-cursor-y)))
             (let ((addr (vga-cell-addr cx cy)))
               (vga-put-char addr c))))
         (vga-advance-cursor))))))

;;; ============================================================
;;; GOP framebuffer info accessors
;;; ============================================================
;;; Integer values stored raw by UEFI stub.
;;; :u32 loads auto-tag (SHL 1), :u32 stores auto-untag (SHR 1).
;;; fb_base stored pre-tagged, loaded with :u64 (raw bits = tagged addr).

(defun fb-base ()      (mem-ref #x1B000 :u64))
(defun fb-ppsl ()      (mem-ref #x1B008 :u32))
(defun fb-cursor-x ()  (mem-ref #x1B010 :u32))
(defun fb-cursor-y ()  (mem-ref #x1B014 :u32))
(defun fb-text-cols () (mem-ref #x1B018 :u32))
(defun fb-text-rows () (mem-ref #x1B01C :u32))
(defun fb-valid ()     (mem-ref #x1B020 :u32))

(defun fb-set-cursor-x (v) (setf (mem-ref #x1B010 :u32) v))
(defun fb-set-cursor-y (v) (setf (mem-ref #x1B014 :u32) v))

;;; ============================================================
;;; Pixel writing
;;; ============================================================
;;; White pixel: raw 0x00FFFFFF via :u32 store.
;;; :u32 stores value >> 1, so pass 0x00FFFFFF << 1 = 0x01FFFFFE.

(defun fb-put-white (addr)
  (let ((a addr))
    (setf (mem-ref a :u32) #x01FFFFFE)))

(defun fb-put-black (addr)
  (let ((a addr))
    (setf (mem-ref a :u32) 0)))

;;; ============================================================
;;; Font row rendering
;;; ============================================================
;;; Font byte: bit 7 = leftmost pixel, bit 0 = rightmost.
;;; Tagged arithmetic: (logand byte 128) tests bit 7 correctly
;;; because both operands are shifted left by 1 (tag), preserving
;;; the relative bit positions.

(defun fb-render-4px-left (addr byte)
  (let ((a addr) (b byte))
    (if (> (logand b 128) 0) (fb-put-white a) (fb-put-black a))
    (let ((a1 (+ a 4)))
      (if (> (logand b 64) 0) (fb-put-white a1) (fb-put-black a1))
      (let ((a2 (+ a 8)))
        (if (> (logand b 32) 0) (fb-put-white a2) (fb-put-black a2))
        (let ((a3 (+ a 12)))
          (if (> (logand b 16) 0) (fb-put-white a3) (fb-put-black a3)))))))

(defun fb-render-4px-right (addr byte)
  (let ((a addr) (b byte))
    (if (> (logand b 8) 0) (fb-put-white a) (fb-put-black a))
    (let ((a1 (+ a 4)))
      (if (> (logand b 4) 0) (fb-put-white a1) (fb-put-black a1))
      (let ((a2 (+ a 8)))
        (if (> (logand b 2) 0) (fb-put-white a2) (fb-put-black a2))
        (let ((a3 (+ a 12)))
          (if (> (logand b 1) 0) (fb-put-white a3) (fb-put-black a3)))))))

(defun fb-render-font-row (addr byte)
  (let ((a addr) (b byte))
    (fb-render-4px-left a b)
    (fb-render-4px-right (+ a 16) b)))

;;; ============================================================
;;; Glyph rendering
;;; ============================================================

(defun fb-font-addr (ch)
  (let ((c ch))
    (if (< c 32)
        #x1B100
      (if (> c 126)
          #x1B100
        (+ #x1B100 (* (- c 32) 8))))))

(defun fb-text-addr (col row)
  ;; byte_offset = (row * ppsl + col) * 32
  ;; (each text cell is 8x8 pixels, 4 bytes per pixel = 32 bytes per cell width)
  (let ((c col) (r row))
    (let ((ppsl (fb-ppsl)))
      (let ((fb (fb-base)))
        (let ((idx (+ (* r ppsl) c)))
          (+ fb (* idx 32)))))))

(defun fb-render-glyph (base-addr font-addr ppsl)
  ;; Render 8x8 glyph: 8 rows, each row offset by ppsl*4 bytes
  (let ((ba base-addr) (fa font-addr) (p ppsl))
    (let ((row-stride (* p 4)))
      (let ((i 0))
        (loop
          (when (>= i 8) (return 0))
          (let ((byte (mem-ref (+ fa i) :u8)))
            (let ((row-addr (+ ba (* i row-stride))))
              (fb-render-font-row row-addr byte)))
          (setq i (+ i 1)))))))

(defun fb-render-char-at-cursor (ch)
  (let ((c ch))
    (let ((addr (fb-text-addr (fb-cursor-x) (fb-cursor-y))))
      (let ((fa (fb-font-addr c)))
        (fb-render-glyph addr fa (fb-ppsl))))))

;;; ============================================================
;;; Scrolling
;;; ============================================================

(defun fb-scroll-copy (dst src count8)
  ;; Copy count8 qwords from src to dst using :u64
  (let ((d dst) (s src) (n count8))
    (let ((i 0))
      (loop
        (when (>= i n) (return 0))
        (let ((off (* i 8)))
          (let ((val (mem-ref (+ s off) :u64)))
            (setf (mem-ref (+ d off) :u64) val)))
        (setq i (+ i 1))))))

(defun fb-scroll-clear (addr count8)
  ;; Zero count8 qwords at addr
  (let ((a addr) (n count8))
    (let ((i 0))
      (loop
        (when (>= i n) (return 0))
        (setf (mem-ref (+ a (* i 8)) :u64) 0)
        (setq i (+ i 1))))))

(defun fb-scroll ()
  ;; Scroll up by one text row (8 pixel rows)
  (let ((fb (fb-base)))
    (let ((ppsl (fb-ppsl)))
      (let ((trows (fb-text-rows)))
        (let ((row-bytes (* ppsl 32)))
          (let ((copy-bytes (* (- trows 1) row-bytes)))
            (let ((count8 (truncate copy-bytes 8)))
              (fb-scroll-copy fb (+ fb row-bytes) count8)
              (fb-scroll-clear (+ fb copy-bytes)
                               (truncate row-bytes 8)))))))))

;;; ============================================================
;;; Cursor movement
;;; ============================================================

(defun fb-newline ()
  (fb-set-cursor-x 0)
  (let ((ny (+ (fb-cursor-y) 1)))
    (if (>= ny (fb-text-rows))
        (progn (fb-scroll)
               (fb-set-cursor-y (- (fb-text-rows) 1)))
      (fb-set-cursor-y ny))))

(defun fb-advance-cursor ()
  (let ((nx (+ (fb-cursor-x) 1)))
    (if (>= nx (fb-text-cols))
        (fb-newline)
      (fb-set-cursor-x nx))))

;;; ============================================================
;;; Character output to framebuffer
;;; ============================================================

(defun fb-write-char (ch)
  (let ((c ch))
    (cond
      ((= c 10) (fb-newline))
      ((= c 13) (fb-set-cursor-x 0))
      ((= c 8)
       (let ((x (fb-cursor-x)))
         (when (> x 0)
           (fb-set-cursor-x (- x 1))
           (fb-render-char-at-cursor 32))))
      (t
       (when (>= c 32)
         (fb-render-char-at-cursor c)
         (fb-advance-cursor))))))

;;; ============================================================
;;; write-char-output override: serial + VGA + framebuffer
;;; ============================================================

(defun write-char-output (c)
  (let ((ch c))
    (write-char-serial ch)
    (vga-write-char ch)
    (when (> (fb-valid) 0)
      (fb-write-char ch))))

;;; ============================================================
;;; PS/2 keyboard input
;;; ============================================================

(defun ps2-data-ready ()
  (> (logand (io-in-byte #x64) 1) 0))

(defun ps2-read-scancode ()
  (io-in-byte #x60))

(defun ps2-shift-state () (mem-ref #x601900 :u32))
(defun ps2-set-shift (v) (setf (mem-ref #x601900 :u32) v))

(defun ps2-scancode-to-char (code)
  (let ((c code))
    (if (> (ps2-shift-state) 0)
        (mem-ref (+ #x601880 c) :u8)
      (mem-ref (+ #x601800 c) :u8))))

(defun ps2-poll-key ()
  ;; Returns ASCII char or 0 if no key available.
  ;; Handles shift state internally.
  (if (ps2-data-ready)
      (let ((scan (ps2-read-scancode)))
        (cond
          ;; Left shift press
          ((= scan #x2A) (ps2-set-shift 1) 0)
          ;; Right shift press
          ((= scan #x36) (ps2-set-shift 1) 0)
          ;; Left shift release
          ((= scan #xAA) (ps2-set-shift 0) 0)
          ;; Right shift release
          ((= scan #xB6) (ps2-set-shift 0) 0)
          ;; Key release (bit 7 set) — ignore
          ((> scan 127) 0)
          ;; Key press — look up in scancode table
          (t (ps2-scancode-to-char scan))))
    0))

;;; ============================================================
;;; Serial port non-blocking check
;;; ============================================================

(defun serial-data-ready ()
  ;; LSR=0xFF means no UART hardware (floating bus) — ignore
  (let ((lsr (io-in-byte #x3FD)))
    (if (= lsr #xFF) nil
      (> (logand lsr 1) 0))))

(defun serial-read-byte ()
  (io-in-byte #x3F8))

;;; ============================================================
;;; read-char-input override: PS/2 keyboard + serial
;;; ============================================================

(defun read-char-input ()
  ;; Block until a character is available from either source
  (loop
    (let ((key (ps2-poll-key)))
      (when (> key 0) (return key)))
    (when (serial-data-ready)
      (return (serial-read-byte)))))
