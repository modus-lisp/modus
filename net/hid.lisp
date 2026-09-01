;;;; hid.lisp - USB HID driver: Keyboard, Mouse, Touchscreen
;;;;
;;;; USB HID class driver using boot protocol (no HID report descriptor parsing).
;;;; Supports keyboard (protocol 1), mouse (protocol 2), tablet (protocol 0).
;;;; Uses DWC2 interrupt IN transfers for persistent polling.
;;;;
;;;; Depends on: dwc2.lisp (host controller), usb.lisp (control transfers)

;; ============================================================
;; HID memory layout at 0x01200000
;; ============================================================
;;
;; +0x000  HID device state (256 bytes)
;;   +0x00  kbd-devaddr, kbd-ep, kbd-mps, kbd-channel  (4xU32)
;;   +0x10  mouse-devaddr, mouse-ep, mouse-mps, mouse-channel  (4xU32)
;;   +0x20  tablet-devaddr, tablet-ep, tablet-mps, tablet-channel  (4xU32)
;;   +0x30  device-count, kbd-present, mouse-present, tablet-present  (4xU32)
;; +0x100  Keyboard DMA buffer (64 bytes, aligned)
;; +0x140  Mouse DMA buffer (64 bytes, aligned)
;; +0x180  Tablet DMA buffer (64 bytes, aligned)
;; +0x200  Keyboard ring buffer
;;   +0x200  head (U32), +0x204 tail (U32), +0x208 data (248 bytes)
;; +0x300  Mouse state: buttons (U32), x (U32), y (U32), wheel (U32)
;; +0x320  Tablet state: buttons (U32), x (U32), y (U32)
;; +0x400  Scancode-to-ASCII table, normal (256 bytes)
;; +0x500  Scancode-to-ASCII table, shifted (256 bytes)
;; +0x600  Modifier state (U32), prev-keys (8 bytes)

(defun hid-base () #x01200000)

;; ============================================================
;; Device state accessors
;; ============================================================

;; Keyboard state: +0x00
(defun hid-kbd-devaddr () (mem-ref (hid-base) :u32))
(defun hid-set-kbd-devaddr (v) (setf (mem-ref (hid-base) :u32) v))
(defun hid-kbd-ep () (mem-ref (+ (hid-base) #x04) :u32))
(defun hid-set-kbd-ep (v) (setf (mem-ref (+ (hid-base) #x04) :u32) v))
(defun hid-kbd-mps () (mem-ref (+ (hid-base) #x08) :u32))
(defun hid-set-kbd-mps (v) (setf (mem-ref (+ (hid-base) #x08) :u32) v))
(defun hid-kbd-channel () (mem-ref (+ (hid-base) #x0C) :u32))
(defun hid-set-kbd-channel (v) (setf (mem-ref (+ (hid-base) #x0C) :u32) v))

;; Mouse state: +0x10
(defun hid-mouse-devaddr () (mem-ref (+ (hid-base) #x10) :u32))
(defun hid-set-mouse-devaddr (v) (setf (mem-ref (+ (hid-base) #x10) :u32) v))
(defun hid-mouse-ep () (mem-ref (+ (hid-base) #x14) :u32))
(defun hid-set-mouse-ep (v) (setf (mem-ref (+ (hid-base) #x14) :u32) v))
(defun hid-mouse-mps () (mem-ref (+ (hid-base) #x18) :u32))
(defun hid-set-mouse-mps (v) (setf (mem-ref (+ (hid-base) #x18) :u32) v))
(defun hid-mouse-channel () (mem-ref (+ (hid-base) #x1C) :u32))
(defun hid-set-mouse-channel (v) (setf (mem-ref (+ (hid-base) #x1C) :u32) v))

;; Tablet state: +0x20
(defun hid-tablet-devaddr () (mem-ref (+ (hid-base) #x20) :u32))
(defun hid-set-tablet-devaddr (v) (setf (mem-ref (+ (hid-base) #x20) :u32) v))
(defun hid-tablet-ep () (mem-ref (+ (hid-base) #x24) :u32))
(defun hid-set-tablet-ep (v) (setf (mem-ref (+ (hid-base) #x24) :u32) v))
(defun hid-tablet-mps () (mem-ref (+ (hid-base) #x28) :u32))
(defun hid-set-tablet-mps (v) (setf (mem-ref (+ (hid-base) #x28) :u32) v))
(defun hid-tablet-channel () (mem-ref (+ (hid-base) #x2C) :u32))
(defun hid-set-tablet-channel (v) (setf (mem-ref (+ (hid-base) #x2C) :u32) v))

;; Device presence: +0x30
(defun hid-device-count () (mem-ref (+ (hid-base) #x30) :u32))
(defun hid-set-device-count (v) (setf (mem-ref (+ (hid-base) #x30) :u32) v))
(defun hid-kbd-present () (mem-ref (+ (hid-base) #x34) :u32))
(defun hid-set-kbd-present (v) (setf (mem-ref (+ (hid-base) #x34) :u32) v))
(defun hid-mouse-present () (mem-ref (+ (hid-base) #x38) :u32))
(defun hid-set-mouse-present (v) (setf (mem-ref (+ (hid-base) #x38) :u32) v))
(defun hid-tablet-present () (mem-ref (+ (hid-base) #x3C) :u32))
(defun hid-set-tablet-present (v) (setf (mem-ref (+ (hid-base) #x3C) :u32) v))

;; DMA buffers
(defun hid-kbd-dma () (+ (hid-base) #x100))
(defun hid-mouse-dma () (+ (hid-base) #x140))
(defun hid-tablet-dma () (+ (hid-base) #x180))

;; Ring buffer
(defun hid-ring-head-addr () (+ (hid-base) #x200))
(defun hid-ring-tail-addr () (+ (hid-base) #x204))
(defun hid-ring-data-addr () (+ (hid-base) #x208))
(defun hid-ring-size () 248)

;; Mouse state
(defun hid-mouse-buttons-addr () (+ (hid-base) #x300))
(defun hid-mouse-x-addr () (+ (hid-base) #x304))
(defun hid-mouse-y-addr () (+ (hid-base) #x308))
(defun hid-mouse-wheel-addr () (+ (hid-base) #x30C))

;; Tablet state
(defun hid-tablet-buttons-addr () (+ (hid-base) #x320))
(defun hid-tablet-x-addr () (+ (hid-base) #x324))
(defun hid-tablet-y-addr () (+ (hid-base) #x328))

;; Scancode tables
(defun hid-scancode-normal () (+ (hid-base) #x400))
(defun hid-scancode-shifted () (+ (hid-base) #x500))

;; Modifier state and previous keys
(defun hid-modifier-addr () (+ (hid-base) #x600))
(defun hid-prevkeys-addr () (+ (hid-base) #x604))

;; ============================================================
;; Keyboard ring buffer
;; ============================================================

(defun hid-kbd-ring-push (ch)
  (let ((head (mem-ref (hid-ring-head-addr) :u32))
        (tail (mem-ref (hid-ring-tail-addr) :u32)))
    (let ((next-head (mod (+ head 1) (hid-ring-size))))
      (when (not (eq next-head tail))
        (setf (mem-ref (+ (hid-ring-data-addr) head) :u8) ch)
        (setf (mem-ref (hid-ring-head-addr) :u32) next-head)))))

(defun hid-kbd-ring-pop ()
  ;; Returns character or -1 if empty
  (let ((head (mem-ref (hid-ring-head-addr) :u32))
        (tail (mem-ref (hid-ring-tail-addr) :u32)))
    (if (eq head tail)
        -1
        (let ((ch (mem-ref (+ (hid-ring-data-addr) tail) :u8)))
          (setf (mem-ref (hid-ring-tail-addr) :u32)
                (mod (+ tail 1) (hid-ring-size)))
          ch))))

;; ============================================================
;; HID class requests
;; ============================================================

(defun usb-hid-set-boot-protocol (devaddr iface)
  ;; SET_PROTOCOL: bmRequestType=0x21 (class, interface, OUT)
  ;; bRequest=0x0B, wValue=0 (boot protocol), wIndex=interface
  (usb-control-transfer devaddr #x21 #x0B 0 iface 0 0))

(defun usb-hid-set-idle (devaddr iface)
  ;; SET_IDLE: bmRequestType=0x21 (class, interface, OUT)
  ;; bRequest=0x0A, wValue=0 (indefinite), wIndex=interface
  (usb-control-transfer devaddr #x21 #x0A 0 iface 0 0))

;; ============================================================
;; Config descriptor parsing — find interrupt IN endpoint
;; ============================================================

;; Parse result stored at hid-base+0x700 (scratch area):
;;   +0x700 ep-addr (U32)
;;   +0x704 ep-mps (U32)
;;   +0x708 iface-class (U32)
;;   +0x70C iface-subclass (U32)
;;   +0x710 iface-protocol (U32)
;;   +0x714 iface-number (U32)

(defun hid-parse-scratch () (+ (hid-base) #x700))

(defun hid-parse-config-find-interrupt-ep (buf total-len)
  ;; Walk config descriptor, find first interface with class=3 (HID)
  ;; and its interrupt IN endpoint.
  ;; Returns 1 if found, 0 otherwise. Results in scratch area.
  (let ((found 0)
        (in-hid-iface 0))
    (let ((pos 9))
      (loop
        (when (>= pos total-len) (return nil))
        (let ((desc-len (mem-ref (+ buf pos) :u8))
              (desc-type (mem-ref (+ buf (+ pos 1)) :u8)))
          (when (< desc-len 2) (return nil))
          ;; Interface descriptor (type 4)
          (when (eq desc-type 4)
            (let ((iface-num (mem-ref (+ buf (+ pos 2)) :u8))
                  (iface-class (mem-ref (+ buf (+ pos 5)) :u8))
                  (iface-subclass (mem-ref (+ buf (+ pos 6)) :u8))
                  (iface-protocol (mem-ref (+ buf (+ pos 7)) :u8)))
              (if (eq iface-class 3)
                  (progn
                    (setq in-hid-iface 1)
                    (setf (mem-ref (+ (hid-parse-scratch) #x08) :u32) iface-class)
                    (setf (mem-ref (+ (hid-parse-scratch) #x0C) :u32) iface-subclass)
                    (setf (mem-ref (+ (hid-parse-scratch) #x10) :u32) iface-protocol)
                    (setf (mem-ref (+ (hid-parse-scratch) #x14) :u32) iface-num))
                  (setq in-hid-iface 0))))
          ;; Endpoint descriptor (type 5)
          (when (eq desc-type 5)
            (when (eq in-hid-iface 1)
              (let ((ep-addr (mem-ref (+ buf (+ pos 2)) :u8))
                    (ep-attr (mem-ref (+ buf (+ pos 3)) :u8))
                    (ep-mps (usb-desc-u16 buf (+ pos 4))))
                ;; Interrupt type (bits 1:0 = 3) and IN direction (bit 7)
                (when (eq (logand ep-attr 3) 3)
                  (when (not (zerop (logand ep-addr #x80)))
                    (setf (mem-ref (hid-parse-scratch) :u32) ep-addr)
                    (setf (mem-ref (+ (hid-parse-scratch) #x04) :u32) ep-mps)
                    (setq found 1))))))
          (setq pos (+ pos desc-len)))))
    found))

;; ============================================================
;; Scancode-to-ASCII tables (USB HID Usage Tables, page 53)
;; ============================================================

(defun hid-set-scan (code normal shifted)
  (setf (mem-ref (+ (hid-scancode-normal) code) :u8) normal)
  (setf (mem-ref (+ (hid-scancode-shifted) code) :u8) shifted))

(defun hid-init-scancode-table ()
  ;; Clear tables
  (let ((i 0))
    (loop
      (when (>= i 256) (return nil))
      (setf (mem-ref (+ (hid-scancode-normal) i) :u8) 0)
      (setf (mem-ref (+ (hid-scancode-shifted) i) :u8) 0)
      (setq i (+ i 1))))
  ;; Letters: a-z = scancodes 4-29
  (hid-set-scan 4 97 65)    ; a A
  (hid-set-scan 5 98 66)    ; b B
  (hid-set-scan 6 99 67)    ; c C
  (hid-set-scan 7 100 68)   ; d D
  (hid-set-scan 8 101 69)   ; e E
  (hid-set-scan 9 102 70)   ; f F
  (hid-set-scan 10 103 71)  ; g G
  (hid-set-scan 11 104 72)  ; h H
  (hid-set-scan 12 105 73)  ; i I
  (hid-set-scan 13 106 74)  ; j J
  (hid-set-scan 14 107 75)  ; k K
  (hid-set-scan 15 108 76)  ; l L
  (hid-set-scan 16 109 77)  ; m M
  (hid-set-scan 17 110 78)  ; n N
  (hid-set-scan 18 111 79)  ; o O
  (hid-set-scan 19 112 80)  ; p P
  (hid-set-scan 20 113 81)  ; q Q
  (hid-set-scan 21 114 82)  ; r R
  (hid-set-scan 22 115 83)  ; s S
  (hid-set-scan 23 116 84)  ; t T
  (hid-set-scan 24 117 85)  ; u U
  (hid-set-scan 25 118 86)  ; v V
  (hid-set-scan 26 119 87)  ; w W
  (hid-set-scan 27 120 88)  ; x X
  (hid-set-scan 28 121 89)  ; y Y
  (hid-set-scan 29 122 90)  ; z Z
  ;; Numbers: 1-9 = scancodes 30-38, 0 = scancode 39
  (hid-set-scan 30 49 33)   ; 1 !
  (hid-set-scan 31 50 64)   ; 2 @
  (hid-set-scan 32 51 35)   ; 3 #
  (hid-set-scan 33 52 36)   ; 4 $
  (hid-set-scan 34 53 37)   ; 5 %
  (hid-set-scan 35 54 94)   ; 6 ^
  (hid-set-scan 36 55 38)   ; 7 &
  (hid-set-scan 37 56 42)   ; 8 *
  (hid-set-scan 38 57 40)   ; 9 (
  (hid-set-scan 39 48 41)   ; 0 )
  ;; Special keys
  (hid-set-scan 40 13 13)   ; Enter
  (hid-set-scan 41 27 27)   ; Escape
  (hid-set-scan 42 8 8)     ; Backspace
  (hid-set-scan 43 9 9)     ; Tab
  (hid-set-scan 44 32 32)   ; Space
  ;; Punctuation: scancodes 45-56
  (hid-set-scan 45 45 95)   ; - _
  (hid-set-scan 46 61 43)   ; = +
  (hid-set-scan 47 91 123)  ; [ {
  (hid-set-scan 48 93 125)  ; ] }
  (hid-set-scan 49 92 124)  ; \ |
  ;; scancode 50 is non-US # ~  (skip)
  (hid-set-scan 51 59 58)   ; ; :
  (hid-set-scan 52 39 34)   ; ' "
  (hid-set-scan 53 96 126)  ; ` ~
  (hid-set-scan 54 44 60)   ; , <
  (hid-set-scan 55 46 62)   ; . >
  (hid-set-scan 56 47 63))  ; / ?

;; ============================================================
;; Keyboard report processing (8-byte boot protocol)
;; ============================================================
;; Report format: [modifiers, reserved, key1, key2, key3, key4, key5, key6]
;; Modifiers: bit0=L-Ctrl, bit1=L-Shift, bit2=L-Alt, bit3=L-GUI
;;            bit4=R-Ctrl, bit5=R-Shift, bit6=R-Alt, bit7=R-GUI

(defun hid-key-is-new (key prev-base)
  ;; Check if key was NOT in previous report (delta detection)
  ;; Returns 1 if new, 0 if already pressed
  (let ((i 0)
        (found 0))
    (loop
      (when (>= i 6) (return nil))
      (when (eq key (mem-ref (+ prev-base i) :u8))
        (setq found 1)
        (return nil))
      (setq i (+ i 1)))
    (if (eq found 1) 0 1)))

(defun hid-process-kbd-report ()
  (let ((dma (hid-kbd-dma))
        (prev (hid-prevkeys-addr)))
    ;; Read modifier byte
    (let ((mods (mem-ref dma :u8)))
      (setf (mem-ref (hid-modifier-addr) :u32) mods)
      ;; Check shift state: bits 1 (L-Shift) and 5 (R-Shift)
      (let ((shifted (if (not (zerop (logand mods #x22))) 1 0)))
        ;; Check ctrl state: bits 0 (L-Ctrl) and 4 (R-Ctrl)
        (let ((ctrl (if (not (zerop (logand mods #x11))) 1 0)))
          ;; Process keys 1-6 (bytes 2-7)
          (let ((k 0))
            (loop
              (when (>= k 6) (return nil))
              (let ((scancode (mem-ref (+ dma (+ k 2)) :u8)))
                (when (not (zerop scancode))
                  ;; Only process newly pressed keys
                  (when (eq (hid-key-is-new scancode prev) 1)
                    (let ((ascii 0))
                      (if (eq shifted 1)
                          (setq ascii (mem-ref (+ (hid-scancode-shifted) scancode) :u8))
                          (setq ascii (mem-ref (+ (hid-scancode-normal) scancode) :u8)))
                      (when (not (zerop ascii))
                        ;; Ctrl+key: map to control character (1-26)
                        (when (eq ctrl 1)
                          (when (>= ascii 97)
                            (when (<= ascii 122)
                              (setq ascii (+ (- ascii 97) 1))))
                          (when (>= ascii 65)
                            (when (<= ascii 90)
                              (setq ascii (+ (- ascii 65) 1)))))
                        ;; Enter: emit CR + LF
                        (if (eq ascii 13)
                            (progn
                              (hid-kbd-ring-push 13)
                              (hid-kbd-ring-push 10))
                            (hid-kbd-ring-push ascii)))))))
              (setq k (+ k 1))))
          ;; Save current keys as previous for next delta
          (let ((k 0))
            (loop
              (when (>= k 6) (return nil))
              (setf (mem-ref (+ prev k) :u8) (mem-ref (+ dma (+ k 2)) :u8))
              (setq k (+ k 1)))))))))

;; ============================================================
;; Mouse report processing (3-4 byte boot protocol)
;; ============================================================
;; Report: [buttons, x_delta, y_delta, (wheel_delta)]
;; Deltas are signed 8-bit (>=128 means negative)

(defun hid-sign-extend-8 (val)
  ;; Sign-extend 8-bit value to signed integer
  (if (>= val 128)
      (- val 256)
      val))

(defun hid-process-mouse-report ()
  (let ((dma (hid-mouse-dma)))
    (let ((buttons (mem-ref dma :u8))
          (dx (hid-sign-extend-8 (mem-ref (+ dma 1) :u8)))
          (dy (hid-sign-extend-8 (mem-ref (+ dma 2) :u8))))
      (setf (mem-ref (hid-mouse-buttons-addr) :u32) buttons)
      ;; Accumulate deltas
      (let ((old-x (mem-ref (hid-mouse-x-addr) :u32))
            (old-y (mem-ref (hid-mouse-y-addr) :u32)))
        (setf (mem-ref (hid-mouse-x-addr) :u32) (+ old-x dx))
        (setf (mem-ref (hid-mouse-y-addr) :u32) (+ old-y dy)))
      ;; Wheel (byte 3, may be absent — MPS check)
      (when (>= (hid-mouse-mps) 4)
        (let ((dw (hid-sign-extend-8 (mem-ref (+ dma 3) :u8))))
          (let ((old-w (mem-ref (hid-mouse-wheel-addr) :u32)))
            (setf (mem-ref (hid-mouse-wheel-addr) :u32) (+ old-w dw))))))))

;; ============================================================
;; Tablet report processing (6 byte QEMU usb-tablet)
;; ============================================================
;; Report: [buttons, x_lo, x_hi, y_lo, y_hi, ...]
;; Absolute coordinates, 0-32767

(defun hid-process-tablet-report ()
  (let ((dma (hid-tablet-dma)))
    (let ((buttons (mem-ref dma :u8))
          (x (logior (mem-ref (+ dma 1) :u8)
                     (ash (mem-ref (+ dma 2) :u8) 8)))
          (y (logior (mem-ref (+ dma 3) :u8)
                     (ash (mem-ref (+ dma 4) :u8) 8))))
      (setf (mem-ref (hid-tablet-buttons-addr) :u32) buttons)
      (setf (mem-ref (hid-tablet-x-addr) :u32) x)
      (setf (mem-ref (hid-tablet-y-addr) :u32) y))))

;; ============================================================
;; HID polling — check all active interrupt channels
;; ============================================================

(defun hid-poll ()
  ;; Poll all active HID channels. Call frequently from REPL loop.
  ;; Keyboard
  (when (not (zerop (hid-kbd-present)))
    (let ((result (dwc2-poll-bulk-in (hid-kbd-channel))))
      (when (not (zerop result))
        (when (eq result 1)
          (hid-process-kbd-report))
        ;; Restart interrupt IN transfer
        (dwc2-start-interrupt-in (hid-kbd-channel)
                                 (hid-kbd-devaddr) (hid-kbd-ep)
                                 (hid-kbd-dma) (hid-kbd-mps) (hid-kbd-mps)))))
  ;; Mouse
  (when (not (zerop (hid-mouse-present)))
    (let ((result (dwc2-poll-bulk-in (hid-mouse-channel))))
      (when (not (zerop result))
        (when (eq result 1)
          (hid-process-mouse-report))
        (dwc2-start-interrupt-in (hid-mouse-channel)
                                 (hid-mouse-devaddr) (hid-mouse-ep)
                                 (hid-mouse-dma) (hid-mouse-mps) (hid-mouse-mps)))))
  ;; Tablet
  (when (not (zerop (hid-tablet-present)))
    (let ((result (dwc2-poll-bulk-in (hid-tablet-channel))))
      (when (not (zerop result))
        (when (eq result 1)
          (hid-process-tablet-report))
        (dwc2-start-interrupt-in (hid-tablet-channel)
                                 (hid-tablet-devaddr) (hid-tablet-ep)
                                 (hid-tablet-dma) (hid-tablet-mps) (hid-tablet-mps))))))

;; ============================================================
;; Read character from USB keyboard (non-blocking and blocking)
;; ============================================================

(defun hid-read-char ()
  ;; Non-blocking: poll HID and return char or -1
  (hid-poll)
  (hid-kbd-ring-pop))

(defun hid-read-char-blocking ()
  ;; Blocking: poll HID + serial UART until a character is available.
  ;; PL011 UARTFR (0x3F201018) bit 4 = RXFE (RX FIFO empty).
  ;; When RXFE=0, data is available at UARTDR (0x3F201000).
  (loop
    ;; Check USB keyboard ring buffer first
    (hid-poll)
    (let ((ch (hid-kbd-ring-pop)))
      (when (>= ch 0)
        (return ch)))
    ;; Fallback: read from serial UART (TRAP intrinsic)
    (let ((fr (mem-ref #x3F201018 :u32)))
      (when (zerop (logand fr 16))
        (return (read-char-serial))))))

;; ============================================================
;; REPL state query functions
;; ============================================================

(defun mouse-buttons () (mem-ref (hid-mouse-buttons-addr) :u32))
(defun mouse-x () (mem-ref (hid-mouse-x-addr) :u32))
(defun mouse-y () (mem-ref (hid-mouse-y-addr) :u32))
(defun mouse-wheel () (mem-ref (hid-mouse-wheel-addr) :u32))
(defun tablet-buttons () (mem-ref (hid-tablet-buttons-addr) :u32))
(defun tablet-x () (mem-ref (hid-tablet-x-addr) :u32))
(defun tablet-y () (mem-ref (hid-tablet-y-addr) :u32))

;; ============================================================
;; HID device setup — configure a single HID device
;; ============================================================

(defun hid-setup-device (devaddr dbuf)
  ;; Device at devaddr is already enumerated and configured.
  ;; Read config descriptor, find HID interface + interrupt endpoint.
  ;; Set boot protocol, start interrupt IN channel.
  ;; Returns 1 on success, 0 on failure.
  (loop
    ;; Get config descriptor (first 9 bytes for total length)
    (let ((r1 (usb-get-descriptor devaddr (usb-desc-configuration) 0 dbuf 9)))
      (when (<= r1 0)
        (%serial-byte 72) (%serial-byte 67) (%serial-byte 69) (%serial-byte 10)
        (return 0))
      (let ((total-len (usb-desc-u16 dbuf 2)))
        (when (> total-len 512) (setq total-len 512))
        ;; Get full config descriptor
        (let ((r2 (usb-get-descriptor devaddr (usb-desc-configuration) 0 dbuf total-len)))
          (when (<= r2 0)
            (%serial-byte 72) (%serial-byte 67) (%serial-byte 50) (%serial-byte 10)
            (return 0))
          ;; Find HID interface + interrupt EP
          (let ((found (hid-parse-config-find-interrupt-ep dbuf total-len)))
            (when (zerop found)
              (%serial-byte 78) (%serial-byte 72) (%serial-byte 73) (%serial-byte 68) (%serial-byte 10)
              (return 0))
            ;; Read parsed results
            (let ((ep-addr (mem-ref (hid-parse-scratch) :u32))
                  (ep-mps (mem-ref (+ (hid-parse-scratch) #x04) :u32))
                  (iface-protocol (mem-ref (+ (hid-parse-scratch) #x10) :u32))
                  (iface-num (mem-ref (+ (hid-parse-scratch) #x14) :u32)))
              (let ((ep-num (logand ep-addr #xF)))
                ;; Set boot protocol (best-effort — some devices may STALL)
                (usb-hid-set-boot-protocol devaddr iface-num)
                (usb-hid-set-idle devaddr iface-num)
                ;; Determine device type from interface protocol
                ;; Protocol 1 = keyboard, 2 = mouse, 0 = other (tablet)
                (if (eq iface-protocol 1)
                    ;; Keyboard
                    (progn
                      (hid-set-kbd-devaddr devaddr)
                      (hid-set-kbd-ep ep-num)
                      (hid-set-kbd-mps ep-mps)
                      (hid-set-kbd-channel 3)
                      (hid-set-kbd-present 1)
                      ;; Start interrupt IN on channel 3
                      (dwc2-start-interrupt-in 3 devaddr ep-num
                                               (hid-kbd-dma) ep-mps ep-mps)
                      (%serial-byte 75) (%serial-byte 66) (%serial-byte 68) ; "KBD"
                      (%serial-byte 58) (%serial-byte 79) (%serial-byte 75) ; ":OK"
                      (%serial-byte 10)
                      (return 1))
                    (if (eq iface-protocol 2)
                        ;; Mouse
                        (progn
                          (hid-set-mouse-devaddr devaddr)
                          (hid-set-mouse-ep ep-num)
                          (hid-set-mouse-mps ep-mps)
                          (hid-set-mouse-channel 4)
                          (hid-set-mouse-present 1)
                          ;; Start interrupt IN on channel 4
                          (dwc2-start-interrupt-in 4 devaddr ep-num
                                                   (hid-mouse-dma) ep-mps ep-mps)
                          (%serial-byte 77) (%serial-byte 79) (%serial-byte 85) ; "MOU"
                          (%serial-byte 58) (%serial-byte 79) (%serial-byte 75) ; ":OK"
                          (%serial-byte 10)
                          (return 1))
                        ;; Tablet / other HID
                        (progn
                          (hid-set-tablet-devaddr devaddr)
                          (hid-set-tablet-ep ep-num)
                          (hid-set-tablet-mps ep-mps)
                          (hid-set-tablet-channel 5)
                          (hid-set-tablet-present 1)
                          ;; Start interrupt IN on channel 5
                          (dwc2-start-interrupt-in 5 devaddr ep-num
                                                   (hid-tablet-dma) ep-mps ep-mps)
                          (%serial-byte 84) (%serial-byte 66) (%serial-byte 76) ; "TBL"
                          (%serial-byte 58) (%serial-byte 79) (%serial-byte 75) ; ":OK"
                          (%serial-byte 10)
                          (return 1))))))))))))

;; ============================================================
;; HID enumeration — find and configure HID devices
;; ============================================================

(defun hid-enumerate-device (dbuf)
  ;; Enumerate the USB device at address 0, assign address, configure, setup HID.
  ;; Handles both direct-connect and hub topologies.
  ;; Returns 1 on success, 0 on failure.
  (loop
    ;; 1. Reset port
    (let ((speed (dwc2-port-reset)))
      (when (< speed 0)
        (%serial-byte 72) (%serial-byte 82) (%serial-byte 69) (%serial-byte 10) ; "HRE"
        (return 0))
      (dwc2-delay-ms 20)
      ;; 2. GET_DEVICE_DESCRIPTOR at address 0 (first 8 bytes)
      (let ((r1 (usb-get-descriptor 0 (usb-desc-device) 0 dbuf 8)))
        (when (<= r1 0)
          (%serial-byte 72) (%serial-byte 68) (%serial-byte 49) (%serial-byte 10) ; "HD1"
          (return 0))
        ;; 3. Reset again before SET_ADDRESS
        (dwc2-port-reset)
        (dwc2-delay-ms 20)
        ;; 4. SET_ADDRESS to 1
        (let ((r2 (usb-set-address 1)))
          (when (<= r2 0)
            (%serial-byte 72) (%serial-byte 65) (%serial-byte 49) (%serial-byte 10) ; "HA1"
            (return 0))
          (dwc2-delay-ms 10)
          ;; 5. Full device descriptor at address 1
          (let ((r3 (usb-get-descriptor 1 (usb-desc-device) 0 dbuf 18)))
            (when (<= r3 0)
              (%serial-byte 72) (%serial-byte 68) (%serial-byte 50) (%serial-byte 10) ; "HD2"
              (return 0))
            ;; Print VID:PID
            (%serial-byte 72) (%serial-byte 73) (%serial-byte 68) (%serial-byte 58) ; "HID:"
            (print-hex-byte (mem-ref (+ dbuf 9) :u8))
            (print-hex-byte (mem-ref (+ dbuf 8) :u8))
            (%serial-byte 58)
            (print-hex-byte (mem-ref (+ dbuf 11) :u8))
            (print-hex-byte (mem-ref (+ dbuf 10) :u8))
            (%serial-byte 10)
            ;; Check if this is a hub (class 9)
            (let ((dev-class (mem-ref (+ dbuf 4) :u8)))
              (if (eq dev-class 9)
                  ;; Hub: configure it, then find downstream HID device
                  (progn
                    ;; Get config descriptor for hub
                    (let ((r4 (usb-get-descriptor 1 (usb-desc-configuration) 0 dbuf 9)))
                      (when (<= r4 0) (return 0))
                      (let ((config-val (mem-ref (+ dbuf 5) :u8)))
                        (usb-set-configuration 1 config-val))
                      ;; Enumerate downstream
                      (return (hid-hub-find-device 1 dbuf))))
                  ;; Direct device: configure and setup HID
                  (progn
                    ;; SET_CONFIGURATION
                    (let ((r4 (usb-get-descriptor 1 (usb-desc-configuration) 0 dbuf 9)))
                      (when (<= r4 0) (return 0))
                      (let ((config-val (mem-ref (+ dbuf 5) :u8)))
                        (let ((r5 (usb-set-configuration 1 config-val)))
                          (when (<= r5 0) (return 0))
                          (return (hid-setup-device 1 dbuf))))))))))))))

(defun hid-hub-find-device (hub-addr dbuf)
  ;; Hub at hub-addr. Find first connected downstream HID device.
  ;; Returns 1 if found, 0 otherwise.
  (loop
    ;; 1. Get hub descriptor
    (let ((r1 (usb-control-transfer hub-addr #xA0 6 #x2900 0 dbuf 8)))
      (when (<= r1 0)
        (%serial-byte 72) (%serial-byte 72) (%serial-byte 68) (%serial-byte 10) ; "HHD"
        (return 0))
      (let ((num-ports (mem-ref (+ dbuf 2) :u8)))
        ;; 2. Power all ports
        (let ((p 1))
          (loop
            (when (> p num-ports) (return nil))
            (usb-control-transfer hub-addr #x23 3 8 p 0 0)
            (setq p (+ p 1))))
        (dwc2-delay-ms 150)
        ;; 3. Find first connected port
        (let ((conn-port 0))
          (let ((p 1))
            (loop
              (when (> p num-ports) (return nil))
              (let ((r2 (usb-control-transfer hub-addr #xA3 0 0 p dbuf 4)))
                (when (> r2 0)
                  (let ((status (usb-desc-u16 dbuf 0)))
                    (when (not (zerop (logand status 1)))
                      (setq conn-port p)
                      (return nil)))))
              (setq p (+ p 1))))
          (when (zerop conn-port)
            (%serial-byte 72) (%serial-byte 78) (%serial-byte 67) (%serial-byte 10) ; "HNC"
            (return 0))
          ;; 4. Reset port
          (usb-control-transfer hub-addr #x23 3 4 conn-port 0 0)
          (dwc2-delay-ms 60)
          (usb-control-transfer hub-addr #x23 1 20 conn-port 0 0)
          (dwc2-delay-ms 10)
          ;; 5. Enumerate downstream device
          (let ((r3 (usb-get-descriptor 0 (usb-desc-device) 0 dbuf 8)))
            (when (<= r3 0)
              (%serial-byte 72) (%serial-byte 68) (%serial-byte 51) (%serial-byte 10) ; "HD3"
              (return 0))
            ;; SET_ADDRESS to 2
            (let ((r4 (usb-set-address 2)))
              (when (<= r4 0) (return 0))
              (dwc2-delay-ms 10)
              ;; Full device descriptor
              (let ((r5 (usb-get-descriptor 2 (usb-desc-device) 0 dbuf 18)))
                (when (<= r5 0) (return 0))
                ;; Print VID:PID
                (%serial-byte 68) (%serial-byte 69) (%serial-byte 86) (%serial-byte 58) ; "DEV:"
                (print-hex-byte (mem-ref (+ dbuf 9) :u8))
                (print-hex-byte (mem-ref (+ dbuf 8) :u8))
                (%serial-byte 58)
                (print-hex-byte (mem-ref (+ dbuf 11) :u8))
                (print-hex-byte (mem-ref (+ dbuf 10) :u8))
                (%serial-byte 10)
                ;; Configure device
                (let ((r6 (usb-get-descriptor 2 (usb-desc-configuration) 0 dbuf 9)))
                  (when (<= r6 0) (return 0))
                  (let ((config-val (mem-ref (+ dbuf 5) :u8)))
                    (let ((r7 (usb-set-configuration 2 config-val)))
                      (when (<= r7 0) (return 0))
                      ;; Setup HID
                      (return (hid-setup-device 2 dbuf)))))))))))))

;; ============================================================
;; HID initialization
;; ============================================================

(defun hid-clear-state ()
  ;; Zero out entire HID memory region
  (let ((i 0))
    (loop
      (when (>= i #x700) (return nil))
      (setf (mem-ref (+ (hid-base) i) :u32) 0)
      (setq i (+ i 4)))))

(defun hid-init ()
  ;; Top-level HID initialization:
  ;; 1. Clear HID state
  ;; 2. Init DWC2 host controller
  ;; 3. Init scancode tables
  ;; 4. Enumerate and configure USB HID devices
  (hid-clear-state)
  (%serial-byte 72) (%serial-byte 73) (%serial-byte 68) (%serial-byte 10) ; "HID\n"
  ;; Init DWC2
  (let ((ok (dwc2-init)))
    (when (zerop ok)
      (%serial-byte 78) (%serial-byte 79) (%serial-byte 68) (%serial-byte 10) ; "NOD\n"
      (return nil))
    ;; Init scancode tables
    (hid-init-scancode-table)
    ;; Enumerate
    (let ((dbuf (usb-data-buf)))
      (hid-enumerate-device dbuf))))

;; ============================================================
;; read-char-input: the indirection point for REPL input
;; This definition is loaded AFTER repl-source.lisp so it wins.
;; ============================================================

(defun read-char-input ()
  (hid-read-char-blocking))
