;;;; usb-hid-split.lisp - USB keyboard and mouse behind the board's hub
;;;;
;;;; On the Pi Zero 2 W the single DWC2 port carries a high-speed hub, and the
;;;; keyboard (full speed) and mouse (low speed) sit behind it, so every
;;;; transfer to them is a SPLIT transaction (net/dwc2-split.lisp).  U-Boot has
;;;; already enumerated the bus when Modus starts and its addresses shuffle per
;;;; boot, so this does not assume any: it finds the hub, walks its ports, and
;;;; for each full/low-speed device either locates the address U-Boot gave it
;;;; or -- when the hub has left the port disabled, which it does for the mouse
;;;; -- resets the port and addresses the device itself.  Boot-protocol
;;;; keyboard and mouse interfaces are then configured and polled.
;;;;
;;;; Polling must run as native code: a complete-split has to reach the hub
;;;; within 2-4 microframes of its start-split, and an interpreted poll misses
;;;; that window and is answered NYET forever.  Everything here is compiled
;;;; into the image, so that holds; a poll pushed at the REPL needs (jit-eager).
;;;;
;;;; The keyboard becomes a REPL input source: the board build binds
;;;; lib/serial-repl.lisp's %CONSOLE-INIT / %CONSOLE-READ-CHAR seams to
;;;; HID-CONSOLE-INIT / HID-CONSOLE-READ-CHAR, AFTER serial-repl's defaults
;;;; (this file is spliced before it, so a defun here would lose).
;;;;
;;;; Depends on: dwc2.lisp, usb.lisp, dwc2-split.lisp, lib/serial-repl.lisp.

;; CPU-side state, u32 fields, in the Device USB window after the split
;; module's buffers (aligned u32 accesses only -- it is Device memory).
(defun hid-st () (+ (split-dma-base) #x1000))
(defun hid-get (off) (mem-ref (+ (hid-st) off) :u32))
(defun hid-put (off v) (setf (mem-ref (+ (hid-st) off) :u32) v))

;; Device records: keyboard at +0x20, mouse at +0x40.
;;   +0 present  +4 addr  +8 hub port  +C low-speed  +10 ep  +14 mps  +18 pid
(defun hid-kbd () #x20)
(defun hid-mouse () #x40)
(defun hid-dev-get (dev f) (hid-get (+ dev f)))
(defun hid-dev-put (dev f v) (hid-put (+ dev f) v))

;; +0x00 hub address, +0x04 hub port count, +0x60 buttons, +0x64 x, +0x68 y,
;; +0x6C wheel (x/y/wheel two's complement u32), +0x70 previous keys (8 bytes),
;; +0x80 ring head, +0x84 ring tail, +0x88 held usage, +0x8C next repeat
;; time, +0x100 ring (256 bytes).
(defun hid-kbd-buf () (+ (split-dma-base) #x400))
(defun hid-mouse-buf () (+ (split-dma-base) #x480))

;; Written last by HID-SPLIT-INIT.  The state lives in memory boot does not
;; clear, so nothing in it is believed until this word says init ran.
(defun hid-magic () #x48494453)
(defun hid-ready-p () (= (hid-get #x1F0) (hid-magic)))

(defun hid-s32 (u) (if (> u #x7FFFFFFF) (- u #x100000000) u))
(defun hid-u32 (s) (logand s #xFFFFFFFF))
(defun hid-s8 (b) (if (> b 127) (- b 256) b))

(defun hid-ring-push (code)
  (let ((h (hid-get #x80)) (tl (hid-get #x84)))
    (let ((nh (logand (+ h 1) 255)))
      (when (not (= nh tl))
        (setf (mem-ref (+ (hid-st) #x100 h) :u8) code)
        (hid-put #x80 nh)))))

(defun hid-ring-pop ()
  ;; Next typed character code, or -1.
  (let ((h (hid-get #x80)) (tl (hid-get #x84)))
    (if (= h tl)
        -1
        (let ((c (mem-ref (+ (hid-st) #x100 tl) :u8)))
          (hid-put #x84 (logand (+ tl 1) 255))
          c))))

;;; ---------------------------------------------------------------------------
;;; Discovery
;;; ---------------------------------------------------------------------------

(defun hid-find-hub ()
  ;; The first device on the root port whose device class is 9 (hub).
  (let ((a 2) (found 0) (b (usb-data-buf)))
    (loop
      (when (or (> found 0) (> a 31)) (return found))
      (setf (mem-ref (+ b 4) :u8) 0)
      (when (> (usb-get-descriptor a 1 0 b 18) 0)
        (when (= (usb-desc-byte b 4) 9) (setq found a)))
      (setq a (+ a 1)))))

(defun hid-port-status (hub port)
  ;; wPortStatus | wPortChange << 16, or 0 on failure.
  (if (> (usb-control-transfer hub #xA3 0 0 port (usb-data-buf) 4) 0)
      (logior (usb-desc-u16 (usb-data-buf) 0)
              (ash (usb-desc-u16 (usb-data-buf) 2) 16))
      0))

(defun hid-clear-port-changes (hub port)
  ;; C_PORT_CONNECTION (16), C_PORT_ENABLE (17), C_PORT_RESET (20).
  (usb-control-transfer hub #x23 1 16 port 0 0)
  (usb-control-transfer hub #x23 1 17 port 0 0)
  (usb-control-transfer hub #x23 1 20 port 0 0))

(defun hid-probe-addr (addr hub port ls)
  ;; T when a device answers GET_DESCRIPTOR(device, 8) at ADDR through PORT.
  (> (usb-split-control-in addr hub port ls 8 #x80 6 #x100 0 8) 7))

(defun hid-locate (hub port ls)
  ;; The address of the device behind PORT: the one U-Boot assigned when the
  ;; port is enabled, else reset the port and give it 32+PORT.  0 = none.
  (let ((st (hid-port-status hub port)))
    (if (not (zerop (logand st 2)))
        (let ((a 2) (found 0))
          (loop
            (when (or (> found 0) (> a 31)) (return found))
            (when (hid-probe-addr a hub port ls) (setq found a))
            (setq a (+ a 1))))
        (progn
          (usb-control-transfer hub #x23 3 4 port 0 0)
          (dwc2-delay-ms 100)
          (hid-clear-port-changes hub port)
          (dwc2-delay-ms 20)
          (if (not (hid-probe-addr 0 hub port ls))
              0
              (let ((new (+ 32 port)))
                (usb-split-control-in 0 hub port ls 8 #x00 5 new 0 0)
                (dwc2-delay-ms 20)
                (if (hid-probe-addr new hub port ls) new 0)))))))

(defun hid-setup-port (hub port ls)
  ;; Find the boot keyboard / mouse interface of the device behind PORT,
  ;; configure it and record it.  Returns 1 = keyboard, 2 = mouse, 0 = neither.
  (let ((addr (hid-locate hub port ls)) (b (split-data-buf)) (kind 0))
    (when (> addr 0)
      (let ((mps0 (if (> (usb-split-control-in addr hub port ls 8 #x80 6 #x100 0 8) 7)
                      (usb-desc-byte b 7)
                      8)))
        (when (> (usb-split-control-in addr hub port ls mps0 #x80 6 #x200 0 9) 8)
          (let ((total (min (usb-desc-u16 b 2) 512)) (cfg (usb-desc-byte b 5)))
            (when (>= (usb-split-control-in addr hub port ls mps0 #x80 6 #x200 0 total) total)
              (let ((i 0) (iface -1) (proto 0) (ep 0) (mps 0))
                ;; Walk descriptors: the first boot interface (class 3,
                ;; subclass 1) and its interrupt-IN endpoint.
                (loop
                  (when (or (>= i total) (> ep 0)) (return nil))
                  (let ((len (usb-desc-byte b i)) (typ (usb-desc-byte b (+ i 1))))
                    (when (zerop len) (return nil))
                    (cond
                      ((= typ 4)
                       (if (and (= (usb-desc-byte b (+ i 5)) 3)
                                (= (usb-desc-byte b (+ i 6)) 1)
                                (or (= (usb-desc-byte b (+ i 7)) 1)
                                    (= (usb-desc-byte b (+ i 7)) 2)))
                           (progn (setq iface (usb-desc-byte b (+ i 2)))
                                  (setq proto (usb-desc-byte b (+ i 7))))
                           (setq iface -1)))
                      ((= typ 5)
                       (when (and (>= iface 0)
                                  (not (zerop (logand (usb-desc-byte b (+ i 2)) #x80)))
                                  (= (logand (usb-desc-byte b (+ i 3)) 3) 3))
                         (setq ep (logand (usb-desc-byte b (+ i 2)) #xF))
                         (setq mps (logand (usb-desc-u16 b (+ i 4)) #x7FF)))))
                    (setq i (+ i len))))
                (when (> ep 0)
                  (usb-split-control-in addr hub port ls mps0 #x00 9 cfg 0 0)
                  (usb-split-control-in addr hub port ls mps0 #x21 #x0B 0 iface 0)
                  (usb-split-control-in addr hub port ls mps0 #x21 #x0A 0 iface 0)
                  (let ((dev (if (= proto 1) (hid-kbd) (hid-mouse))))
                    (hid-dev-put dev #x04 addr)
                    (hid-dev-put dev #x08 port)
                    (hid-dev-put dev #x0C (if ls 1 0))
                    (hid-dev-put dev #x10 ep)
                    (hid-dev-put dev #x14 (min mps 64))
                    (hid-dev-put dev #x18 (hctsiz-pid-data0))
                    (hid-dev-put dev #x00 1)
                    (setq kind proto)))))))))
    kind))

(defun hid-split-init ()
  ;; Find the hub and set up every boot keyboard / mouse behind it.
  ;; Returns (KEYBOARD-P MOUSE-P).
  ;; Zero everything, the ready word included: this RAM survives a board
  ;; reset, so a previous boot's state would otherwise look current.
  (let ((i 0))
    (loop (when (>= i #x200) (return nil)) (hid-put i 0) (setq i (+ i 4))))
  (let ((hub (hid-find-hub)))
    (hid-put #x00 hub)
    (when (> hub 0)
      (when (> (usb-control-transfer hub #xA0 6 #x2900 0 (usb-data-buf) 8) 0)
        (let ((n (usb-desc-byte (usb-data-buf) 2)) (p 1))
          (hid-put #x04 n)
          (loop
            (when (> p n) (return nil))
            (let ((st (hid-port-status hub p)))
              ;; connected, and not high speed (bit 10): a split device.
              (when (and (not (zerop (logand st 1))) (zerop (logand st #x400)))
                (hid-setup-port hub p (not (zerop (logand st #x200))))))
            (setq p (+ p 1)))))))
  (hid-put #x1F0 (hid-magic))
  (list (= (hid-dev-get (hid-kbd) 0) 1) (= (hid-dev-get (hid-mouse) 0) 1)))

(defun hid-console-init ()
  ;; At boot, before the REPL banner.  Reports what it found on serial.
  ;; (WRITE-STRING-SERIAL takes a literal: one call per outcome.)
  (let ((r (hid-split-init)))
    (write-string-serial "USB-HID: keyboard ")
    (if (car r) (write-string-serial "yes") (write-string-serial "no"))
    (write-string-serial ", mouse ")
    (if (cadr r) (write-string-serial "yes") (write-string-serial "no"))
    (write-char-serial 10)))

;;; ---------------------------------------------------------------------------
;;; Polling
;;; ---------------------------------------------------------------------------

(defun hid-poll-dev (dev buf)
  ;; One interrupt-IN poll.  Bytes received (> 0), 0 for none, < 0 on error.
  (if (= (hid-dev-get dev 0) 1)
      (multiple-value-bind (n np)
          (usb-split-interrupt-in (hid-dev-get dev #x04) (hid-get #x00)
                                  (hid-dev-get dev #x08)
                                  (= (hid-dev-get dev #x0C) 1)
                                  (hid-dev-get dev #x10) (hid-dev-get dev #x14)
                                  (hid-dev-get dev #x18) buf)
        (hid-dev-put dev #x18 np)
        n)
      0))

;; HID usage -> character code.  Usages 4..56; anything else is ignored.
(defun hid-usage-char (u shift)
  (cond
    ((and (>= u 4) (<= u 29))
     (+ u (if shift 61 93)))                         ; a-z / A-Z
    ((and (>= u 30) (<= u 39))
     (char-code (char (if shift "!@#$%^&*()" "1234567890") (- u 30))))
    ((= u 40) 13)
    ((= u 41) 27)
    ((= u 42) 127)
    ((= u 43) 9)
    ((= u 44) 32)
    ((and (>= u 45) (<= u 56))
     (char-code (char (if shift "_+{}|~:\"~<>?" "-=[]\\#;'`,./") (- u 45))))
    (t -1)))

;; Key repeat.  The keyboard reports only CHANGES (SET_IDLE 0), so a held key
;; is one report and then silence: repeat it here, from the poll loop.  The
;; clock is the DWC2's own (micro)frame counter, HFNUM: 14 bits of 125 us,
;; wrapping every 2.048 s, compared modulo 2^14.  Not the BCM system timer:
;; on this board a u32 read of CLO (0x3F003004) returns one live byte under
;; three constant ones ("tim\x??"), so it cannot time anything.  The newest
;; key pressed is the one that repeats; a report without it stops the repeat.
(defun hid-now () (logand (dwc2-read (dwc2-hfnum)) #x3FFF))
(defun hid-repeat-delay () 4000)              ; 125-us ticks: 500 ms
(defun hid-repeat-period () 264)              ; ~33 ms, ~30 repeats/s
(defun hid-time-reached-p (t1)
  (< (logand (- (hid-now) t1) #x3FFF) #x2000))

(defun hid-key-char (u mods)
  ;; Character for usage U under modifier byte MODS, or -1.
  (let ((c (hid-usage-char u (not (zerop (logand mods #x22))))))
    (if (and (>= c 0) (not (zerop (logand mods #x11))) (>= c 64))
        (logand c 31)
        c)))

(defun hid-repeat-tick ()
  (let ((u (hid-get #x88)))
    (when (and (> u 0) (hid-time-reached-p (hid-get #x8C)))
      (let ((c (hid-key-char u (mem-ref (+ (hid-st) #x70) :u8))))
        (when (>= c 0) (hid-ring-push c)))
      (hid-put #x8C (logand (+ (hid-now) (hid-repeat-period)) #x3FFF)))))

(defun hid-key-was-down (u)
  (let ((i 2) (r nil))
    (loop
      (when (or r (> i 7)) (return r))
      (when (= (mem-ref (+ (hid-st) #x70 i) :u8) u) (setq r t))
      (setq i (+ i 1)))))

(defun hid-kbd-report (b)
  ;; Queue the characters of keys newly down in boot report B.
  (let ((mods (mem-ref b :u8)) (i 2) (held (hid-get #x88)) (still nil))
    (progn
      (loop
        (when (> i 7) (return nil))
        (let ((u (mem-ref (+ b i) :u8)))
          (when (= u held) (setq still t))
          (when (and (> u 3) (not (hid-key-was-down u)))
            (let ((c (hid-key-char u mods)))
              (when (>= c 0)
                (hid-ring-push c)
                ;; the newest key repeats, after the initial delay
                (setq held u) (setq still t)
                (hid-put #x88 u)
                (hid-put #x8C (logand (+ (hid-now) (hid-repeat-delay)) #x3FFF))))))
        (setq i (+ i 1)))
      (when (not still) (hid-put #x88 0))
      (let ((j 0))
        (loop
          (when (> j 7) (return nil))
          (setf (mem-ref (+ (hid-st) #x70 j) :u8) (mem-ref (+ b j) :u8))
          (setq j (+ j 1)))))))

(defun hid-mouse-report (b n)
  (hid-put #x60 (mem-ref b :u8))
  (hid-put #x64 (hid-u32 (+ (hid-s32 (hid-get #x64)) (hid-s8 (mem-ref (+ b 1) :u8)))))
  (hid-put #x68 (hid-u32 (+ (hid-s32 (hid-get #x68)) (hid-s8 (mem-ref (+ b 2) :u8)))))
  (when (> n 3)
    (hid-put #x6C (hid-u32 (+ (hid-s32 (hid-get #x6C)) (hid-s8 (mem-ref (+ b 3) :u8)))))))

(defun hid-split-poll ()
  ;; Poll the keyboard and the mouse once each.
  (when (not (hid-ready-p)) (return-from hid-split-poll nil))
  (when (> (hid-poll-dev (hid-kbd) (hid-kbd-buf)) 0)
    (hid-kbd-report (hid-kbd-buf)))
  (hid-repeat-tick)
  (let ((n (hid-poll-dev (hid-mouse) (hid-mouse-buf))))
    (when (> n 2) (hid-mouse-report (hid-mouse-buf) n)))
  nil)

(defun mouse-buttons () (hid-get #x60))
(defun mouse-x () (hid-s32 (hid-get #x64)))
(defun mouse-y () (hid-s32 (hid-get #x68)))
(defun mouse-wheel () (hid-s32 (hid-get #x6C)))

;;; ---------------------------------------------------------------------------
;;; Console input: serial OR keyboard
;;; ---------------------------------------------------------------------------

;; PL011 flag register: RXFE (bit 4) set while the receive FIFO is empty.
;; A mini-UART console build replaces this (build-cl-repl-common binds it with
;; the console seams): checking the wrong UART never sees serial input.
(defun hid-serial-ready-p ()
  (zerop (logand (mem-ref #x3F201018 :u32) #x10)))

(defun hid-console-read-char ()
  ;; Serial first, so a serial-driven session is never held up by the
  ;; keyboard; then the keyboard.  Without a keyboard or mouse this is
  ;; exactly READ-CHAR-SERIAL.
  (if (or (not (hid-ready-p))
          (and (zerop (hid-dev-get (hid-kbd) 0)) (zerop (hid-dev-get (hid-mouse) 0))))
      (read-char-serial)
      (let ((c -1))
        (loop
          (when (>= c 0) (return c))
          (if (hid-serial-ready-p)
              (setq c (read-char-serial))
              (progn
                (hid-split-poll)
                (setq c (hid-ring-pop))))))))
