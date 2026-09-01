;;;; dwc2-device.lisp - DWC2 USB Device Mode + CDC-ECM Ethernet Gadget
;;;;
;;;; Provides USB Ethernet gadget for bare-metal networking on Pi Zero 2 W.
;;;; The host sees a standard CDC-ECM USB Ethernet adapter.
;;;;
;;;; Replaces: dwc2.lisp + usb.lisp + cdc-ether.lisp
;;;; Provides: e1000-send, e1000-receive, e1000-rx-buf, cdc-ether-init
;;;; Depends on: arch adapter (dwc2-base, e1000-state-base, etc.)

;;; ============================================================
;;; Register offsets
;;; ============================================================

;; Core global registers
(defun dwc2-gahbcfg ()   (+ (dwc2-base) #x008))
(defun dwc2-gusbcfg ()   (+ (dwc2-base) #x00C))
(defun dwc2-grstctl ()   (+ (dwc2-base) #x010))
(defun dwc2-gintsts ()   (+ (dwc2-base) #x014))
(defun dwc2-gintmsk ()   (+ (dwc2-base) #x018))
(defun dwc2-grxstsp ()   (+ (dwc2-base) #x020))
(defun dwc2-grxfsiz ()   (+ (dwc2-base) #x024))
(defun dwc2-gnptxfsiz () (+ (dwc2-base) #x028))

;; Device-mode registers
(defun dwc2-dcfg ()      (+ (dwc2-base) #x800))
(defun dwc2-dctl ()      (+ (dwc2-base) #x804))
(defun dwc2-dsts ()      (+ (dwc2-base) #x808))
(defun dwc2-diepmsk ()   (+ (dwc2-base) #x810))
(defun dwc2-doepmsk ()   (+ (dwc2-base) #x814))
(defun dwc2-daintmsk ()  (+ (dwc2-base) #x81C))

;; IN endpoint registers
(defun dwc2-diepctl (n)  (+ (dwc2-base) #x900 (* n #x20)))
(defun dwc2-diepint (n)  (+ (dwc2-base) #x908 (* n #x20)))
(defun dwc2-dieptsiz (n) (+ (dwc2-base) #x910 (* n #x20)))

;; OUT endpoint registers
(defun dwc2-doepctl (n)  (+ (dwc2-base) #xB00 (* n #x20)))
(defun dwc2-doepint (n)  (+ (dwc2-base) #xB08 (* n #x20)))
(defun dwc2-doeptsiz (n) (+ (dwc2-base) #xB10 (* n #x20)))

;; Data FIFO access
(defun dwc2-dfifo (n)    (+ (dwc2-base) #x1000 (* n #x1000)))

;; Power and Clock Gating Control
(defun dwc2-pcgcctl ()   (+ (dwc2-base) #xE00))

;;; ============================================================
;;; Register read/write
;;; ============================================================

(defun dwc2-read (addr)
  (mem-ref addr :u32))

(defun dwc2-write (addr val)
  (setf (mem-ref addr :u32) val)
  ;; DSB SY: On AArch64 with MMU off, all memory is Normal Non-cacheable.
  ;; Writes to different 4KB pages can be reordered by the write buffer.
  ;; DWC2 registers span multiple pages (control at +0x900, FIFO at +0x1000).
  ;; Without DSB, FIFO data may arrive before EPENA, causing silent TX failure.
  (memory-barrier))

;;; ============================================================
;;; Gadget state at 0x01050000
;;; ============================================================
;;; +0x00: configured    +0x04: address-pending
;;; +0x08: rx-ready      +0x0C: rx-len
;;; +0x10: rx-offset     +0x14: ep0-setup-pending

(defun gadget-state () #x01050000)

(defun gadget-configured ()     (mem-ref (gadget-state) :u32))
(defun gadget-set-configured (v)(setf (mem-ref (gadget-state) :u32) v))
(defun gadget-addr-pending ()     (mem-ref (+ (gadget-state) 4) :u32))
(defun gadget-set-addr-pending (v)(setf (mem-ref (+ (gadget-state) 4) :u32) v))
(defun gadget-rx-ready ()       (mem-ref (+ (gadget-state) 8) :u32))
(defun gadget-set-rx-ready (v)  (setf (mem-ref (+ (gadget-state) 8) :u32) v))
(defun gadget-rx-len ()         (mem-ref (+ (gadget-state) #xC) :u32))
(defun gadget-set-rx-len (v)    (setf (mem-ref (+ (gadget-state) #xC) :u32) v))
(defun gadget-rx-offset ()      (mem-ref (+ (gadget-state) #x10) :u32))
(defun gadget-set-rx-offset (v) (setf (mem-ref (+ (gadget-state) #x10) :u32) v))

;; Descriptor and EP0 buffers
(defun desc-base () #x01000100)
(defun setup-pkt () #x01000080)

;;; ============================================================
;;; Zero-latency event trace buffer at 0x01060000
;;; ============================================================
;;; Writes single-byte event codes to memory — no serial overhead.
;;; Dump with gadget-trace-dump after enumeration timeout.

;; Trace buffer at 0x01080000 — well above e1000-state-base (0x01060000)
;; e1000-state-base uses offsets up to at least +0x74C (SSH host keys)
(defun trace-buf () #x01080000)
(defun trace-pos () (mem-ref #x01080FFC :u32))
(defun trace-set-pos (v) (setf (mem-ref #x01080FFC :u32) v))
(defun trace-reset () (trace-set-pos 0))

(defun trace-event (code)
  (let ((pos (trace-pos)))
    (when (< pos 4000)
      (setf (mem-ref (+ (trace-buf) pos) :u8) code)
      (trace-set-pos (+ pos 1)))))

(defun trace-event-val (code val)
  ;; Write code byte + 4-byte value (LE)
  (let ((pos (trace-pos)))
    (when (< pos 3990)
      (setf (mem-ref (+ (trace-buf) pos) :u8) code)
      (setf (mem-ref (+ (trace-buf) (+ pos 1)) :u8) (logand val #xFF))
      (setf (mem-ref (+ (trace-buf) (+ pos 2)) :u8) (logand (ash val -8) #xFF))
      (setf (mem-ref (+ (trace-buf) (+ pos 3)) :u8) (logand (ash val -16) #xFF))
      (setf (mem-ref (+ (trace-buf) (+ pos 4)) :u8) (logand (ash val -24) #xFF))
      (trace-set-pos (+ pos 5)))))

(defun gadget-trace-dump ()
  ;; Dump trace buffer over serial
  (%serial-byte 84)(%serial-byte 82)(%serial-byte 58)  ;; "TR:"
  (let ((count (trace-pos)))
    (print-dec count)
    (%serial-byte 10)
    (let ((i 0))
      (loop
        (when (>= i count) (return 0))
        (print-hex-byte (mem-ref (+ (trace-buf) i) :u8))
        (when (zerop (logand (+ i 1) 31))
          (%serial-byte 10))  ;; newline every 32 bytes
        (setq i (+ i 1))))
    (%serial-byte 10)))

;;; ============================================================
;;; Delay helpers
;;; ============================================================

(defun gadget-delay (n)
  (let ((i 0))
    (loop
      (when (>= i n) (return 0))
      (io-delay)
      (setq i (+ i 1)))))

(defun gadget-trace (ch)
  ;; Print a single trace character using raw serial TRAP
  ;; Bypasses the defun %serial-byte (capture-aware) to rule out flag issues
  (write-char-serial ch))

;;; ============================================================
;;; FIFO read/write helpers
;;; ============================================================

;; Read bcnt bytes from RX FIFO into buffer (byte-by-byte extraction)
(defun fifo-read-to-buf (buf bcnt)
  (let ((words (ash (+ bcnt 3) -2)))
    (let ((i 0))
      (loop
        (when (>= i words) (return 0))
        (let ((w (dwc2-read (dwc2-dfifo 0))))
          (let ((off (* i 4)))
            (setf (mem-ref (+ buf off) :u8) (logand w #xFF))
            (when (< (+ off 1) bcnt)
              (setf (mem-ref (+ buf (+ off 1)) :u8) (logand (ash w -8) #xFF)))
            (when (< (+ off 2) bcnt)
              (setf (mem-ref (+ buf (+ off 2)) :u8) (logand (ash w -16) #xFF)))
            (when (< (+ off 3) bcnt)
              (setf (mem-ref (+ buf (+ off 3)) :u8) (logand (ash w -24) #xFF)))))
        (setq i (+ i 1))))))

;; Discard bcnt bytes from RX FIFO
(defun fifo-discard (bcnt)
  (let ((words (ash (+ bcnt 3) -2)))
    (let ((i 0))
      (loop
        (when (>= i words) (return 0))
        (dwc2-read (dwc2-dfifo 0))
        (setq i (+ i 1))))))

;; Write len bytes from buffer to TX FIFO n
(defun fifo-write-from-buf (n buf len)
  (let ((words (ash (+ len 3) -2)))
    (let ((i 0))
      (loop
        (when (>= i words) (return 0))
        (let ((off (* i 4)))
          (let ((b0 (mem-ref (+ buf off) :u8)))
            (let ((b1 (if (< (+ off 1) len) (mem-ref (+ buf (+ off 1)) :u8) 0)))
              (let ((b2 (if (< (+ off 2) len) (mem-ref (+ buf (+ off 2)) :u8) 0)))
                (let ((b3 (if (< (+ off 3) len) (mem-ref (+ buf (+ off 3)) :u8) 0)))
                  (let ((lo (logior b0 (ash b1 8))))
                    (let ((hi (logior b2 (ash b3 8))))
                      (dwc2-write (dwc2-dfifo n) (logior lo (ash hi 16))))))))))
        (setq i (+ i 1))))))

;; Write len bytes from descriptor memory to EP0 TX FIFO
(defun fifo-write-from-desc (src len)
  (fifo-write-from-buf 0 src len))

;;; ============================================================
;;; Descriptor initialization (CDC-ECM)
;;; ============================================================
;;;
;;; Layout:  desc-base+0x000: Device descriptor (18 bytes)
;;;          desc-base+0x020: Config descriptor (71 bytes)
;;;          desc-base+0x080: String 0 - Language (4 bytes)
;;;          desc-base+0x090: String 1 - "Modus" (12 bytes)
;;;          desc-base+0x0A0: String 2 - "USB Ether" (20 bytes)
;;;          desc-base+0x0C0: String 3 - "000001" (14 bytes)
;;;          desc-base+0x0E0: String 4 - MAC "020000000001" (26 bytes)

(defun gadget-init-dev-desc ()
  (let ((d (desc-base)))
    (setf (mem-ref d :u8) 18)            ;; bLength
    (setf (mem-ref (+ d 1) :u8) 1)      ;; bDescriptorType = DEVICE
    (setf (mem-ref (+ d 2) :u8) 0)      ;; bcdUSB lo
    (setf (mem-ref (+ d 3) :u8) 2)      ;; bcdUSB hi = 2.00
    (setf (mem-ref (+ d 4) :u8) 2)      ;; bDeviceClass = CDC
    (setf (mem-ref (+ d 5) :u8) 0)      ;; bDeviceSubClass
    (setf (mem-ref (+ d 6) :u8) 0)      ;; bDeviceProtocol
    (setf (mem-ref (+ d 7) :u8) 64)     ;; bMaxPacketSize0
    (setf (mem-ref (+ d 8) :u8) #x6B)   ;; idVendor lo = 0x1D6B
    (setf (mem-ref (+ d 9) :u8) #x1D)   ;; idVendor hi
    (setf (mem-ref (+ d 10) :u8) #x37)  ;; idProduct lo
    (setf (mem-ref (+ d 11) :u8) 1)     ;; idProduct hi = 0x0137
    (setf (mem-ref (+ d 12) :u8) 0)     ;; bcdDevice lo
    (setf (mem-ref (+ d 13) :u8) 1)     ;; bcdDevice hi
    (setf (mem-ref (+ d 14) :u8) 1)     ;; iManufacturer
    (setf (mem-ref (+ d 15) :u8) 2)     ;; iProduct
    (setf (mem-ref (+ d 16) :u8) 3)     ;; iSerialNumber
    (setf (mem-ref (+ d 17) :u8) 1)))   ;; bNumConfigurations

(defun gadget-init-config-desc ()
  (let ((c (+ (desc-base) #x20)))
    ;; Configuration descriptor (9)
    (setf (mem-ref c :u8) 9)
    (setf (mem-ref (+ c 1) :u8) 2)       ;; CONFIGURATION
    (setf (mem-ref (+ c 2) :u8) 80)      ;; wTotalLength lo (9+9+5+5+13+7+9+9+7+7=80)
    (setf (mem-ref (+ c 3) :u8) 0)       ;; wTotalLength hi
    (setf (mem-ref (+ c 4) :u8) 2)       ;; bNumInterfaces
    (setf (mem-ref (+ c 5) :u8) 1)       ;; bConfigurationValue
    (setf (mem-ref (+ c 6) :u8) 0)       ;; iConfiguration
    (setf (mem-ref (+ c 7) :u8) #xC0)    ;; bmAttributes (self-powered)
    (setf (mem-ref (+ c 8) :u8) 50)      ;; bMaxPower (100mA)
    ;; Interface 0: CDC Communication (9)
    (setf (mem-ref (+ c 9) :u8) 9)
    (setf (mem-ref (+ c 10) :u8) 4)      ;; INTERFACE
    (setf (mem-ref (+ c 11) :u8) 0)      ;; bInterfaceNumber
    (setf (mem-ref (+ c 12) :u8) 0)      ;; bAlternateSetting
    (setf (mem-ref (+ c 13) :u8) 1)      ;; bNumEndpoints
    (setf (mem-ref (+ c 14) :u8) 2)      ;; bInterfaceClass = CDC
    (setf (mem-ref (+ c 15) :u8) 6)      ;; bInterfaceSubClass = ECM
    (setf (mem-ref (+ c 16) :u8) 0)      ;; bInterfaceProtocol
    (setf (mem-ref (+ c 17) :u8) 0)      ;; iInterface
    ;; CDC Header Functional Descriptor (5)
    (setf (mem-ref (+ c 18) :u8) 5)
    (setf (mem-ref (+ c 19) :u8) #x24)   ;; CS_INTERFACE
    (setf (mem-ref (+ c 20) :u8) 0)      ;; Header
    (setf (mem-ref (+ c 21) :u8) #x20)   ;; bcdCDC lo = 1.20
    (setf (mem-ref (+ c 22) :u8) 1)      ;; bcdCDC hi
    ;; CDC Union Functional Descriptor (5)
    (setf (mem-ref (+ c 23) :u8) 5)
    (setf (mem-ref (+ c 24) :u8) #x24)   ;; CS_INTERFACE
    (setf (mem-ref (+ c 25) :u8) 6)      ;; Union
    (setf (mem-ref (+ c 26) :u8) 0)      ;; bControlInterface
    (setf (mem-ref (+ c 27) :u8) 1)      ;; bSubordinateInterface0
    ;; CDC Ethernet Networking Functional Descriptor (13)
    (setf (mem-ref (+ c 28) :u8) 13)
    (setf (mem-ref (+ c 29) :u8) #x24)   ;; CS_INTERFACE
    (setf (mem-ref (+ c 30) :u8) #x0F)   ;; Ethernet Networking
    (setf (mem-ref (+ c 31) :u8) 4)      ;; iMACAddress = string 4
    (setf (mem-ref (+ c 32) :u8) 0)      ;; bmEthernetStatistics
    (setf (mem-ref (+ c 33) :u8) 0)
    (setf (mem-ref (+ c 34) :u8) 0)
    (setf (mem-ref (+ c 35) :u8) 0)
    (setf (mem-ref (+ c 36) :u8) #xEA)   ;; wMaxSegmentSize lo (1514)
    (setf (mem-ref (+ c 37) :u8) 5)      ;; wMaxSegmentSize hi
    (setf (mem-ref (+ c 38) :u8) 0)      ;; wNumberMCFilters
    (setf (mem-ref (+ c 39) :u8) 0)
    (setf (mem-ref (+ c 40) :u8) 0)      ;; bNumberPowerFilters
    ;; Endpoint: Interrupt IN EP3 (7)
    (setf (mem-ref (+ c 41) :u8) 7)
    (setf (mem-ref (+ c 42) :u8) 5)      ;; ENDPOINT
    (setf (mem-ref (+ c 43) :u8) #x83)   ;; EP3 IN
    (setf (mem-ref (+ c 44) :u8) 3)      ;; Interrupt
    (setf (mem-ref (+ c 45) :u8) 16)     ;; wMaxPacketSize lo
    (setf (mem-ref (+ c 46) :u8) 0)
    (setf (mem-ref (+ c 47) :u8) 32)     ;; bInterval
    ;; Interface 1: CDC Data — Alt 0: zero-bandwidth (no endpoints)
    ;; CDC-ECM spec requires Alt 0 with no endpoints;
    ;; host switches to Alt 1 to activate data path
    (setf (mem-ref (+ c 48) :u8) 9)
    (setf (mem-ref (+ c 49) :u8) 4)      ;; INTERFACE
    (setf (mem-ref (+ c 50) :u8) 1)      ;; bInterfaceNumber
    (setf (mem-ref (+ c 51) :u8) 0)      ;; bAlternateSetting = 0
    (setf (mem-ref (+ c 52) :u8) 0)      ;; bNumEndpoints = 0 (zero-bandwidth)
    (setf (mem-ref (+ c 53) :u8) #x0A)   ;; bInterfaceClass = CDC Data
    (setf (mem-ref (+ c 54) :u8) 0)
    (setf (mem-ref (+ c 55) :u8) 0)
    (setf (mem-ref (+ c 56) :u8) 0)
    ;; Interface 1: CDC Data — Alt 1: active data interface with endpoints
    (setf (mem-ref (+ c 57) :u8) 9)
    (setf (mem-ref (+ c 58) :u8) 4)      ;; INTERFACE
    (setf (mem-ref (+ c 59) :u8) 1)      ;; bInterfaceNumber
    (setf (mem-ref (+ c 60) :u8) 1)      ;; bAlternateSetting = 1
    (setf (mem-ref (+ c 61) :u8) 2)      ;; bNumEndpoints = 2
    (setf (mem-ref (+ c 62) :u8) #x0A)   ;; bInterfaceClass = CDC Data
    (setf (mem-ref (+ c 63) :u8) 0)
    (setf (mem-ref (+ c 64) :u8) 0)
    (setf (mem-ref (+ c 65) :u8) 0)
    ;; Endpoint: Bulk IN EP1 (7)
    (setf (mem-ref (+ c 66) :u8) 7)
    (setf (mem-ref (+ c 67) :u8) 5)      ;; ENDPOINT
    (setf (mem-ref (+ c 68) :u8) #x81)   ;; EP1 IN
    (setf (mem-ref (+ c 69) :u8) 2)      ;; Bulk
    (setf (mem-ref (+ c 70) :u8) 64)     ;; wMaxPacketSize (FS)
    (setf (mem-ref (+ c 71) :u8) 0)
    (setf (mem-ref (+ c 72) :u8) 0)
    ;; Endpoint: Bulk OUT EP2 (7)
    (setf (mem-ref (+ c 73) :u8) 7)
    (setf (mem-ref (+ c 74) :u8) 5)      ;; ENDPOINT
    (setf (mem-ref (+ c 75) :u8) 2)      ;; EP2 OUT
    (setf (mem-ref (+ c 76) :u8) 2)      ;; Bulk
    (setf (mem-ref (+ c 77) :u8) 64)     ;; wMaxPacketSize (FS)
    (setf (mem-ref (+ c 78) :u8) 0)
    (setf (mem-ref (+ c 79) :u8) 0)))

(defun gadget-init-string-descs ()
  ;; String 0: Language (4 bytes) at +0x80
  (let ((s (+ (desc-base) #x80)))
    (setf (mem-ref s :u8) 4)
    (setf (mem-ref (+ s 1) :u8) 3)
    (setf (mem-ref (+ s 2) :u8) 9)
    (setf (mem-ref (+ s 3) :u8) 4))
  ;; String 1: "Modus" (12 bytes) at +0x90
  (let ((s (+ (desc-base) #x90)))
    (setf (mem-ref s :u8) 12)
    (setf (mem-ref (+ s 1) :u8) 3)
    (setf (mem-ref (+ s 2) :u8) 77)(setf (mem-ref (+ s 3) :u8) 0)
    (setf (mem-ref (+ s 4) :u8) 111)(setf (mem-ref (+ s 5) :u8) 0)
    (setf (mem-ref (+ s 6) :u8) 100)(setf (mem-ref (+ s 7) :u8) 0)
    (setf (mem-ref (+ s 8) :u8) 117)(setf (mem-ref (+ s 9) :u8) 0)
    (setf (mem-ref (+ s 10) :u8) 115)(setf (mem-ref (+ s 11) :u8) 0))
  ;; String 2: "USB Ether" (20 bytes) at +0xA0
  (let ((s (+ (desc-base) #xA0)))
    (setf (mem-ref s :u8) 20)
    (setf (mem-ref (+ s 1) :u8) 3)
    (setf (mem-ref (+ s 2) :u8) 85)(setf (mem-ref (+ s 3) :u8) 0)
    (setf (mem-ref (+ s 4) :u8) 83)(setf (mem-ref (+ s 5) :u8) 0)
    (setf (mem-ref (+ s 6) :u8) 66)(setf (mem-ref (+ s 7) :u8) 0)
    (setf (mem-ref (+ s 8) :u8) 32)(setf (mem-ref (+ s 9) :u8) 0)
    (setf (mem-ref (+ s 10) :u8) 69)(setf (mem-ref (+ s 11) :u8) 0)
    (setf (mem-ref (+ s 12) :u8) 116)(setf (mem-ref (+ s 13) :u8) 0)
    (setf (mem-ref (+ s 14) :u8) 104)(setf (mem-ref (+ s 15) :u8) 0)
    (setf (mem-ref (+ s 16) :u8) 101)(setf (mem-ref (+ s 17) :u8) 0)
    (setf (mem-ref (+ s 18) :u8) 114)(setf (mem-ref (+ s 19) :u8) 0))
  ;; String 3: "000001" (14 bytes) at +0xC0
  (let ((s (+ (desc-base) #xC0)))
    (setf (mem-ref s :u8) 14)
    (setf (mem-ref (+ s 1) :u8) 3)
    (setf (mem-ref (+ s 2) :u8) 48)(setf (mem-ref (+ s 3) :u8) 0)
    (setf (mem-ref (+ s 4) :u8) 48)(setf (mem-ref (+ s 5) :u8) 0)
    (setf (mem-ref (+ s 6) :u8) 48)(setf (mem-ref (+ s 7) :u8) 0)
    (setf (mem-ref (+ s 8) :u8) 48)(setf (mem-ref (+ s 9) :u8) 0)
    (setf (mem-ref (+ s 10) :u8) 48)(setf (mem-ref (+ s 11) :u8) 0)
    (setf (mem-ref (+ s 12) :u8) 49)(setf (mem-ref (+ s 13) :u8) 0))
  ;; String 4: MAC "020000000001" (26 bytes) at +0xE0
  (let ((s (+ (desc-base) #xE0)))
    (setf (mem-ref s :u8) 26)
    (setf (mem-ref (+ s 1) :u8) 3)
    (setf (mem-ref (+ s 2) :u8) 48)(setf (mem-ref (+ s 3) :u8) 0)
    (setf (mem-ref (+ s 4) :u8) 50)(setf (mem-ref (+ s 5) :u8) 0)
    (setf (mem-ref (+ s 6) :u8) 48)(setf (mem-ref (+ s 7) :u8) 0)
    (setf (mem-ref (+ s 8) :u8) 48)(setf (mem-ref (+ s 9) :u8) 0)
    (setf (mem-ref (+ s 10) :u8) 48)(setf (mem-ref (+ s 11) :u8) 0)
    (setf (mem-ref (+ s 12) :u8) 48)(setf (mem-ref (+ s 13) :u8) 0)
    (setf (mem-ref (+ s 14) :u8) 48)(setf (mem-ref (+ s 15) :u8) 0)
    (setf (mem-ref (+ s 16) :u8) 48)(setf (mem-ref (+ s 17) :u8) 0)
    (setf (mem-ref (+ s 18) :u8) 48)(setf (mem-ref (+ s 19) :u8) 0)
    (setf (mem-ref (+ s 20) :u8) 48)(setf (mem-ref (+ s 21) :u8) 0)
    (setf (mem-ref (+ s 22) :u8) 48)(setf (mem-ref (+ s 23) :u8) 0)
    (setf (mem-ref (+ s 24) :u8) 49)(setf (mem-ref (+ s 25) :u8) 0)))

(defun gadget-init-descriptors ()
  (gadget-init-dev-desc)
  (gadget-init-config-desc)
  (gadget-init-string-descs))

;;; ============================================================
;;; VideoCore mailbox: power on USB controller
;;; ============================================================
;;; BCM2837 mailbox at 0x3F00B880. Required on real hardware
;;; (QEMU has USB always powered). Uses property tag channel 8.
;;; Buffer at 0x01050100 (16-byte aligned, after gadget state).

(defun mbox-base () #x3F00B880)
(defun mbox-read () (mbox-base))
(defun mbox-status () (+ (mbox-base) #x18))
(defun mbox-write () (+ (mbox-base) #x20))
(defun mbox-write-status () (+ (mbox-base) #x38))
(defun mbox-buf () #x01050100)

(defun mbox-send-power (state-val)
  ;; Send SET_POWER_STATE for USB HCD (device 3) with given state
  (let ((buf (mbox-buf)))
    (setf (mem-ref buf :u32) 32)              ;; buffer size
    (setf (mem-ref (+ buf 4) :u32) 0)        ;; request code
    (setf (mem-ref (+ buf 8) :u32) #x00028001) ;; SET_POWER_STATE
    (setf (mem-ref (+ buf 12) :u32) 8)       ;; value buffer size
    (setf (mem-ref (+ buf 16) :u32) 8)       ;; request length
    (setf (mem-ref (+ buf 20) :u32) 3)       ;; device ID = USB HCD
    (setf (mem-ref (+ buf 24) :u32) state-val) ;; state
    (setf (mem-ref (+ buf 28) :u32) 0)       ;; end tag
    ;; Wait until mailbox is not full
    (let ((w 0))
      (loop
        (when (>= w 100000) (return 0))
        (when (zerop (logand (dwc2-read (mbox-write-status)) (ash 1 31)))
          (return 0))
        (setq w (+ w 1))))
    ;; Write buffer address (uncached: |0xC0000000) | channel 8
    (dwc2-write (mbox-write) (logior #xC1050100 8))
    ;; Read response: wait for non-empty, match channel 8
    (let ((r 0))
      (loop
        (when (>= r 100000) (return 0))
        (when (zerop (logand (dwc2-read (mbox-status)) (ash 1 30)))
          (let ((resp (dwc2-read (mbox-read))))
            (when (eq (logand resp #xF) 8)
              (return resp))))
        (setq r (+ r 1))))
    (mem-ref (+ buf 24) :u32)))

(defun mbox-set-uart-clock ()
  ;; Set PL011 UART clock to 48MHz via VideoCore mailbox
  ;; Required on real hardware — firmware only enables mini UART clock
  (let ((buf (mbox-buf)))
    (setf (mem-ref buf :u32) 36)              ;; buffer size
    (setf (mem-ref (+ buf 4) :u32) 0)        ;; request code
    (setf (mem-ref (+ buf 8) :u32) #x00038002) ;; SET_CLOCK_RATE
    (setf (mem-ref (+ buf 12) :u32) 12)      ;; value buffer size
    (setf (mem-ref (+ buf 16) :u32) 12)      ;; request length
    (setf (mem-ref (+ buf 20) :u32) 2)       ;; clock ID = UART
    (setf (mem-ref (+ buf 24) :u32) #x02DC6C00) ;; 48000000 Hz = 48MHz
    (setf (mem-ref (+ buf 28) :u32) 0)       ;; skip turbo = 0
    (setf (mem-ref (+ buf 32) :u32) 0)       ;; end tag
    ;; Wait until mailbox is not full
    (let ((w 0))
      (loop
        (when (>= w 100000) (return 0))
        (when (zerop (logand (dwc2-read (mbox-write-status)) (ash 1 31)))
          (return 0))
        (setq w (+ w 1))))
    ;; Write buffer address | channel 8
    (dwc2-write (mbox-write) (logior #xC1050100 8))
    ;; Read response
    (let ((r 0))
      (loop
        (when (>= r 100000) (return 0))
        (when (zerop (logand (dwc2-read (mbox-status)) (ash 1 30)))
          (let ((resp (dwc2-read (mbox-read))))
            (when (eq (logand resp #xF) 8)
              (return resp))))
        (setq r (+ r 1))))))

(defun mbox-power-on-usb ()
  ;; Power ON USB HCD
  (let ((state (mbox-send-power 3)))  ;; ON | WAIT (bit 1=WAIT, bit 0=1=ON)
    (%serial-byte 80)(%serial-byte 87)(%serial-byte 82)  ;; "PWR"
    (%serial-byte 58) ;; ":"
    (print-hex-byte (logand state #xFF))
    (%serial-byte 10)
    state))

;;; ============================================================
;;; Core DWC2 device mode initialization
;;; ============================================================

(defun gadget-core-reset ()
  ;; Clear PCGCCTL — ensure PHY clocks are running
  ;; If start.elf or boot ROM left clocks gated, all register writes are dead
  (dwc2-write (dwc2-pcgcctl) 0)
  (gadget-delay 10)
  ;; Wait for AHB idle (bit 31 of GRSTCTL)
  (let ((i 0))
    (loop
      (when (>= i 100000) (return 0))
      (when (not (zerop (logand (dwc2-read (dwc2-grstctl)) (ash 1 31))))
        (return 1))
      (setq i (+ i 1))))
  ;; Core soft reset (bit 0)
  (dwc2-write (dwc2-grstctl) 1)
  (let ((i 0))
    (loop
      (when (>= i 100000) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) 1))
        (return 1))
      (setq i (+ i 1))))
  (gadget-delay 100))

(defun gadget-init-fifos ()
  ;; RX FIFO: 512 words (2KB)
  (dwc2-write (dwc2-grxfsiz) 512)
  ;; EP0 TX FIFO: start=512, size=64 words
  (dwc2-write (dwc2-gnptxfsiz) (logior 512 (ash 64 16)))
  ;; EP1 TX FIFO: start=576, size=384 words (1.5KB for Ethernet frames)
  (dwc2-write (+ (dwc2-base) #x104) (logior 576 (ash 384 16)))
  ;; EP3 TX FIFO: start=960, size=16 words
  (dwc2-write (+ (dwc2-base) #x10C) (logior 960 (ash 16 16)))
  ;; Flush all TX FIFOs
  (dwc2-write (dwc2-grstctl) (logior (ash 1 5) (ash #x10 6)))
  (let ((i 0))
    (loop
      (when (>= i 10000) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (ash 1 5)))
        (return 0))
      (setq i (+ i 1))))
  ;; Flush RX FIFO
  (dwc2-write (dwc2-grstctl) (ash 1 4))
  (let ((i 0))
    (loop
      (when (>= i 10000) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (ash 1 4)))
        (return 0))
      (setq i (+ i 1)))))

(defun gadget-ep0-prepare ()
  ;; Clear any stale OUT endpoint 0 interrupt flags before re-arming
  (dwc2-write (dwc2-doepint 0) #xFFFFFFFF)
  ;; Program DOEPTSIZ[0]: XFERSIZE=64, PKTCNT=1, SUPCNT=3
  (dwc2-write (dwc2-doeptsiz 0)
              (logior 64 (logior (ash 1 19) (ash 3 29))))
  ;; Enable EP0 OUT: CNAK | EPENA
  (let ((ctl (dwc2-read (dwc2-doepctl 0))))
    (dwc2-write (dwc2-doepctl 0)
                (logior ctl (logior (ash 1 26) (ash 1 31))))))

(defun gadget-init ()
  (write-char-serial 88)  ;; 'X' raw serial test
  (trace-reset)
  ;; Clear state
  (gadget-set-configured 0)
  (gadget-set-addr-pending 0)
  (gadget-set-rx-ready 0)
  (gadget-set-rx-len 0)
  (gadget-set-rx-offset 0)
  (setf (mem-ref (+ (gadget-state) #x20) :u32) 0)  ;; diagnostic counter
  ;; Power ON USB
  (mbox-power-on-usb)
  (gadget-delay 500)
  ;; Init descriptors
  (gadget-init-descriptors)
  ;; ---- Phase 1: Ungate PHY clocks ----
  (dwc2-write (dwc2-pcgcctl) 0)
  (gadget-delay 10)
  ;; ---- Phase 2: Core reset (TinyUSB approach: single reset) ----
  (gadget-core-reset)
  (dwc2-write (dwc2-pcgcctl) 0)
  (gadget-delay 100)
  ;; ---- Phase 3: Configure GUSBCFG ----
  ;; PHYSEL=0 (UTMI+ — hardware has it per GHWCFG2), TRDT=9 (8-bit), TOCAL=7
  ;; ForceDevMode(30)
  (let ((cfg (dwc2-read (dwc2-gusbcfg))))
    (let ((mask (logior (ash 1 6) (logior (ash 1 4) (logior (ash 1 3)
                (logior (ash 1 29) (logior (ash #xF 10) 7)))))))
      (let ((c1 (logand cfg (logxor mask #xFFFFFFFF))))
        (let ((c2 (logior c1 (logior (ash 1 30) (logior (ash 9 10) 7)))))
          (dwc2-write (dwc2-gusbcfg) c2)))))
  (gadget-delay 200)  ;; Wait for mode switch
  ;; Debug: print GUSBCFG
  (%serial-byte 71)(%serial-byte 85)(%serial-byte 58)  ;; "GU:"
  (let ((gu (dwc2-read (dwc2-gusbcfg))))
    (print-hex-byte (logand gu #xFF))
    (print-hex-byte (logand (ash gu -8) #xFF))
    (print-hex-byte (logand (ash gu -16) #xFF))
    (print-hex-byte (logand (ash gu -24) #xFF)))
  (%serial-byte 10)
  ;; ---- Phase 4: Force session valid (GOTGCTL) ----
  ;; Force VbValid + BValid + AValid for device mode without VBUS sensing
  (let ((otg (dwc2-read (dwc2-base))))
    (dwc2-write (dwc2-base) (logior otg #xFC)))
  ;; ---- Phase 5: DCFG ----
  ;; DevSpd=1 (FS via UTMI+ 30/60MHz) | NZLSOHSK (bit 2)
  ;; Force Full Speed to avoid HS chirp — HS gets EPROTO on read/all
  ;; FS is known to work for 8-byte reads; testing full enumeration at FS
  (dwc2-write (dwc2-dcfg) 5)
  (%serial-byte 68)(%serial-byte 67)(%serial-byte 58)  ;; "DC:"
  (print-hex-byte (logand (dwc2-read (dwc2-dcfg)) #xFF))
  (%serial-byte 10)
  ;; ---- Phase 6: AHB + FIFOs + interrupts ----
  (dwc2-write (dwc2-gahbcfg) 1)
  ;; Flush all FIFOs (TinyUSB does this in core_init after reset)
  (dwc2-write (dwc2-grstctl) (logior (ash 1 5) (ash #x10 6)))
  (let ((i 0))
    (loop (when (>= i 10000) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (ash 1 5))) (return 0))
      (setq i (+ i 1))))
  (dwc2-write (dwc2-grstctl) (ash 1 4))
  (let ((i 0))
    (loop (when (>= i 10000) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (ash 1 4))) (return 0))
      (setq i (+ i 1))))
  ;; Clear all pending interrupts
  (dwc2-write (dwc2-gintsts) #xFFFFFFFF)
  ;; Init FIFOs
  (gadget-init-fifos)
  ;; Interrupt masks — match TinyUSB: OTGInt(2), SOF(3), RxFLvl(4), USBRst(12),
  ;; EnumDone(13), IEPInt(18), OEPInt(19), WkUpInt(31)
  (dwc2-write (dwc2-gintmsk)
              (logior (ash 1 2) (logior (ash 1 3) (logior (ash 1 4) (logior (ash 1 12)
              (logior (ash 1 13) (logior (ash 1 18) (logior (ash 1 19)
              (ash 1 31)))))))))
  (dwc2-write (dwc2-daintmsk) (logior #xF (ash #xF 16)))
  (dwc2-write (dwc2-diepmsk) (logior 1 (ash 1 3)))
  (dwc2-write (dwc2-doepmsk) (logior 1 (logior (ash 1 3) (ash 1 5))))
  ;; Clear all EP interrupts
  (dwc2-write (dwc2-diepint 0) #xFFFFFFFF)
  (dwc2-write (dwc2-doepint 0) #xFFFFFFFF)
  ;; Clear Global NAKs
  (let ((dctl (dwc2-read (dwc2-dctl))))
    (dwc2-write (dwc2-dctl) (logior dctl (logior (ash 1 10) (ash 1 8)))))
  ;; Prepare EP0 for SETUP
  (gadget-ep0-prepare)
  ;; ---- Phase 7: Soft connect ----
  (gadget-delay 500)  ;; Let host see disconnect state
  (let ((dctl (dwc2-read (dwc2-dctl))))
    (let ((cleared (logand dctl (logxor 2 #xFFFFFFFF))))
      (dwc2-write (dwc2-dctl) (logior cleared (logior (ash 1 10) (ash 1 8))))))
  (dwc2-write (dwc2-gahbcfg) 1)
  (%serial-byte 71)(%serial-byte 65)(%serial-byte 68)(%serial-byte 10)  ;; "GAD\n"
  1)

;;; ============================================================
;;; EP0 SETUP handling
;;; ============================================================

(defun gadget-ep0-send-data (src len wlength)
  (trace-event-val #x16 len)  ;; EP0-SEND len
  ;; Send min(len, wlength) bytes on EP0 IN
  (let ((send-len (if (< len wlength) len wlength)))
    (let ((pktcnt (if (zerop send-len) 1 (/ (+ send-len 63) 64))))
      ;; NO TX FIFO flush here — TinyUSB doesn't do it, and flushing during
      ;; an ongoing NAK response can corrupt the wire transmission → EPROTO.
      ;; Program DIEPTSIZ[0]
      (dwc2-write (dwc2-dieptsiz 0) (logior send-len (ash pktcnt 19)))
      ;; Enable EP0 IN: CNAK | EPENA (BEFORE FIFO write per DWC2 slave mode)
      (let ((ctl (dwc2-read (dwc2-diepctl 0))))
        (dwc2-write (dwc2-diepctl 0)
                    (logior ctl (logior (ash 1 26) (ash 1 31)))))
      ;; Write data to TX FIFO (after EPENA, per DWC2 slave mode protocol)
      (fifo-write-from-desc src send-len)
      ;; Prepare EP0 OUT for STATUS
      (gadget-ep0-prepare))))

(defun gadget-ep0-send-zlp ()
  ;; Send zero-length packet on EP0 IN (STATUS stage)
  ;; Clear stale DIEPINT[0] flags first
  (dwc2-write (dwc2-diepint 0) #xFF)
  (dwc2-write (dwc2-dieptsiz 0) (ash 1 19))
  (let ((ctl (dwc2-read (dwc2-diepctl 0))))
    (dwc2-write (dwc2-diepctl 0)
                (logior ctl (logior (ash 1 26) (ash 1 31)))))
  (gadget-ep0-prepare))

(defun gadget-ep0-stall ()
  ;; STALL EP0 IN
  (let ((ctl (dwc2-read (dwc2-diepctl 0))))
    (dwc2-write (dwc2-diepctl 0) (logior ctl (ash 1 21))))
  (gadget-ep0-prepare))

(defun gadget-get-string (idx wlength)
  (if (eq idx 0)
      (gadget-ep0-send-data (+ (desc-base) #x80) 4 wlength)
      (if (eq idx 1)
          (gadget-ep0-send-data (+ (desc-base) #x90) 16 wlength)
          (if (eq idx 2)
              (gadget-ep0-send-data (+ (desc-base) #xA0) 20 wlength)
              (if (eq idx 3)
                  (gadget-ep0-send-data (+ (desc-base) #xC0) 14 wlength)
                  (if (eq idx 4)
                      (gadget-ep0-send-data (+ (desc-base) #xE0) 26 wlength)
                      (gadget-ep0-stall)))))))

(defun gadget-activate-endpoints ()
  ;; EP1 IN (Bulk): MPS=64, USBActEP, EPType=Bulk, TxFNum=1
  (dwc2-write (dwc2-diepctl 1)
              (logior 64 (logior (ash 1 15) (logior (ash 2 18)
              (logior (ash 1 22) (ash 1 28))))))
  ;; EP2 OUT (Bulk): MPS=64, USBActEP, EPType=Bulk
  (dwc2-write (dwc2-doepctl 2)
              (logior 64 (logior (ash 1 15) (logior (ash 2 18) (ash 1 28)))))
  ;; EP3 IN (Interrupt): MPS=16, USBActEP, EPType=Interrupt, TxFNum=3
  ;; DIEPCTL[3] = 16 | (1<<15) | (3<<18) | (3<<22) | (1<<28)
  (dwc2-write (dwc2-diepctl 3)
              (logior 16 (logior (ash 1 15) (logior (ash 3 18)
              (logior (ash 3 22) (ash 1 28))))))
  ;; Arm EP2 OUT for receiving
  (gadget-arm-ep2))

(defun gadget-arm-ep2 ()
  ;; Clear residual Global OUT NAK (safety: SETUP handling may leave it set)
  (let ((dctl (dwc2-read (dwc2-dctl))))
    (dwc2-write (dwc2-dctl) (logior dctl (ash 1 10))))  ;; CGOUTNAK
  ;; Clear stale DOEPINT[2] flags
  (dwc2-write (dwc2-doepint 2) #xFFFFFFFF)
  ;; Program DOEPTSIZ[2]: XFERSIZE=2048, PKTCNT=32
  (dwc2-write (dwc2-doeptsiz 2) (logior 2048 (ash 32 19)))
  ;; Enable EP2 OUT: CNAK | EPENA
  (let ((ctl (dwc2-read (dwc2-doepctl 2))))
    (dwc2-write (dwc2-doepctl 2)
                (logior ctl (logior (ash 1 26) (ash 1 31))))))

(defun gadget-handle-setup ()
  (let ((breq (mem-ref (+ (setup-pkt) 1) :u8))
        (wval-lo (mem-ref (+ (setup-pkt) 2) :u8))
        (wval-hi (mem-ref (+ (setup-pkt) 3) :u8))
        (wlen-lo (mem-ref (+ (setup-pkt) 6) :u8))
        (wlen-hi (mem-ref (+ (setup-pkt) 7) :u8)))
    (let ((wvalue (logior wval-lo (ash wval-hi 8)))
          (wlength (logior wlen-lo (ash wlen-hi 8))))
      (trace-event-val #x15 breq)  ;; SETUP bRequest
      ;; Debug: print "S:XX" for every SETUP (breq in hex)
      (%serial-byte 83)(%serial-byte 58)  ;; "S:"
      (print-hex-byte breq)
      (%serial-byte 87)(%serial-byte 58)  ;; "W:"
      (print-hex-byte (logand wlength #xFF))
      (%serial-byte 10)
      (if (eq breq 6)       ;; GET_DESCRIPTOR
          (let ((desc-type (ash wvalue -8))
                (desc-idx (logand wvalue #xFF)))
            (if (eq desc-type 1)
                (gadget-ep0-send-data (desc-base) 18 wlength)
                (if (eq desc-type 2)
                    (gadget-ep0-send-data (+ (desc-base) #x20) 80 wlength)
                    (if (eq desc-type 3)
                        (gadget-get-string desc-idx wlength)
                        (gadget-ep0-stall)))))
          (if (eq breq 5)   ;; SET_ADDRESS
              (progn
                ;; Apply address IMMEDIATELY (before ZLP), matching Linux dwc2 gadget driver.
                ;; DWC2 core internally latches: uses OLD address for STATUS phase,
                ;; NEW address for subsequent transactions.
                (%serial-byte 83)(%serial-byte 65)(%serial-byte 58)  ;; "SA:"
                (print-hex-byte (logand wvalue #xFF))
                (let ((dcfg (dwc2-read (dwc2-dcfg))))
                  (let ((cleared (logand dcfg (logxor (ash #x7F 4) #xFFFFFFFF))))
                    (dwc2-write (dwc2-dcfg) (logior cleared (ash wvalue 4)))))
                (%serial-byte 10)
                (gadget-ep0-send-zlp))
              (if (eq breq 9)   ;; SET_CONFIGURATION
                  (progn
                    (gadget-set-configured wvalue)
                    (when (not (zerop wvalue))
                      (gadget-activate-endpoints))
                    (gadget-ep0-send-zlp))
                  (if (eq breq 11)  ;; SET_INTERFACE
                      (progn
                        ;; CDC-ECM: Alt 0 = no endpoints, Alt 1 = data active
                        (%serial-byte 65)(%serial-byte 76)(%serial-byte 84)(%serial-byte 58)  ;; "ALT:"
                        (print-hex-byte (logand wvalue #xFF))
                        (%serial-byte 10)
                        (if (eq wvalue 1)
                            ;; Alt 1: activate data endpoints + reset DATA0
                            (progn
                              (gadget-activate-endpoints)
                              (gadget-ep0-send-zlp))
                            ;; Alt 0: deactivate data endpoints
                            (progn
                              (let ((ctl2 (dwc2-read (dwc2-doepctl 2))))
                                (dwc2-write (dwc2-doepctl 2) (logior ctl2 (ash 1 27))))
                              (gadget-ep0-send-zlp))))
                      ;; CDC or unknown: ACK
                      (gadget-ep0-send-zlp))))))))

;;; ============================================================
;;; USB event handling
;;; ============================================================

(defun gadget-handle-reset ()
  (trace-event #x10)  ;; RST
  (%serial-byte 82)(%serial-byte 83)(%serial-byte 84)(%serial-byte 10)  ;; "RST\n"
  (gadget-set-configured 0)
  (gadget-set-addr-pending 0)
  ;; Reset address to 0
  (let ((dcfg (dwc2-read (dwc2-dcfg))))
    (dwc2-write (dwc2-dcfg) (logand dcfg (logxor (ash #x7F 4) #xFFFFFFFF))))
  ;; NAK all non-EP0 OUT endpoints (Linux/TinyUSB pattern)
  ;; This prevents stale endpoint state from corrupting re-enumeration
  (let ((ctl2 (dwc2-read (dwc2-doepctl 2))))
    (dwc2-write (dwc2-doepctl 2) (logior ctl2 (ash 1 27))))  ;; SNAK on EP2 OUT
  ;; Disable EP1 IN and EP3 IN if enabled
  (let ((ctl1 (dwc2-read (dwc2-diepctl 1))))
    (when (not (zerop (logand ctl1 (ash 1 31))))  ;; if EPEna
      (dwc2-write (dwc2-diepctl 1) (logior ctl1 (logior (ash 1 30) (ash 1 27))))))
  (let ((ctl3 (dwc2-read (dwc2-diepctl 3))))
    (when (not (zerop (logand ctl3 (ash 1 31))))  ;; if EPEna
      (dwc2-write (dwc2-diepctl 3) (logior ctl3 (logior (ash 1 30) (ash 1 27))))))
  ;; Clear IN endpoint interrupts (cancel pending IN transfers)
  (dwc2-write (dwc2-diepint 0) #xFFFFFFFF)
  (dwc2-write (dwc2-diepint 1) #xFFFFFFFF)
  (dwc2-write (dwc2-diepint 3) #xFFFFFFFF)
  ;; Clear EP2 OUT interrupt
  (dwc2-write (dwc2-doepint 2) #xFFFFFFFF)
  ;; DO NOT clear DOEPINT[0] — valid SetUp flag may be pending
  ;; DO NOT flush RX FIFO — valid SETUP data from host may be there
  ;; DO NOT reinit FIFO sizes — they survive USB bus reset
  ;; DO NOT clear GINTSTS — OEPInt/RxFLvl may be pending for valid SETUP
  ;; Flush TX FIFOs only (cancel any pending IN data)
  (dwc2-write (dwc2-grstctl) (logior (ash 1 5) (ash #x10 6)))
  (let ((i 0))
    (loop
      (when (>= i 10000) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (ash 1 5)))
        (return 0))
      (setq i (+ i 1))))
  ;; Clear Global NAKs
  (let ((dctl (dwc2-read (dwc2-dctl))))
    (dwc2-write (dwc2-dctl) (logior dctl (logior (ash 1 10) (ash 1 8)))))
  ;; Set EP0 MPS on both IN and OUT (0=64 bytes for HS/FS)
  (let ((ctl (dwc2-read (dwc2-diepctl 0))))
    (dwc2-write (dwc2-diepctl 0) (logand ctl (logxor 3 #xFFFFFFFF))))
  (let ((ctl (dwc2-read (dwc2-doepctl 0))))
    (dwc2-write (dwc2-doepctl 0) (logand ctl (logxor 3 #xFFFFFFFF))))
  ;; DO NOT arm EP0 here — EnumDone handler will do it.
  ;; This avoids double-arming when USBRst + EnumDone fire together.
  )

(defun gadget-handle-enum-done ()
  (trace-event #x11)  ;; ENUM
  (%serial-byte 69)(%serial-byte 68)(%serial-byte 10)  ;; "ED\n" = EnumDone
  ;; Enumeration complete — read DSTS for negotiated speed
  ;; DSTS bits 2:1: 0=HS, 1=FS(UTMI 30/60), 2=LS, 3=FS(48)
  ;; EP0 MPS=64 for HS and FS (DIEPCTL[0] bits 1:0 = 0)
  (let ((ctl (dwc2-read (dwc2-diepctl 0))))
    (dwc2-write (dwc2-diepctl 0) (logand ctl (logxor 3 #xFFFFFFFF))))
  ;; Update GUSBCFG USBTRDTIM based on negotiated speed
  ;; HS with 16-bit UTMI: 5, HS with 8-bit UTMI: 9, FS: 5
  (let ((dsts (dwc2-read (dwc2-dsts))))
    (let ((speed (logand (ash dsts -1) 3)))
      (let ((trdtim (if (zerop speed) 9 5)))  ;; 0=HS→9(8-bit), else FS→5
        (let ((cfg (dwc2-read (dwc2-gusbcfg))))
          (let ((c1 (logand cfg (logxor (ash #xF 10) #xFFFFFFFF))))
            (dwc2-write (dwc2-gusbcfg) (logior c1 (ash trdtim 10))))))))
  ;; Re-arm EP0 for SETUP
  (gadget-ep0-prepare))

(defun gadget-handle-ep0-in-complete ()
  (trace-event #x17)  ;; EP0-IN-COMPLETE
  ;; Always re-arm EP0 OUT after IN transfer completes.
  ;; This ensures EP0 is ready for the next SETUP packet.
  (gadget-ep0-prepare))

(defun gadget-handle-rxflvl ()
  ;; Read and pop RX status
  (let ((rxsts (dwc2-read (dwc2-grxstsp))))
    (let ((ep (logand rxsts #xF))
          (bcnt (logand (ash rxsts -4) #x7FF))
          (pktsts (logand (ash rxsts -17) #xF)))
      ;; SETUP data received (pktsts=6) — read data from RX FIFO
      (when (eq pktsts 6)
        (trace-event #x13)  ;; SETUP-RX
        (fifo-read-to-buf (setup-pkt) bcnt))
      ;; SETUP complete (pktsts=4) — process SETUP + re-arm STUPCNT
      ;; Process here (not in OEPInt) to avoid race where gadget-handle-reset
      ;; or gadget-ep0-prepare clears the DOEPINT SetUp flag before we read it.
      ;; This matches TinyUSB's approach (handle SETUP in RxFLvl).
      (when (eq pktsts 4)
        (trace-event #x14)  ;; SETUP-DONE
        (let ((tsiz (dwc2-read (dwc2-doeptsiz 0))))
          (dwc2-write (dwc2-doeptsiz 0) (logior tsiz (ash 3 29))))
        (gadget-handle-setup)
        ;; Clear Global OUT NAK — DWC2 sets GOUTNakEff on SETUP reception,
        ;; which blocks ALL OUT endpoints (including EP2 bulk) until cleared.
        ;; Both Linux dwc2 gadget and TinyUSB clear this after SETUP handling.
        (let ((dctl (dwc2-read (dwc2-dctl))))
          (dwc2-write (dwc2-dctl) (logior dctl (ash 1 10)))))
      ;; OUT data received (pktsts=2)
      (when (eq pktsts 2)
        (if (eq ep 2)
            ;; Bulk OUT — Ethernet frame data
            (progn
              (let ((offset (gadget-rx-offset)))
                (fifo-read-to-buf (+ (e1000-rx-buf-base) offset) bcnt)
                (gadget-set-rx-offset (+ offset bcnt))))
            ;; EP0 OUT data (STATUS ZLP) or unexpected — discard
            (fifo-discard bcnt)))
      ;; OUT transfer complete (pktsts=3)
      (when (eq pktsts 3)
        (when (eq ep 2)
          ;; Ethernet frame complete
          (let ((total (gadget-rx-offset)))
            (when (> total 0)
              (gadget-set-rx-len total)
              (gadget-set-rx-ready 1)
              (gadget-set-rx-offset 0))))))))

(defun gadget-poll ()
  ;; Handle USB events by polling GINTSTS
  (let ((sts (dwc2-read (dwc2-gintsts))))
    ;; Trace only interesting GINTSTS events: RST(12), EnumDone(13), RxFLvl(4), IEP(18), OEP(19)
    (let ((interesting (logand sts (logior (ash 1 4) (logior (ash 1 12) (logior (ash 1 13)
                       (logior (ash 1 18) (ash 1 19))))))))
      (when (not (zerop interesting))
        (trace-event-val #x19 sts)))
    ;; Bus Reset
    (when (not (zerop (logand sts (ash 1 12))))
      (gadget-handle-reset)
      (dwc2-write (dwc2-gintsts) (ash 1 12)))
    ;; Enum Done
    (when (not (zerop (logand sts (ash 1 13))))
      (gadget-handle-enum-done)
      (dwc2-write (dwc2-gintsts) (ash 1 13)))
    ;; RX FIFO non-empty — drain all pending entries
    (when (not (zerop (logand sts (ash 1 4))))
      (let ((rx-count 0))
        (loop
          (when (>= rx-count 32) (return 0))
          (when (zerop (logand (dwc2-read (dwc2-gintsts)) (ash 1 4)))
            (return 0))
          (gadget-handle-rxflvl)
          (setq rx-count (+ rx-count 1)))))
    ;; IN endpoint interrupt
    (when (not (zerop (logand sts (ash 1 18))))
      ;; Check EP0 IN complete
      (let ((ep0int (dwc2-read (dwc2-diepint 0))))
        (when (not (zerop (logand ep0int 1)))
          (dwc2-write (dwc2-diepint 0) 1)
          (gadget-handle-ep0-in-complete)))
      ;; Clear EP1 IN complete
      (let ((ep1int (dwc2-read (dwc2-diepint 1))))
        (when (not (zerop (logand ep1int 1)))
          (dwc2-write (dwc2-diepint 1) 1))))
    ;; OUT endpoint interrupt
    (when (not (zerop (logand sts (ash 1 19))))
      ;; EP0 OUT: SETUP is now handled in RxFLvl (pktsts=4).
      ;; Just clear all DOEPINT[0] flags here to de-assert OEPInt.
      (let ((ep0int (dwc2-read (dwc2-doepint 0))))
        (trace-event-val #x18 ep0int)  ;; OEP0INT value
        (when (not (zerop ep0int))
          (dwc2-write (dwc2-doepint 0) ep0int)))
      ;; EP2 OUT complete — finalize frame + re-arm
      (let ((ep2int (dwc2-read (dwc2-doepint 2))))
        (when (not (zerop (logand ep2int 1)))
          (dwc2-write (dwc2-doepint 2) ep2int)
          ;; Backup: if pktsts=3 was missed in RxFLvl drain, finalize here
          (let ((total (gadget-rx-offset)))
            (when (> total 0)
              (gadget-set-rx-len total)
              (gadget-set-rx-ready 1)
              (gadget-set-rx-offset 0)))
          (gadget-arm-ep2))))))

;;; ============================================================
;;; Interrupt-driven ring buffer support
;;; ============================================================
;;;
;;; The ISR in boot-rpi.lisp drains the RX FIFO into a 4-slot ring buffer
;;; at usb-ring-base (0x01090000). These functions initialize, enable,
;;; and consume from the ring buffer.
;;;
;;; Ring layout:
;;;   +0x000  write_idx (u32)      +0x004  read_idx (u32)
;;;   +0x008  frame_len[4] (16B)   +0x018  frame_accum (u32)
;;;   +0x01C  deferred (u32)       +0x800  slot 0..3 data (2KB each)

(defun gadget-ring-init ()
  ;; Zero ring buffer header
  (let ((base (usb-ring-base)))
    (setf (mem-ref base :u32) 0)            ;; write_idx
    (setf (mem-ref (+ base 4) :u32) 0)     ;; read_idx
    (setf (mem-ref (+ base 8) :u32) 0)     ;; frame_len[0]
    (setf (mem-ref (+ base 12) :u32) 0)    ;; frame_len[1]
    (setf (mem-ref (+ base 16) :u32) 0)    ;; frame_len[2]
    (setf (mem-ref (+ base 20) :u32) 0)    ;; frame_len[3]
    (setf (mem-ref (+ base #x18) :u32) 0)  ;; frame_accum
    (setf (mem-ref (+ base #x1C) :u32) 0)));; deferred

(defun gadget-enable-irq ()
  ;; Initialize ring buffer, enable BCM USB IRQ, unmask interrupts
  (gadget-ring-init)
  ;; BCM Enable_IRQs_1: bit 9 = USB
  (setf (mem-ref #x3F00B210 :u32) #x200)
  ;; STI: unmask IRQ+FIQ (MSR DAIFClr, #3)
  (sti))

(defun gadget-check-deferred ()
  ;; Bottom-half: check deferred flags set by ISR and handle them.
  ;; Runs with IRQs disabled to avoid concurrent ISR execution.
  (cli)
  (let ((base (usb-ring-base)))
    (let ((flags (mem-ref (+ base #x1C) :u32)))
      (when (not (zerop flags))
        ;; Clear flags before handling (ISR may set new ones)
        (setf (mem-ref (+ base #x1C) :u32) 0)
        ;; bit 0: USB reset
        (when (not (zerop (logand flags 1)))
          (gadget-handle-reset))
        ;; bit 1: enum done
        (when (not (zerop (logand flags 2)))
          (gadget-handle-enum-done))
        ;; bit 2: SETUP packet (data at setup-pkt = 0x01000080)
        (when (not (zerop (logand flags 4)))
          (gadget-handle-setup)))))
  (sti))

;;; ============================================================
;;; NIC API (same interface as e1000.lisp / cdc-ether.lisp)
;;; ============================================================

(defun ring-receive ()
  ;; Check ISR ring buffer for a completed frame.
  ;; ISR writes frames to ring slots at usb-ring-base + 0x800 + idx*0x800.
  ;; Copy to e1000-rx-buf-base (where networking stack expects it).
  (let ((base (usb-ring-base)))
    (let ((wi (mem-ref base :u32)))
      (let ((ri (mem-ref (+ base 4) :u32)))
        (if (= wi ri)
            0
            (let ((len (mem-ref (+ base (+ 8 (* ri 4))) :u32)))
              (let ((src (+ base (+ #x800 (* ri #x800)))))
                (let ((dst (e1000-rx-buf-base)))
                  (let ((i 0))
                    (loop
                      (when (>= i len) (return 0))
                      (setf (mem-ref (+ dst i) :u8) (mem-ref (+ src i) :u8))
                      (setq i (+ i 1))))))
              (setf (mem-ref (+ base 4) :u32) (logand (+ ri 1) 3))
              len))))))

(defun e1000-receive ()
  ;; Try ISR ring buffer first (interrupt-driven path).
  ;; If ring is empty, fall back to polling (handles pre-IRQ enumeration
  ;; and the case where gadget-enable-irq hasn't been called yet).
  (let ((rlen (ring-receive)))
    (if (not (zerop rlen))
        rlen
        (progn
          (gadget-check-deferred)
          (gadget-poll)
          (if (not (zerop (gadget-rx-ready)))
              (let ((len (gadget-rx-len)))
                (gadget-set-rx-ready 0)
                len)
              0)))))

(defun e1000-rx-buf ()
  (e1000-rx-buf-base))

(defun e1000-send (buf len)
  ;; Copy Lisp byte array to TX buffer, then send via EP1 IN
  (let ((tx-buf (e1000-tx-buf-base)))
    (let ((i 0))
      (loop
        (when (>= i len) (return 0))
        (setf (mem-ref (+ tx-buf i) :u8) (aref buf i))
        (setq i (+ i 1))))
    ;; Wait for previous TX to complete
    (let ((w 0))
      (loop
        (when (>= w 50000) (return 0))
        (when (zerop (logand (dwc2-read (dwc2-diepctl 1)) (ash 1 31)))
          (return 0))
        (setq w (+ w 1))))
    ;; Program DIEPTSIZ[1]: XFERSIZE=len, PKTCNT
    (let ((pktcnt (/ (+ len 63) 64)))
      (dwc2-write (dwc2-dieptsiz 1) (logior len (ash pktcnt 19))))
    ;; Enable EP1 IN: CNAK | EPENA
    (let ((ctl (dwc2-read (dwc2-diepctl 1))))
      (dwc2-write (dwc2-diepctl 1)
                  (logior ctl (logior (ash 1 26) (ash 1 31)))))
    ;; Write to EP1 TX FIFO
    (fifo-write-from-buf 1 tx-buf len)
    1))

(defun e1000-probe ()
  (cdc-ether-init))

;; E1000 register stubs
(defun e1000-write-reg (reg val) nil)
(defun e1000-read-reg (reg) 0)

;;; ============================================================
;;; Main initialization
;;; ============================================================

(defun cdc-ether-init ()
  ;; Initialize DWC2 device mode + CDC-ECM gadget
  (gadget-init)
  ;; Set up network state
  (let ((state (e1000-state-base)))
    ;; MAC: 02:00:00:00:00:01 (locally administered)
    (setf (mem-ref (+ state #x08) :u8) 2)
    (setf (mem-ref (+ state #x09) :u8) 0)
    (setf (mem-ref (+ state #x0A) :u8) 0)
    (setf (mem-ref (+ state #x0B) :u8) 0)
    (setf (mem-ref (+ state #x0C) :u8) 0)
    (setf (mem-ref (+ state #x0D) :u8) 1)
    ;; Static IP: 10.0.0.2 (host will be 10.0.0.1)
    (setf (mem-ref (+ state #x18) :u32) #x0200000A)
    (setf (mem-ref (+ state #x1C) :u32) #x0100000A)
    ;; Gateway MAC: broadcast (FF:FF:FF:FF:FF:FF) as default for point-to-point CDC-ECM.
    ;; ip-send uses state+0x28 as destination MAC. Without this, TCP SYN-ACK goes to
    ;; 00:00:00:00:00:00 which the host drops. ARP reply learns the real MAC later.
    (setf (mem-ref (+ state #x28) :u8) #xFF)
    (setf (mem-ref (+ state #x29) :u8) #xFF)
    (setf (mem-ref (+ state #x2A) :u8) #xFF)
    (setf (mem-ref (+ state #x2B) :u8) #xFF)
    (setf (mem-ref (+ state #x2C) :u8) #xFF)
    (setf (mem-ref (+ state #x2D) :u8) #xFF)
    ;; Clear cursors
    (setf (mem-ref (+ state #x10) :u32) 0)
    (setf (mem-ref (+ state #x14) :u32) 0)
    (setf (mem-ref (+ state #x44) :u32) 0))
  ;; Clear ALL stale state before entering enum loop
  ;; GOTGINT: clear pending OTG interrupts (keeps OTGInt set in GINTSTS)
  (dwc2-write (+ (dwc2-base) 4) #xFFFFFFFF)  ;; GOTGINT at offset 0x004
  ;; GINTSTS: clear all pending interrupts
  (dwc2-write (dwc2-gintsts) #xFFFFFFFF)
  ;; Flush RX FIFO (may have stale data from ROM USB boot)
  (dwc2-write (dwc2-grstctl) (ash 1 4))
  (let ((i 0))
    (loop (when (>= i 10000) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (ash 1 4))) (return 0))
      (setq i (+ i 1))))
  ;; Drain stale RX FIFO entries (internal SETUP buffer not cleared by flush)
  ;; Read and discard all pending GRXSTSP entries until RxFLvl clears
  (let ((n 0))
    (loop
      (when (>= n 64) (return 0))
      (when (zerop (logand (dwc2-read (dwc2-gintsts)) (ash 1 4)))
        (return 0))
      (let ((rxsts (dwc2-read (dwc2-grxstsp))))
        (let ((bcnt (logand (ash rxsts -4) #x7FF))
              (pktsts (logand (ash rxsts -17) #xF)))
          ;; Discard FIFO data for data packets (pktsts=2 or 6)
          (when (eq pktsts 2) (fifo-discard bcnt))
          (when (eq pktsts 6) (fifo-discard bcnt))))
      (setq n (+ n 1))))
  ;; Clear GINTSTS after drain (clear stale OEPInt/RxFLvl)
  (dwc2-write (dwc2-gintsts) #xFFFFFFFF)
  ;; Re-arm EP0 after drain
  (gadget-ep0-prepare)
  ;; Print hardware registers (after deploy output window closes)
  (%serial-byte 73)(%serial-byte 68)(%serial-byte 58)  ;; "ID:"
  (let ((id (dwc2-read (+ (dwc2-base) #x40))))  ;; GSNPSID
    (print-hex-byte (logand id #xFF))
    (print-hex-byte (logand (ash id -8) #xFF))
    (print-hex-byte (logand (ash id -16) #xFF))
    (print-hex-byte (logand (ash id -24) #xFF)))
  (%serial-byte 10)
  (%serial-byte 72)(%serial-byte 87)(%serial-byte 58)  ;; "HW:"
  (let ((hw (dwc2-read (+ (dwc2-base) #x48))))  ;; GHWCFG2
    (print-hex-byte (logand hw #xFF))
    (print-hex-byte (logand (ash hw -8) #xFF))
    (print-hex-byte (logand (ash hw -16) #xFF))
    (print-hex-byte (logand (ash hw -24) #xFF)))
  (%serial-byte 10)
  (%serial-byte 72)(%serial-byte 52)(%serial-byte 58)  ;; "H4:"
  (let ((h4 (dwc2-read (+ (dwc2-base) #x50))))  ;; GHWCFG4
    (print-hex-byte (logand h4 #xFF))
    (print-hex-byte (logand (ash h4 -8) #xFF))
    (print-hex-byte (logand (ash h4 -16) #xFF))
    (print-hex-byte (logand (ash h4 -24) #xFF)))
  (%serial-byte 10)
  ;; GHWCFG3 (bits 31:16 = DFIFO depth in 32-bit words)
  (%serial-byte 72)(%serial-byte 51)(%serial-byte 58)  ;; "H3:"
  (let ((h3 (dwc2-read (+ (dwc2-base) #x4C))))
    (print-hex-byte (logand h3 #xFF))
    (print-hex-byte (logand (ash h3 -8) #xFF))
    (print-hex-byte (logand (ash h3 -16) #xFF))
    (print-hex-byte (logand (ash h3 -24) #xFF)))
  (%serial-byte 10)
  ;; Also print GUSBCFG post-connect for verification
  (%serial-byte 71)(%serial-byte 85)(%serial-byte 58)  ;; "GU:"
  (let ((gu (dwc2-read (dwc2-gusbcfg))))
    (print-hex-byte (logand gu #xFF))
    (print-hex-byte (logand (ash gu -8) #xFF))
    (print-hex-byte (logand (ash gu -16) #xFF))
    (print-hex-byte (logand (ash gu -24) #xFF)))
  (%serial-byte 10)
  ;; Wait for enumeration to complete (host configures us)
  (%serial-byte 69)(%serial-byte 78)(%serial-byte 85)(%serial-byte 77)  ;; "ENUM"
  (%serial-byte 10)
  ;; Poll loop — 100M iterations (~30 seconds)
  ;; Each iteration does 1+ MMIO reads via gadget-poll (~300ns each)
  ;; Host needs ~10-12s to enumerate (2 × 5s timeouts then success)
  (let ((i 0))
    (loop
      (when (not (zerop (gadget-configured))) (return 1))
      (when (>= i 100000000) (return 0))
      (gadget-poll)
      (setq i (+ i 1))
      ;; Watchdog marker every ~3 seconds (10M iterations)
      (when (zerop (logand i 16777215))
        (write-char-serial 46))))  ;; '.'
  (write-char-serial 10)
  ;; Always dump trace buffer
  (gadget-trace-dump)
  (%serial-byte 67)(%serial-byte 70)(%serial-byte 71)  ;; "CFG"
  (%serial-byte 10)
  ;; Continue polling USB after SET_CONFIGURATION.
  ;; The host's cdc_ether driver sends SET_INTERFACE and other requests
  ;; immediately after configuration. We must respond or it times out (-110).
  ;; Poll for ~2 seconds (500K iterations) to handle driver probe.
  (let ((j 0))
    (loop
      (when (>= j 500000) (return 0))
      (gadget-poll)
      (setq j (+ j 1))))
  (%serial-byte 80)(%serial-byte 49)(%serial-byte 10)  ;; "P1\n" = post-config done
  ;; Final EP2 re-arm — ensure it's properly armed after all probe activity
  (gadget-arm-ep2)
  (%serial-byte 80)(%serial-byte 82)(%serial-byte 66)(%serial-byte 10)  ;; "PRB\n" = probe done
  ;; Enable interrupt-driven USB: ISR drains RX FIFO to ring buffer,
  ;; allowing USB to stay responsive during long crypto operations.
  (gadget-enable-irq)
  1)
