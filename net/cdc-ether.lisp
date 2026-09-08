;;;; cdc-ether.lisp - USB CDC Ethernet driver
;;;;
;;;; Replaces e1000.lisp for USB-based networking (Raspberry Pi, QEMU usb-net).
;;;; Provides the same NIC interface expected by ip.lisp:
;;;;   e1000-send, e1000-receive, e1000-rx-buf, e1000-probe
;;;;
;;;; CDC Ethernet uses raw Ethernet frames over USB bulk endpoints:
;;;;   Bulk OUT = send Ethernet frame
;;;;   Bulk IN  = receive Ethernet frame
;;;; No extra framing headers (unlike RNDIS or LAN9514).
;;;;
;;;; Depends on: dwc2.lisp, usb.lisp, arch adapter (e1000-state-base, etc.)

;; ============================================================
;; Buffer layout
;; Reuses the same memory regions as E1000 (defined in arch adapter):
;;   e1000-rx-buf-base: receive buffer (single buffer, 2048 bytes)
;;   e1000-tx-buf-base: transmit buffer (single buffer, 1536 bytes)
;;   e1000-state-base:  network state (same layout as ip.lisp expects)
;; ============================================================

;; RX state: position and length of last received packet
;; stored in state-base+0x10 (rx-cur) and state-base+0x44 (rx-pkt-len)
(defun cdc-rx-buf-addr ()
  ;; Single RX buffer for USB bulk IN
  (e1000-rx-buf-base))

(defun cdc-tx-buf-addr ()
  ;; Single TX buffer for USB bulk OUT
  (e1000-tx-buf-base))

;; ============================================================
;; MAC address reading from USB CDC Ethernet descriptor
;; ============================================================

(defun cdc-hex-val (ch)
  ;; Parse hex ASCII char to 0-15
  (if (>= ch 97)
      (+ (- ch 97) 10)
      (if (>= ch 65)
          (+ (- ch 65) 10)
          (- ch 48))))

(defun cdc-find-mac-string-idx (buf total-len)
  ;; Walk config descriptor for CDC Ethernet Networking Functional Descriptor
  ;; (type=0x24 CS_INTERFACE, subtype=0x0F Ethernet Networking).
  ;; Returns iMACAddress string descriptor index, or 0 if not found.
  (let ((pos 0) (result 0))
    (loop
      (when (>= pos total-len) (return result))
      (let ((desc-len (mem-ref (+ buf pos) :u8)))
        (when (< desc-len 2) (return result))
        (let ((desc-type (mem-ref (+ buf (+ pos 1)) :u8)))
          (when (eq desc-type #x24)
            (when (>= desc-len 4)
              (let ((subtype (mem-ref (+ buf (+ pos 2)) :u8)))
                (when (eq subtype #x0F)
                  (setq result (mem-ref (+ buf (+ pos 3)) :u8))
                  (return result))))))
        (setq pos (+ pos desc-len))))))

(defun cdc-try-config-mac (devaddr dbuf cfg-idx)
  ;; Read config descriptor at cfg-idx, find CDC Ethernet MAC string index.
  ;; Returns string index > 0 if found, 0 otherwise.
  (loop
    (let ((r (usb-get-descriptor devaddr (usb-desc-configuration) cfg-idx dbuf 9)))
      (when (<= r 0) (return 0))
      (let ((total-len (usb-desc-u16 dbuf 2)))
        (when (> total-len 512) (setq total-len 512))
        (let ((r2 (usb-get-descriptor devaddr (usb-desc-configuration) cfg-idx dbuf total-len)))
          (when (<= r2 0) (return 0))
          (return (cdc-find-mac-string-idx dbuf total-len)))))))

(defun cdc-read-mac-string (devaddr dbuf str-idx state)
  ;; Read USB string descriptor str-idx, parse 12-char hex MAC into
  ;; state+0x08..0x0D. String descriptor: [bLen][bType=3][UTF-16LE chars...]
  ;; MAC string: "AABBCCDDEEFF" — 12 hex chars, 24 bytes UTF-16LE + 2 header.
  ;; Returns 1 on success, 0 on failure.
  (loop
    ;; GET_DESCRIPTOR string: wValue=(3<<8)|idx, wIndex=0x0409 (English)
    (let ((wvalue (logior (ash 3 8) str-idx)))
      (let ((r (usb-control-transfer devaddr #x80 (usb-req-get-descriptor)
                                     wvalue #x0409 dbuf 64)))
        (when (<= r 0) (return 0))
        ;; Verify string descriptor type
        (let ((btype (mem-ref (+ dbuf 1) :u8)))
          (when (not (eq btype 3)) (return 0))
          (let ((blen (mem-ref dbuf :u8)))
            ;; Need at least 26 bytes (2 header + 12 chars * 2 UTF-16LE)
            (when (< blen 26) (return 0))
            ;; Parse 6 MAC bytes from UTF-16LE hex chars
            ;; Char i*2 at offset 2+(i*4), char i*2+1 at offset 4+(i*4)
            (let ((i 0))
              (loop
                (when (>= i 6) (return nil))
                (let ((hi-off (+ 2 (ash i 2))))
                  (let ((lo-off (+ 4 (ash i 2))))
                    (let ((hi-ch (mem-ref (+ dbuf hi-off) :u8)))
                      (let ((lo-ch (mem-ref (+ dbuf lo-off) :u8)))
                        (let ((byte-val (logior (ash (cdc-hex-val hi-ch) 4)
                                                (cdc-hex-val lo-ch))))
                          (setf (mem-ref (+ state (+ #x08 i)) :u8) byte-val))))))
                (setq i (+ i 1))))
            (return 1)))))))

(defun cdc-read-mac (state)
  ;; Read MAC address from USB device's CDC Ethernet descriptor.
  ;; Stores MAC at state+0x08..0x0D.
  ;; Returns 1 if MAC read from device, 0 if not found.
  (let ((dbuf (usb-data-buf))
        (devaddr (usb-dev-addr)))
    (loop
      ;; Try config descriptor index 0
      (let ((idx0 (cdc-try-config-mac devaddr dbuf 0)))
        (when (> idx0 0)
          (return (cdc-read-mac-string devaddr dbuf idx0 state)))
        ;; Try config descriptor index 1
        (let ((idx1 (cdc-try-config-mac devaddr dbuf 1)))
          (when (> idx1 0)
            (return (cdc-read-mac-string devaddr dbuf idx1 state)))
          (return 0))))))

;; ============================================================
;; CDC Ethernet initialization
;; Uses loop + return for early exit (MVM doesn't support return-from)
;; ============================================================

(defun cdc-ether-init ()
  ;; Initialize DWC2 + USB + CDC Ethernet.
  ;; Returns 1 on success, 0 on failure.
  (loop
    ;; 1. Initialize DWC2 host controller
    (let ((r1 (dwc2-init)))
      (when (zerop r1)
        (%serial-byte 67) (%serial-byte 68) (%serial-byte 67)  ; "CDC"
        (%serial-byte 58) (%serial-byte 69) (%serial-byte 49)  ; ":E1"
        (%serial-byte 10)
        (return 0))

      ;; 2. Enumerate USB device
      (let ((r2 (usb-enumerate)))
        (when (zerop r2)
          (%serial-byte 67) (%serial-byte 68) (%serial-byte 67)  ; "CDC"
          (%serial-byte 58) (%serial-byte 69) (%serial-byte 50)  ; ":E2"
          (%serial-byte 10)
          (return 0))

        ;; 3. Initialize network state
        (let ((state (e1000-state-base)))
          ;; Read MAC from USB device descriptor
          (let ((mac-ok (cdc-read-mac state)))
            (when (zerop mac-ok)
              ;; Fallback: store zeros (will show MAC:00:00:00:00:00:00)
              (let ((j 0))
                (loop
                  (when (>= j 6) (return nil))
                  (setf (mem-ref (+ state (+ #x08 j)) :u8) 0)
                  (setq j (+ j 1))))))

          ;; Initialize cursors
          (setf (mem-ref (+ state #x10) :u32) 0)  ; RX cursor
          (setf (mem-ref (+ state #x14) :u32) 0)  ; TX cursor
          (setf (mem-ref (+ state #x44) :u32) 0)  ; RX packet length

          ;; Default IP (will be overwritten by DHCP)
          (setf (mem-ref (+ state #x18) :u32) #x0F02000A)  ; 10.0.2.15
          (setf (mem-ref (+ state #x1C) :u32) #x0202000A)  ; 10.0.2.2

          ;; Print MAC (read from state, not hardcoded)
          (%serial-byte 77) (%serial-byte 65) (%serial-byte 67) (%serial-byte 58)  ; "MAC:"
          (print-hex-byte (mem-ref (+ state #x08) :u8)) (%serial-byte 58)
          (print-hex-byte (mem-ref (+ state #x09) :u8)) (%serial-byte 58)
          (print-hex-byte (mem-ref (+ state #x0A) :u8)) (%serial-byte 58)
          (print-hex-byte (mem-ref (+ state #x0B) :u8)) (%serial-byte 58)
          (print-hex-byte (mem-ref (+ state #x0C) :u8)) (%serial-byte 58)
          (print-hex-byte (mem-ref (+ state #x0D) :u8)) (%serial-byte 10)

          ;; Print status
          (%serial-byte 67) (%serial-byte 68) (%serial-byte 67)  ; "CDC"
          (%serial-byte 58) (%serial-byte 79) (%serial-byte 75)  ; ":OK"
          (%serial-byte 10)

          ;; Start the first bulk IN transfer (persistent channel).
          ;; DWC2's work_bh will auto-retry NAK'd transfers.
          ;; Subsequent calls to usb-bulk-receive just poll for completion.
          (dwc2-start-bulk-in 1 (usb-dev-addr) (usb-bulk-in-ep)
                              (cdc-rx-buf-addr) 2048 (usb-bulk-in-mps))
          (return 1))))))

;; ============================================================
;; NIC interface (same signatures as e1000.lisp)
;; ============================================================

;; Send raw Ethernet frame from byte array.
;; buf: Lisp byte array, len: frame length in bytes.
;; Returns 1 on success, 0 on failure.
(defun cdcether-send (buf len)
  (let ((tx-buf (cdc-tx-buf-addr)))
    ;; Copy from Lisp array to DMA buffer
    (let ((i 0))
      (loop
        (when (>= i len) (return nil))
        (setf (mem-ref (+ tx-buf i) :u8) (aref buf i))
        (setq i (+ i 1))))
    ;; USB bulk OUT
    (let ((result (usb-bulk-send tx-buf len)))
      (if (eq result 1) 1 0))))

;; Check for received packet. Returns length or 0 if none.
(defun cdcether-receive ()
  ;; Read HCTSIZ BEFORE polling — the poll may start a new transfer,
  ;; overwriting the register. We need the remaining bytes from the
  ;; completed transfer to compute the actual received length.
  (let ((hctsiz-before (dwc2-read (dwc2-hctsiz 1))))
    (let ((result (usb-bulk-receive (cdc-rx-buf-addr) 2048)))
      (if (eq result 1)
          ;; Data received. Use saved HCTSIZ to compute actual length.
          (let ((remaining (logand hctsiz-before #x7FFFF)))
            (let ((actual (- 2048 remaining)))
              (setf (mem-ref (+ (e1000-state-base) #x44) :u32) actual)
              actual))
          ;; No data or error
          0))))

;; Get pointer to current RX buffer data (physical address).
;; Called after e1000-receive returns non-zero.
(defun cdcether-rx-buf ()
  (cdc-rx-buf-addr))

;; Find NIC and initialize. On RPi this initializes USB + CDC Ethernet.
(defun cdcether-probe ()
  (cdc-ether-init))

;; Thin e1000-* forwarders: images WITHOUT net/usb-netdev.lisp keep today's
;; behaviour (cdc-ether IS the NIC).  Where usb-netdev.lisp is loaded (LAST),
;; its dispatchers win last-defun-wins and these are shadowed.
(defun e1000-send (buf len) (cdcether-send buf len))
(defun e1000-receive () (cdcether-receive))
(defun e1000-rx-buf () (cdcether-rx-buf))
(defun e1000-probe () (cdcether-probe))

;; ============================================================
;; E1000 register stubs (called by ip.lisp init functions)
;; These are no-ops since we don't have E1000 hardware.
;; ============================================================

(defun e1000-write-reg (reg val) nil)
(defun e1000-read-reg (reg) 0)
