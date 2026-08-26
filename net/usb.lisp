;;;; usb.lisp - USB Core: Enumeration, Control Transfers, Descriptor Parsing
;;;;
;;;; USB protocol layer using DWC2 host channels.
;;;; Channel 0: control transfers
;;;; Channel 1: bulk IN
;;;; Channel 2: bulk OUT
;;;;
;;;; Depends on: dwc2.lisp (host controller driver)

;; ============================================================
;; USB request types and descriptor types
;; ============================================================

;; bmRequestType: direction | type | recipient
;; D7: 0=host-to-device, 1=device-to-host
;; D6:5: 0=standard, 1=class, 2=vendor
;; D4:0: 0=device, 1=interface, 2=endpoint

;; Standard bRequest values
(defun usb-req-get-status ()       0)
(defun usb-req-clear-feature ()    1)
(defun usb-req-set-feature ()      3)
(defun usb-req-set-address ()      5)
(defun usb-req-get-descriptor ()   6)
(defun usb-req-set-descriptor ()   7)
(defun usb-req-get-configuration () 8)
(defun usb-req-set-configuration () 9)

;; Descriptor types
(defun usb-desc-device ()         1)
(defun usb-desc-configuration ()  2)
(defun usb-desc-string ()         3)
(defun usb-desc-interface ()      4)
(defun usb-desc-endpoint ()       5)

;; Endpoint direction (from bEndpointAddress bit 7)
(defun usb-ep-dir-out ()  0)
(defun usb-ep-dir-in ()   #x80)

;; Endpoint transfer type (from bmAttributes bits 1:0)
(defun usb-ep-type-control ()     0)
(defun usb-ep-type-isochronous () 1)
(defun usb-ep-type-bulk ()        2)
(defun usb-ep-type-interrupt ()   3)

;; ============================================================
;; SETUP packet builder
;; Writes 8 bytes to the setup buffer at usb-setup-buf
;; ============================================================

(defun usb-build-setup (request-type request value index length)
  ;; Write 8-byte SETUP packet to DMA buffer
  (let ((buf (usb-setup-buf)))
    (setf (mem-ref buf :u8) request-type)
    (setf (mem-ref (+ buf 1) :u8) request)
    ;; wValue (little-endian u16)
    (setf (mem-ref (+ buf 2) :u8) (logand value #xFF))
    (setf (mem-ref (+ buf 3) :u8) (logand (ash value -8) #xFF))
    ;; wIndex (little-endian u16)
    (setf (mem-ref (+ buf 4) :u8) (logand index #xFF))
    (setf (mem-ref (+ buf 5) :u8) (logand (ash index -8) #xFF))
    ;; wLength (little-endian u16)
    (setf (mem-ref (+ buf 6) :u8) (logand length #xFF))
    (setf (mem-ref (+ buf 7) :u8) (logand (ash length -8) #xFF))))

;; ============================================================
;; Control transfer (complete 3-stage: SETUP -> DATA -> STATUS)
;; ============================================================

(defun usb-control-transfer (devaddr request-type request value index buf len)
  ;; Execute a complete control transfer.
  ;; request-type bit 7: 0=OUT (host->device), 1=IN (device->host)
  ;; buf: DMA buffer for data stage (physical address). Ignored if len=0.
  ;; Returns: 1=success, <=0=error
  ;;
  ;; #275 history: this function once flushed the TX+RX FIFOs per transfer to
  ;; fight a "stale SETUP replay" on real silicon.  The true root cause was
  ;; the ARM DATA CACHE inherited from U-Boot's `go' (MMU+dcache left ON):
  ;; CPU stores to this buffer sat dirty in cache while the DWC2 DMA-fetched
  ;; stale DRAM, so the wire carried the previous request.  Fixed at the root
  ;; by the boot preamble's cache/MMU sanitize (boot/boot-rpi-cl.lisp step
  ;; -1).  No per-transfer FIFO flush is needed (U-Boot and Linux flush only
  ;; at core init), and none ever fixed it.
  (let ((ch 0))
    ;; Stage 1: SETUP
    (usb-build-setup request-type request value index len)
    (let ((r1 (dwc2-control-setup ch devaddr (usb-setup-buf))))
      (if (<= r1 0)
          r1
          ;; Stage 2: DATA (if len > 0)
          (if (> len 0)
              (let ((is-in (not (zerop (logand request-type #x80)))))
                (let ((r2 (if is-in
                               (dwc2-control-data-in ch devaddr buf len)
                               (dwc2-control-data-out ch devaddr buf len))))
                  (if (<= r2 0)
                      r2
                      ;; Stage 3: STATUS (opposite direction of data)
                      (if is-in
                          (dwc2-control-status-out ch devaddr)
                          (dwc2-control-status-in ch devaddr)))))
              ;; No data stage -- STATUS only (IN direction for no-data requests)
              (dwc2-control-status-in ch devaddr))))))

;; ============================================================
;; Standard USB requests
;; ============================================================

(defun usb-get-descriptor (devaddr desc-type desc-index buf len)
  ;; GET_DESCRIPTOR: bmRequestType=0x80 (IN, standard, device)
  ;; wValue = (desc-type << 8) | desc-index
  (let ((wvalue (logior (ash desc-type 8) desc-index)))
    (usb-control-transfer devaddr #x80 (usb-req-get-descriptor)
                          wvalue 0 buf len)))

(defun usb-set-address (new-addr)
  ;; SET_ADDRESS: bmRequestType=0x00 (OUT, standard, device)
  ;; wValue = new address (1-127)
  ;; No data stage
  (usb-control-transfer 0 #x00 (usb-req-set-address)
                        new-addr 0 0 0))

(defun usb-set-configuration (devaddr config)
  ;; SET_CONFIGURATION: bmRequestType=0x00 (OUT, standard, device)
  ;; wValue = configuration value (usually 1)
  (usb-control-transfer devaddr #x00 (usb-req-set-configuration)
                        config 0 0 0))

(defun usb-set-interface (devaddr iface alt-setting)
  ;; SET_INTERFACE: bmRequestType=0x01 (OUT, standard, interface)
  ;; bRequest=11, wValue=alternate setting, wIndex=interface number
  (usb-control-transfer devaddr #x01 11
                        alt-setting iface 0 0))

;; ============================================================
;; Descriptor parsing helpers
;; ============================================================

(defun usb-desc-byte (buf off)
  ;; Read byte at offset from DMA buffer
  (mem-ref (+ buf off) :u8))

(defun usb-desc-u16 (buf off)
  ;; Read little-endian u16 from DMA buffer
  (logior (mem-ref (+ buf off) :u8)
          (ash (mem-ref (+ buf (+ off 1)) :u8) 8)))

(defun usb-parse-config-find-bulk-eps (buf total-len)
  ;; Walk through configuration descriptor, find bulk IN and bulk OUT endpoints.
  ;; Stores results in USB state via usb-set-bulk-in-ep, etc.
  ;; Returns 1 if both found, 0 otherwise.
  (let ((found-in 0) (found-out 0))
    (let ((pos 9))  ; skip config descriptor header
      (loop
        (when (>= pos total-len) (return nil))
        (let ((desc-len (usb-desc-byte buf pos))
              (desc-type (usb-desc-byte buf (+ pos 1))))
          ;; Sanity: desc-len must be >= 2 to avoid infinite loop
          (when (< desc-len 2) (return nil))
          ;; Check for endpoint descriptor
          (when (eq desc-type 5)
            (let ((ep-addr (usb-desc-byte buf (+ pos 2)))
                  (ep-attr (usb-desc-byte buf (+ pos 3)))
                  (ep-mps (usb-desc-u16 buf (+ pos 4))))
              ;; Check if bulk type (bits 1:0 = 2)
              (when (eq (logand ep-attr 3) 2)
                ;; Check direction (bit 7 of bEndpointAddress)
                (if (not (zerop (logand ep-addr #x80)))
                    ;; Bulk IN
                    (progn
                      (usb-set-bulk-in-ep (logand ep-addr #xF))
                      (usb-set-bulk-in-mps ep-mps)
                      (setq found-in 1))
                    ;; Bulk OUT
                    (progn
                      (usb-set-bulk-out-ep (logand ep-addr #xF))
                      (usb-set-bulk-out-mps ep-mps)
                      (setq found-out 1))))))
          (setq pos (+ pos desc-len)))))
    ;; Return 1 if both endpoints found
    (if (eq (+ found-in found-out) 2) 1 0)))

;; ============================================================
;; Full enumeration sequence
;; Uses a single-iteration loop for early exit via (return 0).
;; MVM compiler supports (return val) inside loops but not return-from.
;; ============================================================

(defun usb-enumerate ()
  (let ((dbuf (usb-data-buf)))
    (loop
      ;; 1. Reset port
      (let ((speed (dwc2-port-reset)))
        (when (< speed 0)
          (write-byte 85) (write-byte 83) (write-byte 66)
          (write-byte 58) (write-byte 69) (write-byte 10)
          (return 0))

        ;; 2. Short delay after reset
        (dwc2-delay-ms 20)

        ;; 3. GET_DEVICE_DESCRIPTOR at address 0, first 8 bytes
        (let ((r1 (usb-get-descriptor 0 (usb-desc-device) 0 dbuf 8)))
          (when (<= r1 0)
            (write-byte 68) (write-byte 49) (write-byte 69) (write-byte 10)
            (return 0))

          ;; Read bMaxPacketSize0 (byte 7)
          (let ((max-pkt (usb-desc-byte dbuf 7)))
            (when (zerop max-pkt) (setq max-pkt 64))

            ;; 4. Reset port again before SET_ADDRESS
            (dwc2-port-reset)
            (dwc2-delay-ms 20)

            ;; 5. SET_ADDRESS to address 1
            (let ((r2 (usb-set-address 1)))
              (when (<= r2 0)
                (write-byte 65) (write-byte 49) (write-byte 69) (write-byte 10)
                (return 0))

              ;; Wait for address to take effect
              (dwc2-delay-ms 10)
              (usb-set-dev-addr 1)

              ;; 6. GET_DEVICE_DESCRIPTOR at new address (full 18 bytes)
              (let ((r3 (usb-get-descriptor 1 (usb-desc-device) 0 dbuf 18)))
                (when (<= r3 0)
                  (write-byte 68) (write-byte 50) (write-byte 69) (write-byte 10)
                  (return 0))

                ;; Print vendor:product
                (write-byte 85) (write-byte 83) (write-byte 66) (write-byte 58)
                (print-hex-byte (usb-desc-byte dbuf 9))
                (print-hex-byte (usb-desc-byte dbuf 8))
                (write-byte 58)
                (print-hex-byte (usb-desc-byte dbuf 11))
                (print-hex-byte (usb-desc-byte dbuf 10))
                (write-byte 10)

                ;; Save device class for hub detection
                (usb-set-device-class (usb-desc-byte dbuf 4))

                ;; 7. GET_CONFIGURATION_DESCRIPTOR (first 9 bytes)
                (let ((r4 (usb-get-descriptor 1 (usb-desc-configuration) 0 dbuf 9)))
                  (when (<= r4 0)
                    (write-byte 67) (write-byte 49) (write-byte 69) (write-byte 10)
                    (return 0))

                  ;; Read wTotalLength
                  (let ((total-len (usb-desc-u16 dbuf 2)))
                    (when (> total-len 512)
                      (setq total-len 512))

                    ;; Get full configuration descriptor
                    (let ((r5 (usb-get-descriptor 1 (usb-desc-configuration) 0 dbuf total-len)))
                      (when (<= r5 0)
                        (write-byte 67) (write-byte 50) (write-byte 69) (write-byte 10)
                        (return 0))

                      ;; 8. Parse endpoints
                      (let ((found (usb-parse-config-find-bulk-eps dbuf total-len)))

                        ;; 9. SET_CONFIGURATION
                        (let ((config-val (usb-desc-byte dbuf 5)))
                          (let ((r6 (usb-set-configuration 1 config-val)))
                            (when (<= r6 0)
                              (write-byte 83) (write-byte 67) (write-byte 69) (write-byte 10)
                              (return 0))

                            ;; Initialize data toggles
                            (usb-set-bulk-in-toggle 0)
                            (usb-set-bulk-out-toggle 0)

                            ;; Print result
                            (write-byte 85) (write-byte 83) (write-byte 66) (write-byte 58)
                            (if (eq found 1)
                                (progn
                                  (write-byte 79) (write-byte 75)
                                  (write-byte 10)
                                  (return 1))
                                ;; No bulk endpoints -- check for hub
                                (if (eq (usb-device-class) 9)
                                    (progn
                                      (write-byte 72) (write-byte 85) (write-byte 66) (write-byte 10)
                                      (let ((hr (usb-hub-enumerate-downstream 1 dbuf)))
                                        (return hr)))
                                    (progn
                                      (write-byte 78) (write-byte 69) (write-byte 10)
                                      (return 0))))))))))))))))))

;; ============================================================
;; Hub enumeration
;; Enumerates a device behind a USB hub.
;; QEMU creates a virtual hub (VID 0409:55AA) between DWC2
;; root port and attached devices like usb-net.
;; ============================================================

;; Which config-descriptor INDEX the hub path selects for the downstream
;; device.  QEMU usb-net: index 0 = RNDIS, index 1 = CDC-ECM -> default 1.
;; net/r8152.lisp overrides this to 0 (last-defun-wins) so real RTL8153
;; hardware enumerates STRAIGHT into vendor config 1 — switching configs
;; after enumeration makes the NIC drop off the hub port.
(defun usb-hub-config-index () 1)

(defun usb-hub-enumerate-downstream (hub-addr dbuf)
  ;; Hub at hub-addr is already configured.
  ;; Find and enumerate the downstream CDC device.
  ;; Returns 1 if bulk endpoints found, 0 otherwise.
  (loop
    ;; 1. Get hub descriptor (type 0x29)
    ;; bmRequestType=0xA0 (class, device, IN), wValue=0x2900
    (let ((r1 (usb-control-transfer hub-addr #xA0 6 #x2900 0 dbuf 8)))
      (when (<= r1 0)
        (write-byte 72) (write-byte 68) (write-byte 10)
        (return 0))
      (let ((num-ports (usb-desc-byte dbuf 2)))
        ;; 2. Power all ports: SET_FEATURE(PORT_POWER=8)
        ;; bmRequestType=0x23 (class, other, OUT), bRequest=SET_FEATURE(3)
        (let ((p 1))
          (loop
            (when (> p num-ports) (return nil))
            (usb-control-transfer hub-addr #x23 3 8 p 0 0)
            (setq p (+ p 1))))
        ;; Wait for power + device connection
        (dwc2-delay-ms 150)
        ;; 3. Find first connected port: GET_STATUS
        ;; bmRequestType=0xA3 (class, other, IN), bRequest=GET_STATUS(0)
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
            (write-byte 72) (write-byte 78) (write-byte 67) (write-byte 10)
            (return 0))
          ;; 4. Reset port: SET_FEATURE(PORT_RESET=4)
          (usb-control-transfer hub-addr #x23 3 4 conn-port 0 0)
          (dwc2-delay-ms 60)
          ;; Clear reset change: CLEAR_FEATURE(C_PORT_RESET=20)
          (usb-control-transfer hub-addr #x23 1 20 conn-port 0 0)
          (dwc2-delay-ms 10)
          ;; 5. Enumerate downstream device (starts at address 0)
          (let ((r3 (usb-get-descriptor 0 (usb-desc-device) 0 dbuf 8)))
            (when (<= r3 0)
              (write-byte 72) (write-byte 68) (write-byte 49) (write-byte 10)
              (return 0))
            ;; SET_ADDRESS to 2 (hub is at 1)
            (let ((r4 (usb-set-address 2)))
              (when (<= r4 0)
                (write-byte 72) (write-byte 65) (write-byte 10)
                (return 0))
              (dwc2-delay-ms 10)
              ;; Full device descriptor at address 2
              (let ((r5 (usb-get-descriptor 2 (usb-desc-device) 0 dbuf 18)))
                (when (<= r5 0)
                  (write-byte 72) (write-byte 68) (write-byte 50) (write-byte 10)
                  (return 0))
                ;; Print downstream VID:PID
                (write-byte 68) (write-byte 69) (write-byte 86) (write-byte 58)
                (print-hex-byte (usb-desc-byte dbuf 9))
                (print-hex-byte (usb-desc-byte dbuf 8))
                (write-byte 58)
                (print-hex-byte (usb-desc-byte dbuf 11))
                (print-hex-byte (usb-desc-byte dbuf 10))
                (write-byte 10)
                ;; Get the selected config descriptor (see usb-hub-config-index:
                ;; QEMU usb-net wants index 1 = CDC-ECM; real RTL8153 wants
                ;; index 0 = vendor config).
                (let ((r6 (usb-get-descriptor 2 (usb-desc-configuration)
                                              (usb-hub-config-index) dbuf 9)))
                  (when (<= r6 0)
                    (write-byte 72) (write-byte 67) (write-byte 49) (write-byte 10)
                    (return 0))
                  (let ((total-len (usb-desc-u16 dbuf 2)))
                    (when (> total-len 512) (setq total-len 512))
                    ;; Full config descriptor
                    (let ((r7 (usb-get-descriptor 2 (usb-desc-configuration)
                                                  (usb-hub-config-index) dbuf total-len)))
                      (when (<= r7 0)
                        (write-byte 72) (write-byte 67) (write-byte 50) (write-byte 10)
                        (return 0))
                      ;; Parse bulk endpoints
                      (let ((found (usb-parse-config-find-bulk-eps dbuf total-len)))
                        ;; SET_CONFIGURATION
                        (let ((config-val (usb-desc-byte dbuf 5)))
                          (let ((r8 (usb-set-configuration 2 config-val)))
                            (when (<= r8 0)
                              (write-byte 72) (write-byte 83) (write-byte 69) (write-byte 10)
                              (return 0))
                            ;; Try SET_INTERFACE (best-effort; QEMU usb-net may STALL)
                            (usb-set-interface 2 1 1)
                            (usb-set-bulk-in-toggle 0)
                            (usb-set-bulk-out-toggle 0)
                            (usb-set-dev-addr 2)
                            (if (eq found 1)
                                (progn
                                  (write-byte 72) (write-byte 58) (write-byte 79) (write-byte 75) (write-byte 10)
                                  (return 1))
                                (progn
                                  (write-byte 72) (write-byte 58) (write-byte 78) (write-byte 69) (write-byte 10)
                                  (return 0)))))))))))))))))

;; ============================================================
;; Bulk transfer wrappers
;; ============================================================

(defun usb-bulk-send (buf len)
  ;; Send data via bulk OUT endpoint.
  ;; buf: physical address of data buffer
  ;; len: bytes to send
  ;; Returns: 1=success, 0=NAK, <0=error
  (dwc2-bulk-transfer 2 (usb-dev-addr) (usb-bulk-out-ep)
                      0 buf len (usb-bulk-out-mps)))

(defun usb-bulk-receive (buf max-len)
  ;; Non-blocking bulk IN using persistent channel.
  ;; The first call starts the transfer; subsequent calls just poll.
  ;; DWC2's work_bh auto-retries NAK'd transfers in the background.
  ;; Returns: 1=success (data in buf), 0=no data yet, <0=error
  (let ((result (dwc2-poll-bulk-in 1)))
    ;; If no data yet (0), the channel is still active — return immediately
    (when (not (zerop result))
      ;; Transfer completed (success, stall, or error).
      ;; Channel is now idle (CHENA cleared by DWC2). Start a new one.
      (dwc2-start-bulk-in 1 (usb-dev-addr) (usb-bulk-in-ep)
                           buf max-len (usb-bulk-in-mps)))
    result))
