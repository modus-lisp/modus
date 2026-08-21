;;;; dwc2.lisp - Synopsys DesignWare USB 2.0 OTG Host Controller Driver
;;;;
;;;; Bare-metal DWC2 host controller driver for Raspberry Pi 3B (BCM2837).
;;;; DWC2 base address defined in arch adapter: (dwc2-base) = 0x3F980000
;;;; Uses DMA mode (required by QEMU's DWC2 emulation).
;;;; Polling-based, single-threaded, no interrupts.

;; ============================================================
;; Register offsets
;; ============================================================

;; Core global registers
(defun dwc2-gotgctl ()   (+ (dwc2-base) #x000))
(defun dwc2-gotgint ()   (+ (dwc2-base) #x004))
(defun dwc2-gahbcfg ()   (+ (dwc2-base) #x008))
(defun dwc2-gusbcfg ()   (+ (dwc2-base) #x00C))
(defun dwc2-grstctl ()   (+ (dwc2-base) #x010))
(defun dwc2-gintsts ()   (+ (dwc2-base) #x014))
(defun dwc2-gintmsk ()   (+ (dwc2-base) #x018))
(defun dwc2-grxstsr ()   (+ (dwc2-base) #x01C))
(defun dwc2-grxstsp ()   (+ (dwc2-base) #x020))
(defun dwc2-grxfsiz ()   (+ (dwc2-base) #x024))
(defun dwc2-gnptxfsiz () (+ (dwc2-base) #x028))
(defun dwc2-gnptxsts ()  (+ (dwc2-base) #x02C))

;; Host-mode registers
(defun dwc2-hcfg ()      (+ (dwc2-base) #x400))
(defun dwc2-hfir ()      (+ (dwc2-base) #x404))
(defun dwc2-hfnum ()     (+ (dwc2-base) #x408))
(defun dwc2-hptxsts ()   (+ (dwc2-base) #x410))
(defun dwc2-haint ()     (+ (dwc2-base) #x414))
(defun dwc2-haintmsk ()  (+ (dwc2-base) #x418))
(defun dwc2-hprt0 ()     (+ (dwc2-base) #x440))

;; Host periodic TX FIFO size
(defun dwc2-hptxfsiz ()  (+ (dwc2-base) #x100))

;; Per-channel register accessors (16 channels, 0x20 apart starting at 0x500)
(defun dwc2-hcchar (ch)   (+ (dwc2-base) #x500 (* ch #x20)))
(defun dwc2-hcsplt (ch)   (+ (dwc2-base) #x504 (* ch #x20)))
(defun dwc2-hcint (ch)    (+ (dwc2-base) #x508 (* ch #x20)))
(defun dwc2-hcintmsk (ch) (+ (dwc2-base) #x50C (* ch #x20)))
(defun dwc2-hctsiz (ch)   (+ (dwc2-base) #x510 (* ch #x20)))
(defun dwc2-hcdma (ch)    (+ (dwc2-base) #x514 (* ch #x20)))

;; ============================================================
;; Register read/write helpers
;; ============================================================

(defun dwc2-read (addr)
  (mem-ref addr :u32))

(defun dwc2-write (addr val)
  (setf (mem-ref addr :u32) val))

;; ============================================================
;; HPRT0 W1C-safe write
;; Bits 1,2,3,5 are write-1-to-clear — must write 0 to preserve
;; ============================================================

(defun dwc2-hprt0-mask ()
  ;; Mask to clear W1C bits: ~(bit1 | bit2 | bit3 | bit5) = ~0x2E = 0xFFFFFFD1
  ;; We AND the read value with this before ORing in new bits
  #xFFFFFFD1)

(defun dwc2-hprt0-read-safe ()
  ;; Read HPRT0 with W1C bits zeroed so writing back doesn't clear them
  (logand (dwc2-read (dwc2-hprt0)) (dwc2-hprt0-mask)))

;; ============================================================
;; Bit constants
;; ============================================================

;; GAHBCFG bits
(defun gahbcfg-glbl-intr-en () #x01)
(defun gahbcfg-dma-en ()       #x20)
(defun gahbcfg-hbstlen-incr4 () #x06)  ; bits [4:1] = 3 (INCR4)

;; GUSBCFG bits
(defun gusbcfg-force-host ()   (ash 1 29))

;; GRSTCTL bits
(defun grstctl-csftrst ()      #x01)
(defun grstctl-ahbidl ()       (ash 1 31))
(defun grstctl-rxfflsh ()      (ash 1 4))
(defun grstctl-txfflsh ()      (ash 1 5))
(defun grstctl-txfnum-all ()   (ash #x10 6))  ; flush all TX FIFOs

;; HPRT0 bits
(defun hprt0-prtconnsts ()     #x01)      ; bit 0: connected
(defun hprt0-prtconndet ()     #x02)      ; bit 1: connect detected (W1C)
(defun hprt0-prtena ()         #x04)      ; bit 2: port enabled (W1C)
(defun hprt0-prtenchng ()      #x08)      ; bit 3: enable changed (W1C)
(defun hprt0-prtrst ()         (ash 1 8)) ; bit 8: port reset
(defun hprt0-prtpwr ()         (ash 1 12)); bit 12: port power

;; HCCHAR bits
(defun hcchar-chena ()         (ash 1 31))
(defun hcchar-chdis ()         (ash 1 30))

;; HCINT bits
(defun hcint-xfercompl ()      #x01)  ; bit 0
(defun hcint-chhltd ()         #x02)  ; bit 1
(defun hcint-ahberr ()         #x04)  ; bit 2
(defun hcint-stall ()          #x08)  ; bit 3
(defun hcint-nak ()            #x10)  ; bit 4
(defun hcint-ack ()            #x20)  ; bit 5
(defun hcint-xacterr ()        #x80)  ; bit 7
(defun hcint-bblerr ()         (ash 1 8))  ; bit 8
(defun hcint-datatglerr ()     (ash 1 10)) ; bit 10

;; HCTSIZ PID values (bits 30:29)
(defun hctsiz-pid-data0 ()     0)                ; 0 << 29
(defun hctsiz-pid-data1 ()     (ash 2 29))       ; 2 << 29
(defun hctsiz-pid-setup ()     (ash 3 29))       ; 3 << 29

;; ============================================================
;; USB state stored at fixed memory addresses
;; usb-dma-base+0x000: control transfer setup buffer (8 bytes)
;; usb-dma-base+0x040: control transfer data buffer (512 bytes)
;; usb-dma-base+0x800: USB state
;;   +0x00: port speed (u32: 0=HS, 1=FS, 2=LS)
;;   +0x04: device address assigned (u32)
;;   +0x08: bulk-in endpoint number (u32)
;;   +0x0C: bulk-out endpoint number (u32)
;;   +0x10: bulk-in MPS (u32)
;;   +0x14: bulk-out MPS (u32)
;;   +0x18: bulk-in data toggle (u32: 0=DATA0, 1=DATA1)
;;   +0x1C: bulk-out data toggle (u32: 0=DATA0, 1=DATA1)
;; ============================================================

(defun usb-setup-buf () (usb-dma-base))
(defun usb-data-buf () (+ (usb-dma-base) #x40))
(defun usb-state-addr () (+ (usb-dma-base) #x800))

(defun usb-port-speed () (mem-ref (usb-state-addr) :u32))
(defun usb-set-port-speed (v) (setf (mem-ref (usb-state-addr) :u32) v))

(defun usb-dev-addr () (mem-ref (+ (usb-state-addr) #x04) :u32))
(defun usb-set-dev-addr (v) (setf (mem-ref (+ (usb-state-addr) #x04) :u32) v))

(defun usb-bulk-in-ep () (mem-ref (+ (usb-state-addr) #x08) :u32))
(defun usb-set-bulk-in-ep (v) (setf (mem-ref (+ (usb-state-addr) #x08) :u32) v))

(defun usb-bulk-out-ep () (mem-ref (+ (usb-state-addr) #x0C) :u32))
(defun usb-set-bulk-out-ep (v) (setf (mem-ref (+ (usb-state-addr) #x0C) :u32) v))

(defun usb-bulk-in-mps () (mem-ref (+ (usb-state-addr) #x10) :u32))
(defun usb-set-bulk-in-mps (v) (setf (mem-ref (+ (usb-state-addr) #x10) :u32) v))

(defun usb-bulk-out-mps () (mem-ref (+ (usb-state-addr) #x14) :u32))
(defun usb-set-bulk-out-mps (v) (setf (mem-ref (+ (usb-state-addr) #x14) :u32) v))

(defun usb-bulk-in-toggle () (mem-ref (+ (usb-state-addr) #x18) :u32))
(defun usb-set-bulk-in-toggle (v) (setf (mem-ref (+ (usb-state-addr) #x18) :u32) v))

(defun usb-bulk-out-toggle () (mem-ref (+ (usb-state-addr) #x1C) :u32))
(defun usb-set-bulk-out-toggle (v) (setf (mem-ref (+ (usb-state-addr) #x1C) :u32) v))

(defun usb-device-class () (mem-ref (+ (usb-state-addr) #x20) :u32))
(defun usb-set-device-class (v) (setf (mem-ref (+ (usb-state-addr) #x20) :u32) v))

;; ============================================================
;; Delay helper (millisecond-ish delays via io-delay loops)
;; Each io-delay is ~5000 UART reads. Approximate 1ms per call
;; on QEMU. For real hardware, calibrate against system timer.
;; ============================================================

(defun dwc2-delay-ms (ms)
  (let ((i 0))
    (loop
      (when (>= i ms) (return nil))
      (io-delay)
      (setq i (+ i 1)))))

;; ============================================================
;; Core initialization
;; ============================================================

(defun dwc2-wait-ahb-idle ()
  ;; Poll GRSTCTL bit 31 (AHBIDL) until set
  (let ((i 0))
    (loop
      (when (>= i 100000) (return 0))
      (let ((val (dwc2-read (dwc2-grstctl))))
        (when (not (zerop (logand val (grstctl-ahbidl))))
          (return 1)))
      (setq i (+ i 1)))))

(defun dwc2-core-reset ()
  ;; 1. Wait for AHB idle
  (dwc2-wait-ahb-idle)
  ;; 2. Trigger core soft reset
  (dwc2-write (dwc2-grstctl) (grstctl-csftrst))
  ;; 3. Wait for reset to complete (bit 0 clears)
  (let ((i 0))
    (loop
      (when (>= i 100000) (return 0))
      (let ((val (dwc2-read (dwc2-grstctl))))
        (when (zerop (logand val (grstctl-csftrst)))
          (return 1)))
      (setq i (+ i 1))))
  ;; 4. Wait for PHY stabilization
  (dwc2-delay-ms 100))

(defun dwc2-flush-tx-fifo ()
  ;; Flush all TX FIFOs
  (dwc2-write (dwc2-grstctl)
              (logior (grstctl-txfflsh) (grstctl-txfnum-all)))
  (let ((i 0))
    (loop
      (when (>= i 10000) (return nil))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (grstctl-txfflsh)))
        (return nil))
      (setq i (+ i 1)))))

(defun dwc2-flush-rx-fifo ()
  ;; Flush RX FIFO
  (dwc2-write (dwc2-grstctl) (grstctl-rxfflsh))
  (let ((i 0))
    (loop
      (when (>= i 10000) (return nil))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (grstctl-rxfflsh)))
        (return nil))
      (setq i (+ i 1)))))

(defun dwc2-init ()
  ;; Initialize DWC2 in host mode
  ;; Returns 1 on success, 0 on failure

  ;; 1. Force host mode
  (let ((cfg (dwc2-read (dwc2-gusbcfg))))
    ;; Clear force-device (bit 30), set force-host (bit 29)
    (let ((cleared (logand cfg (logxor (ash 1 30) #xFFFFFFFF))))
      (dwc2-write (dwc2-gusbcfg) (logior cleared (gusbcfg-force-host)))))

  ;; Wait for mode switch
  (dwc2-delay-ms 50)

  ;; 2. Core reset
  (dwc2-core-reset)

  ;; 3. Configure AHB: DMA mode, INCR4 burst, global interrupt enable
  (dwc2-write (dwc2-gahbcfg)
              (logior (gahbcfg-dma-en)
                      (logior (gahbcfg-hbstlen-incr4)
                              (gahbcfg-glbl-intr-en))))

  ;; 4. Configure FIFO sizes
  ;; RX FIFO: 1024 dwords (4KB)
  (dwc2-write (dwc2-grxfsiz) 1024)
  ;; Non-periodic TX FIFO: start at 1024, size 256 dwords
  (dwc2-write (dwc2-gnptxfsiz) (logior (ash 1024 16) 256))
  ;; Periodic TX FIFO: start at 1280, size 256 dwords
  (dwc2-write (dwc2-hptxfsiz) (logior (ash 1280 16) 256))

  ;; 5. Flush FIFOs
  (dwc2-flush-tx-fifo)
  (dwc2-flush-rx-fifo)

  ;; 6. Configure host: FS/LS clock = 48MHz (FSLSPCLKSEL = 1)
  (dwc2-write (dwc2-hcfg) 1)

  ;; 7. Clear all channel interrupts
  (let ((ch 0))
    (loop
      (when (>= ch 16) (return nil))
      (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
      (dwc2-write (dwc2-hcintmsk ch) 0)
      (setq ch (+ ch 1))))

  ;; 8. Enable channel interrupts in HAINTMSK
  (dwc2-write (dwc2-haintmsk) #xFFFF)

  ;; 9. Disable global interrupt mask bits we don't need
  (dwc2-write (dwc2-gintmsk) 0)

  ;; 10. Power the port
  (let ((hprt (dwc2-hprt0-read-safe)))
    (dwc2-write (dwc2-hprt0) (logior hprt (hprt0-prtpwr))))

  ;; Wait for power stabilization
  (dwc2-delay-ms 100)

  ;; Print status
  (write-byte 68) (write-byte 87) (write-byte 67)   ; "DWC"
  (write-byte 50) (write-byte 58)                    ; "2:"

  ;; Check port connect
  (let ((hprt (dwc2-read (dwc2-hprt0))))
    (if (not (zerop (logand hprt (hprt0-prtconnsts))))
        (progn
          (write-byte 79) (write-byte 75) (write-byte 10)  ; "OK\n"
          1)
        (progn
          (write-byte 78) (write-byte 67) (write-byte 10)  ; "NC\n"
          0))))

;; ============================================================
;; Port reset
;; ============================================================

(defun dwc2-port-reset ()
  ;; Reset the USB port. Returns port speed (0=HS, 1=FS, 2=LS)
  ;; or -1 on failure.

  ;; Assert reset (bit 8)
  (let ((hprt (dwc2-hprt0-read-safe)))
    (dwc2-write (dwc2-hprt0) (logior hprt (hprt0-prtrst))))

  ;; Hold reset for 60ms
  (dwc2-delay-ms 60)

  ;; Deassert reset
  (let ((hprt (dwc2-hprt0-read-safe)))
    (let ((cleared (logand hprt (logxor (hprt0-prtrst) #xFFFFFFFF))))
      (dwc2-write (dwc2-hprt0) cleared)))

  ;; Wait for port enabled (bit 2)
  (dwc2-delay-ms 20)
  (let ((result -1))
    (let ((i 0))
      (loop
        (when (>= i 50000) (return nil))
        (let ((hprt (dwc2-read (dwc2-hprt0))))
          (when (not (zerop (logand hprt (hprt0-prtena))))
            ;; Port enabled — read speed from bits 17:18
            (let ((speed (logand (ash hprt -17) 3)))
              (setq result speed)
              (usb-set-port-speed speed))
            (return nil)))
        (setq i (+ i 1))))
    ;; Clear port enable change (W1C bit 3) and connect detect (W1C bit 1).
    ;;
    ;; MUST be based on the CURRENT HPRT0, not written bare.  HPRT0 mixes W1C
    ;; status bits with ordinary R/W control bits — notably PPWR (bit 12), the
    ;; port POWER enable.  Writing just the two W1C bits sends 0 to PPWR and
    ;; cuts power to the port, so the device drops off the bus and every
    ;; subsequent control transfer fails.  Measured on a real Pi Zero 2 W:
    ;;     after dwc2-init   HPRT0 = 0x21401  (PCSTS=1 connected, PPWR=1)
    ;;     after this write  HPRT0 = 0x00400  (PCSTS=0, PPWR=0 -- dead port)
    ;; and usb-enumerate then failed at its first GET_DEVICE_DESCRIPTOR with
    ;; "D1E", which looks exactly like a transfer bug and is not one.
    ;;
    ;; QEMU does not model port power, so this was invisible under emulation —
    ;; the same class as the zeroed-DRAM dependency: emulator-benign, fatal on
    ;; silicon.  dwc2-hprt0-read-safe masks the W1C bits OUT of the read, so
    ;; ORing in exactly the ones we intend to clear is the correct idiom; the
    ;; assert/deassert-reset writes above already use it.
    (dwc2-write (dwc2-hprt0)
                (logior (dwc2-hprt0-read-safe)
                        (hprt0-prtenchng) (hprt0-prtconndet)))
    ;; Print speed
    (write-byte 80) (write-byte 79) (write-byte 82) (write-byte 84)  ; "PORT"
    (write-byte 58)  ; ":"
    (if (eq result 1)
        (progn (write-byte 70) (write-byte 83))    ; "FS"
        (if (eq result 0)
            (progn (write-byte 72) (write-byte 83)) ; "HS"
            (if (eq result 2)
                (progn (write-byte 76) (write-byte 83)) ; "LS"
                (write-byte 63))))                        ; "?"
    (write-byte 10)
    result))

;; ============================================================
;; Host channel transfer
;; ============================================================

(defun dwc2-build-hcchar (mps epnum epdir eptype devaddr)
  ;; Build HCCHAR register value
  ;; mps: max packet size (bits 10:0)
  ;; epnum: endpoint number (bits 14:11)
  ;; epdir: 0=OUT, 1=IN (bit 15)
  ;; eptype: 0=control, 1=iso, 2=bulk, 3=interrupt (bits 19:18)
  ;; devaddr: device address (bits 28:22)
  (logior (logand mps #x7FF)
          (logior (ash (logand epnum #xF) 11)
                  (logior (ash (logand epdir 1) 15)
                          (logior (ash (logand eptype 3) 18)
                                  (ash (logand devaddr #x7F) 22))))))

(defun dwc2-build-hctsiz (xfersize pktcnt pid)
  ;; Build HCTSIZ register value
  ;; xfersize: bytes (bits 18:0)
  ;; pktcnt: packets (bits 28:19)
  ;; pid: DATA0=0, DATA1=2<<29, SETUP=3<<29
  (logior (logand xfersize #x7FFFF)
          (logior (ash (logand pktcnt #x3FF) 19)
                  pid)))

(defun dwc2-setup-channel (ch hcchar-val)
  ;; Write HCCHAR for channel (without enabling yet)
  ;; Clear any pending interrupts first
  (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
  ;; Enable all interrupt masks for this channel
  (dwc2-write (dwc2-hcintmsk ch) #x7FF)
  ;; Write HCCHAR without CHENA
  (dwc2-write (dwc2-hcchar ch) hcchar-val)
  ;; No split transfers
  (dwc2-write (dwc2-hcsplt ch) 0))

(defun dwc2-start-transfer (ch hctsiz-val dma-addr)
  ;; Configure transfer size and DMA, then enable channel
  (dwc2-write (dwc2-hctsiz ch) hctsiz-val)
  (dwc2-write (dwc2-hcdma ch) dma-addr)
  ;; Enable channel: OR in CHENA bit
  (let ((hcchar (dwc2-read (dwc2-hcchar ch))))
    (let ((enabled (logior hcchar (hcchar-chena))))
      ;; Clear CHDIS if set
      (let ((final (logand enabled (logxor (hcchar-chdis) #xFFFFFFFF))))
        (dwc2-write (dwc2-hcchar ch) final)))))

(defun dwc2-halt-channel (ch)
  ;; Halt an active channel. Sets CHDIS+CHENA, waits for CHHLTD.
  (let ((hcchar (dwc2-read (dwc2-hcchar ch))))
    (dwc2-write (dwc2-hcchar ch)
                (logior hcchar (logior (hcchar-chena) (hcchar-chdis)))))
  (let ((j 0))
    (loop
      (when (>= j 10000) (return nil))
      (when (not (zerop (logand (dwc2-read (dwc2-hcint ch)) (hcint-chhltd))))
        (return nil))
      (setq j (+ j 1))))
  (dwc2-write (dwc2-hcint ch) #xFFFFFFFF))

(defun dwc2-poll-channel (ch)
  ;; Poll channel for transfer completion.
  ;; Returns:
  ;;   1 = success (XFERCOMPL)
  ;;   0 = NAK (no data / timeout)
  ;;  -1 = STALL
  ;;  -2 = error (transaction error, babble, etc.)
  ;;  -3 = timeout
  ;;
  ;; IMPORTANT: QEMU's DWC2 auto-retries bulk NAK via dwc2_work_bh.
  ;; Do NOT halt the channel on NAK — just clear the flag and keep
  ;; polling.  The DWC2 model will resubmit the packet internally.
  (let ((result -3))
    (let ((i 0))
      (loop
        (when (>= i 50000)
          ;; Timeout: halt channel before returning
          (dwc2-halt-channel ch)
          (return nil))
        (let ((hcint (dwc2-read (dwc2-hcint ch))))
          ;; Check if channel halted (DWC2 halts on completion/error)
          (when (not (zerop (logand hcint (hcint-chhltd))))
            (if (not (zerop (logand hcint (hcint-xfercompl))))
                (setq result 1)
                (if (not (zerop (logand hcint (hcint-stall))))
                    (setq result -1)
                    (if (not (zerop (logand hcint (hcint-nak))))
                        (setq result 0)
                        (if (not (zerop (logand hcint (hcint-xacterr))))
                            (setq result -2)
                            (setq result -2)))))
            (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
            (return nil))
          ;; Non-halted XFERCOMPL (DMA mode may set this without halt)
          (when (not (zerop (logand hcint (hcint-xfercompl))))
            (setq result 1)
            (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
            (return nil))
          ;; NAK without halt: DWC2 auto-retries bulk/control.
          ;; Clear NAK flag and continue polling — do NOT halt.
          (when (not (zerop (logand hcint (hcint-nak))))
            (dwc2-write (dwc2-hcint ch) (hcint-nak))))
        (setq i (+ i 1))))
    result))

;; ============================================================
;; High-level transfer helpers
;; ============================================================

(defun dwc2-control-setup (ch devaddr setup-buf)
  ;; Send SETUP stage of a control transfer on channel ch
  ;; setup-buf: physical address of 8-byte SETUP packet
  ;; Returns: 1=success, <=0 = error
  (let ((hcchar (dwc2-build-hcchar 64 0 0 0 devaddr)))
    (dwc2-setup-channel ch hcchar)
    (let ((hctsiz (dwc2-build-hctsiz 8 1 (hctsiz-pid-setup))))
      (dwc2-start-transfer ch hctsiz setup-buf)
      (dwc2-poll-channel ch))))

(defun dwc2-control-data-in (ch devaddr buf len)
  ;; DATA stage of control IN transfer
  ;; Returns: 1=success, <=0 = error
  (let ((hcchar (dwc2-build-hcchar 64 0 1 0 devaddr)))
    (dwc2-setup-channel ch hcchar)
    (let ((pktcnt (+ (/ len 64) 1)))
      (let ((hctsiz (dwc2-build-hctsiz len pktcnt (hctsiz-pid-data1))))
        (dwc2-start-transfer ch hctsiz buf)
        (dwc2-poll-channel ch)))))

(defun dwc2-control-data-out (ch devaddr buf len)
  ;; DATA stage of control OUT transfer
  (let ((hcchar (dwc2-build-hcchar 64 0 0 0 devaddr)))
    (dwc2-setup-channel ch hcchar)
    (let ((pktcnt (+ (/ len 64) 1)))
      (let ((hctsiz (dwc2-build-hctsiz len pktcnt (hctsiz-pid-data1))))
        (dwc2-start-transfer ch hctsiz buf)
        (dwc2-poll-channel ch)))))

(defun dwc2-control-status-in (ch devaddr)
  ;; STATUS stage for control OUT (host sends data, device sends ZLP IN)
  (let ((hcchar (dwc2-build-hcchar 64 0 1 0 devaddr)))
    (dwc2-setup-channel ch hcchar)
    (let ((hctsiz (dwc2-build-hctsiz 0 1 (hctsiz-pid-data1))))
      (dwc2-start-transfer ch hctsiz (usb-data-buf))
      (dwc2-poll-channel ch))))

(defun dwc2-control-status-out (ch devaddr)
  ;; STATUS stage for control IN (host receives data, host sends ZLP OUT)
  (let ((hcchar (dwc2-build-hcchar 64 0 0 0 devaddr)))
    (dwc2-setup-channel ch hcchar)
    (let ((hctsiz (dwc2-build-hctsiz 0 1 (hctsiz-pid-data1))))
      (dwc2-start-transfer ch hctsiz (usb-data-buf))
      (dwc2-poll-channel ch))))

;; ============================================================
;; Bulk transfers
;; ============================================================

(defun dwc2-bulk-transfer (ch devaddr epnum epdir buf len mps)
  ;; Execute a bulk transfer (blocking, with halt on timeout).
  ;; Used for bulk OUT and control transfers.
  ;; epdir: 0=OUT, 1=IN
  ;; Returns: 1=success, 0=NAK, <0=error
  (let ((hcchar (dwc2-build-hcchar mps epnum epdir 2 devaddr)))
    (dwc2-setup-channel ch hcchar)
    ;; QEMU's DWC2 manages data toggle internally via USB endpoint state.
    ;; Always use DATA0 PID — the actual toggle is tracked by QEMU.
    (let ((pktcnt (if (zerop len) 1 (/ (+ len (- mps 1)) mps))))
      (let ((hctsiz (dwc2-build-hctsiz len pktcnt (hctsiz-pid-data0))))
        (dwc2-start-transfer ch hctsiz buf)
        (dwc2-poll-channel ch)))))

;; Non-blocking bulk IN poll for persistent channel.
;; The channel stays active between calls — DWC2's work_bh auto-retries NAK.
;; Returns: 1=data ready, 0=no data yet (channel still active), <0=error
;;
;; IMPORTANT: Halting channels corrupts QEMU's DWC2 internal state
;; (needs_service persists, causing stale work_bh retries). Instead,
;; we keep the channel active and let DWC2 handle NAK retries naturally.
;; When data arrives, DWC2 sets XFERCOMPL+CHHLTD and clears CHENA.
(defun dwc2-poll-bulk-in (ch)
  (let ((hcint (dwc2-read (dwc2-hcint ch))))
    ;; Check if channel halted (DWC2 halts on completion/error, NOT on NAK)
    (if (not (zerop (logand hcint (hcint-chhltd))))
        (progn
          (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
          (if (not (zerop (logand hcint (hcint-xfercompl))))
              1   ; success — data ready
              (if (not (zerop (logand hcint (hcint-stall))))
                  -1  ; STALL
                  -2)))  ; other error
        ;; Non-halted XFERCOMPL (DMA mode may set this without halt)
        (if (not (zerop (logand hcint (hcint-xfercompl))))
            (progn
              (dwc2-write (dwc2-hcint ch) #xFFFFFFFF)
              1)
            ;; NAK without halt: clear NAK flag, DWC2 auto-retries
            (progn
              (when (not (zerop (logand hcint (hcint-nak))))
                (dwc2-write (dwc2-hcint ch) (hcint-nak)))
              0)))))  ; no data yet

;; Start a new bulk IN transfer on channel ch.
;; Called after dwc2-poll-bulk-in returned non-zero (transfer completed/errored).
(defun dwc2-start-bulk-in (ch devaddr epnum buf len mps)
  (let ((hcchar (dwc2-build-hcchar mps epnum 1 2 devaddr)))
    (dwc2-setup-channel ch hcchar)
    (let ((pktcnt (if (zerop len) 1 (/ (+ len (- mps 1)) mps))))
      (let ((hctsiz (dwc2-build-hctsiz len pktcnt (hctsiz-pid-data0))))
        (dwc2-start-transfer ch hctsiz buf)))))

;; Start a new interrupt IN transfer on channel ch.
;; Identical to dwc2-start-bulk-in but uses eptype=3 (interrupt).
;; Reuses dwc2-poll-bulk-in unchanged for polling (DWC2 treats them the same).
(defun dwc2-start-interrupt-in (ch devaddr epnum buf len mps)
  (let ((hcchar (dwc2-build-hcchar mps epnum 1 3 devaddr)))
    (dwc2-setup-channel ch hcchar)
    (let ((pktcnt (if (zerop len) 1 (/ (+ len (- mps 1)) mps))))
      (let ((hctsiz (dwc2-build-hctsiz len pktcnt (hctsiz-pid-data0))))
        (dwc2-start-transfer ch hctsiz buf)))))
