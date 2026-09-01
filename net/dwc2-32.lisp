;;;; dwc2-32.lisp - 32-bit fixnum safe overrides for DWC2 USB host controller
;;;;
;;;; On 32-bit targets with fixnum-shift=1, values >= 0x40000000 overflow
;;;; positive fixnum range. This file overrides DWC2 functions that use
;;;; (ash 1 30), (ash 1 31), or #xFFFFFFFF with safe equivalents.
;;;;
;;;; IMPORTANT: QEMU's DWC2 MMIO only accepts 4-byte (word) accesses.
;;;; Byte writes to MMIO registers are silently dropped!
;;;;
;;;; Approach: use scratch RAM to build 32-bit values with bits 30-31,
;;;; then read/write via :u64 (raw, no tag shift) for full 32-bit MMIO access.
;;;; (- 0 1) = -1 = 0xFFFFFFFF in two's complement (safe on 32-bit).

;; Safe bit constants
;; (ash 1 31) and (ash 1 30) overflow on 32-bit — handled via scratch
;; (- 0 1) = 0xFFFFFFFF (safe)
(defun hcchar-chena ()  0)  ;; Placeholder — real bit 31 set via scratch
(defun hcchar-chdis ()  0)  ;; Placeholder — real bit 30 set via scratch
(defun grstctl-ahbidl () 0) ;; Placeholder — check via scratch

;; HCTSIZ PID values overflow on 32-bit (bits 30:29)
;; DATA0 = 0 << 29 = 0 (safe)
;; DATA1 = 2 << 29 = 0x40000000 (overflows)
;; SETUP = 3 << 29 = 0x60000000 (overflows)
;; Strategy: pid functions store PID code at scratch, return 0.
;; dwc2-start-transfer reads scratch and applies PID via scratch+:u64.
;; PID scratch: ssh-ipc-base + 0x60800
(defun hctsiz-pid-data0 ()
  (setf (mem-ref (+ #x01100000 #x60800) :u32) 0)
  0)
(defun hctsiz-pid-data1 ()
  (setf (mem-ref (+ #x01100000 #x60800) :u32) 2)
  0)
(defun hctsiz-pid-setup ()
  (setf (mem-ref (+ #x01100000 #x60800) :u32) 3)
  0)

;; Safe HPRT0 mask: ~(bit1|bit2|bit3|bit5) = ~0x2E = 0xFFFFFFD1
;; (- 0 47) = -47 = 0xFFFFFFD1 in 32-bit two's complement
(defun dwc2-hprt0-mask () (- 0 47))

;; ============================================================
;; Scratch-based MMIO helpers
;; QEMU DWC2 requires full 32-bit word read/write (min_access_size=4).
;; We copy MMIO → scratch RAM (via :u64 raw), manipulate bytes in RAM,
;; then copy scratch → MMIO (via :u64 raw).
;; Scratch address: ssh-ipc-base + 0x60C00
;; ============================================================

(defun dwc2-scratch () (+ #x01100000 #x60C00))

;; Read DWC2 MMIO register (32-bit) into scratch RAM via :u64 (raw)
(defun dwc2-read-to-scratch (addr)
  (setf (mem-ref (dwc2-scratch) :u64) (mem-ref addr :u64)))

;; Write scratch RAM to DWC2 MMIO register (32-bit) via :u64 (raw)
(defun dwc2-write-from-scratch (addr)
  (setf (mem-ref addr :u64) (mem-ref (dwc2-scratch) :u64)))

;; Set bit 31 of a DWC2 register: read → scratch → set byte 3 bit 7 → write
(defun dwc2-set-bit31 (addr)
  (dwc2-read-to-scratch addr)
  (let ((b3 (mem-ref (+ (dwc2-scratch) 3) :u8)))
    (setf (mem-ref (+ (dwc2-scratch) 3) :u8) (logior b3 #x80)))
  (dwc2-write-from-scratch addr))

;; Set bit 30 of a DWC2 register
(defun dwc2-set-bit30 (addr)
  (dwc2-read-to-scratch addr)
  (let ((b3 (mem-ref (+ (dwc2-scratch) 3) :u8)))
    (setf (mem-ref (+ (dwc2-scratch) 3) :u8) (logior b3 #x40)))
  (dwc2-write-from-scratch addr))

;; Clear bit 30 of a DWC2 register
(defun dwc2-clear-bit30 (addr)
  (dwc2-read-to-scratch addr)
  (let ((b3 (mem-ref (+ (dwc2-scratch) 3) :u8)))
    (setf (mem-ref (+ (dwc2-scratch) 3) :u8) (logand b3 #x3F)))
  (dwc2-write-from-scratch addr))

;; Test bit 31 of a DWC2 register (via scratch read)
(defun dwc2-test-bit31 (addr)
  (dwc2-read-to-scratch addr)
  (not (zerop (logand (mem-ref (+ (dwc2-scratch) 3) :u8) #x80))))

;; Test bit 30 of a DWC2 register (via scratch read)
(defun dwc2-test-bit30 (addr)
  (dwc2-read-to-scratch addr)
  (not (zerop (logand (mem-ref (+ (dwc2-scratch) 3) :u8) #x40))))

;; Override: write all 1s to a register (clear all interrupts)
(defun dwc2-write-allones (addr)
  (dwc2-write addr (- 0 1)))

;; Override dwc2-init: replace #xFFFFFFFF with (- 0 1), handle bit 30/31
(defun dwc2-init ()
  ;; 1. Force host mode: clear bit 30 (force-device), set bit 29 (force-host)
  (let ((cfg (dwc2-read (dwc2-gusbcfg))))
    (let ((with-host (logior cfg (gusbcfg-force-host))))
      (dwc2-write (dwc2-gusbcfg) with-host)
      (dwc2-clear-bit30 (dwc2-gusbcfg))))

  ;; Wait for mode switch
  (dwc2-delay-ms 50)

  ;; 2. Core reset
  (dwc2-core-reset)

  ;; 3. Configure AHB
  (dwc2-write (dwc2-gahbcfg)
              (logior (gahbcfg-dma-en)
                      (logior (gahbcfg-hbstlen-incr4)
                              (gahbcfg-glbl-intr-en))))

  ;; 4. Configure FIFO sizes
  (dwc2-write (dwc2-grxfsiz) 1024)
  (dwc2-write (dwc2-gnptxfsiz) (logior (ash 1024 16) 256))
  (dwc2-write (dwc2-hptxfsiz) (logior (ash 1280 16) 256))

  ;; 5. Flush FIFOs
  (dwc2-flush-tx-fifo)
  (dwc2-flush-rx-fifo)

  ;; 6. Configure host: FS/LS clock = 48MHz
  (dwc2-write (dwc2-hcfg) 1)

  ;; 7. Clear all channel interrupts
  (let ((ch 0))
    (loop
      (when (>= ch 16) (return nil))
      (dwc2-write-allones (dwc2-hcint ch))
      (dwc2-write (dwc2-hcintmsk ch) 0)
      (setq ch (+ ch 1))))

  ;; 8. Enable channel interrupts
  (dwc2-write (dwc2-haintmsk) #xFFFF)

  ;; 9. Disable global interrupt mask
  (dwc2-write (dwc2-gintmsk) 0)

  ;; 10. Power the port
  (let ((hprt (dwc2-hprt0-read-safe)))
    (dwc2-write (dwc2-hprt0) (logior hprt (hprt0-prtpwr))))

  ;; Wait for power stabilization
  (dwc2-delay-ms 100)

  ;; Print status
  (%serial-byte 68) (%serial-byte 87) (%serial-byte 67)
  (%serial-byte 50) (%serial-byte 58)

  (let ((hprt (dwc2-read (dwc2-hprt0))))
    (if (not (zerop (logand hprt (hprt0-prtconnsts))))
        (progn
          (%serial-byte 79) (%serial-byte 75) (%serial-byte 10)
          1)
        (progn
          (%serial-byte 78) (%serial-byte 67) (%serial-byte 10)
          0))))

;; Override dwc2-core-reset: check AHB idle via scratch (bit 31)
(defun dwc2-core-reset ()
  ;; Wait for AHB master idle (bit 31 of GRSTCTL)
  (let ((i 0))
    (loop
      (when (>= i 100000) (return nil))
      (when (dwc2-test-bit31 (dwc2-grstctl))
        (return nil))
      (setq i (+ i 1))))
  ;; Assert core soft reset
  (dwc2-write (dwc2-grstctl) (grstctl-csftrst))
  ;; Wait for reset to clear
  (let ((i 0))
    (loop
      (when (>= i 100000) (return nil))
      (when (zerop (logand (dwc2-read (dwc2-grstctl)) (grstctl-csftrst)))
        (return nil))
      (setq i (+ i 1))))
  (dwc2-delay-ms 10))

;; Override dwc2-start-transfer: set PID bits + CHENA via scratch+:u64
(defun dwc2-start-transfer (ch hctsiz-val dma-addr)
  ;; Write HCTSIZ (PID bits are 0 — we set them via scratch below)
  (dwc2-write (dwc2-hctsiz ch) hctsiz-val)
  ;; Set PID bits 30:29 in HCTSIZ via scratch
  ;; PID code was stored at scratch by hctsiz-pid-* function
  (let ((pid (mem-ref (+ #x01100000 #x60800) :u32)))
    (when (not (zerop pid))
      (let ((hctsiz-addr (dwc2-hctsiz ch)))
        (dwc2-read-to-scratch hctsiz-addr)
        (let ((b3 (mem-ref (+ (dwc2-scratch) 3) :u8)))
          (setf (mem-ref (+ (dwc2-scratch) 3) :u8)
                (logior b3 (ash pid 5))))  ; pid << 5 puts bits at 29:30 of byte 3
        (dwc2-write-from-scratch hctsiz-addr))))
  ;; Write DMA address
  (dwc2-write (dwc2-hcdma ch) dma-addr)
  ;; Enable channel: read HCCHAR into scratch, clear CHDIS (bit 30),
  ;; set CHENA (bit 31), write back as single 32-bit word
  (let ((hcchar-addr (dwc2-hcchar ch)))
    (dwc2-read-to-scratch hcchar-addr)
    (let ((b3 (mem-ref (+ (dwc2-scratch) 3) :u8)))
      ;; Clear bit 30 (CHDIS) and set bit 31 (CHENA) in one operation
      (setf (mem-ref (+ (dwc2-scratch) 3) :u8)
            (logior (logand b3 #x3F) #x80)))
    (dwc2-write-from-scratch hcchar-addr)))

;; Override dwc2-halt-channel: set CHDIS (bit 30) + CHENA (bit 31) via scratch
(defun dwc2-halt-channel (ch)
  (let ((hcchar-addr (dwc2-hcchar ch)))
    (dwc2-read-to-scratch hcchar-addr)
    (let ((b3 (mem-ref (+ (dwc2-scratch) 3) :u8)))
      ;; Set both CHDIS (bit 30) and CHENA (bit 31)
      (setf (mem-ref (+ (dwc2-scratch) 3) :u8)
            (logior b3 #xC0)))
    (dwc2-write-from-scratch hcchar-addr))
  (let ((j 0))
    (loop
      (when (>= j 10000) (return nil))
      (when (not (zerop (logand (dwc2-read (dwc2-hcint ch)) (hcint-chhltd))))
        (return nil))
      (setq j (+ j 1))))
  (dwc2-write-allones (dwc2-hcint ch)))

;; Override dwc2-poll-channel: uses #xFFFFFFFF
;; Must match original logic: only return on CHHLTD (halted) or non-halt XFERCOMPL.
;; NAK without halt means DWC2 is auto-retrying — clear flag and keep polling.
(defun dwc2-poll-channel (ch)
  (let ((result (- 0 3)))
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
                    (setq result (- 0 1))
                    (if (not (zerop (logand hcint (hcint-nak))))
                        (setq result 0)
                        (if (not (zerop (logand hcint (hcint-xacterr))))
                            (setq result (- 0 2))
                            (setq result (- 0 2))))))
            (dwc2-write-allones (dwc2-hcint ch))
            (return nil))
          ;; Non-halted XFERCOMPL (DMA mode may set this without halt)
          (when (not (zerop (logand hcint (hcint-xfercompl))))
            (setq result 1)
            (dwc2-write-allones (dwc2-hcint ch))
            (return nil))
          ;; NAK without halt: DWC2 auto-retries. Clear NAK and keep polling.
          (when (not (zerop (logand hcint (hcint-nak))))
            (dwc2-write (dwc2-hcint ch) (hcint-nak))))
        (setq i (+ i 1))))
    result))

;; Override dwc2-poll-bulk-in: same fix for #xFFFFFFFF
(defun dwc2-poll-bulk-in (ch)
  (let ((hcint (dwc2-read (dwc2-hcint ch))))
    (if (not (zerop (logand hcint (hcint-chhltd))))
        (progn
          (dwc2-write-allones (dwc2-hcint ch))
          (if (not (zerop (logand hcint (hcint-xfercompl))))
              1
              (if (not (zerop (logand hcint (hcint-stall))))
                  (- 0 1)
                  (- 0 2))))
        (if (not (zerop (logand hcint (hcint-xfercompl))))
            (progn
              (dwc2-write-allones (dwc2-hcint ch))
              1)
            (progn
              (when (not (zerop (logand hcint (hcint-nak))))
                (dwc2-write (dwc2-hcint ch) (hcint-nak)))
              0)))))

;; Override dwc2-setup-channel: uses #xFFFFFFFF
(defun dwc2-setup-channel (ch hcchar-val)
  (dwc2-write-allones (dwc2-hcint ch))
  (dwc2-write (dwc2-hcintmsk ch) #x7FF)
  (dwc2-write (dwc2-hcchar ch) hcchar-val)
  (dwc2-write (dwc2-hcsplt ch) 0))

;; Override dwc2-port-reset to use safe HPRT0 mask and #xFFFFFFFF
(defun dwc2-port-reset ()
  (let ((hprt (dwc2-hprt0-read-safe)))
    (dwc2-write (dwc2-hprt0) (logior hprt (hprt0-prtrst))))
  (dwc2-delay-ms 60)
  ;; Clear reset
  (let ((hprt (dwc2-hprt0-read-safe)))
    (let ((cleared (logand hprt (logxor (hprt0-prtrst) (- 0 1)))))
      (dwc2-write (dwc2-hprt0) cleared)))
  (dwc2-delay-ms 20)
  ;; Read port speed from HPRT0 bits [17:16]
  (let ((hprt (dwc2-read (dwc2-hprt0))))
    ;; Print speed
    (%serial-byte 80) (%serial-byte 79) (%serial-byte 82) (%serial-byte 84) (%serial-byte 58)
    (let ((speed (logand (ash hprt -17) 3)))
      (cond
        ((eq speed 0) (%serial-byte 72) (%serial-byte 83))
        ((eq speed 1) (%serial-byte 70) (%serial-byte 83))
        ((eq speed 2) (%serial-byte 76) (%serial-byte 83)))
      (%serial-byte 10)
      speed)))
