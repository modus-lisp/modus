;;;; r8152.lisp - Realtek RTL8153 RX enable for CDC-ECM mode
;;;;
;;;; Loaded AFTER cdc-ether.lisp.  On real RTL8153 silicon (Anker hub dongle,
;;;; 0x291A:2817 hub + 0BDA:8153 NIC) the CDC-ECM function enumerates,
;;;; configures, reads its MAC and TRANSMITS perfectly — but never forwards a
;;;; single received frame: the MAC-level receive path is left disabled and
;;;; no amount of ECM-standard requests (SET_INTERFACE alt 1, packet filter,
;;;; endpoint-halt clears) opens it.  Wire-verified with tcpdump on the far
;;;; end: our ARP requests hit the wire and were answered in microseconds;
;;;; the replies died inside the NIC.
;;;;
;;;; The fix is Realtek-specific: the vendor register interface (bRequest 5,
;;;; wIndex = MCU_TYPE_PLA 0x0100 | byte-enables) WORKS while the ECM config
;;;; is active, so we drive the MAC's RX enable directly and keep the ECM
;;;; data path (raw frames, no descriptors).  Every step below was bisected
;;;; live on the board with PLA_PHYSTATUS reads between writes:
;;;;
;;;;   - PLA_RCR (0xc010, dword): |= AAP|APM|AM|AB — safe as whole-dword RMW.
;;;;   - PLA_CR (0xe813, byte lane 3 of dword 0xe810): |= RE|TE — must be
;;;;     bracketed by the PLA_CRWECR (0xe81c) unlock: 0xC0 before, 0x00
;;;;     after.  Verified stuck by read-back (RE|TE = 0x0C).
;;;;   - PLA_MISC_1 (0xe85a, word at lanes 2-3 of dword 0xe858): clear
;;;;     RXDY_GATED_EN (0x0008).  THE TRAP: a whole-dword RMW of 0xe858
;;;;     WEDGES THE DEVICE (vendor interface dies, TX dies) even when the
;;;;     neighbour bytes are written back with their read values — the write
;;;;     MUST be word-granular via byte-enables (0x33 << 2 -> wIndex 0x01CC),
;;;;     exactly as Linux's ocp_write_word does.  With the proper enables the
;;;;     un-gate lands cleanly and is the final step that opens RX.
;;;;
;;;; Config note: the "vendor" config 1 on this dongle personality is a trap —
;;;; vendor register access DIES when it is selected (and enumerating into it
;;;; gives no working registers either).  Stay in ECM (config-descriptor
;;;; index 1, the usb-hub-config-index default).
;;;;
;;;; What is NOT here, deliberately: the FMC packet-filter-MCU toggle and the
;;;; full U-Boot r8153_init — unnecessary per the live bisect; the NIC
;;;; arrives from the loader/firmware with everything else in order.

;; ============================================================
;; Vendor register access (MCU_TYPE_PLA)
;; ============================================================

(defun r8152-reg-scratch ()
  ;; A dword scratch inside the USB DMA window, clear of the setup buffer
  ;; (+0x000), data buffer (+0x40..+0x240) and usb-state (+0x800).
  (+ (usb-dma-base) #x400))

(defun r8152-read-dword-once (addr)
  ;; One PLA dword read.  Returns the 32-bit value, or -1 on transfer failure.
  (let ((r (usb-control-transfer (usb-dev-addr) #xC0 5 addr #x0100
                                 (r8152-reg-scratch) 4)))
    (if (<= r 0) -1 (mem-ref (r8152-reg-scratch) :u32))))

(defun r8152-read-dword (addr)
  ;; #275: vendor control transfers on this DWC2+RTL8153 are FLAKY — an
  ;; occasional -1 that clears on retry.  Retry a failed read up to 5x with a
  ;; short settle so a single transient glitch does not fail the whole enable.
  (let ((i 0) (v -1))
    (loop
      (when (or (>= v 0) (>= i 5)) (return v))
      (setq v (r8152-read-dword-once addr))
      (when (< v 0) (dwc2-delay-ms 20))
      (setq i (+ i 1)))
    v))

(defun r8152-write-dword (addr val)
  ;; Whole-dword write (BYTE_EN_DWORD).  Safe only for registers whose
  ;; aligned dword has no write-sensitive neighbours (RCR, MAR).
  (setf (mem-ref (r8152-reg-scratch) :u32) val)
  (usb-control-transfer (usb-dev-addr) #x40 5 addr #x01FF
                        (r8152-reg-scratch) 4))

(defun r8152-rmw-dword (addr set clear)
  ;; new = (old & ~clear) | set on a PLA dword.  Returns 1/0.
  (let ((old (r8152-read-dword addr)))
    (if (< old 0)
        0
        (let ((r (r8152-write-dword addr
                                    (logior (logand old (logxor clear #xFFFFFFFF))
                                            set))))
          (if (> r 0) 1 0)))))

(defun r8152-phystatus ()
  ;; PLA_PHYSTATUS byte (link bit 0x02).  0xFF/255 = vendor access dead.
  (logand (r8152-read-dword #xe908) #xFF))

;; ============================================================
;; The RX enable (order and byte-enables are wire-bisected — see header)
;; ============================================================

(defun r8152-ungate-rxdy ()
  ;; Clear RXDY_GATED_EN in PLA_MISC_1 with a WORD-granular write
  ;; (byte-enables 0x33 << 2 for lanes 2-3 of dword 0xe858).  A dword write
  ;; here bricks the vendor interface AND the data path until re-enumeration.
  (let ((old (r8152-read-dword #xe858)))
    (if (< old 0)
        0
        (progn
          (setf (mem-ref (r8152-reg-scratch) :u32)
                (logand old #xFFF7FFFF))
          (let ((r (usb-control-transfer (usb-dev-addr) #x40 5 #xe858 #x01CC
                                         (r8152-reg-scratch) 4)))
            (if (> r 0) 1 0))))))

(defun r8152-rx-ok ()
  ;; RX is enabled iff PLA_CR shows RE|TE (byte lane 3 of dword 0xe810), the
  ;; RCR accept bits are set (0xc010) AND RXDY_GATED_EN is clear in PLA_MISC_1
  ;; (bit 19 of dword 0xe858).
  (let ((cr (r8152-read-dword #xe810))
        (rc (r8152-read-dword #xc010))
        (m1 (r8152-read-dword #xe858)))
    (if (and (>= cr 0) (>= rc 0) (>= m1 0)
             (= (logand cr #x0C000000) #x0C000000)
             (= (logand rc #x0E) #x0E)
             (= (logand m1 #x80000) 0))
        1 0)))

;; Both confirm-helpers are defined BEFORE r8152-rx-enable that calls them —
;; a forward call can compile to a silent NIL sentinel on the MVM (CLAUDE.md
;; limitation #1 / #215), which would make the enable a no-op.
(defun r8152-set-bits-confirm (addr set)
  ;; RMW-set SET in the dword at ADDR, read back, retry up to 4x (50ms) until
  ;; the bits stick.  Safe only for non-config-gated registers (RCR here).
  (let ((i 0) (ok 0))
    (loop
      (when (or (> ok 0) (>= i 4)) (return ok))
      (r8152-rmw-dword addr set 0)
      (dwc2-delay-ms 50)
      (let ((v (r8152-read-dword addr)))
        (when (and (>= v 0) (= (logand v set) set)) (setq ok 1)))
      (setq i (+ i 1)))
    ok))

(defun r8152-ungate-confirm ()
  ;; Clear RXDY_GATED_EN (word-granular via r8152-ungate-rxdy), read back,
  ;; retry up to 4x (50ms) until the gate bit is clear.
  (let ((i 0) (ok 0))
    (loop
      (when (or (> ok 0) (>= i 4)) (return ok))
      (r8152-ungate-rxdy)
      (dwc2-delay-ms 50)
      (let ((v (r8152-read-dword #xe858)))
        (when (and (>= v 0) (= (logand v #x80000) 0)) (setq ok 1)))
      (setq i (+ i 1)))
    ok))

(defun r8152-rx-enable ()
  ;; MINIMAL + IDEMPOTENT.  Root-caused on a Pi Zero 2 W by reading the
  ;; registers back over the REPL:
  ;;   * U-Boot (which used this NIC for its own TFTP) leaves PLA_CR with
  ;;     RE|TE ALREADY SET, and that survives Modus's port reset — so we must
  ;;     NOT touch PLA_CR or its CRWECR config-write unlock.  Re-writing
  ;;     PLA_CR under the unlock is exactly what wedged the chip (PHYSTATUS
  ;;     0xFF, vendor interface dead) in every earlier attempt.
  ;;   * What U-Boot leaves WRONG for us is two "safe" (non-config-gated)
  ;;     registers: PLA_RCR accept bits are clear (so every frame is filtered
  ;;     out) and RXDY_GATED_EN is set (so the RX path is gated off).
  ;; So the whole enable is two idempotent writes — RCR accept + RXDY ungate
  ;; — each written then READ BACK, retrying only that one write on a flaky
  ;; -1 (vendor transfers occasionally glitch; see r8152-read-dword).  No
  ;; whole-sequence retry (that collides and wedges), no PLA_CR, no CRWECR.
  ;; RCR = APM|AM|AB (0x0E), matching Linux rtl8152_set_rx_mode (NOT AAP/
  ;; promiscuous).
  (r8152-set-bits-confirm #xc010 #x0E)     ; RCR accept perfect|multi|broadcast
  (r8152-ungate-confirm)                    ; clear RXDY_GATED_EN (word-granular)
  (r8152-rx-ok))

;; ============================================================
;; Probe override: CDC-ECM bring-up + Realtek RX enable
;; ============================================================

(defun e1000-probe ()
  ;; cdc-ether-init does the whole USB bring-up (hub enumeration into the
  ;; ECM config, alt-setting 1, MAC from the ECM iMACAddress string, state
  ;; init) and arms the persistent bulk-IN; then the Realtek RX enable opens
  ;; the receive path and the channel is re-armed clean.  ECM framing is raw
  ;; ethernet, so cdc-ether's e1000-send/e1000-receive/e1000-rx-buf are used
  ;; unchanged.
  (let ((r (cdc-ether-init)))
    (if (zerop r)
        0
        (progn
          (r8152-rx-enable)
          (dwc2-halt-channel 1)
          (usb-set-bulk-in-toggle 0)
          (dwc2-start-bulk-in 1 (usb-dev-addr) (usb-bulk-in-ep)
                              (cdc-rx-buf-addr) 2048 (usb-bulk-in-mps))
          1))))
