;;;; r8152.lisp - Realtek RTL8153 driver, ADOPT + vendor-descriptor framing
;;;;
;;;; Loaded AFTER cdc-ether.lisp; overrides e1000-probe/send/receive/rx-buf.
;;;;
;;;; DESIGN: on this bare-metal RPi the netboot loader (U-Boot) drives the
;;;; SAME DWC2 host controller and the SAME RTL8153 to fetch the kernel over
;;;; TFTP.  When Modus takes over it INHERITS that fully-working device rather
;;;; than tearing it down: the controller is left running, the NIC enumerated
;;;; in its vendor configuration with RX already enabled, and Modus can issue
;;;; control/bulk transfers to it directly (verified: GET_DESCRIPTOR from the
;;;; REPL with no dwc2-init / no enumerate / no port reset returns the device
;;;; descriptor).  So e1000-probe does NOT dwc2-init or usb-enumerate — those
;;;; re-configured the NIC into CDC-ECM and produced a flaky raw-frame path.
;;;;
;;;; The earlier CDC-ECM approach (drive the MAC RX-enable registers by hand
;;;; and send/receive RAW ethernet frames) pinged but dropped ~half its
;;;; frames — the chip was in ECM mode while we poked its vendor registers, a
;;;; hybrid it only half-supports.  This driver instead matches U-Boot's
;;;; r8152: use the VENDOR framing the RTL8153 actually expects —
;;;;   TX: an 8-byte tx_desc {opts1 = len | TX_FS(1<<31) | TX_LS(1<<30);
;;;;       opts2 = 0} followed by the frame.
;;;;   RX: each bulk-IN payload is one or more frames, each prefixed by a
;;;;       24-byte rx_desc; frame length = opts1 & 0x7FFF, minus the 4-byte
;;;;       CRC; payload starts 24 bytes in.
;;;; (We take the first frame per bulk transfer; a ring parser for aggregated
;;;; frames is a follow-up.)
;;;;
;;;; The RTL8153's USB address is assigned by U-Boot and SHUFFLES per boot
;;;; (seen at 3 and 5), so e1000-probe SCANS for VID 0x0BDA PID 0x8153 rather
;;;; than hard-coding it.  Endpoints are fixed by the vendor config: bulk-IN
;;;; ep1, bulk-OUT ep2, interrupt-IN ep3, all mps 512.

;; ============================================================
;; Vendor register access (MCU_TYPE_PLA) — used for the MAC read and an
;; optional RX-enable safety net.  bRequest 5, bmRequestType 0xC0 read /
;; 0x40 write, wValue = register addr, wIndex = MCU_TYPE_PLA (0x0100) |
;; byte-enables.
;; ============================================================

(defun r8152-reg-scratch () (+ (usb-dma-base) #x400))

(defun r8152-read-dword-once (addr)
  (let ((r (usb-control-transfer (usb-dev-addr) #xC0 5 addr #x0100
                                 (r8152-reg-scratch) 4)))
    (if (<= r 0) -1 (mem-ref (r8152-reg-scratch) :u32))))

(defun r8152-read-dword (addr)
  ;; Vendor control transfers occasionally glitch (-1); retry a few times.
  (let ((i 0) (v -1))
    (loop
      (when (or (>= v 0) (>= i 5)) (return v))
      (setq v (r8152-read-dword-once addr))
      (when (< v 0) (dwc2-delay-ms 20))
      (setq i (+ i 1)))
    v))

(defun r8152-rx-ok ()
  ;; RX enabled iff PLA_CR RE|TE set, PLA_RCR accept bits set, RXDY ungated.
  (let ((cr (r8152-read-dword #xe810))
        (rc (r8152-read-dword #xc010))
        (m1 (r8152-read-dword #xe858)))
    (if (and (>= cr 0) (>= rc 0) (>= m1 0)
             (= (logand cr #x0C000000) #x0C000000)
             (= (logand rc #x0E) #x0E)
             (= (logand m1 #x80000) 0))
        1 0)))

;; ============================================================
;; Adopt: find the RTL8153 among U-Boot's already-enumerated devices
;; ============================================================

(defun r8152-is-8153 (buf)
  ;; buf holds an 18-byte device descriptor; VID 0x0BDA PID 0x8153 at 8..11.
  (and (eq (usb-desc-byte buf 8) #xDA) (eq (usb-desc-byte buf 9) #x0B)
       (eq (usb-desc-byte buf 10) #x53) (eq (usb-desc-byte buf 11) #x81)))

(defun r8152-find-addr ()
  ;; Scan USB addresses 2..7 for the RTL8153.  Returns the address, or 0.
  (let ((a 2) (found 0) (dbuf (usb-data-buf)))
    (loop
      (when (or (> found 0) (> a 7)) (return found))
      (let ((r (usb-get-descriptor a 1 0 dbuf 18)))
        (when (and (> r 0) (r8152-is-8153 dbuf)) (setq found a)))
      (setq a (+ a 1)))
    found))

(defun r8152-read-mac (state)
  ;; MAC from PLA_IDR (0xc000, 6 bytes) into state+0x08..0x0D.
  (let ((lo (r8152-read-dword #xc000)) (hi (r8152-read-dword #xc004)))
    (when (and (>= lo 0) (>= hi 0))
      (setf (mem-ref (+ state #x08) :u8) (logand lo #xFF))
      (setf (mem-ref (+ state #x09) :u8) (logand (ash lo -8) #xFF))
      (setf (mem-ref (+ state #x0A) :u8) (logand (ash lo -16) #xFF))
      (setf (mem-ref (+ state #x0B) :u8) (logand (ash lo -24) #xFF))
      (setf (mem-ref (+ state #x0C) :u8) (logand hi #xFF))
      (setf (mem-ref (+ state #x0D) :u8) (logand (ash hi -8) #xFF)))))

;; RXDY ungate (word-granular, byte-enables 0x33<<2 -> wIndex 0x01CC).
(defun r8152-ungate-rxdy ()
  (let ((old (r8152-read-dword #xe858)))
    (if (< old 0) 0
        (progn
          (setf (mem-ref (r8152-reg-scratch) :u32) (logand old #xFFF7FFFF))
          (let ((r (usb-control-transfer (usb-dev-addr) #x40 5 #xe858 #x01CC
                                         (r8152-reg-scratch) 4)))
            (if (> r 0) 1 0))))))

;; Set the PLA_RCR accept bits (whole-dword write, wIndex = PLA | 0xFF).
;; U-Boot leaves RCR clear, so the NIC filters every frame until we set
;; APM|AM|AB (0x0E, non-promiscuous, matching Linux rtl8152_set_rx_mode).
(defun r8152-set-rcr ()
  (let ((old (r8152-read-dword #xc010)))
    (if (< old 0) 0
        (progn
          (setf (mem-ref (r8152-reg-scratch) :u32) (logior old #x0E))
          (let ((r (usb-control-transfer (usb-dev-addr) #x40 5 #xc010 #x01FF
                                         (r8152-reg-scratch) 4)))
            (if (> r 0) 1 0))))))

;; ============================================================
;; Probe: adopt U-Boot's running device (no init, no enumerate)
;; ============================================================

(defun e1000-probe ()
  (let ((addr (r8152-find-addr)))
    (if (zerop addr)
        (progn (write-string-serial "R8152:NOTFOUND") (write-char-serial 10) 0)
        (let ((state (e1000-state-base)))
          (usb-set-dev-addr addr)
          (usb-set-bulk-in-ep 1)
          (usb-set-bulk-out-ep 2)
          (usb-set-bulk-in-mps 512)
          (usb-set-bulk-out-mps 512)
          (usb-set-bulk-in-toggle 0)
          (usb-set-bulk-out-toggle 0)
          (r8152-read-mac state)
          (setf (mem-ref (+ state #x10) :u32) 0)   ; RX cursor
          (setf (mem-ref (+ state #x14) :u32) 0)   ; TX cursor
          (setf (mem-ref (+ state #x44) :u32) 0)   ; RX pkt len
          (setf (mem-ref (+ state #x18) :u32) #x0F02000A)  ; 10.0.2.15 (DHCP overwrites)
          (setf (mem-ref (+ state #x1C) :u32) #x0202000A)  ; 10.0.2.2
          ;; U-Boot leaves PLA_CR RE|TE set (survives handoff) but RCR clear
          ;; and RXDY gated.  Set the two idempotent "safe" registers — RCR
          ;; accept bits and the RXDY ungate — matching the proven enable.
          ;; No PLA_CR / CRWECR (RE|TE already set; touching it wedges).
          (r8152-set-rcr)
          (r8152-ungate-rxdy)
          ;; Print MAC + addr for diagnostics.
          (write-string-serial "R8152:A") (print-dec addr)
          (write-string-serial " MAC:")
          (print-hex-byte (mem-ref (+ state #x08) :u8)) (write-char-serial 58)
          (print-hex-byte (mem-ref (+ state #x0D) :u8)) (write-char-serial 10)
          ;; Arm the persistent bulk-IN (vendor RX: rx_desc + frame + CRC).
          (dwc2-start-bulk-in 1 addr 1 (cdc-rx-buf-addr) 2048 512)
          1))))

;; ============================================================
;; NIC interface — vendor descriptor framing
;; ============================================================

(defun e1000-send (buf len)
  ;; 8-byte tx_desc {len | TX_FS(1<<31) | TX_LS(1<<30), 0} then the frame.
  (let ((tx (e1000-tx-buf-base)))
    (setf (mem-ref tx :u32) (logior len #xC0000000))
    (setf (mem-ref (+ tx 4) :u32) 0)
    (let ((i 0))
      (loop (when (>= i len) (return nil))
        (setf (mem-ref (+ tx 8 i) :u8) (aref buf i))
        (setq i (+ i 1))))
    (let ((r (dwc2-bulk-transfer 2 (usb-dev-addr) (usb-bulk-out-ep)
                                 0 tx (+ len 8) (usb-bulk-out-mps))))
      (if (eq r 1) 1 0))))

(defun e1000-rx-buf () (+ (cdc-rx-buf-addr) 24))   ; skip the 24-byte rx_desc

(defun e1000-receive ()
  ;; DEFERRED RE-ARM (state+0x48 = rearm-pending): the old code re-armed the
  ;; next bulk-IN into the SAME single rx buffer immediately on completion —
  ;; before the caller copied the frame out.  Back-to-back TCP segments then
  ;; DMA-overwrote the buffer MID-COPY (client KEXINIT cookie bytes replaced
  ;; by later payload text -> wrong I_C -> wrong exchange hash -> OpenSSH
  ;; "incorrect signature").  Re-arming at the START of the NEXT call keeps
  ;; the buffer stable across the caller's whole copy window; USB flow
  ;; control makes it loss-free (un-armed endpoint NAKs, the RTL8153 holds
  ;; frames in its internal FIFO).
  (when (not (zerop (mem-ref (+ (e1000-state-base) #x48) :u32)))
    (setf (mem-ref (+ (e1000-state-base) #x48) :u32) 0)
    (dwc2-start-bulk-in 1 (usb-dev-addr) (usb-bulk-in-ep)
                        (cdc-rx-buf-addr) 2048 (usb-bulk-in-mps)))
  (let ((result (dwc2-poll-bulk-in 1)))
    (if (zerop result)
        0
        (let ((plen (if (eq result 1)
                        (- (logand (mem-ref (cdc-rx-buf-addr) :u32) #x7FFF) 4)
                        0)))
          (setf (mem-ref (+ (e1000-state-base) #x48) :u32) 1)
          (if (and (> plen 0) (< plen 1600))
              (progn (setf (mem-ref (+ (e1000-state-base) #x44) :u32) plen) plen)
              0)))))
