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

;; Whole-dword vendor write; TYPE is #x0100 (PLA) or #x0000 (USB).
(defun r8152-write-dword (addr type v)
  (setf (mem-ref (r8152-reg-scratch) :u32) v)
  (let ((r (usb-control-transfer (usb-dev-addr) #x40 5 addr (logior type #xFF)
                                 (r8152-reg-scratch) 4)))
    (if (> r 0) 1 0)))

(defun r8152-read-dword-type (addr type)
  (let ((r (usb-control-transfer (usb-dev-addr) #xC0 5 addr type
                                 (r8152-reg-scratch) 4)))
    (if (<= r 0) -1 (mem-ref (r8152-reg-scratch) :u32))))

;; RX AGGREGATION.  The RTL8153 packs frames into one bulk-IN transfer as
;; [rx_desc 24 B][frame + CRC], each record starting on an 8-byte boundary,
;; filling up to the buffer size it is told the host has (USB_RX_EARLY_SIZE)
;; or until USB_RX_EARLY_TIMEOUT passes.  U-Boot programs it for a 2 KB
;; buffer, and a driver that read ONE frame per transfer lost everything
;; queued behind the first frame: full-size TCP segments simply never arrived.
;; So: a 16 KB buffer, programmed as Linux programs an RTL8153 at USB 2.0
;; (r8153_set_rx_early_size / _timeout), and R8152-RECEIVE walks every frame
;; of a transfer before re-arming.
;;   USB_USB_CTRL   (0xd406, upper half of 0xd404): RX_AGG_DISABLE 0x10 clear
;;   USB_RX_EARLY_TIMEOUT (0xd42c) = 250000 ns / 8 (COALESCE_HIGH)
;;   USB_RX_EARLY_SIZE    (0xd42e) = (16384 - rx_reserved 1554) / 4
;;   PLA_RMS        (0xc016, upper half of 0xc014): 1522 (mtu + VLAN hdr + FCS;
;;     U-Boot's 1518 drops a full 1514-byte frame -- measured).
(defun r8152-agg-size () 16384)

;; The aggregate buffer is CACHEABLE RAM, not the Device-mapped USB DMA window
;; at 0x11000000: every load from Device memory is its own uncached bus read,
;; and copying a 1460-byte payload out of it cost ~85 us per segment -- 42% of
;; a 4 MB fetch (instrumented).  0x11400000 is ordinary identity-mapped
;; Normal-WB DRAM nothing else uses (the window is one 2 MB block; the SSH map
;; starts at 0x12000000).  The DWC2's DMA is NOT cache-coherent, so after each
;; transfer completes, the bytes it delivered are cleaned+invalidated (DC CIVAC)
;; before the CPU reads them; the CPU never writes the buffer, so no dirty line
;; can be evicted over incoming DMA.
(defun r8152-agg-buf () #x11400000)
(defun r8152-meta () (+ (r8152-agg-buf) (r8152-agg-size)))   ; +0 code page

(defun r8152-u64 (a v)
  (setf (mem-ref a :u32) (logand v #xFFFFFFFF))
  (setf (mem-ref (+ a 4) :u32) (logand (ash v -32) #xFFFFFFFF)))

(defun r8152-cache-init ()
  ;; One exec page: DC CIVAC over [scratch+0] for [scratch+8] bytes; scratch
  ;; at +1536.  ldr x0,[x3]; ldr x1,[x3,#8]; L: dc civac,x0; add x0,x0,#64;
  ;; subs x1,x1,#64; b.ne L; dsb sy; ret.  (net/hdmi-console's encoding.)
  (let* ((code (%mmap-exec-page 4096)) (scr (+ code 1536)) (p code))
    (dolist (w (list (logior #xD2800003 (ash (logand scr #xFFFF) 5))
                     (logior #xF2A00003 (ash (logand (ash scr -16) #xFFFF) 5))
                     #xF9400060 #xF9400461 #xD50B7E20 #x91010000 #xF1010021
                     #x54FFFFA1 #xD5033F9F #xD65F03C0))
      (setf (mem-ref p :u32) w) (setq p (+ p 4)))
    (%jit-icache-flush code 64)
    (r8152-u64 (r8152-meta) code)
    code))

(defun r8152-dcache-civac (addr bytes)
  ;; ADDR 64-aligned; BYTES rounded up to whole lines.
  (let* ((code (mem-ref (r8152-meta) :u32)) (scr (+ code 1536)))
    (r8152-u64 scr addr)
    (r8152-u64 (+ scr 8) (logand (+ (max bytes 64) 63) (lognot 63)))
    (%jit-call code)))

(defun r8152-rx-config ()
  (let ((ctrl (r8152-read-dword-type #xd404 0))
        (rms (r8152-read-dword #xc014)))
    (when (>= ctrl 0)
      (r8152-write-dword #xd404 0 (logand ctrl (logxor (ash #x10 16) #xFFFFFFFF))))
    (r8152-write-dword #xd42c 0 (logior (ash (floor (- (r8152-agg-size) 1554) 4) 16)
                                        (floor 250000 8)))
    (when (>= rms 0)
      (r8152-write-dword #xc014 #x100 (logior (logand rms #xFFFF) (ash 1522 16))))))

;; ============================================================
;; Probe: adopt U-Boot's running device (no init, no enumerate)
;; ============================================================

;; Thin e1000-* forwarders (see net/cdc-ether.lisp): keep last-defun-wins
;; behaviour for images without net/usb-netdev.lisp; shadowed by the dispatcher
;; where usb-netdev.lisp is loaded.
(defun e1000-send (buf len) (r8152-send buf len))
(defun e1000-receive () (r8152-receive))
(defun e1000-rx-buf () (r8152-rx-buf))
(defun e1000-probe () (r8152-probe))

(defun r8152-probe ()
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
          (r8152-rx-config)
          ;; Print MAC + addr for diagnostics.
          (write-string-serial "R8152:A") (print-dec addr)
          (write-string-serial " MAC:")
          (print-hex-byte (mem-ref (+ state #x08) :u8)) (write-char-serial 58)
          (print-hex-byte (mem-ref (+ state #x0D) :u8)) (write-char-serial 10)
          ;; Arm the persistent bulk-IN (vendor RX: rx_desc + frame + CRC).
          (setf (mem-ref (+ state #x48) :u32) 0)
          (r8152-cache-init)
          (r8152-dcache-civac (r8152-agg-buf) (r8152-agg-size))
          (dwc2-start-bulk-in 1 addr 1 (r8152-agg-buf) (r8152-agg-size) 512)
          1))))

;; ============================================================
;; NIC interface — vendor descriptor framing
;; ============================================================

(defun r8152-send (buf len)
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

(defun r8152-rx-buf ()
  ;; The CURRENT frame: past its 24-byte rx_desc, at offset state+0x54 of the
  ;; aggregate.
  (+ (r8152-agg-buf) (mem-ref (+ (e1000-state-base) #x54) :u32) 24))

;; Aggregate state in the NIC block: +0x48 = 1 while a completed transfer is
;; being walked (the channel is NOT armed, so the buffer cannot change under
;; the caller's copy -- the old deferred re-arm, now per transfer), +0x54 =
;; offset of the frame last returned, +0x78 = bytes the transfer delivered.
(defun r8152-frame-len (off)
  "Frame length (CRC excluded) of the record at OFF, or 0 if none is valid."
  (let ((plen (- (logand (mem-ref (+ (r8152-agg-buf) off) :u32) #x7FFF) 4)))
    (if (and (> plen 0) (< plen 1600)
             (<= (+ off 24 plen) (mem-ref (+ (e1000-state-base) #x78) :u32)))
        plen 0)))

(defun r8152-rearm ()
  (setf (mem-ref (+ (e1000-state-base) #x48) :u32) 0)
  (dwc2-start-bulk-in 1 (usb-dev-addr) (usb-bulk-in-ep)
                      (r8152-agg-buf) (r8152-agg-size) (usb-bulk-in-mps)))

;; Next frame of the aggregate being walked, or 0 (re-arming the channel
;; when the walk is over).
(defun r8152-next-in-aggregate (st)
  (if (zerop (mem-ref (+ st #x48) :u32))
      0
      (let* ((cur (mem-ref (+ st #x54) :u32))
             (clen (r8152-frame-len cur))
             (next (logand (+ cur 24 clen 4 7) (lognot 7)))
             (nlen (if (and (> clen 0) (< (+ next 24) (mem-ref (+ st #x78) :u32)))
                       (r8152-frame-len next) 0)))
        (if (> nlen 0)
            (progn (setf (mem-ref (+ st #x54) :u32) next)
                   (setf (mem-ref (+ st #x44) :u32) nlen)
                   nlen)
            (progn (r8152-rearm) 0)))))

;; A newly completed transfer: its first frame, or 0.
(defun r8152-poll-new (st)
  (let ((result (dwc2-poll-bulk-in 1)))
    (cond
      ((zerop result) 0)
      ((not (eq result 1)) (r8152-rearm) 0)
      (t
       (setf (mem-ref (+ st #x78) :u32)
             (- (r8152-agg-size) (logand (dwc2-read (dwc2-hctsiz 1)) #x7FFFF)))
       (r8152-dcache-civac (r8152-agg-buf) (mem-ref (+ st #x78) :u32))
       (setf (mem-ref (+ st #x54) :u32) 0)
       (setf (mem-ref (+ st #x48) :u32) 1)
       (let ((plen (r8152-frame-len 0)))
         (setf (mem-ref (+ st #x44) :u32) plen)
         (when (zerop plen) (r8152-rearm))
         plen)))))

(defun r8152-receive ()
  (let* ((st (e1000-state-base))
         (n (r8152-next-in-aggregate st)))
    (if (> n 0) n (r8152-poll-new st))))
