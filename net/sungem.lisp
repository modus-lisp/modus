;;;; sungem.lisp -- Apple's GMAC (Sun GEM) Ethernet, for the bare PowerPC Mac.
;;;;
;;;; Every New World Mac from the G4s on has one: on Uni-N's internal PCI bus
;;;; (a PowerBook's #xF5000000 registers), QEMU mac99's "sungem" on the main bus.
;;;; Package MODUS.NET.SUNGEM implements MODUS.NET's NIC interface -- GEM-SEND /
;;;; -RECEIVE / -RX-BUF / -PROBE -- which the ppcmac image binds at boot
;;;; (mvm/build-cl-repl-common.lisp %NIC-BINDING-TEXT).
;;;;
;;;; References: Linux drivers/net/ethernet/sun/sungem.[ch] (registers, reset,
;;;; GEM-INIT-DMA / -MAC, the descriptor words) and QEMU hw/net/sungem.c.
;;;;
;;;; Where things are: the boot stub (boot/boot-ppcmac.lisp) asks the firmware for
;;;; the "enet" alias and leaves its register PA at +PPCMAC-ENET-PA-SLOT+
;;;; (#x07BFFF00) and its local-mac-address at +4; registers are reached through
;;;; mac-io's device window (VA #x08000000, boot info +52/+56) or, when the GMAC
;;;; is outside it, DBAT7's (VA #x0A000000, boot info +64/+68).  Registers and
;;;; descriptors are little-endian (PCI): PPCMAC-IN32LE / -OUT32LE (lwbrx/stwbrx).
;;;;
;;;; DMA memory (GEM-DMA-BASE, #x07200000, identity-mapped, cache-coherent: Uni-N
;;;; snoops): +0 the TX ring, +#x800 the RX ring (32 descriptors of 16 bytes,
;;;; each ring 2 KB-aligned), +#x1000 32 RX buffers of 2 KB, +#x11000 32 TX
;;;; buffers, +#x30000 the driver's words (RX next, TX next, the register base,
;;;; the RX descriptor lent out by the last GEM-RECEIVE).
;;;;
;;;; GEM-RECEIVE returns a frame IN PLACE: GEM-RX-BUF is its DMA buffer, valid
;;;; until the next GEM-RECEIVE, which gives that descriptor back to the chip.
;;;; The PHY is brought up as Linux does on a Mac (GEM-PHY-UP): Open Firmware's
;;;; QUIESCE leaves it silent on the MII bus.

(in-package :modus.net.sungem)

(defun gem-dma-base () #x07200000)
(defun gem-ring-size () 32)
(defun gem-state () (+ (gem-dma-base) #x30000))
(defun gem-rx-ring () (+ (gem-dma-base) #x800))
(defun gem-rx-bufs () (+ (gem-dma-base) #x1000))
(defun gem-tx-bufs () (+ (gem-dma-base) #x11000))

(defun gem-regs () (mem-ref (+ (gem-state) 8) :u32))

(defun gem-rd (off) (modus-int::ppcmac-in32le (+ (gem-regs) off)))
(defun gem-wr (off v) (modus-int::ppcmac-out32le (+ (gem-regs) off) v))
;; a little-endian word in DMA memory (a descriptor's)
(defun gem-le! (addr v) (modus-int::ppcmac-out32le addr v))
(defun gem-le (addr) (modus-int::ppcmac-in32le addr))

(defun gem-spin (n) (dotimes (i n) (mem-ref (gem-state) :u8)))

(defun gem-dt-scan ()
  "Find the first node whose device_type is \"network\" in the device tree the
   stub copied (#x07B00000, boot-ppcmac.lisp +PPCMAC-DT+: 1 a node begins, 2 a
   property -- name in 32 bytes at +4, length +36, kept +40, data +44 -- 3 a
   node ends, 9 the end) and leave its register PA and MAC where the stub puts
   the \"enet\" alias's (#x07BFFF00, +4): the firmware has no such alias (QEMU's).
   A node's properties come before its children, so it is judged at the next
   1 or 3."
  (let ((a #x07B00004) (net nil) (pa 0) (mac 0))
    (when (= (mem-ref #x07B00000 :u16) #x4454)                ; \"DT\"
      (loop
        (let ((k (mem-ref a :u32)))
          (cond ((or (= k 1) (= k 3) (= k 9) (> a #x07BE0000))
                 (when (and net (> pa 0))
                   (setf (mem-ref #x07BFFF00 :u32) pa)
                   (when (> mac 0)
                     (dotimes (i 6) (setf (mem-ref (+ #x07BFFF04 i) :u8) (mem-ref (+ mac i) :u8))))
                   (return pa))
                 (when (or (= k 9) (> a #x07BE0000)) (return 0))
                 (setq net nil pa 0 mac 0)
                 (setq a (+ a 4)))
                ((= k 2)
                 (let* ((name (+ a 4)) (kept (mem-ref (+ a 40) :u32)) (data (+ a 44)))
                   (flet ((is (str)
                            (and (dotimes (i (length str) t)
                                   (unless (= (mem-ref (+ name i) :u8) (char-code (char str i))) (return nil)))
                                 (zerop (mem-ref (+ name (length str)) :u8)))))
                     (cond ((is "device_type")
                            (setq net (and (>= kept 7) (= (mem-ref data :u8) 110) (= (mem-ref (+ data 1) :u8) 101)
                                           (= (mem-ref (+ data 2) :u8) 116) (= (mem-ref (+ data 3) :u8) 119))))
                           ((is "local-mac-address") (when (>= kept 6) (setq mac data)))
                           ((is "assigned-addresses")
                            (let ((e data) (left kept))
                              (loop while (and (zerop pa) (>= left 20))
                                    do (when (= (mod (floor (mem-ref e :u32) #x1000000) 4) 2)
                                         (setq pa (mem-ref (+ e 8) :u32)))
                                       (setq e (+ e 20) left (- left 20)))))))
                   (setq a (+ data (* 4 (floor (+ kept 3) 4))))))
                (t (return 0))))))))

(defun gem-find-regs ()
  "The VA of the GMAC's registers, or 0: its PA from the boot stub (or the
   device tree), through whichever window holds it."
  (when (zerop (mem-ref #x07BFFF00 :u32)) (gem-dt-scan))
  (let ((pa (mem-ref #x07BFFF00 :u32))
        (ebase (mem-ref #x07F80040 :u32)) (esize (mem-ref #x07F80044 :u32))
        (dbase (mem-ref #x07F80034 :u32)) (dsize (mem-ref #x07F80038 :u32)))
    (cond ((zerop pa) 0)
          ((and (> esize 0) (>= pa ebase) (< (- pa ebase) esize)) (+ #x0A000000 (- pa ebase)))
          ((and (> dsize 0) (>= pa dbase) (< (- pa dbase) dsize)) (+ #x08000000 (- pa dbase)))
          (t 0))))

(defun gem-mac-byte (i) (mem-ref (+ (e1000-state-base) 8 i) :u8))

(defun gem-take-mac ()
  "The MAC into the NIC state (+8..+13, where ip.lisp reads it): the firmware's
   local-mac-address, else what the firmware left in MAC_ADDR0..2, else a
   locally administered 02:00:00:00:00:09."
  (let ((st (e1000-state-base)) (any 0))
    (dotimes (i 6)
      (let ((b (mem-ref (+ #x07BFFF04 i) :u8)))
        (setf (mem-ref (+ st 8 i) :u8) b)
        (setq any (logior any b))))
    (when (zerop any)
      (let ((a2 (gem-rd #x6088)) (a1 (gem-rd #x6084)) (a0 (gem-rd #x6080)))
        (setf (mem-ref (+ st 8) :u8) (logand (ash a2 -8) 255))
        (setf (mem-ref (+ st 9) :u8) (logand a2 255))
        (setf (mem-ref (+ st 10) :u8) (logand (ash a1 -8) 255))
        (setf (mem-ref (+ st 11) :u8) (logand a1 255))
        (setf (mem-ref (+ st 12) :u8) (logand (ash a0 -8) 255))
        (setf (mem-ref (+ st 13) :u8) (logand a0 255))
        (setq any (logior a0 a1 a2))))
    (when (zerop any)
      (dotimes (i 6) (setf (mem-ref (+ st 8 i) :u8) (if (= i 0) 2 (if (= i 5) 9 0)))))))

(defun gem-ms (n)
  "Wait N milliseconds (the timebase, through GET-INTERNAL-REAL-TIME)."
  (let ((end (+ (get-internal-real-time) (* n (floor internal-time-units-per-second 1000)) 1)))
    (loop while (< (get-internal-real-time) end))))

;;; ---- the PHY, through the MIF in frame mode (Linux's __sungem_phy_read/write) ----
(defun gem-phy-rd (a g)
  (gem-wr #x620C (logior #x60020000 (ash a 23) (ash g 18)))       ; ST, read, TA MSB
  (dotimes (i 1000) (when (logtest (gem-rd #x620C) #x10000) (return nil)))
  (logand (gem-rd #x620C) #xFFFF))

(defun gem-phy-wr (a g v)
  (gem-wr #x620C (logior #x50020000 (ash a 23) (ash g 18) (logand v #xFFFF)))
  (dotimes (i 1000) (when (logtest (gem-rd #x620C) #x10000) (return nil))))

(defun gem-phy-up ()
  "The PHY, as Linux brings it up on a Mac: reset by KeyLargo's GPIO
   (core99_gmac_phy_reset: KL_GPIO_ETH_PHY_RESET, mac-io +#x7A -- the
   firmware's QUIESCE leaves it silent), the MII bus MDIO1 when present
   (gem_check_invariants), the first address that answers, a BMCR reset,
   autonegotiation, and up to 5 s for the link.  T for full duplex.  Kept at
   (GEM-STATE) +20: the address (#xFF none), +24 BMSR, +28 the partner's
   abilities, +32 MIF_CFG."
  (let ((io (mem-ref #x07F80018 :u32)) (st (gem-state)) (addr #xFF))
    (when (> io 0)
      (setf (mem-ref (+ io #x7A) :u8) 4)                          ; output, low: reset
      (gem-ms 10)
      (setf (mem-ref (+ io #x7A) :u8) 1)                          ; released
      (gem-ms 20))
    (let ((cfg (logand (gem-rd #x6210) (lognot 7))))              ; frame mode, no poll
      (gem-wr #x6210 (if (logtest cfg #x200) (logior cfg 1) cfg))
      (setf (mem-ref (+ st 32) :u32) (gem-rd #x6210)))
    (dotimes (a 32)
      (let ((b (gem-phy-rd a 1)))
        (when (and (/= b #xFFFF) (/= b 0)) (setq addr a) (return nil))))
    (setf (mem-ref (+ st 20) :u32) addr)
    (if (= addr #xFF)
        t
        (progn
          (gem-phy-wr addr 0 #x8000)                                ; BMCR reset
          (dotimes (i 50) (gem-ms 10) (unless (logtest (gem-phy-rd addr 0) #x8000) (return nil)))
          (gem-phy-wr addr 4 #x01E1)                                ; advertise 10/100, half/full
          (gem-phy-wr addr 0 #x1200)                                ; autonegotiate, restart
          (dotimes (i 50)
            (gem-ms 100)
            (gem-phy-rd addr 1)                                     ; BMSR's link bit latches low
            (when (logtest (gem-phy-rd addr 1) #x24) (when (logtest (gem-phy-rd addr 1) 4) (return nil))))
          (let ((bmsr (gem-phy-rd addr 1)) (lpa (gem-phy-rd addr 5)))
            (setf (mem-ref (+ st 24) :u32) bmsr)
            (setf (mem-ref (+ st 28) :u32) lpa)
            ;; full duplex when both ends offer it at the speed they share
            (or (logtest lpa #x0100) (and (not (logtest lpa #x0080)) (logtest lpa #x0040))
                (zerop lpa)))))))

(defun gem-rx-fresh (i)
  "Give RX descriptor I to the chip: its 2 KB buffer, OWN."
  (let ((d (+ (gem-rx-ring) (* i 16))))
    (gem-le! (+ d 8) (+ (gem-rx-bufs) (* i #x800)))
    (gem-le! (+ d 12) 0)
    (gem-le! (+ d 4) 0)
    ;; status word last: BUFSZ (bits 16..30) and OWN (bit 31)
    (gem-le! d (logior (ash #x800 16) #x80000000))))

(defun gem-probe ()
  "Find, reset and start the GMAC.  1 when it is running, 0 when there is none."
  (let ((va (gem-find-regs)))
    (setf (mem-ref (+ (gem-state) 8) :u32) va)
    (if (zerop va)
        (progn (write-string-serial "GEM: none") (write-char-serial 10) 0)
        (progn
          (gem-take-mac)
          ;; reset: interrupts masked, TX and RX reset, until both bits clear
          (gem-wr #x0010 #xFFFFFFFF)
          (gem-wr #x1010 3)
          (dotimes (i 1000)
            (when (zerop (logand (gem-rd #x1010) 3)) (return nil))
            (gem-spin 200))
          ;; the rings: TX empty, RX all the chip's
          (dotimes (i 128) (setf (mem-ref (+ (gem-dma-base) (* i 4)) :u32) 0))
          (dotimes (i (gem-ring-size)) (gem-rx-fresh i))
          (setf (mem-ref (gem-state) :u32) 0)                 ; RX next
          (setf (mem-ref (+ (gem-state) 4) :u32) 0)           ; TX next
          (setf (mem-ref (+ (gem-state) 12) :u32) #xFFFF)     ; no RX lent
          ;; DMA: 32-entry rings, TX FIFO threshold #x7FF, PMODE; RX frames from
          ;; byte 0, checksum start past the Ethernet header, 128-byte threshold
          (gem-wr #x2004 (logior (ash #x7FF 10) #x200000))
          (gem-wr #x200C 0) (gem-wr #x2008 (gem-dma-base))
          (gem-wr #x2000 0)
          (gem-wr #x4000 (logior (ash 14 13) #x01000000))
          (gem-wr #x4008 0) (gem-wr #x4004 (gem-rx-ring))
          (gem-wr #x4100 (- (gem-ring-size) 4))
          (gem-wr #x4020 0) (gem-wr #x4108 0)
          ;; the MAC (GEM-INIT-MAC)
          (gem-wr #x6008 #x1BF0)
          (gem-wr #x6040 0) (gem-wr #x6044 8) (gem-wr #x6048 4)
          (gem-wr #x604C 64) (gem-wr #x6050 64)
          (gem-wr #x6054 (logior #x20000000 1522))
          (gem-wr #x6058 7) (gem-wr #x605C 4) (gem-wr #x6060 16) (gem-wr #x6064 #x8808)
          (gem-wr #x6130 (logand (logior (gem-mac-byte 5) (ash (gem-mac-byte 4) 8)) #x3FF))
          (gem-wr #x6080 (logior (ash (gem-mac-byte 4) 8) (gem-mac-byte 5)))
          (gem-wr #x6084 (logior (ash (gem-mac-byte 2) 8) (gem-mac-byte 3)))
          (gem-wr #x6088 (logior (ash (gem-mac-byte 0) 8) (gem-mac-byte 1)))
          (gem-wr #x608C 0) (gem-wr #x6090 0) (gem-wr #x6094 0)
          (gem-wr #x6098 1) (gem-wr #x609C #xC200) (gem-wr #x60A0 #x0180)
          (dotimes (i 5) (gem-wr (+ #x60A4 (* i 4)) 0))
          (dotimes (i 16) (gem-wr (+ #x60C0 (* i 4)) #xFFFF))
          (gem-wr #x6020 #xFFFF) (gem-wr #x6024 #xFFFF) (gem-wr #x6028 #xFFFFFFFF)
          ;; the link: the PHY up, and the MAC told its duplex (GEM-SET-LINK-MODES)
          (gem-wr #x6038 0)
          (if (gem-phy-up)
              (progn (gem-wr #x6030 (logior #x08 #x10 #x02 #x04))   ; EIPG0 NGU ICS ICOLL
                     (gem-wr #x603C (logior #x01 #x20 #x40))         ; OE LLED FLED
                     (gem-wr #x604C 64))
              (progn (gem-wr #x6030 (logior #x08 #x10))             ; half: EIPG0 NGU
                     (gem-wr #x603C (logior #x01 #x20 #x04))         ; OE LLED DISE
                     (gem-wr #x604C 512)))
          ;; go: DMA, then the MACs (RX: strip FCS, promiscuous -- a point-to-point
          ;; link, and broadcast for ARP and DHCP without the hash filter)
          (gem-wr #x2004 (logior (gem-rd #x2004) 1))
          (gem-wr #x4000 (logior (gem-rd #x4000) 1))
          (gem-wr #x6030 (logior (gem-rd #x6030) 1))
          (gem-wr #x6034 (logior 1 4 8))
          (gem-spin 2000)
          (gem-wr #x4100 (- (gem-ring-size) 4))
          (write-string-serial "GEM: up, MAC ")
          (dotimes (i 6)
            (let ((b (gem-mac-byte i)))
              (write-char-serial (char-code (char "0123456789abcdef" (ash b -4))))
              (write-char-serial (char-code (char "0123456789abcdef" (logand b 15))))
              (when (< i 5) (write-char-serial 58))))
          (write-char-serial 10)
          1))))

(defun gem-send (arr len)
  "Frame ARR[0..LEN) out of the next TX descriptor; wait for the chip to take it."
  (let* ((i (mem-ref (+ (gem-state) 4) :u32))
         (buf (+ (gem-tx-bufs) (* i #x800)))
         (n (if (< len 60) 60 len))
         (d (+ (gem-dma-base) (* i 16)))
         (next (if (< (+ i 1) (gem-ring-size)) (+ i 1) 0)))
    (dotimes (k len) (setf (mem-ref (+ buf k) :u8) (aref arr k)))
    (when (< len 60) (dotimes (k (- 60 len)) (setf (mem-ref (+ buf len k) :u8) 0)))
    (gem-le! (+ d 8) buf)
    (gem-le! (+ d 12) 0)
    (gem-le! (+ d 4) 0)
    (gem-le! d (logior n #xC0000000))                          ; SOF | EOF | size
    (setf (mem-ref (+ (gem-state) 4) :u32) next)
    (gem-wr #x2000 next)
    ;; TXDMA_TXDONE catches up with the kick
    (dotimes (k 20000)
      (when (= (gem-rd #x2100) next) (return 1))
      (gem-spin 10))
    1))

(defun gem-receive ()
  "The next frame's length, 0 when none; it is at GEM-RX-BUF."
  (let ((lent (mem-ref (+ (gem-state) 12) :u32)))
    ;; the frame returned last time goes back to the chip, and the kick follows
    ;; the consumer: everything but the descriptors not yet read is the chip's
    ;; (the kick a multiple of 4, as the chip wants)
    (unless (= lent #xFFFF)
      (gem-rx-fresh lent)
      (setf (mem-ref (+ (gem-state) 12) :u32) #xFFFF)
      (let ((c (if (< (+ lent 1) (gem-ring-size)) (+ lent 1) 0)))
        (gem-wr #x4100 (logand (if (zerop c) (- (gem-ring-size) 1) (- c 1)) (lognot 3))))))
  (let* ((i (mem-ref (gem-state) :u32))
         (d (+ (gem-rx-ring) (* i 16)))
         (st (gem-le d)))
    (if (logtest st #x80000000)
        0                                                      ; still the chip's
        (let ((len (logand (ash st -16) #x7FFF))
              (next (if (< (+ i 1) (gem-ring-size)) (+ i 1) 0)))
          (setf (mem-ref (gem-state) :u32) next)
          (setf (mem-ref (+ (gem-state) 12) :u32) i)
          (setf (mem-ref (+ (gem-state) 16) :u32) (+ (gem-rx-bufs) (* i #x800)))
          ;; a frame with a bad CRC (status bit 62): dropped
          (if (logtest (gem-le (+ d 4)) #x40000000) 0 len)))))

(defun gem-rx-buf () (mem-ref (+ (gem-state) 16) :u32))
