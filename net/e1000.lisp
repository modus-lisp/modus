;;;; e1000.lisp - Intel E1000 NIC driver (shared, arch-independent)
;;;;
;;;; Internal functions use e1000-hw-* names to allow driver dispatch.
;;;; Default wrappers at the end provide the e1000-* API.
;;;; For dual-driver builds, a dispatch layer loaded after this file
;;;; overrides the wrappers via last-defun-wins.
;;;;
;;;; Requires arch adapter defining:
;;;;   pci-config-read, pci-config-write, io-delay
;;;;   e1000-state-base, e1000-rx-desc-base, e1000-rx-buf-base
;;;;   e1000-tx-desc-base, e1000-tx-buf-base

;; Scan PCI bus 0 for E1000 (vendor=8086 device=100E).
;; Store MMIO base at state-base+0. Returns MMIO base or 0.
(defun pci-find-e1000 ()
  (let ((found 0))
    (dotimes (dev 32)
      (let ((id (pci-config-read 0 dev 0 0)))
        (when (eq id #x100E8086)
          ;; Enable PCI Bus Mastering + Memory Space + I/O Space
          (let ((cmd (pci-config-read 0 dev 0 4)))
            (pci-config-write 0 dev 0 4 (logior cmd 7)))
          ;; Read BAR0 (register 0x10)
          (let ((bar0 (pci-config-read 0 dev 0 #x10)))
            (setq found (logand bar0 #xFFFFFFF0))
            (setf (mem-ref (e1000-state-base) :u64) found)))))
    found))

;; Write to E1000 MMIO register
(defun e1000-write-reg (reg val)
  (setf (mem-ref (+ (mem-ref (e1000-state-base) :u64) reg) :u32) val))

;; Read E1000 MMIO register
(defun e1000-read-reg (reg)
  (mem-ref (+ (mem-ref (e1000-state-base) :u64) reg) :u32))

;; Read 16-bit word from E1000 EEPROM
(defun e1000-read-eeprom (addr)
  (let ((mmio (mem-ref (e1000-state-base) :u64)))
    ;; Write EERD: start bit | (addr << 8)
    (setf (mem-ref (+ mmio #x14) :u32) (logior 1 (ash addr 8)))
    ;; Poll for done (bit 4)
    (let ((result 0))
      (dotimes (try 10000)
        (let ((val (mem-ref (+ mmio #x14) :u32)))
          (when (not (zerop (logand val 16)))
            (setq result (logand (ash val -16) #xFFFF))
            (setq try 10001))))
      result)))

;; RX ring depth, as a DEFUN so an arch adapter can override it by
;; last-defun-wins (a defvar initform would not run in-image, Limitation 7).
;;
;; WHY THIS IS TUNABLE.  The ring is the ONLY buffering between the wire and a
;; guest that can pause (a GC mid-transfer is exactly such a pause).  Measured
;; 2026-08-31 on QEMU virt: with 128 descriptors the ring holds 128*2048 =
;; 262144 bytes, and the ONLY HTTP body that ever truncated was the only one
;; larger than that -- 348160 bytes, while 194560 and 9287 always arrived whole.
;; Under ring capacity the NIC absorbs the whole response across any stall; over
;; it, a stall drops packets, TCP retransmits, and the peer closes.
;; Default stays 128 so every other arch keeps its existing DMA layout; an arch
;; that raises it MUST also move e1000-tx-desc-base/e1000-tx-buf-base clear of
;; the larger rx-buf region.
(defun e1000-rx-ring-count () 128)

;; Initialize E1000 RX descriptors
;; (e1000-rx-ring-count) descriptors, buffers at rx-buf-base (each 2048 bytes)
(defun e1000-init-rx ()
  (let ((rx-desc (e1000-rx-desc-base))
        (rx-buf (e1000-rx-buf-base)))
    (dotimes (i (e1000-rx-ring-count))
      (let ((desc-addr (+ rx-desc (* i 16)))
            (buf-addr (+ rx-buf (* i 2048))))
        ;; Buffer address low 32 bits
        (setf (mem-ref desc-addr :u32) buf-addr)
        ;; Buffer address high 32 bits = 0
        (setf (mem-ref (+ desc-addr 4) :u32) 0)
        ;; Clear status/length fields
        (setf (mem-ref (+ desc-addr 8) :u32) 0)
        (setf (mem-ref (+ desc-addr 12) :u32) 0)))
    ;; Set RX descriptor ring registers
    (e1000-write-reg #x2800 rx-desc)   ; RDBAL
    (e1000-write-reg #x2804 0)         ; RDBAH
    (e1000-write-reg #x2808 (* (e1000-rx-ring-count) 16))  ; RDLEN
    (e1000-write-reg #x2810 0)         ; RDH (head)
    (e1000-write-reg #x2818 (- (e1000-rx-ring-count) 1))    ; RDT (tail)
    ;; Store RX cursor = 0
    (setf (mem-ref (+ (e1000-state-base) #x10) :u32) 0)))

;; Initialize E1000 TX descriptors
;; 64 descriptors, buffers at tx-buf-base (each 1536 bytes)
(defun e1000-init-tx ()
  (let ((tx-desc (e1000-tx-desc-base))
        (tx-buf (e1000-tx-buf-base)))
    (dotimes (i 64)
      (let ((desc-addr (+ tx-desc (* i 16)))
            (buf-addr (+ tx-buf (* i 1536))))
        ;; Buffer address low 32 bits
        (setf (mem-ref desc-addr :u32) buf-addr)
        ;; Buffer address high 32 bits = 0
        (setf (mem-ref (+ desc-addr 4) :u32) 0)
        ;; Clear cmd/status fields
        (setf (mem-ref (+ desc-addr 8) :u32) 0)
        (setf (mem-ref (+ desc-addr 12) :u32) 0)))
    ;; Set TX descriptor ring registers
    (e1000-write-reg #x3800 tx-desc)   ; TDBAL
    (e1000-write-reg #x3804 0)         ; TDBAH
    (e1000-write-reg #x3808 1024)      ; TDLEN (64 * 16)
    (e1000-write-reg #x3810 0)         ; TDH (head)
    (e1000-write-reg #x3818 0)         ; TDT (tail)
    ;; Store TX cursor = 0
    (setf (mem-ref (+ (e1000-state-base) #x14) :u32) 0)))

;; Full E1000 initialization
(defun e1000-init ()
  (let ((mmio (mem-ref (e1000-state-base) :u64))
        (state (e1000-state-base)))
    (when (zerop mmio)
      ;; "E1000:No" + newline
      (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
      (%serial-byte 48) (%serial-byte 58) (%serial-byte 78) (%serial-byte 111)
      (%serial-byte 10)
      (return 0))

    ;; 1. Reset: write CTRL.RST (bit 26)
    (e1000-write-reg 0 #x04000000)
    ;; Wait for reset to clear
    (dotimes (i 10000)
      (when (zerop (logand (e1000-read-reg 0) #x04000000))
        (setq i 10001)))

    ;; 2. Read MAC from EEPROM
    (let ((mac0 (e1000-read-eeprom 0))
          (mac1 (e1000-read-eeprom 1))
          (mac2 (e1000-read-eeprom 2)))
      ;; Store MAC bytes at state+0x08
      (setf (mem-ref (+ state #x08) :u8) (logand mac0 #xFF))
      (setf (mem-ref (+ state #x09) :u8) (logand (ash mac0 -8) #xFF))
      (setf (mem-ref (+ state #x0A) :u8) (logand mac1 #xFF))
      (setf (mem-ref (+ state #x0B) :u8) (logand (ash mac1 -8) #xFF))
      (setf (mem-ref (+ state #x0C) :u8) (logand mac2 #xFF))
      (setf (mem-ref (+ state #x0D) :u8) (logand (ash mac2 -8) #xFF))

      ;; 3. Set MAC in RAL0/RAH0
      (e1000-write-reg #x5400 (logior mac0 (ash mac1 16)))
      (e1000-write-reg #x5404 (logior mac2 #x80000000))  ; AV bit

      ;; Print "MAC:" then hex bytes
      (%serial-byte 77) (%serial-byte 65) (%serial-byte 67) (%serial-byte 58)
      (print-hex-byte (logand mac0 #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand (ash mac0 -8) #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand mac1 #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand (ash mac1 -8) #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand mac2 #xFF))
      (%serial-byte 58)
      (print-hex-byte (logand (ash mac2 -8) #xFF))
      (%serial-byte 10))

    ;; 4. Clear multicast table (128 dwords at 0x5200)
    (dotimes (i 128)
      (e1000-write-reg (+ #x5200 (* i 4)) 0))

    ;; 5. Setup RX ring
    (e1000-init-rx)

    ;; 6. Setup TX ring
    (e1000-init-tx)

    ;; 7. Init link: CTRL = SLU (bit 6) | ASDE (bit 5)
    (e1000-write-reg 0 (logior #x40 #x20))

    ;; 8. Enable RX: RCTL = EN | BAM | BSIZE_2048 | SECRC
    (e1000-write-reg #x100 (logior 2 (logior #x8000 #x04000000)))

    ;; 9. Enable TX: TCTL = EN | PSP | CT=0x0F | COLD=0x40
    (e1000-write-reg #x400 (logior 2 (logior 8 (logior #xF0 #x40000))))

    ;; 10. Set TIPG
    (e1000-write-reg #x410 (logior 10 (logior (ash 8 10) (ash 6 20))))

    ;; 11. TXDCTL: Queue Enable (bit 25)
    (e1000-write-reg #x3828 (ash 1 25))

    ;; 12. Clear TX delay registers
    (e1000-write-reg #x3820 0)   ; TIDV
    (e1000-write-reg #x382C 0)   ; TADV

    ;; Enable RXT0 interrupt (bit 7) — RX timer fires on packet receive.
    ;; IRQs stay masked at CPU (PSTATE.I=1 / EFLAGS.IF=0) so no ISR runs,
    ;; but the pending interrupt signal wakes HLT/WFI in io-delay.
    ;; Clear all first, then set RXT0.
    (e1000-write-reg #xD8 #xFFFFFFFF)  ; IMC: clear all
    (e1000-write-reg #xD0 #x80)        ; IMS: enable RXT0 (bit 7)

    ;; Start UNCONFIGURED (0.0.0.0) — DHCP must supply the address.
    ;;
    ;; This used to preload 10.0.2.15 / 10.0.2.2 with the comment "will be
    ;; overwritten by DHCP".  Those are exactly slirp's first lease and its
    ;; gateway, so when DHCP FAILED the stale defaults still routed under QEMU
    ;; and the run looked like a success — the one failure the network bring-up
    ;; most needs to report was the one it could not show.  A value that
    ;; happens to match the expected answer is not evidence the mechanism ran.
    ;;
    ;; 0.0.0.0 is also the correct source address for a DHCP DISCOVER, so this
    ;; is what an unconfigured interface should hold anyway.  Anything needing
    ;; a static address must now set it explicitly and visibly.
    (setf (mem-ref (+ state #x18) :u32) 0)
    (setf (mem-ref (+ state #x1C) :u32) 0)

    ;; "E1000:OK" + newline
    (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
    (%serial-byte 48) (%serial-byte 58) (%serial-byte 79) (%serial-byte 75)
    (%serial-byte 10)
    1))

;; Send raw ethernet frame from byte array
(defun e1000-hw-send (buf len)
  (let ((state (e1000-state-base))
        (tx-desc (e1000-tx-desc-base))
        (tx-buf (e1000-tx-buf-base)))
    (let ((tx-cur (mem-ref (+ state #x14) :u32)))
      (let ((desc-addr (+ tx-desc (* tx-cur 16)))
            (buf-addr (+ tx-buf (* tx-cur 1536))))
        ;; Copy data to TX buffer
        (dotimes (i len)
          (setf (mem-ref (+ buf-addr i) :u8) (aref buf i)))
        ;; Set descriptor: length in low 16 bits of +8, cmd in byte at +11
        ;; cmd = EOP(1) | IFCS(2) | RS(8) = 0x0B
        (setf (mem-ref (+ desc-addr 8) :u32)
              (logior len (ash #x0B 24)))
        ;; Clear status
        (setf (mem-ref (+ desc-addr 12) :u32) 0)
        ;; Bump TX tail
        ;; Ring wrap WITHOUT (mod …): compile-mod expands to
        ;; (nth-value 1 (truncate …)) — a multiple-value return plus a
        ;; runtime NTH, machinery a bare net image cannot rely on.  It
        ;; produced 0 instead of 1 on aarch64, so TDT never advanced past
        ;; TDH and NOTHING was ever transmitted.  A compare-and-reset is
        ;; both correct and cheaper.  Task #219.
        (let ((next (if (< (+ tx-cur 1) 64) (+ tx-cur 1) 0)))
          (setf (mem-ref (+ state #x14) :u32) next)
          (e1000-write-reg #x3818 next)
          ;; Wait for TX done: poll TDH register
          (let ((done 0))
            (dotimes (try 100000)
              (when (eq (e1000-read-reg #x3810) next)
                (setq done 1)
                (setq try 100001)))
            done))))))

;; Check for received packet. Returns length or 0 if none.
;; Reads ICR to clear pending interrupt cause (allows HLT/WFI to sleep
;; until next packet arrives, rather than waking immediately on stale cause).
(defun e1000-hw-receive ()
  (let ((state (e1000-state-base)))
    (let ((rx-cur (mem-ref (+ state #x10) :u32)))
      ;; Check if E1000 has advanced RDH past our cursor
      (if (eq (e1000-read-reg #x2810) rx-cur)
          (progn
            ;; No packet — read ICR to clear pending interrupt cause
            (e1000-read-reg #xC0)
            0)
          ;; Packet received! Get length from descriptor
          (let ((desc-addr (+ (e1000-rx-desc-base) (* rx-cur 16))))
            (let ((pkt-len (mem-ref (+ desc-addr 8) :u16)))
              ;; Advance cursor, then DEFERRED RE-ARM.
              ;;
              ;; RDT must NOT be set to rx-cur here.  Writing rx-cur hands that
              ;; descriptor straight back to the NIC while its buffer is still
              ;; unread: the caller only fetches it AFTER this returns, via
              ;; e1000-hw-rx-buf (which deliberately returns rx-cur-1, "the
              ;; previous cursor's buffer, since we already advanced") and then
              ;; copies it.  So the NIC owned a buffer the driver was still
              ;; reading, and at line rate it overwrote it.
              ;;
              ;; Interpreted, packets arrived sparsely enough relative to
              ;; processing that the window almost never closed on us.  Once the
              ;; aarch64 JIT made this loop native the driver re-armed instantly,
              ;; the NIC always had free descriptors and streamed flat out, and a
              ;; 348160-byte HTTP body came back TRUNCATED at 97723 / 182683 /
              ;; 226080 / 298080 bytes -- varying, because it is a race.  TCP saw
              ;; the loss, retransmitted, and the peer eventually closed
              ;; (RXEND why=1), which is what proved this was data loss and not
              ;; the receive loop's own timeout.
              ;;
              ;; Re-arm rx-cur-1 instead: the descriptor consumed by the PREVIOUS
              ;; call, which the caller has provably finished with.  Costs one
              ;; descriptor of ring depth (127 of 128 usable) and needs no extra
              ;; state.  On the very first call prev = 127, which is exactly the
              ;; RDT the init path already programmed, so it is a no-op.
              ;;
              ;; Same defect and same fix as the Pi's r8152 RX DMA race.  A
              ;; latent race is a function of RELATIVE speed, so a speedup is a
              ;; correctness change: after one, re-audit everything that hands a
              ;; buffer to hardware.
              (let ((next (if (< (+ rx-cur 1) (e1000-rx-ring-count))
                              (+ rx-cur 1) 0))
                    (prev (if (zerop rx-cur)
                              (- (e1000-rx-ring-count) 1)
                              (- rx-cur 1))))
                (setf (mem-ref (+ state #x10) :u32) next)
                (e1000-write-reg #x2818 prev))
              pkt-len))))))

;; Get pointer to current RX buffer data
(defun e1000-hw-rx-buf ()
  (let ((rx-cur (mem-ref (+ (e1000-state-base) #x10) :u32)))
    ;; Return previous cursor's buffer since we already advanced
    (let ((prev (if (zerop rx-cur) (- (e1000-rx-ring-count) 1) (- rx-cur 1))))
      (+ (e1000-rx-buf-base) (* prev 2048)))))

;; Find E1000 and initialize. Main entry point.
(defun e1000-hw-probe ()
  (let ((mmio (pci-find-e1000)))
    (if (zerop mmio)
        (progn
          ;; "E1000:NF" + newline
          (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
          (%serial-byte 48) (%serial-byte 58) (%serial-byte 78) (%serial-byte 70)
          (%serial-byte 10)
          0)
        (progn
          ;; "E1000:MMIO=" then hex32
          (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
          (%serial-byte 48) (%serial-byte 58)
          (print-hex32 (logand mmio #xFFFFFFFF))
          (%serial-byte 10)
          (e1000-init)))))

;;; Default API wrappers — override these via last-defun-wins for other drivers
(defun e1000-send (buf len) (e1000-hw-send buf len))
(defun e1000-receive () (e1000-hw-receive))
(defun e1000-rx-buf () (e1000-hw-rx-buf))
(defun e1000-probe () (e1000-hw-probe))
