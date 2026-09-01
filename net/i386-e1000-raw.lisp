;;;; i386-e1000-raw.lisp — Raw MMIO E1000/E1000e driver for i386
;;;;
;;;; Overrides e1000.lisp functions to use raw MMIO TRAPs for BAR0 addresses
;;;; above 2GB (i386 fixnums are 30-bit, can't represent addresses >= 1GB).
;;;;
;;;; Supports: Intel E1000 (8086:100E, QEMU) and 82579LM (8086:1502, T420).
;;;;
;;;; Requires (loaded before this file):
;;;;   arch-i386.lisp    — e1000-state-base, io-delay, %serial-byte, etc.
;;;;   e1000.lisp        — e1000-init-rx, e1000-init-tx, e1000-hw-send/receive
;;;;   i386-console.lisp — mmio-do-read32, mmio-do-write32, mmio-result-byte,
;;;;                        pci-addr, pci-config-read-raw, addr-add-byte*
;;;;
;;;; Memory layout:
;;;;   0x600160: NIC BAR0 (4 bytes, little-endian, raw)
;;;;   0x600140: Raw MMIO address (shared scratch, 4 bytes)
;;;;   0x600148: Raw MMIO value (shared scratch, 4 bytes)
;;;;   0x60015C: Carry byte for address addition
;;;;   0x7900000: RX descriptors (128 x 16 = 2048 bytes)
;;;;   0x7901000: TX descriptors (64 x 16 = 1024 bytes)
;;;;   0x7A00000: RX buffers (128 x 2048 = 262144 bytes)
;;;;   0x7B00000: TX buffers (64 x 1536 = 98304 bytes)

;;; ============================================================
;;; Override console print functions back to serial for NIC init
;;; ============================================================
;;; i386-console.lisp redefines print-nibble/print-str to use
;;; write-char-output (VGA). Override back to %serial-byte (serial)
;;; so NIC diagnostic output doesn't spam the VGA screen.

(defun print-nibble (n)
  (let ((v n))
    (if (< v 10)
        (%serial-byte (+ v 48))
      (%serial-byte (+ v 55)))))

(defun print-str (s)
  (let ((str s))
    (let ((i 0))
      (let ((len (array-length str)))
        (loop
          (when (>= i len) (return 0))
          (%serial-byte (aref str i))
          (setq i (+ i 1)))))))

;;; ============================================================
;;; RX/TX descriptor and buffer base addresses
;;; ============================================================
;;; These are ABOVE the heap limit (0x7800000) to avoid heap corruption.
;;; NIC DMA uses these physical addresses (paging is off on i386).
;;; QEMU must have -m 512 or more.

(defun e1000-rx-desc-base () #x7900000)
(defun e1000-tx-desc-base () #x7901000)
(defun e1000-rx-buf-base ()  #x7A00000)
(defun e1000-tx-buf-base ()  #x7B00000)

;;; ============================================================
;;; NIC BAR0 address setup (raw bytes at 0x600160)
;;; ============================================================

(defun nic-mmio-setup (reg)
  ;; Set raw MMIO address at 0x600140 = NIC BAR0 + reg offset.
  ;; reg is always < 0x10000 (fits in fixnum).
  (let ((r reg))
    ;; Copy BAR0 base from 0x600160 to 0x600140
    (setf (mem-ref #x600140 :u8) (mem-ref #x600160 :u8))
    (setf (mem-ref #x600141 :u8) (mem-ref #x600161 :u8))
    (setf (mem-ref #x600142 :u8) (mem-ref #x600162 :u8))
    (setf (mem-ref #x600143 :u8) (mem-ref #x600163 :u8))
    ;; Add register offset with carry propagation
    (addr-add-byte0 (logand r #xFF))
    (addr-add-byte1 (logand (ash r -8) #xFF))
    (addr-add-byte2 0)))

;;; ============================================================
;;; e1000-write-reg / e1000-read-reg overrides (raw MMIO)
;;; ============================================================

(defun e1000-write-reg (reg val)
  ;; Write val to E1000 MMIO register (BAR0 + reg).
  ;; val must fit in 30-bit fixnum. For bit-31 values use e1000-write-reg-ff etc.
  (let ((r reg) (v val))
    (nic-mmio-setup r)
    (setf (mem-ref #x600148 :u8) (logand v #xFF))
    (setf (mem-ref #x600149 :u8) (logand (ash v -8) #xFF))
    (setf (mem-ref #x60014A :u8) (logand (ash v -16) #xFF))
    (setf (mem-ref #x60014B :u8) (logand (ash v -24) #xFF))
    (mmio-do-write32)))

(defun e1000-read-reg (reg)
  ;; Read E1000 MMIO register, return low 30 bits as fixnum.
  ;; Sufficient for descriptor indices, control bits up to bit 29.
  (let ((r reg))
    (nic-mmio-setup r)
    (mmio-do-read32)
    (let ((b0 (mem-ref #x600148 :u8)))
      (let ((b1 (mem-ref #x600149 :u8)))
        (let ((lo (logior b0 (ash b1 8))))
          (let ((b2 (mem-ref #x60014A :u8)))
            (let ((b3 (logand (mem-ref #x60014B :u8) #x3F)))
              (let ((hi (logior (ash b2 16) (ash b3 24))))
                (logior lo hi)))))))))

(defun e1000-write-reg-ff (reg)
  ;; Write 0xFFFFFFFF to register (can't fit in 30-bit fixnum).
  (let ((r reg))
    (nic-mmio-setup r)
    (setf (mem-ref #x600148 :u8) #xFF)
    (setf (mem-ref #x600149 :u8) #xFF)
    (setf (mem-ref #x60014A :u8) #xFF)
    (setf (mem-ref #x60014B :u8) #xFF)
    (mmio-do-write32)))

;;; ============================================================
;;; PCI config overrides (real PCI instead of ISA stubs)
;;; ============================================================

(defun pci-config-read (bus dev fn reg)
  ;; Read PCI config register, return low 30 bits as fixnum.
  ;; Uses TRAP #x0333 (pci-config-read-raw) for enable bit.
  (let ((b bus) (d dev))
    (let ((f fn) (r reg))
      (let ((addr (pci-addr b d f r)))
        (pci-config-read-raw addr)
        (let ((b0 (mmio-result-byte 0)))
          (let ((b1 (mmio-result-byte 1)))
            (let ((lo (logior b0 (ash b1 8))))
              (let ((b2 (mmio-result-byte 2)))
                (let ((b3 (logand (mmio-result-byte 3) #x3F)))
                  (let ((hi (logior (ash b2 16) (ash b3 24))))
                    (logior lo hi)))))))))))

(defun pci-config-write (bus dev fn reg val)
  ;; Write PCI config register. Uses TRAP to set address, then io-out-dword.
  ;; val must fit in 30-bit fixnum (OK for command register etc.).
  (let ((b bus) (d dev))
    (let ((f fn) (r reg))
      (let ((v val))
        (let ((addr (pci-addr b d f r)))
          (pci-config-read-raw addr)
          (io-out-dword #xCFC v))))))

;;; ============================================================
;;; PCI device detection for Intel NIC
;;; ============================================================

(defun pci-is-intel ()
  ;; Check if result at 0x600148 has vendor ID 0x8086.
  ;; Must call pci-read-raw first.
  (let ((v0 (mmio-result-byte 0)))
    (let ((v1 (mmio-result-byte 1)))
      (if (= v0 #x86) (= v1 #x80) nil))))

(defun pci-is-nic ()
  ;; Check device ID for supported Intel NICs.
  ;; Vendor/device already at 0x600148 from prior pci-read-raw.
  (let ((d0 (mmio-result-byte 2)))
    (let ((d1 (mmio-result-byte 3)))
      (if (= d1 #x15)
          (= d0 #x02)
        (if (= d1 #x10)
            (= d0 #x0E)
          nil)))))

(defun pci-nic-enable (dev)
  ;; Enable bus mastering + memory space + I/O space (bits 0-2 of command reg).
  (let ((d dev))
    (let ((addr (pci-addr 0 d 0 4)))
      (pci-config-read-raw addr)
      (let ((cmd (logior (mmio-result-byte 0) 7)))
        (pci-config-read-raw addr)
        (io-out-dword #xCFC cmd)))))

(defun pci-nic-save-bar0 (dev)
  ;; Read BAR0 from PCI config, save raw bytes at 0x600160 (masked to 4KB).
  (let ((d dev))
    (pci-read-raw 0 d 0 #x10)
    (let ((b0 (logand (mmio-result-byte 0) #xF0)))
      (let ((b1 (mmio-result-byte 1)))
        (let ((b2 (mmio-result-byte 2)))
          (let ((b3 (mmio-result-byte 3)))
            (setf (mem-ref #x600160 :u8) b0)
            (setf (mem-ref #x600161 :u8) b1)
            (setf (mem-ref #x600162 :u8) b2)
            (setf (mem-ref #x600163 :u8) b3)))))))

(defun pci-nic-print-bar0 ()
  ;; Print NIC BAR0 address
  (print-hex-byte (mem-ref #x600163 :u8))
  (print-hex-byte (mem-ref #x600162 :u8))
  (print-hex-byte (mem-ref #x600161 :u8))
  (print-hex-byte (mem-ref #x600160 :u8)))

(defun pci-find-e1000 ()
  ;; Scan PCI bus 0 for Intel E1000/E1000e NIC.
  ;; Stores BAR0 as raw bytes at 0x600160.
  ;; Returns 1 if found, 0 otherwise.
  (let ((found 0))
    (let ((dev 0))
      (loop
        (when (>= dev 32) (return found))
        (let ((d dev))
          (pci-read-raw 0 d 0 0)
          (when (pci-is-intel)
            (when (pci-is-nic)
              (pci-nic-enable d)
              (pci-nic-save-bar0 d)
              (setf (mem-ref #x600164 :u8) d)
              (setq found 1))))
        (setq dev (+ dev 1))))))

;;; ============================================================
;;; MAC address read from NIC registers (not EEPROM)
;;; ============================================================
;;; On real hardware, BIOS/firmware programs RAL0/RAH0 from NVM.
;;; On QEMU, the emulated e1000 auto-loads the MAC.
;;; Reading from registers avoids EEPROM protocol differences.

(defun e1000-read-mac-ral0 ()
  ;; Read RAL0 (MAC bytes 0-3), store at state+0x08.
  (let ((state (e1000-state-base)))
    (nic-mmio-setup #x5400)
    (mmio-do-read32)
    (let ((s state))
      (setf (mem-ref (+ s #x08) :u8) (mem-ref #x600148 :u8))
      (setf (mem-ref (+ s #x09) :u8) (mem-ref #x600149 :u8))
      (setf (mem-ref (+ s #x0A) :u8) (mem-ref #x60014A :u8))
      (setf (mem-ref (+ s #x0B) :u8) (mem-ref #x60014B :u8)))))

(defun e1000-read-mac-rah0 ()
  ;; Read RAH0 (MAC bytes 4-5), store at state+0x0C.
  (let ((state (e1000-state-base)))
    (nic-mmio-setup #x5404)
    (mmio-do-read32)
    (let ((s state))
      (setf (mem-ref (+ s #x0C) :u8) (mem-ref #x600148 :u8))
      (setf (mem-ref (+ s #x0D) :u8) (mem-ref #x600149 :u8)))))

(defun e1000-read-mac-from-regs ()
  (e1000-read-mac-ral0)
  (e1000-read-mac-rah0))

;;; ============================================================
;;; EEPROM override — read from stored MAC bytes
;;; ============================================================
;;; e1000-read-eeprom is called by e1000-init in e1000.lisp.
;;; Our e1000-init override doesn't call it, but provide safe override.

(defun e1000-read-eeprom (addr)
  ;; Return 16-bit "EEPROM word" from already-stored MAC bytes.
  ;; addr 0 = MAC[0]:MAC[1], addr 1 = MAC[2]:MAC[3], addr 2 = MAC[4]:MAC[5]
  (let ((a addr))
    (let ((state (e1000-state-base)))
      (let ((off (+ #x08 (* a 2))))
        (let ((lo (mem-ref (+ state off) :u8)))
          (let ((hi (mem-ref (+ state (+ off 1)) :u8)))
            (logior lo (ash hi 8))))))))

;;; ============================================================
;;; e1000-init override (split into phases for form count)
;;; ============================================================

(defun e1000-write-ral0 ()
  ;; Write RAL0 from stored MAC (may need >30 bits, use raw bytes).
  (let ((state (e1000-state-base)))
    (nic-mmio-setup #x5400)
    (let ((s state))
      (setf (mem-ref #x600148 :u8) (mem-ref (+ s #x08) :u8))
      (setf (mem-ref #x600149 :u8) (mem-ref (+ s #x09) :u8))
      (setf (mem-ref #x60014A :u8) (mem-ref (+ s #x0A) :u8))
      (setf (mem-ref #x60014B :u8) (mem-ref (+ s #x0B) :u8)))
    (mmio-do-write32)))

(defun e1000-write-rah0 ()
  ;; Write RAH0 with AV bit (bit 31) from stored MAC bytes.
  (let ((state (e1000-state-base)))
    (nic-mmio-setup #x5404)
    (let ((s state))
      (setf (mem-ref #x600148 :u8) (mem-ref (+ s #x0C) :u8))
      (setf (mem-ref #x600149 :u8) (mem-ref (+ s #x0D) :u8))
      (setf (mem-ref #x60014A :u8) 0)
      (setf (mem-ref #x60014B :u8) #x80))
    (mmio-do-write32)))

(defun e1000-init-check-bar0 ()
  ;; Verify BAR0 was found (non-zero at 0x600160).
  (let ((b0 (mem-ref #x600160 :u8)))
    (let ((b1 (mem-ref #x600161 :u8)))
      (let ((b2 (mem-ref #x600162 :u8)))
        (let ((b3 (mem-ref #x600163 :u8)))
          (let ((any (logior b0 b1)))
            (let ((all (logior any (logior b2 b3))))
              (not (zerop all)))))))))

(defun e1000-wait-auto-rd ()
  ;; Wait for EECD.AUTO_RD_DONE (bit 9) — NVM config reload after reset.
  ;; Fast poll first (no io-delay), then slow poll. ~200 iterations typical.
  ;; VGA: 'a'=done, '!'=timeout
  (let ((i 0))
    (loop
      (when (>= i 500)
        (write-char-output 33)
        (return 0))
      (let ((eecd (e1000-read-reg #x10)))
        (when (not (zerop (logand eecd #x200)))
          (write-char-output 97)
          (return 1)))
      (setq i (+ i 1)))))

(defun e1000-acquire-swflag ()
  ;; Acquire ICH software config semaphore (EXTCNF_CTRL bit 5).
  ;; Required on ICH/PCH NICs before programming registers.
  (let ((i 0))
    (loop
      (when (>= i 1000) (return 0))
      (let ((ext (e1000-read-reg #xF00)))
        (e1000-write-reg #xF00 (logior ext #x20))
        (let ((chk (e1000-read-reg #xF00)))
          (when (not (zerop (logand chk #x20)))
            (return 1))))
      (io-delay)
      (setq i (+ i 1)))))

;;; ============================================================
;;; PCI Power Management — transition NIC to D0 (active) state
;;; ============================================================
;;; Coreboot may leave the 82579LM in D3 (low-power), where MMIO reads
;;; return stale values but DMA is disabled. Linux pci_enable_device_mem()
;;; always transitions to D0 first. We scan the PCI capability list for
;;; the PM capability (ID=1) and clear PMCSR bits 0-1.

(defun pci-nic-pm-set-d0 (dev off)
  ;; Write PMCSR at pm-cap-offset+4, clearing bits 0-1 to set D0.
  ;; Must re-enable bus mastering after D3→D0 transition.
  (let ((d dev) (o off))
    (let ((pmcsr-off (+ o 4)))
      (let ((pmcsr (pci-config-read 0 d 0 pmcsr-off)))
        ;; VGA: show old power state (bits 0-1)
        (vga-hex-nib (logand pmcsr 3))
        ;; Clear bits 0-1 (power state) + bit 8 (PME_EN), preserve rest
        (let ((new-pmcsr (logand pmcsr #x3FFFFCFC)))
          (pci-config-write 0 d 0 pmcsr-off new-pmcsr))
        ;; D3→D0 requires 10ms+ recovery
        (dotimes (i 20) (io-delay))
        ;; Re-enable bus mastering (cleared by D3→D0 transition)
        (pci-nic-enable d)
        1))))

(defun pci-nic-pm-force-d3 (dev off)
  ;; Force NIC into D3 power state. Used for D3→D0 cycling to reset
  ;; the MAC DMA engine without destroying ME firmware state.
  (let ((d dev) (o off))
    (let ((pmcsr-off (+ o 4)))
      (let ((pmcsr (pci-config-read 0 d 0 pmcsr-off)))
        ;; Set bits 0-1 to 11 (D3hot)
        (pci-config-write 0 d 0 pmcsr-off (logior (logand pmcsr #x3FFFFCFC) 3))
        ;; Wait 10ms
        (dotimes (i 20) (io-delay))
        1))))

(defun pci-nic-find-pm-cap ()
  ;; Find PM capability offset in PCI config space. Returns offset or 0.
  (let ((dev (mem-ref #x600164 :u8)))
    (let ((d dev))
      (let ((ptr (logand (pci-config-read 0 d 0 #x34) #xFF)))
        (let ((p ptr))
          (let ((result 0))
            (loop
              (when (zerop p) (return result))
              (let ((cap (pci-config-read 0 d 0 p)))
                (if (= (logand cap #xFF) 1)
                    (progn (setq result p) (setq p 0))
                  (setq p (logand (ash cap -8) #xFF)))))))))))

(defun pci-nic-disable-msi-cap (dev off)
  ;; Disable MSI at capability offset: clear bit 16 (MSI Enable).
  ;; Can't use mask #xFFFEFFFF (exceeds 30-bit fixnum), use subtraction.
  (let ((d dev) (o off))
    (let ((cap (pci-config-read 0 d 0 o)))
      (when (not (zerop (logand cap #x10000)))
        (pci-config-write 0 d 0 o (- cap #x10000))))
    (write-char-output 105)))   ; 'i'

(defun pci-nic-disable-msix-cap (dev off)
  ;; Disable MSI-X at capability offset: clear bit 15 of msg-ctrl at off+2.
  ;; (Bit 31 of dword at off, but bit 31 exceeds 30-bit fixnum.)
  (let ((d dev) (o off))
    (let ((mc (pci-config-read 0 d 0 (+ o 2))))
      (when (not (zerop (logand mc #x8000)))
        (pci-config-write 0 d 0 (+ o 2) (- mc #x8000)))))
  (write-char-output 88))      ; 'X'

(defun pci-nic-clear-intx-disable (dev)
  ;; Clear PCI Command bit 10 (Interrupt Disable) so legacy INTx works.
  ;; Linux sets this when MSI is active; we need to undo it.
  (let ((d dev))
    (let ((cmd (pci-config-read 0 d 0 4)))
      (when (not (zerop (logand cmd #x400)))
        (pci-config-write 0 d 0 4 (- cmd #x400))
        (write-char-output 120)))))   ; 'x' = INTx re-enabled

(defun pci-nic-disable-msi ()
  ;; Walk PCI capabilities, disable MSI (ID=5) and MSI-X (ID=0x11).
  ;; Also clear PCI Command bit 10 (INTx Disable) left by Linux.
  ;; VGA: 'i'=MSI disabled, 'X'=MSI-X disabled, 'x'=INTx enabled, '-'=none found
  (let ((dev (mem-ref #x600164 :u8)))
    (let ((d dev))
      (let ((ptr (logand (pci-config-read 0 d 0 #x34) #xFF)))
        (let ((p ptr))
          (let ((found 0))
            (loop
              (when (zerop p) (return found))
              (let ((cap (pci-config-read 0 d 0 p)))
                (let ((id (logand cap #xFF)))
                  (when (= id 5)
                    (pci-nic-disable-msi-cap d p)
                    (setq found 1))
                  (when (= id #x11)
                    (pci-nic-disable-msix-cap d p)
                    (setq found 1))
                  (setq p (logand (ash cap -8) #xFF)))))
            (when (zerop found)
              (write-char-output 45))
            ;; Always clear INTx Disable regardless of MSI state
            (pci-nic-clear-intx-disable d)))))))

(defun pci-nic-set-d0 ()
  ;; Scan PCI capability list for PM cap (ID=1) and transition to D0.
  ;; VGA: 'P'+digit if PM found (digit=old power state), 'p' if not found.
  (let ((off (pci-nic-find-pm-cap)))
    (if (zerop off)
        (progn (write-char-output 112) 0)    ; 'p' = PM not found
      (let ((dev (mem-ref #x600164 :u8)))
        (pci-nic-pm-set-d0 dev off)
        (write-char-output 80)               ; 'P' = PM found + D0 set
        1))))

(defun pci-nic-cycle-d3-d0 ()
  ;; Force D3→D0 power cycle to reset MAC DMA engine.
  ;; On 82579LM, this resets internal DMA state machine without
  ;; destroying ME firmware configuration (ME handles power independently).
  ;; VGA: 'D' = D3→D0 cycle done, 'd' = PM not found.
  (let ((off (pci-nic-find-pm-cap)))
    (if (zerop off)
        (progn (write-char-output 100) 0)    ; 'd' = PM not found
      (let ((dev (mem-ref #x600164 :u8)))
        (pci-nic-pm-force-d3 dev off)
        (pci-nic-pm-set-d0 dev off)
        (write-char-output 68)               ; 'D' = cycle done
        1))))

(defun e1000-init-reset-pre ()
  ;; Pre-reset: disable TX/RX, clear interrupts
  (e1000-write-reg #x100 0)     ; RCTL = 0 (disable RX)
  (e1000-write-reg #x400 8)     ; TCTL = PSP only
  (e1000-write-reg-ff #xD8)     ; IMC clear
  (e1000-read-reg #xC0)         ; ICR clear
  (dotimes (i 20) (io-delay))   ; 20ms settle
  (write-char-output 49))       ; '1' = pre-reset done

(defun e1000-init-reset-ctrl ()
  ;; CTRL.RST (bit 26) — full device reset
  (let ((ctrl (e1000-read-reg 0)))
    (e1000-write-reg 0 (logior ctrl #x4000000)))
  (dotimes (i 20) (io-delay))   ; 20ms post-reset
  (write-char-output 50))       ; '2' = CTRL.RST done

(defun e1000-wait-cfg-done ()
  ;; Wait for EEMNGCTL.NVM_CFG_DONE_PORT_0 (bit 18) at offset 0x01010.
  ;; This indicates PHY configuration is complete — separate from AUTO_RD_DONE.
  ;; Required on ICH/PCH NICs (82579LM). Linux: e1000_get_cfg_done_ich8lan.
  ;; VGA: 'g'=done, '?'=timeout
  (let ((i 0))
    (loop
      (when (>= i 500)
        (write-char-output 63)
        (return 0))
      (let ((reg (e1000-read-reg #x1010)))
        (when (not (zerop (logand reg #x40000)))
          (write-char-output 103)
          (return 1)))
      (setq i (+ i 1)))))

(defun e1000-init-reset ()
  ;; Full hardware reset matching Linux e1000_reset_hw_ich8lan.
  ;; VGA: 1=pre, 2=RST, a/!=auto-rd, g/?=cfg-done
  (e1000-init-reset-pre)
  (e1000-init-reset-ctrl)
  (e1000-wait-auto-rd)
  ;; Wait for PHY config done (EEMNGCTL bit 18)
  (e1000-wait-cfg-done)
  ;; Clear PHYRA after PHY config complete (RW1C bit 10)
  (e1000-write-reg 8 #x400)
  ;; Clear interrupt state after reset
  (e1000-write-reg-ff #xD8)
  (e1000-read-reg #xC0)
  1)

(defun e1000-init-mac ()
  ;; Read MAC from NIC registers and print it.
  (e1000-read-mac-from-regs)
  (let ((state (e1000-state-base)))
    ;; "MAC:" header
    (%serial-byte 77) (%serial-byte 65) (%serial-byte 67) (%serial-byte 58)
    (let ((s state))
      (print-hex-byte (mem-ref (+ s #x08) :u8))
      (%serial-byte 58)
      (print-hex-byte (mem-ref (+ s #x09) :u8))
      (%serial-byte 58)
      (print-hex-byte (mem-ref (+ s #x0A) :u8))
      (%serial-byte 58)
      (print-hex-byte (mem-ref (+ s #x0B) :u8))
      (%serial-byte 58)
      (print-hex-byte (mem-ref (+ s #x0C) :u8))
      (%serial-byte 58)
      (print-hex-byte (mem-ref (+ s #x0D) :u8))
      (%serial-byte 10))))

(defun e1000-init-mac-regs ()
  ;; Program RAL0/RAH0 with MAC address.
  (e1000-write-ral0)
  (e1000-write-rah0))

(defun e1000-init-clear-mta ()
  ;; Clear multicast table array (128 dwords at offset 0x5200).
  (let ((i 0))
    (loop
      (when (>= i 128) (return 0))
      (e1000-write-reg (+ #x5200 (* i 4)) 0)
      (setq i (+ i 1)))))

(defun e1000-init-link ()
  ;; Set link control: SLU (bit 6) | ASDE (bit 5) = 0x60
  (e1000-write-reg 0 #x60))

(defun e1000-init-link-rmw ()
  ;; Read-modify-write CTRL: add SLU (bit 6), clear RFCE(27)/TFCE(28).
  ;; NEVER blind-write CTRL on 82579LM — firmware sets FD, speed, PHY bits
  ;; that we can't reconstruct. Blind write 0x60 wipes them all.
  ;; Must clear flow control bits — ME may enable FC, and without proper
  ;; negotiation the NIC honors pause frames that never clear → TX stuck.
  (let ((ctrl (e1000-read-reg 0)))
    (let ((c (logior ctrl #x40)))
      ;; Clear RFCE (bit 27 = 0x8000000) if set
      (let ((c2 (if (zerop (logand c #x8000000)) c (- c #x8000000))))
        ;; Clear TFCE (bit 28 = 0x10000000) if set
        (let ((c3 (if (zerop (logand c2 #x10000000)) c2 (- c2 #x10000000))))
          (e1000-write-reg 0 c3))))))

(defun e1000-init-wait-link ()
  ;; Wait up to ~5s for link-up (GbE auto-negotiation takes 2-4s).
  ;; VGA: 'L' = link up, '!' = timeout.
  (let ((i 0))
    (loop
      (when (>= i 5000)
        (write-char-output 33)
        (return 0))
      (let ((st (e1000-read-reg 8)))
        (when (not (zerop (logand st 2)))
          (write-char-output 76)
          (return 1)))
      (io-delay)
      (setq i (+ i 1)))))

(defun e1000-init-rx-ctrl ()
  ;; Enable RX: EN(1) | BAM(15) | BSIZE_2048(default) | SECRC(26)
  ;; Note: RXDCTL/RFCTL writes removed — they trigger ME firmware hangs (~120s).
  ;; 82579LM RX is confirmed dead (neutered ME blocks RX DMA).
  (e1000-write-reg #x100 #x4008002))

(defun e1000-me-wait ()
  ;; Wait for ME firmware to release bus (FWSM bit 24 = PCIM2PCI).
  ;; Linux __ew32_prepare: polls FWSM, waits up to 10ms.
  ;; Required on 82579 with active ME — writes during ME access are dropped.
  (let ((i 0))
    (loop
      (when (>= i 200) (return 0))
      (when (zerop (logand (e1000-read-reg #x5B54) #x1000000))
        (return 1))
      (io-delay)
      (setq i (+ i 1)))))

(defun e1000-init-tx-ctrl ()
  ;; Linux order: TIPG, TXDCTL, TARC0 BEFORE TCTL.EN.
  ;; Setting TCTL.EN before TXDCTL causes 82579 TX DMA to start with
  ;; wrong thresholds → stuck TDH.
  ;; 1. TIPG: IPGT=8 | IPGR1=8<<10 | IPGR2=6<<20 (copper)
  (e1000-me-wait)
  (e1000-write-reg #x410 #x602008)
  ;; 2. TXDCTL: match Linux exactly.
  ;; Linux sets MAX_TX_DESC_PREFETCH (PTHRESH=31, bit 24) + FULL_TX_DESC_WB (WTHRESH=1, bit 24).
  ;; CRITICAL: Do NOT set bit 22 (GRAN) — changes thresholds from descriptor to cacheline
  ;; granularity.  With GRAN=1, PTHRESH=31 means 31*64=1984 bytes, but our ring is only
  ;; 64*16=1024 bytes — the NIC can NEVER meet the prefetch threshold → TDH stays 0.
  (e1000-me-wait)
  (e1000-write-reg #x3828 #x0101001F)
  ;; 3. TARC0: RMW — bits 23,24,26,27 per Linux e1000_initialize_hw_bits_ich8lan
  ;; plus bit 0 (RR) and bit 7 (COMPENSATION).
  ;; bits: 0x0D800000 | 0x81 = 0x0D800081
  (e1000-me-wait)
  (let ((tarc (e1000-read-reg #x3840)))
    (e1000-write-reg #x3840 (logior tarc #xD800081)))
  ;; 4. TX interrupt delay
  (e1000-write-reg #x3820 8)
  (e1000-write-reg #x382C 32)
  ;; 5. TCTL LAST: EN(1) | PSP(3) | CT=0x0F(4-11) | COLD=0x3F(12-21) | RTLC(24)
  ;; Linux: RMW, clears CT, sets PSP|RTLC, then config_collision_dist sets COLD=63.
  ;; RTLC (bit 24): Retransmit on Late Collision.
  ;; MULR (bit 28): Multiple Request Support — CRITICAL on 82579LM.
  ;; Without MULR, TX DMA can fetch descriptors but can't issue second DMA
  ;; to read buffer data → TDH advances for bare descs but stalls on real packets.
  ;; Bit 29: set by Linux e1000e for ICH/PCH.
  ;; Linux value: 0x3103F0FA.
  (e1000-me-wait)
  (e1000-write-reg #x400 #x3103F0FA))

(defun e1000-init-irq ()
  ;; Clear masks, clear pending causes, then enable key interrupts.
  ;; 82579LM with AMT firmware may need IMS set before TX DMA starts.
  (e1000-write-reg-ff #xD8)       ; IMC = 0xFFFFFFFF (clear all masks)
  (e1000-read-reg #xC0)           ; read ICR to clear pending causes
  (e1000-write-reg #xD0 #x83))   ; IMS = TXDW(0)|TXQE(1)|RXT0(7)

(defun e1000-init-clear-phyra ()
  ;; Clear STATUS.PHYRA (bit 10) — PHY Reset Asserted flag.
  ;; RW1C: write 1 to bit 10 clears it. Required after device reset on 82579.
  (e1000-write-reg 8 #x400))

(defun e1000-init-manageability ()
  ;; When AMT firmware is active (FWSM.FW_VALID), set MANC.EN_MNG2HOST.
  ;; Required for proper host/ME cooperation on 82579LM with Intel AMT.
  (let ((fwsm (e1000-read-reg #x5B54)))
    (when (not (zerop (logand fwsm #x8000)))
      (let ((manc (e1000-read-reg #x5820)))
        (e1000-write-reg #x5820 (logior manc #x1000))))))

(defun e1000-init-hw-bits-ich ()
  ;; From Linux e1000_initialize_hw_bits_ich8lan + e1000_init_hw_ich8lan:
  ;; CTRL_EXT: bit 22 (ICH/PCH required), bit 28 (DRV_LOAD), bit 17 (RO_DIS)
  (let ((ext (e1000-read-reg #x18)))
    (e1000-write-reg #x18 (logior ext #x10420000)))
  ;; GCR (0x5B00): clear all NO_SNOOP bits (0-5) — enable DMA snooping.
  ;; Linux: gcr &= ~PCIE_NO_SNOOP_ALL (bits 0-5 are per-type no-snoop).
  ;; gcr & ~0x3F: shift right 6 then left 6 to zero bits 0-5.
  (let ((gcr (e1000-read-reg #x5B00)))
    (e1000-write-reg #x5B00 (ash (ash gcr -6) 6)))
  ;; RFCTL: silicon errata workaround for descriptor data corruption.
  ;; NFSW_DIS(6) | NFSR_DIS(7) | IPV6_EX_DIS(8) | NEW_IPV6_EXT_DIS(9)
  (let ((rfctl (e1000-read-reg #x5008)))
    (e1000-write-reg #x5008 (logior rfctl #x3C0)))
  ;; KABGTXD (0x3004 on ICH/PCH): TX arbitration errata (BGSQLBIAS bits 16+18).
  ;; WARNING: 0xE8 is FEXTNVM2 on 82579, NOT KABGTXD — was corrupting NIC state!
  (let ((kab (e1000-read-reg #x3004)))
    (e1000-write-reg #x3004 (logior kab #x50000)))
  ;; FEXTNVM (0x28, NOT 0xE4 which is FEXTNVM7): set SW_CONFIG_DONE (bit 14).
  ;; 0xE4 was wrong — FEXTNVM7 controls SMB/ULP, writing bit 14 there is harmful.
  (let ((fextnvm (e1000-read-reg #x28)))
    (e1000-write-reg #x28 (logior fextnvm #x4000))))

(defun e1000-set-drv-load ()
  ;; Set SWSM.DRV_LOAD (bit 0 of 0x5B50) to tell Intel ME firmware
  ;; that a host driver is loaded. Required on 82579LM (PCH) — without
  ;; this, ME may block host TX DMA.
  (let ((swsm (e1000-read-reg #x5B50)))
    (e1000-write-reg #x5B50 (logior swsm 1))))

(defun e1000-reenable-bm ()
  ;; Re-enable PCI bus mastering after CTRL.RST.
  ;; On PCH (82579LM), device reset may corrupt PCI config space.
  (let ((dev (mem-ref #x600164 :u8)))
    (pci-nic-enable dev)))

(defun e1000-init-ip ()
  ;; Set initial IP addresses (DHCP will override).
  ;; 10.0.2.15 = 0x0F02000A, 10.0.2.2 = 0x0202000A
  (let ((state (e1000-state-base)))
    (let ((s state))
      (setf (mem-ref (+ s #x18) :u32) #x0F02000A)
      (setf (mem-ref (+ s #x1C) :u32) #x0202000A))))

(defun e1000-init-print-bar0 ()
  ;; "BAR0:" to serial
  (%serial-byte 66) (%serial-byte 65) (%serial-byte 82) (%serial-byte 48) (%serial-byte 58)
  (pci-nic-print-bar0)
  (%serial-byte 10))

(defun e1000-init-print-ok ()
  ;; "NIC:OK\n" to serial
  (%serial-byte 78) (%serial-byte 73) (%serial-byte 67)
  (%serial-byte 58) (%serial-byte 79) (%serial-byte 75) (%serial-byte 10))

(defun e1000-init-phase2 ()
  ;; RX/TX descriptor rings + control registers
  ;; VGA: r=reset done, m=MAC, x=RX/TX, L=link, c=control
  (write-char-output 114)
  (e1000-reenable-bm)
  ;; DRV_LOAD FIRST — tell ME firmware host driver is loaded.
  ;; Must be before ring setup (Linux does this in init_hw, before configure).
  (e1000-set-drv-load)
  (e1000-init-hw-bits-ich)
  (e1000-init-mac)
  (e1000-init-mac-regs)
  (e1000-init-clear-mta)
  (write-char-output 109)
  (e1000-init-rx)
  (e1000-init-tx)
  (wbinvd)
  (write-char-output 120)
  (e1000-init-link)
  (e1000-init-wait-link)
  (e1000-init-rx-ctrl)
  (e1000-init-tx-ctrl)
  (e1000-init-clear-phyra)
  (e1000-init-irq)
  (e1000-init-manageability)
  (e1000-init-ip)
  (write-char-output 99)
  (e1000-init-print-ok)
  1)

(defun e1000-do-reset ()
  ;; Full CTRL.RST with ME arbitration.
  ;; Linux: DRV_LOAD → disable TX/RX → CTRL.RST → wait → re-enable BM.
  ;; VGA: 'R'=reset, digits=wait, '!'=timeout, 'b'=BM re-enabled
  (write-char-output 82)          ; 'R' = doing reset
  ;; Disable TX/RX before reset
  (e1000-write-reg #x100 0)      ; RCTL = 0
  (e1000-write-reg #x400 8)      ; TCTL = PSP only
  (e1000-write-reg-ff #xD8)      ; IMC = 0xFFFFFFFF
  (e1000-read-reg #xC0)          ; clear ICR
  ;; Wait 10ms for pending DMA
  (dotimes (i 20) (io-delay))
  ;; RMW CTRL: set RST (bit 26) + PHY_RST (bit 31 — needs special write)
  ;; Only set RST, skip PHY_RST to avoid 30-bit overflow
  (e1000-me-wait)
  (let ((ctrl (e1000-read-reg 0)))
    (e1000-write-reg 0 (logior ctrl #x4000000)))
  ;; Wait 20ms for reset (cannot flush — hangs hardware)
  (dotimes (i 40) (io-delay))
  ;; Re-enable PCI bus mastering (reset may clear it)
  (e1000-reenable-bm)
  (write-char-output 98))        ; 'b' = BM re-enabled

(defun e1000-init-noreset-pre ()
  ;; Phase 1: DRV_LOAD → CTRL.RST → reinit.
  ;; Linux e1000e_open for AMT: get_hw_control → reset → init_hw → configure.
  ;; VGA: P=PM, D=DRV_LOAD, R=reset, b=BM, S/s=SWFLAG, m=MAC
  (write-char-output 78)          ; 'N' init start
  ;; 1. Tell ME we're taking over (DRV_LOAD in CTRL_EXT)
  (let ((ext (e1000-read-reg #x18)))
    (e1000-write-reg #x18 (logior ext #x10400000)))
  (e1000-set-drv-load)
  (write-char-output 68)          ; 'D' = DRV_LOAD set
  ;; 2. Wait for ME to acknowledge
  (dotimes (i 10) (io-delay))
  ;; 3. Full hardware reset (like Linux e1000_reset_hw_ich8lan)
  (e1000-do-reset)
  ;; 4. Post-reset: acquire SWFLAG, set up HW bits
  (if (zerop (e1000-acquire-swflag))
      (write-char-output 115)     ; 's' = SWFLAG timeout
    (write-char-output 83))       ; 'S' = SWFLAG acquired
  ;; Re-assert DRV_LOAD after reset (reset may clear it)
  (e1000-set-drv-load)
  (e1000-init-hw-bits-ich)
  ;; KABGTXD (0x3004) after reset (Linux does this post-reset)
  (let ((kab (e1000-read-reg #x3004)))
    (e1000-write-reg #x3004 (logior kab #x50000)))
  ;; Read MAC from registers
  (e1000-init-mac)
  (e1000-init-mac-regs)
  (e1000-init-clear-mta)
  (write-char-output 109))        ; 'm'

(defun e1000-force-cfg-done ()
  ;; Force EEMNGCTL.CFG_DONE (bit 18) if ME firmware didn't set it.
  ;; On T420 with coreboot, ME never completes port config because
  ;; coreboot skips the Intel Boot Agent that normally triggers it.
  ;; Without CFG_DONE, TX DMA engine may be gated.
  ;; VGA: 'G'=forced, 'g'=already set
  (let ((reg (e1000-read-reg #x1010)))
    (if (not (zerop (logand reg #x40000)))
        (write-char-output 103)        ; 'g' = already set
      (progn
        (e1000-write-reg #x1010 (logior reg #x40000))
        (write-char-output 71)))))     ; 'G' = forced

(defun e1000-init-noreset-enable ()
  ;; No-reset init phase 2: configure rings, enable TX/RX.
  (e1000-init-rx)
  (e1000-init-tx)
  (wbinvd)
  (write-char-output 120)         ; 'x'
  ;; Force CFG_DONE before enabling TX — may ungate TX DMA engine
  (e1000-force-cfg-done)
  ;; RMW CTRL: add SLU, preserve firmware bits
  (e1000-init-link-rmw)
  (e1000-init-wait-link)
  (e1000-init-rx-ctrl)
  ;; Re-write RDT AFTER RCTL.EN — 82579LM ignores RDT when receiver disabled.
  ;; Without this, RDT stays at reset value 0 (empty ring), NIC never receives.
  (e1000-write-reg #x2818 127)
  (wbinvd)
  (e1000-init-tx-ctrl)
  (e1000-init-clear-phyra)
  ;; Flow control: set IEEE 802.3x addresses, disable TX/RX FC.
  ;; Without these, 82579 may pause TX waiting for flow control negotiation.
  (e1000-write-reg #x28 #xC28001)   ; FCAL (flow control address low)
  (e1000-write-reg #x2C #x100)      ; FCAH (flow control address high)
  (e1000-write-reg #x30 #x8808)     ; FCT (flow control type)
  (e1000-write-reg #x170 0)         ; FCTTV (transmit timer = 0)
  (e1000-init-irq)
  (e1000-init-manageability)
  ;; Trigger LSC (Link Status Change) — matches Linux e1000e_trigger_lsc.
  ;; On 82579 with ME, this kicks the link state machine.
  (e1000-write-reg #xC8 4)          ; ICS = LSC (bit 2)
  ;; Release SWFLAG — holding it may block ME from completing TX init.
  ;; Linux acquires/releases SWFLAG around each operation, never holds it.
  (let ((ext (e1000-read-reg #xF00)))
    (when (not (zerop (logand ext #x20)))
      (e1000-write-reg #xF00 (- ext #x20))))
  (write-char-output 102)           ; 'f' = SWFLAG released
  (e1000-init-ip)
  (write-char-output 99)          ; 'c'
  (e1000-init-print-ok)
  1)

(defun e1000-init-minimal ()
  ;; MINIMAL TX init: no reset, no HW bits, just rings + enable.
  ;; Tests whether CTRL.RST destroys TX capability that ME firmware set up.
  ;; VGA: Z=minimal, D=DRV_LOAD, x=rings, T=TCTL, t=TDT
  (write-char-output 90)            ; 'Z' = minimal init
  ;; 1. DRV_LOAD — tell ME we're taking over
  (let ((ext (e1000-read-reg #x18)))
    (e1000-write-reg #x18 (logior ext #x10000000)))
  (e1000-set-drv-load)
  (write-char-output 68)            ; 'D'
  ;; 2. Read MAC (don't change it)
  (e1000-init-mac)
  (e1000-init-mac-regs)
  ;; 3. Set up descriptor rings only
  (e1000-init-rx)
  (e1000-init-tx)
  (wbinvd)
  (write-char-output 120)           ; 'x'
  ;; 4. Enable RX (minimal)
  (e1000-write-reg #x100 #x4008002)
  ;; 5. Enable TX — JUST TCTL.EN + PSP, nothing else
  (e1000-write-reg #x400 #xA)      ; EN(1) | PSP(3) = 0xA
  (write-char-output 84)            ; 'T'
  ;; 6. Set up IP and try to send
  (e1000-init-ip)
  (write-char-output 99)
  (e1000-init-print-ok)
  1)

(defun tx-fuzz-check ()
  ;; Check TDH. If > 0, TX DMA started — print '!' and return 1.
  ;; Also sync software TX cursor to TDH so e1000-hw-send uses correct slot.
  (let ((h (e1000-read-reg #x3810)))
    (if (zerop h) 0
      (progn
        (setf (mem-ref (+ (e1000-state-base) #x14) :u32) h)
        (write-char-output 33) 1))))

(defun tx-fuzz-poke ()
  ;; Bare poke: cmd=0, length=0 descriptor. The 82579LM advances TDH for
  ;; these but refuses descriptors with actual data (DMA buffer read issue?).
  ;; After confirming DMA works, caller must reset TX ring for real sends.
  (wbinvd)
  (e1000-me-wait)
  (e1000-write-reg #x3818 1)
  (wbinvd)
  (dotimes (i 50) (io-delay))
  (tx-fuzz-check))

(defun tx-reset-ring ()
  ;; Toggle TCTL.EN to reset TDH, reinit descriptors, set cursor to 0.
  (let ((tctl (e1000-read-reg #x400)))
    ;; Disable TX (clear bit 1 = EN)
    (let ((off (if (zerop (logand tctl 2)) tctl (- tctl 2))))
      (e1000-write-reg #x400 off)))
  (e1000-init-tx)
  (wbinvd)
  ;; Re-enable TX
  (e1000-write-reg #x400 #x3103F0FA)
  (write-char-output 82))   ; 'R' = ring reset

(defun tx-fuzz-try (reg val tag)
  ;; Write val to NIC reg, poke TDT, print tag char always, '!' on hit.
  (let ((r reg) (v val) (t tag))
    (write-char-output t)
    (e1000-me-wait)
    (e1000-write-reg r v)
    (tx-fuzz-poke)))

(defun tx-fuzz-1 ()
  ;; Batch 1: TCTL variants
  ;; a: TCTL = just EN
  (when (not (zerop (tx-fuzz-try #x400 2 97))) (return 1))
  ;; b: TCTL = EN|PSP
  (when (not (zerop (tx-fuzz-try #x400 #xA 98))) (return 1))
  ;; c: TCTL = EN|PSP|CT|COLD (no RTLC)
  (when (not (zerop (tx-fuzz-try #x400 #x3F0FA 99))) (return 1))
  ;; d: TCTL = full with RTLC + MULR(bit 28)
  (when (not (zerop (tx-fuzz-try #x400 #x113F0FA 100))) (return 1))
  0)

(defun tx-fuzz-2 ()
  ;; Batch 2: TXDCTL variants
  ;; e: TXDCTL = 0 (clear everything)
  (when (not (zerop (tx-fuzz-try #x3828 0 101))) (return 1))
  ;; f: TXDCTL = Linux default (PTHRESH=31, WTHRESH=1, bit 24)
  (when (not (zerop (tx-fuzz-try #x3828 #x0101001F 102))) (return 1))
  ;; g: TXDCTL with bit 25 (queue enable on some models)
  (when (not (zerop (tx-fuzz-try #x3828 #x0301001F 103))) (return 1))
  0)

(defun tx-fuzz-3 ()
  ;; Batch 3: power/clock registers
  ;; h: FEXTNVM6 (0x10) — request PLL clock (bit 12)
  (let ((fv6 (e1000-read-reg #x10)))
    (when (not (zerop (tx-fuzz-try #x10 (logior fv6 #x1000) 104))) (return 1)))
  ;; i: FEXTNVM3 (0x3C) — PHY cfg counter = 50msec
  (when (not (zerop (tx-fuzz-try #x3C #x0A100000 105))) (return 1))
  ;; j: DTXCTL (0x3590) — try enabling DMA TX
  (when (not (zerop (tx-fuzz-try #x3590 #x05 106))) (return 1))
  0)

(defun tx-fuzz-4 ()
  ;; Batch 4: management/config
  ;; k: MANC = 0 (disable ALL ME management pass-through)
  (when (not (zerop (tx-fuzz-try #x5820 0 107))) (return 1))
  ;; l: Clear EXTCNF_CTRL (0xF00) entirely
  (when (not (zerop (tx-fuzz-try #xF00 0 108))) (return 1))
  ;; m: CTRL = SLU only (0x40), blow away everything else
  (when (not (zerop (tx-fuzz-try 0 #x40 109))) (return 1))
  ;; n: CTRL = SLU + FRCSPD + SPD_1000 (force 1000Mbps)
  (when (not (zerop (tx-fuzz-try 0 #xA40 110))) (return 1))
  0)

(defun tx-fuzz-5 ()
  ;; Batch 5: re-init TX ring from scratch
  ;; o: Disable TCTL, rewrite ring, re-enable
  (write-char-output 111)
  (e1000-write-reg #x400 0)
  (e1000-write-reg #x3800 #x7901000)
  (e1000-write-reg #x3804 0)
  (e1000-write-reg #x3808 1024)
  (e1000-write-reg #x3810 0)
  (e1000-write-reg #x3818 0)
  (wbinvd)
  (e1000-write-reg #x400 #x3103F0FA)
  (when (not (zerop (tx-fuzz-poke))) (return 1))
  ;; p: Try TCTL.EN toggle (disable then re-enable)
  (write-char-output 112)
  (e1000-write-reg #x400 0)
  (dotimes (i 10) (io-delay))
  (e1000-write-reg #x400 #x3103F0FA)
  (when (not (zerop (tx-fuzz-poke))) (return 1))
  0)

(defun tx-fuzz-6 ()
  ;; Batch 6: PHY kickstart attempts
  ;; q: Write PHY BMCR = auto-negotiate restart (reg 0, value 0x1200)
  (e1000-write-reg #x20 (logior (ash 0 16) #x04201200))
  (dotimes (i 500) (io-delay))
  (when (not (zerop (tx-fuzz-poke))) (write-char-output 113) (return 1))
  ;; r: Kumeran write — TIMEOUTS = 0xFFFF (reg 0x22, offset 0x34)
  (e1000-write-reg #x34 (logior #x22 (ash #xFFFF 16)))
  (dotimes (i 100) (io-delay))
  (when (not (zerop (tx-fuzz-poke))) (write-char-output 114) (return 1))
  ;; s: Kumeran write — INBAND_PARAM |= 0x3F (reg 0x09)
  (e1000-write-reg #x34 (logior #x09 (ash #x3F 16)))
  (dotimes (i 100) (io-delay))
  (when (not (zerop (tx-fuzz-poke))) (write-char-output 115) (return 1))
  0)

(defun tx-fuzz-all ()
  ;; Blast every plausible TX-gating register. Print '!' + tag letter on hit.
  ;; Restores TCTL to working value after each batch.
  ;; Test if TX already works before fuzzing (QEMU e1000).
  ;; Bare poke: if TDH advances, TX DMA works. Reset ring for clean state.
  (when (not (zerop (tx-fuzz-poke)))
    (tx-reset-ring)
    (return 1))
  (write-char-output 10)
  (write-char-output 70) (write-char-output 85) (write-char-output 90)
  (write-char-output 90) (write-char-output 58)   ; "FUZZ:"
  (when (not (zerop (tx-fuzz-1))) (return 1))
  (e1000-write-reg #x400 #x3103F0FA)
  (when (not (zerop (tx-fuzz-2))) (return 1))
  (when (not (zerop (tx-fuzz-3))) (return 1))
  (e1000-write-reg #x400 #x3103F0FA)
  (when (not (zerop (tx-fuzz-4))) (return 1))
  (e1000-write-reg #x400 #x3103F0FA)
  (e1000-write-reg 0 #x100240)
  (when (not (zerop (tx-fuzz-5))) (return 1))
  (when (not (zerop (tx-fuzz-6))) (return 1))
  ;; Restore sane state
  (e1000-write-reg #x400 #x3103F0FA)
  (write-char-output 46)   ; '.' = all tried, none worked
  0)

(defun e1000-wait-link ()
  ;; Poll STATUS.LU (bit 1) for up to ~3 seconds. Print 'L' on success, 'l' on timeout.
  (let ((up 0))
    (dotimes (i 3000)
      (when (not (zerop (logand (e1000-read-reg 8) 2)))
        (setq up 1)
        (setq i 3001))
      (io-delay))
    (if (zerop up)
      (write-char-output 108)    ; 'l' = link timeout
      (write-char-output 76))    ; 'L' = link up
    up))

;;; ============================================================
;;; VT-d (IOMMU) disable — Linux may leave it enabled
;;; ============================================================
;;; On QM67 (T420), DRHD units at 0xFED90000 (graphics) and 0xFED91000
;;; (general, covers NIC). If VT-d Translation Enable is set, NIC DMA
;;; addresses go through IOMMU with stale Linux page tables → DMA blocked.

(defun vtd-read-gsts (unit-hi)
  ;; Read GSTS (offset 0x1C) from DRHD at 0xFED9xx00.
  ;; unit-hi = 0x00 for DRHD0, 0x10 for DRHD1.
  ;; Returns byte 3 of GSTS (contains TES in bit 7 = global bit 31).
  (let ((u unit-hi))
    (setf (mem-ref #x600140 :u8) #x1C)
    (setf (mem-ref #x600141 :u8) u)
    (setf (mem-ref #x600142 :u8) #xD9)
    (setf (mem-ref #x600143 :u8) #xFE)
    (mmio-do-read32)
    (mem-ref #x60014B :u8)))

(defun vtd-write-gcmd-zero (unit-hi)
  ;; Write 0 to GCMD (offset 0x18) at DRHD 0xFED9xx00.
  (let ((u unit-hi))
    (setf (mem-ref #x600140 :u8) #x18)
    (setf (mem-ref #x600141 :u8) u)
    (setf (mem-ref #x600142 :u8) #xD9)
    (setf (mem-ref #x600143 :u8) #xFE)
    (setf (mem-ref #x600148 :u8) 0)
    (setf (mem-ref #x600149 :u8) 0)
    (setf (mem-ref #x60014A :u8) 0)
    (setf (mem-ref #x60014B :u8) 0)
    (mmio-do-write32)))

(defun vtd-poll-tes-clear (unit-hi)
  ;; Poll GSTS for TES (bit 31) clear. Returns 1 on success, 0 on timeout.
  (let ((u unit-hi))
    (let ((done 0))
      (dotimes (i 1000)
        (when (zerop (logand (vtd-read-gsts u) #x80))
          (setq done 1)
          (setq i 1001)))
      done)))

(defun vtd-disable-unit (unit-hi)
  ;; Disable VT-d for DRHD at 0xFED9xx00.
  ;; VGA: 'V'=was enabled+disabled, 'v'=was already off, '?'=timeout
  (let ((u unit-hi))
    (let ((gsts3 (vtd-read-gsts u)))
      (if (zerop (logand gsts3 #x80))
          (progn (write-char-output 118) 1)
        (progn
          (vtd-write-gcmd-zero u)
          (if (zerop (vtd-poll-tes-clear u))
              (progn (write-char-output 63) 0)
            (progn (write-char-output 86) 1))))))) ; 'V' = disabled

(defun vtd-disable ()
  ;; Disable VT-d on both QM67 DRHDs. Safe if already disabled.
  (vtd-disable-unit #x00)   ; DRHD0 at 0xFED90000 (graphics)
  (vtd-disable-unit #x10))  ; DRHD1 at 0xFED91000 (general/NIC)

(defun e1000-init-bios-phase1 ()
  ;; Phase 1: DRV_LOAD, MAC, descriptor rings
  (e1000-set-drv-load)
  (write-char-output 68)
  (e1000-init-mac)
  (e1000-init-mac-regs)
  (e1000-init-clear-mta)
  (write-char-output 109)
  (e1000-init-rx)
  (e1000-init-tx)
  (wbinvd)
  (write-char-output 120)
  (e1000-init-link-rmw)
  (e1000-init-wait-link)
  1)

(defun e1000-init-bios-phase2 ()
  ;; Phase 2: Enable RX/TX, kick TX ring, set IP
  (e1000-init-rx-ctrl)
  (e1000-write-reg #x2818 127)
  (wbinvd)
  (e1000-init-tx-ctrl)
  (e1000-init-irq)
  ;; Kick TX DMA: bare poke to wake the engine, then reset ring
  (tx-fuzz-poke)
  (tx-reset-ring)
  (e1000-init-ip)
  (write-char-output 99)
  (e1000-init-print-ok)
  1)

(defun e1000-init-bios ()
  (e1000-init-bios-phase1)
  (e1000-init-bios-phase2))

(defun pci-nic-read-irq ()
  ;; Read IRQ from PCI config 0x3C, store at 0x600024.
  (let ((dev (mem-ref #x600164 :u8)))
    (let ((d dev))
      (pci-read-raw 0 d 0 #x3C)
      (let ((irq (mmio-result-byte 0)))
        (setf (mem-ref #x600024 :u32) irq)
        irq))))

(defun nic-setup-interrupt ()
  ;; Read NIC IRQ from PCI config and install IDT entry + unmask PIC.
  ;; Also enable NIC interrupts via IMS register.
  (pci-nic-read-irq)
  (setup-nic-idt)
  ;; Enable RXT0 (bit 7) + TXDW (bit 0) in IMS
  (e1000-write-reg #xD0 #x83))

(defun nic-ack-interrupt ()
  ;; Clear pending flag, read ICR to ack NIC, unmask IRQ in PIC.
  (setf (mem-ref #x600020 :u32) 0)
  (e1000-read-reg #xC0)
  (nic-irq-unmask))

(defun e1000-init ()
  ;; E1000/E1000e initialization — BIOS-preserving (no CTRL.RST).
  (when (not (e1000-init-check-bar0))
    (%serial-byte 78) (%serial-byte 73) (%serial-byte 67)
    (%serial-byte 58) (%serial-byte 78) (%serial-byte 111) (%serial-byte 10)
    (return 0))
  (e1000-init-print-bar0)
  (pci-nic-set-d0)
  (pci-nic-disable-msi)
  (e1000-init-bios)
  ;; NIC interrupt setup skipped for now — TRAP #x0322 has encoding bugs.
  ;; Packet polling via e1000-receive works without NIC interrupts.
  (e1000-wait-link)
  1)

;;; ============================================================
;;; e1000-hw-send/receive overrides (WBINVD for cache coherency)
;;; ============================================================
;;; On real x86 hardware, CPU caches hide descriptor/buffer writes from
;;; the NIC's DMA engine. WBINVD flushes all caches before TDT write
;;; (TX) and before reading RX descriptor status.

(defun e1000-hw-send-setup (buf len buf-addr desc-addr)
  ;; Copy packet to TX buffer and set descriptor fields.
  (let ((b buf) (l len))
    (let ((ba buf-addr) (da desc-addr))
      (dotimes (i l)
        (setf (mem-ref (+ ba i) :u8) (aref b i)))
      (setf (mem-ref (+ da 8) :u32) (logior l (ash #x0B 24)))
      (setf (mem-ref (+ da 12) :u32) 0))))

(defun e1000-hw-send-wait (next)
  ;; Short TX wait: 1000 iterations (~1ms), fire-and-forget if timeout.
  ;; Prevents hang from DMA stall. 82579LM TX DMA is intermittent.
  (let ((n next))
    (let ((done 0))
      (dotimes (try 1000)
        (when (eq (e1000-read-reg #x3810) n)
          (setq done 1)
          (setq try 1001)))
      done)))

(defun e1000-hw-send (buf len)
  ;; Override with WBINVD before TDT write for DMA coherency.
  (let ((b buf) (l len))
    (let ((state (e1000-state-base)))
      (let ((tx-cur (mem-ref (+ state #x14) :u32)))
        (let ((desc-addr (+ (e1000-tx-desc-base) (* tx-cur 16))))
          (let ((buf-addr (+ (e1000-tx-buf-base) (* tx-cur 1536))))
            (e1000-hw-send-setup b l buf-addr desc-addr)
            (wbinvd)
            (let ((next (mod (+ tx-cur 1) 64)))
              (setf (mem-ref (+ state #x14) :u32) next)
              (e1000-me-wait)
              (e1000-write-reg #x3818 next)
              (wbinvd)
              (e1000-hw-send-wait next))))))))

(defun e1000-hw-receive ()
  ;; Override with WBINVD before reading RX descriptor for DMA coherency.
  (let ((state (e1000-state-base)))
    (let ((rx-cur (mem-ref (+ state #x10) :u32)))
      (wbinvd)
      (if (eq (e1000-read-reg #x2810) rx-cur)
          (progn (e1000-read-reg #xC0) 0)
          (let ((desc-addr (+ (e1000-rx-desc-base) (* rx-cur 16))))
            (let ((pkt-len (mem-ref (+ desc-addr 8) :u16)))
              (let ((next (mod (+ rx-cur 1) 128)))
                (setf (mem-ref (+ state #x10) :u32) next)
                (e1000-write-reg #x2818 rx-cur))
              pkt-len))))))

;;; ============================================================
;;; e1000-probe override
;;; ============================================================

(defun e1000-probe ()
  ;; Find Intel NIC on PCI bus and initialize.
  ;; VGA markers: A=scan start, B=scan done, F=found, N=not found
  (write-char-output 65)
  (let ((found (pci-find-e1000)))
    (write-char-output 66)
    (if (zerop found)
        (progn
          (write-char-output 78)
          ;; "NIC:NF\n" (not found)
          (%serial-byte 78) (%serial-byte 73) (%serial-byte 67)
          (%serial-byte 58) (%serial-byte 78) (%serial-byte 70) (%serial-byte 10)
          0)
      (progn
        (write-char-output 70)
        (e1000-init)))))

;;; ============================================================
;;; A20 gate test
;;; ============================================================
;;; Verifies A20 line is enabled by checking whether addresses differing
;;; only in bit 20 alias to the same physical memory.
;;; VGA: 'A' = A20 enabled (no aliasing), 'a' = A20 DISABLED (aliasing!)

(defun test-a20 ()
  ;; 0x600170 has bit 20 = 0, 0x700170 has bit 20 = 1.
  ;; If A20 off, 0x700170 wraps to 0x600170.
  (setf (mem-ref #x600170 :u8) #x11)
  (setf (mem-ref #x700170 :u8) #x22)
  (let ((val (mem-ref #x600170 :u8)))
    (if (eq val #x22)
        (write-char-output 97)   ; 'a' = A20 DISABLED
      (write-char-output 65)))) ; 'A' = A20 enabled

;;; ============================================================
;;; VT-d (IOMMU) detection and disable
;;; ============================================================
;;; Sandy Bridge DMAR unit typically at 0xFED90000.
;;; If translation is active, NIC DMA is blocked.

(defun vtd-read-gsts ()
  ;; Read GSTS at 0xFED9001C, return high byte (bit 7 = TES).
  (setf (mem-ref #x600140 :u8) #x1C)
  (setf (mem-ref #x600141 :u8) #x00)
  (setf (mem-ref #x600142 :u8) #xD9)
  (setf (mem-ref #x600143 :u8) #xFE)
  (mmio-do-read32)
  (mem-ref #x60014B :u8))

(defun vtd-write-gcmd (val)
  ;; Write 32-bit val to GCMD at 0xFED90018.
  (let ((v val))
    (setf (mem-ref #x600140 :u8) #x18)
    (setf (mem-ref #x600141 :u8) #x00)
    (setf (mem-ref #x600142 :u8) #xD9)
    (setf (mem-ref #x600143 :u8) #xFE)
    (setf (mem-ref #x600148 :u8) (logand v #xFF))
    (setf (mem-ref #x600149 :u8) (logand (ash v -8) #xFF))
    (setf (mem-ref #x60014A :u8) (logand (ash v -16) #xFF))
    (setf (mem-ref #x60014B :u8) (logand (ash v -24) #xFF))
    (mmio-do-write32)))

(defun vtd-read-gsts-at (b1)
  ;; Read GSTS at 0xFED9xx1C. b1=byte1 (0x00 or 0x10). Return high byte.
  (let ((h b1))
    (setf (mem-ref #x600140 :u8) #x1C)
    (setf (mem-ref #x600141 :u8) h)
    (setf (mem-ref #x600142 :u8) #xD9)
    (setf (mem-ref #x600143 :u8) #xFE)
    (mmio-do-read32)
    (mem-ref #x60014B :u8)))

(defun vtd-write-gcmd-at (b1)
  ;; Write 0 to GCMD at 0xFED9xx18.
  (let ((h b1))
    (setf (mem-ref #x600140 :u8) #x18)
    (setf (mem-ref #x600141 :u8) h)
    (setf (mem-ref #x600142 :u8) #xD9)
    (setf (mem-ref #x600143 :u8) #xFE)
    (setf (mem-ref #x600148 :u8) 0)
    (setf (mem-ref #x600149 :u8) 0)
    (setf (mem-ref #x60014A :u8) 0)
    (setf (mem-ref #x60014B :u8) 0)
    (mmio-do-write32)))

(defun vtd-read-ver (b1)
  ;; Read VER (offset 0x00) from DRHD. Returns byte 3.
  ;; Valid VT-d: VER=0x10 (v1.0), byte 3=0x00. Unmapped: 0xFF.
  (let ((h b1))
    (setf (mem-ref #x600140 :u8) #x00)
    (setf (mem-ref #x600141 :u8) h)
    (setf (mem-ref #x600142 :u8) #xD9)
    (setf (mem-ref #x600143 :u8) #xFE)
    (mmio-do-read32)
    (mem-ref #x60014B :u8)))

(defun vtd-try-disable (hi)
  ;; Check DMAR at 0xFED9xx00. Return GSTS byte, or 0 if no unit.
  ;; Skip if VER high byte = 0xFF (unmapped MMIO, no VT-d hardware).
  (let ((h hi))
    (let ((ver3 (vtd-read-ver h)))
      (if (= ver3 #xFF) 0
        (let ((b3 (vtd-read-gsts-at h)))
          (when (not (zerop (logand b3 #x80)))
            (vtd-write-gcmd-at h)
            (dotimes (i 100) (io-delay)))
          b3)))))

(defun vtd-disable ()
  ;; Check both common Sandy Bridge DMAR addresses and disable.
  ;; VGA: Vxx.yy (xx=0xFED90000, yy=0xFED91000)
  (write-char-output 86)
  (vga-hex-byte (vtd-try-disable #x00))
  (write-char-output 46)
  (vga-hex-byte (vtd-try-disable #x10)))

;;; ============================================================
;;; VGA-visible NIC diagnostics
;;; ============================================================

(defun vga-clear-screen ()
  ;; Clear VGA text mode: write white-on-black spaces to all 2000 cells.
  ;; Reset cursor to (0,0).
  (let ((i 0))
    (loop
      (when (>= i 2000) (return 0))
      (setf (mem-ref (+ #xB8000 (* i 2)) :u8) 32)
      (setf (mem-ref (+ #xB8001 (* i 2)) :u8) 15)
      (setq i (+ i 1))))
  (vga-set-cursor-x 0)
  (vga-set-cursor-y 0)
  (vga-update-hw-cursor))

(defun vga-hex-nib (n)
  (if (< n 10)
      (write-char-output (+ n 48))
      (write-char-output (+ n 55))))

(defun vga-hex-byte (b)
  (let ((v b))
    (vga-hex-nib (logand (ash v -4) 15))
    (vga-hex-nib (logand v 15))))

(defun vga-hex-u16 (val)
  (let ((v val))
    (vga-hex-byte (logand (ash v -8) #xFF))
    (vga-hex-byte (logand v #xFF))))

(defun vga-hex-u32 (val)
  (let ((v val))
    (vga-hex-byte (logand (ash v -24) #xFF))
    (vga-hex-byte (logand (ash v -16) #xFF))
    (vga-hex-byte (logand (ash v -8) #xFF))
    (vga-hex-byte (logand v #xFF))))

(defun vga-pci-cmd ()
  ;; Read PCI command register for NIC, return low 16 bits.
  ;; Device number saved at 0x600164 by pci-find-e1000.
  (let ((dev (mem-ref #x600164 :u8)))
    (pci-config-read 0 dev 0 4)))

(defun vga-nic-dump-line1 ()
  ;; S=STATUS(32) C=CTRL(32) H=TDH(16) P=PCI-CMD(16)
  (write-char-output 83) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg 8))
  (write-char-output 32)
  (write-char-output 67) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg 0))
  (write-char-output 32)
  (write-char-output 72) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x3810))
  (write-char-output 32)
  (write-char-output 80) (write-char-output 61)
  (vga-hex-u16 (vga-pci-cmd)))

(defun vga-nic-dump-line2 ()
  ;; D=TDBAL(32) t=TDT(16) Q=TXDCTL(32) n=TDLEN(16)
  (write-char-output 68) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x3800))
  (write-char-output 32)
  (write-char-output 116) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x3818))
  (write-char-output 32)
  (write-char-output 81) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x3828))
  (write-char-output 32)
  (write-char-output 110) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x3808)))

(defun vga-nic-dump-desc ()
  ;; Dump first TX descriptor bytes from memory at 0x7901000.
  ;; Format: B=xxxxxxxx L=xxxx K=xx D=xx
  ;; B=buf addr, L=length, K=CMD, D=status(DD)
  (let ((base #x7901000))
    (let ((b base))
      ;; Buffer address (bytes 0-3, little-endian)
      (write-char-output 66) (write-char-output 61)
      (vga-hex-byte (mem-ref (+ b 3) :u8))
      (vga-hex-byte (mem-ref (+ b 2) :u8))
      (vga-hex-byte (mem-ref (+ b 1) :u8))
      (vga-hex-byte (mem-ref b :u8))
      (write-char-output 32)
      ;; Length (bytes 8-9)
      (write-char-output 76) (write-char-output 61)
      (vga-hex-byte (mem-ref (+ b 9) :u8))
      (vga-hex-byte (mem-ref (+ b 8) :u8))
      (write-char-output 32)
      ;; CMD (byte 11)
      (write-char-output 75) (write-char-output 61)
      (vga-hex-byte (mem-ref (+ b 11) :u8))
      (write-char-output 32)
      ;; Status (byte 12) — DD bit = bit 0
      (write-char-output 100) (write-char-output 61)
      (vga-hex-byte (mem-ref (+ b 12) :u8)))))

(defun vga-nic-dump-bar0 ()
  ;; Print BAR0 from saved raw bytes at 0x600160
  (write-char-output 82) (write-char-output 61)
  (vga-hex-byte (mem-ref #x600163 :u8))
  (vga-hex-byte (mem-ref #x600162 :u8))
  (vga-hex-byte (mem-ref #x600161 :u8))
  (vga-hex-byte (mem-ref #x600160 :u8)))

(defun vga-nic-dump-line3 ()
  ;; I=ICR(32) E=CTRL_EXT(32) g=GCR(32) R=RDH(16)
  (write-char-output 73) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #xC0))
  (write-char-output 32)
  (write-char-output 69) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x18))
  (write-char-output 32)
  (write-char-output 103) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x5B00))
  (write-char-output 32)
  (write-char-output 82) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x2810)))

(defun vga-nic-dump-line4 ()
  ;; T=TCTL(32) G=TIPG(32) P=PBA(32) A=TARC0(32)
  (write-char-output 84) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x400))
  (write-char-output 32)
  (write-char-output 71) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x410))
  (write-char-output 32)
  (write-char-output 80) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x1000))
  (write-char-output 32)
  (write-char-output 65) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x3840)))

(defun e1000-read-phy (reg)
  ;; Read PHY register via MDIC. Returns 16-bit PHY data or 0 on error.
  ;; MDIC: [data:16][regadd:5][phyadd:5][op:2][r:1][i:1][e:1][dest:1]
  ;; op=1 (read), phyadd=1 (82579 internal PHY)
  (let ((r reg))
    (let ((cmd (logior (ash r 16) #x08200000)))
      (e1000-write-reg #x20 cmd)
      (let ((done 0))
        (dotimes (try 10000)
          (when (not (zerop (logand (e1000-read-reg #x20) #x10000000)))
            (setq done 1)
            (setq try 10001)))
        (if (zerop done) 0
          (logand (e1000-read-reg #x20) #xFFFF))))))

(defun vga-nic-dump-line5 ()
  ;; R=RDH(16) r=RDT(16) p=PHY-CTRL(16) s=PHY-STATUS(16) M=MANC(32)
  (write-char-output 82) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x2810))
  (write-char-output 32)
  (write-char-output 114) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x2818))
  (write-char-output 32)
  (write-char-output 112) (write-char-output 61)
  (vga-hex-u16 (e1000-read-phy 0))
  (write-char-output 32)
  (write-char-output 115) (write-char-output 61)
  (vga-hex-u16 (e1000-read-phy 1))
  (write-char-output 32)
  (write-char-output 77) (write-char-output 61)
  (vga-hex-u32 (e1000-read-reg #x5820)))

(defun vga-nic-dump-line6 ()
  ;; W=SWSM(16) F=FWSM(16)
  (write-char-output 87) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x5B50))
  (write-char-output 32)
  (write-char-output 70) (write-char-output 61)
  (vga-hex-u16 (e1000-read-reg #x5B54)))

(defun vga-nic-dump ()
  (write-char-output 10)
  (vga-nic-dump-line1)
  (write-char-output 10)
  (vga-nic-dump-line2)
  (write-char-output 10)
  (vga-nic-dump-desc)
  (write-char-output 10)
  (vga-nic-dump-line3)
  (write-char-output 10)
  (vga-nic-dump-line4)
  (write-char-output 10)
  (vga-nic-dump-line5)
  (write-char-output 32)
  (vga-nic-dump-line6)
  (write-char-output 10))

;;; ============================================================
;;; NIC MMIO diagnostic helpers (callable from REPL)
;;; ============================================================

(defun nic-read-reg (reg)
  ;; Read NIC MMIO register, print result as hex.
  (let ((r reg))
    (nic-mmio-setup r)
    (mmio-do-read32)
    (mmio-print-result)
    (write-char-output 10)
    0))

(defun tx-fill-buf (b)
  ;; Write a minimal broadcast frame at address b.
  (let ((addr b))
    (dotimes (i 6) (setf (mem-ref (+ addr i) :u8) #xFF))
    (setf (mem-ref (+ addr 12) :u8) #x08)
    (setf (mem-ref (+ addr 13) :u8) #x06)
    0))

(defun tx-test-at (buf-addr)
  ;; Test TX with a specific buffer address. Returns TDH.
  ;; Usage: (tx-test-at #x7B00000) or (tx-test-at #x7901400)
  (let ((ba buf-addr))
    ;; Reset ring first
    (tx-reset-ring)
    ;; Fill buffer
    (tx-fill-buf ba)
    ;; Write buffer address into descriptor 0
    (let ((desc (e1000-tx-desc-base)))
      (setf (mem-ref desc :u32) ba)
      (setf (mem-ref (+ desc 4) :u32) 0)
      ;; cmd=EOP|IFCS|RS, length=64
      (setf (mem-ref (+ desc 8) :u32) (logior 64 (ash #x0B 24)))
      (setf (mem-ref (+ desc 12) :u32) 0))
    (wbinvd)
    (e1000-me-wait)
    (setf (mem-ref (+ (e1000-state-base) #x14) :u32) 1)
    (e1000-write-reg #x3818 1)
    (wbinvd)
    (dotimes (i 200) (io-delay))
    (e1000-read-reg #x3810)))

(defun tx-test ()
  ;; Test TX with default buffer. Returns TDH.
  (tx-test-at (e1000-tx-buf-base)))

(defun tx-test-desc ()
  ;; Test TX with buffer in descriptor region (known DMA-readable).
  (tx-test-at (+ (e1000-tx-desc-base) 1024)))

(defun tx-test-low ()
  ;; Test TX with buffer at low address (0x100000 = 1MB).
  (tx-test-at #x100000))

(defun nic-status ()
  ;; Print NIC status registers.
  (print-str "NIC BAR0: ")
  (pci-nic-print-bar0)
  (write-char-output 10)
  (print-str "STATUS:   ")
  (nic-read-reg 8)
  (print-str "CTRL:     ")
  (nic-read-reg 0)
  (print-str "RCTL:     ")
  (nic-read-reg #x100)
  (print-str "TCTL:     ")
  (nic-read-reg #x400)
  (print-str "RDH/RDT:  ")
  (nic-read-reg #x2810)
  (print-str "TDH/TDT:  ")
  (nic-read-reg #x3810)
  0)

;;; ============================================================
;;; REPL-callable I/O port wrappers (io-in/out-byte need constant ports)
;;; ============================================================

;; KT UART (16550A at 0x50E0) — serial-over-LAN diagnostic
(defun kt-lsr () (io-in-byte #x50E5))    ; Line Status Register

(defun reboot ()
  ;; Pulse CPU reset via i8042 keyboard controller
  (io-out-byte #x64 #xFE)
  0)
(defun kt-mcr () (io-in-byte #x50E4))    ; Modem Control Register
(defun kt-msr () (io-in-byte #x50E6))    ; Modem Status Register
(defun kt-iir () (io-in-byte #x50E2))    ; Interrupt ID Register

;;; ============================================================
;;; read-char-input override (backspace filter)
;;; ============================================================
;;; Filters out backspace/delete to prevent REPL symbol corruption.
;;; Erases char from screen, skips to next real char.

;; read-char-input: use the i386-console.lisp version (interrupt-driven
;; keyboard ring buffer + network polling via nic-check-interrupt).
;; DO NOT override here — the console version handles backspace in
;; the REPL reader, and adds nic-check-interrupt for SSH packet handling.
