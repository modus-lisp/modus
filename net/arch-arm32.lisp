;;;; arch-arm32.lisp - ARM32 (QEMU virt) PCI + I/O adapter for shared networking
;;;;
;;;; QEMU virt machine: PCI ECAM at 0x3F000000, PL011 UART at 0x09000000
;;;; 30-bit fixnums (fixnum-shift=1), same constraints as i386.

;; PCI Configuration Space Access via ECAM MMIO
;; QEMU virt (32-bit) places ECAM at 0x3F000000

(defun pci-config-read (bus dev fn reg)
  (let ((addr (+ #x3F000000
                 (logior (ash bus 20)
                         (logior (ash dev 15)
                                 (logior (ash fn 12)
                                         (logand reg #xFFC)))))))
    (mem-ref addr :u32)))

(defun pci-config-write (bus dev fn reg val)
  (let ((addr (+ #x3F000000
                 (logior (ash bus 20)
                         (logior (ash dev 15)
                                 (logior (ash fn 12)
                                         (logand reg #xFFC)))))))
    (setf (mem-ref addr :u32) val)))

;; I/O delay: read UART to yield to QEMU event loop
(defun io-delay ()
  (dotimes (d 5000) (mem-ref #x09000000 :u8)))

;; Seed entropy from timing
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (mem-ref #x09000000 :u8)
      (setq s (logxor (ash s 8) (logand i #xFF))))
    (when (zerop s) (setq s 42))
    s))

;; %serial-byte: capture-aware for SSH output routing
;; Flags at (ssh-ipc-base)+0x14: bit0=capture-to-buffer, bit1=suppress-serial
;; Buffer at (ssh-ipc-base)+0x100, pos at (ssh-ipc-base)+0x18
(defun %serial-byte (b)
  (let ((flags (mem-ref (+ #x40300000 #x14) :u32)))
    (when (zerop (logand flags 2))
      (write-char-serial b))
    (when (not (zerop (logand flags 1)))
      (let ((pos (mem-ref (+ #x40300000 #x18) :u32)))
        (when (< pos 4096)
          (setf (mem-ref (+ #x40300100 pos) :u8) b)
          (setf (mem-ref (+ #x40300000 #x18) :u32) (+ pos 1)))))))

;; Hex printing utilities (needed by E1000/IP/crypto debug output)

(defun print-hex-digit (n)
  (if (< n 10)
      (%serial-byte (+ n 48))
      (%serial-byte (+ n 55))))

(defun print-hex-byte (b)
  (let ((hi (logand (ash b -4) 15))
        (lo (logand b 15)))
    (print-hex-digit hi)
    (print-hex-digit lo)))

(defun print-hex32 (n)
  (print-hex-byte (logand (ash n -24) 255))
  (print-hex-byte (logand (ash n -16) 255))
  (print-hex-byte (logand (ash n -8) 255))
  (print-hex-byte (logand n 255)))

;; PCI BAR assignment (no firmware on bare-metal ARM32 virt)
;; QEMU virt PCI 32-bit MMIO window: 0x10000000 - 0x3EFFFFFF
(defun pci-assign-bars ()
  (let ((next-addr #x10000000))
    (dotimes (dev 32)
      (let ((id (pci-config-read 0 dev 0 0)))
        (when (not (eq id (- 0 1)))
          (pci-config-write 0 dev 0 #x10 (- 0 1))
          (let ((bar-size-mask (pci-config-read 0 dev 0 #x10)))
            (when (not (zerop bar-size-mask))
              (let ((size (logand (+ (logxor (logand bar-size-mask (- 0 16)) (- 0 1)) 1) (- 0 1))))
                (let ((aligned (logand (+ next-addr (- size 1)) (logxor (- size 1) (- 0 1)))))
                  (pci-config-write 0 dev 0 #x10 aligned)
                  (let ((cmd (pci-config-read 0 dev 0 4)))
                    (pci-config-write 0 dev 0 4 (logior cmd 7)))
                  (setq next-addr (+ aligned size)))))))))))

;; DMA and state base addresses for ARM32 virt
;; Located between kernel (0x40000000) and cons space (0x41000000)
(defun e1000-state-base () #x40200000)
(defun e1000-rx-desc-base () #x40100000)
(defun e1000-rx-buf-base () #x40101000)
(defun e1000-tx-desc-base () #x40141000)
(defun e1000-tx-buf-base () #x40141400)
(defun ssh-conn-base () #x40280000)

;; Base address for SSH IPC shared state
(defun ssh-ipc-base () #x40300000)

;; Stubs for x86-specific runtime functions
(defun init-gc-helper () nil)
(defun hash-of (name) 0)
(defun nfn-lookup (hash) 0)

;; ============================================================
;; Object allocation (uses MVM intrinsics: get-alloc-ptr, etc.)
;; Scratch address 0x40270000 for object tag construction
;; ============================================================

(defun tag-as-object (aptr)
  ;; aptr = tagged fixnum from get-alloc-ptr (before advancing)
  ;; On 32-bit: untag → raw address, OR with 0x9 (object tag),
  ;; return as tagged value via :u64 (raw bits, no shift)
  (let ((raw (untag aptr)))
    (setf (mem-ref #x40270000 :u64) raw)
    (let ((b0 (mem-ref #x40270000 :u8)))
      (setf (mem-ref #x40270000 :u8) (logior b0 3))
      (mem-ref #x40270000 :u64))))

(defun try-alloc-obj (len subtag)
  ;; len = data byte count, subtag = type tag (tagged fixnum)
  ;; Returns object pointer or 0 on OOM
  ;; 32-bit: headers are 4 bytes, objects 16-byte aligned
  (let ((aptr (get-alloc-ptr)))
    (let ((data-size (+ len 1)))
      (let ((padded (logand (+ data-size 15) (- 0 16))))
        ;; total must be 16-aligned so cons alloc pointer stays aligned
        (let ((total (logand (+ 4 padded 15) (- 0 16))))
          (if (< (+ aptr total) (get-alloc-limit))
              (progn
                ;; 32-bit header: [subtag:8][element-count:24]
                (let ((header (logior (ash len 8) (untag subtag))))
                  (setf (mem-ref aptr :u32) header))
                (let ((i 0))
                  (loop
                    (if (< i padded)
                        (progn
                          (setf (mem-ref (+ aptr (+ 4 i)) :u32) 0)
                          (setq i (+ i 4)))
                        (return ()))))
                (set-alloc-ptr (+ aptr total))
                (tag-as-object aptr))
              0))))))

(defun make-array (len)
  (try-alloc-obj len #x32))

(defun aref (arr idx)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (mem-ref (+ raw 4 idx) :u8)))

(defun aset (arr idx val)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (setf (mem-ref (+ raw 4 idx) :u8) val)))

(defun array-length (arr)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (ash (mem-ref raw :u32) -8)))

(defun numberp (obj)
  (eq obj (logand obj (- 0 1))))

;; ============================================================
;; Single-threaded stubs (no actors/SMP)
;; ============================================================

(defun actor-spawn (fn) nil)
(defun actor-exit () nil)
(defun yield () (io-delay))
(defun receive ()
  (e1000-receive)
  nil)

(defun spin-lock (addr) nil)
(defun spin-unlock (addr) nil)

;; Line editor state at ssh-ipc-base + offsets
;; On ARM32 :u64 = 32-bit load/store with no tag shift (same as on 64-bit)
(defun edit-line-len () (mem-ref (+ #x40300000 #x12800) :u64))
(defun edit-set-line-len (v) (setf (mem-ref (+ #x40300000 #x12800) :u64) v))
(defun edit-cursor-pos () (mem-ref (+ #x40300000 #x12808) :u64))
(defun edit-set-cursor-pos (v) (setf (mem-ref (+ #x40300000 #x12808) :u64) v))

;; Eval stubs -- MVM doesn't have native compiler at runtime
(defun native-eval (form) 0)
(defun eval-line-expr (line) 0)
(defun rt-compile-defun (name args body) 0)

;; Print decimal number (recursive, for small values)
(defun print-dec (n)
  (when (>= n 10)
    (print-dec (truncate n 10)))
  (%serial-byte (+ (mod n 10) 48)))

;; Prompt
(defun emit-prompt ()
  (if (zerop (mem-ref (+ #x40300000 #x12A00) :u64))
      (progn (%serial-byte 62) (%serial-byte 32))
      (progn
        (%serial-byte 109) (%serial-byte 111)
        (%serial-byte 100) (%serial-byte 117)
        (%serial-byte 115) (%serial-byte 62) (%serial-byte 32))))

;; ============================================================
;; E1000 init override for 30-bit fixnum safety
;; ============================================================
;;
;; The shared e1000.lisp has one 30-bit-unsafe literal: #x80000000
;; in the RAH0 AV bit. Override e1000-init with a safe version
;; that sets the AV bit via :u8 write to the high byte.

(defun e1000-init ()
  (let ((mmio (mem-ref (e1000-state-base) :u64))
        (state (e1000-state-base)))
    (when (zerop mmio)
      (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
      (%serial-byte 48) (%serial-byte 58) (%serial-byte 78) (%serial-byte 111)
      (%serial-byte 10)
      (return 0))

    ;; 1. Reset: write CTRL.RST (bit 26)
    (e1000-write-reg 0 #x04000000)
    (dotimes (i 10000)
      (when (zerop (logand (e1000-read-reg 0) #x04000000))
        (setq i 10001)))

    ;; 2. Read MAC from EEPROM
    (let ((mac0 (e1000-read-eeprom 0))
          (mac1 (e1000-read-eeprom 1))
          (mac2 (e1000-read-eeprom 2)))
      (setf (mem-ref (+ state #x08) :u8) (logand mac0 #xFF))
      (setf (mem-ref (+ state #x09) :u8) (logand (ash mac0 -8) #xFF))
      (setf (mem-ref (+ state #x0A) :u8) (logand mac1 #xFF))
      (setf (mem-ref (+ state #x0B) :u8) (logand (ash mac1 -8) #xFF))
      (setf (mem-ref (+ state #x0C) :u8) (logand mac2 #xFF))
      (setf (mem-ref (+ state #x0D) :u8) (logand (ash mac2 -8) #xFF))

      ;; 3. Set MAC in RAL0/RAH0
      (e1000-write-reg #x5400 (logior mac0 (ash mac1 16)))
      ;; RAH0: mac2 in low 16 bits, AV bit (bit 31) set via byte write
      (e1000-write-reg #x5404 mac2)
      ;; Set AV bit: byte 3 of RAH0 register = 0x80
      (setf (mem-ref (+ mmio #x5407) :u8) #x80)

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

    ;; 4. Clear multicast table
    (dotimes (i 128)
      (e1000-write-reg (+ #x5200 (* i 4)) 0))

    ;; 5-6. Setup RX/TX rings
    (e1000-init-rx)
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
    (e1000-write-reg #x3820 0)
    (e1000-write-reg #x382C 0)

    ;; Disable interrupts (0xFFFFFFFF = -1 on 30-bit fixnums, stored correctly)
    (e1000-write-reg #xD8 (- 0 1))

    ;; Store our IP (10.0.2.15) and gateway (10.0.2.2)
    (setf (mem-ref (+ state #x18) :u32) #x0F02000A)
    (setf (mem-ref (+ state #x1C) :u32) #x0202000A)

    ;; "E1000:OK" + newline
    (%serial-byte 69) (%serial-byte 49) (%serial-byte 48) (%serial-byte 48)
    (%serial-byte 48) (%serial-byte 58) (%serial-byte 79) (%serial-byte 75)
    (%serial-byte 10)
    1))
