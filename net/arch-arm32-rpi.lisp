;;;; arch-arm32-rpi.lisp - ARM32 RPi (raspi2b) adapter for shared networking
;;;;
;;;; BCM2836 (QEMU raspi2b): DWC2 USB at 0x3F980000, PL011 UART at 0x3F201000
;;;; 32-bit word size, 31-bit fixnums (fixnum-shift=1), RAM at 0x0
;;;; Same DWC2/USB/CDC-ECM stack as raspi3b (AArch64) and Pi Zero 2 W
;;;; No PCI bus — networking via USB CDC Ethernet

;; No PCI on RPi — stubs for shared code
(defun pci-config-read (bus dev fn reg) 0)
(defun pci-config-write (bus dev fn reg val) nil)
(defun pci-assign-bars () nil)

;; DWC2 USB controller base address (BCM2836, same as BCM2837)
(defun dwc2-base () #x3F980000)

;; I/O delay: WFI when timer configured, NOP otherwise.
;; timer-rearm re-arms ARM virtual timer (~1ms), WFI sleeps until it fires.
;; Flag at usb-ring-base+0x10: 1=timer active (set by enable-arm32-timer).
(defun io-delay ()
  (if (zerop (mem-ref (+ #x01090000 #x10) :u32))
      0
      (progn (timer-rearm) (wfi))))

;; %serial-byte: capture-aware for SSH output routing
;; Flags at ssh-ipc-base+0x14: bit0=capture-to-buffer, bit1=suppress-serial
;; Buffer at ssh-ipc-base+0x100, pos at ssh-ipc-base+0x18
(defun %serial-byte (b)
  (let ((flags (mem-ref (+ #x01100000 #x14) :u32)))
    (when (zerop (logand flags 2))
      (write-char-serial b))
    (when (not (zerop (logand flags 1)))
      (let ((pos (mem-ref (+ #x01100000 #x18) :u32)))
        (when (< pos 4096)
          (setf (mem-ref (+ #x01100100 pos) :u8) b)
          (setf (mem-ref (+ #x01100000 #x18) :u32) (+ pos 1)))))))

;; Seed entropy from timing
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (mem-ref #x3F201000 :u8)
      (setq s (logxor (ash s 8) (logand i #xFF))))
    (when (zerop s) (setq s 42))
    s))

;; Hex printing utilities
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

;; DMA and state base addresses — same layout as raspi3b
;; All below 0x40000000 (fits in 31-bit fixnums)
(defun e1000-state-base () #x01060000)

;; Scratch memory for cons-free field arithmetic (crypto-32-fast.lisp)
;; 256 bytes at state-base + 0x900 (well past used state offsets)
(defun fe-scratch-base () #x01060900)
(defun e1000-rx-desc-base () #x01000000)
(defun e1000-rx-buf-base () #x01001000)
(defun e1000-tx-desc-base () #x01041000)
(defun e1000-tx-buf-base () #x01041400)
(defun ssh-conn-base () #x01080000)

;; USB IRQ ring buffer base
(defun usb-ring-base () #x01090000)

;; Base address for SSH IPC shared state
(defun ssh-ipc-base () #x01100000)

;; USB DMA buffer base
(defun usb-dma-base () #x01000000)

;; Stubs for x86-specific runtime functions
(defun init-gc-helper () nil)
(defun hash-of (name) 0)
(defun nfn-lookup (hash) 0)

;; ============================================================
;; Object allocation — 32-bit headers (4 bytes, not 8)
;; Scratch address 0x01070000 for object tag construction
;; ============================================================

(defun tag-as-object (aptr)
  ;; aptr = tagged fixnum from get-alloc-ptr (before advancing)
  ;; On 32-bit: untag → raw address, OR with 0x3 (object tag bits),
  ;; return as tagged value via :u64 (raw bits, no shift)
  (let ((raw (untag aptr)))
    (setf (mem-ref #x01070000 :u64) raw)
    (let ((b0 (mem-ref #x01070000 :u8)))
      (setf (mem-ref #x01070000 :u8) (logior b0 3))
      (mem-ref #x01070000 :u64))))

(defun try-alloc-obj (len subtag)
  ;; len = data byte count, subtag = type tag (tagged fixnum)
  ;; Returns object pointer or 0 on OOM
  ;; 32-bit: headers are 4 bytes, objects 16-byte aligned
  (let ((aptr (get-alloc-ptr)))
    (let ((data-size (+ len 1)))
      (let ((padded (logand (+ data-size 15) (- 0 16))))
        ;; total must be 16-aligned so cons alloc pointer stays aligned
        (let ((total (logand (+ 4 (+ padded 15)) (- 0 16))))
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
    (mem-ref (+ raw (+ 4 idx)) :u8)))

(defun aset (arr idx val)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (setf (mem-ref (+ raw (+ 4 idx)) :u8) val)))

(defun array-length (arr)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (ash (mem-ref raw :u32) -8)))

(defun numberp (obj)
  (eq obj (logand obj (- 0 1))))

;; ============================================================
;; ARM virtual timer for WFI-based io-delay
;; ============================================================
;; setup-irq initializes CNTVTVAL/CNTVCTL via CP15 (1ms at 62.5MHz).
;; No interrupt controller setup needed — timer PPI wakes WFI directly.
;; Call after all crypto init (before net-actor-main).

(defun enable-arm32-timer ()
  (setup-irq)
  (setf (mem-ref (+ #x01090000 #x10) :u32) 1))

;; ============================================================
;; DWC2 Host-Mode Interrupt Enable
;; ============================================================
;;
;; Enable DWC2 host channel interrupts so the ISR (in boot code) fires
;; on USB transfer completion. The ISR stores HCINT values to shared
;; memory at usb-ring-base; dwc2-poll-bulk-in reads them from there.
;;
;; Call after dwc2-init + USB enumeration + CDC setup.

(defun dwc2-enable-host-irq ()
  ;; Enable DWC2 host channel interrupts at controller level.
  ;; WFI wakes on pending interrupts even with CPSR I-bit set (IRQs masked),
  ;; so we DON'T need an ISR or sti. The main loop polls HCINT after waking.
  ;;
  ;; Enable XFERCOMPL + CHHLTD + NAK + STALL on channels 0,1,2
  ;; Bits: XFERCOMPL=0, CHHLTD=1, STALL=3, NAK=4
  (let ((mask (logior 1 (logior 2 (logior 8 16)))))
    (setf (mem-ref (+ #x3F98050C) :u32) mask)
    (setf (mem-ref (+ #x3F98052C) :u32) mask)
    (setf (mem-ref (+ #x3F98054C) :u32) mask))
  ;; GINTMSK bit 25 = HCINT (host channel interrupt)
  (setf (mem-ref (+ #x3F980018) :u32) #x02000000)
  ;; BCM Enable_IRQs_1: bit 9 = USB (propagate to CPU)
  (setf (mem-ref #x3F00B210 :u32) #x200)
  ;; Set "WFI ready" flag so io-delay uses WFI instead of NOP
  (setf (mem-ref (+ #x01090000 #x0C) :u32) 1))

;; ============================================================
;; Single-threaded stubs (no actors/SMP)
;; ============================================================

(defun actor-spawn (fn) nil)
(defun actor-exit () nil)
;; yield: WFI when interrupts are configured, NOP otherwise
;; Called from main idle loop (net-actor-main) between SSH connections
(defun yield ()
  (if (zerop (mem-ref (+ #x01090000 #x0C) :u32))
      0
      (wfi)))
(defun receive ()
  (e1000-receive)
  nil)

(defun spin-lock (addr) nil)
(defun spin-unlock (addr) nil)

;; Line editor state at ssh-ipc-base + offsets
;; On ARM32 :u64 = 32-bit load/store with no tag shift
(defun edit-line-len () (mem-ref (+ #x01100000 #x12800) :u64))
(defun edit-set-line-len (v) (setf (mem-ref (+ #x01100000 #x12800) :u64) v))
(defun edit-cursor-pos () (mem-ref (+ #x01100000 #x12808) :u64))
(defun edit-set-cursor-pos (v) (setf (mem-ref (+ #x01100000 #x12808) :u64) v))

;; Eval stubs -- MVM doesn't have native compiler at runtime
(defun native-eval (form) 0)
(defun eval-line-expr (line) 0)
(defun rt-compile-defun (name args body) 0)

;; Print decimal number
(defun print-dec (n)
  (when (>= n 10)
    (print-dec (truncate n 10)))
  (%serial-byte (+ (mod n 10) 48)))

;; Prompt
(defun emit-prompt ()
  (if (zerop (mem-ref (+ #x01100000 #x12A00) :u64))
      (progn (%serial-byte 62) (%serial-byte 32))
      (progn
        (%serial-byte 109) (%serial-byte 111)
        (%serial-byte 100) (%serial-byte 117)
        (%serial-byte 115) (%serial-byte 62) (%serial-byte 32))))
