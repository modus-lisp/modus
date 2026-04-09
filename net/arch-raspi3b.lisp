;;;; arch-raspi3b.lisp - Raspberry Pi 3B adapter for shared networking
;;;;
;;;; BCM2837 (QEMU raspi3b): DWC2 USB at 0x3F980000, PL011 UART at 0x3F201000
;;;; No PCI bus — networking via USB CDC Ethernet

;; No PCI on RPi — stubs for shared code that calls these
(defun pci-config-read (bus dev fn reg) 0)
(defun pci-config-write (bus dev fn reg val) nil)
(defun pci-assign-bars () nil)

;; DWC2 USB controller base address (BCM2837)
(defun dwc2-base () #x3F980000)

;; I/O delay: WFI when timer configured, UART spin otherwise
;; timer-rearm re-arms ARM virtual timer (~1ms), WFI sleeps until it fires.
;; Flag at usb-ring-base+0x10: 1=timer active (set by enable-rpi-timer).
(defun io-delay ()
  (if (zerop (mem-ref (+ #x01090000 #x10) :u32))
      (dotimes (d 5000) (mem-ref #x3F201000 :u8))
      (progn (timer-rearm) (wfi))))

;; write-byte: capture-aware for SSH output routing
;; Flags at ssh-ipc-base+0x14: bit0=capture-to-buffer, bit1=suppress-serial
;; Buffer at ssh-ipc-base+0x100, pos at ssh-ipc-base+0x18
(defun write-byte (b)
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

;; Hex printing utilities (needed by IP/crypto debug output)

(defun print-hex-digit (n)
  (if (< n 10)
      (write-byte (+ n 48))
      (write-byte (+ n 55))))

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

;; DMA and state base addresses for RPi 3B
;; Same layout as E1000 state — ip.lisp reads state at these offsets
(defun e1000-state-base () #x01060000)
(defun e1000-rx-desc-base () #x01000000)
(defun e1000-rx-buf-base () #x01001000)
(defun e1000-tx-desc-base () #x01041000)
(defun e1000-tx-buf-base () #x01041400)
(defun ssh-conn-base () #x01080000)

;; USB IRQ ring buffer base (10KB, used by interrupt-driven DWC2 gadget ISR)
(defun usb-ring-base () #x01090000)

;; Base address for SSH IPC shared state
(defun ssh-ipc-base () #x01100000)

;; USB DMA buffer base (for control/bulk transfers)
(defun usb-dma-base () #x01000000)

;; Stubs for x86-specific runtime functions
(defun init-gc-helper () nil)
(defun hash-of (name) 0)
(defun nfn-lookup (hash) 0)

;; ============================================================
;; Object allocation (uses MVM intrinsics: get-alloc-ptr, etc.)
;; Scratch address 0x01070000 for object tag construction
;; ============================================================

(defun tag-as-object (aptr)
  ;; aptr = tagged fixnum from get-alloc-ptr (before advancing)
  ;; Returns object pointer: raw_addr | 3
  (let ((raw (untag aptr)))
    (setf (mem-ref #x01070000 :u64) raw)
    (let ((b0 (mem-ref #x01070000 :u8)))
      (setf (mem-ref #x01070000 :u8) (logior b0 3))
      (mem-ref #x01070000 :u64))))

(defun try-alloc-obj (len subtag)
  ;; len = data byte count, subtag = type tag (tagged fixnum)
  ;; Returns object pointer or 0 on OOM
  (let ((aptr (get-alloc-ptr)))
    (let ((data-size (+ len 1)))
      (let ((padded (logand (+ data-size 15) (- 0 16))))
        ;; total must be 16-aligned so cons alloc pointer stays aligned
        (let ((total (logand (+ 8 padded 15) (- 0 16))))
          (if (< (+ aptr total) (get-alloc-limit))
              (progn
                (let ((header (logior (ash len 15) (untag subtag))))
                  (setf (mem-ref aptr :u64) header))
                (let ((i 0))
                  (loop
                    (if (< i padded)
                        (progn
                          (setf (mem-ref (+ aptr (+ 8 i)) :u64) 0)
                          (setq i (+ i 8)))
                        (return ()))))
                (set-alloc-ptr (+ aptr total))
                (tag-as-object aptr))
              0))))))

(defun make-array (len)
  (try-alloc-obj len #x32))

(defun aref (arr idx)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (mem-ref (+ raw 8 idx) :u8)))

(defun aset (arr idx val)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (setf (mem-ref (+ raw 8 idx) :u8) val)))

(defun array-length (arr)
  (let ((raw (ash (logand arr (- 0 4)) 1)))
    (ash (mem-ref raw :u64) -15)))

(defun numberp (obj)
  (eq obj (logand obj (- 0 1))))

;; ============================================================
;; ARM virtual timer for WFI-based io-delay
;; ============================================================
;; setup-irq initializes CNTV_TVAL_EL0/CNTV_CTL_EL0 (1ms at 62.5MHz).
;; BCM2836 local interrupt controller at 0x40000000 routes CNTVIRQ
;; (bit 3) to core 0. No GIC on raspi3b — use QA7 peripheral instead.
;; Call after all crypto init (before net-actor-main).

(defun enable-rpi-timer ()
  (setup-irq)
  ;; Enable CNTVIRQ (bit 3) on core 0: Core0 Timer IRQ Control at 0x40000040
  (setf (mem-ref #x40000040 :u32) 8)
  (setf (mem-ref (+ #x01090000 #x10) :u32) 1))

;; ============================================================
;; DWC2 host-mode interrupt enable (WFI-based waking)
;; ============================================================
;;
;; Enable DWC2 host channel interrupts so WFI wakes on USB activity.
;; Does NOT require an ISR — WFI wakes on pending interrupt even with
;; IRQs masked (PSTATE.I=1). Main loop polls HCINT after waking.
;; Call after dwc2-init + USB enumeration + CDC setup.

(defun dwc2-enable-host-irq ()
  ;; HCINTMSK[0..2]: XFERCOMPL(0) + CHHLTD(1) + STALL(3) + NAK(4)
  (let ((mask (logior 1 (logior 2 (logior 8 16)))))
    (setf (mem-ref (+ #x3F98050C) :u32) mask)
    (setf (mem-ref (+ #x3F98052C) :u32) mask)
    (setf (mem-ref (+ #x3F98054C) :u32) mask))
  ;; GINTMSK bit 25 = HCINT (host channel interrupt)
  (setf (mem-ref (+ #x3F980018) :u32) #x02000000)
  ;; BCM Enable_IRQs_1: bit 9 = USB
  (setf (mem-ref #x3F00B210 :u32) #x200)
  ;; Set WFI flag at usb-ring-base+0x0C
  (setf (mem-ref (+ #x01090000 #x0C) :u32) 1))

;; ============================================================
;; Single-threaded stubs for RPi MVM (no actors/SMP)
;; ============================================================

;; Actor model stubs -- single-threaded, no spawn/receive/yield
(defun actor-spawn (fn) nil)
(defun actor-exit () nil)
(defun yield () (io-delay))
(defun receive ()
  ;; Single-threaded: poll NIC for incoming data
  (e1000-receive)
  nil)

;; Spinlock stubs -- single-threaded, no contention
(defun spin-lock (addr) nil)
(defun spin-unlock (addr) nil)

;; Line editor state at ssh-ipc-base + offsets
;; 0x12800 = line-len, 0x12808 = cursor-pos, 0x12810 = escape-state
;; 0x12A08 = return-code (1=Enter, 2=Ctrl-D)
(defun edit-line-len () (mem-ref (+ #x01100000 #x12800) :u64))
(defun edit-set-line-len (v) (setf (mem-ref (+ #x01100000 #x12800) :u64) v))
(defun edit-cursor-pos () (mem-ref (+ #x01100000 #x12808) :u64))
(defun edit-set-cursor-pos (v) (setf (mem-ref (+ #x01100000 #x12808) :u64) v))

;; Eval stubs -- MVM doesn't have native compiler at runtime
(defun native-eval (form) nil)
(defun eval-line-expr (line) nil)
(defun rt-compile-defun (name args body) nil)

;; Output helpers
(defun print-dec (n)
  ;; Print non-negative integer in decimal
  (if (< n 10)
      (write-byte (+ 48 n))
      (let ((q 0) (r 0))
        (setq q (/ n 10))
        (setq r (- n (* q 10)))
        (print-dec q)
        (write-byte (+ 48 r)))))

(defun emit-prompt ()
  (if (zerop (mem-ref (+ #x01100000 #x12A00) :u64))
      (progn (write-byte 62) (write-byte 32))
      (progn
        (write-byte 109) (write-byte 111)
        (write-byte 100) (write-byte 117)
        (write-byte 115) (write-byte 62) (write-byte 32))))

;; ============================================================
;; Actor system address hooks (RPi 3B / Pi Zero 2 W memory layout)
;; ============================================================
;;
;; Memory layout (1GB RAM at 0x00000000):
;;   0x02000000  Per-CPU data (512B) + locks (+0x200)
;;   0x02010000  Actor table (8KB)
;;   0x02012000  Scheduler state
;;   0x02020000  Actor stacks (4MB)
;;   0x02420000  Mailbox pool (128KB)
;;   0x02440000  Pool state
;;   0x02500000  Staging buffers (1MB)
;;     --- gap ---
;;   0x04000000  Cons heap (existing, 16MB)
;;   0x06000000  Per-actor heaps (4MB each)

(defun percpu-data-base ()   #x02000000)
(defun sched-lock-addr ()    #x02000200)
(defun actor-table-base ()   #x02010000)
(defun sched-state-base ()   #x02012000)
(defun scratch-addr ()       #x02012050)
(defun decode-ptr-addr ()    #x02012058)
(defun actor-stack-base ()   #x02020000)
(defun mailbox-pool-base ()  #x02420000)
(defun mailbox-pool-limit () #x02440000)
(defun pool-state-base ()    #x02440000)
(defun staging-base-addr ()  #x02500000)
(defun actor-heap-base ()    #x06000000)
