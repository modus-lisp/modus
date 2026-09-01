;;;; arch-aarch64.lisp - AArch64 (QEMU virt) PCI + I/O adapter for shared networking
;;;;
;;;; QEMU virt machine: PCI ECAM at 0x4010000000, PL011 UART at 0x09000000

;; PCI Configuration Space Access via ECAM MMIO
;; QEMU virt (highmem) places ECAM at 0x40_10000000

(defun pci-config-read (bus dev fn reg)
  (let ((addr (+ #x4010000000
                 (logior (ash bus 20)
                         (logior (ash dev 15)
                                 (logior (ash fn 12)
                                         (logand reg #xFFC)))))))
    (mem-ref addr :u32)))

(defun pci-config-write (bus dev fn reg val)
  (let ((addr (+ #x4010000000
                 (logior (ash bus 20)
                         (logior (ash dev 15)
                                 (logior (ash fn 12)
                                         (logand reg #xFFC)))))))
    (setf (mem-ref addr :u32) val)))

;; I/O delay: WFI when GIC timer configured, UART spin otherwise
;; WFI wakes on pending timer PPI (~1ms). IRQs stay masked (no ISR).
;; timer-rearm clears pending interrupt and sets new countdown.
(defun io-delay ()
  (if (zerop (mem-ref #x41060704 :u32))
      (dotimes (d 5000) (mem-ref #x09000000 :u8))
      (progn (timer-rearm) (wfi))))

;; %serial-byte: capture-aware for SSH output routing
;; Flags at (ssh-ipc-base)+0x14: bit0=capture-to-buffer, bit1=suppress-serial
;; Buffer at (ssh-ipc-base)+0x100, pos at (ssh-ipc-base)+0x18
(defun %serial-byte (b)
  (let ((flags (mem-ref (+ #x41100000 #x14) :u32)))
    (when (zerop (logand flags 2))
      (write-char-serial b))
    (when (not (zerop (logand flags 1)))
      (let ((pos (mem-ref (+ #x41100000 #x18) :u32)))
        (when (< pos 4096)
          (setf (mem-ref (+ #x41100100 pos) :u8) b)
          (setf (mem-ref (+ #x41100000 #x18) :u32) (+ pos 1)))))))

;; Seed entropy from timing
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (mem-ref #x09000000 :u8)
      (setq s (logxor (ash s 8) (logand i #xFF))))
    (when (zerop s) (setq s 42))
    s))

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

;; PCI BAR assignment (no BIOS/firmware on AArch64 bare-metal)
;; QEMU virt PCI 32-bit MMIO window: 0x10000000 - 0x3eff0000
(defun pci-assign-bars ()
  (let ((next-addr #x10000000))
    (dotimes (dev 32)
      (let ((id (pci-config-read 0 dev 0 0)))
        (when (not (eq id #xFFFFFFFF))
          (pci-config-write 0 dev 0 #x10 #xFFFFFFFF)
          (let ((bar-size-mask (pci-config-read 0 dev 0 #x10)))
            (when (not (zerop bar-size-mask))
              (let ((size (logand (+ (logxor (logand bar-size-mask #xFFFFFFF0) #xFFFFFFFF) 1) #xFFFFFFFF)))
                (let ((aligned (logand (+ next-addr (- size 1)) (logxor (- size 1) #xFFFFFFFF))))
                  (pci-config-write 0 dev 0 #x10 aligned)
                  (let ((cmd (pci-config-read 0 dev 0 4)))
                    (pci-config-write 0 dev 0 4 (logior cmd 7)))
                  (setq next-addr (+ aligned size)))))))))))

;; DMA and state base addresses for AArch64 virt
(defun e1000-state-base () #x41060000)
(defun fe-scratch-base () #x41060900)
(defun e1000-rx-desc-base () #x41000000)
(defun e1000-rx-buf-base () #x41001000)
(defun e1000-tx-desc-base () #x41041000)
(defun e1000-tx-buf-base () #x41041400)
(defun ssh-conn-base () #x41080000)

;; Base address for SSH IPC shared state (REPL, evaluator, connection management)
;; On AArch64, mapped to RAM at 0x41100000 (between conn blocks and heap)
(defun ssh-ipc-base () #x41100000)

;; Stubs for x86-specific runtime functions
(defun init-gc-helper () nil)
(defun hash-of (name) 0)
(defun nfn-lookup (hash) 0)

;; ============================================================
;; Object allocation (uses MVM intrinsics: get-alloc-ptr, etc.)
;; Scratch address 0x41070000 for object tag construction
;; ============================================================

(defun tag-as-object (aptr)
  ;; aptr = tagged fixnum from get-alloc-ptr (before advancing)
  ;; Returns object pointer: raw_addr | 3
  (let ((raw (untag aptr)))
    (setf (mem-ref #x41070000 :u64) raw)
    (let ((b0 (mem-ref #x41070000 :u8)))
      (setf (mem-ref #x41070000 :u8) (logior b0 3))
      (mem-ref #x41070000 :u64))))

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
;; GICv2 timer interrupt for WFI-based io-delay
;; ============================================================
;; Boot code sets up GIC + virtual timer (1ms at 62.5MHz).
;; ISR re-arms timer on each tick. Call this after init to
;; unmask IRQs and enable WFI in io-delay.
;;
;; Timer flag at state+0x704. MUST NOT collide with:
;;   +0x6C4..0x6E3  eph X25519 private key (32B)
;;   +0x6E4..0x703  eph X25519 public key (32B)
;;   +0x710..0x72F  host private key (32B)
;;   +0x730..0x74F  host public key (32B)

(defun enable-gic-timer ()
  (setup-irq)
  (setf (mem-ref #x41060704 :u32) 1))

;; ============================================================
;; Single-threaded stubs for AArch64 MVM (no actors/SMP)
;; ============================================================

;; Actor model stubs -- single-threaded, no spawn/receive/yield
(defun actor-spawn (fn) nil)
(defun actor-exit () nil)
(defun yield () (io-delay))
(defun receive ()
  ;; Single-threaded: poll E1000 for incoming data
  (e1000-receive)
  nil)

;; Spinlock stubs -- single-threaded, no contention
(defun spin-lock (addr) nil)
(defun spin-unlock (addr) nil)

;; Line editor state at ssh-ipc-base + offsets
;; 0x12800 = line-len, 0x12808 = cursor-pos, 0x12810 = escape-state
;; 0x12A08 = return-code (1=Enter, 2=Ctrl-D)
;; Line buffer at ssh-ipc-base + 0x28
(defun edit-line-len () (mem-ref (+ #x41100000 #x12800) :u64))
(defun edit-set-line-len (v) (setf (mem-ref (+ #x41100000 #x12800) :u64) v))
(defun edit-cursor-pos () (mem-ref (+ #x41100000 #x12808) :u64))
(defun edit-set-cursor-pos (v) (setf (mem-ref (+ #x41100000 #x12808) :u64) v))

;; Eval stubs -- MVM doesn't have native compiler at runtime
(defun native-eval (form) nil)
(defun eval-line-expr (line) nil)
(defun rt-compile-defun (name args body) nil)

;; Output helpers
(defun print-dec (n)
  ;; Print non-negative integer in decimal
  (if (< n 10)
      (%serial-byte (+ 48 n))
      (let ((q 0) (r 0))
        (setq q (/ n 10))
        (setq r (- n (* q 10)))
        (print-dec q)
        (%serial-byte (+ 48 r)))))

(defun emit-prompt ()
  (if (zerop (mem-ref (+ #x41100000 #x12A00) :u64))
      (progn (%serial-byte 62) (%serial-byte 32))
      (progn
        (%serial-byte 109) (%serial-byte 111)
        (%serial-byte 100) (%serial-byte 117)
        (%serial-byte 115) (%serial-byte 62) (%serial-byte 32))))

;; ============================================================
;; Actor system address hooks (QEMU virt memory layout)
;; ============================================================
;;
;; Memory layout (RAM at 0x40000000):
;;   0x41200000  Per-CPU data (8 CPUs x 64 bytes = 512 bytes)
;;   0x41200200  Locks: +0x00 scheduler, +0x08 pool, +0x10 actor-table
;;   0x41210000  Actor table (64 x 128 bytes = 8KB)
;;   0x41212000  Scheduler state
;;   0x41220000  Actor stacks (64 x 64KB = 4MB)
;;   0x41620000  Mailbox pool (128KB = 8192 x 16-byte cells)
;;   0x41640000  Pool state
;;   0x41700000  Staging buffers (64 x 16KB = 1MB)
;;   0x46000000  Per-actor heaps (64 x 4MB = 256MB)

(defun percpu-data-base ()   #x41200000)
(defun sched-lock-addr ()    #x41200200)
(defun actor-table-base ()   #x41210000)
(defun sched-state-base ()   #x41212000)
(defun scratch-addr ()       #x41212050)
(defun decode-ptr-addr ()    #x41212058)
(defun actor-stack-base ()   #x41220000)
(defun mailbox-pool-base ()  #x41620000)
(defun mailbox-pool-limit () #x41640000)
(defun pool-state-base ()    #x41640000)
(defun staging-base-addr ()  #x41700000)
(defun actor-heap-base ()    #x46000000)
