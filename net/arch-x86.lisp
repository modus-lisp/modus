;;;; arch-x86.lisp - x86-64 PCI + I/O adapter for shared networking code
;;;;
;;;; Defines arch-specific functions called by the shared e1000/ip/crypto/ssh code.
;;;; Used by MVM-compiled x64 SSH builds (build-x64-ssh.lisp) and fixpoint.

;; PCI Configuration Space Access via I/O ports 0xCF8/0xCFC

(defun pci-config-read (bus dev fn reg)
  (io-out-dword #xCF8
    (logior #x80000000
            (logior (ash bus 16)
                    (logior (ash dev 11)
                            (logior (ash fn 8)
                                    (logand reg #xFC))))))
  (io-in-dword #xCFC))

(defun pci-config-write (bus dev fn reg val)
  (io-out-dword #xCF8
    (logior #x80000000
            (logior (ash bus 16)
                    (logior (ash dev 11)
                            (logior (ash fn 8)
                                    (logand reg #xFC))))))
  (io-out-dword #xCFC val))

;; I/O delay: HLT when PIT timer configured, serial spin otherwise
;; STI+HLT sleeps until PIT tick (~1ms), CLI re-masks interrupts.
;; Flag at 0x600010: 1=PIT active (set by enable-pit-timer).
(defun io-delay ()
  (if (zerop (mem-ref #x600010 :u32))
      (dotimes (d 5000) (io-in-byte #x3F8))
      (progn (sti-hlt) (cli))))

;; Enable PIT timer interrupt for HLT-based io-delay
(defun enable-pit-timer ()
  (setup-irq)
  (setf (mem-ref #x600010 :u32) 1))

;; %serial-byte: capture-aware for SSH output routing
;; Flags at ssh-ipc-base+0x14: bit0=capture-to-buffer, bit1=suppress-serial
;; Buffer at ssh-ipc-base+0x100, pos at ssh-ipc-base+0x18
(defun %serial-byte (b)
  (let ((flags (mem-ref (+ #x300000 #x14) :u32)))
    (when (zerop (logand flags 2))
      (write-char-serial b))
    (when (not (zerop (logand flags 1)))
      (let ((pos (mem-ref (+ #x300000 #x18) :u32)))
        (when (< pos 4096)
          (setf (mem-ref (+ #x300100 pos) :u8) b)
          (setf (mem-ref (+ #x300000 #x18) :u32) (+ pos 1)))))))

;; Seed entropy from I/O timing (x86 has PIT counter at 0x40)
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (io-in-byte #x3F8)
      (setq s (logxor (ash s 8) (logand (io-in-byte #x40) #xFF))))
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

;; PCI BAR assignment — no-op on x86-64 because QEMU's BIOS pre-assigns BARs.
;; Page tables map full 4GB so BIOS-assigned BARs (e.g. 0xFEBC0000) are accessible.
(defun pci-assign-bars () nil)

;; DMA and state base addresses for x86-64
(defun e1000-state-base () #x05060000)
(defun fe-scratch-base () #x05060900)
(defun e1000-rx-desc-base () #x05000000)
(defun e1000-rx-buf-base () #x05001000)
(defun e1000-tx-desc-base () #x05041000)
(defun e1000-tx-buf-base () #x05041400)
(defun ssh-conn-base () #x05080000)

;; Base address for SSH IPC shared state (REPL, evaluator, connection management)
;; Must be #x300000 to match hardcoded addresses in build.lisp REPL code
(defun ssh-ipc-base () #x300000)

;; Stubs for x86-specific runtime functions
(defun init-gc-helper () nil)
(defun hash-of (name) 0)
(defun nfn-lookup (hash) 0)

;; ============================================================
;; Object allocation (uses MVM intrinsics: get-alloc-ptr, etc.)
;; Scratch address 0x05070000 for object tag construction
;; ============================================================

(defun tag-as-object (aptr)
  (let ((raw (untag aptr)))
    (setf (mem-ref #x05070000 :u64) raw)
    (let ((b0 (mem-ref #x05070000 :u8)))
      (setf (mem-ref #x05070000 :u8) (logior b0 3))
      (mem-ref #x05070000 :u64))))

(defun try-alloc-obj (len subtag)
  (let ((aptr (get-alloc-ptr)))
    (let ((data-size (+ len 1)))
      (let ((padded (logand (+ data-size 15) (- 0 16))))
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
;; Single-threaded stubs (no actors/SMP for MVM build)
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
(defun edit-line-len () (mem-ref (+ #x300000 #x12800) :u64))
(defun edit-set-line-len (v) (setf (mem-ref (+ #x300000 #x12800) :u64) v))
(defun edit-cursor-pos () (mem-ref (+ #x300000 #x12808) :u64))
(defun edit-set-cursor-pos (v) (setf (mem-ref (+ #x300000 #x12808) :u64) v))

;; Eval stubs -- MVM doesn't have native compiler at runtime
(defun native-eval (form) nil)
(defun eval-line-expr (line) nil)
(defun rt-compile-defun (name args body) nil)

;; Output helpers
(defun print-dec (n)
  (if (< n 10)
      (%serial-byte (+ 48 n))
      (let ((q 0) (r 0))
        (setq q (/ n 10))
        (setq r (- n (* q 10)))
        (print-dec q)
        (%serial-byte (+ 48 r)))))

(defun emit-prompt ()
  (if (zerop (mem-ref (+ #x300000 #x12A00) :u64))
      (progn (%serial-byte 62) (%serial-byte 32))
      (progn
        (%serial-byte 109) (%serial-byte 111)
        (%serial-byte 100) (%serial-byte 117)
        (%serial-byte 115) (%serial-byte 62) (%serial-byte 32))))

;; Print object (stub for non-self-hosting MVM builds)
(defun print-obj (x) (print-dec x))

;; ============================================================
;; Actor system address hooks (x86-64 memory layout)
;; ============================================================
;; x86-64 RAM starts at 0, heap at 0x10000000-0x1E000000.
;; Actor infrastructure at 0x06000000-0x08000000 (between NIC and PCI BARs).

(defun percpu-data-base ()   #x06000000)
(defun sched-lock-addr ()    #x06000200)
(defun actor-table-base ()   #x06010000)
(defun sched-state-base ()   #x06012000)
(defun scratch-addr ()       #x06012050)
(defun decode-ptr-addr ()    #x06012058)
(defun actor-stack-base ()   #x06020000)
(defun mailbox-pool-base ()  #x06420000)
(defun mailbox-pool-limit () #x06440000)
(defun pool-state-base ()    #x06440000)
(defun staging-base-addr ()  #x06500000)
(defun actor-heap-base ()    #x20000000)
