;;;; arch-i386.lisp - i386 adapter for shared networking code
;;;;
;;;; Uses NE2000 ISA NIC (port I/O, no PCI/MMIO needed).
;;;; All values stay within 31-bit fixnum range.

;; I/O delay: HLT when PIT timer configured, serial spin otherwise
;; STI+HLT sleeps until PIT tick (~1ms), CLI re-masks interrupts.
;; Flag at 0x600010: 1=PIT active (set by enable-pit-timer).
(defun io-delay ()
  (if (zerop (mem-ref #x600010 :u32))
      (dotimes (d 5000) (io-in-byte #x3F8))
      (progn (sti-hlt) (cli))))

;; Seed entropy from I/O timing (x86 PIT counter at 0x40)
(defun arch-seed-random ()
  (let ((s 0))
    (dotimes (i 4)
      (io-in-byte #x3F8)
      (setq s (logxor (ash s 8) (logand (io-in-byte #x40) 255))))
    (when (zerop s) (setq s 42))
    s))

;; PCI stubs — i386 uses ISA NIC (NE2000), no PCI bus
(defun pci-config-read (bus dev fn reg) 0)
(defun pci-config-write (bus dev fn reg val) nil)
(defun pci-assign-bars () nil)

;; State base addresses for i386
;; Must be above max kernel image + stack (image loads at 0x100000, can reach ~2MB+)
;; Stack at 0x400000 grows down. State areas start at 0x500000.
;; Note: PIT timer flag is at 0x600010 (hardcoded in io-delay) — don't overlap!
(defun e1000-state-base () #x500000)     ; 5MB - shared state (crypto K, MAC, IP, etc.)
(defun ssh-conn-base () #x520000)        ; SSH connection state (4 conns * 0x4000)
(defun ssh-ipc-base () #x540000)         ; SSH IPC shared state (extends to ~0x5A0438)

;; Scratch memory for cons-free field arithmetic (crypto-32-fast.lisp)
;; 256 bytes at state-base + 0x900 (well past used state offsets)
(defun fe-scratch-base () #x500900)

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

;; Enable PIT timer interrupt for HLT-based io-delay
(defun enable-pit-timer ()
  (setup-irq)
  (setf (mem-ref #x600010 :u32) 1))

;; %serial-byte: capture-aware for SSH output routing
(defun %serial-byte (b)
  (let ((flags (mem-ref (+ #x540000 #x14) :u32)))
    (when (zerop (logand flags 2))
      (write-char-serial b))
    (when (not (zerop (logand flags 1)))
      (let ((pos (mem-ref (+ #x540000 #x18) :u32)))
        (when (< pos 4096)
          (setf (mem-ref (+ #x540100 pos) :u8) b)
          (setf (mem-ref (+ #x540000 #x18) :u32) (+ pos 1)))))))

;; Print decimal number (recursive, for small values)
(defun print-dec (n)
  (when (>= n 10)
    (print-dec (truncate n 10)))
  (%serial-byte (+ (mod n 10) 48)))

;; buf-read-u32-mem, buf-read-u32, ssh-get-u32, htonl overrides
;; are in 32bit-overrides.lisp (loaded LAST, after ip.lisp/ssh.lisp)

;; Line editor state at ssh-ipc-base + offsets
;; 0x12800 = line-len, 0x12808 = cursor-pos, 0x12810 = escape-state
;; 0x12A08 = return-code (1=Enter, 2=Ctrl-D)
;; On i386 :u64 = 32-bit load/store with no tag shift (same as on 64-bit)
(defun edit-line-len () (mem-ref (+ #x540000 #x12800) :u64))
(defun edit-set-line-len (v) (setf (mem-ref (+ #x540000 #x12800) :u64) v))
(defun edit-cursor-pos () (mem-ref (+ #x540000 #x12808) :u64))
(defun edit-set-cursor-pos (v) (setf (mem-ref (+ #x540000 #x12808) :u64) v))

;; Eval stubs -- MVM doesn't have native compiler at runtime
(defun native-eval (form) 0)
(defun eval-line-expr (line) 0)
(defun rt-compile-defun (name args body) 0)

;; Prompt
(defun emit-prompt ()
  (if (zerop (mem-ref (+ #x540000 #x12A00) :u64))
      (progn (%serial-byte 62) (%serial-byte 32))
      (progn
        (%serial-byte 109) (%serial-byte 111)
        (%serial-byte 100) (%serial-byte 117)
        (%serial-byte 115) (%serial-byte 62) (%serial-byte 32))))
