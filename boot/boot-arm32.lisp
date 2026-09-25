;;;; boot-arm32.lisp - ARM32 Boot Sequence for Modus
;;;;
;;;; ARM32 boot protocol (for qemu-system-arm -machine versatilepb):
;;;;   1. QEMU loads kernel at 0x00010000 and jumps to it
;;;;   2. CPU is in ARM mode (A32), SVC mode
;;;;   3. Set up stack (r13)
;;;;   4. Initialize allocation registers (r9=alloc, r10=limit, r8=NIL)
;;;;   5. Frame pointer r11 = 0 (base frame)
;;;;   6. Fall through to translated native code
;;;;
;;;; UART: versatilepb maps a PL011 UART at 0x101F1000.
;;;; Data register at offset 0 — write a byte to transmit.

(in-package :modus.mvm)

;;; ============================================================
;;; ARM32 Boot Constants
;;; ============================================================

;; QEMU versatilepb memory map
(defconstant +arm32-uart-base+     #x101F1000)  ; PL011 UART
(defconstant +arm32-kernel-base+   #x00010000)  ; -kernel load address
(defconstant +arm32-stack-top+     #x04000000)  ; 64MB - stack top
(defconstant +arm32-cons-base+     #x01000000)  ; 16MB - cons space
(defconstant +arm32-general-base+  #x02000000)  ; 32MB - general heap

;;; ============================================================
;;; ARM32 Boot Code Generation
;;; ============================================================

(defun arm32-boot-insn (value)
  "Encode a 32-bit ARM instruction value for boot emission."
  value)

;; ARM immediate encoding helpers for boot
(defun arm32-boot-mov-imm (rd rotate imm8)
  "MOV Rd, #imm  (cond=AL, opcode=MOV=1101, S=0, Rn=0)"
  (logior (ash #xE 28) (ash 1 25) (ash #b1101 21)
          (ash (logand rd #xF) 12)
          (ash (logand rotate #xF) 8)
          (logand imm8 #xFF)))

(defun arm32-boot-orr-imm (rd rn rotate imm8)
  "ORR Rd, Rn, #imm"
  (logior (ash #xE 28) (ash 1 25) (ash #b1100 21)
          (ash (logand rn #xF) 16)
          (ash (logand rd #xF) 12)
          (ash (logand rotate #xF) 8)
          (logand imm8 #xFF)))

(defun arm32-boot-mov-reg (rd rm)
  "MOV Rd, Rm"
  (logior (ash #xE 28) (ash #b1101 21)
          (ash (logand rd #xF) 12)
          (logand rm #xF)))

(defun emit-arm32-insn (buf insn)
  "Emit a 32-bit ARM instruction in little-endian byte order."
  (mvm-emit-byte buf (logand insn #xFF))
  (mvm-emit-byte buf (logand (ash insn -8) #xFF))
  (mvm-emit-byte buf (logand (ash insn -16) #xFF))
  (mvm-emit-byte buf (logand (ash insn -24) #xFF)))

(defun emit-arm32-load-imm (buf rd value)
  "Emit instructions to load a 32-bit immediate into Rd."
  (let ((b0 (logand value #xFF))
        (b1 (logand (ash value -8) #xFF))
        (b2 (logand (ash value -16) #xFF))
        (b3 (logand (ash value -24) #xFF)))
    ;; MOV Rd, #b0
    (emit-arm32-insn buf (arm32-boot-mov-imm rd 0 b0))
    ;; ORR Rd, Rd, #b1, rotate=12  (byte at bits 8-15)
    (when (plusp b1)
      (emit-arm32-insn buf (arm32-boot-orr-imm rd rd 12 b1)))
    ;; ORR Rd, Rd, #b2, rotate=8  (byte at bits 16-23)
    (when (plusp b2)
      (emit-arm32-insn buf (arm32-boot-orr-imm rd rd 8 b2)))
    ;; ORR Rd, Rd, #b3, rotate=4  (byte at bits 24-31)
    (when (plusp b3)
      (emit-arm32-insn buf (arm32-boot-orr-imm rd rd 4 b3)))))

(defun emit-arm32-entry (buf)
  "Emit ARM32 versatilepb kernel entry point.
   Sets up stack, allocation registers, frame pointer,
   and falls through to translated native code."
  ;; --- Set up stack pointer (r13 = +arm32-stack-top+) ---
  (emit-arm32-load-imm buf 13 +arm32-stack-top+)

  ;; --- Initialize allocation registers ---
  ;; r9 (VA) = alloc pointer = +arm32-cons-base+
  (emit-arm32-load-imm buf 9 +arm32-cons-base+)
  ;; r10 (VL) = alloc limit = +arm32-general-base+
  (emit-arm32-load-imm buf 10 +arm32-general-base+)
  ;; r8 (VN) = NIL (tagged: 0)
  (emit-arm32-insn buf (arm32-boot-mov-imm 8 0 0))

  ;; --- Frame pointer (r11) = 0 (base frame) ---
  (emit-arm32-insn buf (arm32-boot-mov-imm 11 0 0))

  ;; --- NOP (fall through to native translated code) ---
  (emit-arm32-insn buf (arm32-boot-mov-reg 0 0)))

;;; ============================================================
;;; ARM32 Boot Integration
;;; ============================================================

(defun arm32-boot-descriptor ()
  "Return the ARM32 boot descriptor for image building."
  (list :arch :arm32
        :entry-fn #'emit-arm32-entry
        :load-addr +arm32-kernel-base+
        :stack-top +arm32-stack-top+
        :cons-base +arm32-cons-base+
        :general-base +arm32-general-base+
        :endianness :little))

;;; ============================================================
;;; ARMv7 Boot Constants (QEMU -machine virt -cpu cortex-a15)
;;; ============================================================

;; QEMU virt machine memory map
(defconstant +armv7-uart-base+     #x09000000)  ; PL011 UART
(defconstant +armv7-kernel-base+   #x40000000)  ; -kernel load address
(defconstant +armv7-stack-top+     #x44000000)  ; 64MB above kernel
(defconstant +armv7-cons-base+     #x41000000)  ; 16MB above kernel
(defconstant +armv7-general-base+  #x42000000)  ; 32MB above kernel

(defun emit-armv7-entry (buf)
  "Emit ARMv7 virt machine kernel entry point.
   Same register setup as ARMv5 but for -machine virt memory layout."
  ;; Stack pointer (r13)
  (emit-arm32-load-imm buf 13 +armv7-stack-top+)
  ;; r9 (VA) = alloc pointer
  (emit-arm32-load-imm buf 9 +armv7-cons-base+)
  ;; r10 (VL) = alloc limit
  (emit-arm32-load-imm buf 10 +armv7-general-base+)
  ;; r8 (VN) = NIL (0)
  (emit-arm32-insn buf (arm32-boot-mov-imm 8 0 0))
  ;; r11 (FP) = 0 (base frame)
  (emit-arm32-insn buf (arm32-boot-mov-imm 11 0 0))
  ;; NOP (fall through to native code)
  (emit-arm32-insn buf (arm32-boot-mov-reg 0 0)))

(defun armv7-boot-descriptor ()
  "Return the ARMv7 boot descriptor for image building."
  (list :arch :armv7
        :entry-fn #'emit-armv7-entry
        :load-addr +armv7-kernel-base+
        :stack-top +armv7-stack-top+
        :cons-base +armv7-cons-base+
        :general-base +armv7-general-base+
        :endianness :little))

;;; ============================================================
;;; ARMv7 RPi Boot Constants (QEMU -machine raspi2b)
;;; ============================================================

;; BCM2836 (Cortex-A7): RAM at 0x0, kernel at 0x8000
;; PL011 UART at 0x3F201000 (same as raspi3b)
;; DWC2 USB at 0x3F980000
(defconstant +armv7-rpi-uart-base+     #x3F201000)
(defconstant +armv7-rpi-kernel-base+   #x00008000)
(defconstant +armv7-rpi-stack-top+     #x07800000)  ; 120MB (above heap, below output buffer)
(defconstant +armv7-rpi-cons-base+     #x01400000)  ; 20MB (after I/O at 0x01000000-0x01200000)
(defconstant +armv7-rpi-general-base+  #x07000000)  ; 112MB (cross-compile needs ~40MB heap)
(defconstant +armv7-rpi-irq-stack+     #x00004000)  ; 16KB IRQ stack (below kernel at 0x8000)
(defconstant +armv7-rpi-isr-addr+      #x00008004)  ; ISR at image+4 (after branch-over)

;;; ============================================================
;;; DWC2 Host-Mode ISR (hand-assembled ARM32)
;;; ============================================================
;;;
;;; Interrupt handler for DWC2 USB host controller on BCM2836 raspi2b.
;;; On DWC2 host channel interrupt, reads HCINT for the active channel
;;; and stores the result to shared memory at usb-ring-base (0x01090000).
;;; The main loop reads this flag instead of polling HCINT via MMIO.
;;;
;;; Shared state at 0x01090000:
;;;   +0x00  hcint_ch0 (u32)   ISR writes HCINT value, main loop reads+clears
;;;   +0x04  hcint_ch1 (u32)   Bulk-in channel (primary receive path)
;;;   +0x08  hcint_ch2 (u32)   Bulk-out channel
;;;   +0x0C  hcint_ch3 (u32)   (reserved)
;;;
;;; Register usage: r0-r3, r12 saved/restored. r14(LR) saved by STMFD.

(defun emit-armv7-rpi-entry (buf)
  "Emit ARMv7 RPi (raspi2b) kernel entry point.
   Sets up SVC stack, IRQ stack, alloc regs, and falls through to kernel."
  ;; --- Set up IRQ mode stack first (before kernel code touches r0) ---
  ;; MRS r0, CPSR
  (emit-arm32-insn buf #xE10F0000)
  ;; BIC r0, r0, #0x1F (clear mode bits)
  (emit-arm32-insn buf #xE3C0001F)
  ;; ORR r0, r0, #0x12 (IRQ mode)
  (emit-arm32-insn buf #xE3800012)
  ;; MSR CPSR_c, r0
  (emit-arm32-insn buf #xE121F000)
  ;; MOV SP, #0x4000 (IRQ stack = 16KB, below kernel at 0x8000)
  (emit-arm32-insn buf #xE3A0DC40)
  ;; Switch back to SVC mode with IRQs disabled
  ;; MRS r0, CPSR
  (emit-arm32-insn buf #xE10F0000)
  ;; BIC r0, r0, #0xFF (clear mode + I/F bits)
  (emit-arm32-insn buf #xE3C000FF)
  ;; ORR r0, r0, #0xD3 (SVC mode + I=1 + F=1)
  (emit-arm32-insn buf #xE38000D3)
  ;; MSR CPSR_c, r0
  (emit-arm32-insn buf #xE121F000)

  ;; --- Set VBAR = 0 (exception vectors at address 0x00000000) ---
  ;; MOV r0, #0
  (emit-arm32-insn buf #xE3A00000)
  ;; MCR p15, 0, r0, c12, c0, 0 (VBAR = r0 = 0)
  (emit-arm32-insn buf #xEE0C0F10)

  ;; --- Enable the VFP: CPACR grants CP10/CP11, then FPEXC.EN ---
  ;; Out of reset both are off, so the first VLDR/VADD is UNDEFINED.  Hosted
  ;; Linux enables the VFP for the process; a bare boot owns these registers and
  ;; must.  Found when arm32 gained double floats: every float rung passed
  ;; hosted and died bare.  (Same class as bare ppc64's MSR[FP].)
  (emit-arm32-insn buf #xEE110F50)   ; MRC p15,0,r0,c1,c0,2   (CPACR)
  (emit-arm32-insn buf #xE380060F)   ; ORR r0,r0,#0x00F00000  (cp10+cp11 full)
  (emit-arm32-insn buf #xEE010F50)   ; MCR p15,0,r0,c1,c0,2
  (emit-arm32-insn buf #xF57FF06F)   ; ISB
  (emit-arm32-insn buf #xE3A00101)   ; MOV r0,#0x40000000     (FPEXC.EN)
  (emit-arm32-insn buf #xEEE80A10)   ; VMSR FPEXC, r0

  ;; --- Set up SVC mode registers ---
  (emit-arm32-load-imm buf 13 +armv7-rpi-stack-top+)
  (emit-arm32-load-imm buf 9 +armv7-rpi-cons-base+)
  (emit-arm32-load-imm buf 10 +armv7-rpi-general-base+)
  (emit-arm32-insn buf (arm32-boot-mov-imm 8 0 0))
  (emit-arm32-insn buf (arm32-boot-mov-imm 11 0 0))
  (emit-arm32-insn buf (arm32-boot-mov-reg 0 0)))

(defun armv7-rpi-boot-descriptor ()
  "Return the ARMv7 RPi boot descriptor for image building."
  (list :arch :armv7-rpi
        :entry-fn #'emit-armv7-rpi-entry
        :load-addr +armv7-rpi-kernel-base+
        :stack-top +armv7-rpi-stack-top+
        :cons-base +armv7-rpi-cons-base+
        :general-base +armv7-rpi-general-base+
        :endianness :little))
