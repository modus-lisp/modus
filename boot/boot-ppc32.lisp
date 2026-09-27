;;;; boot-ppc32.lisp - PPC32 Boot Sequence for Modus
;;;;
;;;; PPC32 boot protocol (for qemu-system-ppc -machine ppce500):
;;;;   1. U-Boot or firmware hands off in real mode
;;;;   2. r3 = device tree (FDT) pointer (or 0)
;;;;   3. MSR: IR=0, DR=0 (MMU off), 32-bit mode
;;;;   4. Save DTB pointer
;;;;   5. Set up stack (r1)
;;;;   6. Initialize allocation registers (r19=alloc, r20=limit, r21=NIL)
;;;;   7. Call kernel-main (falls through to translated native code)
;;;;
;;;; UART: ppce500 maps a 16550-compatible UART in the CCSR space.
;;;; For QEMU ppce500, the UART is at 0xE0004500.

(in-package :modus.mvm)

;;; ============================================================
;;; PPC32 Boot Constants
;;; ============================================================

;; QEMU ppce500 memory map
(defconstant +ppc32-uart-base+     #xE0004500)  ; CCSR 16550 UART
(defconstant +ppc32-kernel-base+   #x00000000)  ; Kernel load address
(defconstant +ppc32-stack-top+     #x03F00000)  ; ~63MB - stack top (within 64MB RAM)
(defconstant +ppc32-cons-base+     #x01000000)  ; 16MB - cons space
(defconstant +ppc32-general-base+  #x02000000)  ; 32MB - general heap

;;; ============================================================
;;; PPC32 Boot Code Generation
;;; ============================================================

(defconstant +ppc-tlbwe+ #x7C0007A4
  "tlbwe instruction encoding (X-form: opcode=31, xo=978)")

(defun emit-ppc32-e500-tlb-entry (buf mas0 mas1 mas2 mas3 &optional (mas7 0))
  "Set up an E500 TLB1 entry. Writes MAS0-MAS3 (and MAS7 for 36-bit
   physical addresses) and executes TLBWE. Uses r0 as scratch."
  ;; MAS0 (SPR 624)
  (emit-ppc-insn buf (ppc-lis 0 (logand (ash mas0 -16) #xFFFF)))
  (emit-ppc-insn buf (ppc-ori 0 0 (logand mas0 #xFFFF)))
  (emit-ppc-insn buf (ppc-mtspr 624 0))
  ;; MAS1 (SPR 625)
  (emit-ppc-insn buf (ppc-lis 0 (logand (ash mas1 -16) #xFFFF)))
  (emit-ppc-insn buf (ppc-ori 0 0 (logand mas1 #xFFFF)))
  (emit-ppc-insn buf (ppc-mtspr 625 0))
  ;; MAS2 (SPR 626)
  (emit-ppc-insn buf (ppc-lis 0 (logand (ash mas2 -16) #xFFFF)))
  (emit-ppc-insn buf (ppc-ori 0 0 (logand mas2 #xFFFF)))
  (emit-ppc-insn buf (ppc-mtspr 626 0))
  ;; MAS3 (SPR 627)
  (emit-ppc-insn buf (ppc-lis 0 (logand (ash mas3 -16) #xFFFF)))
  (emit-ppc-insn buf (ppc-ori 0 0 (logand mas3 #xFFFF)))
  (emit-ppc-insn buf (ppc-mtspr 627 0))
  ;; MAS7 (SPR 944) — upper 4 bits of 36-bit physical address
  (when (plusp mas7)
    (emit-ppc-insn buf (ppc-li 0 mas7))
    (emit-ppc-insn buf (ppc-mtspr 944 0)))
  ;; tlbwe
  (emit-ppc-insn buf +ppc-tlbwe+))

(defun emit-ppc32-entry (buf)
  "Emit PPC32 ppce500 kernel entry point.
   Sets up TLB for MMIO, stack, allocation registers,
   and falls through to translated native code."
  ;; --- Save DTB pointer (r3) into r30 (callee-saved, not in vreg map) ---
  (emit-ppc-insn buf (ppc-mr 30 3))         ; mr r30, r3

  ;; --- Enable SPE: MSR[SPE] (#x02000000) ---
  ;; e500v2 has no classic FPU; the translator does doubles with SPE's
  ;; embedded double-precision unit (*ppc-float-isa* :spe), and every SPE
  ;; instruction traps as SPE-unavailable until this bit is set.  Same class as
  ;; bare ppc64's MSR[FP].
  (emit-ppc-insn buf #x7C0000A6)            ; mfmsr r0
  (emit-ppc-insn buf #x64000200)            ; oris  r0, r0, 0x0200  (SPE)
  (emit-ppc-insn buf #x7C000124)            ; mtmsr r0
  (emit-ppc-insn buf #x4C00012C)            ; isync

  ;; --- Set up TLB1 entry for CCSR MMIO space (UART at 0xE0004500) ---
  ;; E500 ppce500 maps CCSR at 36-bit physical address 0x0FE0000000.
  ;; TLB1 entry 15: 1MB at VA 0xE0000000 → PA 0x0FE0000000
  (emit-ppc32-e500-tlb-entry buf
    #x100F0000   ; MAS0: TLBSEL=1 (TLB1), ESEL=15
    #x80000500   ; MAS1: V=1, TSIZE=5 (1MB)
    #xE000000A   ; MAS2: EPN=0xE0000000, I=1 (cache-inhibited), G=1 (guarded)
    #xE0000005   ; MAS3: RPN[31:12]=0xE0000, SW=1, SR=1
    #x0F)        ; MAS7: RPN[35:32]=0x0F

  ;; --- Set up stack pointer (r1 = +ppc32-stack-top+) ---
  (emit-ppc-insn buf (ppc-lis 1 (ash +ppc32-stack-top+ -16)))
  (emit-ppc-insn buf (ppc-ori 1 1 (logand +ppc32-stack-top+ #xFFFF)))

  ;; --- Initialize allocation registers ---
  ;; r19 (VA) = alloc pointer = +ppc32-cons-base+
  (emit-ppc-insn buf (ppc-lis 19 (ash +ppc32-cons-base+ -16)))
  (emit-ppc-insn buf (ppc-ori 19 19 (logand +ppc32-cons-base+ #xFFFF)))
  ;; r20 (VL) = alloc limit = +ppc32-general-base+
  (emit-ppc-insn buf (ppc-lis 20 (ash +ppc32-general-base+ -16)))
  (emit-ppc-insn buf (ppc-ori 20 20 (logand +ppc32-general-base+ #xFFFF)))
  ;; r21 (VN) = NIL (tagged: 0 initially)
  (emit-ppc-insn buf (ppc-li 21 0))

  ;; --- Frame pointer (r31) = 0 (base frame) ---
  (emit-ppc-insn buf (ppc-li 31 0))

  ;; --- Fall through to native translated code ---
  (emit-ppc-insn buf +ppc-nop+))

;;; ============================================================
;;; PPC32 Boot Integration
;;; ============================================================

(defun ppc32-boot-descriptor ()
  "Return the PPC32 boot descriptor for image building"
  (list :arch :ppc32
        :entry-fn #'emit-ppc32-entry
        :elf-machine 20       ; EM_PPC
        :elf-class 32
        :load-addr 0          ; Load entire image at address 0
        :stack-top +ppc32-stack-top+
        :cons-base +ppc32-cons-base+
        :general-base +ppc32-general-base+
        :endianness :big))
