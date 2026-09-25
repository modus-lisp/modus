;;;; boot-ppc64.lisp - PPC64 Boot Sequence for Modus
;;;;
;;;; PPC64 boot protocol (for qemu-system-ppc64 -machine powernv):
;;;;   1. Skiboot firmware hands off in hypervisor real mode
;;;;   2. r3 = device tree (FDT) pointer
;;;;   3. MSR: SF=1 (64-bit), IR=0, DR=0 (MMU off)
;;;;   4. Save DTB pointer
;;;;   5. Set up stack (r1) and TOC (r2)
;;;;   6. Initialize UART (LPC 16550 at 0x60300D00010)
;;;;   7. Set up GC metadata, NFN table, symbol table
;;;;   8. Initialize allocation registers (r19=alloc, r20=limit, r21=NIL)
;;;;   9. Call kernel-main
;;;;
;;;; SMP via OPAL firmware calls:
;;;;   - opal_start_cpu(server_no, start_addr, r3_value) to wake threads

(in-package :modus.mvm)

;;; ============================================================
;;; PPC64 Boot Constants
;;; ============================================================

;; QEMU powernv machine memory map
(defconstant +ppc64-uart-base+     #x60300D00010) ; LPC 16550 UART (ISA I/O via LPC)
(defconstant +ppc64-uart-alt+      #x60300D00000) ; Alternate LPC UART base
(defconstant +ppc64-xscom-base+    #x603FC00000)  ; XSCOM (SCOM over MMIO)
(defconstant +ppc64-kernel-base+   #x20000000)    ; 512MB - kernel load address
(defconstant +ppc64-dram-base+     #x00000000)    ; DRAM start (real mode)

;; Memory regions
(defconstant +ppc64-stack-top+     #x20400000)    ; Stack top (kernel + 4MB)
(defconstant +ppc64-page-tables+   #x20500000)    ; HPT / Radix page tables
(defconstant +ppc64-wired-base+    #x22000000)    ; Wired memory
(defconstant +ppc64-cons-base+     #x24000000)    ; Cons space
(defconstant +ppc64-general-base+  #x25000000)    ; General heap

;; Per-CPU structures
(defconstant +ppc64-percpu-base+   #x20360000)    ; Per-CPU data area
(defconstant +ppc64-percpu-stride+ #x40)          ; 64 bytes per CPU

;; DTB save location (in low memory, below kernel)
(defconstant +ppc64-dtb-save+      #x1FF00000)    ; Saved DTB pointer

;; OPAL function token IDs (for SMP)
(defconstant +opal-start-cpu+      10)
(defconstant +opal-poll-events+    10)

;;; ============================================================
;;; PPC64 Big-Endian Instruction Emission
;;; ============================================================

(defun emit-ppc-insn (buf word)
  "Emit a 32-bit PPC instruction in big-endian byte order.
   The mvm-emit-byte primitives emit raw bytes, so we emit
   MSB first for the big-endian PPC64 target."
  (mvm-emit-byte buf (logand (ash word -24) #xFF))
  (mvm-emit-byte buf (logand (ash word -16) #xFF))
  (mvm-emit-byte buf (logand (ash word -8)  #xFF))
  (mvm-emit-byte buf (logand word #xFF)))

;;; ============================================================
;;; PPC64 Instruction Builders
;;; ============================================================
;;; These return 32-bit instruction words for common PPC64 ops.

(defun ppc-li (rd imm16)
  "li rD, imm16 -> addi rD, 0, imm16 (D-form, opcode 14)"
  (logior (ash 14 26) (ash (logand rd #x1F) 21) (ash 0 16)
          (logand imm16 #xFFFF)))

(defun ppc-lis (rd imm16)
  "lis rD, imm16 -> addis rD, 0, imm16 (D-form, opcode 15)"
  (logior (ash 15 26) (ash (logand rd #x1F) 21) (ash 0 16)
          (logand imm16 #xFFFF)))

(defun ppc-ori (ra rs imm16)
  "ori rA, rS, imm16 (D-form, opcode 24)"
  (logior (ash 24 26) (ash (logand rs #x1F) 21) (ash (logand ra #x1F) 16)
          (logand imm16 #xFFFF)))

(defun ppc-oris (ra rs imm16)
  "oris rA, rS, imm16 (D-form, opcode 25)"
  (logior (ash 25 26) (ash (logand rs #x1F) 21) (ash (logand ra #x1F) 16)
          (logand imm16 #xFFFF)))

(defun ppc-std (rs ds ra)
  "std rS, ds(rA) (DS-form, opcode 62, xo=0).
   DS must be 4-byte aligned (low 2 bits = 0)."
  (logior (ash 62 26) (ash (logand rs #x1F) 21) (ash (logand ra #x1F) 16)
          (logand ds #xFFFC)))

(defun ppc-ld (rd ds ra)
  "ld rD, ds(rA) (DS-form, opcode 58, xo=0).
   DS must be 4-byte aligned."
  (logior (ash 58 26) (ash (logand rd #x1F) 21) (ash (logand ra #x1F) 16)
          (logand ds #xFFFC)))

(defun ppc-or (ra rs rb)
  "or rA, rS, rB (X-form, opcode 31, xo 444)"
  (logior (ash 31 26) (ash (logand rs #x1F) 21) (ash (logand ra #x1F) 16)
          (ash (logand rb #x1F) 11) (ash 444 1)))

(defun ppc-mr (ra rs)
  "mr rA, rS -> or rA, rS, rS"
  (ppc-or ra rs rs))

(defun ppc-mflr (rd)
  "mflr rD -> mfspr rD, LR (SPR 8)"
  (logior (ash 31 26) (ash (logand rd #x1F) 21)
          (ash 8 16)                        ; spr[4:0] = 8, spr[9:5] = 0
          (ash 339 1)))                     ; xo = 339 (mfspr)

(defun ppc-mtlr (rs)
  "mtlr rS -> mtspr LR, rS (SPR 8)"
  (logior (ash 31 26) (ash (logand rs #x1F) 21)
          (ash 8 16)
          (ash 467 1)))                     ; xo = 467 (mtspr)

(defun ppc-mtspr (spr rs)
  "mtspr SPR, rS. SPR encoding: spr[4:0] || spr[9:5] swapped in instruction."
  (let ((spr-lo (logand spr #x1F))
        (spr-hi (logand (ash spr -5) #x1F)))
    (logior (ash 31 26) (ash (logand rs #x1F) 21)
            (ash spr-lo 16) (ash spr-hi 11)
            (ash 467 1))))

(defun ppc-stb (rs d ra)
  "stb rS, d(rA) (D-form, opcode 38)"
  (logior (ash 38 26) (ash (logand rs #x1F) 21) (ash (logand ra #x1F) 16)
          (logand d #xFFFF)))

(defun ppc-lbz (rd d ra)
  "lbz rD, d(rA) (D-form, opcode 34)"
  (logior (ash 34 26) (ash (logand rd #x1F) 21) (ash (logand ra #x1F) 16)
          (logand d #xFFFF)))

(defconstant +ppc-blr+   #x4E800020)   ; blr (branch to link register)
(defconstant +ppc-nop+   #x60000000)   ; nop (ori 0,0,0)

;;; ============================================================
;;; PPC64 Boot Code Generation
;;; ============================================================

(defun emit-ppc64-be64 (buf val)
  "Emit a 64-bit big-endian value as raw data bytes."
  (mvm-emit-byte buf (logand (ash val -56) #xFF))
  (mvm-emit-byte buf (logand (ash val -48) #xFF))
  (mvm-emit-byte buf (logand (ash val -40) #xFF))
  (mvm-emit-byte buf (logand (ash val -32) #xFF))
  (mvm-emit-byte buf (logand (ash val -24) #xFF))
  (mvm-emit-byte buf (logand (ash val -16) #xFF))
  (mvm-emit-byte buf (logand (ash val -8)  #xFF))
  (mvm-emit-byte buf (logand val #xFF)))

(defun emit-ppc64-entry (buf)
  "Emit PPC64 powernv kernel entry point.
   Called by skiboot firmware with:
     r3 = device tree (FDT) pointer
     MSR: SF=1, IR=0, DR=0 (real mode, 64-bit)

   Sets up stack, saves DTB, initializes allocation registers,
   and falls through to translated native code.

   Starts with a PPC64 ELFv1 function descriptor (24 bytes) because
   Skiboot dereferences the ELF entry point as a function descriptor."
  ;; --- ELFv1 Function Descriptor (24 bytes) ---
  ;; Skiboot reads the entry point from the ELF header, then treats
  ;; it as a function descriptor pointer:
  ;;   [0]  actual code entry address
  ;;   [8]  TOC base (r2)
  ;;   [16] environment pointer
  ;; ELF64 headers = 64 + 56 = 120 bytes (0x78).
  ;; Function descriptor = 24 bytes (0x18).
  ;; Code starts at load_addr + 0x78 + 0x18 = load_addr + 0x90.
  (let ((code-addr (+ +ppc64-kernel-base+ #x90)))
    (emit-ppc64-be64 buf code-addr)           ; actual code entry
    (emit-ppc64-be64 buf 0)                   ; TOC = 0
    (emit-ppc64-be64 buf 0))                  ; env = 0

  ;; --- Save DTB pointer (r3) into r30 (callee-saved, not in vreg map) ---
  (emit-ppc-insn buf (ppc-mr 30 3))         ; mr r30, r3

  ;; --- Set up stack pointer (r1 = +ppc64-stack-top+) ---
  ;; Load 0x20400000 via lis + ori
  (emit-ppc-insn buf (ppc-lis 1 #x2040))    ; lis r1, 0x2040
  (emit-ppc-insn buf (ppc-ori 1 1 #x0000))  ; ori r1, r1, 0x0000

  ;; --- Enable the FPU: MSR[FP] (#x2000) ---
  ;; Firmware hands over with FP off, and the first LFD/FADD then takes a
  ;; floating-point-unavailable interrupt that nothing here handles.  Hosted
  ;; Linux enables it for the process; a bare boot owns the MSR and must.  Found
  ;; when PowerPC gained double floats: every float rung passed hosted and hung
  ;; bare.  MTMSRD (L=0) writes the whole MSR back, SF included, from MFMSR.
  (emit-ppc-insn buf #x7C0000A6)            ; mfmsr  r0
  (emit-ppc-insn buf #x60002000)            ; ori    r0, r0, 0x2000  (FP)
  (emit-ppc-insn buf #x7C000164)            ; mtmsrd r0
  (emit-ppc-insn buf #x4C00012C)            ; isync

  ;; --- Set up TOC (r2 = 0, no shared library TOC in bare metal) ---
  (emit-ppc-insn buf (ppc-li 2 0))          ; li r2, 0

  ;; --- Store DTB pointer to known memory location ---
  ;; r11 = +ppc64-dtb-save+ (0x1FF00000)
  (emit-ppc-insn buf (ppc-lis 11 #x1FF0))   ; lis r11, 0x1FF0
  (emit-ppc-insn buf (ppc-ori 11 11 #x0000)) ; ori r11, r11, 0x0000
  (emit-ppc-insn buf (ppc-std 30 0 11))     ; std r30, 0(r11)

  ;; --- Initialize allocation registers ---
  ;; r19 (VA) = alloc pointer = +ppc64-cons-base+ (0x24000000)
  (emit-ppc-insn buf (ppc-lis 19 #x2400))   ; lis r19, 0x2400
  (emit-ppc-insn buf (ppc-ori 19 19 #x0000)) ; ori r19, r19, 0x0000
  ;; r20 (VL) = alloc limit = +ppc64-general-base+ (0x25000000)
  (emit-ppc-insn buf (ppc-lis 20 #x2500))   ; lis r20, 0x2500
  (emit-ppc-insn buf (ppc-ori 20 20 #x0000)) ; ori r20, r20, 0x0000
  ;; r21 (VN) = NIL (tagged: use 0 initially, patched by kernel-main)
  (emit-ppc-insn buf (ppc-li 21 0))         ; li r21, 0

  ;; --- Set r13 (thread pointer) = per-CPU base for CPU 0 ---
  (emit-ppc-insn buf (ppc-lis 13 (ash +ppc64-percpu-base+ -16)))
  (emit-ppc-insn buf (ppc-ori 13 13 (logand +ppc64-percpu-base+ #xFFFF)))

  ;; --- Also set SPRG0 to per-CPU pointer (fast access in interrupts) ---
  (emit-ppc-insn buf (ppc-mtspr 272 13))    ; mtsprg0 r13

  ;; --- Initialize serial for early debug output ---
  ;; Inline call: the UART init is done via ppc64-init-serial below
  ;; which emits MMIO writes. For the entry stub, emit NOPs as
  ;; placeholders; the actual init sequence is in the descriptor.

  ;; --- Frame pointer (r31) = 0 (base frame) ---
  (emit-ppc-insn buf (ppc-li 31 0))         ; li r31, 0

  ;; --- Fall through to native translated code ---
  ;; (no branch needed; kernel-main code follows immediately)
  (emit-ppc-insn buf +ppc-nop+))            ; alignment nop

;;; ============================================================
;;; PPC64 UART (16550 on LPC bus)
;;; ============================================================

(defun ppc64-init-serial (buf)
  "Emit PPC64 code to initialize the 16550 UART on the LPC bus.
   For QEMU powernv, the UART is memory-mapped via LPC at a high
   physical address. We use a simplified base (0x60300D00010) but
   for stub purposes emit register writes using r11 as base and
   r12 as data scratch.

   Configures 115200 baud, 8N1, FIFO enabled."
  ;; Load UART base address into r11
  ;; 0x60300D00010 requires 5 steps to build the 48-bit address:
  ;; For the stub we use the simpler LPC legacy UART at 0x60300D00000
  ;; and offset from there.
  ;;
  ;; Simplification: QEMU powernv also maps a UART at the OPAL console;
  ;; we write to the LPC address space.  Build address in r11:
  ;; r11 = 0x00060030_0D000010 -- but that exceeds 48 bits.
  ;; Actual powernv LPC UART is at 0x0006_0300_D000_0010.
  ;; In practice: lis + oris + ori chain for the upper bits, then offset.
  ;;
  ;; For the boot stub we use a simple approach: just emit the UART
  ;; register configuration as data (address, value) pairs. The boot
  ;; integration code interprets this.

  ;; Disable interrupts: UART+1 = 0x00
  (emit-ppc-insn buf (ppc-li 12 #x00))       ; li r12, 0x00
  (emit-ppc-insn buf (ppc-stb 12 1 11))      ; stb r12, 1(r11)
  ;; Enable DLAB: UART+3 = 0x80
  (emit-ppc-insn buf (ppc-li 12 #x80))       ; li r12, 0x80
  (emit-ppc-insn buf (ppc-stb 12 3 11))      ; stb r12, 3(r11)
  ;; Baud rate divisor LSB = 1 (115200 baud): UART+0 = 0x01
  (emit-ppc-insn buf (ppc-li 12 #x01))       ; li r12, 0x01
  (emit-ppc-insn buf (ppc-stb 12 0 11))      ; stb r12, 0(r11)
  ;; Baud rate divisor MSB = 0: UART+1 = 0x00
  (emit-ppc-insn buf (ppc-li 12 #x00))       ; li r12, 0x00
  (emit-ppc-insn buf (ppc-stb 12 1 11))      ; stb r12, 1(r11)
  ;; 8N1: UART+3 = 0x03
  (emit-ppc-insn buf (ppc-li 12 #x03))       ; li r12, 0x03
  (emit-ppc-insn buf (ppc-stb 12 3 11))      ; stb r12, 3(r11)
  ;; Enable FIFO: UART+2 = 0xC7
  (emit-ppc-insn buf (ppc-li 12 #xC7))       ; li r12, 0xC7
  (emit-ppc-insn buf (ppc-stb 12 2 11))      ; stb r12, 2(r11)
  ;; RTS/DSR set: UART+4 = 0x0B
  (emit-ppc-insn buf (ppc-li 12 #x0B))       ; li r12, 0x0B
  (emit-ppc-insn buf (ppc-stb 12 4 11)))     ; stb r12, 4(r11)

;;; ============================================================
;;; PPC64 SMP via OPAL
;;; ============================================================

(defun ppc64-start-cpu (server-no entry-addr r3-value)
  "Generate OPAL call description to start a secondary CPU thread.
   Uses opal_start_cpu(server_no, start_addr, r3_value).
   The OPAL call convention passes token in r0, args in r3-r5."
  (list :opal-call
        :token +opal-start-cpu+
        :args (list server-no entry-addr r3-value)))

;;; ============================================================
;;; PPC64 Per-CPU Layout
;;; ============================================================

(defun ppc64-percpu-layout ()
  "Per-CPU structure for PPC64.
   Accessed via r13 (thread pointer) or SPRG0 in interrupt context."
  '((:self-ptr       0   8)    ; Pointer to this per-CPU struct
    (:reduction       8   8)    ; Reduction counter (tagged fixnum)
    (:cpu-id         16   8)    ; CPU/thread number (tagged fixnum)
    (:current-actor  24   8)    ; Current actor pointer
    (:obj-alloc      40   8)    ; Per-actor object alloc pointer
    (:obj-limit      48   8)    ; Per-actor object alloc limit
    (:scratch-stack  56   8)))  ; Scratch/idle stack for this CPU

;;; ============================================================
;;; PPC64 Boot Integration
;;; ============================================================

(defun ppc64-boot-descriptor ()
  "Return the PPC64 boot descriptor for image building"
  (list :arch :ppc64
        :entry-fn #'emit-ppc64-entry
        :serial-init-fn #'ppc64-init-serial
        :smp-start-fn #'ppc64-start-cpu
        :percpu-layout-fn #'ppc64-percpu-layout
        :elf-machine 21       ; EM_PPC64
        :elf-class 64
        :elf-flags 2          ; EF_PPC64_ABI_V2 (direct entry, not function descriptor)
        :load-addr +ppc64-kernel-base+
        :stack-top +ppc64-stack-top+
        :cons-base +ppc64-cons-base+
        :general-base +ppc64-general-base+
        :percpu-base +ppc64-percpu-base+
        :percpu-stride +ppc64-percpu-stride+
        :dtb-save-addr +ppc64-dtb-save+
        :endianness :big))
