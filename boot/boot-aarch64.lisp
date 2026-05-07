;;;; boot-aarch64.lisp - AArch64 Boot Sequence for Modus
;;;;
;;;; AArch64 boot protocol (for -machine virt with UEFI or direct kernel):
;;;;   1. Firmware hands off in EL1 (or EL2 with virtualization)
;;;;   2. x0 = DTB pointer (or ACPI pointer)
;;;;   3. Set up page tables (4KB granule, 4-level or 3-level)
;;;;   4. Enable MMU (SCTLR_EL1.M = 1)
;;;;   5. Set up exception vectors (VBAR_EL1)
;;;;   6. Initialize PL011 UART (0x09000000 on QEMU virt)
;;;;   7. Set up GC metadata, NFN table, symbol table
;;;;   8. Initialize allocation registers (x24=alloc, x25=limit, x26=NIL)
;;;;   9. Call kernel-main
;;;;
;;;; SMP via PSCI (Power State Coordination Interface):
;;;;   - PSCI CPU_ON to wake secondary cores

(in-package :modus.mvm)

;;; ============================================================
;;; AArch64 Boot Constants
;;; ============================================================

;; QEMU virt machine memory map
(defconstant +aarch64-uart-base+     #x09000000)   ; PL011 UART
(defconstant +aarch64-gic-dist+      #x08000000)   ; GIC Distributor
(defconstant +aarch64-gic-cpu+       #x08010000)   ; GIC CPU Interface
(defconstant +aarch64-gic-redist+    #x080A0000)   ; GICv3 Redistributor
(defconstant +aarch64-dram-base+     #x40000000)   ; DRAM start
(defconstant +aarch64-kernel-base+   #x40200000)   ; Kernel load address

;; Memory regions
(defconstant +aarch64-stack-top+     #x40400000)   ; Stack top
(defconstant +aarch64-page-tables+   #x40500000)   ; Page tables
(defconstant +aarch64-wired-base+    #x42000000)   ; Wired memory
(defconstant +aarch64-cons-base+     #x44000000)   ; Cons space
(defconstant +aarch64-general-base+  #x45000000)   ; General heap

;; Page table constants (4KB granule, Stage 1)
(defconstant +aarch64-page-size+     4096)
(defconstant +aarch64-pte-valid+     #x01)
(defconstant +aarch64-pte-table+     #x02)          ; Table descriptor (next level)
(defconstant +aarch64-pte-af+        (ash 1 10))    ; Access flag
(defconstant +aarch64-pte-sh-inner+  (ash 3 8))     ; Inner shareable
(defconstant +aarch64-pte-mair-wb+   (ash 0 2))     ; MAIR index 0 (write-back)

;; PSCI function IDs (SMC/HVC calling convention)
(defconstant +psci-cpu-on-64+        #xC4000003)
(defconstant +psci-cpu-off+          #x84000002)
(defconstant +psci-system-reset+     #x84000009)

;; Timer
(defconstant +aarch64-timer-irq+     27)            ; Virtual timer PPI

;;; ============================================================
;;; AArch64 Boot Code Generation
;;; ============================================================

(defun emit-aarch64-u32 (buf word)
  "Emit a 32-bit little-endian instruction into BUF."
  (mvm-emit-byte buf (ldb (byte 8  0) word))
  (mvm-emit-byte buf (ldb (byte 8  8) word))
  (mvm-emit-byte buf (ldb (byte 8 16) word))
  (mvm-emit-byte buf (ldb (byte 8 24) word)))

(defun emit-aarch64-movz (buf rd imm16 &optional (shift 0))
  "MOVZ Xd, #imm16{, LSL #shift}  shift=0,16,32,48"
  (let ((hw (/ shift 16)))
    (emit-aarch64-u32 buf (logior (ash 1 31)     ; sf=1
                                  (ash #b10 29)   ; opc=10 (MOVZ)
                                  (ash #b100101 23)
                                  (ash hw 21)
                                  (ash imm16 5)
                                  rd))))

(defun emit-aarch64-movk (buf rd imm16 &optional (shift 0))
  "MOVK Xd, #imm16{, LSL #shift}"
  (let ((hw (/ shift 16)))
    (emit-aarch64-u32 buf (logior (ash 1 31)     ; sf=1
                                  (ash #b11 29)   ; opc=11 (MOVK)
                                  (ash #b100101 23)
                                  (ash hw 21)
                                  (ash imm16 5)
                                  rd))))

(defun emit-aarch64-mov-sp (buf rd rn)
  "MOV Xd, Xn (using ADD Xd, Xn, #0 to handle SP correctly)"
  (emit-aarch64-u32 buf (logior (ash 1 31)       ; sf=1
                                (ash #b00 29)     ; op=0 (ADD), S=0
                                (ash #b100010 23) ; ADD imm
                                (ash 0 22)        ; shift=0
                                (ash 0 10)        ; imm12=0
                                (ash rn 5)
                                rd)))

(defun emit-aarch64-strb (buf rt rn)
  "STRB Wt, [Xn]  (unsigned offset 0)"
  (emit-aarch64-u32 buf (logior (ash #b00 30)     ; size=00 (byte)
                                (ash #b111001 24)  ; opc=00, STR
                                (ash 0 22)         ; opc=00
                                (ash 0 10)         ; imm12=0
                                (ash rn 5)
                                rt)))

(defun emit-aarch64-entry (buf)
  "Emit AArch64 EL1 kernel entry point.
   QEMU virt starts execution at the load address (0x40000000 or 0x40200000)
   with registers mostly zeroed. We need to:
   1. Set up SP
   2. Initialize PL011 UART at 0x09000000
   3. Set up allocation registers
   4. Fall through to native code"
  (let ((sp 31)   ; SP encoding in ADD/SUB context
        (x0 0) (x1 1) (x16 16) (x17 17)
        (x24 24) (x25 25) (x26 26))
    ;; 1. Set up stack pointer
    ;; MOV x16, #0x4040  (x16 = 0x40400000 = stack top)
    ;; MOVK x16, #0x0000, LSL #0
    ;; MOV SP, x16
    (emit-aarch64-movz buf x16 #x4040 16)   ; x16 = 0x40400000
    (emit-aarch64-mov-sp buf sp x16)         ; SP = x16

    ;; 2. Initialize PL011 UART at 0x09000000
    ;; Load UART base into x17
    (emit-aarch64-movz buf x17 #x0900 16)   ; x17 = 0x09000000

    ;; Disable UART: str wzr, [x17, #0x30] (UARTCR = 0)
    ;; Use a simpler approach: store bytes for key registers
    ;; UARTCR offset 0x30 = 48
    ;; For QEMU PL011, UART is already usable — just need to enable TX
    ;; Write UARTCR = 0x0301 (UARTEN | TXE | RXE)
    ;; Load 0x0301 into x0, store to [x17 + 0x30]
    (emit-aarch64-movz buf x0 #x0301 0)
    ;; STR w0, [x17, #0x30]  — unsigned offset, 32-bit store
    ;; Encoding: size=10 | 111001 | 00 | imm12 | Rn | Rt
    ;; imm12 = 0x30 / 4 = 12 (scaled by 4 for 32-bit)
    (emit-aarch64-u32 buf (logior (ash #b10 30)     ; size=10 (32-bit)
                                  (ash #b111001 24)  ; STR
                                  (ash 0 22)         ; opc=00
                                  (ash 12 10)        ; imm12=12 (offset 48/4)
                                  (ash x17 5)        ; Rn
                                  x0))               ; Rt

    ;; Set UARTLCR_H = 0x70 (8-bit, FIFO enable)
    (emit-aarch64-movz buf x0 #x70 0)
    ;; STR w0, [x17, #0x2C]  imm12 = 0x2C / 4 = 11
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 11 10) (ash x17 5) x0))

    ;; Set UARTIBRD = 13  [x17 + #0x24]  imm12 = 0x24/4 = 9
    (emit-aarch64-movz buf x0 13 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 9 10) (ash x17 5) x0))

    ;; Set UARTFBRD = 1  [x17 + #0x28]  imm12 = 0x28/4 = 10
    (emit-aarch64-movz buf x0 1 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 10 10) (ash x17 5) x0))

    ;; Re-enable UART: UARTCR = 0x0301
    (emit-aarch64-movz buf x0 #x0301 0)
    (emit-aarch64-u32 buf (logior (ash #b10 30) (ash #b111001 24) (ash 0 22)
                                  (ash 12 10) (ash x17 5) x0))

    ;; 3. Initialize allocation registers
    ;; x24 = cons alloc pointer = 0x44000000
    (emit-aarch64-movz buf x24 #x4400 16)
    ;; x25 = alloc limit = 0x45000000
    (emit-aarch64-movz buf x25 #x4500 16)
    ;; x26 = NIL = 0 (already zero from QEMU reset, but be explicit)
    (emit-aarch64-movz buf x26 0 0)

    ;; 4. Set TPIDR_EL1 = BSP per-CPU data base (0x41200000)
    (emit-aarch64-movz buf x16 #x4120 16)     ; x16 = 0x41200000
    (emit-aarch64-u32 buf #xD518D090)          ; MSR TPIDR_EL1, x16

    ;; 5. Set VBAR_EL1 = exception vector table (at image offset 0x800)
    ;; Image loads at 0x40000000 on QEMU virt, so vectors at 0x40000800
    (emit-aarch64-movz buf x16 #x4000 16)     ; x16 = 0x40000000
    (emit-aarch64-movk buf x16 #x0800 0)      ; x16 = 0x40000800
    (emit-aarch64-u32 buf #xD518C010)          ; MSR VBAR_EL1, x16
    (emit-aarch64-u32 buf #xD5033FDF)          ; ISB (sync system reg writes)

    ;; 6. Branch over exception vectors to native code
    ;; Vectors occupy 0x800-0x1000 (2KB). Native code starts at offset 0x1000.
    (let* ((current-insn (/ (mvm-buffer-position buf) 4))
           (native-start 1024)                  ; instruction 1024 = offset 0x1000
           (skip (- native-start current-insn)))
      ;; B forward to native code
      (emit-aarch64-u32 buf (logior (ash #b000101 26) (logand skip #x3FFFFFF)))
      ;; Pad with NOPs to offset 0x800 (instruction 512)
      (let ((pad (- 512 (/ (mvm-buffer-position buf) 4))))
        (dotimes (i pad)
          (emit-aarch64-u32 buf #xD503201F))))

    ;; 7. Exception vector table (2KB at offset 0x800)
    (emit-aarch64-exception-vectors buf)
    ;; Native code follows immediately (kernel-main at offset 0x1000)
    ))

(defun emit-aarch64-exception-vectors (buf)
  "Emit AArch64 exception vector table.
   Must be aligned to 2KB (0x800).
   4 exception levels × 4 types = 16 entries, 32 instructions each.
   Entry 5 (offset 0x280, Current EL with SP_ELx IRQ) has a real
   GICv2 IRQ handler. All others are infinite loops for debugging."
  ;; Exception vectors layout (16 entries × 32 instructions = 512 words):
  ;;   Entry 0 (0x000): Current EL, SP_EL0, Sync  → B .
  ;;   Entry 1 (0x080): Current EL, SP_EL0, IRQ   → B .
  ;;   Entry 2 (0x100): Current EL, SP_EL0, FIQ   → B .
  ;;   Entry 3 (0x180): Current EL, SP_EL0, SError → B .
  ;;   Entry 4 (0x200): Current EL, SP_ELx, Sync  → B .
  ;;   Entry 5 (0x280): Current EL, SP_ELx, IRQ   → GIC IRQ handler
  ;;   Entry 6 (0x300): Current EL, SP_ELx, FIQ   → B .
  ;;   Entry 7-15: Lower EL vectors               → B .
  (dotimes (entry 16)
    (cond
      ((= entry 4)
       ;; Entry 4: Sync exception, Current EL with SP_ELx.
       ;; This is the SIGSEGV-equivalent path on bare-metal AArch64.
       ;; Mirrors x64 SIGSEGV stub (translate-x64.lisp #x0520):
       ;; if a handler-case is active (saved SP at 0x10000180 != 0),
       ;; restore SP/FP/IP from saved slots and ERET back to it,
       ;; with X0 = T to signal "longjmp-return".  Otherwise fall
       ;; into B . (debug halt).
       ;;
       ;; Capture state to diag slots (mirror x64 layout) FIRST so a
       ;; later FAIL line can print useful debug info:
       ;;   0x10000C30 = ELR (faulting PC)
       ;;   0x10000C40 = FAR (fault addr / "site" on x64)
       ;;   0x10000C50 = X0 at fault
       ;;
       ;; Layout of this entry (need <= 32 instructions = 128 bytes):
       ;;   1.  MRS x16, ELR_EL1           ; faulting PC
       ;;   2.  MOVZ x17, #0x0c30          ; addr lo
       ;;   3.  MOVK x17, #0x1000, lsl #16 ; addr hi
       ;;   4.  STR x16, [x17]             ; save ELR
       ;;   5.  MRS x16, FAR_EL1           ; fault addr
       ;;   6.  STR x16, [x17, #0x10]      ; save FAR (slot C40)
       ;;   7.  STR x0,  [x17, #0x18]      ; save X0 (slot C48)
       ;;   8.  MOVZ x16, #0x0180          ; addr lo
       ;;   9.  MOVK x16, #0x1000, lsl #16 ; addr hi
       ;;  10.  LDR x17, [x16]             ; saved SP (handler-case state)
       ;;  11.  CBZ x17, +N                ; if 0, no active handler
       ;;  12.  ADD sp, x17, #0            ; restore SP (MOV SP)
       ;;  13.  LDR x29, [x16, #8]         ; restore FP
       ;;  14.  LDR x17, [x16, #16]        ; saved IP
       ;;  15.  MSR ELR_EL1, x17           ; ELR = saved IP
       ;;  16.  MOVZ x0, #0x1009           ; X0 lo = T low
       ;;  17.  MOVK x0, #0xDEAD, lsl #16  ; X0 = T (#xDEAD1009)
       ;;  18.  ERET
       ;;  19.  B .                         (no-handler halt — CBZ target)
       (mvm-emit-u32 buf #xD5384030)        ; MRS x16, ELR_EL1
       (mvm-emit-u32 buf #xD2818611)        ; MOVZ x17, #0x0c30
       (mvm-emit-u32 buf #xF2A20011)        ; MOVK x17, #0x1000, lsl #16
       (mvm-emit-u32 buf #xF9000230)        ; STR x16, [x17]
       (mvm-emit-u32 buf #xD5386010)        ; MRS x16, FAR_EL1
       (mvm-emit-u32 buf #xF9000A30)        ; STR x16, [x17, #0x10]
       (mvm-emit-u32 buf #xF9000E20)        ; STR x0,  [x17, #0x18]
       (mvm-emit-u32 buf #xD2803010)        ; MOVZ x16, #0x0180
       (mvm-emit-u32 buf #xF2A20010)        ; MOVK x16, #0x1000, lsl #16
       (mvm-emit-u32 buf #xF9400211)        ; LDR x17, [x16]
       (mvm-emit-u32 buf #xB4000111)        ; CBZ x17, +8 instructions — lands on B .
       (mvm-emit-u32 buf #x9100023F)        ; ADD sp, x17, #0
       (mvm-emit-u32 buf #xF940061D)        ; LDR x29, [x16, #8]
       (mvm-emit-u32 buf #xF9400A11)        ; LDR x17, [x16, #16]
       (mvm-emit-u32 buf #xD5184031)        ; MSR ELR_EL1, x17
       (mvm-emit-u32 buf #xD2820120)        ; MOVZ x0, #0x1009
       (mvm-emit-u32 buf #xF2BBD5A0)        ; MOVK x0, #0xDEAD, lsl #16
       (mvm-emit-u32 buf #xD69F03E0)        ; ERET
       (mvm-emit-u32 buf #x14000000)        ; B .  (no handler — halt here)
       ;; Fill remaining 13 instructions with NOP
       (dotimes (i 13)
         (mvm-emit-u32 buf #xD503201F)))
      ((= entry 5)
       ;; Entry 5: IRQ handler for Current EL with SP_ELx.
       ;;
       ;; Per-test wall-clock deadline.  Slot 0x10000C70 is a tick
       ;; countdown.  Each timer IRQ decrements it; when it reaches
       ;; zero AND a handler-case is active (slot 0x10000180 != 0),
       ;; we longjmp to the handler-case as if a sync exception fired
       ;; (mirrors entry 4's longjmp path: SP/FP/IP from saved slots,
       ;; X0 = T, ERET).  This unblocks test runs that hit infinite
       ;; loops: ANSI tests like (acons) called with too few args
       ;; previously hung the entire suite.
       ;;
       ;; Slot 0x10000180 base + #2800 offset reaches 0x10000C70, so
       ;; we load the address once (MOVZ + MOVK) and reuse it for
       ;; both the deadline slot and the handler-case state.
       ;;
       ;; Layout (need <= 32 instructions = 128 bytes):
       ;;   1.  STP x0, x1, [SP, #-16]!     ; save scratch
       ;;   2.  MOVZ x0, #0x0801, LSL #16   ; GICC base
       ;;   3.  LDR  w1, [x0, #0x0C]        ; GICC_IAR (acknowledge)
       ;;   4.  STR  w1, [x0, #0x10]        ; GICC_EOIR (end of int)
       ;;   5.  MOVZ x0, #0xF424            ; 62500 (1ms @ 62.5MHz)
       ;;   6.  MSR  CNTV_TVAL_EL0, x0      ; re-arm timer
       ;;   7.  MOVZ x0, #0x0180            ; addr lo
       ;;   8.  MOVK x0, #0x1000, LSL #16   ; addr hi → 0x10000180
       ;;   9.  LDR  x1, [x0, #2800]        ; deadline (slot 0xC70)
       ;;  10.  CBZ  x1, NORMAL (+13)        ; not armed → return
       ;;  11.  SUBS x1, x1, #1
       ;;  12.  STR  x1, [x0, #2800]        ; store decremented
       ;;  13.  B.NE NORMAL (+10)            ; not yet zero → return
       ;;  14.  LDR  x1, [x0, #0]            ; saved SP from setjmp
       ;;  15.  CBZ  x1, NORMAL (+8)         ; no handler-case → return
       ;;  16.  ADD  SP, x1, #0              ; restore SP
       ;;  17.  LDR  x29, [x0, #8]           ; restore FP
       ;;  18.  LDR  x1, [x0, #16]           ; saved IP
       ;;  19.  MSR  ELR_EL1, x1
       ;;  20.  MOVZ x0, #0x1009             ; X0 lo
       ;;  21.  MOVK x0, #0xDEAD, LSL #16    ; X0 = T
       ;;  22.  ERET                          ; longjmp to handler-case
       ;;  23.  NORMAL: LDP x0, x1, [SP], #16
       ;;  24.  ERET
       (progn
         (mvm-emit-u32 buf #xA9BF07E0)  ; STP x0,x1,[SP,#-16]!
         (mvm-emit-u32 buf #xD2A10020)  ; MOVZ x0,#0x0801,LSL #16  (GICC base)
         (mvm-emit-u32 buf #xB9400C01)  ; LDR w1,[x0,#0x0C]        (GICC_IAR)
         (mvm-emit-u32 buf #xB9001001)  ; STR w1,[x0,#0x10]        (GICC_EOIR)
         (mvm-emit-u32 buf #xD29E8480)  ; MOVZ x0,#0xF424          (62500)
         (mvm-emit-u32 buf #xD51BE300)  ; MSR CNTV_TVAL_EL0,x0     (re-arm)
         (mvm-emit-u32 buf #xD2803000)  ; MOVZ x0,#0x0180
         (mvm-emit-u32 buf #xF2A20000)  ; MOVK x0,#0x1000,LSL #16  (x0 = 0x10000180)
         (mvm-emit-u32 buf #xF9457801)  ; LDR x1,[x0,#2800]        (deadline @ slot 0xC70)
         (mvm-emit-u32 buf #xB4000201)  ; CBZ x1,+16 → NORMAL      (not armed)
         (mvm-emit-u32 buf #xF1000421)  ; SUBS x1,x1,#1
         (mvm-emit-u32 buf #xF9057801)  ; STR x1,[x0,#2800]
         (mvm-emit-u32 buf #x540001A1)  ; B.NE +13 → NORMAL        (not yet zero)
         ;; Deadline expired this tick — try slot 0x10000180 (per-test
         ;; handler) first; if zero, fall back to slot 0x100001A0
         ;; (file-level outer handler set by SAVE-OUTER trap).
         (mvm-emit-u32 buf #xF9400001)  ; LDR x1,[x0]              (slot 180 SP)
         (mvm-emit-u32 buf #xB5000081)  ; CBNZ x1,+4 → DO_LJ       (use slot 180)
         (mvm-emit-u32 buf #x91010000)  ; ADD x0,x0,#0x40          (→ slot 1C0)
         (mvm-emit-u32 buf #xF9400001)  ; LDR x1,[x0]              (slot 1A0 SP)
         (mvm-emit-u32 buf #xB4000101)  ; CBZ x1,+8 → NORMAL       (no handler at all)
         ;; DO_LJ:
         (mvm-emit-u32 buf #x9100003F)  ; ADD SP,x1,#0
         (mvm-emit-u32 buf #xF940041D)  ; LDR x29,[x0,#8]
         (mvm-emit-u32 buf #xF9400801)  ; LDR x1,[x0,#16]
         (mvm-emit-u32 buf #xD5184021)  ; MSR ELR_EL1,x1
         (mvm-emit-u32 buf #xD2820120)  ; MOVZ x0,#0x1009
         (mvm-emit-u32 buf #xF2BBD5A0)  ; MOVK x0,#0xDEAD,LSL #16  (X0 = T)
         (mvm-emit-u32 buf #xD69F03E0)  ; ERET (longjmp)
         ;; NORMAL:
         (mvm-emit-u32 buf #xA8C107E0)  ; LDP x0,x1,[SP],#16
         (mvm-emit-u32 buf #xD69F03E0)  ; ERET (normal return)
         ;; Fill remaining 5 instructions with NOP
         (dotimes (i 5)
           (mvm-emit-u32 buf #xD503201F))))
      ((= entry 6)
       ;; DIAGNOSTIC: FIQ probe — write 'f' to UART each tick.  GICv2 on
       ;; QEMU virt routes Group 0 interrupts as FIQ to non-secure EL1,
       ;; and the virtual timer is Group 0 by default.  If we see 'f'
       ;; chars but no '!', the vtimer fires as FIQ not IRQ.
       (progn
         (mvm-emit-u32 buf #xA9BF07E0)  ; STP x0,x1,[SP,#-16]!
         (mvm-emit-u32 buf #xD2A40000)  ; MOVZ x0,#0x2000,LSL #16  (UART VA)
         (mvm-emit-u32 buf #xD2800CC1)  ; MOVZ x1,#0x66            ('f')
         (mvm-emit-u32 buf #xB9000001)  ; STR  w1,[x0]
         (mvm-emit-u32 buf #xD2A10020)  ; MOVZ x0,#0x0801,LSL #16  (GICC base)
         (mvm-emit-u32 buf #xB9400C01)  ; LDR  w1,[x0,#0x0C]       (GICC_IAR)
         (mvm-emit-u32 buf #xB9001001)  ; STR  w1,[x0,#0x10]       (GICC_EOIR)
         (mvm-emit-u32 buf #xD29E8480)  ; MOVZ x0,#0xF424          (62500)
         (mvm-emit-u32 buf #xD51BE300)  ; MSR  CNTV_TVAL_EL0,x0    (re-arm)
         (mvm-emit-u32 buf #xA8C107E0)  ; LDP  x0,x1,[SP],#16
         (mvm-emit-u32 buf #xD69F03E0)  ; ERET
         (dotimes (i 21)
           (mvm-emit-u32 buf #xD503201F))))
      (t
       ;; All other entries: B . (infinite loop for debugging)
       (progn
         (mvm-emit-u32 buf #x14000000)    ; B . (branch to self)
         (dotimes (i 31)
           (mvm-emit-u32 buf #xD503201F)))))))

;;; ============================================================
;;; AArch64 PL011 UART
;;; ============================================================

(defun aarch64-init-uart ()
  "Return PL011 UART initialization sequence.
   PL011 on QEMU virt at 0x09000000."
  (let ((base +aarch64-uart-base+))
    (list
     ;; Disable UART
     (cons (+ base #x30) #x0000)     ; UARTCR = 0
     ;; Set baud rate (115200 with 24MHz clock)
     ;; IBRD = 24000000 / (16 * 115200) = 13
     ;; FBRD = round(0.0208 * 64) = 1
     (cons (+ base #x24) 13)          ; UARTIBRD
     (cons (+ base #x28) 1)           ; UARTFBRD
     ;; 8N1, enable FIFO
     (cons (+ base #x2C) #x70)        ; UARTLCR_H: 8-bit, FIFO enable
     ;; Enable UART, TX, RX
     (cons (+ base #x30) #x0301))))   ; UARTCR: UARTEN | TXE | RXE

;;; ============================================================
;;; AArch64 GIC (Generic Interrupt Controller)
;;; ============================================================

(defun aarch64-init-gic ()
  "GIC initialization sequence (GICv2 for QEMU virt default)."
  (list
   ;; Enable distributor
   (cons (+ +aarch64-gic-dist+ #x000) 1)    ; GICD_CTLR = enable
   ;; Enable CPU interface
   (cons (+ +aarch64-gic-cpu+ #x000) 1)     ; GICC_CTLR = enable
   ;; Set priority mask (allow all)
   (cons (+ +aarch64-gic-cpu+ #x004) #xFF)  ; GICC_PMR = 0xFF
   ;; Enable timer interrupt (PPI 27)
   ;; GICD_ISENABLER1 (PPI enables)
   (cons (+ +aarch64-gic-dist+ #x104) (ash 1 (- +aarch64-timer-irq+ 16)))))

;;; ============================================================
;;; AArch64 SMP via PSCI
;;; ============================================================

(defun aarch64-start-cpu (cpu-id entry-addr context-id)
  "Generate PSCI CPU_ON call to start a secondary CPU.
   Uses SMC or HVC depending on conduit method (from DTB)."
  (list :psci-call
        :function-id +psci-cpu-on-64+
        :target-cpu cpu-id
        :entry-point entry-addr
        :context-id context-id))

(defun aarch64-percpu-layout ()
  "Per-CPU structure for AArch64.
   Accessed via TPIDR_EL1 (thread ID register)."
  '((:self-ptr       0   8)
    (:reduction       8   8)
    (:cpu-id         16   8)
    (:current-actor  24   8)
    (:obj-alloc      40   8)
    (:obj-limit      48   8)
    (:scratch-stack  56   8)))

;;; ============================================================
;;; AArch64 Page Table Setup
;;; ============================================================

(defun aarch64-setup-page-tables ()
  "AArch64 page table setup description.
   4KB granule, 4-level (48-bit VA):
   Level 0: 512GB per entry (L0 table)
   Level 1: 1GB per entry
   Level 2: 2MB per entry (block descriptors)
   Level 3: 4KB per entry (page descriptors)

   For QEMU virt: map first 4GB using 1GB block descriptors."
  '(:granule 4096
    :va-bits 48
    :levels 4
    :block-size-l1 #x40000000   ; 1GB
    :block-size-l2 #x200000     ; 2MB
    :identity-map-range (#x00000000 . #x100000000)))

;;; ============================================================
;;; AArch64 Boot Integration
;;; ============================================================

(defun aarch64-boot-descriptor ()
  "Return the AArch64 boot descriptor for image building"
  (list :arch :aarch64
        :entry-fn #'emit-aarch64-entry
        :exception-vectors-fn #'emit-aarch64-exception-vectors
        :uart-init-fn #'aarch64-init-uart
        :gic-init-fn #'aarch64-init-gic
        :smp-start-fn #'aarch64-start-cpu
        :percpu-layout-fn #'aarch64-percpu-layout
        :page-table-fn #'aarch64-setup-page-tables
        :load-addr +aarch64-kernel-base+
        :stack-top +aarch64-stack-top+
        :cons-base +aarch64-cons-base+
        :general-base +aarch64-general-base+))

;;; ============================================================
;;; Fixpoint Boot: MMU with VA=PA-0x40000000 offset mapping
;;; ============================================================
;;;
;;; The fixpoint proves the MVM compiler is a fixed point across
;;; architectures. For the runtime's build-image (with hardcoded x64
;;; addresses like 0x08000000, 0x330000, 0x4FF080) to work on AArch64,
;;; we set up page tables mapping low VAs to DRAM:
;;;
;;;   VA 0x00000000-0x1FFFFFFF → PA 0x40000000-0x5FFFFFFF (DRAM, normal)
;;;   VA 0x20000000-0x201FFFFF → PA 0x09000000-0x091FFFFF (UART, device)
;;;   VA 0x40000000-0x7FFFFFFF → PA 0x40000000-0x7FFFFFFF (identity, boot)
;;;
;;; This means:
;;;   VA 0x100000 → PA 0x40100000 (kernel code, same as load address)
;;;   VA 0x08000000 → PA 0x48000000 (image buffer, in DRAM)
;;;   VA 0x330000 → PA 0x40330000 (NFN table, in DRAM)
;;;   VA 0x4FF080 → PA 0x44FF080 (metadata, in DRAM)
;;;   VA 0x10000000 → PA 0x50000000 (alloc region, in DRAM)
;;;   VA 0x20000000 → PA 0x09000000 (PL011 UART, device memory)

(defconstant +tdk-page-table-pa+ #x40010000)  ; L1 table in DRAM
(defconstant +tdk-l2-table-pa+   #x40011000)  ; L2 table in DRAM
(defconstant +tdk-dram-base-pa+  #x40000000)  ; QEMU virt DRAM start
(defconstant +tdk-uart-pa+       #x09000000)  ; PL011 UART physical

;; VA addresses for fixpoint runtime (same as x64)
(defconstant +tdk-stack-va+      #x00200000)  ; Stack top
(defconstant +tdk-cons-base-va+  #x09000000)  ; Cons alloc (above image buffer at 0x08000000)
(defconstant +tdk-cons-limit-va+ #x10000000)  ; Cons limit (112MB heap, below E1000 BAR at 0x10000000)
(defconstant +tdk-uart-va+       #x20000000)  ; UART via page tables
(defconstant +tdk-percpu-va+     #x10080000)  ; Per-CPU data (DRAM scratch — 0x00360000 was inside the kernel image for builds >5MB)

(defun emit-aarch64-load-imm64 (buf rd value)
  "Load a 64-bit immediate into Xd using MOVZ + up to 3 MOVK."
  (emit-aarch64-movz buf rd (logand value #xFFFF) 0)
  (let ((hw1 (logand (ash value -16) #xFFFF))
        (hw2 (logand (ash value -32) #xFFFF))
        (hw3 (logand (ash value -48) #xFFFF)))
    (when (not (zerop hw1))
      (emit-aarch64-movk buf rd hw1 16))
    (when (not (zerop hw2))
      (emit-aarch64-movk buf rd hw2 32))
    (when (not (zerop hw3))
      (emit-aarch64-movk buf rd hw3 48))))

(defun emit-aarch64-str-x (buf rt rn &optional (imm12 0))
  "STR Xt, [Xn, #imm12*8]  (64-bit store, unsigned offset scaled by 8)"
  (emit-aarch64-u32 buf (logior (ash #b11 30)      ; size=11 (64-bit)
                                (ash #b111001 24)   ; STR
                                (ash 0 22)          ; opc=00
                                (ash imm12 10)      ; imm12 (scaled by 8)
                                (ash rn 5)
                                rt)))

(defun emit-aarch64-str-w (buf rt rn &optional (imm12 0))
  "STR Wt, [Xn, #imm12*4]  (32-bit store, unsigned offset scaled by 4)"
  (emit-aarch64-u32 buf (logior (ash #b10 30)      ; size=10 (32-bit)
                                (ash #b111001 24)   ; STR
                                (ash 0 22)          ; opc=00
                                (ash imm12 10)      ; imm12 (scaled by 4)
                                (ash rn 5)
                                rt)))

(defun emit-aarch64-fixpoint-entry (buf)
  "Emit AArch64 fixpoint kernel entry with MMU page tables.
   QEMU virt loads raw binary at PA 0x40000000. Boot code runs at PA,
   sets up page tables for VA=PA-0x40000000 offset, enables MMU,
   then branches to native code via offset-mapped VA."
  (let ((sp 31)
        (x0 0) (x1 1) (x2 2) (x3 3) (x4 4)
        (x16 16) (x17 17)
        (x24 24) (x25 25) (x26 26))

    ;; ================================================================
    ;; Phase A: Pre-MMU setup (running at PA 0x40000000+)
    ;; ================================================================

    ;; 1. Temporary stack in DRAM (for early init, won't be used much)
    (emit-aarch64-movz buf x16 #x4040 16)     ; x16 = 0x40400000
    (emit-aarch64-mov-sp buf sp x16)           ; SP = PA 0x40400000

    ;; ================================================================
    ;; Phase B: Build page tables at PA 0x40010000
    ;; ================================================================

    ;; 2. Zero L1 table (4KB = 512 entries × 8 bytes) at PA 0x40010000
    (emit-aarch64-load-imm64 buf x0 +tdk-page-table-pa+)  ; x0 = L1 base
    (emit-aarch64-movz buf x1 0 0)              ; x1 = 0 (zero value)
    (emit-aarch64-movz buf x2 512 0)            ; x2 = 512 (entries)
    ;; loop: str xzr, [x0], #8; sub x2, x2, #1; cbnz x2, loop
    (let ((zero-loop-pos (mvm-buffer-position buf)))
      ;; STR XZR, [X0], #8  (post-index)
      ;; Encoding: 11 111000 00 0 000001000 01 00000 11111
      (emit-aarch64-u32 buf #xF800841F)         ; STR XZR, [X0], #8
      ;; SUB X2, X2, #1
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      ;; CBNZ X2, loop  (back 2 instructions = -8 bytes = -2 words)
      (let ((offset (/ (- zero-loop-pos (mvm-buffer-position buf)) 4)))
        (emit-aarch64-u32 buf (logior (ash #b10110101 24) ; CBNZ (64-bit)
                                      (ash (logand offset #x7FFFF) 5)
                                      x2))))

    ;; 3. Zero L2 table (4KB) at PA 0x40011000
    (emit-aarch64-load-imm64 buf x0 +tdk-l2-table-pa+)
    (emit-aarch64-movz buf x2 512 0)
    (let ((zero-loop2-pos (mvm-buffer-position buf)))
      (emit-aarch64-u32 buf #xF800841F)         ; STR XZR, [X0], #8
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      (let ((offset (/ (- zero-loop2-pos (mvm-buffer-position buf)) 4)))
        (emit-aarch64-u32 buf (logior (ash #b10110101 24)
                                      (ash (logand offset #x7FFFF) 5)
                                      x2))))

    ;; 4. L1[0] = table descriptor → L2 at PA 0x40011000
    ;;    entry = PA | 0x3 (valid + table)
    (emit-aarch64-load-imm64 buf x0 +tdk-page-table-pa+)  ; x0 = L1 base
    (emit-aarch64-load-imm64 buf x1 (logior +tdk-l2-table-pa+ #x3))
    (emit-aarch64-str-x buf x1 x0 0)            ; L1[0] = table desc

    ;; 5. L1[1] = 1GB block descriptor, identity map DRAM
    ;;    PA 0x40000000, normal memory, AF=1, SH=inner, AttrIndx=0
    ;;    entry = 0x40000000 | (1<<10) | (3<<8) | 0b01 = 0x40000701
    (emit-aarch64-load-imm64 buf x1 #x40000701)
    (emit-aarch64-str-x buf x1 x0 1)            ; L1[1] (offset 8)

    ;; 5b. L1[256] = 1GB block, device memory for PCI ECAM (0x4010000000)
    ;;    PA 0x4000000000, AttrIndx=1 (device nGnRnE), AF=1, SH=inner
    ;;    entry = 0x4000000000 | (1<<10) | (3<<8) | (1<<2) | 0b01 = 0x4000000705
    ;;    L1 entry 256 at L1_base + 256*8 = L1_base + 0x800
    (emit-aarch64-load-imm64 buf x0 (+ +tdk-page-table-pa+ #x800))
    (emit-aarch64-load-imm64 buf x1 #x4000000705)
    (emit-aarch64-str-x buf x1 x0 0)
    (emit-aarch64-load-imm64 buf x0 +tdk-page-table-pa+)  ; restore x0 = L1 base

    ;; 6. Fill L2[0..255] = 2MB blocks, VA 0x00-0x1FF → PA 0x400-0x5FF
    ;;    Each entry: (0x40000000 + i*0x200000) | 0x701
    (emit-aarch64-load-imm64 buf x0 +tdk-l2-table-pa+)  ; x0 = L2 base
    (emit-aarch64-load-imm64 buf x1 #x40000701)         ; x1 = first entry
    (emit-aarch64-movz buf x2 256 0)                     ; x2 = count
    (emit-aarch64-load-imm64 buf x3 #x200000)           ; x3 = 2MB step
    (let ((fill-loop-pos (mvm-buffer-position buf)))
      ;; STR X1, [X0], #8
      (emit-aarch64-u32 buf #xF8008401)
      ;; ADD X1, X1, X3  (next PA)
      ;; sf=1 op=0 S=0 01011 shift=00 0 Rm=X3 imm6=0 Rn=X1 Rd=X1
      (emit-aarch64-u32 buf #x8B030021)
      ;; SUB X2, X2, #1
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      ;; CBNZ X2, fill_loop
      (let ((offset (/ (- fill-loop-pos (mvm-buffer-position buf)) 4)))
        (emit-aarch64-u32 buf (logior (ash #b10110101 24)
                                      (ash (logand offset #x7FFFF) 5)
                                      x2))))

    ;; 7. Fill L2[128..511] = identity-mapped device memory for PCI MMIO
    ;;    VA 0x10000000-0x3FFFFFFF → PA 0x10000000-0x3FFFFFFF
    ;;    Covers entire PCI MMIO window (E1000 BAR can be anywhere in range)
    ;;    entry = PA | 0x705 (device nGnRnE, AF, SH=inner, AttrIndx=1)
    ;;    L2[128] at L2_base + 128*8 = L2_base + 0x400
    (emit-aarch64-load-imm64 buf x0 (+ +tdk-l2-table-pa+ #x400))  ; x0 = &L2[128]
    (emit-aarch64-load-imm64 buf x1 #x10000705)                     ; x1 = first entry (PA 0x10000000)
    (emit-aarch64-movz buf x2 384 0)                                 ; x2 = count (128..511 = 384 entries)
    ;; Reuse x3 = 0x200000 (2MB step, already loaded from step 6)
    (let ((pci-loop-pos (mvm-buffer-position buf)))
      ;; STR X1, [X0], #8
      (emit-aarch64-u32 buf #xF8008401)
      ;; ADD X1, X1, X3 (next PA)
      (emit-aarch64-u32 buf #x8B030021)
      ;; SUB X2, X2, #1
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      ;; CBNZ X2, pci_loop
      (let ((offset (/ (- pci-loop-pos (mvm-buffer-position buf)) 4)))
        (emit-aarch64-u32 buf (logior (ash #b10110101 24)
                                      (ash (logand offset #x7FFFF) 5)
                                      x2))))

    ;; 7c. Restore L2[256] = UART (was overwritten by PCI loop above)
    ;;    VA 0x20000000 → PA 0x09000000 (device)
    (emit-aarch64-load-imm64 buf x0 (+ +tdk-l2-table-pa+ (* 256 8)))
    (emit-aarch64-load-imm64 buf x1 #x09000705)
    (emit-aarch64-str-x buf x1 x0 0)

    ;; 7d. Restore L2[128] = DRAM scratch for runtime metadata (was
    ;;     overwritten by PCI loop above).  VA 0x10000000-0x10200000
    ;;     → PA 0x50000000-0x50200000 (DRAM, normal cacheable).
    ;;     The runtime hardcodes its BSS-equivalent slots at
    ;;     0x10000040+ (GC), 0x10000080+ (globals), 0x10000148 (kw),
    ;;     0x10000180+ (handler-case), 0x10000C30+ (SIGSEGV diag).
    ;;     Without this remap those addresses land in PCI device
    ;;     memory and writes to them hang.  Matches the comment at
    ;;     ;;;   VA 0x10000000 → PA 0x50000000 (alloc region, in DRAM)
    ;;     above — Phase A.1 ANSI port discovered this.
    (emit-aarch64-load-imm64 buf x0 (+ +tdk-l2-table-pa+ (* 128 8)))
    (emit-aarch64-load-imm64 buf x1 #x50000701)
    (emit-aarch64-str-x buf x1 x0 0)

    ;; 7e. Override L2[64] = GIC identity map, VA 0x08000000-0x081FFFFF
    ;;     → PA 0x08000000-0x081FFFFF (device memory).  Required for the
    ;;     AArch64 ANSI build's per-test deadline IRQ: setup-irq writes
    ;;     to GICD/GICC at "address 0x08000000" / "0x08010000" which
    ;;     the fixpoint MMU's L2[64] otherwise routes to PA 0x48000000
    ;;     (image-buffer DRAM in the cross-compile pipeline, but
    ;;     unused on the ANSI build).  Without this override the GIC
    ;;     enable writes silently land in DRAM and the timer IRQ never
    ;;     fires — Boulder #30 root cause.
    ;;
    ;; Cost: VA 0x08000000-0x081FFFFF is no longer DRAM.  Fixpoint
    ;; runtime's image-buffer at this VA stops working.  Acceptable
    ;; for the ANSI build (no runtime image-buffer use); other builds
    ;; that depend on this VA→DRAM mapping must skip this fixup.
    (emit-aarch64-load-imm64 buf x0 (+ +tdk-l2-table-pa+ (* 64 8)))
    (emit-aarch64-load-imm64 buf x1 #x08000705)
    (emit-aarch64-str-x buf x1 x0 0)

    ;; ================================================================
    ;; Phase C: Configure system registers and enable MMU
    ;; ================================================================

    ;; 8. MAIR_EL1: attr0=0xFF (Normal WB RWA), attr1=0x00 (Device nGnRnE)
    ;;    MAIR_EL1 = 0x00FF
    (emit-aarch64-movz buf x0 #x00FF 0)
    (emit-aarch64-u32 buf #xD518A200)            ; MSR MAIR_EL1, X0

    ;; 9. TCR_EL1: T0SZ=25 (39-bit VA), TG0=0 (4KB), SH0=3, ORGN0=1, IRGN0=1
    ;;    IPS=2 (40-bit PA) at bits [34:32]
    ;;    TCR = 0x19 | (3<<12) | (1<<10) | (1<<8) | (2<<32)
    ;;        = 0x0000000200003519
    (emit-aarch64-load-imm64 buf x0 #x0000000200003519)
    (emit-aarch64-u32 buf #xD5182040)            ; MSR TCR_EL1, X0

    ;; 10. TTBR0_EL1 = PA 0x40010000 (L1 table)
    (emit-aarch64-load-imm64 buf x0 +tdk-page-table-pa+)
    (emit-aarch64-u32 buf #xD5182000)            ; MSR TTBR0_EL1, X0

    ;; 11. DSB ISH (ensure page table writes are visible)
    (emit-aarch64-u32 buf #xD5033B9F)            ; DSB ISH

    ;; 12. ISB (synchronize context)
    (emit-aarch64-u32 buf #xD5033FDF)            ; ISB

    ;; 13. Enable MMU: SCTLR_EL1 |= M (bit 0) | C (bit 2) | I (bit 12)
    ;;     Read SCTLR_EL1, OR with 0x1005, write back
    (emit-aarch64-u32 buf #xD5381000)            ; MRS X0, SCTLR_EL1
    (emit-aarch64-load-imm64 buf x1 #x1005)
    ;; ORR X0, X0, X1
    ;; sf=1 opc=01 01010 shift=00 N=0 Rm=X1 imm6=0 Rn=X0 Rd=X0
    (emit-aarch64-u32 buf #xAA010000)
    (emit-aarch64-u32 buf #xD5181000)            ; MSR SCTLR_EL1, X0

    ;; 14. ISB (ensure MMU is active for next instruction)
    (emit-aarch64-u32 buf #xD5033FDF)            ; ISB

    ;; ================================================================
    ;; Phase D: Post-MMU setup (now running at VA via identity map)
    ;; CPU is at VA 0x4000xxxx (identity map). We set up VA-space
    ;; resources and then branch to native code via offset map.
    ;; ================================================================

    ;; 15. Set stack pointer to VA 0x200000 (→ PA 0x40200000)
    (emit-aarch64-load-imm64 buf x16 +tdk-stack-va+)
    (emit-aarch64-mov-sp buf sp x16)

    ;; 16. Initialize PL011 UART at VA 0x20000000 (→ PA 0x09000000)
    (emit-aarch64-load-imm64 buf x17 +tdk-uart-va+)
    ;; UARTCR = 0x0301 (enable UART + TX + RX) at [x17 + 0x30]
    (emit-aarch64-movz buf x0 #x0301 0)
    (emit-aarch64-str-w buf x0 x17 12)           ; +0x30/4 = 12
    ;; UARTLCR_H = 0x70 (8-bit, FIFO) at [x17 + 0x2C]
    (emit-aarch64-movz buf x0 #x70 0)
    (emit-aarch64-str-w buf x0 x17 11)           ; +0x2C/4 = 11
    ;; UARTIBRD = 13 at [x17 + 0x24]
    (emit-aarch64-movz buf x0 13 0)
    (emit-aarch64-str-w buf x0 x17 9)            ; +0x24/4 = 9
    ;; UARTFBRD = 1 at [x17 + 0x28]
    (emit-aarch64-movz buf x0 1 0)
    (emit-aarch64-str-w buf x0 x17 10)           ; +0x28/4 = 10
    ;; Re-enable UART
    (emit-aarch64-movz buf x0 #x0301 0)
    (emit-aarch64-str-w buf x0 x17 12)

    ;; 17. Set allocation registers (VA addresses)
    (emit-aarch64-load-imm64 buf x24 +tdk-cons-base-va+)   ; cons alloc
    (emit-aarch64-load-imm64 buf x25 +tdk-cons-limit-va+)  ; cons limit
    (emit-aarch64-movz buf x26 0 0)                         ; NIL = 0

    ;; 18. Set TPIDR_EL1 = per-CPU data VA
    (emit-aarch64-load-imm64 buf x16 +tdk-percpu-va+)
    (emit-aarch64-u32 buf #xD518D090)            ; MSR TPIDR_EL1, X16

    ;; 19. Set VBAR_EL1 for exception vectors.  QEMU virt's `-kernel`
    ;; loads our binary at PA 0x40080000 (Linux Image convention).
    ;; With offset MMU (VA = PA - 0x40000000), the binary's exception
    ;; vectors at file offset 0x800 land at VA 0x80800, NOT 0x800.
    ;; Setting VBAR=0x800 caused sync exceptions to vector to PA 0x40000A00
    ;; (before our binary, unloaded memory) — undefined-instruction loop.
    ;; Compute the right VA: load 0x80800 via MOVZ + MOVK (0x80800 doesn't
    ;; fit in a single MOVZ).
    (emit-aarch64-movz buf x16 #x0800 0)         ; x16 = 0x0800
    (emit-aarch64-movk buf x16 #x0008 16)        ; x16 |= 0x80000 (bits 31:16)
    (emit-aarch64-u32 buf #xD518C010)            ; MSR VBAR_EL1, X16
    (emit-aarch64-u32 buf #xD5033FDF)            ; ISB

    ;; 20. Branch to native code via offset-mapped VA
    ;; Native code starts at offset 0x1000 in the image = VA 0x1000
    ;; (Boot preamble occupies offsets 0x000-0x7FF, vectors at 0x800-0xFFF)
    ;; We need to branch from identity-mapped VA (0x4000xxxx) to offset VA (0x1000)
    (let* ((current-insn (/ (mvm-buffer-position buf) 4))
           (native-start-insn 1024)               ; instruction 1024 = offset 0x1000
           (skip (- native-start-insn current-insn)))
      ;; B forward to native code at offset 0x1000
      (emit-aarch64-u32 buf (logior (ash #b000101 26) (logand skip #x3FFFFFF)))
      ;; Pad with NOPs to offset 0x800 (instruction 512)
      (let ((pad (- 512 (/ (mvm-buffer-position buf) 4))))
        (dotimes (i pad)
          (emit-aarch64-u32 buf #xD503201F))))

    ;; 21. Exception vector table at offset 0x800 (= VA 0x800)
    (emit-aarch64-exception-vectors buf)

    ;; Native code follows at offset 0x1000 (= VA 0x1000)
    ))

(defun aarch64-fixpoint-boot-descriptor ()
  "Return the AArch64 boot descriptor for fixpoint builds.
   Uses MMU with offset page tables so x64-compatible addresses work."
  (list :arch :aarch64
        :entry-fn #'emit-aarch64-fixpoint-entry
        :serial-base +tdk-uart-va+
        :load-addr +tdk-dram-base-pa+
        :stack-top +tdk-stack-va+
        :cons-base +tdk-cons-base-va+
        :general-base (+ +tdk-cons-limit-va+ #x01000000)))
