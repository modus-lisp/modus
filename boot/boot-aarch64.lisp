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
  "Emit a 32-bit instruction into BUF.

   BUF is an a64-buffer (translate-aarch64.lisp:153), which stores the
   word in its `code` array as a u32 (little-endian conversion deferred
   to a64-buffer-to-bytes).  Earlier this emitted 4 mvm-emit-byte calls
   into an mvm-buffer; switched to a64-buffer so boot preamble and
   translated kernel code share the same label/fixup space.  See task
   #34 and reference_handler_stack_design.md."
  (a64-emit buf word))

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
    (let* ((current-insn (a64-buffer-position buf))
           (native-start 1024)                  ; instruction 1024 = offset 0x1000
           (skip (- native-start current-insn)))
      ;; B forward to native code
      (emit-aarch64-u32 buf (logior (ash #b000101 26) (logand skip #x3FFFFFF)))
      ;; Pad with NOPs to offset 0x800 (instruction 512)
      (let ((pad (- 512 (a64-buffer-position buf))))
        (dotimes (i pad)
          (emit-aarch64-u32 buf #xD503201F))))

    ;; 7. Exception vector table (2KB at offset 0x800)
    (emit-aarch64-exception-vectors buf)
    ;; Native code follows immediately (kernel-main at offset 0x1000)
    ))

;; ANSI-runner deadline/fault hardening for the exception vectors.
;; Default NIL keeps the legacy entry-4/5/7/8 encodings byte-identical
;; (all other bare-metal aa64 builds unaffected).  The ANSI runner sets
;; T, which ports two x64 a41a760 ISR lessons + one aa64-specific one:
;;   - entry 5 (vtimer deadline): counter CLAMP (a wild-written C70 >
;;     65536 resets to 2001 — heap pointers scribbled into the slot
;;     effectively disarmed the watchdog) and SELF-RE-ARM to 2000 on
;;     every expiry (one-shot consumption left C70=0, so any later hang
;;     ran without a watchdog — observed at the 13196 bignum band).
;;     The expiry longjmp continuation lives in the unused entry-7 slot.
;;   - entry 4 (sync recovery): FAULT-STORM BUDGET at 0x10000CD0 — a
;;     fault→longjmp→refault cycle that re-arms its own handler-case
;;     frame each pass spins forever (observed: PC alternating between
;;     entry-4 and the poisoned print path, stable frame, C70=0).  Each
;;     recovery increments the counter; past 2048 the recovery is
;;     FORCED onto the 0x100001C0 fallback (fork-file's frame) — the
;;     file wedges and the run continues.  fork-file resets the counter
;;     per file.  Continuation lives in the unused entry-8 slot.
(defvar *aarch64-ansi-deadline-hardening* nil)

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
      ;; ================= ANSI-hardened variants (gated) =================
      ;; See *aarch64-ansi-deadline-hardening*.  Each variant emits
      ;; EXACTLY 32 words.  Entry-5's expiry longjmp continuation lives
      ;; in the (otherwise B-to-self) entry-7 slot; entry-4's storm-
      ;; budget recovery lives in the entry-8 slot.  Cross-entry branch
      ;; offsets are computed from the block-start position (every
      ;; entry is exactly 32 instructions).
      ((and (= entry 4) *aarch64-ansi-deadline-hardening*)
       ;; Hardened entry 4: capture diag (ELR/FAR/X0 → 0x10000C30/40/48)
       ;; then branch to the storm-budget recovery in the entry-8 slot.
       (let ((base (a64-buffer-position buf)))
         (emit-aarch64-u32 buf #xD5384030)      ; MRS x16, ELR_EL1
         (emit-aarch64-u32 buf #xD2818611)      ; MOVZ x17,#0x0c30
         (emit-aarch64-u32 buf #xF2A20011)      ; MOVK x17,#0x1000,lsl 16
         (emit-aarch64-u32 buf #xF9000230)      ; STR x16,[x17]
         (emit-aarch64-u32 buf #xD5386010)      ; MRS x16, FAR_EL1
         (emit-aarch64-u32 buf #xF9000A30)      ; STR x16,[x17,#0x10]
         (emit-aarch64-u32 buf #xF9000E20)      ; STR x0,[x17,#0x18]
         ;; B → entry-8 slot start (= base + 4*32 instructions)
         (let ((off (- (+ base 128) (a64-buffer-position buf))))
           (emit-aarch64-u32 buf (logior #x14000000 (logand off #x3FFFFFF))))
         (dotimes (i (- 32 (- (a64-buffer-position buf) base)))
           (emit-aarch64-u32 buf #xD503201F))))
      ((and (= entry 5) *aarch64-ansi-deadline-hardening*)
       ;; Hardened entry 5 (vtimer deadline IRQ): ack + re-arm timer,
       ;; C70 clamp (>65536 → 2001), decrement; on expiry branch to the
       ;; longjmp continuation in the entry-7 slot (which SELF-RE-ARMS
       ;; C70 = 2000 before longjmping).
       (let ((base (a64-buffer-position buf)))
         (emit-aarch64-u32 buf #xA9BF07E0)      ; STP x0,x1,[SP,#-16]!
         (emit-aarch64-u32 buf #xD2A10020)      ; MOVZ x0,#0x0801,LSL 16 (GICC)
         (emit-aarch64-u32 buf #xB9400C01)      ; LDR w1,[x0,#0x0C] (IAR)
         (emit-aarch64-u32 buf #xB9001001)      ; STR w1,[x0,#0x10] (EOIR)
         (emit-aarch64-u32 buf #xD29E8480)      ; MOVZ x0,#0xF424 (62500)
         (emit-aarch64-u32 buf #xD51BE300)      ; MSR CNTV_TVAL_EL0,x0
         (emit-aarch64-u32 buf #xD2803000)      ; MOVZ x0,#0x0180
         (emit-aarch64-u32 buf #xF2A20000)      ; MOVK x0,#0x1000,LSL 16
         (emit-aarch64-u32 buf #xF9457801)      ; LDR x1,[x0,#2800] (C70)
         (emit-aarch64-u32 buf #xB4000101)      ; CBZ x1,+8 → NORMAL
         (emit-aarch64-u32 buf #xF140403F)      ; CMP x1,#16,LSL#12 (65536)
         (emit-aarch64-u32 buf #x54000049)      ; B.LS +2 (skip clamp)
         (emit-aarch64-u32 buf #xD280FA21)      ; MOVZ x1,#2001 (clamp)
         (emit-aarch64-u32 buf #xF1000421)      ; SUBS x1,x1,#1
         (emit-aarch64-u32 buf #xF9057801)      ; STR x1,[x0,#2800]
         (emit-aarch64-u32 buf #x54000041)      ; B.NE +2 → NORMAL
         ;; B → EXPIRE continuation at entry-7 slot (= base + 64)
         (let ((off (- (+ base 64) (a64-buffer-position buf))))
           (emit-aarch64-u32 buf (logior #x14000000 (logand off #x3FFFFFF))))
         ;; NORMAL:
         (emit-aarch64-u32 buf #xA8C107E0)      ; LDP x0,x1,[SP],#16
         (emit-aarch64-u32 buf #xD69F03E0)      ; ERET
         (dotimes (i (- 32 (- (a64-buffer-position buf) base)))
           (emit-aarch64-u32 buf #xD503201F))))
      ((and (= entry 7) *aarch64-ansi-deadline-hardening*)
       ;; EXPIRE continuation for hardened entry 5.  On entry: x0 =
       ;; 0x10000180, x0/x1 saved on the interrupted stack (leaked on
       ;; the longjmp path — SP is replaced from the frame).  The
       ;; slot-1C0 (rescue) path routes through the shared budgeted
       ;; consume in the entry-9 slot (multi-shot, serial '!' diag).
       (let ((base (a64-buffer-position buf)))
         (emit-aarch64-u32 buf #xD280FA01)      ; e0  MOVZ x1,#2000 (SELF-RE-ARM)
         (emit-aarch64-u32 buf #xF9057801)      ; e1  STR x1,[x0,#2800]
         (emit-aarch64-u32 buf #xF9400001)      ; e2  LDR x1,[x0] (slot 180 SP)
         (emit-aarch64-u32 buf #xB5000081)      ; e3  CBNZ x1,+4 → DO_LJ (e7)
         (emit-aarch64-u32 buf #x91010000)      ; e4  ADD x0,x0,#0x40 (→1C0)
         (emit-aarch64-u32 buf #xF9400001)      ; e5  LDR x1,[x0]
         (emit-aarch64-u32 buf #xB4000181)      ; e6  CBZ x1,+12 → NORMAL2 (e18)
         ;; DO_LJ:
         (emit-aarch64-u32 buf #x9100003F)      ; e7  ADD SP,x1,#0
         (emit-aarch64-u32 buf #xF940041D)      ; e8  LDR x29,[x0,#8]
         (emit-aarch64-u32 buf #xF9400801)      ; e9  LDR x1,[x0,#16]
         (emit-aarch64-u32 buf #x37300140)      ; e10 TBNZ x0,#6,+10 → slot_1C0 (e20)
         (cond
           (modus.mvm::*aarch64-handler-pop-label*
            (let ((idx (modus.mvm::a64-current-index buf)))
              (modus.mvm::a64-bl buf 0)          ; e11 BL pop_helper
              (modus.mvm::a64-add-fixup buf idx
                                        modus.mvm::*aarch64-handler-pop-label*
                                        :bl)))
           (t (emit-aarch64-u32 buf #xD503201F)))
         (emit-aarch64-u32 buf #xD5184021)      ; e12 MSR ELR_EL1,x1
         (emit-aarch64-u32 buf #xD2820120)      ; e13 MOVZ x0,#0x1009
         (emit-aarch64-u32 buf #xF2BBD5A0)      ; e14 MOVK x0,#0xDEAD,LSL 16
         (emit-aarch64-u32 buf #xD69F03E0)      ; e15 ERET (longjmp)
         (emit-aarch64-u32 buf #xD503201F)      ; e16 NOP
         (emit-aarch64-u32 buf #xD503201F)      ; e17 NOP
         ;; NORMAL2 (no handler armed — spurious expiry):
         (emit-aarch64-u32 buf #xA8C107E0)      ; e18 LDP x0,x1,[SP],#16
         (emit-aarch64-u32 buf #xD69F03E0)      ; e19 ERET
         (emit-aarch64-u32 buf #xAA0103F1)      ; e20 MOV x17,x1 (IP for shared consume)
         ;; e21: B → entry-9 slot (= base + 64)
         (let ((off (- (+ base 64) (a64-buffer-position buf))))
           (emit-aarch64-u32 buf (logior #x14000000 (logand off #x3FFFFFF))))
         (dotimes (i (- 32 (- (a64-buffer-position buf) base)))
           (emit-aarch64-u32 buf #xD503201F))))
      ((and (= entry 9) *aarch64-ansi-deadline-hardening*)
       ;; Shared 1C0-rescue consume (from hardened entries 7 and 8):
       ;; x17 = frame IP (SP/FP already restored by the caller path).
       ;; Emits one '!' to the UART per rescue landing (visible storm
       ;; diagnostic), decrements the rescue BUDGET at 0x10000CD8
       ;; (multi-shot, x64-D90 parity — armed to 50 by fork-file), and
       ;; only CLEARS 1C0 when the budget is exhausted (guaranteeing
       ;; termination via entry-8's HALT), then longjmps with X0 = T.
       (let ((base (a64-buffer-position buf)))
         (emit-aarch64-u32 buf #xD2800432)      ; c0  MOVZ x18,#0x21 ('!')
         (emit-aarch64-u32 buf #xD2A40010)      ; c1  MOVZ x16,#0x2000,LSL 16 (UART VA)
         (emit-aarch64-u32 buf #xB9000212)      ; c2  STR w18,[x16]
         (emit-aarch64-u32 buf #xD2819B12)      ; c3  MOVZ x18,#0x0CD8
         (emit-aarch64-u32 buf #xF2A20012)      ; c4  MOVK x18,#0x1000,LSL 16
         (emit-aarch64-u32 buf #xF9400250)      ; c5  LDR x16,[x18]
         (emit-aarch64-u32 buf #xF1000610)      ; c6  SUBS x16,x16,#1
         (emit-aarch64-u32 buf #xF9000250)      ; c7  STR x16,[x18]
         (emit-aarch64-u32 buf #x5400008C)      ; c8  B.GT +4 → skip (c12)
         (emit-aarch64-u32 buf #xD2803810)      ; c9  MOVZ x16,#0x01C0 (exhausted:
         (emit-aarch64-u32 buf #xF2A20010)      ; c10 MOVK x16,#0x1000,LSL 16
         (emit-aarch64-u32 buf #xF900021F)      ; c11 STR XZR,[x16]  clear 1C0)
         (emit-aarch64-u32 buf #xD5184031)      ; c12 MSR ELR_EL1,x17
         (emit-aarch64-u32 buf #xD2820120)      ; c13 MOVZ x0,#0x1009
         (emit-aarch64-u32 buf #xF2BBD5A0)      ; c14 MOVK x0,#0xDEAD,LSL 16
         (emit-aarch64-u32 buf #xD69F03E0)      ; c15 ERET (longjmp)
         (dotimes (i (- 32 (- (a64-buffer-position buf) base)))
           (emit-aarch64-u32 buf #xD503201F))))
      ((and (= entry 8) *aarch64-ansi-deadline-hardening*)
       ;; Storm-budget recovery for hardened entry 4.  Counter at
       ;; 0x10000CD0; past 256 recoveries the longjmp is FORCED onto
       ;; the 0x100001C0 fallback (fork-file frame) and the counter is
       ;; reset — a self-sustaining fault→longjmp cycle becomes a
       ;; FILE-WEDGE instead of a permanently frozen machine.  run-test
       ;; and fork-file both reset the counter (per-test/per-file), so
       ;; a legit fault-heavy file cannot false-trip the budget; the
       ;; poison cycles this targets never reach run-test.
       (let ((base (a64-buffer-position buf)))
         (emit-aarch64-u32 buf #xD2819A10)      ; i0  MOVZ x16,#0x0CD0
         (emit-aarch64-u32 buf #xF2A20010)      ; i1  MOVK x16,#0x1000,LSL 16
         (emit-aarch64-u32 buf #xF9400211)      ; i2  LDR x17,[x16]
         (emit-aarch64-u32 buf #x91000631)      ; i3  ADD x17,x17,#1
         (emit-aarch64-u32 buf #xF9000211)      ; i4  STR x17,[x16]
         (emit-aarch64-u32 buf #xF104023F)      ; i5  CMP x17,#256
         (emit-aarch64-u32 buf #x540002A8)      ; i6  B.HI +21 → FORCE (i27)
         (emit-aarch64-u32 buf #xD2803010)      ; i7  MOVZ x16,#0x0180
         (emit-aarch64-u32 buf #xF2A20010)      ; i8  MOVK x16,#0x1000,LSL 16
         (emit-aarch64-u32 buf #xF9400211)      ; i9  LDR x17,[x16]
         (emit-aarch64-u32 buf #xB5000091)      ; i10 CBNZ x17,+4 → DO_LJ (i14)
         (emit-aarch64-u32 buf #x91010210)      ; i11 ADD x16,x16,#0x40 (→1C0)
         (emit-aarch64-u32 buf #xF9400211)      ; i12 LDR x17,[x16]
         (emit-aarch64-u32 buf #xB4000171)      ; i13 CBZ x17,+11 → HALT (i24)
         (emit-aarch64-u32 buf #x9100023F)      ; i14 ADD sp,x17,#0 (DO_LJ)
         (emit-aarch64-u32 buf #xF940061D)      ; i15 LDR x29,[x16,#8]
         (emit-aarch64-u32 buf #xF9400A11)      ; i16 LDR x17,[x16,#16]
         (emit-aarch64-u32 buf #x37300130)      ; i17 TBNZ x16,#6,+9 → slot_1C0 (i26)
         (cond
           (modus.mvm::*aarch64-handler-pop-label*
            (let ((idx (modus.mvm::a64-current-index buf)))
              (modus.mvm::a64-bl buf 0)          ; i18 BL pop_helper
              (modus.mvm::a64-add-fixup buf idx
                                        modus.mvm::*aarch64-handler-pop-label*
                                        :bl)))
           (t (emit-aarch64-u32 buf #xD503201F)))
         (emit-aarch64-u32 buf #xD5184031)      ; i19 MSR ELR_EL1,x17
         (emit-aarch64-u32 buf #xD2820120)      ; i20 MOVZ x0,#0x1009
         (emit-aarch64-u32 buf #xF2BBD5A0)      ; i21 MOVK x0,#0xDEAD,LSL 16
         (emit-aarch64-u32 buf #xD69F03E0)      ; i22 ERET (longjmp)
         (emit-aarch64-u32 buf #xD503201F)      ; i23 NOP
         (emit-aarch64-u32 buf #x14000000)      ; i24 HALT: B .
         (emit-aarch64-u32 buf #xD503201F)      ; i25 NOP
         ;; i26: slot_1C0 → shared budgeted consume (entry-9 slot;
         ;; x17 = IP already loaded).  offset = base+32 - 26 = +6.
         (emit-aarch64-u32 buf #x14000006)      ; i26 B → entry-9
         (emit-aarch64-u32 buf #xF900021F)      ; i27 FORCE: STR XZR,[x16] (reset ctr)
         (emit-aarch64-u32 buf #xD2803810)      ; i28 MOVZ x16,#0x01C0
         (emit-aarch64-u32 buf #xF2A20010)      ; i29 MOVK x16,#0x1000,LSL 16
         (emit-aarch64-u32 buf #x17FFFFEE)      ; i30 B -18 → i12 (LDR x17,[x16])
         (emit-aarch64-u32 buf #xD503201F)      ; i31 NOP
         (dotimes (i (- 32 (- (a64-buffer-position buf) base)))
           (emit-aarch64-u32 buf #xD503201F))))
      ;; ================= legacy entries =================
      ((= entry 4)
       ;; Entry 4: Sync exception, Current EL with SP_ELx.
       ;; This is the SIGSEGV-equivalent path on bare-metal AArch64.
       ;; Mirrors x64 SIGSEGV stub (translate-x64.lisp #x0520):
       ;; if a handler-case is active (saved SP at 0x10000180 != 0),
       ;; restore SP/FP/IP from saved slots and ERET back to it,
       ;; with X0 = T to signal "longjmp-return".
       ;;
       ;; FALLBACK to slot 0x100001C0 (set by SAVE-OUTER trap, used by
       ;; fork-file for the outer "deadline-can-longjmp-here" handler):
       ;; when slot 180 is zero (no nested handler-case armed), try
       ;; slot 1C0 before halting.  Without this, a sync exception
       ;; (e.g. cdr-walk-past-tail) that fires when the nested
       ;; handler-case stack has unwound to zero would halt the
       ;; kernel even though fork-file's outer fallback IS armed —
       ;; manifested as the kernel halting at T:14577 once the
       ;; make-array `'(N)` fix (aefe8f2) stopped triggering spurious
       ;; allocs that hid this case.
       ;;
       ;; Mirrors entry-5 (IRQ) which already had this fallback.
       ;;
       ;; Capture state to diag slots first:
       ;;   0x10000C30 = ELR (faulting PC)
       ;;   0x10000C40 = FAR (fault addr)
       ;;   0x10000C48 = X0 at fault
       ;;
       ;; Layout (26 instructions):
       ;;   1-9.  Save ELR/FAR/X0 to diag slots, load x16 = 0x10000180
       ;;  10.   LDR x17, [x16]              ; slot 180 SP
       ;;  11.   CBNZ x17, +4 → DO_LJ        ; use slot 180
       ;;  12.   ADD x16, x16, #0x40         ; → slot 0x100001C0
       ;;  13.   LDR x17, [x16]              ; slot 1C0 SP (outer fallback)
       ;;  14.   CBZ x17, +12 → HALT         ; both zero, halt
       ;;  15.   ADD sp, x17, #0
       ;;  16.   LDR x29, [x16, #8]
       ;;  17.   LDR x17, [x16, #16]
       ;;  18.   TBNZ x16, #6, +3 → slot_1C0 ; discriminate by addr bit 6
       ;;  19.   BL pop_helper                (slot 180 path)
       ;;  20.   B +2 → do_eret
       ;;  21.   STR XZR, [x16]              ; slot_1C0: clear 1C0
       ;;  22.   MSR ELR_EL1, x17            ; do_eret
       ;;  23.   MOVZ x0, #0x1009
       ;;  24.   MOVK x0, #0xDEAD, lsl #16
       ;;  25.   ERET
       ;;  26.   B .                          ; HALT
       (emit-aarch64-u32 buf #xD5384030)        ; MRS x16, ELR_EL1
       (emit-aarch64-u32 buf #xD2818611)        ; MOVZ x17, #0x0c30
       (emit-aarch64-u32 buf #xF2A20011)        ; MOVK x17, #0x1000, lsl #16
       (emit-aarch64-u32 buf #xF9000230)        ; STR x16, [x17]
       (emit-aarch64-u32 buf #xD5386010)        ; MRS x16, FAR_EL1
       (emit-aarch64-u32 buf #xF9000A30)        ; STR x16, [x17, #0x10]
       (emit-aarch64-u32 buf #xF9000E20)        ; STR x0,  [x17, #0x18]
       (emit-aarch64-u32 buf #xD2803010)        ; MOVZ x16, #0x0180
       (emit-aarch64-u32 buf #xF2A20010)        ; MOVK x16, #0x1000, lsl #16
       (emit-aarch64-u32 buf #xF9400211)        ; LDR x17, [x16]   (slot 180 SP)
       (emit-aarch64-u32 buf #xB5000091)        ; CBNZ x17, +4 → DO_LJ
       (emit-aarch64-u32 buf #x91010210)        ; ADD x16, x16, #0x40 (→ 0x1C0)
       (emit-aarch64-u32 buf #xF9400211)        ; LDR x17, [x16]   (slot 1C0 SP)
       (emit-aarch64-u32 buf #xB4000191)        ; CBZ x17, +12 → HALT
       ;; DO_LJ (instr 15):
       (emit-aarch64-u32 buf #x9100023F)        ; ADD sp, x17, #0
       (emit-aarch64-u32 buf #xF940061D)        ; LDR x29, [x16, #8]
       (emit-aarch64-u32 buf #xF9400A11)        ; LDR x17, [x16, #16]
       ;; Discriminate slot 180 (bit6=0) vs slot 1C0 (bit6=1) of x16.
       (emit-aarch64-u32 buf #x37300070)        ; TBNZ x16, #6, +3 → slot_1C0
       ;; slot 180 path (instr 19): BL pop_helper.
       (cond
         (modus.mvm::*aarch64-handler-pop-label*
          (let ((idx (modus.mvm::a64-current-index buf)))
            (modus.mvm::a64-bl buf 0)
            (modus.mvm::a64-add-fixup buf idx
                                      modus.mvm::*aarch64-handler-pop-label*
                                      :bl)))
         (t
          (emit-aarch64-u32 buf #xD503201F)))   ; NOP if no pop helper
       (emit-aarch64-u32 buf #x14000002)        ; B +2 → do_eret
       ;; slot_1C0 (instr 21): STR XZR, [x16] (clear 1C0 fallback)
       (emit-aarch64-u32 buf #xF900021F)
       ;; do_eret (instr 22):
       (emit-aarch64-u32 buf #xD5184031)        ; MSR ELR_EL1, x17
       (emit-aarch64-u32 buf #xD2820120)        ; MOVZ x0, #0x1009
       (emit-aarch64-u32 buf #xF2BBD5A0)        ; MOVK x0, #0xDEAD, lsl #16
       (emit-aarch64-u32 buf #xD69F03E0)        ; ERET
       ;; HALT (instr 26):
       (emit-aarch64-u32 buf #x14000000)        ; B .
       ;; Fill remaining 6 instructions with NOP
       (dotimes (i 6)
         (emit-aarch64-u32 buf #xD503201F)))
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
       ;; Phase 3(e) entry-5: when slot 180 is the active handler,
       ;; BL the pop helper instead of STR XZR — so the handler stack
       ;; depth stays balanced after an IRQ-deadline longjmp (no
       ;; explicit CLEAR-HANDLER runs on that path, mirroring x64's
       ;; SIGSEGV-stub-pops design).  For the slot-1C0 fallback (set
       ;; by the SAVE-OUTER trap, not part of the per-fork stack),
       ;; keep the legacy STR XZR.  Discriminate via TBNZ x0, #6 —
       ;; the only bit that differs between 0x180 (bit6=0) and 0x1C0
       ;; (bit6=1) within the LSB nibble of the address.
       ;;
       ;; Layout grows from 28 emits + 4 NOPs to 31 emits + 1 NOP.
       ;; CBZ/B.NE forward offsets shift by +3.
       (progn
         (emit-aarch64-u32 buf #xA9BF07E0)  ;  1. STP x0,x1,[SP,#-16]!
         (emit-aarch64-u32 buf #xD2A10020)  ;  2. MOVZ x0,#0x0801,LSL #16  (GICC base)
         (emit-aarch64-u32 buf #xB9400C01)  ;  3. LDR w1,[x0,#0x0C]        (GICC_IAR)
         (emit-aarch64-u32 buf #xB9001001)  ;  4. STR w1,[x0,#0x10]        (GICC_EOIR)
         (emit-aarch64-u32 buf #xD29E8480)  ;  5. MOVZ x0,#0xF424          (62500)
         (emit-aarch64-u32 buf #xD51BE300)  ;  6. MSR CNTV_TVAL_EL0,x0     (re-arm)
         (emit-aarch64-u32 buf #xD2803000)  ;  7. MOVZ x0,#0x0180
         (emit-aarch64-u32 buf #xF2A20000)  ;  8. MOVK x0,#0x1000,LSL #16  (x0 = 0x10000180)
         (emit-aarch64-u32 buf #xF9457801)  ;  9. LDR x1,[x0,#2800]        (deadline @ slot 0xC70)
         (emit-aarch64-u32 buf #xB4000281)  ; 10. CBZ x1,+20 → NORMAL      (not armed)
         (emit-aarch64-u32 buf #xF1000421)  ; 11. SUBS x1,x1,#1
         (emit-aarch64-u32 buf #xF9057801)  ; 12. STR x1,[x0,#2800]
         (emit-aarch64-u32 buf #x54000221)  ; 13. B.NE +17 → NORMAL        (not yet zero)
         (emit-aarch64-u32 buf #xF9400001)  ; 14. LDR x1,[x0]              (slot 180 SP)
         (emit-aarch64-u32 buf #xB5000081)  ; 15. CBNZ x1,+4 → DO_LJ       (use slot 180)
         (emit-aarch64-u32 buf #x91010000)  ; 16. ADD x0,x0,#0x40          (→ slot 1C0)
         (emit-aarch64-u32 buf #xF9400001)  ; 17. LDR x1,[x0]              (slot 1C0 SP)
         (emit-aarch64-u32 buf #xB4000181)  ; 18. CBZ x1,+12 → NORMAL      (no handler at all)
         ;; DO_LJ (instr 19):
         (emit-aarch64-u32 buf #x9100003F)  ; 19. ADD SP,x1,#0
         (emit-aarch64-u32 buf #xF940041D)  ; 20. LDR x29,[x0,#8]
         (emit-aarch64-u32 buf #xF9400801)  ; 21. LDR x1,[x0,#16]
         ;; Discriminate slot 180 vs 1C0 by bit 6 of x0.
         (emit-aarch64-u32 buf #x37300060)  ; 22. TBNZ x0,#6,+3 → slot_1C0_path
         (cond
           (modus.mvm::*aarch64-handler-pop-label*
            (let ((idx (modus.mvm::a64-current-index buf)))
              (modus.mvm::a64-bl buf 0)        ; 23. BL pop_helper (slot 180 path)
              (modus.mvm::a64-add-fixup buf idx
                                        modus.mvm::*aarch64-handler-pop-label*
                                        :bl)))
           (t
            (emit-aarch64-u32 buf #xF900001F))) ; fallback: STR XZR,[x0]
         (emit-aarch64-u32 buf #x14000002)  ; 24. B +2 → do_eret
         (emit-aarch64-u32 buf #xF900001F)  ; 25. STR XZR,[x0] (slot_1C0_path — clear 1C0)
         ;; do_eret (instr 26):
         (emit-aarch64-u32 buf #xD5184021)  ; 26. MSR ELR_EL1,x1
         (emit-aarch64-u32 buf #xD2820120)  ; 27. MOVZ x0,#0x1009
         (emit-aarch64-u32 buf #xF2BBD5A0)  ; 28. MOVK x0,#0xDEAD,LSL #16  (X0 = T)
         (emit-aarch64-u32 buf #xD69F03E0)  ; 29. ERET (longjmp)
         ;; NORMAL (instr 30):
         (emit-aarch64-u32 buf #xA8C107E0)  ; 30. LDP x0,x1,[SP],#16
         (emit-aarch64-u32 buf #xD69F03E0)  ; 31. ERET (normal return)
         ;; Fill remaining 1 instruction with NOP
         (emit-aarch64-u32 buf #xD503201F)))
      ((= entry 6)
       ;; DIAGNOSTIC: FIQ probe — write 'f' to UART each tick.  GICv2 on
       ;; QEMU virt routes Group 0 interrupts as FIQ to non-secure EL1,
       ;; and the virtual timer is Group 0 by default.  If we see 'f'
       ;; chars but no '!', the vtimer fires as FIQ not IRQ.
       (progn
         (emit-aarch64-u32 buf #xA9BF07E0)  ; STP x0,x1,[SP,#-16]!
         (emit-aarch64-u32 buf #xD2A40000)  ; MOVZ x0,#0x2000,LSL #16  (UART VA)
         (emit-aarch64-u32 buf #xD2800CC1)  ; MOVZ x1,#0x66            ('f')
         (emit-aarch64-u32 buf #xB9000001)  ; STR  w1,[x0]
         (emit-aarch64-u32 buf #xD2A10020)  ; MOVZ x0,#0x0801,LSL #16  (GICC base)
         (emit-aarch64-u32 buf #xB9400C01)  ; LDR  w1,[x0,#0x0C]       (GICC_IAR)
         (emit-aarch64-u32 buf #xB9001001)  ; STR  w1,[x0,#0x10]       (GICC_EOIR)
         (emit-aarch64-u32 buf #xD29E8480)  ; MOVZ x0,#0xF424          (62500)
         (emit-aarch64-u32 buf #xD51BE300)  ; MSR  CNTV_TVAL_EL0,x0    (re-arm)
         (emit-aarch64-u32 buf #xA8C107E0)  ; LDP  x0,x1,[SP],#16
         (emit-aarch64-u32 buf #xD69F03E0)  ; ERET
         (dotimes (i 21)
           (emit-aarch64-u32 buf #xD503201F))))
      (t
       ;; All other entries: B . (infinite loop for debugging)
       (progn
         (emit-aarch64-u32 buf #x14000000)    ; B . (branch to self)
         (dotimes (i 31)
           (emit-aarch64-u32 buf #xD503201F)))))))

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
;; ATTEMPTED stack move (task #47, 2026-05-13, reverted):
;; Image loads at PA 0x40080000 (= VA 0x80000) and grows up; the ANSI
;; test image is ~40 MB (ends at ~VA 0x02880000).  The stack top at
;; VA 0x00200000 (= PA 0x40200000) sits 1.5 MB *inside* the image, so
;; every stack push overwrites whatever the build happened to emit at
;; file offset 0x180000+.  Each byte the image grew shifts what got
;; clobbered, which is the AArch64 layout-fragility source (see
;; reference_aarch64_ansi_saturation).
;;
;; Moved to VA 0x08000000 to fix this — that PA is 88 MB above the
;; largest reasonable image end and 16 MB below the alloc heap base,
;; safe in both directions.  But the move *exposes* a different wedge
;; somewhere around fill tests (17138-17140) where the per-test
;; deadline timer fails to recover — likely a layout-shift artifact
;; in the same family.  Reverted while we investigate; the audit
;; comments below stay so the trail is preserved.
;; Stack moved to VA 0x08000000 (PA 0x48000000) — 88 MB above the
;; ~42 MB image end, 16 MB below the alloc heap base (0x09000000).
;; Resolves the structural corruption where stack pushes at VA
;; 0x00200000 were overwriting code at PA 0x40080000+ via shared
;; physical mapping.  T:13672 wedge (reference_aarch64_t13672_corruption.md):
;; corrupted bytes at VA 0x40090110 were ACTUALLY the stack working
;; memory at PA 0x40090110 = VA 0x00090110, written when SP grew
;; past ~1.4 MB.  See task #47 audit for the original investigation.
(defconstant +tdk-stack-va+      #x08000000)  ; Stack top (outside image)

;; NIL register (x26) init value for the fixpoint entry.  Legacy fixpoint
;; builds use 0; the bare-metal AArch64 ANSI runner (build-aarch64.lisp)
;; sets #xDEAD0001 to match the modern compiler's +nil-value+ (compiled
;; literals and interp.lisp's truthiness both key on that exact bit
;; pattern).  Default 0 keeps existing fixpoint builds byte-identical.
(defvar *aarch64-fixpoint-nil-value* 0)

;; Boot re-entry guard (see Phase 0 in emit-aarch64-fixpoint-entry).
;; Default NIL emits nothing — legacy fixpoint builds byte-identical.
;; The bare-metal ANSI runner sets T: a wild jump to the image base then
;; longjmps to the armed handler-case instead of re-running boot (which
;; would rebuild the page tables under the live MMU and kill the machine).
(defvar *aarch64-fixpoint-reentry-guard* nil)


;; Heap layout for Cheney semispace GC.  Total heap = 112 MB
;; (0x09000000-0x10000000); split into two 56-MB semispaces.  The
;; boot loader sets x24=base and x25=mid-point (end of the initial
;; from-space).  When x24 hits x25 the +op-gc-check+ pattern BL's the
;; GC trampoline (translate-aarch64.lisp::emit-aarch64-handler-helpers),
;; which calls %gc-collect (gc.lisp).  After collection the trampoline
;; reloads x24/x25 from updated metadata slots.  See mvm/gc.lisp for
;; the algorithm and 0x10000040–0x10000078 metadata layout.
(defconstant +tdk-cons-base-va+   #x09000000)  ; Cons alloc base
(defconstant +tdk-cons-half-va+   #x0C800000)  ; Mid-point of 112-MB heap
(defconstant +tdk-cons-end-va+    #x10000000)  ; End of 112-MB heap
(defconstant +tdk-cons-limit-va+  #x0C800000)  ; Initial alloc limit (= half)
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
    ;; Phase 0 (OPTIONAL, gated on *aarch64-fixpoint-reentry-guard*):
    ;; BOOT RE-ENTRY GUARD.  A wild indirect jump in the running kernel
    ;; (e.g. a corrupted longjmp/NLX target of exactly the image base)
    ;; RE-EXECUTES this boot preamble — which then rebuilds the page
    ;; tables under the live MMU, unmapping the code AND the exception
    ;; vectors mid-flight: the machine dies in a recursive fetch abort
    ;; at vector entry 4 (observed: bare-AArch64 ANSI runner, test
    ;; 12996's GO-from-unwind-protect-cleanup NLX).  Guard: the cold
    ;; boot stores a 64-bit magic at PA 0x50000FF0 just before entering
    ;; native code; if the entry ever runs with the magic present, this
    ;; is a RE-entry — treat it exactly like a sync exception: longjmp
    ;; to the armed handler-case frame (slot 0x10000180, fallback
    ;; 0x100001C0) with X0 = T, mirroring vector entry 4 (minus ERET —
    ;; we are not in an exception).  PA 0x50000FF0 == VA 0x50000FF0
    ;; under the identity L1[1] block, so the same address works both
    ;; pre-MMU (cold, garbage-RAM false positive ~2^-64) and post-MMU
    ;; (re-entry).  Default NIL emits NOTHING (legacy byte-identical).
    (when *aarch64-fixpoint-reentry-guard*
      ;; x16 = 0x50000FF0; x17 = [x16]
      (emit-aarch64-movz buf x16 #x0FF0 0)
      (emit-aarch64-movk buf x16 #x5000 16)
      (emit-aarch64-u32 buf #xF9400211)          ; LDR x17,[x16]
      ;; x18 = magic 0x1DEADEADB007CAFE
      (emit-aarch64-movz buf 18 #xCAFE 0)
      (emit-aarch64-movk buf 18 #xB007 16)
      (emit-aarch64-movk buf 18 #xDEAD 32)
      (emit-aarch64-movk buf 18 #x1DEA 48)
      ;; CMP x17,x18 ; B.NE cold  (patched below)
      (emit-aarch64-u32 buf #xEB12023F)          ; CMP x17,x18 (SUBS XZR,x17,x18)
      (let ((bne-idx (a64-buffer-position buf)))
        (emit-aarch64-u32 buf 0)                 ; B.NE placeholder
        ;; ---- RE-ENTRY RECOVERY (mirrors vector entry 4) ----
        ;; Diag: record the jumper's LR at 0x10000C30 (FELR field).
        (emit-aarch64-u32 buf #xD2818611)        ; MOVZ x17,#0x0C30
        (emit-aarch64-u32 buf #xF2A20011)        ; MOVK x17,#0x1000,lsl #16
        (emit-aarch64-u32 buf #xF900023E)        ; STR x30,[x17]
        (emit-aarch64-u32 buf #xD2803010)        ; MOVZ x16,#0x0180
        (emit-aarch64-u32 buf #xF2A20010)        ; MOVK x16,#0x1000,lsl #16
        (emit-aarch64-u32 buf #xF9400211)        ; LDR x17,[x16]   (slot 180 SP)
        (emit-aarch64-u32 buf #xB5000091)        ; CBNZ x17,+4 → DO_LJ
        (emit-aarch64-u32 buf #x91010210)        ; ADD x16,x16,#0x40 (→ 0x1C0)
        (emit-aarch64-u32 buf #xF9400211)        ; LDR x17,[x16]   (slot 1C0 SP)
        (emit-aarch64-u32 buf #xB4000171)        ; CBZ x17,+11 → WFI_HALT
        ;; DO_LJ:
        (emit-aarch64-u32 buf #x9100023F)        ; ADD sp,x17,#0
        (emit-aarch64-u32 buf #xF940061D)        ; LDR x29,[x16,#8]
        (emit-aarch64-u32 buf #xF9400A11)        ; LDR x17,[x16,#16]
        (emit-aarch64-u32 buf #x37300070)        ; TBNZ x16,#6,+3 → slot_1C0
        (cond
          (modus.mvm::*aarch64-handler-pop-label*
           (let ((idx (modus.mvm::a64-current-index buf)))
             (modus.mvm::a64-bl buf 0)           ; BL pop_helper (slot 180 path)
             (modus.mvm::a64-add-fixup buf idx
                                       modus.mvm::*aarch64-handler-pop-label*
                                       :bl)))
          (t
           (emit-aarch64-u32 buf #xD503201F)))   ; NOP if no pop helper
        (emit-aarch64-u32 buf #x14000002)        ; B +2 → do_br
        (emit-aarch64-u32 buf #xF900021F)        ; slot_1C0: STR XZR,[x16]
        ;; do_br:
        (emit-aarch64-u32 buf #xD2820120)        ; MOVZ x0,#0x1009
        (emit-aarch64-u32 buf #xF2BBD5A0)        ; MOVK x0,#0xDEAD,lsl #16 (X0=T)
        (emit-aarch64-u32 buf #xD61F0220)        ; BR x17 (longjmp)
        ;; WFI_HALT (no handler armed at all):
        (emit-aarch64-u32 buf #xD503207F)        ; WFI
        (emit-aarch64-u32 buf #x17FFFFFF)        ; B -1 (back to WFI)
        ;; Patch the B.NE to land here (cold boot continues).
        (let ((off (- (a64-buffer-position buf) bne-idx)))
          (setf (aref (a64-buffer-code buf) bne-idx)
                (logior #x54000000 (ash (logand off #x7FFFF) 5) 1)))))

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
    (let ((zero-loop-pos (a64-buffer-position buf)))
      ;; STR XZR, [X0], #8  (post-index)
      ;; Encoding: 11 111000 00 0 000001000 01 00000 11111
      (emit-aarch64-u32 buf #xF800841F)         ; STR XZR, [X0], #8
      ;; SUB X2, X2, #1
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      ;; CBNZ X2, loop  (back 2 instructions = -8 bytes = -2 words)
      (let ((offset (- zero-loop-pos (a64-buffer-position buf))))
        (emit-aarch64-u32 buf (logior (ash #b10110101 24) ; CBNZ (64-bit)
                                      (ash (logand offset #x7FFFF) 5)
                                      x2))))

    ;; 3. Zero L2 table (4KB) at PA 0x40011000
    (emit-aarch64-load-imm64 buf x0 +tdk-l2-table-pa+)
    (emit-aarch64-movz buf x2 512 0)
    (let ((zero-loop2-pos (a64-buffer-position buf)))
      (emit-aarch64-u32 buf #xF800841F)         ; STR XZR, [X0], #8
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      (let ((offset (- zero-loop2-pos (a64-buffer-position buf))))
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
    (let ((fill-loop-pos (a64-buffer-position buf)))
      ;; STR X1, [X0], #8
      (emit-aarch64-u32 buf #xF8008401)
      ;; ADD X1, X1, X3  (next PA)
      ;; sf=1 op=0 S=0 01011 shift=00 0 Rm=X3 imm6=0 Rn=X1 Rd=X1
      (emit-aarch64-u32 buf #x8B030021)
      ;; SUB X2, X2, #1
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      ;; CBNZ X2, fill_loop
      (let ((offset (- fill-loop-pos (a64-buffer-position buf))))
        (emit-aarch64-u32 buf (logior (ash #b10110101 24)
                                      (ash (logand offset #x7FFFF) 5)
                                      x2))))

    ;; 6b. DIAG: mark image-covering L2 entries RO (AP=10 at EL1).
    ;;    Image lives at VA 0x00080000 to ~VA 0x02880000.  L2 entries 0..20
    ;;    cover VA 0x00000000-0x02BFFFFF — including all image bytes.
    ;;    Setting AP[2]=1 (bit 7) on those entries means writes via low-VA
    ;;    into image faults with permission abort → entry-4 → diag slots
    ;;    capture the writing PC.  This catches the corruption-of-code
    ;;    class (see reference_aarch64_post_stack_move_wedge.md) directly.
    ;;
    ;;    Entries 0..20: read L2[i], OR in 0x80 (AP[2]=1), write back.
    (emit-aarch64-load-imm64 buf x0 +tdk-l2-table-pa+)  ; x0 = L2 base
    (emit-aarch64-movz buf x2 21 0)                     ; x2 = count (21 entries)
    (let ((ro-loop-pos (a64-buffer-position buf)))
      ;; LDR X1, [X0]
      (emit-aarch64-u32 buf #xF9400001)
      ;; ORR X1, X1, #0x80 (bit 7 = AP[2])
      ;; Logical-immediate AND/ORR/EOR: 0x80 = bit 7 only.
      ;; ORR (immediate) Xd, Xn, #imm = sf=1 0x32x | imms<<10 | immr<<16 | Rn<<5 | Rd
      ;; For 0x80 = 0b1<<7, element size 64: N=1, immr=63-7=56 mod 64 → wait
      ;; This is complex.  Simpler: MOV imm + ORR reg.
      (emit-aarch64-movz buf x4 #x80 0)                 ; x4 = 0x80
      ;; ORR X1, X1, X4 — sf=1 opc=01 01010 shift=00 N=0 Rm=4 imm6=0 Rn=1 Rd=1
      (emit-aarch64-u32 buf (logior #xAA040021))
      ;; STR X1, [X0], #8 (post-index +8)
      (emit-aarch64-u32 buf #xF8008401)
      ;; SUB X2, X2, #1
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      ;; CBNZ X2, ro_loop
      (let ((offset (- ro-loop-pos (a64-buffer-position buf))))
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
    (let ((pci-loop-pos (a64-buffer-position buf)))
      ;; STR X1, [X0], #8
      (emit-aarch64-u32 buf #xF8008401)
      ;; ADD X1, X1, X3 (next PA)
      (emit-aarch64-u32 buf #x8B030021)
      ;; SUB X2, X2, #1
      (emit-aarch64-u32 buf (logior (ash 1 31) (ash #b10 29) (ash #b100010 23) (ash 1 10) (ash x2 5) x2))
      ;; CBNZ X2, pci_loop
      (let ((offset (- pci-loop-pos (a64-buffer-position buf))))
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

    ;; 15. Set stack pointer to +tdk-stack-va+.
    ;; RAW-ADDR-AUDIT: SP gets a raw VA, not a Lisp value.  See the
    ;; defconstant for the layout-fragility caveat — every byte that
    ;; crosses the stack region between boot and the first GC scan is
    ;; un-tagged and trusted.  Also influences GC: %gc-init's
    ;; stack-base argument MUST match this value (in raw bytes), and
    ;; the GC trampoline must apply LSL/ASR around the mem-ref :u64
    ;; pair that bridges the trampoline (raw) and %gc-collect (Lisp).
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
    ;; RAW-ADDR-AUDIT: x24 (alloc ptr) and x25 (alloc limit) hold RAW
    ;; byte addresses.  Compiled code BUMPS x24 by 16 per CONS and by
    ;; alloc-bytes per ALLOC-OBJ, then writes through it.  The GC
    ;; trampoline rotates x24/x25 via the LSL/ASR convention at
    ;; 0x10000070/78 (see translate-aarch64.lisp).  x26 = NIL is also
    ;; raw 0 — *not* a tagged immediate — and the AArch64 call-ind
    ;; check explicitly CBZ's on it to catch `(funcall NIL)`.
    (emit-aarch64-load-imm64 buf x24 +tdk-cons-base-va+)   ; cons alloc
    (emit-aarch64-load-imm64 buf x25 +tdk-cons-limit-va+)  ; cons limit
    ;; NIL: 0 for legacy fixpoint builds, #xDEAD0001 for the ANSI runner
    ;; (see *aarch64-fixpoint-nil-value* defvar above).  With value 0 this
    ;; emits the single MOVZ the legacy sequence used (byte-identical).
    (emit-aarch64-load-imm64 buf x26 *aarch64-fixpoint-nil-value*)

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

    ;; 19c. Record code_base and code_end at fixed slots so functionp's
    ;; range-check arm can classify raw fn-addrs correctly.  Mirrors x64
    ;; emit-code-bounds-init; cross.lisp patches the MOVZ/MOVK pairs
    ;; once the runtime layout is known.  Without this, fn-addrs whose
    ;; low byte happens to be 0x05 get misclassified as characters by
    ;; functionp's fallback path.
    (emit-aarch64-code-bounds-init buf)

    ;; 19d. Boot-completed magic for the re-entry guard (Phase 0).  MMU
    ;; is on; VA 0x50000FF0 → PA 0x50000FF0 via the identity L1[1] block.
    (when *aarch64-fixpoint-reentry-guard*
      (emit-aarch64-movz buf x16 #x0FF0 0)
      (emit-aarch64-movk buf x16 #x5000 16)
      (emit-aarch64-movz buf x17 #xCAFE 0)
      (emit-aarch64-movk buf x17 #xB007 16)
      (emit-aarch64-movk buf x17 #xDEAD 32)
      (emit-aarch64-movk buf x17 #x1DEA 48)
      (emit-aarch64-u32 buf #xF9000211))         ; STR x17,[x16]

    ;; 20. Branch to native code via offset-mapped VA
    ;; Native code starts at offset 0x1000 in the image = VA 0x1000
    ;; (Boot preamble occupies offsets 0x000-0x7FF, vectors at 0x800-0xFFF)
    ;; We need to branch from identity-mapped VA (0x4000xxxx) to offset VA (0x1000)
    (let* ((current-insn (a64-buffer-position buf))
           (native-start-insn 1024)               ; instruction 1024 = offset 0x1000
           (skip (- native-start-insn current-insn)))
      ;; B forward to native code at offset 0x1000
      (emit-aarch64-u32 buf (logior (ash #b000101 26) (logand skip #x3FFFFFF)))
      ;; Pad with NOPs to offset 0x800 (instruction 512)
      (let ((pad (- 512 (a64-buffer-position buf))))
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
