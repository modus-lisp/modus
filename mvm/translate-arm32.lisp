;;;; translate-arm32.lisp - MVM to ARM32 (ARMv5) Translator
;;;;
;;;; Translates MVM virtual ISA bytecode to native ARM32 (A32) machine code.
;;;; Targets ARMv5 (ARM mode, 32-bit fixed-width instructions).
;;;;
;;;; Key characteristics:
;;;;   - Every instruction is 32 bits, little-endian
;;;;   - Condition code in bits 31:28 on every instruction
;;;;   - 16 registers (r0-r15), r13=SP, r14=LR, r15=PC
;;;;   - No hardware divide (ARMv5 lacks UDIV/SDIV) — uses software divide
;;;;   - Rotated-immediate encoding: 8-bit value rotated by even number
;;;;   - Branch offsets relative to PC+8 (2-stage pipeline legacy)
;;;;
;;;; Register mapping:
;;;;   V0-V3  -> r0-r3  (args, caller-saved)
;;;;   V4-V7  -> r4-r7  (callee-saved)
;;;;   V8-V15 -> stack spill
;;;;   VR     -> r0     (return value, aliases V0)
;;;;   VA     -> r9     (alloc pointer, callee-saved)
;;;;   VL     -> r10    (alloc limit, callee-saved)
;;;;   VN     -> r8     (NIL constant, callee-saved)
;;;;   VSP    -> r13    (stack pointer)
;;;;   VFP    -> r11    (frame pointer)
;;;;
;;;; Scratch registers: r12 (IP), r14 (LR, after saving in prologue)
;;;;
;;;; QEMU target: versatilepb -cpu arm926
;;;;   PL011 UART at 0x101F1000

(in-package :modus.mvm)

;;; ============================================================
;;; ARMv5/ARMv7 Mode Selection
;;; ============================================================

(defvar *arm32-v7* nil
  "When T, emit ARMv7-A instructions (SDIV, MOVW/MOVT, DMB, LDREX/STREX).
   When NIL (default), emit ARMv5 instructions (software divide, SWP, MCR fence).")

(defvar *arm32-uart-override* nil
  "When non-NIL, use this UART base address instead of the default.")

(defvar *arm32-labels-ht* nil
  "Global hash table for ARM32 label positions (avoids buf corruption).")

(defvar *arm32-fn-labels* nil
  "Reusable array for per-function branch target labels (avoids hash table GC pressure).")

(defvar *arm32-code-bytes* nil
  "Pre-allocated byte array for ARM32 code output (1048576 bytes).")

(defun arm32-uart-base ()
  "Return the UART base address for the current mode.
   ARMv5 versatilepb: PL011 at 0x101F1000
   ARMv7 virt:        PL011 at 0x09000000
   ARMv7 RPi:         PL011 at 0x3F201000"
  (or *arm32-uart-override*
      (if *arm32-v7* #x09000000 #x101F1000)))

;;; ============================================================
;;; ARM32 Physical Register Constants
;;; ============================================================

(defconstant +arm-r0+   0)
(defconstant +arm-r1+   1)
(defconstant +arm-r2+   2)
(defconstant +arm-r3+   3)
(defconstant +arm-r4+   4)
(defconstant +arm-r5+   5)
(defconstant +arm-r6+   6)
(defconstant +arm-r7+   7)
(defconstant +arm-r8+   8)    ; VN (NIL)
(defconstant +arm-r9+   9)    ; VA (alloc pointer)
(defconstant +arm-r10+ 10)    ; VL (alloc limit)
(defconstant +arm-r11+ 11)    ; VFP (frame pointer)
(defconstant +arm-r12+ 12)    ; IP (scratch 0)
(defconstant +arm-sp+  13)    ; Stack pointer
(defconstant +arm-lr+  14)    ; Link register (scratch 1 after save)
(defconstant +arm-pc+  15)    ; Program counter

;;; ============================================================
;;; Virtual -> Physical Register Mapping
;;; ============================================================

(defparameter *arm32-vreg-map*
  (vector +arm-r0+  +arm-r1+  +arm-r2+  +arm-r3+    ; V0-V3
          +arm-r4+  +arm-r5+  +arm-r6+  +arm-r7+    ; V4-V7
          nil nil nil nil                              ; V8-V11 (spill)
          nil nil nil nil                              ; V12-V15 (spill)
          +arm-r0+                                     ; VR (aliases V0)
          +arm-r9+                                     ; VA
          +arm-r10+                                    ; VL
          +arm-r8+                                     ; VN
          +arm-sp+                                     ; VSP
          +arm-r11+                                    ; VFP
          nil))                                        ; VPC

(defun arm32-phys-reg (vreg)
  "Return the physical ARM register for a virtual register, or NIL if spilled."
  (and (< vreg (length *arm32-vreg-map*))
       (aref *arm32-vreg-map* vreg)))

(defconstant +arm32-max-inline+ 8
  "Virtual registers V0-V7 have dedicated physical registers.")

(defconstant +arm32-frame-size+ 512
  "Stack frame: 512 bytes (8-byte aligned).
   Layout from SP: frame slots 0-119 at SP+0..SP+476 (via OBJ-REF VFP),
   spill V8-V15 at SP+480..SP+508.
   Previous 192-byte frame caused spill/slot overlap for 32+ param functions
   (e.g., fe-mul-4 with 31 params: let bindings at slot 32 = SP+128
   collided with V8 spill at SP+128).")

(defconstant +arm32-spill-base+ 482
  "Spill slots start at VFP+482 = SP+480, safely past max frame slot 119 at SP+476.
   Spills V8-V15 use SP+480..SP+508 (8 slots * 4 bytes).")

(defun arm32-spill-offset (vreg)
  "Compute the frame offset for a spilled virtual register."
  (let ((slot (- vreg +arm32-max-inline+)))
    (+ +arm32-spill-base+ (* slot 4))))

;;; ============================================================
;;; ARM32 Condition Codes
;;; ============================================================

(defconstant +arm-cc-eq+ #x0)   ; Z=1 (equal)
(defconstant +arm-cc-ne+ #x1)   ; Z=0 (not equal)
(defconstant +arm-cc-cs+ #x2)   ; C=1 (unsigned >=)
(defconstant +arm-cc-cc+ #x3)   ; C=0 (unsigned <)
(defconstant +arm-cc-mi+ #x4)   ; N=1 (negative)
(defconstant +arm-cc-pl+ #x5)   ; N=0 (positive/zero)
(defconstant +arm-cc-hi+ #x8)   ; C=1 & Z=0 (unsigned >)
(defconstant +arm-cc-ls+ #x9)   ; C=0 | Z=1 (unsigned <=)
(defconstant +arm-cc-ge+ #xA)   ; N=V (signed >=)
(defconstant +arm-cc-lt+ #xB)   ; N!=V (signed <)
(defconstant +arm-cc-gt+ #xC)   ; Z=0 & N=V (signed >)
(defconstant +arm-cc-le+ #xD)   ; Z=1 | N!=V (signed <=)
(defconstant +arm-cc-al+ #xE)   ; always

;;; ============================================================
;;; ARM32 Code Buffer
;;; ============================================================
;;;
;;; ARM32 instructions are 32-bit, little-endian.
;;; Buffer stores instruction words; converted to LE bytes at end.

(defstruct arm32-buffer
  (code (make-array 262144))            ; fixed-size, position tracks fill
  (labels (make-hash-table :test 'eql))
  (fixups nil)
  (div-label nil)    ; label ID for software divide routine
  (position 0))      ; current instruction index (word count)

(defun arm32-emit (buf word)
  "Emit a 32-bit ARM instruction."
  (let ((pos (arm32-buffer-position buf)))
    (setf (aref (arm32-buffer-code buf) pos) (logand word #xFFFFFFFF))
    (setf (arm32-buffer-position buf) (+ pos 1))))

(defun arm32-current-index (buf)
  "Current instruction index (word count)."
  (arm32-buffer-position buf))

(defun arm32-emit-label (buf label-id)
  "Record that LABEL-ID is at the current code position."
  (setf (gethash label-id (arm32-buffer-labels buf))
        (arm32-current-index buf)))

(defun arm32-emit-fixup (buf label-id fixup-type)
  "Record a fixup at the current position for LABEL-ID."
  (push (list (1- (arm32-current-index buf)) label-id fixup-type)
        (arm32-buffer-fixups buf)))

(defun arm32-buffer-to-bytes (buf)
  "Convert ARM32 word buffer to little-endian byte vector."
  (let* ((nwords (arm32-buffer-position buf))
         (bytes (make-array (* nwords 4))))
    (dotimes (i nwords bytes)
      (let ((w (aref (arm32-buffer-code buf) i)))
        ;; Little-endian: LSB first
        (setf (aref bytes (* i 4))       (logand w #xFF)
              (aref bytes (+ (* i 4) 1)) (logand (ash w -8) #xFF)
              (aref bytes (+ (* i 4) 2)) (logand (ash w -16) #xFF)
              (aref bytes (+ (* i 4) 3)) (logand (ash w -24) #xFF))))))

(defun arm32-resolve-fixups (buf)
  "Resolve all branch fixups.  Asserts the signed 24-bit imm fits
   before encoding (±2^23 instructions = ±32 MB).  Without the assert
   an over-range branch silently truncates to a wild jump."
  (let ((code (arm32-buffer-code buf)))
    (dolist (fixup (arm32-buffer-fixups buf))
      (destructuring-bind (index label-id type) fixup
        (let ((target (gethash label-id (arm32-buffer-labels buf))))
          (unless target
            (error "ARM32: undefined label ~D" label-id))
          ;; ARM branch offset: (target - source - 2) in instruction units
          ;; because PC reads as current_instruction + 8 at execution time
          (let ((offset (- target index 2)))
            (ecase type
              ((:b :bl :b-cond)
               (unless (<= (- (ash 1 23)) offset (1- (ash 1 23)))
                 (error "ARM32 ~A branch offset ~D out of range ~
                         [-2^23, 2^23-1] at index ~D — would silently truncate"
                        type offset index))
               ;; Patch bits [23:0] with signed 24-bit offset
               (let ((word (aref code index)))
                 (setf (aref code index)
                       (logior (logand word #xFF000000)
                               (logand offset #xFFFFFF))))))))))))

;;; ============================================================
;;; ARM32 Instruction Encoders
;;; ============================================================

;;; --- Data Processing (register operand) ---
;;; cond[31:28] | 00 | 0[25] | opcode[24:21] | S[20] | Rn[19:16] | Rd[15:12] | shift[11:7] | shtype[6:5] | 0[4] | Rm[3:0]

(defconstant +arm-dp-and+ #b0000)
(defconstant +arm-dp-eor+ #b0001)
(defconstant +arm-dp-sub+ #b0010)
(defconstant +arm-dp-rsb+ #b0011)
(defconstant +arm-dp-add+ #b0100)
(defconstant +arm-dp-tst+ #b1000)
(defconstant +arm-dp-cmp+ #b1010)
(defconstant +arm-dp-orr+ #b1100)
(defconstant +arm-dp-mov+ #b1101)
(defconstant +arm-dp-bic+ #b1110)
(defconstant +arm-dp-mvn+ #b1111)

(defconstant +arm-shift-lsl+ 0)
(defconstant +arm-shift-lsr+ 1)
(defconstant +arm-shift-asr+ 2)
(defconstant +arm-shift-ror+ 3)

(defun arm32-dp-reg (buf opcode rd rn rm &key (s 0) (shift-type 0) (shift-amt 0) (cond +arm-cc-al+))
  "Emit data-processing instruction with register operand."
  (arm32-emit buf (logior (ash cond 28)
                          (ash opcode 21)
                          (ash s 20)
                          (ash (logand rn #xF) 16)
                          (ash (logand rd #xF) 12)
                          (ash (logand shift-amt #x1F) 7)
                          (ash (logand shift-type #x3) 5)
                          (logand rm #xF))))

;;; --- Data Processing (immediate operand) ---
;;; cond[31:28] | 00 | 1[25] | opcode[24:21] | S[20] | Rn[19:16] | Rd[15:12] | rotate[11:8] | imm8[7:0]

(defun arm32-dp-imm (buf opcode rd rn rotate imm8 &key (s 0) (cond +arm-cc-al+))
  "Emit data-processing instruction with rotated immediate."
  (arm32-emit buf (logior (ash cond 28)
                          (ash 1 25)
                          (ash opcode 21)
                          (ash s 20)
                          (ash (logand rn #xF) 16)
                          (ash (logand rd #xF) 12)
                          (ash (logand rotate #xF) 8)
                          (logand imm8 #xFF))))

;;; --- Data Processing (register-shifted register) ---
;;; cond[31:28] | 00 | 0[25] | opcode[24:21] | S[20] | Rn[19:16] | Rd[15:12] | Rs[11:8] | 0[7] | shtype[6:5] | 1[4] | Rm[3:0]

(defun arm32-dp-reg-shift (buf opcode rd rn rm shift-type rs &key (s 0) (cond +arm-cc-al+))
  "Emit data-processing with register-controlled shift in operand2."
  (arm32-emit buf (logior (ash cond 28)
                          (ash opcode 21)
                          (ash s 20)
                          (ash (logand rn #xF) 16)
                          (ash (logand rd #xF) 12)
                          (ash (logand rs #xF) 8)
                          (ash (logand shift-type #x3) 5)
                          (ash 1 4)
                          (logand rm #xF))))

;;; --- Convenience wrappers ---

(defun arm32-add (buf rd rn rm)
  (arm32-dp-reg buf +arm-dp-add+ rd rn rm))

(defun arm32-sub (buf rd rn rm)
  (arm32-dp-reg buf +arm-dp-sub+ rd rn rm))

(defun arm32-rsb-imm (buf rd rn rotate imm8)
  "RSB Rd, Rn, #imm  (Rd = imm - Rn, for NEG: RSB Rd, Rm, #0)"
  (arm32-dp-imm buf +arm-dp-rsb+ rd rn rotate imm8))

(defun arm32-and-r (buf rd rn rm)
  (arm32-dp-reg buf +arm-dp-and+ rd rn rm))

(defun arm32-orr (buf rd rn rm)
  (arm32-dp-reg buf +arm-dp-orr+ rd rn rm))

(defun arm32-eor (buf rd rn rm)
  (arm32-dp-reg buf +arm-dp-eor+ rd rn rm))

(defun arm32-bic-imm (buf rd rn rotate imm8)
  "BIC Rd, Rn, #imm  (Rd = Rn AND NOT imm)"
  (arm32-dp-imm buf +arm-dp-bic+ rd rn rotate imm8))

(defun arm32-mov (buf rd rm)
  "MOV Rd, Rm"
  (arm32-dp-reg buf +arm-dp-mov+ rd 0 rm))

(defun arm32-mov-cond (buf cond rd rm)
  "MOV<cond> Rd, Rm"
  (arm32-dp-reg buf +arm-dp-mov+ rd 0 rm :cond cond))

(defun arm32-mov-imm (buf rd rotate imm8)
  "MOV Rd, #imm"
  (arm32-dp-imm buf +arm-dp-mov+ rd 0 rotate imm8))

(defun arm32-mov-imm-cond (buf cond rd rotate imm8)
  "MOV<cond> Rd, #imm"
  (arm32-dp-imm buf +arm-dp-mov+ rd 0 rotate imm8 :cond cond))

(defun arm32-mvn (buf rd rm)
  "MVN Rd, Rm  (bitwise NOT)"
  (arm32-dp-reg buf +arm-dp-mvn+ rd 0 rm))

(defun arm32-cmp (buf rn rm)
  "CMP Rn, Rm  (SUBS with Rd discarded)"
  (arm32-dp-reg buf +arm-dp-cmp+ 0 rn rm :s 1))

(defun arm32-cmp-imm (buf rn rotate imm8)
  "CMP Rn, #imm"
  (arm32-dp-imm buf +arm-dp-cmp+ 0 rn rotate imm8 :s 1))

(defun arm32-tst (buf rn rm)
  "TST Rn, Rm  (AND with result discarded, sets flags)"
  (arm32-dp-reg buf +arm-dp-tst+ 0 rn rm :s 1))

(defun arm32-add-imm (buf rd rn rotate imm8)
  "ADD Rd, Rn, #imm"
  (arm32-dp-imm buf +arm-dp-add+ rd rn rotate imm8))

(defun arm32-sub-imm (buf rd rn rotate imm8)
  "SUB Rd, Rn, #imm"
  (arm32-dp-imm buf +arm-dp-sub+ rd rn rotate imm8))

(defun arm32-orr-imm (buf rd rn rotate imm8)
  "ORR Rd, Rn, #imm"
  (arm32-dp-imm buf +arm-dp-orr+ rd rn rotate imm8))

(defun arm32-and-imm (buf rd rn rotate imm8)
  "AND Rd, Rn, #imm"
  (arm32-dp-imm buf +arm-dp-and+ rd rn rotate imm8))

;;; --- Shift by immediate (via MOV with shifted operand2) ---

(defun arm32-lsl-imm (buf rd rm amount)
  "LSL Rd, Rm, #amount"
  (arm32-dp-reg buf +arm-dp-mov+ rd 0 rm :shift-type +arm-shift-lsl+ :shift-amt amount))

(defun arm32-lsr-imm (buf rd rm amount)
  "LSR Rd, Rm, #amount"
  (arm32-dp-reg buf +arm-dp-mov+ rd 0 rm :shift-type +arm-shift-lsr+ :shift-amt amount))

(defun arm32-asr-imm (buf rd rm amount)
  "ASR Rd, Rm, #amount"
  (arm32-dp-reg buf +arm-dp-mov+ rd 0 rm :shift-type +arm-shift-asr+ :shift-amt amount))

;;; --- Shift by register ---

(defun arm32-lsl-reg (buf rd rm rs)
  "LSL Rd, Rm, Rs  (MOV Rd, Rm, LSL Rs)"
  (arm32-dp-reg-shift buf +arm-dp-mov+ rd 0 rm +arm-shift-lsl+ rs))

(defun arm32-lsr-reg (buf rd rm rs)
  "LSR Rd, Rm, Rs"
  (arm32-dp-reg-shift buf +arm-dp-mov+ rd 0 rm +arm-shift-lsr+ rs))

(defun arm32-asr-reg (buf rd rm rs)
  "ASR Rd, Rm, Rs"
  (arm32-dp-reg-shift buf +arm-dp-mov+ rd 0 rm +arm-shift-asr+ rs))

;;; --- Multiply ---
;;; cond[31:28] | 000000 | A[21] | S[20] | Rd[19:16] | Rn[15:12] | Rs[11:8] | 1001[7:4] | Rm[3:0]

(defun arm32-mul (buf rd rm rs)
  "MUL Rd, Rm, Rs -> Rd = Rm * Rs  (Rd must not equal Rm on ARMv5!)"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash (logand rd #xF) 16)
                          (ash (logand rs #xF) 8)
                          (ash #b1001 4)
                          (logand rm #xF))))

(defun arm32-umull (buf rdlo rdhi rm rs)
  "UMULL RdLo, RdHi, Rm, Rs -> RdHi:RdLo = Rm * Rs (unsigned 64-bit).
   Constraints (ARMv5): RdHi != RdLo, RdHi != Rm, RdLo != Rm."
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b100 21)
                          (ash (logand rdhi #xF) 16)
                          (ash (logand rdlo #xF) 12)
                          (ash (logand rs #xF) 8)
                          (ash #b1001 4)
                          (logand rm #xF))))

;;; --- Load/Store (word/byte, 12-bit immediate offset) ---
;;; cond[31:28] | 01 | 0[25] | P[24] | U[23] | B[22] | W[21] | L[20] | Rn[19:16] | Rd[15:12] | offset[11:0]

(defun arm32-mem (buf rd rn offset &key (load t) (byte nil) (writeback nil) (post nil))
  "General LDR/STR with immediate offset."
  (let* ((u (if (>= offset 0) 1 0))
         (abs-off (min (abs offset) #xFFF))
         (p (if post 0 1))
         (w (if writeback 1 0))
         (l (if load 1 0))
         (b (if byte 1 0)))
    (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                            (ash #b01 26)
                            (ash p 24)
                            (ash u 23)
                            (ash b 22)
                            (ash w 21)
                            (ash l 20)
                            (ash (logand rn #xF) 16)
                            (ash (logand rd #xF) 12)
                            abs-off))))

(defun arm32-ldr (buf rd rn offset)
  "LDR Rd, [Rn, #offset]"
  (arm32-mem buf rd rn offset :load t))

(defun arm32-str (buf rd rn offset)
  "STR Rd, [Rn, #offset]"
  (arm32-mem buf rd rn offset :load nil))

(defun arm32-ldrb (buf rd rn offset)
  "LDRB Rd, [Rn, #offset]"
  (arm32-mem buf rd rn offset :load t :byte t))

(defun arm32-strb (buf rd rn offset)
  "STRB Rd, [Rn, #offset]"
  (arm32-mem buf rd rn offset :load nil :byte t))

;; Pre-decrement store: STR Rd, [Rn, #-off]!
(defun arm32-str-pre (buf rd rn offset)
  "STR Rd, [Rn, #offset]!  (pre-indexed with writeback)"
  (arm32-mem buf rd rn offset :load nil :writeback t))

;; Post-increment load: LDR Rd, [Rn], #off
(defun arm32-ldr-post (buf rd rn offset)
  "LDR Rd, [Rn], #offset  (post-indexed)"
  (arm32-mem buf rd rn offset :load t :post t))

;;; --- Halfword Load/Store ---
;;; cond[31:28] | 000 | P[24] | U[23] | 1[22] | W[21] | L[20] | Rn[19:16] | Rd[15:12] | imm4H[11:8] | 1011[7:4] | imm4L[3:0]

(defun arm32-ldrh (buf rd rn offset)
  "LDRH Rd, [Rn, #offset]  (unsigned halfword load)"
  (let* ((u (if (>= offset 0) 1 0))
         (abs-off (min (abs offset) #xFF)))
    (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                            (ash 1 24)          ; P=1
                            (ash u 23)
                            (ash 1 22)          ; I=1 (imm offset)
                            (ash 1 20)          ; L=1 (load)
                            (ash (logand rn #xF) 16)
                            (ash (logand rd #xF) 12)
                            (ash (logand (ash abs-off -4) #xF) 8)
                            (ash #b1011 4)      ; unsigned halfword
                            (logand abs-off #xF)))))

(defun arm32-strh (buf rd rn offset)
  "STRH Rd, [Rn, #offset]  (halfword store)"
  (let* ((u (if (>= offset 0) 1 0))
         (abs-off (min (abs offset) #xFF)))
    (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                            (ash 1 24)
                            (ash u 23)
                            (ash 1 22)
                            (ash 0 20)          ; L=0 (store)
                            (ash (logand rn #xF) 16)
                            (ash (logand rd #xF) 12)
                            (ash (logand (ash abs-off -4) #xF) 8)
                            (ash #b1011 4)
                            (logand abs-off #xF)))))

;;; --- Branch ---
;;; cond[31:28] | 101 | L[24] | offset[23:0]

(defun arm32-b (buf &optional label-id)
  "B label  (unconditional branch)"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b101 25)))
  (when label-id
    (arm32-emit-fixup buf label-id :b)))

(defun arm32-bl (buf &optional label-id)
  "BL label  (branch with link)"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b101 25)
                          (ash 1 24)))
  (when label-id
    (arm32-emit-fixup buf label-id :bl)))

(defun arm32-b-cond (buf cond &optional label-id)
  "B<cond> label  (conditional branch)"
  (arm32-emit buf (logior (ash cond 28)
                          (ash #b101 25)))
  (when label-id
    (arm32-emit-fixup buf label-id :b-cond)))

(defun arm32-bx (buf rm)
  "BX Rm  (branch and exchange, used for return: BX LR)"
  ;; cond | 0001 0010 1111 1111 1111 0001 | Rm
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          #x012FFF10
                          (logand rm #xF))))

;;; --- PUSH / POP ---
;;; PUSH = STMDB SP!, {regs}:  cond | 1001 0010 1101 | reglist[15:0]
;;; POP  = LDMIA SP!, {regs}:  cond | 1000 1011 1101 | reglist[15:0]

(defun arm32-push (buf reglist)
  "PUSH {regs}  (STMDB SP!, {regs})"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28) #x092D0000 reglist)))

(defun arm32-pop (buf reglist)
  "POP {regs}  (LDMIA SP!, {regs})"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28) #x08BD0000 reglist)))

;;; --- NOP ---

(defun arm32-nop (buf)
  "NOP = MOV r0, r0"
  (arm32-mov buf +arm-r0+ +arm-r0+))

;;; --- SWI ---

(defun arm32-swi (buf imm)
  "SWI #imm  (software interrupt)"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b1111 24)
                          (logand imm #xFFFFFF))))

;;; --- SWP (atomic swap, ARMv5) ---

(defun arm32-swp (buf rd rm rn)
  "SWP Rd, Rm, [Rn]  (atomic word swap)"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b00010000 20)
                          (ash (logand rn #xF) 16)
                          (ash (logand rd #xF) 12)
                          (ash #b1001 4)
                          (logand rm #xF))))

;;; --- MRS / MSR (for CLI/STI) ---

(defun arm32-mrs (buf rd)
  "MRS Rd, CPSR"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          #x010F0000
                          (ash (logand rd #xF) 12))))

(defun arm32-msr-c (buf rm)
  "MSR CPSR_c, Rm  (write control field)"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          #x0129F000
                          (logand rm #xF))))

;;; --- MCR (for FENCE on ARMv5) ---

(defun arm32-fence (buf)
  "Data memory barrier.
   ARMv5: MCR p15, 0, r0, c7, c10, 4 (drain write buffer).
   ARMv7: DMB SY (proper data memory barrier)."
  (if *arm32-v7*
      (arm32-dmb buf)
      (arm32-emit buf #xEE070F9A)))

;;; --- ARMv7 Instructions ---

(defun arm32-movw (buf rd imm16)
  "MOVW Rd, #imm16  (ARMv7: load bits 0-15, zero bits 16-31)
   Encoding: cond 0011 0000 imm4 Rd imm12"
  (let ((imm4  (logand (ash imm16 -12) #xF))
        (imm12 (logand imm16 #xFFF)))
    (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                            (ash #b00110000 20)
                            (ash imm4 16)
                            (ash (logand rd #xF) 12)
                            imm12))))

(defun arm32-movt (buf rd imm16)
  "MOVT Rd, #imm16  (ARMv7: load bits 16-31, preserving 0-15)
   Encoding: cond 0011 0100 imm4 Rd imm12"
  (let ((imm4  (logand (ash imm16 -12) #xF))
        (imm12 (logand imm16 #xFFF)))
    (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                            (ash #b00110100 20)
                            (ash imm4 16)
                            (ash (logand rd #xF) 12)
                            imm12))))

(defun arm32-sdiv (buf rd rn rm)
  "SDIV Rd, Rn, Rm  (ARMv7-A: signed divide)
   Encoding: cond 0111 0001 Rd 1111 Rm 0001 Rn"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b01110001 20)
                          (ash (logand rd #xF) 16)
                          (ash #xF 12)
                          (ash (logand rm #xF) 8)
                          (ash #b0001 4)
                          (logand rn #xF))))

(defun arm32-udiv (buf rd rn rm)
  "UDIV Rd, Rn, Rm  (ARMv7-A: unsigned divide)
   Encoding: cond 0111 0011 Rd 1111 Rm 0001 Rn"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b01110011 20)
                          (ash (logand rd #xF) 16)
                          (ash #xF 12)
                          (ash (logand rm #xF) 8)
                          (ash #b0001 4)
                          (logand rn #xF))))

(defun arm32-dmb (buf)
  "DMB SY  (ARMv7: full system data memory barrier)
   Encoding: #xF57FF050 (unconditional, NV condition space)"
  (arm32-emit buf #xF57FF050))

(defun arm32-ldrex (buf rd rn)
  "LDREX Rd, [Rn]  (ARMv7: exclusive load word)
   Encoding: cond 0001 1001 Rn Rd 1111 1001 1111"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b00011001 20)
                          (ash (logand rn #xF) 16)
                          (ash (logand rd #xF) 12)
                          #xF9F)))

(defun arm32-strex (buf rd rm rn)
  "STREX Rd, Rm, [Rn]  (ARMv7: exclusive store word)
   Rd = status (0=success), Rm = value, Rn = address
   Encoding: cond 0001 1000 Rn Rd 1111 1001 Rm"
  (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                          (ash #b00011000 20)
                          (ash (logand rn #xF) 16)
                          (ash (logand rd #xF) 12)
                          (ash #xF9 4)
                          (logand rm #xF))))

(defun arm32-sev (buf)
  "SEV  (ARMv7: Send Event hint)
   Encoding: cond 0011 0010 0000 1111 0000 0000 0100"
  (arm32-emit buf #xE320F004))

(defun arm32-wfe (buf)
  "WFE  (ARMv7: Wait For Event hint)
   Encoding: cond 0011 0010 0000 1111 0000 0000 0010"
  (arm32-emit buf #xE320F002))

;;; ============================================================
;;; Immediate Encoding
;;; ============================================================

(defun arm32-encode-imm (value)
  "Try to encode VALUE as an ARM rotated immediate.
   Returns (VALUES rotate_imm imm8) or NIL if not encodable.
   The actual value is imm8 ROR (rotate_imm * 2)."
  (let ((val (logand value #xFFFFFFFF)))
    (dotimes (rot 16)
      (let* ((shift (* rot 2))
             ;; Rotate left by shift to undo the ROR encoding
             (rotated (logand (logior (ash val shift)
                                      (if (> shift 0)
                                          (ash val (- shift 32))
                                          0))
                              #xFFFFFFFF)))
        (when (<= rotated 255)
          (return-from arm32-encode-imm (cons rot (logand rotated #xFF))))))
    nil))

(defun arm32-load-imm32 (buf rd value)
  "Load an arbitrary 32-bit immediate into Rd.
   ARMv5: MOV if encodable, else MOV + up to 3 ORR (1-4 instructions).
   ARMv7: MOV if encodable, else MOVW + optional MOVT (1-2 instructions)."
  (let ((val (logand value #xFFFFFFFF)))
    ;; Try direct MOV (1 instruction, works on all ARM)
    (let ((enc (arm32-encode-imm val)))
      (when enc
        (let ((rot (car enc))
              (imm (cdr enc)))
          (arm32-mov-imm buf rd rot imm)
          (return-from arm32-load-imm32))))
    ;; Try MVN (1 instruction, works on all ARM)
    (let ((enc (arm32-encode-imm (logand (lognot val) #xFFFFFFFF))))
      (when enc
        (let ((rot (car enc))
              (imm (cdr enc)))
          (arm32-dp-imm buf +arm-dp-mvn+ rd 0 rot imm)
          (return-from arm32-load-imm32))))
    ;; Multi-instruction path diverges by mode
    (if *arm32-v7*
        ;; ARMv7: MOVW + MOVT (always <= 2 instructions)
        (let ((lo (logand val #xFFFF))
              (hi (logand (ash val -16) #xFFFF)))
          (arm32-movw buf rd lo)
          (when (plusp hi)
            (arm32-movt buf rd hi)))
        ;; ARMv5: MOV byte0 + ORR byte1 + ORR byte2 + ORR byte3
        (let ((b0 (logand val #xFF))
              (b1 (logand (ash val -8) #xFF))
              (b2 (logand (ash val -16) #xFF))
              (b3 (logand (ash val -24) #xFF)))
          (arm32-mov-imm buf rd 0 b0)
          (when (plusp b1)
            (arm32-orr-imm buf rd rd 12 b1))    ; rotate=12 → ROR 24 → byte at bits 8-15
          (when (plusp b2)
            (arm32-orr-imm buf rd rd 8 b2))     ; rotate=8 → ROR 16 → byte at bits 16-23
          (when (plusp b3)
            (arm32-orr-imm buf rd rd 4 b3))))))  ; rotate=4 → ROR 8 → byte at bits 24-31

;;; ============================================================
;;; Vreg Load / Store Helpers
;;; ============================================================

(defun arm32-load-vreg (buf phys-dst vreg)
  "Load virtual register VREG into physical register PHYS-DST."
  (let ((phys (arm32-phys-reg vreg)))
    (if phys
        (unless (= phys phys-dst)
          (arm32-mov buf phys-dst phys))
        ;; Spilled: load from frame
        (arm32-ldr buf phys-dst +arm-r11+ (arm32-spill-offset vreg)))))

(defun arm32-store-vreg (buf phys-src vreg)
  "Store physical register PHYS-SRC into virtual register VREG."
  (let ((phys (arm32-phys-reg vreg)))
    (if phys
        (unless (= phys phys-src)
          (arm32-mov buf phys phys-src))
        ;; Spilled: store to frame
        (arm32-str buf phys-src +arm-r11+ (arm32-spill-offset vreg)))))

(defun arm32-resolve-src (buf vreg scratch)
  "Ensure VREG is in a physical register. Returns the physical reg."
  (let ((phys (arm32-phys-reg vreg)))
    (if phys phys
        (progn (arm32-ldr buf scratch +arm-r11+ (arm32-spill-offset vreg))
               scratch))))

;;; ============================================================
;;; Software Division Routine
;;; ============================================================
;;;
;;; ARMv5 has no hardware divide. This emits a small subroutine
;;; at the start of native code. Called via BL for div/mod ops.
;;;
;;; Input:  r0 = dividend (tagged fixnum), r1 = divisor (tagged fixnum)
;;; Output: r0 = quotient (tagged fixnum), r1 = remainder (tagged fixnum)
;;; Clobbers: r2, r3, r12

;;; Second version (correct) follows:

(defun arm32-emit-divmod (buf)
  "Emit the software signed division subroutine.
   Input:  r0 = tagged dividend, r1 = tagged divisor
   Output: r0 = tagged quotient, r1 = tagged remainder
   Clobbers: r2, r3, r12
   Uses the pre-allocated div-label from the buffer if set."
  (let ((div-label (or (arm32-buffer-div-label buf) (mvm-make-label)))
        (shift-label (mvm-make-label))
        (shift-done (mvm-make-label))
        (loop-label (mvm-make-label)))

    (arm32-emit-label buf div-label)

    ;; Untag both: ASR #1
    (arm32-asr-imm buf +arm-r0+ +arm-r0+ 1)
    (arm32-asr-imm buf +arm-r1+ +arm-r1+ 1)

    ;; Save sign: EOR r12, r0, r1 (bit 31 = sign of quotient)
    (arm32-eor buf +arm-r12+ +arm-r0+ +arm-r1+)

    ;; Abs(dividend): CMP r0, #0; RSBLT r0, r0, #0
    (arm32-cmp-imm buf +arm-r0+ 0 0)
    (arm32-dp-imm buf +arm-dp-rsb+ +arm-r0+ +arm-r0+ 0 0 :cond +arm-cc-lt+)

    ;; Abs(divisor): CMP r1, #0; RSBLT r1, r1, #0
    (arm32-cmp-imm buf +arm-r1+ 0 0)
    (arm32-dp-imm buf +arm-dp-rsb+ +arm-r1+ +arm-r1+ 0 0 :cond +arm-cc-lt+)

    ;; r2 = quotient = 0; r3 = shifted divisor
    (arm32-mov-imm buf +arm-r2+ 0 0)
    (arm32-mov buf +arm-r3+ +arm-r1+)

    ;; Shift r3 up until r3 > r0 or overflow
    (arm32-emit-label buf shift-label)
    (arm32-cmp buf +arm-r3+ +arm-r0+)
    (arm32-b-cond buf +arm-cc-hi+ shift-done)
    ;; Check overflow: TST r3, #0x80000000
    (arm32-dp-imm buf +arm-dp-tst+ 0 +arm-r3+ 1 2 :s 1)
    (arm32-b-cond buf +arm-cc-ne+ shift-done)
    (arm32-lsl-imm buf +arm-r3+ +arm-r3+ 1)
    (arm32-b buf shift-label)

    ;; Subtract loop
    (arm32-emit-label buf shift-done)
    (arm32-emit-label buf loop-label)
    (arm32-cmp buf +arm-r0+ +arm-r3+)
    (arm32-dp-reg buf +arm-dp-sub+ +arm-r0+ +arm-r0+ +arm-r3+ :cond +arm-cc-cs+)
    ;; ADC r2, r2, r2 (double quotient, add carry)
    (arm32-emit buf (logior (ash +arm-cc-al+ 28) (ash #b0101 21)
                            (ash +arm-r2+ 16) (ash +arm-r2+ 12) +arm-r2+))
    (arm32-cmp buf +arm-r3+ +arm-r1+)
    (arm32-lsr-imm buf +arm-r3+ +arm-r3+ 1)
    (arm32-b-cond buf +arm-cc-hi+ loop-label)

    ;; Now: r0 = unsigned remainder, r2 = unsigned quotient
    ;; Fix quotient sign: TST r12, #0x80000000; RSBNE r2, r2, #0
    (arm32-dp-imm buf +arm-dp-tst+ 0 +arm-r12+ 1 2 :s 1)
    (arm32-dp-imm buf +arm-dp-rsb+ +arm-r2+ +arm-r2+ 0 0 :cond +arm-cc-ne+)

    ;; Re-tag and arrange: r0 = quotient << 1, r1 = remainder << 1
    (arm32-lsl-imm buf +arm-r1+ +arm-r0+ 1)    ; r1 = remainder << 1 (safe, r0 not yet overwritten)
    (arm32-lsl-imm buf +arm-r0+ +arm-r2+ 1)    ; r0 = quotient << 1

    ;; Return
    (arm32-bx buf +arm-lr+)

    div-label))

;;; ============================================================
;;; Prologue / Epilogue
;;; ============================================================

(defun arm32-emit-prologue (buf)
  "Emit function prologue: save callee-saved regs, set up frame.
   After this, r11 (VFP) = SP - 2 (tagged with object tag 2),
   pointing into the allocated frame area below saved registers.
   obj-ref slot 0 maps to [SP], slot 1 to [SP+4], etc."
  ;; PUSH {r4-r8, r11, lr}
  ;; Register list: bits 4-8, 11, 14
  ;; r9 (alloc ptr) and r10 (alloc limit) are global — must NOT be saved/restored
  (let ((reglist (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)
                         (ash 1 8) (ash 1 11)
                         (ash 1 14))))
    (arm32-push buf reglist))
  ;; SUB sp, sp, #512  (allocate frame)
  ;; 512 = 0x200 = 2 ROR 24 → rotate=12, imm8=2
  (arm32-sub-imm buf +arm-sp+ +arm-sp+ 12 2)
  ;; SUB r11, sp, #2  (VFP = SP - 2, tagged with object tag 2)
  ;; This ensures obj-ref slots access the frame area, not saved registers.
  (arm32-sub-imm buf +arm-r11+ +arm-sp+ 0 2))

(defun arm32-emit-epilogue (buf)
  "Emit function epilogue: restore and return."
  ;; ADD sp, sp, #512  (deallocate frame)
  ;; 512 = 0x200 = 2 ROR 24 → rotate=12, imm8=2
  (arm32-add-imm buf +arm-sp+ +arm-sp+ 12 2)
  ;; POP {r4-r8, r11, pc}  (restore callee-saved, return via PC)
  ;; r9/r10 not restored — they are global alloc ptr/limit
  (let ((reglist (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)
                         (ash 1 8) (ash 1 11)
                         (ash 1 15))))   ; POP to PC = return
    (arm32-pop buf reglist)))

;;; ============================================================
;;; MVM Opcode Translation
;;; ============================================================

(defun arm32-translate-insn (buf opcode operands mvm-pc label-map function-table)
  "Translate a single MVM instruction to ARM32 native code."
  (declare (ignorable mvm-pc function-table))

  ;; Labels are emitted in the main loop at the correct PC position.
  ;; mvm-pc here is new-pc (position after this instruction), used for
  ;; branch offset computation.

  (flet ((vreg (n) (nth n operands))
         (ensure-label (target-pc)
           (or (gethash target-pc label-map)
               (let ((l (mvm-make-label)))
                 (setf (gethash target-pc label-map) l)
                 l))))

    (macrolet ((with-src ((name vreg-expr &optional (scratch '+arm-r12+)) &body body)
                 `(let ((,name (arm32-resolve-src buf ,vreg-expr ,scratch)))
                    ,@body))
               (with-src2 ((n1 v1 s1) (n2 v2 s2) &body body)
                 `(let ((,n1 (arm32-resolve-src buf ,v1 ,s1))
                        (,n2 (arm32-resolve-src buf ,v2 ,s2)))
                    ,@body)))

      (case opcode

        ;;; --- Special ---

        (#.+op-nop+
         (arm32-nop buf))

        (#.+op-break+
         ;; BKPT #0 = 0xE1200070
         (arm32-emit buf #xE1200070))

        (#.+op-trap+
         (let ((code (first operands)))
           (cond
             ((< code #x0100)
              ;; Frame-enter: emit function prologue (push regs, allocate frame)
              (arm32-emit-prologue buf)
              ;; Copy overflow args (params 5+) from caller's stack to frame slots.
              ;; After prologue: PUSH 7 regs (28 bytes) + SUB SP 512.
              ;; Overflow arg k is at [SP + 540 + k*4].
              ;; Compiler accesses param N (N>=4) via obj-ref VFP N → [SP + N*4].
              (when (> code 4)
                (loop for param-idx from 4 below code
                      for k from 0
                      do (let ((src-off (+ 540 (* k 4)))
                               (dst-off (* param-idx 4)))
                           (arm32-ldr buf +arm-r12+ +arm-sp+ src-off)
                           (arm32-str buf +arm-r12+ +arm-sp+ dst-off)))))
             ((< code #x0300)
              nil) ; frame-alloc/frame-free: NOP for now
             ((= code #x0300)
              ;; Serial write: V0 (r0) has tagged fixnum char
              ;; ASR r12, r0, #1  (untag)
              (arm32-asr-imm buf +arm-r12+ +arm-r0+ 1)
              ;; Load UART base into r14 (LR is scratch after prologue)
              ;; ARMv5 versatilepb: 0x101F1000, ARMv7 virt: 0x09000000
              (arm32-load-imm32 buf +arm-lr+ (arm32-uart-base))
              ;; STRB r12, [r14, #0]
              (arm32-strb buf +arm-r12+ +arm-lr+ 0))
             ((= code #x0301)
              ;; Serial read: poll PL011 UART until byte available,
              ;; return tagged fixnum char in r0.
              ;; UARTFR at offset 0x18, RXFE = bit 4 (RX FIFO empty)
              ;; Load UART base into r14
              (arm32-load-imm32 buf +arm-lr+ (arm32-uart-base))
              ;; Poll loop: LDRB r12, [r14, #0x18]  (read UARTFR)
              (let ((poll-label (mvm-make-label)))
                (arm32-emit-label buf poll-label)
                (arm32-ldrb buf +arm-r12+ +arm-lr+ #x18)
                ;; TST r12, #0x10  (test RXFE bit 4)
                (arm32-dp-imm buf +arm-dp-tst+ 0 +arm-r12+ 0 #x10 :s 1)
                ;; BNE poll-label  (loop while RXFE set)
                (arm32-b-cond buf +arm-cc-ne+ poll-label))
              ;; Read data byte: LDRB r0, [r14, #0]
              (arm32-ldrb buf +arm-r0+ +arm-lr+ 0)
              ;; Tag as fixnum: LSL r0, r0, #1
              (arm32-lsl-imm buf +arm-r0+ +arm-r0+ 1))
             ((= code #x0304)
              ;; WFI: Wait For Interrupt
              ;; ARM encoding: 0xE320F003 (cond=AL, WFI hint)
              ;; Wakes on pending interrupt even with CPSR I-bit set
              (arm32-emit buf #xE320F003))
             ((= code #x0320)
              ;; SETUP-IRQ: init ARM virtual timer on ARMv7
              ;; CNTVTVAL = 62500 (1ms at 62.5MHz), CNTVCTL = 1 (enable)
              (when *arm32-v7*
                (arm32-movw buf +arm-r0+ #xF424)      ; r0 = 62500
                (arm32-emit buf #xEE0E0F13)            ; MCR p15, 0, r0, c14, c3, 0
                (arm32-movw buf +arm-r0+ 1)            ; r0 = 1
                (arm32-emit buf #xEE0E0F33)))          ; MCR p15, 0, r0, c14, c3, 1
             ((= code #x0321)
              ;; TIMER-REARM: re-arm virtual timer on ARMv7
              (when *arm32-v7*
                (arm32-movw buf +arm-r0+ #xF424)      ; r0 = 62500
                (arm32-emit buf #xEE0E0F13)))
             (t
              ;; Real trap: SWI
              (arm32-swi buf code)))))

        ;;; --- Data Movement ---

        (#.+op-mov+
         (let ((vd (vreg 0))
               (vs (vreg 1)))
           (let ((pd (arm32-phys-reg vd))
                 (ps (arm32-phys-reg vs)))
             (cond
               ((and pd ps)
                (unless (= pd ps)
                  (arm32-mov buf pd ps)))
               ((and pd (not ps))
                ;; Source spilled, dest in reg
                (arm32-ldr buf pd +arm-r11+ (arm32-spill-offset vs)))
               ((and (not pd) ps)
                ;; Source in reg, dest spilled
                (arm32-str buf ps +arm-r11+ (arm32-spill-offset vd)))
               (t
                ;; Both spilled
                (arm32-ldr buf +arm-r12+ +arm-r11+ (arm32-spill-offset vs))
                (arm32-str buf +arm-r12+ +arm-r11+ (arm32-spill-offset vd)))))))

        (#.+op-li+
         (let ((vd (vreg 0))
               (imm (vreg 1)))
           (let ((pd (arm32-phys-reg vd)))
             (if pd
                 (arm32-load-imm32 buf pd (logand imm #xFFFFFFFF))
                 (progn
                   (arm32-load-imm32 buf +arm-r12+ (logand imm #xFFFFFFFF))
                   (arm32-str buf +arm-r12+ +arm-r11+ (arm32-spill-offset vd)))))))

        (#.+op-push+
         (with-src (ps (vreg 0))
           ;; STR ps, [sp, #-4]!
           (arm32-str-pre buf ps +arm-sp+ -4)))

        (#.+op-pop+
         (let* ((vd (vreg 0))
                (pd (arm32-phys-reg vd)))
           (if pd
               ;; LDR pd, [sp], #4
               (arm32-ldr-post buf pd +arm-sp+ 4)
               (progn
                 (arm32-ldr-post buf +arm-r12+ +arm-sp+ 4)
                 (arm32-str buf +arm-r12+ +arm-r11+ (arm32-spill-offset vd))))))

        ;;; --- Arithmetic ---

        (#.+op-add+
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (pb (vreg 2) +arm-lr+)
             (arm32-add buf +arm-r12+ pa pb)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-sub+
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (pb (vreg 2) +arm-lr+)
             (arm32-sub buf +arm-r12+ pa pb)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-mul+
         ;; Tagged: (a<<1) * (b<<1) = (a*b)<<2, need (a*b)<<1
         ;; So: untag one, multiply, result is correctly tagged
         ;; ASR r12, Pa, #1 ; MUL rd, r12, Pb
         ;; Note: MUL Rd != Rm constraint on ARMv5 — use r12 as Rm, Pb as Rs
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (pb (vreg 2) +arm-lr+)
             (arm32-asr-imm buf +arm-r12+ pa 1)  ; untag first operand
             ;; MUL dest, r12, pb — dest must != r12
             ;; Use LR as dest if vd maps to r12 (unlikely but safe)
             (let ((dest (or (arm32-phys-reg vd) +arm-lr+)))
               (if (= dest +arm-r12+)
                   (progn
                     (arm32-mul buf +arm-lr+ +arm-r12+ pb)
                     (arm32-store-vreg buf +arm-lr+ vd))
                   (progn
                     (arm32-mul buf dest +arm-r12+ pb)
                     (arm32-store-vreg buf dest vd)))))))

        (#.+op-div+
         (let ((vd (vreg 0))
               (va-reg (vreg 1))
               (vb-reg (vreg 2)))
           (arm32-load-vreg buf +arm-r0+ va-reg)
           (arm32-load-vreg buf +arm-r1+ vb-reg)
           (if *arm32-v7*
               ;; ARMv7: hardware SDIV
               (progn
                 (arm32-asr-imm buf +arm-r0+ +arm-r0+ 1)   ; untag dividend
                 (arm32-asr-imm buf +arm-r1+ +arm-r1+ 1)   ; untag divisor
                 (arm32-sdiv buf +arm-r12+ +arm-r0+ +arm-r1+)  ; r12 = r0 / r1
                 (arm32-lsl-imm buf +arm-r12+ +arm-r12+ 1)    ; re-tag
                 (arm32-store-vreg buf +arm-r12+ vd))
               ;; ARMv5: software divide subroutine
               (progn
                 (arm32-bl buf (arm32-buffer-div-label buf))
                 (arm32-store-vreg buf +arm-r0+ vd)))))

        (#.+op-mod+
         (let ((vd (vreg 0))
               (va-reg (vreg 1))
               (vb-reg (vreg 2)))
           (arm32-load-vreg buf +arm-r0+ va-reg)
           (arm32-load-vreg buf +arm-r1+ vb-reg)
           (if *arm32-v7*
               ;; ARMv7: remainder = dividend - (dividend/divisor)*divisor
               (progn
                 (arm32-asr-imm buf +arm-r0+ +arm-r0+ 1)   ; untag dividend
                 (arm32-asr-imm buf +arm-r1+ +arm-r1+ 1)   ; untag divisor
                 (arm32-sdiv buf +arm-r12+ +arm-r0+ +arm-r1+)  ; r12 = quotient
                 (arm32-mul buf +arm-lr+ +arm-r12+ +arm-r1+)   ; lr = quot * divisor
                 (arm32-sub buf +arm-r12+ +arm-r0+ +arm-lr+)   ; r12 = dividend - lr
                 (arm32-lsl-imm buf +arm-r12+ +arm-r12+ 1)    ; re-tag
                 (arm32-store-vreg buf +arm-r12+ vd))
               ;; ARMv5: software divide subroutine
               (progn
                 (arm32-bl buf (arm32-buffer-div-label buf))
                 (arm32-store-vreg buf +arm-r1+ vd)))))

        (#.+op-neg+
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             ;; RSB rd, ps, #0  (rd = 0 - ps)
             (arm32-rsb-imm buf +arm-r12+ ps 0 0)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-inc+
         (let* ((vd (vreg 0))
                (pd (arm32-phys-reg vd)))
           (if pd
               ;; ADD pd, pd, #2  (tagged fixnum 1 = 2)
               (arm32-add-imm buf pd pd 0 2)
               (progn
                 (arm32-ldr buf +arm-r12+ +arm-r11+ (arm32-spill-offset vd))
                 (arm32-add-imm buf +arm-r12+ +arm-r12+ 0 2)
                 (arm32-str buf +arm-r12+ +arm-r11+ (arm32-spill-offset vd))))))

        (#.+op-dec+
         (let* ((vd (vreg 0))
                (pd (arm32-phys-reg vd)))
           (if pd
               (arm32-sub-imm buf pd pd 0 2)
               (progn
                 (arm32-ldr buf +arm-r12+ +arm-r11+ (arm32-spill-offset vd))
                 (arm32-sub-imm buf +arm-r12+ +arm-r12+ 0 2)
                 (arm32-str buf +arm-r12+ +arm-r11+ (arm32-spill-offset vd))))))

        ;;; --- Bitwise ---

        (#.+op-and+
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (pb (vreg 2) +arm-lr+)
             (arm32-and-r buf +arm-r12+ pa pb)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-or+
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (pb (vreg 2) +arm-lr+)
             (arm32-orr buf +arm-r12+ pa pb)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-xor+
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (pb (vreg 2) +arm-lr+)
             (arm32-eor buf +arm-r12+ pa pb)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-shl+
         (let ((vd (vreg 0))
               (amount (vreg 2)))
           (with-src (ps (vreg 1))
             (arm32-lsl-imm buf +arm-r12+ ps (logand amount 31))
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-shr+
         (let ((vd (vreg 0))
               (amount (vreg 2)))
           (with-src (ps (vreg 1))
             (arm32-lsr-imm buf +arm-r12+ ps (logand amount 31))
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-sar+
         (let ((vd (vreg 0))
               (amount (vreg 2)))
           (with-src (ps (vreg 1))
             (arm32-asr-imm buf +arm-r12+ ps (logand amount 31))
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-shlv+
         ;; (shlv Vd Vs Vc) — shift left by register
         (let ((vd (vreg 0)))
           (with-src2 (ps (vreg 1) +arm-r12+) (pc (vreg 2) +arm-lr+)
             (arm32-lsl-reg buf +arm-r12+ ps pc)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-sarv+
         ;; (sarv Vd Vs Vc) — arithmetic shift right by register
         (let ((vd (vreg 0)))
           (with-src2 (ps (vreg 1) +arm-r12+) (pc (vreg 2) +arm-lr+)
             (arm32-asr-reg buf +arm-r12+ ps pc)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-ldb+
         ;; Extract bit field: (LDB (BYTE size pos) src)
         (let ((vd (vreg 0))
               (pos (vreg 2))
               (size (vreg 3)))
           (with-src (ps (vreg 1))
             ;; LSR r12, ps, #pos
             (arm32-lsr-imm buf +arm-r12+ ps (logand pos 31))
             ;; AND r12, r12, #((1<<size)-1)
             (let ((mask (1- (ash 1 size))))
               (let ((enc (arm32-encode-imm mask)))
                 (if enc
                     (let ((rot (car enc))
                           (imm (cdr enc)))
                       (arm32-and-imm buf +arm-r12+ +arm-r12+ rot imm))
                     (progn
                       (arm32-load-imm32 buf +arm-lr+ mask)
                       (arm32-and-r buf +arm-r12+ +arm-r12+ +arm-lr+)))))
             (arm32-store-vreg buf +arm-r12+ vd))))

        ;;; --- Comparison ---

        (#.+op-cmp+
         (with-src2 (pa (vreg 0) +arm-r12+) (pb (vreg 1) +arm-lr+)
           (arm32-cmp buf pa pb)))

        (#.+op-test+
         (with-src2 (pa (vreg 0) +arm-r12+) (pb (vreg 1) +arm-lr+)
           (arm32-tst buf pa pb)))

        ;;; --- Branches ---

        (#.+op-br+
         (let* ((off (vreg 0))
                (target-pc (+ mvm-pc off))
                (label (ensure-label target-pc)))
           (arm32-b buf label)))

        (#.+op-beq+
         (let* ((off (vreg 0))
                (target-pc (+ mvm-pc off))
                (label (ensure-label target-pc)))
           (arm32-b-cond buf +arm-cc-eq+ label)))

        (#.+op-bne+
         (let* ((off (vreg 0))
                (target-pc (+ mvm-pc off))
                (label (ensure-label target-pc)))
           (arm32-b-cond buf +arm-cc-ne+ label)))

        (#.+op-blt+
         (let* ((off (vreg 0))
                (target-pc (+ mvm-pc off))
                (label (ensure-label target-pc)))
           (arm32-b-cond buf +arm-cc-lt+ label)))

        (#.+op-bge+
         (let* ((off (vreg 0))
                (target-pc (+ mvm-pc off))
                (label (ensure-label target-pc)))
           (arm32-b-cond buf +arm-cc-ge+ label)))

        (#.+op-ble+
         (let* ((off (vreg 0))
                (target-pc (+ mvm-pc off))
                (label (ensure-label target-pc)))
           (arm32-b-cond buf +arm-cc-le+ label)))

        (#.+op-bgt+
         (let* ((off (vreg 0))
                (target-pc (+ mvm-pc off))
                (label (ensure-label target-pc)))
           (arm32-b-cond buf +arm-cc-gt+ label)))

        (#.+op-bnull+
         (with-src (ps (vreg 0))
           (arm32-cmp buf ps +arm-r8+)  ; compare with VN (NIL in r8)
           (let* ((off (vreg 1))
                  (target-pc (+ mvm-pc off))
                  (label (ensure-label target-pc)))
             (arm32-b-cond buf +arm-cc-eq+ label))))

        (#.+op-bnnull+
         (with-src (ps (vreg 0))
           (arm32-cmp buf ps +arm-r8+)
           (let* ((off (vreg 1))
                  (target-pc (+ mvm-pc off))
                  (label (ensure-label target-pc)))
             (arm32-b-cond buf +arm-cc-ne+ label))))

        ;;; --- List Operations ---

        (#.+op-car+
         ;; Cons cells tagged with low bit 1. CAR is first word.
         ;; LDR Rd, [Ps, #-1]  (untag: subtract 1)
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             (arm32-ldr buf +arm-r12+ ps -1)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-cdr+
         ;; CDR is second word: offset = -1 + 4 = 3
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             (arm32-ldr buf +arm-r12+ ps 3)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-cons+
         ;; Allocate cons cell from alloc pointer (r9=VA)
         ;; STR car, [r9, #0]; STR cdr, [r9, #4]; ORR Rd, r9, #1; ADD r9, r9, #8
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (pb (vreg 2) +arm-lr+)
             (arm32-str buf pa +arm-r9+ 0)        ; store car
             (arm32-str buf pb +arm-r9+ 4)         ; store cdr
             ;; Tag: ORR Rd, r9, #1
             (arm32-orr-imm buf +arm-r12+ +arm-r9+ 0 1)
             (arm32-store-vreg buf +arm-r12+ vd)
             ;; Bump alloc pointer: ADD r9, r9, #16
             ;; 16-byte alignment required: 4-bit tag uses low nibble.
             ;; Cons is 8 bytes (2 words) but we need 16-byte stride so
             ;; low 4 bits of alloc pointer stay 0x0 (not 0x8).
             (arm32-add-imm buf +arm-r9+ +arm-r9+ 0 16))))

        (#.+op-setcar+
         ;; STR Vs, [Vd, #-1]
         (with-src2 (pd (vreg 0) +arm-r12+) (ps (vreg 1) +arm-lr+)
           (arm32-str buf ps pd -1)))

        (#.+op-setcdr+
         ;; STR Vs, [Vd, #3]
         (with-src2 (pd (vreg 0) +arm-r12+) (ps (vreg 1) +arm-lr+)
           (arm32-str buf ps pd 3)))

        (#.+op-consp+
         ;; Check low 4 bits == 1 (cons tag)
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             ;; AND r12, ps, #0xF
             (arm32-and-imm buf +arm-r12+ ps 0 #xF)
             ;; CMP r12, #1
             (arm32-cmp-imm buf +arm-r12+ 0 1)
             ;; MOVEQ rd, #2 (tagged true = fixnum 1 = 2)
             (arm32-mov-imm-cond buf +arm-cc-eq+ +arm-r12+ 0 2)
             ;; MOVNE rd, r8 (NIL)
             (arm32-mov-cond buf +arm-cc-ne+ +arm-r12+ +arm-r8+)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-atom+
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             (arm32-and-imm buf +arm-r12+ ps 0 #xF)
             (arm32-cmp-imm buf +arm-r12+ 0 1)
             ;; MOVNE rd, #2 (true if NOT cons)
             (arm32-mov-imm-cond buf +arm-cc-ne+ +arm-r12+ 0 2)
             ;; MOVEQ rd, r8 (NIL if cons)
             (arm32-mov-cond buf +arm-cc-eq+ +arm-r12+ +arm-r8+)
             (arm32-store-vreg buf +arm-r12+ vd))))

        ;;; --- Object Operations ---

        (#.+op-alloc-obj+
         ;; (alloc-obj Vd size subtag)
         ;; Header word: (size << 8) | subtag, stored at VA
         ;; Object pointer: VA + 4 (skip header), tagged with object tag
         (let ((vd (vreg 0))
               (size (vreg 1))
               (subtag (vreg 2)))
           ;; Build header in r12: (size << 8) | subtag
           (arm32-load-imm32 buf +arm-r12+ (logior (ash size 8) subtag))
           ;; STR r12, [r9, #0]  (store header at alloc ptr)
           (arm32-str buf +arm-r12+ +arm-r9+ 0)
           ;; Object tag = 2; pointer = alloc | tag (like i386 uses alloc | 0x09)
           ;; Header at alloc, slots at alloc+4+. OBJ-REF offset = 2 + idx*4.
           (arm32-orr-imm buf +arm-r12+ +arm-r9+ 0 2)
           (arm32-store-vreg buf +arm-r12+ vd)
           ;; Bump alloc: total = (1 + size) * 4 bytes (header + slots)
           ;; Align to 16 bytes to keep cons alloc pointer aligned
           (let ((total (logand (+ (* (1+ size) 4) 15) (lognot 15))))
             (let ((enc (arm32-encode-imm total)))
               (if enc
                   (let ((rot (car enc))
                         (imm (cdr enc)))
                     (arm32-add-imm buf +arm-r9+ +arm-r9+ rot imm))
                   (progn
                     (arm32-load-imm32 buf +arm-lr+ total)
                     (arm32-add buf +arm-r9+ +arm-r9+ +arm-lr+)))))))

        (#.+op-alloc-array+
         ;; (alloc-array Vd Vcount) — dynamic array allocation
         ;; Vcount: UNTAGGED element count (compiler SAR'd it already)
         ;; 32-bit: header = (count << 8) | 0x32, 4 bytes
         ;; Allocates (count+1)*4 bytes, aligned to 16
         ;; Result = VA | 2 (object tag)
         (let ((vd (vreg 0))
               (vcount (vreg 1)))
           ;; Load count into R12
           (arm32-load-vreg buf +arm-r12+ vcount)
           ;; Save count in LR (can't push — frame is set)
           (arm32-mov buf +arm-lr+ +arm-r12+)
           ;; Build header: R12 = (count << 8) | 0x32
           (arm32-lsl-imm buf +arm-r12+ +arm-r12+ 8)
           (arm32-orr-imm buf +arm-r12+ +arm-r12+ 0 #x32)
           ;; Store header at [R9] (alloc pointer)
           (arm32-str buf +arm-r12+ +arm-r9+ 0)
           ;; Result: R12 = R9 | 2 (object tag)
           (arm32-orr-imm buf +arm-r12+ +arm-r9+ 0 2)
           (arm32-store-vreg buf +arm-r12+ vd)
           ;; Compute alloc size from saved count in LR
           ;; total = (count + 1) * 4, aligned to 16
           (arm32-add-imm buf +arm-lr+ +arm-lr+ 0 1)   ; count + 1
           (arm32-lsl-imm buf +arm-lr+ +arm-lr+ 2)     ; * 4
           (arm32-add-imm buf +arm-lr+ +arm-lr+ 0 15)  ; + 15
           (arm32-bic-imm buf +arm-lr+ +arm-lr+ 0 15)  ; & ~15 (align to 16)
           ;; Bump alloc pointer: R9 += LR
           (arm32-add buf +arm-r9+ +arm-r9+ +arm-lr+)))

        (#.+op-obj-ref+
         ;; (obj-ref Vd Vobj idx)
         ;; Object tagged with 2 at low bits. Slot at (ptr - 2) + 4 + idx*4
         ;; Simplify: offset = -2 + 4 + idx*4 = 2 + idx*4
         (let ((vd (vreg 0))
               (idx (vreg 2)))
           (with-src (ps (vreg 1))
             (let ((off (+ 2 (* idx 4))))
               (arm32-ldr buf +arm-r12+ ps off)
               (arm32-store-vreg buf +arm-r12+ vd)))))

        (#.+op-obj-set+
         ;; (obj-set Vobj idx Vs)
         (let ((idx (vreg 1)))
           (with-src2 (pobj (vreg 0) +arm-r12+) (ps (vreg 2) +arm-lr+)
             (let ((off (+ 2 (* idx 4))))
               (arm32-str buf ps pobj off)))))

        (#.+op-aref+
         ;; (aref Vd Vobj Vidx) — variable-index array load (tagged word)
         ;; Address = Vobj + Vidx*2 + 2 (matches OBJ-REF offset formula)
         (let ((vd (vreg 0)))
           (with-src2 (pidx (vreg 2) +arm-r12+) (pobj (vreg 1) +arm-lr+)
             ;; R12 = Vidx * 2 (tagged idx → slot byte offset)
             (arm32-lsl-imm buf +arm-r12+ pidx 1)
             ;; R12 = R12 + Vobj
             (arm32-add buf +arm-r12+ +arm-r12+ pobj)
             ;; LDR R12, [R12, #2]
             (arm32-ldr buf +arm-r12+ +arm-r12+ 2)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-aset+
         ;; (aset Vobj Vidx Vs) — variable-index array store (tagged word)
         ;; Address = Vobj + Vidx*2 + 2 (matches OBJ-SET offset formula)
         (with-src (pval (vreg 2) +arm-lr+)
           ;; Load Vidx into R12
           (arm32-load-vreg buf +arm-r12+ (vreg 1))
           ;; R12 = Vidx * 2
           (arm32-lsl-imm buf +arm-r12+ +arm-r12+ 1)
           ;; Save value on stack
           (arm32-str-pre buf pval +arm-sp+ -4)
           ;; Load Vobj into LR
           (arm32-load-vreg buf +arm-lr+ (vreg 0))
           ;; R12 = R12 + Vobj
           (arm32-add buf +arm-r12+ +arm-r12+ +arm-lr+)
           ;; Restore value from stack
           (arm32-ldr-post buf +arm-lr+ +arm-sp+ 4)
           ;; STR value, [R12, #2]
           (arm32-str buf +arm-lr+ +arm-r12+ 2)))

        (#.+op-array-len+
         ;; (array-len Vd Vobj) — extract element count from header
         ;; Header at Vobj - 2 (object tag = 2)
         ;; Header format: [subtag_half:8][count:24] — count = header >> 8
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             ;; SUB R12, Vobj, #2 (strip object tag)
             (arm32-sub-imm buf +arm-r12+ ps 0 2)
             ;; LDR R12, [R12] (load header word)
             (arm32-ldr buf +arm-r12+ +arm-r12+ 0)
             ;; LSR R12, R12, #8 (count = header >> 8)
             (arm32-lsr-imm buf +arm-r12+ +arm-r12+ 8)
             ;; BIC R12, R12, #0xFF000000 (mask to 24 bits — clear top byte)
             (arm32-bic-imm buf +arm-r12+ +arm-r12+ 4 #xFF)
             ;; LSL R12, R12, #1 (tag as fixnum)
             (arm32-lsl-imm buf +arm-r12+ +arm-r12+ 1)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-obj-tag+
         ;; Extract low 4 bits
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             (arm32-and-imm buf +arm-r12+ ps 0 #xF)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-obj-subtag+
         ;; Load header word, extract low byte
         (let ((vd (vreg 0)))
           (with-src (ps (vreg 1))
             ;; Untag: SUB r12, ps, #2  (object tag = 2)
             (arm32-sub-imm buf +arm-r12+ ps 0 2)
             ;; LDR r12, [r12]  (load header)
             (arm32-ldr buf +arm-r12+ +arm-r12+ 0)
             ;; AND r12, r12, #0xFF
             (arm32-and-imm buf +arm-r12+ +arm-r12+ 0 #xFF)
             (arm32-store-vreg buf +arm-r12+ vd))))

        ;;; --- Memory (raw) ---

        (#.+op-load+
         ;; (load Vd Vaddr width)
         (let ((vd (vreg 0))
               (width (vreg 2)))
           (with-src (pa (vreg 1))
             (ecase width
               (0 (arm32-ldrb buf +arm-r12+ pa 0))   ; byte
               (1 (arm32-ldrh buf +arm-r12+ pa 0))    ; halfword
               (2 (arm32-ldr buf +arm-r12+ pa 0))     ; word
               (3 (arm32-ldr buf +arm-r12+ pa 0)))    ; word (32-bit arch)
             (arm32-store-vreg buf +arm-r12+ vd))))

        (#.+op-store+
         ;; (store Vaddr Vs width)
         (let ((width (vreg 2)))
           (with-src2 (pa (vreg 0) +arm-r12+) (ps (vreg 1) +arm-lr+)
             (ecase width
               (0 (arm32-strb buf ps pa 0))
               (1 (arm32-strh buf ps pa 0))
               (2 (arm32-str buf ps pa 0))
               (3 (arm32-str buf ps pa 0))))))

        (#.+op-fence+
         ;; MCR p15, 0, r0, c7, c10, 4  (drain write buffer on ARMv5)
         (arm32-fence buf))

        ;;; --- Function Calling ---

        (#.+op-call+
         ;; (call target:imm32) - operand is bytecode offset of target function
         (let* ((target-pc (vreg 0))
                (label (ensure-label target-pc)))
           (arm32-bl buf label)))

        (#.+op-call-ind+
         ;; Indirect call: MOV LR, PC; BX Ps
         ;; MOV LR, PC captures PC+8, which is the instruction after BX
         (with-src (ps (vreg 0))
           ;; MOV lr, pc
           (arm32-mov buf +arm-lr+ +arm-pc+)
           ;; BX ps
           (arm32-bx buf ps)))

        (#.+op-ret+
         (arm32-emit-epilogue buf))

        (#.+op-tailcall+
         ;; (tailcall target:imm32) - operand is bytecode offset
         ;; Restore frame, then branch (not link)
         (let ((target-pc (vreg 0)))
           ;; Deallocate frame: ADD SP, SP, #512
           (arm32-add-imm buf +arm-sp+ +arm-sp+ 12 2)
           ;; POP {r4-r8, r11, lr}  (restore callee-saved but DON'T return)
           ;; r9/r10 not restored — global alloc ptr/limit
           (let ((reglist (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)
                                  (ash 1 8) (ash 1 11)
                                  (ash 1 14))))
             (arm32-pop buf reglist))
           ;; B target
           (arm32-b buf (ensure-label target-pc))))

        ;;; --- GC / Allocation ---

        (#.+op-alloc-cons+
         ;; (alloc-cons Vd) — allocate cons cell without filling
         ;; 16-byte alignment for 4-bit tag (same as CONS)
         (let ((vd (vreg 0)))
           ;; Tag: ORR Rd, r9, #1
           (arm32-orr-imm buf +arm-r12+ +arm-r9+ 0 1)
           (arm32-store-vreg buf +arm-r12+ vd)
           ;; Bump: ADD r9, r9, #16
           (arm32-add-imm buf +arm-r9+ +arm-r9+ 0 16)))

        (#.+op-gc-check+
         ;; CMP r9(VA), r10(VL); trap if VA >= VL
         (arm32-cmp buf +arm-r9+ +arm-r10+)
         ;; BKPTCS #1  (if unsigned >=)
         ;; Actually ARM BKPTcc doesn't exist; use: BLO .ok; BKPT #1; .ok:
         (let ((ok-label (mvm-make-label)))
           (arm32-b-cond buf +arm-cc-cc+ ok-label)  ; branch if VA < VL (unsigned)
           (arm32-emit buf #xE1200071)               ; BKPT #1
           (arm32-emit-label buf ok-label)))

        (#.+op-write-barrier+
         ;; Simplified: NOP on bare-metal
         (arm32-nop buf))

        ;;; --- Actor / Concurrency ---

        (#.+op-save-ctx+
         ;; Save arg regs to frame
         (arm32-str buf +arm-r0+ +arm-r11+ -4)
         (arm32-str buf +arm-r1+ +arm-r11+ -8)
         (arm32-str buf +arm-r2+ +arm-r11+ -12)
         (arm32-str buf +arm-r3+ +arm-r11+ -16))

        (#.+op-restore-ctx+
         ;; Restore arg regs from frame
         (arm32-ldr buf +arm-r0+ +arm-r11+ -4)
         (arm32-ldr buf +arm-r1+ +arm-r11+ -8)
         (arm32-ldr buf +arm-r2+ +arm-r11+ -12)
         (arm32-ldr buf +arm-r3+ +arm-r11+ -16))

        (#.+op-yield+
         ;; NOP on all ARM32 variants. WFE halts the CPU on QEMU raspi2b
         ;; with no wake event, causing 0% CPU hang in polling loops.
         (arm32-nop buf))

        (#.+op-atomic-xchg+
         (let ((vd (vreg 0)))
           (with-src2 (pa (vreg 1) +arm-r12+) (ps (vreg 2) +arm-lr+)
             (if *arm32-v7*
                 ;; ARMv7: LDREX/STREX loop (proper exclusive monitor)
                 (let ((loop-label (mvm-make-label)))
                   (arm32-emit-label buf loop-label)
                   (arm32-ldrex buf +arm-r12+ pa)             ; r12 = old value at [pa]
                   (arm32-strex buf +arm-r3+ ps pa)           ; try store, r3 = status
                   (arm32-cmp-imm buf +arm-r3+ 0 0)          ; success?
                   (arm32-b-cond buf +arm-cc-ne+ loop-label)  ; retry if failed
                   (arm32-store-vreg buf +arm-r12+ vd))
                 ;; ARMv5: SWP (atomic swap)
                 (progn
                   (arm32-swp buf +arm-r12+ ps pa)
                   (arm32-store-vreg buf +arm-r12+ vd))))))

        ;;; --- System / Platform ---

        (#.+op-io-read+
         ;; (io-read Vd port width)
         (let ((vd (vreg 0))
               (port (vreg 1))
               (width (vreg 2)))
           (arm32-load-imm32 buf +arm-r12+ port)
           (ecase width
             (0 (arm32-ldrb buf +arm-lr+ +arm-r12+ 0))
             (1 (arm32-ldrh buf +arm-lr+ +arm-r12+ 0))
             (2 (arm32-ldr buf +arm-lr+ +arm-r12+ 0))
             (3 (arm32-ldr buf +arm-lr+ +arm-r12+ 0)))
           (arm32-store-vreg buf +arm-lr+ vd)))

        (#.+op-io-write+
         ;; (io-write port Vs width)
         (let ((port (vreg 0))
               (width (vreg 2)))
           (with-src (ps (vreg 1) +arm-lr+)
             (arm32-load-imm32 buf +arm-r12+ port)
             (ecase width
               (0 (arm32-strb buf ps +arm-r12+ 0))
               (1 (arm32-strh buf ps +arm-r12+ 0))
               (2 (arm32-str buf ps +arm-r12+ 0))
               (3 (arm32-str buf ps +arm-r12+ 0))))))

        (#.+op-halt+
         ;; B . (infinite loop)
         ;; ARM32 B with offset -2 instruction units = branch to self
         ;; offset field = 0xFFFFFE (signed -2)
         (arm32-emit buf (logior (ash +arm-cc-al+ 28)
                                 (ash #b101 25)
                                 (logand -2 #xFFFFFF))))

        (#.+op-cli+
         ;; MRS r12, CPSR; ORR r12, r12, #0xC0; MSR CPSR_c, r12
         (arm32-mrs buf +arm-r12+)
         (arm32-orr-imm buf +arm-r12+ +arm-r12+ 0 #xC0)
         (arm32-msr-c buf +arm-r12+))

        (#.+op-sti+
         ;; MRS r12, CPSR; BIC r12, r12, #0xC0; MSR CPSR_c, r12
         (arm32-mrs buf +arm-r12+)
         (arm32-bic-imm buf +arm-r12+ +arm-r12+ 0 #xC0)
         (arm32-msr-c buf +arm-r12+))

        (#.+op-percpu-ref+
         ;; No TPIDR on ARMv5. Use fixed location approach.
         ;; For now: LDR from a fixed base address
         (let ((vd (vreg 0))
               (offset (vreg 1)))
           ;; Use zero-based per-CPU area (single-core on versatilepb)
           (arm32-load-imm32 buf +arm-r12+ offset)
           (arm32-ldr buf +arm-r12+ +arm-r12+ 0)
           (arm32-store-vreg buf +arm-r12+ vd)))

        (#.+op-percpu-set+
         (let ((offset (vreg 0)))
           (with-src (ps (vreg 1) +arm-lr+)
             (arm32-load-imm32 buf +arm-r12+ offset)
             (arm32-str buf ps +arm-r12+ 0))))

        (#.+op-mul26lo+
         ;; (mul26lo Vd Va Vb) — low 26 bits of untag(Va)*untag(Vb), tagged
         ;; Uses UMULL for 32x32→64 unsigned multiply, extracts low 26 bits.
         ;; Needs 3 scratch regs: r12, lr, r4 (saved/restored via PUSH/POP).
         (let ((vd (vreg 0))
               (va (vreg 1))
               (vb (vreg 2)))
           ;; Save r4 (may hold V4)
           (arm32-push buf (ash 1 4))   ; PUSH {r4}
           ;; Load Va → r12, Vb → r4
           (arm32-load-vreg buf +arm-r12+ va)
           (arm32-load-vreg buf 4 vb)   ; r4
           ;; Untag both
           (arm32-asr-imm buf +arm-r12+ +arm-r12+ 1)
           (arm32-asr-imm buf 4 4 1)    ; ASR r4, r4, #1
           ;; UMULL lr, r12, r4, r12 → r12:lr = r4 * r12
           ;; Constraints: RdLo(lr)!=Rm(r4) ✓, RdHi(r12)!=Rm(r4) ✓
           (arm32-umull buf +arm-lr+ +arm-r12+ 4 +arm-r12+)
           ;; Extract low 26 bits: BIC lr, lr, #0xFC000000
           ;; 0xFC000000 = 0xFC ROR 8, rotate=4, imm8=0xFC
           (arm32-bic-imm buf +arm-lr+ +arm-lr+ 4 #xFC)
           ;; Re-tag: LSL r12, lr, #1
           (arm32-lsl-imm buf +arm-r12+ +arm-lr+ 1)
           ;; Restore r4
           (arm32-pop buf (ash 1 4))    ; POP {r4}
           ;; Store result
           (arm32-store-vreg buf +arm-r12+ vd)))

        (#.+op-mul26hi+
         ;; (mul26hi Vd Va Vb) — bits 26+ of untag(Va)*untag(Vb), tagged
         ;; UMULL then extract (RdHi << 6) | (RdLo >> 26).
         (let ((vd (vreg 0))
               (va (vreg 1))
               (vb (vreg 2)))
           ;; Save r4
           (arm32-push buf (ash 1 4))   ; PUSH {r4}
           ;; Load Va → r12, Vb → r4
           (arm32-load-vreg buf +arm-r12+ va)
           (arm32-load-vreg buf 4 vb)   ; r4
           ;; Untag both
           (arm32-asr-imm buf +arm-r12+ +arm-r12+ 1)
           (arm32-asr-imm buf 4 4 1)    ; ASR r4, r4, #1
           ;; UMULL lr, r12, r4, r12 → r12:lr = r4 * r12
           (arm32-umull buf +arm-lr+ +arm-r12+ 4 +arm-r12+)
           ;; Extract bits 26+: (r12 << 6) | (lr >> 26)
           (arm32-lsr-imm buf +arm-lr+ +arm-lr+ 26)
           ;; ORR lr, lr, r12, LSL #6
           (arm32-dp-reg buf +arm-dp-orr+ +arm-lr+ +arm-lr+ +arm-r12+
                         :shift-type +arm-shift-lsl+ :shift-amt 6)
           ;; Re-tag: LSL r12, lr, #1
           (arm32-lsl-imm buf +arm-r12+ +arm-lr+ 1)
           ;; Restore r4
           (arm32-pop buf (ash 1 4))    ; POP {r4}
           ;; Store result
           (arm32-store-vreg buf +arm-r12+ vd)))

        (otherwise
         ;; Unknown opcode: emit NOP
         (arm32-nop buf))))))

;;; ============================================================
;;; Main Translation Entry Point
;;; ============================================================

(defun translate-mvm-to-arm32 (bytecode function-table &key (v7 nil))
  "Translate MVM bytecode to ARM32 native code.
   When V7 is T, emit ARMv7-A instructions (SDIV, MOVW/MOVT, DMB, LDREX/STREX).
   Returns an arm32-buffer."
  (let* ((*arm32-v7* v7)
         (buf (make-arm32-buffer))
         (label-map (make-hash-table :test 'eql))
         (bc bytecode)
         (len (length bc))
         (pc 0))

    ;; Pre-allocate divmod label (code emitted at end, after all functions)
    (unless *arm32-v7*
      (setf (arm32-buffer-div-label buf) (mvm-make-label)))

    ;; Emit prologue for the main entry
    (arm32-emit-prologue buf)

    ;; First pass: scan for branch targets
    (loop while (< pc len)
          do (let* ((decoded (decode-instruction bc pc))
                    (opcode (car decoded))
                    (operands (cadr decoded))
                    (new-pc (cddr decoded)))
               (let ((info (gethash opcode *opcode-table*)))
                 (when info
                   (let ((op-types (opcode-info-operands info)))
                     (cond
                       ((and (member :off32 op-types)
                             (not (member :reg op-types)))
                        (let* ((off (first operands))
                               (target (+ new-pc off)))
                          (unless (gethash target label-map)
                            (setf (gethash target label-map)
                                  (mvm-make-label)))))
                       ((and (member :off32 op-types)
                             (member :reg op-types))
                        (let* ((off (second operands))
                               (target (+ new-pc off)))
                          (unless (gethash target label-map)
                            (setf (gethash target label-map)
                                  (mvm-make-label)))))))))
               (setf pc new-pc)))

    ;; Register function entry points
    ;; function-table is a list of (name offset length) — same format as i386
    (when function-table
      (dolist (entry function-table)
        (let ((mvm-offset (second entry)))
          (unless (gethash mvm-offset label-map)
            (setf (gethash mvm-offset label-map) (mvm-make-label))))))

    ;; Second pass: translate instructions
    (setf pc 0)
    (loop while (< pc len)
          do (progn
               ;; Emit label at current PC before translating
               (let ((label (gethash pc label-map)))
                 (when label
                   (arm32-emit-label buf label)))
               (let* ((decoded (decode-instruction bc pc))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pc (cddr decoded)))
                 (arm32-translate-insn buf opcode operands new-pc label-map function-table)
                 (setf pc new-pc))))

    ;; Emit software divide routine at end (only needed for ARMv5)
    (unless *arm32-v7*
      (arm32-emit-divmod buf))

    ;; Resolve branch fixups
    (arm32-resolve-fixups buf)

    ;; Build fn-map: name → resolved native position
    (let ((fn-map (make-hash-table :test 'equal)))
      (when function-table
        (dolist (entry function-table)
          (let* ((name (first entry))
                 (mvm-offset (second entry))
                 (label (gethash mvm-offset label-map)))
            (when label
              ;; Label positions are instruction indices (word count),
              ;; convert to byte offsets for cross.lisp
              (setf (gethash name fn-map)
                    (* (gethash label (arm32-buffer-labels buf)) 4))))))
      (values buf fn-map))))

;;; ============================================================
;;; Installer
;;; ============================================================

(defun install-arm32-translator ()
  "Install the ARM32 (ARMv5) translator into the target descriptor."
  (let ((target *target-arm32*))
    (setf (target-translate-fn target) #'translate-mvm-to-arm32)
    (setf (target-emit-prologue target)
          (lambda (target buf)
            (declare (ignore target))
            (arm32-emit-prologue buf)))
    (setf (target-emit-epilogue target)
          (lambda (target buf)
            (declare (ignore target))
            (arm32-emit-epilogue buf)))
    target))

(defun install-armv7-translator ()
  "Install the ARMv7-A translator into the target descriptor."
  (let ((target *target-armv7*))
    (setf (target-translate-fn target)
          (lambda (bytecode function-table)
            (translate-mvm-to-arm32 bytecode function-table :v7 t)))
    (setf (target-emit-prologue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*arm32-v7* t)) (arm32-emit-prologue buf))))
    (setf (target-emit-epilogue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*arm32-v7* t)) (arm32-emit-epilogue buf))))
    target))

(defun install-armv7-rpi-translator ()
  "Install the ARMv7-A RPi translator (PL011 UART at 0x3F201000)."
  (let ((target *target-armv7-rpi*))
    (setf (target-translate-fn target)
          (lambda (bytecode function-table)
            (let ((*arm32-uart-override* #x3F201000))
              (translate-mvm-to-arm32 bytecode function-table :v7 t))))
    (setf (target-emit-prologue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*arm32-v7* t)) (arm32-emit-prologue buf))))
    (setf (target-emit-epilogue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*arm32-v7* t)) (arm32-emit-epilogue buf))))
    target))
