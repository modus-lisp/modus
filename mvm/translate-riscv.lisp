;;;; translate-riscv.lisp - MVM → RISC-V 64-bit (RV64GC) native code translator
;;;;
;;;; Translates MVM bytecode to native RISC-V machine code. Includes a
;;;; complete RV64GC instruction encoder (R/I/S/B/U/J-type formats) and
;;;; translation patterns for all MVM instructions.
;;;;
;;;; RISC-V register mapping (from target.lisp):
;;;;   V0  -> a0  (x10)    V1  -> a1  (x11)    V2  -> a2  (x12)   V3  -> a3  (x13)
;;;;   V4  -> s0  (x8)     V5  -> s1  (x9)     V6  -> s2  (x18)   V7  -> s3  (x19)
;;;;   V8  -> s4  (x20)    V9  -> s5  (x21)    V10 -> s6  (x22)   V11 -> s7  (x23)
;;;;   V12-V15 -> stack spill
;;;;   VR  -> a0  (x10)    VA  -> s8  (x24)    VL  -> s9  (x25)   VN  -> s10 (x26)
;;;;   VSP -> sp  (x2)     VFP -> fp  (x8, alias of s0)
;;;;
;;;; Scratch temporaries: t0-t6 (x5-x7, x28-x31)

(in-package :modus.mvm)

;;; ============================================================
;;; RISC-V Physical Register Encoding
;;; ============================================================

;; Standard RISC-V register numbers (x0-x31)
(defconstant +rv-x0+   0)   ; zero (hardwired zero)
(defconstant +rv-ra+    1)   ; x1  - return address
(defconstant +rv-sp+    2)   ; x2  - stack pointer
(defconstant +rv-gp+    3)   ; x3  - global pointer
(defconstant +rv-tp+    4)   ; x4  - thread pointer
(defconstant +rv-t0+    5)   ; x5  - temporary 0
(defconstant +rv-t1+    6)   ; x6  - temporary 1
(defconstant +rv-t2+    7)   ; x7  - temporary 2
(defconstant +rv-s0+    8)   ; x8  - saved 0 / frame pointer
(defconstant +rv-fp+    8)   ; x8  - frame pointer (alias of s0)
(defconstant +rv-s1+    9)   ; x9  - saved 1
(defconstant +rv-a0+   10)   ; x10 - argument 0 / return value
(defconstant +rv-a1+   11)   ; x11 - argument 1
(defconstant +rv-a2+   12)   ; x12 - argument 2
(defconstant +rv-a3+   13)   ; x13 - argument 3
(defconstant +rv-a4+   14)   ; x14 - argument 4
(defconstant +rv-a5+   15)   ; x15 - argument 5
(defconstant +rv-a6+   16)   ; x16 - argument 6
(defconstant +rv-a7+   17)   ; x17 - argument 7
(defconstant +rv-s2+   18)   ; x18 - saved 2
(defconstant +rv-s3+   19)   ; x19 - saved 3
(defconstant +rv-s4+   20)   ; x20 - saved 4
(defconstant +rv-s5+   21)   ; x21 - saved 5
(defconstant +rv-s6+   22)   ; x22 - saved 6
(defconstant +rv-s7+   23)   ; x23 - saved 7
(defconstant +rv-s8+   24)   ; x24 - saved 8
(defconstant +rv-s9+   25)   ; x25 - saved 9
(defconstant +rv-s10+  26)   ; x26 - saved 10
(defconstant +rv-s11+  27)   ; x27 - saved 11
(defconstant +rv-t3+   28)   ; x28 - temporary 3
(defconstant +rv-t4+   29)   ; x29 - temporary 4
(defconstant +rv-t5+   30)   ; x30 - temporary 5
(defconstant +rv-t6+   31)   ; x31 - temporary 6

;;; MVM virtual register -> RISC-V physical register mapping
;;; NIL for registers that spill to stack.
(defparameter *riscv-reg-map*
  (vector +rv-a0+    ; V0  -> a0  (x10)
          +rv-a1+    ; V1  -> a1  (x11)
          +rv-a2+    ; V2  -> a2  (x12)
          +rv-a3+    ; V3  -> a3  (x13)
          +rv-s11+   ; V4  -> s11 (x27)  [NOT s0/fp — that's the frame pointer]
          +rv-s1+    ; V5  -> s1  (x9)
          +rv-s2+    ; V6  -> s2  (x18)
          +rv-s3+    ; V7  -> s3  (x19)
          +rv-s4+    ; V8  -> s4  (x20)
          +rv-s5+    ; V9  -> s5  (x21)
          +rv-s6+    ; V10 -> s6  (x22)
          +rv-s7+    ; V11 -> s7  (x23)
          nil        ; V12 -> spill
          nil        ; V13 -> spill
          nil        ; V14 -> spill
          nil        ; V15 -> spill
          +rv-a0+    ; VR  -> a0  (x10, aliases V0)
          +rv-s8+    ; VA  -> s8  (x24)
          +rv-s9+    ; VL  -> s9  (x25)
          +rv-s10+   ; VN  -> s10 (x26)
          +rv-sp+    ; VSP -> sp  (x2)
          +rv-fp+    ; VFP -> fp  (x8, alias of s0)
          nil))      ; VPC -> not mapped

;;; ============================================================
;;; RISC-V Native Code Buffer
;;; ============================================================

(defstruct rv-buffer
  (bytes (make-array 131072))           ; fixed-size, position tracks fill
  (position 0)
  (labels (make-hash-table :test 'eql))
  (fixups nil))    ; list of (byte-position label-id type)

(defun rv-emit-u32 (buf word)
  "Emit a 32-bit instruction word (little-endian) to the RISC-V code buffer."
  (let ((bytes (rv-buffer-bytes buf))
        (pos (rv-buffer-position buf)))
    (setf (aref bytes pos)       (logand word #xFF))
    (setf (aref bytes (+ pos 1)) (logand (ash word -8) #xFF))
    (setf (aref bytes (+ pos 2)) (logand (ash word -16) #xFF))
    (setf (aref bytes (+ pos 3)) (logand (ash word -24) #xFF))
    (setf (rv-buffer-position buf) (+ pos 4))))

(defun rv-current-offset (buf)
  "Return the current emission offset in bytes."
  (rv-buffer-position buf))

(defun rv-emit-label (buf label-id)
  "Record the current position as the target of LABEL-ID."
  (setf (gethash label-id (rv-buffer-labels buf))
        (rv-buffer-position buf)))

(defun rv-patch-u32 (buf byte-pos word)
  "Overwrite 4 bytes at BYTE-POS with WORD (little-endian)."
  (let ((bytes (rv-buffer-bytes buf)))
    (setf (aref bytes byte-pos)       (logand word #xFF))
    (setf (aref bytes (+ byte-pos 1)) (logand (ash word -8) #xFF))
    (setf (aref bytes (+ byte-pos 2)) (logand (ash word -16) #xFF))
    (setf (aref bytes (+ byte-pos 3)) (logand (ash word -24) #xFF))))

;;; ============================================================
;;; RISC-V Instruction Encoding (RV64GC)
;;; ============================================================
;;;
;;; All instructions are 32 bits. Formats:
;;; R-type: [funct7:7][rs2:5][rs1:5][funct3:3][rd:5][opcode:7]
;;; I-type: [imm[11:0]:12][rs1:5][funct3:3][rd:5][opcode:7]
;;; S-type: [imm[11:5]:7][rs2:5][rs1:5][funct3:3][imm[4:0]:5][opcode:7]
;;; B-type: [imm[12|10:5]:7][rs2:5][rs1:5][funct3:3][imm[4:1|11]:5][opcode:7]
;;; U-type: [imm[31:12]:20][rd:5][opcode:7]
;;; J-type: [imm[20|10:1|11|19:12]:20][rd:5][opcode:7]

(defun rv-encode-r-type (funct7 rs2 rs1 funct3 rd opcode)
  "Encode an R-type instruction."
  (logior (ash (logand funct7 #x7F) 25)
          (ash (logand rs2 #x1F) 20)
          (ash (logand rs1 #x1F) 15)
          (ash (logand funct3 #x07) 12)
          (ash (logand rd #x1F) 7)
          (logand opcode #x7F)))

(defun rv-encode-i-type (imm12 rs1 funct3 rd opcode)
  "Encode an I-type instruction. IMM12 is sign-extended 12-bit immediate."
  (logior (ash (logand imm12 #xFFF) 20)
          (ash (logand rs1 #x1F) 15)
          (ash (logand funct3 #x07) 12)
          (ash (logand rd #x1F) 7)
          (logand opcode #x7F)))

(defun rv-encode-s-type (imm12 rs2 rs1 funct3 opcode)
  "Encode an S-type instruction. IMM12 split across two fields."
  (logior (ash (logand (ash imm12 -5) #x7F) 25)
          (ash (logand rs2 #x1F) 20)
          (ash (logand rs1 #x1F) 15)
          (ash (logand funct3 #x07) 12)
          (ash (logand imm12 #x1F) 7)
          (logand opcode #x7F)))

(defun rv-encode-b-type (imm13 rs2 rs1 funct3 opcode)
  "Encode a B-type instruction. IMM13 is a signed 13-bit offset (bit 0 always 0).
   Layout: [imm[12]][imm[10:5]] | rs2 | rs1 | funct3 | [imm[4:1]][imm[11]] | opcode"
  (unless (<= -4096 imm13 4095)
    (error "RISC-V B-type imm13 ~D out of range [-4096, 4095] — ~
            would silently truncate to ±4KB conditional branch"
           imm13))
  (let ((imm (logand imm13 #x1FFF)))
    (logior (ash (logand (ash imm -12) #x01) 31)   ; imm[12]
            (ash (logand (ash imm -5) #x3F) 25)    ; imm[10:5]
            (ash (logand rs2 #x1F) 20)
            (ash (logand rs1 #x1F) 15)
            (ash (logand funct3 #x07) 12)
            (ash (logand (ash imm -1) #x0F) 8)     ; imm[4:1]
            (ash (logand (ash imm -11) #x01) 7)    ; imm[11]
            (logand opcode #x7F))))

(defun rv-encode-u-type (imm32 rd opcode)
  "Encode a U-type instruction. IMM32 uses bits [31:12]."
  (logior (logand imm32 #xFFFFF000)
          (ash (logand rd #x1F) 7)
          (logand opcode #x7F)))

(defun rv-encode-j-type (imm21 rd opcode)
  "Encode a J-type instruction. IMM21 is a signed 21-bit offset (bit 0 always 0).
   Layout: [imm[20]][imm[10:1]][imm[11]][imm[19:12]] | rd | opcode"
  (unless (<= -1048576 imm21 1048575)
    (error "RISC-V J-type imm21 ~D out of range [-1MB, 1MB-1] — ~
            would silently truncate JAL/J branch"
           imm21))
  (let ((imm (logand imm21 #x1FFFFF)))
    (logior (ash (logand (ash imm -20) #x01) 31)   ; imm[20]
            (ash (logand (ash imm -1) #x3FF) 21)   ; imm[10:1]
            (ash (logand (ash imm -11) #x01) 20)   ; imm[11]
            (ash (logand (ash imm -12) #xFF) 12)    ; imm[19:12]
            (ash (logand rd #x1F) 7)
            (logand opcode #x7F))))

;;; ============================================================
;;; RISC-V Instruction Emitters
;;; ============================================================
;;; Convenience functions that encode and emit common instructions.

;; --- R-type ALU (opcode #b0110011 = #x33) ---

(defun rv-emit-add (buf rd rs1 rs2)
  "ADD rd, rs1, rs2"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x0 rd #x33)))

(defun rv-emit-sub (buf rd rs1 rs2)
  "SUB rd, rs1, rs2"
  (rv-emit-u32 buf (rv-encode-r-type #x20 rs2 rs1 #x0 rd #x33)))

(defun rv-emit-and (buf rd rs1 rs2)
  "AND rd, rs1, rs2"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x7 rd #x33)))

(defun rv-emit-or (buf rd rs1 rs2)
  "OR rd, rs1, rs2"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x6 rd #x33)))

(defun rv-emit-xor (buf rd rs1 rs2)
  "XOR rd, rs1, rs2"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x4 rd #x33)))

(defun rv-emit-sll (buf rd rs1 rs2)
  "SLL rd, rs1, rs2 (shift left logical, shift amount in rs2[4:0])"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x1 rd #x33)))

(defun rv-emit-srl (buf rd rs1 rs2)
  "SRL rd, rs1, rs2 (shift right logical)"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x5 rd #x33)))

(defun rv-emit-sra (buf rd rs1 rs2)
  "SRA rd, rs1, rs2 (shift right arithmetic)"
  (rv-emit-u32 buf (rv-encode-r-type #x20 rs2 rs1 #x5 rd #x33)))

(defun rv-emit-slt (buf rd rs1 rs2)
  "SLT rd, rs1, rs2 (set less than, signed)"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x2 rd #x33)))

;; --- RV64 R-type word ops (opcode #b0111011 = #x3B) ---

(defun rv-emit-addw (buf rd rs1 rs2)
  "ADDW rd, rs1, rs2 (32-bit add)"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs2 rs1 #x0 rd #x3B)))

(defun rv-emit-subw (buf rd rs1 rs2)
  "SUBW rd, rs1, rs2 (32-bit sub)"
  (rv-emit-u32 buf (rv-encode-r-type #x20 rs2 rs1 #x0 rd #x3B)))

;; --- M extension (multiply/divide, funct7=#x01) ---

(defun rv-emit-mul (buf rd rs1 rs2)
  "MUL rd, rs1, rs2 (multiply, low 64 bits)"
  (rv-emit-u32 buf (rv-encode-r-type #x01 rs2 rs1 #x0 rd #x33)))

(defun rv-emit-mulh (buf rd rs1 rs2)
  "MULH rd, rs1, rs2 (multiply high, signed x signed)"
  (rv-emit-u32 buf (rv-encode-r-type #x01 rs2 rs1 #x1 rd #x33)))

(defun rv-emit-mulhu (buf rd rs1 rs2)
  "MULHU rd, rs1, rs2 -- high XLEN bits of the UNSIGNED product."
  (rv-emit-u32 buf (rv-encode-r-type #x01 rs2 rs1 #x3 rd #x33)))

(defun rv-emit-div (buf rd rs1 rs2)
  "DIV rd, rs1, rs2 (signed divide)"
  (rv-emit-u32 buf (rv-encode-r-type #x01 rs2 rs1 #x4 rd #x33)))

(defun rv-emit-rem (buf rd rs1 rs2)
  "REM rd, rs1, rs2 (signed remainder)"
  (rv-emit-u32 buf (rv-encode-r-type #x01 rs2 rs1 #x6 rd #x33)))

;; --- I-type ALU (opcode #b0010011 = #x13) ---

(defun rv-emit-addi (buf rd rs1 imm12)
  "ADDI rd, rs1, imm12"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x0 rd #x13)))

(defun rv-emit-andi (buf rd rs1 imm12)
  "ANDI rd, rs1, imm12"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x7 rd #x13)))

(defun rv-emit-ori (buf rd rs1 imm12)
  "ORI rd, rs1, imm12"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x6 rd #x13)))

(defun rv-emit-xori (buf rd rs1 imm12)
  "XORI rd, rs1, imm12"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x4 rd #x13)))

(defun rv-emit-slti (buf rd rs1 imm12)
  "SLTI rd, rs1, imm12 (set less than immediate, signed)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x2 rd #x13)))

(defun rv-shamt (shamt)
  "Mask an immediate shift amount to the SHAMT FIELD OF THIS WIDTH: 6 bits on
   RV64, 5 on RV32.  The extra bit is not merely ignored on RV32 -- bit 25 is
   part of the funct7 field there, so a shamt of 40 does not shift by 8, it
   decodes as a reserved instruction.  Every width-dependent shift distance in
   this file goes through a derived helper (RV-WORD-SHIFT, RV-MASK24-SHIFT,
   RV-MASK26-SHIFT); this is the backstop for the rest."
  (logand shamt (if *riscv-64-bit* #x3F #x1F)))

(defun rv-emit-slli (buf rd rs1 shamt)
  "SLLI rd, rs1, shamt (shift left logical immediate)"
  (rv-emit-u32 buf (rv-encode-i-type (rv-shamt shamt) rs1 #x1 rd #x13)))

(defun rv-emit-srli (buf rd rs1 shamt)
  "SRLI rd, rs1, shamt (shift right logical immediate)"
  (rv-emit-u32 buf (rv-encode-i-type (rv-shamt shamt) rs1 #x5 rd #x13)))

(defun rv-emit-srai (buf rd rs1 shamt)
  "SRAI rd, rs1, shamt (shift right arithmetic immediate)"
  (rv-emit-u32 buf (rv-encode-i-type (logior #x400 (rv-shamt shamt))
                                      rs1 #x5 rd #x13)))

;; --- RV64 I-type word ops (opcode #b0011011 = #x1B) ---

(defun rv-emit-addiw (buf rd rs1 imm12)
  "ADDIW rd, rs1, imm12 (32-bit add immediate)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x0 rd #x1B)))

;; --- Load instructions (opcode #b0000011 = #x03) ---

(defparameter *riscv-64-bit* t
  "T for RV64, NIL for RV32.  Same shape as *PPC-64-BIT*: one translator, two
   word sizes, chosen by the installer.

   RV32 IS THE EMBEDDED RISC-V.  RV64 is what servers and QEMU virt run; the
   parts that cost ten cents — CH32V003, ESP32-C3 — are RV32IMC.  A 3 KB bare
   image that computes is only interesting if it can be flashed onto one of
   those, and that means emitting 4-byte words and a 30-bit fixnum tower, which
   the MVM already derives from (TARGET-WORD-SIZE ...).")

(defun rv-word-size ()
  "Current word size in bytes: 8 on RV64, 4 on RV32."
  (if *riscv-64-bit* 8 4))

(defun rv-index-shift ()
  "Shift that turns a TAGGED index (2k) into a word byte-offset (k*word):
   log2(word) - 1.  2 on RV64, 1 on RV32."
  (if *riscv-64-bit* 2 1))

(defvar *riscv-li-const-patches* nil
  "List of (NATIVE-BYTE-OFFSET . POOL-INDEX) recorded by +OP-LI-CONST+.

   Each entry says: at NATIVE-BYTE-OFFSET there is a FIVE-INSTRUCTION
   shift-and-add chain whose three ADDI immediate fields must be patched with
   the tagged constant-pool address of slot POOL-INDEX, once the image layout is
   final.  cross.lisp's APPLY-LI-CONST-PATCHES does it, exactly as it does for
   x64's MOVABS immediate and AArch64's MOVZ/MOVK quad.

   WHY A CHAIN AND NOT `LUI + ADDI'.  LUI SIGN-EXTENDS ON RV64, so any address
   with bit 31 set -- every bare-metal DRAM address, since QEMU virt starts DRAM
   at 0x80000000 -- would come out as 0xFFFFFFFF_8.......  The chain builds the
   value from three NON-NEGATIVE sub-2048 chunks (11 + 11 + 10 bits = 32), so
   nothing sign-extends and each field is independent, which is what makes it
   patchable at all.  Same split RV-EMIT-LI's 64-bit case uses, and it is the
   same bug that case was rebuilt to avoid.")

(defun rv-word-shift ()
  "Shift that multiplies a count by the word size: 3 on RV64, 2 on RV32."
  (if *riscv-64-bit* 3 2))

(defun rv-granule ()
  "Allocation granule: TWO words, so the bump pointer stays word-pair aligned
   and a tag of 1 (cons) or 2 (object) is exact.  16 on RV64, 8 on RV32."
  (* 2 (rv-word-size)))

(defun rv-mask26-shift ()
  "Shift-out/shift-back distance that masks a value to its low 26 bits:
   register width minus 26.  38 on RV64, 6 on RV32."
  (- (* 8 (rv-word-size)) 26))

(defun rv-mask24-shift ()
  "Shift-out/shift-back distance that masks a value to its low 24 bits:
   register width minus 24.  40 on RV64, 8 on RV32.  RV32's SLLI takes a 5-bit
   shamt, so 40 is not merely wrong there — it is unencodable."
  (- (* 8 (rv-word-size)) 24))

(defun rv-emit-store-word (buf rs2 rs1 imm12)
  "Store one Lisp word: SD on RV64, SW on RV32."
  (if *riscv-64-bit*
      (rv-emit-sd buf rs2 rs1 imm12)
      (rv-emit-sw buf rs2 rs1 imm12)))

(defun rv-emit-load-word (buf rd rs1 imm12)
  "Load one Lisp word: LD on RV64, LW on RV32."
  (if *riscv-64-bit*
      (rv-emit-ld buf rd rs1 imm12)
      (rv-emit-lw buf rd rs1 imm12)))

(defun rv-emit-ld (buf rd rs1 imm12)
  "LD rd, imm12(rs1) (load doubleword, 64-bit)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x3 rd #x03)))

(defun rv-emit-lw (buf rd rs1 imm12)
  "LW rd, imm12(rs1) (load word, 32-bit sign-extended)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x2 rd #x03)))

(defun rv-emit-lh (buf rd rs1 imm12)
  "LH rd, imm12(rs1) (load halfword, 16-bit sign-extended)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x1 rd #x03)))

(defun rv-emit-lb (buf rd rs1 imm12)
  "LB rd, imm12(rs1) (load byte, 8-bit sign-extended)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x0 rd #x03)))

(defun rv-emit-lbu (buf rd rs1 imm12)
  "LBU rd, imm12(rs1) (load byte unsigned)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x4 rd #x03)))

(defun rv-emit-lwu (buf rd rs1 imm12)
  "LWU rd, imm12(rs1) (load word unsigned)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x6 rd #x03)))

;; --- Store instructions (opcode #b0100011 = #x23) ---

(defun rv-emit-sd (buf rs2 rs1 imm12)
  "SD rs2, imm12(rs1) (store doubleword, 64-bit)"
  (rv-emit-u32 buf (rv-encode-s-type imm12 rs2 rs1 #x3 #x23)))

(defun rv-emit-sw (buf rs2 rs1 imm12)
  "SW rs2, imm12(rs1) (store word, 32-bit)"
  (rv-emit-u32 buf (rv-encode-s-type imm12 rs2 rs1 #x2 #x23)))

(defun rv-emit-sh (buf rs2 rs1 imm12)
  "SH rs2, imm12(rs1) (store halfword, 16-bit)"
  (rv-emit-u32 buf (rv-encode-s-type imm12 rs2 rs1 #x1 #x23)))

(defun rv-emit-sb (buf rs2 rs1 imm12)
  "SB rs2, imm12(rs1) (store byte, 8-bit)"
  (rv-emit-u32 buf (rv-encode-s-type imm12 rs2 rs1 #x0 #x23)))

;; --- Branch instructions (opcode #b1100011 = #x63) ---

(defun rv-emit-beq (buf rs1 rs2 offset)
  "BEQ rs1, rs2, offset (branch if equal)"
  (rv-emit-u32 buf (rv-encode-b-type offset rs2 rs1 #x0 #x63)))

(defun rv-emit-bne (buf rs1 rs2 offset)
  "BNE rs1, rs2, offset (branch if not equal)"
  (rv-emit-u32 buf (rv-encode-b-type offset rs2 rs1 #x1 #x63)))

(defun rv-emit-blt (buf rs1 rs2 offset)
  "BLT rs1, rs2, offset (branch if less than, signed)"
  (rv-emit-u32 buf (rv-encode-b-type offset rs2 rs1 #x4 #x63)))

(defun rv-emit-bge (buf rs1 rs2 offset)
  "BGE rs1, rs2, offset (branch if greater or equal, signed)"
  (rv-emit-u32 buf (rv-encode-b-type offset rs2 rs1 #x5 #x63)))

;; --- U-type instructions ---

(defun rv-emit-lui (buf rd imm20)
  "LUI rd, imm20 (load upper immediate, bits [31:12])"
  (rv-emit-u32 buf (rv-encode-u-type (ash imm20 12) rd #x37)))

(defun rv-emit-auipc (buf rd imm20)
  "AUIPC rd, imm20 (add upper immediate to PC)"
  (rv-emit-u32 buf (rv-encode-u-type (ash imm20 12) rd #x17)))

;; --- J-type / JALR ---

(defun rv-emit-jal (buf rd offset)
  "JAL rd, offset (jump and link, 21-bit signed offset)"
  (rv-emit-u32 buf (rv-encode-j-type offset rd #x6F)))

(defun rv-emit-jalr (buf rd rs1 imm12)
  "JALR rd, rs1, imm12 (jump and link register)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x0 rd #x67)))

;; --- System instructions ---

(defun rv-emit-ecall (buf)
  "ECALL (environment call)"
  (rv-emit-u32 buf (rv-encode-i-type #x000 +rv-x0+ #x0 +rv-x0+ #x73)))

(defun rv-emit-ebreak (buf)
  "EBREAK (breakpoint)"
  (rv-emit-u32 buf (rv-encode-i-type #x001 +rv-x0+ #x0 +rv-x0+ #x73)))

(defun rv-emit-fence (buf pred succ)
  "FENCE pred, succ (memory ordering).
   pred/succ are 4-bit masks: bit3=i, bit2=o, bit1=r, bit0=w."
  (rv-emit-u32 buf (rv-encode-i-type (logior (ash (logand pred #xF) 4)
                                              (logand succ #xF))
                                      +rv-x0+ #x0 +rv-x0+ #x0F)))

(defun rv-emit-wfi (buf)
  "WFI (wait for interrupt, needs M-mode privilege)"
  (rv-emit-u32 buf (rv-encode-i-type #x105 +rv-x0+ #x0 +rv-x0+ #x73)))

;; --- CSR instructions ---

(defun rv-emit-csrrs (buf rd csr rs1)
  "CSRRS rd, csr, rs1 (read-set CSR)"
  (rv-emit-u32 buf (rv-encode-i-type csr rs1 #x2 rd #x73)))

(defun rv-emit-csrrw (buf rd csr rs1)
  "CSRRW rd, csr, rs1 (read-write CSR)"
  (rv-emit-u32 buf (rv-encode-i-type csr rs1 #x1 rd #x73)))

(defun rv-emit-csrrc (buf rd csr rs1)
  "CSRRC rd, csr, rs1 (read-clear CSR)"
  (rv-emit-u32 buf (rv-encode-i-type csr rs1 #x3 rd #x73)))

;; --- Atomic (A extension, opcode #b0101111 = #x2F) ---

(defun rv-emit-amoswap (buf rd rs2 rs1 aqrl)
  "AMOSWAP.D (RV64) / AMOSWAP.W (RV32) rd, rs2, (rs1) with acquire/release.
   AQRL: bit1=aq, bit0=rl.  AMOSWAP.D DOES NOT EXIST ON RV32 -- funct3 is the
   access width, so emitting 011 there is an illegal instruction, not a wider
   swap."
  (rv-emit-u32 buf (logior (ash #x01 27)              ; funct5 = 00001
                            (ash (logand aqrl #x3) 25) ; aq/rl
                            (ash (logand rs2 #x1F) 20)
                            (ash (logand rs1 #x1F) 15)
                            (ash (if *riscv-64-bit* #x3 #x2) 12) ; width: D or W
                            (ash (logand rd #x1F) 7)
                            #x2F)))

(defun rv-emit-amoswap-d (buf rd rs2 rs1 aqrl)
  "Deprecated name kept for callers; dispatches on width."
  (rv-emit-amoswap buf rd rs2 rs1 aqrl))

;; --- Pseudo-instructions ---

(defun rv-emit-nop (buf)
  "NOP (addi x0, x0, 0)"
  (rv-emit-addi buf +rv-x0+ +rv-x0+ 0))

(defun rv-emit-mv (buf rd rs1)
  "MV rd, rs1 (addi rd, rs1, 0)"
  (rv-emit-addi buf rd rs1 0))

(defun rv-emit-li (buf rd imm64)
  "Load a 64-bit immediate into RD. Uses the minimal instruction sequence:
   - Small values (fits in 12-bit signed): addi rd, x0, imm
   - 32-bit values: lui + addi
   - Large values: lui + addi + slli + addi (up to 6 instructions for full 64-bit)"
  ;; RV32: every immediate IS 32 bits.  Re-read it as signed-32 so cases 1 and 2
  ;; cover the whole range and the 64-bit chain below is unreachable.
  (unless *riscv-64-bit*
    (let ((v (logand imm64 #xFFFFFFFF)))
      (setf imm64 (if (logbitp 31 v) (- v #x100000000) v))))
  (cond
    ;; Case 1: fits in signed 12-bit [-2048, 2047]
    ((and (>= imm64 -2048) (<= imm64 2047))
     (rv-emit-addi buf rd +rv-x0+ (logand imm64 #xFFF)))

    ;; Case 2: fits in signed 32-bit
    ((and (>= imm64 (- (ash 1 31))) (<= imm64 (1- (ash 1 31))))
     (let* ((lo12 (logand imm64 #xFFF))
            (lo12-sext (if (>= lo12 #x800) (- lo12 #x1000) lo12))
            (hi20 (ash (- imm64 lo12-sext) -12)))
       (rv-emit-lui buf rd (logand hi20 #xFFFFF))
       (when (/= lo12-sext 0)
         (rv-emit-addi buf rd rd (logand lo12-sext #xFFF)))))

    ;; Case 3: full 64-bit immediate — shift-and-add, no scratch register.
    ;;
    ;; TWO BUGS LIVED IN THE OLD `lui scratch,hi20; add rd,rd,scratch` form:
    ;;
    ;;   1. The scratch was hard-coded to t0, and every caller of this path
    ;;      passes t0 AS RD -- so the LUI destroyed the high half that had just
    ;;      been shifted into place, leaving twice the low half.
    ;;   2. LUI SIGN-EXTENDS on RV64.  Any low half with bit 31 set (which is
    ;;      every virt DRAM address, since DRAM starts at 0x80000000) came out
    ;;      as 0xFFFFFFFF_8......, so the store faulted on a nonsense address.
    ;;
    ;; Building the value as one signed-32 head plus three NON-NEGATIVE
    ;; sub-2048 ADDI chunks avoids both: ADDI's immediate is never negative
    ;; here, so nothing sign-extends, and RD is the only register touched.
    ;; Fixed six instructions, so the two-pass size measurement stays stable.
    (t
     (let* ((val (if (minusp imm64) (logand imm64 #xFFFFFFFFFFFFFFFF) imm64))
            (hi32 (logand (ash val -32) #xFFFFFFFF))
            (lo32 (logand val #xFFFFFFFF))
            (c-a (logand (ash lo32 -21) #x7FF))   ; bits 31..21  (11 bits)
            (c-b (logand (ash lo32 -10) #x7FF))   ; bits 20..10  (11 bits)
            (c-c (logand lo32 #x3FF)))            ; bits  9..0   (10 bits)
       ;; Upper 32 bits via the signed-32 path (recursion terminates: case 2).
       (rv-emit-li buf rd (if (logbitp 31 hi32)
                              (- hi32 #x100000000)
                              hi32))
       (rv-emit-slli buf rd rd 11)
       (rv-emit-addi buf rd rd c-a)
       (rv-emit-slli buf rd rd 11)
       (rv-emit-addi buf rd rd c-b)
       (rv-emit-slli buf rd rd 10)
       (rv-emit-addi buf rd rd c-c)))))

;;; ============================================================
;;; Convention slots (nargs / cenv / mv-count)
;;; ============================================================
;;;
;;; x64 and aarch64 keep these in spare PHYSICAL registers.  RISC-V has spare
;;; registers too, but the i386 translator already established the portable
;;; shape -- fixed absolute slots written by the caller and read by the callee
;;; -- and single-threaded cooperative execution makes a memory slot exactly as
;;; correct as a register.  Mirroring i386 keeps one contract to reason about.
;;;
;;; Placed at 0x80700000: inside virt's DRAM, above the page tables
;;; (0x80500000) and well below wired memory (0x82000000), so it collides with
;;; neither the downward stack (top 0x80400000) nor the heap.
(defparameter *rv-globals-base* #x80700000
  "Base of the RISC-V absolute-address convention slot block.

   #x80700000 IS BARE-METAL ONLY.  It is DRAM on QEMU virt, chosen because the
   shared #x10000000 block is the NS16550 UART's MMIO window there.  Under
   hosted Linux neither address is special and #x80700000 is simply NOT MAPPED:
   INSTALL-RISCV-TRANSLATOR moves this into the mmap'd heap when
   *RISCV-LINUX-MODE* is set.  Measured before it did — the hosted image
   mmap'd its heap successfully and then took SIGSEGV at si_addr=0x80700000 on
   the first :set-nargs, which qemu-riscv64-static -strace named in one line.

   RISCV64 is not one target.  Bare and hosted are different memory maps,
   and every absolute address this back end bakes has to follow that.")
(defparameter *rv-hosted-globals-base* #x10000A00
  "Convention slots for the HOSTED port, inside the mmap'd heap and above the
   Cheney metadata at #x10000040.  Same choice boot-linux-i386.lisp makes for
   the same reason (its comment names 0x10000A00 the i386 global slot block).")
(defun rv-nargs-addr () (+ *rv-globals-base* #x00))
(defun rv-cenv-addr ()  (+ *rv-globals-base* #x08))
;;; MV-COUNT DIVERGES FROM THE SHARED CONTRACT ADDRESS, AND MUST.
;;; modus.mvm::+mv-count-addr+ is #x10000090, baked into compiler-emitted
;;; mem-refs and into shared CL source.  On QEMU virt that address is not RAM
;;; at all -- it is INSIDE the NS16550 UART's MMIO window at 0x10000000 -- so
;;; honouring the shared address would turn every epilogue's "I returned one
;;; value" reset into a UART register write.  A private slot is therefore the
;;; only safe choice here, with the consequence stated plainly: MULTIPLE-VALUE
;;; forms that read the shared literal will NOT see what :set-mv-count wrote,
;;; so multiple values are not yet correct on RISC-V.  Fixing that properly
;;; means making +mv-count-addr+ per-target, which is a change to shared code.
;;; The MV-count slot is NO LONGER private to this back end.  It is
;;; +MV-COUNT-ADDR+, set per target in mvm/target.lisp (RISC-V's value is
;;; #x80700010, in DRAM, because #x10000090 is UART MMIO on virt) and injected
;;; into every compilation, so compiler-expanded mem-refs, shared CL source and
;;; this :set-mv-count all name the same word.  Read it at EMISSION time: the
;;; target is chosen before translation, not when this file loads.

(defun rv-emit-store-abs (buf src-reg addr)
  "Store SRC-REG (one WORD, 8 or 4 bytes) to absolute ADDR via t1, so t0 stays free."
  (rv-emit-li buf +rv-t1+ addr)
  (rv-emit-store-word buf src-reg +rv-t1+ 0))

(defun rv-emit-load-abs (buf rd addr)
  "Load one WORD (8 or 4 bytes) from absolute ADDR into RD (via t1)."
  (rv-emit-li buf +rv-t1+ addr)
  (rv-emit-load-word buf rd +rv-t1+ 0))

(defun rv-emit-j (buf offset)
  "J offset (unconditional jump, jal x0, offset)"
  (rv-emit-jal buf +rv-x0+ offset))

(defun rv-emit-ret (buf)
  "RET (jalr x0, ra, 0)"
  (rv-emit-jalr buf +rv-x0+ +rv-ra+ 0))

(defun rv-emit-call (buf offset)
  "CALL offset (auipc ra, hi20; jalr ra, ra, lo12) for +-2GB range."
  (let* ((lo12 (logand offset #xFFF))
         (lo12-sext (if (>= lo12 #x800) (- lo12 #x1000) lo12))
         (hi20 (logand (ash (- offset lo12-sext) -12) #xFFFFF)))
    (rv-emit-auipc buf +rv-ra+ hi20)
    (rv-emit-jalr buf +rv-ra+ +rv-ra+ (logand lo12-sext #xFFF))))

(defun rv-emit-neg (buf rd rs1)
  "NEG rd, rs1 (sub rd, x0, rs1)"
  (rv-emit-sub buf rd +rv-x0+ rs1))

(defun rv-emit-not (buf rd rs1)
  "NOT rd, rs1 (xori rd, rs1, -1)"
  (rv-emit-xori buf rd rs1 #xFFF))

(defun rv-emit-seqz (buf rd rs1)
  "SEQZ rd, rs1 (sltiu rd, rs1, 1)"
  (rv-emit-u32 buf (rv-encode-i-type 1 rs1 #x3 rd #x13)))

(defun rv-emit-snez (buf rd rs1)
  "SNEZ rd, rs1 (sltu rd, x0, rs1)"
  (rv-emit-u32 buf (rv-encode-r-type #x00 rs1 +rv-x0+ #x3 rd #x33)))

;;; ============================================================
;;; Virtual Register Resolution
;;; ============================================================

(defun rv-resolve-vreg (vreg)
  "Map an MVM virtual register to a RISC-V physical register number.
   Returns NIL for spilled registers (V12-V15, VPC)."
  (when (< vreg (length *riscv-reg-map*))
    (aref *riscv-reg-map* vreg)))

(defconstant +rv-spill-base-offset+ -120
  "FP-relative offset for the first spill slot (V12).
   Spill slots are below the save area (14 regs * 8 = 112 bytes).
   V12 at FP-120, V13 at FP-128, V14 at FP-136, V15 at FP-144.")

(defconstant +rv-frame-slot-base+ -152
  "FP-relative offset for frame slot 0 (local variables via obj-ref VFP).
   Frame slots grow downward: slot N is at FP + frame-slot-base + N*(-8).
   This is below all spill slots (which end at FP-144) to avoid overlap.")

(defconstant +rv-local-frame-size+ 96
  "Bytes reserved for locals: 4 spill slots (32) + 8 frame slots (64) = 96.
   Total frame = this + 112 (save area) = 208.")

(defun rv-spill-offset (vreg)
  "Compute the FP-relative offset for a spilled vreg (V12-V15)."
  (+ +rv-spill-base-offset+ (* (- vreg 12) -8)))

(defun rv-vreg-or-load (buf vreg target-phys)
  "Resolve VREG to a physical register. If VREG is spilled, load it from
   the frame into TARGET-PHYS (a scratch register) and return TARGET-PHYS.
   Otherwise return the physical register directly."
  (let ((phys (rv-resolve-vreg vreg)))
    (if phys
        phys
        ;; Spilled: load from stack frame (safe FP-relative offsets)
        (progn
          (rv-emit-load-word buf target-phys +rv-fp+ (rv-spill-offset vreg))
          target-phys))))

(defun rv-store-vreg (buf vreg phys)
  "If VREG is spilled, store PHYS back to the frame slot for VREG.
   If VREG is in a register, emit a move if PHYS differs from the target."
  (let ((dest (rv-resolve-vreg vreg)))
    (if dest
        (when (/= dest phys)
          (rv-emit-mv buf dest phys))
        ;; Spilled: store to stack frame (safe FP-relative offsets)
        (rv-emit-store-word buf phys +rv-fp+ (rv-spill-offset vreg)))))

;;; ============================================================
;;; MVM -> RISC-V Translation
;;; ============================================================

(defvar *rv-last-cmp-rs1* +rv-t3+
  "Physical register holding the first operand of the most recent MVM-CMP.")
(defvar *rv-last-cmp-rs2* +rv-t4+
  "Physical register holding the second operand of the most recent MVM-CMP.")

(defun translate-mvm-insn-riscv (buf opcode operands mvm-pc
                                  &key label-map function-table)
  "Translate a single MVM instruction to RISC-V native code.
   BUF is an rv-buffer. OPCODE and OPERANDS come from decode-instruction.
   MVM-PC is the bytecode offset of this instruction (for branch resolution).
   LABEL-MAP maps MVM bytecode offsets to rv-buffer offsets.
   FUNCTION-TABLE maps function indices to native code offsets."
  (flet ((vreg (n) (nth n operands))
         (resolve (vreg &optional (scratch +rv-t0+))
           (rv-vreg-or-load buf vreg scratch))
         (resolve2 (vreg)
           (rv-vreg-or-load buf vreg +rv-t1+))
         (store-result (vreg phys)
           (rv-store-vreg buf vreg phys))
         (branch-offset (mvm-target-pc)
           ;; Compute native byte offset from current position to target
           ;; The label-map is populated in the first pass
           (let ((native-target (gethash mvm-target-pc label-map)))
             (if native-target
                 (- native-target (rv-current-offset buf))
                 0))))  ; placeholder, fixed up in second pass

    (case opcode
      ;; ---- Special ----
      (#.+op-nop+
       (rv-emit-nop buf))

      (#.+op-break+
       (rv-emit-ebreak buf))

      (#.+op-trap+
       (let ((code (vreg 0)))
         (cond
           ((< code #x0100)
            ;; Frame-enter: emit function prologue
            (rv-emit-prologue buf +rv-local-frame-size+))
           ((< code #x0300)
            ;; Frame-alloc/frame-free: NOP for now
            nil)
           ((and (= code #x0300) *riscv-linux-mode*)
            ;; HOSTED: serial write becomes write(1, &byte, 1).  The byte goes
            ;; on the stack because write(2) wants an address, and a0 has to be
            ;; freed for the fd — so the char is untagged into t0 FIRST.
            (rv-emit-srai buf +rv-t0+ +rv-a0+ 1)
            (rv-emit-addi buf +rv-sp+ +rv-sp+ -16)
            (rv-emit-sb buf +rv-t0+ +rv-sp+ 0)
            (rv-emit-li buf +rv-a0+ 1)                 ; fd = stdout
            (rv-emit-mv buf +rv-a1+ +rv-sp+)           ; buf
            (rv-emit-li buf +rv-a2+ 1)                 ; count
            (rv-emit-li buf +rv-a7+ +rv-linux-sys-write+)
            (rv-emit-ecall buf)
            (rv-emit-addi buf +rv-sp+ +rv-sp+ 16))
           ((and (= code #x0301) *riscv-linux-mode*)
            ;; HOSTED: serial read becomes read(0, &byte, 1); the byte comes
            ;; back TAGGED in V0 (a0), matching the bare-metal arm's contract.
            (rv-emit-addi buf +rv-sp+ +rv-sp+ -16)
            (rv-emit-li buf +rv-a0+ 0)                 ; fd = stdin
            (rv-emit-mv buf +rv-a1+ +rv-sp+)
            (rv-emit-li buf +rv-a2+ 1)
            (rv-emit-li buf +rv-a7+ +rv-linux-sys-read+)
            (rv-emit-ecall buf)
            (rv-emit-lbu buf +rv-t0+ +rv-sp+ 0)
            (rv-emit-addi buf +rv-sp+ +rv-sp+ 16)
            (rv-emit-slli buf +rv-a0+ +rv-t0+ 1))      ; tag as fixnum
           ((and (= code #x0500) *riscv-linux-mode*)
            ;; HOSTED: exit(status), status arriving TAGGED in V0.
            (rv-emit-srai buf +rv-a0+ +rv-a0+ 1)
            (rv-emit-li buf +rv-a7+ +rv-linux-sys-exit+)
            (rv-emit-ecall buf))
           ((= code #x0300)
            ;; BARE METAL: serial write: V0 (a0) contains tagged fixnum char code
            ;; srai t0, a0, 1 (untag)
            (rv-emit-srai buf +rv-t0+ +rv-a0+ 1)
            ;; lui t1, 0x10000 (t1 = 0x10000000, QEMU virt UART base)
            (rv-emit-lui buf +rv-t1+ #x10000)
            ;; sb t0, 0(t1) (store byte to UART data register)
            (rv-emit-sb buf +rv-t0+ +rv-t1+ 0))
           (t
            ;; Real CPU trap: ecall with code in a7
            (rv-emit-addi buf +rv-a7+ +rv-x0+ code)
            (rv-emit-ecall buf)))))

      ;; ---- Data Movement ----
      (#.+op-mov+
       (let* ((vd (vreg 0))
              (vs (vreg 1))
              (rs (resolve vs)))
         (store-result vd rs)))

      (#.+op-li+
       (let ((vd (vreg 0))
             (imm (vreg 1)))
         (rv-emit-li buf +rv-t0+ imm)
         (store-result vd +rv-t0+)))

      (#.+op-push+
       (let ((rs (resolve (vreg 0))))
         ;; addi sp, sp, -8; sd rs, 0(sp)
         (rv-emit-addi buf +rv-sp+ +rv-sp+ -8)
         (rv-emit-store-word buf rs +rv-sp+ 0)))

      (#.+op-pop+
       (let ((vd (vreg 0)))
         ;; ld t0, 0(sp); addi sp, sp, 8
         (rv-emit-load-word buf +rv-t0+ +rv-sp+ 0)
         (rv-emit-addi buf +rv-sp+ +rv-sp+ 8)
         (store-result vd +rv-t0+)))

      ;; ---- Arithmetic (tagged fixnums: value << 1 | 0) ----
      ;; For add/sub the tag bits cancel out: (a<<1) + (b<<1) = (a+b)<<1
      ((#.+op-add+ #.+op-add-checked+ #.+op-adds+)
       ;; :ADDS shares this clause.  :adds/:subs are "arithmetic that also sets
       ;; the overflow flag", for a following :bvs to branch on.  The result
       ;; they compute is identical to :add/:sub -- translate-i386 uses the
       ;; very same code for both pairs -- so the value is right here too.
       ;; What is NOT provided is :bvs, which still traps: these back ends do
       ;; not promote on overflow (see the :add-checked note), so a :bvs that
       ;; silently fell through would be a quiet wrong answer rather than a
       ;; visible gap.  RISC-V has no condition flags at all, so a faithful
       ;; :bvs there needs the operands, not a flag -- that is a real design
       ;; question, and it should stay loud until someone answers it.
       ;; :ADD-CHECKED shares this clause.  The checked opcodes mean "tagged
       ;; arithmetic that promotes to a bignum on overflow"; implementing the
       ;; promotion needs the generic-arith slow path, which these back ends do
       ;; not have yet.  Falling back to plain WRAPPING arithmetic is exactly
       ;; what translate-x64 and translate-i386 do when a module has no
       ;; generic-arith entry (see *i386-checked-arith-slowpath*), so this is
       ;; the documented degrade rather than a new invention -- and it is a
       ;; large step up from the previous behaviour, which was to trap.

       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-add buf +rv-t0+ ra rb)
         (store-result vd +rv-t0+)))

      ((#.+op-sub+ #.+op-sub-checked+ #.+op-subs+)
       ;; :SUBS shares this clause.  :adds/:subs are "arithmetic that also sets
       ;; the overflow flag", for a following :bvs to branch on.  The result
       ;; they compute is identical to :add/:sub -- translate-i386 uses the
       ;; very same code for both pairs -- so the value is right here too.
       ;; What is NOT provided is :bvs, which still traps: these back ends do
       ;; not promote on overflow (see the :add-checked note), so a :bvs that
       ;; silently fell through would be a quiet wrong answer rather than a
       ;; visible gap.  RISC-V has no condition flags at all, so a faithful
       ;; :bvs there needs the operands, not a flag -- that is a real design
       ;; question, and it should stay loud until someone answers it.
       ;; :SUB-CHECKED shares this clause.  The checked opcodes mean "tagged
       ;; arithmetic that promotes to a bignum on overflow"; implementing the
       ;; promotion needs the generic-arith slow path, which these back ends do
       ;; not have yet.  Falling back to plain WRAPPING arithmetic is exactly
       ;; what translate-x64 and translate-i386 do when a module has no
       ;; generic-arith entry (see *i386-checked-arith-slowpath*), so this is
       ;; the documented degrade rather than a new invention -- and it is a
       ;; large step up from the previous behaviour, which was to trap.

       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-sub buf +rv-t0+ ra rb)
         (store-result vd +rv-t0+)))

      ((#.+op-mul+ #.+op-mul-checked+)
       ;; :MUL-CHECKED shares this clause.  The checked opcodes mean "tagged
       ;; arithmetic that promotes to a bignum on overflow"; implementing the
       ;; promotion needs the generic-arith slow path, which these back ends do
       ;; not have yet.  Falling back to plain WRAPPING arithmetic is exactly
       ;; what translate-x64 and translate-i386 do when a module has no
       ;; generic-arith entry (see *i386-checked-arith-slowpath*), so this is
       ;; the documented degrade rather than a new invention -- and it is a
       ;; large step up from the previous behaviour, which was to trap.

       ;; Tagged multiply: untag one operand first.
       ;; (a<<1) * (b>>1) = a*b << 1 (preserves single tag bit)
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t0+ ra 1)       ; untag first operand
         (rv-emit-mul buf +rv-t0+ +rv-t0+ rb)  ; multiply (result has one tag bit)
         (store-result vd +rv-t0+)))

      (#.+op-mul26lo+
       ;; Low 26 bits of untag(Va)*untag(Vb), tagged
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t0+ ra 1)        ; untag a
         (rv-emit-srai buf +rv-t1+ rb 1)        ; untag b
         (rv-emit-mul buf +rv-t0+ +rv-t0+ +rv-t1+) ; 64-bit result
         ;; Mask to 26 bits (the low 26 of the product are in the low word
         ;; on both widths, so this arm needs no high half)
         (rv-emit-slli buf +rv-t0+ +rv-t0+ (rv-mask26-shift))
         (rv-emit-srli buf +rv-t0+ +rv-t0+ (rv-mask26-shift))
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)   ; retag
         (store-result vd +rv-t0+)))

      (#.+op-mul26hi+
       ;; Bits 26+ of untag(Va)*untag(Vb), tagged
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t0+ ra 1)        ; untag a
         (rv-emit-srai buf +rv-t1+ rb 1)        ; untag b
         ;; Bits 26+ need the FULL product.  On RV64 one MUL holds it; on RV32
         ;; the product of two 26-bit operands is up to 52 bits, so the high
         ;; half must be brought down -- MULHU is what makes this arm correct
         ;; on 32 bits rather than silently truncated.
         (if *riscv-64-bit*
             (progn
               (rv-emit-mul buf +rv-t0+ +rv-t0+ +rv-t1+)
               (rv-emit-srli buf +rv-t0+ +rv-t0+ 26))
             (progn
               (rv-emit-mulhu buf +rv-t2+ +rv-t0+ +rv-t1+)  ; high 32
               (rv-emit-mul buf +rv-t0+ +rv-t0+ +rv-t1+)    ; low 32
               (rv-emit-srli buf +rv-t0+ +rv-t0+ 26)
               (rv-emit-slli buf +rv-t2+ +rv-t2+ 6)
               (rv-emit-or buf +rv-t0+ +rv-t0+ +rv-t2+)))
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)   ; retag
         (store-result vd +rv-t0+)))

      (#.+op-div+
       ;; Tagged divide: untag both, divide, retag
       ;; (a>>1) / (b>>1) then <<1
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t0+ ra 1)        ; untag a
         (rv-emit-srai buf +rv-t1+ rb 1)        ; untag b
         (rv-emit-div buf +rv-t0+ +rv-t0+ +rv-t1+) ; divide
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)   ; retag
         (store-result vd +rv-t0+)))

      (#.+op-mod+
       ;; Tagged mod: untag both, remainder, retag
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t0+ ra 1)
         (rv-emit-srai buf +rv-t1+ rb 1)
         (rv-emit-rem buf +rv-t0+ +rv-t0+ +rv-t1+)
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)
         (store-result vd +rv-t0+)))

      (#.+op-neg+
       ;; Negate tagged: sub from 0 preserves tag (0 - (v<<1) = (-v)<<1)
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         (rv-emit-neg buf +rv-t0+ rs)
         (store-result vd +rv-t0+)))

      (#.+op-inc+
       ;; Tagged increment: add 2 (one fixnum = shift 1, so +1 is +2 in tagged)
       (let* ((vd (vreg 0))
              (rd (resolve vd)))
         (rv-emit-addi buf rd rd 2)))

      (#.+op-dec+
       ;; Tagged decrement: sub 2
       (let* ((vd (vreg 0))
              (rd (resolve vd)))
         (rv-emit-addi buf rd rd -2)))

      ;; ---- Bitwise ----
      (#.+op-and+
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-and buf +rv-t0+ ra rb)
         (store-result vd +rv-t0+)))

      (#.+op-or+
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-or buf +rv-t0+ ra rb)
         (store-result vd +rv-t0+)))

      (#.+op-xor+
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-xor buf +rv-t0+ ra rb)
         (store-result vd +rv-t0+)))

      (#.+op-shl+
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (amt (vreg 2)))
         (rv-emit-slli buf +rv-t0+ rs amt)
         (store-result vd +rv-t0+)))

      (#.+op-shr+
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (amt (vreg 2)))
         (rv-emit-srli buf +rv-t0+ rs amt)
         (store-result vd +rv-t0+)))

      (#.+op-sar+
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (amt (vreg 2)))
         (rv-emit-srai buf +rv-t0+ rs amt)
         (store-result vd +rv-t0+)))

      (#.+op-shlv+
       ;; (shlv Vd Vs Vc) — shift left by register
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (rc (resolve2 (vreg 2))))
         (rv-emit-sll buf +rv-t0+ rs rc)
         (store-result vd +rv-t0+)))

      (#.+op-sarv+
       ;; (sarv Vd Vs Vc) — arithmetic shift right by register
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (rc (resolve2 (vreg 2))))
         (rv-emit-sra buf +rv-t0+ rs rc)
         (store-result vd +rv-t0+)))

      (#.+op-ldb+
       ;; Bit field extract: (src >> pos) & ((1 << size) - 1)
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (pos (vreg 2))
              (size (vreg 3))
              (mask (1- (ash 1 size))))
         (rv-emit-srli buf +rv-t0+ rs pos)
         (if (<= mask 2047)
             (rv-emit-andi buf +rv-t0+ +rv-t0+ mask)
             (progn
               (rv-emit-li buf +rv-t1+ mask)
               (rv-emit-and buf +rv-t0+ +rv-t0+ +rv-t1+)))
         (store-result vd +rv-t0+)))

      ;; ---- Comparison ----
      (#.+op-cmp+
       ;; Save both operands for subsequent conditional branch
       (let ((ra (resolve (vreg 0)))
             (rb (resolve2 (vreg 1))))
         ;; Copy to dedicated comparison registers so branches can use them
         (rv-emit-mv buf *rv-last-cmp-rs1* ra)
         (rv-emit-mv buf *rv-last-cmp-rs2* rb)))

      (#.+op-test+
       ;; AND and save result for branch
       (let ((ra (resolve (vreg 0)))
             (rb (resolve2 (vreg 1))))
         (rv-emit-and buf *rv-last-cmp-rs1* ra rb)
         (rv-emit-mv buf *rv-last-cmp-rs2* +rv-x0+)))

      ;; ---- Branch ----
      (#.+op-br+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-j buf native-off)))

      (#.+op-beq+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-beq buf *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-bne+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-bne buf *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-blt+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-blt buf *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-bge+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-bge buf *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-ble+
       ;; BLE a,b = BGE b,a (swap operands)
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-bge buf *rv-last-cmp-rs2* *rv-last-cmp-rs1* native-off)))

      (#.+op-bgt+
       ;; BGT a,b = BLT b,a (swap operands)
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-blt buf *rv-last-cmp-rs2* *rv-last-cmp-rs1* native-off)))

      (#.+op-bnull+
       ;; Branch if register equals VN (NIL)
       (let* ((rs (resolve (vreg 0)))
              (mvm-offset (vreg 1))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-beq buf rs +rv-s10+ native-off)))

      (#.+op-bnnull+
       ;; Branch if register is not VN (NIL)
       (let* ((rs (resolve (vreg 0)))
              (mvm-offset (vreg 1))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-bne buf rs +rv-s10+ native-off)))

      ;; ---- List operations ----
      (#.+op-car+
       ;; Cons cell layout: [car|cdr] with tag bit 0 = 1 for cons.
       ;; ld rd, -1(rs)  -- untag cons pointer (subtract tag), load car
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         (rv-emit-load-word buf +rv-t0+ rs -1)
         (store-result vd +rv-t0+)))

      (#.+op-cdr+
       ;; ld rd, ws-1(rs)  -- untag cons (-1), offset to cdr (+ws)
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         (rv-emit-load-word buf +rv-t0+ rs (1- (rv-word-size)))
         (store-result vd +rv-t0+)))

      (#.+op-cons+
       ;; Allocate cons from bump pointer VA (s8):
       ;; sd car, 0(VA); sd cdr, 8(VA); addi result, VA, 1; addi VA, VA, 16
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))          ; car
              (rb (resolve2 (vreg 2))))        ; cdr
         (rv-emit-store-word buf ra +rv-s8+ 0)          ; store car at alloc ptr
         (rv-emit-store-word buf rb +rv-s8+ (rv-word-size))  ; cdr at alloc ptr + word
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 1)   ; tag: cons tag = 1
         (rv-emit-addi buf +rv-s8+ +rv-s8+ (rv-granule))  ; bump alloc pointer
         (store-result vd +rv-t0+)))

      (#.+op-setcar+
       ;; Store value into car slot: sd vs, -1(vd)
       (let ((rd (resolve (vreg 0)))
             (rs (resolve2 (vreg 1))))
         (rv-emit-store-word buf rs rd -1)))

      (#.+op-setcdr+
       ;; Store value into cdr slot: sd vs, ws-1(vd)
       (let ((rd (resolve (vreg 0)))
             (rs (resolve2 (vreg 1))))
         (rv-emit-store-word buf rs rd (1- (rv-word-size)))))

      (#.+op-consp+
       ;; Check if lowest bit of tag is 1 (cons tag)
       ;; andi t0, vs, 0x07; slti t0, t0, 2; xori t0, t0, 1 ... no, simpler:
       ;; andi t0, vs, 0x07; addi t1, x0, 1; beq/set
       ;; Result: tagged boolean. We return VN (NIL) for false, or a non-NIL for true.
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         ;; Extract low 3 tag bits, compare with cons tag (1)
         (rv-emit-andi buf +rv-t0+ rs #x07)
         (rv-emit-addi buf +rv-t1+ +rv-x0+ 1)    ; cons tag = 1
         (rv-emit-sub buf +rv-t0+ +rv-t0+ +rv-t1+)
         (rv-emit-seqz buf +rv-t0+ +rv-t0+)        ; 1 if equal (is cons)
         ;; Convert to tagged boolean: 0 -> NIL, 1 -> tagged T
         ;; Use conditional move: if t0=0 -> VN, else -> tagged T value
         (rv-emit-beq buf +rv-t0+ +rv-x0+ 8)      ; skip next if not cons
         (rv-emit-li buf +rv-t0+ #x16)              ; tagged T (0x0B << 1 | tag...)
         ;; Actually, simplify: store raw boolean result as fixnum
         (store-result vd +rv-t0+)))

      (#.+op-atom+
       ;; Atom = not consp. Same as consp but inverted.
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         (rv-emit-andi buf +rv-t0+ rs #x07)
         (rv-emit-addi buf +rv-t1+ +rv-x0+ 1)
         (rv-emit-sub buf +rv-t0+ +rv-t0+ +rv-t1+)
         (rv-emit-snez buf +rv-t0+ +rv-t0+)        ; 1 if NOT cons
         (store-result vd +rv-t0+)))

      ;; ---- Object operations ----
      (#.+op-alloc-obj+
       ;; Allocate object: size in imm16 (words), subtag in imm8
       ;; Build header: (size << 8) | subtag, store at VA, tag pointer
       (let* ((vd (vreg 0))
              (size-words (vreg 1))
              (subtag (vreg 2))
              ;; Align to the granule to keep the cons alloc pointer aligned
              (g (rv-granule))
              (total-bytes (logand (+ (* (1+ size-words) (rv-word-size)) (1- g))
                                   (lognot (1- g)))))
         ;; Build and store header
         (rv-emit-li buf +rv-t0+ (logior (ash size-words 8) subtag))
         (rv-emit-store-word buf +rv-t0+ +rv-s8+ 0)
         ;; Tag pointer: object tag = 2
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 2)
         ;; Bump alloc pointer
         (rv-emit-addi buf +rv-s8+ +rv-s8+ total-bytes)
         (store-result vd +rv-t0+)))

      (#.+op-obj-ref+
       ;; Load object slot: untag (-2), offset by (1+idx)*8 to skip header
       (let* ((vd (vreg 0))
              (vobj (vreg 1))
              (idx (vreg 2)))
         (if (= vobj +vreg-vfp+)
             ;; Frame slot access: use safe FP-relative offset below spill area
             (let ((off (+ +rv-frame-slot-base+ (* idx (- (rv-word-size))))))
               (rv-emit-load-word buf +rv-t0+ +rv-fp+ off))
             ;; Normal object slot access
             (let* ((robj (resolve vobj))
                    (offset (- (* (1+ idx) (rv-word-size)) 2)))  ; (1+idx)*word - tag
               (if (and (>= offset -2048) (<= offset 2047))
                   (rv-emit-load-word buf +rv-t0+ robj offset)
                   (progn
                     (rv-emit-li buf +rv-t0+ offset)
                     (rv-emit-add buf +rv-t0+ robj +rv-t0+)
                     (rv-emit-load-word buf +rv-t0+ +rv-t0+ 0)))))
         (store-result vd +rv-t0+)))

      (#.+op-obj-set+
       ;; Store object slot
       (let* ((vobj (vreg 0))
              (idx (vreg 1))
              (rs (resolve2 (vreg 2))))
         (if (= vobj +vreg-vfp+)
             ;; Frame slot store: use safe FP-relative offset below spill area
             (let ((off (+ +rv-frame-slot-base+ (* idx (- (rv-word-size))))))
               (rv-emit-store-word buf rs +rv-fp+ off))
             ;; Normal object slot store
             (let* ((robj (resolve vobj))
                    (offset (- (* (1+ idx) (rv-word-size)) 2)))
               (if (and (>= offset -2048) (<= offset 2047))
                   (rv-emit-store-word buf rs robj offset)
                   (progn
                     (rv-emit-li buf +rv-t0+ offset)
                     (rv-emit-add buf +rv-t0+ robj +rv-t0+)
                     (rv-emit-store-word buf rs +rv-t0+ 0)))))))

      (#.+op-obj-tag+
       ;; Extract 3-bit tag from pointer
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         (rv-emit-andi buf +rv-t0+ rs #x07)
         (store-result vd +rv-t0+)))

      (#.+op-obj-subtag+
       ;; Extract 8-bit subtag from header word: load header, andi 0xFF
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         ;; Untag pointer (object tag=2), load header at offset 0
         (rv-emit-load-word buf +rv-t0+ rs -2)
         (rv-emit-andi buf +rv-t0+ +rv-t0+ #xFF)
         (store-result vd +rv-t0+)))

      ;; ---- Memory (raw) ----
      (#.+op-load+
       ;; MASK +WIDTH-TLS-BIT+ (widths 4..7 are 0..3 plus "this is a per-thread
       ;; window slot").  RISC-V has no per-thread window, so the bit is
       ;; dropped -- exactly as translate-i386 and translate-aarch64 do.  It is
       ;; masked rather than ignored because an unmatched CASE here emits
       ;; NOTHING, and a load that silently emits nothing is a wrong value.
       ;;
       ;; Width 3 is :u64.  On RV32 it degrades to a 32-bit access, which is
       ;; what the other 32-bit back ends do; the compiler splits a promoting
       ;; width into halves on a 30-bit tower anyway.
       (let* ((vd (vreg 0))
              (raddr (resolve (vreg 1)))
              (width (logand (vreg 2) 3)))
         (case width
           (0 (rv-emit-lbu buf +rv-t0+ raddr 0))   ; u8
           (1 (rv-emit-lh buf +rv-t0+ raddr 0))     ; u16
           (2 (rv-emit-lw buf +rv-t0+ raddr 0))     ; u32
           (3 (rv-emit-load-word buf +rv-t0+ raddr 0)))    ; u64 (one word)
         (store-result vd +rv-t0+)))

      (#.+op-store+
       (let* ((raddr (resolve (vreg 0)))
              (rs (resolve2 (vreg 1)))
              (width (logand (vreg 2) 3)))   ; +WIDTH-TLS-BIT+: see op-load
         (case width
           (0 (rv-emit-sb buf rs raddr 0))
           (1 (rv-emit-sh buf rs raddr 0))
           (2 (rv-emit-sw buf rs raddr 0))
           (3 (rv-emit-store-word buf rs raddr 0)))))

      (#.+op-fence+
       ;; Full memory barrier: fence iorw, iorw
       (rv-emit-fence buf #xF #xF))

      ;; ---- Function Calling ----
      (#.+op-call+
       ;; Call function by index. Look up native offset in function-table.
       (let* ((target-idx (vreg 0))
              (target-offset (if function-table
                                 (gethash target-idx function-table)
                                 0))
              (rel-offset (if target-offset
                              (- target-offset (rv-current-offset buf))
                              0)))
         ;; For nearby calls, use jal directly; for far calls, use auipc+jalr
         (if (and (>= rel-offset -1048576) (<= rel-offset 1048575))
             (rv-emit-jal buf +rv-ra+ rel-offset)
             (rv-emit-call buf rel-offset))))

      (#.+op-fn-addr+
       ;; (fn-addr Vd target:imm32) — the native address of a function, TAGGED
       ;; with +tag-function+ (3), which is how funcall dispatch and FUNCTIONP
       ;; tell it from a cons (tag 1) or an object (tag 9).
       ;;
       ;; RISC-V has AUIPC, so this is a clean two-instruction PC-relative
       ;; materialisation -- no literal pool and none of i386's call/pop dance.
       ;; AUIPC's PC is the address of the AUIPC ITSELF, so the displacement is
       ;; measured from this instruction's own offset.
       ;;
       ;; The 0x800 in the split is the standard AUIPC+ADDI correction: ADDI's
       ;; immediate is SIGN-extended, so when lo12 >= 0x800 the high part must be
       ;; pre-incremented or the result is 4096 too low.
       ;;
       ;; OR-3 IS EXACT HERE because every instruction this back end emits is
       ;; four bytes, so a function's native offset is always a multiple of 4 and
       ;; the low two bits are free.  :call-ind below subtracts the same 3.
       ;;
       ;; Fixed three instructions, so the two-pass size measurement is stable.
       (let* ((vd (vreg 0))
              (target-idx (vreg 1))
              (target-offset (if function-table
                                 (gethash target-idx function-table)
                                 0))
              (rel (if target-offset
                       (- target-offset (rv-current-offset buf))
                       0))
              (hi20 (ash (+ rel #x800) -12))
              (lo12 (- rel (ash hi20 12))))
         (rv-emit-auipc buf +rv-t0+ (logand hi20 #xFFFFF))
         (rv-emit-addi buf +rv-t0+ +rv-t0+ (logand lo12 #xFFF))
         (rv-emit-ori buf +rv-t0+ +rv-t0+ +tag-function+)
         (store-result vd +rv-t0+)))

      (#.+op-li-const+
       ;; (li-const Vd idx) — load the TAGGED address of constant-pool slot IDX.
       ;;
       ;; The address is not known until the image is assembled, so this emits a
       ;; FIXED-SIZE placeholder and records the site; see
       ;; *RISCV-LI-CONST-PATCHES* for why the shape is a shift-and-add chain
       ;; rather than the obvious LUI+ADDI.
       ;;
       ;; Five instructions, ALWAYS, so the two-pass size measurement is stable
       ;; and the patcher knows exactly where the three ADDI words are: at
       ;; +0, +8 and +16 from the start of the sequence.
       (let* ((vd (vreg 0))
              (idx (vreg 1))
              (start (rv-current-offset buf)))
         ;; V = (a << 21) | (b << 10) | c, i.e. ((a << 11) | b) << 10 | c.
         ;; THE SECOND SHIFT IS 10, NOT 11 — the chunk widths are 11/11/10 and
         ;; each SLLI must be the width of everything still to come, not the
         ;; width of the chunk just added.
         (rv-emit-addi buf +rv-t0+ +rv-x0+ 0)      ; chunk a  <- patched
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 11)
         (rv-emit-addi buf +rv-t0+ +rv-t0+ 0)      ; chunk b  <- patched
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 10)
         (rv-emit-addi buf +rv-t0+ +rv-t0+ 0)      ; chunk c  <- patched
         (push (cons start idx) *riscv-li-const-patches*)
         (store-result vd +rv-t0+)))

      (#.+op-call-ind+
       ;; Indirect call through a register holding a TAGGED function pointer
       ;; (tag 3 — see :fn-addr).  The tag must be stripped before the jump.
       ;;
       ;; THIS IS HALF OF A PAIR.  Until :fn-addr existed here, nothing on this
       ;; target produced a tagged function pointer, so the bare JALR was
       ;; accidentally right; with :fn-addr it would jump THREE BYTES into the
       ;; function, which on RISC-V is not even an instruction boundary.
       ;;
       ;; Stripping into t1 rather than in place: RESOLVE may hand back a live
       ;; vreg, and a funcall inside a loop would otherwise untag the caller's
       ;; own copy of the function pointer once per iteration.
       (let ((rs (resolve (vreg 0))))
         (rv-emit-addi buf +rv-t1+ rs (- +tag-function+))
         (rv-emit-jalr buf +rv-ra+ +rv-t1+ 0)))

      (#.+op-ret+
       (rv-emit-epilogue buf +rv-local-frame-size+))

      (#.+op-tailcall+
       ;; Tail call: restore frame, then jump (not jal)
       (let* ((target-idx (vreg 0))
              (target-offset (if function-table
                                 (gethash target-idx function-table)
                                 0))
              (total-frame (+ +rv-local-frame-size+ 112)))
         ;; Restore callee-saved registers
         (rv-emit-load-word buf +rv-ra+  +rv-sp+ (- total-frame 8))
         (rv-emit-load-word buf +rv-fp+  +rv-sp+ (- total-frame 16))
         (rv-emit-load-word buf +rv-s1+  +rv-sp+ (- total-frame 24))
         (rv-emit-load-word buf +rv-s2+  +rv-sp+ (- total-frame 32))
         (rv-emit-load-word buf +rv-s3+  +rv-sp+ (- total-frame 40))
         (rv-emit-load-word buf +rv-s4+  +rv-sp+ (- total-frame 48))
         (rv-emit-load-word buf +rv-s5+  +rv-sp+ (- total-frame 56))
         (rv-emit-load-word buf +rv-s6+  +rv-sp+ (- total-frame 64))
         (rv-emit-load-word buf +rv-s7+  +rv-sp+ (- total-frame 72))
         (rv-emit-load-word buf +rv-s8+  +rv-sp+ (- total-frame 80))
         (rv-emit-load-word buf +rv-s9+  +rv-sp+ (- total-frame 88))
         (rv-emit-load-word buf +rv-s10+ +rv-sp+ (- total-frame 96))
         (rv-emit-load-word buf +rv-s11+ +rv-sp+ (- total-frame 104))
         ;; Deallocate frame
         (rv-emit-addi buf +rv-sp+ +rv-sp+ total-frame)
         ;; Jump to target
         (let ((rel-offset (if target-offset
                               (- target-offset (rv-current-offset buf))
                               0)))
           (rv-emit-j buf rel-offset))))

      ;; ---- GC and Allocation ----
      (#.+op-alloc-cons+
       ;; Bump-allocate a cons cell into Vd (just reserve 16 bytes)
       ;; addi vd, VA, 1 (tag); addi VA, VA, 16
       (let ((vd (vreg 0)))
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 1)    ; tagged cons pointer
         (rv-emit-addi buf +rv-s8+ +rv-s8+ (rv-granule))  ; bump alloc
         (store-result vd +rv-t0+)))

      ((#.+op-gc-check+ #.+op-gc-check-n+ #.+op-gc-check-r+)
       ;; :GC-CHECK-N and :GC-CHECK-R share this clause.  They carry an
       ;; allocation SIZE (a constant, or a runtime value) so a back end can
       ;; check `VA + n < VL` rather than `VA < VL`.  None of x64, i386 or
       ;; aarch64 uses the size either -- translate-i386 routes all three to
       ;; the same plain VA-vs-VL comparison -- so doing the same here matches
       ;; the reference back ends exactly.  What it replaces is worse than an
       ;; imprecise check: RISC-V/PPC/68k TRAPPED on these opcodes, and
       ;; make-array emits :gc-check-n, so no array could be allocated at all.
       ;; Compare VA (alloc pointer) with VL (alloc limit)
       ;; If VA >= VL, call GC
       ;; blt VA, VL, +8  (skip ecall if below limit)
       ;; ecall            (invoke GC through SBI or trap handler)
       (rv-emit-blt buf +rv-s8+ +rv-s9+ 8)    ; skip if VA < VL
       (rv-emit-ecall buf))                     ; trigger GC

      (#.+op-write-barrier+
       ;; Mark card table dirty for the object.
       ;; For now: no-op (placeholder for generational GC)
       (rv-emit-nop buf))

      ;; ---- Actor / Concurrency ----
      (#.+op-save-ctx+
       ;; Save all callee-saved registers to the stack frame
       ;; This is used for actor context switching
       (rv-emit-addi buf +rv-sp+ +rv-sp+ -96)  ; 12 regs * 8 bytes
       (rv-emit-store-word buf +rv-ra+  +rv-sp+ 88)
       (rv-emit-store-word buf +rv-s0+  +rv-sp+ 80)
       (rv-emit-store-word buf +rv-s1+  +rv-sp+ 72)
       (rv-emit-store-word buf +rv-s2+  +rv-sp+ 64)
       (rv-emit-store-word buf +rv-s3+  +rv-sp+ 56)
       (rv-emit-store-word buf +rv-s4+  +rv-sp+ 48)
       (rv-emit-store-word buf +rv-s5+  +rv-sp+ 40)
       (rv-emit-store-word buf +rv-s6+  +rv-sp+ 32)
       (rv-emit-store-word buf +rv-s7+  +rv-sp+ 24)
       (rv-emit-store-word buf +rv-s8+  +rv-sp+ 16)
       (rv-emit-store-word buf +rv-s9+  +rv-sp+ 8)
       (rv-emit-store-word buf +rv-s10+ +rv-sp+ 0))

      (#.+op-restore-ctx+
       ;; Restore all callee-saved registers from the stack frame
       (rv-emit-load-word buf +rv-ra+  +rv-sp+ 88)
       (rv-emit-load-word buf +rv-s0+  +rv-sp+ 80)
       (rv-emit-load-word buf +rv-s1+  +rv-sp+ 72)
       (rv-emit-load-word buf +rv-s2+  +rv-sp+ 64)
       (rv-emit-load-word buf +rv-s3+  +rv-sp+ 56)
       (rv-emit-load-word buf +rv-s4+  +rv-sp+ 48)
       (rv-emit-load-word buf +rv-s5+  +rv-sp+ 40)
       (rv-emit-load-word buf +rv-s6+  +rv-sp+ 32)
       (rv-emit-load-word buf +rv-s7+  +rv-sp+ 24)
       (rv-emit-load-word buf +rv-s8+  +rv-sp+ 16)
       (rv-emit-load-word buf +rv-s9+  +rv-sp+ 8)
       (rv-emit-load-word buf +rv-s10+ +rv-sp+ 0)
       (rv-emit-addi buf +rv-sp+ +rv-sp+ 96))

      (#.+op-yield+
       ;; Preemption check: read mstatus or timer, branch to scheduler if needed
       ;; For now, emit ecall as yield trap
       (rv-emit-addi buf +rv-a7+ +rv-x0+ #x0A)  ; yield syscall number
       (rv-emit-ecall buf))

      (#.+op-atomic-xchg+
       ;; Atomic exchange: amoswap.d rd, rs, (raddr) with aq+rl
       (let* ((vd (vreg 0))
              (raddr (resolve (vreg 1)))
              (rs (resolve2 (vreg 2))))
         (rv-emit-amoswap-d buf +rv-t0+ rs raddr #x3) ; aq=1, rl=1
         (store-result vd +rv-t0+)))

      ;; ---- System / Platform ----
      (#.+op-io-read+
       ;; RISC-V has memory-mapped I/O, so treat port as address
       ;; Load port address into t0, then load from it
       (let* ((vd (vreg 0))
              (port (vreg 1))
              (width (vreg 2)))
         (rv-emit-li buf +rv-t0+ port)
         (case width
           (0 (rv-emit-lbu buf +rv-t1+ +rv-t0+ 0))
           (1 (rv-emit-lh buf +rv-t1+ +rv-t0+ 0))
           (2 (rv-emit-lw buf +rv-t1+ +rv-t0+ 0))
           (3 (rv-emit-load-word buf +rv-t1+ +rv-t0+ 0)))
         (store-result vd +rv-t1+)))

      (#.+op-io-write+
       ;; Memory-mapped I/O write
       (let* ((port (vreg 0))
              (rs (resolve (vreg 1)))
              (width (vreg 2)))
         (rv-emit-li buf +rv-t0+ port)
         (case width
           (0 (rv-emit-sb buf rs +rv-t0+ 0))
           (1 (rv-emit-sh buf rs +rv-t0+ 0))
           (2 (rv-emit-sw buf rs +rv-t0+ 0))
           (3 (rv-emit-store-word buf rs +rv-t0+ 0)))))

      (#.+op-halt+
       ;; WFI loop: wfi; j -4 (loop back to wfi)
       (rv-emit-wfi buf)
       (rv-emit-j buf -4))

      (#.+op-cli+
       ;; Disable interrupts: clear MIE bit in mstatus
       ;; csrci mstatus, 0x8 -- but csrci uses zimm, so:
       ;; li t0, 8; csrrc x0, mstatus, t0
       (rv-emit-addi buf +rv-t0+ +rv-x0+ 8)
       (rv-emit-csrrc buf +rv-x0+ #x300 +rv-t0+))  ; mstatus = 0x300

      (#.+op-sti+
       ;; Enable interrupts: set MIE bit in mstatus
       (rv-emit-addi buf +rv-t0+ +rv-x0+ 8)
       (rv-emit-csrrs buf +rv-x0+ #x300 +rv-t0+))

      (#.+op-percpu-ref+
       ;; Read per-CPU data: use tp (thread pointer) as base
       (let* ((vd (vreg 0))
              (offset (vreg 1)))
         (if (and (>= offset -2048) (<= offset 2047))
             (rv-emit-load-word buf +rv-t0+ +rv-tp+ offset)
             (progn
               (rv-emit-li buf +rv-t0+ offset)
               (rv-emit-add buf +rv-t0+ +rv-tp+ +rv-t0+)
               (rv-emit-load-word buf +rv-t0+ +rv-t0+ 0)))
         (store-result vd +rv-t0+)))

      (#.+op-percpu-set+
       ;; Write per-CPU data
       (let* ((offset (vreg 0))
              (rs (resolve (vreg 1))))
         (if (and (>= offset -2048) (<= offset 2047))
             (rv-emit-store-word buf rs +rv-tp+ offset)
             (progn
               (rv-emit-li buf +rv-t0+ offset)
               (rv-emit-add buf +rv-t0+ +rv-tp+ +rv-t0+)
               (rv-emit-store-word buf rs +rv-t0+ 0)))))

      ;; ---- Arrays ----
      ;; Object layout, as alloc-obj/obj-ref already use it on this target:
      ;; tag 2, header word at obj-2, element k at obj-2 + (1+k)*8.  The index
      ;; arrives TAGGED (2k), so k*8 == tagged*4 and the element sits at
      ;; obj + tagged*4 + 6 -- which is 8-aligned, since obj is raw+2.
      (#.+op-alloc-array+
       ;; (alloc-array Vd Vcount) — Vcount is UNTAGGED (the compiler SAR'd it).
       (let* ((vd (vreg 0))
              (rc (resolve (vreg 1) +rv-t2+)))
         ;; header = (count << 8) | #x32   (array subtag, as on i386/arm32)
         (rv-emit-slli buf +rv-t0+ rc 8)
         (rv-emit-addi buf +rv-t0+ +rv-t0+ #x32)
         (rv-emit-store-word buf +rv-t0+ +rv-s8+ 0)
         ;; bytes = align-to-granule((count + 1) * word)
         (rv-emit-addi buf +rv-t1+ rc 1)
         (rv-emit-slli buf +rv-t1+ +rv-t1+ (rv-word-shift))
         (rv-emit-addi buf +rv-t1+ +rv-t1+ (1- (rv-granule)))
         (rv-emit-andi buf +rv-t1+ +rv-t1+ (- (rv-granule)))
         ;; result = VA + 2 (VA stays granule-aligned -- 16 on RV64, 8 on RV32 --
         ;; so tag 2 is exact at both widths)
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 2)
         (rv-emit-add buf +rv-s8+ +rv-s8+ +rv-t1+)
         (store-result vd +rv-t0+)))

      (#.+op-aref+
       (let* ((vd (vreg 0))
              (robj (resolve (vreg 1)))
              (ridx (resolve2 (vreg 2))))
         (rv-emit-slli buf +rv-t2+ ridx (rv-index-shift))
         (rv-emit-add buf +rv-t2+ +rv-t2+ robj)
         (rv-emit-load-word buf +rv-t2+ +rv-t2+ (- (rv-word-size) 2))
         (store-result vd +rv-t2+)))

      (#.+op-aset+
       ;; (aset Vobj Vidx Vs).  The value is materialised BEFORE t1 is reused
       ;; as the address, and neither resolve target can be t1.
       (let* ((robj (resolve (vreg 0)))
              (rval (rv-vreg-or-load buf (vreg 2) +rv-t2+))
              (ridx (rv-vreg-or-load buf (vreg 1) +rv-t1+)))
         (rv-emit-slli buf +rv-t1+ ridx (rv-index-shift))
         (rv-emit-add buf +rv-t1+ +rv-t1+ robj)
         (rv-emit-store-word buf rval +rv-t1+ (- (rv-word-size) 2))))

      (#.+op-array-len+
       ;; count = (header >> 8) & 0xFFFFFF, returned TAGGED.
       (let* ((vd (vreg 0))
              (robj (resolve (vreg 1))))
         (rv-emit-load-word buf +rv-t0+ robj -2)
         (rv-emit-srli buf +rv-t0+ +rv-t0+ 8)
         (rv-emit-slli buf +rv-t0+ +rv-t0+ (rv-mask24-shift))  ; mask to 24 bits
         (rv-emit-srli buf +rv-t0+ +rv-t0+ (rv-mask24-shift))
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)    ; tag as fixnum
         (store-result vd +rv-t0+)))

      ;; ---- Byte vectors and strings ----
      ;; Payload starts right after the header word, at obj - 2 + 8 = obj + 6.
      ;; Mirrors translate-i386's shapes exactly, including which operands
      ;; arrive TAGGED: :alloc-u8 takes a tagged count and untags it here,
      ;; while :alloc-string's count has already been SAR'd by the compiler
      ;; (see the :alloc-array/:alloc-string note in cross.lisp).
      (#.+op-alloc-u8+
       (let* ((vd (vreg 0))
              (rc (resolve (vreg 1) +rv-t2+)))
         (rv-emit-srai buf +rv-t2+ rc 1)              ; N = count >> 1
         ;; header = (N << 8) | #x11   (u8-vector subtag)
         (rv-emit-slli buf +rv-t0+ +rv-t2+ 8)
         (rv-emit-addi buf +rv-t0+ +rv-t0+ #x11)
         (rv-emit-store-word buf +rv-t0+ +rv-s8+ 0)
         ;; bytes = align-to-granule(N + word) — header word plus the byte payload
         (rv-emit-addi buf +rv-t1+ +rv-t2+ (rv-word-size))
         (rv-emit-addi buf +rv-t1+ +rv-t1+ (1- (rv-granule)))
         (rv-emit-andi buf +rv-t1+ +rv-t1+ (- (rv-granule)))
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 2)
         (rv-emit-add buf +rv-s8+ +rv-s8+ +rv-t1+)
         (store-result vd +rv-t0+)))

      (#.+op-alloc-string+
       ;; One character CODE per WORD, as on every other target.
       (let* ((vd (vreg 0))
              (rc (resolve (vreg 1) +rv-t2+)))
         (rv-emit-slli buf +rv-t0+ rc 8)
         (rv-emit-addi buf +rv-t0+ +rv-t0+ #x31)      ; string subtag
         (rv-emit-store-word buf +rv-t0+ +rv-s8+ 0)
         (rv-emit-addi buf +rv-t1+ rc 1)
         (rv-emit-slli buf +rv-t1+ +rv-t1+ (rv-word-shift))
         (rv-emit-addi buf +rv-t1+ +rv-t1+ (1- (rv-granule)))
         (rv-emit-andi buf +rv-t1+ +rv-t1+ (- (rv-granule)))
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 2)
         (rv-emit-add buf +rv-s8+ +rv-s8+ +rv-t1+)
         (store-result vd +rv-t0+)))

      (#.+op-u8-ref+
       ;; (u8-ref Vd Varr Vidx) — Vidx TAGGED, result TAGGED.
       (let* ((vd (vreg 0))
              (rarr (resolve (vreg 1)))
              (ridx (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t2+ ridx 1)
         (rv-emit-add buf +rv-t2+ +rv-t2+ rarr)
         (rv-emit-lbu buf +rv-t2+ +rv-t2+ (- (rv-word-size) 2))
         (rv-emit-slli buf +rv-t2+ +rv-t2+ 1)
         (store-result vd +rv-t2+)))

      (#.+op-u8-set+
       ;; (u8-set Varr Vidx Vval) — Vidx and Vval both TAGGED.
       ;; The address lands in t1 and the untagged value in t2, so neither
       ;; can be the other's input.
       (let* ((rarr (resolve (vreg 0)))
              (ridx (rv-vreg-or-load buf (vreg 1) +rv-t1+))
              (rval (rv-vreg-or-load buf (vreg 2) +rv-t2+)))
         (rv-emit-srai buf +rv-t1+ ridx 1)
         (rv-emit-add buf +rv-t1+ +rv-t1+ rarr)
         (rv-emit-srai buf +rv-t2+ rval 1)
         (rv-emit-sb buf +rv-t2+ +rv-t1+ (- (rv-word-size) 2))))

      ;; ---- System area pointers ----
      ;; A SAP is a one-slot object, subtag #x16: header (1<<8)|#x16 then the
      ;; raw address.  Header plus slot is exactly ONE GRANULE at either width.
      (#.+op-sap-new+
       (let* ((vd (vreg 0))
              (raddr (resolve (vreg 1) +rv-t2+)))
         (rv-emit-li buf +rv-t0+ #x116)
         (rv-emit-store-word buf +rv-t0+ +rv-s8+ 0)
         (rv-emit-store-word buf raddr +rv-s8+ (rv-word-size))
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 2)
         (rv-emit-addi buf +rv-s8+ +rv-s8+ (rv-granule))
         (store-result vd +rv-t0+)))

      (#.+op-sap-addr+
       ;; Raw address out, TAGGED as a fixnum (as on x64/i386) so it can be
       ;; handed to a syscall that untags every argument.
       (let* ((vd (vreg 0))
              (rsap (resolve (vreg 1))))
         ;; Payload offset is WORD - TAG, not a constant 6: the SAP's one slot
         ;; sits immediately after its header word, and the pointer in hand is
         ;; already tag-2.  Reading 6 on RV32 reads PAST the slot and returns 0,
         ;; which is what r13-sap measured.
         (rv-emit-load-word buf +rv-t0+ rsap (- (rv-word-size) 2))
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)
         (store-result vd +rv-t0+)))

      ;; ---- Calling-convention slots ----
      ;; These used to fall into the OTHERWISE trap below, which emits
      ;; `li a7,<opcode>; ebreak`.  :set-nargs is emitted before EVERY call,
      ;; so the first function call in any image trapped -- that is why an
      ;; arithmetic-only probe ran and a two-function one did not.

      ;; nargs is stored RAW (untagged); :get-nargs tags it on the way out so
      ;; the tagged IR world can compare it against :li values.  Same
      ;; convention as i386.
      (#.+op-set-nargs+
       (let ((n (logand (vreg 0) #xFF)))
         (rv-emit-li buf +rv-t2+ n)
         (rv-emit-store-abs buf +rv-t2+ (rv-nargs-addr))))

      (#.+op-get-nargs+
       (let ((vd (vreg 0)))
         (rv-emit-load-abs buf +rv-t0+ (rv-nargs-addr))
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)   ; tag as fixnum
         (store-result vd +rv-t0+)))

      (#.+op-set-cenv+
       (let ((rs (resolve (vreg 0) +rv-t2+)))
         (rv-emit-store-abs buf rs (rv-cenv-addr))))

      (#.+op-get-cenv+
       (let ((vd (vreg 0)))
         (rv-emit-load-abs buf +rv-t0+ (rv-cenv-addr))
         (store-result vd +rv-t0+)))

      (#.+op-set-mv-count+
       ;; The count is stored TAGGED (<<1), matching i386/x64.
       (let ((tagged (ash (vreg 0) 1)))
         (rv-emit-li buf +rv-t2+ tagged)
         (rv-emit-store-abs buf +rv-t2+ +mv-count-addr+)))

      ;; ---- Unknown opcode ----
      (otherwise
       ;; Emit trap for unrecognized instruction
       (rv-emit-addi buf +rv-a7+ +rv-x0+ opcode)
       (rv-emit-ebreak buf)))))

;;; ============================================================
;;; Function Prologue / Epilogue
;;; ============================================================

(defun rv-emit-prologue (buf frame-size)
  "Emit a RISC-V function prologue.
   FRAME-SIZE is the number of bytes needed for locals/spills.
   Saves ra, fp, and callee-saved registers used by MVM."
  (let ((total-frame (+ frame-size 112)))  ; 14 callee-saved regs * 8 = 112
    ;; Allocate stack frame
    (rv-emit-addi buf +rv-sp+ +rv-sp+ (- total-frame))
    ;; Save return address and frame pointer
    (rv-emit-store-word buf +rv-ra+ +rv-sp+ (- total-frame 8))
    (rv-emit-store-word buf +rv-fp+ +rv-sp+ (- total-frame 16))
    ;; Save callee-saved registers (s1-s11 used by MVM)
    (rv-emit-store-word buf +rv-s1+  +rv-sp+ (- total-frame 24))
    (rv-emit-store-word buf +rv-s2+  +rv-sp+ (- total-frame 32))
    (rv-emit-store-word buf +rv-s3+  +rv-sp+ (- total-frame 40))
    (rv-emit-store-word buf +rv-s4+  +rv-sp+ (- total-frame 48))
    (rv-emit-store-word buf +rv-s5+  +rv-sp+ (- total-frame 56))
    (rv-emit-store-word buf +rv-s6+  +rv-sp+ (- total-frame 64))
    (rv-emit-store-word buf +rv-s7+  +rv-sp+ (- total-frame 72))
    (rv-emit-store-word buf +rv-s8+  +rv-sp+ (- total-frame 80))
    (rv-emit-store-word buf +rv-s9+  +rv-sp+ (- total-frame 88))
    (rv-emit-store-word buf +rv-s10+ +rv-sp+ (- total-frame 96))
    (rv-emit-store-word buf +rv-s11+ +rv-sp+ (- total-frame 104))
    ;; Set up frame pointer
    (rv-emit-addi buf +rv-fp+ +rv-sp+ total-frame)))

(defun rv-emit-epilogue (buf frame-size)
  "Emit a RISC-V function epilogue. Restores callee-saved registers and returns."
  (let ((total-frame (+ frame-size 112)))
    ;; Restore callee-saved registers
    (rv-emit-load-word buf +rv-ra+  +rv-sp+ (- total-frame 8))
    (rv-emit-load-word buf +rv-fp+  +rv-sp+ (- total-frame 16))
    (rv-emit-load-word buf +rv-s1+  +rv-sp+ (- total-frame 24))
    (rv-emit-load-word buf +rv-s2+  +rv-sp+ (- total-frame 32))
    (rv-emit-load-word buf +rv-s3+  +rv-sp+ (- total-frame 40))
    (rv-emit-load-word buf +rv-s4+  +rv-sp+ (- total-frame 48))
    (rv-emit-load-word buf +rv-s5+  +rv-sp+ (- total-frame 56))
    (rv-emit-load-word buf +rv-s6+  +rv-sp+ (- total-frame 64))
    (rv-emit-load-word buf +rv-s7+  +rv-sp+ (- total-frame 72))
    (rv-emit-load-word buf +rv-s8+  +rv-sp+ (- total-frame 80))
    (rv-emit-load-word buf +rv-s9+  +rv-sp+ (- total-frame 88))
    (rv-emit-load-word buf +rv-s10+ +rv-sp+ (- total-frame 96))
    (rv-emit-load-word buf +rv-s11+ +rv-sp+ (- total-frame 104))
    ;; Deallocate stack frame and return
    (rv-emit-addi buf +rv-sp+ +rv-sp+ total-frame)
    (rv-emit-ret buf)))

;;; ============================================================
;;; Two-Pass Translation
;;; ============================================================

(defun translate-mvm-to-riscv (bytecode function-table)
  "Translate MVM bytecode to RISC-V native code.
   BYTECODE is a vector of (unsigned-byte 8) containing MVM instructions.
   FUNCTION-TABLE is a hash-table mapping function indices to MVM bytecode offsets,
   or NIL if not needed.

   Returns an rv-buffer containing the native RISC-V machine code.

   Uses a two-pass approach:
     Pass 1: Decode all MVM instructions, measure native code sizes,
             build a map from MVM bytecode offsets to native code offsets.
     Pass 2: Emit native code using the label map for branch resolution."
  (setf *riscv-li-const-patches* nil)
  (let* ((label-map (make-hash-table :test 'eql))
         (native-fn-table (make-hash-table :test 'eql))
         (mvm-len (length bytecode))
         ;; Collect decoded instructions: (mvm-pc opcode operands next-pc)
         (insns nil))

    ;; ---- Decode pass: collect all instructions ----
    (let ((pos 0))
      (loop while (< pos mvm-len)
            do (let* ((decoded (decode-instruction bytecode pos))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pos (cddr decoded)))
                 (push (list pos opcode operands new-pos) insns)
                 (setf pos new-pos))))
    (setf insns (nreverse insns))

    ;; ---- Pass 1: Measure native code sizes ----
    ;; Emit into a temporary buffer to measure sizes, build label-map
    (let ((measure-buf (make-rv-buffer)))
      (dolist (insn insns)
        (destructuring-bind (mvm-pc opcode operands next-pc) insn
          (setf (gethash mvm-pc label-map) (rv-current-offset measure-buf))
          ;; Pass next-pc for branch offset computation (MVM offsets are from end of insn)
          (translate-mvm-insn-riscv measure-buf opcode operands next-pc
                                    :label-map label-map
                                    :function-table native-fn-table)))
      ;; Record end position
      (setf (gethash mvm-len label-map) (rv-current-offset measure-buf)))

    ;; Build native function table from MVM function table
    ;; Key by bytecode offset (which is what CALL operands use)
    (when function-table
      (maphash (lambda (idx mvm-offset)
                 (declare (ignore idx))
                 (let ((native-offset (gethash mvm-offset label-map)))
                   (when native-offset
                     (setf (gethash mvm-offset native-fn-table) native-offset))))
               function-table))

    ;; ---- Pass 2: Emit final native code with resolved branches ----
    (let ((final-buf (make-rv-buffer)))
      (dolist (insn insns)
        (destructuring-bind (mvm-pc opcode operands next-pc) insn
          ;; Update label-map with final positions
          (setf (gethash mvm-pc label-map) (rv-current-offset final-buf))
          ;; Pass next-pc for branch offset computation (MVM offsets are from end of insn)
          (translate-mvm-insn-riscv final-buf opcode operands next-pc
                                    :label-map label-map
                                    :function-table native-fn-table)))
      (setf (gethash mvm-len label-map) (rv-current-offset final-buf))
      ;; Re-derive the function map from PASS 2's positions and RETURN it.
      ;; This map was already being built (from pass 1) and then dropped on
      ;; the floor: the second value was never returned, so cross.lisp fell
      ;; into its proportional-estimate branch and GUESSED each function's
      ;; native offset from its bytecode offset.  With one function the guess
      ;; is 0 and happens to be right; with two it lands mid-prologue and the
      ;; image executes garbage.  Keyed by bytecode offset, which is the form
      ;; cross.lisp looks up second (the aarch64 shape).
      (let ((fn-map (make-hash-table :test 'eql)))
        (when function-table
          (maphash (lambda (idx mvm-offset)
                     (declare (ignore idx))
                     (let ((native-offset (gethash mvm-offset label-map)))
                       (when native-offset
                         (setf (gethash mvm-offset fn-map) native-offset))))
                   function-table))
        (values final-buf fn-map)))))

;;; ============================================================
;;; Target Descriptor Installation
;;; ============================================================

(defun riscv-translate-fn (opcode operands target buf)
  "Translation function for the RISC-V target descriptor.
   Wraps translate-mvm-insn-riscv for the target interface."
  (declare (ignore target))
  (translate-mvm-insn-riscv buf opcode operands 0
                             :label-map (make-hash-table)
                             :function-table nil))

(defun riscv-emit-prologue-fn (target buf)
  "Emit RISC-V function prologue via target descriptor."
  (declare (ignore target))
  (rv-emit-prologue buf +rv-local-frame-size+))

(defun riscv-emit-epilogue-fn (target buf)
  "Emit RISC-V function epilogue via target descriptor."
  (declare (ignore target))
  (rv-emit-epilogue buf +rv-local-frame-size+))

(defun rv-buffer-to-bytes (buf)
  "Convert a RISC-V code buffer to a simple byte vector."
  (let* ((raw (rv-buffer-bytes buf))
         (len (rv-buffer-position buf))
         (result (make-array len)))
    (dotimes (i len result)
      (setf (aref result i) (aref raw i)))))

(defun riscv-disassemble-native (buf &key (start 0) (end nil))
  "Print a hex dump of RISC-V native code for debugging.
   Each line shows one 32-bit instruction word."
  (let* ((raw (rv-buffer-bytes buf))
         (limit (or end (rv-buffer-position buf))))
    (loop for pos from start below limit by 4
          do (let ((w (logior (aref raw pos)
                              (ash (aref raw (+ pos 1)) 8)
                              (ash (aref raw (+ pos 2)) 16)
                              (ash (aref raw (+ pos 3)) 24))))
               (format t "  ~4,'0X: ~8,'0X~%" pos w)))))

(defparameter *riscv-linux-mode* nil
  "When true the target is a HOSTED Linux/RV64 ELF rather than bare metal: the
   serial traps become write(2)/read(2) and the exit trap becomes exit(2).
   Counterpart of *X64-LINUX-MODE* and *I386-LINUX-MODE*.

   RV64 USES THE asm-generic SYSCALL NUMBERS, not x86's: read is 63 and write
   is 64, where x86-64 says 0 and 1.  They are named in boot-linux-riscv.lisp
   rather than inlined here because the x86 numbers are muscle memory.")

(defconstant +rv-linux-sys-read+  63)
(defconstant +rv-linux-sys-write+ 64)
(defconstant +rv-linux-sys-exit+  93)

(defun riscv-set-linux-mode (on)
  "Turn hosted mode on or off, moving the convention slots with it.  Kept
   together in one function so the two cannot drift apart — an unmapped slot
   base is a SIGSEGV on the first call, not a subtle wrong answer."
  (setf *riscv-linux-mode* (and on t))
  (setf *rv-globals-base* (if on *rv-hosted-globals-base* #x80700000)))

(defun install-riscv-translator ()
  "Install the RISC-V translator into the target descriptor.
   Sets translate-fn, emit-prologue, and emit-epilogue on *target-riscv64*."
  (setf *riscv-64-bit* t)
  (setf (target-translate-fn *target-riscv64*) #'translate-mvm-to-riscv)
  (setf (target-emit-prologue *target-riscv64*) #'riscv-emit-prologue-fn)
  (setf (target-emit-epilogue *target-riscv64*) #'riscv-emit-epilogue-fn)
  *target-riscv64*)

(defun install-riscv32-translator ()
  "Install the SAME translator on *TARGET-RISCV32*, with *RISCV-64-BIT* off.
   The width is a property of the INSTALL, not of the call, so a build that
   installs RV32 cannot later emit an RV64 instruction by accident -- and
   INSTALL-RISCV-TRANSLATOR sets the flag back, so the two installers are
   order-independent."
  (setf *riscv-64-bit* nil)
  (setf (target-translate-fn *target-riscv32*) #'translate-mvm-to-riscv)
  (setf (target-emit-prologue *target-riscv32*) #'riscv-emit-prologue-fn)
  (setf (target-emit-epilogue *target-riscv32*) #'riscv-emit-epilogue-fn)
  *target-riscv32*)
