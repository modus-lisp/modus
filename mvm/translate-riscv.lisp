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

(defun rv-emit-slli (buf rd rs1 shamt)
  "SLLI rd, rs1, shamt (shift left logical immediate, RV64: shamt is 6 bits)"
  (rv-emit-u32 buf (rv-encode-i-type (logand shamt #x3F) rs1 #x1 rd #x13)))

(defun rv-emit-srli (buf rd rs1 shamt)
  "SRLI rd, rs1, shamt (shift right logical immediate)"
  (rv-emit-u32 buf (rv-encode-i-type (logand shamt #x3F) rs1 #x5 rd #x13)))

(defun rv-emit-srai (buf rd rs1 shamt)
  "SRAI rd, rs1, shamt (shift right arithmetic immediate)"
  (rv-emit-u32 buf (rv-encode-i-type (logior #x400 (logand shamt #x3F))
                                      rs1 #x5 rd #x13)))

;; --- RV64 I-type word ops (opcode #b0011011 = #x1B) ---

(defun rv-emit-addiw (buf rd rs1 imm12)
  "ADDIW rd, rs1, imm12 (32-bit add immediate)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x0 rd #x1B)))

;; --- Load instructions (opcode #b0000011 = #x03) ---

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

(defun rv-emit-amoswap-d (buf rd rs2 rs1 aqrl)
  "AMOSWAP.D rd, rs2, (rs1) with acquire/release bits.
   AQRL: bit1=aq, bit0=rl."
  (rv-emit-u32 buf (logior (ash #x01 27)              ; funct5 = 00001
                            (ash (logand aqrl #x3) 25) ; aq/rl
                            (ash (logand rs2 #x1F) 20)
                            (ash (logand rs1 #x1F) 15)
                            (ash #x3 12)               ; funct3 = 011 (doubleword)
                            (ash (logand rd #x1F) 7)
                            #x2F)))

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

    ;; Case 3: full 64-bit immediate - build in stages
    (t
     (let* ((val (if (minusp imm64) (logand imm64 #xFFFFFFFFFFFFFFFF) imm64))
            ;; Split into chunks working from the top
            (hi32 (logand (ash val -32) #xFFFFFFFF))
            (lo32 (logand val #xFFFFFFFF)))
       ;; Load upper 32 bits
       (rv-emit-li buf rd (if (logbitp 31 hi32)
                              (- hi32 #x100000000)
                              hi32))
       ;; Shift left by 32
       (rv-emit-slli buf rd rd 32)
       ;; Add lower 32 bits: split into lui-range + addi-range
       (let* ((lo12 (logand lo32 #xFFF))
              (lo12-sext (if (>= lo12 #x800) (- lo12 #x1000) lo12))
              (hi20 (logand (ash (- lo32 (logand lo12-sext #xFFFFFFFF)) -12) #xFFFFF)))
         (when (/= hi20 0)
           ;; Load hi20 into scratch, shift left 12, add
           (rv-emit-lui buf +rv-t0+ hi20)
           (rv-emit-add buf rd rd +rv-t0+))
         (when (/= lo12-sext 0)
           (rv-emit-addi buf rd rd (logand lo12-sext #xFFF))))))))

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
          (rv-emit-ld buf target-phys +rv-fp+ (rv-spill-offset vreg))
          target-phys))))

(defun rv-store-vreg (buf vreg phys)
  "If VREG is spilled, store PHYS back to the frame slot for VREG.
   If VREG is in a register, emit a move if PHYS differs from the target."
  (let ((dest (rv-resolve-vreg vreg)))
    (if dest
        (when (/= dest phys)
          (rv-emit-mv buf dest phys))
        ;; Spilled: store to stack frame (safe FP-relative offsets)
        (rv-emit-sd buf phys +rv-fp+ (rv-spill-offset vreg)))))

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
           ((= code #x0300)
            ;; Serial write: V0 (a0) contains tagged fixnum char code
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
         (rv-emit-sd buf rs +rv-sp+ 0)))

      (#.+op-pop+
       (let ((vd (vreg 0)))
         ;; ld t0, 0(sp); addi sp, sp, 8
         (rv-emit-ld buf +rv-t0+ +rv-sp+ 0)
         (rv-emit-addi buf +rv-sp+ +rv-sp+ 8)
         (store-result vd +rv-t0+)))

      ;; ---- Arithmetic (tagged fixnums: value << 1 | 0) ----
      ;; For add/sub the tag bits cancel out: (a<<1) + (b<<1) = (a+b)<<1
      (#.+op-add+
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-add buf +rv-t0+ ra rb)
         (store-result vd +rv-t0+)))

      (#.+op-sub+
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-sub buf +rv-t0+ ra rb)
         (store-result vd +rv-t0+)))

      (#.+op-mul+
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
         ;; Mask to 26 bits: SLLI 38, SRLI 38 (clears bits 26+)
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 38)
         (rv-emit-srli buf +rv-t0+ +rv-t0+ 38)
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)   ; retag
         (store-result vd +rv-t0+)))

      (#.+op-mul26hi+
       ;; Bits 26+ of untag(Va)*untag(Vb), tagged
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))
              (rb (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t0+ ra 1)        ; untag a
         (rv-emit-srai buf +rv-t1+ rb 1)        ; untag b
         (rv-emit-mul buf +rv-t0+ +rv-t0+ +rv-t1+) ; 64-bit result
         (rv-emit-srli buf +rv-t0+ +rv-t0+ 26)  ; shift right 26
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
         (rv-emit-ld buf +rv-t0+ rs -1)
         (store-result vd +rv-t0+)))

      (#.+op-cdr+
       ;; ld rd, 7(rs)  -- untag cons (-1), offset to cdr (+8) = +7
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         (rv-emit-ld buf +rv-t0+ rs 7)
         (store-result vd +rv-t0+)))

      (#.+op-cons+
       ;; Allocate cons from bump pointer VA (s8):
       ;; sd car, 0(VA); sd cdr, 8(VA); addi result, VA, 1; addi VA, VA, 16
       (let* ((vd (vreg 0))
              (ra (resolve (vreg 1)))          ; car
              (rb (resolve2 (vreg 2))))        ; cdr
         (rv-emit-sd buf ra +rv-s8+ 0)          ; store car at alloc ptr
         (rv-emit-sd buf rb +rv-s8+ 8)          ; store cdr at alloc ptr + 8
         (rv-emit-addi buf +rv-t0+ +rv-s8+ 1)   ; tag: cons tag = 1
         (rv-emit-addi buf +rv-s8+ +rv-s8+ 16)  ; bump alloc pointer
         (store-result vd +rv-t0+)))

      (#.+op-setcar+
       ;; Store value into car slot: sd vs, -1(vd)
       (let ((rd (resolve (vreg 0)))
             (rs (resolve2 (vreg 1))))
         (rv-emit-sd buf rs rd -1)))

      (#.+op-setcdr+
       ;; Store value into cdr slot: sd vs, 7(vd)
       (let ((rd (resolve (vreg 0)))
             (rs (resolve2 (vreg 1))))
         (rv-emit-sd buf rs rd 7)))

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
              ;; Align to 16 bytes to keep cons alloc pointer aligned
              (total-bytes (logand (+ (* (1+ size-words) 8) 15) (lognot 15))))
         ;; Build and store header
         (rv-emit-li buf +rv-t0+ (logior (ash size-words 8) subtag))
         (rv-emit-sd buf +rv-t0+ +rv-s8+ 0)
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
             (let ((off (+ +rv-frame-slot-base+ (* idx -8))))
               (rv-emit-ld buf +rv-t0+ +rv-fp+ off))
             ;; Normal object slot access
             (let* ((robj (resolve vobj))
                    (offset (- (* (1+ idx) 8) 2)))  ; (1+idx)*8 - tag
               (if (and (>= offset -2048) (<= offset 2047))
                   (rv-emit-ld buf +rv-t0+ robj offset)
                   (progn
                     (rv-emit-li buf +rv-t0+ offset)
                     (rv-emit-add buf +rv-t0+ robj +rv-t0+)
                     (rv-emit-ld buf +rv-t0+ +rv-t0+ 0)))))
         (store-result vd +rv-t0+)))

      (#.+op-obj-set+
       ;; Store object slot
       (let* ((vobj (vreg 0))
              (idx (vreg 1))
              (rs (resolve2 (vreg 2))))
         (if (= vobj +vreg-vfp+)
             ;; Frame slot store: use safe FP-relative offset below spill area
             (let ((off (+ +rv-frame-slot-base+ (* idx -8))))
               (rv-emit-sd buf rs +rv-fp+ off))
             ;; Normal object slot store
             (let* ((robj (resolve vobj))
                    (offset (- (* (1+ idx) 8) 2)))
               (if (and (>= offset -2048) (<= offset 2047))
                   (rv-emit-sd buf rs robj offset)
                   (progn
                     (rv-emit-li buf +rv-t0+ offset)
                     (rv-emit-add buf +rv-t0+ robj +rv-t0+)
                     (rv-emit-sd buf rs +rv-t0+ 0)))))))

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
         (rv-emit-ld buf +rv-t0+ rs -2)
         (rv-emit-andi buf +rv-t0+ +rv-t0+ #xFF)
         (store-result vd +rv-t0+)))

      ;; ---- Memory (raw) ----
      (#.+op-load+
       (let* ((vd (vreg 0))
              (raddr (resolve (vreg 1)))
              (width (vreg 2)))
         (case width
           (0 (rv-emit-lbu buf +rv-t0+ raddr 0))   ; u8
           (1 (rv-emit-lh buf +rv-t0+ raddr 0))     ; u16
           (2 (rv-emit-lw buf +rv-t0+ raddr 0))     ; u32
           (3 (rv-emit-ld buf +rv-t0+ raddr 0)))    ; u64
         (store-result vd +rv-t0+)))

      (#.+op-store+
       (let* ((raddr (resolve (vreg 0)))
              (rs (resolve2 (vreg 1)))
              (width (vreg 2)))
         (case width
           (0 (rv-emit-sb buf rs raddr 0))
           (1 (rv-emit-sh buf rs raddr 0))
           (2 (rv-emit-sw buf rs raddr 0))
           (3 (rv-emit-sd buf rs raddr 0)))))

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

      (#.+op-call-ind+
       ;; Indirect call via register
       (let ((rs (resolve (vreg 0))))
         (rv-emit-jalr buf +rv-ra+ rs 0)))

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
         (rv-emit-ld buf +rv-ra+  +rv-sp+ (- total-frame 8))
         (rv-emit-ld buf +rv-fp+  +rv-sp+ (- total-frame 16))
         (rv-emit-ld buf +rv-s1+  +rv-sp+ (- total-frame 24))
         (rv-emit-ld buf +rv-s2+  +rv-sp+ (- total-frame 32))
         (rv-emit-ld buf +rv-s3+  +rv-sp+ (- total-frame 40))
         (rv-emit-ld buf +rv-s4+  +rv-sp+ (- total-frame 48))
         (rv-emit-ld buf +rv-s5+  +rv-sp+ (- total-frame 56))
         (rv-emit-ld buf +rv-s6+  +rv-sp+ (- total-frame 64))
         (rv-emit-ld buf +rv-s7+  +rv-sp+ (- total-frame 72))
         (rv-emit-ld buf +rv-s8+  +rv-sp+ (- total-frame 80))
         (rv-emit-ld buf +rv-s9+  +rv-sp+ (- total-frame 88))
         (rv-emit-ld buf +rv-s10+ +rv-sp+ (- total-frame 96))
         (rv-emit-ld buf +rv-s11+ +rv-sp+ (- total-frame 104))
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
         (rv-emit-addi buf +rv-s8+ +rv-s8+ 16)   ; bump alloc
         (store-result vd +rv-t0+)))

      (#.+op-gc-check+
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
       (rv-emit-sd buf +rv-ra+  +rv-sp+ 88)
       (rv-emit-sd buf +rv-s0+  +rv-sp+ 80)
       (rv-emit-sd buf +rv-s1+  +rv-sp+ 72)
       (rv-emit-sd buf +rv-s2+  +rv-sp+ 64)
       (rv-emit-sd buf +rv-s3+  +rv-sp+ 56)
       (rv-emit-sd buf +rv-s4+  +rv-sp+ 48)
       (rv-emit-sd buf +rv-s5+  +rv-sp+ 40)
       (rv-emit-sd buf +rv-s6+  +rv-sp+ 32)
       (rv-emit-sd buf +rv-s7+  +rv-sp+ 24)
       (rv-emit-sd buf +rv-s8+  +rv-sp+ 16)
       (rv-emit-sd buf +rv-s9+  +rv-sp+ 8)
       (rv-emit-sd buf +rv-s10+ +rv-sp+ 0))

      (#.+op-restore-ctx+
       ;; Restore all callee-saved registers from the stack frame
       (rv-emit-ld buf +rv-ra+  +rv-sp+ 88)
       (rv-emit-ld buf +rv-s0+  +rv-sp+ 80)
       (rv-emit-ld buf +rv-s1+  +rv-sp+ 72)
       (rv-emit-ld buf +rv-s2+  +rv-sp+ 64)
       (rv-emit-ld buf +rv-s3+  +rv-sp+ 56)
       (rv-emit-ld buf +rv-s4+  +rv-sp+ 48)
       (rv-emit-ld buf +rv-s5+  +rv-sp+ 40)
       (rv-emit-ld buf +rv-s6+  +rv-sp+ 32)
       (rv-emit-ld buf +rv-s7+  +rv-sp+ 24)
       (rv-emit-ld buf +rv-s8+  +rv-sp+ 16)
       (rv-emit-ld buf +rv-s9+  +rv-sp+ 8)
       (rv-emit-ld buf +rv-s10+ +rv-sp+ 0)
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
           (3 (rv-emit-ld buf +rv-t1+ +rv-t0+ 0)))
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
           (3 (rv-emit-sd buf rs +rv-t0+ 0)))))

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
             (rv-emit-ld buf +rv-t0+ +rv-tp+ offset)
             (progn
               (rv-emit-li buf +rv-t0+ offset)
               (rv-emit-add buf +rv-t0+ +rv-tp+ +rv-t0+)
               (rv-emit-ld buf +rv-t0+ +rv-t0+ 0)))
         (store-result vd +rv-t0+)))

      (#.+op-percpu-set+
       ;; Write per-CPU data
       (let* ((offset (vreg 0))
              (rs (resolve (vreg 1))))
         (if (and (>= offset -2048) (<= offset 2047))
             (rv-emit-sd buf rs +rv-tp+ offset)
             (progn
               (rv-emit-li buf +rv-t0+ offset)
               (rv-emit-add buf +rv-t0+ +rv-tp+ +rv-t0+)
               (rv-emit-sd buf rs +rv-t0+ 0)))))

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
    (rv-emit-sd buf +rv-ra+ +rv-sp+ (- total-frame 8))
    (rv-emit-sd buf +rv-fp+ +rv-sp+ (- total-frame 16))
    ;; Save callee-saved registers (s1-s11 used by MVM)
    (rv-emit-sd buf +rv-s1+  +rv-sp+ (- total-frame 24))
    (rv-emit-sd buf +rv-s2+  +rv-sp+ (- total-frame 32))
    (rv-emit-sd buf +rv-s3+  +rv-sp+ (- total-frame 40))
    (rv-emit-sd buf +rv-s4+  +rv-sp+ (- total-frame 48))
    (rv-emit-sd buf +rv-s5+  +rv-sp+ (- total-frame 56))
    (rv-emit-sd buf +rv-s6+  +rv-sp+ (- total-frame 64))
    (rv-emit-sd buf +rv-s7+  +rv-sp+ (- total-frame 72))
    (rv-emit-sd buf +rv-s8+  +rv-sp+ (- total-frame 80))
    (rv-emit-sd buf +rv-s9+  +rv-sp+ (- total-frame 88))
    (rv-emit-sd buf +rv-s10+ +rv-sp+ (- total-frame 96))
    (rv-emit-sd buf +rv-s11+ +rv-sp+ (- total-frame 104))
    ;; Set up frame pointer
    (rv-emit-addi buf +rv-fp+ +rv-sp+ total-frame)))

(defun rv-emit-epilogue (buf frame-size)
  "Emit a RISC-V function epilogue. Restores callee-saved registers and returns."
  (let ((total-frame (+ frame-size 112)))
    ;; Restore callee-saved registers
    (rv-emit-ld buf +rv-ra+  +rv-sp+ (- total-frame 8))
    (rv-emit-ld buf +rv-fp+  +rv-sp+ (- total-frame 16))
    (rv-emit-ld buf +rv-s1+  +rv-sp+ (- total-frame 24))
    (rv-emit-ld buf +rv-s2+  +rv-sp+ (- total-frame 32))
    (rv-emit-ld buf +rv-s3+  +rv-sp+ (- total-frame 40))
    (rv-emit-ld buf +rv-s4+  +rv-sp+ (- total-frame 48))
    (rv-emit-ld buf +rv-s5+  +rv-sp+ (- total-frame 56))
    (rv-emit-ld buf +rv-s6+  +rv-sp+ (- total-frame 64))
    (rv-emit-ld buf +rv-s7+  +rv-sp+ (- total-frame 72))
    (rv-emit-ld buf +rv-s8+  +rv-sp+ (- total-frame 80))
    (rv-emit-ld buf +rv-s9+  +rv-sp+ (- total-frame 88))
    (rv-emit-ld buf +rv-s10+ +rv-sp+ (- total-frame 96))
    (rv-emit-ld buf +rv-s11+ +rv-sp+ (- total-frame 104))
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
      final-buf)))

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

(defun install-riscv-translator ()
  "Install the RISC-V translator into the target descriptor.
   Sets translate-fn, emit-prologue, and emit-epilogue on *target-riscv64*."
  (setf (target-translate-fn *target-riscv64*) #'translate-mvm-to-riscv)
  (setf (target-emit-prologue *target-riscv64*) #'riscv-emit-prologue-fn)
  (setf (target-emit-epilogue *target-riscv64*) #'riscv-emit-epilogue-fn)
  *target-riscv64*)
