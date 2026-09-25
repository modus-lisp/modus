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
  "Emit a 32-bit instruction word (little-endian), DOUBLING the byte array when
   it would overflow.

   THE FIXED 131072-BYTE ARRAY WAS NOT A BUDGET, IT WAS A CLIFF.  The real CL
   image's native code is about 36 MB, so the translator aborted mid-emit with
   `Invalid index 131072' and the build printed `giving up on translator, using
   partial result' — a PARTIAL IMAGE that would have been written and shipped.
   It was caught only because check-compiler-warns refuses a degraded build,
   which is exactly the silent-degradation class that check exists for.

   Same fix, same class, as ARM32-EMIT's 262144 cliff (fixed earlier in this
   campaign) and A64-EMIT's.  Four spare bytes of headroom because one call
   writes four."
  (let ((bytes (rv-buffer-bytes buf))
        (pos (rv-buffer-position buf)))
    (when (>= (+ pos 4) (length bytes))
      (let ((new (make-array (* 2 (length bytes)) :initial-element 0)))
        (replace new bytes)
        (setf (rv-buffer-bytes buf) new)
        (setf bytes new)))
    (setf (aref bytes pos)       (logand word #xFF))
    (setf (aref bytes (+ pos 1)) (logand (ash word -8) #xFF))
    (setf (aref bytes (+ pos 2)) (logand (ash word -16) #xFF))
    (setf (aref bytes (+ pos 3)) (logand (ash word -24) #xFF))
    (setf (rv-buffer-position buf) (+ pos 4))))

(defun rv-patch-branch-here (buf branch-pos)
  "Back-patch the B-type branch at BRANCH-POS to land at the CURRENT position.

   HAND-COUNTED BRANCH DISTANCES ARE A BUG FACTORY.  The first version of the
   handler-case triple counted instructions to size its forward branches and was
   off by three — a silent jump into the middle of a frame copy.  Emitting the
   branch with a placeholder and patching it from the MEASURED position removes
   the whole class, and costs one function.

   B-type scrambles the immediate: imm[12|10:5] in bits 31..25 and
   imm[4:1|11] in bits 11..7, with imm[0] always zero."
  (let* ((bytes (rv-buffer-bytes buf))
         (target (rv-buffer-position buf))
         (off (- target branch-pos))
         (w (logior (aref bytes branch-pos)
                    (ash (aref bytes (+ branch-pos 1)) 8)
                    (ash (aref bytes (+ branch-pos 2)) 16)
                    (ash (aref bytes (+ branch-pos 3)) 24))))
    (assert (and (>= off -4096) (< off 4096) (evenp off)) ()
            "rv-patch-branch-here: offset ~D out of B-type range" off)
    ;; clear the two immediate fields, then re-scatter
    (setf w (logand w #x01FFF07F))
    (setf w (logior w
                    (ash (logand (ash off -12) #x1) 31)
                    (ash (logand (ash off -5) #x3F) 25)
                    (ash (logand (ash off -1) #xF) 8)
                    (ash (logand (ash off -11) #x1) 7)))
    (dotimes (i 4)
      (setf (aref bytes (+ branch-pos i)) (logand (ash w (* i -8)) #xFF)))))

(defun rv-patch-jal-here (buf jal-pos)
  "Back-patch the J-type JAL at JAL-POS to land at the CURRENT position.
   J-type immediate: imm[20|10:1|11|19:12] — the same scramble cross.lisp's
   entry jump builds by hand."
  (let* ((bytes (rv-buffer-bytes buf))
         (target (rv-buffer-position buf))
         (off (- target jal-pos))
         (w (logior (aref bytes jal-pos)
                    (ash (aref bytes (+ jal-pos 1)) 8)
                    (ash (aref bytes (+ jal-pos 2)) 16)
                    (ash (aref bytes (+ jal-pos 3)) 24))))
    (assert (and (>= off -1048576) (< off 1048576) (evenp off)) ()
            "rv-patch-jal-here: offset ~D out of J-type range" off)
    (setf w (logand w #x00000FFF))
    (setf w (logior w
                    (ash (logand (ash off -20) #x1) 31)
                    (ash (logand (ash off -1) #x3FF) 21)
                    (ash (logand (ash off -11) #x1) 20)
                    (ash (logand (ash off -12) #xFF) 12)))
    (dotimes (i 4)
      (setf (aref bytes (+ jal-pos i)) (logand (ash w (* i -8)) #xFF)))))

(defun rv-patch-addi-imm (buf pos imm12)
  "Rewrite the I-type immediate of the instruction at POS.  Used to fill in an
   AUIPC+ADDI pair's low half once the target offset is known."
  (let* ((bytes (rv-buffer-bytes buf))
         (w (logior (aref bytes pos)
                    (ash (aref bytes (+ pos 1)) 8)
                    (ash (aref bytes (+ pos 2)) 16)
                    (ash (aref bytes (+ pos 3)) 24))))
    (setf w (logior (logand w #x000FFFFF) (ash (logand imm12 #xFFF) 20)))
    (dotimes (i 4)
      (setf (aref bytes (+ pos i)) (logand (ash w (* i -8)) #xFF)))))

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

;;; --- RV64D: double-precision floating point (opcode 0x53) ---
;;;
;;; FP R-type: funct7[31:25] | rs2[24:20] | rs1[19:15] | rm[14:12] | rd[11:7] | 0x53
;;; RM is the rounding mode: 0 = RNE (nearest-even, the IEEE default) and
;;; 1 = RTZ (toward zero), which is what a float->int TRUNCATION needs.
;;;
;;; THE FPU HAS TO BE ON.  RISC-V gates every FP instruction on mstatus.FS: if FS
;;; is 0 the instruction is ILLEGAL, not slow.  Linux sets FS for userspace, so
;;; the hosted port needs nothing; a BARE-metal image must set it in its boot
;;; stub, which boot-riscv.lisp does not yet do.  That is the same reason
;;; CLAUDE.md gives for the float opcodes being unimplemented everywhere.
(defun rv-emit-fp-r (buf funct7 rs2 rs1 rm rd)
  (rv-emit-u32 buf (logior (ash (logand funct7 #x7F) 25)
                           (ash (logand rs2 #x1F) 20)
                           (ash (logand rs1 #x1F) 15)
                           (ash (logand rm #x7) 12)
                           (ash (logand rd #x1F) 7)
                           #x53)))

(defun rv-emit-fadd-d (buf fd fs1 fs2) (rv-emit-fp-r buf #x01 fs2 fs1 0 fd))
(defun rv-emit-fsub-d (buf fd fs1 fs2) (rv-emit-fp-r buf #x05 fs2 fs1 0 fd))
(defun rv-emit-fmul-d (buf fd fs1 fs2) (rv-emit-fp-r buf #x09 fs2 fs1 0 fd))
(defun rv-emit-fdiv-d (buf fd fs1 fs2) (rv-emit-fp-r buf #x0D fs2 fs1 0 fd))

(defun rv-emit-fmv-d-x (buf fd rs1)
  "FMV.D.X fd, rs1 -- move 64 raw bits from an INTEGER register into an FP one.
   RV64 ONLY: on RV32 there is no 64-bit integer register to move from, so a
   32-bit port has to bounce the value through memory with FLD instead."
  (rv-emit-fp-r buf #x79 0 rs1 0 fd))

(defun rv-emit-fmv-x-d (buf rd fs1)
  "FMV.X.D rd, fs1 -- move 64 raw bits from an FP register into an integer one."
  (rv-emit-fp-r buf #x71 0 fs1 0 rd))

(defun rv-emit-fcvt-d-l (buf fd rs1)
  "FCVT.D.L fd, rs1 -- signed 64-bit integer to double."
  (rv-emit-fp-r buf #x69 2 rs1 0 fd))

(defun rv-emit-fcvt-l-d (buf rd fs1)
  "FCVT.L.D rd, fs1, rtz -- double to signed 64-bit integer, TRUNCATING toward
   zero, which is what :ftoi is specified to do."
  (rv-emit-fp-r buf #x61 2 fs1 1 rd))

(defun rv-emit-fcvt-d-w (buf fd rs1)
  "FCVT.D.W fd, rs1 -- signed 32-bit integer to double.  RV32's FCVT.D.L: a
   fixnum's value always fits in 32 bits there, so nothing is lost."
  (rv-emit-fp-r buf #x69 0 rs1 0 fd))

(defun rv-emit-fcvt-w-d (buf rd fs1)
  "FCVT.W.D rd, fs1, rtz -- double to signed 32-bit integer, truncating."
  (rv-emit-fp-r buf #x61 0 fs1 1 rd))

(defun rv-emit-lhu (buf rd rs1 imm12)
  "LHU rd, imm12(rs1) (load halfword unsigned)"
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x5 rd #x03)))

(defun rv-emit-fld (buf fd rs1 imm12)
  "FLD fd, imm12(rs1) -- load a double into an FP register (D extension)."
  (rv-emit-u32 buf (rv-encode-i-type imm12 rs1 #x3 fd #x07)))

(defun rv-emit-fsd (buf fs2 rs1 imm12)
  "FSD fs2, imm12(rs1) -- store a double from an FP register (D extension)."
  (rv-emit-u32 buf (rv-encode-s-type imm12 fs2 rs1 #x3 #x27)))

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

(defparameter *riscv-record-unimpl* t
  "Record every MVM opcode and trap code that reaches this translator's default
   arm, in *RISCV-UNIMPL-OPS*.

   i386 learned this the hard way and says why: a silent placeholder is the worst
   failure mode when bringing a target up, because the image BUILDS CLEAN and
   then dies with no explanation.  Worse here, because the real CL image is 40 MB
   and one unimplemented opcode buried in it is not something you find by
   reading.  The census turns that into a list.")

(defvar *riscv-unimpl-ops* nil
  "Hash of key -> count.  An opcode is its own number; a TRAP is #x10000 + its
   code, so the two cannot collide in one table.")

(defun rv-note-unimpl (key)
  (when *riscv-record-unimpl*
    (let ((tbl (or *riscv-unimpl-ops*
                   (setf *riscv-unimpl-ops* (make-hash-table :test 'eql)))))
      (incf (gethash key tbl 0)))))

(defun riscv-unimplemented-report ()
  "Alist of (key . count) for everything that fell through to the default arm
   during the last translation, most frequent first.  Keys >= #x10000 are traps."
  (let ((acc nil))
    (when *riscv-unimpl-ops*
      (maphash (lambda (k v) (push (cons k v) acc)) *riscv-unimpl-ops*))
    (sort acc #'> :key #'cdr)))

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

(defun rv-object-tag ()
  "The low-nibble tag on an object pointer: 9, +TAG-OBJECT+, at both widths.

   EVERY POINTER TAG MUST BE ODD.  The shared compiler recognises a fixnum by
   its low bit alone -- compile-integerp is `test dest, 1' -- and recognises an
   array or string by comparing OBJ-TAG with a baked +TAG-OBJECT+ of 9.  With
   objects tagged 2 (as RISC-V once did), (integerp <string>) is T and
   (stringp <string>) NIL for every object in the image; measured in the real
   CL image as SYMBOL-NAME taking its integer branch for a symbol.

   Tag 9 needs 16-byte objects, which is why rv-granule is 16 at both widths."
  9)

(defun rv-granule ()
  "Allocation granule: SIXTEEN BYTES AT BOTH WIDTHS.

   Pointer types are read from the low FOUR bits (+tag-mask+ #x0F: cons 0001,
   function 0011, object 1001), so every heap object must start 16-aligned or
   bit 3 of its address leaks into the tag.  RV32 used a word pair -- eight
   bytes -- which left half of all objects at 8 mod 16, where a cons (tag 1)
   reads as 9 and an object (tag 9) as 1.  That is also why RV32 had been given
   object tag 2, which the shared compiler cannot use (it needs every pointer
   tag ODD; see rv-object-tag).  A cons on RV32 now wastes eight bytes; in
   exchange the tag scheme is the same on every port."
  16)

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
(defun rv-nargs-addr ()
  "THE NARGS SLOT IS THE SHARED CONTRACT ADDRESS #x10000150 IN HOSTED MODE, not
   globals_base+0, and that is a correctness requirement rather than tidiness.

   mvm/cl-eval.lisp reads the RAW LITERAL #x10000150 in two places — the macro
   expander shims, which check `(= nargs 2)'.  A back end that writes nargs
   anywhere else leaves those reads looking at a word nothing ever wrote, so the
   comparison is against zero and every (funcall (macro-function 'X) form env)
   signals PROGRAM-ERROR.  This is the i386 #260 defect, whose fix (route the
   shims through the %GET-NARGS primitive) is written but NOT merged, because it
   converted four ladder libraries from exit 0 to exit 139 for reasons still
   unfound.  Rather than inherit an unmerged fix, this port does what #261 did for
   mv-count: a convention slot that SHARED SOURCE reads lives at the shared
   address, full stop.

   HOW IT PRESENTED, because the symptom names nothing: the real CL image built,
   booted, ran ~885 blocks, and died building the string \"force-output requires 0
   or 1 arguments\" — an ARITY error, because the &rest prologue read a stale nargs
   and made a list with too many elements.  The error reporter then faulted at
   si_addr=0x56, so the visible failure was a wild pointer two layers below the
   cause.  Reconstructing that string out of the instruction trace (the stores are
   tagged character codes) is what turned it into a one-line fix.

   #x10000150 is inside the mmap'd heap and below the allocator start, exactly
   where the shared low-memory map puts it.  BARE metal keeps globals_base+0: no
   shared-source read reaches it there, and #x10000150 is UART MMIO on virt."
  (if *riscv-linux-mode* #x10000150 (+ *rv-globals-base* #x00)))
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

(defun rv-emit-branch-far (buf kind rs1 rs2 offset)
  "Emit a conditional branch that can reach ANYWHERE within JAL range, as the
   INVERTED condition skipping an unconditional jump:

       <inverted> rs1, rs2, +8
       jal  x0, offset-4

   WHY THIS IS UNCONDITIONALLY TWO INSTRUCTIONS, never one when one would fit.
   RISC-V's B-type immediate reaches +/-4 KB; the real CL image needs 18792 bytes
   from one branch alone, so the short form is not always available.  But sizing
   the choice per-branch is NOT SAFE in this translator: pass 1 builds the label
   map WHILE measuring, so an early branch sees no target yet and measures SHORT,
   while pass 2 sees the real far target and emits LONG -- and every offset after
   it is then wrong.  A fixed size cannot disagree with itself.

   The cost is 4 bytes per conditional branch.  The alternative is a convergence
   loop over the whole module, which is the right answer for a code-size-sensitive
   back end and not worth it here.

   The JAL's displacement is measured from the JAL, which sits 4 bytes after the
   branch OFFSET was measured from -- hence offset-4."
  (let ((inverted (ecase kind
                    ;; The inverse of each test, because the jump is what the
                    ;; ORIGINAL condition should reach.
                    (:beq :bne) (:bne :beq)
                    (:blt :bge) (:bge :blt))))
    (ecase inverted
      (:beq (rv-emit-beq buf rs1 rs2 8))
      (:bne (rv-emit-bne buf rs1 rs2 8))
      (:blt (rv-emit-blt buf rs1 rs2 8))
      (:bge (rv-emit-bge buf rs1 rs2 8)))
    (rv-emit-j buf (- offset 4))))

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

(defconstant +rv-local-frame-size+ 1056
  "Bytes reserved for locals: 4 spill slots (32) + 128 frame slots (1024).
   Total frame = this + 112 (save area) = 1168.

   ONE HUNDRED TWENTY-EIGHT FRAME SLOTS, THE SAME COUNT AS translate-x64's
   +frame-slot-base+ AREA -- not eight.  The MVM compiler addresses let-bound
   locals as `obj-ref VFP <idx>' with an index it chooses per function, and
   nothing tells the back end a bound; x64 reserves 128 slots (1024 bytes,
   total frame 1120) precisely because the index is not bounded by anything
   the translator can see.

   RISC-V reserved EIGHT.  A function with a ninth local then addressed
   fp-160-8*8 = fp-216, which is BELOW a 208-byte frame -- memory the next
   CALL's own frame occupies, so the local read back whatever the callee
   left there.  Measured in the real CL image: %GV-CELL's `%gv-holder' read
   RAW 0 out of its slot, and 0 is neither NIL (#xDEAD0001) nor cons-tagged,
   so `(car %gv-holder)' fell into %SIGNAL-TYPE-ERROR -- whose own use of a
   special re-entered %GV-CELL, giving the infinite mutual recursion that
   looked like a hang.  %GV-CELL binds eight locals and calls two functions.

   THE 12-BIT IMMEDIATE IS THE CEILING HERE.  Every prologue offset is
   sp-relative and every slot offset fp-relative, and RISC-V I-type
   immediates are signed 12-bit: -2048..2047.  Total frame 1168 and the
   deepest slot at fp-1168 both fit; a larger frame would need the offsets
   materialised into a register first, so raising this count is not free.")

(defun rv-spill-offset (vreg)
  "Compute the FP-relative offset for a spilled vreg (V12-V15)."
  (+ +rv-spill-base-offset+ (* (- vreg 12) -8)))

(defun rv-vreg-spills-p (vreg)
  "Does VREG live in a spill slot?  ONLY V12-V15 DO.

   `rv-resolve-vreg' answers NIL for two different things: a vreg that spills
   (V12-V15) and a vreg that HAS NO LOCATION AT ALL -- VPC, index 22, marked
   `nil ; VPC -> not mapped' in *riscv-reg-map*, and any index past the map's
   end.  Treating the second as the first is silent: `rv-spill-offset'
   extrapolates its arithmetic happily and hands back fp-200 for VPC, which is
   a REAL, LIVE address inside the frame, so the mistake becomes a load of
   someone else's local rather than an error.  translate-x64 draws this line
   explicitly (`vreg-spills-p' is (and (>= vreg 9) (<= vreg 15)) and both
   emit-load-vreg and emit-store-vreg ERROR otherwise); RISC-V did not."
  (and (>= vreg 12) (<= vreg 15)))

(defun rv-vreg-or-load (buf vreg target-phys)
  "Resolve VREG to a physical register. If VREG is spilled, load it from
   the frame into TARGET-PHYS (a scratch register) and return TARGET-PHYS.
   Otherwise return the physical register directly."
  (let ((phys (rv-resolve-vreg vreg)))
    (cond (phys phys)
          ((rv-vreg-spills-p vreg)
           (rv-emit-load-word buf target-phys +rv-fp+ (rv-spill-offset vreg))
           target-phys)
          (t (error "MVM RISC-V: cannot load vreg ~D -- it has no location" vreg)))))

(defun rv-store-vreg (buf vreg phys)
  "If VREG is spilled, store PHYS back to the frame slot for VREG.
   If VREG is in a register, emit a move if PHYS differs from the target."
  (let ((dest (rv-resolve-vreg vreg)))
    (cond (dest
           (when (/= dest phys)
             (rv-emit-mv buf dest phys)))
          ((rv-vreg-spills-p vreg)
           (rv-emit-store-word buf phys +rv-fp+ (rv-spill-offset vreg)))
          (t (error "MVM RISC-V: cannot store vreg ~D -- it has no location" vreg)))))

;;; ============================================================
;;; MVM -> RISC-V Translation
;;; ============================================================

(defvar *rv-last-cmp-rs1* +rv-t3+
  "Physical register holding the first operand of the most recent MVM-CMP.")
(defvar *rv-last-cmp-rs2* +rv-t4+
  "Physical register holding the second operand of the most recent MVM-CMP.")

(defun translate-mvm-insn-riscv (buf opcode operands mvm-pc
                                  &key (pass2 nil) label-map function-table)
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
           ;; Native byte offset from here to the target, via pass 1's label map.
           ;;
           ;; A MISS USED TO RETURN 0, commented "placeholder, fixed up in second
           ;; pass" — AND THERE IS NO FIXUP PASS.  Offset 0 on a branch is a branch
           ;; TO ITSELF: an infinite self-loop, one basic block, no diagnostic.
           ;; That is exactly what the real CL image did — a -d exec census showed
           ;; ONE block executing 300,000 times while a probe inside the function
           ;; printed its entry marker and then never reached the first statement of
           ;; the loop body.
           ;;
           ;; Now it is fatal and names the target.  A branch whose destination is
           ;; not an instruction boundary recorded by the decode pass is a compiler
           ;; or decoder bug, and silence turned it into a hang 12 MB into a 32 MB
           ;; image.  Same discipline as the pass-2 size assertion above: make the
           ;; unrepresentable state a build failure that says which one it is.
           (let ((native-target (gethash mvm-target-pc label-map)))
             ;; A MISS IS EXPECTED IN PASS 1 AND A BUG IN PASS 2.
             ;;
             ;; Pass 1 builds this map as it measures, so a FORWARD branch there
             ;; legitimately cannot see its target yet — that is what the original
             ;; "placeholder, fixed up in second pass" comment meant, and returning 0
             ;; is harmless because pass 1's only product is SIZES, and every emitter
             ;; here is size-stable (see the pass-2 assertion).
             ;;
             ;; In PASS 2 the map is complete, so a miss means a branch to a target
             ;; that is not an instruction boundary, and returning 0 would emit a
             ;; branch TO ITSELF: an infinite self-loop, one basic block, no
             ;; diagnostic.  Verified for this module: the two decoders agree on all
             ;; 3,006,921 instructions and every observed target IS a boundary, so
             ;; pass 2 should never miss — and if it ever does, it now says so.
             (cond (native-target (- native-target (rv-current-offset buf)))
                   (pass2 (error "riscv PASS 2 branch target ~D is not in the label ~
                                  map (opcode #x~2,'0X at mvm-pc ~D); a 0 offset here ~
                                  would branch to itself"
                                 mvm-target-pc opcode mvm-pc))
                   (t 0)))))

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
            ;; Frame-enter: CODE is the function's parameter count.  Emit the
            ;; prologue, then COPY PARAMETERS 5.. INTO FRAME SLOTS 4.. .
            ;;
            ;; Only V0-V3 travel in registers.  The caller PUSHes the rest before
            ;; the CALL, and the compiled body reads parameter i as
            ;; `obj-ref VFP i' -- so the prologue has to put it there.
            ;; translate-x64 does exactly this ("If > 4 params, copy overflow args
            ;; from caller's stack to local frame slots"); RISC-V emitted the
            ;; prologue and nothing else, so every function with a fifth
            ;; parameter read it from an UNINITIALISED frame slot.
            ;;
            ;; Measured in the real CL image: the first INTERN of %INIT-PACKAGES
            ;; reaches COPY-SEQ -> (%bulk-copy result 0 array 0 len), and LEN is
            ;; the fifth argument.  It arrived as stack garbage, so the loop test
            ;; (>= i n) went down the generic NUMERIC->= path on a non-number and
            ;; never came true -- the image sat in NUMERIC-VALUE-LESS-P /
            ;; %IEEE-FLOAT-P forever, 70 functions into boot, printing nothing.
            ;;
            ;; LAYOUT.  PUSH is always `addi sp,sp,-8; store' (8 bytes on both
            ;; widths), the compiler pushes overflow args in reverse so arg 4 is
            ;; pushed LAST, and JALR puts the return address in ra rather than on
            ;; the stack.  The prologue sets fp to the caller's sp at the call, so
            ;; arg i lives at fp + (i-4)*8.  x64's version reads rbp+16+(i-4)*8;
            ;; its 16 is the return address and saved rbp that RISC-V does not push.
            (rv-emit-prologue buf +rv-local-frame-size+)
            (when (> code 128)
              (error "MVM RISC-V: ~D parameters exceed the 128-slot frame" code))
            (loop for i from 4 below code
                  do (rv-emit-load-word buf +rv-t0+ +rv-fp+ (* (- i 4) 8))
                     (rv-emit-store-word buf +rv-t0+ +rv-fp+
                                         (+ +rv-frame-slot-base+
                                            (* i (- (rv-word-size)))))))
           ((< code #x0300)
            ;; Frame-alloc/frame-free: NOP for now
            nil)
           ((= code #x0520)
            ;; INSTALL-SIGNAL-HANDLERS -- a deliberate, named NOP, exactly as on
            ;; i386 (*i386-safe-nop-traps*): a pure side effect with no result
            ;; and no control transfer, so skipping it makes a hardware fault
            ;; fatal instead of recovered into a handler-case -- worse
            ;; diagnostics, never a wrong value.  Every OTHER unimplemented trap
            ;; stops loudly.  (x64/aarch64 install SIGSEGV/SIGBUS -> condition;
            ;; doing that here is real work, not this.)
            nil)
           ((= code #x0530)
            ;; COPY-OVERFLOW-ARGS -- the &rest/&key prologue's RUNTIME copy of
            ;; arguments 4.. into frame slots 4.., mirroring translate-x64's and
            ;; translate-i386's #x0530 arms.
            ;;
            ;; The compiler emits this at the head of every &rest ladder
            ;; (compile-rest-prologue): the ladder then loads argument k as
            ;; `obj-ref VFP k' for EVERY k, so arguments past the four register
            ;; ones have to be in the frame first -- and how many there are is
            ;; only known at run time, from the nargs slot.  RISC-V had no arm
            ;; for it at all.  An earlier census wrote it off as "the JIT seam,
            ;; inert as on i386"; i386 in fact implements it.  Missing, every
            ;; &key call with more than four arguments parsed its keywords out
            ;; of uninitialised slots: in the real CL image, "unknown keyword
            ;; argument" with garbage values while evaluating genera-compat and
            ;; sb-shims, where x64 reports nothing.
            ;;
            ;; Layout as in the fixed-count copy on frame-enter: argument i is
            ;; at fp + (i-4)*8, slot i at fp + frame-slot-base - i*word.  Capped
            ;; at 32 arguments, as x64 and i386 cap it (the ladder is unrolled to
            ;; match).  Every branch skips a fixed run, so the arm is size-stable;
            ;; n < 5 falls straight out because i starts at 4.
            (rv-emit-li buf +rv-t3+ (rv-nargs-addr))
            (rv-emit-load-word buf +rv-t1+ +rv-t3+ 0)          ; n (raw)
            (rv-emit-addi buf +rv-t2+ +rv-x0+ 32)
            (rv-emit-bge buf +rv-t2+ +rv-t1+ 8)                ; n <= 32: keep
            (rv-emit-mv buf +rv-t1+ +rv-t2+)                   ; n = 32
            (rv-emit-addi buf +rv-t2+ +rv-x0+ 4)               ; i = 4
            (rv-emit-addi buf +rv-t3+ +rv-fp+ 0)               ; src = fp
            (rv-emit-addi buf +rv-t4+ +rv-fp+                  ; dst = slot 4
                          (+ +rv-frame-slot-base+ (* 4 (- (rv-word-size)))))
            ;; top:
            (rv-emit-bge buf +rv-t2+ +rv-t1+ 28)               ; i >= n -> done
            (rv-emit-load-word buf +rv-t0+ +rv-t3+ 0)
            (rv-emit-store-word buf +rv-t0+ +rv-t4+ 0)
            (rv-emit-addi buf +rv-t3+ +rv-t3+ 8)               ; pushes are 8 bytes
            (rv-emit-addi buf +rv-t4+ +rv-t4+ (- (rv-word-size)))
            (rv-emit-addi buf +rv-t2+ +rv-t2+ 1)
            (rv-emit-j buf -24))                               ; -> top
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
           ((= code #x0510)
            ;; SETJMP.  Stack the outer frame, then save sp / fp / resume-ip and
            ;; the eight callee-saved V-regs.  Returns NIL the first time; a
            ;; LONGJMP re-enters at the SAME point with V0 already holding T.
            ;;
            ;; The resume address is AUIPC-relative and points at the
            ;; instruction AFTER the whole block, so both the first call and the
            ;; longjmp land there — no skip-branch, exactly x64's shape.
            (rv-emit-handler-push buf)
            (let ((to-skip (rv-current-offset buf)))
              ;; A capped push stored no frame, so DO NOT arm: an over-deep
              ;; handler-case degrades to a transparent no-op rather than
              ;; overwriting the frame that is still live.
              (rv-emit-bne buf +rv-t3+ +rv-x0+ 0)              ; patched
              (rv-emit-li buf +rv-t0+ *rv-jmpbuf-addr*)
              (rv-emit-store-word buf +rv-sp+ +rv-t0+ 0)
              (rv-emit-store-word buf +rv-fp+ +rv-t0+ 8)
              ;; resume-ip: AUIPC gives PC-of-this-instruction, and the landing
              ;; point is a fixed number of BYTES further on — computed after the
              ;; fact would need another patcher, so it is counted here and the
              ;; count is CHECKED by an assert below.
              (let ((auipc-pos (rv-current-offset buf)))
                (rv-emit-auipc buf +rv-t1+ 0)
                (rv-emit-addi buf +rv-t1+ +rv-t1+ 0)           ; patched
                (rv-emit-store-word buf +rv-t1+ +rv-t0+ 16)
                (dotimes (i 8)
                  (rv-emit-store-word buf (rv-resolve-vreg (+ 4 i))
                                      +rv-t0+ (* 8 (+ 3 i))))
                (rv-patch-branch-here buf to-skip)
                ;; First return: NIL, which lives in VN.
                (rv-emit-mv buf +rv-a0+ +rv-s10+)          ; VN
                ;; Patch the ADDI so auipc+addi == the address of the NEXT
                ;; instruction, which is where LONGJMP jumps back to.
                (let ((delta (- (rv-current-offset buf) auipc-pos)))
                  (assert (< delta 2048) () "riscv setjmp: resume delta ~D too large" delta)
                  (rv-patch-addi-imm buf (+ auipc-pos 4) delta)))))

           ((= code #x0511)
            ;; LONGJMP.  Copy the jmpbuf to scratch FIRST — the pop restores the
            ;; OUTER frame over it, and we still need the INNER one we are about
            ;; to jump to.  Then restore and jump with V0 = T.
            ;;
            ;; Word 0 == 0 is the "no handler armed" sentinel.  With nothing
            ;; armed this falls through to a TRAP rather than jumping to address
            ;; zero, so an unhandled condition is an honest crash at a named
            ;; instruction instead of a wild branch.
            (rv-emit-li buf +rv-t0+ *rv-jmpbuf-addr*)
            (rv-emit-load-word buf +rv-t1+ +rv-t0+ 0)
            (let ((to-nohandler (rv-current-offset buf)))
              (rv-emit-beq buf +rv-t1+ +rv-x0+ 0)              ; patched
              (rv-emit-li buf +rv-t2+ *rv-longjmp-scratch-addr*)
              (dotimes (i +rv-jmpbuf-words+)
                (rv-emit-load-word buf +rv-t3+ +rv-t0+ (* 8 i))
                (rv-emit-store-word buf +rv-t3+ +rv-t2+ (* 8 i)))
              ;; Zero the LIVE capped count: this unwind passes every capped
              ;; (strictly inner) frame at once, so their pending absorbs must
              ;; not fire against an outer pop afterwards.
              (rv-emit-li buf +rv-t1+ *rv-hstack-capped-addr*)
              (rv-emit-store-word buf +rv-x0+ +rv-t1+ 0)
              (rv-emit-handler-pop buf)
              ;; Restore from the scratch copy.  V0 is loaded LAST but one so
              ;; nothing below clobbers it, and t2 holds the jump target.
              (rv-emit-li buf +rv-t0+ *rv-longjmp-scratch-addr*)
              (dotimes (i 8)
                (rv-emit-load-word buf (rv-resolve-vreg (+ 4 i))
                                   +rv-t0+ (* 8 (+ 3 i))))
              (rv-emit-load-word buf +rv-t2+ +rv-t0+ 16)       ; resume ip
              (rv-emit-load-word buf +rv-fp+ +rv-t0+ 8)
              (rv-emit-load-word buf +rv-sp+ +rv-t0+ 0)
              (rv-emit-li buf +rv-a0+ +t-value+)               ; second return: T
              (rv-emit-jalr buf +rv-x0+ +rv-t2+ 0)
              (rv-patch-branch-here buf to-nohandler)
              ;; No handler armed: trap with the code in a7, so the failure names
              ;; itself.  mvm-eval prints "LONGJMP with no active handler-case"
              ;; on the arches that can; this one at least stops HERE.
              (rv-emit-addi buf +rv-a7+ +rv-x0+ #x0511)
              (rv-emit-ebreak buf)))

           ((= code #x0512)
            ;; CLEAR-HANDLER: pop one frame.  V0 carries the handler-case's
            ;; RESULT at this point, so the pop must not touch it — which is why
            ;; RV-EMIT-HANDLER-POP works in t0..t6 and never in a0.
            (rv-emit-handler-pop buf))

           ((and (= code #x0502) *riscv-linux-mode*)
            ;; GENERIC 3-ARG SYSCALL.  V0 = number, V1..V3 = args, all TAGGED;
            ;; result comes back TAGGED in V0.  V0..V3 are a0..a3 here, so the
            ;; number has to move OUT of a0 before the args shift DOWN into it —
            ;; hence t0 first, and the shifts strictly left-to-right.
            ;;
            ;; NO NUMBER REMAPPING, deliberately.  translate-aarch64's version of
            ;; this trap carries a cmp/csel chain that rewrites x86-64 syscall
            ;; numbers into generic-ABI ones, because cl-fileio.lisp hardcodes the
            ;; x86-64 table.  i386 instead OVERRIDES the %sys-* functions in its
            ;; arch slot with the right numbers, which keeps the ABI knowledge in
            ;; one readable place instead of a branch chain in the code generator.
            ;; This port follows i386.
            (rv-emit-srai buf +rv-t0+ +rv-a0+ 1)        ; t0 = syscall number
            (rv-emit-srai buf +rv-a0+ +rv-a1+ 1)        ; a0 = arg1
            (rv-emit-srai buf +rv-a1+ +rv-a2+ 1)        ; a1 = arg2
            (rv-emit-srai buf +rv-a2+ +rv-a3+ 1)        ; a2 = arg3
            (rv-emit-mv buf +rv-a7+ +rv-t0+)
            (rv-emit-ecall buf)
            (rv-emit-slli buf +rv-a0+ +rv-a0+ 1))       ; tag the result
           ((and (= code #x050B) *riscv-linux-mode*)
            ;; GENERIC 6-ARG SYSCALL.  V0 = number, V1..V6 = args 1..6, tagged.
            ;; This is what lets the arch slot express openat/unlinkat/mkdirat/
            ;; renameat/newfstatat — the asm-generic ABI dropped open/stat/unlink
            ;; in favour of *at forms that take FOUR arguments, one more than
            ;; syscall3 carries.  AArch64 added five dedicated traps for them
            ;; (#x0506..#x050A); one general trap covers the same ground.
            ;;
            ;; V4/V5/V6 are s11/s1/s2 — NOT s0, which is the frame pointer.
            (rv-emit-srai buf +rv-t0+ +rv-a0+ 1)        ; t0 = syscall number
            (rv-emit-srai buf +rv-a0+ +rv-a1+ 1)        ; a0 = arg1
            (rv-emit-srai buf +rv-a1+ +rv-a2+ 1)        ; a1 = arg2
            (rv-emit-srai buf +rv-a2+ +rv-a3+ 1)        ; a2 = arg3
            (rv-emit-srai buf +rv-a3+ +rv-s11+ 1)       ; a3 = arg4 (V4)
            (rv-emit-srai buf +rv-a4+ +rv-s1+ 1)        ; a4 = arg5 (V5)
            (rv-emit-srai buf +rv-a5+ +rv-s2+ 1)        ; a5 = arg6 (V6)
            (rv-emit-mv buf +rv-a7+ +rv-t0+)
            (rv-emit-ecall buf)
            (rv-emit-slli buf +rv-a0+ +rv-a0+ 1))       ; tag the result
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
            ;; An unhandled TRAP CODE.  Recorded as #x10000+code so the build
            ;; report distinguishes "this opcode is missing" from "this trap is".
            (rv-note-unimpl (+ #x10000 code))
            (rv-emit-addi buf +rv-a7+ +rv-x0+ code)
            ;; Hosted, an ECALL here would be a real syscall numbered by the
            ;; trap code (#x533 = 1331, ...), whose error return lands in a0.
            ;; EBREAK stops with the code in a7 instead; bare keeps the ecall
            ;; its machine-mode handler expects.
            (if *riscv-linux-mode*
                (rv-emit-ebreak buf)
                (rv-emit-ecall buf))))))

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

      ;; An IMMEDIATE shift distance can reach or pass the register width -- the
      ;; compiler inlines (ash x -32) as `:sar 32', which is how mvm-emit-u64
      ;; extracts the high word.  The shamt field cannot hold it (RV-SHAMT masks
      ;; 32 to 0 on RV32), so a wide shift must be spelled by its RESULT: SHL and
      ;; SHR give 0, SAR gives the sign fill, i.e. a shift by width-1.  Before
      ;; this, `(ash v -32)' on RV32 returned V itself, and every 64-bit MVM
      ;; immediate the in-image compiler wrote (char literals, quote-pool
      ;; indices) had its low word duplicated into the high word.
      (#.+op-shl+
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (amt (vreg 2)))
         (if (>= amt (* 8 (rv-word-size)))
             (store-result vd +rv-x0+)
             (progn (rv-emit-slli buf +rv-t0+ rs amt)
                    (store-result vd +rv-t0+)))))

      (#.+op-shr+
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (amt (vreg 2)))
         (if (>= amt (* 8 (rv-word-size)))
             (store-result vd +rv-x0+)
             (progn (rv-emit-srli buf +rv-t0+ rs amt)
                    (store-result vd +rv-t0+)))))

      (#.+op-sar+
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1)))
              (amt (min (vreg 2) (1- (* 8 (rv-word-size))))))
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
         (rv-emit-branch-far buf :beq *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-bne+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-branch-far buf :bne *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-blt+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-branch-far buf :blt *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-bge+
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-branch-far buf :bge *rv-last-cmp-rs1* *rv-last-cmp-rs2* native-off)))

      (#.+op-ble+
       ;; BLE a,b = BGE b,a (swap operands)
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-branch-far buf :bge *rv-last-cmp-rs2* *rv-last-cmp-rs1* native-off)))

      (#.+op-bgt+
       ;; BGT a,b = BLT b,a (swap operands)
       (let* ((mvm-offset (vreg 0))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-branch-far buf :blt *rv-last-cmp-rs2* *rv-last-cmp-rs1* native-off)))

      (#.+op-bnull+
       ;; Branch if register equals VN (NIL)
       (let* ((rs (resolve (vreg 0)))
              (mvm-offset (vreg 1))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-branch-far buf :beq rs +rv-s10+ native-off)))

      (#.+op-bnnull+
       ;; Branch if register is not VN (NIL)
       (let* ((rs (resolve (vreg 0)))
              (mvm-offset (vreg 1))
              (target-pc (+ mvm-pc mvm-offset))
              (native-off (branch-offset target-pc)))
         (rv-emit-branch-far buf :bne rs +rv-s10+ native-off)))

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

      ;; PREDICATES MUST ANSWER NIL OR T, NEVER A RAW 0/1.
      ;;
      ;; These two returned 0 for false and #x16 for true, with a comment that
      ;; admitted the muddle ("Result: tagged boolean... Actually, simplify: store
      ;; raw boolean result as fixnum").  Every CALLER tests the result against VN
      ;; -- `bne s1, s10' -- so neither value is ever NIL and (CONSP x) WAS ALWAYS
      ;; TRUE on this target.
      ;;
      ;; Measured consequence: the real CL image walked a cdr chain in
      ;; %CDR-IS-ARRAY-OR-WRAPPER-P straight off the end and faulted taking the
      ;; cdr of 0.  No ladder rung caught it because none uses CONSP -- r08-cons
      ;; calls car/cdr directly -- which is the same "a rung per OPCODE, not per
      ;; data type" gap that hid :setcar on PowerPC.
      ;;
      ;; T IS MATERIALISED BEFORE THE BRANCH.  +T-VALUE+ is #xDEAD1009, which
      ;; RV-EMIT-LI builds in several instructions, and a multi-instruction
      ;; sequence inside a hand-counted branch span is how the handler triple's
      ;; first version went wrong.  So both answers are in registers first and the
      ;; branch skips exactly one MV.
      (#.+op-consp+
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         ;; TEST RS BEFORE TOUCHING t0.  RESOLVE hands back t0 when the vreg is
         ;; SPILLED, so writing the default answer into t0 first would destroy the
         ;; argument -- and NIL is #xDEAD0001, whose low three bits are 1, THE CONS
         ;; TAG, so the answer would then be T for everything.  Exactly the
         ;; always-true bug this arm was rewritten to fix, one register apart.
         ;; AND NIL IS NOT A CONS.  The tag mask is FOUR bits (+tag-mask+ #x0F:
         ;; cons 0001, function 0011, object 1001) -- a three-bit mask makes
         ;; +T-VALUE+ #xDEAD1009, whose nibble is 1001, read as 001 and so
         ;; answer T to (consp T).  And NIL's nibble IS +tag-cons+ by design, so
         ;; the tag test alone answers T for NIL: x64 compares against R15 and
         ;; i386 against *vn-addr* before testing the tag for exactly that
         ;; reason, and translate-i386's own comment records what the omission
         ;; costs -- `(loop while (consp cur) ... (setq cur (cdr cur)))' never
         ;; terminates, because (car NIL) is NIL and the walk recurses on NIL
         ;; forever.  Both answers are in registers before any branch and each
         ;; branch skips exactly one 4-byte MV, so the arm is size-stable.
         (rv-emit-li buf +rv-t2+ +t-value+)
         (rv-emit-andi buf +rv-t1+ rs #x0F)   ; +tag-mask+   ; read RS while it is still RS
         (rv-emit-xor buf +rv-t4+ rs +rv-s10+)      ; t4 = 0 iff RS is NIL
         (rv-emit-addi buf +rv-t3+ +rv-x0+ +tag-cons+)
         (rv-emit-mv buf +rv-t0+ +rv-s10+)          ; default NIL
         (rv-emit-bne buf +rv-t1+ +rv-t3+ 8)        ; not a cons -> keep NIL
         (rv-emit-mv buf +rv-t0+ +rv-t2+)           ; cons-tagged -> T
         (rv-emit-bne buf +rv-t4+ +rv-x0+ 8)        ; not NIL -> that answer stands
         (rv-emit-mv buf +rv-t0+ +rv-s10+)          ; NIL is an ATOM, not a cons
         (store-result vd +rv-t0+)))

      (#.+op-atom+
       ;; ATOM is the exact inverse: T unless the tag says cons.
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         ;; Same ordering requirement as :consp — read RS before writing t0.
         ;; Four-bit mask and the NIL exclusion, both for the reasons given on
         ;; :consp -- (atom NIL) is T.
         (rv-emit-li buf +rv-t2+ +t-value+)
         (rv-emit-andi buf +rv-t1+ rs #x0F)   ; +tag-mask+
         (rv-emit-xor buf +rv-t4+ rs +rv-s10+)      ; t4 = 0 iff RS is NIL
         (rv-emit-addi buf +rv-t3+ +rv-x0+ +tag-cons+)
         (rv-emit-mv buf +rv-t0+ +rv-t2+)           ; default T
         (rv-emit-bne buf +rv-t1+ +rv-t3+ 8)        ; not a cons -> keep T
         (rv-emit-mv buf +rv-t0+ +rv-s10+)          ; cons-tagged -> NIL
         (rv-emit-bne buf +rv-t4+ +rv-x0+ 8)        ; not NIL -> that answer stands
         (rv-emit-mv buf +rv-t0+ +rv-t2+)           ; NIL is an atom
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
         ;; Tag pointer: (rv-object-tag) -- 9 on RV64, 2 on RV32
         (rv-emit-addi buf +rv-t0+ +rv-s8+ (rv-object-tag))
         ;; Bump alloc pointer.  ADDI's immediate is 12 bits SIGNED, and the
         ;; compiler inlines constant sizes up to 65535 slots: a 4096-char
         ;; stream buffer is 16400 bytes, which ADDI silently wrapped to 16 --
         ;; so the stream's own conses were allocated INSIDE its buffer, and
         ;; every refill's byte count landed on character 7.
         (if (<= total-bytes 2047)
             (rv-emit-addi buf +rv-s8+ +rv-s8+ total-bytes)
             (progn (rv-emit-li buf +rv-t1+ total-bytes)
                    (rv-emit-add buf +rv-s8+ +rv-s8+ +rv-t1+)))
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
                    (offset (- (* (1+ idx) (rv-word-size)) (rv-object-tag))))  ; (1+idx)*word - tag
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
                    (offset (- (* (1+ idx) (rv-word-size)) (rv-object-tag))))
               (if (and (>= offset -2048) (<= offset 2047))
                   (rv-emit-store-word buf rs robj offset)
                   (progn
                     (rv-emit-li buf +rv-t0+ offset)
                     (rv-emit-add buf +rv-t0+ robj +rv-t0+)
                     (rv-emit-store-word buf rs +rv-t0+ 0)))))))

      (#.+op-obj-tag+
       ;; Extract the FOUR-bit tag (+tag-mask+) and return it TAGGED as a
       ;; fixnum, matching translate-x64 and translate-i386 -- both AND with
       ;; #x0F and then SHL 1.  A three-bit mask cannot tell object (1001)
       ;; from cons (0001), and an untagged result is read as half its value.
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         (rv-emit-andi buf +rv-t0+ rs #x0F)   ; +tag-mask+
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)       ; tag as fixnum
         (store-result vd +rv-t0+)))

      (#.+op-obj-subtag+
       ;; Extract 8-bit subtag from header word: load header, andi 0xFF
       (let* ((vd (vreg 0))
              (rs (resolve (vreg 1))))
         ;; translate-x64's contract, which the shared compiler is written
         ;; against: a TAGGED fixnum subtag for a real heap object, and 0 for
         ;; anything else -- never a dereference of a non-object.
         ;;
         ;; RISC-V returned the RAW byte, and read the header of whatever it
         ;; was given.  The compiler compares against TAGGED constants
         ;; ((ash +subtag-bignum+ +fixnum-shift+) and friends), so a raw
         ;; subtag never matched -- and worse, an odd one looks like a pointer:
         ;; %CL-SYM-P's (= (obj-subtag x) #x50) handed = the keyword subtag
         ;; #x53, whose low nibble 3 is the FUNCTION tag, so = took the generic
         ;; numeric path and %IEEE-FLOAT-P dereferenced #x53.  SIGSEGV, in the
         ;; real CL image, the moment object tag 9 let SYMBOL-NAME get that far.
         ;;
         ;; T is excluded explicitly: +T-VALUE+ #xDEAD1009 has the object tag's
         ;; nibble on RV64 but is an immediate, and T-9 is unmapped.  Both
         ;; answers are in registers before the branches, each branch skips a
         ;; fixed run, so the arm is size-stable.
         (rv-emit-li buf +rv-t3+ +t-value+)
         (rv-emit-andi buf +rv-t1+ rs #x0F)   ; +tag-mask+, read RS
         (rv-emit-mv buf +rv-t4+ rs)                  ; keep RS: it may be t0
         (rv-emit-addi buf +rv-t2+ +rv-x0+ (rv-object-tag))
         (rv-emit-addi buf +rv-t0+ +rv-x0+ 0)         ; default: subtag 0
         (rv-emit-bne buf +rv-t1+ +rv-t2+ 20)         ; not an object -> 0
         (rv-emit-beq buf +rv-t4+ +rv-t3+ 16)         ; T -> 0
         (rv-emit-load-word buf +rv-t0+ +rv-t4+ (- (rv-object-tag)))
         (rv-emit-andi buf +rv-t0+ +rv-t0+ #xFF)
         (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)         ; TAGGED fixnum
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
           ;; ZERO-EXTENDING loads: these widths are :u16 and :u32.  LH and
           ;; (on RV64) LW SIGN-extend, so a halfword with bit 15 set, or a word
           ;; with bit 31 set, read back NEGATIVE.  Found on RV32, where a u32
           ;; mem-ref is built from two u16 halves: the staged argv pointer
           ;; #x1000A027 came back as #x1000A027 - #x10000 because its low half
           ;; #xA027 was sign-extended.  On RV64 the same LW made every u32 at
           ;; or above 2^31 negative -- latent there, not yet hit.
           (1 (rv-emit-lhu buf +rv-t0+ raddr 0))    ; u16
           (2 (if *riscv-64-bit*
                  (rv-emit-u32 buf (rv-encode-i-type 0 raddr #x6 +rv-t0+ #x03)) ; LWU
                  (rv-emit-lw buf +rv-t0+ raddr 0)))  ; u32 (RV32: a full word)
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
         ;; ALWAYS the two-instruction form.  NEVER size this by distance.
         ;;
         ;; This used to be `if the offset fits in JAL's +/-1 MB, emit 4 bytes,
         ;; else emit 8' — and that is a PASS-DEPENDENT SIZE, which corrupts the
         ;; whole label map.  Pass 1 measures sizes while BUILDING the function
         ;; table, so a call whose target is not yet known measures with
         ;; rel-offset 0 (short); pass 2 knows the real distance and may emit
         ;; long.  One call that changes size shifts every native offset after
         ;; it, and every branch that resolves through the map then lands in the
         ;; wrong place.
         ;;
         ;; MEASURED, and the shape is worth remembering: the real CL image ran
         ;; ~885 blocks and then a plain `j' inside MAKE-ARRAY jumped 92 KB
         ;; BACKWARD into the middle of SLOT-VALUE, onto an instruction that
         ;; happened to be an ECALL — so the visible failure was a nonsense
         ;; mmap(NULL, 956301312, MAP_FIXED) returning EPERM, then a SIGSEGV at
         ;; si_addr=0x56.  Three layers between the cause and the symptom.
         ;;
         ;; Invisible on every ladder image, because a few-KB image has every
         ;; call inside +/-1 MB, so both passes agree on 4 bytes.  Same cliff
         ;; class as the code buffer, the conditional branch and the entry jump:
         ;; a SIZE that depends on a DISTANCE is a bug in a two-pass assembler
         ;; unless the passes are iterated to a fixpoint.
         (rv-emit-call buf rel-offset)))

      ;; ---- Double-precision floating point ----
      ;;
      ;; A double is a FOUR-SLOT object, subtag #x60, whose 64 IEEE bits live as
      ;; four TAGGED 16-BIT CHUNKS — slot k holds bits (63-16k)..(48-16k) — because
      ;; a raw 64-bit pattern does not fit in a slot that carries a 62-bit fixnum.
      ;; Exactly translate-x64's representation; the only difference here is WHERE
      ;; the slots are, since this target tags objects with 2 and has no padding
      ;; word: slot k is at tagged + (1+k)*word - 2, i.e. +6/+14/+22/+30.
      ;;
      ;; RV32 GOES THROUGH MEMORY.  FMV.D.X / FMV.X.D and FCVT.D.L / FCVT.L.D are
      ;; RV64-only, so each arm below has an RV32 branch using rv32-float-unbox /
      ;; rv32-float-box (SH chunks + FLD, FSD + LHU chunks) and the 32-bit
      ;; converts.  Zfa's FMVH.X.D / FMVP.D.X would do it in registers, but plenty
      ;; of RV32D hardware lacks Zfa, so it is not the baseline.
      ((#.+op-fadd+ #.+op-fsub+ #.+op-fmul+ #.+op-fdiv+)
       (if (not *riscv-64-bit*)
           ;; RV32: through memory -- see rv32-float-unbox.  t3 is the chunk
           ;; scratch because RESOLVE/RESOLVE2 may hand back t0/t1 as ra/rb.
           (let* ((vd (vreg 0))
                  (ra (resolve (vreg 1)))
                  (rb (resolve2 (vreg 2))))
             (rv32-float-unbox buf ra 0 +rv-t3+)
             (rv32-float-unbox buf rb 1 +rv-t3+)
             (cond ((= opcode +op-fadd+) (rv-emit-fadd-d buf 0 0 1))
                   ((= opcode +op-fsub+) (rv-emit-fsub-d buf 0 0 1))
                   ((= opcode +op-fmul+) (rv-emit-fmul-d buf 0 0 1))
                   (t                    (rv-emit-fdiv-d buf 0 0 1)))
             (rv32-float-box buf 0 +rv-t3+ +rv-t2+)
             (store-result vd +rv-t2+))
           (let* ((vd (vreg 0))
                  (ra (resolve (vreg 1)))
                  (rb (resolve2 (vreg 2))))
             ;; Unbox both operands into f0 and f1.
             (rv-float-load-bits buf ra +rv-t0+ +rv-t1+)
             (rv-emit-fmv-d-x buf 0 +rv-t0+)
             (rv-float-load-bits buf rb +rv-t0+ +rv-t1+)
             (rv-emit-fmv-d-x buf 1 +rv-t0+)
             (cond ((= opcode +op-fadd+) (rv-emit-fadd-d buf 0 0 1))
                   ((= opcode +op-fsub+) (rv-emit-fsub-d buf 0 0 1))
                   ((= opcode +op-fmul+) (rv-emit-fmul-d buf 0 0 1))
                   (t                    (rv-emit-fdiv-d buf 0 0 1)))
             (rv-emit-fmv-x-d buf +rv-t0+ 0)
             (rv-float-box buf +rv-t0+ +rv-t1+ +rv-t2+)
             (store-result vd +rv-t2+))))

      (#.+op-itof+
       ;; Tagged integer -> a fresh double.  The value arrives TAGGED, so it is
       ;; untagged with an arithmetic shift first: FCVT.D.L converts the integer
       ;; VALUE, not its tagged encoding.
       (if (not *riscv-64-bit*)
           ;; RV32: FCVT.D.L is RV64-only; a 32-bit fixnum's value fits FCVT.D.W.
           (let* ((vd (vreg 0))
                  (rs (resolve (vreg 1))))
             (rv-emit-srai buf +rv-t0+ rs 1)
             (rv-emit-fcvt-d-w buf 0 +rv-t0+)
             (rv32-float-box buf 0 +rv-t3+ +rv-t2+)
             (store-result vd +rv-t2+))
           (let* ((vd (vreg 0))
                  (rs (resolve (vreg 1))))
             (rv-emit-srai buf +rv-t0+ rs 1)
             (rv-emit-fcvt-d-l buf 0 +rv-t0+)
             (rv-emit-fmv-x-d buf +rv-t0+ 0)
             (rv-float-box buf +rv-t0+ +rv-t1+ +rv-t2+)
             (store-result vd +rv-t2+))))

      (#.+op-ftoi+
       ;; Double -> tagged integer, truncating toward zero (FCVT.L.D with rm=RTZ).
       (if (not *riscv-64-bit*)
           ;; RV32: unbox through memory, truncate with FCVT.W.D (rm=RTZ).
           (let* ((vd (vreg 0))
                  (rs (resolve (vreg 1))))
             (rv32-float-unbox buf rs 0 +rv-t3+)
             (rv-emit-fcvt-w-d buf +rv-t0+ 0)
             (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)      ; tag as a fixnum
             (store-result vd +rv-t0+))
           (let* ((vd (vreg 0))
                  (rs (resolve (vreg 1))))
             (rv-float-load-bits buf rs +rv-t0+ +rv-t1+)
             (rv-emit-fmv-d-x buf 0 +rv-t0+)
             (rv-emit-fcvt-l-d buf +rv-t0+ 0)
             (rv-emit-slli buf +rv-t0+ +rv-t0+ 1)      ; tag as a fixnum
             (store-result vd +rv-t0+))))

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
         ;; #xFFFFFFF0 is the compiler's UNDEFINED-NAME sentinel: load NIL so
         ;; FUNCALL signals UNDEFINED-FUNCTION (translate-x64's contract).  A
         ;; miss used to give rel 0 -- a pointer to this very AUIPC, tagged as a
         ;; function.  Tested on the operand VALUE, so both passes emit the same
         ;; three instructions.
         (if (= target-idx #xFFFFFFF0)
             (progn (rv-emit-mv buf +rv-t0+ +rv-s10+)
                    (rv-emit-nop buf)
                    (rv-emit-nop buf))
             (progn
               (rv-emit-auipc buf +rv-t0+ (logand hi20 #xFFFFF))
               (rv-emit-addi buf +rv-t0+ +rv-t0+ (logand lo12 #xFFF))
               (rv-emit-ori buf +rv-t0+ +rv-t0+ +tag-function+)))
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
       ;;
       ;; HOSTED, THE OVER-LIMIT ARM IS A LOUD STOP, NOT AN ECALL.  There is no
       ;; hosted RISC-V collector yet, so VA >= VL means the heap is exhausted.
       ;; The bare-metal `ecall' here traps to a machine-mode handler; hosted it
       ;; was a real SYSCALL with whatever a7 held, the kernel answered ENOSYS
       ;; (-38) IN a0 -- which is V0 -- and every allocation after the limit
       ;; silently clobbered a live register.  The hosted RV32 CL image crossed
       ;; the limit during boot and died far away in BIGNUM-ASH with a0 = -38.
       ;; EBREAK with a7 = #x0F0 ("heap exhausted") stops at the allocation that
       ;; ran out, which is where the answer is.  Size-stable: two arms of the
       ;; same length.
       (if *riscv-linux-mode*
           (progn
             (rv-emit-blt buf +rv-s8+ +rv-s9+ 12)       ; skip if VA < VL
             (rv-emit-addi buf +rv-a7+ +rv-x0+ #x0F0)   ; heap-exhausted marker
             (rv-emit-ebreak buf))
           (progn
             (rv-emit-blt buf +rv-s8+ +rv-s9+ 8)        ; skip if VA < VL
             (rv-emit-ecall buf)                        ; trigger GC (bare)
             (rv-emit-nop buf))))

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
       ;; Preemption check, which the compiler plants at EVERY LOOP BACK-EDGE.
       ;;
       ;; HOSTED IT IS A NOP, as translate-x64 makes it under *x64-linux-mode*
       ;; ("LINUX: NOP (no scheduler, no deadline)") and as i386, ppc, 68k and
       ;; arm32 make it everywhere.  RISC-V emitted `li a7, 10; ecall' -- a
       ;; placeholder "yield syscall" -- and on Linux syscall 10 is FGETXATTR.
       ;; So every iteration of every loop in the image made a real system call
       ;; that failed with EFAULT on its Lisp-value arguments: 192,180 of them in
       ;; the first 30 s of boot, seen with `qemu-riscv64-static -strace'.  That
       ;; was essentially the whole of the real CL image's 866 s boot against
       ;; x64's 2.7 s -- the same algorithm (package tables measured identical)
       ;; paying a kernel round trip through qemu's syscall layer per iteration.
       ;;
       ;; Bare metal keeps the ecall: there it traps to the machine-mode handler
       ;; the bare boot installs, and the bare gate passes with it.
       (if *riscv-linux-mode*
           (rv-emit-nop buf)
           (progn
             (rv-emit-addi buf +rv-a7+ +rv-x0+ #x0A)  ; yield trap number
             (rv-emit-ecall buf))))

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
      ;; tag (rv-object-tag) T, header at obj-T, element k at obj-T + (1+k)*word.  The index
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
         ;; so (rv-object-tag) is exact at both widths)
         (rv-emit-addi buf +rv-t0+ +rv-s8+ (rv-object-tag))
         (rv-emit-add buf +rv-s8+ +rv-s8+ +rv-t1+)
         (store-result vd +rv-t0+)))

      (#.+op-aref+
       (let* ((vd (vreg 0))
              (robj (resolve (vreg 1)))
              (ridx (resolve2 (vreg 2))))
         (rv-emit-slli buf +rv-t2+ ridx (rv-index-shift))
         (rv-emit-add buf +rv-t2+ +rv-t2+ robj)
         (rv-emit-load-word buf +rv-t2+ +rv-t2+ (- (rv-word-size) (rv-object-tag)))
         (store-result vd +rv-t2+)))

      (#.+op-aset+
       ;; (aset Vobj Vidx Vs).  The value is materialised BEFORE t1 is reused
       ;; as the address, and neither resolve target can be t1.
       (let* ((robj (resolve (vreg 0)))
              (rval (rv-vreg-or-load buf (vreg 2) +rv-t2+))
              (ridx (rv-vreg-or-load buf (vreg 1) +rv-t1+)))
         (rv-emit-slli buf +rv-t1+ ridx (rv-index-shift))
         (rv-emit-add buf +rv-t1+ +rv-t1+ robj)
         (rv-emit-store-word buf rval +rv-t1+ (- (rv-word-size) (rv-object-tag)))))

      (#.+op-array-len+
       ;; count = (header >> 8) & 0xFFFFFF, returned TAGGED.
       (let* ((vd (vreg 0))
              (robj (resolve (vreg 1))))
         (rv-emit-load-word buf +rv-t0+ robj (- (rv-object-tag)))
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
         (rv-emit-addi buf +rv-t0+ +rv-s8+ (rv-object-tag))
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
         (rv-emit-addi buf +rv-t0+ +rv-s8+ (rv-object-tag))
         (rv-emit-add buf +rv-s8+ +rv-s8+ +rv-t1+)
         (store-result vd +rv-t0+)))

      (#.+op-u8-ref+
       ;; (u8-ref Vd Varr Vidx) — Vidx TAGGED, result TAGGED.
       (let* ((vd (vreg 0))
              (rarr (resolve (vreg 1)))
              (ridx (resolve2 (vreg 2))))
         (rv-emit-srai buf +rv-t2+ ridx 1)
         (rv-emit-add buf +rv-t2+ +rv-t2+ rarr)
         (rv-emit-lbu buf +rv-t2+ +rv-t2+ (- (rv-word-size) (rv-object-tag)))
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
         (rv-emit-sb buf +rv-t2+ +rv-t1+ (- (rv-word-size) (rv-object-tag)))))

      ;; ---- System area pointers ----
      ;; A SAP is a one-slot object, subtag #x16: header (1<<8)|#x16 then the
      ;; raw address.  Header plus slot is exactly ONE GRANULE at either width.
      (#.+op-sap-new+
       (let* ((vd (vreg 0))
              (raddr (resolve (vreg 1) +rv-t2+)))
         (rv-emit-li buf +rv-t0+ #x116)
         (rv-emit-store-word buf +rv-t0+ +rv-s8+ 0)
         (rv-emit-store-word buf raddr +rv-s8+ (rv-word-size))
         (rv-emit-addi buf +rv-t0+ +rv-s8+ (rv-object-tag))
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
         (rv-emit-load-word buf +rv-t0+ rsap (- (rv-word-size) (rv-object-tag)))
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
       ;; Unrecognised opcode: trap with the number in a7, and RECORD it so a
       ;; build reports the gap instead of shipping a landmine.
       (rv-note-unimpl opcode)
       (rv-emit-addi buf +rv-a7+ +rv-x0+ opcode)
       (rv-emit-ebreak buf)))))

;;; ============================================================
;;; Function Prologue / Epilogue
;;; ============================================================

;;; ============================================================
;;; handler-case: the SETJMP / LONGJMP / CLEAR-HANDLER triple
;;; ============================================================
;;;
;;; These three traps ARE handler-case, and the real CL image emits 1540 of them
;;; (clear-handler 709, setjmp 708, longjmp 123 — measured, not guessed).
;;;
;;; THE FRAME PUSH AND POP ARE INLINED, where i386 and AArch64 call a helper.
;;; This translator resolves branches through a label map keyed by BYTECODE
;;; OFFSET, so it has no way to name an arbitrary emitted label and no
;;; call-a-helper idiom; adding one is a bigger change than inlining twenty
;;; instructions.  Cost, stated: ~1400 sites x ~15 instructions x 4 bytes is
;;; about 84 KB in a 40 MB image.
;;;
;;; EVERY BRANCH DISTANCE HERE IS COUNTED BY HAND, in units of whole
;;; instructions, because every instruction this back end emits is four bytes.
;;; That is only safe as long as the emitters below stay one instruction each --
;;; RV-EMIT-LI IS NOT ONE INSTRUCTION and must never appear between a branch and
;;; its target.  Where an address is needed inside a branched region it is
;;; materialised BEFORE the branch.

(defparameter *rv-jmpbuf-addr* #x80700180  ; BARE default; riscv-set-linux-mode moves it
  "Twelve words: sp, fp, resume-ip, then V4..V11.

   WHY ALL EIGHT V-REGS.  V4..V11 map to s11,s1..s7 — every one a CALLEE-SAVED
   register, saved by the prologue of whichever function uses it and restored by
   that function's EPILOGUE.  A longjmp jumps over those epilogues, so anything
   the abandoned computation put in them would still be there.  x64 saves only
   RBX because its V5..V8 are caller-saved and live in frame slots; here they are
   not, so all eight travel in the jmpbuf.

   VA / VL / VN (s8/s9/s10) are DELIBERATELY ABSENT.  Restoring the allocation
   pointer would un-allocate everything the abandoned computation built — the
   condition object among it, which the handler is about to read.")
(defconstant +rv-jmpbuf-words+ 12)
(defconstant +rv-jmpbuf-size+ 96)

(defparameter *rv-hstack-depth-addr* #x80700400  ; BARE default; riscv-set-linux-mode moves it
  "Handler-stack depth.  The mmap'd heap is MAP_ANONYMOUS, so it starts at 0.")
(defparameter *rv-hstack-base-addr* #x80700408  ; BARE default; riscv-set-linux-mode moves it
  "Frame 0.  Frame N is at base + 96*N.")
(defparameter *rv-hstack-max-depth* 21
  "Frames that fit between the stack base and #x10000C10, which is where the
   shared low-memory map puts the longjmp scratch.  21 nested handler-cases; a
   deeper one is CAPPED (see below) rather than allowed to run off the end.")

(defparameter *rv-longjmp-scratch-addr* #x80700300  ; BARE default; riscv-set-linux-mode moves it
  "Twelve words.  LONGJMP copies the jmpbuf here BEFORE the pop overwrites it --
   the pop restores the OUTER frame into the jmpbuf, and longjmp still needs the
   INNER one it is jumping to.  #x300 because #x250..#x3FF is the one free span
   in the fixed block below the handler stack.")
(defparameter *rv-hstack-capped-addr* #x80700360  ; BARE default; riscv-set-linux-mode moves it
  "LIVE count of capped pushes.  A capped push stores NO frame, so the
   CLEAR-HANDLER that textually matches it must ABSORB its pop instead of
   draining a real frame — otherwise an over-deep handler-case silently pops
   somebody else's.  LONGJMP zeroes it: it unwinds past every capped (strictly
   inner) frame at once, so their pending absorbs must not fire against outer
   pops afterwards.")

(defun rv-emit-handler-push (buf)
  "Stack the CURRENT jmpbuf so a nested handler-case does not overwrite it.
   Leaves t3 = 0 if a frame was stored, 1 if the push was CAPPED.

   NESTING IS NOT DEFERRABLE.  SEQUENTIAL handler-cases work with a single-level
   implementation -- and that is exactly what init-all-globals does, one per init
   thunk -- so an implementation without this passes the early milestones and
   fails later, the worst available failure shape.

   Branch distances are BACK-PATCHED from measured positions, never counted."
  (rv-emit-li buf +rv-t1+ *rv-hstack-depth-addr*)
  (rv-emit-load-word buf +rv-t2+ +rv-t1+ 0)              ; t2 = depth
  (rv-emit-addi buf +rv-t3+ +rv-x0+ 1)                   ; assume capped
  (rv-emit-addi buf +rv-t4+ +rv-x0+ *rv-hstack-max-depth*)
  ;; Both addresses are materialised BEFORE the branch.  RV-EMIT-LI is
  ;; multi-instruction; back-patching no longer cares about the count, but
  ;; keeping LI out of a branched span keeps both arms the shape of the rest
  ;; of this file.
  (rv-emit-li buf +rv-t5+ *rv-hstack-base-addr*)
  (rv-emit-li buf +rv-t6+ *rv-jmpbuf-addr*)
  (let ((to-capped (rv-current-offset buf)))
    (rv-emit-bge buf +rv-t2+ +rv-t4+ 0)                  ; patched below
    ;; dest = base + depth*96, built as 64+32 so nothing here is multi-word
    (rv-emit-slli buf +rv-t0+ +rv-t2+ 6)
    (rv-emit-add buf +rv-t5+ +rv-t5+ +rv-t0+)
    (rv-emit-slli buf +rv-t0+ +rv-t2+ 5)
    (rv-emit-add buf +rv-t5+ +rv-t5+ +rv-t0+)
    (dotimes (i +rv-jmpbuf-words+)
      (rv-emit-load-word buf +rv-t0+ +rv-t6+ (* 8 i))
      (rv-emit-store-word buf +rv-t0+ +rv-t5+ (* 8 i)))
    (rv-emit-addi buf +rv-t2+ +rv-t2+ 1)
    (rv-emit-store-word buf +rv-t2+ +rv-t1+ 0)
    (rv-emit-addi buf +rv-t3+ +rv-x0+ 0)                 ; stored, not capped
    (let ((to-done (rv-current-offset buf)))
      (rv-emit-jal buf +rv-x0+ 0)                        ; patched below
      (rv-patch-branch-here buf to-capped)
      ;; --- capped: bump the LIVE capped count, leaving t3 = 1
      (rv-emit-li buf +rv-t1+ *rv-hstack-capped-addr*)
      (rv-emit-load-word buf +rv-t0+ +rv-t1+ 0)
      (rv-emit-addi buf +rv-t0+ +rv-t0+ 1)
      (rv-emit-store-word buf +rv-t0+ +rv-t1+ 0)
      (rv-patch-jal-here buf to-done))))

(defun rv-emit-handler-pop (buf)
  "Restore the top stacked frame into the jmpbuf, or ZERO the jmpbuf when the
   stack is empty.  Clobbers t0..t6 only.

   THREE ARMS, and the first one is the subtle one: a pop that textually matches
   a CAPPED push must ABSORB it and touch nothing else.  Draining a real frame
   there would pop a handler that is still armed — the drain-to-depth-0 class."
  ;; --- arm 1: capped > 0 ?  absorb and return.
  (rv-emit-li buf +rv-t1+ *rv-hstack-capped-addr*)
  (rv-emit-load-word buf +rv-t0+ +rv-t1+ 0)
  (let ((to-not-capped (rv-current-offset buf)))
    (rv-emit-beq buf +rv-t0+ +rv-x0+ 0)                  ; patched
    (rv-emit-addi buf +rv-t0+ +rv-t0+ -1)
    (rv-emit-store-word buf +rv-t0+ +rv-t1+ 0)
    (let ((to-done-1 (rv-current-offset buf)))
      (rv-emit-jal buf +rv-x0+ 0)                        ; patched
      (rv-patch-branch-here buf to-not-capped)
      ;; --- arm 2: depth > 0 ?  restore frame[depth-1] into the jmpbuf.
      (rv-emit-li buf +rv-t1+ *rv-hstack-depth-addr*)
      (rv-emit-load-word buf +rv-t2+ +rv-t1+ 0)
      (rv-emit-li buf +rv-t5+ *rv-hstack-base-addr*)
      (rv-emit-li buf +rv-t6+ *rv-jmpbuf-addr*)
      (let ((to-empty (rv-current-offset buf)))
        (rv-emit-beq buf +rv-t2+ +rv-x0+ 0)              ; patched
        (rv-emit-addi buf +rv-t2+ +rv-t2+ -1)
        (rv-emit-store-word buf +rv-t2+ +rv-t1+ 0)
        (rv-emit-slli buf +rv-t0+ +rv-t2+ 6)
        (rv-emit-add buf +rv-t5+ +rv-t5+ +rv-t0+)
        (rv-emit-slli buf +rv-t0+ +rv-t2+ 5)
        (rv-emit-add buf +rv-t5+ +rv-t5+ +rv-t0+)
        (dotimes (i +rv-jmpbuf-words+)
          (rv-emit-load-word buf +rv-t0+ +rv-t5+ (* 8 i))
          (rv-emit-store-word buf +rv-t0+ +rv-t6+ (* 8 i)))
        (let ((to-done-2 (rv-current-offset buf)))
          (rv-emit-jal buf +rv-x0+ 0)                    ; patched
          (rv-patch-branch-here buf to-empty)
          ;; --- arm 3: empty.  ZERO THE WHOLE JMPBUF, not just word 0.
          ;; Word 0 = 0 is the "no handler armed" sentinel LONGJMP tests, but
          ;; leaving the other eleven words holding a dead frame's callee-saved
          ;; registers would leave stale values for the next restore to load.
          (dotimes (i +rv-jmpbuf-words+)
            (rv-emit-store-word buf +rv-x0+ +rv-t6+ (* 8 i)))
          (rv-patch-jal-here buf to-done-2)
          (rv-patch-jal-here buf to-done-1))))))

(defun rv-float-load-bits (buf ptr acc tmp)
  "Reassemble the four tagged 16-bit chunks of the double whose TAGGED pointer is
   in PTR into the 64-bit IEEE pattern in ACC.  TMP is clobbered.

   Each `slli 48 / srli N' pair both MASKS the chunk to 16 bits and positions it,
   so a slot carrying junk above bit 15 cannot corrupt its neighbour — the same
   reason translate-x64's version shifts rather than ands."
  (let ((ws (rv-word-size)))
    ;; slot 0 -> bits 63..48
    (rv-emit-load-word buf acc ptr (- ws (rv-object-tag)))
    (rv-emit-srai buf acc acc 1)
    (rv-emit-slli buf acc acc 48)
    ;; slots 1..3 -> bits 47..32, 31..16, 15..0
    (loop for k from 1 to 3
          do (rv-emit-load-word buf tmp ptr (- (* (1+ k) ws) (rv-object-tag)))
             (rv-emit-srai buf tmp tmp 1)
             (rv-emit-slli buf tmp tmp 48)
             (rv-emit-srli buf tmp tmp (* 16 k))
             (rv-emit-or buf acc acc tmp))))

(defun rv-float-box (buf bits tmp out)
  "Allocate a fresh double object holding the 64 IEEE bits in BITS, leaving its
   TAGGED pointer in OUT.  TMP is clobbered; BITS is preserved.

   Header is (count=4)<<8 | subtag #x60, then the four tagged 16-bit chunks.  The
   allocation is FIVE words (header + 4 slots) rounded up to the granule, so the
   bump pointer stays aligned and (rv-object-tag) remains exact."
  (let* ((ws (rv-word-size))
         (g (rv-granule))
         (bytes (logand (+ (* 5 ws) (1- g)) (lognot (1- g)))))
    (rv-emit-li buf tmp (logior #x60 (ash 4 8)))
    (rv-emit-store-word buf tmp +rv-s8+ 0)
    ;; chunk k = bits (63-16k)..(48-16k), stored TAGGED.
    (loop for k from 0 to 3
          do (rv-emit-srli buf tmp bits (- 48 (* 16 k)))
             (rv-emit-slli buf tmp tmp 48)         ; mask to 16 bits
             (rv-emit-srli buf tmp tmp 48)
             (rv-emit-slli buf tmp tmp 1)          ; tag as a fixnum
             (rv-emit-store-word buf tmp +rv-s8+ (* (1+ k) ws)))
    (rv-emit-addi buf out +rv-s8+ (rv-object-tag)) ; object tag
    (rv-emit-addi buf +rv-s8+ +rv-s8+ bytes)))

(defun rv32-float-unbox (buf ptr fd tmp)
  "RV32: load the double whose TAGGED pointer is in PTR into FP register FD.

   RV32D HAS NO FMV.D.X -- there is no 64-bit integer register to move from --
   so the value goes through memory, the standard RV32D route.  And since a
   boxed double is already four tagged 16-bit chunks, the chunks are written
   straight into a 16-byte stack scratch with SH (which keeps the low 16 bits,
   so it also masks) and loaded with one FLD: no 64-bit integer arithmetic,
   which RV32 cannot do in a register anyway.  Little-endian, so chunk k -- bits
   (63-16k)..(48-16k) -- lives at byte offset 6-2k.  The scratch is allocated by
   moving sp, not taken from below it: on hosted Linux a signal frame may be
   written below sp at any instruction.  TMP must not be PTR."
  (let ((ws (rv-word-size)))
    (rv-emit-addi buf +rv-sp+ +rv-sp+ -16)
    (loop for k from 0 to 3
          do (rv-emit-load-word buf tmp ptr (- (* (1+ k) ws) (rv-object-tag)))
             (rv-emit-srai buf tmp tmp 1)                ; untag the chunk
             (rv-emit-sh buf tmp +rv-sp+ (- 6 (* 2 k))))
    (rv-emit-fld buf fd +rv-sp+ 0)
    (rv-emit-addi buf +rv-sp+ +rv-sp+ 16)))

(defun rv32-float-box (buf fs tmp out)
  "RV32: box the double in FP register FS as a fresh four-chunk object, leaving
   its TAGGED pointer in OUT.  The inverse of rv32-float-unbox: FSD to a stack
   scratch, then LHU each 16-bit chunk back out, tag it, and store it in its
   slot.  Same layout and granule rounding as rv-float-box."
  (let* ((ws (rv-word-size))
         (g (rv-granule))
         (bytes (logand (+ (* 5 ws) (1- g)) (lognot (1- g)))))
    (rv-emit-addi buf +rv-sp+ +rv-sp+ -16)
    (rv-emit-fsd buf fs +rv-sp+ 0)
    (rv-emit-li buf tmp (logior #x60 (ash 4 8)))
    (rv-emit-store-word buf tmp +rv-s8+ 0)
    (loop for k from 0 to 3
          do (rv-emit-lhu buf tmp +rv-sp+ (- 6 (* 2 k)))
             (rv-emit-slli buf tmp tmp 1)                ; tag as a fixnum
             (rv-emit-store-word buf tmp +rv-s8+ (* (1+ k) ws)))
    (rv-emit-addi buf +rv-sp+ +rv-sp+ 16)
    (rv-emit-addi buf out +rv-s8+ (rv-object-tag))
    (rv-emit-addi buf +rv-s8+ +rv-s8+ bytes)))

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
    ;; s8 (VA), s9 (VL) and s10 (VN) ARE DELIBERATELY NOT SAVED.  Their three
    ;; slots at total-frame-80/-88/-96 stay unused; see rv-emit-epilogue.
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
    ;; VA, VL AND VN ARE GLOBAL STATE AND MUST NOT BE RESTORED.
    ;;
    ;; s8 is VA, THE ALLOCATION POINTER.  Restoring it on return rolls the heap
    ;; pointer back over everything the callee allocated, so the caller's next
    ;; CONS is handed memory that is already live.  translate-x64's
    ;; emit-function-prologue states the rule -- "In kernel mode, R12 (alloc
    ;; ptr), R14 (alloc limit), R15 (nil) are global state that must NOT be
    ;; saved/restored.  RBX (V4) is callee-saved" -- and saves RBX alone.
    ;; RISC-V saved and restored s1-s11, all eleven.
    ;;
    ;; MEASURED, in the real CL image, as the FIRST NINE CONSES EVER ALLOCATED.
    ;; (make-hash-table) with no options builds its table innermost-cons-first:
    ;; (cons 0 t), then the bucket-holder around it, then six metadata cells,
    ;; then the table.  Conses 3-9 were correct and conses 1-2 held a
    ;; (name-hash . value) pair and a one-element list of it -- the alist that
    ;; SET-SYMBOL-VALUE's puthash built AFTERWARDS, on top of them, because
    ;; make-hash-table's return had rolled VA back to the start of the heap.
    ;; %GV-CELL then read the clobbered holder, found a cons where the bucket
    ;; vector belongs, AREF'd it, got 0 out of unwritten heap, and signalled a
    ;; type error -- from a function %SIGNAL-TYPE-ERROR itself needs, so the
    ;; two recursed and the image presented as a hang in init with an empty log.
    ;; It is also why only ONE global of 683 was ever registered.
    ;;
    ;; s9 is VL, the allocation LIMIT, which a collection legitimately moves,
    ;; and s10 is VN, a constant.  Neither belongs to a frame either.  V4 (s11)
    ;; is a real virtual register and stays callee-saved, exactly as RBX is.
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
          ;; PASS 1'S MAP IS THE AUTHORITY.  This used to REWRITE the entry with
          ;; pass 2's position, which quietly mixes the two passes: a BACKWARD
          ;; branch then resolves against a pass-2 offset while a FORWARD branch
          ;; (whose target pass 2 has not reached yet) resolves against pass 1's.
          ;; That is only harmless while every instruction measures identically in
          ;; both passes — exactly the property a variable-size emitter breaks.
          ;;
          ;; So instead of rewriting, CHECK.  A size that differs between passes is
          ;; now a NAMED BUILD FAILURE carrying the opcode, rather than a wild
          ;; branch 92 KB into another function discovered by reading a 31 MB
          ;; instruction trace.  That is how op-call's distance-dependent sizing was
          ;; found, and it cost hours; this assertion would have printed it.
          (let ((expected (gethash mvm-pc label-map))
                (actual (rv-current-offset final-buf)))
            (unless (eql expected actual)
              (error "riscv two-pass size mismatch at mvm-pc ~D (opcode #x~2,'0X): ~
                      pass 1 measured offset ~D, pass 2 is at ~D (delta ~D).  Some ~
                      emitter's SIZE depends on a value that differs between the ~
                      passes — most often a DISTANCE, since pass 1 builds the label ~
                      and function maps while measuring."
                     mvm-pc opcode expected actual (- actual expected))))
          ;; Pass next-pc for branch offset computation (MVM offsets are from end of insn)
          (translate-mvm-insn-riscv final-buf opcode operands next-pc
                                    :label-map label-map
                                    :function-table native-fn-table
                                    :pass2 t)))
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
  (setf *rv-globals-base* (if on *rv-hosted-globals-base* #x80700000))
  ;; THE HANDLER STACK MOVES WITH THE MODE TOO.  Its hosted addresses are the
  ;; shared #x10000xxx contract (the hosted CLI reads #x10000180/#x10000400 as
  ;; literals), and on QEMU virt #x10000000 is the NS16550 UART -- so on bare
  ;; metal every SETJMP wrote its jmpbuf into MMIO space, and r19-handler passed
  ;; hosted and failed bare.  Bare keeps the SAME low offsets inside the DRAM
  ;; convention block at #x80700000, where #x00-#x17 are nargs/cenv/mv-count
  ;; and #x180-#xC10 is otherwise unused.
  (let ((base (if on #x10000000 #x80700000)))
    (setf *rv-jmpbuf-addr*          (+ base #x180)
          *rv-longjmp-scratch-addr* (+ base #x300)
          *rv-hstack-capped-addr*   (+ base #x360)
          *rv-hstack-depth-addr*    (+ base #x400)
          *rv-hstack-base-addr*     (+ base #x408))))

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
