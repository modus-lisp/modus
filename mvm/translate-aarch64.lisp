;;;; translate-aarch64.lisp - MVM Bytecode → AArch64 Native Code Translator
;;;;
;;;; Translates MVM virtual instructions into AArch64 (ARM64) machine code.
;;;; Includes a self-contained AArch64 instruction encoder and the main
;;;; translation loop that walks decoded MVM bytecode and emits native
;;;; 32-bit instruction words.
;;;;
;;;; AArch64 register mapping (from target.lisp):
;;;;   V0 → x0, V1 → x1, V2 → x2, V3 → x3    (args)
;;;;   V4 → x19, V5 → x20, V6 → x21, V7 → x22  (callee-saved)
;;;;   V8 → x23, V9-V15 → stack spill
;;;;   VR → x0   (return, aliases V0)
;;;;   VA → x24  (alloc pointer)
;;;;   VL → x25  (alloc limit)
;;;;   VN → x26  (NIL)
;;;;   VSP → sp  (stack pointer)
;;;;   VFP → x29 (frame pointer)
;;;;
;;;; Scratch registers:
;;;;   x16 (IP0) - intra-procedure scratch 0
;;;;   x17 (IP1) - intra-procedure scratch 1
;;;;   x30 (LR)  - link register

(in-package :modus.mvm)

;;; ============================================================
;;; Configurable UART Base Address
;;; ============================================================

(defvar *aarch64-serial-base* nil
  "Base address of the UART for AArch64 serial-out.
   If nil (default), build-image uses the boot descriptor's :serial-base,
   falling back to 0x09000000 (QEMU virt PL011).
   Set explicitly to override: 0x3F215040 for BCM2837 mini UART,
   0x3F201000 for BCM2837 PL011, 0x1F00030000 for Raspberry Pi 5.")

(defvar *aarch64-serial-width* 0
  "Store width for serial write: 0=byte (strb, PL011), 2=word (str, mini UART).
   BCM2835 mini UART requires 32-bit stores to AUX_MU_IO register.")

(defvar *aarch64-serial-tx-poll* nil
  "TX-ready poll config: nil (no poll) or (offset bit polarity).
   offset = byte offset from serial base to status register.
   bit = bit number to test.
   polarity = :tbz (wait while clear) or :tbnz (wait while set).
   Mini UART: '(#x14 5 :tbz) — LSR at base+0x14, bit 5 = TX empty, wait while clear.
   PL011:     '(#x18 5 :tbnz) — UARTFR at base+0x18, bit 5 = TXFF, wait while set.")

(defvar *aarch64-sched-lock-addr* nil
  "Scheduler lock address for RESTORE-CONTEXT unlock. Nil = skip (no actors).
   Set to the sched-lock-addr value for actor builds (e.g. #x41200200 for QEMU virt,
   #x02000200 for RPi). Non-actor builds get zero overhead.")

(defvar *aarch64-setup-irq-enable* nil
  "When non-nil, TRAP #x0320 (setup-irq) emits GICv2 + virtual timer init.
   Set to t for QEMU virt standalone builds. Nil for fixpoint (no GIC access).")

(defvar *aarch64-yield-nop* nil
  "When non-nil, YIELD emits NOP instead of SEV+WFE. Required for QEMU raspi3b
   where WFE halts the CPU with no wake event (no GIC to generate events).")

;;; ============================================================
;;; AArch64 Physical Register Numbers
;;; ============================================================

(defconstant +a64-x0+   0)
(defconstant +a64-x1+   1)
(defconstant +a64-x2+   2)
(defconstant +a64-x3+   3)
(defconstant +a64-x16+ 16)   ; IP0 scratch
(defconstant +a64-x17+ 17)   ; IP1 scratch
(defconstant +a64-x19+ 19)
(defconstant +a64-x20+ 20)
(defconstant +a64-x21+ 21)
(defconstant +a64-x22+ 22)
(defconstant +a64-x23+ 23)
(defconstant +a64-x24+ 24)   ; VA alloc pointer
(defconstant +a64-x25+ 25)   ; VL alloc limit
(defconstant +a64-x26+ 26)   ; VN nil
(defconstant +a64-x29+ 29)   ; FP
(defconstant +a64-x30+ 30)   ; LR
(defconstant +a64-sp+  31)   ; SP (context-dependent encoding with XZR)
(defconstant +a64-xzr+ 31)   ; Zero register (same encoding as SP)

;;; ============================================================
;;; Virtual → Physical Register Mapping
;;; ============================================================

(defparameter *a64-vreg-to-phys*
  (let ((map (make-array 23 :initial-element nil)))
    ;; GPR arguments
    (setf (aref map +vreg-v0+) +a64-x0+)
    (setf (aref map +vreg-v1+) +a64-x1+)
    (setf (aref map +vreg-v2+) +a64-x2+)
    (setf (aref map +vreg-v3+) +a64-x3+)
    ;; Callee-saved
    (setf (aref map +vreg-v4+) +a64-x19+)
    (setf (aref map +vreg-v5+) +a64-x20+)
    (setf (aref map +vreg-v6+) +a64-x21+)
    (setf (aref map +vreg-v7+) +a64-x22+)
    (setf (aref map +vreg-v8+) +a64-x23+)
    ;; V9-V15 spill (nil)
    ;; Special registers
    (setf (aref map +vreg-vr+)  +a64-x0+)    ; return = x0
    (setf (aref map +vreg-va+)  +a64-x24+)   ; alloc pointer
    (setf (aref map +vreg-vl+)  +a64-x25+)   ; alloc limit
    (setf (aref map +vreg-vn+)  +a64-x26+)   ; NIL
    (setf (aref map +vreg-vsp+) +a64-sp+)    ; stack pointer
    (setf (aref map +vreg-vfp+) +a64-x29+)   ; frame pointer
    map))

(defun a64-phys-reg (vreg)
  "Map a virtual register to its AArch64 physical register number.
   Returns NIL for spilled registers (V9-V15)."
  (when (< vreg (length *a64-vreg-to-phys*))
    (aref *a64-vreg-to-phys* vreg)))

(defun a64-spill-offset (vreg)
  "Return the frame-relative offset for a spilled virtual register.
   Spill slots begin at [FP, #-64] and grow downward.
   Returns NIL for non-spilled registers."
  (when (and (>= vreg +vreg-v9+) (<= vreg +vreg-v15+))
    (* (- vreg +vreg-v9+) -8)))

(defun a64-resolve-reg (vreg scratch)
  "Resolve VREG to a physical register. If it spills, load it into
   SCRATCH from the spill slot and return SCRATCH."
  (or (a64-phys-reg vreg) scratch))

;;; ============================================================
;;; AArch64 Native Code Buffer
;;; ============================================================
;;;
;;; Native code is emitted as a vector of 32-bit instruction words.
;;; Branch fixups are resolved in a second pass.

(defstruct a64-buffer
  (code (make-array 524288))                  ; 512K entries, position tracks fill
  (labels (make-hash-table :test 'eql))      ; label-id → instruction index
  (fixups nil)                                ; (index label-id type)
  (position 0))                               ; current instruction index

(defun a64-emit (buf word)
  "Emit a single 32-bit instruction word."
  (let ((pos (a64-buffer-position buf)))
    (setf (aref (a64-buffer-code buf) pos) (logand word #xFFFFFFFF))
    (setf (a64-buffer-position buf) (+ pos 1))))

(defun a64-current-index (buf)
  "Return the current instruction index (next emission slot)."
  (a64-buffer-position buf))

(defun a64-set-label (buf label-id)
  "Record the current position as the target of LABEL-ID."
  (setf (gethash label-id (a64-buffer-labels buf))
        (a64-buffer-position buf)))

(defun a64-add-fixup (buf index label-id type)
  "Record a branch fixup: INDEX is the instruction to patch,
   LABEL-ID is the target, TYPE is :b/:bcond/:bl."
  (push (list index label-id type) (a64-buffer-fixups buf)))

(defun a64-resolve-fixups (buf)
  "Resolve all branch fixups by patching instruction words."
  (let ((code (a64-buffer-code buf)))
    (dolist (fixup (a64-buffer-fixups buf))
      (destructuring-bind (index label-id type) fixup
        (let* ((target (gethash label-id (a64-buffer-labels buf))))
          (unless target
            (error "AArch64: undefined label ~D (fixup at index ~D, type ~A)" label-id index type))
          (let ((offset (- target index)))
          (ecase type
            (:b
             ;; B imm26: patch bits [25:0]
             (let ((word (aref code index)))
               (setf (aref code index)
                     (logior (logand word #xFC000000)
                             (logand offset #x3FFFFFF)))))
            (:bl
             ;; BL imm26: patch bits [25:0]
             (let ((word (aref code index)))
               (setf (aref code index)
                     (logior (logand word #xFC000000)
                             (logand offset #x3FFFFFF)))))
            (:bcond
             ;; B.cond imm19: patch bits [23:5]
             (let ((word (aref code index)))
               (setf (aref code index)
                     (logior (logand word #xFF00001F)
                             (ash (logand offset #x7FFFF) 5)))))
            (:adr
             ;; ADR Xd: immlo(2)|10000|immhi(19)|Rd(5)
             ;; offset is in instructions, convert to bytes for ADR encoding
             (let* ((byte-off (* offset 4))
                    (immlo (logand byte-off 3))
                    (immhi (logand (ash byte-off -2) #x7FFFF))
                    (word (aref code index))
                    (rd (logand word #x1F)))
               ;; Use nested 2-arg logior (bare-metal multi-arg may clobber)
               (setf (aref code index)
                     (logior (logior (ash immlo 29) (ash #b10000 24))
                             (logior (ash immhi 5) rd))))))))))))

(defun a64-buffer-to-bytes (buf)
  "Convert the instruction buffer to a byte vector (little-endian)."
  (let* ((code (a64-buffer-code buf))
         (n (a64-buffer-position buf))
         (bytes (make-array (* n 4))))
    (dotimes (i n bytes)
      (let ((w (aref code i))
            (base (* i 4)))
        (setf (aref bytes base)       (logand w #xFF))
        (setf (aref bytes (+ base 1)) (logand (ash w -8) #xFF))
        (setf (aref bytes (+ base 2)) (logand (ash w -16) #xFF))
        (setf (aref bytes (+ base 3)) (logand (ash w -24) #xFF))))))

;;; ============================================================
;;; AArch64 Instruction Encoders
;;; ============================================================
;;;
;;; All AArch64 instructions are 32-bit fixed width.
;;; We encode for the 64-bit (sf=1) variant unless noted.

;;; --- Condition codes ---

(defconstant +cc-eq+ #x0)    ; equal (Z=1)
(defconstant +cc-ne+ #x1)    ; not equal (Z=0)
(defconstant +cc-lt+ #xB)    ; signed less than (N!=V)
(defconstant +cc-ge+ #xA)    ; signed greater or equal (N==V)
(defconstant +cc-le+ #xD)    ; signed less or equal (Z=1 || N!=V)
(defconstant +cc-gt+ #xC)    ; signed greater than (Z=0 && N==V)
(defconstant +cc-cs+ #x2)    ; carry set / unsigned >=
(defconstant +cc-cc+ #x3)    ; carry clear / unsigned <
(defconstant +cc-al+ #xE)    ; always

;;; --- ADD (shifted register) ---
;;; sf|0|0|01011|shift(2)|0|Rm(5)|imm6(6)|Rn(5)|Rd(5)

(defun a64-add-reg (buf rd rn rm shift amount)
  "ADD Xd, Xn, Xm{, shift #amount}  (64-bit)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)        ; sf=1 (64-bit)
                        (ash 0 30)        ; op=0 (ADD)
                        (ash 0 29)        ; S=0
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)        ; must be 0
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

(defun a64-adds-reg (buf rd rn rm shift amount)
  "ADDS Xd, Xn, Xm{, shift #amount}  (64-bit, sets flags)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)        ; sf=1
                        (ash 0 30)        ; op=0
                        (ash 1 29)        ; S=1
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

;;; --- SUB (shifted register) ---
;;; sf|1|0|01011|shift(2)|0|Rm(5)|imm6(6)|Rn(5)|Rd(5)

(defun a64-sub-reg (buf rd rn rm shift amount)
  "SUB Xd, Xn, Xm{, shift #amount}  (64-bit)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)        ; sf=1
                        (ash 1 30)        ; op=1 (SUB)
                        (ash 0 29)        ; S=0
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

(defun a64-subs-reg (buf rd rn rm shift amount)
  "SUBS Xd, Xn, Xm{, shift #amount}  (64-bit, sets flags)  shift: 0=LSL 1=LSR 2=ASR"
  (a64-emit buf (logior (ash 1 31)
                        (ash 1 30)        ; SUB
                        (ash 1 29)        ; S=1
                        (ash #b01011 24)
                        (ash shift 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash amount 10)
                        (ash rn 5)
                        rd)))

(defun a64-cmp-reg (buf rn rm)
  "CMP Xn, Xm  →  SUBS XZR, Xn, Xm"
  (a64-subs-reg buf +a64-xzr+ rn rm 0 0))

;;; --- ADD/SUB (immediate) ---
;;; sf|op|S|100010|sh|imm12(12)|Rn(5)|Rd(5)
;;; sh=0: imm12 not shifted. sh=1: imm12 << 12.

(defun a64-add-imm (buf rd rn imm12)
  "ADD Xd, Xn, #imm12"
  (a64-emit buf (logior (ash 1 31)          ; sf=1
                        (ash 0 30)          ; op=0 (ADD)
                        (ash 0 29)          ; S=0
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-adds-imm (buf rd rn imm12)
  "ADDS Xd, Xn, #imm12  (sets flags)"
  (a64-emit buf (logior (ash 1 31)
                        (ash 0 30)
                        (ash 1 29)          ; S=1
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-sub-imm (buf rd rn imm12)
  "SUB Xd, Xn, #imm12"
  (a64-emit buf (logior (ash 1 31)
                        (ash 1 30)          ; SUB
                        (ash 0 29)
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-subs-imm (buf rd rn imm12)
  "SUBS Xd, Xn, #imm12  (sets flags)"
  (a64-emit buf (logior (ash 1 31)
                        (ash 1 30)
                        (ash 1 29)          ; S=1
                        (ash #b100010 23)
                        (ash (logand imm12 #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun a64-cmp-imm (buf rn imm12)
  "CMP Xn, #imm12  →  SUBS XZR, Xn, #imm12"
  (a64-subs-imm buf +a64-xzr+ rn imm12))

;;; --- MOV (register) ---
;;; MOV Xd, Xm  →  ORR Xd, XZR, Xm

(defun a64-mov-reg (buf rd rm)
  "MOV Xd, Xm  →  ORR Xd, XZR, Xm"
  ;; ORR (shifted register): sf|01|01010|shift|0|Rm|imm6|Rn|Rd
  (a64-emit buf (logior (ash 1 31)          ; sf=1
                        (ash #b01 29)       ; opc=01 (ORR)
                        (ash #b01010 24)
                        (ash 0 22)          ; shift=LSL
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)          ; imm6=0
                        (ash +a64-xzr+ 5)  ; Rn=XZR
                        rd)))

;;; --- Logical (shifted register) ---
;;; sf|opc(2)|01010|shift(2)|N|Rm(5)|imm6(6)|Rn(5)|Rd(5)
;;; opc: 00=AND, 01=ORR, 10=EOR, 11=ANDS

(defun a64-and-reg (buf rd rn rm)
  "AND Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)          ; sf=1
                        (ash #b00 29)       ; AND
                        (ash #b01010 24)
                        (ash 0 22)          ; shift=LSL
                        (ash 0 21)          ; N=0
                        (ash rm 16)
                        (ash 0 10)          ; imm6=0
                        (ash rn 5)
                        rd)))

(defun a64-orr-reg (buf rd rn rm)
  "ORR Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b01 29)       ; ORR
                        (ash #b01010 24)
                        (ash 0 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-eor-reg (buf rd rn rm)
  "EOR Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b10 29)       ; EOR
                        (ash #b01010 24)
                        (ash 0 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-ands-reg (buf rd rn rm)
  "ANDS Xd, Xn, Xm  (sets flags → TST when Rd=XZR)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b11 29)       ; ANDS
                        (ash #b01010 24)
                        (ash 0 22)
                        (ash 0 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-tst-reg (buf rn rm)
  "TST Xn, Xm  →  ANDS XZR, Xn, Xm"
  (a64-ands-reg buf +a64-xzr+ rn rm))

;;; --- Shift (register) ---
;;; Variable shifts: LSLV/LSRV/ASRV encoded as data-processing (2 source)
;;; sf|0|0|11010110|Rm|001000|Rn|Rd  = LSLV
;;; sf|0|0|11010110|Rm|001001|Rn|Rd  = LSRV
;;; sf|0|0|11010110|Rm|001010|Rn|Rd  = ASRV

(defun a64-lslv (buf rd rn rm)
  "LSL Xd, Xn, Xm  (variable shift left)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b001000 10)
                        (ash rn 5)
                        rd)))

(defun a64-lsrv (buf rd rn rm)
  "LSR Xd, Xn, Xm  (variable logical shift right)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b001001 10)
                        (ash rn 5)
                        rd)))

(defun a64-asrv (buf rd rn rm)
  "ASR Xd, Xn, Xm  (variable arithmetic shift right)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b001010 10)
                        (ash rn 5)
                        rd)))

;;; --- Bitfield: UBFM / SBFM ---
;;; sf|opc(2)|100110|N|immr(6)|imms(6)|Rn(5)|Rd(5)
;;; UBFM: opc=10, SBFM: opc=00

(defun a64-ubfm (buf rd rn immr imms)
  "UBFM Xd, Xn, #immr, #imms  (unsigned bitfield move)"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b10 29)         ; opc=10 (UBFM)
                        (ash #b100110 23)
                        (ash 1 22)            ; N=1 for 64-bit
                        (ash (logand immr #x3F) 16)
                        (ash (logand imms #x3F) 10)
                        (ash rn 5)
                        rd)))

(defun a64-lsr-imm (buf rd rn amount)
  "LSR Xd, Xn, #amount  →  UBFM Xd, Xn, #amount, #63"
  (a64-ubfm buf rd rn amount 63))

(defun a64-lsl-imm (buf rd rn amount)
  "LSL Xd, Xn, #amount  →  UBFM Xd, Xn, #(64-amount), #(63-amount)"
  (a64-ubfm buf rd rn (logand (- 64 amount) #x3F) (- 63 amount)))

(defun a64-sbfm (buf rd rn immr imms)
  "SBFM Xd, Xn, #immr, #imms  (signed bitfield move)"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b00 29)         ; opc=00 (SBFM)
                        (ash #b100110 23)
                        (ash 1 22)            ; N=1
                        (ash (logand immr #x3F) 16)
                        (ash (logand imms #x3F) 10)
                        (ash rn 5)
                        rd)))

(defun a64-asr-imm (buf rd rn amount)
  "ASR Xd, Xn, #amount  →  SBFM Xd, Xn, #amount, #63"
  (a64-sbfm buf rd rn amount 63))

;;; --- MUL / SDIV ---
;;; MUL: sf|00|11011|000|Rm|0|Ra(=11111)|Rn|Rd   (MADD with Ra=XZR)
;;; SDIV: sf|00|11010110|Rm|00001|1|Rn|Rd

(defun a64-mul (buf rd rn rm)
  "MUL Xd, Xn, Xm  →  MADD Xd, Xn, Xm, XZR"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b0011011000 21)
                        (ash rm 16)
                        (ash 0 15)            ; o0=0 (MADD)
                        (ash +a64-xzr+ 10)   ; Ra=XZR → MUL alias
                        (ash rn 5)
                        rd)))

(defun a64-umulh (buf rd rn rm)
  "UMULH Xd, Xn, Xm — high 64 bits of unsigned Xn*Xm.
   Encoding: 1|00|11011|1|10|Rm|0|Ra(=11111)|Rn|Rd"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b0011011110 21) ; UMULH
                        (ash rm 16)
                        (ash 0 15)            ; o0=0
                        (ash +a64-xzr+ 10)   ; Ra=XZR
                        (ash rn 5)
                        rd)))

(defun a64-adc (buf rd rn rm)
  "ADC Xd, Xn, Xm — add with carry (uses C flag from previous ADDS).
   Encoding: 1|00|11010000|Rm|000000|Rn|Rd"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b0011010000 21)
                        (ash rm 16)
                        (ash 0 10)
                        (ash rn 5)
                        rd)))

(defun a64-sdiv (buf rd rn rm)
  "SDIV Xd, Xn, Xm"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b0011010110 21)
                        (ash rm 16)
                        (ash #b000011 10)
                        (ash rn 5)
                        rd)))

;;; --- NEG ---

(defun a64-neg (buf rd rm)
  "NEG Xd, Xm  →  SUB Xd, XZR, Xm"
  (a64-sub-reg buf rd +a64-xzr+ rm 0 0))

;;; --- Move wide (MOVZ / MOVK) ---
;;; sf|opc(2)|100101|hw(2)|imm16(16)|Rd(5)
;;; MOVZ: opc=10, MOVK: opc=11

(defun a64-movz (buf rd imm16 hw)
  "MOVZ Xd, #imm16{, LSL #(hw*16)}"
  (a64-emit buf (logior (ash 1 31)            ; sf=1
                        (ash #b10 29)         ; MOVZ
                        (ash #b100101 23)
                        (ash (logand hw 3) 21)
                        (ash (logand imm16 #xFFFF) 5)
                        rd)))

(defun a64-movk (buf rd imm16 hw)
  "MOVK Xd, #imm16{, LSL #(hw*16)}"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b11 29)         ; MOVK
                        (ash #b100101 23)
                        (ash (logand hw 3) 21)
                        (ash (logand imm16 #xFFFF) 5)
                        rd)))

(defun a64-movn (buf rd imm16 hw)
  "MOVN Xd, #imm16{, LSL #(hw*16)}  (move wide NOT)"
  (a64-emit buf (logior (ash 1 31)
                        (ash #b00 29)         ; MOVN
                        (ash #b100101 23)
                        (ash (logand hw 3) 21)
                        (ash (logand imm16 #xFFFF) 5)
                        rd)))

(defun a64-load-imm64 (buf rd imm64)
  "Load a 64-bit immediate into Xd using minimal MOVZ/MOVK sequence."
  (let* ((val (logand imm64 #xFFFFFFFFFFFFFFFF))
         (hw0 (logand val #xFFFF))
         (hw1 (logand (ash val -16) #xFFFF))
         (hw2 (logand (ash val -32) #xFFFF))
         (hw3 (logand (ash val -48) #xFFFF))
         (chunks (list (cons 0 hw0) (cons 1 hw1) (cons 2 hw2) (cons 3 hw3)))
         (nonzero (remove-if (lambda (c) (zerop (cdr c))) chunks)))
    (cond
      ;; Zero
      ((zerop val)
       (a64-movz buf rd 0 0))
      ;; Single 16-bit chunk
      ((= (length nonzero) 1)
       (let ((c (first nonzero)))
         (a64-movz buf rd (cdr c) (car c))))
      ;; Check if all-ones complement is cheaper (MOVN)
      ((let* ((inv (logxor val #xFFFFFFFFFFFFFFFF))
              (ihw0 (logand inv #xFFFF))
              (ihw1 (logand (ash inv -16) #xFFFF))
              (ihw2 (logand (ash inv -32) #xFFFF))
              (ihw3 (logand (ash inv -48) #xFFFF))
              (ichunks (list (cons 0 ihw0) (cons 1 ihw1)
                             (cons 2 ihw2) (cons 3 ihw3)))
              (inv-nonzero (remove-if (lambda (c) (zerop (cdr c))) ichunks)))
         (when (= (length inv-nonzero) 1)
           (let ((c (first inv-nonzero)))
             (a64-movn buf rd (cdr c) (car c))
             t))))
      ;; General case: MOVZ first non-zero, then MOVK the rest
      (t
       (let ((first-chunk (first nonzero))
             (rest-chunks (rest nonzero)))
         (a64-movz buf rd (cdr first-chunk) (car first-chunk))
         (dolist (c rest-chunks)
           (a64-movk buf rd (cdr c) (car c))))))))

;;; --- Load/Store (unsigned offset) ---
;;; size(2)|111|V|01|opc(2)|imm12(12)|Rn(5)|Rt(5)
;;; LDR (64-bit): size=11, V=0, opc=01 → 0xF9400000
;;; STR (64-bit): size=11, V=0, opc=00 → 0xF9000000

(defun a64-ldr-unsigned (buf rt rn imm12)
  "LDR Xt, [Xn, #imm12]  (unsigned offset, scaled by 8)"
  ;; imm12 is byte offset / 8 for 64-bit loads
  (let ((scaled (ash imm12 -3)))
    (a64-emit buf (logior #xF9400000
                          (ash (logand scaled #xFFF) 10)
                          (ash rn 5)
                          rt))))

(defun a64-str-unsigned (buf rt rn imm12)
  "STR Xt, [Xn, #imm12]  (unsigned offset, scaled by 8)"
  (let ((scaled (ash imm12 -3)))
    (a64-emit buf (logior #xF9000000
                          (ash (logand scaled #xFFF) 10)
                          (ash rn 5)
                          rt))))

;;; --- Load/Store (unscaled immediate, LDUR/STUR) ---
;;; size(2)|111|V|00|opc(2)|0|imm9(9)|00|Rn(5)|Rt(5)
;;; LDUR (64-bit): 0xF8400000 | imm9<<12
;;; STUR (64-bit): 0xF8000000 | imm9<<12

(defun a64-ldur (buf rt rn simm9)
  "LDUR Xt, [Xn, #simm9]  (unscaled signed offset)"
  (a64-emit buf (logior #xF8400000
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

(defun a64-stur (buf rt rn simm9)
  "STUR Xt, [Xn, #simm9]  (unscaled signed offset)"
  (a64-emit buf (logior #xF8000000
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

;;; --- Load/Store with variable width ---

(defun a64-ldr-width (buf rt rn offset width)
  "Load from [Xn, #offset] with WIDTH (0=u8, 1=u16, 2=u32, 3=u64).
   Uses LDUR for the unscaled offset form."
  ;; LDURB: 00|111000|01|0|imm9|00|Rn|Rt  = #x38400000
  ;; LDURH: 01|111000|01|0|imm9|00|Rn|Rt  = #x78400000
  ;; LDUR W: 10|111000|01|0|imm9|00|Rn|Rt = #xB8400000
  ;; LDUR X: 11|111000|01|0|imm9|00|Rn|Rt = #xF8400000
  (let ((base (ecase width
                (0 #x38400000)    ; LDURB
                (1 #x78400000)    ; LDURH
                (2 #xB8400000)    ; LDUR W
                (3 #xF8400000)))) ; LDUR X
    (a64-emit buf (logior base
                          (ash (logand offset #x1FF) 12)
                          (ash rn 5)
                          rt))))

(defun a64-str-width (buf rt rn offset width)
  "Store to [Xn, #offset] with WIDTH (0=u8, 1=u16, 2=u32, 3=u64).
   Uses STUR for the unscaled offset form."
  (let ((base (ecase width
                (0 #x38000000)    ; STURB
                (1 #x78000000)    ; STURH
                (2 #xB8000000)    ; STUR W
                (3 #xF8000000)))) ; STUR X
    (a64-emit buf (logior base
                          (ash (logand offset #x1FF) 12)
                          (ash rn 5)
                          rt))))

;;; --- Load/Store Pair (STP/LDP) ---
;;; opc(2)|101|V|0|type(2)|L|imm7(7)|Rt2(5)|Rn(5)|Rt1(5)
;;; STP (64-bit, signed offset): opc=10, V=0, type=10, L=0 → 0xA9000000
;;; LDP (64-bit, signed offset): opc=10, V=0, type=10, L=1 → 0xA9400000
;;; STP (pre-index):  type=11 → 0xA9800000
;;; LDP (post-index): type=01 (L=1) → 0xA8C00000

(defun a64-stp-offset (buf rt1 rt2 rn simm7)
  "STP Xt1, Xt2, [Xn, #simm7]  (signed offset, scaled by 8)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA9000000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

(defun a64-ldp-offset (buf rt1 rt2 rn simm7)
  "LDP Xt1, Xt2, [Xn, #simm7]  (signed offset, scaled by 8)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA9400000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

(defun a64-stp-pre (buf rt1 rt2 rn simm7)
  "STP Xt1, Xt2, [Xn, #simm7]!  (pre-index)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA9800000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

(defun a64-ldp-post (buf rt1 rt2 rn simm7)
  "LDP Xt1, Xt2, [Xn], #simm7  (post-index)"
  (let ((scaled (ash simm7 -3)))
    (a64-emit buf (logior #xA8C00000
                          (ash (logand scaled #x7F) 15)
                          (ash rt2 10)
                          (ash rn 5)
                          rt1))))

;;; --- Load/Store (pre-index / post-index, single register) ---
;;; Pre-index:  size|111|V|00|opc|0|imm9|11|Rn|Rt
;;; Post-index: size|111|V|00|opc|0|imm9|01|Rn|Rt

(defun a64-str-pre (buf rt rn simm9)
  "STR Xt, [Xn, #simm9]!  (pre-index, 64-bit)"
  (a64-emit buf (logior #xF8000C00
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

(defun a64-ldr-post (buf rt rn simm9)
  "LDR Xt, [Xn], #simm9  (post-index, 64-bit)"
  (a64-emit buf (logior #xF8400400
                        (ash (logand simm9 #x1FF) 12)
                        (ash rn 5)
                        rt)))

;;; --- Branch (unconditional) ---
;;; B:  0|00101|imm26(26)
;;; BL: 1|00101|imm26(26)

(defun a64-b (buf imm26)
  "B imm26  (unconditional branch, PC-relative)"
  (a64-emit buf (logior (ash #b000101 26)
                        (logand imm26 #x3FFFFFF))))

(defun a64-bl (buf imm26)
  "BL imm26  (branch with link, PC-relative)"
  (a64-emit buf (logior (ash #b100101 26)
                        (logand imm26 #x3FFFFFF))))

;;; --- Conditional branch ---
;;; 0101010|0|imm19(19)|0|cond(4)

(defun a64-bcond (buf cond imm19)
  "B.cond imm19  (conditional branch, PC-relative)"
  (a64-emit buf (logior (ash #b01010100 24)
                        (ash (logand imm19 #x7FFFF) 5)
                        (logand cond #xF))))

;;; --- Branch register ---
;;; 1101011|opc(4)|11111|000000|Rn(5)|00000
;;; RET: opc=0010, BR: opc=0000, BLR: opc=0001

(defun a64-ret (buf &optional (rn +a64-x30+))
  "RET {Xn}  (return, default x30)"
  (a64-emit buf (logior #xD65F0000
                        (ash rn 5))))

(defun a64-br (buf rn)
  "BR Xn  (branch to register)"
  (a64-emit buf (logior #xD61F0000
                        (ash rn 5))))

(defun a64-blr (buf rn)
  "BLR Xn  (branch with link to register)"
  (a64-emit buf (logior #xD63F0000
                        (ash rn 5))))

;;; --- System instructions ---

(defun a64-nop (buf)
  "NOP"
  (a64-emit buf #xD503201F))

(defun a64-wfi (buf)
  "WFI  (wait for interrupt)"
  (a64-emit buf #xD503207F))

(defun a64-wfe (buf)
  "WFE  (wait for event)"
  (a64-emit buf #xD503205F))

(defun a64-brk (buf imm16)
  "BRK #imm16  (software breakpoint)"
  (a64-emit buf (logior #xD4200000
                        (ash (logand imm16 #xFFFF) 5))))

(defun a64-hlt (buf imm16)
  "HLT #imm16  (halt / semihosting trap)"
  (a64-emit buf (logior #xD4400000
                        (ash (logand imm16 #xFFFF) 5))))

(defun a64-svc (buf imm16)
  "SVC #imm16  (supervisor call)"
  (a64-emit buf (logior #xD4000001
                        (ash (logand imm16 #xFFFF) 5))))

;;; --- Data Memory Barrier ---
;;; DMB: 11010101000000110011|CRm(4)|1|01|11111
;;; CRm: #xB = ISH (inner shareable), #xF = SY (full system)

(defun a64-dmb (buf option)
  "DMB option  (data memory barrier)"
  (a64-emit buf (logior #xD503301F
                        (ash #b101 5)
                        (ash (logand option #xF) 8))))

(defun a64-dsb (buf option)
  "DSB option  (data synchronization barrier)"
  (a64-emit buf (logior #xD503301F
                        (ash #b100 5)
                        (ash (logand option #xF) 8))))

(defun a64-isb (buf)
  "ISB  (instruction synchronization barrier)"
  (a64-emit buf #xD5033FDF))

;;; --- MSR/MRS (interrupt control) ---
;;; MSR DAIFSet, #imm4:  1101010100|0|00|011|0100|imm4|110|11111
;;; MSR DAIFClr, #imm4:  1101010100|0|00|011|0100|imm4|111|11111

(defun a64-msr-daifset (buf imm4)
  "MSR DAIFSet, #imm4  (mask interrupts: bit 1=F, bit 2=I, bit 3=A)"
  ;; Encoding: D503419F | (imm4 << 8) for DAIFSet
  (a64-emit buf (logior #xD5034000
                        (ash #b110 5)
                        (ash (logand imm4 #xF) 8)
                        #x1F)))

(defun a64-msr-daifclr (buf imm4)
  "MSR DAIFClr, #imm4  (unmask interrupts)"
  (a64-emit buf (logior #xD5034000
                        (ash #b111 5)
                        (ash (logand imm4 #xF) 8)
                        #x1F)))

;;; --- MRS/MSR (system register access) ---
;;; System register encoding: op0[1:0]|op1[2:0]|CRn[3:0]|CRm[3:0]|op2[2:0] = 16 bits
;;; This 16-bit value must be shifted left by 5 to align with instruction bits [20:5].
;;; Common encodings:
;;;   TPIDR_EL1 (S3_0_C13_C0_4) = #xC684
;;;   VBAR_EL1  (S3_0_C12_C0_0) = #xC600

(defun a64-mrs (buf rt sysreg-encoding)
  "MRS Xt, <sysreg>  (read system register)"
  ;; MRS: 1101010100|1|op0[1:0]|op1[2:0]|CRn[3:0]|CRm[3:0]|op2[2:0]|Rt[4:0]
  ;; Base #xD5300000 has L=1 (read), op0 MSB set (op0 >= 2)
  (a64-emit buf (logior #xD5300000
                        (ash sysreg-encoding 5)
                        rt)))

(defun a64-msr-sysreg (buf sysreg-encoding rt)
  "MSR <sysreg>, Xt  (write system register)"
  ;; MSR: 1101010100|0|op0[1:0]|op1[2:0]|CRn[3:0]|CRm[3:0]|op2[2:0]|Rt[4:0]
  ;; Base #xD5100000 has L=0 (write), op0 MSB set (op0 >= 2)
  (a64-emit buf (logior #xD5100000
                        (ash sysreg-encoding 5)
                        rt)))

(defconstant +sysreg-tpidr-el1+ #xC684 "TPIDR_EL1: S3_0_C13_C0_4")
(defconstant +sysreg-vbar-el1+  #xC600 "VBAR_EL1: S3_0_C12_C0_0")
(defconstant +sysreg-cntpct-el0+ #xDF01 "CNTPCT_EL0: S3_3_C14_C0_1")

;;; --- Atomic exchange (LDXR/STXR pair) ---
;;; LDXR Xt, [Xn]: size|001000|0|1|0|Rs(11111)|0|Rt2(11111)|Rn|Rt
;;; STXR Ws, Xt, [Xn]: size|001000|0|0|0|Rs|0|Rt2(11111)|Rn|Rt
;;; 64-bit: size=11

(defun a64-ldxr (buf rt rn)
  "LDXR Xt, [Xn]  (load exclusive register)"
  (a64-emit buf (logior #xC85F7C00
                        (ash rn 5)
                        rt)))

(defun a64-stxr (buf rs rt rn)
  "STXR Ws, Xt, [Xn]  (store exclusive register, Rs=status)"
  (a64-emit buf (logior #xC8007C00
                        (ash rs 16)
                        (ash rn 5)
                        rt)))

;;; --- CSET (conditional set) ---
;;; CSET Xd, cond  →  CSINC Xd, XZR, XZR, invert(cond)
;;; CSINC encoding: sf|op|S|11010100|Rm(5)|cond(4)|0|o2|Rn(5)|Rd(5)
;;; For CSINC: sf=1, op=0, S=0, o2=1
;;; Rm=XZR(31), Rn=XZR(31), cond is inverted for CSET alias

(defun a64-cset (buf rd cond)
  "CSET Xd, cond  →  CSINC Xd, XZR, XZR, invert(cond)"
  (let ((inv-cond (logxor cond 1)))
    (a64-emit buf (logior (ash 1 31)              ; sf=1
                          (ash 0 30)              ; op=0
                          (ash 0 29)              ; S=0
                          (ash #b11010100 21)     ; CSINC fixed bits
                          (ash +a64-xzr+ 16)     ; Rm = XZR
                          (ash inv-cond 12)       ; cond (inverted)
                          (ash 0 11)              ; bit 11 = 0
                          (ash 1 10)              ; o2=1 (CSINC)
                          (ash +a64-xzr+ 5)      ; Rn = XZR
                          rd))))

;;; ============================================================
;;; Spill Slot Helpers
;;; ============================================================
;;;
;;; Spilled virtual registers (V9-V15) live in the stack frame.
;;; The frame layout (set up in prologue):
;;;   [FP+8]  = saved LR
;;;   [FP]    = saved FP
;;;   [FP-8]  = spill slot 0 (V9)
;;;   [FP-16] = spill slot 1 (V10)
;;;   ... etc
;;;
;;; For 7 spill slots (V9-V15) we need 56 bytes.

(defconstant +a64-spill-base-offset+ -8
  "Offset from FP to the first spill slot.")

(defconstant +a64-frame-slot-base+ -64
  "FP-relative offset for frame slot 0 (local variables via obj-ref VFP).
   Frame slots grow downward: slot N is at FP + frame-slot-base + N*(-8).
   This is below all spill slots (which end at FP-56) to avoid overlap.")

(defconstant +a64-locals-frame-size+ 1024
  "Extra stack allocation below FP for spill slots (56 bytes) and
   frame slots. 1024 bytes provides ~120 frame slots for local variables,
   sufficient for deeply nested crypto functions like fe-mul (~80 slots).")

(defun a64-spill-slot-offset (vreg)
  "Compute the FP-relative offset for a spilled vreg (V9-V15).
   Returns a negative offset suitable for LDUR/STUR."
  (+ +a64-spill-base-offset+ (* (- vreg +vreg-v9+) -8)))

(defun a64-load-spill (buf phys-dest vreg)
  "Load a spilled virtual register from its frame slot into PHYS-DEST."
  (a64-ldur buf phys-dest +a64-x29+ (a64-spill-slot-offset vreg)))

(defun a64-store-spill (buf phys-src vreg)
  "Store PHYS-SRC into the frame spill slot for VREG."
  (a64-stur buf phys-src +a64-x29+ (a64-spill-slot-offset vreg)))

(defun a64-emit-load-vreg (buf phys-dest vreg)
  "Ensure VREG is in PHYS-DEST. If VREG maps to a physical register,
   emit MOV if needed. If it spills, emit a load from the frame."
  (let ((phys (a64-phys-reg vreg)))
    (cond
      (phys
       (unless (= phys phys-dest)
         (a64-mov-reg buf phys-dest phys)))
      ((and (>= vreg +vreg-v9+) (<= vreg +vreg-v15+))
       (a64-load-spill buf phys-dest vreg))
      (t
       (error "AArch64: cannot load virtual register ~D" vreg)))))

(defun a64-emit-store-vreg (buf phys-src vreg)
  "Store PHYS-SRC into VREG's location. If VREG maps to a physical
   register, emit MOV if needed. If it spills, store to frame."
  (let ((phys (a64-phys-reg vreg)))
    (cond
      (phys
       (unless (= phys phys-src)
         (a64-mov-reg buf phys phys-src)))
      ((and (>= vreg +vreg-v9+) (<= vreg +vreg-v15+))
       (a64-store-spill buf phys-src vreg))
      (t
       (error "AArch64: cannot store to virtual register ~D" vreg)))))

;;; ============================================================
;;; Prologue / Epilogue
;;; ============================================================

(defun a64-emit-prologue (buf)
  "Emit the standard function prologue:
     STP x29, x30, [sp, #-80]!
     MOV x29, sp
     STP x19, x20, [sp, #16]
     STP x21, x22, [sp, #32]
     STP x23, xzr, [sp, #48]
     ;; x24/x25/x26 are global state (alloc/limit/nil) — NOT saved
     SUB sp, sp, #1024
   Frame: 80 bytes save area + 1024 bytes for spill slots and frame locals.
   Note: x24 (alloc ptr), x25 (alloc limit), x26 (nil) are global state
   shared across all functions. They must NOT be saved/restored, or
   allocations made by callees would be lost on return."
  ;; Save FP and LR, allocate save area
  (a64-stp-pre buf +a64-x29+ +a64-x30+ +a64-sp+ -80)
  ;; Set up frame pointer: ADD x29, SP, #0
  ;; (Cannot use a64-mov-reg because ORR encodes reg 31 as XZR, not SP)
  (a64-add-imm buf +a64-x29+ +a64-sp+ 0)
  ;; Save callee-saved registers (x19-x23 only)
  ;; x24/x25/x26 are global alloc/limit/nil — must persist across calls
  (a64-stp-offset buf +a64-x19+ +a64-x20+ +a64-sp+ 16)
  (a64-stp-offset buf +a64-x21+ +a64-x22+ +a64-sp+ 32)
  ;; Save x23 paired with xzr (x31 = zero register in this context)
  (a64-stp-offset buf +a64-x23+ +a64-xzr+ +a64-sp+ 48)
  ;; Allocate space for spill slots and frame locals below FP
  (a64-sub-imm buf +a64-sp+ +a64-sp+ +a64-locals-frame-size+))

(defun a64-emit-epilogue (buf)
  "Emit the standard function epilogue:
     ADD sp, sp, #1024
     LDP x23, xzr, [sp, #48]
     LDP x21, x22, [sp, #32]
     LDP x19, x20, [sp, #16]
     LDP x29, x30, [sp], #80
     RET
   Note: x24/x25/x26 are NOT restored (global state)."
  ;; Deallocate spill/frame-slot area
  (a64-add-imm buf +a64-sp+ +a64-sp+ +a64-locals-frame-size+)
  ;; Restore callee-saved registers (x19-x23 only)
  ;; x24/x25/x26 are global alloc/limit/nil — do NOT restore
  (a64-ldp-offset buf +a64-x23+ +a64-xzr+ +a64-sp+ 48)
  (a64-ldp-offset buf +a64-x21+ +a64-x22+ +a64-sp+ 32)
  (a64-ldp-offset buf +a64-x19+ +a64-x20+ +a64-sp+ 16)
  ;; Restore FP/LR and deallocate save area
  (a64-ldp-post buf +a64-x29+ +a64-x30+ +a64-sp+ 80)
  (a64-ret buf))

;;; ============================================================
;;; MVM Bytecode Decoder Helpers
;;; ============================================================
;;;
;;; These walk through MVM bytecode and build a list of decoded
;;; instructions with their source byte positions, used for
;;; computing branch target mappings.

(defstruct decoded-mvm-insn
  offset      ; byte position in MVM bytecode
  opcode      ; numeric opcode
  operands    ; list of operand values
  size)       ; size in bytes

(defun decode-mvm-stream (bytes &key (start 0) (end nil))
  "Decode all MVM instructions from BYTES into a list of decoded-mvm-insn."
  (let ((limit (or end (length bytes)))
        (pos start)
        (insns nil))
    (loop while (< pos limit)
          do (let ((ipos pos))
               (let* ((decoded (decode-instruction bytes pos))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pos (cddr decoded)))
                 (push (make-decoded-mvm-insn
                        :offset ipos
                        :opcode opcode
                        :operands operands
                        :size (- new-pos ipos))
                       insns)
                 (setf pos new-pos))))
    (nreverse insns)))

(defun build-offset-to-index-map (insns)
  "Build a hash table mapping MVM byte offset → instruction index."
  (let ((map (make-hash-table :test 'eql)))
    (loop for insn in insns
          for idx from 0
          do (setf (gethash (decoded-mvm-insn-offset insn) map) idx))
    map))

;;; ============================================================
;;; MVM → AArch64 Instruction Translation
;;; ============================================================
;;;
;;; Each MVM opcode translates to a short sequence of AArch64
;;; instructions. The translator does a two-pass approach:
;;;   Pass 1: Translate all instructions, emit placeholder branches
;;;   Pass 2: Resolve branch fixups (MVM offsets → native offsets)

(defun translate-mvm-insn (insn buf mvm-to-native-label)
  "Translate a single decoded MVM instruction, emitting AArch64
   native code into BUF. MVM-TO-NATIVE-LABEL maps MVM byte offsets
   to native label IDs for branch targets."
  (let ((op (decoded-mvm-insn-opcode insn))
        (args (decoded-mvm-insn-operands insn)))
    (macrolet ((vr (n) `(nth ,n args))
               (phys (n) `(a64-resolve-reg (nth ,n args) +a64-x16+))
               (phys2 (n) `(a64-resolve-reg (nth ,n args) +a64-x17+)))
      (flet ((ensure-src (vreg scratch)
               "Load vreg into scratch if it spills, return the physical reg."
               (let ((p (a64-phys-reg vreg)))
                 (if p p
                     (progn (a64-emit-load-vreg buf scratch vreg) scratch))))
             (store-dst (phys-src vreg)
               "Store phys-src into vreg's location."
               (a64-emit-store-vreg buf phys-src vreg)))

        (cond
          ;; ---- NOP ----
          ((= op +op-nop+)
           (a64-nop buf))

          ;; ---- BREAK ----
          ((= op +op-break+)
           (a64-brk buf 0))

          ;; ---- TRAP ----
          ((= op +op-trap+)
           (let ((code (vr 0)))
             (cond
               ((< code #x0100)
                ;; Frame-enter: emit function prologue
                (a64-emit-prologue buf)
                ;; If > 4 params, copy overflow args from caller's stack
                ;; to local frame slots so stack-load can find them.
                ;; Overflow arg k is at [FP + 80 + k*8] (above save area).
                (when (> code 4)
                  (loop for i from 4 below code
                        for src-offset = (+ 80 (* (- i 4) 8))
                        for dst-offset = (+ +a64-frame-slot-base+ (* i -8))
                        do (a64-ldur buf +a64-x16+ +a64-x29+ src-offset)
                           (a64-stur buf +a64-x16+ +a64-x29+ dst-offset))))
               ((< code #x0300)
                ;; Frame-alloc/frame-free: NOP for now
                nil)
               ((= code #x0300)
                ;; Serial write: V0 (x0) contains tagged fixnum char code
                ;; asr x16, x0, #1 (untag)
                (a64-asr-imm buf +a64-x16+ +a64-x0+ 1)
                ;; load UART base (configurable via *aarch64-serial-base*)
                (a64-load-imm64 buf +a64-x17+ *aarch64-serial-base*)
                ;; TX-ready poll: wait for transmit FIFO to have space
                (when *aarch64-serial-tx-poll*
                  (destructuring-bind (offset bit polarity) *aarch64-serial-tx-poll*
                    ;; ldr w18, [x17, #offset] — read status register (32-bit)
                    (a64-ldr-width buf 18 +a64-x17+ offset 2)
                    ;; tbz/tbnz x18, #bit, -4 — loop back to ldr
                    (let ((opcode (ecase polarity
                                    (:tbz  #b00110110)   ; wait while bit clear
                                    (:tbnz #b00110111)))) ; wait while bit set
                      (a64-emit buf (logior (ash opcode 24)
                                            (ash bit 19)
                                            (ash (logand -1 #x3FFF) 5)
                                            18)))))
                ;; store to UART data register (strb for PL011, str for mini UART)
                (a64-str-width buf +a64-x16+ +a64-x17+ 0 *aarch64-serial-width*))
               ((= code #x0301)
                ;; Serial read: poll UART until a byte is available,
                ;; return tagged fixnum char code in x0.
                ;; PL011 UARTFR offset = 0x18, RXFE bit = bit 4
                ;; load UART base into x17
                (a64-load-imm64 buf +a64-x17+ *aarch64-serial-base*)
                ;; poll loop (2 instructions):
                ;;   ldrb w16, [x17, #0x18]   ; read UARTFR
                (a64-ldr-width buf +a64-x16+ +a64-x17+ #x18 0)
                ;;   tbnz x16, #4, -4         ; if RXFE set, branch back
                ;; TBNZ encoding: b5|011011|1|b40|imm14|Rt
                ;; b5=0, b40=00100 (bit 4), imm14=-1 (back 1 insn), Rt=x16
                (a64-emit buf (logior (ash #b00110111 24)  ; TBNZ
                                      (ash 4 19)           ; bit number = 4
                                      (ash (logand -1 #x3FFF) 5)  ; imm14 = -1
                                      +a64-x16+))
                ;; read data byte: ldrb w0, [x17, #0]
                (a64-ldr-width buf +a64-x0+ +a64-x17+ 0 0)
                ;; tag as fixnum: lsl x0, x0, #1
                (a64-lsl-imm buf +a64-x0+ +a64-x0+ 1))
               ((= code #x0302)
                ;; DSB SY barrier: force all previous memory accesses to complete
                ;; Required on AArch64 with MMU off (Normal Non-cacheable) to
                ;; prevent write buffer reordering between peripheral pages
                (a64-dsb buf #xF))  ;; SY = full system
               ((= code #x0303)
                ;; Jump to address: untag V0 (ASR 1), BR x0
                (a64-asr-imm buf +a64-x0+ +a64-x0+ 1)
                (a64-br buf +a64-x0+))
               ((= code #x0304)
                ;; WFI: Wait For Interrupt (sleep until next IRQ)
                (a64-emit buf #xD503207F))
               ((= code #x0310)
                ;; RDTSC equivalent: read CNTPCT_EL0 (physical timer counter) into X0 (VR)
                (a64-mrs buf +a64-x0+ +sysreg-cntpct-el0+))
               ((= code #x0320)
                ;; SETUP-IRQ: virtual timer init (always) + GICv2 (conditional)
                ;; Save x0/x16 — TRAP is inline, compiler doesn't know we clobber them
                (a64-str-pre buf +a64-x0+ +a64-sp+ -16)
                (a64-str-pre buf +a64-x16+ +a64-sp+ -16)
                ;; GICv2 only on QEMU virt (not RPi, not fixpoint)
                (when *aarch64-setup-irq-enable*
                  ;; GIC distributor enable: GICD_CTLR = 1
                  (a64-load-imm64 buf +a64-x16+ #x08000000)
                  (a64-load-imm64 buf +a64-x0+ 1)
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; Enable virtual timer PPI (INTID 27): GICD_ISENABLER0[27] = 1
                  ;; GICD_ISENABLER0 at GICD_base+0x100 — load full address (STUR max offset=255)
                  (a64-load-imm64 buf +a64-x16+ #x08000100)
                  (a64-load-imm64 buf +a64-x0+ #x08000000)  ; 1<<27
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; Enable E1000 PCI INTA SPI (INTID 35): GICD_ISENABLER1[3] = 1
                  ;; GICD_ISENABLER1 at GICD_base+0x104 — covers INTIDs 32-63
                  (a64-load-imm64 buf +a64-x16+ #x08000104)
                  (a64-load-imm64 buf +a64-x0+ 8)  ; 1<<3 = INTID 35
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; GIC CPU interface enable: GICC_CTLR = 1
                  (a64-load-imm64 buf +a64-x16+ #x08010000)
                  (a64-load-imm64 buf +a64-x0+ 1)
                  (a64-str-width buf +a64-x0+ +a64-x16+ 0 2)
                  ;; GICC_PMR = 0xFF (accept all priorities)
                  (a64-load-imm64 buf +a64-x0+ #xFF)
                  (a64-str-width buf +a64-x0+ +a64-x16+ 4 2))
                ;; Virtual timer: always init (works on virt, RPi, fixpoint)
                ;; CNTV_TVAL_EL0 = 62500 (1ms at 62.5MHz)
                (a64-load-imm64 buf +a64-x0+ 62500)
                (a64-emit buf #xD51BE300)  ; MSR CNTV_TVAL_EL0, x0
                ;; Enable timer: CNTV_CTL_EL0 = 1
                (a64-load-imm64 buf +a64-x0+ 1)
                (a64-emit buf #xD51BE320)  ; MSR CNTV_CTL_EL0, x0
                ;; NOTE: IRQs stay masked. WFI wakes on pending timer PPI
                ;; even with PSTATE.I=1. io-delay re-arms timer before WFI.
                ;; Restore x16/x0 (reverse order)
                (a64-ldr-post buf +a64-x16+ +a64-sp+ 16)
                (a64-ldr-post buf +a64-x0+ +a64-sp+ 16))
               ((= code #x0321)
                ;; TIMER-REARM: re-arm virtual timer (always emit on AArch64)
                (a64-load-imm64 buf +a64-x0+ 62500)
                (a64-emit buf #xD51BE300))  ; MSR CNTV_TVAL_EL0, x0
               ((= code #x0400)
                ;; switch-idle-stack: set SP to per-CPU idle-stack-top
                (a64-mrs buf +a64-x16+ +sysreg-tpidr-el1+)
                (a64-ldr-unsigned buf +a64-x16+ +a64-x16+ #x38)
                (a64-add-imm buf +a64-sp+ +a64-x16+ 0))
               (t
                ;; Real CPU trap
                (a64-svc buf code)))))

          ;; ---- MOV Vd, Vs ----
          ((= op +op-mov+)
           (let* ((vd (vr 0)) (vs (vr 1))
                  (ps (ensure-src vs +a64-x16+)))
             (store-dst ps vd)))

          ;; ---- LI Vd, imm64 ----
          ((= op +op-li+)
           (let ((vd (vr 0)) (imm (vr 1)))
             (let ((pd (a64-phys-reg vd)))
               (if pd
                   (a64-load-imm64 buf pd imm)
                   (progn
                     (a64-load-imm64 buf +a64-x16+ imm)
                     (store-dst +a64-x16+ vd))))))

          ;; ---- PUSH Vs ----
          ((= op +op-push+)
           (let ((ps (ensure-src (vr 0) +a64-x16+)))
             ;; STR Xs, [SP, #-8]!
             (a64-str-pre buf ps +a64-sp+ -8)))

          ;; ---- POP Vd ----
          ((= op +op-pop+)
           (let ((pd (a64-phys-reg (vr 0))))
             (if pd
                 ;; LDR Xd, [SP], #8
                 (a64-ldr-post buf pd +a64-sp+ 8)
                 (progn
                   (a64-ldr-post buf +a64-x16+ +a64-sp+ 8)
                   (store-dst +a64-x16+ (vr 0))))))

          ;; ---- ADD Vd, Va, Vb ----
          ((= op +op-add+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-add-reg buf pd pa pb 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SUB Vd, Va, Vb ----
          ((= op +op-sub+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-sub-reg buf pd pa pb 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL Vd, Va, Vb ----
          ;; Tagged fixnum multiply: Vd = (Va >> 1) * Vb
          ;; (because both args carry a tag bit; shifting one removes double-tag)
          ((= op +op-mul+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; ASR x16, Va, #1  (remove one tag bit)
             (a64-asr-imm buf +a64-x16+ pa 1)
             ;; MUL Vd, x16, Vb
             (a64-mul buf pd +a64-x16+ pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL26LO Vd, Va, Vb ----
          ;; Low 26 bits of untag(Va)*untag(Vb), tagged
          ((= op +op-mul26lo+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asr-imm buf +a64-x16+ pa 1)  ; untag va
             (a64-asr-imm buf +a64-x17+ pb 1)  ; untag vb
             (a64-mul buf pd +a64-x16+ +a64-x17+) ; 64-bit result
             ;; UBFX pd, pd, #0, #26 → UBFM pd, pd, #0, #25
             (a64-ubfm buf pd pd 0 25)          ; mask to 26 bits
             (a64-lsl-imm buf pd pd 1)          ; retag
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL26HI Vd, Va, Vb ----
          ;; Bits 26+ of untag(Va)*untag(Vb), tagged
          ((= op +op-mul26hi+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asr-imm buf +a64-x16+ pa 1)  ; untag va
             (a64-asr-imm buf +a64-x17+ pb 1)  ; untag vb
             (a64-mul buf pd +a64-x16+ +a64-x17+) ; 64-bit result
             (a64-lsr-imm buf pd pd 26)         ; shift right 26
             (a64-lsl-imm buf pd pd 1)          ; retag
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL64LO Vd, Va, Vb ----
          ;; Low 64 bits of raw Va*Vb (no tag/untag)
          ((= op +op-mul64lo+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; MUL Xd, Xa, Xb — raw 64-bit multiply, no untag/retag
             (a64-mul buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MUL64HI Vd, Va, Vb ----
          ;; High 64 bits of raw Va*Vb (no tag/untag)
          ((= op +op-mul64hi+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; UMULH Xd, Xa, Xb — upper 64 bits of unsigned multiply
             (a64-umulh buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ACC128 Vaddr, Vlo, Vhi ----
          ;; mem128[Vaddr] += Vhi:Vlo  (128-bit accumulate, raw)
          ;; Strategy: push hi to stack, addr→x30, lo→x16, then:
          ;;   load mem[addr+0]→x17, ADDS x17,x17,x16, store back
          ;;   load mem[addr+8]→x16, pop hi→x17, ADC x16,x16,x17, store back
          ((= op +op-acc128+)
           (let* ((vaddr (vr 0))
                  (vlo (vr 1))
                  (vhi (vr 2)))
             ;; Step 1: Load hi into x17 and push to stack
             (let ((phi (ensure-src vhi +a64-x17+)))
               (unless (= phi +a64-x17+)
                 (a64-mov-reg buf +a64-x17+ phi)))
             (a64-stp-pre buf +a64-x17+ +a64-xzr+ +a64-sp+ -16)
             ;; Step 2: Load addr→x30, lo→x16
             (let ((paddr (ensure-src vaddr +a64-x16+)))
               (a64-mov-reg buf +a64-x30+ paddr))
             (let ((plo (ensure-src vlo +a64-x16+)))
               (unless (= plo +a64-x16+)
                 (a64-mov-reg buf +a64-x16+ plo)))
             ;; Now: x30=addr, x16=lo_add, stack[0]=hi_add
             ;; Step 3: Load mem_lo, ADDS, store
             (a64-ldur buf +a64-x17+ +a64-x30+ 0)       ; x17 = mem[addr+0]
             (a64-adds-reg buf +a64-x17+ +a64-x17+ +a64-x16+ 0 0) ; x17 += lo_add, sets C
             (a64-stur buf +a64-x17+ +a64-x30+ 0)       ; mem[addr+0] = x17
             ;; Step 4: Load mem_hi, pop hi_add, ADC, store
             (a64-ldur buf +a64-x16+ +a64-x30+ 8)       ; x16 = mem[addr+8]
             (a64-ldp-post buf +a64-x17+ +a64-xzr+ +a64-sp+ 16)  ; pop hi_add→x17
             (a64-adc buf +a64-x16+ +a64-x16+ +a64-x17+) ; x16 += hi_add + carry
             (a64-stur buf +a64-x16+ +a64-x30+ 8)))

          ;; ---- DIV Vd, Va, Vb ----
          ;; Tagged fixnum divide: result = (Va / Vb) then re-tag
          ;; SDIV gives untagged quotient, must shift left by 1 to re-tag
          ((= op +op-div+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; SDIV x16, Va, Vb  (tagged / tagged = untagged)
             (a64-sdiv buf +a64-x16+ pa pb)
             ;; LSL Vd, x16, #1  (re-tag)
             (a64-lsl-imm buf pd +a64-x16+ 1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- MOD Vd, Va, Vb ----
          ;; Vd = Va - (Va/Vb)*Vb  (tagged)
          ((= op +op-mod+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; SDIV x16, Va, Vb
             (a64-sdiv buf +a64-x16+ pa pb)
             ;; MUL x16, x16, Vb  (quotient * divisor)
             (a64-mul buf +a64-x16+ +a64-x16+ pb)
             ;; SUB Vd, Va, x16  (remainder = dividend - q*divisor)
             (a64-sub-reg buf pd pa +a64-x16+ 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- NEG Vd, Vs ----
          ((= op +op-neg+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-neg buf pd ps)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- INC Vd ----
          ;; Tagged increment: add 2 (because tag bit is in bit 0)
          ((= op +op-inc+)
           (let* ((vd (vr 0))
                  (pd (ensure-src vd +a64-x16+)))
             (a64-add-imm buf pd pd 2)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- DEC Vd ----
          ((= op +op-dec+)
           (let* ((vd (vr 0))
                  (pd (ensure-src vd +a64-x16+)))
             (a64-sub-imm buf pd pd 2)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- AND Vd, Va, Vb ----
          ((= op +op-and+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-and-reg buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OR Vd, Va, Vb ----
          ((= op +op-or+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-orr-reg buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- XOR Vd, Va, Vb ----
          ((= op +op-xor+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-eor-reg buf pd pa pb)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SHL Vd, Vs, imm8 ----
          ((= op +op-shl+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (amt (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-lsl-imm buf pd ps amt)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SHR Vd, Vs, imm8 ----
          ((= op +op-shr+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (amt (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-lsr-imm buf pd ps amt)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SAR Vd, Vs, imm8 ----
          ((= op +op-sar+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (amt (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asr-imm buf pd ps amt)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SHLV Vd, Vs, Vc ---- (shift left by register)
          ((= op +op-shlv+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pc (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-lslv buf pd ps pc)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SARV Vd, Vs, Vc ---- (arithmetic shift right by register)
          ((= op +op-sarv+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pc (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-asrv buf pd ps pc)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- LDB Vd, Vs, pos, size ----
          ;; Bit field extract: UBFM Xd, Xn, #pos, #(pos+size-1)
          ((= op +op-ldb+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pos (vr 2))
                  (sz (vr 3))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ubfm buf pd ps pos (+ pos sz -1))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- CMP Va, Vb ----
          ((= op +op-cmp+)
           (let ((pa (ensure-src (vr 0) +a64-x16+))
                 (pb (ensure-src (vr 1) +a64-x17+)))
             (a64-cmp-reg buf pa pb)))

          ;; ---- TEST Va, Vb ----
          ((= op +op-test+)
           (let ((pa (ensure-src (vr 0) +a64-x16+))
                 (pb (ensure-src (vr 1) +a64-x17+)))
             (a64-tst-reg buf pa pb)))

          ;; ---- BR off16 ----
          ((= op +op-br+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (gethash target-byte mvm-to-native-label)))
             (unless label
               (setf label (incf *mvm-label-counter*))
               (setf (gethash target-byte mvm-to-native-label) label))
             (let ((idx (a64-current-index buf)))
               (a64-b buf 0)  ; placeholder
               (a64-add-fixup buf idx label :b))))

          ;; ---- Conditional branches: BEQ/BNE/BLT/BGE/BLE/BGT ----
          ((= op +op-beq+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-eq+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-bne+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-ne+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-blt+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-lt+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-bge+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-ge+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-ble+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-le+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ((= op +op-bgt+)
           (let* ((mvm-offset (vr 0))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-gt+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ;; ---- BNULL Vs, off16 ----
          ;; CMP Vs, VN(x26); B.EQ target
          ((= op +op-bnull+)
           (let* ((ps (ensure-src (vr 0) +a64-x16+))
                  (mvm-offset (vr 1))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (a64-cmp-reg buf ps +a64-x26+)
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-eq+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ;; ---- BNNULL Vs, off16 ----
          ((= op +op-bnnull+)
           (let* ((ps (ensure-src (vr 0) +a64-x16+))
                  (mvm-offset (vr 1))
                  (target-byte (+ (decoded-mvm-insn-offset insn)
                                  (decoded-mvm-insn-size insn)
                                  mvm-offset))
                  (label (or (gethash target-byte mvm-to-native-label)
                             (setf (gethash target-byte mvm-to-native-label)
                                   (incf *mvm-label-counter*)))))
             (a64-cmp-reg buf ps +a64-x26+)
             (let ((idx (a64-current-index buf)))
               (a64-bcond buf +cc-ne+ 0)
               (a64-add-fixup buf idx label :bcond))))

          ;; ---- CAR Vd, Vs ----
          ;; Cons cell layout: [car|cdr] with tag=1 (low bit)
          ;; CAR: LDUR Xd, [Xs, #-1]  (untag cons pointer)
          ((= op +op-car+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ldur buf pd ps -1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- CDR Vd, Vs ----
          ;; CDR: LDR Xd, [Xs, #7]  (untag + skip car = -1 + 8 = 7)
          ((= op +op-cdr+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-ldur buf pd ps 7)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- CONS Vd, Va, Vb ----
          ;; STP Va, Vb, [VA]  (store car, cdr at alloc pointer)
          ;; ADD Vd, VA, #1    (tag the pointer with cons tag = 1)
          ;; ADD VA, VA, #16   (bump alloc pointer by 2 words)
          ((= op +op-cons+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (pb (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-stp-offset buf pa pb +a64-x24+ 0)
             (a64-add-imm buf pd +a64-x24+ 1)
             (a64-add-imm buf +a64-x24+ +a64-x24+ 16)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- SETCAR Vd, Vs ----
          ;; STUR Vs, [Vd, #-1]  (untag and store to car)
          ((= op +op-setcar+)
           (let ((pd (ensure-src (vr 0) +a64-x16+))
                 (ps (ensure-src (vr 1) +a64-x17+)))
             (a64-stur buf ps pd -1)))

          ;; ---- SETCDR Vd, Vs ----
          ;; STUR Vs, [Vd, #7]  (untag + skip car)
          ((= op +op-setcdr+)
           (let ((pd (ensure-src (vr 0) +a64-x16+))
                 (ps (ensure-src (vr 1) +a64-x17+)))
             (a64-stur buf ps pd 7)))

          ;; ---- CONSP Vd, Vs ----
          ;; Test low 4 bits for cons tag (1)
          ;; AND x16, Vs, #0xF  →  use register-based: load 0xF, AND
          ;; CMP x16, #1
          ;; CSET Vd, EQ
          ((= op +op-consp+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             ;; Load mask into x17 scratch
             (a64-movz buf +a64-x17+ #xF 0)
             (a64-and-reg buf +a64-x16+ ps +a64-x17+)
             (a64-cmp-imm buf +a64-x16+ 1)
             (a64-cset buf pd +cc-eq+)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ATOM Vd, Vs ----
          ;; Opposite of consp: tag != 1
          ((= op +op-atom+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             (a64-movz buf +a64-x17+ #xF 0)
             (a64-and-reg buf +a64-x16+ ps +a64-x17+)
             (a64-cmp-imm buf +a64-x16+ 1)
             (a64-cset buf pd +cc-ne+)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ALLOC-OBJ Vd, count:imm16, subtag:imm8 ----
          ;; Allocate object with COUNT element slots from bump allocator
          ;; Header word: [element-count:48 | unused:8 | subtag:8]
          ;; = (count << 16) | subtag
          ((= op +op-alloc-obj+)
           (let* ((vd (vr 0))
                  (count (vr 1))
                  (subtag (vr 2))
                  ;; Total bytes = 8 (header) + count * 8 (data slots)
                  ;; Aligned to 16 bytes for cons pointer tag safety
                  (data-bytes (* count 8))
                  (total-size (logand (+ 8 data-bytes 15) (lognot 15)))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Build header: (count << 16) | subtag
             (a64-movz buf +a64-x16+ subtag 0)
             (a64-movk buf +a64-x16+ count 1)
             ;; Store header at alloc pointer
             (a64-stur buf +a64-x16+ +a64-x24+ 0)
             ;; Result = alloc pointer + 8 (skip header) + object tag (2)
             (a64-add-imm buf pd +a64-x24+ 10)  ; 8 (header) + 2 (tag)
             ;; Bump alloc pointer (aligned to 16 bytes)
             (if (<= total-size #xFFF)
                 (a64-add-imm buf +a64-x24+ +a64-x24+ total-size)
                 (progn
                   (a64-load-imm64 buf +a64-x17+ total-size)
                   (a64-add-reg buf +a64-x24+ +a64-x24+ +a64-x17+ 0 0)))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OBJ-REF Vd, Vobj, idx:imm8 ----
          ;; Load slot: LDR Vd, [Vobj + idx*8 - tag_offset]
          ((= op +op-obj-ref+)
           (let* ((vd (vr 0))
                  (vobj (vr 1))
                  (idx (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             (if (= vobj +vreg-vfp+)
                 ;; Frame slot access: use FP-relative offset below spill area
                 (let ((offset (+ +a64-frame-slot-base+ (* idx -8))))
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-ldur buf pd +a64-x29+ offset)
                       ;; Large offset: SUB x16, x29, #abs_offset; LDUR pd, [x16]
                       (progn
                         (a64-sub-imm buf +a64-x16+ +a64-x29+ (- offset))
                         (a64-ldur buf pd +a64-x16+ 0))))
                 ;; Normal object slot access
                 (let* ((pobj (ensure-src vobj +a64-x16+))
                        (offset (- (* idx 8) 2)))  ; subtract object tag
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-ldur buf pd pobj offset)
                       (progn
                         (a64-load-imm64 buf +a64-x17+ offset)
                         (a64-add-reg buf +a64-x17+ pobj +a64-x17+ 0 0)
                         (a64-ldur buf pd +a64-x17+ 0)))))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OBJ-SET Vobj, idx:imm8, Vs ----
          ((= op +op-obj-set+)
           (let* ((vobj (vr 0))
                  (idx (vr 1))
                  (ps (ensure-src (vr 2) +a64-x17+)))
             (if (= vobj +vreg-vfp+)
                 ;; Frame slot store: use FP-relative offset below spill area
                 (let ((offset (+ +a64-frame-slot-base+ (* idx -8))))
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-stur buf ps +a64-x29+ offset)
                       ;; Large offset: SUB x16, x29, #abs_offset; STUR ps, [x16]
                       (progn
                         (a64-sub-imm buf +a64-x16+ +a64-x29+ (- offset))
                         (a64-stur buf ps +a64-x16+ 0))))
                 ;; Normal object slot store
                 (let* ((pobj (ensure-src vobj +a64-x16+))
                        (offset (- (* idx 8) 2)))
                   (if (and (>= offset -256) (<= offset 255))
                       (a64-stur buf ps pobj offset)
                       (progn
                         (a64-load-imm64 buf +a64-x16+ offset)
                         (a64-add-reg buf +a64-x16+ pobj +a64-x16+ 0 0)
                         (a64-stur buf ps +a64-x16+ 0)))))))

          ;; ---- OBJ-TAG Vd, Vs ----
          ;; Extract low 4 bits: AND Vd, Vs, #0xF
          ((= op +op-obj-tag+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-movz buf +a64-x17+ #xF 0)
             (a64-and-reg buf pd ps +a64-x17+)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- OBJ-SUBTAG Vd, Vs ----
          ;; Load header word, extract subtag from bits [7:0]
          ((= op +op-obj-subtag+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Header is at [Vs - tag(2) - 8]
             (a64-ldur buf +a64-x17+ ps -10)
             (a64-movz buf +a64-x16+ #xFF 0)
             (a64-and-reg buf pd +a64-x17+ +a64-x16+)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- AREF Vd, Vobj, Vidx ----
          ;; Variable-index array load: Vd = obj[idx]
          ;; Vidx is raw (untagged) index, Vobj is tagged object pointer
          ((= op +op-aref+)
           (let* ((vd (vr 0))
                  (pobj (ensure-src (vr 1) +a64-x16+))
                  (pidx (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Compute address: x16 = (Vobj - 2) + Vidx * 4
             ;; Vidx is tagged fixnum: real_idx*2, *4 gives real_idx*8
             (a64-sub-imm buf +a64-x16+ pobj 2)
             (a64-add-reg buf +a64-x16+ +a64-x16+ pidx 0 2)
             ;; Load from computed address
             (a64-ldur buf pd +a64-x16+ 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ASET Vobj, Vidx, Vs ----
          ;; Variable-index array store: obj[idx] = Vs
          ((= op +op-aset+)
           (let* ((pobj (ensure-src (vr 0) +a64-x16+))
                  (pidx (ensure-src (vr 1) +a64-x17+)))
             ;; Compute address: x16 = (Vobj - 2) + Vidx * 4
             ;; Vidx is tagged fixnum: real_idx*2, *4 gives real_idx*8
             (a64-sub-imm buf +a64-x16+ pobj 2)
             (a64-add-reg buf +a64-x16+ +a64-x16+ pidx 0 2)
             ;; Reload value into x17 (safe — done with idx)
             (let ((ps (ensure-src (vr 2) +a64-x17+)))
               (a64-stur buf ps +a64-x16+ 0))))

          ;; ---- ARRAY-LEN Vd, Vobj ----
          ;; Extract element count from object header, return as tagged fixnum
          ;; Header at [Vobj - 10], element-count = header >> 16
          ((= op +op-array-len+)
           (let* ((vd (vr 0))
                  (ps (ensure-src (vr 1) +a64-x16+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Load header: at Vobj - 2 (tag) - 8 (header size) = Vobj - 10
             (a64-ldur buf +a64-x17+ ps -10)
             ;; Extract element count: LSR by 16, then tag as fixnum (LSL 1)
             ;; Combined: LSR by 15 then clear low bit
             ;; Simpler: two shifts
             (a64-lsr-imm buf +a64-x17+ +a64-x17+ 16)
             (a64-lsl-imm buf pd +a64-x17+ 1)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- ALLOC-ARRAY Vd, Vcount ----
          ;; Dynamic array allocation: count is in a register (raw, untagged)
          ;; Header: (count << 16) | #x32 (array subtag)
          ;; Allocate 8 + count*8 bytes, aligned to 16
          ((= op +op-alloc-array+)
           (let* ((vd (vr 0))
                  (pcount (ensure-src (vr 1) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             ;; Build header: x16 = (count << 16) | #x32
             (a64-lsl-imm buf +a64-x16+ pcount 16)
             (a64-movk buf +a64-x16+ #x32 0)
             ;; Store header at alloc pointer
             (a64-stur buf +a64-x16+ +a64-x24+ 0)
             ;; Compute aligned allocation: floor((count+2)/2) * 16
             ;; This equals (8 + count*8) rounded up to 16
             (a64-add-imm buf +a64-x17+ pcount 2)
             (a64-lsr-imm buf +a64-x17+ +a64-x17+ 1)
             (a64-lsl-imm buf +a64-x17+ +a64-x17+ 4)
             ;; Result = alloc_ptr + 10 (8 header + 2 tag)
             (a64-add-imm buf pd +a64-x24+ 10)
             ;; Bump alloc pointer
             (a64-add-reg buf +a64-x24+ +a64-x24+ +a64-x17+ 0 0)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- LOAD Vd, Vaddr, width ----
          ((= op +op-load+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (width (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x17+)))
             (a64-ldr-width buf pd pa 0 width)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- STORE Vaddr, Vs, width ----
          ((= op +op-store+)
           (let ((pa (ensure-src (vr 0) +a64-x16+))
                 (ps (ensure-src (vr 1) +a64-x17+))
                 (width (vr 2)))
             (a64-str-width buf ps pa 0 width)))

          ;; ---- FENCE ----
          ((= op +op-fence+)
           (a64-dmb buf #xB))  ; DMB ISH

          ;; ---- CALL target:imm32 ----
          ;; Target operand is the bytecode offset of the called function.
          ;; Look up in mvm-to-native-label (bytecode-offset → label, set during init).
          ((= op +op-call+)
           (let* ((target-offset (vr 0))
                  (label (gethash target-offset mvm-to-native-label)))
             (when label
               (let ((idx (a64-current-index buf)))
                 (a64-bl buf 0)  ; placeholder
                 (a64-add-fixup buf idx label :bl)))))

          ;; ---- CALL-IND Vs ----
          ((= op +op-call-ind+)
           (let ((ps (ensure-src (vr 0) +a64-x16+)))
             (a64-blr buf ps)))

          ;; ---- RET ----
          ((= op +op-ret+)
           (a64-emit-epilogue buf))

          ;; ---- TAILCALL target:imm32 ----
          ;; Target operand is the bytecode offset of the called function.
          ;; Restore frame, then B (not BL) to target.
          ((= op +op-tailcall+)
           (let* ((target-offset (vr 0))
                  (label (gethash target-offset mvm-to-native-label)))
             (when label
               ;; Deallocate spill/frame-slot area and restore callee-saved regs
               ;; x24/x25/x26 are global state — NOT restored
               (a64-add-imm buf +a64-sp+ +a64-sp+ +a64-locals-frame-size+)
               (a64-ldp-offset buf +a64-x23+ +a64-xzr+ +a64-sp+ 48)
               (a64-ldp-offset buf +a64-x21+ +a64-x22+ +a64-sp+ 32)
               (a64-ldp-offset buf +a64-x19+ +a64-x20+ +a64-sp+ 16)
               (a64-ldp-post buf +a64-x29+ +a64-x30+ +a64-sp+ 80)
               (let ((idx (a64-current-index buf)))
                 (a64-b buf 0)
                 (a64-add-fixup buf idx label :b)))))

          ;; ---- ALLOC-CONS Vd ----
          ;; Bump-allocate 16 bytes, return untagged pointer in Vd
          ((= op +op-alloc-cons+)
           (let* ((vd (vr 0))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-mov-reg buf pd +a64-x24+)
             (a64-add-imm buf +a64-x24+ +a64-x24+ 16)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- GC-CHECK ----
          ;; CMP VA, VL; B.LT ok; BRK #1 (GC trap); ok:
          ((= op +op-gc-check+)
           (a64-cmp-reg buf +a64-x24+ +a64-x25+)
           (a64-bcond buf +cc-lt+ 2)  ; skip BRK if VA < VL
           (a64-brk buf 1))           ; GC needed trap

          ;; ---- WRITE-BARRIER Vobj ----
          ;; Mark the card table entry dirty (simplified: just a DMB for now)
          ((= op +op-write-barrier+)
           ;; In a real implementation this would compute the card table
           ;; offset and store a dirty byte. For now, emit a memory barrier.
           (a64-dmb buf #xB))

          ;; ---- SAVE-CTX Vd ----
          ;; Real setjmp semantics for actor context switching.
          ;; Vd holds save area address (untagged by compiler).
          ;; Returns 0 (initial save) or 2 (resumed via restore-ctx).
          ;; Extra callee-saved (x20-x23, x29, x30) are pushed to the stack
          ;; so they're recovered when SP is restored. Only essential state
          ;; (SP, x24, x25, x19, continuation, obj-alloc/limit) goes in save area.
          ((= op +op-save-ctx+)
           (let* ((vd (vr 0))
                  (pa (ensure-src vd +a64-x0+)))
             ;; 1. Push extra callee-saved to stack (48 bytes, 16-byte aligned)
             (a64-stp-pre buf +a64-x20+ +a64-x21+ +a64-sp+ -48)
             (a64-stp-offset buf +a64-x22+ +a64-x23+ +a64-sp+ 16)
             (a64-stp-offset buf +a64-x29+ +a64-x30+ +a64-sp+ 32)
             ;; 2. Save SP (post-push) to save area [pa+0x00]
             (a64-add-imm buf +a64-x16+ +a64-sp+ 0)   ; MOV x16, SP
             (a64-str-unsigned buf +a64-x16+ pa 0)     ; [pa+0x00] = SP
             ;; 3. Save key registers to save area
             (a64-str-unsigned buf +a64-x24+ pa 8)     ; [pa+0x08] = x24 (alloc ptr)
             (a64-str-unsigned buf +a64-x25+ pa 16)    ; [pa+0x10] = x25 (alloc limit)
             (a64-str-unsigned buf +a64-x19+ pa 24)    ; [pa+0x18] = x19 (V4)
             ;; 4. ADR x17 → continuation; store to [pa+0x28]
             (let ((adr-idx (a64-current-index buf)))
               (a64-emit buf 0)                         ; placeholder for ADR x17
               (a64-str-unsigned buf +a64-x17+ pa #x28) ; [pa+0x28] = continuation
               ;; 5. Save per-CPU obj-alloc/obj-limit from TPIDR_EL1
               (a64-mrs buf +a64-x16+ +sysreg-tpidr-el1+)
               (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ #x28)
               (a64-str-unsigned buf +a64-x17+ pa #x68)  ; [pa+0x68] = obj-alloc
               (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ #x30)
               (a64-str-unsigned buf +a64-x17+ pa #x70)  ; [pa+0x70] = obj-limit
               ;; 6. Initial save: return 0
               (a64-movz buf +a64-x0+ 0 0)
               ;; 7. B to pop (skip resume entry)
               (let ((b-idx (a64-current-index buf)))
                 (a64-b buf 0)                           ; placeholder
                 ;; 8. Continuation label — restore-ctx BR's here
                 (let ((cont-idx (a64-current-index buf)))
                   ;; Patch ADR x17, <continuation>
                   ;; ADR: immlo(2)|10000|immhi(19)|Rd(5)
                   (let* ((byte-off (* (- cont-idx adr-idx) 4))
                          (immlo (logand byte-off 3))
                          (immhi (logand (ash byte-off -2) #x7FFFF)))
                     (setf (aref (a64-buffer-code buf) adr-idx)
                           (logior (ash immlo 29)
                                   (ash #b10000 24)
                                   (ash immhi 5)
                                   +a64-x17+)))
                   ;; Resume path: return 2 (tagged fixnum 1)
                   (a64-movz buf +a64-x0+ 2 0)
                   ;; 9. Pop callee-saved (both paths converge here)
                   (let ((pop-idx (a64-current-index buf)))
                     ;; Patch B forward to here
                     (setf (aref (a64-buffer-code buf) b-idx)
                           (logior (ash #b000101 26)
                                   (logand (- pop-idx b-idx) #x3FFFFFF)))
                     (a64-ldp-offset buf +a64-x29+ +a64-x30+ +a64-sp+ 32)
                     (a64-ldp-offset buf +a64-x22+ +a64-x23+ +a64-sp+ 16)
                     (a64-ldp-post buf +a64-x20+ +a64-x21+ +a64-sp+ 48)
                     ;; 10. Store result (x0) into Vd
                     (store-dst +a64-x0+ vd)))))))

          ;; ---- RESTORE-CTX Vd ----
          ;; Real longjmp semantics. Restores registers from save area,
          ;; switches SP, releases scheduler lock, enables interrupts,
          ;; then BR to saved continuation. Never returns.
          ((= op +op-restore-ctx+)
           (let* ((vd (vr 0))
                  (pa (ensure-src vd +a64-x0+)))
             ;; Move addr to x16 (pa may be x19/x24/x25 which get overwritten)
             (a64-mov-reg buf +a64-x16+ pa)
             ;; Load continuation address into x17
             (a64-ldr-unsigned buf +a64-x17+ +a64-x16+ #x28)
             ;; Restore per-CPU obj-alloc/obj-limit via TPIDR_EL1
             (a64-mrs buf +a64-x0+ +sysreg-tpidr-el1+)
             (a64-ldr-unsigned buf +a64-x1+ +a64-x16+ #x68)
             (a64-str-unsigned buf +a64-x1+ +a64-x0+ #x28)  ; TPIDR+0x28 = obj-alloc
             (a64-ldr-unsigned buf +a64-x1+ +a64-x16+ #x70)
             (a64-str-unsigned buf +a64-x1+ +a64-x0+ #x30)  ; TPIDR+0x30 = obj-limit
             ;; Restore callee-saved from save area
             (a64-ldr-unsigned buf +a64-x19+ +a64-x16+ #x18)
             (a64-ldr-unsigned buf +a64-x24+ +a64-x16+ #x08)
             (a64-ldr-unsigned buf +a64-x25+ +a64-x16+ #x10)
             ;; Restore SP from save area (AFTER register loads, SP change is the
             ;; "point of no return" — we're now on the resumed actor's stack)
             (a64-ldr-unsigned buf +a64-x0+ +a64-x16+ 0)
             (a64-add-imm buf +a64-sp+ +a64-x0+ 0)     ; MOV SP, x0
             ;; Release scheduler lock (MUST be after SP switch to prevent
             ;; another CPU from dequeuing this actor while on its stack)
             (when *aarch64-sched-lock-addr*
               (a64-load-imm64 buf +a64-x0+ *aarch64-sched-lock-addr*)
               (a64-str-unsigned buf +a64-xzr+ +a64-x0+ 0) ; store 0 → unlock
               ;; Memory barrier — ensure lock release visible to other CPUs
               (a64-dmb buf #xB)
               ;; Enable interrupts
               (a64-msr-daifclr buf #x3))
             ;; Jump to continuation (save-ctx's resume entry point)
             (a64-br buf +a64-x17+)))

          ;; ---- YIELD ----
          ;; Preemption check point (emitted at end of every loop iteration).
          ;; QEMU virt: SEV+WFE (SEV sets event register, WFE sees it and
          ;; returns immediately — lets QEMU process events).
          ;; QEMU raspi3b: NOP (WFE halts CPU with no wake source).
          ((= op +op-yield+)
           (if *aarch64-yield-nop*
               (a64-nop buf)
               (progn
                 (a64-emit buf #xD503209F)   ; SEV (send event)
                 (a64-wfe buf))))

          ;; ---- ATOMIC-XCHG Vd, Vaddr, Vs ----
          ;; LDXR/STXR loop for atomic exchange.
          ;; STXR Ws, Xt, [Xn] requires Ws ≠ Xt and Ws ≠ Xn (ARM spec).
          ;; Pick status register that doesn't conflict with pa or ps.
          ((= op +op-atomic-xchg+)
           (let* ((vd (vr 0))
                  (pa (ensure-src (vr 1) +a64-x16+))
                  (ps (ensure-src (vr 2) +a64-x17+))
                  (pd (or (a64-phys-reg vd) +a64-x16+))
                  ;; Status reg must differ from ps (Xt) and pa (Xn)
                  (status (cond ((/= ps +a64-x17+) +a64-x17+)
                                ((/= pa 15) 15)     ; x15
                                (t +a64-x0+)))
                  (loop-idx (a64-current-index buf)))
             ;; loop: LDXR Xd, [Vaddr]
             (a64-ldxr buf pd pa)
             ;; STXR Ws, Vs, [Vaddr]
             (a64-stxr buf status ps pa)
             ;; CBNZ Ws, loop (32-bit variant, sf=0)
             (let ((back-offset (- loop-idx (a64-current-index buf))))
               (a64-emit buf (logior #x35000000
                                     (ash (logand back-offset #x7FFFF) 5)
                                     status)))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- IO-READ Vd, port:imm16, width:imm8 ----
          ;; On AArch64, MMIO: load the address (port as MMIO base + offset)
          ;; For bare-metal, port is treated as an absolute MMIO address
          ((= op +op-io-read+)
           (let* ((vd (vr 0))
                  (port (vr 1))
                  (width (vr 2))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-load-imm64 buf +a64-x17+ port)
             (a64-ldr-width buf pd +a64-x17+ 0 width)
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- IO-WRITE port:imm16, Vs, width:imm8 ----
          ((= op +op-io-write+)
           (let ((port (vr 0))
                 (ps (ensure-src (vr 1) +a64-x17+))
                 (width (vr 2)))
             (a64-load-imm64 buf +a64-x16+ port)
             (a64-str-width buf ps +a64-x16+ 0 width)))

          ;; ---- HALT ----
          ;; WFI for idle scheduler loop. Wakes on interrupt (SGI/timer).
          ;; Semihosting exit is handled by a separate shutdown() function.
          ((= op +op-halt+)
           (a64-wfi buf))

          ;; ---- CLI (disable interrupts) ----
          ((= op +op-cli+)
           ;; MSR DAIFSet, #0x3  (mask IRQ + FIQ)
           (a64-msr-daifset buf #x3))

          ;; ---- STI (enable interrupts) ----
          ((= op +op-sti+)
           ;; MSR DAIFClr, #0x3  (unmask IRQ + FIQ)
           (a64-msr-daifclr buf #x3))

          ;; ---- PERCPU-REF Vd, offset:imm16 ----
          ;; Read per-CPU data via TPIDR_EL1
          ((= op +op-percpu-ref+)
           (let* ((vd (vr 0))
                  (offset (vr 1))
                  (pd (or (a64-phys-reg vd) +a64-x16+)))
             (a64-mrs buf +a64-x17+ +sysreg-tpidr-el1+)  ; system reg encoding for TPIDR_EL1
             ;; LDR Xd, [x17, #offset]
             (if (and (zerop (mod offset 8)) (<= offset (* #xFFF 8)))
                 (a64-ldr-unsigned buf pd +a64-x17+ offset)
                 (progn
                   (a64-load-imm64 buf +a64-x16+ offset)
                   (a64-add-reg buf +a64-x17+ +a64-x17+ +a64-x16+ 0 0)
                   (a64-ldur buf pd +a64-x17+ 0)))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- PERCPU-SET offset:imm16, Vs ----
          ((= op +op-percpu-set+)
           (let ((offset (vr 0))
                 (ps (ensure-src (vr 1) +a64-x17+)))
             (a64-mrs buf +a64-x16+ +sysreg-tpidr-el1+)
             (if (and (zerop (mod offset 8)) (<= offset (* #xFFF 8)))
                 (a64-str-unsigned buf ps +a64-x16+ offset)
                 (progn
                   (a64-load-imm64 buf +a64-x17+ offset)
                   (a64-add-reg buf +a64-x16+ +a64-x16+ +a64-x17+ 0 0)
                   (a64-stur buf ps +a64-x16+ 0)))))

          ;; ---- FN-ADDR Vd, target:imm32 ----
          ;; Load tagged function address. Target is bytecode offset.
          ;; Resolved via ADR (PC-relative), then LSL #1 to tag as fixnum.
          ((= op +op-fn-addr+)
           (let* ((vd (vr 0))
                  (target-offset (vr 1))
                  (pd (or (a64-phys-reg vd) +a64-x16+))
                  (label (gethash target-offset mvm-to-native-label)))
             (if label
                 (let ((idx (a64-current-index buf)))
                   ;; ADR Xd, target — placeholder, resolved by fixup
                   ;; Result is raw address (NOT tagged) — matches x64 behavior.
                   ;; CALL-IND uses BLR which needs a raw address.
                   (a64-emit buf (logior (ash #b10000 24) pd))
                   (a64-add-fixup buf idx label :adr))
                 ;; Unknown target: load 0
                 (a64-movz buf pd 0 0))
             (unless (a64-phys-reg vd)
               (store-dst pd vd))))

          ;; ---- Unknown opcode ----
          (t
           ;; Emit a BRK with the opcode as immediate for debugging
           (a64-brk buf op)))))))

;;; ============================================================
;;; Main Translation Entry Point
;;; ============================================================

(defun translate-mvm-to-aarch64 (bytecode function-table)
  "Translate MVM bytecode to AArch64 native code.
   BYTECODE is a vector of (unsigned-byte 8) containing MVM instructions.
   FUNCTION-TABLE is a hash table mapping function index → MVM byte offset,
   or NIL if translation is for a single function body.

   Returns (values a64-buffer fn-offset-map) where fn-offset-map maps
   MVM bytecode offset → native byte offset for each function entry point.

   The translation proceeds in multiple passes:
     1. Decode all MVM instructions
     2. Build byte-offset → label mapping for branch targets
     3. Emit prologue
     4. Translate each instruction, recording native label positions
     5. Emit epilogue
     6. Resolve all branch fixups"
  (let* ((buf (make-a64-buffer))
         (insns (decode-mvm-stream bytecode))
         (offset-map (build-offset-to-index-map insns))
         (mvm-to-native-label (make-hash-table :test 'equal))
         ;; Pre-assign labels for all MVM byte offsets that might be
         ;; branch targets. We assign labels for every instruction
         ;; position conservatively.
         (mvm-offset-to-native-index (make-hash-table :test 'eql))
         ;; Track function entry positions: bytecode-offset → native-byte-offset
         (fn-entry-offsets (make-hash-table :test 'eql))
         (fn-bc-offsets (make-hash-table :test 'eql)))

    ;; Pre-register function entry points in the label table
    (when function-table
      (maphash (lambda (func-idx mvm-offset)
                 (let ((label (incf *mvm-label-counter*)))
                   (setf (gethash (list :func func-idx) mvm-to-native-label) label)
                   (setf (gethash mvm-offset mvm-to-native-label) label)
                   ;; Remember this bytecode offset is a function entry
                   (setf (gethash mvm-offset fn-bc-offsets) t)))
               function-table))

    ;; Pre-pass: register labels for ALL branch targets (including backward branches)
    (dolist (insn insns)
      (let ((op (decoded-mvm-insn-opcode insn))
            (operands (decoded-mvm-insn-operands insn)))
        (when (and (>= op #x40) (<= op #x48))  ; BR through BNNULL
          (let* ((off-idx (if (or (= op #x47) (= op #x48)) 1 0))  ; BNULL/BNNULL have Vs first
                 (mvm-offset (nth off-idx operands))
                 (target-byte (+ (decoded-mvm-insn-offset insn)
                                 (decoded-mvm-insn-size insn)
                                 mvm-offset)))
            (unless (gethash target-byte mvm-to-native-label)
              (setf (gethash target-byte mvm-to-native-label)
                    (incf *mvm-label-counter*)))))))

    ;; Pass 1: Translate all instructions
    (dolist (insn insns)
      (let ((mvm-off (decoded-mvm-insn-offset insn)))
        ;; If this MVM offset has a label assigned (branch target),
        ;; record the native position for it now
        (let ((label (gethash mvm-off mvm-to-native-label)))
          (when label
            (a64-set-label buf label)))
        ;; Record native position for function entries
        (when (gethash mvm-off fn-bc-offsets)
          (setf (gethash mvm-off fn-entry-offsets)
                (* (a64-current-index buf) 4)))  ; instruction index → byte offset
        ;; Record MVM offset → native index mapping
        (setf (gethash mvm-off mvm-offset-to-native-index)
              (a64-current-index buf))
        ;; Translate the instruction
        (translate-mvm-insn insn buf mvm-to-native-label)))

    ;; Any labels pointing to the end of the bytecode stream
    (let ((end-offset (length bytecode)))
      (let ((label (gethash end-offset mvm-to-native-label)))
        (when label
          (a64-set-label buf label))))

    ;; Pass 2: Resolve all branch fixups
    (a64-resolve-fixups buf)

    (values buf fn-entry-offsets)))

(defun translate-mvm-function (bytecode)
  "Translate a single MVM function body to AArch64 native code.
   Wraps the translated code with prologue and epilogue.
   Returns the native bytes as a (vector (unsigned-byte 8))."
  (let* ((buf (make-a64-buffer)))
    ;; Emit prologue
    (a64-emit-prologue buf)
    ;; Translate the body
    (let* ((body-buf (translate-mvm-to-aarch64 bytecode nil))
           (body-code (a64-buffer-code body-buf))
           (body-len (a64-buffer-position body-buf)))
      ;; Copy body instructions into our buffer
      (dotimes (i body-len)
        (a64-emit buf (aref body-code i))))
    ;; Emit epilogue
    (a64-emit-epilogue buf)
    ;; Convert to bytes
    (a64-buffer-to-bytes buf)))

;;; ============================================================
;;; Multi-function Translation
;;; ============================================================

(defun translate-mvm-image (bytecode function-table)
  "Translate an entire MVM image (multiple functions) to AArch64.
   FUNCTION-TABLE maps function-index → (mvm-byte-offset . arity).

   Returns a byte vector of AArch64 machine code with each function
   preceded by its prologue and followed by its epilogue.

   Also returns as a second value a hash table mapping
   function-index → native-byte-offset."
  (let* ((buf (make-a64-buffer))
         (func-offsets (make-hash-table :test 'eql))
         (mvm-to-native-label (make-hash-table :test 'equal))
         ;; Collect function entries sorted by offset
         (func-entries nil))

    ;; Build sorted list of (func-idx mvm-offset arity)
    (maphash (lambda (idx info)
               (let ((off (if (consp info) (car info) info))
                     (arity (if (consp info) (cdr info) 0)))
                 (push (list idx off arity) func-entries)))
             function-table)
    (setf func-entries (sort func-entries #'< :key #'second))

    ;; Pre-assign labels for function entries
    (dolist (entry func-entries)
      (destructuring-bind (func-idx mvm-off arity) entry
        (declare (ignore arity))
        (let ((label (incf *mvm-label-counter*)))
          (setf (gethash (list :func func-idx) mvm-to-native-label) label)
          (setf (gethash mvm-off mvm-to-native-label) label))))

    ;; Decode all MVM instructions
    (let ((all-insns (decode-mvm-stream bytecode)))

      ;; Translate all instructions with prologues at function boundaries
      (let ((func-offsets-set (make-hash-table :test 'eql)))
        (dolist (entry func-entries)
          (setf (gethash (second entry) func-offsets-set) (first entry)))

        (dolist (insn all-insns)
          (let* ((mvm-off (decoded-mvm-insn-offset insn))
                 (func-idx (gethash mvm-off func-offsets-set)))
            ;; Emit prologue at function entry points
            (when func-idx
              (let ((label (gethash (list :func func-idx) mvm-to-native-label)))
                (when label
                  (a64-set-label buf label)))
              (setf (gethash func-idx func-offsets)
                    (* (a64-current-index buf) 4))
              (a64-emit-prologue buf))

            ;; Set label if this offset is a branch target
            (let ((label (gethash mvm-off mvm-to-native-label)))
              (when (and label (not func-idx))
                (a64-set-label buf label)))

            ;; Translate the instruction
            ;; Special handling: MVM-RET becomes epilogue
            (if (= (decoded-mvm-insn-opcode insn) +op-ret+)
                (a64-emit-epilogue buf)
                (translate-mvm-insn insn buf mvm-to-native-label))))))

    ;; Handle labels pointing past the last instruction
    (let ((end-offset (length bytecode)))
      (let ((label (gethash end-offset mvm-to-native-label)))
        (when label
          (a64-set-label buf label))))

    ;; Resolve fixups
    (a64-resolve-fixups buf)

    (values (a64-buffer-to-bytes buf) func-offsets)))

;;; ============================================================
;;; Target Descriptor Installation
;;; ============================================================

(defun install-aarch64-translator ()
  "Install the AArch64 translator into the target descriptor.
   Sets the translate-fn, emit-prologue, and emit-epilogue slots
   on *target-aarch64*."
  (setf (target-translate-fn *target-aarch64*)
        #'translate-mvm-to-aarch64)
  (setf (target-emit-prologue *target-aarch64*)
        (lambda (target buf)
          (declare (ignore target))
          (a64-emit-prologue buf)))
  (setf (target-emit-epilogue *target-aarch64*)
        (lambda (target buf)
          (declare (ignore target))
          (a64-emit-epilogue buf)))
  :aarch64)

;;; ============================================================
;;; Diagnostic: Disassemble Native Buffer
;;; ============================================================

(defun a64-disassemble-buffer (buf &key (start 0) (end nil))
  "Print a hex dump of the AArch64 instruction buffer for debugging."
  (let* ((code (a64-buffer-code buf))
         (limit (or end (a64-buffer-position buf))))
    (loop for i from start below limit
          do (format t "  ~4,'0X: ~8,'0X~%" (* i 4) (aref code i)))))

(defun a64-instruction-count (buf)
  "Return the number of instructions in the buffer."
  (a64-buffer-position buf))

(defun a64-code-size (buf)
  "Return the total code size in bytes."
  (* (a64-buffer-position buf) 4))
