;;;; translate-68k.lisp - MVM to Motorola 68000 Translator
;;;;
;;;; Translates MVM virtual ISA bytecode to native Motorola 68000 machine code.
;;;; The 68k has a split register file (D0-D7 data, A0-A7 address) and
;;;; is BIG-ENDIAN -- the first endianness challenge for MVM translation.
;;;;
;;;; Register mapping (from target.lisp):
;;;;   V0  -> D0   (arg0, return value)
;;;;   V1  -> D1   (arg1)
;;;;   V2  -> D2   (arg2)
;;;;   V3  -> D3   (arg3)
;;;;   V4  -> D4   (callee-saved)
;;;;   V5  -> D5   (callee-saved)
;;;;   V6  -> D6   (callee-saved)
;;;;   V7  -> D7   (callee-saved)
;;;;   V8-V15 -> stack spill
;;;;   VR  -> D0   (return value, aliases V0)
;;;;   VA  -> A2   (alloc pointer, address reg)
;;;;   VL  -> A3   (alloc limit, address reg)
;;;;   VN  -> A4   (NIL constant, address reg)
;;;;   VSP -> A7   (stack pointer = SP)
;;;;   VFP -> A6   (frame pointer = FP)
;;;;
;;;; Scratch registers: A0, A1 (caller-saved address registers)
;;;; All 8 data registers are mapped to virtual registers.
;;;;
;;;; 68000 instruction encoding is variable-length (2-10 bytes per insn).
;;;; Instructions are word-aligned (16-bit words). Extensions follow the
;;;; first instruction word.
;;;;
;;;; Key 68k concepts:
;;;;   - Effective Address (EA) modes: Dn, An, (An), (An)+, -(An),
;;;;     d16(An), d8(An,Xi), abs.W, abs.L, #imm
;;;;   - Data registers (Dn) for arithmetic
;;;;   - Address registers (An) for pointer operations
;;;;   - MOVE is the most general instruction
;;;;   - SUB/CMP set condition codes in the status register (SR)
;;;;   - MOVEM for saving/restoring multiple registers
;;;;   - PEA for pushing effective addresses

(in-package :modus.mvm)

;;; ============================================================
;;; 68k Register Encoding
;;; ============================================================
;;;
;;; In 68k instruction encoding, register numbers are 3 bits (0-7).
;;; Data registers D0-D7 and address registers A0-A7 are distinguished
;;; by mode bits in the effective address field.

;; Data registers
(defconstant +68k-d0+ 0)
(defconstant +68k-d1+ 1)
(defconstant +68k-d2+ 2)
(defconstant +68k-d3+ 3)
(defconstant +68k-d4+ 4)
(defconstant +68k-d5+ 5)
(defconstant +68k-d6+ 6)
(defconstant +68k-d7+ 7)

;; Address registers
(defconstant +68k-a0+ 0)
(defconstant +68k-a1+ 1)
(defconstant +68k-a2+ 2)   ; VA (alloc pointer)
(defconstant +68k-a3+ 3)   ; VL (alloc limit)
(defconstant +68k-a4+ 4)   ; VN (NIL)
(defconstant +68k-a5+ 5)   ; Available
(defconstant +68k-a6+ 6)   ; VFP (frame pointer)
(defconstant +68k-a7+ 7)   ; VSP (stack pointer)

;;; ============================================================
;;; Virtual -> Physical Register Mapping
;;; ============================================================

;; Register type: :data or :address
(defstruct m68k-reg
  type    ; :data or :address
  number) ; 0-7

(defparameter *68k-vreg-map*
  (vector (make-m68k-reg :type :data :number +68k-d0+)     ; V0
          (make-m68k-reg :type :data :number +68k-d1+)     ; V1
          (make-m68k-reg :type :data :number +68k-d2+)     ; V2
          (make-m68k-reg :type :data :number +68k-d3+)     ; V3
          (make-m68k-reg :type :data :number +68k-d4+)     ; V4
          (make-m68k-reg :type :data :number +68k-d5+)     ; V5
          (make-m68k-reg :type :data :number +68k-d6+)     ; V6
          (make-m68k-reg :type :data :number +68k-d7+)     ; V7
          nil nil nil nil   ; V8-V11 (spill)
          nil nil nil nil   ; V12-V15 (spill)
          (make-m68k-reg :type :data :number +68k-d0+)     ; VR (aliases V0)
          (make-m68k-reg :type :address :number +68k-a2+)  ; VA
          (make-m68k-reg :type :address :number +68k-a3+)  ; VL
          (make-m68k-reg :type :address :number +68k-a4+)  ; VN
          (make-m68k-reg :type :address :number +68k-a7+)  ; VSP
          (make-m68k-reg :type :address :number +68k-a6+)  ; VFP
          nil))  ; VPC

;;; ============================================================
;;; 68k Code Buffer
;;; ============================================================
;;;
;;; 68k instructions are big-endian and word-aligned.
;;; We emit 16-bit words. The buffer stores them as words
;;; and converts to big-endian bytes at the end.

(defstruct m68k-buffer
  (words (make-array 65536))            ; fixed-size, position tracks fill
  (labels (make-hash-table :test 'eql))
  (fixups nil)      ; list of (word-index label-id fixup-type)
  (position 0)      ; byte position
  (word-count 0))   ; word index (position / 2)

(defun m68k-emit-word (buf word)
  "Emit a 16-bit instruction word."
  (let ((idx (m68k-buffer-word-count buf)))
    (setf (aref (m68k-buffer-words buf) idx) (logand word #xFFFF))
    (setf (m68k-buffer-word-count buf) (+ idx 1))
    (incf (m68k-buffer-position buf) 2)))

(defun m68k-emit-long (buf val)
  "Emit a 32-bit value as two 16-bit words (big-endian: high word first)."
  (m68k-emit-word buf (logand (ash val -16) #xFFFF))   ; high
  (m68k-emit-word buf (logand val #xFFFF)))             ; low

(defun m68k-current-pos (buf)
  "Current byte position in the code buffer."
  (m68k-buffer-position buf))

(defun m68k-emit-label (buf label-id)
  "Record current position as a branch target."
  (setf (gethash label-id (m68k-buffer-labels buf))
        (m68k-buffer-position buf)))

(defun m68k-emit-fixup (buf label-id fixup-type)
  "Record a fixup. FIXUP-TYPE is :disp16 or :disp32."
  (push (list (1- (m68k-buffer-word-count buf))
              label-id fixup-type
              (m68k-buffer-position buf))
        (m68k-buffer-fixups buf)))

(defun m68k-fixup-labels (buf)
  "Resolve all branch label references.  Each displacement size is
   range-checked before encoding — out-of-range silently truncates."
  (let ((words (m68k-buffer-words buf)))
    (dolist (fixup (m68k-buffer-fixups buf))
      (destructuring-bind (word-idx label-id fixup-type ref-pos) fixup
        (let ((target (gethash label-id (m68k-buffer-labels buf))))
          (unless target
            (error "68k: undefined label ~D" label-id))
          (ecase fixup-type
            (:disp16
             ;; 16-bit displacement relative to the extension word position
             ;; The extension word is at ref-pos, displacement is target - (ref-pos - 2)
             ;; Because on 68k, PC for displacement calculation points to the extension word
             (let ((disp (- target (- ref-pos 2))))
               (unless (<= -32768 disp 32767)
                 (error "68k :disp16 ~D out of range at word-idx ~D"
                        disp word-idx))
               (setf (aref words word-idx) (logand disp #xFFFF))))
            (:disp8-in-opcode
             ;; 8-bit displacement embedded in opcode word (Bcc.S)
             ;; PC points to opcode + 2 at execution time
             (let ((disp (- target ref-pos)))
               (unless (<= -128 disp 127)
                 (error "68k :disp8 ~D out of range at word-idx ~D"
                        disp word-idx))
               (setf (aref words word-idx)
                     (logior (logand (aref words word-idx) #xFF00)
                             (logand disp #xFF)))))
            (:disp32
             ;; 32-bit displacement (two words) — full 32-bit signed range,
             ;; no realistic overflow but the assert keeps the invariant.
             (let ((disp (- target (- ref-pos 2))))
               (unless (<= -2147483648 disp 2147483647)
                 (error "68k :disp32 ~D out of range at word-idx ~D"
                        disp word-idx))
               (setf (aref words word-idx)
                     (logand (ash disp -16) #xFFFF))
               (setf (aref words (1+ word-idx))
                     (logand disp #xFFFF))))))))))

(defun m68k-buffer-to-bytes (buf)
  "Convert the 68k word buffer to a big-endian byte vector."
  (let* ((nwords (m68k-buffer-word-count buf))
         (bytes (make-array (* nwords 2))))
    (dotimes (i nwords bytes)
      (let ((w (aref (m68k-buffer-words buf) i)))
        ;; Big-endian: MSB first
        (setf (aref bytes (* i 2))       (logand (ash w -8) #xFF)
              (aref bytes (+ (* i 2) 1)) (logand w #xFF))))))

;;; ============================================================
;;; 68k Effective Address Encoding
;;; ============================================================
;;;
;;; An EA is encoded as mode:3 + register:3 in instruction words.
;;; Mode 0: Dn            (data register direct)
;;; Mode 1: An            (address register direct)
;;; Mode 2: (An)          (address register indirect)
;;; Mode 3: (An)+         (postincrement)
;;; Mode 4: -(An)         (predecrement)
;;; Mode 5: d16(An)       (displacement)
;;; Mode 6: d8(An,Xi)     (indexed)
;;; Mode 7, reg 0: abs.W  (absolute short)
;;; Mode 7, reg 1: abs.L  (absolute long)
;;; Mode 7, reg 4: #imm   (immediate)

(defconstant +ea-mode-dn+     0)
(defconstant +ea-mode-an+     1)
(defconstant +ea-mode-an-ind+ 2)
(defconstant +ea-mode-an-postinc+ 3)
(defconstant +ea-mode-an-predec+  4)
(defconstant +ea-mode-an-disp+    5)
(defconstant +ea-mode-special+    7)

(defun m68k-ea-dn (dn)
  "EA encoding for data register direct: Dn"
  (values +ea-mode-dn+ dn))

(defun m68k-ea-an (an)
  "EA encoding for address register direct: An"
  (values +ea-mode-an+ an))

(defun m68k-ea-an-ind (an)
  "EA encoding for address register indirect: (An)"
  (values +ea-mode-an-ind+ an))

(defun m68k-ea-an-disp (an disp)
  "EA encoding for d16(An). Returns mode, reg and the disp word to emit."
  (declare (ignore disp))
  (values +ea-mode-an-disp+ an))

(defun m68k-ea-imm ()
  "EA encoding for immediate mode: #imm"
  (values +ea-mode-special+ 4))

;;; ============================================================
;;; 68k Instruction Size Codes
;;; ============================================================

(defconstant +68k-size-byte+ 0)  ; .B
(defconstant +68k-size-word+ 1)  ; .W (16-bit)
(defconstant +68k-size-long+ 2)  ; .L (32-bit)

;;; ============================================================
;;; 68k Instruction Emitters
;;; ============================================================

;;; --- MOVE.L ---
;;; Format: 0010 dst-reg dst-mode src-mode src-reg
;;; Note: In MOVE, the destination EA field is reg:mode (reversed from source!)

(defun m68k-encode-move-word (size dst-mode dst-reg src-mode src-reg)
  "Encode the first word of a MOVE instruction.
   SIZE: 1=byte, 3=word, 2=long"
  (logior (ash size 12)
          (ash (logand dst-reg 7) 9)
          (ash (logand dst-mode 7) 6)
          (ash (logand src-mode 7) 3)
          (logand src-reg 7)))

(defun m68k-emit-move-dn-dn (buf src dst)
  "MOVE.L Ds, Dd"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dst +ea-mode-dn+ src)))

(defun m68k-emit-move-an-dn (buf an dn)
  "MOVE.L An, Dd (address to data)"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dn +ea-mode-an+ an)))

(defun m68k-emit-move-dn-an (buf dn an)
  "MOVEA.L Ds, Ad (data to address, uses MOVEA)"
  ;; MOVEA.L: 0010 An 001 src-mode src-reg
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an+ an +ea-mode-dn+ dn)))

(defun m68k-emit-move-an-ind-dn (buf an dn)
  "MOVE.L (An), Dd"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dn +ea-mode-an-ind+ an)))

(defun m68k-emit-move-dn-an-ind (buf dn an)
  "MOVE.L Dd, (An)"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-ind+ an +ea-mode-dn+ dn)))

(defun m68k-emit-move-disp-dn (buf an disp dn)
  "MOVE.L d16(An), Dd"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dn +ea-mode-an-disp+ an))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-move-dn-disp (buf dn an disp)
  "MOVE.L Dd, d16(An)"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-disp+ an +ea-mode-dn+ dn))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-move-disp-an (buf src-an disp dst-an)
  "MOVEA.L d16(An), Ad"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an+ dst-an +ea-mode-an-disp+ src-an))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-move-an-disp (buf src-an dst-an disp)
  "MOVE.L As, d16(Ad) -- store address reg to memory"
  ;; Need to use An direct as source: mode=1
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-disp+ dst-an +ea-mode-an+ src-an))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-moveq (buf dn imm8)
  "MOVEQ #imm8, Dn - move quick (sign-extended byte to long)"
  ;; Format: 0111 Dn 0 data(8)
  (m68k-emit-word buf (logior (ash #b0111 12)
                              (ash (logand dn 7) 9)
                              (ash 0 8)
                              (logand imm8 #xFF))))

(defun m68k-emit-move-imm-dn (buf imm32 dn)
  "MOVE.L #imm32, Dn"
  ;; Source: immediate (mode 7, reg 4)
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dn +ea-mode-special+ 4))
  (m68k-emit-long buf imm32))

(defun m68k-emit-move-imm-an (buf imm32 an)
  "MOVEA.L #imm32, An"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an+ an +ea-mode-special+ 4))
  (m68k-emit-long buf imm32))

(defun m68k-emit-move-an-an (buf src-an dst-an)
  "MOVEA.L As, Ad"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an+ dst-an +ea-mode-an+ src-an)))

;;; --- MOVE with 8-bit and 16-bit sizes ---

(defun m68k-emit-move-byte-an-ind-dn (buf an dn)
  "MOVE.B (An), Dd"
  (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn +ea-mode-an-ind+ an)))

(defun m68k-emit-move-byte-dn-an-ind (buf dn an)
  "MOVE.B Dd, (An)"
  (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-an-ind+ an +ea-mode-dn+ dn)))

(defun m68k-emit-move-word-an-ind-dn (buf an dn)
  "MOVE.W (An), Dd"
  (m68k-emit-word buf (m68k-encode-move-word 3 +ea-mode-dn+ dn +ea-mode-an-ind+ an)))

(defun m68k-emit-move-word-dn-an-ind (buf dn an)
  "MOVE.W Dd, (An)"
  (m68k-emit-word buf (m68k-encode-move-word 3 +ea-mode-an-ind+ an +ea-mode-dn+ dn)))

;;; --- LEA ---

(defun m68k-emit-lea-disp (buf an disp dst-an)
  "LEA d16(An), Ad"
  ;; Format: 0100 Ad 111 mode reg
  ;; Source EA: d16(An) = mode 5, reg An
  (m68k-emit-word buf (logior (ash #b0100 12)
                              (ash (logand dst-an 7) 9)
                              (ash #b111 6)
                              (ash +ea-mode-an-disp+ 3)
                              (logand an 7)))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-lea-an-ind (buf an dst-an)
  "LEA (An), Ad"
  (m68k-emit-word buf (logior (ash #b0100 12)
                              (ash (logand dst-an 7) 9)
                              (ash #b111 6)
                              (ash +ea-mode-an-ind+ 3)
                              (logand an 7))))

;;; --- Arithmetic ---

(defun m68k-emit-add-dn-dn (buf src dst)
  "ADD.L Ds, Dd (Dd = Dd + Ds)"
  ;; Format: 1101 Dd opm(3) mode reg
  ;; opm=010 for .L Dn (EA) + Dn -> Dn
  (m68k-emit-word buf (logior (ash #b1101 12)
                              (ash (logand dst 7) 9)
                              (ash #b010 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7))))

(defun m68k-emit-addi (buf dn imm32)
  "ADDI.L #imm32, Dn"
  ;; Format: 0000 0110 10 mode reg (opcode word), then imm32
  (m68k-emit-word buf (logior (ash #b00000110 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7)))
  (m68k-emit-long buf imm32))

(defun m68k-emit-addq (buf dn imm3)
  "ADDQ.L #imm3, Dn - add quick (1-8)"
  ;; Format: 0101 data(3) 0 10 mode reg
  (let ((data (if (= imm3 8) 0 imm3)))
    (m68k-emit-word buf (logior (ash #b0101 12)
                                (ash (logand data 7) 9)
                                (ash 0 8)
                                (ash +68k-size-long+ 6)
                                (ash +ea-mode-dn+ 3)
                                (logand dn 7)))))

(defun m68k-emit-addq-an (buf an imm3)
  "ADDQ.L #imm3, An - add quick to address register"
  (let ((data (if (= imm3 8) 0 imm3)))
    (m68k-emit-word buf (logior (ash #b0101 12)
                                (ash (logand data 7) 9)
                                (ash 0 8)
                                (ash +68k-size-long+ 6)
                                (ash +ea-mode-an+ 3)
                                (logand an 7)))))

(defun m68k-emit-sub-dn-dn (buf src dst)
  "SUB.L Ds, Dd (Dd = Dd - Ds)"
  ;; Format: 1001 Dd opm(3) mode reg
  ;; opm=010 for .L (EA) - Dn -> Dn... wait, direction:
  ;; opm = 010: Dn - <ea> -> Dn (subtract source from Dn)
  (m68k-emit-word buf (logior (ash #b1001 12)
                              (ash (logand dst 7) 9)
                              (ash #b010 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7))))

(defun m68k-emit-subi (buf dn imm32)
  "SUBI.L #imm32, Dn"
  (m68k-emit-word buf (logior (ash #b00000100 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7)))
  (m68k-emit-long buf imm32))

(defun m68k-emit-subq (buf dn imm3)
  "SUBQ.L #imm3, Dn"
  (let ((data (if (= imm3 8) 0 imm3)))
    (m68k-emit-word buf (logior (ash #b0101 12)
                                (ash (logand data 7) 9)
                                (ash 1 8)
                                (ash +68k-size-long+ 6)
                                (ash +ea-mode-dn+ 3)
                                (logand dn 7)))))

(defun m68k-emit-subq-an (buf an imm3)
  "SUBQ.L #imm3, An"
  (let ((data (if (= imm3 8) 0 imm3)))
    (m68k-emit-word buf (logior (ash #b0101 12)
                                (ash (logand data 7) 9)
                                (ash 1 8)
                                (ash +68k-size-long+ 6)
                                (ash +ea-mode-an+ 3)
                                (logand an 7)))))

(defun m68k-emit-neg (buf dn)
  "NEG.L Dn"
  ;; Format: 0100 0100 10 mode reg
  (m68k-emit-word buf (logior (ash #b01000100 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7))))

(defun m68k-emit-muls (buf src dst)
  "MULS.L Ds, Dd - signed multiply (32x32->32 low)"
  ;; 68020+ long multiply: 0100 1100 00 mode reg, then extension word
  ;; Extension: 0 Dl 1 0000 0000 0 Dh (for 32-bit result, Dh is unused)
  (m68k-emit-word buf (logior (ash #b01001100 8)
                              (ash #b00 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7)))
  ;; Extension word: 0 Dl:3 1 0000 0000 0 Dh:3
  ;; For 32-bit result: bit 11=0 (32-bit), Dl=dst
  (m68k-emit-word buf (logior (ash (logand dst 7) 12)
                              (ash 1 11)    ; signed
                              0)))          ; 32-bit result

(defun m68k-emit-mulu-64 (buf src dlo dhi)
  "MULU.L Ds, Dhi:Dlo - unsigned multiply (32x32->64)"
  ;; 68020+ long multiply with 64-bit result
  ;; Extension word bit 10=1 for 64-bit result, bit 11=0 for unsigned
  (m68k-emit-word buf (logior (ash #b01001100 8)
                              (ash #b00 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7)))
  (m68k-emit-word buf (logior (ash (logand dlo 7) 12)
                              (ash 1 10)   ; 64-bit result
                              (logand dhi 7))))

(defun m68k-emit-divs (buf src dst)
  "DIVS.L Ds, Dd - signed divide (Dd / Ds -> Dd quotient)"
  ;; 68020+ long divide: 0100 1100 01 mode reg, then extension word
  (m68k-emit-word buf (logior (ash #b01001100 8)
                              (ash #b01 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7)))
  ;; Extension word: 0 Dq:3 1 0000 0000 0 Dr:3
  ;; Dq = quotient destination, Dr = remainder destination
  ;; For simplicity, remainder goes to same register (overwritten)
  (m68k-emit-word buf (logior (ash (logand dst 7) 12)
                              (ash 1 11)              ; signed
                              (logand dst 7))))       ; remainder to same reg

;;; --- Logical ---

(defun m68k-emit-and-dn-dn (buf src dst)
  "AND.L Ds, Dd"
  ;; Format: 1100 Dd opm mode reg; opm=010 for .L <ea>,Dn
  (m68k-emit-word buf (logior (ash #b1100 12)
                              (ash (logand dst 7) 9)
                              (ash #b010 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7))))

(defun m68k-emit-andi (buf dn imm32)
  "ANDI.L #imm32, Dn"
  (m68k-emit-word buf (logior (ash #b00000010 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7)))
  (m68k-emit-long buf imm32))

(defun m68k-emit-or-dn-dn (buf src dst)
  "OR.L Ds, Dd"
  ;; Format: 1000 Dd opm mode reg
  (m68k-emit-word buf (logior (ash #b1000 12)
                              (ash (logand dst 7) 9)
                              (ash #b010 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7))))

(defun m68k-emit-ori (buf dn imm32)
  "ORI.L #imm32, Dn"
  (m68k-emit-word buf (logior (ash #b00000000 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7)))
  (m68k-emit-long buf imm32))

(defun m68k-emit-eor-dn-dn (buf src dst)
  "EOR.L Ds, Dd (Dd = Dd XOR Ds)"
  ;; Format: 1011 Ds opm mode reg; opm=110 for .L Dn,<ea>
  (m68k-emit-word buf (logior (ash #b1011 12)
                              (ash (logand src 7) 9)
                              (ash #b110 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dst 7))))

(defun m68k-emit-eori (buf dn imm32)
  "EORI.L #imm32, Dn"
  (m68k-emit-word buf (logior (ash #b00001010 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7)))
  (m68k-emit-long buf imm32))

;;; --- Shifts ---

(defun m68k-emit-lsl-imm (buf dn count)
  "LSL.L #count, Dn (logical shift left)"
  ;; Format: 1110 count(3) 1 10 i/r=0 01 reg
  ;; For immediate count: i/r=0, count 1-8 (0 means 8)
  (let ((cnt (if (= count 8) 0 count)))
    (m68k-emit-word buf (logior (ash #b1110 12)
                                (ash (logand cnt 7) 9)
                                (ash 1 8)      ; direction=left
                                (ash +68k-size-long+ 6)
                                (ash 0 5)      ; i/r=0 (immediate)
                                (ash #b01 3)   ; type=LSL
                                (logand dn 7)))))

(defun m68k-emit-lsr-imm (buf dn count)
  "LSR.L #count, Dn (logical shift right)"
  (let ((cnt (if (= count 8) 0 count)))
    (m68k-emit-word buf (logior (ash #b1110 12)
                                (ash (logand cnt 7) 9)
                                (ash 0 8)      ; direction=right
                                (ash +68k-size-long+ 6)
                                (ash 0 5)      ; i/r=0 (immediate)
                                (ash #b01 3)   ; type=LSR
                                (logand dn 7)))))

(defun m68k-emit-asr-imm (buf dn count)
  "ASR.L #count, Dn (arithmetic shift right)"
  (let ((cnt (if (= count 8) 0 count)))
    (m68k-emit-word buf (logior (ash #b1110 12)
                                (ash (logand cnt 7) 9)
                                (ash 0 8)      ; direction=right
                                (ash +68k-size-long+ 6)
                                (ash 0 5)      ; i/r=0
                                (ash #b00 3)   ; type=ASR
                                (logand dn 7)))))

(defun m68k-emit-lsl-dn (buf count-dn dst-dn)
  "LSL.L Dc, Dd (shift left by register)"
  (m68k-emit-word buf (logior (ash #b1110 12)
                              (ash (logand count-dn 7) 9)
                              (ash 1 8)      ; left
                              (ash +68k-size-long+ 6)
                              (ash 1 5)      ; i/r=1 (register)
                              (ash #b01 3)
                              (logand dst-dn 7))))

(defun m68k-emit-lsr-dn (buf count-dn dst-dn)
  "LSR.L Dc, Dd (shift right by register)"
  (m68k-emit-word buf (logior (ash #b1110 12)
                              (ash (logand count-dn 7) 9)
                              (ash 0 8)      ; right
                              (ash +68k-size-long+ 6)
                              (ash 1 5)      ; register
                              (ash #b01 3)
                              (logand dst-dn 7))))

(defun m68k-emit-asr-dn (buf count-dn dst-dn)
  "ASR.L Dc, Dd (arithmetic shift right by register)"
  (m68k-emit-word buf (logior (ash #b1110 12)
                              (ash (logand count-dn 7) 9)
                              (ash 0 8)
                              (ash +68k-size-long+ 6)
                              (ash 1 5)
                              (ash #b00 3)
                              (logand dst-dn 7))))

;;; --- Compare ---

(defun m68k-emit-cmp-dn-dn (buf src dst)
  "CMP.L Ds, Dd (sets CCR = Dd - Ds)"
  ;; Format: 1011 Dd opm mode reg; opm=010 for .L
  (m68k-emit-word buf (logior (ash #b1011 12)
                              (ash (logand dst 7) 9)
                              (ash #b010 6)
                              (ash +ea-mode-dn+ 3)
                              (logand src 7))))

(defun m68k-emit-cmpi (buf dn imm32)
  "CMPI.L #imm32, Dn"
  (m68k-emit-word buf (logior (ash #b00001100 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7)))
  (m68k-emit-long buf imm32))

(defun m68k-emit-cmpa (buf an dn)
  "CMPA.L Dn, An (compare data reg with address reg)"
  ;; Format: 1011 An opm(111 for .L) mode reg
  (m68k-emit-word buf (logior (ash #b1011 12)
                              (ash (logand an 7) 9)
                              (ash #b111 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7))))

(defun m68k-emit-cmpa-an (buf src-an dst-an)
  "CMPA.L As, Ad (compare two address regs)"
  (m68k-emit-word buf (logior (ash #b1011 12)
                              (ash (logand dst-an 7) 9)
                              (ash #b111 6)
                              (ash +ea-mode-an+ 3)
                              (logand src-an 7))))

(defun m68k-emit-tst (buf dn)
  "TST.L Dn (compare with zero)"
  ;; Format: 0100 1010 10 mode reg
  (m68k-emit-word buf (logior (ash #b01001010 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-dn+ 3)
                              (logand dn 7))))

;;; --- Branch ---

(defun m68k-emit-bra (buf &optional label-id)
  "BRA.W target (unconditional branch, 16-bit displacement)"
  ;; Format: 0110 0000 00000000 disp16
  (m68k-emit-word buf #x6000)    ; BRA with 0 byte displacement = use word extension
  (if label-id
      (progn
        (m68k-emit-word buf 0)   ; placeholder (must emit BEFORE fixup)
        (m68k-emit-fixup buf label-id :disp16))
      (m68k-emit-word buf 0)))

(defun m68k-emit-bcc-word (buf cc &optional label-id)
  "Bcc.W target - conditional branch with 16-bit displacement.
   CC is the 4-bit condition code."
  ;; Format: 0110 cc(4) 00000000 disp16
  (m68k-emit-word buf (logior (ash #b0110 12)
                              (ash (logand cc #xF) 8)
                              0))  ; 0 byte disp = use extension word
  (if label-id
      (progn
        (m68k-emit-word buf 0)   ; placeholder (must emit BEFORE fixup)
        (m68k-emit-fixup buf label-id :disp16))
      (m68k-emit-word buf 0)))

;; 68k condition codes
(defconstant +68k-cc-t+   0)  ; True (always)
(defconstant +68k-cc-f+   1)  ; False (never)
(defconstant +68k-cc-hi+  2)  ; Higher (unsigned >)
(defconstant +68k-cc-ls+  3)  ; Lower or Same (unsigned <=)
(defconstant +68k-cc-cc+  4)  ; Carry Clear (unsigned >=)
(defconstant +68k-cc-cs+  5)  ; Carry Set (unsigned <)
(defconstant +68k-cc-ne+  6)  ; Not Equal
(defconstant +68k-cc-eq+  7)  ; Equal
(defconstant +68k-cc-vc+  8)  ; Overflow Clear
(defconstant +68k-cc-vs+  9)  ; Overflow Set
(defconstant +68k-cc-pl+ 10)  ; Plus (positive)
(defconstant +68k-cc-mi+ 11)  ; Minus (negative)
(defconstant +68k-cc-ge+ 12)  ; Greater or Equal (signed)
(defconstant +68k-cc-lt+ 13)  ; Less Than (signed)
(defconstant +68k-cc-gt+ 14)  ; Greater Than (signed)
(defconstant +68k-cc-le+ 15)  ; Less or Equal (signed)

(defun m68k-emit-beq (buf &optional label-id)
  (m68k-emit-bcc-word buf +68k-cc-eq+ label-id))

(defun m68k-emit-bne (buf &optional label-id)
  (m68k-emit-bcc-word buf +68k-cc-ne+ label-id))

(defun m68k-emit-blt (buf &optional label-id)
  (m68k-emit-bcc-word buf +68k-cc-lt+ label-id))

(defun m68k-emit-bge (buf &optional label-id)
  (m68k-emit-bcc-word buf +68k-cc-ge+ label-id))

(defun m68k-emit-bgt (buf &optional label-id)
  (m68k-emit-bcc-word buf +68k-cc-gt+ label-id))

(defun m68k-emit-ble (buf &optional label-id)
  (m68k-emit-bcc-word buf +68k-cc-le+ label-id))

;;; --- JSR / RTS / JMP ---

(defun m68k-emit-jsr-an-ind (buf an)
  "JSR (An) - jump to subroutine via address register indirect"
  ;; Format: 0100 1110 10 mode reg
  (m68k-emit-word buf (logior (ash #b01001110 8)
                              (ash #b10 6)
                              (ash +ea-mode-an-ind+ 3)
                              (logand an 7))))

(defun m68k-emit-bsr (buf &optional label-id)
  "BSR.W target (branch to subroutine, 16-bit displacement)"
  ;; Format: 0110 0001 00000000 disp16
  (m68k-emit-word buf #x6100)    ; BSR with 0 byte displacement = use word extension
  (if label-id
      (progn
        (m68k-emit-word buf 0)   ; placeholder (must emit BEFORE fixup)
        (m68k-emit-fixup buf label-id :disp16))
      (m68k-emit-word buf 0)))

(defun m68k-emit-rts (buf)
  "RTS - return from subroutine"
  (m68k-emit-word buf #x4E75))

(defun m68k-emit-jmp-an-ind (buf an)
  "JMP (An) - jump via address register indirect"
  ;; Format: 0100 1110 11 mode reg
  (m68k-emit-word buf (logior (ash #b01001110 8)
                              (ash #b11 6)
                              (ash +ea-mode-an-ind+ 3)
                              (logand an 7))))

;;; --- Stack Operations ---

(defun m68k-emit-push-dn (buf dn)
  "MOVE.L Dn, -(SP) - push data register"
  ;; Destination: -(A7) = mode 4, reg 7
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-predec+ +68k-a7+
                                             +ea-mode-dn+ dn)))

(defun m68k-emit-pop-dn (buf dn)
  "MOVE.L (SP)+, Dn - pop to data register"
  ;; Source: (A7)+ = mode 3, reg 7
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dn
                                             +ea-mode-an-postinc+ +68k-a7+)))

(defun m68k-emit-push-an (buf an)
  "MOVE.L An, -(SP)"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-predec+ +68k-a7+
                                             +ea-mode-an+ an)))

(defun m68k-emit-pop-an (buf an)
  "MOVEA.L (SP)+, An"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an+ an
                                             +ea-mode-an-postinc+ +68k-a7+)))

(defun m68k-emit-pea-an-ind (buf an)
  "PEA (An) - push effective address"
  ;; Format: 0100 1000 01 mode reg
  (m68k-emit-word buf (logior (ash #b01001000 8)
                              (ash #b01 6)
                              (ash +ea-mode-an-ind+ 3)
                              (logand an 7))))

;;; --- LINK / UNLK ---

(defun m68k-emit-link (buf an disp)
  "LINK An, #disp - create stack frame"
  ;; Format: 0100 1110 0101 0 An, then d16
  (m68k-emit-word buf (logior (ash #b0100111001010 3)
                              (logand an 7)))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-unlk (buf an)
  "UNLK An - tear down stack frame"
  ;; Format: 0100 1110 0101 1 An
  (m68k-emit-word buf (logior (ash #b0100111001011 3)
                              (logand an 7))))

;;; --- MOVEM (save/restore multiple) ---

(defun m68k-emit-movem-to-predec (buf register-mask)
  "MOVEM.L registers, -(SP) - save multiple registers.
   REGISTER-MASK: bit 0=D0, ..., bit 7=D7, bit 8=A0, ..., bit 15=A7.
   Note: For predecrement mode, bits are reversed! bit 0=A7, bit 15=D0."
  ;; Format: 0100 1000 10 mode reg, then register mask word
  ;; -(A7): mode 4, reg 7
  (m68k-emit-word buf (logior (ash #b01001000 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-an-predec+ 3)
                              +68k-a7+))
  (m68k-emit-word buf register-mask))

(defun m68k-emit-movem-from-postinc (buf register-mask)
  "MOVEM.L (SP)+, registers - restore multiple registers.
   REGISTER-MASK: bit 0=D0, ..., bit 7=D7, bit 8=A0, ..., bit 15=A7."
  ;; Format: 0100 1100 10 mode reg, then mask
  ;; (A7)+: mode 3, reg 7
  (m68k-emit-word buf (logior (ash #b01001100 8)
                              (ash +68k-size-long+ 6)
                              (ash +ea-mode-an-postinc+ 3)
                              +68k-a7+))
  (m68k-emit-word buf register-mask))

;;; --- System ---

(defun m68k-emit-nop (buf)
  "NOP"
  (m68k-emit-word buf #x4E71))

(defun m68k-emit-trap (buf vector)
  "TRAP #vector (0-15)"
  ;; Format: 0100 1110 0100 vector(4)
  (m68k-emit-word buf (logior (ash #b010011100100 4)
                              (logand vector #xF))))

(defun m68k-emit-illegal (buf)
  "ILLEGAL - cause illegal instruction trap"
  (m68k-emit-word buf #x4AFC))

(defun m68k-emit-stop (buf sr)
  "STOP #sr - stop processor and load status register"
  (m68k-emit-word buf #x4E72)
  (m68k-emit-word buf (logand sr #xFFFF)))

(defun m68k-emit-swap (buf dn)
  "SWAP Dn - swap upper and lower words"
  ;; Format: 0100 1000 0100 0 Dn
  (m68k-emit-word buf (logior (ash #b0100100001000 3)
                              (logand dn 7))))

;;; --- TAS (atomic test-and-set) ---

(defun m68k-emit-tas-an-ind (buf an)
  "TAS (An) - test and set byte (atomic on 68k)"
  ;; Format: 0100 1010 11 mode reg
  (m68k-emit-word buf (logior (ash #b01001010 8)
                              (ash #b11 6)
                              (ash +ea-mode-an-ind+ 3)
                              (logand an 7))))

;;; ============================================================
;;; Spill Slot Management
;;; ============================================================

(defconstant +68k-spill-base-offset+ -32
  "Offset from A6 (FP) where spill slots begin.
   First slots are for saving callee-saved regs.")

(defun m68k-spill-offset (vreg)
  "Calculate the A6-relative offset for a spilled virtual register."
  (cond
    ((and (>= vreg 8) (<= vreg 15))
     (+ +68k-spill-base-offset+ (* (- vreg 8) -4)))
    (t (error "68k: unexpected spill for vreg ~D" vreg))))

(defun m68k-vreg-phys (vreg)
  "Return the physical register descriptor for vreg, or NIL if spilled."
  (and (< vreg (length *68k-vreg-map*))
       (aref *68k-vreg-map* vreg)))

(defun m68k-load-vreg (buf scratch-dn vreg)
  "Load virtual register VREG into data register SCRATCH-DN.
   If VREG has a physical data register, emit MOVE.L. If spilled, load from frame."
  (let ((phys (m68k-vreg-phys vreg)))
    (cond
      ;; In a data register
      ((and phys (eq (m68k-reg-type phys) :data))
       (unless (= (m68k-reg-number phys) scratch-dn)
         (m68k-emit-move-dn-dn buf (m68k-reg-number phys) scratch-dn)))
      ;; In an address register -- need MOVE.L An, Dn
      ((and phys (eq (m68k-reg-type phys) :address))
       (m68k-emit-move-an-dn buf (m68k-reg-number phys) scratch-dn))
      ;; Spilled
      (t
       (m68k-emit-move-disp-dn buf +68k-a6+ (m68k-spill-offset vreg) scratch-dn)))))

(defun m68k-store-vreg (buf vreg scratch-dn)
  "Store data register SCRATCH-DN into virtual register VREG."
  (let ((phys (m68k-vreg-phys vreg)))
    (cond
      ((and phys (eq (m68k-reg-type phys) :data))
       (unless (= (m68k-reg-number phys) scratch-dn)
         (m68k-emit-move-dn-dn buf scratch-dn (m68k-reg-number phys))))
      ((and phys (eq (m68k-reg-type phys) :address))
       (m68k-emit-move-dn-an buf scratch-dn (m68k-reg-number phys)))
      (t
       (m68k-emit-move-dn-disp buf scratch-dn +68k-a6+ (m68k-spill-offset vreg))))))

(defun m68k-load-vreg-to-an (buf an vreg)
  "Load virtual register VREG into address register AN."
  (let ((phys (m68k-vreg-phys vreg)))
    (cond
      ((and phys (eq (m68k-reg-type phys) :address))
       (unless (= (m68k-reg-number phys) an)
         (m68k-emit-move-an-an buf (m68k-reg-number phys) an)))
      ((and phys (eq (m68k-reg-type phys) :data))
       (m68k-emit-move-dn-an buf (m68k-reg-number phys) an))
      (t
       (m68k-emit-move-disp-an buf +68k-a6+ (m68k-spill-offset vreg) an)))))

(defun m68k-vreg-data-reg (vreg)
  "If VREG is mapped to a data register, return its number. Otherwise NIL."
  (let ((phys (m68k-vreg-phys vreg)))
    (when (and phys (eq (m68k-reg-type phys) :data))
      (m68k-reg-number phys))))

(defun m68k-vreg-or-scratch (buf vreg scratch-dn)
  "If VREG maps to a data register, return it. Otherwise load into scratch."
  (let ((dn (m68k-vreg-data-reg vreg)))
    (if dn
        dn
        (progn
          (m68k-load-vreg buf scratch-dn vreg)
          scratch-dn))))

;;; ============================================================
;;; Prologue / Epilogue
;;; ============================================================

(defconstant +68k-frame-slot-base+ -64
  "A6-relative offset for frame slot 0 (local variables via obj-ref VFP).
   Frame slots grow downward: slot N is at A6 + frame-slot-base - N*4.
   This is below all spill slots (which end at A6-60) to avoid overlap.")

(defconstant +68k-frame-size+ 96
  "Stack frame size: 60 bytes spill area + 32 bytes frame slots = 92,
   rounded to 96 for alignment.")

(defun m68k-emit-prologue (buf)
  "Emit 68k function prologue. LINK + save callee-saved registers."
  ;; LINK A6, #-frame-size
  (m68k-emit-link buf +68k-a6+ (logand (- +68k-frame-size+) #xFFFF))
  ;; Save callee-saved data registers D4-D7 using MOVEM
  ;; Predecrement mask is reversed: bit 15=D0, bit 14=D1, ...
  ;; D4=bit 11, D5=bit 10, D6=bit 9, D7=bit 8
  ;; Also save A2-A4 (VA, VL, VN): A2=bit 5, A3=bit 4, A4=bit 3
  (let ((mask (logior (ash 1 11)   ; D4
                      (ash 1 10)   ; D5
                      (ash 1 9)    ; D6
                      (ash 1 8)    ; D7
                      (ash 1 5)    ; A2
                      (ash 1 4)    ; A3
                      (ash 1 3)))) ; A4
    (m68k-emit-movem-to-predec buf mask)))

(defun m68k-emit-epilogue (buf)
  "Emit 68k function epilogue. Restore registers, UNLK, RTS."
  ;; Restore callee-saved registers with MOVEM (postincrement)
  ;; Normal mask: bit 0=D0, ..., bit 7=D7, bit 8=A0, ...
  ;; D4=bit 4, D5=bit 5, D6=bit 6, D7=bit 7
  ;; A2=bit 10, A3=bit 11, A4=bit 12
  (let ((mask (logior (ash 1 4)    ; D4
                      (ash 1 5)    ; D5
                      (ash 1 6)    ; D6
                      (ash 1 7)    ; D7
                      (ash 1 10)   ; A2
                      (ash 1 11)   ; A3
                      (ash 1 12)))) ; A4
    (m68k-emit-movem-from-postinc buf mask))
  ;; UNLK A6
  (m68k-emit-unlk buf +68k-a6+)
  ;; RTS
  (m68k-emit-rts buf))

;;; ============================================================
;;; MVM Opcode Translation
;;; ============================================================

(defun m68k-translate-insn (buf opcode operands mvm-pc label-map function-table)
  "Translate a single MVM instruction to 68k native code."
  (declare (ignorable mvm-pc function-table))

  ;; Labels are emitted in the main loop at the correct PC position.
  ;; mvm-pc here is new-pc (position after this instruction), used for
  ;; branch offset computation.

  (flet ((ensure-label (target-pc)
           (or (gethash target-pc label-map)
               (let ((l (mvm-make-label)))
                 (setf (gethash target-pc label-map) l)
                 l))))

    (case opcode
      ;;; --- NOP / BREAK / TRAP ---
      (#.+op-nop+
       (m68k-emit-nop buf))

      (#.+op-break+
       (m68k-emit-illegal buf))

      (#.+op-trap+
       (let ((code (first operands)))
         (cond
           ((< code #x0100)
            ;; Frame-enter: emit function prologue
            (m68k-emit-prologue buf))
           ((< code #x0300)
            ;; Frame-alloc/frame-free: NOP for now
            nil)
           ((= code #x0300)
            ;; Serial write: V0 (D0) contains tagged fixnum char code
            ;; move.l d0, d1 (copy V0 to scratch)
            (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
            ;; asr.l #1, d1 (untag fixnum)
            (m68k-emit-asr-imm buf +68k-d1+ 1)
            ;; movea.l #uart_base, a0
            (m68k-emit-move-imm-an buf +m68k-uart-base+ +68k-a0+)
            ;; move.l d1, (a0) (write to UART — Goldfish TTY needs 32-bit write)
            (m68k-emit-move-dn-an-ind buf +68k-d1+ +68k-a0+))
           (t
            ;; Real CPU trap
            (if (<= code 15)
                (m68k-emit-trap buf code)
                (progn
                  (m68k-emit-move-imm-dn buf code +68k-d0+)
                  (m68k-emit-push-dn buf +68k-d0+)
                  (m68k-emit-trap buf 0)))))))

      ;;; --- Data Movement ---
      (#.+op-mov+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((pd (m68k-vreg-phys vd))
               (ps (m68k-vreg-phys vs)))
           (cond
             ;; Both in data registers
             ((and pd ps
                   (eq (m68k-reg-type pd) :data)
                   (eq (m68k-reg-type ps) :data))
              (unless (= (m68k-reg-number pd) (m68k-reg-number ps))
                (m68k-emit-move-dn-dn buf (m68k-reg-number ps) (m68k-reg-number pd))))
             ;; Source in data reg, dest in address reg
             ((and pd ps
                   (eq (m68k-reg-type pd) :address)
                   (eq (m68k-reg-type ps) :data))
              (m68k-emit-move-dn-an buf (m68k-reg-number ps) (m68k-reg-number pd)))
             ;; Source in address reg, dest in data reg
             ((and pd ps
                   (eq (m68k-reg-type pd) :data)
                   (eq (m68k-reg-type ps) :address))
              (m68k-emit-move-an-dn buf (m68k-reg-number ps) (m68k-reg-number pd)))
             ;; Both address regs
             ((and pd ps
                   (eq (m68k-reg-type pd) :address)
                   (eq (m68k-reg-type ps) :address))
              (unless (= (m68k-reg-number pd) (m68k-reg-number ps))
                (m68k-emit-move-an-an buf (m68k-reg-number ps) (m68k-reg-number pd))))
             ;; Source spills, dest in data register
             ((and pd (not ps) (eq (m68k-reg-type pd) :data))
              (m68k-emit-move-disp-dn buf +68k-a6+ (m68k-spill-offset vs)
                                      (m68k-reg-number pd)))
             ;; Source in data reg, dest spills
             ((and (not pd) ps (eq (m68k-reg-type ps) :data))
              (m68k-emit-move-dn-disp buf (m68k-reg-number ps)
                                      +68k-a6+ (m68k-spill-offset vd)))
             ;; Source spills, dest is address register
             ((and pd (not ps) (eq (m68k-reg-type pd) :address))
              (m68k-emit-move-disp-an buf +68k-a6+ (m68k-spill-offset vs)
                                      (m68k-reg-number pd)))
             ;; Both spill: use D0 as scratch (careful if vd=VR=D0!)
             (t
              (m68k-emit-move-disp-dn buf +68k-a6+ (m68k-spill-offset vs) +68k-d0+)
              (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a6+ (m68k-spill-offset vd)))))))

      (#.+op-li+
       (let ((vd (first operands))
             (imm (second operands)))
         ;; 32-bit on 68k
         (let ((imm32 (logand imm #xFFFFFFFF))
               (pd (m68k-vreg-phys vd)))
           (cond
             ;; Data register, small immediate
             ((and pd (eq (m68k-reg-type pd) :data)
                   (<= -128 (if (>= imm32 #x80000000) (- imm32 #x100000000) imm32) 127))
              (m68k-emit-moveq buf (m68k-reg-number pd)
                               (logand (if (>= imm32 #x80000000)
                                           (- imm32 #x100000000)
                                           imm32)
                                       #xFF)))
             ;; Data register, full immediate
             ((and pd (eq (m68k-reg-type pd) :data))
              (m68k-emit-move-imm-dn buf imm32 (m68k-reg-number pd)))
             ;; Address register
             ((and pd (eq (m68k-reg-type pd) :address))
              (m68k-emit-move-imm-an buf imm32 (m68k-reg-number pd)))
             ;; Spilled
             (t
              (m68k-emit-move-imm-dn buf imm32 +68k-d0+)
              (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a6+ (m68k-spill-offset vd)))))))

      (#.+op-push+
       (let ((vs (first operands)))
         (let ((ps (m68k-vreg-phys vs)))
           (cond
             ((and ps (eq (m68k-reg-type ps) :data))
              (m68k-emit-push-dn buf (m68k-reg-number ps)))
             ((and ps (eq (m68k-reg-type ps) :address))
              (m68k-emit-push-an buf (m68k-reg-number ps)))
             (t
              (m68k-load-vreg buf +68k-d0+ vs)
              (m68k-emit-push-dn buf +68k-d0+))))))

      (#.+op-pop+
       (let ((vd (first operands)))
         (let ((pd (m68k-vreg-phys vd)))
           (cond
             ((and pd (eq (m68k-reg-type pd) :data))
              (m68k-emit-pop-dn buf (m68k-reg-number pd)))
             ((and pd (eq (m68k-reg-type pd) :address))
              (m68k-emit-pop-an buf (m68k-reg-number pd)))
             (t
              (m68k-emit-pop-dn buf +68k-d0+)
              (m68k-store-vreg buf vd +68k-d0+))))))

      ;;; --- Arithmetic ---
      (#.+op-add+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((da (m68k-vreg-or-scratch buf va +68k-d0+))
               (db (m68k-vreg-or-scratch buf vb +68k-d1+)))
           ;; We need the result in a scratch. If da!=d0, move it.
           (unless (= da +68k-d0+)
             (m68k-emit-move-dn-dn buf da +68k-d0+))
           (m68k-emit-add-dn-dn buf db +68k-d0+)
           (m68k-store-vreg buf vd +68k-d0+))))

      (#.+op-sub+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((da (m68k-vreg-or-scratch buf va +68k-d0+)))
           (unless (= da +68k-d0+)
             (m68k-emit-move-dn-dn buf da +68k-d0+))
           (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
             (m68k-emit-sub-dn-dn buf db +68k-d0+)
             (m68k-store-vreg buf vd +68k-d0+)))))

      (#.+op-mul+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; Tagged multiply: untag one operand
         (m68k-load-vreg buf +68k-d0+ va)
         (m68k-emit-asr-imm buf +68k-d0+ 1)  ; untag
         (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
           (m68k-emit-muls buf db +68k-d0+)
           (m68k-store-vreg buf vd +68k-d0+))))

      (#.+op-mul26lo+
       ;; Low 26 bits of untag(Va)*untag(Vb), tagged
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (m68k-load-vreg buf +68k-d0+ va)
         (m68k-emit-asr-imm buf +68k-d0+ 1)  ; untag va
         (m68k-load-vreg buf +68k-d1+ vb)
         (m68k-emit-asr-imm buf +68k-d1+ 1)  ; untag vb
         ;; MULS.L d1, d0 (32-bit result is fine for low 26 bits)
         (m68k-emit-muls buf +68k-d1+ +68k-d0+)
         ;; AND #0x3FFFFFF (mask to 26 bits)
         (m68k-emit-andi buf +68k-d0+ #x3FFFFFF)
         ;; Retag: ADD d0, d0
         (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-mul26hi+
       ;; Bits 26+ of untag(Va)*untag(Vb), tagged
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (m68k-load-vreg buf +68k-d0+ va)
         (m68k-emit-asr-imm buf +68k-d0+ 1)  ; untag va
         (m68k-load-vreg buf +68k-d1+ vb)
         (m68k-emit-asr-imm buf +68k-d1+ 1)  ; untag vb
         ;; MULU.L d1, d1:d0 (64-bit unsigned: low→d0, high→d1)
         (m68k-emit-mulu-64 buf +68k-d1+ +68k-d0+ +68k-d1+)
         ;; Result in d1:d0. We want bits 26+.
         ;; LSR d0, #26 (get top 6 bits of low word)
         (m68k-emit-lsr-imm buf +68k-d0+ 26)
         ;; LSL d1, #6 (shift high word left 6)
         (m68k-emit-lsl-imm buf +68k-d1+ 6)
         ;; OR d1, d0 (merge)
         (m68k-emit-or-dn-dn buf +68k-d1+ +68k-d0+)
         ;; Retag: ADD d0, d0
         (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-div+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; Tagged divide, then re-tag
         (m68k-load-vreg buf +68k-d0+ va)
         (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
           (m68k-emit-divs buf db +68k-d0+)
           ;; Re-tag: quotient is untagged, shift left 1 to re-tag
           (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d0+)
           (m68k-store-vreg buf vd +68k-d0+))))

      (#.+op-mod+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; mod = a - (a/b)*b
         (m68k-load-vreg buf +68k-d0+ va)
         (m68k-load-vreg buf +68k-d1+ vb)
         ;; Save original a
         (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)  ; save va in d2
         ;; Divide
         (m68k-emit-divs buf +68k-d1+ +68k-d0+)         ; d0 = va/vb
         ;; Multiply back
         (m68k-emit-muls buf +68k-d1+ +68k-d0+)         ; d0 = (va/vb)*vb
         ;; Subtract from original: d2 - d0
         (m68k-emit-sub-dn-dn buf +68k-d0+ +68k-d2+)    ; d2 = va - (va/vb)*vb
         (m68k-store-vreg buf vd +68k-d2+)))

      (#.+op-neg+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-neg buf +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-inc+
       (let ((vd (first operands)))
         ;; Tagged increment: add 2 (fixnum 1 = 1 << 1)
         (let ((dd (m68k-vreg-data-reg vd)))
           (if dd
               (m68k-emit-addq buf dd 2)
               (progn
                 (m68k-load-vreg buf +68k-d0+ vd)
                 (m68k-emit-addq buf +68k-d0+ 2)
                 (m68k-store-vreg buf vd +68k-d0+))))))

      (#.+op-dec+
       (let ((vd (first operands)))
         (let ((dd (m68k-vreg-data-reg vd)))
           (if dd
               (m68k-emit-subq buf dd 2)
               (progn
                 (m68k-load-vreg buf +68k-d0+ vd)
                 (m68k-emit-subq buf +68k-d0+ 2)
                 (m68k-store-vreg buf vd +68k-d0+))))))

      ;;; --- Bitwise ---
      (#.+op-and+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((da (m68k-vreg-or-scratch buf va +68k-d0+)))
           (unless (= da +68k-d0+)
             (m68k-emit-move-dn-dn buf da +68k-d0+))
           (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
             (m68k-emit-and-dn-dn buf db +68k-d0+)
             (m68k-store-vreg buf vd +68k-d0+)))))

      (#.+op-or+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((da (m68k-vreg-or-scratch buf va +68k-d0+)))
           (unless (= da +68k-d0+)
             (m68k-emit-move-dn-dn buf da +68k-d0+))
           (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
             (m68k-emit-or-dn-dn buf db +68k-d0+)
             (m68k-store-vreg buf vd +68k-d0+)))))

      (#.+op-xor+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((da (m68k-vreg-or-scratch buf va +68k-d0+)))
           (unless (= da +68k-d0+)
             (m68k-emit-move-dn-dn buf da +68k-d0+))
           (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
             (m68k-emit-eor-dn-dn buf db +68k-d0+)
             (m68k-store-vreg buf vd +68k-d0+)))))

      (#.+op-shl+
       (let ((vd (first operands))
             (vs (second operands))
             (amt (third operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         ;; 68k immediate shift max is 8; larger needs register shift
         (if (<= 1 amt 8)
             (m68k-emit-lsl-imm buf +68k-d0+ amt)
             (progn
               (m68k-emit-moveq buf +68k-d1+ amt)
               (m68k-emit-lsl-dn buf +68k-d1+ +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-shr+
       (let ((vd (first operands))
             (vs (second operands))
             (amt (third operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (if (<= 1 amt 8)
             (m68k-emit-lsr-imm buf +68k-d0+ amt)
             (progn
               (m68k-emit-moveq buf +68k-d1+ amt)
               (m68k-emit-lsr-dn buf +68k-d1+ +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-sar+
       (let ((vd (first operands))
             (vs (second operands))
             (amt (third operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (if (<= 1 amt 8)
             (m68k-emit-asr-imm buf +68k-d0+ amt)
             (progn
               (m68k-emit-moveq buf +68k-d1+ amt)
               (m68k-emit-asr-dn buf +68k-d1+ +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-shlv+
       ;; (shlv Vd Vs Vc) — shift left by register
       (let ((vd (first operands))
             (vs (second operands))
             (vc (third operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-load-vreg buf +68k-d1+ vc)
         (m68k-emit-lsl-dn buf +68k-d1+ +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-sarv+
       ;; (sarv Vd Vs Vc) — arithmetic shift right by register
       (let ((vd (first operands))
             (vs (second operands))
             (vc (third operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-load-vreg buf +68k-d1+ vc)
         (m68k-emit-asr-dn buf +68k-d1+ +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-ldb+
       (let ((vd (first operands))
             (vs (second operands))
             (pos (third operands))
             (size (fourth operands)))
         ;; Bit field extract: shift right by pos, mask size bits
         (m68k-load-vreg buf +68k-d0+ vs)
         (when (> pos 0)
           (m68k-emit-moveq buf +68k-d1+ pos)
           (m68k-emit-lsr-dn buf +68k-d1+ +68k-d0+))
         (m68k-emit-andi buf +68k-d0+ (1- (ash 1 size)))
         (m68k-store-vreg buf vd +68k-d0+)))

      ;;; --- Comparison ---
      (#.+op-cmp+
       (let ((va (first operands))
             (vb (second operands)))
         ;; CMP.L Db, Da (sets CCR = Da - Db)
         (let ((da (m68k-vreg-or-scratch buf va +68k-d0+))
               (db (m68k-vreg-or-scratch buf vb +68k-d1+)))
           (m68k-emit-cmp-dn-dn buf db da))))

      (#.+op-test+
       (let ((va (first operands))
             (vb (second operands)))
         ;; AND.L then check zero: compute AND into scratch and TST
         (m68k-load-vreg buf +68k-d0+ va)
         (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
           (m68k-emit-and-dn-dn buf db +68k-d0+)
           (m68k-emit-tst buf +68k-d0+))))

      ;;; --- Branches ---
      (#.+op-br+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (m68k-emit-bra buf label)))

      (#.+op-beq+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (m68k-emit-beq buf label)))

      (#.+op-bne+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (m68k-emit-bne buf label)))

      (#.+op-blt+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (m68k-emit-blt buf label)))

      (#.+op-bge+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (m68k-emit-bge buf label)))

      (#.+op-ble+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (m68k-emit-ble buf label)))

      (#.+op-bgt+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (m68k-emit-bgt buf label)))

      (#.+op-bnull+
       (let ((vs (first operands))
             (off16 (second operands)))
         (let ((target-pc (+ mvm-pc off16)))
           ;; Compare with NIL (A4)
           (let ((ds (m68k-vreg-or-scratch buf vs +68k-d0+)))
             ;; CMPA.L Dn, An  -- but we need CMP with A4's value
             ;; Use CMPA to compare: need value in Dn
             (m68k-emit-cmpa buf +68k-a4+ ds)
             (m68k-emit-beq buf (ensure-label target-pc))))))

      (#.+op-bnnull+
       (let ((vs (first operands))
             (off16 (second operands)))
         (let ((target-pc (+ mvm-pc off16)))
           (let ((ds (m68k-vreg-or-scratch buf vs +68k-d0+)))
             (m68k-emit-cmpa buf +68k-a4+ ds)
             (m68k-emit-bne buf (ensure-label target-pc))))))

      ;;; --- List Operations ---
      (#.+op-car+
       (let ((vd (first operands))
             (vs (second operands)))
         ;; Type check: low 4 bits == +tag-cons+
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
         (m68k-emit-andi buf +68k-d1+ #xF)
         (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
         (let ((ok-label (mvm-make-label)))
           (m68k-emit-beq buf ok-label)
           (m68k-emit-illegal buf)   ; trap on non-cons
           (m68k-emit-label buf ok-label))
         ;; Strip tag, load car via address register
         ;; D0 has the tagged pointer, subtract tag to get raw address
         (m68k-emit-subi buf +68k-d0+ +tag-cons+)
         ;; Move to address register for indirect access
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         ;; MOVE.L (A0), D0  -- load car
         (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-cdr+
       (let ((vd (first operands))
             (vs (second operands)))
         ;; Type check
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
         (m68k-emit-andi buf +68k-d1+ #xF)
         (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
         (let ((ok-label (mvm-make-label)))
           (m68k-emit-beq buf ok-label)
           (m68k-emit-illegal buf)
           (m68k-emit-label buf ok-label))
         ;; Strip tag, load cdr (offset 4 on 32-bit)
         (m68k-emit-subi buf +68k-d0+ +tag-cons+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         ;; MOVE.L 4(A0), D0
         (m68k-emit-move-disp-dn buf +68k-a0+ 4 +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-cons+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; Bump allocator using A2 (VA)
         ;; Store car at (A2)
         (m68k-load-vreg buf +68k-d0+ va)
         (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
         ;; Store cdr at 4(A2)
         (m68k-load-vreg buf +68k-d1+ vb)
         (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a2+ 4)
         ;; Tag pointer: result = A2 | +tag-cons+
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-cons+)
         ;; Bump A2 by 8
         (m68k-emit-addq-an buf +68k-a2+ 8)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-setcar+
       (let ((vd (first operands))
             (vs (second operands)))
         ;; Load cons pointer, strip tag, store new car
         (m68k-load-vreg buf +68k-d0+ vd)
         (m68k-emit-subi buf +68k-d0+ +tag-cons+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-load-vreg buf +68k-d1+ vs)
         (m68k-emit-move-dn-an-ind buf +68k-d1+ +68k-a0+)))

      (#.+op-setcdr+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vd)
         (m68k-emit-subi buf +68k-d0+ +tag-cons+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-load-vreg buf +68k-d1+ vs)
         (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 4)))

      (#.+op-consp+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
         (m68k-emit-andi buf +68k-d1+ #xF)
         (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
         (let ((true-label (mvm-make-label))
               (done-label (mvm-make-label)))
           (m68k-emit-beq buf true-label)
           ;; False: load NIL (from A4)
           (m68k-emit-move-an-dn buf +68k-a4+ +68k-d0+)
           (m68k-emit-bra buf done-label)
           ;; True: load T (tagged fixnum 1 = 2)
           (m68k-emit-label buf true-label)
           (m68k-emit-moveq buf +68k-d0+ +mvm-t+)
           (m68k-emit-label buf done-label))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-atom+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
         (m68k-emit-andi buf +68k-d1+ #xF)
         (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
         (let ((true-label (mvm-make-label))
               (done-label (mvm-make-label)))
           (m68k-emit-bne buf true-label)
           ;; Is cons: return NIL
           (m68k-emit-move-an-dn buf +68k-a4+ +68k-d0+)
           (m68k-emit-bra buf done-label)
           ;; Not cons: return T
           (m68k-emit-label buf true-label)
           (m68k-emit-moveq buf +68k-d0+ +mvm-t+)
           (m68k-emit-label buf done-label))
         (m68k-store-vreg buf vd +68k-d0+)))

      ;;; --- Object Operations ---
      (#.+op-alloc-obj+
       (let ((vd (first operands))
             (size (second operands))
             (subtag (third operands)))
         ;; Build header: (subtag << 8) | +tag-object+
         (let ((header (logior (ash subtag 8) +tag-object+)))
           ;; Store header at (A2)
           (m68k-emit-move-imm-dn buf header +68k-d0+)
           (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
           ;; Tag the pointer
           (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
           (m68k-emit-ori buf +68k-d0+ +tag-object+)
           ;; Bump alloc: (1+size)*4 bytes, aligned to 16 bytes to keep cons alloc pointer aligned
           (let ((total (logand (+ (* (1+ size) 4) 15) (lognot 15))))
             (if (<= total 8)
                 (m68k-emit-addq-an buf +68k-a2+ total)
                 (progn
                   (m68k-emit-move-imm-dn buf total +68k-d1+)
                   (m68k-emit-move-dn-an buf +68k-d1+ +68k-a1+)  ; temp
                   ;; ADDA.L D1, A2
                   (m68k-emit-word buf (logior (ash #b1101 12)
                                               (ash +68k-a2+ 9)
                                               (ash #b111 6)  ; .L ADDA
                                               (ash +ea-mode-dn+ 3)
                                               +68k-d1+)))))
           (m68k-store-vreg buf vd +68k-d0+))))

      (#.+op-obj-ref+
       (let ((vd (first operands))
             (vobj (second operands))
             (idx (third operands)))
         (if (= vobj +vreg-vfp+)
             ;; Frame slot access: use safe A6-relative offset below spill area
             (progn
               (m68k-emit-move-disp-dn buf +68k-a6+
                                        (+ +68k-frame-slot-base+ (* idx -4))
                                        +68k-d0+)
               (m68k-store-vreg buf vd +68k-d0+))
             ;; Normal object slot access
             (progn
               (m68k-load-vreg buf +68k-d0+ vobj)
               ;; Strip tag
               (m68k-emit-subi buf +68k-d0+ +tag-object+)
               ;; Load slot via address register
               (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
               (m68k-emit-move-disp-dn buf +68k-a0+ (* (1+ idx) 4) +68k-d0+)
               (m68k-store-vreg buf vd +68k-d0+)))))

      (#.+op-obj-set+
       (let ((vobj (first operands))
             (idx (second operands))
             (vs (third operands)))
         (if (= vobj +vreg-vfp+)
             ;; Frame slot store: use safe A6-relative offset below spill area
             (progn
               (m68k-load-vreg buf +68k-d0+ vs)
               (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a6+
                                        (+ +68k-frame-slot-base+ (* idx -4))))
             ;; Normal object slot store
             (progn
               (m68k-load-vreg buf +68k-d0+ vobj)
               (m68k-emit-subi buf +68k-d0+ +tag-object+)
               (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
               (m68k-load-vreg buf +68k-d1+ vs)
               (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ (* (1+ idx) 4))))))

      (#.+op-obj-tag+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-andi buf +68k-d0+ #xF)
         ;; Tag as fixnum: shift left 1
         (m68k-emit-lsl-imm buf +68k-d0+ 1)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-obj-subtag+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-subi buf +68k-d0+ +tag-object+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         ;; Load header
         (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
         ;; Extract subtag
         (m68k-emit-lsr-imm buf +68k-d0+ 8)
         (m68k-emit-andi buf +68k-d0+ #xFF)
         ;; Tag as fixnum
         (m68k-emit-lsl-imm buf +68k-d0+ 1)
         (m68k-store-vreg buf vd +68k-d0+)))

      ;;; --- Memory (raw) ---
      (#.+op-load+
       (let ((vd (first operands))
             (vaddr (second operands))
             (width (third operands)))
         ;; Load address into A0
         (m68k-load-vreg-to-an buf +68k-a0+ vaddr)
         (ecase width
           (0 (m68k-emit-move-byte-an-ind-dn buf +68k-a0+ +68k-d0+)
              (m68k-emit-andi buf +68k-d0+ #xFF))        ; zero-extend
           (1 (m68k-emit-move-word-an-ind-dn buf +68k-a0+ +68k-d0+)
              (m68k-emit-andi buf +68k-d0+ #xFFFF))
           (2 (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+))
           (3 (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)))  ; 32-bit max on 68k
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-store+
       (let ((vaddr (first operands))
             (vs (second operands))
             (width (third operands)))
         (m68k-load-vreg-to-an buf +68k-a0+ vaddr)
         (m68k-load-vreg buf +68k-d0+ vs)
         (ecase width
           (0 (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a0+))
           (1 (m68k-emit-move-word-dn-an-ind buf +68k-d0+ +68k-a0+))
           (2 (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+))
           (3 (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)))))

      (#.+op-fence+
       ;; 68k is strongly ordered; NOP suffices
       (m68k-emit-nop buf))

      ;;; --- Function Calling ---
      (#.+op-call+
       ;; Target operand is the bytecode offset of the called function.
       (let* ((target-offset (first operands))
              (label (gethash target-offset label-map)))
         (if label
             (m68k-emit-bsr buf label)
             ;; Unknown target: emit BSR with no fixup
             (m68k-emit-bsr buf nil))))

      (#.+op-call-ind+
       (let ((vs (first operands)))
         ;; Load target address into A0, then JSR (A0)
         (m68k-load-vreg-to-an buf +68k-a0+ vs)
         (m68k-emit-jsr-an-ind buf +68k-a0+)))

      (#.+op-ret+
       (m68k-emit-epilogue buf))

      (#.+op-tailcall+
       ;; Target operand is the bytecode offset of the called function.
       (let* ((target-offset (first operands))
              (label (gethash target-offset label-map)))
         ;; Tear down frame, then jump
         ;; Restore callee-saved regs
         (let ((mask (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)
                             (ash 1 10) (ash 1 11) (ash 1 12))))
           (m68k-emit-movem-from-postinc buf mask))
         (m68k-emit-unlk buf +68k-a6+)
         ;; Branch to target (BRA, not BSR — this is a tail call)
         (m68k-emit-bra buf label)))

      ;;; --- GC / Allocation ---
      (#.+op-alloc-cons+
       (let ((vd (first operands)))
         ;; Tag A2 (VA) as cons pointer
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-cons+)
         ;; Bump A2 by 8
         (m68k-emit-addq-an buf +68k-a2+ 8)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-gc-check+
       ;; Compare A2 (VA) against A3 (VL)
       (m68k-emit-cmpa-an buf +68k-a2+ +68k-a3+)
       (let ((ok-label (mvm-make-label)))
         ;; If A2 < A3 (unsigned), still room
         (m68k-emit-bcc-word buf +68k-cc-cs+ ok-label)  ; CS = carry set = A3 > A2
         ;; GC needed: trap
         (m68k-emit-illegal buf)
         (m68k-emit-label buf ok-label)))

      (#.+op-write-barrier+
       (let ((vobj (first operands)))
         ;; Card table write barrier stub
         (m68k-load-vreg buf +68k-d0+ vobj)
         (m68k-emit-lsr-imm buf +68k-d0+ 8)  ; approximate page shift for card index
         (m68k-emit-lsr-imm buf +68k-d0+ 4)
         (m68k-emit-nop buf)))

      ;;; --- Actor/Concurrency ---
      (#.+op-save-ctx+
       ;; Save D0-D3 (argument registers) to stack
       ;; Using MOVEM for efficiency: mask D0-D3 in predecrement format
       ;; Predecrement: bit 15=D0, bit 14=D1, bit 13=D2, bit 12=D3
       (let ((mask (logior (ash 1 15) (ash 1 14) (ash 1 13) (ash 1 12))))
         (m68k-emit-movem-to-predec buf mask)))

      (#.+op-restore-ctx+
       ;; Restore D0-D3 from stack
       ;; Postincrement: bit 0=D0, bit 1=D1, bit 2=D2, bit 3=D3
       (let ((mask (logior (ash 1 0) (ash 1 1) (ash 1 2) (ash 1 3))))
         (m68k-emit-movem-from-postinc buf mask)))

      (#.+op-yield+
       ;; Preemption check: NOP stub
       (m68k-emit-nop buf)
       (m68k-emit-nop buf))

      (#.+op-atomic-xchg+
       (let ((vd (first operands))
             (vaddr (second operands))
             (vs (third operands)))
         ;; 68k TAS instruction does test-and-set on a byte.
         ;; For a full word exchange, we use a TAS-based spinlock approach
         ;; or just do non-atomic exchange (68000 is single-core anyway).
         ;; Simple non-atomic exchange:
         (m68k-load-vreg-to-an buf +68k-a0+ vaddr)
         (m68k-load-vreg buf +68k-d1+ vs)
         ;; Load old value
         (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
         ;; Store new value
         (m68k-emit-move-dn-an-ind buf +68k-d1+ +68k-a0+)
         ;; Old value in D0
         (m68k-store-vreg buf vd +68k-d0+)))

      ;;; --- System / Platform ---
      (#.+op-io-read+
       ;; 68k uses memory-mapped I/O
       (let ((vd (first operands))
             (port (second operands))
             (width (third operands)))
         ;; Load port address
         (m68k-emit-move-imm-an buf port +68k-a0+)
         (ecase width
           (0 (m68k-emit-move-byte-an-ind-dn buf +68k-a0+ +68k-d0+)
              (m68k-emit-andi buf +68k-d0+ #xFF))
           (1 (m68k-emit-move-word-an-ind-dn buf +68k-a0+ +68k-d0+)
              (m68k-emit-andi buf +68k-d0+ #xFFFF))
           (2 (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+))
           (3 (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-io-write+
       (let ((port (first operands))
             (vs (second operands))
             (width (third operands)))
         (m68k-emit-move-imm-an buf port +68k-a0+)
         (m68k-load-vreg buf +68k-d0+ vs)
         (ecase width
           (0 (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a0+))
           (1 (m68k-emit-move-word-dn-an-ind buf +68k-d0+ +68k-a0+))
           (2 (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+))
           (3 (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)))))

      (#.+op-halt+
       ;; STOP #$2000 - stop with supervisor mode, interrupts disabled
       (let ((halt-label (mvm-make-label)))
         (m68k-emit-label buf halt-label)
         (m68k-emit-stop buf #x2000)
         (m68k-emit-bra buf halt-label)))

      (#.+op-cli+
       ;; Disable interrupts: set interrupt mask to 7
       ;; OR.W #$0700, SR (requires supervisor mode)
       (m68k-emit-word buf #x007C)  ; ORI to SR
       (m68k-emit-word buf #x0700))

      (#.+op-sti+
       ;; Enable interrupts: clear interrupt mask
       ;; AND.W #$F8FF, SR
       (m68k-emit-word buf #x027C)  ; ANDI to SR
       (m68k-emit-word buf #xF8FF))

      (#.+op-percpu-ref+
       ;; 68k doesn't have per-CPU data in the traditional sense.
       ;; Use a fixed memory location.
       (let ((vd (first operands))
             (offset (second operands)))
         ;; Load from absolute address (use a base register, A5, if available)
         (m68k-emit-move-imm-an buf offset +68k-a0+)
         (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-percpu-set+
       (let ((offset (first operands))
             (vs (second operands)))
         (m68k-emit-move-imm-an buf offset +68k-a0+)
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)))

      (otherwise
       ;; Unknown opcode: ILLEGAL
       (m68k-emit-illegal buf)))))

;;; ============================================================
;;; Main Translation Entry Point
;;; ============================================================

(defun translate-mvm-to-68k (bytecode function-table)
  "Translate MVM bytecode to Motorola 68000 native code.
   BYTECODE is a (vector (unsigned-byte 8)).
   FUNCTION-TABLE maps function indices to bytecode offsets.
   Returns a byte vector of 68k big-endian machine code."
  (let* ((buf (make-m68k-buffer))
         (label-map (make-hash-table :test 'eql))
         (bc bytecode)
         (len (length bc))
         (pc 0))

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
                        (let ((off (first operands)))
                          (let ((target (+ new-pc off)))
                            (unless (gethash target label-map)
                              (setf (gethash target label-map)
                                    (mvm-make-label))))))
                       ((and (member :off32 op-types)
                             (member :reg op-types))
                        (let ((off (second operands)))
                          (let ((target (+ new-pc off)))
                            (unless (gethash target label-map)
                              (setf (gethash target label-map)
                                    (mvm-make-label))))))))))
               (setf pc new-pc)))

    ;; Register function entry points as branch targets
    (when function-table
      (maphash (lambda (idx mvm-offset)
                 (declare (ignore idx))
                 (unless (gethash mvm-offset label-map)
                   (setf (gethash mvm-offset label-map) (mvm-make-label))))
               function-table))

    ;; Emit prologue
    (m68k-emit-prologue buf)

    ;; Second pass: translate
    (setf pc 0)
    (loop while (< pc len)
          do (progn
               ;; Emit label at current PC before translating
               (let ((label (gethash pc label-map)))
                 (when label
                   (m68k-emit-label buf label)))
               (let* ((decoded (decode-instruction bc pc))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pc (cddr decoded)))
                 (m68k-translate-insn buf opcode operands new-pc label-map function-table)
                 (setf pc new-pc))))

    ;; Resolve labels
    (m68k-fixup-labels buf)

    ;; Convert to byte vector
    (m68k-buffer-to-bytes buf)))

;;; ============================================================
;;; Installer
;;; ============================================================

(defun m68k-disassemble-native (buf &key (start 0) (end nil))
  "Print a hex dump of 68k native code for debugging.
   Each line shows one 16-bit word (big-endian)."
  (let* ((words (m68k-buffer-words buf))
         (limit (or end (m68k-buffer-word-count buf))))
    (loop for i from start below limit
          do (format t "  ~4,'0X: ~4,'0X~%" (* i 2) (aref words i)))))

(defun install-68k-translator ()
  "Install the 68k translator into the target descriptor."
  (let ((target *target-68k*))
    (setf (target-translate-fn target) #'translate-mvm-to-68k)
    (setf (target-emit-prologue target)
          (lambda (target buf)
            (declare (ignore target))
            (m68k-emit-prologue buf)))
    (setf (target-emit-epilogue target)
          (lambda (target buf)
            (declare (ignore target))
            (m68k-emit-epilogue buf)))
    target))
