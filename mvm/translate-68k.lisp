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
  ;; D0 and D1 are the TRANSLATOR'S SCRATCH and must not also be allocatable.
  ;; They used to be V0/V1, and ~200 emit sites load a vreg into D0 to work on
  ;; it -- so any op touching a vreg clobbered V0 while it was live.  The
  ;; visible symptom: `(setf (mem-ref addr :u8) v)` stored (addr >> 2) instead
  ;; of v, because the address untag ran through D0 after the value had been
  ;; placed there.  Sliding the window to D2..D7 makes every existing scratch
  ;; use correct at once, at the cost of two inline vregs (V6/V7 now spill).
  (vector (make-m68k-reg :type :data :number +68k-d2+)     ; V0
          (make-m68k-reg :type :data :number +68k-d3+)     ; V1
          (make-m68k-reg :type :data :number +68k-d4+)     ; V2
          (make-m68k-reg :type :data :number +68k-d5+)     ; V3
          (make-m68k-reg :type :data :number +68k-d6+)     ; V4
          (make-m68k-reg :type :data :number +68k-d7+)     ; V5
          nil nil           ; V6-V7 (spill -- D0/D1 are reserved scratch)
          nil nil nil nil   ; V8-V11 (spill)
          nil nil nil nil   ; V12-V15 (spill)
          (make-m68k-reg :type :data :number +68k-d2+)     ; VR (aliases V0)
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
  (words (make-array 65536))            ; GROWS (m68k-emit-word); position tracks fill
  (labels (make-hash-table :test 'eql))
  (fixups nil)      ; list of (word-index label-id fixup-type)
  (position 0)      ; byte position
  (word-count 0))   ; word index (position / 2)

(defvar *68k-li-const-patches* nil
  "List of (NATIVE-BYTE-OFFSET . POOL-INDEX) recorded by +OP-LI-CONST+: at that
   offset is a MOVE.L #imm32,D0 whose 32-bit immediate (bytes +2..+5, big-endian)
   cross.lisp's apply-li-const-patches fills with the constant-pool slot's
   tagged address.  Reset at the start of every translation.")

(defun m68k-emit-word (buf word)
  "Emit a 16-bit instruction word."
  (let ((idx (m68k-buffer-word-count buf)))
    ;; Grow by doubling.  The buffer was a fixed 64K words (128 KB), which no
    ;; single-file image ever reached and the CL image exceeds many times over.
    (when (>= idx (length (m68k-buffer-words buf)))
      (let ((new (make-array (* 2 (length (m68k-buffer-words buf))))))
        (replace new (m68k-buffer-words buf))
        (setf (m68k-buffer-words buf) new)))
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

;;; ============================================================
;;; Convention slots (nargs / cenv / mv-count)
;;; ============================================================
;;;
;;; Same absolute-slot shape as i386: the caller writes, the callee reads.
;;; The block sits at 6MB -- above the image (loaded at 64KB) and below the
;;; downward stack (top 8MB), so it is clear of both stack and heap (cons at
;;; 9MB, general at 10MB).  A1 carries the address; D0/D1 are the reserved
;;; translator scratch (see *68k-vreg-map*).
(defparameter *68k-globals-base* #x00600000
  "Base of the 68k absolute-address convention slot block.")

(defparameter *68k-linux-mode* nil
  "T when building a HOSTED Linux/m68k image rather than a bare-metal one.
   Bare and hosted are different memory maps: #x00600000 is ordinary RAM on
   `virt' and is NOT MAPPED under Linux, and the Goldfish TTY at
   +M68K-UART-BASE+ does not exist in our address space there.
   M68K-SET-LINUX-MODE moves the slots and turns the console byte into write(2).")

(defparameter *68k-hosted-globals-base* #x10000A00
  "Convention slots for the HOSTED port, inside the mmap'd heap and above the
   Cheney metadata at #x10000040 — the address every hosted port picks.")

;;; Linux/m68k syscall numbers.  This is the ORIGINAL Linux table (the one i386
;;; also uses) and not the asm-generic one: write is 4 here, 64 on RISC-V.
;;; 192 is mmap2, whose sixth argument is an offset in PAGES; it is used in
;;; preference to 90 (old_mmap) because old_mmap takes a POINTER to a block of
;;; six arguments in d1 rather than the arguments themselves.
;;;
;;; THE ARGUMENT REGISTERS ARE d1,d2,d3,d4,d5,a0 — five data registers and then
;;; an ADDRESS register, which is why a six-argument syscall needs a0 here and
;;; nothing else in this file does.  The number goes in d0 and the result comes
;;; back in d0.
(defconstant +68k-linux-sys-exit+   1)
(defconstant +68k-linux-sys-read+   3)
(defconstant +68k-linux-sys-write+  4)
(defconstant +68k-linux-sys-mmap2+ 192)
(defun m68k-nargs-addr ()   (+ *68k-globals-base* #x00))
(defun m68k-cenv-addr ()    (+ *68k-globals-base* #x08))
;;; SHARED contract address, as on x64/aarch64/i386/ppc64: compiler-emitted
;;; mem-refs and shared CL source read #x10000090 directly.  It is 256MB in,
;;; so the 68k images must be run with at least that much RAM -- the harness
;;; passes -m 512.  Unlike RISC-V (where it lands in UART MMIO) and ppc32
;;; (outside the mapped TLBs), on m68k virt it is ordinary RAM.
(defun m68k-mvcount-addr () modus.mvm::+mv-count-addr+)

(defun m68k-emit-store-abs (buf dn addr)
  "Store data register DN to absolute ADDR (address parked in A1)."
  (m68k-emit-move-imm-an buf addr +68k-a1+)
  (m68k-emit-move-dn-an-ind buf dn +68k-a1+))

(defun m68k-emit-load-abs (buf dn addr)
  "Load from absolute ADDR into data register DN (address parked in A1)."
  (m68k-emit-move-imm-an buf addr +68k-a1+)
  (m68k-emit-move-an-ind-dn buf +68k-a1+ dn))

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
  "BRA.L target -- 68020+, 32-bit displacement.  Every branch is long: the CL
   image is megabytes of native code, and a 16-bit displacement reaches 32 KB."
  (m68k-emit-word buf #x60FF)
  (let ((disp-pos (m68k-buffer-position buf)))
    (m68k-emit-word buf 0)
    (when label-id
      (push (list (1- (m68k-buffer-word-count buf)) label-id :disp32 (+ disp-pos 2))
            (m68k-buffer-fixups buf)))
    (m68k-emit-word buf 0)))

(defun m68k-emit-bcc-word (buf cc &optional label-id)
  "Bcc.L target -- 68020+, 32-bit displacement (the name is historical).  CC is
   the 4-bit condition code.  See M68K-EMIT-BRA for why every branch is long."
  ;; Format: 0110 cc(4) 11111111, then disp32
  (m68k-emit-word buf (logior (ash #b0110 12) (ash (logand cc #xF) 8) #xFF))
  (let ((disp-pos (m68k-buffer-position buf)))
    (m68k-emit-word buf 0)
    (when label-id
      (push (list (1- (m68k-buffer-word-count buf)) label-id :disp32 (+ disp-pos 2))
            (m68k-buffer-fixups buf)))
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
  "BSR.L target -- 68020+, 32-bit displacement.  Every branch is long: the CL
   image is megabytes of native code, and a 16-bit displacement reaches 32 KB."
  (m68k-emit-word buf #x61FF)
  (let ((disp-pos (m68k-buffer-position buf)))
    (m68k-emit-word buf 0)
    (when label-id
      (push (list (1- (m68k-buffer-word-count buf)) label-id :disp32 (+ disp-pos 2))
            (m68k-buffer-fixups buf)))
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
  ;; Bit 7 is fixed 1 and bit 6 is the SIZE (1 = .L).  This used to OR in
  ;; (ash +68k-size-long+ 6) -- MOVE's size code, 2, which sets bit 7 and
  ;; leaves bit 6 clear -- so every MOVEM was MOVEM.W: the prologue saved
  ;; D4-D7 as their low 16 bits and the epilogue restored them SIGN-EXTENDED.
  ;; Every callee-saved value above 32767 was destroyed across every call.
  (m68k-emit-word buf (logior #x48C0
                              (ash +ea-mode-an-predec+ 3)
                              +68k-a7+))
  (m68k-emit-word buf register-mask))

(defun m68k-emit-movem-from-postinc (buf register-mask)
  "MOVEM.L (SP)+, registers - restore multiple registers.
   REGISTER-MASK: bit 0=D0, ..., bit 7=D7, bit 8=A0, ..., bit 15=A7."
  ;; Format: 0100 1100 10 mode reg, then mask
  ;; (A7)+: mode 3, reg 7
  ;; .L -- see M68K-EMIT-MOVEM-TO-PREDEC.
  (m68k-emit-word buf (logior #x4CC0
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
  "Calculate the A6-relative offset for a spilled virtual register.

   V6 and V7 spill now too: D0/D1 are reserved as the translator's scratch
   (see *68k-vreg-map*), which cost two inline registers.  Slots are laid out
   by vreg index from V6, so V8..V15 keep moving down as before, two slots
   further along -- the frame reserves room for all of them."
  (cond
    ((and (>= vreg 6) (<= vreg 15))
     (+ +68k-spill-base-offset+ (* (- vreg 6) -4)))
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

(defconstant +68k-frame-slot-base+ -72
  "A6-relative offset for frame slot 0 (local variables via obj-ref VFP).
   Frame slots grow downward: slot N is at A6 + frame-slot-base - N*4.

   This MUST stay below the last spill slot.  Reserving D0/D1 as the
   translator's scratch pushed V6 and V7 out of registers and into the spill
   area, which grew it by 8 bytes (it now ends at A6-68 rather than A6-60).
   The old -64 base then overlapped the V14/V15 spill slots: a local and a
   spilled vreg shared a word, and `(cons 40 2)` returned through a smashed
   pointer -- the 68k ran off to an odd PC (0x006f4001) and took an address
   error.  Move the base down by the same 8 bytes the spill area gained.")

(defconstant +68k-frame-size+ 592
  "Stack frame size: 68 bytes spill area (V6-V15) + 512 bytes of frame slots
   (128 slots from A6-72 down to A6-580) = 580, rounded to 592.

   ONE HUNDRED TWENTY-EIGHT SLOTS, NOT FOURTEEN.  The MVM compiler picks the
   `obj-ref VFP <idx>' index per function and tells the back end no bound,
   which is why translate-x64 reserves 128; a local past the last reserved
   slot is addressed BELOW the frame, in memory the next call's frame
   occupies.  Measured on RISC-V: MAKE-HASH-TABLE's &rest prologue alone
   reads frame slot 31.  -592 still fits LINK's 16-bit displacement.")

(defun m68k-frame-bytes (slots)
  "LINK size for SLOTS slots: slot N is at A6-72-4N, so 68+4*SLOTS, rounded
   to 16.  The unsized frame keeps 592."
  (if (= slots +frame-legacy-slots+)
      +68k-frame-size+
      (logand (+ 68 (* 4 slots) 15) (lognot 15))))

(defun m68k-emit-prologue (buf &optional (nparams 0) (slots +frame-legacy-slots+))
  "Emit 68k function prologue. LINK, copy parameters 5.. into frame slots 4..,
   save callee-saved registers.

   NPARAMS is the frame-enter TRAP's code.  Only V0-V3 travel in registers;
   the caller PUSHes the rest (arg 4 last, a longword each) and the body reads
   parameter i as `obj-ref VFP i', so they must be copied into the frame --
   translate-x64/i386/aarch64/arm32 all do it, and this back end did not, so
   every fifth-and-later parameter was an uninitialised slot.  Found on RISC-V
   in the real CL image (COPY-SEQ's fifth argument to %BULK-COPY).

   JSR pushed the return address and LINK the old A6, so parameter i is at
   A6 + 8 + (i-4)*4.  D0 is translator scratch and free here."
  (when (> nparams (max slots 128))
    (error "MVM 68k: ~D parameters exceed the ~D-slot frame" nparams slots))
  ;; LINK A6, #-frame-size
  (m68k-emit-link buf +68k-a6+ (logand (- (m68k-frame-bytes slots)) #xFFFF))
  (loop for i from 4 below nparams
        do (m68k-emit-move-disp-dn buf +68k-a6+ (+ 8 (* (- i 4) 4)) +68k-d0+)
           (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a6+
                                   (- +68k-frame-slot-base+ (* i 4))))
  ;; Save callee-saved data registers D4-D7 using MOVEM
  ;; Predecrement mask is reversed: bit 15=D0, bit 14=D1, ...
  ;; D4=bit 11, D5=bit 10, D6=bit 9, D7=bit 8
  ;; A2-A4 (VA, VL, VN) ARE NOT SAVED -- see m68k-emit-epilogue.
  (let ((mask (logior (ash 1 11)   ; D4
                      (ash 1 10)   ; D5
                      (ash 1 9)    ; D6
                      (ash 1 8))))  ; D7
    (m68k-emit-movem-to-predec buf mask)))

(defun m68k-emit-epilogue (buf)
  "Emit 68k function epilogue. Restore registers, UNLK, RTS."
  ;; Restore callee-saved registers with MOVEM (postincrement)
  ;; Normal mask: bit 0=D0, ..., bit 7=D7, bit 8=A0, ...
  ;; D4=bit 4, D5=bit 5, D6=bit 6, D7=bit 7
  ;; A2 (VA), A3 (VL) AND A4 (VN) ARE GLOBAL STATE AND ARE NOT RESTORED.
  ;; A2 is the ALLOCATION POINTER; restoring it on return rolls the heap
  ;; pointer back over everything the callee allocated, so the caller's next
  ;; CONS is handed live memory.  translate-aarch64 names the consequence
  ;; ("allocations made by callees would be lost on return"); found on RISC-V,
  ;; where it destroyed the first nine conses of the image.  A3 is the alloc
  ;; LIMIT, which a collection legitimately moves, and A4 is a constant.  The
  ;; two masks must stay symmetric, so both dropped the same three.
  (let ((mask (logior (ash 1 4)    ; D4
                      (ash 1 5)    ; D5
                      (ash 1 6)    ; D6
                      (ash 1 7))))  ; D7
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

    ;; NO FPU: the compiler lowers every float op to mvm/softfloat.lisp, so one
    ;; reaching here is a gap -- recorded and ILLEGAL, never an FPU instruction
    ;; a 68LC040 would trap on.
    (when (and (eql +target-soft-float+ 1)
               (member opcode (list +op-fadd+ +op-fsub+ +op-fmul+ +op-fdiv+ +op-fsqrt+
                                    +op-fround32+ +op-itof+ +op-ftoi+
                                    +op-f32-load+ +op-f32-store+)))
      (pushnew opcode *68k-unimpl-opcodes*)
      (m68k-emit-illegal buf)
      (return-from m68k-translate-insn nil))

    (case opcode
      ;;; --- NOP / BREAK / TRAP ---
      (#.+op-nop+
       (m68k-emit-nop buf))

      (#.+op-break+
       (m68k-emit-illegal buf))

      (#.+op-trap+
       (let ((code (first operands)))
         (cond
           ((frame-enter-code-p code)
            ;; Frame-enter: CODE carries the parameter count -- see m68k-emit-prologue.
            (m68k-emit-prologue buf (frame-enter-nparams code) (frame-enter-slots code)))
           ((< code #x0300)
            ;; Frame-alloc/frame-free: NOP for now
            nil)
           ((= code #x0510)
            ;; SETJMP.  Stack the outer jmpbuf, then save A7 / A6 / resume
            ;; address / D4..D7.  First return NIL; LONGJMP re-enters at RESUME
            ;; with D2 = T.  A capped push arms nothing.  The resume address is
            ;; LEA (bd.l,PC) with the same hand-referenced :disp32 fixup
            ;; +op-fn-addr+ uses.
            (let ((skip (mvm-make-label))
                  (resume (mvm-make-label)))
              (m68k-emit-handler-push buf)
              (m68k-emit-tst buf +68k-d0+)
              (m68k-emit-bne buf skip)
              (m68k-emit-move-imm-an buf (m68k-jmpbuf-addr) +68k-a1+)
              (m68k-emit-move-an-disp buf +68k-a7+ +68k-a1+ 0)
              (m68k-emit-move-an-disp buf +68k-a6+ +68k-a1+ 4)
              (m68k-emit-word buf #x41FB)                     ; LEA (bd.l,PC),A0
              (let ((ext-pos (m68k-buffer-position buf)))
                (m68k-emit-word buf #x0170)
                (m68k-emit-word buf 0)
                (push (list (1- (m68k-buffer-word-count buf)) resume :disp32
                            (+ ext-pos 2))
                      (m68k-buffer-fixups buf))
                (m68k-emit-word buf 0))
              (m68k-emit-move-an-disp buf +68k-a0+ +68k-a1+ 8)
              (loop for d in (list +68k-d4+ +68k-d5+ +68k-d6+ +68k-d7+)
                    for i from 3
                    do (m68k-emit-move-dn-disp buf d +68k-a1+ (* 4 i)))
              (m68k-emit-label buf skip)
              (m68k-emit-move-an-dn buf +68k-a4+ +68k-d2+)      ; first return: NIL
              (m68k-emit-label buf resume)))
           ((= code #x0511)
            ;; LONGJMP.  Copy the jmpbuf aside first, zero the live capped count,
            ;; pop, then restore and JMP with D2 = T.  Nothing armed (word 0 = 0):
            ;; ILLEGAL rather than a jump to address zero.
            (let ((nohandler (mvm-make-label)))
              (m68k-emit-move-imm-an buf (m68k-jmpbuf-addr) +68k-a1+)
              (m68k-emit-move-disp-dn buf +68k-a1+ 0 +68k-d0+)
              (m68k-emit-tst buf +68k-d0+)
              (m68k-emit-beq buf nohandler)
              (m68k-emit-move-imm-an buf (m68k-lj-scratch-addr) +68k-a0+)
              (m68k-emit-copy-words buf +68k-a1+ +68k-a0+)
              (m68k-emit-move-imm-an buf (m68k-hcapped-addr) +68k-a1+)
              (m68k-emit-move-imm-dn buf 0 +68k-d0+)
              (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ 0)
              (m68k-emit-handler-pop buf)
              (m68k-emit-move-imm-an buf (m68k-lj-scratch-addr) +68k-a1+)
              (loop for d in (list +68k-d4+ +68k-d5+ +68k-d6+ +68k-d7+)
                    for i from 3
                    do (m68k-emit-move-disp-dn buf +68k-a1+ (* 4 i) d))
              (m68k-emit-move-disp-an buf +68k-a1+ 8 +68k-a0+)  ; resume
              (m68k-emit-move-disp-an buf +68k-a1+ 4 +68k-a6+)
              (m68k-emit-move-disp-an buf +68k-a1+ 0 +68k-a7+)
              (m68k-emit-move-imm-dn buf #xDEAD1009 +68k-d2+)  ; second return: T
              (m68k-emit-jmp-an-ind buf +68k-a0+)
              (m68k-emit-label buf nohandler)
              (m68k-emit-illegal buf)))
           ((= code #x0512)
            ;; CLEAR-HANDLER: pop one frame; D2 (the result) untouched.
            (m68k-emit-handler-pop buf))
           ((= code #x0530)
            ;; COPY-OVERFLOW-ARGS: the &rest/&key prologue's RUNTIME copy of
            ;; arguments 4.. into frame slots 4.., as translate-x64/i386/riscv
            ;; do.  JSR pushed the return address and LINK the old A6, so
            ;; argument i is at A6 + 8 + (i-4)*4, and slot i at
            ;; A6 + slot-base - i*4.  A LOOP, capped at 121 arguments as on
            ;; x64: +APPLY-SPREAD-MAX+ positional and the tail-list word of a
            ;; +REST-TAIL-NARGS+ (254) call.  (It was unrolled to 32, so a
            ;; 33..120-argument &rest call read slots nothing had copied.)  D0
            ;; counts, D1 carries the word, A0 walks the arguments up and A1
            ;; the slots down -- A0/A1 are translator scratch, not vregs.
            (let ((done (mvm-make-label)) (nocap (mvm-make-label)) (top (mvm-make-label)))
              (m68k-emit-load-abs buf +68k-d0+ (m68k-nargs-addr))
              (m68k-emit-cmpi buf +68k-d0+ 4)
              (m68k-emit-ble buf done)
              (m68k-emit-cmpi buf +68k-d0+ 121)
              (m68k-emit-ble buf nocap)
              (m68k-emit-move-imm-dn buf 121 +68k-d0+)
              (m68k-emit-label buf nocap)
              (m68k-emit-subq buf +68k-d0+ 4)                        ; count = n - 4
              (m68k-emit-lea-disp buf +68k-a6+ 8 +68k-a0+)            ; argument 4
              (m68k-emit-lea-disp buf +68k-a6+ (- +68k-frame-slot-base+ 16) +68k-a1+) ; slot 4
              (m68k-emit-label buf top)
              (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d1+)
              (m68k-emit-move-dn-an-ind buf +68k-d1+ +68k-a1+)
              (m68k-emit-addq-an buf +68k-a0+ 4)
              (m68k-emit-subq-an buf +68k-a1+ 4)
              (m68k-emit-subq buf +68k-d0+ 1)
              (m68k-emit-bne buf top)
              (m68k-emit-label buf done)))
           ((and (= code #x0300) *68k-linux-mode*)
            ;; HOSTED: the serial write becomes write(1, &byte, 1).
            ;;
            ;; write(2) wants an ADDRESS, so the untagged byte is PUSHED and the
            ;; buffer is 3(a7) — THREE, not zero: m68k is BIG-ENDIAN, so the low
            ;; byte of a pushed longword is the LAST of its four bytes.  Passing
            ;; a7 itself would print the high byte of the char, which for any
            ;; ASCII character is 0.
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d1+)   ; V0 -> scratch
            (m68k-emit-asr-imm buf +68k-d1+ 1)             ; untag
            (m68k-emit-push-dn buf +68k-d1+)
            (m68k-emit-lea-disp buf +68k-a7+ 3 +68k-a0+)   ; a0 = &low byte
            (m68k-emit-move-imm-dn buf 1 +68k-d1+)         ; arg1: fd = stdout
            (m68k-emit-move-an-dn buf +68k-a0+ +68k-d2+)   ; arg2: buf
            (m68k-emit-move-imm-dn buf 1 +68k-d3+)         ; arg3: count
            (m68k-emit-move-imm-dn buf +68k-linux-sys-write+ +68k-d0+)
            (m68k-emit-trap buf 0)
            (m68k-emit-addq-an buf +68k-a7+ 4))            ; pop the byte
           ((and (= code #x0301) *68k-linux-mode*)
            ;; HOSTED: read(0, &byte, 1); the byte comes back TAGGED in V0 (d2),
            ;; matching the bare arm's contract.  Same 3(a7) big-endian offset.
            (m68k-emit-move-imm-dn buf 0 +68k-d1+)         ; make room (value
            (m68k-emit-push-dn buf +68k-d1+)               ; irrelevant)
            (m68k-emit-lea-disp buf +68k-a7+ 3 +68k-a0+)
            (m68k-emit-move-imm-dn buf 0 +68k-d1+)         ; arg1: fd = stdin
            (m68k-emit-move-an-dn buf +68k-a0+ +68k-d2+)   ; arg2: buf
            (m68k-emit-move-imm-dn buf 1 +68k-d3+)         ; arg3: count
            (m68k-emit-move-imm-dn buf +68k-linux-sys-read+ +68k-d0+)
            (m68k-emit-trap buf 0)
            (m68k-emit-move-imm-dn buf 0 +68k-d2+)
            (m68k-emit-lea-disp buf +68k-a7+ 3 +68k-a0+)
            (m68k-emit-move-byte-an-ind-dn buf +68k-a0+ +68k-d2+)
            (m68k-emit-addq-an buf +68k-a7+ 4)
            (m68k-emit-lsl-imm buf +68k-d2+ 1))            ; tag as fixnum
           ;; --- HOSTED SERVICE TRAPS (the CL image's syscall layer) -------
           ;; Linux/m68k: number in d0, arguments d1..d5 then a0, result d0.
           ;; V0..V5 are D2..D7, so each argument is read before its register
           ;; is overwritten; V4/V5 (D6/D7) are only read.  A trap is not a
           ;; call, but V0..V3 are argument registers and dead after it.
           ((and (= code #x0502) *68k-linux-mode*)
            ;; GENERIC 3-ARG SYSCALL: V0 = number, V1..V3 args, all TAGGED;
            ;; result TAGGED in V0.
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d0+)
            (m68k-emit-asr-imm buf +68k-d0+ 1)
            (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d1+)
            (m68k-emit-asr-imm buf +68k-d1+ 1)
            (m68k-emit-move-dn-dn buf +68k-d4+ +68k-d2+)
            (m68k-emit-asr-imm buf +68k-d2+ 1)
            (m68k-emit-move-dn-dn buf +68k-d5+ +68k-d3+)
            (m68k-emit-asr-imm buf +68k-d3+ 1)
            (m68k-emit-trap buf 0)
            (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)
            (m68k-emit-lsl-imm buf +68k-d2+ 1))
           ((and (= code #x0503) *68k-linux-mode*)
            ;; RAW 3-ARG SYSCALL: number TAGGED, arguments and result RAW.
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d0+)
            (m68k-emit-asr-imm buf +68k-d0+ 1)
            (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d1+)
            (m68k-emit-move-dn-dn buf +68k-d4+ +68k-d2+)
            (m68k-emit-move-dn-dn buf +68k-d5+ +68k-d3+)
            (m68k-emit-trap buf 0)
            (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+))
           ((and (= code #x050B) *68k-linux-mode*)
            ;; GENERIC 6-ARG SYSCALL: V0 = number, V1..V6 args, TAGGED.  V6 is
            ;; a frame slot (spilled), so it goes first, through d0, to a0.
            (m68k-load-vreg buf +68k-d0+ 6)
            (m68k-emit-asr-imm buf +68k-d0+ 1)
            (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
            (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d1+)
            (m68k-emit-asr-imm buf +68k-d1+ 1)
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d0+)
            (m68k-emit-asr-imm buf +68k-d0+ 1)
            (m68k-emit-move-dn-dn buf +68k-d4+ +68k-d2+)
            (m68k-emit-asr-imm buf +68k-d2+ 1)
            (m68k-emit-move-dn-dn buf +68k-d5+ +68k-d3+)
            (m68k-emit-asr-imm buf +68k-d3+ 1)
            (m68k-emit-move-dn-dn buf +68k-d6+ +68k-d4+)
            (m68k-emit-asr-imm buf +68k-d4+ 1)
            (m68k-emit-move-dn-dn buf +68k-d7+ +68k-d5+)
            (m68k-emit-asr-imm buf +68k-d5+ 1)
            (m68k-emit-trap buf 0)
            (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)
            (m68k-emit-lsl-imm buf +68k-d2+ 1))
           ((and (= code #x0310) *68k-linux-mode*)
            ;; RDTSC, hosted: MILLISECONDS SINCE THE FIRST READ, TAGGED, in V0;
            ;; rate 1000 Hz (#x0311) -- ARM32's and RV32's answer, because a
            ;; 30-bit fixnum holds 12.4 days of this and no absolute counter.
            ;; clock_gettime(CLOCK_MONOTONIC) is 260 on m68k.  The first read's
            ;; seconds are kept TAGGED, +1 so 0 means unset, at #x10000FB8.
            (let ((have (mvm-make-label)))
              (m68k-emit-lea-disp buf +68k-a7+ -8 +68k-a7+)
              (m68k-emit-moveq buf +68k-d1+ 1)                     ; CLOCK_MONOTONIC
              (m68k-emit-move-an-dn buf +68k-a7+ +68k-d2+)
              (m68k-emit-move-imm-dn buf 260 +68k-d0+)
              (m68k-emit-trap buf 0)
              (m68k-emit-move-disp-dn buf +68k-a7+ 0 +68k-d0+)     ; tv_sec
              (m68k-emit-move-disp-dn buf +68k-a7+ 4 +68k-d1+)     ; tv_nsec
              (m68k-emit-lea-disp buf +68k-a7+ 8 +68k-a7+)
              (m68k-emit-move-imm-an buf #x10000FB8 +68k-a1+)
              (m68k-emit-move-an-ind-dn buf +68k-a1+ +68k-d2+)
              (m68k-emit-tst buf +68k-d2+)
              (m68k-emit-bne buf have)
              (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)
              (m68k-emit-addq buf +68k-d2+ 1)
              (m68k-emit-lsl-imm buf +68k-d2+ 1)
              (m68k-emit-move-dn-an-ind buf +68k-d2+ +68k-a1+)
              (m68k-emit-label buf have)
              (m68k-emit-asr-imm buf +68k-d2+ 1)
              (m68k-emit-subq buf +68k-d2+ 1)                      ; first second
              (m68k-emit-sub-dn-dn buf +68k-d2+ +68k-d0+)          ; elapsed seconds
              (m68k-emit-move-imm-dn buf 1000 +68k-d2+)
              (m68k-emit-muls buf +68k-d2+ +68k-d0+)
              (m68k-emit-move-imm-dn buf 1000000 +68k-d2+)
              (m68k-emit-divs buf +68k-d2+ +68k-d1+)               ; nsec -> ms
              (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
              (m68k-emit-lsl-imm buf +68k-d0+ 1)
              (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)))
           ((and (= code #x0520) *68k-linux-mode*)
            ;; INSTALL SIGNAL HANDLERS (faults as conditions): NOT YET on 68k.
            ;; An explicit no-op, so a fault ends the process with the kernel's
            ;; own signal rather than the fallback below issuing #x520 as a
            ;; syscall number and pushing a word nobody pops.  BRING-UP GAP.
            nil)
           ((and (= code #x0311) *68k-linux-mode*)
            ;; CNTFRQ: #x0310's rate, TAGGED (1000 Hz: milliseconds).
            (m68k-emit-move-imm-dn buf 2000 +68k-d2+))
           ((and (= code #x0500) *68k-linux-mode*)
            ;; HOSTED: exit(status), status arriving TAGGED in V0 (d2).
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d1+)
            (m68k-emit-asr-imm buf +68k-d1+ 1)
            (m68k-emit-move-imm-dn buf +68k-linux-sys-exit+ +68k-d0+)
            (m68k-emit-trap buf 0))
           ((= code #x0300)
            ;; BARE METAL: serial write.  V0 IS D2, NOT D0 — this arm read D0
            ;; until 2026-09-24, left behind when *68K-VREG-MAP* slid the window
            ;; to D2..D7 (D0/D1 became reserved scratch).  The arch ladder cannot
            ;; see it: its oracle is guest MEMORY, not serial output, so the one
            ;; path that would notice is the one no test on this target uses.
            ;; move.l d2, d1 (copy V0 to scratch)
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d1+)
            ;; asr.l #1, d1 (untag fixnum)
            (m68k-emit-asr-imm buf +68k-d1+ 1)
            ;; movea.l #uart_base, a0
            (m68k-emit-move-imm-an buf +m68k-uart-base+ +68k-a0+)
            ;; move.l d1, (a0) (write to UART — Goldfish TTY needs 32-bit write)
            (m68k-emit-move-dn-an-ind buf +68k-d1+ +68k-a0+))
           (t
            ;; Real CPU trap -- hosted, an MVM service trap with no arm here,
            ;; recorded as #x1xxxx in the census.
            (when (and *68k-linux-mode* (> code 15))
              (pushnew (+ #x10000 code) *68k-unimpl-opcodes*))
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
             ;; Everything else -- both spilled, OR an address-register source
             ;; into a spilled destination -- through D0 with the general
             ;; helpers, which handle every register kind.  The old fallback
             ;; assumed BOTH spilled and asked for the source's spill slot, so
             ;; `MOV V7 VN' (VN lives in A4, V7 spills) died at build time with
             ;; "unexpected spill for vreg 19".  That is the first instruction
             ;; of every &rest ladder with nothing in it, which is how
             ;; r29-rest-many found it.
             (t
              (m68k-load-vreg buf +68k-d0+ vs)
              (m68k-store-vreg buf vd +68k-d0+))))))

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

       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((da (m68k-vreg-or-scratch buf va +68k-d0+)))
           (unless (= da +68k-d0+)
             (m68k-emit-move-dn-dn buf da +68k-d0+))
           (let ((db (m68k-vreg-or-scratch buf vb +68k-d1+)))
             (m68k-emit-sub-dn-dn buf db +68k-d0+)
             (m68k-store-vreg buf vd +68k-d0+)))))

      ((#.+op-mul+ #.+op-mul-checked+)
       ;; :MUL-CHECKED shares this clause.  The checked opcodes mean "tagged
       ;; arithmetic that promotes to a bignum on overflow"; implementing the
       ;; promotion needs the generic-arith slow path, which these back ends do
       ;; not have yet.  Falling back to plain WRAPPING arithmetic is exactly
       ;; what translate-x64 and translate-i386 do when a module has no
       ;; generic-arith entry (see *i386-checked-arith-slowpath*), so this is
       ;; the documented degrade rather than a new invention -- and it is a
       ;; large step up from the previous behaviour, which was to trap.

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

      ;; (cmp-imm / test-imm Va imm32): the immediate into D1, then as CMP / TEST
      (#.+op-cmp-imm+
       (let ((da (m68k-vreg-or-scratch buf (first operands) +68k-d0+)))
         (m68k-emit-move-imm-dn buf (logand (%imm32-signed (second operands)) #xFFFFFFFF) +68k-d1+)
         (m68k-emit-cmp-dn-dn buf +68k-d1+ da)))

      (#.+op-test-imm+
       (progn
         (m68k-load-vreg buf +68k-d0+ (first operands))
         (m68k-emit-move-imm-dn buf (logand (%imm32-signed (second operands)) #xFFFFFFFF) +68k-d1+)
         (m68k-emit-and-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-tst buf +68k-d0+)))

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
         (m68k-emit-alloc-mark buf 2)   ; FIRST: clobbers D0/A0
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
         ;; Bump A2 by SIXTEEN, not 8.  Objects are rounded to 16 and pointer
         ;; types are read from the low FOUR bits, so the heap pointer must
         ;; never sit at 8 mod 16: an odd number of 8-byte conses left it there,
         ;; and the next object's tag 9 read as nibble 1 (a cons) while the next
         ;; cons's 1 read as 9.  r26-callee-alloc answered 1042 on 68k (and on
         ;; riscv32 and ppc32).  ADDQ stops at 8, hence LEA.
         (m68k-emit-lea-disp buf +68k-a2+ 16 +68k-a2+)
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
         ;; AND NIL IS NOT A CONS.  NIL's low nibble IS +tag-cons+ by design --
         ;; that is what lets car/cdr of NIL be a plain load into a NIL-filled
         ;; page -- so the tag test alone answers T for NIL.  translate-i386's
         ;; comment records the cost: `(loop while (consp cur) ... (setq cur
         ;; (cdr cur)))' never terminates, because (car NIL) hands back NIL and
         ;; the walk recurses on NIL forever.  r24-nil-atom measured 68k at 12:
         ;; the four-bit mask was right, so (consp T) worked and (consp NIL) did
         ;; not.  D0 still holds the untouched value here (D1 is the masked
         ;; copy), so CMPA.L against A4 costs one instruction and no register.
         (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
         (let ((true-label (mvm-make-label))
               (false-label (mvm-make-label))
               (done-label (mvm-make-label)))
           (m68k-emit-bne buf false-label)
           (m68k-emit-cmpa buf +68k-a4+ +68k-d0+)   ; is the value NIL?
           (m68k-emit-beq buf false-label)          ; NIL -> not a cons
           (m68k-emit-bra buf true-label)
           ;; False: load NIL (from A4)
           (m68k-emit-label buf false-label)
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
         ;; The inverse of +op-consp+'s NIL exclusion: (atom NIL) is T.
         (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
         (let ((true-label (mvm-make-label))
               (done-label (mvm-make-label)))
           (m68k-emit-bne buf true-label)
           (m68k-emit-cmpa buf +68k-a4+ +68k-d0+)   ; is the value NIL?
           (m68k-emit-beq buf true-label)           ; NIL is an atom
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
         (m68k-emit-alloc-mark buf 1)   ; FIRST: clobbers D0/A0
         ;; Header: (size << 8) | subtag -- the one 32-bit header layout every
         ;; other allocation site here, ARM32 and i386 write, and the one the
         ;; collector sizes objects by.  This arm used to write
         ;; (subtag << 8) | 9, and OBJ-SUBTAG read byte 1 to match it -- a
         ;; private convention that made every ARRAY and STRING report the low
         ;; byte of its LENGTH as its subtag.
         (let ((header (logior (ash size 8) subtag)))
           ;; Store header at (A2)
           (m68k-emit-move-imm-dn buf header +68k-d0+)
           (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
           ;; Tag the pointer
           (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
           (m68k-emit-ori buf +68k-d0+ +tag-object+)
           ;; Bump alloc: (1+size)*4 bytes, aligned to 16 bytes to keep cons
           ;; alloc pointer aligned -- after ZEROING every word past the
           ;; header.  Unzeroed, a fresh object held whatever an earlier
           ;; collection left in reused memory until its slots were filled:
           ;; on ARM32's identical collector that failed 22-36 of 150
           ;; DEFCLASS/MAKE-INSTANCE rounds in a 16 MB heap (see
           ;; translate-arm32's +op-alloc-obj+).  x64, aarch64, i386 and arm32
           ;; all zero here.
           (let ((total (logand (+ (* (1+ size) 4) 15) (lognot 15)))
                 (zl (mvm-make-label)))
             (m68k-emit-lea-disp buf +68k-a2+ 4 +68k-a0+)
             (m68k-emit-move-imm-dn buf (1- (floor total 4)) +68k-d1+)
             (m68k-emit-label buf zl)
             (m68k-emit-word buf #x4298)                  ; CLR.L (A0)+
             (m68k-emit-subq buf +68k-d1+ 1)
             (m68k-emit-bne buf zl)
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
         ;; translate-x64's CONTRACT, which the compiler and the runtime are
         ;; written against: the header's low byte, TAGGED, for a real heap
         ;; object -- and 0 FOR ANYTHING ELSE.  GENERIC-ADD reads both its
         ;; arguments' subtags before looking at either ("OBJ-SUBTAG is safe on
         ;; any value"), and this arm dereferenced a fixnum: the first 68k CL
         ;; image faulted at address -1 on its first addition.  T is excluded
         ;; explicitly: +T-VALUE+ carries the object nibble but is an immediate.
         (let ((zero (mvm-make-label)) (done (mvm-make-label)))
           (m68k-load-vreg buf +68k-d0+ vs)
           (m68k-emit-cmpi buf +68k-d0+ +t-value+)
           (m68k-emit-beq buf zero)
           (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
           (m68k-emit-andi buf +68k-d1+ #xF)
           (m68k-emit-cmpi buf +68k-d1+ +tag-object+)
           (m68k-emit-bne buf zero)
           (m68k-emit-subi buf +68k-d0+ +tag-object+)
           (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
           ;; Load header; the subtag is its LOW byte, (count << 8) | subtag
           (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
           (m68k-emit-andi buf +68k-d0+ #xFF)
           ;; Tag as fixnum
           (m68k-emit-lsl-imm buf +68k-d0+ 1)
           (m68k-emit-bra buf done)
           (m68k-emit-label buf zero)
           (m68k-emit-moveq buf +68k-d0+ 0)
           (m68k-emit-label buf done)
           (m68k-store-vreg buf vd +68k-d0+))))

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

      ((#.+op-fadd+ #.+op-fsub+ #.+op-fmul+ #.+op-fdiv+)
       (let ((vd (first operands)))
         (m68k-load-vreg buf +68k-d0+ (second operands))
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-float-unbox buf 0)
         (m68k-load-vreg buf +68k-d0+ (third operands))
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-float-unbox buf 1)
         (m68k-emit-fop buf (cond ((= opcode +op-fadd+) #x22)
                                  ((= opcode +op-fsub+) #x28)
                                  ((= opcode +op-fmul+) #x23)
                                  (t                    #x20))
                        1 0)                                  ; FP0 op= FP1
         (m68k-float-box buf 0)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-fsqrt+
       ;; (fsqrt Vd Vs): FSQRT of the payload, boxed as a DOUBLE.  The 68040
       ;; implements FSQRT in hardware (unlike FSIN & co, which trap to FPSP).
       (let ((vd (first operands)))
         (m68k-load-vreg buf +68k-d0+ (second operands))
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-float-unbox buf 0)
         (m68k-emit-fop buf #x04 0 0)                           ; FSQRT FP0,FP0
         (m68k-float-box buf 0)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-fround32+
       ;; (fround32 Vd Vs): the payload rounded to single precision, boxed as a
       ;; SINGLE-FLOAT (#x464).  Through memory as FMOVE.S, not the 68040's
       ;; FSMOVE, so a 68020/030 with a 6888x runs it too.
       (let ((vd (first operands)))
         (m68k-load-vreg buf +68k-d0+ (second operands))
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-float-unbox buf 0)
         (m68k-emit-lea-disp buf +68k-a7+ -4 +68k-a7+)
         (m68k-emit-move-an-an buf +68k-a7+ +68k-a0+)
         (m68k-emit-fmove-s-to-an-ind buf 0 +68k-a0+)
         (m68k-emit-fmove-s-from-an-ind buf +68k-a0+ 0)
         (m68k-emit-lea-disp buf +68k-a7+ 4 +68k-a7+)
         (m68k-float-box buf 0 (logior #x64 (ash 4 8)))
         (m68k-store-vreg buf vd +68k-d0+)))

      ;; ---- Packed single-float vectors (subtag #x12) ----
      ;; Header (N << 8) | #x12, N 4-byte IEEE lanes at +4: lane i at
      ;; tagged + 4i - 5 = tagged + 2*(tagged index) - 5.  The collector sizes
      ;; it 4N + 4 (and treats it as a leaf).
      (#.+op-alloc-f32+
       (let ((vd (first operands)) (zl (mvm-make-label)))
         (m68k-emit-alloc-mark buf 1)   ; FIRST: clobbers D0/A0
         (m68k-load-vreg buf +68k-d1+ (second operands))
         (m68k-emit-asr-imm buf +68k-d1+ 1)                     ; N
         (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-lsl-imm buf +68k-d0+ 8)
         (m68k-emit-ori buf +68k-d0+ #x12)
         (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-object+)
         (m68k-store-vreg buf vd +68k-d0+)
         ;; bytes = align16(4N + 4); ZERO everything past the header (a new
         ;; vector reads 0.0f0), then VA += bytes.
         (m68k-emit-lsl-imm buf +68k-d1+ 2)
         (m68k-emit-addi buf +68k-d1+ 19)
         (m68k-emit-andi buf +68k-d1+ (logand (lognot 15) #xFFFFFFFF))
         (m68k-emit-lea-disp buf +68k-a2+ 4 +68k-a0+)
         (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-lsr-imm buf +68k-d0+ 2)
         (m68k-emit-subq buf +68k-d0+ 1)                        ; words past header
         (m68k-emit-label buf zl)
         (m68k-emit-word buf #x4298)                            ; CLR.L (A0)+
         (m68k-emit-subq buf +68k-d0+ 1)
         (m68k-emit-bne buf zl)
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a2+)))

      (#.+op-f32-load+
       ;; (f32-load Vd Varr Vidx): lane -> FMOVE.S (widen) -> fresh boxed single.
       (let ((vd (first operands)))
         (m68k-load-vreg buf +68k-d0+ (third operands))         ; tagged index
         (m68k-emit-lsl-imm buf +68k-d0+ 1)                     ; 4i
         (m68k-load-vreg buf +68k-d1+ (second operands))
         (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-subi buf +68k-d0+ 5)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-emit-fmove-s-from-an-ind buf +68k-a0+ 0)
         (m68k-float-box buf 0 (logior #x64 (ash 4 8)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-f32-store+
       ;; (f32-store Varr Vidx Vval): payload -> FMOVE.S (round) -> lane.
       ;; Unbox FIRST (it uses A0 and D1), then form the lane address.
       (progn
         (m68k-load-vreg buf +68k-d0+ (third operands))
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-float-unbox buf 0)
         (m68k-load-vreg buf +68k-d0+ (second operands))        ; tagged index
         (m68k-emit-lsl-imm buf +68k-d0+ 1)
         (m68k-load-vreg buf +68k-d1+ (first operands))
         (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-subi buf +68k-d0+ 5)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-emit-fmove-s-to-an-ind buf 0 +68k-a0+)))

      (#.+op-itof+
       ;; Tagged fixnum -> double: untag, FMOVE.L Dn,FP0 (exact for 32 bits).
       (let ((vd (first operands)))
         (m68k-load-vreg buf +68k-d0+ (second operands))
         (m68k-emit-asr-imm buf +68k-d0+ 1)
         (m68k-emit-fmove-l-from-dn buf +68k-d0+ 0)
         (m68k-float-box buf 0)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-ftoi+
       ;; Double -> tagged fixnum, TRUNCATING: FINTRZ first (round toward
       ;; zero, whatever FPCR's mode), then FMOVE.L of an already-integral value.
       (let ((vd (first operands)))
         (m68k-load-vreg buf +68k-d0+ (second operands))
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-float-unbox buf 0)
         (m68k-emit-fop buf #x03 0 0)                         ; FINTRZ FP0,FP0
         (m68k-emit-fmove-l-to-dn buf 0 +68k-d0+)
         (m68k-emit-lsl-imm buf +68k-d0+ 1)                   ; tag
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-fn-addr+
       ;; (fn-addr Vd target) -- a function's native address TAGGED with
       ;; +tag-function+ (3), as funcall dispatch and FUNCTIONP expect.
       ;;
       ;; LEA (bd.l,PC),A1 -- the 68020 full-extension-word form, because
       ;; LEA d16(PC) reaches only +-32 KB and a real image is megabytes.  The
       ;; extension word #x0170 says: base register PC, index suppressed, 32-bit
       ;; base displacement, no memory indirection.  The PC it adds is the
       ;; address of the EXTENSION WORD, so the fixup's reference position is
       ;; set to that by hand (m68k-emit-fixup would measure from the
       ;; displacement word itself).  The translator already emits 68020
       ;; instructions (MULS.L/DIVS.L), so this raises no CPU floor.
       ;; Entries are 4-aligned (translate-mvm-to-68k-1), so OR 3 is exact.
       ;; THE OPERAND IS THE TARGET'S BYTECODE OFFSET, not a function index
       ;; (compiler.lisp's :fn-addr lowering passes function-info-bytecode-
       ;; offset), and #xFFFFFFF0 is the compiler's sentinel for an undefined
       ;; name, which must load NIL so FUNCALL signals UNDEFINED-FUNCTION --
       ;; translate-x64's contract.  An index lookup passed r16 and r29 only
       ;; because each rung's target was the image's FIRST function, offset 0 =
       ;; index 0; the whole prelude, pushed through arm32 as a census, found it
       ;; ("no label for function index 37512").
       (let* ((vd (first operands))
              (target (second operands))
              (label (gethash target label-map)))
         (when (= target #xFFFFFFF0)                     ; undefined name: NIL
           (m68k-emit-move-an-dn buf +68k-a4+ +68k-d0+)
           (m68k-store-vreg buf vd +68k-d0+)
           (return-from m68k-translate-insn nil))
         (unless label
           (error "68k fn-addr: no function at bytecode offset ~D" target))
         (m68k-emit-word buf #x43FB)                     ; LEA (bd.l,PC),A1
         (let ((ext-pos (m68k-buffer-position buf)))
           (m68k-emit-word buf #x0170)                   ; full extension word
           (m68k-emit-word buf 0)                        ; bd high (patched)
           (push (list (1- (m68k-buffer-word-count buf)) label :disp32
                       (+ ext-pos 2))
                 (m68k-buffer-fixups buf))
           (m68k-emit-word buf 0))                       ; bd low (patched)
         (m68k-emit-move-an-dn buf +68k-a1+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-function+)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-li-const+
       ;; (li-const Vd idx) -- the TAGGED address of constant-pool slot IDX, not
       ;; known until the image is laid out: MOVE.L #0,D0 recorded in
       ;; *68k-li-const-patches* and filled in by cross.lisp.
       (let ((vd (first operands))
             (idx (second operands)))
         (push (cons (m68k-buffer-position buf) idx) *68k-li-const-patches*)
         (m68k-emit-word buf #x203C)                     ; MOVE.L #imm32,D0
         (m68k-emit-long buf 0)
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-call-ind+
       (let ((vs (first operands)))
         ;; CLEAR THE FUNCTION TAG, then JSR (A0).  A function value carries tag
         ;; 3 (+op-fn-addr+), and JSR to an odd address is an ADDRESS ERROR on
         ;; 68k -- unlike PowerPC, whose BCCTR ignores the low two bits.  AND
         ;; rather than SUBQ #3 so an untagged (4-aligned) target passes through
         ;; unchanged too.
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-andi buf +68k-d0+ #xFFFFFFFC)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-emit-jsr-an-ind buf +68k-a0+)))

      (#.+op-ret+
       (m68k-emit-epilogue buf))

      (#.+op-tailcall+
       ;; Target operand is the bytecode offset of the called function.
       (let* ((target-offset (first operands))
              (label (gethash target-offset label-map)))
         ;; Tear down frame, then jump
         ;; Restore callee-saved regs -- EXACTLY the epilogue's mask.  This
         ;; one also restored A2/A3/A4 (bits 10-12), which the prologue never
         ;; saved, so every tail call loaded VA/VL/VN from the frame below the
         ;; saved D4-D7: the allocation pointer, limit and NIL replaced with
         ;; whatever the frame held.
         (let ((mask (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7))))
           (m68k-emit-movem-from-postinc buf mask))
         (m68k-emit-unlk buf +68k-a6+)
         ;; Branch to target (BRA, not BSR — this is a tail call)
         (m68k-emit-bra buf label)))

      ;;; --- GC / Allocation ---
      (#.+op-alloc-cons+
       (let ((vd (first operands)))
         (m68k-emit-alloc-mark buf 2)   ; FIRST: clobbers D0/A0
         ;; Tag A2 (VA) as cons pointer
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-cons+)
         ;; Bump A2 by 16 -- the same reason as +op-cons+.
         (m68k-emit-lea-disp buf +68k-a2+ 16 +68k-a2+)
         (m68k-store-vreg buf vd +68k-d0+)))

      ((#.+op-gc-check+ #.+op-gc-check-n+ #.+op-gc-check-r+)
       ;; :GC-CHECK-N and :GC-CHECK-R share this clause.  They carry an
       ;; allocation SIZE (a constant, or a runtime value) so a back end can
       ;; check `VA + n < VL` rather than `VA < VL`.  None of x64, i386 or
       ;; aarch64 uses the size either -- translate-i386 routes all three to
       ;; the same plain VA-vs-VL comparison -- so doing the same here matches
       ;; the reference back ends exactly.  What it replaces is worse than an
       ;; imprecise check: RISC-V/PPC/68k TRAPPED on these opcodes, and
       ;; make-array emits :gc-check-n, so no array could be allocated at all.
       ;; Room remains while VA (A2, alloc pointer) is below VL (A3, limit).
       ;;
       ;; THE OPERAND ORDER WAS REVERSED AND THE TEST RAN BACKWARDS.
       ;; m68k-emit-cmpa-an takes (src dst) and emits CMPA.L src,dst, which
       ;; computes dst - src; carry is the BORROW, so it is set when dst <
       ;; src.  Comparing (A2, A3) therefore computed A3 - A2 and set carry
       ;; when VL < VA -- out of memory -- and the branch treated that as the
       ;; OK case.  Every allocation with room available fell through to the
       ;; ILLEGAL instead: `(cons 42 0)` trapped on the first cons, which is
       ;; why 68k passed arithmetic and recursion but could not build a pair.
       ;; Comparing (A3, A2) computes A2 - A3, so carry set means VA < VL.
       ;; Comparing (A3, A2) computes A2 - A3, so carry set means VA < VL.
       ;; Hosted, a full heap calls the collector; bare, it is still ILLEGAL.
       (m68k-emit-gc-check buf))

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

      ;;; --- Arrays ---
      ;; Object layout as obj-ref already uses it here: tag 9 (+tag-object+),
      ;; header longword at obj-9, element k at obj-9 + (1+k)*4.  The index
      ;; arrives TAGGED (2k), so k*4 == tagged*2 and the element sits at
      ;; obj + tagged*2 - 5.  D0/D1 are the reserved scratch, A0 the address
      ;; scratch, A2 the alloc pointer (VA).
      (#.+op-alloc-array+
       ;; (alloc-array Vd Vcount) — Vcount is UNTAGGED (the compiler SAR'd it).
       (let ((vd (first operands))
             (vcount (second operands)))
         (m68k-emit-alloc-mark buf 1)   ; FIRST: clobbers D0/A0
         ;; D1 = count (kept); D0 = header, stored at (A2).
         (m68k-load-vreg buf +68k-d1+ vcount)
         (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-lsl-imm buf +68k-d0+ 8)
         (m68k-emit-ori buf +68k-d0+ #x32)             ; array subtag
         (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
         ;; D0 = VA | 9 — the RESULT, taken before the pointer is bumped.
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-object+)
         (m68k-store-vreg buf vd +68k-d0+)
         ;; D1 = align16((count + 1) * 4), then A2 += D1.
         (m68k-emit-addi buf +68k-d1+ 1)
         (m68k-emit-lsl-imm buf +68k-d1+ 2)
         (m68k-emit-addi buf +68k-d1+ 15)
         (m68k-emit-andi buf +68k-d1+ (logand (lognot 15) #xFFFFFFFF))
         (m68k-emit-zero-and-bump buf)))

      (#.+op-aref+
       (let ((vd (first operands))
             (vobj (second operands))
             (vidx (third operands)))
         (m68k-load-vreg buf +68k-d0+ vidx)
         (m68k-emit-lsl-imm buf +68k-d0+ 1)            ; tagged*2 = k*4
         (if (= vobj +vreg-vfp+)
             ;; VARIABLE-INDEX FRAME SLOT load: the &rest prologue's loop
             ;; (emit-rest-prologue) walks frame slots with `aref VFP idx'.  The
             ;; constant-index VFP case in OBJ-REF addresses frame slots, not an
             ;; object; without the same arm here the loop read object-formula
             ;; addresses off the frame pointer, and every &rest call with more
             ;; than three rest arguments in an image built since adebd4f got a
             ;; garbage list (x64 and aarch64 had this arm already).
             ;; Slot N is at A6 + base - N*4.
             (progn
               (m68k-emit-neg buf +68k-d0+)                 ; D0 = -k*4
               (m68k-emit-move-an-dn buf +68k-a6+ +68k-d1+)
               (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)  ; D0 = A6 - k*4
               (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
               (m68k-emit-move-disp-dn buf +68k-a0+ +68k-frame-slot-base+ +68k-d0+))
             (progn
               (m68k-load-vreg buf +68k-d1+ vobj)
               (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)   ; D0 += Vobj
               (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
               (m68k-emit-move-disp-dn buf +68k-a0+ (- 4 +tag-object+) +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-aset+
       ;; (aset Vobj Vidx Vs).  The address is finished into A0 FIRST, so the
       ;; value can then own D0 without either clobbering the other.
       (let ((vobj (first operands))
             (vidx (second operands))
             (vs (third operands)))
         (m68k-load-vreg buf +68k-d0+ vidx)
         (m68k-emit-lsl-imm buf +68k-d0+ 1)
         (m68k-load-vreg buf +68k-d1+ vobj)
         (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a0+ (- 4 +tag-object+))))

      (#.+op-array-len+
       ;; count = (header >> 8) & 0xFFFFFF, returned TAGGED.
       (let ((vd (first operands))
             (vobj (second operands)))
         (m68k-load-vreg buf +68k-d0+ vobj)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-emit-move-disp-dn buf +68k-a0+ (- +tag-object+) +68k-d0+)
         (m68k-emit-lsr-imm buf +68k-d0+ 8)
         (m68k-emit-andi buf +68k-d0+ #xFFFFFF)
         (m68k-emit-lsl-imm buf +68k-d0+ 1)            ; tag as fixnum
         (m68k-store-vreg buf vd +68k-d0+)))

      ;;; --- Byte vectors and strings ---
      ;; Payload starts right after the header longword, at obj - 9 + 4, i.e.
      ;; obj - 5.  The 68k has no byte move with a displacement here, so the
      ;; -5 is folded into the address before it reaches A0.  Shapes mirror
      ;; translate-i386, including which operands arrive TAGGED.
      (#.+op-alloc-u8+
       (let ((vd (first operands))
             (vcount (second operands)))
         (m68k-emit-alloc-mark buf 1)   ; FIRST: clobbers D0/A0
         (m68k-load-vreg buf +68k-d1+ vcount)
         (m68k-emit-asr-imm buf +68k-d1+ 1)              ; N = count >> 1
         (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-lsl-imm buf +68k-d0+ 8)
         (m68k-emit-ori buf +68k-d0+ #x11)               ; u8-vector subtag
         (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
         ;; result = VA | 9, taken BEFORE the pointer is bumped
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-object+)
         (m68k-store-vreg buf vd +68k-d0+)
         ;; bytes = align16(N + 4)
         (m68k-emit-addi buf +68k-d1+ 19)                ; N + 4 + 15
         (m68k-emit-andi buf +68k-d1+ (logand (lognot 15) #xFFFFFFFF))
         (m68k-emit-zero-and-bump buf)))

      (#.+op-alloc-string+
       ;; One character CODE per WORD; count already UNTAGGED by the compiler.
       (let ((vd (first operands))
             (vcount (second operands)))
         (m68k-emit-alloc-mark buf 1)   ; FIRST: clobbers D0/A0
         (m68k-load-vreg buf +68k-d1+ vcount)
         (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-lsl-imm buf +68k-d0+ 8)
         (m68k-emit-ori buf +68k-d0+ #x31)               ; string subtag
         (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-object+)
         (m68k-store-vreg buf vd +68k-d0+)
         ;; bytes = align16((count + 1) * 4)
         (m68k-emit-addi buf +68k-d1+ 1)
         (m68k-emit-lsl-imm buf +68k-d1+ 2)
         (m68k-emit-addi buf +68k-d1+ 15)
         (m68k-emit-andi buf +68k-d1+ (logand (lognot 15) #xFFFFFFFF))
         (m68k-emit-zero-and-bump buf)))

      (#.+op-u8-ref+
       (let ((vd (first operands))
             (varr (second operands))
             (vidx (third operands)))
         (m68k-load-vreg buf +68k-d0+ vidx)
         (m68k-emit-asr-imm buf +68k-d0+ 1)
         (m68k-load-vreg buf +68k-d1+ varr)
         (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-subi buf +68k-d0+ 5)                 ; tag 9 - header 4
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-emit-move-byte-an-ind-dn buf +68k-a0+ +68k-d0+)
         (m68k-emit-andi buf +68k-d0+ #xFF)              ; MOVE.B leaves the top
         (m68k-emit-lsl-imm buf +68k-d0+ 1)              ; tag as fixnum
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-u8-set+
       ;; (u8-set Varr Vidx Vval) — Vidx and Vval both TAGGED.  The address is
       ;; finished into A0 first, so the value can then own D0.
       (let ((varr (first operands))
             (vidx (second operands))
             (vval (third operands)))
         (m68k-load-vreg buf +68k-d0+ vidx)
         (m68k-emit-asr-imm buf +68k-d0+ 1)
         (m68k-load-vreg buf +68k-d1+ varr)
         (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-subi buf +68k-d0+ 5)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-load-vreg buf +68k-d0+ vval)
         (m68k-emit-asr-imm buf +68k-d0+ 1)
         (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a0+)))

      ;;; --- System area pointers ---
      ;; One-slot object, subtag #x16: header (1<<8)|#x16 then the raw address.
      (#.+op-sap-new+
       (let ((vd (first operands))
             (vaddr (second operands)))
         (m68k-emit-alloc-mark buf 1)   ; FIRST: clobbers D0/A0
         ;; Payload first — the allocation sequence below owns D0.
         (m68k-load-vreg buf +68k-d1+ vaddr)
         (m68k-emit-move-imm-dn buf #x116 +68k-d0+)
         (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)
         (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a2+ 4)
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-ori buf +68k-d0+ +tag-object+)
         (m68k-store-vreg buf vd +68k-d0+)
         (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
         (m68k-emit-addi buf +68k-d0+ 16)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a2+)))

      (#.+op-sap-addr+
       ;; Raw address out, TAGGED as a fixnum (as on x64/i386).
       (let ((vd (first operands))
             (vsap (second operands)))
         (m68k-load-vreg buf +68k-d0+ vsap)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-emit-move-disp-dn buf +68k-a0+ (- 4 +tag-object+) +68k-d0+)
         (m68k-emit-lsl-imm buf +68k-d0+ 1)
         (m68k-store-vreg buf vd +68k-d0+)))

      ;;; --- Calling-convention slots ---
      ;; These fell into the OTHERWISE branch below, which emits ILLEGAL.
      ;; :set-nargs precedes every call, so the first call in any image
      ;; executed an illegal instruction.
      ;; nargs is stored RAW; :get-nargs tags it (<<1) on the way out.
      (#.+op-set-nargs+
       (let ((n (logand (first operands) #xFF)))
         (m68k-emit-move-imm-dn buf n +68k-d0+)
         (m68k-emit-store-abs buf +68k-d0+ (m68k-nargs-addr))))

      (#.+op-get-nargs+
       (let ((vd (first operands)))
         (m68k-emit-load-abs buf +68k-d0+ (m68k-nargs-addr))
         (m68k-emit-lsl-imm buf +68k-d0+ 1)     ; tag as fixnum
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-set-cenv+
       (let ((vs (first operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-store-abs buf +68k-d0+ (m68k-cenv-addr))))

      (#.+op-get-cenv+
       (let ((vd (first operands)))
         (m68k-emit-load-abs buf +68k-d0+ (m68k-cenv-addr))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-set-mv-count+
       (let ((tagged (ash (first operands) 1)))
         (m68k-emit-move-imm-dn buf tagged +68k-d0+)
         (m68k-emit-store-abs buf +68k-d0+ (m68k-mvcount-addr))))

      (otherwise
       ;; Unknown opcode: ILLEGAL -- and RECORDED, so a build can say what is
       ;; missing instead of shipping an image that dies at the first one.
       (pushnew opcode *68k-unimpl-opcodes*)
       (m68k-emit-illegal buf)))))

;;; ============================================================
;;; Hosted collector
;;; ============================================================
;;;
;;; A Cheney copying collector in native 68k, the port of translate-arm32's
;;; ARM32-EMIT-GC-COLLECTOR (itself translate-riscv's), with ONE design change:
;;; NOTHING ABOUT THE HEAP'S SIZE IS A TRANSLATE-TIME CONSTANT.  The boot stub
;;; chooses the size when the process starts (--dynamic-space-size, else half
;;; of physical RAM, capped) and leaves the geometry in memory:
;;;
;;;   #x10000040 + 0x00  from_start      + 0x08  to_start    + 0x10  space_size
;;;              + 0x18  stack_base      + 0x20  gc_count
;;;
;;; Same offsets as mvm/compiler.lisp's +GC-OFF-*+, so the block reads the way
;;; every other port's does.  The collector reads it at entry, the overshoot
;;; margin is computed from space_size, and the kind map sits at a fixed address
;;; past the largest heap (+68K-GC-KIND-MAP+), so the stub has to store NOTHING
;;; else and one binary runs in a 1 MB heap or a 248 MB one.  (A first cut kept
;;; the bias and margin at #x10000F90/F98 -- which are aarch64's handler-helper
;;; slots, read by every CLI image.)
;;;
;;; Allocation marks are a KIND BYTE per 16-byte granule (1 = object start,
;;; 2 = cons), as on ARM32.  A conservative root must name an address in
;;; from-space whose kind byte matches its tag (cons tag 1 <-> 2, object tag 9
;;; <-> 1), or it is left alone.
;;;
;;; ROOTS: every register (the entry MOVEMs D0-D7/A0-A6 and the stack scan
;;; starts at that save frame, so a register holding a pointer is updated in
;;; place and restored updated), the stack up to stack_base, and the whole low
;;; block [#x10000000, from-space) -- globals, MV values, jmpbuf and handler
;;; frames -- so a slot added later is covered by construction.
;;;
;;; TO-SPACE IS SPLIT BY SHAPE: objects copied UP from to_start, conses DOWN
;;; from to_end, so the Cheney scan always knows what it is looking at and can
;;; skip leaf payloads.  The mutator then allocates in the gap.  Failure is
;;; loud: a message on stderr and exit status 3 (heap still full after a
;;; collection, or to-space overflow) -- not a SIGILL.
;;;
;;; Register plan inside the collector:  D4 from_start  D5 from_end
;;; D6 kind bias  A2 object free (up)  A3 cons low (down)  A5/A6 scan cursors,
;;; A4/D7 loop bounds.  SCAN (A0 = slot address) uses D0-D3 and A1 only,
;;; advances A2/A3 as it copies, preserves everything else, and returns RTS.
;;; A2/A3 are the mutator's VA/VL, which is why the collector can simply leave
;;; the new frontier in them.

(defconstant +68k-gc-meta+ #x10000040)
(defconstant +68k-gc-kind-map+ #x1FC00000
  "The kind map's FIXED address: just past the largest heap the stub will
   choose (#x10000000 + 248 MB + 4 MB guard), where ARM32's has always been.
   The stub maps only the S/16 bytes a heap of size S needs, so a small heap
   costs a small map, and the bias below stays a translate-time constant.")
(defconstant +68k-gc-kind-bias+ (- +68k-gc-kind-map+ (ash #x10000000 -4))
  "K: the kind byte of address A is at K + (A >> 4).")
(defconstant +68k-gc-margin-max+ #x100000
  "The collector leaves min(1 MB, a quarter of a semispace) free below VL --
   COMPUTED from space_size at each collection, so it needs no stored word.")

(defvar *68k-gc-poison* nil
  "DEBUG, as *ARM32-GC-POISON*: every collection overwrites the semispace it
   evacuated with #x00000BA9, so a pointer the collector failed to update
   faults on its first read instead of reading the stale copy.  Set by
   build-68k-cli.lisp from MODUS_68K_GC_POISON=1.  Off: byte-identical.")

(defvar *68k-gc-label* nil
  "Label of the hosted collector while translating a hosted module, else NIL
   (then :gc-check is the old ILLEGAL and no allocation marks are emitted).")

(defun m68k-emit-adda-dn (buf dn an)
  "ADDA.L Dn, An"
  (m68k-emit-word buf (logior (ash #b1101 12) (ash (logand an 7) 9)
                              (ash #b111 6) (ash +ea-mode-dn+ 3) (logand dn 7))))

(defun m68k-emit-move-postinc-postinc (buf src-an dst-an)
  "MOVE.L (As)+, (Ad)+"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-postinc+ dst-an
                                             +ea-mode-an-postinc+ src-an)))

(defun m68k-emit-move-byte-postinc-dn (buf an dn)
  "MOVE.B (An)+, Dd"
  (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn +ea-mode-an-postinc+ an)))

(defun m68k-emit-cmpi-byte (buf dn imm8)
  "CMPI.B #imm8, Dn"
  (m68k-emit-word buf (logior (ash #b00001100 8) (ash 0 6)
                              (ash +ea-mode-dn+ 3) (logand dn 7)))
  (m68k-emit-word buf (logand imm8 #xFF)))

(defun m68k-emit-bsr-long (buf label-id)
  "BSR.L target -- 68020+, 32-bit displacement, so a gc-check anywhere in an
   image of any size reaches the collector emitted after the last function."
  (m68k-emit-word buf #x61FF)
  (let ((disp-pos (m68k-buffer-position buf)))
    (m68k-emit-word buf 0)
    (push (list (1- (m68k-buffer-word-count buf)) label-id :disp32 (+ disp-pos 2))
          (m68k-buffer-fixups buf))
    (m68k-emit-word buf 0)))

(defun m68k-emit-zero-and-bump (buf)
  "ZERO the D1 bytes (16-aligned, >= 16) of the object at VA past its header,
   then VA += D1.  Clobbers D0 and A0.  Every allocation site zeroes (the alloc
   zeroing contract): alloc-array, alloc-u8 and alloc-string here did not, and
   the bignum kernel's (MAKE-ARRAY N) digit vectors then read whatever an
   earlier collection had left in reused memory -- harmless until the second
   collection, the first to allocate over used space, after which BIGNUM-ASH
   of #xFF by 24 came back with junk low bits, odd words reached conses, and
   the collector took them for forwarding pointers."
  (let ((zl (mvm-make-label)))
    (m68k-emit-lea-disp buf +68k-a2+ 4 +68k-a0+)
    (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
    (m68k-emit-lsr-imm buf +68k-d0+ 2)
    (m68k-emit-subq buf +68k-d0+ 1)                       ; longwords past header
    (m68k-emit-label buf zl)
    (m68k-emit-word buf #x4298)                           ; CLR.L (A0)+
    (m68k-emit-subq buf +68k-d0+ 1)
    (m68k-emit-bne buf zl)
    (m68k-emit-adda-dn buf +68k-d1+ +68k-a2+)))

(defun m68k-emit-alloc-mark (buf kind)
  "Mark the allocation about to start at VA (A2): KIND 1 = object, 2 = cons.
   Hosted with a collector only.  Clobbers D0 and A0, so it is emitted FIRST in
   every allocation arm, before either holds an operand."
  (when *68k-gc-label*
    (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
    (m68k-emit-lsr-imm buf +68k-d0+ 4)
    (m68k-emit-move-imm-an buf +68k-gc-kind-bias+ +68k-a0+) ; A0 = K
    (m68k-emit-adda-dn buf +68k-d0+ +68k-a0+)
    (m68k-emit-moveq buf +68k-d0+ kind)
    (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a0+)))

(defun m68k-emit-gc-check (buf)
  "Room while VA (A2) < VL (A3); else call the collector (hosted) or ILLEGAL."
  (let ((ok-label (mvm-make-label)))
    (m68k-emit-cmpa-an buf +68k-a3+ +68k-a2+)              ; A2 - A3
    (m68k-emit-bcc-word buf +68k-cc-cs+ ok-label)           ; CS = VA < VL
    (if *68k-gc-label*
        (m68k-emit-bsr-long buf *68k-gc-label*)
        (m68k-emit-illegal buf))
    (m68k-emit-label buf ok-label)))

(defun m68k-emit-die (buf message status)
  "write(2, MESSAGE) then exit(STATUS).  The text is pushed onto the stack a
   longword at a time, so it needs no data section and no PC-relative address.
   Does not return."
  (let* ((bytes (map 'list #'char-code message))
         (padded (append bytes (make-list (mod (- (length bytes)) 4) :initial-element 0))))
    (loop for i from (- (length padded) 4) downto 0 by 4
          do (m68k-emit-move-imm-dn buf (logior (ash (nth i padded) 24)
                                                (ash (nth (+ i 1) padded) 16)
                                                (ash (nth (+ i 2) padded) 8)
                                                (nth (+ i 3) padded))
                                    +68k-d0+)
             (m68k-emit-push-dn buf +68k-d0+))
    (m68k-emit-moveq buf +68k-d1+ 2)                        ; fd
    (m68k-emit-move-an-dn buf +68k-a7+ +68k-d2+)            ; buf
    (m68k-emit-move-imm-dn buf (length bytes) +68k-d3+)     ; len
    (m68k-emit-move-imm-dn buf +68k-linux-sys-write+ +68k-d0+)
    (m68k-emit-trap buf 0)
    (m68k-emit-moveq buf +68k-d1+ status)
    (m68k-emit-move-imm-dn buf +68k-linux-sys-exit+ +68k-d0+)
    (m68k-emit-trap buf 0)))

(defun m68k-emit-kind-addr (buf dst-dn addr-dn)
  "DST-DN = the kind-byte address of the granule at ADDR-DN; K is in D6."
  (unless (= dst-dn addr-dn) (m68k-emit-move-dn-dn buf addr-dn dst-dn))
  (m68k-emit-lsr-imm buf dst-dn 4)
  (m68k-emit-add-dn-dn buf +68k-d6+ dst-dn))

(defun m68k-emit-object-size (buf dst hdr)
  "DST = the byte size of the object whose header word is in HDR, aligned to
   16.  Mirrors every 68k allocation site: a u8 vector (#x11) counts bytes
   after its 4-byte header, everything else (count + 1) longwords.  Clobbers
   only DST."
  (let ((words (mvm-make-label)) (al (mvm-make-label)))
    (m68k-emit-move-dn-dn buf hdr dst)
    (m68k-emit-andi buf dst #xFF)
    (m68k-emit-cmpi buf dst +subtag-u8-vector+)
    (m68k-emit-bne buf words)
    (m68k-emit-move-dn-dn buf hdr dst)
    (m68k-emit-lsr-imm buf dst 8)
    (m68k-emit-addq buf dst 4)
    (m68k-emit-bra buf al)
    (m68k-emit-label buf words)
    (m68k-emit-move-dn-dn buf hdr dst)
    (m68k-emit-lsr-imm buf dst 8)
    (m68k-emit-addq buf dst 1)
    (m68k-emit-lsl-imm buf dst 2)
    (m68k-emit-label buf al)
    (m68k-emit-addi buf dst 15)
    (m68k-emit-andi buf dst (logand (lognot 15) #xFFFFFFFF))))

(defparameter *68k-gc-leaf-subtags* '(#x10 #x11 #x12 #x14 #x16 #x60 #x64 #x65 #x66)
  "Objects whose payload holds no pointers: copied, never scanned.  ARM32's
   list exactly.")

(defun m68k-emit-gc-collector (buf)
  "Emit the hosted collector at *68K-GC-LABEL*.  Entered by BSR.L from
   :gc-check; every register is preserved except the new VA/VL it installs."
  (let ((scan (mvm-make-label))
        (meta +68k-gc-meta+)
        (save #xFFFE)                     ; D0-D7/A0-A6, predecrement order
        (restore #x7FFF))
    (flet ((lbl () (mvm-make-label))
           (here (l) (m68k-emit-label buf l))
           (root-range ()
             ;; for (A5 = A5; A5 < D7; A5 += 4) scan(A5)
             (let ((top (mvm-make-label)) (out (mvm-make-label)))
               (m68k-emit-label buf top)
               (m68k-emit-cmpa buf +68k-a5+ +68k-d7+)              ; A5 - D7
               (m68k-emit-bcc-word buf +68k-cc-cc+ out)
               (m68k-emit-move-an-an buf +68k-a5+ +68k-a0+)
               (m68k-emit-bsr buf scan)
               (m68k-emit-addq-an buf +68k-a5+ 4)
               (m68k-emit-bra buf top)
               (m68k-emit-label buf out))))
      (here *68k-gc-label*)
      (m68k-emit-movem-to-predec buf save)
      ;; ---- geometry, all of it read from memory ----
      (m68k-emit-move-imm-an buf meta +68k-a1+)
      (m68k-emit-move-disp-dn buf +68k-a1+ #x00 +68k-d4+)        ; from_start
      (m68k-emit-move-disp-dn buf +68k-a1+ #x10 +68k-d0+)        ; space_size
      (m68k-emit-move-dn-dn buf +68k-d4+ +68k-d5+)
      (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d5+)                ; from_end
      (m68k-emit-move-disp-an buf +68k-a1+ #x08 +68k-a2+)        ; object free = to_start
      (m68k-emit-move-disp-dn buf +68k-a1+ #x08 +68k-d1+)
      (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d1+)
      (m68k-emit-move-dn-an buf +68k-d1+ +68k-a3+)               ; cons low = to_end
      (m68k-emit-move-imm-dn buf +68k-gc-kind-bias+ +68k-d6+)    ; K
      ;; ---- roots: the stack from the save frame, then the low block ----
      (m68k-emit-move-an-an buf +68k-a7+ +68k-a5+)
      (m68k-emit-move-disp-dn buf +68k-a1+ #x18 +68k-d7+)        ; stack_base
      (root-range)
      (m68k-emit-move-imm-an buf #x10000000 +68k-a5+)
      ;; the low block ends where the FIRST semispace starts, whichever one is
      ;; from-space now: min(from_start, to_start).  TO_START FROM THE BLOCK,
      ;; NOT A2: the stack roots above have already copied objects, so A2 is
      ;; the to-space FREE pointer by now.  Using it made the range run into
      ;; to-space at every collection whose to-space is the LOW semispace (the
      ;; second, fourth...), scanning the objects just copied -- byte-vector
      ;; payloads included -- as conservative roots: a bytecode word that
      ;; happened to spell a live from-space cons was "forwarded" in place, and
      ;; the interpreter then decoded garbage.
      (let ((have (lbl)))
        (m68k-emit-move-dn-dn buf +68k-d4+ +68k-d7+)
        (m68k-emit-move-imm-an buf meta +68k-a0+)
        (m68k-emit-move-disp-dn buf +68k-a0+ #x08 +68k-d0+)       ; to_start
        (m68k-emit-cmp-dn-dn buf +68k-d0+ +68k-d7+)               ; from_start - to_start
        (m68k-emit-bcc-word buf +68k-cc-cs+ have)
        (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d7+)
        (here have))
      (root-range)
      ;; ---- Cheney: A5 = object scan (up), A6 = cons scan (down) ----
      (m68k-emit-move-imm-an buf meta +68k-a1+)
      (m68k-emit-move-disp-an buf +68k-a1+ #x08 +68k-a5+)          ; to_start
      ;; A6 = to_end, NOT the current A3: the roots have already copied conses
      ;; down from to_end, and those need their cars and cdrs scanned too.
      (m68k-emit-move-disp-dn buf +68k-a1+ #x10 +68k-d1+)
      (m68k-emit-move-disp-dn buf +68k-a1+ #x08 +68k-d0+)
      (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
      (m68k-emit-move-dn-an buf +68k-d0+ +68k-a6+)
      (let ((ch (lbl)) (obj (lbl)) (cons (lbl)) (cdone (lbl))
            (leaf (lbl)) (sl (lbl)) (tst-cons (lbl)))
        (here ch)
        (m68k-emit-cmpa-an buf +68k-a2+ +68k-a5+)                ; A5 - A2
        (m68k-emit-bcc-word buf +68k-cc-cc+ tst-cons)
        (m68k-emit-bra buf obj)
        (here tst-cons)
        (m68k-emit-cmpa-an buf +68k-a6+ +68k-a3+)                ; A3 - A6
        (m68k-emit-bcc-word buf +68k-cc-cs+ cons)
        (m68k-emit-bra buf cdone)
        ;; -- one object at A5 --
        (here obj)
        (m68k-emit-move-an-ind-dn buf +68k-a5+ +68k-d3+)         ; header
        (m68k-emit-object-size buf +68k-d0+ +68k-d3+)
        (m68k-emit-move-an-dn buf +68k-a5+ +68k-d7+)
        (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d7+)              ; D7 = next
        (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d1+)
        (m68k-emit-andi buf +68k-d1+ #xFF)
        (dolist (st *68k-gc-leaf-subtags*)
          (m68k-emit-cmpi buf +68k-d1+ st)
          (m68k-emit-beq buf leaf))
        (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d1+)
        (m68k-emit-lsr-imm buf +68k-d1+ 8)                       ; count
        (m68k-emit-lsl-imm buf +68k-d1+ 2)
        (m68k-emit-addq buf +68k-d1+ 4)
        (m68k-emit-move-an-dn buf +68k-a5+ +68k-d0+)
        (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d1+)
        (m68k-emit-move-dn-an buf +68k-d1+ +68k-a4+)             ; A4 = slot end
        (m68k-emit-addq-an buf +68k-a5+ 4)
        (here sl)
        (m68k-emit-cmpa-an buf +68k-a4+ +68k-a5+)                ; A5 - A4
        (m68k-emit-bcc-word buf +68k-cc-cc+ leaf)
        (m68k-emit-move-an-an buf +68k-a5+ +68k-a0+)
        (m68k-emit-bsr buf scan)
        (m68k-emit-addq-an buf +68k-a5+ 4)
        (m68k-emit-bra buf sl)
        (here leaf)
        (m68k-emit-move-dn-an buf +68k-d7+ +68k-a5+)             ; A5 = next
        (m68k-emit-bra buf ch)
        ;; -- one cons below A6 --
        (here cons)
        (m68k-emit-lea-disp buf +68k-a6+ -16 +68k-a6+)
        (m68k-emit-move-an-an buf +68k-a6+ +68k-a0+)
        (m68k-emit-bsr buf scan)
        (m68k-emit-lea-disp buf +68k-a6+ 4 +68k-a0+)
        (m68k-emit-bsr buf scan)
        (m68k-emit-bra buf ch)
        (here cdone))
      ;; ---- clear the kind bytes of the evacuated semispace [D4, D5) ----
      (let ((zl (lbl)) (zd (lbl)))
        (m68k-emit-kind-addr buf +68k-d0+ +68k-d4+)
        (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
        (m68k-emit-kind-addr buf +68k-d1+ +68k-d5+)
        (m68k-emit-moveq buf +68k-d2+ 0)
        (here zl)
        (m68k-emit-cmpa buf +68k-a0+ +68k-d1+)                   ; A0 - D1
        (m68k-emit-bcc-word buf +68k-cc-cc+ zd)
        (m68k-emit-move-dn-an-ind buf +68k-d2+ +68k-a0+)
        (m68k-emit-addq-an buf +68k-a0+ 4)
        (m68k-emit-bra buf zl)
        (here zd))
      (when *68k-gc-poison*
        (let ((pl (lbl)) (pd (lbl)))
          (m68k-emit-move-dn-an buf +68k-d4+ +68k-a0+)
          (m68k-emit-move-imm-dn buf #x00000BA9 +68k-d2+)
          (here pl)
          (m68k-emit-cmpa buf +68k-a0+ +68k-d5+)                 ; A0 - D5
          (m68k-emit-bcc-word buf +68k-cc-cc+ pd)
          (m68k-emit-move-dn-an-ind buf +68k-d2+ +68k-a0+)
          (m68k-emit-addq-an buf +68k-a0+ 4)
          (m68k-emit-bra buf pl)
          (here pd)))
      ;; ---- swap the semispaces, count the collection ----
      (m68k-emit-move-imm-an buf meta +68k-a1+)
      (m68k-emit-move-disp-dn buf +68k-a1+ #x08 +68k-d0+)
      (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ #x00)        ; from = old to
      (m68k-emit-move-dn-disp buf +68k-d4+ +68k-a1+ #x08)        ; to = old from
      (m68k-emit-move-disp-dn buf +68k-a1+ #x20 +68k-d0+)
      (m68k-emit-addq buf +68k-d0+ 1)
      (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ #x20)
      ;; ---- new VA = object free, VL = cons low - margin, into the frame ----
      (let ((exhausted (lbl)))
        ;; margin = min(1 MB, space_size / 4), 16-aligned -- the stub's rule
        (let ((small (lbl)))
          (m68k-emit-move-disp-dn buf +68k-a1+ #x10 +68k-d0+)
          (m68k-emit-lsr-imm buf +68k-d0+ 2)
          (m68k-emit-andi buf +68k-d0+ (logand (lognot 15) #xFFFFFFFF))
          (m68k-emit-cmpi buf +68k-d0+ +68k-gc-margin-max+)
          (m68k-emit-bcc-word buf +68k-cc-ls+ small)
          (m68k-emit-move-imm-dn buf +68k-gc-margin-max+ +68k-d0+)
          (here small))
        (m68k-emit-move-an-dn buf +68k-a3+ +68k-d1+)
        (m68k-emit-sub-dn-dn buf +68k-d0+ +68k-d1+)              ; D1 = VL
        (m68k-emit-cmpa buf +68k-a2+ +68k-d1+)                   ; A2 - VL
        (m68k-emit-bcc-word buf +68k-cc-cc+ exhausted)
        (m68k-emit-move-an-disp buf +68k-a2+ +68k-a7+ 40)        ; saved A2 (VA)
        (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a7+ 44)        ; saved A3 (VL)
        (m68k-emit-movem-from-postinc buf restore)
        (m68k-emit-rts buf)
        (here exhausted)
        (m68k-emit-die buf (format nil "modus: heap exhausted (raise --dynamic-space-size)~%") 3))
      ;; ================= SCAN (A0 = slot address) =================
      (let ((ret (lbl)) (isptr (lbl)) (kobj (lbl)) (kok (lbl)) (notfwd (lbl))
            (obj (lbl)) (ovf (lbl)) (cl (lbl)))
        (here scan)
        (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
        (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
        (m68k-emit-andi buf +68k-d1+ 15)                         ; tag
        (m68k-emit-cmpi buf +68k-d1+ 1)
        (m68k-emit-beq buf isptr)
        (m68k-emit-cmpi buf +68k-d1+ 9)
        (m68k-emit-bne buf ret)
        (here isptr)
        (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)
        (m68k-emit-andi buf +68k-d2+ (logand (lognot 15) #xFFFFFFFF)) ; address
        (m68k-emit-cmp-dn-dn buf +68k-d4+ +68k-d2+)               ; D2 - from_start
        (m68k-emit-bcc-word buf +68k-cc-cs+ ret)
        (m68k-emit-cmp-dn-dn buf +68k-d5+ +68k-d2+)               ; D2 - from_end
        (m68k-emit-bcc-word buf +68k-cc-cc+ ret)
        ;; a real start of the right kind, or not a root at all
        (m68k-emit-kind-addr buf +68k-d3+ +68k-d2+)
        (m68k-emit-move-dn-an buf +68k-d3+ +68k-a1+)
        (m68k-emit-moveq buf +68k-d3+ 0)
        (m68k-emit-move-byte-an-ind-dn buf +68k-a1+ +68k-d3+)
        (m68k-emit-cmpi buf +68k-d1+ 1)
        (m68k-emit-bne buf kobj)
        (m68k-emit-cmpi buf +68k-d3+ 2)
        (m68k-emit-bne buf ret)
        (m68k-emit-bra buf kok)
        (here kobj)
        (m68k-emit-cmpi buf +68k-d3+ 1)
        (m68k-emit-bne buf ret)
        (here kok)
        (m68k-emit-move-dn-an buf +68k-d2+ +68k-a1+)              ; A1 = old base
        (m68k-emit-move-an-ind-dn buf +68k-a1+ +68k-d3+)          ; first word
        (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d0+)
        (m68k-emit-andi buf +68k-d0+ 15)
        (m68k-emit-cmpi buf +68k-d0+ 15)
        (m68k-emit-bne buf notfwd)
        ;; already forwarded: (fwd & ~15) | tag
        (m68k-emit-andi buf +68k-d3+ (logand (lognot 15) #xFFFFFFFF))
        (m68k-emit-or-dn-dn buf +68k-d1+ +68k-d3+)
        (m68k-emit-move-dn-an-ind buf +68k-d3+ +68k-a0+)
        (m68k-emit-rts buf)
        (here notfwd)
        (m68k-emit-cmpi buf +68k-d1+ 1)
        (m68k-emit-bne buf obj)
        ;; -- cons: 16 bytes down from A3 --
        (m68k-emit-lea-disp buf +68k-a3+ -16 +68k-a3+)
        (m68k-emit-cmpa-an buf +68k-a2+ +68k-a3+)                 ; A3 - A2
        (m68k-emit-bcc-word buf +68k-cc-cs+ ovf)
        (m68k-emit-move-dn-disp buf +68k-d3+ +68k-a3+ 0)          ; car
        (m68k-emit-move-disp-dn buf +68k-a1+ 4 +68k-d0+)
        (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a3+ 4)          ; cdr
        (m68k-emit-move-an-dn buf +68k-a3+ +68k-d0+)
        (m68k-emit-ori buf +68k-d0+ 15)
        (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a1+)          ; forward
        (m68k-emit-move-an-dn buf +68k-a3+ +68k-d0+)
        (m68k-emit-ori buf +68k-d0+ 1)
        (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)          ; update slot
        (m68k-emit-move-an-dn buf +68k-a3+ +68k-d0+)
        (m68k-emit-kind-addr buf +68k-d0+ +68k-d0+)
        (m68k-emit-move-dn-an buf +68k-d0+ +68k-a1+)
        (m68k-emit-moveq buf +68k-d0+ 2)
        (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a1+)
        (m68k-emit-rts buf)
        ;; -- object: header in D3, old base in A1 and D2 --
        (here obj)
        (m68k-emit-object-size buf +68k-d0+ +68k-d3+)
        ;; a header whose object cannot lie inside from-space is a false root
        (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d1+)
        (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d1+)
        (m68k-emit-cmp-dn-dn buf +68k-d5+ +68k-d1+)               ; end - from_end
        (m68k-emit-bcc-word buf +68k-cc-hi+ ret)
        (m68k-emit-cmp-dn-dn buf +68k-d2+ +68k-d1+)               ; end - base
        (m68k-emit-bcc-word buf +68k-cc-cs+ ret)                  ; wrapped
        ;; room below the cons frontier, else to-space overflow
        (m68k-emit-move-an-dn buf +68k-a2+ +68k-d1+)
        (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d1+)
        (m68k-emit-cmpa buf +68k-a3+ +68k-d1+)                    ; A3 - (A2 + size)
        (m68k-emit-bcc-word buf +68k-cc-cs+ ovf)
        (m68k-emit-move-an-dn buf +68k-a2+ +68k-d3+)              ; D3 = new base
        (m68k-emit-lsr-imm buf +68k-d0+ 2)                        ; longwords (>= 4)
        (here cl)
        (m68k-emit-move-postinc-postinc buf +68k-a1+ +68k-a2+)    ; A2 advances
        (m68k-emit-subq buf +68k-d0+ 1)
        (m68k-emit-bne buf cl)
        (m68k-emit-move-dn-an buf +68k-d2+ +68k-a1+)
        (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d0+)
        (m68k-emit-ori buf +68k-d0+ 15)
        (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a1+)          ; forward
        (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d0+)
        (m68k-emit-ori buf +68k-d0+ 9)
        (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)          ; update slot
        (m68k-emit-kind-addr buf +68k-d0+ +68k-d3+)
        (m68k-emit-move-dn-an buf +68k-d0+ +68k-a1+)
        (m68k-emit-moveq buf +68k-d0+ 1)
        (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a1+)
        (here ret)
        (m68k-emit-rts buf)
        (here ovf)
        (m68k-emit-die buf (format nil "modus: collector to-space overflow~%") 3)))))

;;; ============================================================
;;; Main Translation Entry Point
;;; ============================================================

(defvar *68k-unimpl-opcodes* nil
  "Opcodes (and, as #x1xxxx, hosted service traps) the last 68k translation had
   no arm for: each became an ILLEGAL.  Reset per translation.")

(defun m68k-unimplemented-report ()
  "Print, and return, what the last 68k translation could not translate."
  (when *68k-unimpl-opcodes*
    (format t "~&  *** 68k translator gaps (ILLEGAL emitted; #x1xxxx = trap xxxx): ~{#x~2,'0X~^ ~} ***~%"
            (sort (copy-list *68k-unimpl-opcodes*) #'<)))
  *68k-unimpl-opcodes*)

(defun translate-mvm-to-68k (bytecode function-table)
  (setf *68k-li-const-patches* nil
        *68k-unimpl-opcodes* nil)
  (translate-mvm-to-68k-1 bytecode function-table))

(defun translate-mvm-to-68k-1 (bytecode function-table)
  "Translate MVM bytecode to Motorola 68000 native code.
   BYTECODE is a (vector (unsigned-byte 8)).
   FUNCTION-TABLE maps function indices to bytecode offsets.
   Returns a byte vector of 68k big-endian machine code."
  (let* ((*68k-gc-label* (and *68k-linux-mode* (mvm-make-label)))
         (buf (make-m68k-buffer))
         (label-map (make-hash-table :test 'eql))
         (fn-entries nil)
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

    ;; Function-entry MVM offsets, for the alignment below.
    (setf fn-entries (make-hash-table :test 'eql))
    (when function-table
      (maphash (lambda (idx mvm-offset)
                 (declare (ignore idx))
                 (setf (gethash mvm-offset fn-entries) t))
               function-table))

    ;; Second pass: translate
    (setf pc 0)
    (loop while (< pc len)
          do (progn
               ;; FUNCTION ENTRIES ARE 16-ALIGNED.  +op-fn-addr+ ORs the function
               ;; tag 3 into an entry address, and everything that classifies a
               ;; value reads the low FOUR bits: an entry at 4/8/12 mod 16 tags
               ;; as 7/B/F -- not a function, and F is the collector's
               ;; forwarding tag.  This was 4-aligned, which passed r16-funcall
               ;; only while ADD2 happened to land on a 16-byte boundary; adding
               ;; allocation marks moved it and FUNCALL took an ILLEGAL.  ARM32,
               ;; x64, i386 and RISC-V align to 16 for the same reason.  Pad with
               ;; NOP -- harmless, a function never falls through (RTS/branch).
               (when (and function-table (gethash pc fn-entries))
                 (loop while (/= 0 (mod (m68k-buffer-position buf) 16))
                       do (m68k-emit-nop buf)))
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

    (when *68k-gc-label*
      (m68k-emit-gc-collector buf))

    ;; Resolve labels
    (m68k-fixup-labels buf)
    (m68k-unimplemented-report)

    ;; Report where each function actually LANDED — see the same change in
    ;; translate-ppc.lisp.  Without it cross.lisp guesses proportionally and
    ;; the entry jump lands mid-prologue as soon as there are two functions.
    (let ((fn-map (make-hash-table :test 'eql)))
      (when function-table
        (maphash (lambda (idx mvm-offset)
                   (declare (ignore idx))
                   (let* ((label (gethash mvm-offset label-map))
                          (pos (and label (gethash label (m68k-buffer-labels buf)))))
                     (when pos
                       (setf (gethash mvm-offset fn-map) pos))))
                 function-table))
      (values (m68k-buffer-to-bytes buf) fn-map))))

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

;;; ============================================================
;;; Double floats through the 68881/68882 coprocessor (on-chip on the 68040)
;;; ============================================================
;;;
;;; A boxed double is four TAGGED 16-bit chunks, slot k holding bits
;;; (63-16k)..(48-16k).  68k is big-endian, so chunk k is the WORD at byte 2k of
;;; the IEEE double: unbox writes the untagged chunks into a 16-byte stack
;;; scratch with MOVE.W and loads it with one FMOVE.D; box is FMOVE.D out, then
;;; each word back, tagged, into the new object -- the route PPC and RV32 take.
;;; FPU words are emitted raw: opword #xF200 | <ea>, then the command word.
;;; Only FP0/FP1 are used.

(defun m68k-emit-move-w-dn-disp (buf dn an disp)
  "MOVE.W Dd, d16(An)"
  (m68k-emit-word buf (m68k-encode-move-word 3 +ea-mode-an-disp+ an +ea-mode-dn+ dn))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-move-w-disp-dn (buf an disp dn)
  "MOVE.W d16(An), Dd -- the upper word of Dd is unchanged."
  (m68k-emit-word buf (m68k-encode-move-word 3 +ea-mode-dn+ dn +ea-mode-an-disp+ an))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-fmove-d-from-sp (buf fpn)
  "FMOVE.D (A7), FPn"
  (m68k-emit-word buf #xF217)
  (m68k-emit-word buf (logior #x5400 (ash fpn 7))))

(defun m68k-emit-fmove-d-to-sp (buf fpn)
  "FMOVE.D FPn, (A7)"
  (m68k-emit-word buf #xF217)
  (m68k-emit-word buf (logior #x7400 (ash fpn 7))))

(defun m68k-emit-fop (buf opmode src dst)
  "Register-to-register FPU op: FADD #x22, FSUB #x28, FMUL #x23, FDIV #x20,
   FINTRZ #x03.  DST op= SRC."
  (m68k-emit-word buf #xF200)
  (m68k-emit-word buf (logior (ash src 10) (ash dst 7) opmode)))

(defun m68k-emit-fmove-l-from-dn (buf dn fpn)
  "FMOVE.L Dn, FPn -- signed 32-bit integer to extended/double."
  (m68k-emit-word buf (logior #xF200 (logand dn 7)))
  (m68k-emit-word buf (logior #x4000 (ash fpn 7))))

(defun m68k-emit-fmove-l-to-dn (buf fpn dn)
  "FMOVE.L FPn, Dn -- to a signed 32-bit integer in the current rounding mode;
   callers FINTRZ first so the value is already integral and exact."
  (m68k-emit-word buf (logior #xF200 (logand dn 7)))
  (m68k-emit-word buf (logior #x6000 (ash fpn 7))))

(defun m68k-emit-fmove-s-from-an-ind (buf an fpn)
  "FMOVE.S (An), FPn -- a 4-byte IEEE single from memory, widened."
  (m68k-emit-word buf (logior #xF210 (logand an 7)))
  (m68k-emit-word buf (logior #x4400 (ash fpn 7))))

(defun m68k-emit-fmove-s-to-an-ind (buf fpn an)
  "FMOVE.S FPn, (An) -- rounded to single precision (FPCR's mode: nearest)."
  (m68k-emit-word buf (logior #xF210 (logand an 7)))
  (m68k-emit-word buf (logior #x6400 (ash fpn 7))))

(defun m68k-float-unbox (buf fpn)
  "Load the double whose TAGGED pointer is in A0 into FPn.  Clobbers D1."
  (m68k-emit-lea-disp buf +68k-a7+ -16 +68k-a7+)
  (loop for k from 0 to 3
        do (m68k-emit-move-disp-dn buf +68k-a0+ (- (* 4 (1+ k)) +tag-object+) +68k-d1+)
           (m68k-emit-lsr-imm buf +68k-d1+ 1)                 ; untag
           (m68k-emit-move-w-dn-disp buf +68k-d1+ +68k-a7+ (* 2 k)))
  (m68k-emit-fmove-d-from-sp buf fpn)
  (m68k-emit-lea-disp buf +68k-a7+ 16 +68k-a7+))

(defun m68k-float-box (buf fpn &optional (header (logior #x60 (ash 4 8))))
  "Box FPn as a fresh four-chunk float; its TAGGED pointer lands in D0.
   HEADER is (4<<8)|#x60 for a double (the default) or (4<<8)|#x64 for a
   single-float, whose payload is the double value already rounded to single
   (ARM32's representation).  Five longwords rounded to 32 bytes (16-aligned
   heap).  Clobbers D1."
  (m68k-emit-alloc-mark buf 1)          ; FIRST: clobbers D0/A0
  (m68k-emit-lea-disp buf +68k-a7+ -16 +68k-a7+)
  (m68k-emit-fmove-d-to-sp buf fpn)
  (m68k-emit-move-imm-dn buf header +68k-d1+)
  (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a2+ 0)            ; header at VA
  (loop for k from 0 to 3
        do (m68k-emit-move-imm-dn buf 0 +68k-d1+)
           (m68k-emit-move-w-disp-dn buf +68k-a7+ (* 2 k) +68k-d1+)
           (m68k-emit-lsl-imm buf +68k-d1+ 1)                 ; tag
           (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a2+ (* 4 (1+ k))))
  (m68k-emit-lea-disp buf +68k-a7+ 16 +68k-a7+)
  (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
  (m68k-emit-ori buf +68k-d0+ +tag-object+)
  (m68k-emit-lea-disp buf +68k-a2+ 32 +68k-a2+))

;;; ============================================================
;;; handler-case: SETJMP (#x0510) / LONGJMP (#x0511) / CLEAR-HANDLER (#x0512)
;;; ============================================================
;;;
;;; The protocol of translate-riscv / translate-ppc: one live jmpbuf, a stack of
;;; saved outer jmpbufs so handler-cases NEST, a depth cap (21) past which a
;;; push stores nothing and its matching pop is ABSORBED, and LONGJMP copying the
;;; jmpbuf aside before the pop overwrites it.
;;;
;;; JMPBUF = 7 longwords: A7, A6 (VFP), resume address, D4..D7 -- V2..V5, the
;;; callee-saved V-registers the prologue's MOVEM saves and whose epilogues a
;;; LONGJMP skips.  VA/VL/VN (A2..A4) deliberately absent; V0/V1 (D2/D3) are
;;; caller-saved.  Frames are 8 longwords (32 bytes, one spare) so a frame
;;; address is a shift.  Temporaries: D0, D1, A0, A1.
;;;
;;; WHERE: hosted, the shared contract block at the heap base (jmpbuf
;;; #x10000180 ... frames from #x10000408; 46 frames end at #x100009C8, clear of
;;; the globals at #x10000A00); bare, the same offsets in *68k-globals-base*'s
;;; block, which otherwise uses only #x00-#x0F.  Computed at translate time.

(defconstant +68k-jmpbuf-words+ 7)
(defparameter *68k-hstack-max-depth* 46
  "As many 32-byte frames as fit below the hosted globals at #x10000A00 (they
   end at #x100009C8).  21 overflowed under a real library load on ARM32, the
   same protocol -- see *ARM32-HSTACK-MAX-DEPTH*.")
(defun m68k-hbase ()           (if *68k-linux-mode* #x10000000 *68k-globals-base*))
(defun m68k-jmpbuf-addr ()     (+ (m68k-hbase) #x180))
(defun m68k-lj-scratch-addr () (+ (m68k-hbase) #x300))
(defun m68k-hcapped-addr ()    (+ (m68k-hbase) #x360))
(defun m68k-hdepth-addr ()     (+ (m68k-hbase) #x400))
(defun m68k-hframes-addr ()    (+ (m68k-hbase) #x408))

(defun m68k-emit-copy-words (buf from-an to-an)
  "Copy the 7 jmpbuf longwords from 0(FROM-AN) to 0(TO-AN) through D1."
  (dotimes (i +68k-jmpbuf-words+)
    (m68k-emit-move-disp-dn buf from-an (* 4 i) +68k-d1+)
    (m68k-emit-move-dn-disp buf +68k-d1+ to-an (* 4 i))))

(defun m68k-emit-frame-addr (buf dst-an depth-dn)
  "DST-AN = frames-base + DEPTH-DN * 32.  Clobbers D1."
  (m68k-emit-move-dn-dn buf depth-dn +68k-d1+)
  (m68k-emit-lsl-imm buf +68k-d1+ 5)
  (m68k-emit-addi buf +68k-d1+ (m68k-hframes-addr))
  (m68k-emit-move-dn-an buf +68k-d1+ dst-an))

(defun m68k-emit-handler-push (buf)
  "Stack the CURRENT jmpbuf.  Leaves D0 = 0 if a frame was stored, 1 if the
   push was CAPPED.  Clobbers D0, D1, A0, A1."
  (let ((capped (mvm-make-label))
        (done (mvm-make-label)))
    (m68k-emit-move-imm-an buf (m68k-hdepth-addr) +68k-a1+)
    (m68k-emit-move-disp-dn buf +68k-a1+ 0 +68k-d0+)        ; D0 = depth
    (m68k-emit-cmpi buf +68k-d0+ *68k-hstack-max-depth*)
    (m68k-emit-bge buf capped)
    (m68k-emit-frame-addr buf +68k-a0+ +68k-d0+)
    (m68k-emit-addi buf +68k-d0+ 1)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ 0)        ; depth++
    (m68k-emit-move-imm-an buf (m68k-jmpbuf-addr) +68k-a1+)
    (m68k-emit-copy-words buf +68k-a1+ +68k-a0+)
    (m68k-emit-move-imm-dn buf 0 +68k-d0+)                  ; stored
    (m68k-emit-bra buf done)
    (m68k-emit-label buf capped)
    (m68k-emit-move-imm-an buf (m68k-hcapped-addr) +68k-a1+)
    (m68k-emit-move-disp-dn buf +68k-a1+ 0 +68k-d0+)
    (m68k-emit-addi buf +68k-d0+ 1)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ 0)
    (m68k-emit-move-imm-dn buf 1 +68k-d0+)                  ; capped
    (m68k-emit-label buf done)))

(defun m68k-emit-handler-pop (buf)
  "Absorb a capped push, restore the top stacked frame into the jmpbuf, or ZERO
   the jmpbuf when the stack is empty.  Never touches D2 (V0), which holds the
   handler-case's result at a CLEAR-HANDLER.  Clobbers D0, D1, A0, A1."
  (let ((not-capped (mvm-make-label))
        (empty (mvm-make-label))
        (done (mvm-make-label)))
    (m68k-emit-move-imm-an buf (m68k-hcapped-addr) +68k-a1+)
    (m68k-emit-move-disp-dn buf +68k-a1+ 0 +68k-d0+)
    (m68k-emit-tst buf +68k-d0+)
    (m68k-emit-beq buf not-capped)
    (m68k-emit-subq buf +68k-d0+ 1)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ 0)
    (m68k-emit-bra buf done)
    (m68k-emit-label buf not-capped)
    (m68k-emit-move-imm-an buf (m68k-hdepth-addr) +68k-a1+)
    (m68k-emit-move-disp-dn buf +68k-a1+ 0 +68k-d0+)
    (m68k-emit-tst buf +68k-d0+)
    (m68k-emit-beq buf empty)
    (m68k-emit-subq buf +68k-d0+ 1)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ 0)        ; depth--
    (m68k-emit-frame-addr buf +68k-a0+ +68k-d0+)
    (m68k-emit-move-imm-an buf (m68k-jmpbuf-addr) +68k-a1+)
    (m68k-emit-copy-words buf +68k-a0+ +68k-a1+)
    (m68k-emit-bra buf done)
    (m68k-emit-label buf empty)                             ; zero the whole jmpbuf
    (m68k-emit-move-imm-an buf (m68k-jmpbuf-addr) +68k-a1+)
    (m68k-emit-move-imm-dn buf 0 +68k-d1+)
    (dotimes (i +68k-jmpbuf-words+)
      (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a1+ (* 4 i)))
    (m68k-emit-label buf done)))

(defun m68k-set-linux-mode (on)
  "Turn hosted mode on or off, moving the convention slots with it.  One
   function so the two cannot drift apart: an unmapped slot base is a SIGSEGV on
   the first :set-nargs, which precedes EVERY call, so the first function call in
   the image dies rather than something subtle later."
  (setf *68k-linux-mode* (and on t))
  (setf *68k-globals-base* (if on *68k-hosted-globals-base* #x00600000)))

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
