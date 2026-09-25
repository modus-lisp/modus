;;;; translate-ppc.lisp - MVM to PowerPC Translator (32-bit and 64-bit)
;;;;
;;;; Translates MVM virtual ISA bytecode to native PPC machine code.
;;;; Supports both PPC32 and PPC64 via the *ppc-64-bit* dynamic variable.
;;;;
;;;; PPC64 is big-endian with fixed 32-bit instruction words.
;;;; All instructions are naturally aligned on 4-byte boundaries.
;;;;
;;;; Register mapping (from target.lisp):
;;;;   V0  -> r3   (arg0, return value)
;;;;   V1  -> r4   (arg1)
;;;;   V2  -> r5   (arg2)
;;;;   V3  -> r6   (arg3)
;;;;   V4  -> r14  (callee-saved)
;;;;   V5  -> r15  (callee-saved)
;;;;   V6  -> r16  (callee-saved)
;;;;   V7  -> r17  (callee-saved)
;;;;   V8  -> r18  (callee-saved)
;;;;   V9-V15 -> stack spill
;;;;   VR  -> r3   (return value, aliases V0)
;;;;   VA  -> r19  (alloc pointer)
;;;;   VL  -> r20  (alloc limit)
;;;;   VN  -> r21  (NIL constant)
;;;;   VSP -> r1   (stack pointer)
;;;;   VFP -> r31  (frame pointer)
;;;;
;;;; Scratch registers: r0, r11, r12
;;;; Reserved: r2 (TOC pointer), r13 (thread pointer)
;;;;
;;;; Key PPC gotcha: r0 reads as literal 0 in some addressing modes
;;;; (instructions that use rA|0 operand format). Never use r0 as a
;;;; base register for loads/stores. Safe as scratch for arithmetic.
;;;;
;;;; Link Register (LR): branch-and-link target, used for call/return.
;;;; Count Register (CTR): used for indirect branches (mtctr/bctr).

(in-package :modus.mvm)

;;; ============================================================
;;; PPC64 Physical Register Encoding
;;; ============================================================

(defconstant +ppc-r0+   0)   ; Scratch (reads as 0 in some modes!)
(defconstant +ppc-r1+   1)   ; Stack pointer (VSP)
(defconstant +ppc-r2+   2)   ; TOC pointer (reserved)
(defconstant +ppc-r3+   3)   ; V0 / VR (arg0 / return)
(defconstant +ppc-r4+   4)   ; V1 (arg1)
(defconstant +ppc-r5+   5)   ; V2 (arg2)
(defconstant +ppc-r6+   6)   ; V3 (arg3)
(defconstant +ppc-r7+   7)   ; Available
(defconstant +ppc-r8+   8)   ; Available
(defconstant +ppc-r9+   9)   ; Available
(defconstant +ppc-r10+  10)  ; Available
(defconstant +ppc-r11+  11)  ; Scratch
(defconstant +ppc-r12+  12)  ; Scratch
(defconstant +ppc-r13+  13)  ; Thread pointer (reserved)
(defconstant +ppc-r14+  14)  ; V4 (callee-saved)
(defconstant +ppc-r15+  15)  ; V5 (callee-saved)
(defconstant +ppc-r16+  16)  ; V6 (callee-saved)
(defconstant +ppc-r17+  17)  ; V7 (callee-saved)
(defconstant +ppc-r18+  18)  ; V8 (callee-saved)
(defconstant +ppc-r19+  19)  ; VA (alloc pointer)
(defconstant +ppc-r20+  20)  ; VL (alloc limit)
(defconstant +ppc-r21+  21)  ; VN (NIL constant)
(defconstant +ppc-r31+  31)  ; VFP (frame pointer)

;;; ============================================================
;;; Virtual -> Physical Register Mapping
;;; ============================================================

(defparameter *ppc-vreg-map*
  (vector +ppc-r3+    ; V0
          +ppc-r4+    ; V1
          +ppc-r5+    ; V2
          +ppc-r6+    ; V3
          +ppc-r14+   ; V4
          +ppc-r15+   ; V5
          +ppc-r16+   ; V6
          +ppc-r17+   ; V7
          +ppc-r18+   ; V8
          nil nil nil  ; V9-V11 (spill)
          nil nil nil nil  ; V12-V15 (spill)
          +ppc-r3+    ; VR (aliases V0)
          +ppc-r19+   ; VA
          +ppc-r20+   ; VL
          +ppc-r21+   ; VN
          +ppc-r1+    ; VSP
          +ppc-r31+   ; VFP
          nil))        ; VPC

(defconstant +ppc-scratch0+ +ppc-r0+)   ; WARNING: reads as 0 in rA|0 modes
(defconstant +ppc-scratch1+ +ppc-r11+)
(defconstant +ppc-scratch2+ +ppc-r12+)

;;; ============================================================
;;; PPC32/64 Mode Selection
;;; ============================================================

(defvar *ppc-64-bit* t
  "When T, emit PPC64 instructions. When NIL, emit PPC32.")

(defconstant +ppc-frame-size+ 1248
  "PPC64 stack frame size: save area + spill + 128 frame slots.
   208 bytes used + 128*8 = 1232, rounded to 1248 for 16-byte alignment.

   ONE HUNDRED TWENTY-EIGHT SLOTS, NOT EIGHT.  The MVM compiler picks the
   `obj-ref VFP <idx>' index per function and tells the back end no bound,
   which is why translate-x64 reserves 128 and translate-aarch64 1024 bytes.
   Eight means a ninth local is addressed BELOW the frame, in the memory the
   next call's frame occupies.  Measured on RISC-V: MAKE-HASH-TABLE's &rest
   prologue alone reads frame slot 31.  -1248 still fits the 16-bit D-form
   displacement that stwu/stdu and the restoring addi use.")

(defconstant +ppc32-frame-size+ 688
  "PPC32 stack frame size: save area + spill + 128 frame slots.
   168 bytes used + 128*4 = 680, rounded to 688 for 8-byte alignment.
   Same 128-slot reasoning as +ppc-frame-size+.")

(defconstant +ppc-frame-slot-base+ 208
  "VFP-relative offset for frame slot 0 on PPC64 (locals via obj-ref VFP).
   Frame slots are at VFP + frame-slot-base + idx*word_size.
   PPC64: spill ends at 128+10*8=208, and 208+128*8=1232 fits the 1248-byte frame.")

(defconstant +ppc32-frame-slot-base+ 168
  "Same, for PPC32 — and it is NOT 208.

   Sharing the PPC64 constant put frame slot 0 at VFP+208 on a frame that is
   208 bytes TOTAL: every local written through a frame slot landed outside
   its own frame, on top of the caller's save area.  One `let` binding stayed
   in a register and survived; a parameter plus two bindings did not, and
   `(defun f (n) (let ((a 5) (b 2)) (+ a b)))` crashed ppc32 while the same
   source passed on all seven other targets — including ppc64, which shares
   this translator.

   +ppc32-frame-size+'s own docstring already assumed this value; only the constant
   disagreed.  Spill ends at 128+10*4=168, and 168+128*4=680 fits 688.")

(defun ppc-frame-slot-base ()
  "VFP-relative offset of frame slot 0 for the target being emitted."
  (if *ppc-64-bit* +ppc-frame-slot-base+ +ppc32-frame-slot-base+))

(defun ppc-word-size ()
  "Return the current word size (4 or 8)."
  (if *ppc-64-bit* 8 4))

(defun ppc-lr-slot ()
  "r1-relative offset of the saved LR, in THIS function's own frame: the top
   word, above the last of the 128 frame slots (1240 of 1248 on ppc64, 684 of
   688 on ppc32).

   LR used to go in the CALLER's linkage slot at old-r1 + 2*ws, the ABI place --
   but this back end passes arguments 5.. by PUSHING them, and old-r1 + 2*ws is
   exactly where the third pushed argument (parameter 7) lives.  A fixed-count
   copy could dodge that by running before the LR store; the &rest copy (trap
   #x0530) runs in the body, long after, so the only sound fix is to stop
   writing into the caller's area.  The epilogue and TAILCALL reload LR from
   here BEFORE popping the frame."
  (- (ppc-frame-size) (ppc-word-size)))

(defun ppc-frame-size ()
  "Return the current frame size."
  (if *ppc-64-bit* +ppc-frame-size+ +ppc32-frame-size+))

;;; ============================================================
;;; PPC Code Buffer
;;; ============================================================

(defstruct ppc-buffer
  (words (make-array 32768))            ; fixed-size, position tracks fill
  (labels (make-hash-table :test 'eql))
  (fixups nil)      ; list of (word-index label-id type)
  (position 0)      ; byte position (always word-aligned)
  (word-count 0))   ; word index (position / 4)

(defvar *ppc-li-const-patches* nil
  "List of (NATIVE-BYTE-OFFSET . POOL-INDEX) recorded by +OP-LI-CONST+: at that
   offset is a LIS/ORI pair whose two 16-bit immediates cross.lisp's
   apply-li-const-patches fills with the constant-pool slot's tagged address.
   Reset at the start of every translation.")

(defun ppc-emit-word (buf word)
  "Emit a 32-bit PPC instruction word."
  (let ((idx (ppc-buffer-word-count buf)))
    (setf (aref (ppc-buffer-words buf) idx) (logand word #xFFFFFFFF))
    (setf (ppc-buffer-word-count buf) (+ idx 1))
    (incf (ppc-buffer-position buf) 4)))

(defun ppc-current-offset (buf)
  "Current byte offset in the code buffer."
  (ppc-buffer-position buf))

(defun ppc-emit-label (buf label-id)
  "Record current position as a branch target."
  (setf (gethash label-id (ppc-buffer-labels buf))
        (ppc-buffer-position buf)))

(defun ppc-emit-fixup (buf label-id fixup-type)
  "Record a fixup for later resolution.
   FIXUP-TYPE is :branch14 (conditional) or :branch24 (unconditional) or :branch-hi/lo."
  (push (list (1- (ppc-buffer-word-count buf)) label-id fixup-type)
        (ppc-buffer-fixups buf)))

(defun ppc-fixup-labels (buf)
  "Resolve all branch label references.  Asserts each branch's signed
   immediate fits before encoding; over-range silently truncates.
   :branch24 = ±2^25 bytes (signed 26-bit byte offset, LI shifted)
   :branch14 = ±2^15 bytes (signed 16-bit byte offset, BD shifted)"
  (let ((words (ppc-buffer-words buf)))
    (dolist (fixup (ppc-buffer-fixups buf))
      (destructuring-bind (word-idx label-id fixup-type) fixup
        (let* ((target (gethash label-id (ppc-buffer-labels buf)))
               (insn-pos (* word-idx 4))
               (rel (- target insn-pos))
               (word (aref words word-idx)))
          (unless target
            (error "PPC: undefined label ~D" label-id))
          (ecase fixup-type
            ;; PC-RELATIVE ADDRESS HALVES for +op-fn-addr+.  The sequence is
            ;;   bl .+4 ; mflr rT ; addis rT,rT,@ha ; addi rT,rT,@l ; ori rT,rT,3
            ;; and LR holds the address of the MFLR, so both halves are measured
            ;; from it: one word before the ADDIS, two before the ADDI.  @ha is
            ;; the high half pre-incremented when the low half's sign bit is set,
            ;; because ADDI sign-extends its immediate.
            (:pcrel-ha
             (let* ((r (- target (* (1- word-idx) 4)))
                    (ha (logand (ash (+ r #x8000) -16) #xFFFF)))
               (setf (aref words word-idx) (logior (logand word #xFFFF0000) ha))))
            (:pcrel-lo
             (let ((r (- target (* (- word-idx 2) 4))))
               (setf (aref words word-idx)
                     (logior (logand word #xFFFF0000) (logand r #xFFFF)))))
            (:branch24
             ;; I-form: bits 6-29 hold offset/4, bit 30=AA, bit 31=LK
             ;; The offset is sign-extended 26-bit, shifted right 2
             (unless (<= (- (ash 1 25)) rel (1- (ash 1 25)))
               (error "PPC :branch24 rel ~D out of range ±2^25 at ~
                       word-idx ~D — would silently truncate"
                      rel word-idx))
             (let ((field (logand (ash rel -2) #xFFFFFF)))
               (setf (aref words word-idx)
                     (logior (logand word #xFC000003)
                             (ash field 2)))))
            (:branch14
             ;; B-form: bits 16-29 hold offset/4, bit 30=AA, bit 31=LK
             (unless (<= (- (ash 1 15)) rel (1- (ash 1 15)))
               (error "PPC :branch14 rel ~D out of range ±2^15 at ~
                       word-idx ~D — would silently truncate"
                      rel word-idx))
             (let ((field (logand (ash rel -2) #x3FFF)))
               (setf (aref words word-idx)
                     (logior (logand word #xFFFF0003)
                             (ash field 2)))))))))))

(defun ppc-buffer-to-bytes (buf)
  "Convert the PPC word buffer to a big-endian byte vector."
  (let* ((nwords (ppc-buffer-word-count buf))
         (bytes (make-array (* nwords 4))))
    (dotimes (i nwords bytes)
      (let ((w (aref (ppc-buffer-words buf) i)))
        ;; Big-endian: MSB first
        (setf (aref bytes (+ (* i 4) 0)) (logand (ash w -24) #xFF)
              (aref bytes (+ (* i 4) 1)) (logand (ash w -16) #xFF)
              (aref bytes (+ (* i 4) 2)) (logand (ash w -8) #xFF)
              (aref bytes (+ (* i 4) 3)) (logand w #xFF))))))

;;; ============================================================
;;; PPC64 Instruction Encoding
;;; ============================================================
;;;
;;; PPC uses a fixed 32-bit instruction format. Major formats:
;;; - I-form:  [opcode:6 | LI:24 | AA:1 | LK:1]
;;; - B-form:  [opcode:6 | BO:5 | BI:5 | BD:14 | AA:1 | LK:1]
;;; - D-form:  [opcode:6 | RT:5 | RA:5 | D:16]
;;; - DS-form: [opcode:6 | RT:5 | RA:5 | DS:14 | XO:2]
;;; - X-form:  [opcode:6 | RT:5 | RA:5 | RB:5 | XO:10 | Rc:1]
;;; - XO-form: [opcode:6 | RT:5 | RA:5 | RB:5 | OE:1 | XO:9 | Rc:1]
;;; - XL-form: [opcode:6 | BO:5 | BI:5 | 0:5 | XO:10 | LK:1]
;;; - M-form:  [opcode:6 | RS:5 | RA:5 | SH:5 | MB:5 | ME:5 | Rc:1]

(defun ppc-d-form (opcode rt ra d)
  "Encode D-form: opcode RT, D(RA)"
  (logior (ash (logand opcode #x3F) 26)
          (ash (logand rt #x1F) 21)
          (ash (logand ra #x1F) 16)
          (logand d #xFFFF)))

(defun ppc-ds-form (opcode rt ra ds xo)
  "Encode DS-form: opcode RT, DS(RA) - for ld/std.

   The DS field holds displacement/4 — the low two bits are the XO field —
   so a displacement that is not a multiple of 4 CANNOT be encoded.  This
   used to (ash ds -2) it away silently, which turned an offset of -1 (what
   `ws - +tag-object+` comes to on ppc64: 8 - 9) into -4 and read the wrong
   word.  Refuse instead: the caller must fold the tag into the base
   register, which is what :obj-ref already does."
  (assert (zerop (logand ds 3)) (ds)
          "PPC DS-form displacement ~D is not a multiple of 4 — ld/std cannot ~
           encode it.  Fold the object tag into the base register first." ds)
  (logior (ash (logand opcode #x3F) 26)
          (ash (logand rt #x1F) 21)
          (ash (logand ra #x1F) 16)
          (ash (logand (ash ds -2) #x3FFF) 2)
          (logand xo #x3)))

(defun ppc-x-form (opcode rt ra rb xo &optional (rc 0))
  "Encode X-form: opcode RT, RA, RB, XO"
  (logior (ash (logand opcode #x3F) 26)
          (ash (logand rt #x1F) 21)
          (ash (logand ra #x1F) 16)
          (ash (logand rb #x1F) 11)
          (ash (logand xo #x3FF) 1)
          (logand rc 1)))

(defun ppc-xo-form (opcode rt ra rb xo &optional (oe 0) (rc 0))
  "Encode XO-form: opcode RT, RA, RB, OE, XO"
  (logior (ash (logand opcode #x3F) 26)
          (ash (logand rt #x1F) 21)
          (ash (logand ra #x1F) 16)
          (ash (logand rb #x1F) 11)
          (ash (logand oe 1) 10)
          (ash (logand xo #x1FF) 1)
          (logand rc 1)))

(defun ppc-xl-form (opcode bo bi xo &optional (lk 0))
  "Encode XL-form: opcode BO, BI, XO (for bclr, bcctr)"
  (logior (ash (logand opcode #x3F) 26)
          (ash (logand bo #x1F) 21)
          (ash (logand bi #x1F) 16)
          (ash 0 11)              ; reserved bits
          (ash (logand xo #x3FF) 1)
          (logand lk 1)))

(defun ppc-i-form (opcode li &optional (aa 0) (lk 0))
  "Encode I-form: opcode LI (for b, bl)"
  (logior (ash (logand opcode #x3F) 26)
          (ash (logand li #xFFFFFF) 2)
          (ash (logand aa 1) 1)
          (logand lk 1)))

(defun ppc-b-form (opcode bo bi bd &optional (aa 0) (lk 0))
  "Encode B-form: opcode BO, BI, BD (conditional branches)"
  (logior (ash (logand opcode #x3F) 26)
          (ash (logand bo #x1F) 21)
          (ash (logand bi #x1F) 16)
          (ash (logand bd #x3FFF) 2)
          (ash (logand aa 1) 1)
          (logand lk 1)))

(defun ppc-md-form (opcode rs ra sh mb xo &optional (rc 0))
  "Encode MD-form: opcode RS, RA, SH, MB, XO (for rotate/shift 64-bit)"
  (let ((sh-lo (logand sh #x1F))
        (sh-hi (logand (ash sh -5) #x1)))
    (logior (ash (logand opcode #x3F) 26)
            (ash (logand rs #x1F) 21)
            (ash (logand ra #x1F) 16)
            (ash sh-lo 11)
            (ash (logand mb #x3F) 5)    ; 6-bit mask begin
            (ash (logand xo #x7) 2)
            (ash sh-hi 1)
            (logand rc 1))))

;;; ============================================================
;;; PPC64 Instruction Emitters
;;; ============================================================

;;; --- Load / Store (64-bit) ---

(defun ppc-emit-ld (buf rt ra offset)
  "LD rt, offset(ra) - 64-bit load (DS-form, opcode 58, XO=0)"
  (ppc-emit-word buf (ppc-ds-form 58 rt ra offset 0)))

(defun ppc-emit-std (buf rs ra offset)
  "STD rs, offset(ra) - 64-bit store (DS-form, opcode 62, XO=0)"
  (ppc-emit-word buf (ppc-ds-form 62 rs ra offset 0)))

(defun ppc-emit-lwz (buf rt ra offset)
  "LWZ rt, offset(ra) - 32-bit unsigned load"
  (ppc-emit-word buf (ppc-d-form 32 rt ra offset)))

(defun ppc-emit-stw (buf rs ra offset)
  "STW rs, offset(ra) - 32-bit store"
  (ppc-emit-word buf (ppc-d-form 36 rs ra offset)))

(defun ppc-emit-lhz (buf rt ra offset)
  "LHZ rt, offset(ra) - 16-bit unsigned load"
  (ppc-emit-word buf (ppc-d-form 40 rt ra offset)))

(defun ppc-emit-sth (buf rs ra offset)
  "STH rs, offset(ra) - 16-bit store"
  (ppc-emit-word buf (ppc-d-form 44 rs ra offset)))

(defun ppc-emit-lbz (buf rt ra offset)
  "LBZ rt, offset(ra) - 8-bit unsigned load"
  (ppc-emit-word buf (ppc-d-form 34 rt ra offset)))

(defun ppc-emit-stb (buf rs ra offset)
  "STB rs, offset(ra) - 8-bit store"
  (ppc-emit-word buf (ppc-d-form 38 rs ra offset)))

;;; --- Arithmetic ---

(defun ppc-emit-add (buf rt ra rb)
  "ADD rt, ra, rb (XO-form, opcode 31, XO=266)"
  (ppc-emit-word buf (ppc-xo-form 31 rt ra rb 266)))

(defun ppc-emit-addi (buf rt ra si)
  "ADDI rt, ra, si - add immediate (D-form, opcode 14)
   When RA=0, this loads the sign-extended immediate into RT."
  (ppc-emit-word buf (ppc-d-form 14 rt ra (logand si #xFFFF))))

(defun ppc-emit-addis (buf rt ra si)
  "ADDIS rt, ra, si - add immediate shifted (D-form, opcode 15)"
  (ppc-emit-word buf (ppc-d-form 15 rt ra (logand si #xFFFF))))

(defun ppc-emit-subf (buf rt ra rb)
  "SUBF rt, ra, rb - subtract from: rt = rb - ra (XO-form, opcode 31, XO=40)"
  (ppc-emit-word buf (ppc-xo-form 31 rt ra rb 40)))

(defun ppc-emit-neg (buf rt ra)
  "NEG rt, ra (XO-form, opcode 31, XO=104)"
  (ppc-emit-word buf (ppc-xo-form 31 rt ra 0 104)))

(defun ppc-emit-mullw (buf rt ra rb)
  "MULLW rt, ra, rb - multiply low word (XO-form, opcode 31, XO=235)"
  (ppc-emit-word buf (ppc-xo-form 31 rt ra rb 235)))

(defun ppc-emit-mulld (buf rt ra rb)
  "MULLD rt, ra, rb - multiply low doubleword (XO-form, opcode 31, XO=233)"
  (ppc-emit-word buf (ppc-xo-form 31 rt ra rb 233)))

(defun ppc-emit-divd (buf rt ra rb)
  "DIVD rt, ra, rb - divide doubleword (XO-form, opcode 31, XO=489)"
  (ppc-emit-word buf (ppc-xo-form 31 rt ra rb 489)))

;;; --- Logical ---

(defun ppc-emit-and (buf ra rs rb)
  "AND ra, rs, rb (X-form, opcode 31, XO=28)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 28)))

(defun ppc-emit-or (buf ra rs rb)
  "OR ra, rs, rb (X-form, opcode 31, XO=444)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 444)))

(defun ppc-emit-xor (buf ra rs rb)
  "XOR ra, rs, rb (X-form, opcode 31, XO=316)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 316)))

(defun ppc-emit-andi-dot (buf ra rs ui)
  "ANDI. ra, rs, ui - AND immediate with record (D-form, opcode 28)"
  (ppc-emit-word buf (ppc-d-form 28 rs ra (logand ui #xFFFF))))

(defun ppc-emit-ori (buf ra rs ui)
  "ORI ra, rs, ui - OR immediate (D-form, opcode 24)"
  (ppc-emit-word buf (ppc-d-form 24 rs ra (logand ui #xFFFF))))

(defun ppc-emit-oris (buf ra rs ui)
  "ORIS ra, rs, ui - OR immediate shifted (D-form, opcode 25)"
  (ppc-emit-word buf (ppc-d-form 25 rs ra (logand ui #xFFFF))))

(defun ppc-emit-xori (buf ra rs ui)
  "XORI ra, rs, ui (D-form, opcode 26)"
  (ppc-emit-word buf (ppc-d-form 26 rs ra (logand ui #xFFFF))))

;;; --- Shift ---

(defun ppc-emit-sld (buf ra rs rb)
  "SLD ra, rs, rb - shift left doubleword (X-form, opcode 31, XO=27)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 27)))

(defun ppc-emit-srd (buf ra rs rb)
  "SRD ra, rs, rb - shift right doubleword (X-form, opcode 31, XO=539)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 539)))

(defun ppc-emit-srad (buf ra rs rb)
  "SRAD ra, rs, rb - shift right algebraic doubleword (X-form, opcode 31, XO=794)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 794)))

(defun ppc-emit-sradi (buf ra rs sh)
  "SRADI ra, rs, sh - shift right algebraic doubleword immediate
   (XS-form: opcode 31, XO=413, sh split across bits)"
  (let ((sh-lo (logand sh #x1F))
        (sh-hi (logand (ash sh -5) 1)))
    (ppc-emit-word buf (logior (ash 31 26)
                               (ash (logand rs #x1F) 21)
                               (ash (logand ra #x1F) 16)
                               (ash sh-lo 11)
                               (ash 413 2)
                               (ash sh-hi 1)))))

(defun ppc-emit-rldicl (buf ra rs sh mb)
  "RLDICL ra, rs, sh, mb - rotate left dword then clear left (MD-form, XO=0)"
  (ppc-emit-word buf (ppc-md-form 30 rs ra sh mb 0)))

;;; --- Compare ---

(defun ppc-emit-cmpd (buf ra rb)
  "CMPD cr0, ra, rb - compare doubleword signed (X-form, opcode 31, XO=0)
   BF=0 (cr0), L=1 (64-bit)"
  (ppc-emit-word buf (ppc-x-form 31 1 ra rb 0)))  ; BF=0,L=1 -> RT field = 1

(defun ppc-emit-cmpdi (buf ra si)
  "CMPDI cr0, ra, si - compare doubleword immediate (D-form, opcode 11)
   BF=0, L=1"
  (ppc-emit-word buf (ppc-d-form 11 1 ra (logand si #xFFFF))))  ; BF=0,L=1 -> RT=1

(defun ppc-emit-cmpld (buf ra rb)
  "CMPLD cr0, ra, rb - compare logical doubleword unsigned (X-form, opcode 31, XO=32)"
  (ppc-emit-word buf (ppc-x-form 31 1 ra rb 32)))  ; BF=0,L=1

;;; --- Branch ---

;; Branch condition (BO) encodings:
;; BO=12 (0b01100) = branch if condition true
;; BO=4  (0b00100) = branch if condition false
;; BO=20 (0b10100) = branch always (unconditional)
;; BI field for cr0: 0=LT, 1=GT, 2=EQ, 3=SO

(defconstant +ppc-bo-true+  12)   ; Branch if condition true
(defconstant +ppc-bo-false+  4)   ; Branch if condition false
(defconstant +ppc-bo-always+ 20)  ; Branch always

(defconstant +ppc-bi-lt+ 0)  ; CR0 LT bit
(defconstant +ppc-bi-gt+ 1)  ; CR0 GT bit
(defconstant +ppc-bi-eq+ 2)  ; CR0 EQ bit
(defconstant +ppc-bi-so+ 3)  ; CR0 SO bit

(defun ppc-emit-b (buf &optional label-id)
  "B target - unconditional branch (I-form, opcode 18)"
  (ppc-emit-word buf (ppc-i-form 18 0))
  (when label-id
    (ppc-emit-fixup buf label-id :branch24)))

(defun ppc-emit-bl (buf &optional label-id)
  "BL target - branch and link (I-form, opcode 18, LK=1)"
  (ppc-emit-word buf (ppc-i-form 18 0 0 1))
  (when label-id
    (ppc-emit-fixup buf label-id :branch24)))

(defun ppc-emit-bc (buf bo bi &optional label-id)
  "BC bo, bi, target - conditional branch (B-form, opcode 16)"
  (ppc-emit-word buf (ppc-b-form 16 bo bi 0))
  (when label-id
    (ppc-emit-fixup buf label-id :branch14)))

(defun ppc-emit-beq (buf &optional label-id)
  "BEQ target"
  (ppc-emit-bc buf +ppc-bo-true+ +ppc-bi-eq+ label-id))

(defun ppc-emit-bne (buf &optional label-id)
  "BNE target"
  (ppc-emit-bc buf +ppc-bo-false+ +ppc-bi-eq+ label-id))

(defun ppc-emit-blt (buf &optional label-id)
  "BLT target"
  (ppc-emit-bc buf +ppc-bo-true+ +ppc-bi-lt+ label-id))

(defun ppc-emit-bge (buf &optional label-id)
  "BGE target"
  (ppc-emit-bc buf +ppc-bo-false+ +ppc-bi-lt+ label-id))

(defun ppc-emit-bgt (buf &optional label-id)
  "BGT target"
  (ppc-emit-bc buf +ppc-bo-true+ +ppc-bi-gt+ label-id))

(defun ppc-emit-ble (buf &optional label-id)
  "BLE target"
  (ppc-emit-bc buf +ppc-bo-false+ +ppc-bi-gt+ label-id))

(defun ppc-emit-sc (buf)
  "SC -- system call.  SC-form, opcode 17, with bit 30 set: 0x44000002.
   Linux/PowerPC passes the syscall number in r0 and arguments in r3..r8, and
   returns in r3.  An error is signalled by CR0.SO rather than by a negative
   return, so a caller that only checks the sign cannot see errno on this
   architecture -- nothing here checks either, but it is the difference that
   matters if something starts to."
  (ppc-emit-word buf #x44000002))

(defun ppc-emit-blr (buf)
  "BLR - branch to link register (return) (XL-form, opcode 19, XO=16)"
  (ppc-emit-word buf (ppc-xl-form 19 +ppc-bo-always+ 0 16)))

(defun ppc-emit-bctr (buf)
  "BCTR - branch to count register (XL-form, opcode 19, XO=528)"
  (ppc-emit-word buf (ppc-xl-form 19 +ppc-bo-always+ 0 528)))

(defun ppc-emit-bctrl (buf)
  "BCTRL - branch to CTR and link (XL-form, opcode 19, XO=528, LK=1)"
  (ppc-emit-word buf (ppc-xl-form 19 +ppc-bo-always+ 0 528 1)))

;;; --- Move to/from Special Registers ---

(defun ppc-emit-mflr (buf rt)
  "MFLR rt - move from link register (X-form: mfspr rt, 8)"
  ;; mfspr encoding: opcode=31, XO=339, SPR=8 (LR) encoded split
  ;; SPR field: bits 11-15 = spr[0:4], bits 16-20 = spr[5:9]
  ;; LR = SPR 8 = 0b0000001000 -> lo=01000=8, hi=00000=0
  (ppc-emit-word buf (logior (ash 31 26)
                             (ash (logand rt #x1F) 21)
                             (ash 8 16)    ; SPR lo bits
                             (ash 0 11)    ; SPR hi bits
                             (ash 339 1))))

(defun ppc-emit-mtlr (buf rs)
  "MTLR rs - move to link register (X-form: mtspr 8, rs)"
  (ppc-emit-word buf (logior (ash 31 26)
                             (ash (logand rs #x1F) 21)
                             (ash 8 16)    ; SPR lo bits
                             (ash 0 11)    ; SPR hi bits
                             (ash 467 1))))

(defun ppc-emit-mtctr (buf rs)
  "MTCTR rs - move to count register (mtspr 9, rs)"
  (ppc-emit-word buf (logior (ash 31 26)
                             (ash (logand rs #x1F) 21)
                             (ash 9 16)    ; SPR lo: CTR=9
                             (ash 0 11)
                             (ash 467 1))))

;;; --- Move Register (simplified mnemonics) ---

(defun ppc-emit-mr (buf ra rs)
  "MR ra, rs - move register (actually OR rs, rs, rs)"
  (ppc-emit-or buf ra rs rs))

;;; --- Nop ---

(defun ppc-emit-nop (buf)
  "NOP (ori 0,0,0)"
  (ppc-emit-ori buf 0 0 0))

;;; --- Trap / System ---

(defun ppc-emit-tw (buf to ra rb)
  "TW to, ra, rb - trap word (X-form, opcode 31, XO=4)"
  (ppc-emit-word buf (ppc-x-form 31 to ra rb 4)))

(defun ppc-emit-twi (buf to ra si)
  "TWI to, ra, si - trap word immediate (D-form, opcode 3)"
  (ppc-emit-word buf (ppc-d-form 3 to ra (logand si #xFFFF))))

(defun ppc-emit-eieio (buf)
  "EIEIO - enforce in-order execution of I/O (memory barrier)"
  (ppc-emit-word buf (ppc-x-form 31 0 0 0 854)))

(defun ppc-emit-sync (buf)
  "SYNC - memory barrier (X-form, opcode 31, XO=598)"
  (ppc-emit-word buf (ppc-x-form 31 0 0 0 598)))

(defun ppc-emit-lwarx (buf rt ra rb)
  "LWARX rt, ra, rb - load word and reserve (X-form, opcode 31, XO=20)"
  (ppc-emit-word buf (ppc-x-form 31 rt ra rb 20)))

(defun ppc-emit-ldarx (buf rt ra rb)
  "LDARX rt, ra, rb - load doubleword and reserve (X-form, opcode 31, XO=84)"
  (ppc-emit-word buf (ppc-x-form 31 rt ra rb 84)))

(defun ppc-emit-stdcx-dot (buf rs ra rb)
  "STDCX. rs, ra, rb - store doubleword conditional (X-form, opcode 31, XO=214, Rc=1)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 214 1)))

(defun ppc-emit-stwcx-dot (buf rs ra rb)
  "STWCX. rs, ra, rb - store word conditional (X-form, opcode 31, XO=150, Rc=1)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 150 1)))

;;; --- PPC32-specific instructions ---

(defun ppc-emit-stwu (buf rs ra offset)
  "STWU rs, offset(ra) - store word with update (D-form, opcode 37)"
  (ppc-emit-word buf (ppc-d-form 37 rs ra (logand offset #xFFFF))))

(defun ppc-emit-divw (buf rt ra rb)
  "DIVW rt, ra, rb - divide word (XO-form, opcode 31, XO=491)"
  (ppc-emit-word buf (ppc-xo-form 31 rt ra rb 491)))

(defun ppc-emit-slw (buf ra rs rb)
  "SLW ra, rs, rb - shift left word (X-form, opcode 31, XO=24)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 24)))

(defun ppc-emit-srw (buf ra rs rb)
  "SRW ra, rs, rb - shift right word (X-form, opcode 31, XO=536)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 536)))

(defun ppc-emit-sraw (buf ra rs rb)
  "SRAW ra, rs, rb - shift right algebraic word (X-form, opcode 31, XO=792)"
  (ppc-emit-word buf (ppc-x-form 31 rs ra rb 792)))

(defun ppc-emit-srawi (buf ra rs sh)
  "SRAWI ra, rs, sh - shift right algebraic word immediate (X-form, opcode 31, XO=824)"
  (ppc-emit-word buf (logior (ash 31 26)
                             (ash (logand rs #x1F) 21)
                             (ash (logand ra #x1F) 16)
                             (ash (logand sh #x1F) 11)
                             (ash 824 1))))

(defun ppc-emit-rlwinm (buf ra rs sh mb me)
  "RLWINM ra, rs, sh, mb, me - rotate left word then AND mask (M-form, opcode 21)"
  (ppc-emit-word buf (logior (ash 21 26)
                             (ash (logand rs #x1F) 21)
                             (ash (logand ra #x1F) 16)
                             (ash (logand sh #x1F) 11)
                             (ash (logand mb #x1F) 6)
                             (ash (logand me #x1F) 1))))

(defun ppc-emit-cmpw (buf ra rb)
  "CMPW cr0, ra, rb - compare word signed (X-form, opcode 31, XO=0, L=0)"
  (ppc-emit-word buf (ppc-x-form 31 0 ra rb 0)))

(defun ppc-emit-cmpwi (buf ra si)
  "CMPWI cr0, ra, si - compare word immediate (D-form, opcode 11, L=0)"
  (ppc-emit-word buf (ppc-d-form 11 0 ra (logand si #xFFFF))))

(defun ppc-emit-cmplw (buf ra rb)
  "CMPLW cr0, ra, rb - compare logical word unsigned (X-form, opcode 31, XO=32, L=0)"
  (ppc-emit-word buf (ppc-x-form 31 0 ra rb 32)))

;;; --- Width-dispatching helpers (select 32 or 64 based on *ppc-64-bit*) ---

(defun ppc-emit-load-word (buf rt ra offset)
  "Load a word (4 or 8 bytes depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-ld buf rt ra offset)
      (ppc-emit-lwz buf rt ra offset)))

(defun ppc-emit-store-word (buf rs ra offset)
  "Store a word (4 or 8 bytes depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-std buf rs ra offset)
      (ppc-emit-stw buf rs ra offset)))

;;; ============================================================
;;; Convention slots (nargs / cenv / mv-count)
;;; ============================================================
;;;
;;; Same shape as i386's absolute-slot block: the caller writes, the callee
;;; reads, single-threaded cooperative execution makes that exactly as correct
;;; as x64's spare physical registers.  The base differs per mode because the
;;; two PPC targets load at different addresses -- ppc32 at 0 (cons space at
;;; 16MB), ppc64 at 0x20000000 (stack top 0x20400000, cons at 0x24000000) --
;;; so each base sits in mapped RAM clear of stack and heap.
(defparameter *ppc-globals-base* #x00900000
  "Base of the PPC absolute-address convention slot block; set per target by
   install-ppc-translator / install-ppc32-translator.")

(defparameter *ppc-linux-mode* nil
  "T when building a HOSTED Linux/PPC image rather than a bare-metal one.

   PPC64 AND PPC32 ARE NOT ONE TARGET EACH -- bare and hosted are different
   memory maps.  Bare ppc32 keeps its convention slots at #x00900000 (inside the
   64 MB boot-ppc32.lisp's TLBs map) and writes the console byte to an e500 UART
   at #xE0004500; bare ppc64 uses #x20900000 and a powernv LPC UART.  Under Linux
   neither UART exists in our address space and neither slot base is mapped, so
   PPC-SET-LINUX-MODE moves the slots into the mmap'd heap and the console byte
   becomes write(2).")

(defparameter *ppc-hosted-globals-base* #x10000A00
  "Convention slots for the HOSTED ports, inside the mmap'd heap and above the
   Cheney metadata at #x10000040 -- the same address the RISC-V and i386 hosted
   ports pick, for the same reason.")

;;; Linux/PowerPC syscall numbers.  The table is the SAME at both widths (unlike
;;; x86, where 32- and 64-bit numbering diverge completely), and it is not the
;;; asm-generic table RISC-V uses either: write is 4 here and 64 there.
;;; 90 is sys_mmap taking SIX REGISTER arguments -- PowerPC does not have i386's
;;; old_mmap-through-a-pointer calling convention, so no argument block is needed.
(defconstant +ppc-linux-sys-exit+   1)
(defconstant +ppc-linux-sys-read+   3)
(defconstant +ppc-linux-sys-write+  4)
(defconstant +ppc-linux-sys-mmap+  90)

;;; The MV-count slot is +MV-COUNT-ADDR+, set per target in mvm/target.lisp
;;; (ppc32's value is #x00900020, inside the 64 MB boot-ppc32.lisp maps; ppc64
;;; keeps the historical #x10000090) and injected into every compilation, so the
;;; compiler's expansions, shared CL source and this :set-mv-count all name the
;;; same word.  This used to be a per-installer private slot, which made the
;;; writer and readers disagree on ppc32.
(defun ppc-nargs-addr ()   (+ *ppc-globals-base* #x00))
(defun ppc-cenv-addr ()    (+ *ppc-globals-base* #x10))
(defun ppc-mvcount-addr () +mv-count-addr+)

(defun ppc-emit-store-abs (buf src-reg addr)
  "Store SRC-REG to absolute ADDR, using scratch2 to hold the address."
  (ppc-emit-li buf +ppc-scratch2+ addr)
  (ppc-emit-store-word buf src-reg +ppc-scratch2+ 0))

(defun ppc-emit-load-abs (buf rt addr)
  "Load from absolute ADDR into RT, using scratch2 to hold the address."
  (ppc-emit-li buf +ppc-scratch2+ addr)
  (ppc-emit-load-word buf rt +ppc-scratch2+ 0))

(defun ppc-emit-cmp-word (buf ra rb)
  "Compare words (cmpd or cmpw depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-cmpd buf ra rb)
      (ppc-emit-cmpw buf ra rb)))

(defun ppc-emit-cmpi-word (buf ra si)
  "Compare word immediate (cmpdi or cmpwi depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-cmpdi buf ra si)
      (ppc-emit-cmpwi buf ra si)))

(defun ppc-emit-cmpl-word (buf ra rb)
  "Compare logical word (cmpld or cmplw depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-cmpld buf ra rb)
      (ppc-emit-cmplw buf ra rb)))

(defun ppc-emit-mul-word (buf rt ra rb)
  "Multiply word (mulld or mullw depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-mulld buf rt ra rb)
      (ppc-emit-mullw buf rt ra rb)))

(defun ppc-emit-div-word (buf rt ra rb)
  "Divide word (divd or divw depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-divd buf rt ra rb)
      (ppc-emit-divw buf rt ra rb)))

(defun ppc-emit-shift-left (buf ra rs rb)
  "Shift left (sld or slw depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-sld buf ra rs rb)
      (ppc-emit-slw buf ra rs rb)))

(defun ppc-emit-shift-right (buf ra rs rb)
  "Shift right logical (srd or srw depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-srd buf ra rs rb)
      (ppc-emit-srw buf ra rs rb)))

(defun ppc-emit-shift-right-arith (buf ra rs rb)
  "Shift right algebraic (srad or sraw depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-srad buf ra rs rb)
      (ppc-emit-sraw buf ra rs rb)))

(defun ppc-emit-shift-right-arith-imm (buf ra rs sh)
  "Shift right algebraic immediate (sradi or srawi depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-sradi buf ra rs sh)
      (ppc-emit-srawi buf ra rs sh)))

(defun ppc-emit-load-reserve (buf rt ra rb)
  "Load and reserve (ldarx or lwarx depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-ldarx buf rt ra rb)
      (ppc-emit-lwarx buf rt ra rb)))

(defun ppc-emit-store-cond (buf rs ra rb)
  "Store conditional (stdcx. or stwcx. depending on *ppc-64-bit*)."
  (if *ppc-64-bit*
      (ppc-emit-stdcx-dot buf rs ra rb)
      (ppc-emit-stwcx-dot buf rs ra rb)))

;;; ============================================================
;;; Spill Slot Management
;;; ============================================================

(defconstant +ppc-spill-base-offset+ 128
  "Offset from VFP (r31) where spill slots begin.
   Slots 0-15 of the frame are reserved for save area.")

(defun ppc-spill-offset (vreg)
  "Calculate the stack frame offset for a spilled virtual register."
  (let ((slot (cond
                ((and (>= vreg 9) (<= vreg 15))  ; V9-V15
                 (- vreg 9))
                ((= vreg +vreg-va+) 7)   ; VA spill (shouldn't normally happen)
                ((= vreg +vreg-vl+) 8)   ; VL spill
                ((= vreg +vreg-vn+) 9)   ; VN spill
                (t (error "PPC: unexpected spill for vreg ~D" vreg)))))
    (+ +ppc-spill-base-offset+ (* slot (ppc-word-size)))))

(defun ppc-load-vreg (buf phys-dst vreg)
  "Load a virtual register into a physical register. If the vreg has a
   physical mapping, emit MR. If it spills, load from the frame."
  (let ((phys (and (< vreg (length *ppc-vreg-map*))
                   (aref *ppc-vreg-map* vreg))))
    (if phys
        (unless (= phys phys-dst)
          (ppc-emit-mr buf phys-dst phys))
        ;; Spill: load from stack frame
        (ppc-emit-load-word buf phys-dst +ppc-r31+ (ppc-spill-offset vreg)))))

(defun ppc-store-vreg (buf vreg phys-src)
  "Store a physical register value into a virtual register. If the vreg
   has a physical mapping, emit MR. If it spills, store to frame."
  (let ((phys (and (< vreg (length *ppc-vreg-map*))
                   (aref *ppc-vreg-map* vreg))))
    (if phys
        (unless (= phys phys-src)
          (ppc-emit-mr buf phys phys-src))
        ;; Spill: store to stack frame
        (ppc-emit-store-word buf phys-src +ppc-r31+ (ppc-spill-offset vreg)))))

(defun ppc-vreg-phys (vreg)
  "Return the physical register for a vreg, or NIL if it spills."
  (and (< vreg (length *ppc-vreg-map*))
       (aref *ppc-vreg-map* vreg)))

;;; ============================================================
;;; Immediate Loading (32-bit or 64-bit)
;;; ============================================================

;;; ---- Floating point (FPRs f0/f1 only; nothing else in this back end uses FPRs)

(defun ppc-emit-lfd (buf frt ra d)
  "LFD frt, d(ra) -- load a double into an FPR (D-form, opcode 50)."
  (ppc-emit-word buf (logior (ash 50 26) (ash frt 21) (ash ra 16) (logand d #xFFFF))))

(defun ppc-emit-stfd (buf frs ra d)
  "STFD frs, d(ra) -- store a double from an FPR (D-form, opcode 54)."
  (ppc-emit-word buf (logior (ash 54 26) (ash frs 21) (ash ra 16) (logand d #xFFFF))))

(defun ppc-emit-fp-a (buf xo frt fra frb frc)
  "A-form, primary opcode 63 (double precision)."
  (ppc-emit-word buf (logior (ash 63 26) (ash frt 21) (ash fra 16) (ash frb 11)
                             (ash frc 6) (ash xo 1))))

(defun ppc-emit-fadd (buf frt fra frb) (ppc-emit-fp-a buf 21 frt fra frb 0))
(defun ppc-emit-fsub (buf frt fra frb) (ppc-emit-fp-a buf 20 frt fra frb 0))
(defun ppc-emit-fdiv (buf frt fra frb) (ppc-emit-fp-a buf 18 frt fra frb 0))
(defun ppc-emit-fmul (buf frt fra frc)
  "FMUL takes its second operand in the FRC field, not FRB."
  (ppc-emit-fp-a buf 25 frt fra 0 frc))

(defun ppc-emit-fp-x (buf xo frt frb)
  "X-form, primary opcode 63, FRA unused."
  (ppc-emit-word buf (logior (ash 63 26) (ash frt 21) (ash frb 11) (ash xo 1))))

(defun ppc-emit-fcfid (buf frt frb)  (ppc-emit-fp-x buf 846 frt frb)) ; ppc64 only
(defun ppc-emit-fctidz (buf frt frb) (ppc-emit-fp-x buf 815 frt frb)) ; ppc64 only
(defun ppc-emit-fctiwz (buf frt frb) (ppc-emit-fp-x buf 15 frt frb))

(defun ppc-float-unbox (buf ptr fd)
  "Load the double whose TAGGED pointer is in PTR into FPR FD.

   A boxed double is four TAGGED 16-bit chunks, slot k holding bits
   (63-16k)..(48-16k).  PowerPC is big-endian, so chunk k is exactly the
   halfword at byte 2k of the IEEE double in memory: untag each chunk, STH it
   into a 16-byte stack scratch, then one LFD.  The same route RV32 takes
   (rv32-float-unbox) -- and the only one on ppc32, which has no 64-bit GPR.
   The scratch is taken by moving r1 (ppc32 SysV has no red zone).  r0 carries
   each chunk: a fine data register, never used here as a base.  PTR must not
   be r0."
  (let ((ws (ppc-word-size)))
    (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ -16)
    (loop for k from 0 to 3
          ;; LWZ, not the width's load: on ppc64 LD is DS-form and cannot encode
          ;; the odd displacement a tag-9 pointer gives (8(1+k)-9).  A chunk is
          ;; under 2^17, so it sits wholly in the slot's LOW word -- bytes 4..7
          ;; of the big-endian doubleword -- which D-form LWZ reaches at +4.
          do (ppc-emit-lwz buf +ppc-r0+ ptr (+ (- (* (1+ k) ws) +tag-object+)
                                               (if *ppc-64-bit* 4 0)))
             (ppc-emit-srawi buf +ppc-r0+ +ppc-r0+ 1)          ; untag
             (ppc-emit-sth buf +ppc-r0+ +ppc-r1+ (* 2 k)))
    (ppc-emit-lfd buf fd +ppc-r1+ 0)
    (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 16)))

(defun ppc-float-box (buf fs out)
  "Box the double in FPR FS as a fresh four-chunk object; its TAGGED pointer
   lands in OUT (not r0).  STFD to a stack scratch, then LHZ each chunk back,
   tag it, store it.  Header (4<<8)|#x60, five words rounded to 16 bytes, so
   the heap pointer stays 16-aligned for the tag scheme."
  (let* ((ws (ppc-word-size))
         (bytes (logand (+ (* 5 ws) 15) (lognot 15))))
    (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ -16)
    (ppc-emit-stfd buf fs +ppc-r1+ 0)
    (ppc-emit-li buf +ppc-r0+ (logior #x60 (ash 4 8)))
    (ppc-emit-store-word buf +ppc-r0+ +ppc-r19+ 0)          ; header at VA
    (loop for k from 0 to 3
          do (ppc-emit-lhz buf +ppc-r0+ +ppc-r1+ (* 2 k))
             (ppc-emit-add buf +ppc-r0+ +ppc-r0+ +ppc-r0+)      ; tag (x2)
             (ppc-emit-store-word buf +ppc-r0+ +ppc-r19+ (* (1+ k) ws)))
    (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 16)
    (ppc-emit-addi buf out +ppc-r19+ +tag-object+)
    (ppc-emit-addi buf +ppc-r19+ +ppc-r19+ bytes)))

;;; ============================================================
;;; handler-case: SETJMP (#x0510) / LONGJMP (#x0511) / CLEAR-HANDLER (#x0512)
;;; ============================================================
;;;
;;; A port of translate-riscv's protocol (see the long comment there): one
;;; live jmpbuf, a stack of saved outer jmpbufs so handler-cases NEST, a depth
;;; cap past which a push stores nothing and its matching pop is ABSORBED, and
;;; LONGJMP copying the jmpbuf aside before the pop overwrites it.
;;;
;;; JMPBUF = 8 words: r1, r31 (VFP), resume address, then r14..r18 -- V4..V8,
;;; the callee-saved V-registers, which the epilogues a LONGJMP skips would
;;; otherwise have restored.  VA/VL/VN are deliberately absent (restoring VA
;;; would un-allocate the condition object the handler is about to read); V0..V3
;;; are caller-saved.  Temporaries: r7..r10, r11, r12 and r0 (r0 only as data).
;;;
;;; WHERE IT LIVES.  Hosted: the shared contract block at the heap base --
;;; jmpbuf #x10000180, scratch #x10000300, capped #x10000360, depth #x10000400,
;;; frames from #x10000408 -- the same addresses as RISC-V.  With 8-word frames,
;;; 21 of them end at #x10000948 (ppc64), clear of ppc's globals at #x10000A00.
;;; Bare: the same offsets inside *ppc-globals-base*'s DRAM block, which uses
;;; only #x00-#x17.  Computed at translate time, after the installers and
;;; ppc-set-linux-mode have fixed the mode.

(defconstant +ppc-jmpbuf-words+ 8)
(defparameter *ppc-hstack-max-depth* 21)
(defun ppc-hbase ()           (if *ppc-linux-mode* #x10000000 *ppc-globals-base*))
(defun ppc-jmpbuf-addr ()     (+ (ppc-hbase) #x180))
(defun ppc-lj-scratch-addr () (+ (ppc-hbase) #x300))
(defun ppc-hcapped-addr ()    (+ (ppc-hbase) #x360))
(defun ppc-hdepth-addr ()     (+ (ppc-hbase) #x400))
(defun ppc-hframes-addr ()    (+ (ppc-hbase) #x408))

(defun ppc-emit-slwi (buf ra rs n)
  "SLWI ra, rs, n  (RLWINM ra, rs, n, 0, 31-n)."
  (ppc-emit-rlwinm buf ra rs n 0 (- 31 n)))

(defun ppc-emit-frame-addr (buf dst depth-reg)
  "DST = frames-base + DEPTH-REG * frame-bytes.  Frame bytes are 8 words: 64 on
   ppc64, 32 on ppc32 -- a power of two, so a shift.  Uses r11."
  (ppc-emit-li buf dst (ppc-hframes-addr))
  (ppc-emit-slwi buf +ppc-r11+ depth-reg (if *ppc-64-bit* 6 5))
  (ppc-emit-add buf dst dst +ppc-r11+))

(defun ppc-emit-copy-words (buf from to)
  "Copy +ppc-jmpbuf-words+ words from 0(FROM) to 0(TO) through r0."
  (let ((ws (ppc-word-size)))
    (dotimes (i +ppc-jmpbuf-words+)
      (ppc-emit-load-word buf +ppc-r0+ from (* i ws))
      (ppc-emit-store-word buf +ppc-r0+ to (* i ws)))))

(defun ppc-emit-handler-push (buf)
  "Stack the CURRENT jmpbuf so a nested handler-case does not overwrite it.
   Leaves r10 = 0 if a frame was stored, 1 if the push was CAPPED."
  (let ((capped (mvm-make-label))
        (done (mvm-make-label)))
    (ppc-emit-li buf +ppc-r7+ (ppc-hdepth-addr))
    (ppc-emit-load-word buf +ppc-r8+ +ppc-r7+ 0)          ; r8 = depth
    (ppc-emit-li buf +ppc-r10+ 1)                         ; assume capped
    (ppc-emit-cmpi-word buf +ppc-r8+ *ppc-hstack-max-depth*)
    (ppc-emit-bge buf capped)
    (ppc-emit-frame-addr buf +ppc-r9+ +ppc-r8+)
    (ppc-emit-li buf +ppc-r12+ (ppc-jmpbuf-addr))
    (ppc-emit-copy-words buf +ppc-r12+ +ppc-r9+)
    (ppc-emit-addi buf +ppc-r8+ +ppc-r8+ 1)
    (ppc-emit-store-word buf +ppc-r8+ +ppc-r7+ 0)
    (ppc-emit-li buf +ppc-r10+ 0)                         ; stored
    (ppc-emit-b buf done)
    (ppc-emit-label buf capped)
    (ppc-emit-li buf +ppc-r7+ (ppc-hcapped-addr))         ; bump LIVE capped count
    (ppc-emit-load-word buf +ppc-r8+ +ppc-r7+ 0)
    (ppc-emit-addi buf +ppc-r8+ +ppc-r8+ 1)
    (ppc-emit-store-word buf +ppc-r8+ +ppc-r7+ 0)
    (ppc-emit-label buf done)))

(defun ppc-emit-handler-pop (buf)
  "Absorb a capped push, or restore the top stacked frame into the jmpbuf, or
   ZERO the jmpbuf when the stack is empty.  Never touches r3 (V0), which holds
   the handler-case's result at a CLEAR-HANDLER."
  (let ((not-capped (mvm-make-label))
        (empty (mvm-make-label))
        (done (mvm-make-label))
        (ws (ppc-word-size)))
    ;; arm 1: capped > 0 -- absorb.
    (ppc-emit-li buf +ppc-r7+ (ppc-hcapped-addr))
    (ppc-emit-load-word buf +ppc-r8+ +ppc-r7+ 0)
    (ppc-emit-cmpi-word buf +ppc-r8+ 0)
    (ppc-emit-beq buf not-capped)
    (ppc-emit-addi buf +ppc-r8+ +ppc-r8+ -1)
    (ppc-emit-store-word buf +ppc-r8+ +ppc-r7+ 0)
    (ppc-emit-b buf done)
    ;; arm 2: depth > 0 -- restore frame[depth-1] into the jmpbuf.
    (ppc-emit-label buf not-capped)
    (ppc-emit-li buf +ppc-r7+ (ppc-hdepth-addr))
    (ppc-emit-load-word buf +ppc-r8+ +ppc-r7+ 0)
    (ppc-emit-li buf +ppc-r12+ (ppc-jmpbuf-addr))
    (ppc-emit-cmpi-word buf +ppc-r8+ 0)
    (ppc-emit-beq buf empty)
    (ppc-emit-addi buf +ppc-r8+ +ppc-r8+ -1)
    (ppc-emit-store-word buf +ppc-r8+ +ppc-r7+ 0)
    (ppc-emit-frame-addr buf +ppc-r9+ +ppc-r8+)
    (ppc-emit-copy-words buf +ppc-r9+ +ppc-r12+)
    (ppc-emit-b buf done)
    ;; arm 3: empty -- zero the WHOLE jmpbuf (word 0 = 0 is LONGJMP's sentinel).
    (ppc-emit-label buf empty)
    (ppc-emit-li buf +ppc-r0+ 0)
    (dotimes (i +ppc-jmpbuf-words+)
      (ppc-emit-store-word buf +ppc-r0+ +ppc-r12+ (* i ws)))
    (ppc-emit-label buf done)))

(defun ppc-emit-li (buf rt imm)
  "Load an immediate into register RT.
   In 64-bit mode, handles full 64-bit values.
   In 32-bit mode, max 32-bit values via lis+ori."
  (cond
    ;; Small signed 16-bit immediate
    ((<= -32768 imm 32767)
     ;; LI rt, imm  (= ADDI rt, 0, imm)
     (ppc-emit-addi buf rt 0 (logand imm #xFFFF)))
    ;; Unsigned 16-bit
    ((<= 0 imm #xFFFF)
     ;; ORI rt, 0, imm  (but r0 reads as 0 in addi)
     (ppc-emit-addi buf rt 0 0)
     (ppc-emit-ori buf rt rt (logand imm #xFFFF)))
    ;; 32-bit value
    ((<= -2147483648 imm #xFFFFFFFF)
     (let ((hi (logand (ash imm -16) #xFFFF))
           (lo (logand imm #xFFFF)))
       ;; LIS rt, hi (= ADDIS rt, 0, hi)
       (ppc-emit-addis buf rt 0 hi)
       (when (not (zerop lo))
         (ppc-emit-ori buf rt rt lo))))
    ;; Full 64-bit value: need 5 instructions (PPC64 only)
    (*ppc-64-bit*
     (let ((hh (logand (ash imm -48) #xFFFF))
           (hl (logand (ash imm -32) #xFFFF))
           (lh (logand (ash imm -16) #xFFFF))
           (ll (logand imm #xFFFF)))
       ;; LIS rt, hh
       (ppc-emit-addis buf rt 0 hh)
       ;; ORI rt, rt, hl
       (when (not (zerop hl))
         (ppc-emit-ori buf rt rt hl))
       ;; RLDICR rt, rt, 32, 31 -- shift left 32, clear right 32
       (ppc-emit-word buf (ppc-md-form 30 rt rt 32 31 1))
       ;; ORIS rt, rt, lh
       (when (not (zerop lh))
         (ppc-emit-oris buf rt rt lh))
       ;; ORI rt, rt, ll
       (when (not (zerop ll))
         (ppc-emit-ori buf rt rt ll))))
    ;; PPC32: truncate to 32-bit
    (t
     (let ((hi (logand (ash imm -16) #xFFFF))
           (lo (logand imm #xFFFF)))
       (ppc-emit-addis buf rt 0 hi)
       (when (not (zerop lo))
         (ppc-emit-ori buf rt rt lo))))))

;;; ============================================================
;;; Prologue / Epilogue
;;; ============================================================

(defun ppc-emit-prologue (buf &optional (nparams 0))
  "Emit function prologue. Creates the frame, copies parameters 5.. into
   frame slots 4.., saves LR and the callee-saved regs.

   NPARAMS is the frame-enter TRAP's code.  Only V0-V3 travel in registers;
   the caller PUSHes the rest (arg 4 last, one word each) and the body reads
   parameter i as `obj-ref VFP i', so they must be copied into the frame --
   translate-x64/i386/aarch64/arm32 all do it, and this back end did not, so
   every fifth-and-later parameter was an uninitialised slot.  Found on RISC-V
   in the real CL image (COPY-SEQ's fifth argument to %BULK-COPY).

   Arguments are read from old-r1 = r1 + fs.  LR is saved in this frame
   (ppc-lr-slot), not the caller's linkage area, which held pushed argument 7.
   r0 carries each word: a fine data register, only never a BASE (rA=0 reads
   as literal zero)."
  (let ((ws (ppc-word-size))
        (fs (ppc-frame-size)))
    (when (> nparams 128)
      (error "MVM PPC: ~D parameters exceed the 128-slot frame" nparams))
    ;; Create stack frame: stdu/stwu r1, -framesize(r1)
    (if *ppc-64-bit*
        (ppc-emit-word buf (ppc-ds-form 62 +ppc-r1+ +ppc-r1+
                                        (logand (- fs) #xFFFC) 1))
        (ppc-emit-stwu buf +ppc-r1+ +ppc-r1+ (logand (- fs) #xFFFF)))
    ;; Parameters 5.. from the caller's pushes (old r1 = r1 + fs) into slots 4..
    (loop for i from 4 below nparams
          do (ppc-emit-load-word buf +ppc-r0+ +ppc-r1+ (+ fs (* (- i 4) ws)))
             (ppc-emit-store-word buf +ppc-r0+ +ppc-r1+
                                  (+ (ppc-frame-slot-base) (* i ws))))
    ;; Save LR in this frame (ppc-lr-slot), never the caller's linkage area.
    (ppc-emit-mflr buf +ppc-r0+)
    (ppc-emit-store-word buf +ppc-r0+ +ppc-r1+ (ppc-lr-slot))
    ;; Save callee-saved registers
    (let ((base (* 6 ws)))  ; save area starts at 6 words into frame
      (ppc-emit-store-word buf +ppc-r14+ +ppc-r1+ base)
      (ppc-emit-store-word buf +ppc-r15+ +ppc-r1+ (+ base (* 1 ws)))
      (ppc-emit-store-word buf +ppc-r16+ +ppc-r1+ (+ base (* 2 ws)))
      (ppc-emit-store-word buf +ppc-r17+ +ppc-r1+ (+ base (* 3 ws)))
      (ppc-emit-store-word buf +ppc-r18+ +ppc-r1+ (+ base (* 4 ws)))
      ;; r19 (VA), r20 (VL) and r21 (VN) ARE NOT SAVED -- see ppc-emit-epilogue.
      ;; Their three slots stay unused.
      (ppc-emit-store-word buf +ppc-r31+ +ppc-r1+ (+ base (* 8 ws)))) ; VFP
    ;; Set up frame pointer
    (ppc-emit-mr buf +ppc-r31+ +ppc-r1+)))

(defun ppc-emit-epilogue (buf)
  "Emit function epilogue. Restores callee-saved regs, frame, LR, returns."
  (let ((ws (ppc-word-size))
        (fs (ppc-frame-size)))
    ;; Restore callee-saved registers
    (let ((base (* 6 ws)))
      (ppc-emit-load-word buf +ppc-r14+ +ppc-r1+ base)
      (ppc-emit-load-word buf +ppc-r15+ +ppc-r1+ (+ base (* 1 ws)))
      (ppc-emit-load-word buf +ppc-r16+ +ppc-r1+ (+ base (* 2 ws)))
      (ppc-emit-load-word buf +ppc-r17+ +ppc-r1+ (+ base (* 3 ws)))
      (ppc-emit-load-word buf +ppc-r18+ +ppc-r1+ (+ base (* 4 ws)))
      ;; VA, VL AND VN ARE GLOBAL STATE AND MUST NOT BE RESTORED.  r19 is the
      ;; ALLOCATION POINTER; restoring it on return rolls the heap pointer back
      ;; over everything the callee allocated, so the caller's next CONS is
      ;; handed memory that is already live.  translate-aarch64 names the
      ;; consequence ("allocations made by callees would be lost on return")
      ;; and translate-x64 the rule; both save only their one real callee-saved
      ;; vreg.  Found on RISC-V, which restored all eleven of s1-s11: it
      ;; destroyed the first nine conses of the image and left the globals
      ;; table malformed from the first global on.  r20 is the alloc LIMIT,
      ;; which a collection legitimately moves, and r21 is a constant.
      (ppc-emit-load-word buf +ppc-r31+ +ppc-r1+ (+ base (* 8 ws))))
    ;; Restore LR from this frame -- BEFORE the frame is popped.
    (ppc-emit-load-word buf +ppc-r0+ +ppc-r1+ (ppc-lr-slot))
    (ppc-emit-mtlr buf +ppc-r0+)
    ;; Restore stack pointer
    (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ fs)
    ;; Return
    (ppc-emit-blr buf)))

;;; ============================================================
;;; MVM Opcode Translation
;;; ============================================================

(defun ppc-translate-insn (buf opcode operands mvm-pc label-map function-table)
  "Translate a single MVM instruction to PPC64 native code.
   LABEL-MAP maps MVM bytecode offsets to PPC label IDs.
   Returns T if a label was already emitted for this PC."
  (declare (ignorable mvm-pc function-table))
  ;; Labels are emitted in the main loop at the correct PC position.
  ;; mvm-pc here is new-pc (position after this instruction), used for
  ;; branch offset computation.

  (flet ((vreg-or-scratch (vreg scratch)
           "Return the physical register for VREG, or load it into SCRATCH and return SCRATCH."
           (let ((phys (ppc-vreg-phys vreg)))
             (if phys
                 phys
                 (progn
                   (ppc-load-vreg buf scratch vreg)
                   scratch))))
         (ensure-label (target-pc)
           "Get or create a label for the given MVM bytecode PC."
           (or (gethash target-pc label-map)
               (let ((l (mvm-make-label)))
                 (setf (gethash target-pc label-map) l)
                 l))))

    (case opcode
      ;;; --- NOP / BREAK / TRAP ---
      (#.+op-nop+
       (ppc-emit-nop buf))

      (#.+op-break+
       ;; TRAP unconditional: TW 31, 0, 0
       (ppc-emit-tw buf 31 0 0))

      (#.+op-trap+
       (let ((code (first operands)))
         (cond
           ((< code #x0100)
            ;; Frame-enter: CODE is the parameter count -- see ppc-emit-prologue.
            (ppc-emit-prologue buf code))
           ((< code #x0300)
            ;; Frame-alloc/frame-free: NOP for now
            nil)
           ((= code #x0510)
            ;; SETJMP.  Stack the outer jmpbuf, then save r1 / r31 / resume
            ;; address / r14..r18.  First return is NIL; a LONGJMP re-enters at
            ;; RESUME with r3 = T.  A CAPPED push arms nothing, so an over-deep
            ;; handler-case degrades to a transparent no-op.  The resume address
            ;; comes from `bl .+4; mflr' plus @ha/@l fixups, as +op-fn-addr+.
            (let ((skip (mvm-make-label))
                  (resume (mvm-make-label))
                  (ws (ppc-word-size)))
              (ppc-emit-handler-push buf)
              (ppc-emit-cmpi-word buf +ppc-r10+ 0)
              (ppc-emit-bne buf skip)
              (ppc-emit-li buf +ppc-r12+ (ppc-jmpbuf-addr))
              (ppc-emit-store-word buf +ppc-r1+ +ppc-r12+ 0)
              (ppc-emit-store-word buf +ppc-r31+ +ppc-r12+ ws)
              (ppc-emit-word buf #x48000005)                    ; bl .+4
              (ppc-emit-mflr buf +ppc-r9+)
              (ppc-emit-addis buf +ppc-r9+ +ppc-r9+ 0)
              (ppc-emit-fixup buf resume :pcrel-ha)
              (ppc-emit-addi buf +ppc-r9+ +ppc-r9+ 0)
              (ppc-emit-fixup buf resume :pcrel-lo)
              (ppc-emit-store-word buf +ppc-r9+ +ppc-r12+ (* 2 ws))
              (loop for r in (list +ppc-r14+ +ppc-r15+ +ppc-r16+ +ppc-r17+ +ppc-r18+)
                    for i from 3
                    do (ppc-emit-store-word buf r +ppc-r12+ (* i ws)))
              (ppc-emit-label buf skip)
              (ppc-emit-mr buf +ppc-r3+ +ppc-r21+)              ; first return: NIL
              (ppc-emit-label buf resume)))
           ((= code #x0511)
            ;; LONGJMP.  Copy the jmpbuf aside FIRST (the pop restores the OUTER
            ;; frame over it, and we are jumping to the INNER one), zero the live
            ;; capped count (this unwind passes every capped frame at once), pop,
            ;; then restore and jump with r3 = T.  Word 0 = 0 means nothing is
            ;; armed: trap, rather than jump to address zero.
            (let ((nohandler (mvm-make-label))
                  (ws (ppc-word-size)))
              (ppc-emit-li buf +ppc-r12+ (ppc-jmpbuf-addr))
              (ppc-emit-load-word buf +ppc-r7+ +ppc-r12+ 0)
              (ppc-emit-cmpi-word buf +ppc-r7+ 0)
              (ppc-emit-beq buf nohandler)
              (ppc-emit-li buf +ppc-r9+ (ppc-lj-scratch-addr))
              (ppc-emit-copy-words buf +ppc-r12+ +ppc-r9+)
              (ppc-emit-li buf +ppc-r7+ (ppc-hcapped-addr))
              (ppc-emit-li buf +ppc-r0+ 0)
              (ppc-emit-store-word buf +ppc-r0+ +ppc-r7+ 0)
              (ppc-emit-handler-pop buf)
              (ppc-emit-li buf +ppc-r12+ (ppc-lj-scratch-addr))
              (loop for r in (list +ppc-r14+ +ppc-r15+ +ppc-r16+ +ppc-r17+ +ppc-r18+)
                    for i from 3
                    do (ppc-emit-load-word buf r +ppc-r12+ (* i ws)))
              (ppc-emit-load-word buf +ppc-r0+ +ppc-r12+ (* 2 ws))
              (ppc-emit-mtctr buf +ppc-r0+)
              (ppc-emit-load-word buf +ppc-r31+ +ppc-r12+ ws)
              (ppc-emit-load-word buf +ppc-r1+ +ppc-r12+ 0)
              (ppc-emit-li buf +ppc-r3+ #xDEAD1009)             ; second return: T
              (ppc-emit-bctr buf)
              (ppc-emit-label buf nohandler)
              (ppc-emit-tw buf 31 0 0)))
           ((= code #x0512)
            ;; CLEAR-HANDLER: pop one frame; r3 (the result) is untouched.
            (ppc-emit-handler-pop buf))
           ((= code #x0530)
            ;; COPY-OVERFLOW-ARGS: the &rest/&key prologue's RUNTIME copy of
            ;; arguments 4.. into frame slots 4.., as translate-x64/i386/riscv
            ;; do.  Unrolled -- for i = 4..31: stop once nargs <= i, else copy
            ;; one word -- so it needs only the count and r0, no loop state.
            ;; Argument i is at old-r1 + (i-4)*ws = VFP + fs + (i-4)*ws, which
            ;; stays intact because LR no longer lives there (ppc-lr-slot).
            ;; Capped at 32 arguments like the other back ends.
            (let ((done (mvm-make-label))
                  (ws (ppc-word-size))
                  (fs (ppc-frame-size)))
              (ppc-emit-load-abs buf +ppc-scratch1+ (ppc-nargs-addr))
              (loop for i from 4 below 32
                    do (ppc-emit-cmpi-word buf +ppc-scratch1+ i)
                       (ppc-emit-ble buf done)
                       (ppc-emit-load-word buf +ppc-r0+ +ppc-r31+ (+ fs (* (- i 4) ws)))
                       (ppc-emit-store-word buf +ppc-r0+ +ppc-r31+
                                            (+ (ppc-frame-slot-base) (* i ws))))
              (ppc-emit-label buf done)))
           ((and (= code #x0300) *ppc-linux-mode*)
            ;; HOSTED: the serial write becomes write(1, &byte, 1).  The byte
            ;; goes on the stack because write(2) wants an ADDRESS, and it is
            ;; untagged into r11 FIRST because r3 has to be freed for the fd.
            ;;
            ;; -16 of stack below r1 is scratch by the PowerPC ABI, but the byte
            ;; is stored at 0(r1) AFTER the bump, i.e. inside the frame this
            ;; sequence owns -- writing below r1 without moving it is what the
            ;; ABI permits a leaf to do and a syscall is not a leaf.
            (ppc-emit-shift-right-arith-imm buf +ppc-r11+ +ppc-r3+ 1)
            (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ (logand -16 #xFFFF))
            (ppc-emit-stb buf +ppc-r11+ +ppc-r1+ 0)
            (ppc-emit-li buf +ppc-r3+ 1)               ; fd = stdout
            (ppc-emit-mr buf +ppc-r4+ +ppc-r1+)        ; buf
            (ppc-emit-li buf +ppc-r5+ 1)               ; count
            (ppc-emit-li buf +ppc-r0+ +ppc-linux-sys-write+)
            (ppc-emit-sc buf)
            (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 16))
           ((and (= code #x0301) *ppc-linux-mode*)
            ;; HOSTED: serial read becomes read(0, &byte, 1), and the byte comes
            ;; back TAGGED in V0 (r3) so the contract matches the bare arm.
            (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ (logand -16 #xFFFF))
            (ppc-emit-li buf +ppc-r3+ 0)               ; fd = stdin
            (ppc-emit-mr buf +ppc-r4+ +ppc-r1+)
            (ppc-emit-li buf +ppc-r5+ 1)
            (ppc-emit-li buf +ppc-r0+ +ppc-linux-sys-read+)
            (ppc-emit-sc buf)
            (ppc-emit-lbz buf +ppc-r11+ +ppc-r1+ 0)
            (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 16)
            (ppc-emit-add buf +ppc-r3+ +ppc-r11+ +ppc-r11+))   ; tag: x*2
           ((and (= code #x0500) *ppc-linux-mode*)
            ;; HOSTED: exit(status), status arriving TAGGED in V0.
            (ppc-emit-shift-right-arith-imm buf +ppc-r3+ +ppc-r3+ 1)
            (ppc-emit-li buf +ppc-r0+ +ppc-linux-sys-exit+)
            (ppc-emit-sc buf))
           ((= code #x0300)
            ;; Serial write: V0 (r3) contains tagged fixnum char code
            (if *ppc-64-bit*
                ;; PPC64 powernv: direct MMIO to LPC UART at 0x60300D00103F8
                (progn
                  ;; Untag: sradi r0, r3, 1
                  (ppc-emit-shift-right-arith-imm buf +ppc-r0+ +ppc-r3+ 1)
                  ;; Load LPC UART address into r11
                  (ppc-emit-li buf +ppc-r11+ #x60300D00103F8)
                  ;; Store byte to UART data register: stb r0, 0(r11)
                  (ppc-emit-stb buf +ppc-r0+ +ppc-r11+ 0))
                ;; PPC32 ppce500: MMIO UART at 0xE0004500
                (progn
                  ;; Untag: srawi r0, r3, 1
                  (ppc-emit-srawi buf +ppc-r0+ +ppc-r3+ 1)
                  ;; Load UART base into r11: lis r11, 0xE000; ori r11, r11, 0x4500
                  (ppc-emit-addis buf +ppc-r11+ 0 #xE000)
                  (ppc-emit-ori buf +ppc-r11+ +ppc-r11+ #x4500)
                  ;; Store byte to UART data register: stb r0, 0(r11)
                  (ppc-emit-stb buf +ppc-r0+ +ppc-r11+ 0))))
           (t
            ;; Real CPU trap
            (ppc-emit-addi buf +ppc-r0+ 0 code)
            (ppc-emit-tw buf 31 +ppc-r0+ +ppc-r0+)))))

      ;;; --- Data Movement ---
      (#.+op-mov+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((pd (ppc-vreg-phys vd))
               (ps (ppc-vreg-phys vs)))
           (cond
             ;; Both in registers
             ((and pd ps)
              (unless (= pd ps)
                (ppc-emit-mr buf pd ps)))
             ;; Source spills, dest in register
             ((and pd (not ps))
              (ppc-emit-load-word buf pd +ppc-r31+ (ppc-spill-offset vs)))
             ;; Source in register, dest spills
             ((and (not pd) ps)
              (ppc-emit-store-word buf ps +ppc-r31+ (ppc-spill-offset vd)))
             ;; Both spill: load into scratch, then store
             (t
              (ppc-emit-load-word buf +ppc-scratch1+ +ppc-r31+ (ppc-spill-offset vs))
              (ppc-emit-store-word buf +ppc-scratch1+ +ppc-r31+ (ppc-spill-offset vd)))))))

      (#.+op-li+
       (let ((vd (first operands))
             (imm (second operands)))
         (let ((pd (ppc-vreg-phys vd)))
           (if pd
               (ppc-emit-li buf pd imm)
               (progn
                 (ppc-emit-li buf +ppc-scratch1+ imm)
                 (ppc-store-vreg buf vd +ppc-scratch1+))))))

      (#.+op-push+
       (let ((vs (first operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+))
               (ws (ppc-word-size)))
           ;; Pre-decrement stack, then store
           (if *ppc-64-bit*
               (ppc-emit-word buf (ppc-ds-form 62 ps +ppc-r1+ (logand (- ws) #xFFFC) 1)) ; stdu
               (ppc-emit-stwu buf ps +ppc-r1+ (logand (- ws) #xFFFF))))))

      (#.+op-pop+
       (let ((vd (first operands)))
         (let ((pd (ppc-vreg-phys vd))
               (ws (ppc-word-size)))
           (if pd
               (progn
                 (ppc-emit-load-word buf pd +ppc-r1+ 0)
                 (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ ws))
               (progn
                 (ppc-emit-load-word buf +ppc-scratch1+ +ppc-r1+ 0)
                 (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ ws)
                 (ppc-store-vreg buf vd +ppc-scratch1+))))))

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
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-add buf pd pa pb)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

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
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           ;; SUBF rd, rb, ra  means rd = ra - rb
           ;; We want vd = va - vb, so: SUBF rd, pb, pa
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-subf buf pd pb pa)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

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
         ;; Tagged fixnum multiply: (va >> 1) * vb keeps the tag
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             ;; Untag va: shift right arith 1
             (ppc-emit-shift-right-arith-imm buf +ppc-r0+ pa 1)
             ;; Multiply
             (ppc-emit-mul-word buf pd +ppc-r0+ pb)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-mul26lo+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; Low 26 bits of untag(Va)*untag(Vb), tagged
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-shift-right-arith-imm buf +ppc-r0+ pa 1)  ; untag va
             (ppc-emit-shift-right-arith-imm buf +ppc-scratch2+ pb 1) ; untag vb
             (ppc-emit-mul-word buf pd +ppc-r0+ +ppc-scratch2+)
             ;; Mask to 26 bits: RLDICL rd, rs, 0, 38 (64-bit) or RLWINM rd, rs, 0, 6, 31 (32-bit)
             (if *ppc-64-bit*
                 (ppc-emit-rldicl buf pd pd 0 38)
                 (ppc-emit-rlwinm buf pd pd 0 6 31))
             ;; Retag: ADD pd, pd, pd (shift left 1)
             (ppc-emit-add buf pd pd pd)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-mul26hi+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; Bits 26+ of untag(Va)*untag(Vb), tagged
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-shift-right-arith-imm buf +ppc-r0+ pa 1)  ; untag va
             (ppc-emit-shift-right-arith-imm buf +ppc-scratch2+ pb 1) ; untag vb
             (ppc-emit-mul-word buf pd +ppc-r0+ +ppc-scratch2+)
             (ppc-emit-shift-right-arith-imm buf pd pd 26) ; >>26
             ;; Retag: ADD pd, pd, pd (shift left 1)
             (ppc-emit-add buf pd pd pd)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-div+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; Tagged fixnum divide: divide, then re-tag
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             ;; divide (tagged/tagged = untagged)
             (ppc-emit-div-word buf pd pa pb)
             ;; Re-tag: shl 1
             ;; (a*2) / (b*2) = a/b (untagged), so re-tag by shifting left 1
             (if *ppc-64-bit*
                 (ppc-emit-word buf (ppc-md-form 30 pd pd 1 63 1)) ; rldicr pd,pd,1,62
                 (ppc-emit-rlwinm buf pd pd 1 0 30)) ; rlwinm pd,pd,1,0,30 = shl 1
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-mod+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         ;; mod = a - (a/b)*b
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-div-word buf +ppc-r0+ pa pb)
             (ppc-emit-mul-word buf +ppc-r0+ +ppc-r0+ pb)
             ;; subf pd, r0, pa  (pd = pa - r0)
             (ppc-emit-subf buf pd +ppc-r0+ pa)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-neg+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-neg buf pd ps)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-inc+
       (let ((vd (first operands)))
         (let ((pd (ppc-vreg-phys vd)))
           (if pd
               ;; Tagged increment: add 2 (fixnum 1 = 1 << 1 = 2)
               (ppc-emit-addi buf pd pd 2)
               (progn
                 (ppc-load-vreg buf +ppc-scratch1+ vd)
                 (ppc-emit-addi buf +ppc-scratch1+ +ppc-scratch1+ 2)
                 (ppc-store-vreg buf vd +ppc-scratch1+))))))

      (#.+op-dec+
       (let ((vd (first operands)))
         (let ((pd (ppc-vreg-phys vd)))
           (if pd
               ;; Tagged decrement: subtract 2 (fixnum 1 = 1 << 1 = 2)
               (ppc-emit-addi buf pd pd (logand -2 #xFFFF))
               (progn
                 (ppc-load-vreg buf +ppc-scratch1+ vd)
                 (ppc-emit-addi buf +ppc-scratch1+ +ppc-scratch1+ (logand -2 #xFFFF))
                 (ppc-store-vreg buf vd +ppc-scratch1+))))))

      ;;; --- Bitwise ---
      (#.+op-and+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-and buf pd pa pb)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-or+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-or buf pd pa pb)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-xor+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-xor buf pd pa pb)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-shl+
       (let ((vd (first operands))
             (vs (second operands))
             (amt (third operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-addi buf +ppc-scratch2+ 0 amt)
             (ppc-emit-shift-left buf pd ps +ppc-scratch2+)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-shr+
       (let ((vd (first operands))
             (vs (second operands))
             (amt (third operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-addi buf +ppc-scratch2+ 0 amt)
             (ppc-emit-shift-right buf pd ps +ppc-scratch2+)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-sar+
       (let ((vd (first operands))
             (vs (second operands))
             (amt (third operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-shift-right-arith-imm buf pd ps amt)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-shlv+
       ;; (shlv Vd Vs Vc) — shift left by register
       (let ((vd (first operands))
             (vs (second operands))
             (vc (third operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+))
               (pc (vreg-or-scratch vc +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-shift-left buf pd ps pc)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-sarv+
       ;; (sarv Vd Vs Vc) — arithmetic shift right by register
       (let ((vd (first operands))
             (vs (second operands))
             (vc (third operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+))
               (pc (vreg-or-scratch vc +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-shift-right-arith buf pd ps pc)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-ldb+
       (let ((vd (first operands))
             (vs (second operands))
             (pos (third operands))
             (size (fourth operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             ;; Extract bit field: rotate right by pos, mask size bits
             (if *ppc-64-bit*
                 (ppc-emit-rldicl buf pd ps (logand (- 64 pos) #x3F) (- 64 size))
                 (ppc-emit-rlwinm buf pd ps (logand (- 32 pos) #x1F)
                                  (- 32 size) 31))
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      ;;; --- Comparison ---
      (#.+op-cmp+
       (let ((va (first operands))
             (vb (second operands)))
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           (ppc-emit-cmp-word buf pa pb))))

      (#.+op-test+
       (let ((va (first operands))
             (vb (second operands)))
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+)))
           ;; AND and set CR0: AND. r0, pa, pb
           (ppc-emit-word buf (ppc-x-form 31 pa +ppc-r0+ pb 28 1)))))

      ;;; --- Branches ---
      (#.+op-br+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))  ; adjusted by decoder
              (label (ensure-label target-pc)))
         (ppc-emit-b buf label)))

      (#.+op-beq+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (ppc-emit-beq buf label)))

      (#.+op-bne+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (ppc-emit-bne buf label)))

      (#.+op-blt+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (ppc-emit-blt buf label)))

      (#.+op-bge+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (ppc-emit-bge buf label)))

      (#.+op-ble+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (ppc-emit-ble buf label)))

      (#.+op-bgt+
       (let* ((off16 (first operands))
              (target-pc (+ mvm-pc off16))
              (label (ensure-label target-pc)))
         (ppc-emit-bgt buf label)))

      (#.+op-bnull+
       (let ((vs (first operands))
             (off16 (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+))
               (target-pc (+ mvm-pc off16)))
           (let ((label (ensure-label target-pc)))
             (ppc-emit-cmp-word buf ps +ppc-r21+)
             (ppc-emit-beq buf label)))))

      (#.+op-bnnull+
       (let ((vs (first operands))
             (off16 (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+))
               (target-pc (+ mvm-pc off16)))
           (let ((label (ensure-label target-pc)))
             (ppc-emit-cmp-word buf ps +ppc-r21+)
             (ppc-emit-bne buf label)))))

      ;;; --- List Operations ---
      (#.+op-car+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-andi-dot buf +ppc-r0+ ps #xF)
             (ppc-emit-cmpi-word buf +ppc-r0+ +tag-cons+)
             (let ((ok-label (mvm-make-label)))
               (ppc-emit-beq buf ok-label)
               (ppc-emit-tw buf 31 0 0)
               (ppc-emit-label buf ok-label))
             ;; Strip tag and load car
             (ppc-emit-addi buf +ppc-scratch2+ ps (logand (- +tag-cons+) #xFFFF))
             (ppc-emit-load-word buf pd +ppc-scratch2+ 0)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-cdr+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+))
                 (ws (ppc-word-size)))
             (ppc-emit-andi-dot buf +ppc-r0+ ps #xF)
             (ppc-emit-cmpi-word buf +ppc-r0+ +tag-cons+)
             (let ((ok-label (mvm-make-label)))
               (ppc-emit-beq buf ok-label)
               (ppc-emit-tw buf 31 0 0)
               (ppc-emit-label buf ok-label))
             ;; Strip tag, load cdr (second word of cons cell)
             (ppc-emit-addi buf +ppc-scratch2+ ps (logand (- +tag-cons+) #xFFFF))
             (ppc-emit-load-word buf pd +ppc-scratch2+ ws)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-cons+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
         (let ((pa (vreg-or-scratch va +ppc-scratch1+))
               (pb (vreg-or-scratch vb +ppc-scratch2+))
               (ws (ppc-word-size)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             ;; Store car at VA (alloc pointer)
             (ppc-emit-store-word buf pa +ppc-r19+ 0)
             ;; Store cdr at VA+ws
             (ppc-emit-store-word buf pb +ppc-r19+ ws)
             ;; Tag the pointer
             (ppc-emit-ori buf pd +ppc-r19+ +tag-cons+)
             ;; Bump alloc pointer: VA += 2*ws
             (ppc-emit-addi buf +ppc-r19+ +ppc-r19+ 16)
             ;; A CONS TAKES SIXTEEN BYTES AT BOTH WIDTHS, not 2*ws.  Objects are
             ;; rounded to 16 and pointer types are read from the low FOUR bits,
             ;; so the heap pointer must never sit at 8 mod 16: on ppc32 an odd
             ;; number of 8-byte conses left it there, and the next object's tag
             ;; 9 read as nibble 1 (a cons) while the next cons's 1 read as 9.
             ;; r26-callee-alloc answered 1042 on ppc32 (and riscv32, 68k).
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      ;; R0 IS NOT A BASE REGISTER.  In a D-form load or store, rA=0 means the
      ;; LITERAL VALUE ZERO, not the contents of r0 -- so `addi r0,obj,-tag;
      ;; stw val,0(r0)' does not store into the cons, it stores to ABSOLUTE
      ;; ADDRESS 0.  These two wrote there on every PowerPC image ever built.
      ;;
      ;; It went unseen because bare ppc32 loads at address 0 with RAM from 0:
      ;; the store landed on the image's own first words and nothing read them
      ;; again.  The HOSTED port has nothing mapped at 0, so it is a SIGSEGV --
      ;; which is how this was found, and is the argument for hosted ports as a
      ;; correctness instrument rather than just a convenience.
      ;;
      ;; The address goes in SCRATCH1: PS comes from vreg-or-scratch with
      ;; scratch2 as its fallback, so PS can never BE scratch1, and PD is dead
      ;; the moment the address is formed.
      (#.+op-setcar+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((pd (vreg-or-scratch vd +ppc-scratch1+))
               (ps (vreg-or-scratch vs +ppc-scratch2+)))
           (ppc-emit-addi buf +ppc-scratch1+ pd (logand (- +tag-cons+) #xFFFF))
           (ppc-emit-store-word buf ps +ppc-scratch1+ 0))))

      (#.+op-setcdr+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((pd (vreg-or-scratch vd +ppc-scratch1+))
               (ps (vreg-or-scratch vs +ppc-scratch2+)))
           (ppc-emit-addi buf +ppc-scratch1+ pd (logand (- +tag-cons+) #xFFFF))
           (ppc-emit-store-word buf ps +ppc-scratch1+ (ppc-word-size)))))

      (#.+op-consp+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             ;; NIL MUST BE EXCLUDED BEFORE THE TAG IS TESTED.  NIL's low nibble
             ;; IS +tag-cons+ by design -- that is what lets car/cdr of NIL be a
             ;; plain load into a NIL-filled page -- so the tag test alone
             ;; answers T for NIL.  x64 compares against R15 and i386 against
             ;; *vn-addr* first; PPC did not, and r24-nil-atom measured the
             ;; result as 12 on both widths.  translate-i386's comment records
             ;; what it costs: `(loop while (consp cur) ... (setq cur (cdr cur)))'
             ;; never terminates, because (car NIL) hands back NIL and the walk
             ;; recurses on NIL forever.  The four-bit mask below is already
             ;; right, which is why (consp T) was the half that worked.
             (let ((true-label (mvm-make-label))
                   (false-label (mvm-make-label))
                   (done-label (mvm-make-label)))
               (ppc-emit-cmp-word buf ps +ppc-r21+)
               (ppc-emit-beq buf false-label)
               (ppc-emit-andi-dot buf +ppc-r0+ ps #xF)
               (ppc-emit-cmpi-word buf +ppc-r0+ +tag-cons+)
               (ppc-emit-beq buf true-label)
               (ppc-emit-label buf false-label)
               (ppc-emit-mr buf pd +ppc-r21+)
               (ppc-emit-b buf done-label)
               (ppc-emit-label buf true-label)
               (ppc-emit-addi buf pd 0 +mvm-t+)
               (ppc-emit-label buf done-label))
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-atom+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             ;; The exact inverse of +op-consp+'s NIL exclusion: (atom NIL) is T.
             (let ((true-label (mvm-make-label))
                   (done-label (mvm-make-label)))
               (ppc-emit-cmp-word buf ps +ppc-r21+)
               (ppc-emit-beq buf true-label)          ; NIL is an atom
               (ppc-emit-andi-dot buf +ppc-r0+ ps #xF)
               (ppc-emit-cmpi-word buf +ppc-r0+ +tag-cons+)
               (ppc-emit-bne buf true-label)
               ;; Is a cons: return NIL
               (ppc-emit-mr buf pd +ppc-r21+)
               (ppc-emit-b buf done-label)
               ;; Not a cons (or NIL): return T
               (ppc-emit-label buf true-label)
               (ppc-emit-addi buf pd 0 +mvm-t+)
               (ppc-emit-label buf done-label))
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      ;;; --- Object Operations ---
      (#.+op-alloc-obj+
       (let ((vd (first operands))
             (size (second operands))
             (subtag (third operands)))
         (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+))
               (ws (ppc-word-size)))
           ;; Align to 16 bytes to keep cons alloc pointer aligned
          (let ((total-bytes (logand (+ (* (1+ size) ws) 15) (lognot 15))))
             ;; Build header: (subtag << 8) | +tag-object+
             (ppc-emit-addi buf +ppc-r0+ 0 (logior (ash subtag 8) +tag-object+))
             (ppc-emit-store-word buf +ppc-r0+ +ppc-r19+ 0)
             (ppc-emit-ori buf pd +ppc-r19+ +tag-object+)
             (ppc-emit-addi buf +ppc-r19+ +ppc-r19+ total-bytes)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-obj-ref+
       (let ((vd (first operands))
             (vobj (second operands))
             (idx (third operands)))
         (let ((ws (ppc-word-size))
               (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
           (if (= vobj +vreg-vfp+)
               ;; Frame slot access: use safe VFP-relative offset above spill area
               (ppc-emit-load-word buf pd +ppc-r31+
                                   (+ (ppc-frame-slot-base) (* idx ws)))
               ;; Normal object slot access
               (let ((pobj (vreg-or-scratch vobj +ppc-scratch1+)))
                 ;; R0 CANNOT BE A BASE REGISTER ON POWERPC.  In a D-form load or
               ;; store, rA=0 means the literal value zero, not the contents of
               ;; r0 -- so `addi r0,pobj,-9; lwz pd,off(r0)` did not read the
               ;; object at all, it read absolute address `off`.  The address
               ;; goes in r12 instead.
                 (ppc-emit-addi buf +ppc-scratch2+ pobj
                                (logand (- +tag-object+) #xFFFF))
                 (ppc-emit-load-word buf pd +ppc-scratch2+ (* (1+ idx) ws))))
           (unless (ppc-vreg-phys vd)
             (ppc-store-vreg buf vd pd)))))

      (#.+op-obj-set+
       (let ((vobj (first operands))
             (idx (second operands))
             (vs (third operands)))
         (let ((ws (ppc-word-size)))
           (if (= vobj +vreg-vfp+)
               ;; Frame slot store: use safe VFP-relative offset above spill area
               (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
                 (ppc-emit-store-word buf ps +ppc-r31+
                                      (+ (ppc-frame-slot-base) (* idx ws))))
               ;; Normal object slot store
               ;; See the r0-is-not-a-base note in :obj-ref above.  The
               ;; address is finished into r12 FIRST, which frees r11 for the
               ;; value even when the object itself arrived there.
               (let ((pobj (vreg-or-scratch vobj +ppc-scratch1+)))
                 (ppc-emit-addi buf +ppc-scratch2+ pobj
                                (logand (- +tag-object+) #xFFFF))
                 (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
                   (ppc-emit-store-word buf ps +ppc-scratch2+
                                        (* (1+ idx) ws))))))))

      (#.+op-obj-tag+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ppc-emit-andi-dot buf pd ps #xF)
             ;; Tag as fixnum: shl 1
             (ppc-emit-addi buf +ppc-r0+ 0 1)
             (ppc-emit-shift-left buf pd pd +ppc-r0+)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-obj-subtag+
       (let ((vd (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             ;; R0 IS NOT A BASE REGISTER -- see :setcar.  `lwz pd,0(r0)' read
             ;; ABSOLUTE ADDRESS 0 instead of the object header, so every subtag
             ;; dispatch (%prim-aref's u8 check among them) branched on whatever
             ;; happened to be at address 0.  SCRATCH2 holds the address: PS is
             ;; scratch1-or-a-vreg and PD is scratch1-or-a-vreg, so neither can
             ;; be scratch2, and the address is dead after the load -- which is
             ;; why the tag-shift below may reuse scratch2.
             (ppc-emit-addi buf +ppc-scratch2+ ps (logand (- +tag-object+) #xFFFF))
             (ppc-emit-load-word buf pd +ppc-scratch2+ 0)   ; load header
             (ppc-emit-shift-right-arith-imm buf pd pd 8)  ; shift right 8
             (ppc-emit-andi-dot buf pd pd #xFF)             ; mask 8 bits
             ;; Tag as fixnum
             (ppc-emit-addi buf +ppc-scratch2+ 0 1)
             (ppc-emit-shift-left buf pd pd +ppc-scratch2+)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      ;;; --- Memory (raw) ---
      (#.+op-load+
       (let ((vd (first operands))
             (vaddr (second operands))
             (width (third operands)))
         (let ((paddr (vreg-or-scratch vaddr +ppc-scratch1+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
             (ecase width
               (0 (ppc-emit-lbz buf pd paddr 0))   ; u8
               (1 (ppc-emit-lhz buf pd paddr 0))   ; u16
               (2 (ppc-emit-lwz buf pd paddr 0))   ; u32
               (3 (ppc-emit-load-word buf pd paddr 0)))  ; native word
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      (#.+op-store+
       (let ((vaddr (first operands))
             (vs (second operands))
             (width (third operands)))
         (let ((paddr (vreg-or-scratch vaddr +ppc-scratch1+))
               (ps (vreg-or-scratch vs +ppc-scratch2+)))
           (ecase width
             (0 (ppc-emit-stb buf ps paddr 0))   ; u8
             (1 (ppc-emit-sth buf ps paddr 0))   ; u16
             (2 (ppc-emit-stw buf ps paddr 0))   ; u32
             (3 (ppc-emit-store-word buf ps paddr 0))))))

      (#.+op-fence+
       (ppc-emit-sync buf))

      ;;; --- Function Calling ---
      (#.+op-call+
       ;; Target operand is the bytecode offset of the called function.
       (let* ((target-offset (first operands))
              (label (gethash target-offset label-map)))
         (if label
             (ppc-emit-bl buf label)
             ;; Unknown target: emit BL with no fixup (will jump to next insn)
             (ppc-emit-bl buf nil))))

      ((#.+op-fadd+ #.+op-fsub+ #.+op-fmul+ #.+op-fdiv+)
       ;; Double arithmetic: unbox both operands into f0/f1 through memory
       ;; (ppc-float-unbox), operate, box the result.  The operands are held
       ;; in r11/r12 (never r0, which the unbox uses to carry chunks).
       (let* ((vd (first operands))
              (pa (vreg-or-scratch (second operands) +ppc-scratch1+))
              (pb (vreg-or-scratch (third operands) +ppc-scratch2+)))
         (ppc-float-unbox buf pa 0)
         (ppc-float-unbox buf pb 1)
         (cond ((= opcode +op-fadd+) (ppc-emit-fadd buf 0 0 1))
               ((= opcode +op-fsub+) (ppc-emit-fsub buf 0 0 1))
               ((= opcode +op-fmul+) (ppc-emit-fmul buf 0 0 1))
               (t                    (ppc-emit-fdiv buf 0 0 1)))
         (ppc-float-box buf 0 +ppc-scratch1+)
         (ppc-store-vreg buf vd +ppc-scratch1+)))

      (#.+op-itof+
       ;; Tagged fixnum -> fresh double.
       ;;   ppc64: untag, STD to scratch, LFD, FCFID (int64 -> double).
       ;;   ppc32: there is NO FCFID on 32-bit PowerPC, so the classic trick:
       ;;     the double with high word #x43300000 and low word (x XOR #x80000000)
       ;;     is 2^52 + 2^31 + x exactly; subtract 2^52 + 2^31 (the same high
       ;;     word over #x80000000) and x is left, exactly.
       (let* ((vd (first operands))
              (ps (vreg-or-scratch (second operands) +ppc-scratch1+)))
         (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ -16)
         (if *ppc-64-bit*
             (progn
               (ppc-emit-sradi buf +ppc-r0+ ps 1)
               (ppc-emit-std buf +ppc-r0+ +ppc-r1+ 0)
               (ppc-emit-lfd buf 0 +ppc-r1+ 0)
               (ppc-emit-fcfid buf 0 0))
             (progn
               (ppc-emit-srawi buf +ppc-r0+ ps 1)
               (ppc-emit-addis buf +ppc-scratch2+ 0 #x8000)        ; lis r12,0x8000
               (ppc-emit-xor buf +ppc-r0+ +ppc-r0+ +ppc-scratch2+)  ; x ^ 0x80000000
               (ppc-emit-stw buf +ppc-r0+ +ppc-r1+ 4)
               (ppc-emit-stw buf +ppc-scratch2+ +ppc-r1+ 12)       ; 0x80000000
               (ppc-emit-addis buf +ppc-scratch2+ 0 #x4330)        ; lis r12,0x4330
               (ppc-emit-stw buf +ppc-scratch2+ +ppc-r1+ 0)
               (ppc-emit-stw buf +ppc-scratch2+ +ppc-r1+ 8)
               (ppc-emit-lfd buf 0 +ppc-r1+ 0)
               (ppc-emit-lfd buf 1 +ppc-r1+ 8)
               (ppc-emit-fsub buf 0 0 1)))
         (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 16)
         (ppc-float-box buf 0 +ppc-scratch1+)
         (ppc-store-vreg buf vd +ppc-scratch1+)))

      (#.+op-ftoi+
       ;; Double -> tagged fixnum, TRUNCATING (FCTIDZ / FCTIWZ, the Z being
       ;; round-toward-zero).  The integer comes out in an FPR, so it goes
       ;; through the scratch once more: STFD, then the low word (ppc32: byte 4,
       ;; big-endian) or the whole doubleword (ppc64).
       (let* ((vd (first operands))
              (ps (vreg-or-scratch (second operands) +ppc-scratch1+)))
         (ppc-float-unbox buf ps 0)
         (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ -16)
         (if *ppc-64-bit*
             (progn (ppc-emit-fctidz buf 0 0)
                    (ppc-emit-stfd buf 0 +ppc-r1+ 0)
                    (ppc-emit-ld buf +ppc-r0+ +ppc-r1+ 0))
             (progn (ppc-emit-fctiwz buf 0 0)
                    (ppc-emit-stfd buf 0 +ppc-r1+ 0)
                    (ppc-emit-lwz buf +ppc-r0+ +ppc-r1+ 4)))
         (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ 16)
         (ppc-emit-add buf +ppc-scratch1+ +ppc-r0+ +ppc-r0+)   ; tag (x2)
         (ppc-store-vreg buf vd +ppc-scratch1+)))

      (#.+op-fn-addr+
       ;; (fn-addr Vd target) -- the native address of a function, TAGGED with
       ;; +tag-function+ (3), which is how funcall dispatch and FUNCTIONP tell it
       ;; from a cons (1) or an object (9).
       ;;
       ;; PowerPC has no AUIPC, so the PC comes from `bl .+4; mflr': the branch
       ;; lands on the very next instruction and leaves its address in LR.
       ;; Clobbering LR mid-function is safe -- the prologue saved it to memory
       ;; and the epilogue reloads it from there, and every call clobbers it
       ;; anyway.  Then @ha/@l of the displacement (fixups :pcrel-ha/:pcrel-lo)
       ;; and OR 3: every instruction is 4 bytes, so a function's address has
       ;; its low two bits free, and BCCTR ignores them, which is why
       ;; +op-call-ind+ below needs no untagging.  Fixed five words.
       (let* ((vd (first operands))
              (idx (second operands))
              (mvm-off (and function-table (gethash idx function-table)))
              (label (and mvm-off (gethash mvm-off label-map)))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         (unless label
           (error "PPC fn-addr: no label for function index ~D" idx))
         (ppc-emit-word buf #x48000005)                 ; bl .+4
         (ppc-emit-mflr buf pd)
         (ppc-emit-addis buf pd pd 0)
         (ppc-emit-fixup buf label :pcrel-ha)
         (ppc-emit-addi buf pd pd 0)
         (ppc-emit-fixup buf label :pcrel-lo)
         (ppc-emit-ori buf pd pd +tag-function+)
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-li-const+
       ;; (li-const Vd idx) -- the TAGGED address of constant-pool slot IDX, not
       ;; known until the image is laid out: a fixed LIS/ORI placeholder recorded
       ;; in *ppc-li-const-patches* and filled by cross.lisp.
       (let* ((vd (first operands))
              (idx (second operands))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         (push (cons (ppc-current-offset buf) idx) *ppc-li-const-patches*)
         (ppc-emit-addis buf pd 0 0)                    ; lis pd, hi16
         (ppc-emit-ori buf pd pd 0)                     ; ori pd, pd, lo16
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-call-ind+
       (let ((vs (first operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           ;; Move target to CTR, then BCTRL
           (ppc-emit-mtctr buf ps)
           (ppc-emit-bctrl buf))))

      (#.+op-ret+
       (ppc-emit-epilogue buf))

      (#.+op-tailcall+
       (let* ((target-offset (first operands))
              (label (gethash target-offset label-map))
              (ws (ppc-word-size))
              (base (* 6 ws)))
         ;; Restore callee-saved regs
         (ppc-emit-load-word buf +ppc-r14+ +ppc-r1+ base)
         (ppc-emit-load-word buf +ppc-r15+ +ppc-r1+ (+ base (* 1 ws)))
         (ppc-emit-load-word buf +ppc-r16+ +ppc-r1+ (+ base (* 2 ws)))
         (ppc-emit-load-word buf +ppc-r17+ +ppc-r1+ (+ base (* 3 ws)))
         (ppc-emit-load-word buf +ppc-r18+ +ppc-r1+ (+ base (* 4 ws)))
         (ppc-emit-load-word buf +ppc-r31+ +ppc-r1+ (+ base (* 8 ws)))
         ;; Restore LR from this frame -- BEFORE the frame is popped.
         (ppc-emit-load-word buf +ppc-r0+ +ppc-r1+ (ppc-lr-slot))
         (ppc-emit-mtlr buf +ppc-r0+)
         ;; Restore stack
         (ppc-emit-addi buf +ppc-r1+ +ppc-r1+ (ppc-frame-size))
         ;; Branch to target
         (ppc-emit-b buf label)))

      ;;; --- GC / Allocation ---
      (#.+op-alloc-cons+
       (let ((vd (first operands)))
         (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+))
               (ws (ppc-word-size)))
           (ppc-emit-ori buf pd +ppc-r19+ +tag-cons+)
           ;; Bump alloc pointer by 2*ws (cons cell = 2 words)
           (ppc-emit-addi buf +ppc-r19+ +ppc-r19+ 16) ; 16 at both widths
           (unless (ppc-vreg-phys vd)
             (ppc-store-vreg buf vd pd)))))

      ((#.+op-gc-check+ #.+op-gc-check-n+ #.+op-gc-check-r+)
       ;; :GC-CHECK-N and :GC-CHECK-R share this clause.  They carry an
       ;; allocation SIZE (a constant, or a runtime value) so a back end can
       ;; check `VA + n < VL` rather than `VA < VL`.  None of x64, i386 or
       ;; aarch64 uses the size either -- translate-i386 routes all three to
       ;; the same plain VA-vs-VL comparison -- so doing the same here matches
       ;; the reference back ends exactly.  What it replaces is worse than an
       ;; imprecise check: RISC-V/PPC/68k TRAPPED on these opcodes, and
       ;; make-array emits :gc-check-n, so no array could be allocated at all.
       (let ((gc-label (mvm-make-label))
             (ok-label (mvm-make-label)))
         (ppc-emit-cmpl-word buf +ppc-r19+ +ppc-r20+)
         (ppc-emit-blt buf ok-label)
         (ppc-emit-label buf gc-label)
         (ppc-emit-tw buf 31 0 0)
         (ppc-emit-label buf ok-label)))

      (#.+op-write-barrier+
       (let ((vobj (first operands)))
         (let ((pobj (vreg-or-scratch vobj +ppc-scratch1+)))
           (ppc-emit-shift-right-arith-imm buf +ppc-r0+ pobj 12)
           (ppc-emit-nop buf))))

      ;;; --- Actor/Concurrency ---
      (#.+op-save-ctx+
       (let ((ws (ppc-word-size)))
         (ppc-emit-store-word buf +ppc-r3+ +ppc-r31+ +ppc-spill-base-offset+)
         (ppc-emit-store-word buf +ppc-r4+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 1 ws)))
         (ppc-emit-store-word buf +ppc-r5+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 2 ws)))
         (ppc-emit-store-word buf +ppc-r6+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 3 ws)))
         (ppc-emit-mflr buf +ppc-r0+)
         (ppc-emit-store-word buf +ppc-r0+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 4 ws)))))

      (#.+op-restore-ctx+
       (let ((ws (ppc-word-size)))
         (ppc-emit-load-word buf +ppc-r3+ +ppc-r31+ +ppc-spill-base-offset+)
         (ppc-emit-load-word buf +ppc-r4+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 1 ws)))
         (ppc-emit-load-word buf +ppc-r5+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 2 ws)))
         (ppc-emit-load-word buf +ppc-r6+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 3 ws)))
         (ppc-emit-load-word buf +ppc-r0+ +ppc-r31+ (+ +ppc-spill-base-offset+ (* 4 ws)))
         (ppc-emit-mtlr buf +ppc-r0+)))

      (#.+op-yield+
       (ppc-emit-nop buf)
       (ppc-emit-nop buf))

      (#.+op-atomic-xchg+
       (let ((vd (first operands))
             (vaddr (second operands))
             (vs (third operands)))
         (let ((paddr (vreg-or-scratch vaddr +ppc-scratch1+))
               (ps (vreg-or-scratch vs +ppc-scratch2+)))
           (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+))
                 (loop-label (mvm-make-label)))
             (ppc-emit-label buf loop-label)
             (ppc-emit-load-reserve buf pd 0 paddr)
             (ppc-emit-store-cond buf ps 0 paddr)
             (ppc-emit-bne buf loop-label)
             (unless (ppc-vreg-phys vd)
               (ppc-store-vreg buf vd pd))))))

      ;;; --- System / Platform ---
      (#.+op-io-read+
       (let ((vd (first operands))
             (port (second operands))
             (width (third operands)))
         (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
           (ppc-emit-li buf +ppc-scratch2+ port)
           (ecase width
             (0 (ppc-emit-lbz buf pd +ppc-scratch2+ 0))
             (1 (ppc-emit-lhz buf pd +ppc-scratch2+ 0))
             (2 (ppc-emit-lwz buf pd +ppc-scratch2+ 0))
             (3 (ppc-emit-load-word buf pd +ppc-scratch2+ 0)))
           (unless (ppc-vreg-phys vd)
             (ppc-store-vreg buf vd pd)))))

      (#.+op-io-write+
       (let ((port (first operands))
             (vs (second operands))
             (width (third operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch2+)))
           (ppc-emit-li buf +ppc-scratch1+ port)
           (ecase width
             (0 (ppc-emit-stb buf ps +ppc-scratch1+ 0))
             (1 (ppc-emit-sth buf ps +ppc-scratch1+ 0))
             (2 (ppc-emit-stw buf ps +ppc-scratch1+ 0))
             (3 (ppc-emit-store-word buf ps +ppc-scratch1+ 0))))))

      (#.+op-halt+
       (let ((halt-label (mvm-make-label)))
         (ppc-emit-label buf halt-label)
         (ppc-emit-b buf halt-label)))

      (#.+op-cli+
       (ppc-emit-word buf (logior (ash 31 26) (ash 163 1))))

      (#.+op-sti+
       (ppc-emit-word buf (logior (ash 31 26) (ash 1 15) (ash 163 1))))

      (#.+op-percpu-ref+
       (let ((vd (first operands))
             (offset (second operands)))
         (let ((pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
           (ppc-emit-load-word buf pd +ppc-r13+ offset)
           (unless (ppc-vreg-phys vd)
             (ppc-store-vreg buf vd pd)))))

      (#.+op-percpu-set+
       (let ((offset (first operands))
             (vs (second operands)))
         (let ((ps (vreg-or-scratch vs +ppc-scratch1+)))
           (ppc-emit-store-word buf ps +ppc-r13+ offset))))

      ;;; --- Arrays ---
      ;; Object layout as alloc-obj/obj-ref already use it here: tag 9
      ;; (+tag-object+), header word at obj-9, element k at obj-9 + (1+k)*ws.
      ;; The index arrives TAGGED (2k), so k*ws == tagged*(ws/2) and the
      ;; element sits at obj + tagged*(ws/2) + ws - 9.
      (#.+op-alloc-array+
       ;; (alloc-array Vd Vcount) — Vcount is UNTAGGED (the compiler SAR'd it).
       (let* ((vd (first operands))
              (pc (vreg-or-scratch (second operands) +ppc-scratch1+))
              (ws (ppc-word-size))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         ;; header = (count << 8) | #x32   (array subtag, as on i386)
         (ppc-emit-addi buf +ppc-scratch2+ 0 8)
         (ppc-emit-shift-left buf +ppc-r0+ pc +ppc-scratch2+)
         (ppc-emit-ori buf +ppc-r0+ +ppc-r0+ #x32)
         (ppc-emit-store-word buf +ppc-r0+ +ppc-r19+ 0)
         ;; bytes = align16((count + 1) * ws), computed in r0
         (ppc-emit-addi buf +ppc-r0+ pc 1)
         (ppc-emit-addi buf +ppc-scratch2+ 0 (if (= ws 8) 3 2))
         (ppc-emit-shift-left buf +ppc-r0+ +ppc-r0+ +ppc-scratch2+)
         (ppc-emit-addi buf +ppc-r0+ +ppc-r0+ 15)
         (ppc-emit-andi-dot buf +ppc-r0+ +ppc-r0+ #xFFF0)
         ;; result = VA | 9, then bump VA (order matters: VA is still the base)
         (ppc-emit-ori buf pd +ppc-r19+ +tag-object+)
         (ppc-emit-add buf +ppc-r19+ +ppc-r19+ +ppc-r0+)
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-aref+
       (let* ((vd (first operands))
              (pobj (vreg-or-scratch (second operands) +ppc-scratch1+))
              (pidx (vreg-or-scratch (third operands) +ppc-scratch2+))
              (ws (ppc-word-size))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         ;; Address in r12 — r0 is not a base register (see :obj-ref).
         (ppc-emit-addi buf +ppc-r0+ 0 (if (= ws 8) 2 1))
         (ppc-emit-shift-left buf +ppc-scratch2+ pidx +ppc-r0+)
         (ppc-emit-add buf +ppc-scratch2+ +ppc-scratch2+ pobj)
         ;; Tag folded into the BASE, so the displacement stays a clean
         ;; multiple of the word size — ld's DS field cannot hold -1.
         (ppc-emit-addi buf +ppc-scratch2+ +ppc-scratch2+
                        (logand (- +tag-object+) #xFFFF))
         (ppc-emit-load-word buf pd +ppc-scratch2+ ws)
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-aset+
       ;; (aset Vobj Vidx Vs)
       (let* ((pobj (vreg-or-scratch (first operands) +ppc-scratch1+))
              (pidx (vreg-or-scratch (second operands) +ppc-scratch2+))
              (ws (ppc-word-size))
              (pval (ppc-vreg-phys (third operands))))
         ;; Address in r12 — r0 is not a base register (see :obj-ref).
         (ppc-emit-addi buf +ppc-r0+ 0 (if (= ws 8) 2 1))
         (ppc-emit-shift-left buf +ppc-scratch2+ pidx +ppc-r0+)
         (ppc-emit-add buf +ppc-scratch2+ +ppc-scratch2+ pobj)
         ;; The value is loaded LAST, into r11, which the address no longer
         ;; needs, so computing the address cannot clobber it.
         (let ((pv (or pval
                       (progn (ppc-load-vreg buf +ppc-scratch1+ (third operands))
                              +ppc-scratch1+))))
           ;; Tag folded into the BASE — see :aref.
           (ppc-emit-addi buf +ppc-scratch2+ +ppc-scratch2+
                          (logand (- +tag-object+) #xFFFF))
           (ppc-emit-store-word buf pv +ppc-scratch2+ ws))))

      (#.+op-array-len+
       ;; count = (header >> 8) & 0xFFFFFF, returned TAGGED.
       (let* ((vd (first operands))
              (pobj (vreg-or-scratch (second operands) +ppc-scratch1+))
              (ws (ppc-word-size))
              (bits (* 8 ws))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         (ppc-emit-addi buf +ppc-scratch2+ pobj
                        (logand (- +tag-object+) #xFFFF))
         (ppc-emit-load-word buf pd +ppc-scratch2+ 0)
         (ppc-emit-addi buf +ppc-r0+ 0 8)
         (ppc-emit-shift-right buf pd pd +ppc-r0+)
         ;; mask to 24 bits by shifting out and back
         (ppc-emit-addi buf +ppc-r0+ 0 (- bits 24))
         (ppc-emit-shift-left buf pd pd +ppc-r0+)
         (ppc-emit-addi buf +ppc-r0+ 0 (- bits 24))
         (ppc-emit-shift-right buf pd pd +ppc-r0+)
         (ppc-emit-addi buf +ppc-r0+ 0 1)
         (ppc-emit-shift-left buf pd pd +ppc-r0+)   ; tag as fixnum
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      ;;; --- Byte vectors and strings ---
      ;; Payload starts right after the header word, at obj - 9 + ws.
      ;; Shapes mirror translate-i386, including which operands arrive
      ;; TAGGED: :alloc-u8 untags its count here, :alloc-string's has already
      ;; been SAR'd by the compiler.
      (#.+op-alloc-u8+
       (let* ((vd (first operands))
              (pc (vreg-or-scratch (second operands) +ppc-scratch1+))
              (ws (ppc-word-size))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         (ppc-emit-shift-right-arith-imm buf +ppc-scratch2+ pc 1)   ; N
         (ppc-emit-addi buf +ppc-r0+ 0 8)
         (ppc-emit-shift-left buf +ppc-r0+ +ppc-scratch2+ +ppc-r0+)
         (ppc-emit-ori buf +ppc-r0+ +ppc-r0+ #x11)                  ; u8 subtag
         (ppc-emit-store-word buf +ppc-r0+ +ppc-r19+ 0)
         ;; bytes = align16(N + ws)
         (ppc-emit-addi buf +ppc-r0+ +ppc-scratch2+ ws)
         (ppc-emit-addi buf +ppc-r0+ +ppc-r0+ 15)
         (ppc-emit-andi-dot buf +ppc-r0+ +ppc-r0+ #xFFF0)
         (ppc-emit-ori buf pd +ppc-r19+ +tag-object+)
         (ppc-emit-add buf +ppc-r19+ +ppc-r19+ +ppc-r0+)
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-alloc-string+
       ;; One character CODE per WORD.
       (let* ((vd (first operands))
              (pc (vreg-or-scratch (second operands) +ppc-scratch1+))
              (ws (ppc-word-size))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         (ppc-emit-addi buf +ppc-scratch2+ 0 8)
         (ppc-emit-shift-left buf +ppc-r0+ pc +ppc-scratch2+)
         (ppc-emit-ori buf +ppc-r0+ +ppc-r0+ #x31)                  ; string subtag
         (ppc-emit-store-word buf +ppc-r0+ +ppc-r19+ 0)
         (ppc-emit-addi buf +ppc-r0+ pc 1)
         (ppc-emit-addi buf +ppc-scratch2+ 0 (if (= ws 8) 3 2))
         (ppc-emit-shift-left buf +ppc-r0+ +ppc-r0+ +ppc-scratch2+)
         (ppc-emit-addi buf +ppc-r0+ +ppc-r0+ 15)
         (ppc-emit-andi-dot buf +ppc-r0+ +ppc-r0+ #xFFF0)
         (ppc-emit-ori buf pd +ppc-r19+ +tag-object+)
         (ppc-emit-add buf +ppc-r19+ +ppc-r19+ +ppc-r0+)
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-u8-ref+
       (let* ((vd (first operands))
              (parr (vreg-or-scratch (second operands) +ppc-scratch1+))
              (pidx (vreg-or-scratch (third operands) +ppc-scratch2+))
              (ws (ppc-word-size))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         ;; Address in r12 — r0 is not a base register (see :obj-ref).
         (ppc-emit-shift-right-arith-imm buf +ppc-scratch2+ pidx 1)
         (ppc-emit-add buf +ppc-scratch2+ +ppc-scratch2+ parr)
         (ppc-emit-lbz buf pd +ppc-scratch2+ (- ws +tag-object+))
         (ppc-emit-addi buf +ppc-r0+ 0 1)
         (ppc-emit-shift-left buf pd pd +ppc-r0+)                   ; tag
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-u8-set+
       ;; (u8-set Varr Vidx Vval) — Vidx and Vval both TAGGED.
       (let* ((parr (vreg-or-scratch (first operands) +ppc-scratch1+))
              (pidx (vreg-or-scratch (second operands) +ppc-scratch2+))
              (ws (ppc-word-size)))
         ;; Address in r12 — r0 is not a base register (see :obj-ref).
         (ppc-emit-shift-right-arith-imm buf +ppc-scratch2+ pidx 1)
         (ppc-emit-add buf +ppc-scratch2+ +ppc-scratch2+ parr)
         ;; Value loaded LAST, into r11, and untagged in r0 (a fine VALUE
         ;; register — only the BASE position treats r0 as zero).
         (let ((pv (or (ppc-vreg-phys (third operands))
                       (progn (ppc-load-vreg buf +ppc-scratch1+ (third operands))
                              +ppc-scratch1+))))
           (ppc-emit-shift-right-arith-imm buf +ppc-r0+ pv 1)
           (ppc-emit-stb buf +ppc-r0+ +ppc-scratch2+ (- ws +tag-object+)))))

      ;;; --- System area pointers ---
      ;; One-slot object, subtag #x16: header (1<<8)|#x16 then the raw address.
      (#.+op-sap-new+
       (let* ((vd (first operands))
              (pa (vreg-or-scratch (second operands) +ppc-scratch1+))
              (ws (ppc-word-size))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         (ppc-emit-li buf +ppc-r0+ #x116)
         (ppc-emit-store-word buf +ppc-r0+ +ppc-r19+ 0)
         (ppc-emit-store-word buf pa +ppc-r19+ ws)
         (ppc-emit-ori buf pd +ppc-r19+ +tag-object+)
         (ppc-emit-addi buf +ppc-r19+ +ppc-r19+ 16)
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      (#.+op-sap-addr+
       ;; Raw address out, TAGGED as a fixnum (as on x64/i386).
       (let* ((vd (first operands))
              (ps (vreg-or-scratch (second operands) +ppc-scratch1+))
              (ws (ppc-word-size))
              (pd (or (ppc-vreg-phys vd) +ppc-scratch1+)))
         (ppc-emit-addi buf +ppc-scratch2+ ps
                        (logand (- +tag-object+) #xFFFF))
         (ppc-emit-load-word buf pd +ppc-scratch2+ ws)
         (ppc-emit-addi buf +ppc-r0+ 0 1)
         (ppc-emit-shift-left buf pd pd +ppc-r0+)
         (unless (ppc-vreg-phys vd)
           (ppc-store-vreg buf vd pd))))

      ;;; --- Calling-convention slots ---
      ;; These fell into the OTHERWISE trap below.  :set-nargs precedes every
      ;; call, so the first function call in any image executed `tw 31,0,0`.
      ;; nargs is stored RAW; :get-nargs tags it (<<1) on the way out.
      (#.+op-set-nargs+
       (let ((n (logand (first operands) #xFF)))
         (ppc-emit-li buf +ppc-scratch1+ n)
         (ppc-emit-store-abs buf +ppc-scratch1+ (ppc-nargs-addr))))

      (#.+op-get-nargs+
       (let ((vd (first operands)))
         (ppc-emit-load-abs buf +ppc-scratch1+ (ppc-nargs-addr))
         (ppc-emit-addi buf +ppc-scratch2+ 0 1)
         (ppc-emit-shift-left buf +ppc-scratch1+ +ppc-scratch1+ +ppc-scratch2+)
         (ppc-store-vreg buf vd +ppc-scratch1+)))

      (#.+op-set-cenv+
       (let ((ps (vreg-or-scratch (first operands) +ppc-scratch1+)))
         (ppc-emit-store-abs buf ps (ppc-cenv-addr))))

      (#.+op-get-cenv+
       (let ((vd (first operands)))
         (ppc-emit-load-abs buf +ppc-scratch1+ (ppc-cenv-addr))
         (ppc-store-vreg buf vd +ppc-scratch1+)))

      (#.+op-set-mv-count+
       ;; TAGGED, and at the SHARED contract address (#x10000090) that
       ;; compiler-emitted mem-refs and shared CL source also read -- see the
       ;; long note in translate-i386.lisp about what happens when a target
       ;; relocates this slot and leaves the one writer with zero readers.
       (let ((tagged (ash (first operands) 1)))
         (ppc-emit-li buf +ppc-scratch1+ tagged)
         (ppc-emit-store-abs buf +ppc-scratch1+ (ppc-mvcount-addr))))

      (otherwise
       ;; Unknown opcode: emit a trap
       (ppc-emit-tw buf 31 0 0)))))

;;; ============================================================
;;; Main Translation Entry Point
;;; ============================================================

(defun translate-mvm-to-ppc (bytecode function-table &key (64-bit t))
  "Translate MVM bytecode to PPC native code.
   When 64-BIT is T (default), emit PPC64 instructions.
   When NIL, emit PPC32 instructions.
   BYTECODE is a (vector (unsigned-byte 8)).
   FUNCTION-TABLE maps function indices to bytecode offsets.
   Returns a PPC buffer (convert with ppc-buffer-to-bytes)."
  (setf *ppc-li-const-patches* nil)
  (let* ((*ppc-64-bit* 64-bit)
         (buf (make-ppc-buffer))
         (label-map (make-hash-table :test 'eql))
         (bc bytecode)
         (len (length bc))
         (pc 0))

    ;; First pass: scan for branch targets and create labels
    (loop while (< pc len)
          do (let* ((decoded (decode-instruction bc pc))
                    (opcode (car decoded))
                    (operands (cadr decoded))
                    (new-pc (cddr decoded)))
               (let ((info (gethash opcode *opcode-table*)))
                 (when info
                   (let ((op-types (opcode-info-operands info)))
                     ;; Check if this instruction has a branch offset
                     (cond
                       ;; Branches with offset only (br, beq, bne, etc.)
                       ((and (member :off32 op-types)
                             (not (member :reg op-types)))
                        (let ((off (first operands)))
                          (let ((target (+ new-pc off)))
                            (unless (gethash target label-map)
                              (setf (gethash target label-map)
                                    (mvm-make-label))))))
                       ;; Branches with reg + offset (bnull, bnnull)
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
    (ppc-emit-prologue buf)

    ;; Second pass: translate instructions
    ;; Second pass: translate instructions
    (setf pc 0)
    (loop while (< pc len)
          do (progn
               ;; Emit label at current PC before translating
               (let ((label (gethash pc label-map)))
                 (when label
                   (ppc-emit-label buf label)))
               (let* ((decoded (decode-instruction bc pc))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pc (cddr decoded)))
                 ;; Compute branch target PCs relative to end of instruction
                 (ppc-translate-insn buf opcode operands new-pc label-map function-table)
                 (setf pc new-pc))))

    ;; Resolve label fixups
    (ppc-fixup-labels buf)

    ;; Report where each function actually LANDED.  Without this second
    ;; value cross.lisp estimates a function's native offset proportionally
    ;; from its bytecode offset — right only when there is one function, and
    ;; mid-prologue otherwise.  Each function entry already has a label from
    ;; the first pass; ppc-emit-label recorded its byte position.
    (let ((fn-map (make-hash-table :test 'eql)))
      (when function-table
        (maphash (lambda (idx mvm-offset)
                   (declare (ignore idx))
                   (let* ((label (gethash mvm-offset label-map))
                          (pos (and label (gethash label (ppc-buffer-labels buf)))))
                     (when pos
                       (setf (gethash mvm-offset fn-map) pos))))
                 function-table))
      (values (ppc-buffer-to-bytes buf) fn-map))))

;;; ============================================================
;;; Installer
;;; ============================================================

(defun ppc-disassemble-native (buf &key (start 0) (end nil))
  "Print a hex dump of PPC64 native code for debugging.
   Each line shows one 32-bit instruction word (big-endian)."
  (let* ((words (ppc-buffer-words buf))
         (limit (or end (ppc-buffer-word-count buf))))
    (loop for i from start below limit
          do (format t "  ~4,'0X: ~8,'0X~%" (* i 4) (aref words i)))))

(defun ppc-set-linux-mode (on)
  "Turn hosted mode on or off, moving the convention slots with it.  One
   function so the two cannot drift apart: an unmapped slot base is a SIGSEGV on
   the first :set-nargs, which is emitted before EVERY call, so the first
   function call in the image dies rather than something subtle later.

   The bare base depends on which PPC target is installed, so this must be
   called AFTER install-ppc-translator / install-ppc32-translator; turning the
   mode OFF restores from *PPC-64-BIT*, which those installers have set."
  (setf *ppc-linux-mode* (and on t))
  (setf *ppc-globals-base*
        (cond (on *ppc-hosted-globals-base*)
              (*ppc-64-bit* #x20900000)
              (t            #x00900000))))

(defun install-ppc-translator ()
  "Install the PPC64 translator into the target descriptor."
  ;; ppc64 loads at 0x20000000, so its convention slots sit just above.
  (setf *ppc-globals-base* #x20900000)
  (let ((target *target-ppc64*))
    (setf (target-translate-fn target)
          (lambda (bytecode function-table)
            (translate-mvm-to-ppc bytecode function-table :64-bit t)))
    (setf (target-emit-prologue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*ppc-64-bit* t)) (ppc-emit-prologue buf))))
    (setf (target-emit-epilogue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*ppc-64-bit* t)) (ppc-emit-epilogue buf))))
    target))

(defun install-ppc32-translator ()
  "Install the PPC32 translator into the target descriptor."
  ;; ppc32 loads at 0; cons space starts at 16MB, so 9MB is clear RAM.
  ;; mv-count must stay inside the 64MB the boot TLBs map -- see its docstring.
  (setf *ppc-globals-base* #x00900000)
  (let ((target *target-ppc32*))
    (setf (target-translate-fn target)
          (lambda (bytecode function-table)
            (translate-mvm-to-ppc bytecode function-table :64-bit nil)))
    (setf (target-emit-prologue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*ppc-64-bit* nil)) (ppc-emit-prologue buf))))
    (setf (target-emit-epilogue target)
          (lambda (target buf)
            (declare (ignore target))
            (let ((*ppc-64-bit* nil)) (ppc-emit-epilogue buf))))
    target))
