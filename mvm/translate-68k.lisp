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

;;; MODUS_68K_PROFILE: a longword counter per function at #x10010000 + 4i (i =
;;; the function's place in translation order), bumped at its entry and at every
;;; loop head (a backward branch's target), so a function that loops inside one
;;; call still scores.  A profiling build only: ADDQ clobbers the flags.
(defvar *68k-profile* nil)
(defconstant +68k-profile-base+ #x10010000)

;;; THREADED COLD CODE's specials (the tier itself is near the end of this
;;; file); declared here, before any function that binds them.
(defvar *68k-threaded* nil
  "MODUS_68K_THREADED: NIL, :ALL (every function that can be), or a hash of
   bytecode offsets to keep NATIVE (the hot ones).")
(defvar *68k-hot-names* nil
  "MODUS_68K_HOT: an EQUAL hash of function names to keep native when
   threading (cross.lisp turns it into bytecode offsets for *68K-THREADED*).")
(defvar *68k-cold-fns* nil "Hash: cold function start pc -> (end-pc . branch-width).")
(defvar *68k-thread-shapes* nil "Hash: shape key -> shape (vector index label count key).")
(defvar *68k-thread-next-label* nil)
(defvar *68k-stream-fixups* nil "List of (byte-pos label size base).")

(defvar *68k-li-const-patches* nil
  "List of (NATIVE-BYTE-OFFSET . POOL-INDEX) recorded by +OP-LI-CONST+: at that
   offset is a MOVE.L #imm32,D0 whose 32-bit immediate (bytes +2..+5, big-endian)
   cross.lisp's apply-li-const-patches fills with the constant-pool slot's
   tagged address.  Reset at the start of every translation.")

;;; THE 68000 (MODUS_68K_CPU=68000): a Mac Classic / Plus / SE.  No long
;;; branches, no 32-bit PC-relative LEA, no 32-bit multiply or divide, no
;;; EXTB.L, no scaled index, and a word or long access at an ODD address is a
;;; bus error.  So, in this mode: a long BRA / BSR is JMP / JSR to an ABSOLUTE
;;; address and a long Bcc is B!cc.S over one (the native code's runtime
;;; address is known only after layout: the translator records each such word
;;; in *68K-ABS-PATCHES* and cross.lisp's apply-68k-abs-patches fills it in);
;;; the PC-relative LEA is LEA abs.L; MULS.L / MULU.L / DIVS.L are calls to
;;; shared subroutines (m68k-emit-68000-helpers); EXTB.L is EXT.W + EXT.L; the
;;; threaded dispatcher scales its index with adds; and every token-stream
;;; operand -- at any alignment -- is read a byte at a time.  A boot stub has
;;; no runtime address to patch, so ITS long branches are 16-bit (the fixup
;;; range check fails loudly if one does not reach).  QEMU's -cpu m68000
;;; rejects the 68020 instructions; it does NOT model the odd-address fault.
(defvar *68k-cpu-68000* (equal (sb-ext:posix-getenv "MODUS_68K_CPU") "68000")
  "T: emit 68000 code only (see above).")
(defvar *68k-abs-patches* nil
  "List of (NATIVE-BYTE-OFFSET . TARGET-NATIVE-OFFSET): a 32-bit absolute
   address of native code, written by cross.lisp after layout.  Reset at the
   start of every translation.")
(defvar *68k-native-p* nil
  "T while translating the image's native code (absolute patches possible).")
;;; PAGED THREADED CODE (MODUS_68K_PAGED=1, with MODUS_68K_THREADED): every cold
;;; function's token stream moves to one STREAM SECTION at the end of the native
;;; code and can run from ANY address, so a small machine can keep it on disk
;;; and load it on demand (a code cache, Mac-segment fashion).  A cold function
;;; is a resident stub -- MOVEA.L A5,A1 / MOVE.W #i,D0 / BRA cold-enter --
;;; and COLD-ENTER loads A5 from entry i of the STREAM TABLE (resident, one
;;; longword a function: the stream's current address, 0 when not loaded).
;;; Position independence: a stream's own branches were already relative; its
;;; call / tail-call / function-address operands (:REL) become ABSOLUTE
;;; addresses of the target's entry (resident), patched after layout like the
;;; 68000's (cross.lisp apply-68k-abs-patches); constant-pool operands (:LIC)
;;; were already absolute.  MODUS_68K_PAGED_SHIFT=N is the test: after
;;; everything is resolved the stream section is MOVED N bytes and only the
;;; table follows it -- anything still position-dependent breaks.
(defvar *68k-paged* (let ((v (sb-ext:posix-getenv "MODUS_68K_PAGED"))) (and v (string= v "1")))
  "T: paged threaded code (see above).")
(defvar *68k-paged-control* (equal (sb-ext:posix-getenv "MODUS_68K_PAGED_CONTROL") "1")
  "The position test's NEGATIVE CONTROL: :REL stays relative, as unpaged.  A
   paged image built so and moved by MODUS_68K_PAGED_SHIFT must break.")
(defvar *68k-paged-fns* nil "Paged: (INDEX START-PC STREAM-LABEL), newest first.")
(defvar *68k-cold-enter-label* nil)
(defvar *68k-stream-table-label* nil)
(defvar *68k-stream-section-label* nil)
;;; THE MAC'S STREAM LOADER (paged + Mac mode).  The disk builder
;;; (scripts/mac68k-disk.py --paged) gives the boot blocks only the RESIDENT
;;; part of the image and marks every stream-table entry "not loaded" -- bit 0
;;; set, its home address otherwise (streams are word-aligned).  COLD-ENTER
;;; takes such an entry to the loader, which reads the blocks holding the
;;; stream from the boot drive into a staging buffer (the ROM's _Read, the
;;; boot blocks' way), copies the stream into the CODE CACHE and makes the
;;; entry its address.  Its length is the gap to the next stream, from the
;;; HOME TABLE (read-only, one longword more than the table: the section's
;;; end).  The disk holds the image at byte 1024, so a home address H is disk
;;; byte 1024 + H - load address.  No eviction yet: a full cache stops.
(defvar *68k-paged-cache* nil
  "Mac paged: (CACHE-LO CACHE-HI STAGING STAGING-SIZE LOAD-ADDR), set by
   boot-mac68k's MAC68K-CONFIGURE from the machine's layout.")
(defconstant +68k-cache-ptr+ #x10006034
  "Paged Mac: the code cache's next free byte (raw), in the low block so the
   boot stub sets it for the machine it finds itself on.")
(defconstant +68k-cache-hi+ #x10006038 "Paged Mac: the code cache's end (raw).")
(defconstant +68k-cache-lo+ #x1000603C
  "Paged Mac: where the code cache's streams start (raw), past its owner map.")
(defconstant +68k-cache-map+ #x10006040
  "Paged Mac: the cache's OWNER MAP (raw address; the cache's own first bytes):
   one 16-bit word per 64-byte granule from here to the cache's end -- the
   stream index + 1 that was last placed over it, 0 for none.  Streams are
   placed on 64-byte boundaries, so a granule has at most one stream.")
(defconstant +68k-cache-granule-shift+ 6)
(defvar *68k-stream-home-label* nil)
(defvar *68k-stream-end-label* nil)

(defvar *68k-paged-layout* nil
  "After a paged translation: plist :table :section :end :count -- native
   offsets of the stream table, the section and its end (cross.lisp, the Mac
   disk builder).")

(defvar *68k-mul32-label* nil)
(defvar *68k-mulu64-label* nil)
(defvar *68k-divs32-label* nil)

(defun m68k-emit-abs32 (buf label-id &optional (ref-pos (+ (m68k-buffer-position buf) 2)) ord)
  "Two placeholder words for the absolute address of LABEL-ID (native code
   only).  REF-POS / ORD feed branch relaxation the way a :disp32 does."
  (unless *68k-native-p*
    (error "68k: an absolute code address outside the native code (boot stub?)"))
  (m68k-emit-word buf 0)
  (push (list (1- (m68k-buffer-word-count buf)) label-id :abs32 ref-pos ord)
        (m68k-buffer-fixups buf))
  (m68k-emit-word buf 0))

(defun m68k-emit-word (buf word)
  "Emit a 16-bit instruction word."
  (when (oddp (m68k-buffer-position buf))
    (error "68k: a word at the odd position ~D" (m68k-buffer-position buf)))
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
      (destructuring-bind (word-idx label-id fixup-type ref-pos &optional ord) fixup
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
            (:abs32
             ;; 68000: an absolute native address, filled in after layout
             ;; (cross.lisp apply-68k-abs-patches).  Distances still feed
             ;; branch relaxation, measured as a :disp32's would be.
             (let ((disp (- target (- ref-pos 2))))
               (when (and ord *68k-branch-distances*)
                 (setf (gethash ord *68k-branch-distances*) (cons disp ref-pos)))
               (push (cons (* 2 word-idx) target) *68k-abs-patches*)))
            (:disp32
             ;; 32-bit displacement (two words) — full 32-bit signed range,
             ;; no realistic overflow but the assert keeps the invariant.
             (let ((disp (- target (- ref-pos 2))))
               (when (and ord *68k-branch-distances*)
                 (setf (gethash ord *68k-branch-distances*) (cons disp ref-pos)))
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

(defun m68k-emit-move-byte-disp-dn (buf an disp dn)
  "MOVE.B d16(An), Dd"
  (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn +ea-mode-an-disp+ an))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-granule ()
  "The heap granule: 16 bytes, or 8 with *68K-TAGS3* (target.lisp)."
  (if *68k-tags3* 8 16))
(defun m68k-tag-mask ()
  "The pointer-tag bits: four, or three with *68K-TAGS3*."
  (if *68k-tags3* 7 15))
(defun m68k-kind-shift ()
  "log2 of the granule: one kind byte per granule."
  (if *68k-tags3* 3 4))
(defun m68k-round (n)
  "N bytes rounded up to the granule."
  (let ((g (m68k-granule))) (logand (+ n g -1) (- g))))

(defun m68k-emit-check-cons (buf dn)
  "Trap (ILLEGAL) unless the low four bits of DN are +TAG-CONS+.  Clobbers D1.
   MOVEQ / AND.L / SUBQ.L (6 bytes) for the ANDI.L / CMPI.L (12) it replaces,
   and the skip is a fixed BEQ.S over the 2-byte ILLEGAL."
  (m68k-emit-moveq buf +68k-d1+ (m68k-tag-mask))
  (m68k-emit-and-dn-dn buf dn +68k-d1+)
  (m68k-emit-subq buf +68k-d1+ +tag-cons+)
  (m68k-emit-word buf #x6702)                    ; BEQ.S *+4: over the ILLEGAL
  (m68k-emit-illegal buf))

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

(defun m68k-emit-68000-binop (buf label src dst)
  "68000: Ddst = HELPER(Ddst, Dsrc) through a shared subroutine taking D0 and
   D1 and answering in D0; every other register, and D0/D1 when neither is
   DST, keeps its value.  The operands go through the stack, so SRC and DST
   may be any data registers, D0 and D1 included."
  (m68k-emit-word buf #x48E7) (m68k-emit-word buf #xC000)  ; MOVEM.L D0-D1,-(SP)
  (m68k-emit-word buf (logior #x2F00 (logand src 7)))      ; MOVE.L Dsrc,-(SP)
  (m68k-emit-word buf (logior #x2F00 (logand dst 7)))      ; MOVE.L Ddst,-(SP)
  (m68k-emit-word buf #x201F)                              ; MOVE.L (SP)+,D0
  (m68k-emit-word buf #x221F)                              ; MOVE.L (SP)+,D1
  (m68k-emit-bsr buf label)
  (case dst
    (0 (m68k-emit-word buf #x2E80))                        ; MOVE.L D0,(SP)
    (1 (m68k-emit-word buf #x2F40) (m68k-emit-word buf 4)) ; MOVE.L D0,4(SP)
    (t (m68k-emit-move-dn-dn buf +68k-d0+ dst)))
  (m68k-emit-word buf #x4CDF) (m68k-emit-word buf #x0003)) ; MOVEM.L (SP)+,D0-D1

(defparameter +68k-68000-mul32+
  '(#x48E7 #x3000        ; MOVEM.L D2-D3,-(SP)
               #x2400 #x4842 #xC4C1 ; D2 = ah*bl
               #x2601 #x4843 #xC6C0 ; D3 = bh*al
               #xD443 #x4842 #x4242 ; D2 = (D2+D3).w << 16
               #xC0C1 #xD082        ; D0 = al*bl + D2
               #x4CDF #x000C #x4E75)
  "MUL32: D0 = low 32 bits of D0 * D1 (sign does not matter for the low half)
   = al*bl + ((ah*bl + al*bh) << 16).")

(defparameter +68k-68000-mulu64+
  '(#x48E7 #x3C00                ; MOVEM.L D2-D5,-(SP)
               #x2400 #xC4C1                ; D2 = p0 = al*bl
               #x2600 #x4843 #xC6C1         ; D3 = p1 = ah*bl
               #x2801 #x4844 #x2A04 #xC8C0  ; D4 = p2 = bh*al, D5.w = bh
               #x2200 #x4841 #xC2C5         ; D1 = p3 = ah*bh
               #xD684 #x6406 #x0681 #x0001 #x0000 ; D3 = p1+p2, carry -> D1 += 2^16
               #x2803 #x4844 #x4244         ; D4 = (mid & #xFFFF) << 16
               #x4243 #x4843                ; D3 = mid >> 16
               #x7A00 #xD484 #xD385 #xD283  ; D2 += D4; D1 += X + D3
               #x2002                       ; D0 = D2
               #x4CDF #x003C #x4E75)
  "MULU64: D1:D0 = D0 * D1, unsigned (D0 low, D1 high).")

(defparameter +68k-68000-divs32+
  '(#x48E7 #x3800                ; MOVEM.L D2-D4,-(SP)
               #x7800                       ; D4 = 0: the sign
               #x4A80 #x6A04 #x4480 #x4604  ; |D0|, flip D4
               #x4A81 #x6A04 #x4481 #x4604  ; |D1|, flip D4
               #x7400 #x761F                ; D2 = 0 (remainder), D3 = 31
               #xD080 #xD582 #xB481 #x6504 #x9481 #x5280 #x51CB #xFFF2
               #x4A04 #x6702 #x4480         ; negate when the signs differed
               #x4CDF #x001C #x4E75)
  "DIVS32: D0 = D0 / D1, signed, truncated (DIVS.L's quotient); restoring
   division on the magnitudes, 32 steps.")

(defun m68k-emit-68000-helpers (buf)
  "The 68000's MULS.L / MULU.L (64) / DIVS.L, as subroutines on D0 and D1."
  (loop for (label words) in (list (list *68k-mul32-label* +68k-68000-mul32+)
                                   (list *68k-mulu64-label* +68k-68000-mulu64+)
                                   (list *68k-divs32-label* +68k-68000-divs32+))
        do (m68k-emit-label buf label)
           (dolist (w words) (m68k-emit-word buf w))))

(defun m68k-68000-local-helper (buf words)
  "Outside the native code (a boot stub): a private copy of a helper, jumped
   over; its label."
  (let ((over (mvm-make-label)) (here (mvm-make-label)))
    (m68k-emit-bra buf over)
    (m68k-emit-label buf here)
    (dolist (w words) (m68k-emit-word buf w))
    (m68k-emit-label buf over)
    here))

(defun m68k-emit-muls (buf src dst)
  "MULS.L Ds, Dd - signed multiply (32x32->32 low)"
  (when *68k-cpu-68000*
    (return-from m68k-emit-muls
      (m68k-emit-68000-binop buf (or *68k-mul32-label*
                                     (m68k-68000-local-helper buf +68k-68000-mul32+))
                             src dst)))
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
  (when *68k-cpu-68000*
    ;; the one caller (:mul26hi) multiplies D0 by D1 into D1:D0, the helper's shape
    (unless (and (= src +68k-d1+) (= dlo +68k-d0+) (= dhi +68k-d1+))
      (error "68k 68000: MULU.L ~D -> ~D:~D is not D1 -> D1:D0" src dhi dlo))
    (return-from m68k-emit-mulu-64
      (m68k-emit-bsr buf (or *68k-mulu64-label*
                             (m68k-68000-local-helper buf +68k-68000-mulu64+)))))
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
  (when *68k-cpu-68000*
    (return-from m68k-emit-divs
      (m68k-emit-68000-binop buf (or *68k-divs32-label*
                                     (m68k-68000-local-helper buf +68k-68000-divs32+))
                             src dst)))
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

(defparameter *68k-mac-mode* nil
  "T when building a BARE 68k MACINTOSH image (boot/boot-mac68k.lisp): booted
   from the disk's boot blocks by the Mac ROM, no OS, no MMU.  It keeps the
   HOSTED memory map -- the low block at #x10000000, the heap above it, the
   native collector -- in real RAM (a Mac with at least 384 MB, as Basilisk II
   gives), and swaps the Linux service traps for the Mac's: the console is the
   ROM's serial driver, the clock is Ticks, exit is a halt.  ARM32's
   *ARM32-CL-BARE* shape.  M68K-SET-MAC-MODE.")

(defun m68k-hosted-layout-p ()
  "The hosted memory map: Linux/m68k, or a bare Mac that keeps it."
  (or *68k-linux-mode* *68k-mac-mode*))

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
(defun m68k-mvcount-addr () (conv-real modus.mvm::+mv-count-addr+))

;;; A5 = THE HOSTED GLOBALS BASE (*68K-A5-GLOBALS*, MODUS_68K_A5_GLOBALS;
;;; unset, it follows MODUS_SMALL_IMAGE).  A5 is otherwise unused outside the
;;; collector, which saves and restores it.  With it on, the boot stub loads A5
;;; once and the calling-convention slots (nargs, cenv, MV count) are reached as
;;; (A5) / d16(A5): nargs before every call is MOVEQ + MOVE.L D0,(A5), 4 bytes
;;; where it was 8.  A raw low-block address in A5 is not a heap pointer, so the
;;; conservative register scan ignores it.
(defvar *68k-a5-globals* nil)

(defun m68k-a5-disp (addr)
  "ADDR's displacement from A5 when A5 holds the globals base and it fits d16."
  (and *68k-a5-globals* (m68k-hosted-layout-p)
       (let ((d (- addr (conv-real *68k-hosted-globals-base*))))
         (and (<= -32768 d 32767) d))))

(defun m68k-emit-store-absl (buf dn addr)
  "MOVE.L Dn, (ADDR).L -- one instruction, 6 bytes, no address register
   (2 or 4 bytes through A5, see *68K-A5-GLOBALS*)."
  (let ((d (m68k-a5-disp addr)))
    (cond ((eql d 0) (m68k-emit-move-dn-an-ind buf dn +68k-a5+))
          (d (m68k-emit-move-dn-disp buf dn +68k-a5+ d))
          (t (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-special+ 1 +ea-mode-dn+ dn))
             (m68k-emit-long buf addr)))))

(defvar *68k-li-shift* nil
  "Small images: an immediate that is a signed byte shifted left 1..8 (a
   tagged fixnum up to 127, say) is MOVEQ + LSL.L, 4 bytes, not MOVE.L #, 6.")

(defun m68k-imm-shifted-byte (s)
  "(values BYTE COUNT) when S = BYTE << COUNT, BYTE a signed byte, COUNT 1..8."
  (loop for k from 1 to 8
        do (when (and (zerop (logand s (1- (ash 1 k))))
                      (<= -128 (ash s (- k)) 127))
             (return (values (ash s (- k)) k)))))

(defun m68k-emit-imm-dn (buf imm32 dn)
  "Dn = IMM32: MOVEQ (2 bytes) when it sign-extends from a byte, MOVEQ + LSL
   (4) for a shifted byte under *68K-LI-SHIFT*, else MOVE.L (6)."
  (let ((s (if (>= imm32 #x80000000) (- imm32 #x100000000) imm32)))
    (if (<= -128 s 127)
        (m68k-emit-moveq buf dn (logand s #xFF))
        (multiple-value-bind (b k) (and *68k-li-shift* (m68k-imm-shifted-byte s))
          (if b
              (progn (m68k-emit-moveq buf dn (logand b #xFF))
                     (m68k-emit-lsl-imm buf dn k))
              (m68k-emit-move-imm-dn buf (logand imm32 #xFFFFFFFF) dn))))))

(defun m68k-emit-movea-word-imm (buf imm16 an)
  "MOVEA.W #imm16, An -- sign-extended to 32 bits, 4 bytes"
  (m68k-emit-word buf (m68k-encode-move-word 3 +ea-mode-an+ an +ea-mode-special+ 4))
  (m68k-emit-word buf (logand imm16 #xFFFF)))

(defun m68k-emit-move-imm-disp (buf imm32 an disp)
  "MOVE.L #imm32, d16(An) -- 8 bytes (source extension first, then d16)"
  (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-an-disp+ an +ea-mode-special+ 4))
  (m68k-emit-long buf (logand imm32 #xFFFFFFFF))
  (m68k-emit-word buf (logand disp #xFFFF)))

(defun m68k-emit-store-abs (buf dn addr)
  "Store data register DN to absolute ADDR (address parked in A1, or through
   A5, see *68K-A5-GLOBALS*)."
  (if (m68k-a5-disp addr)
      (m68k-emit-store-absl buf dn addr)
      (progn (m68k-emit-move-imm-an buf addr +68k-a1+)
             (m68k-emit-move-dn-an-ind buf dn +68k-a1+))))

(defun m68k-emit-load-abs (buf dn addr)
  "Load from absolute ADDR into data register DN (address parked in A1, or
   through A5, see *68K-A5-GLOBALS*)."
  (let ((d (m68k-a5-disp addr)))
    (cond ((eql d 0) (m68k-emit-move-an-ind-dn buf +68k-a5+ dn))
          (d (m68k-emit-move-disp-dn buf +68k-a5+ d dn))
          (t (m68k-emit-move-imm-an buf addr +68k-a1+)
             (m68k-emit-move-an-ind-dn buf +68k-a1+ dn)))))

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

(defun m68k-emit-asl-imm (buf dn count)
  "ASL.L #count, Dn -- V set when the sign changes during the shift"
  (let ((cnt (if (= count 8) 0 count)))
    (m68k-emit-word buf (logior (ash #b1110 12)
                                (ash (logand cnt 7) 9)
                                (ash 1 8)      ; direction=left
                                (ash +68k-size-long+ 6)
                                (ash 0 5)      ; i/r=0
                                (ash #b00 3)   ; type=AS
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

;;; BRANCH RELAXATION (*68K-RELAX-BRANCHES*).  A branch is long by default: the
;;; CL image is megabytes of native code and a 16-bit displacement reaches 32 KB.
;;; With relaxation TRANSLATE-MVM-TO-68K translates twice.  Pass 1 emits every
;;; branch long and records, by its ordinal, where it went; pass 2 emits each
;;; one as short as that distance allows: Bcc.S (2 bytes), Bcc.W (4), Bcc.L (6).
;;; Pass 2 only ever shrinks code, so a distance can only shrink -- except
;;; that a function entry's 16-byte alignment pad can grow by up to 15 when the
;;; code before it shrinks, so every entry a branch crosses costs it 16 bytes of
;;; margin.  The branches are emitted in the same order both times, which is
;;; what makes the ordinal a name.

(defvar *68k-relax-branches* nil
  "When T, 68k branches are relaxed to their shortest form (two translations).")
(defvar *68k-branch-plan* nil
  "Pass 2: vector, branch ordinal -> :BYTE / :WORD / :LONG.  NIL: all long.")
(defvar *68k-branch-ordinal* 0)
(defvar *68k-branch-distances* nil
  "Pass 1: hash, branch ordinal -> (DISP . REF-POS) of its long form.")
(defvar *68k-fn-entry-positions* nil
  "Pass 1: native positions of the function entries, for the margin.")

(defun m68k-emit-branch (buf base label-id)
  "BRA (BASE #x6000), BSR (#x6100) or Bcc (#x6000 | cc<<8) to LABEL-ID, in the
   size the relaxation plan gives this branch's ordinal (long without one)."
  (let* ((ord (incf *68k-branch-ordinal*))
         (size (if (and *68k-branch-plan* label-id (< ord (length *68k-branch-plan*)))
                   (aref *68k-branch-plan* ord)
                   :long))
         (pos (m68k-buffer-position buf)))
    ;; 68000: a boot stub's long branch is 16-bit; native code's is absolute.
    (when (and *68k-cpu-68000* (eq size :long) (not *68k-native-p*))
      (setq size :word))
    (when (and *68k-cpu-68000* (eq size :long))
      (unless label-id (error "68k 68000: a long branch with no label"))
      (case base
        (#x6000 (m68k-emit-word buf #x4EF9))             ; JMP abs.L
        (#x6100 (m68k-emit-word buf #x4EB9))             ; JSR abs.L
        (t (m68k-emit-word buf (logior (logxor base #x0100) #x06)) ; B!cc.S over:
           (m68k-emit-word buf #x4EF9)))                 ; JMP abs.L
      (m68k-emit-abs32 buf label-id (+ pos 4) ord)
      (return-from m68k-emit-branch nil))
    (ecase size
      (:byte
       (m68k-emit-word buf base)                ; the fixup fills the low byte
       (push (list (1- (m68k-buffer-word-count buf)) label-id :disp8-in-opcode (+ pos 2))
             (m68k-buffer-fixups buf)))
      (:word
       (m68k-emit-word buf base)
       (let ((ext (m68k-buffer-position buf)))
         (m68k-emit-word buf 0)
         (push (list (1- (m68k-buffer-word-count buf)) label-id :disp16 (+ ext 2))
               (m68k-buffer-fixups buf))))
      (:long
       (m68k-emit-word buf (logior base #xFF))
       (let ((disp-pos (m68k-buffer-position buf)))
         (m68k-emit-word buf 0)
         (when label-id
           (push (list (1- (m68k-buffer-word-count buf)) label-id :disp32 (+ disp-pos 2) ord)
                 (m68k-buffer-fixups buf)))
         (m68k-emit-word buf 0))))))

(defun m68k-relax-plan (n-branches)
  "The pass-2 size of each branch, from pass 1's distances and entry positions."
  (let ((plan (make-array (1+ n-branches) :initial-element :long))
        (entries (sort (coerce *68k-fn-entry-positions* 'vector) #'<)))
    (flet ((crossed (a b)                       ; entries strictly between
             (let ((lo (min a b)) (hi (max a b)) (n 0))
               (loop for e across entries when (and (> e lo) (< e hi)) do (incf n))
               n)))
      (maphash
       (lambda (ord v)
         (destructuring-bind (disp . ref-pos) v
           ;; DISP is from the extension word (opcode + 2) in both forms; the
           ;; long form's own 4 extension bytes are between it and a forward
           ;; target, and vanish in the shorter forms.
           (let* ((d (if (> disp 0) (- disp 4) disp))
                  (margin (* 16 (1+ (crossed (- ref-pos 4) (+ (- ref-pos 2) disp))))))
             (setf (aref plan ord)
                   (cond ((and (if (> disp 0) (>= d 2) t)
                               (<= (+ -128 margin) d (- 127 margin)))
                          :byte)
                         ((<= (+ -32768 margin) d (- 32767 margin)) :word)
                         (t :long))))))
       *68k-branch-distances*))
    plan))

(defun m68k-emit-bra (buf &optional label-id)
  "BRA to LABEL-ID (relaxed; long without a plan)."
  (m68k-emit-branch buf #x6000 label-id))

(defun m68k-emit-bcc-word (buf cc &optional label-id)
  "Bcc to LABEL-ID (relaxed; long without a plan -- the name is historical).
   CC is the 4-bit condition code."
  (m68k-emit-branch buf (logior #x6000 (ash (logand cc #xF) 8)) label-id))

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
  "BSR to LABEL-ID (relaxed; long without a plan)."
  (m68k-emit-branch buf #x6100 label-id))

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
              (m68k-emit-lea-label buf resume +68k-a0+)
              (m68k-emit-move-an-disp buf +68k-a0+ +68k-a1+ 8)
              (loop for d in (list +68k-d4+ +68k-d5+ +68k-d6+ +68k-d7+)
                    for i from 3
                    do (m68k-emit-move-dn-disp buf d +68k-a1+ (* 4 i)))
              (when *68k-threaded*                        ; the token pointer
                (m68k-emit-move-an-disp buf +68k-a5+ +68k-a1+ 28))
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
              (when *68k-threaded*
                (m68k-emit-move-disp-an buf +68k-a1+ 28 +68k-a5+))
              (m68k-emit-move-disp-an buf +68k-a1+ 4 +68k-a6+)
              (m68k-emit-move-disp-an buf +68k-a1+ 0 +68k-a7+)
              (m68k-emit-move-imm-dn buf +t-value+ +68k-d2+)  ; second return: T
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
           ((= code #x0532)
            ;; (%JIT-CALL addr): call native code at the TAGGED byte address
            ;; in V0; its result comes back in VR.
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d0+)
            (m68k-emit-asr-imm buf +68k-d0+ 1)
            (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
            (m68k-emit-jsr-an-ind buf +68k-a0+))
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
           ;; --- THE BARE MAC (*68K-MAC-MODE*): the ROM is the platform -------
           ((and (= code #x0300) *68k-mac-mode*)
            ;; serial write: the untagged byte to putc
            (m68k-emit-move-dn-dn buf +68k-d2+ +68k-d0+)
            (m68k-emit-asr-imm buf +68k-d0+ 1)
            (m68k-emit-mac-putc buf))
           ((and (= code #x0301) *68k-mac-mode*)
            ;; serial read: getc waits for a byte; TAGGED in V0, as hosted
            (m68k-emit-bsr-long buf *68k-mac-getc-label*)
            (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)
            (m68k-emit-lsl-imm buf +68k-d2+ 1))
           ((and (= code #x0310) *68k-mac-mode*)
            ;; the clock: Ticks ($16A), the ROM's 60 Hz count, TAGGED
            (m68k-emit-word buf #x2438) (m68k-emit-word buf #x016A)   ; MOVE.L $16A.W,D2
            (m68k-emit-lsl-imm buf +68k-d2+ 1))
           ((and (= code #x0311) *68k-mac-mode*)
            ;; its rate: 60 Hz, TAGGED
            (m68k-emit-move-imm-dn buf 120 +68k-d2+))
           ((and (or (= code #x0500) (= code #x0304)) *68k-mac-mode*)
            ;; exit / halt: nothing to return to -- spin with interrupts on, so
            ;; the ROM's serial and tick machinery keep running
            (m68k-emit-word buf #x60FE))                              ; BRA.S *
           ((and (= code #x0520) *68k-mac-mode*)
            ;; signal handlers: none on a bare Mac (a fault is the ROM's)
            nil)
           ((and *68k-mac-mode* (> code 15))
            ;; a Linux service trap: a gap here, recorded, and ILLEGAL if run
            (pushnew (+ #x10000 code) *68k-unimpl-opcodes*)
            (m68k-emit-word buf #x4AFC))
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
             ;; NIL is A4 (VN): one register move, 2 bytes (4 to a spill slot).
             ((and (= imm32 +nil-value+) pd (eq (m68k-reg-type pd) :data))
              (m68k-emit-move-an-dn buf +68k-a4+ (m68k-reg-number pd)))
             ((and (= imm32 +nil-value+) pd (eq (m68k-reg-type pd) :address))
              (m68k-emit-move-an-an buf +68k-a4+ (m68k-reg-number pd)))
             ((= imm32 +nil-value+)
              (m68k-emit-move-an-disp buf +68k-a4+ +68k-a6+ (m68k-spill-offset vd)))
             ;; Data register: MOVEQ, (small images) MOVEQ + LSL, else MOVE.L #
             ((and pd (eq (m68k-reg-type pd) :data))
              (m68k-emit-imm-dn buf imm32 (m68k-reg-number pd)))
             ;; Address register: MOVEA.W # sign-extends, 4 bytes for a word
             ((and pd (eq (m68k-reg-type pd) :address))
              (if (or (< imm32 #x8000) (>= imm32 #xFFFF8000))
                  (m68k-emit-movea-word-imm buf (logand imm32 #xFFFF) (m68k-reg-number pd))
                  (m68k-emit-move-imm-an buf imm32 (m68k-reg-number pd))))
             ;; Spilled: a byte by MOVEQ through D0 (6 bytes), else
             ;; MOVE.L #, d16(A6) (8 bytes; it was MOVE.L #,D0 + store, 10)
             ((or (< imm32 128) (>= imm32 #xFFFFFF80))
              (m68k-emit-moveq buf +68k-d0+ (logand imm32 #xFF))
              (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a6+ (m68k-spill-offset vd)))
             (t
              (m68k-emit-move-imm-disp buf imm32 +68k-a6+ (m68k-spill-offset vd)))))))

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
        (multiple-value-bind (skip-to target-pc) (m68k-range-guard-at mvm-pc vd vs amt)
         (if skip-to
          ;; EMIT-ARITH-PAIR's range guard (compiler.lisp
          ;; *ARITH-RANGE-BY-SHIFT*): ASL sets V exactly when a bit shifted
          ;; out differs from the sign, i.e. when (x << s) >> s /= x.  The
          ;; scratch gets x, which is its value on the fall-through.
          (progn
            (m68k-load-vreg buf +68k-d0+ vs)
            (m68k-store-vreg buf vd +68k-d0+)
            (m68k-emit-asl-imm buf +68k-d0+ amt)
            (m68k-emit-bcc-word buf +68k-cc-vs+ (ensure-label target-pc))
            (setq *68k-skip-to* skip-to))
          (progn
         (m68k-load-vreg buf +68k-d0+ vs)
         ;; 68k immediate shift max is 8; larger needs register shift
         (if (<= 1 amt 8)
             (m68k-emit-lsl-imm buf +68k-d0+ amt)
             (progn
               (m68k-emit-moveq buf +68k-d1+ amt)
               (m68k-emit-lsl-dn buf +68k-d1+ +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+))))))

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
         (m68k-emit-imm-dn buf (logand (%imm32-signed (second operands)) #xFFFFFFFF) +68k-d1+)
         (m68k-emit-cmp-dn-dn buf +68k-d1+ da)))

      (#.+op-test-imm+
       (progn
         (m68k-load-vreg buf +68k-d0+ (first operands))
         (m68k-emit-imm-dn buf (logand (%imm32-signed (second operands)) #xFFFFFFFF) +68k-d1+)
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
         ;; Type check (trap on a non-cons), then the car at the tag-folded
         ;; displacement: -tag(A0) is the cons's first word.
         (m68k-load-vreg buf +68k-d0+ vs)
         (if *68k-car-label*
             (m68k-emit-bsr buf *68k-car-label*)
             (progn
               (m68k-emit-check-cons buf +68k-d0+)
               (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
               (m68k-emit-move-disp-dn buf +68k-a0+ (- +tag-cons+) +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-cdr+
       (let ((vd (first operands))
             (vs (second operands)))
         ;; Type check, then the cdr at (4 - tag)(A0)
         (m68k-load-vreg buf +68k-d0+ vs)
         (if *68k-cdr-label*
             (m68k-emit-bsr buf *68k-cdr-label*)
             (progn
               (m68k-emit-check-cons buf +68k-d0+)
               (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
               (m68k-emit-move-disp-dn buf +68k-a0+ (- 4 +tag-cons+) +68k-d0+)))
         (m68k-store-vreg buf vd +68k-d0+)))

      (#.+op-cons+
       (let ((vd (first operands))
             (va (second operands))
             (vb (third operands)))
        (if *68k-cons-label*
         (progn                          ; the shared CONS (M68K-EMIT-CONS-HELPER)
           (m68k-load-vreg buf +68k-d0+ va)
           (m68k-load-vreg buf +68k-d1+ vb)
           (m68k-emit-bsr buf *68k-cons-label*)
           (m68k-store-vreg buf vd +68k-d0+))
         (progn
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
         (m68k-emit-lea-disp buf +68k-a2+ (m68k-granule) +68k-a2+)
         (m68k-store-vreg buf vd +68k-d0+)))))

      (#.+op-setcar+
       (let ((vd (first operands))
             (vs (second operands)))
         ;; Load cons pointer, strip tag, store new car
         (m68k-load-vreg buf +68k-d0+ vd)
         (m68k-emit-subi buf +68k-d0+ +tag-cons+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-load-vreg buf +68k-d1+ vs)
         (m68k-emit-move-dn-an-ind buf +68k-d1+ +68k-a0+)
         (when *68k-barrier-label* (m68k-emit-bsr buf *68k-barrier-label*))))

      (#.+op-setcdr+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vd)
         (m68k-emit-subi buf +68k-d0+ +tag-cons+)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-load-vreg buf +68k-d1+ vs)
         (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ 4)
         (when *68k-barrier-label* (m68k-emit-bsr buf *68k-barrier-label*))))

      (#.+op-consp+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d1+)
         (m68k-emit-andi buf +68k-d1+ (m68k-tag-mask))
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
         (m68k-emit-andi buf +68k-d1+ (m68k-tag-mask))
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
        (if *68k-alloc-obj-label*
         (progn                          ; the shared ALLOC-OBJ
           (m68k-emit-imm-dn buf (logior (ash size 8) subtag) +68k-d0+)
           (m68k-emit-imm-dn buf (m68k-round (* (1+ size) 4)) +68k-d1+)
           (m68k-emit-bsr buf *68k-alloc-obj-label*)
           (m68k-store-vreg buf vd +68k-d0+))
         (progn
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
           (let ((total (m68k-round (* (1+ size) 4)))
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
           (m68k-store-vreg buf vd +68k-d0+))))))

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
             ;; Normal object slot access.  The tagged pointer goes straight
             ;; to A0 and the tag folds into the displacement: (4(i+1)-tag)(A0)
             ;; is the same address SUBI #tag then 4(i+1)(A0) was, minus the
             ;; SUBI (6 bytes) and a MOVEA.
             (let ((disp (- (* (1+ idx) 4) +tag-object+)))
               (if (<= -32768 disp 32767)
                   (progn
                     (m68k-load-vreg-to-an buf +68k-a0+ vobj)
                     (m68k-emit-move-disp-dn buf +68k-a0+ disp +68k-d0+))
                   (progn
                     (m68k-load-vreg buf +68k-d0+ vobj)
                     (m68k-emit-subi buf +68k-d0+ +tag-object+)
                     (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
                     (m68k-emit-move-disp-dn buf +68k-a0+ (* (1+ idx) 4) +68k-d0+)))
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
             ;; Normal object slot store, the tag folded as in OBJ-REF.  A0
             ;; is translator scratch, never a vreg, so loading it first
             ;; cannot clobber VS.
             (let ((disp (- (* (1+ idx) 4) +tag-object+)))
               (if (<= -32768 disp 32767)
                   (progn
                     (m68k-load-vreg-to-an buf +68k-a0+ vobj)
                     (m68k-load-vreg buf +68k-d1+ vs)
                     (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ disp))
                   (progn
                     (m68k-load-vreg buf +68k-d0+ vobj)
                     (m68k-emit-subi buf +68k-d0+ +tag-object+)
                     (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
                     (m68k-load-vreg buf +68k-d1+ vs)
                     (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a0+ (* (1+ idx) 4))))
               (when *68k-barrier-label*
                 (m68k-emit-move-an-dn buf +68k-a0+ +68k-d0+)
                 (m68k-emit-bsr buf *68k-barrier-label*))))))

      (#.+op-obj-tag+
       (let ((vd (first operands))
             (vs (second operands)))
         (m68k-load-vreg buf +68k-d0+ vs)
         (m68k-emit-moveq buf +68k-d1+ (m68k-tag-mask))  ; AND in 4 bytes, not 6
         (m68k-emit-and-dn-dn buf +68k-d1+ +68k-d0+)
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
         (if *68k-subtag-label*
          (progn                         ; the shared OBJ-SUBTAG
            (m68k-load-vreg buf +68k-d0+ vs)
            (m68k-emit-bsr buf *68k-subtag-label*)
            (m68k-store-vreg buf vd +68k-d0+))
         (let ((zero (mvm-make-label)) (done (mvm-make-label)))
           (m68k-load-vreg buf +68k-d0+ vs)
           (m68k-emit-cmpi buf +68k-d0+ +t-value+)
           (m68k-emit-beq buf zero)
           (m68k-emit-moveq buf +68k-d1+ (m68k-tag-mask))
           (m68k-emit-and-dn-dn buf +68k-d0+ +68k-d1+)
           (m68k-emit-cmpi-byte buf +68k-d1+ +tag-object+)
           (m68k-emit-bne buf zero)
           (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
           ;; The subtag is the header's LOW byte, (count << 8) | subtag: the
           ;; header long is at -tag(A0), so its low byte, big-endian, is at
           ;; (3 - tag)(A0).
           (m68k-emit-moveq buf +68k-d0+ 0)
           (m68k-emit-move-byte-disp-dn buf +68k-a0+ (- 3 +tag-object+) +68k-d0+)
           ;; Tag as fixnum
           (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d0+)
           (m68k-emit-bra buf done)
           (m68k-emit-label buf zero)
           (m68k-emit-moveq buf +68k-d0+ 0)
           (m68k-emit-label buf done)
           (m68k-store-vreg buf vd +68k-d0+)))))

      ;;; --- Memory (raw) ---
      (#.+op-load+
       (let ((vd (first operands))
             (vaddr (second operands))
             (width (third operands)))
         ;; Load address into A0
         (m68k-load-vreg-to-an buf +68k-a0+ vaddr)
         (ecase (logand width 3)   ; widths 4..7 / 8+: the TLS / conv marks
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
         (ecase (logand width 3)   ; widths 4..7 / 8+: the TLS / conv marks
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
         (m68k-emit-addi buf +68k-d1+ (+ 3 (m68k-granule)))
         (m68k-emit-andi buf +68k-d1+ (logand (- (m68k-granule)) #xFFFFFFFF))
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
         (m68k-emit-subi buf +68k-d0+ (- +tag-object+ 4))
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
         (m68k-emit-subi buf +68k-d0+ (- +tag-object+ 4))
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
       ;; Double -> tagged fixnum, TRUNCATING: FMOVE.L under FPCR's
       ;; round-toward-zero mode, the caller's mode put back after.  NOT FINTRZ:
       ;; the 68040 does not implement it (Motorola's FPSP emulates it in
       ;; software -- Linux/m68k carries the FPSP, a bare Mac does not), and
       ;; Basilisk II's emulation of it rounds a negative value toward +1
       ;; (-3.5 -> -2), which r31-float-negative caught on the Mac ladder.
       (let ((vd (first operands)))
         (m68k-load-vreg buf +68k-d0+ (second operands))
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-float-unbox buf 0)
         (m68k-emit-word buf #xF201) (m68k-emit-word buf #xB000)   ; FMOVE.L FPCR,D1
         (m68k-emit-word buf #xF23C) (m68k-emit-word buf #x9000)   ; FMOVE.L #RZ,FPCR
         (m68k-emit-long buf #x00000010)
         (m68k-emit-fmove-l-to-dn buf 0 +68k-d0+)
         (m68k-emit-word buf #xF201) (m68k-emit-word buf #x9000)   ; FMOVE.L D1,FPCR
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
         (m68k-emit-lea-label buf label +68k-a1+)
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
         (m68k-emit-lea-disp buf +68k-a2+ (m68k-granule) +68k-a2+)
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
       ;; A plain GC-CHECK right before a CONS: the shared CONS checks itself.
       (unless (and (eql opcode +op-gc-check+)
                    (m68k-next-is-unlabelled-cons-p mvm-pc))
         (m68k-emit-gc-check buf)))

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
         (ecase (logand width 3)   ; widths 4..7 / 8+: the TLS / conv marks
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
         (ecase (logand width 3)   ; widths 4..7 / 8+: the TLS / conv marks
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
         (m68k-emit-addi buf +68k-d1+ (1- (m68k-granule)))
         (m68k-emit-andi buf +68k-d1+ (logand (- (m68k-granule)) #xFFFFFFFF))
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
         (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a0+ (- 4 +tag-object+))
         (when *68k-barrier-label*
           (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
           (m68k-emit-bsr buf *68k-barrier-label*))))

      (#.+op-array-len+
       ;; count = (header >> 8) & 0xFFFFFF, returned TAGGED.
       (let ((vd (first operands))
             (vobj (second operands)))
        (if *68k-array-len-label*
         (progn                          ; the shared ARRAY-LEN
           (m68k-load-vreg buf +68k-d0+ vobj)
           (m68k-emit-bsr buf *68k-array-len-label*)
           (m68k-store-vreg buf vd +68k-d0+))
         (progn
         (m68k-load-vreg buf +68k-d0+ vobj)
         (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
         (m68k-emit-move-disp-dn buf +68k-a0+ (- +tag-object+) +68k-d0+)
         (m68k-emit-lsr-imm buf +68k-d0+ 8)
         (m68k-emit-andi buf +68k-d0+ #xFFFFFF)
         (m68k-emit-lsl-imm buf +68k-d0+ 1)            ; tag as fixnum
         (m68k-store-vreg buf vd +68k-d0+)))))

      ;;; --- Byte vectors and strings ---
      ;; Payload starts right after the header longword, at obj - tag + 4 (obj - 5
      ;; with tag 9, obj - 3 with *68K-TAGS3*'s 7).  The 68k has no byte move with
      ;; a displacement here, so it is folded into the address before A0.  Shapes mirror
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
         (m68k-emit-addi buf +68k-d1+ (+ 3 (m68k-granule)))  ; N + 4 + granule - 1
         (m68k-emit-andi buf +68k-d1+ (logand (- (m68k-granule)) #xFFFFFFFF))
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
         (m68k-emit-addi buf +68k-d1+ (1- (m68k-granule)))
         (m68k-emit-andi buf +68k-d1+ (logand (- (m68k-granule)) #xFFFFFFFF))
         (m68k-emit-zero-and-bump buf)))

      (#.+op-u8-ref+
       (let ((vd (first operands))
             (varr (second operands))
             (vidx (third operands)))
         (m68k-load-vreg buf +68k-d0+ vidx)
         (m68k-emit-asr-imm buf +68k-d0+ 1)
         (m68k-load-vreg buf +68k-d1+ varr)
         (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
         (m68k-emit-subi buf +68k-d0+ (- +tag-object+ 4)) ; tag - header 4
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
         (m68k-emit-subi buf +68k-d0+ (- +tag-object+ 4))
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
       ;; MOVEQ + MOVE.L to absolute long: 8 bytes, was 14 (MOVE.L #, MOVEA.L #,
       ;; MOVE.L (A1)).  D0 is scratch; nothing after reads A1 for this address.
       (let ((n (logand (first operands) #xFF)))
         (m68k-emit-imm-dn buf n +68k-d0+)
         (m68k-emit-store-absl buf +68k-d0+ (m68k-nargs-addr))))

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
         (m68k-emit-imm-dn buf tagged +68k-d0+)
         (m68k-emit-store-absl buf +68k-d0+ (m68k-mvcount-addr))))

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
(defvar *68k-gc-kind-map* #x1FC00000
  "The kind map's FIXED address: just past the largest heap the stub will
   choose (#x10000000 + 248 MB + 4 MB guard), where ARM32's has always been.
   The stub maps only the S/16 bytes a heap of size S needs, so a small heap
   costs a small map, and the bias below stays a translate-time constant.
   A board with less address space below 2^29 (the 68k Mac: RAM ends at
   384 MB) moves it, and caps its heap below it.")
(defun m68k-kind-bias ()
  "K: the kind byte of address A is at K + (A >> kind-shift), one byte per
   heap granule."
  (- *68k-gc-kind-map* (ash (conv-real #x10000000) (- (m68k-kind-shift)))))
(defvar *68k-gc-static* nil
  "STATIC BOOT HEAP (MODUS_68K_GC_STATIC; unset, it follows MODUS_SMALL_IMAGE).
   A request word makes the collector, at the end of the collection that sees
   it, keep what that collection copied where it is -- the objects it copied up
   and the conses it copied down become a STATIC range each -- and continue on
   two semispaces the size of the room left between them.  Every later
   collection walks both ranges as typed roots (like the Cheney scan, leaf
   payloads skipped) and never copies them: what boot built is held once, not
   twice.  No barrier is needed because nothing static is skipped.  AArch64's
   GC-TENURE without its cards, on 68k's own words (+68K-GC-STATIC-BLOCK+).")

(defconstant +68k-gc-static-block+ #x10006000
  "Raw words: +0 request (non-zero: freeze at the end of the next collection),
   +4/+8 the static objects [lo, hi), +C/+10 the static conses [lo, hi).
   Below the heap, so the low-block root scan reads them: raw 16-aligned
   addresses, which carry no pointer tag.")

(defconstant +68k-card-marks+ #x10006100
  "One byte per static card (non-zero: an object starting in it was written
   since the collection that last scanned it), up to #x10007F00: 7,680 cards.")
(defconstant +68k-card-max+ 7680)
(defconstant +68k-card-crossing+ #x1001E000
  "One byte per static card: where its first object starts, in 16-byte
   granules from the card's start (255: none).  The 8 KB below the heap.")
(defvar *68k-barrier-label* nil
  "While translating a static-heap image: the shared write barrier.  D0 = the
   object (or cons) just stored into; marks its card when it is static.
   Clobbers D0, D1, A0.")

(defconstant +68k-gc-heap-start+ #x10020000
  "Where the first semispace starts (boot-linux-68k's heap + alloc-start).")

(defconstant +68k-gc-margin-max+ #x100000
  "The collector leaves min(1 MB, a quarter of a semispace) free below VL --
   COMPUTED from space_size at each collection, so it needs no stored word.")

(defvar *68k-gc-poison* nil
  "DEBUG, as *ARM32-GC-POISON*: every collection overwrites the semispace it
   evacuated with #x00000BA9, so a pointer the collector failed to update
   faults on its first read instead of reading the stale copy.  Set by
   build-68k-cli.lisp from MODUS_68K_GC_POISON=1.  Off: byte-identical.")

(defvar *68k-mac-console-label* nil
  "While translating a *68K-MAC-MODE* image: the label of the serial console's
   putc (M68K-EMIT-MAC-CONSOLE).")
(defvar *68k-mac-getc-label* nil
  "... and of its getc, 21 words on.")

(defparameter +68k-mac-console-code+
  #(#x48E7 #x6040 #x13C0 #x1000 #x9100 #x41F9 #x1000 #x9000 #x217C #x1000 #x9100
    #x0020 #x7001 #x2140 #x0024 #x4268 #x002C #xA003 #x4CDF #x0206 #x4E75 #x48E7
    #x6040 #x41F9 #x1000 #x9080 #x317C #x0002 #x001A #x42A8 #x001C #xA005 #x4AA8
    #x001C #x6600 #x0030 #x41F9 #x1000 #x9110 #x4250 #x7028 #xA031 #x3039 #x1000
    #x9110 #x0C40 #x0003 #x6700 #x000A #x0C40 #x0005 #x6600 #xFFC6 #x7000 #x1039
    #x1000 #x9115 #x6000 #x001E #x217C #x1000 #x9102 #x0020 #x7001 #x2140 #x0024
    #x4268 #x002C #xA002 #x7000 #x1039 #x1000 #x9102 #x4CDF #x0206 #x4E75)
  "The bare Mac's console, assembled (m68k-linux-gnu-as -m68040):

   putc:   movem.l d1-d2/a1,-(sp)       ; D0.B -> serial port A
           move.b  d0,$10009100
           lea     $10009000,a0          ; .AOut's parameter block
           move.l  #$10009100,32(a0)     ; ioBuffer
           moveq   #1,d0
           move.l  d0,36(a0)             ; ioReqCount
           clr.w   44(a0)                ; ioPosMode
           _Write                        ; A003
           movem.l (sp)+,d1-d2/a1
           rts
   getc:   movem.l d1-d2/a1,-(sp)       ; -> D0.L: a key, or a byte from port A
   wait:   lea     $10009080,a0          ; .AIn: bytes waiting?
           move.w  #2,26(a0)             ;   csCode 2
           clr.l   28(a0)
           _Status                       ; A005
           tst.l   28(a0)
           bne     serial
           lea     $10009110,a0          ; an EventRecord
           clr.w   (a0)
           moveq   #$28,d0               ; keyDownMask | autoKeyMask
           _GetOSEvent                   ; A031
           move.w  $10009110,d0          ; what
           cmp.w   #3,d0                 ; keyDown
           beq     key
           cmp.w   #5,d0                 ; autoKey
           bne     wait
   key:    moveq   #0,d0
           move.b  $10009115,d0          ; the message's low byte: the character
           bra     done
   serial: move.l  #$10009102,32(a0)
           moveq   #1,d0
           move.l  d0,36(a0)
           clr.w   44(a0)
           _Read                         ; A002
           moveq   #0,d0
           move.b  $10009102,d0
   done:   movem.l (sp)+,d1-d2/a1
           rts

   The ROM's .AIn/.AOut drivers, opened by the boot stub into the parameter
   blocks at #x10009080 / #x10009000 (the low block's argv area, unused on a
   Mac), and its event queue, which its keyboard driver fills; Basilisk II
   routes the serial port to a host device, a real Mac to its modem port.
   Both routines keep every register but D0/A0.")

(defun m68k-emit-mac-console (buf)
  "Emit +68K-MAC-CONSOLE-CODE+: putc at *68K-MAC-CONSOLE-LABEL*, getc (word
   21) at *68K-MAC-GETC-LABEL*."
  (m68k-emit-label buf *68k-mac-console-label*)
  (let ((code (copy-seq +68k-mac-console-code+)))
    ;; every absolute operand is a $1000_9xxx long: move it with the
    ;; runtime-data region (CONV-REAL)
    (loop for i from 0 below (1- (length code))
          when (and (= (aref code i) #x1000) (= (logand (aref code (1+ i)) #xF000) #x9000))
            do (let ((a (conv-real (logior #x10000000 (aref code (1+ i))))))
                 (setf (aref code i) (ash a -16) (aref code (1+ i)) (logand a #xFFFF))))
    (loop for w across code for i from 0
          do (when (= i 21) (m68k-emit-label buf *68k-mac-getc-label*))
             (m68k-emit-word buf w))))

(defun m68k-emit-mac-putc (buf)
  "D0.B to the Mac's serial console (D0/A0 clobbered)."
  (m68k-emit-bsr-long buf *68k-mac-console-label*))

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

(defun m68k-emit-lea-label (buf label-id an)
  "An = the address of LABEL-ID: LEA (bd.l,PC),An -- the 68020 full extension
   word (#x0170: base PC, index suppressed, 32-bit displacement), whose PC is
   the extension word's address, so the fixup's reference is set by hand -- or,
   on a 68000, LEA abs.L,An with the address filled in after layout."
  (cond
    (*68k-cpu-68000*
     (m68k-emit-word buf (logior #x41F9 (ash (logand an 7) 9)))   ; LEA abs.L,An
     (m68k-emit-abs32 buf label-id))
    (t
     (m68k-emit-word buf (logior #x41FB (ash (logand an 7) 9)))   ; LEA (bd.l,PC),An
     (let ((ext-pos (m68k-buffer-position buf)))
       (m68k-emit-word buf #x0170)
       (m68k-emit-word buf 0)
       (push (list (1- (m68k-buffer-word-count buf)) label-id :disp32 (+ ext-pos 2))
             (m68k-buffer-fixups buf))
       (m68k-emit-word buf 0)))))

(defun m68k-emit-bsr-long (buf label-id)
  "BSR.L target -- 68020+, 32-bit displacement, so a gc-check anywhere in an
   image of any size reaches the collector emitted after the last function."
  (when *68k-cpu-68000*
    (m68k-emit-word buf #x4EB9)                          ; JSR abs.L
    (m68k-emit-abs32 buf label-id)
    (return-from m68k-emit-bsr-long nil))
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
    (m68k-emit-lsr-imm buf +68k-d0+ (m68k-kind-shift))
    (m68k-emit-move-imm-an buf (m68k-kind-bias) +68k-a0+) ; A0 = K
    (m68k-emit-adda-dn buf +68k-d0+ +68k-a0+)
    (m68k-emit-moveq buf +68k-d0+ kind)
    (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a0+)))

;;; SHARED SEQUENCES (*68K-SHARED-SEQUENCES*, MODUS_SMALL_IMAGE / MODUS_68K_SHARED).
;;; The most repeated native sequences, emitted once after the collector and
;;; called with BSR, instead of inline at every site.  First: CONS, and the
;;; GC-CHECK the compiler puts right before each one.

(defvar *68k-shared-sequences* nil
  "When T, CONS (with the GC-CHECK before it) is a call to one shared copy.")
(defvar *68k-cons-label* nil
  "Label of the shared CONS sequence while translating, else NIL.")
(defvar *68k-bytecode* nil
  "The bytecode being translated, for a one-instruction lookahead.")
(defvar *68k-label-map* nil
  "The translation's bytecode offset -> label map.")

(defun m68k-emit-cons-helper (buf)
  "The shared CONS: D0 = car, D1 = cdr in; D0 = the tagged cons out.  Clobbers
   A0.  The heap check comes first (a GC-CHECK just before a CONS is dropped
   at the site, see the GC-CHECK arm): a collection here sees D0 and D1, which
   the collector saves to the stack, scans and restores, as the roots they are."
  (m68k-emit-label buf *68k-cons-label*)
  (m68k-emit-gc-check buf)
  (when *68k-gc-label*
    (m68k-emit-push-dn buf +68k-d0+)
    (m68k-emit-alloc-mark buf 2)                    ; clobbers D0 and A0
    (m68k-emit-pop-dn buf +68k-d0+))
  (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)   ; car
  (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a2+ 4)   ; cdr
  (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
  (m68k-emit-addq buf +68k-d0+ +tag-cons+)            ; 16-aligned: + tag = OR tag
  (m68k-emit-lea-disp buf +68k-a2+ (m68k-granule) +68k-a2+)       ; 16, as the inline CONS
  (m68k-emit-rts buf))

(defvar *68k-subtag-label* nil
  "Label of the shared OBJ-SUBTAG sequence while translating, else NIL.")
(defvar *68k-car-label* nil
  "Label of the shared CAR (D0 = car of D0, trap unless a cons), else NIL.")
(defvar *68k-cdr-label* nil
  "Label of the shared CDR, else NIL.")

(defun m68k-emit-car-cdr-helpers (buf)
  "The shared CAR and CDR: D0 = a cons in; D0 = its car / cdr.  The same
   type check as the inline form (ILLEGAL on a non-cons).  Clobbers D1, A0."
  (loop for (label disp) in (list (list *68k-car-label* (- +tag-cons+))
                                  (list *68k-cdr-label* (- 4 +tag-cons+)))
        do (m68k-emit-label buf label)
           (m68k-emit-check-cons buf +68k-d0+)
           (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
           (m68k-emit-move-disp-dn buf +68k-a0+ disp +68k-d0+)
           (m68k-emit-rts buf)))
(defvar *68k-array-len-label* nil
  "Label of the shared ARRAY-LEN sequence while translating, else NIL.")

(defun m68k-emit-subtag-helper (buf)
  "The shared OBJ-SUBTAG: D0 = any value in; D0 = its header's subtag, tagged,
   for a real heap object, else 0 (T included).  Clobbers D1 and A0."
  (let ((zero (mvm-make-label)))
    (m68k-emit-label buf *68k-subtag-label*)
    (m68k-emit-cmpi buf +68k-d0+ +t-value+)
    (m68k-emit-beq buf zero)
    (m68k-emit-moveq buf +68k-d1+ (m68k-tag-mask))
    (m68k-emit-and-dn-dn buf +68k-d0+ +68k-d1+)
    (m68k-emit-cmpi-byte buf +68k-d1+ +tag-object+)
    (m68k-emit-bne buf zero)
    (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
    (m68k-emit-moveq buf +68k-d0+ 0)
    (m68k-emit-move-byte-disp-dn buf +68k-a0+ (- 3 +tag-object+) +68k-d0+)
    (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d0+)
    (m68k-emit-rts buf)
    (m68k-emit-label buf zero)
    (m68k-emit-moveq buf +68k-d0+ 0)
    (m68k-emit-rts buf)))

(defun m68k-emit-array-len-helper (buf)
  "The shared ARRAY-LEN: D0 = an object in; D0 = its header's count, tagged.
   Clobbers A0."
  (m68k-emit-label buf *68k-array-len-label*)
  (m68k-emit-move-dn-an buf +68k-d0+ +68k-a0+)
  (m68k-emit-move-disp-dn buf +68k-a0+ (- +tag-object+) +68k-d0+)
  (m68k-emit-lsr-imm buf +68k-d0+ 8)
  (m68k-emit-andi buf +68k-d0+ #xFFFFFF)
  (m68k-emit-lsl-imm buf +68k-d0+ 1)
  (m68k-emit-rts buf))

(defvar *68k-alloc-obj-label* nil
  "Label of the shared ALLOC-OBJ sequence while translating, else NIL.")

(defun m68k-emit-alloc-obj-helper (buf)
  "The shared ALLOC-OBJ: D0 = the header, D1 = the object's total bytes (a
   multiple of 16, at least 16) in; D0 = the tagged object out, every word past
   the header zeroed.  Clobbers D1 and A0.  The site's GC-CHECK-N stays at the
   site, so no collection runs in here."
  (let ((zl (mvm-make-label)))
    (m68k-emit-label buf *68k-alloc-obj-label*)
    (when *68k-gc-label*
      (m68k-emit-push-dn buf +68k-d0+)
      (m68k-emit-alloc-mark buf 1)                    ; clobbers D0 and A0
      (m68k-emit-pop-dn buf +68k-d0+))
    (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a2+)   ; header
    (m68k-emit-lea-disp buf +68k-a2+ 4 +68k-a0+)
    (m68k-emit-move-dn-dn buf +68k-d1+ +68k-d0+)
    (m68k-emit-lsr-imm buf +68k-d0+ 2)
    (m68k-emit-subq buf +68k-d0+ 1)                   ; words past the header
    (m68k-emit-label buf zl)
    (m68k-emit-word buf #x4298)                       ; CLR.L (A0)+
    (m68k-emit-subq buf +68k-d0+ 1)
    (m68k-emit-bne buf zl)
    (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
    (m68k-emit-ori buf +68k-d0+ +tag-object+)
    (m68k-emit-adda-dn buf +68k-d1+ +68k-a2+)          ; bump
    (m68k-emit-rts buf)))

;;; MODUS_68K_THREAD_STATS: what the image would cost as TOKEN-THREADED code --
;;; a 2-byte token per MVM instruction naming a shared native handler (the
;;; instruction's own native code with its register operands baked in, plus a
;;; 6-byte dispatch), the instruction's other operands inline after the token.
;;; Policy A puts every non-register operand inline; policy B bakes byte-sized
;;; ones into the handler.  Inline operands are 2 bytes when they fit a signed
;;; word, else 4.  A handler is costed at its representative's native size + 6
;;; (+ 4 for each inline operand it must fetch).
(defun m68k-thread-stats-note (ts opcode operands native)
  (let* ((info (gethash opcode *opcode-table*))
         (types (and info (opcode-info-operands info)))
         (v (third ts))
         (flag-op (member opcode (list +op-cmp+ +op-test+ +op-cmp-imm+ +op-test-imm+)))
         (cond-br (member opcode (list +op-beq+ +op-bne+ +op-blt+ +op-bge+ +op-ble+
                                       +op-bgt+ +op-bvs+))))
    (incf (aref v 0)) (incf (aref v 1) native)
    ;; Policy C: byte-sized inline operands by value (1/2/4), the flag setter
    ;; and the conditional branch after it fused into one shape; token size is
    ;; decided in the report (1 byte for the 255 commonest shapes, else 3).
    (let* ((h (fourth ts))
           (pend (aref v 2))                    ; a flag setter's (key . inline)
           (key (list opcode)) (inline 0))
      (loop for ty in types for x in operands
            do (cond ((eq ty :reg) (push x key))
                     ((eq ty :imm8) (push x key))
                     (t (let ((n (cond ((not (integerp x)) 4)
                                       ((<= -128 x 127) 1) ((<= -32768 x 32767) 2) (t 4))))
                          (push n key) (incf inline n)))))
      (cond ((and cond-br (consp pend))
             (setq key (append key (car pend)) inline (+ inline (cdr pend)))
             (setf (aref v 2) 0))
            ((consp pend)
             (let ((e (or (gethash (car pend) h) (setf (gethash (car pend) h) (list 0 0)))))
               (incf (first e)) (incf (second e) (cdr pend)))
             (setf (aref v 2) 0)))
      (if flag-op
          (setf (aref v 2) (cons key inline))
          (let ((e (or (gethash key h) (setf (gethash key h) (list 0 0)))))
            (incf (first e)) (incf (second e) inline))))
    (loop for pol in '(:a :b) for h in (list (first ts) (second ts))
          do (let ((key (list opcode)) (inline 0) (nparam 0))
               (loop for ty in types for x in operands
                     do (cond ((eq ty :reg) (push x key))
                              ((and (eq pol :b) (eq ty :imm8)) (push x key))
                              (t (push :p key) (incf nparam)
                                 (incf inline (if (and (integerp x) (<= -32768 x 32767)) 2 4)))))
               (let ((e (gethash key h)))
                 (unless e
                   (setq e (setf (gethash key h) (list 0 0 (+ native 6 (* 4 nparam))))))
                 (incf (first e)) (incf (second e) (+ 2 inline)))))))

(defun m68k-thread-stats-report (ts)
  (let ((v (third ts)))
    (format t "~&;; 68k thread stats: ~D instructions, ~D native bytes~%" (aref v 0) (aref v 1))
    (loop for pol in '("A all inline" "B imm8 baked") for h in (list (first ts) (second ts))
          do (let ((tok 0) (hand 0) (shapes 0) (single 0))
               (maphash (lambda (k e) (declare (ignore k))
                          (incf shapes) (incf tok (second e)) (incf hand (third e))
                          (when (= 1 (first e)) (incf single)))
                        h)
               (format t ";;   ~A: tokens ~D B, ~D shapes (~D used once), handlers ~D B, total ~D B (~,2Fx native)~%"
                       pol tok shapes single hand (+ tok hand)
                       (/ (+ tok hand) (max 1 (aref v 1))))))
    (let ((rows nil) (inl 0) (tok 0) (n 0))
      (maphash (lambda (k e) (push (cons k e) rows) (incf inl (second e))) (fourth ts))
      (setq rows (sort rows #'> :key #'second))
      (loop for r in rows for i from 0
            do (incf n (second r)) (incf tok (* (second r) (if (< i 255) 1 3))))
      (format t ";;   C byte tokens, fused cmp+branch: ~D tokens, ~D shapes, tokens ~D B + operands ~D B = ~D B (~,2Fx native); top 255 shapes cover ~,1F%~%"
              n (length rows) tok inl (+ tok inl) (/ (+ tok inl) (max 1 (aref v 1)))
              (* 100.0 (/ (loop for r in rows for i below 255 sum (second r)) (max 1 n)))))))

(defvar *68k-skip-to* nil
  "Set by a handler that consumed the instructions after its own: the MVM pc
   the translation loop resumes at.")

(defun m68k-range-guard-at (next-pc vd vs amt)
  "When SHL VD,VS,AMT is followed by SAR VD,VD,AMT / CMP VD,VS / BNE, none a
   branch target, under *ARITH-RANGE-BY-SHIFT*: (values PC-AFTER-BNE TARGET)."
  (when (and *arith-range-by-shift* *68k-bytecode* (/= vd vs) (<= 1 amt 8))
    (let ((bc *68k-bytecode*) (pc next-pc) (ops nil))
      (dotimes (i 3)
        (when (or (>= pc (length bc)) (gethash pc *68k-label-map*))
          (return-from m68k-range-guard-at nil))
        (let ((d (decode-instruction bc pc)))
          (push (cons (car d) (cadr d)) ops)
          (setq pc (cddr d))))
      (destructuring-bind (bne cmp sar) ops
        (when (and (eql (car sar) +op-sar+) (equal (cdr sar) (list vd vd amt))
                   (eql (car cmp) +op-cmp+) (equal (cdr cmp) (list vd vs))
                   (eql (car bne) +op-bne+))
          (values pc (+ pc (second bne))))))))

(defun m68k-next-is-unlabelled-cons-p (next-pc)
  "True when the instruction at NEXT-PC is a CONS that no branch lands on."
  (and *68k-cons-label* *68k-bytecode*
       (< next-pc (length *68k-bytecode*))
       (null (gethash next-pc *68k-label-map*))
       (eql (car (decode-instruction *68k-bytecode* next-pc)) +op-cons+)))

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
   Does not return.  On a bare Mac: MESSAGE to the serial console, then halt."
  (when *68k-mac-mode*
    (loop for c across message
          do (m68k-emit-moveq buf +68k-d0+ (char-code c))
             (m68k-emit-mac-putc buf))
    (m68k-emit-word buf #x60FE)                         ; BRA.S *
    (return-from m68k-emit-die nil))
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
  (m68k-emit-lsr-imm buf dst-dn (m68k-kind-shift))
  (m68k-emit-add-dn-dn buf +68k-d6+ dst-dn))

(defun m68k-emit-object-size (buf dst hdr)
  "DST = the byte size of the object whose header word is in HDR, aligned to
   16.  Mirrors every 68k allocation site: a u8 vector (#x11) and a narrow
   string (#x13, %MAKE-BASE-STRING: a u8 vector with its subtag rewritten)
   count bytes after their 4-byte header, everything else (count + 1)
   longwords.  Clobbers only DST."
  (let ((words (mvm-make-label)) (bytes (mvm-make-label)) (al (mvm-make-label)))
    (m68k-emit-move-dn-dn buf hdr dst)
    (m68k-emit-andi buf dst #xFF)
    (m68k-emit-cmpi buf dst +subtag-u8-vector+)
    (m68k-emit-beq buf bytes)
    (m68k-emit-cmpi buf dst #x13)                 ; narrow string
    (m68k-emit-bne buf words)
    (m68k-emit-label buf bytes)
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
    (m68k-emit-addi buf dst (1- (m68k-granule)))
    (m68k-emit-andi buf dst (logand (- (m68k-granule)) #xFFFFFFFF))))

(defparameter *68k-gc-leaf-subtags* '(#x10 #x11 #x12 #x13 #x14 #x16 #x60 #x64 #x65 #x66)
  "Objects whose payload holds no pointers: copied, never scanned.  ARM32's
   list, plus #x13: a narrow string's payload is bytes.")

(defun m68k-emit-barrier-helper (buf)
  "The write barrier: D0 = an object or cons just stored into.  When it lies
   in the static span [lo, lo + span) its card's mark is set.  Before the
   freeze (and when the span was too big for the mark table) span is 0 and
   nothing is marked; the collector then walks the whole static heap."
  (let ((out (mvm-make-label)) (blk (conv-real +68k-gc-static-block+)))
    (m68k-emit-label buf *68k-barrier-label*)
    (m68k-emit-word buf #x90B9) (m68k-emit-long buf (+ blk #x18))   ; SUB.L lo,D0
    (m68k-emit-word buf #xB0B9) (m68k-emit-long buf (+ blk #x1C))   ; CMP.L span,D0
    (m68k-emit-bcc-word buf +68k-cc-cc+ out)                         ; outside the span
    (m68k-emit-word buf #x2239) (m68k-emit-long buf (+ blk #x20))   ; MOVE.L shift,D1
    (m68k-emit-word buf #xE2A8)                                     ; LSR.L D1,D0
    (m68k-emit-move-imm-an buf (conv-real +68k-card-marks+) +68k-a0+)           ; 6 bytes
    (m68k-emit-word buf #x50F0) (m68k-emit-word buf #x0800)         ; ST (0,A0,D0.L)
    (m68k-emit-label buf out)
    (m68k-emit-rts buf)))

(defun m68k-emit-gc-card-walk (buf scan)
  "Collector, static-heap images whose span has cards: scan only the marked
   cards.  An object card scans every object STARTING in it (its first from the
   crossing table), a cons card its conses.  A scanned card is cleared, and
   marked again when a slot still points into to-space afterwards, so a static
   object keeps being scanned while it refers to the young space.  Uses D7
   (card), A5 (cursor), A4, D3, D1, D0 and the scratch longword at blk+#x24."
  (let* ((blk (conv-real +68k-gc-static-block+))
         (cl (mvm-make-label)) (cdone (mvm-make-label)) (cnext (mvm-make-label))
         (conses (mvm-make-label)) (olp (mvm-make-label)) (odone (mvm-make-label))
         (sl (mvm-make-label)) (leaf (mvm-make-label)) (keep (mvm-make-label))
         (cpl (mvm-make-label)) (aftercard (mvm-make-label)))
    (flet ((here (l) (m68k-emit-label buf l))
           (abs-dn (addr dn)                                   ; MOVE.L (addr).L,Dn
             (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dn 7 1))
             (m68k-emit-long buf addr))
           (young-check (slot-an)
             ;; flag |= (to_start <= (slot) < to_start + size)
             (let ((no (mvm-make-label)))
               (m68k-emit-move-an-ind-dn buf slot-an +68k-d0+)
               (m68k-emit-move-imm-an buf (conv-real +68k-gc-meta+) +68k-a0+)
               (m68k-emit-move-disp-dn buf +68k-a0+ #x08 +68k-d1+)   ; to_start
               (m68k-emit-sub-dn-dn buf +68k-d1+ +68k-d0+)           ; v - to_start
               (m68k-emit-move-disp-dn buf +68k-a0+ #x10 +68k-d1+)   ; size
               (m68k-emit-cmp-dn-dn buf +68k-d1+ +68k-d0+)
               (m68k-emit-bcc-word buf +68k-cc-cc+ no)               ; unsigned >= size
               (m68k-emit-move-imm-an buf (+ blk #x24) +68k-a0+)
               (m68k-emit-moveq buf +68k-d0+ 1)
               (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)
               (m68k-emit-label buf no))))
      (m68k-emit-moveq buf +68k-d7+ 0)
      (here cl)
      ;; ncards = span >> shift
      (abs-dn (+ blk #x1C) +68k-d0+)
      (abs-dn (+ blk #x20) +68k-d1+)
      (m68k-emit-word buf #xE2A8)                           ; LSR.L D1,D0
      (m68k-emit-cmp-dn-dn buf +68k-d0+ +68k-d7+)            ; card - ncards
      (m68k-emit-bcc-word buf +68k-cc-cc+ cdone)
      (m68k-emit-move-imm-an buf (conv-real +68k-card-marks+) +68k-a0+)
      (m68k-emit-word buf #x4A30) (m68k-emit-word buf #x7800) ; TST.B (0,A0,D7.L)
      (m68k-emit-beq buf cnext)
      (m68k-emit-word buf #x4230) (m68k-emit-word buf #x7800) ; CLR.B (0,A0,D7.L)
      (m68k-emit-move-imm-an buf (+ blk #x24) +68k-a0+)
      (m68k-emit-moveq buf +68k-d0+ 0)
      (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)       ; flag = 0
      ;; A5 = card start = lo + (card << shift);  A4 = card end
      (abs-dn (+ blk #x20) +68k-d1+)
      (m68k-emit-move-dn-dn buf +68k-d7+ +68k-d0+)
      (m68k-emit-word buf #xE3A8)                           ; LSL.L D1,D0
      (abs-dn (+ blk #x18) +68k-d3+)
      (m68k-emit-add-dn-dn buf +68k-d3+ +68k-d0+)
      (m68k-emit-move-dn-an buf +68k-d0+ +68k-a5+)
      (m68k-emit-moveq buf +68k-d3+ 1)
      (m68k-emit-word buf #xE3AB)                           ; LSL.L D1,D3
      (m68k-emit-add-dn-dn buf +68k-d3+ +68k-d0+)
      (m68k-emit-move-dn-an buf +68k-d0+ +68k-a4+)           ; card end
      ;; an object card when its start is below the static objects' end
      (abs-dn (+ blk 8) +68k-d0+)
      (m68k-emit-cmpa buf +68k-a5+ +68k-d0+)                 ; A5 - ohi
      (m68k-emit-bcc-word buf +68k-cc-cc+ conses)
      ;; A5 += crossing[card] * 16 (255: nothing starts here)
      (m68k-emit-move-imm-an buf (conv-real +68k-card-crossing+) +68k-a0+)
      (m68k-emit-moveq buf +68k-d0+ 0)
      (m68k-emit-word buf #x1030) (m68k-emit-word buf #x7800) ; MOVE.B (0,A0,D7.L),D0
      (m68k-emit-cmpi buf +68k-d0+ 255)
      (m68k-emit-beq buf aftercard)
      (m68k-emit-lsl-imm buf +68k-d0+ (m68k-kind-shift))      ; granules
      (m68k-emit-word buf #xDBC0)                           ; ADDA.L D0,A5
      ;; objects starting in [A5, card end), and below ohi
      (here olp)
      (m68k-emit-cmpa-an buf +68k-a4+ +68k-a5+)              ; A5 - card end
      (m68k-emit-bcc-word buf +68k-cc-cc+ aftercard)
      (abs-dn (+ blk 8) +68k-d0+)
      (m68k-emit-cmpa buf +68k-a5+ +68k-d0+)                 ; A5 - ohi
      (m68k-emit-bcc-word buf +68k-cc-cc+ aftercard)
      (m68k-emit-move-an-ind-dn buf +68k-a5+ +68k-d3+)       ; header
      (m68k-emit-object-size buf +68k-d0+ +68k-d3+)
      ;; the object's end, kept on the stack across its slots
      (m68k-emit-move-an-dn buf +68k-a5+ +68k-d1+)
      (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d1+)
      (m68k-emit-word buf #x2F01)                           ; MOVE.L D1,-(SP)
      (m68k-emit-move-an-an buf +68k-a4+ +68k-a0+)
      (m68k-emit-word buf #x2F08)                           ; MOVE.L A0,-(SP): card end
      (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d1+)
      (m68k-emit-andi buf +68k-d1+ #xFF)
      (dolist (st *68k-gc-leaf-subtags*)
        (m68k-emit-cmpi buf +68k-d1+ st)
        (m68k-emit-beq buf leaf))
      (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d1+)
      (m68k-emit-lsr-imm buf +68k-d1+ 8)
      (m68k-emit-lsl-imm buf +68k-d1+ 2)
      (m68k-emit-addq buf +68k-d1+ 4)
      (m68k-emit-move-an-dn buf +68k-a5+ +68k-d0+)
      (m68k-emit-add-dn-dn buf +68k-d0+ +68k-d1+)
      (m68k-emit-move-dn-an buf +68k-d1+ +68k-a4+)           ; A4 = slot end
      (m68k-emit-addq-an buf +68k-a5+ 4)
      (here sl)
      (m68k-emit-cmpa-an buf +68k-a4+ +68k-a5+)
      (m68k-emit-bcc-word buf +68k-cc-cc+ leaf)
      (m68k-emit-move-an-an buf +68k-a5+ +68k-a0+)
      (m68k-emit-bsr buf scan)
      (young-check +68k-a5+)
      (m68k-emit-addq-an buf +68k-a5+ 4)
      (m68k-emit-bra buf sl)
      (here leaf)
      (m68k-emit-word buf #x285F)                           ; MOVEA.L (SP)+,A4: card end
      (m68k-emit-word buf #x2A5F)                           ; MOVEA.L (SP)+,A5: next object
      (m68k-emit-bra buf olp)
      ;; a cons card: the conses in [max(start, clo), min(end, chi))
      (here conses)
      (abs-dn (+ blk 12) +68k-d0+)
      (m68k-emit-cmpa buf +68k-a5+ +68k-d0+)                 ; A5 - clo
      (m68k-emit-bcc-word buf +68k-cc-cc+ cpl)
      (m68k-emit-move-dn-an buf +68k-d0+ +68k-a5+)
      (here cpl)
      (m68k-emit-cmpa-an buf +68k-a4+ +68k-a5+)
      (m68k-emit-bcc-word buf +68k-cc-cc+ aftercard)
      (abs-dn (+ blk 16) +68k-d0+)
      (m68k-emit-cmpa buf +68k-a5+ +68k-d0+)                 ; A5 - chi
      (m68k-emit-bcc-word buf +68k-cc-cc+ aftercard)
      (m68k-emit-move-an-an buf +68k-a5+ +68k-a0+)
      (m68k-emit-bsr buf scan)
      (young-check +68k-a5+)
      (m68k-emit-lea-disp buf +68k-a5+ 4 +68k-a0+)
      (m68k-emit-bsr buf scan)
      (m68k-emit-lea-disp buf +68k-a5+ 4 +68k-a1+)
      (young-check +68k-a1+)
      (m68k-emit-lea-disp buf +68k-a5+ (m68k-granule) +68k-a5+)
      (m68k-emit-bra buf cpl)
      ;; re-mark the card if it still refers to the young space
      (here aftercard)
      (m68k-emit-move-imm-an buf (+ blk #x24) +68k-a0+)
      (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
      (m68k-emit-tst buf +68k-d0+)
      (m68k-emit-beq buf cnext)
      (m68k-emit-move-imm-an buf (conv-real +68k-card-marks+) +68k-a0+)
      (m68k-emit-word buf #x50F0) (m68k-emit-word buf #x7800) ; ST (0,A0,D7.L)
      (here cnext)
      (m68k-emit-addq buf +68k-d7+ 1)
      (m68k-emit-bra buf cl)
      (here cdone))))

(defun m68k-emit-gc-static-walk (buf scan)
  "Collector: scan every slot of the static objects [+4, +8) -- leaf payloads
   skipped, as the Cheney object scan does -- and the car and cdr of every
   static cons [+C, +10).  SCAN is the collector's slot routine (A0 = slot).
   Uses A5, A4, D7, D3, D1, D0 (all saved by the collector's entry)."
  (let ((ol (mvm-make-label)) (od (mvm-make-label)) (sl (mvm-make-label))
        (leaf (mvm-make-label)) (cl (mvm-make-label)) (cd (mvm-make-label))
        (blk (conv-real +68k-gc-static-block+)))
    (flet ((here (l) (m68k-emit-label buf l)))
      ;; -- objects: A5 = cursor, [blk+8] = end --
      (m68k-emit-move-imm-an buf blk +68k-a0+)
      (m68k-emit-move-disp-an buf +68k-a0+ 4 +68k-a5+)
      (here ol)
      (m68k-emit-move-imm-an buf blk +68k-a0+)
      (m68k-emit-move-disp-dn buf +68k-a0+ 8 +68k-d0+)
      (m68k-emit-cmpa buf +68k-a5+ +68k-d0+)                   ; A5 - end
      (m68k-emit-bcc-word buf +68k-cc-cc+ od)
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
      (m68k-emit-move-dn-an buf +68k-d7+ +68k-a5+)
      (m68k-emit-bra buf ol)
      (here od)
      ;; -- conses: A5 = cursor, [blk+16] = end --
      (m68k-emit-move-imm-an buf blk +68k-a0+)
      (m68k-emit-move-disp-an buf +68k-a0+ 12 +68k-a5+)
      (here cl)
      (m68k-emit-move-imm-an buf blk +68k-a0+)
      (m68k-emit-move-disp-dn buf +68k-a0+ 16 +68k-d0+)
      (m68k-emit-cmpa buf +68k-a5+ +68k-d0+)
      (m68k-emit-bcc-word buf +68k-cc-cc+ cd)
      (m68k-emit-move-an-an buf +68k-a5+ +68k-a0+)
      (m68k-emit-bsr buf scan)
      (m68k-emit-lea-disp buf +68k-a5+ 4 +68k-a0+)
      (m68k-emit-bsr buf scan)
      (m68k-emit-lea-disp buf +68k-a5+ (m68k-granule) +68k-a5+)
      (m68k-emit-bra buf cl)
      (here cd))))

(defun m68k-emit-gc-static-freeze (buf)
  "Collector, after the swap (A1 = meta, A2 = object free, A3 = cons low, D4 =
   the evacuated semispace): when the request word is set and nothing is static
   yet, keep [from, A2) and [A3, from + size) as the static ranges and continue
   on semispaces of size A3 - A2: the gap [A2, A3) and the start of the
   evacuated one.  Clears the request.  Clobbers D0, D1, A0."
  (let ((skip (mvm-make-label)) (blk (conv-real +68k-gc-static-block+)))
    (m68k-emit-move-imm-an buf blk +68k-a0+)
    (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)          ; request
    (m68k-emit-tst buf +68k-d0+)
    (m68k-emit-beq buf skip)
    (m68k-emit-move-disp-dn buf +68k-a0+ 8 +68k-d0+)          ; already static?
    (m68k-emit-tst buf +68k-d0+)
    (m68k-emit-bne buf skip)
    (m68k-emit-move-disp-dn buf +68k-a1+ #x00 +68k-d0+)       ; from (= old to)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a0+ 4)          ; objects lo
    (m68k-emit-move-an-disp buf +68k-a2+ +68k-a0+ 8)          ; objects hi
    (m68k-emit-move-an-disp buf +68k-a3+ +68k-a0+ 12)         ; conses lo
    (m68k-emit-move-disp-dn buf +68k-a1+ #x10 +68k-d1+)
    (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a0+ 16)         ; conses hi
    (m68k-emit-move-an-dn buf +68k-a3+ +68k-d0+)
    (m68k-emit-move-an-dn buf +68k-a2+ +68k-d1+)
    (m68k-emit-sub-dn-dn buf +68k-d1+ +68k-d0+)               ; size' = A3 - A2
    (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ #x10)
    (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a1+ #x00)       ; from = A2
    (m68k-emit-moveq buf +68k-d0+ 0)
    (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)          ; request done
    (m68k-emit-label buf skip)))

(defun m68k-emit-gc-collector (buf)
  "Emit the hosted collector at *68K-GC-LABEL*.  Entered by BSR.L from
   :gc-check; every register is preserved except the new VA/VL it installs."
  (let ((scan (mvm-make-label))
        (meta (conv-real +68k-gc-meta+))
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
      (m68k-emit-move-imm-dn buf (m68k-kind-bias) +68k-d6+)    ; K
      ;; ---- roots: the stack from the save frame, then the low block ----
      (m68k-emit-move-an-an buf +68k-a7+ +68k-a5+)
      (m68k-emit-move-disp-dn buf +68k-a1+ #x18 +68k-d7+)        ; stack_base
      (root-range)
      (m68k-emit-move-imm-an buf (conv-real #x10000000) +68k-a5+)
      ;; the low block ends where the FIRST semispace starts, whichever one is
      ;; from-space now: min(from_start, to_start).  TO_START FROM THE BLOCK,
      ;; NOT A2: the stack roots above have already copied objects, so A2 is
      ;; the to-space FREE pointer by now.  Using it made the range run into
      ;; to-space at every collection whose to-space is the LOW semispace (the
      ;; second, fourth...), scanning the objects just copied -- byte-vector
      ;; payloads included -- as conservative roots: a bytecode word that
      ;; happened to spell a live from-space cons was "forwarded" in place, and
      ;; the interpreter then decoded garbage.
      ;; With a static heap the semispaces no longer start at the heap's start
      ;; (the static ranges sit inside them), so the low block ends at the
      ;; heap's FIXED start, as on arm32 -- a typed range must never be read
      ;; as conservative roots.
      (if *68k-gc-static*
          (m68k-emit-move-imm-dn buf (conv-real +68k-gc-heap-start+) +68k-d7+)
          (let ((have (lbl)))
            (m68k-emit-move-dn-dn buf +68k-d4+ +68k-d7+)
            (m68k-emit-move-imm-an buf meta +68k-a0+)
            (m68k-emit-move-disp-dn buf +68k-a0+ #x08 +68k-d0+)       ; to_start
            (m68k-emit-cmp-dn-dn buf +68k-d0+ +68k-d7+)               ; from_start - to_start
            (m68k-emit-bcc-word buf +68k-cc-cs+ have)
            (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d7+)
            (here have)))
      (root-range)
      ;; ---- the static ranges, typed (empty until a freeze) ----
      (when *68k-gc-static*
        (let ((full (mvm-make-label)) (done (mvm-make-label)))
          (m68k-emit-move-imm-an buf (conv-real (+ +68k-gc-static-block+ #x1C)) +68k-a0+)
          (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d0+)
          (m68k-emit-tst buf +68k-d0+)
          (m68k-emit-beq buf full)
          (m68k-emit-gc-card-walk buf scan)
          (m68k-emit-bra buf done)
          (m68k-emit-label buf full)
          (m68k-emit-gc-static-walk buf scan)
          (m68k-emit-label buf done)))
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
        (m68k-emit-lea-disp buf +68k-a6+ (- (m68k-granule)) +68k-a6+)
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
      ;; ---- hand the evacuated semispace's pages back (static-heap images) ----
      ;; madvise(MADV_DONTNEED) on the page-aligned inside of [D4, D5): resident
      ;; memory is then what lives plus what is being allocated into, not every
      ;; page either semispace ever touched -- at the freeze, the whole of the
      ;; semispace boot filled.  Not with poison, which wants the pages written.
      (when (and *68k-gc-static* (not *68k-gc-poison*) *68k-linux-mode*)
        (let ((none (lbl)))
          (m68k-emit-move-dn-dn buf +68k-d4+ +68k-d1+)
          (m68k-emit-addi buf +68k-d1+ 4095)
          (m68k-emit-andi buf +68k-d1+ #xFFFFF000)                  ; start, up
          (m68k-emit-move-dn-dn buf +68k-d5+ +68k-d2+)
          (m68k-emit-andi buf +68k-d2+ #xFFFFF000)                  ; end, down
          (m68k-emit-sub-dn-dn buf +68k-d1+ +68k-d2+)               ; length
          (m68k-emit-bcc-word buf +68k-cc-ls+ none)                 ; <= 0
          (m68k-emit-move-imm-dn buf 238 +68k-d0+)                  ; madvise
          (m68k-emit-moveq buf +68k-d3+ 4)                          ; MADV_DONTNEED
          (m68k-emit-word buf #x4E40)                               ; TRAP #0
          (here none))
        ;; ... and the kind bytes of that range, zeroed above: a released page
        ;; reads back as zero, which is what they hold.
        (let ((none (lbl)))
          (m68k-emit-kind-addr buf +68k-d1+ +68k-d4+)
          (m68k-emit-addi buf +68k-d1+ 4095)
          (m68k-emit-andi buf +68k-d1+ #xFFFFF000)
          (m68k-emit-kind-addr buf +68k-d2+ +68k-d5+)
          (m68k-emit-andi buf +68k-d2+ #xFFFFF000)
          (m68k-emit-sub-dn-dn buf +68k-d1+ +68k-d2+)
          (m68k-emit-bcc-word buf +68k-cc-ls+ none)
          (m68k-emit-move-imm-dn buf 238 +68k-d0+)
          (m68k-emit-moveq buf +68k-d3+ 4)
          (m68k-emit-word buf #x4E40)
          (here none)))
      ;; MODUS_68K_GC_PEAK=1 (a measurement): the most bytes any collection
      ;; kept -- objects [to_start, A2) plus conses [A3, to_end) -- at
      ;; #x10006080, and the latest at #x10006084: what an old space that is
      ;; never copied (generational promotion) would have to hold.
      (when (equal (sb-ext:posix-getenv "MODUS_68K_GC_PEAK") "1")
        (let ((no (lbl)))
          (m68k-emit-move-imm-an buf meta +68k-a1+)
          (m68k-emit-move-an-dn buf +68k-a2+ +68k-d0+)
          (m68k-emit-move-disp-dn buf +68k-a1+ #x08 +68k-d1+)
          (m68k-emit-sub-dn-dn buf +68k-d1+ +68k-d0+)             ; objects
          (m68k-emit-move-disp-dn buf +68k-a1+ #x10 +68k-d2+)
          (m68k-emit-add-dn-dn buf +68k-d2+ +68k-d1+)             ; to_end
          (m68k-emit-move-an-dn buf +68k-a3+ +68k-d2+)
          (m68k-emit-sub-dn-dn buf +68k-d2+ +68k-d1+)             ; conses
          (m68k-emit-add-dn-dn buf +68k-d1+ +68k-d0+)
          (m68k-emit-move-imm-an buf (conv-real #x10006080) +68k-a0+)
          (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a0+ 4)
          (m68k-emit-move-an-ind-dn buf +68k-a0+ +68k-d1+)
          (m68k-emit-cmp-dn-dn buf +68k-d1+ +68k-d0+)             ; kept - peak
          (m68k-emit-bcc-word buf +68k-cc-ls+ no)
          (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a0+)
          (here no)))
      ;; ---- swap the semispaces, count the collection ----
      (m68k-emit-move-imm-an buf meta +68k-a1+)
      (m68k-emit-move-disp-dn buf +68k-a1+ #x08 +68k-d0+)
      (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ #x00)        ; from = old to
      (m68k-emit-move-dn-disp buf +68k-d4+ +68k-a1+ #x08)        ; to = old from
      (m68k-emit-move-disp-dn buf +68k-a1+ #x20 +68k-d0+)
      (m68k-emit-addq buf +68k-d0+ 1)
      (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a1+ #x20)
      (when *68k-gc-static*
        (m68k-emit-gc-static-freeze buf))
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
        (m68k-emit-andi buf +68k-d1+ (m68k-tag-mask))            ; tag
        (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
        (m68k-emit-beq buf isptr)
        (m68k-emit-cmpi buf +68k-d1+ +tag-object+)
        (m68k-emit-bne buf ret)
        (here isptr)
        (m68k-emit-move-dn-dn buf +68k-d0+ +68k-d2+)
        (m68k-emit-andi buf +68k-d2+ (logand (lognot (m68k-tag-mask)) #xFFFFFFFF)) ; address
        (m68k-emit-cmp-dn-dn buf +68k-d4+ +68k-d2+)               ; D2 - from_start
        (m68k-emit-bcc-word buf +68k-cc-cs+ ret)
        (m68k-emit-cmp-dn-dn buf +68k-d5+ +68k-d2+)               ; D2 - from_end
        (m68k-emit-bcc-word buf +68k-cc-cc+ ret)
        ;; a real start of the right kind, or not a root at all
        (m68k-emit-kind-addr buf +68k-d3+ +68k-d2+)
        (m68k-emit-move-dn-an buf +68k-d3+ +68k-a1+)
        (m68k-emit-moveq buf +68k-d3+ 0)
        (m68k-emit-move-byte-an-ind-dn buf +68k-a1+ +68k-d3+)
        (if *68k-tags3*
            ;; 3-BIT TAGS: forwarding is in the kind map, not in the word --
            ;; 3 = forwarded object, 4 = forwarded cons -- and the old first
            ;; word holds the new TAGGED pointer.  (Every 3-bit pointer tag is
            ;; a live type; there is no spare one to mark a forward with.)
            (let ((fwd (lbl)))
              (m68k-emit-cmpi buf +68k-d1+ +tag-cons+)
              (m68k-emit-bne buf kobj)
              (m68k-emit-cmpi buf +68k-d3+ 4)
              (m68k-emit-beq buf fwd)
              (m68k-emit-cmpi buf +68k-d3+ 2)
              (m68k-emit-bne buf ret)
              (m68k-emit-bra buf kok)
              (here kobj)
              (m68k-emit-cmpi buf +68k-d3+ 3)
              (m68k-emit-beq buf fwd)
              (m68k-emit-cmpi buf +68k-d3+ 1)
              (m68k-emit-bne buf ret)
              (m68k-emit-bra buf kok)
              (here fwd)
              (m68k-emit-move-dn-an buf +68k-d2+ +68k-a1+)
              (m68k-emit-move-an-ind-dn buf +68k-a1+ +68k-d3+)    ; new, tagged
              (m68k-emit-move-dn-an-ind buf +68k-d3+ +68k-a0+)
              (m68k-emit-rts buf)
              (here kok)
              (m68k-emit-move-dn-an buf +68k-d2+ +68k-a1+)        ; A1 = old base
              (m68k-emit-move-an-ind-dn buf +68k-a1+ +68k-d3+))   ; first word
            (progn
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
        (m68k-emit-rts buf)))
        (here notfwd)
        (m68k-emit-cmpi buf +68k-d1+ 1)
        (m68k-emit-bne buf obj)
        ;; -- cons: a granule down from A3 --
        (m68k-emit-lea-disp buf +68k-a3+ (- (m68k-granule)) +68k-a3+)
        (m68k-emit-cmpa-an buf +68k-a2+ +68k-a3+)                 ; A3 - A2
        (m68k-emit-bcc-word buf +68k-cc-cs+ ovf)
        (m68k-emit-move-dn-disp buf +68k-d3+ +68k-a3+ 0)          ; car
        (m68k-emit-move-disp-dn buf +68k-a1+ 4 +68k-d0+)
        (m68k-emit-move-dn-disp buf +68k-d0+ +68k-a3+ 4)          ; cdr
        (m68k-emit-move-an-dn buf +68k-a3+ +68k-d0+)
        (if *68k-tags3*
            (progn                                                ; forward:
              (m68k-emit-ori buf +68k-d0+ +tag-cons+)             ; the new pointer,
              (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a1+)
              (m68k-emit-kind-addr buf +68k-d0+ +68k-d2+)         ; and kind 4
              (m68k-emit-move-dn-an buf +68k-d0+ +68k-a1+)
              (m68k-emit-moveq buf +68k-d0+ 4)
              (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a1+))
            (progn
              (m68k-emit-ori buf +68k-d0+ 15)
              (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a1+)))  ; forward
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
        (if *68k-tags3*
            (progn                                                ; forward:
              (m68k-emit-ori buf +68k-d0+ +tag-object+)           ; the new pointer,
              (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a1+)
              (m68k-emit-kind-addr buf +68k-d0+ +68k-d2+)         ; and kind 3
              (m68k-emit-move-dn-an buf +68k-d0+ +68k-a1+)
              (m68k-emit-moveq buf +68k-d0+ 3)
              (m68k-emit-move-byte-dn-an-ind buf +68k-d0+ +68k-a1+))
            (progn
              (m68k-emit-ori buf +68k-d0+ 15)
              (m68k-emit-move-dn-an-ind buf +68k-d0+ +68k-a1+)))  ; forward
        (m68k-emit-move-dn-dn buf +68k-d3+ +68k-d0+)
        (m68k-emit-ori buf +68k-d0+ +tag-object+)
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
  (if *68k-relax-branches*
      (let ((plan nil))
        ;; Pass 1: every branch long; where each one went is recorded.
        (let ((*68k-branch-plan* nil)
              (*68k-branch-ordinal* 0)
              (*68k-branch-distances* (make-hash-table))
              (*68k-fn-entry-positions* nil))
          (setf *68k-li-const-patches* nil *68k-unimpl-opcodes* nil)
          (translate-mvm-to-68k-1 bytecode function-table)
          (setq plan (m68k-relax-plan *68k-branch-ordinal*))
          (format t "~&;; 68k branch relaxation: ~D branches, ~D short, ~D word, ~D long~%"
                  *68k-branch-ordinal* (count :byte plan) (count :word plan)
                  (- (count :long plan) 1)))
        ;; Pass 2: the real translation, each branch as short as it can be.
        (let ((*68k-branch-plan* plan)
              (*68k-branch-ordinal* 0))
          (setf *68k-li-const-patches* nil *68k-unimpl-opcodes* nil)
          (translate-mvm-to-68k-1 bytecode function-table)))
      (progn
        (setf *68k-li-const-patches* nil
              *68k-unimpl-opcodes* nil)
        (translate-mvm-to-68k-1 bytecode function-table))))

(defun translate-mvm-to-68k-1 (bytecode function-table)
  "Translate MVM bytecode to Motorola 68000 native code.
   BYTECODE is a (vector (unsigned-byte 8)).
   FUNCTION-TABLE maps function indices to bytecode offsets.
   Returns a byte vector of 68k big-endian machine code."
  (setf *68k-abs-patches* nil)
  (setf *68k-paged-layout* nil *68k-paged-table-refs* nil)
  (let* ((*68k-native-p* t)
         (*68k-paged-fns* nil)
         (*68k-cold-enter-label* (and *68k-paged* (mvm-make-label)))
         (*68k-stream-table-label* (and *68k-paged* (mvm-make-label)))
         (*68k-stream-section-label* (and *68k-paged* (mvm-make-label)))
         (*68k-stream-home-label* (and *68k-paged* (mvm-make-label)))
         (*68k-stream-end-label* (and *68k-paged* (mvm-make-label)))
         (*68k-mul32-label* (and *68k-cpu-68000* (mvm-make-label)))
         (*68k-mulu64-label* (and *68k-cpu-68000* (mvm-make-label)))
         (*68k-divs32-label* (and *68k-cpu-68000* (mvm-make-label)))
         (*68k-gc-label* (and (m68k-hosted-layout-p) (mvm-make-label)))
         (*68k-mac-console-label* (and *68k-mac-mode* (mvm-make-label)))
         (*68k-mac-getc-label* (and *68k-mac-mode* (mvm-make-label)))
         (*68k-cons-label* (and *68k-shared-sequences* (mvm-make-label)))
         (*68k-subtag-label* (and *68k-shared-sequences* (mvm-make-label)))
         (*68k-car-label* (and *68k-shared-sequences* (mvm-make-label)))
         (*68k-barrier-label* (and *68k-gc-static* (m68k-hosted-layout-p) (mvm-make-label)))
         (*68k-cdr-label* (and *68k-shared-sequences* (mvm-make-label)))
         (*68k-array-len-label* (and *68k-shared-sequences* (mvm-make-label)))
         (*68k-alloc-obj-label* (and *68k-shared-sequences* (mvm-make-label)))
         (*68k-bytecode* bytecode)
         (buf (make-m68k-buffer))
         (label-map (make-hash-table :test 'eql))
         (*68k-label-map* label-map)
         (*68k-cold-fns* nil)
         (*68k-thread-shapes* nil)
         (*68k-thread-next-label* (and *68k-threaded* (mvm-make-label)))
         (*68k-stream-fixups* nil)
         (loop-heads (make-hash-table :test 'eql))
         (prof-index -1)
         (fn-entries nil)
         (bc bytecode)
         (len (length bc))
         (pc 0)
         (op-stats (and (sb-ext:posix-getenv "MODUS_68K_OP_STATS")
                        (make-hash-table :test 'eql)))
         (thread-stats (and (sb-ext:posix-getenv "MODUS_68K_THREAD_STATS")
                            (list (make-hash-table :test 'equal) (make-hash-table :test 'equal)
                                  (vector 0 0 0) (make-hash-table :test 'equal))))
         (li-stats (make-hash-table :test 'equal))
         (li-imms (make-hash-table :test 'eql)))

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

    ;; MODUS_68K_PROFILE: loop heads are the targets of backward branches.
    (when *68k-profile*
      (setf pc 0)
      (loop while (< pc len)
            do (destructuring-bind (op operands . npc) (decode-instruction bc pc)
                 (let* ((info (gethash op *opcode-table*))
                        (types (and info (opcode-info-operands info)))
                        (k (position :off32 types)))
                   (when k
                     (let ((target (+ npc (nth k operands))))
                       (when (< target pc) (setf (gethash target loop-heads) t)))))
                 (setf pc npc))))

    ;; Register function entry points as branch targets
    (when function-table
      (maphash (lambda (idx mvm-offset)
                 (declare (ignore idx))
                 (unless (gethash mvm-offset label-map)
                   (setf (gethash mvm-offset label-map) (mvm-make-label))))
               function-table))

    ;; Threaded cold code: choose the cold functions, number the shapes.
    (when (and *68k-threaded* function-table)
      (m68k-thread-plan function-table))

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
                 (m68k-word-align buf)
                 (loop while (/= 0 (mod (m68k-buffer-position buf) 16))
                       do (m68k-emit-nop buf))
                 (when *68k-branch-distances*
                   (push (m68k-buffer-position buf) *68k-fn-entry-positions*)))
               ;; Emit label at current PC before translating
               (let ((label (gethash pc label-map)))
                 (when label
                   (m68k-emit-label buf label)))
               (when (and *68k-profile* function-table
                          (or (gethash pc fn-entries) (gethash pc loop-heads)))
                 (when (gethash pc fn-entries) (incf prof-index))
                 (m68k-emit-word buf #x52B9)            ; ADDQ.L #1,(counter).L
                 (m68k-emit-long buf (+ +68k-profile-base+ (* 4 (max 0 prof-index)))))
               (if (and *68k-cold-fns* (gethash pc *68k-cold-fns*))
                 (setf pc (m68k-emit-cold-function buf pc))
               (let* ((decoded (decode-instruction bc pc))
                      (opcode (car decoded))
                      (operands (cadr decoded))
                      (new-pc (cddr decoded))
                      (at (m68k-buffer-position buf)))
                 (m68k-translate-insn buf opcode operands new-pc label-map function-table)
                 (when thread-stats
                   (m68k-thread-stats-note thread-stats opcode operands
                                           (- (m68k-buffer-position buf) at)))
                 (when op-stats
                   (let ((e (or (gethash opcode op-stats)
                                (setf (gethash opcode op-stats) (list 0 0 0)))))
                     (incf (first e)) (incf (second e) (- (m68k-buffer-position buf) at))
                     (incf (third e) (- new-pc pc)))
                   (when (eql opcode +op-li+)
                     (let* ((pd (m68k-vreg-phys (first operands)))
                            (imm (logand (second operands) #xFFFFFFFF))
                            (k (list (if pd (m68k-reg-type pd) :spill)
                                     (cond ((= imm +nil-value+) :nil)
                                           ((or (< imm 128) (>= imm #xFFFFFF80)) :byte)
                                           ((or (< imm #x8000) (>= imm #xFFFF8000)) :word)
                                           (t :long))))
                            (e (or (gethash k li-stats) (setf (gethash k li-stats) (list 0 0)))))
                       (incf (first e)) (incf (second e) (- (m68k-buffer-position buf) at))
                       (incf (gethash imm li-imms 0)))))
                 (when *68k-skip-to*
                   (setq new-pc *68k-skip-to* *68k-skip-to* nil))
                 (setf pc new-pc)))))

    (when thread-stats (m68k-thread-stats-report thread-stats))
    ;; MODUS_68K_OP_STATS: native bytes per MVM opcode, the largest first
    (when op-stats
      (let ((rows nil) (total 0))
        (maphash (lambda (op e)
                   (incf total (second e))
                   (push (list op (first e) (second e) (third e)) rows))
                 op-stats)
        (setq rows (sort rows #'> :key #'third))
        (format t "~&;; 68k op stats: ~D native bytes from ~D MVM bytecode bytes~%;;   ~10A ~8@A ~10@A ~6@A ~8@A~%"
                total (let ((bc 0)) (maphash (lambda (op e) (declare (ignore op)) (incf bc (third e))) op-stats) bc)
                "opcode" "count" "bytes" "%" "B/insn")
        (dolist (r (subseq rows 0 (min 30 (length rows))))
          (let* ((info (gethash (first r) *opcode-table*))
                 (nm (if info (opcode-info-name info) (first r))))
            (format t ";;   ~10A ~8D ~10D ~5,1F% ~8,1F~%" nm (second r) (third r)
                    (/ (* 100.0 (third r)) total) (/ (third r) (max 1 (second r))))))
        ;; LI by destination (data / address register, spilled) and immediate
        ;; (NIL, signed byte, signed word, long), then the commonest immediates.
        (let ((lr nil) (ir nil))
          (maphash (lambda (k e) (push (cons k e) lr)) li-stats)
          (dolist (r (sort lr #'> :key #'third))
            (format t ";;   LI ~8A ~6A ~8D ~10D~%" (first (car r)) (second (car r))
                    (second r) (third r)))
          (maphash (lambda (k n) (push (cons k n) ir)) li-imms)
          (setq ir (sort ir #'> :key #'cdr))
          (format t ";;   LI imms:~{ ~X:~D~}~%"
                  (loop for (k . n) in (subseq ir 0 (min 24 (length ir))) append (list k n))))))

    (when *68k-gc-label*
      (m68k-emit-gc-collector buf))
    (when *68k-mac-console-label*
      (m68k-emit-mac-console buf))
    (when *68k-cons-label*
      (m68k-emit-cons-helper buf))
    (when *68k-subtag-label*
      (m68k-emit-subtag-helper buf))
    (when *68k-car-label*
      (m68k-emit-car-cdr-helpers buf))
    (when *68k-barrier-label*
      (m68k-emit-barrier-helper buf))
    (when *68k-cold-fns*
      (m68k-emit-thread-dispatch buf function-table))
    (when *68k-array-len-label*
      (m68k-emit-array-len-helper buf))
    (when *68k-alloc-obj-label*
      (m68k-emit-alloc-obj-helper buf))
    (when *68k-mul32-label*
      (m68k-emit-68000-helpers buf))
    ;; paged: the stream section LAST, so it is the native code's tail
    (when (and *68k-paged* *68k-paged-fns*)
      (m68k-emit-paged-section buf))

    ;; Resolve labels
    (m68k-fixup-labels buf)
    (when *68k-cold-fns*
      (m68k-resolve-stream-fixups buf))
    (when (and *68k-paged* *68k-paged-fns*)
      (m68k-paged-finish buf))
    ;; where the shared native regions start, for a profile (MODUS_68K_REGIONS=FILE:
    ;; name and native offset a line; the functions come before all of them)
    (let ((f (sb-ext:posix-getenv "MODUS_68K_REGIONS")))
      (when (and f (plusp (length f)))
        (with-open-file (o f :direction :output :if-exists :supersede)
          (loop for (name label) in
                (list (list "gc-collector" *68k-gc-label*) (list "mac-console" *68k-mac-console-label*)
                      (list "cons-helper" *68k-cons-label*) (list "subtag-helper" *68k-subtag-label*)
                      (list "car-cdr-helpers" *68k-car-label*) (list "barrier" *68k-barrier-label*)
                      (list "thread-dispatch" *68k-thread-next-label*)
                      (list "array-len-helper" *68k-array-len-label*)
                      (list "alloc-obj-helper" *68k-alloc-obj-label*)
                      (list "68000-mul-div" *68k-mul32-label*)
                      (list "paged-loader" *68k-cold-enter-label*)
                      (list "stream-table" *68k-stream-table-label*)
                      (list "stream-section" *68k-stream-section-label*))
                for pos = (and label (gethash label (m68k-buffer-labels buf)))
                when pos do (format o "~A ~D~%" name pos)))))
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
;;; THREADED COLD CODE (*68K-THREADED*, MODUS_68K_THREADED)
;;; ============================================================
;;;
;;; A cold function is not translated instruction by instruction: it becomes a
;;; native STUB (MOVEA.L A5,A1 / LEA stream(PC),A5 / BRA next) followed by a
;;; byte stream of TOKENS, one per MVM instruction (a compare and the
;;; conditional branch after it are one), each naming a shared native HANDLER:
;;; the code this translator emits for that instruction with its register
;;; operands baked in, then BRA next.  The 255 commonest shapes are one byte;
;;; the rest are 255 and a 16-bit index.  Operands that vary a lot follow the
;;; token: immediates (1/2/4 bytes by value), constant addresses (patched by
;;; cross.lisp as LI-CONST's are), call and function targets (32-bit,
;;; relative to the operand's end), branch offsets (16-bit, 32 in big
;;; functions, relative to the operand's end).  Everything else is baked.
;;;
;;; A5 IS THE TOKEN POINTER, so threaded images do not keep the globals base
;;; there (*68K-A5-GLOBALS* is off): native code never touches A5, so a cold
;;; function's A5 survives every native call it makes.  A cold function saves
;;; its caller's A5 at A6-4 (free: spill slots start at A6-32) in its frame
;;; enter, and puts it back in RET and TAILCALL; SETJMP saves A5 in the
;;; jmpbuf's eighth word and LONGJMP restores it, so a non-local exit lands
;;; with its own frame's token pointer.  Registers, frames and the calling
;;; convention are native: the collector, backtraces and callers see no
;;; difference.
;;;
;;; SETJMP / LONGJMP thread as they are: SETJMP's resume point is a label in its
;;; own handler, which falls through to NEXT, and the A5 it saves points at the
;;; token after it -- so a LONGJMP back resumes the stream there.
;;;
;;; NOT threaded (stays native): a function whose first instruction is not a
;;; frame enter, that contains a context switch, a
;;; conditional branch not directly after the compare that sets its flags
;;; (the dispatch's MOVE.B clobbers them; BVS always), or a call whose target
;;; has no label.

;;; (the threaded tier's specials are declared near the top of this file)

(defun m68k-jmpbuf-words () (if *68k-threaded* 8 +68k-jmpbuf-words+))

(defun m68k-emit-byte (buf b)
  "One byte at the current (possibly odd) position."
  (let ((pos (m68k-buffer-position buf)))
    (if (oddp pos)
        (let ((i (1- (m68k-buffer-word-count buf))))
          (setf (aref (m68k-buffer-words buf) i)
                (logior (aref (m68k-buffer-words buf) i) (logand b #xFF)))
          (incf (m68k-buffer-position buf)))
        (progn (m68k-emit-word buf (ash (logand b #xFF) 8))
               (decf (m68k-buffer-position buf))))))

(defun m68k-set-byte (buf pos b)
  (let ((i (ash pos -1)) (w (m68k-buffer-words buf)))
    (setf (aref w i) (if (evenp pos)
                         (logior (logand (aref w i) #x00FF) (ash (logand b #xFF) 8))
                         (logior (logand (aref w i) #xFF00) (logand b #xFF))))))

(defun m68k-word-align (buf)
  (when (oddp (m68k-buffer-position buf)) (m68k-emit-byte buf 0)))

(defun m68k-flag-op-p (op) (member op (list +op-cmp+ +op-test+ +op-cmp-imm+ +op-test-imm+)))
(defun m68k-cond-br-cc (op)
  (cond ((eql op +op-beq+) +68k-cc-eq+) ((eql op +op-bne+) +68k-cc-ne+)
        ((eql op +op-blt+) +68k-cc-lt+) ((eql op +op-bge+) +68k-cc-ge+)
        ((eql op +op-ble+) +68k-cc-le+) ((eql op +op-bgt+) +68k-cc-gt+)))

(defun m68k-imm-class (x)
  "Bytes a signed 32-bit immediate needs in the stream: 1, 2 or 4."
  (let ((s (%imm32-signed (logand x #xFFFFFFFF))))
    (cond ((<= -128 s 127) 1) ((<= -32768 s 32767) 2) (t 4))))

(defvar *68k-thread-why* nil "The last reason a span could not be threaded.")

(defun m68k-thread-classify (op operands new-pc w)
  "(values KEY PARAMS) for one unfused instruction, or :NATIVE when a function
   holding it cannot be threaded.  PARAMS: (:i SIZE VALUE) (:br TARGET-PC)
   (:rel TARGET-PC) (:lic INDEX)."
  (cond
    ((eql op +op-li+)
     (let ((n (m68k-imm-class (second operands))))
       (values (list :li (first operands) n) (list (list :i n (second operands))))))
    ((eql op +op-li-const+)
     (values (list :li-const (first operands)) (list (list :lic (second operands)))))
    ((or (eql op +op-cmp-imm+) (eql op +op-test-imm+))
     (let ((n (m68k-imm-class (second operands))))
       (values (list op (first operands) n) (list (list :i n (second operands))))))
    ((or (eql op +op-call+) (eql op +op-tailcall+))
     (if (gethash (first operands) *68k-label-map*)
         (values (list op) (list (list :rel (first operands))))
         :native))
    ((eql op +op-fn-addr+)
     (cond ((= (second operands) #xFFFFFFF0) (values (list op (first operands) :nil) nil))
           ((gethash (second operands) *68k-label-map*)
            (values (list op (first operands)) (list (list :rel (second operands)))))
           (t :native)))
    ((eql op +op-br+)
     (values (list :br w) (list (list :br (+ new-pc (first operands))))))
    ((or (eql op +op-bnull+) (eql op +op-bnnull+))
     (values (list op (first operands) w) (list (list :br (+ new-pc (second operands))))))
    ((or (m68k-cond-br-cc op) (eql op +op-bvs+))             ; only fused
     (setq *68k-thread-why* (list :unfused-branch op)) :native)
    ((eql op +op-ret+) (values (list :ret) nil))
    ((eql op +op-trap+)
     (let ((code (first operands)))
       (cond ((>= code #x8000) (values (list :enter code) nil))
             (t (values (list op code) nil)))))
    ((or (eql op +op-save-ctx+) (eql op +op-restore-ctx+))
     (setq *68k-thread-why* (list :ctx op)) :native)
    (t (values (cons op operands) nil))))

(defun m68k-thread-walk (start end w fn)
  "Call FN with (pc key params next-pc) for each token of [START, END), fusing
   a flag setter with the conditional branch after it.  :NATIVE if the span
   cannot be threaded."
  (let ((bc *68k-bytecode*) (pc start))
    (loop while (< pc end)
          do (destructuring-bind (op operands . npc) (decode-instruction bc pc)
               (let ((nxt (and (< npc end) (null (gethash npc *68k-label-map*))
                               (decode-instruction bc npc))))
                 (if (and (m68k-flag-op-p op) nxt (m68k-cond-br-cc (car nxt)))
                     (multiple-value-bind (k p) (m68k-thread-classify op operands npc w)
                       (when (eq k :native) (return-from m68k-thread-walk :native))
                       (let ((bnpc (cddr nxt)))
                         (funcall fn pc (list :fused k (car nxt) w)
                                  (append p (list (list :br (+ bnpc (first (cadr nxt))))))
                                  bnpc)
                         (setq pc bnpc)))
                     (multiple-value-bind (k p) (m68k-thread-classify op operands npc w)
                       (when (eq k :native) (return-from m68k-thread-walk :native))
                       (funcall fn pc k p npc)
                       (setq pc npc))))))
    t))

(defun m68k-thread-plan (function-table)
  "Pre-pass: choose the cold functions and number the shapes, commonest first."
  (let* ((starts (sort (let (l) (maphash (lambda (i off) (declare (ignore i)) (push off l))
                                         function-table)
                         (remove-duplicates l))
                       #'<))
         (len (length *68k-bytecode*))
         (counts (make-hash-table :test 'equal))
         (why (make-hash-table :test 'equal))
         (cold (make-hash-table :test 'eql)))
    (loop for (s . rest) on starts
          for e = (or (first rest) len)
          for w = (if (> (- e s) 20000) 4 2)
          do (unless (or (and (hash-table-p *68k-threaded*) (gethash s *68k-threaded*))
                         (let ((d (decode-instruction *68k-bytecode* s)))
                           (not (and (eql (car d) +op-trap+) (>= (first (cadr d)) #x8000)))))
               (let ((local (make-hash-table :test 'equal)))
                 (setq *68k-thread-why* (list :other))
                 (if (eq :native
                         (m68k-thread-walk s e w
                                           (lambda (pc k p npc) (declare (ignore pc p npc))
                                             (incf (gethash k local 0)))))
                     (let ((r (gethash *68k-thread-why* why)))
                       (unless r (setq r (setf (gethash *68k-thread-why* why) (list 0 0))))
                       (incf (first r)) (incf (second r) (- e s)))
                     (progn
                       (setf (gethash s cold) (cons e w))
                       (maphash (lambda (k n) (incf (gethash k counts 0) n)) local))))))
    (maphash (lambda (k r) (format t "~&;; 68k threaded: native because ~S: ~D functions, ~D MVM bytes~%"
                                   k (first r) (second r)))
             why)
    (let ((rows nil) (shapes (make-hash-table :test 'equal)))
      (maphash (lambda (k n) (push (cons k n) rows)) counts)
      (setq rows (sort rows #'> :key #'cdr))
      ;; index 255 is the escape, so shapes skip it
      (loop for (k . n) in rows for j from 0
            do (setf (gethash k shapes) (vector (if (< j 255) j (1+ j)) (mvm-make-label) n k)))
      (setq *68k-cold-fns* cold *68k-thread-shapes* shapes)
      ;; MODUS_68K_THREAD_STATS: stream bytes, and the commonest adjacent
      ;; token pairs (superinstruction candidates), by bytes they would save.
      (when (sb-ext:posix-getenv "MODUS_68K_THREAD_STATS")
        (let ((tok 0) (opnd 0) (pairs (make-hash-table :test 'equal)))
          (maphash (lambda (st info)
                     (let ((prev nil))
                       (m68k-thread-walk st (car info) (cdr info)
                                         (lambda (pc k p npc) (declare (ignore npc))
                                           (let ((i (svref (gethash k shapes) 0)))
                                             (incf tok (if (< i 255) 1 3)))
                                           (dolist (x p)
                                             (incf opnd (case (first x) (:i (second x)) (:lic 4) (:rel 4)
                                                          (:br (cdr info)))))
                                           (when (and prev (null (gethash pc *68k-label-map*)))
                                             (incf (gethash (cons prev k) pairs 0)))
                                           (setq prev k)))))
                   cold)
          (format t "~&;; 68k thread stats: stream ~D bytes = tokens ~D + operands ~D~%"
                  (+ tok opnd) tok opnd)
          (let ((rows nil))
            (maphash (lambda (k n) (push (cons k n) rows)) pairs)
            (setq rows (sort rows #'> :key #'cdr))
            (loop for (k . n) in rows repeat 40
                  do (format t ";;   pair ~7D  ~S  ~S~%" n (car k) (cdr k))))))
      (format t "~&;; 68k threaded: ~D of ~D functions cold, ~D shapes~%"
              (hash-table-count cold) (length starts) (hash-table-count shapes)))))

(defun m68k-emit-stream-param (buf param w)
  (destructuring-bind (kind a &optional b) param
    (ecase kind
      (:i (let ((v (logand b #xFFFFFFFF)))
            (loop for i from (1- a) downto 0 do (m68k-emit-byte buf (ldb (byte 8 (* 8 i)) v)))))
      (:lic (push (cons (- (m68k-buffer-position buf) 2) a) *68k-li-const-patches*)
       (dotimes (i 4) (m68k-emit-byte buf 0)))
      (:rel
       (let ((pos (m68k-buffer-position buf))
             (label (or (gethash a *68k-label-map*)
                        (error "68k threaded: no label for pc ~D" a))))
         ;; paged: the target entry's ABSOLUTE address (patched after layout)
         (push (list pos label 4 (if (and *68k-paged* (not *68k-paged-control*)) :abs (+ pos 4)))
               *68k-stream-fixups*)
         (dotimes (i 4) (m68k-emit-byte buf 0))))
      (:br
       (let* ((size (if (eq kind :rel) 4 w))
              (pos (m68k-buffer-position buf))
              (label (or (gethash a *68k-label-map*)
                         (error "68k threaded: no label for pc ~D" a))))
         (push (list pos label size (+ pos size)) *68k-stream-fixups*)
         (dotimes (i size) (m68k-emit-byte buf 0)))))))

(defun m68k-emit-cold-function (buf start)
  "Emit cold function START (already aligned, its label placed): the stub,
   then its tokens.  Returns the pc after it."
  (destructuring-bind (end . w) (gethash start *68k-cold-fns*)
    (when *68k-paged*
      (let ((i (length *68k-paged-fns*)))
        (when (> i 32767) (error "68k paged: more than 32767 cold functions"))
        (push (list i start (mvm-make-label)) *68k-paged-fns*)
        (m68k-emit-word buf #x224D)                  ; MOVEA.L A5,A1 (caller's A5)
        (m68k-emit-word buf #x303C)                  ; MOVE.W #i,D0
        (m68k-emit-word buf i)
        (m68k-emit-bra buf *68k-cold-enter-label*)
        (m68k-word-align buf)
        (return-from m68k-emit-cold-function end)))
    (let ((stream (mvm-make-label)))
      (m68k-emit-word buf #x224D)                    ; MOVEA.L A5,A1 (caller's A5)
      (m68k-emit-word buf #x4BFA)                    ; LEA (d16,PC),A5
      (m68k-emit-word buf 0)
      (m68k-emit-fixup buf stream :disp16)
      (m68k-emit-bra buf *68k-thread-next-label*)
      (m68k-emit-label buf stream)
      (m68k-thread-walk start end w
                        (lambda (pc k params npc)
                          (declare (ignore npc))
                          (unless (= pc start)
                            (let ((l (gethash pc *68k-label-map*)))
                              (when l (m68k-emit-label buf l))))
                          (let ((i (svref (gethash k *68k-thread-shapes*) 0)))
                            (if (< i 255)
                                (m68k-emit-byte buf i)
                                (progn (m68k-emit-byte buf 255)
                                       (m68k-emit-byte buf (ash i -8))
                                       (m68k-emit-byte buf (logand i #xFF)))))
                          (dolist (p params) (m68k-emit-stream-param buf p w))))
      (m68k-word-align buf)
      end)))

(defun m68k-emit-stream-body (buf start end w)
  "The tokens of cold function START..END (branch width W), with its labels."
  (m68k-thread-walk start end w
                    (lambda (pc k params npc)
                      (declare (ignore npc))
                      (unless (= pc start)
                        (let ((l (gethash pc *68k-label-map*)))
                          (when l (m68k-emit-label buf l))))
                      (let ((i (svref (gethash k *68k-thread-shapes*) 0)))
                        (if (< i 255)
                            (m68k-emit-byte buf i)
                            (progn (m68k-emit-byte buf 255)
                                   (m68k-emit-byte buf (ash i -8))
                                   (m68k-emit-byte buf (logand i #xFF)))))
                      (dolist (p params) (m68k-emit-stream-param buf p w)))))

(defun m68k-emit-paged-section (buf)
  "Paged: COLD-ENTER, the Mac's loader, the stream table and the home table,
   the descriptor the disk builder finds, then every stream (the section)."
  (let ((miss (mvm-make-label)) (ill (mvm-make-label)) (diskbias (mvm-make-label))
        (alloc (mvm-make-label)) (ca5 (mvm-make-label)) (chk (mvm-make-label))
        (ill-drive (mvm-make-label)) (ill-read (mvm-make-label)) (ill-full (mvm-make-label))
        (cacheptr (mvm-make-label)) (lastidx (mvm-make-label)) (pb (mvm-make-label))
        (fns (reverse *68k-paged-fns*))
        (mac (and *68k-mac-mode* *68k-paged-cache*)))
    (m68k-word-align buf)
    (m68k-emit-label buf *68k-cold-enter-label*)
    (m68k-emit-word buf #x48C0)                               ; EXT.L D0 (i < 32768)
    (m68k-emit-word buf #xD080) (m68k-emit-word buf #xD080)   ; D0 *= 4
    (m68k-emit-lea-label buf *68k-stream-table-label* +68k-a5+)
    (m68k-emit-word buf #x0835) (m68k-emit-word buf 0) (m68k-emit-word buf #x0803) ; BTST #0,(3,A5,D0.L)
    (m68k-emit-bne buf miss)                                  ; not loaded
    (m68k-emit-word buf #x2A75) (m68k-emit-word buf #x0800)   ; MOVEA.L (0,A5,D0.L),A5
    (m68k-emit-bra buf *68k-thread-next-label*)
    (m68k-emit-label buf miss)
    (when mac
      (destructuring-bind (lo hi staging staging-size load-addr) *68k-paged-cache*
        (declare (ignore lo hi load-addr staging staging-size))
        (flet ((ws (&rest ws) (dolist (w ws) (m68k-emit-word buf w)))
               (l32 (v) (m68k-emit-long buf v)))
          (ws #x48E7 #x7FFA)                                   ; MOVEM.L D1-D7/A0-A4/A6,-(SP)
          (m68k-emit-lea-label buf ca5 +68k-a0+) (ws #x2089)   ; CA5 = A1: the caller's A5
          (ws #x2E00)                                          ; D7 = i*4
          (m68k-emit-lea-label buf lastidx +68k-a0+) (ws #x2080)   ; lastidx = D0
          (m68k-emit-lea-label buf *68k-stream-home-label* +68k-a0+)
          (ws #x2A30 #x7800)                                   ; D5 = home[i]
          (ws #x2C30 #x7804)                                   ; D6 = home[i+1]
          (ws #x9C85)                                          ; D6 = length
          (ws #x2805)                                          ; D4 = home
          (m68k-emit-lea-label buf diskbias +68k-a0+)
          (ws #x9890)                                          ; SUB.L (A0),D4: disk byte
          (ws #x2604 #x0283) (l32 511)                         ; D3 = skip (D4 mod 512)
          (ws #x9883)                                          ; D4 = first block's byte
          (ws #x2403 #xD486 #x0682) (l32 511)
          (ws #x0282) (l32 #xFFFFFE00)                         ; D2 = bytes to read
          ;; the blocks go straight into the code cache, at the place the FIFO
          ;; allocator finds for D2 bytes (A4); the stream is then slid down
          ;; over the lead-in (D3 bytes)
          (m68k-emit-bsr buf alloc)
          (ws #x4A80) (m68k-emit-bne buf ill-full)             ; TST.L D0: no room at all
          ;; the boot drive's drive-queue element (BootDrive $210, DrvQHdr $30A)
          (let ((dq (mvm-make-label)) (found (mvm-make-label)) (zl (mvm-make-label)))
            (ws #x3238 #x0210 #x2678 #x030A)
            (m68k-emit-label buf dq)
            (ws #x200B) (m68k-emit-beq buf ill-drive)
            (ws #xB26B #x0006) (m68k-emit-beq buf found)
            (ws #x2653) (m68k-emit-bra buf dq)
            (m68k-emit-label buf found)
            (m68k-emit-lea-label buf pb +68k-a0+)
            (ws #x7013)                                        ; MOVEQ #19,D0
            (m68k-emit-label buf zl)
            (ws #x4298)                                        ; CLR.L (A0)+
            (ws #x51C8) (m68k-emit-word buf 0) (m68k-emit-fixup buf zl :disp16) ; DBRA D0,zl
            (m68k-emit-lea-label buf pb +68k-a0+)
            (ws #x316B #x0006 #x0016)                          ; ioVRefNum = dQDrive
            (ws #x316B #x0008 #x0018)                          ; ioRefNum = dQRefNum
            (ws #x214C #x0020)                                 ; ioBuffer = A4
            (ws #x2142 #x0024)                                 ; ioReqCount = D2
            (ws #x317C #x0001 #x002C)                          ; ioPosMode = fsFromStart
            (ws #x2144 #x002E)                                 ; ioPosOffset = D4
            (ws #xA002)                                        ; _Read
            (ws #x4A40) (m68k-emit-bne buf ill-read))
          ;; slide the stream down to A4: A2 = A4 + skip, D6 bytes, upward (dest < src)
          (ws #x244C #xD5C3 #x264C #x2006)
          (ws #x5380 #x6B04 #x16DA #x60F8)
          ;; the owner map: the stream's granules are now index + 1
          (ws #x200C #x90B9) (l32 (conv-real +68k-cache-map+))   ; D0 = A4 - map base
          (ws #xEC88 #xD080)                                   ; first granule's word
          (ws #x220C #xD286 #x92B9) (l32 (conv-real +68k-cache-map+)) ; D1 = end - map base
          (ws #x0681) (l32 63) (ws #xEC89 #xD281)              ; past the last granule's word
          (ws #x2407 #xE48A #x5282)                            ; D2 = i + 1
          (ws #x2079) (l32 (conv-real +68k-cache-map+))        ; A0 = the map
          (let ((ml (mvm-make-label)) (md (mvm-make-label)))
            (m68k-emit-label buf ml)
            (ws #xB280) (m68k-emit-bcc-word buf +68k-cc-ls+ md)
            (ws #x3182 #x0800 #x5480)                          ; MOVE.W D2,(0,A0,D0.L); next
            (m68k-emit-bra buf ml)
            (m68k-emit-label buf md))
          ;; the allocator's next place: A4 + length, up to the granule
          (ws #x2006 #x0680) (l32 63) (ws #x0280) (l32 #xFFFFFFC0) (ws #xD08C)
          (m68k-emit-move-imm-an buf (conv-real +68k-cache-ptr+) +68k-a0+)
          (ws #x2080)
          (m68k-emit-lea-label buf *68k-stream-table-label* +68k-a0+)
          (ws #x218C #x7800)                                   ; table[i] = A4
          ;; MODUS_68K_PAGED_CHECK=1: every root into the cache must be inside a
          ;; loaded stream, or an active one was evicted (!LDR-LOST)
          (when (equal (sb-ext:posix-getenv "MODUS_68K_PAGED_CHECK") "1")
            (m68k-emit-bsr buf chk))
          (ws #x4CDF #x5FFE)                                   ; MOVEM.L (SP)+,D1-D7/A0-A4/A6
          (m68k-emit-lea-label buf lastidx +68k-a5+) (ws #x2015)
          (m68k-emit-lea-label buf *68k-stream-table-label* +68k-a5+)
          (ws #x2A75 #x0800)                                   ; A5 = the stream
          (m68k-emit-bra buf *68k-thread-next-label*)
          ;; a stream that cannot be had: say which way on serial port A, stop
          (loop for (label text) in (list (list ill-drive "!LDR-DRIVE") (list ill-read "!LDR-READ")
                                          (list ill-full "!LDR-FULL"))
                do (m68k-emit-label buf label)
                   (loop for ch across text
                         do (m68k-emit-moveq buf +68k-d0+ (char-code ch))
                            (m68k-emit-bsr buf *68k-mac-console-label*))
                   (m68k-emit-moveq buf +68k-d0+ 10)
                   (m68k-emit-bsr buf *68k-mac-console-label*)
                   (let ((spin (mvm-make-label)))
                     (m68k-emit-label buf spin)
                     (m68k-emit-bra buf spin)))
          (m68k-emit-paged-alloc buf alloc (length fns) ca5)
          (when (equal (sb-ext:posix-getenv "MODUS_68K_PAGED_CHECK") "1")
            (m68k-emit-paged-check buf chk (length fns) ca5)))))
    (m68k-emit-label buf ill)
    (m68k-emit-word buf #x4AFC)                               ; ILLEGAL: not loadable
    ;; data: the entry being loaded, a parameter block
    (loop while (/= 0 (mod (m68k-buffer-position buf) 4)) do (m68k-emit-word buf #x4E71))
    (m68k-emit-label buf cacheptr)
    (m68k-emit-long buf 0)                                    ; (unused: low-block word now)
    (m68k-emit-label buf lastidx)
    (m68k-emit-long buf 0)
    (m68k-emit-label buf ca5)
    (m68k-emit-long buf 0)
    (m68k-emit-label buf pb)
    (dotimes (i 40) (m68k-emit-word buf 0))
    ;; the descriptor: "modusPGD", the table's address, the count, the
    ;; section, and the DISK BIAS: home address - bias = the stream's byte on
    ;; the boot disk (load address - 1024 when the image follows the boot
    ;; blocks; scripts/mac68k-disk.py rewrites it when the streams go elsewhere)
    (dolist (w '(#x6D6F #x6475 #x7350 #x4744)) (m68k-emit-word buf w))
    (m68k-emit-abs32-data buf *68k-stream-table-label*)
    (m68k-emit-long buf (length fns))
    (m68k-emit-abs32-data buf *68k-stream-section-label*)
    (m68k-emit-label buf diskbias)
    (m68k-emit-long buf (if *68k-paged-cache* (- (fifth *68k-paged-cache*) 1024) 0))
    ;; the stream table (live: the stream's address; bit 0 = not loaded) and
    ;; the home table (where each stream is in the image; one more: the end)
    (m68k-emit-label buf *68k-stream-table-label*)
    (dolist (f fns) (m68k-emit-abs32-data buf (third f)))
    (m68k-emit-label buf *68k-stream-home-label*)
    (dolist (f fns) (m68k-emit-abs32-data buf (third f)))
    (m68k-emit-abs32-data buf *68k-stream-end-label*)
    ;; the section
    (loop while (/= 0 (mod (m68k-buffer-position buf) 16)) do (m68k-emit-word buf #x4E71))
    (m68k-emit-label buf *68k-stream-section-label*)
    (dolist (f fns)
      (destructuring-bind (i start label) f
        (declare (ignore i))
        (destructuring-bind (end . w) (gethash start *68k-cold-fns*)
          (m68k-emit-label buf label)
          (m68k-emit-stream-body buf start end w)
          (m68k-word-align buf))))
    (m68k-emit-label buf *68k-stream-end-label*)))

(defun m68k-emit-paged-alloc (buf alloc count ca5)
  "The code cache's FIFO allocator, at ALLOC: D2 = bytes wanted (preserved).
   Returns A4 = a 64-byte-aligned place and D0 = 0, or D0 = 1 when none
   exists.  Preserves D2-D7 and A1-A3, A5, A6.

   A place is taken at the allocation pointer, wrapping to the cache's start.
   It must hold no ACTIVE stream: one an exact root points into (the caller's
   A5 -- CA5, which the loader keeps -- -4 of every frame on the A6 chain, the
   jmpbuf's and every stacked handler frame's A5 at +28; every function of a
   paged image is threaded, so those slots are token pointers).  A root's
   stream is found through the OWNER MAP in one step, not by searching the
   stream table (measured on a Mac Classic: the searches were 70-80% of a
   form's time); it is active when it OVERLAPS the place.  An active stream is
   skipped; every stream that owns a granule of the place is then marked NOT
   LOADED (home | 1) and its granules cleared."
  (let* ((ipno-d1 (mvm-make-label)) (done-l (mvm-make-label)) (top (mvm-make-label)) (wrapped (mvm-make-label)) (roots (mvm-make-label))
        (chain (mvm-make-label)) (cdone (mvm-make-label)) (frames (mvm-make-label))
        (fdone (mvm-make-label)) (evict (mvm-make-label)) (enext (mvm-make-label))
        (ekeep (mvm-make-label)) (eclr (mvm-make-label)) (ok (mvm-make-label))
        (fail (mvm-make-label)) (inplace (mvm-make-label)) (ipno (mvm-make-label))
        (moved (mvm-make-label)) (hb (m68k-hbase)) (gs +68k-cache-granule-shift+))
    (declare (ignore count))
    (flet ((ws (&rest ws) (dolist (w ws) (m68k-emit-word buf w)))
           (l32 (v) (m68k-emit-long buf v)))
      (m68k-emit-label buf alloc)
      (ws #x48E7 #x3F76)                         ; MOVEM.L D2-D7/A1-A3/A5/A6,-(SP)
      (m68k-emit-lea-label buf *68k-stream-table-label* +68k-a3+)
      (m68k-emit-lea-label buf *68k-stream-home-label* +68k-a5+)
      (ws #x2C79) (l32 (conv-real +68k-cache-map+))            ; A6 = the owner map
      (ws #x7C00)                                ; D6 = 0: wraps
      (m68k-emit-move-imm-an buf (conv-real +68k-cache-ptr+) +68k-a0+)
      (ws #x2850)                                ; A4 = the allocation pointer
      ;; ---- try [A4, D7 = A4 + D2 rounded up to the granule) ----
      (m68k-emit-label buf top)
      (ws #x2E02 #x0687) (l32 63) (ws #x0287) (l32 #xFFFFFFC0)  ; D7 = D2 up to 64
      (ws #xDE8C)                                ; D7 += A4
      (ws #xBEB9) (l32 (conv-real +68k-cache-hi+))               ; CMP.L hi,D7
      (m68k-emit-bcc-word buf +68k-cc-ls+ roots)
      (m68k-emit-label buf wrapped)
      (ws #x5286 #x0C86) (l32 2)                 ; ++wraps > 2: nothing fits
      (m68k-emit-bcc-word buf +68k-cc-gt+ fail)
      (ws #x2879) (l32 (conv-real +68k-cache-lo+)) ; A4 = the streams' start
      (m68k-emit-bra buf top)
      ;; ---- the roots ----
      (m68k-emit-label buf roots)
      (m68k-emit-lea-label buf ca5 +68k-a0+)
      (ws #x2010) (m68k-emit-bsr buf inplace) (m68k-emit-bne buf moved)   ; the caller's A5
      ;; the A6 chain starts at the caller's A6, which MOVEM saved last:
      ;; D2-D7 A1-A3 A5 A6 = eleven longwords, A6 at 40(SP)
      (ws #x206F #x0028)                         ; MOVEA.L 40(SP),A0
      (ws #x2479) (l32 (conv-real #x10000058))   ; A2 = the stack base
      (m68k-emit-label buf chain)
      (ws #xB1CA) (m68k-emit-bcc-word buf +68k-cc-cc+ cdone)
      (ws #x2028 #xFFFC) (m68k-emit-bsr buf inplace) (m68k-emit-bne buf moved)
      (ws #x2248 #x2050)                         ; A1 = A0; A0 = (A0)
      (ws #xB1C9) (m68k-emit-bcc-word buf +68k-cc-ls+ cdone)
      (m68k-emit-bra buf chain)
      (m68k-emit-label buf cdone)
      (m68k-emit-move-imm-an buf (+ hb #x180) +68k-a0+)
      (ws #x2028 #x001C) (m68k-emit-bsr buf inplace) (m68k-emit-bne buf moved)
      (ws #x2239) (l32 (+ hb #x400))             ; D1 = depth
      (ws #x0C81) (l32 *68k-hstack-max-depth*)
      (m68k-emit-bcc-word buf +68k-cc-ls+ frames)
      (ws #x7200 #xD2BC) (l32 *68k-hstack-max-depth*)
      (m68k-emit-label buf frames)
      (m68k-emit-move-imm-an buf (+ hb #x408) +68k-a0+)
      (let ((fl (mvm-make-label)))
        (m68k-emit-label buf fl)
        (ws #x4A81) (m68k-emit-beq buf fdone)
        (ws #x2028 #x001C) (m68k-emit-bsr buf inplace) (m68k-emit-bne buf moved)
        (ws #x41E8 #x0020 #x5381)
        (m68k-emit-bra buf fl))
      (m68k-emit-label buf fdone)
      (m68k-emit-bra buf evict)
      (m68k-emit-label buf moved)
      (m68k-emit-bra buf top)
      ;; ---- INPLACE: D0 a root's value.  Its stream, from the owner map, when it
      ;; is loaded, holds D0, and overlaps [A4, D7): A4 = its end rounded up to
      ;; the granule, Z clear.  Else Z set.  Uses D3, D4; keeps D1, A0-A2 ----
      (m68k-emit-label buf inplace)
      (ws #xB0B9) (l32 (conv-real +68k-cache-lo+)) (m68k-emit-bcc-word buf +68k-cc-cs+ ipno)
      (ws #xB0B9) (l32 (conv-real +68k-cache-hi+)) (m68k-emit-bcc-word buf +68k-cc-cc+ ipno)
      (ws #x2600 #x96B9) (l32 (conv-real +68k-cache-map+))       ; D3 = D0 - map base
      (ws #xEC8B)                                ; LSR.L #6,D3: the granule
      (ws #xD683)                                ; D3 *= 2: its word
      (ws #x3636 #x3800)                         ; MOVE.W (0,A6,D3.L),D3: owner + 1
      (ws #x0283) (l32 #xFFFF)
      (m68k-emit-beq buf ipno)                   ; nobody
      (ws #x5383 #xE58B)                         ; D3 = index * 4
      (ws #x2833 #x3800)                         ; D4 = table[i]
      (ws #x0804 #x0000) (m68k-emit-bne buf ipno)                     ; not loaded
      (ws #xB880) (m68k-emit-bcc-word buf +68k-cc-hi+ ipno)           ; starts after D0
      (ws #x2F01)                                ; keep D1
      (ws #x2235 #x3804 #x92B5 #x3800 #xD284)    ; D1 = home[i+1] - home[i] + start: the end
      (ws #xB280) (m68k-emit-bcc-word buf +68k-cc-ls+ ipno-d1)
      (ws #x0681) (l32 63) (ws #x0281) (l32 #xFFFFFFC0)               ; end up to 64
      (ws #xB28C) (m68k-emit-bcc-word buf +68k-cc-ls+ ipno-d1)        ; ends by A4
      (ws #xB887) (m68k-emit-bcc-word buf +68k-cc-cc+ ipno-d1)        ; starts at/after D7
      (ws #x2841 #x221F)                         ; A4 = its end; D1 back
      (ws #x7601 #x4E75)                         ; Z clear; RTS
      (m68k-emit-label buf ipno-d1)
      (ws #x221F)                                ; D1 back
      (m68k-emit-label buf ipno)
      (ws #x7600 #x4E75)                         ; Z set; RTS
      ;; ---- the place holds no active stream: evict the owners of its granules ----
      (m68k-emit-label buf evict)
      (ws #x200C #x90B9) (l32 (conv-real +68k-cache-map+))       ; D0 = A4 - map base
      (ws #xEC88 #xD080)                         ; its first granule's word offset
      (ws #x2207 #x92B9) (l32 (conv-real +68k-cache-map+))       ; D1 = D7 - map base
      (ws #xEC89 #xD281)                         ; the place's end, as a word offset
      (m68k-emit-label buf enext)
      (ws #xB280) (m68k-emit-bcc-word buf +68k-cc-ls+ ok)        ; done
      (ws #x3636 #x0800)                         ; D3 = owner + 1 (MOVE.W (0,A6,D0.L))
      (ws #x0283) (l32 #xFFFF)
      (m68k-emit-beq buf eclr)
      (ws #x5383 #xE58B)                         ; i * 4
      (ws #x2833 #x3800)                         ; D4 = table[i]
      (ws #x0804 #x0000) (m68k-emit-bne buf eclr)                ; not loaded: stale
      ;; does stream i hold THIS granule?  start <= its address < end
      (ws #x2F00)                                ; keep D0
      (ws #xEB88)                                ; LSL.L #5: word offset -> byte offset
      (ws #xD0B9) (l32 (conv-real +68k-cache-map+))              ; + map base: the granule
      (ws #xB880) (m68k-emit-bcc-word buf +68k-cc-hi+ ekeep)     ; starts after it
      (ws #x2275 #x3804 #x93F5 #x3800 #xD3C4)    ; A1 = home[i+1] - home[i] + start: its end
      (ws #xB3C0) (m68k-emit-bcc-word buf +68k-cc-ls+ ekeep)     ; CMPA.L D0,A1: ends by it
      (ws #x2835 #x3800 #x08C4 #x0000 #x2784 #x3800)   ; table[i] = home[i] | 1
      (m68k-emit-label buf ekeep)
      (ws #x201F)                                ; D0 back
      (m68k-emit-label buf eclr)
      (ws #x4276 #x0800)                         ; CLR.W (0,A6,D0.L)
      (ws #x5480)                                ; next granule's word
      (m68k-emit-bra buf enext)
      (m68k-emit-label buf fail)
      (ws #x7001)                                ; D0 = 1
      (m68k-emit-bra buf done-l)
      (m68k-emit-label buf ok)
      (ws #x7000)                                ; D0 = 0
      (m68k-emit-label buf done-l)
      (ws #x4CDF #x6EFC)                         ; MOVEM.L (SP)+,D2-D7/A1-A3/A5/A6
      (ws #x4E75))))

(defun m68k-emit-paged-check (buf chk count ca5)
  "MODUS_68K_PAGED_CHECK: at CHK, walk the allocator's roots; one pointing into
   the cache but into no loaded stream prints !LDR-LOST, the root's kind (C
   caller, F frame chain, J jmpbuf, H handler frame), its value and the root's
   address, and stops.  Uses D0-D7, A0-A5."
  (let ((one (mvm-make-label)) (scan (mvm-make-label)) (found (mvm-make-label))
        (lost (mvm-make-label)) (hex (mvm-make-label)) (hb (m68k-hbase)))
    (flet ((ws (&rest ws) (dolist (w ws) (m68k-emit-word buf w)))
           (l32 (v) (m68k-emit-long buf v))
           (putc () (m68k-emit-bsr buf *68k-mac-console-label*)))
      (m68k-emit-label buf chk)
      (m68k-emit-lea-label buf ca5 +68k-a0+)
      (ws #x7C43) (ws #x2010) (m68k-emit-bsr buf one)            ; 'C'
      (ws #x7C46 #x204E)                                         ; 'F', A0 = A6
      (ws #x2479) (l32 (conv-real #x10000058))
      (let ((cl (mvm-make-label)) (cd (mvm-make-label)))
        (m68k-emit-label buf cl)
        (ws #xB1CA) (m68k-emit-bcc-word buf +68k-cc-cc+ cd)
        (ws #x2028 #xFFFC) (m68k-emit-bsr buf one)
        (ws #x2248 #x2050 #xB1C9) (m68k-emit-bcc-word buf +68k-cc-ls+ cd)
        (m68k-emit-bra buf cl)
        (m68k-emit-label buf cd))
      (ws #x7C4A) (m68k-emit-move-imm-an buf (+ hb #x180) +68k-a0+)   ; 'J'
      (ws #x2028 #x001C) (m68k-emit-bsr buf one)
      (ws #x7C48 #x2239) (l32 (+ hb #x400))                      ; 'H', D1 = depth
      (m68k-emit-move-imm-an buf (+ hb #x408) +68k-a0+)
      (let ((fl (mvm-make-label)) (fd (mvm-make-label)))
        (m68k-emit-label buf fl)
        (ws #x4A81) (m68k-emit-beq buf fd)
        (ws #x0C81) (l32 *68k-hstack-max-depth*) (m68k-emit-bcc-word buf +68k-cc-hi+ fd)
        (ws #x2028 #x001C) (m68k-emit-bsr buf one)
        (ws #x41E8 #x0020 #x5381)
        (m68k-emit-bra buf fl)
        (m68k-emit-label buf fd))
      (ws #x4E75)
      ;; ONE: D0 = a root's value, A0 = where (A0-4 for the chain), D6 = kind
      (m68k-emit-label buf one)
      (ws #xB0B9) (l32 (conv-real +68k-cache-lo+)) (m68k-emit-bcc-word buf +68k-cc-cs+ found)
      (ws #xB0B9) (l32 (conv-real +68k-cache-hi+)) (m68k-emit-bcc-word buf +68k-cc-cc+ found)
      (m68k-emit-lea-label buf *68k-stream-table-label* +68k-a3+)
      (m68k-emit-lea-label buf *68k-stream-home-label* +68k-a5+)
      (m68k-emit-move-imm-dn buf (1- count) +68k-d5+)
      (m68k-emit-label buf scan)
      (let ((skip (mvm-make-label)))
        (ws #x2613 #x0803 #x0000) (m68k-emit-bne buf skip)
        (ws #xB680) (m68k-emit-bcc-word buf +68k-cc-hi+ skip)
        (ws #x282D #x0004 #x9895 #xD883)
        (ws #xB880) (m68k-emit-bcc-word buf +68k-cc-ls+ skip)
        (m68k-emit-bra buf found)
        (m68k-emit-label buf skip)
        (ws #x588B #x588D)
        (ws #x51CD) (m68k-emit-word buf 0) (m68k-emit-fixup buf scan :disp16))
      (m68k-emit-label buf lost)
      (ws #x2E00 #x2A08)                         ; D7 = value, D5 = where
      (loop for ch across "!LDR-LOST " do (m68k-emit-moveq buf +68k-d0+ (char-code ch)) (putc))
      (ws #x2006) (putc)                         ; the kind
      (m68k-emit-moveq buf +68k-d0+ 32) (putc)
      (ws #x2607) (m68k-emit-bsr buf hex)        ; the value
      (m68k-emit-moveq buf +68k-d0+ 32) (putc)
      (ws #x2605) (m68k-emit-bsr buf hex)        ; where
      (m68k-emit-moveq buf +68k-d0+ 10) (putc)
      (let ((spin (mvm-make-label))) (m68k-emit-label buf spin) (m68k-emit-bra buf spin))
      (m68k-emit-label buf found)
      (ws #x4E75)
      ;; HEX: D3 as eight digits.  Uses D0, D4.
      (m68k-emit-label buf hex)
      (ws #x7807)                                ; D4 = 7
      (let ((hl (mvm-make-label)) (digit (mvm-make-label)))
        (m68k-emit-label buf hl)
        (ws #xE99B)                              ; ROL.L #4,D3
        (ws #x7000 #x1003 #x0200 #x000F)         ; D0 = D3 & 15 (byte)
        (ws #x0C00 #x0009) (m68k-emit-bcc-word buf +68k-cc-ls+ digit)
        (ws #x5E00)                              ; + 7 ('A' - '9' - 1)
        (m68k-emit-label buf digit)
        (ws #x0600 #x0030) (putc)                ; + '0'
        (ws #x51CC) (m68k-emit-word buf 0) (m68k-emit-fixup buf hl :disp16))
      (ws #x4E75))))

(defun m68k-emit-abs32-data (buf label)
  "A longword holding LABEL's absolute address (patched after layout)."
  (m68k-emit-word buf 0) (m68k-emit-word buf 0)
  (push (cons (- (m68k-buffer-position buf) 4) label) *68k-paged-table-refs*))

(defvar *68k-paged-table-refs* nil
  "Paged: (TABLE-SLOT-POS . STREAM-LABEL), resolved into *68K-ABS-PATCHES*.")

(defun m68k-paged-finish (buf)
  "Paged, after every fixup: the table's patches, the layout, and the
   MODUS_68K_PAGED_SHIFT test move."
  (let* ((labels (m68k-buffer-labels buf))
         (sec (gethash *68k-stream-section-label* labels))
         (table (gethash *68k-stream-table-label* labels))
         (shift (let ((v (sb-ext:posix-getenv "MODUS_68K_PAGED_SHIFT")))
                  (if (and v (string/= v "")) (logand (parse-integer v) (lognot 15)) 0))))
    (dolist (r *68k-paged-table-refs*)
      (push (cons (car r) (gethash (cdr r) labels)) *68k-abs-patches*))
    (unless (zerop shift)
      ;; MOVE the section SHIFT bytes on: words in, positions after it moved,
      ;; and ONLY the patches pointing into it follow
      (let* ((w (m68k-buffer-words buf)) (n (m68k-buffer-word-count buf))
             (k (ash sec -1)) (dw (ash shift -1))
             (new (make-array (+ n dw 16) :initial-element 0)))
        (replace new w :end2 k)
        (fill new #x4E71 :start k :end (+ k dw))
        (replace new w :start1 (+ k dw) :start2 k :end2 n)
        (setf (m68k-buffer-words buf) new
              (m68k-buffer-word-count buf) (+ n dw))
        (incf (m68k-buffer-position buf) shift)
        (flet ((mv (x) (if (>= x sec) (+ x shift) x)))
          (setf *68k-abs-patches*
                (mapcar (lambda (p) (cons (mv (car p)) (mv (cdr p)))) *68k-abs-patches*)
                *68k-li-const-patches*
                (mapcar (lambda (p) (cons (mv (car p)) (cdr p))) *68k-li-const-patches*))
          (maphash (lambda (k v) (setf (gethash k labels) (mv v))) labels))
        (format t "~&;; 68k paged: stream section MOVED ~D bytes (the position test)~%" shift)
        (incf sec shift)))
    (setf *68k-paged-layout*
          (list :table table :section sec :end (m68k-buffer-position buf)
                :count (length *68k-paged-fns*)))
    (format t "~&;; 68k paged: ~D cold functions, stream table at ~D, section ~D..~D (~D bytes)~%"
            (length *68k-paged-fns*) table sec (m68k-buffer-position buf)
            (- (m68k-buffer-position buf) sec))))

(defun m68k-resolve-stream-fixups (buf)
  (dolist (f *68k-stream-fixups*)
    (destructuring-bind (pos label size base) f
      (let* ((target (or (gethash label (m68k-buffer-labels buf))
                         (error "68k threaded: undefined label ~D" label)))
             (v (if (eq base :abs) 0 (- target base))))
        (when (eq base :abs)
          (push (cons pos target) *68k-abs-patches*))
        (when (and (= size 2) (not (<= -32768 v 32767)))
          (error "68k threaded: branch of ~D bytes in a 16-bit stream operand" v))
        (dotimes (i size)
          (m68k-set-byte buf (+ pos i) (ldb (byte 8 (* 8 (- size 1 i))) v)))))))

;;; --- handlers ---

(defun m68k-emit-fetch (buf size dn)
  "Dn = the next SIZE-byte signed stream operand, sign-extended; A5 advances."
  (when *68k-cpu-68000*
    ;; byte at a time: a stream operand may be at an odd address
    (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn 3 5))       ; MOVE.B (A5)+,Dn
    (ecase size
      (1 (m68k-emit-word buf (logior #x4880 dn)))                            ; EXT.W Dn
      (2 (m68k-emit-word buf (logior #xE148 dn))                             ; LSL.W #8,Dn
         (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn 3 5)))
      (4 (m68k-emit-word buf (logior #xE148 dn))
         (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn 3 5))
         (m68k-emit-word buf (logior #x4840 dn))                             ; SWAP Dn
         (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn 3 5))
         (m68k-emit-word buf (logior #xE148 dn))
         (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn 3 5))))
    (unless (= size 4) (m68k-emit-word buf (logior #x48C0 dn)))              ; EXT.L Dn
    (return-from m68k-emit-fetch nil))
  (ecase size
    (1 (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ dn 3 5))   ; MOVE.B (A5)+,Dn
       (m68k-emit-word buf (logior #x49C0 dn)))                               ; EXTB.L Dn
    (2 (m68k-emit-word buf (m68k-encode-move-word 3 +ea-mode-dn+ dn 3 5))   ; MOVE.W (A5)+,Dn
       (m68k-emit-word buf (logior #x48C0 dn)))                               ; EXT.L Dn
    (4 (m68k-emit-word buf (m68k-encode-move-word 2 +ea-mode-dn+ dn 3 5))))) ; MOVE.L (A5)+,Dn

(defun m68k-emit-fetch-target (buf)
  "A0 = the address a 32-bit relative stream operand names (target - its end)."
  (m68k-emit-fetch buf 4 +68k-d0+)
  (when (and *68k-paged* (not *68k-paged-control*))
    (m68k-emit-word buf #x2040)                   ; MOVEA.L D0,A0: an absolute address
    (return-from m68k-emit-fetch-target nil))
  (m68k-emit-word buf #x41F5)                     ; LEA (0,A5,D0.L),A0
  (m68k-emit-word buf #x0800))

(defun m68k-emit-stream-branch (buf cc w)
  "Take the stream branch whose offset follows when CC holds (CC NIL: always).
   MOVEA and ADDA leave the condition codes alone."
  (when *68k-cpu-68000*
    ;; a byte-wise read sets the condition codes, so test first: B!cc.S over
    ;; the taken arm to ADDQ #W,A5; the taken arm builds the offset on the stack
    (let ((taken (if (= w 2)
                     (list (m68k-encode-move-word 1 4 7 3 5)                 ; MOVE.B (A5)+,-(SP)
                           (m68k-encode-move-word 1 5 7 3 5) 1               ; MOVE.B (A5)+,1(SP)
                           #x305F)                                           ; MOVEA.W (SP)+,A0
                     (list #x598F                                            ; SUBQ.L #4,SP
                           (m68k-encode-move-word 1 2 7 3 5)                 ; MOVE.B (A5)+,(SP)
                           (m68k-encode-move-word 1 5 7 3 5) 1
                           (m68k-encode-move-word 1 5 7 3 5) 2
                           (m68k-encode-move-word 1 5 7 3 5) 3
                           #x205F))))                                        ; MOVEA.L (SP)+,A0
      (setq taken (append taken (list #xDBC8)))                              ; ADDA.L A0,A5
      (when cc
        (m68k-emit-word buf (logior #x6000 (ash (logxor cc 1) 8)
                                    (+ (* 2 (length taken)) 2))))            ; B!cc.S skip
      (dolist (x taken) (m68k-emit-word buf x))
      (when cc
        (m68k-emit-word buf #x6002)                                          ; BRA.S over
        (m68k-emit-word buf (if (= w 2) #x548D #x588D))))                    ; skip: ADDQ.L #W,A5
    (return-from m68k-emit-stream-branch nil))
  (m68k-emit-word buf (if (= w 2) #x305D #x205D)) ; MOVEA.W / MOVEA.L (A5)+,A0
  (when cc
    (m68k-emit-word buf (logior #x6002 (ash (logxor cc 1) 8))))  ; B!cc.S over the ADDA
  (m68k-emit-word buf #xDBC8))                    ; ADDA.L A0,A5

(defun m68k-emit-thread-handler (buf key function-table)
  "The native body of shape KEY (without the trailing BRA next); T when it
   ends in a return or a jump of its own."
  (let ((*68k-bytecode* nil) (lm (make-hash-table :test 'eql)))
    (flet ((native (op operands)
             (m68k-translate-insn buf op operands 0 lm function-table)
             (when (plusp (hash-table-count lm))
               (error "68k threaded: shape ~S referenced an MVM label" key))))
      (case (first key)
        (:li (m68k-emit-fetch buf (third key) +68k-d0+)
         (m68k-store-vreg buf (second key) +68k-d0+) nil)
        (:li-const (m68k-emit-fetch buf 4 +68k-d0+)
         (m68k-store-vreg buf (second key) +68k-d0+) nil)
        (:br (m68k-emit-stream-branch buf nil (second key)) nil)
        (:ret (m68k-emit-move-disp-an buf +68k-a6+ -4 +68k-a5+)
         (m68k-emit-epilogue buf) t)
        (:enter (native +op-trap+ (list (second key)))
         (m68k-emit-move-an-disp buf +68k-a1+ +68k-a6+ -4) nil)
        (:fused
         (destructuring-bind (fk brop w) (rest key)
           (m68k-emit-thread-flag buf fk function-table #'native)
           (m68k-emit-stream-branch buf (m68k-cond-br-cc brop) w))
         nil)
        (t
         (let ((op (first key)))
           (cond
             ((or (eql op +op-cmp-imm+) (eql op +op-test-imm+))
              (m68k-emit-thread-flag buf key function-table #'native) nil)
             ((eql op +op-call+)
              (m68k-emit-fetch-target buf)
              (m68k-emit-jsr-an-ind buf +68k-a0+) nil)
             ((eql op +op-tailcall+)
              (m68k-emit-fetch-target buf)
              (m68k-emit-move-disp-an buf +68k-a6+ -4 +68k-a5+)
              (m68k-emit-movem-from-postinc buf (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)))
              (m68k-emit-unlk buf +68k-a6+)
              (m68k-emit-jmp-an-ind buf +68k-a0+) t)
             ((eql op +op-fn-addr+)
              (if (eq (third key) :nil)
                  (native op (list (second key) #xFFFFFFF0))
                  (progn (m68k-emit-fetch-target buf)
                         (m68k-emit-move-an-dn buf +68k-a0+ +68k-d0+)
                         (m68k-emit-ori buf +68k-d0+ 3)
                         (m68k-store-vreg buf (second key) +68k-d0+)))
              nil)
             ((or (eql op +op-bnull+) (eql op +op-bnnull+))
              (let ((ds (m68k-vreg-or-scratch buf (second key) +68k-d0+)))
                (m68k-emit-cmpa buf +68k-a4+ ds))
              (m68k-emit-stream-branch buf (if (eql op +op-bnull+) +68k-cc-eq+ +68k-cc-ne+)
                                       (third key))
              nil)
             (t (native op (rest key)) nil))))))))

(defun m68k-emit-thread-flag (buf fk function-table native)
  "A flag setter's body: CMP / TEST natively, CMP-IMM / TEST-IMM with the
   immediate from the stream (the native forms' exact instructions)."
  (declare (ignore function-table))
  (let ((op (first fk)))
    (cond ((eql op +op-cmp-imm+)
           (let ((da (m68k-vreg-or-scratch buf (second fk) +68k-d0+)))
             (m68k-emit-fetch buf (third fk) +68k-d1+)
             (m68k-emit-cmp-dn-dn buf +68k-d1+ da)))
          ((eql op +op-test-imm+)
           (m68k-load-vreg buf +68k-d0+ (second fk))
           (m68k-emit-fetch buf (third fk) +68k-d1+)
           (m68k-emit-and-dn-dn buf +68k-d1+ +68k-d0+)
           (m68k-emit-tst buf +68k-d0+))
          (t (funcall native op (rest fk))))))

(defun m68k-emit-thread-dispatch (buf function-table)
  "The escape, NEXT, the table (32-bit handler offsets from its own start),
   then every handler."
  (let* ((shapes (let (l) (maphash (lambda (k v) (declare (ignore k)) (push v l))
                                   *68k-thread-shapes*)
                   (sort l #'< :key (lambda (v) (svref v 0)))))
         (n (1+ (reduce #'max shapes :key (lambda (v) (svref v 0)) :initial-value 255)))
         (by-index (let ((a (make-array n :initial-element nil)))
                     (dolist (v shapes a) (setf (aref a (svref v 0)) v))))
         (tpos 0)
         (table (mvm-make-label))
         (escape (mvm-make-label)))
    (m68k-word-align buf)
    (flet ((d8-to-table (opword extword-base)
             ;; OPWORD, then a brief extension word whose d8 reaches TABLE;
             ;; resolved below once TABLE's position is known.
             (m68k-emit-word buf opword)
             (let ((ext (m68k-buffer-position buf)))
               (m68k-emit-word buf extword-base)
               ext)))
      ;; escape: D0 = the 16-bit index after 255
      (m68k-emit-label buf escape)
      (m68k-emit-moveq buf +68k-d0+ 0)
      (if *68k-cpu-68000*
          (progn
            (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ 0 3 5)) ; MOVE.B (A5)+,D0
            (m68k-emit-word buf #xE148)                                        ; LSL.W #8,D0
            (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ 0 3 5)))
          (m68k-emit-word buf (m68k-encode-move-word 3 +ea-mode-dn+ 0 3 5)))  ; MOVE.W (A5)+,D0
      (when *68k-cpu-68000*
        (m68k-emit-word buf #xD040) (m68k-emit-word buf #xD040))             ; D0 *= 4
      (let ((e1 (d8-to-table (m68k-encode-move-word 2 +ea-mode-dn+ 0 7 3)
                             (if *68k-cpu-68000* #x0000 #x0400)))              ; MOVE.L (d8,PC,D0.W*4),D0
            (e2 (d8-to-table #x4EFB #x0800)))                                       ; JMP (d8,PC,D0.L)
        ;; NEXT
        (m68k-emit-label buf *68k-thread-next-label*)
        (m68k-emit-moveq buf +68k-d0+ 0)
        (m68k-emit-word buf (m68k-encode-move-word 1 +ea-mode-dn+ 0 3 5))   ; MOVE.B (A5)+,D0
        (when *68k-cpu-68000*
          (m68k-emit-word buf #xD040) (m68k-emit-word buf #xD040))           ; D0 *= 4
        (let ((e3 (d8-to-table (m68k-encode-move-word 2 +ea-mode-dn+ 0 7 3)
                               (if *68k-cpu-68000* #x0000 #x0400)))
              (e4 (d8-to-table #x4EFB #x0800)))
          (m68k-emit-label buf table)
          (setq tpos (m68k-buffer-position buf))
          (progn
            (dolist (e (list e1 e2 e3 e4))
              (let ((d8 (- tpos e)))
                (unless (<= 0 d8 127) (error "68k threaded: table out of d8 reach"))
                (let ((i (ash e -1)))
                  (setf (aref (m68k-buffer-words buf) i)
                        (logior (aref (m68k-buffer-words buf) i) d8))))))
          ;; the table: entry I = handler I - table; 255 is the escape
          (dotimes (i n)
            (let ((label (if (aref by-index i) (svref (aref by-index i) 1) escape)))
              (m68k-emit-word buf 0) (m68k-emit-word buf 0)
              (push (list (- (m68k-buffer-word-count buf) 2) label :disp32 (+ tpos 2))
                    (m68k-buffer-fixups buf))))
          ;; shapes at index >= 255 are reached through the escape: their
          ;; table entry is at index I as well (the escape reads I itself).
          )))
    ;; the handlers
    (dolist (v shapes)
      (m68k-emit-label buf (svref v 1))
      (unless (m68k-emit-thread-handler buf (svref v 3) function-table)
        (m68k-emit-bra buf *68k-thread-next-label*)))))

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
(defun m68k-hbase ()           (if (m68k-hosted-layout-p) (conv-real #x10000000) *68k-globals-base*))
(defun m68k-jmpbuf-addr ()     (+ (m68k-hbase) #x180))
(defun m68k-lj-scratch-addr () (+ (m68k-hbase) #x300))
(defun m68k-hcapped-addr ()    (+ (m68k-hbase) #x360))
(defun m68k-hdepth-addr ()     (+ (m68k-hbase) #x400))
(defun m68k-hframes-addr ()    (+ (m68k-hbase) #x408))

(defun m68k-emit-copy-words (buf from-an to-an)
  "Copy the 7 jmpbuf longwords from 0(FROM-AN) to 0(TO-AN) through D1."
  (dotimes (i (m68k-jmpbuf-words))
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
    (dotimes (i (m68k-jmpbuf-words))
      (m68k-emit-move-dn-disp buf +68k-d1+ +68k-a1+ (* 4 i)))
    (m68k-emit-label buf done)))

(defun m68k-set-linux-mode (on)
  "Turn hosted mode on or off, moving the convention slots with it.  One
   function so the two cannot drift apart: an unmapped slot base is a SIGSEGV on
   the first :set-nargs, which precedes EVERY call, so the first function call in
   the image dies rather than something subtle later."
  (setf *68k-linux-mode* (and on t))
  (setf *68k-globals-base* (if on *68k-hosted-globals-base* #x00600000)))

(defun m68k-set-mac-mode (on)
  "Turn the bare-Macintosh mode on or off (*68K-MAC-MODE*): the hosted memory
   map and convention slots, with the Mac's service traps.  Linux mode off."
  (setf *68k-linux-mode* nil)
  (setf *68k-mac-mode* (and on t))
  (setf *68k-globals-base* (if on (conv-real *68k-hosted-globals-base*) #x00600000)))

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
