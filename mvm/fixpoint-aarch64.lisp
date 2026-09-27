;;; ============================================================
;;; AArch64 translator overrides (bare-metal compatible)
;;; ============================================================

;;; Override a64-resolve-fixups: no destructuring-bind or ecase.
;;; Fixup TYPES are the KEYWORDS translate-aarch64's a64-add-fixup records
;;; (:b :bl :bcond :adr); this used to compare them against name-hash
;;; integers, so every fixup counted as a miss (fm507946) and Gen1 shipped with
;;; every branch/BL/B.cond still holding its zero placeholder (#252).
(defun a64-resolve-fixups (buf)
  (let ((code (a64-buffer-code buf))
        (rest-fixups (a64-buffer-fixups buf))
        (fix-b 0) (fix-bl 0) (fix-bcond 0) (fix-adr 0) (fix-skip 0) (fix-miss 0))
    (loop
      (when (null rest-fixups) (return nil))
      (let ((fixup (car rest-fixups)))
        (let ((index (car fixup))
              (label-id (car (cdr fixup)))
              (type (car (cdr (cdr fixup)))))
          (let ((target (gethash label-id (a64-buffer-labels buf))))
            (if target
              (let ((offset (- target index)))
                (let ((word (aref code index)))
                  (cond
                    ;; :b (name-hash 518081061) and :bl (391831169)
                    ;; Both use imm26 encoding
                    ((eq type :b)
                     (setq fix-b (+ fix-b 1))
                     (aset code index
                           (logior (logand word #xFC000000)
                                   (logand offset #x3FFFFFF))))
                    ((eq type :bl)
                     (setq fix-bl (+ fix-bl 1))
                     (aset code index
                           (logior (logand word #xFC000000)
                                   (logand offset #x3FFFFFF))))
                    ;; :bcond (name-hash 248172622495451147)
                    ;; Reconstruct from scratch to avoid bit4 tagging issue
                    ((eq type :bcond)
                     (setq fix-bcond (+ fix-bcond 1))
                     (let ((cond-bits (logand word #xF)))
                       (let ((new-word (logior (ash #b01010100 24)
                                              (logior (ash (logand offset #x7FFFF) 5)
                                                      cond-bits))))
                         ;; Check for bad bit4: write to scratch and check
                         (setf (mem-ref #x3000078 :u64) new-word)
                         (let ((check-b0 (mem-ref #x3000078 :u8)))
                           (when (not (zerop (logand check-b0 #x20)))
                             (when (<= fix-bcond 5)
                               (write-char-serial 33) ;; !
                               (write-char-serial 105) ;; i
                               (print-dec index) (write-char-serial 32)
                               (write-char-serial 111) ;; o
                               (print-dec offset) (write-char-serial 32)
                               (write-char-serial 99) ;; c
                               (print-dec cond-bits) (write-char-serial 32)
                               (write-char-serial 119) ;; w
                               (print-dec new-word) (write-char-serial 10))))
                         (aset code index new-word))))
                    ;; :adr (name-hash 253724600)
                    ((eq type :adr)
                     (setq fix-adr (+ fix-adr 1))
                     (let ((byte-off (* offset 4)))
                       (let ((immlo (logand byte-off 3)))
                         (let ((immhi (logand (ash byte-off -2) #x7FFFF)))
                           (let ((rd (logand word 31)))
                             ;; Use nested 2-arg logior (bare-metal 4-arg logior is broken)
                             (let ((hi (logior (ash immlo 29) (ash 16 24))))
                               (let ((lo (logior (ash immhi 5) rd)))
                                 (aset code index
                                       (logior hi lo)))))))))
                    (t (setq fix-miss (+ fix-miss 1))
                       ;; name the first few unknown fixup types instead of silently skipping
                       (when (<= fix-miss 3)
                         (write-char-serial 63) ;; ?
                         (if (symbolp type) (write-string-serial (symbol-name type)) (print-dec (if (fixnump type) type -1)))
                         (write-char-serial 10))))))
              (setq fix-skip (+ fix-skip 1))))))
      (setq rest-fixups (cdr rest-fixups)))
    ;; Print fixup stats
    (write-char-serial 102) ;; f
    (write-char-serial 98)  ;; b
    (print-dec fix-b) (write-char-serial 10)
    (write-char-serial 102) ;; f
    (write-char-serial 108) ;; l
    (print-dec fix-bl) (write-char-serial 10)
    (write-char-serial 102) ;; f
    (write-char-serial 99)  ;; c
    (print-dec fix-bcond) (write-char-serial 10)
    (write-char-serial 102) ;; f
    (write-char-serial 97)  ;; a
    (print-dec fix-adr) (write-char-serial 10)
    (write-char-serial 102) ;; f
    (write-char-serial 115) ;; s (skip - no label)
    (print-dec fix-skip) (write-char-serial 10)
    (write-char-serial 102) ;; f
    (write-char-serial 109) ;; m (miss - no type match)
    (print-dec fix-miss) (write-char-serial 10))
  buf)

;;; ============================================================
;;; Bare-metal overrides for AArch64 encoder functions
;;; These handle callers that pass fewer args (missing keyword values
;;; that were stripped by preprocessing — use null defaults)
;;; ============================================================

;;; --- RET: override to hardcode x30 (avoids &optional call-site bug) ---
;;; The original a64-ret has &optional rn which becomes a 2-param function
;;; but call sites pass only buf. This 1-param override avoids the mismatch.
(defun a64-ret (buf)
  (a64-emit buf (logior #xD65F0000 (ash 30 5))))

;;; --- MOV instructions (null-safe hw) ---
(defun a64-movz (buf rd imm16 hw)
  (when (null hw) (setq hw 0))
  (a64-emit buf (logior (ash 1 31) (ash #b10 29) (ash #b100101 23)
                        (ash (logand hw 3) 21) (ash (logand imm16 65535) 5) rd)))

(defun a64-movk (buf rd imm16 hw)
  (when (null hw) (setq hw 0))
  (a64-emit buf (logior (ash 1 31) (ash #b11 29) (ash #b100101 23)
                        (ash (logand hw 3) 21) (ash (logand imm16 65535) 5) rd)))

(defun a64-movn (buf rd imm16 hw)
  (when (null hw) (setq hw 0))
  (a64-emit buf (logior (ash 1 31) (ash #b00 29) (ash #b100101 23)
                        (ash (logand hw 3) 21) (ash (logand imm16 65535) 5) rd)))

;;; The ADD/SUB-immediate "null-safe shift" overrides that used to live here
;;; are GONE (#252): translate-aarch64's a64-add-imm / a64-adds-imm /
;;; a64-sub-imm / a64-subs-imm take (buf rd rn imm12) and every caller passes
;;; four arguments, so the five-parameter overrides that outlived their
;;; &key era won last-defun-wins and turned every ADD/SUB-immediate into an
;;; arity mismatch that emitted NOTHING -- Gen1's metadata-zeroing loop had
;;; no increment and spun forever, and its translated code was missing every
;;; such instruction.

;;; --- ADD/SUB register (null-safe shift/amount) ---
(defun a64-add-reg (buf rd rn rm shift amount)
  (when (null shift) (setq shift 0))
  (when (null amount) (setq amount 0))
  (a64-emit buf (logior (ash 1 31) (ash #b01011 24)
                        (ash (logand shift 3) 22) (ash rm 16)
                        (ash amount 10) (ash rn 5) rd)))

(defun a64-adds-reg (buf rd rn rm shift amount)
  (when (null shift) (setq shift 0))
  (when (null amount) (setq amount 0))
  (a64-emit buf (logior (ash 1 31) (ash 1 29) (ash #b01011 24)
                        (ash (logand shift 3) 22) (ash rm 16)
                        (ash amount 10) (ash rn 5) rd)))

(defun a64-sub-reg (buf rd rn rm shift amount)
  (when (null shift) (setq shift 0))
  (when (null amount) (setq amount 0))
  (a64-emit buf (logior (ash 1 31) (ash 1 30) (ash #b01011 24)
                        (ash (logand shift 3) 22) (ash rm 16)
                        (ash amount 10) (ash rn 5) rd)))

(defun a64-subs-reg (buf rd rn rm shift amount)
  (when (null shift) (setq shift 0))
  (when (null amount) (setq amount 0))
  (a64-emit buf (logior (ash 1 31) (ash 1 30) (ash 1 29) (ash #b01011 24)
                        (ash (logand shift 3) 22) (ash rm 16)
                        (ash amount 10) (ash rn 5) rd)))

(defun a64-cmp-reg (buf rn rm)
  (a64-subs-reg buf 31 rn rm 0 0))

;;; --- DMB/DSB (null-safe option) ---
(defun a64-dmb (buf option)
  (when (null option) (setq option 11))
  (a64-emit buf (logior #xD5033000 (ash (logand option 15) 8) #xBF)))

(defun a64-dsb (buf option)
  (when (null option) (setq option 11))
  (a64-emit buf (logior #xD5033000 (ash (logand option 15) 8) #x9F)))

;;; --- a64-load-imm64: bare-metal compatible (no list/remove-if/lambda) ---
(defun a64-load-imm64 (buf rd imm64)
  (let ((hw0 (logand imm64 65535))
        ;; #252: IMM64 may be a bignum.  (ash bignum -16) is an INLINED :sar on
        ;; the bignum's POINTER (CLAUDE.md limitation 8: address-dependent
        ;; garbage, so Gen3 could never equal Gen1), and (ash bignum -32) is a
        ;; runtime BIGNUM-ASH with a variable count (8b, runaway bignum).
        (hw1 (logand (floor imm64 65536) 65535))
        ;; (continued) -- FLOOR is bignum-safe and brings the high halves into
        ;; fixnum range before the remaining constant shift.
        (hw2 (logand (floor imm64 4294967296) 65535))
        (hw3 (logand (ash (floor imm64 140737488355328) -1) 65535)))
    (let ((nz 0) (first-nz-idx 0))
      (when (> hw0 0) (setq nz (+ nz 1)) (setq first-nz-idx 0))
      (when (> hw1 0) (setq nz (+ nz 1)) (when (= nz 1) (setq first-nz-idx 1)))
      (when (> hw2 0) (setq nz (+ nz 1)) (when (= nz 1) (setq first-nz-idx 2)))
      (when (> hw3 0) (setq nz (+ nz 1)) (when (= nz 1) (setq first-nz-idx 3)))
      (cond
        ((= nz 0) (a64-movz buf rd 0 0))
        ((= nz 1)
         (let ((val 0))
           (cond ((> hw0 0) (setq val hw0) (setq first-nz-idx 0))
                 ((> hw1 0) (setq val hw1) (setq first-nz-idx 1))
                 ((> hw2 0) (setq val hw2) (setq first-nz-idx 2))
                 ((> hw3 0) (setq val hw3) (setq first-nz-idx 3)))
           (a64-movz buf rd val first-nz-idx)))
        (t
         (let ((did-first nil))
           (when (> hw0 0)
             (if did-first (a64-movk buf rd hw0 0)
                 (progn (a64-movz buf rd hw0 0) (setq did-first t))))
           (when (> hw1 0)
             (if did-first (a64-movk buf rd hw1 1)
                 (progn (a64-movz buf rd hw1 1) (setq did-first t))))
           (when (> hw2 0)
             (if did-first (a64-movk buf rd hw2 2)
                 (progn (a64-movz buf rd hw2 2) (setq did-first t))))
           (when (> hw3 0)
             (if did-first (a64-movk buf rd hw3 3)
                 (progn (a64-movz buf rd hw3 3) (setq did-first t))))))))))

;;; The a64-emit-prologue / a64-emit-epilogue overrides that lived here are
;;; GONE (#252).  They set x29 = sp BEFORE the `sub sp, #1024', while
;;; translate-aarch64's frame model (and every [x29 + off] local it emits)
;;; assumes x29 was set LAST, at the bottom of the locals region -- so every
;;; local store in Gen1 landed 1 KB up, in the CALLER's frame, over its saved
;;; x19..x23; a callee's epilogue then handed %BULK-COPY a stack address as
;;; its index and the store went through the page tables.  They also never
;;; saved x27 (CENV) or the V9-V13 local registers.  The modern pair takes
;;; only 4-argument helpers, so nothing here needs to stand in for it.

;;; Override a64-buffer-to-bytes: dotimes on bare metal returns nil (ignores
;;; result form), so the original function returns nil instead of the byte array.
;;; Use explicit loop with return.
(defun td-bc-xor (tag bc)
  ;; #252 diag: X<tag>=<xor of the bytecode array> -- the array is being
  ;; overwritten with cons pointers at 8-byte slots during translation.
  (let ((x 0) (i 0) (n (array-length bc)))
    (loop (when (>= i n) (return nil)) (setq x (logxor x (aref bc i))) (setq i (+ i 1)))
    (write-char-serial 88) (write-char-serial tag) (write-char-serial 61) (print-dec x) (write-char-serial 10)))
(defun td-a64-gc-bitmap-init (obj-base cons-base nbytes)
  ;; The CL image's %rpi-gc-bitmap-init, for the fixpoint's AArch64 generation:
  ;; zero the object-start and cons-kind bitmaps and publish page_base (from
  ;; the %gc-init'd block), the object bitmap and the cons bitmap in the MCGC
  ;; config words the native trampoline reads.  Must run BEFORE the first
  ;; allocation: an object allocated earlier carries no start bit and the
  ;; collector would refuse to forward references to it.
  (let ((i 0))
    (loop
      (when (>= i nbytes) (return nil))
      (setf (mem-ref (+ obj-base i) :u64) 0)
      (setf (mem-ref (+ cons-base i) :u64) 0)
      (setq i (+ i 8))))
  (setf (mem-ref #x10000E00 :u64) (%gc-from-start))
  (setf (mem-ref #x10000E18 :u64) obj-base)
  (setf (mem-ref #x10000E40 :u64) cons-base)
  nil)
(defvar *td-bc-shadow* nil)
(defun td-bc-diff (bc)
  ;; #252 diag: first index where BC differs from the shadow copy taken at S1,
  ;; the 8-byte word there (little-endian, from the 8-aligned slot), and the count.
  (let ((n (array-length bc)) (i 0) (first -1) (cnt 0))
    (loop (when (>= i n) (return nil))
      (unless (= (aref bc i) (aref *td-bc-shadow* i))
        (setq cnt (+ cnt 1)) (when (< first 0) (setq first i)))
      (setq i (+ i 1)))
    (write-char-serial 68) (write-char-serial 61) (print-dec first) (write-char-serial 47) (print-dec cnt)
    (when (>= first 0)
      (let ((b (logand first -8)) (w 0) (k 7))
        (loop (when (< k 0) (return nil)) (setq w (logior (ash w 8) (aref bc (+ b k)))) (setq k (- k 1)))
        (write-char-serial 32) (write-char-serial 87) (print-dec w)))
    (write-char-serial 10)))
(defvar *td-bad-word-index* -1)
(defvar *td-bc-to-native* nil)
(defun td-apply-fn-addr-patches (native-start code-base native-size)
  ;; NATIVE-START = image offset of the native code; CODE-BASE = its VA;
  ;; NATIVE-SIZE bounds the patch positions -- 40 entries once landed PAST
  ;; the native code and rewrote the appended bytecode (#252); they are
  ;; skipped, counted, and the first few named (position, target).
  (let ((rest *aarch64-fn-addr-patches*) (applied 0) (missing 0) (oob 0))
    (loop
      (when (null rest) (return nil))
      (let ((patch (car rest)))
        (let ((movz-pos (car patch)) (target-bc (cdr patch)))
          (let ((noff (if (and (fixnump movz-pos) (>= movz-pos 0) (< (+ movz-pos 8) native-size))
                          (gethash target-bc *td-bc-to-native*)
                          (progn
                            (setq oob (+ oob 1))
                            (when (<= oob 4)
                              (write-char-serial 79) (write-char-serial 66) (write-char-serial 58) ;; OB:
                              (if (fixnump movz-pos) (print-dec movz-pos) (write-char-serial 63)) (write-char-serial 32)
                              (if (fixnump target-bc) (print-dec target-bc) (write-char-serial 63)) (write-char-serial 10))
                            nil))))
            (if noff
                (let ((vaddr (logior (+ code-base noff) 3)))
                  (td-patch-a64-imm16 (+ native-start movz-pos) (logand vaddr 65535))
                  (td-patch-a64-imm16 (+ native-start movz-pos 4) (logand (ash vaddr -16) 65535))
                  (setq applied (+ applied 1)))
                (setq missing (+ missing 1))))))
      (setq rest (cdr rest)))
    (write-char-serial 70) (write-char-serial 80) (write-char-serial 58) ;; FP:<applied> <missing> <out-of-range>
    (print-dec applied) (write-char-serial 32) (print-dec missing) (write-char-serial 32) (print-dec oob) (write-char-serial 10)))
(defun a64-buffer-to-bytes (buf)
  (let ((code (a64-buffer-code buf)))
    (let ((n (a64-buffer-position buf)))
      ;; A BYTE array (4n bytes), not a general array (32n bytes): at 16.6 MB
      ;; of AArch64 code the general array was 133 MB, and with the 134 MB
      ;; code array and the bytecode live beside it the peak no longer fit a
      ;; 360 MB semispace -- the pre-check collected, the allocation STILL
      ;; overshot into to-space, and the next collection copied the bytecode
      ;; array over this array's tail: Gen1's last 5 MB of native code were
      ;; bytecode (#252).
      (let ((bytes (make-array (* n 4) :element-type '(unsigned-byte 8))))
        ;; #252 diag: NB:<position> <4n> <array-length bytes> <array-length code>
        (write-char-serial 78) (write-char-serial 66) (write-char-serial 58)
        (print-dec n) (write-char-serial 32) (print-dec (* n 4)) (write-char-serial 32)
        (print-dec (array-length bytes)) (write-char-serial 32) (print-dec (array-length code)) (write-char-serial 10)
        (let ((i 0))
          (loop
            (when (>= i n) (return bytes))
            (let ((w (aref code i)))
              ;; #252 diag: the first non-fixnum code word (the byte array inherits it)
              (unless (fixnump w)
                (when (< *td-bad-word-index* 0)
                  (setq *td-bad-word-index* i)
                  (write-char-serial 88) (write-char-serial 87) (write-char-serial 58) ;; XW:
                  (print-dec i) (write-char-serial 32)
                  (print-dec (cond ((null w) 1) ((consp w) 2) (t (obj-subtag w)))) (write-char-serial 32)
                  (let ((k (- i 2)))
                    (loop (when (> k (+ i 2)) (return nil))
                      (let ((v (aref code k))) (write-char-serial 91) (if (fixnump v) (print-dec v) (write-char-serial 63)) (write-char-serial 93))
                      (setq k (+ k 1))))
                  (write-char-serial 10))
                (setq w 0))
              (let ((base (* i 4)))
                (aset bytes base (logand w 255))
                (aset bytes (+ base 1) (logand (ash w -8) 255))
                (aset bytes (+ base 2) (logand (ash w -16) 255))
                (aset bytes (+ base 3) (logand (ash w -24) 255))))
            (setq i (+ i 1))))))))

;;; Closure fix: flet functions ensure-src and store-dst in translate-mvm-insn
;;; capture 'buf' from parent scope, but MVM compiler compiles them as separate
;;; global functions. The parent's V1 (buf) maps to their own V1 (scratch/vreg),
;;; so they read wrong values. Fix: store buf at memory address 0x300058.

(defun set-current-a64-buf (buf)
  (setf (mem-ref #x3000058 :u64) buf))

(defun get-current-a64-buf ()
  (mem-ref #x3000058 :u64))

;;; Override ensure-src: reads buf from fixed memory instead of broken closure
(defun ensure-src (vreg scratch)
  (let ((p (a64-phys-reg vreg)))
    (if p p
        (let ((buf (get-current-a64-buf)))
          (a64-emit-load-vreg buf scratch vreg)
          scratch))))

;;; Override store-dst: reads buf from fixed memory instead of broken closure
(defun store-dst (phys-src vreg)
  (let ((buf (get-current-a64-buf)))
    (a64-emit-store-vreg buf phys-src vreg)))

;;; AArch64 per-function translation helper
(defun td-a64-translate-fn-body (bytecode offset len buf mvm-to-native-label)
  ;; Store buf in fixed memory for ensure-src/store-dst closure fix
  (set-current-a64-buf buf)
  (let ((pos offset)
        (limit (+ offset len)))
    (loop
      (when (>= pos limit) (return nil))
      ;; Write diagnostic
      (td-write-u32 #x3000048 pos)
      ;; Set label if this offset has one
      (let ((label (gethash pos mvm-to-native-label)))
        (when label
          (a64-set-label buf label)))
      ;; Decode instruction
      (let ((decoded (decode-instruction bytecode pos)))
        (let ((opcode (car decoded))
              (operands (car (cdr decoded)))
              (new-pos (cdr (cdr decoded))))
          (td-write-u32 #x300004C opcode)
          ;; Build a decoded-mvm-insn struct
          (let ((insn (make-decoded-mvm-insn)))
            (set-decoded-mvm-insn-offset insn pos)
            (set-decoded-mvm-insn-opcode insn opcode)
            (set-decoded-mvm-insn-operands insn operands)
            (set-decoded-mvm-insn-size insn (- new-pos pos))
            (translate-mvm-insn insn buf mvm-to-native-label))
          (setq pos new-pos))))))

;;; AArch64 branch target pre-scan
(defun td-a64-scan-branches (bytecode offset len mvm-to-native-label)
  (let ((pos offset)
        (limit (+ offset len)))
    (loop
      (when (>= pos limit) (return nil))
      (let ((decoded (decode-instruction bytecode pos)))
        (let ((opcode (car decoded))
              (operands (car (cdr decoded)))
              (new-pos (cdr (cdr decoded))))
          ;; Branch opcodes: #x40-#x48
          (when (>= opcode #x40)
            (when (<= opcode #x48)
              ;; BNULL(#x47)/BNNULL(#x48) have Vs first, offset second
              (let ((off-idx 0))
                (when (>= opcode #x47)
                  (setq off-idx 1))
                (let ((mvm-offset (nth off-idx operands)))
                  (let ((target-byte (+ pos (- new-pos pos) mvm-offset)))
                    (let ((existing (gethash target-byte mvm-to-native-label)))
                      (when (null existing)
                        (let ((lbl (gensym-label)))
                          (puthash target-byte mvm-to-native-label lbl)))))))))
          (setq pos new-pos))))))

;;; Helper: generate unique label ID using *mvm-label-counter*
(defun gensym-label ()
  (let ((v *mvm-label-counter*))
    (setq *mvm-label-counter* (+ v 1))
    v))

;;; Override translate-mvm-to-aarch64 for bare metal
;;; Same pattern as translate-mvm-to-x64 override: works with function-table list
;;; Returns (cons a64-buffer fn-map) where fn-map maps name-hash to native-byte-offset
(defun translate-mvm-to-aarch64 (bytecode function-table)
  (write-char-serial 97) (write-char-serial 54) (write-char-serial 52) ;; a64
  (write-char-serial 10)
  ;; #'NAME loads are MOVZ/MOVK placeholders the translator records on
  ;; *aarch64-fn-addr-patches* (native byte pos . target bytecode offset) for
  ;; a post-link pass -- cross.lisp's apply-aarch64-fn-addr-patches on the
  ;; host.  Nothing applied them here, so every #'f in Gen1 was 0 and the
  ;; first FUNCALL of one (PUTHASH's comparator) trapped (#252).  Start the
  ;; list fresh, keep a bytecode-offset -> native-offset map for the
  ;; assembler, which applies them (td-apply-fn-addr-patches).
  (setq *aarch64-fn-addr-patches* nil)
  (setq *aarch64-translated-start-idx* 0)
  (setq *td-bc-to-native* (make-hash-table))
  ;; #252 diag: shadow copy of the bytecode for td-bc-diff
  (let ((n (array-length bytecode)))
    (setq *td-bc-shadow* (make-array n :element-type (quote (unsigned-byte 8))))
    (let ((i 0)) (loop (when (>= i n) (return nil)) (aset *td-bc-shadow* i (aref bytecode i)) (setq i (+ i 1)))))
  (let ((buf (make-a64-buffer)))
    (let ((n-functions (length function-table)))
      (print-dec n-functions) (write-char-serial 10)
      (let ((mvm-to-native-label (make-hash-table)))
        ;; First pass: register labels for all function entry points
        (let ((rest-ft function-table)
              (i 0))
          (loop
            (when (>= i n-functions) (return nil))
            (let ((entry (car rest-ft)))
              (let ((offset (car (cdr entry))))
                (let ((lbl (gensym-label)))
                  (puthash offset mvm-to-native-label lbl))))
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1))))
        (td-bc-xor 97 bytecode)  ;; Xa= after first pass (labels for entries)
        ;; Pre-scan ALL function bodies for branch targets
        (let ((rest-ft function-table)
              (i 0))
          (loop
            (when (>= i n-functions) (return nil))
            (let ((entry (car rest-ft)))
              (let ((offset (car (cdr entry)))
                    (len (car (cdr (cdr entry)))))
                (td-a64-scan-branches bytecode offset len mvm-to-native-label)))
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1))
            (when (zerop (mod i 500))
              (write-char-serial 115) (print-dec i) (write-char-serial 32)
              (let ((h (%ht-bucket-holder mvm-to-native-label)))
                (print-dec (if h (let ((v (%ht-h-vec h))) (cond ((null v) 0) ((arrayp v) 1) (t 2))) -1))
                (write-char-serial 47) (print-dec (if h (%ht-h-count h) -1)))
              ;; #252: g<collections> A<bytecode array word>/<its length> V<label bucket vector word>/<its length> then the checksum
              (write-char-serial 32) (td-gc-mark) (write-char-serial 65) (print-dec (%gc-word-of bytecode #x3000070))
              (write-char-serial 47) (print-dec (%prim-array-length bytecode)) (write-char-serial 32)
              (let ((h (%ht-bucket-holder mvm-to-native-label)))
                (let ((v (and h (%ht-h-vec h))))
                  (write-char-serial 86)
                  (if (arrayp v)
                      (progn (print-dec (%gc-word-of v #x3000070)) (write-char-serial 47) (print-dec (%prim-array-length v)))
                      (write-char-serial 45))
                  (write-char-serial 32)))
              (td-bc-xor 112 bytecode)
              (td-bc-diff bytecode))))
        (td-bc-xor 98 bytecode)  ;; Xb= after the branch pre-scan
        ;; label-table state after the pre-scan: W<index kind: 1 = bucketed> N<count> (#252)
        (let ((h (%ht-bucket-holder mvm-to-native-label)))
          (write-char-serial 87) (print-dec (if h (let ((v (%ht-h-vec h))) (cond ((null v) 0) ((arrayp v) 1) (t 2))) -1))
          (write-char-serial 32) (write-char-serial 78) (print-dec (if h (%ht-h-count h) -1)) (write-char-serial 10))
        ;; The NATIVE collector (#252): bind the trampoline label now so every
        ;; gc-check the second pass emits is a BL to it (bare metal never loads
        ;; x28, so *aarch64-gc-trampoline-call-via-bl* must be T); the trampoline
        ;; itself is emitted after the last function, and Gen1's kernel-main
        ;; publishes the heap geometry and zeroes the bitmaps at boot.
        (setq *aarch64-gc-trampoline-label* (gensym-label))
        ;; Second pass: translate each function
        (write-char-serial 84) (write-char-serial 10) ;; T
        (let ((fn-map (make-hash-table)))
          (let ((rest-ft function-table)
                (i 0))
            (loop
              (when (>= i n-functions) (return nil))
              (td-write-u32 #x3000040 i)
              (let ((entry (car rest-ft)))
                (let ((name (car entry))
                      (offset (car (cdr entry)))
                      (len (car (cdr (cdr entry)))))
                  ;; 16-byte-align the ENTRY VA, as translate-mvm-to-aarch64 proper
                  ;; does: fn pointers are addr|3 and CALL-IND checks the low
                  ;; nibble is exactly 3, so the raw address must end in 0.  The
                  ;; native code starts at image offset 0x1004 (VA 0x81004), so the
                  ;; entry VA is 16-aligned when (4*index + 4) mod 16 = 0.  Without
                  ;; this every #'f in Gen1 was addr|3 with nibble B and the first
                  ;; FUNCALL trapped (#252).
                  (loop
                    (when (zerop (mod (+ (* (a64-current-index buf) 4) 4) 16)) (return nil))
                    (a64-emit buf #xD503201F))
                  ;; Set label at function entry
                  (let ((fn-label (gethash offset mvm-to-native-label)))
                    (when fn-label
                      (a64-set-label buf fn-label)))
                  ;; Record native byte offset for this function
                  (let ((native-off (* (a64-current-index buf) 4)))
                    (puthash name fn-map native-off)
                    (puthash offset *td-bc-to-native* native-off)
                    ;; per-function native map for mapping a Gen1 fault PC: F<hash>@<off>
                    (write-char-serial 70) (print-dec name) (write-char-serial 64) (print-dec native-off) (write-char-serial 10))
                  ;; NOTE: No explicit prologue here — the TRAP instruction
                  ;; at function start triggers prologue via translate-mvm-insn
                  ;; Translate body
                  (td-a64-translate-fn-body bytecode offset len buf mvm-to-native-label)))
              (setq rest-ft (cdr rest-ft))
              (setq i (+ i 1))
              (when (zerop (mod i 50))
                (write-char-serial 35)
                (print-dec i) (write-char-serial 64) (print-dec (a64-buffer-position buf))
                (write-char-serial 10))))
          ;; End-of-stream label
          (let ((end-label (gethash (array-length bytecode) mvm-to-native-label)))
            (when end-label
              (a64-set-label buf end-label)))
          (td-bc-xor 99 bytecode)  ;; Xc= after the second pass (translation)
          ;; The native Cheney collector, reached by BL from every gc-check.
          (when (and *aarch64-gc-native-mcgc* *aarch64-gc-trampoline-label*)
            (write-char-serial 84) (write-char-serial 82) (write-char-serial 58) ;; TR:
            (print-dec (a64-buffer-position buf)) (write-char-serial 32)
            (emit-aarch64-native-gc-trampoline buf)
            (write-char-serial 47) (print-dec (a64-buffer-position buf)) (write-char-serial 10))
          ;; #252 diag: which function owns the first bad code word (see a64-buffer-to-bytes)
          (when (>= *td-bad-word-index* 0)
            (let ((bad (* *td-bad-word-index* 4)) (best -1) (best-name 0) (rf function-table))
              (loop (when (null rf) (return nil))
                (let ((nm (car (car rf))))
                  (let ((off (gethash nm fn-map)))
                    (when (and off (<= off bad) (> off best)) (setq best off) (setq best-name nm))))
                (setq rf (cdr rf)))
              (write-char-serial 88) (write-char-serial 70) (write-char-serial 58) ;; XF:
              (print-dec best-name) (write-char-serial 32) (print-dec best) (write-char-serial 32) (print-dec bad) (write-char-serial 10)))
          ;; #252 diag: final position and the KERNEL-MAIN entry as recorded
          (write-char-serial 81) (print-dec (a64-buffer-position buf)) (write-char-serial 32)
          (print-dec (gethash (td-read-u32 #x3000028) fn-map)) (write-char-serial 10) ;; Q<pos> <km-off>
          ;; Resolve fixups
          (write-char-serial 82) (write-char-serial 10) ;; R
          (a64-resolve-fixups buf)
          (write-char-serial 68) (print-dec (a64-buffer-position buf)) (write-char-serial 10) ;; D<pos after fixups>
          (td-bc-xor 100 bytecode)  ;; Xd= after fixup resolution
          ;; Return (cons buf fn-map)
          ;; Convert buffer to bytes for consistency
          ;; #252: hand back the WORD array itself.  The 4n byte copy was a
          ;; ~46 MB single allocation, larger than the 16 MB guard band, so
          ;; its gc-check fired after the allocation had already run 30 MB
          ;; into to-space and the collection copied live data over it.
          (let ((native-size (* (a64-current-index buf) 4)))
            (cons (a64-buffer-code buf) (cons native-size fn-map))))))))

;;; ============================================================
;;; AArch64 image assembly
;;; ============================================================

;;; Generate AArch64 boot preamble into image buffer
;;; Uses emit-aarch64-fixpoint-entry which writes into an mvm-buffer,
;;; then copies the bytes into the image.
(defun td-generate-aarch64-boot ()
  ;; emit-aarch64-u32 writes 32-bit WORDS into an a64-buffer's code array
  ;; (task #34: boot preamble and translated code share one label/fixup
  ;; space).  This used to hand it an mvm-buffer and read mvm-buffer-bytes
  ;; back -- so every Gen1 began with garbage (first word 0x20001f10, an
  ;; undefined instruction; #252).  Emit into an a64-buffer, resolve any
  ;; fixups, and serialise the words little-endian.
  (let ((boot-buf (make-a64-buffer)))
    (emit-aarch64-fixpoint-entry boot-buf)
    (a64-resolve-fixups boot-buf)
    (let ((bytes (a64-buffer-to-bytes boot-buf)))
      (let ((boot-size (* (a64-buffer-position boot-buf) 4))
            (i 0))
        (loop
          (when (>= i boot-size) (return boot-size))
          (img-emit (aref bytes i))
          (setq i (+ i 1)))))))

;;; #252: the metadata block sits at a FIXED image offset (VA 0x3000000 is what
;;; every td-read-u32 #x30000xx in this file reads).  With the native GC
;;; collector and bitmaps baked in, AArch64 native code alone is ~46 MB and
;;; the 10 MB bytecode no longer fits below the block, so when it would not,
;;; the bytecode (and the function table after it) go ABOVE the block instead.
;;; Consumers only ever reach them through the metadata's offset fields.
(defun td-bytecode-start (bc-len md-off)
  (when (> (+ (img-pos) bc-len #x40000) md-off)
    (let ((target (+ md-off #x1000)))
      (loop
        (when (>= (img-pos) target) (return nil))
        (img-emit 0))
      (write-char-serial 94) (print-dec target) (write-char-serial 10))) ;; ^<bytecode moved above metadata>
  (img-pos))

(defun td-image-total-size (md-off)
  (if (> (img-pos) (+ md-off 64)) (img-pos) (+ md-off 64)))

;;; Assemble Gen1 AArch64 image from translated native code
;;; Image layout: [boot preamble 4096B] [native code at offset 0x1000] [bytecodes] [fn-table] [pad] [metadata at 0x500000]
(defun td-patch-a64-imm16 (byte-off imm16)
  ;; Rewrite the imm16 field (bits 5..20) of the MOVZ/MOVK at image offset BYTE-OFF.
  (let ((w (mem-ref (+ #x08000000 byte-off) :u32)))
    (img-patch-u32 byte-off (logior (logand w #xFFE0001F) (ash (logand imm16 65535) 5)))))

(defun td-assemble-gen1-aarch64 (result bc ft)
  ;; result = (cons native-bytes (cons native-size fn-map))
  (let ((native-bytes (car result))
        (native-size (car (cdr result)))
        (fn-map (cdr (cdr result))))
    ;; 1. Init image buffer
    (img-init)
    (write-char-serial 65) (write-char-serial 49) ;; A1
    (write-char-serial 58) (write-char-serial 10)
    ;; 2. Generate AArch64 boot preamble (fills up to offset 0x1000)
    (let ((boot-size (td-generate-aarch64-boot)))
      (write-char-serial 80) ;; P
      (print-dec boot-size) (write-char-serial 10)
      ;; Pad to 0x1000 if needed (native code must start at instruction 1024 = offset 0x1000)
      (let ((pad-target #x1000))
        (loop
          (when (>= (img-pos) pad-target) (return nil))
          (img-emit 0)))
      ;; 3. Emit B instruction at 0x1000 to jump to kernel-main
      ;; The boot preamble branches to offset 0x1000 (instruction 1024).
      ;; We emit a B instruction here that jumps forward to kernel-main.
      (write-char-serial 75) ;; K
      (let ((km-hash (td-read-u32 #x3000028)))
        (print-dec km-hash) (write-char-serial 10)
        (let ((km-native-off (gethash km-hash fn-map)))
          (write-char-serial 79) ;; O
          (if km-native-off
              (let ((dummy1 (print-dec km-native-off)))
                (write-char-serial 10)
                (let ((km-insn-offset (ash km-native-off -2)))
                  ;; B forward: offset = km_insn_offset + 1 (skip this B instruction)
                  (let ((b-offset (+ km-insn-offset 1)))
                    (write-char-serial 66) ;; B
                    (print-dec b-offset) (write-char-serial 10)
                    (let ((b-word (logior (ash #b000101 26)
                                          (logand b-offset #x3FFFFFF))))
                      (write-char-serial 87) ;; W
                      (print-dec b-word) (write-char-serial 10)
                      (write-char-serial 73) ;; I  img-pos before emit
                      (print-dec (img-pos)) (write-char-serial 10)
                      (img-emit-u32 b-word)
                      (write-char-serial 74) ;; J  img-pos after emit
                      (print-dec (img-pos)) (write-char-serial 10)))))
              ;; No kernel-main found — emit NOP (shouldn't happen)
              (let ((dummy2 0))
                (write-char-serial 33) (write-char-serial 10) ;; !
                (img-emit-u32 #xD503201F)))))
      ;; 4. Copy native code (starts at 0x1004)
      (write-char-serial 78) ;; N
      (td-write-u32 #x3000050 (img-pos))
      ;; native-bytes is the a64-buffer's WORD array (see td-translate); a
      ;; non-fixnum word (never observed since the flat-scan fix) writes 0.
      (let ((i 0) (nwords (ash native-size -2)))
        (loop
          (when (>= i nwords) (return nil))
          (let ((w (aref native-bytes i)))
            (img-emit-u32 (if (fixnump w) w 0)))
          (setq i (+ i 1))
          (when (zerop (mod i 12500))
            (write-char-serial 46))))
      (write-char-serial 10)
      ;; Patch the code-bounds placeholders emit-aarch64-code-bounds-init left
      ;; in the preamble (what cross.lisp's apply-aarch64-code-bounds-patches
      ;; does for a host build): code_base = image VA 0x80000 + native start,
      ;; code_end = code_base + native size.  Unpatched they read 0 and
      ;; FUNCTIONP's code-range check misclassifies fn-addrs (#252).
      (let ((code-base (+ #x80000 (td-read-u32 #x3000050))))
        (let ((code-end (+ code-base native-size)))
          (td-patch-a64-imm16 *aarch64-code-base-patch-offset* (logand code-base 65535))
          (td-patch-a64-imm16 (+ *aarch64-code-base-patch-offset* 4) (logand (ash code-base -16) 65535))
          (td-patch-a64-imm16 *aarch64-code-end-patch-offset* (logand code-end 65535))
          (td-patch-a64-imm16 (+ *aarch64-code-end-patch-offset* 4) (logand (ash code-end -16) 65535))
          (write-char-serial 67) (write-char-serial 66) (write-char-serial 58) ;; CB:
          (print-dec code-base) (write-char-serial 45) (print-dec code-end) (write-char-serial 10)
          (td-apply-fn-addr-patches (td-read-u32 #x3000050) code-base native-size)))
      (td-bc-xor 101 bc)  ;; Xe= just before the bytecode is appended to the image
      ;; 4. Append MVM bytecode
      (write-char-serial 84) ;; T
      (let ((bc-len (array-length bc))
            (bc-img-offset (td-bytecode-start (array-length bc) #x2F80000)))
        (let ((bi 0))
          (loop
            (when (>= bi bc-len) (return nil))
            (img-emit (aref bc bi))
            (setq bi (+ bi 1))))
        ;; 5. Append function table (12-byte u32 LE entries)
        (let ((ft-img-offset (img-pos))
              (rest-ft ft)
              (ft-count 0))
          (loop
            (when (null rest-ft) (return nil))
            (let ((entry (car rest-ft)))
              (let ((name (car entry))
                    (offset (car (cdr entry)))
                    (len (car (cdr (cdr entry)))))
                (img-emit-u32 name)
                (img-emit-u32 offset)
                (img-emit-u32 len)))
            (setq rest-ft (cdr rest-ft))
            (setq ft-count (+ ft-count 1)))
          (write-char-serial 10)
          (print-dec ft-count) (write-char-serial 10)
          ;; 6. Write metadata at offset 0x440000
          ;; QEMU virt loads raw binary at PA 0x40080000 (not 0x40000000).
          ;; MMU maps VA = PA - 0x40000000, so image start VA = 0x80000.
          ;; Metadata VA must be 0x500000, so image offset = 0x500000 - 0x80000 = 0x440000.
          (let ((md-img-off #x2F80000))
            ;; magic MVMT
            (img-patch-u32 md-img-off #x544D564D)
            ;; version = 1
            (img-patch-u32 (+ md-img-off 4) 1)
            ;; my-architecture = 1 (aarch64)
            (img-patch-u32 (+ md-img-off 8) 1)
            ;; bytecode-offset
            (img-patch-u32 (+ md-img-off 12) bc-img-offset)
            ;; bytecode-length
            (img-patch-u32 (+ md-img-off 16) bc-len)
            ;; fn-table-offset
            (img-patch-u32 (+ md-img-off 20) ft-img-offset)
            ;; fn-table-count
            (img-patch-u32 (+ md-img-off 24) ft-count)
            ;; native-code-offset
            (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x3000050))
            ;; native-code-length
            (img-patch-u32 (+ md-img-off 32) native-size)
            ;; preamble-size = 0x1000 (4096, AArch64 boot preamble)
            (img-patch-u32 (+ md-img-off 36) #x1000)
            ;; kernel-main-hash-lo (copy from running kernel)
            (let ((km-hash (td-read-u32 #x3000028)))
              (img-patch-u32 (+ md-img-off 40) km-hash))
            ;; kernel-main native offset (look up in fn-map)
            (let ((km-native-off (gethash (td-read-u32 #x3000028) fn-map)))
              (if km-native-off
                  (img-patch-u32 (+ md-img-off 44) km-native-off)
                  (img-patch-u32 (+ md-img-off 44) 0)))
            ;; image-load-addr: QEMU loads at PA 0x40080000.
            ;; MMU maps VA = PA - 0x40000000, so VA load-addr = 0x80000.
            (img-patch-u32 (+ md-img-off 48) #x80000)
            ;; target-architecture (default: 0=x64, overridden by host script)
            (img-patch-u32 (+ md-img-off 52) 0)
            ;; mode (default: 0=cross-compile, overridden by host script)
            (img-patch-u32 (+ md-img-off 56) 0))
          ;; Total size must cover metadata at 0x440000
          (let ((total-size (td-image-total-size #x2F80000)))
            (write-char-serial 65) (write-char-serial 49) ;; A1
            (write-char-serial 61) ;; =
            (print-dec total-size) (write-char-serial 10)
            total-size))))))

;;; ============================================================
;;; x64 image assembly (from AArch64 Gen1 going back to x64)
;;; ============================================================

;;; Generate x64 boot preamble into image buffer
(defun td-generate-x64-boot ()
  (let ((boot-buf (make-mvm-buffer)))
    (emit-x64-multiboot-header boot-buf)
    (emit-x64-boot32 boot-buf)
    (emit-x64-kernel64-entry boot-buf)
    ;; Copy boot bytes into image
    (let ((boot-size (mvm-buffer-position boot-buf))
          (i 0))
      (loop
        (when (>= i boot-size) (return boot-size))
        (img-emit (aref (mvm-buffer-bytes boot-buf) i))
        (setq i (+ i 1))))))

;;; Assemble Gen1 x64 image (when running on AArch64)
(defun td-assemble-gen1-x64 (result bc ft)
  ;; result = (cons code-buffer fn-map) from translate-mvm-to-x64
  (let ((buf (car result))
        (fn-map (cdr result)))
    (let ((native-bytes (code-buffer-bytes buf))
          (native-size (code-buffer-position buf)))
      ;; 1. Init image buffer
      (img-init)
      (write-char-serial 88) (write-char-serial 49) ;; X1
      (write-char-serial 58) (write-char-serial 10)
      ;; 2. Generate x64 boot preamble
      (let ((boot-size (td-generate-x64-boot)))
        (write-char-serial 80) ;; P
        (print-dec boot-size) (write-char-serial 10)
        ;; 3. Emit JMP rel32 to kernel-main
        ;; Look up kernel-main native offset from fn-map
        (let ((km-hash (td-read-u32 #x3000028)))
          (let ((km-label (gethash km-hash fn-map)))
            (let ((km-offset 0))
              (when km-label
                (setq km-offset (aref km-label 1)))
              ;; JMP rel32
              (img-emit #xE9)
              (img-emit-u32 km-offset))))
        ;; 4. Copy native code
        (write-char-serial 78) ;; N
        (td-write-u32 #x3000050 (img-pos))
        (let ((i 0))
          (loop
            (when (>= i native-size) (return nil))
            (img-emit (aref native-bytes i))
            (setq i (+ i 1))
            (when (zerop (mod i 50000))
              (write-char-serial 46))))
        (write-char-serial 10)
        ;; 5. Append bytecodes
        (write-char-serial 84) ;; T
        (let ((bc-len (array-length bc))
              (bc-img-offset (td-bytecode-start (array-length bc) #x2F00000)))
          (let ((bi 0))
            (loop
              (when (>= bi bc-len) (return nil))
              (img-emit (aref bc bi))
              (setq bi (+ bi 1))))
          ;; 6. Append function table (12-byte u32 LE entries)
          (let ((ft-img-offset (img-pos))
                (rest-ft ft)
                (ft-count 0))
            (loop
              (when (null rest-ft) (return nil))
              (let ((entry (car rest-ft)))
                (let ((name (car entry))
                      (offset (car (cdr entry)))
                      (len (car (cdr (cdr entry)))))
                  (img-emit-u32 name)
                  (img-emit-u32 offset)
                  (img-emit-u32 len)))
              (setq rest-ft (cdr rest-ft))
              (setq ft-count (+ ft-count 1)))
            (write-char-serial 10)
            (print-dec ft-count) (write-char-serial 10)
            ;; 7. Write metadata at image offset 0x2F00000 = VA 0x3000000 for an x64
            ;;    image loaded at 0x100000 (the aarch64 arm uses 0x2F80000 because
            ;;    its image loads at VA 0x80000; both must read back at #x3000000).
            (let ((md-img-off #x2F00000))
              ;; magic
              (img-patch-u32 md-img-off #x544D564D)
              (img-patch-u32 (+ md-img-off 4) 1)
              ;; my-architecture = 0 (x64)
              (img-patch-u32 (+ md-img-off 8) 0)
              (img-patch-u32 (+ md-img-off 12) bc-img-offset)
              (img-patch-u32 (+ md-img-off 16) bc-len)
              (img-patch-u32 (+ md-img-off 20) ft-img-offset)
              (img-patch-u32 (+ md-img-off 24) ft-count)
              (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x3000050))
              (img-patch-u32 (+ md-img-off 32) native-size)
              (img-patch-u32 (+ md-img-off 36) boot-size)
              ;; kernel-main-hash-lo
              (let ((km-hash (td-read-u32 #x3000028)))
                (img-patch-u32 (+ md-img-off 40) km-hash)
                ;; kernel-main-native-offset
                (let ((km-label (gethash km-hash fn-map)))
                  (if km-label
                      (img-patch-u32 (+ md-img-off 44) (aref km-label 1))
                      (img-patch-u32 (+ md-img-off 44) 0))))
              ;; image-load-addr = 0x100000 (x64)
              (img-patch-u32 (+ md-img-off 48) #x100000)
              ;; target-architecture (default: 1=aarch64, overridden by host script)
              (img-patch-u32 (+ md-img-off 52) 1)
              ;; mode (default: 0=cross-compile, overridden by host script)
              (img-patch-u32 (+ md-img-off 56) 0))
            ;; 8. Patch multiboot header
            (let ((total-size (td-image-total-size #x2F00000)))
              (let ((load-end (+ #x100000 total-size)))
                (img-patch-u32 20 load-end)
                (img-patch-u32 24 load-end))
              (write-char-serial 88) (write-char-serial 49) ;; X1
              (write-char-serial 61) ;; =
              (print-dec total-size) (write-char-serial 10)
              total-size)))))))

