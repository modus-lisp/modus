;;; ============================================================
;;; AArch64 translator overrides (bare-metal compatible)
;;; ============================================================

;;; Override a64-resolve-fixups: no destructuring-bind or ecase
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
                    ((eql type 518081061)
                     (setq fix-b (+ fix-b 1))
                     (aset code index
                           (logior (logand word #xFC000000)
                                   (logand offset #x3FFFFFF))))
                    ((eql type 391831169)
                     (setq fix-bl (+ fix-bl 1))
                     (aset code index
                           (logior (logand word #xFC000000)
                                   (logand offset #x3FFFFFF))))
                    ;; :bcond (name-hash 248172622495451147)
                    ;; Reconstruct from scratch to avoid bit4 tagging issue
                    ((eql type 248172622495451147)
                     (setq fix-bcond (+ fix-bcond 1))
                     (let ((cond-bits (logand word #xF)))
                       (let ((new-word (logior (ash #b01010100 24)
                                              (logior (ash (logand offset #x7FFFF) 5)
                                                      cond-bits))))
                         ;; Check for bad bit4: write to scratch and check
                         (setf (mem-ref #x500078 :u64) new-word)
                         (let ((check-b0 (mem-ref #x500078 :u8)))
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
                    ((eql type 253724600)
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
                    (t (setq fix-miss (+ fix-miss 1))))))
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

;;; --- ADD/SUB immediate (null-safe shift) ---
(defun a64-add-imm (buf rd rn imm12 shift)
  (when (null shift) (setq shift 0))
  (let ((sh (if (= shift 12) 1 0)))
    (a64-emit buf (logior (ash 1 31) (ash #b100010 23) (ash sh 22)
                          (ash (logand imm12 4095) 10) (ash rn 5) rd))))

(defun a64-adds-imm (buf rd rn imm12 shift)
  (when (null shift) (setq shift 0))
  (let ((sh (if (= shift 12) 1 0)))
    (a64-emit buf (logior (ash 1 31) (ash 1 29) (ash #b100010 23) (ash sh 22)
                          (ash (logand imm12 4095) 10) (ash rn 5) rd))))

(defun a64-sub-imm (buf rd rn imm12 shift)
  (when (null shift) (setq shift 0))
  (let ((sh (if (= shift 12) 1 0)))
    (a64-emit buf (logior (ash 1 31) (ash 1 30) (ash #b100010 23) (ash sh 22)
                          (ash (logand imm12 4095) 10) (ash rn 5) rd))))

(defun a64-subs-imm (buf rd rn imm12 shift)
  (when (null shift) (setq shift 0))
  (let ((sh (if (= shift 12) 1 0)))
    (a64-emit buf (logior (ash 1 31) (ash 1 30) (ash 1 29) (ash #b100010 23)
                          (ash sh 22) (ash (logand imm12 4095) 10) (ash rn 5) rd))))

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
        (hw1 (logand (ash imm64 -16) 65535))
        (hw2 (logand (ash imm64 -32) 65535))
        (hw3 (logand (ash imm64 -48) 65535)))
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

;;; Override a64-emit-prologue for bare metal - direct instruction encoding
(defun a64-emit-prologue (buf)
  ;; STP x29, x30, [sp, #-80]!
  (a64-stp-pre buf 29 30 31 -80)
  ;; ADD x29, sp, #0 (no shift arg - avoid &key issue)
  (a64-emit buf (logior (ash 1 31) (ash #b100010 23) (ash 31 5) 29))
  ;; STP x19, x20, [sp, #16]
  (a64-stp-offset buf 19 20 31 16)
  ;; STP x21, x22, [sp, #32]
  (a64-stp-offset buf 21 22 31 32)
  ;; STP x23, xzr, [sp, #48]
  (a64-stp-offset buf 23 31 31 48)
  ;; SUB sp, sp, #1024 (no shift arg)
  (a64-emit buf (logior (ash 1 31) (ash 1 30) (ash #b100010 23) (ash 1024 10) (ash 31 5) 31)))

;;; Override a64-emit-epilogue for bare metal
(defun a64-emit-epilogue (buf)
  ;; ADD sp, sp, #1024 (no shift arg)
  (a64-emit buf (logior (ash 1 31) (ash #b100010 23) (ash 1024 10) (ash 31 5) 31))
  ;; LDP x23, xzr, [sp, #48]
  (a64-ldp-offset buf 23 31 31 48)
  ;; LDP x21, x22, [sp, #32]
  (a64-ldp-offset buf 21 22 31 32)
  ;; LDP x19, x20, [sp, #16]
  (a64-ldp-offset buf 19 20 31 16)
  ;; LDP x29, x30, [sp], #80
  (a64-ldp-post buf 29 30 31 80)
  ;; RET
  (a64-ret buf))

;;; Override a64-buffer-to-bytes: dotimes on bare metal returns nil (ignores
;;; result form), so the original function returns nil instead of the byte array.
;;; Use explicit loop with return.
(defun a64-buffer-to-bytes (buf)
  (let ((code (a64-buffer-code buf)))
    (let ((n (a64-buffer-position buf)))
      (let ((bytes (make-array (* n 4))))
        (let ((i 0))
          (loop
            (when (>= i n) (return bytes))
            (let ((w (aref code i)))
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
  (setf (mem-ref #x500058 :u64) buf))

(defun get-current-a64-buf ()
  (mem-ref #x500058 :u64))

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
      (td-write-u32 #x500048 pos)
      ;; Set label if this offset has one
      (let ((label (gethash pos mvm-to-native-label)))
        (when label
          (a64-set-label buf label)))
      ;; Decode instruction
      (let ((decoded (decode-instruction bytecode pos)))
        (let ((opcode (car decoded))
              (operands (car (cdr decoded)))
              (new-pos (cdr (cdr decoded))))
          (td-write-u32 #x50004C opcode)
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
            (setq i (+ i 1))))
        ;; Second pass: translate each function
        (write-char-serial 84) (write-char-serial 10) ;; T
        (let ((fn-map (make-hash-table)))
          (let ((rest-ft function-table)
                (i 0))
            (loop
              (when (>= i n-functions) (return nil))
              (td-write-u32 #x500040 i)
              (let ((entry (car rest-ft)))
                (let ((name (car entry))
                      (offset (car (cdr entry)))
                      (len (car (cdr (cdr entry)))))
                  ;; Set label at function entry
                  (let ((fn-label (gethash offset mvm-to-native-label)))
                    (when fn-label
                      (a64-set-label buf fn-label)))
                  ;; Record native byte offset for this function
                  (let ((native-off (* (a64-current-index buf) 4)))
                    (puthash name fn-map native-off))
                  ;; NOTE: No explicit prologue here — the TRAP instruction
                  ;; at function start triggers prologue via translate-mvm-insn
                  ;; Translate body
                  (td-a64-translate-fn-body bytecode offset len buf mvm-to-native-label)))
              (setq rest-ft (cdr rest-ft))
              (setq i (+ i 1))
              (when (zerop (mod i 50))
                (write-char-serial 35)
                (print-dec i)
                (write-char-serial 10))))
          ;; End-of-stream label
          (let ((end-label (gethash (array-length bytecode) mvm-to-native-label)))
            (when end-label
              (a64-set-label buf end-label)))
          ;; Resolve fixups
          (write-char-serial 82) (write-char-serial 10) ;; R
          (a64-resolve-fixups buf)
          (write-char-serial 68) (write-char-serial 10) ;; D
          ;; Return (cons buf fn-map)
          ;; Convert buffer to bytes for consistency
          (let ((native-bytes (a64-buffer-to-bytes buf)))
            ;; Diagnostic: scan byte array for B.cond (byte3=0x54)
            (let ((nb-len (array-length native-bytes))
                  (nb-good 0) (nb-bad 0) (nb-i 0))
              (loop
                (when (>= nb-i nb-len) (return nil))
                (let ((b3 (aref native-bytes (+ nb-i 3))))
                  (when (= b3 #x54)
                    (let ((b0 (aref native-bytes nb-i)))
                      (if (zerop (logand b0 #x10))
                          (setq nb-good (+ nb-good 1))
                          (let ((dummy (+ 0 0)))
                            (setq nb-bad (+ nb-bad 1))
                            (when (<= nb-bad 5)
                              (write-char-serial 88) ;; X
                              (print-dec nb-i) (write-char-serial 58)
                              (print-dec b0) (write-char-serial 44)
                              (print-dec b3) (write-char-serial 10)))))))
                (setq nb-i (+ nb-i 4)))
              (write-char-serial 71) ;; G
              (print-dec nb-good) (write-char-serial 10)
              (write-char-serial 66) ;; B
              (print-dec nb-bad) (write-char-serial 10))
            ;; Also check: scan code buffer (tagged words) for B.cond
            ;; Use a different approach: check if (aref code i) >> 25 (untagged) = 0x54
            ;; On tagged: ((tagged >> 1) >> 24) = hi byte. Check = 0x54.
            ;; We can compute: (ash (ash word -1) -24) but this is tricky with tagged values.
            ;; Instead, read the tagged word, write it to a scratch memory as u64,
            ;; then read byte 3 via u8.
            (let ((diag-code (a64-buffer-code buf))
                  (diag-n (a64-buffer-position buf))
                  (dg 0) (db 0) (di 0))
              (loop
                (when (>= di diag-n) (return nil))
                (let ((w (aref diag-code di)))
                  ;; Write tagged word to scratch address, read bytes
                  (setf (mem-ref #x500070 :u64) w)
                  ;; :u64 writes raw tagged bits. Now read byte 3 (bits [31:24] of tagged)
                  ;; Tagged = untagged << 1, so byte 3 of tagged is different from untagged.
                  ;; For untagged 0x54000001, tagged 0xA8000002:
                  ;; byte0=0x02, byte1=0x00, byte2=0x00, byte3=0xA8
                  ;; We want to check untagged byte3 = 0x54. That's tagged byte3 = 0xA8.
                  (let ((tb3 (mem-ref #x500073 :u8)))
                    (when (= tb3 #xA8)
                      ;; Check bit 4 of untagged byte0 = bit 5 of tagged byte0
                      (let ((tb0 (mem-ref #x500070 :u8)))
                        (if (zerop (logand tb0 #x20))
                            (setq dg (+ dg 1))
                            (let ((dummy2 (+ 0 0)))
                              (setq db (+ db 1))
                              (when (<= db 3)
                                (write-char-serial 67) ;; C (code buffer bad)
                                (print-dec di) (write-char-serial 58)
                                (print-dec tb0) (write-char-serial 10))))))))
                (setq di (+ di 1)))
              (write-char-serial 99) ;; g (code buffer good)
              (print-dec dg) (write-char-serial 10)
              (write-char-serial 98) ;; b (code buffer bad)
              (print-dec db) (write-char-serial 10))
            (let ((native-size (* (a64-current-index buf) 4)))
              (cons native-bytes (cons native-size fn-map)))))))))

;;; ============================================================
;;; AArch64 image assembly
;;; ============================================================

;;; Generate AArch64 boot preamble into image buffer
;;; Uses emit-aarch64-fixpoint-entry which writes into an mvm-buffer,
;;; then copies the bytes into the image.
(defun td-generate-aarch64-boot ()
  (let ((boot-buf (make-mvm-buffer)))
    (emit-aarch64-fixpoint-entry boot-buf)
    ;; Copy boot bytes into image
    (let ((boot-size (mvm-buffer-position boot-buf))
          (i 0))
      (loop
        (when (>= i boot-size) (return boot-size))
        (img-emit (aref (mvm-buffer-bytes boot-buf) i))
        (setq i (+ i 1))))))

;;; Assemble Gen1 AArch64 image from translated native code
;;; Image layout: [boot preamble 4096B] [native code at offset 0x1000] [bytecodes] [fn-table] [pad] [metadata at 0x500000]
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
      (let ((km-hash (td-read-u32 #x500028)))
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
      (td-write-u32 #x500050 (img-pos))
      (let ((i 0))
        (loop
          (when (>= i native-size) (return nil))
          (img-emit (aref native-bytes i))
          (setq i (+ i 1))
          (when (zerop (mod i 50000))
            (write-char-serial 46))))
      (write-char-serial 10)
      ;; 4. Append MVM bytecode
      (write-char-serial 84) ;; T
      (let ((bc-len (array-length bc))
            (bc-img-offset (img-pos)))
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
          (let ((md-img-off #x480000))
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
            (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x500050))
            ;; native-code-length
            (img-patch-u32 (+ md-img-off 32) native-size)
            ;; preamble-size = 0x1000 (4096, AArch64 boot preamble)
            (img-patch-u32 (+ md-img-off 36) #x1000)
            ;; kernel-main-hash-lo (copy from running kernel)
            (let ((km-hash (td-read-u32 #x500028)))
              (img-patch-u32 (+ md-img-off 40) km-hash))
            ;; kernel-main native offset (look up in fn-map)
            (let ((km-native-off (gethash (td-read-u32 #x500028) fn-map)))
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
          (let ((total-size (+ #x480000 64)))
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
        (let ((km-hash (td-read-u32 #x500028)))
          (let ((km-label (gethash km-hash fn-map)))
            (let ((km-offset 0))
              (when km-label
                (setq km-offset (aref km-label 1)))
              ;; JMP rel32
              (img-emit #xE9)
              (img-emit-u32 km-offset))))
        ;; 4. Copy native code
        (write-char-serial 78) ;; N
        (td-write-u32 #x500050 (img-pos))
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
              (bc-img-offset (img-pos)))
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
            ;; 7. Write metadata at offset 0x400000 (= VA 0x500000 for x64)
            (let ((md-img-off #x400000))
              ;; magic
              (img-patch-u32 md-img-off #x544D564D)
              (img-patch-u32 (+ md-img-off 4) 1)
              ;; my-architecture = 0 (x64)
              (img-patch-u32 (+ md-img-off 8) 0)
              (img-patch-u32 (+ md-img-off 12) bc-img-offset)
              (img-patch-u32 (+ md-img-off 16) bc-len)
              (img-patch-u32 (+ md-img-off 20) ft-img-offset)
              (img-patch-u32 (+ md-img-off 24) ft-count)
              (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x500050))
              (img-patch-u32 (+ md-img-off 32) native-size)
              (img-patch-u32 (+ md-img-off 36) boot-size)
              ;; kernel-main-hash-lo
              (let ((km-hash (td-read-u32 #x500028)))
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
            (let ((total-size (+ #x400000 64)))
              (let ((load-end (+ #x100000 total-size)))
                (img-patch-u32 20 load-end)
                (img-patch-u32 24 load-end))
              (write-char-serial 88) (write-char-serial 49) ;; X1
              (write-char-serial 61) ;; =
              (print-dec total-size) (write-char-serial 10)
              total-size)))))))

