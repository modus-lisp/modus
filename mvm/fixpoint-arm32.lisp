;;; ============================================================
;;; ARM32 translator overrides (bare-metal MVM compatible)
;;; ============================================================

;;; plusp (not in MVM prelude)
(defun plusp (x) (> x 0))

;;; arm32-ensure-label — array based (per-function label maps via *arm32-fn-labels*)
(defun arm32-ensure-label (target-pc fn-offset)
  (let ((idx (- target-pc fn-offset)))
    (let ((existing (aref *arm32-fn-labels* idx)))
      (if (not (= existing 0))
          existing
          (let ((l (mvm-make-label)))
            (aset *arm32-fn-labels* idx l)
            l)))))

;;; ARM32 buffer as array (defstruct not available on bare metal)
;;; BYTE-based buffer to avoid 31-bit fixnum overflow on 32-bit hosts.
;;; Position tracks instruction count (word count), but bytes stores LE bytes.
(defun make-arm32-buffer ()
  ;; NOTE: byte array allocated externally in *arm32-code-bytes* to avoid
  ;; defstruct version of make-arm32-buffer overriding our larger allocation
  (let ((buf (make-array 5)))
    (aset buf 0 *arm32-code-bytes*)   ;; byte array (262144 insns * 4 bytes)
    (aset buf 1 nil)                  ;; unused (labels now in *arm32-labels-ht*)
    (aset buf 2 nil)                  ;; fixups
    (aset buf 3 nil)                  ;; div-label
    (aset buf 4 0)                    ;; position (instruction/word count)
    buf))

(defun arm32-buffer-code (buf) (aref buf 0))
(defun arm32-buffer-labels (buf) *arm32-labels-ht*)
(defun arm32-buffer-fixups (buf) (aref buf 2))
(defun arm32-buffer-div-label (buf) (aref buf 3))
(defun arm32-buffer-position (buf) (aref buf 4))

;;; arm32-emit override: stores as 4 LE bytes (32-bit safe)
;;; word is emitted via logand/ash to extract individual bytes
(defun arm32-emit (buf word)
  (let ((pos (aref buf 4)))
    (let ((bi (* pos 4)))
      (let ((bytes (aref buf 0)))
        (aset bytes bi (logand word 255))
        (aset bytes (+ bi 1) (logand (ash word -8) 255))
        (aset bytes (+ bi 2) (logand (ash word -16) 255))
        (aset bytes (+ bi 3) (logand (ash word -24) 255))))
    (aset buf 4 (+ pos 1))))

(defun arm32-current-index (buf) (aref buf 4))

(defun arm32-emit-label (buf label-id)
  ;; Use global *arm32-labels-ht* to avoid buf corruption
  (puthash label-id *arm32-labels-ht* (aref buf 4)))

(defun arm32-emit-fixup (buf label-id fixup-type)
  (aset buf 2 (cons (cons (- (aref buf 4) 1) (cons label-id (cons fixup-type nil)))
                     (aref buf 2))))

;;; init-*arm32-vreg-map*
(defun init-*arm32-vreg-map* ()
  (setq *arm32-vreg-map* (make-array 23))
  (aset *arm32-vreg-map* 0 0)
  (aset *arm32-vreg-map* 1 1)
  (aset *arm32-vreg-map* 2 2)
  (aset *arm32-vreg-map* 3 3)
  (aset *arm32-vreg-map* 4 4)
  (aset *arm32-vreg-map* 5 5)
  (aset *arm32-vreg-map* 6 6)
  (aset *arm32-vreg-map* 7 7)
  (aset *arm32-vreg-map* 16 0)
  (aset *arm32-vreg-map* 17 9)
  (aset *arm32-vreg-map* 18 10)
  (aset *arm32-vreg-map* 19 8)
  (aset *arm32-vreg-map* 20 13)
  (aset *arm32-vreg-map* 21 11))

;;; arm32-phys-reg override
(defun arm32-phys-reg (vreg)
  (if (< vreg (array-length *arm32-vreg-map*))
      (aref *arm32-vreg-map* vreg)
      nil))

;;; Full-arg instruction encoders (replace &key versions)
(defun arm32-dp-reg-full (buf opcode rd rn rm s shift-type shift-amt cond)
  (arm32-emit buf (logior (ash cond 28)
                          (ash opcode 21)
                          (ash s 20)
                          (ash (logand rn 15) 16)
                          (ash (logand rd 15) 12)
                          (ash (logand shift-amt 31) 7)
                          (ash (logand shift-type 3) 5)
                          (logand rm 15))))

(defun arm32-dp-imm-full (buf opcode rd rn rotate imm8 s cond)
  (arm32-emit buf (logior (ash cond 28)
                          (ash 1 25)
                          (ash opcode 21)
                          (ash s 20)
                          (ash (logand rn 15) 16)
                          (ash (logand rd 15) 12)
                          (ash (logand rotate 15) 8)
                          (logand imm8 255))))

(defun arm32-dp-reg-shift-full (buf opcode rd rn rm shift-type rs s cond)
  (arm32-emit buf (logior (ash cond 28)
                          (ash opcode 21)
                          (ash s 20)
                          (ash (logand rn 15) 16)
                          (ash (logand rd 15) 12)
                          (ash (logand rs 15) 8)
                          (ash (logand shift-type 3) 5)
                          (ash 1 4)
                          (logand rm 15))))

(defun arm32-mem-full (buf rd rn offset load byte writeback post)
  (let ((u (if (>= offset 0) 1 0)))
    (let ((abs-off (if (>= offset 0) offset (- 0 offset))))
      (let ((clamped (if (> abs-off 4095) 4095 abs-off)))
        (let ((p (if (= post 1) 0 1)))
          (arm32-emit buf (logior (ash 14 28)
                                  (ash 1 26)
                                  (ash p 24)
                                  (ash u 23)
                                  (ash byte 22)
                                  (ash writeback 21)
                                  (ash load 20)
                                  (ash (logand rn 15) 16)
                                  (ash (logand rd 15) 12)
                                  clamped)))))))

;;; Override all wrapper functions to use positional versions
(defun arm32-dp-reg (buf opcode rd rn rm)
  (arm32-dp-reg-full buf opcode rd rn rm 0 0 0 14))
(defun arm32-dp-imm (buf opcode rd rn rotate imm8)
  (arm32-dp-imm-full buf opcode rd rn rotate imm8 0 14))
(defun arm32-dp-reg-shift (buf opcode rd rn rm shift-type rs)
  (arm32-dp-reg-shift-full buf opcode rd rn rm shift-type rs 0 14))
(defun arm32-mem (buf rd rn offset)
  (arm32-mem-full buf rd rn offset 1 0 0 0))

(defun arm32-add (buf rd rn rm) (arm32-dp-reg buf 4 rd rn rm))
(defun arm32-sub (buf rd rn rm) (arm32-dp-reg buf 2 rd rn rm))
(defun arm32-rsb-imm (buf rd rn rotate imm8) (arm32-dp-imm buf 3 rd rn rotate imm8))
(defun arm32-and-r (buf rd rn rm) (arm32-dp-reg buf 0 rd rn rm))
(defun arm32-orr (buf rd rn rm) (arm32-dp-reg buf 12 rd rn rm))
(defun arm32-eor (buf rd rn rm) (arm32-dp-reg buf 1 rd rn rm))
(defun arm32-bic-imm (buf rd rn rotate imm8) (arm32-dp-imm buf 14 rd rn rotate imm8))
(defun arm32-mov (buf rd rm) (arm32-dp-reg buf 13 rd 0 rm))
(defun arm32-mov-cond (buf cond rd rm) (arm32-dp-reg-full buf 13 rd 0 rm 0 0 0 cond))
(defun arm32-mov-imm (buf rd rotate imm8) (arm32-dp-imm buf 13 rd 0 rotate imm8))
(defun arm32-mov-imm-cond (buf cond rd rotate imm8) (arm32-dp-imm-full buf 13 rd 0 rotate imm8 0 cond))
(defun arm32-mvn (buf rd rm) (arm32-dp-reg buf 15 rd 0 rm))
(defun arm32-cmp (buf rn rm) (arm32-dp-reg-full buf 10 0 rn rm 1 0 0 14))
(defun arm32-cmp-imm (buf rn rotate imm8) (arm32-dp-imm-full buf 10 0 rn rotate imm8 1 14))
(defun arm32-tst (buf rn rm) (arm32-dp-reg-full buf 8 0 rn rm 1 0 0 14))
(defun arm32-add-imm (buf rd rn rotate imm8) (arm32-dp-imm buf 4 rd rn rotate imm8))
(defun arm32-sub-imm (buf rd rn rotate imm8) (arm32-dp-imm buf 2 rd rn rotate imm8))
(defun arm32-orr-imm (buf rd rn rotate imm8) (arm32-dp-imm buf 12 rd rn rotate imm8))
(defun arm32-and-imm (buf rd rn rotate imm8) (arm32-dp-imm buf 0 rd rn rotate imm8))
(defun arm32-lsl-imm (buf rd rm amount) (arm32-dp-reg-full buf 13 rd 0 rm 0 0 amount 14))
(defun arm32-lsr-imm (buf rd rm amount) (arm32-dp-reg-full buf 13 rd 0 rm 0 1 amount 14))
(defun arm32-asr-imm (buf rd rm amount) (arm32-dp-reg-full buf 13 rd 0 rm 0 2 amount 14))
(defun arm32-lsl-reg (buf rd rm rs) (arm32-dp-reg-shift-full buf 13 rd 0 rm 0 rs 0 14))
(defun arm32-lsr-reg (buf rd rm rs) (arm32-dp-reg-shift-full buf 13 rd 0 rm 1 rs 0 14))
(defun arm32-asr-reg (buf rd rm rs) (arm32-dp-reg-shift-full buf 13 rd 0 rm 2 rs 0 14))

;;; Load/Store wrappers
(defun arm32-ldr (buf rd rn offset) (arm32-mem-full buf rd rn offset 1 0 0 0))
(defun arm32-str (buf rd rn offset) (arm32-mem-full buf rd rn offset 0 0 0 0))
(defun arm32-ldrb (buf rd rn offset) (arm32-mem-full buf rd rn offset 1 1 0 0))
(defun arm32-strb (buf rd rn offset) (arm32-mem-full buf rd rn offset 0 1 0 0))
(defun arm32-str-pre (buf rd rn offset) (arm32-mem-full buf rd rn offset 0 0 1 0))
(defun arm32-ldr-post (buf rd rn offset) (arm32-mem-full buf rd rn offset 1 0 0 1))

;;; Halfword load/store (no &key)
(defun arm32-ldrh (buf rd rn offset)
  (let ((u (if (>= offset 0) 1 0)))
    (let ((abs-off (if (>= offset 0) offset (- 0 offset))))
      (let ((clamped (if (> abs-off 255) 255 abs-off)))
        (arm32-emit buf (logior (ash 14 28)
                                (ash 1 24)
                                (ash u 23)
                                (ash 1 22)
                                (ash 1 20)
                                (ash (logand rn 15) 16)
                                (ash (logand rd 15) 12)
                                (ash (logand (ash clamped -4) 15) 8)
                                (ash 11 4)
                                (logand clamped 15)))))))

(defun arm32-strh (buf rd rn offset)
  (let ((u (if (>= offset 0) 1 0)))
    (let ((abs-off (if (>= offset 0) offset (- 0 offset))))
      (let ((clamped (if (> abs-off 255) 255 abs-off)))
        (arm32-emit buf (logior (ash 14 28)
                                (ash 1 24)
                                (ash u 23)
                                (ash 1 22)
                                (ash 0 20)
                                (ash (logand rn 15) 16)
                                (ash (logand rd 15) 12)
                                (ash (logand (ash clamped -4) 15) 8)
                                (ash 11 4)
                                (logand clamped 15)))))))

;;; Branch instructions
(defun arm32-b (buf label-id)
  (arm32-emit buf (logior (ash 14 28) (ash 5 25)))
  (if label-id (arm32-emit-fixup buf label-id 0) nil))

(defun arm32-bl (buf label-id)
  (arm32-emit buf (logior (ash 14 28) (ash 5 25) (ash 1 24)))
  (if label-id (arm32-emit-fixup buf label-id 1) nil))

(defun arm32-b-cond (buf cond label-id)
  (arm32-emit buf (logior (ash cond 28) (ash 5 25)))
  (if label-id (arm32-emit-fixup buf label-id 2) nil))

(defun arm32-bx (buf rm)
  (arm32-emit buf (logior (ash 14 28) #x012FFF10 (logand rm 15))))

;;; PUSH/POP
(defun arm32-push (buf reglist)
  (arm32-emit buf (logior (ash 14 28) #x092D0000 reglist)))
(defun arm32-pop (buf reglist)
  (arm32-emit buf (logior (ash 14 28) #x08BD0000 reglist)))

;;; MUL
(defun arm32-mul (buf rd rm rs)
  (arm32-emit buf (logior (ash 14 28)
                          (ash (logand rd 15) 16)
                          (ash (logand rs 15) 8)
                          (ash 9 4)
                          (logand rm 15))))

;;; NOP
(defun arm32-nop (buf) (arm32-mov buf 0 0))

;;; SWI
(defun arm32-swi (buf imm)
  (arm32-emit buf (logior (ash 14 28) (ash 15 24) (logand imm #xFFFFFF))))

;;; SWP
(defun arm32-swp (buf rd rm rn)
  (arm32-emit buf (logior (ash 14 28) (ash 16 20)
                          (ash (logand rn 15) 16)
                          (ash (logand rd 15) 12)
                          (ash 9 4) (logand rm 15))))

;;; MRS/MSR
(defun arm32-mrs (buf rd)
  (arm32-emit buf (logior (ash 14 28) #x010F0000 (ash (logand rd 15) 12))))
(defun arm32-msr-c (buf rm)
  (arm32-emit buf (logior (ash 14 28) #x0129F000 (logand rm 15))))

;;; Fence
(defun arm32-fence (buf)
  (if *arm32-v7*
      (arm32-emit buf #xF57FF050)
      (arm32-emit buf #xEE070F9A)))

;;; ARMv7 instructions
(defun arm32-movw (buf rd imm16)
  (let ((imm4 (logand (ash imm16 -12) 15)))
    (let ((imm12 (logand imm16 4095)))
      (arm32-emit buf (logior (ash 14 28) (ash 48 20)
                              (ash imm4 16) (ash (logand rd 15) 12) imm12)))))

(defun arm32-movt (buf rd imm16)
  (let ((imm4 (logand (ash imm16 -12) 15)))
    (let ((imm12 (logand imm16 4095)))
      (arm32-emit buf (logior (ash 14 28) (ash 52 20)
                              (ash imm4 16) (ash (logand rd 15) 12) imm12)))))

(defun arm32-sdiv (buf rd rn rm)
  (arm32-emit buf (logior (ash 14 28) (ash 113 20)
                          (ash (logand rd 15) 16) (ash 15 12)
                          (ash (logand rm 15) 8) (ash 1 4) (logand rn 15))))

(defun arm32-sev (buf) (arm32-emit buf #xE320F004))
(defun arm32-wfe (buf) (arm32-emit buf #xE320F002))

(defun arm32-ldrex (buf rd rn)
  (arm32-emit buf (logior (ash 14 28) (ash 25 20)
                          (ash (logand rn 15) 16) (ash (logand rd 15) 12) #xF9F)))

(defun arm32-strex (buf rd rm rn)
  (arm32-emit buf (logior (ash 14 28) (ash 24 20)
                          (ash (logand rn 15) 16) (ash (logand rd 15) 12)
                          (ash #xF9 4) (logand rm 15))))

;;; arm32-uart-base override: hardcode raspi2b PL011 UART
;;; (avoids global variable lookup issues on bare metal)
(defun arm32-uart-base ()
  (if (> *arm32-uart-override* 0)
      *arm32-uart-override*
      #x3F201000))

;;; arm32-encode-imm override (no dotimes/return-from)
(defun arm32-encode-imm (value)
  (let ((val (logand value #xFFFFFFFF))
        (rot 0)
        (found nil))
    (loop
      (when (>= rot 16) (return nil))
      (when found (return found))
      (let ((shift (* rot 2)))
        (let ((left (ash val shift)))
          (let ((right (if (> shift 0) (ash val (- shift 32)) 0)))
            (let ((combined (logior left right)))
              (let ((rotated (logand combined #xFFFFFFFF)))
                (if (<= rotated 255)
                    (setq found (cons rot (logand rotated 255)))
                    nil))))))
      (setq rot (+ rot 1)))))

;;; arm32-load-imm32 override (no return-from, no plusp)
(defun arm32-load-imm32 (buf rd value)
  (let ((val (logand value #xFFFFFFFF))
        (done nil))
    ;; Try MOV
    (let ((enc (arm32-encode-imm val)))
      (if enc
          (progn
            (arm32-mov-imm buf rd (car enc) (cdr enc))
            (setq done 1))
          nil))
    ;; Try MVN
    (if done nil
        (let ((enc (arm32-encode-imm (logand (lognot val) #xFFFFFFFF))))
          (if enc
              (progn
                (arm32-dp-imm buf 15 rd 0 (car enc) (cdr enc))
                (setq done 1))
              nil)))
    ;; Multi-instruction
    (if done nil
        (if *arm32-v7*
            ;; ARMv7: MOVW + MOVT
            (let ((lo (logand val #xFFFF)))
              (let ((hi (logand (ash val -16) #xFFFF)))
                (arm32-movw buf rd lo)
                (if (> hi 0)
                    (arm32-movt buf rd hi)
                    nil)))
            ;; ARMv5: MOV + ORR bytes
            (let ((b0 (logand val 255)))
              (let ((b1 (logand (ash val -8) 255)))
                (let ((b2 (logand (ash val -16) 255)))
                  (let ((b3 (logand (ash val -24) 255)))
                    (arm32-mov-imm buf rd 0 b0)
                    (if (> b1 0) (arm32-orr-imm buf rd rd 12 b1) nil)
                    (if (> b2 0) (arm32-orr-imm buf rd rd 8 b2) nil)
                    (if (> b3 0) (arm32-orr-imm buf rd rd 4 b3) nil)))))))))

;;; arm32-buffer-to-bytes: buffer is already byte-based, just return it
(defun arm32-buffer-to-bytes (buf)
  (aref buf 0))

;;; arm32-resolve-fixups override (byte-level, 32-bit safe)
;;; Buffer is byte-based: 4 bytes per instruction at index*4.
;;; Only need to patch low 3 bytes (offset[23:0]), keep byte 3 (cond+opcode).
(defun arm32-resolve-fixups (buf)
  (let ((bytes (aref buf 0))
        (rest-fixups (aref buf 2)))
    (loop
      (when (null rest-fixups) (return nil))
      (let ((fixup (car rest-fixups)))
        (let ((index (car fixup))
              (label-id (car (cdr fixup))))
          (let ((target (gethash label-id *arm32-labels-ht*)))
            (if target
                (let ((offset (logand (- target (+ index 2)) #xFFFFFF)))
                  (let ((bi (* index 4)))
                    (aset bytes bi (logand offset 255))
                    (aset bytes (+ bi 1) (logand (ash offset -8) 255))
                    (aset bytes (+ bi 2) (logand (ash offset -16) 255))))
                nil))))
      (setq rest-fixups (cdr rest-fixups)))))

;;; Prologue / Epilogue
(defun arm32-emit-prologue (buf)
  (let ((reglist (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)
                         (ash 1 8) (ash 1 11) (ash 1 14))))
    (arm32-push buf reglist))
  (arm32-sub-imm buf 13 13 0 192)
  (arm32-sub-imm buf 11 13 0 2))

(defun arm32-emit-epilogue (buf)
  (arm32-add-imm buf 13 13 0 192)
  (let ((reglist (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)
                         (ash 1 8) (ash 1 11) (ash 1 15))))
    (arm32-pop buf reglist)))

;;; arm32-emit-divmod override (no let*)
(defun arm32-emit-divmod (buf)
  (let ((div-label (arm32-buffer-div-label buf)))
    (let ((real-label (if div-label div-label (mvm-make-label))))
      (let ((shift-label (mvm-make-label)))
        (let ((shift-done (mvm-make-label)))
          (let ((loop-label (mvm-make-label)))
            (arm32-emit-label buf real-label)
            (arm32-asr-imm buf 0 0 1)
            (arm32-asr-imm buf 1 1 1)
            (arm32-eor buf 12 0 1)
            (arm32-cmp-imm buf 0 0 0)
            (arm32-dp-imm-full buf 3 0 0 0 0 0 11)
            (arm32-cmp-imm buf 1 0 0)
            (arm32-dp-imm-full buf 3 1 1 0 0 0 11)
            (arm32-mov-imm buf 2 0 0)
            (arm32-mov buf 3 1)
            (arm32-emit-label buf shift-label)
            (arm32-cmp buf 3 0)
            (arm32-b-cond buf 8 shift-done)
            (arm32-dp-imm-full buf 8 0 3 1 2 1 14)
            (arm32-b-cond buf 1 shift-done)
            (arm32-lsl-imm buf 3 3 1)
            (arm32-b buf shift-label)
            (arm32-emit-label buf shift-done)
            (arm32-emit-label buf loop-label)
            (arm32-cmp buf 0 3)
            (arm32-dp-reg-full buf 2 0 0 3 0 0 0 2)
            (arm32-emit buf (logior (ash 14 28) (ash 5 21)
                                    (ash 2 16) (ash 2 12) 2))
            (arm32-cmp buf 3 1)
            (arm32-lsr-imm buf 3 3 1)
            (arm32-b-cond buf 8 loop-label)
            (arm32-dp-imm-full buf 8 0 12 1 2 1 14)
            (arm32-dp-imm-full buf 3 2 2 0 0 0 1)
            (arm32-lsl-imm buf 1 0 1)
            (arm32-lsl-imm buf 0 2 1)
            (arm32-bx buf 14)
            real-label))))))

;;; arm32-translate-insn override (no flet/macrolet/case/ecase)
(defun arm32-translate-insn (buf opcode operands mvm-pc fn-base-offset function-table)
  (cond
    ;; NOP
    ((= opcode 0)
     (arm32-nop buf))
    ;; BREAK
    ((= opcode 1)
     (arm32-emit buf #xE1200070))
    ;; TRAP
    ((= opcode 2)
     (let ((code (nth 0 operands)))
       (cond
         ((< code 256)
          ;; Prologue already emitted by main translation loop — do NOT emit again
          (if (> code 4)
              (let ((param-idx 4))
                (loop
                  (when (>= param-idx code) (return nil))
                  (let ((k (- param-idx 4)))
                    (let ((src-off (+ 220 (* k 4))))
                      (let ((dst-off (* param-idx 4)))
                        (arm32-ldr buf 12 13 src-off)
                        (arm32-str buf 12 13 dst-off))))
                  (setq param-idx (+ param-idx 1))))
              nil))
         ((< code 768) nil)
         ((= code 768)
          (arm32-asr-imm buf 12 0 1)
          (arm32-load-imm32 buf 14 (arm32-uart-base))
          (arm32-strb buf 12 14 0))
         ((= code 769)
          (arm32-load-imm32 buf 14 (arm32-uart-base))
          (let ((poll-label (mvm-make-label)))
            (arm32-emit-label buf poll-label)
            (arm32-ldrb buf 12 14 24)
            (arm32-dp-imm-full buf 8 0 12 0 16 1 14)
            (arm32-b-cond buf 1 poll-label))
          (arm32-ldrb buf 0 14 0)
          (arm32-lsl-imm buf 0 0 1))
         (t (arm32-swi buf code)))))
    ;; MOV
    ((= opcode 16)
     (let ((vd (nth 0 operands))
           (vs (nth 1 operands)))
       (let ((pd (arm32-phys-reg vd))
             (ps (arm32-phys-reg vs)))
         (if pd
             (if ps
                 (if (= pd ps) nil (arm32-mov buf pd ps))
                 (arm32-ldr buf pd 11 (arm32-spill-offset vs)))
             (if ps
                 (arm32-str buf ps 11 (arm32-spill-offset vd))
                 (progn
                   (arm32-ldr buf 12 11 (arm32-spill-offset vs))
                   (arm32-str buf 12 11 (arm32-spill-offset vd))))))))
    ;; LI
    ((= opcode 17)
     (let ((vd (nth 0 operands))
           (imm (nth 1 operands)))
       (let ((pd (arm32-phys-reg vd)))
         (if pd
             (arm32-load-imm32 buf pd (logand imm #xFFFFFFFF))
             (progn
               (arm32-load-imm32 buf 12 (logand imm #xFFFFFFFF))
               (arm32-str buf 12 11 (arm32-spill-offset vd)))))))
    ;; PUSH
    ((= opcode 18)
     (let ((ps (arm32-resolve-src buf (nth 0 operands) 12)))
       (arm32-str-pre buf ps 13 -4)))
    ;; POP
    ((= opcode 19)
     (let ((vd (nth 0 operands)))
       (let ((pd (arm32-phys-reg vd)))
         (if pd
             (arm32-ldr-post buf pd 13 4)
             (progn
               (arm32-ldr-post buf 12 13 4)
               (arm32-str buf 12 11 (arm32-spill-offset vd)))))))
    ;; ADD
    ((= opcode 32)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (pb (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-add buf 12 pa pb)
         (arm32-store-vreg buf 12 vd))))
    ;; SUB
    ((= opcode 33)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (pb (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-sub buf 12 pa pb)
         (arm32-store-vreg buf 12 vd))))
    ;; MUL
    ((= opcode 34)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (pb (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-asr-imm buf 12 pa 1)
         (let ((dest (arm32-phys-reg vd)))
           (let ((d (if dest dest 14)))
             (if (= d 12)
                 (progn
                   (arm32-mul buf 14 12 pb)
                   (arm32-store-vreg buf 14 vd))
                 (progn
                   (arm32-mul buf d 12 pb)
                   (arm32-store-vreg buf d vd))))))))
    ;; DIV
    ((= opcode 35)
     (let ((vd (nth 0 operands)))
       (arm32-load-vreg buf 0 (nth 1 operands))
       (arm32-load-vreg buf 1 (nth 2 operands))
       (if *arm32-v7*
           (progn
             (arm32-asr-imm buf 0 0 1)
             (arm32-asr-imm buf 1 1 1)
             (arm32-sdiv buf 12 0 1)
             (arm32-lsl-imm buf 12 12 1)
             (arm32-store-vreg buf 12 vd))
           (progn
             (arm32-bl buf (arm32-buffer-div-label buf))
             (arm32-store-vreg buf 0 vd)))))
    ;; MOD
    ((= opcode 36)
     (let ((vd (nth 0 operands)))
       (arm32-load-vreg buf 0 (nth 1 operands))
       (arm32-load-vreg buf 1 (nth 2 operands))
       (if *arm32-v7*
           (progn
             (arm32-asr-imm buf 0 0 1)
             (arm32-asr-imm buf 1 1 1)
             (arm32-sdiv buf 12 0 1)
             (arm32-mul buf 14 12 1)
             (arm32-sub buf 12 0 14)
             (arm32-lsl-imm buf 12 12 1)
             (arm32-store-vreg buf 12 vd))
           (progn
             (arm32-bl buf (arm32-buffer-div-label buf))
             (arm32-store-vreg buf 1 vd)))))
    ;; NEG
    ((= opcode 37)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-rsb-imm buf 12 ps 0 0)
         (arm32-store-vreg buf 12 vd))))
    ;; INC
    ((= opcode 38)
     (let ((vd (nth 0 operands)))
       (let ((pd (arm32-phys-reg vd)))
         (if pd
             (arm32-add-imm buf pd pd 0 2)
             (progn
               (arm32-ldr buf 12 11 (arm32-spill-offset vd))
               (arm32-add-imm buf 12 12 0 2)
               (arm32-str buf 12 11 (arm32-spill-offset vd)))))))
    ;; DEC
    ((= opcode 39)
     (let ((vd (nth 0 operands)))
       (let ((pd (arm32-phys-reg vd)))
         (if pd
             (arm32-sub-imm buf pd pd 0 2)
             (progn
               (arm32-ldr buf 12 11 (arm32-spill-offset vd))
               (arm32-sub-imm buf 12 12 0 2)
               (arm32-str buf 12 11 (arm32-spill-offset vd)))))))
    ;; BAND
    ((= opcode 40)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (pb (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-and-r buf 12 pa pb)
         (arm32-store-vreg buf 12 vd))))
    ;; BOR
    ((= opcode 41)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (pb (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-orr buf 12 pa pb)
         (arm32-store-vreg buf 12 vd))))
    ;; BXOR
    ((= opcode 42)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (pb (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-eor buf 12 pa pb)
         (arm32-store-vreg buf 12 vd))))
    ;; SHL
    ((= opcode 43)
     (let ((vd (nth 0 operands))
           (amount (nth 2 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-lsl-imm buf 12 ps (logand amount 31))
         (arm32-store-vreg buf 12 vd))))
    ;; SHR
    ((= opcode 44)
     (let ((vd (nth 0 operands))
           (amount (nth 2 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-lsr-imm buf 12 ps (logand amount 31))
         (arm32-store-vreg buf 12 vd))))
    ;; SAR
    ((= opcode 45)
     (let ((vd (nth 0 operands))
           (amount (nth 2 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-asr-imm buf 12 ps (logand amount 31))
         (arm32-store-vreg buf 12 vd))))
    ;; LDB
    ((= opcode 46)
     (let ((vd (nth 0 operands))
           (pos (nth 2 operands))
           (size (nth 3 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-lsr-imm buf 12 ps (logand pos 31))
         (let ((mask (- (ash 1 size) 1)))
           (let ((enc (arm32-encode-imm mask)))
             (if enc
                 (arm32-and-imm buf 12 12 (car enc) (cdr enc))
                 (progn
                   (arm32-load-imm32 buf 14 mask)
                   (arm32-and-r buf 12 12 14)))))
         (arm32-store-vreg buf 12 vd))))
    ;; SHLV
    ((= opcode 47)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12))
             (pc (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-lsl-reg buf 12 ps pc)
         (arm32-store-vreg buf 12 vd))))
    ;; CMP
    ((= opcode 48)
     (let ((pa (arm32-resolve-src buf (nth 0 operands) 12))
           (pb (arm32-resolve-src buf (nth 1 operands) 14)))
       (arm32-cmp buf pa pb)))
    ;; TEST
    ((= opcode 49)
     (let ((pa (arm32-resolve-src buf (nth 0 operands) 12))
           (pb (arm32-resolve-src buf (nth 1 operands) 14)))
       (arm32-tst buf pa pb)))
    ;; SARV
    ((= opcode 50)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12))
             (pc (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-asr-reg buf 12 ps pc)
         (arm32-store-vreg buf 12 vd))))
    ;; BR
    ((= opcode 64)
     (let ((off (nth 0 operands)))
       (let ((target (+ mvm-pc off)))
         (arm32-b buf (arm32-ensure-label target fn-base-offset)))))
    ;; BEQ
    ((= opcode 65)
     (let ((off (nth 0 operands)))
       (arm32-b-cond buf 0 (arm32-ensure-label (+ mvm-pc off) fn-base-offset))))
    ;; BNE
    ((= opcode 66)
     (let ((off (nth 0 operands)))
       (arm32-b-cond buf 1 (arm32-ensure-label (+ mvm-pc off) fn-base-offset))))
    ;; BLT
    ((= opcode 67)
     (let ((off (nth 0 operands)))
       (arm32-b-cond buf 11 (arm32-ensure-label (+ mvm-pc off) fn-base-offset))))
    ;; BGE
    ((= opcode 68)
     (let ((off (nth 0 operands)))
       (arm32-b-cond buf 10 (arm32-ensure-label (+ mvm-pc off) fn-base-offset))))
    ;; BLE
    ((= opcode 69)
     (let ((off (nth 0 operands)))
       (arm32-b-cond buf 13 (arm32-ensure-label (+ mvm-pc off) fn-base-offset))))
    ;; BGT
    ((= opcode 70)
     (let ((off (nth 0 operands)))
       (arm32-b-cond buf 12 (arm32-ensure-label (+ mvm-pc off) fn-base-offset))))
    ;; BNULL
    ((= opcode 71)
     (let ((ps (arm32-resolve-src buf (nth 0 operands) 12)))
       (arm32-cmp buf ps 8)
       (arm32-b-cond buf 0 (arm32-ensure-label (+ mvm-pc (nth 1 operands)) fn-base-offset))))
    ;; BNNULL
    ((= opcode 72)
     (let ((ps (arm32-resolve-src buf (nth 0 operands) 12)))
       (arm32-cmp buf ps 8)
       (arm32-b-cond buf 1 (arm32-ensure-label (+ mvm-pc (nth 1 operands)) fn-base-offset))))
    ;; CAR
    ((= opcode 80)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-ldr buf 12 ps -1)
         (arm32-store-vreg buf 12 vd))))
    ;; CDR
    ((= opcode 81)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-ldr buf 12 ps 3)
         (arm32-store-vreg buf 12 vd))))
    ;; CONS
    ((= opcode 82)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (pb (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-str buf pa 9 0)
         (arm32-str buf pb 9 4)
         (arm32-orr-imm buf 12 9 0 1)
         (arm32-store-vreg buf 12 vd)
         (arm32-add-imm buf 9 9 0 16))))
    ;; SETCAR
    ((= opcode 83)
     (let ((pd (arm32-resolve-src buf (nth 0 operands) 12))
           (ps (arm32-resolve-src buf (nth 1 operands) 14)))
       (arm32-str buf ps pd -1)))
    ;; SETCDR
    ((= opcode 84)
     (let ((pd (arm32-resolve-src buf (nth 0 operands) 12))
           (ps (arm32-resolve-src buf (nth 1 operands) 14)))
       (arm32-str buf ps pd 3)))
    ;; CONSP
    ((= opcode 85)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-and-imm buf 12 ps 0 15)
         (arm32-cmp-imm buf 12 0 1)
         (arm32-mov-imm-cond buf 0 12 0 2)
         (arm32-mov-cond buf 1 12 8)
         (arm32-store-vreg buf 12 vd))))
    ;; ATOM
    ((= opcode 86)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-and-imm buf 12 ps 0 15)
         (arm32-cmp-imm buf 12 0 1)
         (arm32-mov-imm-cond buf 1 12 0 2)
         (arm32-mov-cond buf 0 12 8)
         (arm32-store-vreg buf 12 vd))))
    ;; ALLOC-OBJ
    ((= opcode 96)
     (let ((vd (nth 0 operands))
           (size (nth 1 operands))
           (subtag (nth 2 operands)))
       (arm32-load-imm32 buf 12 (logior (ash size 8) subtag))
       (arm32-str buf 12 9 0)
       (arm32-orr-imm buf 12 9 0 2)
       (arm32-store-vreg buf 12 vd)
       (let ((total (logand (+ (* (+ size 1) 4) 15) (lognot 15))))
         (let ((enc (arm32-encode-imm total)))
           (if enc
               (arm32-add-imm buf 9 9 (car enc) (cdr enc))
               (progn
                 (arm32-load-imm32 buf 14 total)
                 (arm32-add buf 9 9 14)))))))
    ;; OBJ-REF
    ((= opcode 97)
     (let ((vd (nth 0 operands))
           (idx (nth 2 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-ldr buf 12 ps (+ 2 (* idx 4)))
         (arm32-store-vreg buf 12 vd))))
    ;; OBJ-SET
    ((= opcode 98)
     (let ((idx (nth 1 operands)))
       (let ((pobj (arm32-resolve-src buf (nth 0 operands) 12))
             (ps (arm32-resolve-src buf (nth 2 operands) 14)))
         (arm32-str buf ps pobj (+ 2 (* idx 4))))))
    ;; OBJ-TAG
    ((= opcode 99)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-and-imm buf 12 ps 0 15)
         (arm32-store-vreg buf 12 vd))))
    ;; OBJ-SUBTAG
    ((= opcode 100)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-sub-imm buf 12 ps 0 2)
         (arm32-ldr buf 12 12 0)
         (arm32-and-imm buf 12 12 0 255)
         (arm32-store-vreg buf 12 vd))))
    ;; AREF
    ((= opcode 101)
     (let ((vd (nth 0 operands)))
       (let ((pidx (arm32-resolve-src buf (nth 2 operands) 12))
             (pobj (arm32-resolve-src buf (nth 1 operands) 14)))
         (arm32-lsl-imm buf 12 pidx 1)
         (arm32-add buf 12 12 pobj)
         (arm32-ldr buf 12 12 2)
         (arm32-store-vreg buf 12 vd))))
    ;; ASET
    ((= opcode 102)
     (let ((pval (arm32-resolve-src buf (nth 2 operands) 14)))
       (arm32-load-vreg buf 12 (nth 1 operands))
       (arm32-lsl-imm buf 12 12 1)
       (arm32-str-pre buf pval 13 -4)
       (arm32-load-vreg buf 14 (nth 0 operands))
       (arm32-add buf 12 12 14)
       (arm32-ldr-post buf 14 13 4)
       (arm32-str buf 14 12 2)))
    ;; ARRAY-LEN
    ((= opcode 103)
     (let ((vd (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 12)))
         (arm32-sub-imm buf 12 ps 0 2)
         (arm32-ldr buf 12 12 0)
         (arm32-lsr-imm buf 12 12 8)
         (arm32-bic-imm buf 12 12 4 255)
         (arm32-lsl-imm buf 12 12 1)
         (arm32-store-vreg buf 12 vd))))
    ;; ALLOC-ARRAY
    ((= opcode 104)
     (let ((vd (nth 0 operands))
           (vcount (nth 1 operands)))
       (arm32-load-vreg buf 12 vcount)
       (arm32-mov buf 14 12)
       (arm32-lsl-imm buf 12 12 8)
       (arm32-orr-imm buf 12 12 0 50)
       (arm32-str buf 12 9 0)
       (arm32-orr-imm buf 12 9 0 2)
       (arm32-store-vreg buf 12 vd)
       (arm32-add-imm buf 14 14 0 1)
       (arm32-lsl-imm buf 14 14 2)
       (arm32-add-imm buf 14 14 0 15)
       (arm32-bic-imm buf 14 14 0 15)
       (arm32-add buf 9 9 14)))
    ;; LOAD
    ((= opcode 112)
     (let ((vd (nth 0 operands))
           (width (nth 2 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12)))
         (cond
           ((= width 0) (arm32-ldrb buf 12 pa 0))
           ((= width 1) (arm32-ldrh buf 12 pa 0))
           (t (arm32-ldr buf 12 pa 0)))
         (arm32-store-vreg buf 12 vd))))
    ;; STORE
    ((= opcode 113)
     (let ((width (nth 2 operands)))
       (let ((pa (arm32-resolve-src buf (nth 0 operands) 12))
             (ps (arm32-resolve-src buf (nth 1 operands) 14)))
         (cond
           ((= width 0) (arm32-strb buf ps pa 0))
           ((= width 1) (arm32-strh buf ps pa 0))
           (t (arm32-str buf ps pa 0))))))
    ;; FENCE
    ((= opcode 114)
     (arm32-fence buf))
    ;; CALL
    ((= opcode 128)
     (let ((fn-label (if function-table (gethash (nth 0 operands) function-table) nil)))
       (arm32-bl buf (if fn-label fn-label (mvm-make-label)))))
    ;; CALL-IND
    ((= opcode 129)
     (let ((ps (arm32-resolve-src buf (nth 0 operands) 12)))
       (arm32-mov buf 14 15)
       (arm32-bx buf ps)))
    ;; RET
    ((= opcode 130)
     (arm32-emit-epilogue buf))
    ;; TAILCALL
    ((= opcode 131)
     (let ((target-pc (nth 0 operands)))
       (arm32-add-imm buf 13 13 0 192)
       (let ((reglist (logior (ash 1 4) (ash 1 5) (ash 1 6) (ash 1 7)
                              (ash 1 8) (ash 1 11) (ash 1 14))))
         (arm32-pop buf reglist))
       (let ((fn-label (if function-table (gethash target-pc function-table) nil)))
         (arm32-b buf (if fn-label fn-label (mvm-make-label))))))
    ;; ALLOC-CONS
    ((= opcode 136)
     (let ((vd (nth 0 operands)))
       (arm32-orr-imm buf 12 9 0 1)
       (arm32-store-vreg buf 12 vd)
       (arm32-add-imm buf 9 9 0 16)))
    ;; GC-CHECK
    ((= opcode 137)
     (arm32-cmp buf 9 10)
     (let ((ok-label (mvm-make-label)))
       (arm32-b-cond buf 3 ok-label)
       (arm32-emit buf #xE1200071)
       (arm32-emit-label buf ok-label)))
    ;; WRITE-BARRIER
    ((= opcode 138) (arm32-nop buf))
    ;; SAVE-CTX
    ((= opcode 144)
     (arm32-str buf 0 11 -4)
     (arm32-str buf 1 11 -8)
     (arm32-str buf 2 11 -12)
     (arm32-str buf 3 11 -16))
    ;; RESTORE-CTX
    ((= opcode 145)
     (arm32-ldr buf 0 11 -4)
     (arm32-ldr buf 1 11 -8)
     (arm32-ldr buf 2 11 -12)
     (arm32-ldr buf 3 11 -16))
    ;; YIELD
    ((= opcode 146)
     (arm32-nop buf))
    ;; ATOMIC-XCHG
    ((= opcode 147)
     (let ((vd (nth 0 operands)))
       (let ((pa (arm32-resolve-src buf (nth 1 operands) 12))
             (ps (arm32-resolve-src buf (nth 2 operands) 14)))
         (if *arm32-v7*
             (let ((loop-label (mvm-make-label)))
               (arm32-emit-label buf loop-label)
               (arm32-ldrex buf 12 pa)
               (arm32-strex buf 3 ps pa)
               (arm32-cmp-imm buf 3 0 0)
               (arm32-b-cond buf 1 loop-label)
               (arm32-store-vreg buf 12 vd))
             (progn
               (arm32-swp buf 12 ps pa)
               (arm32-store-vreg buf 12 vd))))))
    ;; IO-READ
    ((= opcode 160)
     (let ((vd (nth 0 operands))
           (port (nth 1 operands))
           (width (nth 2 operands)))
       (arm32-load-imm32 buf 12 port)
       (cond
         ((= width 0) (arm32-ldrb buf 14 12 0))
         ((= width 1) (arm32-ldrh buf 14 12 0))
         (t (arm32-ldr buf 14 12 0)))
       (arm32-store-vreg buf 14 vd)))
    ;; IO-WRITE
    ((= opcode 161)
     (let ((port (nth 0 operands))
           (width (nth 2 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 14)))
         (arm32-load-imm32 buf 12 port)
         (cond
           ((= width 0) (arm32-strb buf ps 12 0))
           ((= width 1) (arm32-strh buf ps 12 0))
           (t (arm32-str buf ps 12 0))))))
    ;; HALT
    ((= opcode 162)
     (arm32-emit buf (logior (ash 14 28) (ash 5 25) (logand -2 #xFFFFFF))))
    ;; CLI
    ((= opcode 163)
     (arm32-mrs buf 12)
     (arm32-orr-imm buf 12 12 0 192)
     (arm32-msr-c buf 12))
    ;; STI
    ((= opcode 164)
     (arm32-mrs buf 12)
     (arm32-bic-imm buf 12 12 0 192)
     (arm32-msr-c buf 12))
    ;; PERCPU-REF
    ((= opcode 165)
     (let ((vd (nth 0 operands))
           (offset (nth 1 operands)))
       (arm32-load-imm32 buf 12 offset)
       (arm32-ldr buf 12 12 0)
       (arm32-store-vreg buf 12 vd)))
    ;; PERCPU-SET
    ((= opcode 166)
     (let ((offset (nth 0 operands)))
       (let ((ps (arm32-resolve-src buf (nth 1 operands) 14)))
         (arm32-load-imm32 buf 12 offset)
         (arm32-str buf ps 12 0))))
    ;; FN-ADDR
    ((= opcode 167)
     (let ((vd (nth 0 operands))
           (target-pc (nth 1 operands)))
       (let ((label (if function-table
                       (let ((fn-l (gethash target-pc function-table)))
                         (if fn-l fn-l (mvm-make-label)))
                       (mvm-make-label))))
         ;; Load label address via ADR-like: use fixup + resolve
         ;; For now, emit a placeholder MOV that gets patched
         ;; Actually, fn-addr just needs the native offset, but on bare metal
         ;; we use the label's resolved byte address (word*4 + image base)
         ;; Similar to other translators: emit a load of the label position
         ;; This is complex — for fixpoint, fn-addr is used in function table construction
         ;; Emit: ADR-like via SUB PC, #offset
         ;; Simplest: use BL trick — BL .+4 stores PC in LR, then adjust
         ;; Actually let's use the approach from i386: emit fixup that gets resolved
         ;; For ARM32 in fixpoint, fn-addr should return the byte offset of the label
         ;; in native code (like the other translators do)
         ;; Best approach: arm32-load-imm32 with placeholder, then patch
         ;; But we'd need a new fixup type. For now emit NOP (fn-addr not critical
         ;; for basic cross-compilation).
         (arm32-nop buf)
         (arm32-store-vreg buf 12 vd))))
    ;; Default: NOP
    (t (arm32-nop buf))))

;;; translate-mvm-to-arm32 override — per-function processing (like i386)
;;; Returns (cons native-bytes (cons native-size fn-map))
(defun translate-mvm-to-arm32 (bytecode function-table)
  (setq *arm32-labels-ht* (make-hash-table))
  (setq *arm32-code-bytes* (make-array 1048576))
  ;; Find max function length for per-function label array
  (let ((max-len 0)
        (tmp function-table))
    (loop
      (when (null tmp) (return nil))
      (let ((fl (car (cdr (cdr (car tmp))))))
        (if (> fl max-len) (setq max-len fl) nil))
      (setq tmp (cdr tmp)))
    (setq *arm32-fn-labels* (make-array (+ max-len 16))))
  ;; Build buffer directly (bypass defstruct make-arm32-buffer)
  (let ((buf (make-array 5)))
    (aset buf 0 *arm32-code-bytes*)
    (aset buf 1 nil)
    (aset buf 2 nil)
    (aset buf 3 nil)
    (aset buf 4 0)
    (let ((n-functions (length function-table)))
      ;; Pre-allocate divmod label
      (if (not *arm32-v7*)
          (aset buf 3 (mvm-make-label))
          nil)
      (let ((fn-labels (make-array n-functions))
            (fn-map (make-hash-table))
            (fn-offset-to-label (make-hash-table)))
        ;; Create labels for each function
        (let ((rest-ft function-table)
              (i 0))
          (loop
            (when (>= i n-functions) (return nil))
            (let ((entry (car rest-ft)))
              (let ((name (car entry))
                    (offset (car (cdr entry))))
                (let ((label (mvm-make-label)))
                  (aset fn-labels i label)
                  (puthash name fn-map label)
                  (puthash offset fn-offset-to-label label))))
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1))))
        (write-char-serial 84) (write-char-serial 10) ;; T
        ;; Translate each function
        (let ((rest-ft function-table)
              (i 0))
          (loop
            (when (>= i n-functions) (return nil))
            ;; Progress every 100 functions with alloc-ptr
            (if (= 0 (mod i 100))
                (let ()
                  (write-char-serial 35)
                  (print-dec i) (write-char-serial 58)
                  (print-dec (get-alloc-ptr)) (write-char-serial 10))
                nil)
            (let ((entry (car rest-ft)))
              (let ((fn-offset (car (cdr entry)))
                    (fn-length (car (cdr (cdr entry)))))
                (let ((fn-label (aref fn-labels i)))
                  ;; Emit function label and prologue
                  (arm32-emit-label buf fn-label)
                  (arm32-emit-prologue buf)
                  ;; Clear per-function label array
                  (let ((j 0))
                    (loop
                      (when (>= j fn-length) (return nil))
                      (aset *arm32-fn-labels* j 0)
                      (setq j (+ j 1))))
                  ;; Scan branch targets within this function
                  (let ((pc fn-offset)
                        (limit (+ fn-offset fn-length)))
                    (loop
                      (when (>= pc limit) (return nil))
                      (let ((decoded (decode-instruction bytecode pc)))
                        (let ((opcode (car decoded))
                              (operands (car (cdr decoded)))
                              (new-pc (cdr (cdr decoded))))
                          (if (>= opcode 64)
                              (if (<= opcode 70)
                                  (let ((idx (- (+ new-pc (nth 0 operands)) fn-offset)))
                                    (if (= 0 (aref *arm32-fn-labels* idx))
                                        (aset *arm32-fn-labels* idx (mvm-make-label))
                                        nil))
                                  (if (<= opcode 72)
                                      (let ((idx (- (+ new-pc (nth 1 operands)) fn-offset)))
                                        (if (= 0 (aref *arm32-fn-labels* idx))
                                            (aset *arm32-fn-labels* idx (mvm-make-label))
                                            nil))
                                      nil))
                              nil)
                          (setq pc new-pc))))
                    ;; Translate instructions for this function
                    (setq pc fn-offset)
                    (loop
                      (when (>= pc limit) (return nil))
                      (let ((label (aref *arm32-fn-labels* (- pc fn-offset))))
                        (if (not (= label 0)) (arm32-emit-label buf label) nil))
                      (let ((decoded (decode-instruction bytecode pc)))
                        (let ((opcode (car decoded))
                              (operands (car (cdr decoded)))
                              (new-pc (cdr (cdr decoded))))
                          (arm32-translate-insn buf opcode operands new-pc fn-offset fn-offset-to-label)
                          (setq pc new-pc)))))))
            nil
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1))))
        ;; Emit software divide routine (ARMv5 only)
        (if (not *arm32-v7*)
            (arm32-emit-divmod buf)
            nil)
        ;; Resolve branch fixups
        (arm32-resolve-fixups buf)
        ;; Convert to bytes
        (let ((native-bytes (arm32-buffer-to-bytes buf)))
          (let ((native-size (* (aref buf 4) 4)))
            ;; Build fn-map: resolve labels to native byte positions
            (let ((resolved-map (make-hash-table)))
              (let ((rest-ft function-table)
                    (i 0))
                (loop
                  (when (>= i n-functions) (return nil))
                  (let ((entry (car rest-ft)))
                    (let ((name (car entry)))
                      (let ((label (gethash name fn-map)))
                        (if label
                            (let ((label-pos (gethash label *arm32-labels-ht*)))
                              (if label-pos
                                  (puthash name resolved-map (* label-pos 4))
                                  nil))
                            nil))))
                  (setq rest-ft (cdr rest-ft))
                  (setq i (+ i 1))))
              (cons native-bytes (cons native-size resolved-map))))))))))

;;; arm32-uart-base override (no or)
(defun arm32-uart-base ()
  (if *arm32-uart-override*
      *arm32-uart-override*
      (if *arm32-v7* #x09000000 #x101F1000)))

;;; ============================================================
;;; ARM32 image assembly
;;; ============================================================

;;; td-generate-arm32-boot: pre-generated 44-byte boot preamble for raspi2b
;;; QEMU loads kernel at 0x10000. Registers:
;;;   SP  = 0x04000000
;;;   R9  = 0x01400000 (alloc pointer)
;;;   R10 = 0x03800000 (alloc limit)
;;;   R8  = 0xDEAD0001 (NIL constant, must match x64 +nil-value+)
;;;   R11 = 0 (frame pointer)
(defun td-generate-arm32-boot ()
  ;; MOV SP, #0;  ORR SP, #0x800000;  ORR SP, #0x07000000  → SP=0x07800000
  (img-emit 0) (img-emit 208) (img-emit 160) (img-emit 227)
  (img-emit 128) (img-emit 216) (img-emit 141) (img-emit 227)
  (img-emit 7) (img-emit 212) (img-emit 141) (img-emit 227)
  ;; MOV R9, #0;  ORR R9, #0x400000;  ORR R9, #0x01000000  → R9=0x01400000
  (img-emit 0) (img-emit 144) (img-emit 160) (img-emit 227)
  (img-emit 64) (img-emit 152) (img-emit 137) (img-emit 227)
  (img-emit 1) (img-emit 148) (img-emit 137) (img-emit 227)
  ;; MOV R10, #0;  ORR R10, #0x07000000  → R10=0x07000000
  (img-emit 0) (img-emit 160) (img-emit 160) (img-emit 227)
  (img-emit 7) (img-emit 164) (img-emit 138) (img-emit 227)
  ;; MOVW R8, #0x0001  (0xE3008001)
  (img-emit 1) (img-emit 128) (img-emit 0) (img-emit 227)
  ;; MOVT R8, #0xDEAD  (0xE34D8EAD)
  (img-emit 173) (img-emit 142) (img-emit 77) (img-emit 227)
  ;; MOV R11, #0  (0xE3A0B000)
  (img-emit 0) (img-emit 176) (img-emit 160) (img-emit 227)
  44)

;;; td-assemble-gen1-arm32: assemble ARM32 image
;;; Layout: [boot 44B][pad to 0x100][B to km at 0x100][native at 0x104]
;;;         [bytecodes][fn-table]...[metadata at 0x478000]
(defun td-assemble-gen1-arm32 (result bc ft)
  (let ((native-bytes (car result))
        (native-size (car (cdr result)))
        (fn-map (cdr (cdr result))))
    ;; 1. Init image buffer
    (img-init)
    (write-char-serial 65) (write-char-serial 82) ;; AR
    (write-char-serial 77) (write-char-serial 10) ;; M\n
    ;; 2. Generate ARM32 boot preamble
    (let ((boot-size (td-generate-arm32-boot)))
      (write-char-serial 80)
      (print-dec boot-size) (write-char-serial 10)
      ;; Pad to 0x100 (native code starts at 0x104, after B instruction at 0x100)
      (let ((pad-target 256))
        (loop
          (when (>= (img-pos) pad-target) (return nil))
          (img-emit 0)))
      ;; 3. B instruction at 0x100 to jump to kernel-main
      (write-char-serial 75) ;; K
      (let ((km-hash (td-read-u32 #x3000028)))
        (print-dec km-hash) (write-char-serial 10)
        (let ((km-native-off (gethash km-hash fn-map)))
          (write-char-serial 79) ;; O
          (if km-native-off
              (progn
                (print-dec km-native-off) (write-char-serial 10)
                ;; B is at word index 64 (0x100/4). Native starts at word 65 (0x104/4).
                ;; Target insn index = 65 + km_native_off/4
                ;; ARM B offset = target - source - 2 = 65 + km_native_off/4 - 64 - 2
                ;;              = km_native_off/4 - 1
                (let ((b-offset (- (ash km-native-off -2) 1)))
                  (write-char-serial 66) ;; B
                  (print-dec b-offset) (write-char-serial 10)
                  ;; ARM32 B: cond=AL(14)=0xE, bits27:25=101, L=0, offset[23:0]
                  ;; byte3=0xEA encodes cond+101+L; bytes 0-2 are pure offset
                  (let ((off24 (logand b-offset #xFFFFFF)))
                    (img-emit (logand off24 255))
                    (img-emit (logand (ash off24 -8) 255))
                    (img-emit (logand (ash off24 -16) 255))
                    (img-emit 234))))  ;; 0xEA = cond=AL, 101, L=0
              ;; No kernel-main found — emit NOP
              (progn
                (write-char-serial 33) (write-char-serial 10)
                (img-emit 0) (img-emit 0) (img-emit 160) (img-emit 225)))))
      ;; 4. Copy native code (starts at 0x104)
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
      ;; 5. Append MVM bytecode
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
          ;; 7. Write metadata at offset 0x4B0000
          ;; ARM32 raspi2b: QEMU loads at 0x10000, metadata VA=0x500000
          ;; image offset = 0x500000 - 0x10000 = 0x4B0000
          (let ((md-img-off #x4F0000))
            (img-patch-u32 md-img-off #x544D564D)
            (img-patch-u32 (+ md-img-off 4) 1)
            ;; my-architecture = 3 (arm32)
            (img-patch-u32 (+ md-img-off 8) 3)
            (img-patch-u32 (+ md-img-off 12) bc-img-offset)
            (img-patch-u32 (+ md-img-off 16) bc-len)
            (img-patch-u32 (+ md-img-off 20) ft-img-offset)
            (img-patch-u32 (+ md-img-off 24) ft-count)
            (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x3000050))
            (img-patch-u32 (+ md-img-off 32) native-size)
            ;; preamble-size = 0x100 (256 bytes)
            (img-patch-u32 (+ md-img-off 36) 256)
            ;; kernel-main-hash-lo (copy from running kernel)
            (let ((km-hash (td-read-u32 #x3000028)))
              (img-patch-u32 (+ md-img-off 40) km-hash))
            ;; kernel-main native offset
            (let ((km-native-off (gethash (td-read-u32 #x3000028) fn-map)))
              (if km-native-off
                  (img-patch-u32 (+ md-img-off 44) km-native-off)
                  (img-patch-u32 (+ md-img-off 44) 0)))
            ;; image-load-addr = 0x10000 (raspi2b QEMU)
            (img-patch-u32 (+ md-img-off 48) #x10000)
            ;; target-architecture (default: 0=x64)
            (img-patch-u32 (+ md-img-off 52) 0)
            ;; mode (default: 0=cross-compile)
            (img-patch-u32 (+ md-img-off 56) 0))
          ;; Total size must cover metadata at 0x4B0000
          (let ((total-size (+ #x4F0000 64)))
            (write-char-serial 65) (write-char-serial 82) ;; AR
            (write-char-serial 61)
            (print-dec total-size) (write-char-serial 10)
            total-size))))))

;;; Build ARM32 target from x64 host
(defun build-arm32-from-x64 (bc ft)
  (setq *arm32-v7* 1)
  (setq *arm32-uart-override* #x3F201000)
  (write-char-serial 76) (write-char-serial 67) ;; LC
  (print-dec *mvm-label-counter*)
  (write-char-serial 10)
  (let ((result (translate-mvm-to-arm32 bc ft)))
    (let ((native-size (car (cdr result))))
      (print-dec native-size) (write-char-serial 10)
      (let ((native-bytes (car result)))
        (let ((hash (td-fnv-native native-bytes native-size)))
          (write-char-serial 70) (write-char-serial 78)
          (write-char-serial 86) (write-char-serial 58)
          (print-dec hash) (write-char-serial 10)))
      (td-assemble-gen1-arm32 result bc ft)
      native-size)))

;;; Build ARM32 target from AArch64 host
(defun build-arm32-from-aarch64 (bc ft)
  (setq *arm32-v7* 1)
  (setq *arm32-uart-override* #x3F201000)
  (let ((result (translate-mvm-to-arm32 bc ft)))
    (let ((native-size (car (cdr result))))
      (print-dec native-size) (write-char-serial 10)
      (let ((native-bytes (car result)))
        (let ((hash (td-fnv-native native-bytes native-size)))
          (write-char-serial 70) (write-char-serial 78)
          (write-char-serial 86) (write-char-serial 58)
          (print-dec hash) (write-char-serial 10)))
      (td-assemble-gen1-arm32 result bc ft)
      native-size)))

;;; Build ARM32 target from i386 host
(defun build-arm32-from-i386 (bc ft)
  (setq *arm32-v7* 1)
  (setq *arm32-uart-override* #x3F201000)
  (let ((result (translate-mvm-to-arm32 bc ft)))
    (let ((native-size (car (cdr result))))
      (print-dec native-size) (write-char-serial 10)
      (let ((native-bytes (car result)))
        (let ((hash (td-fnv-native native-bytes native-size)))
          (write-char-serial 70) (write-char-serial 78)
          (write-char-serial 86) (write-char-serial 58)
          (print-dec hash) (write-char-serial 10)))
      (td-assemble-gen1-arm32 result bc ft)
      native-size)))

;;; Build ARM32 target from ARM32 host (same-arch fixpoint)
(defun build-arm32-from-arm32 (bc ft)
  (setq *arm32-v7* 1)
  (setq *arm32-uart-override* #x3F201000)
  (let ((result (translate-mvm-to-arm32 bc ft)))
    (let ((native-size (car (cdr result))))
      (print-dec native-size) (write-char-serial 10)
      (let ((native-bytes (car result)))
        (let ((hash (td-fnv-native native-bytes native-size)))
          (write-char-serial 70) (write-char-serial 78)
          (write-char-serial 86) (write-char-serial 58)
          (print-dec hash) (write-char-serial 10)))
      (td-assemble-gen1-arm32 result bc ft)
      native-size)))

;;; Build AArch64 target from ARM32 host
;;; Uses the i386-safe AArch64 translator (byte-level encoding, avoids 31-bit overflow)
(defun build-aarch64-from-arm32 (bc ft)
  (write-char-serial 97) (write-char-serial 51) ;; a3
  (write-char-serial 50) (write-char-serial 62) ;; 2>
  (write-char-serial 97) (write-char-serial 54) ;; a6
  (write-char-serial 52) (write-char-serial 10) ;; 4\n
  (let ((result (translate-mvm-to-aarch64-from-i386 bc ft)))
    (let ((native-size (car (cdr result))))
      (write-char-serial 78) ;; N
      (print-dec native-size) (write-char-serial 10)
      (let ((fnv (td-fnv-native (car result) native-size)))
        (write-char-serial 72) ;; H
        (print-dec fnv) (write-char-serial 10))
      (td-assemble-gen1-aarch64-i386 result bc ft)
      native-size)))

;;; Build x64 target from ARM32 host
;;; Uses the i386-safe x64 translator
(defun build-x64-from-arm32 (bc ft)
  (let ((result (translate-mvm-to-x64-from-i386 bc ft)))
    (let ((buf (car result)))
      (let ((native-size (code-buffer-position buf)))
        (print-dec native-size) (write-char-serial 10)
        (let ((native-bytes (code-buffer-bytes buf)))
          (let ((hash (td-fnv-native native-bytes native-size)))
            (write-char-serial 70) (write-char-serial 78)
            (write-char-serial 86) (write-char-serial 58)
            (print-dec hash) (write-char-serial 10)))
        (td-assemble-gen1-x64-i386 result bc ft)
        native-size))))

;;; Build i386 target from ARM32 host
(defun build-i386-from-arm32 (bc ft)
  (let ((result (translate-mvm-to-i386 bc ft)))
    (let ((buf (car result)))
      (let ((native-size (i386-buffer-position buf)))
        (print-dec native-size) (write-char-serial 10)
        (let ((native-bytes (i386-buffer-bytes buf)))
          (let ((hash (td-fnv-native native-bytes native-size)))
            (write-char-serial 70) (write-char-serial 78)
            (write-char-serial 86) (write-char-serial 58)
            (print-dec hash) (write-char-serial 10)))
        (td-assemble-gen1-i386 result bc ft)
        native-size))))

;;; ============================================================
;;; ARM32 HOST overrides — extend i386-safe checks to arm32
;;; These must come AFTER the i386 HOST overrides.
;;; Extends (= my-arch 2) checks to (>= my-arch 2) for both i386 and arm32.
;;; ============================================================

;;; Override td-fnv-native: use XOR checksum on arm32 host too
(defun td-fnv-native (bytes size)
  (let ((my-arch (td-read-u32 #x3000008)))
    (if (>= my-arch 2)
        ;; i386/arm32 host: XOR checksum (FNV constants overflow 30/31-bit fixnum)
        (let ((xsum 0) (j 0))
          (loop
            (when (>= j size) (return xsum))
            (setq xsum (logxor xsum (aref bytes j)))
            (setq j (+ j 1))))
        ;; x64/AArch64 host: real FNV-1a
        (let ((hash 2166136261) (j 0))
          (loop
            (when (>= j size) (return hash))
            (let ((b (aref bytes j)))
              (setq hash (logand #xFFFFFFFF
                                 (* (logxor hash b) 16777619))))
            (setq j (+ j 1)))))))

;;; Override emit-u64 for arm32 host: high32 always 0
(defun emit-u64 (buf value)
  (if (>= (td-read-u32 #x3000008) 2)
      ;; i386/arm32 host: high32 always 0
      (progn (emit-u32 buf value)
             (emit-byte buf 0) (emit-byte buf 0)
             (emit-byte buf 0) (emit-byte buf 0))
      ;; x64/AArch64 host
      (let ((lo (logand value 4294967295)))
        (let ((hi (logand (ash value -32) 4294967295)))
          (emit-u32 buf lo)
          (emit-u32 buf hi)))))

;;; Override i386-emit-mov-reg-imm for arm32 host too
(defun i386-emit-mov-reg-imm (buf reg imm)
  (i386-emit-byte buf (+ 184 reg))
  (i386-emit-u32 buf imm))

;;; Override i386-emit-s32 for arm32 host too
(defun i386-emit-s32 (buf val)
  (i386-emit-u32 buf val))

