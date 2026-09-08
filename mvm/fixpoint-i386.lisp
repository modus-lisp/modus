;;; ============================================================
;;; i386 translator bare-metal overrides
;;; ============================================================

;;; Initialize *i386-vreg-map* (vector isn't available on bare metal)
(defun init-*i386-vreg-map* ()
  (setq *i386-vreg-map* (make-array 23))
  ;; V0=ESI(6), V1=EDI(7), V2=nil, V3=nil, V4=EBX(3)
  (aset *i386-vreg-map* 0 6)
  (aset *i386-vreg-map* 1 7)
  ;; V2-V3 = nil (default 0 = spill, checked via i386-vreg-phys)
  (aset *i386-vreg-map* 4 3)
  ;; V5-V15 = nil (spill) — default 0
  ;; VR=EAX(0) — index 16
  (aset *i386-vreg-map* 16 255)
  ;; VA(17), VL(18), VN(19) = nil (spill)
  ;; VSP=ESP(4) — index 20
  (aset *i386-vreg-map* 20 4)
  ;; VFP=EBP(5) — index 21
  (aset *i386-vreg-map* 21 5)
  ;; VPC(22) = nil
  *i386-vreg-map*)

;;; Fix: i386-vreg-phys needs to return nil for spilled regs, not 0.
;;; On bare metal, make-array initializes to 0, but 0=EAX is a valid phys reg.
;;; Override to explicitly check known mappings.
(defun i386-vreg-phys (vreg)
  (cond
    ((= vreg 0) 6)     ;; V0 = ESI
    ((= vreg 1) 7)     ;; V1 = EDI
    ((= vreg 4) 3)     ;; V4 = EBX
    ((= vreg 16) 0)    ;; VR = EAX
    ((= vreg 20) 4)    ;; VSP = ESP
    ((= vreg 21) 5)    ;; VFP = EBP
    (t nil)))

;;; Override i386-emit-jcc — assoc on *i386-cc-codes* doesn't work because
;;; keyword constants from defparameter are different objects than caller keywords.
;;; Use = on tagged name hashes instead.
(defun i386-emit-jcc (buf cc &optional label-id)
  (let ((code (cond
                ((= cc :e)  4)
                ((= cc :ne) 5)
                ((= cc :l)  12)
                ((= cc :ge) 13)
                ((= cc :le) 14)
                ((= cc :g)  15)
                ((= cc :b)  2)
                ((= cc :ae) 3)
                ((= cc :be) 6)
                ((= cc :a)  7)
                ((= cc :z)  4)
                ((= cc :nz) 5)
                ((= cc :s)  8)
                ((= cc :ns) 9)
                (t nil))))
    (when (null code)
      (write-char-serial 74) (write-char-serial 67) ;; JC
      (write-char-serial 67) (write-char-serial 33) ;; C!
      (print-dec cc) (write-char-serial 10)
      (error 99))
    (i386-emit-byte buf 15)
    (i386-emit-byte buf (+ 128 code))
    (if label-id
        (i386-emit-fixup-rel32 buf label-id)
        (i386-emit-u32 buf 0))))

;;; Helper: translate one instruction, reducing nesting depth in main loop
;;; Helper: translate one instruction.
;;; Saves new-pos to 0x300060 for caller (avoids AArch64 return-value clobber).
;;; Handles common opcodes inline to avoid deep cond dispatch in i386-translate-insn.
;;; Emit a LI immediate by reading 4 raw bytes from bytecodes directly.
;;; This avoids decode-u32 which loses bit 31 on i386 (fixnum-shift=1).
(defun i386-emit-raw-imm32 (buf bytecode imm-pos)
  (i386-emit-byte buf (aref bytecode imm-pos))
  (i386-emit-byte buf (aref bytecode (+ imm-pos 1)))
  (i386-emit-byte buf (aref bytecode (+ imm-pos 2)))
  (i386-emit-byte buf (aref bytecode (+ imm-pos 3))))

;;; Handle LI opcode with raw byte emission (i386 fixnum-safe).
;;; LI bytecode format: [opcode:1][reg:1][imm64:8] = 10 bytes total.
(defun i386-translate-li-raw (buf state bytecode pos)
  (let ((new-pos (+ pos 10)))
    (td-write-u32 #x3000060 new-pos)
    (let ((label (gethash pos (i386-translate-state-label-map state))))
      (when label
        (i386-emit-label buf label)))
    (td-write-u32 #x300004C 17)
    (td-write-u32 #x3000048 pos)
    (let ((vd (logand (aref bytecode (+ pos 1)) 31)))
      (let ((imm-pos (+ pos 2)))
        (cond
          ((i386-has-phys vd)
           ;; MOV physical-reg, imm32: opcode B8+reg, then 4 LE bytes
           (i386-emit-byte buf (+ 184 (i386-vreg-phys vd)))
           (i386-emit-raw-imm32 buf bytecode imm-pos))
          ((i386-vreg-abs-addr vd)
           ;; MOV scratch0(ECX), imm32; then MOV [abs-addr], ECX
           (let ((abs-addr (i386-vreg-abs-addr vd)))
             (i386-emit-byte buf (+ 184 1))  ;; B8+ECX
             (i386-emit-raw-imm32 buf bytecode imm-pos)
             (i386-emit-mov-abs-reg buf abs-addr 1)))
          (t
           ;; MOV DWORD [EBP + spill-offset], imm32
           (i386-emit-byte buf 199)  ;; C7 opcode
           (i386-emit-modrm-mem buf 0 5 (i386-spill-offset vd))
           (i386-emit-raw-imm32 buf bytecode imm-pos)))))
    new-pos))

(defun i386-translate-one-insn (buf state bytecode pos)
  ;; Check for LI opcode (0x11 = 17): handle with raw byte emission
  ;; to avoid decode-u32 overflow on i386 (fixnum-shift=1 loses bit 31)
  (if (= (aref bytecode pos) 17)
      (i386-translate-li-raw buf state bytecode pos)
      ;; All other opcodes: normal decode path
      (let ((decoded (decode-instruction bytecode pos)))
        (let ((new-pos (cdr (cdr decoded))))
          (td-write-u32 #x3000060 new-pos)
          (let ((label (gethash pos (i386-translate-state-label-map state))))
            (when label
              (i386-emit-label buf label)))
          (let ((opcode (car decoded)))
            (let ((operands (car (cdr decoded))))
              (td-write-u32 #x300004C opcode)
              (td-write-u32 #x3000048 pos)
              (i386-translate-insn state opcode operands new-pos)))
          new-pos))))

;;; Progressive i386-translate-insn override for AArch64 bisection
;;; Group A: NOP/BREAK/TRAP + data movement + arithmetic + bitwise + comparison
;;; Group B: branches
;;; Group C: list ops + object ops + memory + call/ret + alloc + IO
;;; Unknown opcodes → NOP (safe fallback)
(defun i386-translate-insn (state opcode operands mvm-next-pos)
  (let ((buf (i386-translate-state-buf state)))
    (cond
      ;; === NOP/BREAK ===
      ((= opcode 0) (i386-emit-nop buf))
      ((= opcode 1) (i386-emit-int3 buf))

      ;; === TRAP (#x02) ===
      ((= opcode 2)
       (let ((code (first operands)))
         (cond
           ((< code 256)
            ;; Frame-enter: code = nparams.
            ;; If nparams > 4, copy overflow args from caller's stack to local frame slots.
            ;; Overflow args at [EBP+16+k*4], frame slot N at [EBP + (-68) + N*(-4)].
            (when (> code 4)
              (let ((param-idx 4))
                (loop
                  (when (>= param-idx code) (return nil))
                  (let ((k (- param-idx 4)))
                    (let ((src-off (+ 16 (* k 4))))
                      (let ((dst-off (+ -68 (* param-idx -4))))
                        (i386-emit-mov-reg-mem buf 1 5 src-off)
                        (i386-emit-mov-mem-reg buf 5 dst-off 1))))
                  (setq param-idx (+ param-idx 1))))))
           ((< code 768) nil)
           ((= code 768)
            (let ((poll-label (i386-make-label)))
              (i386-emit-byte buf 102) (i386-emit-byte buf 186)
              (i386-emit-byte buf 253) (i386-emit-byte buf 3)
              (i386-emit-label buf poll-label)
              (i386-emit-byte buf 236)
              (i386-emit-byte buf 168) (i386-emit-byte buf 32)
              (i386-emit-jcc buf :z poll-label))
            (i386-emit-byte buf 137) (i386-emit-byte buf 240)
            (i386-emit-byte buf 209) (i386-emit-byte buf 248)
            (i386-emit-byte buf 102) (i386-emit-byte buf 186)
            (i386-emit-byte buf 248) (i386-emit-byte buf 3)
            (i386-emit-byte buf 238))
           ((= code 769)
            (let ((poll-label (i386-make-label)))
              (i386-emit-byte buf 102) (i386-emit-byte buf 186)
              (i386-emit-byte buf 253) (i386-emit-byte buf 3)
              (i386-emit-label buf poll-label)
              (i386-emit-byte buf 236)
              (i386-emit-byte buf 168) (i386-emit-byte buf 1)
              (i386-emit-jcc buf :z poll-label))
            (i386-emit-byte buf 102) (i386-emit-byte buf 186)
            (i386-emit-byte buf 248) (i386-emit-byte buf 3)
            (i386-emit-byte buf 236)
            (i386-emit-byte buf 15) (i386-emit-byte buf 182) (i386-emit-byte buf 192)
            (i386-emit-byte buf 209) (i386-emit-byte buf 224)
            (i386-emit-byte buf 137) (i386-emit-byte buf 198))
           (t nil))))

      ;; === MOV (#x10) ===
      ((= opcode 16)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 1 vs)
         (i386-store-vreg buf vd 1)))

      ;; === LI (#x11) ===
      ((= opcode 17)
       (let ((vd (first operands))
             (imm (logand (second operands) 4294967295)))
         (cond
           ((i386-has-phys vd)
            (i386-emit-mov-reg-imm buf (i386-vreg-phys vd) imm))
           ((i386-vreg-abs-addr vd)
            (let ((abs (i386-vreg-abs-addr vd)))
              (i386-emit-mov-reg-imm buf 1 imm)
              (i386-emit-mov-abs-reg buf abs 1)))
           (t (i386-emit-mov-mem-imm buf 5 (i386-spill-offset vd) imm)))))

      ;; === PUSH (#x12) ===
      ((= opcode 18)
       (let ((vs (first operands)))
         (cond
           ((i386-has-phys vs) (i386-emit-push-reg buf (i386-vreg-phys vs)))
           ((i386-vreg-abs-addr vs) (i386-emit-push-abs buf (i386-vreg-abs-addr vs)))
           (t (i386-emit-push-mem buf 5 (i386-spill-offset vs))))))

      ;; === POP (#x13) ===
      ((= opcode 19)
       (let ((vd (first operands)))
         (if (i386-has-phys vd)
             (i386-emit-pop-reg buf (i386-vreg-phys vd))
             (progn
               (i386-emit-pop-reg buf 1)
               (i386-store-vreg buf vd 1)))))

      ;; === ADD (#x20) ===
      ((= opcode 32)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-add-reg-reg buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === SUB (#x21) ===
      ((= opcode 33)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-sub-reg-reg buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === MUL (#x22) ===
      ((= opcode 34)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-sar-reg-imm buf 0 1)
         (i386-emit-imul-reg-reg buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === DIV (#x23) ===
      ((= opcode 35)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-emit-sar-reg-imm buf 1 1)
         (i386-load-vreg buf 0 va)
         (i386-emit-sar-reg-imm buf 0 1)
         (i386-emit-cdq buf)
         (i386-emit-idiv-reg buf 1)
         (i386-emit-shl-reg-imm buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === MOD (#x24) ===
      ((= opcode 36)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-emit-sar-reg-imm buf 1 1)
         (i386-load-vreg buf 0 va)
         (i386-emit-sar-reg-imm buf 0 1)
         (i386-emit-cdq buf)
         (i386-emit-idiv-reg buf 1)
         (i386-emit-shl-reg-imm buf 2 1)
         (i386-store-vreg buf vd 2)))

      ;; === NEG (#x25) ===
      ((= opcode 37)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-neg-reg buf 0)
         (i386-store-vreg buf vd 0)))

      ;; === INC (#x26) ===
      ((= opcode 38)
       (let ((vd (first operands)))
         (if (i386-has-phys vd)
             (i386-emit-add-reg-imm buf (i386-vreg-phys vd) 2)
             (progn
               (i386-load-vreg buf 0 vd)
               (i386-emit-add-reg-imm buf 0 2)
               (i386-store-vreg buf vd 0)))))

      ;; === DEC (#x27) ===
      ((= opcode 39)
       (let ((vd (first operands)))
         (if (i386-has-phys vd)
             (i386-emit-sub-reg-imm buf (i386-vreg-phys vd) 2)
             (progn
               (i386-load-vreg buf 0 vd)
               (i386-emit-sub-reg-imm buf 0 2)
               (i386-store-vreg buf vd 0)))))

      ;; === AND (#x28) ===
      ((= opcode 40)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-and-reg-reg buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === OR (#x29) ===
      ((= opcode 41)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-or-reg-reg buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === XOR (#x2A) ===
      ((= opcode 42)
       (let ((vd (first operands)) (va (second operands)) (vb (third operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-xor-reg-reg buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === SHL (#x2B) ===
      ((= opcode 43)
       (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
         (i386-load-vreg buf 1 vs)
         (i386-emit-shl-reg-imm buf 1 amt)
         (i386-store-vreg buf vd 1)))

      ;; === SHR (#x2C) ===
      ((= opcode 44)
       (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
         (i386-load-vreg buf 1 vs)
         (i386-emit-shr-reg-imm buf 1 amt)
         (i386-store-vreg buf vd 1)))

      ;; === SAR (#x2D) ===
      ((= opcode 45)
       (let ((vd (first operands)) (vs (second operands)) (amt (third operands)))
         (i386-load-vreg buf 1 vs)
         (i386-emit-sar-reg-imm buf 1 amt)
         (i386-store-vreg buf vd 1)))

      ;; === LDB (#x2E) ===
      ((= opcode 46)
       (let ((vd (first operands)) (vs (second operands))
             (pos (third operands)) (size (fourth operands)))
         (i386-load-vreg buf 0 vs)
         (when (> pos 0)
           (i386-emit-shr-reg-imm buf 0 pos))
         (i386-emit-and-reg-imm buf 0 (logand (- (ash 1 size) 1) 4294967295))
         (i386-store-vreg buf vd 0)))

      ;; === SHLV (#x2F) ===
      ((= opcode 47)
       (let ((vd (first operands)) (vs (second operands)) (vc (third operands)))
         (i386-load-vreg buf 1 vc)
         (i386-load-vreg buf 0 vs)
         (i386-emit-shl-reg-cl buf 0)
         (i386-store-vreg buf vd 0)))

      ;; === CMP (#x30) ===
      ((= opcode 48)
       (let ((va (first operands)) (vb (second operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-cmp-reg-reg buf 0 1)))

      ;; === TEST (#x31) ===
      ((= opcode 49)
       (let ((va (first operands)) (vb (second operands)))
         (i386-load-vreg buf 1 vb)
         (i386-load-vreg buf 0 va)
         (i386-emit-test-reg-reg buf 0 1)))

      ;; === SARV (#x32) ===
      ((= opcode 50)
       (let ((vd (first operands)) (vs (second operands)) (vc (third operands)))
         (i386-load-vreg buf 1 vc)
         (i386-load-vreg buf 0 vs)
         (i386-emit-sar-reg-cl buf 0)
         (i386-store-vreg buf vd 0)))

      ;; === BRANCHES (#x40-#x48) ===
      ((= opcode 64)
       (let ((off (first operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-emit-jmp-rel32 buf label)))))
      ((= opcode 65)
       (let ((off (first operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-emit-jcc buf :e label)))))
      ((= opcode 66)
       (let ((off (first operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-emit-jcc buf :ne label)))))
      ((= opcode 67)
       (let ((off (first operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-emit-jcc buf :l label)))))
      ((= opcode 68)
       (let ((off (first operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-emit-jcc buf :ge label)))))
      ((= opcode 69)
       (let ((off (first operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-emit-jcc buf :le label)))))
      ((= opcode 70)
       (let ((off (first operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-emit-jcc buf :g label)))))

      ;; === BNULL (#x47) ===
      ((= opcode 71)
       (let ((vs (first operands)) (off (second operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-load-vreg buf 0 vs)
             (i386-emit-cmp-reg-abs buf 0 1544)
             (i386-emit-jcc buf :e label)))))

      ;; === BNNULL (#x48) ===
      ((= opcode 72)
       (let ((vs (first operands)) (off (second operands)))
         (let ((target-pos (+ mvm-next-pos off)))
           (let ((label (i386-ensure-label-at state target-pos)))
             (i386-load-vreg buf 0 vs)
             (i386-emit-cmp-reg-abs buf 0 1544)
             (i386-emit-jcc buf :ne label)))))

      ;; === CAR (#x50) ===
      ((= opcode 80)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-sub-reg-imm buf 0 1)
         (i386-emit-mov-reg-mem buf 0 0 0)
         (i386-store-vreg buf vd 0)))

      ;; === CDR (#x51) ===
      ((= opcode 81)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-sub-reg-imm buf 0 1)
         (i386-emit-mov-reg-mem buf 0 0 4)
         (i386-store-vreg buf vd 0)))

      ;; === CONS (#x52) ===
      ((= opcode 82)
       (let ((vd (first operands)) (va-arg (second operands)) (vb-arg (third operands)))
         (i386-load-vreg buf 1 vb-arg)
         (i386-emit-mov-reg-abs buf 2 1536)
         (i386-load-vreg buf 0 va-arg)
         (i386-emit-mov-mem-reg buf 2 0 0)
         (i386-emit-mov-mem-reg buf 2 4 1)
         (i386-emit-mov-reg-reg buf 0 2)
         (i386-emit-or-reg-imm buf 0 1)
         (i386-emit-add-reg-imm buf 2 16)
         (i386-emit-mov-abs-reg buf 1536 2)
         (i386-store-vreg buf vd 0)))

      ;; === SETCAR (#x53) ===
      ((= opcode 83)
       (let ((vd-reg (first operands)) (vs (second operands)))
         (i386-load-vreg buf 1 vs)
         (i386-load-vreg buf 0 vd-reg)
         (i386-emit-sub-reg-imm buf 0 1)
         (i386-emit-mov-mem-reg buf 0 0 1)))

      ;; === SETCDR (#x54) ===
      ((= opcode 84)
       (let ((vd-reg (first operands)) (vs (second operands)))
         (i386-load-vreg buf 1 vs)
         (i386-load-vreg buf 0 vd-reg)
         (i386-emit-sub-reg-imm buf 0 1)
         (i386-emit-mov-mem-reg buf 0 4 1)))

      ;; === CONSP (#x55) ===
      ((= opcode 85)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-and-reg-imm buf 0 15)
         (i386-emit-cmp-reg-imm buf 0 1)
         (let ((true-label (i386-make-label))
               (done-label (i386-make-label)))
           (i386-emit-jcc buf :e true-label)
           (i386-emit-mov-reg-abs buf 0 1544)
           (i386-emit-jmp-rel32 buf done-label)
           (i386-emit-label buf true-label)
           (i386-emit-mov-reg-imm buf 0 262)
           (i386-emit-label buf done-label))
         (i386-store-vreg buf vd 0)))

      ;; === ATOM (#x56) ===
      ((= opcode 86)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-and-reg-imm buf 0 15)
         (i386-emit-cmp-reg-imm buf 0 1)
         (let ((true-label (i386-make-label))
               (done-label (i386-make-label)))
           (i386-emit-jcc buf :ne true-label)
           (i386-emit-mov-reg-abs buf 0 1544)
           (i386-emit-jmp-rel32 buf done-label)
           (i386-emit-label buf true-label)
           (i386-emit-mov-reg-imm buf 0 262)
           (i386-emit-label buf done-label))
         (i386-store-vreg buf vd 0)))

      ;; === ALLOC-OBJ (#x60) ===
      ((= opcode 96)
       (let ((vd (first operands)) (count (second operands)) (subtag (third operands)))
         (i386-emit-mov-reg-abs buf 2 1536)
         (i386-emit-mov-mem-imm buf 2 0 (logior (ash count 8) subtag))
         (i386-emit-mov-reg-reg buf 0 2)
         (i386-emit-or-reg-imm buf 0 9)
         (let ((total (logand (+ (* (+ count 1) 4) 15) (lognot 15))))
           (i386-emit-add-reg-imm buf 2 total))
         (i386-emit-mov-abs-reg buf 1536 2)
         (i386-store-vreg buf vd 0)))

      ;; === OBJ-REF (#x61) ===
      ((= opcode 97)
       (let ((vd (first operands)) (vobj (second operands)) (idx (third operands)))
         (if (= vobj 21)
             (progn
               (i386-emit-mov-reg-mem buf 0 5 (+ -68 (* idx -4)))
               (i386-store-vreg buf vd 0))
             (progn
               (i386-load-vreg buf 0 vobj)
               (i386-emit-sub-reg-imm buf 0 9)
               (i386-emit-mov-reg-mem buf 0 0 (* (+ idx 1) 4))
               (i386-store-vreg buf vd 0)))))

      ;; === OBJ-SET (#x62) ===
      ((= opcode 98)
       (let ((vobj (first operands)) (idx (second operands)) (vs (third operands)))
         (if (= vobj 21)
             (progn
               (i386-load-vreg buf 1 vs)
               (i386-emit-mov-mem-reg buf 5 (+ -68 (* idx -4)) 1))
             (progn
               (i386-load-vreg buf 1 vs)
               (i386-load-vreg buf 0 vobj)
               (i386-emit-sub-reg-imm buf 0 9)
               (i386-emit-mov-mem-reg buf 0 (* (+ idx 1) 4) 1)))))

      ;; === OBJ-TAG (#x63) ===
      ((= opcode 99)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-and-reg-imm buf 0 15)
         (i386-emit-shl-reg-imm buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === OBJ-SUBTAG (#x64) ===
      ((= opcode 100)
       (let ((vd (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-sub-reg-imm buf 0 9)
         (i386-emit-mov-reg-mem buf 0 0 0)
         (i386-emit-and-reg-imm buf 0 255)
         (i386-emit-shl-reg-imm buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === AREF (#x65) ===
      ((= opcode 101)
       (let ((vd (first operands)) (vobj (second operands)) (vidx (third operands)))
         (i386-load-vreg buf 1 vidx)
         (i386-emit-shl-reg-imm buf 1 1)
         (i386-load-vreg buf 0 vobj)
         (i386-emit-add-reg-reg buf 0 1)
         (i386-emit-mov-reg-mem buf 0 0 -5)
         (i386-store-vreg buf vd 0)))

      ;; === ASET (#x66) ===
      ((= opcode 102)
       (let ((vobj (first operands)) (vidx (second operands)) (vs (third operands)))
         (i386-load-vreg buf 1 vs)
         (i386-load-vreg buf 0 vidx)
         (i386-emit-shl-reg-imm buf 0 1)
         (i386-emit-push-reg buf 1)
         (i386-load-vreg buf 2 vobj)
         (i386-emit-add-reg-reg buf 0 2)
         (i386-emit-pop-reg buf 1)
         (i386-emit-mov-mem-reg buf 0 -5 1)))

      ;; === ARRAY-LEN (#x67) ===
      ((= opcode 103)
       (let ((vd (first operands)) (vobj (second operands)))
         (i386-load-vreg buf 0 vobj)
         (i386-emit-sub-reg-imm buf 0 9)
         (i386-emit-mov-reg-mem buf 0 0 0)
         (i386-emit-shr-reg-imm buf 0 8)
         (i386-emit-and-reg-imm buf 0 16777215)
         (i386-emit-shl-reg-imm buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === ALLOC-ARRAY (#x68) ===
      ((= opcode 104)
       (let ((vd (first operands)) (vcount (second operands)))
         (i386-load-vreg buf 1 vcount)
         (i386-emit-push-reg buf 1)
         (i386-emit-shl-reg-imm buf 1 8)
         (i386-emit-or-reg-imm buf 1 50)
         (i386-emit-mov-reg-abs buf 2 1536)
         (i386-emit-mov-mem-reg buf 2 0 1)
         (i386-emit-mov-reg-reg buf 0 2)
         (i386-emit-or-reg-imm buf 0 9)
         (i386-emit-pop-reg buf 1)
         (i386-emit-add-reg-imm buf 1 1)
         (i386-emit-shl-reg-imm buf 1 2)
         (i386-emit-add-reg-imm buf 1 15)
         (i386-emit-and-reg-imm buf 1 -16)
         (i386-emit-add-reg-reg buf 2 1)
         (i386-emit-mov-abs-reg buf 1536 2)
         (i386-store-vreg buf vd 0)))

      ;; === LOAD (#x70) ===
      ((= opcode 112)
       (let ((vd (first operands)) (vaddr (second operands)) (width (third operands)))
         (i386-load-vreg buf 0 vaddr)
         (cond
           ((= width 0) (i386-emit-movzx-byte buf 0 0 0))
           ((= width 1) (i386-emit-movzx-word buf 0 0 0))
           (t (i386-emit-mov-reg-mem buf 0 0 0)))
         (i386-store-vreg buf vd 0)))

      ;; === STORE (#x71) ===
      ((= opcode 113)
       (let ((vaddr (first operands)) (vs (second operands)) (width (third operands)))
         (i386-load-vreg buf 1 vs)
         (i386-load-vreg buf 0 vaddr)
         (cond
           ((= width 0) (i386-emit-mov-mem8-reg buf 0 0 1))
           ((= width 1) (i386-emit-mov-mem16-reg buf 0 0 1))
           (t (i386-emit-mov-mem-reg buf 0 0 1)))))

      ;; === FENCE (#x72) ===
      ((= opcode 114) (i386-emit-mfence buf))

      ;; === CALL (#x80) ===
      ((= opcode 128)
       (let ((target-offset (first operands)))
         (let ((fn-table (i386-translate-state-function-table state)))
           (let ((label (when fn-table (gethash target-offset fn-table))))
             (i386-emit-push-mem buf 5 -20)
             (i386-emit-push-mem buf 5 -16)
             (if label
                 (i386-emit-call-rel32 buf label)
                 (i386-emit-call-rel32 buf nil))
             (i386-emit-add-reg-imm buf 4 8)))))

      ;; === CALL-IND (#x81) ===
      ((= opcode 129)
       (let ((vs (first operands)))
         (i386-emit-push-mem buf 5 -20)
         (i386-emit-push-mem buf 5 -16)
         (i386-load-vreg buf 0 vs)
         (i386-emit-call-reg buf 0)
         (i386-emit-add-reg-imm buf 4 8)))

      ;; === RET (#x82) ===
      ((= opcode 130)
       (i386-emit-epilogue buf))

      ;; === TAILCALL (#x83) ===
      ((= opcode 131)
       (let ((target-offset (first operands)))
         (let ((fn-table (i386-translate-state-function-table state)))
           (let ((label (when fn-table (gethash target-offset fn-table))))
             ;; Save V2/V3 before frame teardown
             (i386-emit-mov-reg-mem buf 1 5 -16)    ;; ECX = V2
             (i386-emit-mov-reg-mem buf 2 5 -20)    ;; EDX = V3
             ;; Restore callee-saved
             (i386-emit-mov-reg-mem buf 3 5 -4)     ;; EBX = [EBP-4]
             (i386-emit-mov-reg-mem buf 6 5 -8)     ;; ESI = [EBP-8]
             (i386-emit-mov-reg-mem buf 7 5 -12)    ;; EDI = [EBP-12]
             ;; Tear down frame
             (i386-emit-mov-reg-reg buf 4 5)         ;; ESP = EBP
             (i386-emit-pop-reg buf 5)               ;; POP EBP
             ;; Pop return addr, push V3, V2, return addr back
             (i386-emit-pop-reg buf 0)               ;; EAX = return addr
             (i386-emit-push-reg buf 2)              ;; push V3
             (i386-emit-push-reg buf 1)              ;; push V2
             (i386-emit-push-reg buf 0)              ;; push return addr
             (if label
                 (i386-emit-jmp-rel32 buf label)
                 (i386-emit-jmp-rel32 buf nil))))))

      ;; === ALLOC-CONS (#x88) ===
      ((= opcode 136)
       (let ((vd (first operands)))
         (i386-emit-mov-reg-abs buf 0 1536)
         (i386-emit-mov-reg-reg buf 1 0)
         (i386-emit-or-reg-imm buf 1 1)
         (i386-emit-add-reg-imm buf 0 16)
         (i386-emit-mov-abs-reg buf 1536 0)
         (i386-store-vreg buf vd 1)))

      ;; === GC-CHECK (#x89) ===
      ((= opcode 137)
       (i386-emit-mov-reg-abs buf 1 1536)
       (i386-emit-cmp-reg-abs buf 1 1540)
       (let ((ok-label (i386-make-label)))
         (i386-emit-jcc buf :b ok-label)
         (let ((gc-lbl (i386-translate-state-gc-label state)))
           (if gc-lbl
               (i386-emit-call-rel32 buf gc-lbl)
               (i386-emit-int buf 49)))
         (i386-emit-label buf ok-label)))

      ;; === WRITE-BARRIER (#x8A) ===
      ((= opcode 138) (i386-emit-nop buf))

      ;; === SAVE-CTX (#x90) ===
      ((= opcode 144)
       (i386-emit-push-reg buf 6)
       (i386-emit-push-reg buf 7)
       (i386-emit-push-reg buf 3)
       (i386-emit-push-abs buf 1536)
       (i386-emit-push-abs buf 1540)
       (i386-emit-push-abs buf 1544))

      ;; === RESTORE-CTX (#x91) ===
      ((= opcode 145)
       (i386-emit-pop-abs buf 1544)
       (i386-emit-pop-abs buf 1540)
       (i386-emit-pop-abs buf 1536)
       (i386-emit-pop-reg buf 3)
       (i386-emit-pop-reg buf 7)
       (i386-emit-pop-reg buf 6))

      ;; === YIELD (#x92) ===
      ((= opcode 146) (i386-emit-nop buf))

      ;; === ATOMIC-XCHG (#x93) ===
      ((= opcode 147)
       (let ((vd (first operands)) (vaddr (second operands)) (vs (third operands)))
         (i386-load-vreg buf 0 vs)
         (i386-load-vreg buf 1 vaddr)
         (i386-emit-xchg-mem-reg buf 1 0 0)
         (i386-store-vreg buf vd 0)))

      ;; === IO-READ (#xA0) ===
      ((= opcode 160)
       (let ((vd (first operands)) (port (second operands)) (width (third operands)))
         (cond
           ((<= port 255)
            (cond
              ((= width 0) (i386-emit-in-al-imm8 buf port)
                           (i386-emit-and-reg-imm buf 0 255))
              ((= width 1) (i386-emit-in-ax-imm8 buf port)
                           (i386-emit-and-reg-imm buf 0 65535))
              (t (i386-emit-in-eax-imm8 buf port))))
           (t
            (i386-emit-mov-reg-imm buf 2 port)
            (cond
              ((= width 0) (i386-emit-in-al-dx buf)
                           (i386-emit-and-reg-imm buf 0 255))
              ((= width 1) (i386-emit-in-ax-dx buf)
                           (i386-emit-and-reg-imm buf 0 65535))
              (t (i386-emit-in-eax-dx buf)))))
         (i386-emit-shl-reg-imm buf 0 1)
         (i386-store-vreg buf vd 0)))

      ;; === IO-WRITE (#xA1) ===
      ((= opcode 161)
       (let ((port (first operands)) (vs (second operands)) (width (third operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-shr-reg-imm buf 0 1)
         (cond
           ((<= port 255)
            (cond
              ((= width 0) (i386-emit-out-imm8-al buf port))
              ((= width 1) (i386-emit-byte buf 102) (i386-emit-out-imm8-eax buf port))
              (t (i386-emit-out-imm8-eax buf port))))
           (t
            (i386-emit-mov-reg-imm buf 2 port)
            (cond
              ((= width 0) (i386-emit-out-dx-al buf))
              ((= width 1) (i386-emit-out-dx-ax buf))
              (t (i386-emit-out-dx-eax buf)))))))

      ;; === HALT (#xA2) ===
      ((= opcode 162)
       (let ((halt-label (i386-make-label)))
         (i386-emit-label buf halt-label)
         (i386-emit-cli buf)
         (i386-emit-hlt buf)
         (i386-emit-jmp-rel32 buf halt-label)))

      ;; === CLI (#xA3) ===
      ((= opcode 163) (i386-emit-cli buf))

      ;; === STI (#xA4) ===
      ((= opcode 164) (i386-emit-sti buf))

      ;; === PERCPU-REF (#xA5) ===
      ((= opcode 165)
       (let ((vd (first operands)) (offset (second operands)))
         (i386-emit-byte buf 100)
         (i386-emit-byte buf 139)
         (i386-emit-byte buf (i386-modrm 0 0 5))
         (i386-emit-u32 buf offset)
         (i386-store-vreg buf vd 0)))

      ;; === PERCPU-SET (#xA6) ===
      ((= opcode 166)
       (let ((offset (first operands)) (vs (second operands)))
         (i386-load-vreg buf 0 vs)
         (i386-emit-byte buf 100)
         (i386-emit-byte buf 137)
         (i386-emit-byte buf (i386-modrm 0 0 5))
         (i386-emit-u32 buf offset)))

      ;; === Unknown → NOP ===
      (t (i386-emit-nop buf)))))

;;; Override i386-vreg-phys to avoid *i386-vreg-map* global lookup
;;; Returns physical register number or nil. WARNING: returns 0 for VR=EAX,
;;; which is indistinguishable from NIL on AArch64 (NIL=0).
;;; Use i386-has-phys to check if a vreg has a physical register.
(defun i386-vreg-phys (vreg)
  (cond
    ((= vreg 0) 6)   ;; V0 = ESI
    ((= vreg 1) 7)   ;; V1 = EDI
    ((= vreg 4) 3)   ;; V4 = EBX
    ((= vreg 16) 0)  ;; VR = EAX
    ((= vreg 20) 4)  ;; VSP = ESP
    ((= vreg 21) 5)  ;; VFP = EBP
    (t nil)))

;;; Predicate: does vreg have a physical register? Safe on AArch64 (NIL=0).
(defun i386-has-phys (vreg)
  (cond
    ((= vreg 0) 1) ((= vreg 1) 1) ((= vreg 4) 1)
    ((= vreg 16) 1) ((= vreg 20) 1) ((= vreg 21) 1)
    (t nil)))

;;; Override i386-store-vreg — fully inlined to avoid NIL=0 truthiness bug
;;; On AArch64, fixnum 0 == NIL, so i386-vreg-phys returning 0 (EAX)
;;; is treated as false. Fix: match vreg values directly, no truthiness check.
(defun i386-store-vreg (buf vreg scratch)
  (cond
    ((= vreg 0)  (unless (= 6 scratch) (i386-emit-mov-reg-reg buf 6 scratch)))
    ((= vreg 1)  (unless (= 7 scratch) (i386-emit-mov-reg-reg buf 7 scratch)))
    ((= vreg 4)  (unless (= 3 scratch) (i386-emit-mov-reg-reg buf 3 scratch)))
    ((= vreg 16) (unless (= 0 scratch) (i386-emit-mov-reg-reg buf 0 scratch)))
    ((= vreg 20) (unless (= 4 scratch) (i386-emit-mov-reg-reg buf 4 scratch)))
    ((= vreg 21) (unless (= 5 scratch) (i386-emit-mov-reg-reg buf 5 scratch)))
    ((= vreg 17) (i386-emit-mov-abs-reg buf 1536 scratch))
    ((= vreg 18) (i386-emit-mov-abs-reg buf 1540 scratch))
    ((= vreg 19) (i386-emit-mov-abs-reg buf 1544 scratch))
    (t (i386-emit-mov-mem-reg buf 5 (i386-spill-offset vreg) scratch))))

;;; Override i386-load-vreg — same NIL=0 fix
(defun i386-load-vreg (buf scratch vreg)
  (cond
    ((= vreg 0)  (unless (= scratch 6) (i386-emit-mov-reg-reg buf scratch 6)))
    ((= vreg 1)  (unless (= scratch 7) (i386-emit-mov-reg-reg buf scratch 7)))
    ((= vreg 4)  (unless (= scratch 3) (i386-emit-mov-reg-reg buf scratch 3)))
    ((= vreg 16) (unless (= scratch 0) (i386-emit-mov-reg-reg buf scratch 0)))
    ((= vreg 20) (unless (= scratch 4) (i386-emit-mov-reg-reg buf scratch 4)))
    ((= vreg 21) (unless (= scratch 5) (i386-emit-mov-reg-reg buf scratch 5)))
    ((= vreg 17) (i386-emit-mov-reg-abs buf scratch 1536))
    ((= vreg 18) (i386-emit-mov-reg-abs buf scratch 1540))
    ((= vreg 19) (i386-emit-mov-reg-abs buf scratch 1544))
    (t (i386-emit-mov-reg-mem buf scratch 5 (i386-spill-offset vreg)))))

;;; Override i386-vreg-abs-addr
(defun i386-vreg-abs-addr (vreg)
  (cond
    ((= vreg 17) 1536)  ;; VA = 0x600
    ((= vreg 18) 1540)  ;; VL = 0x604
    ((= vreg 19) 1544)  ;; VN = 0x608
    (t nil)))

;;; Override i386-emit-modrm-mem — the original uses 3-arg <= and /= which
;;; may have issues on AArch64 bare metal. Simplify to always use disp8 or disp32.
(defun i386-emit-modrm-mem (buf reg-field base-reg offset)
  (let ((needs-sib (= base-reg 4)))  ;; ESP=4
    (cond
      ;; 8-bit displacement: offset fits in [-128, 127]
      ((and (>= offset -128) (<= offset 127))
       (if needs-sib
           (progn
             (i386-emit-byte buf (i386-modrm 1 reg-field 4))
             (i386-emit-byte buf (i386-sib 0 4 4)))
           (i386-emit-byte buf (i386-modrm 1 reg-field base-reg)))
       (i386-emit-s8 buf offset))
      ;; 32-bit displacement
      (t
       (if needs-sib
           (progn
             (i386-emit-byte buf (i386-modrm 2 reg-field 4))
             (i386-emit-byte buf (i386-sib 0 4 4)))
           (i386-emit-byte buf (i386-modrm 2 reg-field base-reg)))
       (i386-emit-s32 buf offset)))))

;;; Override i386-emit-s32 — original may use logand with negative numbers
(defun i386-emit-s32 (buf val)
  (let ((v (if (< val 0) (+ val 4294967296) val)))
    (i386-emit-byte buf (logand v 255))
    (i386-emit-byte buf (logand (ash v -8) 255))
    (i386-emit-byte buf (logand (ash v -16) 255))
    (i386-emit-byte buf (logand (ash v -24) 255))))

;;; Override i386-emit-init-serial — the original uses labels (local functions) which
;;; the bare-metal compiler handles as global functions, but may have issues with
;;; function resolution after 968+ functions are compiled. Inline instead.
(defun i386-serial-out (buf port val)
  (mvm-emit-byte buf 102)   ;; 66 prefix
  (mvm-emit-byte buf 186)   ;; BA = mov dx, imm16
  (mvm-emit-u16 buf port)
  (mvm-emit-byte buf 176)   ;; B0 = mov al, imm8
  (mvm-emit-byte buf val)
  (mvm-emit-byte buf 238))  ;; EE = out dx, al

(defun i386-init-serial (buf)
  (i386-serial-out buf 1017 0)     ;; 0x3F9: disable interrupts
  (i386-serial-out buf 1019 128)   ;; 0x3FB: DLAB
  (i386-serial-out buf 1016 1)     ;; 0x3F8: divisor lo
  (i386-serial-out buf 1017 0)     ;; 0x3F9: divisor hi
  (i386-serial-out buf 1019 3)     ;; 0x3FB: 8N1
  (i386-serial-out buf 1018 193))  ;; 0x3FA: FIFO

;;; Override emit-i386-entry — avoid labels construct, use simpler code
(defun emit-i386-entry (buf)
  ;; CLI
  (mvm-emit-byte buf 250)
  ;; mov [0x500], ebx
  (mvm-emit-byte buf 137) (mvm-emit-byte buf 29) (mvm-emit-u32 buf 1280)
  ;; mov esp, 0x400000
  (mvm-emit-byte buf 188) (mvm-emit-u32 buf 4194304)
  ;; push ebp; mov ebp, esp
  (mvm-emit-byte buf 85)
  (mvm-emit-byte buf 137) (mvm-emit-byte buf 229)
  ;; sub esp, 80
  (mvm-emit-byte buf 131) (mvm-emit-byte buf 236) (mvm-emit-byte buf 80)
  ;; mov [0x600], 0x800000
  (mvm-emit-byte buf 199) (mvm-emit-byte buf 5) (mvm-emit-u32 buf 1536) (mvm-emit-u32 buf 8388608)
  ;; mov [0x604], 0x7800000  (120MB — cross-compile needs ~70MB heap for 32-bit arrays)
  (mvm-emit-byte buf 199) (mvm-emit-byte buf 5) (mvm-emit-u32 buf 1540) (mvm-emit-u32 buf 125829120)
  ;; mov [0x608], 0x00
  (mvm-emit-byte buf 199) (mvm-emit-byte buf 5) (mvm-emit-u32 buf 1544) (mvm-emit-u32 buf 0)
  ;; Serial init
  (i386-init-serial buf))

;;; Override i386 ALU emitters (macro-generated versions may not work on bare metal
;;; due to 3-arg <= with negative numbers or constant resolution issues)
(defun i386-emit-alu-reg-reg (buf opcode dst src)
  (i386-emit-byte buf opcode)
  (i386-emit-byte buf (i386-modrm 3 src dst)))

(defun i386-emit-alu-reg-imm (buf opcode-ri8 opcode-ri32 modrm-ext reg imm)
  (cond
    ((and (>= imm -128) (<= imm 127))
     (i386-emit-byte buf opcode-ri8)
     (i386-emit-byte buf (i386-modrm 3 modrm-ext reg))
     (i386-emit-s8 buf imm))
    ((= reg 0) ;; EAX short form
     (i386-emit-byte buf opcode-ri32)
     (i386-emit-s32 buf imm))
    (t
     (i386-emit-byte buf 129) ;; #x81
     (i386-emit-byte buf (i386-modrm 3 modrm-ext reg))
     (i386-emit-s32 buf imm))))

(defun i386-emit-alu-reg-mem (buf opcode reg base offset)
  (i386-emit-byte buf opcode)
  (i386-emit-modrm-mem buf reg base offset))

(defun i386-emit-alu-mem-reg (buf opcode base offset reg)
  (i386-emit-byte buf opcode)
  (i386-emit-modrm-mem buf reg base offset))

;; ADD: rr=#x01, ri8=#x83, ri32=#x05, ext=0, rm=#x03, mr=#x01
(defun i386-emit-add-reg-reg (buf dst src) (i386-emit-alu-reg-reg buf 1 dst src))
(defun i386-emit-add-reg-imm (buf reg imm) (i386-emit-alu-reg-imm buf 131 5 0 reg imm))
(defun i386-emit-add-reg-mem (buf reg base offset) (i386-emit-alu-reg-mem buf 3 reg base offset))
(defun i386-emit-add-mem-reg (buf base offset reg) (i386-emit-alu-mem-reg buf 1 base offset reg))

;; SUB: rr=#x29, ri8=#x83, ri32=#x2D, ext=5, rm=#x2B, mr=#x29
(defun i386-emit-sub-reg-reg (buf dst src) (i386-emit-alu-reg-reg buf 41 dst src))
(defun i386-emit-sub-reg-imm (buf reg imm) (i386-emit-alu-reg-imm buf 131 45 5 reg imm))
(defun i386-emit-sub-reg-mem (buf reg base offset) (i386-emit-alu-reg-mem buf 43 reg base offset))
(defun i386-emit-sub-mem-reg (buf base offset reg) (i386-emit-alu-mem-reg buf 41 base offset reg))

;; CMP: rr=#x39, ri8=#x83, ri32=#x3D, ext=7, rm=#x3B, mr=#x39
(defun i386-emit-cmp-reg-reg (buf dst src) (i386-emit-alu-reg-reg buf 57 dst src))
(defun i386-emit-cmp-reg-imm (buf reg imm) (i386-emit-alu-reg-imm buf 131 61 7 reg imm))
(defun i386-emit-cmp-reg-mem (buf reg base offset) (i386-emit-alu-reg-mem buf 59 reg base offset))
(defun i386-emit-cmp-mem-reg (buf base offset reg) (i386-emit-alu-mem-reg buf 57 base offset reg))

;; AND: rr=#x21, ri8=#x83, ri32=#x25, ext=4, rm=#x23, mr=#x21
(defun i386-emit-and-reg-reg (buf dst src) (i386-emit-alu-reg-reg buf 33 dst src))
(defun i386-emit-and-reg-imm (buf reg imm) (i386-emit-alu-reg-imm buf 131 37 4 reg imm))
(defun i386-emit-and-reg-mem (buf reg base offset) (i386-emit-alu-reg-mem buf 35 reg base offset))
(defun i386-emit-and-mem-reg (buf base offset reg) (i386-emit-alu-mem-reg buf 33 base offset reg))

;; OR: rr=#x09, ri8=#x83, ri32=#x0D, ext=1, rm=#x0B, mr=#x09
(defun i386-emit-or-reg-reg (buf dst src) (i386-emit-alu-reg-reg buf 9 dst src))
(defun i386-emit-or-reg-imm (buf reg imm) (i386-emit-alu-reg-imm buf 131 13 1 reg imm))
(defun i386-emit-or-reg-mem (buf reg base offset) (i386-emit-alu-reg-mem buf 11 reg base offset))
(defun i386-emit-or-mem-reg (buf base offset reg) (i386-emit-alu-mem-reg buf 9 base offset reg))

;; XOR: rr=#x31, ri8=#x83, ri32=#x35, ext=6, rm=#x33, mr=#x31
(defun i386-emit-xor-reg-reg (buf dst src) (i386-emit-alu-reg-reg buf 49 dst src))
(defun i386-emit-xor-reg-imm (buf reg imm) (i386-emit-alu-reg-imm buf 131 53 6 reg imm))
(defun i386-emit-xor-reg-mem (buf reg base offset) (i386-emit-alu-reg-mem buf 51 reg base offset))
(defun i386-emit-xor-mem-reg (buf base offset reg) (i386-emit-alu-mem-reg buf 49 base offset reg))

;;; Override i386-emit-byte: verify writes + diagnostic for first bytes
(defun i386-emit-byte (buf byte)
  (let ((pos (i386-buffer-position buf)))
    (let ((masked (logand byte 255)))
      (let ((dummy (aset (i386-buffer-bytes buf) pos masked)))
        (set-i386-buffer-position buf (+ pos 1))
        dummy))))

;;; Override i386-emit-u32: avoid calling through original chain
(defun i386-emit-u32 (buf val)
  (i386-emit-byte buf (logand val 255))
  (i386-emit-byte buf (logand (ash val -8) 255))
  (i386-emit-byte buf (logand (ash val -16) 255))
  (i386-emit-byte buf (logand (ash val -24) 255)))

;;; Override i386-emit-s8: bare metal compatible
(defun i386-emit-s8 (buf val)
  (i386-emit-byte buf (if (< val 0) (logand val 255) val)))

;;; Override i386-emit-fixup-rel32: no push/list on bare metal
(defun i386-emit-fixup-rel32 (buf label-id)
  (let ((pos (i386-buffer-position buf)))
    (let ((entry (cons pos (cons label-id (cons 0 nil)))))
      (let ((old (i386-buffer-fixups buf)))
        (set-i386-buffer-fixups buf (cons entry old)))))
  (i386-emit-u32 buf 0))

;;; Override i386-emit-label: store (pos + 1) to avoid NIL=0 on AArch64.
;;; Position 0 becomes 1 (truthy), lookup miss returns 0/nil.
(defun i386-emit-label (buf label-id)
  (puthash label-id (i386-buffer-labels buf) (+ (i386-buffer-position buf) 1)))

;;; Helper: write byte 0 of a 32-bit LE value
(defun fixup-write-b0 (bytes pos val)
  (aset bytes pos (logand val 255)))

;;; Helper: write byte 1 of a 32-bit LE value
(defun fixup-write-b1 (bytes pos val)
  (aset bytes (+ pos 1) (logand (ash val -8) 255)))

;;; Helper: write byte 2 of a 32-bit LE value
(defun fixup-write-b2 (bytes pos val)
  (aset bytes (+ pos 2) (logand (ash val -16) 255)))

;;; Helper: write byte 3 of a 32-bit LE value
(defun fixup-write-b3 (bytes pos val)
  (aset bytes (+ pos 3) (logand (ash val -24) 255)))

;;; Helper: write a 32-bit little-endian value into byte array at pos.
;;; Split into 4 separate function calls to minimize vreg pressure.
(defun fixup-write-u32-le (bytes pos val)
  (fixup-write-b0 bytes pos val)
  (fixup-write-b1 bytes pos val)
  (fixup-write-b2 bytes pos val)
  (fixup-write-b3 bytes pos val))

;;; Helper: apply one fixup entry. Returns 1 if applied, 0 if label not found.
;;; Separate function to keep vreg count within V9-V15 spill limit.
(defun fixup-apply-one (buf bytes fixup)
  (let ((pos (car fixup))
        (label-id (car (cdr fixup))))
    (let ((raw-target (gethash label-id (i386-buffer-labels buf))))
      (if raw-target
          (let ((target (- raw-target 1)))
            (let ((rel (- target (+ pos 4))))
              (let ((urel (if (< rel 0) (logand rel 4294967295) rel)))
                (fixup-write-u32-le bytes pos urel)
                1)))
          0))))

;;; Override i386-fixup-labels: no destructuring-bind/ecase
;;; Label positions stored as (pos + 1) in hash table to avoid NIL=0 on AArch64.
;;; Kept shallow: outer loop has V0=buf + V9-V13 (5 vars).
(defun i386-fixup-labels (buf)
  (let ((bytes (i386-buffer-bytes buf))
        (rest-fixups (i386-buffer-fixups buf))
        (fix-count 0)
        (skip-count 0))
    (loop
      (when (null rest-fixups) (return nil))
      (let ((applied (fixup-apply-one buf bytes (car rest-fixups))))
        (if (= applied 1)
            (setq fix-count (+ fix-count 1))
            (setq skip-count (+ skip-count 1))))
      (setq rest-fixups (cdr rest-fixups)))
    (write-char-serial 70) (write-char-serial 88) ;; FX
    (print-dec fix-count)
    (write-char-serial 47) ;; /
    (print-dec skip-count)
    (write-char-serial 10)))

;;; Override i386-buffer-to-bytes: no dotimes-with-result
(defun i386-buffer-to-bytes (buf)
  (let ((len (i386-buffer-position buf)))
    (let ((result (make-array len))
          (i 0))
      (loop
        (when (>= i len) (return result))
        (aset result i (aref (i386-buffer-bytes buf) i))
        (setq i (+ i 1))))))

;;; Override i386-scan-branch-targets: no loop-while/member/position
(defun i386-scan-branch-targets (state)
  (let ((bytes (i386-translate-state-mvm-bytes state))
        (offset (i386-translate-state-mvm-offset state))
        (length (i386-translate-state-mvm-length state)))
    (let ((pos offset)
          (limit (+ offset length)))
      (loop
        (when (>= pos limit) (return nil))
        (let ((decoded (decode-instruction bytes pos)))
          (let ((opcode (car decoded))
                (operands (car (cdr decoded)))
                (new-pos (cdr (cdr decoded))))
            ;; Branch opcodes: 64-72 (BR through BNNULL)
            (when (and (>= opcode 64) (<= opcode 72))
              (let ((off (if (<= opcode 70)
                             (car operands)
                             (car (cdr operands)))))
                (let ((target-pos (+ new-pos off)))
                  (i386-ensure-label-at state target-pos))))
            (setq pos new-pos)))))))

;;; Override translate-mvm-to-i386: no loop-for/maphash/lambda/values
(defun translate-mvm-to-i386 (bytecode function-table)
  (write-char-serial 105) (write-char-serial 51) ;; i3
  (write-char-serial 56) (write-char-serial 54) ;; 86
  (write-char-serial 10)
  (let ((buf (make-i386-buffer)))
    (let ((n-functions (length function-table)))
      (print-dec n-functions) (write-char-serial 10)
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
                (let ((label (i386-make-label)))
                  (aset fn-labels i label)
                  (puthash name fn-map label)
                  (puthash offset fn-offset-to-label label))))
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1))))
        ;; Translate each function — use single reusable state
        (write-char-serial 84) (write-char-serial 10) ;; T
        ;; Reset all diagnostic addresses
        (td-write-u32 #x3000040 0)
        (td-write-u32 #x3000044 0)
        (td-write-u32 #x3000048 0)
        (td-write-u32 #x300004C 0)
        (td-write-u32 #x3000050 0)
        (td-write-u32 #x3000054 0)
        (td-write-u32 #x3000058 0)
        (td-write-u32 #x300005C 0)
        (td-write-u32 #x3000060 0)
        ;; Print alloc pointer before translation
        (write-char-serial 65) ;; A
        (write-char-serial 80) ;; P
        (write-char-serial 61) ;; =
        (print-dec (get-alloc-ptr))
        (write-char-serial 47) ;; /
        (print-dec (get-alloc-limit))
        (write-char-serial 10) ;; newline
        (let ((state (make-i386-translate-state)))
          (set-i386-translate-state-buf state buf)
          (set-i386-translate-state-mvm-bytes state bytecode)
          (set-i386-translate-state-function-table state fn-offset-to-label)
          (let ((rest-ft function-table)
                (i 0))
            (loop
              (when (>= i n-functions) (return nil))
              (let ((entry (car rest-ft)))
                (let ((fn-offset (car (cdr entry)))
                      (fn-length (car (cdr (cdr entry)))))
                  (let ((fn-label (aref fn-labels i)))
                    (set-i386-translate-state-mvm-length state fn-length)
                    (set-i386-translate-state-mvm-offset state fn-offset)
                    (set-i386-translate-state-label-map state (make-hash-table))
                    (i386-emit-label buf fn-label)
                    (i386-emit-prologue buf)
                    (i386-scan-branch-targets state)
                    (let ((pos fn-offset)
                          (limit (+ fn-offset fn-length)))
                      (loop
                        (when (>= pos limit) (return nil))
                        (i386-translate-one-insn buf state bytecode pos)
                        (setq pos (td-read-u32 #x3000060)))))))
            (when (zerop (mod i 50))
              (write-char-serial 46)) ;; progress dot every 50 functions
            (setq rest-ft (cdr rest-ft))
            (setq i (+ i 1)))))
        ;; Fixup labels
        (i386-fixup-labels buf)
        ;; Resolve fn-map: replace label IDs with native byte positions
        (let ((resolved-map (make-hash-table)))
          (let ((rest-ft function-table)
                (i 0))
            (loop
              (when (>= i n-functions) (return nil))
              (let ((entry (car rest-ft)))
                (let ((name (car entry)))
                  (let ((label (gethash name fn-map)))
                    (when label
                      (let ((raw-pos (gethash label (i386-buffer-labels buf))))
                        (when raw-pos
                          (puthash name resolved-map (- raw-pos 1))))))))
              (setq rest-ft (cdr rest-ft))
              (setq i (+ i 1))))
          (cons buf resolved-map))))))

;;; Generate i386 boot preamble into image buffer
(defun td-generate-i386-boot ()
  (let ((boot-buf (make-mvm-buffer)))
    (emit-i386-multiboot-header boot-buf)
    (emit-i386-entry boot-buf)
    (let ((boot-size (mvm-buffer-position boot-buf))
          (i 0))
      (loop
        (when (>= i boot-size) (return boot-size))
        (img-emit (aref (mvm-buffer-bytes boot-buf) i))
        (setq i (+ i 1))))))

;;; Assemble Gen1 i386 image from translated native code
;;; Layout: [boot preamble][JMP to kernel-main][native code][bytecodes][fn-table]...[metadata at 0x400000]
(defun td-assemble-gen1-i386 (result bc ft)
  ;; result = (cons i386-buffer fn-map)
  (let ((buf (car result))
        (fn-map (cdr result)))
    (let ((native-bytes (i386-buffer-bytes buf))
          (native-size (i386-buffer-position buf)))
      ;; 1. Init image buffer
      (img-init)
      (write-char-serial 73) (write-char-serial 49) ;; I1
      (write-char-serial 58) (write-char-serial 10)
      ;; 2. Generate i386 boot preamble
      (let ((boot-size (td-generate-i386-boot)))
        (write-char-serial 80) ;; P
        (print-dec boot-size) (write-char-serial 10)
        ;; 3. Emit JMP rel32 to kernel-main
        (let ((km-hash (td-read-u32 #x3000028)))
          (let ((km-native-off (gethash km-hash fn-map)))
            (let ((km-offset (if km-native-off km-native-off 0)))
              (img-emit 233)  ;; 0xE9 = JMP rel32
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
        (print-dec (img-pos)) (write-char-serial 10)
        ;; 5. Append MVM bytecode
        (write-char-serial 84) ;; T
        (let ((bc-len (array-length bc))
              (bc-img-offset (img-pos)))
          (let ((bi 0))
            (loop
              (when (>= bi bc-len) (return nil))
              (img-emit (aref bc bi))
              (setq bi (+ bi 1))))
          ;; 6. Append function table — raw byte copy from source image
          ;; (avoids u32 overflow for name hashes with byte 3 >= 0x80 on i386)
          (let ((ft-img-offset (img-pos))
                (src-ft-addr (+ (td-read-u32 #x3000030) (td-read-u32 #x3000014)))
                (ft-count (td-read-u32 #x3000018)))
            (let ((total-ft-bytes (* ft-count 12))
                  (bi 0))
              (loop
                (when (>= bi total-ft-bytes) (return nil))
                (img-emit (mem-ref (+ src-ft-addr bi) :u8))
                (setq bi (+ bi 1))))
            (write-char-serial 10)
            (print-dec ft-count) (write-char-serial 10)
            ;; 7. Write metadata at offset 0x400000 (VA = 0x100000 + 0x400000 = 0x500000)
            (let ((md-img-off #x2F00000))
              ;; magic MVMT = 0x544D564D — write as individual bytes
              ;; (0x544D564D > 2^30, overflows i386 30-bit fixnum)
              (let ((base (+ #x08000000 md-img-off)))
                (setf (mem-ref base :u8) #x4D)
                (setf (mem-ref (+ base 1) :u8) #x56)
                (setf (mem-ref (+ base 2) :u8) #x4D)
                (setf (mem-ref (+ base 3) :u8) #x54))
              (img-patch-u32 (+ md-img-off 4) 1)
              ;; my-architecture = 2 (i386)
              (img-patch-u32 (+ md-img-off 8) 2)
              (img-patch-u32 (+ md-img-off 12) bc-img-offset)
              (img-patch-u32 (+ md-img-off 16) bc-len)
              (img-patch-u32 (+ md-img-off 20) ft-img-offset)
              (img-patch-u32 (+ md-img-off 24) ft-count)
              (img-patch-u32 (+ md-img-off 28) (td-read-u32 #x3000050))
              (img-patch-u32 (+ md-img-off 32) native-size)
              (img-patch-u32 (+ md-img-off 36) boot-size)
              ;; kernel-main-hash-lo — raw byte copy (avoids u32 overflow on i386)
              (let ((dst-base (+ #x08000000 md-img-off 40)))
                (setf (mem-ref dst-base :u8) (mem-ref #x3000028 :u8))
                (setf (mem-ref (+ dst-base 1) :u8) (mem-ref #x3000029 :u8))
                (setf (mem-ref (+ dst-base 2) :u8) (mem-ref #x300002A :u8))
                (setf (mem-ref (+ dst-base 3) :u8) (mem-ref #x300002B :u8)))
              ;; kernel-main native offset
              (let ((km-native-off (gethash (td-read-u32 #x3000028) fn-map)))
                (if km-native-off
                    (img-patch-u32 (+ md-img-off 44) km-native-off)
                    (img-patch-u32 (+ md-img-off 44) 0)))
              ;; image-load-addr = 0x100000 (i386 Multiboot, same as x64)
              (img-patch-u32 (+ md-img-off 48) #x100000)
              ;; target-architecture (not used — i386 is target-only)
              (img-patch-u32 (+ md-img-off 52) 0)
              (img-patch-u32 (+ md-img-off 56) 0))
            ;; 8. Patch multiboot header: load_end_addr, bss_end_addr
            (let ((total-size (+ #x2F00000 64)))
              (let ((load-end (+ #x100000 total-size)))
                (img-patch-u32 20 load-end)
                (img-patch-u32 24 load-end))
              (write-char-serial 73) (write-char-serial 49) ;; I1
              (write-char-serial 61) ;; =
              (print-dec total-size) (write-char-serial 10)
              total-size)))))))

;;; Build i386 target from x64 host (cross-arch)
(defun build-i386-from-x64 (bc ft)
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

;;; Build i386 target from AArch64 host (cross-arch)
(defun build-i386-from-aarch64 (bc ft)
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
;;; i386 HOST overrides — safe for 30-bit fixnum (max ~1073741823)
;;; These override earlier defuns via last-defun-wins when running
;;; on i386, where #xFFFFFFFF and 4294967296 overflow fixnums.
;;; ============================================================

;;; Override i386-emit-mov-reg-imm: drop logand #xFFFFFFFF
;;; On i386 all values are already 32-bit; logand mask overflows.
(defun i386-emit-mov-reg-imm (buf reg imm)
  (i386-emit-byte buf (+ 184 reg))
  (i386-emit-u32 buf imm))

;;; Override i386-emit-s32: drop (+ val 4294967296) for negatives.
;;; On i386, negative fixnums already have correct bit pattern when
;;; extracted byte-by-byte with logand 255 / ash -8.
(defun i386-emit-s32 (buf val)
  (i386-emit-u32 buf val))

;;; Override td-fnv-native for i386 host: FNV init 2166136261 overflows
;;; 30-bit fixnum. Return XOR checksum instead (diagnostic only, not
;;; used for fixpoint comparison — SHA256 of full image is the proof).
(defun td-fnv-native (bytes size)
  (let ((my-arch (td-read-u32 #x3000008)))
    (if (= my-arch 2)
        ;; i386 host: XOR checksum (FNV constants overflow)
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

;;; Build i386 target from i386 host (same-arch fixpoint)
(defun build-i386-from-i386 (bc ft)
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

