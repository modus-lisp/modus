;;; Preprocess source text: fix &key calls for bare-metal MVM compilation.
;;; The MVM compiler doesn't handle keyword arguments at call sites — it
;;; treats :hw, :shift etc. as positional args. Transform to positional calls.

;; Preprocess: manual string replacement for &key patterns
(flet ((replace-all (old new string)
         (with-output-to-string (s)
           (let ((old-len (length old))
                 (pos 0))
             (loop
               (let ((found (search old string :start2 pos)))
                 (if found
                     (progn
                       (write-string string s :start pos :end found)
                       (write-string new s)
                       (setf pos (+ found old-len)))
                     (progn
                       (write-string string s :start pos)
                       (return)))))))))
  ;; &key in defun params → positional
  (setf *mvm-source-text* (replace-all "&key (hw 0)" "hw" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "&key (shift 0)" "shift" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "&key (shift :lsl) (amount 0)"
                                        "shift amount" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "&key (option #xB)" "option" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "&key (start 0) (end nil)"
                                        "start end" *mvm-source-text*))
  ;; Call sites: strip keyword indicators
  (setf *mvm-source-text* (replace-all " :hw " " " *mvm-source-text*))
  (setf *mvm-source-text* (replace-all " :shift :lsl :amount " " 0 " *mvm-source-text*))
  (setf *mvm-source-text* (replace-all " :shift :lsr :amount " " 1 " *mvm-source-text*))
  (setf *mvm-source-text* (replace-all " :shift :asr :amount " " 2 " *mvm-source-text*))
  (setf *mvm-source-text* (replace-all " :option " " " *mvm-source-text*))
  ;; ecase shift → shift directly (already numeric after keyword stripping)
  (setf *mvm-source-text* (replace-all "(ecase shift (:lsl 0) (:lsr 1) (:asr 2))"
                                        "shift" *mvm-source-text*))
  ;; i386 translator preprocessing:
  ;; 1. Expand macrolet in i386-translate-insn (MVM compiler can't handle macrolet)
  (setf *mvm-source-text*
        (replace-all "(macrolet ((op= (sym) `(= opcode ,sym)))"
                     "(progn" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "(op= " "(= opcode " *mvm-source-text*))
  ;; 2. Replace first/second/third (not in prelude) with car/cadr/caddr
  (setf *mvm-source-text* (replace-all "(first " "(car " *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "(second " "(cadr " *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "(third " "(caddr " *mvm-source-text*))
  ;; 3. Replace /= with (not (= ...)) — single occurrence in i386-emit-modrm-mem
  (setf *mvm-source-text*
        (replace-all "(/= base-reg +i386-ebp+)" "(not (= base-reg +i386-ebp+))" *mvm-source-text*))
  ;; 4. Replace loop-for in TRAP handler (overflow args copy)
  (setf *mvm-source-text*
        (replace-all
         "(loop for param-idx from 4 below code
                      for k from 0  ;; k-th overflow arg
                      do (let ((src-off (+ 16 (* k 4)))
                               (dst-off (+ +frame-slot-base+ (* param-idx -4))))
                           (i386-emit-mov-reg-mem buf +scratch0+ +i386-ebp+ src-off)
                           (i386-emit-mov-mem-reg buf +i386-ebp+ dst-off +scratch0+)))"
         "(let ((param-idx 4))
                  (loop
                    (when (>= param-idx code) (return nil))
                    (let ((k (- param-idx 4)))
                      (let ((src-off (+ 16 (* k 4))))
                        (let ((dst-off (+ +frame-slot-base+ (* param-idx -4))))
                          (i386-emit-mov-reg-mem buf +scratch0+ +i386-ebp+ src-off)
                          (i386-emit-mov-mem-reg buf +i386-ebp+ dst-off +scratch0+))))
                    (setq param-idx (+ param-idx 1))))"
         *mvm-source-text*))
  ;; 5. Replace (vector ...) in *i386-vreg-map* with nil (init via explicit function)
  (setf *mvm-source-text*
        (replace-all
         "(defparameter *i386-vreg-map*
  (vector +i386-esi+     ; V0  -> ESI
          +i386-edi+     ; V1  -> EDI
          nil nil         ; V2, V3 (spill -- only 2 arg regs on i386)
          +i386-ebx+     ; V4  -> EBX
          nil nil nil     ; V5-V7 (spill)
          nil nil nil nil ; V8-V11 (spill)
          nil nil nil nil ; V12-V15 (spill)
          +i386-eax+     ; VR  -> EAX
          nil             ; VA  -> spill (alloc pointer)
          nil             ; VL  -> spill (alloc limit)
          nil             ; VN  -> spill (NIL constant)
          +i386-esp+     ; VSP -> ESP
          +i386-ebp+     ; VFP -> EBP
          nil))"
         "(defvar *i386-vreg-map* nil)"
         *mvm-source-text*))
  ;;; ============================================================
  ;;; ARM32 translator preprocessing
  ;;; ============================================================
  ;; 1. Replace (vector ...) in *arm32-vreg-map* with nil (init via explicit function)
  (setf *mvm-source-text*
        (replace-all
         "(defparameter *arm32-vreg-map*
  (vector +arm-r0+  +arm-r1+  +arm-r2+  +arm-r3+    ; V0-V3
          +arm-r4+  +arm-r5+  +arm-r6+  +arm-r7+    ; V4-V7
          nil nil nil nil                              ; V8-V11 (spill)
          nil nil nil nil                              ; V12-V15 (spill)
          +arm-r0+                                     ; VR (aliases V0)
          +arm-r9+                                     ; VA
          +arm-r10+                                    ; VL
          +arm-r8+                                     ; VN
          +arm-sp+                                     ; VSP
          +arm-r11+                                    ; VFP
          nil))"
         "(defvar *arm32-vreg-map* nil)"
         *mvm-source-text*))
  ;; 2. Replace #.+op-xxx+ in ARM32 translator with numeric values
  (setf *mvm-source-text* (replace-all "#.+op-nop+" "0" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-break+" "1" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-trap+" "2" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-mov+" "16" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-li+" "17" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-push+" "18" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-pop+" "19" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-add+" "32" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-sub+" "33" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-mul+" "34" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-div+" "35" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-mod+" "36" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-neg+" "37" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-inc+" "38" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-dec+" "39" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-band+" "40" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-bor+" "41" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-bxor+" "42" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-shlv+" "47" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-shl+" "43" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-shr+" "44" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-sar+" "45" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-ldb+" "46" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-cmp-imm+" "51" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-test-imm+" "52" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-cmp+" "48" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-test+" "49" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-sarv+" "50" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-br+" "64" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-beq+" "65" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-bne+" "66" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-blt+" "67" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-bge+" "68" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-ble+" "69" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-bgt+" "70" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-bnull+" "71" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-bnnull+" "72" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-car+" "80" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-cdr+" "81" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-cons+" "82" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-setcar+" "83" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-setcdr+" "84" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-consp+" "85" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-atom+" "86" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-alloc-obj+" "96" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-obj-ref+" "97" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-obj-set+" "98" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-obj-tag+" "99" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-obj-subtag+" "100" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-aref+" "101" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-aset+" "102" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-array-len+" "103" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-alloc-array+" "104" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-load+" "112" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-store+" "113" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-fence+" "114" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-call+" "128" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-call-ind+" "129" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-ret+" "130" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-tailcall+" "131" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-alloc-cons+" "136" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-gc-check+" "137" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-write-barrier+" "138" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-save-ctx+" "144" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-restore-ctx+" "145" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-yield+" "146" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-atomic-xchg+" "147" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-io-read+" "160" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-io-write+" "161" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-halt+" "162" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-cli+" "163" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-sti+" "164" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-percpu-ref+" "165" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-percpu-set+" "166" *mvm-source-text*))
  (setf *mvm-source-text* (replace-all "#.+op-fn-addr+" "167" *mvm-source-text*))
  ;; 3. plusp — defined as function in *fixpoint-extra-source* instead of replacing
  (format t "Preprocessed source (~D chars)~%" (length *mvm-source-text*)))

(format t "MVM system source: ~D chars across ~D files~%"
        (length *mvm-source-text*) (length *mvm-source-files*))
