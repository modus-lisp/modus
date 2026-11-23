;;;; packages.lisp - Package definitions for MVM system
;;;;
;;;; Defines :modus.asm (x86-64 assembler, used by x64 translator)

(defpackage :modus.asm
  (:use :cl)
  (:export
   ;; Code buffer
   #:make-code-buffer
   #:emit-byte
   #:emit-bytes
   #:emit-u16
   #:emit-u32
   #:emit-u64
   #:emit-s32
   #:code-buffer-bytes
   #:code-buffer-position
   #:%seg-begin
   #:+win-tls-save+
   #:+win-tls-window+
   #:+win-tls-percpu+
   ;; Labels
   #:make-label
   #:label-p
   #:label-position
   #:emit-label
   #:emit-label-ref-rel32
   #:fixup-labels
   ;; Registers
   #:rax #:rcx #:rdx #:rbx #:rsp #:rbp #:rsi #:rdi
   #:r8 #:r9 #:r10 #:r11 #:r12 #:r13 #:r14 #:r15
   #:eax #:ecx #:edx #:ebx #:esp #:ebp #:esi #:edi
   #:al #:cl #:dl #:bl #:spl #:bpl #:sil #:dil
   #:reg-code #:reg-size #:reg-needs-rex-p #:reg-extended-p
   ;; Instructions
   #:emit-ret #:emit-nop #:emit-push #:emit-pop
   #:emit-mov-reg-reg #:emit-mov-reg-imm #:emit-mov-reg-mem #:emit-mov-mem-reg
   #:emit-add-reg-reg #:emit-add-reg-imm #:emit-sub-reg-reg #:emit-sub-reg-imm
   #:emit-cmp-reg-reg #:emit-cmp-reg-imm #:emit-and-reg-reg #:emit-and-reg-imm
   #:emit-or-reg-reg #:emit-or-reg-imm #:emit-xor-reg-reg #:emit-xor-reg-imm
   #:emit-shl-reg-imm #:emit-shr-reg-imm #:emit-sar-reg-imm
   #:emit-shl-reg-cl #:emit-shr-reg-cl #:emit-sar-reg-cl
   #:emit-test-reg-reg #:emit-test-reg-imm
   #:emit-jmp #:emit-jmp-reg #:emit-call #:emit-call-reg #:emit-jcc
   #:emit-lea #:emit-int
   #:*registers* #:*condition-codes*))
