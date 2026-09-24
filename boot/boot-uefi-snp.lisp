;;;; boot-uefi-snp.lisp — AMD SEV-SNP guest support for the UEFI x86-64 boot path
;;;;
;;;; Gated on *X64-SNP-MODE*:
;;;;   NIL    — nothing is emitted.  The UEFI image is byte-identical to a build
;;;;            without this file (checked by test/run-uefi-snp-test.sh).
;;;;   :SNP   — a real SEV-SNP guest.  At boot, after ExitBootServices:
;;;;              1. detect SEV via CPUID 8000_001F, SNP via MSR SEV_STATUS
;;;;                 (C001_0131), and the C-bit position via the GHCB MSR
;;;;                 protocol's SEV_INFO request (no CPUID needed for that);
;;;;              2. build the identity map with the C-bit set on every entry
;;;;                 EXCEPT the one 2 MB page at +SNP-SHARED-BASE+, and load CR3
;;;;                 with the C-bit too;
;;;;              3. turn that 2 MB region SHARED page by page (PVALIDATE to
;;;;                 rescind, then a Page State Change through the GHCB MSR
;;;;                 protocol) — it holds the GHCB and the E1000 DMA rings;
;;;;              4. register the GHCB GPA and install an IDT whose vector 29
;;;;                 (#VC) is our handler.
;;;;   :TEST  — the same handler, IDT and boot ordering on a PLAIN machine
;;;;            (no C-bit, no PSC).  #VC cannot be raised without SEV-ES, so
;;;;            vector 29 gets a thunk that pushes the IOIO exit code and the
;;;;            boot stub raises it with INT 29 in front of real IN/OUT
;;;;            instructions.  The "VMGEXIT" backend then performs the port
;;;;            operation itself from the GHCB fields.  This exercises the
;;;;            decode, the GHCB marshalling, the valid bitmap, the RIP advance
;;;;            and the IRETQ — everything except the hypervisor.
;;;;
;;;; If SNP is not active at runtime in an :SNP build, steps 2–3 are skipped
;;;; (plain identity map) and the IDT is still installed, so the same binary
;;;; boots on a plain machine.
;;;;
;;;; References: AMD GHCB spec (56421) rev 2.03 — MSR protocol §2.3.1,
;;;; IOIO/CPUID/MSR NAE events §4; AMD APM vol 2 §15.36 (SEV-SNP), PVALIDATE.
;;;; GHCB field offsets are those of struct ghcb in Linux arch/x86/include/asm/svm.h.

(in-package :modus.mvm)

(defvar *x64-snp-mode* nil
  "NIL, :SNP or :TEST — see the file header.")

;;; ---- Fixed addresses -----------------------------------------------------
(defconstant +snp-shared-base+  #x05000000
  "The ONE 2 MB identity-map page mapped without the C-bit.  Holds the E1000
   RX/TX rings and buffers (net/arch-x86.lisp: 0x05000000..0x05060000+) and the
   GHCB.  Chosen so the whole shared set fits in a single PD entry and the 2 MB
   tables never need a 4 KB split.")
(defconstant +snp-shared-size+  #x200000)
(defconstant +snp-ghcb-addr+    #x051FF000
  "GHCB: the last 4 KB page of the shared region.")
(defconstant +snp-idt-addr+     #x18000  "32 entries x 16 bytes, below 0x18200.")
(defconstant +snp-idtr-addr+    #x18800  "10-byte IDTR scratch.")
(defconstant +snp-vc-addr+      #x19000  "#VC handler code, copied here from the stub.")
(defconstant +snp-vc-thunk-addr+ #x19F00 ":TEST vector-29 thunk (push 0x7B; jmp handler).")

;;; Scratch words (0x600180.. is unused by the UEFI stub and the console)
(defconstant +snp-active-addr+  #x600180 "u32: 1 if SNP detected and the shared region is live")
(defconstant +snp-cbit-mask-addr+ #x600188 "u64: 1 << C-bit, or 0")
(defconstant +snp-cbit-pos-addr+  #x600190 "u32: C-bit position")
(defconstant +snp-vc-count-addr+  #x600198 "u64: number of #VC entries taken")
(defconstant +snp-vc-last-addr+   #x6001A0 "u64: last exit code seen")

;;; GHCB layout (struct ghcb)
(defconstant +ghcb-rax+        #x1F8)
(defconstant +ghcb-rcx+        #x308)
(defconstant +ghcb-rdx+        #x310)
(defconstant +ghcb-rbx+        #x318)
(defconstant +ghcb-exit-code+  #x390)
(defconstant +ghcb-exit-info1+ #x398)
(defconstant +ghcb-exit-info2+ #x3A0)
(defconstant +ghcb-valid-bitmap+ #x3F0)
(defconstant +ghcb-version+    #xFFA)  ; u16
(defconstant +ghcb-usage+      #xFFC)  ; u32

;;; valid_bitmap bit for a save-area field: index = offset/8, LSB-first bytes
(defun ghcb-valid-byte (field) (+ +ghcb-valid-bitmap+ (floor (floor field 8) 8)))
(defun ghcb-valid-bit  (field) (ash 1 (mod (floor field 8) 8)))

;;; MSRs and MSR-protocol codes
(defconstant +msr-ghcb+       #xC0010130)
(defconstant +msr-sev-status+ #xC0010131)
(defconstant +ghcb-msr-sev-info-req+ #x002)
(defconstant +ghcb-msr-sev-info-resp+ #x001)
(defconstant +ghcb-msr-reg-gpa-req+  #x012)
(defconstant +ghcb-msr-reg-gpa-resp+ #x013)
(defconstant +ghcb-msr-psc-req+      #x014)
(defconstant +ghcb-msr-psc-resp+     #x015)
(defconstant +ghcb-msr-terminate+    #x100)
(defconstant +psc-op-shared+ 2)   ; bits 55:52 of the PSC request

;;; NAE exit codes
(defconstant +vc-exit-rdtsc+  #x6E)
(defconstant +vc-exit-cpuid+  #x72)
(defconstant +vc-exit-ioio+   #x7B)
(defconstant +vc-exit-msr+    #x7C)
(defconstant +vc-exit-rdtscp+ #x87)

;;; ---- A tiny labelled byte assembler ---------------------------------------
;;; The handler is assembled into its own buffer (position 0 = +snp-vc-addr+)
;;; and copied into place by the stub, so it can use absolute addresses.

(defstruct snp-asm
  (buf (make-mvm-buffer))
  (labels (make-hash-table))
  (fixups nil))   ; (pos label kind)  kind = :rel32

(defun sa-pos (a) (mvm-buffer-position (snp-asm-buf a)))
(defun sa (a &rest bytes) (dolist (b bytes) (mvm-emit-byte (snp-asm-buf a) b)))
(defun sa-u32 (a v) (mvm-emit-u32 (snp-asm-buf a) v))
(defun sa-u64 (a v) (mvm-emit-u64 (snp-asm-buf a) v))
(defun sa-label (a name) (setf (gethash name (snp-asm-labels a)) (sa-pos a)))
(defun sa-rel32 (a label)
  "Emit a rel32 placeholder to LABEL (resolved by SA-FINISH)."
  (push (list (sa-pos a) label) (snp-asm-fixups a))
  (sa-u32 a 0))
(defun sa-jmp (a label) (sa a #xE9) (sa-rel32 a label))
(defun sa-call (a label) (sa a #xE8) (sa-rel32 a label))
(defun sa-jcc (a cc label)
  "CC: :e :ne :z :nz :b :ae"
  (sa a #x0F (ecase cc ((:e :z) #x84) ((:ne :nz) #x85) (:b #x82) (:ae #x83)))
  (sa-rel32 a label))
(defun sa-finish (a)
  (dolist (f (snp-asm-fixups a))
    (destructuring-bind (pos label) f
      (let ((target (gethash label (snp-asm-labels a))))
        (unless target (error "snp-asm: undefined label ~S" label))
        (let ((rel (logand (- target (+ pos 4)) #xFFFFFFFF))
              (bytes (mvm-buffer-bytes (snp-asm-buf a))))
          (dotimes (i 4) (setf (aref bytes (+ pos i)) (ldb (byte 8 (* 8 i)) rel)))))))
  (mvm-buffer-used-bytes (snp-asm-buf a)))

;;; Handy encodings (all with RBX = GHCB base)
(defun sa-mov-rax-rsp (a disp)  (sa a #x48 #x8B #x44 #x24 disp))     ; mov rax,[rsp+disp8]
(defun sa-mov-rsp-rax (a disp)  (sa a #x48 #x89 #x44 #x24 disp))     ; mov [rsp+disp8],rax
(defun sa-mov-ghcb-rax (a off)  (sa a #x48 #x89 #x83) (sa-u32 a off)) ; mov [rbx+off],rax
(defun sa-mov-rax-ghcb (a off)  (sa a #x48 #x8B #x83) (sa-u32 a off)) ; mov rax,[rbx+off]
(defun sa-mov-ghcb-imm (a off imm32) (sa a #x48 #xC7 #x83) (sa-u32 a off) (sa-u32 a imm32)) ; mov qword [rbx+off],imm32
(defun sa-set-valid (a field)                                          ; or byte [rbx+valid],bit
  (sa a #x80 #x8B) (sa-u32 a (ghcb-valid-byte field)) (sa a (ghcb-valid-bit field)))
(defun sa-cmp-rax-imm (a imm32) (sa a #x48 #x3D) (sa-u32 a imm32))
(defun sa-rax-field-to-slot (a field slot)
  "saved-register slot <- GHCB field"
  (sa-mov-rax-ghcb a field) (sa-mov-rsp-rax a slot))
(defun sa-slot-to-field (a slot field)
  "GHCB field <- saved-register slot, and mark it valid"
  (sa-mov-rax-rsp a slot) (sa-mov-ghcb-rax a field) (sa-set-valid a field))

;;; Saved-register frame inside the handler (after 6 pushes):
(defconstant +vcf-rdi+ 0) (defconstant +vcf-rsi+ 8) (defconstant +vcf-rbx+ 16)
(defconstant +vcf-rdx+ 24) (defconstant +vcf-rcx+ 32) (defconstant +vcf-rax+ 40)
(defconstant +vcf-err+ 48) (defconstant +vcf-rip+ 56)

(defun assemble-vc-handler (mode)
  "Return the #VC handler as a byte vector to be placed at +snp-vc-addr+.
   Entry: CPU frame with error code (= NAE exit code) on top of stack.
   MODE :snp — the exit is a real VMGEXIT; :test — the exit is emulated."
  (let ((a (make-snp-asm)))
    ;; ---- prologue: save the registers we use; RBX = GHCB
    (sa a #x50 #x51 #x52 #x53 #x56 #x57)                ; push rax rcx rdx rbx rsi rdi
    (sa a #x48 #xBB) (sa-u64 a +snp-ghcb-addr+)         ; mov rbx, GHCB
    (sa-mov-rax-rsp a +vcf-err+)                        ; rax = exit code
    (sa a #x48 #x8B #x74 #x24 +vcf-rip+)                ; rsi = faulting RIP
    (sa a #x48 #xFF #x04 #x25) (sa-u32 a +snp-vc-count-addr+)   ; inc qword [count]
    (sa a #x48 #x89 #x04 #x25) (sa-u32 a +snp-vc-last-addr+)    ; mov [last], rax
    ;; ---- common GHCB header: clear valid bitmap, exit code/info, version/usage
    (sa-mov-ghcb-imm a +ghcb-valid-bitmap+ 0)
    (sa-mov-ghcb-imm a (+ +ghcb-valid-bitmap+ 8) 0)
    (sa-mov-ghcb-rax a +ghcb-exit-code+) (sa-set-valid a +ghcb-exit-code+)
    (sa-mov-ghcb-imm a +ghcb-exit-info1+ 0) (sa-set-valid a +ghcb-exit-info1+)
    (sa-mov-ghcb-imm a +ghcb-exit-info2+ 0) (sa-set-valid a +ghcb-exit-info2+)
    (sa a #xC7 #x83) (sa-u32 a +ghcb-usage+) (sa-u32 a 0)          ; mov dword [rbx+usage],0
    (sa a #x66 #xC7 #x83) (sa-u32 a +ghcb-version+) (sa a 1 0)     ; mov word [rbx+version],1
    ;; ---- dispatch on exit code
    (sa-cmp-rax-imm a +vc-exit-ioio+)   (sa-jcc a :e :ioio)
    (sa-cmp-rax-imm a +vc-exit-cpuid+)  (sa-jcc a :e :cpuid)
    (sa-cmp-rax-imm a +vc-exit-msr+)    (sa-jcc a :e :msr)
    (sa-cmp-rax-imm a +vc-exit-rdtsc+)  (sa-jcc a :e :rdtsc)
    (sa-cmp-rax-imm a +vc-exit-rdtscp+) (sa-jcc a :e :rdtscp)
    (sa-jmp a :fatal)

    ;; ================= IOIO =================
    ;; Decode IN/OUT at RSI.  Forms: [66] E4/E5/E6/E7 imm8 | [66] EC/ED/EE/EF (DX)
    ;;   opcode bit1 set = OUT; bit0 set = word/dword (word iff 66 prefix); bit3 set = DX form
    ;; EDI = instruction length, ECX = 66-prefix flag, EDX = SW_EXITINFO1
    (sa-label a :ioio)
    (sa a #x31 #xFF)                                    ; xor edi,edi
    (sa a #x31 #xC9)                                    ; xor ecx,ecx
    (sa a #x0F #xB6 #x06)                               ; movzx eax, byte [rsi]
    (sa a #x3C #x66) (sa-jcc a :ne :no66)               ; cmp al,0x66 ; jne
    (sa a #x48 #xFF #xC6)                               ; inc rsi
    (sa a #xFF #xC7)                                    ; inc edi
    (sa a #xB9) (sa-u32 a 1)                            ; mov ecx,1
    (sa a #x0F #xB6 #x06)                               ; movzx eax, byte [rsi]
    (sa-label a :no66)
    (sa a #xFF #xC7)                                    ; inc edi (opcode byte)
    (sa a #xA8 #x08) (sa-jcc a :ne :dxform)             ; test al,8 ; jne
    (sa a #x0F #xB6 #x56 #x01)                          ; movzx edx, byte [rsi+1]  (imm8 port)
    (sa a #xFF #xC7)                                    ; inc edi
    (sa-jmp a :haveport)
    (sa-label a :dxform)
    (sa a #x0F #xB7 #x54 #x24 +vcf-rdx+)                ; movzx edx, word [rsp+rdx]
    (sa-label a :haveport)
    (sa a #xC1 #xE2 #x10)                               ; shl edx,16
    (sa a #xA8 #x02) (sa-jcc a :ne :isout)              ; test al,2 ; jne
    (sa a #x83 #xCA #x01)                               ; or edx,1  (TYPE = IN)
    (sa-label a :isout)
    (sa a #xA8 #x01) (sa-jcc a :ne :notbyte)            ; test al,1
    (sa a #x83 #xCA #x10) (sa-jmp a :sized)             ; or edx,SZ8
    (sa-label a :notbyte)
    (sa a #x85 #xC9) (sa-jcc a :e :isdword)             ; test ecx,ecx
    (sa a #x83 #xCA #x20) (sa-jmp a :sized)             ; or edx,SZ16
    (sa-label a :isdword)
    (sa a #x83 #xCA #x40)                               ; or edx,SZ32
    (sa-label a :sized)
    (sa a #x48 #x89 #x93) (sa-u32 a +ghcb-exit-info1+)  ; mov [rbx+info1], rdx
    (sa a #x89 #xD1)                                    ; mov ecx,edx (keep decode)
    (sa a #xF6 #xC1 #x01) (sa-jcc a :ne :io-exit)       ; test cl,1 ; jne (IN: no rax in)
    (sa-slot-to-field a +vcf-rax+ +ghcb-rax+)           ; OUT: GHCB.rax = saved rax
    (sa-label a :io-exit)
    (sa a #x51) (sa-call a :vmgexit) (sa a #x59)        ; push rcx; call; pop rcx
    (sa a #x8B #x83) (sa-u32 a +ghcb-exit-info1+)       ; mov eax,[rbx+info1]
    (sa a #x85 #xC0) (sa-jcc a :ne :fatal)              ; test eax,eax ; jne fatal
    (sa a #xF6 #xC1 #x01) (sa-jcc a :e :advance)        ; OUT: done
    (sa-mov-rax-ghcb a +ghcb-rax+)
    (sa a #xF6 #xC1 #x10) (sa-jcc a :e :in-notbyte)     ; test cl,SZ8
    (sa a #x88 #x44 #x24 +vcf-rax+) (sa-jmp a :advance) ; mov [rsp+rax], al
    (sa-label a :in-notbyte)
    (sa a #xF6 #xC1 #x20) (sa-jcc a :e :in-dword)       ; test cl,SZ16
    (sa a #x66 #x89 #x44 #x24 +vcf-rax+) (sa-jmp a :advance) ; mov [rsp+rax], ax
    (sa-label a :in-dword)
    (sa a #x89 #xC0) (sa-mov-rsp-rax a +vcf-rax+)       ; mov eax,eax (zero-extend) ; store
    (sa-label a :advance)
    (sa a #x48 #x01 #x7C #x24 +vcf-rip+)                ; add [rsp+rip], rdi
    (sa-jmp a :done)

    ;; ================= CPUID =================
    (sa-label a :cpuid)
    (sa-slot-to-field a +vcf-rax+ +ghcb-rax+)
    (sa-slot-to-field a +vcf-rcx+ +ghcb-rcx+)
    (sa-call a :vmgexit)
    (sa a #x8B #x83) (sa-u32 a +ghcb-exit-info1+) (sa a #x85 #xC0) (sa-jcc a :ne :fatal)
    (sa-rax-field-to-slot a +ghcb-rax+ +vcf-rax+)
    (sa-rax-field-to-slot a +ghcb-rbx+ +vcf-rbx+)
    (sa-rax-field-to-slot a +ghcb-rcx+ +vcf-rcx+)
    (sa-rax-field-to-slot a +ghcb-rdx+ +vcf-rdx+)
    (sa a #xBF) (sa-u32 a 2) (sa-jmp a :advance)        ; mov edi,2

    ;; ================= MSR =================
    (sa-label a :msr)
    (sa-slot-to-field a +vcf-rcx+ +ghcb-rcx+)
    (sa a #x0F #xB6 #x46 #x01)                          ; movzx eax, byte [rsi+1]
    (sa a #x3C #x30) (sa-jcc a :ne :rdmsr)              ; 0F 30 = WRMSR
    (sa-mov-ghcb-imm a +ghcb-exit-info1+ 1)             ; WRMSR
    (sa-slot-to-field a +vcf-rax+ +ghcb-rax+)
    (sa-slot-to-field a +vcf-rdx+ +ghcb-rdx+)
    (sa-call a :vmgexit)
    (sa a #x8B #x83) (sa-u32 a +ghcb-exit-info1+) (sa a #x85 #xC0) (sa-jcc a :ne :fatal)
    (sa a #xBF) (sa-u32 a 2) (sa-jmp a :advance)
    (sa-label a :rdmsr)
    (sa-call a :vmgexit)
    (sa a #x8B #x83) (sa-u32 a +ghcb-exit-info1+) (sa a #x85 #xC0) (sa-jcc a :ne :fatal)
    (sa-rax-field-to-slot a +ghcb-rax+ +vcf-rax+)
    (sa-rax-field-to-slot a +ghcb-rdx+ +vcf-rdx+)
    (sa a #xBF) (sa-u32 a 2) (sa-jmp a :advance)

    ;; ================= RDTSC / RDTSCP =================
    (sa-label a :rdtsc)
    (sa-call a :vmgexit)
    (sa a #x8B #x83) (sa-u32 a +ghcb-exit-info1+) (sa a #x85 #xC0) (sa-jcc a :ne :fatal)
    (sa-rax-field-to-slot a +ghcb-rax+ +vcf-rax+)
    (sa-rax-field-to-slot a +ghcb-rdx+ +vcf-rdx+)
    (sa a #xBF) (sa-u32 a 2) (sa-jmp a :advance)
    (sa-label a :rdtscp)
    (sa-call a :vmgexit)
    (sa a #x8B #x83) (sa-u32 a +ghcb-exit-info1+) (sa a #x85 #xC0) (sa-jcc a :ne :fatal)
    (sa-rax-field-to-slot a +ghcb-rax+ +vcf-rax+)
    (sa-rax-field-to-slot a +ghcb-rdx+ +vcf-rdx+)
    (sa-rax-field-to-slot a +ghcb-rcx+ +vcf-rcx+)
    (sa a #xBF) (sa-u32 a 3) (sa-jmp a :advance)

    ;; ================= epilogue =================
    (sa-label a :done)
    (sa a #x5F #x5E #x5B #x5A #x59 #x58)                ; pop rdi rsi rbx rdx rcx rax
    (sa a #x48 #x83 #xC4 #x08)                          ; add rsp,8 (error code)
    (sa a #x48 #xCF)                                    ; iretq

    ;; ================= fatal =================
    (sa-label a :fatal)
    (ecase mode
      (:snp  ;; GHCB MSR terminate request, then halt
       (sa a #xB9) (sa-u32 a +msr-ghcb+)
       (sa a #xB8) (sa-u32 a +ghcb-msr-terminate+)
       (sa a #x31 #xD2) (sa a #x0F #x30)                ; xor edx,edx ; wrmsr
       (sa a #xF3 #x0F #x01 #xD9))                      ; rep vmmcall
      (:test ;; write '!' to COM1 directly so the failure is visible
       (sa a #xB0 (char-code #\!)) (sa a #x66 #xBA #xF8 #x03) (sa a #xEE)))
    (sa-label a :halt)
    (sa a #xF4) (sa-jmp a :halt)                        ; hlt ; jmp halt

    ;; ================= the exit =================
    (sa-label a :vmgexit)
    (ecase mode
      (:snp
       (sa a #xB9) (sa-u32 a +msr-ghcb+)                ; mov ecx, GHCB MSR
       (sa a #xB8) (sa-u32 a +snp-ghcb-addr+)           ; mov eax, GHCB GPA
       (sa a #x31 #xD2) (sa a #x0F #x30)                ; xor edx,edx ; wrmsr
       (sa a #xF3 #x0F #x01 #xD9)                       ; rep vmmcall = VMGEXIT
       (sa a #xC3))
      (:test
       ;; Emulate the hypervisor for IOIO only: perform the port op described
       ;; by the GHCB.  Anything else is answered with SW_EXITINFO1 = 1.
       (sa-mov-rax-ghcb a +ghcb-exit-code+)
       (sa-cmp-rax-imm a +vc-exit-ioio+) (sa-jcc a :ne :emu-err)
       (sa a #x8B #x93) (sa-u32 a +ghcb-exit-info1+)    ; mov edx,[rbx+info1]
       (sa a #x89 #xD1)                                 ; mov ecx,edx
       (sa a #xC1 #xEA #x10)                            ; shr edx,16  (dx = port)
       (sa-mov-rax-ghcb a +ghcb-rax+)
       (sa a #xF6 #xC1 #x01) (sa-jcc a :ne :emu-in)
       (sa a #xF6 #xC1 #x10) (sa-jcc a :e :emu-out-nb)
       (sa a #xEE) (sa-jmp a :emu-ok)                   ; out dx,al
       (sa-label a :emu-out-nb)
       (sa a #xF6 #xC1 #x20) (sa-jcc a :e :emu-out-dw)
       (sa a #x66 #xEF) (sa-jmp a :emu-ok)              ; out dx,ax
       (sa-label a :emu-out-dw)
       (sa a #xEF) (sa-jmp a :emu-ok)                   ; out dx,eax
       (sa-label a :emu-in)
       (sa a #xF6 #xC1 #x10) (sa-jcc a :e :emu-in-nb)
       (sa a #xEC #x0F #xB6 #xC0) (sa-jmp a :emu-store) ; in al,dx ; movzx eax,al
       (sa-label a :emu-in-nb)
       (sa a #xF6 #xC1 #x20) (sa-jcc a :e :emu-in-dw)
       (sa a #x66 #xED #x0F #xB7 #xC0) (sa-jmp a :emu-store) ; in ax,dx ; movzx eax,ax
       (sa-label a :emu-in-dw)
       (sa a #xED)                                      ; in eax,dx
       (sa-label a :emu-store)
       (sa-mov-ghcb-rax a +ghcb-rax+)
       (sa-label a :emu-ok)
       (sa-mov-ghcb-imm a +ghcb-exit-info1+ 0)
       (sa a #xC3)
       (sa-label a :emu-err)
       (sa-mov-ghcb-imm a +ghcb-exit-info1+ 1)
       (sa a #xC3)))
    (sa-finish a)))

;;; ---- Stub-side emitters (into the UEFI entry stub buffer) --------------

(defun snp-emit-abs-u32-store (buf addr imm32)
  "mov dword [addr32], imm32"
  (uefi-emit-store-imm32-abs32 buf addr imm32))

(defun emit-snp-detect (buf)
  "Before the page tables: decide whether SNP is active and learn the C-bit.
   Writes +snp-active-addr+ (u32) and +snp-cbit-mask-addr+ (u64).
   :TEST mode writes active=0, mask=0 and probes nothing."
  (snp-emit-abs-u32-store buf +snp-active-addr+ 0)
  (snp-emit-abs-u32-store buf +snp-cbit-mask-addr+ 0)
  (snp-emit-abs-u32-store buf (+ +snp-cbit-mask-addr+ 4) 0)
  (snp-emit-abs-u32-store buf +snp-cbit-pos-addr+ 0)
  (snp-emit-abs-u32-store buf +snp-vc-count-addr+ 0)
  (snp-emit-abs-u32-store buf (+ +snp-vc-count-addr+ 4) 0)
  (when (eq *x64-snp-mode* :snp)
    (let ((a (make-snp-asm)))
      ;; CPUID 8000_001F: EAX bit 1 = SEV supported.  Under SNP this CPUID
      ;; is itself a #VC, taken by the FIRMWARE's handler, which is still
      ;; installed (we have not touched the IDT or CR3 yet).
      (sa a #xB8) (sa-u32 a #x8000001F) (sa a #x31 #xC9) (sa a #x0F #xA2)  ; cpuid
      (sa a #xA9) (sa-u32 a 2) (sa-jcc a :e :nosnp)                          ; test eax,2
      ;; MSR SEV_STATUS bit 2 = SNP active
      (sa a #xB9) (sa-u32 a +msr-sev-status+) (sa a #x0F #x32)               ; rdmsr
      (sa a #xA9) (sa-u32 a 4) (sa-jcc a :e :nosnp)                          ; test eax,4
      ;; GHCB MSR protocol: SEV_INFO request -> C-bit position in bits 31:24
      (sa a #xB9) (sa-u32 a +msr-ghcb+)
      (sa a #xB8) (sa-u32 a +ghcb-msr-sev-info-req+) (sa a #x31 #xD2) (sa a #x0F #x30) ; wrmsr
      (sa a #xF3 #x0F #x01 #xD9)                                             ; vmgexit
      (sa a #x0F #x32)                                                       ; rdmsr
      (sa a #x89 #xC1) (sa a #x81 #xE1) (sa-u32 a #xFFF)                     ; mov ecx,eax; and ecx,0xFFF
      (sa a #x83 #xF9 +ghcb-msr-sev-info-resp+) (sa-jcc a :ne :fatal)        ; cmp ecx,1
      (sa a #xC1 #xE8 #x18) (sa a #x83 #xE0 #x3F)                            ; shr eax,24 ; and eax,0x3F
      (sa a #x89 #x04 #x25) (sa-u32 a +snp-cbit-pos-addr+)                   ; mov [pos],eax
      (sa a #x89 #xC1)                                                       ; mov ecx,eax
      (sa a #x48 #xC7 #xC2) (sa-u32 a 1)                                     ; mov rdx,1
      (sa a #x48 #xD3 #xE2)                                                  ; shl rdx,cl
      (sa a #x48 #x89 #x14 #x25) (sa-u32 a +snp-cbit-mask-addr+)             ; mov [mask],rdx
      (sa a #xC7 #x04 #x25) (sa-u32 a +snp-active-addr+) (sa-u32 a 1)        ; mov dword [active],1
      (sa-jmp a :end)
      (sa-label a :fatal)
      (sa a #xF4) (sa-jmp a :fatal)
      (sa-label a :nosnp)
      (sa-label a :end)
      (let ((bytes (sa-finish a)))
        (loop for b across bytes do (mvm-emit-byte buf b))))))

(defun emit-snp-load-cbit-rbx (buf)
  "mov rbx, [+snp-cbit-mask-addr+] — the page-table build ORs RBX into every
   table pointer and every 2 MB entry.  Zero when SNP is not active."
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x1C)
  (mvm-emit-byte buf #x25) (mvm-emit-u32 buf +snp-cbit-mask-addr+))

(defun emit-snp-or-rax-rbx (buf)
  "or rax, rbx"
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x09) (mvm-emit-byte buf #xD8))

(defun emit-snp-pd-entry-fixup (buf)
  "Inside the PD fill loop, after `mov rax,rdx ; or rax,0x83`:
     or rax, rbx                 ; C-bit
     cmp rdx, +snp-shared-base+  ; the one shared 2 MB page
     jne +3
     xor rax, rbx                ; ... gets the C-bit taken back out"
  (emit-snp-or-rax-rbx buf)
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x81) (mvm-emit-byte buf #xFA)
  (mvm-emit-u32 buf +snp-shared-base+)
  (mvm-emit-byte buf #x75) (mvm-emit-byte buf #x03)
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xD8))

(defun emit-snp-post-cr3 (buf)
  "After CR3/GDT/segments: make the shared region shared, register the GHCB,
   copy the #VC handler into place and load an IDT with vector 29.
   The PSC/registration part runs only if +snp-active-addr+ is 1."
  (let ((a (make-snp-asm))
        (handler (assemble-vc-handler *x64-snp-mode*)))
    (when (> (length handler) (- +snp-vc-thunk-addr+ +snp-vc-addr+))
      (error "#VC handler too large: ~D bytes" (length handler)))
    ;; -- (a) SNP-active only: PSC + GHCB registration
    (sa a #x8B #x04 #x25) (sa-u32 a +snp-active-addr+)         ; mov eax,[active]
    (sa a #x85 #xC0) (sa-jcc a :e :install)                    ; test eax,eax ; jz
    ;; rsi = page cursor over the shared region
    (sa a #x48 #xC7 #xC6) (sa-u32 a +snp-shared-base+)         ; mov rsi, base
    (sa-label a :psc-loop)
    ;;   PVALIDATE rsi, 4K, rescind:  rax=va rcx=0 rdx=0 ; F2 0F 01 FF
    (sa a #x48 #x89 #xF0) (sa a #x31 #xC9) (sa a #x31 #xD2)
    (sa a #xF2 #x0F #x01 #xFF)
    ;;   MSR PSC request: GHCBData = 0x014 | (2 << 52) | gfn<<12
    (sa a #x48 #x89 #xF0)                                      ; mov rax,rsi (4K aligned)
    (sa a #x48 #x83 #xC8 +ghcb-msr-psc-req+)                   ; or rax, 0x14
    (sa a #x48 #xBA) (sa-u64 a (ash +psc-op-shared+ 52))       ; mov rdx, 2<<52
    (sa a #x48 #x09 #xD0)                                      ; or rax,rdx
    (sa a #x48 #x89 #xC2) (sa a #x48 #xC1 #xEA #x20)           ; mov rdx,rax ; shr rdx,32
    (sa a #xB9) (sa-u32 a +msr-ghcb+) (sa a #x0F #x30)         ; wrmsr
    (sa a #xF3 #x0F #x01 #xD9)                                 ; vmgexit
    (sa a #x0F #x32)                                           ; rdmsr
    (sa a #x89 #xC1) (sa a #x81 #xE1) (sa-u32 a #xFFF)         ; ecx = eax & 0xFFF
    (sa a #x83 #xF9 +ghcb-msr-psc-resp+) (sa-jcc a :ne :fatal) ; must be 0x015
    (sa a #x85 #xD2) (sa-jcc a :ne :fatal)                     ; error code (hi 32) must be 0
    (sa a #x48 #x81 #xC6) (sa-u32 a #x1000)                    ; add rsi,4096
    (sa a #x48 #x81 #xFE) (sa-u32 a (+ +snp-shared-base+ +snp-shared-size+)) ; cmp rsi,end
    (sa-jcc a :b :psc-loop)
    ;;   Register the GHCB GPA
    (sa a #xB9) (sa-u32 a +msr-ghcb+)
    (sa a #xB8) (sa-u32 a (logior +snp-ghcb-addr+ +ghcb-msr-reg-gpa-req+))
    (sa a #x31 #xD2) (sa a #x0F #x30) (sa a #xF3 #x0F #x01 #xD9) (sa a #x0F #x32)
    (sa a #x89 #xC1) (sa a #x81 #xE1) (sa-u32 a #xFFF)
    (sa a #x83 #xF9 +ghcb-msr-reg-gpa-resp+) (sa-jcc a :ne :fatal)
    ;;   Zero the (now shared) GHCB page
    (sa a #x48 #xC7 #xC7) (sa-u32 a +snp-ghcb-addr+)           ; mov rdi, GHCB
    (sa a #xB9) (sa-u32 a 512) (sa a #x31 #xC0) (sa a #xFC) (sa a #xF3 #x48 #xAB) ; rep stosq
    ;;   GHCB MSR = GHCB GPA for the handler's VMGEXITs
    (sa a #xB9) (sa-u32 a +msr-ghcb+) (sa a #xB8) (sa-u32 a +snp-ghcb-addr+)
    (sa a #x31 #xD2) (sa a #x0F #x30)
    ;;   The GOP framebuffer is hypervisor memory: with the C-bit set on its
    ;;   identity mapping every store would be an MMIO #VC (NPF), which this
    ;;   handler does not implement.  Mark it invalid so the console stays
    ;;   serial-only under SNP (+fb-valid-addr+ = 0x600120).
    (sa a #xC7 #x04 #x25) (sa-u32 a #x600120) (sa-u32 a 0)
    ;; -- (b) always: copy handler, build IDT, lidt
    (sa-label a :install)
    ;;   lea rsi,[rip+HANDLER]; mov rdi,VC; mov ecx,len; rep movsb
    (sa a #x48 #x8D #x35) (sa-rel32 a :handler-bytes)
    (sa a #x48 #xC7 #xC7) (sa-u32 a +snp-vc-addr+)
    (sa a #xB9) (sa-u32 a (length handler))
    (sa a #xFC #xF3 #xA4)
    ;;   clear the IDT (512 bytes)
    (sa a #x48 #xC7 #xC7) (sa-u32 a +snp-idt-addr+)
    (sa a #xB9) (sa-u32 a 64) (sa a #x31 #xC0) (sa a #xF3 #x48 #xAB)   ; 64 x stosq
    ;;   vector 29 -> handler (:snp) or thunk (:test).  Selector 0x08 = the
    ;;   stub's 64-bit code segment; type 0x8E = present, DPL0, interrupt gate.
    (let* ((target (if (eq *x64-snp-mode* :test) +snp-vc-thunk-addr+ +snp-vc-addr+))
           (entry (+ +snp-idt-addr+ (* 29 16)))
           (w0 (logior (logand target #xFFFF) (ash #x08 16)))
           (w1 (logior (ash #x8E 8) (ash (logand (ash target -16) #xFFFF) 16))))
      (sa a #x48 #xC7 #xC7) (sa-u32 a entry)                   ; mov rdi, entry
      (sa a #xC7 #x07) (sa-u32 a w0)                           ; mov dword [rdi],w0
      (sa a #xC7 #x47 #x04) (sa-u32 a w1)                      ; mov dword [rdi+4],w1
      (sa a #xC7 #x47 #x08) (sa-u32 a (ash target -32))        ; [rdi+8] = offset hi
      (sa a #xC7 #x47 #x0C) (sa-u32 a 0))
    (when (eq *x64-snp-mode* :test)
      ;;   thunk at +snp-vc-thunk-addr+:  6A 7B (push 0x7B) ; E9 rel32 -> handler
      (let ((rel (logand (- +snp-vc-addr+ (+ +snp-vc-thunk-addr+ 7)) #xFFFFFFFF)))
        (sa a #x48 #xC7 #xC7) (sa-u32 a +snp-vc-thunk-addr+)
        (sa a #xC7 #x07) (sa a #x6A +vc-exit-ioio+ #xE9 (ldb (byte 8 0) rel))
        (sa a #xC7 #x47 #x04) (sa a (ldb (byte 8 8) rel) (ldb (byte 8 16) rel) (ldb (byte 8 24) rel) #x90)))
    ;;   IDTR at +snp-idtr-addr+: limit 511, base
    (sa a #x66 #xC7 #x04 #x25) (sa-u32 a +snp-idtr-addr+) (sa a #xFF #x01)      ; mov word [idtr],511
    (sa a #xC7 #x04 #x25) (sa-u32 a (+ +snp-idtr-addr+ 2)) (sa-u32 a +snp-idt-addr+)
    (sa a #xC7 #x04 #x25) (sa-u32 a (+ +snp-idtr-addr+ 6)) (sa-u32 a 0)
    (sa a #x0F #x01 #x1C #x25) (sa-u32 a +snp-idtr-addr+)                       ; lidt [idtr]
    (sa-jmp a :end)
    (sa-label a :fatal)
    (sa a #xF4) (sa-jmp a :fatal)
    ;;   the handler bytes ride along in the stub, skipped by the jmp above
    (sa-label a :handler-bytes)
    (loop for b across handler do (sa a b))
    (sa-label a :end)
    (let ((bytes (sa-finish a)))
      (loop for b across bytes do (mvm-emit-byte buf b)))))

(defun emit-snp-selftest (buf)
  ":TEST mode only, after the serial port is initialised.  Raises the fake #VC
   in front of real port instructions.  Expected serial output:  VC+wi5
     V C  — two OUT DX,AL through the handler
     +    — IN AL,DX of LSR (0x3FD) through the handler; bit 5 (THR empty) set
     w    — IN AX,DX (66-prefixed word form) through the handler, then a byte OUT
     i    — IN AL,imm8 (E4 61) through the handler, then 'i' written directly
     5    — the #VC entry count as a digit
   A '!' anywhere means the handler took its fatal path."
  (when (eq *x64-snp-mode* :test)
    (let ((a (make-snp-asm)))
      (dolist (ch '(#\V #\C))
        (sa a #xB0 (char-code ch)) (sa a #x66 #xBA #xF8 #x03)   ; mov al,ch ; mov dx,0x3F8
        (sa a #xCD 29) (sa a #xEE))                            ; int 29 ; out dx,al
      ;; IN AL,DX from LSR
      (sa a #x66 #xBA #xFD #x03) (sa a #xCD 29) (sa a #xEC)   ; int 29 ; in al,dx
      (sa a #xA8 #x20) (sa-jcc a :ne :thr-ok)                 ; test al,0x20
      (sa a #xB0 (char-code #\-)) (sa-jmp a :thr-out)
      (sa-label a :thr-ok) (sa a #xB0 (char-code #\+))
      (sa-label a :thr-out) (sa a #x66 #xBA #xF8 #x03) (sa a #xEE)
      ;; IN AX,DX (66 ED) from 0x3FD
      (sa a #x66 #xBA #xFD #x03) (sa a #xCD 29) (sa a #x66 #xED)
      (sa a #xB0 (char-code #\w)) (sa a #x66 #xBA #xF8 #x03) (sa a #xEE)
      ;; IN AL,imm8 (E4 61) — port 0x61
      (sa a #xCD 29) (sa a #xE4 #x61)
      (sa a #xB0 (char-code #\i)) (sa a #x66 #xBA #xF8 #x03) (sa a #xEE)
      ;; count digit
      (sa a #x8B #x04 #x25) (sa-u32 a +snp-vc-count-addr+)    ; mov eax,[count]
      (sa a #x04 (char-code #\0)) (sa a #x66 #xBA #xF8 #x03) (sa a #xEE)  ; add al,'0' ; out
      (sa a #xB0 13) (sa a #xEE) (sa a #xB0 10) (sa a #xEE)   ; CR LF
      (let ((bytes (sa-finish a)))
        (loop for b across bytes do (mvm-emit-byte buf b))))))
