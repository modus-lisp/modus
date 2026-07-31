;;;; boot-i386.lisp - i386 (32-bit x86) Boot Sequence for Modus
;;;; LEGACY MINI-LISP PATH.  This image runs mvm/repl-source.lisp — a separate
;;;; 708-line toy Lisp with its own reader, printer and evaluator, NOT the real
;;;; Common Lisp stack.  The real CL image on i386 is mvm/build-i386-cli.lisp
;;;; (prelude + gc + rt + the cl-*.lisp bridge + crypto), driven by
;;;; scripts/run-i386.sh.  Retiring this second Lisp is its own workstream.
;;;;
;;;; Boot protocol for i386 (Multiboot1 via QEMU -kernel or GRUB):
;;;;   1. Multiboot1 header at 0x100000 (1MB)
;;;;   2. GRUB hands off in 32-bit protected mode (A20 enabled, paging off)
;;;;   3. EAX = Multiboot magic (0x2BADB002), EBX = multiboot info pointer
;;;;   4. Set up flat GDT (code + data segments)
;;;;   5. Set up stack
;;;;   6. Initialize serial console (COM1 @ 0x3F8)
;;;;   7. Initialize allocation state in stack frame:
;;;;      VA (alloc ptr) -> [EBP - 16]
;;;;      VL (alloc limit) -> [EBP - 20]
;;;;      VN (NIL const) -> [EBP - 24]
;;;;      (i386 has no dedicated alloc registers -- all spilled)
;;;;   8. Call kernel-main
;;;;
;;;; This is simpler than x86-64: no long mode transition, no page tables
;;;; needed for identity-mapped 32-bit operation.

(in-package :modus.mvm)

;;; ============================================================
;;; i386 Boot Constants
;;; ============================================================

(defconstant +i386-kernel-load-addr+ #x100000)     ; 1MB - standard Multiboot load
(defconstant +i386-stack-top+        #x400000)     ; 4MB - initial stack top

;; Memory regions (32-bit addresses)
(defconstant +i386-cons-base+        #x00800000)   ; 8MB - cons space
(defconstant +i386-general-base+     #x07800000)   ; 120MB - general heap (cross-compile needs ~70MB for 32-bit arrays)

;; Per-CPU structures
(defconstant +i386-percpu-base+      #x5B0000)     ; Per-CPU data area (above ssh-ipc extent)
(defconstant +i386-percpu-stride+    #x20)          ; 32 bytes per CPU (4-byte pointers)

;; Multiboot1 constants
(defconstant +i386-multiboot1-magic+    #x1BADB002)
(defconstant +i386-multiboot1-aout-kludge+ #x00010000) ; Use address fields
(defconstant +i386-multiboot1-flags+    (logior #x00000003  ; Page-align + memory info
                                                +i386-multiboot1-aout-kludge+))
(defconstant +i386-multiboot1-bl-magic+ #x2BADB002) ; Bootloader writes this to EAX

;; Serial port
(defconstant +i386-com1-base+ #x3F8)

;;; ============================================================
;;; i386 Multiboot1 Header
;;; ============================================================

(defun emit-i386-multiboot-header (buf)
  "Emit Multiboot1 header with aout-kludge for QEMU -kernel loading.
   With aout-kludge, header is 32 bytes:
     magic(4) flags(4) checksum(4) header_addr(4) load_addr(4)
     load_end_addr(4) bss_end_addr(4) entry_addr(4)
   Entry code starts immediately after this header at load_addr + 32."
  (let* ((load-addr +i386-kernel-load-addr+)
         (entry-addr (+ load-addr 32)))  ; entry right after 32-byte header
    ;; Magic number
    (mvm-emit-u32 buf +i386-multiboot1-magic+)
    ;; Flags: page-align + memory info + aout-kludge
    (mvm-emit-u32 buf +i386-multiboot1-flags+)
    ;; Checksum: -(magic + flags) mod 2^32
    (mvm-emit-u32 buf (logand #xFFFFFFFF
                               (- (+ +i386-multiboot1-magic+
                                     +i386-multiboot1-flags+))))
    ;; Address fields (aout-kludge)
    (mvm-emit-u32 buf load-addr)    ; header_addr (where header is in memory)
    (mvm-emit-u32 buf load-addr)    ; load_addr (where to load file)
    (mvm-emit-u32 buf 0)            ; load_end_addr (0 = load entire file)
    (mvm-emit-u32 buf 0)            ; bss_end_addr
    (mvm-emit-u32 buf entry-addr))) ; entry_addr

;;; ============================================================
;;; i386 Serial Port Initialization
;;; ============================================================

(defun i386-init-serial (buf)
  "Emit i386 machine code to initialize COM1 (0x3F8) for 115200 8N1.
   Uses: DX (port number), AL (data byte), OUT instruction.
   Encoding: mov dx, imm16 = 66 BA lo hi
             mov al, imm8  = B0 imm
             out dx, al    = EE"
  (labels ((serial-out (port val)
             ;; mov dx, port (16-bit immediate with operand size prefix)
             (mvm-emit-byte buf #x66)
             (mvm-emit-byte buf #xBA)
             (mvm-emit-u16 buf port)
             ;; mov al, val
             (mvm-emit-byte buf #xB0)
             (mvm-emit-byte buf val)
             ;; out dx, al
             (mvm-emit-byte buf #xEE)))
    (let ((base +i386-com1-base+))
      ;; Disable interrupts
      (serial-out (+ base 1) #x00)
      ;; Enable DLAB (set baud rate)
      (serial-out (+ base 3) #x80)
      ;; Set divisor = 1 (115200 baud), low byte
      (serial-out (+ base 0) #x01)
      ;; Divisor high byte
      (serial-out (+ base 1) #x00)
      ;; 8 bits, no parity, 1 stop bit (8N1), clear DLAB
      (serial-out (+ base 3) #x03)
      ;; Enable FIFO, 14-byte threshold
      ;; 0xC1 = enable FIFO (bit0), 14-byte trigger (bits 6-7), no clear
      (serial-out (+ base 2) #xC1))))

;;; ============================================================
;;; i386 32-bit Entry Point
;;; ============================================================

(defun emit-i386-entry (buf)
  "Emit 32-bit protected mode entry point for i386.
   Multiboot spec guarantees: EAX = 0x2BADB002, EBX = multiboot info ptr,
   CS/DS/ES/FS/GS/SS = valid flat 32-bit segments, A20 enabled, paging off.

   We rely on bootloader segments (no GDT reload needed for initial boot).

   Register usage in the i386 translator (from translate-i386.lisp):
     V0=ESI, V1=EDI, V4=EBX (callee-saved GPRs)
     VR=EAX (return value)
     VA,VL,VN = stack slots at [EBP-16], [EBP-20], [EBP-24]
     Scratch: ECX, EDX"
  ;; --- CLI (redundant but safe) ---
  (mvm-emit-byte buf #xFA)                         ; cli

  ;; --- Save multiboot info pointer (EBX) to scratch location ---
  ;; mov [0x500], ebx
  (mvm-emit-byte buf #x89)                         ; mov [disp32], ebx
  (mvm-emit-byte buf #x1D)                         ; ModR/M: EBX -> [disp32]
  (mvm-emit-u32 buf #x500)

  ;; --- Set up stack ---
  ;; mov esp, +i386-stack-top+
  (mvm-emit-byte buf #xBC)                         ; mov esp, imm32
  (mvm-emit-u32 buf +i386-stack-top+)

  ;; --- Set up frame pointer ---
  ;; push ebp; mov ebp, esp
  (mvm-emit-byte buf #x55)                         ; push ebp
  (mvm-emit-byte buf #x89)                         ; mov ebp, esp
  (mvm-emit-byte buf #xE5)
  ;; sub esp, 80 (reserve frame: 76 bytes rounded to 80 for alignment)
  (mvm-emit-byte buf #x83)                         ; sub esp, imm8
  (mvm-emit-byte buf #xEC)
  (mvm-emit-byte buf 80)

  ;; --- Initialize alloc/limit/NIL at absolute addresses ---
  ;; mov [0x600], +i386-cons-base+  (VA = alloc pointer)
  (mvm-emit-byte buf #xC7)                         ; mov [disp32], imm32
  (mvm-emit-byte buf #x05)                         ; ModR/M: mod=00 r/m=5 (disp32)
  (mvm-emit-u32 buf #x600)
  (mvm-emit-u32 buf +i386-cons-base+)
  ;; mov [0x604], +i386-general-base+  (VL = alloc limit)
  (mvm-emit-byte buf #xC7)
  (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf #x604)
  (mvm-emit-u32 buf +i386-general-base+)
  ;; mov [0x608], 0x00  (VN = NIL = 0, matching AArch64/RISC-V convention)
  ;; NIL=0 ensures fixnum 0 is falsy, required by (when (logand ...)) idioms
  (mvm-emit-byte buf #xC7)
  (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf #x608)
  (mvm-emit-u32 buf #x00)

  ;; --- Initialize serial port ---
  (i386-init-serial buf)

  ;; --- Set up timer interrupt for HLT-based io-delay ---
  (emit-i386-interrupt-setup buf)

  ;; --- Fall through to compiled native code ---
  ;; The image builder appends translated Lisp code immediately after.
  )

(defun i386-out (buf port val)
  "Emit: mov al, val; out port, al (short form for port <= 255)"
  (mvm-emit-byte buf #xB0) (mvm-emit-byte buf val)
  (mvm-emit-byte buf #xE6) (mvm-emit-byte buf port))

(defun emit-i386-interrupt-setup (buf)
  "Set up PIC remap, PIT timer (~100Hz), and minimal 32-bit IDT for HLT-based io-delay."
  ;; === Remap PIC ===
  (i386-out buf #x20 #x11) (i386-out buf #xA0 #x11)
  (i386-out buf #x21 #x20) (i386-out buf #xA1 #x28)
  (i386-out buf #x21 #x04) (i386-out buf #xA1 #x02)
  (i386-out buf #x21 #x01) (i386-out buf #xA1 #x01)
  ;; Mask: unmask IRQ0+IRQ2(cascade) on master, unmask IRQ1(NE2000=IRQ9) on slave
  (i386-out buf #x21 #xFA) (i386-out buf #xA1 #xFD)

  ;; === Program PIT channel 0 ~100Hz ===
  (i386-out buf #x43 #x34)
  (i386-out buf #x40 #x9C)  ; 11932 & 0xFF
  (i386-out buf #x40 #x2E)  ; 11932 >> 8

  ;; === Build minimal 32-bit IDT at 0x90000 ===
  ;; 32-bit IDT entry (8 bytes): [offset_lo:16][selector:16][zero:8][type_attr:8][offset_hi:16]
  ;; ISR at 0x90400
  (let* ((idt-base #x90000)
         (isr-addr #x90400)
         (entry-offset (* #x20 8))   ; entry 0x20 at byte offset 256
         (entry-addr (+ idt-base entry-offset))
         (offset-lo (logand isr-addr #xFFFF))
         (offset-hi (logand (ash isr-addr -16) #xFFFF))
         (selector #x08)             ; code segment (from multiboot GDT)
         (type-attr #x8E))           ; P=1, DPL=0, 32-bit interrupt gate

    ;; Zero IDT area (48 entries × 8 bytes = 384 bytes)
    ;; mov edi, idt-base; xor eax,eax; mov ecx, 96; rep stosd
    (mvm-emit-byte buf #xBF) (mvm-emit-u32 buf idt-base)
    (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)
    (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf 96)
    (mvm-emit-byte buf #xF3) (mvm-emit-byte buf #xAB)

    ;; Write IDT entry 0x20 (PIT timer IRQ 0)
    ;; mov dword [entry_addr], (selector << 16) | offset_lo
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
    (mvm-emit-u32 buf entry-addr)
    (mvm-emit-u32 buf (logior offset-lo (ash selector 16)))
    ;; mov dword [entry_addr+4], (offset_hi << 16) | type_attr << 8
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
    (mvm-emit-u32 buf (+ entry-addr 4))
    (mvm-emit-u32 buf (logior (ash offset-hi 16) (ash type-attr 8)))

    ;; Write IDT entry 0x29 (NE2000 IRQ 9, slave IRQ 1)
    ;; ISR at 0x90410 (slave+master EOI)
    (let* ((ne2k-isr #x90410)
           (ne2k-entry-addr (+ idt-base (* #x29 8)))
           (ne2k-off-lo (logand ne2k-isr #xFFFF))
           (ne2k-off-hi (logand (ash ne2k-isr -16) #xFFFF)))
      (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
      (mvm-emit-u32 buf ne2k-entry-addr)
      (mvm-emit-u32 buf (logior ne2k-off-lo (ash selector 16)))
      (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
      (mvm-emit-u32 buf (+ ne2k-entry-addr 4))
      (mvm-emit-u32 buf (logior (ash ne2k-off-hi 16) (ash type-attr 8))))

    ;; === Write ISR at 0x90400 ===
    ;; push eax; mov al, 0x20; out 0x20, al; pop eax; iret
    ;; 50 B0 20 E6 20 58 CF = 7 bytes
    (let ((a isr-addr))
      ;; mov byte [a+0], 0x50 (push eax)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf a) (mvm-emit-byte buf #x50)
      ;; mov byte [a+1], 0xB0 (mov al, imm8)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ a 1)) (mvm-emit-byte buf #xB0)
      ;; mov byte [a+2], 0x20 (imm8)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ a 2)) (mvm-emit-byte buf #x20)
      ;; mov byte [a+3], 0xE6 (out imm8, al)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ a 3)) (mvm-emit-byte buf #xE6)
      ;; mov byte [a+4], 0x20 (port 0x20)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ a 4)) (mvm-emit-byte buf #x20)
      ;; mov byte [a+5], 0x58 (pop eax)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ a 5)) (mvm-emit-byte buf #x58)
      ;; mov byte [a+6], 0xCF (iret)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ a 6)) (mvm-emit-byte buf #xCF))

    ;; === Write NE2000 ISR at 0x90410 (9 bytes) ===
    ;; Slave IRQ: EOI to slave PIC (0xA0) then master PIC (0x20)
    ;; push eax; mov al, 0x20; out 0xA0, al; out 0x20, al; pop eax; iret
    ;; 50 B0 20 E6 A0 E6 20 58 CF = 9 bytes
    (let ((b #x90410))
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf b) (mvm-emit-byte buf #x50)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 1)) (mvm-emit-byte buf #xB0)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 2)) (mvm-emit-byte buf #x20)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 3)) (mvm-emit-byte buf #xE6)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 4)) (mvm-emit-byte buf #xA0)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 5)) (mvm-emit-byte buf #xE6)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 6)) (mvm-emit-byte buf #x20)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 7)) (mvm-emit-byte buf #x58)
      (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x05) (mvm-emit-u32 buf (+ b 8)) (mvm-emit-byte buf #xCF))

    ;; === Load IDTR ===
    ;; Build IDTR on stack: [limit:16 | base:32]
    ;; sub esp, 8
    (mvm-emit-byte buf #x83) (mvm-emit-byte buf #xEC) (mvm-emit-byte buf #x08)
    ;; mov word [esp], 383 (48*8-1)
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x24)
    (mvm-emit-u16 buf (1- (* 48 8)))
    ;; mov dword [esp+2], idt-base
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x44)
    (mvm-emit-byte buf #x24) (mvm-emit-byte buf #x02)
    (mvm-emit-u32 buf idt-base)
    ;; lidt [esp]
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x01)
    (mvm-emit-byte buf #x1C) (mvm-emit-byte buf #x24)
    ;; add esp, 8
    (mvm-emit-byte buf #x83) (mvm-emit-byte buf #xC4) (mvm-emit-byte buf #x08)))

;;; ============================================================
;;; i386 Per-CPU Layout
;;; ============================================================

(defun i386-percpu-layout ()
  "Per-CPU structure for i386. 32 bytes per CPU.
   Accessed via a fixed base address + cpu_id * stride.
   All fields are 4 bytes (32-bit pointers/values)."
  '((:self-ptr       0   4)   ; Pointer to this per-CPU struct
    (:reduction       4   4)   ; Reduction counter (tagged fixnum)
    (:cpu-id          8   4)   ; CPU number (tagged fixnum)
    (:current-actor  12   4)   ; Current actor pointer
    (:obj-alloc      16   4)   ; Per-actor object alloc pointer
    (:obj-limit      20   4)   ; Per-actor object alloc limit
    (:idle-stack     24   4)   ; Idle stack top for this CPU
    (:reserved       28   4))) ; Padding to 32-byte stride

;;; ============================================================
;;; i386 Boot Integration
;;; ============================================================

(defun i386-boot-descriptor ()
  "Return the i386 boot descriptor for image building.
   i386 is simpler than x86-64: no long mode, no page tables needed
   for the initial identity-mapped 32-bit flat model."
  (list :arch :i386
        :multiboot-header-fn #'emit-i386-multiboot-header
        :entry-fn #'emit-i386-entry
        :serial-init-fn #'i386-init-serial
        :percpu-layout-fn #'i386-percpu-layout
        :load-addr +i386-kernel-load-addr+
        :stack-top +i386-stack-top+
        :cons-base +i386-cons-base+
        :general-base +i386-general-base+
        :percpu-base +i386-percpu-base+
        :percpu-stride +i386-percpu-stride+))
