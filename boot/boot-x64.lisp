;;;; boot-x64.lisp - x86-64 Boot Sequence for Modus
;;;;
;;;; Refactored from the original monolithic boot code.
;;;; This file contains the x86-64-specific boot sequence that runs
;;;; before the platform-independent kernel-main.
;;;;
;;;; Boot protocol for x86-64 (BIOS/Multiboot):
;;;;   1. Multiboot header at 0x100000
;;;;   2. 32-bit protected mode entry (from GRUB/bootloader)
;;;;   3. Set up page tables (identity map + higher half)
;;;;   4. Enable long mode (64-bit)
;;;;   5. Jump to 64-bit entry point
;;;;   6. Set up GDT, IDT, TSS
;;;;   7. Initialize serial console
;;;;   8. Set up GC metadata, NFN table, symbol table
;;;;   9. Initialize allocation registers (R12=alloc, R14=limit, R15=NIL)
;;;;  10. Call kernel-main

(in-package :modus.mvm)

;;; ============================================================
;;; x86-64 Boot Constants
;;; ============================================================

(defconstant +x64-kernel-load-addr+ #x100000)     ; 1MB - multiboot load address
;; Page tables and the IDT page MUST live OUTSIDE the kernel image range,
;; otherwise paging setup / IDT writes overlay native code or constant
;; pool bytes that the linker happened to place there.  Symptom: a call
;; to such a clobbered function lands on PDPT/IDT byte garbage (e.g.
;; `push rax` byte 0x50 at 0x501002, RSP=0 the moment the fault frame
;; couldn't be pushed, triple-fault).
;;
;; Conventional memory between 0x1000 and 0x80000 (just above the legacy
;; IVT/BDA, below the EBDA) is free RAM on every BIOS/UEFI x86-64 host
;; and is well BELOW the kernel load address — the image can grow to
;; fill all 4GB of identity-mapped RAM without ever colliding.  We
;; bundle: PML4 / PDPT / PD0..3 / IDT / handlers in 0x10000..0x1A000.
(defconstant +x64-page-tables-addr+ #x10000)      ; 64KB - PML4 + PDPT + 4xPD (24KB)
(defconstant +x64-idt-addr+         #x18000)      ; 96KB - IDT + #PF/#GP/PIT ISRs
(defconstant +x64-stack-top+        #x800000)     ; 8MB - initial stack top (must stay above page tables, below image growth)
(defconstant +x64-kernel64-addr+    #x100100)     ; 64-bit entry point

;; Memory regions
(defconstant +x64-wired-base+    #x02000000)      ; 32MB - wired memory
(defconstant +x64-pinned-base+   #x03000000)      ; 48MB - pinned memory
(defconstant +x64-fn-base+       #x03800000)      ; 56MB - function space
(defconstant +x64-cons-base+     #x04000000)      ; 64MB - cons space
(defconstant +x64-general-base+  #x05000000)      ; 80MB - general heap

;; Mostly-Copying GC (MCGC) metadata for bare-metal x64.  The Cheney
;; semispaces live in [0x10001000, 0x1DFFF000); the MCGC metadata region
;; is placed ABOVE the heap at 0x1E000000.  Through stages 1-2 the old
;; collector still runs and these config words are unused/write-only;
;; they keep the bare-metal image building and ready for stage 3.
;; Config-word BSS slots match boot-linux-x64 (+mcgc-cfg-*+ there).
(defconstant +x64-mcgc-data-base+  #x10001000)    ; first allocatable data byte
(defconstant +x64-mcgc-data-end+   #x1DFFF000)    ; one past data region
(defconstant +x64-mcgc-meta-base+  #x1E000000)    ; metadata region base
(defconstant +x64-mcgc-page-size+  #x1000)
(defconstant +x64-mcgc-page-count+
  (truncate (- +x64-mcgc-data-end+ +x64-mcgc-data-base+) +x64-mcgc-page-size+))
(defconstant +x64-mcgc-descriptor-base+ +x64-mcgc-meta-base+)
(defconstant +x64-mcgc-bitmap-base+
  (logand (+ +x64-mcgc-descriptor-base+ +x64-mcgc-page-count+ 63) (lognot 63)))
(defconstant +x64-mcgc-freelist-base+
  (logand (+ +x64-mcgc-bitmap-base+
             (truncate (- +x64-mcgc-data-end+ +x64-mcgc-data-base+) (* 16 8)) 63)
          (lognot 63)))

;; Per-CPU structures
(defconstant +x64-percpu-base+   #x360000)        ; Per-CPU data area
(defconstant +x64-percpu-stride+ #x40)            ; 64 bytes per CPU

;; AP trampoline
(defconstant +x64-ap-trampoline+ #x8000)          ; Real-mode AP startup code

;;; ============================================================
;;; x86-64 Boot Code Generation
;;; ============================================================

(defun emit-x64-multiboot-header (buf)
  "Emit Multiboot1 header with aout-kludge for QEMU -kernel loading.
   32 bytes: magic(4) flags(4) checksum(4) header_addr(4) load_addr(4)
   load_end_addr(4) bss_end_addr(4) entry_addr(4).
   Entry point (boot32 code) starts immediately after at load_addr + 32."
  (let* ((magic #x1BADB002)
         (flags (logior #x00000003   ; page-align + memory info
                        #x00010000)) ; aout-kludge (address fields)
         (load-addr +x64-kernel-load-addr+)
         (entry-addr (+ load-addr 32)))  ; boot32 starts right after header
    (mvm-emit-u32 buf magic)
    (mvm-emit-u32 buf flags)
    ;; Checksum: -(magic + flags) mod 2^32
    ;; Emit as bytes to avoid i386 30-bit fixnum overflow.
    ;; For magic=#x1BADB002, flags=#x10003: checksum = #xE4514FFB
    (let ((neg-sum (- (+ magic flags))))
      (mvm-emit-byte buf (logand neg-sum 255))
      (mvm-emit-byte buf (logand (ash neg-sum -8) 255))
      (mvm-emit-byte buf (logand (ash neg-sum -16) 255))
      (mvm-emit-byte buf (logand (ash neg-sum -24) 255)))
    ;; Address fields
    (mvm-emit-u32 buf load-addr)     ; header_addr
    (mvm-emit-u32 buf load-addr)     ; load_addr
    (mvm-emit-u32 buf 0)            ; load_end_addr (0 = whole file)
    (mvm-emit-u32 buf 0)            ; bss_end_addr
    (mvm-emit-u32 buf entry-addr))) ; entry_addr → boot32 code

(defun emit-x64-boot32 (buf)
  "Emit 32-bit protected mode boot stub.
   Multiboot enters here in 32-bit protected mode with flat segments.
   Sets up page tables at 0x110000, enables PAE + long mode + paging,
   then far-jumps to 64-bit code segment.
   The 64-bit entry follows immediately after this code."
  (let ((base-addr +x64-kernel-load-addr+)
        (pml4-addr +x64-page-tables-addr+)
        (boot32-start (mvm-buffer-position buf)))
    ;; cli
    (mvm-emit-byte buf #xFA)

    ;; Save multiboot info: mov [0x500], ebx
    (mvm-emit-byte buf #x89)
    (mvm-emit-byte buf #x1D)
    (mvm-emit-u32 buf #x500)

    ;; Set up stack: mov esp, stack-top (above kernel image, below page tables)
    (mvm-emit-byte buf #xBC)
    (mvm-emit-u32 buf +x64-stack-top+)

    ;; Clear page table area (28KB at pml4-addr: PML4+PDPT+4xPD)
    ;; mov edi, pml4-addr
    (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf pml4-addr)
    ;; mov ecx, 7168  (28KB / 4 bytes)
    (mvm-emit-byte buf #xB9)
    (mvm-emit-u32 buf 7168)
    ;; xor eax, eax
    (mvm-emit-byte buf #x31)
    (mvm-emit-byte buf #xC0)
    ;; rep stosd
    (mvm-emit-byte buf #xF3)
    (mvm-emit-byte buf #xAB)

    ;; PML4[0] -> PDPT  (pml4_addr+0x1000 | 3)
    (mvm-emit-byte buf #xC7)  ; mov [addr], imm32
    (mvm-emit-byte buf #x05)
    (mvm-emit-u32 buf pml4-addr)
    (mvm-emit-u32 buf (logior (+ pml4-addr #x1000) 3))

    ;; PDPT[0] -> PD0  (pml4_addr+0x2000 | 3)
    (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #x05)
    (mvm-emit-u32 buf (+ pml4-addr #x1000))
    (mvm-emit-u32 buf (logior (+ pml4-addr #x2000) 3))

    ;; PDPT[1] -> PD1  (pml4_addr+0x3000 | 3)
    (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #x05)
    (mvm-emit-u32 buf (+ pml4-addr #x1008))
    (mvm-emit-u32 buf (logior (+ pml4-addr #x3000) 3))

    ;; PDPT[2] -> PD2  (pml4_addr+0x4000 | 3)
    (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #x05)
    (mvm-emit-u32 buf (+ pml4-addr #x1010))
    (mvm-emit-u32 buf (logior (+ pml4-addr #x4000) 3))

    ;; PDPT[3] -> PD3  (pml4_addr+0x5000 | 3)
    (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #x05)
    (mvm-emit-u32 buf (+ pml4-addr #x1018))
    (mvm-emit-u32 buf (logior (+ pml4-addr #x5000) 3))

    ;; Fill 4 PDs with 2048 x 2MB pages (identity map first 4GB)
    ;; mov edi, pd0_addr
    (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf (+ pml4-addr #x2000))
    ;; mov eax, 0x83  (present + writable + 2MB page)
    (mvm-emit-byte buf #xB8)
    (mvm-emit-u32 buf #x83)
    ;; mov ecx, 2048
    (mvm-emit-byte buf #xB9)
    (mvm-emit-u32 buf 2048)
    ;; loop: stosd; add eax,0x200000; mov [edi],0; add edi,4; loop
    (let ((loop-start (mvm-buffer-position buf)))
      (mvm-emit-byte buf #xAB)        ; stosd (store eax to [edi], edi+=4)
      (mvm-emit-byte buf #x05)        ; add eax, 0x200000
      (mvm-emit-u32 buf #x200000)
      (mvm-emit-byte buf #xC7)        ; mov dword [edi], 0  (high 32 bits)
      (mvm-emit-byte buf #x07)
      (mvm-emit-u32 buf 0)
      (mvm-emit-byte buf #x83)        ; add edi, 4
      (mvm-emit-byte buf #xC7)
      (mvm-emit-byte buf #x04)
      (mvm-emit-byte buf #xE2)        ; loop
      (mvm-emit-byte buf (logand #xFF (- loop-start (mvm-buffer-position buf) 1))))

    ;; Load CR3 with PML4
    (mvm-emit-byte buf #xB8)          ; mov eax, pml4
    (mvm-emit-u32 buf pml4-addr)
    (mvm-emit-byte buf #x0F)          ; mov cr3, eax
    (mvm-emit-byte buf #x22)
    (mvm-emit-byte buf #xD8)

    ;; Enable PAE (CR4.PAE = bit 5)
    (mvm-emit-byte buf #x0F)          ; mov eax, cr4
    (mvm-emit-byte buf #x20)
    (mvm-emit-byte buf #xE0)
    (mvm-emit-byte buf #x83)          ; or eax, 0x20
    (mvm-emit-byte buf #xC8)
    (mvm-emit-byte buf #x20)
    (mvm-emit-byte buf #x0F)          ; mov cr4, eax
    (mvm-emit-byte buf #x22)
    (mvm-emit-byte buf #xE0)

    ;; Enable long mode (IA32_EFER.LME = bit 8)
    (mvm-emit-byte buf #xB9)          ; mov ecx, 0xC0000080 (IA32_EFER)
    ;; Emit as bytes (0xC0000080 overflows i386 30-bit fixnum)
    (mvm-emit-byte buf #x80) (mvm-emit-byte buf #x00)
    (mvm-emit-byte buf #x00) (mvm-emit-byte buf #xC0)
    (mvm-emit-byte buf #x0F)          ; rdmsr
    (mvm-emit-byte buf #x32)
    (mvm-emit-byte buf #x0D)          ; or eax, 0x100
    (mvm-emit-u32 buf #x100)
    (mvm-emit-byte buf #x0F)          ; wrmsr
    (mvm-emit-byte buf #x30)

    ;; Enable paging (CR0.PG = bit 31)
    (mvm-emit-byte buf #x0F)          ; mov eax, cr0
    (mvm-emit-byte buf #x20)
    (mvm-emit-byte buf #xC0)
    (mvm-emit-byte buf #x0D)          ; or eax, 0x80000000
    ;; Emit as bytes (0x80000000 overflows i386 30-bit fixnum)
    (mvm-emit-byte buf #x00) (mvm-emit-byte buf #x00)
    (mvm-emit-byte buf #x00) (mvm-emit-byte buf #x80)
    (mvm-emit-byte buf #x0F)          ; mov cr0, eax
    (mvm-emit-byte buf #x22)
    (mvm-emit-byte buf #xC0)

    ;; We need a GDT with a 64-bit code segment for the far jump.
    ;; Emit GDT inline, then lgdt, then far jump.
    ;; GDT is right here in the code stream — we jump over it.
    ;; jmp short past_gdt (2-byte instruction, patched below)
    (let ((jmp-pos (mvm-buffer-position buf)))
      (mvm-emit-byte buf #xEB)        ; jmp rel8
      (mvm-emit-byte buf 0)           ; placeholder

      ;; GDT data (3 descriptors = 24 bytes)
      (let ((gdt-pos (mvm-buffer-position buf)))
        ;; Null descriptor
        (mvm-emit-u32 buf 0) (mvm-emit-u32 buf 0)
        ;; 32-bit code (selector 0x08) — not needed after jump but harmless
        (mvm-emit-u32 buf #x0000FFFF) (mvm-emit-u32 buf #x00CF9A00)
        ;; 64-bit code (selector 0x10)
        (mvm-emit-u32 buf #x0000FFFF) (mvm-emit-u32 buf #x00AF9A00)

        ;; GDTR (6 bytes: limit u16 + base u32)
        (let ((gdtr-pos (mvm-buffer-position buf))
              (gdt-addr (+ base-addr gdt-pos)))
          (mvm-emit-u16 buf 23)       ; limit = 3*8 - 1
          (mvm-emit-u32 buf gdt-addr)

          ;; Patch jmp rel8 to skip past GDT+GDTR (land here)
          (let ((after-gdt (mvm-buffer-position buf))
                (bytes (mvm-buffer-bytes buf)))
            (setf (aref bytes (1+ jmp-pos))
                  (logand #xFF (- after-gdt jmp-pos 2)))

            ;; lgdt [gdtr_addr]
            (mvm-emit-byte buf #x0F)
            (mvm-emit-byte buf #x01)
            (mvm-emit-byte buf #x15)  ; lgdt [disp32]
            (mvm-emit-u32 buf (+ base-addr gdtr-pos))

            ;; Far jump to 64-bit code: jmp far 0x10:entry64_addr
            ;; entry64 is the next thing emitted (by emit-x64-kernel64-entry)
            ;; We don't know the exact offset yet, so emit placeholder and patch
            (let ((jmp64-pos (mvm-buffer-position buf)))
              (mvm-emit-byte buf #xEA)  ; jmp far ptr16:32
              (mvm-emit-u32 buf 0)      ; placeholder entry64 addr
              (mvm-emit-u16 buf #x0010) ; 64-bit code selector

              ;; hlt (shouldn't reach)
              (mvm-emit-byte buf #xF4)

              ;; Record the patch location for emit-x64-kernel64-entry to fill
              ;; Store it as a property on the buffer's labels hash
              (setf (gethash :jmp64-patch (mvm-buffer-labels buf))
                    (1+ jmp64-pos)))))))))

(defun emit-x64-kernel64-entry (buf)
  "Emit 64-bit kernel entry point.
   Called after long mode is established by boot32."
  (let ((base-addr +x64-kernel-load-addr+)
        (entry64-pos (mvm-buffer-position buf)))
    ;; Patch boot32's far-jump target to point here
    (let ((patch-offset (gethash :jmp64-patch (mvm-buffer-labels buf))))
      (when patch-offset
        (let ((entry64-addr (+ base-addr entry64-pos))
              (bytes (mvm-buffer-bytes buf)))
          (setf (aref bytes (+ patch-offset 0)) (ldb (byte 8  0) entry64-addr))
          (setf (aref bytes (+ patch-offset 1)) (ldb (byte 8  8) entry64-addr))
          (setf (aref bytes (+ patch-offset 2)) (ldb (byte 8 16) entry64-addr))
          (setf (aref bytes (+ patch-offset 3)) (ldb (byte 8 24) entry64-addr)))))

    ;; Reload data segments with 64-bit selector (GDT entry 2 = 0x10 is code;
    ;; we don't have a separate 64-bit data selector, use null/0 which is fine
    ;; in long mode where segment bases are ignored except FS/GS)
    ;; mov ax, 0
    (mvm-emit-byte buf #x66)
    (mvm-emit-byte buf #xB8)
    (mvm-emit-u16 buf #x0000)
    ;; mov ds, ax
    (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xD8)
    ;; mov es, ax
    (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xC0)
    ;; mov ss, ax
    (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xD0)

    ;; Set up 64-bit stack: mov rsp, stack-top (below page tables)
    (mvm-emit-byte buf #x48)          ; REX.W
    (mvm-emit-byte buf #xBC)          ; mov rsp, imm64
    (mvm-emit-u32 buf +x64-stack-top+)
    (mvm-emit-u32 buf 0)              ; high 32 bits

    ;; Initialize serial console (COM1 = 0x3F8)
    ;; Disable interrupts
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x03F9)
    (mvm-emit-byte buf #xB0) (mvm-emit-byte buf #x00)
    (mvm-emit-byte buf #xEE)
    ;; DLAB
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x03FB)
    (mvm-emit-byte buf #xB0) (mvm-emit-byte buf #x80)
    (mvm-emit-byte buf #xEE)
    ;; Baud 115200 (divisor=1)
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x03F8)
    (mvm-emit-byte buf #xB0) (mvm-emit-byte buf #x01)
    (mvm-emit-byte buf #xEE)
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x03F9)
    (mvm-emit-byte buf #xB0) (mvm-emit-byte buf #x00)
    (mvm-emit-byte buf #xEE)
    ;; 8N1
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x03FB)
    (mvm-emit-byte buf #xB0) (mvm-emit-byte buf #x03)
    (mvm-emit-byte buf #xEE)
    ;; FIFO
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x03FA)
    (mvm-emit-byte buf #xB0) (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #xEE)

    ;; Initialize runtime registers for MVM-compiled code
    ;; R15 = NIL (must match +nil-value+ = #xDEAD0001 used by MVM compiler)
    ;; mov r15, #xDEAD0001
    (mvm-emit-byte buf #x49)          ; REX.WB (W=64-bit, B=R15 extended)
    (mvm-emit-byte buf #xBF)          ; mov r15, imm64
    ;; Emit as bytes (0xDEAD0001 overflows i386 30-bit fixnum)
    (mvm-emit-byte buf #x01) (mvm-emit-byte buf #x00)
    (mvm-emit-byte buf #xAD) (mvm-emit-byte buf #xDE)
    (mvm-emit-u32 buf 0)              ; high 32 bits

    ;; R12 = allocation pointer.  Heap region is 0x10000000+ but the
    ;; first 4KB (0x10000000-0x10000FFF) is reserved for runtime metadata
    ;; (GC slots, global-alist head, symbol intern table, keyword intern
    ;; table, handler-case state, SIGSEGV diag slots, etc. — see CLAUDE.md
    ;; "Linux x64 Memory Layout").  On Linux those are part of the ELF
    ;; BSS segment and the mmap'd heap lives elsewhere; on bare-metal the
    ;; same address space is shared, so we start the alloc pointer past
    ;; the metadata reservation.  Anything below 0x10001000 is metadata.
    ;;
    ;; Heap split into two Cheney semispaces of ~112MB each:
    ;;   from-space: [0x10001000, 0x17000000)
    ;;   to-space:   [0x17000000, 0x1DFFF000)
    ;;   space_size: 0x06FFF000
    ;; R14 = from_start + space_size = 0x16FFF000, so the FIRST overflow
    ;; trips the GC trampoline at the correct from-space boundary.
    ;; Without this, the trampoline reads zero metadata and ALL HELL
    ;; BREAKS LOOSE (see comment block below for GC metadata init).
    ;; mov r12, 0x10001000
    (mvm-emit-byte buf #x49)          ; REX.WB
    (mvm-emit-byte buf #xBC)          ; mov r12, imm64
    (mvm-emit-u32 buf #x10001000)
    (mvm-emit-u32 buf 0)

    ;; R14 = from-space end (alloc limit).
    ;; mov r14, 0x17000000
    (mvm-emit-byte buf #x49)          ; REX.WB
    (mvm-emit-byte buf #xBE)          ; mov r14, imm64
    (mvm-emit-u32 buf #x17000000)
    (mvm-emit-u32 buf 0)

    ;; Initialize GC metadata at 0x10000040..0x10000058.  The x64 GC
    ;; trampoline (translate-x64.lisp emit-gc-trampoline) reads these
    ;; slots as RAW byte addresses — NOT as tagged Lisp values.  So
    ;; we must store RAW addresses via asm, NOT via Lisp's
    ;; (setf (mem-ref ADDR :u64) val) which auto-stores the
    ;; fixnum-tagged form (2*val).  Without this initialization, any
    ;; GC trigger reads zero metadata, swaps both semispaces to
    ;; address 0, and sets R14 := 0 — after which every cons triggers
    ;; another zero-metadata GC, allocating cons cells at address 0
    ;; (overwriting BIOS/IDT/page tables/etc.).  Worth ~1000 extra
    ;; ANSI passes on bare-metal x64 by neutralising the
    ;; (make-array '(N)) compile-time bug's runaway allocations.
    ;;
    ;; Slot 0x10000040 = from_start = 0x10001000
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf #x10000040) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB8)
    (mvm-emit-u32 buf #x10001000) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)
    ;; Slot 0x10000048 = to_start = 0x17000000
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf #x10000048) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB8)
    (mvm-emit-u32 buf #x17000000) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)
    ;; Slot 0x10000050 = space_size = 0x06FFF000
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf #x10000050) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB8)
    (mvm-emit-u32 buf #x06FFF000) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)
    ;; Slot 0x10000058 = stack_base = 0x00800000 (top of stack)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf #x10000058) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB8)
    (mvm-emit-u32 buf #x00800000) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)

    ;; ---- MCGC config words (bare-metal: physical addresses) ----
    ;; Mirror boot-linux-x64's config block so the bare-metal image carries
    ;; the same metadata layout.  Stages 1-2: written, unused by old GC.
    (flet ((mcgc-store (slot val)
             ;; mov rdi, slot ; mov rax, val ; mov [rdi], rax
             (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
             (mvm-emit-u32 buf slot) (mvm-emit-u32 buf 0)
             (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB8)
             (mvm-emit-u32 buf (logand val #xFFFFFFFF))
             (mvm-emit-u32 buf (logand (ash val -32) #xFFFFFFFF))
             (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)))
      (mcgc-store #x10000E00 +x64-mcgc-data-base+)        ; page_base
      (mcgc-store #x10000E08 +x64-mcgc-page-count+)       ; page_count
      (mcgc-store #x10000E10 +x64-mcgc-descriptor-base+)  ; descriptor
      (mcgc-store #x10000E18 +x64-mcgc-bitmap-base+)      ; bitmap
      (mcgc-store #x10000E20 +x64-mcgc-freelist-base+)    ; freelist
      (mcgc-store #x10000E28 0)                           ; freelist_count
      (mcgc-store #x10000E30 0)                           ; alloc_page
      (mcgc-store #x10000E38 +x64-mcgc-data-end+))        ; data_end

    ;; RBP = frame pointer (same as RSP initially)
    ;; mov rbp, rsp
    (mvm-emit-byte buf #x48)          ; REX.W
    (mvm-emit-byte buf #x89)          ; mov r/m64, r64
    (mvm-emit-byte buf #xE5)          ; ModRM: reg=RSP(4), rm=RBP(5)

    ;; Set up timer interrupt for HLT-based io-delay
    (emit-x64-interrupt-setup buf)

    ;; Fall through to native code
    ))

(defun emit-x64-out (buf port val)
  "Emit: mov al, val; mov dx, port; out dx, al"
  (mvm-emit-byte buf #xB0) (mvm-emit-byte buf val)         ; mov al, imm8
  (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA)        ; mov dx, imm16
  (mvm-emit-u16 buf port)
  (mvm-emit-byte buf #xEE))                                 ; out dx, al

(defun emit-x64-interrupt-setup (buf)
  "Set up PIC remap, PIT timer (~100Hz), and minimal IDT for HLT-based io-delay.
   After this, STI + HLT will sleep until the next PIT timer tick (~10ms)."
  ;; === Remap PIC: master IRQ 0x20-0x27, slave IRQ 0x28-0x2F ===
  ;; ICW1: init + ICW4 needed
  (emit-x64-out buf #x20 #x11)   ; master ICW1
  (emit-x64-out buf #xA0 #x11)   ; slave ICW1
  ;; ICW2: vector offset
  (emit-x64-out buf #x21 #x20)   ; master: IRQ0 → INT 0x20
  (emit-x64-out buf #xA1 #x28)   ; slave: IRQ8 → INT 0x28
  ;; ICW3: master/slave wiring
  (emit-x64-out buf #x21 #x04)   ; master: slave on IRQ2
  (emit-x64-out buf #xA1 #x02)   ; slave: cascade identity
  ;; ICW4: 8086 mode
  (emit-x64-out buf #x21 #x01)
  (emit-x64-out buf #xA1 #x01)
  ;; Mask all IRQs except IRQ0 (timer)
  (emit-x64-out buf #x21 #xFE)   ; master: unmask IRQ0 only
  (emit-x64-out buf #xA1 #xFF)   ; slave: mask all

  ;; === Program PIT channel 0 for ~1000Hz (divisor = 1193 = 0x04A9) ===
  ;; Mode 2 (rate generator), binary, channel 0, lo/hi byte
  (emit-x64-out buf #x43 #x34)   ; command: channel 0, lobyte/hibyte, mode 2
  (emit-x64-out buf #x40 #xA9)   ; divisor low byte (1193 & 0xFF = 0xA9)
  (emit-x64-out buf #x40 #x04)   ; divisor high byte (1193 >> 8 = 0x04)

  ;; === Build minimal 64-bit IDT at +x64-idt-addr+ ===
  ;; We only need entry 0x20 (PIT timer IRQ). All others can be absent/zero.
  ;; IDT entry format (16 bytes):
  ;;   [offset_lo:16][selector:16][IST:3][zero:5][type:4][zero:1][DPL:2][P:1][offset_mid:16]
  ;;   [offset_hi:32][reserved:32]
  ;; ISR is placed 2KB into the IDT page (after the IDT entries themselves).
  (let* ((idt-base +x64-idt-addr+)
         (isr-addr (+ idt-base #x800))
         (entry-offset (* #x20 16))  ; entry 0x20 = byte offset 512
         (entry-addr (+ idt-base entry-offset))
         (offset-lo (logand isr-addr #xFFFF))
         (offset-mid (logand (ash isr-addr -16) #xFFFF))
         (offset-hi (logand (ash isr-addr -32) #xFFFFFFFF))
         (selector #x10)              ; 64-bit code segment
         (type-attr #x8E))            ; P=1, DPL=0, interrupt gate (0xE)

    ;; Zero IDT area (entries 0x00-0x2F = 48 entries × 16 bytes = 768 bytes)
    ;; Use REP STOSQ: rcx = count, rdi = addr, rax = value
    ;; mov rdi, idt-base
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf idt-base) (mvm-emit-u32 buf 0)
    ;; xor rax, rax
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)
    ;; mov rcx, 96 (768/8 qwords)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xB9)
    (mvm-emit-u32 buf 96) (mvm-emit-u32 buf 0)
    ;; rep stosq
    (mvm-emit-byte buf #xF3) (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)

    ;; Write IDT entry 0x20 (PIT timer) at idt-base + 0x200
    ;; mov rdi, entry_addr
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf entry-addr) (mvm-emit-u32 buf 0)
    ;; Word 0: [offset_lo:16 | selector:16] = (selector << 16) | offset_lo
    ;; mov eax, imm32
    (mvm-emit-byte buf #xB8)
    (mvm-emit-u32 buf (logior offset-lo (ash selector 16)))
    ;; mov [rdi], eax
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)
    ;; Word 1: [IST=0 | type_attr | offset_mid]
    ;; mov eax, imm32
    (mvm-emit-byte buf #xB8)
    (mvm-emit-u32 buf (logior (ash type-attr 8) (ash offset-mid 16)))
    ;; mov [rdi+4], eax
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x04)
    ;; Word 2: offset_hi (0 for addresses < 4GB)
    ;; mov dword [rdi+8], offset_hi
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x08)
    (mvm-emit-u32 buf offset-hi)
    ;; Word 3: reserved = 0
    ;; mov dword [rdi+12], 0
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x0C)
    (mvm-emit-u32 buf 0)

    ;; === Write ISR at 0x4F0800 ===
    ;; Minimal timer ISR: push rax, send EOI to PIC, pop rax, iretq
    ;; mov rdi, isr_addr
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf isr-addr) (mvm-emit-u32 buf 0)
    ;; ISR code (11 bytes):
    ;;   50           push rax
    ;;   B0 20        mov al, 0x20
    ;;   E6 20        out 0x20, al    (EOI to master PIC)
    ;;   58           pop rax
    ;;   48 CF        iretq
    ;; mov byte [rdi+0], 0x50 (push rax)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x07) (mvm-emit-byte buf #x50)
    ;; mov byte [rdi+1], 0xB0 (mov al, imm8)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x01)
    (mvm-emit-byte buf #xB0)
    ;; mov byte [rdi+2], 0x20 (imm8 = 0x20 = EOI)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x02)
    (mvm-emit-byte buf #x20)
    ;; mov byte [rdi+3], 0xE6 (out imm8, al)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x03)
    (mvm-emit-byte buf #xE6)
    ;; mov byte [rdi+4], 0x20 (port 0x20)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x04)
    (mvm-emit-byte buf #x20)
    ;; mov byte [rdi+5], 0x58 (pop rax)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x05)
    (mvm-emit-byte buf #x58)
    ;; mov byte [rdi+6], 0x48 (REX.W prefix for iretq)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x06)
    (mvm-emit-byte buf #x48)
    ;; mov byte [rdi+7], 0xCF (iretq)
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x07)
    (mvm-emit-byte buf #xCF)

    ;; ============================================================
    ;; Deadline-aware PIT ISR at +x64-idt-addr+ +0x900.  Mirrors AArch64 entry-5:
    ;; on every IRQ tick, decrement the counter at 0x10000C70.  When
    ;; the counter is 0, do nothing (no deadline armed).  When it
    ;; transitions 1→0, check slot 0x10000180 (handler-case armed by
    ;; setjmp) and longjmp to its saved RIP with RAX = T sentinel.
    ;;
    ;; Critical design choice: we use STI + JMP rather than IRETQ.
    ;; IRETQ in 64-bit mode pops 5 quadwords (RIP, CS, RFLAGS, RSP,
    ;; SS) unconditionally — even at CPL=0.  Constructing a faithful
    ;; 5-quad frame inside an asynchronous ISR is fiddly.  Instead:
    ;; mov rsp, saved_rsp; mov rbp, saved_rbp; mov eax, T_sentinel;
    ;; sti; jmp saved_rip.  This skips the segment-restore phase of
    ;; IRETQ but in 64-bit kernel mode CS/SS values are not used for
    ;; addressing anyway (only descriptor-cache flags matter).
    ;;
    ;; Deadline-aware PIT ISR at +x64-idt-addr+ +0x900.  Decrements counter at
    ;; 0x10000C70; on 1→0 transition, longjmp via slot 0x10000180.
    ;;
    ;; CRITICAL fix #1: pit_normal IRETQ path injects IF=1 into the
    ;; saved IRETQ-frame RFLAGS via `or [rsp+16], 0x200`.  CPU clears
    ;; IF on IRQ entry through an interrupt gate; without the OR, the
    ;; first tick re-restores IF=0 and the timer never fires again.
    ;;
    ;; CRITICAL fix #2: the deadline-hit path INLINES the handler-stack
    ;; pop (mirrors the 150-byte #PF handler at +x64-idt-addr+ +0x820): reads frame
    ;; [0x10000408 + (depth-1)*24] into slot 0x10000180/188/190, or
    ;; zeros slot 180 if depth==0.  Without this, depth grows unbounded
    ;; across timer-longjmps — each SETJMP pushes but only CLEAR-HANDLER
    ;; and explicit LONGJMP normally pop.  The IRQ-deadline longjmp
    ;; otherwise leaves a stale push that fakes nested-handler-case
    ;; state, so the next OUTER CLEAR-HANDLER restores a stale slot
    ;; 180 and the next FORMAT-spam test infinitely re-enters the same
    ;; failed handler.  AArch64 entry-5 calls BL pop_helper for the
    ;; same reason (boot-aarch64.lisp lines 375-381).
    (let ((deadline-isr-addr (+ idt-base #x900))
          (deadline-bytes
           #(;; entry + EOI
             #x50                                       ; 0: push rax
             #x51                                       ; 1: push rcx
             #x52                                       ; 2: push rdx
             #xB0 #x20                                  ; 3: mov al, 0x20
             #xE6 #x20                                  ; 5: out 0x20, al
             ;; load counter address
             #x48 #xB9 #x70 #x0C #x00 #x10 #x00 #x00 #x00 #x00  ; 7: mov rcx, 0x10000C70
             #x48 #x8B #x01                             ; 17: mov rax, [rcx]
             #x48 #x85 #xC0                             ; 20: test rax, rax
             ;; jz NEAR pit_normal (target byte 177; delta from PC=29 = 148 = 0x94)
             #x0F #x84 #x94 #x00 #x00 #x00              ; 23: jz pit_normal
             #x48 #xFF #xC8                             ; 29: dec rax
             #x48 #x89 #x01                             ; 32: mov [rcx], rax
             ;; jnz NEAR pit_normal (delta from PC=41 = 136 = 0x88)
             #x0F #x85 #x88 #x00 #x00 #x00              ; 35: jnz pit_normal
             ;; Deadline hit:
             #x48 #xB9 #x80 #x01 #x00 #x10 #x00 #x00 #x00 #x00  ; 41: mov rcx, 0x10000180
             #x48 #x8B #x01                             ; 51: mov rax, [rcx]
             #x48 #x85 #xC0                             ; 54: test rax, rax
             ;; jz SHORT pit_normal (delta from PC=59 to 177 = 118 = 0x76)
             #x74 #x76                                  ; 57: jz pit_normal
             #x48 #x8B #x69 #x08                        ; 59: mov rbp, [rcx+8]
             #x48 #x8B #x51 #x10                        ; 63: mov rdx, [rcx+16]
             ;; Inline handler-stack pop:
             #x4C #x8B #x14 #x25 #x00 #x04 #x00 #x10    ; 67: mov r10, [0x10000400]
             #x4D #x85 #xD2                             ; 75: test r10, r10
             ;; jz SHORT zero_fill (target byte 139; delta from PC=80 = 59 = 0x3B)
             #x74 #x3B                                  ; 78: jz zero_fill
             #x49 #xFF #xCA                             ; 80: dec r10
             #x4C #x89 #x14 #x25 #x00 #x04 #x00 #x10    ; 83: mov [0x10000400], r10
             #x4D #x6B #xDA #x18                        ; 91: imul r11, r10, 24
             #x49 #x81 #xC3 #x08 #x04 #x00 #x10         ; 95: add r11, 0x10000408
             #x4D #x8B #x13                             ; 102: mov r10, [r11]
             #x4C #x89 #x14 #x25 #x80 #x01 #x00 #x10    ; 105: mov [0x10000180], r10
             #x4D #x8B #x53 #x08                        ; 113: mov r10, [r11+8]
             #x4C #x89 #x14 #x25 #x88 #x01 #x00 #x10    ; 117: mov [0x10000188], r10
             #x4D #x8B #x53 #x10                        ; 125: mov r10, [r11+16]
             #x4C #x89 #x14 #x25 #x90 #x01 #x00 #x10    ; 129: mov [0x10000190], r10
             ;; jmp SHORT epilogue (target byte 166; delta from PC=139 = 27 = 0x1B)
             #xEB #x1B                                  ; 137: jmp epilogue
             ;; zero_fill (byte 139):
             #x4D #x31 #xD2                             ; 139: xor r10, r10
             #x4C #x89 #x14 #x25 #x80 #x01 #x00 #x10    ; 142: mov [0x10000180], r10
             #x4C #x89 #x14 #x25 #x88 #x01 #x00 #x10    ; 150: mov [0x10000188], r10
             #x4C #x89 #x14 #x25 #x90 #x01 #x00 #x10    ; 158: mov [0x10000190], r10
             ;; epilogue (byte 166):
             #x48 #x89 #xC4                             ; 166: mov rsp, rax
             #xB8 #x09 #x10 #xAD #xDE                   ; 169: mov eax, 0xDEAD1009
             #xFB                                       ; 174: sti
             #xFF #xE2                                  ; 175: jmp rdx
             ;; pit_normal (byte 177):
             #x5A                                       ; 177: pop rdx
             #x59                                       ; 178: pop rcx
             #x58                                       ; 179: pop rax
             #x48 #x81 #x4C #x24 #x10 #x00 #x02 #x00 #x00  ; 180: or qword [rsp+16], 0x200
             #x48 #xCF                                  ; 189: iretq
             )))
      ;; Write the ISR bytes at +x64-idt-addr+ +0x900.
      (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
      (mvm-emit-u32 buf deadline-isr-addr) (mvm-emit-u32 buf 0)
      (dotimes (i (length deadline-bytes))
        (cond
          ((< i 128)
           (mvm-emit-byte buf #xC6)
           (mvm-emit-byte buf #x47)
           (mvm-emit-byte buf i)
           (mvm-emit-byte buf (aref deadline-bytes i)))
          (t
           (mvm-emit-byte buf #xC6)
           (mvm-emit-byte buf #x87)
           (mvm-emit-u32  buf i)
           (mvm-emit-byte buf (aref deadline-bytes i)))))
      ;; Repatch IDT entry 0x20 (PIT timer) to point at +x64-idt-addr+ +0x900.
      (let* ((eaddr (+ idt-base (* #x20 16)))
             (off-lo (logand deadline-isr-addr #xFFFF))
             (off-mid (logand (ash deadline-isr-addr -16) #xFFFF))
             (off-hi (logand (ash deadline-isr-addr -32) #xFFFFFFFF)))
        (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
        (mvm-emit-u32 buf eaddr) (mvm-emit-u32 buf 0)
        (mvm-emit-byte buf #xB8)
        (mvm-emit-u32 buf (logior off-lo (ash selector 16)))
        (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)
        (mvm-emit-byte buf #xB8)
        (mvm-emit-u32 buf (logior (ash type-attr 8) (ash off-mid 16)))
        (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x04)
        (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x08)
        (mvm-emit-u32 buf off-hi)
        (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x0C)
        (mvm-emit-u32 buf 0)))

    ;; ============================================================
    ;; Exception-recovery handler for #GP (13) and #PF (14).
    ;; Mirrors AArch64 sync-exception entry-4: if a handler-case is
    ;; armed (slot 0x10000180 != 0), restore SP/RBP/RIP from
    ;; 0x10000180/188/190 and IRETQ with RAX = T (#xDEAD1009).  If not
    ;; armed, HLT loop.  Clears slot 0x10000180 before IRETQ so a
    ;; subsequent fault without a re-armed handler-case takes the
    ;; halt path instead of resuming on stale state.
    ;;
    ;; Handler bytes at +x64-idt-addr+ +0x820 (58 bytes):
    ;;   48 83 C4 08              add rsp, 8   (pop CPU-pushed error code)
    ;;   48 B9 ...........        mov rcx, 0x10000180
    ;;   48 8B 01                 mov rax, [rcx]            ; saved RSP
    ;;   48 85 C0                 test rax, rax
    ;;   74 21                    jz halt (+33)
    ;;   48 8B 69 08              mov rbp, [rcx+8]          ; saved RBP
    ;;   48 8B 51 10              mov rdx, [rcx+16]         ; saved RIP -> rdx
    ;;   48 C7 01 00 00 00 00     mov qword [rcx], 0        ; clear slot 180
    ;;   48 89 C4                 mov rsp, rax              ; switch to saved stack
    ;;   68 02 02 00 00           push 0x202                ; RFLAGS (IF set)
    ;;   6A 10                    push 0x10                 ; CS = kernel selector
    ;;   52                       push rdx                  ; RIP
    ;;   B8 09 10 AD DE           mov eax, 0xDEAD1009       ; T sentinel
    ;;   48 CF                    iretq
    ;;   F4                       hlt                       ; halt:
    ;;   EB FD                    jmp halt
    (let* ((sigsegv-isr-addr (+ idt-base #x820))
           (sg-bytes
            ;; 150-byte handler with INLINE per-fork handler-stack pop:
            ;; before iretq, walks the depth counter at 0x10000400 and
            ;; copies frame[depth-1] back into slot 0x10000180/188/190
            ;; (or zero-fills if depth==0).  Subsequent faults during
            ;; the recovered handler-clause body then see the OUTER
            ;; handler-case instead of "no handler", which previously
            ;; caused a clean halt after 2-3 nested faults.
            ;; Epilogue at byte 129 onwards uses STI+JMP instead of
            ;; pushing 3 quads + IRETQ.  IRETQ in 64-bit mode pops 5
            ;; quadwords unconditionally (RIP, CS, RFLAGS, RSP, SS),
            ;; but the original handler only pushed 3 — IRETQ then
            ;; popped RSP and SS from undefined memory above the
            ;; saved-stack region, corrupting state on resume.  With
            ;; STI+JMP, we use the RSP we just set (mov rsp, rax) and
            ;; jump directly to the saved RIP (in RDX) with IF=1.
            ;; Layout shifts: halt now at byte 140 (was 147), so the
            ;; "no handler armed" jz at byte 20-21 targets 0x76 (was
            ;; 0x7D).
            #(#x48 #x83 #xC4 #x08
              #x48 #xB9 #x80 #x01 #x00 #x10 #x00 #x00 #x00 #x00
              #x48 #x8B #x01
              #x48 #x85 #xC0
              #x74 #x76
              #x48 #x8B #x69 #x08
              #x48 #x8B #x51 #x10
              #x4C #x8B #x14 #x25 #x00 #x04 #x00 #x10
              #x4D #x85 #xD2
              #x74 #x3B
              #x49 #xFF #xCA
              #x4C #x89 #x14 #x25 #x00 #x04 #x00 #x10
              #x4D #x6B #xDA #x18
              #x49 #x81 #xC3 #x08 #x04 #x00 #x10
              #x4D #x8B #x13
              #x4C #x89 #x14 #x25 #x80 #x01 #x00 #x10
              #x4D #x8B #x53 #x08
              #x4C #x89 #x14 #x25 #x88 #x01 #x00 #x10
              #x4D #x8B #x53 #x10
              #x4C #x89 #x14 #x25 #x90 #x01 #x00 #x10
              #xEB #x1B
              #x4D #x31 #xD2
              #x4C #x89 #x14 #x25 #x80 #x01 #x00 #x10
              #x4C #x89 #x14 #x25 #x88 #x01 #x00 #x10
              #x4C #x89 #x14 #x25 #x90 #x01 #x00 #x10
              #x48 #x89 #xC4
              #xB8 #x09 #x10 #xAD #xDE
              #xFB
              #xFF #xE2
              #xF4
              #xEB #xFD)))
      ;; mov rdi, sigsegv-isr-addr
      (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
      (mvm-emit-u32 buf sigsegv-isr-addr) (mvm-emit-u32 buf 0)
      ;; For each byte i in sg-bytes: mov byte [rdi+i], imm8.
      ;; i<128: disp8 (C6 47 disp8 imm8).
      ;; i>=128: disp32 (C6 87 disp32 imm8) — signed disp8 would wrap negative.
      (dotimes (i (length sg-bytes))
        (cond
          ((< i 128)
           (mvm-emit-byte buf #xC6)
           (mvm-emit-byte buf #x47)
           (mvm-emit-byte buf i)
           (mvm-emit-byte buf (aref sg-bytes i)))
          (t
           (mvm-emit-byte buf #xC6)
           (mvm-emit-byte buf #x87)
           (mvm-emit-u32  buf i)
           (mvm-emit-byte buf (aref sg-bytes i)))))

      ;; Write IDT entries 13 (#GP) and 14 (#PF) pointing at our handler.
      ;; Same encoding pattern as vector 0x20 above.
      (dolist (vec '(13 14))
        (let* ((eoff (* vec 16))
               (eaddr (+ idt-base eoff))
               (off-lo (logand sigsegv-isr-addr #xFFFF))
               (off-mid (logand (ash sigsegv-isr-addr -16) #xFFFF))
               (off-hi (logand (ash sigsegv-isr-addr -32) #xFFFFFFFF)))
          ;; mov rdi, eaddr
          (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
          (mvm-emit-u32 buf eaddr) (mvm-emit-u32 buf 0)
          ;; Word 0: offset_lo + selector
          (mvm-emit-byte buf #xB8)
          (mvm-emit-u32 buf (logior off-lo (ash selector 16)))
          (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)
          ;; Word 1: type_attr + offset_mid
          (mvm-emit-byte buf #xB8)
          (mvm-emit-u32 buf (logior (ash type-attr 8) (ash off-mid 16)))
          (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x04)
          ;; Word 2: offset_hi
          (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x08)
          (mvm-emit-u32 buf off-hi)
          ;; Word 3: reserved = 0
          (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x0C)
          (mvm-emit-u32 buf 0))))

    ;; === Load IDTR ===
    ;; IDTR format: [limit:16 | base:64] at a scratch location
    ;; Use stack for IDTR descriptor
    ;; lidt [rsp-10] after writing limit+base there
    ;; sub rsp, 16
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x83)
    (mvm-emit-byte buf #xEC) (mvm-emit-byte buf #x10)
    ;; mov word [rsp], limit (48*16-1 = 767)
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x24)
    (mvm-emit-u16 buf (1- (* 48 16)))
    ;; mov qword [rsp+2], idt-base
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xC7)
    (mvm-emit-byte buf #x44) (mvm-emit-byte buf #x24) (mvm-emit-byte buf #x02)
    (mvm-emit-u32 buf idt-base)
    ;; Also need high 4 bytes of base = 0
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x44)
    (mvm-emit-byte buf #x24) (mvm-emit-byte buf #x06)
    (mvm-emit-u32 buf 0)
    ;; lidt [rsp]
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x01)
    (mvm-emit-byte buf #x1C) (mvm-emit-byte buf #x24)
    ;; add rsp, 16  (restore stack)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x83)
    (mvm-emit-byte buf #xC4) (mvm-emit-byte buf #x10)
    ;; Zero handler-case state + handler-stack depth + deadline counter
    ;; BEFORE enabling interrupts.  On Linux the fork-time BSS is
    ;; zeroed; bare-metal has no such guarantee — leftover memory
    ;; could fake a handler-case armed (slot 180 != 0) or a non-empty
    ;; handler-stack (depth > 0), causing the first IRQ-longjmp or
    ;; CLEAR-HANDLER to read a fictional frame.
    ;; Zero qword [0x10000180]: mov rdi, addr; mov qword [rdi], 0
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf #x10000180) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x07)
    (mvm-emit-u32 buf 0)
    ;; Zero qword [0x10000188]: mov qword [rdi+8], 0
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x08)
    (mvm-emit-u32 buf 0)
    ;; Zero qword [0x10000190]: mov qword [rdi+16], 0
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x10)
    (mvm-emit-u32 buf 0)
    ;; Zero qword [0x10000400] (handler-stack depth)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf #x10000400) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x07)
    (mvm-emit-u32 buf 0)
    ;; Zero qword [0x10000C70] (deadline counter)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xBF)
    (mvm-emit-u32 buf #x10000C70) (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x07)
    (mvm-emit-u32 buf 0)
    ;; STI — enable interrupts so the deadline-aware PIT ISR (+x64-idt-addr+ +0x900)
    ;; can fire and longjmp out of infinite-loop tests.  In the original
    ;; boot, IRQs stayed disabled and Lisp used (sti-hlt) at idle points
    ;; only.  For ANSI testing the timer must fire throughout each test.
    (mvm-emit-byte buf #xFB))   ; sti
  )

(defun emit-x64-ap-trampoline (buf)
  "Emit AP (Application Processor) startup trampoline for SMP.
   This code runs in real mode at a low physical address,
   transitions through protected mode to long mode,
   then jumps to the AP entry point."
  ;; Real mode → protected mode → long mode → ap-entry
  ;; The trampoline is copied to physical address 0x8000
  ;; AP processors start here after INIT-SIPI-SIPI
  (dotimes (i 512)
    (mvm-emit-byte buf #x90)))  ; Placeholder

;;; ============================================================
;;; x86-64 Platform Initialization
;;; ============================================================

(defun x64-init-serial (port)
  "Generate code to initialize a serial port (COM1 = 0x3F8).
   Returns a list of (port value) pairs for OUT instructions."
  (let ((base port))
    (list
     ;; Disable interrupts
     (cons (+ base 1) #x00)
     ;; Enable DLAB
     (cons (+ base 3) #x80)
     ;; Set baud rate divisor = 1 (115200 baud)
     (cons (+ base 0) #x01)
     (cons (+ base 1) #x00)
     ;; 8N1 (8 bits, no parity, 1 stop bit)
     (cons (+ base 3) #x03)
     ;; Enable FIFO
     (cons (+ base 2) #xC7)
     ;; RTS/DSR set
     (cons (+ base 4) #x0B))))

(defun x64-init-lapic ()
  "Generate LAPIC initialization sequence for the BSP.
   The LAPIC is memory-mapped at 0xFEE00000."
  ;; Spurious interrupt vector: 0xFF, enable LAPIC
  ;; Timer: periodic mode, vector 0x40
  ;; Divide configuration: divide by 16
  '((:lapic-svr   . #xFEE000F0)
    (:lapic-timer  . #xFEE00320)
    (:lapic-divide . #xFEE003E0)
    (:lapic-count  . #xFEE00380)))

;;; ============================================================
;;; x86-64 Interrupt Handling
;;; ============================================================

(defun x64-idt-entry (vector handler-addr ist dpl)
  "Create an IDT entry for the given vector.
   Returns an 16-byte IDT gate descriptor."
  (let ((offset-low (logand handler-addr #xFFFF))
        (offset-mid (logand (ash handler-addr -16) #xFFFF))
        (offset-high (logand (ash handler-addr -32) #xFFFFFFFF))
        (selector #x08)  ; 64-bit code segment
        (type-attr (logior #x8E (ash dpl 5))))  ; Present, interrupt gate
    (list offset-low selector ist type-attr offset-mid offset-high 0)))

;;; ============================================================
;;; x86-64 SMP (Symmetric Multi-Processing)
;;; ============================================================

(defun x64-init-smp-sequence ()
  "Return the SMP initialization sequence for x86-64.
   Uses INIT-SIPI-SIPI protocol:
   1. Send INIT IPI to all APs
   2. Wait 10ms
   3. Send SIPI with trampoline address
   4. Wait 200us
   5. Send SIPI again (some CPUs need two)
   6. APs start executing trampoline"
  '(:init-ipi :wait-10ms :sipi :wait-200us :sipi))

(defun x64-percpu-layout ()
  "Return the per-CPU structure layout for x86-64.
   Accessed via GS segment base."
  '((:self-ptr     0   8)   ; Pointer to this per-CPU struct
    (:reduction     8   8)   ; Reduction counter (tagged fixnum)
    (:cpu-id       16   8)   ; CPU number (tagged fixnum)
    (:current-actor 24  8)   ; Current actor pointer
    (:obj-alloc    40   8)   ; Per-actor object alloc pointer
    (:obj-limit    48   8)   ; Per-actor object alloc limit
    (:idle-stack   56   8))) ; Idle stack top for this CPU

;;; ============================================================
;;; x86-64 Boot Integration
;;; ============================================================

(defun x64-boot-descriptor ()
  "Return the x86-64 boot descriptor for image building"
  (list :arch :x86-64
        :multiboot-header-fn #'emit-x64-multiboot-header
        :boot32-fn #'emit-x64-boot32
        :kernel64-entry-fn #'emit-x64-kernel64-entry
        :ap-trampoline-fn #'emit-x64-ap-trampoline
        :serial-init-fn #'x64-init-serial
        :smp-sequence-fn #'x64-init-smp-sequence
        :percpu-layout-fn #'x64-percpu-layout
        :load-addr +x64-kernel-load-addr+
        :stack-top +x64-stack-top+
        :cons-base +x64-cons-base+
        :general-base +x64-general-base+))
