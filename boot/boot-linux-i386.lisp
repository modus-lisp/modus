;;;; boot-linux-i386.lisp — Linux i386 (32-bit) ELF entry for Modus
;;;;
;;;; Counterpart to boot-linux-x64.lisp / boot-linux-aarch64.lisp: produces a
;;;; hosted Linux userspace ELF32 executable instead of a bare-metal multiboot
;;;; kernel.  This is the FAST iteration vehicle for the i386 CL port — the
;;;; other two 64-bit targets each have one, i386 had only bare metal.
;;;;
;;;; Run with:  qemu-i386-static ./modus-i386-cli [args...]
;;;; (binfmt_misc is generally NOT registered for i386 on an x86-64 box, so a
;;;;  bare ./binary silently fails to exec — always go through qemu-i386-static.)

(in-package :modus.mvm)

;;; ============================================================
;;; Linux i386 memory map
;;; ============================================================
;;;
;;;   0x08048000  ELF image.  ehdr(52) + phdr(32) = 84 bytes of wrapper,
;;;               then the boot stub, the JMP to kernel-main, native code,
;;;               bytecode, constant pool, fn table, source blob, metadata.
;;;   0x10000000  BSS block — SAME PT_LOAD segment, p_memsz > p_filesz, so
;;;               the kernel demand-zeroes it.  Deliberately at the SAME
;;;               absolute addresses the 64-bit ports use, because the shared
;;;               CL runtime hard-codes them:
;;;                 0x10000040..0x10000068  Cheney GC metadata
;;;                 0x10000080              global special-variable table
;;;                 0x10000088              symbol intern table
;;;                 0x10000090              MV-count, 0x98.. MV-values
;;;                 0x10000148              keyword intern table
;;;                 0x10000170              package-by-hash table
;;;                 0x10000200              argc
;;;                 0x10000208              argv[1] string (64 bytes)
;;;                 0x10000248              argv[2] string (64 bytes)
;;;                 0x10000400              per-fork handler stack
;;;                 0x10000A00              i386 global slot block  <-- i386 only
;;;   0x30000000  heap (mmap2, MAP_ANON)
;;;
;;; The i386 global slot block is the one genuinely new thing.  x64 and
;;; aarch64 keep the alloc pointer, alloc limit, NIL constant, closure-env
;;; register, nargs and MV-count in PHYSICAL registers (R12/R14/R15/R13...).
;;; i386 has eight registers and every one is already spoken for, so those
;;; six live in memory instead — see translate-i386.lisp's
;;; *i386-globals-base* / I386-SET-GLOBALS-BASE.  #x600 (the bare-metal
;;; default) is unusable here: it is below mmap_min_addr.

(defconstant +linux-i386-load-addr+   #x08048000)
(defconstant +linux-i386-bss-addr+    #x10000000)
(defconstant +linux-i386-bss-end+     #x10001000)
(defconstant +linux-i386-globals+     #x10000A00
  "Base of the i386 absolute-address global slot block (VA/VL/VN/nargs/
   cenv/mv-count), inside the demand-zeroed BSS.")

(defconstant +linux-i386-heap-hint+   #x30000000)
(defconstant +linux-i386-heap-size+   #x20000000)  ; 512 MB = two 256 MB semispaces
(defconstant +linux-i386-gc-midpoint+ #x10000000
  "Cheney semispace boundary: from-space is [alloc_start, midpoint),
   to-space is [midpoint, 2*midpoint).  Sized so that EVERY address in the
   arena stays below 2^30 and is therefore a FIXNUM on the 30-bit tower —
   which is what lets gc.lisp hold them as ordinary Lisp integers without
   promoting to bignums mid-collection (the aarch64 re-entrancy trap).")
(defconstant +linux-i386-stack-addr+ #x18000000
  "MAP_FIXED base of our OWN stack.  The kernel-supplied stack sits around
   0x40800000, which is just above 2^30-1 and therefore NOT a fixnum, so
   %gc-stack-base could not be represented at all.  Rather than change
   gc.lisp's metadata convention — shared code, load-bearing on two working
   targets — we relocate the stack somewhere that satisfies it.  This is
   i386-local by construction: x64 and aarch64 are untouched.")
(defconstant +linux-i386-stack-size+ #x800000)   ; 8 MB
(defconstant +linux-i386-heap-alloc-start+ #x200
  "Offset from heap base to the first allocatable byte.  The low 512 bytes
   mirror argc/argv the way the 64-bit ports do.")

(defparameter *linux-i386-vl-offset* #x10000000
  "Offset from heap base for VL (the alloc limit).  With GC OFF this is set
   just short of the mapping end so :gc-check never fires; a GC-on build
   would lower it to the semispace midpoint.

   HARD CEILING AT 2^31, measured.  The arena top must stay below 2^31:
   with the top at 0x9EFFD000 a 64 KiB SHA-256 COMPLETED but produced a WRONG
   digest (heap addresses above 2^31 have the sign bit set and something in
   the path does signed arithmetic on them); with the top at 0x5EFFD000 the
   same run SIGSEGVs on arena exhaustion.  So this offset is set to keep the
   top at 0x7EFFD000, just under 2^31 — the largest arena that is both safe
   and useful.  Growing past it trades a crash for silent corruption, which
   is strictly worse.

   CONSEQUENCE: bumping the arena is NOT a general fallback for the missing
   collector.  64 KiB of SHA-256 needs somewhere between 1 GB and 2 GB of
   permanent allocation, which is already past what fits below 2^31.  Bulk
   crypto on i386 genuinely REQUIRES a collector; the arena only buys the
   small-input cases.

   THE CEILING, stated because i386 HAS NO COLLECTOR (see the note below).
   Every allocation is permanent, so this offset IS the total lifetime
   allocation budget of the process — 2032 MiB.  When VA reaches VL the
   :gc-check calls a collector that does not exist and the process dies on
   `int $0x31`.  Raised from 496 MiB because SHA-256 over 64 KiB exhausted
   that; see WS5 notes for the measured input-size ceiling.

   WHY THERE IS NO COLLECTOR, and why wiring the existing Lisp-side
   %GC-COLLECT is NOT a small job on i386.  gc.lisp reads its metadata with
   (mem-ref addr :u64), and :u64 is RAW (needs-tag nil), so memory must hold
   address<<1 for the Lisp value to be the address.  On a 30-bit tower:
     heap base  0x1FFFD000   fits a fixnum, <<1 fits 32-bit signed   OK
     VL         0x3EFFD000   fits, <<1 fits                          OK
     space_size 0x1F000000   fits, <<1 fits                          OK
     stack base ~0x40800000  1082130432 > 2^30-1                     DOES NOT FIT
   The stack base cannot be represented at all in the convention gc.lisp
   uses, and <<1 overflows signed 32-bit on top of that.  Representing it as
   a promoted bignum instead makes %gc-scan-stack do bignum arithmetic
   DURING a collection, which allocates, which re-trips gc-check at
   from-space-full — the documented aarch64 re-entrancy bug
   (reference_aa64_gc_poison_root_cause), there fixed by keeping the tag
   checks allocation-free.  Add that i386 has no object-start bitmap, so
   %gc-forward-slot would have no conservative-root validation either — the
   poison class that cost aarch64 most of a session.

   ONE PIECE OF GOOD LUCK worth recording: every address INSIDE the arena
   (0x1FFFD000..0x3EFFD000 at the old size) is below 2^30, so heap pointers
   themselves are fixnums and the aarch64 re-entrancy trap does not fire for
   them.  Only the STACK base, which lives above the arena, breaks.")

(defconstant +linux-i386-nil-value+ #xDEAD0001
  "NIL.  Same immediate the 64-bit ports use — low nibble 1 (cons tag), a
   value that is never dereferenced.  NOTE this differs from the bare-metal
   i386 boot, which uses 0.")

;;; ============================================================
;;; Byte emitters (local names so loading this file alongside
;;; boot-linux-x64.lisp cannot collide)
;;; ============================================================

(defun i386l-bytes (buf &rest bytes)
  (dolist (b bytes) (mvm-emit-byte buf b)))

(defun i386l-le32 (buf val)
  (let ((v (logand val #xFFFFFFFF)))
    (mvm-emit-byte buf (logand v #xFF))
    (mvm-emit-byte buf (logand (ash v -8) #xFF))
    (mvm-emit-byte buf (logand (ash v -16) #xFF))
    (mvm-emit-byte buf (logand (ash v -24) #xFF))))

(defun i386l-le16 (buf val)
  (mvm-emit-byte buf (logand val #xFF))
  (mvm-emit-byte buf (logand (ash val -8) #xFF)))

;;; Convenience: MOV r32, imm32 / MOV [abs32], imm32 / MOV [abs32], r32
(defun i386l-mov-reg-imm (buf reg imm) (i386l-bytes buf (+ #xB8 reg)) (i386l-le32 buf imm))
(defun i386l-mov-abs-imm (buf addr imm)
  (i386l-bytes buf #xC7 #x05) (i386l-le32 buf addr) (i386l-le32 buf imm))
(defun i386l-mov-abs-reg (buf addr reg)
  ;; 89 /r with mod=00 rm=101 (disp32)
  (i386l-bytes buf #x89 (logior #x05 (ash (logand reg 7) 3))) (i386l-le32 buf addr))

;;; ============================================================
;;; ELF32 little-endian wrapper (EM_386)
;;; ============================================================

(defun wrap-in-elf32-le-i386 (raw-bytes load-addr &key bss-end)
  "Wrap RAW-BYTES in a minimal ELF32-LE i386 executable: one ELF header
   (52 bytes) + one PT_LOAD program header (32 bytes) = 84 bytes, matching
   WRAP-HEADER-SIZE-FOR-BOOT's :linux-i386 arm.

   p_memsz is extended to BSS-END so the kernel demand-zeroes the whole
   fixed-address BSS block the shared CL runtime relies on (same trick as
   wrap-in-elf64-le on x64, where p_memsz >> p_filesz).

   NOTE the ELF32 program-header field ORDER differs from ELF64:
     ELF32: type offset vaddr paddr filesz memsz flags align
     ELF64: type flags  offset vaddr paddr  filesz memsz align
   Getting that wrong yields a segment the kernel silently refuses to map."
  (let* ((ehdr-size 52)
         (phdr-size 32)
         (header-total (+ ehdr-size phdr-size))
         (raw-len (length raw-bytes))
         (file-size (+ header-total raw-len))
         (mem-size (if bss-end
                       (max file-size (- bss-end load-addr))
                       file-size))
         (entry-point (+ load-addr header-total))
         (buf (make-mvm-buffer)))
    ;; ---- ELF header (52 bytes) ----
    (i386l-bytes buf #x7F (char-code #\E) (char-code #\L) (char-code #\F))
    (i386l-bytes buf 1)                 ; EI_CLASS  = ELFCLASS32
    (i386l-bytes buf 1)                 ; EI_DATA   = ELFDATA2LSB
    (i386l-bytes buf 1)                 ; EI_VERSION
    (i386l-bytes buf 0)                 ; EI_OSABI  = SYSV
    (dotimes (i 8) (mvm-emit-byte buf 0))  ; EI_ABIVERSION + padding
    (i386l-le16 buf 2)                  ; e_type    = ET_EXEC
    (i386l-le16 buf 3)                  ; e_machine = EM_386
    (i386l-le32 buf 1)                  ; e_version
    (i386l-le32 buf entry-point)        ; e_entry
    (i386l-le32 buf ehdr-size)          ; e_phoff
    (i386l-le32 buf 0)                  ; e_shoff (no section headers)
    (i386l-le32 buf 0)                  ; e_flags
    (i386l-le16 buf ehdr-size)          ; e_ehsize
    (i386l-le16 buf phdr-size)          ; e_phentsize
    (i386l-le16 buf 1)                  ; e_phnum
    (i386l-le16 buf 0)                  ; e_shentsize
    (i386l-le16 buf 0)                  ; e_shnum
    (i386l-le16 buf 0)                  ; e_shstrndx
    ;; ---- Program header (32 bytes), ELF32 field order ----
    (i386l-le32 buf 1)                  ; p_type   = PT_LOAD
    (i386l-le32 buf 0)                  ; p_offset (file offset 0 == vaddr mod align)
    (i386l-le32 buf load-addr)          ; p_vaddr
    (i386l-le32 buf load-addr)          ; p_paddr
    (i386l-le32 buf file-size)          ; p_filesz
    (i386l-le32 buf mem-size)           ; p_memsz  (covers the BSS block)
    (i386l-le32 buf 7)                  ; p_flags  = R|W|X
    (i386l-le32 buf #x1000)             ; p_align
    ;; ---- Payload ----
    (loop for b across raw-bytes do (mvm-emit-byte buf b))
    (mvm-buffer-used-bytes buf)))

;;; ============================================================
;;; Entry stub
;;; ============================================================

(defun emit-linux-i386-entry (buf)
  "Emit the Linux/i386 entry stub.

   On entry ESP points at [argc, argv[0], argv[1], ..., NULL, envp...] with
   4-byte slots (the 64-bit ports see the same shape with 8-byte slots).

   Sequence: stage argc/argv into the fixed BSS addresses the CLI probes
   read, mmap the heap, publish VA/VL/VN into the i386 global slot block,
   publish the Cheney GC metadata, then fall through to the JMP that
   cross.lisp emits to kernel-main.

   Ends with NOP padding chosen so that native code byte 0 lands on a
   16-byte VIRTUAL boundary — see the comment at the padding site."
  ;; --- argc / argv[1] / argv[2] off the incoming stack ---
  (i386l-bytes buf #x8B #x1C #x24)              ; mov ebx, [esp]      (argc)
  (i386l-bytes buf #x8B #x44 #x24 #x08)         ; mov eax, [esp+8]    (argv[1])
  (i386l-bytes buf #x8B #x54 #x24 #x0C)         ; mov edx, [esp+12]   (argv[2])
  (i386l-bytes buf #x52)                        ; push edx            (save argv[2])
  (i386l-bytes buf #x50)                        ; push eax            (save argv[1])

  ;; [0x10000200] = argc
  (i386l-mov-abs-reg buf #x10000200 3)          ; mov [0x10000200], ebx

  ;; Zero 128 bytes at 0x10000208 (both argv string buffers).
  (i386l-mov-reg-imm buf 7 #x10000208)          ; mov edi, 0x10000208
  (i386l-mov-reg-imm buf 1 32)                  ; mov ecx, 32 (dwords)
  (i386l-bytes buf #x31 #xC0)                   ; xor eax, eax
  (i386l-bytes buf #xF3 #xAB)                   ; rep stosd

  ;; Copy argv[1] -> 0x10000208 when argc > 1.  The copy block below is
  ;; exactly 15 bytes: 3 + 5 + 5 + 2.
  (i386l-bytes buf #x83 #xFB #x01)              ; cmp ebx, 1
  (i386l-bytes buf #x7E #x0F)                   ; jle +15
  (i386l-bytes buf #x8B #x34 #x24)              ; mov esi, [esp]      (argv[1])   3
  (i386l-mov-reg-imm buf 7 #x10000208)          ; mov edi, 0x10000208             5
  (i386l-mov-reg-imm buf 1 63)                  ; mov ecx, 63                     5
  (i386l-bytes buf #xF3 #xA4)                   ; rep movsb                       2

  ;; Copy argv[2] -> 0x10000248 when argc > 2.  Block is 16 bytes (the
  ;; source load takes one more byte for the disp8).
  (i386l-bytes buf #x83 #xFB #x02)              ; cmp ebx, 2
  (i386l-bytes buf #x7E #x10)                   ; jle +16
  (i386l-bytes buf #x8B #x74 #x24 #x04)         ; mov esi, [esp+4]    (argv[2])   4
  (i386l-mov-reg-imm buf 7 #x10000248)          ; mov edi, 0x10000248             5
  (i386l-mov-reg-imm buf 1 63)                  ; mov ecx, 63                     5
  (i386l-bytes buf #xF3 #xA4)                   ; rep movsb                       2

  (i386l-bytes buf #x83 #xC4 #x08)              ; add esp, 8  (drop saved argv ptrs)

  ;; --- Relocate the stack below 2^30 -------------------------------------
  ;; Must come AFTER the argv staging above, which reads the kernel-supplied
  ;; stack and copies argv[1]/argv[2] into fixed BSS.  Nothing else depends on
  ;; the original ESP, and we never return, so switching is safe here.
  ;; mmap2(0x18000000, 8MB, PROT_READ|WRITE, PRIVATE|ANON|FIXED, -1, 0)
  (i386l-mov-reg-imm buf 3 +linux-i386-stack-addr+)  ; ebx = addr
  (i386l-mov-reg-imm buf 1 +linux-i386-stack-size+)  ; ecx = length
  (i386l-mov-reg-imm buf 2 3)                        ; edx = PROT_READ|WRITE
  (i386l-mov-reg-imm buf 6 #x32)                     ; esi = PRIVATE|ANON|FIXED
  (i386l-mov-reg-imm buf 7 #xFFFFFFFF)               ; edi = -1
  (i386l-bytes buf #x55)                             ; push ebp
  (i386l-bytes buf #x31 #xED)                        ; xor ebp, ebp (pgoff 0)
  (i386l-mov-reg-imm buf 0 192)                      ; SYS_mmap2
  (i386l-bytes buf #xCD #x80)
  (i386l-bytes buf #x5D)                             ; pop ebp
  ;; switch to the new stack (top, 16-aligned)
  (i386l-bytes buf #xBC)                             ; mov esp, imm32
  (i386l-le32 buf (- (+ +linux-i386-stack-addr+ +linux-i386-stack-size+) 16))

  ;; --- mmap2(hint, size, PROT_READ|WRITE, MAP_PRIVATE|MAP_ANON, -1, 0) ---
  ;; i386 has no 6-register syscall convention beyond EBP, and old_mmap(90)
  ;; takes a struct pointer; mmap2(192) is the clean register form.  Its 6th
  ;; argument (page offset) goes in EBP, so EBP is stacked around the call.
  (i386l-mov-reg-imm buf 3 +linux-i386-heap-hint+)   ; mov ebx, hint
  (i386l-mov-reg-imm buf 1 +linux-i386-heap-size+)   ; mov ecx, length
  (i386l-mov-reg-imm buf 2 3)                        ; mov edx, PROT_READ|PROT_WRITE
  (i386l-mov-reg-imm buf 6 #x22)                     ; mov esi, MAP_PRIVATE|MAP_ANONYMOUS
  (i386l-mov-reg-imm buf 7 #xFFFFFFFF)               ; mov edi, -1 (fd)
  (i386l-bytes buf #x55)                             ; push ebp
  (i386l-bytes buf #x31 #xED)                        ; xor ebp, ebp (pgoff = 0)
  (i386l-mov-reg-imm buf 0 192)                      ; mov eax, SYS_mmap2
  (i386l-bytes buf #xCD #x80)                        ; int 0x80
  (i386l-bytes buf #x5D)                             ; pop ebp
  ;; EAX = heap base.  (A failure returns a small negative value; the image
  ;; would fault immediately on the first allocation, which is the loudest
  ;; and most debuggable outcome available without a message path yet.)

  ;; --- argc/argv mirror at the heap base (parity with the 64-bit ports) ---
  (i386l-bytes buf #x89 #x18)                        ; mov [eax], ebx (argc)

  ;; --- VA = heap + alloc-start ---
  (i386l-bytes buf #x89 #xC1)                        ; mov ecx, eax
  (i386l-bytes buf #x81 #xC1) (i386l-le32 buf +linux-i386-heap-alloc-start+) ; add ecx, imm32
  (i386l-mov-abs-reg buf (+ +linux-i386-globals+ #x00) 1)   ; mov [VA], ecx

  ;; --- VL = heap + vl-offset (alloc limit / GC trigger) ---
  (i386l-bytes buf #x89 #xC1)                        ; mov ecx, eax
  (i386l-bytes buf #x81 #xC1) (i386l-le32 buf *linux-i386-vl-offset*)
  (i386l-mov-abs-reg buf (+ +linux-i386-globals+ #x04) 1)   ; mov [VL], ecx

  ;; --- VN = NIL ---
  (i386l-mov-abs-imm buf (+ +linux-i386-globals+ #x08) +linux-i386-nil-value+)

  ;; nargs / cenv / mv-count slots are already zero (demand-zeroed BSS).

  ;; --- Cheney GC metadata at the shared fixed addresses ---
  ;; --- Cheney GC metadata, stored SHIFTED LEFT ONE -----------------------
  ;; gc.lisp reads these with (mem-ref addr :u64), and :u64 is RAW
  ;; (memory-width-code -> needs-tag NIL), so the raw word IS the tagged Lisp
  ;; value.  Memory must therefore hold address<<1 for the Lisp value to be
  ;; the address.  Storing raw was the latent bug documented on aarch64
  ;; (reference_aa64_gc_poison_root_cause): harmless while nothing collects,
  ;; and it HALVES every address the moment something does.
  ;; [0x10000040] from_start = (heap + alloc_start) << 1
  (i386l-bytes buf #x89 #xC1)
  (i386l-bytes buf #x81 #xC1) (i386l-le32 buf +linux-i386-heap-alloc-start+)
  (i386l-bytes buf #x01 #xC9)                         ; add ecx, ecx  (<<1)
  (i386l-mov-abs-reg buf #x10000040 1)
  ;; [0x10000048] to_start = (heap + midpoint) << 1
  (i386l-bytes buf #x89 #xC1)
  (i386l-bytes buf #x81 #xC1) (i386l-le32 buf +linux-i386-gc-midpoint+)
  (i386l-bytes buf #x01 #xC9)
  (i386l-mov-abs-reg buf #x10000048 1)
  ;; [0x10000050] space_size << 1
  (i386l-mov-abs-imm buf #x10000050
                     (ash (- +linux-i386-gc-midpoint+ +linux-i386-heap-alloc-start+) 1))
  ;; [0x10000058] stack_base = our RELOCATED stack top, << 1
  (i386l-mov-reg-imm buf 1 (- (+ +linux-i386-stack-addr+ +linux-i386-stack-size+) 16))
  (i386l-bytes buf #x01 #xC9)
  (i386l-mov-abs-reg buf #x10000058 1)
  ;; [0x10000060] gc_count = 0
  (i386l-mov-abs-imm buf #x10000060 0)

  ;; --- frame pointer ---
  (i386l-bytes buf #x89 #xE5)                         ; mov ebp, esp

  ;; --- Alignment padding ---
  ;; Native code starts at  load-addr + 84 (ELF wrapper) + (this stub) + 5
  ;; (the JMP rel32 to kernel-main that cross.lisp appends).  With
  ;; *i386-fn-tag-3* on, :fn-addr OR-3s function entry addresses, so entries
  ;; must be 16-byte aligned in the RUNTIME address — and the translator's
  ;; per-function padding is computed relative to native-code byte 0.  Pad
  ;; here so that byte 0 is itself 16-aligned; then
  ;; *i386-native-code-offset* = 0 is exactly right.
  (loop until (zerop (mod (+ 84 (mvm-buffer-position buf) 5) 16))
        do (mvm-emit-byte buf #x90)))                 ; nop

;;; ============================================================
;;; Boot descriptor
;;; ============================================================

(defun linux-i386-boot-descriptor ()
  (list :arch :i386
        :entry-fn #'emit-linux-i386-entry
        :load-addr +linux-i386-load-addr+
        :elf-format :linux-i386
        :bss-end +linux-i386-bss-end+))
