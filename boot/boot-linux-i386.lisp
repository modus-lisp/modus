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
(defconstant +linux-i386-bss-end+     #x10020000
  "End of the demand-zeroed BSS block.  Was #x10001000 (one page) while the
   image only needed the fixed metadata slots.  File I/O needs two scratch
   buffers at fixed RAW addresses — cl-fileio's *cstr-scratch* (C-string
   staging for path arguments) and *io-buf-addr* (the 4 KB read/write
   buffer) — and i386 has nowhere else to put them: the ELF ends around
   0x0A800000, the stack is a MAP_FIXED 8 MB at 0x18000000 and the heap a
   512 MB arena at 0x30000000, all GC-owned.  Extending p_memsz by 124 KB
   costs nothing (the pages are demand-zeroed and only two are touched) and,
   critically, RESERVES the range at exec time, so the later NULL-hinted
   bitmap mmaps cannot land in it.  Both scratch addresses stay below 2^30,
   which they must: syscall3 takes TAGGED fixnums and untags with SAR, so an
   address at or above 2^30 could not be passed at all on this word size.")
(defconstant +linux-i386-globals+     #x10000A00
  "Base of the i386 absolute-address global slot block (VA/VL/VN/nargs/
   cenv/mv-count), inside the demand-zeroed BSS.")

;;; ---- Staged argv/envp (BSS) --------------------------------------------
;;; The kernel hands us argv/envp on ITS stack, near 0x40800000.  An MVM
;;; mem-ref carries its address as a TAGGED fixnum (the opcode untags with
;;; SHR 1), so on a 32-bit word any address at or above 2^30 is
;;; unrepresentable — measured: esp=0x40800390.  argc survives only because
;;; the boot stub already copies it down to 0x10000200.
;;;
;;; So the stub stages the WHOLE vector into the BSS: the strings into an
;;; arena, and a pointer array holding their BSS addresses.  The array has
;;; exactly the shape the kernel stack had — argv[0..n-1], NULL, envp[0..m-1],
;;; NULL — so lib/cli-toplevel.lisp's walking logic is unchanged; only the
;;; base address and the slot width differ.  Both regions sit below 2^30, so
;;; every address in them is an ordinary fixnum.
(defconstant +linux-i386-argv-ptrs+  #x10009000
  "Staged pointer array: argv[0..n-1], NULL, envp[0..m-1], NULL. 4-byte slots.")
(defconstant +linux-i386-argv-ptrs-end+ #x1000A000)
(defconstant +linux-i386-argv-arena+ #x1000A000
  "Staged string arena — every argv/envp string, NUL-terminated, packed.")
(defconstant +linux-i386-argv-arena-end+ #x1001E000)

(defconstant +linux-i386-heap-hint+   #x30000000)
(defconstant +linux-i386-gc-guard+    #x1000000
  "16 MB of MAPPED-BUT-UNCOUNTED memory past the SECOND semispace's from_end.
   This is boot-linux-x64.lisp's +linux-x64-gc-guard+, which the i386 port
   omitted; the omission is bug B3 (16 of the 22 ladder libraries died on it).

   WHY IT IS NEEDED.  :gc-check (translate-i386.lisp, +op-gc-check+) tests
   `VA < VL' BEFORE an allocation whose SIZE it does not know, so the alloc
   that follows a passing check overshoots VL by up to that object's whole
   size — measured 0xB8450 (754 KB) for one (make-array 1000000).  In the
   FIRST semispace that overshoot lands in the (mapped) second semispace and
   is harmless: copy_object's read pointer trails its write pointer by a
   constant, so the straddling object still copies correctly.  In the SECOND
   semispace from_end sat only +linux-i386-heap-alloc-start+ = 512 BYTES below
   the end of the mmap, so ANY allocation bigger than 512 bytes that tripped
   the check ran off the end of the mapping and the object's own initialising
   stores SIGSEGV'd.  Measured on the pre-fix image, deterministically:
   gc_count=1, VA=0x400b5250, VL=0x3fffce00, mapping end 0x3fffd000, fault at
   cr2=0x3ffff000 inside an array-element store.

   RESIDUAL, stated rather than assumed (and identical on x64): a SINGLE
   allocation larger than the guard still overruns.  The real cure is a
   size-aware :gc-check, which is a shared-compiler change, not an arch one.")
(defconstant +linux-i386-heap-size+   (+ #x20000000 +linux-i386-gc-guard+)
  "512 MB of arena (two 256 MB semispaces) + the 16 MB overshoot guard.")
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
(defconstant +linux-i386-bitmap-size+ #x800000
  "8 MB per bitmap = 1 bit / 16-byte granule over a 1 GB span; the arena is
   512 MB + the 16 MB guard, so 4.125 MB is used.  TWO bitmaps:
   object-start (conservative-root validation) and cons-kind (so %gc-scan-copied walks to-space by TYPE rather
   than forwarding every word — the latter mis-forwards bignum limbs, and
   SHA-256 on a 30-bit tower allocates almost nothing but bignums).")
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

   *** STALE AS OF #202 — READ THIS BEFORE BELIEVING THE PARAGRAPHS BELOW. ***
   The text from here to the end of this docstring describes the PRE-#202
   world, in which i386 had no collector at all.  That is no longer true for
   the HOSTED Linux build:

     translate-i386.lisp:4345
       (setf *i386-gc-collect-label*
             (and *i386-gc-enabled* *i386-linux-mode* (i386-make-label)))

   so a hosted i386 image DOES wire a real collector — `build-i386-cli` at
   layer 5 reports `GC: collector=T bitmap=T`.  BARE-METAL i386 still has
   none (its global slot block at #x600 is zero, so the collector would run
   with a null bitmap base and a garbage from-space); it keeps `int $0x31`
   and stays byte-identical to a pre-collector build.

   Consequence for debugging: on the hosted CLI a crash under sustained
   allocation is NOT automatically \"arena exhausted, no collector to call\".
   That inference is what this stale text invites, and it is wrong for the
   very build most likely to be under test (see task #218).  VL is at the
   semispace midpoint here, which is the GC-ON value, not the GC-OFF one.

   THE CEILING, as it applied WITHOUT a collector (bare metal, still true
   there).  Every allocation is permanent, so this offset IS the total
   lifetime allocation budget of the process — 2032 MiB.  When VA reaches VL
   the :gc-check calls a collector that does not exist and the process dies
   on `int $0x31`.  Raised from 496 MiB because SHA-256 over 64 KiB
   exhausted that; see WS5 notes for the measured input-size ceiling.

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

(defun i386l-le32-list (v)
  "The 4 little-endian bytes of V, as a LIST (for length-computed blocks)."
  (let ((n (logand v #xFFFFFFFF)))
    (list (logand n #xFF) (logand (ash n -8) #xFF)
          (logand (ash n -16) #xFF) (logand (ash n -24) #xFF))))

(defun i386l-rel8 (d)
  "One signed displacement byte.  Errors rather than truncating: a silently
   wrapped jump target is the worst possible boot bug to debug."
  (assert (<= -128 d 127) (d) "rel8 displacement ~D out of range" d)
  (list (logand d #xFF)))

(defun emit-i386-stage-argv (buf)
  "Copy the kernel's argv AND envp — pointers and the strings themselves —
   down into the BSS, so both are addressable on a 32-bit word.

   Must run while ESP still points at the kernel's stack, i.e. BEFORE the
   relocation below.  Clobbers EAX/EBX/ECX-free/EDX/ESI/EDI/EBP; nothing
   after it depends on those.

   Walks slots from &argv[0] until it has seen TWO consecutive NULLs (end of
   argv, then end of envp), copying each string into the arena and recording
   its BSS address.  Both regions are bounds-checked; overflowing either stops
   the walk cleanly rather than scribbling over the heap metadata below.

   EVERY jump displacement here is COMPUTED from the measured length of the
   emitted blocks, never hand-counted — a wrong displacement in a boot stub
   presents as an unattributable early crash, and this session already lost a
   round to hand arithmetic that a script got right."
  (let* ((pro (append (list #x89 #xE3)                       ; mov ebx, esp
                      (list #x83 #xC3 #x04)                  ; add ebx, 4  -> &argv[0]
                      (cons #xBA (i386l-le32-list +linux-i386-argv-ptrs+))   ; mov edx, ptrs
                      (cons #xBF (i386l-le32-list +linux-i386-argv-arena+))  ; mov edi, arena
                      (list #x31 #xED)))                     ; xor ebp, ebp (NULL run)
         ;; --- loop head ---
         (a1  (append (list #x81 #xFA)
                      (i386l-le32-list +linux-i386-argv-ptrs-end+)))  ; cmp edx, ptrs-end
         (sz-a2 6)                                           ; jae DONE (0F 83 rel32)
         (a3  (list #x8B #x03))                              ; mov eax, [ebx]
         (a4  (list #x85 #xC0))                              ; test eax, eax
         (sz-a5 2)                                           ; jnz COPY (rel8)
         (a6  (list #xC7 #x02 #x00 #x00 #x00 #x00))          ; mov dword [edx], 0
         (a7  (list #x83 #xC2 #x04))                         ; add edx, 4
         (a8  (list #x83 #xC3 #x04))                         ; add ebx, 4
         (a9  (list #x45))                                   ; inc ebp
         (a10 (list #x83 #xFD #x02))                         ; cmp ebp, 2
         (sz-a11 2)                                          ; jl LOOP (rel8)
         (sz-a12 5)                                          ; jmp DONE (rel32)
         ;; --- copy one string ---
         (c13 (append (list #x81 #xFF)
                      (i386l-le32-list +linux-i386-argv-arena-end+)))  ; cmp edi, arena-end
         (sz-c14 2)                                          ; jae DONE (rel8)
         ;; NOTE: no `xor ebp,ebp' here.  EBP counts NULL terminators SEEN IN
         ;; TOTAL, not consecutively.  The stack is argv[..] NULL envp[..] NULL
         ;; and then the AUXILIARY VECTOR, which is (type,value) pairs, not
         ;; pointers.  Resetting the count on each non-NULL entry meant envp's
         ;; own terminator read as the FIRST null again, so the walk ran on
         ;; into auxv and dereferenced AT_* type codes as char* -- it faulted
         ;; on esi=3 (AT_PHDR) after staging 33 slots.  Two terminators total
         ;; is exactly the end of envp.
         (c15 (list))
         (c16 (list #x89 #x3A))                              ; mov [edx], edi
         (c17 (list #x83 #xC2 #x04))                         ; add edx, 4
         (c18 (list #x89 #xC6))                              ; mov esi, eax
         (s19 (list #x8A #x06))                              ; mov al, [esi]
         (s20 (list #x88 #x07))                              ; mov [edi], al
         (s21 (list #x46))                                   ; inc esi
         (s22 (list #x47))                                   ; inc edi
         (s23 (list #x84 #xC0))                              ; test al, al
         (sz-s24 2)                                          ; jnz STR (rel8)
         (c25 (list #x83 #xC3 #x04))                         ; add ebx, 4
         (sz-c26 5)                                          ; jmp LOOP (rel32)
         ;; --- measured spans ---
         (str-body (+ (length s19) (length s20) (length s21)
                      (length s22) (length s23) sz-s24))
         (null-tail (+ (length a6) (length a7) (length a8)
                       (length a9) (length a10) sz-a11 sz-a12))
         (copy-len (+ (length c13) sz-c14 (length c15) (length c16)
                      (length c17) (length c18) str-body (length c25) sz-c26))
         (loop-len (+ (length a1) sz-a2 (length a3) (length a4) sz-a5 null-tail))
         ;; The `jl LOOP' sits BEFORE the trailing `jmp DONE', so its backward
         ;; span must exclude that jmp.  (Caught by disassembling the emitted
         ;; stub: the jl landed 5 bytes early, mid-instruction, and the boot
         ;; SIGSEGV'd before printing anything.  Every other displacement here
         ;; verified correct against the same disassembly.)
         (back-to-loop (- loop-len sz-a12)))
    (flet ((emit (bytes) (dolist (b bytes) (mvm-emit-byte buf b))))
      (emit pro)
      (emit a1)
      ;; jae DONE — past the rest of the loop head and the whole copy block
      (emit (append (list #x0F #x83)
                    (i386l-le32-list (+ (length a3) (length a4) sz-a5
                                        null-tail copy-len))))
      (emit a3) (emit a4)
      (emit (append (list #x75) (i386l-rel8 null-tail)))     ; jnz COPY
      (emit a6) (emit a7) (emit a8) (emit a9) (emit a10)
      (emit (append (list #x7C) (i386l-rel8 (- back-to-loop))))  ; jl LOOP
      (emit (append (list #xE9) (i386l-le32-list copy-len))) ; jmp DONE
      (emit c13)
      (emit (append (list #x73)                              ; jae DONE
                    (i386l-rel8 (+ (length c15) (length c16) (length c17)
                                   (length c18) str-body (length c25) sz-c26))))
      (emit c16) (emit c17) (emit c18)
      (emit s19) (emit s20) (emit s21) (emit s22) (emit s23)
      (emit (append (list #x75) (i386l-rel8 (- str-body))))  ; jnz STR
      (emit c25)
      (emit (append (list #xE9)                              ; jmp LOOP
                    (i386l-le32-list (- (+ loop-len copy-len))))))))
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

  ;; --- Stage the FULL argv + envp into the BSS ---------------------------
  ;; ESP is the initial process stack again here, which is what this needs.
  ;; The argv[1]/argv[2] copies above stay: probe-argv and the older CLI
  ;; probes read those two fixed slots directly, and they cost 128 bytes.
  (emit-i386-stage-argv buf)

  ;; [0x10000290] = the INITIAL process ESP, saved RAW, before the relocation
  ;; below replaces it.  lib/cli-toplevel.lisp walks the live initial stack to
  ;; read the FULL argv and envp (not just the argv[1]/argv[2] copies above),
  ;; and derives its base from the initial stack pointer.  On x64 and aarch64
  ;; that pointer IS %gc-stack-base, because those ports keep the kernel's
  ;; stack.  i386 relocates (the kernel's stack sits near 0x40800000, above the
  ;; 2^30 fixnum ceiling), so stack_base is our NEW stack and would point at a
  ;; page that never held argv.  Hence a slot of its own.  The pages of the
  ;; original stack stay mapped for the life of the process, so walking them
  ;; later is safe.
  (i386l-mov-abs-reg buf #x10000290 4)          ; mov [0x10000290], esp

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
  ;; --- Cheney GC metadata, stored RAW ------------------------------------
  ;; RAW byte addresses, matching x64 and aarch64.  The consumer is the NATIVE
  ;; collector (i386-emit-gc-trampoline in translate-i386.lisp), which holds
  ;; these in registers.  They used to be stored address<<1 for mvm/gc.lisp,
  ;; whose (mem-ref :u64) is RAW and therefore reinterprets any loaded word as
  ;; a TAGGED Lisp value — the same halving that makes gc.lisp unable to
  ;; recognise a heap pointer at all.  Native code has no such convention.
  ;; [0x10000040] from_start = heap + alloc_start
  (i386l-bytes buf #x89 #xC1)
  (i386l-bytes buf #x81 #xC1) (i386l-le32 buf +linux-i386-heap-alloc-start+)
  (i386l-mov-abs-reg buf +gc-from-start-addr+ 1)
  ;; [0x10000048] to_start = heap + midpoint
  (i386l-bytes buf #x89 #xC1)
  (i386l-bytes buf #x81 #xC1) (i386l-le32 buf +linux-i386-gc-midpoint+)
  (i386l-mov-abs-reg buf +gc-to-start-addr+ 1)
  ;; [0x10000050] space_size
  (i386l-mov-abs-imm buf +gc-space-size-addr+
                     (- +linux-i386-gc-midpoint+ +linux-i386-heap-alloc-start+))
  ;; [0x10000058] stack_base = our RELOCATED stack top
  (i386l-mov-reg-imm buf 1 (- (+ +linux-i386-stack-addr+ +linux-i386-stack-size+) 16))
  (i386l-mov-abs-reg buf +gc-stack-base-addr+ 1)
  ;; [0x10000060] gc_count = 0
  (i386l-mov-abs-imm buf +gc-count-addr+ 0)

  ;; --- Object-start + cons-kind bitmaps ---------------------------------
  ;; gc.lisp's %gc-bitmap-init cannot be used on i386: it allocates via
  ;; %mmap-exec-page (trap #x0531), which is unimplemented here and would hit
  ;; the loud-trap reporter.  So the boot stub reserves them directly.
  ;; page_base = from_start (the lowest object address, before any collection).
  ;; Each config word at 0x10000E.. is stored <<1 because gc.lisp reads them
  ;; with (mem-ref :u64), which is RAW; the RAW copies in the i386 global slot
  ;; block are what the TRANSLATOR's inline bit-set uses.
  ;; EAX still holds the heap base here; the two mmaps below clobber it, so
  ;; every EAX-dependent store above must already have happened.
  (i386l-bytes buf #x89 #xC1)                         ; mov ecx, eax
  (i386l-bytes buf #x81 #xC1) (i386l-le32 buf +linux-i386-heap-alloc-start+)
  (i386l-mov-abs-reg buf (+ +linux-i386-globals+ #x18) 1)   ; raw page_base
  (i386l-mov-abs-reg buf #x10000E00 1)                ; raw copy, diagnostics only

  ;; object-start bitmap
  (i386l-mov-reg-imm buf 3 0)                         ; addr = NULL (any)
  (i386l-mov-reg-imm buf 1 +linux-i386-bitmap-size+)
  (i386l-mov-reg-imm buf 2 3)                         ; PROT_READ|WRITE
  (i386l-mov-reg-imm buf 6 #x22)                      ; PRIVATE|ANON
  (i386l-mov-reg-imm buf 7 #xFFFFFFFF)
  (i386l-bytes buf #x55) (i386l-bytes buf #x31 #xED)
  (i386l-mov-reg-imm buf 0 192) (i386l-bytes buf #xCD #x80)
  (i386l-bytes buf #x5D)
  (i386l-mov-abs-reg buf (+ +linux-i386-globals+ #x1C) 0)   ; raw, for codegen
  (i386l-mov-abs-reg buf #x10000E18 0)                ; raw copy, diagnostics only

  ;; cons-kind bitmap
  (i386l-mov-reg-imm buf 3 0)
  (i386l-mov-reg-imm buf 1 +linux-i386-bitmap-size+)
  (i386l-mov-reg-imm buf 2 3)
  (i386l-mov-reg-imm buf 6 #x22)
  (i386l-mov-reg-imm buf 7 #xFFFFFFFF)
  (i386l-bytes buf #x55) (i386l-bytes buf #x31 #xED)
  (i386l-mov-reg-imm buf 0 192) (i386l-bytes buf #xCD #x80)
  (i386l-bytes buf #x5D)
  (i386l-mov-abs-reg buf (+ +linux-i386-globals+ #x20) 0)   ; raw, for codegen
  (i386l-mov-abs-reg buf #x10000E40 0)                ; raw copy, diagnostics only

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
