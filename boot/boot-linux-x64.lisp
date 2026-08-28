;;;; boot-linux-x64.lisp - Linux x86-64 ELF entry for Modus
;;;;
;;;; Produces a Linux userspace executable instead of a bare-metal kernel.
;;;; Entry point receives argc/argv on the stack from the Linux loader.
;;;; Uses Linux syscalls for I/O instead of hardware port I/O.

(in-package :modus.mvm)

;;; ============================================================
;;; Linux x64 Constants
;;; ============================================================

(defconstant +linux-x64-load-addr+ #x400000)    ; Traditional Linux x64 load address
(defconstant +linux-x64-heap-addr+ #x10000000)  ; Heap start (same as bare-metal)
;; The two Cheney semispaces are [0x200, midpoint) and [midpoint, midpoint+space_size).
;; The SECOND semispace's from_end = midpoint + space_size ≈ 2*midpoint ≈ 0x37FFFE00.
;; The mmap'd region MUST extend past that by a guard band, because the gc-check
;; is POST-allocation: R12 (alloc ptr) overshoots R14 (= from_end) by one or more
;; objects before the next check fires the collector (~33 KB observed for the
;; interp's make-string loop).  In the FIRST space that overshoot lands in the
;; (mapped) second space and is harmless; in the SECOND space, without a guard it
;; runs off the end of the mapping and SIGSEGVs mid-write — the deterministic
;; "interp value lost across the 2nd GC" fault was make-string writing at
;; heap_end+0 on the overshoot of the second collection.  16 MB of guard past the
;; second from_end is far more than any plausible inter-check overshoot.
(defconstant +linux-x64-gc-guard+ #x1000000)    ; 16MB guard past the 2nd semispace
(defconstant +linux-x64-gc-midpoint+ #x38000000)  ; 896MB semispace (2x orig): self-compile ~580MB total alloc fits WITHOUT a collection (avoids the large-working-set-collection corruption). Total mmap 2x896MB+guard ~1.8GB < 2GB imm32 cap.
(defconstant +linux-x64-heap-data-size+ (+ #x70000000 +linux-x64-gc-guard+))  ; 1792MB + 16MB guard
;; #x400 AND NOT #x200, WHICH IS A GC CONCURRENCY FIX, NOT A COSMETIC ONE.
;; page_base is heap_base + this offset, from_start is the same address, and
;; space_size is (midpoint - this offset).  The object-start and cons-kind
;; bitmaps are ONE pair for the whole heap and translate-x64 sets bits with an
;; unLOCKed `BTS [base], idx' at 64-bit operand size, whose read-modify-write
;; unit is eight bitmap bytes = 1024 HEAP BYTES.  So every region boundary must
;; be a multiple of 1024 FROM page_base (mvm/gc.lisp %GC-REGION-ALIGN-CHECK).
;; At #x200 the midpoint is 1024-aligned but space_size was 512 mod 1024, so
;; region 0's to-space start and size both violated the rule; at #x400 both are
;; multiples of 1024 and region 0 satisfies it like every carved region.
;; The 512 extra bytes are padding nobody allocates in.
(defconstant +linux-x64-heap-alloc-start+ #x400)  ; Offset from heap base to first allocatable byte

;;; ------------------------------------------------------------
;;; Mostly-Copying (Bartlett) GC metadata region (MCGC).
;;;
;;; A 16 MiB metadata region is APPENDED to the heap mmap, immediately
;;; after the existing data region.  Layout within the region (offsets
;;; from MCGC-META-OFFSET, all relative to the mmap base):
;;;   descriptor array : 1 byte / 4 KiB page  (state 0=free/1=live/2=pinned)
;;;   object-start bmap : 1 bit / 16-byte granule over the data region
;;;   free-list stack   : u32 page-index per slot
;;; The kernel relies on MAP_ANON zero-fill for the descriptor/bitmap/
;;; free-list arrays — the boot stub only stores the small CONFIG WORDS
;;; (raw addresses + sizes) into the BSS so the trampoline/allocator can
;;; find the arrays.  Through MCGC stages 1-2 the old Cheney collector
;;; still runs UNCHANGED over the two semispaces; the metadata is built
;;; but unused (stage 1) / write-only (stage 2).
;;; ------------------------------------------------------------
(defconstant +mcgc-page-size+ #x1000)           ; 4 KiB pages
(defconstant +mcgc-page-count+ (truncate +linux-x64-heap-data-size+ +mcgc-page-size+)) ; pages over the data region
(defconstant +mcgc-descriptor-size+ +mcgc-page-count+)  ; 1 byte/page
;; Object-start bitmap: 1 bit / 16-byte granule = data_size/16/8 bytes.
(defconstant +mcgc-bitmap-size+ (truncate +linux-x64-heap-data-size+ (* 16 8)))
(defconstant +mcgc-freelist-size+ (* +mcgc-page-count+ 4))  ; u32 per page index
(defconstant +mcgc-meta-size+ #x4000000)        ; 64 MiB metadata region (enlarged for 2x heap: 2x~14.5MB bitmaps + descriptor + freelist ~32MB; was 24 MiB;
                                                ; holds descriptor + object-start
                                                ; bitmap + freelist + cons-kind bitmap)
;; Offsets (from mmap base) of the metadata arrays, each 64-byte aligned.
(defconstant +mcgc-meta-offset+ +linux-x64-heap-data-size+) ; metadata starts right after data
(defconstant +mcgc-descriptor-offset+ +mcgc-meta-offset+)
(defconstant +mcgc-bitmap-offset+
  (logand (+ +mcgc-descriptor-offset+ +mcgc-descriptor-size+ 63) (lognot 63)))
(defconstant +mcgc-freelist-offset+
  (logand (+ +mcgc-bitmap-offset+ +mcgc-bitmap-size+ 63) (lognot 63)))
;; Cons-kind bitmap: a SECOND object-start-sized bitmap (1 bit / 16-byte
;; granule) marking which starts are CONSES (vs objects).  scan_word
;; cross-checks a candidate's TAG against it so a conservative scratch word
;; aliasing a live object's BASE with the wrong tag can't be copied as the
;; wrong type.  Its runtime base is derived as [+mcgc-cfg-bitmap-addr+] +
;; modus.mvm.x64::+mcgc-kindbitmap-delta+ (no extra boot config word, so no
;; boot-preamble growth / *x64-native-code-offset* bump).  The two asserts
;; below pin that delta to the real layout.
(defconstant +mcgc-kindbitmap-offset+
  (logand (+ +mcgc-freelist-offset+ +mcgc-freelist-size+ 63) (lognot 63)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (= (- +mcgc-kindbitmap-offset+ +mcgc-bitmap-offset+)
             modus.mvm.x64::+mcgc-kindbitmap-delta+)
          () "cons-kind bitmap delta ~X != translate-x64 +mcgc-kindbitmap-delta+ ~X"
          (- +mcgc-kindbitmap-offset+ +mcgc-bitmap-offset+)
          modus.mvm.x64::+mcgc-kindbitmap-delta+)
  (assert (<= (+ +mcgc-kindbitmap-offset+ +mcgc-bitmap-size+)
              (+ +mcgc-meta-offset+ +mcgc-meta-size+))
          () "cons-kind bitmap overruns the metadata region"))
;; Full mmap = data region + metadata region.
(defconstant +linux-x64-heap-size+ (+ +linux-x64-heap-data-size+ +mcgc-meta-size+))

;;; MCGC config-word slots in the fixed BSS block (zero-init by the ELF
;;; loader; the boot stub overwrites with mmap-relative raw addresses).
;;; Placed at 0x10000E00.. — a verified-free 256-byte BSS gap above the
;;; signal-handler scratch (0x10000C30-0xC80) and below 0x10000FF0.
(defconstant +mcgc-cfg-page-base+      #x10000E00)  ; raw addr, first data byte
(defconstant +mcgc-cfg-page-count+     #x10000E08)  ; number of pages
(defconstant +mcgc-cfg-descriptor+     #x10000E10)  ; raw addr of descriptor array
(defconstant +mcgc-cfg-bitmap+         #x10000E18)  ; raw addr of object-start bitmap
(defconstant +mcgc-cfg-freelist+       #x10000E20)  ; raw addr of free-list stack
(defconstant +mcgc-cfg-freelist-count+ #x10000E28)  ; current free page count
(defconstant +mcgc-cfg-alloc-page+     #x10000E30)  ; current alloc page index
(defconstant +mcgc-cfg-data-end+       #x10000E38)  ; raw addr one past data region
;; THIS LIST IS NOT THE WHOLE BLOCK.  mvm/translate-x64.lisp declares eleven
;; MORE config words at 0x10000E40..0x10000EA8 (the stage-4 page-collector
;; scratch) which boot does not initialise and therefore does not name here.
;; Reading only this list is how WS5 #223 first put the JIT constant-vector root
;; at 0x10000E40 — on top of +mcgc-cfg-run-start-addr+.  The assert below keeps
;; that root clear of the entire MCGC block; grep `#x10000[EF]..` repo-wide
;; before claiming any word in this range.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (or (< modus.mvm.x64::*x64-jit-constvec-root* #x10000E00)
              (> modus.mvm.x64::*x64-jit-constvec-root* #x10000EA8))
          () "WS5 #223: *x64-jit-constvec-root* ~X collides with the MCGC config block 0x10000E00..0x10000EA8"
          modus.mvm.x64::*x64-jit-constvec-root*)
  (assert (< modus.mvm.x64::*x64-jit-constvec-root* #x10000FF0)
          () "WS5 #223: *x64-jit-constvec-root* ~X is past the end of the fixed BSS block"
          modus.mvm.x64::*x64-jit-constvec-root*))

;; When GC is enabled, R14 = midpoint (GC fires at half heap).
;; When GC is disabled, R14 = full heap size (no GC trigger).
(defvar *linux-x64-r14-offset* +linux-x64-heap-size+
  "Offset from heap base for R14 (alloc limit). Set to midpoint for GC.")
;; Globals at start of heap region (after mmap succeeds)
;; +0x00: argc, +0x08: argv base, +0x10: argv[0], +0x18: argv[1], etc.
(defconstant +linux-x64-globals+  #x10000000)

;;; ============================================================
;;; ELF64 Little-Endian Wrapper
;;; ============================================================

(defun %sanitize-symbol-name (name)
  "ELF strtab can't have NULs (used as terminator).  Replace any NULs in
   NAME with '_' and limit length to a reasonable upper bound."
  (let ((s (string name)))
    (with-output-to-string (out)
      (loop for c across s
            for i below (min (length s) 200)
            do (write-char (if (char= c #\Null) #\_ c) out)))))

(defun wrap-in-elf64-le (raw-bytes load-addr &key function-table native-image-offset
                                                  native-code-length)
  "Wrap raw image bytes in a Linux ELF64 little-endian executable.
   Emits section headers (.text + .shstrtab + .symtab + .strtab) plus
   one symbol per FUNCTION-TABLE entry, so objdump -d shows function
   names and addr2line works.  Without section headers, objdump
   returns empty and disassembly tools refuse to operate.

   NATIVE-IMAGE-OFFSET is the byte offset of the native-code area within
   raw-bytes (= len(boot-code) + len(JMP-stub)).  Symbol addresses are
   computed as: load-addr + ehdr+phdr + native-image-offset + native-offset.
   Since p_offset=0 and p_vaddr=load-addr, file offset == virtual addr -
   load-addr, so this lands symbols on real prologue bytes.

   NATIVE-CODE-LENGTH is the size of the native-code area; used to compute
   each symbol's st_size (distance to the next function, or to native-end
   for the last)."
  (let* ((ehdr-size 64)
         (phdr-size 56)
         (shdr-size 64)
         (sym-size  24)
         (header-total (+ ehdr-size phdr-size))
         (raw-len (length raw-bytes))
         (nio (or native-image-offset 0))
         (ncl (or native-code-length 0))
         ;; Sort function-table by native-offset so each symbol's size can
         ;; be computed as (next.offset - this.offset).  Stable sort to
         ;; preserve source order on ties.
         (sorted-fns (stable-sort (copy-list function-table)
                                  #'< :key #'mvm-function-info-native-offset))
         (fn-sizes (let ((arr (make-array (length sorted-fns) :initial-element 0)))
                     (loop for i from 0 below (length sorted-fns)
                           for fi in sorted-fns
                           for next-off = (if (< (1+ i) (length sorted-fns))
                                              (mvm-function-info-native-offset
                                                (nth (1+ i) sorted-fns))
                                              ncl)
                           do (setf (aref arr i)
                                    (max 0 (- next-off
                                              (mvm-function-info-native-offset fi)))))
                     arr))
         ;; .shstrtab: section names
         (shstrtab (concatenate 'string
                                (string #\Null)
                                ".text" (string #\Null)
                                ".shstrtab" (string #\Null)
                                ".symtab" (string #\Null)
                                ".strtab" (string #\Null)))
         (shstrtab-bytes (map 'vector #'char-code shstrtab))
         (shstrtab-len (length shstrtab-bytes))
         ;; .strtab: symbol names.  First byte must be NUL.
         ;; Use SORTED-FNS so name order matches the symbol-table order
         ;; (otherwise st_name offsets land on the wrong names).
         (sym-names (cons "" (mapcar (lambda (fi)
                                       (%sanitize-symbol-name
                                         (mvm-function-info-name fi)))
                                     sorted-fns)))
         (sym-name-offsets (let ((acc 0) (offs nil))
                             (dolist (n sym-names (nreverse offs))
                               (push acc offs)
                               (incf acc (1+ (length n))))))
         (strtab-bytes (with-output-to-string (out)
                         (dolist (n sym-names)
                           (write-string n out)
                           (write-char #\Null out))))
         (strtab-byte-vec (map 'vector #'char-code strtab-bytes))
         (strtab-len (length strtab-byte-vec))
         ;; Symbol entries: one NULL + one per function
         (n-syms (1+ (length function-table)))
         (symtab-len (* n-syms sym-size))
         ;; Layout:
         ;;   ehdr | phdr | raw-bytes | shstrtab | symtab | strtab | shdrs(5*64)
         (shstrtab-offset (+ header-total raw-len))
         (symtab-offset (+ shstrtab-offset shstrtab-len))
         (strtab-offset (+ symtab-offset symtab-len))
         (shdrs-offset (+ strtab-offset strtab-len))
         (entry-point (+ load-addr header-total))
         (buf (make-mvm-buffer)))
    ;; ---- ELF Header (64 bytes) ----
    (mvm-emit-byte buf #x7F)
    (mvm-emit-byte buf (char-code #\E))
    (mvm-emit-byte buf (char-code #\L))
    (mvm-emit-byte buf (char-code #\F))
    (mvm-emit-byte buf 2)              ; ELFCLASS64
    (mvm-emit-byte buf 1)              ; ELFDATA2LSB
    (mvm-emit-byte buf 1)              ; EV_CURRENT
    (mvm-emit-byte buf 0)              ; ELFOSABI_NONE
    (dotimes (i 8) (mvm-emit-byte buf 0))
    (mvm-emit-u16 buf 2)              ; ET_EXEC
    (mvm-emit-u16 buf 62)             ; EM_X86_64
    (mvm-emit-u32 buf 1)              ; e_version
    (mvm-emit-u64 buf entry-point)    ; e_entry
    (mvm-emit-u64 buf ehdr-size)      ; e_phoff
    (mvm-emit-u64 buf shdrs-offset)   ; e_shoff
    (mvm-emit-u32 buf 0)              ; e_flags
    (mvm-emit-u16 buf ehdr-size)      ; e_ehsize
    (mvm-emit-u16 buf phdr-size)      ; e_phentsize
    (mvm-emit-u16 buf 1)              ; e_phnum
    (mvm-emit-u16 buf shdr-size)      ; e_shentsize
    (mvm-emit-u16 buf 5)              ; e_shnum (NULL + .text + .shstrtab + .symtab + .strtab)
    (mvm-emit-u16 buf 2)              ; e_shstrndx (.shstrtab at index 2)
    ;; ---- Program Header (56 bytes) ----
    (mvm-emit-u32 buf 1)              ; PT_LOAD
    (mvm-emit-u32 buf 7)              ; PF_R|PF_W|PF_X
    (mvm-emit-u64 buf 0)              ; p_offset
    (mvm-emit-u64 buf load-addr)      ; p_vaddr
    (mvm-emit-u64 buf load-addr)      ; p_paddr
    ;; p_filesz: only the LOAD segment; shstrtab/symtab/strtab/shdrs are not loaded
    (mvm-emit-u64 buf (+ header-total raw-len))
    (mvm-emit-u64 buf (+ header-total raw-len +linux-x64-heap-size+)) ; p_memsz
    (mvm-emit-u64 buf #x200000)       ; p_align
    ;; ---- Raw image data ----
    (loop for b across raw-bytes do (mvm-emit-byte buf b))
    ;; ---- .shstrtab ----
    (loop for b across shstrtab-bytes do (mvm-emit-byte buf b))
    ;; ---- .symtab — one ELF64 Sym entry per function ----
    ;; Sym layout: u32 name + u8 info + u8 other + u16 shndx + u64 value + u64 size
    ;; First: NULL symbol (all zeros, except maybe).
    (dotimes (i sym-size) (mvm-emit-byte buf 0))
    ;; Then one per function-table entry, in NATIVE-OFFSET order so addresses
    ;; are monotonic and st_size is the gap to the next function.
    (loop for fi in sorted-fns
          for name-offset in (cdr sym-name-offsets)
          for fn-size across fn-sizes
          do (let* ((nat-off  (or (mvm-function-info-native-offset fi) 0))
                    (sym-addr (+ load-addr header-total nio nat-off)))
               (mvm-emit-u32 buf name-offset)            ; st_name (offset in .strtab)
               (mvm-emit-byte buf #x12)                   ; st_info: STB_GLOBAL(1) | STT_FUNC(2)
               (mvm-emit-byte buf 0)                      ; st_other
               (mvm-emit-u16 buf 1)                       ; st_shndx (point at .text section)
               (mvm-emit-u64 buf sym-addr)                ; st_value
               (mvm-emit-u64 buf fn-size)))               ; st_size
    ;; ---- .strtab — symbol names ----
    (loop for b across strtab-byte-vec do (mvm-emit-byte buf b))
    ;; ---- Section headers (5 × 64 bytes) ----
    ;; [0] NULL
    (dotimes (i shdr-size) (mvm-emit-byte buf 0))
    ;; [1] .text — covers full LOAD region
    (mvm-emit-u32 buf 1)              ; sh_name (".text" offset in shstrtab)
    (mvm-emit-u32 buf 1)              ; sh_type = SHT_PROGBITS
    (mvm-emit-u64 buf 6)              ; sh_flags = SHF_ALLOC|SHF_EXECINSTR
    (mvm-emit-u64 buf load-addr)      ; sh_addr
    (mvm-emit-u64 buf 0)              ; sh_offset (file offset)
    (mvm-emit-u64 buf (+ header-total raw-len)) ; sh_size
    (mvm-emit-u32 buf 0)              ; sh_link
    (mvm-emit-u32 buf 0)              ; sh_info
    (mvm-emit-u64 buf 16)             ; sh_addralign
    (mvm-emit-u64 buf 0)              ; sh_entsize
    ;; [2] .shstrtab
    (mvm-emit-u32 buf 7)              ; sh_name (".shstrtab" offset)
    (mvm-emit-u32 buf 3)              ; sh_type = SHT_STRTAB
    (mvm-emit-u64 buf 0)              ; sh_flags
    (mvm-emit-u64 buf 0)              ; sh_addr
    (mvm-emit-u64 buf shstrtab-offset); sh_offset
    (mvm-emit-u64 buf shstrtab-len)   ; sh_size
    (mvm-emit-u32 buf 0)              ; sh_link
    (mvm-emit-u32 buf 0)              ; sh_info
    (mvm-emit-u64 buf 1)              ; sh_addralign
    (mvm-emit-u64 buf 0)              ; sh_entsize
    ;; [3] .symtab
    (mvm-emit-u32 buf 17)             ; sh_name (".symtab" offset = 1+5+1+9+1 = 17)
    (mvm-emit-u32 buf 2)              ; sh_type = SHT_SYMTAB
    (mvm-emit-u64 buf 0)              ; sh_flags
    (mvm-emit-u64 buf 0)              ; sh_addr
    (mvm-emit-u64 buf symtab-offset)  ; sh_offset
    (mvm-emit-u64 buf symtab-len)     ; sh_size
    (mvm-emit-u32 buf 4)              ; sh_link = .strtab section index
    (mvm-emit-u32 buf 1)              ; sh_info = index of first non-local symbol (1 past NULL)
    (mvm-emit-u64 buf 8)              ; sh_addralign
    (mvm-emit-u64 buf sym-size)       ; sh_entsize
    ;; [4] .strtab
    (mvm-emit-u32 buf 25)             ; sh_name (".strtab" offset = 17+8 = 25)
    (mvm-emit-u32 buf 3)              ; sh_type = SHT_STRTAB
    (mvm-emit-u64 buf 0)              ; sh_flags
    (mvm-emit-u64 buf 0)              ; sh_addr
    (mvm-emit-u64 buf strtab-offset)  ; sh_offset
    (mvm-emit-u64 buf strtab-len)     ; sh_size
    (mvm-emit-u32 buf 0)              ; sh_link
    (mvm-emit-u32 buf 0)              ; sh_info
    (mvm-emit-u64 buf 1)              ; sh_addralign
    (mvm-emit-u64 buf 0)              ; sh_entsize
    (mvm-buffer-used-bytes buf)))

;;; ============================================================
;;; Linux x64 Entry Point
;;; ============================================================

(defun emit-linux-x64-entry (buf)
  "Emit the Linux x64 entry stub.
   On entry: RSP → [argc, argv[0], argv[1], ..., NULL, envp...]
   Strategy: save argc/argv to callee-saved regs, mmap heap, then
   copy argc/argv to globals area in the mmap'd heap."
  ;; Save argc and argv[0..3] in callee-saved registers (survive mmap syscall)
  ;; RBX = argc, R13 = argv[0], R14 = argv[1], R15 = argv[2]
  ;; (R14/R15 will be overwritten later for alloc ptr/NIL)
  (emit-bytes buf #x48 #x8B #x1C #x24)          ; mov rbx, [rsp]    (argc)
  (emit-bytes buf #x4C #x8B #x6C #x24 #x08)     ; mov r13, [rsp+8]  (argv[0])
  (emit-bytes buf #x4C #x8B #x74 #x24 #x10)     ; mov r14, [rsp+16] (argv[1])
  (emit-bytes buf #x4C #x8B #x7C #x24 #x18)     ; mov r15, [rsp+24] (argv[2])

  ;; Store argc and COPIES of argv[1], argv[2] strings to fixed BSS addresses
  ;; before mmap. We copy content (not pointers) so Lisp can read strings
  ;; without dealing with raw pointer-as-fixnum arithmetic. Each buffer is
  ;; 64 bytes, null-terminated.
  ;;   [0x10000200]: argc (u32)
  ;;   [0x10000208]: argv[1] string (64 bytes)
  ;;   [0x10000248]: argv[2] string (64 bytes)

  ;; [0x10000200] = argc (32-bit store; Lisp reads via mem-ref :u32 which tags)
  (emit-bytes buf #x89 #x1C #x25)                ; mov [abs32], ebx (argc low32)
  (emit-le32 buf #x10000200)

  ;; Zero-fill 128 bytes at 0x10000208 (both string buffers).
  ;; mov rdi, 0x10000208; mov ecx, 16; xor rax, rax; rep stosq
  (emit-bytes buf #x48 #xBF)                     ; movabs rdi, imm64
  (emit-le32 buf #x10000208) (emit-le32 buf 0)
  (emit-bytes buf #xB9 #x10 #x00 #x00 #x00)      ; mov ecx, 16 (8-byte words)
  (emit-bytes buf #x48 #x31 #xC0)                ; xor rax, rax
  (emit-bytes buf #xF3 #x48 #xAB)                ; rep stosq

  ;; Copy argv[1] → 0x10000208 if argc > 1. Skip block is 20 bytes.
  (emit-bytes buf #x48 #x83 #xFB #x01)           ; cmp rbx, 1
  (emit-bytes buf #x7E #x14)                     ; jle +20 (skip copy block)
  (emit-bytes buf #x4C #x89 #xF6)                ; mov rsi, r14 (argv[1] ptr)
  (emit-bytes buf #x48 #xBF)                     ; movabs rdi, 0x10000208
  (emit-le32 buf #x10000208) (emit-le32 buf 0)
  (emit-bytes buf #xB9 #x3F #x00 #x00 #x00)      ; mov ecx, 63 (max)
  (emit-bytes buf #xF3 #xA4)                     ; rep movsb  — 20 bytes

  ;; Copy argv[2] → 0x10000248 if argc > 2. Same structure.
  (emit-bytes buf #x48 #x83 #xFB #x02)           ; cmp rbx, 2
  (emit-bytes buf #x7E #x14)                     ; jle +20
  (emit-bytes buf #x4C #x89 #xFE)                ; mov rsi, r15 (argv[2] ptr)
  (emit-bytes buf #x48 #xBF)                     ; movabs rdi, 0x10000248
  (emit-le32 buf #x10000248) (emit-le32 buf 0)
  (emit-bytes buf #xB9 #x3F #x00 #x00 #x00)      ; mov ecx, 63
  (emit-bytes buf #xF3 #xA4)                     ; rep movsb  — 20 bytes

  ;; mmap heap: rax=9, rdi=hint, rsi=size, rdx=prot, r10=flags, r8=fd, r9=off
  (emit-bytes buf #x48 #xC7 #xC7 #x00 #x00 #x00 #x10) ; mov rdi, 0x10000000
  (emit-bytes buf #x48 #xC7 #xC6)                ; mov rsi, imm32
  (emit-le32 buf +linux-x64-heap-size+)
  (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, 3 (PROT_RW)
  (emit-bytes buf #x49 #xC7 #xC2 #x22 #x00 #x00 #x00) ; mov r10, 0x22 (MAP_PRIV|MAP_ANON)
  (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
  (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
  (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, 9 (SYS_mmap)
  (emit-bytes buf #x0F #x05)                      ; syscall

  ;; Store argc/argv to globals area at heap base (mmap result in RAX)
  ;; argc at [rax+0], argv[0] at [rax+0x10], argv[1] at [rax+0x18], argv[2] at [rax+0x20]
  (emit-bytes buf #x48 #x89 #x18)                ; mov [rax], rbx     (argc)
  (emit-bytes buf #x4C #x89 #x68 #x10)           ; mov [rax+0x10], r13 (argv[0])
  (emit-bytes buf #x4C #x89 #x70 #x18)           ; mov [rax+0x18], r14 (argv[1])
  (emit-bytes buf #x4C #x89 #x78 #x20)           ; mov [rax+0x20], r15 (argv[2])

  ;; Set up MVM runtime registers
  ;; R12 = alloc pointer (skip first 512 bytes used for globals + MV storage)
  ;; Layout: 0x00-0x3F argc/argv, 0x40-0x7F GC metadata,
  ;; 0x80 globals, 0x88 symtab,
  ;; 0x90 MV-count, 0x98-0x138 MV-values (20 slots), 0x140-0x1FF reserved
  ;; RAX = mmap result = heap base
  (emit-bytes buf #x49 #x89 #xC4)                ; mov r12, rax
  (emit-bytes buf #x49 #x81 #xC4)                ; add r12, imm32 (alloc start offset)
  (emit-le32 buf +linux-x64-heap-alloc-start+)
  ;; R14 = alloc limit
  ;; With GC: midpoint (GC fires when from-space fills)
  ;; Without GC: full heap end (no GC trigger)
  (emit-bytes buf #x49 #x89 #xC6)                ; mov r14, rax
  (emit-bytes buf #x49 #x81 #xC6)                ; add r14, imm32
  (emit-le32 buf *linux-x64-r14-offset*)

  ;; Initialize GC metadata at ABSOLUTE addresses 0x10000040..0x10000060
  ;; These are in the ELF BSS region (zero-initialized by the kernel loader).
  ;; The mmap'd heap is at a DIFFERENT address (RAX), so we store
  ;; mmap-relative addresses here for the GC to use.
  ;; All values are raw byte addresses.

  ;; [0x10000040] = from_start = mmap_base + alloc_offset
  (emit-bytes buf #x48 #x89 #xC1)                ; mov rcx, rax     (mmap base)
  (emit-bytes buf #x48 #x81 #xC1)                ; add rcx, imm32
  (emit-le32 buf +linux-x64-heap-alloc-start+)
  (emit-bytes buf #x48 #x89 #x0C #x25)           ; mov [abs32], rcx
  (emit-le32 buf +gc-from-start-addr+)

  ;; [0x10000048] = to_start = mmap_base + midpoint
  (emit-bytes buf #x48 #x89 #xC1)                ; mov rcx, rax
  (emit-bytes buf #x48 #x81 #xC1)                ; add rcx, imm32
  (emit-le32 buf +linux-x64-gc-midpoint+)
  (emit-bytes buf #x48 #x89 #x0C #x25)           ; mov [abs32], rcx
  (emit-le32 buf +gc-to-start-addr+)

  ;; [0x10000050] = space_size = midpoint - alloc_offset
  (emit-bytes buf #x48 #xC7 #xC1)                ; mov rcx, imm32
  (emit-le32 buf (- +linux-x64-gc-midpoint+ +linux-x64-heap-alloc-start+))
  (emit-bytes buf #x48 #x89 #x0C #x25)           ; mov [abs32], rcx
  (emit-le32 buf +gc-space-size-addr+)

  ;; [0x10000058] = stack_base = initial RSP
  (emit-bytes buf #x48 #x89 #xE1)                ; mov rcx, rsp
  (emit-bytes buf #x48 #x89 #x0C #x25)           ; mov [abs32], rcx
  (emit-le32 buf +gc-stack-base-addr+)

  ;; [0x10000060] = gc_count = 0
  (emit-bytes buf #x48 #xC7 #x04 #x25)           ; mov qword [abs32], imm32
  (emit-le32 buf +gc-count-addr+)
  (emit-le32 buf 0)

  ;; ---- MCGC config words (mmap base still in RAX) ----
  ;; Store raw mmap-relative addresses + sizes for the mostly-copying
  ;; collector's metadata arrays.  Through stages 1-2 these are written
  ;; but the old Cheney GC ignores them.  The descriptor / bitmap /
  ;; free-list arrays are MAP_ANON zero-filled — no init needed here.
  (flet ((store-base+off (slot off)
           ;; mov rcx, rax ; add rcx, off ; mov [slot], rcx
           (emit-bytes buf #x48 #x89 #xC1)          ; mov rcx, rax
           (emit-bytes buf #x48 #x81 #xC1)          ; add rcx, imm32
           (emit-le32 buf off)
           (emit-bytes buf #x48 #x89 #x0C #x25)     ; mov [abs32], rcx
           (emit-le32 buf slot))
         (store-imm (slot val)
           ;; mov rcx, imm32(zero-ext via mov ecx) won't sign-ext for the
           ;; full 64; use movabs rcx, imm64 to be safe for large sizes.
           (emit-bytes buf #x48 #xB9)               ; movabs rcx, imm64
           (emit-le64 buf val)
           (emit-bytes buf #x48 #x89 #x0C #x25)     ; mov [abs32], rcx
           (emit-le32 buf slot)))
    ;; page_base = mmap_base + alloc_start (first allocatable data byte)
    (store-base+off +mcgc-cfg-page-base+ +linux-x64-heap-alloc-start+)
    ;; data_end = mmap_base + data_size
    (store-base+off +mcgc-cfg-data-end+  +linux-x64-heap-data-size+)
    ;; descriptor / bitmap / freelist array bases
    (store-base+off +mcgc-cfg-descriptor+ +mcgc-descriptor-offset+)
    (store-base+off +mcgc-cfg-bitmap+     +mcgc-bitmap-offset+)
    (store-base+off +mcgc-cfg-freelist+   +mcgc-freelist-offset+)
    ;; page_count, freelist_count (0 until stage-3 builds the list), alloc_page (0)
    (store-imm +mcgc-cfg-page-count+      +mcgc-page-count+)
    (store-imm +mcgc-cfg-freelist-count+  0)
    (store-imm +mcgc-cfg-alloc-page+      0))

  ;; R15 = NIL
  (emit-bytes buf #x49 #xBF)                      ; mov r15, imm64
  (emit-le64 buf #xDEAD0001)
  ;; Frame pointer
  (emit-bytes buf #x48 #x89 #xE5))               ; mov rbp, rsp

  ;; Fall through to JMP kernel-main

;;; ============================================================
;;; Helper: emit raw bytes
;;; ============================================================

(defun emit-bytes (buf &rest bytes)
  (dolist (b bytes) (mvm-emit-byte buf b)))

(defun emit-le32 (buf val)
  (mvm-emit-byte buf (logand val #xFF))
  (mvm-emit-byte buf (logand (ash val -8) #xFF))
  (mvm-emit-byte buf (logand (ash val -16) #xFF))
  (mvm-emit-byte buf (logand (ash val -24) #xFF)))

(defun emit-le64 (buf val)
  (emit-le32 buf (logand val #xFFFFFFFF))
  (emit-le32 buf (logand (ash val -32) #xFFFFFFFF)))

;;; ============================================================
;;; Boot Descriptor
;;; ============================================================

(defun linux-x64-boot-descriptor ()
  (list :arch :x86-64
        :entry-fn #'emit-linux-x64-entry
        :load-addr +linux-x64-load-addr+
        :elf-format :linux-x64))
