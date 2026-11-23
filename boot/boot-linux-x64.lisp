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
;;; SAVE-AND-DIE (lib/save-image.lisp, docs/save-and-die.md): the heap and the
;;; JIT exec arena are asked for at FIXED addresses with MAP_FIXED_NOREPLACE,
;;; so a snapshot restores with no relocation -- the same two mappings the
;;; hosted AArch64 CLI has (boot-linux-aarch64.lisp).  If the kernel refuses
;;; the heap address the stub falls back to the historical hint mmap and a
;;; later --core refuses with "heap base differs"; if it refuses the arena the
;;; bump word stays 0 and the %mmap-exec-page trap falls back to mmap(NULL).
;;; The bump word is 0x10000FB8: in the window span save-image stages (it is
;;; per-process and set explicitly on restore), claimed by nothing on x64
;;; (the aarch64 word, 0x10000F58, is inside x64's per-region cell table).
(defconstant +linux-x64-fixed-heap-base+ #x2000000000)
(defconstant +linux-x64-jit-arena-base+  #x3000000000)
(defconstant +linux-x64-jit-arena-size+  #x20000000)    ; 512 MB of VA, MAP_NORESERVE
(defconstant +linux-x64-jit-bump-slot+   #x10000FB8)
;; The two Cheney semispaces are [0x200, midpoint) and [midpoint, midpoint+space_size).
;; The SECOND semispace's from_end = midpoint + space_size ≈ 2*midpoint ≈ 0x37FFFE00.
;; The mmap'd region MUST extend past that by a guard band, because the gc-check
;; is POST-allocation: R12 (alloc ptr) overshoots R14 (= from_end) by one or more
;; objects before the next check fires the collector (~33 KB observed for the
;; interp's make-string loop).  In the FIRST space that overshoot lands in the
;; (mapped) second space -- which is NOT harmless, it is the TO-SPACE the next
;; collection copies survivors into, on top of the straddling object's tail
;; (see the LIMIT HEADROOM note in translate-x64's EMIT-GC-TRAMPOLINE; the limit
;; now stops short of each semispace's end).  In the SECOND space, without a guard it
;; runs off the end of the mapping and SIGSEGVs mid-write — the deterministic
;; "interp value lost across the 2nd GC" fault was make-string writing at
;; heap_end+0 on the overshoot of the second collection.  16 MB of guard past the
;; second from_end is far more than any plausible inter-check overshoot.
(defconstant +linux-x64-gc-guard+ #x1000000)    ; 16MB guard past the 2nd semispace
(defconstant +linux-x64-gc-midpoint+
  ;; MODUS_X64_MIDPOINT=<bytes> (A/B knob, host build time): a SMALL semispace
  ;; makes a hosted library load collect several times with a large live set,
  ;; the way the 104 MB bare-x64 heap does -- the shape the default below was
  ;; chosen to avoid.  Must be a multiple of 1024 (see +linux-x64-heap-alloc-start+).
  (let ((v (sb-ext:posix-getenv "MODUS_X64_MIDPOINT")))
    (if (and v (plusp (length v))) (parse-integer v) #x38000000)))  ; 896MB semispace (2x orig): self-compile ~580MB total alloc fits WITHOUT a collection (avoids the large-working-set-collection corruption). Past 2 GB the stub loads sizes with MOVABS (EMIT-X64-STUB-ADD).
(defconstant +linux-x64-heap-data-size+ (+ (* 2 +linux-x64-gc-midpoint+) +linux-x64-gc-guard+))  ; 2 semispaces + 16MB guard
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
;;; The layout is a function of the semispace size, chosen at startup: a hosted
;;; image's stub computes it from --dynamic-space-size with exactly the arithmetic
;;; X64-HEAP-LAYOUT does (EMIT-LINUX-X64-CHOOSE-HEAP), and stores what the
;;; translator and the heap tail need in three words (fixed-addresses.lisp:
;;; :X64-HEAP-MIDPOINT / -DATA-SIZE / :X64-KIND-DELTA).  The constants below are
;;; the DEFAULT layout (no flag; the Windows stub's).
(defconstant +mcgc-page-size+ #x1000)           ; 4 KiB pages

(defun x64-heap-layout (mid)
  "The heap mapping for semispaces MID bytes apart (MID = the second semispace's
   offset from the base), as a plist: :DATA (two semispaces and the guard), then
   the metadata arrays after it, each 64-byte aligned -- descriptors (1 byte a
   4 KB page), the object-start bitmap (1 bit / 16 bytes), the free list (4 bytes a
   page), the cons-kind bitmap -- :KIND-DELTA (kind bitmap minus start bitmap) and
   :MAP (the whole mapping, page-rounded)."
  (let* ((data (+ (* 2 mid) +linux-x64-gc-guard+))
         (pages (ash data -12))
         (bmo (logand (+ data pages 63) (lognot 63)))
         (bms (ash data -7))
         (flo (logand (+ bmo bms 63) (lognot 63)))
         (kdo (logand (+ flo (* 4 pages) 63) (lognot 63))))
    (list :data data :pages pages :bitmap-offset bmo :freelist-offset flo
          :kind-delta (- kdo bmo) :map (logand (+ kdo bms #xFFF) (lognot #xFFF)))))

(defconstant +mcgc-page-count+ (getf (x64-heap-layout +linux-x64-gc-midpoint+) :pages))
(defconstant +mcgc-meta-offset+ +linux-x64-heap-data-size+) ; metadata starts right after data
(defconstant +mcgc-descriptor-offset+ +mcgc-meta-offset+)
(defconstant +mcgc-bitmap-offset+ (getf (x64-heap-layout +linux-x64-gc-midpoint+) :bitmap-offset))
(defconstant +mcgc-freelist-offset+ (getf (x64-heap-layout +linux-x64-gc-midpoint+) :freelist-offset))
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; the default layout's: bare x64 stores this one (boot-x64.lisp), a hosted stub
  ;; the one it computes
  (setf modus.mvm.x64::*mcgc-kindbitmap-delta*
        (getf (x64-heap-layout +linux-x64-gc-midpoint+) :kind-delta)))
;; Full mmap = data region + metadata region (the default layout's).
(defconstant +linux-x64-heap-size+ (getf (x64-heap-layout +linux-x64-gc-midpoint+) :map))
;; --dynamic-space-size N: semispaces N/2, rounded down to 1 MB, within these.
;; The heap is mapped at +LINUX-X64-FIXED-HEAP-BASE+ (0x2000000000) and the JIT
;; arena at 0x3000000000, so the whole mapping stays under 64 GB.
(defconstant +linux-x64-min-midpoint+ #x2000000)       ; 32 MB
(defconstant +linux-x64-max-midpoint+ #x600000000)     ; 24 GB

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
    ;; p_memsz: the zero-filled BSS must cover the fixed low window the runtime
    ;; addresses directly (0x10000000.. : GC metadata, globals, MV buffer, the
    ;; per-thread window, scratch pages), so the segment runs to 0x20000000.
    ;; It used to be sized by the HEAP (+ +linux-x64-heap-size+) -- a leftover
    ;; from when the heap lived in the BSS (06bfebd) -- which made the kernel
    ;; commit ~1.9 GB at exec: on a 1 GB VM the default overcommit heuristic
    ;; refuses that, and a refused BSS is a SIGSEGV before our first instruction.
    (mvm-emit-u64 buf (max (+ header-total raw-len) (- #x20000000 load-addr))) ; p_memsz
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

  ;; mmap heap, FIRST at the fixed base with MAP_FIXED_NOREPLACE (0x100000):
  ;; rax=9, rdi=addr, rsi=size, rdx=prot, r10=flags, r8=fd, r9=off.
  ;; MAP_NORESERVE (0x4000), as SBCL maps its dynamic space: the ~1.9 GB heap is
  ;; reserved, not committed, so a machine with less RAM than that can run
  ;; (pages are backed as they are touched).  Without it the kernel's default
  ;; overcommit heuristic refuses the mapping on a 1 GB VM.
  (emit-linux-x64-choose-heap buf)
  (let ((hbase (modus.mvm::hosted-layout :heap-base +linux-x64-fixed-heap-base+))
        (abase (modus.mvm::hosted-layout :jit-arena-base +linux-x64-jit-arena-base+)))
    (emit-bytes buf #x48 #xBF) (emit-le64 buf hbase)      ; movabs rdi, fixed heap base
    (emit-bytes buf #x4C #x89 #xE6)                ; mov rsi, r12 (EMIT-LINUX-X64-CHOOSE-HEAP's size)
    (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, 3 (PROT_RW)
    (emit-bytes buf #x49 #xC7 #xC2 #x22 #x40 #x10 #x00) ; mov r10, 0x104022 (PRIV|ANON|NORESERVE|FIXED_NOREPLACE)
    (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
    (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
    (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, 9 (SYS_mmap)
    (emit-bytes buf #x0F #x05)                      ; syscall
    (emit-bytes buf #x48 #xB9) (emit-le64 buf hbase)      ; movabs rcx, fixed heap base
    (emit-bytes buf #x48 #x39 #xC8)                ; cmp rax, rcx
    (emit-bytes buf #x74 #x2F)                     ; je +47: got it, skip the fallback
    ;; Fallback: the historical hint mmap (kernel picks the address; --core will refuse).
    (emit-bytes buf #x48 #xC7 #xC7 #x00 #x00 #x00 #x10) ; mov rdi, 0x10000000
    (emit-bytes buf #x4C #x89 #xE6)                ; mov rsi, r12 (47 bytes from the je)
    (emit-bytes buf #x48 #xC7 #xC2 #x03 #x00 #x00 #x00) ; mov rdx, 3 (PROT_RW)
    (emit-bytes buf #x49 #xC7 #xC2 #x22 #x40 #x00 #x00) ; mov r10, 0x4022 (MAP_PRIV|MAP_ANON|NORESERVE)
    (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
    (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
    (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, 9 (SYS_mmap)
    (emit-bytes buf #x0F #x05)                      ; syscall
    ;; Both paths land here with the heap mapping's result in RAX.  An error is
    ;; -errno, i.e. unsigned >= -4095: say so and exit_group(1) rather than use
    ;; it as the heap base (which segfaulted with no message at all).
    (let* ((msg (append (map 'list #'char-code "modus: cannot map the heap (mmap failed)")
                        (list 10)))
           (tail 24)                               ; bytes after the lea, before msg
           (fail-len (+ 5 7 tail (length msg))))   ; mov edi + lea + tail + msg
      (when (>= fail-len 128) (error "heap-map failure path too long for a rel8 jb"))
      (emit-bytes buf #x48 #x3D #x01 #xF0 #xFF #xFF) ; cmp rax, -4095
      (emit-bytes buf #x72 fail-len)                 ; jb ok (a valid address)
      (emit-bytes buf #xBF #x02 #x00 #x00 #x00)      ; mov edi, 2 (stderr)
      (emit-bytes buf #x48 #x8D #x35) (emit-le32 buf tail) ; lea rsi, [rip+tail] = msg
      (emit-bytes buf #xBA) (emit-le32 buf (length msg)) ; mov edx, len
      (emit-bytes buf #xB8 #x01 #x00 #x00 #x00)      ; mov eax, 1 (SYS_write)
      (emit-bytes buf #x0F #x05)                     ; syscall
      (emit-bytes buf #xBF #x01 #x00 #x00 #x00)      ; mov edi, 1
      (emit-bytes buf #xB8 #xE7 #x00 #x00 #x00)      ; mov eax, 231 (SYS_exit_group)
      (emit-bytes buf #x0F #x05)                     ; syscall
      (dolist (b msg) (emit-bytes buf b)))           ; the message; then ok:
    ;; The JIT exec arena: one fixed RWX MAP_NORESERVE mapping; bump word =
    ;; its base, or 0 if the kernel refused.  RAX (heap base) is parked in RBP,
    ;; which nothing has used yet (kernel-main sets its own frame pointer).
    (emit-bytes buf #x48 #x89 #xC5)                ; mov rbp, rax
    (emit-bytes buf #x48 #xBF) (emit-le64 buf abase)      ; movabs rdi, arena base
    (emit-bytes buf #x48 #xC7 #xC6)                ; mov rsi, imm32
    (emit-le32 buf +linux-x64-jit-arena-size+)
    (emit-bytes buf #x48 #xC7 #xC2 #x07 #x00 #x00 #x00) ; mov rdx, 7 (PROT_RWX)
    (emit-bytes buf #x49 #xC7 #xC2 #x22 #x40 #x10 #x00) ; mov r10, 0x104022 (PRIV|ANON|NORESERVE|FIXED_NOREPLACE)
    (emit-bytes buf #x49 #xC7 #xC0 #xFF #xFF #xFF #xFF)   ; mov r8, -1
    (emit-bytes buf #x49 #xC7 #xC1 #x00 #x00 #x00 #x00) ; mov r9, 0
    (emit-bytes buf #x48 #xC7 #xC0 #x09 #x00 #x00 #x00) ; mov rax, 9 (SYS_mmap)
    (emit-bytes buf #x0F #x05)                      ; syscall
    (emit-bytes buf #x48 #xB9) (emit-le64 buf abase)      ; movabs rcx, arena base
    (emit-bytes buf #x48 #x39 #xC8)                ; cmp rax, rcx
    (emit-bytes buf #x74 #x02)                     ; je +2
    (emit-bytes buf #x31 #xC0)                     ; xor eax, eax   (refused -> 0)
    (emit-bytes buf #x48 #x89 #x04 #x25)           ; mov [abs32], rax
    (emit-le32 buf +linux-x64-jit-bump-slot+)
    (emit-bytes buf #x48 #x89 #xE8))               ; mov rax, rbp  (heap base back)

  (emit-linux-x64-heap-tail buf))

  ;; Fall through to JMP kernel-main

;;; ------------------------------------------------------------
;;; --dynamic-space-size: the heap is sized at startup
;;; ------------------------------------------------------------

(defun x64-stub-assemble (items)
  "Bytes for ITEMS: lists of bytes, (:LABEL K), (:JCC CC K) and (:JMP K), both
   rel32 -- CC the condition nibble (#x4 E, #x5 NE, #x6 BE, #x7 A, #x3 AE, #xD GE).
   For the hand-written stub code below, so no jump distance is counted by hand."
  (let ((labels (make-hash-table)) (pos 0))
    (dolist (it items)
      (case (car it)
        (:label (setf (gethash (second it) labels) pos))
        (:jcc (incf pos 6)) (:jmp (incf pos 5))
        (t (incf pos (length it)))))
    (setq pos 0)
    (let ((out nil))
      (flet ((rel32 (k end)
               (let ((d (- (or (gethash k labels) (error "stub label ~S" k)) end)))
                 (dotimes (i 4) (push (ldb (byte 8 (* 8 i)) d) out)))))
        (dolist (it items)
          (case (car it)
            (:label nil)
            (:jcc (push #x0F out) (push (+ #x80 (second it)) out) (incf pos 6) (rel32 (third it) pos))
            (:jmp (push #xE9 out) (incf pos 5) (rel32 (second it) pos))
            (t (dolist (b it) (push b out)) (incf pos (length it))))))
      (nreverse out))))

(defun %x64-stub-le32 (v) (loop for i below 4 collect (ldb (byte 8 (* 8 i)) v)))
(defun %x64-stub-le64 (v) (loop for i below 8 collect (ldb (byte 8 (* 8 i)) v)))

(defun emit-linux-x64-choose-heap (buf)
  "Choose the heap's size and lay it out, before it is mapped.  RSP is the
   kernel's argv block; RBX argc; RBX, R13-R15 are preserved.  The first
   --dynamic-space-size N (N in MB, or with a KB / MB / GB suffix, as SBCL takes
   it) sets the semispaces to N/2, rounded down to 1 MB and kept within
   +LINUX-X64-MIN/MAX-MIDPOINT+; with none, +LINUX-X64-GC-MIDPOINT+.  Stores the
   midpoint, data size and cons-kind delta (X64-HEAP-LAYOUT's arithmetic) in
   their words and leaves the mapping's size in R12."
  (let ((flag "--dynamic-space-size"))
    (dolist (b (x64-stub-assemble
                `((#x31 #xD2)                                 ; xor edx, edx   (no size yet)
                  (#xB9 1 0 0 0)                              ; mov ecx, 1
                  (:label :top)
                  (#x48 #x39 #xD9)                            ; cmp rcx, rbx
                  (:jcc #xD :done)                            ; jge
                  (#x48 #x8B #x74 #xCC #x08)                  ; mov rsi, [rsp+rcx*8+8]
                  ,@(loop for c across flag for k from 0
                          append `((#x80 #x7E ,k ,(char-code c)) (:jcc #x5 :next)))
                  (#x80 #x7E ,(length flag) 0) (:jcc #x5 :next)
                  (#x48 #x8D #x41 #x01)                       ; lea rax, [rcx+1]
                  (#x48 #x39 #xD8)                            ; cmp rax, rbx
                  (:jcc #xD :done)
                  (#x48 #x8B #x74 #xC4 #x08)                  ; mov rsi, [rsp+rax*8+8]
                  (#x45 #x31 #xC0)                            ; xor r8d, r8d
                  (:label :digits)
                  (#x0F #xB6 #x06)                            ; movzx eax, byte [rsi]
                  (#x48 #xFF #xC6)                            ; inc rsi
                  (#x8D #x78 #xD0)                            ; lea edi, [rax-48]
                  (#x83 #xFF #x09)                            ; cmp edi, 9
                  (:jcc #x7 :suffix)                          ; ja
                  (#x4D #x6B #xC0 #x0A)                       ; imul r8, r8, 10
                  (#x49 #x01 #xF8)                            ; add r8, rdi
                  (#x49 #x81 #xF8 ,@(%x64-stub-le32 #x100000))         ; cmp r8, 2^20 units
                  (:jcc #x6 :digits)                          ; jbe
                  (#x41 #xB8 ,@(%x64-stub-le32 #x100000))              ; mov r8d, 2^20 (saturate)
                  (:jmp :digits)
                  (:label :suffix)
                  (#x25 #xDF 0 0 0)                           ; and eax, 0xDF  (upper case)
                  (#x83 #xF8 #x4B) (:jcc #x5 :not-k)          ; cmp eax, 'K'
                  (#x49 #xC1 #xE0 10) (:jmp :have)            ; shl r8, 10
                  (:label :not-k)
                  (#x83 #xF8 #x47) (:jcc #x5 :mb)             ; cmp eax, 'G'
                  (#x49 #xC1 #xE0 30) (:jmp :have)            ; shl r8, 30
                  (:label :mb)
                  (#x49 #xC1 #xE0 20)                         ; shl r8, 20   (MB, the default unit)
                  (:label :have)
                  (#x4C #x89 #xC2)                            ; mov rdx, r8
                  (:jmp :done)
                  (:label :next)
                  (#x48 #xFF #xC1)                            ; inc rcx
                  (:jmp :top)
                  (:label :done)
                  (#x48 #x85 #xD2) (:jcc #x5 :sized)          ; test rdx, rdx ; jnz
                  (#x48 #xBA ,@(%x64-stub-le64 +linux-x64-gc-midpoint+)) ; movabs rdx, default midpoint
                  (:jmp :mid)
                  (:label :sized)
                  (#x48 #xD1 #xEA)                            ; shr rdx, 1   (two semispaces)
                  (#x48 #xB8 ,@(%x64-stub-le64 +linux-x64-min-midpoint+))
                  (#x48 #x39 #xC2) (:jcc #x3 :ge-min)         ; cmp rdx, rax ; jae
                  (#x48 #x89 #xC2)                            ; mov rdx, rax
                  (:label :ge-min)
                  (#x48 #xB8 ,@(%x64-stub-le64 +linux-x64-max-midpoint+))
                  (#x48 #x39 #xC2) (:jcc #x6 :le-max)         ; cmp rdx, rax ; jbe
                  (#x48 #x89 #xC2)                            ; mov rdx, rax
                  (:label :le-max)
                  (#x48 #x81 #xE2 ,@(%x64-stub-le32 #xFFF00000))       ; and rdx, -1 MB
                  (:label :mid)
                  ;; X64-HEAP-LAYOUT, instruction for instruction
                  (#x48 #x89 #x14 #x25 ,@(%x64-stub-le32 #x10000F88))  ; mov [midpoint], rdx
                  (#x48 #x8D #x3C #x12)                       ; lea rdi, [rdx+rdx]
                  (#x48 #x81 #xC7 ,@(%x64-stub-le32 +linux-x64-gc-guard+)) ; add rdi, guard    = data
                  (#x48 #x89 #x3C #x25 ,@(%x64-stub-le32 #x10000F90))  ; mov [data size], rdi
                  (#x49 #x89 #xF8) (#x49 #xC1 #xE8 12)        ; r8 = data >> 12   = pages
                  (#x4E #x8D #x4C #x07 #x3F)                  ; lea r9, [rdi+r8+63]
                  (#x49 #x83 #xE1 #xC0)                       ; and r9, -64       = bitmap offset
                  (#x49 #x89 #xFA) (#x49 #xC1 #xEA 7)         ; r10 = data >> 7   = bitmap bytes
                  (#x4F #x8D #x5C #x11 #x3F)                  ; lea r11, [r9+r10+63]
                  (#x49 #x83 #xE3 #xC0)                       ; and r11, -64      = free list
                  (#x4B #x8D #x44 #x83 #x3F)                  ; lea rax, [r11+r8*4+63]
                  (#x48 #x83 #xE0 #xC0)                       ; and rax, -64      = kind bitmap
                  (#x48 #x89 #xC1) (#x4C #x29 #xC9)           ; rcx = rax - r9
                  (#x48 #x89 #x0C #x25 ,@(%x64-stub-le32 #x10000F98))  ; mov [kind delta], rcx
                  (#x4E #x8D #xA4 #x10 ,@(%x64-stub-le32 #xFFF))       ; lea r12, [rax+r10+0xFFF]
                  (#x49 #x81 #xE4 ,@(%x64-stub-le32 #xFFFFF000))))))   ; and r12, -4096   = map size
      (emit-bytes buf b))))

;;; A heap-geometry value that does not fit a sign-extended imm32 (a MODUS_X64_MIDPOINT
;;; heap past 2 GB) is loaded with MOVABS through R11, which the stub does not otherwise
;;; use (a syscall clobbers it anyway).  A value that fits keeps the imm32 form, so the
;;; default layout's bytes are unchanged.
(defun %x64-simm32-p (v) (<= (- (ash 1 31)) v (1- (ash 1 31))))

(defun emit-x64-stub-add (buf reg v)
  "add REG, V for REG one of :rcx :r12 :r14."
  (if (%x64-simm32-p v)
      (progn (emit-bytes buf (if (eq reg :rcx) #x48 #x49) #x81
                         (ecase reg (:rcx #xC1) (:r12 #xC4) (:r14 #xC6)))
             (emit-le32 buf v))
      (progn (emit-bytes buf #x49 #xBB) (emit-le64 buf v)          ; movabs r11, v
             (emit-bytes buf (if (eq reg :rcx) #x4C #x4D) #x01
                         (ecase reg (:rcx #xD9) (:r12 #xDC) (:r14 #xDE))))))

(defun emit-linux-x64-heap-tail (buf)
  "The part of the Linux x64 entry that follows the heap mapping, with RAX =
   the heap base: argc/argv into the low block, the GC metadata, the MCGC
   config words and the MVM registers (R12, R14, R15, RBP).  Shared with the
   Windows stub (boot-win64.lisp), which maps the heap with VirtualAlloc but
   otherwise enters the same state.  Position independent: absolute addresses
   and rel8 jumps inside it only."
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
  ;; The FIRST semispace needs the same headroom the trampoline leaves after
  ;; every flip (translate-x64 X64-GC-LIMIT-MARGIN): it ends exactly where the
  ;; second begins, so a limit AT the midpoint lets an allocation straddle into
  ;; the to-space the first collection copies into.  Only the normal midpoint
  ;; limit moves; a MODUS_GC_R14 stress value is taken as given.
  ;; The midpoint is the one the stub chose ([0x10000F88]); a MODUS_GC_R14
  ;; stress value is a fixed offset.
  (if (= *linux-x64-r14-offset* +linux-x64-gc-midpoint+)
      (progn
        (emit-bytes buf #x4C #x8B #x34 #x25) (emit-le32 buf #x10000F88) ; mov r14, [midpoint]
        (emit-bytes buf #x49 #x81 #xEE) (emit-le32 buf +linux-x64-gc-guard+) ; sub r14, guard
        (emit-bytes buf #x49 #x01 #xC6))                                ; add r14, rax
      (progn
        (emit-bytes buf #x49 #x89 #xC6)                                 ; mov r14, rax
        (emit-x64-stub-add buf :r14 *linux-x64-r14-offset*)))

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
  (emit-bytes buf #x48 #x8B #x0C #x25) (emit-le32 buf #x10000F88) ; mov rcx, [midpoint]
  (emit-bytes buf #x48 #x01 #xC1)                ; add rcx, rax
  (emit-bytes buf #x48 #x89 #x0C #x25)           ; mov [abs32], rcx
  (emit-le32 buf +gc-to-start-addr+)

  ;; [0x10000050] = space_size = midpoint - alloc_offset
  (emit-bytes buf #x48 #x8B #x0C #x25) (emit-le32 buf #x10000F88) ; mov rcx, [midpoint]
  (emit-bytes buf #x48 #x81 #xE9) (emit-le32 buf +linux-x64-heap-alloc-start+) ; sub rcx, alloc start
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
           (emit-x64-stub-add buf :rcx off)
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
    ;; The rest from the data size the stub chose: X64-HEAP-LAYOUT's offsets.
    ;; rcx = data, rdx = pages, rsi = bitmap offset, rdi = free-list offset.
    (emit-bytes buf #x48 #x8B #x0C #x25) (emit-le32 buf #x10000F90) ; mov rcx, [data size]
    (emit-bytes buf #x48 #x89 #xCA #x48 #xC1 #xEA 12)  ; rdx = rcx >> 12
    (emit-bytes buf #x48 #x8D #x74 #x11 #x3F)          ; lea rsi, [rcx+rdx+63]
    (emit-bytes buf #x48 #x83 #xE6 #xC0)               ; and rsi, -64
    (emit-bytes buf #x48 #x89 #xCF #x48 #xC1 #xEF 7)   ; rdi = rcx >> 7
    (emit-bytes buf #x48 #x8D #x7C #x3E #x3F)          ; lea rdi, [rsi+rdi+63]
    (emit-bytes buf #x48 #x83 #xE7 #xC0)               ; and rdi, -64
    (flet ((store-rax+ (reg-sib slot)
             ;; lea r8, [rax+REG] ; mov [slot], r8
             (emit-bytes buf #x4C #x8D #x04 reg-sib)
             (emit-bytes buf #x4C #x89 #x04 #x25) (emit-le32 buf slot)))
      (store-rax+ #x08 +mcgc-cfg-data-end+)     ; data_end   = base + data   (SIB rax+rcx)
      (store-rax+ #x08 +mcgc-cfg-descriptor+)   ; descriptors follow the data
      (store-rax+ #x30 +mcgc-cfg-bitmap+)       ; SIB rax+rsi
      (store-rax+ #x38 +mcgc-cfg-freelist+))    ; SIB rax+rdi
    (emit-bytes buf #x48 #x89 #x14 #x25) (emit-le32 buf +mcgc-cfg-page-count+) ; mov [page count], rdx
    (store-imm +mcgc-cfg-freelist-count+  0)
    (store-imm +mcgc-cfg-alloc-page+      0))

  ;; R15 = NIL
  (emit-bytes buf #x49 #xBF)                      ; mov r15, imm64
  (emit-le64 buf #xDEAD0001)
  ;; Frame pointer
  (emit-bytes buf #x48 #x89 #xE5))               ; mov rbp, rsp

;;; ============================================================
;;; Helper: emit raw bytes
;;; ============================================================

(defun emit-bytes (buf &rest bytes)
  (dolist (b bytes) (mvm-emit-byte buf b)))

;;; WHERE NATIVE CODE STARTS in a linux-x64 image: ELF header + program header
;;; (64 + 56), the entry stub above, and the 5-byte JMP cross.lisp emits after
;;; it.  COMPUTED FROM THE STUB, never pinned: translate-x64 keeps every
;;; function's address clear of nibble 1 (the closure tag CALL-IND tests) using
;;; this number, so a stub that grows under a pinned value moves every function
;;; to the wrong nibble -- measured 2026-10-07: the fixed-heap + JIT-arena stub
;;; (570 bytes) under the old pinned 397 (a 272-byte stub) was a SIGSEGV at boot
;;; in every variant, with RIP = RSP = 0 after an unwind onto an empty handler
;;; frame.  Six build scripts set *X64-NATIVE-CODE-OFFSET* from this.
(defun linux-x64-native-code-offset-for (entry-fn)
  "The offset for an image whose boot descriptor uses ENTRY-FN as its stub
   (the ANSI runner wraps the stub with a NIL page and the code-bounds init)."
  (let ((buf (make-mvm-buffer)))
    (funcall entry-fn buf)
    (+ 64 56 (length (mvm-buffer-used-bytes buf)) 5)))
(defun linux-x64-native-code-offset ()
  (linux-x64-native-code-offset-for #'emit-linux-x64-entry))

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
