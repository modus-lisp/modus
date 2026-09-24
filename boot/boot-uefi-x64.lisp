;;;; boot-uefi-x64.lisp - UEFI Boot for x86-64
;;;;
;;;; Produces a PE32+ EFI application that:
;;;;   1. Calls ExitBootServices to take full control
;;;;   2. Copies kernel to 0x100000
;;;;   3. Sets up page tables, GDT, serial, alloc registers
;;;;   4. Jumps to kernel-main
;;;;
;;;; Phase 2: Serial + GOP framebuffer + PS/2 keyboard.
;;;; Serial for QEMU testing, framebuffer for real hardware (ThinkPad T420).

(in-package :modus.mvm)

;;; ============================================================
;;; UEFI Constants
;;; ============================================================

(defconstant +efi-st-boot-services+ #x60)    ; SystemTable->BootServices

;; EFI_BOOT_SERVICES function pointer offsets
(defconstant +efi-bs-get-memory-map+      #x38)
(defconstant +efi-bs-exit-boot-services+  #xE8)
(defconstant +efi-bs-locate-protocol+     #x140)

;; EFI_GRAPHICS_OUTPUT_PROTOCOL GUID
;; {9042A9DE-23DC-4A38-96FB-7ADE-D080-516A}
;; As two little-endian qwords:
(defconstant +gop-guid-qw0+ #x4A3823DC9042A9DE)
(defconstant +gop-guid-qw1+ #x6A5180D0DE7AFB96)

;; Framebuffer info memory layout
(defconstant +fb-info-base+   #x600100)  ; fb_base (u64, pre-tagged)
(defconstant +fb-ppsl-addr+   #x600108)  ; ppsl (u32, raw)
(defconstant +fb-pixfmt-addr+ #x60010C)  ; pixel_format (u32)
(defconstant +fb-cx-addr+     #x600110)  ; cursor_x (u32)
(defconstant +fb-cy-addr+     #x600114)  ; cursor_y (u32)
(defconstant +fb-tcols-addr+  #x600118)  ; text_cols (u32)
(defconstant +fb-trows-addr+  #x60011C)  ; text_rows (u32)
(defconstant +fb-valid-addr+  #x600120)  ; fb_valid (u32)
(defconstant +font-base+      #x601000)  ; 95 chars × 8 bytes
(defconstant +scan-normal+    #x601800)  ; 128 bytes
(defconstant +scan-shifted+   #x601880)  ; 128 bytes
(defconstant +shift-state+    #x601900)  ; u32

;;; ============================================================
;;; PE32+ Emitter
;;; ============================================================

(defun pe-align-up (n alignment)
  (logand (+ n (1- alignment)) (lognot (1- alignment))))

(defun emit-pe32plus-header (buf code-size)
  "Emit a minimal PE32+ header for an EFI application.
   The .text section starts at file offset 0x200, RVA 0x1000.
   Entry point RVA = 0x1000 (start of .text)."
  (let* ((header-size #x200)
         (section-vsize (pe-align-up code-size #x1000))
         (section-fsize (pe-align-up code-size #x200))
         (image-size (+ #x1000 section-vsize)))

    ;; DOS Header (64 bytes)
    (mvm-emit-byte buf #x4D) (mvm-emit-byte buf #x5A)   ; "MZ"
    (dotimes (i 58) (mvm-emit-byte buf 0))
    (mvm-emit-u32 buf #x40)                               ; e_lfanew -> PE sig

    ;; PE Signature at offset 0x40
    (mvm-emit-byte buf #x50) (mvm-emit-byte buf #x45)    ; "PE"
    (mvm-emit-byte buf 0) (mvm-emit-byte buf 0)

    ;; COFF File Header (20 bytes)
    (mvm-emit-u16 buf #x8664)          ; Machine = AMD64
    (mvm-emit-u16 buf 1)               ; NumberOfSections
    (mvm-emit-u32 buf 0)               ; TimeDateStamp
    (mvm-emit-u32 buf 0)               ; PointerToSymbolTable
    (mvm-emit-u32 buf 0)               ; NumberOfSymbols
    (mvm-emit-u16 buf 240)             ; SizeOfOptionalHeader
    (mvm-emit-u16 buf #x2022)          ; Characteristics: EXEC|LARGE_ADDR|DLL

    ;; PE32+ Optional Header (240 bytes)
    (mvm-emit-u16 buf #x20B)           ; PE32+ magic
    (mvm-emit-byte buf 0) (mvm-emit-byte buf 0)  ; Linker version
    (mvm-emit-u32 buf section-fsize)   ; SizeOfCode
    (mvm-emit-u32 buf 0)               ; SizeOfInitializedData
    (mvm-emit-u32 buf 0)               ; SizeOfUninitializedData
    (mvm-emit-u32 buf #x1000)          ; AddressOfEntryPoint (RVA)
    (mvm-emit-u32 buf #x1000)          ; BaseOfCode
    (mvm-emit-u32 buf 0) (mvm-emit-u32 buf 0)  ; ImageBase (u64, 0 = relocatable)
    (mvm-emit-u32 buf #x1000)          ; SectionAlignment
    (mvm-emit-u32 buf #x200)           ; FileAlignment
    (mvm-emit-u16 buf 0) (mvm-emit-u16 buf 0)  ; OS version
    (mvm-emit-u16 buf 0) (mvm-emit-u16 buf 0)  ; Image version
    (mvm-emit-u16 buf 0) (mvm-emit-u16 buf 0)  ; Subsystem version
    (mvm-emit-u32 buf 0)               ; Win32VersionValue
    (mvm-emit-u32 buf image-size)      ; SizeOfImage
    (mvm-emit-u32 buf header-size)     ; SizeOfHeaders
    (mvm-emit-u32 buf 0)               ; CheckSum
    (mvm-emit-u16 buf 10)              ; Subsystem = EFI_APPLICATION
    (mvm-emit-u16 buf 0)               ; DllCharacteristics
    (dotimes (i 4) (mvm-emit-u32 buf 0) (mvm-emit-u32 buf 0))  ; Stack/Heap sizes (4 u64s)
    (mvm-emit-u32 buf 0)               ; LoaderFlags
    (mvm-emit-u32 buf 16)              ; NumberOfRvaAndSizes
    (dotimes (i 16) (mvm-emit-u32 buf 0) (mvm-emit-u32 buf 0)) ; 16 data directories

    ;; Section Table: .text (40 bytes)
    ;; Name
    (dolist (c '(#\. #\t #\e #\x #\t #\Nul #\Nul #\Nul))
      (mvm-emit-byte buf (if (characterp c) (char-code c) 0)))
    (mvm-emit-u32 buf section-vsize)   ; VirtualSize
    (mvm-emit-u32 buf #x1000)          ; VirtualAddress (RVA)
    (mvm-emit-u32 buf section-fsize)   ; SizeOfRawData
    (mvm-emit-u32 buf header-size)     ; PointerToRawData
    (mvm-emit-u32 buf 0)               ; PointerToRelocations
    (mvm-emit-u32 buf 0)               ; PointerToLinenumbers
    (mvm-emit-u16 buf 0)               ; NumberOfRelocations
    (mvm-emit-u16 buf 0)               ; NumberOfLinenumbers
    (mvm-emit-u32 buf #xE0000060)      ; CODE|EXECUTE|READ|WRITE

    ;; Pad to 0x200
    (let ((pos (mvm-buffer-position buf)))
      (dotimes (i (- header-size pos))
        (mvm-emit-byte buf 0)))))

(defun wrap-in-pe32plus (raw-bytes)
  "Wrap raw image bytes in a PE32+ EFI application."
  (let* ((code-size (length raw-bytes))
         (hdr-buf (make-mvm-buffer)))
    (emit-pe32plus-header hdr-buf code-size)
    (let* ((hdr-bytes (mvm-buffer-used-bytes hdr-buf))
           (hdr-size (length hdr-bytes))
           (section-fsize (pe-align-up code-size #x200))
           (total (+ hdr-size section-fsize))
           (result (make-array total :element-type '(unsigned-byte 8) :initial-element 0)))
      (replace result hdr-bytes)
      (replace result raw-bytes :start1 hdr-size)
      result)))

;;; ============================================================
;;; UEFI Stub Patch Info (passed from emitter to assembler)
;;; ============================================================

(defvar *uefi-gdt-layout* :toy
  "Which GDT the entry stub installs.
   :TOY      — 0x08 = 64-bit code, 0x10 = data (the UEFI REPL; the kernel at
               0x100000 is the toy's own native code).
   :BOOT-X64 — boot-x64.lisp's layout: 0x08 = 32-bit code, 0x10 = 64-bit code,
               data selectors NULL.  Required when the stub hands over to
               emit-x64-kernel64-entry, whose IDT gates name selector 0x10.")
(defvar *uefi-console-tables* t
  "Emit the toy console's font / scancode tables and framebuffer clear.  NIL
   for the CL image: those tables sit at 0x601000, INSIDE a 40 MB kernel copied
   to 0x100000, and would overwrite it.")
(defvar *uefi-stub-pad* nil
  "When an integer, the entry stub is zero-padded to exactly this many bytes
   and the kernel data copied to 0x100000 starts there (not at the end of the
   whole boot-code buffer).  Lets a descriptor put more boot code (boot-x64's
   kernel64 entry) AFTER the stub, at a KNOWN offset, so the image's runtime
   addresses are load-addr + (image offset - pad) with load-addr = 0x100000 - pad.")
(defvar *uefi-kernel-data-start* nil
  "Set by emit-uefi-entry-stub when *uefi-stub-pad* is on: the image offset of
   the kernel data.  patch-uefi-stub prefers it over the boot-code length.")

(defvar *uefi-lea-patch-pos* nil
  "Position of disp32 in LEA RSI,[RIP+disp32] for kernel data source address.")
(defvar *uefi-size-patch-pos* nil
  "Position of imm32 in MOV ECX,imm32 for kernel data size.")

(defun patch-uefi-stub (raw-bytes boot-code-len)
  "Patch the UEFI stub in RAW-BYTES with kernel data offset and size.
   BOOT-CODE-LEN is the byte offset where kernel data starts."
  (when (and *uefi-lea-patch-pos* *uefi-size-patch-pos*)
    (when *uefi-kernel-data-start* (setq boot-code-len *uefi-kernel-data-start*))
    (let* ((kernel-data-size (- (length raw-bytes) boot-code-len))
           ;; LEA RSI,[RIP+disp32]: RIP = lea-pos + 4, target = boot-code-len
           (lea-disp32 (- boot-code-len (+ *uefi-lea-patch-pos* 4))))
      ;; Patch LEA disp32 (4 bytes, little-endian)
      (setf (aref raw-bytes (+ *uefi-lea-patch-pos* 0)) (ldb (byte 8 0) lea-disp32))
      (setf (aref raw-bytes (+ *uefi-lea-patch-pos* 1)) (ldb (byte 8 8) lea-disp32))
      (setf (aref raw-bytes (+ *uefi-lea-patch-pos* 2)) (ldb (byte 8 16) lea-disp32))
      (setf (aref raw-bytes (+ *uefi-lea-patch-pos* 3)) (ldb (byte 8 24) lea-disp32))
      ;; Patch MOV ECX,imm32 (4 bytes)
      (setf (aref raw-bytes (+ *uefi-size-patch-pos* 0)) (ldb (byte 8 0) kernel-data-size))
      (setf (aref raw-bytes (+ *uefi-size-patch-pos* 1)) (ldb (byte 8 8) kernel-data-size))
      (setf (aref raw-bytes (+ *uefi-size-patch-pos* 2)) (ldb (byte 8 16) kernel-data-size))
      (setf (aref raw-bytes (+ *uefi-size-patch-pos* 3)) (ldb (byte 8 24) kernel-data-size))
      (format t "UEFI stub patched: kernel at +~D, ~D bytes~%" boot-code-len kernel-data-size))))

;;; ============================================================
;;; UEFI Entry Stub
;;; ============================================================
;;;
;;; MS x64 ABI: args in RCX, RDX, R8, R9, then stack.
;;; 32-byte shadow space before every call.
;;; Non-volatile: RBX, RBP, RSI, RDI, R12-R15.
;;;
;;; On entry: RCX=ImageHandle, RDX=SystemTable, 64-bit long mode, paging on.

;; Register constants for readability
(defconstant +rax+ 0) (defconstant +rcx+ 1) (defconstant +rdx+ 2) (defconstant +rbx+ 3)
(defconstant +rsp+ 4) (defconstant +rbp+ 5) (defconstant +rsi+ 6) (defconstant +rdi+ 7)
(defconstant +r8+ 8) (defconstant +r9+ 9) (defconstant +r10+ 10) (defconstant +r11+ 11)
(defconstant +r12+ 12) (defconstant +r13+ 13) (defconstant +r14+ 14) (defconstant +r15+ 15)

;;; Simple x64 instruction emitters (correct for all reg combos including RSP)

(defun uefi-emit-push (buf reg)
  (when (>= reg 8) (mvm-emit-byte buf #x41))
  (mvm-emit-byte buf (logior #x50 (logand reg 7))))

(defun uefi-emit-pop (buf reg)
  (when (>= reg 8) (mvm-emit-byte buf #x41))
  (mvm-emit-byte buf (logior #x58 (logand reg 7))))

(defun uefi-emit-mov-reg-imm64 (buf reg imm)
  "mov REG, imm64"
  (mvm-emit-byte buf (logior #x48 (if (>= reg 8) 1 0)))
  (mvm-emit-byte buf (logior #xB8 (logand reg 7)))
  (mvm-emit-u32 buf (ldb (byte 32 0) imm))
  (mvm-emit-u32 buf (ldb (byte 32 32) imm)))

(defun uefi-emit-mov-reg-reg (buf dst src)
  "mov DST, SRC (64-bit)"
  (mvm-emit-byte buf (logior #x48
                              (if (>= src 8) 4 0)
                              (if (>= dst 8) 1 0)))
  (mvm-emit-byte buf #x89)
  (mvm-emit-byte buf (logior #xC0 (ash (logand src 7) 3) (logand dst 7))))

(defun uefi-emit-mov-reg-mem (buf dst base disp)
  "mov DST, [BASE + disp32] (64-bit load). Handles RSP base correctly."
  (mvm-emit-byte buf (logior #x48
                              (if (>= dst 8) 4 0)
                              (if (>= base 8) 1 0)))
  (mvm-emit-byte buf #x8B)
  (if (= (logand base 7) 4)  ; RSP/R12 needs SIB
      (progn
        (mvm-emit-byte buf (logior #x84 (ash (logand dst 7) 3)))
        (mvm-emit-byte buf #x24)  ; SIB: base=RSP, index=none
        (mvm-emit-u32 buf disp))
      (progn
        (mvm-emit-byte buf (logior #x80 (ash (logand dst 7) 3) (logand base 7)))
        (mvm-emit-u32 buf disp))))

(defun uefi-emit-mov-mem-reg (buf base disp src)
  "mov [BASE + disp32], SRC (64-bit store). Handles RSP base correctly."
  (mvm-emit-byte buf (logior #x48
                              (if (>= src 8) 4 0)
                              (if (>= base 8) 1 0)))
  (mvm-emit-byte buf #x89)
  (if (= (logand base 7) 4)  ; RSP/R12 needs SIB
      (progn
        (mvm-emit-byte buf (logior #x84 (ash (logand src 7) 3)))
        (mvm-emit-byte buf #x24)
        (mvm-emit-u32 buf disp))
      (progn
        (mvm-emit-byte buf (logior #x80 (ash (logand src 7) 3) (logand base 7)))
        (mvm-emit-u32 buf disp))))

(defun uefi-emit-lea-rsp (buf dst disp)
  "lea DST, [RSP + disp32]"
  (mvm-emit-byte buf (logior #x48 (if (>= dst 8) 4 0)))
  (mvm-emit-byte buf #x8D)
  (mvm-emit-byte buf (logior #x84 (ash (logand dst 7) 3)))
  (mvm-emit-byte buf #x24)
  (mvm-emit-u32 buf disp))

(defun uefi-emit-sub-rsp (buf imm32)
  "sub rsp, imm32"
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x81) (mvm-emit-byte buf #xEC)
  (mvm-emit-u32 buf imm32))

(defun uefi-emit-add-rsp (buf imm32)
  "add rsp, imm32"
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x81) (mvm-emit-byte buf #xC4)
  (mvm-emit-u32 buf imm32))

(defun uefi-emit-call-mem (buf base disp)
  "call qword [BASE + disp32]. BASE is 0-15."
  ;; REX prefix: 0x40 + W=0(default 64 for call) + B if base>=8
  ;; Actually call through memory doesn't need REX.W
  (when (>= base 8) (mvm-emit-byte buf #x41))
  (mvm-emit-byte buf #xFF)
  ;; ModRM: mod=10 (disp32), reg=010 (/2 for CALL), rm=base
  (let ((rm (logand base 7)))
    (if (= rm 4)  ; RSP/R12 needs SIB
        (progn
          (mvm-emit-byte buf (logior #x94))  ; mod=10, reg=2, rm=4(SIB)
          (mvm-emit-byte buf #x24)            ; SIB: base=4(RSP), index=none
          (mvm-emit-u32 buf disp))
        (if (= rm 5)  ; RBP/R13 with disp=0 still needs disp32 (mod=10 is fine)
            (progn
              (mvm-emit-byte buf (logior #x90 rm))
              (mvm-emit-u32 buf disp))
            (progn
              (mvm-emit-byte buf (logior #x90 rm))
              (mvm-emit-u32 buf disp))))))

(defun uefi-emit-lea-rip-disp32 (buf dst)
  "lea DST, [RIP + disp32]. Returns position of the disp32 for patching."
  (mvm-emit-byte buf (logior #x48 (if (>= dst 8) 4 0)))
  (mvm-emit-byte buf #x8D)
  (mvm-emit-byte buf (logior (ash (logand dst 7) 3) #x05))  ; ModRM: reg,[RIP+disp32]
  (let ((pos (mvm-buffer-position buf)))
    (mvm-emit-u32 buf 0)
    pos))

;;; ============================================================
;;; Additional instruction emitters for GOP/FB setup
;;; ============================================================

(defun uefi-emit-store-imm32-abs32 (buf addr imm32)
  "mov dword [addr32], imm32 — write 32-bit immediate to absolute address."
  ;; C7 /0 ModRM=04(SIB) SIB=25(disp32) addr32 imm32
  (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
  (mvm-emit-u32 buf addr) (mvm-emit-u32 buf imm32))

(defun uefi-emit-load-eax-abs32 (buf addr)
  "mov eax, [addr32] — load 32-bit from absolute address."
  (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
  (mvm-emit-u32 buf addr))

(defun uefi-emit-store-eax-abs32 (buf addr)
  "mov [addr32], eax — store 32-bit to absolute address."
  (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
  (mvm-emit-u32 buf addr))

(defun uefi-emit-shr-eax-imm (buf count)
  "shr eax, imm8"
  (mvm-emit-byte buf #xC1) (mvm-emit-byte buf #xE8) (mvm-emit-byte buf count))

;;; ============================================================
;;; GOP Query — get framebuffer info before ExitBootServices
;;; ============================================================

(defun emit-uefi-gop-query (buf)
  "Query GOP for framebuffer info. R14 = BootServices.
   Stores results at 0x600100..0x600120."

  ;; Pre-zero fb_valid so it stays 0 on failure
  (uefi-emit-store-imm32-abs32 buf +fb-valid-addr+ 0)

  ;; Allocate sub-frame: 56 bytes (shadow=32 + GUID=16 + Interface=8)
  ;; After 8 pushes RSP ≡ 8 mod 16; sub 56 → RSP ≡ 0 mod 16 → correct for CALL
  (uefi-emit-sub-rsp buf 56)

  ;; Zero Interface pointer at [RSP+48]
  (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)  ; xor eax, eax
  (uefi-emit-mov-mem-reg buf +rsp+ 48 +rax+)

  ;; Write GOP GUID at [RSP+32..47]
  (uefi-emit-mov-reg-imm64 buf +rax+ +gop-guid-qw0+)
  (uefi-emit-mov-mem-reg buf +rsp+ 32 +rax+)
  (uefi-emit-mov-reg-imm64 buf +rax+ +gop-guid-qw1+)
  (uefi-emit-mov-mem-reg buf +rsp+ 40 +rax+)

  ;; LocateProtocol(RCX=&GUID, RDX=NULL, R8=&Interface)
  (uefi-emit-lea-rsp buf +rcx+ 32)           ; RCX = &GUID
  (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xD2) ; xor edx, edx (NULL)
  (uefi-emit-lea-rsp buf +r8+ 48)            ; R8 = &Interface

  (uefi-emit-call-mem buf +r14+ +efi-bs-locate-protocol+)

  ;; Test return: EAX = 0 on success
  ;; test eax, eax
  (mvm-emit-byte buf #x85) (mvm-emit-byte buf #xC0)
  ;; JNZ rel32 — skip success block on failure
  (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x85)
  (let ((jnz-patch-pos (mvm-buffer-position buf)))
    (mvm-emit-u32 buf 0)  ; placeholder disp32

    ;; ---- GOP found: extract framebuffer info ----
    ;; RAX = GOP protocol pointer
    (uefi-emit-mov-reg-mem buf +rax+ +rsp+ 48)

    ;; RBX = GOP->Mode (offset 0x18 in protocol struct)
    ;; mov rbx, [rax+0x18]
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x58)
    (mvm-emit-byte buf #x18)

    ;; FrameBufferBase = [RBX+0x18] (u64)
    ;; mov rax, [rbx+0x18]
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x43)
    (mvm-emit-byte buf #x18)
    ;; Pre-tag: shl rax, 1
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xD1) (mvm-emit-byte buf #xE0)
    ;; Store at 0x600100 (u64)
    ;; mov rdi, addr; mov [rdi], rax
    (uefi-emit-mov-reg-imm64 buf +rdi+ +fb-info-base+)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)

    ;; Info pointer = [RBX+0x08]
    ;; mov rdx, [rbx+0x08]
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x53)
    (mvm-emit-byte buf #x08)

    ;; PixelsPerScanLine = [RDX+0x20]
    ;; mov eax, [rdx+0x20]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x42) (mvm-emit-byte buf #x20)
    (uefi-emit-store-eax-abs32 buf +fb-ppsl-addr+)

    ;; PixelFormat = [RDX+0x0C]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x42) (mvm-emit-byte buf #x0C)
    (uefi-emit-store-eax-abs32 buf +fb-pixfmt-addr+)

    ;; text_cols = Width / 8   (Width = [RDX+0x04])
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x42) (mvm-emit-byte buf #x04)
    (uefi-emit-shr-eax-imm buf 3)
    (uefi-emit-store-eax-abs32 buf +fb-tcols-addr+)

    ;; text_rows = Height / 8  (Height = [RDX+0x08])
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x42) (mvm-emit-byte buf #x08)
    (uefi-emit-shr-eax-imm buf 3)
    (uefi-emit-store-eax-abs32 buf +fb-trows-addr+)

    ;; cursor_x = 0, cursor_y = 0
    (uefi-emit-store-imm32-abs32 buf +fb-cx-addr+ 0)
    (uefi-emit-store-imm32-abs32 buf +fb-cy-addr+ 0)

    ;; fb_valid = 1
    (uefi-emit-store-imm32-abs32 buf +fb-valid-addr+ 1)

    ;; Patch JNZ to land here
    (let* ((here (mvm-buffer-position buf))
           (rel (- here (+ jnz-patch-pos 4)))
           (bytes (mvm-buffer-bytes buf)))
      (setf (aref bytes (+ jnz-patch-pos 0)) (ldb (byte 8 0) rel))
      (setf (aref bytes (+ jnz-patch-pos 1)) (ldb (byte 8 8) rel))
      (setf (aref bytes (+ jnz-patch-pos 2)) (ldb (byte 8 16) rel))
      (setf (aref bytes (+ jnz-patch-pos 3)) (ldb (byte 8 24) rel))))

  ;; Deallocate sub-frame
  (uefi-emit-add-rsp buf 56))

;;; ============================================================
;;; Font Data — 8×8 bitmap font, ASCII 32–126 (95 glyphs)
;;; ============================================================

(defun font-rows-to-qword (r0 r1 r2 r3 r4 r5 r6 r7)
  "Pack 8 font row bytes into a little-endian qword for STOSQ."
  (logior r0 (ash r1 8) (ash r2 16) (ash r3 24)
          (ash r4 32) (ash r5 40) (ash r6 48) (ash r7 56)))

(defvar *font-8x8-qwords*
  (mapcar (lambda (rows) (apply #'font-rows-to-qword rows))
    '(;; 32 space
      (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      ;; 33 !
      (#x18 #x3C #x3C #x18 #x18 #x00 #x18 #x00)
      ;; 34 "
      (#x6C #x6C #x6C #x00 #x00 #x00 #x00 #x00)
      ;; 35 #
      (#x6C #x6C #xFE #x6C #xFE #x6C #x6C #x00)
      ;; 36 $
      (#x18 #x3E #x60 #x3C #x06 #x7C #x18 #x00)
      ;; 37 %
      (#x00 #xC6 #xCC #x18 #x30 #x66 #xC6 #x00)
      ;; 38 &
      (#x38 #x6C #x38 #x76 #xDC #xCC #x76 #x00)
      ;; 39 '
      (#x18 #x18 #x30 #x00 #x00 #x00 #x00 #x00)
      ;; 40 (
      (#x0C #x18 #x30 #x30 #x30 #x18 #x0C #x00)
      ;; 41 )
      (#x30 #x18 #x0C #x0C #x0C #x18 #x30 #x00)
      ;; 42 *
      (#x00 #x66 #x3C #xFF #x3C #x66 #x00 #x00)
      ;; 43 +
      (#x00 #x18 #x18 #x7E #x18 #x18 #x00 #x00)
      ;; 44 ,
      (#x00 #x00 #x00 #x00 #x00 #x18 #x18 #x30)
      ;; 45 -
      (#x00 #x00 #x00 #x7E #x00 #x00 #x00 #x00)
      ;; 46 .
      (#x00 #x00 #x00 #x00 #x00 #x18 #x18 #x00)
      ;; 47 /
      (#x06 #x0C #x18 #x30 #x60 #xC0 #x80 #x00)
      ;; 48 0
      (#x3C #x66 #x6E #x76 #x66 #x66 #x3C #x00)
      ;; 49 1
      (#x18 #x38 #x18 #x18 #x18 #x18 #x7E #x00)
      ;; 50 2
      (#x3C #x66 #x06 #x1C #x30 #x60 #x7E #x00)
      ;; 51 3
      (#x3C #x66 #x06 #x1C #x06 #x66 #x3C #x00)
      ;; 52 4
      (#x0C #x1C #x3C #x6C #xFE #x0C #x0C #x00)
      ;; 53 5
      (#x7E #x60 #x7C #x06 #x06 #x66 #x3C #x00)
      ;; 54 6
      (#x1C #x30 #x60 #x7C #x66 #x66 #x3C #x00)
      ;; 55 7
      (#x7E #x06 #x0C #x18 #x30 #x30 #x30 #x00)
      ;; 56 8
      (#x3C #x66 #x66 #x3C #x66 #x66 #x3C #x00)
      ;; 57 9
      (#x3C #x66 #x66 #x3E #x06 #x0C #x38 #x00)
      ;; 58 :
      (#x00 #x00 #x18 #x00 #x00 #x18 #x00 #x00)
      ;; 59 ;
      (#x00 #x00 #x18 #x00 #x00 #x18 #x18 #x30)
      ;; 60 <
      (#x0C #x18 #x30 #x60 #x30 #x18 #x0C #x00)
      ;; 61 =
      (#x00 #x00 #x7E #x00 #x7E #x00 #x00 #x00)
      ;; 62 >
      (#x30 #x18 #x0C #x06 #x0C #x18 #x30 #x00)
      ;; 63 ?
      (#x3C #x66 #x06 #x0C #x18 #x00 #x18 #x00)
      ;; 64 @
      (#x3C #x66 #x6E #x6A #x6E #x60 #x3C #x00)
      ;; 65 A
      (#x18 #x3C #x66 #x66 #x7E #x66 #x66 #x00)
      ;; 66 B
      (#x7C #x66 #x66 #x7C #x66 #x66 #x7C #x00)
      ;; 67 C
      (#x3C #x66 #x60 #x60 #x60 #x66 #x3C #x00)
      ;; 68 D
      (#x78 #x6C #x66 #x66 #x66 #x6C #x78 #x00)
      ;; 69 E
      (#x7E #x60 #x60 #x78 #x60 #x60 #x7E #x00)
      ;; 70 F
      (#x7E #x60 #x60 #x78 #x60 #x60 #x60 #x00)
      ;; 71 G
      (#x3C #x66 #x60 #x6E #x66 #x66 #x3E #x00)
      ;; 72 H
      (#x66 #x66 #x66 #x7E #x66 #x66 #x66 #x00)
      ;; 73 I
      (#x7E #x18 #x18 #x18 #x18 #x18 #x7E #x00)
      ;; 74 J
      (#x06 #x06 #x06 #x06 #x06 #x66 #x3C #x00)
      ;; 75 K
      (#x66 #x6C #x78 #x70 #x78 #x6C #x66 #x00)
      ;; 76 L
      (#x60 #x60 #x60 #x60 #x60 #x60 #x7E #x00)
      ;; 77 M
      (#xC6 #xEE #xFE #xD6 #xC6 #xC6 #xC6 #x00)
      ;; 78 N
      (#xC6 #xE6 #xF6 #xDE #xCE #xC6 #xC6 #x00)
      ;; 79 O
      (#x3C #x66 #x66 #x66 #x66 #x66 #x3C #x00)
      ;; 80 P
      (#x7C #x66 #x66 #x7C #x60 #x60 #x60 #x00)
      ;; 81 Q
      (#x3C #x66 #x66 #x66 #x6A #x6C #x36 #x00)
      ;; 82 R
      (#x7C #x66 #x66 #x7C #x6C #x66 #x66 #x00)
      ;; 83 S
      (#x3C #x66 #x60 #x3C #x06 #x66 #x3C #x00)
      ;; 84 T
      (#x7E #x18 #x18 #x18 #x18 #x18 #x18 #x00)
      ;; 85 U
      (#x66 #x66 #x66 #x66 #x66 #x66 #x3C #x00)
      ;; 86 V
      (#x66 #x66 #x66 #x66 #x66 #x3C #x18 #x00)
      ;; 87 W
      (#xC6 #xC6 #xC6 #xD6 #xFE #xEE #xC6 #x00)
      ;; 88 X
      (#x66 #x66 #x3C #x18 #x3C #x66 #x66 #x00)
      ;; 89 Y
      (#x66 #x66 #x66 #x3C #x18 #x18 #x18 #x00)
      ;; 90 Z
      (#x7E #x06 #x0C #x18 #x30 #x60 #x7E #x00)
      ;; 91 [
      (#x3C #x30 #x30 #x30 #x30 #x30 #x3C #x00)
      ;; 92 backslash
      (#xC0 #x60 #x30 #x18 #x0C #x06 #x02 #x00)
      ;; 93 ]
      (#x3C #x0C #x0C #x0C #x0C #x0C #x3C #x00)
      ;; 94 ^
      (#x10 #x38 #x6C #xC6 #x00 #x00 #x00 #x00)
      ;; 95 _
      (#x00 #x00 #x00 #x00 #x00 #x00 #x00 #xFE)
      ;; 96 `
      (#x30 #x18 #x0C #x00 #x00 #x00 #x00 #x00)
      ;; 97 a
      (#x00 #x00 #x3C #x06 #x3E #x66 #x3E #x00)
      ;; 98 b
      (#x60 #x60 #x7C #x66 #x66 #x66 #x7C #x00)
      ;; 99 c
      (#x00 #x00 #x3C #x66 #x60 #x66 #x3C #x00)
      ;; 100 d
      (#x06 #x06 #x3E #x66 #x66 #x66 #x3E #x00)
      ;; 101 e
      (#x00 #x00 #x3C #x66 #x7E #x60 #x3C #x00)
      ;; 102 f
      (#x1C #x30 #x7C #x30 #x30 #x30 #x30 #x00)
      ;; 103 g
      (#x00 #x00 #x3E #x66 #x66 #x3E #x06 #x3C)
      ;; 104 h
      (#x60 #x60 #x7C #x66 #x66 #x66 #x66 #x00)
      ;; 105 i
      (#x18 #x00 #x38 #x18 #x18 #x18 #x3C #x00)
      ;; 106 j
      (#x06 #x00 #x0E #x06 #x06 #x66 #x66 #x3C)
      ;; 107 k
      (#x60 #x60 #x66 #x6C #x78 #x6C #x66 #x00)
      ;; 108 l
      (#x38 #x18 #x18 #x18 #x18 #x18 #x3C #x00)
      ;; 109 m
      (#x00 #x00 #xCC #xFE #xD6 #xC6 #xC6 #x00)
      ;; 110 n
      (#x00 #x00 #x7C #x66 #x66 #x66 #x66 #x00)
      ;; 111 o
      (#x00 #x00 #x3C #x66 #x66 #x66 #x3C #x00)
      ;; 112 p
      (#x00 #x00 #x7C #x66 #x66 #x7C #x60 #x60)
      ;; 113 q
      (#x00 #x00 #x3E #x66 #x66 #x3E #x06 #x06)
      ;; 114 r
      (#x00 #x00 #x7C #x66 #x60 #x60 #x60 #x00)
      ;; 115 s
      (#x00 #x00 #x3E #x60 #x3C #x06 #x7C #x00)
      ;; 116 t
      (#x30 #x30 #x7C #x30 #x30 #x30 #x1C #x00)
      ;; 117 u
      (#x00 #x00 #x66 #x66 #x66 #x66 #x3E #x00)
      ;; 118 v
      (#x00 #x00 #x66 #x66 #x66 #x3C #x18 #x00)
      ;; 119 w
      (#x00 #x00 #xC6 #xC6 #xD6 #xFE #x6C #x00)
      ;; 120 x
      (#x00 #x00 #x66 #x3C #x18 #x3C #x66 #x00)
      ;; 121 y
      (#x00 #x00 #x66 #x66 #x66 #x3E #x06 #x3C)
      ;; 122 z
      (#x00 #x00 #x7E #x0C #x18 #x30 #x7E #x00)
      ;; 123 {
      (#x0E #x18 #x18 #x70 #x18 #x18 #x0E #x00)
      ;; 124 |
      (#x18 #x18 #x18 #x18 #x18 #x18 #x18 #x00)
      ;; 125 }
      (#x70 #x18 #x18 #x0E #x18 #x18 #x70 #x00)
      ;; 126 ~
      (#x76 #xDC #x00 #x00 #x00 #x00 #x00 #x00))))

(defun emit-data-block (buf addr data-bytes)
  "Emit code to write DATA-BYTES (a list/vector of u8) at ADDR using STOSQ.
   Sets up RDI, CLD, then emits MOV RAX,imm64; STOSQ for each 8-byte chunk."
  (uefi-emit-mov-reg-imm64 buf +rdi+ addr)
  (mvm-emit-byte buf #xFC)  ; CLD
  (let* ((len (length data-bytes))
         (nqw (ceiling len 8)))
    (loop for i from 0 below nqw
          do (let ((qw 0))
               (loop for j from 0 below 8
                     for idx = (+ (* i 8) j)
                     when (< idx len)
                     do (setf qw (logior qw (ash (elt data-bytes idx) (* j 8)))))
               (uefi-emit-mov-reg-imm64 buf +rax+ qw)
               ;; stosq = REX.W STOSQ
               (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)))))

(defun emit-uefi-font-data (buf)
  "Emit code to write 8×8 bitmap font at 0x601000."
  ;; Each qword is one glyph (8 row bytes packed little-endian).
  (uefi-emit-mov-reg-imm64 buf +rdi+ +font-base+)
  (mvm-emit-byte buf #xFC)  ; CLD
  (dolist (qw *font-8x8-qwords*)
    (uefi-emit-mov-reg-imm64 buf +rax+ qw)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)))

;;; ============================================================
;;; PS/2 Scancode Tables
;;; ============================================================

(defun build-scancode-table (entries)
  "Build 128-byte table from list of (scancode . ascii) pairs."
  (let ((table (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dolist (e entries)
      (setf (aref table (car e)) (cdr e)))
    table))

(defvar *ps2-scancode-normal*
  (build-scancode-table
    '((#x01 . 27)                                             ; ESC
      (#x02 . 49) (#x03 . 50) (#x04 . 51) (#x05 . 52)       ; 1 2 3 4
      (#x06 . 53) (#x07 . 54) (#x08 . 55) (#x09 . 56)       ; 5 6 7 8
      (#x0A . 57) (#x0B . 48) (#x0C . 45) (#x0D . 61)       ; 9 0 - =
      (#x0E .  8) (#x0F .  9)                                 ; BS TAB
      (#x10 . 113) (#x11 . 119) (#x12 . 101) (#x13 . 114)   ; q w e r
      (#x14 . 116) (#x15 . 121) (#x16 . 117) (#x17 . 105)   ; t y u i
      (#x18 . 111) (#x19 . 112) (#x1A . 91) (#x1B . 93)     ; o p [ ]
      (#x1C . 13)                                             ; Enter
      (#x1E . 97) (#x1F . 115) (#x20 . 100) (#x21 . 102)    ; a s d f
      (#x22 . 103) (#x23 . 104) (#x24 . 106) (#x25 . 107)   ; g h j k
      (#x26 . 108) (#x27 . 59) (#x28 . 39) (#x29 . 96)      ; l ; ' `
      (#x2B . 92)                                             ; backslash
      (#x2C . 122) (#x2D . 120) (#x2E . 99) (#x2F . 118)    ; z x c v
      (#x30 . 98) (#x31 . 110) (#x32 . 109)                  ; b n m
      (#x33 . 44) (#x34 . 46) (#x35 . 47)                    ; , . /
      (#x37 . 42)                                             ; * (kp)
      (#x39 . 32))))                                          ; Space

(defvar *ps2-scancode-shifted*
  (build-scancode-table
    '((#x01 . 27)                                             ; ESC
      (#x02 . 33) (#x03 . 64) (#x04 . 35) (#x05 . 36)       ; ! @ # $
      (#x06 . 37) (#x07 . 94) (#x08 . 38) (#x09 . 42)       ; % ^ & *
      (#x0A . 40) (#x0B . 41) (#x0C . 95) (#x0D . 43)       ; ( ) _ +
      (#x0E .  8) (#x0F .  9)                                 ; BS TAB
      (#x10 . 81) (#x11 . 87) (#x12 . 69) (#x13 . 82)       ; Q W E R
      (#x14 . 84) (#x15 . 89) (#x16 . 85) (#x17 . 73)       ; T Y U I
      (#x18 . 79) (#x19 . 80) (#x1A . 123) (#x1B . 125)     ; O P { }
      (#x1C . 13)                                             ; Enter
      (#x1E . 65) (#x1F . 83) (#x20 . 68) (#x21 . 70)       ; A S D F
      (#x22 . 71) (#x23 . 72) (#x24 . 74) (#x25 . 75)       ; G H J K
      (#x26 . 76) (#x27 . 58) (#x28 . 34) (#x29 . 126)      ; L : " ~
      (#x2B . 124)                                            ; |
      (#x2C . 90) (#x2D . 88) (#x2E . 67) (#x2F . 86)       ; Z X C V
      (#x30 . 66) (#x31 . 78) (#x32 . 77)                    ; B N M
      (#x33 . 60) (#x34 . 62) (#x35 . 63)                    ; < > ?
      (#x39 . 32))))                                          ; Space

(defun emit-uefi-scancode-tables (buf)
  "Emit PS/2 scancode tables at 0x601800 (normal) and 0x601880 (shifted)."
  (emit-data-block buf +scan-normal+ *ps2-scancode-normal*)
  (emit-data-block buf +scan-shifted+ *ps2-scancode-shifted*)
  ;; Zero shift state
  (uefi-emit-store-imm32-abs32 buf +shift-state+ 0))

;;; ============================================================
;;; UEFI ConOut print (diagnostic — before ExitBootServices)
;;; ============================================================

(defun emit-uefi-conout-char (buf ch)
  "Print a single ASCII character via UEFI ConOut->OutputString.
   R13 = SystemTable. Preserves all non-volatile regs."
  ;; sub rsp, 40 (32 shadow + 8 string area — gives 16-byte alignment)
  (uefi-emit-sub-rsp buf 40)
  ;; Write UTF-16LE null-terminated string at [RSP+32]: char(2 bytes) + null(2 bytes)
  ;; mov dword [rsp+32], char_code  (upper 16 bits = 0 = null terminator)
  (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x44) (mvm-emit-byte buf #x24)
  (mvm-emit-byte buf 32) (mvm-emit-u32 buf ch)
  ;; RCX = ConOut = [R13 + 0x40]
  (uefi-emit-mov-reg-mem buf +rcx+ +r13+ #x40)
  ;; RDX = &string at RSP+32
  (uefi-emit-lea-rsp buf +rdx+ 32)
  ;; call [RCX + 0x08]  (OutputString)
  (uefi-emit-call-mem buf +rcx+ #x08)
  (uefi-emit-add-rsp buf 40))

(defun emit-uefi-conout-string (buf string)
  "Print a string via UEFI ConOut. STRING is a Lisp string."
  (loop for ch across string
        do (emit-uefi-conout-char buf (char-code ch))))

;;; ============================================================
;;; Keyboard LED toggle (diagnostic — works before and after EBS)
;;; ============================================================

(defun emit-uefi-set-kbd-leds (buf led-state)
  "Set keyboard LEDs. LED-STATE: bit0=ScrollLock, bit1=NumLock, bit2=CapsLock.
   Uses ports 0x60/0x64. Timeout-protected to avoid hanging."
  ;; Wait for controller input buffer empty (bit 1 of 0x64 = 0), with timeout
  ;; mov ecx, 0x10000
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf #x10000)
  ;; .wait1: in al, 0x64
  (let ((wait1 (mvm-buffer-position buf)))
    (mvm-emit-byte buf #xE4) (mvm-emit-byte buf #x64)
    ;; test al, 2
    (mvm-emit-byte buf #xA8) (mvm-emit-byte buf #x02)
    ;; loopnz wait1 (dec ecx + jnz if ZF=0 — but LOOPNZ checks both ECX and ZF)
    ;; Actually use: jz .ready1; dec ecx; jnz wait1
    ;; jz +4 (skip dec+jnz)
    (mvm-emit-byte buf #x74) (mvm-emit-byte buf 4)
    ;; dec ecx
    (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC9)
    ;; jnz wait1
    (mvm-emit-byte buf #x75)
    (mvm-emit-byte buf (logand (- wait1 (+ (mvm-buffer-position buf) 1)) #xFF)))

  ;; Send 0xED command (set LEDs)
  (mvm-emit-byte buf #xB0) (mvm-emit-byte buf #xED)
  (mvm-emit-byte buf #xE6) (mvm-emit-byte buf #x60)

  ;; Wait for controller output buffer full (bit 0 of 0x64 = 1) — ACK
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf #x10000)
  (let ((wait2 (mvm-buffer-position buf)))
    (mvm-emit-byte buf #xE4) (mvm-emit-byte buf #x64)
    (mvm-emit-byte buf #xA8) (mvm-emit-byte buf #x01)
    ;; jnz +4 (skip dec+jnz)
    (mvm-emit-byte buf #x75) (mvm-emit-byte buf 4)
    (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC9)
    (mvm-emit-byte buf #x75)
    (mvm-emit-byte buf (logand (- wait2 (+ (mvm-buffer-position buf) 1)) #xFF)))

  ;; Read ACK byte (discard)
  (mvm-emit-byte buf #xE4) (mvm-emit-byte buf #x60)

  ;; Wait for input buffer empty again
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf #x10000)
  (let ((wait3 (mvm-buffer-position buf)))
    (mvm-emit-byte buf #xE4) (mvm-emit-byte buf #x64)
    (mvm-emit-byte buf #xA8) (mvm-emit-byte buf #x02)
    (mvm-emit-byte buf #x74) (mvm-emit-byte buf 4)
    (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC9)
    (mvm-emit-byte buf #x75)
    (mvm-emit-byte buf (logand (- wait3 (+ (mvm-buffer-position buf) 1)) #xFF)))

  ;; Send LED state byte
  (mvm-emit-byte buf #xB0) (mvm-emit-byte buf led-state)
  (mvm-emit-byte buf #xE6) (mvm-emit-byte buf #x60))

;;; ============================================================
;;; PC speaker beep (diagnostic)
;;; ============================================================

(defun emit-uefi-beep (buf freq-divisor delay-count)
  "Emit PC speaker beep. freq-divisor = PIT divisor, delay-count = busy-wait loop count."
  ;; PIT channel 2, mode 3 (square wave), binary
  (emit-x64-out buf #x43 #xB6)
  ;; Frequency divisor
  (emit-x64-out buf #x42 (logand freq-divisor #xFF))
  (emit-x64-out buf #x42 (logand (ash freq-divisor -8) #xFF))
  ;; Enable speaker: in al, 0x61; or al, 3; out 0x61, al
  (mvm-emit-byte buf #xE4) (mvm-emit-byte buf #x61)
  (mvm-emit-byte buf #x0C) (mvm-emit-byte buf #x03)
  (mvm-emit-byte buf #xE6) (mvm-emit-byte buf #x61)
  ;; Delay: mov ecx, N; .L: dec ecx; jnz .L
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf delay-count)
  (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC9)   ; dec ecx
  (mvm-emit-byte buf #x75) (mvm-emit-byte buf #xFC)   ; jnz -4
  ;; Disable speaker: in al, 0x61; and al, 0xFC; out 0x61, al
  (mvm-emit-byte buf #xE4) (mvm-emit-byte buf #x61)
  (mvm-emit-byte buf #x24) (mvm-emit-byte buf #xFC)
  (mvm-emit-byte buf #xE6) (mvm-emit-byte buf #x61))

(defun emit-uefi-silence (buf delay-count)
  "Emit silent delay."
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf delay-count)
  (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC9)
  (mvm-emit-byte buf #x75) (mvm-emit-byte buf #xFC))

(defun emit-uefi-beep-pattern (buf)
  "Emit 2 short beeps (~1000Hz) as diagnostic: 'we're alive'."
  ;; 1193 = 1193180/1000 ≈ 1000Hz. Delay ~100ms each.
  (emit-uefi-beep buf 1193 #x8000000)
  (emit-uefi-silence buf #x4000000)
  (emit-uefi-beep buf 1193 #x8000000))

;;; ============================================================
;;; VGA text mode clear + cursor disable
;;; ============================================================

(defun emit-uefi-vga-clear (buf)
  "Clear VGA text screen (80x25) and disable hardware cursor."
  ;; Clear screen: fill 2000 cells with space (0x20) + bright white attr (0x0F)
  ;; Each STOSD writes 2 cells = 4 bytes: 0x0F200F20
  (uefi-emit-mov-reg-imm64 buf +rdi+ #xB8000)
  ;; mov eax, 0x0F200F20
  (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf #x0F200F20)
  ;; mov ecx, 1000
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf 1000)
  (mvm-emit-byte buf #xFC)   ; CLD
  (mvm-emit-byte buf #xF3)   ; REP
  (mvm-emit-byte buf #xAB)   ; STOSD

  ;; Disable VGA hardware cursor (blinking underscore)
  (emit-x64-out buf #x3D4 #x0A)   ; CRT register: cursor start
  (emit-x64-out buf #x3D5 #x20)   ; bit 5 = cursor disable

  ;; Zero VGA cursor position
  (uefi-emit-store-imm32-abs32 buf #x600130 0)
  (uefi-emit-store-imm32-abs32 buf #x600134 0))

;;; ============================================================
;;; The actual UEFI entry stub (rewritten cleanly)
;;; ============================================================

(defun emit-uefi-entry-stub (buf)
  "Emit UEFI entry point (MS x64 ABI).
   RCX=ImageHandle, RDX=SystemTable."

  (setf *uefi-lea-patch-pos* nil)
  (setf *uefi-size-patch-pos* nil)

  ;; ---- Save callee-saved registers (8 regs = 64 bytes, keeps RSP aligned) ----
  (dolist (r (list +rbx+ +rbp+ +rsi+ +rdi+ +r12+ +r13+ +r14+ +r15+))
    (uefi-emit-push buf r))

  ;; Save args to non-volatile regs
  (uefi-emit-mov-reg-reg buf +r12+ +rcx+)  ; R12 = ImageHandle
  (uefi-emit-mov-reg-reg buf +r13+ +rdx+)  ; R13 = SystemTable

  ;; R14 = BootServices
  (uefi-emit-mov-reg-mem buf +r14+ +r13+ +efi-st-boot-services+)

  ;; ---- Diagnostic: print "1" via ConOut (EFI app started) ----
  (emit-uefi-conout-char buf (char-code #\1))

  ;; ---- Query GOP for framebuffer info (before ExitBootServices) ----
  (emit-uefi-gop-query buf)

  ;; ---- Diagnostic: print "2" via ConOut (GOP query survived) ----
  (emit-uefi-conout-char buf (char-code #\2))

  ;; ---- Allocate stack frame ----
  ;; 16384 bytes for memory map + 32 bytes for vars + 48 bytes shadow/5th-arg = 16464
  ;; Round up to 16-byte alignment: 16464 -> 16464 (already aligned: 64+16464 = 16528 = 1033*16)
  (let ((frame-size 16464))
    (uefi-emit-sub-rsp buf frame-size)

    ;; Stack layout (offsets from RSP after allocation):
    ;; [RSP+0    .. RSP+47]    = shadow space (32) + 5th arg (8) + padding (8)
    ;; [RSP+48   .. RSP+79]    = vars: MapSize(8), MapKey(8), DescSize(8), DescVer(8)
    ;; [RSP+80   .. RSP+16463] = memory map buffer (16384 bytes)
    (let ((var-base 48)          ; offset to MapSize
          (map-base 80))         ; offset to map buffer

      ;; Init MapSize = 16384
      (uefi-emit-mov-reg-imm64 buf +rax+ 16384)
      (uefi-emit-mov-mem-reg buf +rsp+ var-base +rax+)

      ;; ---- Call GetMemoryMap ----
      ;; RCX = &MapSize
      (uefi-emit-lea-rsp buf +rcx+ var-base)
      ;; RDX = map buffer
      (uefi-emit-lea-rsp buf +rdx+ map-base)
      ;; R8 = &MapKey
      (uefi-emit-lea-rsp buf +r8+ (+ var-base 8))
      ;; R9 = &DescriptorSize
      (uefi-emit-lea-rsp buf +r9+ (+ var-base 16))
      ;; 5th arg [RSP+32] = &DescriptorVersion
      (uefi-emit-lea-rsp buf +rax+ (+ var-base 24))
      (uefi-emit-mov-mem-reg buf +rsp+ 32 +rax+)

      (uefi-emit-call-mem buf +r14+ +efi-bs-get-memory-map+)

      ;; ---- Diagnostic: print "3" via ConOut (about to ExitBootServices) ----
      ;; Note: ConOut call needs R13=SystemTable, which we reload from stack
      ;; Actually R13 was saved as non-volatile, and we haven't changed it
      ;; But the stack frame is different here. Use ConOut directly.
      ;; sub rsp, 40 for ConOut call (already inside frame, just need alignment)
      ;; Actually simpler: just skip ConOut here, the "12" vs "123" tells us enough.

      ;; ---- Call ExitBootServices ----
      ;; RCX = ImageHandle (R12)
      (uefi-emit-mov-reg-reg buf +rcx+ +r12+)
      ;; RDX = MapKey = [RSP + var-base + 8]
      (uefi-emit-mov-reg-mem buf +rdx+ +rsp+ (+ var-base 8))

      (uefi-emit-call-mem buf +r14+ +efi-bs-exit-boot-services+)

      ;; If EBS returned error, we should retry, but for now just continue.
      ;; On OVMF this typically succeeds on first try.

      ;; ---- No more UEFI calls. We own the machine. ----
      ;; CLI
      (mvm-emit-byte buf #xFA)

      ;; ---- Diagnostic: Caps Lock LED ON = past ExitBootServices ----
      ;; ---- Diagnostic beep: 2 short beeps = we got past ExitBootServices ----
      ;; Both are port I/O.  In an SNP build they are skipped: no #VC handler
      ;; of ours is installed yet (boot-uefi-snp.lisp).
      (unless *x64-snp-mode*
        (emit-uefi-set-kbd-leds buf 4)  ; bit 2 = Caps Lock
        (emit-uefi-beep-pattern buf))

      ;; Deallocate the UEFI frame
      (uefi-emit-add-rsp buf frame-size))

    ;; Pop callee-saved (we won't return to UEFI, but clean stack before our setup)
    (dolist (r (list +r15+ +r14+ +r13+ +r12+ +rdi+ +rsi+ +rbp+ +rbx+))
      (uefi-emit-pop buf r)))

  ;; ====== From here: no UEFI, we set up our own world ======

  ;; ---- Copy kernel data to 0x100000 ----
  ;; RSI = source (kernel data within PE image, RIP-relative)
  (setf *uefi-lea-patch-pos* (uefi-emit-lea-rip-disp32 buf +rsi+))

  ;; RDI = destination = 0x100000
  (uefi-emit-mov-reg-imm64 buf +rdi+ +x64-kernel-load-addr+)

  ;; ECX = size (patched by assembler; use 32-bit mov for up to 4GB)
  ;; mov ecx, imm32
  (mvm-emit-byte buf #xB9)
  (setf *uefi-size-patch-pos* (mvm-buffer-position buf))
  (mvm-emit-u32 buf 0)  ; placeholder

  ;; rep movsb
  (mvm-emit-byte buf #xFC)   ; CLD (clear direction flag, just in case)
  (mvm-emit-byte buf #xF3)   ; REP
  (mvm-emit-byte buf #xA4)   ; MOVSB

  ;; ---- Emit font data and scancode tables to fixed addresses ----
  (when *uefi-console-tables*
    (emit-uefi-font-data buf)
    (emit-uefi-scancode-tables buf))

  ;; ---- SEV-SNP: detect, learn the C-bit (boot-uefi-snp.lisp) ----
  (when *x64-snp-mode* (emit-snp-detect buf))

  ;; ---- Page tables at +x64-page-tables-addr+: identity-map first 4GB ----
  ;; Clear 28KB (7 pages: PML4 + PDPT + 4×PD)
  (uefi-emit-mov-reg-imm64 buf +rdi+ +x64-page-tables-addr+)
  (uefi-emit-mov-reg-imm64 buf +rcx+ (* 7 4096))
  (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)  ; xor eax, eax
  (mvm-emit-byte buf #xFC)   ; CLD
  (mvm-emit-byte buf #xF3)   ; REP
  (mvm-emit-byte buf #xAA)   ; STOSB
  ;; SNP: RBX = C-bit mask (0 when not active), OR'd into every entry below
  (when *x64-snp-mode* (emit-snp-load-cbit-rbx buf))

  ;; PML4[0] = &PDPT | 3 (present + writable)
  (uefi-emit-mov-reg-imm64 buf +rax+ (+ +x64-page-tables-addr+ #x1003))
  (when *x64-snp-mode* (emit-snp-or-rax-rbx buf))
  (uefi-emit-mov-reg-imm64 buf +rdi+ +x64-page-tables-addr+)
  ;; mov [rdi], rax
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)

  ;; PDPT[0..3] = &PD[0..3] | 3
  (dotimes (i 4)
    (uefi-emit-mov-reg-imm64 buf +rax+ (+ +x64-page-tables-addr+ #x2000 (* i #x1000) 3))
    (when *x64-snp-mode* (emit-snp-or-rax-rbx buf))
    ;; mov [rdi + 0x1000 + i*8], rax
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89)
    (mvm-emit-byte buf #x87)  ; ModRM: mod=10, reg=rax, rm=rdi
    (mvm-emit-u32 buf (+ #x1000 (* i 8))))

  ;; Fill PDs: 2048 entries, each 2MB page with flags 0x83
  (uefi-emit-mov-reg-imm64 buf +rdi+ (+ +x64-page-tables-addr+ #x2000))
  (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xD2)   ; xor edx, edx (phys addr = 0)
  (uefi-emit-mov-reg-imm64 buf +rcx+ 2048)

  (let ((loop-start (mvm-buffer-position buf)))
    ;; mov rax, rdx; or rax, 0x83
    (uefi-emit-mov-reg-reg buf +rax+ +rdx+)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x0D)  ; or rax, imm32
    (mvm-emit-u32 buf #x83)
    ;; SNP: C-bit on every entry except the shared 2MB page
    (when *x64-snp-mode* (emit-snp-pd-entry-fixup buf))
    ;; mov [rdi], rax
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x07)
    ;; add rdi, 8
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x83) (mvm-emit-byte buf #xC7) (mvm-emit-byte buf 8)
    ;; add rdx, 0x200000
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x81) (mvm-emit-byte buf #xC2)
    (mvm-emit-u32 buf #x200000)
    ;; dec rcx
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC9)
    ;; jnz loop_start (JNZ rel32 = 6 bytes: 0F 85 + disp32)
    ;; disp32 relative to end of instruction = pos + 6
    (let ((rel (- loop-start (+ (mvm-buffer-position buf) 6))))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x85)  ; JNZ rel32
      (mvm-emit-u32 buf (logand rel #xFFFFFFFF))))

  ;; Load CR3 (SNP: the PML4 page is encrypted, so CR3 carries the C-bit too)
  (uefi-emit-mov-reg-imm64 buf +rax+ +x64-page-tables-addr+)
  (when *x64-snp-mode* (emit-snp-or-rax-rbx buf))
  (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x22) (mvm-emit-byte buf #xD8) ; mov cr3, rax

  ;; ---- GDT at 0x600 ----
  (uefi-emit-mov-reg-imm64 buf +rdi+ #x600)

  ;; Entry 0: null (8 zero bytes)
  (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)  ; xor eax, eax
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)  ; stosq

  (ecase *uefi-gdt-layout*
    (:toy
     ;; Entry 1 (sel 0x08): 64-bit code = 0x00AF9A000000FFFF
     (uefi-emit-mov-reg-imm64 buf +rax+ #x00AF9A000000FFFF)
     (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)
     ;; Entry 2 (sel 0x10): data = 0x00CF92000000FFFF
     (uefi-emit-mov-reg-imm64 buf +rax+ #x00CF92000000FFFF)
     (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB))
    (:boot-x64
     ;; Entry 1 (sel 0x08): 32-bit code (unused after this stub)
     (uefi-emit-mov-reg-imm64 buf +rax+ #x00CF9A000000FFFF)
     (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)
     ;; Entry 2 (sel 0x10): 64-bit code — boot-x64's kernel selector
     (uefi-emit-mov-reg-imm64 buf +rax+ #x00AF9A000000FFFF)
     (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)))

  ;; GDTR at 0x620: limit(2) + base(8)
  ;; mov word [0x620], 23
  (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x04)
  (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #x620) (mvm-emit-u16 buf 23)
  ;; mov qword [0x622], 0x600
  (uefi-emit-mov-reg-imm64 buf +rax+ #x600)
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x04)
  (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #x622)

  ;; lgdt [0x620]
  (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x01) (mvm-emit-byte buf #x14)
  (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #x620)

  ;; Far return to reload CS with selector 0x08
  ;; push 0x08; lea rax, [rip + N]; push rax; retfq
  ;; N = 1 (push rax) + 2 (retfq 48 CB) = 3
  (mvm-emit-byte buf #x6A) (mvm-emit-byte buf (uefi-code-selector))  ; push code selector
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8D) (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf 3)     ; skip: push(1) + lretq(2) = 3
  (uefi-emit-push buf +rax+)
  ;; retfq
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xCB)

  ;; Reload data segments: 0x10 (toy layout) or NULL (boot-x64 layout, where
  ;; 0x10 is code; null SS/DS/ES are legal at CPL0 in 64-bit mode and are
  ;; exactly what emit-x64-kernel64-entry loads itself)
  (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xB8)
  (mvm-emit-u16 buf (if (eq *uefi-gdt-layout* :toy) #x10 0))
  (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xD8)  ; mov ds, ax
  (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xC0)  ; mov es, ax
  (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xD0)  ; mov ss, ax
  ;; Zero FS/GS
  (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)   ; xor eax, eax
  (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xE0)  ; mov fs, ax
  (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xE8)  ; mov gs, ax

  ;; ---- SEV-SNP: shared region, GHCB, #VC handler + IDT ----
  (when *x64-snp-mode* (emit-snp-post-cr3 buf (uefi-code-selector)))

  ;; ---- Stack at 0x800000 ----
  (uefi-emit-mov-reg-imm64 buf +rsp+ +x64-stack-top+)

  ;; ---- Serial (COM1 0x3F8, 115200 8N1) ----
  (emit-x64-out buf #x3F9 #x00)   ; disable interrupts
  (emit-x64-out buf #x3FB #x80)   ; DLAB on
  (emit-x64-out buf #x3F8 #x01)   ; divisor low = 1 (115200)
  (emit-x64-out buf #x3F9 #x00)   ; divisor high = 0
  (emit-x64-out buf #x3FB #x03)   ; 8N1, DLAB off
  (emit-x64-out buf #x3FA #xC7)   ; enable FIFO
  ;; SNP :TEST — drive port I/O through the #VC handler (prints VC+wi5)
  (when *x64-snp-mode* (emit-snp-selftest buf))

  ;; ---- Runtime registers ----
  ;; R15 = NIL
  (uefi-emit-mov-reg-imm64 buf +r15+ #xDEAD0001)
  ;; R12 = alloc pointer
  (uefi-emit-mov-reg-imm64 buf +r12+ #x10000000)
  ;; R14 = alloc limit
  (uefi-emit-mov-reg-imm64 buf +r14+ #x1E000000)
  ;; RBP = RSP
  (uefi-emit-mov-reg-reg buf +rbp+ +rsp+)

  ;; ---- Clear framebuffer (if GOP found) ----
  ;; Check fb_valid; skip if 0
  ;; mov eax, [0x600120]
  (when *uefi-console-tables*
  (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
  (mvm-emit-u32 buf +fb-valid-addr+)
  ;; test eax, eax
  (mvm-emit-byte buf #x85) (mvm-emit-byte buf #xC0)
  ;; JZ rel32 — skip clear
  (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x84)
  (let ((jz-patch (mvm-buffer-position buf)))
    (mvm-emit-u32 buf 0)  ; placeholder

    ;; RDI = fb_base >> 1  (pre-tagged → raw address)
    ;; mov rdi, [0x600100]
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x3C)
    (mvm-emit-byte buf #x25) (mvm-emit-u32 buf +fb-info-base+)
    ;; shr rdi, 1
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xD1) (mvm-emit-byte buf #xEF)

    ;; Compute qword count: ppsl * text_rows * 4
    ;; (ppsl * text_rows * 8 pixel_rows * 4 bytes/pixel) / 8 bytes/qword = ppsl * text_rows * 4
    ;; mov eax, [0x600108]  ; ppsl
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf +fb-ppsl-addr+)
    ;; mov edx, [0x60011C]  ; text_rows
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x14) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf +fb-trows-addr+)
    ;; imul eax, edx
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #xAF) (mvm-emit-byte buf #xC2)
    ;; shl eax, 2  (×4)
    (mvm-emit-byte buf #xC1) (mvm-emit-byte buf #xE0) (mvm-emit-byte buf 2)
    ;; mov ecx, eax
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC1)

    ;; xor eax, eax; cld; rep stosq
    (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)
    (mvm-emit-byte buf #xFC)
    (mvm-emit-byte buf #xF3) (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)

    ;; Patch JZ target
    (let ((target (mvm-buffer-position buf)))
      (setf (aref (mvm-buffer-bytes buf) (+ jz-patch 0)) (ldb (byte 8 0) (- target (+ jz-patch 4))))
      (setf (aref (mvm-buffer-bytes buf) (+ jz-patch 1)) (ldb (byte 8 8) (- target (+ jz-patch 4))))
      (setf (aref (mvm-buffer-bytes buf) (+ jz-patch 2)) (ldb (byte 8 16) (- target (+ jz-patch 4))))
      (setf (aref (mvm-buffer-bytes buf) (+ jz-patch 3)) (ldb (byte 8 24) (- target (+ jz-patch 4))))))

  ;; ---- Clear VGA text screen + disable hardware cursor ----
  (emit-uefi-vga-clear buf))

  ;; ---- Diagnostic: all LEDs ON = about to jump to kernel ----
  (unless *x64-snp-mode*
    (emit-uefi-set-kbd-leds buf 7))  ; bits 0+1+2 = Scroll+Num+Caps

  ;; ---- Jump to kernel at 0x100000 (absolute) ----
  ;; Kernel data was copied to 0x100000 by rep movsb above.
  ;; Must use absolute jump — we're running at UEFI-chosen address.
  ;; mov rax, 0x100000
  (uefi-emit-mov-reg-imm64 buf +rax+ #x100000)
  ;; jmp rax
  (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xE0)
  ;; ---- Optional fixed-size pad (see *uefi-stub-pad*) ----
  (setf *uefi-kernel-data-start* nil)
  (when *uefi-stub-pad*
    (let ((n (mvm-buffer-position buf)))
      (when (> n *uefi-stub-pad*)
        (error "UEFI entry stub is ~D bytes, larger than *uefi-stub-pad* ~D" n *uefi-stub-pad*))
      (dotimes (i (- *uefi-stub-pad* n)) (mvm-emit-byte buf 0))
      (setf *uefi-kernel-data-start* *uefi-stub-pad*)))
  )

(defun uefi-code-selector ()
  (ecase *uefi-gdt-layout* (:toy #x08) (:boot-x64 #x10)))

;;; ============================================================
;;; Boot Descriptor
;;; ============================================================

(defun uefi-x64-boot-descriptor ()
  "Return the UEFI x86-64 boot descriptor for image building."
  (list :arch :x86-64
        :uefi t
        :entry-fn #'emit-uefi-entry-stub
        :load-addr +x64-kernel-load-addr+
        :stack-top +x64-stack-top+
        :cons-base +x64-cons-base+
        :general-base +x64-general-base+))

;;; ============================================================
;;; UEFI + the REAL CL image (build-x64-cl-repl via OVMF)
;;; ============================================================
;;; The stub above, then boot-x64's 64-bit kernel entry (serial, GC metadata,
;;; MCGC config, IDT/PIC/PIT), then cross.lisp's JMP + native code — i.e. the
;;; multiboot image's layout from its 64-bit entry onward, so the CL image
;;; boots via OVMF (and, later, is MEASURED by AmdSev OVMF's -kernel path).
;;;
;;; Address arithmetic: the stub is padded to +UEFI-CL-STUB-PAD+ bytes and
;;; copies everything after itself to 0x100000, so runtime VA = 0x100000 +
;;; (image offset - pad).  cross.lisp computes VAs as load-addr + offset, hence
;;; :load-addr = 0x100000 - pad (the "wrap header" is the stub).

(defconstant +uefi-cl-stub-pad+ #x2000
  "Stub slot size.  Multiple of 16 (constant-pool alignment) and generous:
   the stub is ~3 KB with SNP :test, ~2.7 KB without.")

(defun emit-uefi-cl-entry (buf)
  (let ((*uefi-gdt-layout* :boot-x64)
        (*uefi-console-tables* nil)
        (*uefi-stub-pad* +uefi-cl-stub-pad+))
    (emit-uefi-entry-stub buf))
  ;; boot-x64's 64-bit entry, at runtime VA 0x100000.  Its own NOP padding
  ;; aligns (buffer length + 5) to 16; the stub pad is a multiple of 16, so
  ;; the alignment it computes on the whole buffer holds at runtime too.
  (emit-x64-kernel64-entry buf))

(defun uefi-cl-preamble-length ()
  "Bytes from 0x100000 to the JMP cross.lisp emits: the kernel64 entry alone.
   Build scripts set *x64-native-code-offset* to this + 5."
  (let ((buf (make-mvm-buffer)))
    (emit-uefi-cl-entry buf)
    (- (mvm-buffer-position buf) +uefi-cl-stub-pad+)))

(defun uefi-x64-cl-boot-descriptor ()
  (list :arch :x86-64
        :uefi t
        :entry-fn #'emit-uefi-cl-entry
        :ap-trampoline-fn #'emit-x64-ap-trampoline
        :serial-init-fn #'x64-init-serial
        :smp-sequence-fn #'x64-init-smp-sequence
        :percpu-layout-fn #'x64-percpu-layout
        :load-addr (- +x64-kernel-load-addr+ +uefi-cl-stub-pad+)
        :stack-top (x64-effective-stack-top)
        :cons-base +x64-cons-base+
        :general-base +x64-general-base+))

;;; ============================================================
;;; Multiboot + Console Boot Descriptor
;;; ============================================================
;;; For GRUB-booted x86-64 with VGA text mode + PS/2 keyboard.
;;; Reuses the standard multiboot boot (boot-x64.lisp) but adds
;;; font data, scancode tables, and VGA/framebuffer init.
;;; Detects GRUB framebuffer from multiboot info (essential for
;;; UEFI-booted GRUB where VGA text mode is unavailable).

(defun emit-x64-patch-jmp32 (buf positions target-pos)
  "Patch a list of (jmp-instruction-start . instruction-length) pairs to jump to target-pos."
  (let ((bytes (mvm-buffer-bytes buf)))
    (dolist (entry positions)
      (let* ((insn-start (car entry))
             (insn-len (cdr entry))
             (disp32-offset (- insn-len 4))
             (rel (- target-pos (+ insn-start insn-len))))
        (setf (aref bytes (+ insn-start disp32-offset 0)) (ldb (byte 8  0) rel))
        (setf (aref bytes (+ insn-start disp32-offset 1)) (ldb (byte 8  8) rel))
        (setf (aref bytes (+ insn-start disp32-offset 2)) (ldb (byte 8 16) rel))
        (setf (aref bytes (+ insn-start disp32-offset 3)) (ldb (byte 8 24) rel))))))

(defun emit-x64-multiboot-fb-detect (buf)
  "Detect framebuffer from multiboot info structure.
   If GRUB provides a 32bpp RGB framebuffer, populate fb_* memory.
   Otherwise set fb_valid=0.  Boot32 saved multiboot info pointer at [0x500]."
  (let (no-fb-jumps)
    ;; mov eax, [0x500]  — multiboot info pointer
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf #x500)
    ;; test eax, eax
    (mvm-emit-byte buf #x85) (mvm-emit-byte buf #xC0)
    ;; jz no_fb
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x84) (mvm-emit-u32 buf 0)
      (push (cons p 6) no-fb-jumps))

    ;; Read flags: mov ecx, [rax]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x08)
    ;; bt ecx, 12 (test framebuffer info bit)
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #xBA)
    (mvm-emit-byte buf #xE1) (mvm-emit-byte buf 12)
    ;; jnc no_fb
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x83) (mvm-emit-u32 buf 0)
      (push (cons p 6) no-fb-jumps))

    ;; Check framebuffer_type == 1 (RGB direct color)
    ;; movzx edx, byte [rax+109]
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #xB6)
    (mvm-emit-byte buf #x50) (mvm-emit-byte buf 109)
    ;; cmp edx, 1
    (mvm-emit-byte buf #x83) (mvm-emit-byte buf #xFA) (mvm-emit-byte buf 1)
    ;; jne no_fb
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x85) (mvm-emit-u32 buf 0)
      (push (cons p 6) no-fb-jumps))

    ;; Check framebuffer_bpp == 32
    ;; movzx edx, byte [rax+108]
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #xB6)
    (mvm-emit-byte buf #x50) (mvm-emit-byte buf 108)
    ;; cmp edx, 32
    (mvm-emit-byte buf #x83) (mvm-emit-byte buf #xFA) (mvm-emit-byte buf 32)
    ;; jne no_fb
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x85) (mvm-emit-u32 buf 0)
      (push (cons p 6) no-fb-jumps))

    ;; === Valid 32bpp RGB framebuffer ===
    ;; fb_base = framebuffer_addr << 1 (pre-tagged for :u64 load)
    ;; mov rdi, [rax+88]
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8B)
    (mvm-emit-byte buf #x78) (mvm-emit-byte buf 88)
    ;; shl rdi, 1
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xD1) (mvm-emit-byte buf #xE7)
    ;; mov [0x600100], rdi
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89)
    (mvm-emit-byte buf #x3C) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf #x600100)

    ;; ppsl = framebuffer_pitch / 4  (bytes → pixels)
    ;; mov esi, [rax+96]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x70) (mvm-emit-byte buf 96)
    ;; shr esi, 2
    (mvm-emit-byte buf #xC1) (mvm-emit-byte buf #xEE) (mvm-emit-byte buf 2)
    ;; mov [0x600108], esi
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x34) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf #x600108)

    ;; text_cols = width / 8
    ;; mov edx, [rax+100]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x50) (mvm-emit-byte buf 100)
    ;; shr edx, 3
    (mvm-emit-byte buf #xC1) (mvm-emit-byte buf #xEA) (mvm-emit-byte buf 3)
    ;; mov [0x600118], edx
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x14) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf #x600118)

    ;; text_rows = height / 8
    ;; mov ecx, [rax+104]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x48) (mvm-emit-byte buf 104)
    ;; shr ecx, 3
    (mvm-emit-byte buf #xC1) (mvm-emit-byte buf #xE9) (mvm-emit-byte buf 3)
    ;; mov [0x60011C], ecx
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x0C) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf #x60011C)

    ;; cursor_x = 0, cursor_y = 0
    (uefi-emit-store-imm32-abs32 buf #x600110 0)
    (uefi-emit-store-imm32-abs32 buf #x600114 0)
    ;; fb_valid = 1
    (uefi-emit-store-imm32-abs32 buf +fb-valid-addr+ 1)

    ;; Clear framebuffer: zero all pixels
    ;; mov rdi, [rax+88]  (raw fb addr)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x8B)
    (mvm-emit-byte buf #x78) (mvm-emit-byte buf 88)
    ;; pitch * height = total bytes; mov esi, [rax+96]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x70) (mvm-emit-byte buf 96)
    ;; mov ecx, [rax+104]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x48) (mvm-emit-byte buf 104)
    ;; imul ecx, esi
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #xAF) (mvm-emit-byte buf #xCE)
    ;; shr ecx, 3  (bytes → qwords)
    (mvm-emit-byte buf #xC1) (mvm-emit-byte buf #xE9) (mvm-emit-byte buf 3)
    ;; xor eax, eax
    (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)
    ;; CLD; rep stosq
    (mvm-emit-byte buf #xFC)
    (mvm-emit-byte buf #xF3) (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB)

    ;; jmp after_fb
    (let ((jmp-pos (mvm-buffer-position buf)))
      (mvm-emit-byte buf #xE9) (mvm-emit-u32 buf 0)

      ;; no_fb: set fb_valid = 0
      (let ((no-fb-pos (mvm-buffer-position buf)))
        (uefi-emit-store-imm32-abs32 buf +fb-valid-addr+ 0)

        ;; after_fb:
        (let ((after-fb-pos (mvm-buffer-position buf)))
          ;; Patch all conditional jumps → no_fb
          (emit-x64-patch-jmp32 buf no-fb-jumps no-fb-pos)
          ;; Patch jmp → after_fb
          (emit-x64-patch-jmp32 buf (list (cons jmp-pos 5)) after-fb-pos))))))

(defun emit-pci-config-read32 (buf config-addr)
  "Emit code to read 32-bit PCI config register. Result in EAX.
   config-addr = 0x80000000 | (bus<<16) | (dev<<11) | (fn<<8) | offset."
  ;; mov eax, config-addr
  (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf config-addr)
  ;; mov dx, 0x0CF8; out dx, eax (32-bit)
  (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x0CF8)
  (mvm-emit-byte buf #xEF)
  ;; mov dx, 0x0CFC; in eax, dx (32-bit)
  (mvm-emit-byte buf #x66) (mvm-emit-byte buf #xBA) (mvm-emit-u16 buf #x0CFC)
  (mvm-emit-byte buf #xED))

(defun emit-x64-pci-fb-detect (buf)
  "Detect Intel HD Graphics and set up framebuffer.
   v31e: Fixed BDSM offset (0xB0, was 0xB8=TSEGMB).
   Writes white to: BDSM+DSPASURF, BAR2+DSPASURF, BDSM+0, BAR2+0.
   Does NOT reprogram display registers — GRUB's pipeline stays intact.
   Diagnostic: step beeps at 1000Hz, DSPASURF[31:28]+1 beeps at 2000Hz."
  (let (skip-jumps)
    ;; Init step counter = 0
    (uefi-emit-store-imm32-abs32 buf #x600150 0)

    ;; Skip if framebuffer already detected (from multiboot info)
    (uefi-emit-load-eax-abs32 buf #x600120)
    (mvm-emit-byte buf #x85) (mvm-emit-byte buf #xC0)    ; test eax, eax
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x85) (mvm-emit-u32 buf 0)
      (push (cons p 6) skip-jumps))                        ; jnz skip
    ;; Step 1: fb_valid was 0 (good, proceeding)
    (uefi-emit-store-imm32-abs32 buf #x600150 1)

    ;; Check Intel GPU exists at bus 0, dev 2, fn 0
    (emit-pci-config-read32 buf #x80001000)
    (uefi-emit-store-eax-abs32 buf #x600154)               ; save vendor/device
    (mvm-emit-byte buf #x66) (mvm-emit-byte buf #x3D) (mvm-emit-u16 buf #x8086)
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x85) (mvm-emit-u32 buf 0)
      (push (cons p 6) skip-jumps))
    ;; Step 2: vendor is Intel
    (uefi-emit-store-imm32-abs32 buf #x600150 2)

    ;; Read BAR0 (MMIO) at GPU bus 0/dev 2/fn 0/offset 0x10
    (emit-pci-config-read32 buf #x80001010)
    (uefi-emit-store-eax-abs32 buf #x600158)
    (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #xFFFFFFF0)
    (mvm-emit-byte buf #x85) (mvm-emit-byte buf #xC0)
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x84) (mvm-emit-u32 buf 0)
      (push (cons p 6) skip-jumps))
    ;; r10 = MMIO base
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC2)
    ;; Step 3: BAR0 non-zero
    (uefi-emit-store-imm32-abs32 buf #x600150 3)

    ;; Read BAR2 (GTT aperture) at offset 0x18
    (emit-pci-config-read32 buf #x80001018)
    (uefi-emit-store-eax-abs32 buf #x60015C)               ; save raw BAR2
    (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #xFFFFFFF0)
    ;; r11 = aperture base (may be 0 if not assigned)
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC3)

    ;; Read BDSM from MCH (bus 0/dev 0/fn 0/offset 0xB0) — FIXED! was 0xB8 (TSEGMB)
    (emit-pci-config-read32 buf #x800000B0)
    (uefi-emit-store-eax-abs32 buf #x600160)               ; save raw BDSM
    (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #xFFF00000)
    (mvm-emit-byte buf #x85) (mvm-emit-byte buf #xC0)
    (let ((p (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x84) (mvm-emit-u32 buf 0)
      (push (cons p 6) skip-jumps))
    ;; r13 = BDSM (physical stolen memory base)
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC5)
    ;; Step 4: BDSM non-zero (now reading correct register!)
    (uefi-emit-store-imm32-abs32 buf #x600150 4)

    ;; Save current display registers for diagnostics
    ;; DSPASURF at MMIO + 0x7019C
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x7019C)
    (uefi-emit-store-eax-abs32 buf #x600164)               ; save DSPASURF
    ;; DSPACNTR at MMIO + 0x70180
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x70180)
    (uefi-emit-store-eax-abs32 buf #x600168)               ; save DSPACNTR
    ;; DSPALINOFF at MMIO + 0x70184
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x70184)
    (uefi-emit-store-eax-abs32 buf #x60016C)               ; save DSPALINOFF
    ;; DSPASTRIDE at MMIO + 0x70188
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x70188)
    (uefi-emit-store-eax-abs32 buf #x600170)               ; save DSPASTRIDE
    ;; PIPEASRC at MMIO + 0x6001C
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x6001C)
    (uefi-emit-store-eax-abs32 buf #x600174)               ; save PIPEASRC
    ;; DSPASURFLIVE at MMIO + 0x701AC (actual surface address GPU is reading NOW)
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x701AC)
    (uefi-emit-store-eax-abs32 buf #x600178)               ; save DSPASURFLIVE
    ;; Step 5: registers saved
    (uefi-emit-store-imm32-abs32 buf #x600150 5)

    ;; === PHASE 1: Fill 8MB white at BDSM + DSPASURF ===
    ;; DSPASURF is a GGTT offset. With identity mapping in stolen memory,
    ;; the physical address is BDSM + DSPASURF.
    (uefi-emit-load-eax-abs32 buf #x600164)                ; eax = DSPASURF
    ;; mov edi, eax  (DSPASURF)
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC7)
    ;; add rdi, r13  (BDSM + DSPASURF)
    (mvm-emit-byte buf #x4C) (mvm-emit-byte buf #x01) (mvm-emit-byte buf #xEF)
    (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf #xFFFFFFFF)  ; mov eax, white
    (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf #x200000)    ; mov ecx, 2M dwords (8MB)
    (mvm-emit-byte buf #xFC)                                 ; cld
    (mvm-emit-byte buf #xF3)
    (mvm-emit-byte buf #xAB)                                 ; rep stosd
    ;; Step 6: BDSM+DSPASURF fill done
    (uefi-emit-store-imm32-abs32 buf #x600150 6)

    ;; === PHASE 2: Fill 8MB white at BAR2 + DSPASURF (through GTT aperture) ===
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x85)
    (mvm-emit-byte buf #xDB)                                 ; test r11d, r11d
    (let ((skip-bar2a-pos (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x74) (mvm-emit-byte buf 0)        ; jz skip_bar2a
      (uefi-emit-load-eax-abs32 buf #x600164)               ; eax = DSPASURF
      ;; mov edi, eax
      (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC7)
      ;; add rdi, r11  (BAR2 + DSPASURF)
      (mvm-emit-byte buf #x4C) (mvm-emit-byte buf #x01) (mvm-emit-byte buf #xDF)
      (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf #xFFFFFFFF)
      (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf #x200000)
      (mvm-emit-byte buf #xFC)
      (mvm-emit-byte buf #xF3)
      (mvm-emit-byte buf #xAB)                               ; rep stosd
      ;; Patch jz skip_bar2a
      (let ((bytes (mvm-buffer-bytes buf)))
        (setf (aref bytes (1+ skip-bar2a-pos))
              (logand #xFF (- (mvm-buffer-position buf) skip-bar2a-pos 2)))))
    ;; Step 7: BAR2+DSPASURF fill done
    (uefi-emit-store-imm32-abs32 buf #x600150 7)

    ;; === PHASE 3: Fill 4MB white at BDSM+0 (start of stolen memory) ===
    ;; In case DSPASURF=0 (this overlaps with phase 1, that's fine)
    (mvm-emit-byte buf #x4C) (mvm-emit-byte buf #x89)
    (mvm-emit-byte buf #xEF)                                 ; mov rdi, r13 (BDSM)
    (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf #xFFFFFFFF)  ; mov eax, white
    (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf #x100000)    ; mov ecx, 1M dwords (4MB)
    (mvm-emit-byte buf #xFC)                                 ; cld
    (mvm-emit-byte buf #xF3)
    (mvm-emit-byte buf #xAB)                                 ; rep stosd

    ;; === PHASE 4: Fill 4MB white at BAR2+0 (aperture start) ===
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x85)
    (mvm-emit-byte buf #xDB)                                 ; test r11d, r11d
    (let ((skip-bar2b-pos (mvm-buffer-position buf)))
      (mvm-emit-byte buf #x74) (mvm-emit-byte buf 0)        ; jz skip_bar2b
      (mvm-emit-byte buf #x4C) (mvm-emit-byte buf #x89)
      (mvm-emit-byte buf #xDF)                               ; mov rdi, r11 (BAR2)
      (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf #xFFFFFFFF)
      (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf #x100000)  ; 4MB
      (mvm-emit-byte buf #xFC)
      (mvm-emit-byte buf #xF3)
      (mvm-emit-byte buf #xAB)                               ; rep stosd
      ;; Patch jz skip_bar2b
      (let ((bytes (mvm-buffer-bytes buf)))
        (setf (aref bytes (1+ skip-bar2b-pos))
              (logand #xFF (- (mvm-buffer-position buf) skip-bar2b-pos 2)))))

    ;; WBINVD: flush all caches so GPU sees our writes
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x09)
    ;; Step 8: all fills + flush done
    (uefi-emit-store-imm32-abs32 buf #x600150 8)

    ;; === Set up fb_info for kernel ===
    ;; fb_base = (BDSM + DSPASURF) << 1 (pre-tagged for MVM)
    ;; Kernel will write characters to where the display pipeline reads from
    (uefi-emit-load-eax-abs32 buf #x600164)                ; eax = DSPASURF
    ;; mov edi, eax
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC7)
    ;; add rdi, r13  (rdi = BDSM + DSPASURF)
    (mvm-emit-byte buf #x4C) (mvm-emit-byte buf #x01) (mvm-emit-byte buf #xEF)
    ;; shl rdi, 1  (pre-tag for MVM :u64 load)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xD1) (mvm-emit-byte buf #xE7)
    ;; mov [0x600100], rdi
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x89)
    (mvm-emit-byte buf #x3C) (mvm-emit-byte buf #x25)
    (mvm-emit-u32 buf #x600100)
    ;; Read stride from DSPASTRIDE at MMIO + 0x70188
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x70188)
    ;; ppsl = stride / 4
    (uefi-emit-shr-eax-imm buf 2)
    (uefi-emit-store-eax-abs32 buf #x600108)
    ;; Read resolution from PIPEASRC at MMIO + 0x6001C
    (mvm-emit-byte buf #x41) (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x82)
    (mvm-emit-u32 buf #x6001C)
    ;; width = (PIPEASRC >> 16) + 1, height = (PIPEASRC & 0xFFFF) + 1
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC1)      ; mov ecx, eax
    (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #xFFFF)     ; and eax, 0xFFFF
    (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC0)      ; inc eax (height)
    (uefi-emit-shr-eax-imm buf 3)                           ; / 8 = text_rows
    (uefi-emit-store-eax-abs32 buf #x60011C)
    (mvm-emit-byte buf #xC1) (mvm-emit-byte buf #xE9) (mvm-emit-byte buf 16) ; shr ecx, 16
    (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC1)      ; inc ecx (width)
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC8)      ; mov eax, ecx
    (uefi-emit-shr-eax-imm buf 3)                           ; / 8 = text_cols
    (uefi-emit-store-eax-abs32 buf #x600118)
    ;; cursor = 0, fb_valid = 1
    (uefi-emit-store-imm32-abs32 buf #x600110 0)
    (uefi-emit-store-imm32-abs32 buf #x600114 0)
    (uefi-emit-store-imm32-abs32 buf +fb-valid-addr+ 1)

    ;; All skip jumps land here
    (let ((skip-pos (mvm-buffer-position buf)))
      (emit-x64-patch-jmp32 buf skip-jumps skip-pos))))

(defun emit-x64-zero-globals (buf)
  "Zero critical memory regions that may contain garbage on real hardware.
   QEMU zeroes memory, real hardware does not."
  ;; Zero globals alist head at 0x600000 (8 bytes)
  ;; Same bug as Pi Zero 2W: uninitialized → crash on first defvar/lookup
  ;; mov qword [0x600000], 0
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xC7)
  (mvm-emit-byte buf #x04) (mvm-emit-byte buf #x25)
  (mvm-emit-u32 buf #x600000) (mvm-emit-u32 buf 0)
  ;; Zero the entire fb info region 0x600100-0x600140 (64 bytes = 8 qwords)
  (uefi-emit-mov-reg-imm64 buf +rdi+ #x600100)
  ;; xor rax, rax
  (mvm-emit-byte buf #x48) (mvm-emit-byte buf #x31) (mvm-emit-byte buf #xC0)
  ;; mov ecx, 8
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf 8)
  ;; CLD; rep stosq
  (mvm-emit-byte buf #xFC)
  (mvm-emit-byte buf #xF3) (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xAB))

(defun emit-x64-exc-isr (buf addr freq-lo freq-hi)
  "Write a minimal exception ISR at ADDR that beeps at given frequency and halts.
   ISR code: cli, set PIT ch2 frequency, enable speaker, hlt loop.
   22 bytes per ISR."
  (uefi-emit-mov-reg-imm64 buf +rdi+ addr)
  (dolist (b (list #xFA                        ; cli
              #xB0 #xB6 #xE6 #x43             ; PIT ch2 setup
              #xB0 freq-lo #xE6 #x42          ; freq low byte
              #xB0 freq-hi #xE6 #x42          ; freq high byte
              #xE4 #x61                        ; in al, 0x61
              #x0C #x03                        ; or al, 3
              #xE6 #x61                        ; out 0x61, al
              #xF4                             ; hlt
              #xEB #xFD))                      ; jmp -3
    (mvm-emit-byte buf #xC6) (mvm-emit-byte buf #x07) (mvm-emit-byte buf b)
    (mvm-emit-byte buf #x48) (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC7)))

(defun emit-x64-idt-entry (buf idt-base vec isr-addr)
  "Install one IDT entry for vector VEC pointing to ISR-ADDR."
  (let* ((selector #x10)
         (type-attr #x8E)
         (entry-addr (+ idt-base (* vec 16)))
         (word0 (logior (logand isr-addr #xFFFF) (ash selector 16)))
         (word1 (logior (ash type-attr 8) (ash (logand (ash isr-addr -16) #xFFFF) 16))))
    (uefi-emit-mov-reg-imm64 buf +rdi+ entry-addr)
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x07) (mvm-emit-u32 buf word0)
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x04)
    (mvm-emit-u32 buf word1)
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x08)
    (mvm-emit-u32 buf 0)
    (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x47) (mvm-emit-byte buf #x0C)
    (mvm-emit-u32 buf 0)))

(defun emit-x64-exception-handlers (buf)
  "Install CPU exception handlers — unique frequency per vector for spectrograph.
   Vector N gets frequency = 200 + N*50 Hz (range: 200Hz to 1750Hz).
   Each ISR is 22 bytes, placed at 0x4F0900 + N*32."
  (let ((idt-base #x4F0000))
    ;; Emit 32 ISRs, each at a unique address with a unique frequency
    (dotimes (vec 32)
      (let* ((isr-addr (+ #x4F0900 (* vec 32)))
             ;; freq = 200 + vec*50 Hz; PIT divisor = 1193182 / freq
             (freq (+ 200 (* vec 50)))
             (divisor (round 1193182 freq))
             (div-lo (logand divisor #xFF))
             (div-hi (logand (ash divisor -8) #xFF)))
        (emit-x64-exc-isr buf isr-addr div-lo div-hi)
        (emit-x64-idt-entry buf idt-base vec isr-addr)))))

(defun emit-x64-disable-mce (buf)
  "Disable Machine Check Exceptions by clearing CR4.MCE (bit 6).
   UEFI firmware on real hardware (ThinkPad T420) leaves MCE enabled with
   pending errors in MC banks, causing spurious #MC (vector 18) exceptions."
  ;; mov rax, cr4
  (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x20) (mvm-emit-byte buf #xE0)
  ;; and eax, ~(1<<6) = 0xFFFFFFBF — clear MCE bit
  (mvm-emit-byte buf #x25) (mvm-emit-u32 buf #xFFFFFFBF)
  ;; mov cr4, rax
  (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x22) (mvm-emit-byte buf #xE0))

(defun emit-x64-console-multiboot-header (buf)
  "Multiboot1 header requesting 32bpp framebuffer for console display.
   48 bytes: standard 32-byte header + 16-byte video mode request."
  (let* ((magic #x1BADB002)
         (flags (logior #x00000007   ; page-align + memory info + video mode
                        #x00010000)) ; aout-kludge (address fields)
         (load-addr +x64-kernel-load-addr+)
         (entry-addr (+ load-addr 48)))  ; boot32 starts after 48-byte header
    (mvm-emit-u32 buf magic)
    (mvm-emit-u32 buf flags)
    ;; Checksum: -(magic + flags) mod 2^32
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
    (mvm-emit-u32 buf entry-addr)   ; entry_addr → boot32 code
    ;; Video mode fields (bit 2 in flags)
    (mvm-emit-u32 buf 1)            ; mode_type = 1 (EGA text)
    (mvm-emit-u32 buf 80)           ; width = 80 columns
    (mvm-emit-u32 buf 25)           ; height = 25 rows
    (mvm-emit-u32 buf 0)))          ; depth = 0 (ignored for text)

(defun emit-x64-console-init (buf)
  "Initialize console: zero globals, detect framebuffer, set up VGA, font, scancodes.
   Called after the standard x64 kernel64 entry (serial, alloc, interrupts)."
  ;; Disable MCE — UEFI firmware leaves it enabled with pending errors
  (emit-x64-disable-mce buf)
  ;; Zero globals and fb info (real hardware has garbage in RAM)
  (emit-x64-zero-globals buf)
  ;; Install exception handlers so CPU exceptions beep instead of triple-faulting
  (emit-x64-exception-handlers buf)
  ;; Detect GRUB framebuffer from multiboot info (sets fb_valid + fb_base etc.)
  (emit-x64-multiboot-fb-detect buf)
  ;; PCI fallback: read Intel GPU registers for framebuffer if multiboot had none
  (emit-x64-pci-fb-detect buf)
  ;; Clear VGA text screen + disable cursor + zero VGA cursor pos
  (emit-uefi-vga-clear buf)
  ;; Write 8×8 bitmap font at 0x601000
  (emit-uefi-font-data buf)
  ;; Write PS/2 scancode tables at 0x601800/0x601880 + zero shift state
  (emit-uefi-scancode-tables buf)
  ;; === Diagnostic beep group 1: step count at 1000Hz ===
  ;; [0x600150]+1 beeps: tells user how far PCI detection got (1-9)
  (uefi-emit-load-eax-abs32 buf #x600150)              ; eax = step counter
  (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC0)    ; inc eax
  (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC3)    ; mov ebx, eax (loop counter)
  (let ((loop-top (mvm-buffer-position buf)))
    (mvm-emit-byte buf #x53)                              ; push rbx
    (emit-uefi-beep buf 1193 #x4000000)                   ; 1000Hz beep ~100ms
    (emit-uefi-silence buf #x2000000)                      ; silence ~50ms
    (mvm-emit-byte buf #x5B)                              ; pop rbx
    (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xCB)    ; dec ebx
    (let ((off (- loop-top (+ (mvm-buffer-position buf) 2))))
      (mvm-emit-byte buf #x75) (mvm-emit-byte buf (logand off #xFF))))
  ;; Long pause between groups
  (emit-uefi-silence buf #x8000000)                        ; ~200ms silence
  ;; === Diagnostic beep group 2: DSPASURF top nibble at 2000Hz ===
  ;; Beeps = (DSPASURF >> 28) + 1.  Tells user the address range:
  ;; 1 beep = 0x0_______, 2 = 0x1_______, ..., 16 = 0xF_______
  ;; Helps diagnose whether DSPASURF is a small GTT offset or large physical addr
  (uefi-emit-load-eax-abs32 buf #x600164)              ; eax = DSPASURF
  (uefi-emit-shr-eax-imm buf 28)                        ; eax = top nibble (0-15)
  (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xC0)    ; inc eax (1-16)
  (mvm-emit-byte buf #x89) (mvm-emit-byte buf #xC3)    ; mov ebx, eax
  (let ((loop-top2 (mvm-buffer-position buf)))
    (mvm-emit-byte buf #x53)                              ; push rbx
    (emit-uefi-beep buf 597 #x4000000)                    ; 2000Hz beep ~100ms
    (emit-uefi-silence buf #x2000000)                      ; silence ~50ms
    (mvm-emit-byte buf #x5B)                              ; pop rbx
    (mvm-emit-byte buf #xFF) (mvm-emit-byte buf #xCB)    ; dec ebx
    (let ((off (- loop-top2 (+ (mvm-buffer-position buf) 2))))
      (mvm-emit-byte buf #x75) (mvm-emit-byte buf (logand off #xFF)))))

(defun emit32-store-dword (buf addr value)
  "Emit 32-bit mode: mov dword [addr], imm32.  10 bytes."
  (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf addr) (mvm-emit-u32 buf value))

(defun emit-x64-console-boot32 (buf)
  "32-bit boot stub that switches to VGA text mode via BIOS INT 10h,
   then falls through to the standard boot32 (page tables, long mode, etc.).
   Multiboot enters here in 32-bit protected mode."
  (let* ((base-addr +x64-kernel-load-addr+)
         (preamble-start (mvm-buffer-position buf))
         ;; Preamble size: save-ebx(6) + gdt(12*10=120) + stub(15*10=150) + lgdt(7) + jmp(7) = 290
         (return-addr (+ base-addr preamble-start 290))
         (ra0 (logand return-addr #xFF))
         (ra1 (logand (ash return-addr -8) #xFF))
         (ra2 (logand (ash return-addr -16) #xFF))
         (ra3 (logand (ash return-addr -24) #xFF)))

    ;; Save EBX (multiboot info pointer) to [0x500]
    ;; mov [0x500], ebx  (89 1D disp32)
    (mvm-emit-byte buf #x89) (mvm-emit-byte buf #x1D) (mvm-emit-u32 buf #x500)

    ;; === Write GDT at 0x7E00 (5 entries = 40 bytes) ===
    ;; Null descriptor (selector 0x00)
    (emit32-store-dword buf #x7E00 0)
    (emit32-store-dword buf #x7E04 0)
    ;; 32-bit code (selector 0x08): base=0, limit=4GB, code r/x, 32-bit
    (emit32-store-dword buf #x7E08 #x0000FFFF)
    (emit32-store-dword buf #x7E0C #x00CF9A00)
    ;; 32-bit data (selector 0x10): base=0, limit=4GB, data r/w, 32-bit
    (emit32-store-dword buf #x7E10 #x0000FFFF)
    (emit32-store-dword buf #x7E14 #x00CF9200)
    ;; 16-bit code (selector 0x18): base=0, limit=64KB, code r/x, 16-bit
    (emit32-store-dword buf #x7E18 #x0000FFFF)
    (emit32-store-dword buf #x7E1C #x000F9A00)
    ;; 16-bit data (selector 0x20): base=0, limit=64KB, data r/w, 16-bit
    (emit32-store-dword buf #x7E20 #x0000FFFF)
    (emit32-store-dword buf #x7E24 #x000F9200)
    ;; GDTR at 0x7E28: limit=39, base=0x7E00 (packed as two dwords)
    (emit32-store-dword buf #x7E28 #x7E000027)  ; limit=0x0027, base_lo=0x7E00
    (emit32-store-dword buf #x7E2C #x00000000)  ; base_hi=0x0000

    ;; === Write real-mode stub at 0x7C00 (60 bytes = 15 dwords) ===
    ;; Stub: 16-bit PM → real mode → INT 10h AX=3 → PM → jmp back
    (emit32-store-dword buf #x7C00 #x8E0020B8) ; mov ax,0x20; mov ds,ax (partial)
    (emit32-store-dword buf #x7C04 #x8EC08ED8) ; mov ds,ax(rest); mov es,ax; mov ss(partial)
    (emit32-store-dword buf #x7C08 #xC0200FD0) ; mov ss,ax(rest); mov eax,cr0
    (emit32-store-dword buf #x7C0C #x220FFE24) ; and al,0xFE; mov cr0,eax (partial)
    (emit32-store-dword buf #x7C10 #x7C16EAC0) ; mov cr0(rest); jmp far 0:0x7C16 (partial)
    (emit32-store-dword buf #x7C14 #xC0310000) ; jmp(rest); xor ax,ax
    (emit32-store-dword buf #x7C18 #xC08ED88E) ; mov ds,ax; mov es,ax
    (emit32-store-dword buf #x7C1C #xF0BCD08E) ; mov ss,ax; mov sp,0x7BF0 (partial)
    (emit32-store-dword buf #x7C20 #x0003B87B) ; sp(rest); mov ax,0x0003
    (emit32-store-dword buf #x7C24 #x0FFA10CD) ; int 0x10; cli; lgdt(partial)
    (emit32-store-dword buf #x7C28 #x7E281601) ; lgdt [0x7E28] (rest)
    (emit32-store-dword buf #x7C2C #x0CC0200F) ; mov eax,cr0; or al,1 (partial)
    (emit32-store-dword buf #x7C30 #xC0220F01) ; or(rest); mov cr0,eax
    ;; jmp far 0x0008:return_addr (66 EA addr32 08 00)
    (emit32-store-dword buf #x7C34
                        (logior #xEA66 (ash ra0 16) (ash ra1 24)))
    (emit32-store-dword buf #x7C38
                        (logior ra2 (ash ra3 8) (ash #x08 16)))

    ;; === Load GDT and jump to 16-bit stub ===
    ;; lgdt [0x7E28]  (0F 01 15 disp32)
    (mvm-emit-byte buf #x0F) (mvm-emit-byte buf #x01) (mvm-emit-byte buf #x15)
    (mvm-emit-u32 buf #x7E28)
    ;; jmp far 0x0018:0x00007C00  (EA off32 seg16)
    (mvm-emit-byte buf #xEA) (mvm-emit-u32 buf #x7C00) (mvm-emit-u16 buf #x0018)

    ;; === Return from real mode (32-bit protected mode) ===
    (let ((actual-return (+ base-addr (mvm-buffer-position buf))))
      (assert (= actual-return return-addr) ()
              "INT10h preamble size mismatch: expected ~X, got ~X" return-addr actual-return))
    ;; Reload 32-bit data segments
    ;; mov eax, 0x10
    (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf #x10)
    ;; mov ds, ax
    (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xD8)
    ;; mov es, ax
    (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xC0)
    ;; mov ss, ax
    (mvm-emit-byte buf #x8E) (mvm-emit-byte buf #xD0)
    ;; Restore EBX from [0x500]: mov ebx, [0x500]
    (mvm-emit-byte buf #x8B) (mvm-emit-byte buf #x1D) (mvm-emit-u32 buf #x500)

    ;; (no diagnostic beep here — counted beep in console-init is sufficient)
    )

  ;; Fall through to standard boot32 (cli, page tables, long mode, etc.)
  (emit-x64-boot32 buf))

(defun emit-x64-console-kernel64-entry (buf)
  "64-bit kernel entry with VGA + PS/2 console initialization.
   Wraps the standard x64 kernel64 entry and adds console init."
  (emit-x64-kernel64-entry buf)
  (emit-x64-console-init buf))

(defun x64-console-boot-descriptor ()
  "Boot descriptor for multiboot x86-64 with VGA text + PS/2 keyboard console.
   For GRUB boot on real hardware (ThinkPad T420 etc.)."
  (list :arch :x86-64
        :multiboot-header-fn #'emit-x64-multiboot-header
        :boot32-fn #'emit-x64-boot32
        :kernel64-entry-fn #'emit-x64-console-kernel64-entry
        :serial-init-fn #'x64-init-serial
        :smp-sequence-fn #'x64-init-smp-sequence
        :percpu-layout-fn #'x64-percpu-layout
        :load-addr +x64-kernel-load-addr+
        :stack-top +x64-stack-top+
        :cons-base +x64-cons-base+
        :general-base +x64-general-base+))
