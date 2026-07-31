;;;; boot-direct-i386.lisp — Direct boot (no GRUB) for i386
;;;; LEGACY MINI-LISP PATH.  This image runs mvm/repl-source.lisp — a separate
;;;; 708-line toy Lisp with its own reader, printer and evaluator, NOT the real
;;;; Common Lisp stack.  The real CL image on i386 is mvm/build-i386-cli.lisp
;;;; (prelude + gc + rt + the cl-*.lisp bridge + crypto), driven by
;;;; scripts/run-i386.sh.  Retiring this second Lisp is its own workstream.
;;;;
;;;; Produces a 512-byte MBR boot sector that:
;;;;   1. Sets VGA text mode 3 (80x25) via INT 10h
;;;;   2. Enables A20 gate
;;;;   3. Sets up GDT for PM transitions
;;;;   4. Loads kernel from disk in chunks (127 sectors each)
;;;;      Each chunk: BIOS read to 0x10000, enter PM, copy to 0x100000+, return to RM
;;;;   5. Final PM entry, jumps to kernel at 0x100020
;;;;
;;;; Supports kernels up to ~16MB (LBA only, no CHS fallback).
;;;; The same kernel binary works with QEMU -kernel (multiboot).

(in-package :modus.mvm)

;;; ============================================================
;;; Boot Sector Generation
;;; ============================================================

(defun generate-boot-sector (kernel-size-bytes)
  "Generate a 512-byte boot sector with chunked PM loading.
   Supports kernels larger than 1MB by copying each chunk to high memory
   via protected mode transitions during the BIOS load loop."
  (let* ((bs (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0))
         (pos 0)
         (kernel-sectors (ceiling kernel-size-bytes 512))
         ;; Positions to patch
         (msg-si-pos 0) (msg-call-pos 0)
         (lba-jc-err-pos 0) (loop-jc-err-pos 0)
         (lba-ok-pos 0) (load-loop-pos 0)
         (far-jmp-pos 0) (print-str-pos 0) (msg-data-pos 0)
         (gdtr-data-pos 0) (gdt-data-pos 0)
         (lgdt-addr-pos 0))
    (labels ((b (v) (setf (aref bs pos) (logand v #xFF)) (incf pos))
             (w (v) (b v) (b (ash v -8)))
             (d (v) (b v) (b (ash v -8)) (b (ash v -16)) (b (ash v -24)))
             (p8 (from to)
               "Patch rel8 jump at FROM to target TO."
               (setf (aref bs (1+ from)) (logand (- to from 2) #xFF))))

      ;; === 16-bit real mode preamble ===
      (b #xFA)                         ; cli
      (b #x31) (b #xC0)               ; xor ax, ax
      (b #x8E) (b #xD8)               ; mov ds, ax
      (b #x8E) (b #xC0)               ; mov es, ax
      (b #x8E) (b #xD0)               ; mov ss, ax
      (b #xBC) (w #x7C00)             ; mov sp, 0x7C00
      (b #xFB)                         ; sti
      (b #x88) (b #x16) (w #x7E00)    ; mov [0x7E00], dl (save boot drive)

      ;; Set VGA text mode 3
      (b #xB8) (w #x0003)             ; mov ax, 3
      (b #xCD) (b #x10)               ; int 0x10

      ;; Print "Modus" message
      (setf msg-si-pos pos)
      (b #xBE) (w 0)                  ; mov si, msg (PATCH)
      (setf msg-call-pos pos)
      (b #xE8) (w 0)                  ; call print_str (PATCH)

      ;; Enable A20 — BIOS method (INT 15h), then fast method + KBC fallback
      (b #xB8) (w #x2401)             ; mov ax, 0x2401 (enable A20)
      (b #xCD) (b #x15)               ; int 0x15
      (b #xE4) (b #x92)               ; in al, 0x92
      (b #x0C) (b #x02)               ; or al, 2
      (b #x24) (b #xFE)               ; and al, ~1
      (b #xE6) (b #x92)               ; out 0x92, al

      ;; === Load GDT from embedded data in boot sector ===
      ;; GDT and GDTR are embedded at the end of the boot sector.
      ;; lgdt loads from physical address 0x7C00 + gdtr_offset.
      ;; (GDTR and GDT positions patched after all code is emitted.)
      (b #x0F) (b #x01) (b #x16) (w 0) ; lgdt [gdtr_addr] (PATCH last 2 bytes)
      (setf lgdt-addr-pos (- pos 2))

      ;; === Check LBA support ===
      (b #xB4) (b #x41)               ; mov ah, 0x41
      (b #xBB) (w #x55AA)             ; mov bx, 0x55AA
      (b #x8A) (b #x16) (w #x7E00)   ; mov dl, [0x7E00]
      (b #xCD) (b #x13)               ; int 0x13
      (setf lba-jc-err-pos pos)
      (b #x72) (b 0)                  ; jc disk_error (PATCH)

      ;; === Set up DAP (Disk Address Packet) at 0x7E10 ===
      (b #xC6) (b #x06) (w #x7E10) (b #x10)     ; DAP size = 16
      (b #xC6) (b #x06) (w #x7E11) (b #x00)     ; reserved = 0
      ;; count filled per-iteration
      (b #xC7) (b #x06) (w #x7E14) (w #x0000)   ; offset = 0
      (b #xC7) (b #x06) (w #x7E16) (w #x1000)   ; segment = 0x1000 (phys 0x10000)
      (b #x66) (b #xC7) (b #x06) (w #x7E18) (d 1) ; LBA = 1 (sector 0 = boot sector)
      (b #x66) (b #xC7) (b #x06) (w #x7E1C) (d 0) ; LBA hi = 0

      ;; === Initialize destination pointer at [0x7E04] ===
      (b #x66) (b #xC7) (b #x06) (w #x7E04) (d #x100000) ; dest = 0x100000

      ;; === Initialize remaining sector count ===
      (b #xBF) (w kernel-sectors)     ; mov di, remaining_sectors

      ;; === Load loop: read chunk, PM copy, repeat ===
      (setf load-loop-pos pos)

      ;; chunk = min(di, 127)
      (b #x89) (b #xF8)               ; mov ax, di
      (b #x3D) (w 127)                ; cmp ax, 127
      (setf lba-ok-pos pos)
      (b #x76) (b 0)                  ; jbe lba_ok (PATCH)
      (b #xB8) (w 127)                ; mov ax, 127
      ;; lba_ok:
      (p8 lba-ok-pos pos)
      (b #xA3) (w #x7E12)             ; mov [0x7E12], ax (DAP count)

      ;; BIOS extended read
      (b #xBE) (w #x7E10)             ; mov si, DAP
      (b #xB4) (b #x42)               ; mov ah, 0x42
      (b #x8A) (b #x16) (w #x7E00)   ; mov dl, [0x7E00]
      (b #xCD) (b #x13)               ; int 0x13
      (setf loop-jc-err-pos pos)
      (b #x72) (b 0)                  ; jc disk_error (PATCH)

      ;; === Enter protected mode for copy ===
      (b #x57)                         ; push di (save remaining sectors)
      (b #xFA)                         ; cli
      (b #x0F) (b #x20) (b #xC0)     ; mov eax, cr0
      (b #x0C) (b #x01)               ; or al, 1
      (b #x0F) (b #x22) (b #xC0)     ; mov cr0, eax
      ;; Now in PM with 16-bit CS cache (no far jump needed for data-only PM)

      ;; Load flat data segments
      (b #xB8) (w #x0010)             ; mov ax, 0x10 (data selector)
      (b #x8E) (b #xD8)               ; mov ds, ax
      (b #x8E) (b #xC0)               ; mov es, ax

      ;; Copy chunk from 0x10000 to destination
      ;; ESI = 0x10000 (source: real-mode load buffer)
      (b #x66) (b #xBE) (d #x10000)  ; mov esi, 0x10000
      ;; EDI = [0x7E04] (destination pointer)
      (b #x66) (b #x8B) (b #x3E) (w #x7E04) ; mov edi, dword [0x7E04]
      ;; ECX = chunk_sectors * 128 (= chunk * 512 / 4)
      (b #x66) (b #x31) (b #xC9)     ; xor ecx, ecx
      (b #x8B) (b #x0E) (w #x7E12)   ; mov cx, [0x7E12]
      (b #x66) (b #xC1) (b #xE1) (b 7) ; shl ecx, 7
      ;; REP MOVSD (32-bit addresses + 32-bit operands in 16-bit mode)
      (b #xFC)                         ; cld
      (b #x67) (b #x66) (b #xF3) (b #xA5) ; rep movsd
      ;; Save updated destination
      (b #x66) (b #x89) (b #x3E) (w #x7E04) ; mov dword [0x7E04], edi

      ;; === Return to real mode ===
      (b #x0F) (b #x20) (b #xC0)     ; mov eax, cr0
      (b #x24) (b #xFE)               ; and al, ~1
      (b #x0F) (b #x22) (b #xC0)     ; mov cr0, eax
      ;; Reload real-mode segments
      (b #x31) (b #xC0)               ; xor ax, ax
      (b #x8E) (b #xD8)               ; mov ds, ax
      (b #x8E) (b #xC0)               ; mov es, ax
      (b #x8E) (b #xD0)               ; mov ss, ax
      (b #xFB)                         ; sti (re-enable for BIOS)
      (b #x5F)                         ; pop di (restore remaining sectors)

      ;; Advance LBA in DAP
      (b #xA1) (w #x7E12)             ; mov ax, [0x7E12] (chunk count)
      (b #x01) (b #x06) (w #x7E18)   ; add [0x7E18], ax
      (b #x83) (b #x16) (w #x7E1A) (b 0)  ; adc word [0x7E1A], 0

      ;; remaining -= chunk
      (b #x2B) (b #x3E) (w #x7E12)   ; sub di, [0x7E12]
      (b #x75) (b (logand (- load-loop-pos (+ pos 1)) #xFF)) ; jnz load_loop

      ;; === All loaded — print '+' to confirm ===
      (b #xB0) (b (char-code #\+))    ; mov al, '+'
      (b #xB4) (b #x0E)               ; mov ah, 0x0E
      (b #x31) (b #xDB)               ; xor bx, bx
      (b #xCD) (b #x10)               ; int 0x10

      ;; === Re-enable A20 gate ===
      ;; BIOS INT 13h calls during chunked loading may disable A20.
      ;; Without A20, bit 20 of DRAM addresses is masked — CPU writes to
      ;; 0x7901000 would go to 0x7801000, but NIC DMA reads the real address.
      ;; Use BIOS INT 15h (most reliable), port 0x92, and KBC output port.
      (b #xB8) (w #x2401)             ; mov ax, 0x2401 (BIOS: enable A20)
      (b #xCD) (b #x15)               ; int 0x15
      (b #xE4) (b #x92)               ; in al, 0x92
      (b #x0C) (b #x02)               ; or al, 2
      (b #x24) (b #xFE)               ; and al, ~1
      (b #xE6) (b #x92)               ; out 0x92, al
      ;; KBC method: write 0xDF to output port (A20 enable)
      (b #xB0) (b #xD1)               ; mov al, 0xD1 (write output port cmd)
      (b #xE6) (b #x64)               ; out 0x64, al
      (b #xB0) (b #xDF)               ; mov al, 0xDF (A20 on + defaults)
      (b #xE6) (b #x60)               ; out 0x60, al

      ;; === Final PM entry ===
      (b #xFA)                         ; cli
      (b #x0F) (b #x20) (b #xC0)     ; mov eax, cr0
      (b #x0C) (b #x01)               ; or al, 1
      (b #x0F) (b #x22) (b #xC0)     ; mov cr0, eax

      ;; Far jump to 32-bit code (loads 32-bit CS from GDT)
      (setf far-jmp-pos pos)
      (b #xEA)                         ; jmp far
      (w 0)                            ; offset (PATCH)
      (w #x0008)                       ; selector

      ;; === 32-bit protected mode code ===
      ;; Patch far jump target to this address
      (let ((pm-addr (+ #x7C00 pos)))
        (setf (aref bs (+ far-jmp-pos 1)) (logand pm-addr #xFF))
        (setf (aref bs (+ far-jmp-pos 2)) (logand (ash pm-addr -8) #xFF)))

      ;; Set up 32-bit data segments
      ;; NOTE: These are 32-bit instructions (D=1 from CS descriptor)
      (b #xB8) (d #x0010)             ; mov eax, 0x10
      (b #x8E) (b #xD8)               ; mov ds, ax
      (b #x8E) (b #xC0)               ; mov es, ax
      (b #x8E) (b #xE0)               ; mov fs, ax
      (b #x8E) (b #xE8)               ; mov gs, ax
      (b #x8E) (b #xD0)               ; mov ss, ax
      (b #xBC) (d #x90000)            ; mov esp, 0x90000

      ;; Multiboot state
      (b #xB8) (d #x2BADB002)         ; mov eax, multiboot magic
      (b #x31) (b #xDB)               ; xor ebx, ebx

      ;; Jump to kernel entry (past 32-byte multiboot header)
      (b #x68) (d #x100020)           ; push 0x100020
      (b #xC3)                         ; ret → jumps to kernel

      ;; === 16-bit subroutines ===

      ;; disk_error:
      (p8 lba-jc-err-pos pos)
      (p8 loop-jc-err-pos pos)
      (b #xB0) (b (char-code #\!))    ; mov al, '!'
      (b #xB4) (b #x0E)               ; mov ah, 0x0E
      (b #x31) (b #xDB)               ; xor bx, bx
      (b #xCD) (b #x10)               ; int 0x10
      (b #xFA) (b #xF4)               ; cli; hlt

      ;; print_str:
      (setf print-str-pos pos)
      (b #xAC)                         ; lodsb
      (b #x08) (b #xC0)               ; or al, al
      (b #x74) (b 5)                  ; jz +5 (to ret)
      (b #xB4) (b #x0E)               ; mov ah, 0x0E
      (b #x31) (b #xDB)               ; xor bx, bx
      (b #xCD) (b #x10)               ; int 0x10
      (b #xEB) (b (logand (- print-str-pos (+ pos 1)) #xFF)) ; jmp print_str
      (b #xC3)                         ; ret

      ;; === Embedded data ===

      ;; "Modus\r\n\0"
      (setf msg-data-pos pos)
      (dolist (ch (coerce "Modus" 'list)) (b (char-code ch)))
      (b #x0D) (b #x0A) (b 0)

      ;; GDTR (6 bytes): limit=23, base=physical address of GDT
      (setf gdtr-data-pos pos)
      (w 23)                           ; GDT limit (3 entries * 8 - 1)
      (d 0)                            ; GDT base (PATCH after GDT is emitted)

      ;; GDT (24 bytes): null + code + data
      (setf gdt-data-pos pos)
      (d 0) (d 0)                     ; null descriptor
      (d #x0000FFFF) (d #x00CF9A00)   ; code: base=0, limit=4G, 32-bit, R/X
      (d #x0000FFFF) (d #x00CF9200)   ; data: base=0, limit=4G, 32-bit, R/W

      ;; === Patch all forward references ===

      ;; Patch message address: mov si, msg
      (let ((msg-linear (+ #x7C00 msg-data-pos)))
        (setf (aref bs (+ msg-si-pos 1)) (logand msg-linear #xFF))
        (setf (aref bs (+ msg-si-pos 2)) (logand (ash msg-linear -8) #xFF)))

      ;; Patch print_str call
      (let* ((call-next (+ msg-call-pos 3))
             (print-linear (+ #x7C00 print-str-pos))
             (disp (- print-linear (+ #x7C00 call-next))))
        (setf (aref bs (+ msg-call-pos 1)) (logand disp #xFF))
        (setf (aref bs (+ msg-call-pos 2)) (logand (ash disp -8) #xFF)))

      ;; Patch lgdt address → point to GDTR data
      (let ((gdtr-linear (+ #x7C00 gdtr-data-pos)))
        (setf (aref bs lgdt-addr-pos) (logand gdtr-linear #xFF))
        (setf (aref bs (1+ lgdt-addr-pos)) (logand (ash gdtr-linear -8) #xFF)))

      ;; Patch GDTR base → physical address of GDT
      (let ((gdt-linear (+ #x7C00 gdt-data-pos)))
        (setf (aref bs (+ gdtr-data-pos 2)) (logand gdt-linear #xFF))
        (setf (aref bs (+ gdtr-data-pos 3)) (logand (ash gdt-linear -8) #xFF))
        (setf (aref bs (+ gdtr-data-pos 4)) (logand (ash gdt-linear -16) #xFF))
        (setf (aref bs (+ gdtr-data-pos 5)) (logand (ash gdt-linear -24) #xFF)))

      (when (> pos 510)
        (error "Boot sector overflow: ~D bytes (max 510)" pos))

      ;; Boot signature
      (setf (aref bs 510) #x55)
      (setf (aref bs 511) #xAA)

      (format t "Boot sector: ~D/510 bytes used~%" pos)
      bs)))

;;; ============================================================
;;; i386 Console Boot — 32-bit init code
;;; ============================================================

(defun emit-i386-vga-clear (buf)
  "Clear VGA text screen (80x25) and disable hardware cursor."
  (mvm-emit-byte buf #xBF) (mvm-emit-u32 buf #xB8000)
  (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf #x0F200F20)
  (mvm-emit-byte buf #xB9) (mvm-emit-u32 buf 1000)
  (mvm-emit-byte buf #xFC) (mvm-emit-byte buf #xF3) (mvm-emit-byte buf #xAB)
  ;; Enable underline cursor (start=14, end=15)
  (i386-out buf #x3D4 #x0A) (i386-out buf #x3D5 #x0E)
  (i386-out buf #x3D4 #x0B) (i386-out buf #x3D5 #x0F)
  (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf #x600130) (mvm-emit-u32 buf 0)
  (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf #x600134) (mvm-emit-u32 buf 0))

(defun emit-i386-data-block (buf addr data-bytes)
  "Write DATA-BYTES at ADDR using STOSD."
  (mvm-emit-byte buf #xBF) (mvm-emit-u32 buf addr)
  (mvm-emit-byte buf #xFC)
  (let* ((len (length data-bytes))
         (ndw (ceiling len 4)))
    (loop for i from 0 below ndw
          do (let ((dw 0))
               (loop for j from 0 below 4
                     for idx = (+ (* i 4) j)
                     when (< idx len)
                     do (setf dw (logior dw (ash (elt data-bytes idx) (* j 8)))))
               (mvm-emit-byte buf #xB8) (mvm-emit-u32 buf dw)
               (mvm-emit-byte buf #xAB)))))

(defun emit-i386-scancode-tables (buf)
  "Write PS/2 scancode tables and zero shift state."
  (emit-i386-data-block buf +scan-normal+ *ps2-scancode-normal*)
  (emit-i386-data-block buf +scan-shifted+ *ps2-scancode-shifted*)
  (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf +shift-state+) (mvm-emit-u32 buf 0))

(defun emit-i386-console-entry (buf)
  "i386 entry with VGA text + PS/2 keyboard console."
  (emit-i386-entry buf)
  (emit-i386-vga-clear buf)
  (emit-i386-scancode-tables buf)
  ;; Zero globals (real hardware has garbage)
  (mvm-emit-byte buf #xC7) (mvm-emit-byte buf #x05)
  (mvm-emit-u32 buf #x600000) (mvm-emit-u32 buf 0))

(defun i386-console-boot-descriptor ()
  "Boot descriptor for i386 with VGA text + PS/2 keyboard console."
  (list :arch :i386
        :multiboot-header-fn #'emit-i386-multiboot-header
        :entry-fn #'emit-i386-console-entry
        :serial-init-fn #'i386-init-serial
        :percpu-layout-fn #'i386-percpu-layout
        :load-addr +i386-kernel-load-addr+
        :stack-top +i386-stack-top+
        :cons-base +i386-cons-base+
        :general-base +i386-general-base+
        :percpu-base +i386-percpu-base+
        :percpu-stride +i386-percpu-stride+))
