; mbr.asm -- the test hard disk's master boot record (make-media.sh).
; The usual job: move out of 0:7C00, find the active partition, read its
; first sector to 0:7C00 and jump there with DL = the boot drive and DS:SI =
; the partition's table entry.  LBA (INT 13h AH=42h) when the BIOS has the
; extensions, CHS from the table otherwise; INT 18h (the BIOS's "no boot
; device") when there is no active partition or it will not read.
; nasm -f bin -o mbr.bin mbr.asm
        bits 16
        org 0x600
start:  cli
        xor ax, ax
        mov ss, ax
        mov sp, 0x7C00
        mov ds, ax
        mov es, ax
        sti
        cld
        mov si, 0x7C00
        mov di, 0x600
        mov cx, 256
        rep movsw
        jmp 0:scan                      ; now running in the copy
scan:   mov si, table
        mov cx, 4
.next:  cmp byte [si], 0x80
        je found
        add si, 16
        loop .next
fail:   int 0x18
        hlt
        jmp fail
found:  mov [entry], si
        mov [drive], dl
        mov bp, 4                       ; tries
retry:  mov ah, 0x41                    ; LBA extensions?
        mov bx, 0x55AA
        mov dl, [drive]
        int 0x13
        jc chs
        cmp bx, 0xAA55
        jne chs
        test cl, 1
        jz chs
        mov si, [entry]
        mov eax, [si + 8]
        mov [dap_lba], eax
        mov si, dap
        mov ah, 0x42
        mov dl, [drive]
        int 0x13
        jnc loaded
        jmp again
chs:    mov si, [entry]
        mov dh, [si + 1]
        mov cx, [si + 2]
        mov bx, 0x7C00
        mov ax, 0x0201
        mov dl, [drive]
        int 0x13
        jnc loaded
again:  xor ax, ax                       ; reset, try again
        mov dl, [drive]
        int 0x13
        dec bp
        jnz retry
        jmp fail
loaded: cmp word [0x7DFE], 0xAA55
        jne fail
        mov si, [entry]
        mov dl, [drive]
        jmp 0:0x7C00

dap:    db 16, 0
        dw 1                            ; one sector
        dw 0x7C00, 0                    ; to 0:7C00
dap_lba: dd 0, 0
entry:  dw 0
drive:  db 0

        times 0x1BE - ($ - $$) db 0
table:
