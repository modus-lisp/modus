; The test hard disk's MBR: booted from C:, boot A: instead.
        bits 16
        org 0x600
start:  cli
        xor ax, ax
        mov ss, ax
        mov sp, 0x7C00
        mov ds, ax
        mov es, ax
        sti
        mov si, 0x7C00
        mov di, 0x600
        mov cx, 256
        cld
        rep movsw
        jmp 0:cont
cont:   mov bp, 4
retry:  xor ax, ax
        xor dx, dx
        int 0x13
        mov ax, 0x0201
        mov bx, 0x7C00
        mov cx, 1
        xor dx, dx
        int 0x13
        jnc ok
        dec bp
        jnz retry
fail:   int 0x18
        hlt
        jmp fail
ok:     cmp word [0x7DFE], 0xAA55
        jne fail
        xor dx, dx
        jmp 0:0x7C00
