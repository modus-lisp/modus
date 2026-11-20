# pcboot1.s -- the boot sector of a modus disk that needs no DOS.
# (scripts/pcboot.sh builds it; scripts/pcboot-disk.lisp writes the disk.)
#
# The BIOS loads this sector to 0:7C00 with DL = the boot drive.  It reads the
# next nine sectors -- the file table (sector 1) and the shim (sectors 2-9,
# scripts/pcboot2.s) -- to 0:0600 and jumps to the shim at 0:0800 with DL kept.
# LBA (INT 13h AH=42h) when the BIOS has the extensions, CHS otherwise, from
# the geometry AH=08h reports (a floppy whose BIOS will not say: 18 x 2).
#
# The partition table holds one active entry over the rest of the disk, so a
# BIOS that boots a USB stick as a hard disk only when it sees one does; a
# floppy's BIOS ignores it.  pcboot-disk.lisp fills in its size.
#
# Assemble: as --32 -o b1.o pcboot1.s
#           ld -m elf_i386 -Ttext=0x7C00 --oformat binary -o pcboot1.bin b1.o

        .code16
        .text
        .globl  _start

        .equ    LOAD,   0x0600          # sectors 1.. go here
        .equ    NSEC,   9               # the table and the shim
        .equ    SHIM,   0x0800

_start:
        cli
        xor     %ax, %ax
        mov     %ax, %ds
        mov     %ax, %es
        mov     %ax, %ss
        mov     $0x7C00, %sp
        sti
        cld
        ljmp    $0, $1f                 # CS = 0 (some BIOSes enter at 07C0:0000)
1:      mov     %dl, drive

        mov     $0x41, %ah              # LBA extensions?
        mov     $0x55AA, %bx
        int     $0x13
        jc      chs
        cmp     $0xAA55, %bx
        jne     chs
        test    $1, %cl
        jz      chs
        mov     $dap, %si               # one call: NSEC sectors from LBA 1
        mov     drive, %dl
        mov     $0x42, %ah
        int     $0x13
        jnc     go
                                        # fall through to CHS on a failure
chs:    mov     drive, %dl
        mov     $0x08, %ah
        xor     %di, %di                # (some BIOSes want ES:DI = 0)
        int     $0x13
        jc      1f
        and     $0x3F, %cl
        jz      1f
        mov     %cl, spt
        inc     %dh
        mov     %dh, heads
1:      xor     %ax, %ax
        mov     %ax, %es
        mov     $1, %ax                 # LBA 1..NSEC, a sector at a time
        mov     $LOAD, %bx
next:   push    %ax
        mov     $3, %bp                 # tries
retry:  push    %ax
        xor     %dx, %dx
        movzbw  spt, %cx
        div     %cx                     # AX = LBA / spt, DX = LBA % spt
        mov     %dl, %cl
        inc     %cl                     # sector, 1-based
        xor     %dx, %dx
        movzbw  heads, %di
        div     %di                     # AX = cylinder, DX = head
        mov     %dl, %dh
        mov     %al, %ch                # cylinder bits 0-7
        shl     $6, %ah
        or      %ah, %cl                # cylinder bits 8-9 in CL 6-7
        mov     drive, %dl
        mov     $0x0201, %ax
        int     $0x13
        pop     %ax
        jnc     2f
        push    %ax
        xor     %ah, %ah                # reset, try again
        mov     drive, %dl
        int     $0x13
        pop     %ax
        dec     %bp
        jnz     retry
        jmp     fail
2:      add     $512, %bx
        pop     %ax
        inc     %ax
        cmp     $NSEC, %ax
        jbe     next

go:     cmpl    $0x55444F4D, LOAD       # "MODUSFS1": the table is there
        jne     fail
        mov     drive, %dl
        ljmp    $0, $SHIM

fail:   mov     $msg, %si
1:      lodsb
        test    %al, %al
        jz      2f
        mov     $0x0E, %ah
        xor     %bx, %bx
        int     $0x10
        jmp     1b
2:      int     $0x18                   # the BIOS's "no boot device"
3:      hlt
        jmp     3b

msg:    .asciz  "MODUS: cannot read this disk\r\n"

dap:    .byte   16, 0
        .word   NSEC                    # sectors
        .word   LOAD, 0                 # to 0:LOAD
        .long   1, 0                    # from LBA 1
drive:  .byte   0
spt:    .byte   18
heads:  .byte   2

        .org    0x1BE
part:   .byte   0x80, 0, 1, 0           # active; CHS start (unused)
        .byte   0x7F, 0xFE, 0xFF, 0xFF  # type 7Fh (reserved for experiments); CHS end
        .long   1                       # LBA start
        .long   0                       # sectors: filled in by pcboot-disk.lisp
        .fill   48, 1, 0
        .word   0xAA55
