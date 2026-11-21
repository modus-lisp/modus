# pcboot2.s -- the shim between pcboot1.s's boot sector and MODUS.COM.
#
# MODUS.COM (scripts/dos386-chain.s) is a DOS program: it reads the image with
# DOS's file calls and prints with DOS's character calls, and everything else
# it does -- the CPU and FPU checks, memory, A20, unreal mode, the multiboot
# entry -- is its own.  So on a disk with no DOS this provides the seven
# INT 21h functions it uses, over the disk's FILE TABLE (sector 1, at 0:0600):
#
#   02h print DL        09h print DS:DX to '$'      4Ch stop
#   3Dh open DS:DX      3Fh read BX: CX bytes to DS:DX
#   42h seek BX to CX:DX (AL = 0)                    3Eh close BX
#
# and starts it the way DOS would: a PSP at 1000:0000 with an empty command
# tail, the program at 1000:0100, SS:SP at the segment's top.  Output goes to
# the screen (INT 10h) and to COM1.
#
# The file table: "MODUSFS1", then 32-byte entries -- the name (16 bytes,
# NUL-padded, upper case), the first sector, the size in bytes -- ended by an
# entry whose name is empty.  A name is matched after its last '\' or ':'.
#
# Reads go through a 32 KB buffer at 3000:0000 (above MODUS.COM's segment and
# the 64 KB it stages HMA-bound data in): one LBA call, or with CHS one call
# per track.
#
# THE BOOT RECORD (at 0:0580, for the image once it runs): "PCBT", the linear
# address of PM_READ, the first sector of the image MODUS.COM opened (a .BIN or
# .SF), the drive.  PM_READ is a 32-bit routine the image calls from protected
# mode, paging on, interrupts off: EAX = a sector, ECX = how many (1..64) ->
# the sectors at 3000:0000 (linear 30000h), EAX = how many were read, CF on a
# disk error; every other register kept.  It drops to real mode with the
# BIOS's interrupt controller mapping (IRQ 0-7 at 08h, 8-15 at 70h; the timer,
# the floppy and the disks unmasked, the keyboard not), reads with INT 13h,
# and returns to the image's mapping (20h / 28h, masks FAh / FDh: what
# boot/boot-i386.lisp EMIT-I386-INTERRUPT-SETUP programs) and its GDT, IDT,
# CR0 and stack.  Everything it touches below 1 MB is the shim's own, the
# buffer, and a real-mode stack under 7C00h.
#
# Assemble: as --32 -o b2.o pcboot2.s
#           ld -m elf_i386 -Ttext=0x0800 --oformat binary -o pcboot2.bin b2.o

        .code16
        .text
        .globl  _start

        .equ    TABLE,  0x0600
        .equ    PSEG,   0x1000          # the program's segment
        .equ    BSEG,   0x3000          # the read buffer's
        .equ    BSECS,  64              # its size in sectors
        .equ    NHANDLE, 4
        .equ    BOOTREC, 0x0580

_start:
        xor     %ax, %ax
        mov     %ax, %ds
        mov     %ax, %es
        mov     %dl, drive
        cld

        mov     $0x41, %ah              # LBA extensions?
        mov     $0x55AA, %bx
        int     $0x13
        jc      1f
        cmp     $0xAA55, %bx
        jne     1f
        test    $1, %cl
        jz      1f
        movb    $1, lba
1:      mov     drive, %dl              # the geometry, for CHS
        mov     $0x08, %ah
        xor     %di, %di
        int     $0x13
        jc      2f
        and     $0x3F, %cl
        jz      2f
        mov     %cl, spt
        inc     %dh
        mov     %dh, heads
2:      xor     %ax, %ax
        mov     %ax, %es

        mov     $msg_boot, %si
        call    putz

        movl    $0x54424350, BOOTREC    # "PCBT"
        movl    $pm_read, BOOTREC+4
        movl    $0, BOOTREC+8
        movzbl  drive, %eax
        mov     %eax, BOOTREC+12

        movw    $int21, 0x84            # INT 21h is ours
        movw    $0, 0x86

        mov     $name_com, %dx          # MODUS.COM to PSEG:0100
        call    open_dsdx
        jc      nocom
        mov     %ax, %bx
        mov     $PSEG, %ax
        mov     %ax, %es
        xor     %di, %di                # the PSP: zeroes, INT 20h at 0, an
        mov     $128, %cx               # empty command tail
        xor     %ax, %ax
        rep stosw
        movw    $0x20CD, %es:0
        movb    $0x0D, %es:0x81
        push    %bx
        mov     %bx, %ax
        call    entry                   # SI = the handle's table entry
        mov     20(%si), %ecx           # its size
        pop     %bx
        cmp     $0xFE00, %ecx
        ja      nocom
        mov     $PSEG, %ax
        mov     $0x0100, %dx
        call    readfile                # BX handle, CX bytes, AX:DX buffer
        jc      nocom
        mov     $PSEG, %ax              # enter it as DOS would
        mov     %ax, %ds
        mov     %ax, %es
        cli
        mov     %ax, %ss
        mov     $0xFFFE, %sp
        sti
        pushw   $0
        ljmp    $PSEG, $0x0100

nocom:  mov     $msg_nocom, %si
        call    putz
halt:   hlt
        jmp     halt

# --- INT 21h ---------------------------------------------------------------
# Entered with the caller's registers; returns with RETF 2 so that CF is ours.
int21:  sti
        cld
        cmp     $0x02, %ah
        je      f02
        cmp     $0x09, %ah
        je      f09
        cmp     $0x3D, %ah
        je      f3d
        cmp     $0x3E, %ah
        je      f3e
        cmp     $0x3F, %ah
        je      f3f
        cmp     $0x42, %ah
        je      f42
        cmp     $0x4C, %ah
        je      f4c
        mov     $1, %ax                 # anything else: invalid function
        stc
        lret    $2

f02:    push    %ax
        mov     %dl, %al
        call    putc
        pop     %ax
        mov     %dl, %al
        clc
        lret    $2

f09:    push    %si
        push    %ax
        mov     %dx, %si
1:      lodsb                           # DS:SI, the caller's DS
        cmp     $'$', %al
        je      2f
        call    putc
        jmp     1b
2:      pop     %ax
        mov     $'$', %al
        pop     %si
        clc
        lret    $2

f3d:    push    %ds                     # the name is at the caller's DS:DX
        push    %es                     # (DOS keeps ES)
        push    %si
        push    %di
        push    %bx
        push    %cx
        mov     %ds, %bx                # copy it to our name buffer
        mov     %dx, %si
        xor     %ax, %ax
        mov     %ax, %es
        mov     $namebuf, %di
        mov     $63, %cx
1:      lodsb
        test    %al, %al
        jz      2f
        stosb
        loop    1b
2:      movb    $0, %es:(%di)
        xor     %ax, %ax
        mov     %ax, %ds
        mov     $namebuf, %dx
        call    open_dsdx
        pop     %cx
        pop     %bx
        pop     %di
        pop     %si
        pop     %es
        pop     %ds
        lret    $2                      # CF and AX from open_dsdx

f3e:    push    %ds                     # free the handle's slot
        push    %bx
        push    %ax
        xor     %ax, %ax
        mov     %ax, %ds
        sub     $5, %bx
        cmp     $NHANDLE, %bx
        jae     1f
        shl     $1, %bx
        movw    $0, ent(%bx)
1:      pop     %ax
        pop     %bx
        pop     %ds
        clc
        lret    $2

f3f:    push    %ds
        push    %es
        push    %si
        push    %di
        push    %bx
        push    %dx
        push    %cx
        mov     %ds, %ax                # the buffer: caller's DS:DX
        push    %ax
        xor     %si, %si
        mov     %si, %ds
        pop     %ax
        call    readfile                # -> AX = bytes read, CF on error
        pop     %cx
        pop     %dx
        pop     %bx
        pop     %di
        pop     %si
        pop     %es
        pop     %ds
        lret    $2

f42:    push    %ds
        push    %si
        push    %bx
        xor     %si, %si
        mov     %si, %ds
        sub     $5, %bx                 # handle -> slot
        cmp     $NHANDLE, %bx
        jae     9f
        shl     $2, %bx
        mov     %dx, pos(%bx)           # CX:DX, from the start (AL = 0)
        mov     %cx, pos+2(%bx)
        mov     %dx, %ax
        mov     %cx, %dx
        pop     %bx
        pop     %si
        pop     %ds
        clc
        lret    $2
9:      pop     %bx
        pop     %si
        pop     %ds
        mov     $6, %ax
        stc
        lret    $2

f4c:    xor     %ax, %ax
        mov     %ax, %ds
        mov     $msg_stop, %si
        call    putz
        jmp     halt

# --- the file table ----------------------------------------------------------
# open_dsdx: DS = 0, DX = a NUL-terminated name -> AX = handle (slot + 5), CF
# clear; or AX = 2, CF set.  The handle's slot remembers the entry; its
# position starts at 0.
open_dsdx:
        push    %si
        push    %di
        push    %bx
        push    %cx
        mov     %dx, %si                # past the last '\' or ':'
        mov     %dx, %bx
1:      lodsb
        test    %al, %al
        jz      2f
        cmp     $'\\', %al
        je      3f
        cmp     $':', %al
        jne     1b
3:      mov     %si, %bx
        jmp     1b
2:      mov     $TABLE+8, %di           # the entries
4:      cmpb    $0, (%di)
        je      notfound
        mov     %bx, %si
        push    %di
5:      lodsb
        cmp     $'a', %al               # upper case
        jb      6f
        cmp     $'z', %al
        ja      6f
        sub     $32, %al
6:      mov     (%di), %ah
        inc     %di
        cmp     %ah, %al
        jne     7f
        test    %al, %al
        jnz     5b
        pop     %di                     # a match: take a slot
        xor     %bx, %bx
8:      cmpw    $0, ent(%bx)
        je      9f
        add     $2, %bx
        cmp     $2*NHANDLE, %bx
        jb      8b
        jmp     notfound
9:      mov     %di, ent(%bx)
        push    %eax                    # an image (not MODUS.COM): its sector
        cmpb    $'.', 5(%di)            # "MODUS.SF" / "MODUS.BIN": '.' at 5
        jne     10f
        cmpb    $'C', 6(%di)
        je      10f
        mov     16(%di), %eax
        mov     %eax, BOOTREC+8
10:     pop     %eax
        shl     $1, %bx                 # pos is 4 bytes a slot
        movl    $0, pos(%bx)
        shr     $2, %bx
        mov     %bx, %ax
        add     $5, %ax
        pop     %cx
        pop     %bx
        pop     %di
        pop     %si
        clc
        ret
7:      pop     %di
        add     $32, %di
        jmp     4b
notfound:
        pop     %cx
        pop     %bx
        pop     %di
        pop     %si
        mov     $2, %ax
        stc
        ret

# entry: AX = handle -> SI = its table entry (0 if not open)
entry:  push    %bx
        mov     %ax, %bx
        sub     $5, %bx
        shl     $1, %bx
        mov     ent(%bx), %si
        pop     %bx
        ret

# readfile: DS = 0, BX = handle, CX = bytes, AX:DX = the buffer -> AX = bytes
# read (fewer at the end of the file), CF set on a disk error.
readfile:
        push    %ax                     # the buffer's segment
        mov     %bx, %ax
        call    entry
        test    %si, %si
        jz      rf_bad
        sub     $5, %bx
        shl     $2, %bx                 # BX = the slot's pos offset
        movzwl  %cx, %ecx
        mov     20(%si), %eax           # what is left of the file
        sub     pos(%bx), %eax
        jbe     rf_zero
        cmp     %ecx, %eax
        jae     1f
        mov     %eax, %ecx
1:      mov     %cx, done_n             # the answer
        movw    $0, done
        pop     %ax
        mov     %ax, dseg
        mov     %dx, doff
rf_loop:
        mov     done_n, %cx
        sub     done, %cx
        jz      rf_ok
        mov     pos(%bx), %eax          # sector and offset of pos
        mov     %eax, %edx
        and     $511, %edx
        shr     $9, %eax
        add     16(%si), %eax           # + the file's first sector
        mov     %eax, rlba
        mov     %dx, roff
        movzwl  %cx, %ecx               # sectors to cover OFF + CX
        add     %edx, %ecx
        add     $511, %ecx
        shr     $9, %ecx
        cmp     $BSECS, %ecx
        jbe     2f
        mov     $BSECS, %ecx
2:      call    readsecs                # ECX sectors from RLBA to BSEG:0 -> CX read
        jc      rf_err
        shl     $9, %cx                 # bytes in the buffer, less OFF
        sub     roff, %cx
        mov     done_n, %ax
        sub     done, %ax
        cmp     %ax, %cx
        jbe     3f
        mov     %ax, %cx
3:      push    %ds                     # copy BSEG:OFF -> DSEG:DOFF+DONE
        push    %si
        mov     dseg, %ax
        mov     %ax, %es
        mov     doff, %di
        add     done, %di
        mov     roff, %si
        mov     $BSEG, %ax
        mov     %ax, %ds
        push    %cx
        rep movsb
        pop     %cx
        pop     %si
        pop     %ds
        add     %cx, done
        movzwl  %cx, %ecx
        add     %ecx, pos(%bx)
        jmp     rf_loop
rf_ok:  mov     done_n, %ax
        clc
        ret
rf_zero: pop    %ax
        xor     %ax, %ax
        clc
        ret
rf_bad: pop     %ax
        mov     $6, %ax
        stc
        ret
rf_err: mov     $5, %ax
        stc
        ret

# readsecs: up to ECX sectors from RLBA to BSEG:0000 -> CX = sectors read (at
# least 1), CF on an error.  LBA: one call.  CHS: to the end of the track.
readsecs:
        push    %bx
        push    %si
        push    %di
        cmpb    $0, lba
        je      rs_chs
        mov     %cx, dap+2
        mov     rlba, %eax
        mov     %eax, dap+8
        mov     $3, %di
1:      mov     $dap, %si
        mov     drive, %dl
        mov     $0x42, %ah
        int     $0x13
        jnc     rs_ok_lba
        xor     %ah, %ah
        mov     drive, %dl
        int     $0x13
        dec     %di
        jnz     1b
        jmp     rs_err
rs_ok_lba:
        mov     dap+2, %cx
        jmp     rs_ok
rs_chs: push    %cx
        mov     rlba, %eax              # sector, head, cylinder
        xor     %edx, %edx
        movzbl  spt, %ecx
        div     %ecx                    # EAX = track, EDX = sector index
        pop     %cx
        mov     spt, %bl                # at most to the end of the track
        xor     %bh, %bh
        sub     %dx, %bx
        cmp     %bx, %cx
        jbe     2f
        mov     %bx, %cx
2:      push    %cx
        mov     %dl, %cl
        inc     %cl                     # sector, 1-based
        xor     %edx, %edx
        movzbl  heads, %ebx
        div     %ebx                    # EAX = cylinder, EDX = head
        mov     %dl, %dh
        mov     %al, %ch
        shl     $6, %ah
        or      %ah, %cl
        pop     %ax                     # AL = sectors
        push    %ax
        mov     $3, %di
3:      push    %ax
        push    %es
        mov     $BSEG, %bx
        mov     %bx, %es
        xor     %bx, %bx
        mov     drive, %dl
        mov     $0x02, %ah
        int     $0x13
        pop     %es
        pop     %ax
        jnc     4f
        push    %ax
        xor     %ah, %ah
        mov     drive, %dl
        int     $0x13
        pop     %ax
        dec     %di
        jnz     3b
        pop     %cx
        jmp     rs_err
4:      pop     %cx
rs_ok:  pop     %di
        pop     %si
        pop     %bx
        clc
        ret
rs_err: pop     %di
        pop     %si
        pop     %bx
        stc
        ret

# --- PM_READ: the image's disk reads (see THE BOOT RECORD above) ------------
        .code32
pm_read:
        pushal
        pushfl
        cli
        mov     %eax, rd_lba
        mov     %ecx, rd_cnt
        mov     %esp, rd_esp
        sgdt    rd_gdt
        sidt    rd_idt
        mov     %cr0, %eax
        mov     %eax, rd_cr0
        mov     %cs, %ax
        mov     %ax, rd_back+4
        mov     %ds, %ax
        mov     %ax, rd_ds
        mov     rd_cr0, %eax            # paging off: this runs identity-mapped
        and     $0x7FFFFFFF, %eax
        mov     %eax, %cr0
        lgdt    rm_gdtr
        ljmp    $0x08, $pm16
        .code16
pm16:   mov     $0x10, %ax              # 16-bit segments, then real mode
        mov     %ax, %ds
        mov     %ax, %es
        mov     %ax, %ss
        mov     %ax, %fs
        mov     %ax, %gs
        mov     %cr0, %eax
        and     $0xFFFFFFFE, %eax
        mov     %eax, %cr0
        ljmp    $0, $rm
rm:     xor     %ax, %ax
        mov     %ax, %ds
        mov     %ax, %es
        mov     %ax, %ss
        mov     $0x7C00, %sp
        lidtl   rm_idtr
        mov     $0x08, %bl              # the BIOS's interrupt mapping
        mov     $0x70, %bh
        mov     $0xBA, %cl              # IRQ 0, 2, 6 (timer, cascade, floppy)
        mov     $0x3F, %ch              # IRQ 14, 15 (the disks)
        call    pic_map
        sti
        mov     rd_lba, %eax
        mov     %eax, rlba
        mov     rd_cnt, %ecx
        call    readsecs                # -> CX sectors, CF
        cli
        setc    rd_err
        mov     %cx, rd_got
        mov     $0x20, %bl              # the image's
        mov     $0x28, %bh
        mov     $0xFA, %cl
        mov     $0xFD, %ch
        call    pic_map
        lgdtl   rd_gdt                  # back: protected mode, its CS
        mov     rd_cr0, %eax
        and     $0x7FFFFFFF, %eax
        mov     %eax, %cr0
        ljmpl   *rd_back
        .code32
pm32back:
        mov     rd_ds, %ax
        mov     %ax, %ds
        mov     %ax, %es
        mov     %ax, %ss
        mov     %ax, %fs
        mov     %ax, %gs
        lidt    rd_idt
        mov     rd_cr0, %eax            # paging back on
        mov     %eax, %cr0
        jmp     1f
1:      mov     rd_esp, %esp
        popfl
        popal
        movzwl  rd_got, %eax
        btw     $0, rd_err              # CF = the error
        ret
        .code16

# pic_map: both 8259s initialised, master at BL, slave at BH, masks CL / CH
pic_map:
        mov     $0x11, %al
        out     %al, $0x20
        out     %al, $0xA0
        mov     %bl, %al
        out     %al, $0x21
        mov     %bh, %al
        out     %al, $0xA1
        mov     $0x04, %al
        out     %al, $0x21
        mov     $0x02, %al
        out     %al, $0xA1
        mov     $0x01, %al
        out     %al, $0x21
        out     %al, $0xA1
        mov     %cl, %al
        out     %al, $0x21
        mov     %ch, %al
        out     %al, $0xA1
        ret

# --- output -----------------------------------------------------------------
putz:   lodsb                           # DS:SI to NUL
        test    %al, %al
        jz      1f
        call    putc
        jmp     putz
1:      ret

putc:   push    %ax                     # AL to the screen and COM1
        push    %bx
        push    %dx
        push    %cx
        mov     %al, %cl
        mov     $0x0E, %ah
        xor     %bx, %bx
        int     $0x10
        mov     $0x3FD, %dx             # COM1, if there is one: wait for THRE
        mov     $0x4000, %bx
1:      in      %dx, %al
        cmp     $0xFF, %al
        je      2f
        test    $0x20, %al
        jnz     3f
        dec     %bx
        jnz     1b
        jmp     2f
3:      mov     $0x3F8, %dx
        mov     %cl, %al
        out     %al, %dx
2:      pop     %cx
        pop     %dx
        pop     %bx
        pop     %ax
        ret

# --- data -------------------------------------------------------------------
msg_boot:  .asciz "MODUS: no DOS -- the disk's own loader\r\n"
msg_nocom: .asciz "MODUS: MODUS.COM is not on this disk, or will not read\r\n"
msg_stop:  .asciz "\r\nMODUS: stopped.\r\n"
name_com:  .asciz "MODUS.COM"

        .p2align 2
dap:    .byte   16, 0
        .word   0                       # sectors
        .word   0, BSEG                 # to BSEG:0
        .long   0, 0                    # from
rlba:   .long   0
roff:   .word   0
dseg:   .word   0
doff:   .word   0
done:   .word   0
done_n: .word   0
drive:  .byte   0
lba:    .byte   0
spt:    .byte   18
heads:  .byte   2
        .p2align 2
ent:    .fill   NHANDLE, 2, 0           # each handle's table entry
pos:    .fill   NHANDLE, 4, 0           # and its position
namebuf: .fill  64, 1, 0

        .p2align 3
rm_gdt: .quad   0
        .quad   0x00009A000000FFFF      # 08h: 16-bit code, base 0, 64 KB
        .quad   0x000092000000FFFF      # 10h: 16-bit data
rm_gdtr: .word  3*8-1
        .long   rm_gdt
rm_idtr: .word  0x3FF
        .long   0
rd_gdt: .word   0
        .long   0
rd_idt: .word   0
        .long   0
        .p2align 2
rd_lba: .long   0
rd_cnt: .long   0
rd_esp: .long   0
rd_cr0: .long   0
rd_back: .long  pm32back
        .word   0
rd_ds:  .word   0
rd_got: .word   0
rd_err: .word   0
