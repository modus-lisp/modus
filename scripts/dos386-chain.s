# dos386-chain.s -- MODUS.COM, the DOS program that starts modus.
# (scripts/dos386-com.sh builds it.)
#
# The DOS counterpart of scripts/mac68k-chain.s: run from DOS, it reads a
# modus image off the disk to its address and jumps to it, and DOS is gone
# from then on -- there is no way back but a reboot.  The image is a
# MULTIBOOT1 KERNEL, the same file QEMU's -kernel boots (boot/boot-i386.lisp:
# a 32-byte header with the a.out kludge, loaded at 1 MB), so this is a
# minimal Multiboot loader that happens to run on DOS:
#
#   MODUS [IMAGE [ARGS...]]       IMAGE defaults to MODUS.BIN; ARGS become
#                                 the multiboot command line
#
# With no FPU (a 386 or 486SX without a coprocessor) it loads IMAGE's
# soft-float sibling instead: the same name with the extension .SF
# (C:\MODUS.BIN -> C:\MODUS.SF).  An x87 image on such a machine would compute
# garbage, not fault: the ESC opcodes do nothing there.
#
#   1. a 386 or better, in REAL mode (not V86: no EMM386 or other memory
#      manager that puts DOS in a virtual 8086 task)
#   2. find the multiboot header in the image's first 8 KB; take its
#      load_addr / load_end_addr / bss_end_addr / entry_addr (the a.out
#      kludge is required: there is no ELF reader here)
#   3. extended memory from the BIOS (INT 15h E801, else 88h, else CMOS),
#      and A20 on (INT 15h 2401, else the 8042's output port, else port 92h),
#      checked by the wrap-around test
#   4. read the image in 32 KB pieces through DOS, copying each one above
#      1 MB through FS in UNREAL mode (FS's descriptor cache keeps a 4 GB
#      limit after the return to real mode), then zero the BSS
#   5. the multiboot information (flags: memory, command line, loader name)
#      in this program's segment, then protected mode on a flat GDT and a jump
#      to the entry with EAX = 2BADB002h and EBX = the information's address,
#      interrupts off and paging off: the state QEMU's -kernel leaves.
#
# DOS=HIGH: DOS itself may live in the HMA, [100000h, 10FFF0h), which is
# exactly where a modus image loads, and DOS is still being called to read the
# rest of the file.  So everything bound for [100000h, 110000h) goes to a
# staging block in conventional memory (the 64 KB after this program's) and is
# copied into place after the last DOS call.
#
# Assemble: as --32 -o c.o dos386-chain.s
#           ld -m elf_i386 -Ttext=0x100 --oformat binary -o MODUS.COM c.o

        .code16
        .text
        .globl  _start

# this segment's layout past the code (a .COM owns its 64 KB segment)
        .equ    MBI,     0x1800         # multiboot information, 88 bytes
        .equ    CMDLINE, 0x1900         # the command line, 128 bytes
        .equ    FNAME,   0x1980         # the image's file name, 128 bytes
        .equ    BUF,     0x2000         # the read buffer (and header scan)
        .equ    BUFSZ,   0x8000
        .equ    HDRSCAN, 8192

        .equ    HMA_LO,  0x100000
        .equ    HMA_HI,  0x110000

_start:
        cld
        mov     $msg_hello, %dx
        call    puts

# --- 1. a 386, in real mode ----------------------------------------------
        pushf
        pop     %bx                     # the caller's flags, kept
        xor     %ax, %ax                # 8086/8088: bits 12-15 stick at 1
        push    %ax
        popf
        pushf
        pop     %ax
        and     $0xF000, %ax
        cmp     $0xF000, %ax
        je      not386
        mov     $0x7000, %ax            # 286 in real mode: bits 12-14 stick at 0
        push    %ax
        popf
        pushf
        pop     %ax
        push    %bx
        popf
        test    $0x7000, %ax
        jz      not386

        smsw    %ax
        test    $1, %al
        jnz     inv86

# --- the command line: IMAGE, then ARGS ----------------------------------
        mov     $0x81, %si
        mov     $FNAME, %di
        call    skipsp
        cmpb    $0x0D, (%si)
        je      default_name
1:      lodsb
        cmp     $' ', %al
        je      2f
        cmp     $0x09, %al
        je      2f
        cmp     $0x0D, %al
        je      3f
        stosb
        jmp     1b
3:      dec     %si
2:      movb    $0, (%di)
        jmp     args
default_name:
        push    %si
        mov     $default_image, %si
1:      lodsb
        stosb
        test    %al, %al
        jnz     1b
        pop     %si
args:
        call    fpu_pick
        call    skipsp
        mov     $CMDLINE, %di
        mov     $FNAME, %bx             # the command line starts with the
1:      mov     (%bx), %al              # image's name, as GRUB's does
        inc     %bx
        test    %al, %al
        jz      2f
        stosb
        jmp     1b
2:      cmpb    $0x0D, (%si)
        je      4f
        mov     $' ', %al
        stosb
3:      lodsb
        cmp     $0x0D, %al
        je      4f
        stosb
        jmp     3b
4:      movb    $0, (%di)

# --- 2. open the image, find its multiboot header -------------------------
        mov     $0x3D00, %ax
        mov     $FNAME, %dx
        int     $0x21
        jc      e_open
        mov     %ax, handle

        mov     $0x3F, %ah
        mov     handle, %bx
        mov     $HDRSCAN, %cx
        mov     $BUF, %dx
        int     $0x21
        jc      e_read
        cmp     $32, %ax
        jb      e_nohdr
        mov     %ax, %cx
        sub     $32, %cx                # last offset a 32-byte header fits at
        xor     %si, %si
scan:
        cmpl    $0x1BADB002, BUF(%si)
        jne     next
        mov     BUF(%si), %eax
        add     BUF+4(%si), %eax
        add     BUF+8(%si), %eax
        jz      found
next:   add     $4, %si
        cmp     %cx, %si
        jbe     scan
        jmp     e_nohdr
found:
        testl   $0x10000, BUF+4(%si)
        jz      e_noaout
        mov     BUF+12(%si), %eax       # header_addr
        mov     BUF+16(%si), %ebx       # load_addr
        mov     %ebx, load_addr
        mov     %ebx, dest
        sub     %ebx, %eax              # header_addr - load_addr
        movzx   %si, %edx
        sub     %eax, %edx              # the file offset load_addr comes from
        jc      e_badhdr
        mov     %edx, file_off
        mov     BUF+20(%si), %eax
        mov     %eax, load_end
        mov     BUF+24(%si), %eax
        mov     %eax, bss_end
        mov     BUF+28(%si), %eax
        mov     %eax, entry
        cmpl    $HMA_LO, load_addr      # nothing of DOS's below 1 MB is ours
        jb      e_low

# --- 3. memory, A20 ---------------------------------------------------------
        mov     $0xE801, %ax
        xor     %cx, %cx
        xor     %dx, %dx
        int     $0x15
        jc      mem88
        jcxz    1f                      # some BIOSes answer in AX/BX only
        mov     %cx, %ax
        mov     %dx, %bx
1:      movzx   %ax, %eax               # KB from 1 MB to 16 MB
        movzx   %bx, %ebx               # 64 KB blocks above 16 MB
        shl     $6, %ebx
        add     %ebx, %eax
        jmp     memdone
mem88:  mov     $0x88, %ah
        int     $0x15
        jc      memcmos
        test    %ax, %ax
        jz      memcmos
        movzx   %ax, %eax
        jmp     memdone
# Neither: some BIOSes answer both with carry set (measured: 86Box's
# 486vipio2, Award 4.50PG, under FreeDOS).  Every AT keeps the count POST
# found in CMOS 30h/31h (KB above 1 MB, to 64 MB), the configured one in
# 17h/18h.
memcmos:
        mov     $0x31, %al
        call    cmosrd
        mov     %al, %ah
        mov     $0x30, %al
        call    cmosrd
        test    %ax, %ax
        jnz     1f
        mov     $0x18, %al
        call    cmosrd
        mov     %al, %ah
        mov     $0x17, %al
        call    cmosrd
        test    %ax, %ax
        jz      e_mem
1:      movzx   %ax, %eax
memdone:
        mov     %eax, mem_upper
        int     $0x12                   # conventional memory, KB
        movzx   %ax, %eax
        mov     %eax, mem_lower

        mov     mem_upper, %eax         # the image must fit: its end
        shl     $10, %eax               # (BSS included) under 1 MB + extended
        add     $HMA_LO, %eax
        mov     bss_end, %ebx
        cmp     load_end, %ebx
        jae     1f
        mov     load_end, %ebx
1:      cmp     %eax, %ebx
        ja      e_toobig

        call    a20_on
        jnz     e_a20

# the staging block for the HMA: the 64 KB after this program's segment
        mov     %ds, %ax
        movzx   %ax, %eax
        add     $0x1000, %eax
        mov     %ax, %es
        shl     $4, %eax
        mov     %eax, stage
        xor     %di, %di                # zero it: the HMA part of a BSS is
        xor     %ax, %ax                # never read, only staged
        mov     $0x8000, %cx
        rep     stosw
        push    %ds
        pop     %es

        mov     %ds, %ax
        movzx   %ax, %eax
        shl     $4, %eax
        mov     %eax, linear            # this segment's linear address
        add     %eax, gdtr_base
        add     %eax, pm_jump

        call    unreal

# --- 4. read the image to its address ---------------------------------------
        mov     $msg_loading, %dx
        call    puts
        mov     $0x4200, %ax            # seek to load_addr's byte
        mov     handle, %bx
        mov     file_off, %dx
        mov     file_off+2, %cx
        int     $0x21
        jc      e_read
readloop:
        mov     $BUFSZ, %ecx
        mov     load_end, %eax
        test    %eax, %eax
        jz      1f                      # 0: the rest of the file
        sub     dest, %eax
        jz      readdone
        cmp     %ecx, %eax
        jae     1f
        mov     %eax, %ecx
1:      mov     $0x3F, %ah
        mov     handle, %bx
        mov     $BUF, %dx
        int     $0x21
        jc      e_read
        test    %ax, %ax
        jz      readeof
        movzx   %ax, %ecx
        push    %ecx
        mov     linear, %esi
        add     $BUF, %esi
        mov     dest, %edi
        call    copyup
        pop     %ecx
        add     %ecx, dest
        mov     $'.', %dl
        mov     $0x02, %ah
        int     $0x21
        jmp     readloop
readeof:
        cmpl    $0, load_end            # short of load_end_addr: a cut file
        jne     e_short
readdone:
        mov     $0x3E, %ah
        mov     handle, %bx
        int     $0x21

        mov     dest, %edi              # the BSS: [end of the data, bss_end)
        mov     bss_end, %ecx
        sub     %edi, %ecx
        jbe     1f
        call    zeroup
1:
        mov     $msg_go, %dx            # the last DOS call
        call    puts

# --- 5. the multiboot information, then the jump -----------------------------
        mov     $MBI, %di
        xor     %ax, %ax
        mov     $44, %cx
        rep     stosw
        movl    $0x205, MBI             # mem_*, cmdline, boot_loader_name
        mov     mem_lower, %eax
        mov     %eax, MBI+4
        mov     mem_upper, %eax
        mov     %eax, MBI+8
        mov     linear, %eax
        add     $CMDLINE, %eax
        mov     %eax, MBI+16
        mov     linear, %eax
        add     $loader_name, %eax
        mov     %eax, MBI+64

        cli
        call    unreal                  # FS again: DOS may have reloaded it
        mov     stage, %esi             # the HMA's 64 KB, from staging
        mov     $HMA_LO, %edi
        mov     $0x4000, %ecx
1:      mov     %fs:(%esi), %eax
        mov     %eax, %fs:(%edi)
        add     $4, %esi
        add     $4, %edi
        dec     %ecx
        jnz     1b

        mov     linear, %ebx
        add     $MBI, %ebx              # EBX: the multiboot information
        mov     linear, %ebp
        add     $0xFFF0, %ebp           # a stack, until the kernel sets its own
        mov     entry, %edx
        lgdt    gdtr
        mov     %cr0, %eax
        or      $1, %al
        mov     %eax, %cr0
        ljmpl   *pm_jump

        .code32
pm32:   mov     $0x10, %ax
        mov     %ax, %ds
        mov     %ax, %es
        mov     %ax, %fs
        mov     %ax, %gs
        mov     %ax, %ss
        mov     %ebp, %esp
        mov     $0x2BADB002, %eax
        jmp     *%edx
        .code16

# --- helpers -----------------------------------------------------------------

# copyup: ECX bytes from linear ESI to linear EDI, a dword at a time (the
# buffer has slack past the bytes read), anything bound for the HMA going to
# its staging block instead.
copyup:
        add     $3, %ecx
        shr     $2, %ecx
        jz      9f
1:      mov     %fs:(%esi), %eax
        mov     %edi, %ebx
        cmp     $HMA_LO, %ebx
        jb      2f
        cmp     $HMA_HI, %ebx
        jae     2f
        sub     $HMA_LO, %ebx
        add     stage, %ebx
2:      mov     %eax, %fs:(%ebx)
        add     $4, %esi
        add     $4, %edi
        dec     %ecx
        jnz     1b
9:      ret

# zeroup: ECX bytes of zero at linear EDI (HMA bytes are already zero in
# staging, so they are skipped).
zeroup:
        add     $3, %ecx
        shr     $2, %ecx
        jz      9f
        xor     %eax, %eax
1:      cmp     $HMA_LO, %edi
        jb      2f
        cmp     $HMA_HI, %edi
        jb      3f
2:      mov     %eax, %fs:(%edi)
3:      add     $4, %edi
        dec     %ecx
        jnz     1b
9:      ret

# unreal: FS gets a 4 GB limit, base 0, and keeps it back in real mode.
unreal:
        pushf
        cli
        push    %eax
        lgdt    gdtr
        mov     %cr0, %eax
        or      $1, %al
        mov     %eax, %cr0
        jmp     1f
1:      mov     $0x10, %ax
        mov     %ax, %fs
        mov     %cr0, %eax
        and     $0xFE, %al
        mov     %eax, %cr0
        jmp     2f
2:      xor     %ax, %ax
        mov     %ax, %fs
        pop     %eax
        popf
        ret

# a20_on: ZF set when A20 is on.  INT 15h 2401 first, then the fast gate.
a20_on:
        call    a20_test
        jz      9f
        mov     $0x2401, %ax
        int     $0x15
        call    a20_test
        jz      9f
        call    kbc_wait                # the keyboard controller's output
        mov     $0xD1, %al              # port: write it with bit 1 (A20) on
        out     %al, $0x64
        call    kbc_wait
        mov     $0xDF, %al
        out     %al, $0x60
        call    kbc_wait
        call    a20_settle
        jz      9f
        in      $0x92, %al
        test    $2, %al
        jnz     1f
        or      $2, %al
        and     $0xFE, %al
        out     %al, $0x92
1:      call    a20_settle
9:      ret

# a20_settle: a20_test until A20 is on or 64K tries have passed; a gate the
# keyboard controller drives can take a while to follow.
a20_settle:
        push    %cx
        xor     %cx, %cx
1:      call    a20_test
        jz      2f
        loop    1b
        or      $1, %ax                 # ZF=0: still off
2:      pop     %cx
        ret

# kbc_wait: until the 8042's input buffer is empty (status bit 1), at most
# 64K reads: a machine with no 8042 must not hang here.
kbc_wait:
        push    %cx
        xor     %cx, %cx
1:      in      $0x64, %al
        test    $2, %al
        loopnz  1b
        pop     %cx
        ret

# a20_test: ZF set when 0000:0500 and FFFF:0510 (= 100500h) differ, i.e. the
# address does not wrap.
a20_test:
        push    %ds
        push    %es
        xor     %ax, %ax
        mov     %ax, %ds
        not     %ax
        mov     %ax, %es
        mov     0x0500, %bl             # keep both bytes
        mov     %es:0x0510, %bh
        movb    $0x00, 0x0500
        movb    $0xFF, %es:0x0510
        cmpb    $0xFF, 0x0500           # wrapped: the write landed at 500h
        mov     %bh, %es:0x0510
        mov     %bl, 0x0500
        pop     %es
        pop     %ds
        je      1f
        xor     %ax, %ax                # ZF=1: on
        ret
1:      or      $1, %ax                 # ZF=0: off
        ret

# fpu_pick: with no FPU, FNAME's extension becomes .SF, and say so.
# The usual probe: FNINIT, then FNSTSW must store 0 and FNSTCW the reset
# control word 037Fh (masked to 103Fh: 003Fh).  Without an FPU the stores
# do not happen and the planted 5A5Ah stays.  SI is kept.
fpu_pick:
        fninit
        movw    $0x5A5A, fpu_word
        fnstsw  fpu_word
        cmpb    $0, fpu_word
        jne     1f
        fnstcw  fpu_word
        mov     fpu_word, %ax
        and     $0x103F, %ax
        cmp     $0x3F, %ax
        jne     1f
        ret
1:      push    %si
        mov     $FNAME, %di             # DI = after the last '.' of the last
        xor     %bx, %bx                # path component, or the end
2:      mov     (%di), %al
        test    %al, %al
        jz      4f
        cmp     $'.', %al
        jne     3f
        mov     %di, %bx
3:      cmp     $'\\', %al
        jne     5f
        xor     %bx, %bx
5:      inc     %di
        jmp     2b
4:      test    %bx, %bx
        jz      6f
        mov     %bx, %di
6:      movl    $0x0046532E, (%di)      # ".SF", 0
        mov     $msg_nofpu, %dx
        call    puts
        mov     $FNAME, %si
7:      lodsb
        test    %al, %al
        jz      8f
        mov     %al, %dl
        mov     $0x02, %ah
        int     $0x21
        jmp     7b
8:      mov     $msg_crlf, %dx
        call    puts
        pop     %si
        ret

# cmosrd: AL = CMOS register -> AL = its value (AH kept)
cmosrd: out     %al, $0x70
        jmp     1f                      # a short delay between the ports
1:      in      $0x71, %al
        ret

skipsp: cmpb    $' ', (%si)
        je      1f
        cmpb    $0x09, (%si)
        jne     2f
1:      inc     %si
        jmp     skipsp
2:      ret

puts:   mov     $0x09, %ah
        int     $0x21
        ret

# errors: say so, exit 1
not386:  mov    $e_not386, %dx
        jmp     fail
inv86:   mov    $e_v86, %dx
        jmp     fail
e_open:  mov    $m_open, %dx
        jmp     fail
e_read:  mov    $m_read, %dx
        jmp     fail
e_nohdr: mov    $m_nohdr, %dx
        jmp     fail
e_noaout: mov   $m_noaout, %dx
        jmp     fail
e_badhdr: mov   $m_badhdr, %dx
        jmp     fail
e_low:   mov    $m_low, %dx
        jmp     fail
e_mem:   mov    $m_mem, %dx
        jmp     fail
e_toobig: mov   $m_toobig, %dx
        jmp     fail
e_a20:   mov    $m_a20, %dx
        jmp     fail
e_short: mov    $m_short, %dx
fail:   push    %dx
        mov     $msg_err, %dx
        call    puts
        pop     %dx
        call    puts
        mov     $0x4C01, %ax
        int     $0x21

# --- data ------------------------------------------------------------------
        .p2align 3
gdt:    .quad   0
        .quad   0x00CF9A000000FFFF      # 08h: code, flat, 32-bit
        .quad   0x00CF92000000FFFF      # 10h: data, flat, 4 GB
gdtr:   .word   gdtr - gdt - 1
gdtr_base: .long gdt                    # + this segment's linear address
pm_jump: .long  pm32                    # + this segment's linear address
        .word   0x08

        .p2align 2
handle:    .word 0
           .word 0
load_addr: .long 0
load_end:  .long 0
bss_end:   .long 0
entry:     .long 0
file_off:  .long 0
dest:      .long 0
mem_lower: .long 0
mem_upper: .long 0
linear:    .long 0
stage:     .long 0
fpu_word:  .word 0

default_image: .asciz "MODUS.BIN"
loader_name:   .asciz "modus.com (DOS)"

msg_hello:   .ascii "MODUS: DOS loader\r\n$"
msg_loading: .ascii "MODUS: loading$"
msg_nofpu:   .ascii "MODUS: no FPU, so the soft-float image $"
msg_crlf:    .ascii "\r\n$"
msg_go:      .ascii "\r\nMODUS: starting\r\n$"
msg_err:     .ascii "\r\nMODUS: $"
e_not386:    .ascii "needs a 386 or later\r\n$"
e_v86:       .ascii "the CPU is in virtual 8086 mode: boot without EMM386 or another memory manager\r\n$"
m_open:      .ascii "cannot open the image (MODUS [IMAGE [ARGS]], default MODUS.BIN)\r\n$"
m_read:      .ascii "cannot read the image\r\n$"
m_nohdr:     .ascii "no multiboot header in the image's first 8 KB\r\n$"
m_noaout:    .ascii "the multiboot header has no load addresses (a.out kludge)\r\n$"
m_badhdr:    .ascii "the multiboot header's addresses do not match its place in the file\r\n$"
m_low:       .ascii "the image loads below 1 MB\r\n$"
m_mem:       .ascii "the BIOS does not say how much extended memory there is\r\n$"
m_toobig:    .ascii "not enough extended memory for the image\r\n$"
m_a20:       .ascii "cannot enable the A20 line\r\n$"
m_short:     .ascii "the image file is shorter than its header says\r\n$"
# (dos386-com.sh checks that the file ends below MBI)
