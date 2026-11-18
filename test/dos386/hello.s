# hello.s -- a multiboot test kernel for MODUS.COM (scripts/dos386-chain.s).
#
# Says on COM1 what the loader handed it and checks what it loaded:
#   magic=2BADB002 flags=... lower=... upper=... cmd=... name=...
#   data ok|BAD AT xxxxxxxx      every word of a 512 KB block that crosses
#                                the HMA staging boundary (100000h..110000h)
#                                and several 32 KB read pieces
#   bss ok|BAD                   64 KB past the file, zeroed by the loader
# then halts.  Built by test/dos386/run-hello.sh.
        .code32
        .text
        .globl  start
        .equ    COM1, 0x3F8
        .equ    NWORDS, 131072
        .equ    BSSLEN, 0x10000
hdr:    .long   0x1BADB002
        .long   0x00010003
        .long   -(0x1BADB002 + 0x00010003)
        .long   hdr, hdr, data_end, bss_end, start

start:  cli
        mov     $0x90000, %esp
        mov     %eax, %esi              # magic
        mov     %ebx, %ebp              # mbi
        mov     $COM1+1, %dx            # 8N1, 115200, no interrupts
        xor     %al, %al
        out     %al, %dx
        mov     $COM1+3, %dx
        mov     $0x80, %al
        out     %al, %dx
        mov     $COM1, %dx
        mov     $1, %al
        out     %al, %dx
        mov     $COM1+1, %dx
        xor     %al, %al
        out     %al, %dx
        mov     $COM1+3, %dx
        mov     $3, %al
        out     %al, %dx

        mov     $s_magic, %ebx;  call puts; mov %esi, %eax; call hex
        mov     $s_flags, %ebx;  call puts; mov 0(%ebp), %eax; call hex
        mov     $s_lower, %ebx;  call puts; mov 4(%ebp), %eax; call hex
        mov     $s_upper, %ebx;  call puts; mov 8(%ebp), %eax; call hex
        mov     $s_cmd, %ebx;    call puts; mov 16(%ebp), %ebx; call puts
        mov     $s_name, %ebx;   call puts; mov 64(%ebp), %ebx; call puts
        mov     $s_nl, %ebx;     call puts

        mov     $s_data, %ebx;   call puts
        mov     $block, %edi
        mov     $0x12345678, %eax
        mov     $NWORDS, %ecx
1:      cmp     %eax, (%edi)
        jne     bad
        add     $0x9E3779B9, %eax
        add     $4, %edi
        dec     %ecx
        jnz     1b
        mov     $s_ok, %ebx;     call puts
        jmp     2f
bad:    mov     $s_badat, %ebx;  call puts; mov %edi, %eax; call hex
        mov     $s_nl, %ebx;     call puts
2:      mov     $s_bss, %ebx;    call puts
        mov     $data_end, %edi
        mov     $BSSLEN/4, %ecx
1:      cmpl    $0, (%edi)
        jne     3f
        add     $4, %edi
        dec     %ecx
        jnz     1b
        mov     $s_ok, %ebx;     call puts
        jmp     4f
3:      mov     $s_bad, %ebx;    call puts
4:      mov     $s_end, %ebx;    call puts
5:      hlt
        jmp     5b

putc:   push    %edx
        push    %eax
        mov     $COM1+5, %dx
1:      in      %dx, %al
        test    $0x20, %al
        jz      1b
        pop     %eax
        mov     $COM1, %dx
        out     %al, %dx
        pop     %edx
        ret
puts:   mov     (%ebx), %al
        test    %al, %al
        jz      1f
        call    putc
        inc     %ebx
        jmp     puts
1:      ret
hex:    mov     $8, %ecx
1:      rol     $4, %eax
        push    %eax
        and     $15, %al
        add     $'0', %al
        cmp     $'9', %al
        jbe     2f
        add     $7, %al
2:      call    putc
        pop     %eax
        dec     %ecx
        jnz     1b
        ret

s_magic: .asciz "magic="
s_flags: .asciz " flags="
s_lower: .asciz " lower="
s_upper: .asciz " upper="
s_cmd:   .asciz "\r\ncmd="
s_name:  .asciz " name="
s_nl:    .asciz "\r\n"
s_data:  .asciz "data "
s_bss:   .asciz "bss "
s_ok:    .asciz "ok\r\n"
s_bad:   .asciz "BAD\r\n"
s_badat: .asciz "BAD AT "
s_end:   .asciz "HELLO-DONE\r\n"

        .p2align 4
block:
        .set    v, 0x12345678
        .rept   NWORDS
        .long   v
        .set    v, (v + 0x9E3779B9) & 0xFFFFFFFF
        .endr
data_end:
        .set    bss_end, data_end + BSSLEN
