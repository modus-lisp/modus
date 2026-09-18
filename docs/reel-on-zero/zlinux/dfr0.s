// bare payload for U-Boot `go`: print ID_AA64DFR0_EL1, MIDR_EL1, CurrentEL, PMCR_EL0 as hex on the mini-UART
.globl _start
_start:
    mov  x20, #0x5040
    movk x20, #0x3f21, lsl #16
    adr  x0, banner
    bl   puts
    mrs  x0, id_aa64dfr0_el1
    bl   puthex
    mrs  x0, midr_el1
    bl   puthex
    mrs  x0, CurrentEL
    bl   puthex
    mrs  x0, pmcr_el0
    bl   puthex
    mrs  x0, pmceid0_el0
    bl   puthex
    adr  x0, done
    bl   puts
1:  b    1b
puts:                            // x0 = string
    mov  x22, x30
    mov  x21, x0
2:  ldrb w0, [x21], #1
    cbz  w0, 3f
    bl   putc
    b    2b
3:  ret  x22
puthex:                          // x0 = value; prints 16 hex digits + space
    mov  x23, x30
    mov  x24, x0
    mov  x25, #60
4:  lsr  x0, x24, x25
    and  x0, x0, #0xf
    cmp  x0, #10
    add  x1, x0, #'0'
    add  x2, x0, #('a'-10)
    csel x0, x1, x2, lo
    bl   putc
    subs x25, x25, #4
    b.pl 4b
    mov  x0, #' '
    bl   putc
    ret  x23
putc:                            // w0 = char; wait AUX_MU_LSR bit 5
5:  ldr  w1, [x20, #0x14]
    tbz  w1, #5, 5b
    str  w0, [x20]
    ret
banner: .asciz "\r\nDFR0-PAYLOAD dfr0 midr currentel pmcr pmceid0: "
done:   .asciz "\r\nDFR0-END\r\n"
