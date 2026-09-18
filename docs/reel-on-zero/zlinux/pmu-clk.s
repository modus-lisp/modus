// bare payload: program the PMU like Modus's stubs, spin, print PMEVCNTR0 (INST_RETIRED|NSH), PMCCNTR, PMEVCNTR1 (INST_RETIRED, NSH=0 control)
.globl _start
_start:
    mov  x20, #0x5040
    movk x20, #0x3f21, lsl #16
    adr  x0, banner
    bl   puts

    // --- raise the ARM clock via the property mailbox (SET_CLOCK_RATE 0x38002, clock 3, 1 GHz) ---
    adr  x5, mbuf
    mov  w1, #36;          str  w1, [x5]
    str  wzr, [x5, #4]
    mov  w1, #0x8002;  movk w1, #0x3, lsl #16;  str w1, [x5, #8]     // tag SET_CLOCK_RATE
    mov  w1, #12;          str  w1, [x5, #12]
    str  wzr, [x5, #16]
    mov  w1, #3;           str  w1, [x5, #20]                        // clock id ARM
    mov  w1, #0xCA00;  movk w1, #0x3B9A, lsl #16;  str w1, [x5, #24] // 1000000000
    str  wzr, [x5, #28]
    str  wzr, [x5, #32]
    dc   civac, x5
    add  x6, x5, #32
    dc   civac, x6
    dsb  sy
    mov  x7, #0xB880
    movk x7, #0x3F00, lsl #16
7:  ldr  w1, [x7, #0x18]
    tbnz w1, #31, 7b                                                  // wait !full
    orr  x1, x5, #8
    orr  x1, x1, #0xC0000000
    str  w1, [x7, #0x20]
8:  ldr  w1, [x7, #0x18]
    tbnz w1, #30, 8b                                                  // wait !empty
    ldr  w1, [x7]
    dc   ivac, x5
    dsb  sy
    ldr  w0, [x5, #24]                                                // rate the firmware reports
    bl   puthex
    // same again with GET_CLOCK_RATE 0x30002 to read it back
    mov  w1, #36;          str  w1, [x5]
    str  wzr, [x5, #4]
    mov  w1, #0x0002;  movk w1, #0x3, lsl #16;  str w1, [x5, #8]
    mov  w1, #12;          str  w1, [x5, #12]
    str  wzr, [x5, #16]
    mov  w1, #3;           str  w1, [x5, #20]
    str  wzr, [x5, #24]
    str  wzr, [x5, #28]
    str  wzr, [x5, #32]
    dc   civac, x5
    dc   civac, x6
    dsb  sy
7:  ldr  w1, [x7, #0x18]
    tbnz w1, #31, 7b
    orr  x1, x5, #8
    orr  x1, x1, #0xC0000000
    str  w1, [x7, #0x20]
8:  ldr  w1, [x7, #0x18]
    tbnz w1, #30, 8b
    ldr  w1, [x7]
    dc   ivac, x5
    dsb  sy
    ldr  w0, [x5, #24]
    bl   puthex
    mov  x1, #0x46                  // PMCR = P|C|LC, E=0
    msr  pmcr_el0, x1
    isb
    mov  x1, #0xffffffff
    msr  pmcntenclr_el0, x1
    msr  pmintenclr_el1, x1
    msr  pmovsclr_el0, x1
    mov  x1, #0x08000000            // PMCCFILTR = NSH
    msr  pmccfiltr_el0, x1
    movz x1, #0x0008                // counter 0: INST_RETIRED | NSH
    movk x1, #0x0800, lsl #16
    msr  pmevtyper0_el0, x1
    mov  x1, #0x00000008            // counter 1: INST_RETIRED, NSH=0 (control: should NOT count at EL2)
    msr  pmevtyper1_el0, x1
    movz x1, #0x0003                // enable cycle counter + counters 0,1
    movk x1, #0x8000, lsl #16
    msr  pmcntenset_el0, x1
    isb
    mov  x1, #0x47                  // E=1
    msr  pmcr_el0, x1
    isb
    mov  x2, #0
    movk x2, #0x000f, lsl #16       // ~1M iterations
6:  add  x3, x3, x2
    subs x2, x2, #1
    b.ne 6b
    isb
    mrs  x0, pmevcntr0_el0
    bl   puthex
    mrs  x0, pmccntr_el0
    bl   puthex
    mrs  x0, pmevcntr1_el0
    bl   puthex
    mrs  x0, pmcntenset_el0
    bl   puthex
    mrs  x0, pmevtyper0_el0
    bl   puthex
    mrs  x0, mdcr_el2
    bl   puthex
    mrs  x0, hcr_el2
    bl   puthex
    adr  x0, done
    bl   puts
1:  b    1b
puts:
    mov  x22, x30
    mov  x21, x0
2:  ldrb w0, [x21], #1
    cbz  w0, 3f
    bl   putc
    b    2b
3:  ret  x22
puthex:
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
putc:
5:  ldr  w1, [x20, #0x14]
    tbz  w1, #5, 5b
    str  w0, [x20]
    ret
banner: .asciz "\r\nPMU-CLK-PAYLOAD setrate getrate evcnt0(inst|nsh) ccntr evcnt1(inst,nsh=0) cntenset typer0 mdcr_el2 hcr_el2: "
done:   .asciz "\r\nPMU-CLK-END\r\n"
.balign 16
mbuf: .space 64
