    adr x0, params
    ldr x1, [x0, #0]
    msr mair_el2, x1
    ldr x1, [x0, #8]
    msr tcr_el2, x1
    ldr x1, [x0, #16]
    msr ttbr0_el2, x1
    tlbi alle2
    dsb sy
    isb
    mrs x1, S3_1_C15_C2_1
    orr x1, x1, #0x40
    msr S3_1_C15_C2_1, x1
    ldr x1, [x0, #32]
    msr vbar_el2, x1
    msr vbar_el1, x1
    mrs x1, cptr_el2
    bic x1, x1, #0x400
    msr cptr_el2, x1
    mov x1, #0x300000
    msr cpacr_el1, x1
    isb
    ldr x1, [x0, #24]
    msr sctlr_el2, x1
    isb
    ldr x1, [x0, #48]
    mov sp, x1
    ldr x1, [x0, #56]
    msr tpidr_el1, x1
    ldr x1, [x0, #64]
    msr tpidr_el0, x1
    ldr x28, [x0, #40]
    ldr x24, [x0, #72]
    ldr x25, [x0, #80]
    movz x18, #0x1000, lsl #16
    movz x26, #0x0001
    movk x26, #0xdead, lsl #16
    mov x1, #1
    str x1, [x0, #96]
    ldr x9, [x0, #88]
    mov x29, #0
    mov x30, #0
    blr x9
    adr x0, params
    mov x1, #2
    str x1, [x0, #96]
1:  wfe
    b 1b
    .balign 2048
params:
    movz x0, #0
    movk x0, #0, lsl #16
    mrs x1, mair_el2
    str x1, [x0, #0]
    mrs x1, tcr_el2
    str x1, [x0, #8]
    mrs x1, ttbr0_el2
    str x1, [x0, #16]
    mrs x1, sctlr_el2
    str x1, [x0, #24]
    mrs x1, vbar_el2
    str x1, [x0, #32]
    str x28, [x0, #40]
    mrs x1, currentel
    str x1, [x0, #104]
    ret
    movz x0, #0
    movk x0, #0, lsl #16
    add x2, x0, #0x900
1:  dc civac, x0
    add x0, x0, #64
    cmp x0, x2
    b.lo 1b
    mov x0, #0xc0
    dc civac, x0
    dsb sy
    sev
    ret
