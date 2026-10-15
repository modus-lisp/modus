// Slot switch for net/bare-threads.lisp, called with %JIT-CALL (no arguments).
// The per-core mailbox at 0x10640000 + 16*core holds [from-record, to-record].
// Pushes the registers a Lisp caller may hold, then PUBLISHES the outgoing slot
// as SAFE with that SP (so a collector scans the pushed registers), switches
// TPIDR_EL0 (the window, which carries the per-CPU pointer) and SP to the
// incoming slot, marks it RUNNING and honours a stop-the-world flag.
        .text
        .global switch
switch:
        sub     sp, sp, #128
        stp     x1, x2, [sp, #0]
        stp     x3, x4, [sp, #16]
        stp     x5, x6, [sp, #32]
        stp     x7, x8, [sp, #48]
        stp     x19, x20, [sp, #64]
        stp     x21, x22, [sp, #80]
        stp     x23, x27, [sp, #96]
        stp     x29, x30, [sp, #112]
        mrs     x9, mpidr_el1
        and     x9, x9, #3
        movz    x10, #0x1064, lsl #16
        add     x10, x10, x9, lsl #4
        ldp     x11, x12, [x10]
        mov     x13, sp
        str     x13, [x11, #0x70]
        ldr     x14, [x11, #0x40]
        cbz     x14, 1f
        lsl     x15, x24, #1
        str     x15, [x14, #0x30]
        lsl     x15, x25, #1
        str     x15, [x14, #0x38]
1:      dmb     ish
        mov     x15, #2
        str     x15, [x11, #0x68]
        ldr     x13, [x12, #0x38]
        msr     tpidr_el0, x13
        ldr     x13, [x12, #0x70]
        mov     sp, x13
        movz    x16, #0x1000, lsl #16
        movk    x16, #0x0FC8
2:      mov     x15, #3
        str     x15, [x12, #0x68]
        dmb     ish
        ldar    x17, [x16]
        cbz     x17, 4f
        mov     x15, #2
        dmb     ish
        str     x15, [x12, #0x68]
3:      yield
        ldar    x17, [x16]
        cbnz    x17, 3b
        b       2b
4:      ldp     x1, x2, [sp, #0]
        ldp     x3, x4, [sp, #16]
        ldp     x5, x6, [sp, #32]
        ldp     x7, x8, [sp, #48]
        ldp     x19, x20, [sp, #64]
        ldp     x21, x22, [sp, #80]
        ldp     x23, x27, [sp, #96]
        ldp     x29, x30, [sp, #112]
        add     sp, sp, #128
        mov     x0, #0
        ret
