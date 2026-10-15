// Preempt a slot from inside the collector entry (translate-aarch64 STW-ENTRY):
// mark it runnable, clear its deadline, and switch to its core's scheduler by
// tail-calling the switch at -0x200.  Returns (through the switch) when the
// scheduler runs the slot again.  Clobbers x9-x17.
        .text
        .global preempt
preempt:
        mrs     x9, tpidr_el0
        movz    x10, #0x1000, lsl #16
        movk    x10, #0xFFE8
        add     x10, x10, x9
        ldr     x10, [x10]
        str     xzr, [x10, #0x3F00]
        ldr     x11, [x10, #16]
        lsr     x11, x11, #1
        movz    x12, #0x1021, lsl #16
        movk    x12, #0x3100
        add     x13, x12, x11, lsl #7
        ldr     w14, [x13, #0x20]
        add     x15, x12, x14, lsl #7
        mov     w16, #1
        str     w16, [x13, #0x60]
        movz    x17, #0x1064, lsl #16
        add     x17, x17, x14, lsl #5
        stp     x13, x15, [x17]
        movz    x16, #0x1000, lsl #16
        movk    x16, #0x0F08
        add     x9, x16, x11, lsl #3
        add     x10, x16, x14, lsl #3
        stp     x9, x10, [x17, #16]
        b       .-0x25c          // the switch, at -0x200 from this routine
