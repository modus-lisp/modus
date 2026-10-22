// Prefix of the bare Pi's synchronous-exception reporter (threaded builds): an
// undefined instruction (EC 0) whose thread's shared-store-guard marker (window
// +0x5058 = 1) is set is the GUARD, not a crash -- resume at the Lisp function whose
// raw entry net/bare-threads.lisp left at 0x10213028, which signals the error.
        .text
        .global guardvec
guardvec:
        mrs     x10, esr_el2
        lsr     x10, x10, #26
        cbnz    x10, report
        movz    x11, #0x1021, lsl #16
        movk    x11, #0x3028
        ldr     x11, [x11]
        cbz     x11, report
        mrs     x10, tpidr_el0
        movz    x12, #0x1000, lsl #16
        movk    x12, #0x5058
        add     x12, x12, x10
        ldr     x12, [x12]
        cmp     x12, #1
        b.ne    report
        msr     elr_el2, x11
        eret
report:
