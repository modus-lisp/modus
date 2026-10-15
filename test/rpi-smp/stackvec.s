// Second prefix of the bare Pi's synchronous-exception reporter (threaded builds),
// after guardvec.s: a DATA ABORT (EC 0x25) whose fault address is in the thread-stack
// area [0x05000000, 0x06000000) is a stack running into its guard page.  Resume at the
// Lisp function net/bare-threads.lisp left at 0x10213030 (it signals a
// STORAGE-CONDITION on the thread), with SP 32 KB above the guard page -- inside the
// thread's own deepest frames, which the error abandons.
        .text
        .global stackvec
stackvec:
        mrs     x10, esr_el2
        lsr     x10, x10, #26
        cmp     x10, #0x25
        b.ne    report
        mrs     x10, far_el2
        lsr     x11, x10, #24
        cmp     x11, #5
        b.ne    report
        movz    x11, #0x1021, lsl #16
        movk    x11, #0x3030
        ldr     x11, [x11]
        cbz     x11, report
        and     x10, x10, #0xFFFFFFFFFFFFF000
        add     x10, x10, #0x9000
        mov     sp, x10
        msr     elr_el2, x11
        eret
report:
tlbi:
        dsb     ishst
        tlbi    alle2is
        dsb     ish
        isb
        ret
