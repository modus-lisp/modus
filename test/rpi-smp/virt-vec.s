// The QEMU-virt (EL1) twins of guardvec.s + stackvec.s: the threaded fault-vector
// prefix, parked in lower-EL vector slots 9-10 (this image never takes those).
// Entry 4 branches here first; REPORT continues at entry 4's normal recovery (the
// branch back is emitted by boot-aarch64.lisp, it depends on where this lands).
        mrs     x10, esr_el1
        lsr     x10, x10, #26
        cbnz    x10, stack
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
        msr     elr_el1, x11
        eret
stack:
        cmp     x10, #0x25
        b.ne    report
        mrs     x10, far_el1
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
        msr     elr_el1, x11
        eret
report:
