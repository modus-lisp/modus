// The QEMU-virt (EL1) twins of stubs.s, for net/bare-threads-virt.lisp.  QEMU virt
// runs the CL image at EL1 (the Pi's runs at EL2), and a secondary core is started by
// PSCI CPU_ON over HVC, entering at a PHYSICAL address with its MMU off.  Three pieces,
// at the same offsets as the Pi's: the core entry stub (S), the parameter block
// (S+0x800, written by Lisp), the release routine (S+0x1000) and core 0's register
// snapshot (S+0x1100).  The first two words of the release and the snapshot routines
// are a MOVZ/MOVK of X0 that Lisp patches with S / the parameter block.
        .text
entry:
        adr     x0, params              // a PA: the MMU is off
        ldr     x1, [x0, #0]
        msr     mair_el1, x1
        ldr     x1, [x0, #8]
        msr     tcr_el1, x1
        ldr     x1, [x0, #16]
        msr     ttbr0_el1, x1
        tlbi    vmalle1
        dsb     sy
        isb
        ldr     x1, [x0, #32]
        msr     vbar_el1, x1
        mov     x1, #0x300000           // CPACR_EL1.FPEN: FP/SIMD on
        msr     cpacr_el1, x1
        isb
        ldr     x1, [x0, #24]
        msr     sctlr_el1, x1           // MMU on; the PC stays a PA, which L1[1] maps 1:1
        isb
        ldr     x1, [x0, #48]
        mov     sp, x1
        ldr     x1, [x0, #56]
        msr     tpidr_el1, x1
        ldr     x1, [x0, #64]
        msr     tpidr_el0, x1
        ldr     x28, [x0, #40]
        ldr     x24, [x0, #72]
        ldr     x25, [x0, #80]
        movz    x18, #0x1000, lsl #16
        movz    x26, #0x0001
        movk    x26, #0xdead, lsl #16
        mov     x1, #1
        str     x1, [x0, #96]
        ldr     x9, [x0, #88]
        mov     x29, #0
        mov     x30, #0
        blr     x9
        adr     x0, params
        mov     x1, #2
        str     x1, [x0, #96]
1:      wfe
        b       1b
        .balign 2048
params:
        .space  0x800
release:                                // S+0x1000: clean the stub, then CPU_ON
        movz    x0, #0
        movk    x0, #0, lsl #16
        add     x2, x0, #0x900
1:      dc      civac, x0
        add     x0, x0, #64
        cmp     x0, x2
        b.lo    1b
        dsb     sy
        sub     x0, x0, #0x900
        ldr     x1, [x0, #0x870]        // params+0x70: target core (MPIDR)
        ldr     x2, [x0, #0x878]        // params+0x78: entry PA
        movz    x0, #0x0003
        movk    x0, #0xc400, lsl #16    // PSCI CPU_ON (SMC64)
        mov     x3, #0
        hvc     #0
        lsl     x0, x0, #1              // the PSCI status, as a fixnum
        ret
        .balign 256
snapshot:                               // S+0x1100
        movz    x0, #0
        movk    x0, #0, lsl #16
        mrs     x1, mair_el1
        str     x1, [x0, #0]
        mrs     x1, tcr_el1
        str     x1, [x0, #8]
        mrs     x1, ttbr0_el1
        str     x1, [x0, #16]
        mrs     x1, sctlr_el1
        str     x1, [x0, #24]
        mrs     x1, vbar_el1
        str     x1, [x0, #32]
        str     x28, [x0, #40]
        mrs     x1, currentel
        str     x1, [x0, #104]
        ret
