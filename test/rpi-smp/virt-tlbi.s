// S+0x1500 on QEMU virt (EL1): the TLB flush after the stack guard pages are mapped.
        dsb     ishst
        tlbi    vmalle1is
        dsb     ish
        isb
        ret
