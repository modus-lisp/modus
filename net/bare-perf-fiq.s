// Vector entry 6 (Current EL, SPx, FIQ) is overwritten with STUB, which jumps to HANDLER.
// Per-core control block (64 bytes at CTL + 64*core), raw u64 fields:
//   +0  ring base            +8  ring end
//   +16 write pointer        +24 interval (timer ticks)
//   +32 depth (frames)       +40 samples taken
//   +48 samples dropped      +56 stop flag (nonzero: disarm on next tick)
// Frame bounds (shared, after the 4 core blocks, at CTL+256): +0 lo  +8 hi
// Record: [header = core | depth<<8 | n<<16] [pc] [x30] [ret0..ret(depth-1)]  (zeros past the chain)
stub:
  stp x0, x1, [sp, #-16]!
  ldr x0, stub_lit
  br x0
  .balign 8
stub_lit: .quad 0                       // handler address, patched

handler:
  stp x2, x3, [sp, #-16]!
  stp x4, x5, [sp, #-16]!
  stp x6, x7, [sp, #-16]!
  ldr x3, ctl_lit                     // CTL
  mrs x0, mpidr_el1
  and x0, x0, #3
  add x3, x3, x0, lsl #6         // this core's block
  ldr x1, [x3, #56]              // stop?
  cbnz x1, stop
  ldr x1, [x3, #24]
  msr cnthp_tval_el2, x1         // rearm first: deasserts the FIQ
  ldr x2, [x3, #16]              // write pointer
  ldr x4, [x3, #32]              // depth
  add x5, x2, x4, lsl #3
  add x5, x5, #24                // end of this record
  ldr x6, [x3, #8]               // ring end
  cmp x5, x6
  b.hi full
  orr x6, x0, x4, lsl #8         // header: core | depth<<8 (n filled below)
  mrs x1, elr_el2
  str x1, [x2, #8]               // pc
  str x30, [x2, #16]             // interrupted lr
  add x7, x2, #24                // frame slots
  mov x1, x29                    // interrupted fp
  ldr x0, ctl_lit
  ldr x5, [x0, #256]             // lo
  ldr x0, [x0, #264]             // hi
  mov x4, #0                     // n
walk:
  ldr x2, [x3, #32]
  cmp x4, x2
  b.hs done
  tst x1, #7                     // frame records are 8-aligned here (sp ends in 8)
  b.ne done
  cmp x1, x5
  b.lo done
  cmp x1, x0
  b.hs done
  ldr x2, [x1, #8]               // return address
  str x2, [x7], #8
  add x4, x4, #1
  ldr x2, [x1]                   // caller's fp
  cmp x2, x1
  b.ls done                      // chain must go up the stack
  mov x1, x2
  b walk
done:
  ldr x2, [x3, #32]
pad:                             // zero the unused slots
  cmp x4, x2
  b.hs padded
  str xzr, [x7], #8
  add x4, x4, #1
  b pad
padded:
  ldr x2, [x3, #16]
  str x6, [x2]                   // header (n not tracked separately: zeros mark the end)
  ldr x4, [x3, #32]
  add x2, x2, x4, lsl #3
  add x2, x2, #24
  str x2, [x3, #16]
  ldr x1, [x3, #40]
  add x1, x1, #1
  str x1, [x3, #40]
  b out
full:
  ldr x1, [x3, #48]
  add x1, x1, #1
  str x1, [x3, #48]
  b out
stop:
  msr cnthp_ctl_el2, xzr         // timer off: no more FIQs from it
out:
  ldp x6, x7, [sp], #16
  ldp x4, x5, [sp], #16
  ldp x2, x3, [sp], #16
  ldp x0, x1, [sp], #16
  eret
  .balign 8
ctl_lit: .quad 0                       // CTL, patched

// arm (called through %jit-call on the core to sample): x3 = this core's block
arm:
  ldr x3, ctl_lit
  mrs x0, mpidr_el1
  and x0, x0, #3
  add x3, x3, x0, lsl #6
  ldr x0, [x3, #24]
  msr cnthp_tval_el2, x0
  mrs x0, mpidr_el1              // route this core's CNTHP to FIQ: 0x40000040 + 4*core, bit 6
  and x0, x0, #3
  movz x1, #0x4000, lsl #16
  add x1, x1, #0x40
  add x1, x1, x0, lsl #2
  ldr w2, [x1]
  orr w2, w2, #0x40
  str w2, [x1]
  dsb sy
  mrs x0, hcr_el2                // FMO: a physical FIQ is taken at EL2.  Without it the FIQ
  orr x0, x0, #8                 // targets EL1, and an exception never goes DOWN a level --
  msr hcr_el2, x0                // at EL2 it stays masked whatever PSTATE.F says
  isb
  mov x0, #1
  msr cnthp_ctl_el2, x0          // enable, unmasked
  msr daifclr, #1                // let FIQs in
  isb
  ret
// disarm: FIQs masked again, timer off
disarm:
  msr daifset, #1
  msr cnthp_ctl_el2, xzr
  mrs x0, mpidr_el1              // stop routing it
  and x0, x0, #3
  movz x1, #0x4000, lsl #16
  add x1, x1, #0x40
  add x1, x1, x0, lsl #2
  ldr w2, [x1]
  and w2, w2, #0xffffffbf
  str w2, [x1]
  dsb sy
  isb
  ret
// vbar: store VBAR_EL2 (tagged) at [x3]
vbar:
  mrs x0, vbar_el2
  lsl x0, x0, #1
  str x0, [x3]
  mrs x0, CurrentEL
  lsl x0, x0, #1
  str x0, [x3, #8]
  mrs x0, cntfrq_el0
  lsl x0, x0, #1
  str x0, [x3, #16]
  mrs x0, mpidr_el1
  and x0, x0, #3
  lsl x0, x0, #1
  str x0, [x3, #24]
  mrs x0, daif
  lsl x0, x0, #1
  str x0, [x3, #32]
  mrs x0, hcr_el2
  lsl x0, x0, #1
  str x0, [x3, #40]
  ret
// barrier: after a page-table write
barrier:
  dsb sy
  tlbi alle2
  dsb sy
  isb
  ret
