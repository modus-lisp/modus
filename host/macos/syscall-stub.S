// syscall-stub.S — the SVC-shaped entry a Darwin modus image calls for every
// syscall (docs/macos-hosting.md, M0).
//
// The image emits, where a Linux image has SVC #0:
//     str x30, [sp, #-16]! ; x16 = slot ; ldr x16, [x16] ; blr x16 ; ldr x30, [sp], #16
// with the LINUX aarch64 syscall number in x8 and arguments in x0..x5.  From
// the image's side this must behave exactly like SVC: every register except
// x0 (the result, -errno on failure) comes back unchanged.  So this saves the
// general and SIMD registers the C translator may clobber under AAPCS64,
// passes the number as the 7th argument, and restores them.  x16 was already
// spent by the caller; x18 is never touched (Apple reserves it); x19..x29 are
// callee-saved by the C code itself.
//
// x19..x28 are STORED as well (never reloaded — the C side preserves them),
// and the frame's address is the 8th argument: clone has to start a thread
// with the caller's whole register file, which only exists here.

    .text
    .globl _modus_syscall_stub
    .p2align 2
_modus_syscall_stub:
    sub  sp, sp, #608
    stp  x1,  x2,  [sp, #0]
    stp  x3,  x4,  [sp, #16]
    stp  x5,  x6,  [sp, #32]
    stp  x7,  x8,  [sp, #48]
    stp  x9,  x10, [sp, #64]
    stp  x11, x12, [sp, #80]
    stp  x13, x14, [sp, #96]
    stp  x15, x17, [sp, #112]
    stp  x29, x30, [sp, #128]
    stp  q0,  q1,  [sp, #144]
    stp  q2,  q3,  [sp, #176]
    stp  q4,  q5,  [sp, #208]
    stp  q6,  q7,  [sp, #240]
    stp  q16, q17, [sp, #272]
    stp  q18, q19, [sp, #304]
    stp  q20, q21, [sp, #336]
    stp  q22, q23, [sp, #368]
    stp  q24, q25, [sp, #400]
    stp  q26, q27, [sp, #432]
    stp  q28, q29, [sp, #464]
    stp  q30, q31, [sp, #496]
    add  x16, sp, #528             // STP reaches only 504: address the
    stp  x19, x20, [x16, #0]       // tail through x16 (spent by the caller)
    stp  x21, x22, [x16, #16]
    stp  x23, x24, [x16, #32]
    stp  x25, x26, [x16, #48]
    stp  x27, x28, [x16, #64]
    add  x29, sp, #128             // a frame the C side can unwind through
    mov  x6, x8                    // 7th argument: the Linux syscall number
    mov  x7, sp                    // 8th: this frame (see struct stub_frame)
    bl   _modus_syscall
    ldp  q30, q31, [sp, #496]
    ldp  q28, q29, [sp, #464]
    ldp  q26, q27, [sp, #432]
    ldp  q24, q25, [sp, #400]
    ldp  q22, q23, [sp, #368]
    ldp  q20, q21, [sp, #336]
    ldp  q18, q19, [sp, #304]
    ldp  q16, q17, [sp, #272]
    ldp  q6,  q7,  [sp, #240]
    ldp  q4,  q5,  [sp, #208]
    ldp  q2,  q3,  [sp, #176]
    ldp  q0,  q1,  [sp, #144]
    ldp  x29, x30, [sp, #128]
    ldp  x15, x17, [sp, #112]
    ldp  x13, x14, [sp, #96]
    ldp  x11, x12, [sp, #80]
    ldp  x9,  x10, [sp, #64]
    ldp  x7,  x8,  [sp, #48]
    ldp  x5,  x6,  [sp, #32]
    ldp  x3,  x4,  [sp, #16]
    ldp  x1,  x2,  [sp, #0]
    add  sp, sp, #608
    ret

// Enter the image: switch to its stack and jump to its entry point.
//   x0 = initial SP (pointing at argc, Linux layout), x1 = entry.
    .globl _modus_enter
    .p2align 2
_modus_enter:
    mov  sp, x0
    mov  x29, #0
    mov  x30, #0
    br   x1

// Start a cloned thread inside the image (docs/macos-hosting.md, threads).
//   x0 = struct resume_ctx { x[31]; sp; pc; }: the parent's register file at
//   its clone call, with x0 = 0.  x16/x17 carry SP and PC and x18 is left
//   alone (Apple's).
    .globl _modus_resume
    .p2align 2
_modus_resume:
    ldr  x16, [x0, #248]
    mov  sp, x16
    ldr  x17, [x0, #256]
    ldp  x1,  x2,  [x0, #8]
    ldp  x3,  x4,  [x0, #24]
    ldp  x5,  x6,  [x0, #40]
    ldp  x7,  x8,  [x0, #56]
    ldp  x9,  x10, [x0, #72]
    ldp  x11, x12, [x0, #88]
    ldp  x13, x14, [x0, #104]
    ldr  x15,      [x0, #120]
    ldp  x19, x20, [x0, #152]
    ldp  x21, x22, [x0, #168]
    ldp  x23, x24, [x0, #184]
    ldp  x25, x26, [x0, #200]
    ldp  x27, x28, [x0, #216]
    ldp  x29, x30, [x0, #232]
    ldr  x0,       [x0, #0]
    br   x17
