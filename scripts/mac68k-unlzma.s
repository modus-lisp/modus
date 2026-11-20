| mac68k-unlzma.s -- the bare Mac's boot-disk loader: unpack a compressed
| modus image to its load address and start it.  It replaced the Lisp loader
| (mvm/mac68k-unlzma.lisp + mvm/unlzma-core.lisp), which took ~15 minutes on a
| 68040; the decoder is scripts/mac68k-lzma.inc.
|
| scripts/mac68k-floppy.lisp assembles this and puts it first in the boot blob,
| which the boot blocks (or stage 2, scripts/mac68k-disk2.s) read to LOADER
| and enter at its first byte.  The payload is at LOADER + 0x4000:
|   +0  the unpacked size (u32); +4 0; +6 the stream's offset from the payload
|   +8  the jumper (from the packer): _HWPriv 1 (flush the instruction cache;
|       a 68040's data cache is pushed too) -- the image was written as DATA --
|       and jump to OUTADDR
|   +n  the LZMA1 stream (lc=2 lp=1 pb=2, no header)
| OUTADDR (below) is where the image goes and starts: #x100000 unless the
| packer patches it (a saved core loads at its map's :LOAD).  With a DRIVER
| (DRVLEN not 0; scripts/mac68k-lzdisk.s, the floppy whose cold code stays
| packed) it is copied from the payload to DRVADDR and its INSTALL called,
| after the unpack, before the jumper.  Too little RAM (MemTop under MEMNEED):
| _SysError 25, out of memory.
| The probabilities and the stack go at STATEADDR, past the blob's end (the
| packer checks); the image is below LOADER.  All of it is RAM the image
| claims and overwrites later.
|
| Assemble: m68k-linux-gnu-as -m68020 -I scripts (it .includes the decoder).

        .equ    PAY, 0x4000

        .text
start:  move.w  #0x2000, %sr            | supervisor, interrupts on (as the image's stub)
        move.l  memneed(%pc), %d0       | the RAM the map needs
        cmp.l   0x108, %d0              | MemTop
        bhi     nomem
        move.l  stateaddr(%pc), %a3
        lea     0xF00(%a3), %sp         | the stack, under the probabilities
        lea     0x1000(%a3), %a3
        lea     start(%pc), %a0
        add.l   #PAY, %a0
        move.l  %a0, -(%sp)             | the payload
        move.l  outaddr(%pc), %a5
        move.l  %a5, %a4
        add.l   (%a0), %a4              | + the size
        moveq   #0, %d0
        move.w  6(%a0), %d0
        lea     (0, %a0, %d0.l), %a2    | the stream
        clr.l   -16(%a3)                | DICTEND: no dictionary
        bsr     lzdec
        move.l  (%sp)+, %a0
        move.l  drvlen(%pc), %d0        | a driver (scripts/mac68k-lzdisk.s) to install?
        beq     2f
        move.l  %a0, -(%sp)
        move.l  %a0, %a1
        add.l   drvsrc(%pc), %a1
        move.l  drvaddr(%pc), %a2
        lsr.l   #2, %d0
        subq.l  #1, %d0
1:      move.l  (%a1)+, (%a2)+          | (a word count: the driver is ~3 KB)
        dbra    %d0, 1b
        moveq   #1, %d0                 | _HWPriv 1: FlushInstructionCache (and,
        .word   0xA198                  |   on a 68040, push the data cache)
        move.l  drvaddr(%pc), %a1
        jsr     (%a1)                   | its INSTALL
        move.l  (%sp)+, %a0
2:      jmp     8(%a0)                  | the jumper: does not return
nomem:  moveq   #25, %d0                | dsMemFullErr: the Mac has too little RAM
        .word   0xA9C9                  | _SysError
        bra     nomem

| Patched by scripts/mac68k-floppy.lisp:
outaddr:   .long 0x00100000             | the image's address (--load)
stateaddr: .long 0x01A00000             | the probabilities at +#x1000, the stack under them
memneed:   .long 0                      | MemTop must be at least this
drvsrc:    .long 0                      | the driver: its offset in the payload,
drvlen:    .long 0                      |   its length (a multiple of 4; 0: none),
drvaddr:   .long 0                      |   where it goes

        .include "mac68k-lzma.inc"
