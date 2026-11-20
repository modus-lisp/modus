| mac68k-unlzma.s -- the bare Mac's boot-disk loader: unpack a compressed
| modus image to its load address and start it.  It replaced the Lisp loader
| (mvm/mac68k-unlzma.lisp + mvm/unlzma-core.lisp), which took ~15 minutes on a
| 68040; the decoder is scripts/mac68k-lzma.inc.
|
| scripts/mac68k-floppy.lisp assembles this and puts it first in the boot blob,
| which the boot blocks (or stage 2, scripts/mac68k-disk2.s) read to LOADER
| and enter at its first byte.  The payload is at LOADER + 0x4000:
|   +0  the unpacked size (u32); +4 0; +6 the stream's offset from the payload
|   +8  the jumper (from the packer): push the 68040's data cache, invalidate
|       both caches -- the image was written as DATA -- and jump to OUT
|   +n  the LZMA1 stream (lc=2 lp=1 pb=2, no header)
| The probabilities and the stack go at STATE, just past the blob's limit
| (the packer checks the blob ends below it); the image at OUT is below
| LOADER.  All of it is RAM the image claims and overwrites later.
|
| Assemble: m68k-linux-gnu-as -m68020 -I scripts (it .includes the decoder).

        .equ    PAY, 0x4000
        .equ    OUT, 0x00100000
        .equ    STATE, 0x01A00000
        .equ    STACK, STATE + 0x0F00
        .equ    PROBS, STATE + 0x1000

        .text
start:  move.w  #0x2000, %sr            | supervisor, interrupts on (as the image's stub)
        move.l  #STACK, %sp
        lea     start(%pc), %a0
        add.l   #PAY, %a0
        move.l  %a0, -(%sp)             | the payload
        move.l  #OUT, %a5
        move.l  %a5, %a4
        add.l   (%a0), %a4              | + the size
        moveq   #0, %d0
        move.w  6(%a0), %d0
        lea     (0, %a0, %d0.l), %a2    | the stream
        move.l  #PROBS, %a3
        bsr     lzdec
        move.l  (%sp)+, %a0
        jmp     8(%a0)                  | the jumper: does not return

        .include "mac68k-lzma.inc"
