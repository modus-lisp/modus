| mac68k-bootblocks.s -- boot blocks that load a bare modus image (boot/boot-mac68k.lisp).
|
| The Mac ROM reads the boot disk's first 1024 bytes and, finding 'LK' and an
| executable version, jumps to bbEntry.  This code finds the boot drive's
| driver in the drive queue, reads LEN bytes from disk offset OFF to DEST with
| one synchronous _Read, and jumps to ENTRY (DEST, or a two-disk boot's
| stage 2, scripts/mac68k-disk2.s).  scripts/mac68k-disk.py patches
| DEST/ENTRY/LEN/OFF.  68000 code: a Mac Classic runs it too.  A failed read paints the top of the screen black and stops.
|
| Assemble: m68k-linux-gnu-as -m68000 -o b.o mac68k-bootblocks.s
|           m68k-linux-gnu-objcopy -O binary b.o b.bin

        .text
| ---- the boot block header (Inside Macintosh: Files, "Boot Blocks") ----
bbbase: .ascii  "LK"                    | bbID
        bra.w   start                   | bbEntry
        .word   0x4418                  | bbVersion: executable boot blocks
        .word   0                       | bbPageFlags
        .byte   6
        .ascii  "System         "       | bbSysName      (Str15)
        .byte   6
        .ascii  "Finder         "       | bbShellName
        .byte   7
        .ascii  "MacsBug        "       | bbDbg1Name
        .byte   12
        .ascii  "Disassembler   "       | bbDbg2Name
        .byte   13
        .ascii  "StartUpScreen  "       | bbScreenName
        .byte   6
        .ascii  "Finder         "       | bbHelloName
        .byte   9
        .ascii  "Clipboard      "       | bbScrapName
        .word   10                      | bbCntFCBs
        .word   20                      | bbCntEvts
        .long   0x00004300              | bb128KSHeap
        .long   0x00008000              | bb256KSHeap
        .long   0x00020000              | bbSysHeapSize
| ---- the loader, at 0x8A ----
| FIRST, OUT OF THE WAY: when the ROM runs boot blocks its stack -- and these
| blocks -- sit in the middle of RAM (measured: #x1FFBFC on a 4 MB Classic,
| #x20FB8C on a 4 MB LC), inside the range a big image is read to.  So copy
| these 1024 bytes to 64 KB under the top of RAM or the screen, whichever is
| lower (MemTop $108, ScrnBase $824; an LC's screen is not in RAM), put the
| stack under them, and go on from the copy.
start:  move.l  0x108, %d0              | MemTop
        move.l  0x824, %d1              | ScrnBase
        cmp.l   %d1, %d0
        bls     1f
        move.l  %d1, %d0
1:      sub.l   #0x10000, %d0
        and.l   #-16, %d0
        move.l  %d0, %a1                | the copy's address
        lea     bbbase(%pc), %a0
        move.w  #255, %d1               | 1024 bytes
2:      move.l  (%a0)+, (%a1)+
        dbra    %d1, 2b
        move.l  %d0, %a7                | the stack, under the copy
        move.l  %d0, %a0
        add.l   #(moved - bbbase), %a0
        cmpi.b  #2, 0x12F.w             | CPUFlag: a 68020 or later has caches,
        bcs.s   9f                      |   and code was just written as data:
        move.l  %a0, -(%sp)             |   _HWPriv 1, FlushInstructionCache
        moveq   #1, %d0                 |   (on a 68040 it pushes the data cache
        .word   0xA198                  |   too).  A 68000 ROM has no _HWPriv.
        move.l  (%sp)+, %a0
9:
        jmp     (%a0)
moved:  move.w  0x210, %d1              | BootDrive: the drive number
        move.l  0x30A, %a1              | DrvQHdr.qHead
dq:     move.l  %a1, %d0
        beq     fail
        cmp.w   6(%a1), %d1             | dQDrive
        beq     found
        move.l  (%a1), %a1              | qLink
        bra     dq
found:  lea     pb(%pc), %a0
        move.w  8(%a1), 24(%a0)         | ioRefNum = dQRefNum
        move.w  %d1, 22(%a0)            | ioVRefNum = the drive
| THE SPLASH (boot/mac68k-splash.pbm), on a compact Mac only: a 512 x 342 x 1
| screen, 64 bytes a row.  The screen is filled with the desktop's 50% grey,
| then the picture's middle -- the box where it differs from that grey -- is
| drawn over it.  On the disk (scripts/mac68k-disk.py, SPLASH): X0 (bytes), Y0,
| W (bytes), H as words, then the box's W*H bytes, each XORed with the byte
| two rows above it in the box, PackBits'd (Apple TN1023).  SPLEN bytes from
| SPOFF are read to DEST, where the image goes next, and unpacked at
| DEST+#x2000.  A failed read only costs the picture.
        move.l  splen(%pc), %d0
        beq     3f
        move.w  0x106, %d2              | ScreenRow
        and.w   #0x3FFF, %d2
        cmp.w   #64, %d2
        bne     3f
        move.l  dest(%pc), 32(%a0)      | ioBuffer
        move.l  %d0, 36(%a0)            | ioReqCount
        move.w  #1, 44(%a0)             | ioPosMode = fsFromStart
        move.l  spoff(%pc), 46(%a0)     | ioPosOffset
        .word   0xA002                  | _Read
        tst.w   %d0
        bne     3f
        move.l  0x824, %a3              | the grey: #xAA.. rows and #x55.. rows
        move.l  #0xAAAAAAAA, %d0
        move.w  #341, %d1
70:     moveq   #15, %d2
71:     move.l  %d0, (%a3)+
        dbra    %d2, 71b
        not.l   %d0
        dbra    %d1, 70b
        move.l  dest(%pc), %a2
        movem.w (%a2)+, %d3-%d6         | X0 Y0 W H
        lea     0x2000-8(%a2), %a3      | unpack to DEST+#x2000 ...
        move.l  %a3, %a5
        move.w  %d5, %d0
        mulu.w  %d6, %d0
        lea     (%a3,%d0.l), %a4        | ... W*H bytes
4:      cmp.l   %a4, %a3
        bhs     6f
        moveq   #0, %d0
        move.b  (%a2)+, %d0             | the header byte
        bmi     5f
41:     move.b  (%a2)+, (%a3)+          | 0..127: n+1 literal bytes
        dbra    %d0, 41b
        bra     4b
5:      neg.b   %d0                     | -1..-127: the next byte 1-n times
        bmi     4b                      | -128: nothing
        move.b  (%a2)+, %d2
51:     move.b  %d2, (%a3)+
        dbra    %d0, 51b
        bra     4b
6:      move.l  %a5, %a3                | undo the XOR, in place
        lea     (%a3,%d5.w), %a2
        add.w   %d5, %a2                | two rows on
62:     cmp.l   %a4, %a2
        bhs     7f
        move.b  (%a3)+, %d0
        eor.b   %d0, (%a2)+
        bra     62b
7:      move.l  %a5, %a3                | the box onto the screen
        move.l  0x824, %a2
        mulu.w  #64, %d4
        add.l   %d4, %a2
        add.w   %d3, %a2
        subq.w  #1, %d6
        subq.w  #1, %d5
72:     move.l  %a2, %a1
        move.w  %d5, %d0
73:     move.b  (%a3)+, (%a1)+
        dbra    %d0, 73b
        lea     64(%a2), %a2
        dbra    %d6, 72b
        lea     pb(%pc), %a0
        move.w  0x210, 22(%a0)          | ioVRefNum again: the trap may have changed the block
3:      move.l  dest(%pc), 32(%a0)      | ioBuffer
        move.l  len(%pc), 36(%a0)       | ioReqCount
        move.w  #1, 44(%a0)             | ioPosMode = fsFromStart
        move.l  off(%pc), 46(%a0)       | ioPosOffset
        .word   0xA002                  | _Read
        tst.w   %d0
        bne     fail
        move.l  entry(%pc), %a0         | the image, or a two-disk boot's stage 2
        cmpi.b  #2, 0x12F.w             | CPUFlag: a 68020 or later has caches,
        bcs.s   9f                      |   and code was just written as data:
        move.l  %a0, -(%sp)             |   _HWPriv 1, FlushInstructionCache
        moveq   #1, %d0                 |   (on a 68040 it pushes the data cache
        .word   0xA198                  |   too).  A 68000 ROM has no _HWPriv.
        move.l  (%sp)+, %a0
9:
        jmp     (%a0)
fail:   move.l  0x824, %a0              | ScrnBase: paint 32 rows black
        move.w  0x106, %d1
        and.l   #0x3fff, %d1
        mulu.w  #32, %d1
        lsr.l   #2, %d1
f1:     move.l  #-1, (%a0)+
        subq.l  #1, %d1
        bne     f1
hang:   bra     hang
        .align  2
| THE RECORD a loader launched from Mac OS reads (scripts/mac68k-chain.py):
| everything from DEST to MEMTOP, after this marker.  REFSLOT is where the
| image wants the open file's reference number (its static block's
| #x10006050), 0 when the image cannot run on top of Mac OS; MEMTOP the
| RAM its map needs.  RELOFF / RELLEN: the disk bytes of its relocation table
| (scripts/mac68k-reloc-disk.py), 0 when it has none; SPAN: the bytes of map
| from DEST it needs, which a relocating loader allocates.
        .ascii  "modusCHN"
dest:   .long   0x0D000000
entry:  .long   0x0D000000
len:    .long   0
off:    .long   1024
spoff:  .long   0                       | the splash: its disk byte and length,
splen:  .long   0                       | 0 when the disk has none
refslot: .long  0
memtop: .long   0
reloff: .long   0                       | the relocation table: its disk byte
rellen: .long   0                       |   and length, 0 = none
span:   .long   0                       | the map's bytes from DEST
pb:     .space  80
