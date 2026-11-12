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
        move.l  dest(%pc), 32(%a0)      | ioBuffer
        move.l  len(%pc), 36(%a0)       | ioReqCount
        move.w  #1, 44(%a0)             | ioPosMode = fsFromStart
        move.l  off(%pc), 46(%a0)       | ioPosOffset
        .word   0xA002                  | _Read
        tst.w   %d0
        bne     fail
        move.l  entry(%pc), %a0         | the image, or a two-disk boot's stage 2
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
dest:   .long   0x0D000000
entry:  .long   0x0D000000
len:    .long   0
off:    .long   1024
pb:     .space  80
