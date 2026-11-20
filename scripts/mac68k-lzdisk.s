| mac68k-lzdisk.s -- a read-only Mac disk driver whose drive is a modus core
| disk (scripts/mac68k-core.py's c.dsk) kept compressed on a floppy.
|
| WHY.  A floppy Mac with 8 MB cannot hold all of a modus image's native code:
| only the functions a session uses are resident, every other one is COLD --
| read off the boot disk at its first call (translate-68k M68K-EMIT-COLD-ENTER)
| through the BootDrive's driver.  Uncompressed, the cold code alone is four
| floppies.  So the floppy (scripts/mac68k-floppy.lisp) carries the core disk
| as the loader's stream (what the boot reads: the image, its static heap and
| low block, unpacked to their address at boot) and THE TAIL (cold code, cold
| names) as independent LZMA blocks; this driver's drive is the core disk, and
| the image reads it as it would read any boot disk.  Nothing in the image
| knows.
|
| THE BLOCKS.  The tail from disk byte VBASE, in BLOCK (64 KB) pieces, each a
| raw LZMA1 stream (lc 2 lp 1 pb 2) against a PRESET DICTIONARY: the image's
| resident code, [LOAD, DICTEND) in RAM, which nothing writes once it runs
| (scripts/mac68k-lzma.inc's DICTEND).  So a block packs nearly as well as the
| one stream would, and still unpacks alone.  TABLE[i] is block i's first byte
| on the floppy, TABLE[NBLK] the end.  One block is kept unpacked (CACHE); a
| read elsewhere reads the block's whole sectors from the floppy (CBUF) and
| unpacks it.  The floppy is the drive the Mac booted from: BootDrive when
| INSTALL runs.
|
| THE DISK.  The blocks live on the floppy whose sector at LABELOFF starts
| "MODUS-D2" and this set's id: the second floppy, a FAT12 disk on which they
| are the file MODUS/COLD.BIN (its first sector the label), with room for the
| user's files -- or, a one-disk set, the boot floppy itself.  FIND (the second
| entry, drvr+4; the loader calls it in the blob, before the unpack, so a user
| with one drive swaps disks while the Mac unpacks) reads that sector in every
| drive; if none has it, it ejects the boot disk once, draws INSERT MODUS DISK
| 2 and polls every half second (scripts/mac68k-disk2.s's way).  The drive it
| finds is the one the blocks are read from (RDRIVE, RREF).
|
| INSTALL (the first byte; called by the loader once the image is unpacked):
| _DrvrInstall at a free unit (39 -- SCSI ID 7 is the Mac -- or the first free
| from 40), a locked, non-ejectable drive numbered past every drive in the
| queue, and BootDrive ($210) set to it.
|
| Placed at a fixed address by the loader (the map's driver area); its buffers
| follow it (BUFS).  68020: the decoder's MULU.L and scaled indexing.
|
| Assemble: m68k-linux-gnu-as -m68020 -I scripts (it .includes the decoder).

        .equ    BLOCK, 0x10000          | 64 KB a block: a position's high word
        .equ    PROBS_OFF, 32           | in BUFS: 20 bytes under the probabilities
        .equ    CBUF_OFF, PROBS_OFF + 16384
        .equ    CACHE_OFF, CBUF_OFF + BLOCK + 2048
        .equ    BUFS_SIZE, CACHE_OFF + BLOCK

        .text
drvr:   bra.w   install
        bra.w   find                    | drvr+4: A1 = a 512-byte buffer
| ---- the DRVR header _DrvrInstall is given ----
header: .word   0x4F00                  | needLock, status, control, write, read
        .word   0, 0, 0                 | delay, event mask, menu
        .word   dopen - header
        .word   dprime - header
        .word   dctl - header
        .word   dstat - header
        .word   dclose - header
        .byte   9
        .ascii  ".modusLZD"
        .align  2

| ---- INSTALL ----
install:
        movem.l %d1-%d7/%a0-%a6, -(%sp)
        lea     rdrive(%pc), %a0
        tst.w   2(%a0)
        beq     ifail                   | FIND found no disk (it does not return then)
        move.l  0x30A, %a1              | DrvQHdr.qHead
        moveq   #0, %d2                 | the highest drive number in the queue
1:      move.l  %a1, %d0
        beq     2f
        cmp.w   6(%a1), %d2
        bcc     3f
        move.w  6(%a1), %d2
3:      move.l  (%a1), %a1
        bra     1b
2:
        addq.w  #1, %d2
        lea     mydrive(%pc), %a0
        move.w  %d2, (%a0)
        | a free unit: 39, else 40 .. UnitNtryCnt - 1
        move.l  0x11C, %a1              | UTableBase
        moveq   #39, %d1
        tst.l   39*4(%a1)
        beq     6f
        moveq   #40, %d1
5:      cmp.w   0x1D2, %d1              | UnitNtryCnt
        bcc     ifail
        tst.l   (%a1,%d1.w*4)
        beq     6f
        addq.w  #1, %d1
        bra     5b
6:      lea     unit(%pc), %a0
        move.w  %d1, (%a0)
        move.w  %d1, %d0
        not.w   %d0                     | refNum = -(unit + 1)
        move.w  %d0, 2(%a0)
        lea     header(%pc), %a0
        .word   0xA03D                  | _DrvrInstall: D0 = refNum, A0 = driver
        tst.w   %d0
        bne     ifail
        | the DCE: UTableBase[unit] is its handle
        move.l  0x11C, %a0
        move.w  unit(%pc), %d0
        move.l  (%a0,%d0.w*4), %a0
        move.l  (%a0), %a0
        lea     header(%pc), %a1
        move.l  %a1, (%a0)              | dCtlDriver: this driver, by pointer
        move.w  #0x4F20, 4(%a0)         | dCtlFlags: the header's, opened, not RAM-based
        | the drive: write-protected, non-ejectable
        lea     dqe(%pc), %a0
        move.b  #0x80, -4(%a0)          | locked
        move.b  #8, -3(%a0)             | a disk in place that does not eject
        clr.l   (%a0)                   | qLink
        move.w  #1, 4(%a0)              | qType 1: dQDrvSz2 is valid
        move.w  mydrive(%pc), 6(%a0)    | dQDrive
        move.w  refnum(%pc), 8(%a0)     | dQRefNum
        clr.w   10(%a0)                 | dQFSID
        move.l  nblk(%pc), %d0          | its size in blocks: VBASE + the tail
        swap    %d0
        add.l   vbase(%pc), %d0
        lsr.l   #8, %d0
        lsr.l   #1, %d0
        move.w  %d0, 12(%a0)            | dQDrvSz (low word)
        swap    %d0
        move.w  %d0, 14(%a0)            | dQDrvSz2 (high word)
        lea     0x308, %a1              | DrvQHdr
        .word   0xA96F                  | _Enqueue
        move.w  mydrive(%pc), 0x210     | BootDrive: this drive from now on
        lea     cached(%pc), %a0
        move.l  #-1, (%a0)              | nothing unpacked yet
        moveq   #0, %d0
        bra     iout
ifail:  moveq   #-1, %d0
iout:   movem.l (%sp)+, %d1-%d7/%a0-%a6
        rts

dopen:
dclose: moveq   #0, %d0
        rts
dctl:   moveq   #-17, %d0               | controlErr, but KillIO
        cmp.w   #1, 26(%a0)
        bne     1f
        moveq   #0, %d0
1:      move.l  0x8FC, -(%sp)           | JIODone
        rts
dstat:  moveq   #-18, %d0               | statusErr
        move.l  0x8FC, -(%sp)
        rts

| ---- PRIME: A0 = the parameter block, A1 = the DCE ----
dprime: movem.l %d1-%d7/%a0-%a6, -(%sp)
        move.l  %a0, %a2                | pb
        move.l  %a1, %a3                | DCE
        move.w  6(%a2), %d0             | ioTrap: aRdCmd (2) or aWrCmd (3)
        and.w   #0xFF, %d0
        cmp.w   #2, %d0
        bne     pwrite
        move.l  16(%a3), %d5            | dCtlPosition: the byte to start at
        move.l  36(%a2), %d6            | ioReqCount
        move.l  %d6, %d7                | bytes still to read
        move.l  32(%a2), %a4            | ioBuffer
ploop:  tst.l   %d7
        beq     pdone
        move.l  %d5, %d4
        sub.l   vbase(%pc), %d4         | the position in the tail
        bcs     perr                    | before it: never read once booted
        move.l  %d4, %d1
        clr.w   %d1
        swap    %d1                     | D1 = the block
        cmp.l   nblk(%pc), %d1
        bcc     perr
        and.l   #BLOCK-1, %d4           | D4 = the offset in it
        movem.l %d4-%d7/%a2-%a4, -(%sp)
        bsr     fetch                   | D1 the block -> D0 = 0 or an error
        movem.l (%sp)+, %d4-%d7/%a2-%a4
        tst.w   %d0
        bne     perr
        move.l  #BLOCK, %d1
        sub.l   %d4, %d1                | what the block has from the offset on
        cmp.l   %d7, %d1
        bls     1f
        move.l  %d7, %d1                | or what is wanted
1:      lea     bufs(%pc), %a0
        add.l   #CACHE_OFF, %a0
        add.l   %d4, %a0
        add.l   %d1, %d5
        sub.l   %d1, %d7
        subq.l  #1, %d1
2:      move.b  (%a0)+, (%a4)+
        dbra    %d1, 2b                 | a word count: under 64 KB a block
        bra     ploop
pdone:  move.l  %d6, 40(%a2)            | ioActCount
        add.l   %d6, 16(%a3)            | dCtlPosition
        moveq   #0, %d0
        bra     pout
pwrite: moveq   #-44, %d0               | wPrErr: read-only
        bra     pout
perr:   moveq   #-36, %d0               | ioErr
pout:   movem.l (%sp)+, %d1-%d7/%a0-%a6
        move.l  0x8FC, -(%sp)           | JIODone, D0 = the result
        rts

| ---- FETCH: block D1 into CACHE.  D0 = 0 or an error.  Uses everything. ----
fetch:  lea     cached(%pc), %a0
        cmp.l   (%a0), %d1
        beq     fok
        move.l  #-1, (%a0)              | (a failed unpack leaves nothing cached)
        move.l  %d1, -(%sp)
        lea     table(%pc), %a0
        move.l  (%a0,%d1.l*4), %d2      | the block's first byte on the floppy
        move.l  4(%a0,%d1.l*4), %d3     | and its end
        move.l  %d2, %d4
        and.l   #-512, %d4              | from its first sector
        add.l   #511, %d3
        and.l   #-512, %d3
        sub.l   %d4, %d3                | to the end of its last: whole sectors
        lea     pb(%pc), %a0
        moveq   #19, %d0                | the parameter block, 80 bytes, zeroed
1:      clr.l   (%a0)+
        dbra    %d0, 1b
        lea     pb(%pc), %a0
        move.w  rdrive(%pc), 22(%a0)    | ioVRefNum = the floppy's drive
        move.w  rref(%pc), 24(%a0)      | ioRefNum = its driver
        lea     bufs(%pc), %a1
        add.l   #CBUF_OFF, %a1
        move.l  %a1, 32(%a0)            | ioBuffer
        move.l  %d3, 36(%a0)            | ioReqCount
        move.w  #1, 44(%a0)             | ioPosMode = fsFromStart
        move.l  %d4, 46(%a0)            | ioPosOffset
        move.l  %d2, -(%sp)
        .word   0xA002                  | _Read
        move.l  (%sp)+, %d2
        move.l  (%sp)+, %d1
        tst.w   %d0
        bne     fout
        move.l  %d1, -(%sp)
        lea     bufs(%pc), %a2          | A2 = the stream: CBUF + the first byte's
        add.l   #CBUF_OFF, %a2          |   place in its sector
        and.l   #511, %d2
        add.l   %d2, %a2
        lea     bufs(%pc), %a3
        add.l   #PROBS_OFF, %a3
        move.l  dictend(%pc), -16(%a3)  | DICTEND: the resident code's end
        lea     bufs(%pc), %a5
        add.l   #CACHE_OFF, %a5
        move.l  %a5, %a4
        add.l   #BLOCK, %a4
        bsr     lzdec
        move.l  (%sp)+, %d1
        lea     cached(%pc), %a0
        move.l  %d1, (%a0)
fok:    moveq   #0, %d0
fout:   rts

| ---- FIND: the disk whose sector at LABELOFF is this set's label ----
find:   movem.l %d0-%d7/%a0-%a6, -(%sp)
        lea     fbuf(%pc), %a0
        move.l  %a1, (%a0)
fscan:  move.l  0x30A, %a3              | DrvQHdr.qHead
1:      move.l  %a3, %d0
        beq     fnone
        bsr     ftry
        tst.w   %d0
        beq     ffound
        move.l  (%a3), %a3              | qLink
        bra     1b
fnone:  lea     ejected(%pc), %a0
        tst.b   (%a0)
        bne     fwait
        st      (%a0)
        bsr     eject
        bsr     message
fwait:  move.l  0x16A, %d1              | Ticks: half a second
        add.l   #30, %d1
2:      cmp.l   0x16A, %d1
        bhi     2b
        bra     fscan
ffound: lea     rdrive(%pc), %a0
        move.w  6(%a3), (%a0)           | dQDrive
        move.w  8(%a3), 2(%a0)          | dQRefNum
        movem.l (%sp)+, %d0-%d7/%a0-%a6
        rts

| ftry: A3 = a drive queue element.  D0 = 0 when its sector at LABELOFF is the label.
ftry:   bsr     pbinit
        move.w  6(%a3), 22(%a0)         | ioVRefNum = dQDrive
        move.w  8(%a3), 24(%a0)         | ioRefNum  = dQRefNum
        move.l  fbuf(%pc), 32(%a0)      | ioBuffer
        move.l  #512, 36(%a0)           | ioReqCount
        move.l  labeloff(%pc), 46(%a0)  | ioPosOffset
        .word   0xA002                  | _Read
        tst.w   %d0
        bne     fno
        move.l  fbuf(%pc), %a2
        cmp.l   #0x4D4F4455, (%a2)      | "MODU"
        bne     fno
        cmp.l   #0x532D4432, 4(%a2)     | "S-D2"
        bne     fno
        move.l  setid(%pc), %d1
        cmp.l   8(%a2), %d1
        bne     fno
        moveq   #0, %d0
        rts
fno:    moveq   #-1, %d0
        rts

| pbinit: A0 = the parameter block, zeroed, ioPosMode fsFromStart.
pbinit: lea     pb(%pc), %a0
        moveq   #19, %d0                | 80 bytes
3:      clr.l   (%a0)+
        dbra    %d0, 3b
        lea     pb(%pc), %a0
        move.w  #1, 44(%a0)
        rts

| eject: the boot drive's disk (BootDrive $210), if the drive queue has it.
eject:  move.l  0x30A, %a3
4:      move.l  %a3, %d0
        beq     5f
        move.w  0x210, %d1
        cmp.w   6(%a3), %d1
        beq     6f
        move.l  (%a3), %a3
        bra     4b
6:      bsr     pbinit
        move.w  6(%a3), 22(%a0)
        move.w  8(%a3), 24(%a0)
        move.w  #7, 26(%a0)             | csCode = ejectCode
        .word   0xA004                  | _Control
5:      rts

| message: MSGN glyphs from MSGBUF, centred on row 160, black on white
| (scripts/mac68k-disk2.s's, which draws depths 1, 8, 16 and 32).
message:
        move.l  0x824, %a2              | ScrnBase
        moveq   #0, %d6
        move.w  0x106, %d6
        and.w   #0x3FFF, %d6            | ScreenRow
        moveq   #1, %d7                 | depth
        move.w  0x28E, %d0              | ROM85
        btst    #14, %d0
        bne     7f                      | no Color QuickDraw: 1 bit
        move.l  0x8A4, %d0              | MainDevice
        beq     7f
        move.l  %d0, %a0
        move.l  (%a0), %a0              | GDevice
        move.l  22(%a0), %a0            | gdPMap
        move.l  (%a0), %a0              | PixMap
        move.w  32(%a0), %d7            | pixelSize
7:      cmp.w   #1, %d7
        beq     10f
        cmp.w   #8, %d7
        beq     10f
        cmp.w   #16, %d7
        beq     10f
        cmp.w   #32, %d7
        bne     9f
10:     move.w  msgn(%pc), %d5
        move.w  %d5, %d0
        mulu.w  %d7, %d0                | the message's width in bytes
        move.l  %d6, %d1
        sub.l   %d0, %d1
        bmi     9f
        lsr.l   #1, %d1
        and.w   #0xFFFC, %d1
        move.l  #160, %d0
        mulu.w  %d6, %d0
        add.l   %d1, %d0
        add.l   %d0, %a2                | A2 = the first glyph's top row
        lea     msgbuf(%pc), %a4
        subq.w  #1, %d5
glyph:  move.l  %a2, %a1
        moveq   #7, %d4
row:    move.b  (%a4)+, %d3
        cmp.w   #1, %d7
        bne     8f
        move.b  %d3, (%a1)
        bra     rownext
8:      move.l  %a1, %a0
        moveq   #7, %d2
pix:    add.b   %d3, %d3
        bcs     ink
        cmp.w   #8, %d7
        bne     11f
        clr.b   (%a0)+
        bra     pnext
11:     cmp.w   #16, %d7
        bne     12f
        move.w  #0x7FFF, (%a0)+
        bra     pnext
12:     move.l  #0x00FFFFFF, (%a0)+
        bra     pnext
ink:    cmp.w   #8, %d7
        bne     13f
        st      (%a0)+
        bra     pnext
13:     cmp.w   #16, %d7
        bne     14f
        clr.w   (%a0)+
        bra     pnext
14:     clr.l   (%a0)+
pnext:  dbra    %d2, pix
rownext:
        add.l   %d6, %a1
        dbra    %d4, row
        adda.w  %d7, %a2
        dbra    %d5, glyph
9:      rts

        .include "mac68k-lzma.inc"

| ---- data: the packer (scripts/mac68k-floppy.lisp) fills VBASE NBLK DICTEND
| LABELOFF SETID TABLE and the message (MSGN glyphs of 8 row bytes at MSGBUF) ----
        .align  2
vbase:  .long   0                       | the core disk's byte at block 0
nblk:   .long   0                       | blocks
dictend: .long  0                       | the preset dictionary's end ([LOAD, DICTEND))
cached: .long   -1                      | the block in CACHE, or -1
labeloff: .long 0                       | the label sector's byte on its floppy
setid:  .long   0                       | this set's id, in the label
fbuf:   .long   0                       | FIND's buffer
rdrive: .word   0                       | the floppy's drive number
rref:   .word   0                       |   and its driver's refNum
mydrive: .word  0
unit:   .word   0
refnum: .word   0
        .word   0
        .space  4                       | the drive queue element's flags
dqe:    .space  16
pb:     .space  80
msgn:   .word   0
ejected: .byte  0
        .align  2
msgbuf: .space  256
table:  .space  4 * 257                 | NBLK + 1 floppy bytes (256 blocks: 16 MB)
        .align  4
bufs:                                   | PROBS, CBUF, CACHE: BUFS_SIZE bytes, not in the image
