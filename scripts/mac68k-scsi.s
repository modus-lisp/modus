| mac68k-scsi.s -- a read-only SCSI disk driver that boots a 68000 Mac (Plus,
| SE, Classic) from a disk holding a modus boot volume (scripts/mac68k-hd.py).
|
| THE BOOT.  The ROM reads the disk's block 0 -- the driver descriptor ('ER'),
| which names this driver's first block and length -- loads the driver into
| the system heap and enters its FIRST BYTE with the disk's SCSI ID in D5.
| INSTALL then makes it a driver (_DrvrInstall at unit 32 + ID, marked open)
| and adds a drive for it to the drive queue; the ROM then reads that drive's
| boot blocks and runs them, as from a floppy (scripts/mac68k-bootblocks.s).
|
| THE VOLUME.  The drive is the disk from block BASE on (patched in by
| mac68k-hd.py), VOLBLOCKS blocks long, read-only: PRIME turns the Device
| Manager's byte position (dCtlPosition) into SCSI READ(6) commands of up to
| 64 blocks, through the ROM's SCSI Manager (_SCSIDispatch: Get, Select, Cmd,
| RBlind, Complete).  A position must be a multiple of 512, as every reader
| of a modus boot volume makes it.
|
| 68000 code, position independent (the ROM puts it anywhere), its data in
| its own block (the system heap is RAM).
|
| Assemble: m68k-linux-gnu-as -m68000 -o s.o mac68k-scsi.s

        .text
drvr:   bra.w   install
| ---- the DRVR header _DrvrInstall is given ----
header: .word   0x4F00                  | needLock, status, control, write, read
        .word   0, 0, 0                 | delay, event mask, menu
        .word   dopen - header
        .word   dprime - header
        .word   dctl - header
        .word   dstat - header
        .word   dclose - header
        .byte   10
        .ascii  ".modusSCSI"
        .align  2

| ---- INSTALL: entered by the ROM, D5 = the SCSI ID ----
install:
        movem.l %d1-%d7/%a0-%a6, -(%sp)
        move.w  %d5, %d1
        and.w   #7, %d1
        lea     scsiid(%pc), %a0
        move.w  %d1, (%a0)
        add.w   #32, %d1                | the unit number
        lea     unit(%pc), %a0
        move.w  %d1, (%a0)
        move.w  %d1, %d0
        not.w   %d0                     | refNum = -(unit + 1)
        lea     refnum(%pc), %a0
        move.w  %d0, (%a0)
        lea     header(%pc), %a0
        .word   0xA03D                  | _DrvrInstall: D0 = refNum, A0 = driver
        tst.w   %d0
        bne     ifail
        | the DCE: UTableBase[unit] is its handle
        move.l  0x11C, %a0
        move.w  unit(%pc), %d0
        lsl.w   #2, %d0
        move.l  (%a0,%d0.w), %a0
        move.l  (%a0), %a0
        lea     header(%pc), %a1
        move.l  %a1, (%a0)              | dCtlDriver: this driver, by pointer
        move.w  #0x4F20, 4(%a0)         | dCtlFlags: the header's, opened, not RAM-based
        | the drive: write-protected, non-ejectable, drive 8 + ID
        lea     dqe(%pc), %a0
        move.b  #0x80, -4(%a0)          | locked
        move.b  #8, -3(%a0)             | a disk in place that does not eject
        clr.l   (%a0)                   | qLink
        move.w  #1, 4(%a0)              | qType 1: dQDrvSz2 is valid
        move.w  scsiid(%pc), %d0
        add.w   #8, %d0
        move.w  %d0, 6(%a0)             | dQDrive
        move.w  refnum(%pc), 8(%a0)     | dQRefNum
        clr.w   10(%a0)                 | dQFSID
        move.l  volblocks(%pc), %d0
        move.w  %d0, 12(%a0)            | dQDrvSz (low word)
        swap    %d0
        move.w  %d0, 14(%a0)            | dQDrvSz2 (high word)
        lea     0x308, %a1              | DrvQHdr
        .word   0xA96F                  | _Enqueue
        moveq   #0, %d0
        bra     iout
ifail:  moveq   #-1, %d0
iout:   movem.l (%sp)+, %d1-%d7/%a0-%a6
        rts

dopen:
dclose: moveq   #0, %d0
        rts

| ---- CONTROL / STATUS: KillIO is fine, nothing else is supported ----
dctl:   moveq   #-17, %d0               | controlErr
        cmp.w   #1, 26(%a0)             | csCode KillIO
        bne     cdone
        moveq   #0, %d0
cdone:  move.l  0x8FC, -(%sp)           | JIODone
        rts
dstat:  moveq   #-18, %d0               | statusErr
        move.l  0x8FC, -(%sp)
        rts

| ---- PRIME: A0 = the parameter block, A1 = the DCE ----
dprime: movem.l %d1-%d7/%a0-%a6, -(%sp)
        move.l  %a0, %a2                | pb
        move.l  %a1, %a3                | DCE
        lea     dbg(%pc), %a0           | evidence for a probe: calls, position, count
        addq.l  #1, (%a0)
        move.l  16(%a3), 4(%a0)
        move.l  36(%a2), 8(%a0)
        move.w  6(%a2), %d0             | ioTrap: aRdCmd (2) or aWrCmd (3)
        and.w   #0xFF, %d0
        cmp.w   #2, %d0
        bne     pwrite
        move.l  16(%a3), %d3            | dCtlPosition, bytes
        lsr.l   #8, %d3
        lsr.l   #1, %d3                 | / 512
        add.l   base(%pc), %d3          | the LBA
        move.l  36(%a2), %d6            | ioReqCount
        move.l  %d6, %d7
        add.l   #511, %d7
        lsr.l   #8, %d7
        lsr.l   #1, %d7                 | blocks
        move.l  32(%a2), %a4            | ioBuffer
ploop:  tst.l   %d7
        beq     pdone
        move.l  %d7, %d2
        cmp.l   #64, %d2
        bls     1f
        moveq   #64, %d2
1:      bsr     sread                   | D3 LBA, D2 blocks, A4 buffer -> D0
        tst.w   %d0
        bne     perr
        add.l   %d2, %d3
        sub.l   %d2, %d7
        move.l  %d2, %d1
        lsl.l   #8, %d1
        lsl.l   #1, %d1
        add.l   %d1, %a4
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

| ---- SREAD: READ(6) of D2 blocks at LBA D3 into A4.  D0 = 0 or an error.
| Three tries.  _SCSIDispatch is a Toolbox trap: Pascal's convention lets it
| clobber D0-D2/A0-A1 (measured: D1, where the errors were ORed, came back
| 5696), so the error lives in D5 and D2 is reloaded from its saved copy ----
sread:  movem.l %d2-%d7, -(%sp)
        moveq   #2, %d4
sretry: move.l  (%sp), %d2              | the block count, again
        lea     cdb(%pc), %a0
        move.b  #0x08, (%a0)            | READ(6)
        move.l  %d3, %d0
        swap    %d0
        and.b   #0x1F, %d0
        move.b  %d0, 1(%a0)
        move.l  %d3, %d0
        lsr.w   #8, %d0
        move.b  %d0, 2(%a0)
        move.b  %d3, 3(%a0)
        move.b  %d2, 4(%a0)             | blocks (64 at most)
        clr.b   5(%a0)
        lea     tib(%pc), %a0
        move.w  #1, (%a0)               | scInc
        move.l  %a4, 2(%a0)
        move.l  %d2, %d0
        lsl.l   #8, %d0
        lsl.l   #1, %d0
        move.l  %d0, 6(%a0)             | bytes
        move.w  #7, 10(%a0)             | scStop
        clr.l   12(%a0)
        clr.l   16(%a0)
        | SCSIGet
        clr.w   -(%sp)
        move.w  #1, -(%sp)
        .word   0xA815
        move.w  (%sp)+, %d0
        lea     dbg(%pc), %a1
        move.w  %d0, 16(%a1)            | Get
        bne     sfail
        | SCSISelect(id)
        clr.w   -(%sp)
        move.w  scsiid(%pc), -(%sp)
        move.w  #2, -(%sp)
        .word   0xA815
        move.w  (%sp)+, %d0
        lea     dbg(%pc), %a1
        move.w  %d0, 18(%a1)            | Select
        bne     sfail
        | SCSICmd(cdb, 6)
        clr.w   -(%sp)
        pea     cdb(%pc)
        move.w  #6, -(%sp)
        move.w  #3, -(%sp)
        .word   0xA815
        move.w  (%sp)+, %d5
        lea     dbg(%pc), %a1
        move.w  %d5, 20(%a1)            | Cmd
        | SCSIRBlind(tib)
        clr.w   -(%sp)
        pea     tib(%pc)
        move.w  #8, -(%sp)
        .word   0xA815
        move.w  (%sp)+, %d0
        lea     dbg(%pc), %a1
        move.w  %d0, 22(%a1)            | RBlind
        or.w    %d0, %d5
        | SCSIComplete(stat, msg, 300 ticks)
        clr.w   -(%sp)
        pea     stat(%pc)
        pea     msg(%pc)
        move.l  #300, -(%sp)
        move.w  #4, -(%sp)
        .word   0xA815
        move.w  (%sp)+, %d0
        lea     dbg(%pc), %a0
        move.w  %d5, 12(%a0)            | Cmd | RBlind
        move.w  %d0, 14(%a0)            | Complete
        or.w    %d5, %d0
        bne     sfail
        move.w  stat(%pc), %d0
        and.w   #0x1E, %d0              | the status byte: GOOD is 0
        bne     sfail
        moveq   #0, %d0
        movem.l (%sp)+, %d2-%d7
        rts
sfail:  dbra    %d4, sretry
        moveq   #-36, %d0
        movem.l (%sp)+, %d2-%d7
        rts

        .align  2
| ---- data (patched: BASE, VOLBLOCKS) ----
base:   .long   64                      | the volume's first block
volblocks: .long 0                      | its length, in blocks
scsiid: .word   0
unit:   .word   0
refnum: .word   0
stat:   .word   0
msg:    .word   0
cdb:    .space  6
| evidence: Prime calls, last position, last count, Cmd|RBlind, Complete, Get, Select
        .ascii  "DBG!"
dbg:    .space  24
tib:    .space  20
        .long   0                       | the drive's flags (dqe - 4)
dqe:    .space  16
