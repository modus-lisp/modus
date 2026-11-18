| mac68k-chain.s -- the Mac application that starts modus from Mac OS
| (scripts/mac68k-chain.py builds it: this is its CODE 1).
|
| The application's DATA FORK is a modus boot disk (scripts/mac68k-core.py's,
| for a map that can run on top of Mac OS: boot-mac68k's :CLASSIC-OS).  This
| code does what that disk's boot blocks do -- read the image to its address
| and jump to it -- with the File Manager instead of a drive: it opens its own
| data fork, finds the boot blocks' record ("modusCHN": DEST ENTRY LEN OFF
| SPOFF SPLEN REFSLOT MEMTOP), checks the memory picture, reads LEN bytes from
| OFF to DEST, stores the open file's reference number at REFSLOT (the
| image's code loader then _Reads its streams from the file) and jumps to
| ENTRY.  Mac OS stays underneath, as the ROM does under a disk boot: low
| memory, the system heap, its drivers and interrupt handlers.  There is no
| way back but a restart.
|
| Anything wrong is said on the screen, and a click returns to the Finder.
|
| Assemble: m68k-linux-gnu-as -m68000 -o c.o mac68k-chain.s

        .macro  pstr text               | a Pascal string
        .byte   .Lpe\@ - .Lps\@
.Lps\@: .ascii  "\text"
.Lpe\@:
        .endm

        .text
| the frame: the parameter block, the file's name, the boot blocks
        .equ    PB, -80
        .equ    NAME, -144
        .equ    BB, -1168
start:  link    %a6, #-1168
        pea     -4(%a5)
        .word   0xA86E                  | _InitGraf
        .word   0xA8FE                  | _InitFonts
        .word   0xA912                  | _InitWindows
        .word   0xA850                  | _InitCursor
| this application's file: PBGetFCBInfo on its resource fork (CurApRefNum
| $900) gives the volume, the folder and the name, wherever it was launched from
        bsr     clrpb
        lea     NAME(%a6), %a1
        move.l  %a1, 18(%a0)            | ioNamePtr
        move.w  0x900, 24(%a0)          | ioRefNum = CurApRefNum
        moveq   #8, %d0                 | GetFCBInfo
        .word   0xA260                  | _HFSDispatch
        lea     m_open(%pc), %a4
        tst.w   %d0
        bne     fail
        move.w  52(%a0), %d6            | ioFCBVRefNum
        move.l  58(%a0), %d5            | ioFCBParID
| ... and its data fork, read-only
        bsr     clrpb
        lea     NAME(%a6), %a1
        move.l  %a1, 18(%a0)
        move.w  %d6, 22(%a0)            | ioVRefNum
        move.b  #1, 27(%a0)             | ioPermssn = fsRdPerm
        move.l  %d5, 48(%a0)            | ioDirID
        .word   0xA200                  | _HOpen
        tst.w   %d0
        bne     fail
        move.w  24(%a0), %d7            | the open file
| the boot blocks
        lea     BB(%a6), %a1
        move.l  %a1, %d1
        move.l  #1024, %d2
        moveq   #0, %d3
        bsr     rd
        lea     m_read(%pc), %a4
        bne     fail
| the record
        lea     BB(%a6), %a2
        move.w  #(1024 - 40) / 2 - 1, %d1
1:      cmp.l   #0x6D6F6475, (%a2)      | "modu"
        bne     2f
        cmp.l   #0x7343484E, 4(%a2)     | "sCHN"
        beq     3f
2:      addq.l  #2, %a2
        dbra    %d1, 1b
        lea     m_notmodus(%pc), %a4
        bra     fail
3:      addq.l  #8, %a2                 | A2: DEST ENTRY LEN OFF SPOFF SPLEN REFSLOT MEMTOP
        lea     m_notchain(%pc), %a4
        tst.l   24(%a2)
        beq     fail
        lea     m_ram(%pc), %a4
| the machine's RAM: Gestalt 'ram ' (System 6.0.4 and later).  Not MemTop
| ($108): System 7 swaps that per application partition (measured on a 32 MB
| Quadra 950 under 7.5.3, with MCL running: $001B13E0).
        move.l  #0x72616D20, %d0        | 'ram '
        .word   0xA1AD                  | _Gestalt: A0 = the response, D0 = OSErr
        tst.w   %d0
        bne     fail
        move.l  %a0, %d0
        cmp.l   28(%a2), %d0
        blo     fail
| the system heap must end below the image (System 6.0.8 with no INITs ends
| near #x24270; the image is at #x40000)
        lea     m_sysheap(%pc), %a4
        move.l  0x2A6, %a3              | SysZone
        move.l  (%a3), %d0              | its bkLim
        cmp.l   (%a2), %d0
        bhi     fail
| and this code and this stack must be outside what the read overwrites
        lea     m_where(%pc), %a4
        move.l  (%a2), %d1              | DEST
        move.l  %d1, %d2
        add.l   8(%a2), %d2             | DEST + LEN
        lea     start(%pc), %a3
        move.l  %a3, %d0
        bsr     outside
        bne     fail
        lea     fail(%pc), %a3          | the end of this code
        move.l  %a3, %d0
        bsr     outside
        bne     fail
        move.l  %sp, %d0
        bsr     outside
        bne     fail
        move.l  %a6, %d0
        bsr     outside
        bne     fail
| the splash, as the boot blocks draw it, on a compact Mac's screen (64
| bytes a row): read to DEST, which the image read overwrites next
        move.l  20(%a2), %d2            | SPLEN
        beq     4f
        move.w  0x106, %d0              | ScreenRow
        and.w   #0x3FFF, %d0
        cmp.w   #64, %d0
        bne     4f
        move.l  (%a2), %d1
        move.l  16(%a2), %d3            | SPOFF
        bsr     rd
        bne     4f
        .word   0xA852                  | _HideCursor
        bsr     splash
| the image
4:      move.l  (%a2), %d1
        move.l  8(%a2), %d2
        move.l  12(%a2), %d3
        bsr     rd
        lea     m_read(%pc), %a4
        bne     fail
| from here on the image's: its streams come from this file, and nothing may
| allocate in the zone it now covers
        move.l  24(%a2), %a3
        move.w  %d7, %d0
        ext.l   %d0
        move.l  %d0, (%a3)              | REFSLOT = the open file
        move.l  0x2A6, 0x118            | TheZone = SysZone
        .word   0xA852                  | _HideCursor (again is harmless)
        move.l  4(%a2), %a0
        cmpi.b  #2, 0x12F.w             | CPUFlag: a 68020 or later has caches,
        bcs.s   9f                      |   and code was just written as data:
        move.l  %a0, -(%sp)             |   _HWPriv 1, FlushInstructionCache
        moveq   #1, %d0                 |   (on a 68040 it pushes the data cache
        .word   0xA198                  |   too).  A 68000 ROM has no _HWPriv.
        move.l  (%sp)+, %a0
9:
        jmp     (%a0)                   | ENTRY

| the splash at DEST (scripts/mac68k-disk.py SPLASH, mac68k-bootblocks.s): the
| screen in the desktop's grey, then the box -- X0 Y0 W H, the bytes XORed two
| rows up, PackBits'd -- unpacked at DEST+#x2000 and drawn over it
splash: movem.l %d3-%d6/%a3-%a4, -(%sp)
        move.l  0x824, %a1              | ScrnBase
        move.l  #0xAAAAAAAA, %d0
        move.w  #341, %d1
1:      moveq   #15, %d2
2:      move.l  %d0, (%a1)+
        dbra    %d2, 2b
        not.l   %d0
        dbra    %d1, 1b
        move.l  (%a2), %a0
        movem.w (%a0)+, %d3-%d6         | X0 Y0 W H
        lea     0x2000-8(%a0), %a1
        move.l  %a1, %a3
        move.w  %d5, %d0
        mulu.w  %d6, %d0
        lea     (%a1,%d0.l), %a4
3:      cmp.l   %a4, %a1
        bhs     6f
        moveq   #0, %d0
        move.b  (%a0)+, %d0             | the header byte
        bmi     5f
4:      move.b  (%a0)+, (%a1)+          | 0..127: n+1 literal bytes
        dbra    %d0, 4b
        bra     3b
5:      neg.b   %d0                     | -1..-127: the next byte 1-n times
        bmi     3b                      | -128: nothing
        move.b  (%a0)+, %d1
51:     move.b  %d1, (%a1)+
        dbra    %d0, 51b
        bra     3b
6:      move.l  %a3, %a0                | undo the XOR, in place
        lea     (%a0,%d5.w), %a1
        add.w   %d5, %a1
7:      cmp.l   %a4, %a1
        bhs     8f
        move.b  (%a0)+, %d0
        eor.b   %d0, (%a1)+
        bra     7b
8:      move.l  %a3, %a0                | the box onto the screen
        move.l  0x824, %a1
        mulu.w  #64, %d4
        add.l   %d4, %a1
        add.w   %d3, %a1
        subq.w  #1, %d6
        subq.w  #1, %d5
81:     move.l  %a1, %a4
        move.w  %d5, %d0
82:     move.b  (%a0)+, (%a4)+
        dbra    %d0, 82b
        lea     64(%a1), %a1
        dbra    %d6, 81b
        movem.l (%sp)+, %d3-%d6/%a3-%a4
        rts

| D0 outside [D1, D2)?  Z set if so
outside:
        cmp.l   %d1, %d0
        blo     1f
        cmp.l   %d2, %d0
        bhs     1f
        moveq   #1, %d0                 | inside: NE
        rts
1:      moveq   #0, %d0                 | outside: EQ
        rts

| A0 = the parameter block, cleared
clrpb:  lea     PB(%a6), %a0
        moveq   #19, %d0
1:      clr.l   (%a0)+
        dbra    %d0, 1b
        lea     PB(%a6), %a0
        rts

| read D2 bytes at D3 of the open file (D7) to D1; Z set when it worked
rd:     bsr     clrpb
        move.w  %d7, 24(%a0)            | ioRefNum
        move.l  %d1, 32(%a0)            | ioBuffer
        move.l  %d2, 36(%a0)            | ioReqCount
        move.w  #1, 44(%a0)             | ioPosMode = fsFromStart
        move.l  %d3, 46(%a0)            | ioPosOffset
        .word   0xA002                  | _Read
        tst.w   %d0
        rts

| A4 = what went wrong (a Pascal string): say it, wait for a click, quit
fail:   pea     band(%pc)
        .word   0xA8A3                  | _EraseRect
        move.w  #12, -(%sp)
        move.w  #36, -(%sp)
        .word   0xA893                  | _MoveTo
        move.l  %a4, -(%sp)
        .word   0xA884                  | _DrawString
        move.w  #12, -(%sp)
        move.w  #56, -(%sp)
        .word   0xA893
        pea     m_click(%pc)
        .word   0xA884
1:      subq.l  #2, %sp
        .word   0xA974                  | _Button
        tst.b   (%sp)+
        beq     1b
        .word   0xA9F4                  | _ExitToShell

band:   .word   20, 0, 64, 512          | top left bottom right
m_open:     pstr "Modus: cannot open my own data fork."
m_read:     pstr "Modus: cannot read the image in my data."
m_notmodus: pstr "Modus: my data fork is not a modus boot disk."
m_notchain: pstr "Modus: this image cannot run on top of Mac OS."
m_ram:      pstr "Modus: this Mac has less memory than the image is built for."
m_sysheap:  pstr "Modus: the System heap is too big.  Try with fewer extensions."
m_where:    pstr "Modus: launched where the image has to go."
m_click:    pstr "Click to return to the Finder."
        .align  2
