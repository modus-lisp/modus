| mac68k-disk2.s -- stage 2 of a two-disk Mac boot (scripts/mac68k-floppy.py).
|
| The boot blocks read disk 1's share of the boot blob (loader, payload head,
| this code, the first part of the LZMA stream) and jump HERE instead of to
| the loader.  This finds disk 2 -- a disk whose block 0 starts "MODUS-D2"
| and this set's id -- in any drive: each drive queue element's block 0 is
| read and compared, so a second drive (or a hard disk) holding it is found
| at once.  If none has it, the boot drive is ejected ONCE (.Sony's Control
| ejectCode), "INSERT MODUS DISK 2" is drawn in the middle of the screen, and
| the drives are polled every half second.  Disk 2's data (from byte 512) is
| read to DEST, right after disk 1's, so the loader sees one stream; then
| every register is put back and the loader is entered.
|
| 68000 code (a Mac Classic has no 68020): no long branches, no scaled
| indexing, no 32-bit multiply; word and long accesses at even addresses.
| The screen: ScrnBase ($824), ScreenRow ($106), depth from the main
| GDevice's PixMap when Color QuickDraw is present (ROM85 $28E bit 14
| clear), else 1.  Depths 1/8/16/32 drawn, others skipped.
|
| scripts/mac68k-floppy.py patches LOADER/DEST/LEN/SETID and the message
| (MSGN glyphs of 8 row bytes at MSGBUF).
|
| Assemble: m68k-linux-gnu-as -m68000 -o s.o mac68k-disk2.s

        .text
stage2: movem.l %d0-%d7/%a0-%a6, -(%sp)
scan:   move.l  0x30A, %a3              | DrvQHdr.qHead
1:      move.l  %a3, %d0
        beq     notfound
        bsr     try
        tst.w   %d0
        beq     found
        move.l  (%a3), %a3              | qLink
        bra     1b
notfound:
        lea     ejected(%pc), %a0
        tst.b   (%a0)
        bne     wait
        st      (%a0)
        bsr     eject
        bsr     message
wait:   move.l  0x16A, %d1              | Ticks: half a second
        add.l   #30, %d1
2:      cmp.l   0x16A, %d1
        bhi     2b
        bra     scan
found:  movem.l (%sp)+, %d0-%d7/%a0-%a6
        move.l  loader(%pc), %a0
        jmp     (%a0)

| try: A3 = a drive queue element.  D0 = 0 when it holds disk 2 and its data
| is now at DEST, else -1.
try:    bsr     pbinit
        move.l  #512, 36(%a0)           | ioReqCount: block 0
        clr.l   46(%a0)                 | ioPosOffset
        .word   0xA002                  | _Read
        tst.w   %d0
        bne     no
        move.l  dest(%pc), %a2
        cmp.l   #0x4D4F4455, (%a2)      | "MODU"
        bne     no
        cmp.l   #0x532D4432, 4(%a2)     | "S-D2"
        bne     no
        move.l  setid(%pc), %d1
        cmp.l   8(%a2), %d1
        bne     no
        bsr     pbinit
        move.l  len(%pc), 36(%a0)       | ioReqCount: the data
        move.l  #512, 46(%a0)           | ioPosOffset: after the label block
        .word   0xA002                  | _Read
        tst.w   %d0
        bne     no
        moveq   #0, %d0
        rts
no:     moveq   #-1, %d0
        rts

| pbinit: A0 = the parameter block, zeroed, aimed at A3's drive, buffer DEST.
pbinit: lea     pb(%pc), %a0
        moveq   #19, %d0                | 80 bytes
3:      clr.l   (%a0)+
        dbra    %d0, 3b
        lea     pb(%pc), %a0
        move.w  6(%a3), 22(%a0)         | ioVRefNum = dQDrive
        move.w  8(%a3), 24(%a0)         | ioRefNum  = dQRefNum
        move.l  dest(%pc), 32(%a0)      | ioBuffer
        move.w  #1, 44(%a0)             | ioPosMode = fsFromStart
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
        move.w  #7, 26(%a0)             | csCode = ejectCode
        .word   0xA004                  | _Control
5:      rts

| message: MSGN glyphs from MSGBUF, centred on row 160, black on white.
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
        and.w   #0xFFFC, %d1            | even, and long-aligned
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
        move.b  %d3, (%a1)              | 1 bit: the row byte is the pixels
        bra     rownext
8:      move.l  %a1, %a0
        moveq   #7, %d2
pix:    add.b   %d3, %d3                | C = this pixel
        bcs     ink
        cmp.w   #8, %d7
        bne     11f
        clr.b   (%a0)+                  | 8 bit: white is index 0
        bra     pnext
11:     cmp.w   #16, %d7
        bne     12f
        move.w  #0x7FFF, (%a0)+
        bra     pnext
12:     move.l  #0x00FFFFFF, (%a0)+
        bra     pnext
ink:    cmp.w   #8, %d7
        bne     13f
        st      (%a0)+                  | black is index 255
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
        adda.w  %d7, %a2                | next glyph: 8 pixels on
        dbra    %d5, glyph
9:      rts

        .align  2
loader: .long   0
dest:   .long   0
len:    .long   0
setid:  .long   0
msgn:   .word   0
ejected: .byte  0
        .align  2
pb:     .space  80
msgbuf: .space  256
