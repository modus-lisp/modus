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
| memory, the system heap, its drivers and interrupt handlers.  A fixed-
| address image has no way back but a restart; a RELOCATED one is an
| ordinary application (THE HOST, below): a window, menus, the event loop,
| Quit back to the Finder.
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
        .equ    BASE, -1172                 | a relocating load: where the map goes
        .equ    TBL, -1176                  |   and its relocation table
        .equ    DELTA, -1180                |   and BASE - the DEST it was built at
        .equ    HEXB, -1192                 | SAY's text: a Pascal string, 10 bytes
        .equ    HHALF, -1196                | the heap from the partition: a semispace,
        .equ    HKMAP, -1200                |   the kind map past the two,
        .equ    HEXTRA, -1204               |   and how far that moved it (x7/8 sites)
        .equ    GUARD, 0x10000              | boot-mac68k :GC-GUARD
        .equ    LOWBLK, 0x20000             | the low block (the layout's :ALLOC-START)
start:  link    %a6, #-1204
        clr.l   HHALF(%a6)
        clr.l   HKMAP(%a6)
        clr.l   HEXTRA(%a6)
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
                                        |   RELOFF RELLEN SPAN KMAP
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
| A RELOCATING LOAD (RELOFF set: scripts/mac68k-reloc-disk.py): the map goes
| in this application's own partition, wherever the Process Manager put it --
| in the free space between the application heap's end and the stack, as the
| fixed-address image sat (SPAN bytes, 512 KB-aligned, so every relocation is
| a 16-bit add: mac68k-reloc.py).  NOT a NewPtr block after MaxApplZone: that
| put the image's stack inside the heap, and Mac OS's stack checks (the VBL
| sniffer, StackSpace before File Manager I/O) took it for a stack that ran
| into the heap -- measured on a Quadra 950 under System 7.5.3: the process
| ended at the first tick, then "The disk Macintosh cannot be found".  The
| table goes in the heap first; ApplLimit then stops the heap short of the
| map.  DEST, ENTRY and REFSLOT in the record copy move with it; everything
| below then works on the new addresses.
        clr.l   BASE(%a6)
        tst.l   32(%a2)                 | RELOFF
        beq     5f
        move.l  36(%a2), %d0            | the table: RELLEN bytes in the heap
        .word   0xA11E                  | _NewPtr: A0, D0 = OSErr
        lea     m_alloc(%pc), %a4
        tst.w   %d0
        bne     fail
        move.l  %a0, TBL(%a6)
| AN APPLICATION: a relocated image runs in this partition next to the
| Finder and the others, so it gets a window, menus and the events (HINIT
| below); the progress lines are drawn in the window
        bsr     hinit
        lea     m_alloc(%pc), %a4
        bne     fail
        move.l  0x114, %d0              | HeapEnd, 64 KB on, to 512 KB
        add.l   #0x10000 + 0x7FFFF, %d0
        and.l   #0xFFF80000, %d0        | BASE
        move.l  %d0, %d1
        add.l   40(%a2), %d1            | BASE + SPAN must leave the stack 128 KB
        add.l   #0x20000, %d1
        cmp.l   0x908, %d1              | CurStackBase
        bhi     fail
        move.l  %d0, BASE(%a6)
        move.l  %d0, 0x130              | ApplLimit: the heap stops short of it
        lea     s_alloc(%pc), %a1
        moveq   #80, %d1
        bsr     say                     | "allocated" and BASE
        move.l  BASE(%a6), %d0
        sub.l   (%a2), %d0              | DELTA = BASE - DEST
        move.l  %d0, DELTA(%a6)
        move.l  BASE(%a6), (%a2)        | DEST
        add.l   %d0, 4(%a2)             | ENTRY
        add.l   %d0, 24(%a2)            | REFSLOT
        bsr     hsize
        lea     m_alloc(%pc), %a4
        bne     fail
5:
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
        tst.l   BASE(%a6)
        beq     6f
        lea     s_read(%pc), %a1
        moveq   #96, %d1
        move.l  8(%a2), %d0
        bsr     say                     | "read" and LEN
        bsr     relocate
        lea     m_reloc(%pc), %a4
        bne     fail
        lea     s_reloc(%pc), %a1
        moveq   #112, %d1
        move.l  4(%a2), %d0
        bsr     say                     | "relocated" and ENTRY
        move.l  TBL(%a6), %a0           | the table is done with: the heap
        .word   0xA01F                  |   is the Toolbox's (_DisposePtr)
        bsr     hclear
6:
| from here on the image's: its streams come from this file, and nothing may
| allocate in the zone it now covers
        move.l  24(%a2), %a3
        move.w  %d7, %d0
        ext.l   %d0
        move.l  %d0, (%a3)              | REFSLOT = the open file
        move.l  HHALF(%a6), 8(%a3)      | boot-mac68k +MAC68K-HEAP-SLOT+: the
        move.l  HKMAP(%a6), 12(%a3)     |   heap, or 0 for the layout's own
        tst.l   BASE(%a6)
        beq     7f
| an application: the host table in the slot after REFSLOT (translate-68k
| +68K-MAC-HOST+); the zone stays the application's, the cursor shown
        lea     htable(%pc), %a0
        move.l  %a0, 4(%a3)
        move.l  hstate(%pc), %a0        | the Toolbox's stack from now on
        lea     -64(%sp), %a1
        move.l  %a1, H_SP(%a0)
        bra     8f
7:      move.l  0x2A6, 0x118            | TheZone = SysZone
| THE STACK SNIFFER OFF.  The VBL task that checks, every tick, that the
| stack pointer is above the application heap's end (StkLowPt $110; 0 turns
| it off, Inside Macintosh) treats the image's stack -- in a relocating load,
| inside the NewPtr block in this application's own heap -- as a stack that
| ran into the heap: measured on a Quadra 950 under System 7.5.3, the first
| tick's interrupt never returned to the image, the process was ended, and
| the Finder then lost its disk.
        clr.l   0x110                   | StkLowPt
        .word   0xA852                  | _HideCursor (again is harmless)
8:      clr.l   0x110                   | StkLowPt (an application's too)
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

| HSIZE (a relocating load whose record has KMAP, the kind map's address as
| built: the young heap is last in the map, boot-mac68k :QUADRA): the heap
| from the partition.  From F = DEST + LEN (the low block's end, where the
| first semispace starts) up to 128 KB below the stack go two semispaces,
| GUARD, then the kind map -- a byte per 8 bytes of [region, guard's end),
| 64 KB-aligned, since its move is added to the high words of the x7/8
| sites.  So a semispace is 4/9 of what is left, in 64 KB units.  HHALF,
| HKMAP, HEXTRA; NE when the partition leaves less than 256 KB a semispace.
hsize:  tst.l   44(%a2)
        beq     9f
        move.l  (%a2), %d1
        add.l   8(%a2), %d1             | F
        move.l  0x908, %d2              | CurStackBase
        sub.l   #0x20000, %d2
        sub.l   %d1, %d2
        sub.l   #GUARD + 0x10000 + LOWBLK / 8, %d2
        bmi     8f
        clr.w   %d2
        swap    %d2                     | 64 KB units
        lsl.l   #2, %d2
        divu.w  #9, %d2
        and.l   #0xFFFF, %d2
        swap    %d2                     | HALF
        cmp.l   #0x40000, %d2
        blo     8f
        move.l  %d2, HHALF(%a6)
        add.l   %d2, %d2
        add.l   %d1, %d2
        add.l   #GUARD + 0xFFFF, %d2
        and.l   #0xFFFF0000, %d2
        move.l  %d2, HKMAP(%a6)
        sub.l   44(%a2), %d2
        sub.l   DELTA(%a6), %d2         | - (KMAP + DELTA): the move
        move.l  %d2, HEXTRA(%a6)
9:      moveq   #0, %d0
        rts
8:      moveq   #-1, %d0
        rts

| Apply the relocation table (A2: the record; BASE, TBL): read RELLEN bytes
| from RELOFF to TBL, then per kind (KIND COUNT, then COUNT gaps: a byte G
| = 2G bytes on, or 0 and a 4-byte offset) add ((DELTA >> 16) * KIND) / 8 to
| the 16-bit word at BASE + offset.  The table is byte-packed, so its longs
| are read a byte at a time (a 68000 faults on an odd MOVE.L).  Z set when
| it worked.
relocate:
        movem.l %d2-%d6/%a3-%a4, -(%sp)
        move.l  TBL(%a6), %d1
        move.l  36(%a2), %d2
        move.l  32(%a2), %d3
        bsr     rd
        bne     9f
        move.l  TBL(%a6), %a3
        move.l  DELTA(%a6), %d6         | DELTA >> 16 in the low word: DELTA
        swap    %d6                     |   is a multiple of 512 KB
        moveq   #2, %d5                 | three kinds
1:      bsr     rd32                    | KIND
        move.w  %d0, %d4
        muls.w  %d6, %d4                | (DELTA >> 16) * KIND, a multiple of 8
        asr.l   #3, %d4
        cmp.l   #7, %d0                 | a kind-map bias: and the kind map's move
        bne     11f
        move.l  HEXTRA(%a6), %d1
        swap    %d1
        add.w   %d1, %d4
11:
        bsr     rd32                    | COUNT
        move.l  %d0, %d2
        move.l  BASE(%a6), %a4
2:      tst.l   %d2
        beq     4f
        moveq   #0, %d0
        move.b  (%a3)+, %d0
        bne     3f
        bsr     rd32                    | an offset
        move.l  BASE(%a6), %a4
        add.l   %d0, %a4
        bra     31f
3:      add.l   %d0, %d0
        add.l   %d0, %a4
31:     add.w   %d4, (%a4)
        subq.l  #1, %d2
        bra     2b
4:      dbra    %d5, 1b
        moveq   #0, %d0
9:      movem.l (%sp)+, %d2-%d6/%a3-%a4
        tst.w   %d0
        rts

| SAY: draw the Pascal string at A1 at (12, D1), then D0 in hex: a launch's
| progress, on the screen, for whoever is watching it go wrong.  The Toolbox
| may use D0-D2/A0-A1.
say:    movem.l %d0-%d3/%a0-%a1, -(%sp)
        move.w  #12, -(%sp)
        move.w  %d1, -(%sp)
        .word   0xA893                  | _MoveTo
        move.l  20(%sp), -(%sp)         | the saved A1
        .word   0xA884                  | _DrawString
        move.l  (%sp), %d3              | the saved D0
        lea     HEXB(%a6), %a0
        move.b  #9, (%a0)+
        move.b  #32, (%a0)+
        moveq   #7, %d2
1:      rol.l   #4, %d3
        move.b  %d3, %d0
        and.b   #15, %d0
        add.b   #48, %d0
        cmp.b   #58, %d0
        blo     2f
        addq.b  #7, %d0
2:      move.b  %d0, (%a0)+
        dbra    %d2, 1b
        pea     HEXB(%a6)
        .word   0xA884
        movem.l (%sp)+, %d0-%d3/%a0-%a1
        rts

| D0 = the next 4 bytes at A3 (any alignment), big-endian
rd32:   moveq   #3, %d1
        moveq   #0, %d0
1:      lsl.l   #8, %d0
        move.b  (%a3)+, %d0
        dbra    %d1, 1b
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


| ---------------------------------------------------------------------------
| THE HOST: Modus as an application.  A relocated image runs in this
| partition next to the Finder and the other applications, so it takes
| nothing of the machine's: its console's putc and getc (translate-68k
| +68K-MAC-HOST+) jump here, through HTABLE, and this code owns a window,
| the menus and the event loop.
|
| The console is a window in Monaco 9, 64 x 16: a text buffer (redrawn
| on an update event), a caret while getc waits, a scroll a ScrollRect.
| getc waits in WaitNextEvent (GetNextEvent + SystemTask on a System 6
| without MultiFinder), so the other applications run while the REPL waits;
| it handles the menus (the Apple menu's desk accessories, File: Quit), a
| window drag or close, a click in another layer, and returns a key.
| Quit or the close box: the data fork closed, _ExitToShell.
|
| Called from the image: the image's registers, the image's stack, any A5.
| Every register but D0/A0 is kept, A5 is this application's (CurrentA5
| $904) across each Toolbox call.  The state is a block in the heap (HSTATE).
| THE TOOLBOX RUNS ON THE APPLICATION'S OWN STACK: the one this code had
| when it jumped to the image (H_SP), not the image's.  On the image's --
| inside the map, between the heap and the application stack -- the first
| _SetPort ended the process (a Quadra 950 under System 7.5.3: PC in a
| system patch, then "The disk Macintosh cannot be found"), though the File
| Manager's reads from there had always worked.
        .equ    H_WIN, 0                | the WindowPtr
        .equ    H_COLS, 4
        .equ    H_ROWS, 6
        .equ    H_X, 8                  | the cursor
        .equ    H_Y, 10
        .equ    H_CARET, 12             | the caret is drawn
        .equ    H_CW, 14                | a cell: width, height, ascent
        .equ    H_LH, 16
        .equ    H_ASC, 18
        .equ    H_TR, 20                | the text's rectangle
        .equ    H_BUF, 28               | the text, COLS x ROWS bytes
        .equ    H_RGN, 32               | ScrollRect's update region
        .equ    H_WNE, 36               | WaitNextEvent is there
        .equ    H_REF, 38               | the data fork
        .equ    H_EVT, 40               | an EventRecord: what message when where modifiers
        .equ    H_WW, 56                | FindWindow's window
        .equ    H_R, 60                 | a rectangle
        .equ    H_FI, 68                | FontInfo: ascent descent widMax leading
        .equ    H_APPLE, 76
        .equ    H_FILE, 80
        .equ    H_STR, 84               | a Str255
        .equ    H_PB, 340               | a parameter block, 80 bytes
        .equ    H_SP, 420               | the application's stack (HSTACK)
        .equ    H_SIZE, 424

hstate: .long   0
htable: .long   0, 0, 0                 | putc getc poll, filled by HINIT

| HINIT: the state, the menus, the window (the current port), the text
| buffer.  D7 = the data fork.  Z set when it worked.
hinit:  movem.l %d1-%d7/%a0-%a4, -(%sp)
        lea     htable(%pc), %a0
        lea     hputc(%pc), %a1
        move.l  %a1, (%a0)+
        lea     hgetc(%pc), %a1
        move.l  %a1, (%a0)+
        lea     hpoll(%pc), %a1
        move.l  %a1, (%a0)+
        move.l  #H_SIZE, %d0
        .word   0xA31E                  | _NewPtr ,CLEAR
        tst.w   %d0
        bne     9f
        lea     hstate(%pc), %a1
        move.l  %a0, (%a1)
        move.l  %a0, %a3
        move.w  %d7, H_REF(%a3)
| the menus: the Apple menu (About, then the desk accessories) and File
        subq.l  #4, %sp
        move.w  #128, -(%sp)
        pea     t_apple(%pc)
        .word   0xA931                  | _NewMenu
        move.l  (%sp)+, %a4
        move.l  %a4, H_APPLE(%a3)
        move.l  %a4, -(%sp)
        pea     i_about(%pc)
        .word   0xA933                  | _AppendMenu
        move.l  %a4, -(%sp)
        move.l  #0x44525652, -(%sp)     | 'DRVR'
        .word   0xA94D                  | _AddResMenu
        move.l  %a4, -(%sp)
        clr.w   -(%sp)
        .word   0xA935                  | _InsertMenu
        subq.l  #4, %sp
        move.w  #129, -(%sp)
        pea     t_file(%pc)
        .word   0xA931
        move.l  (%sp)+, %a4
        move.l  %a4, H_FILE(%a3)
        move.l  %a4, -(%sp)
        pea     i_quit(%pc)
        .word   0xA933
        move.l  %a4, -(%sp)
        clr.w   -(%sp)
        .word   0xA935
        .word   0xA937                  | _DrawMenuBar
| the window: a terminal of WCOLS x WROWS Monaco 9 cells (6 x 11 pixels),
| below the menu bar and a title bar, inside the screen (QuickDraw's
| screenBits.bounds, -120(A5)) on a small one
        .equ    WCOLS, 64
        .equ    WROWS, 16
        lea     H_R(%a3), %a0
        move.w  -120(%a5), %d0
        add.w   #42, %d0
        move.w  %d0, (%a0)
        add.w   #WROWS * 11 + 4, %d0
        move.w  -116(%a5), %d1
        subq.w  #4, %d1
        cmp.w   %d1, %d0
        ble     1f
        move.w  %d1, %d0
1:      move.w  %d0, 4(%a0)
        move.w  -118(%a5), %d0
        addq.w  #8, %d0
        move.w  %d0, 2(%a0)
        add.w   #WCOLS * 6 + 8, %d0
        move.w  -114(%a5), %d1
        subq.w  #4, %d1
        cmp.w   %d1, %d0
        ble     2f
        move.w  %d1, %d0
2:      move.w  %d0, 6(%a0)
        subq.l  #4, %sp
        clr.l   -(%sp)                  | the record: in the heap
        pea     H_R(%a3)
        pea     t_win(%pc)
        move.b  #1, -(%sp)              | visible
        move.w  #4, -(%sp)              | noGrowDocProc
        moveq   #-1, %d0
        move.l  %d0, -(%sp)             | in front
        move.b  #1, -(%sp)              | a close box
        clr.l   -(%sp)
        .word   0xA913                  | _NewWindow
        move.l  (%sp)+, %a4
        move.l  %a4, H_WIN(%a3)
        moveq   #-1, %d0
        cmp.l   #0, %a4
        beq     9f
        move.l  %a4, -(%sp)
        .word   0xA873                  | _SetPort
        move.w  #4, -(%sp)              | Monaco
        .word   0xA887                  | _TextFont
        move.w  #9, -(%sp)
        .word   0xA88A                  | _TextSize
        pea     H_FI(%a3)
        .word   0xA88B                  | _GetFontInfo
        move.w  H_FI(%a3), %d0
        move.w  %d0, H_ASC(%a3)
        add.w   H_FI+2(%a3), %d0
        add.w   H_FI+6(%a3), %d0
        move.w  %d0, H_LH(%a3)
        move.w  H_FI+4(%a3), H_CW(%a3)
| COLS x ROWS from the port's rectangle (16(A4): top left bottom right)
        moveq   #0, %d0
        move.w  22(%a4), %d0
        sub.w   18(%a4), %d0
        subq.w  #8, %d0
        divu.w  H_CW(%a3), %d0
        move.w  %d0, H_COLS(%a3)
        moveq   #0, %d0
        move.w  20(%a4), %d0
        sub.w   16(%a4), %d0
        subq.w  #4, %d0
        divu.w  H_LH(%a3), %d0
        move.w  %d0, H_ROWS(%a3)
        lea     H_TR(%a3), %a0
        move.w  #2, (%a0)
        move.w  #4, 2(%a0)
        move.w  H_ROWS(%a3), %d0
        mulu.w  H_LH(%a3), %d0
        addq.w  #2, %d0
        move.w  %d0, 4(%a0)
        move.w  H_COLS(%a3), %d0
        mulu.w  H_CW(%a3), %d0
        addq.w  #4, %d0
        move.w  %d0, 6(%a0)
        move.w  H_COLS(%a3), %d0
        mulu.w  H_ROWS(%a3), %d0
        move.l  %d0, %d3
        .word   0xA11E                  | _NewPtr: the text
        tst.w   %d0
        bne     9f
        move.l  %a0, H_BUF(%a3)
1:      move.b  #32, (%a0)+
        subq.l  #1, %d3
        bne     1b
        subq.l  #4, %sp
        .word   0xA8D8                  | _NewRgn
        move.l  (%sp)+, H_RGN(%a3)
| WaitNextEvent: is it implemented?  (System 6 without MultiFinder: no)
        move.w  #0x60, %d0
        .word   0xA746                  | _GetToolTrapAddress
        move.l  %a0, %d1
        move.w  #0x9F, %d0              | _Unimplemented's
        .word   0xA746
        cmp.l   %a0, %d1
        sne     H_WNE(%a3)
        moveq   #0, %d0
9:      movem.l (%sp)+, %d1-%d7/%a0-%a4
        tst.w   %d0
        rts

| HCLEAR: the window blank, the cursor home (the launch's progress lines go)
hclear: movem.l %d0-%d2/%a0-%a1/%a3-%a4, -(%sp)
        move.l  hstate(%pc), %a3
        move.l  H_WIN(%a3), %a4
        pea     16(%a4)
        .word   0xA8A3                  | _EraseRect
        clr.l   H_X(%a3)                | X and Y
        movem.l (%sp)+, %d0-%d2/%a0-%a1/%a3-%a4
        rts

| onto the application's stack, the image's SP pushed there (the caller
| pops it back with MOVEA.L (SP),SP).  A BSR'd routine: its return moves too.
hstack: move.l  (%sp)+, %a1             | the return
        move.l  %sp, %d1                | the image's SP
        move.l  hstate(%pc), %a3
        move.l  H_SP(%a3), %sp
        move.l  %d1, -(%sp)
        jmp     (%a1)

| the entry: A5 this application's, A3 the state, the window the port
henter: move.l  0x904, %a5              | CurrentA5
        move.l  hstate(%pc), %a3
        move.l  H_WIN(%a3), -(%sp)
        .word   0xA873                  | _SetPort
        rts

| PUTC: D0.B to the console
hputc:  movem.l %d1-%d7/%a1-%a6, -(%sp)
        bsr     hstack
        bsr     henter
        moveq   #0, %d4
        move.b  %d0, %d4
        cmp.b   #10, %d4
        beq     hnl
        cmp.b   #13, %d4
        beq     hcr
        cmp.b   #8, %d4
        beq     hbs
        cmp.b   #9, %d4
        beq     htab
        cmp.b   #7, %d4
        beq     hbel
        cmp.b   #32, %d4
        blo     hout
        bsr     hput
hout:   movea.l (%sp), %sp              | the image's stack
        movem.l (%sp)+, %d1-%d7/%a1-%a6
        rts
hcr:    clr.w   H_X(%a3)
        bra     hout
hbs:    tst.w   H_X(%a3)
        beq     hout
        subq.w  #1, H_X(%a3)
        bra     hout
htab:   moveq   #32, %d4
1:      bsr     hput
        move.w  H_X(%a3), %d0
        and.w   #7, %d0
        bne     1b
        bra     hout
hbel:   move.w  #1, -(%sp)
        .word   0xA9C8                  | _SysBeep
        bra     hout
hnl:    bsr     hnewl
        bra     hout

| D4 at the cursor, in the buffer and the window; the cursor on
hput:   move.w  H_Y(%a3), %d1
        mulu.w  H_COLS(%a3), %d1
        add.w   H_X(%a3), %d1
        move.l  H_BUF(%a3), %a0
        move.b  %d4, (%a0,%d1.l)
        move.w  H_X(%a3), %d1
        move.w  H_Y(%a3), %d2
        bsr     hmove
        move.w  %d4, -(%sp)
        .word   0xA883                  | _DrawChar
        addq.w  #1, H_X(%a3)
        move.w  H_X(%a3), %d0
        cmp.w   H_COLS(%a3), %d0
        blo     1f
        bsr     hnewl
1:      rts

| MoveTo the baseline of cell D1, D2
hmove:  mulu.w  H_CW(%a3), %d1
        add.w   H_TR+2(%a3), %d1
        mulu.w  H_LH(%a3), %d2
        add.w   H_TR(%a3), %d2
        add.w   H_ASC(%a3), %d2
        move.w  %d1, -(%sp)
        move.w  %d2, -(%sp)
        .word   0xA893                  | _MoveTo
        rts

| a new line, and at the bottom a scroll: the window's bits up a line
| (what was hidden and is now shown comes back as an update), the text too
hnewl:  clr.w   H_X(%a3)
        addq.w  #1, H_Y(%a3)
        move.w  H_Y(%a3), %d0
        cmp.w   H_ROWS(%a3), %d0
        blo     9f
        subq.w  #1, H_Y(%a3)
        pea     H_TR(%a3)
        clr.w   -(%sp)
        move.w  H_LH(%a3), %d0
        neg.w   %d0
        move.w  %d0, -(%sp)
        move.l  H_RGN(%a3), -(%sp)
        .word   0xA8EF                  | _ScrollRect
        move.l  H_RGN(%a3), -(%sp)
        .word   0xA927                  | _InvalRgn
        moveq   #0, %d0
        move.w  H_COLS(%a3), %d0
        move.l  H_BUF(%a3), %a1
        lea     (%a1,%d0.l), %a0
        move.w  H_ROWS(%a3), %d1
        subq.w  #1, %d1
        mulu.w  %d1, %d0
        move.l  %d0, %d2
        .word   0xA02E                  | _BlockMove
        move.l  H_BUF(%a3), %a0
        add.l   %d2, %a0
        move.w  H_COLS(%a3), %d0
        subq.w  #1, %d0
1:      move.b  #32, (%a0)+
        dbra    %d0, 1b
9:      rts

| the caret: the cursor's cell inverted.  HCARON / HCAROFF keep D0.
hcaron: tst.b   H_CARET(%a3)
        bne     9f
        bra     hcarinv
hcaroff:
        tst.b   H_CARET(%a3)
        beq     9f
hcarinv:
        move.l  %d0, -(%sp)
        not.b   H_CARET(%a3)
        lea     H_R(%a3), %a0
        move.w  H_Y(%a3), %d0
        mulu.w  H_LH(%a3), %d0
        add.w   H_TR(%a3), %d0
        move.w  %d0, (%a0)
        add.w   H_LH(%a3), %d0
        move.w  %d0, 4(%a0)
        move.w  H_X(%a3), %d0
        mulu.w  H_CW(%a3), %d0
        add.w   H_TR+2(%a3), %d0
        move.w  %d0, 2(%a0)
        add.w   H_CW(%a3), %d0
        move.w  %d0, 6(%a0)
        move.l  %a0, -(%sp)
        .word   0xA8A4                  | _InverRect
        move.l  (%sp)+, %d0
9:      rts

| the whole window from the text (inside BeginUpdate .. EndUpdate)
hredraw:
        move.l  H_WIN(%a3), %a0
        pea     16(%a0)
        .word   0xA8A3                  | _EraseRect
        moveq   #0, %d3                 | the row
        moveq   #0, %d4                 | its first byte
1:      cmp.w   H_ROWS(%a3), %d3
        bhs     2f
        moveq   #0, %d1
        move.w  %d3, %d2
        bsr     hmove
        move.l  H_BUF(%a3), -(%sp)
        move.w  %d4, -(%sp)
        move.w  H_COLS(%a3), -(%sp)
        .word   0xA885                  | _DrawText
        addq.w  #1, %d3
        add.w   H_COLS(%a3), %d4
        bra     1b
2:      tst.b   H_CARET(%a3)
        beq     3f
        clr.b   H_CARET(%a3)
        bsr     hcaron
3:      rts

| GETC: -> D0.L a key, the other applications running while it waits
hgetc:  movem.l %d1-%d7/%a1-%a6, -(%sp)
        bsr     hstack
        bsr     henter
        bsr     hcaron
1:      bsr     hevent
        tst.l   %d0
        bmi     1b
        bsr     hcaroff
        movea.l (%sp), %sp
        movem.l (%sp)+, %d1-%d7/%a1-%a6
        rts

| POLL: nothing yet (the image does not call it)
hpoll:  rts

| one event: D0 = a key, or -1
hevent: tst.b   H_WNE(%a3)
        beq     1f
        clr.w   -(%sp)
        move.w  #-1, -(%sp)             | everyEvent
        pea     H_EVT(%a3)
        moveq   #30, %d0                | sleep half a second
        move.l  %d0, -(%sp)
        clr.l   -(%sp)                  | no mouse region
        .word   0xA860                  | _WaitNextEvent
        bra     2f
1:      .word   0xA9B4                  | _SystemTask
        clr.w   -(%sp)
        move.w  #-1, -(%sp)
        pea     H_EVT(%a3)
        .word   0xA970                  | _GetNextEvent
2:      move.l  H_WIN(%a3), -(%sp)      | a desk accessory may have moved the port
        .word   0xA873
        move.b  (%sp)+, %d0
        beq     hnone
        move.w  H_EVT(%a3), %d0
        cmp.w   #1, %d0
        beq     hmouse
        cmp.w   #3, %d0
        beq     hkey
        cmp.w   #5, %d0
        beq     hkey
        cmp.w   #6, %d0
        beq     hupd
hnone:  moveq   #-1, %d0
        rts

hkey:   moveq   #0, %d0
        move.b  H_EVT+5(%a3), %d0       | the message's low byte: the character
        btst    #0, H_EVT+14(%a3)       | cmdKey (modifiers bit 8)
        beq     9f
        clr.l   -(%sp)
        move.w  %d0, -(%sp)
        .word   0xA93E                  | _MenuKey
        move.l  (%sp)+, %d0
        bsr     hmenu
        bra     hnone
9:      rts

hupd:   move.l  H_EVT+2(%a3), %a0       | the window
        cmp.l   H_WIN(%a3), %a0
        bne     hnone
        move.l  %a0, -(%sp)
        .word   0xA922                  | _BeginUpdate
        bsr     hredraw
        move.l  H_WIN(%a3), -(%sp)
        .word   0xA923                  | _EndUpdate
        bra     hnone

hmouse: clr.w   -(%sp)
        move.l  H_EVT+10(%a3), -(%sp)
        pea     H_WW(%a3)
        .word   0xA92C                  | _FindWindow
        move.w  (%sp)+, %d0
        cmp.w   #1, %d0                 | inMenuBar
        bne     1f
        clr.l   -(%sp)
        move.l  H_EVT+10(%a3), -(%sp)
        .word   0xA93D                  | _MenuSelect
        move.l  (%sp)+, %d0
        bsr     hmenu
        bra     hnone
1:      cmp.w   #2, %d0                 | inSysWindow
        bne     2f
        pea     H_EVT(%a3)
        move.l  H_WW(%a3), -(%sp)
        .word   0xA9B3                  | _SystemClick
        bra     hnone
2:      cmp.w   #3, %d0                 | inContent
        bne     3f
        move.l  H_WW(%a3), -(%sp)
        .word   0xA91F                  | _SelectWindow
        bra     hnone
3:      cmp.w   #4, %d0                 | inDrag
        bne     4f
        move.l  H_WW(%a3), -(%sp)
        move.l  H_EVT+10(%a3), -(%sp)
        pea     -120(%a5)               | screenBits.bounds
        .word   0xA925                  | _DragWindow
        bra     hnone
4:      cmp.w   #6, %d0                 | inGoAway
        bne     hnone
        clr.w   -(%sp)
        move.l  H_WW(%a3), -(%sp)
        move.l  H_EVT+10(%a3), -(%sp)
        .word   0xA91E                  | _TrackGoAway
        tst.b   (%sp)+
        bne     hquit
        bra     hnone

| a menu choice, D0 = menu << 16 | item
hmenu:  move.l  %d0, %d1
        swap    %d1
        cmp.w   #129, %d1               | File
        bne     1f
        cmp.w   #1, %d0                 | Quit
        beq     hquit
1:      cmp.w   #128, %d1               | Apple: a desk accessory
        bne     2f
        cmp.w   #3, %d0
        blo     2f
        move.l  H_APPLE(%a3), -(%sp)
        move.w  %d0, -(%sp)
        pea     H_STR(%a3)
        .word   0xA946                  | _GetMenuItemText
        clr.w   -(%sp)
        pea     H_STR(%a3)
        .word   0xA9B6                  | _OpenDeskAcc
        addq.l  #2, %sp
2:      clr.w   -(%sp)
        .word   0xA938                  | _HiliteMenu 0
        move.l  H_WIN(%a3), -(%sp)
        .word   0xA873                  | _SetPort
        rts

| QUIT: the data fork closed, back to the Finder
hquit:  lea     H_PB(%a3), %a0
        moveq   #19, %d0
1:      clr.l   (%a0)+
        dbra    %d0, 1b
        lea     H_PB(%a3), %a0
        move.w  H_REF(%a3), 24(%a0)
        .word   0xA001                  | _Close
        .word   0xA9F4                  | _ExitToShell

t_apple:    .byte 1, 0x14
t_file:     pstr "File"
t_win:      pstr "Modus"
i_about:    pstr "(About Modus...;(-"
i_quit:     pstr "Quit/Q"
        .align  2

band:   .word   20, 0, 64, 512          | top left bottom right
m_open:     pstr "Modus: cannot open my own data fork."
m_read:     pstr "Modus: cannot read the image in my data."
m_notmodus: pstr "Modus: my data fork is not a modus boot disk."
m_notchain: pstr "Modus: this image cannot run on top of Mac OS."
m_ram:      pstr "Modus: this Mac has less memory than the image is built for."
m_sysheap:  pstr "Modus: the System heap is too big.  Try with fewer extensions."
m_where:    pstr "Modus: launched where the image has to go."
m_click:    pstr "Click to return to the Finder."
m_alloc:    pstr "Modus: my partition is too small for the image (Get Info: raise its size)."
m_reloc:    pstr "Modus: cannot read my relocation table."
s_alloc:    pstr "Modus: allocated at"
s_read:     pstr "Modus: read"
s_reloc:    pstr "Modus: relocated, entry"
        .align  2
