| mac68k-lzma.inc -- a raw LZMA1 decoder in 68020 assembly (LzmaSpec.cpp's
| algorithm, as mvm/unlzma-core.lisp has it), shared by the Mac boot loader
| (scripts/mac68k-unlzma.s) and its hosted test (test/mac68k-unlzma/).
|
| The stream is what scripts/mac68k-floppy.lisp writes: lc=2 lp=1 pb=2, no
| header, no end marker, the unpacked size known.  Those three parameters are
| built in (the literal context below, the pos-state masks).
|
|   lzdec:  A5 = output start, A4 = output end, A2 = the stream (its first
|           byte, which is 0), A3 = 7991 u16 probabilities (15982 bytes,
|           initialised here) with 12 bytes below A3 for REP1..REP3.
|           Returns with the output written.  Uses D0-D7, A0-A6.
|
| 68020 and up: MULU.L and scaled indexing (the floppy's Macs are an LC and a
| Quadra; a 68000 would need a 16x16 split multiply here).
|
| Registers while decoding: D7 range, D6 code, D5 state, D4 rep0, A1 the
| output pointer.  RCBIT returns its bit in the condition codes -- Z when 0,
| X and C when 1 -- so a bit tree shifts it in with one ADDX.

        .equ    IS_MATCH, 0
        .equ    IS_REP, 192
        .equ    IS_REP_G0, 204
        .equ    IS_REP_G1, 216
        .equ    IS_REP_G2, 228
        .equ    IS_REP0_LONG, 240
        .equ    POS_SLOT, 432
        .equ    SPEC_POS, 688
        .equ    ALIGN, 803
        .equ    LEN_DEC, 819
        .equ    REP_LEN_DEC, 1333
        .equ    LITERAL, 1847
        .equ    N_PROBS, 7991
        .equ    REP1, -4
        .equ    REP2, -8
        .equ    REP3, -12

lzdec:  move.l  %a3, %a0                | every probability 1024
        move.w  #N_PROBS-1, %d0
1:      move.w  #1024, (%a0)+
        dbra    %d0, 1b
        moveq   #-1, %d7                | range
        addq.l  #1, %a2                 | the first byte is 0
        moveq   #3, %d0                 | code: the next four, big-endian
2:      lsl.l   #8, %d6
        move.b  (%a2)+, %d6
        dbra    %d0, 2b
        moveq   #0, %d5                 | state
        moveq   #0, %d4                 | rep0..rep3
        clr.l   REP1(%a3)
        clr.l   REP2(%a3)
        clr.l   REP3(%a3)
        move.l  %a5, %a1

lzloop: cmp.l   %a4, %a1
        bcc     lzdone
        move.l  %a1, %d3                | D3 = pos & 3
        sub.l   %a5, %d3
        and.l   #3, %d3
        move.l  %d5, %d0                | is_match[state*16 + posState]
        lsl.l   #4, %d0
        add.l   %d3, %d0
        lea     (IS_MATCH*2, %a3, %d0.l*2), %a0
        bsr     rcbit
        bne     lzmatch

| A literal: probabilities LITERAL + 0x300 * (((pos & 1) << 2) + (prev >> 6))
        moveq   #0, %d0
        cmp.l   %a5, %a1
        beq     3f
        move.b  -1(%a1), %d0
        lsr.b   #6, %d0
3:      btst    #0, %d3
        beq     4f
        addq.w  #4, %d0
4:      mulu.w  #0x600, %d0             | x 0x300 probabilities x 2 bytes
        lea     (LITERAL*2, %a3, %d0.l), %a6
        moveq   #1, %d0                 | sym
        cmp.w   #7, %d5
        bcs     lit_plain
        move.l  %a1, %a0                | the match byte, at -rep0-1
        sub.l   %d4, %a0
        move.b  -1(%a0), %d3
lit_match:
        add.b   %d3, %d3                | C = the match bit
        bcs     lm1
        lea     (512, %a6, %d0.l*2), %a0 | match bit 0: probs + 0x100 + sym
        bsr     rcbit
        addx.w  %d0, %d0
        btst    #0, %d0
        bne     lit_plain               | the bits differ: plain from here
        cmp.w   #256, %d0
        bcs     lit_match
        bra     lit_store
lm1:    lea     (1024, %a6, %d0.l*2), %a0 | match bit 1: probs + 0x200 + sym
        bsr     rcbit
        addx.w  %d0, %d0
        btst    #0, %d0
        beq     lit_plain
        cmp.w   #256, %d0
        bcs     lit_match
        bra     lit_store
lit_plain:
        cmp.w   #256, %d0
        bcc     lit_store
5:      lea     (0, %a6, %d0.l*2), %a0
        bsr     rcbit
        addx.w  %d0, %d0
        cmp.w   #256, %d0
        bcs     5b
lit_store:
        move.b  %d0, (%a1)+
        cmp.w   #4, %d5                 | state: <4 -> 0, <10 -> -3, else -6
        bcc     6f
        moveq   #0, %d5
        bra     lzloop
6:      cmp.w   #10, %d5
        bcc     7f
        subq.w  #3, %d5
        bra     lzloop
7:      subq.w  #6, %d5
        bra     lzloop

lzmatch:
        lea     (IS_REP*2, %a3, %d5.l*2), %a0
        bsr     rcbit
        bne     lzrep
        move.l  REP2(%a3), REP3(%a3)    | a new match
        move.l  REP1(%a3), REP2(%a3)
        move.l  %d4, REP1(%a3)
        lea     LEN_DEC*2(%a3), %a6
        bsr     lendec
        cmp.w   #7, %d5
        bcs     8f
        moveq   #10, %d5
        bra     9f
8:      moveq   #7, %d5
9:      move.l  %d0, -(%sp)
        bsr     distdec
        move.l  (%sp)+, %d0
        bra     lzcopy

lzrep:  lea     (IS_REP_G0*2, %a3, %d5.l*2), %a0
        bsr     rcbit
        bne     lzrepg1
        move.l  %d5, %d0                | is_rep0_long[state*16 + posState]
        lsl.l   #4, %d0
        add.l   %d3, %d0
        lea     (IS_REP0_LONG*2, %a3, %d0.l*2), %a0
        bsr     rcbit
        bne     lzreplen
        cmp.w   #7, %d5                 | a short rep: one byte at rep0
        bcs     10f
        moveq   #11, %d5
        bra     11f
10:     moveq   #9, %d5
11:     move.l  %a1, %a0
        sub.l   %d4, %a0
        move.b  -1(%a0), (%a1)+
        bra     lzloop
lzrepg1:
        lea     (IS_REP_G1*2, %a3, %d5.l*2), %a0
        bsr     rcbit
        bne     12f
        move.l  REP1(%a3), %d1          | dist = rep1
        bra     14f
12:     lea     (IS_REP_G2*2, %a3, %d5.l*2), %a0
        bsr     rcbit
        bne     13f
        move.l  REP2(%a3), %d1          | dist = rep2
        bra     15f
13:     move.l  REP3(%a3), %d1          | dist = rep3; rep3 = rep2
        move.l  REP2(%a3), REP3(%a3)
15:     move.l  REP1(%a3), REP2(%a3)    | rep2 = rep1
14:     move.l  %d4, REP1(%a3)          | rep1 = rep0; rep0 = dist
        move.l  %d1, %d4
lzreplen:
        lea     REP_LEN_DEC*2(%a3), %a6
        bsr     lendec
        cmp.w   #7, %d5
        bcs     16f
        moveq   #11, %d5
        bra     lzcopy
16:     moveq   #8, %d5

| Copy len + 2 bytes from rep0 + 1 back, no further than the output's end.
lzcopy: addq.l  #2, %d0
        move.l  %a4, %d1
        sub.l   %a1, %d1
        cmp.l   %d1, %d0
        bls     17f
        move.l  %d1, %d0
17:     move.l  %a1, %a0
        sub.l   %d4, %a0
        subq.l  #1, %a0
        subq.w  #1, %d0
18:     move.b  (%a0)+, (%a1)+
        dbra    %d0, 18b
        bra     lzloop

lzdone: rts

| rcbit: A0 = a probability.  Decode one bit with it, update it, normalise.
| Z when 0; X and C when 1.  Uses D1, D2.
rcbit:  moveq   #0, %d1
        move.w  (%a0), %d1
        move.l  %d7, %d2
        lsr.l   #8, %d2
        lsr.l   #3, %d2
        mulu.l  %d1, %d2                | bound = (range >> 11) * p
        cmp.l   %d2, %d6
        bcc     rc1
        move.l  %d2, %d7                | 0: range = bound; p += (2048 - p) >> 5
        move.w  #2048, %d2
        sub.w   %d1, %d2
        lsr.w   #5, %d2
        add.w   %d2, (%a0)
        cmp.l   #0x01000000, %d7
        bcc     rc0n
        lsl.l   #8, %d7
        lsl.l   #8, %d6
        move.b  (%a2)+, %d6
rc0n:   move.w  #4, %ccr
        rts
rc1:    sub.l   %d2, %d7                | 1: range -= bound; code -= bound; p -= p >> 5
        sub.l   %d2, %d6
        lsr.w   #5, %d1
        sub.w   %d1, (%a0)
        cmp.l   #0x01000000, %d7
        bcc     rc1n
        lsl.l   #8, %d7
        lsl.l   #8, %d6
        move.b  (%a2)+, %d6
rc1n:   move.w  #0x11, %ccr
        rts

| bittree: A6 = a tree's probabilities (index 1 up), D3 = bits - 1.
| D0 = the symbol with its top bit (1 << bits) still set.
bittree:
        moveq   #1, %d0
19:     lea     (0, %a6, %d0.l*2), %a0
        bsr     rcbit
        addx.w  %d0, %d0
        dbra    %d3, 19b
        rts

| bittree_rev: the same, least significant bit first.  D0 = the symbol.
| Uses D3 (bits - 1 in), D1, D2.
bittree_rev:
        move.l  %d3, -(%sp)
        bsr     bittree
        move.l  (%sp)+, %d3
        moveq   #0, %d1                 | reverse the low bits of D0
20:     lsr.l   #1, %d0
        roxl.l  #1, %d1
        dbra    %d3, 20b
        move.l  %d1, %d0
        rts

| lendec: A6 = a length coder.  D0 = the length (0..271).  Uses D3.
lendec: move.l  %a6, %a0
        bsr     rcbit
        bne     21f
        moveq   #0, %d0                 | low: 3 bits at 2 + posState*8
        move.w  %d3, %d0
        lsl.w   #4, %d0
        add.l   %d0, %a6
        addq.l  #4, %a6
        moveq   #2, %d3
        bsr     bittree
        subq.w  #8, %d0
        rts
21:     lea     2(%a6), %a0
        bsr     rcbit
        bne     22f
        moveq   #0, %d0                 | mid: 8 + 3 bits at 130 + posState*8
        move.w  %d3, %d0
        lsl.w   #4, %d0
        add.l   %d0, %a6
        lea     260(%a6), %a6
        moveq   #2, %d3
        bra     bittree                 | m - 8 + 8
22:     lea     516(%a6), %a6           | high: 16 + 8 bits at 258
        moveq   #7, %d3
        bsr     bittree
        sub.w   #240, %d0               | m - 256 + 16
        rts

| distdec: D0 = len.  D4 = the new rep0.
distdec:
        cmp.w   #3, %d0                 | lenState = min(len, 3)
        bls     23f
        moveq   #3, %d0
23:     lsl.w   #7, %d0                 | x 64 probabilities x 2 bytes
        lea     (POS_SLOT*2, %a3, %d0.l), %a6
        moveq   #5, %d3
        bsr     bittree
        sub.w   #64, %d0                | slot
        cmp.w   #4, %d0
        bcc     24f
        move.l  %d0, %d4
        rts
24:     move.l  %d0, %d1
        lsr.w   #1, %d1
        subq.w  #1, %d1                 | numDirectBits
        moveq   #1, %d4                 | (2 | slot & 1)
        and.l   %d0, %d4
        addq.l  #2, %d4
        cmp.w   #14, %d0
        bcc     26f
        lsl.l   %d1, %d4                | dist = that << nd
        move.l  %d4, %d2                | + rev tree at SPEC_POS + dist - slot
        sub.l   %d0, %d2
        lea     (SPEC_POS*2, %a3, %d2.l*2), %a6
        move.w  %d1, %d3
        subq.w  #1, %d3
        bsr     bittree_rev
        add.l   %d0, %d4
        rts
26:     subq.w  #5, %d1                 | nd - 4 direct bits (dbra count)
27:     lsr.l   #1, %d7
        cmp.l   %d7, %d6
        bcs     28f
        sub.l   %d7, %d6
28:     scc     %d0                     | the bit: code >= range
        neg.b   %d0                     | 0 or 1, as X
        lsr.b   #1, %d0
        addx.l  %d4, %d4
        cmp.l   #0x01000000, %d7
        bcc     29f
        lsl.l   #8, %d7
        lsl.l   #8, %d6
        move.b  (%a2)+, %d6
29:     dbra    %d1, 27b
        lsl.l   #4, %d4
        lea     ALIGN*2(%a3), %a6
        moveq   #3, %d3
        bsr     bittree_rev
        add.l   %d0, %d4
        rts
