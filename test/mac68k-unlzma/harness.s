| harness.s -- scripts/mac68k-lzma.inc as a Linux/m68k program, to check it
| against other LZMA1 decoders: `harness SIZE [DSIZE] < [DICT] STREAM > OUT`
| (sizes in decimal; the stream lc=2 lp=1 pb=2, raw; with DSIZE, the input's
| first DSIZE bytes are a preset dictionary, the stream after them).  Linux/m68k
| syscalls: TRAP #0, D0 the number.
        .text
        .globl  _start
_start: move.l  8(%sp), %a0             | argv[1]: SIZE
        bsr     decimal
        move.l  %d7, size
        moveq   #0, %d7
        move.l  12(%sp), %d0            | argv[2]: DSIZE, if any
        beq     2f
        move.l  %d0, %a0
        bsr     decimal
2:      move.l  %d7, dsize
        move.l  #BUFIN, %a6             | read stdin to EOF
3:      moveq   #3, %d0
        moveq   #0, %d1
        move.l  %a6, %d2
        move.l  #0x100000, %d3
        trap    #0
        tst.l   %d0
        ble     4f
        add.l   %d0, %a6
        bra     3b
4:      move.l  #BUFOUT, %a5
        move.l  %a5, %a4
        add.l   size, %a4
        move.l  #BUFIN, %a2
        move.l  #probs, %a3
        clr.l   -16(%a3)                | DICTEND: none
        move.l  dsize, %d0
        beq     7f
        add.l   %d0, %a2                | the stream after the dictionary
        move.l  %a2, -16(%a3)           | DICTEND: the dictionary is BUFIN's start
7:      bsr     lzdec
        move.l  #BUFOUT, %a6            | write SIZE bytes to stdout
        move.l  size, %d7
5:      tst.l   %d7
        beq     6f
        moveq   #4, %d0
        moveq   #1, %d1
        move.l  %a6, %d2
        move.l  %d7, %d3
        trap    #0
        tst.l   %d0
        ble     6f
        add.l   %d0, %a6
        sub.l   %d0, %d7
        bra     5b
6:      moveq   #1, %d0
        moveq   #0, %d1
        trap    #0
decimal:                                | A0 = digits -> D7
        moveq   #0, %d7
1:      moveq   #0, %d0
        move.b  (%a0)+, %d0
        beq     2f
        sub.b   #'0', %d0
        mulu.l  #10, %d7
        add.l   %d0, %d7
        bra     1b
2:      rts
        .data
size:   .long   0
dsize:  .long   0
        .bss
        .align  4
        .space  32                      | REP1..REP3, DICTEND, DELTA below the probabilities
probs:  .space  16384
BUFIN:  .space  0x04000000
BUFOUT: .space  0x04000000
        .text
        .include "mac68k-lzma.inc"
