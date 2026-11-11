| 68k-floppy-copier.s -- the last step of the floppy loader (mvm/68k-unlzma.lisp).
|
| Runs from the payload segment, after the decoder has unpacked the image's
| segments.  For each entry of the table that the packer appends right after
| this code (TABLE below)
|     count.l, then count x { vaddr.l src.l filesz.l memsz.l }, then entry.l
| it maps [vaddr, vaddr+memsz) fresh (mmap2 MAP_FIXED, zero-filled, RWX) over
| whatever the loader had there and copies filesz bytes from src.  Then it puts
| back the stack pointer the process started with -- the loader's boot stub
| saved it as the collector's stack_base, #x10000058, which is read FIRST
| because the image's low block lands on top of it -- and jumps to the entry.
|
| Assemble: m68k-linux-gnu-as -m68040 -o c.o 68k-floppy-copier.s
|           m68k-linux-gnu-objcopy -O binary c.o c.bin

        .text
copier:
        move.l  0x10000058, %d7         | the process's initial SP (&argc)
        lea     table(%pc), %a3
        move.l  (%a3)+, %d6             | segment count
seg:
        tst.l   %d6
        beq     go
        move.l  (%a3)+, %d1             | vaddr (page-aligned)
        move.l  (%a3)+, %a4             | src
        move.l  (%a3)+, -(%sp)          | filesz
        move.l  (%a3)+, %d2             | memsz, rounded up to a page
        add.l   #4095, %d2
        and.l   #-4096, %d2
        move.l  %d1, %a2                | dst
        moveq   #7, %d3                 | PROT_READ|WRITE|EXEC
        moveq   #0x32, %d4              | MAP_FIXED|MAP_ANONYMOUS|MAP_PRIVATE
        moveq   #-1, %d5                | fd
        sub.l   %a0, %a0                | pgoff
        move.l  #192, %d0               | mmap2
        trap    #0
        move.l  (%sp)+, %d5
copy:
        subq.l  #1, %d5
        bmi     next
        move.b  (%a4)+, (%a2)+
        bra     copy
next:
        subq.l  #1, %d6
        bra     seg
go:
        move.l  (%a3), %a0              | entry
        move.l  %d7, %sp
        sub.l   %a1, %a1
        moveq   #0, %d0
        jmp     (%a0)
        .align  2
table:
