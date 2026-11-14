/* test/w32-tbcount.c -- executed guest instructions from a qemu-user log.
 *
 *   qemu-riscv32-static -d in_asm,exec,nochain -D /dev/stderr IMAGE ARGS \
 *       2>&1 >/dev/null | w32-tbcount
 *
 * qemu 7.2 here has no TCG plugins, and -singlestep logs one line per guest
 * instruction (a CLI boot is billions).  Instead: in_asm prints each
 * translation block once ("IN:" then one "0x<pc>:" line per instruction),
 * exec prints one "Trace ..." line per block EXECUTED, naming its guest pc
 * ([cs_base/pc/flags/cflags]), and nochain makes every execution print.
 * Sum, over executions, the instruction count of the block's latest
 * translation.  A block left early (a fault, a syscall mid-block) still
 * counts in full; that error is per such exit, not per instruction.
 * Prints: instructions executed, blocks executed, executions of a pc never
 * seen translated (should be 0).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define BITS 22
#define SIZE (1u << BITS)
static uint32_t keys[SIZE];
static uint32_t vals[SIZE];
static unsigned char used[SIZE];

static uint32_t *slot(uint32_t pc) {
  uint32_t h = (pc * 2654435761u) >> (32 - BITS);
  while (used[h] && keys[h] != pc) h = (h + 1) & (SIZE - 1);
  if (!used[h]) { used[h] = 1; keys[h] = pc; vals[h] = 0; }
  return &vals[h];
}

int main(void) {
  char *line = NULL; size_t cap = 0; ssize_t n;
  int in_block = 0; uint32_t block_pc = 0, block_n = 0;
  unsigned long long insns = 0, execs = 0, unknown = 0;
  while ((n = getline(&line, &cap, stdin)) > 0) {
    if (line[0] == 'T' && strncmp(line, "Trace ", 6) == 0) {
      if (in_block) { *slot(block_pc) = block_n; in_block = 0; }
      char *p = strchr(line, '['); if (!p) continue;
      p = strchr(p, '/'); if (!p) continue;          /* skip cs_base */
      uint32_t pc = (uint32_t)strtoul(p + 1, NULL, 16);
      uint32_t c = *slot(pc);
      if (c == 0) unknown++;
      insns += c; execs++;
    } else if (strncmp(line, "IN:", 3) == 0) {
      if (in_block) *slot(block_pc) = block_n;
      in_block = 1; block_n = 0;
    } else if (in_block && line[0] == '0' && line[1] == 'x') {
      uint32_t pc = (uint32_t)strtoul(line, NULL, 16);
      if (block_n == 0) block_pc = pc;
      block_n++;
    } else if (in_block && (line[0] == '\n' || line[0] == '-')) {
      if (block_n) *slot(block_pc) = block_n;
      in_block = 0;
    }
  }
  printf("insns=%llu blocks=%llu unknown=%llu\n", insns, execs, unknown);
  return 0;
}
