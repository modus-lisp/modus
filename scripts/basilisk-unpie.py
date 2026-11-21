#!/usr/bin/env python3
"""basilisk-unpie.py IN OUT BASE -- relink an x86-64 PIE as an ET_EXEC at BASE.

    scripts/basilisk-unpie.py /usr/bin/BasiliskII-jit BasiliskII-jit-lo 0x10000000
    MALLOC_MMAP_MAX_=0 ./BasiliskII-jit-lo --jit true --jitcachesize 16384 ...


For Debian's BasiliskII-jit, whose JIT bakes 32-bit absolute addresses of the
emulator's globals and heap: a PIE loads above 4 GB and they truncate.  At a
low BASE the executable, and the brk heap the kernel puts after it, are both
below 2 GB; MALLOC_MMAP_MAX_=0 keeps every malloc in that heap.  Measured
before: SIGSEGV at 0x558b9b50 (= 0x5555558b9b50 truncated) before the ROM
loads, with System 7.5.3 as with modus.  After: both boot.  Rewritten: e_type/e_entry, program and section headers, dynamic
pointers, every relocation offset, RELATIVE addends, defined dynsyms, the PLT
GOT slots (lazy binding adds l_addr, which is 0 for an EXEC), DF_1_PIE."""
import struct, sys
from elftools.elf.elffile import ELFFile
from elftools.elf.dynamic import DynamicSection
from elftools.elf.relocation import RelocationSection
from elftools.elf.sections import SymbolTableSection

src, dst, B = sys.argv[1], sys.argv[2], int(sys.argv[3], 0)
data = bytearray(open(src, 'rb').read())
f = ELFFile(open(src, 'rb'))
assert f['e_type'] == 'ET_DYN' and f['e_machine'] == 'EM_X86_64'
u64 = lambda off: struct.unpack_from('<Q', data, off)[0]
def add64(off, d=B): struct.pack_into('<Q', data, off, u64(off) + d)

def v2o(va):                                  # link-time vaddr -> file offset
    for seg in f.iter_segments():
        if seg['p_type'] == 'PT_LOAD' and seg['p_vaddr'] <= va < seg['p_vaddr'] + seg['p_filesz']:
            return va - seg['p_vaddr'] + seg['p_offset']
    return None

struct.pack_into('<H', data, 16, 2)           # ET_EXEC
add64(24)                                     # e_entry
ph, phn, phs = f['e_phoff'], f['e_phnum'], f['e_phentsize']
for i in range(phn):
    o = ph + i * phs
    if struct.unpack_from('<I', data, o)[0] != 0x6474e551:   # not PT_GNU_STACK
        add64(o + 16); add64(o + 24)          # p_vaddr, p_paddr
sh, shn, shs = f['e_shoff'], f['e_shnum'], f['e_shentsize']
for i in range(shn):
    o = sh + i * shs
    if u64(o + 16): add64(o + 16)             # sh_addr

PTR_TAGS = {'DT_PLTGOT', 'DT_HASH', 'DT_GNU_HASH', 'DT_STRTAB', 'DT_SYMTAB', 'DT_RELA',
            'DT_REL', 'DT_JMPREL', 'DT_INIT', 'DT_FINI', 'DT_INIT_ARRAY', 'DT_FINI_ARRAY',
            'DT_PREINIT_ARRAY', 'DT_VERSYM', 'DT_VERNEED', 'DT_VERDEF'}
counts = {}
for sec in f.iter_sections():
    if isinstance(sec, DynamicSection):
        for i, tag in enumerate(sec.iter_tags()):
            o = sec['sh_offset'] + i * 16
            if tag.entry.d_tag in PTR_TAGS:
                add64(o + 8)
            elif tag.entry.d_tag == 'DT_FLAGS_1':
                struct.pack_into('<Q', data, o + 8, u64(o + 8) & ~0x08000000)  # DF_1_PIE
    elif isinstance(sec, RelocationSection):
        assert sec.is_RELA()
        for i, r in enumerate(sec.iter_relocations()):
            o = sec['sh_offset'] + i * 24
            t = r['r_info_type']
            counts[t] = counts.get(t, 0) + 1
            if t == 8:                        # RELATIVE: addend is an address
                add64(o + 16)
            elif t == 7:                      # JUMP_SLOT: its GOT word holds a PLT address
                fo = v2o(r['r_offset'])
                if fo is not None: add64(fo)
            elif t not in (1, 5, 6):          # 64, COPY, GLOB_DAT: symbol-based
                sys.exit('basilisk-unpie: relocation type %d' % t)
            add64(o)                          # r_offset
    elif isinstance(sec, SymbolTableSection):
        for i, s in enumerate(sec.iter_symbols()):
            if s['st_shndx'] not in ('SHN_UNDEF', 'SHN_ABS') and s['st_value']:
                add64(sec['sh_offset'] + i * 24 + 8)
gp = f.get_section_by_name('.got.plt')       # GOT[0] = _DYNAMIC's link address
if gp is not None and u64(gp['sh_offset']): add64(gp['sh_offset'])
open(dst, 'wb').write(data)
print('basilisk-unpie: %s -> %s at %#x; relocations %s' % (src, dst, B, counts))
