#!/usr/bin/env python3
"""check-macho.py -- an independent checker for an arm64 macOS executable.

    python3 -I host/macos/check-macho.py FILE [--elf IMAGE] [--modus] [--quiet]

Written apart from mvm/macho-writer.lisp (which has its own VERIFY-MACHO) so that a mistake
in one is not repeated in the other.  It parses FILE from scratch and checks what the
kernel and dyld check before they run an executable, as their sources state them:

  xnu   bsd/kern/mach_loader.c   MH_DYLDLINK required on arm64 (static executables refused),
                                 MH_PIE, a hard 4 GB page zero, a code signature
  dyld  mach_o/UnsafeHeader.cpp  load-command sizes, __TEXT at file offset 0 holding the
                                 load commands, segment permissions (__TEXT r-x, __DATA and
                                 __DATA_CONST rw-, SG_READ_ONLY on __DATA_CONST, __LINKEDIT
                                 r--), sections inside their segments, no overlaps, segments
                                 in order, one LC_UUID, LC_MAIN, LC_LOAD_DYLIB
  dyld  mach_o/Image.cpp         LINKEDIT blobs aligned, inside __LINKEDIT, not overlapping;
                                 symbol counts agree with LC_DYSYMTAB; no relocations beside
                                 LC_DYLD_INFO_ONLY
  dyld  MachOAnalyzer.cpp        every bind in range of a writable, non-executable segment,
                                 with a valid library ordinal; indirect symbols in range
  xnu   osfmk/kern/cs_blobs.h    the embedded signature: SuperBlob, CodeDirectory fields, and
                                 every page hash recomputed (hashlib.sha256)

On top of that it walks the host code in __text: every ADRP+ADD / ADRP+LDR pair and every
B/BL/B.cond/CBZ must land inside a mapped section of the right kind (LDRs from __got must
read a bound slot).  With --elf it checks that __TEXT,__modus is that image, 16 KB aligned.
--modus adds the expectations specific to mvm/build-macos-cli.lisp's output (the exact
signature flags and fields Apple's ld writes).  Exit status 0 = every check passed.
"""
import hashlib
import struct
import sys

PAGE = 0x4000
fails = []
notes = []


def fail(msg):
    fails.append(msg)


def u32(d, o): return struct.unpack_from('<I', d, o)[0]
def u64(d, o): return struct.unpack_from('<Q', d, o)[0]
def be32(d, o): return struct.unpack_from('>I', d, o)[0]
def be64(d, o): return struct.unpack_from('>Q', d, o)[0]
def cstr(d, o, n=None):
    e = d.index(b'\0', o) if n is None else o + n
    return d[o:e].split(b'\0')[0].decode('latin-1')


def uleb(d, i):
    v = s = 0
    while True:
        b = d[i]; i += 1
        v |= (b & 0x7f) << s; s += 7
        if not b & 0x80: return v, i


def sleb(d, i):
    v = s = 0
    while True:
        b = d[i]; i += 1
        v |= (b & 0x7f) << s; s += 7
        if not b & 0x80:
            if b & 0x40: v -= 1 << s
            return v, i


def main(argv):
    path = argv[1]
    elf = None
    modus = '--modus' in argv
    quiet = '--quiet' in argv
    if '--elf' in argv:
        elf = open(argv[argv.index('--elf') + 1], 'rb').read()
    d = open(path, 'rb').read()

    magic, cpu, sub, ftype, ncmds, sizeofcmds, flags, _ = struct.unpack_from('<IiiIIIII', d, 0)
    if magic != 0xfeedfacf: fail('not MH_MAGIC_64'); return report(d)
    if cpu != 0x0100000c: fail('cputype %#x is not CPU_TYPE_ARM64' % cpu)
    if sub & 0xffffff not in (0, 1): fail('cpusubtype %#x is not ARM64_ALL/V8' % sub)
    if ftype != 2: fail('filetype %d is not MH_EXECUTE' % ftype)
    if not flags & 0x4: fail('MH_DYLDLINK missing: arm64 kernels refuse static executables')
    if not flags & 0x200000: fail('MH_PIE missing: required for arm64')
    if modus and flags != 0x200085: fail('flags %#x, expected NOUNDEFS|DYLDLINK|TWOLEVEL|PIE' % flags)
    if 32 + sizeofcmds > len(d): fail('load commands run past the file')

    segs, cmds = [], []
    o, end = 32, 32 + sizeofcmds
    for i in range(ncmds):
        cmd, cs = struct.unpack_from('<II', d, o)
        if cs < 8 or cs % 8 or o + cs > end:
            fail('load command %d (%#x) has bad size %d' % (i, cmd, cs)); return report(d)
        cmds.append((cmd, o, cs))
        if cmd == 0x19:
            name = cstr(d, o + 8, 16)
            vm, vms, fo, fs, maxp, initp, ns, fl = struct.unpack_from('<QQQQiiII', d, o + 24)
            if cs != 72 + 80 * ns: fail('segment %s: cmdsize does not match its sections' % name)
            sects = []
            for k in range(ns):
                so = o + 72 + 80 * k
                sn, sg = cstr(d, so, 16), cstr(d, so + 16, 16)
                a, sz, off, al, ro, nr, sf, r1, r2, r3 = struct.unpack_from('<QQIIIIIIII', d, so + 32)
                sects.append(dict(name=sn, seg=sg, addr=a, size=sz, off=off, align=al, reloff=ro,
                                  nreloc=nr, flags=sf, r1=r1, r2=r2))
            segs.append(dict(name=name, vm=vm, vmsize=vms, off=fo, fsize=fs, maxp=maxp, initp=initp,
                             flags=fl, sects=sects))
        o += cs
    if o != end: fail('load commands end at %d, sizeofcmds says %d' % (o, end))
    have = [c for c, _, _ in cmds]
    for need, name in ((0x19, 'LC_SEGMENT_64'), (0xe, 'LC_LOAD_DYLINKER'), (0xc, 'LC_LOAD_DYLIB'),
                       (0x80000028, 'LC_MAIN'), (0x1d, 'LC_CODE_SIGNATURE'), (0x1b, 'LC_UUID'),
                       (0x2, 'LC_SYMTAB'), (0xb, 'LC_DYSYMTAB')):
        if need not in have: fail('missing %s' % name)
    if have.count(0x1b) > 1: fail('more than one LC_UUID')
    if have.count(0x80000028) != 1: fail('need exactly one LC_MAIN')
    if 0x5 in have: fail('LC_UNIXTHREAD present')
    if 0x80000022 in have and 0x80000034 in have: fail('both LC_DYLD_INFO_ONLY and LC_DYLD_CHAINED_FIXUPS')
    for c, co, cs in cmds:
        if c & 0x80000000 and c not in (0x80000022, 0x80000028, 0x80000034, 0x80000033, 0x8000001c, 0x80000018, 0x8000001f):
            fail('unknown required load command %#x' % c)

    seg = {s['name']: s for s in segs}
    # segments
    if 'segname' and len(set(s['name'] for s in segs)) != len(segs): fail('duplicate segment names')
    pz = seg.get('__PAGEZERO')
    if not pz or pz['vm'] != 0 or pz['vmsize'] < 0x100000000 or pz['initp'] or pz['maxp'] or pz['fsize']:
        fail('no hard 4 GB __PAGEZERO (xnu: required for 64-bit arm64)')
    text = seg.get('__TEXT')
    if not text: fail('no __TEXT'); return report(d)
    if text['off'] != 0: fail('__TEXT does not start at file offset 0')
    if text['fsize'] < 32 + sizeofcmds: fail('load commands do not fit in __TEXT')
    for s in segs:
        n = s['name']
        if s['initp'] & ~7: fail('%s: bad protection bits' % n)
        unmapped = s['vmsize'] == 0 and s['initp'] == 0     # dyld: e.g. a __DWARF segment
        if s['fsize'] > s['vmsize'] and not unmapped: fail('%s: filesize > vmsize' % n)
        if s['off'] + s['fsize'] > len(d): fail('%s: runs past the end of the file' % n)
        if n != '__PAGEZERO':
            if s['vm'] % PAGE or s['off'] % PAGE: fail('%s: not 16 KB aligned (vm %#x, file %#x)' % (n, s['vm'], s['off']))
            if modus and s['vmsize'] % PAGE: fail('%s: vmsize %#x not a 16 KB multiple' % (n, s['vmsize']))
        want = {'__TEXT': 5, '__DATA': 3, '__DATA_CONST': 3, '__LINKEDIT': 1}.get(n)
        if want is not None and s['initp'] != want: fail('%s: initprot %d, dyld requires %d' % (n, s['initp'], want))
        if n == '__DATA_CONST' and not s['flags'] & 0x10: fail('__DATA_CONST lacks SG_READ_ONLY')
        if s['vm'] + s['vmsize'] > 1 << 64: fail('%s: vm range wraps' % n)
        for sc in ([] if unmapped else s['sects']):
            if sc['seg'] != n: fail('section %s names segment %s inside %s' % (sc['name'], sc['seg'], n))
            if sc['addr'] < s['vm'] or sc['addr'] + sc['size'] > s['vm'] + s['vmsize']:
                fail('section %s outside segment %s' % (sc['name'], n))
            if sc['addr'] % (1 << sc['align']): fail('section %s misaligned for 2^%d' % (sc['name'], sc['align']))
            zerofill = (sc['flags'] & 0xff) in (1, 0xc, 0x12)
            if not zerofill and sc['off'] - s['off'] != sc['addr'] - s['vm']:
                fail('section %s: file and vm offsets disagree' % sc['name'])
            if sc['nreloc']: fail('section %s carries relocations' % sc['name'])
    for a in segs:
        for b in segs:
            if a is b: continue
            if a['vmsize'] and b['vmsize'] and a['vm'] < b['vm'] + b['vmsize'] and b['vm'] < a['vm'] + a['vmsize']:
                fail('vm ranges of %s and %s overlap' % (a['name'], b['name']))
            if a['fsize'] and b['fsize'] and a['off'] < b['off'] + b['fsize'] and b['off'] < a['off'] + a['fsize']:
                fail('file ranges of %s and %s overlap' % (a['name'], b['name']))
    for i in range(1, len(segs)):
        if segs[i]['vm'] < segs[i - 1]['vm']: fail('segment %s out of vm order' % segs[i]['name'])
        if segs[i]['fsize'] and segs[i]['off'] < segs[i - 1]['off']: fail('segment %s out of file order' % segs[i]['name'])
    le = seg.get('__LINKEDIT')
    if not le: fail('no __LINKEDIT'); return report(d)
    if le is not segs[-1]: fail('__LINKEDIT is not the last segment')
    if le['off'] + le['fsize'] != len(d): fail('__LINKEDIT does not end the file')

    def seg_of_index(i):
        return segs[i] if 0 <= i < len(segs) else None

    def section_at(addr):
        for s in segs:
            for sc in s['sects']:
                if sc['addr'] <= addr < sc['addr'] + sc['size']:
                    return s, sc
        return None, None

    # load commands' contents
    dylibs = []
    main_off = None
    for c, co, cs in cmds:
        if c == 0xe:
            if cstr(d, co + u32(d, co + 8)) != '/usr/lib/dyld': fail('LC_LOAD_DYLINKER is not /usr/lib/dyld')
        elif c == 0xc:
            dylibs.append(cstr(d, co + u32(d, co + 8)))
        elif c == 0x80000028:
            main_off = u64(d, co + 8)
        elif c == 0x32:
            plat, minos, sdk, ntools = struct.unpack_from('<IIII', d, co + 8)
            if plat != 1: fail('LC_BUILD_VERSION platform %d is not macOS' % plat)
            notes.append('build version: macOS %d.%d (sdk %d.%d)' % (minos >> 16, (minos >> 8) & 0xff, sdk >> 16, (sdk >> 8) & 0xff))
        elif c == 0x1b:
            notes.append('uuid %s' % d[co + 8:co + 24].hex())
    if '/usr/lib/libSystem.B.dylib' not in dylibs: fail('does not load libSystem.B.dylib')
    if main_off is not None:
        s, sc = section_at(text['vm'] + main_off)
        if s is not text or not (sc and sc['flags'] & 0x80000000):
            fail('LC_MAIN entry %#x is not in __TEXT code' % main_off)

    # LINKEDIT blobs
    blobs = []
    symtab = dysym = info = sig = None
    for c, co, cs in cmds:
        if c == 0x2:
            symtab = struct.unpack_from('<IIII', d, co + 8)
            blobs += [('symbol table', 8, symtab[0], symtab[1] * 16), ('string table', 1, symtab[2], symtab[3])]
        elif c == 0xb:
            dysym = struct.unpack_from('<18I', d, co + 8)
            if dysym[16 - 2] and False: pass
            blobs.append(('indirect symbols', 4, dysym[12], dysym[13] * 4))
            if dysym[15] or dysym[17]: fail('LC_DYSYMTAB has relocations (not allowed with LC_DYLD_INFO_ONLY)')
        elif c in (0x22, 0x80000022):
            info = struct.unpack_from('<10I', d, co + 8)
            for nm, k in (('rebase', 0), ('bind', 2), ('weak bind', 4), ('lazy bind', 6), ('export', 8)):
                blobs.append((nm, 8, info[k], info[k + 1]))
        elif c == 0x1d:
            sig = struct.unpack_from('<II', d, co + 8)
            blobs.append(('code signature', 8, sig[0], sig[1]))
    prev = le['off']
    for nm, al, off, size in sorted((b for b in blobs if b[3]), key=lambda b: b[2]):
        if off < prev: fail('LINKEDIT %s overlaps the previous blob' % nm)
        if off + size > le['off'] + le['fsize']: fail('LINKEDIT %s runs past __LINKEDIT' % nm)
        if off % al: fail('LINKEDIT %s misaligned (%#x)' % (nm, off))
        prev = off + size

    # symbols
    syms = []
    if symtab:
        symoff, nsyms, stroff, strsize = symtab
        for i in range(nsyms):
            strx, typ, sect, desc, val = struct.unpack_from('<IBBHQ', d, symoff + 16 * i)
            if strx >= strsize: fail('symbol %d name out of range' % i)
            syms.append((cstr(d, stroff + strx), typ, sect, desc, val))
        if dysym:
            il, nl, ie, ne, iu, nu = dysym[:6]
            if il != 0 or ie != nl or iu != ie + ne: fail('LC_DYSYMTAB symbol groups out of order')
            if iu + nu != nsyms: fail('LC_DYSYMTAB counts %d symbols, LC_SYMTAB %d' % (iu + nu, nsyms))
            for i in range(iu, iu + nu):
                n, typ, sect, desc, val = syms[i]
                if typ & 0x0e != 0 or not typ & 1: fail('undefined symbol %s is not N_UNDF|N_EXT' % n)
                if flags & 0x80 and not 1 <= (desc >> 8) <= len(dylibs): fail('%s: bad library ordinal' % n)

    # binds
    bound = {}
    if info:
        rebase_off, rebase_size = info[0], info[1]
        if rebase_size:
            notes.append('rebase opcodes present (%d bytes): not interpreted' % rebase_size)
        for kind, off, size in (('bind', info[2], info[3]), ('lazy', info[6], info[7])):
            i, end = off, off + size
            ordn = sym = None; typ = 1; segi = None; segoff = 0; addend = 0
            while i < end:
                b = d[i]; i += 1
                op, imm = b & 0xf0, b & 0x0f
                if op == 0x00:
                    if kind == 'bind': break
                    continue
                elif op == 0x10: ordn = imm
                elif op == 0x20: ordn, i = uleb(d, i)
                elif op == 0x30: ordn = -imm if imm else 0
                elif op == 0x40:
                    e = d.index(b'\0', i); sym = d[i:e].decode(); i = e + 1
                elif op == 0x50: typ = imm
                elif op == 0x60: addend, i = sleb(d, i)
                elif op == 0x70: segi = imm; segoff, i = uleb(d, i)
                elif op == 0x80:
                    v, i = uleb(d, i); segoff = (segoff + v) & (2 ** 64 - 1)
                elif op in (0x90, 0xa0, 0xb0, 0xc0):
                    count, skip = 1, 0
                    if op == 0xc0:
                        count, i = uleb(d, i); skip, i = uleb(d, i)
                    for _ in range(count):
                        s = seg_of_index(segi) if segi is not None else None
                        if s is None: fail('%s: no segment set' % kind); break
                        if sym is None or ordn is None: fail('%s: symbol or ordinal missing' % kind); break
                        if ordn > len(dylibs): fail('%s %s: ordinal %d > %d dylibs' % (kind, sym, ordn, len(dylibs)))
                        if typ != 1: fail('%s %s: type %d is not BIND_TYPE_POINTER' % (kind, sym, typ))
                        if segoff + 8 > s['vmsize']: fail('%s %s: offset past %s' % (kind, sym, s['name']))
                        if not s['initp'] & 2 or s['initp'] & 4: fail('%s %s: in a non-writable or executable segment' % (kind, sym))
                        addr = s['vm'] + segoff
                        if addr in bound: fail('slot %#x bound twice' % addr)
                        bound[addr] = sym
                        segoff += 8
                        if op == 0xa0:
                            v, i = uleb(d, i); segoff += v
                        elif op == 0xb0:
                            segoff += imm * 8
                        elif op == 0xc0:
                            segoff += skip
                else:
                    fail('%s: unknown opcode %#x' % (kind, b)); break

    # indirect symbols vs __got
    got = None
    for s in segs:
        for sc in s['sects']:
            if sc['flags'] & 0xff == 6:
                got = sc
                n = sc['size'] // 8
                if dysym and sc['r1'] + n > dysym[13]: fail('%s overflows the indirect symbol table' % sc['name'])
                for k in range(n):
                    slot = sc['addr'] + 8 * k
                    idx = u32(d, dysym[12] + 4 * (sc['r1'] + k)) if dysym else None
                    if idx is None or idx >= len(syms): fail('%s[%d]: bad indirect symbol' % (sc['name'], k)); continue
                    if bound.get(slot) != syms[idx][0]:
                        fail('%s[%d]: indirect symbol %s, bound to %s' % (sc['name'], k, syms[idx][0], bound.get(slot)))

    # code signature
    cdhash = None
    if sig:
        so, ss = sig
        if so % 16: fail('code signature not 16-byte aligned')
        if so + ss != len(d): fail('code signature does not end the file')
        if be32(d, so) != 0xfade0cc0: fail('no embedded-signature SuperBlob')
        sb_len, count = be32(d, so + 4), be32(d, so + 8)
        if sb_len > ss: fail('SuperBlob longer than LC_CODE_SIGNATURE says')
        cd = None
        for k in range(count):
            typ, off = be32(d, so + 12 + 8 * k), be32(d, so + 16 + 8 * k)
            if typ == 0: cd = so + off
        if cd is None or be32(d, cd) != 0xfade0c02: fail('no CodeDirectory'); return report(d)
        cdlen, ver, cflags, hoff, ioff, nspecial, ncode, limit = struct.unpack_from('>8I', d, cd + 4)
        hsize, htype, platform, pbits = d[cd + 36], d[cd + 37], d[cd + 38], d[cd + 39]
        ident = cstr(d, cd + ioff)
        if htype != 2 or hsize != 32: fail('hash is not SHA-256')
        if limit != so: fail('codeLimit %#x is not the signature offset %#x' % (limit, so))
        psize = 1 << pbits
        if ncode != -(-limit // psize): fail('nCodeSlots %d for codeLimit %#x' % (ncode, limit))
        bad = 0
        for k in range(ncode):
            h = hashlib.sha256(d[k * psize:min(limit, (k + 1) * psize)]).digest()
            if h != d[cd + hoff + 32 * k:cd + hoff + 32 * (k + 1)]: bad += 1
        if bad: fail('%d of %d page hashes do not match' % (bad, ncode))
        cdhash = hashlib.sha256(d[cd:cd + cdlen]).hexdigest()[:40]
        if ver >= 0x20400:
            base, lim, xf = struct.unpack_from('>QQQ', d, cd + 64)
            if base != text['off'] or lim != text['fsize']: fail('execSeg %#x+%#x is not __TEXT' % (base, lim))
            if not xf & 1: fail('execSegFlags lacks CS_EXECSEG_MAIN_BINARY')
        if modus:
            if ver != 0x20400: fail('CodeDirectory version %#x, Apple ld writes 0x20400' % ver)
            if cflags != 0x20002: fail('CodeDirectory flags %#x, Apple ld writes CS_ADHOC|CS_LINKER_SIGNED' % cflags)
            if nspecial: fail('special slots present; ld writes none')
            if pbits != 12: fail('page size 2^%d, ld uses 4 KB' % pbits)
            if hoff != 88 + len(ident) + 1 or ioff != 88: fail('CodeDirectory offsets unexpected')
            if count != 1: fail('SuperBlob has %d blobs; ld writes one' % count)
            if ss % 8: fail('signature size not 8-byte padded')
        notes.append('signature: identifier %r, %d pages of %d bytes, flags %#x, cdhash %s' % (ident, ncode, psize, cflags, cdhash))

    # the image
    tsect = {sc['name']: sc for sc in text['sects']}
    if modus or elf is not None:
        m = tsect.get('__modus')
        if not m: fail('no __TEXT,__modus')
        else:
            if m['addr'] % PAGE or m['off'] % PAGE: fail('__modus not 16 KB aligned')
            if d[m['off']:m['off'] + 4] != b'\x7fELF': fail('__modus does not start with an ELF header')
            if elf is not None and d[m['off']:m['off'] + m['size']] != elf: fail('__modus is not the given image')
            # the shim remaps round_up(p_filesz, 16K) from the section start: it must stay in __TEXT
            phoff = u64(d, m['off'] + 32); filesz = u64(d, m['off'] + phoff + 32)
            span = (filesz + PAGE - 1) & ~(PAGE - 1)
            if m['addr'] + span > text['vm'] + text['vmsize']: fail('the remapped span runs past __TEXT')

    # the host code: branch and address-forming targets
    code = tsect.get('__text')
    if code and modus:
        check_code(d, code, segs, bound, got)
    return report(d, quiet)


def check_code(d, code, segs, bound, got):
    base, off, n = code['addr'], code['off'], code['size'] // 4
    text_lo, text_hi = base, base + code['size']
    nadrp = nbr = nlocal = 0
    def mapped(addr):
        for s in segs:
            if s['name'] != '__PAGEZERO' and s['vm'] <= addr < s['vm'] + s['vmsize']: return s
        return None
    for i in range(n):
        pc = base + 4 * i
        w = u32(d, off + 4 * i)
        if w & 0x9f000000 == 0x90000000:                      # ADRP
            nadrp += 1
            imm = ((w >> 29) & 3) | (((w >> 5) & 0x7ffff) << 2)
            if imm & (1 << 20): imm -= 1 << 21
            page = (pc & ~0xfff) + (imm << 12)
            rd = w & 31
            nxt = u32(d, off + 4 * (i + 1))
            if nxt & 0xffc00000 == 0x91000000 and (nxt >> 5) & 31 == rd:        # ADD imm
                tgt = page + ((nxt >> 10) & 0xfff)
                if not mapped(tgt): fail('ADRP+ADD at %#x -> %#x unmapped' % (pc, tgt))
            elif nxt & 0xffc00000 == 0xf9400000 and (nxt >> 5) & 31 == rd:      # LDR X imm
                tgt = page + ((nxt >> 10) & 0xfff) * 8
                if got and got['addr'] <= tgt < got['addr'] + got['size']:
                    if tgt not in bound: fail('LDR at %#x reads unbound GOT slot %#x' % (pc, tgt))
                elif not mapped(tgt): fail('ADRP+LDR at %#x -> %#x unmapped' % (pc, tgt))
            else:
                fail('ADRP at %#x not followed by ADD/LDR of its register' % pc)
        elif w & 0x7c000000 == 0x14000000:                     # B / BL
            nbr += 1
            imm = w & 0x3ffffff
            if imm & (1 << 25): imm -= 1 << 26
            tgt = pc + 4 * imm
            if not text_lo <= tgt < text_hi: fail('branch at %#x leaves __text (-> %#x)' % (pc, tgt))
        elif w & 0xff000010 == 0x54000000 or w & 0x7e000000 == 0x34000000:   # B.cond, CBZ/CBNZ
            nlocal += 1
            imm = (w >> 5) & 0x7ffff
            if imm & (1 << 18): imm -= 1 << 19
            tgt = pc + 4 * imm
            if not text_lo <= tgt < text_hi: fail('conditional branch at %#x leaves __text' % pc)
    notes.append('host code: %d instructions, %d ADRP pairs, %d B/BL, %d conditional branches checked'
                 % (n, nadrp, nbr, nlocal))


def report(d, quiet=False):
    for n in notes:
        if not quiet: print('  ' + n)
    for f in fails:
        print('FAIL: ' + f)
    print('%s: %d checks failed' % ('FAIL' if fails else 'PASS', len(fails)))
    return 1 if fails else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
