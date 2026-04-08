# Memory, GC, and Foreign Function Interface

## Memory Layout

### Tagged Values (64-bit)

All values are tagged 64-bit words:

```
xxx0  Fixnum     63-bit signed integer (value << 1)
0001  Cons        Pointer to car/cdr pair, 16-byte aligned
1001  Object      Pointer to header + data (general heap object)
0101  Immediate   Characters, nil, booleans (no heap allocation)
1111  Forward     GC forwarding pointer (only during collection)
```

Fixnum shift is 1, so arithmetic can often use tagged values directly (addition works, multiplication needs shift correction).

### Object Header

All heap objects (tag `1001`) have an 8-byte header at the untagged address:

```
[subtag:8][unused:8][element-count:48]
```

The subtag determines the object type and tells the GC how to scan it.

### Subtag Ranges

The high nibble of the subtag determines GC scanning behavior:

```
00-0F  Node vectors     All elements are tagged pointers → GC scans each one
10-1F  Byte vectors     Raw bytes, no pointers → GC copies but doesn't scan
20-2F  Bit vectors      Raw bits → GC copies but doesn't scan
30-3F  Numeric vectors  Raw numbers → GC copies but doesn't scan
40-4F  Struct/instance  Mixed → GC consults struct definition
50-5F  Special          Symbols, functions, closures → type-specific scanning
60-6F  MVM objects      Bytecode, modules → no scan (code is not relocatable)
```

Assigned subtags:

| Subtag | Type | Elements | Scanned? |
|--------|------|----------|----------|
| `#x01` | simple-vector | tagged pointers | Yes |
| `#x10` | string | UTF-8 bytes | No |
| `#x11` | u8-vector | unsigned bytes | No |
| `#x14` | u64-vector | raw 64-bit words | No |
| `#x15` | s64-vector | signed 64-bit words | No |
| `#x16` | **SAP** | 1 raw address word | **No** |
| `#x30` | bignum | raw limbs | No |
| `#x32` | array | tagged elements | Yes |
| `#x40` | struct | mixed | Per-definition |
| `#x41` | hash-table | mixed | Per-definition |
| `#x50` | symbol | name-hash + value | Yes |
| `#x51` | function | code pointer | Partially |
| `#x52` | closure | code + captured vars | Yes |
| `#x60` | mvm-bytecode | raw bytes | No |
| `#x61` | mvm-module | raw bytes | No |

## Allocation

### Bump Allocator

All allocation uses a bump pointer (R12 on x64, R12 on AArch64) with a limit (R14):

```
R12 (alloc pointer) → [next free byte]
R14 (alloc limit)   → [end of heap region]
```

To allocate N bytes:
1. Write header at `[R12]`
2. Tag result: `R12 | 0x09` (object tag)
3. Advance: `R12 += align16(N)`

No locks, no free lists — just a pointer bump. When R12 >= R14, trigger GC.

### Inline Allocation (Compiler-Generated)

The compiler emits inline allocation for common patterns:

- `ALLOC-CONS`: 3 instructions (lea + or + add R12, 16)
- `ALLOC-OBJ`: Write header, tag, advance R12 (5-6 instructions)
- `ALLOC-ARRAY`: Same but element count is dynamic

### Runtime Allocation

`try-alloc-obj(len, subtag)` in Lisp — checks bounds, writes header, zero-fills, advances pointer. Returns tagged object or 0 (OOM). `make-array(len)` wraps this with subtag `#x32`.

## Garbage Collection

### Cheney Copying Collector

When R12 >= R14, the GC copies live objects from the current semispace to the other:

1. **Root scan**: Registers (V0-V8, alloc ptr, stack), stack frames, global variables
2. **Object copy**: For each live object, copy to to-space, leave forwarding pointer in from-space
3. **Pointer update**: Scan copied objects, update any pointers to from-space addresses
4. **Flip**: Swap semispaces, reset R12/R14

### Subtag-Driven Scanning

The GC uses the subtag to determine how to scan each copied object:

- **Subtags 0x00-0x0F** (node vectors): Scan every 8-byte element as a potential pointer
- **Subtags 0x10-0x3F** (byte/bit/numeric): Copy the data blob, don't scan elements
- **Subtags 0x40-0x5F** (structs, symbols, closures): Type-specific scanning
- **Subtag 0x16 (SAP)**: Copy the header + raw address word, **never follow the raw address**

### GC Check Opcode

The compiler inserts `GC-CHECK` after allocation-heavy code:

```asm
cmp R12, R14          ; alloc vs limit
jl skip               ; still have space
call gc_routine       ; trigger collection
skip:
```

### Write Barrier (Stub)

`WRITE-BARRIER` opcode exists but is currently NOP. When generational GC is implemented, it will mark card table entries dirty for old→young pointer detection.

## Actor Heaps

Each actor gets an independent 4MB heap:

```
Actor 1: actor-heap-base + 0x000000 → +0x400000
Actor 2: actor-heap-base + 0x400000 → +0x800000
Actor N: actor-heap-base + (N-1)*4MB
```

Context switch saves/restores R12 (alloc ptr) and R14 (limit) per actor. Each actor's GC runs independently — no stop-the-world collection.

## Foreign Function Interface (SAP)

### System Area Pointer (SAP)

A SAP wraps a raw machine address in a proper Lisp object. It lives on the GC heap like any other object, but the GC never follows the raw address inside it.

```
SAP object (subtag #x16, 16 bytes total):
  [0x00] header: (1 << 8) | 0x16     (element-count=1, subtag=SAP)
  [0x08] raw-address: u64            (NOT traced by GC)
```

The SAP is tagged with `0x09` (object tag) like any heap object. The GC copies the 16-byte object during collection but treats the raw address as opaque data (subtag `0x16` is in the byte-vector range `0x10-0x1F`).

### SAP Operations

```lisp
(make-sap raw-addr)         ; Allocate SAP, store raw address
(sap-ref-8 sap offset)     ; Load unsigned byte at sap.addr + offset
(sap-ref-32 sap offset)    ; Load unsigned 32-bit at sap.addr + offset
(sap-ref-64 sap offset)    ; Load raw 64-bit at sap.addr + offset
(sap-set-8 sap offset val) ; Store byte
(sap-set-32 sap offset val); Store 32-bit
(sap-set-64 sap offset val); Store raw 64-bit
(sap+ sap byte-offset)     ; New SAP at sap.addr + offset
(sap-address sap)           ; Extract raw address (tagged fixnum if < 2^62)
(sap-null? sap)             ; Test if address is 0 (NULL)
```

### Syscall Convention

Linux syscalls use SAPs for buffer arguments:

```lisp
(sys-open path-sap flags mode)  → fd (tagged fixnum)
(sys-read fd buf-sap len)       → bytes-read (tagged fixnum)
(sys-write fd buf-sap len)      → bytes-written (tagged fixnum)
(sys-close fd)                  → 0 on success
(sys-mmap len prot flags)       → SAP to mapped region
```

The syscall layer converts: SAP → extract raw address for kernel call, integer return → tag as fixnum.

### Use Cases

**Linux userspace** (mvm tool):
```lisp
(let ((buf (sys-mmap 4096 3 #x22)))   ; PROT_RW, MAP_PRIVATE|MAP_ANON
  (sys-read fd buf 4096)               ; read into mmap'd buffer
  (let ((byte0 (sap-ref-8 buf 0)))    ; access first byte
    ...))
```

**Bare-metal MMIO** (hardware drivers):
```lisp
(let ((bar0 (make-sap #xF2528000)))   ; EHCI BAR0
  (sap-set-32 bar0 0 2)               ; HCRESET
  (let ((status (sap-ref-32 bar0 4))) ; read USBSTS
    ...))
```

**DMA buffers** (fixed physical addresses):
```lisp
(let ((dma (make-sap #x480000)))       ; pre-allocated DMA region
  (sap-set-32 dma 0 qh-data)          ; write QH
  (wbinvd)                             ; flush cache
  ...)
```

### Implementation: New Opcodes

```
SAP-NEW    Vd, Vaddr        Allocate SAP with raw address from Vaddr
SAP-REF8   Vd, Vsap, Voff   Load u8 at sap.addr + off → tagged fixnum
SAP-REF32  Vd, Vsap, Voff   Load u32 at sap.addr + off → tagged fixnum
SAP-REF64  Vd, Vsap, Voff   Load raw u64 at sap.addr + off → raw
SAP-SET8   Vsap, Voff, Vval Store byte
SAP-SET32  Vsap, Voff, Vval Store u32
SAP-SET64  Vsap, Voff, Vval Store raw u64
SAP-ADDR   Vd, Vsap         Extract raw address → Vd (raw u64)
SYSCALL    Vd, Vnum, ...    Linux syscall with proper SAP/fixnum handling
```

These compile to ~3-5 x64 instructions each:
1. Untag SAP object pointer (AND + SHL to get raw object address)
2. Load raw address from SAP data word (MOV from [obj+8])
3. Add offset (untag if tagged fixnum)
4. Perform the memory access (MOV/MOVZX)
5. Tag result if needed (SHL 1 for fixnum)
