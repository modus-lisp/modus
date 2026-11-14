# W32: native 32-bit words — implementation spec

Status: SPEC, 2026-11-14.  Written to be implemented by another session on its
own worktree and then evaluated against section 9.  Every claim about the
current tree below was read from the source on branch `mac68k-1mb` at the
commit that adds this file; line numbers are approximate pointers, function
names are the reference.

## 0. Why

The MVM was designed from a 64-bit base and every target was then unified on
it.  On the five 32-bit targets (i386, arm32, rv32, ppc32, 68k) the value model
leaks: fixnums are 30 bits, so an ordinary 32-bit quantity is a bignum, and
the same source that is fast on x64 allocates on every operation.  Measured
consequences, each fixed one at a time so far:

- `net/crypto-fast.lisp`'s 26x26-bit limb products are bignums on 30-bit
  towers; a collection inside `fe-mul` moved the arrays under a cached raw
  address (CLAUDE.md, "QEMU i386 — the bare-metal CL image").  The answer was a
  second, 990-line copy, `net/crypto-32-fast.lisp`: hand-written `MUL26LO/HI`,
  intermediates in FIXED scratch at `FE-SCRATCH-BASE` (not reentrant), spliced
  into i386 only, its limb offset patched by `%replace-all` on its source text
  (`mvm/build-cl-repl-common.lisp` ~1616-1640).
- TCP sequence numbers are bignums (`eq` -> `eql` in ip.lisp).
- `DECODE-S32` made bignums for every negative branch offset (6b7ca507).
- `(logand x #xFFFFFFFF)` on a 30-bit target compiles `#xFFFFFFFF` as a bignum
  literal: `:gc-check` + `:alloc-obj` EVERY EVALUATION (`compile-integer`,
  compiler.lisp ~8369-8409), then calls GENERIC-LOGAND.

The crypto never needed 64 bits.  Curve25519 in `crypto-fast` is already the
radix-2^25.5 (ten-limb) layout designed for 32-bit machines; SHA-256, ChaCha20
and Poly1305 are 32-bit algorithms.  What 64-bit targets get for free is
UNBOXED arithmetic: a 52-bit product fits a 62-bit fixnum.  A 32-bit target
needs the same thing expressed natively: raw 32-bit machine words the compiler
keeps out of the collector's sight.

**Sovereignty is not at stake.**  DDC compares FINAL IMAGES compiled from the
same dumped source by SBCL and by the self-hosted compiler
(`test/run-uefi-ddc.sh`, `test/run-aarch64-ddc.sh`); the MVM bytecode is
internal to both compilers and is already per-target (fixnum width and byte
order are folded into it).  A target's lowering may therefore differ from the
64-bit one freely, as long as both compilers run the same compiler source and
agree byte for byte.

## 1. Goals

- **G1 — unboxed 32-bit integer arithmetic on 30-bit targets.**  Declared
  `(unsigned-byte 32)` / `(signed-byte 32)` locals and the modular idioms
  `(logand E #xFFFFFFFF)` / `(ldb (byte 32 0) E)` (the bootstrap LDB macro
  already rewrites a constant bytespec to LOGAND, compiler.lisp ~6428) compile,
  inside a W32 scope (section 4), to native 32-bit machine operations: no
  bignum, no allocation, no GENERIC-* call.
- **G2 — one field-arithmetic source.**  `net/crypto-32-fast.lisp` and the
  `%replace-all` hack are deleted.  32-bit images use ordinary Lisp that W32
  compiles natively; nothing in a field operation allocates and nothing uses
  fixed scratch memory.
- **G3 — compiling-for is separate from running-on.**  A self-hosted compiler
  running on x64 emits a correct 30-bit image.  Proved by a new DDC:
  `modus-sh --compile-riscv32` byte-identical to SBCL's build of the same
  dumped source.

## 2. Non-goals (do not do these here)

- Microcontroller images, flash-resident layout, the per-target context block
  that replaces the fixed `#x10000xxx` addresses, a Thumb-2 back end.
- A direct native compiler for 68k, or any change to the Mac small image.
- A JIT for any 32-bit target.  (Consequence, stated so nobody is surprised:
  on the hosted 32-bit CLIs, code LOADed at runtime is interpreted, so W32
  speeds up AOT-compiled image code, not a `--load`ed natrium.)
- Changing tags, fixnum width, object layout, or the `:u64`-is-one-32-bit-op
  behaviour of the 32-bit translators.
- Using W32 on 64-bit targets.  On x64/aarch64 the compiler must never emit a
  W32 opcode in this work.

## 3. Hard constraints

- **C1 — 64-bit output unchanged.**  For any input, the code the compiler
  EMITS for x64 and aarch64 is unchanged.  The only 64-bit image functions
  allowed to differ are compiler/target-description functions whose own code
  changes because of G3 (they read a descriptor instead of a folded literal);
  list every one in the report.  Check with `scripts/fndiff.py` on symmaps +
  native dumps, base vs new, and with the x64 ANSI sweep (same pass set by
  name).
- **C2 — determinism.**  Every W32 decision is a function of the compilation's
  target descriptor (G3) and the source; never of a host special, the running
  image's folded constants, the clock, or hash-table iteration order.  The two
  existing DDCs keep passing, and the new RV32 DDC passes.
- **C3 — the collector rule.**  Every collector in the tree is a copying
  collector that scans the saved register file and the stack conservatively and
  REWRITES IN PLACE any word that passes its tag + start-bit/kind-byte test
  (i386 PUSHAD + bitmaps; arm32/ppc/68k kind bytes; RISC-V START/CONS bitmaps).
  A raw 32-bit word can pass that test.  Therefore: **no raw W32 value may be
  live across any point at which a collection can happen** — any allocation,
  any `:gc-check*`, and any CALL (calls are not modelled as GC points today,
  and any callee may allocate: a GENERIC-* slow path, a bignum literal).  A
  dead raw word left in a register or spill slot is harmless (forwarding a dead
  slot changes nothing live), so the rule is about liveness, not about
  cleaning up.
- **C4 — the interpreter is the semantic oracle.**  `mvm/interp.lisp`
  implements every new opcode with the same results as the translators.  Its
  register file is a Lisp vector the collector scans precisely, so a W32
  register value there is a Lisp INTEGER in `[0, 2^32)` (a bignum on a 30-bit
  tower: slow, correct, GC-safe).  Define each opcode's semantics on that
  mathematical value; the translators implement it on the machine word.
- **C5 — no new fixed addresses** in the `#x10000xxx` block (CLAUDE.md: the
  BSS gap is full) and **no fixed scratch memory**.  W32 must be reentrant.
- **C6 — every new opcode is tested where it is emitted.**  Interpreter + the
  32-bit translators that enable it (section 5.3) + a ladder rung per opcode
  PAIR (CLAUDE.md's lesson from the PowerPC `:setcar` bug), each with the
  harness's deliberately-wrong control.

## 4. Design

The implementer may deviate from anything in this section; record each
deviation and its reason in `docs/w32.md` (the design note this work leaves
behind).  Sections 1-3 and 9 are not negotiable without asking.

### 4.1 Target descriptor (G3) — do this first

Today `+fixnum-bits+`, `+fixnum-signed-bits+`, `+fixnum-bits-1+`,
`+target-big-endian+`, `+target-soft-float+`, `+mv-count-addr+` etc. are host
DEFPARAMETERs set per build (`set-target-fixnum-bits-for`, target.lisp ~806,
called by `build-image`, cross.lisp ~2043) and are injected into every
compilation as DEFCONSTANTs (`width-constants-source`, target.lisp ~913,
prepended by `compile-source-to-module`, cross.lisp ~89).  Inside an image the
compiler's own references to them are FOLDED LITERALS of that image's target
(`compile-variable-ref` checks `*constants*` first, compiler.lisp ~8567;
target.lisp is baked whole), so `width-constants-source` in a child build
injects the PARENT's widths.  A self-hosted x64 compiler today would emit
62-bit decisions and little-endian `:u32` splits for any target.  No existing
self-hosted path changes width (`--compile`, `--compile-aarch64`,
`--compile-uefi` are all 64-bit little-endian).

Required shape:

- One descriptor object for the compilation in progress, bound for the
  duration of `build-image` / `compile-source-to-module` (a special such as
  `*compile-target*`).  Fields: word bits, fixnum bits (and the derived
  values), big-endian, soft-float, has-fpd, W32-capable, the MV addresses, and
  anything else `width-constants-source` emits.
- **Compiler-side code** (everything that decides what to emit: compiler.lisp,
  the target/cross plumbing, `width-constants-source` itself) reads the
  descriptor.  **Runtime code** (prelude, bignum core, cl-*, anything that runs
  in the image on its own machine) keeps the folded constants, which describe
  the machine it runs on.  Draw the line explicitly; the census of the first
  kind (28 references in compiler.lisp alone) goes in `docs/w32.md`.
- A `build-checks.lisp` ratchet that fails the build if compiler-side code
  names a running-on constant (verify it fires by reintroducing one).
- Translator knobs that are defvar thunks (which do not run in-image —
  `%init-aarch64-translator`, build-modus-selfhost.lisp ~573) need the same
  explicit co-init for RISC-V: `*riscv-64-bit*` NIL, `riscv-set-linux-mode`,
  `*riscv-unimpl-ops*`.

The new DDC (`test/run-riscv32-ddc.sh`, modelled on `run-aarch64-ddc.sh`):
bake `mvm/translate-riscv.lisp` into modus-sh (names are `rv-` prefixed, so
the `+FRAME-SLOT-BASE+` collision that ruled out i386 does not arise; truncate
at `install-riscv-translator`), plus the boot descriptors in
`build-riscv32-cli.lisp`'s order; add `--compile-riscv32` to the argv dispatch
(build-modus-selfhost.lisp ~1941); an SBCL twin `test/ddc-riscv32-host.lisp`
that mirrors the co-init.  Hook it into `scripts/acceptance-gate.sh` beside
the aarch64 DDC (both refs; `MODUS_GATE_NO_DDC` skips all three).  The gate
must stay under half an hour.

**Milestone M1 = 4.1 alone, no new opcodes.**  It is independently valuable
and must land and pass the gate before W32 codegen starts, because once W32
exists the RV32 DDC is what proves both compilers emit it identically.

### 4.2 Representation

- A W32 value is an untagged 32-bit machine word in an ordinary GPR virtual
  register (V0..V15).  No separate register pool: unlike the FP pool there is
  no separate physical file, and spills of V8+ (arm32), V2/V3/V5+ (i386),
  V12+ (rv32), V9+ (ppc32), V6+ (68k) to the frame are fine under C3 because
  the scope contains no GC point.
- Signed and unsigned are properties of the OPERATION, not the value (as in
  machine code).  The compiler tracks which a variable was declared as and
  chooses `ltu`/`lts`, `shr`/`sar`, and the boxing path accordingly.
- Boxing (W32 -> Lisp integer) is itself a GC point (it may allocate a bignum),
  so the raw word must not be its live input across the allocation.  Box by
  first splitting into two 16-bit FIXNUMS (`w32-hi16`, `w32-lo16`), then
  joining in Lisp: inline `(+ (ash hi 16) lo)` when `hi < 2^(fixnum-bits-17)`
  (the value is a fixnum), else a call to a runtime joiner (sign-extending for
  a signed variable).  This is exactly the shape `compile-mem-ref` already
  uses for promoting `:u32` loads (compiler.lisp ~25814), so reuse it.
- Unboxing (Lisp integer -> W32): a fixnum is untagged in place (`w32-from-fix`,
  mod 2^32, two's complement); a non-fixnum goes through a runtime helper that
  returns the two 16-bit halves as fixnums, then `w32-join`.  A non-integer is
  a TYPE-ERROR at safety >= 1, as for any declared variable.

### 4.3 W32 scopes

The precedent is the unboxed FP tree (`%fp-tree-need` ~22650,
`%fp-scope-call-free-p` ~14055, `%let-fp-kinds` ~14257): a whitelist walker
proves a scope call-free, otherwise the variables fall back to boxed.  Do the
same for W32, with one difference that matters: the FP path relies on the
collector not touching FP registers (compiler.lisp ~13998); W32 values are in
GPRs the collector DOES scan, so the W32 walker must reject not just calls but
every allocating form and every `:gc-check`.

- **Expression scope:** an integer expression tree whose root is a modular
  idiom (`(logand E #xFFFFFFFF)`, or a declared-W32 store/binding) and whose
  nodes are + - * logand logior logxor lognot, ash by a constant, a 32-bit
  rotate idiom (section 4.5), comparisons, W32 variables, fixnum variables
  (unboxed on entry), integer literals that fit 32 bits, `mem-ref :u32`, and
  lane reads of packed u32 vectors if those are enabled (4.6).  Anything else
  makes the tree ineligible and it compiles exactly as today.
- **LET scope:** `(let ((x ...)) (declare (type (unsigned-byte 32) x)) ...)` keeps
  `x` raw for the whole body only if the walker proves the body has no GC
  point while `x` is live.  Otherwise `x` is boxed (today's behaviour).  This
  is the main lever for loops; a body with a call in it simply does not
  qualify (this is why natrium-style code keeps its rotates as macros).
- Arithmetic wraps mod 2^32 ONLY where the source says so (the modular idiom,
  or a store into a declared W32 variable at `(safety 0)`).  At safety >= 1 a
  store into a declared `(unsigned-byte 32)` variable of a value outside its
  type is a TYPE-ERROR as today; W32 must not silently change CL semantics.
  Where the compiler cannot prove an intermediate fits, it uses the wrapping
  op only under an enclosing mask.  Write down the rule you implement and test
  its edges (section 8).

### 4.4 Opcodes

New opcodes, all with `:reg` operands unless noted.  Semantics on the
mathematical value (C4); `mod` is mod 2^32.

| opcode | semantics |
|---|---|
| `w32-li Vd imm32` | Vd = imm |
| `w32-add/sub/mul` | (a op b) mod |
| `w32-mulhu` | floor(a*b / 2^32), unsigned |
| `w32-and/or/xor`, `w32-not` | bitwise |
| `w32-shl/shr/sar Vd Vs imm8` | count 0..31 only (compiler guarantees) |
| `w32-rotl Vd Vs imm8` | rotate left; rotr is rotl by 32-n |
| `w32-ltu/lts/eq Vd Va Vb` | Vd = TAGGED fixnum 1 or 0 (carry = sum ltu a) |
| `w32-from-fix Vd Vs` | Vs a tagged fixnum -> its value mod |
| `w32-hi16 / w32-lo16 Vd Vs` | Vd = tagged fixnum of bits 31..16 / 15..0 |
| `w32-join Vd Vhi Vlo` | Vhi, Vlo tagged fixnums 0..65535 -> hi*2^16+lo |
| `w32-load Vd Vaddr imm8` / `w32-store` | 32 bits at (untagged Vaddr)+off, CPU byte order, raw |

Notes:
- Variable shift counts are deliberately absent: x86 masks to 5 bits, ARM uses
  the low byte, RISC-V masks to 5 bits; a defined-everywhere variable shift
  costs a compare on most targets.  Add `w32-shlv` etc. only with that compare
  and a rung that tests counts 31, 32 and 33.
- `w32-mulhu` + `w32-ltu` (carry) are what a 64-bit accumulator built from two
  W32 words needs; that is how field arithmetic avoids both bignums and
  `mul26`.  Whether the compiler offers a W64-pair abstraction or the crypto
  source writes the pairs out (through macros) is the implementer's call;
  record it.
- 68000 (no `MULU.L`): `w32-mul`/`w32-mulhu` go through a helper like the
  existing `mul26hi` BSR helper (translate-68k ~767).  The 68k CLI is a 68020+
  target; the Mac small image is out of scope but must still BUILD.
- Existing `mul26lo/hi`: keep the opcodes; if crypto-32-fast was their only
  caller, record that they are now unused.  Do not fix ppc32's truncating
  `mul26hi` (it becomes dead) — note it.

### 4.5 Rotate recognition

Recognise the two shapes natrium and net/crypto actually use, inside a W32
scope: `(logior (ash x n) (ash x (- n 32)))` masked to 32 bits (and the
`logand`-first variant), with constant n.  Do not try to be general.  Record
exactly which shapes are recognised.

### 4.6 Packed u32 vectors on 32-bit builds — optional

`(simple-array (unsigned-byte 32) (*))` is packed (subtag `#x17`) only on
build-generic-cli and the Pi board image, both 64-bit (`*u32-vectors-p*`,
compiler.lisp ~1243); on 30-bit builds it is a generic tagged-word array whose
`aref` has no width.  Its lane layout and `%make-u32-vector` (prelude ~565)
assume the 64-bit header.  Enabling it on 32-bit builds (lane read = raw
`w32` in a scope, boxed outside) is in scope only if M3 needs it for the
measurements; otherwise list it as follow-up.

## 5. Where it lands

### 5.1 Files

- `mvm/target.lisp`, `mvm/cross.lisp`, `mvm/compiler.lisp` (descriptor; W32
  walker and lowering), `mvm/mvm.lisp` (opcodes), `mvm/interp.lisp`.
- `mvm/translate-i386.lisp`, `-riscv.lisp` (RV32 arm), `-arm32.lisp`,
  `-68k.lisp`; `-ppc.lisp` optional (5.3).
- `mvm/build-modus-selfhost.lisp`, `test/run-riscv32-ddc.sh`,
  `test/ddc-riscv32-host.lisp`, `scripts/acceptance-gate.sh`.
- `net/crypto-fast.lisp` / its replacement; delete `net/crypto-32-fast.lisp`;
  the splice in `mvm/build-cl-repl-common.lisp`.
- Stale docs found by the survey: `docs/i386.md` (63, 137-139, 254),
  `docs/networking.md` (177), `mvm/target.lisp` (562) still describe the
  deleted `crypto-32.lisp`/`crypto-w32.lisp`; fix them in passing.

### 5.2 Capability flag

The descriptor carries `w32-capable`.  The compiler emits W32 only when it is
true.  64-bit targets: false (C1).  A 32-bit target whose translator lacks an
arm: false, and it compiles exactly as today.

### 5.3 Translators

Required: i386, rv32, arm32, 68k (020+).  ppc32: either implement it or leave
`w32-capable` false there; say which.  rv64/ppc64/x64/aarch64: none (never
emitted); a W32 opcode reaching them must be a recorded gap, not silence
(CLAUDE.md: an unmatched CASE emits NOTHING).

## 6. Milestones

Each lands as its own commit(s) with the evidence in the message, in order:

- **M0 — baseline, before any change.**  The measurements of section 9.2 on
  the unmodified tree, saved, so every later number has a "before".
- **M1 — target descriptor + RV32 DDC** (4.1).  Gate passes at both refs.
- **M2 — opcodes**: interpreter + translators + rungs (C6, section 8).
  The compiler does not emit them yet except from a test-only intrinsic, so
  images are unchanged apart from that.
- **M3 — compiler W32 scopes** (4.2, 4.3, 4.5).  32-bit images change; 64-bit
  do not (C1).
- **M4 — crypto**: crypto-32-fast deleted, replacement in place, i386 SSH still
  interoperates.

Stop and report after any milestone that cannot meet its acceptance; do not
paper over it in the next one.

## 7. Working rules for the implementing session

- Work on your own worktree and branch, based on the commit that adds this
  file.  Commit freely there; **do not push** and do not merge.
- Never edit files an in-progress `sbcl` build is reading: snapshot the tree
  (copy) per build, as this branch's Mac work did.
- Stop background jobs with TaskStop, not `pkill` patterns.  Do not delete
  files other sessions own (other worktrees, other sessions' `/tmp` images).
- Disk is tight (about 4.6 GB free on /home, 6.3 GB on / today).  The
  acceptance gate needs `GATE_MIN_FREE_GB` (8) free under a workdir outside
  /tmp; clean up your own artifacts first and say so if you cannot reach it.
- Commit messages end with
  `Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>`; no model
  identifiers anywhere else in commits, code or docs.
- Benchmarks follow the workspace CLAUDE.md: SBCL and Modus numbers, with an
  independent reference check of the outputs.
- Line numbers in this spec are pointers into a moving tree; trust the
  function names.

## 8. Tests

- **Rungs** in `test/arch-rungs/` (hosted ladder: riscv32, arm32, ppc32, 68k;
  bare arch-ladder: i386, arm32, ppc32, 68k), one per opcode pair that a
  native lowering can get wrong together: add/carry (`w32-add` + `w32-ltu`
  across the 2^32 wrap), mul/mulhu across `#xFFFFFFFF * #xFFFFFFFF`, sar vs shr
  of a word with bit 31 set, rotl by 1/16/31, hi16/lo16/join round trip at
  `#x80000000` and `#xFFFFFFFF`, load/store in both byte orders (68k and ppc32
  are big-endian), boxing at 2^29-1, 2^29, 2^31, 2^32-1 for both signednesses.
- **The collector rung, with its negative control.**  A W32 value whose bits
  equal the tagged address of a live from-space object, live across an
  allocation that forces a collection (`%gc-collect-here`, or a small
  `--dynamic-space-size`).  Correct code boxes or splits it; the value must
  read back unchanged.  A build knob (e.g. `MODUS_W32_UNSAFE_LIVE=1`) that
  lets a raw W32 value stay live across GC points must make that rung FAIL,
  deterministically (choose the value so it passes the collector's start-bit
  test).  Without the failing control the rung proves nothing.
- **Semantic edges** (SBCL is the reference, run as a differential): modular
  idiom vs plain arithmetic, declared ub32 at safety 0 and 1 (TYPE-ERROR
  preserved), signed boxing, a scope rejected because of a call (must compile
  as today), a rotate shape that is NOT recognised (must still be correct).
- **Interpreter vs native differential**: the same W32 forms through the
  interpreter and through each native translator must agree.
- **Crypto**: RFC 7748 X25519 vectors, RFC 8439 ChaCha20/Poly1305 vectors,
  FIPS SHA-256 vectors on i386, rv32 and arm32 images, AOT-compiled; plus
  `./scripts/run.sh i386-ssh` with a real OpenSSH client (`= 3`), before and
  after M4.

## 9. Evaluation — what we will check

### 9.1 Correctness (all required)

1. x64 ANSI sweep (`scripts/ansi-sweep.sh`): same pass set by name as base.
2. `scripts/fndiff.py` on x64 and aarch64: only the functions listed under C1
   differ.
3. UEFI and aarch64 DDCs pass; the new RV32 DDC passes; it is in the gate.
4. `scripts/acceptance-gate.sh BASE FIX` passes (or, if disk prevents it, the
   exact parts that ran and the parts that did not).
5. Hosted ladder (all six ports) and bare arch-ladder for the 32-bit arches:
   every rung, new and old, passes; the controls answer.
6. `scripts/allarch-ansi.sh` for i386, arm32 and riscv32: same pass sets as
   base (it is the secondary suite; run it before declaring done).
7. The collector rung fails under its negative control and passes without it.
8. The `build-checks` ratchet for running-on constants fires when a violation
   is reintroduced.

### 9.2 Performance (measured before (M0) and after, reported, not gated)

AOT-compiled, on the i386, rv32 and arm32 images, with a reference check of
every output:
- SHA-256 and ChaCha20 throughput, from a self-contained AOT benchmark source
  (copy natrium's block functions with a comment naming the canonical version,
  and a test checking the copy against natrium on SBCL, per the workspace
  CLAUDE.md rule on local copies).  For i386, which `build-hosted.lisp` does
  not take, find or add a way to splice the benchmark into an image and
  document it.
- X25519 scalar multiplication time.
- Executed instruction counts for one SHA-256 block on rv32: this qemu (7.2,
  Debian) has no TCG plugins, so use `qemu-riscv32-static -singlestep -d exec`
  on a short run; use wall time with at least 5 repetitions for long runs.
- Image size of each 32-bit image before/after.

### 9.3 The report

`docs/w32.md`: what was built; every deviation from section 4 with its
reason; the running-on/compiling-for census; the C1 function list; all the
numbers of 9.2 as before/after tables; what is still open.  Plus a short
CLAUDE.md section pointing at it.
