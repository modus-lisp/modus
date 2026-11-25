# IR rework: register allocation at the IR layer

Status: in progress (branch `modus-float-scopes`).  Started 2026-11-25.

## Why

Modus compiles Lisp to MVM IR, then to bytecode, then (JIT) to native code nearly one to one.
Register choices are made by the **front end** while it emits IR: temps come from a counter
(`alloc-temp-reg` / `free-temp-reg`), five locals per function are promoted to V9-V13 for the
whole function (`%promotion-choice`), everything else lives in frame slots (`OBJ-REF VFP n`).
Nothing downstream can revisit those choices.

Measured on stave's small mill ops (2026-11-25): mill's Where inner loop keeps ~13 values live
(four arrays, three indices, three strides, counter, bound, output index); five get registers
and the rest are reloaded from the frame on every element.  Add / Mul / Slice are 1.7-2.7x SBCL
per call, Where 1.1x after the boxing fix, and most of what is left is this.

## Plan

The direction is the IR layer, without locking the front end into its current shape:

1. **IR rework (now).**  Recover virtual registers from the IR the front end already emits
   (def-use webs over registers and non-escaping frame slots), allocate them with liveness onto
   the same physical set, rewrite the IR.  Bytecode, translators, interpreter and fasl cache are
   unchanged.  First version must reproduce today's bytecode byte for byte; then allocate better.
2. **Front-end notes (alongside).**  Every place the front end constrains the IR goes in the log
   below, with what a front end free of it would emit instead.
3. **New front end (after).**  With the IR rework landed, the log says what a better front end
   emits.  Implemented in bulk (agents), verified by a differential harness against the current
   pipeline.
4. **Settle the IR on the new front end.**  Remove the recovery scaffolding the new front end
   makes unnecessary; keep the allocator.

Rule while doing 1: **no front-end-specific cleanup rules in the allocator.**  If the allocator
seems to need a rule that undoes a front-end habit (a save PUSH/POP, a spill gate), that is a
front-end constraint: log it here instead.

## Step 1 pieces

- **Operand table.**  For every IR op: which operands are GPR reads / writes, which are FP
  registers, and hidden effects (calls, allocates / GC point, clobbers VR or scratch, traps,
  touches the frame).  `%IRDF-SHAPE` (compiler.lisp) covers 69 of the 161 ops the front end
  emits -- 85% of 1,546 emit sites.  Inventory: see "IR op inventory" below.
- **Webs.**  Def-use webs over V4-V15 and frame slots whose address never escapes
  (`%IRDF-FRAME-TOUCH-P`).  Each web is a virtual register.
- **Allocator.**  Interference graph, colored onto the target's physical set (aarch64: V4-V8
  temps, V9-V13 locals, spill slots), fixed constraints at calls (V0-V3 arguments, VR return,
  webs live across a call only in callee-saved V9-V13 or a spill), costs weighted by loop depth.
- **Rewrite + check.**  Rename operands, emit bytecode as today.  `MODUS_IR_ALLOC=0` turns it
  off; a range variable bisects functions (as `MODUS_IR_DATAFLOW_RANGE`).  The identity mode
  must produce byte-identical bytecode for the image, the suite, mill and stave.

## Findings (2026-11-25)

- **On aarch64 all ten of V4-V13 survive a call.**  Every prologue saves x19-x23 (V4-V8) and
  the five local registers x6 x7 x4 x5 x8 (V9-V13) (`a64-emit-prologue`,
  `a64-emit-local-regs-save`), and the interpreter's in-module call snapshots V4..V13 and
  restores them at RET (interp.lisp, op-call, op-call-ind).  So an allocator may keep any value in
  any of V4-V13 across calls: twice today's local pool, with no translator change.  x64 differs
  (V4 = RBX; its callee-saved set is RBX R12 R14 R15), so the allocator takes the callee-saved
  set per target.  Exceptions to honour: traps in `*a64-local-scratch-traps*` clobber x4/x5/x8
  but the translator saves them around those arms; CALL-NATIVE into non-modus code (AAPCS) does
  not preserve x4/x5/x8 -- treat it as clobbering V11-V13 (or skip such functions in v1).
- **TRAP 257 / 513 / 33025 in listings are not traps in the IR.**  They are FRAME-ALLOC n
  (#x100+n), FRAME-FREE n (#x200+n) and FRAME-ENTER, lowered to TRAP when bytecode is emitted
  (`emit-bytecode-for-ir`).  Real TRAP ops in the IR are I/O, syscalls, the &rest copy (#x0530),
  setjmp / longjmp and the like -- rare in numeric code.
- **Float ops seen from the GPR side** (aarch64 translator): FP-OP1 / FP-OP2 / FP-MOV / FPD-ADD
  .. FPD-DIV / FP-LI / FPD-LI / V4-DUP touch no GPR; FPD-UNBOX / FP-UNBOX / FPD-ITOF / FPD-SCALE
  read operand 2; FPD-BOX / FP-BOX / FP-TOI / FPD-LT / FPD-LE / FP-CMP write operand 1 (the
  boxes bump the allocation pointer directly; their GC-CHECK is a separate op); FP-LANE-LOAD /
  V4-LANE-LOAD read 2 and 3; FP-LANE-STORE / V4-LANE-STORE read 1 and 2; F32-LOAD / F32-REF /
  FSQRT / FROUND32 / ALLOC-F32 / ITOF / FTOI read their sources and write operand 1; F32-STORE /
  F32-SET read 1 2 3; FP-SPILL writes frame slot(s) (two for a double), FP-RELOAD reads them.
  The legacy FADD / FSUB / FMUL / FDIV use D0 / D1 implicitly: left unknown.
- **Target-specific bytecode is safe to cache.**  Once the allocator uses aarch64's callee-saved
  set, its bytecode is only correct where V4-V13 survive calls.  The fasl key (`%fasl-key`,
  fasl.lisp) hashes *FEATURES*, which carries the architecture (:ARM64 here), so another target
  never replays it.  The allocator's on/off switch must go into the key as well (as
  `*park-safepoints*` does), or MODUS_IR_ALLOC=0 would replay allocated bytecode from the cache
  and bisecting would lie.

## Step 1a: slot promotion (2026-11-25) -- built, off by default

`%IR-ALLOC` (whole function) and `%IR-ALLOC-LOOPS` (per innermost loop, read-only slots) in
compiler.lisp, run from `%RT-IR-PASSES` with `MODUS_IR_ALLOC=1` (aarch64 run-time compiles).
`MODUS_IR_ALLOC_TRACE=1` reports per function why it was skipped or what moved;
`MODUS_IR_ALLOC_WHOLE=0` / `MODUS_IR_ALLOC_LOOPS=0` turn either half off.  The switch is part of
the fasl key.

What it showed:

- **No measurable gain on stave.**  Over a pass it changed ~900 of ~1,350 functions and promoted
  7 loops; pass time and the per-op replays did not move beyond noise.
- **mill's hot loops do not reload frame slots.**  Where's inner loop reads no slot at all: its
  values are already in registers.  Its remaining cost against SBCL is per-element work -- bounds
  checks that reload the array length, tag tests on (unsigned-byte 62) counters, overflow-checked
  increments: representations (log entry 6), not allocation.
- **Loops are rotated.**  The entry jumps to the loop test partway through the body, so "first
  label to the back edge" is not the loop: most loops failed the single-entry test.  The web
  allocator needs a real CFG with natural loops (dominators), not index ranges (log entry 12).
- **Code alignment swamps small effects.**  A hot loop's speed swung 30% (1.41 vs 1.85 ns an
  element) with every 4 bytes of code added before it.  Fixed in the aarch64 translator: loop
  headers (targets of backward branches) are padded to 16 bytes (`MODUS_ALIGN_LOOPS=0` disables).
  Measurements before this were subject to that swing.
- **hosted-channel under load (open).**  With MODUS_IR_ALLOC=1, test/hosted-channel.lisp fails
  its "arena did not move" check (moved -1 MB) every time six copies run at once; alone it passes,
  and with the pass off it always passes -- also when the arena is shifted by up to 6 MB before the
  test, so it is not where the window starts.  Bisecting by function (MODUS_IR_ALLOC_RANGE,
  MODUS_IR_ALLOC_HASH) finds no single culprit: it takes changes in both halves of any split.  And
  MODUS_IR_ALLOC_DRYRUN=1 (do all the work, keep the original IR) fails the same way, so it
  follows the pass's compile-time activity, not the code it produces.  The receive loop is one
  top-level form compiled before it runs; what compiles inside the window is not yet known.
- **The code it produces is slower to load.**  stave's load: 5.85 s off, 6.6 s on, 5.9 s dry run.
  Suspect: the entry loads -- every function now loads each promoted parameter slot into its
  register at entry, though many are read once on a cold path.  The web allocator should weigh
  entry cost against uses (loop depth) and not promote cold slots.
- **Bisecting tools** (compiler.lisp): MODUS_IR_ALLOC_TRACE=1 names each function and what was
  done; MODUS_IR_ALLOC_RANGE=LO,HI (by order offered), MODUS_IR_ALLOC_ONLY=TEXT (name contains),
  MODUS_IR_ALLOC_HASH=D,V (low D bits of a hash of the name), MODUS_IR_ALLOC_DRYRUN=1.
  Liveness sets keep frame slots at bit 17+ so they stay fixnums (they were bignums at bit 32).
- The operand table (`%IRA-SHAPE`), liveness (`%IRA-LIVENESS`) and eligibility rules
  (`%IRA-BLOCKER`: raw LOAD / STORE are ordinary memory unless the function names VSP / VFP; a
  CALL reads no frame slot in a function without a setjmp landing) carry over to the web
  allocator.

## Step 1b: natural loops (2026-11-25) -- built, off by default

`%IRA-CFG` (basic blocks), `%IRA-DOMINATORS` (Cooper-Harvey-Kennedy over reverse postorder),
`%IRA-NATURAL-LOOPS` (back edge T -> H with H dominating T; body = H plus what reaches T without
passing H), and `%IR-ALLOC-LOOPS` rebuilt on them: in each innermost natural loop the hottest
slots -- read-write ones too -- take registers no instruction of the body uses, loaded on every
edge in and stored back on every edge out (stores before loads on an edge that leaves one loop
and enters another; fall-through edge code before the target's label, taken-branch edge code in
a block at the end of the function).

- **Correct:** the suite passes with it on (MODUS_IR_ALLOC=1 MODUS_IR_ALLOC_WHOLE=0), and stave's
  transcript is unchanged.
- **Finds the real loops:** mill's OP-WHERE (27,287 instructions, 6,315 blocks) has 15 loops, 10
  innermost; each per-dtype element loop gets 2-3 of its index slots (31-34) into V7 / V8 / V13.
- **No gain:** Where's replay stays at 11.2-11.5 us, the stave pass at ~1.5 s.  And compiling is
  slower: stave's load goes 5.9 -> 8.5 s (dominators over thousands of blocks, per function).
- **Why no gain -- the real finding of step 1:** on Apple cores a frame-slot load hits L1 and a
  store forwards to the next load; they cost about what a register move does.  The gap to SBCL is
  instruction count.  SBCL's loop for the same Where-shaped code is 35 instructions an element and
  does the same safety work (reloads each array's length per access, tag-tests the steps,
  overflow-checks the adds); modus's is ~60 IR operations an element, more native instructions.
  Example: a bounds check is SBCL's LDR length / CMP / B.LS (unsigned: a negative index fails too)
  against modus's CMP-IMM idx 0 / BLT / ARRAY-LEN / CMP / BGE.

## Revised direction (2026-11-25)

Allocation is not the bottleneck on this hardware; instructions per operation are.  What to do
next, in order of expected payoff:

1. **Count native instructions per IR op in hot loops** (JIT bytes disassembled) against SBCL's,
   to rank the expansions worth fixing.
2. **Unsigned compare-and-branch** (an IR / bytecode branch on unsigned >=): every bounds check
   becomes length / compare / one branch.  Needs an interpreter arm and translator arms (aarch64
   first; elsewhere the old sequence).
3. **Leaner translator expansions** of ARRAY-LEN, ADD-CHECKED, tag tests and tagged index
   scaling (SBCL folds the untag into the address: ADD TMP, R2, NL1, LSL #1).
4. **Loop-invariant hoisting** of ARRAY-LEN and array loads, using the natural loops above --
   only once each costs instructions, not memory traffic.
5. The web allocator stays the long-term plan (and the front end's register decisions stay in
   the log), but it is no longer the first lever.

## Step 2: what the native loop actually costs (2026-11-25)

Method: warm a function past the hot-call threshold, read its JIT code (the native address as the
profiler's JIT map computes it, words via `%GC-READ64`), wrap the words in `.word` and disassemble
with `as` / `otool -tv`.  Where-shaped loop, one element, hot path:

- ~87 native instructions against SBCL's 35 -- but cutting 10 (the alignment NOPs below, MOVZ of
  compare immediates) changed nothing.  Moves and NOPs are nearly free on Apple cores.
- **The floor was taken branches**: ~9 an element (each checked add's `B.VC` around its inline
  overflow call, the IF arm's branch, the jump to the safepoint, the safepoint's `B.EQ` around its
  inline parking code, the back edge) against SBCL's ~2; these cores retire about one taken branch
  a cycle, and 9 of them is ~11 cycles = the measured 2.5 ns.

Fixed in the aarch64 translator:

- **Overflow slow paths out of line**: checked add / subtract / multiply branch to the
  GENERIC-ADD call (not around it); the call is emitted after the function and branches back.
  Where's loop 2.50 -> 1.89 ns an element.
- **Safepoint parking out of line**: the poll falls through when no stop is asked for.  1.88 ->
  1.83.
- **Compare / test immediates in the instruction** (CMP / CMN #imm12, TST #bit-or-low-ones).
- **Loop-header alignment only at real loop headers** (a backward BR right after SAFEPOINT /
  YIELD): out-of-line slow paths branch back into loop bodies, and padding their targets had put
  NOPs in hot paths.

stave: pass 1.47 -> 1.37 s (SBCL 1.3).  Per call: Where 11.2 -> 8.4 us (SBCL 9.5), Add 3.8 -> 3.5,
Transpose 5.5 -> 4.9.

cl-bench geomean 2.61x -> 2.47x SBCL (2d-arrays, fft, puzzle 12-14% faster).  One regression:
crc40 +7% (0.212 -> 0.227 s), with a loop that is strictly better code (48 instructions against 84,
one taken branch fewer).  Its critical path is a loop-carried chain THROUGH A FRAME SLOT -- the
remainder is stored and reloaded every step (store-to-load forwarding, ~15 cycles a step) -- and
that latency moves with layout.  The first case where memory traffic, not branches, is the floor:
the web allocator would keep the remainder in a register.

Then, with the loop down to two taken branches an element (one IF arm, the back edge), it was
throughput-bound (~9 instructions a cycle), and instruction count started to matter:

- **Tail duplication** (`%IR-TAIL-DUP`, an IR pass; MODUS_IR_TAILDUP=0 disables): a BR to a short
  straight block (<= 4 instructions, ending BR / RET / TAILCALL) becomes a copy of it, so the back
  edge's `(:safepoint) (:br top)` is inline.  No time change by itself -- branches were no longer
  the floor -- but one taken branch fewer per iteration.
- **Bounds checks as one unsigned compare** (aarch64 translator, A64-BOUNDS-CHECK-MATCH): the
  compiler's `CMP-IMM idx 0 / BLT bad / ARRAY-LEN / CMP idx len / BGE bad` is translated as the
  length load, the CMP, and `B.HS bad` -- a negative tagged index read unsigned is above any
  length.  Translator-only: the interpreter and the IR are unchanged.
- **Tagged index scaling folded into the address** (f32 and u8 lanes): `ADD x9, arr, idx, LSL #1`
  / `ASR #1` in place of an ASR and an ADD.

Where-shaped loop 1.76 -> 1.54 ns an element.  stave pass 1.33-1.36 s (SBCL 1.3); Where 7.7 us a call.

Still to take: the IF arm's jump over the else arm (not a short block -- needs layout, or
if-conversion of a float select into FCSEL); copies the front end leaves before uses (`MOV x19,
x4` then x19); the tag test on an (unsigned-byte 62) counter (5 instructions; TBNZ is 1).
- **Copy forwarding through float code** (`%IRDF-SHAPE*`): at aarch64 run-time compiles the
  dataflow pass uses `%IRA-SHAPE`, so FP lane loads / stores no longer end forwarding -- the
  index copied into a temp before every element access goes.  ADD-TILED core 1.44 -> 1.26 ns an
  element; Add 3.6 -> 3.4 us a call.  MODUS_IRDF_WIDE=0 disables.
- **Dataflow liveness sets as fixnums** (`+IRDF-SLOT-BIT+` 17, was 32): every set was a bignum.
  stave's load (cache off) 5.8 -> 5.1 s, every target's compile cheaper; with the wider
  forwarding above it is 5.8 s -- where it started.

Tried and reverted: skipping the tag test of an operand loaded with a fixnum literal a few
instructions up (`%IR-RECENT-FIXNUM-P` in EMIT-ARITH-PAIR) -- no gain on stave and cl-bench
BITVECTORS 5% slower, consistently.  Probably layout again; not worth it unmeasured.

Earlier note, before these: the IF arm's unconditional jump over the else arm,
and the jump from the loop body to the back-edge block (the front end emits `(:br L) ... (:label
L) (:safepoint) (:br top)` -- block layout / jump threading, an IR pass).  Then the instruction-
count items (bounds check 8 -> 3 with an unsigned branch, index scaling, tag tests).

## Step 3: model load (2026-11-25)

stave on modus vs SBCL: passes now within 4% (1.27 vs 1.22 s), but loading the Zipformer model
took 3.3 s against 0.4.  Profile: ~60% in %BITS->SINGLE (SB-KERNEL:MAKE-SINGLE-FLOAT, one call per
weight: a field-by-field Lisp conversion with checked shifts and bignum ASH), ~25% in reading the
262 MB weight file a byte at a time out of a 4 KB I/O page.

- `%BITS->SINGLE` goes through a one-lane packed vector and the native lane load (infinities and
  NaNs keep the Lisp path, which preserves a signalling NaN's payload).
- `(setf (aref f32 i) (sb-kernel:make-single-float x))` as a statement compiles to
  `%F32-BITS-SET` -- the bits into the lane, no float made, exact for every pattern.
- `READ-SEQUENCE` of a regular file into a packed byte vector reads straight into the vector, a
  megabyte a syscall (`%FS-READ-U8-BULK`): 262 MB 0.57 -> 0.032 s (SBCL 0.021).  Safe: no
  allocation between taking the address and the read returning, and the plain syscall (not the
  GC-safe-region one sockets use) holds collections off for the read.

Model load 3.3 -> 0.6 s (SBCL 0.4).  test/model-load-paths.lisp.

System load (ASDF loading stave + mill, compiling them): 5.35 -> ~4.5 s, all in the compiler's own
work.  `(member x '(...))` of a short constant list is inlined as an EQL chain returning the
list's own tails (it parsed keyword arguments per call -- a third of the load, in the IR passes);
FUNCTIONP answers T for a closure / native function object before walking the generic-function
registry; bignum limbs read with %PRIM-AREF (the dataflow sets of functions with many slots are
bignums).

**The FASL cache now keeps modules with layout addresses.**  It rejected any module with LI-ADDR /
LI-TADDR, so 136 of stave's forms (every function touching a special variable's fixed cell, among
others) were recompiled at every load even with the cache warm.  Their operands are link-time
addresses every consumer slides into its own process (mvm.lisp), and the key's build record fixes
the layout: replay is sound.  Checked: recorded at slide 0x300000000, replayed at 0x200000000,
transcript unchanged.  Warm load 4.4 -> 1.2 s (441 modules replayed, 67 forms source).

Only addresses fixed by the build are cached: the recorder classifies each LI-ADDR / LI-TADDR
operand (%FASL-LAYOUT-REGION) and keeps a module only when they are in the image's code region or
the fixed runtime-data region; one in the heap or the JIT arena names what that run put there,
and the module stays source.  stave has none of the latter.

The cl-bench "regressions" this seemed to cause (div2-test-1 70 -> 83 ms, deriv 66 -> 72 ms) are
collection placement, not code: compiling from scratch churns enough garbage that a collection
happens just before the timed batch; a warm start begins mid-semispace and takes two collections
inside it (CLB_GCCOUNT: 0 vs 2 collections, 0.069 vs 0.079 s; the next batch 0.072 vs 0.071).
Compare runners on the same cache state.

Fixed (cf923dec): `(logand (+ 3 7) 255)` failing to compile.  The cause was APPLY, not LOGAND: in
baked code `(apply #'+ list)` took COMPILE-APPLY's pre-packed &rest call, which is wrong for a
callee with &optional parameters (CL's + and * have them), so + saw NIL -- and %CONST-INT-VALUE
folds literal arithmetic with APPLY.  Also (48a31786): generic + - * / signal TYPE-ERROR on any
non-number once *%ARITH-STRICT* is set (after boot); comparisons stay lenient, pending their audit.

## Front-end constraint log

Each entry: what the front end does, what it costs, what a front end without the constraint
would emit.

1. **Physical register names in the IR.**  Temps are V4-V15 handed out as a stack
   (`alloc-temp-reg`, 276 sites; `free-temp-reg`, 261); V4-V8 are real, V9-V15 spill
   (aarch64: V9-V13 are the five local registers when promotion is on).  *Future:* unlimited
   virtual registers; physical placement is the allocator's.
2. **Save PUSH/POP around operands.**  Binary ops push the left operand while compiling the
   right unless `%ARG-PRESERVES-DEST-P` / `%PURE-SIMPLE-EXPR-P` prove it safe; the predicates
   are about physical registers (and x64's VR-is-scratch).  Found this week: an index sum
   `(+ i 3)` was "not call-free", so every 4-way string compare pushed and popped.  *Future:*
   no saves in the IR; a value live across a call is the allocator's to keep or spill.
3. **Spill gates and temp budgets.**  `%TEMPS-MUST-SPILL-P` switches constructs to push-based
   variants near the 12-temp limit; "out of temporary registers" is a compile error.  *Future:*
   none.
4. **Function-wide local promotion.**  Five locals get V9-V13 for the whole function
   (`%promotion-choice`, `%inner-let-candidates`); the rest are frame slots read per use.  *Future:*
   locals are virtual registers like temps; the allocator weighs them by loop depth.
5. **FP register budget per tree.**  The FP pool is 12 registers including the enclosing tree;
   a node whose need exceeds it silently boxes the whole tree.  Every float slowdown chased in
   November traced to a tree that did not "fit" or a node the tree did not accept (a single
   operand in a double tree, EXP's need of 8, LOG).  *Future:* FP values are virtual registers
   with a float representation; boxing is an explicit instruction placed where a value leaves it.
6. **No representations.**  IR values are tagged words.  An `(unsigned-byte 62)` counter is
   tag-tested on every increment (wider than a modus fixnum); unboxed floats exist only inside
   FP trees and FP-resident locals; FP-SPILL / FP-RELOAD are emitted by the front end around
   calls.  *Future:* each virtual register has a representation (tagged, raw integer, single,
   double, vector lane); boxing and unboxing are instructions the allocator / representation
   pass places.
7. **Fixed call protocol in the front end.**  Arguments are moved into V0-V3 and VR read back
   at every call site, and live temps are saved there.  *Future:* a call instruction with its
   arguments as operands and a declared clobber set; the allocator places the moves.
8. **Frame slots are addressable.**  Raw LOAD / STORE and VFP operands mean a slot can be
   touched indirectly, so a function using them is left alone.  *Future:* frame slots do not
   exist before allocation; spill slots are allocator-private.
9. **Traps with implicit register effects.**  47 TRAP sites, each with its own effect on
   registers (syscalls, setjmp landings, the &rest copy); a function with one is skipped by
   `%IR-DATAFLOW`.  *Future:* each trap is an instruction with declared operands and clobbers.
10. **Saves sized for the weakest target.**  The front end saves live temps around every call
    because some targets do not preserve them; on aarch64 every one of V4-V13 is preserved, so
    those saves are pure cost there.  *Future:* the call's clobber set comes from the target.
11. **Frame slots allocated per scope.**  LET bodies grow the stack with FRAME-ALLOC n and shrink
    it with FRAME-FREE n around their temporaries.  *Future:* a fixed frame laid out once by the
    allocator.
12. **Loops are not visible, and are laid out rotated.**  The IR is a flat list with labels; loop structure is re-derived
    from branches.  Also: the back edge is a
    separate block reached by a jump (`(:br L) ... (:label L) (:safepoint) (:br top)`), so the hot
    path takes two branches per iteration where one would do.  *Future:* blocks and edges in the
    IR, placed by a layout pass.

## IR op inventory (2026-11-25)

- 195 bytecode opcodes defined (`defopcode`, mvm.lisp); the front end emits 161 of them across
  1,546 sites; 50 defined opcodes are never emitted by compiler.lisp.
- 16 emitted ops are IR-only (never bytecode as themselves): STACK-LOAD / STACK-STORE,
  FRAME-ALLOC / FRAME-FREE / FRAME-ENTER / FRAME-LEAVE, AND / OR / XOR, SHL-VAR / SAR-VAR,
  CALL-INDIRECT, CALL-NATIVE, LI-FUNC, LI-HALVES, RAW-MARK.
- 92 emitted ops have no `%IRDF-SHAPE` entry: TRAP (48 sites), the float family (FP-*, FPD-*,
  F*, lanes, box / unbox, spill / reload), vectors (V4-*, VI-*), 32-bit word ops (W32-*),
  SAP refs, wide multiplies, atomics, I/O and per-CPU ops.
