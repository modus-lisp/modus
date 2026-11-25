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
- **Known bug, must be found before enabling:** with MODUS_IR_ALLOC=1, test/hosted-channel.lisp
  fails its "arena did not move" check (moved -1 MB) whenever six copies run at once; it passes
  run alone, and always passes with the pass off.
- The operand table (`%IRA-SHAPE`), liveness (`%IRA-LIVENESS`) and eligibility rules
  (`%IRA-BLOCKER`: raw LOAD / STORE are ordinary memory unless the function names VSP / VFP; a
  CALL reads no frame slot in a function without a setjmp landing) carry over to the web
  allocator.

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
    from branches.  *Future:* blocks and edges in the IR.

## IR op inventory (2026-11-25)

- 195 bytecode opcodes defined (`defopcode`, mvm.lisp); the front end emits 161 of them across
  1,546 sites; 50 defined opcodes are never emitted by compiler.lisp.
- 16 emitted ops are IR-only (never bytecode as themselves): STACK-LOAD / STACK-STORE,
  FRAME-ALLOC / FRAME-FREE / FRAME-ENTER / FRAME-LEAVE, AND / OR / XOR, SHL-VAR / SAR-VAR,
  CALL-INDIRECT, CALL-NATIVE, LI-FUNC, LI-HALVES, RAW-MARK.
- 92 emitted ops have no `%IRDF-SHAPE` entry: TRAP (48 sites), the float family (FP-*, FPD-*,
  F*, lanes, box / unbox, spill / reload), vectors (V4-*, VI-*), 32-bit word ops (W32-*),
  SAP refs, wide multiplies, atomics, I/O and per-CPU ops.
