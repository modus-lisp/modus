# ALLOC-CHECK-PLAN — make `:gc-check` size-aware by fusing it with the bump

## Goal

Replace Modus's **size-blind** allocation check

```
    cmp  VA, VL          ; is the alloc pointer past the limit?
    b.cc skip
    bl   gc_trampoline
skip:
    ... allocation happens LATER, as a separate IR op, size unknown to the check
```

with the **size-aware fused** form every serious Lisp uses:

```
    add  scratch, VA, size     ; where the pointer WILL be
    cmp  scratch, VL           ; test that
    b.hi ALLOC                 ; slow path, out of line
```

> This is primarily a **correctness** change. The ~20%-of-emitted-code
> speedup is the secondary benefit, not the justification.

## Why — the correctness half

`CLAUDE.md` already states the defect, in the i386 guard-band section:

> **`:gc-check` tests `VA < VL` BEFORE an allocation whose size it does not
> know**, so the alloc that follows a passing check overshoots VL by up to that
> object's whole size. […] Residual, and identical on x64: a SINGLE allocation
> larger than the guard still overruns; **the real cure is a size-aware
> `:gc-check`, a shared-compiler change.**

Consequences we are living with today:

- The 16 MB `+linux-x64-gc-guard+` / `+linux-i386-gc-guard+` bands exist **only**
  to absorb this overshoot. They are a workaround, not a design.
- A single allocation larger than the guard still runs off the mmap and
  SIGSEGVs inside its own initialising stores.
- It has already cost us a real bug: alloc-overshoot onto the GC config page was
  the single root cause behind **three** distinct symptoms on the bare-metal Pi
  (see the alexandria-on-bare-metal-Pi work).
- `boot-linux-aarch64.lisp` still has the un-guarded shape (heap 0x38000000,
  midpoint 0x1C000000 → the same ~512-byte margin) and is a live suspect.

Fusing the check with the bump makes the overshoot **structurally impossible**,
and the guard bands become deletable.

## Why — the performance half

Measured on 93 real alexandria toplevel forms
(`/home/claude/cabfs/regmeasure.lisp`, `regmeasure2.lisp`):

```
IR opcode histogram, 74,535 IR insns:
  STACK-LOAD 23.4%   GC-CHECK 21.1%   CONS 21.0%   LI 6.2%   MOV 4.9%
```

**GC-CHECK tracks CONS at ~1:1 — 42% of all emitted IR — because we pay for the
pointer twice**: once to test it, once to bump it. Fusion collapses that.

## Prior art — SBCL, read from source

Cloned to `/home/claude/sbcl-src` (shallow). Do not re-derive from memory; the
first attempt at this comparison got SBCL's arg convention wrong.

**arm64** (`src/compiler/arm64/macros.lisp:179`, `allocation`):

```
    ldp  tmp, flag, [thread, #tlab]    ; free_ptr AND end in ONE load
    add  result, tmp, size             ; bump
    cmp  result, flag                  ; test the POST-alloc pointer
    b.hi ALLOC                         ; (assemble (:elsewhere)) — fully out of line
    str  result, [thread, #tlab]       ; commit
BACK-FROM-ALLOC:
    add  result, tmp, lowtag           ; tag
```

**x86** (`src/compiler/x86/alloc.lisp:85`+) is the same shape, plus per-register
overflow stubs `alloc-overflow-{eax,ecx,edx,ebx,esi,edi}` (and
`alloc-list-overflow-*`) selected by `(tn-offset alloc-tn)` — so the fast path
needs **no register save/restore at all** around the slow path.

Three separable cribs:

| | what | benefit |
|---|---|---|
| **(a)** | fuse bump into the check | kills the overshoot class; removes ~half of the 42% |
| **(b)** | slow path `(:elsewhere)` | fast path spends 0 instructions on overflow (we keep an inline `bl`) |
| **(c)** | per-register overflow stubs | no conservative register save around the slow path |

## Explicitly NOT doing

- **Do not move VA/VL to memory.** SBCL x86 *and* arm64 keep the free pointer
  and limit in memory and reload them per allocation (`ldp` from the TLAB).
  Modus holds them in x24/x25 (aarch64) / R12/R14 (x64) and does **zero loads**
  per check. **We are ahead of SBCL here — keep it.** The fused check composes
  fine with registers: `add scratch,VA,size; cmp scratch,VL`.
- **Do not touch the aarch64 vreg map.** Measured: the whole V9–V15 spill policy
  is **0.26%** of emitted aarch64 instructions (400 of 153,829); mapping V9/V10
  into x6/x7 nets 0.13% and is *unsafe* — only 4 of 29 `:call` emission sites do
  caller-save, so the frame spill slots are what keeps them correct across the
  other 25, across `longjmp` (SETJMP saves only SP/FP/IP), and across the actor
  `SAVE-CTX`. An earlier "5.7%" figure motivating this was a measurement bug
  (it counted any IR operand in 0..22 as a vreg, so `(:li V4 9)` scored as V9).
- **i386 register work: deferred to another day.** Real (86.8% of i386 vreg
  touches are memory-resident; V6/V7 are 77.6% of traffic and spilled while
  ESI/EDI hold V0/V1 at 0.7%) but out of scope here, and this change may move
  the arithmetic anyway by removing allocation traffic.

## Staged plan

**Stage 0 — unblock merging.** Identify ANSI test **24718** (`lambda.lsp`,
`GOT:NIL EXP:T`), the one deterministic regression from the six fixes in
`d277611`. Get the harness's own id→test mapping rather than assuming
contiguity with deftest order, then bisect the six. `#297 CL:COMPILE` is the
suspect, not the answer. Nothing merges to main until this is understood.

**Stage 1a — fuse only, guard band STAYS.**
Emit `add scratch,VA,size; cmp scratch,VL` in place of `cmp VA,VL`.
This is a strict superset of the current check: it can only reject *more*
allocations, never fewer, so **it cannot introduce an overshoot** even if the
size plumbing is imperfect. That property is the whole reason for splitting the
stages — keep it true.

**Stage 1b — remove the guard bands**, as a *separate* commit, only after 1a is
proven. One-commit-wide bisect if anything moves.

**Stage 1c — cribs (b) and (c).** Out-of-line slow path; per-register overflow
stubs.

**Stage 2 — i386.** Another day.

## Gating

This touches **every allocation on every arch** — the largest blast radius we
have taken on in a while. Required before each stage lands:

- Full ANSI, **sub-sharded comm-diff**. A single coarse shard has lied about GC
  changes before: an apparent −210 was a 600 s shard timeout in the alloc-heavy
  printer/format cluster, and sub-sharding at 900 s recovered reg=0.
  Judge by **crash markers + passed**, never raw lost-to-crash.
- **The library ladder**, because the ANSI gate is blind to library loading
  *and* blind to `mvm/gc.lisp`.
- `MODUS_GC_R14=262144` to force frequent collection on a short run.
- Bare-metal aarch64 (QEMU, then board) — the overshoot bug was found there and
  QEMU zero-fills DRAM where hardware does not.

What actually protects this change is the VERIFICATION discipline below, not
picking a good moment to start.  Every measurement error made while planning
this (a stale `ansi-file-ranges.txt`, a binary older than the commit under
test, a null test range read as a pass, a build failure diagnosed off the
wrong end of a backtrace) was caught by re-checking the measurement, and none
of them correlated with how long the session had run.  Re-check the
measurement; don't wait for a quiet box.

## Files in scope

- `mvm/compiler.lisp` — where `:gc-check` is emitted, and where the size must
  become an operand. The shared-compiler half.
- `mvm/translate-x64.lisp`, `mvm/translate-aarch64.lisp`,
  `mvm/translate-i386.lisp` — the three lowerings.
- `mvm/interp.lisp` — `op-gc-check` (currently a no-op; see the open task about
  interpreted code collecting far too rarely).
- `boot-linux-x64.lisp`, `boot-linux-aarch64.lisp`, `mvm/translate-i386.lisp`
  guard-band constants — Stage 1b only.
