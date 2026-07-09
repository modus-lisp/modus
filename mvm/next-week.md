# Pick-up notes — fragility investigation, week of 2026-04-27

Concise version for getting back in.  Full backstory lives in
`fragility-notes.md` (running log) and `fragility-open-problem.md`
(the deep-detail current-state doc).  This file is just the
shortest path to "where am I, what's next."

## Where we are (refreshed 2026-04-28)

Five same-shape bugs of the "deref-without-tag-check" family fixed
across the prior week (functionp/characterp, obj-subtag, array-len,
plus two earlier).  Cumulative: **+226 unique tests at N=0, +230
at N=1** vs pre-investigation baseline.

Originally a sixth bug was named ("car/cdr IR-op tag-unsafe") with
markers 9130-9133.  **Bug 6 is now retired as a misdiagnosis.**
car/cdr SIGSEGVing on non-cons IS the implementation of CL's
TYPE-ERROR signaling — the in-process SIGSEGV handler converts the
fault into a condition.  An attempted "fix" that returned NIL on
non-cons closed the markers but **broke 90 *.ERROR.* ANSI tests**
because it silenced an error signal that ANSI requires.  Reverted;
markers re-aimed to expect the caught-condition sentinel.  See
`fragility-open-problem.md` "bug 6 misdiagnosis (debrief 2026-04-28)".

The doc's "11-test CLOS family" list is stale.  At 2026-04-28
baseline, 9 of those 11 already pass — only 27084 and 27465
still fail.  The current flippy distribution has not been
measured against an up-to-date binary.

## First thing to do back at the keyboard

```sh
sbcl --dynamic-space-size 2048 --script mvm/build-x64-linux.lisp
/tmp/modus-ansi-test | grep -E '^(P|FAIL ).?91[03][0-3]'
```

  - **4 Ps in 9130-9133:** car/cdr error-signaling is functional.
    That's the expected steady state.
  - **Any FAIL in 9130-9133:** the SIGSEGV → handler-case → :crashed
    path is broken.  Read the captured RIP/SITE/RAX from the FAIL
    line, decode per the rubric below.

## What's left, in priority order

### 1. Refresh the flippy list

The per-test landscape has moved since the doc was written.  The
old list (REINITIALIZE-INSTANCE.{1-4}, etc.) is largely
already-passing.  Before doing any new "root cause" work, find out
what's actually flippy *today*:

```sh
scripts/fragility-fuzzer.sh    # 9 builds × ~22min ≈ 3-4 hours
```

Then diff the per-N pass sets to get the current flippy list, with
*test names* (look up via `build-x64-linux.lisp`'s ID-to-name dump).
Only after that do the "what to fix next" question.

### 2. The two remaining flippy CLOS tests (27084, 27465)

CLASS-0206.1 and MAKE-INSTANCE.ORDER.1 are the only two from the
old CLOS family still failing at baseline.  They have no captured
state (so they're not crashing — they're returning wrong values).
Worth pulling apart with a `*run-only-below*=27500` shard and
adding diag prints to see what they return vs expect.

### 3. CHANGE-CLASS isn't fragility — separate work

`ansi-bridge.lisp:1352-1354` is currently a no-op stub:

```lisp
(defun change-class (instance new-class &rest initargs)
  instance)
```

Several originally-flippy CLOS tests (CLASS-0203.2,
CLASS-0205A.1, CLASS-0206.1, CHANGE-CLASS.4.5) fail because of
this — slot values aren't transferred, initforms in the new
class never run.  Test 27081's GOT/EXP diff shows it
unambiguously.

Implementing properly requires: walk new-class's slot list, find
same-named slot in old class, copy value if found else leave
unbound for initforms.  Then run slot initforms / supplied
initargs (the `shared-initialize` machinery).

**Don't fold this into the fragility narrative.**  It's
unimplemented-feature work; mixing the two contaminates each
other's debugging.

## Diagnostic infrastructure already in place

Don't re-invent these next time you need them.

### SIGSEGV handler state capture

`translate-x64.lisp` `+op-install-signal-handlers+` (#x0520
trap) — the embedded sigaction stub captures pre-longjmp state
into fixed slots:

  - `0x10000C30` — saved RIP   (ucontext+0xA8 = uc_mcontext.gregs[16])
  - `0x10000C38` — saved RSP   (ucontext+0xA0 = gregs[15])
  - `0x10000C40` — `[saved RSP]` (return addr — byte after failing call)
  - `0x10000C48` — saved RAX   (ucontext+0x90 = gregs[13])
  - `0x10000C50` — si_addr     (siginfo+16)
  - `0x10000C58` — ucontext ptr itself

`%record-test-fail` (in build-x64-linux.lisp) prints all five
divided by 4 (to keep print-dec on the fast path; raw values lose
low 2 bits in the sar+and chain).  Multiply by 4 to recover real
addresses.

**Critical:** sa_flags includes `SA_SIGINFO` (= 0x4); without it
the kernel doesn't put ucontext in RDX and the captures read
garbage.  I missed this the first time.

### Fault classification rubric (from collaborator)

  - `si_addr` inside NIL-page (`[0xDEAD0000, 0xDEAD1000)`) +
    saved-RIP equals si_addr → **instruction-fetch fault** =
    "called something that resolved to a tagged immediate".
    The actual offending instruction is the call; `[saved RSP]`
    points just past it in the caller — that's the byte to disasm.
  - `si_addr` elsewhere → **data fault**.  Saved-RIP is the
    load/store instruction; si_addr is the bad effective
    address.  Disasm at saved RIP to see what kind of access.

### eq-collision probe at `%specializer-matches-p`

`cl-clos.lisp` lines around 503 (the function).  Tightly gated
(only fires on eq-mismatch + name-hash match, budget-limited via
slot `0x10000C60`).  Today it ran a full ANSI suite and never
fired — so cross-function intern non-determinism is **not** the
cause of method-dispatch fails on this codebase.

Initialize the budget in `kernel-main`:

```lisp
(setf (mem-ref #x10000C60 :u64) 5)
```

### fragility-fuzzer

`scripts/fragility-fuzzer.sh` does the multi-N sweep.  Reads
`MODUS_FUZZ_FUNCALL_NOPS` env var.  Outputs flippy tests in
`/tmp/fragility-fuzzer/`.  Already plumbed; just run it.

## What didn't work — don't re-try these

  - **In-process instrumentation of the predicates** (printing
    inside characterp/functionp/symbolp).  Heisenbergs — the
    diag bytes shift layout and the bug moves.
  - **Tightening characterp** to check 16 bits.  MVM's char
    encoding is `(code << 8) | tag`, so byte 1 is the char-code's
    low byte.  Tightening rejects every non-#\Null character
    (-191 tests).  See compile-characterp's docstring.
  - **The naive GC stale-table fix in `%intern-symbol`.**
    Theoretically correct but the one extra mem-ref shifted
    layout and regressed 159 unrelated AREF/ARRAY tests.
    Documented in the docstring as a hazard.  GC's stack scan
    actually does update frame slots, so the local `table`
    variable isn't stale across `%make-symbol` — the bug
    doesn't exist as I'd hypothesized.
  - **gdb on the fork's SIGSEGV.**  The binary's sigaction
    handler intercepts before gdb sees it, even with
    `set follow-fork-mode child` and `catch signal SIGSEGV`.
    Use the in-binary handler-state capture instead, or break
    on the handler entry address and walk siginfo/ucontext
    from RSI/RDX manually.
  - **Disassembly-diff of test thunk** at N=0 vs N=1.  The thunk
    bytes are nearly identical; the bug is in *called* code (the
    primitives the thunk reaches).  Don't burn a day on this.

## How to debug another one

When the next same-shape bug surfaces:

1. **Get the failing test's captured state** — RIP, SITE, RAX,
   si_addr.  All printed in the FAIL line.
2. **Classify**: `si_addr` vs saved RIP relationship (rubric
   above).  Tells you instruction-fetch vs data fault.
3. **Disasm at saved RIP** (`objdump --adjust-vma=0x400000
   --start-address=...`).  The faulting instruction reveals the
   IR-op (`mov d, [src + N]` patterns map to `:car`, `:cdr`,
   `:obj-ref`, `:obj-subtag`, `:array-len`, `:aref`).
4. **Look at the IR-op's translator code** in
   `translate-x64.lisp` for the same shape: bare `mov d, [src ±
   offset]` without a tag-check.
5. **Apply the same shape of fix** — tag-check, return
   sentinel-value (NIL or 0) on tag mismatch.  Use the same
   register conventions as obj-subtag/array-len got: `tmp =
   (if (eq d 'rax) 'r13 'rax)`, push/pop around the fix.
6. **Add a regression test** in `ansi-tests.lisp`'s 91xx range
   that exercises the bug pattern explicitly with expected NIL
   (or the right value) — pre-fix it FAILs with the captured
   signature, post-fix it Ps.
7. **Validate via fragility-fuzzer.**

## Methodology meta-lessons

  - **Heisenberg-resistant probes are the only useful ones for
    this class of bug.**  Place at end-of-kernel-main (the last
    defun in source order — adding bytes there shifts only
    kernel-main's body, not earlier functions where the bug
    actually lives).  Or gate tightly enough that the probe
    only fires on the diagnostic event itself, not on the hot
    success path.
  - **A falsified hypothesis with a tight probe is a clean
    result, not a wasted run.**  We falsified
    cross-function-intern-non-determinism in one run because
    the probe was tightly gated.  That's far more useful than
    "we couldn't tell" from an ambiguous probe.
  - **Don't conflate fragility with feature gaps.**  CHANGE-CLASS
    being a stub looks like a fragility bug if you only see the
    test fail.  Treat them as separate tracks; debugging
    intermixed is a mess.
  - **Per-call-site code growth has been the limiting factor on
    several proposed fixes.**  Single mem-ref change → 159-test
    regression.  When the right fix has high per-site cost,
    consider fast-path/trampoline structures or compile-time
    inference before resorting to "add bytes everywhere."
  - **The fragility-fuzzer's non-monotonic flip pattern is
    diagnostic.**  Tests that pass at N=0,7,8 but fail at N=1,2,4
    aren't "code grew, broke" — they're "an alignment-modulo
    coincidence fires at certain N."  That signature pointed
    directly at tag-collision bugs; without it I'd have been
    chasing optimization bugs for weeks.

## State of the repo at end-of-day

`git log --oneline | head -25` should show the chain of
fragility commits ending in `348ccb6 fragility: regression
markers + fast-path-fix sketch for bug 6`.  Tree clean.

`mvm/fragility-notes.md` is the running log (older entries first).
`mvm/fragility-open-problem.md` is the most-detailed
current-state doc.  This file is the shortest path to "what
do I do today."

If next week you find yourself reading this and the regression
markers 9130-9133 are passing, the family is presumed closed.
Run the fuzzer to validate; if any test still flips, follow §"How
to debug another one" with that test as the entry point.
