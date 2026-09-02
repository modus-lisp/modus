# The 512-frame handler stack disables the per-thread handler stack

**Reporter:** the threads/glass branch (`fix-bq-macroexpand`, `746236b`)
**Against:** `main` at `0b7dc88` — specifically `b490cfb`, *"x64-Linux: port the
BALANCED-CAP to the per-fork handler stack + 512 frames at 0x10001000"*
**Severity:** silent. A mechanical merge builds, passes the ANSI acceptance
gate, and reintroduces a bug that took this campaign four rounds to find.

This is a **design collision, not a textual one**. Resolving the four conflicted
files by picking hunks will produce a working single-threaded modus and a
subtly broken multi-threaded one. The conflict markers are not where the
problem is.

---

## The two designs

| | address | frames | per-thread? |
|---|---|---|---|
| `main` `0b7dc88`, `*x64-linux-mode*` | `0x10001000` | 512 | no — one per process |
| `main` `0b7dc88`, bare metal | `0x10000408` | 64 | no |
| ours `746236b`, all modes | `0x10000408` | 64 | **yes** — FS-segment relative |

Ours moves the handler-frame stack into the per-thread window so that two
threads unwinding at the same time do not pop each other's frames. The window
is `+TLS-WINDOW-BASE+` = `0x10000000`; `%TLS-WINDOW-OFFSET-P`
(`mvm/compiler.lisp` ~397) claims offsets `0x400..0xC0F` (depth + 64 frames)
and `0xC10..0xC37` (longjmp scratch + self slot). A claimed access is emitted
with an FS override, so it reads *this thread's* copy.

## The failure

`0x10001000` is **outside the window**. Measured:

```
0x10000408   offset 0x408    claimed = T
0x10000C00   offset 0xC00    claimed = T
0x10001000   offset 0x1000   claimed = NIL     <-- theirs
```

The window ends at offset `0xFFF`. An unclaimed address gets an ordinary
process-global access — which is the deliberately safe direction for a *missed*
slot, and exactly the wrong direction here.

**And it reaches the shipping image.** `*x64-linux-mode*` is not gate-runner
only:

```
mvm/build-generic-cli.lisp:137   (setf modus.mvm.x64::*x64-linux-mode* t)
mvm/build-cli-common.lisp:422    (setq *x64-linux-mode* t)
```

So `./modus` — the binary the threading work runs on — takes the `0x10001000`
path. After a mechanical merge, every thread shares one handler stack again.

## Why this matters, concretely

The per-thread handler stack is not a refinement. It is one of the two things
that made threads usable for real Lisp, and its absence is what
`TRAP #x0511 — "MVM LONGJMP with no active handler-case"` looks like: a thread
unwinds, finds a frame another thread pushed, and longjmps into a dead stack.
That signature dominated this campaign for weeks.

The tests that catch it:

- **`test/hosted-mv-handler.lisp`** — 24 checks. Two threads, 400 iterations,
  forced collection every 25, each unwinding through `handler-case` while the
  other holds multiple values.
- **`test/hosted-mv-handler-unsync.lisp`** — the negative control, which
  deliberately skips the per-thread install. **It is probabilistic: ~90% per
  run.** One clean run of it means nothing; run it 6+ times.

Neither is in the ANSI gate. `acceptance-gate.sh` measures conformance
pass counts and crash markers over IDs 10001-27800 — it is a good gate and it
cannot see this, because the corpus is single-threaded.

## What we think is right

**The two designs compose. We are not asking for the 512 frames back.**

512 is clearly needed — the QL ladder and the ANSI suites nest handlers far
deeper than 64, which is presumably why `b490cfb` exists at all, and a
per-thread stack that overflows at 64 frames is useless for the same workloads.

The composition is: **the window holds a pointer to a per-thread 512-frame
region, rather than embedding the frames.** That is the same move already used
in this branch for the per-thread intern store and the dynamic-binding stack —
and it carries the same non-obvious requirement:

> **A per-thread region must be scanned by the collector, or its contents are
> invisible roots.** A raw per-thread word is *not* a GC root; the precise root
> set is the fixed list in `EMIT-GC-TRAMPOLINE`. See `EMIT-DYNBIND-ROOT-SCAN`
> in `mvm/translate-x64.lisp` for the shape — it walks exactly DEPTH value
> words through `EMIT-TLS-BASE`, and is emitted into both trampolines.

Without that scan, the frames hold pointers nothing traces, which is a
different silent bug in the same family.

## Addresses that are NOT free

While in this area: the range `0x10000C38..0x10000FFF` looks unclaimed as
*window offsets*, and is not free as *addresses*. These are process-global BSS
inside it, and giving a thread a private copy of any of them would hand every
worker its own copy of the collector's configuration:

```
0x10000DB8   threads-live gate (%RT-ENTER tests this)
0x10000DE0   lock acquisition counter
0x10000DE8   lock contention counter
0x10000DF0   arena base      \_ B-lite
0x10000DF8   arena limit     /
0x10000E00   bitmap page base
0x10000E18   object-start bitmap base
0x10000E40   cons-kind bitmap base
0x10000F08   active-region pointer (per-CPU array base)
0x10000FF8   per-CPU mode word
```

## Suggested acceptance for the merged result

1. `test/hosted-mv-handler.lisp` — 24 checks, and its `-unsync` control failing
   **6 of 6** (it is probabilistic; a single pass proves nothing).
2. `test/classify-thread-lisp.sh ./modus 100` — 100 pass / 0 hang. Hangs are a
   distinct class from deaths and must be counted separately.
3. A handler-nesting depth test at **>64 frames on a worker thread** — the case
   neither branch currently covers, and the one where these two designs
   actually meet.
4. `test/run-glass-serve.sh ./modus 3` — glass's own RFB server, 3 concurrent
   real VNC clients, 0 pixels differing. It exercises handler unwinding on
   per-client threads and is the end-to-end witness.
5. The ANSI gate, unchanged, from your side — **we cannot run it here.** See
   below; that asymmetry is why we are writing this down rather than resolving
   the merge ourselves.

## The corpus is not obtainable from this repo

Five scripts hard-code the corpus at an absolute path outside the tree:

```
scripts/build-ansi-runner.sh:16       TESTS_ROOT=/home/claude/modus-ref/ansi-test/tests
scripts/build-ansi-runner.sh:17       AUX=/home/claude/modus-ref/ansi-test/auxiliary/ansi_aux
scripts/build-ansi-file-runner.sh:12  AUX=/home/claude/modus-ref/ansi-test/auxiliary/ansi_aux
scripts/run-ansi-per-file.sh:24       TESTS_ROOT=/home/claude/modus-ref/ansi-test/tests
scripts/run-ansi-all.sh:11            TESTS_ROOT=/home/claude/modus-ref/ansi-test/tests
```

`/home/claude/modus-ref` **does not exist on this machine**, and no corpus is
present anywhere on it (`find / -name ansi-aux-macros.lsp` → nothing). So every
shard reports `expected/ran = 0`, and `acceptance-gate.sh` would compare two
zeroes and call it PASS.

**Nothing in the repo says where the corpus comes from** — no clone URL, no
fetch step, no note of which suite it is. `CLAUDE.md`'s ANSI Conformance
section reports 16,489/17,465 = 94.4% without stating the provenance of the
17,465. The layout (`tests/*.lsp`, `auxiliary/ansi-aux-macros.lsp`,
`auxiliary/ansi_aux`) is recognisably the GCL ANSI test suite, but that is an
inference from filenames, not something the tree asserts.

### The paths are absolute, and contexts run in separate containers

This is why the corpus "was absent" here and present for you: **same host,
different container, different checkout.** `/home/claude/modus/tmp/ansi-test`
and `/home/claude/modus-ref/ansi-test` are absolute paths that resolve inside
whichever container is running, so each context silently measures **its own
copy** of the corpus under the same name, with nothing in the tree recording
which copy that is.

**How close the two copies actually are — measured, after an earlier draft of
this section got it wrong.** A gate run here on `b60624a` gives
`base: passed=17501`; the other context reported `17521 → 17522`. That is a
**20-test difference, ~0.1%** — near-identical corpora, almost certainly
adjacent upstream revisions.

An earlier draft claimed the corpora were materially different, reasoning that
a passed count of 17,521 exceeded our **17,376 static `(deftest` occurrences**
and so had to come from a bigger corpus. That reasoning was wrong: our own base
run passes **17,501** against the same 17,376 static count, because a
meaningful fraction of the suite is **macro-generated** and never appears as a
literal `(deftest` in the source. Grepping `(deftest` undercounts the corpus
and cannot be used to compare two copies of it.

**What still stands:** the paths are absolute and each container has its own
copy, so an absolute count is only meaningful alongside the corpus revision it
came from, and `scripts/get-ansi-corpus.sh` prints that revision for exactly
this reason. `NET` is same-run and sound within one context — the other
context used it correctly.

**Two requests, both cheap and both worth more than this bug report:**

1. **Record the provenance** — a line in `CLAUDE.md` or a `scripts/get-ansi-corpus.sh`
   saying which suite, which revision, and where it came from. A conformance
   number whose corpus cannot be reconstructed is not reproducible, and the
   gate silently passes when the corpus is missing rather than refusing.
2. **Make the gate refuse a missing corpus.** `acceptance-gate.sh` should exit
   2 (infrastructure error) when `TESTS_ROOT` is absent or the aux files do not
   load, rather than reporting `NET = 0` and PASS. As written, a machine
   without the corpus cannot tell a clean merge from an unmeasured one — which
   is the same class of defect as everything else in this report: an unsafe
   state that is indistinguishable from a safe one.

## Merge-order note

Either order works, but they are not equally cheap. Ours has an end-to-end
acceptance chain that runs on this machine in minutes (`tx-cell` → `send-worker`
→ `glass-serve`), so a rebase of the 512-frame work onto ours can be *shown* to
still serve frames. The reverse — ours rebased onto `0b7dc88` — can only be
shown to still pass the ANSI gate on your machine. Whoever resolves should
have both, and only one of us has the corpus.

---

*Written from `746236b`, verified against `origin/main` at `0b7dc88` by trial
merge (4 conflicts: `mvm/compiler.lisp`, `mvm/interp.lisp`,
`mvm/translate-x64.lisp` ×2, `mvm/translate-aarch64.lisp`) and by direct
measurement of the window predicate. Nothing in this report is inferred from a
comment; the address arithmetic was run.*

---

## Our branch through the same gate

Run here, `scripts/acceptance-gate.sh b60624a 3110068`, 64 shards, same corpus
both sides:

```
base b60624a   passed=17501   CHUNK-CRASH=0   FILE-WEDGE=30
fix  3110068   passed=17499   CHUNK-CRASH=0   FILE-WEDGE=30
NET=-2   lost=9   gained=7                    VERDICT: PASS
```

**Markers unchanged in both timing-immune counters**, which is the part that
matters: 98 commits of per-region GC, native threads, sockets, an arena for
locked sections and per-thread dynamic binding cost **no crashes and no
wedges**.

Per the doctrine, the lost set, read rather than waved at:

```
lost    14593  21928 21929 21937 21938 21946  24276  26456 26457
gained  13500  21573 21944 21945  24465 24467 24503
```

Five of the nine losses and two of the seven gains sit in one narrow band
(`219xx`), flipping in **both** directions. Adjacent IDs moving both ways in a
single neighbourhood is the signature of shard timing, not of a behaviour
change — consistent with the ±200 absolute jitter this gate documents, against
a NET of −2 on 17,500 (0.01%).

**That is an inference, not a measurement.** The doctrine's actual remedy for a
negative NET — sub-shard the disagreeing range at `NSH=128+` and diff the
per-ID sets again — was **not run**. If −2 needs to be explained rather than
absorbed before merge, that is the run to do, and `21900-21999` is where to
point it.

---

## The merge happened, and the collision is now MEASURED, not predicted

`origin/merge-fix-bq` (`c8864da`) merges this branch with `main` at `ea12243`.
The merge itself is careful — five textual conflicts resolved by hand with
stated reasoning, and it caught a real bug in this branch (`GET-INTERNAL-RUN-TIME`
returned FLOOR's remainder as a second value; ANSI 26072/26073; fixed in
`c8864da` and cherry-picked here as `1552eb9`). **It resolved the text and
never engaged the design collision above.** Its handler push emits

```
add r11, 0x10001000          ; frame = depth*32 + 0x10001000   (main's address)
FS: mov [r11], rax           ; FS-relative store                 (our prefix)
```

and the geometry makes that worse than "process-global again":

```
%THR-TLS-BLOCK(cpu)  = page + 0x2000 + cpu * 0x1000     (stride 0x1000, contiguous)
FS base              = block - 0x10000000               (%TLS-INSTALL)
FS:[0x10001000]      = block + 0x1000 = %THR-TLS-BLOCK(cpu + 1)
```

**A worker's handler frames are written into the NEXT thread's window.**

Measured on a build of `c8864da`, `test/hosted-handler-neighbour.lisp` — main
reads two slots of cpu 2's window, one worker on cpu 1 nests 100
`handler-case`s and returns, main reads again; nothing is instrumented inside
the nesting, so the observation cannot move the bug:

```
                       merge c8864da              this branch e6285dc
cpu2 +0x640  (frame 50)  0 -> 730228544470          0 -> 0
cpu2 +0x1900 (frame 200) 0 -> 0   (control)         0 -> 0
                         FAIL 3 of 3               PASS 3 of 3
```

Every existing threaded test passes on the merge — `hosted-mv-handler` 24,
`hosted-thread-lisp` 29, `hosted-atomics` 31, `classify-thread-lisp` 100/0/0,
and glass's own RFB server delivers a correct frame — **because none of them has
three armed workers nesting handlers at the same time.** With cpu N's frames
over cpu N+1's MV buffer, handler depth and dynamic bindings, the failure is a
hang or a silent wrong value, never a clean fault; a live-victim variant of this
test hung to its timeout.

**Two instrument traps met on the way, recorded so the next person skips them:**
a `WITH-MUTEX` (or any unwind-protect-bearing macro) inside the nesting function
sends it to the interpreter, whose `handler-case` never touches the native frame
stack — the test then passes and proves nothing; and a 6-worker "deep nesting"
test passed because each victim only checked its window before its neighbour
had written into it. The committed test reads from main, after the join.

**Fix shape, unchanged from above:** the per-thread slot must actually hold the
512 frames. Either the window stride grows to cover `0x1000..0x4FFF` (16 KB of
frames after the 4 KB window, with `%TLS-WINDOW-OFFSET-P`, the page size at
344064, the thread table at `+0x13000` and the per-CPU blocks at `+0x14000` all
moved accordingly), or the window holds a pointer to a separately-mapped
per-thread frame region — and in either case the collector must be told about
the frames (`EMIT-DYNBIND-ROOT-SCAN` is the precedent). `hosted-handler-neighbour`
going 3-of-3 FAIL to 3-of-3 PASS on the merge is the acceptance.

**Still open on both builds, unrelated to the stack:** `run-glass-serve.sh`
reports client PASS (0 pixels differing) but server `rc=1` — a toplevel form in
the harness's post-serve "nothing left behind" section (the `inet-socket` fd
probe) dies with a swallowed `TYPE-ERROR` after the frame is delivered. Harness,
not glass, not the frame; not yet diagnosed.
