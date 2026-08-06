# GATE-RESULT — SSH cells migrated into `mvm/build.lisp`, scripts retired

Branch `ssh-cells`, worktree `/home/claude/ws-ssh`, base `main` @ `0730ad4`.
Unpushed, not merged.

## Headline

**`mvm/build-*.lisp` count: 33 → 28.**  All five SSH/actors cells migrated,
every one gated on a **byte-identical** image, every retired script `git rm`ed.

## Per-cell result

Method, per cell: build with the OLD script from a clean tree, `md5sum`, stash
the image; flip the cell to `:native`; build with `sbcl --script mvm/build.lisp
<cell>`; `md5sum`; compare.

| cell | retired script | md5 before (legacy script) | md5 after (build.lisp cell) | result | retired |
|---|---|---|---|---|---|
| `x64/bare/qemu/ssh` | `mvm/build-x64-ssh.lisp` (228 L) | `782c7414cf3ad555c302500942004a8c` | `782c7414cf3ad555c302500942004a8c` | **MATCH** | yes |
| `aarch64/bare/qemu/ssh` | `mvm/build-aarch64-ssh.lisp` (132 L) | `a168c3fe76e313cf9c644b7b5af0ac3d` | `a168c3fe76e313cf9c644b7b5af0ac3d` | **MATCH** | yes |
| `aarch64/bare/qemu/actors` | `mvm/build-aarch64-actors.lisp` (127 L) | `98cc10becd7a7da404ba700d86fd6ce2` | `98cc10becd7a7da404ba700d86fd6ce2` | **MATCH** | yes |
| `aarch64/bare/qemu/isolated` | `mvm/build-aarch64-isolated.lisp` (133 L) | `2083abab509dca80f930a36cffee9a5b` | `2083abab509dca80f930a36cffee9a5b` | **MATCH** | yes |
| `i386/bare/qemu/ssh` | `mvm/build-i386-ssh.lisp` (313 L) | `dcadafd144d94459e6aac9302998c5f9` | `dcadafd144d94459e6aac9302998c5f9` | **MATCH** | yes |

5 / 5 byte-identical.  No cell was retired on anything weaker than a matching
md5, and no cell is left `:legacy` "for now".

### Second, post-deletion rebuild (alias path)

After `git rm` of all five scripts, rebuilt two cells **through the retired
script name** — the alias convention established by commit `0730ad4`:

```
sbcl --script mvm/build.lisp build-i386-ssh  -> dcadafd144d94459e6aac9302998c5f9  (same)
sbcl --script mvm/build.lisp build-x64-ssh   -> 782c7414cf3ad555c302500942004a8c  (same)
```

So: the alias still resolves with the file gone, and the images are
deterministic across independent rebuilds (not a one-run coincidence).

## What had to be designed (why these were deferred)

The three already-migrated REPL cells were four values: translator, `:TARGET`,
output path, `*repl-source*`.  These five are not.  Each one:

1. composes a per-arch list of `net/` source files, where **order is semantic**
   (last-defun-wins — `crypto-w32.lisp` after `crypto.lisp` *is* the override
   mechanism, and `mvm-ssh-fixes.lisp` must be last on x64);
2. splices an inline `kernel-main` written as Lisp source strings, deliberately
   chopped into `km-init-crypto` / `km-init-net` / … to stay under the
   per-function sequential-form limit;
3. may set a translator special (`*aarch64-setup-irq-enable*`,
   `*aarch64-sched-lock-addr*`);
4. concatenates those parts **in a per-cell order** that is itself load-bearing.

So `build.lisp` grew a `*COMPOSITES*` table rather than a fifth positional
column:

```
:net    net/ file names in load order (transcribed verbatim)
:main   kernel-main source lines (the old script's `ssh-main' list)
:extra  a further raw chunk appended after :MAIN (i386 only)
:parts  concatenation order of :NET / :REPL / :MAIN / :EXTRA
:flags  MODUS.MVM specials set after the translator is installed
```

`:parts` is the non-obvious one and had to be preserved per cell: the
single-threaded SSH builds are `(:net :repl :main)` — kernel-main LAST so
last-defun-wins makes it the entry point — while the actor builds are
`(:main :net :repl)`.  Getting that wrong still *builds*; it just boots the
wrong entry point.  The md5 gate is what makes it safe to assert.

**Real dedup found:** `actors` and `isolated` have byte-identical kernel-main
text (verified by `diff` of the two source ranges); they differ only in that
`isolated` appends `net/isolated-net.lisp`.  They now share one
`*aarch64-actor-main*`.  That duplication was invisible while it lived in two
files.

The kernel-main line lists were **extracted mechanically** (`sed` over the
exact line ranges of the old scripts), not retyped, so transcription error was
not on the table.

### One real bug fixed on the way

`install-translator`'s `:i386` branch interned `INSTALL-I386-TRANSLATOR` in
`:modus.mvm`, but `translate-i386.lisp` does `(in-package :modus.mvm.i386)`.
That path had never been exercised (no i386 cell was `:native`), and it would
have failed with an unbound-symbol FUNCALL the moment one was.  Fixed; the
`i386/bare/qemu/ssh` cell above is the proof it now works.

## Referrers repointed

Every hit of the five script names across `.sh`, `.lisp`, `.md`
(`grep -RIn`, which unlike `-r` does follow symlinked dirs):

- `scripts/run.sh` — BUILD column now holds **cell keys**, not filenames
  (`x64/bare/qemu/ssh`, `aarch64/bare/qemu/{ssh,actors,isolated}`,
  `i386/bare/qemu/ssh`), consistent with the convention from `0730ad4`.
- `scripts/run-x64-ssh.sh` — build command **and** the staleness test
  (`[ mvm/build-x64-ssh.lisp -nt "$BIN" ]` → `mvm/build.lisp`).
- `scripts/run-aarch64-ssh.sh`, `scripts/run-i386-ssh.sh`,
  `scripts/run-aarch64-actors.sh` (comment).
- `test/features/aarch64-{ssh,e1000,actors,http,isolated}.sh`.
- `CLAUDE.md`, `mvm/BUILDS.md`, `docs/networking.md`, `docs/i386.md`,
  `docs/aarch64.md`.
- `mvm/build-i386-diag-ssh.lisp` (comment).

After the sweep, `grep -RIn` for the five retired filenames returns exactly
**one** hit outside this report: a prose mention in `net/arch-x86.lisp`'s
header comment. That one is left stale **on purpose** — see below; it names a
build that is still reachable under that name as an alias, so it misleads
nobody.

## Regression safety

Files touched: `mvm/build.lisp`, the five retired scripts, and the referrers
above. **No shared file** (`compiler.lisp`, `prelude.lisp`, `cl-*.lisp`,
`translate-*.lisp`, `net/*.lisp`) is modified, so no ANSI gate run is required
and none was run — nothing on the x64 gate path was touched.

### Why `net/arch-x86.lisp` was deliberately left alone (measured, not assumed)

Its header comment still says `(build-x64-ssh.lisp)`. I edited it, rebuilt, and
**the image changed**: `782c7414…` → `31bdcccf…`, 2264640 → 2264648 bytes, with
the combined source string 10 chars longer — exactly the length of my comment
edit. Cause: `build-image` calls `embed-source-blob` (`mvm/cross.lisp:336`),
which bakes the **entire source text verbatim**, comments included, into the
kernel image. Reverting the comment restored `782c7414…` exactly.

Two consequences, both worth writing down:

1. A comment-only edit to any `net/` file *does* change every shipping image
   that bakes it. That is not a reason to never edit comments, but it is a
   reason not to do it inside a change whose entire warrant is byte-identity.
   So the edit is reverted; the one-line doc rot is the cheaper defect.
2. **The md5 gate in the table above is far stronger than it looks.** Because
   the source blob is embedded, byte-identity means the `*COMPOSITES*` table
   reconstructs the old scripts' combined source string *character for
   character* — every `net/` file, in order, every kernel-main line, every
   separator newline. A reordered `net/` list or a dropped blank line could not
   have produced a matching md5.

## Not done / honest boundary

- **Build-only proof.** Every claim here is "the image is byte-identical to
  what the retired script produced". These images were not booted in QEMU as
  part of this work — they do not need to be, because bit-for-bit identity to
  the previously shipping artifact is a *stronger* statement than a smoke test,
  but it is also *only* that statement. `BUILDS.md`'s standing warning applies:
  BUILD status is not RUN status.
- `net/mvm-ssh-fixes.lisp`, `net/ssh-profile.lisp` and friends are still
  per-cell `net/` lists rather than named profiles. Turning "the x64 SSH stack"
  into a named, reusable set is a further consolidation, deliberately not
  attempted here — it would change source *content*, which forfeits the
  byte-identity gate that makes this change safe.
- The remaining `:legacy` cells (RPi family, pizero, uefi, diag-ssh, the CLI
  and ANSI builds) are untouched. The RPi family is still blocked on the shared
  `A64-BUFFER` type error, not on migration.

## Reproducing

```sh
# legacy side (from main @ 0730ad4)
sbcl --dynamic-space-size 8192 --script mvm/build-aarch64-ssh.lisp
md5sum /tmp/modus-aarch64-ssh.bin

# native side (this branch)
sbcl --dynamic-space-size 8192 --script mvm/build.lisp aarch64/bare/qemu/ssh
md5sum /tmp/modus-aarch64-ssh.bin
```
