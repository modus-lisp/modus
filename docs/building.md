# Building modus with modus

A modus release can build modus. Given a released binary and this source tree, the build
gives the same bytes, whether the host is that binary on Linux, the Windows one,
or SBCL, CCL or ABCL. Nothing else is needed: no compiler, no C toolchain, no network.

Released binaries live in the dist repository, `modus-lisp/dist`, under `bin/RELEASE/`:

| file | what it is |
|---|---|
| `modus-linux-x64` | the modus CLI for Linux x86-64 (`mvm/build-generic-cli.lisp`) |
| `modus-windows-x64.exe` | the same image as a Windows PE32+ console program (`mvm/build-win64-cli.lisp`) |
| `modus-builder-linux-x64` | the CLI with the image-building tooling in it (`MODUS_BUILDER=1`): this one runs builds |
| `modus-builder-windows-x64.exe` | the same, for Windows |
| `SHA256SUMS` | every file's SHA-256 |
| `RELEASE.md` | the source commit, the build ID, and the commands that made each file |

The builders are the plain CLI plus the targets, `build-image`, the boot descriptors and the
AArch64/RV32 translators (`mvm/selfhost-tooling.lisp`, `mvm/builder.lisp`), built with narrow
strings (`MODUS_NARROW_STRINGS=1`, a byte per character where a string's characters fit), which
keeps a build's working set in the heap.  Use the plain CLI as your Lisp; use a builder to build.

## Rebuild a release and check it

From a checkout of the release's commit (`RELEASE.md` names it):

```sh
C=$(git rev-parse HEAD)                    # or the commit RELEASE.md names
export MODUS_SOURCE_COMMIT=$C              # recorded in the image (else it asks git)
export MODUS_BUILD_ID=$((16#${C:0:7}))     # the saved-core ID (else a fresh random one)
export MODUS_FASL=0                        # compile everything; no FASL cache

# Linux
MODUS_CLI_OUT=modus ./modus-builder-linux-x64 --dynamic-space-size 16384 \
  --script mvm/build-generic-cli.lisp
MODUS_WIN64_OUT=modus.exe ./modus-builder-linux-x64 --dynamic-space-size 16384 \
  --script mvm/build-win64-cli.lisp
MODUS_BUILDER=1 MODUS_NARROW_STRINGS=1 MODUS_CLI_OUT=modus-builder \
  ./modus-builder-linux-x64 --dynamic-space-size 16384 --script mvm/build-generic-cli.lisp

sha256sum modus modus.exe modus-builder    # compare with SHA256SUMS
```

On Windows, run the same scripts with `modus-builder-windows-x64.exe` (`set NAME=value` in
`cmd`, `$env:NAME="value"` in PowerShell).  Every build reads the tree it runs in, so run it
from the checkout's root.  A build takes 15-25 minutes and about 16 GB of address space.

**Both settings matter for the hash.**  `MODUS_SOURCE_COMMIT` replaces a `git rev-parse` and a
dirty-tree check the image records (`(modus:image-configuration)` prints them back);
`MODUS_BUILD_ID` is the identity a saved core checks before it restores, so it must differ
between releases, and deriving it from the commit does that.

## With another Common Lisp

The same scripts build the same bytes on SBCL, CCL and ABCL:

```sh
sbcl --dynamic-space-size 12288 --script mvm/build-generic-cli.lisp
scripts/build-ccl.sh mvm/build-generic-cli.lisp
scripts/build-abcl.sh mvm/build-generic-cli.lisp
```

They are interchangeable at every stage: SBCL can build a builder, that builder can build the
next, and CCL can check the result.

## What a build reads

The tree it runs in, the `MODUS_*` environment variables (each is listed in the image:
`(modus:image-configuration)`), and nothing else: no network, and no files outside the tree.
The hosted CLIs need nothing from `modus-lisp/vessel`; the bare-metal images do, through
`MODUS_VESSEL` or `.vessel/`.

## What is not here yet

- **macOS.**  The Darwin image (`mvm/build-aarch64-cli.lisp` with `MODUS_DARWIN=1`) is built
  by modus, but the native wrapper around it (`host/macos/`) is C and needs Apple's `clang`
  and `ld`.  A Mach-O writer in Lisp, with an ad-hoc code signature, is the next step.
- **AArch64 builders.**  The builders are x86-64; an AArch64 one (Linux, then macOS) follows
  the same recipe with `mvm/build-aarch64-cli.lisp`.

## How it stays reproducible

The compiler's output depends only on the source and the `MODUS_*` settings, never on the
host Lisp.  The rules that keep it that way, each learned from a byte that differed:

- The image's source is read with a fixed feature list and modus's own backquote, never the
  host's (`mvm/cross.lisp`).
- Every `MODUS_*` read is recorded, and the recorder is loaded before the first read.
- A builder resets its compiler to the state SBCL's has at the build call
  (`*BUILDER-FRESH-STATE*`), and an `EVAL` during a build leaves the compiler as it found it.
- The host's own compiles (its JIT, `EVAL`, `LOAD`) keep the host's settings when a build
  script sets the same variables for its target (`*JIT-HOST-FLAGS*`).
- Build scripts set compiler variables through `HOST-SYMBOL`, never a bare `FIND-SYMBOL`.

To find a new difference: build both with `MODUS_SYMMAP=FILE` and compare function names and
sizes, then compare `(getf (modus:image-configuration :data t) :build)` from each image.
