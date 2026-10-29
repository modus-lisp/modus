# Builder packages: image code in real packages

**Status:** plan, 2026-10-28.  Nothing below is built yet.

## The problem, as it stands today

An image is one concatenated source string compiled by the build-time compiler
(`mvm/compiler.lisp`).  Three things about that are not CL:

1. **Last definition wins, silently.**  A second `DEFUN` of a name replaces
   the first everywhere, including in callers compiled before it.  The build
   prints `NOTE: redefining` and moves on.  Measured on today's bare-metal
   images: **119** redefinitions (x64 SSH), **105** (i386 SSH), **88** (Pi REPL).
   Some are deliberate seams (`ne2000.lisp` replacing the `e1000-*` wrappers,
   `crypto-fast` replacing `crypto`, `rtest`'s `DO-TESTS`); some are accidents
   that happened to be harmless (`NAME-EQ` and `COMPUTE-NAME-HASH` are each
   defined twice in the x64 image).  Nothing tells the two kinds apart, and
   `mvm/build-checks.lisp` has no duplicate check.
2. **Packages are read but not compiled.**  Since #211 the build reader honours
   `in-package` (each file is wrapped to start and end in `MODUS.MVM`,
   `cross.lisp:2017-2042`), and `*functions*` registers a `PKG::NAME` key
   beside the bare one (`compiler.lisp:2958-3108`).  But the compiler treats
   `in-package` / `defpackage` / `export` / `import` as no-ops
   (`compiler.lisp:29264-29283`).  It also falls back to the bare key, and keys
   globals and constants by name hash alone (`normalize-name`).  The emitted
   native function table and the runtime `*symbol-function-table*` are keyed
   by the bare name.  And nearly everything is read in `MODUS.MVM` anyway:
   none of the `cl-*` runtime files and none of `net/` declare a package.
3. **Baked names have the wrong home at runtime.**  `MODUS.MVM` does not exist
   in a running image, so a baked implementation symbol lands in `CL-USER`
   (`prelude.lisp` ~4210, re-homed by `cl-packages.lisp:1191`).  The kernel's
   internals share a namespace with the user's REPL: a user's `(defun
   e1000-send …)` at the prompt replaces the NIC driver.

The goal: **an image is built the way a CL program is loaded.**  Files are read
into declared packages, a definition's identity is (package, name),
redefinition is an explicit act, and at runtime every baked symbol has the
home package it was read in.

## Constraints any step must respect

- **Gate each step** with `scripts/acceptance-gate.sh`: every image builds, the
  web image boots, and the x64 ANSI sweep has 0 lost.  Steps 1-2 should be
  per-function identical (`scripts/fndiff.py`); later steps are not, and must
  be judged by the gate plus their own tests.
- **The DDC rule:** every emission-affecting host flag must also be set in
  `build-modus-selfhost`'s `--compile` / `--compile-uefi` blocks.  Run
  `kiln image x64-uefi --ddc` before merging anything that changes emitted
  code.
- **Bare-name probes:** the runtime looks functions up by bare name hash
  (`KERNEL-MAIN`, `%GC-COLLECT`, `GENERIC-ADD`, `%MVM-RESOLVE-RUNTIME-FN`, the
  JIT's linkage stubs, save-and-die cores).  Qualifying the table changes every
  one of those lookups.
- **Symbols across actors** (user decision 2026-10-23): `TERM-ENCODE` sends
  package + name.  A symbol that moves packages must still decode to the same
  symbol on the receiver, and the region-0 audits must stay green.
- **Static literals:** `%STATIC-SYMBOL-REF` keys are (name-hash, host package
  name hash).  If the host package changes, the slot changes too.  That is
  correct, but it moves bytes.

## Steps

### 1. Make every redefinition explicit (no packages yet)

The cheapest step, and the one that removes the silent half of the problem
without moving anything.

- Add `(declaim (modus:overridable NAME…))`, a build-time declaration the
  compiler records.  It goes in the file that defines the default (the seam).
- Make `mvm-compile-function`'s redefinition branch (`compiler.lisp:28968`)
  check it.  Redefining an overridable name is fine and is logged.  Redefining
  anything else is a build error that names both source lines.  A knob
  (`MODUS_REDEF_CHECK=warn`) gives a migration period.
- Classify the ~120 per image.  A deliberate seam gets a declaration.  An
  accident (two identical `NAME-EQ`s) gets one copy deleted.  A shadowing that
  only works by luck becomes a seam or a rename.

**Acceptance:** every gate image builds with the check fatal.  Emitted code is
per-function identical, because declarations emit nothing.  Inserting a
duplicate `defun` fails the build (the negative control).  This alone answers
"you cannot accidentally replace a function in an image".

### 2. A real package graph at build time

Make the packages that the reader already honours mean something to the
compiler.

- Compile `defpackage` / `in-package` at build time.  Today they are host-only
  SBCL packages; they must also be recorded so step 4 can create them in the
  image.
- Key **globals and constants** by (package, name), as functions already are.
- Drop the bare-name fallback in `%fn-info-for-key` one package at a time.  A
  call that crosses packages must name the symbol it means, either by
  qualification or by `:use` / `:import-from`, exactly as in CL.  Its current
  job ("nothing that resolved before stops resolving") becomes a measured
  census: log each fallback hit, drive the count to zero, then delete the
  fallback.
- Keep `function-info-name` bare for now: the runtime table is step 4.

**Acceptance:** zero fallback hits in every image, and a deliberately
unqualified cross-package call fails the build.

### 3. Put the code into packages

Proposed layout; the names are a proposal:

| package | contents |
|---|---|
| `MODUS.MVM` | compiler, interpreter, JIT, GC (as today) |
| `MODUS.RUNTIME` | `prelude`, the `cl-*` files: the implementation of `COMMON-LISP` |
| `MODUS.NET` | `ip`, `http-client`, TCP; exports the NIC interface `nic-send`, `nic-receive`, `nic-mac`, … |
| `MODUS.NET.E1000`, `.NE2000`, `.R8152`, `.VIRTIO`, `.CDC` | one per driver; each implements the NIC interface |
| `MODUS.CRYPTO`, `MODUS.SSH` | as named |
| `MODUS.BOARD` | arch adapters (`arch-*-cl`), consoles, GPIO, HDMI |
| `MODUS` | the user-facing API, `:use`d by `CL-USER`: `ssh-boot`, `run-net-pipeline`, `led-on`, `make-channel`, `sys-exit`, … |

**Seams become interfaces.**  "`ne2000.lisp` redefines `e1000-send`" becomes
"`MODUS.NET`'s `nic-send` is bound to `MODUS.NET.NE2000:send` for this image".
The binding happens in the image's composition step 5, not by load order.
Two mechanisms, chosen per seam:

- a **static binding** (`(modus:bind-seam modus.net:nic-send modus.net.ne2000:send)`
  at build time), which the compiler turns into a direct call: zero runtime
  cost, the right default for hardware;
- a **dynamic hook** (a function-valued global) for the few seams that really
  change at runtime (consoles that come and go, `%core-post-restore`).

Order the work by blast radius: drivers first (small and self-contained; each
one is a gate cycle), then crypto/SSH, then the board, and the runtime last.
The runtime is mostly implementing `COMMON-LISP` and may legitimately stay one
package.

**Acceptance per package:** gate PASS; the step-1 overridable list shrinks
(each seam turned interface leaves the list); `run.sh x64-ssh` / `i386-ssh` /
`aarch64-ssh` answer; the Pi board image boots on hardware for the board
packages.

### 4. Runtime identity: every baked symbol keeps its home package

- Create the step-2 package graph in the image at boot, from data the build
  emits, before `init-all-globals`.
- Key the native function table and `*symbol-function-table*` by symbol,
  i.e. (package, name).  Port every bare-name probe (list above) to a qualified
  one.
- `(symbol-package 'modus.net:nic-send)` is `MODUS.NET`; a user's `(defun
  e1000-send …)` in `CL-USER` creates `CL-USER::E1000-SEND` and touches nothing.
- `TERM-ENCODE` keeps working unchanged, because it already sends the package
  name; it now sends the right one.

This is the step that changes user-visible behaviour.  Kernel names stop being
`CL-USER` names, so anything typed at a REPL or loaded from a script that calls
an internal must use the `MODUS` export or qualify the name.  Census first: grep
`test/`, `docs/`, `run.sh` expressions and the ladders for unqualified internal
calls.

**Acceptance:** the ANSI sweep (packages chapter especially), `test/hosted-*`,
the actor/region audits (`run-worker-xregion.sh`, `run-term-xregion.sh`,
`run-intern-collect.sh`), save-and-die round trips on x64 and the Pi, and the
DDC.

### 5. Images as systems

With steps 1-4 done, an image's source list stops being a hand-ordered string
concatenation in two 3000-line build files.  It becomes a **system
definition**: components with dependencies and per-image seam bindings,
read by the existing `defsystem` parser host-side.  The build compiles that
system; `build-cl-repl-common.lisp`'s `%rpi-net-text` splices and the
`*cli-bare-metal-*` slots turn into component lists.  This is also the natural
shape for the "suite as a `--load`-able script on a clean image" end state, and
for the gadget/bootloader ports, which become one more driver package.

## Order and size

| step | changes emitted code? | rough size | blocks |
|---|---|---|---|
| 1 explicit overrides | no | 1-2 gate cycles; ~120 classifications × ~45 images (mostly shared) | nothing |
| 2 build-time package graph | globals keying moves bytes | 2-3 cycles | 1 |
| 3 code into packages | yes, per package | one cycle per package group, ~8 | 2 |
| 4 runtime identity | yes; user-visible | the big one: 3-5 cycles plus census | 3 for whatever it exposes |
| 5 images as systems | build scripts only | 2-3 cycles | 3 |

**Recommendation:** do step 1 now.  It is small, emits nothing, and turns the
one unsafe property, silent replacement, into a build error.  Steps 2-4 are
what make it CL, and step 4 needs a decision first: should kernel internals
leave `CL-USER`, and what goes in the exported `MODUS` API?

## Open questions

1. **Is `MODUS` the right user-facing package?**  What belongs in it: the
   board, net and thread APIs?  SBCL's analogue is `SB-EXT`, which modus
   already shims.
2. Should **step 3 static seam bindings** be a compiler feature (a direct call,
   zero cost) or plain `(setf (fdefinition …))` at boot (simple, one indirect
   call)?  Hardware drivers argue for static.
3. **Does the runtime stay one package?**  Splitting `cl-*` buys little, since
   it *is* `COMMON-LISP`, and costs the most churn.
