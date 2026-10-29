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

**DONE 2026-10-29.**  Measured first: `MODUS_DEFUN_INVENTORY` lists every DEFUN
each image compiles, and build logs now say `FILE:LINE` (`after:FILE:LINE` for
text a build script appends).  Across the 31 matrix images 92 definitions were
unreachable everywhere and are deleted; 186 seams carry `(declaim (overridable
NAME))`; the check is fatal by default (`MODUS_REDEF_CHECK=warn|0` to relax),
and a baked duplicate `STRING-UPCASE` fails the build naming both lines.  The
declaration is plain `overridable`, not `modus:overridable`: there is no MODUS
package in the image yet (step 4).  Found on the way: `%FORM-MENTIONS-P` was
two functions with one name, and the UEFI image had never run a build check.

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

**PART DONE 2026-10-29: no call relies on the bare-name fallback.**  Measured
with a host-side census (every *FUNCTIONS* lookup that missed its qualified key
and resolved by bare name): **866** distinct pairs in the x64 CLI.  Nearly all
were the build compiler's OWN doing -- the generated SFT seeding read every
`#'NAME` in MODUS.MVM, DEFVAR init thunks were compiled under bare strings but
called as MODUS.MVM symbols, DEFSTRUCT and the SETF-writer / EMIT-BYTES
expansions interned their generated names in MODUS.MVM whatever package the
form was read in.  The rest were the x64 JIT glue calling across MODUS.MVM /
MODUS.ASM / MODUS.MVM.X64, fixed on the HOST side (an export, imports in
lib/load-mvm.lisp) because image source cannot write `modus.asm:x` until the
packages exist in the image (step 4; the self-host image re-reads its source).
All 31 images: 0.  The census is now `CHECK-CROSS-PACKAGE-CALLS`
(mvm/build-checks.lisp), fatal by default (`MODUS_PKG_CHECK=warn|0`); a
MODUS.ASM-read call to a MODUS.MVM function fails the build.  The fallback
itself stays for FORWARD references (the caller's own package defines the name
later), which the check allows.  Emitted code: only the changed compiler
functions and the generated tables moved (function sizes compared; the SFT
tables +0.06%).

**GLOBALS DONE 2026-10-29.**  Globals and constants are still STORED by name
hash (in the compiler and at runtime), so true per-package storage is step 4's
runtime identity; what step 2 needed was that no reference crosses packages by
accident.  `CHECK-CROSS-PACKAGE-GLOBALS` records every global/constant
definition and reference as (package, name): the x64 CLI had 18 cross-package
references and 2 names defined in two packages, all the x64 JIT glue (the
co-init, read in MODUS.ASM, setting translator variables; mvm-eval's bare
DEFVARs of two translator variables).  Host imports fixed them; all 31 images:
0 and 0, fatal by default (`MODUS_GLOBAL_PKG_CHECK=warn`), and a MODUS.ASM-read
reference to `*MACRO-TABLE*` fails the build.  **Recording defpackage /
in-package moved to step 4**, where creating the packages in the image is its
consumer; recorded now it would be data nothing reads.

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

**Cycle 1, 2026-10-29: the NIC interface and the NE2000.**
- `mvm/kernel-packages.lisp` is the one list of step-3 packages: the build host
  loads it, and %INIT-PACKAGES makes the same packages in every image (and
  %KERNEL-HOME-PKG-NAME-P counts them as the kernel's).
- `MODUS.NET` (`net/nic.lisp`) holds the NIC interface; ip.lisp, ssh.lisp,
  tcp-conns.lisp and the pipeline call `modus.net:nic-*`.  The seam mechanism
  is %DEFINE-SEAM (cl-eval.lisp): the interface function calls through a
  cell, and SET-SYMBOL-FUNCTION / FDEFINITION / SYMBOL-FUNCTION treat a seam
  name as that cell, so the user's (setf fdefinition) binding reaches
  ahead-of-time callers on targets with no entry pad to patch.
- `net/ne2000.lisp` is package `MODUS.NET.NE2000`, exporting NE2K-SEND,
  -RECEIVE, -RX-BUF, -PROBE; the i386 image binds the interface to them.  Its
  board hooks (NE2K-RX-HOST, E1000-STATE-BASE) stay MODUS-INT until the board
  package exists.  The other images bind the interface to the MODUS-INT
  E1000-* functions their drivers still define.
- Exported names keep their driver prefix (NE2K-SEND, not SEND): every kernel
  package still registers its functions under the BARE name too, so two
  packages' SEND would collide.  Per-package function identity for the
  kernel's sub-packages is its own piece of work.
- Measured: x64, aarch64 and i386 SSH images answer `(* 6 7)` with the
  interface bound to the expected driver; build checks unchanged.

**Cycle 2, 2026-10-29: the E1000 and virtio drivers.**  `net/e1000.lisp` is
MODUS.NET.E1000 and `net/virtio-net.lisp` MODUS.NET.VIRTIO.  The drivers
`:use` MODUS-INT, which exports to them the console printers, PCI config
access and the board's address hooks (%MODUS-INT-DRIVER-EXPORTS, on host
and image alike) -- those move to MODUS.BOARD with the boards.  Virtio's
"wrap the E1000 functions by last-defun-wins and dispatch per call" is
gone: the x64 image binds NIC-PROBE to %PICK-VIRTIO-OR-E1000, which rebinds
the other seams to the virtio functions when the device answers.
ip.lisp's register dump calls the new NIC-READ-REG.  Measured: i386
(NE2000), x64 with an E1000, x64 with virtio-net-pci and aarch64 (E1000)
answer over ssh, each with NIC-SEND bound to its driver.  Left in MODUS-INT:
the boards' USB NICs (cdc-ether, r8152, usb-netdev), which only the Pi
board image can run.

**Cycle 3, 2026-10-29: MODUS.CRYPTO.**  net/crypto.lisp, crypto-fast.lisp and
crypto-32-fast.lisp (the overrides stay in the package they override) read
into MODUS.CRYPTO, which :uses MODUS-INT for its byte-buffer helpers, its
scratch and the keep-alive seam.  It exports the 19 names anything outside
calls (SHA-256/512, ChaCha20, Poly1305, X25519, Ed25519's pieces); ssh.lisp,
hosted-ssh.lisp, isolated-cl.lisp, snp-ssh-attest.lisp and the tests name them
modus.crypto:..., until SSH gets a package that :uses it.  Measured: the
i386, x64 and aarch64 SSH images log in (key exchange, signing and cipher all
through MODUS.CRYPTO), test/crypto-sha-vectors.lisp 14/14 and
test/run-hosted-ssh.sh PASS on the hosted CLI.

**Cycle 4, 2026-10-29: MODUS.SSH.**  net/ssh.lisp reads into MODUS.SSH, which
:uses MODUS.CRYPTO (so its modus.crypto: prefixes are gone) and MODUS-INT.
MODUS-INT exports to it the TCP/IP stack it rides on, the connection and IPC
blocks, the line editor, and the shell hooks the image supplies (SSH-EVAL-LINE,
SSH-DO-EVAL-EXPR, SSH-HANDLE-CHANNEL-DATA -- build text or an adapter defines
them).  MODUS.SSH exports the 24 functions the boot text, ip.lisp and the
adapters call.  Its 16 declared seams are still replaced by later DEFUNs, now
written modus.ssh::NAME in net/hosted-ssh.lisp, isolated-cl.lisp,
ssh-x64-cl.lisp, hwrng-bcm2835.lisp and the build text: explicit, and checked
(redef-check 0), but not yet (setf fdefinition) interfaces.  Measured: the
i386, x64 and aarch64 SSH images and test/run-hosted-ssh.sh pass.

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

## Order, revised 2026-10-29

Step 2 found that image source cannot name a symbol of another build package
(`modus.asm:x` is a reader error in a running image, and the self-host image
re-reads its source), so every cross-package fix had to be a host-side import.
Step 3 done in that state would push all of net/crypto/ssh/board through the
same workaround.  **So the core of step 4 comes next** -- the build's packages
exist in the image at boot, internals live in MODUS-INT, the public API in
MODUS -- and step 3 follows, written with real qualified names and interfaces.
(User, 2026-10-29: "that sounds better than the previous plan".)

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

## Decided

- **Kernel internals get their own package, an `SB-INT` analog (user,
  2026-10-29: "we should have our own sb-int analog").**  `MODUS-INT` holds the
  runtime's internal names that other Modus code shares (the %-functions, the
  seams, the board/net/thread plumbing); they leave `CL-USER`.  `MODUS` stays
  the public extension package, `SB-EXT`'s analog (`modus:share`,
  `modus:make-channel` already live there).  A REPL or script reaching an
  internal writes `modus-int::name`, as an SBCL user writes `sb-int:...`.

- **Seams are bound by `(setf fdefinition)` at boot (user, 2026-10-29),** not
  by a compiler feature: `modus.net:nic-send` is an ordinary function whose
  definition the image's composition sets from the driver's.  (Open question
  2, closed.)
- **The runtime gets its own package, MODUS.RUNTIME (user, 2026-10-29)**:
  prelude and the cl-* files, the implementation of COMMON-LISP, with an
  export list of what the rest of the kernel calls.  (Open question 3,
  closed.)

## Step 4 phase B: kernel names stop being CL-USER names

**LANDED 2026-10-29 (functions; MODUS exports).**  As built, kernel functions
keep their BARE keys (every internal by-name lookup -- compiler-emitted
runtime calls, the JIT/kernel-redirect tables, the boot-evaluated shim texts --
keeps working unchanged); instead a NON-kernel symbol whose name collides with
a kernel function (snapshot of the function table at boot,
`*KERNEL-FN-NAMES*`) is keyed `PKG::NAME`, so defining it creates its own
function.  Reads of such a symbol with no function of its own fall back to the
kernel's and are counted (`*KERNEL-NAME-FALLBACKS*`).  The design notes below
are the plan as first written.

Phase A (53772220) made the build packages exist in the image; every baked
internal symbol's home is MODUS-INT.  But the RUNTIME function table is keyed
by bare name, so a CL-USER `(defun e1000-send ...)` still replaces the kernel's
function.  Phase B:

1. Key image functions whose symbol is homed in a build package as
   `"MODUS-INT::NAME"` (SFT seeding, %SYM-NAME-OR-HASH, runtime compiles); CL
   functions stay bare.  A transitional read fallback (bare miss -> the
   MODUS-INT key) keeps unqualified callers working while a census counts them;
   it is removed once the count is zero, as step 2's fallback was.
2. `MODUS` (SB-EXT's role) exports the public API and CL-USER uses it.
   Proposed exports, from the census of what tests/scripts/docs call (to be
   revised by the user):
   - process/image: sys-exit, save-and-die, install-tarball (+ the existing
     jit-eager, jit-prune, jit-source, make-/free-static-vector)
   - actors: actors-spawn, actors-send, actors-receive, actors-receive-if,
     actors-self, actors-yield, actors-start, actors-stop, spawn-link, link
   - sockets: socket-connect, socket-listen, socket-listen-on,
     socket-listen-port, socket-accept, socket-send, socket-send-from,
     socket-recv, socket-recv-fully, socket-close, socket-local-port
   - servers/net: ssh-serve-tcp, ssh-serve-tcp-on, ssh-serve-vsock, ssh-boot,
     run-net-pipeline, http-fetch
   - profiling: pmu-start, pmu-stop, pmu-read, pmu-measure, pmu-profile,
     pmu-annotate
   Everything else (GC probes, console primitives, crypto, the compiler) stays
   in MODUS-INT and is reached as `modus-int::name`, as SBCL code reaches
   sb-int / sb-impl.
3. Qualify the remaining internal calls in test/, scripts/ and docs/ (218
   names, 163 files measured), then drop the fallback.
   **DONE for test/ and scripts/ (f785daa0):** every test/*.lisp gives the same
   exit status with the fallback on and off, and strict mode logs no hits.
   **And for docs/ and the helper scripts that send forms to a REPL; the
   fallback is deleted.**  Prose about kernel source keeps bare names: that
   source is read in MODUS-INT.
4. Globals and specials: the same, later (they are stored by name hash in
   compiled code, so this one moves bytes).
   **DONE without moving bytes:** baked code keeps its bare keys.  The build
   emits the kernel's global names (SCAN-GLOBAL-NAMES, beside the function
   table), and a non-kernel symbol with one of those names gets the
   package-qualified key the runtime already used for runtime-born packages
   (task #241's %SYM-GLOBAL-PKG-KEY / %GLOBAL-VAR-PKG-KEY).  The census
   (*KERNEL-GLOBAL-SHADOW-P* NIL) found 9 tests, the Pi core driver and the
   board helpers; MODUS exports *JIT-ON*, *JIT-HOT-ONLY*, *JIT-HOT-CALLS*.

## Open questions

1. **What exactly does `MODUS` export** beyond today's thread/channel API?
   (Its role is settled above: the `SB-EXT` analog.)
2. Should **step 3 static seam bindings** be a compiler feature (a direct call,
   zero cost) or plain `(setf (fdefinition …))` at boot (simple, one indirect
   call)?  Hardware drivers argue for static.
3. **Does the runtime stay one package?**  Splitting `cl-*` buys little, since
   it *is* `COMMON-LISP`, and costs the most churn.
