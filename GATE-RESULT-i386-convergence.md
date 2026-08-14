# GATE-RESULT-i386-convergence — the i386 CLI joins x64's bring-up, and the first 32-bit library measurement

Branch `converge-i386-cli` in `/home/claude/ws-i386`, unpushed, off `main` @ `cb8d9c7`.

| commit | what |
|---|---|
| `ba693fa` | `build-cli-common`: admit `:i386`; new `*CLI-ARCH-IO-SCRATCH-SOURCE*` slot |
| `b2a5f9c` | Converge `build-i386-cli`: **2145 → 384 lines**, one lineage |
| `7f177f9` | `build.lisp` / `BUILDS.md`: drop the retired `MODUS_I386_LAYER` |

---

## 1. x64 and aarch64 did not move — proven by bytes, not by a gate run

| image | from | bytes | md5 | verdict |
|---|---|---|---|---|
| `./modus` (x64) | `main` @ `cb8d9c7` | 37,960,175 | `a7a019c48c4156db78a1fea628651176` | baseline |
| `./modus` (x64) | this branch | 37,960,175 | `a7a019c48c4156db78a1fea628651176` | **`cmp` clean** |
| `modus-aa64-cli` | `main` @ `cb8d9c7` | 59,382,641 | `c53bf8ce2791ddfdad1b9be8910f7d16` | baseline |
| `modus-aa64-cli` | this branch | 59,382,641 | `c53bf8ce2791ddfdad1b9be8910f7d16` | **`cmp` clean** |

Both wrappers' assembled blobs were also compared directly
(`MODUS_DUMP_FULL_SOURCE`, a ~20 s check rather than a ~20 min build): x64 and
aarch64 blobs are **byte-identical to `main`**.  A byte-identical binary is a
stronger claim than an ANSI gate run carrying ±10-15 noise, so the 64-shard gate
was skipped per the acceptance criteria.

The new `*CLI-ARCH-IO-SCRATCH-SOURCE*` slot is spliced at a point where x64 and
aarch64 pass `""`, so it contributes zero bytes to their blobs.

---

## 2. What the old i386 wrapper was

A **third build lineage**.  `mvm/build-i386-cli.lisp` loaded `lib/load-mvm.lisp`
directly — not `build-cli-common.lisp`, not `build-ansi-common.lisp` — and
re-derived the entire source set by hand: its own defun scanner, symbol-name
scanner, macro-table scanner, opcode-table emitter, float-slot override, CL
bridge file list, and its own boot sequence (`%l5-boot`) running in parallel with
the shared `kernel-main`.

The consequence was not stylistic.  It baked **zero library surface** — no
RTEST, no `tar`/`install-tarball`, no hosted sockets/storage/HTTP, no ASDF
interface.  The 22-library ladder and alexandria's own test suite could not
*run* on 32 bits.  Not "scored badly": could not run.  i386 was the only
release-gate platform with no library measurement, and nothing about the
arrangement would ever have produced one, because every capability added to the
hosted CLI landed in the shared file this wrapper did not read.

---

## 3. Convergences — differences that turned out not to be facts

* **The probe slot is now byte-for-byte x64's.**  `boot-linux-i386.lisp`
  publishes `argc`/`argv[1]`/`argv[2]` at the *same* BSS addresses as the 64-bit
  boots (`0x10000200`/`0x10000208`/`0x10000248`), and `translate-i386.lisp` puts
  the handler jmpbuf at `0x10000180` and the handler-stack depth at `0x10000400`
  — again the same.  The forked copy was pure drift.
* **`%argv1` returned a NUMBER on i386 and a STRING on both other arches.**
* **`%l5-boot` duplicated the shared boot init**, minus everything added to the
  shared one since it was forked: `char-code-limit`, `lambda-list-keywords`, the
  ANSI array/call limits, `%init-rtest`, and the genera and ASDF installs.
* **`MODUS_I386_LAYER=1..5`** — a bring-up scaffold, retired.
* **The ~1300-line baked probe suite** reached by a numeric `argv[1]`
  (`run-i386.sh test/gc/bulk/chain/argv/probe N`) — retired.  A shipping image
  bakes no test corpus (CLAUDE.md "Build taxonomy"), and that suite is exactly
  what kept i386 on its own lineage.  Recoverable at `ba693fa`.  Replacement
  gate: `scripts/run-ladder-i386.sh` (added here).

---

## 4. Remaining i386 divergences — each named and justified

| # | divergence | class | justification |
|---|---|---|---|
| 1 | `exit` = syscall **1** | ABI fact | i386 `int 0x80` numbering (x64 60, aarch64 93) |
| 2 | file-I/O syscall numbers; `access(2)` for existence; `stat64`/`fstat64` with `st_size` at **44** (not 48) and `st_mtime` at **72** (not 88) | ABI fact | `cl-fileio.lisp` hardcodes the x86-64 table; same class `build-aarch64-cli` fixed for the `*at`-only ABI |
| 3 | argv/envp read from the boot stub's **staged BSS copy** at `0x10009000`, 4-byte slots | ABI + address-width fact | the live kernel stack is at `0x40800390`, above the 2^30 ceiling a tagged `mem-ref` address can express |
| 4 | `*cstr-scratch*`/`*io-buf-addr*` in the `0x10004000..0x10009000` BSS window (`*CLI-ARCH-IO-SCRATCH-SOURCE*`) | memory-map fact | i386's heap is at `0x30000000`; the 64-bit default `0x1DF00000` is unmapped, and a syscall address travels as a tagged fixnum so it must be < 2^30 |
| 5 | `*scratch-mmapped*` forced T | ABI fact | `%ensure-scratch-mmapped` issues `(syscall3 9 …)` — `mmap` on x86-64, **`link(2)`** on i386 |
| 6 | **no in-image JIT** (`*JIT-ON*` forced NIL) | **OPEN ITEM, not a hardware fact** | `translate-i386.lisp` builds the image but has no runtime arm: no code-buffer shrink needle, no co-init for the tables limitation #7 leaves empty, no runtime `PROT_EXEC` page primitive.  `mvm-eval` falls back to `mvm-interpret` — correct, just slower.  Deleting one `cond` clause is the whole change when an i386 JIT arm lands. |

**A finding that fell out of the extraction:** `kernel-main`'s
`(setq *cstr-scratch* #x0FE00000)` / `(setq *io-buf-addr* #x0FF00000)` are
**dead code on x64 and aarch64** — they run *before* `(init-all-globals)`, which
re-runs `cl-fileio.lisp`'s defvar init thunks and restores `0x1DF00000` /
`0x1DE00000`.  A running `./modus` reports exactly those.  Harmless there only
because the 64-bit BSS happens to cover both addresses.  Left byte-for-byte
alone so the two shipping images stay identical; fixing it is a separate,
gated change.

---

## 5. Translator gaps

| gap | before (per `GATE-RESULT-i386-float.md`) | after convergence |
|---|---|---|
| `trap #x0520` INSTALL-SIGNAL-HANDLERS ×1 | present, deliberately no-op'd | unchanged |
| `trap #x0507` SYSCALL6 ×3 | — | **NEW** |

The convergence adds exactly one gap, and it is attributable: baking
`net/hosted-sockets.lisp` (a capability i386 never had) introduced three
`syscall6` sites — `setsockopt`, `sendto`, `recvfrom`.  Hosted **UDP** sockets
are therefore dead on i386; hosted TCP additionally needs the i386 `socketcall(102)`
multiplexer rather than the direct socket syscalls the shared file uses.

`#x0520` is the load-bearing one for everything in §7: with no signal handler,
a hardware fault **kills the process** instead of being longjmp-recovered into
the nearest `handler-case`.  On x64 a latent defect surfaces as `(:ERR …)` in
one probe; on i386 the same defect ends the run and destroys the rest of the log.

---

## 6. CLUSTERED INVENTORY — what breaks on i386

Every row was measured by running the SAME probe file on the x64 binary from
this tree and on the i386 binary under `qemu-i386-static`, so "also fails on
x64" is an observation, not an inference.

### 6A. GENUINE 32-bit width issues — report, do not paper over

| id | repro | x64 | i386 |
|---|---|---|---|
| **#216** integer literals ≥ 2^29 miscompile | `--eval '(print 536870912)'` | `536870912` | **`202088934`** |
| **#216b** literals ≥ 2^30 are FATAL | `--eval '(print 1073741824)'` | `1073741824` | **SIGSEGV, process dies** |
| **#217** `~F` on a binary-inexact double | `(format nil "~F" 0.1)` | `"0.1"` | **SIGSEGV, process dies** |

`(format nil "~F" 1.5)` is fine on both — it is specifically the inexact case,
matching #217's `%format-f` 31-bit-width diagnosis.  These are #201's umbrella
(width-neutral numeric tower) and are exactly the class the brief said to expect.

### 6B. i386-only defects that are NOT width issues

| # | symptom | minimal repro | x64 | i386 |
|---|---|---|---|---|
| **B1** | boot-time `%install-genera-compat` and `%install-asdf-interface` both fail | `--eval '(print (list (and (member :genera *features*) t) (and (find-package "ASDF") t)))'` | `(T T)` | **`(NIL NIL)`** |
| **B2** | `MACROEXPAND` of a runtime-defined macro signals `PROGRAM-ERROR` | see below | `(+ 5 1)` | **`PROGRAM-ERROR`** |
| **B3** | GC faults when allocation crosses the VL trigger | see below | survives | **SIGSEGV** |
| **B4** | `trap #x0507` SYSCALL6 unimplemented | build report | — | hosted UDP sockets dead |
| **B5** | `trap #x0520` no signal handlers (pre-existing, deliberate) | build report | faults recover into `handler-case` | **every fault kills the process** |

**B1 is the highest-leverage one** — it is the aarch64-shaped finding of this
rotation.  The compat SOURCE is fine: evaluating `net/cooperative-atomics.lisp`
+ `net/genera-compat.lisp` + `net/asdf-interface.lisp` *from disk* on the i386
image installs both surfaces correctly (`:genera` lands on `*features*`, the
`ASDF` package appears).  `%it-eval-source` itself is fine (a 25 KB dynamically
built source evals identically on both arches).  The failing thing is
specifically the boot-time path through the build-time-BAKED 26–29 KB string
literal returned by `%genera-compat-source` / `%asdf-interface-source` — the
only two boot steps that do that.  `%init-rtest`, the adjacent boot step that
does NOT go through a baked string, works.

B1's blast radius is measured, not guessed: `install-tarball` LOAD-ABORTs on
exactly the libraries whose `.asd` opens with a read-time ASDF guard —

```lisp
#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))
```

Of the 22 ladder tarballs, three carry an `asdf-version` read-time guard
(`bordeaux-threads`, `iterate`, `split-sequence`); `bordeaux-threads` and
`split-sequence` are exactly the two `LF-INSTALL=(LOAD-ABORT SIMPLE-ERROR)` in
the i386 run and both load clean on x64.

**B2 repro** (3 lines, no library needed):

```lisp
(%it-eval-source "(defmacro m1 (x) (list (quote +) x 1))" "m")
(macroexpand-1 '(m1 5))                    ; => (+ 5 1)      on BOTH
(macroexpand   '(m1 5))                    ; => (+ 5 1) x64  / PROGRAM-ERROR i386
(funcall (macro-function 'm1) '(m1 5) nil) ; => (+ 5 1) x64  / PROGRAM-ERROR i386
```

So it is not macro expansion as such — `macroexpand-1` and *using* the macro
both work.  It is calling the expander closure with **two** arguments
(form, env), which is what `macroexpand`'s loop and any code walker does.
`&rest` and `&optional` on ordinary runtime `defun`s are fine, so this is
specific to the macro-expander closure's arity path.  This is the direct cause
of all 7 alexandria `LINE-UP-FIRST`/`LINE-UP-LAST` errors (§7) and of the
`P*.wgs-expand`-shaped ladder probes.

**B3 repro** — allocate past the collector's trigger:

```lisp
(defun burn (n) (let ((k 0)) (loop (when (>= k n) (return k)) (make-array 100000) (setq k (+ k 1)))))
(burn 500)   ; ~100 MB — OK with the default VL (256 MB trigger)
(burn 1000)  ; crosses 256 MB — SIGSEGV
```

Rebuilding with `MODUS_I386_VL=8388608` (8 MB trigger) moves the crash *earlier*,
proportionally: `(burn 500)` now dies.  Two data points, both at the trigger ⇒
the native i386 Cheney collector faults on its first collection in this image,
rather than "never fires".

### 6C. Measurement traps found while doing this — read before trusting a number

* **Interpreted `mem-ref` is SIMULATED, on every arch.**  `(mem-ref #x10000200 :u32)`
  evaluated through `--load`/`--eval` returns 0 on x64 *and* i386, while the
  native `(%argc)` returns 4.  x64's ladder logs show real GC counters only
  because the JIT compiles the driver's `lf-gcn` defun to native.  With no i386
  JIT (§4 row 6) the same defun is interpreted, so **`LF-GC-*=0` in the i386
  logs is an artifact, not evidence about the collector.**  Consequence:
  `lf-force-gc` can never observe its own exit condition on i386 and always runs
  its full 2500-iteration bound (~500 MB), which is what walks into B3.  That is
  a property of a JIT-less image, not a harness defect — the harness was not
  touched.
* **`(print (find-package "CL-USER"))` appears to hang** on i386.  It does not:
  printing a package walks a circular use-list/used-by structure under a
  1,000,000-object print budget, which under qemu takes many minutes.  x64 does
  the same thing, faster.  Probe with `(and (find-package "…") t)`.

### 6D. Fails on x64 too — NOT i386 issues

Listed so nobody re-reports them as 32-bit problems.  Identical output on both.

| symptom | both arches |
|---|---|
| `(length (make-array 100000))` | **50000** — halved |
| `(let ((n 1000)) (length (make-array n)))` | **500** — halved (the known variable-size `make-array` bug) |
| `(format nil "~E" 1.5)` | `"~E"` — directive not implemented |
| `(defstruct pt x y)` via runtime eval | `SIMPLE-ERROR` |
| `with-open-file` write-then-read-line round trip | `END-OF-FILE`, `probe-file` → `NIL` |
| `(eq (gensym) (gensym))` | `NIL` (correct) |
| ladder probes `P*.subst`, `P*.match`, `P*.digest`, `P*.defrt` | error on both |

### 6E. Verified IDENTICAL to x64 on i386

The whole of this list was previously unmeasurable on 32 bits:

`(+ 1 2)` · float `+`/`/` · `sqrt` · `format ~D` · `format ~F` (exact) ·
`string-upcase` · `sort` · `defclass`/`make-instance`/accessor · hash tables ·
`handler-case` · `restart-case`/`invoke-restart` · `unwind-protect` ·
`loop … collect` · `package-name` · `%cli-getenv` (present *and* absent vars) ·
`%cli-argc`/`%cli-collect-argv` · `find-package "RTEST"` · `%it-eval-source` of
25 KB of source · 30,000-character string literals through the in-image compiler ·
`install-tarball` (gunzip, untar, `.asd` parse, component ordering) ·
`--eval`/`--load`/`--quit`/`~/.modusrc` toplevel.
