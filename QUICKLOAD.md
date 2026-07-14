# `ql:quickload` on Modus (x64-linux)

Modus can load a small, dependency-free pure-CL library from a local tarball
bundle via a minimal `ql:quickload`, using the self-hosted CL runtime and the
`eval2` evaluator (compile → MVM bytecode → interpret).  This is fully offline:
no network, no host Lisp — the built binary is an ordinary Linux ELF process.

**It follows the stock SBCL model.**  The hosted binary (`./modus`) ships
*plain* — a REPL with SBCL-style flags and a `~/.modusrc`, and **no `ql`
symbol at all**, exactly as stock SBCL has no `ql` until you install Quicklisp.
`ql:quickload` becomes available only after you `(load)` a setup file — the
analogue of `~/.sbclrc` loading `quicklisp/setup.lisp`.

## Do something

Build the hosted CLI image and run it:

```bash
sbcl --dynamic-space-size 4096 --script mvm/build-generic-cli.lisp   # → ./modus
./modus
```

Then, at the REPL (or piped via stdin), pull in quicklisp and use it:

```lisp
> (load "modus-quicklisp/setup.lisp")   ; the "install quicklisp" step
> (ql:quickload :sha1)                  ; loads the sha1 system from systems/sha1.tar
> (sha1:sha1-hex "abc")                 ; => "A9993E364706816ABA3E25717850C26C9CD0D89D"
```

Piped, end to end:

```bash
printf '(load "modus-quicklisp/setup.lisp")\n(ql:quickload :sha1)\n(sha1:sha1-hex "abc")\n' | ./modus
```

Run `./modus` from the repo root: `modus-quicklisp/setup.lisp` and the bundled
`systems/sha1.tar` are found relative to the current working directory.  If you
run from elsewhere, set the root first:
`(setq *modus-quicklisp-root* "/abs/path/to/modus/")` before loading setup.

## Automatic on interactive start (`~/.modusrc`)

Put the SBCL-`~/.sbclrc`-style one-liner in `~/.modusrc`:

```lisp
(load "/abs/path/to/modus/modus-quicklisp/setup.lisp")
```

Now every **interactive** `./modus` has `ql:quickload` ready with no manual
load.  Like SBCL, the rc is loaded ONLY before an interactive REPL: `--script`,
`--non-interactive`/`--quit`, and `--no-userinit` all skip it (so `ql` stays
absent in those modes unless you load setup yourself).

## What it is

- **`mvm/build-generic-cli.lisp`** — the build.  It is `mvm/build-generic.lisp`
  (full CL runtime, no baked-in ANSI tests) plus ONE baked-on file, the shared
  SBCL-faithful CLI toplevel:
  - `lib/cli-toplevel.lisp` — reads the full argv, parses SBCL toplevel options
    left-to-right (`--eval`/`--load`/`--script`/`--quit`/`--version`/`--help`/
    `--userinit`/`--no-userinit`/…), loads `~/.modusrc` before an interactive
    REPL, and runs the REPL or exits.

  The output binary is **`modus`** — the canonical hosted Modus binary (override
  the path with `MODUS_CLI_OUT`).  It has **no `ql` symbol** and no quicklisp
  machinery reachable as `ql`.

  The untar → find `.asd` → parse `defsystem` `:components` → topo-sort by
  `:depends-on` → `eval` each source form pipeline (`lib/tar.lisp` +
  `lib/install-tarball.lisp`) **is baked into the binary as GENERAL library
  primitives** (`install-tarball`, `tar-extract`, …) — *not* as `ql`.  It is
  baked (rather than runtime-`(load)`ed by setup) to sidestep a pre-existing
  `eval2` gap: `%tar-slice` calls `(make-array LEN)` with a *variable* size, and
  a variable-size `make-array` returns an array of length `LEN/2` under the
  interpreter, which truncates any tar entry larger than 512 bytes so its source
  won't `READ`.  The native build compiles the same `make-array` correctly, so
  baking loads whole files.  (A runtime-`(load)` of these two files was verified
  to truncate `sha1.lisp` 7311 → 3655 bytes; the baked path loads it in full.)

- **`modus-quicklisp/setup.lisp`** — the loadable "install quicklisp" step.
  When `(load)`ed it:
  - defines the **`QL` package** and **`ql:quickload`** (a keyword / string /
    symbol designator → `<*ql-systems-dir*>/<name>.tar` → `install-tarball`),
  - points `*ql-systems-dir*` at the bundled `systems/`.

  The `QL` package + `quickload` live **only** in this loaded setup — never in
  the binary — which is what makes `ql` absent-until-loaded.

- **`systems/sha1.tar`** — the bundled offline payload for `:sha1`
  (the pure-CL, zero-dependency
  [sha1](https://github.com/massung/sha1) library, ustar-packed).

## Adding another system

Bundle any dependency-free, pure-CL system as a plain `.tar` (ustar) whose top
directory contains the `.asd` and its source files:

```bash
cd ~/quicklisp/dists/quicklisp/software/
tar --format=ustar -cf /path/to/modus/systems/<name>.tar <name>-<version>/
```

Then, after `(load "modus-quicklisp/setup.lisp")`: `(ql:quickload :<name>)`.

Notes / limitations (v1):

- **Offline only.**  `quickload` reads `systems/<name>.tar`; it does not fetch.
  Network fetch over `net/http-client` (DNS + HTTP already exist in the tree) is
  a documented follow-up — `install-tarball-from-bytes` already accepts an
  in-memory byte vector, so the fetch path is a thin addition.
- **Plain `.tar`, not `.tar.gz`.**  The gzip (`chipz`) decompressor can't be
  cleanly baked into the flat-namespace image yet, so the image ships plain tar.
  `install-tarball` only calls `decompress` when the gzip magic `1f 8b` is
  present, so the dormant reference is harmless.
- **No dependency resolution across systems.**  `:depends-on` is honored *within*
  a system's own `.asd` (topo-sort of its files), but `quickload` does not pull
  transitive *system* dependencies.  Pick zero-dep libraries (like `sha1`), or
  bundle and load dependencies first.
- **Per-library eval2 gaps.**  A library may use CL features `eval2` doesn't yet
  cover; `sha1` loads and runs fully.
- **Bare-metal builds don't get any of this** — no filesystem, no argv.  The CLI
  toplevel and quicklisp setup are hosted-image (Linux-ELF) only.

## Reproduced

Verified on x64-linux from a clean build of `mvm/build-generic-cli.lisp`:

```
$ printf '(load "modus-quicklisp/setup.lisp")\n(ql:quickload :sha1)\n(sha1:sha1-hex "abc")\n' | ./modus
...
"sha1"
"A9993E364706816ABA3E25717850C26C9CD0D89D"
```

`A9993E364706816ABA3E25717850C26C9CD0D89D` is the canonical SHA-1 of `"abc"`,
confirming the loaded system runs correctly under `eval2`.

Absence-until-loaded is verified too: in a fresh `./modus`, `(find-package "QL")`
is `NIL` and naming `ql:quickload` is a `READER-ERROR` (unknown package) — while
`(fboundp 'install-tarball)` and `(fboundp 'tar-extract)` are `T` (baked general
primitives).  After `(load "modus-quicklisp/setup.lisp")`, `ql:quickload` is
`fboundp`.
