# `ql:quickload` on Modus (x64-linux)

Modus can load a small, dependency-free pure-CL library from a local tarball
bundle via a minimal `ql:quickload`, using the self-hosted CL runtime and the
`eval2` evaluator (compile → MVM bytecode → interpret).  This is fully offline:
no network, no host Lisp — the built binary is an ordinary Linux ELF process.

## Do something

Build the REPL image and run it:

```bash
sbcl --dynamic-space-size 4096 --script mvm/build-x64-ql.lisp   # → ./modus-ql
./modus-ql                                                      # → Modus REPL on stdin
```

Then, typed into that REPL (or piped via stdin):

```lisp
(ql:quickload :sha1)          ; loads the sha1 system from systems/sha1.tar
(sha1:sha1-hex "abc")         ; => "A9993E364706816ABA3E25717850C26C9CD0D89D"
```

Piped, end to end:

```bash
printf '(ql:quickload :sha1)\n(sha1:sha1-hex "abc")\n' | ./modus-ql
```

Expected (progress/`WARN` lines omitted):

```
Modus REPL (x64-linux).  Ctrl-D to exit.
> ; loading sha1 from systems/sha1.tar
"sha1"
> "A9993E364706816ABA3E25717850C26C9CD0D89D"
>
```

`Ctrl-D` (EOF) exits the REPL.  The binary looks for tarballs in `systems/`
relative to the current working directory, so run it from the repo root (where
the bundled `systems/sha1.tar` lives) or set `*ql-systems-dir*` (see below).

## What it is

- **`mvm/build-x64-ql.lisp`** — the build.  Derived from `mvm/build-generic.lisp`
  (full CL runtime, no baked-in ANSI tests), plus three baked-in `lib/` files:
  - `lib/tar.lisp` — POSIX ustar reader (pure CL, no FFI).
  - `lib/install-tarball.lisp` — untar → find `.asd` → parse `defsystem`
    `:components` → topo-sort by `:depends-on` → `eval` each source form.
  - `lib/ql-shim.lisp` — the `QL` package, `ql:quickload`, and the stdin REPL.

  When no argv script is given, the driver enters the REPL (`%ql-repl`).  An argv
  script still works (`./modus-ql path/to/script.lisp` loads it and exits, exactly
  like `build-generic`).

- **`lib/ql-shim.lisp`** — the client surface:
  - `ql:quickload` — accepts a keyword / string / symbol designator, resolves it
    to `<*ql-systems-dir*>/<name>.tar`, reads the bytes, calls `install-tarball`,
    and returns the loaded system name (string).  A missing tarball signals a CL
    error (the REPL catches it and continues).
  - `*ql-systems-dir*` (default `"systems/"`) — configurable via
    `(ql-set-systems-dir "…/")`.
  - The `QL` package and the `QL:QUICKLOAD` fdefinition are established at boot in
    `kernel-main` (`%ql-init`) — the package MUST exist before the reader first
    sees a `ql:…` token.

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

Then in the REPL: `(ql:quickload :<name>)`.

Notes / limitations (v1):

- **Offline only.**  `quickload` reads `systems/<name>.tar`; it does not fetch.
  Network fetch over `net/http-client` (DNS + HTTP already exist in the tree) is
  a documented follow-up — `install-tarball-from-bytes` already accepts an
  in-memory byte vector, so the fetch path is a thin addition.
- **Plain `.tar`, not `.tar.gz`.**  The gzip (`chipz`) decompressor can't be
  cleanly baked into the flat-namespace image yet (its read-time constant tables
  need a build-time host-load pass), so the image ships plain tar.
  `install-tarball` only calls `decompress` when the gzip magic `1f 8b` is
  present, so the dormant reference is harmless.  Gunzip support is the same
  build-time follow-up documented for the aarch64 net build.
- **No dependency resolution across systems.**  `:depends-on` is honored *within*
  a system's own `.asd` (topo-sort of its files), but `quickload` does not pull
  transitive *system* dependencies.  Pick zero-dep libraries (like `sha1`), or
  bundle and load dependencies first.
- **Per-library eval2 gaps.**  A library may use CL features `eval2` doesn't yet
  cover; `sha1` loads and runs fully.  (`trivial-indent`, for example, currently
  hits an `eval2` gap around package nicknames / `*modules*`.)

## Reproduced

Verified on x64-linux from a clean build of `mvm/build-x64-ql.lisp`:

```
$ printf '(ql:quickload :sha1)\n(sha1:sha1-hex "abc")\n' | ./modus-ql
...
"sha1"
"A9993E364706816ABA3E25717850C26C9CD0D89D"
$ printf '(ql:quickload :sha1)\n(sha1:sha1-hex "The quick brown fox jumps over the lazy dog")\n' | ./modus-ql
...
"2FD4E1C67A2D28FCED849EE1BB76E7391B93EB12"
```

Both digests are the canonical SHA-1 values, confirming the loaded system runs
correctly under `eval2`.
