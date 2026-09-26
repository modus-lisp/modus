# The library ladder

Twenty-two real Common Lisp libraries, each loaded from its **unmodified
quicklisp tarball** by Modus's own `INSTALL-TARBALL` (so the `.asd` parse,
component walk and load order are Modus's, not a host ASDF's), then exercised by
use-probes before (`P1.*`) and after (`P2.*`) a forced collection.

This is the gate for i386 (see "Running i386" in CLAUDE.md) and the cross-arch
comparison for x64 / aarch64. It used to live outside the repo at
`/home/claude/lf` ("library frontier", after `GATE-RESULT-libfrontier.md`),
which is why the logs and Lisp helpers still say `LF-` / `lf-`.

```bash
test/ladder/run.sh ./modus x64-mytag                  # native
test/ladder/run.sh /path/to/modus-i386-cli i386-mytag # qemu-i386-static, picked automatically
python3 test/ladder/score.py tmp/ladder-logs/x64-mytag
```

`run.sh <binary> <tag> [parallel=8] [timeout-secs=6000]` reads the binary's
architecture from its ELF header and wraps a foreign one in qemu-user
(`QEMU_I386` / `QEMU_AARCH64` / `QEMU_X86_64` override the emulator).
`scripts/run-ladder-i386.sh` and `scripts/run-ladder-aarch64.sh` are thin
aliases for it.

| file | what |
|---|---|
| `gen-drivers.py` | the probes (`LADDER`), declared `:depends-on` (`DEPS`) and the driver prelude; writes one `<lib>-ql.lisp` per library plus `ladder.txt` |
| `run.sh` | verifies `tars/SHA256SUMS`, generates the drivers into `<logs>/<tag>/drivers/`, runs every library in parallel |
| `score.py` | per-library EXIT / OK / ERR / MISSING and the headline `libs= clean= … FAILURES=` line |
| `tars/` | the 22 quicklisp tarballs, byte-for-byte what was used at `/home/claude/lf/tars` |

Logs go to `$LADDER_LOGS/<tag>/` (default `tmp/ladder-logs/`). Drivers are
generated per run rather than committed because they embed the absolute
tarball path; generation is deterministic, and the copy kept beside the logs is
exactly what produced them. Historical logs under `/home/claude/lf/logs/<tag>`
still score: `python3 test/ladder/score.py /home/claude/lf/logs/<tag>
/home/claude/lf/drivers`.

Reading a score:

- **clean** = exit 0, `LF-END` reached, no `(ERR …)`, nothing missing, no
  `LOAD-ABORT` among the dependencies.
- **MISSING** = probes the driver contains that never printed. A library that
  dies mid-run shows up here, not under ERR.
- Some outcomes are coin flips for a fixed binary (e.g. `alexandria-ql`'s exit
  139 at `P1.curry` on i386). Treat ±1 clean on one library as noise. Compare
  logs from two binaries built from the same tree, run at the same time.

Two dependencies are declared but have no tarball, and are left missing rather
than shimmed: `named-readtables` → `mgl-pax-bootstrap`, and `md5` →
`flexi-streams` (non-SBCL only). Adding a library means adding its tarball
(and updating `SHA256SUMS`), an entry in `LADDER` and one in `DEPS`.
