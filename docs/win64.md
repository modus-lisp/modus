# Hosted Windows x86-64 (`modus.exe`)

Written 2026-11-21.  Status: **the REPL, the collector and the runtime JIT work
under Wine**; single-threaded; no file I/O or sockets yet.

```sh
sbcl --dynamic-space-size 4096 --script mvm/build-win64-cli.lisp   # -> modus.exe
WINE_RUN="wine64" test/win64/run-wine.sh modus.exe                  # 11 checks
```

`modus.exe` is the same image as `./modus` -- same source, same runtime, the
runtime JIT on -- packaged as a PE32+ console executable.
`mvm/build-win64-cli.lisp` sets `*CLI-WIN64*` and loads
`mvm/build-generic-cli.lisp`; every Windows difference there is guarded by that
flag, and the Windows facts live in `boot/boot-win64.lisp`.  With the flag off
the Linux image is unchanged (checked byte for byte: a Linux and a Windows build
from before and after that refactor differ only in `%CORE-BUILD-ID`).

Knobs: `MODUS_WIN64_OUT` (output), `MODUS_NO_JIT=1` (interpreter only),
`MODUS_WIN64_TRACE=1` (see Debugging), and every `build-generic-cli` knob.

## How it works

**Syscalls.**  The runtime keeps issuing Linux system calls.  Every `syscall`
the translator emits goes through `EMIT-X64-SYSCALL`, which on Windows emits
`lea rsp,[rsp-0x80]; mov r11,SHIM; call r11; lea rsp,[rsp+0x80]`.  The shim, in
the boot stub, takes the Linux number in RAX and arguments in RDI RSI RDX R10 R8
R9, calls kernel32 through the import table, and returns `-errno` on failure.
Like `SYSCALL` it preserves everything but RAX, RCX and R11, and it leaves the
Lisp stack alone: Win32 runs on the shim's own stack, and the call site steps
over the 128-byte red zone.

Implemented: `read`/`write` on fds 0-2, `mmap`/`munmap` (anonymous),
`clock_gettime`, `nanosleep`, `getpid`, `exit`/`exit_group`; `rt_sigaction`
and `rt_sigprocmask` return 0.  Everything else returns `-ENOSYS`.

**Memory.**  Every region the runtime knows by address is where the ELF has it:

| address | what |
|---|---|
| `0x401000` | `.text`: boot stub, native code, constants (RWX) |
| `..0x20000000` | `.bss`: the fixed low block at `0x10000000`, zero-filled |
| `0x2000000000` | heap |
| `0x3000000000` | JIT arena (bump word at `0x10000FB8`) |
| `0x4000000000` | 16 MB stack: Win32 stack (bottom 1 MB), Lisp stack, argv/envp block on top |
| `0x5000000000`+ | hint-less mmaps, placed high as Linux places them |

The image base is fixed (no relocations): the runtime's absolute addresses are
compiled in, as they are for the ELF.

**Startup.**  The prologue maps the heap, the JIT arena and the stack, then
builds what a Linux process starts with above its stack base -- `argc`, `argv`,
NULL, `envp`, NULL -- because `lib/cli-toplevel.lisp` reads argv and envp from
there.  `argv[0]` is the whole command line (`GetCommandLineA`), which a Windows
`%CLI-COLLECT-ARGV` splits; `envp` points into `GetEnvironmentStringsA`'s block.
It then runs the same heap tail as the Linux entry
(`EMIT-LINUX-X64-HEAP-TAIL`).

**What is off.**  The per-CPU GS block, the FS per-thread window and
stop-the-world: on Windows GS is the TEB and FS is not the runtime's to set.
So is everything that needs them -- threads above all.  The runtime JIT's
co-init (`%INIT-X64-TRANSLATOR`, `*CLI-WIN64*` in build-cli-common) turns the
same features off, and its bridge thunks write their window slots without the
FS prefix (`%JIT-WINDOW-PREFIX`).

## Testing

- `test/win64/run-wine.sh EXE`: 11 checks on stdin (arithmetic, bignums,
  printing, allocation churn, the JIT's calling paths), after a deliberately
  wrong expectation that must fail.  The churn checks only collect on a
  small-semispace build (`MODUS_X64_MIDPOINT=16777216`).
- `test/win64/stdin-diff.sh WIN_EXE LINUX_BIN OUTDIR TESTS...`: runs repo tests
  on both images through stdin (there is no file I/O yet) and compares.
  2026-11-21: hot-jit, redefine-native-callers, gc-forced-stress, region-gc,
  region-gc-roots, region-gc-depth, gc-tenure, static-symbols, kernel-shadow,
  kernel-global-shadow and word-boundary give the same verdicts as Linux;
  redefine-kernel-callers fails one check more, the one that `load`s a file.
- JIT and collector together, at the default heap: a JIT-compiled churn past the
  896 MB semispace returns the same value with 6 collections on both platforms.
  (A 16 MB semispace with the JIT overflows the stack inside a collection on
  Linux too, so it is not a test of this port.)

Wine without root: unpack the `wine64`, `libwine` and `wine` debs, make
`/usr/lib/wine` and `/usr/lib/x86_64-linux-gnu/wine` resolve (an unprivileged
overlay over `/usr` works), copy `zlib1.dll` into `x86_64-windows`, and run
`/usr/lib/wine/wine64` directly.

## Debugging

- `MODUS_WIN64_TRACE=1` at build time: the shim writes `s<nr>` (hex) to stderr
  for every syscall.  Calls answered with `-ENOSYS` never reach Win32, so
  `WINEDEBUG=+relay` cannot see them.
- `WINEDEBUG=+relay` shows each kernel32 call, its arguments and its return;
  `WINEDEBUG=+seh` shows the registers at a fault.
- Compare the runtime's own state words with a Linux build before theorising:
  `(modus-int::%gc-read64 #x10000060)` (collection count), `#x10000040/48/50`
  (from/to/size), `#x10000058` (stack base).  The collector fault fixed on
  2026-11-21 was invisible until the count read 0 on Windows and 6 on Linux.
- JIT code: `(jit-eager)` compiles every DEFUN without running it;
  `modus.runtime::*jit-native-dump-hook*` receives each page's bytes; arena
  memory can be read with `%gc-read64`.
- `MODUS_SYMMAP=path` writes a symbol map for the image.

## Next

1. A generic "call import K" path in the shim and a second import DLL, so the
   Windows side of files and sockets can be written in Lisp.
2. File I/O (`--script`, `load`, `compile-file`, the FASL cache).
3. Sockets (Winsock: an fd table, `pollfd` conversion, option and errno maps).
4. Threads.  Needs a per-thread window Windows allows: `WRFSBASE` works under
   Wine on Linux but is unverified on real Windows; a TEB TLS slot holding the
   window base works everywhere but changes every window access in the
   translator.
