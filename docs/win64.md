# Hosted Windows x86-64 (`modus.exe`)

Written 2026-11-21.  Status: **the REPL, the collector, the runtime JIT, file
I/O (`load`, `--script`, the FASL cache, drive letters), IPv4 sockets, native
threads, fault recovery and save-and-die cores work, under Wine and in a real
Windows Server 2022 guest, and the glass desk runs** (Testing, below).  No
directory listing yet.

```sh
sbcl --dynamic-space-size 4096 --script mvm/build-win64-cli.lisp   # -> modus.exe
WINE_RUN="wine64" test/win64/run-wine.sh modus.exe                  # 19 checks, Wine
test/win64/run-vm.sh win2022.qcow2 modus.exe                        # the same, real Windows
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

Implemented in the shim: `read`/`write` (fds 0-2 are the console; any other fd
is a Win32 HANDLE), `close`, `lseek`, `getcwd` (slashes, not backslashes),
`mmap`/`munmap` (anonymous; the hint-less bump is atomic), `clock_gettime`
(the CPU-time clocks from `GetProcessTimes`/`GetThreadTimes`, the rest from the
precise system time), `nanosleep`, `getpid`, `exit`/`exit_group`, the thread
calls (Threads, below), and `rt_sigaction` (Faults, below).
`rt_sigprocmask`, `sigaltstack`, `mprotect` and the uid/gid calls return 0.
Everything else returns `-ENOSYS`.

**Sockets.**  The shim answers the Linux socket calls (`socket`, `bind`,
`listen`, `accept`/`accept4`, `connect`, `getsockname`, `getpeername`,
`setsockopt`/`getsockopt`, `sendto`/`recvfrom`, `shutdown`, `poll`, `fcntl`
O_NONBLOCK, `ioctl` FIONREAD) with WS2_32; the prologue calls `WSAStartup`.  A
SOCKET is the fd, so `read`/`write`/`close` on an fd above 2 try
`recv`/`send`/`closesocket` first and fall back to the file call on
`WSAENOTSOCK`.  WSA errors map to Linux errno values; IPv4 only.

**Calling Win32 from Lisp.**  Syscall number `#x1000 + slot` calls the import in
that IAT slot directly: the six Linux argument registers in Win64 order, a zero
seventh argument (enough for `CreateFileA`), RAX returned as it is.  The build
generates `(defconstant +w32-createfilea+ ...)` for every import
(`WIN64-IMPORT-CONSTANTS-SOURCE`), so the Windows runtime writes
`(syscall6 +w32-getfileattributesexa+ path 0 buf 0 0 0)`.  Imports are grouped by
DLL in `*WIN64-IMPORTS*`.

**Files.**  The file primitives cl-fileio calls (`%sys-open-*`, `%sys-stat-*`,
`%sys-fstat-size`, `%sys-unlink`, `%sys-rename`, `%sys-mkdir`, `%sys-rmdir`) are
overridden in the Windows runtime source on top of that call path; a HANDLE is
the fd.  Paths go to the ANSI calls as they are: forward slashes work, relative
paths are relative to the process's current directory.  **Drive letters**:
`%DRIVE-NAMESTRING-P` (overridable in cl-fileio, NIL elsewhere) makes `C:/a/b`
an absolute namestring whose first directory component is `"C:"` -- not stripped
as a logical host, not merged with the current directory, rendered without a
leading slash, walked from the drive by truename.  **The FASL cache is the
Windows image's own** (`$MODUS_FASL_DIR`, else `$HOME/.cache/modus/fasl-win64/`,
else `%LOCALAPPDATA%/modus/fasl/`): the key covers the source, not the target,
and through Wine's `Z:` drive the Linux cache is right there -- loading its
FASLs failed silently.

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

**Threads: the window and per-CPU bases are TEB TLS slots.**  On Linux the
per-thread window is FS-relative and the per-CPU block GS-relative, one prefix
byte per access.  Windows gives a user process neither: GS is the TEB, and a
user FS base is lost at the first context switch (`test/win64/probe/fsbase.asm`:
`WRFSBASE` executes and `fs:` loads use the base, then a `Sleep(1)` resets it;
Wine keeps it, so Wine cannot answer this).  The prologue claims TlsSlots 59-63
(`gs:[0x1480 + 8*i]`, by calling `TlsAlloc` until it returns 63):

| slot | TEB offset | holds |
|---|---|---|
| 59 | `0x1658` | 0 on the main thread; in a cloned thread its clear-tid word, or-ed with 1 |
| 60 | `0x1660` | this thread's Win32 stack, where the shim runs Win32 calls |
| 61 | `0x1668` | the translator's scratch register, parked around an access |
| 62 | `0x1670` | the per-CPU block (Linux: the GS base) |
| 63 | `0x1678` | the window base (Linux: the FS base) |

Every access the translator would prefix with FS or GS becomes
`mov gs:[0x1668],scr; mov scr,gs:[base]; <the access, based on scr>; mov scr,gs:[0x1668]`,
with SCR a register the access does not name: no stack, no flags.  No call site
changed shape.  With `*X64-WIN-TLS*` the prefix starts a capture
(`MODUS.ASM:%SEG-BEGIN`); the translator emits the Linux instruction as always,
and `EMIT-BYTE` rewrites it in place once complete -- it is then the last thing
in the buffer.  Lengths change, so the one hand-counted distance across such an
access (SETJMP's resume LEA) is measured (`X64-TLS-INSN-LENGTH`), and the JIT's
bridge thunks take their window store from `%JIT-WINDOW-STORE` in 64-byte slots.
The cost: the image is ~7.6 MB of native code larger than the Linux one.

The thread syscalls are the shim's: `clone` is `CreateThread` (suspended until
the parent's tid word is written; the child starts in the stub, takes its TLS
slots and R12-R15 from a record on its new stack, and resumes at the call's
return address with RAX 0, as a Linux child does); `exit` in a cloned thread
clears its tid word, `WakeByAddressAll`s it and `ExitThread`s; `futex` is
`WaitOnAddress`/`WakeByAddressSingle`/`All` (KERNELBASE); `arch_prctl` reads and
writes slots 62/63; `gettid` is `GetCurrentThreadId`; `sched_yield` is
`SwitchToThread`.  The window, per-CPU storage and stop-the-world are on, as on
Linux, in the image and in the JIT's co-init.  Differences that remain: the main
thread's TID is not the PID.

**Faults.**  `rt_sigaction` for SIGSEGV, SIGILL, SIGFPE and SIGBUS records the
runtime's signal stub and installs one vectored exception handler.  For an
access violation, illegal or privileged instruction, or integer divide or
overflow in the image, heap, JIT arena or mapped stacks, the handler builds a
siginfo (`si_addr` at +16) and a ucontext (RBP, RAX, RSP, RIP where Linux puts
them) below its own frame, rewrites the CONTEXT to enter the stub with
RDI/RSI/RDX as a signal handler, and resumes it.  So a memory fault, a division
by zero or a refused shared store becomes a Lisp condition, as on Linux.  Not
covered: stack overflow (the Lisp stacks have no guard page Windows knows).

**Cores.**  `save-and-die` writes a core as on Linux.  `modus --core FILE` is
decided in kernel-main from `argv[1]` before anything exists; here argv is one
command line, so `%CORE-REQUESTED-P` and `%CORE-OPEN-IN` read its tokens as raw
bytes and open the path in place with `CreateFileA`.  A core is tied to the
binary that saved it, so one saved under Wine starts the same `modus.exe` in a
Windows guest.  **Home**: `user-homedir-pathname` is `$HOME` when set, else
`%USERPROFILE%` (Wine does not pass `HOME` through).

**Native windows** (`net/win32-window.lisp`; glass's `SERVE-NATIVE` on branch
`native-window`).  `W32-WINDOW-OPEN` / `-PRESENT` / `-EVENTS` / `-CLOSE` /
`-STATS`, in MODUS-INT (callable by name: the Windows build hands the file to the
name scanners through `*CLI-SCAN-EXTRA-SOURCE*`).  USER32 and GDI32 are loaded at
the first window (LoadLibraryA + GetProcAddress) and called through shim number
`#x3000` (function pointer, up to 20 arguments in memory), so a console modus.exe
never maps them -- and this Wine, which cannot load them, still runs it.  The
window procedure is native (`WIN64-WNDPROC-ITEMS`): it rings the messages Lisp
reads and defers to DefWindowProcA; no Lisp runs inside a Win32 callback.  The
events are RFB's, `(:key DOWN KEYSYM)` and `(:pointer MASK X Y)`, so glass hands
them on unchanged.

Presenting is damage-only and in place: glass's damage (one box per composite,
`FB-TAKE-FRAME`) is blitted straight from its pixel vector -- a packed
`(unsigned-byte 32)` vector is a top-down 32-bit DIB -- as kiln presents on the
board and the Mac.  It is sound because the call is not GC-safe (a stop-the-world
collection waits for it) and the address is taken after the last allocation.
Measured in the guest with `GLASS_NATIVE_STATS=1` (glass/wm 800x560): a window
drag, ~20 frames a second of ~250 Kpx each, cost 300-450 ms a second presenting
with the old per-pixel copy (`MODUS_W32_COPY=1`) and 33-52 ms in place; the idle
clock (35 Kpx a second) 4-67 ms against 0.5-10 ms.  The move hint (CopyRect) is
not used: the window manager repaints the whole box, and only a diff against a
snapshot -- a Lisp loop as slow as the copy -- could say which moved pixels are
unchanged.

## Testing

- `test/win64/cases.sh` holds the checks (arithmetic, bignums, printing,
  allocation churn, the JIT's calling paths, a loopback socket, files:
  write/read, probe/delete, `load`, `--script`, drive-letter paths).  The churn
  checks only collect on a small-semispace build (`MODUS_X64_MIDPOINT=16777216`).
- `test/win64/run-wine.sh EXE` runs them under Wine, after a deliberately wrong
  expectation that must fail.  The drive-letter checks run for an `.exe` only.
- `test/win64/run-vm.sh BASE.qcow2 EXE` runs them in **real Windows** under KVM:
  modus.exe, each case's input and a `RUN.CMD` go on a small FAT disk; a
  boot-time task in the guest runs `RUN.CMD` and powers off; the outputs are
  read back with mtools and judged on the host.  Each run boots a throwaway
  overlay, so the base disk stays clean; a run takes under a minute.  The
  control there is that `(+ 1 2)` answered 3 in the guest and the check rejects
  4.  2026-11-21: 19/19 on Windows Server 2022 (10.0.20348.587), as under Wine.
- The base disk: `test/win64/vm-install.sh SERVER_EVAL.iso DISK.qcow2` installs
  Server 2022 Core unattended from Microsoft's evaluation ISO (180-day licence;
  ~15 minutes with KVM), no network, using `test/win64/vm/autounattend.xml`.
  `test/win64/vm-screen.py SOCKET OUT.png` screenshots either script's VM
  through its QMP socket.
- Repo tests with `--script`, Windows against a Linux build of the same source
  (2026-11-21): hot-jit, redefine-native-callers, redefine-kernel-callers,
  gc-forced-stress, region-gc, region-gc-roots, region-gc-depth, gc-tenure,
  static-symbols, kernel-shadow, kernel-global-shadow and word-boundary give
  the same checks, ok and FAIL alike.  `test/win64/stdin-diff.sh` does the same
  through stdin, for an image without files.
- Threads (2026-11-21, after merging main's a320c0c6): the hosted thread tests
  -- sleep, threads, many-threads, mutex, sb-thread (45), thread-lisp (30),
  many-regions (107), thread-gc (40), thread-regions (37), thread-actors (39),
  stw (7), stw-spinning-worker (27) -- pass in the Windows guest as on Linux,
  except hosted-threads' "the main thread's TID is the PID".  Put the test files
  on the disk and run them from `EXTRA_CMD`:
  `EXTRA_CMD='modus.exe --script hosted-mutex.lisp > hosted-mutex.res 2>&1' test/win64/run-vm.sh BASE modus.exe test/hosted-mutex.lisp`.
- glass: `test/run-glass-serve.sh WRAPPER 3` passes under Wine (WRAPPER: a
  shell script that runs `wine64 modus.exe "$@"`): three VNC clients at once,
  every pixel checked.  The desk (`glass/demo/desk.sh` with `MODUS=WRAPPER`)
  builds its core under Wine and serves; the same `modus.exe` and core run in
  the guest with `VM_FWD=tcp::5933-:5910` and an `EXTRA_CMD` that turns the
  firewall off and starts `modus.exe --core desk.core --script desk-run.lisp`
  with `DESK_ADDRESS=0.0.0.0`; `test/win64/vnc-shot.py 127.0.0.1 5933 OUT.png
  [--click X Y]` then shows the desk, and a click opens its root menu.
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

1. Directory listing (`%sys-getdents64`: FindFirstFile/FindNextFile), so
   `directory` works -- and with it Quicklisp's local-projects scan; and drive
   letters in the rest of the pathname layer (`pathname-device`).
2. Stack overflow as STACK-EXHAUSTED: a guard page on each Lisp stack and a
   handler stack Windows can dispatch on.
3. Size: the window accesses cost ~7.6 MB of native code.  Keeping the base in
   a register across a function would recover most of it.
