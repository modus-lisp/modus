# What glass's RFB server needs that modus does not have

**Purpose.** The campaign is *run the glass compositor on modus via kiln*, in the
order threads → sockets → glass.  Threads landed; sockets landed with this
document.  This is the map that decides whether "glass" is the next step or
whether something else is.

**Method.** Every symbol below was found by an exhaustive sweep of
`/home/claude/glass` (112 `.lisp`/`.asd` files) for SBCL-package references, and
every "modus has this" / "modus does not" line was **executed against
`./modus`**, not inferred from the source.  Where a probe was run, the probe is
quoted.  Where something was only reasoned about, it says so.

**Headline, stated plainly because the campaign needs it: the socket layer is
NOT what stands between modus and glass.**  After this step, sockets are close
to sufficient for RFB.  The three things that actually block glass are, in
order, **threads (glass needs 2 per connected client and modus has 1 spare)**,
**`sb-alien` reaching libc**, and **`sb-ext:run-program :pty`**.  Only the first
is on the RFB server's critical path.  A gap map that said "sockets, then glass"
would be wrong, and this is the information the campaign asked for.

---

## 0. The ranked answer

| # | Gap | Blocks | Size | Verdict |
|---|-----|--------|------|---------|
| 1 | **`sb-thread:make-thread` of a CLOSURE, N times** | the RFB server's whole concurrency model | LARGE — a campaign | **the real blocker** |
| 2 | `sb-bsd-sockets` as ~22 symbols over a dispatchable CLASS | all of glass's transport | SMALL — a shim file | do it |
| 3 | `sb-thread` mutex / waitqueue / semaphore over glass's API | the sender/reader handoff | SMALL–MEDIUM | do it |
| 4 | `sb-alien` → `getsockopt(SO_PEERCRED)`, `ioctl(SIOCOUTQ)` | UNIX-socket peer gating; sender backpressure | SMALL if shimmed as syscalls | shim the 2 functions, not sb-alien |
| 5 | `(listen stream)` is **wrong** on a socket in modus today | any readiness poll on a stream | TINY | fix it |
| 6 | `sb-posix` — 12 production sites, all in `src/socket.lisp` | UNIX-socket hygiene | SMALL | shim |
| 7 | `sb-ext:posix-getenv`, `*exit-hooks*` | configuration, socket-file cleanup | TINY | shim |
| 8 | **`sb-ext:run-program :pty`** | `glass/term` only | LARGE — needs fork/exec/pty | **cut the system** |
| 9 | `sb-concurrency` mailboxes | the **McCLIM** backend only | MEDIUM | **cut the system** |
| 10 | `cram`, `scribe`, `reed`, `seal`, `chord`, `stave`, `cl-nostr`, `ironclad` | various glass systems | separate question | only `cram` is on the RFB path |

**The minimum viable target is the ASDF system `:glass` and nothing else** —
`src/packages`, `src/record`, `src/framebuffer`, `src/clipboard`, `src/perf`,
`src/socket`, `src/rfb`, `src/zrle` — whose declared dependencies are
`glass/fb`, `glass/clipboard`, `sb-bsd-sockets`, `sb-posix`, `cram`.  That is
the RFB server.  `mcclim-glass` (which adds McCLIM, `sb-concurrency` and
`glass/term`) is a different and much larger project and should not be conflated
with it.

---

## 1. THE BLOCKER: threads

Glass is **thread-per-client, times two**.  `/home/claude/glass/src/rfb.lisp`:

* `:1046` — the accept loop, blocking `socket-accept`, spawning
  `sb-thread:make-thread … :name "glass-client"` per connection;
* `:1050` — that thread is the **reader**, parked in `(read-byte s nil :eof)` at
  `:894`, and it never writes to the socket (`:881`);
* `:883` — it immediately spawns a **second** thread, `"glass-sender"`, running
  `rfb-sender-loop` (`:775-827`), which is the sole writer;
* `:744-753` — the two are joined by an `sb-thread:make-waitqueue` +
  `condition-wait :timeout 1/60` handoff.

So **N viewers on M seats costs 2N + M threads**, plus a framebuffer thread.
`sb-thread:make-thread` appears **99 times**, `with-mutex` **104 times**,
`make-mutex` 28, `join-thread` 23, `make-waitqueue` 2, `condition-wait` 2,
`condition-notify` 4, `condition-broadcast` 1, semaphores 6+9+6.

**What modus has.** Exactly two OS threads, and the second is not a general
facility:

* `net/hosted-actors-post.lisp` holds **one** stack (`*HA-T2-STACK*`), **one**
  thread block, **one** TID word.  `%HA-SPAWN-T2` is singular by construction.
* `%SPAWN-THREAD` takes a **raw native entry address of a zero-argument compiled
  function** (`%SK-T2-ENTRY` = `(- (%gc-word-of (fn-addr %sk-t2-body) …) 3)`).
  It cannot take a closure, because the clone stub branches in the instruction
  stream before any compiled code runs and the child has no environment.
* `%RT-ENTER`/`%RT-LEAVE` serialize **every** thread that touches the runtime
  tables — `SYMBOL-VALUE`, `INTERN`, `FORMAT` — behind one recursive mutex, and
  under that lock exactly one thread is in region 0.  So even with more threads,
  Lisp-level work does not run concurrently yet.
* The multiple-value return buffer (`0x10000090`) and the handler-frame stack
  (`0x10000400`) are still **one per process** — addresses the compiler bakes
  into every `MULTIPLE-VALUE-BIND` and every function epilogue on four
  back-ends.  CLAUDE.md already names making them per-thread as "its own
  campaign, and the next real obstacle".

**Consequence for the shim.**  A faithful `sb-thread:make-thread` is not a shim,
it is a compiler campaign.  The tractable path is the opposite one: **give glass
a different concurrency model rather than give modus glass's**.  The RFB server's
per-client pair is exactly the shape the poll loop this step just built serves
natively — a reader that parks on the socket and a sender that parks on a
condition variable are, together, one poll set entry with `POLLIN` plus a wake
word.  That is a **glass-side port**, not a modus-side shim, and it is the honest
next step of the campaign: *port glass's RFB server to a multiplexed I/O model*,
which is useful to glass on its own terms and is the only version of this that
does not require the MV-buffer campaign first.

Note it is not merely a performance question.  With one spare thread, the
literal shim serves **zero** clients: the first client consumes the reader
thread and the sender thread has nowhere to go.

---

## 2. `sb-bsd-sockets` — 22 symbols, and modus can provide almost all of them

305 occurrences on 273 lines.  There is **no `(:use … sb-bsd-sockets)`, no
`:import-from`, and no package-local nickname anywhere in glass** — every
reference is explicitly package-qualified — so a shim package named literally
`SB-BSD-SOCKETS` exporting these names is sufficient, exactly the way
`net/genera-compat.lisp` creates `SCL`, `SYS`, `SI`, `PROCESS`, `CLI`,
`GRAY-STREAMS` with `:USE NIL`.

**Verified by probe:** modus's CLOS does class definition, inheritance, method
dispatch on a class, `(setf …)` methods, `typep` against a class, and
`define-condition` + `handler-case` on a user condition.

```
PROBE defclass/defmethod/dispatch          => (:BASE :SUB 7)
PROBE setf-able generic (defmethod (setf foo)) => 42
PROBE define-condition + handler-case on it    => 9
PROBE typep on a class                     => T
PROBE defpackage/find-package/find-symbol  => (T T)
```

| Symbol | Uses | Production sites | modus can provide? |
|---|---|---|---|
| `socket-close` | 67 | `src/socket.lisp:305,334,393,444,454,516`, `audio-stream:183,446`, `mic-stream:256,313,535`, `nostr:1921,2012`, `rfb-client:742,811`, `backend:604` | **yes** — `socket-close` exists |
| `inet-socket` | 46 | `src/socket.lisp:280,514`, `backend/backend.lisp:597` (UDP), `test/oracle.lisp:33` | **yes** — a `defclass` over an fd |
| `socket-make-stream` | 46 | `src/socket.lisp:401,414,528`, `audio-stream:154`, `mic-stream:266`, `nostr:1899` | **yes, and this is the big one — see §3** |
| `socket-connect` | 43 | `src/socket.lisp:302,518,519`, `backend:599` | **yes** — `socket-connect` exists (TCP + UDP); AF_UNIX needs a `sockaddr_un` builder |
| `make-inet-address` | 43 | `src/socket.lisp:282`, `backend:600` | **yes** — trivial; modus already carries host-order IPv4 ints |
| `socket` (base class) | 7 | `src/socket.lisp:144,246,252,260,375,405,442` — **CLOS specializers** | **yes** — must be a real class, not a struct; probe confirms dispatch works |
| `local-socket` (AF_UNIX) | 5 | `src/socket.lisp:300,332,513` | **needs work** — modus's `%SOCK-BUILD-ADDR` only builds `sockaddr_in`; `sockaddr_un` is a 110-byte struct.  Straightforward, not present |
| `socket-bind` | 7 | `src/socket.lisp:282,335` | **yes** — `socket-listen-on` |
| `socket-listen` | 6 | `src/socket.lisp:283,337` | **yes** |
| `socket-accept` | 5 | `src/socket.lisp:369,376,382` | **yes** — `socket-accept` / `socket-accept-raw` |
| `socket-shutdown :direction :io` | 5 | `src/socket.lisp:443,453` | **yes** — `shutdown(2)` = syscall 48, 3 args.  **Load-bearing**: it is how glass cancels a parked `accept` (`src/socket.lisp:420-440`) |
| `sockopt-tcp-nodelay` (setf) | 8 | `src/socket.lisp:372,377,526`, `audio-stream:153`, `mic-stream:265`, `nostr:1898` | **yes** — `socket-set-nodelay`, landed this step |
| `sockopt-reuse-address` (setf) | 5 | `src/socket.lisp:281` | **yes** — `%SOCK-SET-REUSEADDR` |
| `socket-file-descriptor` | 3 | `src/socket.lisp:144`, `audio-stream:194`, `mic-stream:513` | **yes** — the fd *is* the object |
| `socket-name` / `socket-peername` | 2 / 1 | `src/socket.lisp:253` / `:208` | **yes** — `getsockname(51)` landed this step; `getpeername(52)` is the same shape |
| `get-host-by-name` / `host-ent-address` | 1 / 1 | `src/socket.lisp:520-521` | **yes** — `dns-lookup` exists.  Only reached for a non-numeric host |
| `connection-refused-error` | 1 | `src/socket.lisp:303` — **production** `handler-case` | **yes** — `define-condition`, signalled on `-111` |
| `socket-error`, `address-in-use-error`, `::socket-error-errno` | 3 | gate scripts only | yes, same way |

**What is genuinely missing on the modus side, and it is small:** `sockaddr_un`
(AF_UNIX), `getpeername`, `shutdown`, and the errno→condition mapping.  All four
are `syscall3`-shaped.  None is a compiler change.

---

## 3. `socket-make-stream` — **MEASURED, and it already works**

This is the single most important finding in this document, because glass does
**100 % of its RFB I/O through Lisp streams**, never through raw socket calls:
`w-u8`/`w-u16`/`w-u32` are `write-byte` (`src/rfb.lisp:25-27`), `w-bytes` is
`write-sequence` (`:28`), `r-u8`/`r-u16`/`r-u32` are `read-byte` (`:29-31`),
`r-bytes` is `read-sequence` (`:33`), and `force-output` appears at
`:212,222,225,242,247,251,252,255,258,263,535,551,665,728`.  Every stream is
`:element-type '(unsigned-byte 8) :buffering :full`.

modus's `mvm/cl-fileio.lisp` has `%MAKE-FILE-STREAM-FULL fd dir element-type` —
a stream constructor that takes a **raw fd**.  Pointed at a socket fd it works
verbatim:

```
PROBE loopback-pair                        => (4 40961 5 6)
PROBE streamp on a socket-fd stream        => (T 5)
PROBE write-byte + force-output + read-byte => (65 66)
PROBE write-sequence 8192 + read-sequence  => (8192 0 255 T)
PROBE open-stream-p                        => T
PROBE close a socket stream                => :CLOSED
```

So `sb-bsd-sockets:socket-make-stream` is a two-line shim over an existing modus
primitive.  **`:timeout` is never passed by `glass:serve`** (`src/rfb.lisp:1047`
calls `accept-stream` with all defaults), so the timeout arm can be a stub.

**One defect found, and it matters.** `(listen stream)` on a **drained** socket
stream returns `T` while `poll(2)` on the same fd correctly reports 0 ready:

```
LISTEN-empty T  (poll says 0 ready)
LISTEN-data  T  (poll says 1 ready)
```

`LISTEN` therefore cannot be used as a readiness test.  Glass uses it in exactly
one place — the audio-stream drain loop, `src/audio-stream.lisp:186-189` — which
is not on the RFB path, but the same wrongness would silently defeat any
stream-level poll a port might reach for.  **Fix `LISTEN` for fd streams** (it
should `poll` with timeout 0, or check the buffer first and then poll); it is a
small, isolated fix and it is a bug regardless of glass.

---

## 4. Write sizes — what the socket layer must survive

From `/home/claude/glass/src/rfb.lisp`:

* Banding governs everything: `*max-band-rows*` = 64 (`:509-514`), applied in
  `send-rects` (`:531`) and to the CopyRect exposed area (`:723`).
* `write-rect-raw` (`:388-401`) allocates `w*h*pb` and issues **one**
  `write-sequence`.  At 1280 wide with default banding that is **327 680 bytes**;
  at 1920 wide, **491 520**.
* `*max-band-rows*` is a supported live-tunable and **`NIL` disables banding** —
  a full-screen raw rect is then `w*h*4` in one `write-sequence`: **4 096 000
  bytes** at 1280×800, **8 294 400** at 1920×1080.
* `ServerCutText` (`:646-665`) is bounded by `*max-cut-text*` = **8 MiB**
  (`src/clipboard.lisp:271`) — one write of up to 8 MiB.
* Read side is bounded: `%read-discard` (`:610-617`) reads in 4096-byte chunks
  and never allocates a wire-supplied length.

**Before this step, modus would have silently corrupted every one of those.**
`socket-send` copied LEN bytes into a 4096-byte page and issued one `write(2)`,
and it returned the short count that a filled send queue produces while every
caller ignored it.  A single 327 KB rect would have written ~323 KB out of
whatever followed the file-I/O buffer.  **This is now fixed** — the transfer
loop chunks against `%SOCK-IO-CAP` and advances by what `write(2)` actually
took; 4 MiB across four concurrent connections was moved and compared
byte-for-byte by a Python client.

Backpressure: `rfb-sender-loop` measures the kernel send queue with
`ioctl(SIOCOUTQ)` (`src/rfb.lisp:819` → `src/socket.lisp:543-557`) and records
it; the audio/mic paths actively **drop frames** on it
(`src/audio-stream.lisp:194-217`, `src/mic-stream.lisp:513-532`).  See §6.

---

## 5. `sb-thread` — the API surface, with a modus verdict per symbol

| Symbol | Uses | modus |
|---|---|---|
| `with-mutex` | 104 | **yes** — `%MUTEX-LOCK`/`%MUTEX-UNLOCK` on futex(2), `net/hosted-sync.lisp`.  A `with-mutex` macro over them is a shim |
| `make-mutex :name` | 28 | **yes** — one 8-byte word; the name is decoration |
| `make-thread` | 99 | **NO** — see §1 |
| `join-thread :timeout :default` | 23 | partial — `%HA-JOIN-T2` polls the CLONE_CHILD_CLEARTID word with a budget; singular |
| `make-waitqueue` / `condition-wait :timeout` / `condition-notify` / `condition-broadcast` | 2/2/4/1 | **yes** — `%COND-INIT`/`%COND-WAIT`/`%COND-SIGNAL`/`%COND-BROADCAST` exist and are futex-backed.  Note modus's condvar **requires the mutex held across signal** (as pthreads permits); glass's `wake-signal` (`src/rfb.lisp:749`) already does exactly that |
| `make-semaphore` / `signal-semaphore` / `wait-on-semaphore :timeout` | 6/9/6 | **no** — but a counting semaphore over the existing mutex+condvar is ~20 lines.  Not on the RFB path (hearing/speech/mic only) |
| `with-recursive-lock` | 3 | **yes** — the runtime lock is already recursive; same construction |
| `terminate-thread` | 7 | **no** — and no plan for one.  Only in gates + `backend/backend.lisp:510`, `listen-app`, `speak-app` |
| `thread-alive-p`, `*current-thread*`, `mutex` (type) | 6/1/1 | trivial or gate-only |

So **the synchronisation half of `sb-thread` is largely already there**; it is
only `make-thread` that is not, and it is the half glass's architecture rests on.

---

## 6. `sb-alien` — two libc functions, and modus has no libc

Glass's entire FFI surface is **two `extern-alien` targets** in **two files**:

1. `getsockopt(fd, SOL_SOCKET=1, SO_PEERCRED=17, int[3], socklen_t*)` —
   `src/socket.lisp:157-176`, constants at `:137-138`.  Returns `struct ucred
   {pid,uid,gid}` and is how `listener-accept ((l unix-listener))`
   (`src/socket.lisp:380-393`) drops disallowed peers before the protocol layer
   ever sees them.  `src/socket.lisp:123-126` says explicitly that
   `sb-bsd-sockets` does not expose it.
2. `ioctl(fd, SIOCOUTQ=#x5411, int*)` — `src/socket.lisp:551-556` (send-queue
   depth), and `ioctl(TIOCSWINSZ)` / `ioctl(TCGETS|TCSETS)` in
   `src/term.lisp:622-647`.

**modus has no dynamic linking and no libc** — it is a static ELF issuing raw
syscalls.  So `sb-alien` as a *facility* is not shimmable.  But it does not need
to be: `getsockopt` is **syscall 55** and `ioctl` is **syscall 16**, both ≤ 5
arguments, both reachable through the existing `syscall6` trap.  The right move
is to **shim the two functions glass actually calls, not the `sb-alien` package**
— `peer-credentials` and `socket-unsent-bytes` become modus primitives, and
`src/socket.lisp`'s `sb-alien` blocks become a `#+modus` branch.  That is a
handful of lines and it is the same judgement `net/genera-compat.lisp` makes:
shim the shape that is used, not the door it came through.

`src/term.lisp`'s two ioctls are termios and window-size on a **pty**, which
does not exist without §8; they go away with `glass/term`.

---

## 7. `sb-posix` and `sb-ext` — small, and mostly not on the RFB path

**`sb-posix`: 14 symbols, 49 occurrences, and PRODUCTION USE IS 12 SITES, ALL IN
`src/socket.lisp`** (lines 71, 72, 73×2, 89, 198, 290×3, 318, 336, 340×2,
463×2, 464): `stat` + `stat-mode` + `stat-uid` + `stat-ino` + `s-isdir` +
`s-issock` (runtime-dir ownership check and stale-socket detection), `chmod`
(0700 on the dir, 0600 on the socket **before** `listen` — a deliberate
bind/chmod race close at `:336`), `unlink`, `getuid`.  Everything else is gate
scripts.

modus verdict: `stat` is syscall 4/5/6 and modus already has `%SYS-FSTAT-SIZE`;
`chmod` is syscall 90 and `mvm/cl-fileio.lisp:133-134` **already calls it**;
`unlink` is 87 and is already there (`:120-121`); `getuid` is 102.  **All
present or one line away.**  All of it is AF_UNIX hygiene, so all of it is
skippable for a TCP-only first cut.

**A trap worth naming: `src/nostr.lisp:228-231` looks `SB-POSIX` up
DYNAMICALLY** —
`(or (find-package "SB-POSIX") (progn (require :sb-posix) (find-package "SB-POSIX")))`
then `find-symbol "CHMOD"` — and **silently no-ops if it is absent**, leaving key
files world-readable.  A shim must register a package literally named
`SB-POSIX`, not only provide the symbols.  (Not on the RFB path; `glass/nostr`
is a separate system.)

**`sb-ext`: 13 symbols, 149 occurrences, and 73 of them are `sb-ext:exit` in
gate scripts.**  Production use is four things:

* `posix-getenv` ×34 — **modus has `%CLI-GETENV`**, verified: `PROBE getenv => "/home/claude"`.
* `*exit-hooks*` ×1 — `src/socket.lisp:467-477`, unlinks UNIX socket files.
  Documented there as a courtesy, not the mechanism.
* `run-program` / `process-pty` / `process-kill` — `src/term.lisp` only, §8.
* `*invoke-debugger-hook*` ×1 — `backend/wm.lisp:1917`, McCLIM only.

`sb-gray` (12 uses) is **gate scripts only** — no production Gray stream.

---

## 8. `sb-ext:run-program :pty` — cut `glass/term`

`src/term.lisp:679-686` is the only production `run-program`, and it wants
`:pty t :wait nil :external-format :latin-1 :environment (…)`, then
`sb-ext:process-pty`, then two ioctls on the pty fd, then
`(sb-ext:process-kill proc 1 :process-group)` at `:700`.

modus has **no `fork`, no `exec`, no `run-program`** — probed:
`PROBE fork/run-program? => (NIL NIL NIL)`.  A pty additionally needs
`/dev/ptmx`, `grantpt`/`unlockpt` (i.e. `TIOCSPTLCK`/`TIOCGPTN` ioctls), and a
session/process-group model.

`glass/term` is a separate ASDF system (`glass.asd:246`) that **the RFB server
does not depend on**.  Cut it.  It is only pulled in by `mcclim-glass`.

---

## 9. The ASDF dependency reality

`glass.asd` declares 16 systems, all `:serial t`.  The platform modules are
declared in exactly three places:

* `glass.asd:47` — `:glass` depends on `"glass/fb" "glass/clipboard"
  "sb-bsd-sockets" "sb-posix" "cram"`.
* `backend/mcclim-glass.asd:10` — `mcclim-glass` depends on `"mcclim"
  "mcclim-render" "glass" "glass/text" "glass/term" "sb-concurrency"`.

`sb-thread`, `sb-ext`, `sb-alien`, `sb-sys`, `sb-gray` are used with **no
`:depends-on` at all** — they are assumed always-present — so a shim environment
must provide them without an ASDF hook.  modus has ASDF (`PROBE asdf present? => T`)
and `net/genera-compat.lisp` already demonstrates registering foreign packages
at image build time, which is the right hook.

`:glass/fb` depends on **nothing** and already feature-gates its threading with
`#+sb-thread` / `#-sb-thread` (`src/framebuffer.lisp:26,27,32,34,154,158`;
`src/clipboard.lisp:76,77,79,81,258,261,390,391,400,401`).  **That is the one
place glass already knows how to run without threads**, and it is the natural
first rung: load `glass/fb` on modus, render into a framebuffer, and check pixels
— no sockets, no threads, no RFB.

---

## 10. Everything else glass touches

* **`/dev/urandom`** — `src/rfb.lisp:182`, `with-open-file :element-type
  '(unsigned-byte 8)` with rejection sampling.  The **only** entropy source in
  the tree, and it is on the RFB path (VNC credentials).  modus does binary
  `with-open-file` + `read-sequence` — verified.
* **Clock** — `get-internal-real-time` is used as a real unit throughout
  (`src/rfb.lisp:813,818`).  modus has it; `internal-time-units-per-second` = 1000.
* **`user-homedir-pathname`, `ensure-directories-exist`, `probe-file`,
  `file-length`, `read-sequence`** — all verified present.
* **No mmap, no shm, no dlopen, no `define-alien-routine`, no
  `load-shared-object`, no signal handlers** anywhere in glass.  Signals are only
  *sent*, via `process-kill`, in `src/term.lisp` and gates.
* **`sb-concurrency`** (24 uses) is the **McCLIM backend only** — mailboxes in
  `backend/compositor.lisp`, `backend/message-port.lisp`, `backend/backend.lisp`.
  Cut with `mcclim-glass`.

---

## 11. So what is the next step of the campaign?

**Not "port glass".**  The ordered proposal, smallest honest rung first:

1. **`glass/fb` on modus, no sockets and no threads.**  It depends on nothing,
   it already has `#-sb-thread` arms, and it is pure pixel work.  This measures
   whether modus's CLOS/array/arithmetic surface carries glass's core data
   structures at all — which nothing has yet tested — and it cannot be blocked
   by anything in this document.
2. **The `sb-bsd-sockets` shim** (§2 + §3), against a real VNC client, serving a
   *static* framebuffer with the RFB handshake and one `write-rect-raw`.  This
   is small now that the transport is right, and it is the first thing that
   proves the protocol end to end.
3. **Fix `LISTEN` on fd streams** (§3) — independent, tiny, and a real bug.
4. **Then the fork in the road**, and it should be taken deliberately rather
   than discovered: either *port glass's RFB server off thread-per-client onto a
   multiplexed loop* (a glass-side change, useful to glass, unblocked today), or
   *make modus's threads general* (the MV-buffer + handler-stack campaign
   CLAUDE.md already names, plus a real `make-thread`).  **The first is far
   smaller and does not block on the second.**

What this step bought the campaign is that item 2's transport is no longer the
question.  What it did not buy is item 4, and item 4 is where glass actually is.
