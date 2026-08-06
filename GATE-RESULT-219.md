# GATE-RESULT-219 — "network bring-up hangs in `dhcp-client`"

Branch `dhcp-hang` off `main` (`9815f46`).  Unpushed, not merged.

## Summary

The framing was right that this is not an SSH bug, but it is not a DHCP bug
either, and it is not in the E1000 transmit path.  **It is a link-time defect
in the compiler**: a call whose callee has no definition in the image is
emitted as `call <bytecode offset 0>` — the module's *first function*, entered
with the caller's arguments.  The bare-metal `net/`-only images have **13 842
such calls to 40 distinct functions**, and the first one on the boot path is
the very first `(aset pkt 0 1)` inside `dhcp-discover`.

`dhcp-discover` did not stall.  It recursed into function #1 until the stack
walked off its base, took a `#GP`, and parked in the boot fault handler's
`hlt; jmp $`.  Nothing about DHCP, the ring, or the NIC was involved.

With the fix the x64 and aarch64 SSH images take a real DHCP lease
(`DHCP:IP=10.0.2.15`) and start `sshd`.  x64 completes the SSH transport
layer, KEXINIT and KEX_ECDH and fails at `incorrect signature`; aarch64 fails
earlier, at the banner exchange.  Those are **further, separate defects**, and
they are described below rather than papered over.

---

## The cheap discriminator: not run, and it would not have discriminated

The suggested first move was a static-IP build.  It would have failed too, and
it would have been misleading: the failing primitive is `aset`, which ARP, IP,
TCP, the SSH framing and all of crypto use on every packet.  DHCP was simply
the first code on the boot path to touch an array — everything before it
(`e1000-init`, `sha256-init`, `ed25519-init`) uses only `mem-ref`.  So "skip
DHCP" moves the same crash a few hundred instructions later.  Confirmed after
the fact: with the array path repaired the whole stack above DHCP came up
without any other change.

## The pcap answer: **no**, nothing left the guest

`-object filter-dump` on the stock image produces a **24-byte pcap — the file
header and zero packets.**  After the fix the same capture holds the
DHCPDISCOVER (291 bytes, `0.0.0.0 → 255.255.255.255`, op=1, xid 12345678,
option 53=1) and slirp's DHCPOFFER (590 bytes, op=2, option 53=2).

## The exact wedge point

Progress markers inside `dhcp-discover` (throwaway build, not committed)
narrowed it to a run of **constant-index `aset`s with no loop in it at all**,
which ruled out the "infinite retry / TX-done bit that never sets" theory.

QEMU's own view settled it:

* `info registers` while wedged: `RIP=0x1897b`, `HLT=1`, stable across samples.
  `0x18820 + 346` is exactly the `hlt` in the `#GP`/`#PF`/`#UD` recovery handler
  that `boot/boot-x64.lisp` writes at `+x64-idt-addr+ + 0x820`; `[0x10000180]`
  (the handler-case slot) was 0, so it took the "no handler armed" halt.
* `-d int` shows the run-up: the PIT interrupt fires at the *same* PC with
  `SP` collapsing `0x7ff2b8 → 0x751ba8 → 0x5a5d88 → 0x3f4ef8 → 0x1e1278`
  — ~700 KB of stack per tick — and then `v=0d` (`#GP`) at `SP=0xf5b58`,
  i.e. below the 1 MB stack base, with `RAX` holding SMBIOS text.  Runaway
  recursion, not a spin.
* Disassembling the recursion target: the first function in the image, whose
  own body contains `callq <first function>`.  That is the shape of an
  unresolved call, not of a Lisp loop.

The build log had been saying so all along:

```
=== 13842 unresolved calls to 40 functions
    (!! NO %%UNRESOLVED-FN STUB — targeting offset 0, this is garbage execution) ===
   2419 × EQL              1348 × %MDA-DATA       794 × %WRAPPER-ASET
   1957 × GENERIC-ADD      1071 × %MDA-P          794 × %ASET-STORE-VAL   …
```

`compile-aref` / `compile-aset` / `compile-array-length` emit wrapper- and
MDA-aware trampolines that call `%MDA-P`, `%MDA-DATA`, `%ASET-STORE-VAL`,
`%WRAPPER-ASET`, `%AREF-MULTI` … — all of which live in `mvm/cl-clos.lisp`
and `mvm/ansi-bridge.lisp`.  A `net/`-only image links none of those files.
The trampolines are correct for the CL images and dead weight for these:
every array here is a flat, non-adjustable, non-displaced, non-fill-pointer
word-slot vector from `(make-array N)`.

## Was it always broken?  Yes, and invisibly

`net/*.lisp` is not on the ANSI gate, `test/features/aarch64-e1000.sh` asserts
only on `E1000:OK` (printed by `e1000-init`, before any array is touched), and
`mvm/BUILDS.md`'s byte-identity table proves only that the migrated build
cells reproduce their retired scripts — not that the images work.  Nothing in
CI ever executed an `aset` on these targets.

**Other feature tests with the same shape** (assert on a marker that precedes
the thing they claim to test):

| test | asserts on | printed by | actually claims to test |
|---|---|---|---|
| `test/features/aarch64-e1000.sh` | `E1000:OK` | `e1000-init`, before any TX/RX | the E1000 driver |
| `test/features/aarch64-ssh.sh` | `SSH:` | `ssh-server` startup | an SSH session |
| `test/features/aarch64-http.sh` | `SSH:` | same | HTTP over the stack |
| `scripts/run.sh` `i386-ssh` row | `DHCP:IP=` | DHCP completion | sshd being up |

`aarch64-ssh.sh` and `aarch64-http.sh` do go on to attempt a real connection,
so they fail honestly today; the `E1000:OK` one is the pure false pass.

---

## The fix

Four changes, no shared-image behaviour change.

### 1. `mvm/compiler.lisp` — `*compile-plain-arrays*` (default NIL)

When set, `compile-aref` / `compile-aset` / `compile-array-length` emit the
primitive word-slot opcodes directly (`compile-word-aref` / `compile-word-aset`
/ `compile-prim-array-length`) instead of the wrapper/MDA trampolines.  This
also removes the `(eql (obj-subtag arr) #x11)` byte-packed dispatch, which the
CL images need and a net image cannot service.

Unresolved calls in `x64/bare/qemu/ssh`: **13 842 → 4 630.**

### 2. `net/bare-runtime-stubs.lisp` (new, first in every net image's file list)

* `%unresolved-fn` — the compiler's `:call` arm looks this name up and points
  every remaining unresolved call at it, so a gap **returns NIL instead of
  executing garbage**.  The build log still enumerates every unresolved callee,
  so nothing is hidden.  This is the structural defence: it converts the whole
  bug class from "silent stack-overflow into function #1" into "wrong value".
* `%ieee-float-p`, `%complex-p` — emitted unconditionally by `compile-truncate`
  / `compile-mod` / `compile-/`.  Unresolved, `(truncate len 2)` in
  `ip-checksum` blew up on every outgoing IP packet.
* `exact-divide`, `%rational-divide` — `compile-/` sends integer/integer here.
  Unresolved, `print-dec`'s `(/ n 10)` returned NIL and `print-dec` then
  recursed on NIL forever (the `DHCP:IP=` wedge).
* `bignum-ash` — `compile-ash` calls it for a *variable* shift count.  `net/`
  has exactly two such sites, both `(ash 1 bit-idx)` in `crypto.lisp`'s Ed25519
  scalar multiply.  Unresolved, they cascaded into **46 million**
  `%unresolved-fn` entries per SSH handshake.
* `nth` — because `compile-mod` expands `(mod n d)` to
  `(nth-value 1 (truncate n d))` and `NTH-VALUE` is a macro over
  `(nth N (multiple-value-list …))`.  With `NTH` unresolved every `MOD`
  returned NIL, which is how **both E1000 ring cursors ended up holding
  `NIL>>1`** — `state+0x10 = state+0x14 = 0x6F568000` (read out of guest
  memory via the monitor) — so the RX descriptor address was computed off a
  garbage cursor and `#PF`'d at `CR2=0x6fa680008`.

### 3. `mvm/build.lisp`

Sets `*COMPILE-PLAIN-ARRAYS*` and prepends `bare-runtime-stubs.lisp` for the
five composite cells: `x64/bare/qemu/ssh`, `aarch64/bare/qemu/ssh`,
`aarch64/bare/qemu/actors`, `aarch64/bare/qemu/isolated`, `i386/bare/qemu/ssh`.

### 4. `net/e1000.lisp` — ring arithmetic without `mod`

`(mod (+ tx-cur 1) 64)` still returned the wrong value **on aarch64** after
`nth` was defined (0 instead of 1), so `TDT` never advanced past `TDH` and the
NIC transmitted nothing — the aarch64 image reached `DHCP:D` with an empty
pcap.  The three ring indices are now compare-and-reset, which is correct,
cheaper, and independent of the multiple-value machinery.  The residual
aarch64 `MOD`/multiple-value divergence is a real bug and is listed below.

### Regression safety

`mvm/compiler.lisp` is shared, so this was gated by byte-identity rather than
by the 64-shard ANSI run:

| unflagged cell | built md5 | `BUILDS.md` |
|---|---|---|
| `x64/bare/qemu/repl` | `269b461a764016eea6533c46798ad3e4` | identical |
| `aarch64/bare/qemu/repl` | `fd0d40b12e984e064f3713ce03ed21e8` | identical |

With `*compile-plain-arrays*` NIL the compiler emits the same bytes it emitted
before, so no image that does not opt in can move.  `net/e1000.lisp` and
`net/bare-runtime-stubs.lisp` are not on the ANSI path
(`grep -c "net/" mvm/build-ansi-common.lisp` = 0).

The five net cells' md5s **do** change, so `mvm/BUILDS.md`'s table is stale for
them by design; it has not been edited here because those rows record
equivalence to the retired scripts, and that claim is now false in a way the
table has no column for.

---

## Where each target stands now

| target | before | after |
|---|---|---|
| x64-ssh | wedged in `dhcp-discover`, **0 packets** | DHCP lease `10.0.2.15`, `SSH:22`, TCP + banner + KEXINIT + KEX_ECDH; fails `incorrect signature` |
| aarch64-ssh | wedged in `dhcp-discover`, 0 packets | DHCP lease `10.0.2.15`, `SSH:22`; fails at banner exchange |
| i386-ssh | **QEMU exits ~10 s in** | boots and stays up; `DHCP:D`/`DHCP:R` (offer + ack); prints `DHCP:IP=00.0.0.00` — IP formatting wrong |
| aarch64-actors / -isolated | bare `> ` prompt | **unchanged** — both still sit at a bare `> `.  They build clean with the flag + stubs, but their `kernel-main` is a different composite (`:parts (:main :net :repl)`, kernel-main FIRST) and never reaches DHCP at all.  A distinct defect, upstream of everything above. |
| arm32-ssh | `Slirp: Failed to send packet, ret: -1` | **untouched** — it builds through a legacy `build-*.lisp` script, not a `build.lisp` composite, so it gets neither the flag nor the stub file |

## One bug or four?

**One root cause, four presentations.**  All four arches share
`mvm/compiler.lisp`, and all four link a `net/`-only image, so all four have
the unresolved-call-to-offset-0 defect.  The differences are downstream of what
function #1 happens to be and what the CPU does with a blown stack:

* x64 / aarch64 — recursion until `#GP`, caught by the boot handler → `hlt`.
* i386 — same recursion, but no equivalent recovery handler, so the fault
  triple-faults and `-no-reboot` makes QEMU exit.  That symptom is now gone.
* arm32 — reaches the wire (its `Slirp: Failed to send packet` proves a TX
  attempt), so its ring/descriptor arithmetic differs; it is the one arch
  whose symptom is *not* explained by this root cause alone, and it is also
  the one arch this branch does not touch.

## Left open (all pre-existing, all now visible instead of masked)

1. **x64 `incorrect signature`.** The SSH transport, KEXINIT and KEX_ECDH all
   complete; the Ed25519 host-key signature is rejected.  Exactly **one**
   `%unresolved-fn` call fires per handshake (instrumented by making the stub
   emit a byte).  A diagnostic build that gave each remaining unresolved name
   its own marker did not complete — it sent the host-side compiler into
   unbounded `COMPILE-CALL` recursion, which is itself worth a look.
2. **aarch64 banner exchange.** DHCP works, so TX and RX both work; the TCP
   connection produces no banner.
3. **aarch64 `MOD` / multiple values.** `(mod 1 64)` yields 0 on aarch64 with
   `NTH` present, and 1 on x64 from identical source.  `compile-mod` routes
   through `nth-value`, so the suspect is the aarch64 translator's
   multiple-value return for `%fixnum-truncate2`.  Worked around in the driver,
   not fixed.
4. **i386 prints `DHCP:IP=00.0.0.00`** after a successful lease — the 30-bit
   fixnum overrides mangle the octet extraction or `print-dec`.
5. **arm32** is untouched; its build path bypasses the composites entirely.
5b. **aarch64-actors / -isolated** still stop at a bare `> ` prompt.  Their
   inline `kernel-main` runs before the net files (`:parts (:main :net :repl)`)
   and never reaches `dhcp-client`, so this branch cannot have helped them; it
   is a separate entry-point/ordering problem.
6. **4 000–7 500 unresolved calls remain per net image.** They now land on
   `%unresolved-fn` and return NIL, which is safe but not right: `GENERIC-ADD`,
   `GENERIC-LOGAND`, `NUMERIC-VALUE-LESS-P`, `%SIGNAL-TYPE-ERROR` and friends
   are cold paths that will silently produce NIL if ever taken.  The real fix
   is either to link a minimal numeric runtime into these images or to teach
   the compiler not to emit fallbacks it cannot resolve.
