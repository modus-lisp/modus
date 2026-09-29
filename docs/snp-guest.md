# Modus as an AMD SEV-SNP guest

Status 2026-09-24: **steps 2 and 3 of the plan are built and exercised as far
as a non-SNP machine allows.  Nothing has run inside a real SNP guest yet.**

## What is measured, and the DDC constraint (2026-09-24, from the user)

The goal is to attest a **DDC'd hash**: the launch measurement must pin an
artifact that Modus's own self-hosted compiler reproduces byte-for-byte from
independent hosts (the SBCL/CCL/ABCL fixpoint, `scripts/run-fixpoint-hosts.sh`).

Two facts narrow the delivery options:

1. **An SNP launch digest covers only pre-launch memory**: the firmware, and
   with the AmdSev OVMF build the hashes of what QEMU passes as `-kernel`,
   `-initrd` and `-append` (OVMF's QemuKernelLoaderFs verifies them against a
   measured hash table).  An EFI application loaded from a disk image — how
   `scripts/run-uefi-cl.sh` and `test/run-uefi-snp-test.sh` boot today — is
   **not in the measurement at all**.  The payload must arrive via `-kernel`
   (AmdSev OVMF boots a `-kernel` argument as a PE/COFF EFI application, which
   the UEFI images are) or as an IGVM image.
2. **The DDC'd artifact that exists today is the hosted Linux ELF**:
   `modus-sh --compile` emits `:linux-x64` only (`mvm/build-modus-selfhost.lisp`).
   So the bare-metal image is not DDC'd until the in-image compiler can emit
   the UEFI image from a deterministic source text and reproduce the
   SBCL-built bytes.

Hence two routes, both kept open:

| route | payload in the digest | DDC status | attestation channel |
|---|---|---|---|
| **Linux intermediate** | bzImage + initramfs containing the fixpoint `modus` ELF | DDC'd now (the ELF); kernel/initrd reproducible by the usual means | `/dev/sev-guest` ioctl from hosted modus |
| **Bare metal (the goal)** | `modus-uefi-cl.efi` via `-kernel` | needs `--compile` to grow a `:uefi-x64-cl` target and match the SBCL build byte-for-byte | GHCB guest request from the #VC-capable image |

`mvm/build-fixpoint.lisp` (the multi-arch Gen0, task #252) **runs the whole
chain again as of 2026-09-27**: `scripts/run-fixpoint.sh` takes SBCL → Gen0
(x64) → Gen1 (aarch64, 60,067,696 bytes) → Gen2 (x64, 49,283,136 bytes) →
Gen3 (aarch64) and `sha256(Gen1) == sha256(Gen3)` (22e8f227… on 9d0790c; the
first passing run, before the collector's leaf-skip fixes, hashed fbd4e2f9…).  Gen1 and
Gen2 each run with a live native collector (Gen1: 50 collections over hop 2).
The chain had been broken since April; the twelve defects and their fixes are
in commit 5f0df2c's message, plus the two collector defects 9d0790c fixes
(found by `test/run-uefi-ddc.sh`, not by the ANSI gate).  What it proves is translator determinism on bare
metal — the bytecode is compiled once by SBCL — which is the complement of the
source-level DDC below, not a substitute for it.  It is a different artifact
from the payload; the DDC'd payload is `--compile-uefi`'s output above.

**Result, 2026-09-27 (tree 460d22f, after the #252 collector fixes): all three
image modes are DDC'd, each byte-identical from SBCL and from modus-sh built
under SBCL, CCL and ABCL, and identical run to run:**

| `MODUS_UEFI_SNP` | image | md5 |
|---|---|---|
| 0 (plain UEFI) | 35,951,616 B | `8a04528c8c708244d6603c2198deb206` |
| 1 (SNP: C-bit tables, #VC handler, GHCB page, **attestation code**, 2c36c06) | 36,822,016 B | `f7c2f4df1cedb1b0fbbbf743e66bad6b` |
| 1, before the attestation code (7520e23) | 35,951,616 B | `eda7f5222c0173e418125234f64484d7` |
| **SNP mode + E1000 + SSH server + host-key binding** (`MODUS_UEFI_SNP=test MODUS_NET_BUILD=1 MODUS_SSH_BUILD=1`, 2026-09-27) | 40,268,288 B | `94062cf68337cc6840ef1d2a36bb322f` |
| test (fake-#VC self-test) | 35,951,616 B | `e511c6efa52fbec20db8e48c74d413a1` (SBCL host) |

The SNP-mode hash is the one an attestation report's measurement should be
checked against.  (The earlier 76ec6cfb was the plain image before those
collector fixes changed the emitted GC trampoline.)

## Making the bare image DDC'd: `modus-sh --compile-uefi`

`mvm/build-modus-selfhost.lisp` now bakes `boot/boot-uefi-snp.lisp`,
`boot/boot-x64.lisp` and `boot/boot-uefi-x64.lisp` next to the Linux boot
descriptor it already carried, and gains

    modus-sh --compile-uefi <full-source.lisp> <out.efi> [0|test|1]

which compiles the CL image's exact source text — dumped by the SBCL build with
`MODUS_DDC_DUMP_SOURCE=path` — to the `:uefi-x64-cl` image inside Modus, with
build-cl-repl-common's x64 knobs set verbatim (stack top, NX, bare translator
mode, GC on, kind-check off, native-code offset from the UEFI preamble) plus
the hosted `--compile`'s static-emit trio, JIT off first.
`test/run-uefi-ddc.sh` runs SBCL build → modus-sh build → two in-image compiles
and compares the bytes: SBCL vs modus-sh (the fixpoint) and run vs run.  The
md5 of the modus-sh output is the DDC'd hash for `-kernel`.

**Result, 2026-09-25 (night): THE FIXPOINT IS REAL.**  `test/run-uefi-ddc.sh`
PASSES: the SBCL static build of the UEFI CL image and `modus-sh --compile-uefi`
of the same source text are **byte-identical** (md5 `76ec6cfb…`, 35,951,104
bytes), the in-image compile is reproducible run to run, and the image boots
under OVMF with zero boot errors and answers `(+ 1 2)` → 3 and
`(list "Modus" (* 6 7) (car '(a b)))` → `("Modus" 42 A)`.  This is the first
full-size, payload-grade self-compile in the tree: the artifact that would be
measured under SEV-SNP is produced identically by SBCL and by Modus itself.

Each divergence was found by the same method — dump every function's MVM
bytecode on both sides (`modus-sh --dump-mvm`, `tmp/mvmdiff/host-dump-all.lisp`,
`tmp/mvmdiff/bcdiff.py`), mask layout-dependent operands, take the FIRST
differing function in compile order; once the bytecode matched, compare the
native image byte-for-byte and read the region the first difference sits in —
and each turned out to be host state the in-image compile lacked or in-image
state the host lacked:

| # | divergence | fix |
|---|---|---|
| 1 | a1095b7 wrote the aarch64 register map into the x64 spill table | restored (partial image → full image) |
| 2 | constants via in-image EVAL (word conversion overflows at 2^61) | literal DEFCONSTANT initforms taken as is; T/NIL too |
| 3 | `(intern x :modus.mvm)` → NIL in-image | thunk names interned with a package fallback |
| 4 | no MODUS.MVM package in modus-sh (all symbols read into CL-USER) | host build-time packages mirrored, un-marked runtime-born |
| 5 | `#.(compute-name-hash …)` unresolved in-image (bare-keyed baked fn) | runtime resolver falls back from PKG::NAME to NAME |
| 6 | `#.+op-nop+` UNBOUND in MODUS.MVM → reader split MVM-INTERPRET into 144 forms | the text's integer DEFCONSTANTs pre-bound in MODUS.MVM |
| 7 | the reader's `#.` leaked mvm-eval's mode flags into the rest of the compile | reader restores them |
| 8 | nested mvm-eval mid-compile reset the shared compiler globals (gensym, multi-shape, macro tables) | guard: snapshot, swap fresh macro tables, restore |
| 9 | intern reused a symbol whose package slot was stale (CL-USER) | re-homed on reuse |
| 10 | SBCL quasiquote structs vs Modus `(backquote …)` lists — every macro body differed | one `expand-backquote` for both representations |
| 11 | `#+sbcl` lambda in mvm-compile-all (one closure) | feature-free diagnostic |
| 12 | Modus `find-package "CL-TEST"` → CL-USER; host NIL | reader switches package only on exact name/nickname |
| 13 | `sb-impl::comma` / `sb-int:quasiquote` spelled as symbols in compiler.lisp | reached by name at run time |
| 14 | float literals rounded twice (`%bignum-to-float` then `%float-div`): `1.7976931348623157d308` read as +inf, 14 init thunks differed | `%exact-ratio-to-double` — correctly rounded from the integer ratio, exact power-of-two scaling |
| 15 | `common-lisp:t` read as a symbol interned in CL, not the constant (COERCE) | qualified T/NIL are the booleans, as in the unqualified branch |
| 16 | a `#.` evaluated mid-static-build with `*static-build-p*` still T: its string literal took the serialized `:li-const` path and the interpreter read a runtime-pool entry instead — every `#.(compute-name-hash "X")` was the hash of IF | the nested eval runs as a runtime eval |
| 17 | vector literals past element 255: `:obj-set`'s imm8 index wrapped, so boot-x64's 347-byte fault-stub blob came out as its own last 91 bytes (in modus-sh itself, hence in every image it built) | elements ≥ 256 stored through a register index (`:aset`) |
| 18 | the source file read as Latin-1 bytes in the image (an em dash = 3 chars) while SBCL reads UTF-8: docstrings in the constant pool were longer, every `LI-CONST` address after them shifted, and the host's embedded source blob was LOSSY (one truncated byte per char) | the image decodes UTF-8 on read; `embed-source-blob` encodes UTF-8 on both sides, so the blob is the file's own bytes |

Rows 14–18 are 9c6a1b2 and the commit after it.  The x64 CLI image is
affected by 17 (any `#(…)` literal longer than 255 elements was silently
wrong in every image) and 18 (`embed-source-blob`), and by the reader changes
in 14–16.  **ANSI gate, 64 shards, same box, base 7a9a87b vs fix 6d311fc,
per-file name-stable: 18365 = 18365, NET 0, zero regressing files, zero
gaining files** (this branch's own baseline; the main headline is measured
on a different tip).

**And it is a DIVERSE double-compile, not just a self-compile.**  modus-sh was
built under three different host Lisps — SBCL, CCL 1.12 (`lx86cl64`) and ABCL
1.9.2 (JVM) — and each `--compile-uefi` of the same source produced the same
image, md5 `76ec6cfb815bfbced196016636556142`, twice each:

    MODUS_SH=tmp/hosts/modus-sh-ccl  MODUS_DDC_WORK=tmp/uefi-ddc-ccl  test/run-uefi-ddc.sh  # PASS
    MODUS_SH=tmp/hosts/modus-sh-abcl MODUS_DDC_WORK=tmp/uefi-ddc-abcl test/run-uefi-ddc.sh  # PASS

The three host-built modus-sh binaries differ (46,098,787 / 46,246,457 /
46,162,657 bytes — host codegen); what they compile does not.  That hash is
the thing an SNP launch measurement can be checked against, and it no longer
depends on trusting any one host compiler.

## The plan

1. **Launch** — boot Modus as the measured payload.  SNP needs firmware, so the
   route is OVMF (the AmdSev build, which measures the loaded image) and the
   existing PE32+ UEFI image (`mvm/build-uefi-repl.lisp`, `boot/boot-uefi-x64.lisp`).
   The multiboot `-kernel` path is not usable.
2. **Detect and encrypt** — C-bit in the page tables, CR3 included.  DONE.
3. **#VC handler** — CPUID, MSR, port I/O and RDTSC(P) trap to the guest; handle
   them through the GHCB.  DONE (IOIO decode verified on plain hardware; the
   VMGEXIT itself is unexercised).
4. **Shared buffers** — E1000 rings must be in shared (C=0) memory.  The mapping
   is DONE; the driver is unchanged and untested under SNP.
5. **Attestation** — request a report over the SNP guest channel (AES-GCM with
   the VMPCK from the secrets page), bind the SSH host key hash in `report_data`.
   NOT STARTED.

## The UEFI-bootable CL image: `mvm/build-uefi-cl-repl.lisp`

The bare-metal CL image (`build-x64-cl-repl`, the real CL stack, E1000 with
`MODUS_NET_BUILD=1`) wrapped as PE32+: `boot-uefi-x64.lisp`'s entry stub (GDT
in boot-x64's layout, no console tables, padded to 8 KB) hands over at
0x100000 to boot-x64's own 64-bit kernel entry, then cross.lisp's JMP and the
native code — the multiboot image from its 64-bit entry onward.  The
descriptor is `:uefi-x64-cl` with `:load-addr` = 0x100000 − pad so every VA
cross.lisp computes is right.  boot-x64's `emit-x64-interrupt-setup` re-adds
IDT vector 29 after its own LIDT when an SNP mode is on.  The shared 2 MB
page for this image is 0x0C000000 (its NIC rings), GHCB 0x0C1FF000.
Boot: `scripts/run-uefi-cl.sh IMAGE.efi '(+ 1 2)'`.

Measured 2026-09-24 (`MODUS_UEFI_SNP=test MODUS_NET_BUILD=1`, 42.5 MB): boots
under plain OVMF, prints the `VC+wi5` witness before the banner, evaluates
`(+ 1 2)` → 3 and `(list (lisp-implementation-type) (* 6 7))` → `("Modus" 42)`.
The toy UEFI image is still byte-identical with the flag off after the stub
emitter grew its layout knobs.  With an E1000 attached, DHCP gets no lease
under OVMF — and **neither does the same image booted by multiboot on this
QEMU 7.2**, so that is environmental/pre-existing, not the UEFI path.

## What is in the tree

`boot/boot-uefi-snp.lisp`, gated on `*x64-snp-mode*` (`MODUS_UEFI_SNP` env for
`build-uefi-repl`: unset/`0` → NIL, `test` → `:test`, anything else → `:snp`).

| mode | what the UEFI stub does |
|---|---|
| NIL | nothing.  Byte-identical to the pre-change tree (md5 `56f4213f…` on both, measured). |
| `:snp` | after ExitBootServices: CPUID 8000_001F (SEV) → MSR C001_0131 (SNP active) → GHCB MSR SEV_INFO (C-bit position); identity map with the C-bit on every entry and on CR3, except the 2 MB page at 0x05000000; PVALIDATE-rescind + MSR-protocol Page State Change of all 512 pages of that region to SHARED; register the GHCB (0x051FF000); zero it; install an IDT at 0x18000 with vector 29 → handler at 0x19000; clear `fb_valid` so the console is serial-only. |
| `:test` | same handler, IDT and boot order on a plain machine; no C-bit, no PSC.  Vector 29 is a thunk that pushes the IOIO exit code, and the stub raises `INT 29` in front of real port instructions.  The "VMGEXIT" performs the port operation from the GHCB fields. |

**The shared region.**  One 2 MB page, 0x05000000–0x051FFFFF, mapped C=0.  It
already holds the E1000 RX/TX descriptors and buffers (`net/arch-x86.lisp`,
0x05000000–0x05060000+) and now the GHCB at its top.  Choosing a whole PD entry
means the 2 MB-only page tables never need a 4 KB split.

**The #VC handler** (`assemble-vc-handler`) is hand-assembled by a tiny
labelled byte assembler in the same file and copied to 0x19000 by the stub.
It handles IOIO (all eight IN/OUT forms, with and without the 0x66 prefix),
CPUID, RDMSR/WRMSR, RDTSC and RDTSCP; anything else, or a non-zero
`SW_EXITINFO1`, is fatal (MSR-protocol terminate, then HLT).  GHCB field
offsets are those of `struct ghcb` in Linux (`rax` 0x1F8, `rcx` 0x308, `rdx`
0x310, `rbx` 0x318, exit code/info at 0x390/0x398/0x3A0, valid bitmap 0x3F0,
version 0xFFA, usage 0xFFC).  No translator change was needed: `translate-x64`
keeps emitting IN/OUT inline and the handler makes them work.

## What was measured

`test/run-uefi-snp-test.sh`:

* `:test` image under plain OVMF prints `VC+wi5` before the REPL banner:
  two `OUT DX,AL`, an `IN AL,DX` (LSR bit 5 seen), an `IN AX,DX` with the
  0x66 prefix, an `IN AL,imm8`, and the handler entry counter reading exactly 5.
  The REPL prompt appears afterwards.
* The flag-off image contains no trace of the handler; the `:test` image does.
* The handler disassembles cleanly with `objdump -b binary -m i386:x86-64`.

## What is NOT established

* **Nothing has executed a VMGEXIT.**  The `:snp` arm has never run.  The host
  here is a bare EPYC 7C13 (Milan, SNP-capable silicon) with a Proxmox 6.8
  kernel (no `/dev/sev`; SNP host support landed in 6.11) and QEMU 7.2 (SNP
  guest launch needs 9.1+), and no AmdSev OVMF build is installed.
* **The C-bit-on-non-SEV case cannot be boot-tested**: bit 51 is reserved on a
  non-SEV CPU, so the `:snp` arm's page-table path is only reachable on real SNP.
  The runtime `active` flag keeps an `:snp` build bootable on a plain machine
  (the C-bit mask is 0 and PSC is skipped) — that fallback IS tested by the
  `:test` build's shared ordering, but not by an `:snp` build itself.
* **Detection uses CPUID under the firmware's #VC handler.**  Between
  ExitBootServices and our own IDT load, OVMF's IDT and GHCB are still in
  place, so the CPUID 8000_001F should be serviced by OVMF.  If that turns out
  false on real hardware, the fallback is the SNP CPUID page OVMF publishes;
  the C-bit position itself already comes from the MSR protocol, not CPUID.
* **PVALIDATE on a page OVMF validated as part of a 2 MB RMP entry** relies on
  the hypervisor smashing the entry on the resulting #NPF; Linux does the same.
* **The GOP framebuffer is disabled under SNP** rather than mapped shared.
  Mapping it shared needs its address (known only after the GOP query) in a
  C=0 page, i.e. a second shared PD entry chosen at runtime.

## Known pre-existing defects met on the way

* The UEFI REPL (`mvm/repl-source.lisp`, the legacy mini-Lisp) echoes input
  at the `>` prompt and never prints a result; `scripts/run-uefi-repl.sh
  '(+ 1 2)'` exits 0 with no output (a silent false pass, already noted in
  commit 36ace50).  The SNP witness therefore uses the boot banner and prompt,
  not an evaluation.
* The UEFI image is the toy Lisp, not the CL stack with SSH.  "Attested SSH"
  needs a UEFI build of the SSH image (`build-x64-ssh` is multiboot today).

## Next

1. A host: kernel ≥ 6.11 with SNP, QEMU ≥ 9.1, `OVMF.amdsev.fd`; then
   `MODUS_UEFI_SNP=1` and `-machine confidential-guest-support=sev0 -object
   sev-snp-guest,id=sev0,cbitpos=51,reduced-phys-bits=1`.
2. E1000 under SNP: the rings are in the shared page and the SSH image builds
   and serves under plain OVMF (`test/run-uefi-ssh.sh`, `MODUS_NET_BUILD=1
   MODUS_SSH_BUILD=1`); the NIC's BAR MMIO still needs a shared mapping or MMIO
   #VC support on a real SNP host.
3. Attestation — BUILT 2026-09-27, awaiting a host to exercise the PSP path:
   `crypto/snp-guest.lisp` (guest-message framing, AES-256-GCM under VMPCK0,
   MSG_REPORT_REQ/RSP, report accessors; the in-tree AES-GCM compiles in-image
   unmodified and matches python-cryptography), `net/snp-attest.lisp` (secrets
   page, shared request/response pages below the GHCB, SNP_GUEST_REQUEST over
   the GHCB, `SNP-ATTESTATION-REPORT`), and in the stub: the EFI CC-blob table
   scan (`emit-snp-find-secrets`, before ExitBootServices) and a callable
   VMGEXIT routine at 0x19E80.  `test/run-snp-guest-msg.sh` round-trips the
   framing against a fake PSP on host and in-image (6/6, tamper-rejecting);
   `test/snp/verify-report.py` is the verifier (report_data vs SHA-512 of the
   host key, measurement vs the DDC'd hash, ECDSA P-384 vs a VCEK).  Under
   plain OVMF the image's `(snp-attest-selftest)` reports active 0, a callable
   routine and status `(:NO-SNP)`.

   **The SSH binding and the remote verifier are built (2026-09-27), and the
   whole chain except the root of trust runs here.**  `net/snp-ssh-attest.lisp`
   (SNP + SSH images only): `report_data = SHA-512(the 32 raw bytes of the
   Ed25519 host public key)`; `(snp-attest-ssh)` prints `SNP-HOSTKEY`,
   `SNP-REPORT-DATA` and either the report as 19 `SNP-REPORT` hex lines or
   `SNP-STATUS`.  The CL image's SSH `ssh-eval-line` now sends what a form
   PRINTS ahead of its `= VALUE` line, so those lines reach the client.
   `test/snp/attested-ssh-client.sh HOST PORT [--vcek C] [--measurement H]` is
   the verifier's side: it takes the host key from OpenSSH's own known_hosts
   record of the handshake (never from anything the server prints), asks the
   session for the report, requires printed key == handshake key and printed
   report_data == SHA-512 of it, then runs `verify-report.py` with that key
   (which also accepts `--hostkey-hex` and the `ssh-keyscan` base64 blob).
   Exit 0 attested, 2 bound-but-no-report, 1 mismatch.  Measured against the
   SNP-test SSH image under plain OVMF: both equalities `yes`, then
   `no report: SNP-STATUS (NO-SNP)`, exit 2 — the correct answer without a PSP.

   The root of trust is stood in for by OUR OWN KEY so the rest can be
   exercised: `fake-psp.py keygen` makes a P-384 key + self-signed cert (the
   VCEK stand-in) and `respond ... vcek-key.pem` signs the report exactly as
   the PSP does (ECDSA P-384 / SHA-384 over bytes 0..0x2A0, r and s as 72-byte
   little-endian fields, sig_algo 1).  `test/run-snp-guest-msg.sh` grew an
   attested arm on host and in a hosted modus image: a fresh Ed25519 host key,
   request -> signed report -> `verify-report.py` PASS with the key file and
   with its ssh-keyscan form; a different host key, a different measurement, a
   flipped signature byte and another signer's certificate each REJECTED
   (12 of 12).  What a real host adds is only the VCEK from AMD KDS in place
   of `vcek.pem`, and `(:NO-SNP)` becoming a report.

   Also this round: e1000-state-base (host PRIVATE key at +0x710, PRNG at
   +0x62C) and ssh-conn/ssh-ipc moved OUT of the 2 MB shared region on x64
   (0x0C200000 / 0x0C280000 / 0x0C320000) — the last SSH secrets that were in
   host-readable memory; the plain SSH image still passes `test/run-uefi-ssh.sh`.

   The SNP status block moved from 0x600180 to 0x1A000: 0x600180.. lies inside
   the 42 MB CL image's native code (%RESOLVE-OUTPUT-STREAM) and the stub was
   overwriting live code; the UEFI framebuffer words at 0x600100..0x600150 have
   the same exposure and are still there (the CL image is serial-only).
