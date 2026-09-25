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

`mvm/build-fixpoint.lisp` (the multi-arch SSH Gen0, task #252) **builds again
on this tree** — a 49 MB Gen0 at metadata VA 0x3000000 — so the build-time
wall my earlier note recorded is gone.  `run-fixpoint-ssh.sh x64 x64` was
re-run: the plain Gen0 boots under QEMU, prints its bring-up markers through
`b0`, and never emits Gen1 (timeout at 400 s); the SSH-mode Gen0 build dies
host-side in SBCL (the runner keeps only a backtrace tail).  So #252 has
moved from a build-time wall to a runtime one.  It is a different artifact
from the payload; the DDC'd payload is `--compile-uefi`'s output above.

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

**Result, 2026-09-25.**  The pipeline runs end to end; the fixpoint does not
hold yet, and the reasons are now enumerated rather than guessed.

1. **A real x64 self-host regression on main was found and fixed.**  The first
   in-image compile emitted a 9.95 MB partial image ("Unknown register: 6" on
   `(mov V9 V6)`).  `git bisect run` over 339 commits with the fixed source text
   → a1095b7 (2026-09-18): its "register map co-inits in five build scripts"
   sweep wrote the aarch64 local-register map (6 7 4 5 8) into the two co-inits
   that build `*vreg-to-x64*` (modus-sh's own x64 emit, and the aarch64 CLI's
   x64 cross-emit); x64 spills V9..V15 and needs NIL there.  Fixed in both.
   The hosted `./modus` x64 JIT was never affected (0 fallbacks measured).
   `scripts/ws5-gate.sh` missed it because its self-compile is a tiny program.
2. **Constants the host knows, the image does not.**  142 `+OP-*+` /
   `+GC-REGION-0-BASE+` references compiled as implicit globals in-image (used
   before their DEFCONSTANT in the stream; SBCL folds them from the host
   image).  A constants forward block is now prepended to the CL image's text
   (`%ddc-constants-forward-block`, 229 integer-literal defconstants); the
   +OP-*+ class is gone.  Still escaping: `+GC-REGION-0-BASE+` (8), and CL's
   `MOST-POSITIVE-FIXNUM` / `MOST-NEGATIVE-FIXNUM`, which the host folds as CL
   constants.
3. **The in-image compile is a different BUILD CONFIGURATION, not just a
   different compiler.**  `--compile-uefi` forces `*static-build-p*` T (as the
   hosted `--compile` does), and under that flag every DEFCONSTANT registers an
   `INIT-+X+` boot thunk (compiler.lisp toplevel DEFCONSTANT clause): the
   in-image image has 266 INIT thunks and 60 more TOPLEVEL thunks than the
   SBCL one (5116 vs 4791 named functions after filtering numbered closures),
   yet is 3.4 MB smaller (37.9 vs 41.2 MB).  For byte identity both sides must
   run ONE configuration — the `*mvm-eval-runtime-p*` / `*static-build-p*`
   consolidation already recorded as the WS5 "one flag" work.
4. **936 call sites resolve to a callee named NIL in-image** (host: 26
   unresolved, none NIL), so the modus-sh-built image boots to its `MODUS-CL`
   marker and dies on an undefined function.  A flag-gated trace at the
   assembler's `:call` site (`*ddc-trace-nil-callee*`) was added but did not
   fire, so the sites are still unattributed.
5. **144 bytes of run-to-run nondeterminism** remain, in 4-byte groups inside
   `%MUL-LIMBS-MAG`, `BIGNUM-EQL`, `%ANY-TO-FLOAT`, `GENERIC-MULTIPLY`,
   `EMIT-LI-TAGGED`, `COMPILE-FORM` … — the emit-u64 high-dword class recorded
   in `reference_selfcompile_nondeterminism`, at a site other than the one
   fixed then (bignum/float immediates).

So the DDC'd bare-metal payload needs, in order: (4) and (5) fixed in the
in-image compiler, then (3) decided (one configuration for both builders),
then `test/run-uefi-ddc.sh` green.  The measurement instrument is done and is
what found (1).

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
2. UEFI build of the SSH image; E1000 under SNP (rings are already in the
   shared page; the NIC's BAR MMIO needs a shared mapping or MMIO #VC support).
3. Attestation: locate the secrets page and CPUID page (EFI config table
   `SEV-SNP secrets` GUID), MSG_REPORT_REQ over the GHCB guest-request NAE
   (0x80000011) encrypted with VMPCK0 via `crypto/gcm.lisp` (needs the MVM
   adaptation), `report_data` = SHA-512 of the Ed25519 host public key.
   Verifier side: `seal` has P-384 ECDSA verify (SBCL-hosted); VCEK chain
   from AMD KDS.
