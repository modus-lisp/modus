# Modus as an AMD SEV-SNP guest

Status 2026-09-24: **steps 2 and 3 of the plan are built and exercised as far
as a non-SNP machine allows.  Nothing has run inside a real SNP guest yet.**

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
