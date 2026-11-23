# An attested release of everything

The goal: one audited source tarball, one small diverse-double-compiled builder, and a
confidential VM that builds every modus artifact from that tarball and attests what it built.
Anyone who trusts their own audit of the tarball and their own DDC of the builder can then trust
every artifact, without building any of them; anyone who distrusts the attestation can still
rebuild any one artifact and compare.

## The chain

1. **The source.**  One tarball of the whole modus-lisp org, `modus-lisp-SRC.tar`, named by its
   SHA-256, `H`.  Auditing it is the human part; everything below is mechanical.  The tarball
   carries everything a build reads: every repository's sources and the pinned binary blobs
   (the Raspberry Pi firmware), and the one build recipe (below).

2. **The builder, diverse-double-compiled.**  The x86-64 builder CLI (`MODUS_BUILDER=1`) built
   from the tarball **with `H` as a build input**: `MODUS_SOURCE_SHA256=H`.  `H` cannot be part
   of the source it hashes, so like `MODUS_SOURCE_COMMIT` today it is a setting the image
   records -- and the builder's release mode refuses any tarball whose hash is not `H`.  A
   verifier DDCs it by building it with SBCL, CCL and ABCL (each `MODUS_SOURCE_SHA256=H`) and
   getting the same bytes from all three, on hardware of their choosing.  That is one image,
   about 20 minutes per host, not every target: the DDC stays light.  It shows the builder is
   the tarball's compiler and nothing else, which is what defeats a "trusting trust" compiler;
   it does not show the tarball is benign, which is the audit's job.

3. **The confidential VM.**  The DDC'd builder boots as an AMD SEV-SNP guest.  The launch
   measurement covers what it boots -- the builder, and so `H` -- and so the attestation report
   proves which builder ran and which tarball it would accept.  The verifier computes the
   expected measurement in advance from the DDC'd image (kiln's measured-boot images already
   work this way; docs/nitro-enclaves.md and the SEV-SNP work for the attestation side).

4. **The build.**  Inside the guest the builder fetches the tarball over any network -- integrity
   comes from `H`, confidentiality is not needed -- checks its hash, stops on a mismatch, and
   runs the build recipe: every hosted CLI (each OS and architecture), every bare-metal image,
   the web build, the boot media and `modus.com`, with nothing from outside the tarball.

5. **The attestation.**  The guest hashes every output and puts the root of a Merkle tree of
   (name, SHA-256) pairs in the report's 64-byte `REPORT_DATA`.  One report covers the whole set;
   it verifies against AMD's chain (ARK -> ASK -> VCEK).  The outputs, the tree and the report go
   to the release location (the dist repository today).

6. **The cross-check.**  Every output is reproducible: the same tarball and recipe give the same
   bytes on any host.  So the attestation is a convenience, not the only evidence -- a verifier
   who does not trust AMD rebuilds any output and compares it with the attested hash.

What is left to trust: AMD's PSP and firmware, for anyone who relies on the report; at least
one of the diverse compilers the verifier uses for the DDC; and the audit.

## The build recipe

One file in the tarball names every output and how it is built: script, settings, output name.
The measurement covers the builder and `H`, the tarball covers the recipe, so the report
implies the exact procedure.  The settings every output shares: `MODUS_SOURCE_SHA256=H`,
`MODUS_SOURCE_COMMIT` per repository as the tarball records it, `MODUS_BUILD_ID` derived
from `H`, `MODUS_FASL=0`, and no vessel or network outside the tarball.

## What exists, and what does not yet

Exists:
- reproducible builds on every host tried: the x86-64 Linux CLI and `modus.exe` are the same
  bytes built by SBCL, by the Linux builder and by the Windows builder; the builder builds
  itself (docs/building.md, release `bin/20261123` in the dist repository);
- DDC of the CLI: SBCL, CCL and ABCL agree;
- `modus.com` from the two CLIs, the same bytes on every host;
- a trimmed source set (`scripts/make-source-tar.lisp`) with a deterministic tar writer;
- modus as a measured, attested SEV-SNP guest (VPSBG) and as an attested Nitro enclave.

To do:
- [ ] `MODUS_SOURCE_SHA256`: recorded in the image, and a builder release mode that fetches,
      checks and refuses on mismatch
- [ ] the org tarball as a deterministic artifact (sorted, fixed metadata, every repository at
      a recorded commit), written by modus
- [ ] the builder carries every translator: i386, 68k, arm32, PowerPC, RV64 beside x86-64,
      AArch64 and RV32, each target's output proved identical to SBCL's
- [ ] reproducibility proved for every target: Darwin AArch64 (in progress), the other hosted
      CLIs, every bare-metal image
- [ ] no external tools in any output's build:
  - [ ] macOS: a Mach-O writer and ad-hoc signer in Lisp, and the C wrapper's job (host/macos/)
        done in modus-emitted code
  - [ ] 68k boot blocks and loader assembled by modus (today binutils)
  - [ ] PC boot: a BIOS boot sector and loader of our own (today GRUB, xorriso, mtools)
  - [ ] Pi card and other disk images through fs-fat / fs-apple (today mtools)
  - [ ] the web build
- [ ] the recipe file, and one entry point that runs it
- [ ] the SNP guest: the builder booting under SEV-SNP with the memory a build needs, the
      fetch / check / build / hash loop, the Merkle root in `REPORT_DATA`
- [ ] a verifier: report chain, expected measurement from the DDC'd image, root over the
      published outputs
