# An attested release of everything

The goal: one audited source tarball per release, one small diverse-double-compiled builder
that serves many releases, and a confidential VM that builds every modus artifact from the
tarball and attests what it built.
Anyone who trusts their own audit of the tarball and their own DDC of the builder can then trust
every artifact, without building any of them; anyone who distrusts the attestation can still
rebuild any one artifact and compare.

## The chain

1. **The source.**  One tarball of the whole modus-lisp org, `modus-lisp-SRC.tar`, named by its
   SHA-256, `H`.  Auditing it is the human part; everything below is mechanical.  The tarball
   carries everything a build reads: every repository's sources and the pinned binary blobs
   (the Raspberry Pi firmware), and the one build recipe (below).

2. **The builder, diverse-double-compiled -- once, not per release.**  An x86-64 builder CLI
   (`MODUS_BUILDER=1`), a long-term-support builder: it knows nothing about any one release.
   A verifier DDCs it by building it with SBCL, CCL and ABCL and getting the same bytes from all
   three, on hardware of their choosing -- one image, about 20 minutes per host.  That shows the
   builder is its source's compiler and nothing else, which defeats a "trusting trust" compiler;
   it does not show any source is benign, which is the audit's job.

   It can build later releases because every build already gives the same bytes on every host:
   to release N+1's build scripts, the LTS builder is one more Common Lisp, like SBCL or CCL, so
   its outputs are the ones any other host would make.  The condition is the portability rule
   the build keeps anyway -- release N+1's scripts run on the LTS builder's Lisp.  When a release
   needs a newer builder, that is when a new LTS is cut and DDC'd.

3. **The confidential VM, told the tarball by HOST_DATA.**  The LTS builder boots as an AMD
   SEV-SNP guest.  The launch measurement covers the builder -- the same for every release.
   The tarball's hash `H` goes in `HOST_DATA`: the 32 bytes the host sets at `SNP_LAUNCH_START`,
   which the guest cannot change and every attestation report carries -- exactly a SHA-256.
   The guest learns `H` by asking for its own report at start-up.  So the report proves which
   builder ran (`MEASUREMENT`, computed in advance from the DDC'd image) and which tarball it
   was given (`HOST_DATA`).  (A measured kernel command line would also do; `HOST_DATA` needs
   nothing from the boot path and cannot be confused with other arguments.)

4. **The build.**  Inside the guest the builder fetches the tarball over any network -- integrity
   comes from `H`, confidentiality is not needed -- checks its hash, stops on a mismatch, and
   runs the build recipe: every hosted CLI (each OS and architecture), every bare-metal image,
   the web build, the boot media and `modus.com`, with nothing from outside the tarball.

5. **The attestation.**  The guest hashes every output and puts the root of a Merkle tree of
   (name, SHA-256) pairs in the report's 64-byte `REPORT_DATA`.  One report covers the whole set:
   `MEASUREMENT` the LTS builder, `HOST_DATA` the tarball, `REPORT_DATA` the outputs; it
   verifies against AMD's chain (ARK -> ASK -> VCEK).  The outputs, the tree and the report go
   to the release location (the dist repository today).

6. **The cross-check.**  Every output is reproducible: the same tarball and recipe give the same
   bytes on any host.  So the attestation is a convenience, not the only evidence -- a verifier
   who does not trust AMD rebuilds any output and compares it with the attested hash.

What is left to trust: AMD's PSP and firmware, for anyone who relies on the report; at least
one of the diverse compilers the verifier used for the LTS builder's DDC; and the audit of the
release's tarball.  A verifier repeats the DDC per LTS, and the audit per release.

## The build recipe

One file in the tarball names every output and how it is built: script, settings, output name.
The measurement covers the builder, `HOST_DATA` the tarball, and the tarball the recipe, so the
report implies the exact procedure.  The settings every output shares: `MODUS_SOURCE_COMMIT` per
repository as the tarball records it, `MODUS_BUILD_ID` derived from `H`, `MODUS_FASL=0`, and no
vessel or network outside the tarball.

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
- [ ] a builder release mode: read `H` from its own report's `HOST_DATA`, fetch the tarball,
      check it, refuse on mismatch, run the recipe
- [ ] the first LTS builder, and its DDC published (SBCL, CCL, ABCL)
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
- [ ] a verifier: report chain, `MEASUREMENT` against the DDC'd LTS image, `HOST_DATA`
      against the audited tarball, `REPORT_DATA` against the published outputs
