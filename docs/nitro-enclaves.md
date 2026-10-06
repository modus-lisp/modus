# AWS Nitro Enclaves — the hosted-Linux route to attested Modus

Written 2026-10-06.  Status: **everything that can be built and tested without an
enclave is built and tested; nothing has run inside an enclave yet.**

## Why this route, and what it attests

An enclave boots a Linux kernel AWS supplies, so the thing we attest is the
hosted `./modus` static ELF inside the enclave's ramdisk, not the bare-metal
image — the "intermediate Linux kernel" route.  The hosted ELF is reproducible
(modus-sh DDC), so our half of the measurement is a known hash.  What Nitro
measures (SHA-384, from `eif_build`):

| PCR | covers |
|---|---|
| PCR0 | the whole EIF: kernel, cmdline, both ramdisks |
| PCR1 | kernel + cmdline + the init ramdisk (AWS's `init` + `nsm.ko`) |
| PCR2 | the application ramdisk: `/modus`, `/tars/*.tar`, `/cmd`, `/env` |

Packages ride INSIDE the measured application ramdisk as tarballs and are
installed by the `/cmd` line at start (no save-and-die core on this route yet;
the EIF's ramdisk is where a core would go, which is simpler than the PE-section
work still pending for SNP).

## What is built

* **`kiln image nitro --with=…`** (modus-lisp/kiln): builds the hosted CLI,
  resolves the package closure to tarballs, writes two deterministic newc cpio
  ramdisks (`test/nitro/mkcpio.py`: mtime 0, uid 0, inode by order — no Docker,
  no host state), fetches AWS's boot blobs (`bzImage`, `bzImage.config`,
  `cmdline`, `init`, `nsm.ko` from aws-nitro-enclaves-cli, pinned by sha256 in
  the manifest), builds `eif_build` from the aws-nitro-enclaves-image-format
  crate, and emits `modus.eif` + `pcrs.json` + `manifest.json`.  No nitro-cli
  and no Docker anywhere.  **Two builds give identical PCRs** (the EIF files
  differ in 13 bytes: the header CRC and metadata; the measured sections are
  byte-identical).
* **`lib/cbor.lisp`**: RFC 8949 encoder/decoder for the NSM's subset (ints, byte
  and text strings, arrays, maps, tags, simple values).  Every RFC head-size
  vector round-trips, and the attestation request encodes byte-for-byte as
  Python's cbor2 does (`test/cbor-probe.lisp`).
* **`net/nsm-attest.lisp`** (hosted x64, baked): `/dev/nsm` via the one ioctl
  (`NSM_IOCTL` = `_IOWR(0x0A, 0, NsmMessage)` = `#xC0200A00`, two iovecs);
  `nsm-describe`, `nsm-attestation-document user-data nonce [public-key]`,
  `nsm-print-document`, `nsm-attest-selftest` (answers `(:NO-NSM -2)` on a
  machine without the device, measured).  Its syscalls take addresses as
  ARGUMENTS and never read a global in the same function — the documented
  runtime-compiled `syscall3` trap bit here first (`open` returned 2, its own
  number) and `%mmap-shared-page` echoes its argument from evaluated code; the
  baked module has neither problem.
* **vsock** in `net/hosted-sockets.lisp`: `vsock-connect cid port`,
  `vsock-listen port backlog`; `socket-accept`/`-send`/`-recv`/`-close` are
  shared with AF_INET.  **`vsock-repl port`** (net/nsm-attest.lisp) is the
  enclave console: one form per line, `= VALUE` or `! CONDITION` back.
* **`test/nitro/fake-nsm.py`**: a stand-in NSM (our own P-384 root and leaf)
  that signs a document with the REAL layout (COSE_Sign1 tag 18, protected
  `{1: -35}`, payload module_id / digest / timestamp / pcrs 0..15 / certificate
  / cabundle / public_key / user_data / nonce, ECDSA P-384 over the
  Sig_structure) — the same role fake-psp.py plays for SEV.
* **`test/nitro/verify-attestation.py`**: the off-box verifier — chain to
  `--root` (default `test/nitro/aws-nitro-root.pem`, AWS's published root,
  fingerprint `64:1A:03:21:…:BB:5B`), signature, PCR0/1/2, nonce, user_data
  == SHA-512 of a host key given raw, hex or as the OpenSSH blob (the SNP
  binding, unchanged).  Measured: the stand-in document PASSES against its
  root with the EIF's PCRs and a bound key; FAILS against AWS's root, against a
  wrong PCR0, and with one signature byte flipped.  Hosted Modus parses the
  same document and prints its PCRs and bound fields.

## What a real enclave adds, and the first run

```
nitro-cli run-enclave --eif-path modus.eif --memory 3072 --cpu-count 2 --enclave-cid 16 [--debug-mode]
socat TCP-LISTEN:5000,reuseaddr,fork VSOCK-CONNECT:16:5000      # on the parent
printf '(+ 1 2)\n(nsm-attest-selftest)\n' | nc 127.0.0.1 5000
```

The hosted ELF maps two 896 MB semispaces plus a guard, so the enclave needs
**at least 2560 MB**; the small-semispace build knob (`MODUS_X64_MIDPOINT`)
exists but a 96 MB build faults at boot and is unchased.  `--debug-mode` gives a
console but makes PCR0..2 read as zero in the document — expected, not a bug.

Verification of the first real document: `test/nitro/verify-attestation.py
DOC.cose --pcr0 … --pcr1 … --pcr2 … --nonce …` with the values from
`pcrs.json`.  The nonce comes from the verifier; the host-key binding waits on
the hosted SSH transport, which is the long pole on this route.

## Not done

* nothing has run inside an enclave; the first run is an EC2 instance away;
* no hosted SSH transport (the SSH-2 server is bound to the bare-metal E1000
  stack), so the first milestone is attested Modus with a vsock REPL;
* no save-and-die core in the EIF (packages install at start instead).
