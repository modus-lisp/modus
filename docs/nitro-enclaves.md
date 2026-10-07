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


## The first real run (2026-10-07, m5.xlarge us-east-2) — PASS

`kiln image nitro --with=alexandria` → `modus.eif`; `nitro-cli describe-eif`
on the parent computed the SAME PCR0/1/2 as eif_build had offline.  Then, in a
**non-debug** enclave (2 vCPU, 3072 MB): alexandria installed from the
measured ramdisk, `(alexandria:iota 3)` answered `(0 1 2)` over the vsock
console, and `nsm-attestation-document` with a nonce and user data returned a
4469-byte COSE_Sign1 that `test/nitro/verify-attestation.py` passes on every
check: chain to the AWS root, ES384 signature, **PCR0/1/2 equal to the EIF's
offline values**, nonce echoed, user_data matched.  The document and the PCRs
are in `test/nitro/records/`.  Debug mode first (`--debug-mode`, console
visible): same document shape with PCR0..3 all zero, as AWS documents.

Four things the enclave taught that no off-box test could, in the order hit:

1. **The application ramdisk needs `rootfs/`.**  AWS's `init` does
   `mount --bind /rootfs /rootfs`, moves it to `/`, chroots, then mounts
   proc/sys/dev/run/tmp INSIDE it (its `ops[]` table) and execs `/cmd`'s argv.
   `cmd` and `env` stay at the ramdisk root; everything the program sees
   goes under `rootfs/`, with those five directories present.  Missing
   `rootfs`: init dies at 0.23 s and the kernel panics ("Attempted to kill
   init", exit 2); missing `run`: the same one mount later.
2. **`/cmd` is one argv entry PER LINE.**  Splitting the command on spaces
   cut `(install-tarball "/tars/x.tar")` in two → `READER-ERROR` on the
   first `--eval`.
3. **The NSM ioctl's response iovec must offer `NSM_RESPONSE_MAX_SIZE`
   (0x3000).**  Offering 100, 163, 164, 200, 1024, 4096 or 8192 bytes: the
   ioctl returns 0 and writes back length 0 — indistinguishable from an
   empty reply, and `cbor-decode` then says "truncated at 0".  Offering
   0x3000 or more: DescribeNSM answers 164 bytes, GetRandom 278, an
   attestation 4463.  (The driver source says `min(user len, resp len)`; the
   device behind it evidently does not.)  `+nsm-response-max+` now.
4. **The document's payload is an indefinite-length CBOR map** (`bf …`), and
   `lib/cbor.lisp` refused indefinite lengths; it decodes them now (strings,
   arrays, maps, RFC 8949 3.2).

Two instrument notes.  `syscall3` from RUNTIME-compiled code (a `--eval` or a
vsock form) returns the syscall NUMBER — `(syscall3 16 …)` → 16, `(syscall3 4
…)` → 4 — so probe the device only through the AOT helpers (`%nsm-ioctl`,
`%nsm-open-at`), which are faithful (-9 for a bad fd, -14 for a null pointer,
-90 for an oversize request).  And a spot instance was reclaimed mid-install
(`Server.SpotInstanceTermination`); `ON_DEMAND=1 test/nitro/aws-launch.sh` is
the ~$0.20/hour alternative.  `test/nitro/aws-launch.sh` builds the VPC when
the account has no default one and sweeps every AZ and nine Nitro-capable
types for capacity.
