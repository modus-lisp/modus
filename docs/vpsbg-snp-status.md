# Modus on a VPSBG SEV-SNP VPS: status, 2026-10-10

The goal: boot Modus on VPSBG's SEV-SNP VPS, reach it over key-only SSH, and
fetch an attestation report whose measurement covers our image.

**Where it stands: attested SSH works end to end (2026-10-10 03:33 UTC); see the next section.  First report: 02:20 UTC.** Booted
through VPSBG measured boot (UKI image 369, `-kernel`, kernel hashes), Modus
answered key-only SSH 34 s after the restart, and over that SSH asked the PSP
for a report with `report_data` = 64 x `09`.  `test/snp/verify-report.py`:
signature valid, VCEK -> ASK -> ARK-Milan (from AMD KDS), VMPL 0, and the
launch measurement

    a984242e6fc129d0b9a285a649e8965a72d193ea5031b2c50462341c656a7b386e49a5fbf64a9f123ef8c2ed204ccba9

equals what `sev-snp-measure` computes OFFLINE from VPSBG's published
`OVMF_SEV_MEASUREDBOOT_4M.fd` + our UKI (1 vCPU, family 25 model 1 stepping 1,
guest features 0x1, `--append ''`).  The current tree rebuilds that image
byte-for-byte (`cmp`), given the same `MODUS_SSH_*_KEY_HEX`.  What changed to get
here is in "Session 2026-10-10" below.  All work is **uncommitted** in
`~/modus-wt`, branch `virtio-net` (HEAD `30b9327`).

## Attested SSH, end to end (2026-10-10 03:33 UTC)

`test/snp/attested-ssh-client.sh 87.120.37.135 22 --measurement <expected>
--vcek vcek.pem --chain chain.pem` against UKI image 373, exit 0: the host key
OpenSSH recorded in the handshake == the key the server printed;
`report_data` == SHA-512 of it; measurement
`f29c4eb4...c13205` == `sev-snp-measure` offline; VCEK signature valid and
chained to ARK-Milan.  The whole report comes back in ONE exec reply.  On that
boot `(snp-active-p)` = T and `(%hw-random32)` returns RDRAND values.  Evidence
in `tmp/vpsbg/2026-10-10/attested-ssh-*`.  What it took, after the first report:

8. **TCP segmentation** (`net/ip.lisp`, `tcp-send-conn`): payloads are split
   at 1360 bytes.  A segment was built in a fixed 1480-byte array and sent as
   one frame, so any SSH reply over ~1400 bytes overran both and never arrived.
9. **The server's ephemeral key was a constant.**  `ssh-random` (a 32-bit
   xorshift) keeps its state at `ssh+0x2C`; `ssh-seed-random` wrote its seed to
   `e1000-state+0x62C`, which nothing reads, and `ssh-boot` zeroes the state, so
   the generator began at 12345 on every boot -- and `pre-compute-server-eph`
   made the X25519 "ephemeral" key from it ONCE and reused it for every
   connection.  Anyone with the source could compute it and decrypt recorded
   sessions.  Now: the boot installs an RDRAND routine at `0x1D000` (CPUID
   checked; a tagged pointer at `0x1D100`, like the SNP VMGEXIT routine), and
   the x64 CL image's `ssh-random` (`net/ssh-x64-cl.lisp`) is SHA-512 in counter
   mode over a 512-bit RDRAND key.  Under SNP there is NO fallback (the host
   drives the PIT and TSC): no RDRAND, no keys.  Off SNP it falls back to
   PIT/TSC and says so on serial.  And `ssh-refresh-ephemeral` replaces the key
   after every connection whose key exchange used it (shared code).
10. **Dead clients.**  The single-threaded server drops SYNs while it serves a
   connection, so a client that is gone holds everyone else off until a wait
   expires -- and fix 6 had made those waits ~17 s.  Now an RST or FIN from the
   client being served ends its waits at once (`ssh-peer-closed-p`, flag at
   `ssh-ipc+0x60458`; `net-wait-ack` sees the RST too, and accepts only ITS
   client's ACK), the version wait on x64 is ~2 s (2^32 cycles: a NAT that
   forgot the mapping eats our SYN-ACK, so no RST ever comes), and frames that
   queued in the RX ring before the server listened are dropped.  Measured with
   a fake gateway (stale SYNs during boot, three clients that answer the SYN-ACK
   with RST, then a real one, all retransmitting SYNs once a second): the real
   client's banner after **18.1 s** before, **1.0 s** after.

**Still open, security:** the Pi / aarch64 SSH images still use the generic
`ssh-random` -- the boot-time ephemeral key from the constant state, later ones
from a state seeded with the client's address and port.  They need the same
treatment with their own hardware RNG (BCM2835 RNG).  TCP's initial sequence
number is the constant 1000 on every connection.

## Session 2026-10-10: from "idle in kernel-main" to a verified report

Every fix below was found by reproducing locally first, which turned
five-minute VPS rounds into one-minute local ones.  Two local instruments:

* **Poisoned RAM.** `-object memory-backend-file,mem-path=<1 GB of
  /dev/urandom>,share=off -machine q35,memory-backend=pmem`.  Under SNP, memory
  the guest never wrote through its encrypted mapping decrypts to noise; QEMU
  hands out zeros.  The VPS image, unfixed, stopped right after `MODUS-CL` with
  random RAM and reached `NETUP` with zero RAM.  Use several seeds: seed 2 found
  a second bug seed 1 did not.
* **A fake gateway.** `-netdev dgram` + a Python script that plays 172.16.0.1
  with raw Ethernet frames (ARP, a TCP SYN to 22, background junk).  Runs the
  exact VPS image with its static address and shows its serial console.

The fixes, each verified locally (twin over SSH, clean RAM + 3 to 8 poison
seeds) and then on the VPS:

1. **Zero the whole runtime-metadata page** `0x10000000..0x10001000` first
   thing in `emit-x64-kernel64-entry` (`boot/boot-x64.lisp`).  The boot zeroed
   an enumerated list of its words; the GC-region word, threads gate, per-CPU
   mode word, atomics lock etc. came up as noise.  The RPi image already
   bulk-zeroes the same page.
2. **`%core-requested-p` used `=` on a raw `:u64` read** of the core address
   (`0x20000000`).  Noise with a pointer tag went to `NUMERIC-EQUAL-P` ->
   `%COMPLEX-P`, which dereferenced it: #GP, halt (found by a QEMU-monitor
   backtrace on poison seed 2).  Now `EQ` (the magic is a fixnum).  The Pi
   shares this source and had the same latent bug on real DRAM.
3. **Zero the 8 KB `e1000-state` block** at the start of `run-net-pipeline`.
   `ssh-boot` re-zeroes only `+0x600..+0x900`; the Ed25519 init flag at
   `+0x5D0` came up non-zero, so `ed25519-init` was skipped (`EI` missing).
4. **The secrets page is copied out of the way** before the 47 MB image is
   copied to `0x100000` (`emit-snp-save-secrets`): OVMF keeps it at ~0x80D000
   (the VPS e820 shows `0x80D000..0x80FFFF` reserved), so the copy destroyed
   VMPCK0.  The copy lives at `0x1C000`; `(snp-secrets-gpa)` reads 114688.
5. **SNP builds mask interrupts** (`MODUS_X64_NO_STI` now defaults on when
   `*x64-snp-mode*`).  Same image otherwise, on the VPS: with STI the SSH
   server loop went idle within ~20 s and nothing answered; without, TCP and
   the banner came up.  The fitting hazard (not proven to be the one): the PIT
   ticks at 1000 Hz, each EOI is an OUT = a #VC through the single GHCB, and a
   tick inside a Lisp GHCB request (`net/snp-ghcb.lisp`, fill ... VMGEXIT ...
   read back) has the handler rewrite the request under it.  That Lisp path is
   not reentrant; if interrupts are ever wanted under SNP, do what Linux does
   (a backup GHCB per CPU).  Ruled out on the way: the #VC fatal path (a
   build whose fatal path spins still went idle), unaccepted memory (none).
6. **SSH waits are timed, not counted.** Every wait for the peer was a count
   of polls; one 50000-try poll is ~15 ms on the native EPYC, so the server
   gave a client ~15 ms to answer and closed every remote connection right
   after its `KEXINIT` (under TCG a poll is slow enough that it never showed).
   `net/ssh.lisp` now has one policy, `ssh-wait-expired-p` (count, limit,
   since), used by the version wait, the three handshake waits
   (`ssh-await-packet`) and the pre-auth idle drop.  The default is the count
   alone, unchanged; the x64 CL image overrides it to also require 2^35 TSC
   cycles (10-17 s).
7. **Bare-metal `(rdtsc)` is `RDTSC`, not `RDTSCP`** (`translate-x64.lisp`,
   hosted unchanged): QEMU's default `qemu64` CPU has no RDTSCP, and fix 6 was
   the first bare-metal code ever to execute it (#UD, `ssh-boot` returned).

**Knobs added for the bisect** (unset = image unchanged):
`MODUS_SNP_VC_FATAL=spin` (the #VC handler's fatal path spins instead of
terminate+`hlt`), and `MODUS_LISP_SPIN_AT=ssh-zeroed|ssh-seeded|ssh-keyed|
ssh-signed|ssh-netup` (between the steps of the x64 `ssh-boot`).
`scripts/vpsbg-ssh-round.sh IMAGE [FORM...]` boots an image through the GRUB
one-shot, evaluates FORMs over SSH, and returns to Ubuntu (`EVAL_TIMEOUT`,
`SAVE`, `VERBOSE`).

**Measured on the VPS, in order:** zero-page + core-p + net-state fixes:
`pre-ssh` and `ssh-netup` REACHED, but idle afterwards, no TCP -> + no STI:
`tcp/22 connects`, banner `SSH-2.0-Modus_1.0`, connection closed after
`KEXINIT` -> + timed waits + RDTSC: `(+ 1 2) = 3`, `(snp-active-p) = T`,
`(snp-secrets-gpa) = 114688`, report 1184 bytes, verified (disk boot, firmware
digest `78dc3011...`, not reproducible: unknown Proxmox build) -> measured boot:
verified with the matching digest above.

**Open, found on the way:**

* ~~An SSH reply longer than ~1400 characters never arrives~~ -- fixed (8).
* **A shell session holds the server forever**: once authenticated it never
  times out and the TCP layer ignores FIN.  Use `ssh host "(form)"` (an exec
  request: the server sends exit-status/EOF/CLOSE and frees itself).
* ~~The SSH server's entropy is the PIT counter~~ -- RDRAND on x64 (9).
* ~~`report_data` is not yet bound to the host key~~ -- `snp-attest-ssh` binds
  it, and the client checks it (section above).
* The first `snp-attestation-report` on one boot took more than 60 s, on
  another 2 s.  Not investigated.
* `(%snp-build-mode)` gives no reply when evaluated over SSH.

## The box

| | |
|---|---|
| Server | 27029 `server-1`, Cloud VPS 1 GB, **1 vCPU**, KVM, UEFI, `amd_sev_level` 3 (SNP), VMPL0 |
| Address | `87.120.37.135/32`, default route via `172.16.0.1` on-link (static; VPSBG runs **no DHCP**) |
| NIC | virtio-net `1af4:1041` (modern only) at `06:12.0`, behind bridges `00:1e.0` → `05:01.0`; BAR4 `0x3838_0000_0000`; offers MAC, VERSION_1, ACCESS_PLATFORM |
| Disk | `sda` on virtio-scsi; ESP `sda15` (vfat, 106 MB), `/boot` `sda13`, `/` `sda1` |
| CPU | EPYC 7713P (Milan), family 25 model 1 stepping 1 |
| Firmware | disk boot: Proxmox `OVMF_SEV_4M.fd`; measured boot: VPSBG's `OVMF_SEV_MEASUREDBOOT_4M.fd` (UKI via `-kernel`, `kernel-hashes=on`) |
| Visibility | no console route in the API. The only signals are `GET /servers/27029` (running, CPU) and `/metrics` (5-minute CPU / network buckets) |
| Access | Ubuntu 26.04 on the disk, root SSH with `~/.ssh/id_ed25519`; API tokens in `~/.config/vpsbg/` |

**An SNP guest cannot reset itself here.** `systemctl reboot` inside Ubuntu
leaves the VM *stopped*; use the API's `restart` (an external stop + start), or
`start` after a stop.

## How Modus is booted on it now: GRUB one-shot from the disk (not measured)

The disk boot uses the same firmware Linux does, which removes VPSBG's `-kernel`
path from the picture, and GRUB's one-shot gives a guaranteed way back:

* `/boot/efi/EFI/modus/modus.efi` is the image; `/etc/grub.d/40_custom` has a
  `menuentry "Modus"` that chainloads it; `GRUB_DEFAULT=saved`, `saved_entry=0`.
* `grub-reboot Modus` then an API restart boots Modus **once**; the next start
  or restart boots Ubuntu again.

`scripts/vpsbg-spin-round.sh POINT` does one whole round: stage
`$SPIN_DIR/POINT.efi`, `grub-reboot Modus`, API restart, sample the server's
running state and CPU for ~2½ minutes, classify, return to Ubuntu.

## The bisect: where the image stops, measured on the VPS

Two build knobs emit an infinite loop at one named point and nowhere else (unset,
the image is byte-identical):

* `MODUS_SNP_SPIN_AT=<name>` in the UEFI/SNP boot stub (`jmp $`):
  `entry pre-ebs post-ebs pre-detect post-detect pre-cr3 post-cr3 pre-snp-setup`
  `post-pvalidate1 post-psc1 post-psc post-reg post-ghcb-zero post-lidt`
  `post-snp-setup post-selftest pre-kernel`.
* `MODUS_LISP_SPIN_AT=<name>` in Lisp boot (`(loop)`): `km-entry`
  `km-after-prologue km-after-tables km-after-globals km-pre-epilogue`
  `pre-pipeline post-probe pre-ssh post-ssh-return`; and
  `MODUS_LISP_SPIN_AFTER="(form)"` spins right after that exact line of
  `kernel-main`.

A variant still running at ~100% CPU **reached** its point; a stopped VM
**died** before it; running at ~2.7% CPU is **idle** (halted) before it.

| point | result | meaning |
|---|---|---|
| `post-detect`, `post-cr3` | reached | SNP detection, C-bit page tables, CR3: fine |
| `pre-snp-setup` | reached | |
| `post-pvalidate1` (old stub) | **died** | the first PVALIDATE killed the VM — fixed, see below |
| `post-snp-setup`, `pre-kernel` (fixed stub) | reached | the whole stub completes and jumps into Lisp |
| `km-after-tables` | reached | symbol / keyword / package / stream tables |
| after `(%init-signal-symbols)` | reached | reader, conditions, method combinations, function tables, macros |
| after `(%init-make-load-form)` | **idle** | **the call that goes idle under SNP** |
| `km-after-globals`, `pre-pipeline` | idle | consistent with the above |

The full image (no spin) runs idle at ~2.7% CPU and never answers on the network.

`%init-make-load-form` (`mvm/ansi-bridge.lisp`) is five calls: `%defgeneric
'make-load-form`, three `%defmethod`s (STANDARD-OBJECT, STRUCTURE-OBJECT,
CONDITION) and `%register-gf-fn`. It works on every non-SNP target. Which of the
five goes idle, and why only under SNP, is the open question; the same
`MODUS_LISP_SPIN_AFTER` approach splits it once each call is its own line.

## What changed on 2026-10-09, and how each part was verified

1. **SNP boot stub: PVALIDATE order** (`boot/boot-uefi-snp.lisp`). The shared
   2 MB region was mapped C=0 in the initial page tables and then PVALIDATE-
   rescinded through that mapping; the first PVALIDATE took the VM down. Now
   every PD entry starts encrypted; the region is rescinded with one 2 MB
   PVALIDATE (4 KB fallback on FAIL_SIZEMISMATCH; result checked), then
   Page-State-Changed page by page, and only then is its PD entry rewritten
   without the C-bit and CR3 reloaded (Linux's order). Verified on the VPS:
   `post-snp-setup` and `pre-kernel` reached; locally the `test`-mode twin is
   unchanged (#VC self-test, GHCB NIC path, SSH login).
2. **Hosted x64 on a 1 GB machine** (`boot/boot-linux-x64.lisp`). The ELF's
   zero-filled BSS was sized by the heap (`p_memsz` = image + 1.9 GB, a leftover
   from when the heap lived in the BSS), which the kernel refuses on a small VM
   and answers with SIGSEGV before our first instruction. The BSS now ends at
   `0x20000000`. The heap `mmap` is `MAP_NORESERVE` and its result is checked
   (`modus: cannot map the heap (mmap failed)`, exit 1). Verified: hosted Modus
   runs on the VPS with default overcommit; locally GC stress, region GC,
   threads, MV/handlers, sb-thread, sockets, dynamic bindings, hosted SSH pass.
3. **Ed25519 verification** (`net/crypto.lisp`, `ed-recover-x`): `(if sign …)`
   on a number decoded every even-x point negated wherever NIL is not 0, so
   every signature check failed. Verified against a real `ssh-keygen` signature.
4. **Key-only SSH image that starts itself**: `MODUS_SSH_AUTOSTART`,
   `MODUS_SSH_AUTH_KEY_HEX`, `MODUS_SSH_HOST_KEY_HEX`; unauthenticated
   connections dropped after `ssh-preauth-idle-limit` (20) empty polls.
5. **`net/aarch64-overrides.lisp` merged** into `ssh.lisp` / `ip.lisp`; deleted
   `x86-ssh-overrides.lisp`, `build-arm32-ssh`, `build-i386-diag-ssh` and their
   wrappers.
6. **virtio MMIO through the GHCB under SNP** (`net/snp-ghcb.lisp`,
   `net/virtio-net.lisp`); verified in `test` mode.
7. **Static IPv4** (`MODUS_NET_IP`, `MODUS_NET_GW`; the gateway word is stored as
   DHCP stores it, the address as wire-order bytes).
8. **UKI tooling**: `scripts/make-uki.py`, `scripts/make-raw-uefi-disk.sh`.
9. **Spin-point knobs and `scripts/vpsbg-spin-round.sh`** (above).

## Measured boot

Earlier images 361/363/365/367 were tried through VPSBG measured boot; none came
up. With the PVALIDATE fix those attempts are worth repeating once the Lisp boot
gets past `%init-make-load-form`. Expected-measurement tooling:
`sev-snp-measure` with VPSBG's firmware, 1 vCPU, Milan signature (results for
image 367 in `tmp/uki/modus-vps.expected`).

**2026-10-10: it works.** Image 369 (this tree's VPS image as a UKI) booted,
served SSH and produced a report whose measurement matches the offline
computation exactly (top of this file).

## Next steps

1. Give the Pi / aarch64 SSH images a real RNG and the per-connection key
   (see "Still open, security").  Random ISNs.
2. Script the measured-boot round (UKI, expected digest, upload, attach, wait
   for the Modus banner, attested client, detach).  Today it is the commands in
   this file's history.  Image 373 is uploaded (auto-removed after two days
   unattached); the server is back on disk boot / Ubuntu.
3. The SSH server serves one connection at a time and drops SYNs meanwhile;
   clients get in by retransmitting.  Fine for one operator, not for a service.

## Traps found on the way

- `pgrep -f` in a wait loop matched the loop's own command line (it contained the
  build command), so "wait for the build" waited forever.  Bracket the pattern AND
  keep the command out of the same shell line, or wait on a file.
- Modus answers the round script's "is Ubuntu back?" `ssh root@IP ...` too: it
  evaluates the command as Lisp (`= (ERROR #<UNBOUND-VARIABLE>)`).  The check now
  requires `uname -s` = Linux.
- `/home` on the dev box fills up under other sessions' ANSI shards; QEMU disk
  images for these tests go on `/dev/shm`.

- An SNP guest's own reboot stops the VM; a harness that reboots from inside and
  then samples sees "stopped" and blames the image.
- `pkill -f` / `pgrep -f` with a pattern in the shell's own command line kills
  the shell (exit 144).
- `ulimit -v` cannot test the heap-failure path: the ELF's own BSS counts too.
- Several legacy builds assemble source with one `~A` per file in a FORMAT
  string; removing a file means removing its `~A`.
