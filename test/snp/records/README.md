# SEV-SNP reports from real hardware

| file | what |
|---|---|
| `2026-10-08-ec2-c6a-report.bin` | ATTESTATION_REPORT from an EC2 `c6a.large` SEV-SNP guest (AMD EPYC 7R13, us-east-2, Amazon Linux 2023, kernel 6.18, VMPL0), taken through configfs-tsm with REPORT_DATA = SHA-512 of the guest's Ed25519 SSH host key |
| `2026-10-08-ec2-c6a-aux.bin` | the auxiliary blob returned with it: the certificate table holding the VLEK |
| `2026-10-08-ec2-c6a-hostkey.txt` | the host key as OpenSSH recorded it on connecting (not as the guest printed it) |

Re-verify, no AWS account needed (fetches the VLEK chain from AMD's KDS):

    python3 test/snp/kds-fetch.py test/snp/records/2026-10-08-ec2-c6a-report.bin /some/dir --aux test/snp/records/2026-10-08-ec2-c6a-aux.bin
    python3 test/snp/verify-report.py test/snp/records/2026-10-08-ec2-c6a-report.bin \
        --hostkey-b64 "$(cat test/snp/records/2026-10-08-ec2-c6a-hostkey.txt)" --vcek /some/dir/vcek.pem --chain /some/dir/chain.pem

Result 2026-10-08: signed_by VLEK, VLEK <- ASVK (SEV-VLEK-Milan) <- ARK-Milan
(the same ARK as `test/snp/amd-milan-chain.pem`), signature valid, report_data
== SHA-512(host key): **PASS**.  Controls: one bit flipped in the measurement
-> signature INVALID, FAIL; a different host key -> report_data False, FAIL.

The measurement (`7a89ccea…`) is of **AWS's UEFI firmware**, not of a modus
image: EC2 boots its own firmware, so this run proves the verifier against the
real AMD signing chain, not the DDC'd image.  That still needs a host where we
supply the OVMF (docs/snp-guest.md, the rental kit).
