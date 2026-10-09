# VPSBG attestation, following VPSBG's own guide (snpguest), 2026-10-08

Steps 3-7 of docs.vpsbg.eu "How to perform AMD SEV-SNP attestation inside a guest",
run on the VPS (Ubuntu 26.04, EPYC 7713P), with snpguest built from virtee/snpguest HEAD:

- `snpguest verify certs`: ARK self-signed, ASK signed by ARK, VCEK signed by ASK. PASS.
- `snpguest verify attestation`: TCB boot loader, TEE, SNP and microcode all match the
  certificate; VCEK signed the report. PASS.

Files: report.bin, vcek.pem, ask.pem, ark.pem (the chain snpguest fetched from AMD KDS).

Not done by the guide: a measured-boot expected value. The guide links a separate page
about rebuilding VPSBG's OVMF for measured boot; that is the next step if we want the
launch measurement checked against the firmware and kernel.
