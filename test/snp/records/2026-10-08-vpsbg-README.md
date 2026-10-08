# VPSBG cloud VPS: SEV-SNP report, 2026-10-08

- Host: VPSBG cloud VPS, AMD EPYC 7713P (Milan, family 19h), 1 vCPU, 885 MB RAM, kernel 7.0.0-30-generic.
- Guest: SEV-SNP at VMPL0, `/dev/sev-guest` present, dmesg "Detected confidential virtualization sev-snp".
- Report: `2026-10-08-vpsbg-epyc7713p-report.bin`, taken through configfs-tsm with a random REPORT_DATA.
- Signed by a **VCEK** (chip-specific): the chain verifies to ARK-Milan, the signature is valid. VERDICT PASS.
- Measurement: `78dc3011…01c5208b…`. Not compared to a known value: we have not computed the expected measurement from VPSBG's published firmware and the kernel the guest booted (that needs their firmware hash, kernel and command line).
- Not a measured-boot launch as far as we know: the report alone does not say which firmware ran.
