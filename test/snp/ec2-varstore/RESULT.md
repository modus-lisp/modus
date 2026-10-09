# Result, 2026-10-08: EC2 does NOT measure the UEFI variable store into the SEV-SNP launch digest

| guest | AMI | variable store | measurement |
|---|---|---|---|
| source | AL2023 (AWS) | none | `7a89ccea…dc766` |
| control-1 | ami-0603176f2aa072fec | none | `7a89ccea…dc766` |
| control-2 | ami-0603176f2aa072fec | none | `7a89ccea…dc766` |
| vars | ami-070d8478c3943f5ee | our KEK + db | `7a89ccea…dc766` |

All c6a.large, us-east-2, from one snapshot; the two AMIs differ only in
`--uefi-data`.  The vars guest really booted with our keys: its `KEK` and `db`
EFI variables contain `modus KEK (development)` / `modus db (development)`, and
the control's have none.  HOST_DATA and the ID-key digest are zero on every
guest.  Reports in `records/`.

**Consequence.** On EC2 the SNP measurement is AWS's firmware and nothing a
customer can choose, so Secure Boot keys -- and therefore "only images signed by
modus" -- cannot be attested through it.  EC2 SEV-SNP cannot vouch for anything
past AWS's firmware.  The AWS route that attests the image is Nitro Enclaves
(PCR0-2 over the EIF, PCR8 over who signed it); SNP attestation of the DDC'd
image needs a host where we supply the firmware.
