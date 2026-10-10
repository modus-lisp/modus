# Reproducing the VPSBG VPS launch measurement: not reproduced (2026-10-08)

Target: 78dc301159e2699f825745b919e15a642b27caccf21571863753e4d784d01c5208b6057b31eaaa97268a234a91787f2c
(SNP report from the VPS, 1 vCPU, EPYC 7713P, family 25 model 1 stepping 1, debug off, VMPL0).

Established:
- VPSBG's published measured-boot OVMF (OVMF_SEV_MEASUREDBOOT_4M.fd) matches its published
  SHA-256 e4ac90be71f3b455922ebc7106c5630536bf67027de585e34319b0a42fcd716e.
- The guest's EFI firmware self-identifies as "Proxmox distribution of EDK II", and it boots
  from disk through GRUB (BOOT_IMAGE= on the kernel command line): no direct kernel boot.

Tried (sev-snp-measure, firmware only, no kernel/initrd, 1 vCPU), none matching:
- OVMF_SEV_MEASUREDBOOT_4M.fd and Proxmox OVMF_SEV_4M.fd (pve-edk2-firmware-ovmf 4.2026.08-1)
- vCPU types EPYC, EPYC-v1..v4, EPYC-Milan, -v1, -v2, EPYC-Genoa
- vmm-type QEMU and ec2
- explicit CPU signature family 25 / model 1 / stepping 1, guest features 0x0 and 0x1

Not tried / unknown: the Proxmox firmware build the VPS host actually runs (the guest does
not report its version); any host-side launch options; whether the host hashes a kernel.
The question for VPSBG: which firmware build and launch parameters this VM was started with.
