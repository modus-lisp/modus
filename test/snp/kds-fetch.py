#!/usr/bin/env python3
"""kds-fetch.py REPORT.bin OUTDIR [--product Milan|Genoa|Turin]
Fetch the VCEK that signed REPORT from AMD's Key Distribution Service, plus the
ASK/ARK chain, and write OUTDIR/vcek.pem, OUTDIR/chain.pem (ASK then ARK).
The VCEK is keyed by the report's chip_id (0x1A0, 64 bytes) and its REPORTED TCB
(0x180: bootloader SPL byte 0, TEE byte 1, SNP byte 6, microcode byte 7).
The product is read from the report's family/model when present (report version 3+),
else --product (default Milan)."""
import sys, struct, urllib.request, subprocess
rep = open(sys.argv[1], "rb").read(); out = sys.argv[2]
opts = {sys.argv[i]: sys.argv[i+1] for i in range(3, len(sys.argv) - 1, 2)}
chip = rep[0x1A0:0x1E0].hex(); tcb = rep[0x180:0x188]
bl, tee, snp, ucode = tcb[0], tcb[1], tcb[6], tcb[7]
version = struct.unpack_from("<I", rep, 0)[0]
product = opts.get("--product")
if product is None and version >= 3:
    family, model = rep[0x188], rep[0x189]           # cpuid_fam_id / cpuid_mod_id (v3)
    product = {(0x19, 0x01): "Milan", (0x19, 0x11): "Genoa", (0x1A, 0x02): "Turin"}.get((family, model), "Milan")
product = product or "Milan"
base = f"https://kdsintf.amd.com/vcek/v1/{product}"
url = f"{base}/{chip}?blSPL={bl}&teeSPL={tee}&snpSPL={snp}&ucodeSPL={ucode}"
print("product", product, "chip_id", chip[:16] + "..", "tcb", dict(bl=bl, tee=tee, snp=snp, ucode=ucode))
der = urllib.request.urlopen(url, timeout=60).read()
pem = subprocess.run(["openssl", "x509", "-inform", "DER", "-outform", "PEM"], input=der, capture_output=True, check=True).stdout
open(f"{out}/vcek.pem", "wb").write(pem)
chain = urllib.request.urlopen(f"{base}/cert_chain", timeout=60).read()
open(f"{out}/chain.pem", "wb").write(chain)
print("wrote", f"{out}/vcek.pem", f"{out}/chain.pem")
