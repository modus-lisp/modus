#!/usr/bin/env python3
"""kds-fetch.py REPORT.bin OUTDIR [--product Milan|Genoa|Turin] [--aux AUXBLOB]
Fetch the VCEK that signed REPORT from AMD's Key Distribution Service, plus the
ASK/ARK chain, and write OUTDIR/vcek.pem, OUTDIR/chain.pem (ASK then ARK).
The VCEK is keyed by the report's chip_id (0x1A0, 64 bytes) and its REPORTED TCB
(0x180: bootloader SPL byte 0, TEE byte 1, SNP byte 6, microcode byte 7).
The product is read from the report's family/model when present (report version 3+),
else --product (default Milan).

VLEK-SIGNED REPORTS (cloud providers -- measured on EC2 c6a, 2026-10-08): the
report's key-info word (0x48, bits 4:2) says which key signed it, 0 = VCEK,
1 = VLEK.  A VLEK is AMD's key for the PROVIDER, not the chip; chip_id is then
zeros and the VCEK URL above is a 404.  The VLEK certificate travels WITH the
report, in the guest's auxiliary blob (configfs-tsm `auxblob`, or the
SNP_GET_EXT_REPORT certificate table): pass it as --aux and this writes
OUTDIR/vcek.pem from it (the verifier treats it as "the signing key") and the
VLEK chain (ASVK then ARK) as OUTDIR/chain.pem, from KDS /vlek/v1/{product}."""
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
signing_key = (struct.unpack_from("<I", rep, 0x48)[0] >> 2) & 7
if signing_key == 1:
    # The certificate table: 24-byte entries {GUID (big-endian bytes), offset u32, length u32},
    # ended by an all-zero GUID; offsets are from the start of the table.
    VLEK_GUID = bytes.fromhex("a8074bc2a25a483eaae639c045a0b8a1")
    aux = open(opts["--aux"], "rb").read() if "--aux" in opts else b""
    vlek = None; o = 0
    while o + 24 <= len(aux) and aux[o:o+16] != bytes(16):
        g = aux[o:o+16]; off, ln = struct.unpack_from("<II", aux, o + 16); o += 24
        if g == VLEK_GUID: vlek = aux[off:off+ln]
    if vlek is None:
        sys.exit("report is VLEK-signed: pass the guest's auxiliary blob with --aux (it carries the VLEK certificate)")
    pem = subprocess.run(["openssl", "x509", "-inform", "DER", "-outform", "PEM"], input=vlek, capture_output=True, check=True).stdout
    open(f"{out}/vcek.pem", "wb").write(pem)
    chain = urllib.request.urlopen(f"https://kdsintf.amd.com/vlek/v1/{product}/cert_chain", timeout=60).read()
    open(f"{out}/chain.pem", "wb").write(chain)
    print("VLEK-signed report; wrote", f"{out}/vcek.pem (the VLEK, from --aux)", f"{out}/chain.pem (ASVK, ARK)")
    sys.exit(0)
base = f"https://kdsintf.amd.com/vcek/v1/{product}"
url = f"{base}/{chip}?blSPL={bl}&teeSPL={tee}&snpSPL={snp}&ucodeSPL={ucode}"
print("product", product, "chip_id", chip[:16] + "..", "tcb", dict(bl=bl, tee=tee, snp=snp, ucode=ucode))
der = urllib.request.urlopen(url, timeout=60).read()
pem = subprocess.run(["openssl", "x509", "-inform", "DER", "-outform", "PEM"], input=der, capture_output=True, check=True).stdout
open(f"{out}/vcek.pem", "wb").write(pem)
chain = urllib.request.urlopen(f"{base}/cert_chain", timeout=60).read()
open(f"{out}/chain.pem", "wb").write(chain)
print("wrote", f"{out}/vcek.pem", f"{out}/chain.pem")
