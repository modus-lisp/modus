#!/usr/bin/env python3
"""Verify an SEV-SNP ATTESTATION_REPORT for attested SSH.

  verify-report.py <report.bin> [--hostkey FILE | --report-data HEX] [--measurement HEX] [--vcek CERT.pem]

Checks, each printed and any failure fatal:
  * report_data == SHA-512 of --hostkey's bytes (or == --report-data)
  * measurement == --measurement (the DDC'd image's launch digest)
  * ECDSA P-384 signature over bytes 0..0x2A0 with the VCEK (if given);
    r and s are 72-byte little-endian fields at 0x2A0 / 0x2E8.
Prints chip_id, TCBs and policy so a KDS VCEK fetch can be scripted from them.
"""
import sys, struct, hashlib
def u32(b,o): return struct.unpack_from('<I',b,o)[0]
def u64(b,o): return struct.unpack_from('<Q',b,o)[0]
def main():
    a=sys.argv[1:]; rep=open(a[0],'rb').read(); opts={}
    i=1
    while i<len(a): opts[a[i]]=a[i+1]; i+=2
    assert len(rep)>=1184, f"report is {len(rep)} bytes, expected 1184"
    print(f"version {u32(rep,0)} guest_svn {u32(rep,4)} policy {u64(rep,8):#x} vmpl {u32(rep,0x30)} sig_algo {u32(rep,0x34)}")
    print(f"current_tcb {u64(rep,0x38):#018x} reported_tcb {u64(rep,0x180):#018x} committed_tcb {u64(rep,0x1E0):#018x} launch_tcb {u64(rep,0x1F0):#018x}")
    print("chip_id", rep[0x1A0:0x1E0].hex())
    rd=rep[0x50:0x90]; meas=rep[0x90:0xC0]
    print("report_data", rd.hex()); print("measurement", meas.hex())
    ok=True
    if '--hostkey' in opts:
        want=hashlib.sha512(open(opts['--hostkey'],'rb').read()).digest()
        print("report_data == SHA-512(hostkey):", rd==want); ok&=rd==want
    if '--report-data' in opts:
        want=bytes.fromhex(opts['--report-data']); print("report_data matches:", rd==want); ok&=rd==want
    if '--measurement' in opts:
        want=bytes.fromhex(opts['--measurement']); print("measurement matches:", meas==want); ok&=meas==want
    if '--vcek' in opts:
        from cryptography import x509
        from cryptography.hazmat.primitives.asymmetric import ec
        from cryptography.hazmat.primitives.asymmetric.utils import encode_dss_signature
        from cryptography.hazmat.primitives import hashes
        cert=x509.load_pem_x509_certificate(open(opts['--vcek'],'rb').read())
        r=int.from_bytes(rep[0x2A0:0x2A0+72],'little'); s=int.from_bytes(rep[0x2E8:0x2E8+72],'little')
        try:
            cert.public_key().verify(encode_dss_signature(r,s), rep[:0x2A0], ec.ECDSA(hashes.SHA384())); print("VCEK signature: valid")
        except Exception as e: print("VCEK signature: INVALID", e); ok=False
    else: print("VCEK signature: not checked (no --vcek)")
    print("VERDICT", "PASS" if ok else "FAIL"); sys.exit(0 if ok else 1)
if __name__=='__main__': main()
