#!/usr/bin/env python3
"""Verify an SEV-SNP ATTESTATION_REPORT for attested SSH.

  verify-report.py <report.bin> [--hostkey FILE | --hostkey-hex HEX | --hostkey-b64 BLOB | --report-data HEX]
                   [--measurement HEX] [--vcek CERT.pem|DER] [--chain CHAIN.pem]

Checks, each printed and any failure fatal:
  * report_data == SHA-512 of the 32 raw Ed25519 host public key bytes, given as
    a file of raw bytes, hex (what the server prints as SNP-HOSTKEY), or the
    base64 blob from `ssh-keyscan` / known_hosts / ssh -v ("ssh-ed25519 AAAA...")
    (or == --report-data)
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
    hk=None
    if '--hostkey' in opts: hk=open(opts['--hostkey'],'rb').read()
    if '--hostkey-hex' in opts: hk=bytes.fromhex(opts['--hostkey-hex'])
    if '--hostkey-b64' in opts:
        import base64
        blob=base64.b64decode(opts['--hostkey-b64'].split()[-1] if ' ' in opts['--hostkey-b64'] else opts['--hostkey-b64'])
        n=struct.unpack_from('>I',blob,0)[0]; assert blob[4:4+n]==b'ssh-ed25519', blob[4:4+n]
        m=struct.unpack_from('>I',blob,4+n)[0]; hk=blob[8+n:8+n+m]
    if hk is not None:
        assert len(hk)==32, f"host key is {len(hk)} bytes, expected 32 raw Ed25519 bytes"
        print("hostkey", hk.hex())
        want=hashlib.sha512(hk).digest()
        print("report_data == SHA-512(hostkey):", rd==want); ok&=rd==want
    if '--report-data' in opts:
        want=bytes.fromhex(opts['--report-data']); print("report_data matches:", rd==want); ok&=rd==want
    if '--measurement' in opts:
        want=bytes.fromhex(opts['--measurement']); print("measurement matches:", meas==want); ok&=meas==want
    if '--vcek' in opts:
        from cryptography import x509
        from cryptography.hazmat.primitives.asymmetric import ec, rsa, padding
        from cryptography.hazmat.primitives.asymmetric.utils import encode_dss_signature
        from cryptography.hazmat.primitives import hashes
        raw=open(opts['--vcek'],'rb').read()
        cert=x509.load_pem_x509_certificate(raw) if raw.startswith(b'-----') else x509.load_der_x509_certificate(raw)
        if '--chain' in opts:
            # ASK then ARK, as AMD KDS serves cert_chain; the ARK is self-signed RSA-PSS 4096.
            pems=[b'-----BEGIN'+c for c in open(opts['--chain'],'rb').read().split(b'-----BEGIN')[1:]]
            ask,ark=[x509.load_pem_x509_certificate(c) for c in pems[:2]]
            def rsa_pss_ok(issuer, c):
                try:
                    issuer.public_key().verify(c.signature, c.tbs_certificate_bytes,
                        padding.PSS(mgf=padding.MGF1(c.signature_hash_algorithm), salt_length=c.signature_hash_algorithm.digest_size), c.signature_hash_algorithm)
                    return True
                except Exception as e: return False
            cok = rsa_pss_ok(ark, ark) and rsa_pss_ok(ark, ask) and rsa_pss_ok(ask, cert) and cert.issuer==ask.subject and ask.issuer==ark.subject
            print("VCEK <- ASK <- ARK chain:", cok, "| ARK", ark.subject.rfc4514_string()); ok&=cok
        r=int.from_bytes(rep[0x2A0:0x2A0+72],'little'); s=int.from_bytes(rep[0x2E8:0x2E8+72],'little')
        try:
            cert.public_key().verify(encode_dss_signature(r,s), rep[:0x2A0], ec.ECDSA(hashes.SHA384())); print("VCEK signature: valid")
        except Exception as e: print("VCEK signature: INVALID", e); ok=False
    else: print("VCEK signature: not checked (no --vcek)")
    print("VERDICT", "PASS" if ok else "FAIL"); sys.exit(0 if ok else 1)
if __name__=='__main__': main()
