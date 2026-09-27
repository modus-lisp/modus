#!/usr/bin/env python3
"""Stand-in for the PSP: decrypt a guest MSG_REPORT_REQ page, check it the way
the firmware would, and answer with a MSG_REPORT_RSP page carrying a synthetic
ATTESTATION_REPORT whose report_data echoes the request.  Also the reference
for the framing (built from the kernel's sev-guest.c conventions).

  fake-psp.py keygen  <dir>                  # our own P-384 "VCEK": <dir>/vcek-key.pem + vcek.pem
  fake-psp.py respond <vmpck.hex> <req.bin> <resp.bin> [measurement.hex|-] [vcek-key.pem]
  fake-psp.py check   <vmpck.hex> <req.bin>              # print decoded fields

With a key the report is SIGNED the way the PSP signs it: ECDSA P-384 over
SHA-384 of bytes 0..0x2A0, r and s as 72-byte little-endian fields at 0x2A0
and 0x2E8, sig_algo 1.  The key is ours, so what this proves off-hardware is
the whole chain EXCEPT the root of trust: framing, binding, verification.
"""
import sys, struct
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
HDR=96; AAD_OFF=0x30; AAD_LEN=48; PAGE=4096; REPORT_SIZE=1184
def open_msg(vmpck, page):
    seqno,=struct.unpack_from('<Q',page,0x20); algo,hver=page[0x30],page[0x31]
    hsz,=struct.unpack_from('<H',page,0x32); mtype,mver=page[0x34],page[0x35]
    msz,=struct.unpack_from('<H',page,0x36); vid=page[0x3C]
    assert algo==1 and hver==1 and hsz==96 and mver==1 and vid==0, (algo,hver,hsz,mver,vid)
    assert seqno!=0
    iv=struct.pack('<Q',seqno)+b'\0'*4
    pt=AESGCM(vmpck).decrypt(iv, bytes(page[HDR:HDR+msz])+bytes(page[0:16]), bytes(page[AAD_OFF:AAD_OFF+AAD_LEN]))
    return dict(seqno=seqno,type=mtype,payload=pt)
def build_msg(vmpck, seqno, mtype, payload):
    page=bytearray(PAGE)
    struct.pack_into('<Q',page,0x20,seqno); page[0x30]=1; page[0x31]=1
    struct.pack_into('<H',page,0x32,96); page[0x34]=mtype; page[0x35]=1
    struct.pack_into('<H',page,0x36,len(payload)); page[0x3C]=0
    iv=struct.pack('<Q',seqno)+b'\0'*4
    ct=AESGCM(vmpck).encrypt(iv, payload, bytes(page[AAD_OFF:AAD_OFF+AAD_LEN]))
    page[HDR:HDR+len(payload)]=ct[:-16]; page[0:16]=ct[-16:]
    return bytes(page)
def synthetic_report(report_data, vmpl, measurement, key=None):
    r=bytearray(REPORT_SIZE)
    struct.pack_into('<I',r,0,2); struct.pack_into('<I',r,4,7); struct.pack_into('<Q',r,8,0x30000)
    struct.pack_into('<I',r,0x30,vmpl); struct.pack_into('<I',r,0x34,1)
    r[0x50:0x90]=report_data; r[0x90:0xC0]=measurement
    r[0x1A0:0x1E0]=bytes(range(64))
    if key is None: r[0x2A0:0x2A0+144]=b'\x5a'*144
    else:
        from cryptography.hazmat.primitives.asymmetric import ec
        from cryptography.hazmat.primitives.asymmetric.utils import decode_dss_signature
        from cryptography.hazmat.primitives import hashes
        rr,ss=decode_dss_signature(key.sign(bytes(r[:0x2A0]), ec.ECDSA(hashes.SHA384())))
        r[0x2A0:0x2A0+72]=rr.to_bytes(72,'little'); r[0x2E8:0x2E8+72]=ss.to_bytes(72,'little')
    return bytes(r)
def keygen(d):
    import os, datetime
    from cryptography import x509
    from cryptography.x509.oid import NameOID
    from cryptography.hazmat.primitives import hashes, serialization
    from cryptography.hazmat.primitives.asymmetric import ec
    k=ec.generate_private_key(ec.SECP384R1())
    name=x509.Name([x509.NameAttribute(NameOID.COMMON_NAME,"SEV-VCEK stand-in (fake-psp.py)")])
    now=datetime.datetime.now(datetime.timezone.utc)
    cert=(x509.CertificateBuilder().subject_name(name).issuer_name(name).public_key(k.public_key())
          .serial_number(1).not_valid_before(now).not_valid_after(now+datetime.timedelta(days=3650))
          .sign(k, hashes.SHA384()))
    os.makedirs(d,exist_ok=True)
    open(f"{d}/vcek-key.pem","wb").write(k.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()))
    open(f"{d}/vcek.pem","wb").write(cert.public_bytes(serialization.Encoding.PEM))
    print(f"wrote {d}/vcek-key.pem {d}/vcek.pem")
if __name__=='__main__':
    cmd=sys.argv[1]
    if cmd=='keygen': keygen(sys.argv[2]); sys.exit(0)
    vmpck=bytes.fromhex(open(sys.argv[2]).read().strip()); req=open(sys.argv[3],'rb').read()
    m=open_msg(vmpck, req)
    if cmd=='check':
        print('seqno',m['seqno'],'type',m['type'],'payload',m['payload'].hex()); sys.exit(0)
    assert m['type']==5, m['type']
    rd=m['payload'][:64]; vmpl,=struct.unpack_from('<I',m['payload'],64)
    meas=bytes.fromhex(sys.argv[5]) if len(sys.argv)>5 and sys.argv[5]!='-' else bytes(range(48))
    key=None
    if len(sys.argv)>6:
        from cryptography.hazmat.primitives import serialization
        key=serialization.load_pem_private_key(open(sys.argv[6],'rb').read(), None)
    resp=struct.pack('<II',0,REPORT_SIZE)+b'\0'*24+synthetic_report(rd,vmpl,meas,key)
    open(sys.argv[4],'wb').write(build_msg(vmpck, m['seqno']+1, 6, resp)); print('responded seqno',m['seqno']+1)
