#!/usr/bin/env python3
"""fake-nsm.py -- a stand-in Nitro Security Module for tests, as fake-psp.py is for SEV.

  fake-nsm.py keygen DIR                          # our own root (P-384) + an NSM leaf under it
  fake-nsm.py document DIR OUT.cose --pcr0 HEX --pcr1 HEX --pcr2 HEX [--user-data HEX] [--nonce HEX] [--public-key HEX]

The document has the real layout: COSE_Sign1 (tag 18) with protected {1: -35 (ES384)},
payload = CBOR map {module_id, digest "SHA384", timestamp, pcrs {0..15: 48 bytes},
certificate (leaf DER), cabundle [root DER], public_key, user_data, nonce},
signature = ECDSA P-384 / SHA-384 over Sig_structure ["Signature1", protected, b"", payload]
with r||s (96 bytes).  What a real NSM adds is only the root of trust; the verifier
takes --root to say which one to require."""
import sys, os, time, cbor2
from cryptography import x509
from cryptography.x509.oid import NameOID
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.hazmat.primitives.asymmetric.utils import decode_dss_signature
import datetime
def keygen(d):
    os.makedirs(d, exist_ok=True); now = datetime.datetime.now(datetime.timezone.utc)
    root_k = ec.generate_private_key(ec.SECP384R1()); leaf_k = ec.generate_private_key(ec.SECP384R1())
    rn = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "fake-nsm root (test/nitro/fake-nsm.py)")])
    ln = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "fake-nsm leaf")])
    root = (x509.CertificateBuilder().subject_name(rn).issuer_name(rn).public_key(root_k.public_key()).serial_number(1)
            .not_valid_before(now).not_valid_after(now + datetime.timedelta(days=3650))
            .add_extension(x509.BasicConstraints(ca=True, path_length=None), critical=True).sign(root_k, hashes.SHA384()))
    leaf = (x509.CertificateBuilder().subject_name(ln).issuer_name(rn).public_key(leaf_k.public_key()).serial_number(2)
            .not_valid_before(now).not_valid_after(now + datetime.timedelta(days=3650)).sign(root_k, hashes.SHA384()))
    for n, obj in (("root.pem", root), ("leaf.pem", leaf)): open(f"{d}/{n}", "wb").write(obj.public_bytes(serialization.Encoding.PEM))
    open(f"{d}/leaf-key.pem", "wb").write(leaf_k.private_bytes(serialization.Encoding.PEM, serialization.PrivateFormat.PKCS8, serialization.NoEncryption()))
    print("wrote", d)
def document(d, out, opts):
    leaf = x509.load_pem_x509_certificate(open(f"{d}/leaf.pem", "rb").read()); root = x509.load_pem_x509_certificate(open(f"{d}/root.pem", "rb").read())
    key = serialization.load_pem_private_key(open(f"{d}/leaf-key.pem", "rb").read(), None)
    pcrs = {i: bytes(48) for i in range(16)}
    for i in (0, 1, 2, 3, 4, 8):
        if f"--pcr{i}" in opts: pcrs[i] = bytes.fromhex(opts[f"--pcr{i}"])
    def opt(n): return bytes.fromhex(opts[n]) if n in opts else None
    payload = {"module_id": "i-0fake0000000000000-enc0fake00000000", "digest": "SHA384", "timestamp": int(time.time() * 1000),
               "pcrs": pcrs, "certificate": leaf.public_bytes(serialization.Encoding.DER),
               "cabundle": [root.public_bytes(serialization.Encoding.DER)],
               "public_key": opt("--public-key"), "user_data": opt("--user-data"), "nonce": opt("--nonce")}
    protected = cbor2.dumps({1: -35}); pl = cbor2.dumps(payload)
    sig_structure = cbor2.dumps(["Signature1", protected, b"", pl])
    r, s = decode_dss_signature(key.sign(sig_structure, ec.ECDSA(hashes.SHA384())))
    cose = cbor2.dumps(cbor2.CBORTag(18, [protected, {}, pl, r.to_bytes(48, "big") + s.to_bytes(48, "big")]))
    open(out, "wb").write(cose); print("wrote", out, len(cose), "bytes")
if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "keygen": keygen(sys.argv[2])
    else:
        a = sys.argv[4:]; opts = {a[i]: a[i+1] for i in range(0, len(a), 2)}
        document(sys.argv[2], sys.argv[3], opts)
