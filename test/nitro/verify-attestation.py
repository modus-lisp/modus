#!/usr/bin/env python3
"""verify-attestation.py -- check a Nitro attestation document (COSE_Sign1) off-box.

  verify-attestation.py DOC.cose [--root ROOT.pem] [--pcr0 HEX] [--pcr1 HEX] [--pcr2 HEX]
                        [--nonce HEX] [--user-data HEX | --hostkey-b64 BLOB | --hostkey-hex HEX]

Checks, each printed, any failure fatal:
  * COSE_Sign1 shape, protected header alg ES384;
  * the leaf certificate in the payload chains to --root (default: AWS's Nitro
    root, test/nitro/aws-nitro-root.pem) through the cabundle, each link's
    signature verified, validity dates honoured;
  * the signature over Sig_structure with the leaf's P-384 key;
  * PCR0/1/2 equal the expected values (the EIF's, from eif_build / nitro-cli);
  * nonce echoed; user_data == given bytes or SHA-512 of the given host key
    (the same binding as the SEV side).
Prints module_id, timestamp and every non-zero PCR so a policy can be written."""
import sys, hashlib, base64, struct, datetime, cbor2
from cryptography import x509
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.hazmat.primitives.asymmetric.utils import encode_dss_signature
from cryptography.hazmat.primitives.serialization import Encoding
def main():
    a = sys.argv[1:]; doc = open(a[0], "rb").read(); opts = {a[i]: a[i+1] for i in range(1, len(a), 2)}
    here = __file__.rsplit("/", 1)[0]
    root = x509.load_pem_x509_certificate(open(opts.get("--root", here + "/aws-nitro-root.pem"), "rb").read())
    cose = cbor2.loads(doc)
    if isinstance(cose, cbor2.CBORTag): assert cose.tag == 18, cose.tag; cose = cose.value
    protected, unprotected, payload, sig = cose
    assert cbor2.loads(protected) == {1: -35}, "protected header is not ES384"
    p = cbor2.loads(payload); ok = True
    print(f"module_id {p['module_id']} digest {p['digest']} timestamp {datetime.datetime.fromtimestamp(p['timestamp']/1000, datetime.timezone.utc).isoformat()}")
    for i, v in sorted(p["pcrs"].items()):
        if any(v): print(f"PCR{i} {v.hex()}")
    leaf = x509.load_der_x509_certificate(p["certificate"]); chain = [x509.load_der_x509_certificate(c) for c in p["cabundle"]]
    # chain: cabundle[0] must be the root given; each cert signs the next; the last signs the leaf
    now = datetime.datetime.now(datetime.timezone.utc)
    def signs(issuer, cert):
        try:
            issuer.public_key().verify(cert.signature, cert.tbs_certificate_bytes, ec.ECDSA(cert.signature_hash_algorithm))
            return cert.not_valid_before_utc <= now <= cert.not_valid_after_utc and cert.issuer == issuer.subject
        except Exception: return False
    chain_ok = chain[0].public_bytes(Encoding.DER) == root.public_bytes(Encoding.DER)
    links = chain + [leaf]
    for i in range(1, len(links)): chain_ok &= signs(links[i-1], links[i])
    print("certificate chain to root:", chain_ok); ok &= chain_ok
    sig_structure = cbor2.dumps(["Signature1", protected, b"", payload])
    r, s = int.from_bytes(sig[:48], "big"), int.from_bytes(sig[48:], "big")
    try: leaf.public_key().verify(encode_dss_signature(r, s), sig_structure, ec.ECDSA(hashes.SHA384())); print("signature: valid")
    except Exception as e: print("signature: INVALID", e); ok = False
    for i in (0, 1, 2):
        if f"--pcr{i}" in opts:
            want = bytes.fromhex(opts[f"--pcr{i}"]); print(f"PCR{i} matches:", p["pcrs"][i] == want); ok &= p["pcrs"][i] == want
    if "--nonce" in opts:
        want = bytes.fromhex(opts["--nonce"]); print("nonce echoed:", p.get("nonce") == want); ok &= p.get("nonce") == want
    hk = None
    if "--hostkey-hex" in opts: hk = bytes.fromhex(opts["--hostkey-hex"])
    if "--hostkey-b64" in opts:
        blob = base64.b64decode(opts["--hostkey-b64"].split()[-1]); n = struct.unpack_from(">I", blob, 0)[0]
        m = struct.unpack_from(">I", blob, 4+n)[0]; hk = blob[8+n:8+n+m]
    if hk is not None: opts["--user-data"] = hashlib.sha512(hk).hexdigest(); print("hostkey", hk.hex())
    if "--user-data" in opts:
        want = bytes.fromhex(opts["--user-data"]); print("user_data matches:", p.get("user_data") == want); ok &= p.get("user_data") == want
    print("VERDICT", "PASS" if ok else "FAIL"); sys.exit(0 if ok else 1)
if __name__ == "__main__": main()
