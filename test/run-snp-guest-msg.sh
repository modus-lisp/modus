#!/bin/bash
# run-snp-guest-msg.sh — the SEV-SNP guest-message framing (crypto/snp-guest.lisp)
# round-trips against a Python fake PSP (test/snp/fake-psp.py), on the HOST
# (SBCL) and IN-IMAGE (a modus CLI), plus tamper checks.  QEMU-free, needs
# python3-cryptography.   usage: test/run-snp-guest-msg.sh [modus-cli]
set -uo pipefail; cd "$(dirname "$0")/.."
CLI=${1:-./modus}; W=tmp/snp; mkdir -p $W; fail=0
python3 -c "import os;print(os.urandom(32).hex())" > $W/vmpck.hex
python3 -c "import hashlib;print(hashlib.sha512(b'ssh-ed25519 host key placeholder').hexdigest())" > $W/rd.hex
LOADS="--load crypto/aes.lisp --load crypto/gcm.lisp --load crypto/snp-guest.lisp --load test/snp/guest-msg-probe.lisp"
run_side() { # name cmd...
  local name=$1; shift; rm -f $W/resp.bin
  "$@" 2>&1 | grep -aq "REQ-WRITTEN" || { echo "FAIL[$name]: request not written"; fail=1; return; }
  python3 test/snp/fake-psp.py respond $W/vmpck.hex $W/req.bin $W/resp.bin >/dev/null || { echo "FAIL[$name]: fake PSP rejected the request"; fail=1; return; }
  local out; out=$("$@" 2>&1)
  echo "$out" | grep -aq "RD-MATCH YES" && echo "ok[$name]: request accepted by the PSP, response opened, report_data round-tripped" || { echo "FAIL[$name]:"; echo "$out" | tail -4; fail=1; }
  # tamper: flip a ciphertext byte -> :auth ; wrong seqno -> :seqno
  python3 - <<PY
p=bytearray(open('$W/resp.bin','rb').read()); p[96]^=1; open('$W/resp.bin','wb').write(p)
PY
  "$@" 2>&1 | grep -aq "OPEN-FAILED status=NIL why=AUTH" && echo "ok[$name]: tampered ciphertext rejected (:auth)" || { echo "FAIL[$name]: tampered ciphertext accepted"; fail=1; }
  python3 test/snp/fake-psp.py respond $W/vmpck.hex $W/req.bin $W/resp.bin >/dev/null
  python3 - <<PY
import struct; p=bytearray(open('$W/resp.bin','rb').read()); struct.pack_into('<Q',p,0x20,99); open('$W/resp.bin','wb').write(p)
PY
  "$@" 2>&1 | grep -aq "OPEN-FAILED status=NIL why=SEQNO" && echo "ok[$name]: wrong sequence number rejected (:seqno)" || { echo "FAIL[$name]: wrong seqno accepted"; fail=1; }
}
run_side host sbcl --noinform --non-interactive $LOADS
[ -x "$CLI" ] && run_side image timeout 600 "$CLI" $LOADS --quit || echo "skip[image]: no CLI at $CLI"

# ---- attested SSH, end to end with OUR OWN signing key standing in for the VCEK:
# report_data = SHA-512(raw Ed25519 host public key); the fake PSP signs the
# report (P-384); verify-report.py must accept the right host key, the right
# measurement and the signature, and reject a different host key, a different
# measurement and a flipped signature byte.
attested_arm() { # name cmd...
  local name=$1; shift
  python3 test/snp/fake-psp.py keygen $W >/dev/null || { echo "FAIL[$name]: keygen"; fail=1; return; }
  python3 - <<PY
from cryptography.hazmat.primitives.asymmetric import ed25519
from cryptography.hazmat.primitives import serialization
import hashlib, base64, struct
k=ed25519.Ed25519PrivateKey.generate().public_key().public_bytes(serialization.Encoding.Raw, serialization.PublicFormat.Raw)
open('$W/hostkey.bin','wb').write(k); open('$W/rd.hex','w').write(hashlib.sha512(k).hexdigest()+'\n')
blob=struct.pack('>I',11)+b'ssh-ed25519'+struct.pack('>I',32)+k
open('$W/hostkey.b64','w').write('ssh-ed25519 '+base64.b64encode(blob).decode()+'\n')
open('$W/meas.hex','w').write(('ab'*48)+'\n')
PY
  rm -f $W/resp.bin $W/report.bin
  "$@" 2>&1 | grep -aq "REQ-WRITTEN" || { echo "FAIL[$name]: request not written"; fail=1; return; }
  python3 test/snp/fake-psp.py respond $W/vmpck.hex $W/req.bin $W/resp.bin $(cat $W/meas.hex) $W/vcek-key.pem >/dev/null || { echo "FAIL[$name]: fake PSP"; fail=1; return; }
  "$@" 2>&1 | grep -aq "RD-MATCH YES" || { echo "FAIL[$name]: report not opened"; fail=1; return; }
  [ -s $W/report.bin ] || { echo "FAIL[$name]: no report.bin"; fail=1; return; }
  local V="python3 test/snp/verify-report.py $W/report.bin --vcek $W/vcek.pem --measurement $(cat $W/meas.hex)"
  $V --hostkey $W/hostkey.bin >/dev/null && echo "ok[$name]: signed report verifies against the host key file" || { echo "FAIL[$name]: verify (file)"; fail=1; }
  $V --hostkey-b64 "$(cat $W/hostkey.b64)" >/dev/null && echo "ok[$name]: ... and against the ssh-keyscan form of the same key" || { echo "FAIL[$name]: verify (b64)"; fail=1; }
  python3 -c "import os;open('$W/other.bin','wb').write(os.urandom(32))"
  $V --hostkey $W/other.bin >/dev/null && { echo "FAIL[$name]: a DIFFERENT host key was accepted"; fail=1; } || echo "ok[$name]: a different host key is rejected"
  python3 test/snp/verify-report.py $W/report.bin --vcek $W/vcek.pem --hostkey $W/hostkey.bin --measurement $(printf 'cd%.0s' $(seq 48)) >/dev/null && { echo "FAIL[$name]: a different measurement was accepted"; fail=1; } || echo "ok[$name]: a different measurement is rejected"
  python3 -c "p=bytearray(open('$W/report.bin','rb').read()); p[0x2A0]^=1; open('$W/report-bad.bin','wb').write(p)"
  python3 test/snp/verify-report.py $W/report-bad.bin --vcek $W/vcek.pem --hostkey $W/hostkey.bin >/dev/null && { echo "FAIL[$name]: a flipped signature byte was accepted"; fail=1; } || echo "ok[$name]: a flipped signature byte is rejected"
  python3 test/snp/fake-psp.py keygen $W/otherkey >/dev/null
  python3 test/snp/verify-report.py $W/report.bin --vcek $W/otherkey/vcek.pem --hostkey $W/hostkey.bin >/dev/null && { echo "FAIL[$name]: a report signed by another key was accepted"; fail=1; } || echo "ok[$name]: another signer's certificate is rejected"
}
attested_arm attested-host sbcl --noinform --non-interactive $LOADS
[ -x "$CLI" ] && attested_arm attested-image timeout 600 "$CLI" $LOADS --quit
[ $fail = 0 ] && echo "PASS" || echo "FAIL"; exit $fail
