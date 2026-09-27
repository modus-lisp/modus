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
[ $fail = 0 ] && echo "PASS" || echo "FAIL"; exit $fail
