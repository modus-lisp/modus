#!/bin/bash
# attested-ssh-client.sh -- the REMOTE VERIFIER's side of attested SSH, Nitro.
#   test/nitro/attested-ssh-client.sh HOST PORT [verify-attestation.py args: --pcr0 HEX --pcr1 HEX --pcr2 HEX
#                                                 --signing-cert PEM (requires PCR8) --root PEM]
# 1. Complete an SSH handshake with the modus server and record the host key it
#    proved possession of (from OpenSSH's own known_hosts line, not anything the
#    server prints).
# 2. Over that session evaluate (nitro-attest-ssh "<fresh nonce>"); collect the
#    NITRO-HOSTKEY, NITRO-USER-DATA and NITRO-DOC lines it prints.
# 3. Require printed host key == handshake host key and printed user_data ==
#    SHA-512 of it; then verify-attestation.py on the document with the
#    handshake key as --hostkey-hex, the nonce, and whatever PCR/root args given.
# Exit 0 = attested; 2 = no document (server printed NITRO-STATUS); 1 = mismatch.
set -u
HOST=${1:?}; PORT=${2:?}; shift 2
W=$(mktemp -d /tmp/attested-ssh.XXXXXX); ASK=$W/askpass.sh; printf '#!/bin/sh\necho x\n' > $ASK; chmod +x $ASK
cd "$(dirname "$0")/../.."
NONCE=$(python3 -c "import os;print(os.urandom(16).hex())")
raw=$(echo "(nitro-attest-ssh \"$NONCE\")" | SSH_ASKPASS=$ASK SSH_ASKPASS_REQUIRE=force DISPLAY=:0 setsid -w timeout 90 \
        ssh -p $PORT -o StrictHostKeyChecking=no -o UserKnownHostsFile=$W/known_hosts -o LogLevel=ERROR \
            -o HostKeyAlgorithms=ssh-ed25519 -o PreferredAuthentications=none,password -o NumberOfPasswordPrompts=1 \
            test@$HOST 2>&1 | tr -d '\r')
printf '%s\n' "$raw" > $W/transcript.txt
lines=$(printf '%s\n' "$raw" | sed 's/^modus> //')
hk_b64=$(awk '$2=="ssh-ed25519"{print $3; exit}' $W/known_hosts)
[ -n "$hk_b64" ] || { echo "FAIL: no ssh-ed25519 host key recorded by the handshake (transcript $W/transcript.txt)"; tail -3 $W/transcript.txt; exit 1; }
hk_hex=$(python3 -c "import base64,struct;b=base64.b64decode('$hk_b64');n=struct.unpack_from('>I',b,0)[0];m=struct.unpack_from('>I',b,4+n)[0];print(b[8+n:8+n+m].hex())")
echo "handshake host key: $hk_hex"
printed_hk=$(printf '%s\n' "$lines" | grep -a '^NITRO-HOSTKEY ' | awk '{print $2}' | tr -d '\n')
printed_ud=$(printf '%s\n' "$lines" | grep -a '^NITRO-USER-DATA ' | awk '{print $2}' | tr -d '\n')
status=$(printf '%s\n' "$lines" | grep -a '^NITRO-STATUS ' | head -1)
[ -n "$printed_hk" ] || { echo "FAIL: server printed no NITRO-HOSTKEY (transcript $W/transcript.txt)"; tail -3 $W/transcript.txt; exit 1; }
[ "$printed_hk" = "$hk_hex" ] && echo "server's host key == handshake host key: yes" || { echo "FAIL: server printed host key $printed_hk"; exit 1; }
want_ud=$(python3 -c "import hashlib;print(hashlib.sha512(bytes.fromhex('$hk_hex')).hexdigest())")
[ "$printed_ud" = "$want_ud" ] && echo "server's user_data == SHA-512(host key): yes" || { echo "FAIL: user_data $printed_ud != $want_ud"; exit 1; }
if [ -n "$status" ]; then echo "no document: $status"; exit 2; fi
printf '%s\n' "$lines" | grep -a '^NITRO-DOC ' | awk '{print $2}' | tr -d '\n' | python3 -c "import sys;open('$W/doc.cose','wb').write(bytes.fromhex(sys.stdin.read()))"
echo "document: $(stat -c %s $W/doc.cose) bytes -> $W/doc.cose"
python3 test/nitro/verify-attestation.py $W/doc.cose --hostkey-hex "$hk_hex" --nonce "$NONCE" "$@"
