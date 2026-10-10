#!/bin/bash
# attested-ssh-client.sh — the REMOTE VERIFIER's side of attested SSH.
#
#   [KEY=id_ed25519] test/snp/attested-ssh-client.sh HOST PORT [--vcek CERT.pem] [--chain CHAIN.pem] [--measurement HEX]
#
# 1. Complete an SSH handshake with the modus server and record the host key it
#    proved possession of (from OpenSSH itself: the known_hosts line it writes,
#    not anything the server prints).
# 2. Over that session evaluate (snp-attest-ssh); collect SNP-HOSTKEY,
#    SNP-REPORT-DATA and the SNP-REPORT lines it prints.
# 3. Require the printed host key == the handshake's host key, the printed
#    report_data == SHA-512 of it, and hand the report to verify-report.py with
#    the handshake key as --hostkey, plus --vcek / --measurement if given.
# Exit 0 = attested; 2 = no report (server said SNP-STATUS ...), 1 = mismatch.
set -u
HOST=${1:?}; PORT=${2:?}; shift 2
W=$(mktemp -d ${TMPDIR:-/tmp}/attested-ssh.XXXXXX)
cd "$(dirname "$0")/../.."
# Key-only auth (KEY, default ~/.ssh/id_ed25519: the key baked in with
# MODUS_SSH_AUTH_KEY_HEX) and an EXEC request: the server evaluates the form,
# sends its output, exit-status, EOF and CLOSE.  A shell session would hold the
# single-threaded server after we hang up.
raw=$(timeout ${ATTEST_TIMEOUT:-300} ssh -p $PORT -i ${KEY:-$HOME/.ssh/id_ed25519} -o IdentitiesOnly=yes -o BatchMode=yes \
            -o StrictHostKeyChecking=no -o UserKnownHostsFile=$W/known_hosts -o LogLevel=ERROR \
            -o HostKeyAlgorithms=ssh-ed25519 test@$HOST "(snp-attest-ssh)" </dev/null 2>&1 | tr -d '\r')
printf '%s\n' "$raw" > $W/transcript.txt
# The REPL prompt precedes the first printed line ("modus> SNP-HOSTKEY ..."), so
# lines are matched by tag, not by line start.
lines=$(printf '%s\n' "$raw" | sed 's/^modus> //')
hk_b64=$(awk '$2=="ssh-ed25519"{print $3; exit}' $W/known_hosts)
[ -n "$hk_b64" ] || { echo "FAIL: no ssh-ed25519 host key recorded by the handshake"; exit 1; }
hk_hex=$(python3 -c "import base64,struct,sys;b=base64.b64decode('$hk_b64');n=struct.unpack_from('>I',b,0)[0];m=struct.unpack_from('>I',b,4+n)[0];print(b[8+n:8+n+m].hex())")
echo "handshake host key: $hk_hex"
printed_hk=$(printf '%s\n' "$lines" | grep -a '^SNP-HOSTKEY ' | awk '{print $2}' | tr -d '\n')
printed_rd=$(printf '%s\n' "$lines" | grep -a '^SNP-REPORT-DATA ' | awk '{print $2}' | tr -d '\n')
status=$(printf '%s\n' "$lines" | grep -a '^SNP-STATUS ' | head -1)
[ -n "$printed_hk" ] || { echo "FAIL: server printed no SNP-HOSTKEY (transcript $W/transcript.txt)"; tail -3 $W/transcript.txt; exit 1; }
[ "$printed_hk" = "$hk_hex" ] && echo "server's host key == handshake host key: yes" || { echo "FAIL: server printed host key $printed_hk"; exit 1; }
want_rd=$(python3 -c "import hashlib;print(hashlib.sha512(bytes.fromhex('$hk_hex')).hexdigest())")
[ "$printed_rd" = "$want_rd" ] && echo "server's report_data == SHA-512(host key): yes" || { echo "FAIL: report_data $printed_rd != $want_rd"; exit 1; }
if [ -n "$status" ]; then echo "no report: $status"; exit 2; fi
 printf '%s\n' "$lines" | grep -a '^SNP-REPORT ' | awk '{print $2}' | tr -d '\n' | python3 -c "import sys;open('$W/report.bin','wb').write(bytes.fromhex(sys.stdin.read()))"
echo "report: $(stat -c %s $W/report.bin) bytes -> $W/report.bin"
python3 test/snp/verify-report.py $W/report.bin --hostkey-hex "$hk_hex" "$@"
