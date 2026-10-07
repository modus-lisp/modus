#!/bin/bash
# run-hosted-ssh.sh -- the hosted SSH server against a REAL OpenSSH client.
#   test/run-hosted-ssh.sh [./modus]
# Starts `modus --eval (ssh-serve-tcp 0)' (127.0.0.1, kernel-chosen port, printed),
# then: (1) an exec of (+ 1 2) must answer "= 3"; (2) the attested client must
# complete a handshake, see the server print the SAME host key OpenSSH recorded,
# and user_data = SHA-512 of it.  Without /dev/nsm the client exits 2
# ("no document", NITRO-STATUS (:NO-NSM ...)), which is the PASS here.
# Kills the server and verifies the port is no longer listening.
set -u
M=${1:-./modus}; cd "$(dirname "$0")/.."
W=$(mktemp -d /tmp/hosted-ssh.XXXXXX); ASK=$W/askpass.sh; printf '#!/bin/sh\necho x\n' > $ASK; chmod +x $ASK
$M --eval "(ssh-serve-tcp 0)" > $W/server.log 2>&1 & SP=$!
for i in $(seq 1 100); do grep -q "SSH: listening" $W/server.log 2>/dev/null && break; sleep 0.2; done
PORT=$(grep -a -o "SSH: listening 127.0.0.1:[0-9]*" $W/server.log | head -1 | sed 's/.*://')
[ -n "$PORT" ] || { echo "FAIL: server did not announce a port"; cat $W/server.log | tail -5; kill $SP 2>/dev/null; exit 1; }
echo "server on 127.0.0.1:$PORT (pid $SP)"
fail=0
raw=$(SSH_ASKPASS=$ASK SSH_ASKPASS_REQUIRE=force DISPLAY=:0 setsid -w timeout 60 \
      ssh -p $PORT -o StrictHostKeyChecking=no -o UserKnownHostsFile=$W/kh -o LogLevel=ERROR \
          -o HostKeyAlgorithms=ssh-ed25519 -o PreferredAuthentications=none,password -o NumberOfPasswordPrompts=1 \
          test@127.0.0.1 "(+ 1 2)" 2>&1 | tr -d '\r')
echo "exec (+ 1 2) -> $(printf '%s' "$raw" | tail -1)"
printf '%s\n' "$raw" | grep -q "^= 3$" && echo "ok: exec" || { echo "FAIL: exec (got: $raw)"; fail=1; }
test/nitro/attested-ssh-client.sh 127.0.0.1 $PORT; rc=$?
case $rc in 0) echo "ok: attested (an NSM is present)";; 2) echo "ok: handshake + host-key binding verified, no NSM here";; *) echo "FAIL: attested client rc=$rc"; fail=1;; esac
kill $SP 2>/dev/null; wait $SP 2>/dev/null
ss -ltn 2>/dev/null | grep -q ":$PORT " && { echo "FAIL: port $PORT still listening"; fail=1; }
[ $fail = 0 ] && echo "PASS: hosted SSH" || { echo "server log:"; tail -8 $W/server.log; echo "FAIL: hosted SSH"; }
exit $fail
