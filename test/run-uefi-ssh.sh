#!/bin/bash
# run-uefi-ssh.sh — boot an x64 UEFI CL image built with MODUS_NET_BUILD=1
# MODUS_SSH_BUILD=1 under OVMF with QEMU user networking, start its SSH server
# from the serial REPL, and evaluate forms over SSH from the host (paramiko).
#
#   test/run-uefi-ssh.sh IMAGE.efi [PORT] [FORM ...]    default port 2222, form (+ 1 2)
set -u
EFI=${1:?usage: run-uefi-ssh.sh IMAGE.efi [PORT] [FORM ...]}; shift
PORT=${1:-2222}; [ $# -gt 0 ] && shift; [ $# = 0 ] && set -- "(+ 1 2)" "(list (lisp-implementation-type) (* 6 7))"
cd "$(dirname "$0")/.."
OVMF=${OVMF:-/usr/share/OVMF/OVMF_CODE_4M.fd}
W=$(mktemp -d /tmp/modus-uefi-ssh.XXXXXX); IMG=$W/boot.img; VARS=$W/vars.fd; OUT=${OUT:-$W/serial.txt}; FIFO=$W/fifo
SZ=$(( ( $(stat -c %s "$EFI") / 1048576 ) + 40 ))
dd if=/dev/zero of=$IMG bs=1M count=$SZ status=none
mformat -i $IMG -F :: && mmd -i $IMG ::/EFI && mmd -i $IMG ::/EFI/BOOT && mcopy -i $IMG "$EFI" ::/EFI/BOOT/BOOTX64.EFI || exit 1
cp /usr/share/OVMF/OVMF_VARS_4M.fd $VARS; mkfifo $FIFO
timeout ${TIMEOUT:-300} qemu-system-x86_64 -drive if=pflash,format=raw,readonly=on,file=$OVMF -drive if=pflash,format=raw,file=$VARS \
  -drive format=raw,file=$IMG -m ${MEM:-512} -nographic -no-reboot \
  -device e1000,netdev=net0,romfile=,rombar=0 -netdev user,id=net0,hostfwd=tcp::${PORT}-:22 < $FIFO > $OUT 2>&1 &
QP=$!; exec 3>$FIFO
for i in $(seq 1 1800); do kill -0 $QP 2>/dev/null || { echo "FAIL: QEMU exited"; tail -5 $OUT; exit 1; }; tail -c 4 $OUT 2>/dev/null | grep -q '> $' && break; sleep 0.1; done
tail -c 4 $OUT | grep -q '> $' || { echo "FAIL: no REPL prompt (serial tail:)"; tr -d '\r' < $OUT | tail -5; kill $QP; exit 1; }
echo "DHCP: $(tr -d '\r' < $OUT | grep -a 'DHCP:IP\|DHCP:F' | tail -1)"
printf '(ssh-boot)\r' >&3
for i in $(seq 1 600); do grep -aq "NETUP\|SSH:22" $OUT && break; sleep 0.1; done
grep -aq "NETUP\|SSH:22" $OUT || { echo "FAIL: ssh-boot did not reach NETUP (serial tail:)"; tr -d '\r' < $OUT | tail -6; kill $QP; exit 1; }
echo "server: $(tr -d '\r' < $OUT | grep -a 'SSH:22\|NETUP' | tr '\n' ' ')"
sleep 1
# OpenSSH, not paramiko: the server speaks curve25519-sha256 + chacha20-poly1305@openssh.com,
# which paramiko lacks.  Any password is accepted; SSH_ASKPASS supplies one non-interactively.
ASK=$W/askpass.sh; printf '#!/bin/sh\necho x\n' > $ASK; chmod +x $ASK
RC=0
for f in "$@"; do
  # The server answers `modus> = VALUE` and does not close the channel on EOF, so
  # the client is bounded by timeout and the reply is taken from anywhere in the stream.
  raw=$(echo "$f" | SSH_ASKPASS=$ASK SSH_ASKPASS_REQUIRE=force DISPLAY=:0 setsid -w timeout 40 \
           ssh -p $PORT -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR \
               -o PreferredAuthentications=none,password -o NumberOfPasswordPrompts=1 test@127.0.0.1 2>&1 | tr -d '\r')
  reply=$(printf '%s' "$raw" | grep -ao '= .*' | head -1 | sed 's/modus>.*//')
  echo "$f -> ${reply:-NO REPLY: $(printf '%s' "$raw" | tail -c 120)}"; [ -n "$reply" ] || RC=1
done
if [ -n "${KEEP:-}" ]; then echo "KEEP: QEMU pid $QP still serving on port $PORT; serial $OUT; work $W"; else exec 3>&-; kill $QP 2>/dev/null; wait $QP 2>/dev/null; fi
echo "serial tail:"; tr -d '\r' < $OUT | tail -4 | cut -c1-120
[ $RC = 0 ] && echo "PASS: SSH REPL over QEMU user net" || { echo "FAIL (rc=$RC)"; }
exit $RC
