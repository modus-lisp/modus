#!/bin/bash
# vpsbg-ssh-round.sh IMAGE.efi [FORM ...] -- boot IMAGE once on VPSBG server 27029
# through GRUB's one-shot "Modus" entry (docs/vpsbg-snp-status.md), wait for its
# key-only SSH server, evaluate FORMs (default (+ 1 2)), then return to Ubuntu.
# Same prerequisites as vpsbg-spin-round.sh.  Modus's host key is not Ubuntu's,
# so its known_hosts is a throwaway file.
IMG=${1:?usage: vpsbg-ssh-round.sh IMAGE.efi [FORM ...]}; shift
# VERBOSE=N: one ssh -vvv attempt first, last N lines.  SAVE=prefix: each reply to prefix.K
[ $# = 0 ] && set -- "(+ 1 2)"
IP=87.120.37.135; ID=27029; KH=$(mktemp)
SSHO="-o BatchMode=yes -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -o ConnectTimeout=15 -i $HOME/.ssh/id_ed25519 -o IdentitiesOnly=yes"
MSSHO="-o BatchMode=yes -o StrictHostKeyChecking=no -o UserKnownHostsFile=$KH -o LogLevel=ERROR -o ConnectTimeout=10 -i $HOME/.ssh/id_ed25519 -o IdentitiesOnly=yes"
RTOK=$(tr -d '\n' < ~/.config/vpsbg/read-only.token); TOK=$(tr -d '\n' < ~/.config/vpsbg/read-write.token)
state(){ curl -s -m 20 -H "Authorization: Bearer $RTOK" https://api.vpsbg.eu/v1/servers/$ID | python3 -c "import json,sys;d=json.load(sys.stdin);s=d['state'];print(int(bool(s['running'])), s['cpu']['used'])"; }
api(){ curl -s -o /dev/null -m 60 -X POST -H "Authorization: Bearer $TOK" -H 'Accept: application/json' https://api.vpsbg.eu/v1/servers/$ID/$1; }
# Modus answers this ssh too (it evaluates the command as Lisp), so require Linux.
ubuntu_up(){ for i in $(seq 1 30); do timeout 30 ssh $SSHO root@$IP 'uname -s' 2>/dev/null | grep -q Linux && return 0; sleep 10; done; return 1; }
# An EXEC request, not a shell: the server answers, sends exit-status/EOF/CLOSE and
# frees itself.  A shell session never times out once authenticated, and this TCP
# stack does not notice the client's FIN, so it would hold the server for good.
modus_eval(){ timeout ${EVAL_TIMEOUT:-60} ssh $MSSHO -p 22 test@$IP "$1" </dev/null 2>&1 | tr -d '\r' | grep -ao '= .*' | head -1; }
ubuntu_up || { echo "Ubuntu not reachable before the round"; exit 2; }
scp -q $SSHO "$IMG" root@$IP:/boot/efi/EFI/modus/modus.efi || exit 2
ssh $SSHO root@$IP 'sha256sum /boot/efi/EFI/modus/modus.efi | cut -c1-12; grub-reboot Modus; grub-editenv list | grep next_entry' 2>/dev/null
api restart; T0=$(date +%s); sleep 40
if [ -n "${VERBOSE:-}" ]; then
  # Wait for the banner, then ONE verbose attempt: shows how far the handshake gets.
  for i in $(seq 1 30); do b=$(timeout 8 bash -c "exec 3<>/dev/tcp/$IP/22; head -c 7 <&3" 2>/dev/null); [ "$b" = SSH-2.0 ] && break; sleep 5; done
  echo "banner after $(( $(date +%s) - T0 )) s: $b"; sleep 5
  echo "(+ 1 2)" | timeout 120 ssh -vvv $MSSHO -p 22 test@$IP 2>&1 | tr -d '\r' | grep -av "^debug3: \(receive packet\|send packet\)" | tail -${VERBOSE}
fi
up=""; for i in $(seq 1 30); do r=$(modus_eval "(+ 1 2)"); [ -n "$r" ] && { up=1; break; }; sleep 5; done
if [ -n "$up" ]; then
  echo "MODUS SSH UP after $(( $(date +%s) - T0 )) s: (+ 1 2) $r"
  n=0; for f in "$@"; do n=$((n+1)); r=$(modus_eval "$f"); echo "$f -> $(echo "$r" | cut -c1-200)"
    [ -n "${SAVE:-}" ] && printf '%s\n' "$r" > "$SAVE.$n"; done
else
  read rr cc <<<"$(state)"; echo "NO MODUS SSH after $(( $(date +%s) - T0 )) s (running=$rr cpu=$cc)"
  # What the network can see: ICMP, a TCP handshake on 22, and the server banner.
  if timeout 8 bash -c "exec 3<>/dev/tcp/$IP/22" 2>/dev/null; then echo "tcp/22: connects"; else echo "tcp/22: no handshake"; fi
  echo "banner: $(timeout 8 bash -c "exec 3<>/dev/tcp/$IP/22; head -c 40 <&3" 2>/dev/null | tr -d '\r\n' | cat -v)"
fi
rm -f $KH
read rr cc <<<"$(state)"; if [ "$rr" = 0 ]; then api start; else api restart; fi
sleep 30; ubuntu_up || { sleep 20; api start; ubuntu_up; } && echo "back on Ubuntu" || echo "Ubuntu NOT back"
