#!/bin/bash
# vpsbg-spin-round.sh POINT -- one round of the spin-point bisect on VPSBG server
# 27029: stage $SPIN_DIR/POINT.efi behind GRUB's one-shot "Modus" entry, restart
# through the API, classify from the hypervisor's view (REACHED = still running at
# ~100% CPU, DIED = stopped, IDLE = running at low CPU), then return to Ubuntu.
# Needs: Ubuntu up with the "Modus" GRUB entry (docs/vpsbg-snp-status.md), tokens
# in ~/.config/vpsbg/, root SSH with ~/.ssh/id_ed25519.
P=$1; IMG=${SPIN_DIR:-$HOME/merge-out/spin}/$P.efi; IP=87.120.37.135; ID=27029
SSHO="-o BatchMode=yes -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o LogLevel=ERROR -o ConnectTimeout=15 -i $HOME/.ssh/id_ed25519 -o IdentitiesOnly=yes"
RTOK=$(tr -d '\n' < ~/.config/vpsbg/read-only.token); TOK=$(tr -d '\n' < ~/.config/vpsbg/read-write.token)
state(){ curl -s -m 20 -H "Authorization: Bearer $RTOK" https://api.vpsbg.eu/v1/servers/$ID | python3 -c "import json,sys;d=json.load(sys.stdin);s=d['state'];print(int(bool(s['running'])), s['cpu']['used'])"; }
ubuntu_up(){ for i in $(seq 1 30); do timeout 30 ssh $SSHO root@$IP true 2>/dev/null && return 0; sleep 10; done; return 1; }
ubuntu_up || { echo "$P: Ubuntu not reachable before the round"; exit 2; }
scp -q $SSHO "$IMG" root@$IP:/boot/efi/EFI/modus/modus.efi || exit 2
ssh $SSHO root@$IP 'sha256sum /boot/efi/EFI/modus/modus.efi | cut -c1-12; grub-reboot Modus; grub-editenv list | grep next_entry' 2>/dev/null
# NOT systemctl reboot: an SNP guest cannot reset itself here, the VM just stops.
# An API restart is an external stop+start, straight into GRUB's one-shot.
curl -s -o /dev/null -m 60 -X POST -H "Authorization: Bearer $TOK" -H 'Accept: application/json' https://api.vpsbg.eu/v1/servers/$ID/restart
sleep 75
spin=0; dead=0; low=0; samples=""
for i in $(seq 1 15); do read r c <<<"$(state)"; samples="$samples $r/$c"
  if [ "$r" = 0 ]; then dead=$((dead+1)); elif awk "BEGIN{exit !($c>50)}"; then spin=$((spin+1)); else low=$((low+1)); fi; sleep 10; done
if timeout 20 ssh $SSHO root@$IP true 2>/dev/null; then verdict="UBUNTU (GRUB did not boot Modus)";
elif [ $dead -ge 3 ]; then verdict="DIED before $P"; elif [ $spin -ge 8 ]; then verdict="REACHED $P"; elif [ $low -ge 8 ]; then verdict="IDLE before $P (running, low cpu)"; else verdict="UNCLEAR (low-cpu $low)"; fi
echo "$P: $verdict | running/cpu:$samples"
# back to Ubuntu: a stopped guest needs START, a spinning one RESTART
read r c <<<"$(state)"
if [ "$r" = 0 ]; then ep=start; else ep=restart; fi
curl -s -o /dev/null -m 60 -X POST -H "Authorization: Bearer $TOK" -H 'Accept: application/json' https://api.vpsbg.eu/v1/servers/$ID/$ep
sleep 30; ubuntu_up || { sleep 20; curl -s -o /dev/null -m 60 -X POST -H "Authorization: Bearer $TOK" -H 'Accept: application/json' https://api.vpsbg.eu/v1/servers/$ID/start; ubuntu_up; } && echo "$P: back on Ubuntu" || echo "$P: Ubuntu NOT back"
