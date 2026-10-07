#!/bin/bash
# snp-host-bringup.sh -- the rented EPYC box, start to attested SSH, in one script.
#
#   scripts/snp-host-bringup.sh preflight            # is this box an SNP host?  (exit 0/1)
#   scripts/snp-host-bringup.sh install              # QEMU >= 9.1 (distro or source), python deps
#   scripts/snp-host-bringup.sh launch KIT [PORT]    # boot KIT/generic.efi as an SNP guest; serial in KIT/serial.txt
#   scripts/snp-host-bringup.sh attest KIT [PORT]    # ssh in, pull the report, fetch the VCEK, verify
#
# KIT is the directory scripts/snp-kit.sh assembled: OVMF.fd (the AmdSev build),
# generic.efi (the DDC'd SNP-mode image), expected-measurement.json, test/snp/*.
# The firmware and the image are built and measured OFF the box; the box only
# runs them.  Read the serial log before blaming anything: the guest prints
# `SNP: active 1 ...` from (snp-attest-selftest) when the C-bit was found.
set -u
cmd=${1:-help}; KIT=${2:-}; PORT=${3:-2222}
say() { echo "[snp-host] $(date +%H:%M:%S) $*"; }
case $cmd in
preflight)
  rc=0
  k=$(uname -r); say "kernel $k (need >= 6.11 for KVM SEV-SNP)"
  grep -qw sev_snp /proc/cpuinfo && say "cpu: sev_snp flag present" || { say "FAIL: no sev_snp cpu flag (BIOS SEV-SNP off, or not Zen 3+)"; rc=1; }
  v=$(cat /sys/module/kvm_amd/parameters/sev_snp 2>/dev/null); [ "$v" = Y ] || [ "$v" = 1 ] && say "kvm_amd sev_snp=$v" || { say "FAIL: kvm_amd sev_snp=${v:-absent} (kernel too old, or kvm_amd not loaded)"; rc=1; }
  [ -c /dev/sev ] && say "/dev/sev present" || { say "FAIL: no /dev/sev (PSP device)"; rc=1; }
  dmesg 2>/dev/null | grep -i "sev-snp\|rmp" | head -3 | sed 's/^/   /'
  [ -r /dev/kvm ] && [ -w /dev/kvm ] && say "/dev/kvm usable" || { say "FAIL: /dev/kvm not usable by this user"; rc=1; }
  [ $rc = 0 ] && say "PREFLIGHT PASS" || say "PREFLIGHT FAIL -- cancel the rental"; exit $rc ;;
install)
  if qemu-system-x86_64 --version 2>/dev/null | grep -qE "version (9\.[1-9]|[1-9][0-9])"; then say "qemu $(qemu-system-x86_64 --version | head -1 | cut -d' ' -f4) is new enough"
  else
    say "building QEMU from source (x86_64-softmmu only)"
    sudo apt-get install -y -q git ninja-build python3-venv pkg-config libglib2.0-dev libpixman-1-dev flex bison python3-pip 2>/dev/null || sudo dnf install -y -q git ninja-build glib2-devel pixman-devel python3-pip
    git clone -q --depth 1 --branch v9.2.0 https://gitlab.com/qemu-project/qemu.git /tmp/qemu && cd /tmp/qemu && ./configure --target-list=x86_64-softmmu --enable-kvm --disable-docs > /dev/null && make -s -j"$(nproc)" > /dev/null && sudo make -s install && say "qemu $(qemu-system-x86_64 --version | head -1)"
  fi
  python3 -c "import cryptography" 2>/dev/null || pip install --user --break-system-packages -q cryptography
  say "INSTALL DONE" ;;
launch)
  [ -n "$KIT" ] || { echo "usage: snp-host-bringup.sh launch KIT [PORT]" >&2; exit 2; }
  [ -s "$KIT/OVMF.fd" ] && [ -s "$KIT/generic.efi" ] || { say "FAIL: KIT needs OVMF.fd and generic.efi"; exit 1; }
  FIFO=$KIT/fifo; rm -f $FIFO; mkfifo $FIFO
  # -kernel under the AmdSev OVMF with kernel-hashes=on puts the EFI's hash into
  # the launch digest (that is why no disk image is involved); no initrd yet.
  qemu-system-x86_64 -enable-kvm -machine q35,confidential-guest-support=sev0,memory-backend=ram0 \
    -object memory-backend-memfd,id=ram0,size=${MEM:-2048}M,share=true \
    -object sev-snp-guest,id=sev0,cbitpos=51,reduced-phys-bits=1,kernel-hashes=on \
    -cpu EPYC-v4 -smp 1 -m ${MEM:-2048} -bios "$KIT/OVMF.fd" -kernel "$KIT/generic.efi" \
    -nographic -no-reboot -device e1000,netdev=net0,romfile=,rombar=0 -netdev user,id=net0,hostfwd=tcp::${PORT}-:22 \
    < $FIFO > "$KIT/serial.txt" 2>&1 & echo $! > "$KIT/qemu.pid"; exec 3>$FIFO
  say "qemu pid $(cat "$KIT/qemu.pid"); serial $KIT/serial.txt"
  for i in $(seq 1 3000); do kill -0 "$(cat "$KIT/qemu.pid")" 2>/dev/null || { say "FAIL: qemu exited"; tail -5 "$KIT/serial.txt"; exit 1; }; tr -d '\r' < "$KIT/serial.txt" | tail -1 | grep -q '^> $' && break; sleep 0.2; done
  tr -d '\r' < "$KIT/serial.txt" | tail -1 | grep -q '^> $' || { say "FAIL: no REPL prompt"; tr -d '\r' < "$KIT/serial.txt" | tail -8; exit 1; }
  printf '(snp-attest-selftest)\r' >&3; sleep 5; tr -d '\r' < "$KIT/serial.txt" | grep -a "^SNP:" | sed 's/^/   /'
  printf '(ssh-boot)\r' >&3
  for i in $(seq 1 600); do grep -aq "NETUP" "$KIT/serial.txt" && break; sleep 0.2; done
  grep -aq NETUP "$KIT/serial.txt" && say "LAUNCHED: ssh -p $PORT test@127.0.0.1 (any password)" || { say "FAIL: ssh-boot did not reach NETUP"; exit 1; }
  echo "$FIFO" > "$KIT/fifo.path"; exec 3>&- ;;   # the fifo stays; qemu keeps running
attest)
  [ -n "$KIT" ] || { echo "usage: snp-host-bringup.sh attest KIT [PORT]" >&2; exit 2; }
  cd "$KIT"
  say "1. the attested client (handshake key == printed key, report_data == SHA-512(key), report)"
  bash test/snp/attested-ssh-client.sh 127.0.0.1 "$PORT" > attest-client.txt 2>&1; rc=$?
  cat attest-client.txt | sed 's/^/   /'
  [ $rc = 2 ] && { say "the guest answered (:NO-SNP): the C-bit was not found -- not an SNP launch"; exit 1; }
  [ $rc = 0 ] || { say "FAIL: client rc=$rc"; exit 1; }
  rep=$(grep -o '/tmp/attested-ssh[^ ]*/report.bin' attest-client.txt | head -1)
  say "2. VCEK from AMD KDS for this chip and TCB"
  mkdir -p kds && python3 test/snp/kds-fetch.py "$rep" kds | sed 's/^/   /' || { say "FAIL: KDS"; exit 1; }
  say "3. verify: signature chain to AMD's ARK, measurement == the DDC'd image under this firmware, host key binding"
  hk=$(grep -o 'handshake host key: [0-9a-f]*' attest-client.txt | cut -d' ' -f4)
  meas=$(python3 -c "import json;m=json.load(open('expected-measurement.json'));print(m.get('${VCPU_TYPE:-EPYC-v4}',''))")
  python3 test/snp/verify-report.py "$rep" --hostkey-hex "$hk" --vcek kds/vcek.pem --chain kds/chain.pem ${meas:+--measurement $meas} | sed 's/^/   /' && say "ATTESTED" || { say "FAIL: verification"; exit 1; } ;;
*) sed -n 2,16p "$0" ;;
esac
