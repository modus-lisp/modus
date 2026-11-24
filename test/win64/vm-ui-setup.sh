#!/bin/sh
# vm-ui-setup.sh - make a disk whose guest logs on to a desktop session by itself.
#
# Usage: test/win64/vm-ui-setup.sh BASE.qcow2 UI.qcow2
#
# UI.qcow2 is a copy-on-write layer over BASE.qcow2 (made by vm-install.sh, never
# written).  Booted once here with a RUN.CMD that sets automatic logon as
# Administrator (the answer file's password) at every boot, and adds a logon task
# that runs C:\modus-ui.cmd in that session: it finds UI.CMD on an attached drive,
# runs it, and powers off.  With the Desktop Experience (vm-install.sh VM_IMAGE=2) it also keeps
# Server Manager from opening at logon and starts Windows Audio, disabled on a server by default
# (on Server Core neither exists, and those lines fail harmlessly).  Then `VM_UI=1 test/win64/run-vm.sh UI.qcow2 ...' puts a
# program's windows on the guest's screen, where vm-screen.py and QMP input reach
# them.  The boot-time RUN.CMD task stays: a disk with RUN.CMD still runs it.
set -eu
base=${1:?usage: vm-ui-setup.sh BASE.qcow2 UI.qcow2}
ui=${2:?}
here=$(cd "$(dirname "$0")" && pwd)
base=$(cd "$(dirname "$base")" && pwd)/$(basename "$base")
qemu-img create -q -f qcow2 -b "$base" -F qcow2 "$ui"
work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
wl='HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon'
{ printf '%s\r\n' '@echo off' \
    "reg add \"$wl\" /v AutoAdminLogon /t REG_SZ /d 1 /f > setup.log 2>&1" \
    "reg add \"$wl\" /v DefaultUserName /t REG_SZ /d Administrator /f >> setup.log 2>&1" \
    "reg add \"$wl\" /v DefaultPassword /t REG_SZ /d Modus-Test-2026 /f >> setup.log 2>&1" \
    "reg add \"$wl\" /v DefaultDomainName /t REG_SZ /d . /f >> setup.log 2>&1" \
    "reg delete \"$wl\" /v AutoLogonCount /f >> setup.log 2>&1" \
    'echo @echo off> C:\modus-ui.cmd' \
    'echo for %%%%d in (D E F G H I J K L M N O P Q R S T U V W X Y Z) do if exist %%%%d:\UI.CMD (cd /d %%%%d:\ ^& call %%%%d:\UI.CMD ^& shutdown /s /t 0 /f)>> C:\modus-ui.cmd' \
    'type C:\modus-ui.cmd >> setup.log' \
    'reg add "HKLM\SOFTWARE\Microsoft\ServerManager" /v DoNotOpenServerManagerAtLogon /t REG_DWORD /d 1 /f >> setup.log 2>&1' \
    'schtasks /change /tn "Microsoft\Windows\Server Manager\ServerManager" /disable >> setup.log 2>&1' \
    'sc config AudioEndpointBuilder start= auto >> setup.log 2>&1' \
    'sc config Audiosrv start= auto >> setup.log 2>&1' \
    'schtasks /create /tn modus-ui /sc onlogon /ru Administrator /rp Modus-Test-2026 /it /rl highest /tr C:\modus-ui.cmd /f >> setup.log 2>&1' \
    'echo done>> setup.log'; } > "$work/RUN.CMD"
disk="$work/data.img"
truncate -s 64M "$disk"
python3 - "$disk" <<'PY'
import struct, sys
with open(sys.argv[1], "r+b") as f:
    f.seek(440); f.write(struct.pack("<I", 0x4D4F4455))
    f.seek(446); f.write(struct.pack("<B3sB3sII", 0, b"\xfe\xff\xff", 0x0C, b"\xfe\xff\xff", 2048, 64*2048-2048))
    f.seek(510); f.write(b"\x55\xaa")
PY
mformat -i "$disk@@1M" -F -T $((64*2048-2048)) -h 64 -s 32 -H 2048 -v SETUP ::
mcopy -i "$disk@@1M" "$work/RUN.CMD" ::
timeout 900 qemu-system-x86_64 -enable-kvm -machine q35 -cpu host -smp 4 -m 4096 \
  -drive file="$ui",if=none,id=hd,format=qcow2 -drive file="$disk",if=none,id=data,format=raw \
  -device ahci,id=ahci -device ide-hd,drive=hd,bus=ahci.0 -device ide-hd,drive=data,bus=ahci.1 \
  -nic none -display none -vga std
mcopy -n -i "$disk@@1M" ::setup.log "$work/setup.log" && tr -d '\r' < "$work/setup.log"
