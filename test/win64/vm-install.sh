#!/bin/sh
# vm-install.sh - install the Windows test VM: Server 2022 Core, unattended.
#
# Usage: test/win64/vm-install.sh SERVER_EVAL.iso DISK.qcow2
#
# SERVER_EVAL.iso is Microsoft's Windows Server 2022 evaluation ISO (180-day
# evaluation licence).  The answer file (vm/autounattend.xml) installs Server
# Core with no network, makes a boot-time task that runs RUN.CMD from any
# attached drive (test/win64/run-vm.sh), and shuts down at its first logon --
# which is how this script knows the install finished.  Needs KVM.
#
# While it runs: VM_QMP (default DISK.qmp) is a QMP socket, so
#   test/win64/vm-screen.py DISK.qmp /tmp/screen.png
# shows the screen.
set -eu
iso=${1:?usage: vm-install.sh SERVER_EVAL.iso DISK.qcow2}
disk=${2:?}
here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp "$here/vm/autounattend.xml" "$work/"
genisoimage -quiet -o "$work/answer.iso" -J -r -V ANSWER "$work/autounattend.xml"
qemu-img create -q -f qcow2 "$disk" 40G
echo "installing into $disk (typically 10-20 minutes with KVM)"
timeout ${VM_INSTALL_TIMEOUT:-5400} qemu-system-x86_64 \
  -enable-kvm -machine q35 -cpu host -smp 4 -m 4096 \
  -drive file="$disk",if=none,id=hd,format=qcow2 \
  -drive file="$iso",if=none,id=cd0,media=cdrom,readonly=on \
  -drive file="$work/answer.iso",if=none,id=cd1,media=cdrom,readonly=on \
  -device ahci,id=ahci \
  -device ide-hd,drive=hd,bus=ahci.0 \
  -device ide-cd,drive=cd0,bus=ahci.1 \
  -device ide-cd,drive=cd1,bus=ahci.2 \
  -boot once=d -nic none -display none -vga std \
  -qmp unix:"${VM_QMP:-$disk.qmp}",server=on,wait=off
echo "installed: $disk"
