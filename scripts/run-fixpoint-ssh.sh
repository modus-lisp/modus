#!/bin/bash
# run-fixpoint-ssh.sh - Multi-architecture fixpoint chain with SSH final stage
#
# Chains multiple fixpoint generations across architectures, then boots the
# final generation as an SSH server. Supports remote QEMU execution hosts
# for cross-architecture steps.
#
# Usage: ./scripts/run-fixpoint-ssh.sh [--host arch=user@host ...] arch1 arch2 [arch3 ...]
#   First arch must be x64 (SBCL builds Gen0 for x64).
#   Last arch boots as SSH server.
#   Minimum 2 architectures.
#
# Remote hosts: --host arch=user@host routes QEMU execution for that
#   architecture to the specified SSH host. The host must have the
#   relevant qemu-system-* binary and python3 installed.
#
# Examples:
#   # Local only (default)
#   ./scripts/run-fixpoint-ssh.sh x64 aarch64
#
#   # Route ARM steps to Pi 5
#   ./scripts/run-fixpoint-ssh.sh --host aarch64=modus@modus-pi --host arm32=modus@modus-pi x64 aarch64
#
#   # Route x86 steps to a remote x86 box (from ARM host)
#   ./scripts/run-fixpoint-ssh.sh --host x64=user@x86box --host i386=user@x86box x64 aarch64 x64
#
# Environment:
#   FIXPOINT_TIMEOUT  - seconds per generation (default 240)
#   SSH_PORT          - local SSH port for final server (default 2223)

set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$(dirname "$0")/.."

TIMEOUT=${FIXPOINT_TIMEOUT:-240}
SSH_PORT="${SSH_PORT:-2223}"

# ============================================================
# Parse --host flags and architecture list
# ============================================================

declare -A REMOTE_HOSTS
ARCHS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --host)
      # --host arch=user@host
      shift
      if [[ "$1" =~ ^([a-z0-9]+)=(.+)$ ]]; then
        REMOTE_HOSTS["${BASH_REMATCH[1]}"]="${BASH_REMATCH[2]}"
      else
        echo "Error: --host requires arch=user@host format (got '$1')"
        exit 1
      fi
      shift
      ;;
    --host=*)
      # --host=arch=user@host
      arg="${1#--host=}"
      if [[ "$arg" =~ ^([a-z0-9]+)=(.+)$ ]]; then
        REMOTE_HOSTS["${BASH_REMATCH[1]}"]="${BASH_REMATCH[2]}"
      else
        echo "Error: --host requires arch=user@host format (got '$arg')"
        exit 1
      fi
      shift
      ;;
    *)
      ARCHS+=("$1")
      shift
      ;;
  esac
done

# Validate arguments
if [[ ${#ARCHS[@]} -lt 2 ]]; then
  echo "Usage: $0 [--host arch=user@host ...] arch1 arch2 [arch3 ...]"
  echo "  Architectures: x64, aarch64, i386, arm32"
  echo "  First must be x64. Last boots as SSH server."
  echo "  --host routes QEMU execution to a remote SSH host."
  exit 1
fi

if [[ "${ARCHS[0]}" != "x64" ]]; then
  echo "Error: first architecture must be x64 (SBCL builds Gen0 for x64)"
  exit 1
fi

for arch in "${ARCHS[@]}"; do
  if [[ "$arch" != "x64" && "$arch" != "aarch64" && "$arch" != "i386" && "$arch" != "arm32" ]]; then
    echo "Error: unknown architecture '$arch' (valid: x64, aarch64, i386, arm32)"
    exit 1
  fi
done

# ============================================================
# Architecture helpers
# ============================================================

arch_id() {
  case "$1" in
    x64)     echo 0 ;;
    aarch64) echo 1 ;;
    i386)    echo 2 ;;
    arm32)   echo 3 ;;
  esac
}

extract_pa() {
  case "$1" in
    x64)     echo 134217728  ;;  # 0x08000000
    aarch64) echo 1207959552 ;;  # 0x48000000
    i386)    echo 134217728  ;;
    arm32)   echo 134217728  ;;
  esac
}

metadata_file_offset() {
  case "$1" in
    x64)     echo 3932160  ;;  # 0x3C0000
    aarch64) echo 4456448  ;;  # 0x440000
    i386)    echo 3932160  ;;  # 0x3C0000
    arm32)   echo 4915200  ;;  # 0x4B0000
  esac
}

image_extract_size() {
  case "$1" in
    x64)     echo 3932224  ;;  # 0x3C0000 + 64
    aarch64) echo 4456512  ;;  # 0x440000 + 64
    i386)    echo 3932224  ;;  # 0x3C0000 + 64
    arm32)   echo 4915264  ;;  # 0x4B0000 + 64
  esac
}

qemu_cmd() {
  # Return the QEMU command line for an architecture (no kernel argument)
  case "$1" in
    x64)     echo "qemu-system-x86_64 -m 1024 -nographic -no-reboot" ;;
    aarch64) echo "qemu-system-aarch64 -M virt -cpu cortex-a53 -m 1024 -nographic" ;;
    i386)    echo "qemu-system-i386 -m 512 -nographic -no-reboot" ;;
    arm32)   echo "qemu-system-arm -M raspi2b -m 1G -nographic" ;;
  esac
}

qemu_nic() {
  # Return NIC device flags for final SSH server
  case "$1" in
    i386)    echo "-device ne2k_isa,netdev=net0,iobase=0x300,irq=9" ;;
    arm32)   echo "-device usb-net,netdev=net0" ;;
    *)       echo "-device e1000,netdev=net0,romfile=,rombar=0" ;;
  esac
}

patch_u32_le() {
  local file=$1 offset=$2 value=$3
  local b0=$((value & 0xFF))
  local b1=$(( (value >> 8) & 0xFF ))
  local b2=$(( (value >> 16) & 0xFF ))
  local b3=$(( (value >> 24) & 0xFF ))
  printf "\\x$(printf '%02x' $b0)\\x$(printf '%02x' $b1)\\x$(printf '%02x' $b2)\\x$(printf '%02x' $b3)" | \
    dd of="$file" bs=1 seek="$offset" conv=notrunc 2>/dev/null
}

NO_THP="$SCRIPT_DIR/no-thp-exec"

# ============================================================
# QEMU execution: local vs remote
# ============================================================

# Run a cross-compile step locally: launch QEMU, wait for DONE, extract via QMP.
# Args: arch kernel_file output_file target_arch
run_step_local() {
  local arch=$1 kernel=$2 output=$3 target=$4
  local sock="/tmp/qmp-fixpoint-ssh.sock"
  local serial="/tmp/fixpoint-ssh-serial.txt"
  local qemu
  qemu=$(qemu_cmd "$arch")

  rm -f "$sock" "$output" "$serial"

  $NO_THP $qemu -kernel "$kernel" \
    -qmp "unix:$sock,server=on,wait=off" \
    -display none -serial "file:$serial" &
  local pid=$!

  local elapsed=0
  while ! grep -q "DONE" "$serial" 2>/dev/null; do
    sleep 1
    elapsed=$((elapsed + 1))
    if [[ $elapsed -ge $TIMEOUT ]]; then
      echo "  TIMEOUT after ${TIMEOUT}s"
      kill "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
      echo "  Serial output:"
      cat "$serial" 2>/dev/null || true
      return 1
    fi
  done
  echo "  Completed in ${elapsed}s"

  # Extract via QMP pmemsave
  local addr size
  addr=$(extract_pa "$arch")
  size=$(image_extract_size "$target")
  python3 -c "
import socket, json, time
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.connect('$sock')
def recv():
    d = b''
    while True:
        d += s.recv(4096)
        try: return json.loads(d)
        except: continue
recv()
s.sendall(json.dumps({'execute': 'qmp_capabilities'}).encode() + b'\n')
recv()
s.sendall(json.dumps({'execute': 'pmemsave', 'arguments': {
    'val': $addr, 'size': $size, 'filename': '$output'
}}).encode() + b'\n')
recv()
time.sleep(1)
s.sendall(json.dumps({'execute': 'quit'}).encode() + b'\n')
s.close()
" 2>/dev/null

  sleep 1
  kill "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
  cp -f "$serial" /tmp/fixpoint-ssh-serial-saved.txt 2>/dev/null || true
  rm -f "$serial"

  [[ -f "$output" ]]
}

# Run a cross-compile step on a remote host via SSH.
# Copies the kernel image over, runs QEMU remotely, extracts and copies back.
# Args: host arch kernel_file output_file target_arch
run_step_remote() {
  local host=$1 arch=$2 kernel=$3 output=$4 target=$5
  local remote_kernel="/tmp/fixpoint-remote-kernel.bin"
  local remote_output="/tmp/fixpoint-remote-output.bin"
  local remote_serial="/tmp/fixpoint-remote-serial.txt"
  local remote_sock="/tmp/qmp-fixpoint-remote.sock"
  local qemu
  qemu=$(qemu_cmd "$arch")
  local addr size
  addr=$(extract_pa "$arch")
  size=$(image_extract_size "$target")

  echo "  [remote: $host]"

  # Copy kernel to remote
  scp -q "$kernel" "$host:$remote_kernel"

  # Run QEMU on remote, wait for DONE, extract, then we pull the result
  ssh "$host" bash -s <<REMOTE_EOF
set -e
rm -f "$remote_sock" "$remote_output" "$remote_serial"

$qemu -kernel "$remote_kernel" \
  -qmp "unix:$remote_sock,server=on,wait=off" \
  -display none -serial "file:$remote_serial" &
PID=\$!

ELAPSED=0
while ! grep -q "DONE" "$remote_serial" 2>/dev/null; do
  sleep 1
  ELAPSED=\$((ELAPSED + 1))
  if [[ \$ELAPSED -ge $TIMEOUT ]]; then
    echo "  TIMEOUT after ${TIMEOUT}s"
    kill \$PID 2>/dev/null || true
    wait \$PID 2>/dev/null || true
    cat "$remote_serial" 2>/dev/null || true
    exit 1
  fi
done
echo "  Completed in \${ELAPSED}s"

python3 -c "
import socket, json, time
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.connect('$remote_sock')
def recv():
    d = b''
    while True:
        d += s.recv(4096)
        try: return json.loads(d)
        except: continue
recv()
s.sendall(json.dumps({'execute': 'qmp_capabilities'}).encode() + b'\n')
recv()
s.sendall(json.dumps({'execute': 'pmemsave', 'arguments': {
    'val': $addr, 'size': $size, 'filename': '$remote_output'
}}).encode() + b'\n')
recv()
time.sleep(1)
s.sendall(json.dumps({'execute': 'quit'}).encode() + b'\n')
s.close()
" 2>/dev/null

sleep 1
kill \$PID 2>/dev/null || true
wait \$PID 2>/dev/null || true
rm -f "$remote_serial" "$remote_sock" "$remote_kernel"

[[ -f "$remote_output" ]] || { echo "  FAIL: image not extracted on remote"; exit 1; }
echo "  Extracted: \$(wc -c < "$remote_output") bytes"
REMOTE_EOF

  # Copy result back
  scp -q "$host:$remote_output" "$output"
  ssh "$host" "rm -f $remote_output" 2>/dev/null || true

  [[ -f "$output" ]]
}

# Run a cross-compile step, dispatching to local or remote as configured.
# Args: arch kernel_file output_file target_arch
run_step() {
  local arch=$1 kernel=$2 output=$3 target=$4
  local host="${REMOTE_HOSTS[$arch]:-}"

  if [[ -n "$host" ]]; then
    run_step_remote "$host" "$arch" "$kernel" "$output" "$target"
  else
    run_step_local "$arch" "$kernel" "$output" "$target"
  fi
}

# Boot the final generation as an SSH server (local or remote with port forwarding).
# Args: arch kernel_file
run_final_ssh() {
  local arch=$1 kernel=$2
  local host="${REMOTE_HOSTS[$arch]:-}"
  local nic
  nic=$(qemu_nic "$arch")
  local qemu
  qemu=$(qemu_cmd "$arch")

  if [[ -n "$host" ]]; then
    echo "  [remote: $host, forwarding port $SSH_PORT]"
    local remote_kernel="/tmp/fixpoint-remote-ssh.bin"
    scp -q "$kernel" "$host:$remote_kernel"

    # SSH with local port forwarding: local:SSH_PORT → remote QEMU's port
    ssh -L "${SSH_PORT}:localhost:${SSH_PORT}" "$host" \
      "$qemu -kernel $remote_kernel $nic -netdev 'user,id=net0,hostfwd=tcp::${SSH_PORT}-:22'"
  else
    $NO_THP $qemu -kernel "$kernel" \
      $nic -netdev "user,id=net0,hostfwd=tcp::${SSH_PORT}-:22"
  fi
}

cleanup() {
  pkill -f "qemu-system.*fixpoint-gen" 2>/dev/null || true
  pkill -f "qemu-system.*fixpoint-remote" 2>/dev/null || true
  cp -f /tmp/fixpoint-ssh-serial.txt /tmp/fixpoint-ssh-serial-saved.txt 2>/dev/null || true
  rm -f /tmp/qmp-fixpoint-ssh.sock /tmp/fixpoint-ssh-serial.txt
  # Clean up remote hosts
  for host in "${REMOTE_HOSTS[@]}"; do
    ssh "$host" 'pkill -f "qemu-system.*fixpoint-remote" 2>/dev/null; rm -f /tmp/fixpoint-remote-*.bin /tmp/fixpoint-remote-serial.txt /tmp/qmp-fixpoint-remote.sock' 2>/dev/null || true
  done
}
trap cleanup EXIT

# ============================================================
# Step 0: Build Gen0 from SBCL
# ============================================================

N=${#ARCHS[@]}
LAST=$((N - 1))

echo "=== Fixpoint SSH Chain: ${ARCHS[*]} ==="
echo "  Generations: $N (Gen0..Gen$LAST)"
echo "  SSH on: Gen$LAST (${ARCHS[$LAST]})"
for arch in "${!REMOTE_HOSTS[@]}"; do
  echo "  Remote: $arch → ${REMOTE_HOSTS[$arch]}"
done
echo ""
echo "Step 0: SBCL → Gen0 (x64, SSH mode)"
sbcl --script mvm/build-fixpoint.lisp 2>&1 | grep -E "bytecode:|Functions:|native code:|written|SSH mode|Networking"
echo ""

GEN=/tmp/fixpoint-gen0.elf

# ============================================================
# Chain: Gen0..Gen(N-2) cross-compile
# ============================================================

for ((i = 0; i < LAST; i++)); do
  NEXT=$((i + 1))
  ARCH="${ARCHS[$i]}"
  TARGET="${ARCHS[$NEXT]}"
  NEXT_IMG="/tmp/fixpoint-gen${NEXT}.bin"

  echo "Step $((i + 1)): Gen$i ($ARCH) → Gen$NEXT ($TARGET)"

  # Patch target-architecture and mode=0 (cross-compile) in current image
  MD_OFF=$(metadata_file_offset "$ARCH")
  TARGET_ID=$(arch_id "$TARGET")
  patch_u32_le "$GEN" $((MD_OFF + 52)) "$TARGET_ID"
  patch_u32_le "$GEN" $((MD_OFF + 56)) 0

  if ! run_step "$ARCH" "$GEN" "$NEXT_IMG" "$TARGET"; then
    echo "  FAIL: Gen$NEXT not extracted"
    exit 1
  fi
  echo "  Gen$NEXT extracted: $(wc -c < "$NEXT_IMG") bytes ($TARGET)"
  echo ""

  GEN="$NEXT_IMG"
done

# ============================================================
# Boot final generation as SSH server
# ============================================================

FINAL_ARCH="${ARCHS[$LAST]}"
echo "=== Booting Gen$LAST ($FINAL_ARCH) as SSH server ==="

# Patch mode=1 (SSH server)
MD_OFF=$(metadata_file_offset "$FINAL_ARCH")
patch_u32_le "$GEN" $((MD_OFF + 56)) 1

echo ""
echo "SSH available at: ssh -p $SSH_PORT test@localhost"
echo "Press Ctrl-C to stop."
echo ""

run_final_ssh "$FINAL_ARCH" "$GEN"
