#!/bin/bash
# run-fixpoint.sh - Fixpoint of Theseus: fixed-point proof
#
# Proves the MVM compiler is a fixed point across architectures:
#   SBCL → Gen0(x64) → Gen1(aarch64) → Gen2(x64) → Gen3(aarch64)
#   SHA256(Gen1) == SHA256(Gen3) proves the cross-arch compiler is a fixed point
#
# Gen0 is the seed (built by SBCL, may differ from bare-metal).
# Gen1 and Gen2 are the first self-hosted generation pair.
# Gen3 must equal Gen1 to prove convergence.
#
# Usage: ./scripts/run-fixpoint.sh

set -e
cd "$(dirname "$0")/.."

TIMEOUT=${FIXPOINT_TIMEOUT:-5400}   # Gen1 (aarch64 under TCG) takes ~45 min
IMAGE_SIZE=4194368
GEN0=/tmp/fixpoint-gen0.elf
GEN1=/tmp/fixpoint-gen1.bin
GEN2=/tmp/fixpoint-gen2.bin
GEN3=/tmp/fixpoint-gen3.bin

cleanup() {
  pkill -f "qemu-system.*fixpoint" 2>/dev/null || true
  rm -f /tmp/qmp-fixpoint.sock
}
trap cleanup EXIT

# Wait for "DONE" marker in QEMU output, then extract image via QMP pmemsave
extract_image() {
  local kernel=$1 output=$2 label=$3 qemu_cmd=$4 pmemsave_addr=$5
  local sock=/tmp/qmp-fixpoint.sock
  local logfile=/tmp/fixpoint-${label}.log

  rm -f "$sock" "$output" "$logfile"

  # shellcheck disable=SC2086
  $qemu_cmd -kernel "$kernel" -nographic \
    -qmp "unix:$sock,server=on,wait=off" > "$logfile" 2>&1 &
  local pid=$!

  # Wait for DONE marker (kernel finished building image)
  local elapsed=0
  while [ $elapsed -lt "$TIMEOUT" ]; do
    if grep -q "DONE" "$logfile" 2>/dev/null; then
      break
    fi
    if ! kill -0 "$pid" 2>/dev/null; then
      echo "  FAIL: $label - QEMU exited early"
      cat "$logfile"
      exit 1
    fi
    sleep 1
    elapsed=$((elapsed + 1))
  done

  if ! grep -q "DONE" "$logfile" 2>/dev/null; then
    echo "  FAIL: $label - timeout waiting for DONE"
    tail -20 "$logfile"
    kill "$pid" 2>/dev/null; wait "$pid" 2>/dev/null || true
    exit 1
  fi

  # Extract image via QMP pmemsave
  # The assembler prints A1=<image bytes>; size the memory save from it (the
  # fixed 4 MB constants predate the CL-runtime fixpoint, whose images are ~49 MB).
  local a1; a1=$(grep -ao "[AX]1=[0-9]*" "$logfile" | tail -1 | cut -d= -f2)   # A1= aarch64 image, X1= x64 image
  local save_size=${a1:-$IMAGE_SIZE}
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
    'val': $pmemsave_addr, 'size': $save_size, 'filename': '$output'
}}).encode() + b'\n')
recv()
time.sleep(1)
s.sendall(json.dumps({'execute': 'quit'}).encode() + b'\n')
s.close()
" 2>/dev/null

  sleep 1
  kill "$pid" 2>/dev/null; wait "$pid" 2>/dev/null || true

  if [ ! -f "$output" ]; then
    echo "  FAIL: $label - output file not created"
    exit 1
  fi
  echo "  $label: $(sha256sum "$output" | cut -d' ' -f1)"
}

echo "=== Fixpoint of Theseus ==="
echo ""

# Step 0: Build Gen0 from SBCL (seed kernel)
echo "Step 0: SBCL → Gen0(x64)"
sbcl --dynamic-space-size 12288 --script mvm/build-fixpoint.lisp 2>&1 | grep -E "bytecode:|Functions:|native code:|written"
echo ""

# Step 1: Gen0(x64) → Gen1(aarch64)
# Image buffer at VA 0x08000000 = PA 0x08000000 (x64 identity-mapped)
echo "Step 1: Gen0(x64) → Gen1(aarch64)"
extract_image "$GEN0" "$GEN1" "Gen1" \
  "qemu-system-x86_64 -m 1024 -no-reboot" \
  134217728  # 0x08000000
echo ""

# Step 2: Gen1(aarch64) → Gen2(x64)
# AArch64 MMU: VA = PA - 0x40000000, so VA 0x08000000 → PA 0x48000000
echo "Step 2: Gen1(aarch64) → Gen2(x64)"
extract_image "$GEN1" "$GEN2" "Gen2" \
  "qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 1024 -semihosting" \
  1207959552  # 0x48000000
echo ""

# Step 3: Gen2(x64) → Gen3(aarch64)
# Same as Step 1 but from Gen2 instead of Gen0
echo "Step 3: Gen2(x64) → Gen3(aarch64)"
extract_image "$GEN2" "$GEN3" "Gen3" \
  "qemu-system-x86_64 -m 1024 -no-reboot" \
  134217728  # 0x08000000
echo ""

# Compare Gen1 and Gen3 (same arch, same generation step)
echo "=== Result ==="
hash1=$(sha256sum "$GEN1" | cut -d' ' -f1)
hash3=$(sha256sum "$GEN3" | cut -d' ' -f1)

if [ "$hash1" = "$hash3" ]; then
  echo "PASS: SHA256(Gen1) == SHA256(Gen3)"
  echo "  Cross-architecture fixpoint proven."
  echo "  $hash1"
else
  echo "FAIL: SHA256(Gen1) != SHA256(Gen3)"
  echo "  Gen1: $hash1"
  echo "  Gen3: $hash3"
  exit 1
fi
