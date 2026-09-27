#!/bin/bash
# run-fixpoint-i386.sh - Fixpoint of Theseus: 3-architecture proof
#
# Proves the MVM compiler is a fixed point across three architectures:
#   x64, AArch64, and i386 (32-bit x86)
#
# Chain:
#   SBCL → Gen0(x64) → Gen1(aarch64) → Gen2(x64) → Gen3(aarch64)
#   SHA256(Gen1) == SHA256(Gen3) proves x64↔aarch64 fixpoint
#
#   Gen0(x64) → i386-A, Gen1(aarch64) → i386-B
#   SHA256(i386-A) == SHA256(i386-B) proves i386 translator determinism
#
#   i386-A → AArch64-E
#   SHA256(AArch64-E) == SHA256(Gen1) proves i386→AArch64 cross-compilation
#
# Usage: ./scripts/run-fixpoint-i386.sh

set -e
cd "$(dirname "$0")/.."

TIMEOUT=${FIXPOINT_TIMEOUT:-120}
X64_IMAGE_SIZE=3670080    # 0x380000 + 64
A64_IMAGE_SIZE=4194368    # 0x400000 + 64
I386_IMAGE_SIZE=3670080   # 0x380000 + 64 (same layout as x64)
GEN0=/tmp/fixpoint-gen0.elf
GEN1=/tmp/fixpoint-gen1.bin
GEN2=/tmp/fixpoint-gen2.bin
GEN3=/tmp/fixpoint-gen3.bin
I386_A=/tmp/fixpoint-i386-from-x64.bin
I386_B=/tmp/fixpoint-i386-from-a64.bin

cleanup() {
  pkill -f "qemu-system.*fixpoint" 2>/dev/null || true
  rm -f /tmp/qmp-fixpoint.sock
}
trap cleanup EXIT

# Wait for "DONE" marker in QEMU output, then extract image via QMP pmemsave
extract_image() {
  local kernel=$1 output=$2 label=$3 qemu_cmd=$4 pmemsave_addr=$5 image_size=$6
  local sock=/tmp/qmp-fixpoint.sock
  local logfile=/tmp/fixpoint-${label}.log

  rm -f "$sock" "$output" "$logfile"

  # shellcheck disable=SC2086
  $qemu_cmd -kernel "$kernel" -nographic \
    -qmp "unix:$sock,server=on,wait=off" > "$logfile" 2>&1 &
  local pid=$!

  # Wait for DONE marker
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
    'val': $pmemsave_addr, 'size': $image_size, 'filename': '$output'
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

# Patch target-architecture byte in image metadata
# Args: image_file metadata_offset target_value
patch_target() {
  local img=$1 md_offset=$2 target=$3
  printf "\\x$(printf '%02x' "$target")" | \
    dd of="$img" bs=1 seek=$((md_offset + 52)) conv=notrunc 2>/dev/null
}

echo "=== Fixpoint of Theseus (3-Architecture) ==="
echo ""

# Step 0: Build Gen0 from SBCL (seed kernel)
echo "Step 0: SBCL → Gen0(x64)"
sbcl --script mvm/build-fixpoint.lisp 2>&1 | grep -E "bytecode:|Functions:|native code:|written|i386"
echo ""

# Step 1: Gen0(x64) → Gen1(aarch64) [target=1, default]
echo "Step 1: Gen0(x64) → Gen1(aarch64)"
extract_image "$GEN0" "$GEN1" "Gen1" \
  "qemu-system-x86_64 -m 1024 -no-reboot" \
  134217728 $A64_IMAGE_SIZE  # 0x08000000
echo ""

# Step 2: Gen1(aarch64) → Gen2(x64) [target=0, default]
echo "Step 2: Gen1(aarch64) → Gen2(x64)"
extract_image "$GEN1" "$GEN2" "Gen2" \
  "qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 1024 -semihosting" \
  1207959552 $X64_IMAGE_SIZE  # 0x48000000
echo ""

# Step 3: Gen2(x64) → Gen3(aarch64) [target=1, default]
echo "Step 3: Gen2(x64) → Gen3(aarch64)"
extract_image "$GEN2" "$GEN3" "Gen3" \
  "qemu-system-x86_64 -m 1024 -no-reboot" \
  134217728 $A64_IMAGE_SIZE
echo ""

# Step 4: Gen0(x64) → i386 [target=2]
echo "Step 4: Gen0(x64) → i386-A"
GEN0_I386=/tmp/fixpoint-gen0-i386.elf
cp "$GEN0" "$GEN0_I386"
patch_target "$GEN0_I386" $((0x380000)) 2
extract_image "$GEN0_I386" "$I386_A" "i386-A" \
  "qemu-system-x86_64 -m 1024 -no-reboot" \
  134217728 $I386_IMAGE_SIZE
echo ""

# Step 5: Gen1(aarch64) → i386 [target=2]
echo "Step 5: Gen1(aarch64) → i386-B"
GEN1_I386=/tmp/fixpoint-gen1-i386.bin
cp "$GEN1" "$GEN1_I386"
patch_target "$GEN1_I386" $((0x300000)) 2
extract_image "$GEN1_I386" "$I386_B" "i386-B" \
  "qemu-system-aarch64 -machine virt -cpu cortex-a57 -m 1024 -semihosting" \
  1207959552 $I386_IMAGE_SIZE
echo ""

# Step 6: i386-A → i386-C [i386 self-hosting: target=2]
echo "Step 6: i386-A → i386-C (i386 self-hosting)"
I386_C=/tmp/fixpoint-i386-self.bin
I386_A_SELF=/tmp/fixpoint-i386a-self.bin
cp "$I386_A" "$I386_A_SELF"
patch_target "$I386_A_SELF" $((0x380000)) 2
extract_image "$I386_A_SELF" "$I386_C" "i386-C" \
  "qemu-system-i386 -m 256 -no-reboot" \
  134217728 $I386_IMAGE_SIZE
echo ""

# Step 7: i386-A → x64-D [i386→x64 cross-compilation: target=0]
echo "Step 7: i386-A → x64-D (i386→x64 cross-compilation)"
X64_D=/tmp/fixpoint-x64-from-i386.bin
I386_A_X64=/tmp/fixpoint-i386a-x64.bin
cp "$I386_A" "$I386_A_X64"
patch_target "$I386_A_X64" $((0x380000)) 0
extract_image "$I386_A_X64" "$X64_D" "x64-D" \
  "qemu-system-i386 -m 256 -no-reboot" \
  134217728 $X64_IMAGE_SIZE
echo ""

# Step 8: i386-A → AArch64-E (i386→AArch64 cross-compilation: target=1)
echo "Step 8: i386-A → AArch64-E (i386→AArch64 cross-compilation)"
A64_E=/tmp/fixpoint-a64-from-i386.bin
I386_A_A64=/tmp/fixpoint-i386a-a64.bin
cp "$I386_A" "$I386_A_A64"
patch_target "$I386_A_A64" $((0x380000)) 1
extract_image "$I386_A_A64" "$A64_E" "AArch64-E" \
  "qemu-system-i386 -m 256 -no-reboot" \
  134217728 $A64_IMAGE_SIZE
echo ""

# Compare results
echo "=== Results ==="

# x64 ↔ aarch64 fixpoint
hash1=$(sha256sum "$GEN1" | cut -d' ' -f1)
hash3=$(sha256sum "$GEN3" | cut -d' ' -f1)

if [ "$hash1" = "$hash3" ]; then
  echo "PASS: SHA256(Gen1) == SHA256(Gen3)  [x64↔aarch64 fixpoint]"
  echo "  $hash1"
else
  echo "FAIL: SHA256(Gen1) != SHA256(Gen3)  [x64↔aarch64]"
  echo "  Gen1: $hash1"
  echo "  Gen3: $hash3"
  exit 1
fi

# i386 translator determinism (x64 vs aarch64 host)
hash_i386a=$(sha256sum "$I386_A" | cut -d' ' -f1)
hash_i386b=$(sha256sum "$I386_B" | cut -d' ' -f1)

if [ "$hash_i386a" = "$hash_i386b" ]; then
  echo "PASS: SHA256(i386-A) == SHA256(i386-B)  [i386 translator fixpoint]"
  echo "  $hash_i386a"
else
  echo "FAIL: SHA256(i386-A) != SHA256(i386-B)  [i386 translator]"
  echo "  i386-A (from x64):    $hash_i386a"
  echo "  i386-B (from aarch64): $hash_i386b"
  exit 1
fi

# i386 self-hosting fixpoint (i386 host produces same as x64 host)
hash_i386c=$(sha256sum "$I386_C" | cut -d' ' -f1)

if [ "$hash_i386a" = "$hash_i386c" ]; then
  echo "PASS: SHA256(i386-A) == SHA256(i386-C)  [i386 self-hosting fixpoint]"
  echo "  $hash_i386c"
else
  echo "FAIL: SHA256(i386-A) != SHA256(i386-C)  [i386 self-hosting]"
  echo "  i386-A (from x64):  $hash_i386a"
  echo "  i386-C (from i386): $hash_i386c"
  exit 1
fi

# i386→x64 cross-compilation (i386 host produces same x64 image as aarch64 host)
hash_x64d=$(sha256sum "$X64_D" | cut -d' ' -f1)
hash_gen2=$(sha256sum "$GEN2" | cut -d' ' -f1)

if [ "$hash_x64d" = "$hash_gen2" ]; then
  echo "PASS: SHA256(x64-D) == SHA256(Gen2)  [i386→x64 cross-compilation fixpoint]"
  echo "  $hash_x64d"
else
  echo "FAIL: SHA256(x64-D) != SHA256(Gen2)  [i386→x64 cross-compilation]"
  echo "  x64-D (from i386): $hash_x64d"
  echo "  Gen2 (from aarch64): $hash_gen2"
  exit 1
fi

# i386→AArch64 cross-compilation (i386 host produces same AArch64 image as x64 host)
hash_a64e=$(sha256sum "$A64_E" | cut -d' ' -f1)

if [ "$hash_a64e" = "$hash1" ]; then
  echo "PASS: SHA256(AArch64-E) == SHA256(Gen1)  [i386→AArch64 cross-compilation fixpoint]"
  echo "  $hash_a64e"
else
  echo "FAIL: SHA256(AArch64-E) != SHA256(Gen1)  [i386→AArch64 cross-compilation]"
  echo "  AArch64-E (from i386): $hash_a64e"
  echo "  Gen1 (from x64):      $hash1"
  exit 1
fi

echo ""
echo "Cross-architecture fixpoint proven across x64, AArch64, and i386."
echo "Including: i386 self-hosting + i386→x64 + i386→AArch64 cross-compilation."
