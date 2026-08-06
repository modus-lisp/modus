#!/bin/bash
# run.sh — ONE entry point for every Modus run configuration.
#
# Replaces ~14 near-identical run-<arch>-<payload>.sh launchers.  They were
# copy-paste siblings that had DRIFTED apart, and the drift was the real cost:
#   - run-x64-ssh rebuilt only when the build script was newer; run-aarch64-ssh
#     rebuilt unconditionally on EVERY invocation (minutes per run)
#   - x64/i386 wrapped QEMU in no-thp-exec (THP compaction hangs); aarch64/arm32
#     did not
#   - some pkill'd a stale QEMU first, some raced it
# Consolidating removes the divergence, not just the duplication: every target
# now gets staleness-checked builds, THP avoidance, and stale-QEMU cleanup.
#
# Usage:
#   ./scripts/run.sh list                    # show the matrix
#   ./scripts/run.sh x64-repl                # interactive serial REPL
#   ./scripts/run.sh x64-repl "(+ 1 2)"      # eval one expression, print, exit
#   ./scripts/run.sh aarch64-ssh             # boot, wait for sshd, run a smoke test
#   ./scripts/run.sh aarch64-ssh --keep      # ... and leave QEMU running
#   PORT=2223 ./scripts/run.sh x64-ssh       # override the forwarded port
#
# Ctrl-A X quits QEMU in interactive mode.

set -e
cd "$(dirname "$0")/.."
SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"

# ---------------------------------------------------------------------------
# The matrix.  One row per run configuration:
#   target | mode | build-script | image | qemu-system-* | machine flags | net device
# `mode` is repl (serial in/out) or ssh (boot in background, forward a port).
# Machine flags are transcribed VERBATIM from the script each row replaces.
# ---------------------------------------------------------------------------
read -r -d '' TARGETS <<'EOF' || true
x64-repl|repl|x64/bare/qemu/repl|/tmp/modus-x64.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|
x64-console-repl|repl|build-x64-console-repl|/tmp/modus-x64-console.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|
x64-cl-repl|repl|build-x64-cl-repl|/tmp/modus-x64-cl.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|
x64-ssh|ssh|build-x64-ssh|/tmp/modus-x64-ssh.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|-device e1000,netdev=net0,romfile=,rombar=0
aarch64-repl|repl|aarch64/bare/qemu/repl|/tmp/modus-aarch64.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|
aarch64-ssh|ssh|build-aarch64-ssh|/tmp/modus-aarch64-ssh.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|-device e1000,netdev=net0,romfile=,rombar=0
aarch64-actors|ssh|build-aarch64-actors|/tmp/modus-aarch64-actors.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|-device e1000,netdev=net0,romfile=,rombar=0
aarch64-isolated|ssh|build-aarch64-isolated|/tmp/modus-aarch64-isolated.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|-device e1000,netdev=net0,romfile=,rombar=0
i386-repl|repl|build-i386-repl|/tmp/modus-i386.bin|qemu-system-i386|-m 256 -display none -serial stdio -no-reboot|
i386-ssh|ssh|build-i386-ssh|/tmp/modus-i386-ssh.bin|qemu-system-i386|-m 256 -nographic -no-reboot|-device ne2k_isa,netdev=net0,iobase=0x300,irq=9
arm32-repl|repl|build-arm32-repl|/tmp/modus-arm32.bin|qemu-system-arm|-M virt,highmem=off -cpu cortex-a15 -m 256 -nographic|
arm32-ssh|ssh|build-arm32-ssh|/tmp/modus-arm32-ssh.bin|qemu-system-arm|-M raspi2b -m 1G -nographic|-device usb-net,netdev=net0
rpi-repl|repl|aarch64/bare/rpi/repl|/tmp/kernel8.img|qemu-system-aarch64|-M raspi3b -m 1G -nographic|
rpi-ssh|ssh|build-rpi-ssh|/tmp/kernel8-ssh.img|qemu-system-aarch64|-M raspi3b -m 1G -nographic|
rpi-hid|repl|build-rpi-hid|/tmp/kernel8-hid.img|qemu-system-aarch64|-M raspi3b -m 1G -nographic|
rpi-periph|repl|build-rpi-periph|/tmp/kernel8.img|qemu-system-aarch64|-M raspi3b -m 1G -nographic|
EOF

# Targets whose BUILD is known broken (mvm/BUILDS.md, verified 2026-08-02).
# Listed rather than hidden: the runner should tell you WHY, not fail obscurely.
KNOWN_BROKEN="rpi-repl rpi-ssh rpi-hid rpi-periph"
BROKEN_NOTE="RPi/PiZero builds are broken (A64-BUFFER/MVM-BUFFER type error).
The plan is to MIGRATE this family to the CL/mvm image (task #209), not to
repair the legacy :RPI path — so this target is expected to fail at build."

# Targets that are NOT plain qemu boots and keep their own scripts.
declare -A SPECIAL=(
  [uefi-repl]="scripts/run-uefi-repl.sh — needs OVMF + mtools + a FAT image"
  [i386-cli]="scripts/run-i386.sh — hosted CLI, runs under qemu-i386-static"
  [fixpoint]="scripts/run-fixpoint.sh — multi-arch fixpoint chain"
  [bench]="scripts/run-bench.sh — benchmark harness"
  [ansi]="scripts/ansi-summary.sh — the 64-shard conformance gate"
)

row_for() { echo "$TARGETS" | grep -E "^$1\|" || true; }

do_list() {
  echo "Modus run configurations — ./scripts/run.sh <target> [expr]"
  echo
  printf "  %-18s %-5s %-26s %s\n" TARGET MODE IMAGE NOTE
  while IFS='|' read -r t mode build bin qemu flags net; do
    [ -z "$t" ] && continue
    local note=""
    case " $KNOWN_BROKEN " in *" $t "*) note="BUILD BROKEN (see #209)";; esac
    [ -n "$net" ] && [ -z "$note" ] && note="net"
    printf "  %-18s %-5s %-26s %s\n" "$t" "$mode" "$bin" "$note"
  done <<< "$TARGETS"
  echo
  echo "Not plain QEMU boots — these keep their own scripts:"
  for k in "${!SPECIAL[@]}"; do printf "  %-18s %s\n" "$k" "${SPECIAL[$k]}"; done
}

case "${1:-list}" in
  list|--list|-l|help|--help|-h) do_list; exit 0 ;;
esac

TARGET="$1"; shift || true

if [ -n "${SPECIAL[$TARGET]}" ]; then
  echo "run.sh: '$TARGET' is not a plain QEMU boot." >&2
  echo "  use: ${SPECIAL[$TARGET]}" >&2
  exit 2
fi

ROW="$(row_for "$TARGET")"
if [ -z "$ROW" ]; then
  echo "run.sh: unknown target '$TARGET'" >&2; echo >&2; do_list >&2; exit 2
fi

IFS='|' read -r _t MODE BUILD BIN QEMU FLAGS NET <<< "$ROW"

case " $KNOWN_BROKEN " in
  *" $TARGET "*) echo "run.sh: NOTE — $BROKEN_NOTE" >&2; echo >&2 ;;
esac

# --- build if the image is missing or the build script is newer -------------
# (Taken from run-x64-*.sh.  The aarch64/arm32 scripts rebuilt every time.)
# Build through mvm/build.lisp, which OWNS the matrix — never by naming a
# build-*.lisp here.  Two reasons: (a) it keeps one table instead of two that
# can drift, which is the exact failure this script was written to end; and
# (b) a cell whose script has been RETIRED (its build is now native to
# build.lisp) has no build-*.lisp left to name.  The BUILD column is a
# build.lisp CELL KEY, not a filename.
if [ ! -f "$BIN" ] || [ mvm/build.lisp -nt "$BIN" ]; then
  echo "run.sh: building $TARGET ($BUILD)..." >&2
  sbcl --script mvm/build.lisp "$BUILD" >&2
fi

# --- THP wrapper: QEMU can hang in khugepaged compaction on this box --------
NO_THP=""
[ -x "$SCRIPTDIR/no-thp-exec" ] && NO_THP="$SCRIPTDIR/no-thp-exec"

# --- clear a stale QEMU holding the image / port ----------------------------
pkill -9 -f "$QEMU.*$(basename "$BIN")" 2>/dev/null || true
sleep 0.3

if [ "$MODE" = "repl" ]; then
  # One expression → delegate to the shared eval harness (handles the
  # boot banner + the serial REPL eating the first character).
  if [ -n "${1:-}" ]; then
    ARCH="${TARGET%%-*}"
    exec "$SCRIPTDIR/run-repl-eval.sh" "$ARCH" "$BIN" "$1"
  fi
  exec $NO_THP $QEMU -kernel "$BIN" $FLAGS
fi

# --- ssh mode ---------------------------------------------------------------
PORT="${PORT:-2222}"
LOGFILE="/tmp/modus-$TARGET.log"
KEEP=""
[ "${1:-}" = "--keep" ] && KEEP=1

$NO_THP $QEMU -kernel "$BIN" $FLAGS \
    $NET -netdev "user,id=net0,hostfwd=tcp::${PORT}-:22" \
    > "$LOGFILE" 2>&1 &
QEMU_PID=$!
trap '[ -z "$KEEP" ] && kill -9 $QEMU_PID 2>/dev/null || true' EXIT

echo "run.sh: $TARGET booting (pid $QEMU_PID), ssh on port $PORT, log $LOGFILE" >&2

for i in $(seq 1 60); do
  if ! kill -0 $QEMU_PID 2>/dev/null; then
    echo "run.sh: QEMU exited early — last 20 log lines:" >&2
    tail -20 "$LOGFILE" >&2; exit 1
  fi
  # bash /dev/tcp probe: no netcat dependency
  (exec 3<>/dev/tcp/127.0.0.1/$PORT) 2>/dev/null && break
  sleep 1
done

echo "run.sh: port $PORT open — smoke test" >&2
echo '(+ 1 2)' | ssh -p "$PORT" \
    -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
    -o ConnectTimeout=30 -o LogLevel=ERROR test@localhost || true

if [ -n "$KEEP" ]; then
  echo "run.sh: leaving QEMU running (pid $QEMU_PID); kill it with: kill -9 $QEMU_PID" >&2
  trap - EXIT
fi
