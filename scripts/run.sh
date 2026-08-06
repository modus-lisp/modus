#!/bin/bash
# run.sh — ONE entry point for every Modus run configuration.
#
# Replaces the near-identical run-<arch>-<payload>.sh launchers.  They were
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
#   ./scripts/run.sh x64-ssh "(* 6 7)"       # boot, run THIS expression over ssh
#   ./scripts/run.sh aarch64-ssh --keep      # ... and leave QEMU running
#   ./scripts/run.sh x64-ssh --serve         # boot and BLOCK, like run-x64-ssh.sh
#   PORT=2223 ./scripts/run.sh x64-ssh       # override the forwarded port
#
# Environment:
#   PORT                 forwarded ssh port (default 2222)
#   SBCL_DYNAMIC_SPACE   MB for the build's SBCL heap (default 12288)
#   REPL_TIMEOUT         see run-repl-eval.sh (default 60s)
#   MODUS_NO_REBUILD=1   never rebuild; boot the image already on disk
#
# Ctrl-A X quits QEMU in interactive mode.

set -e
cd "$(dirname "$0")/.."
SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"

# ---------------------------------------------------------------------------
# The matrix.  One row per run configuration:
#
#   target|mode|build|image|qemu-system-*|machine flags|net device|ready|arch
#
# `mode` is repl (serial in/out) or ssh (boot in background, forward a port).
# `ready` (ssh only) is  MARKER;TRIES;SETTLE  — the guest-side log line that
#   says the payload is up, how many 0.5s polls to allow, and how long to let
#   the guest settle before connecting.  ALL THREE are transcribed from the
#   script the row replaces; see "Wait for the GUEST to say it is up" below for
#   why a TCP probe is NOT a substitute.
# `arch` is the key run-repl-eval.sh uses to pick its QEMU command line.
# Machine flags are transcribed VERBATIM from the script each row replaces.
# ---------------------------------------------------------------------------
read -r -d '' TARGETS <<'EOF' || true
x64-repl|repl|x64/bare/qemu/repl|/tmp/modus-x64.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|||x64
x64-console-repl|repl|build-x64-console-repl|/tmp/modus-x64-console.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|||x64
x64-cl-repl|repl|build-x64-cl-repl|/tmp/modus-x64-cl-repl.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|||x64
x64-ssh|ssh|x64/bare/qemu/ssh|/tmp/modus-x64-ssh.bin|qemu-system-x86_64|-m 512 -nographic -no-reboot|-device e1000,netdev=net0,romfile=,rombar=0|SSH:;120;5|x64
aarch64-repl|repl|aarch64/bare/qemu/repl|/tmp/modus-aarch64.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|||aarch64
aarch64-ssh|ssh|aarch64/bare/qemu/ssh|/tmp/modus-aarch64-ssh.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|-device e1000,netdev=net0,romfile=,rombar=0|SSH:;120;5|aarch64
aarch64-actors|ssh|aarch64/bare/qemu/actors|/tmp/modus-aarch64-actors.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|-device e1000,netdev=net0,romfile=,rombar=0|SSH:;120;5|aarch64
aarch64-isolated|ssh|aarch64/bare/qemu/isolated|/tmp/modus-aarch64-isolated.bin|qemu-system-aarch64|-machine virt -cpu cortex-a57 -m 512 -nographic -semihosting|-device e1000,netdev=net0,romfile=,rombar=0|SSH:;120;5|aarch64
i386-repl|repl|build-i386-repl|/tmp/modus-i386.bin|qemu-system-i386|-m 256 -display none -serial stdio -no-reboot|||i386
i386-ssh|ssh|i386/bare/qemu/ssh|/tmp/modus-i386-ssh.bin|qemu-system-i386|-m 256 -nographic -no-reboot|-device ne2k_isa,netdev=net0,iobase=0x300,irq=9|DHCP:IP=;120;5|i386
arm32-repl|repl|build-arm32-repl|/tmp/modus-arm32.bin|qemu-system-arm|-M virt,highmem=off -cpu cortex-a15 -m 256 -nographic|||arm32
arm32-ssh|ssh|build-arm32-ssh|/tmp/modus-arm32-ssh.bin|qemu-system-arm|-M raspi2b -m 1G -nographic|-device usb-net,netdev=net0|SSH:;180;5|arm32
rpi-repl|repl|aarch64/bare/rpi/repl|/tmp/kernel8.img|qemu-system-aarch64|-machine raspi3b -display none -serial stdio -semihosting|||rpi
rpi-ssh|ssh|build-rpi-ssh|/tmp/kernel8-ssh.img|qemu-system-aarch64|-machine raspi3b -display none -serial stdio|-device usb-net,netdev=net0|SSH:;180;15|rpi
rpi-hid|repl|build-rpi-hid|/tmp/kernel8-hid.img|qemu-system-aarch64|-machine raspi3b -display none -serial stdio -device usb-kbd|||rpi
rpi-periph|repl|build-rpi-periph|/tmp/piboot/kernel8.img|qemu-system-aarch64|-M raspi3b -serial null -serial stdio -display gtk|||rpi
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
  echo "Modus run configurations — ./scripts/run.sh <target> [expr] [--keep|--serve]"
  echo
  printf "  %-18s %-5s %-26s %s\n" TARGET MODE IMAGE NOTE
  while IFS='|' read -r t mode build bin qemu flags net ready arch; do
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

IFS='|' read -r _t MODE BUILD BIN QEMU FLAGS NET READY ARCH <<< "$ROW"

EXPR=""; KEEP=""; SERVE=""
while [ $# -gt 0 ]; do
  case "$1" in
    --keep)  KEEP=1 ;;
    --serve) SERVE=1 ;;
    *)       EXPR="$1" ;;
  esac
  shift
done

case " $KNOWN_BROKEN " in
  *" $TARGET "*) echo "run.sh: NOTE — $BROKEN_NOTE" >&2; echo >&2 ;;
esac

# --- build if the image is missing or ANY first-party source is newer -------
# The *-repl.sh scripts compared the image against ONE build script;
# run-x64-console-repl.sh compared it against a hand-maintained list of seven
# sources (repl-source.lisp, the boot files, compiler.lisp, translate-x64.lisp).
# A newest-source-wins check is a superset of both, so consolidating here does
# not silently drop the console-repl script's wider staleness test.
# (run-aarch64-ssh.sh / run-arm32-ssh.sh / run-i386-ssh.sh had NO staleness
# test at all — they rebuilt on every invocation, minutes per run.  That is the
# drift this ends.)
#
# COST, stated plainly: this is deliberately over-broad.  `build-image` bakes
# the whole combined source text (comments included — see BUILDS.md), so any
# .lisp under those trees CAN change an image, and a stale image is a much
# worse failure than a slow one.  But it also means editing an unrelated build
# script rebuilds everything.  `MODUS_NO_REBUILD=1` skips the check when you
# know the image on disk is the one you want.
NEED_BUILD=""
if [ -n "${MODUS_NO_REBUILD:-}" ]; then
  [ -f "$BIN" ] || { echo "run.sh: MODUS_NO_REBUILD set but $BIN does not exist" >&2; exit 1; }
elif [ ! -f "$BIN" ] ||
     [ -n "$(find mvm boot net lib runtime -name '*.lisp' -newer "$BIN" -print -quit 2>/dev/null || true)" ]; then
  NEED_BUILD=1
fi
if [ -n "$NEED_BUILD" ]; then
  echo "run.sh: building $TARGET ($BUILD)..." >&2
  # Build through mvm/build.lisp, which OWNS the matrix — never by naming a
  # build-*.lisp here.  Two reasons: (a) it keeps one table instead of two
  # that can drift, which is the exact failure this script was written to
  # end; and (b) a cell whose script has been RETIRED (its build is now
  # native to build.lisp) has no build-*.lisp left to name.  The BUILD column
  # is a build.lisp CELL KEY, not a filename.
  #
  # --dynamic-space-size is NOT optional: the default SBCL heap is too small
  # for the larger images (build-x64-cl-repl heap-exhausts inside BUILD-IMAGE
  # and leaves no kernel behind).  12288 matches mvm/BUILDS.md's audit recipe.
  sbcl --dynamic-space-size "${SBCL_DYNAMIC_SPACE:-12288}" \
       --script mvm/build.lisp "$BUILD" >&2
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
  if [ -n "$EXPR" ]; then
    exec "$SCRIPTDIR/run-repl-eval.sh" "$ARCH" "$BIN" "$EXPR"
  fi
  exec $NO_THP $QEMU -kernel "$BIN" $FLAGS
fi

# --- ssh mode ---------------------------------------------------------------
PORT="${PORT:-2222}"
LOGFILE="/tmp/modus-$TARGET.log"
MARKER="${READY%%;*}"; rest="${READY#*;}"
TRIES="${rest%%;*}"; SETTLE="${rest##*;}"

> "$LOGFILE"
$NO_THP $QEMU -kernel "$BIN" $FLAGS \
    $NET -netdev "user,id=net0,hostfwd=tcp::${PORT}-:22" \
    > "$LOGFILE" 2>&1 &
QEMU_PID=$!
trap '[ -z "$KEEP$SERVE" ] && kill -9 $QEMU_PID 2>/dev/null || true' EXIT

echo "run.sh: $TARGET booting (pid $QEMU_PID), ssh on port $PORT, log $LOGFILE" >&2

# Wait for the GUEST to say it is up.
#
# This must be a log-marker wait, not a TCP probe.  QEMU binds the hostfwd
# port the moment it starts, so `/dev/tcp/127.0.0.1/$PORT` succeeds on the
# first poll — before the guest has even reached its NIC init.  The earlier
# version of this script did exactly that, connected instantly, and reported
# "Connection timed out during banner exchange" while still exiting 0: a
# silent false pass.  Every run-*-ssh.sh it replaces greps the log instead
# ("SSH:" on x64/aarch64/arm32/rpi, "DHCP:IP=" on i386); so does this.
i=0
while [ "$i" -lt "$TRIES" ]; do
  if ! kill -0 $QEMU_PID 2>/dev/null; then
    echo "run.sh: QEMU exited early — last 20 log lines:" >&2
    tail -20 "$LOGFILE" >&2; exit 1
  fi
  grep -q "$MARKER" "$LOGFILE" 2>/dev/null && break
  sleep 0.5
  i=$((i + 1))
done

if ! grep -q "$MARKER" "$LOGFILE" 2>/dev/null; then
  echo "run.sh: timed out waiting for '$MARKER' — last 20 log lines:" >&2
  tail -20 "$LOGFILE" >&2
  # Kill it even under --keep/--serve: the boot FAILED, so leaving a wedged
  # QEMU behind (the EXIT trap honours --keep) would just orphan it.
  kill -9 $QEMU_PID 2>/dev/null || true
  exit 1
fi

echo "run.sh: guest reported '$MARKER' — settling ${SETTLE}s, then ssh" >&2
sleep "$SETTLE"

RC=0
echo "${EXPR:-(+ 1 2)}" | ssh -p "$PORT" \
    -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
    -o ConnectTimeout=30 -o LogLevel=ERROR test@localhost || RC=$?

if [ -n "$SERVE" ]; then
  echo >&2
  echo "run.sh: Modus $TARGET SSH server ready on port $PORT." >&2
  echo "  ssh -p $PORT -o StrictHostKeyChecking=no test@localhost" >&2
  echo >&2
  echo "run.sh: Press Ctrl-C to stop." >&2
  wait "$QEMU_PID" 2>/dev/null || true
elif [ -n "$KEEP" ]; then
  echo "run.sh: leaving QEMU running (pid $QEMU_PID); kill it with: kill -9 $QEMU_PID" >&2
  trap - EXIT
fi

exit $RC
