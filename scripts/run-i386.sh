#!/usr/bin/env bash
# run-i386.sh — the entry point for the i386 CL image (mvm/build-i386-cli.lisp).
#
# This is the REAL Common Lisp image on 32-bit: prelude + gc + rt + the whole
# cl-*.lisp bridge + the unforked net/crypto.lisp, compiled to native i386.
# It is NOT the legacy mini-Lisp (mvm/build-i386-{repl,ssh,diag-ssh}.lisp run
# mvm/repl-source.lisp, a separate 708-line toy Lisp with its own reader and
# evaluator; those have their own scripts/run-i386-repl.sh and -ssh.sh).
#
# WHY THIS SCRIPT EXISTS: binfmt_misc is NOT registered for i386 here, so
# running a 32-bit ELF directly does not fail loudly — it silently fails to
# exec. Everything must go through qemu-i386-static. That cost hours once;
# nobody should have to remember it again.
#
#   ./scripts/run-i386.sh build          build the image (layer 4)
#   ./scripts/run-i386.sh test           run the regression suite (exit 0 = green)
#   ./scripts/run-i386.sh gc             dump heap / collector state
#   ./scripts/run-i386.sh bulk 64        SHA-256 over 64 KiB; prints digest + collections
#   ./scripts/run-i386.sh chain 10       cons-chain survival over 1000 conses
#   ./scripts/run-i386.sh probe N [ARG]  run probe N directly
#   ./scripts/run-i386.sh exec ARGS...   run the image with arbitrary arguments
#
# Build knobs (env, all dev-only except OUT/LAYER) are documented at the top of
# mvm/build-i386-cli.lisp. Pass them through: MODUS_I386_VL=1048576 ./run-i386.sh build
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="${MODUS_I386_OUT:-/home/claude/ws5-gate-out/modus-i386-cli}"
QEMU="${QEMU_I386:-qemu-i386-static}"

die() { echo "run-i386: $*" >&2; exit 1; }

need_qemu() {
  command -v "$QEMU" >/dev/null 2>&1 || die \
"$QEMU not found. 32-bit ELFs cannot be run directly here — binfmt_misc is not
 registered, so ./\$binary silently fails to exec rather than reporting an error.
 Install qemu-user-static or set QEMU_I386."
}

need_image() {
  [ -x "$IMAGE" ] || die "no image at $IMAGE — run: $0 build"
}

run_image() { need_qemu; need_image; exec "$QEMU" "$IMAGE" "$@"; }

cmd="${1:-help}"; shift || true
case "$cmd" in
  build)
    layer="${1:-4}"
    echo "run-i386: building layer $layer -> $IMAGE"
    MODUS_I386_LAYER="$layer" MODUS_I386_OUT="$IMAGE" \
      sbcl --dynamic-space-size 8192 --script "$ROOT/mvm/build-i386-cli.lisp"
    ;;
  test)
    # No argv selects the regression suite; it exits non-zero if any check fails.
    # Known gaps are reported on every run but do not fail the suite.
    run_image
    ;;
  gc)    run_image 1 ;;
  bulk)  run_image 2 "${1:?usage: $0 bulk <KiB>}" ;;
  chain) run_image 3 "${1:?usage: $0 chain <hundreds-of-conses>}" ;;
  probe) run_image "${1:?usage: $0 probe <n> [arg]}" "${2:-}" ;;
  exec)  run_image "$@" ;;
  help|-h|--help)
    sed -n '2,30p' "$0" | sed 's/^# \{0,1\}//'
    ;;
  *) die "unknown command '$cmd' (try: $0 help)" ;;
esac
