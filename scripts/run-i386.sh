#!/usr/bin/env bash
# run-i386.sh — the entry point for the i386 Modus CLI (mvm/build-i386-cli.lisp).
#
# This is the REAL Common Lisp image on 32 bits: the same shared assembly
# (mvm/build-cli-common.lisp) x64's ./modus and the aarch64 CLI are built from —
# CL bridge, in-image MVM compiler, RTEST, tar/install-tarball, the ASDF and
# :GENERA surfaces, and the SBCL-faithful toplevel.  It is NOT the legacy
# mini-Lisp (mvm/build-i386-{repl,ssh,diag-ssh}.lisp run mvm/repl-source.lisp,
# a separate 708-line toy Lisp with its own reader and evaluator; those are
# ./scripts/run.sh i386-repl and scripts/run-i386-ssh.sh).
#
# WHY THIS SCRIPT EXISTS: binfmt_misc is NOT registered for i386 here, so
# running a 32-bit ELF directly does not fail loudly — it silently fails to
# exec. Everything must go through qemu-i386-static. That cost hours once;
# nobody should have to remember it again.
#
#   ./scripts/run-i386.sh build              build the image
#   ./scripts/run-i386.sh eval '(+ 1 2)'     evaluate one form and exit
#   ./scripts/run-i386.sh repl               interactive REPL on stdin
#   ./scripts/run-i386.sh exec ARGS...       run the image with arbitrary flags
#   ./scripts/run-i386.sh ladder TAG         the 22-library ladder (see below)
#
# RETIRED (2026-08 CLI convergence).  `test`, `gc`, `bulk`, `chain`, `argv` and
# `probe N` drove a ~1300-line probe suite BAKED INTO the image and reached by a
# bare numeric argv[1].  A shipping image bakes no test corpus (CLAUDE.md,
# "Build taxonomy"), and that suite is what kept the i386 image on its own build
# lineage — which is why the library ladder and alexandria's own test suite had
# never run on 32 bits at all.  The suite is recoverable from git history at
# ba693fa (mvm/build-i386-cli.lisp).  Its replacement:
#
#   ./scripts/run-ladder-i386.sh <image> <tag>   # then lf/score.py on the logs
#
# and, for anything the ladder does not cover, a --load-able script — which this
# image can now actually run.
#
# Build knobs (env, all dev-only except MODUS_I386_OUT) are documented at the
# top of mvm/build-i386-cli.lisp. Pass them through:
#   MODUS_I386_VL=1048576 ./scripts/run-i386.sh build
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="${MODUS_I386_OUT:-/tmp/modus-i386-cli}"
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
    echo "run-i386: building -> $IMAGE"
    MODUS_I386_OUT="$IMAGE" \
      sbcl --dynamic-space-size 8192 --script "$ROOT/mvm/build-i386-cli.lisp"
    ;;
  eval)  run_image --eval "${1:?usage: $0 eval <form>}" --quit ;;
  repl)  run_image ;;
  exec)  run_image "$@" ;;
  ladder)
    need_image
    exec "$ROOT/scripts/run-ladder-i386.sh" "$IMAGE" "${1:?usage: $0 ladder <tag>}" "${2:-8}" "${3:-6000}"
    ;;
  test|gc|bulk|chain|argv|probe)
    die "'$cmd' is RETIRED — the baked probe suite went away with the CLI
 convergence (see the header of this script and of mvm/build-i386-cli.lisp).
 Recover it from git history at ba693fa, or use:
   $0 ladder <tag>     # the 22-library ladder, the replacement gate
   $0 eval '<form>'    # a one-off check"
    ;;
  help|-h|--help)
    sed -n '2,40p' "$0" | sed 's/^# \{0,1\}//'
    ;;
  *) die "unknown command '$cmd' (try: $0 help)" ;;
esac
