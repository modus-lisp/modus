#!/usr/bin/env bash
# build-ccl.sh — build a Modus image with Clozure CL (CCL) as the host Lisp
# instead of SBCL, via the lib/ccl-host-compat.lisp shim.
#
#   scripts/build-ccl.sh [build-script]
#
# build-script defaults to mvm/build-generic-cli.lisp (the hosted ./modus CLI).
# Set CCL to the CCL binary (default: $HOME/ccl/lx86cl64).  Output path follows
# the build script's own default (MODUS_CLI_OUT is #+sbcl-only, so `modus`).
#
# Verified with CCL 1.12.2 on linux-x86-64; see lib/ccl-host-compat.lisp.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
cd "$here"

CCL="${CCL:-$HOME/ccl/lx86cl64}"
BUILD_SCRIPT="${1:-mvm/build-generic-cli.lisp}"

if [ ! -x "$CCL" ]; then
  echo "error: CCL binary not found/executable at: $CCL" >&2
  echo "  install by extracting a Clozure CL release, or set CCL=/path/to/lx86cl64" >&2
  exit 1
fi

CCL_DIR="$(dirname "$CCL")"
launcher="$(mktemp)"
trap 'rm -f "$launcher"' EXIT
cat > "$launcher" <<EOF
(load "lib/ccl-host-compat.lisp")
(setf ccl:*default-file-character-encoding* :utf-8)
(handler-case (load "$BUILD_SCRIPT")
  (error (e) (format t "~&BUILD ERROR: ~A~%" e)
    (ignore-errors (ccl:print-call-history :count 25))
    (ccl:quit 1)))
(ccl:quit 0)
EOF

CCL_DEFAULT_DIRECTORY="$CCL_DIR" "$CCL" --no-init --batch --load "$launcher"
