#!/usr/bin/env bash
# build-abcl.sh — build a Modus image with Armed Bear Common Lisp (ABCL, JVM)
# as the host Lisp instead of SBCL, via the lib/abcl-host-compat.lisp shim and
# the compiled-speed driver scripts/build-abcl-driver.lisp.
#
#   scripts/build-abcl.sh [build-script]
#
# build-script defaults to mvm/build-generic-cli.lisp (the hosted ./modus CLI).
# Set ABCL_JAR (default $HOME/abcl-bin-1.9.2/abcl.jar) and JAVA_XMX (default 6g).
# Output path follows the build script's own default (`modus`).
#
# MODUS_GLOBAL_CHECK defaults to `warn` here: ABCL's stricter reader turns the
# host-side check-source-parses sweep of two baked-as-source files (which name
# in-image-only packages UIOP/SCL/ASDF/…) into false positives.  The image is
# unaffected — see the note in lib/abcl-host-compat.lisp.
#
# Verified with ABCL 1.9.2 on OpenJDK 17.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
cd "$here"

ABCL_JAR="${ABCL_JAR:-$HOME/abcl-bin-1.9.2/abcl.jar}"
JAVA_XMX="${JAVA_XMX:-6g}"
export MODUS_ABCL_BUILD_SCRIPT="${1:-mvm/build-generic-cli.lisp}"
export MODUS_GLOBAL_CHECK="${MODUS_GLOBAL_CHECK:-warn}"

if [ ! -f "$ABCL_JAR" ]; then
  echo "error: abcl.jar not found at: $ABCL_JAR" >&2
  echo "  extract an ABCL release, or set ABCL_JAR=/path/to/abcl.jar" >&2
  exit 1
fi

java "-Xmx${JAVA_XMX}" -jar "$ABCL_JAR" --batch --load scripts/build-abcl-driver.lisp
