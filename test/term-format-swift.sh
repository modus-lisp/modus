#!/bin/sh
# term-format-swift.sh -- the wire format, both ways between the image and Swift.
#
#   test/term-format-swift.sh          (from anywhere; uses the aarch64 runner below)
#
# 1. the image encodes the samples (test/term-samples.lisp) to /tmp/term-lisp.hex;
# 2. Swift (host/ios/swift/Term.swift) decodes them, checks them against its own copy of the samples,
#    re-encodes them to the same bytes, and writes its encodings to /tmp/term-swift.hex;
# 3. the image decodes Swift's encodings and compares them with the samples.
#
# MODUS_RUNNER: an aarch64 image with the modus runtime (default: the scratch runner, which the
# iOS world's build also produces).  MODUS_SLIDE: the slide that runner needs on this Mac.
set -eu
RUNNER=${MODUS_RUNNER:-/tmp/modus-a64/modus-mac4}
SLIDE=${MODUS_SLIDE:-300000000}
cd "$(dirname "$0")/.."
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

echo "== 1. the image encodes the samples"
MODUS_SLIDE=$SLIDE "$RUNNER" --script test/term-swift-emit.lisp 2>&1 | grep "emitted" || true

echo "== 2. Swift decodes them and encodes its own"
swiftc -o "$TMP/term-check" host/ios/swift/Term.swift test/term-swift/main.swift
"$TMP/term-check" /tmp/term-lisp.hex /tmp/term-swift.hex

echo "== 3. the image decodes Swift's encodings"
MODUS_SLIDE=$SLIDE "$RUNNER" --script test/term-swift-check.lisp 2>&1 | grep "PASS\|FAIL\|of " || true
