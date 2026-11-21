#!/bin/sh
# build-macos.sh IMAGE.elf OUT [cli|gui] — wrap a Darwin-layout modus image in a native,
# signed macOS executable (docs/macos-hosting.md, M0; the window, M4).
#
#   cli  (default)  OUT is a Mach-O executable: the modus REPL on the terminal.  No AppKit.
#   gui             OUT is an .app bundle; the image runs in a native AppKit window
#                   (host/macos/modus-ui-mac.m).  Same image, same syscalls, one more host.
#
# Build the image first, e.g.:
#   MODUS_DARWIN=1 MODUS_CODE_BASE=7000010000 MODUS_CONV_DELTA=7040000000 \
#   MODUS_HEAP_BASE=7080000000 MODUS_JIT_ARENA_BASE=7100000000 \
#   MODUS_CLI_OUT=/tmp/modus-darwin.elf \
#   sbcl --dynamic-space-size 16384 --script mvm/build-aarch64-cli.lisp
# (MODUS_NO_JIT=1 also works; the JIT build is the default and is what the runner was tested with.)
set -eu
IMAGE=$1
OUT=$2
KIND=${3:-cli}
HERE=$(cd "$(dirname "$0")" && pwd)
# The image goes into __TEXT (r-x, covered by the code signature) on a 16 KB
# boundary, so the shim can mach_vm_remap it page for page.  ld signs arm64
# executables ad hoc.
# MODUS_IN_PLACE=1: a PC-relative image (MODUS_PCREL=1) instead gets its own
# segments and runs where the loader put it (image-segments.sh).
if [ "${MODUS_IN_PLACE:-0}" != 0 ]; then
  WHERE=$("$HERE/image-segments.sh" "$IMAGE" "$OUT.segs")
else
  WHERE="-Wl,-sectcreate,__TEXT,__modus,$IMAGE -Wl,-sectalign,__TEXT,__modus,0x4000"
fi

case $KIND in
  cli)
    BIN=$OUT
    DEFS=""
    EXTRA_SRC=""
    FRAMEWORKS="-framework AudioToolbox -framework Security -framework Foundation"
    ;;
  gui)
    rm -rf "$OUT"
    mkdir -p "$OUT/Contents/MacOS"
    BIN=$OUT/Contents/MacOS/modus
    DEFS="-DMODUS_MAC_GUI=1"
    EXTRA_SRC="$HERE/modus-ui-mac.m"
    FRAMEWORKS="-framework AudioToolbox -framework Security -framework Foundation -framework AppKit -framework IOSurface -framework QuartzCore"
    cat > "$OUT/Contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key><string>modus</string>
  <key>CFBundleDisplayName</key><string>modus</string>
  <key>CFBundleIdentifier</key><string>org.modus-lisp.modus</string>
  <key>CFBundleExecutable</key><string>modus</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>1.0</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>LSMinimumSystemVersion</key><string>14.0</string>
  <key>NSPrincipalClass</key><string>NSApplication</string>
  <key>NSHighResolutionCapable</key><true/>
</dict>
</plist>
PLIST
    ;;
  *)
    echo "build-macos.sh: kind must be cli or gui, not $KIND" >&2
    exit 2
    ;;
esac

# shellcheck disable=SC2086  # WHERE, DEFS, EXTRA_SRC and FRAMEWORKS are flag lists
cc -O2 -Wall -fobjc-arc $DEFS -o "$BIN" "$HERE/modus-shim.c" "$HERE/modus-audio.c" "$HERE/modus-keychain.m" $EXTRA_SRC "$HERE/syscall-stub.S" $FRAMEWORKS $WHERE
codesign --force --sign - "$BIN" >/dev/null 2>&1 || true
if [ "$KIND" = gui ]; then
  codesign --force --sign - "$OUT" >/dev/null 2>&1 || true
fi
echo "wrote $OUT ($KIND, $(wc -c < "$BIN" | tr -d ' ') bytes in the executable)"
