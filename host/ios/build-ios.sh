#!/bin/sh
# build-ios.sh IMAGE.elf OUT.app [sim|device] [SIGN-IDENTITY] [PROFILE.mobileprovision]
#
# Wrap a Darwin-layout, JIT-off modus image in a minimal iOS app bundle
# (docs/macos-hosting.md, M5).  The same shim as macOS (host/macos), built with
# the iOS SDK.  Build the image with the IOS layout (low addresses: an iOS app
# cannot map at macOS's 448 GB), e.g.:
#   MODUS_DARWIN=1 MODUS_NO_JIT=1 MODUS_CODE_BASE=600010000 \
#   MODUS_CONV_DELTA=640000000 MODUS_HEAP_BASE=680000000 \
#   MODUS_JIT_ARENA_BASE=700000000 MODUS_CLI_OUT=/tmp/modus-ios.elf \
#   sbcl --dynamic-space-size 16384 --script mvm/build-aarch64-cli.lisp
# Any extra files or directories in $MODUS_IOS_FILES (space-separated) are
# copied into the bundle, before it is signed; pass "@NAME" on the command line
# to name one, or list the arguments one per line in a bundled modus.args.
# MODUS_CORE=FILE puts a snapshot's compiled code in the app (image-segments.sh).
set -eu
IMAGE=$1; OUT=$2; KIND=${3:-sim}; IDENT=${4:-}; PROFILE=${5:-}
HERE=$(cd "$(dirname "$0")" && pwd)
MAC=$HERE/../macos
case $KIND in
  sim)    SDKN=iphonesimulator; TARGET=arm64-apple-ios18.0-simulator ;;
  device) SDKN=iphoneos;        TARGET=arm64-apple-ios18.0 ;;
  *) echo "kind must be sim or device" >&2; exit 2 ;;
esac
SDK=$(xcrun --sdk $SDKN --show-sdk-path)
rm -rf "$OUT"; mkdir -p "$OUT"
# MODUS_IN_PLACE=1: run a PC-relative image from its own signed segments
# (host/macos/image-segments.sh) — the only way a device can run it.
if [ "${MODUS_IN_PLACE:-0}" != 0 ]; then
  WHERE=$("$MAC/image-segments.sh" "$IMAGE" "$OUT.segs")
else
  WHERE="-Wl,-sectcreate,__TEXT,__modus,$IMAGE -Wl,-sectalign,__TEXT,__modus,0x4000"
fi
# shellcheck disable=SC2086  # WHERE is a flag list
xcrun --sdk $SDKN clang -target $TARGET -isysroot "$SDK" -O2 -Wall \
  -o "$OUT/modus" "$MAC/modus-shim.c" "$MAC/syscall-stub.S" "$HERE/modus-ui.m" -fobjc-arc -framework UIKit -framework QuartzCore -framework CoreGraphics -framework CoreFoundation $WHERE
cat > "$OUT/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleIdentifier</key><string>${MODUS_IOS_BUNDLE_ID:-org.modus-lisp.modus}</string>
  <key>CFBundleExecutable</key><string>modus</string>
  <key>CFBundleName</key><string>modus</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundleShortVersionString</key><string>0.1</string>
  <key>MinimumOSVersion</key><string>18.0</string>
  <key>LSRequiresIPhoneOS</key><true/>
  <key>UILaunchScreen</key><dict/>
  <key>UIDeviceFamily</key><array><integer>1</integer><integer>2</integer></array>
  <key>CFBundleSupportedPlatforms</key><array><string>$( [ $KIND = sim ] && echo iPhoneSimulator || echo iPhoneOS )</string></array>
</dict></plist>
PLIST
for f in ${MODUS_IOS_FILES:-}; do cp -R "$f" "$OUT/"; done
if [ -n "$PROFILE" ]; then cp "$PROFILE" "$OUT/embedded.mobileprovision"; fi
if [ -n "$IDENT" ]; then
  ENT=$(mktemp); security cms -D -i "$PROFILE" > "$ENT.plist" 2>/dev/null && \
    /usr/libexec/PlistBuddy -x -c "Print :Entitlements" "$ENT.plist" > "$ENT" || echo '<plist version="1.0"><dict/></plist>' > "$ENT"
  codesign --force --sign "$IDENT" --entitlements "$ENT" "$OUT"
else
  codesign --force --sign - "$OUT"
fi
echo "wrote $OUT"
