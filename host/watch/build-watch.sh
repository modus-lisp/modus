#!/bin/sh
# build-watch.sh IMAGE OUT.app — a standalone watchOS Simulator app running a
# modus image in place (docs/macos-hosting.md, "On the watch").
#
# IMAGE is a PC-relative, JIT-off Darwin image (the iOS layout):
#   MODUS_PCREL=1 MODUS_DARWIN=1 MODUS_NO_JIT=1 MODUS_CODE_BASE=300010000 \
#   MODUS_CONV_DELTA=2FA000000 MODUS_HEAP_BASE=33A000000 \
#   MODUS_JIT_ARENA_BASE=3AC000000 MODUS_CLI_OUT=… sbcl … mvm/build-aarch64-cli.lisp
#
# The Simulator runs watch apps as 64-bit processes on the Mac, but NOT at the
# iOS layout: the watch Simulator's dyld maps its shared cache privately from
# 6 GB to past 9 GB, over the iOS layout's heap and arena.  The layout above
# (12 GB) is what `kiln ios watch` builds.  A real watch is arm64_32 (a 4 GB
# address space) and needs a smaller layout — not this script's job yet.
#
# MODUS_CORE=FILE puts a snapshot's compiled code in the app (image-segments.sh).
#
# Files named in MODUS_WATCH_FILES are copied into the bundle, for "@NAME"
# arguments (host/macos/modus-shim.c).
set -eu
IMAGE=$1; OUT=$2
HERE=$(cd "$(dirname "$0")" && pwd)
MAC=$HERE/../macos
SDK=$(xcrun --sdk watchsimulator --show-sdk-path)
TARGET=arm64-apple-watchos11.0-simulator
OBJ="$OUT.obj"
rm -rf "$OUT" "$OBJ"; mkdir -p "$OUT" "$OBJ"

# The Swift side, one module: the screen (modus-watch.swift), the SwiftUI components Lisp drives
# (modus-watch-ui.swift, the twin of host/ios/swift/ModusSwift.swift), and the term format they speak.
xcrun --sdk watchsimulator swiftc -target $TARGET -parse-as-library -O -wmo \
  -module-name ModusWatch -import-objc-header "$HERE/../ios/swift/bridge.h" \
  -c "$HERE/modus-watch.swift" "$HERE/modus-watch-ui.swift" "$HERE/../ios/swift/Term.swift" \
  -o "$OBJ/modus-watch.o"
xcrun --sdk watchsimulator clang -target $TARGET -isysroot "$SDK" -O2 -Wall \
  -c "$MAC/modus-shim.c" -o "$OBJ/modus-shim.o"
# The speaker, where the SDK has AudioToolbox; watchOS 27's has none, and gets a stub that answers ENOSYS.
AUDIO_FW=""
if [ -d "$SDK/System/Library/Frameworks/AudioToolbox.framework" ]; then
  xcrun --sdk watchsimulator clang -target $TARGET -isysroot "$SDK" -O2 -Wall \
    -c "$MAC/modus-audio.c" -o "$OBJ/modus-audio.o"
  AUDIO_FW="-framework AudioToolbox"
else
  xcrun --sdk watchsimulator clang -target $TARGET -isysroot "$SDK" -O2 -Wall \
    -c "$HERE/modus-audio-none.c" -o "$OBJ/modus-audio.o"
fi
# The Objective-C runtime (1040), the Swift registry (1060) and the keychain (1030-1032), as on the phone.
for SRC in "$HERE/../ios/modus-objc.m" "$HERE/../ios/modus-swift.m" "$MAC/modus-keychain.m"; do
  xcrun --sdk watchsimulator clang -target $TARGET -isysroot "$SDK" -O2 -Wall -fobjc-arc \
    -c "$SRC" -o "$OBJ/$(basename "$SRC").o"
done
xcrun --sdk watchsimulator clang -target $TARGET -isysroot "$SDK" \
  -c "$MAC/syscall-stub.S" -o "$OBJ/syscall-stub.o"

# The image in its own segments, run in place, with its data layout reserved.
WHERE=$("$MAC/image-segments.sh" "$IMAGE" "$OUT.segs")
TOOLCHAIN=$(dirname "$(dirname "$(xcrun --sdk watchsimulator --find swiftc)")")
# shellcheck disable=SC2086  # WHERE and AUDIO_FW are flag lists
xcrun --sdk watchsimulator clang -target $TARGET -isysroot "$SDK" \
  -o "$OUT/modus" "$OBJ/modus-watch.o" "$OBJ/modus-shim.o" "$OBJ/modus-audio.o" "$OBJ/syscall-stub.o" \
  "$OBJ/modus-objc.m.o" "$OBJ/modus-swift.m.o" "$OBJ/modus-keychain.m.o" \
  -L"$SDK/usr/lib/swift" -L"$TOOLCHAIN/lib/swift/watchsimulator" \
  -framework SwiftUI $AUDIO_FW -framework CoreGraphics -framework CoreFoundation \
  -framework Foundation -framework Security $WHERE

cat > "$OUT/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleIdentifier</key><string>${MODUS_WATCH_BUNDLE_ID:-org.modus-lisp.modus.watch}</string>
  <key>CFBundleExecutable</key><string>modus</string>
  <key>CFBundleName</key><string>modus</string>
  <key>CFBundleDisplayName</key><string>modus</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundleShortVersionString</key><string>0.1</string>
  <key>MinimumOSVersion</key><string>11.0</string>
  <key>UIDeviceFamily</key><array><integer>4</integer></array>
  <key>CFBundleSupportedPlatforms</key><array><string>WatchSimulator</string></array>
  <key>WKApplication</key><true/>
  <key>WKWatchOnly</key><true/>
</dict></plist>
PLIST
for f in ${MODUS_WATCH_FILES:-}; do cp "$f" "$OUT/"; done
codesign --force --sign - "$OUT"
echo "wrote $OUT"
