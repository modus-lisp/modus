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
    # the window, and the iOS build's bridges (neither needs UIKit): the Objective-C runtime (modus-objc.m) and
    # Lisp-calls-Swift (modus-swift.m, whose functions here are host/macos/swift/)
    EXTRA_SRC="$HERE/modus-ui-mac.m $HERE/../ios/modus-objc.m $HERE/../ios/modus-swift.m"
    FRAMEWORKS="-framework AudioToolbox -framework Security -framework Foundation -framework AppKit -framework IOSurface -framework QuartzCore"
    # THE APP'S NAME (the Dock's, the menu bar's, the window's first title) and identity, and its icon:
    #   MODUS_MAC_NAME       default "modus"
    #   MODUS_MAC_BUNDLE_ID  default org.modus-lisp.modus
    #   MODUS_MAC_ICON       a square PNG (1024 x 1024 is best), made into the bundle's AppIcon.icns
    #   MODUS_MAC_URL_SCHEME a URL scheme ("modus"): a NAME://... link opens the app, and the image hears it
    #   MODUS_MAC_OPENS      what the Finder offers to open with it, as UTIs ("public.audio,public.movie"), as an
    #                        alternative, never taking a type over from the app that has it
    NAME=$(printf '%s' "${MODUS_MAC_NAME:-modus}" | sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g')
    BUNDLE_ID=${MODUS_MAC_BUNDLE_ID:-org.modus-lisp.modus}
    ICON_KEY=""
    URL_KEY=""
    if [ -n "${MODUS_MAC_URL_SCHEME:-}" ]; then
      URL_KEY="<key>CFBundleURLTypes</key><array><dict><key>CFBundleURLName</key><string>$BUNDLE_ID</string><key>CFBundleURLSchemes</key><array><string>$MODUS_MAC_URL_SCHEME</string></array></dict></array>"
    fi
    OPENS_KEY=""
    if [ -n "${MODUS_MAC_OPENS:-}" ]; then
      UTIS=""
      for u in $(printf '%s' "$MODUS_MAC_OPENS" | tr ',' ' '); do UTIS="$UTIS<string>$u</string>"; done
      OPENS_KEY="<key>CFBundleDocumentTypes</key><array><dict><key>CFBundleTypeName</key><string>Media</string><key>CFBundleTypeRole</key><string>Viewer</string><key>LSHandlerRank</key><string>Alternate</string><key>LSItemContentTypes</key><array>$UTIS</array></dict></array>"
    fi
    if [ -n "${MODUS_MAC_ICON:-}" ]; then
      SET=$(mktemp -d)/AppIcon.iconset
      mkdir -p "$SET" "$OUT/Contents/Resources"
      for S in 16 32 128 256 512; do
        sips -z $S $S "$MODUS_MAC_ICON" --out "$SET/icon_${S}x${S}.png" >/dev/null
        sips -z $((S * 2)) $((S * 2)) "$MODUS_MAC_ICON" --out "$SET/icon_${S}x${S}@2x.png" >/dev/null
      done
      iconutil -c icns "$SET" -o "$OUT/Contents/Resources/AppIcon.icns"
      rm -rf "$(dirname "$SET")"
      ICON_KEY="<key>CFBundleIconFile</key><string>AppIcon</string>"
    fi
    cat > "$OUT/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key><string>$NAME</string>
  <key>CFBundleDisplayName</key><string>$NAME</string>
  <key>CFBundleIdentifier</key><string>$BUNDLE_ID</string>
  $ICON_KEY
  $URL_KEY
  $OPENS_KEY
  <key>CFBundleExecutable</key><string>modus</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>1.0</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>LSMinimumSystemVersion</key><string>14.0</string>
  <key>NSPrincipalClass</key><string>NSApplication</string>
  <key>NSHighResolutionCapable</key><true/>
  <key>NSMicrophoneUsageDescription</key><string>Listen: the desk hears what you say.</string>
  <key>NSSpeechRecognitionUsageDescription</key><string>Listen: what you say becomes text.</string>
  <key>NSBluetoothAlwaysUsageDescription</key><string>Chat: messages travel to nearby devices over Bluetooth.</string>
  <key>NSCameraUsageDescription</key><string>Camera: photos and video, taken here.</string>
  <key>NSMotionUsageDescription</key><string>Your head, followed through your headphones, so a sound stays where it is as you turn.</string>
</dict>
</plist>
PLIST
    ;;
  *)
    echo "build-macos.sh: kind must be cli or gui, not $KIND" >&2
    exit 2
    ;;
esac

# shellcheck disable=SC2086  # WHERE, DEFS, EXTRA_SRC, FRAMEWORKS and LINKX are flag lists
if [ "$KIND" = gui ]; then
  # THE OBJECTS, then THE LINK, as build-ios.sh does: clang compiles each C and Objective-C file; the Swift files
  # (host/macos/swift/, with the phone's Term.swift: the wire format) compile to one object; swiftc links them,
  # because it brings the Swift runtime.  The image's linker flags become swiftc's -Xlinker options.
  OBJS=$(mktemp -d)
  for SRC in "$HERE/modus-shim.c" "$HERE/modus-audio.c" "$HERE/modus-keychain.m" $EXTRA_SRC "$HERE/syscall-stub.S"; do
    # the same minimum macOS as the Swift (and Info.plist), or ld warns that every object is "built for newer"
    cc -O2 -Wall -fobjc-arc -mmacosx-version-min=14.0 $DEFS -c "$SRC" -o "$OBJS/$(basename "$SRC").o"
  done
  swiftc -target arm64-apple-macos14.0 -swift-version 5 -O -wmo -parse-as-library -module-name ModusSwift \
    -import-objc-header "$HERE/swift/bridge.h" -emit-object -o "$OBJS/swift.o" \
    "$HERE/../ios/swift/Term.swift" "$HERE/../ios/swift/GPU.swift" "$HERE/../ios/swift/Speech.swift" \
    "$HERE/../ios/swift/Bluetooth.swift" "$HERE/../ios/swift/Files.swift" "$HERE/../ios/swift/Camera.swift" \
    "$HERE/../ios/swift/Microphone.swift" "$HERE/../ios/swift/NowPlaying.swift" \
    "$HERE/../ios/swift/Gamepad.swift" "$HERE/../ios/swift/Spatial.swift" "$HERE/../ios/swift/Notify.swift" \
    "$HERE/../ios/swift/Motion.swift" "$HERE/../ios/swift/Link.swift" \
    "$HERE"/swift/*.swift
  LINKX=""
  for W in $WHERE; do
    case $W in
      *.s)   cc -c "$W" -o "$OBJS/$(basename "$W").o" ;;
      -Wl,*) LINKX="$LINKX $(echo "$W" | sed -e 's/^-Wl,/-Xlinker /' -e 's/,/ -Xlinker /g')" ;;
      *)     LINKX="$LINKX $W" ;;
    esac
  done
  swiftc -target arm64-apple-macos14.0 -o "$BIN" "$OBJS"/*.o $FRAMEWORKS $LINKX
  rm -rf "$OBJS"
else
  cc -O2 -Wall -fobjc-arc $DEFS -o "$BIN" "$HERE/modus-shim.c" "$HERE/modus-audio.c" "$HERE/modus-keychain.m" $EXTRA_SRC "$HERE/syscall-stub.S" $FRAMEWORKS $WHERE
fi
# THE SIGNATURE: MODUS_MAC_SIGN, a codesigning identity ("-": ad hoc); by default this Mac's Apple Development
# identity if it has one.  Signed by a team, the app is the same app to macOS from one build to the next, and a
# permission given to it (the microphone, the camera) is kept; signed ad hoc, it is asked again after every build.
SIGN=${MODUS_MAC_SIGN:-$(security find-identity -v -p codesigning 2>/dev/null | awk '/Apple Development/ { print $2; exit }')}
SIGN=${SIGN:--}
codesign --force --sign "$SIGN" "$BIN" >/dev/null 2>&1 || codesign --force --sign - "$BIN" >/dev/null 2>&1 || true
if [ "$KIND" = gui ]; then
  codesign --force --sign "$SIGN" "$OUT" >/dev/null 2>&1 || codesign --force --sign - "$OUT" >/dev/null 2>&1 || true
fi
echo "wrote $OUT ($KIND, $(wc -c < "$BIN" | tr -d ' ') bytes in the executable)"
