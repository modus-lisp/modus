#!/bin/sh
# build-ios.sh IMAGE.elf OUT.app [sim|device|tvsim|tvdevice] [SIGN-IDENTITY] [PROFILE.mobileprovision]
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
# MODUS_IOS_ROTATES=1: the app turns with the phone (portrait and both landscapes); otherwise iOS's default.
# tvsim and tvdevice: the same app for an Apple TV (tvOS, its Simulator or a device): the desk on the TV,
# driven by the remote; the phone's native controls, pickers and dictation are not there (#if os(tvOS)).
# MODUS_IOS_ICON=PNG: the app's icon, 1024x1024, square (iOS rounds it); MODUS_IOS_ICON_DARK=PNG its dark one.
# On a TV the same square icon becomes its layered icon and Top Shelf banner (tv-icon.swift).
set -eu
IMAGE=$1; OUT=$2; KIND=${3:-sim}; IDENT=${4:-}; PROFILE=${5:-}
HERE=$(cd "$(dirname "$0")" && pwd)
MAC=$HERE/../macos
case $KIND in
  sim)    SDKN=iphonesimulator; TARGET=arm64-apple-ios18.0-simulator ;;
  device) SDKN=iphoneos;        TARGET=arm64-apple-ios18.0 ;;
  tvsim)    SDKN=appletvsimulator; TARGET=arm64-apple-tvos18.0-simulator ;;
  tvdevice) SDKN=appletvos;        TARGET=arm64-apple-tvos18.0 ;;
  *) echo "kind must be sim, device, tvsim or tvdevice" >&2; exit 2 ;;
esac
TV=0; case $KIND in tv*) TV=1 ;; esac
# the frameworks only the phone has: Speech's recognizer, Quick Look
PHONE_FW="-framework Speech -framework QuickLook"
# and the TV's: AVKit, whose UIWindow.avDisplayManager (a category, so nothing pulls it in) sets the TV's mode
[ $TV = 1 ] && PHONE_FW="-framework AVKit -framework CoreMedia"
case $KIND in
  sim) PLATFORM=iPhoneSimulator ;; device) PLATFORM=iPhoneOS ;;
  tvsim) PLATFORM=AppleTVSimulator ;; tvdevice) PLATFORM=AppleTVOS ;;
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
# THE OBJECTS, then THE LINK.  clang compiles each C and Objective-C file; the Swift files (swift/,
# one module) compile to one object; swiftc links them all, because it brings the Swift runtime.
OBJS=$OUT/obj
mkdir -p "$OBJS"
for SRC in "$MAC/modus-shim.c" "$MAC/modus-audio.c" "$MAC/syscall-stub.S" "$HERE/modus-ui.m" \
           "$HERE/modus-objc.m" "$HERE/modus-swift.m" "$MAC/modus-keychain.m"; do
  xcrun --sdk $SDKN clang -target $TARGET -isysroot "$SDK" -O2 -Wall -fobjc-arc -c "$SRC" -o "$OBJS/$(basename "$SRC").o"
done
if ls "$HERE"/swift/*.swift >/dev/null 2>&1; then
  xcrun --sdk $SDKN swiftc -target $TARGET -sdk "$SDK" -swift-version 5 -wmo -parse-as-library \
    -module-name ModusSwift -import-objc-header "$HERE/swift/bridge.h" -emit-object \
    -o "$OBJS/swift.o" "$HERE"/swift/*.swift
fi
# the image's section, WHERE: its linker flags (as swiftc's -Xlinker options), and for MODUS_IN_PLACE
# the assembly file it comes with, which clang assembles (swiftc takes no .s input)
LINKX=""
for W in $WHERE; do
  case $W in
    *.s)   xcrun --sdk $SDKN clang -target $TARGET -c "$W" -o "$OBJS/$(basename "$W").o" ;;
    -Wl,*) LINKX="$LINKX $(echo "$W" | sed -e 's/^-Wl,/-Xlinker /' -e 's/,/ -Xlinker /g')" ;;
    *)     LINKX="$LINKX $W" ;;
  esac
done
# THE SIMULATOR'S ENTITLEMENTS go in a section of the binary, as Xcode puts them, not in the ad-hoc
# signature (an ad-hoc signature that carries them is not launched): without an application identifier
# and its keychain group, every keychain write is refused (errSecMissingEntitlement), so nothing the app
# keeps there -- an SSH key, a password -- outlives the call that made it.
case $KIND in *sim) SIMSIG=1 ;; *) SIMSIG=0 ;; esac
if [ $SIMSIG = 1 ]; then
  SIMENT=$OBJS/sim.entitlements
  APPID=SIMULATOR.${MODUS_IOS_BUNDLE_ID:-org.modus-lisp.modus}
  cat > "$SIMENT" <<ENT
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>application-identifier</key><string>$APPID</string>
  <key>keychain-access-groups</key><array><string>$APPID</string></array>
</dict></plist>
ENT
  LINKX="$LINKX -Xlinker -sectcreate -Xlinker __TEXT -Xlinker __entitlements -Xlinker $SIMENT"
fi
# shellcheck disable=SC2086  # LINKX is a flag list
xcrun --sdk $SDKN swiftc -target $TARGET -sdk "$SDK" -o "$OUT/modus" "$OBJS"/*.o \
  -framework AudioToolbox -framework AVFoundation -framework UIKit -framework QuartzCore \
  -framework CoreGraphics -framework CoreFoundation -framework Security -framework Metal $PHONE_FW $LINKX
cat > "$OUT/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>CFBundleIdentifier</key><string>${MODUS_IOS_BUNDLE_ID:-org.modus-lisp.modus}</string>
  <key>CFBundleExecutable</key><string>modus</string>
  <key>CFBundleName</key><string>${MODUS_IOS_NAME:-modus}</string>
  <key>CFBundleDisplayName</key><string>${MODUS_IOS_NAME:-modus}</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleVersion</key><string>1</string>
  <key>CFBundleShortVersionString</key><string>0.1</string>
  <key>MinimumOSVersion</key><string>18.0</string>
  <key>LSRequiresIPhoneOS</key><true/>
  <key>NSMicrophoneUsageDescription</key><string>modus listens when you ask it to: the Listen app transcribes what it hears.</string>
  <key>NSCameraUsageDescription</key><string>${MODUS_IOS_NAME:-modus} takes photos and video when you ask it to.</string>
  <key>NSLocationWhenInUseUsageDescription</key><string>${MODUS_IOS_NAME:-modus} uses where you are when you ask: the sun's place in your sky, say.</string>
  <key>NSMotionUsageDescription</key><string>${MODUS_IOS_NAME:-modus} follows your head through your headphones, so a sound stays where it is as you turn.</string>
$( [ "${MODUS_IOS_ROTATES:-0}" != 0 ] && printf '%s\n' \
  '<key>UISupportedInterfaceOrientations</key><array><string>UIInterfaceOrientationPortrait</string><string>UIInterfaceOrientationLandscapeLeft</string><string>UIInterfaceOrientationLandscapeRight</string></array>' )
  <key>NSLocalNetworkUsageDescription</key><string>${MODUS_IOS_NAME:-modus} connects to computers on your network and on your VPN: the servers you add, over SSH.</string>
  <key>NSBluetoothAlwaysUsageDescription</key><string>${MODUS_IOS_NAME:-modus} talks with phones nearby over Bluetooth: a mesh chat, phone to phone.</string>
  <key>NSSpeechRecognitionUsageDescription</key><string>The Listen app turns what you say into text.</string>
  <key>UILaunchScreen</key><dict/>
  <key>UIFileSharingEnabled</key><true/>
  <key>LSSupportsOpeningDocumentsInPlace</key><true/>
  <key>UIDeviceFamily</key><array>$( [ $TV = 1 ] && echo '<integer>3</integer>' || echo '<integer>1</integer><integer>2</integer>' )</array>
  <key>UIApplicationSceneManifest</key><dict>
    <key>UIApplicationSupportsMultipleScenes</key><false/>
    <key>UISceneConfigurations</key><dict>
      <key>UIWindowSceneSessionRoleApplication</key><array><dict>
        <key>UISceneConfigurationName</key><string>Default</string>
        <key>UISceneDelegateClassName</key><string>ModusSceneDelegate</string>
      </dict></array>
    </dict>
  </dict>
  <key>CFBundleSupportedPlatforms</key><array><string>$PLATFORM</string></array>
</dict></plist>
PLIST
# THE ICON: an asset catalog of one 1024-point image (and its dark one), compiled as Xcode compiles it --
# Assets.car and the icon's keys, which go into the Info.plist
if [ -n "${MODUS_IOS_ICON:-}" ] && [ $TV = 1 ]; then
  # A TV'S ICON is a stack of layers the system tilts under the remote's thumb, in a brand-assets set with the Top
  # Shelf's banners: tv-icon.swift makes them all from the same square icon (a TV has no dark mode for icons)
  rm -rf "$OBJS/Assets.xcassets"
  xcrun swift "$(dirname "$0")/tv-icon.swift" "$MODUS_IOS_ICON" "$OBJS/Assets.xcassets"
  xcrun actool --compile "$OUT" --platform $SDKN --minimum-deployment-target 18.0 \
    --target-device tv --app-icon "App Icon & Top Shelf Image" \
    --output-partial-info-plist "$OBJS/icon.plist" "$OBJS/Assets.xcassets" > "$OBJS/actool.log"
  /usr/libexec/PlistBuddy -c "Merge $OBJS/icon.plist" "$OUT/Info.plist" > /dev/null
  rm -rf "$OBJS/Assets.xcassets"          # the sources, megabytes of banners: Assets.car has them
elif [ -n "${MODUS_IOS_ICON:-}" ]; then
  SET=$OBJS/Assets.xcassets/AppIcon.appiconset
  mkdir -p "$SET"
  cp "$MODUS_IOS_ICON" "$SET/icon.png"
  DARK=""
  if [ -n "${MODUS_IOS_ICON_DARK:-}" ]; then
    cp "$MODUS_IOS_ICON_DARK" "$SET/icon-dark.png"
    DARK=',{"filename":"icon-dark.png","idiom":"universal","platform":"ios","size":"1024x1024",
            "appearances":[{"appearance":"luminosity","value":"dark"}]}'
  fi
  cat > "$SET/Contents.json" <<JSON
{"images":[{"filename":"icon.png","idiom":"universal","platform":"ios","size":"1024x1024"}$DARK],
 "info":{"author":"xcode","version":1}}
JSON
  echo '{"info":{"author":"xcode","version":1}}' > "$OBJS/Assets.xcassets/Contents.json"
  xcrun actool --compile "$OUT" --platform $SDKN --minimum-deployment-target 18.0 \
    --target-device iphone --target-device ipad --app-icon AppIcon \
    --output-partial-info-plist "$OBJS/icon.plist" "$OBJS/Assets.xcassets" > "$OBJS/actool.log"
  /usr/libexec/PlistBuddy -c "Merge $OBJS/icon.plist" "$OUT/Info.plist" > /dev/null
fi
for f in ${MODUS_IOS_FILES:-}; do cp -R "$f" "$OUT/"; done
# THE WATCH APP that goes with it (MODUS_IOS_WATCH_APP, already signed: build-watch.sh), in Watch/, where the phone
# looks for its companion -- installed beside it on the paired watch, and linked to it (WatchConnectivity)
if [ -n "${MODUS_IOS_WATCH_APP:-}" ]; then mkdir -p "$OUT/Watch"; cp -R "$MODUS_IOS_WATCH_APP" "$OUT/Watch/"; fi
if [ -n "$PROFILE" ]; then cp "$PROFILE" "$OUT/embedded.mobileprovision"; fi
if [ -n "$IDENT" ]; then
  ENT=$(mktemp); security cms -D -i "$PROFILE" > "$ENT.plist" 2>/dev/null && \
    /usr/libexec/PlistBuddy -x -c "Print :Entitlements" "$ENT.plist" > "$ENT" || echo '<plist version="1.0"><dict/></plist>' > "$ENT"
  codesign --force --sign "$IDENT" --entitlements "$ENT" "$OUT"
else
  codesign --force --sign - "$OUT"
fi
echo "wrote $OUT"
