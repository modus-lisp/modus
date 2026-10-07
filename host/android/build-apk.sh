#!/bin/sh
# build-apk.sh IMAGE.elf OUT.apk [SCRIPT.lisp]
#
# Wrap an aarch64 hosted-Linux modus image (mvm/build-aarch64-cli.lisp) in an
# Android app: a NativeActivity whose launcher (modus-launcher.c) runs the image
# as a child process and gives it a screen and touch (see that file for the
# protocol).  SCRIPT (default host/android/draw.lisp) is embedded and run with
# --script.  Debug-signed with a key kept in $MODUS_ANDROID_KEYSTORE.
#
# MODUS_ANDROID_ASSETS=DIR packs DIR's files as assets, which the app extracts
# into its data dir on the first start of each build.  A modus.args there (one
# argument per line, "@NAME" for NAME in the data dir) replaces --script SCRIPT,
# e.g. "--core / @kiln.core / --eval / (kiln-android-main)" (kiln android).
#
# Needs, all from Google's SDK repository, x86-64 Linux only:
#   ANDROID_BUILD_TOOLS  a build-tools dir (aapt2, zipalign, apksigner)
#   ANDROID_JAR          platforms/android-NN/android.jar (NN >= 30)
#   NDK_SYSROOT          the NDK's toolchains/llvm/prebuilt/linux-x86_64/sysroot
#   CLANG                any clang that targets aarch64 (default: clang)
# Only the sysroot is needed from the NDK; a system clang and lld link against
# it.  Install with: adb install -r OUT.apk
set -eu
IMAGE=$1; OUT=$2
HERE=$(cd "$(dirname "$0")" && pwd)
SCRIPT=${3:-$HERE/draw.lisp}
: "${ANDROID_BUILD_TOOLS:?}" "${ANDROID_JAR:?}" "${NDK_SYSROOT:?}"
CLANG=${CLANG:-clang}
API=30
KS=${MODUS_ANDROID_KEYSTORE:-$HOME/.android/modus-debug.keystore}
W=$(mktemp -d); trap 'rm -rf "$W"' EXIT

# The script, embedded in the launcher.
python3 - "$SCRIPT" "$W/modus-script.h" <<'PY'
import sys, os
data = open(sys.argv[1], 'rb').read()
name = os.path.basename(sys.argv[1])
with open(sys.argv[2], 'w') as h:
    h.write('#define MODUS_SCRIPT_NAME "%s"\nstatic const unsigned char modus_script[] = {\n' % name)
    for i in range(0, len(data), 16):
        h.write(''.join('%d,' % b for b in data[i:i+16]) + '\n')
    h.write('};\nstatic const unsigned long modus_script_len = %d;\n' % len(data))
PY

# The launcher: link by hand against the NDK sysroot (crt objects, libc,
# libandroid, liblog), so no NDK clang or compiler-rt is needed.
L=$NDK_SYSROOT/usr/lib/aarch64-linux-android/$API
mkdir -p "$W/apk/lib/arm64-v8a"
"$CLANG" --target=aarch64-linux-android$API --sysroot="$NDK_SYSROOT" -fuse-ld=lld \
  -O2 -fPIC -shared -nostdlib -Wall -I"$W" \
  -o "$W/apk/lib/arm64-v8a/libmodusapp.so" \
  "$L/crtbegin_so.o" "$HERE/modus-launcher.c" -L"$L" -landroid -laaudio -llog -ldl -lc "$L/crtend_so.o" \
  -Wl,-soname,libmodusapp.so -Wl,-z,max-page-size=16384
cp "$IMAGE" "$W/apk/lib/arm64-v8a/libmodus.so"

# Assets: the caller's files, the list of them, and a build id (the stamp the
# launcher compares before extracting them again).
mkdir -p "$W/assets"
if [ -n "${MODUS_ANDROID_ASSETS:-}" ]; then
  cp -R "$MODUS_ANDROID_ASSETS"/. "$W/assets/"
  (cd "$W/assets" && find . -type f ! -name modus.args ! -name files.list | sed 's|^\./||' | sort > files.list)
  date -u +%Y%m%dT%H%M%SZ-$$ > "$W/assets/build.id"
fi

# Manifest -> binary XML (and the assets), then the native libraries alongside.
"$ANDROID_BUILD_TOOLS/aapt2" link -o "$W/base.apk" --manifest "$HERE/AndroidManifest.xml" \
  -I "$ANDROID_JAR" --min-sdk-version $API --target-sdk-version 36 -A "$W/assets"
python3 - "$W/base.apk" "$W/apk" <<'PY'
import sys, os, zipfile
z = zipfile.ZipFile(sys.argv[1], 'a', zipfile.ZIP_DEFLATED)
root = sys.argv[2]
for d, _, fs in os.walk(root):
    for f in fs:
        p = os.path.join(d, f)
        z.write(p, os.path.relpath(p, root))
z.close()
PY
"$ANDROID_BUILD_TOOLS/zipalign" -f -P 16 4 "$W/base.apk" "$W/aligned.apk"

if [ ! -f "$KS" ]; then
  mkdir -p "$(dirname "$KS")"
  keytool -genkeypair -keystore "$KS" -storepass android -keypass android -alias modus \
    -keyalg RSA -keysize 2048 -validity 10000 -dname "CN=modus debug" >/dev/null 2>&1
fi
"$ANDROID_BUILD_TOOLS/apksigner" sign --ks "$KS" --ks-pass pass:android --ks-key-alias modus \
  --out "$OUT" "$W/aligned.apk"
echo "wrote $OUT ($(wc -c < "$OUT") bytes)"
