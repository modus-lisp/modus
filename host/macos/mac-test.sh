#!/bin/sh
# mac-test.sh MODUS [CONTROL] -- the first run of a Linux-built macOS CLI on a real Mac.
#
#   host/macos/mac-test.sh ./modus                  # built by mvm/build-macos-cli.lisp
#   host/macos/mac-test.sh ./modus ./modus-ld       # ... and build-macos.sh's, as a control
#
# Run it from a modus checkout (it uses test/macho-e2e-script.lisp).  It stops at the first
# step that fails and says what that step rules out.  With a CONTROL binary (made on the Mac
# by `host/macos/build-macos.sh IMAGE modus-ld cli` from the SAME image), every runtime step
# also runs against it, so a difference is the host code's and not the image's.
# On a failure, rerun the failing command with MODUS_SHIM_STRACE=1 for every translated call.
set -u
BIN=$1
CONTROL=${2:-}
HERE=$(cd "$(dirname "$0")/../.." && pwd)
SCRIPT=$HERE/test/macho-e2e-script.lisp
TMP=$(mktemp -d)
fail() { echo "mac-test: FAIL at step $1"; echo "  $2"; exit 1; }
step() { echo; echo "== step $1: $2"; }

chmod +x "$BIN" ${CONTROL:+"$CONTROL"}
xattr -d com.apple.quarantine "$BIN" 2>/dev/null || true     # copied from another machine

step 1 "codesign -v --verbose=4 (Apple's own check of the ad-hoc signature)"
codesign -v --verbose=4 "$BIN" || fail 1 \
  "the signature is malformed or a page hash is wrong: the kernel would SIGKILL it.  Compare with: codesign -d --verbose=4 $BIN"
codesign -d --verbose=4 "$BIN" 2>&1 | grep -E 'Identifier|Format|CodeDirectory|Signature|CDHash|flags' || true

step 2 "the structure dyld sees (informational)"
otool -hl "$BIN" | grep -E 'cmd LC_|segname|filetype|flags' | head -40
command -v dyld_info >/dev/null && dyld_info -dependents -fixups "$BIN" | head -20

step 3 "./modus --eval '(print (+ 1 2))' --quit"
out=$("$BIN" --eval '(print (+ 1 2))' --quit </dev/null 2>"$TMP/err"); rc=$?
echo "rc=$rc out=[$(echo $out)]"; cat "$TMP/err"
[ "$rc" = 0 ] && [ "$(echo $out)" = 3 ] || fail 3 \
  "rc 137 (SIGKILL) before any output = the kernel refused it (signature or load commands: see Console.app, 'Code Signature Invalid'); 'dyld: ...' on stderr = a missing or misbound libSystem symbol; 'modus-shim: ...' = the host code's own startup report"

step 4 "--load of a file (files, directories, clocks, the collector, a caught fault, the JIT, a socket, threads)"
run_script() {
  MODUS_TEST_THREADS=1 MODUS_TEST_DIR=$TMP "$1" --script "$SCRIPT" </dev/null 2>&1
}
run_script "$BIN" | tee "$TMP/script.out"
grep -q "checks, 0 failed" "$TMP/script.out" || fail 4 \
  "a FAIL line names the subsystem; rerun with MODUS_SHIM_STRACE=1 to see each Linux call and its answer"

step 5 "a GC-heavy loop (hundreds of collections, MAP_FIXED / NOREPLACE paths)"
out=$("$BIN" --eval '(let ((k nil)) (dotimes (i 3000000) (push (make-array 64) k) (when (> (length k) 1000) (setq k nil))) (print :gc-ok))' --quit </dev/null 2>&1)
echo "$out" | tail -3
echo "$out" | grep -q GC-OK || fail 5 "collections fault or run out of memory"

step 6 "the REPL on stdin (read(2) on fd 0, a pipe)"
out=$(echo '(* 6 7)' | "$BIN" 2>&1)
echo "$out" | tail -2
echo "$out" | grep -q '> 42' || fail 6 "the REPL did not read and answer a form from stdin"
# (sb-ext:exit :code 42) exits 84 here -- and exits 84 from the Linux aarch64 CLI under
# qemu too: the aarch64 image hands exit_group a tagged code.  An image bug, not the host's.

if [ -n "$CONTROL" ]; then
  step 7 "the control (build-macos.sh, Apple's toolchain), same checks"
  run_script "$CONTROL" > "$TMP/control.out"
  diff "$TMP/script.out" "$TMP/control.out" && echo "identical output" || \
    echo "mac-test: the two binaries differ (above): the host code is the only difference between them"
  echo; echo "sizes:"; ls -l "$BIN" "$CONTROL"
fi
echo; echo "mac-test: PASS"
rm -rf "$TMP"
