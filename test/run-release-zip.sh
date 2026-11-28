#!/bin/sh
# test/run-release-zip.sh ZIP WORKDIR -- a release zip (scripts/make-release-zip.lisp),
# checked as a user gets it: unzipped by Info-ZIP unzip (UNZIP_BIN, default unzip), the
# layout and modes looked at, ./repl.sh made to evaluate a form, ./desk.sh --help made to
# print the desk's usage.  Starts no listener: --help exits before anything binds.
# Last line "RELEASE-ZIP PASS" or "RELEASE-ZIP FAIL".
set -u
Z=${1:?usage: test/run-release-zip.sh ZIP WORKDIR}; W=${2:?usage: test/run-release-zip.sh ZIP WORKDIR}
UNZIP_BIN=${UNZIP_BIN:-unzip}
fail=0
bad() { echo "FAIL: $*"; fail=1; }
rm -rf "$W"; mkdir -p "$W"
"$UNZIP_BIN" -tq "$Z" || bad "unzip -t"
(cd "$W" && "$UNZIP_BIN" -q "$Z") || bad "unzip"
top=$(cd "$W" && ls)
[ "$(echo "$top" | wc -l)" = 1 ] || bad "more than one top directory: $top"
D="$W/$top"
echo "top: $top"
for f in modus-linux-x64 modus-macos-arm64 desk.sh repl.sh desk.command repl.command; do
  [ -x "$D/$f" ] || bad "$f is not executable"
done
for f in modus-windows-x64.exe desk.cmd repl.cmd README.txt src/modus/scripts/glass-desk.lisp src/glass/glass.asd src/cram/src/zip.lisp; do
  [ -f "$D/$f" ] || bad "no $f"
done
[ -x "$D/src/modus/scripts/run.sh" ] || bad "a source file lost its execute bit (src/modus/scripts/run.sh)"
[ -x "$D/src/modus/scripts/source-set.lisp" ] && bad "a source file gained an execute bit"
grep -q "$(echo "$top" | sed 's/^modus-//')" "$D/README.txt" || bad "README.txt does not name the release"
echo "src: $(ls "$D/src" | wc -l) repositories, $(find "$D/src" -type f | wc -l) files"

out=$(cd /; echo '(print (+ 1 2))' | "$D/repl.sh" 2>&1)
echo "$out" | grep -qx '3\|3 ' || echo "$out" | grep -q '^3' || bad "repl.sh, stdin: $out"
out=$(cd /; "$D/repl.sh" --eval '(progn (print (* 6 7)) (sb-ext:exit))' < /dev/null 2>&1)
echo "$out" | grep -q '42' || bad "repl.sh --eval: $out"
echo "repl.sh: (+ 1 2) and --eval (* 6 7) answered"

out=$(cd /; "$D/desk.sh" --help 2>&1); rc=$?
echo "$out" | grep -q -- '--web-port' && [ $rc = 0 ] || bad "desk.sh --help (rc $rc): $out"
echo "desk.sh --help: rc $rc, $(echo "$out" | grep -c -- '^  --') flags"
out=$(cd /; "$D/desk.sh" --no-such-flag 2>&1); rc=$?
[ $rc = 2 ] || bad "desk.sh --no-such-flag: rc $rc"

[ $fail = 0 ] && echo "RELEASE-ZIP PASS" || echo "RELEASE-ZIP FAIL"
exit $fail
