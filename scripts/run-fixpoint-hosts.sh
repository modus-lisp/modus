#!/usr/bin/env bash
# run-fixpoint-hosts.sh — the self-host fixpoint across THREE host Lisps.
#
# Builds the self-hosting Modus image (mvm/build-modus-selfhost.lisp → modus-sh)
# with SBCL, CCL, and ABCL, then has EACH modus-sh `--compile` the same program
# to a standalone bare-metal binary.  The three host-built modus-sh images differ
# (~0.3%, host codegen), but the program they COMPILE is produced by Modus's own
# in-image compiler — so the `--compile` output must be BYTE-IDENTICAL across all
# three hosts.  That byte-identity is the fixpoint: the host washes out once Modus
# is compiling.  QEMU-free.
#
# Each compiled output is also run (expects "YYYYYY" — six in-image bignum
# checks) and `--compile` is repeated to confirm per-host reproducibility.
#
#   scripts/run-fixpoint-hosts.sh [hosts...]     # default: sbcl ccl abcl
#
# Env: SBCL (default sbcl), CCL (default $HOME/ccl/lx86cl64),
#      ABCL_JAR (default $HOME/abcl-bin-1.9.2/abcl.jar), DSS (SBCL heap MB, 12288).
# CCL/ABCL are skipped (with a note) if their runtime is not installed.
set -uo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
cd "$here"

HOSTS=("$@"); [ ${#HOSTS[@]} -eq 0 ] && HOSTS=(sbcl ccl abcl)
SELFHOST=mvm/build-modus-selfhost.lisp
DSS="${DSS:-12288}"
SBCL="${SBCL:-sbcl}"
CCL="${CCL:-$HOME/ccl/lx86cl64}"
ABCL_JAR="${ABCL_JAR:-$HOME/abcl-bin-1.9.2/abcl.jar}"
WORK="$here/tmp/fixpoint-hosts"; mkdir -p "$WORK"

# The program each modus-sh will --compile (self-contained: kernel-main only).
PROG="$WORK/prog.lisp"
cat > "$PROG" <<'LISP'
(defun putc (c) (write-char-serial c))
(defun sxit (code) (syscall3 60 code 0 0))
(defun kernel-main ()
  (if (= (- 4611686018427387903 4611686018427387902) 1) (putc 89) (putc 78))
  (if (= (+ 2305843009213693952 2305843009213693951) 4611686018427387903) (putc 89) (putc 78))
  (if (= (+ -4611686018427387903 4611686018427387903) 0) (putc 89) (putc 78))
  (if (= (ash 2305843009213693952 -1) 1152921504606846976) (putc 89) (putc 78))
  (if (= (ash 4611686018427387903 -61) 1) (putc 89) (putc 78))
  (if (= (* 2305843009213693951 2) 4611686018427387902) (putc 89) (putc 78))
  (putc 10) (sxit 0))
LISP

# available HOST -> 0/1 (and reason)
have() {
  case "$1" in
    sbcl) command -v "$SBCL" >/dev/null;;
    ccl)  [ -x "$CCL" ];;
    abcl) [ -f "$ABCL_JAR" ];;
    *) return 1;;
  esac
}

# build_modus_sh HOST OUTPATH  -> builds modus-sh for HOST at OUTPATH
build_modus_sh() {
  local host="$1" out="$2" log="$WORK/build-$1.log"
  echo "  [$host] building modus-sh ($SELFHOST) ..." >&2
  case "$host" in
    sbcl)
      MODUS_CLI_OUT="$out" "$SBCL" --dynamic-space-size "$DSS" \
        --script "$SELFHOST" >"$log" 2>&1 ;;
    ccl)
      # CCL/ABCL ignore MODUS_CLI_OUT (it is #+sbcl-only) → they write ./modus.
      rm -f "$here/modus"
      CCL="$CCL" ./scripts/build-ccl.sh "$SELFHOST" >"$log" 2>&1
      [ -f "$here/modus" ] && mv "$here/modus" "$out" ;;
    abcl)
      rm -f "$here/modus"
      ABCL_JAR="$ABCL_JAR" ./scripts/build-abcl.sh "$SELFHOST" >"$log" 2>&1
      [ -f "$here/modus" ] && mv "$here/modus" "$out" ;;
  esac
  [ -f "$out" ] || { echo "  [$host] BUILD FAILED (see $log)" >&2; return 1; }
  chmod +x "$out"
  echo "  [$host] modus-sh: $(wc -c <"$out") bytes" >&2
}

declare -A MD5 RUN SIZE
BUILT=()
for host in "${HOSTS[@]}"; do
  if ! have "$host"; then
    echo "  [$host] SKIP — runtime not installed" >&2; continue
  fi
  out="$WORK/modus-sh-$host"
  if build_modus_sh "$host" "$out"; then
    SIZE[$host]=$(wc -c <"$out")
    # --compile twice → per-host reproducibility, and capture the artifact md5.
    "$out" --compile "$PROG" "$WORK/out-$host.a" >/dev/null 2>&1
    "$out" --compile "$PROG" "$WORK/out-$host.b" >/dev/null 2>&1
    a=$(md5sum "$WORK/out-$host.a" | cut -d' ' -f1)
    b=$(md5sum "$WORK/out-$host.b" | cut -d' ' -f1)
    if [ "$a" != "$b" ]; then
      echo "  [$host] NON-REPRODUCIBLE across two --compile runs ($a != $b)" >&2
    fi
    MD5[$host]=$a
    chmod +x "$WORK/out-$host.a"
    RUN[$host]=$("$WORK/out-$host.a" 2>/dev/null | head -1)
    BUILT+=("$host")
  fi
done

echo ""
echo "=== self-host fixpoint across hosts ==="
printf "%-6s  %-12s  %-32s  %s\n" HOST "modus-sh" "--compile md5" "output"
ref=""
for host in "${BUILT[@]}"; do
  printf "%-6s  %-12s  %-32s  %s\n" "$host" "${SIZE[$host]}" "${MD5[$host]}" "${RUN[$host]}"
  [ -z "$ref" ] && ref="${MD5[$host]}"
done

echo ""
fail=0
[ ${#BUILT[@]} -lt 2 ] && { echo "NEED >=2 hosts to compare (built: ${BUILT[*]:-none})"; exit 1; }
for host in "${BUILT[@]}"; do
  [ "${MD5[$host]}" = "$ref" ] || { echo "FAIL: $host --compile md5 differs from ${BUILT[0]}"; fail=1; }
  [ "${RUN[$host]}" = "YYYYYY" ] || { echo "FAIL: $host compiled output printed '${RUN[$host]}' (expected YYYYYY)"; fail=1; }
done
if [ "$fail" = 0 ]; then
  echo "PASS: ${#BUILT[@]} hosts (${BUILT[*]}) produce BYTE-IDENTICAL --compile output"
  echo "      (md5 $ref) and it runs correctly — the host washes out (fixpoint)."
fi
exit $fail
