#!/usr/bin/env bash
# run-crosscompile-repro.sh — DIVERSE-DOUBLE-COMPILATION (DDC) / reproducible-
# build proof for the Modus cross-emit seeds.
#
# The ARM64 SEED (mvm/build-aarch64-cli.lisp) is a hosted Linux/AArch64 Modus
# image that can `--compile' a program to a hosted Linux/x64 ELF and
# `--compile-aarch64' it to a hosted Linux/AArch64 ELF — entirely in-image, no
# host Lisp in the cross-emit loop.
#
# DDC PROOF: build the SAME seed under each available host Lisp (SBCL, CCL,
# ABCL).  The per-host seed IMAGES differ (~host codegen), but the ELF each seed
# CROSS-COMPILES for a given target must be BYTE-IDENTICAL across hosts — because
# that ELF is produced by Modus's OWN in-image compiler, which no longer depends
# on the host once the seed is running.  Byte-identity across hosts = the seed's
# cross-compiled toolchain is host-independent (the trust property).
#
# What is compared (see the matrix + verdict):
#   * x64 target   : all per-host seeds must emit the SAME x64 ELF, and that md5
#                    must equal the known reference (8621f9d7…), and it must run
#                    (natively) printing YYYYYY.
#   * aarch64 target: all per-host seeds must emit the SAME aarch64 ELF, and it
#                    must run (via qemu-aarch64-static) printing YYYYYY.
#   * BONUS two-SEED cross-arch: the x64 SELF-HOST seed
#                    (mvm/build-modus-selfhost.lisp, SBCL) must emit the SAME x64
#                    ELF as the arm64 seed does — cross-ARCH reproducibility of
#                    the x64 target (the arm64 target across seeds differs by the
#                    deferred fn-align issue, so it is NOT compared here).
#
# Robust: an unavailable host or a failed build/compile is marked SKIP/FAIL and
# the run continues.  SBCL is the must-have column; CCL/ABCL are bonus.
#
#   scripts/run-crosscompile-repro.sh [hosts...]      # default: sbcl ccl abcl
#
# Env: SBCL (default sbcl), CCL (default $HOME/ccl/lx86cl64),
#      ABCL_JAR (default $HOME/abcl-bin-1.9.2/abcl.jar), DSS (SBCL heap MB, 12288),
#      QEMU (default qemu-aarch64-static), REPRO_TWO_SEED (default 1; 0 skips the
#      bonus x64-self-host seed build).
set -uo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
cd "$here"

HOSTS=("$@"); [ ${#HOSTS[@]} -eq 0 ] && HOSTS=(sbcl ccl abcl)
SEED=mvm/build-aarch64-cli.lisp          # the arm64 cross-emit seed
X64_SEED=mvm/build-modus-selfhost.lisp    # the x64 self-host seed (bonus row)
DSS="${DSS:-12288}"
SBCL="${SBCL:-sbcl}"
CCL="${CCL:-$HOME/ccl/lx86cl64}"
ABCL_JAR="${ABCL_JAR:-$HOME/abcl-bin-1.9.2/abcl.jar}"
QEMU="${QEMU:-qemu-aarch64-static}"
REPRO_TWO_SEED="${REPRO_TWO_SEED:-1}"
# The non-SBCL default output path build-aarch64-cli.lisp writes to (MODUS_CLI_OUT
# is #+sbcl-only, so CCL/ABCL land here); we mv it to a per-host path.
AA64_DEFAULT_OUT="/home/claude/modus-aa64-cli"
X64_REF="8621f9d7fb87ee88d04c6b380f2fb7d1"   # known x64-target reference md5

WORK="$here/tmp/crosscompile-repro"; mkdir -p "$WORK"

# The program every seed cross-compiles (self-contained: kernel-main only; six
# in-image bignum checks → prints YYYYYY).  Identical to run-fixpoint-hosts.sh.
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

have() {
  case "$1" in
    sbcl) command -v "$SBCL" >/dev/null;;
    ccl)  [ -x "$CCL" ];;
    abcl) [ -f "$ABCL_JAR" ];;
    *) return 1;;
  esac
}

# build_seed HOST OUTPATH BUILD_SCRIPT DEFAULT_OUT -> build BUILD_SCRIPT for HOST.
# SBCL honours MODUS_CLI_OUT; CCL/ABCL write DEFAULT_OUT (their #-sbcl default),
# which we then mv to OUTPATH.
# REPRO_REUSE_SEED=1 reuses an already-built seed at OUTPATH (dev-only fast path;
# default 0 = always rebuild, which is the actual DDC discipline).
build_seed() {
  local host="$1" out="$2" script="$3" default_out="$4"
  local log="$WORK/build-$host-$(basename "$script" .lisp).log"
  if [ "${REPRO_REUSE_SEED:-0}" = "1" ] && [ -x "$out" ]; then
    echo "  [$host] REUSING existing seed $out ($(wc -c <"$out") bytes)" >&2
    return 0
  fi
  echo "  [$host] building $(basename "$script") ..." >&2
  rm -f "$out"
  case "$host" in
    sbcl)
      MODUS_CLI_OUT="$out" "$SBCL" --dynamic-space-size "$DSS" \
        --script "$script" >"$log" 2>&1 ;;
    ccl)
      rm -f "$default_out"
      CCL="$CCL" ./scripts/build-ccl.sh "$script" >"$log" 2>&1
      [ -f "$default_out" ] && mv "$default_out" "$out" ;;
    abcl)
      rm -f "$default_out"
      ABCL_JAR="$ABCL_JAR" ./scripts/build-abcl.sh "$script" >"$log" 2>&1
      [ -f "$default_out" ] && mv "$default_out" "$out" ;;
  esac
  if [ ! -f "$out" ]; then
    echo "  [$host] BUILD FAILED — $(basename "$script") (see $log)" >&2
    return 1
  fi
  chmod +x "$out"
  echo "  [$host] $(basename "$script"): $(wc -c <"$out") bytes" >&2
}

# md5_of FILE -> md5 or "-" if missing
md5_of() { [ -f "$1" ] && md5sum "$1" | cut -d' ' -f1 || echo "-"; }

declare -A SEED_SIZE X64_MD5 X64_RUN AA64_MD5 AA64_RUN CELL
BUILT=()

for host in "${HOSTS[@]}"; do
  if ! have "$host"; then
    echo "  [$host] SKIP — runtime not installed" >&2
    X64_MD5[$host]="SKIP"; AA64_MD5[$host]="SKIP"
    continue
  fi
  seed="$WORK/seed-$host"
  if ! build_seed "$host" "$seed" "$SEED" "$AA64_DEFAULT_OUT"; then
    X64_MD5[$host]="BUILDFAIL"; AA64_MD5[$host]="BUILDFAIL"
    continue
  fi
  SEED_SIZE[$host]=$(wc -c <"$seed")

  # --- x64 target: seed runs under qemu, emits an x86-64 ELF, run NATIVELY ---
  x64out="$WORK/out-x64-$host.elf"; rm -f "$x64out"
  "$QEMU" "$seed" --compile "$PROG" "$x64out" >"$WORK/compile-x64-$host.log" 2>&1
  X64_MD5[$host]="$(md5_of "$x64out")"
  if [ -f "$x64out" ]; then
    chmod +x "$x64out"
    X64_RUN[$host]="$("$x64out" 2>/dev/null | head -1)"
  else
    X64_RUN[$host]="NO-OUTPUT"
  fi

  # --- aarch64 target: seed emits an aarch64 ELF, run via qemu ---
  aaout="$WORK/out-aa64-$host.elf"; rm -f "$aaout"
  "$QEMU" "$seed" --compile-aarch64 "$PROG" "$aaout" >"$WORK/compile-aa64-$host.log" 2>&1
  AA64_MD5[$host]="$(md5_of "$aaout")"
  if [ -f "$aaout" ]; then
    chmod +x "$aaout"
    AA64_RUN[$host]="$("$QEMU" "$aaout" 2>/dev/null | head -1)"
  else
    AA64_RUN[$host]="NO-OUTPUT"
  fi

  BUILT+=("$host")
done

# --- BONUS: two-SEED cross-arch x64 reproducibility (x64 self-host seed) ------
TWO_SEED_MD5="-"; TWO_SEED_RUN="-"; TWO_SEED_STATE="skipped"
if [ "$REPRO_TWO_SEED" = "1" ] && have sbcl && [ -f "$X64_SEED" ]; then
  xseed="$WORK/x64seed-sbcl"
  if [ "${REPRO_REUSE_SEED:-0}" = "1" ] && [ -x "$xseed" ]; then
    echo "  [bonus] REUSING existing x64 self-host seed $xseed" >&2
  else
  echo "  [bonus] building x64 self-host seed ($(basename "$X64_SEED"), MODUS_NO_JIT=1) ..." >&2
  MODUS_NO_JIT=1 MODUS_CLI_OUT="$xseed" "$SBCL" --dynamic-space-size "$DSS" \
        --script "$X64_SEED" >"$WORK/build-x64seed.log" 2>&1 || true
  fi
  if [ -f "$xseed" ]; then
    chmod +x "$xseed"
    # x64 self-host seed runs NATIVELY (it is an x86-64 ELF).
    xs_out="$WORK/out-x64-from-x64seed.elf"; rm -f "$xs_out"
    "$xseed" --compile "$PROG" "$xs_out" >"$WORK/compile-x64-x64seed.log" 2>&1
    TWO_SEED_MD5="$(md5_of "$xs_out")"
    if [ -f "$xs_out" ]; then chmod +x "$xs_out"; TWO_SEED_RUN="$("$xs_out" 2>/dev/null | head -1)"; fi
    TWO_SEED_STATE="built"
  else
    TWO_SEED_STATE="BUILDFAIL"
    echo "  [bonus] x64 self-host seed BUILD FAILED (see $WORK/build-x64seed.log)" >&2
  fi
fi

# ============================ MATRIX ==========================================
echo ""
echo "=== cross-compile reproducibility matrix (arm64 seed built per host) ==="
printf "%-6s  %-10s  %-34s  %-7s  %-34s  %-7s\n" \
  HOST "seed-size" "x64-target md5" "run" "aarch64-target md5" "run"
for host in "${HOSTS[@]}"; do
  printf "%-6s  %-10s  %-34s  %-7s  %-34s  %-7s\n" \
    "$host" "${SEED_SIZE[$host]:--}" \
    "${X64_MD5[$host]:--}" "${X64_RUN[$host]:--}" \
    "${AA64_MD5[$host]:--}" "${AA64_RUN[$host]:--}"
done
echo ""
echo "x64-target reference md5 (expected): $X64_REF"
echo "bonus two-seed (x64 self-host seed) x64-target md5: $TWO_SEED_MD5  run: $TWO_SEED_RUN  [$TWO_SEED_STATE]"

# ============================ VERDICT =========================================
echo ""
if [ ${#BUILT[@]} -eq 0 ]; then
  echo "VERDICT: no seed built on any host — cannot prove DDC."
  exit 1
fi
nhosts=${#BUILT[@]}
multi=0; [ "$nhosts" -ge 2 ] && multi=1

# (a) x64 target: identical across all built hosts, == reference, runs YYYYYY.
x64ref=""; x64_ident=1; x64_run_ok=1
for host in "${BUILT[@]}"; do
  [ -z "$x64ref" ] && x64ref="${X64_MD5[$host]}"
  [ "${X64_MD5[$host]}" = "$x64ref" ] || x64_ident=0
  [ "${X64_RUN[$host]}" = "YYYYYY" ] || x64_run_ok=0
done
x64_ref_ok=1; [ "$x64ref" = "$X64_REF" ] || x64_ref_ok=0
x64_pass=0
if [ "$x64_ident" = 1 ] && [ "$x64_run_ok" = 1 ] && [ "$x64_ref_ok" = 1 ]; then
  x64_pass=1
  echo "PASS  x64 target: $nhosts host(s) (${BUILT[*]}) emit BYTE-IDENTICAL x64 ELF"
  echo "                  md5 $x64ref == reference, runs YYYYYY."
else
  echo "FAIL  x64 target: ident=$x64_ident (md5 $x64ref) ref-match=$x64_ref_ok run-YYYYYY=$x64_run_ok"
fi

# Two-seed cross-arch x64 (bonus): the x64 self-host seed must also match ref.
x64_seed_ok=1
if [ "$TWO_SEED_STATE" = "built" ]; then
  if [ "$TWO_SEED_MD5" = "$X64_REF" ] && [ "$TWO_SEED_RUN" = "YYYYYY" ]; then
    echo "PASS  x64 target cross-SEED: x64 self-host seed emits the SAME x64 ELF ($X64_REF), runs YYYYYY."
  else
    x64_seed_ok=0
    echo "FAIL  x64 target cross-SEED: md5 $TWO_SEED_MD5 (ref $X64_REF) run $TWO_SEED_RUN"
  fi
fi

# (b) aarch64 target: identical across all built hosts, runs YYYYYY.
aaref=""; aa_ident=1; aa_run_ok=1
for host in "${BUILT[@]}"; do
  [ -z "$aaref" ] && aaref="${AA64_MD5[$host]}"
  [ "${AA64_MD5[$host]}" = "$aaref" ] || aa_ident=0
  [ "${AA64_RUN[$host]}" = "YYYYYY" ] || aa_run_ok=0
done
aa_pass=0
if [ "$aa_ident" = 1 ] && [ "$aa_run_ok" = 1 ]; then
  aa_pass=1
  echo "PASS  aarch64 target: $nhosts host(s) (${BUILT[*]}) emit BYTE-IDENTICAL aarch64 ELF"
  echo "                      md5 $aaref, runs YYYYYY (qemu-aarch64-static)."
else
  # Distinguish a run failure (real regression) from a byte-divergence with
  # every output still correct (the known host-dependent aarch64-emit leak).
  if [ "$aa_run_ok" = 1 ]; then
    echo "FAIL  aarch64 target: host-DEPENDENT — outputs differ across hosts but each runs"
    echo "                      YYYYYY (functionally correct, NOT byte-reproducible)."
    for host in "${BUILT[@]}"; do echo "                        $host: ${AA64_MD5[$host]}"; done
  else
    echo "FAIL  aarch64 target: ident=$aa_ident run-YYYYYY=$aa_run_ok (a compiled output MISBEHAVED)"
  fi
fi

# ---- final, per-target verdict --------------------------------------------
echo ""
echo "=== DDC VERDICT (built hosts: ${BUILT[*]}; $nhosts) ==="
if [ "$x64_pass" = 1 ] && [ "$x64_seed_ok" = 1 ] && [ "$multi" = 1 ]; then
  echo "x64 target     : PASS — host-INDEPENDENT (byte-identical across $nhosts hosts + the"
  echo "                 x64 self-host seed; md5 $X64_REF; runs YYYYYY)."
elif [ "$x64_pass" = 1 ]; then
  echo "x64 target     : PARTIAL — self-consistent + matches reference, but only $nhosts host"
  echo "                 built (need >=2 to PROVE host-independence)."
else
  echo "x64 target     : FAIL — see FAIL line above."
fi
if [ "$aa_pass" = 1 ] && [ "$multi" = 1 ]; then
  echo "aarch64 target : PASS — host-INDEPENDENT (byte-identical across $nhosts hosts)."
elif [ "$aa_run_ok" = 1 ]; then
  echo "aarch64 target : NOT host-independent — every host's output RUNS correctly (YYYYYY)"
  echo "                 but the bytes differ per host (host-state leak in the aarch64 emit;"
  echo "                 distinct from the fn-align cross-SEED issue).  Deferred."
else
  echo "aarch64 target : FAIL — a compiled output misbehaved (see above)."
fi

# Exit 0 only when BOTH targets are proven host-independent; else non-zero.
if [ "$x64_pass" = 1 ] && [ "$x64_seed_ok" = 1 ] && [ "$aa_pass" = 1 ] && [ "$multi" = 1 ]; then
  echo "OVERALL        : PASS (full DDC — both targets host-independent)."
  exit 0
elif [ "$x64_pass" = 1 ] && [ "$x64_seed_ok" = 1 ]; then
  echo "OVERALL        : x64 DDC PROVEN; aarch64 DDC not yet (host-dependent, deferred)."
  exit 3
else
  echo "OVERALL        : FAIL — x64 DDC not established (regression)."
  exit 1
fi
