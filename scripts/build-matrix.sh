#!/bin/bash
# build-matrix.sh — build every image in scripts/build-matrix.list.
#
#   scripts/build-matrix.sh [-j N] [-t TREE] OUTDIR [NAME ...]
#
# Each build runs in its own user+mount namespace with a PRIVATE /tmp
# (OUTDIR/NAME/tmp), so the dozen scripts that write fixed /tmp paths can run
# side by side and cannot touch the real /tmp.  The build's log is
# OUTDIR/NAME/build.log and its exit status OUTDIR/NAME/rc.  Whatever it left
# in its /tmp is its output.
#
# Use it in pairs to prove a build change is byte-identical:
#   scripts/build-matrix.sh -t ../wt-base  ../regwork/mx-base
#   scripts/build-matrix.sh                ../regwork/mx-fix
#   scripts/build-matrix-compare.sh ../regwork/mx-base ../regwork/mx-fix
#
# MODUS_BUILD_ID is pinned to 1: the CLI lineage otherwise bakes a fresh
# random core id into every build (%CORE-BUILD-ID), so no two of its images
# would ever compare equal.
#
# TREE defaults to the checkout this script is in.  NAMEs select lines of the
# list; none means all of them.
set -u
here=$(cd "$(dirname "$0")/.." && pwd)
tree=$here
jobs=16
while getopts "j:t:" o; do
  case $o in
    j) jobs=$OPTARG ;;
    t) tree=$(cd "$OPTARG" && pwd) ;;
    *) echo "usage: $0 [-j N] [-t TREE] OUTDIR [NAME ...]" >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))
[ $# -ge 1 ] || { echo "usage: $0 [-j N] [-t TREE] OUTDIR [NAME ...]" >&2; exit 2; }
mkdir -p "$1"
out=$(cd "$1" && pwd)
shift
list=$tree/scripts/build-matrix.list
[ -f "$list" ] || list=$here/scripts/build-matrix.list

entries=$(sed -e 's/#.*//' "$list" | awk -F'|' 'NF==3 {
  gsub(/^ +| +$/, "", $1); gsub(/^ +| +$/, "", $2); gsub(/^ +| +$/, "", $3);
  print $1 "|" $2 "|" $3 }')
if [ $# -gt 0 ]; then
  want=" $* "
  entries=$(echo "$entries" | while IFS='|' read -r n e c; do
    case "$want" in *" $n "*) echo "$n|$e|$c" ;; esac; done)
fi
[ -n "$entries" ] || { echo "no matching builds" >&2; exit 2; }

build_one() {
  local name=$1 envs=$2 cmd=$3 d=$out/$1
  rm -rf "$d"; mkdir -p "$d/tmp"
  [ "$envs" = "-" ] && envs=
  local t0=$(date +%s)
  # shellcheck disable=SC2086
  unshare -rm sh -c "mount --bind '$d/tmp' /tmp && mkdir -p /tmp/piboot && cd '$tree' && exec env MODUS_BUILD_ID=1 $envs \
      sbcl --dynamic-space-size 12288 --script $cmd" < /dev/null > "$d/build.log" 2>&1
  local rc=$?
  echo $rc > "$d/rc"
  printf '%-20s rc=%-3s %4ss\n' "$name" "$rc" $(( $(date +%s) - t0 ))
}

running=0
while IFS='|' read -r name envs cmd; do
  build_one "$name" "$envs" "$cmd" &
  running=$((running + 1))
  if [ $running -ge "$jobs" ]; then wait -n; running=$((running - 1)); fi
done <<< "$entries"
wait
