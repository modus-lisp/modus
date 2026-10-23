#!/bin/bash
# build-matrix-compare.sh — compare two scripts/build-matrix.sh output dirs.
#
#   scripts/build-matrix-compare.sh BASE FIX
#
# For every build in BASE: both exit statuses, then every file the build left
# in its /tmp, by cmp.  Prints one line per build and exits non-zero if any
# build's status or any output differs, or a file is missing on one side.
set -u
[ $# -eq 2 ] || { echo "usage: $0 BASE FIX" >&2; exit 2; }
base=$(cd "$1" && pwd) fix=$(cd "$2" && pwd)
bad=0
for d in "$base"/*/; do
  n=$(basename "$d")
  [ -f "$d/rc" ] || continue
  if [ ! -f "$fix/$n/rc" ]; then echo "$n: MISSING in FIX"; bad=1; continue; fi
  rb=$(cat "$d/rc") rf=$(cat "$fix/$n/rc")
  files=$( (cd "$d/tmp" && find . -type f; cd "$fix/$n/tmp" && find . -type f) | sort -u)
  same=0 diff=0 note=
  for f in $files; do
    if [ ! -f "$d/tmp/$f" ] || [ ! -f "$fix/$n/tmp/$f" ]; then
      diff=$((diff + 1)); note="$note only-one-side:$f"
    elif cmp -s "$d/tmp/$f" "$fix/$n/tmp/$f"; then
      same=$((same + 1))
    else
      diff=$((diff + 1)); note="$note differs:$f"
    fi
  done
  if [ "$rb" != "$rf" ] || [ $diff -gt 0 ]; then
    bad=1; verdict=DIFF
  else
    verdict=same
  fi
  printf '%-20s %-4s rc %s/%s  files same=%d differ=%d%s\n' "$n" "$verdict" "$rb" "$rf" $same $diff "$note"
done
exit $bad
