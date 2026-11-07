#!/bin/sh
# vessel-checkout.sh — put modus-lisp/vessel at the commit in vessel.pin into .vessel/
#
#   scripts/vessel-checkout.sh [MODUS-DIR]
#
# modus is the Lisp; the bare-metal drivers, boot stubs and board runtime its kernel
# images are compiled from live in vessel, under kernel/.  A build finds them through
# lib/load-mvm.lisp's MODUS-SOURCE: MODUS_VESSEL if set, else this tree's .vessel/ (what
# this script makes), else a sibling ../vessel checkout in a workspace.
#
# The clone comes from MODUS_VESSEL_REPO (default github.com/modus-lisp/vessel) once,
# into a cache shared by every modus tree on the machine (MODUS_VESSEL_CACHE, default
# ~/.cache/modus/vessel.git), and each tree gets a detached worktree of it at its pin --
# so two refs with two pins (the acceptance gate's base and fix) never share a checkout.
set -eu
MODUS=${1:-$(cd "$(dirname "$0")/.." && pwd)}
PIN=$(sed -n '1{s/[^0-9a-f].*//;p;}' "$MODUS/vessel.pin")
[ -n "$PIN" ] || { echo "vessel-checkout: no commit in $MODUS/vessel.pin" >&2; exit 2; }
REPO=${MODUS_VESSEL_REPO:-https://github.com/modus-lisp/vessel.git}
CACHE=${MODUS_VESSEL_CACHE:-$HOME/.cache/modus/vessel.git}
if [ ! -d "$CACHE" ]; then
  mkdir -p "$(dirname "$CACHE")"
  git clone -q --bare "$REPO" "$CACHE"
fi
git -C "$CACHE" cat-file -e "$PIN^{commit}" 2>/dev/null || git -C "$CACHE" fetch -q "$REPO" '+refs/heads/*:refs/heads/*'
git -C "$CACHE" cat-file -e "$PIN^{commit}" || { echo "vessel-checkout: $PIN is not in $REPO" >&2; exit 2; }
DEST=$MODUS/.vessel
if [ -d "$DEST" ]; then
  [ "$(git -C "$DEST" rev-parse HEAD)" = "$(git -C "$CACHE" rev-parse "$PIN^{commit}")" ] && exit 0
  git -C "$DEST" checkout -q --detach "$PIN"
else
  git -C "$CACHE" worktree prune
  git -C "$CACHE" worktree add -q --detach "$DEST" "$PIN"
fi
echo "vessel $(git -C "$DEST" rev-parse --short HEAD) at $DEST"
