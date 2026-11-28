#!/bin/sh
# desk.command -- the glass desk (macOS): a window manager served to a web browser and over
# VNC.  Double-click it in Finder, or run it in Terminal with flags: ./desk.command --help.
# Finder starts it in your home directory, so it moves to its own first.
cd -- "$(dirname -- "$0")" || exit 1
exec ./modus-macos-arm64 --dynamic-space-size 8192 \
     --script src/modus/scripts/glass-desk.lisp "$@"
