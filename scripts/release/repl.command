#!/bin/sh
# repl.command -- the modus REPL (macOS).  Double-click it in Finder, or run it in Terminal;
# any arguments go to modus.  Finder starts it in your home directory, so it moves to its own.
cd -- "$(dirname -- "$0")" || exit 1
exec ./modus-macos-arm64 "$@"
