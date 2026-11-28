#!/bin/sh
# repl.sh -- the modus REPL (Linux).  Any arguments go to modus: ./repl.sh --help.
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd) || exit 1
exec "$here/modus-linux-x64" "$@"
