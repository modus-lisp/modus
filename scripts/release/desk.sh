#!/bin/sh
# desk.sh -- the glass desk (Linux): a window manager served to a web browser and over VNC.
# Its flags are glass-desk.lisp's: ./desk.sh --help.
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd) || exit 1
exec "$here/modus-linux-x64" --dynamic-space-size 8192 \
     --script "$here/src/modus/scripts/glass-desk.lisp" "$@"
