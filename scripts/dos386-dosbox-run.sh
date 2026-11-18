#!/bin/sh
# dos386-dosbox-run.sh IMAGE [ARGS...] -- boot a multiboot IMAGE under DOSBox
# through MODUS.COM and copy COM1 to stdout.
#
#   DOSBOX        the dosbox binary (default: dosbox on PATH, else
#                 ~/.local/dosbox/bin/dosbox -- 0.74-3 built with SDL_net,
#                 which the nullmodem serial port needs)
#   DOS_MEM       DOSBox memsize, MB (default 63, 0.74's maximum)
#   DOS_CPU       DOSBox cputype (default auto; 386, 486_slow, pentium_slow)
#   DOS_TIMEOUT   seconds before giving up (default 120)
#   DOS_UNTIL     stop as soon as COM1 has printed this string
#   DOS_KEEP      a directory: keep the C: drive and dosbox.conf there
#
# COM1 is DOSBox's nullmodem port, which connects out to a TCP listener this
# script runs (0.74 has no "serial to a file").  Headless: SDL's dummy video
# and audio drivers.  MODUS.COM's own messages go to C:\LOADER.TXT, which
# is copied to stderr at the end; when it fails, DOSBox exits (exit 3 when
# DOS_UNTIL was given).
set -e
here=$(cd "$(dirname "$0")" && pwd)
img=$1; shift || { echo "usage: $0 IMAGE [ARGS...]" >&2; exit 2; }
[ -f "$img" ] || { echo "$0: no image $img" >&2; exit 2; }
dosbox=${DOSBOX:-$(command -v dosbox || echo "$HOME/.local/dosbox/bin/dosbox")}
[ -x "$dosbox" ] || { echo "$0: no dosbox at $dosbox (set DOSBOX)" >&2; exit 2; }

work=${DOS_KEEP:-$(mktemp -d)}
mkdir -p "$work/c"
[ -n "$DOS_KEEP" ] || trap 'rm -rf "$work"' EXIT
"$here/dos386-com.sh" "$work/c/MODUS.COM"
cp "$img" "$work/c/MODUS.BIN"

port=$(python3 -I -c 'import socket; s=socket.socket(); s.bind(("127.0.0.1",0)); print(s.getsockname()[1])')
cat > "$work/dosbox.conf" <<CONF
[sdl]
output=surface
[dosbox]
memsize=${DOS_MEM:-63}
[cpu]
core=auto
cputype=${DOS_CPU:-auto}
cycles=max
[dos]
xms=false
ems=false
umb=false
[serial]
serial1=nullmodem server:127.0.0.1 port:$port transparent:1
serial2=disabled
[autoexec]
mount c "$work/c"
c:
MODUS.COM MODUS.BIN $* > LOADER.TXT
exit
CONF

rc=0
python3 -I "$here/dos386-dosbox-capture.py" "$port" "${DOS_TIMEOUT:-120}" "${DOS_UNTIL:-}" \
    "$dosbox" -conf "$work/dosbox.conf" || rc=$?
for f in "$work/c/LOADER.TXT" "$work/c/loader.txt"; do
    [ -f "$f" ] && tr -d '\r' < "$f" >&2
done
exit $rc
