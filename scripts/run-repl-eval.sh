#!/bin/bash
# run-repl-eval.sh — Boot a Modus REPL kernel, eval an expression, print result
#
# Usage: run-repl-eval.sh ARCH KERNEL EXPR
#
# Internal helper used by run-{x64,i386,aarch64,arm32}-repl.sh batch mode.

ARCH="$1"
KERNEL="$2"
EXPR="$3"
TIMEOUT=${REPL_TIMEOUT:-60}
SCRIPTDIR="$(cd "$(dirname "$0")" && pwd)"

# Disable THP for x86 QEMU (avoids 30s hangs from THP compaction)
NO_THP=""
if [ -x "$SCRIPTDIR/no-thp-exec" ]; then
    case "$ARCH" in x64|i386) NO_THP="$SCRIPTDIR/no-thp-exec" ;; esac
fi

qemu_args() {
    case "$ARCH" in
        x64)     echo "-kernel $KERNEL -m 512 -nographic -no-reboot" ;;
        i386)    echo "-kernel $KERNEL -m 256 -display none -serial stdio -no-reboot" ;;
        aarch64) echo "-kernel $KERNEL -nographic -semihosting -machine virt -cpu cortex-a57 -m 512" ;;
        arm32)   echo "-kernel $KERNEL -nographic -M virt,highmem=off -cpu cortex-a15 -m 256" ;;
        *)       echo "Unknown arch: $ARCH" >&2; exit 1 ;;
    esac
}

qemu_bin() {
    case "$ARCH" in
        x64)     echo "qemu-system-x86_64" ;;
        i386)    echo "qemu-system-i386" ;;
        aarch64) echo "qemu-system-aarch64" ;;
        arm32)   echo "qemu-system-arm" ;;
    esac
}

QEMU="$(qemu_bin)"
ARGS="$(qemu_args)"
OUT=$(mktemp /tmp/modus-eval.XXXXXX)
FIFO=$(mktemp -u /tmp/modus-fifo.XXXXXX)
mkfifo "$FIFO"
trap 'rm -f "$OUT" "$FIFO"; kill $QPID 2>/dev/null; wait $QPID 2>/dev/null' EXIT

# Boot QEMU: stdin from FIFO, stdout to file
timeout "$TIMEOUT" $NO_THP $QEMU $ARGS < "$FIFO" > "$OUT" 2>/dev/null &
QPID=$!
exec 3>"$FIFO"  # hold FIFO open so QEMU doesn't get EOF

# Wait for REPL prompt (poll output file)
for i in $(seq 1 300); do
    kill -0 $QPID 2>/dev/null || { echo "QEMU died" >&2; exit 1; }
    # Match "> " at start of line (the REPL prompt), not iPXE ">" in middle of line
    if tail -1 "$OUT" 2>/dev/null | grep -q '^> $'; then
        break
    fi
    sleep 0.1
done

if ! tail -1 "$OUT" 2>/dev/null | grep -q '^> $'; then
    echo "Timeout waiting for prompt" >&2
    exit 1
fi

# Send expression.
# CR, not LF: the bare-metal serial line editor terminates a line on CR
# (0x0D) and ignores LF.  Sending "\n" made this helper silently produce
# NOTHING and exit 0 — the expression echoed at the prompt and was never
# evaluated, so the extraction below found no result line.  Verified on both
# x64 and aarch64: with "\r" a bare symbol evaluates and prints, with "\n" it
# does not.
printf '%s\r' "$EXPR" >&3
sleep 1

# Close FIFO and let QEMU process
exec 3>&-
sleep 0.5

# Extract result: line after "> expr"
tr -d '\r' < "$OUT" | sed -n '/^> .*[^ ]/{n; /^>/!{/^$/!p;};}' | head -1
