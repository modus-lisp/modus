#!/bin/bash
# run-ladder-aarch64.sh <aarch64-cli-binary> <tag> [parallel] [timeout-secs]
#
# Kept for existing callers; the ladder lives in test/ladder/ now (it was
# /home/claude/lf).  test/ladder/run.sh reads the binary's arch from its ELF
# header and supplies the qemu-user wrapper itself.
#
# Score with:  python3 test/ladder/score.py tmp/ladder-logs/<tag>
exec "$(dirname "$0")/../test/ladder/run.sh" "$@"
