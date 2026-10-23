#!/bin/bash
# The acceptance gate used to come in two variants that differed only in
# whether the two baked ANSI runners built in parallel.  Both are now
# scripts/acceptance-gate.sh, which this forwards to.
exec "$(dirname "$0")/acceptance-gate.sh" "$@"
