#!/bin/bash
# run-hosted-core.sh -- SAVE-AND-DIE on hosted x86-64: snapshot a process that has
# alexandria loaded, restore it with --core, and serve attested SSH from the
# restored process.
#   test/run-hosted-core.sh [./modus] [alexandria.tar]
# 1. modus --eval (install-tarball TAR) --eval (save-and-die CORE)   -> CORE written
# 2. modus --core CORE --eval (print (alexandria:iota 3))              -> (0 1 2), no install
# 3. test/run-hosted-ssh.sh "modus --core CORE"                        -> PASS (a fresh host key)
# Also asserts the heap base is the fixed one (the core refuses otherwise) and
# that the restored process has NOT inherited a host key from the saving one.
set -u
M=${1:-./modus}; TAR=${2:-test/ladder/tars/alexandria.tar}; cd "$(dirname "$0")/.."
W=$(mktemp -d /tmp/hosted-core.XXXXXX); CORE=$W/alex.core
fail=0
t0=$(date +%s.%N)
$M --eval "(install-tarball \"$TAR\")" --eval "(save-and-die \"$CORE\")" > $W/save.log 2>&1; rc=$?
t1=$(date +%s.%N)
[ $rc = 0 ] && [ -s $CORE ] && echo "save: $(stat -c %s $CORE) bytes in $(python3 -c "print(round($t1-$t0,1))") s" || { echo "FAIL: save rc=$rc"; grep -v "WARN: implicit" $W/save.log | tail -5; exit 1; }
t2=$(date +%s.%N)
out=$($M --core $CORE --eval "(print (list (alexandria:iota 3) (fboundp 'ssh-serve-tcp) (nitro-ssh-host-pubkey)))" --quit 2>&1 | grep -v "WARN: implicit\|^$" | tail -1)
t3=$(date +%s.%N)
echo "restore + eval: $out  ($(python3 -c "print(round($t3-$t2,2))") s)"
[ "$(echo $out)" = "((0 1 2) T NIL)" ] && echo "ok: restored heap answers, no inherited host key" || { echo "FAIL: restore"; fail=1; }
test/run-hosted-ssh.sh "$M --core $CORE" | grep "ok:\|FAIL\|PASS" | sed 's/^/  /'
[ ${PIPESTATUS[0]} = 0 ] || fail=1
[ $fail = 0 ] && echo "PASS: hosted core" || echo "FAIL: hosted core"
exit $fail
