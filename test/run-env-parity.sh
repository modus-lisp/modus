#!/bin/bash
# An argv/envp string can start on an ODD byte, and which ones do depends on the
# lengths of every string before it.  The CLI's reader used to double a :u64
# load (whose value IS the raw word), which for an odd pointer is not a fixnum:
# depending on the tag nibble and the bytes around it, generic * either happened
# to compute the right address or SIGNALLED -- and the boot shims'
# (handler-case ... (t () nil)) swallowed that, so the process came up with no
# SB-EXT and no :GENERA.  One character of $PWD decided it.
# Sweep an argv pad through 64 lengths (every env string's address moves with
# it) and require every probe variable read back exactly, plus the shims.
BIN=${1:-./modus}; fail=0; bad=0
for n in $(seq 0 63); do
  pad=$(head -c $n /dev/zero | tr '\0' a)
  r=$(env -i HOME=/home/x ZA=1 ZBB=22 ZCCC=333 ZDDDD=4444 ZEEEEE=55555 ZFFFFFF=666666 ZGGGGGGG=7777777 \
      $BIN --eval '(progn (princ (list :r (if (member :genera *features*) 1 0) (if (find-package "SB-EXT") 1 0) (%cli-getenv "ZA") (%cli-getenv "ZBB") (%cli-getenv "ZCCC") (%cli-getenv "ZDDDD") (%cli-getenv "ZEEEEE") (%cli-getenv "ZFFFFFF") (%cli-getenv "ZGGGGGGG") (length (%cli-collect-argv)))) (terpri) (sys-exit 0))' "p$pad" 2>/dev/null </dev/null | grep -a '^(R ' | tail -1)
  if [ "$r" != "(R 1 1 1 22 333 4444 55555 666666 7777777 4)" ]; then echo "pad=$n BAD: $r"; bad=$((bad+1)); fi
done
echo "env-parity: $bad of 64 bad"; [ $bad = 0 ] && echo PASS || { echo FAIL; exit 1; }
