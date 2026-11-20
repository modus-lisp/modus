#!/bin/bash
# run-fault-stops.sh — A FAULT THE PROCESS CANNOT RECOVER FROM STOPS IT, LOUDLY.
#
#   test/run-fault-stops.sh [BINARY]          (hosted x86-64 or AArch64)
#
# Each case runs BINARY on a small script of its own and looks at what a parent
# sees: the exit status and stderr.  A shell script, because the subject is how
# the process ENDS (hosted AArch64 has no RUN-PROGRAM to watch a child from
# Lisp, and a test inside the dying process cannot report its own death).
#
# A FAULT INSIDE A COLLECTION (translate-x64 / translate-aarch64, A FAULT
# INSIDE A COLLECTION STOPS THE PROCESS).  The heap is half-copied while a
# thread runs the collector; recovering the fault into a HANDLER-CASE would
# run Lisp on it.  The test-only window word +0x5630 makes this thread's next
# collection read address 8.  The process must exit 71 with one line naming
# the fault, "inside a garbage collection" and the thread, and must NOT print
# what the script prints after the collection (it never resumes).  On main
# (region 0, the world stopped when threads are armed) and on a worker (which
# collects its OWN region: the CONTROL case shows region 0's count unmoved).
# CONTROL: the same collections without the knob return, and a fault right
# after them is an ordinary, caught MEMORY-FAULT-ERROR -- the mark is cleared.
#
# Before (origin/main 45ac60ed): AArch64 had no such check at all, and the
# x86-64 one tested words of a dormant protocol (X64-STW-P answers NIL).
# Measured with the knob and without the check, both arches: main and a worker
# recovered the fault into the HANDLER-CASE around the collection and carried
# on (exit 0, "RESUMED (:RECOVERED MEMORY-FAULT-ERROR)"), and main with a
# worker running HUNG (exit 124) -- the world was left stopped.

set -u
BIN=${1:-./modus}
case $BIN in /*) ;; *) BIN=$PWD/$BIN ;; esac
[ -x "$BIN" ] || { echo "no such binary: $BIN" >&2; exit 1; }
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT
fails=0; checks=0

ok()   { checks=$((checks+1)); echo "  ok   $1"; }
fail() { checks=$((checks+1)); fails=$((fails+1)); echo "  FAIL $1"; }

# run NAME — runs $T/NAME.lisp; sets RC, OUT, ERR
run() {
  timeout -k 5 120 "$BIN" --script "$T/$1.lisp" </dev/null >"$T/$1.out" 2>"$T/$1.err"
  RC=$?
  OUT=$(cat "$T/$1.out"); ERR=$(cat "$T/$1.err")
}

# expect NAME RC GREP-STDERR NOT-IN-STDOUT
expect() {
  local name=$1 want=$2 pat=$3 notout=$4
  run "$name"
  if [ "$RC" = "$want" ]; then ok "$name: exit $RC"; else fail "$name: exit $RC, want $want"; fi
  local line
  line=$(grep -a "^modus: " "$T/$name.err" | head -n 1)
  if printf '%s\n' "$line" | grep -aq -- "$pat"; then ok "$name: says: $line"
  else fail "$name: stderr lacks /$pat/: $(head -c 600 "$T/$name.err")"; fi
  if [ "$(grep -ac '^modus: ' "$T/$name.err")" = 1 ]; then ok "$name: one line"
  else fail "$name: not exactly one modus: line"; fi
  if printf '%s' "$OUT" | grep -aq -- "$notout"; then fail "$name: resumed Lisp (stdout has $notout)"
  else ok "$name: never resumed"; fi
}

KNOB='(defun knob () (modus-int::%gc-write64 (+ (modus-int::%tls-self-base) #x10005630) 1))'

echo "=== A FAULT INSIDE A COLLECTION STOPS THE PROCESS ==="
echo "binary: $BIN"

cat >"$T/gc-main.lisp" <<EOF
$KNOB
(defun collect () (handler-case (progn (sb-ext:gc) :no-fault) (condition (c) (list :recovered (type-of c)))))
(format t "start~%")
(knob)
(format t "RESUMED ~S~%" (collect))
EOF
expect gc-main 71 "memory fault (SIGSEGV) inside a garbage collection on the main thread (tid [0-9]*): address #x8, pc #x[0-9A-F]*; the heap" RESUMED

cat >"$T/gc-main-threads.lisp" <<EOF
$KNOB
(defun collect () (handler-case (progn (sb-ext:gc) :no-fault) (condition (c) (list :recovered (type-of c)))))
(defvar *stop* nil)
;; a worker running alongside, so the main collection stops the world
(defvar *th* (sb-thread:make-thread (lambda () (let ((n 0)) (loop (when *stop* (return n)) (setq n (+ n 1)))))))
(modus-int::%sleep-ms 50)
(knob)
(format t "RESUMED ~S~%" (collect))
EOF
expect gc-main-threads 71 "inside a garbage collection on the main thread" RESUMED

cat >"$T/gc-worker.lisp" <<EOF
$KNOB
(defun collect () (handler-case (progn (sb-ext:gc) :no-fault) (condition (c) (list :recovered (type-of c)))))
(format t "RESUMED ~S~%"
        (sb-thread:join-thread (sb-thread:make-thread (lambda () (knob) (list (collect) :after)))))
EOF
expect gc-worker 71 "memory fault (SIGSEGV) inside a garbage collection on worker [0-9]* (tid [0-9]*): address #x8" RESUMED

cat >"$T/gc-control.lisp" <<'EOF'
(defun region0-gcs () (modus-int::%gc-meta-read (+ (modus-int::%gc-region-0) #x20) (modus-int::%gc-meta-scale)))
(defun after () (handler-case (progn (mem-ref 64 :u32) :no-fault) (error (c) (symbol-name (type-of c)))))
(sb-ext:gc)
(format t "MAIN ~A~%" (after))
(let* ((g0 (region0-gcs))
       (r (sb-thread:join-thread (sb-thread:make-thread (lambda () (sb-ext:gc) (sb-ext:gc) (after)))))
       (g1 (region0-gcs)))
  (format t "WORKER ~A~%" r)
  (format t "WORKER-R0 ~A~%" (if (= g0 g1) "unmoved" "moved")))
(format t "CONTROL DONE~%")
EOF
run gc-control
[ "$RC" = 0 ] && ok "control: exit 0" || fail "control: exit $RC: $(head -c 400 "$T/gc-control.err")"
printf '%s' "$OUT" | grep -q "MAIN MEMORY-FAULT-ERROR" && ok "control: main fault after a collection is caught" || fail "control main: $OUT"
printf '%s' "$OUT" | grep -q "WORKER MEMORY-FAULT-ERROR" && ok "control: worker fault after a collection is caught" || fail "control worker: $OUT"
printf '%s' "$OUT" | grep -q "WORKER-R0 unmoved" && ok "control: the worker collected its own region (region 0 unmoved)" || fail "control worker region: $OUT"
printf '%s' "$OUT" | grep -q "CONTROL DONE" && ok "control: finished" || fail "control: did not finish"

echo
echo "$checks checks, $fails failed"
if [ "$fails" = 0 ]; then echo "FAULT STOPS: PASS"; exit 0; else echo "FAULT STOPS: FAIL"; exit 1; fi
