# Reader-failure triage instrumentation

The library ladder reports read failures as `#(READER-ERROR ((STREAM)))`, which
carries no position and no cause.  These two drivers recover both.

    ./modus --load tools/reader-triage/read-bisect.lisp --eval '(rd-probe "/path/file.lisp")'

* `read-bisect.lisp`  — `(rd-probe PATH)`: READ every top-level form from a
  string stream, reporting `form#`, `startpos`, `endpos` (the stream position at
  the moment READ signalled — i.e. the offending character) and 260 chars of
  context.  Pure READ: no evaluation, so `in-package` / runtime readtable
  changes are NOT in effect.
* `read-eval-trace.lisp` — `(rde2 PATH)`: READ **and EVAL** interleaved, exactly
  like `%it-eval-source`, printing `F<n>@<pos>` before each form.  Use this when
  the file installs its own reader macros (iterate's `#L`) or when the failure
  only appears after earlier forms have run.  Also reports per-form EVAL errors.

Method note: map the reported `endpos` back to the source with
`python3 -c "s=open(P).read(); print(repr(s[N-200:N]),'<<<HERE>>>',repr(s[N:N+80]))"`.
