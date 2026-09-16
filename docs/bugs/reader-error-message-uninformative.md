# Reader error on unbalanced input is uninformative

**Severity:** minor (developer-experience), not a correctness bug.
**Where:** the CL reader (`mvm/cl-reader.lisp`) + condition printing.
**Modus commit:** `ab6a927` (hosted x64 CLI, `tmp/modus-vi` in this session; any
`build-generic-cli` / `build-aarch64-cli` image reproduces it).

## What happens

When `read` hits end-of-file inside an unclosed list, Modus signals a
`READER-ERROR` whose printed representation carries **no useful information** —
no stream position, no "unexpected end of file", nothing that points at the
problem:

```lisp
(handler-case (read-from-string "(a (b c)")      ; one ')' missing
  (error (e) (format t "TYPE ~s~%PRINC ~a~%" (type-of e) e)))
```

Modus:

```
TYPE READER-ERROR
PRINC #(READER-ERROR ((STREAM)))
```

SBCL, same input:

```
end of file on #<...STRING-INPUT-STREAM... from "(a (b c)">
```

Two problems in the Modus output:

1. The message is empty of content. `#(READER-ERROR ((STREAM)))` is the raw
   object vector leaking through `princ`/`format ~a` — the condition has no
   `report` method, so a human (or a `load` failure log) learns only the
   condition type, not that it was EOF, and not where.
2. It does not distinguish end-of-file-in-object from other reader errors, so
   the one hint that immediately localises the mistake ("you're missing a
   close paren / the file is truncated") is absent.

## Why it matters

`load` of a source file with one missing paren aborts with
`load-read-error-stops-load: READER-ERROR` and nothing else. Bisecting *which*
form and *where* then means external paren-counting, because the reader won't
say. In this session that turned a one-character typo in a hand-written kernel
into a long detour — the tell that should have ended it in seconds ("EOF at
stream position N") never appeared.

## Reproduction

`docs/bugs/reader-error-message-uninformative.lisp` (below) — load it in any
Modus image and compare to SBCL:

```lisp
(handler-case (read-from-string "(a (b c)")
  (error (e) (format t "~a~%" e)))
```

Expected (SBCL-like): a message naming end-of-file and the stream / position.
Actual: `#(READER-ERROR ((STREAM)))`.

## Suggested fix (not done here)

Give `READER-ERROR` (or a `end-of-file`/`READER-ERROR` subtype) a `report`
method that prints the stream and the reader's current position, and signal the
EOF-in-object case with a message like SBCL's "end of file on <stream>". The
reader already knows the position; it is only the condition's printed form that
drops it.

## Note

This was found while chasing what looked like a reader bug on a large form; it
turned out the form was simply missing a paren, and **both** SBCL and Modus
correctly reject it — the only real gap is the message quality above. There is
no reader *correctness* bug here.
