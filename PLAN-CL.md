# Plan: Full ANSI Common Lisp Implementation

## Current State
- 371 ANSI tests loaded, 359 passing, 12 failures (9 CLOS, 3 pre-existing)
- ~350+ CL functions implemented (core language + packages + streams + reader + printer + format + conditions)
- ~550 CL functions stubbed or unresolved
- Compiler: MVM bytecode → x64 native, self-hosting
- Runtime: bare-metal, no OS, tagged 63-bit fixnums + bignums

## Architecture: Build Bottom-Up

### Layer 0: Packages (FOUNDATION)
Everything in CL is organized by packages. Without them we have "last defun wins."

**Implement:**
- `make-package`, `find-package`, `delete-package`
- `intern`, `find-symbol`, `unintern`
- `export`, `unexport`, `import`, `shadowing-import`
- `use-package`, `unuse-package`
- `in-package`, `defpackage`
- `*package*`, `package-name`, `package-nicknames`
- `package-use-list`, `package-used-by-list`
- `do-symbols`, `do-external-symbols`, `do-all-symbols`
- Built-in packages: COMMON-LISP, COMMON-LISP-USER, KEYWORD

**Data structure:** Package = struct with name, nickname list, internal symbols (hash table), external symbols (hash table), use-list, used-by-list.

**Impact:** Eliminates namespace collisions. Enables `cl:car` vs `modus:car`. The compiler's function table becomes per-package.

### Layer 1: Streams
The I/O abstraction layer. Needed by reader, printer, format, file I/O.

**Implement:**
- Stream protocol: `stream-read-char`, `stream-write-char`, `stream-unread-char`
- `make-string-input-stream`, `make-string-output-stream` (partial — extend)
- `make-broadcast-stream`, `make-concatenated-stream`
- `make-echo-stream` (partial — extend)
- `make-two-way-stream`, `make-synonym-stream`
- `open-stream-p`, `input-stream-p`, `output-stream-p`
- `read-char`, `write-char`, `peek-char`, `unread-char`
- `read-line`, `write-line`, `write-string` (partial — extend)
- `terpri`, `fresh-line`
- `*standard-input*`, `*standard-output*`, `*error-output*`
- `*terminal-io*`, `*query-io*`, `*debug-io*`, `*trace-output*`

**Data structure:** Stream = tagged object with type tag, direction, element-type, and type-specific state (string+position, file descriptor, etc.)

### Layer 2: Reader
Parse Lisp source from streams. Currently we use SBCL's reader at build time.

**Implement:**
- `read`, `read-preserving-whitespace`
- `read-from-string` (partial — extend beyond integers)
- Readtable: `*readtable*`, `copy-readtable`, `set-macro-character`
- Standard syntax: lists, strings, symbols, numbers, characters
- `#'`, `#\`, `#(`, `#:`, `#.`, reader macros
- `read-delimited-list`
- Backquote/comma expansion

### Layer 3: Printer
Write Lisp objects to streams. Currently we have basic serial output.

**Implement:**
- `write`, `prin1`, `princ`, `print`, `pprint`
- `write-to-string`, `prin1-to-string`, `princ-to-string`
- Printer control: `*print-base*`, `*print-radix*`, `*print-case*`, `*print-escape*`, `*print-circle*`, `*print-level*`, `*print-length*`, `*print-pretty*`, `*print-array*`
- `print-object` (generic function — needs CLOS or dispatch table)
- `with-standard-io-syntax`

### Layer 4: Format
The format string mini-language.

**Implement:**
- `format` with full directives
- `~A` (aesthetic), `~S` (standard) — have
- `~D` (decimal), `~B` (binary), `~O` (octal), `~X` (hex) — partial
- `~C` (character), `~R` (radix/English)
- `~%` (newline), `~~` (tilde) — have
- `~{...~}` (iteration), `~[...~]` (conditional)
- `~*` (goto), `~?` (recursive), `~P` (plural)
- `~T` (tabulate), `~<...~>` (justification)
- `~W` (write)

### Layer 5: Conditions & Restarts
Full condition system. We have `handler-case` with setjmp/longjmp.

**Implement:**
- `define-condition` (needs CLOS or struct-based)
- `signal`, `error`, `warn`, `cerror`
- `handler-bind` (vs handler-case)
- `restart-case`, `restart-bind`
- `invoke-restart`, `find-restart`
- `with-simple-restart`, `abort`, `continue`, `muffle-warning`
- Standard conditions: `error`, `type-error`, `arithmetic-error`, etc.

### Layer 6: CLOS
The Common Lisp Object System. Largest single feature.

**Implement (minimal):**
- `defclass` with slots, initargs, initforms
- `make-instance`, `initialize-instance`
- `slot-value`, `slot-boundp`, `slot-makunbound`
- `defgeneric`, `defmethod`
- Method dispatch (single dispatch first, then multi)
- `call-next-method`, `next-method-p`
- Standard method combination
- `class-of`, `typep` integration
- Built-in classes: `t`, `standard-object`, `standard-class`

**Implement (full):**
- `change-class`, `update-instance-for-redefined-class`
- `slot-missing`, `slot-unbound`
- MOP (Meta-Object Protocol) — metaclasses, generic function protocol
- `print-object` methods
- Condition classes (integrates with Layer 5)

### Layer 7: File I/O & Pathnames
File system access. Less critical for bare-metal but needed for conformance.

**Implement:**
- `open`, `close`, `with-open-file`
- `read-byte`, `write-byte`, `read-sequence`, `write-sequence`
- `file-position`, `file-length`
- `pathname`, `make-pathname`, `merge-pathnames`
- `namestring`, `directory-namestring`, `file-namestring`
- `probe-file`, `truename`, `delete-file`, `rename-file`
- Logical pathnames

### Layer 8: Everything Else
- `eval`, `compile`, `load` — the evaluator
- `trace`, `untrace`, `step`
- `describe`, `inspect`, `documentation`
- `time`, `room`
- `random-state`, full random
- `loop` extensions (accumulation into, hash-table iteration)
- `setf` expansions for all accessors
- Declarations: `optimize`, `type`, `inline`, `dynamic-extent`

## Implementation Order

```
Session 1: Packages          ✓ DONE — 24 functions, 303 tests
Session 2: Streams            ✓ DONE — 9 stream types, read/write-char
Session 3: Reader             ✓ DONE — full read, readtable, #-dispatch, backquote
Session 4: Printer            ✓ DONE — write with *print-* vars, prin1, princ, print
Session 5: Format             ✓ DONE — ~A ~S ~D ~B ~O ~X ~R ~C ~P ~% ~~ ~& ~| ~T ~* ~? ~W ~(~) ~[~] ~{~} ~^
Session 6: Conditions         ✓ DONE — 24 types, handler-bind, restart-case, signal/warn/cerror
Session 7: CLOS core          IN PROGRESS — defclass, make-instance, slot-value
Session 8: CLOS dispatch      — defgeneric, defmethod, call-next-method
Session 9: File I/O           — open, close, pathnames
Session 10: Polish            — eval, compile, loop extensions, declarations
```

Each session: implement the layer, add test coverage, verify 0 regressions.
