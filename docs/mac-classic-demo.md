# Modus on a Mac Classic: a demo script

What to type, and what to leave out.  Measured on MAME's Mac Classic with 4 MB, System 6.0.8,
Modus launched as an application (`scripts/mac68k-chain.py`; SMALL.md,
"Launched from Mac OS").  Times are from pressing Return to the next prompt and
include echoing the line; a real Classic's SCSI disk may add a little on the
first use of something.

## Starting it

Double-click **Modus** (or make it the startup application).  The splash shows
while the image loads; the prompt comes about 15 s after launch, ~60-70 s
after power-on including System 6's own boot.  Restart the Mac to return to
the Finder.

## Quick (2-7 s)

| type | answer |
|---|---|
| `(+ 1 2)` | `3` |
| `(expt 2 100)` | `1267650600228229401496703205376` |
| `(* 123456789 987654321)` | `121932631112635269` |
| `(format nil "~R" 1987)` | `"one thousand nine hundred eighty-seven"` |
| `(format nil "~@R" 1987)` | `"MCMLXXXVII"` |
| `(reverse '(1 2 3 4 5))` | `(5 4 3 2 1)` |
| `(string-upcase "hello, macintosh")` | `"HELLO, MACINTOSH"` |
| `(length "Common Lisp")` | `11` |
| `(machine-type)` | `"m68k"` |
| `(lisp-implementation-type)` | `"Modus"` |
| `(greet "Classic")` (after the DEFUN below) | `"Hello, Classic!"` |

## Worth the wait (15-25 s)

| type | answer |
|---|---|
| `(defun greet (name) (format nil "Hello, ~A!" name))` | `GREET` (21 s) |
| `(mapcar #'1+ '(1 2 3))` | `(2 3 4)` (17 s) |
| `(apply #'+ '(1 2 3 4 5 6 7 8 9 10))` | `55` (19 s) |
| `(sort (list 5 3 1 4 2) #'<)` | `(1 2 3 4 5)` (23 s) |
| `(defvar *count* 0)` then `(incf *count*)` | `*COUNT*`, `1` (35 s, 15 s) |

Compiling is the slow part: a DEFUN takes ~20-30 s; calling it afterwards is
a few seconds.

## Recursion and loops (the threaded JIT)

An image built with `MODUS_68K_JIT=1` translates each DEFUN to the threaded
code image functions run as (docs/68k-threaded-jit.md), so a function defined
at the prompt runs at the image's own speed.  MAME's Classic (68000), 4 MB:

| type | answer |
|---|---|
| `(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))` | `FIB` (80 s) |
| `(fib 15)` | `610` (4 s; ~19 minutes interpreted) |
| `(fib 20)` | `6765` (17 s) |

On a 1 MB Mac Plus (`plus1mjit`, SMALL.md) DEFUN stays interpreted and
`(compile 'fib)` translates (97 s); then `(fib 15)` 6 s, `(fib 20)` 18 s.

The price is at the DEFUN: translating adds up to a minute (`greet` 30 s, `fib`
80 s).  A function the JIT cannot translate (a shape the image lacks, or a
module over 2 KB of bytecode -- every DEFSTRUCT) stays interpreted, with the
same answers; `modus-int::*t68-refused*` says which and why.

## Leave out for now

- **Loops and recursion of any length, without the JIT**: a 100-iteration
  DOTIMES takes ~70 s and `(fib 15)` ~19 minutes interpreted.
- **Many definitions in one session**: the 4 MB layout's session heap is
  small; after a dozen DEFUNs, a DEFSTRUCT runs out of heap (with or without
  the JIT).
- **LOOP, DEFSTRUCT, DEFMACRO**: right answers, but 1.5-9 minutes to expand.
- **Ratios, keywords, floats and structures in output**: `(/ 1 3)` prints
  `#<?51>`, `:a` prints as `A`, `4.5` as `4.500000`, a structure as
  `#(%STRUCT-INSTANCE POINT 3 4)` (being fixed).
