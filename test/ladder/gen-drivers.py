#!/usr/bin/env python3
"""gen-drivers.py --tars DIR --out DIR -- generate the library-ladder drivers.

One driver per library.  Each driver:
  1. records gc_count,
  2. INSTALL-TARBALLs the library's dependencies, then the library itself --
     the UNMODIFIED quicklisp tarball in tars/; the .asd is parsed and the
     component walk / load order computed by Modus, not by a host ASDF,
  3. runs a set of USE probes (defined-in-one-form / used-in-a-later-form),
  4. FORCES a real collection,
  5. runs the SAME probes again (P1.* before the GC, P2.* after).

Probe sources are strings read at RUNTIME (read-from-string) so the driver
itself can be READ before the library's package exists.

Inter-system :depends-on is resolved HERE, from each .asd's own declared
:depends-on (DEPS below) -- INSTALL-TARBALL has no cross-system resolver and no
system registry.  That is the one remaining piece of host help, and it is
declared, not discovered.

Drivers embed the ABSOLUTE tarball path, so they are generated per run
(test/ladder/run.sh writes them into the log directory beside the logs they
produced) rather than committed.  Output is deterministic: the same --tars
gives byte-identical drivers.

History: this was /home/claude/lf ("library frontier", GATE-RESULT-libfrontier.md)
-- gen.py (probes) + gen-ql.py (the real-path drivers), merged.  gen.py's
other mode, drivers that loaded a file list computed by the HOST's ASDF, is
not carried over: it measured the language and almost none of the system.
"""
import argparse, os

# name -> (unused, [(probe-name, source-string), ...]).  The middle field is
# the old host-ASDF mode's dep list; load order now comes from DEPS below.
LADDER = [
 ("alexandria", [], [
    ("wgs-mf",      "(if (macro-function (read-from-string \"alexandria:with-gensyms\")) 1 0)"),
    ("wgs-expand",  "(let ((x (macroexpand-1 (read-from-string \"(alexandria:with-gensyms (g) (list g))\")))) (if (consp x) (car x) x))"),
    ("wgs-use",     "(eval (read-from-string \"(alexandria:with-gensyms (a) (symbolp a))\"))"),
    ("iflet",       "(eval (read-from-string \"(alexandria:if-let ((x 5)) x 0)\"))"),
    ("whenlet",     "(eval (read-from-string \"(alexandria:when-let ((x 7)) x)\"))"),
    ("flatten",     "(funcall (read-from-string \"alexandria:flatten\") (list 1 (list 2 (list 3))))"),
    ("iota",        "(funcall (read-from-string \"alexandria:iota\") 5)"),
    ("curry",       "(funcall (funcall (read-from-string \"alexandria:curry\") (function +) 1) 2)"),
    ("ensure-list", "(funcall (read-from-string \"alexandria:ensure-list\") 1)"),
    ("mean",        "(funcall (read-from-string \"alexandria:mean\") (list 1 2 3))"),
    ("last-elt",    "(funcall (read-from-string \"alexandria:last-elt\") (list 1 2 3))"),
    ("emptyp",      "(funcall (read-from-string \"alexandria:emptyp\") (list))"),
    ("starts-with", "(funcall (read-from-string \"alexandria:starts-with-subseq\") \"ab\" \"abc\")"),
    ("plist-ht",    "(length (funcall (read-from-string \"alexandria:hash-table-plist\") (funcall (read-from-string \"alexandria:plist-hash-table\") (list :a 1))))"),
    ("copy-array",  "(length (funcall (read-from-string \"alexandria:copy-array\") (vector 1 2 3)))"),
 ]),
 ("split-sequence", [], [
    ("split",  "(funcall (read-from-string \"split-sequence:split-sequence\") #\\, \"a,b,c\")"),
    ("split-if","(funcall (read-from-string \"split-sequence:split-sequence-if\") (function digit-char-p) \"a1b\")"),
 ]),
 ("cl-utilities", [], [
    ("split",  "(funcall (read-from-string \"cl-utilities:split-sequence\") #\\, \"a,b\")"),
    ("wun",    "(eval (read-from-string \"(cl-utilities:with-unique-names (a) (symbolp a))\"))"),
    ("collect","(eval (read-from-string \"(cl-utilities:with-collectors (c) (c 1) (c 2))\"))"),
 ]),
 ("trivial-features", [], [
    ("pkg",  "(if (find-package \"TRIVIAL-FEATURES\") 1 0)"),
    ("feat", "(if (member :x86-64 *features*) 1 0)"),
 ]),
 ("global-vars", [], [
    ("defglobal", "(progn (eval (read-from-string \"(global-vars:define-global-var *lf-gv* 5)\")) (eval (read-from-string \"*lf-gv*\")))"),
 ]),
 ("trivial-gray-streams", [], [
    ("pkg", "(if (find-package \"TRIVIAL-GRAY-STREAMS\") 1 0)"),
    ("cls", "(if (find-class (read-from-string \"trivial-gray-streams:fundamental-stream\") nil) 1 0)"),
 ]),
 ("trivial-indent", [], [
    ("pkg",    "(if (find-package \"TRIVIAL-INDENT\") 1 0)"),
    ("define", "(progn (eval (read-from-string \"(trivial-indent:define-indentation lf-foo (4 2))\")) (if (funcall (read-from-string \"trivial-indent:indentation\") (quote lf-foo)) 1 0))"),
 ]),
 ("documentation-utils", ["trivial-indent"], [
    ("pkg",  "(if (find-package \"DOCUMENTATION-UTILS\") 1 0)"),
    ("defdoc","(if (macro-function (read-from-string \"documentation-utils:define-docs\")) 1 0)"),
 ]),
 ("named-readtables", [], [
    ("pkg",  "(if (find-package \"NAMED-READTABLES\") 1 0)"),
    ("defrt","(progn (eval (read-from-string \"(named-readtables:defreadtable lf-rt (:merge :standard))\")) (if (funcall (read-from-string \"named-readtables:find-readtable\") (quote lf-rt)) 1 0))"),
 ]),
 ("bordeaux-threads", ["alexandria", "global-vars", "trivial-features"], [
    ("pkg",   "(if (find-package \"BORDEAUX-THREADS\") 1 0)"),
    ("threadp","(if (funcall (read-from-string \"bordeaux-threads:threadp\") 1) 1 0)"),
    ("lock",  "(if (funcall (read-from-string \"bordeaux-threads:make-lock\")) 1 0)"),
    ("wlh",   "(eval (read-from-string \"(bordeaux-threads:with-lock-held ((bordeaux-threads:make-lock)) 42)\"))"),
 ]),
 ("iterate", [], [
    ("pkg",  "(if (find-package \"ITERATE\") 1 0)"),
    ("sum",  "(eval (read-from-string \"(iterate:iter (iterate:for i from 1 to 3) (iterate:summing i))\"))"),
    ("collect","(eval (read-from-string \"(iterate:iter (iterate:for i from 1 to 3) (iterate:collect i))\"))"),
 ]),
 ("cl-ppcre", [], [
    ("pkg",   "(if (find-package \"CL-PPCRE\") 1 0)"),
    ("match", "(funcall (read-from-string \"cl-ppcre:all-matches-as-strings\") \"b+\" \"abbbc\")"),
    ("subst", "(funcall (read-from-string \"cl-ppcre:regex-replace-all\") \"b\" \"abbc\" \"X\")"),
 ]),
 ("md5", [], [
    ("digest", "(coerce (funcall (read-from-string \"md5:md5sum-string\") \"abc\") (quote list))"),
 ]),
 ("sha1", [], [
    ("hex", "(funcall (read-from-string \"sha1:sha1-hex\") \"abc\")"),
 ]),
 ("cl-base64", [], [
    ("enc", "(funcall (read-from-string \"cl-base64:string-to-base64-string\") \"abc\")"),
    ("dec", "(funcall (read-from-string \"cl-base64:base64-string-to-string\") \"YWJj\")"),
 ]),
 ("parse-float", [], [
    ("pf", "(funcall (read-from-string \"parse-float:parse-float\") \"1.5\")"),
 ]),
 ("ieee-floats", [], [
    ("enc32", "(funcall (read-from-string \"ieee-floats:encode-float32\") 1.0)"),
    ("dec32", "(funcall (read-from-string \"ieee-floats:decode-float32\") 1065353216)"),
 ]),
 ("trivial-garbage", [], [
    ("wp", "(funcall (read-from-string \"trivial-garbage:weak-pointer-value\") (funcall (read-from-string \"trivial-garbage:make-weak-pointer\") 5))"),
    ("wht","(if (funcall (read-from-string \"trivial-garbage:make-weak-hash-table\")) 1 0)"),
 ]),
 ("babel", ["trivial-features", "alexandria"], [
    ("pkg",  "(if (find-package \"BABEL\") 1 0)"),
    ("s2o",  "(length (funcall (read-from-string \"babel:string-to-octets\") \"abc\"))"),
    ("o2s",  "(funcall (read-from-string \"babel:octets-to-string\") (funcall (read-from-string \"babel:string-to-octets\") \"abc\"))"),
 ]),
 ("salza2", [], [
    ("pkg", "(if (find-package \"SALZA2\") 1 0)"),
 ]),
 ("puri", [], [
    ("host", "(funcall (read-from-string \"puri:uri-host\") (funcall (read-from-string \"puri:parse-uri\") \"http://x.com/a\"))"),
 ]),
 ("cl-annot", [], [
    ("pkg", "(if (find-package \"CL-ANNOT\") 1 0)"),
 ]),
]

PRELUDE = r'''
;;; libfrontier driver -- generated, do not edit.
;;; NOTE: no DEFMACRO anywhere in this harness on purpose; a runtime macro is
;;; exactly what the measurement is about, so the harness must not depend on one.
(defun lf-gcn () (mem-ref #x10000060 :u32))
(defun lf-say (k v)
  ;; Build the whole line BEFORE writing it.  On a JIT image the translator
  ;; prints "GC trampoline emitted ..." to stdout while a page is built, which
  ;; would otherwise land BETWEEN (princ k) and (princ v) and split the record.
  (princ (concatenate (quote string) k "=" (princ-to-string v)))
  (terpri) (finish-output))
(defun lf-probe (name src)
  (lf-say name (handler-case (eval (read-from-string src))
                 (t (c) (list :ERR (handler-case (type-of c) (t (c2) :UNKNOWN)))))))
(defun lf-force-gc ()
  ;; A constant-size MAKE-ARRAY is one interpreted call per 800KB, which is
  ;; ~5000x cheaper per allocated byte than a make-list loop.  gc_count is a
  ;; RAW word: read it :u32 (a :u64 read of 1 comes back as a CONS).
  (let ((b (lf-gcn)) (k 0))
    (loop
      (when (or (> (lf-gcn) b) (>= k 2500)) (return (- (lf-gcn) b)))
      (make-array 100000)
      (setq k (+ k 1)))))
(defun lf-load-file (path)
  "Read+eval every top-level form of PATH.  Per-form errors are REPORTED and
   do not abort the file (%it-eval-source), so the log shows how far it got.
   *PACKAGE* is saved and restored around the file, which is what CL:LOAD is
   required to do -- without it the DRIVER's own later forms get READ in the
   library's package and every harness call is UNDEFINED-FUNCTION."
  (let ((p *package*) (r nil))
    (setq r (handler-case
                (%it-eval-source (tar-bytes-to-string (%it-slurp-bytes path)) path)
              (t (c) (list :FILE-ABORT (handler-case (type-of c) (t (c2) :UNKNOWN))))))
    (setq *package* p)
    r))
(defun lf-load (path name)
  (handler-case (progn (install-tarball path name) :LOADED)
    (t (c) (list :LOAD-ABORT (handler-case (type-of c) (t (c2) :UNKNOWN))))))
'''

# Declared :depends-on transcribed from each library's own .asd, restricted to
# systems we hold a tarball for.  Deps marked (MISSING) are declared but have
# no tarball -- they are reported, not shimmed.
DEPS = {
 "alexandria": [],
 "split-sequence": [],
 "cl-utilities": [],
 "trivial-features": [],
 "global-vars": [],
 "trivial-gray-streams": [],
 "trivial-indent": [],
 "documentation-utils": ["trivial-indent"],
 "named-readtables": [],                # declares mgl-pax-bootstrap (MISSING)
 "bordeaux-threads": ["alexandria", "global-vars", "trivial-features",
                      "trivial-garbage"],
 "iterate": [],
 "cl-ppcre": [],
 "md5": [],                             # declares flexi-streams on non-SBCL (MISSING)
 "sha1": [],
 "cl-base64": [],
 "parse-float": ["alexandria"],
 "ieee-floats": [],
 "trivial-garbage": [],
 "babel": ["trivial-features", "alexandria"],
 "salza2": ["trivial-gray-streams"],
 "puri": [],
 "cl-annot": ["alexandria"],
}

def closure(name, seen=None):
    if seen is None: seen = []
    for d in DEPS.get(name, []):
        if d not in seen:
            closure(d, seen)
            if d not in seen: seen.append(d)
    return seen

def json_lisp_string(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tars", required=True, help="directory holding <lib>.tar")
    ap.add_argument("--out", required=True, help="directory to write drivers into")
    ap.add_argument("--list", action="store_true",
                    help="print the ladder (one <lib>-ql per line) and exit")
    a = ap.parse_args()
    names = [name for name, _deps, _probes in LADDER]
    if a.list:
        print("\n".join(n + "-ql" for n in names)); return
    tars = os.path.join(os.path.abspath(a.tars), "")
    os.makedirs(a.out, exist_ok=True)
    for name, _deps, probes in LADDER:
        lines = [PRELUDE]
        lines.append('(lf-say "LF-LIB" "%s-QL")' % name)
        lines.append('(lf-say "LF-GC-0" (lf-gcn))')
        for d in closure(name):
            lines.append('(lf-say "LF-DEP %s" (lf-load "%s%s.tar" "%s"))' % (d, tars, d, d))
        lines.append('(lf-say "LF-INSTALL" (lf-load "%s%s.tar" "%s"))' % (tars, name, name))
        lines.append('(lf-say "LF-GC-1" (lf-gcn))')
        lines.append('(lf-say "LF-PHASE" "PRE-GC")')
        for pn, s in probes:
            lines.append('(lf-probe "P1.%s" %s)' % (pn, json_lisp_string(s)))
        lines.append('(lf-say "LF-FORCED-GC" (lf-force-gc))')
        lines.append('(lf-say "LF-GC-2" (lf-gcn))')
        lines.append('(lf-say "LF-PHASE" "POST-GC")')
        for pn, s in probes:
            lines.append('(lf-probe "P2.%s" %s)' % (pn, json_lisp_string(s)))
        lines.append('(lf-say "LF-END" "%s")' % name)
        with open(os.path.join(a.out, name + "-ql.lisp"), "w") as f:
            f.write("\n".join(lines) + "\n")
    with open(os.path.join(a.out, "ladder.txt"), "w") as f:
        f.write("\n".join(n + "-ql" for n in names) + "\n")

if __name__ == "__main__":
    main()
