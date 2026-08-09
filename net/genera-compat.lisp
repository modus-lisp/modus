;;;; genera-compat.lisp — make Modus present as :GENERA.
;;;;
;;;; Modus is an operating system written in Lisp.  Of every implementation
;;;; a portable CL library knows how to reader-conditionalise for, exactly
;;;; two others were ever that: Symbolics Genera and Mezzano.  Every other
;;;; implementation's "-specific" surface is ultimately `ffi:' — a door out
;;;; to C that Modus does not have and does not want.  So when a library
;;;; asks "which Lisp am I on?", the least-wrong answer Modus can give is
;;;; :GENERA: a Lisp that owned its machine, had no C underneath it, and
;;;; whose implementation-specific surface is therefore *scheduler and
;;;; storage* primitives rather than foreign-function glue.
;;;;
;;;; Why not the alternatives (measured over the 69-system local Quicklisp
;;;; corpus, 1211 files):
;;;;   :sbcl     — worst possible.  71 files of SBCL-specific code and 53
;;;;               distinct foreign packages (sb-alien, sb-bsd-sockets,
;;;;               sb-kernel, sb-sys …) inside #+sbcl blocks.
;;;;   :scl/:mkcl/:clasp — best recognition-to-special-casing ratios, but
;;;;               each drags in its parent's internals (CMU / ECL).
;;;;   :mezzano  — family-free and an OS, but bordeaux-threads v0.9.4 gates
;;;;               atomics behind #+(or allegro ccl clasp ecl genera
;;;;               lispworks sbcl) and mezzano is NOT in that list.
;;;;   :genera   — family-free, an OS, and IS in that list.
;;;;
;;;; ---------------------------------------------------------------------
;;;; WHAT GENERA MEANS HERE, AND WHAT IT DOES NOT
;;;;
;;;; Advertising :GENERA is a claim about SHAPE, not about history.  Modus
;;;; is not bit-compatible with a 3600.  Everything in this file is a
;;;; deliberately minimal, honestly-degenerate shim: where Genera had a
;;;; real facility and Modus has none, the shim does the harmless thing and
;;;; says so in a comment rather than pretending.  The specific
;;;; degeneracies are listed at the bottom of this file under KNOWN
;;;; DEGENERACIES — read that before trusting a Genera branch.
;;;; ---------------------------------------------------------------------
;;;;
;;;; !!! SMP LANDMINE — see net/cooperative-atomics.lisp !!!
;;;; The atomic operations reached through this file are atomic only under
;;;; Modus's cooperative single-core scheduler.  The full argument (and the
;;;; three facts it rests on) lives in cooperative-atomics.lisp; every site
;;;; that depends on it is tagged COOPERATIVE-ATOMIC-PRECONDITION.

;;; =====================================================================
;;; 1.  Packages
;;;
;;; Genera's namespace, as the portable corpus actually uses it:
;;;   SCL      Symbolics Common Lisp extensions   (locf, let-globally,
;;;            make-hash-table with storage options)
;;;   SYS      low-level system                   (store-conditional,
;;;            gc-immediately)
;;;   SI       system internals                   (GC reporting knobs)
;;;   PROCESS  the scheduler                      (atomic-incf/-decf)
;;;   CLI      command-loop / table internals     (basic-table-options)
;;;   GRAY-STREAMS   Genera's Gray-stream package (trivial-gray-streams)
;;;   FUTURE-COMMON-LISP  Genera's name for the ANSI CL package
;;;
;;; These are created with :USE NIL — they are namespaces, not CL-using
;;; packages.  Nothing in them shadows a CL symbol.
;;; =====================================================================

;;; SCL on a real Genera is Common Lisp PLUS the Symbolics extensions — every
;;; CL symbol is accessible as `scl:foo'.  This stub is not that; it is the
;;; three extension operators the portable corpus actually reaches for.  That
;;; makes it the wrong SHAPE, and the shape shows: vendored ASDF's Genera
;;; branch does `(:shadowing-import-from :scl :boolean)', and BOOLEAN is a CL
;;; type that SCL would inherit for free on a real Genera.  Exported here as a
;;; correctness fix — SCL:BOOLEAN now names CL:BOOLEAN, which is what it means
;;; on the machine this package is imitating.  IMPORT before EXPORT: without
;;; it, EXPORT would intern a fresh SCL::BOOLEAN and `scl:boolean' would name
;;; a symbol with no type, which is worse than not exporting it at all.
(defpackage "SCL"
  (:use)
  (:import-from "COMMON-LISP" "BOOLEAN")
  (:export "LOCF" "LET-GLOBALLY" "MAKE-HASH-TABLE" "BOOLEAN"))

(defpackage "SYS"
  (:use)
  (:export "STORE-CONDITIONAL" "GC-IMMEDIATELY"))

(defpackage "SI"
  (:use)
  (:export "GC-REPORT-STREAM" "GC-REPORTS-ENABLE" "GC-EPHEMERAL-REPORTS-ENABLE"
           "GC-WARNINGS-ENABLE" "EPHEMERAL-GC-FLIP"))

(defpackage "PROCESS"
  (:use)
  (:export "ATOMIC-INCF" "ATOMIC-DECF"))

;;; CLI is referenced only as `cli::basic-table-options' (double colon), so
;;; the name need not be external — but the PACKAGE must exist or the form
;;; is a READ error, which in Modus drops the whole enclosing toplevel form.
(defpackage "CLI"
  (:use)
  (:export "BASIC-TABLE-OPTIONS"))

;;; =====================================================================
;;; 2.  FUTURE-COMMON-LISP
;;;
;;; On a real Genera, LISP is CLtL1 and FUTURE-COMMON-LISP is the ANSI CL
;;; package.  On Modus, COMMON-LISP *is* the ANSI package — there is no
;;; CLtL1 package to be distinct from.  So FUTURE-COMMON-LISP is not a
;;; separate namespace here; it is another name for COMMON-LISP, installed
;;; as a nickname.  cl-ppcre's
;;;     (:use #-:genera :cl #+:genera :future-common-lisp)
;;;     #+:genera (:shadowing-import-from :common-lisp :lambda :simple-string :string)
;;; then means exactly what the #-:genera branch meant, and the
;;; shadowing-import is a no-op because the three symbols it imports are
;;; already the very symbols inherited.
;;;
;;; A separate package that :USEs CL would NOT work: :USE is not transitive,
;;; so a package using FUTURE-COMMON-LISP would inherit only FCL's own
;;; externals, and every CL symbol would have to be re-exported by hand.
;;; The nickname is both simpler and semantically the truth.
;;; =====================================================================

(defun %genera-add-cl-nickname ()
  (let ((cl (find-package "COMMON-LISP")))
    (when (and cl (not (find-package "FUTURE-COMMON-LISP")))
      (rename-package cl "COMMON-LISP"
                      (cons "FUTURE-COMMON-LISP" (package-nicknames cl))))))

;;; =====================================================================
;;; 3.  GRAY-STREAMS
;;;
;;; trivial-gray-streams' package.lisp does
;;;     (:import-from #+(or abcl genera) :gray-streams  <28 symbols>)
;;; and streams.lisp reads `gray-streams:stream-read-sequence' with a
;;; SINGLE colon, so the 28 symbols must exist AND be external.
;;;
;;; They are plain symbols, not generic functions: trivial-gray-streams
;;; defines its own mirror CLASSES in its own package and only needs the
;;; FUNCTION symbols to be shared, so `defmethod gray-streams:stream-…'
;;; creates the generic function on first use.  Creating them here as
;;; anything else would be a lie about a facility Modus does not have.
;;; See KNOWN DEGENERACIES.
;;; =====================================================================

(defpackage "GRAY-STREAMS"
  (:use)
  (:export
   ;; classes
   "FUNDAMENTAL-STREAM"
   "FUNDAMENTAL-INPUT-STREAM" "FUNDAMENTAL-OUTPUT-STREAM"
   "FUNDAMENTAL-CHARACTER-STREAM" "FUNDAMENTAL-BINARY-STREAM"
   "FUNDAMENTAL-CHARACTER-INPUT-STREAM" "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM"
   "FUNDAMENTAL-BINARY-INPUT-STREAM" "FUNDAMENTAL-BINARY-OUTPUT-STREAM"
   ;; functions
   "STREAM-READ-CHAR" "STREAM-UNREAD-CHAR" "STREAM-READ-CHAR-NO-HANG"
   "STREAM-PEEK-CHAR" "STREAM-LISTEN" "STREAM-READ-LINE"
   "STREAM-CLEAR-INPUT" "STREAM-WRITE-CHAR" "STREAM-LINE-COLUMN"
   "STREAM-START-LINE-P" "STREAM-WRITE-STRING" "STREAM-TERPRI"
   "STREAM-FRESH-LINE" "STREAM-FINISH-OUTPUT" "STREAM-FORCE-OUTPUT"
   "STREAM-CLEAR-OUTPUT" "STREAM-ADVANCE-TO-COLUMN"
   "STREAM-READ-BYTE" "STREAM-WRITE-BYTE"
   ;; the three trivial-gray-streams extends the proposal with, which its
   ;; #+genera branch defines methods on
   "STREAM-READ-SEQUENCE" "STREAM-WRITE-SEQUENCE" "STREAM-FILE-POSITION"))

;;; =====================================================================
;;; 4.  Locatives
;;;
;;; Genera's LOCF returns a LOCATIVE: a first-class pointer to a place.
;;; STORE-CONDITIONAL is its only consumer in the portable corpus
;;; (bordeaux-threads apiv2/atomics.lisp:14 is the ONLY call site), so
;;; Modus defines BOTH ends and the representation is a free choice.
;;;
;;; REPRESENTATION CHOSEN: a locative is ONE closure of two arguments —
;;; a read/write dispatcher over the place's subforms.
;;;
;;;   (scl:locf (svref v 0))
;;;     => (lambda (op val)
;;;          (if (eq op :read) (svref v 0) (setf (svref v 0) val)))
;;;
;;;   read  = (funcall loc :read nil)
;;;   write = (funcall loc :write new)
;;;
;;; Why a closure and not a raw address:
;;;   - Modus's collector COPIES (Cheney semispace).  A raw interior
;;;     address handed out to Lisp code would be invalidated by the next
;;;     GC, silently.  A closure is an ordinary heap object the collector
;;;     already traces and forwards correctly.
;;;   - It works for ANY setf-able place, not just slots the runtime knows
;;;     how to take an address of — locf's whole point.
;;;   - It needs no new object subtag, so it cannot collide with
;;;     runtime/tags.lisp (a documented crash class in this project).
;;;
;;; Why ONE dispatcher closure and not the more obvious
;;; (cons READER WRITER) pair of closures:
;;;
;;;   THE PAIR SHAPE HITS A PRE-EXISTING MODUS COMPILER BUG.  Measured on
;;;   this tree (e4d26a8) with a plain `./modus', no Genera code involved:
;;;
;;;       (defvar *v* (make-array 1))
;;;       (defun k () (cons (lambda () (svref *v* 0)) (lambda () 1)))
;;;       (funcall (car (k)))          ; => UNHANDLED-ESCAPE, swallowed
;;;
;;;   The trigger is TWO closures constructed inside ONE compiled DEFUN
;;;   body where at least one of them references a GLOBAL variable;
;;;   funcalling either of the resulting closures escapes.  ONE closure
;;;   referencing a global is fine; two closures referencing only
;;;   lexicals are fine; the identical cons-of-two-lambdas written at
;;;   toplevel (not inside a defun) is fine.  This matters because LOCF
;;;   expands INSIDE the caller's defun — bordeaux's
;;;   ATOMIC-INTEGER-COMPARE-AND-SWAP — so the pair shape would have put
;;;   two closures in one defun body at exactly the wrong place.
;;;
;;;   The dispatcher shape allocates one closure and sidesteps it
;;;   entirely.  It is also cheaper.  (The underlying compiler bug is
;;;   real and independent of this work; it is reported separately.)
;;;
;;; NOTE the double evaluation: PLACE's subforms are evaluated on every
;;; read and every write, exactly as `setf' of the same place would.
;;; =====================================================================

(defmacro scl::locf (place)
  (let ((op (gensym "LOCOP")) (v (gensym "LOCV")))
    `(lambda (,op ,v)
       (if (eq ,op :read) ,place (setf ,place ,v)))))

(defun %genera-locative-read (loc) (funcall loc :read nil))
(defun %genera-locative-write (loc value) (funcall loc :write value))

;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
;;; Genera contract: returns T if the store happened, NIL if it did not.
;;; bordeaux uses the result directly as ATOMIC-INTEGER-COMPARE-AND-SWAP's
;;; return value, which is documented "Returns T if the replacement was
;;; successful, otherwise NIL".
(defun sys::store-conditional (locative old new)
  (if (eql (%genera-locative-read locative) old)
      (progn (%genera-locative-write locative new) t)
      nil))

;;; =====================================================================
;;; 5.  PROCESS:ATOMIC-INCF / ATOMIC-DECF
;;;
;;; RETURN VALUE IS THE **NEW** VALUE.  bordeaux-threads uses these
;;; UNWRAPPED —
;;;     #+genera `(process:atomic-incf ,place ,delta)
;;; — where its #+sbcl / #+ecl branches wrap the call in (+ … delta) /
;;; (- … delta) because those implementations return the PRIOR value.
;;; ATOMIC-INTEGER-INCF is documented "Returns the new value".  Do not
;;; "fix" these to return the prior value without also changing the
;;; advertised feature.
;;; =====================================================================

;;; These are thin Genera spellings of the primitives in
;;; net/cooperative-atomics.lisp — load that file FIRST.  The atomicity
;;; argument and the SMP landmine warning live there.
;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
(defmacro process::atomic-incf (place &optional (delta 1))
  `(%atomic-incf ,place ,delta))

;;; COOPERATIVE-ATOMIC-PRECONDITION: no LOOP => no YIELD => no interleaving.
(defmacro process::atomic-decf (place &optional (delta 1))
  `(%atomic-decf ,place ,delta))

;;; =====================================================================
;;; 6.  Storage / GC surface (trivial-garbage's #+genera branches)
;;; =====================================================================

;;; Genera's SCL:MAKE-HASH-TABLE accepts storage options CL:MAKE-HASH-TABLE
;;; does not — trivial-garbage passes :GC-PROTECT-VALUES.  Modus's
;;; collector has no weak references at all, so the option is accepted and
;;; ignored, and the table is an ordinary strong hash table.  This is the
;;; SAME strength trivial-garbage gets on Modus today (its non-genera path
;;; refuses to make a weak table too), so nothing is weakened; it just
;;; stops being an error.
(defun scl::make-hash-table (&rest args)
  (let ((clean nil) (rest args))
    (loop
      (when (null rest) (return nil))
      (if (member (car rest) '(:gc-protect-values :gc-protect-keys
                               :store-hash-code :rehash-before-cold
                               :growth-factor :area :locking))
          (setq rest (cddr rest))
          (progn (setq clean (cons (cadr rest) (cons (car rest) clean)))
                 (setq rest (cddr rest)))))
    (apply #'cl:make-hash-table (reverse clean))))

;;; Genera's GC reporting knobs.  Modus's collector is fully automatic and
;;; reports nothing, so these are inert specials that exist only so
;;; SCL:LET-GLOBALLY has something to bind.
(defvar si::gc-report-stream nil)
(defvar si::gc-reports-enable nil)
(defvar si::gc-ephemeral-reports-enable nil)
(defvar si::gc-warnings-enable nil)

;;; SCL:LET-GLOBALLY binds the *global* value of each variable for the
;;; dynamic extent of the body (Genera's answer to "bind a special that
;;; other processes should also see").  Modus is single-core and
;;; cooperative, so a dynamic binding is the whole of that semantics.
;;; PROGV is used rather than LET because the variable names come from the
;;; caller's source and need not be known-special here.
(defmacro scl::let-globally (bindings &rest body)
  `(progv (list ,@(mapcar (lambda (b) (list 'quote (car b))) bindings))
       (list ,@(mapcar #'cadr bindings))
     ,@body))

;;; Modus's collector is triggered by allocation, not by request: there is
;;; no user-callable "collect now" entry point in the shipping image.  Both
;;; of these therefore do nothing and return NIL.  A caller asking for a
;;; full GC gets one on its next allocation, which is the honest answer.
(defun sys::gc-immediately (&optional full) (declare (ignore full)) nil)
(defun si::ephemeral-gc-flip () nil)

;;; Genera hash tables carry a plist of storage options; trivial-garbage
;;; reads :GC-PROTECT-VALUES out of it to answer HASH-TABLE-WEAKNESS.
;;; Modus tables carry no such plist and are never weak, so returning NIL
;;; makes trivial-garbage's
;;;     (if (null (getf (cli::basic-table-options ht) :gc-protect-values t)) …)
;;; take the (getf … t) => T => NIL branch: "this table has no weakness",
;;; which is correct.
(defun cli::basic-table-options (ht) (declare (ignore ht)) nil)

;;; =====================================================================
;;; 7.  Feature advertisement
;;;
;;; Done LAST, and only after every package and operator above exists, so
;;; that no reader-conditional can ever select a Genera branch whose
;;; support has not been installed yet.
;;;
;;; :64-BIT is not a Genera claim — it is simply true of this runtime — but
;;; it is included here because it only becomes load-bearing once :GENERA
;;; is on: bordeaux-threads'
;;;     (deftype %atomic-integer-value () #+32-bit … #+64-bit …)
;;; is only ever reached down a recognised-implementation path, and with
;;; neither feature present the deftype body is empty, which makes the type
;;; NIL and every CHECK-TYPE against it fail.
;;; =====================================================================

(defun %genera-install-features ()
  (when (boundp '*features*)
    (unless (member :64-bit *features*)
      (setq *features* (cons :64-bit *features*)))
    (unless (member :genera *features*)
      (setq *features* (cons :genera *features*)))))

(defun %init-genera-compat ()
  (handler-case (%genera-add-cl-nickname) (t (c) nil))
  (%genera-install-features)
  t)

(%init-genera-compat)

;;; =====================================================================
;;; KNOWN DEGENERACIES  (what a Genera branch will and will not get)
;;;
;;;  * WEAK REFERENCES DO NOT EXIST.  SCL:MAKE-HASH-TABLE ignores
;;;    :GC-PROTECT-VALUES; trivial-garbage's genera weak-pointer path keeps
;;;    its objects alive forever through a strong hash table.  This is not a
;;;    regression — Modus's non-genera trivial-garbage path has no weak
;;;    references either — but a program that RELIES on weakness to bound
;;;    memory will grow without limit.
;;;
;;;  * GC IS NOT REQUESTABLE.  SYS:GC-IMMEDIATELY and SI:EPHEMERAL-GC-FLIP
;;;    are no-ops.
;;;
;;;  * GRAY-STREAMS IS A NAMESPACE, NOT AN IMPLEMENTATION.  The 28 symbols
;;;    exist so trivial-gray-streams can load; they are not wired into
;;;    Modus's own stream dispatch.  A Gray stream class defined through
;;;    trivial-gray-streams will therefore NOT be usable by CL:READ-CHAR
;;;    and friends.  Wiring Modus's stream system to dispatch through these
;;;    generic functions is the real fix and is out of scope here.
;;;
;;;  * FINALIZERS.  trivial-garbage's #+genera branch signals
;;;    "Finalizers are not available in Genera." — which is TRUE of Modus
;;;    as well, and is a better outcome than the silent empty body the
;;;    unrecognised-implementation path produces.
;;;
;;;  * GENERA PREDATES ANSI.  Some #+genera branches in the wild are
;;;    decades stale.  Anything that loads down a Genera path should be
;;;    behaviour-probed, not assumed.  The per-library evidence for this
;;;    tree is in the task #237 report.
;;; =====================================================================
