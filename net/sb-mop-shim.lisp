;;;; sb-mop-shim.lisp — the SB-MOP SURFACE.  A namespace, not an implementation.
;;;;
;;;; This file is NOT compiled into the image.  It is carried as a source string and evaluated at
;;;; boot, like net/sb-thread-shim.lisp: it publishes a package that already exists on the host
;;;; reading the build, and CHECK-SOURCE-PARSES reads every first-party build source with SBCL's
;;;; own reader.  It therefore names SB-MOP only as a STRING and never writes an `sb-mop::x' token,
;;;; which would have to intern into SBCL's package-LOCKED SB-MOP and fail the build.
;;;;
;;;; WHY THIS IS TWENTY LINES AND NOT TWO HUNDRED.  Modus already implements the MOP: CLASS-SLOTS,
;;;; CLASS-DIRECT-SLOTS, CLASS-PRECEDENCE-LIST, CLASS-DIRECT-SUPERCLASSES, CLASS-FINALIZED-P,
;;;; FINALIZE-INHERITANCE, VALIDATE-SUPERCLASS, nine SLOT-DEFINITION-* accessors, the METHOD-* and
;;;; GENERIC-FUNCTION-* accessors and COMPUTE-APPLICABLE-METHODS are all ordinary functions in
;;;; mvm/cl-clos.lisp.  What portable code cannot find is not the behaviour but the NAME: it writes
;;;; `sb-mop:class-slots', and modus answers to `class-slots'.  So the whole job is to publish the
;;;; functions modus already has under the package the ecosystem spells them in.  Anything that
;;;; belongs in a MOP and is missing gets FIXED IN cl-clos.lisp, where every caller benefits —
;;;; never reimplemented here, or the two copies answer differently and both look right.
;;;;
;;;; :SB-MOP IS PUSHED, :SBCL IS NOT.  Same rule as the thread shim — modus is not SBCL and code
;;;; asking whether it is still gets the right answer.  NOTE THE HAZARD, because it differs from
;;;; :SB-THREAD: real SBCL does NOT push :SB-MOP, so `#+sb-mop' alone is FALSE on SBCL.  Portable
;;;; code wanting "a host with a MOP surface" must write `#+(or sbcl sb-mop)'.
;;;;
;;;; QUOTED SYMBOLS, NOT FIND-SYMBOL.  Measured, and it is the opposite of the usual advice: at the
;;;; moment this file runs, (find-symbol "CLASS-SLOTS" "COMMON-LISP-USER") answers NIL even though
;;;; CLASS-SLOTS is fbound and callable.  Modus's image-baked symbols are not in the package's
;;;; symbol table until the READER interns them, so FIND-SYMBOL sees nothing and a probe that reads
;;;; the name first interns it and then "finds" it — measuring only itself.  Reading `(quote
;;;; class-slots)' here resolves to the very symbol the rest of the image calls.
;;;;
;;;; ALL-OR-NOTHING.  Every name is checked BEFORE the package is created, and :SB-MOP is pushed
;;;; only after the last IMPORT succeeds.  A half-populated SB-MOP is the worst outcome available:
;;;; the feature would advertise a MOP while `sb-mop:class-slots' signalled UNDEFINED-FUNCTION, and
;;;; consumers guarding on #+sb-mop would take the branch that cannot work.  Measured — an earlier
;;;; revision did exactly that, and warp's inspector died at the call site instead of degrading.
;;;;
;;;; ONE KNOWN GAP.  SLOT-DEFINITION-ALLOCATION always answers :INSTANCE.  Modus tracks
;;;; :ALLOCATION :CLASS per class in *CLOS-CLASS-SLOTS*, but a slot definition here is a bare
;;;; SYMBOL naming no owning class, so the accessor cannot reach that registry.  It is a limit of
;;;; the representation, documented at the function, and NOT papered over here.

(let ((%mop-syms (list 'class-slots 'class-direct-slots 'class-precedence-list
                       'class-direct-superclasses 'class-finalized-p 'finalize-inheritance
                       'validate-superclass 'class-default-initargs
                       'slot-definition-name 'slot-definition-allocation
                       'slot-definition-initargs 'slot-definition-initform
                       'slot-definition-initfunction 'slot-definition-type
                       'slot-definition-readers 'slot-definition-writers
                       'slot-definition-location
                       'generic-function-name 'generic-function-methods
                       'generic-function-lambda-list
                       'method-qualifiers 'method-specializers 'method-function
                       'method-generic-function
                       'compute-applicable-methods 'compute-applicable-methods-using-classes
                       'compute-class-precedence-list 'standard-instance-access)))
  ;; check every one before creating anything, so a missing name leaves NO SB-MOP behind
  (dolist (%s %mop-syms)
    (when (not (fboundp %s))
      (error "sb-mop: ~s is not fbound — the MOP function it names is missing from cl-clos.lisp, ~
              which is where it belongs; re-exporting it would publish a name that signals ~
              UNDEFINED-FUNCTION at the call site" %s)))
  (let ((%pkg (or (find-package "SB-MOP") (make-package "SB-MOP" :use '()))))
    (dolist (%s %mop-syms)
      (import %s %pkg)
      (export %s %pkg)))
  (pushnew :sb-mop *features*))
