;;;; modus.asd - ASDF system definition for Modus
;;;;
;;;; Loads the MVM system (compiler, translators, cross-compiler) into the host Lisp.
;;;; The file list lives in ONE place, lib/load-mvm.lisp, and this defers to it: the
;;;; sources must be LOADed form by form (compiler.lisp uses #.(compute-name-hash ...)
;;;; after defining it earlier in the same file), which COMPILE-FILE cannot do.
;;;; Images are built by the scripts in mvm/: `sbcl --script mvm/build.lisp --list`
;;;; names every one, and scripts/build-matrix.sh builds them all.

(asdf:defsystem :modus
  :description "Modus - bare-metal Lisp OS via MVM"
  :version "0.2.0"
  :author "Modus Project"
  :license "MIT"
  :depends-on ()
  :components ((:static-file "lib/load-mvm.lisp"))
  :perform (asdf:load-op (o c)
             (load (asdf:system-relative-pathname c "lib/load-mvm.lisp"))))
