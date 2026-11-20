;;;; fixed-address-allowlist.lisp — the RATCHET's debt register (CHECK H4).
;;;;
;;;; Literal uses of fixed addresses that are NOT in the registry
;;;; (mvm/fixed-addresses.lisp), as (FILE KIND VALUE COUNT).  KIND :ABS is an
;;;; absolute #x1000xxxx literal; :WINDOW / :THR-TABLE an offset literal
;;;; (+ BASE #xOFF).  A use not listed here, or more uses than COUNT, fails the
;;;; build.  THIS LIST MAY ONLY SHRINK: register the word instead of adding a
;;;; row.  Regenerate (to shrink) with MODUS_FIXED_ADDR_ALLOWLIST_WRITE=1.
;;;; Data, not code: read with READ, never loaded or baked.

(
 ("mvm/build-cl-repl-common.lisp" :ABS #x1000FFFF 1)
)
