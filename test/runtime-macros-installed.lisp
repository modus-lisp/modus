;;; Every entry of *MODUS-RUNTIME-MACROS* must actually be installed.  The
;;; installer is unrolled to a fixed length, and entries past it used to be
;;; silently dropped (PPRINT-LOGICAL-BLOCK, PPRINT-POP, DO-ALL-SYMBOLS ...).
(defparameter *missing*
  (let ((acc nil))
    (dolist (src *modus-runtime-macros* (nreverse acc))
      (let ((name (cadr (read-from-string src))))
        (unless (macro-function name) (push name acc))))))
(format t "~&runtime-macros-installed: ~a of ~a missing ~s~%"
        (length *missing*) (length *modus-runtime-macros*) *missing*)
(format t "~&runtime-macros-installed: ~a~%" (if *missing* "FAIL" "PASS"))
(sys-exit (if *missing* 1 0))
