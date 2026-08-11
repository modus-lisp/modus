;;; Task #244 SYNTHETIC reproducer of the forward-#'FN raw-offset escape.
;;; Load with esc2/fwd4.lisp (which loads iterate's package.lisp first, then
;;; this file, via %it-eval-source with *package* = ITERATE -- the same loader
;;; install-tarball uses).  On a pre-fix binary R1 = (:ERR #(SIMPLE-ERROR NIL))
;;; (a recovered SIGSEGV); with the fix R1 = (:INT NIL :FNP T :S (1 2) :M (101 102)).
;;; NB the escape is sensitive to the module's fn-name keying, so trimming the
;;; filler defuns or their bodies can route #'KA5 to a runtime stub instead and
;;; hide it -- keep this shape.
(eval-when (:compile-toplevel :execute)
  (defun ka1 (l)
    (let* ((s (sort (copy-list l) #'< :key #'ka5))
           (m (mapcar #'ka5 s)))
      (list :int (integerp #'ka5) :fnp (functionp #'ka5) :s s :m m)))
  (defun ka2 (n) (intern (format nil "!~d" n)))
  (defun ka3 (form vars) (if (consp form) vars vars))
  (defun ka4 (sym) (char= (char (symbol-name sym) 0) #\!))
  (defun ka5 (x) (+ x 100)))
