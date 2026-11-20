;;;; dict.lisp DICT INPUT OUT -- INPUT as a raw LZMA1 stream (lc 2 lp 1 pb 2, no
;;;; end marker) against the preset dictionary DICT, by cram's encoder: what
;;;; run.sh feeds the harness to check mac68k-lzma.inc's dictionary reads.
(load (merge-pathnames "../../scripts/mac68k-tools.lisp" *load-truename*))
(in-package #:mac68k-tools)
(load-cram)
(destructuring-bind (dict input out) (argv)
  (write-bytes out (funcall (find-symbol "LZMA-COMPRESS" "CRAM") (read-bytes input)
                            :dict (read-bytes dict) :lc 2 :lp 1 :pb 2 :end-marker nil)))
