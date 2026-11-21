;;;; mac68k-reloc.lisp -- the relocations of a Mac image, from two builds of it
;;;; whose whole memory map is SHIFT apart.
;;;;
;;;;   sbcl --script scripts/mac68k-reloc.lisp A B SHIFT [OUT]
;;;;
;;;; SHIFT is hex, a multiple of 512 KB (boot-mac68k's MODUS_MAC68K_SHIFT).  A word
;;;; that holds an address differs between the two by SHIFT; one that holds an
;;;; address as a tagged fixnum (address << 1) by twice that; the allocation sites'
;;;; kind-map base (kind map - region / 8) by 7/8 of it.  So a relocation's KIND is
;;;; eighths of the shift: 8, 16 or 7.  With SHIFT and the run-time shift both
;;;; multiples of 512 KB (64 KB x 8), no relocated value's low 16 bits move, and
;;;; every relocation is one 16-bit word to which the loader adds
;;;; ((shift >> 16) * KIND) / 8.  That also covers code that builds an address from
;;;; two separate 16-bit halves.
;;;;
;;;; Prints a summary and every difference that is neither (with the symbol map's
;;;; function around it, when A's .map exists and LOAD=HEX is set); with OUT,
;;;; writes the table: the number of kinds, then per kind its KIND, its count and
;;;; its offsets, ascending, all big-endian u32s.  Exit 0 only with no others.
;;;;
;;;; Also a library: (defvar cl-user::*mac68k-reloc-library* t) before loading it,
;;;; then MAC68K-RELOC:CLASSIFY.  It replaced mac68k-reloc.py, byte for byte
;;;; (2026-11-21).

(load (merge-pathnames "mac68k-tools.lisp" *load-truename*))
(defpackage #:mac68k-reloc (:use #:cl #:mac68k-tools) (:export #:classify #:+kinds+))
(in-package #:mac68k-reloc)

(defparameter +kinds+ '(8 16 7))

(defun die (fmt &rest args)
  (format *error-output* "~&mac68k-reloc: ~?~%" fmt args)
  (sb-ext:exit :code 1 :abort t))

(defun classify (a b shift)
  "(values an alist KIND -> its word offsets, ascending; and a list of
   (offset word-a word-b) that are none of them)."
  (unless (zerop (mod shift #x80000)) (die "SHIFT must be a multiple of 512 KB"))
  (unless (= (length a) (length b))
    (die "the builds differ in size (~D vs ~D): the layout moved, not just addresses"
         (length a) (length b)))
  (let* ((hi (ash shift -16))
         (by-d (mapcar (lambda (k) (cons (logand (floor (* hi k) 8) #xFFFF) k)) +kinds+))
         (kinds (mapcar #'list +kinds+))
         (odd nil))
    (loop for i from 0 below (logand (length a) (lognot 1)) by 2
          unless (and (= (aref a i) (aref b i)) (= (aref a (1+ i)) (aref b (1+ i))))
            do (let* ((wa (logior (ash (aref a i) 8) (aref a (1+ i))))
                      (wb (logior (ash (aref b i) 8) (aref b (1+ i))))
                      (k (cdr (assoc (logand (- wb wa) #xFFFF) by-d))))
                 (if k
                     (push i (cdr (assoc k kinds)))
                     (push (list i wa wb) odd))))
    (values (mapcar (lambda (e) (cons (car e) (nreverse (cdr e)))) kinds) (nreverse odd))))

(defun fn-map (path)
  "The symbol map's (address . name) rows, by address, or NIL."
  (when (probe-file path)
    (with-open-file (in path)
      (sort (loop for line = (read-line in nil) while line
                  unless (and (plusp (length line)) (char= (char line 0) #\#))
                    append (let ((p (loop with s = 0 for i = (position #\Tab line :start s)
                                          collect (subseq line s i) while i do (setq s (1+ i)))))
                             (when (>= (length p) 4)
                               (list (cons (parse-integer (first p) :radix 16) (fourth p))))))
            #'< :key #'car))))

(defun main (a-path b-path shift-hex &optional out)
  (let ((a (read-bytes a-path)) (b (read-bytes b-path)) (shift (parse-integer shift-hex :radix 16)))
    (multiple-value-bind (kinds odd) (classify a b shift)
      (flet ((n (k) (length (cdr (assoc k kinds)))))
        (format t "mac68k-reloc: ~D bytes; ~D address words, ~D tagged-address words, ~D kind-map words, ~D other differences~%"
                (length a) (n 8) (n 16) (n 7) (length odd)))
      (let* ((fns (fn-map (make-pathname :type "map" :defaults a-path)))
             (load (and fns (env "LOAD") (parse-integer (env "LOAD") :radix 16))))
        (loop for (off wa wb) in odd repeat 200
              do (format t "  +~7,'0X  ~4,'0X -> ~4,'0X  ~A~%" off wa wb
                         (if (and fns load)
                             (let ((r (find-if (lambda (r) (<= (car r) (+ load off))) fns :from-end t)))
                               (if r (cdr r) "?"))
                             "")))
        (when (> (length odd) 200)
          (format t "  ... ~D more~%" (- (length odd) 200))))
      (when out
        (let ((words (list (length +kinds+))))
          (dolist (k +kinds+)
            (let ((offs (cdr (assoc k kinds))))
              (setq words (append words (list k (length offs)) offs))))
          (let ((v (make-array (* 4 (length words)) :element-type '(unsigned-byte 8))))
            (loop for w in words for i from 0 by 4 do (fs-apple:set-u32 v i w))
            (write-bytes out v)))
        (format t "mac68k-reloc: ~A, ~D relocations~%" out
                (reduce #'+ kinds :key (lambda (e) (length (cdr e))))))
      (if odd 1 0))))

(unless (and (boundp 'cl-user::*mac68k-reloc-library*) (symbol-value 'cl-user::*mac68k-reloc-library*))
  (sb-ext:exit :code (apply #'main (argv))))
