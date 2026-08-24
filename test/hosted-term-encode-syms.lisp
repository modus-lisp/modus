;;;; hosted-term-encode-syms.lisp — TERM-ENCODE SENDS SYMBOLS AND STRINGS BY POINTER.
;;;;
;;;;   ./modus --script test/hosted-term-encode-syms.lisp
;;;;
;;;; ============================================================
;;;; WHAT THIS IS
;;;; ============================================================
;;;;
;;;; The whole per-region collector rests on one rule: no region may hold a
;;;; pointer into another.  For ACTORS the mechanism that is supposed to
;;;; guarantee it is TERM-ENCODE — messages are SERIALISED, copied and not
;;;; shared, so a receiving actor never holds a reference into the sender's
;;;; region.
;;;;
;;;; It does not hold.  TERM-ENCODE dispatches on exactly five shapes — NIL,
;;;; cons, fixnum, array (subtag #x32) and bignum (#x30) — and everything else
;;;; falls off the end into
;;;;
;;;;     ;; Unknown object type — encode as fixnum
;;;;     (setf (mem-ref buf :u8) 1)
;;;;     (setf (mem-ref (+ buf 1) :u64) val)
;;;;
;;;; which writes THE OBJECT'S OWN TAGGED POINTER and labels it a fixnum.  A
;;;; SYMBOL goes that way.  So does a STRING.  Both are ordinary message
;;;; payloads, so cross-actor messages carry cross-region references today —
;;;; the same violation class that is fatal for a worker's fresh intern, sitting
;;;; in the message path.
;;;;
;;;; ============================================================
;;;; WHAT WOULD MAKE THIS A LIE
;;;; ============================================================
;;;;
;;;;   IT IS DEMONSTRATED, NOT READ.  The test encodes a real symbol and then
;;;;   compares the nine bytes that landed against the symbol's own machine
;;;;   word, taken independently with %GC-WORD-OF.  Equality is the finding;
;;;;   the source quoted above is only the explanation.
;;;;
;;;;   THE TAG IS READ WITH %GC-READ64 AND MASKED, not with (MEM-REF buf :U8),
;;;;   which returned 0 for every shape here while the very same buffer read
;;;;   back correct 64-bit words.  An instrument that reports 0 for everything
;;;;   would have failed this test's own control — which is what a control is
;;;;   for, and it caught it.
;;;;
;;;;   IT CARRIES A POSITIVE CONTROL.  A CONS encodes as tag 2 and 19 bytes —
;;;;   a shape TERM-ENCODE really does serialise — so "everything looks like a
;;;;   pointer" is excluded.  If the cons ever reports tag 1 as well, the test
;;;;   is measuring nothing and says so.
;;;;
;;;; WHEN THIS IS FIXED — symbols by NAME, strings by CONTENT — this file goes
;;;; green and becomes the regression test for it.

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "ok   ~a~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "FAIL ~a~%" name))))

(defvar *buf* 0)
(defvar *scratch* 0)
(setq *buf* (%mmap-shared-page 4096))
(setq *scratch* (%mmap-shared-page 4096))

(let* ((buf *buf*)
       (scratch *scratch*)
       (sym (intern "TERM-ENCODE-DEMO-SYMBOL" "COMMON-LISP-USER"))
       (nsym (term-encode sym buf))
       (sym-tag (logand (%gc-read64 buf) 255))
       (sym-payload (%gc-read64 (+ buf 1)))
       (sym-word (%gc-word-of sym scratch)))
  (format t "~&=== A SYMBOL ===~%")
  (format t "~&tag=~d bytes=~d payload=~x symbol's own word=~x~%"
          sym-tag nsym sym-payload sym-word)

  (let* ((nstr (term-encode "term-encode-demo-string" buf))
         (str-tag (logand (%gc-read64 buf) 255)))
    (format t "~&~%=== A STRING ===~%")
    (format t "~&tag=~d bytes=~d~%" str-tag nstr)

    (let* ((ncons (term-encode (cons 1 2) buf))
           (cons-tag (logand (%gc-read64 buf) 255)))
      (format t "~&~%=== A CONS (the positive control) ===~%")
      (format t "~&tag=~d bytes=~d~%" cons-tag ncons)
      (format t "~&~%")

      ;; The control first: if this fails the instrument is broken, not the encoder.
      (chk-true "CONTROL: a cons is really serialised (tag 2)" (eql cons-tag 2))
      (chk-true "a symbol is NOT encoded as a raw pointer"
                (not (eql sym-payload sym-word)))
      (chk-true "a symbol does not use the fixnum tag" (not (eql sym-tag 1)))
      (chk-true "a string does not use the fixnum tag" (not (eql str-tag 1)))))

  (format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
  (if (= *fail* 0)
      (format t "TERM-ENCODE COPIES WHAT IT SENDS: PASS~%")
      (format t "TERM-ENCODE SENDS POINTERS ACROSS THE ACTOR BOUNDARY: FAIL~%"))
  (finish-output))

;;; TOPLEVEL, because SYS-EXIT from inside a nested LET*/IF in a --script does
;;; not take effect — see test/hosted-worker-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
