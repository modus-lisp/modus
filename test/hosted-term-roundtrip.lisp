;;;; hosted-term-roundtrip.lisp — THE SERIALISER ROUND-TRIPS, AND LEAVES NO POINTER.
;;;;
;;;;   ./modus --script test/hosted-term-roundtrip.lisp
;;;;
;;;; ============================================================
;;;; WHY THIS EXISTS, AND WHAT IT REPLACES
;;;; ============================================================
;;;;
;;;; A serialiser test that does not ROUND-TRIP is not a serialiser test.
;;;; test/hosted-term-encode-syms.lisp checks the TAG and the BYTE COUNT — that
;;;; a symbol is no longer shipped as its own pointer — and it passed an encoder
;;;; that destroyed every payload it touched, because it never looked at what
;;;; came back.  The strings decoded to the right LENGTH and no content and the
;;;; symbols to NIL, and twenty-plus green tests said nothing, because nothing
;;;; in the bar sends a string or a symbol through the serialiser.
;;;;
;;;; So this asserts THE DECODED VALUE.  `(equal (rt "hello") "hello")'.
;;;; `(eq (rt sym) (intern "..."))'.  Nothing about tags.
;;;;
;;;; ============================================================
;;;; IT USES THE REAL STAGING BUFFER
;;;; ============================================================
;;;;
;;;; (STAGING-BASE-ADDR) after %HA-ACTORS-BRINGUP — the same memory the live
;;;; actor path encodes into.  An earlier harness used a private
;;;; %MMAP-SHARED-PAGE and faulted on CONS, a path nobody had touched, which
;;;; sent a whole diagnostic down the wrong hole.  Use the buffer the system
;;;; uses.
;;;;
;;;; ============================================================
;;;; AND THE AUDIT, WHICH IS THE ACTUAL POINT
;;;; ============================================================
;;;;
;;;; Round-tripping correctly is necessary and not sufficient: a decoder that
;;;; returned the sender's own object would round-trip perfectly and still be
;;;; the bug.  So the values are also required to be DIFFERENT OBJECTS from the
;;;; ones encoded — a copy, not a share — and for the symbol, EQ to what
;;;; interning the same name gives HERE, which is what "the receiver interns"
;;;; means.
;;;;
;;;; WHAT WOULD MAKE THIS A LIE: if TERM-ENCODE silently stopped being called,
;;;; every check would pass on values that never went near a buffer.  The CONS
;;;; case is the guard — it is a shape the serialiser has always handled, and it
;;;; must come back EQUAL but NOT EQ.  If a cons ever comes back EQ, the harness
;;;; is not serialising anything and says so.

(defvar *fail* 0)
(defvar *checks* 0)

(defun chk-true (name got)
  (setq *checks* (+ *checks* 1))
  (if got
      (format t "ok   ~a~%" name)
      (progn (setq *fail* (+ *fail* 1)) (format t "FAIL ~a~%" name))))

(defun chk (name got want)
  (setq *checks* (+ *checks* 1))
  (if (equal got want)
      (format t "ok   ~a = ~s~%" name got)
      (progn (setq *fail* (+ *fail* 1))
             (format t "FAIL ~a: got ~s want ~s~%" name got want))))

;; The actor band has to exist before there is a staging buffer to encode into.
(%ha-actors-bringup 16 0)

(defun rt (v)
  "Encode V into the real staging buffer and decode it straight back."
  (let ((buf (staging-base-addr)))
    (term-encode v buf)
    (setf (mem-ref (decode-ptr-addr) :u64) buf)
    (term-decode-step)))

(format t "~&=== THE GUARD: a shape the serialiser has always handled ===~%")
(let* ((c (cons 11 22))
       (got (rt c)))
  (chk "a cons round-trips" got (cons 11 22))
  (chk-true "and it is a COPY, not the same object (else nothing is being serialised)"
            (not (eq got c))))

(format t "~&~%=== STRINGS, BY VALUE ===~%")
(let* ((s "hello")
       (got (rt s)))
  (chk "a string round-trips" got "hello")
  (chk-true "it is really a string" (stringp got))
  (chk-true "and it is a COPY, not the sender's object" (not (eq got s))))
(chk "an empty string round-trips" (rt "") "")
(chk "a longer string round-trips"
     (rt "the quick brown fox jumps over the lazy dog")
     "the quick brown fox jumps over the lazy dog")

(format t "~&~%=== SYMBOLS, BY NAME — THE RECEIVER INTERNS ===~%")
(let* ((sym (intern "TERM-RT-DEMO" "COMMON-LISP-USER"))
       (got (rt sym)))
  (chk-true "a symbol comes back a symbol" (symbolp got))
  (chk "its name survives" (symbol-name got) "TERM-RT-DEMO")
  ;; THE POINT: EQ to what interning that name gives on THIS side.  That is
  ;; what makes the decoded symbol the receiver's own object rather than a
  ;; pointer into the sender.
  (chk-true "and it is EQ to interning the same name here"
            (eq got (intern "TERM-RT-DEMO" "COMMON-LISP-USER"))))

(let ((got (rt :term-rt-keyword)))
  (chk-true "a keyword comes back a keyword" (keywordp got))
  (chk-true "and it is EQ to the same keyword here" (eq got :term-rt-keyword)))

(let* ((g (make-symbol "UNINTERNED-GG"))
       (got (rt g)))
  (chk-true "an uninterned symbol comes back a symbol" (symbolp got))
  (chk "its name survives" (symbol-name got) "UNINTERNED-GG")
  ;; Two actors cannot share an uninterned symbol by name, and must not pretend
  ;; to: the receiver gets its OWN.
  (chk-true "and it is NOT the sender's object" (not (eq got g))))

(format t "~&~%=== NESTED, because that is how real messages look ===~%")
(chk "a list of a symbol and a string"
     (rt (cons (intern "TERM-RT-DEMO" "COMMON-LISP-USER") (cons "hello" 7)))
     (cons (intern "TERM-RT-DEMO" "COMMON-LISP-USER") (cons "hello" 7)))

;;; ============================================================
;;; AND THE SAME THING THROUGH THE REAL MAILBOX
;;; ============================================================
;;;
;;; THE GAP THAT LET A BROKEN SERIALISER SHIP GREEN.  Four actor tests exercise
;;; TERM-ENCODE/TERM-DECODE today and pass; every one of them sends fixnums and
;;; conses, so a serialiser that destroyed every string and symbol it touched
;;; went past twenty-plus green tests without a murmur.  This closes it, and it
;;; should outlive the campaign that prompted it.
;;;
;;; It is not the same code path as the section above.  RT calls TERM-ENCODE and
;;; TERM-DECODE-STEP directly; SEND decides for itself whether a value needs
;;; serialising, picks a staging buffer, and RECEIVE deserialises on the far
;;; side.  The primordial actor is id 1 and runs on the process stack, so
;;; sending to itself puts the value through all of that for real.
;;;
;;; WHAT THIS SECTION DOES *NOT* PROVE, AND IT IS THE THING THE DESIGN IS FOR.
;;; The sender and the receiver here are ONE ACTOR IN ONE REGION, so
;;;
;;;     (eq got (intern "MBOX-DEMO-SYM" "COMMON-LISP-USER"))
;;;
;;; passes for a reason weaker than the one that matters: there is only one
;;; intern table and one heap, so it would pass even if the decoder had handed
;;; back the sender's own pointer.  What it establishes is that the SERIALISER
;;; round-trips through the real SEND/RECEIVE path — which is exactly the gap
;;; that let a broken one ship green, and worth having on its own.
;;;
;;; The claim it does not reach is the cross-region one: that a symbol decoded
;;; by an actor in ANOTHER region leaves ZERO pointers from that actor's region
;;; into the sender's, per %GC-COUNT-FOREIGN-REFS, with the control still able
;;; to answer non-zero.  That needs two actors on two regions and is NOT
;;; TESTED HERE.  Do not read this section as if it were.

(format t "~&~%=== THROUGH THE REAL MAILBOX (SEND / RECEIVE) ===~%")

(defun via-mailbox (v) (send 1 v) (receive))

(chk "a fixnum survives the mailbox" (via-mailbox 42) 42)
(chk "a cons survives the mailbox" (via-mailbox (cons 1 2)) (cons 1 2))
(chk "A STRING survives the mailbox" (via-mailbox "hello") "hello")
(let ((got (via-mailbox "the quick brown fox")))
  (chk "a longer string survives the mailbox" got "the quick brown fox")
  (chk-true "and it really is a string" (stringp got)))
(let ((got (via-mailbox (intern "MBOX-DEMO-SYM" "COMMON-LISP-USER"))))
  (chk-true "A SYMBOL survives the mailbox as a symbol" (symbolp got))
  (chk "its name survives" (symbol-name got) "MBOX-DEMO-SYM")
  (chk-true "and it is EQ to interning that name on this side"
            (eq got (intern "MBOX-DEMO-SYM" "COMMON-LISP-USER"))))
(let ((got (via-mailbox :mbox-demo-kw)))
  (chk-true "a keyword survives the mailbox" (keywordp got))
  (chk-true "and it is EQ to the same keyword here" (eq got :mbox-demo-kw)))
(chk "a symbol and a string together survive the mailbox"
     (via-mailbox (cons (intern "MBOX-DEMO-SYM" "COMMON-LISP-USER") (cons "hello" 7)))
     (cons (intern "MBOX-DEMO-SYM" "COMMON-LISP-USER") (cons "hello" 7)))

(format t "~&~%~d checks, ~d failed~%" *checks* *fail*)
(if (= *fail* 0)
    (format t "TERM SERIALISER ROUND-TRIP: PASS~%")
    (format t "TERM SERIALISER ROUND-TRIP: FAIL~%"))
(finish-output)

;;; TOPLEVEL — SYS-EXIT from inside a nested LET*/IF in a --script does not take
;;; effect; see test/hosted-worker-xregion.lisp.
(sys-exit (if (= *fail* 0) 0 1))
