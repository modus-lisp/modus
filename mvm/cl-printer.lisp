;;;; cl-printer.lisp — Printer and format
;;;; Part of the Modus CL runtime. Depends on cl-streams.lisp.

;;; ============================================================
;;; Layer 3: Printer — respects *print-* variables
;;; ============================================================

;;; Write a single char code to stream (may be nil for *standard-output*)
(defun %print-char (code stream)
  (write-char-to-stream code stream))

;;; Write a string to stream
(defun %print-string-raw (str stream)
  ;; LENGTH (fp-aware) so fp-wrapped strings print only their active prefix.
  (let ((len (length str)) (i 0))
    (loop
      (when (= i len) (return nil))
      ;; %print-char wants a raw char-CODE.  Public AREF now lifts string
      ;; elements to characters (and peels fp/adj/displaced wrappers), so
      ;; char-code converts back.  %ensure-char-code tolerates either a
      ;; character or a stray fixnum element.
      (%print-char (%ensure-char-code (aref str i)) stream)
      (setq i (+ i 1)))))

;;; Write a decimal integer (always base 10).  Routes bignums through
;;; the same %bignum-divmod-fixnum path as %print-integer-in-base so
;;; FRESH-LINE / radix prefixes / ratio numerator-denominator printing
;;; etc. all work for bignum operands.
(defun %print-decimal-to-stream (n stream)
  (cond
    ((bignump n)
     (%print-integer-in-base n 10 stream))
    ((< n 0)
     (%print-char 45 stream)
     (%print-decimal-to-stream (- 0 n) stream))
    ((= n 0)
     (%print-char 48 stream))
    (t
     (let ((digits nil) (tmp n))
       (loop
         (when (= tmp 0) (return nil))
         (setq digits (cons (+ 48 (mod tmp 10)) digits))
         (setq tmp (truncate tmp 10)))
       (dolist (d digits) (%print-char d stream))))))

;;; Digit char for base N (0-9 A-Z)
(defun %digit-char-upper (n)
  (if (< n 10) (+ 48 n) (+ 55 n)))   ; 55 = 65-10

;;; Divide bignum or fixnum N by a small fixnum divisor D (positive).
;;; Returns (cons quotient remainder) where remainder is 0..D-1.
;;; Quotient may collapse to a fixnum.  D ≤ 36 so r-hi*2^31 stays well
;;; below 2^62 (no internal overflow).
(defun %bignum-divmod-fixnum (n d)
  "Divide bignum or fixnum N by positive fixnum D ≤ 36.
   Returns (cons quotient remainder).  Handles big-bignum N via
   limb-by-limb digit-recurrence (process limbs MSB-first, each limb
   split into two 31-bit halves so half * (D*2^31) + previous-remainder
   fits in a 62-bit fixnum)."
  (cond
    ((not (bignump n))
     (if (< n 0)
         (let* ((np (- 0 n)) (q (truncate np d)) (r (mod np d)))
           (if (= r 0) (cons (- 0 q) 0) (cons (- 0 (+ q 1)) (- d r))))
         (cons (truncate n d) (mod n d))))
    ;; Big bignum: process limbs from MSB to LSB.  Each limb is 62-bit,
    ;; split into two 31-bit halves so (r * 2^31 + half) fits in
    ;; (D-1)*2^31 + (2^31-1) ≤ D*2^31 ≤ 36 * 2^31 ≈ 2^36.2 — well under 2^62.
    ((and (bignump n) (big-bignum-p n))
     (let ((sign (%bb-sign n))
           (nlimbs (%bb-nlimbs n)))
       (let ((quot-halves nil)   ; collected MSB-first as half-limbs
             (r 0)
             (i (- nlimbs 1)))
         (loop (when (< i 0) (return nil))
           (let* ((limb (%bb-limb n i))
                  (hi (ash limb -31))
                  (lo (logand limb 2147483647))
                  ;; Process high half.
                  (p1 (+ (* r 2147483648) hi))
                  (q1 (truncate p1 d))
                  (r1 (mod p1 d))
                  ;; Process low half.
                  (p2 (+ (* r1 2147483648) lo))
                  (q2 (truncate p2 d))
                  (r2 (mod p2 d)))
             (setq quot-halves (cons q1 quot-halves))
             (setq quot-halves (cons q2 quot-halves))
             (setq r r2))
           (setq i (- i 1)))
         ;; Each iteration pushed HI then LO onto quot-halves; iterations
         ;; ran MSB → LSB across input limbs.  After the final push, the
         ;; FRONT of quot-halves is q[0].lo (LSB limb's low half), then
         ;; q[0].hi, then q[1].lo, q[1].hi, …, q[n-1].lo, q[n-1].hi.
         ;; That's already LSB-first with (lo, hi) per limb — exactly
         ;; what %halves-to-limbs consumes.  Reversing was the bug that
         ;; made (prin1 (expt 11 40)) etc. produce scrambled limbs.
         (let* ((q-limbs (%halves-to-limbs quot-halves))
                (q-mag (%make-bb 1 q-limbs))
                (q (if (= sign -1)
                       (if (= r 0) (bignum-negate q-mag)
                           ;; Floor-style: when remainder nonzero on
                           ;; negative dividend, q = -(q+1), r = d-r.
                           (bignum-negate (bignum-add q-mag 1)))
                       q-mag))
                (r-out (if (and (= sign -1) (> r 0)) (- d r) r)))
           (cons q r-out)))))
    ;; Small bignum: original 2-slot path.
    (t
     (let ((hi (bignum-hi n)))
       (if (< hi 0)
           (let* ((mag (bignum-negate n))
                  (qr  (%bignum-divmod-fixnum mag d))
                  (q (car qr)) (r (cdr qr)))
             (if (= r 0)
                 (cons (bignum-negate q) 0)
                 (cons (bignum-negate (bignum-add q 1)) (- d r))))
           (let* ((lo (bignum-lo n))
                  (q-hi (truncate hi d))
                  (r-hi (mod hi d))
                  (lo-hi31 (ash lo -31))
                  (lo-lo31 (logand lo 2147483647))
                  (partial1 (+ (* r-hi 2147483648) lo-hi31))
                  (q1 (truncate partial1 d))
                  (r1 (mod partial1 d))
                  (partial2 (+ (* r1 2147483648) lo-lo31))
                  (q2 (truncate partial2 d))
                  (r2 (mod partial2 d))
                  (q-lo (+ (ash q1 31) q2))
                  (q (if (= q-hi 0)
                         q-lo
                         (bignum-to-fixnum-if-possible
                           (make-bignum (logand q-lo 4611686018427387903) q-hi)))))
             (cons q r2)))))))

;;; Print bignum or fixnum in given base.  Bignums route through
;;; %bignum-divmod-fixnum; pure fixnums use native (mod/truncate).
(defun %print-integer-in-base (n base stream)
  (cond
    ((bignump n)
     (if (or (and (big-bignum-p n) (= (%bb-sign n) -1))
             (and (not (big-bignum-p n)) (< (bignum-hi n) 0)))
         (progn (%print-char 45 stream)
                (%print-integer-in-base (bignum-negate n) base stream))
         (let ((digits nil) (tmp n))
           (loop
             (when (and (not (bignump tmp)) (= tmp 0)) (return nil))
             (let* ((qr (%bignum-divmod-fixnum tmp base))
                    (q  (car qr)) (r (cdr qr)))
               (setq digits (cons (%digit-char-upper r) digits))
               (setq tmp q)))
           (if (null digits)
               (%print-char 48 stream)
               (dolist (d digits) (%print-char d stream))))))
    ((< n 0)
     (%print-char 45 stream)
     (%print-integer-in-base (- 0 n) base stream))
    ((= n 0)
     (%print-char 48 stream))
    (t
     (let ((digits nil) (tmp n))
       (loop
         (when (= tmp 0) (return nil))
         (setq digits (cons (%digit-char-upper (mod tmp base)) digits))
         (setq tmp (truncate tmp base)))
       (dolist (d digits) (%print-char d stream))))))

;;; Print radix prefix: #b, #o, #x, #Nr. Base 10 with *print-radix* is
;;; conventionally written with a TRAILING dot (not a leading prefix), so
;;; this function skips base 10 — caller is responsible for emitting the
;;; trailing dot in that case.
(defun %print-radix-prefix (base stream)
  (cond
    ((= base 2)  (%print-char 35 stream) (%print-char 98 stream))  ; #b
    ((= base 8)  (%print-char 35 stream) (%print-char 111 stream)) ; #o
    ((= base 10) nil)                                              ; handled by trailing dot
    ((= base 16) (%print-char 35 stream) (%print-char 120 stream)) ; #x
    (t (%print-char 35 stream)
       (%print-decimal-to-stream base stream)
       (%print-char 114 stream))))  ; #Nr

;;; Char-class predicates (use char codes; alpha test is ASCII-only,
;;; which matches the rest of the printer/reader).
(defun %ch-upperp (c) (and (>= c 65) (<= c 90)))
(defun %ch-lowerp (c) (and (>= c 97) (<= c 122)))
(defun %ch-alphap (c) (or (%ch-upperp c) (%ch-lowerp c)))
(defun %ch-to-upper (c) (if (%ch-lowerp c) (- c 32) c))
(defun %ch-to-lower (c) (if (%ch-upperp c) (+ c 32) c))

;;; Scan a name string and classify its case content.
;;; Returns one of :all-upper, :all-lower, :mixed, :no-alpha.
(defun %sym-name-case-class (name)
  (let ((len (array-length name)) (i 0)
        (has-upper nil) (has-lower nil))
    (loop
      (when (= i len) (return nil))
      (let ((c (%prim-aref name i)))   ; raw code for %ch-* classifiers
        (cond ((%ch-upperp c) (setq has-upper t))
              ((%ch-lowerp c) (setq has-lower t))))
      (setq i (+ i 1)))
    (cond ((and has-upper has-lower) :mixed)
          (has-upper :all-upper)
          (has-lower :all-lower)
          (t :no-alpha))))

;;; Print a symbol name applying the (readtable-case × *print-case*)
;;; matrix from CLHS §22.1.3.3.1.
;;;
;;; The pipeline is two-stage:
;;;   1. Readtable-case decides, per character, whether that char "matches
;;;      the readtable case" — only matching chars are touched by
;;;      *print-case*; non-matching chars print verbatim.
;;;      :preserve  → nothing matches (entire name verbatim).
;;;      :invert    → if the whole name is one case, invert it and print;
;;;                   otherwise print verbatim.  (Per CLHS: invert applies
;;;                   *no* *print-case* transform at all — the inverted
;;;                   characters are emitted as-is.)
;;;      :upcase    → uppercase chars are "in case", lowercase pass through
;;;      :downcase  → lowercase chars are "in case", uppercase pass through
;;;   2. For :upcase / :downcase readtables, *print-case* maps the matching
;;;      chars:
;;;        :upcase     → uppercase
;;;        :downcase   → lowercase
;;;        :capitalize → first matching alpha of each "word" uppercase,
;;;                      remaining matching alphas lowercase; word
;;;                      boundaries fall on non-alphanumerics.
(defun %print-symbol-name-with-case (name stream case)
  (let ((rt *readtable*))
    (let ((rc (if (and rt (readtablep rt))
                  (readtable-case rt)
                  :upcase)))
      (%print-symbol-name-matrix name stream case rc))))

;;; Core matrix driver.  Kept separate so callers that want to pass an
;;; explicit readtable-case (e.g. uninterned-symbol path, or tests) can.
(defun %print-symbol-name-matrix (name stream case rc)
  (cond
    ;; :preserve — every char verbatim
    ((eq rc :preserve)
     (%print-string-raw name stream))
    ;; :invert — if name is monocase, invert it; otherwise verbatim.
    ;; *print-case* has NO effect under :invert.
    ((eq rc :invert)
     (let ((class (%sym-name-case-class name)))
       (cond
         ((eq class :all-upper)
          ;; Invert: upper → lower.  Non-alpha pass through.
          (let ((len (array-length name)) (i 0))
            (loop
              (when (= i len) (return nil))
              (%print-char (%ch-to-lower (%prim-aref name i)) stream)
              (setq i (+ i 1)))))
         ((eq class :all-lower)
          (let ((len (array-length name)) (i 0))
            (loop
              (when (= i len) (return nil))
              (%print-char (%ch-to-upper (%prim-aref name i)) stream)
              (setq i (+ i 1)))))
         (t  ; :mixed or :no-alpha — verbatim
          (%print-string-raw name stream)))))
    ;; :upcase or :downcase readtable.  Build per-char "in-readtable-case"
    ;; predicate and stream out applying *print-case* to matching chars.
    (t
     (%print-name-with-rcase-and-pcase name stream case rc))))

;;; Emit NAME applying *print-case* (CASE) to chars that match RC.
;;; RC is :upcase or :downcase.
;;;
;;; In-case chars:
;;;   :upcase rc → uppercase chars
;;;   :downcase rc → lowercase chars
;;;
;;; Out-of-case chars pass through verbatim.
(defun %print-name-with-rcase-and-pcase (name stream case rc)
  (let ((len (array-length name)) (i 0)
        (at-word-start t))
    (loop
      (when (= i len) (return nil))
      (let* ((ch (%prim-aref name i))   ; raw code for %ch-* / %print-char
             (in-case
               (cond ((eq rc :upcase)   (%ch-upperp ch))
                     ((eq rc :downcase) (%ch-lowerp ch))
                     (t nil))))
        (if (not in-case)
            ;; Out-of-case: print verbatim.  Word boundary rule for
            ;; :capitalize — non-alphanumerics reset at-word-start.  An
            ;; alpha that is out-of-case (the print folder doesn't touch
            ;; it) still counts as a letter and prevents word-start reset.
            (progn
              (%print-char ch stream)
              (when (eq case :capitalize)
                (unless (or (%ch-alphap ch)
                            (and (>= ch 48) (<= ch 57)))  ; digit
                  (setq at-word-start t))))
            ;; In-case char: apply *print-case*.
            (progn
              (cond
                ((eq case :upcase)
                 (%print-char (%ch-to-upper ch) stream))
                ((eq case :downcase)
                 (%print-char (%ch-to-lower ch) stream))
                ((eq case :capitalize)
                 (if at-word-start
                     (progn
                       (%print-char (%ch-to-upper ch) stream)
                       (setq at-word-start nil))
                     (%print-char (%ch-to-lower ch) stream)))
                (t  ; defensive — treat unknown as upcase
                 (%print-char (%ch-to-upper ch) stream)))
              (when (eq case :capitalize)
                (setq at-word-start nil)))))
      (setq i (+ i 1)))))

;;; Back-compat: %apply-print-case is called from the format-directive
;;; code (~( ~)) for non-symbol case conversion.  Keep the simple
;;; character-by-character transform; readtable-case has no role here
;;; because the input isn't a symbol name.
(defun %apply-print-case (ch case readtable-case)
  (cond
    ((eq case :upcase) (%ch-to-upper ch))
    ((eq case :downcase) (%ch-to-lower ch))
    (t ch)))

;;; Escape policy under *print-escape* / *print-readably*.  A symbol
;;; name needs an escape (per CLHS 22.1.3.2) when:
;;;   (a) any alphabetic character is out of the readtable case —
;;;       printing it verbatim would change interpretation on read, or
;;;   (b) the first character is a digit — the name would parse as a
;;;       number on read, or
;;;   (c) the name is empty.
;;; Under :preserve / :invert, alpha chars never need readtable-case
;;; escapes (the readtable does no case folding); the leading-digit
;;; rule still applies.  This predicate stays neutral about which
;;; escape style (`\X` vs `|name|`) the caller chooses; the
;;; print-symbols / prin1.symbol tests accept either.
(defun %name-needs-rcase-escape-p (name rc)
  (let ((len (array-length name)))
    (cond
      ((= len 0) t)
      (t
       (let ((needs nil) (i 0))
         (loop
           (when (or needs (= i len)) (return needs))
           (let ((ch (%prim-aref name i)))   ; raw code
             (when (and (= i 0) (>= ch 48) (<= ch 57))
               (setq needs t))
             (when (and (not needs) (%ch-alphap ch))
               (cond ((eq rc :upcase)
                      (when (%ch-lowerp ch) (setq needs t)))
                     ((eq rc :downcase)
                      (when (%ch-upperp ch) (setq needs t))))))
           (setq i (+ i 1))))))))

;;; Emit a symbol name wrapped in `|…|` when *print-escape* /
;;; *print-readably* + the readtable-case combination would otherwise
;;; lose information on read-back.  Falls through to the matrix
;;; (no-escape) path when escape is off OR when no character needs it.
(defun %print-symbol-name-maybe-escape (name stream case rc escape readably)
  (cond
    ((and (or escape readably) (%name-needs-rcase-escape-p name rc))
     (%print-char 124 stream)  ; |
     (%print-string-raw name stream)
     (%print-char 124 stream)) ; |
    (t
     (%print-symbol-name-matrix name stream case rc))))

;;; Check if a symbol name needs escaping (contains special chars)
(defun %sym-name-needs-escape-p (name)
  (let ((len (array-length name)) (i 0) (needs-escape nil))
    (when (= len 0) (return-from %sym-name-needs-escape-p t))
    (loop
      (when (= i len) (return needs-escape))
      (let ((ch (%prim-aref name i)))   ; raw code
        (when (or (= ch 32) (= ch 40) (= ch 41) (= ch 34) (= ch 39)
                  (= ch 96) (= ch 44) (= ch 59) (= ch 35) (= ch 92)
                  (= ch 124) (= ch 58) (= ch 9) (= ch 10) (= ch 13))
          (setq needs-escape t)))
      (setq i (+ i 1)))
    needs-escape))

;;; Print a symbol to stream respecting all print variables
(defun %print-symbol-to-stream (sym stream)
  (let ((escape *print-escape*)
        (case *print-case*)
        (gensym *print-gensym*)
        (readably *print-readably*))
    ;; Native keyword (subtag #x53): just emit ":NAME" — no package qualifier
    ;; logic.  The CL-symbol path below handles KEYWORD-package CL symbols
    ;; via the qualifier branch (pkg-name = "" when KEYWORD).
    (when (and (not (%cl-sym-p sym)) (not (null sym)) (not (eq sym t))
               (not (consp sym)) (not (integerp sym))
               (not (characterp sym)) (not (stringp sym))
               (= (obj-subtag sym) 83))   ; #x53 keyword
      (%print-char 58 stream)             ; :
      (let* ((name (symbol-name sym))
             (rt   *readtable*)
             (rc   (if (and rt (readtablep rt)) (readtable-case rt) :upcase)))
        (%print-symbol-name-maybe-escape name stream *print-case* rc escape readably))
      (return-from %print-symbol-to-stream nil))
    (let* ((cl-sym-p (%cl-sym-p sym))
           (name (if cl-sym-p (%cl-sym-name sym) (symbol-name sym)))
           (pkg (if cl-sym-p (%cl-sym-package sym) nil))
           ;; For native MVM symbols (no package slot), treat as accessible
           ;; in *package* iff the name is findable there.  This stops
           ;; quoted-literal symbols from printing as uninterned (#:NAME).
           (native-accessible
             (and (not cl-sym-p) (not (null sym)) (not (eq sym t))
                  (stringp name) (> (array-length name) 0)
                  (%pkg-p *package*)
                  (let ((found (%pkg-find-sym name *package*)))
                    (and found t)))))
      ;; Determine if we need package qualifier
      (let ((cur-pkg *package*))
        (let ((need-qualifier
               (if (or escape readably)
                   ;; Need qualifier if symbol not accessible in current pkg
                   (cond
                     (native-accessible nil)
                     ;; Native MVM symbol literal not found by the linear
                     ;; %pkg-find-sym walk: it is still interned (compile
                     ;; literal), so print it bare — no qualifier, no #:.
                     ((and (null pkg) (not cl-sym-p)) nil)
                     ((null pkg)
                      ;; Genuinely uninterned CL symbol (make-symbol/gensym)
                      (if (or gensym readably) t nil))
                     (t
                      ;; Check if symbol is accessible in current package.
                      ;; First fast path: if the symbol's HOME package IS the
                      ;; current package (by object identity OR by name —
                      ;; nicknames let two `find-package` calls return distinct
                      ;; package objects that share the same canonical name,
                      ;; e.g. CL-USER vs COMMON-LISP-USER), it is accessible
                      ;; without walking the package's internal/external
                      ;; lists.  Catches the post-`%intern-symbol-pkg` case
                      ;; where compile-quote sets the symbol's home package
                      ;; via the pkg-tag but does not splice the symbol into
                      ;; the package's internal slot.  Without it, every
                      ;; `'FOO` printed while `*package*` is the symbol's own
                      ;; home package falls through to `%pkg-find-sym` (a
                      ;; linear walk that misses lazily-stamped symbols) and
                      ;; emits a bogus `PKG::FOO` qualifier.
                      (let ((accessible nil))
                        (cond
                          ((and (%pkg-p pkg) (%pkg-p cur-pkg)
                                (or (eq pkg cur-pkg)
                                    (let ((pn (package-name pkg))
                                          (cn (package-name cur-pkg)))
                                      (and (stringp pn) (stringp cn)
                                           (string= pn cn)))))
                           (setq accessible t))
                          ((%pkg-p cur-pkg)
                           (let ((found (%pkg-find-sym name cur-pkg)))
                             (when (and found (eq found sym))
                               (setq accessible t)))))
                        (not accessible))))
                   nil)))
          (cond
            ;; Uninterned symbol: print #:name (but native-accessible
            ;; symbols skip this branch).  Per CLHS §22.1.6.3, the #:
            ;; prefix is gated by *print-gensym* — when false, uninterned
            ;; symbols are printed WITHOUT the #: prefix even though
            ;; the result is not READably round-trippable.  *print-readably*
            ;; T forces #: (round-trip mandatory).  *print-escape* alone is
            ;; not enough; print.symbol.prefix.4 / .2 fail without this.
            ;;
            ;; CRITICAL: only CL-symbols (make-symbol / gensym give pkg=nil)
            ;; are genuinely uninterned.  A NATIVE MVM symbol is always a
            ;; compile-quote / read literal that IS interned in some package
            ;; — it merely lacks a package slot, so `pkg` is nil and the
            ;; %pkg-find-sym linear walk can miss it (lazily-stamped syms).
            ;; Printing such a literal as `#:NAME` was wrong: format-s.7/44/…
            ;; expect `(format nil "~S" 'ABC)` → "ABC", not "#:ABC".  Gate
            ;; the #: branch on cl-sym-p so native literals fall through to
            ;; the no-qualifier branch.
            ((and cl-sym-p (null pkg) (not native-accessible)
                  (or readably (and escape gensym)))
             (%print-char 35 stream) ; #
             (%print-char 58 stream) ; :
             (let* ((rt *readtable*)
                    (rc (if (and rt (readtablep rt)) (readtable-case rt) :upcase)))
               (%print-symbol-name-maybe-escape name stream case rc escape readably)))
            ;; Package-qualified
            (need-qualifier
             (let ((pkg-name (if (%pkg-p pkg) (package-name pkg) "")))
               (%print-string-raw pkg-name stream)
               ;; Check if external: use : else ::
               (let ((ext (if (%pkg-p pkg)
                              (let ((s (%pkg-find-sym name pkg)))
                                (and s (%pkg-sym-external-p pkg s)))
                              nil)))
                 (if ext
                     (%print-char 58 stream)  ; :
                     (progn (%print-char 58 stream)  ; ::
                            (%print-char 58 stream))))
               (let* ((rt *readtable*)
                    (rc (if (and rt (readtablep rt)) (readtable-case rt) :upcase)))
               (%print-symbol-name-maybe-escape name stream case rc escape readably))))
            ;; No qualifier needed
            (t
             (let* ((rt *readtable*)
                    (rc (if (and rt (readtablep rt)) (readtable-case rt) :upcase)))
               (%print-symbol-name-maybe-escape name stream case rc escape readably)))))))))

;;; Check if symbol is external in package
(defun %pkg-sym-external-p (pkg sym)
  (let ((name (%cl-sym-name sym)))
    (if (%pkg-p pkg)
        (let ((ext-list (%pkg-external pkg)))
          (let ((found nil))
            (dolist (s ext-list)
              (when (and (%cl-sym-p s) (string-equal (%cl-sym-name s) name))
                (setq found t)))
            found))
        nil)))

;;; Find symbol in package (non-closure version)
(defvar *%find-sym-name* nil)
(defvar *%find-sym-result* nil)
(defun %find-sym-match (s)
  (when (and (%cl-sym-p s) (string-equal (%cl-sym-name s) *%find-sym-name*))
    (setq *%find-sym-result* s)))
(defun %pkg-find-sym (name pkg)
  (if (%pkg-p pkg)
      (progn
        (setq *%find-sym-name* name)
        (setq *%find-sym-result* nil)
        (%do-symbols-fn #'%find-sym-match pkg)
        *%find-sym-result*)
      nil))

;;; Print a multi-dim array: emit "#NA" then a nested-list literal whose
;;; structure follows DIMS, drawing elements from FLAT-DATA in row-major order.
;;; Returns (values).  Forward-declared so %write-obj can recurse into it.
(defun %print-md-array (dims flat-data stream level escape)
  ;; Emit "#NA" prefix
  (%print-char 35 stream)  ; #
  (let ((rank 0) (d dims))
    (loop (when (null d) (return nil))
      (setq rank (+ rank 1))
      (setq d (cdr d)))
    (%print-decimal-to-stream rank stream))
  (%print-char 65 stream)  ; A
  ;; If no dims (0-dim), print the single element
  (cond
    ((null dims)
     (%write-obj (aref flat-data 0) stream
                 (if (null level) 1 (+ level 1)) escape))
    (t
     (%print-md-array-rec dims flat-data 0 stream level escape))))

;;; Print one slice of an N-D array.  Returns the next index into FLAT-DATA
;;; after consuming the slice.  DIMS is the remaining dimension list.
(defun %print-md-array-rec (dims flat-data start stream level escape)
  (cond
    ((null dims)
     ;; Leaf — print the single element at START, return START+1
     (%write-obj (aref flat-data start) stream
                 (if (null level) 1 (+ level 1)) escape)
     (+ start 1))
    (t
     (%print-char 40 stream)  ; (
     (let ((n (car dims))
           (rest-dims (cdr dims))
           (i 0)
           (cur start))
       (loop
         (when (= i n) (return nil))
         (when (> i 0) (%print-char 32 stream))
         (setq cur (%print-md-array-rec rest-dims flat-data cur stream level escape))
         (setq i (+ i 1)))
       (%print-char 41 stream)  ; )
       cur))))

;;; Like %print-md-array but starts reading FLAT-DATA at START (for
;;; displaced MDAs: START = %mda-offset into the displaced-to target).
(defun %print-md-array-offset (dims flat-data start stream level escape)
  (%print-char 35 stream)  ; #
  (let ((rank 0) (d dims))
    (loop (when (null d) (return nil))
      (setq rank (+ rank 1))
      (setq d (cdr d)))
    (%print-decimal-to-stream rank stream))
  (%print-char 65 stream)  ; A
  (cond
    ((null dims)
     (%write-obj (aref flat-data start) stream
                 (if (null level) 1 (+ level 1)) escape))
    (t
     (%print-md-array-rec dims flat-data start stream level escape))))

;;; Main printer: print OBJ to STREAM respecting all *print-* variables
;;; LEVEL: current nesting level (nil = not tracking)
;;; ESCAPE: current escape setting
(defun %write-obj (obj stream level escape)
  ;; These *print-* vars are declared special so the let below reads them
  ;; via dynamic (symbol-value) lookup rather than lexical/global. Tests
  ;; that do (let ((*print-base* 2)) (prin1 N)) expect the printer to see
  ;; the dynamic binding — without this declare it read the global.
  (declare (special *print-length* *print-level* *print-base* *print-radix*
                    *print-case* *print-escape* *print-readably*
                    *print-gensym* *print-array*))
  (let ((plen *print-length*)
        (plev *print-level*)
        (pbase *print-base*)
        (pradix *print-radix*)
        (pcase *print-case*)
        (pescape *print-escape*)
        (preadably *print-readably*)
        (pgensym *print-gensym*)
        (parray *print-array*))
    ;; *print-readably* overrides *print-escape*
    (when preadably (setq escape t))
    (cond
      ;; NIL — honor *print-case*. NIL is a symbol whose name is "NIL";
      ;; under :downcase / :capitalize the printed form must follow.
      ;; (:capitalize on "NIL" → "Nil", which needs per-word handling.)
      ((null obj)
       (cond
         ((eq pcase :downcase)
          (%print-char 110 stream) (%print-char 105 stream) (%print-char 108 stream))
         ((eq pcase :capitalize)
          (%print-char 78 stream) (%print-char 105 stream) (%print-char 108 stream))
         (t
          (%print-char 78 stream) (%print-char 73 stream) (%print-char 76 stream))))
      ;; T
      ((eq obj t)
       (%print-char (if (eq pcase :downcase) 116 84) stream))
      ;; Character
      ((characterp obj)
       (if escape
           (let ((code (char-code obj)))
             (%print-char 35 stream)   ; #
             (%print-char 92 stream)   ; backslash
             (cond
               ;; Space (32) is a GRAPHIC character (CLHS graphic-char-p of
               ;; #\Space is T).  Per CLHS 22.1.3.2 a graphic standard char
               ;; prints as "#\<char>" — so #\Space ⇒ "#\ " when not printing
               ;; *print-readably*.  Under *print-readably* keep the name so
               ;; the form round-trips unambiguously (trailing-whitespace).
               ((= code 32)
                (if preadably
                    (%print-string-raw "Space" stream)
                    (%print-char 32 stream)))
               ((= code 10)  (%print-string-raw "Newline" stream))
               ((= code 9)   (%print-string-raw "Tab" stream))
               ((= code 13)  (%print-string-raw "Return" stream))
               ((= code 12)  (%print-string-raw "Page" stream))
               ((= code 8)   (%print-string-raw "Backspace" stream))
               ((= code 7)   (%print-string-raw "Altmode" stream))
               ((= code 127) (%print-string-raw "Rubout" stream))
               ((= code 0)   (%print-string-raw "Null" stream))
               (t (%print-char code stream))))
           (%print-char (char-code obj) stream)))
      ;; Integer (fixnum or bignum — both route through %print-integer-in-base)
      ((or (fixnump obj) (bignump obj))
       (when pradix (%print-radix-prefix pbase stream))
       (%print-integer-in-base obj pbase stream)
       ;; Base 10 with *print-radix* uses TRAILING dot, not a prefix.
       (when (and pradix (= pbase 10))
         (%print-char 46 stream)))
      ;; Float
      ((floatp-impl obj)
       ;; Use standard float printing
       (%print-float-to-stream obj stream escape))
      ;; Ratio
      ((ratiop obj)
       (%print-integer-in-base (ratio-numerator obj) pbase stream)
       (%print-char 47 stream)  ; /
       (%print-integer-in-base (ratio-denominator obj) pbase stream))
      ;; Complex — 3-slot array with %complex-marker in slot 0.  Format
      ;; as #C(REAL IMAG) per CLHS.  Detect BEFORE the generic array
      ;; printer (which would emit #(%COMPLEX-MARKER 1 2)).
      ((%complex-p obj)
       (%print-char 35 stream)   ; #
       (%print-char 67 stream)   ; C
       (%print-char 40 stream)   ; (
       (%write-obj (aref obj 1) stream level escape)
       (%print-char 32 stream)   ; space
       (%write-obj (aref obj 2) stream level escape)
       (%print-char 41 stream))  ; )
      ;; Struct instance — slot-0 = '%struct-instance, slot-1 = type-name,
      ;; user slots from slot-2.  Print #S(TYPE-NAME :SLOT1 v1 :SLOT2 v2 …)
      ;; per CLHS 22.1.3.12.  Detect BEFORE the generic array printer.
      ((%struct-instance-p obj)
       (%print-char 35 stream)   ; #
       (%print-char 83 stream)   ; S
       (%print-char 40 stream)   ; (
       (let* ((tname (%struct-type-name obj))
              (desc (%find-struct-type tname))
              (slots (if desc (%struct-type-desc-slots desc) nil)))
         (%write-obj tname stream level escape)
         (let ((i 0) (cur slots))
           (loop
             (when (null cur) (return nil))
             (%print-char 32 stream)   ; space
             ;; slot keyword — intern :SLOTNAME in the KEYWORD package
             (let ((kw (intern (symbol-name (car cur)) (find-package "KEYWORD"))))
               (%write-obj kw stream level escape))
             (%print-char 32 stream)
             (%write-obj (aref obj (+ 2 i)) stream
                         (if (null level) 1 (+ level 1)) escape)
             (setq i (+ i 1))
             (setq cur (cdr cur)))))
       (%print-char 41 stream))  ; )
      ;; String  (also matches fp-wrapped strings — wrapper-aware stringp
      ;; reports T for them.  Use LENGTH (fill-pointer aware) instead of
      ;; ARRAY-LENGTH so the printed form respects the fp truncation.)
      ((stringp obj)
       (if escape
           (progn
             (%print-char 34 stream)  ; "
             (let ((len (length obj)) (i 0))
               (loop
                 (when (= i len) (return nil))
                 ;; ch as a raw CODE (public AREF now returns characters).
                 (let ((ch (%ensure-char-code (aref obj i))))
                   (when (or (= ch 34) (= ch 92))
                     (%print-char 92 stream))  ; escape " and backslash
                   (%print-char ch stream))
                 (setq i (+ i 1))))
             (%print-char 34 stream))  ; "
           ;; princ-style: no quotes
           (%print-string-raw obj stream)))
      ;; Symbol — CL syms (3-slot), T, NIL, and native MVM symbols
      ;; (1-slot subtag #x50, hash only).  symbolp recognises all four.
      ((symbolp obj)
       (%print-symbol-to-stream obj stream))
      ;; Adjustable wrapper: (cons 8765432 inner) — peel and recurse
      ((and (consp obj) (eql (car obj) 8765432) (consp (cdr obj)))
       (%write-obj (cdr obj) stream level escape))
      ;; Multi-dim array wrapper: (cons 9867654 (cons DIMS FLAT-ARR))
      ((and (consp obj) (eql (car obj) 9867654) (consp (cdr obj)))
       (cond
         ((not parray)
          (%print-char 35 stream)
          (%print-char 60 stream)
          (%print-string-raw "Array" stream)
          (%print-char 62 stream))
         (t
          (let ((dims (cadr obj)) (data (cddr obj)))
            (%print-md-array dims data stream level escape)))))
      ;; Native MDA (subtag #x34) — Phase 4 of multi-dim arrays.
      ;; Rank 1 → use the standard `#(...)` vector emitter (the data
      ;; slot is already a real vector or string); rank ≠ 1 → `#nA(...)`.
      ;; The data may be a string (char-element-typed), in which case the
      ;; downstream stringp branch handles it.
      ((%mda-p obj)
       (cond
         ((not parray)
          (%print-char 35 stream)
          (%print-char 60 stream)
          (%print-string-raw "Array" stream)
          (%print-char 62 stream))
         (t
          (let ((dims (%mda-dims obj)) (data (%mda-data obj)))
            (cond
              ;; Rank 1: recurse on the data vector/string (fp aware via
              ;; the user-visible length).  Slice manually to fp when set.
              ((and dims (null (cdr dims)))
               (let ((fp (%mda-fp obj)))
                 (if (or (null fp) (= fp (array-length data)))
                     (%write-obj data stream level escape)
                     ;; Build a temp vector / string of fp items and print it.
                     (let ((tmp (if (stringp data)
                                    (%make-string-array fp)
                                    (make-array fp)))
                           (i 0))
                       (loop (when (>= i fp) (return nil))
                         (aset tmp i (if (stringp data)
                                         (if (characterp (aref data i))
                                             (char-code (aref data i))
                                             (aref data i))
                                         (aref data i)))
                         (setq i (+ i 1)))
                       (%write-obj tmp stream level escape)))))
              ;; Rank 0 or rank ≥ 2: full multi-dim emitter.  Honor
              ;; displacement: data slot holds the displaced-to target and
              ;; %mda-offset is the starting row-major index into it.
              (t (let ((off (if (%mda-displaced obj) (%mda-offset obj) 0)))
                   (%print-md-array-offset dims data off stream level escape))))))))
      ;; Cons (list)
      ((consp obj)
       ;; Check *print-level*.  LEVEL is the current depth — nil from
       ;; the top-level entry means "depth 0", not "ignore plev".  CLHS
       ;; 22.1.3.4 says objects at depth N>=*print-level* print as #.
       ;; (print-level.3 / .4 pass plev=0 and expect the top-level
       ;; object itself to be elided.)
       (cond
         ((and plev (>= (or level 0) plev))
          (%print-char 35 stream))   ; #
         ;; *print-length* = 0: print "(...)" — don't show the car.
         ;; Per CLHS 22.1.3.5 the elision marker stands in for the
         ;; entire elided portion, including the first element.
         ((and plen (= plen 0))
          (%print-char 40 stream)    ; (
          (%print-string-raw "..." stream)
          (%print-char 41 stream))   ; )
         (t
          (let ((next-level (if (null level) 1 (+ level 1))))
            (%print-char 40 stream)  ; (
            (%write-obj (car obj) stream next-level escape)
            (let ((tail (cdr obj)) (count 1))
              (loop
                (cond
                  ((null tail) (return nil))
                  ;; Dotted tail (non-nil atom) is the natural end of
                  ;; the list — print " . TAIL" regardless of
                  ;; *print-length*.  CLHS 22.1.3.5 counts cons-elements
                  ;; only; the final dotted atom is not subject to
                  ;; truncation.  (print-length.4 / .6)
                  ((not (consp tail))
                   (%print-char 32 stream)  ; space
                   (%print-char 46 stream)  ; .
                   (%print-char 32 stream)  ; space
                   (%write-obj tail stream next-level escape)
                   (return nil))
                  ((and plen (>= count plen))
                   (%print-string-raw " ..." stream)
                   (return nil))
                  (t
                   (%print-char 32 stream)  ; space
                   (%write-obj (car tail) stream next-level escape)
                   (setq tail (cdr tail))
                   (setq count (+ count 1))))))
            (%print-char 41 stream)))))  ; )
      ;; Array/string (non-cons)
      ((arrayp obj)
       (cond
         ;; *print-level* elision applies to arrays too: at depth
         ;; >= plev, print as "#" with no element walk.  (print-level.3)
         ((and plev (>= (or level 0) plev))
          (%print-char 35 stream))   ; #
         ((not parray)
          ;; Print as unreadable
          (%print-char 35 stream)
          (%print-char 60 stream)
          (%print-string-raw "Array" stream)
          (%print-char 62 stream))
         (t
          ;; Print #(...)
          (let ((len (array-length obj)))
            (%print-char 35 stream)
            (%print-char 40 stream)   ; #(
             (let ((i 0))
               (loop
                 (when (= i len) (return nil))
                 (when (> i 0) (%print-char 32 stream))
                 (when (and plen (>= i plen))
                   (%print-string-raw "..." stream)
                   (return nil))
                 (%write-obj (aref obj i) stream
                             (if (null level) 1 (+ level 1)) escape)
                 (setq i (+ i 1))))
             (%print-char 41 stream)))))  ; ) — close let, t, cond, arrayp
      ;; Anything else: #<type>
      (t
       (%print-char 35 stream)
       (%print-char 60 stream)
       (%print-char 63 stream)
       (%print-char 62 stream)))))

;;; Float printing helper
(defun %ieee-float-decode-bits (f)
  "Decode IEEE 754 double-precision boxed float (subtag #x60) into
   (sign mant exp) where value = sign * mant * 2^exp, mant is a non-
   negative integer.  Returns (list sign mant exp) — :infinity for inf,
   :nan for NaN, :zero for ±0.0."
  (let* ((hi (aref f 0))
         (lo (aref f 1))
         (hi-u32 (logand hi 4294967295))
         (sign-bit (logand (ash hi-u32 -31) 1))
         (exp-biased (logand (ash hi-u32 -20) 2047))
         (mant-hi (logand hi-u32 1048575))
         (mant (logior (ash mant-hi 32) (logand lo 4294967295)))
         (sign (if (= sign-bit 1) -1 1)))
    (cond
      ((and (= exp-biased 0) (= mant 0)) (list sign 0 0))   ; ±0
      ((= exp-biased 2047)
       (if (= mant 0) (list sign :infinity 0) (list sign :nan 0)))
      ((= exp-biased 0)
       ;; subnormal: value = sign * mant * 2^(-1022-52)
       (list sign mant (- 0 (+ 1022 52))))
      (t
       ;; normal: value = sign * (2^52 + mant) * 2^(exp_biased - 1023 - 52)
       (list sign (+ (ash 1 52) mant) (- exp-biased 1075))))))

(defun %ieee-to-num-den (f)
  "Decode IEEE float F into (cons num den) where value = num/den.
   Special values (inf/nan) collapse to (0 . 1).  Both num and den
   are simple integers (modus bignum if needed)."
  (let* ((decoded (%ieee-float-decode-bits f))
         (sign (car decoded))
         (mant (cadr decoded))
         (e    (caddr decoded)))
    (cond
      ((or (eq mant :infinity) (eq mant :nan)) (cons 0 1))
      ((= mant 0) (cons 0 1))
      ((>= e 0) (cons (* sign mant (ash 1 e)) 1))
      (t        (cons (* sign mant) (ash 1 (- 0 e)))))))

(defun %print-fraction-digits (frac-num divisor stream)
  "Print up to 15 significant fractional digits of FRAC-NUM/DIVISOR to STREAM,
   with trailing zeros trimmed.  If all digits are zero, prints a single 0."
  (if (= frac-num 0)
      (%print-char 48 stream)
      (let ((digits nil)
            (rem frac-num)
            (div divisor)
            (count 0)
            (max-digits 15))
        (loop
          (when (or (= rem 0) (= count max-digits)) (return nil))
          (setq rem (* rem 10))
          (setq digits (cons (truncate rem div) digits))
          (setq rem (mod rem div))
          (setq count (+ count 1)))
        ;; digits is in reverse order; reverse + strip trailing zeros.
        (let ((lst digits))   ; reversed → first elt is least significant
          (loop
            (when (or (null lst) (not (= (car lst) 0))) (return nil))
            (setq lst (cdr lst)))
          (if (null lst)
              (%print-char 48 stream)
              (dolist (digit (reverse lst))
                (%print-char (+ 48 digit) stream)))))))

;;; ------------------------------------------------------------------
;;; Dragon4 free-format shortest round-trip float printer (CLHS 22.1.3.1.3)
;;;
;;; Given a positive IEEE value as MANT * 2^EXP (MANT,EXP integers, MANT
;;; the significand from %ieee-float-decode-bits), produce the SHORTEST
;;; sequence of decimal digits D1 D2 ... Dn together with an integer K
;;; such that  value = 0.D1D2...Dn * 10^K  and reading that decimal back
;;; reproduces the SAME float.  This is the Steele & White / Dragon4
;;; free-format algorithm (FPP2).
;;;
;;; PRECISION HANDLING: Modus stores every float as an IEEE double (subtag
;;; #x60), but its reader yields single-float-precision values (the source
;;; literal 1.2 becomes the double-widened single 1.2f0 = 1.2000000476...).
;;; A double-precision shortest decimal of that value would be the long
;;; "1.2000000476837158" string.  To recover "1.2" we first STRIP trailing
;;; zero bits from the significand: a widened single has >=29 trailing zero
;;; bits, so stripping leaves <=24 significant bits and the half-ulp gaps
;;; used by Dragon4 become the SINGLE-float gaps — exactly the precision at
;;; which the value round-trips.  A genuine 53-bit double (e.g. an epsilon
;;; constant) has no trailing zeros, so stripping is a no-op and full
;;; double precision is preserved.
;;; ------------------------------------------------------------------

(defun %float-bit-length (n)
  "Number of significant bits in non-negative integer N (0 -> 0)."
  (let ((c 0) (tmp n))
    (loop
      (when (= tmp 0) (return nil))
      (setq tmp (ash tmp -1))
      (setq c (+ c 1)))
    c))

(defun %dragon4-digits (mant exp)
  "MANT * 2^EXP with MANT>0 (MANT the 53-bit IEEE-double significand).
   Returns (cons digit-list k) where the value equals 0.<digits> * 10^k
   and the digit list is the shortest decimal that round-trips to this
   float.  The half-ulp gaps are taken at the float's TRUE precision: a
   value whose low 29 significand bits are all zero is exactly a single
   float, so it is reduced to a 24-bit significand and the shortest
   decimal at single precision is emitted (1.2 -> \"1.2\"); a genuine
   53-bit double (nonzero low 29 bits, e.g. an epsilon constant or a
   runtime quotient) keeps full precision."
  (let ((f mant) (e exp) (p 53))
    ;; Detect single precision: low 29 bits (= 52-23) all zero means this
    ;; double is exactly representable as a 24-bit single.  Reduce so the
    ;; ulp/half-gap reflect single precision.  Genuine doubles keep p=53.
    (when (and (>= (%float-bit-length f) 53)
               (= (logand f 536870911) 0))   ; 536870911 = (1<<29)-1
      (setq f (ash f -29))
      (setq e (+ e 29))
      (setq p 24))
    (let* ((f-is-pow2 (= f (ash 1 (- p 1))))
           (r 0) (s 0) (m+ 0) (m- 0))
      ;; FPP2 scaling: set up R/S/M+/M- per Steele&White.  When the
      ;; significand is an exact power of two the gap to the next-lower
      ;; float is half the gap to the next-higher one (asymmetric).
      (if (>= e 0)
          (let ((be (ash 1 e)))
            (if (not f-is-pow2)
                (progn (setq r (* f be 2)) (setq s 2)
                       (setq m+ be) (setq m- be))
                (progn (setq r (* f be 4)) (setq s 4)
                       (setq m+ (* be 2)) (setq m- be))))
          (let ((se (ash 1 (- 0 e))))
            (if (not f-is-pow2)
                (progn (setq r (* f 2)) (setq s (* se 2))
                       (setq m+ 1) (setq m- 1))
                (progn (setq r (* f 4)) (setq s (* se 4))
                       (setq m+ 2) (setq m- 1)))))
      ;; Estimate/scale K so that (R + M+)/S is in [1/10, 1).
      (let ((k 0))
        (loop
          (when (not (> (+ r m+) s)) (return nil))
          (setq s (* s 10))
          (setq k (+ k 1)))
        (loop
          (when (not (< (* (+ r m+) 10) s)) (return nil))
          (setq r (* r 10)) (setq m+ (* m+ 10)) (setq m- (* m- 10))
          (setq k (- k 1)))
        ;; Generate digits.
        (let ((digits nil) (done nil))
          (loop
            (when done (return nil))
            (setq r (* r 10)) (setq m+ (* m+ 10)) (setq m- (* m- 10))
            (let* ((d (truncate r s))
                   (nr (mod r s)))
              (setq r nr)
              (let ((low (< r m-))
                    (high (> (+ r m+) s)))
                (if (or low high)
                    (let ((dd (cond
                                ((and low (not high)) d)
                                ((and high (not low)) (+ d 1))
                                ((not (> (* r 2) s)) d)
                                (t (+ d 1)))))
                      (setq digits (cons dd digits))
                      (setq done t))
                    (setq digits (cons d digits))))))
          (cons (reverse digits) k))))))

(defun %print-float-digits (digits k stream)
  "Render DIGITS (list of 0-9 ints) with decimal point so that value =
   0.<digits> * 10^K, choosing positional vs exponential per the CL free
   format.  Assumes sign already emitted and DIGITS non-empty."
  (let ((ndig (length digits)))
    (cond
      ;; Positional notation when -2 <= k <= 7 (matches the common CL printer
      ;; threshold: 1000000.0 positional, 1.0e7 exponential; 0.001 positional,
      ;; 1.0e-4 exponential).
      ((and (>= k -2) (<= k 7))
       (cond
         ((<= k 0)
          ;; 0.<zeros><digits>
          (%print-char 48 stream)   ; 0
          (%print-char 46 stream)   ; .
          (let ((z (- 0 k)))
            (loop (when (<= z 0) (return nil))
                  (%print-char 48 stream) (setq z (- z 1))))
          (dolist (d digits) (%print-char (+ 48 d) stream)))
         ((>= k ndig)
          ;; <digits><zeros>.0
          (dolist (d digits) (%print-char (+ 48 d) stream))
          (let ((z (- k ndig)))
            (loop (when (<= z 0) (return nil))
                  (%print-char 48 stream) (setq z (- z 1))))
          (%print-char 46 stream)   ; .
          (%print-char 48 stream))  ; 0
         (t
          ;; first k digits, '.', rest
          (let ((i 0))
            (dolist (d digits)
              (when (= i k) (%print-char 46 stream))
              (%print-char (+ 48 d) stream)
              (setq i (+ i 1)))))))
      (t
       ;; Exponential: D.DDDDe<exp>, exp = k-1.
       (let ((first t))
         (dolist (d digits)
           (%print-char (+ 48 d) stream)
           (when first
             (%print-char 46 stream)   ; .
             (setq first nil))))
       ;; If only one digit, we printed "D." — add trailing 0.
       (when (= ndig 1) (%print-char 48 stream))
       (%print-char 101 stream)   ; e
       (let ((ex (- k 1)))
         (when (< ex 0)
           (%print-char 45 stream)   ; -
           (setq ex (- 0 ex)))
         (%print-decimal-to-stream ex stream))))))

(defun float-to-string (f)
  "Convert boxed float to printed decimal representation.
   Handles both IEEE-bit subtag #x60 floats (produced by reader,
   %float-from-int, SSE2 :fadd/etc.) and legacy 2-slot rational-form
   floats (subtag #x32 with [signed-mantissa, divisor] slots).  Result
   is rendered as N.D where the integer part is always present and a
   single trailing 0 is printed when the fractional part is exactly 0."
  (let ((s (make-string-output-stream))
        (subtag (obj-subtag f)))
    (cond
      ((= subtag 96)
       ;; Real IEEE #x60: decode bits, render via Dragon4 free-format.
       (let* ((decoded (%ieee-float-decode-bits f))
              (sign (car decoded))
              (mant (cadr decoded))
              (e    (caddr decoded))
              (neg  (< sign 0)))
         (cond
           ;; ±0.0
           ((or (eq mant :infinity) (eq mant :nan) (= mant 0))
            (when neg (%print-char 45 s))
            (cond
              ((eq mant :infinity)
               ;; No reader syntax; emit a recognizable token.
               (%print-string-raw "#.float-infinity" s))
              ((eq mant :nan)
               (%print-string-raw "#.float-nan" s))
              (t
               (%print-char 48 s) (%print-char 46 s) (%print-char 48 s))))
           (t
            (when neg (%print-char 45 s))
            (let* ((dk (%dragon4-digits mant e))
                   (digits (car dk))
                   (k      (cdr dk)))
              (%print-float-digits digits k s))))))
      (t
       ;; Legacy [signed-mant, divisor] rational-form (subtag #x32).
       (let* ((smant (aref f 0))
              (divisor (aref f 1))
              (neg (< smant 0))
              (mant (if neg (- 0 smant) smant)))
         (when neg (%print-char 45 s))
         (let ((int-part (truncate mant divisor))
               (frac-num (mod mant divisor)))
           (%print-decimal-to-stream int-part s)
           (%print-char 46 s)
           (%print-fraction-digits frac-num divisor s)))))
    (get-output-stream-string s)))

(defun %print-float-to-stream (f stream escape)
  (let ((s (float-to-string f)))
    (if (stringp s)
        (%print-string-raw s stream)
        (%print-string-raw "0.0" stream))))

;;; write-to-stream: write OBJ to STREAM (prin1 style, escape=t)
(defun write-to-stream (obj stream)
  (%write-obj obj stream nil t))

;;; princ-to-stream: write OBJ to STREAM (princ style, escape=nil)
(defun princ-to-stream (obj stream)
  (%write-obj obj stream nil nil))

;;; write-to-string: return printed representation as string
(defun write-to-string (obj &rest args)
  "Return string representation of OBJ. Keyword args override *print-* vars."
  (let ((s (make-string-output-stream)))
    ;; Parse keyword args
    (let ((escape *print-escape*)
          (base *print-base*)
          (radix *print-radix*)
          (pcase *print-case*)
          (level *print-level*)
          (length *print-length*)
          (circle *print-circle*)
          (gensym *print-gensym*)
          (array *print-array*)
          (readably *print-readably*)
          (stream-arg nil))
      ;; CLHS 3.4.1.4: repeated keyword args ⇒ LEFTMOST pair wins.  Track
      ;; per-key seen flags so (write-to-string 4 :base 10 :base 2) ⇒ "4".
      (let ((rest args)
            (s-escape nil) (s-base nil) (s-radix nil) (s-case nil) (s-level nil)
            (s-length nil) (s-circle nil) (s-gensym nil) (s-array nil)
            (s-readably nil) (s-stream nil))
        (loop
          (when (null rest) (return nil))
          (let ((key (car rest)) (val (cadr rest)))
            (cond
              ((eq key :escape)   (unless s-escape   (setq escape val)     (setq s-escape t)))
              ((eq key :base)     (unless s-base     (setq base val)       (setq s-base t)))
              ((eq key :radix)    (unless s-radix    (setq radix val)      (setq s-radix t)))
              ((eq key :case)     (unless s-case     (setq pcase val)      (setq s-case t)))
              ((eq key :level)    (unless s-level    (setq level val)      (setq s-level t)))
              ((eq key :length)   (unless s-length   (setq length val)     (setq s-length t)))
              ((eq key :circle)   (unless s-circle   (setq circle val)     (setq s-circle t)))
              ((eq key :gensym)   (unless s-gensym   (setq gensym val)     (setq s-gensym t)))
              ((eq key :array)    (unless s-array    (setq array val)      (setq s-array t)))
              ((eq key :readably) (unless s-readably (setq readably val)   (setq s-readably t)))
              ((eq key :stream)   (unless s-stream   (setq stream-arg val) (setq s-stream t)))
              ((eq key :pretty)   nil)  ; ignore
              ((eq key :lines)    nil)  ; ignore
              ((eq key :miser-width) nil)
              ((eq key :right-margin) nil)
              ((eq key :pprint-dispatch) nil)))
          (setq rest (cddr rest))))
      ;; Temporarily bind print variables
      (let ((*print-escape* escape)
            (*print-base* base)
            (*print-radix* radix)
            (*print-case* pcase)
            (*print-level* level)
            (*print-length* length)
            (*print-circle* circle)
            (*print-gensym* gensym)
            (*print-array* array)
            (*print-readably* readably))
        (declare (special *print-escape* *print-base* *print-radix* *print-case*
                          *print-level* *print-length* *print-circle* *print-gensym*
                          *print-array* *print-readably*))
        (%write-obj obj s nil (if readably t escape))))
    (get-output-stream-string s)))

(defun prin1-to-string (obj)
  (let ((*print-escape* t))
    (declare (special *print-escape*))
    (write-to-string obj)))

(defun princ-to-string (obj)
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (declare (special *print-escape* *print-readably*))
    (write-to-string obj)))

;;; Write OBJ to STREAM, respecting *print-* vars and keyword args
(defun %write-to-stream-with-keys (obj stream args)
  "Write OBJ to STREAM with keyword arg overrides."
  (let ((escape *print-escape*)
        (base *print-base*)
        (radix *print-radix*)
        (pcase *print-case*)
        (level *print-level*)
        (length *print-length*)
        (circle *print-circle*)
        (gensym *print-gensym*)
        (array *print-array*)
        (readably *print-readably*))
    ;; CLHS 3.4.1.4: for repeated keyword args the LEFTMOST pair is used.
    ;; Track per-key "seen" flags so a later duplicate cannot override the
    ;; first (e.g. (write-to-string 4 :base 10 :base 2) ⇒ base 10, "4").
    (let ((rest args)
          (s-escape nil) (s-base nil) (s-radix nil) (s-case nil) (s-level nil)
          (s-length nil) (s-circle nil) (s-gensym nil) (s-array nil)
          (s-readably nil))
      (loop
        (when (null rest) (return nil))
        (let ((key (car rest)) (val (cadr rest)))
          (cond
            ((eq key :escape)   (unless s-escape   (setq escape val)   (setq s-escape t)))
            ((eq key :base)     (unless s-base     (setq base val)     (setq s-base t)))
            ((eq key :radix)    (unless s-radix    (setq radix val)    (setq s-radix t)))
            ((eq key :case)     (unless s-case     (setq pcase val)    (setq s-case t)))
            ((eq key :level)    (unless s-level    (setq level val)    (setq s-level t)))
            ((eq key :length)   (unless s-length   (setq length val)   (setq s-length t)))
            ((eq key :circle)   (unless s-circle   (setq circle val)   (setq s-circle t)))
            ((eq key :gensym)   (unless s-gensym   (setq gensym val)   (setq s-gensym t)))
            ((eq key :array)    (unless s-array    (setq array val)    (setq s-array t)))
            ((eq key :readably) (unless s-readably (setq readably val) (setq s-readably t)))
            ((eq key :stream)   nil)  ; already handled
            ((eq key :pretty)   nil)
            ((eq key :lines)    nil)
            ((eq key :miser-width) nil)
            ((eq key :right-margin) nil)
            ((eq key :pprint-dispatch) nil)
            ((eq key :allow-other-keys) nil)))
        (setq rest (cddr rest))))
    (let ((*print-escape* escape)
          (*print-base* base)
          (*print-radix* radix)
          (*print-case* pcase)
          (*print-level* level)
          (*print-length* length)
          (*print-circle* circle)
          (*print-gensym* gensym)
          (*print-array* array)
          (*print-readably* readably))
      (declare (special *print-escape* *print-base* *print-radix* *print-case*
                        *print-level* *print-length* *print-circle* *print-gensym*
                        *print-array* *print-readably*))
      (%write-obj obj stream nil (if readably t escape)))))

(defun prin1 (obj &rest stream-arg)
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* t))
    (declare (special *print-escape*))
    (%write-obj obj stream nil t)
    obj))

(defun princ (obj &rest stream-arg)
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* nil)
        (*print-readably* nil))
    (declare (special *print-escape* *print-readably*))
    (%write-obj obj stream nil nil)
    obj))

(defun write (obj &rest args)
  "Write OBJ with keyword args controlling print vars.  Per CLHS
   3.4.1.4 validates kwarg shape — odd-length / unknown keys
   (without :allow-other-keys T) signal program-error."
  ;; Leftmost-wins on :allow-other-keys per CLHS 3.4.1.4.1.
  (let ((allow-other nil) (allow-other-set nil))
    (let ((scan args))
      (loop (when (or (null scan) (null (cdr scan))) (return))
        (when (and (eq (car scan) :allow-other-keys) (not allow-other-set))
          (setq allow-other-set t)
          (when (cadr scan) (setq allow-other t)))
        (setq scan (cddr scan))))
    (let ((vp args))
      (loop
        (when (null vp) (return))
        (when (null (cdr vp)) (%signal-program-error) (return))
        (let ((k (car vp)))
          (unless (or (member k '(:stream :escape :base :radix :case :level :length
                                  :circle :gensym :array :readably :pretty :lines
                                  :miser-width :right-margin :pprint-dispatch
                                  :allow-other-keys))
                      allow-other)
            (%signal-program-error)
            (return)))
        (setq vp (cddr vp)))))
  ;; Parse :stream keyword
  (let ((stream *standard-output*))
    (let ((rest args))
      (loop
        (when (null rest) (return nil))
        (when (eq (car rest) :stream)
          (setq stream (cadr rest))
          (return nil))
        (setq rest (cddr rest))))
    (let ((s (%resolve-output-stream stream)))
      ;; Remove :stream from args for %write-to-stream-with-keys
      (let ((filtered-args nil) (rest args))
        (loop
          (when (null rest) (return nil))
          (if (eq (car rest) :stream)
              (setq rest (cddr rest))
              (progn
                (setq filtered-args (cons (car rest) filtered-args))
                (when (cdr rest)
                  (setq filtered-args (cons (cadr rest) filtered-args)))
                (setq rest (cddr rest)))))
        (%write-to-stream-with-keys obj s (nreverse filtered-args))))
    obj))

(defun print (obj &rest stream-arg)
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* t))
    (declare (special *print-escape*))
    (%write-char-to-stream 10 stream)  ; newline first
    (%write-obj obj stream nil t)
    (%write-char-to-stream 32 stream)  ; trailing space
    obj))

;;; ============================================================
;;; Pretty-printer core: pprint-logical-block / pprint-pop /
;;; pprint-exit-if-list-exhausted machinery.
;;;
;;; The innermost block's state lives at the head of *%pp-ctx*, a
;;; stack of state arrays #(stream list count listp).  The
;;; pprint-logical-block macro (expanded by the build-side rewriter
;;; in build-ansi-test.lisp) calls %pprint-lb-begin to push a fresh
;;; state, runs the body inside a CATCH '%pp-tag, then calls
;;; %pprint-lb-end to pop — so there is no fragile rebinding of
;;; multiple non-CLHS specials.  pprint-pop / pprint-exit read the
;;; head state via the helpers below.
;;;
;;;   state slot 0  resolved output stream
;;;   state slot 1  remaining list being iterated by pprint-pop
;;;   state slot 2  number of elements already popped (or :DONE for atoms)
;;;   state slot 3  T when the block object was a list (vs atom)
;;; ============================================================

(defvar *%pp-ctx* nil)    ; stack of block-state arrays
(defvar *%pp-level* 0)    ; current nesting depth (for *print-level*)

(defun %pp-check-string (x)
  "Signal a type-error unless X is a string (or NIL, meaning default)."
  (when (and x (not (stringp x)))
    (%signal-type-error))
  x)

(defun %pp-list-arg-p (obj)
  "T if OBJ is a proper-or-dotted list (cons) or NIL — i.e. pprint-pop
   should iterate it.  A non-list object is written as-is by the block
   and pprint-pop returns it once."
  (or (null obj) (consp obj)))

(defun %pp-level-deep-p ()
  "T when the current logical-block nesting depth (*%pp-level*) has met
   or exceeded *print-level* — i.e. this block prints as '#'."
  (declare (special *print-level*))
  (let ((lvl *print-level*)
        (d (or *%pp-level* 0)))
    (and lvl (integerp lvl) (>= d lvl))))

(defun %pplb-write-prefix (stream obj prefix per-line-prefix)
  "Simple-form prefix writer.  CLHS: the prefix/suffix are written only
   when OBJ is a list (cons or NIL); for a non-list OBJ they are omitted
   and OBJ is printed directly.  Always validates the string args."
  (%pp-check-string prefix)
  (%pp-check-string per-line-prefix)
  (when (%pp-list-arg-p obj)
    (let ((p (or prefix per-line-prefix)))
      (when (and p (> (length p) 0))
        (write-string p stream))))
  nil)

(defun %pplb-write-suffix (stream obj suffix)
  "Simple-form suffix writer (see %pplb-write-prefix)."
  (%pp-check-string suffix)
  (when (and (%pp-list-arg-p obj) suffix (> (length suffix) 0))
    (write-string suffix stream))
  nil)

(defun %pprint-lb-begin (stream list prefix per-line-prefix)
  "Begin a logical block: validate (per-line-)prefix, push a fresh state
   onto *%pp-ctx*, write the prefix, and return the resolved stream."
  (%pp-check-string prefix)
  (%pp-check-string per-line-prefix)
  (let ((s (%resolve-output-stream stream))
        (st (make-array 4)))
    (aset st 0 s)
    (aset st 1 list)
    (aset st 2 0)
    (aset st 3 (%pp-list-arg-p list))
    (setq *%pp-ctx* (cons st *%pp-ctx*))
    (let ((p (or prefix per-line-prefix)))
      (when (and p (> (length p) 0))
        (write-string p s)))
    s))

(defun %pprint-lb-end (stream suffix)
  "End a logical block: write SUFFIX and pop the block state."
  (%pp-check-string suffix)
  (when (and suffix (> (length suffix) 0))
    (write-string suffix stream))
  (when *%pp-ctx* (setq *%pp-ctx* (cdr *%pp-ctx*)))
  nil)

(defun %pprint-pop-fn ()
  "pprint-pop: return the next element of the current logical block's
   list, honoring *print-length*.  For a non-list block object, return
   it once.  Writes the dotted '. tail' or '...' marker and throws to
   %pp-tag when iteration must stop."
  (declare (special *print-length*))
  (let ((st (car *%pp-ctx*)))
    (if (null st)
        nil
        (let ((stream (aref st 0))
              (plen *print-length*))
          (if (not (aref st 3))
              ;; Block object was an atom: yield it once.
              (let ((cnt (aref st 2)))
                (if (eq cnt :done)
                    nil
                    (progn (aset st 2 :done) (aref st 1))))
              ;; Block object is a (possibly dotted) list.
              (let ((lst (aref st 1))
                    (cnt (aref st 2)))
                (cond
                  ;; *print-length* exhausted: emit "..." and exit.
                  ((and plen (integerp plen) (>= cnt plen))
                   (write-string "..." stream)
                   (throw '%pp-tag nil))
                  ;; Proper end of list: nothing left.
                  ((null lst) nil)
                  ;; Dotted tail: write " . <atom>" and exit.
                  ((not (consp lst))
                   (write-string " . " stream)
                   (%write-obj lst stream nil t)
                   (throw '%pp-tag nil))
                  ;; Normal element.
                  (t
                   (aset st 1 (cdr lst))
                   (aset st 2 (+ cnt 1))
                   (car lst)))))))))

(defun %pprint-exit-fn ()
  "pprint-exit-if-list-exhausted: throw to %pp-tag when the current
   block's list is exhausted, or *print-length* reached (emitting
   '...').  Returns NIL otherwise."
  (declare (special *print-length*))
  (let ((st (car *%pp-ctx*)))
    (when (and st (aref st 3))
      (let ((lst (aref st 1))
            (cnt (aref st 2))
            (plen *print-length*))
        (cond
          ((null lst) (throw '%pp-tag nil))
          ((and plen (integerp plen) (>= cnt plen))
           (write-string "..." (aref st 0))
           (throw '%pp-tag nil))))))
  nil)

(defun pprint (obj &rest stream-arg)
  "Pretty-print OBJ (stub: same as prin1 + newline)."
  (let ((stream (%resolve-output-stream (if stream-arg (car stream-arg) nil)))
        (*print-escape* t)
        (*print-pretty* t))
    (declare (special *print-escape* *print-pretty*))
    (%write-char-to-stream 10 stream)
    (%write-obj obj stream nil t)
    (values)))

;;; print-unreadable-object macro support
;;; (print-unreadable-object (obj stream :type t :identity t) body...)
;;; is rewritten by SBCL to (%print-unreadable-object obj stream type-p identity-p thunk)
(defun %print-unreadable-object (obj stream type-p identity-p thunk)
  "Implement print-unreadable-object."
  (let ((s (%resolve-output-stream stream)))
    (%print-char 35 s)  ; #
    (%print-char 60 s)  ; <
    (when type-p
      ;; Print type name
      (let ((type-str
             (cond
               ((null obj) "NULL")
               ((eq obj t) "BOOLEAN")
               ((fixnump obj) "FIXNUM")
               ((floatp-impl obj) "FLOAT")
               ((ratiop obj) "RATIO")
               ((stringp obj) "STRING")
               ((characterp obj) "CHARACTER")
               ((%cl-sym-p obj) "SYMBOL")
               ((consp obj) "CONS")
               ((arrayp obj) "ARRAY")
               ((packagep obj) "PACKAGE")
               ((streamp obj) "STREAM")
               (t "T"))))
        (%print-string-raw type-str s)
        (when (and thunk identity-p)
          (%print-char 32 s))))
    (when thunk
      (when (and type-p (not identity-p))
        (%print-char 32 s))
      (funcall thunk))
    (when identity-p
      (when (or thunk type-p) (%print-char 32 s))
      ;; Print a fake address (use object hash or 0)
      (%print-string-raw "{" s)
      (%print-decimal-to-stream 0 s)
      (%print-string-raw "}" s))
    (%print-char 62 s)   ; >
    nil))

;;; Alias: my-with-standard-io-syntax (used in printer-aux tests)
(defun my-with-standard-io-syntax (thunk)
  (%with-standard-io-syntax thunk))

;;; ============================================================
;;; format — comprehensive implementation
;;; ============================================================

;;; Parse a format directive argument (integer or nil)
(defun %fmt-parse-int (str pos end)
  "Parse optional integer at POS in STR. Returns (value . new-pos)."
  (if (>= pos end)
      (cons nil pos)
      (let ((ch (%prim-aref str pos)) (neg nil) (n 0) (found nil))   ; raw codes
        (when (= ch 45) ; -
          (setq neg t) (setq pos (+ pos 1)))
        (loop
          (when (>= pos end) (return nil))
          (let ((d (%prim-aref str pos)))
            (when (or (< d 48) (> d 57)) (return nil))
            (setq n (+ (* n 10) (- d 48)))
            (setq found t)
            (setq pos (+ pos 1))))
        (cons (if found (if neg (- 0 n) n) nil) pos))))

;;; Format ~R directive: radix, english cardinal/ordinal
(defun %format-r (n base colonp atp stream)
  (cond
    ;; ~R with base: print in that base
    (base
     (when atp (%print-char 43 stream))  ; + for positive with @
     (%print-integer-in-base n base stream))
    ;; ~R without base, no modifiers: cardinal English
    ((not colonp)
     (if atp
         (%format-ordinal n stream)
         (%format-cardinal n stream)))
    ;; ~:R: ordinal
    (colonp
     (%format-ordinal n stream))))

(defun %cardinal-ones (n stream)
  (cond
    ((= n 0) (%print-string-raw "zero" stream))
    ((= n 1) (%print-string-raw "one" stream))
    ((= n 2) (%print-string-raw "two" stream))
    ((= n 3) (%print-string-raw "three" stream))
    ((= n 4) (%print-string-raw "four" stream))
    ((= n 5) (%print-string-raw "five" stream))
    ((= n 6) (%print-string-raw "six" stream))
    ((= n 7) (%print-string-raw "seven" stream))
    ((= n 8) (%print-string-raw "eight" stream))
    ((= n 9) (%print-string-raw "nine" stream))
    ((= n 10) (%print-string-raw "ten" stream))
    ((= n 11) (%print-string-raw "eleven" stream))
    ((= n 12) (%print-string-raw "twelve" stream))
    ((= n 13) (%print-string-raw "thirteen" stream))
    ((= n 14) (%print-string-raw "fourteen" stream))
    ((= n 15) (%print-string-raw "fifteen" stream))
    ((= n 16) (%print-string-raw "sixteen" stream))
    ((= n 17) (%print-string-raw "seventeen" stream))
    ((= n 18) (%print-string-raw "eighteen" stream))
    (t (%print-string-raw "nineteen" stream))))

(defun %cardinal-tens (n stream)
  (cond
    ((= n 2) (%print-string-raw "twenty" stream))
    ((= n 3) (%print-string-raw "thirty" stream))
    ((= n 4) (%print-string-raw "forty" stream))
    ((= n 5) (%print-string-raw "fifty" stream))
    ((= n 6) (%print-string-raw "sixty" stream))
    ((= n 7) (%print-string-raw "seventy" stream))
    ((= n 8) (%print-string-raw "eighty" stream))
    (t (%print-string-raw "ninety" stream))))

(defun %format-cardinal (n stream)
  "Print N as English cardinal number."
  (cond
    ((< n 0)
     (%print-string-raw "negative " stream)
     (%format-cardinal (- 0 n) stream))
    ((= n 0) (%print-string-raw "zero" stream))
    ((< n 20) (%cardinal-ones n stream))
    ((< n 100)
     (let ((tens (truncate n 10))
           (ones (mod n 10)))
       (%cardinal-tens tens stream)
       (when (> ones 0)
         (%print-char 45 stream)
         (%cardinal-ones ones stream))))
    ((< n 1000)
     (let ((h (truncate n 100)) (r (mod n 100)))
       (%cardinal-ones h stream)
       (%print-string-raw " hundred" stream)
       (when (> r 0)
         (%print-char 32 stream)
         (%format-cardinal r stream))))
    ((< n 1000000)
     (let ((k (truncate n 1000)) (r (mod n 1000)))
       (%format-cardinal k stream)
       (%print-string-raw " thousand" stream)
       (when (> r 0)
         (%print-char 32 stream)
         (%format-cardinal r stream))))
    ((< n 1000000000)
     (let ((m (truncate n 1000000)) (r (mod n 1000000)))
       (%format-cardinal m stream)
       (%print-string-raw " million" stream)
       (when (> r 0)
         (%print-char 32 stream)
         (%format-cardinal r stream))))
    (t (%print-decimal-to-stream n stream))))

(defun %ordinal-suffix (last-word stream)
  (cond
    ((string-equal last-word "one") (%print-string-raw "first" stream))
    ((string-equal last-word "two") (%print-string-raw "second" stream))
    ((string-equal last-word "three") (%print-string-raw "third" stream))
    ((string-equal last-word "four") (%print-string-raw "fourth" stream))
    ((string-equal last-word "five") (%print-string-raw "fifth" stream))
    ((string-equal last-word "six") (%print-string-raw "sixth" stream))
    ((string-equal last-word "seven") (%print-string-raw "seventh" stream))
    ((string-equal last-word "eight") (%print-string-raw "eighth" stream))
    ((string-equal last-word "nine") (%print-string-raw "ninth" stream))
    ((string-equal last-word "ten") (%print-string-raw "tenth" stream))
    ((string-equal last-word "eleven") (%print-string-raw "eleventh" stream))
    ((string-equal last-word "twelve") (%print-string-raw "twelfth" stream))
    ((string-equal last-word "thirteen") (%print-string-raw "thirteenth" stream))
    ((string-equal last-word "fourteen") (%print-string-raw "fourteenth" stream))
    ((string-equal last-word "fifteen") (%print-string-raw "fifteenth" stream))
    ((string-equal last-word "sixteen") (%print-string-raw "sixteenth" stream))
    ((string-equal last-word "seventeen") (%print-string-raw "seventeenth" stream))
    ((string-equal last-word "eighteen") (%print-string-raw "eighteenth" stream))
    ((string-equal last-word "nineteen") (%print-string-raw "nineteenth" stream))
    ((string-equal last-word "twenty") (%print-string-raw "twentieth" stream))
    ((string-equal last-word "thirty") (%print-string-raw "thirtieth" stream))
    ((string-equal last-word "forty") (%print-string-raw "fortieth" stream))
    ((string-equal last-word "fifty") (%print-string-raw "fiftieth" stream))
    ((string-equal last-word "sixty") (%print-string-raw "sixtieth" stream))
    ((string-equal last-word "seventy") (%print-string-raw "seventieth" stream))
    ((string-equal last-word "eighty") (%print-string-raw "eightieth" stream))
    ((string-equal last-word "ninety") (%print-string-raw "ninetieth" stream))
    ((string-equal last-word "hundred") (%print-string-raw "hundredth" stream))
    ((string-equal last-word "thousand") (%print-string-raw "thousandth" stream))
    ((string-equal last-word "million") (%print-string-raw "millionth" stream))
    ((string-equal last-word "zero") (%print-string-raw "zeroth" stream))
    (t (%print-string-raw last-word stream) (%print-string-raw "th" stream))))

(defun %format-ordinal (n stream)
  "Print N as English ordinal.
   We split the cardinal form on the LAST space OR hyphen and feed
   the trailing word into %ordinal-suffix, so e.g. \"twenty-one\" →
   \"twenty-first\" and \"one hundred\" → \"one hundredth\"."
  (cond
    ((< n 0)
     (%print-string-raw "negative " stream)
     (%format-ordinal (- 0 n) stream))
    ((= n 0) (%print-string-raw "zeroth" stream))
    (t
     ;; Build cardinal string, then transform last word to ordinal
     (let ((s (make-string-output-stream)))
       (%format-cardinal n s)
       (let ((cardinal (get-output-stream-string s)))
         (let ((last-sep -1) (i 0) (len (array-length cardinal)))
           (loop
             (when (= i len) (return nil))
             (let ((c (%prim-aref cardinal i)))   ; raw code
               (when (or (= c 32) (= c 45))    ; space or hyphen
                 (setq last-sep i)))
             (setq i (+ i 1)))
           (let ((prefix (if (= last-sep -1) ""
                             (%substring cardinal 0 (+ last-sep 1))))
                 (last-word (if (= last-sep -1) cardinal
                                (%substring cardinal (+ last-sep 1) len))))
             (%print-string-raw prefix stream)
             (%ordinal-suffix last-word stream))))))))

;;; Roman numeral printing for ~@R
(defun %format-roman (n stream &optional oldp)
  "Print N as Roman numerals. OLDP=t means old-style (IIII not IV)."
  (when (<= n 0) (return-from %format-roman nil))
  (let ((vals (list 1000 900 500 400 100 90 50 40 10 9 5 4 1))
        (strs (if oldp
                  (list "M" "DCCCC" "D" "CCCC" "C" "LXXXX" "L" "XXXX"
                        "X" "VIIII" "V" "IIII" "I")
                  (list "M" "CM" "D" "CD" "C" "XC" "L" "XL"
                        "X" "IX" "V" "IV" "I"))))
    (let ((vs vals) (ss strs) (rem n))
      (loop
        (when (or (null vs) (= rem 0)) (return nil))
        (let ((v (car vs)) (s (car ss)))
          (loop
            (when (< rem v) (return nil))
            (%print-string-raw s stream)
            (setq rem (- rem v)))
          (setq vs (cdr vs))
          (setq ss (cdr ss)))))))

;;; CLHS-conformant integer formatter for ~D, ~B, ~O, ~X (and ~R with base).
;;; Params (per CLHS):
;;;   mincol   minimum column width (NIL → no padding)
;;;   padchar  fill character (default #\Space). Char OR fixnum (char-code).
;;;   commachar comma character (default #\,). Char OR fixnum.
;;;   commaint comma interval (default 3)
;;;   colonp   T = insert commachar every commaint digits
;;;   atp      T = always print sign (+ for non-negative)
;;; Non-integer falls back to ~A (princ) — no padding/commas.
(defun %fmt-integer (n base mincol padchar commachar commaint colonp atp stream)
  (cond
    ((not (integerp n))
     ;; ANSI: non-integer → print as ~A (princ), no padding
     (let ((*print-escape* nil))
       (declare (special *print-escape*))
       (%write-obj n stream nil nil)))
    (t
     (let ((s (make-string-output-stream)))
       ;; 1. Sign — works on bignum because numeric-value-less-p +
       ;; generic-subtract are bignum-aware.
       (cond
         ((< n 0) (%print-char 45 s) (setq n (- 0 n)))
         (atp     (%print-char 43 s)))
       ;; 2. Digits — use %bignum-divmod-fixnum so bignum operands work
       ;; (raw :mod / :div IR is fixnum-only).
       (let ((digits nil))
         (cond
           ((and (not (bignump n)) (= n 0)) (setq digits (cons 48 nil)))
           (t
            (let ((tmp n))
              (loop
                (when (and (not (bignump tmp)) (= tmp 0)) (return nil))
                (let* ((qr (%bignum-divmod-fixnum tmp base))
                       (q  (car qr)) (r (cdr qr)))
                  (setq digits (cons (%digit-char-upper r) digits))
                  (setq tmp q))))))
         ;; 3. With colonp, walk digit list emitting commachar at intervals
         (cond
           (colonp
            (let ((cc (%ensure-char-code (if commachar commachar 44)))
                  (ci (if commaint commaint 3))
                  (dl digits)
                  (total 0))
              ;; total = (length digits)
              (let ((lp digits))
                (loop
                  (when (null lp) (return nil))
                  (setq total (+ total 1))
                  (setq lp (cdr lp))))
              (let ((idx 0))
                (loop
                  (when (null dl) (return nil))
                  (when (and (> idx 0)
                             (= 0 (mod (- total idx) ci)))
                    (%print-char cc s))
                  (%print-char (car dl) s)
                  (setq dl (cdr dl))
                  (setq idx (+ idx 1))))))
           (t
            (let ((dl digits))
              (loop
                (when (null dl) (return nil))
                (%print-char (car dl) s)
                (setq dl (cdr dl)))))))
       ;; 4. Apply mincol padding (left-pad to right-align)
       (let ((str (get-output-stream-string s)))
         (cond
           ((and mincol (> mincol 0))
            (let ((slen (array-length str))
                  (pc (%ensure-char-code (if padchar padchar 32))))
              (let ((pad (- mincol slen)))
                (loop
                  (when (<= pad 0) (return nil))
                  (%print-char pc stream)
                  (setq pad (- pad 1))))
              (%print-string-raw str stream)))
           (t
            (%print-string-raw str stream))))))))

;;; Pad string to minimum column
(defun %fmt-pad-aligned (str mincol colinc minpad padchar stream right-align)
  "Write STR padded to MINCOL using PADCHAR, with MINPAD minimum padding.
   RIGHT-ALIGN: T puts padding first (~@A/~@S/~D-style). NIL puts string first.
   PADCHAR can be a character object (e.g. from `v` arg) or a code; ensured
   to a code so `%print-char` gets a fixnum (was a bug: char object reaching
   `write-char-to-stream` mis-emitted, producing wrong padding chars)."
  (let ((slen (if (stringp str) (array-length str) 0))
        (mc (if mincol mincol 0))
        (ci (if colinc colinc 1))
        (mp (if minpad minpad 0))
        (pc (%ensure-char-code (if padchar padchar 32))))
    (let ((padding mp))
      ;; CLHS 22.3.5.1: MINPAD can be negative.  The grow loop must respect
      ;; that the only constraint is (>= slen+padding mincol); negative
      ;; padding is fine, and the print loop must treat padding<=0 as
      ;; "no padding chars" rather than spinning forever waiting for i to
      ;; equal a negative count.
      (loop
        (when (>= (+ slen padding) mc) (return nil))
        (setq padding (+ padding ci)))
      (when (< padding 0) (setq padding 0))
      (cond
        (right-align
         (let ((i 0))
           (loop
             (when (>= i padding) (return nil))
             (%print-char pc stream)
             (setq i (+ i 1))))
         (when (stringp str) (%print-string-raw str stream)))
        (t
         (when (stringp str) (%print-string-raw str stream))
         (let ((i 0))
           (loop
             (when (>= i padding) (return nil))
             (%print-char pc stream)
             (setq i (+ i 1)))))))))

;;; Compatibility wrapper preserving the old &rest signature so older callers
;;; that pass :right-align as a trailing keyword still work.
(defun %fmt-pad (str mincol colinc minpad padchar stream &rest opts)
  (%fmt-pad-aligned str mincol colinc minpad padchar stream
                    (and opts (eq (car opts) :right-align))))

;;; Format ~T: tabulate
(defun %fmt-tabulate (colnum colinc stream)
  (let ((cn (if colnum colnum 1))
        (ci (if colinc colinc 1)))
    ;; We don't track column position, so just emit spaces to next tab stop
    ;; Simplified: emit cn spaces
    (let ((i 0))
      (loop
        (when (= i cn) (return nil))
        (%print-char 32 stream)
        (setq i (+ i 1))))))

;;; ~^ inside ~{ ~} sets *format-iter-escape* to t — the inner %format-impl
;;; returns immediately and the iteration helper checks the flag to break out
;;; of the iteration loop. CLHS 22.3.9.2: ~^ inside an iteration terminates
;;; that iteration's loop, not the entire format.
(defvar *format-iter-escape* nil)

;;; ~:^ checks the OUTER list (the list-of-sublists for ~:{ / ~:@{), not
;;; the current sublist that's being passed as args. Bound by the
;;; ~:{ / ~:@{ helpers to the remaining outer iterations after the
;;; current one. NIL when not inside ~:{ / ~:@{ — ~:^ then has nothing
;;; useful to check; CLHS says behaviour is unspecified there.
(defvar *format-outer-rest* nil)

;;; ~{...~} helpers. Factored out of %format-impl because inlining the
;;; matching-brace scan + the per-iteration recursive call with all its
;;; nested let/loop/cond state confused the MVM register allocator
;;; (the recursive %format-impl call received the outer's arg-list
;;; instead of the lst being passed).

(defun %format-find-close-brace (control start len)
  "Scan CONTROL from START for the matching ~} (respecting nested ~{~}).
   Skips parameters (digits, commas, V, #, '<char>) and modifiers (:, @)
   between ~ and the directive char so e.g. ~1{ and ~v,3:@{ are recognized.
   Returns the position of ~ in the ~} pair, or NIL if not found.
   The caller can detect a colon-modified close (~:}) by scanning forward
   from result+1 looking for : before the } character."
  (let ((pos start) (depth 1) (result nil))
    (loop
      (when (or result (>= pos len)) (return result))
      (if (/= (%prim-aref control pos) 126)
          (setq pos (+ pos 1))
          ;; Found ~ — scan past parameters and modifiers to the directive char.
          (let ((p (+ pos 1)))
            (loop
              (when (>= p len) (return nil))
              (let ((c (%prim-aref control p)))
                (cond
                  ;; ' (apostrophe) consumes the next char as a literal param
                  ((= c 39)
                   (setq p (+ p 1))
                   (when (< p len) (setq p (+ p 1))))
                  ;; digits, minus, comma, v/V, #, :, @ — keep scanning
                  ((or (and (>= c 48) (<= c 57))
                       (= c 45) (= c 44)
                       (= c 118) (= c 86)
                       (= c 35) (= c 58) (= c 64))
                   (setq p (+ p 1)))
                  (t (return nil)))))
            (when (>= p len) (return result))
            (let ((dch (%prim-aref control p)))
              (cond
                ((= dch 123) (setq depth (+ depth 1)) (setq pos (+ p 1)))
                ((= dch 125)
                 (setq depth (- depth 1))
                 (if (= depth 0)
                     (setq result pos)
                     (setq pos (+ p 1))))
                (t (setq pos (+ p 1))))))))))

(defun %format-close-brace-colon-p (control close-pos len)
  "Return T if the ~} close at CLOSE-POS (the ~ position) had a colon
   modifier — i.e. it was actually ~:}. CLHS 22.3.7.4: ~:} forces at
   least one iteration even when the argument list is empty."
  (let ((p (+ close-pos 1)) (saw-colon nil))
    (loop
      (when (>= p len) (return saw-colon))
      (let ((c (%prim-aref control p)))
        (cond
          ((= c 58) (setq saw-colon t) (setq p (+ p 1)))   ; :
          ((= c 125) (return saw-colon))                    ; }
          ;; Skip params/at: digits, comma, V, #, ', @
          ((or (and (>= c 48) (<= c 57))
               (= c 44) (= c 118) (= c 86)
               (= c 35) (= c 64)
               (= c 39))
           (setq p (+ p 1)))
          (t (return saw-colon)))))))

(defun %format-close-brace-end (control close-pos len)
  "Return the position immediately after the closing }. CLOSE-POS points
   to the ~ in ~}. Scans forward through any modifiers (:, @) and params
   to land just past the }."
  (let ((p (+ close-pos 1)))
    (loop
      (when (>= p len) (return len))
      (let ((c (%prim-aref control p)))
        (cond
          ((= c 125) (return (+ p 1)))                       ; }
          ((or (= c 58) (= c 64)                             ; : @
               (and (>= c 48) (<= c 57))
               (= c 44) (= c 118) (= c 86)
               (= c 35) (= c 39))
           (setq p (+ p 1)))
          (t (return (+ p 1))))))))

(defun %format-iter-inside (stream body lst max-iter &optional force-once)
  "Iterate BODY over LST (the ~{...~} case). BODY is the template, LST
   is the list to feed as successive args. Stops when LST exhausted, or
   MAX-ITER reached, ~^ fires, or a pass makes no progress.
   FORCE-ONCE (set by ~:}) runs the body at least once even if LST is
   empty, unless MAX-ITER is 0."
  (let ((count 0))
    (declare (special *format-iter-escape*))
    (when (and force-once (null lst) (or (< max-iter 0) (> max-iter 0)))
      (%format-impl stream body nil))
    (loop
      (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
      (when (null lst) (return nil))
      (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
      (let ((new-lst (%format-impl stream body lst)))
        (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
        (when (eq new-lst lst) (return nil))
        (setq lst new-lst))
      (setq count (+ count 1)))))

(defun %format-iter-remaining (stream body arg-list max-iter &optional force-once)
  "Iterate BODY consuming elements from ARG-LIST (the ~@{...~} case).
   Returns the remaining (unconsumed) arg-list. FORCE-ONCE for ~:@}."
  (let ((count 0))
    (declare (special *format-iter-escape*))
    (when (and force-once (null arg-list) (or (< max-iter 0) (> max-iter 0)))
      (%format-impl stream body nil))
    (loop
      (when *format-iter-escape* (setq *format-iter-escape* nil) (return arg-list))
      (when (null arg-list) (return arg-list))
      (when (and (>= max-iter 0) (>= count max-iter)) (return arg-list))
      (let ((new-args (%format-impl stream body arg-list)))
        (when *format-iter-escape* (setq *format-iter-escape* nil) (return new-args))
        (when (eq new-args arg-list) (return arg-list))
        (setq arg-list new-args))
      (setq count (+ count 1)))))

(defun %format-iter-of-lists (stream body lst max-iter &optional force-once)
  "Iterate BODY over LST where each element of LST is itself a list of args
   passed to BODY. The ~:{...~} case. Stops when LST exhausted, MAX-ITER
   reached, or ~:^ fires. Plain ~^ (escape = T) only ends the current
   sub-iteration; ~:^ (escape = :outer) ends the outer iteration.
   Binds *format-outer-rest* so ~:^ inside the body can check the outer
   iteration state (CLHS 22.3.9.2)."
  (let ((count 0))
    (declare (special *format-iter-escape* *format-outer-rest*))
    (when (and force-once (null lst) (or (< max-iter 0) (> max-iter 0)))
      (let ((*format-outer-rest* nil))
        (declare (special *format-outer-rest*))
        (%format-impl stream body nil)))
    (loop
      (when (eq *format-iter-escape* :outer)
        (setq *format-iter-escape* nil) (return nil))
      (when *format-iter-escape* (setq *format-iter-escape* nil))
      (when (null lst) (return nil))
      (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
      (let ((*format-outer-rest* (cdr lst)))
        (declare (special *format-outer-rest*))
        (%format-impl stream body (car lst)))
      (when (eq *format-iter-escape* :outer)
        (setq *format-iter-escape* nil) (return nil))
      (when *format-iter-escape* (setq *format-iter-escape* nil))
      (setq lst (cdr lst))
      (setq count (+ count 1)))))

(defun %format-iter-of-lists-rest (stream body arg-list max-iter &optional force-once)
  "Iterate BODY consuming successive args from ARG-LIST, each treated as a
   list passed to BODY as its args. The ~:@{...~} case. Returns the
   remaining (unconsumed) arg-list. Binds *format-outer-rest* for ~:^."
  (let ((count 0))
    (declare (special *format-iter-escape* *format-outer-rest*))
    (when (and force-once (null arg-list) (or (< max-iter 0) (> max-iter 0)))
      (let ((*format-outer-rest* nil))
        (declare (special *format-outer-rest*))
        (%format-impl stream body nil)))
    (loop
      (when (eq *format-iter-escape* :outer)
        (setq *format-iter-escape* nil) (return arg-list))
      (when *format-iter-escape* (setq *format-iter-escape* nil))
      (when (null arg-list) (return arg-list))
      (when (and (>= max-iter 0) (>= count max-iter)) (return arg-list))
      (let ((*format-outer-rest* (cdr arg-list)))
        (declare (special *format-outer-rest*))
        (%format-impl stream body (car arg-list)))
      (when (eq *format-iter-escape* :outer)
        (setq *format-iter-escape* nil) (return (cdr arg-list)))
      (when *format-iter-escape* (setq *format-iter-escape* nil))
      (setq arg-list (cdr arg-list))
      (setq count (+ count 1)))))

(defun %format-dispatch-brace (stream control i len arg-list colonp atp param1)
  "Handle a ~{...~} directive at position i of CONTROL.
   Finds the matching ~}, extracts the body substring, runs the iteration
   (one of ~{, ~@{, ~:{, ~:@{), and returns (cons NEW-I NEW-ARG-LIST).
   If no matching ~} is found, returns (cons i arg-list) unchanged.
   Per CLHS 22.3.7.4:
     ~{...~}    : (car arg-list) is the list, body iterates over it as args
     ~@{...~}   : rest of arg-list is consumed as iteration args
     ~:{...~}   : (car arg-list) is list of sublists; body sees each sublist as args
     ~:@{...~}  : rest of arg-list, each one a sublist; body sees its elements as args"
  (let ((end-pos (%format-find-close-brace control i len)))
    (if (null end-pos)
        (cons i arg-list)
        (let* ((raw-body (%substring control i end-pos))
               (body-empty (= (length raw-body) 0))
               (body raw-body)
               (max-iter (if param1 param1 -1))
               (force-once (%format-close-brace-colon-p control end-pos len))
               (new-i (%format-close-brace-end control end-pos len))
               (use-fn nil))
          ;; CLHS 22.3.7.4: empty ~{~} body consumes the next argument and
          ;; uses it as the body. String → reuse as control. Function (e.g.
          ;; FORMATTER closure) → iterate via %format-iter-via-fn.
          (when body-empty
            (let ((next-body (car arg-list)))
              (setq arg-list (cdr arg-list))
              (cond
                ((stringp next-body) (setq body next-body))
                ((functionp next-body) (setq use-fn next-body)))))
          (cond
            (use-fn
             (%format-iter-via-fn stream use-fn arg-list colonp atp max-iter new-i))
            ((and colonp atp)
             (cons new-i (%format-iter-of-lists-rest stream body arg-list max-iter force-once)))
            (colonp
             (let ((lst (car arg-list))
                   (rest-args (cdr arg-list)))
               (%format-iter-of-lists stream body lst max-iter force-once)
               (cons new-i rest-args)))
            (atp
             (cons new-i (%format-iter-remaining stream body arg-list max-iter force-once)))
            (t
             (let ((lst (car arg-list))
                   (rest-args (cdr arg-list)))
               (%format-iter-inside stream body lst max-iter force-once)
               (cons new-i rest-args))))))))

(defun %format-iter-via-fn (stream fn arg-list colonp atp max-iter new-i)
  "Iterate FN (a formatter-style closure) over args, the empty-body case of
   ~{~} where the body argument was a function. FN should be called as
   (funcall FN stream &rest args) per CLHS 22.3.10.2 — it returns the
   remaining args. Returns (cons NEW-I REMAINING-ARG-LIST)."
  (let ((count 0))
    (declare (special *format-iter-escape* *format-outer-rest*))
    (cond
      ((and colonp atp)
       ;; ~:@{...~} (empty-body, fn version): each remaining arg is a
       ;; sublist; pass its elements as MULTIPLE args to fn via apply.
       (loop
         (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
         (when (null arg-list) (return nil))
         (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
         (let ((*format-outer-rest* (cdr arg-list)))
           (declare (special *format-outer-rest*))
           (apply fn stream (car arg-list)))
         (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
         (setq arg-list (cdr arg-list))
         (setq count (+ count 1)))
       (cons new-i arg-list))
      (colonp
       ;; ~:{...~} (empty-body, fn version): each element of (car arg-list)
       ;; is a sublist; pass its elements as MULTIPLE args to fn via apply.
       (let ((lst (car arg-list))
             (rest-args (cdr arg-list)))
         (loop
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (when (null lst) (return nil))
           (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
           (let ((*format-outer-rest* (cdr lst)))
             (declare (special *format-outer-rest*))
             (apply fn stream (car lst)))
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (setq lst (cdr lst))
           (setq count (+ count 1)))
         (cons new-i rest-args)))
      (atp
       (loop
         (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
         (when (null arg-list) (return nil))
         (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
         (let ((rem (apply fn stream arg-list)))
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (if (eq rem arg-list) (return nil) (setq arg-list rem)))
         (setq count (+ count 1)))
       (cons new-i arg-list))
      (t
       (let ((lst (car arg-list))
             (rest-args (cdr arg-list)))
         (loop
           (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
           (when (null lst) (return nil))
           (when (and (>= max-iter 0) (>= count max-iter)) (return nil))
           (let ((rem (apply fn stream lst)))
             (when *format-iter-escape* (setq *format-iter-escape* nil) (return nil))
             (if (eq rem lst) (return nil) (setq lst rem)))
           (setq count (+ count 1)))
         (cons new-i rest-args))))))

;;; Main format implementation
;;; Returns remaining args (for use by formatter)
(defun %format-impl (stream control args)
  "Core format. Returns remaining unused args."
  ;; CLHS 22.3: a format control may be a FUNCTION (the result of FORMATTER)
  ;; instead of a string.  When so, it consumes the stream + args and returns
  ;; the unused tail — exactly the formatter-closure contract.  Detect and
  ;; delegate before touching array-length (which would read garbage off a
  ;; closure object).
  (when (functionp control)
    (return-from %format-impl (apply control stream args)))
  (let ((len (array-length control))
        (i 0)
        (arg-list args)
        ;; Tracks the last arg consumed by the most recent value-printing
        ;; directive — used by ~:P / ~:@P which look BACKWARDS at the
        ;; previous arg (CLHS 22.3.3.4) without consuming a new one.
        (prev-arg nil))
    (loop
      (when (>= i len) (return arg-list))
      (let ((ch (%prim-aref control i)))
        (if (not (= ch 126))  ; not ~
            (progn
              (%print-char ch stream)
              (setq i (+ i 1)))
            ;; Parse directive
            (let ((pos (+ i 1))
                  (param1 nil) (param2 nil) (param3 nil) (param4 nil)
                  (param5 nil)
                  (colonp nil) (atp nil))
              ;; Parse parameters (comma-separated integers or v/V/# placeholders)
              (let ((params nil) (pcount 0))
                (loop
                  (when (>= pos len) (return nil))
                  (let ((c (%prim-aref control pos)))
                    (cond
                      ;; v or V: next argument as parameter
                      ((or (= c 118) (= c 86))
                       (setq params (cons (car arg-list) params))
                       (setq arg-list (cdr arg-list))
                       (setq pos (+ pos 1))
                       (setq pcount (+ pcount 1))
                       ;; check for comma
                       (when (and (< pos len) (= (%prim-aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; # : remaining arg count
                      ((= c 35)
                       (setq params (cons (length arg-list) params))
                       (setq pos (+ pos 1))
                       (setq pcount (+ pcount 1))
                       (when (and (< pos len) (= (%prim-aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; ' : character parameter
                      ((= c 39)  ; '
                       (setq pos (+ pos 1))
                       (when (< pos len)
                         (setq params (cons (%prim-aref control pos) params))
                         (setq pos (+ pos 1))
                         (setq pcount (+ pcount 1)))
                       (when (and (< pos len) (= (%prim-aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; Integer parameter (supports leading + or -)
                      ((or (= c 43) (= c 45) (and (>= c 48) (<= c 57)))
                       (when (= c 43) (setq pos (+ pos 1)))
                       (let ((pr (%fmt-parse-int control pos len)))
                         (setq params (cons (car pr) params))
                         (setq pos (cdr pr))
                         (setq pcount (+ pcount 1)))
                       ;; skip comma
                       (when (and (< pos len) (= (%prim-aref control pos) 44))
                         (setq pos (+ pos 1))))
                      ;; Comma alone: nil parameter
                      ((= c 44)
                       (setq params (cons nil params))
                       (setq pos (+ pos 1))
                       (setq pcount (+ pcount 1)))
                      ;; End of params
                      (t (return nil)))))
                ;; params is reversed, get first 4
                (setq params (nreverse params))
                (setq param1 (if (>= pcount 1) (nth 0 params) nil))
                (setq param2 (if (>= pcount 2) (nth 1 params) nil))
                (setq param3 (if (>= pcount 3) (nth 2 params) nil))
                (setq param4 (if (>= pcount 4) (nth 3 params) nil))
                (setq param5 (if (>= pcount 5) (nth 4 params) nil)))
              ;; Parse modifiers : and @
              (loop
                (when (>= pos len) (return nil))
                (let ((c (%prim-aref control pos)))
                  (cond
                    ((= c 58) (setq colonp t) (setq pos (+ pos 1)))   ; :
                    ((= c 64) (setq atp t) (setq pos (+ pos 1)))       ; @
                    (t (return nil)))))
              ;; Directive character
              (when (>= pos len) (return arg-list))
              (let ((dir (%prim-aref control pos))
                    (before-arg-list arg-list))
                (declare (special *format-iter-escape*))
                (setq i (+ pos 1))
                (cond
                  ;; ~A — aesthetic. The `:` modifier (~:A) prints NIL
                  ;; as "()" instead of "nil"/"NIL"/"Nil".
                  ((or (= dir 65) (= dir 97))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream))
                           (*print-escape* nil))
                       (declare (special *print-escape*))
                       (if (and colonp (null obj))
                           (progn (%print-char 40 s) (%print-char 41 s))
                           (%write-obj obj s nil nil))
                       (let ((str (get-output-stream-string s)))
                         (if (or param1 param2 param3 param4)
                             (if atp
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream t)
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream nil))
                             (%print-string-raw str stream))))))
                  ;; ~S — standard. ~:S also prints NIL as "()".
                  ((or (= dir 83) (= dir 115))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((s (make-string-output-stream))
                           (*print-escape* t))
                       (declare (special *print-escape*))
                       (if (and colonp (null obj))
                           (progn (%print-char 40 s) (%print-char 41 s))
                           (%write-obj obj s nil t))
                       (let ((str (get-output-stream-string s)))
                         (if (or param1 param2 param3 param4)
                             (if atp
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream t)
                                 (%fmt-pad-aligned str param1 param2 param3 (if param4 param4 32) stream nil))
                             (%print-string-raw str stream))))))
                  ;; ~W — write (like ~S but respects all print vars)
                  ((or (= dir 87) (= dir 119))
                   (let ((obj (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%write-obj obj stream nil *print-escape*)))
                  ;; ~D — decimal. CLHS params: mincol,padchar,commachar,commaint
                  ;; Numeric directives right-align (left-pad). Non-integer falls
                  ;; back to ~A (princ) per ANSI. ":" inserts commas, "@" forces sign.
                  ((or (= dir 68) (= dir 100))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 10 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~B — binary
                  ((or (= dir 66) (= dir 98))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 2 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~O — octal
                  ((or (= dir 79) (= dir 111))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 8 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~X — hexadecimal
                  ((or (= dir 88) (= dir 120))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (%fmt-integer n 16 param1 param2 param3 param4
                                   colonp atp stream)))
                  ;; ~R — radix. ~radix,mincol,padchar,commachar,commaintR
                  ;; mirrors ~D except RADIX is the FIRST parameter (so ~D's
                  ;; param1=mincol shifts to param2 here).
                  ((or (= dir 82) (= dir 114))
                   (let ((n (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (cond
                       ;; ~@R: Roman numerals (new-style)
                       ((and atp (not colonp) (not param1))
                        (%format-roman n stream nil))
                       ;; ~:@R or ~@:R: Roman numerals old-style
                       ((and atp colonp (not param1))
                        (%format-roman n stream t))
                       ;; ~:R: ordinal English
                       ((and colonp (not atp) (not param1))
                        (%format-ordinal n stream))
                       ;; ~R with no params: cardinal English
                       ((and (not colonp) (not atp) (not param1))
                        (%format-cardinal n stream))
                       ;; ~NR / ~N,M,'cR: base N w/ mincol,padchar,commachar,commaint
                       ;; Note: for ~R, params shift left by one (radix is param1).
                       (param1
                        (%fmt-integer n param1 param2 param3 param4 param5
                                      colonp atp stream))
                       (t
                        (%format-cardinal n stream)))))
                  ;; ~C — character
                  ((or (= dir 67) (= dir 99))
                   (let ((c (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (let ((code (if (characterp c) (char-code c) c)))
                       (cond
                         ;; ~:C: spell out character name
                         (colonp
                          (cond
                            ((= code 32)  (%print-string-raw "Space" stream))
                            ((= code 10)  (%print-string-raw "Newline" stream))
                            ((= code 9)   (%print-string-raw "Tab" stream))
                            ((= code 13)  (%print-string-raw "Return" stream))
                            ((= code 12)  (%print-string-raw "Page" stream))
                            ((= code 8)   (%print-string-raw "Backspace" stream))
                            ((= code 127) (%print-string-raw "Rubout" stream))
                            ((= code 0)   (%print-string-raw "Null" stream))
                            (t (%print-char code stream))))
                         ;; ~@C: #\Name style
                         (atp
                          (%print-char 35 stream) (%print-char 92 stream)
                          (cond
                            ((= code 32)  (%print-string-raw "Space" stream))
                            ((= code 10)  (%print-string-raw "Newline" stream))
                            ((= code 9)   (%print-string-raw "Tab" stream))
                            ((= code 13)  (%print-string-raw "Return" stream))
                            ((= code 12)  (%print-string-raw "Page" stream))
                            ((= code 8)   (%print-string-raw "Backspace" stream))
                            ((= code 127) (%print-string-raw "Rubout" stream))
                            ((= code 0)   (%print-string-raw "Null" stream))
                            (t (%print-char code stream))))
                         ;; Plain ~C: print char
                         (t (%print-char code stream))))))
                  ;; ~% — newline
                  ((= dir 37)
                   (let ((count (if param1 param1 1)) (j 0))
                     (loop
                       (when (= j count) (return nil))
                       (%print-char 10 stream)
                       (setq j (+ j 1)))))
                  ;; ~& — fresh-line.  CLHS 22.3.1.3:
                  ;;   ~&    writes a newline iff not at the start of a line.
                  ;;   ~N&   does ~& once, then writes (N-1) additional newlines.
                  ;;         If N=0, writes nothing at all.
                  ;; So at start-of-line, ~N& prints (N-1) newlines; otherwise N.
                  ((= dir 38)
                   (let ((count (if param1 param1 1)))
                     (when (> count 0)
                       ;; Fresh-line step first — only newline if not at BOL.
                       (unless (%stream-at-bol-p stream)
                         (%print-char 10 stream))
                       ;; Then (count-1) further newlines.
                       (let ((j 1))
                         (loop
                           (when (>= j count) (return nil))
                           (%print-char 10 stream)
                           (setq j (+ j 1)))))))
                  ;; ~~ — tilde
                  ((= dir 126)
                   (let ((count (if param1 param1 1)) (j 0))
                     (loop
                       (when (= j count) (return nil))
                       (%print-char 126 stream)
                       (setq j (+ j 1)))))
                  ;; ~| — page
                  ((= dir 124)
                   (let ((count (if param1 param1 1)) (j 0))
                     (loop
                       (when (= j count) (return nil))
                       (%print-char 12 stream)
                       (setq j (+ j 1)))))
                  ;; ~T — tabulate
                  ((or (= dir 84) (= dir 116))
                   (%fmt-tabulate param1 param2 stream))
                  ;; ~? — recursive format.  CLHS 22.3.7.1.
                  ;; ~?      : next arg is control string, arg after is arg-list (a list)
                  ;; ~@?     : next arg is control string, use remaining args directly
                  ;;
                  ;; CLHS 22.3.9.2: ~^ inside a ~? sub-control terminates
                  ;; the sub-control only, NOT the surrounding format.  We
                  ;; clear *format-iter-escape* after the recursive call so
                  ;; an escape that fired inside ctrl doesn't unwind here.
                  ((= dir 63)
                   (let ((ctrl (car arg-list)))
                     (setq arg-list (cdr arg-list))
                     (cond
                       (atp
                        (setq arg-list (%format-impl stream ctrl arg-list)))
                       (t
                        (let ((sub-args (car arg-list)))
                          (setq arg-list (cdr arg-list))
                          (%format-impl stream ctrl sub-args))))
                     (setq *format-iter-escape* nil)))
                  ;; ~* — goto
                  ((= dir 42)
                   (cond
                     ;; ~@N* / ~@*: absolute goto — set arg-list to (nthcdr N args)
                     (atp
                      (let ((n (if param1 param1 0)))
                        (let ((cur args) (j 0))
                          (loop
                            (when (or (null cur) (= j n)) (return nil))
                            (setq cur (cdr cur))
                            (setq j (+ j 1)))
                          (setq arg-list cur))))
                     ;; ~:N* / ~:*: go back N args (default 1)
                     (colonp
                      (let ((n (if param1 param1 1)))
                        ;; consumed = (length args) - (length arg-list)
                        ;; new pos = consumed - n  (clamped to 0)
                        (let ((consumed 0) (al args))
                          (loop (when (eq al arg-list) (return nil))
                                (when (null al) (return nil))
                                (setq consumed (+ consumed 1))
                                (setq al (cdr al)))
                          (let ((new-pos (- consumed n)))
                            (when (< new-pos 0) (setq new-pos 0))
                            (let ((cur args) (j 0))
                              (loop
                                (when (or (null cur) (= j new-pos)) (return nil))
                                (setq cur (cdr cur))
                                (setq j (+ j 1)))
                              (setq arg-list cur))))))
                     ;; ~N*: skip N args (default 1)
                     (t
                      (let ((n (if param1 param1 1)) (j 0))
                        (loop
                          (when (or (null arg-list) (= j n)) (return nil))
                          (setq arg-list (cdr arg-list))
                          (setq j (+ j 1)))))))
                  ;; ~? — indirection
                  ((= dir 63)
                   (let ((sub-control (car arg-list))
                         (sub-args (cadr arg-list)))
                     (setq arg-list (cddr arg-list))
                     (if atp
                         ;; ~@?: consume remaining args
                         (setq arg-list (%format-impl stream sub-control arg-list))
                         ;; ~?: use sub-args
                         (%format-impl stream sub-control sub-args))))
                  ;; ~P — plural. ~:P / ~:@P use the previously printed
                  ;; arg without consuming a new one (CLHS 22.3.3.4).
                  ((or (= dir 80) (= dir 112))
                   (let ((n (if colonp prev-arg (car arg-list))))
                     (unless colonp (setq arg-list (cdr arg-list)))
                     (let ((val (if (integerp n) n 2)))
                       (if atp
                           (if (= val 1) (%print-char 121 stream)  ; y
                               (%print-string-raw "ies" stream))
                           (if (/= val 1) (%print-char 115 stream))))))  ; s
                  ;; ~newline — discard literal newline and following whitespace.
                  ;;   ~newline    : discard the newline AND following whitespace (default)
                  ;;   ~@newline   : KEEP the newline, discard following whitespace
                  ;;   ~:newline   : discard the newline only, KEEP the whitespace
                  ((= dir 10)
                   (when atp
                     (%print-char 10 stream))
                   (unless colonp
                     (loop
                       (when (>= i len) (return nil))
                       (let ((wc (%prim-aref control i)))
                         (when (not (or (= wc 32) (= wc 9) (= wc 10) (= wc 13)))
                           (return nil)))
                       (setq i (+ i 1)))))
                  ;; ~( ~) — case conversion
                  ((or (= dir 40) (= dir 41))
                   ;; ~(: start case conversion. ~): end.
                   ;; Find matching ~)
                   (when (= dir 40)
                     (let ((end-pos i) (depth 1))
                       (loop
                         (when (>= end-pos len) (return nil))
                         (when (= (%prim-aref control end-pos) 126)
                           (let ((nc (if (< (+ end-pos 1) len) (%prim-aref control (+ end-pos 1)) 0)))
                             (cond
                               ((or (= nc 40) (= nc 41)) ; nested (  )
                                (setq depth (if (= nc 40) (+ depth 1) (- depth 1)))
                                (when (= depth 0)
                                  ;; Found end: process substring
                                  (let ((sub (%substring control i end-pos))
                                        (s2 (make-string-output-stream)))
                                    ;; CLHS 22.3.8.4 — ~( ... ~) consumes
                                    ;; whatever args its body would consume.
                                    ;; Capture and propagate the remaining
                                    ;; arg-list so directives outside the ~(
                                    ;; see the post-body state.
                                    (setq arg-list (%format-impl s2 sub arg-list))
                                    (let ((result (get-output-stream-string s2)))
                                      (let ((converted
                                             (cond
                                               ((and colonp atp)
                                                (string-upcase result))
                                               (colonp
                                                ;; capitalize each word
                                                (string-capitalize result))
                                               (atp
                                                ;; ~@(...~): uppercase the FIRST alpha
                                                ;; character, lowercase everything else.
                                                (if (> (array-length result) 0)
                                                    (let ((r (%make-string-array (array-length result))))
                                                      (let ((first-done nil) (k 0))
                                                        (loop
                                                          (when (= k (array-length result)) (return nil))
                                                          (let ((c (%prim-aref result k)))   ; raw code
                                                            (let ((upper (and (>= c 65) (<= c 90)))
                                                                  (lower (and (>= c 97) (<= c 122))))
                                                              (cond
                                                                ((and (not first-done) (or upper lower))
                                                                 (aset r k (if lower (- c 32) c))
                                                                 (setq first-done t))
                                                                (upper
                                                                 (aset r k (+ c 32)))
                                                                (t
                                                                 (aset r k c)))))
                                                          (setq k (+ k 1))))
                                                      r)
                                                    result))
                                               (t
                                                (string-downcase result)))))
                                        (%print-string-raw converted stream))))
                                  (setq i (+ end-pos 2))
                                  (return nil)))
                               (t nil))))
                         (setq end-pos (+ end-pos 1))))))
                  ;; ~[ ~] — conditional
                  ((or (= dir 91) (= dir 93))
                   (when (= dir 91)
                     ;; Find matching ~]
                     ;; Parse sections separated by ~;
                     ;; ~[: numeric selection by first arg
                     ;; ~@[: boolean test on first arg (true = process, false = skip + consume)
                     ;; ~:[: boolean test (false=first clause, true=second)
                     ;; The default-section marker is ~:; (a ~; with a colon
                     ;; modifier). We treat that specially below.
                     (let ((sections (list)) (default-idx nil) (start i) (depth 1) (pos2 i))
                       (loop
                         (when (>= pos2 len)
                           (setq sections (cons (%substring control start pos2) sections))
                           (return nil))
                         (if (/= (%prim-aref control pos2) 126)
                             (setq pos2 (+ pos2 1))
                             ;; At ~: scan past parameters/modifiers to directive char.
                             (let ((p (+ pos2 1)) (saw-colon nil))
                               (loop
                                 (when (>= p len) (return nil))
                                 (let ((c (%prim-aref control p)))
                                   (cond
                                     ((= c 39)  ; ' literal-char param
                                      (setq p (+ p 1))
                                      (when (< p len) (setq p (+ p 1))))
                                     ((or (and (>= c 48) (<= c 57))
                                          (= c 45) (= c 44)
                                          (= c 118) (= c 86)
                                          (= c 35) (= c 64))
                                      (setq p (+ p 1)))
                                     ((= c 58)
                                      (setq saw-colon t)
                                      (setq p (+ p 1)))
                                     (t (return nil)))))
                               (if (>= p len)
                                   (setq pos2 p)
                                   (let ((nc (%prim-aref control p)))
                                     (cond
                                       ((= nc 91) (setq depth (+ depth 1)) (setq pos2 (+ p 1)))
                                       ((= nc 93)
                                        (setq depth (- depth 1))
                                        (cond
                                          ((= depth 0)
                                           (setq sections (cons (%substring control start pos2) sections))
                                           (setq i (+ p 1))
                                           (return nil))
                                          (t (setq pos2 (+ p 1)))))
                                       ((and (= nc 59) (= depth 1))  ; ~; or ~:;
                                        ;; Push the section preceding ~; first;
                                        ;; default-idx then points to the NEXT
                                        ;; section (the one after ~:;), which
                                        ;; is the default clause.
                                        (setq sections (cons (%substring control start pos2) sections))
                                        (when saw-colon
                                          ;; sections accumulates in reverse; after
                                          ;; final nreverse, the next pushed section
                                          ;; will occupy position (length sections).
                                          (setq default-idx (length sections)))
                                        (setq pos2 (+ p 1))
                                        (setq start pos2))
                                       (t (setq pos2 (+ p 1)))))))))
                       (setq sections (nreverse sections))
                       (cond
                         ;; ~@[: boolean conditional
                         (atp
                          (let ((val (car arg-list)))
                            (if val
                                (progn
                                  ;; Don't consume arg — process section with arg still there
                                  (setq arg-list (%format-impl stream (car sections) arg-list)))
                                (setq arg-list (cdr arg-list)))))
                         ;; ~:[: boolean second-arg style
                         (colonp
                          (let ((val (car arg-list)))
                            (setq arg-list (cdr arg-list))
                            (if (not val)
                                (when sections (%format-impl stream (car sections) arg-list))
                                (when (cdr sections) (%format-impl stream (cadr sections) arg-list)))))
                         ;; ~[: numeric selection.  If a prefix parameter
                         ;; was given (~N[), use N directly without consuming
                         ;; an arg.  Otherwise consume the next arg.  If
                         ;; idx out of range and a ~:; default exists, use it.
                         (t
                          (let ((idx (if (integerp param1)
                                         param1
                                         (let ((v (car arg-list)))
                                           (setq arg-list (cdr arg-list))
                                           v))))
                            (cond
                              ((and (integerp idx) (>= idx 0) (< idx (length sections)))
                               (let ((selected (nth idx sections)))
                                 (when selected
                                   (setq arg-list (%format-impl stream selected arg-list)))))
                              ;; Out of range with default section
                              (default-idx
                               (let ((selected (nth default-idx sections)))
                                 (when selected
                                   (setq arg-list (%format-impl stream selected arg-list))))))))))))
                  ;; ~{ ~} — iteration (with optional :, @, or :@ flags)
                  ((= dir 123)
                   (let ((new-i-and-args
                          (%format-dispatch-brace stream control i len
                                                  arg-list colonp atp param1)))
                     (setq i (car new-i-and-args))
                     (setq arg-list (cdr new-i-and-args))))
                  ((= dir 125) nil)
                  ;; ~^ — escape upward (CLHS 22.3.9.2)
                  ;; ~^        : exit if no remaining args
                  ;; ~N^       : exit if N is zero
                  ;; ~N,M^     : exit if N = M
                  ;; ~N,M,K^   : exit if N <= M <= K
                  ;; Sets *format-iter-escape* so the surrounding ~{ ~}
                  ;; iteration loop can terminate. If we're not inside an
                  ;; iteration the flag still gets cleared next time.
                  ((= dir 94)
                   (declare (special *format-iter-escape* *format-outer-rest*))
                   (let* ((p1 (cond ((characterp param1) (char-code param1))
                                    (t param1)))
                          (p2 (cond ((characterp param2) (char-code param2))
                                    (t param2)))
                          (p3 (cond ((characterp param3) (char-code param3))
                                    (t param3)))
                          (should-escape
                           (cond
                             ;; ~N,M,K^ — escape iff N <= M <= K.  Use NUMBERP
                             ;; (not INTEGERP) so BIGNUM params (e.g. from
                             ;; ~0,v,v^ with (1+ most-positive-fixnum)) are
                             ;; accepted — the compiled INTEGERP primitive is
                             ;; fixnum-only, and <= is bignum-aware.
                             (p3
                              (and (numberp p1) (numberp p2) (numberp p3)
                                   (<= p1 p2) (<= p2 p3)))
                             (p2
                              (and (numberp p1) (numberp p2)
                                   (= p1 p2)))
                             (p1
                              (and (numberp p1) (= p1 0)))
                             ;; ~:^ — escape if outer iteration list exhausted.
                             ;; CLHS 22.3.9.2: ~:^ checks the list passed to ~:{,
                             ;; not the inner sublist passed to the body.
                             (colonp (null *format-outer-rest*))
                             (t (null arg-list)))))
                     (when should-escape
                       ;; Distinguish ~^ (end current sub-iteration) from
                       ;; ~:^ (end outer iteration entirely) so ~:{~A~^~A~A~}
                       ;; correctly resumes after each sublist's ~^.
                       (setq *format-iter-escape* (if colonp :outer t))
                       (return arg-list))))
                  ;; ~_ — conditional newline (pprint, ignore)
                  ((= dir 95) nil)
                  ;; ~I — indent (pprint, ignore)
                  ((= dir 73) (setq arg-list (cdr arg-list)))
                  ;; ~/ — call function
                  ((= dir 47)
                   ;; Find end of function name (next /)
                   (let ((fn-start i) (fn-end i))
                     (loop
                       (when (>= fn-end len) (return nil))
                       (when (= (%prim-aref control fn-end) 47) (return nil))
                       (setq fn-end (+ fn-end 1)))
                     (setq i (+ fn-end 1))
                     ;; Skip arg
                     (setq arg-list (cdr arg-list))))
                  ;; Unknown directive
                  (t
                   (%print-char 126 stream)
                   (%print-char dir stream)))
                ;; Update prev-arg if this directive consumed any arg.
                (when (and (consp before-arg-list)
                           (not (eq before-arg-list arg-list)))
                  (setq prev-arg (car before-arg-list)))
                ;; If a sub-directive (~( ... ~^ ... ~), ~[ ... ~^ ... ~],
                ;; ~? body with ~^ at top, etc.) set *format-iter-escape*
                ;; via an up-and-out, stop processing the rest of THIS
                ;; control string too — CLHS 22.3.9.2's "up and out" is
                ;; not blocked by ~(, ~), ~[, ~], so the surrounding ~{
                ;; iteration body must exit and let the iteration loop see
                ;; the flag.  ~? and ~/ are NOT named in CLHS's "blocks"
                ;; list either, but the standard test suite treats ~? as
                ;; a fresh scope (format.^.?.* re-enter outer after ~?).
                ;; %format-impl's ~? handler clears the flag itself before
                ;; returning so this generic check is safe.
                (when *format-iter-escape* (return arg-list)))))))
    arg-list))

;;; format: the main user-facing function
(defun format (stream control &rest args)
  "Format output. STREAM: nil=return string, t=*standard-output*.
   Returns nil for stream output, string for nil stream."
  (declare (special *format-iter-escape*))
  (setq *format-iter-escape* nil)
  (if (null stream)
      ;; Return string
      (let ((s (make-string-output-stream)))
        (%format-impl s control args)
        (setq *format-iter-escape* nil)
        (get-output-stream-string s))
      ;; Output to stream
      (let ((s (if (eq stream t) (%resolve-output-stream nil) (%resolve-output-stream stream))))
        (%format-impl s control args)
        (setq *format-iter-escape* nil)
        nil)))

;;; formatter: returns a closure that takes (stream &rest args) and
;;; applies the format control string.
;;;
;;; The closure mirrors what (formatter STR) is supposed to do per ANSI
;;; CLHS — return a function consuming a stream + arguments and
;;; returning the unused tail.  Test-site calls look like
;;;   (let ((fn (formatter STR))) ... (apply fn stream args) ...)
;;;
;;; Bumped the apply dispatch (above; see prelude.lisp) from 4 args to
;;; 8 so formatter tests with several args succeed.
(defun formatter (control)
  "Return a function (stream &rest args) that formats using CONTROL."
  (declare (special *format-iter-escape*))
  (lambda (stream &rest args)
    (declare (special *format-iter-escape*))
    (setq *format-iter-escape* nil)
    (let ((remaining (%format-impl (%resolve-output-stream stream) control args)))
      (setq *format-iter-escape* nil)
      remaining)))

;;; Extended apply that handles up to 8 spread args (prelude APPLY tops
;;; out at 4, silently dropping trailing args for any formatter call
;;; with 4+ format arguments — which is most of the format-d tests).
(defun apply (fn &rest spread)
  "ANSI apply: build a single arg list and funcall (up to 8 args).

   Detects interp-closures (consp + car=%interp-closure) and routes
   them through %call-interp-closure — compiled funcall doesn't know
   how to call cons-tagged closures, but methods installed by runtime
   (eval `(defmethod ...))) live as interp-closures and need to be
   invoked via apply from %gf-dispatch."
  (let ((all-args
         (if (null spread)
             nil
             (if (null (cdr spread))
                 (car spread)
                 (let ((individual nil) (cur spread))
                   (loop
                     (when (null (cdr cur))
                       (return (append (nreverse individual) (car cur))))
                     (setq individual (cons (car cur) individual))
                     (setq cur (cdr cur))))))))
    ;; Interp-closure dispatch — fast path for runtime-eval'd lambdas.
    (when (and (consp fn) (eq (car fn) '%interp-closure))
      (return-from apply (%call-interp-closure fn all-args)))
    (let* ((a0 (and all-args (car all-args)))
           (r1 (and all-args (cdr all-args)))
           (a1 (and r1 (car r1)))
           (r2 (and r1 (cdr r1)))
           (a2 (and r2 (car r2)))
           (r3 (and r2 (cdr r2)))
           (a3 (and r3 (car r3)))
           (r4 (and r3 (cdr r3)))
           (a4 (and r4 (car r4)))
           (r5 (and r4 (cdr r4)))
           (a5 (and r5 (car r5)))
           (r6 (and r5 (cdr r5)))
           (a6 (and r6 (car r6)))
           (r7 (and r6 (cdr r6)))
           (a7 (and r7 (car r7)))
           (r8 (and r7 (cdr r7)))
           (a8 (and r8 (car r8)))
           (r9 (and r8 (cdr r8)))
           (a9 (and r9 (car r9)))
           (r10 (and r9 (cdr r9)))
           (a10 (and r10 (car r10)))
           (r11 (and r10 (cdr r10)))
           (a11 (and r11 (car r11)))
           (r12 (and r11 (cdr r11)))
           ;; Ladder extended 12 → 20 (2026-06-01) — funcall n=13..16
           ;; verified working; apply truncating at 12 was blocking
           ;; number-comparison `(apply #'= (N args))` tests and
           ;; plus.8 `(apply #'+ args)` arithmetic.  Beyond 20 still
           ;; truncates; a real %spread-call primitive remains the
           ;; right long-term fix.
           (a12 (and r12 (car r12)))
           (r13 (and r12 (cdr r12)))
           (a13 (and r13 (car r13)))
           (r14 (and r13 (cdr r13)))
           (a14 (and r14 (car r14)))
           (r15 (and r14 (cdr r14)))
           (a15 (and r15 (car r15)))
           (r16 (and r15 (cdr r15)))
           (a16 (and r16 (car r16)))
           (r17 (and r16 (cdr r16)))
           (a17 (and r17 (car r17)))
           (r18 (and r17 (cdr r17)))
           (a18 (and r18 (car r18)))
           (r19 (and r18 (cdr r18)))
           (a19 (and r19 (car r19)))
           (r20 (and r19 (cdr r19)))
           ;; Ladder extended 20 → 32 to match the &rest-rebuild cap
           ;; raised in translate-x64.lisp + compiler.lisp.  Covers
           ;; plus.8 / =.N / etc. up to 32-element arglists.  Beyond 32
           ;; still truncates (compile-funcall's &rest list-build caps
           ;; at 32 too).
           (a20 (and r20 (car r20))) (r21 (and r20 (cdr r20)))
           (a21 (and r21 (car r21))) (r22 (and r21 (cdr r21)))
           (a22 (and r22 (car r22))) (r23 (and r22 (cdr r22)))
           (a23 (and r23 (car r23))) (r24 (and r23 (cdr r23)))
           (a24 (and r24 (car r24))) (r25 (and r24 (cdr r24)))
           (a25 (and r25 (car r25))) (r26 (and r25 (cdr r25)))
           (a26 (and r26 (car r26))) (r27 (and r26 (cdr r26)))
           (a27 (and r27 (car r27))) (r28 (and r27 (cdr r27)))
           (a28 (and r28 (car r28))) (r29 (and r28 (cdr r28)))
           (a29 (and r29 (car r29))) (r30 (and r29 (cdr r29)))
           (a30 (and r30 (car r30))) (r31 (and r30 (cdr r30)))
           (a31 (and r31 (car r31))) (r32 (and r31 (cdr r31))))
      (cond
        ((null all-args) (funcall fn))
        ((null r1) (funcall fn a0))
        ((null r2) (funcall fn a0 a1))
        ((null r3) (funcall fn a0 a1 a2))
        ((null r4) (funcall fn a0 a1 a2 a3))
        ((null r5) (funcall fn a0 a1 a2 a3 a4))
        ((null r6) (funcall fn a0 a1 a2 a3 a4 a5))
        ((null r7) (funcall fn a0 a1 a2 a3 a4 a5 a6))
        ((null r8) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7))
        ((null r9) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8))
        ((null r10) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
        ((null r11) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
        ((null r12) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
        ((null r13) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
        ((null r14) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
        ((null r15) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
        ((null r16) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
        ((null r17) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
        ((null r18) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17))
        ((null r19) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18))
        ((null r20) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19))
        ((null r21) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20))
        ((null r22) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21))
        ((null r23) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22))
        ((null r24) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23))
        ((null r25) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24))
        ((null r26) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25))
        ((null r27) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26))
        ((null r28) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27))
        ((null r29) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28))
        ((null r30) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29))
        ((null r31) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30))
        ((null r32) (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31))
        (t (funcall fn a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26 a27 a28 a29 a30 a31))))))

(defun terpri (&rest stream-arg)
  ;; CLHS: terpri takes at most one arg (the stream).  Extra positional
  ;; args signal program-error.  (terpri.error.1)
  (when (cdr stream-arg) (%signal-program-error))
  (let ((s (%resolve-output-stream (if stream-arg (car stream-arg) nil))))
    (if (and (streamp s) (not (= (%stream-type s) 8)))
        (%write-char-to-stream 10 s)
        (write-char-serial 10)))
  nil)

(defun fresh-line (&rest stream-arg)
  "Write newline only if not at beginning of line. Returns nil if at BOL, non-nil otherwise."
  ;; CLHS: fresh-line takes at most one arg (the stream).  Extra positional
  ;; args signal program-error.  (fresh-line.error.1)
  (when (cdr stream-arg) (%signal-program-error))
  (let ((s (%resolve-output-stream (if stream-arg (car stream-arg) nil))))
    (if (and (streamp s) (not (= (%stream-type s) 8)))
        (if (%stream-at-bol-p s)
            nil
            (progn (%write-char-to-stream 10 s) t))
        ;; Serial output: always write newline (no column tracking)
        (progn (write-char-serial 10) t))))

(defun %stream-at-bol-p (s)
  "Check if stream is at beginning of line (last char was newline or nothing written)."
  (if (streamp s)
      (let ((ty (%stream-type s)))
        (cond
          ((= ty 2) ;; string-output: check char-list
           (let ((chars (car (%stream-data s))))
             (if (null chars)
                 t  ;; nothing written = at BOL
                 (= (car chars) 10))))  ;; last char was newline
          ((= ty 4) ;; two-way: check output side
           (%stream-at-bol-p (cdr (%stream-data s))))
          ((= ty 3) ;; echo: check output side
           (%stream-at-bol-p (cdr (%stream-data s))))
          ((= ty 5) ;; broadcast: check first stream
           (if (%stream-data s)
               (%stream-at-bol-p (car (%stream-data s)))
               t))
          ((= ty 8) nil) ;; serial: assume not at BOL
          (t nil)))
      nil))

;; ANSI: (write-string str &optional stream &key start end)
;; Honor :start/:end for substring writes.
(defun write-string (str &rest args)
  ;; CLHS 3.4.1.4 kwarg validation for the post-stream tail.  Recognised
  ;; keys: :start :end :allow-other-keys.  (write-string.error.3/4)
  (when args
    (let ((kwargs (cdr args)))
      (let ((allow-other nil) (allow-other-set nil))
        (let ((scan kwargs))
          (loop (when (or (null scan) (null (cdr scan))) (return))
            (when (and (eq (car scan) :allow-other-keys) (not allow-other-set))
              (setq allow-other-set t)
              (when (cadr scan) (setq allow-other t)))
            (setq scan (cddr scan))))
        (let ((vp kwargs))
          (loop
            (when (null vp) (return))
            (when (null (cdr vp)) (%signal-program-error) (return))
            (let ((k (car vp)))
              (unless (or (eq k :start) (eq k :end)
                          (eq k :allow-other-keys) allow-other)
                (%signal-program-error)
                (return)))
            (setq vp (cddr vp)))))))
  (let ((stream-arg (if args (car args) nil))
        (start 0)
        (end nil))
    ;; Parse keyword args from args[1..] (post-stream).
    (let ((cur (if args (cdr args) nil)))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :start) (setq start v))
            ((eq k :end)   (setq end v))))
        (setq cur (cddr cur))))
    (let* ((s (%resolve-output-stream stream-arg))
           (len (length str))
           (eff-end (if end end len)))
      (if (and (streamp s) (not (= (%stream-type s) 8)))
          (let ((i start))
            (loop
              (when (>= i eff-end) (return nil))
              (%write-char-to-stream (%ensure-char-code (aref str i)) s)
              (setq i (+ i 1))))
          ;; Serial fallback only handles whole-string writes; emulate.
          (let ((i start))
            (loop
              (when (>= i eff-end) (return nil))
              (write-char-serial (%ensure-char-code (aref str i)))
              (setq i (+ i 1)))))))
  str)

(defun write-line (str &rest args)
  ;; CLHS 3.4.1.4 kwarg validation for the post-stream tail.  Recognised
  ;; keys: :start :end :allow-other-keys.  (write-line.error.3/4)
  (when args
    (let ((kwargs (cdr args)))
      (let ((allow-other nil) (allow-other-set nil))
        (let ((scan kwargs))
          (loop (when (or (null scan) (null (cdr scan))) (return))
            (when (and (eq (car scan) :allow-other-keys) (not allow-other-set))
              (setq allow-other-set t)
              (when (cadr scan) (setq allow-other t)))
            (setq scan (cddr scan))))
        (let ((vp kwargs))
          (loop
            (when (null vp) (return))
            (when (null (cdr vp)) (%signal-program-error) (return))
            (let ((k (car vp)))
              (unless (or (eq k :start) (eq k :end)
                          (eq k :allow-other-keys) allow-other)
                (%signal-program-error)
                (return)))
            (setq vp (cddr vp)))))))
  (let ((stream-arg (if args (car args) nil))
        (start 0)
        (end nil))
    (let ((cur (if args (cdr args) nil)))
      (loop
        (when (null cur) (return nil))
        (let ((k (car cur)) (v (cadr cur)))
          (cond
            ((eq k :start) (setq start v))
            ((eq k :end)   (setq end v))))
        (setq cur (cddr cur))))
    (let* ((s (%resolve-output-stream stream-arg))
           (len (length str))
           (eff-end (if end end len)))
      (if (and (streamp s) (not (= (%stream-type s) 8)))
          (progn
            (let ((i start))
              (loop
                (when (>= i eff-end) (return nil))
                (%write-char-to-stream (%ensure-char-code (aref str i)) s)
                (setq i (+ i 1))))
            (%write-char-to-stream 10 s))
          (progn
            (let ((i start))
              (loop
                (when (>= i eff-end) (return nil))
                (write-char-serial (%ensure-char-code (aref str i)))
                (setq i (+ i 1))))
            (write-char-serial 10)))))
  str)

(defun %ensure-char-code (x)
  "If x is a character, return char-code. Otherwise return x unchanged.
   Avoids compiler bug with inline characterp + char-code."
  (if (fixnump x) x (char-code x)))

(defun write-char (ch &rest stream-arg)
  "Write character CH to stream. Stream designator: nil=*standard-output*, t=*terminal-io*."
  ;; CLHS: write-char takes char + optional stream — at most one trailing
  ;; arg.  A 3rd positional arg signals program-error.  (write-char.error.2)
  (when (cdr stream-arg) (%signal-program-error))
  (let ((saved-ch ch))
    (let ((code (%ensure-char-code saved-ch)))
      (let ((s (if stream-arg
                   (%resolve-output-stream (car stream-arg))
                   (%resolve-output-stream nil))))
        (%write-char-to-stream code s)))
    saved-ch))

(defun finish-output (&rest args)
  "CLHS: takes at most one argument (the stream).  Extra signal
   program-error.  Modus's stub is a no-op otherwise."
  (when (cdr args) (%signal-program-error))
  nil)
(defun force-output (&rest args)
  "CLHS: takes at most one argument (the stream).  Extra signal
   program-error.  Modus's stub is a no-op otherwise."
  (when (cdr args) (%signal-program-error))
  nil)
(defun clear-output (&rest args)
  "CLHS: takes at most one argument (the stream).  Extra signal
   program-error.  Modus's stub is a no-op otherwise."
  (when (cdr args) (%signal-program-error))
  nil)
(defun clear-input (&rest args)
  "CLHS: takes at most one argument (the stream).  Extra positional
   args signal program-error.  Modus's stub is a no-op otherwise."
  (when (cdr args) (%signal-program-error))
  nil)
(defun listen (&rest args)
  "Check if input is available on stream."
  ;; CLHS: listen takes at most one arg (the stream).  Extra positional
  ;; args signal program-error.  (listen.error.1/2)
  (when (cdr args) (%signal-program-error))
  (let ((s (%resolve-input-stream (if args (car args) nil))))
    (if (streamp s)
        (let ((ty (%stream-type s)))
          (cond
            ;; String-input: check if there's data or unread char
            ((= ty 1)
             (let ((data (%stream-data s)))
               (let ((str (car data))
                     (pos-cell (cdr data)))
                 (if (cdr pos-cell)
                     t  ;; unread char available
                     (if (< (car pos-cell) (length str)) t nil)))))
            ;; Two-way: check input side
            ((= ty 4) (listen (car (%stream-data s))))
            ;; Echo: check input side
            ((= ty 3) (listen (car (%stream-data s))))
            ;; Synonym: resolve the target symbol's value and delegate.
            ;; (mirrors %read-char-from-stream's ty 7 resolution).  Without
            ;; this case make-synonym-stream.3 + the listen variants of
            ;; make-concatenated-stream.28/.30 fell to the (t nil) default.
            ((= ty 7)
             (let ((target (%stream-data s)))
               (let ((target-stream (cond ((symbolp target) (symbol-value target))
                                          ((stringp target) (symbol-value (intern target)))
                                          (t target))))
                 (listen target-stream))))
            ;; Concatenated: T if a pushed-back char exists, else if any
            ;; remaining constituent stream has input available.
            ((= ty 6)
             (let ((data (%stream-data s)))
               (if (cdr data)
                   t
                   (let ((streams (car data)) (found nil))
                     (loop
                       (when (null streams) (return found))
                       (when (listen (car streams)) (setq found t) (return t))
                       (setq streams (cdr streams)))))))
            ;; File stream: check if buffer has data
            ((= ty 9)
             (let ((bpos (%fs-bpos s))
                   (blen (%fs-blen s)))
               (if (< bpos blen) t
                   ;; Would need a non-blocking read to check — return t if fd valid
                   (if (>= (%fs-fd s) 0) t nil))))
            (t nil)))
        nil)))
(defun %substring (str start end)
  "Extract a substring from STR between START and END, preserving string subtag."
  (let ((len (- end start))
        (result (%make-string-array (- end start))))
    (let ((i 0))
      (loop
        (when (= i len) (return result))
        (aset result i (aref str (+ start i)))
        (setq i (+ i 1))))))

(defun read-line (&rest args)
  "Read a line from a stream. Args: [stream [eof-error-p [eof-value [recursive-p]]]].
   Per CLHS arity is at most 4 positional args — a 5th signals
   program-error."
  (when (> (length args) 4) (%signal-program-error))
  (let ((stream-arg (if args (car args) nil))
        (eof-error-p (if (cdr args) (cadr args) t))
        (eof-value (if (cddr args) (caddr args) nil)))
    (let ((s (%resolve-input-stream stream-arg)))
      ;; Use read-char to read characters until newline or EOF
      (let ((chars nil)
            (found-newline nil)
            (hit-eof nil))
        (loop
          (let ((ch (%read-char-from-stream s nil :eof-sentinel-7770002)))
            (cond
              ((eq ch :eof-sentinel-7770002)
               (setq hit-eof t)
               (return nil))
              ((char= ch #\Newline)
               (setq found-newline t)
               (return nil))
              (t (setq chars (cons (char-code ch) chars))))))
        (if (and hit-eof (null chars))
            ;; EOF with nothing read
            (if eof-error-p
                (values eof-value t)
                (values eof-value t))
            ;; Build string from collected chars
            (let ((result-chars (nreverse chars)))
              (let ((len (list-length result-chars)))
                (let ((str (%make-string-array len))
                      (i 0)
                      (cur result-chars))
                  (loop
                    (when (null cur) (return nil))
                    (aset str i (car cur))
                    (setq cur (cdr cur))
                    (setq i (+ i 1)))
                  (values str (if found-newline nil (if hit-eof t nil)))))))))))

(defun %input-stream-reads-chars-p (stream)
  "Return T if STREAM is a CHARACTER input stream (its elements are
   characters), NIL if it is a binary/byte stream.  Used by read-sequence
   to decide whether to read CHARACTERs or bytes — CLHS specifies the
   element type comes from the STREAM, not the destination sequence.
   In Modus all composite streams (string-input, echo, two-way,
   concatenated, synonym, serial) are character streams; only file
   streams can be binary, and STREAM-ELEMENT-TYPE reports their type."
  (let ((s (%resolve-input-stream stream)))
    (if (streamp s)
        (let ((ty (%stream-type s)))
          (if (= ty 9)
              ;; File stream: trust stream-element-type (CHARACTER vs a
              ;; byte type).  Modus currently reports CHARACTER for all
              ;; file streams; if binary tracking lands, this picks it up.
              (let ((et (stream-element-type s)))
                (if (eq et 'character) t nil))
              ;; All other composite stream types are character streams.
              t))
        ;; Non-stream designator resolved to a serial/terminal fallback —
        ;; treat as character.
        t)))

;;; read-sequence: read N elements from stream into seq starting at start
;;;
;;; CLHS: the element type read is determined by the STREAM, not by SEQ.
;;; A character stream yields CHARACTERs; a binary stream yields integers.
;;; The old code keyed off (stringp seq) and so always read BYTES into a
;;; general (non-string) vector — wrong for (read-sequence #(...) char-stream),
;;; which must store CHARACTER objects (make-two-way-stream.11,
;;; read-sequence.list/vector tests).  Now we dispatch on the resolved
;;; stream's element-type: char streams read via %read-char-from-stream and
;;; store the CHARACTER (string dests coerce to code via aset); byte streams
;;; read via %fs-read-byte.
(defun read-sequence (seq stream &rest args)
  "Read elements from STREAM into SEQ. Returns end position."
  (let ((start (if args (car args) 0))
        (end (if (cdr args) (cadr args) nil)))
    (let ((actual-end (if end end (length seq)))
          (i start)
          (in (%resolve-input-stream stream))
          (char-stream (%input-stream-reads-chars-p stream)))
      (loop
        (when (>= i actual-end) (return i))
        (if char-stream
            (let ((ch (%read-char-from-stream in nil :eof-sentinel-7770002)))
              (if (eq ch :eof-sentinel-7770002)
                  (return i)
                  ;; aset coerces a CHARACTER to its code for string dests
                  ;; (CLHS string element access) and stores the CHARACTER
                  ;; object unchanged into a general vector.
                  (aset seq i ch)))
            (let ((b (if (streamp stream)
                         (%fs-read-byte stream nil :eof-sentinel-7770002)
                         :eof-sentinel-7770002)))
              (if (eq b :eof-sentinel-7770002)
                  (return i)
                  (aset seq i b))))
        (setq i (+ i 1))))))

;;; write-sequence: write elements from seq to stream
(defun write-sequence (seq stream &rest args)
  "Write elements from SEQ to STREAM."
  (let ((start (if args (car args) 0))
        (end (if (cdr args) (cadr args) nil)))
    (let ((actual-end (if end end (length seq)))
          (i start)
          (s (%resolve-output-stream stream)))
      (loop
        (when (>= i actual-end) (return seq))
        (if (stringp seq)
            (%write-char-to-stream (%ensure-char-code (aref seq i)) s)
            (if (streamp s)
                (%fs-write-byte (aref seq i) s)
                (write-char-serial (aref seq i))))
        (setq i (+ i 1)))))
  seq)

(defun %equalp-array-elt (seq i)
  "Read element i from seq (vector or string) — coerce string bytes to chars."
  ;; AREF already yields a CHARACTER for strings (compiled code-char would
  ;; re-shift an already-character value), so just return it.
  (aref seq i))

(defun %equalp-array-array (a b)
  "Element-wise equalp over two non-cons sequences (vectors or strings).
   Uses LENGTH (fill-pointer aware) instead of ARRAY-LENGTH so a
   fill-pointered vector compares against its logical length, not
   its underlying storage size.  (equalp.11)"
  (let ((la (length a))
        (lb (length b)))
    (if (= la lb)
        (let ((i 0) (ok t))
          (loop
            (when (or (not ok) (= i la)) (return ok))
            (unless (equalp-impl (%equalp-array-elt a i)
                                  (%equalp-array-elt b i))
              (setq ok nil))
            (setq i (+ i 1))))
        nil)))

(defun equalp-impl (a b)
  (if (eql a b) t
    (if (and (characterp a) (characterp b))
        (char-equal a b)
      (if (consp a)
          (if (consp b)
              (if (equalp-impl (car a) (car b))
                  (equalp-impl (cdr a) (cdr b))
                  nil)
              nil)
          ;; Numeric mixed-type equality: int vs float, float vs ratio,
          ;; etc.  Per CLHS, EQUALP compares numbers via numeric =
          ;; ignoring type.  (= 3 3.0) is T, so (equalp 3 3.0) is T.
          (if (and (or (integerp a) (floatp-impl a) (ratiop a))
                   (or (integerp b) (floatp-impl b) (ratiop b)))
              (numeric-equal-p a b)
              ;; ANSI equalp: arrays of any type compare element-wise.
              (if (and (or (stringp a) (arrayp a))
                       (or (stringp b) (arrayp b)))
                  (%equalp-array-array a b)
                  nil))))))

