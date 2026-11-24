;;;; lib/unicode-names-gen.lisp -- BUILD-HOST side of the Unicode character-name table.
;;;;
;;;; Runs in SBCL and returns the SOURCE TEXT of the data functions that
;;;; mvm/cl-unicode-names.lisp decodes in the image.  Every name comes from SBCL's own
;;;; CHAR-NAME and SB-UNICODE:UNICODE-1-NAME, so the image answers what SBCL answers.  The
;;;; text is checked in as mvm/cl-unicode-names-data.lisp, which every build reads -- a modus
;;;; host has no SBCL CHAR-NAME to generate it from -- and an SBCL build regenerates it and
;;;; warns when the two differ (build-cli-common; MODUS_UNICODE_NAMES_REGEN=1 rewrites it).
;;;;
;;;; WHAT IS STORED (see mvm/cl-unicode-names.lisp for the decoder):
;;;;   - codes 33..126 and 160..#x10FFFF whose CHAR-NAME is not "U<hex>" and is not ALGORITHMIC;
;;;;   - the Unicode 1.0 names SBCL's NAME-CHAR accepts as aliases (LINE_FEED_(LF), BYTE_ORDER_MARK);
;;;;   - ALGORITHMIC names are not stored: HANGUL_SYLLABLE_* is computed from the jamo, and runs
;;;;     like CJK_COMPATIBILITY_IDEOGRAPH-F900 (PREFIX-<code in hex>) or TANGUT_COMPONENT-001
;;;;     (PREFIX-<decimal offset>) are stored as one range each.  Every computed name is checked
;;;;     against the host here, so a rule that does not reproduce SBCL is a build error.
;;;;
;;;; ENCODING.  Names split into words at `_' and `-'.  A word dictionary (most frequent first,
;;;; last char of each word +128) plus, per name, a byte K (tokens shared with the previous name,
;;;; and the separator after them) and the remaining tokens, each V = word*3 + S (S: 0 end, 1 `_'
;;;; follows, 2 `-' follows) in 1 byte (V < 128), 2 bytes (lead 128..254) or 3 bytes (lead 255).
;;;; Every 32nd name shares nothing, and its byte offset is a checkpoint, so one name decodes
;;;; from at most 31 neighbours.  All data is strings of chars below 256: narrow literals on
;;;; x64/AArch64, one byte per entry.

(defpackage "MODUS.UNICODE-NAMES-GEN" (:use "CL") (:export "UNICODE-NAMES-SOURCE"))
(in-package "MODUS.UNICODE-NAMES-GEN")

(defconstant +block+ 32)        ; names per checkpoint
(defconstant +word-block+ 16)   ; words per word checkpoint

(defun hex (n) (format nil "~X" n))

(defun host-char-name (code)
  #+sbcl (char-name (code-char code))
  #-sbcl (error "The Unicode name table is generated from SBCL's CHAR-NAME; this host is ~A.  ~
                 Build with MODUS_UNICODE_NAMES=0, or build on SBCL."
                (lisp-implementation-type)))

(defun host-name-char (name)
  #+sbcl (let ((c (name-char name))) (and c (char-code c)))
  #-sbcl (progn name nil))

(defun host-unicode-1-name (code)
  #+sbcl (sb-unicode:unicode-1-name (code-char code))
  #-sbcl (progn code nil))

;;; ---- Hangul syllables (Unicode ch. 3.12) ---------------------------------------------------

(defparameter *jamo-l* #("G" "GG" "N" "D" "DD" "R" "M" "B" "BB" "S" "SS" "" "J" "JJ" "C" "K" "T" "P" "H"))
(defparameter *jamo-v* #("A" "AE" "YA" "YAE" "EO" "E" "YEO" "YE" "O" "WA" "WAE" "OE" "YO" "U" "WEO"
                          "WE" "WI" "YU" "EU" "YI" "I"))
(defparameter *jamo-t* #("" "G" "GG" "GS" "N" "NJ" "NH" "D" "L" "LG" "LM" "LB" "LS" "LT" "LP" "LH"
                          "M" "B" "BS" "S" "SS" "NG" "J" "C" "K" "T" "P" "H"))

(defun hangul-name (code)
  (let ((s (- code #xAC00)))
    (concatenate 'string "HANGUL_SYLLABLE_"
                 (aref *jamo-l* (floor s 588))
                 (aref *jamo-v* (floor (mod s 588) 28))
                 (aref *jamo-t* (mod s 28)))))

;;; ---- the ranges ----------------------------------------------------------------------------

(defun rule-of (code name)
  "(PREFIX KIND BASE WIDTH) when NAME is PREFIX-<suffix> with a suffix computable from CODE."
  (let ((p (position #\- name :from-end t)))
    (when (and p (< (1+ p) (length name)))
      (let ((pre (subseq name 0 p)) (suf (subseq name (1+ p))))
        (cond ((string= suf (hex code)) (list pre :hex 0 0))
              ((every #'digit-char-p suf)
               (list pre :dec (- code (parse-integer suf)) (length suf))))))))

(defun algorithmic-ranges (names)
  "Runs of at least 32 consecutive codes sharing one rule.  NAMES: vector code -> name or NIL."
  (let ((ranges nil) (code 0) (n (length names)))
    (loop while (< code n)
          do (let ((r (and (aref names code) (rule-of code (aref names code)))))
               (if (null r)
                   (incf code)
                   (let ((end code))
                     (loop while (and (< (1+ end) n) (aref names (1+ end))
                                      (equal r (rule-of (1+ end) (aref names (1+ end)))))
                           do (incf end))
                     (when (>= (- end code) 31)
                       (push (list* code end r) ranges))
                     (setq code (1+ end))))))
    (nreverse ranges)))

(defun range-name (range code)
  (destructuring-bind (start end pre kind base width) range
    (declare (ignore start end))
    (if (eq kind :hex)
        (format nil "~A-~X" pre code)
        (format nil "~A-~V,'0D" pre width (- code base)))))

;;; ---- encoding ------------------------------------------------------------------------------

(defun split-name (name)
  "List of (WORD . S): S 0 = end, 1 = `_' follows, 2 = `-' follows."
  (let ((out nil) (st 0))
    (loop for i from 0 to (length name)
          do (when (or (= i (length name)) (find (char name i) "_-"))
               (push (cons (subseq name st i)
                           (cond ((= i (length name)) 0) ((char= (char name i) #\_) 1) (t 2)))
                     out)
               (setq st (1+ i))))
    (nreverse out)))

(defun put-varint (n out)
  (loop (if (< n 128) (progn (vector-push-extend n out) (return))
            (progn (vector-push-extend (logior 128 (logand n 127)) out) (setq n (ash n -7))))))

(defun put-u24 (n out)
  (vector-push-extend (logand n 255) out)
  (vector-push-extend (logand (ash n -8) 255) out)
  (vector-push-extend (logand (ash n -16) 255) out))

(defun put-token (v out)
  (cond ((< v 128) (vector-push-extend v out))
        ((< v (+ 128 (* 127 256)))
         (let ((w (- v 128))) (vector-push-extend (+ 128 (ash w -8)) out) (vector-push-extend (logand w 255) out)))
        (t (let ((w (- v (+ 128 (* 127 256)))))
             (assert (< w 65536))
             (vector-push-extend 255 out)
             (vector-push-extend (ash w -8) out) (vector-push-extend (logand w 255) out)))))

(defun bytes () (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun lisp-string-literal (octets)
  (with-output-to-string (s)
    (write-char #\" s)
    (loop for b across octets
          do (let ((c (code-char b)))
               (when (or (char= c #\") (char= c #\\)) (write-char #\\ s))
               (write-char c s)))
    (write-char #\" s)))

(defun unicode-names-source (&key (stream nil))
  "Source text of the table's data functions.  STREAM, when given, gets a size report."
  (let* ((limit #x110000)
         (names (make-array limit :initial-element nil))
         (stored nil) (aliases nil))
    ;; 1. what SBCL calls each code; NIL for the unnamed (U<hex>) and for 0..32 / 127..159,
    ;;    which the image's own CHAR-NAME answers without the table.
    (loop for code from 0 below limit
          do (let ((nm (host-char-name code)))
               (when (and nm (or (<= 33 code 126) (>= code 160))
                          (string/= nm (concatenate 'string "U" (hex code))))
                 (setf (aref names code) nm))))
    ;; 2. Hangul and ranges come out; check every computed name against SBCL.
    (loop for code from #xAC00 to #xD7A3
          do (unless (equal (aref names code) (hangul-name code))
               (error "Hangul rule disagrees with the host at U+~X: ~S vs ~S"
                      code (hangul-name code) (aref names code))))
    (let ((ranges (algorithmic-ranges names)))
      (loop for code from #xAC00 to #xD7A3 do (setf (aref names code) nil))
      (dolist (r ranges)
        (loop for code from (first r) to (second r)
              do (assert (equal (range-name r code) (aref names code)))
                 (setf (aref names code) nil)))
      ;; 3. the stored names, in code order, and the aliases SBCL's NAME-CHAR accepts
      (loop for code from 0 below limit
            do (when (aref names code) (push (cons code (aref names code)) stored)))
      (setq stored (nreverse stored))
      (loop for code from 0 below limit
            do (let ((u (host-unicode-1-name code)))
                 (when (and u (plusp (length u))
                            (not (string-equal u (or (host-char-name code) "")))
                            (eql (host-name-char u) code))
                   (push (cons code (string-upcase u)) aliases))))
      (setq aliases (nreverse aliases))
      ;; 4. dictionary, by frequency of the words that are actually emitted
      (let* ((all (append stored aliases))
             (toks (mapcar (lambda (e) (split-name (cdr e))) all))
             (ks nil) (freq (make-hash-table :test 'equal)))
        (let ((prev nil) (i 0))
          (dolist (tk toks)
            (assert (<= (length tk) 32))
            (let ((k 0))
              ;; shared tokens agree on the word AND the separator after it, except the last
              ;; shared one, whose separator rides in the K byte; the name keeps one own token
              (unless (zerop (mod i +block+))
                (loop for j from 0 below (1- (length tk))
                      for a in tk for b in prev
                      do (if (string= (car a) (car b)) (setq k (1+ j)) (return))
                         (unless (eql (cdr a) (cdr b)) (return))))
              (setq k (min k 60))
              (push k ks)
              (dolist (w (nthcdr k tk)) (incf (gethash (car w) freq 0)))
              (setq prev tk i (1+ i)))))
        (setq ks (nreverse ks))
        (let* ((words (mapcar #'car (sort (loop for w being the hash-keys of freq using (hash-value f)
                                                 collect (cons w f))
                                           (lambda (a b) (or (> (cdr a) (cdr b))
                                                             (and (= (cdr a) (cdr b)) (string< (car a) (car b))))))))
               (index (make-hash-table :test 'equal))
               (dict (bytes)) (wck (bytes)) (stream-b (bytes)) (nck (bytes))
               (runs (bytes)) (acodes (bytes)) (rng (bytes)))
          (loop for w in words for i from 0
                do (setf (gethash w index) i)
                   (when (zerop (mod i +word-block+)) (put-u24 (fill-pointer dict) wck))
                   ;; the empty word (TIBETAN_LETTER_-A) is the lone byte 128
                   (when (zerop (length w)) (vector-push-extend 128 dict))
                   (loop for c across w for j from 0
                         do (assert (< 32 (char-code c) 128))
                            (vector-push-extend (if (= j (1- (length w))) (+ 128 (char-code c)) (char-code c))
                                                dict)))
          (loop for tk in toks for k in ks for i from 0
                do (when (zerop (mod i +block+)) (put-u24 (fill-pointer stream-b) nck))
                   (vector-push-extend (if (zerop k) 0 (+ (* k 2) (if (= (cdr (nth (1- k) tk)) 2) 1 0)))
                                       stream-b)
                   (dolist (w (nthcdr k tk))
                     (put-token (+ (* 3 (gethash (car w) index)) (cdr w)) stream-b)))
          ;; runs of stored codes: (gap, length) varints
          (let ((prev-end 0) (start nil) (last nil))
            (flet ((flush () (when start (put-varint (- start prev-end) runs)
                                   (put-varint (1+ (- last start)) runs)
                                   (setq prev-end (1+ last)))))
              (dolist (e stored)
                (if (and last (= (car e) (1+ last)))
                    (setq last (car e))
                    (progn (flush) (setq start (car e) last (car e)))))
              (flush)))
          (let ((p 0)) (dolist (e aliases) (put-varint (- (car e) p) acodes) (setq p (car e))))
          (dolist (r ranges)
            (destructuring-bind (start end pre kind base width) r
              (put-u24 start rng) (put-u24 end rng)
              (vector-push-extend (if (eq kind :hex) 0 1) rng)
              (put-u24 (if (eq kind :hex) 0 base) rng)
              (vector-push-extend width rng)
              (vector-push-extend (length pre) rng)
              (loop for c across pre do (vector-push-extend (char-code c) rng))))
          (let ((total (+ (length dict) (length wck) (length stream-b) (length nck)
                          (length runs) (length acodes) (length rng)))
                (raw (+ (reduce #'+ stored :key (lambda (e) (length (cdr e))))
                        (reduce #'+ aliases :key (lambda (e) (length (cdr e)))))))
            (when stream
              (format stream "~&;; unicode names: ~D stored + ~D aliases (~D raw bytes) + ~D ranges + Hangul; ~D words~%~
                              ;;   encoded ~D bytes: dict ~D, names ~D, checkpoints ~D+~D, runs ~D, alias codes ~D, ranges ~D~%"
                      (length stored) (length aliases) raw (length ranges) (length words)
                      total (length dict) (length stream-b) (length nck) (length wck)
                      (length runs) (length acodes) (length rng)))
            (with-output-to-string (s)
              (format s ";;; GENERATED by lib/unicode-names-gen.lisp from SBCL's CHAR-NAME, and checked in so~%~
                         ;;; that a host without it (modus building modus) bakes the same table.  An~%~
                         ;;; SBCL build regenerates it and warns if this file differs; MODUS_UNICODE_~%~
                         ;;; NAMES_REGEN=1 rewrites it.  ~D stored names, ~D aliases, ~D encoded bytes.~%"
                      (length stored) (length aliases) total)
              (format s "(in-package :modus.runtime)~%(defun %ucn-present-p () t)~%")
              (format s "(defun %ucn-stored-count () ~D)~%(defun %ucn-alias-count () ~D)~%"
                      (length stored) (length aliases))
              (loop for (fn data) in (list (list "%ucn-dict" dict) (list "%ucn-word-checkpoints" wck)
                                           (list "%ucn-names" stream-b) (list "%ucn-name-checkpoints" nck)
                                           (list "%ucn-runs" runs) (list "%ucn-alias-codes" acodes)
                                           (list "%ucn-ranges" rng))
                    do (format s "(defun ~A () ~A)~%" fn (lisp-string-literal data))))))))))

;;; ---- the build switch ----------------------------------------------------------------------

(defun unicode-names-wanted-p (default)
  "MODUS_UNICODE_NAMES=1 / =0 decides; unset, DEFAULT (build scripts pass: hosted T, bare NIL)."
  (let ((v (sb-ext:posix-getenv "MODUS_UNICODE_NAMES")))
    (cond ((null v) default)
          ((or (string= v "") (string= v "0")) nil)
          (t t))))
(export 'unicode-names-wanted-p)
