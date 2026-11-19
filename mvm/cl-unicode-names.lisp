;;;; mvm/cl-unicode-names.lisp -- the Unicode character-name table, decoder half.
;;;;
;;;; CHAR-NAME and NAME-CHAR (mvm/cl-eval.lisp) answer the names every image knows -- the
;;;; control characters, SBCL's short aliases, U+XXXX -- and ask this file for the rest:
;;;; %UCN-CHAR-NAME (code -> SBCL's name) and %UCN-NAME-CHAR (upcased name -> code).
;;;;
;;;; The DATA is not in this file.  lib/unicode-names-gen.lisp generates it at build time
;;;; from the build host's own CHAR-NAME and appends it after this file, so the image always
;;;; names characters exactly as the SBCL that built it does.  The encoding is described
;;;; there.  Images built without the table (MODUS_UNICODE_NAMES=0, the bare-metal default)
;;;; bake mvm/cl-unicode-names-absent.lisp instead, whose %UCN-PRESENT-P is NIL.
;;;;
;;;; A dictionary word ends at its first byte >= 128 (its last char + 128); the empty word
;;;; (TIBETAN_LETTER_-A splits into ..., "", "A") is the lone byte 128.
;;;;
;;;; The only state is a lazily built hash table, name hash -> ordinal(s), made on the
;;;; first NAME-CHAR that needs the table (~34k entries).  CHAR-NAME needs no state.

(in-package :modus.runtime)

(defvar *%ucn-hash* nil)

(defun %ucn-byte (s i) (char-code (char s i)))

(defun %ucn-u24 (s i)
  (+ (%ucn-byte s i) (* 256 (%ucn-byte s (+ i 1))) (* 65536 (%ucn-byte s (+ i 2)))))

;;; A two-slot cursor, so readers can return a value AND advance without multiple values:
;;; (aref cur 0) is the position, (aref cur 1) the value last read.
(defun %ucn-varint (s cur)
  (let ((p (aref cur 0)) (v 0) (shift 1))
    (loop
      (let ((b (%ucn-byte s p)))
        (setq p (+ p 1))
        (if (< b 128)
            (progn (setq v (+ v (* b shift))) (return nil))
            (progn (setq v (+ v (* (- b 128) shift))) (setq shift (* shift 128))))))
    (setf (aref cur 0) p)
    (setf (aref cur 1) v)
    v))

(defun %ucn-token (s cur)
  (let* ((p (aref cur 0)) (b (%ucn-byte s p)) (v 0))
    (cond ((< b 128) (setq v b) (setq p (+ p 1)))
          ((< b 255) (setq v (+ 128 (* (- b 128) 256) (%ucn-byte s (+ p 1)))) (setq p (+ p 2)))
          (t (setq v (+ 128 (* 127 256) (* 256 (%ucn-byte s (+ p 1))) (%ucn-byte s (+ p 2))))
             (setq p (+ p 3))))
    (setf (aref cur 0) p)
    (setf (aref cur 1) v)
    v))

(defun %ucn-word-start (idx)
  "Byte offset of word IDX in the dictionary (a checkpoint every 16 words)."
  (let* ((d (%ucn-dict))
         (p (%ucn-u24 (%ucn-word-checkpoints) (* 3 (floor idx 16))))
         (n (mod idx 16)))
    (loop
      (when (= n 0) (return p))
      (loop (let ((b (%ucn-byte d p))) (setq p (+ p 1)) (when (>= b 128) (return nil))))
      (setq n (- n 1)))))

;;; ---- one name's tokens ---------------------------------------------------------------------
;;; STATE is a vector: slot 0 the token count, then (word, separator) pairs from slot 1.

(defun %ucn-make-state () (make-array 66 :initial-element 0))

(defun %ucn-decode-next (names cur state)
  "Decode the name at (aref CUR 0) on top of STATE (the previous name's tokens)."
  (let* ((p (aref cur 0)) (kb (%ucn-byte names p)) (n 0))
    (setf (aref cur 0) (+ p 1))
    (when (> kb 0)
      (setq n (floor kb 2))
      (setf (aref state (* 2 n)) (if (= (mod kb 2) 1) 2 1)))
    (loop
      (let* ((v (%ucn-token names cur)) (s (mod v 3)))
        (setf (aref state (+ 1 (* 2 n))) (floor v 3))
        (setf (aref state (+ 2 (* 2 n))) s)
        (setq n (+ n 1))
        (when (= s 0) (return nil))))
    (setf (aref state 0) n)
    n))

(defun %ucn-decode-ordinal (ord state)
  "STATE <- the tokens of name ORD (0-based over stored names, then aliases)."
  (let* ((names (%ucn-names))
         (cur (make-array 2 :initial-element 0))
         (n (mod ord 32)))
    (setf (aref cur 0) (%ucn-u24 (%ucn-name-checkpoints) (* 3 (floor ord 32))))
    (%ucn-decode-next names cur state)
    (loop
      (when (= n 0) (return state))
      (%ucn-decode-next names cur state)
      (setq n (- n 1)))))

(defun %ucn-state-string (state)
  (let* ((d (%ucn-dict)) (n (aref state 0)) (len 0) (i 0))
    ;; length first
    (loop
      (when (= i n) (return nil))
      (let ((p (%ucn-word-start (aref state (+ 1 (* 2 i))))))
        (loop (let ((b (%ucn-byte d p)))
                (unless (= b 128) (setq len (+ len 1)))
                (setq p (+ p 1))
                (when (>= b 128) (return nil)))))
      (when (> (aref state (+ 2 (* 2 i))) 0) (setq len (+ len 1)))
      (setq i (+ i 1)))
    (let ((out (make-string len)) (j 0))
      (setq i 0)
      (loop
        (when (= i n) (return out))
        (let ((p (%ucn-word-start (aref state (+ 1 (* 2 i))))))
          (loop (let ((b (%ucn-byte d p)))
                  (unless (= b 128)
                    (setf (char out j) (code-char (if (> b 128) (- b 128) b)))
                    (setq j (+ j 1)))
                  (setq p (+ p 1))
                  (when (>= b 128) (return nil)))))
        (let ((s (aref state (+ 2 (* 2 i)))))
          (when (> s 0)
            (setf (char out j) (if (= s 1) #\_ #\-))
            (setq j (+ j 1))))
        (setq i (+ i 1))))))

;;; ---- ordinals <-> codes --------------------------------------------------------------------

(defun %ucn-code-ordinal (code)
  "Ordinal of CODE among the stored names, or NIL."
  (let* ((runs (%ucn-runs)) (end (length runs))
         (cur (make-array 2 :initial-element 0))
         (prev-end 0) (ord 0))
    (loop
      (when (>= (aref cur 0) end) (return nil))
      (let* ((start (+ prev-end (%ucn-varint runs cur)))
             (len (%ucn-varint runs cur)))
        (when (< code start) (return nil))
        (when (< code (+ start len)) (return (+ ord (- code start))))
        (setq ord (+ ord len))
        (setq prev-end (+ start len))))))

(defun %ucn-ordinal-code (ord)
  (let ((nstored (%ucn-stored-count)) (cur (make-array 2 :initial-element 0)))
    (if (< ord nstored)
        (let ((runs (%ucn-runs)) (prev-end 0))
          (loop
            (let* ((start (+ prev-end (%ucn-varint runs cur)))
                   (len (%ucn-varint runs cur)))
              (when (< ord len) (return (+ start ord)))
              (setq ord (- ord len))
              (setq prev-end (+ start len)))))
        (let ((codes (%ucn-alias-codes)) (k (- ord nstored)) (c 0))
          (loop
            (setq c (+ c (%ucn-varint codes cur)))
            (when (= k 0) (return c))
            (setq k (- k 1)))))))

;;; ---- Hangul syllables (Unicode ch. 3.12) ---------------------------------------------------

(defun %ucn-jamo (kind i)
  (cond ((= kind 0) (nth i '("G" "GG" "N" "D" "DD" "R" "M" "B" "BB" "S" "SS" "" "J" "JJ" "C" "K" "T" "P" "H")))
        ((= kind 1) (nth i '("A" "AE" "YA" "YAE" "EO" "E" "YEO" "YE" "O" "WA" "WAE" "OE" "YO" "U" "WEO"
                             "WE" "WI" "YU" "EU" "YI" "I")))
        (t (nth i '("" "G" "GG" "GS" "N" "NJ" "NH" "D" "L" "LG" "LM" "LB" "LS" "LT" "LP" "LH"
                    "M" "B" "BS" "S" "SS" "NG" "J" "C" "K" "T" "P" "H")))))

(defun %ucn-hangul-name (code)
  (let ((s (- code #xAC00)))
    (concatenate 'string "HANGUL_SYLLABLE_"
                 (%ucn-jamo 0 (floor s 588))
                 (%ucn-jamo 1 (floor (mod s 588) 28))
                 (%ucn-jamo 2 (mod s 28)))))

(defun %ucn-match-at (name pos part)
  "T when PART occurs in NAME at POS."
  (let ((n (length part)) (i 0))
    (if (> (+ pos n) (length name))
        nil
        (loop
          (when (= i n) (return t))
          (unless (char= (char name (+ pos i)) (char part i)) (return nil))
          (setq i (+ i 1))))))

(defun %ucn-hangul-code (name)
  "NAME (upcased) as a Hangul syllable, or NIL."
  (if (not (%ucn-match-at name 0 "HANGUL_SYLLABLE_"))
      nil
      (let ((l 0) (hit nil))
        (loop
          (when (or hit (= l 19)) (return hit))
          (let ((pl (%ucn-jamo 0 l)))
            (when (%ucn-match-at name 16 pl)
              (let ((v 0) (p1 (+ 16 (length pl))))
                (loop
                  (when (or hit (= v 21)) (return nil))
                  (let ((pv (%ucn-jamo 1 v)))
                    (when (%ucn-match-at name p1 pv)
                      (let ((tt 0) (p2 (+ p1 (length pv))))
                        (loop
                          (when (or hit (= tt 28)) (return nil))
                          (let ((pt (%ucn-jamo 2 tt)))
                            (when (and (= (+ p2 (length pt)) (length name))
                                       (%ucn-match-at name p2 pt))
                              (setq hit (+ #xAC00 (* l 588) (* v 28) tt))))
                          (setq tt (+ tt 1))))))
                  (setq v (+ v 1))))))
          (setq l (+ l 1))))))

;;; ---- ranges: PREFIX-<hex code> and PREFIX-<decimal offset> ---------------------------------
;;; A range record: start(3) end(3) kind(1: 0 hex, 1 decimal) base(3) width(1) prefix-length(1) prefix.

(defun %ucn-range-name (r p code)
  "SBCL's name for CODE under the range record at offset P of R."
  (let* ((kind (%ucn-byte r (+ p 6)))
         (base (%ucn-u24 r (+ p 7)))
         (width (%ucn-byte r (+ p 10)))
         (plen (%ucn-byte r (+ p 11)))
         (prefix (make-string plen))
         (i 0))
    (loop (when (= i plen) (return nil))
      (setf (char prefix i) (char r (+ p 12 i)))
      (setq i (+ i 1)))
    (concatenate 'string prefix "-"
                 (if (= kind 0)
                     (string-upcase (write-to-string code :base 16 :radix nil))
                     (let ((s (write-to-string (- code base) :base 10 :radix nil)))
                       (if (< (length s) width)
                           (concatenate 'string (make-string (- width (length s)) :initial-element #\0) s)
                           s))))))

(defun %ucn-range-char-name (code)
  (let* ((r (%ucn-ranges)) (p 0) (end (length r)))
    (loop
      (when (>= p end) (return nil))
      (when (and (>= code (%ucn-u24 r p)) (<= code (%ucn-u24 r (+ p 3))))
        (return (%ucn-range-name r p code)))
      (setq p (+ p 12 (%ucn-byte r (+ p 11)))))))

(defun %ucn-range-name-code (name)
  "NAME (upcased) as a range name, or NIL: the suffix gives a candidate code, which counts
   only if that code's name is NAME exactly (no leading zeros SBCL would not write)."
  (let* ((r (%ucn-ranges)) (p 0) (end (length r)) (hit nil))
    (loop
      (when (or hit (>= p end)) (return hit))
      (let* ((plen (%ucn-byte r (+ p 11))) (i 0) (ok (> (length name) (+ plen 1))))
        (loop (when (or (not ok) (= i plen)) (return nil))
          (unless (char= (char name i) (char r (+ p 12 i))) (setq ok nil))
          (setq i (+ i 1)))
        (when (and ok (char= (char name plen) #\-))
          (let* ((kind (%ucn-byte r (+ p 6)))
                 (n (parse-integer name :start (+ plen 1) :radix (if (= kind 0) 16 10) :junk-allowed t))
                 (code (and n (if (= kind 0) n (+ n (%ucn-u24 r (+ p 7)))))))
            (when (and code (>= code (%ucn-u24 r p)) (<= code (%ucn-u24 r (+ p 3)))
                       (string= name (%ucn-range-name r p code)))
              (setq hit code))))
        (setq p (+ p 12 plen))))))

;;; ---- the two entry points ------------------------------------------------------------------

(defun %ucn-char-name (code)
  "SBCL's CHAR-NAME for CODE (33..126 and 160 up): its Unicode name, or U<hex> when it has none."
  (cond ((and (>= code #xAC00) (<= code #xD7A3)) (%ucn-hangul-name code))
        ((%ucn-range-char-name code))
        (t (let ((ord (%ucn-code-ordinal code)))
             (if ord
                 (%ucn-state-string (%ucn-decode-ordinal ord (%ucn-make-state)))
                 (concatenate 'string "U" (string-upcase (write-to-string code :base 16 :radix nil))))))))

(defun %ucn-hash-step (h c) (logand (+ (* h 33) c) #xFFFFFF))

(defun %ucn-string-hash (s)
  (let ((h 0) (i 0) (n (length s)))
    (loop (when (= i n) (return h))
      (setq h (%ucn-hash-step h (char-code (char s i))))
      (setq i (+ i 1)))))

(defun %ucn-build-hash ()
  "Name hash -> ordinal (a list when hashes collide), over every stored name and alias."
  (let* ((table (make-hash-table))
         (names (%ucn-names)) (d (%ucn-dict))
         (total (+ (%ucn-stored-count) (%ucn-alias-count)))
         (cur (make-array 2 :initial-element 0))
         (state (%ucn-make-state))
         ;; HW[i]: the hash after word i of the current name (prefixes shared with the
         ;; previous name keep theirs)
         (hw (make-array 33 :initial-element 0))
         (ord 0))
    (loop
      (when (= ord total) (return nil))
      (when (= (mod ord 32) 0)
        (setf (aref cur 0) (%ucn-u24 (%ucn-name-checkpoints) (* 3 (floor ord 32)))))
      (let* ((kb (%ucn-byte names (aref cur 0)))
             (k (floor kb 2))
             (n (%ucn-decode-next names cur state))
             (i k)
             (h (if (= k 0) 0
                    (%ucn-hash-step (aref hw (- k 1))
                                    (if (= (aref state (* 2 k)) 1) 95 45)))))
        (loop
          (when (= i n) (return nil))
          (let ((p (%ucn-word-start (aref state (+ 1 (* 2 i))))))
            (loop (let ((b (%ucn-byte d p)))
                    (unless (= b 128)
                      (setq h (%ucn-hash-step h (if (> b 128) (- b 128) b))))
                    (setq p (+ p 1))
                    (when (>= b 128) (return nil)))))
          (setf (aref hw i) h)
          (let ((s (aref state (+ 2 (* 2 i)))))
            (when (> s 0) (setq h (%ucn-hash-step h (if (= s 1) 95 45)))))
          (setq i (+ i 1)))
        (let ((key (aref hw (- n 1))))
          (let ((old (gethash key table)))
            (setf (gethash key table)
                  (cond ((null old) ord)
                        ((consp old) (append old (list ord)))
                        (t (list old ord)))))))
      (setq ord (+ ord 1)))
    table))

(defun %ucn-name-char (name)
  "The code SBCL's NAME-CHAR gives NAME (already upcased) by its Unicode name, or NIL."
  (or (%ucn-hangul-code name)
      (%ucn-range-name-code name)
      (let ((table *%ucn-hash*))
        (unless table
          (setq table (%ucn-build-hash))
          (setq *%ucn-hash* table))
        (let ((hit (gethash (%ucn-string-hash name) table)))
          (when hit
            (let ((cands (if (consp hit) hit (list hit))) (state (%ucn-make-state)) (found nil))
              ;; ascending ordinals: a stored name beats an alias, as in SBCL (#\Bell is U+1F514)
              (loop
                (when (or found (null cands)) (return found))
                (when (string= name (%ucn-state-string (%ucn-decode-ordinal (car cands) state)))
                  (setq found (%ucn-ordinal-code (car cands))))
                (setq cands (cdr cands)))))))))
