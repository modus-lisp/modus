;;;; scripts/gen-unicode-case.lisp -- write the Unicode case and digit tables into
;;;; mvm/prelude.lisp, between the BEGIN/END GENERATED UNICODE TABLES markers.
;;;;
;;;;   sbcl --script scripts/gen-unicode-case.lisp      (from the modus root)
;;;;
;;;; The tables are SBCL's answers, read off the host SBCL for every code point, so that
;;;; ALPHA-CHAR-P, UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P, CHAR-UPCASE, CHAR-DOWNCASE and
;;;; DIGIT-CHAR-P agree with SBCL's on all of them (the north star is SBCL drop-in: whatever
;;;; Unicode version that SBCL carries is the one modus answers with).  The output is SOURCE:
;;;; a build reads the generated text, not the host's tables, so an image is the same whichever
;;;; Lisp builds it.  Re-run after moving to an SBCL with a newer Unicode, and commit the result.
;;;;
;;;; Encoding.  Each table is one string constant (a long string literal is ONE pool constant,
;;;; not a store per element) holding a flat list of integers, four characters each, base 40
;;;; with digit d spelled (code-char (+ 40 d)): characters "(" .. "O", none of which is a quote
;;;; or a backslash.  40^4 = 2,560,000 covers a code point (< #x110000) and a case delta biased
;;;; by #x110000.
;;;;
;;;;   range tables (ALPHA UPPER LOWER BOTH):  lo hi lo hi ...        a code is in the set
;;;;                                                                 iff some lo <= code <= hi
;;;;   digit table:                            lo hi lo hi ...        each range is a run of
;;;;                                                                 decimal digits whose
;;;;                                                                 values are 0.. from lo
;;;;   case runs (UPCASE DOWNCASE):            lo hi delta stride ... code -> code + delta for
;;;;                                                                 lo <= code <= hi with
;;;;                                                                 (code - lo) mod stride = 0
;;;; All tables are sorted by lo and cover code points >= 128 only; ASCII stays arithmetic.

(defconstant +bias+ #x110000)

(defun ranges (pred)
  (let ((r '()) (lo nil))
    (loop for c from 128 below char-code-limit
          do (let ((ch (code-char c)))
               (if (and ch (funcall pred ch))
                   (unless lo (setf lo c))
                   (when lo (push (list lo (1- c)) r) (setf lo nil)))))
    (when lo (push (list lo (1- char-code-limit)) r))
    (nreverse r)))

(defun runs (f)
  "The points C >= 128 with (F C) /= C, as runs (lo hi delta stride)."
  (let ((pts '()))
    (loop for c from 128 below char-code-limit
          do (let ((ch (code-char c)))
               (when ch
                 (let ((d (char-code (funcall f ch))))
                   (unless (= d c) (push (cons c (- d c)) pts))))))
    (setf pts (nreverse pts))
    (let ((out '()))
      (loop while pts
            do (let* ((p (pop pts)) (lo (car p)) (d (cdr p)) (hi lo) (stride nil))
                 (loop while (and pts (= (cdar pts) d)
                                  (let ((s (- (caar pts) hi)))
                                    (and (<= s 2) (or (null stride) (= s stride)))))
                       do (setf stride (- (caar pts) hi) hi (car (pop pts))))
                 (push (list lo hi (+ d +bias+) (or stride 1)) out)))
      (nreverse out))))

(defun encode (rows)
  (with-output-to-string (s)
    (dolist (row rows)
      (dolist (v row)
        (assert (< -1 v (expt 40 4)))
        (loop for k from 3 downto 0
              do (write-char (code-char (+ 40 (mod (floor v (expt 40 k)) 40))) s))))))

;;; ---- the shapes the lookups in prelude.lisp rely on -------------------------
(defun check (what ok) (unless ok (error "gen-unicode-case: SBCL's data breaks an assumption: ~a" what)))

(check "ASCII is A-Z / a-z only, as prelude's fast path computes"
       (loop for c below 128 for ch = (code-char c)
             always (and (eq (not (alpha-char-p ch)) (not (or (<= 65 c 90) (<= 97 c 122))))
                         (eq (not (upper-case-p ch)) (not (<= 65 c 90)))
                         (eq (not (lower-case-p ch)) (not (<= 97 c 122)))
                         (eq (not (both-case-p ch)) (not (or (<= 65 c 90) (<= 97 c 122))))
                         (= (char-code (char-upcase ch)) (if (<= 97 c 122) (- c 32) c))
                         (= (char-code (char-downcase ch)) (if (<= 65 c 90) (+ c 32) c)))))
(check "ASCII case maps stay in ASCII, other case maps stay out of it"
       (loop for c from 128 below char-code-limit for ch = (code-char c)
             always (or (null ch) (and (>= (char-code (char-upcase ch)) 128)
                                       (>= (char-code (char-downcase ch)) 128)))))
(check "ALPHANUMERICP is ALPHA-CHAR-P or DIGIT-CHAR-P"
       (loop for c below char-code-limit for ch = (code-char c)
             always (or (null ch) (eq (not (alphanumericp ch))
                                      (not (or (alpha-char-p ch) (digit-char-p ch)))))))
(check "a non-ASCII digit's value does not depend on the radix (it is below 10)"
       (loop for c from 128 below char-code-limit for ch = (code-char c)
             always (or (null ch) (null (digit-char-p ch))
                        (and (< (digit-char-p ch) 10)
                             (loop for r from 2 to 36
                                   always (eql (digit-char-p ch r)
                                               (and (< (digit-char-p ch) r) (digit-char-p ch))))))))
(defvar *digits*
  ;; split wherever the weight does not go on by one, so adjacent blocks of 0..9 are two ranges
  (let ((out '()))
    (dolist (r (ranges #'digit-char-p) (nreverse out))
      (destructuring-bind (lo hi) r
        (let ((start lo))
          (loop for c from (1+ lo) to hi
                unless (= (digit-char-p (code-char c)) (1+ (digit-char-p (code-char (1- c)))))
                  do (push (list start (1- c)) out) (setf start c))
          (push (list start hi) out))))))
(check "each non-ASCII digit range counts 0, 1, 2 ... from its first code"
       (loop for (lo hi) in *digits*
             always (loop for c from lo to hi always (= (digit-char-p (code-char c)) (- c lo)))))
(check "CHAR-EQUAL is equality of CHAR-DOWNCASE of CHAR-UPCASE, against each code's own case maps"
       (loop for c from 128 below char-code-limit for ch = (code-char c)
             always (or (null ch)
                        (let ((key (char-downcase (char-upcase ch))))
                          (loop for o in (list (char-upcase ch) (char-downcase ch) key
                                               (char-upcase key) (char-downcase key))
                                always (eq (not (char-equal ch o))
                                           (not (char= key (char-downcase (char-upcase o))))))))))

;;; ---- write the block -------------------------------------------------------
(defvar *begin* ";;; BEGIN GENERATED UNICODE TABLES")
(defvar *end* ";;; END GENERATED UNICODE TABLES")

(defun table-defun (name doc rows)
  (format nil "(defun ~a ()~%  ~s~%  ~s)~%" name doc (encode rows)))

(let* ((path "mvm/prelude.lisp")
       (text (with-open-file (i path :external-format :utf-8)
               (let ((s (make-string (file-length i))))
                 (subseq s 0 (read-sequence s i)))))
       (b (search *begin* text)) (e (search *end* text))
       (block
         (with-output-to-string (o)
           (format o "~a -- written by scripts/gen-unicode-case.lisp from ~a ~a; do not edit.~%"
                   *begin* (lisp-implementation-type) (lisp-implementation-version))
           (format o ";;; Code points >= 128 only (ASCII is arithmetic in %CODE-UPCASE and friends).~%")
           (dolist (spec `(("%UC-ALPHA-TABLE" "ALPHA-CHAR-P ranges: lo hi ..." ,(ranges #'alpha-char-p))
                           ("%UC-UPPER-TABLE" "UPPER-CASE-P ranges: lo hi ..." ,(ranges #'upper-case-p))
                           ("%UC-LOWER-TABLE" "LOWER-CASE-P ranges: lo hi ..." ,(ranges #'lower-case-p))
                           ("%UC-BOTH-TABLE" "BOTH-CASE-P ranges: lo hi ..." ,(ranges #'both-case-p))
                           ("%UC-DIGIT-TABLE" "decimal digit ranges, value (- code lo): lo hi ..." ,*digits*)
                           ("%UC-UPCASE-TABLE" "CHAR-UPCASE runs: lo hi delta+#x110000 stride ..." ,(runs #'char-upcase))
                           ("%UC-DOWNCASE-TABLE" "CHAR-DOWNCASE runs: lo hi delta+#x110000 stride ..." ,(runs #'char-downcase))))
             (destructuring-bind (name doc rows) spec
               (format o "~a" (table-defun name (format nil "~a (~d rows)" doc (length rows)) rows))))
           (format o "~a" *end*))))
  (unless (and b e (< b e)) (error "gen-unicode-case: no ~a ... ~a block in ~a" *begin* *end* path))
  (with-open-file (o path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string (subseq text 0 b) o)
    (write-string block o)
    (write-string (subseq text (+ e (length *end*))) o))
  (format t "~&gen-unicode-case: wrote ~d characters of tables into ~a~%" (length block) path))
