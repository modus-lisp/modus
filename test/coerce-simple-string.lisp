;;;; test/coerce-simple-string.lisp -- COERCE to SIMPLE-STRING copies a
;;;; non-simple string (CLHS 4.7).
;;;;
;;;;   ./modus --script test/coerce-simple-string.lisp
;;;;
;;;; A fill-pointer/adjustable character vector came back UNCHANGED from
;;;; (coerce x 'simple-string): its header said string, its slots did not hold
;;;; codes, so compiled CHAR read raw slots and got garbage.  seal's UTF8-DECODE
;;;; ends in exactly this coerce, so every websocket message parsed as junk.

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (name got want)
  (if (equal got want) (incf *pass*)
      (progn (incf *fail*) (format t "~&FAIL ~A: got ~S want ~S~%" name got want))))
(defun build (str)
  (let ((out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for ch across str do (vector-push-extend ch out))
    out))
(defun scan (s) ; compiled character access over the whole string
  (let ((codes '()))
    (dotimes (i (length s) (nreverse codes)) (push (char-code (char s i)) codes))))
(dolist (type '(simple-string simple-base-string))
  (let* ((fp (build "[1,\"ab\"]"))
         (s (coerce fp type)))
    (check "result is a fresh object" (eq s fp) nil)
    (check "simple" (typep s 'simple-string) t)
    (check "equal" (equal s "[1,\"ab\"]") t)
    (check "raw slot 0 is a code" (%prim-aref s 0) 91)
    (check "compiled char scan" (scan s) (scan "[1,\"ab\"]"))
    (check "read-from-string" (read-from-string (coerce (build "(1 \"ab\")") type)) '(1 "ab"))))
;; PARSE-INTEGER on a non-simple string (it read raw slots: PARSE-ERROR)
(check "parse-integer fill-pointer" (multiple-value-list (parse-integer (build "[12,") :start 1 :end 3)) '(12 3))
(check "parse-integer junk" (parse-integer (build " 42x") :junk-allowed t) 42)
;; TYPEP: a string is never an array of a non-character element type
(check "typep string u8" (typep "abc" '(vector (unsigned-byte 8))) nil)
(check "typep string simple-array u8" (typep "abc" '(simple-array (unsigned-byte 8) (*))) nil)
(check "typep string integer" (typep "abc" '(vector integer)) nil)
(check "typep string character" (typep "abc" '(vector character)) t)
(check "typep u8 vector u8" (typep (make-array 3 :element-type '(unsigned-byte 8)) '(vector (unsigned-byte 8))) t)
;; a target that is not SIMPLE- may keep the object (CLHS: already of the type)
(check "coerce 'string keeps a string" (equal (coerce (build "xy") 'string) "xy") t)
(check "simple string unchanged" (let ((s "abc")) (eq (coerce s 'simple-string) s)) t)
(format t "~&coerce-simple-string: ~D passed, ~D failed~%" *pass* *fail*)
(sys-exit (if (zerop *fail*) 0 1))
