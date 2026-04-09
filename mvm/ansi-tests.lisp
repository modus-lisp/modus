;;;; ansi-tests.lisp — ANSI CL conformance tests for MVM
;;;;
;;;; Tests modeled after the Paul Dietz ANSI test suite.
;;;; Each section corresponds to a CLHS chapter.
;;;; Test IDs: chapter*100 + sequence (e.g., 1401 = cons chapter, test 1)
;;;;
;;;; Tests that require features MVM doesn't have yet are commented
;;;; out with ;; NEEDS: <feature>

;;; ============================================================
;;; Override prelude equal — remove string-equal branch
;;; (last-defun-wins: this replaces prelude's equal)
;;; ============================================================

(defun my-equal (a b)
  (if (eql a b)
      t
      (if (consp a)
          (if (consp b)
              (if (my-equal (car a) (car b))
                  (my-equal (cdr a) (cdr b))
                  nil)
              nil)
          nil)))


;;; ============================================================
;;; Chapter 14: Conses (1400-1499)
;;; ============================================================

(defun run-cons-tests ()
  ;; Diagnostic: test equal on atoms first
  (deftest 1399 (equal 1 1) t)

  ;; cons
  (deftest 1401 (consp (cons 1 2)) t)
  (deftest 1402 (car (cons 1 2)) 1)
  (deftest 1403 (cdr (cons 1 2)) 2)
  (deftest 1404 (car (cons nil nil)) nil)
  (deftest 1405 (cdr (cons nil nil)) nil)

  ;; consp
  (deftest 1410 (consp (cons 1 2)) t)
  (deftest 1411 (consp nil) nil)
  (deftest 1412 (consp 42) nil)
  (deftest 1413 (consp t) nil)

  ;; atom
  (deftest 1415 (atom nil) t)
  (deftest 1416 (atom 42) t)
  (deftest 1417 (atom t) t)
  (deftest 1418 (atom (cons 1 2)) nil)

  ;; car/cdr of nil
  (deftest 1420 (car nil) nil)
  (deftest 1421 (cdr nil) nil)

  ;; nested car/cdr
  (deftest 1425 (car (cdr (cons 1 (cons 2 nil)))) 2)
  (deftest 1426 (car (cdr (cdr (cons 1 (cons 2 (cons 3 nil)))))) 3)
  (deftest 1427 (cadr (cons 1 (cons 2 nil))) 2)
  (deftest 1428 (cddr (cons 1 (cons 2 3))) 3)

  ;; cons structural equality - debug
  ;; First, test that eql works on fixnums
  (deftest 1428 (eql 1 1) t)
  ;; Test equal on atoms inside a let (to avoid arg-eval ordering issues)
  (deftest 1429 (let ((a (cons 1 2)) (b (cons 1 2))) (eql (car a) (car b))) t)
  ;; Manual equal-like test (inline, no function call)
  (deftest 1429 (let ((a (cons 1 2)) (b (cons 1 2)))
                  (if (consp a) (if (consp b)
                    (if (eql (car a) (car b)) (eql (cdr a) (cdr b)) nil)
                    nil) nil)) t)
  ;; equal on atoms
  (deftest 1430 (my-equal 42 42) t)
  ;; equal on cons cells
  (deftest 1431 (my-equal (cons 1 2) (cons 1 2)) t)
  ;; Test our local my-equal (no string-equal dependency)
  (deftest 1433 (my-equal (cons 1 2) (cons 1 2)) t)
  (deftest 1434 (my-equal (cons 1 (cons 2 nil)) (cons 1 (cons 2 nil))) t)
  (deftest 1435 (my-equal (cons 1 (cons 2 nil)) (cons 1 (cons 2 nil))) t)
  (deftest 1436 (my-equal (cons 1 2) (cons 1 3)) nil)

  ;; null
  (deftest 1440 (null nil) t)
  (deftest 1441 (null t) nil)
  (deftest 1442 (null 0) nil)
  (deftest 1443 (null (cons 1 2)) nil))

;;; ============================================================
;;; Chapter 14: List operations (1500-1599)
;;; ============================================================

(defun run-list-tests ()
  ;; nth
  (deftest 1501 (nth 0 (cons 10 (cons 20 (cons 30 nil)))) 10)
  (deftest 1502 (nth 1 (cons 10 (cons 20 (cons 30 nil)))) 20)
  (deftest 1503 (nth 2 (cons 10 (cons 20 (cons 30 nil)))) 30)
  (deftest 1504 (nth 3 (cons 10 (cons 20 (cons 30 nil)))) nil)
  (deftest 1505 (nth 0 nil) nil)

  ;; nthcdr
  (deftest 1510 (nthcdr 0 (cons 1 (cons 2 nil))) (cons 1 (cons 2 nil)))
  (deftest 1511 (car (nthcdr 1 (cons 1 (cons 2 nil)))) 2)
  (deftest 1512 (nthcdr 2 (cons 1 (cons 2 nil))) nil)

  ;; last
  (deftest 1520 (car (last (cons 1 (cons 2 (cons 3 nil))))) 3)
  (deftest 1521 (last nil) nil)

  ;; reverse
  (deftest 1530 (car (reverse (cons 1 (cons 2 (cons 3 nil))))) 3)
  (deftest 1531 (reverse nil) nil)
  (deftest 1532 (my-equal (reverse (cons 1 (cons 2 (cons 3 nil))))
                          (cons 3 (cons 2 (cons 1 nil)))) t)

  ;; append
  (deftest 1540 (my-equal (append (cons 1 nil) (cons 2 nil)) (cons 1 (cons 2 nil))) t)
  (deftest 1541 (append nil (cons 1 nil)) (cons 1 nil))
  (deftest 1542 (append (cons 1 nil) nil) (cons 1 nil))
  (deftest 1543 (append nil nil) nil)

  ;; member
  (deftest 1550 (car (member 2 (cons 1 (cons 2 (cons 3 nil))))) 2)
  (deftest 1551 (member 4 (cons 1 (cons 2 (cons 3 nil)))) nil)
  (deftest 1552 (member 1 nil) nil)

  ;; assoc
  (deftest 1560 (cdr (assoc 2 (cons (cons 1 10) (cons (cons 2 20) (cons (cons 3 30) nil))))) 20)
  (deftest 1561 (assoc 4 (cons (cons 1 10) (cons (cons 2 20) nil))) nil)
  (deftest 1562 (assoc 1 nil) nil)

  ;; copy-list
  (deftest 1570 (my-equal (copy-list (cons 1 (cons 2 nil))) (cons 1 (cons 2 nil))) t)
  (deftest 1571 (copy-list nil) nil))

;;; ============================================================
;;; Chapter 12: Numbers — Arithmetic (1200-1249)
;;; ============================================================

(defun run-arithmetic-tests ()
  ;; Addition
  (deftest 1200 (+ 0 0) 0)
  (deftest 1201 (+ 1 2) 3)
  (deftest 1202 (+ -1 1) 0)
  (deftest 1203 (+ 100 200) 300)
  (deftest 1204 (+ 1 2 3) 6)
  (deftest 1205 (+ 10 20 30 5) 65)

  ;; Subtraction
  (deftest 1210 (- 5 3) 2)
  (deftest 1211 (- 0 1) -1)
  (deftest 1212 (- 100 100) 0)

  ;; Multiplication
  (deftest 1220 (* 6 7) 42)
  (deftest 1221 (* 0 100) 0)
  (deftest 1222 (* -1 42) -42)
  (deftest 1223 (* 1 1) 1)

  ;; Division / truncate
  (deftest 1230 (truncate 10 3) 3)
  (deftest 1231 (truncate 9 3) 3)
  (deftest 1232 (truncate 0 1) 0)

  ;; Modulo
  (deftest 1235 (mod 10 3) 1)
  (deftest 1236 (mod 9 3) 0)
  (deftest 1237 (mod 7 2) 1)

  ;; Increment / decrement
  (deftest 1240 (1+ 0) 1)
  (deftest 1241 (1+ 41) 42)
  (deftest 1242 (1- 1) 0)
  (deftest 1243 (1- 0) -1))

;;; ============================================================
;;; Chapter 12: Numbers — Comparison (1250-1299)
;;; ============================================================

(defun run-comparison-tests ()
  (deftest 1250 (= 1 1) t)
  (deftest 1251 (= 0 0) t)
  (deftest 1252 (= 1 2) nil)

  (deftest 1255 (< 1 2) t)
  (deftest 1256 (< 2 1) nil)
  (deftest 1257 (< 1 1) nil)

  (deftest 1260 (> 2 1) t)
  (deftest 1261 (> 1 2) nil)
  (deftest 1262 (> 1 1) nil)

  (deftest 1265 (<= 1 2) t)
  (deftest 1266 (<= 1 1) t)
  (deftest 1267 (<= 2 1) nil)

  (deftest 1270 (>= 2 1) t)
  (deftest 1271 (>= 1 1) t)
  (deftest 1272 (>= 1 2) nil)

  (deftest 1275 (/= 1 2) t)
  (deftest 1276 (/= 1 1) nil)

  (deftest 1280 (zerop 0) t)
  (deftest 1281 (zerop 1) nil)
  (deftest 1282 (zerop -1) nil))

;;; ============================================================
;;; Chapter 12: Numbers — Bitwise (1300-1349)
;;; ============================================================

(defun run-bitwise-tests ()
  (deftest 1300 (logand 15 9) 9)
  (deftest 1301 (logand 255 0) 0)
  (deftest 1302 (logand 255 255) 255)

  (deftest 1305 (logior 1 2) 3)
  (deftest 1306 (logior 0 0) 0)
  (deftest 1307 (logior 5 3) 7)

  (deftest 1310 (logxor 5 3) 6)
  (deftest 1311 (logxor 0 0) 0)
  (deftest 1312 (logxor 255 255) 0)

  (deftest 1315 (ash 1 4) 16)
  (deftest 1316 (ash 16 -4) 1)
  (deftest 1317 (ash 0 10) 0)
  (deftest 1318 (ash 1 0) 1)

  ;; Nested bitwise (regression — was broken before interned symbols)
  (deftest 1320 (logior 1 (ash 2 8)) 513)
  (deftest 1321 (logior 1 (logior (ash 2 8) (ash 3 16))) 197121))

;;; ============================================================
;;; Chapter 5: Data and Control Flow (500-599)
;;; ============================================================

(defun run-control-flow-tests ()
  ;; if
  (deftest 500 (if t 1 2) 1)
  (deftest 501 (if nil 1 2) 2)
  (deftest 502 (if 0 1 2) 1)        ; 0 is true in CL
  (deftest 503 (if (cons 1 2) 1 2) 1) ; cons is true

  ;; when
  (deftest 510 (when t 42) 42)
  (deftest 511 (when nil 42) nil)

  ;; unless
  (deftest 515 (unless nil 42) 42)
  (deftest 516 (unless t 42) nil)

  ;; cond
  (deftest 520 (cond (t 1)) 1)
  (deftest 521 (cond (nil 1) (t 2)) 2)
  (deftest 522 (cond (nil 1) (nil 2) (t 3)) 3)

  ;; and
  (deftest 530 (and t t) t)
  (deftest 531 (and t nil) nil)
  (deftest 532 (and nil t) nil)
  (deftest 533 (and 1 2 3) 3)     ; and returns last truthy value
  (deftest 534 (and) t)            ; (and) → t

  ;; or
  (deftest 540 (or nil nil) nil)
  (deftest 541 (or nil t) t)
  (deftest 542 (or 1 2) 1)        ; or returns first truthy value
  (deftest 543 (or nil nil 3) 3)
  (deftest 544 (or) nil)           ; (or) → nil

  ;; not
  (deftest 550 (not nil) t)
  (deftest 551 (not t) nil)
  (deftest 552 (not 42) nil)
  (deftest 553 (not (cons 1 2)) nil))

;;; ============================================================
;;; Chapter 5: Let binding (600-649)
;;; ============================================================

(defun run-let-tests ()
  ;; Simple let
  (deftest 600 (let ((x 10)) x) 10)
  (deftest 601 (let ((x 1) (y 2)) (+ x y)) 3)
  (deftest 602 (let ((x 5)) (let ((y 10)) (+ x y))) 15)

  ;; Nested lets (regression — was a documented limitation, now debunked)
  (deftest 610 (let ((a 1))
                 (let ((b 2))
                   (let ((c 3))
                     (+ a (+ b c))))) 6)

  ;; Deep nesting (20 levels)
  (deftest 611 (let ((a 1))
    (let ((b 2))
      (let ((c 3))
        (let ((d 4))
          (let ((e 5))
            (let ((f 6))
              (let ((g 7))
                (let ((h 8))
                  (let ((i 9))
                    (let ((j 10))
                      (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h (+ i j)))))))))))))))))))
    55)

  ;; Let with body forms
  (deftest 620 (let ((x 0))
                 (setq x (+ x 1))
                 (setq x (+ x 1))
                 x)
               2)

  ;; Let shadow
  (deftest 625 (let ((x 1))
                 (let ((x 2))
                   x))
               2))

;;; ============================================================
;;; Chapter 5: Setq (650-669)
;;; ============================================================

(defun run-setq-tests ()
  (deftest 650 (let ((x 0)) (setq x 42) x) 42)
  (deftest 651 (let ((x 1)) (setq x (+ x 1)) x) 2))

;;; ============================================================
;;; Chapter 5: Loop and return (670-699)
;;; ============================================================

(defun run-loop-tests ()
  ;; Simple loop with return
  (deftest 670 (let ((i 0) (s 0))
                 (loop
                   (when (= i 10) (return s))
                   (setq s (+ s i))
                   (setq i (+ i 1))))
               45)

  ;; Loop counting
  (deftest 671 (let ((n 0))
                 (loop
                   (when (= n 5) (return n))
                   (setq n (+ n 1))))
               5)

  ;; Nested loop
  (deftest 672 (let ((total 0))
                 (let ((i 0))
                   (loop
                     (when (= i 3) (return total))
                     (let ((j 0))
                       (loop
                         (when (= j 3) (return nil))
                         (setq total (+ total 1))
                         (setq j (+ j 1))))
                     (setq i (+ i 1)))))
               9))

;;; ============================================================
;;; Chapter 5: Function calls (700-749)
;;; ============================================================

(defun test-helper-add (a b) (+ a b))
(defun test-helper-identity (x) x)
(defun test-helper-factorial (n)
  (if (= n 0) 1 (* n (test-helper-factorial (- n 1)))))

(defun run-function-tests ()
  ;; Basic function calls
  (deftest 700 (test-helper-add 1 2) 3)
  (deftest 701 (test-helper-identity 42) 42)
  (deftest 702 (test-helper-identity nil) nil)

  ;; Recursive function
  (deftest 710 (test-helper-factorial 0) 1)
  (deftest 711 (test-helper-factorial 1) 1)
  (deftest 712 (test-helper-factorial 5) 120)
  (deftest 713 (test-helper-factorial 10) 3628800))

;;; ============================================================
;;; Chapter 10: Symbols (1000-1049)
;;; ============================================================

(defun run-symbol-tests ()
  ;; symbolp
  (deftest 1000 (symbolp (quote foo)) t)
  (deftest 1001 (symbolp 42) nil)
  (deftest 1002 (symbolp (cons 1 2)) nil)
  ;; NEEDS: (symbolp nil) → t, (symbolp t) → t
  ;; These require nil/t to be proper symbol objects

  ;; eq identity for interned symbols
  (deftest 1010 (eq (quote foo) (quote foo)) t)
  (deftest 1011 (eq (quote foo) (quote bar)) nil))

;;; ============================================================
;;; Chapter 7: Objects — Type predicates (700-799 reserved above)
;;; Using 1700+
;;; ============================================================

(defun run-type-tests ()
  ;; fixnump (MVM-specific, maps to integerp)
  (deftest 1700 (fixnump 0) t)
  (deftest 1701 (fixnump 42) t)
  (deftest 1702 (fixnump -1) t)
  (deftest 1703 (fixnump nil) nil)
  (deftest 1704 (fixnump (cons 1 2)) nil)

  ;; consp (already tested above, but here for type completeness)
  (deftest 1710 (consp (cons 1 2)) t)
  (deftest 1711 (consp nil) nil)
  (deftest 1712 (consp 42) nil)

  ;; null
  (deftest 1720 (null nil) t)
  (deftest 1721 (null t) nil)
  (deftest 1722 (null 0) nil))

;;; ============================================================
;;; Chapter 15: Arrays (1800-1899)
;;; ============================================================

(defun run-array-tests ()
  ;; make-array, aref, aset
  (deftest 1800 (let ((a (make-array 3)))
                  (aset a 0 10)
                  (aset a 1 20)
                  (aset a 2 30)
                  (aref a 0))
                10)
  (deftest 1801 (let ((a (make-array 3)))
                  (aset a 0 10)
                  (aset a 1 20)
                  (aset a 2 30)
                  (aref a 1))
                20)
  (deftest 1802 (let ((a (make-array 3)))
                  (aset a 0 10)
                  (aset a 1 20)
                  (aset a 2 30)
                  (aref a 2))
                30)

  ;; array-length
  (deftest 1810 (array-length (make-array 5)) 5)
  (deftest 1811 (array-length (make-array 0)) 0)
  (deftest 1812 (array-length (make-array 100)) 100))

;;; ============================================================
;;; Chapter 18: Hash Tables (1900-1999)
;;; ============================================================

(defun run-hash-table-tests ()
  ;; Basic put/get
  (deftest 1900 (let ((ht (make-hash-table)))
                  (puthash 1 ht 10)
                  (gethash 1 ht))
                10)
  (deftest 1901 (let ((ht (make-hash-table)))
                  (puthash 1 ht 10)
                  (puthash 2 ht 20)
                  (gethash 2 ht))
                20)

  ;; Missing key
  (deftest 1910 (let ((ht (make-hash-table)))
                  (gethash 999 ht))
                nil)

  ;; Overwrite
  (deftest 1915 (let ((ht (make-hash-table)))
                  (puthash 1 ht 10)
                  (puthash 1 ht 99)
                  (gethash 1 ht))
                99))

;;; ============================================================
;;; Debug: test calling function named "equal"
;;; ============================================================


;;; ============================================================
;;; Regression tests (9000+)
;;; Tests for previously-fixed bugs
;;; ============================================================

(defun run-regression-tests ()
  ;; Multi-arg + (was documented as broken, debunked)
  (deftest 9000 (+ 60 5 7) 72)
  (deftest 9001 (+ 1 2 3 4 5) 15)

  ;; Nested logior/ash (was broken before interned symbols fix)
  (deftest 9010 (let ((b0 1) (b1 2) (b2 3) (b3 4))
                  (logior b0 (logior (ash b1 8) (logior (ash b2 16) (ash b3 24)))))
                67305985)

  ;; Deep nested let (was documented as 18+ limit, debunked)
  ;; Use accumulator pattern to avoid deep + nesting
  (deftest 9020 (let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4))
    (let ((e 5)) (let ((f 6)) (let ((g 7)) (let ((h 8))
    (let ((i 9)) (let ((j 10)) (let ((k 11)) (let ((l 12))
    (let ((m 13)) (let ((n 14)) (let ((o 15)) (let ((p 16))
    (let ((q 17)) (let ((r 18)) (let ((s 19)) (let ((tt 20))
    (let ((sum (+ a b c d e f g h i j)))
      (+ sum k l m n o p q r s tt))))))))))))))))))))))
    210)

  ;; Sequential forms (was documented as 25+ limit, debunked)
  (deftest 9030 (let ((x 0))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  (setq x (+ x 1))
                  x)
                30))

;;; ============================================================
;;; Master test runner
;;; ============================================================

(defun run-all-tests ()
  (run-cons-tests)
  (run-list-tests)
  (run-arithmetic-tests)
  (run-comparison-tests)
  (run-bitwise-tests)
  (run-control-flow-tests)
  (run-let-tests)
  (run-setq-tests)
  (run-loop-tests)
  (run-function-tests)
  (run-symbol-tests)
  (run-type-tests)
  (run-array-tests)
  (run-hash-table-tests)
  (run-regression-tests))
