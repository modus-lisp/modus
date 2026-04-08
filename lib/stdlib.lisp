;;;; stdlib.lisp - MVM Standard Library (MVM-subset Lisp)
;;;;
;;;; Shared utility functions for MVM-compiled programs.
;;;; All functions use only the MVM source language subset:
;;;; defun, let, if, when, loop, return, setq, cons, car, cdr,
;;;; set-cdr, atom, make-array, aref, aset, arithmetic, comparison.
;;;;
;;;; Load as source text via (read-file-text "lib/stdlib.lisp")
;;;; and concatenate with application source before compilation.

;;; ============================================================
;;; List Operations
;;; ============================================================

(defun list-length (lst)
  (let ((l lst))
    (let ((n 0))
      (loop
        (when (atom l) (return n))
        (setq n (+ n 1))
        (setq l (cdr l))))))

(defun nreverse (lst)
  (let ((l lst))
    (let ((prev 0))
      (loop
        (when (atom l) (return prev))
        (let ((next (cdr l)))
          (set-cdr l prev)
          (setq prev l)
          (setq l next))))))

(defun nth-elem (n lst)
  (let ((nn n))
    (let ((l lst))
      (loop
        (when (atom l) (return 0))
        (when (= nn 0) (return (car l)))
        (setq nn (- nn 1))
        (setq l (cdr l))))))

(defun member-eq (val lst)
  (let ((v val))
    (let ((l lst))
      (loop
        (when (atom l) (return 0))
        (when (= v (car l)) (return l))
        (setq l (cdr l))))))

(defun assoc-eq (key alist)
  (let ((k key))
    (let ((l alist))
      (loop
        (when (atom l) (return 0))
        (let ((pair (car l)))
          (when (= k (car pair))
            (return pair)))
        (setq l (cdr l))))))

(defun list1 (a) (cons a 0))
(defun list2 (a b) (cons a (cons b 0)))
(defun list3 (a b c)
  (let ((aa a)) (let ((bb b)) (let ((cc c))
    (cons aa (cons bb (cons cc 0)))))))

;;; ============================================================
;;; Hash Table (open addressing, linear probing)
;;; ============================================================
;;; Layout: array with slot 0 = capacity, then [key val ...] pairs.
;;; Empty slots have key = 0. Keys must be non-zero fixnums.
;;; Total array size = 1 + 2*capacity.

(defun ht-make (capacity)
  (let ((c capacity))
    (let ((arr (make-array (+ 1 (* c 2)))))
      (aset arr 0 c)
      arr)))

(defun ht-hash (key capacity)
  (let ((k key))
    (let ((h (logand (ash (* k 2654435761) -16) #x7FFFFFFF)))
      (let ((cap capacity))
        (- h (* (/ h cap) cap))))))

(defun ht-get (ht key)
  (let ((h ht))
    (let ((k key))
      (let ((cap (aref h 0)))
        (let ((idx (ht-hash k cap)))
          (let ((i 0))
            (loop
              (when (>= i cap) (return 0))
              (let ((slot (- (+ idx i) (* (/ (+ idx i) cap) cap))))
                (let ((ki (+ 1 (* slot 2))))
                  (let ((sk (aref h ki)))
                    (if (= sk 0)
                        (return 0)
                        (when (= sk k)
                          (return (aref h (+ ki 1))))))))
              (setq i (+ i 1)))))))))

(defun ht-put (ht key val)
  (let ((h ht))
    (let ((k key))
      (let ((v val))
        (let ((cap (aref h 0)))
          (let ((idx (ht-hash k cap)))
            (let ((i 0))
              (loop
                (when (>= i cap) (return 0))
                (let ((slot (- (+ idx i) (* (/ (+ idx i) cap) cap))))
                  (let ((ki (+ 1 (* slot 2))))
                    (let ((sk (aref h ki)))
                      (if (= sk 0)
                          (progn (aset h ki k) (aset h (+ ki 1) v) (return v))
                          (when (= sk k)
                            (aset h (+ ki 1) v) (return v))))))
                (setq i (+ i 1))))))))))

;;; ============================================================
;;; Output Helpers
;;; ============================================================

(defun print-dec (n)
  (let ((nn n))
    (if (< nn 0)
        (progn (write-char-serial 45) (print-dec (- 0 nn)))
        (if (< nn 10)
            (write-char-serial (+ nn 48))
            (progn (print-dec (/ nn 10))
                   (write-char-serial (+ 48 (- nn (* (/ nn 10) 10)))))))))

(defun print-str-arr (arr len)
  (let ((a arr))
    (let ((l len))
      (let ((i 0))
        (loop
          (when (>= i l) (return 0))
          (write-char-serial (aref a i))
          (setq i (+ i 1)))))))

(defun print-hex-byte (b)
  (let ((hi (ash b -4)))
    (let ((lo (logand b 15)))
      (write-char-serial (if (< hi 10) (+ 48 hi) (+ 55 hi)))
      (write-char-serial (if (< lo 10) (+ 48 lo) (+ 55 lo))))))

(defun newline () (write-char-serial 10))
(defun space () (write-char-serial 32))
