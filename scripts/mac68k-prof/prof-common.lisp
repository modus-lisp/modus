;;;; prof-common.lisp -- what the mac68k-prof tools share: arguments, counters
;;;; that break ties by first appearance (as Python's Counter.most_common), the
;;;; symbol map, and json-simple / cram from the workspace (WORKSPACE, default the
;;;; directory above this checkout).

(require :asdf)
(defpackage #:mac68k-prof (:use #:cl))
(in-package #:mac68k-prof)

(defvar *workspace*
  (let ((w (sb-ext:posix-getenv "WORKSPACE")))
    (if (and w (plusp (length w)))
        (pathname (if (char= (char w (1- (length w))) #\/) w (concatenate 'string w "/")))
        (merge-pathnames "../../../" (make-pathname :name nil :type nil :defaults *load-truename*)))))
(push (lambda (name)
        (let ((base (subseq name 0 (or (position #\/ name) (length name)))))
          (probe-file (merge-pathnames (format nil "~A/~A.asd" base base) *workspace*))))
      asdf:*system-definition-search-functions*)
(defun need (&rest systems)
  (let ((*error-output* (make-broadcast-stream)) (*standard-output* (make-broadcast-stream)))
    (mapc #'asdf:load-system systems)))

(defun argv () (rest sb-ext:*posix-argv*))
(defun env (name &optional default)
  (let ((v (sb-ext:posix-getenv name))) (if (and v (plusp (length v))) v default)))
(defun hex (s) (parse-integer s :start (if (and (> (length s) 1) (string-equal (subseq s 0 2) "0x")) 2 0) :radix 16))
(defun num (s)
  "An integer as Python's int(s, 0) reads it: 0x.. hex, else decimal."
  (if (and (> (length s) 1) (string-equal (subseq s 0 2) "0x")) (parse-integer s :start 2 :radix 16) (parse-integer s)))

(defun words (line)
  (let ((out nil) (start nil))
    (loop for i from 0 to (length line)
          for c = (if (< i (length line)) (char line i) #\Space)
          do (if (member c '(#\Space #\Tab #\Return #\Newline))
                 (when start (push (subseq line start i) out) (setq start nil))
                 (unless start (setq start i))))
    (nreverse out)))

(defun lines (path)
  (with-open-file (in path) (loop for l = (read-line in nil) while l collect l)))

;;; a counter: KEY -> count, and the order keys first appeared
(defstruct (counter (:constructor make-counter ())) (table (make-hash-table :test 'equal)) (order nil))
(defun bump (c key &optional (by 1))
  (unless (nth-value 1 (gethash key (counter-table c))) (push key (counter-order c)))
  (incf (gethash key (counter-table c) 0) by))
(defun count-of (c key) (gethash key (counter-table c) 0))
(defun total (c) (loop for v being the hash-values of (counter-table c) sum v))
(defun most-common (c &optional n)
  "(key . count) pairs, most first, ties in first-appearance order."
  (let ((all (stable-sort (mapcar (lambda (k) (cons k (count-of c k))) (reverse (counter-order c)))
                          #'> :key #'cdr)))
    (if n (subseq all 0 (min n (length all))) all)))

(defun pct (c n) (* 100.0 (/ c n)))

;;; the symbol map: rows (start size name), by start
(defun read-map (path)
  (let ((rows (loop for l in (lines path)
                    unless (and (plusp (length l)) (char= (char l 0) #\#))
                      append (let ((p (loop with s = 0 for i = (position #\Tab l :start s)
                                            collect (subseq l s i) while i do (setq s (1+ i)))))
                               (when (>= (length p) 4)
                                 (list (list (parse-integer (first p) :radix 16) (parse-integer (second p)) (fourth p))))))))
    (coerce (stable-sort rows #'< :key #'first) 'vector)))

(defun map-name (rows a &optional (slack 16))
  "The function whose body (and SLACK bytes past it) holds A, or NIL."
  (let ((lo 0) (hi (length rows)))
    (loop while (< lo hi)
          do (let ((mid (floor (+ lo hi) 2)))
               (if (< a (first (aref rows mid))) (setq hi mid) (setq lo (1+ mid)))))
    (when (> lo 0)
      (let ((r (aref rows (1- lo))))
        (when (< a (+ (first r) (second r) slack)) (third r))))))

;;; idx.json (mkidx's): stream index -> function name
(defun read-idx (dir)
  (need "json-simple")
  (let* ((j (funcall (find-symbol "PARSE" "JSON-SIMPLE")
                     (with-open-file (in (format nil "~A/idx.json" dir))
                       (let ((s (make-string (file-length in)))) (subseq s 0 (read-sequence s in))))))
         (idx (make-hash-table)))
    (maphash (lambda (k v)
               (let ((p (search " [chunk]" v)))
                 (setf (gethash (parse-integer k) idx) (if p (concatenate 'string (subseq v 0 p) (subseq v (+ p 8))) v))))
             (gethash "idx" j))
    idx))

(defun collapse (fs)
  "FS with direct repeats (recursion) merged."
  (let ((path nil))
    (dolist (f fs (nreverse path)) (unless (and path (equal (first path) f)) (push f path)))))

;;; a call tree: name -> (count . children)
(defun tree-add (tree path)
  (dolist (f path)
    (let ((node (or (assoc f (cdr tree) :test #'equal)
                    (let ((n (list f 0))) (setf (cdr tree) (append (cdr tree) (list n))) n))))
      (incf (second node))
      (setq tree (cdr node)))))
(defun tree-walk (tree n threshold max-depth fmt &optional (depth 0))
  (dolist (node (stable-sort (copy-list (cdr tree)) #'> :key #'second))
    (when (and (>= (pct (second node) n) threshold) (< depth max-depth))
      (format t fmt (make-string (* 2 depth) :initial-element #\Space) (pct (second node) n) (first node))
      (tree-walk (cdr node) n threshold max-depth fmt (1+ depth)))))
