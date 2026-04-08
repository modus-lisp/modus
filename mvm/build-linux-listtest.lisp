;;;; build-linux-listtest.lisp - Test list operations in Linux MVM binary
(load (merge-pathnames "../lib/load-mvm.lisp"
                       (directory-namestring (truename *load-truename*))))
(modus.mvm.x64:install-x64-translator)
(setf modus.mvm.x64::*x64-linux-mode* t)
(load (merge-pathnames "../boot/boot-linux-x64.lisp"
                       (directory-namestring (truename *load-truename*))))
(in-package :modus.mvm)

(let* ((source "
;; List operations in MVM-subset Lisp.
;; cons, car, cdr are MVM primitives.
;; nil = 0 (the MVM nil value on Linux — bare metal uses #xDEAD0001)

;; length of a list
(defun list-length (lst)
  (let ((l lst))
    (let ((n 0))
      (loop
        (when (atom l) (return n))
        (setq n (+ n 1))
        (setq l (cdr l))))))

;; nreverse: destructively reverse a list
(defun nreverse (lst)
  (let ((l lst))
    (let ((prev 0))
      (loop
        (when (atom l) (return prev))
        (let ((next (cdr l)))
          (set-cdr l prev)
          (setq prev l)
          (setq l next))))))

;; append (non-destructive, copies first list)
(defun append2 (a b)
  (let ((aa a))
    (let ((bb b))
      (if (atom aa)
          bb
          (let ((result 0))
            (let ((tail 0))
              (let ((cur aa))
                (loop
                  (when (atom cur)
                    (when (not (atom tail))
                      (set-cdr tail bb))
                    (return result))
                  (let ((new-cell (cons (car cur) 0)))
                    (if (atom result)
                        (progn (setq result new-cell) (setq tail new-cell))
                        (progn (set-cdr tail new-cell) (setq tail new-cell))))
                  (setq cur (cdr cur))))))))))

;; mapcar (1-arg version)
(defun mapcar1 (fn lst)
  (let ((f fn))
    (let ((l lst))
      (let ((result 0))
        (loop
          (when (atom l) (return (nreverse result)))
          (let ((val (f (car l))))
            (setq result (cons val result)))
          (setq l (cdr l)))))))

;; nth: get nth element (0-based)
(defun nth (n lst)
  (let ((nn n))
    (let ((l lst))
      (loop
        (when (atom l) (return 0))
        (when (= nn 0) (return (car l)))
        (setq nn (- nn 1))
        (setq l (cdr l))))))

;; member: is val in list? returns tail from match or nil
(defun member (val lst)
  (let ((v val))
    (let ((l lst))
      (loop
        (when (atom l) (return 0))
        (when (= v (car l)) (return l))
        (setq l (cdr l))))))

;; assoc: find pair with matching car in alist
(defun assoc (key alist)
  (let ((k key))
    (let ((l alist))
      (loop
        (when (atom l) (return 0))
        (let ((pair (car l)))
          (when (= k (car pair))
            (return pair)))
        (setq l (cdr l))))))

;; Build a list from varargs (up to 5)
(defun list1 (a) (cons a 0))
(defun list2 (a b) (cons a (cons b 0)))
(defun list3 (a b c) (let ((aa a)) (let ((bb b)) (let ((cc c)) (cons aa (cons bb (cons cc 0)))))))

;; print-dec
(defun print-dec (n)
  (let ((nn n))
    (if (< nn 0) (progn (write-char-serial 45) (print-dec (- 0 nn)))
        (if (< nn 10) (write-char-serial (+ nn 48))
            (progn (print-dec (/ nn 10))
                   (write-char-serial (+ 48 (- nn (* (/ nn 10) 10)))))))))

;; print a list of integers
(defun print-list (lst)
  (write-char-serial 40)
  (let ((l lst))
    (let ((first 1))
      (loop
        (when (atom l)
          (write-char-serial 41)
          (return 0))
        (when (= first 0) (write-char-serial 32))
        (setq first 0)
        (print-dec (car l))
        (setq l (cdr l))))))

(defun double (x) (let ((xx x)) (+ xx xx)))

(defun kernel-main ()
  ;; Test list building
  (let ((ls (list3 10 20 30)))
    (print-list ls)
    (write-char-serial 10))

  ;; Test length
  (let ((ls (list3 1 2 3)))
    (write-char-serial 76)  ;; L
    (print-dec (list-length ls))
    (write-char-serial 10))

  ;; Test nreverse
  (let ((ls (list3 1 2 3)))
    (let ((rev (nreverse ls)))
      (print-list rev)
      (write-char-serial 10)))

  ;; Test nth
  (let ((ls (list3 100 200 300)))
    (write-char-serial 78)  ;; N
    (print-dec (nth 0 ls))
    (write-char-serial 32)
    (print-dec (nth 2 ls))
    (write-char-serial 10))

  ;; Test manual map (mapcar needs closures/funcall)
  (let ((ls (list3 5 10 15)))
    (let ((result 0))
      (let ((l ls))
        (loop
          (when (atom l)
            (print-list (nreverse result))
            (write-char-serial 10)
            (return 0))
          (setq result (cons (double (car l)) result))
          (setq l (cdr l))))))

  ;; Test assoc
  (let ((alist (list3 (cons 1 10) (cons 2 20) (cons 3 30))))
    (let ((found (assoc 2 alist)))
      (write-char-serial 65)  ;; A
      (print-dec (cdr found))
      (write-char-serial 10)))

  (sys-exit 0))
")
       (image (build-image :target :linux-x64 :source-text source)))
  (let ((path "/tmp/modus-listtest"))
    (with-open-file (out path :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
      (write-sequence (kernel-image-image-bytes image) out))
    #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" path) :wait t)
    (format t "Wrote ~D bytes to ~A~%" (length (kernel-image-image-bytes image)) path)))
