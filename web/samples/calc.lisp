;;;; calc.lisp - a DOM calculator driven from Lisp.  (load "gui.lisp") first.
;;;;   (calc)      ; opens the panel; Esc closes it.
;;;;
;;;; The page owns the pixels; this file owns the arithmetic.  Buttons post
;;;; click events (their guiid is the label), we fold them into calculator
;;;; state and push the display string back.  Integer math stays exact;
;;;; anything with a "." or a non-dividing "/" becomes a float, and it prints
;;;; the way the reader would read it back.

(defun %str-has (s ch)
  (let ((i 0) (n (length s)))
    (loop (when (>= i n) (return nil))
      (when (char= (char s i) ch) (return t))
      (setq i (+ i 1)))))

(defun %digit-id-p (id)
  (and (= (length id) 1) (char<= #\0 (char id 0)) (char<= (char id 0) #\9)))

(defun %parse-num (s)
  (if (or (null s) (= (length s) 0) (string= s "-") (string= s ".") (string= s "-."))
      0
      (let ((v (read-from-string s))) (if (numberp v) v 0))))

(defun %calc-show (n)
  ;; print floats without the "d0"/"f0" exponent marker by making the value a
  ;; double and matching *read-default-float-format* to it
  (let* ((*read-default-float-format* 'double-float)
         (m (if (floatp n) (coerce n 'double-float) n))
         (s (write-to-string m)))
    (if (> (length s) 16) (subseq s 0 16) s)))

(defun %calc-apply (a op b)
  (cond ((string= op "+") (+ a b))
        ((string= op "-") (- a b))
        ((string= op "*") (* a b))
        ((string= op "/")
         (cond ((and (numberp b) (zerop b)) 0)
               ((and (integerp a) (integerp b) (zerop (mod a b))) (/ a b))
               (t (/ (float a) b))))
        (t b)))

;; Fold one button press into (disp acc op fresh); returns the next 4-tuple.
(defun %calc-step (disp acc op fresh id)
  (cond
    ((string= id "C") (list "0" nil nil t))
    ((%digit-id-p id)
     (list (if (or fresh (string= disp "0")) id (concatenate 'string disp id)) acc op nil))
    ((string= id ".")
     (list (cond (fresh "0.") ((%str-has disp #\.) disp) (t (concatenate 'string disp "."))) acc op nil))
    ((string= id "neg") (list (%calc-show (- (%parse-num disp))) acc op fresh))
    ((string= id "%")   (list (%calc-show (/ (coerce (%parse-num disp) 'double-float) 100)) acc op fresh))
    ((or (string= id "+") (string= id "-") (string= id "*") (string= id "/"))
     (let ((cur (%parse-num disp)))
       (if (and op (not fresh))
           (let ((r (%calc-apply acc op cur))) (list (%calc-show r) r id t))
           (list disp cur id t))))
    ((string= id "=")
     (if op
         (let ((r (%calc-apply acc op (%parse-num disp)))) (list (%calc-show r) nil nil t))
         (list disp acc op t)))
    (t (list disp acc op fresh))))

(defun %calc-btn (id label)
  (gui-el id "pad" "button")
  (gui-text id label)
  (gui-style id "font:20px system-ui,sans-serif;padding:16px 0;border:none;border-radius:8px;background:#2c2c30;color:#eee;cursor:pointer")
  (gui-on id "click"))

(defun calc-build-ui ()
  (gui-reset)
  (gui-panel "Calculator")
  (gui-el "calc" "root" "div")
  (gui-style "calc" "width:252px;margin:26px auto;user-select:none")
  (gui-el "disp" "calc" "div")
  (gui-text "disp" "0")
  (gui-style "disp" "background:#0b0b0d;color:#4caf50;font:30px monospace;text-align:right;padding:16px;border-radius:10px;margin-bottom:8px;overflow:hidden;white-space:nowrap")
  (gui-el "pad" "calc" "div")
  (gui-style "pad" "display:grid;grid-template-columns:repeat(4,1fr);gap:6px")
  (dolist (b '(("C" "C") ("neg" "+/-") ("%" "%") ("/" "/")
               ("7" "7") ("8" "8") ("9" "9") ("*" "x")
               ("4" "4") ("5" "5") ("6" "6") ("-" "-")
               ("1" "1") ("2" "2") ("3" "3") ("+" "+")
               ("0" "0") ("." ".") ("=" "=")))
    (%calc-btn (car b) (cadr b)))
  ;; accent the operator column and the equals key
  (dolist (id '("/" "*" "-" "+"))
    (gui-style id "font:20px system-ui,sans-serif;padding:16px 0;border:none;border-radius:8px;background:#ff9500;color:#fff;cursor:pointer"))
  (gui-style "=" "font:20px system-ui,sans-serif;padding:16px 0;border:none;border-radius:8px;background:#4caf50;color:#fff;cursor:pointer;grid-column:span 2"))

(defun calc ()
  (calc-build-ui)
  (let ((disp "0") (acc nil) (op nil) (fresh t) (done nil))
    (loop
      (when done (gui-close) (return :bye))
      (gui-wait 150)
      (dolist (ev (gui-events))
        (cond
          ((key-down-p ev "Escape") (setq done t))
          ((ev-is ev "click")
           (let ((st (%calc-step disp acc op fresh (cadr ev))))
             (setq disp (car st) acc (cadr st) op (caddr st) fresh (cadddr st))
             (gui-text "disp" disp))))))))

(format t "calc.lisp loaded - run (calc)~%")
